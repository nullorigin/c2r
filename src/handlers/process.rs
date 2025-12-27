//! Handler Processing Pipeline
//!
//! Defines the core processing pipeline for handlers:
//! - `ProcessStage`: Current stage in the pipeline
//! - `ProcessDecision`: Decision returned by process() to indicate next action
//! - `Processor`: Trait defining the handler processing pipeline
//! - `ProcessingContext`: Manages handler stack for nested processing

use crate::db::pattern::{ExtractContext, Pattern, REPEAT_NONE};
use crate::db::token::Token;
use crate::db::web::Build;

// ============================================================================
// Processing Stage Enum
// ============================================================================

/// Current stage in the processing pipeline
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProcessStage {
    /// Initial state - not yet validated
    Pending,
    /// Validation passed
    Validated,
    /// Data extracted from tokens
    Extracted,
    /// Conversion to Rust completed
    Converted,
    /// Routed to another handler
    Routed,
    /// Processing complete
    Complete,
    /// Processing failed at some stage
    Failed,
}

impl ProcessStage {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Pending => "pending",
            Self::Validated => "validated",
            Self::Extracted => "extracted",
            Self::Converted => "converted",
            Self::Routed => "routed",
            Self::Complete => "complete",
            Self::Failed => "failed",
        }
    }
}

// ============================================================================
// Process Decision - Returned by process() to indicate next action
// ============================================================================

/// Decision returned by process() indicating what happened and what to do next
#[derive(Debug, Clone)]
pub enum ProcessDecision {
    /// Continue to next stage
    Continue { stage: ProcessStage, reason: String },
    /// Processing is complete - store results
    Complete { rust_code: String, confidence: f64 },
    /// Route to another handler (replaces current handler)
    Route {
        target_handler: String,
        reason: String,
    },
    /// Route to nested handler, then return to current handler
    /// Used when extract() finds a nested construct (e.g., array inside function)
    RouteNested {
        /// Handler to process the nested construct
        target_handler: String,
        /// Token range for the nested construct (start..end indices)
        token_range: std::ops::Range<usize>,
        /// Reason for nesting
        reason: String,
    },
    /// Nested handler completed, return result to parent
    NestedComplete {
        /// Rust code from nested handler
        rust_code: String,
        /// Confidence from nested handler
        confidence: f64,
        /// Token range that was processed
        token_range: std::ops::Range<usize>,
    },
    /// Cannot proceed - need more information or tokens
    Pause { stage: ProcessStage, reason: String },
    /// Processing failed
    Fail { stage: ProcessStage, reason: String },
}

// ============================================================================
// Processor Trait - The core processing pipeline
// ============================================================================

/// Trait defining the handler processing pipeline.
/// Each handler must implement this trait to participate in the conversion process.
pub trait Processor: Build {
    /// Get the handler name
    fn name(&self) -> &str;

    /// Get supported pattern names for this handler
    fn supported_patterns(&self) -> &[&str];

    /// Get pattern pairs: (validation_pattern, extraction_pattern)
    /// - First pattern: Simple rules for fast validation (no callbacks)
    /// - Second pattern: Full rules with extraction callbacks
    ///
    /// Both patterns should match the same constructs, but:
    /// - Validation pattern is lightweight for quick matching
    /// - Extraction pattern has callbacks to populate ExtractContext
    fn patterns(&self) -> Vec<(Pattern, Pattern)>;

    /// Validate that the tokens can be handled by this processor.
    /// Uses simple validation patterns (first of pair) for speed.
    /// Returns true if tokens are valid for this handler, false otherwise.
    fn validate(&mut self, tokens: &[Token]) -> bool {
        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        for (validation_pattern, _) in self.patterns() {
            if let Pattern::Definition {
                rules, min_tokens, ..
            } = &validation_pattern
            {
                if token_strs.len() < *min_tokens {
                    continue;
                }

                // Simple validation - just check if rules match without callbacks
                let mut token_idx = 0;
                let mut rule_idx = 0;
                let mut matched = true;

                while rule_idx < rules.len() && token_idx < token_strs.len() {
                    let rule = &rules[rule_idx];
                    let token = &token_strs[token_idx];

                    if Self::rule_matches(rule, token) {
                        token_idx += 1;

                        // Handle repeating rules
                        if rule.can_repeat != REPEAT_NONE {
                            while token_idx < token_strs.len()
                                && Self::rule_matches(rule, &token_strs[token_idx])
                            {
                                token_idx += 1;
                            }
                        }
                        rule_idx += 1;
                    } else if rule.optional {
                        rule_idx += 1;
                    } else {
                        matched = false;
                        break;
                    }
                }

                // Check remaining rules are optional
                while rule_idx < rules.len() {
                    if !rules[rule_idx].optional {
                        matched = false;
                        break;
                    }
                    rule_idx += 1;
                }

                if matched {
                    return true;
                }
            }
        }

        false
    }

    /// Extract structured data from the tokens.
    /// Called after validation passes. Populates internal state with extracted info.
    ///
    /// Default implementation uses pattern-based extraction with callbacks.
    /// Handlers can override for custom extraction logic.
    fn extract(&mut self, tokens: &[Token]) -> bool {
        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        let mut ctx = ExtractContext::new(&token_strs);

        // Try each pattern until one matches (use extraction pattern from tuple)
        for (_, extraction_pattern) in self.patterns() {
            if let Pattern::Definition {
                rules, min_tokens, ..
            } = &extraction_pattern
            {
                if token_strs.len() < *min_tokens {
                    continue;
                }

                // Match tokens against rules, calling extract callbacks
                let mut token_idx = 0;
                let mut rule_idx = 0;
                let mut matched = true;

                while rule_idx < rules.len() && token_idx < token_strs.len() {
                    let rule = &rules[rule_idx];
                    let token = &token_strs[token_idx];

                    // Update context for this token
                    ctx.current_token = token.clone();
                    ctx.token_idx = token_idx;

                    // Check if rule matches token
                    let rule_matches = Self::rule_matches(rule, token);
                    ctx.did_match = rule_matches;
                    ctx.matched = if rule_matches {
                        Some(token.clone())
                    } else {
                        None
                    };

                    if rule_matches {
                        // Apply extraction callback if present
                        if rule.has_extract() {
                            rule.apply_extract(&mut ctx);
                        }

                        token_idx += 1;

                        // Handle repeating rules
                        if rule.can_repeat != REPEAT_NONE {
                            while token_idx < token_strs.len() {
                                let next_token = &token_strs[token_idx];
                                if Self::rule_matches(rule, next_token) {
                                    ctx.current_token = next_token.clone();
                                    ctx.token_idx = token_idx;
                                    ctx.matched = Some(next_token.clone());
                                    ctx.did_match = true;

                                    if rule.has_extract() {
                                        rule.apply_extract(&mut ctx);
                                    }
                                    token_idx += 1;
                                } else {
                                    break;
                                }
                            }
                        }
                        rule_idx += 1;
                    } else if rule.optional {
                        ctx.matched = None;
                        ctx.did_match = false;
                        rule_idx += 1;
                    } else {
                        matched = false;
                        break;
                    }
                }

                // Check remaining rules are optional
                while rule_idx < rules.len() {
                    if !rules[rule_idx].optional {
                        matched = false;
                        break;
                    }
                    rule_idx += 1;
                }

                if matched {
                    // Apply extracted context to handler
                    self.apply_context(&ctx);
                    return true;
                }
            }
        }

        false
    }

    /// Apply extracted context to handler's internal state.
    /// Override this to map ExtractContext values to your handler's data struct.
    fn apply_context(&mut self, _ctx: &ExtractContext) {
        // Default: no-op. Handlers should override to populate their data.
    }

    /// Check if a rule matches a token.
    /// Can be overridden for custom matching logic.
    fn rule_matches(rule: &crate::db::pattern::PatternRule, token: &str) -> bool {
        use crate::db::pattern::RuleType;

        match &rule.rule_type {
            RuleType::Exact => rule.value == token,
            RuleType::OneOf => rule.value.split('|').any(|opt| opt == token),
            RuleType::Identifier => token
                .chars()
                .next()
                .map(|c| c.is_alphabetic() || c == '_')
                .unwrap_or(false),
            RuleType::TypeKeyword => {
                matches!(
                    token,
                    "int"
                        | "char"
                        | "void"
                        | "float"
                        | "double"
                        | "long"
                        | "short"
                        | "unsigned"
                        | "signed"
                        | "struct"
                        | "enum"
                        | "union"
                        | "const"
                        | "static"
                        | "extern"
                        | "volatile"
                        | "register"
                        | "auto"
                        | "size_t"
                        | "ssize_t"
                        | "ptrdiff_t"
                        | "intptr_t"
                        | "uintptr_t"
                        | "int8_t"
                        | "int16_t"
                        | "int32_t"
                        | "int64_t"
                        | "uint8_t"
                        | "uint16_t"
                        | "uint32_t"
                        | "uint64_t"
                        | "bool"
                        | "_Bool"
                )
            }
            RuleType::Keyword => {
                matches!(
                    token,
                    "if" | "else"
                        | "while"
                        | "for"
                        | "do"
                        | "switch"
                        | "case"
                        | "break"
                        | "continue"
                        | "return"
                        | "goto"
                        | "default"
                        | "sizeof"
                        | "typedef"
                        | "struct"
                        | "enum"
                        | "union"
                )
            }
            RuleType::Any => true,
            RuleType::AnyExcept => {
                // Exclusions stored as pipe-separated in rule.value
                !rule.value.split('|').any(|excl| excl == token)
            }
            RuleType::Custom(_) => true, // Custom rules need handler-specific logic
        }
    }

    /// Convert the extracted data to Rust code.
    /// Called after extraction. Produces the Rust output.
    fn convert(&mut self) -> Option<String>;

    /// Determine if this should be routed to another handler.
    /// Returns Some(handler_name) if routing is needed, None otherwise.
    fn route(&self) -> Option<String> {
        None // Default: no routing
    }

    /// Route a statement to the appropriate handler based on its first token.
    /// This is used by handlers that process bodies (functions, loops, conditionals).
    /// Returns the converted Rust code for the statement.
    fn route_statement(&self, tokens: &[Token], source_handler: &str) -> String {
        use crate::db::routing::{create_routing, set_routing_result};

        if tokens.is_empty() {
            return String::new();
        }

        let first = tokens[0].to_string();

        // Break/continue statements
        if first == "break" || first == "continue" {
            return format!("{};", first);
        }

        // Return statement
        if first == "return" {
            let expr_tokens: Vec<Token> = tokens[1..]
                .iter()
                .filter(|t| t.to_string() != ";")
                .cloned()
                .collect();

            if expr_tokens.is_empty() {
                return "return;".to_string();
            }

            use crate::handlers::expressions::ExpressionHandler;
            let routing_id = create_routing(
                source_handler,
                "expression",
                0..expr_tokens.len(),
                expr_tokens.clone(),
                "return expression",
            );

            let mut handler = ExpressionHandler::new();
            if handler.validate(&expr_tokens) && handler.extract(&expr_tokens) {
                if let Some(code) = handler.convert() {
                    set_routing_result(routing_id, code.clone(), handler.confidence(), "ExpressionHandler");
                    return format!("return {};", code);
                }
            }

            let expr: Vec<String> = expr_tokens.iter().map(|t| t.to_string()).collect();
            return format!("return {};", expr.join(" "));
        }

        // Switch statement
        if first == "switch" {
            use crate::handlers::conditionals::SwitchCaseHandler;
            let routing_id = create_routing(source_handler, "switch", 0..tokens.len(), tokens.to_vec(), "switch statement");
            let mut handler = SwitchCaseHandler::new();
            if handler.validate(tokens) && handler.extract(tokens) {
                if let Some(code) = handler.convert() {
                    set_routing_result(routing_id, code.clone(), handler.confidence(), "SwitchCaseHandler");
                    return code;
                }
            }
        }

        // If statement
        if first == "if" {
            use crate::handlers::conditionals::IfElseHandler;
            let routing_id = create_routing(source_handler, "if", 0..tokens.len(), tokens.to_vec(), "if statement");
            let mut handler = IfElseHandler::new();
            if handler.validate(tokens) && handler.extract(tokens) {
                if let Some(code) = handler.convert() {
                    set_routing_result(routing_id, code.clone(), handler.confidence(), "IfElseHandler");
                    return code;
                }
            }
        }

        // Loop statements
        if matches!(first.as_str(), "for" | "while" | "do") {
            use crate::handlers::loops::LoopHandler;
            let routing_id = create_routing(source_handler, "loop", 0..tokens.len(), tokens.to_vec(), "loop statement");
            let mut handler = LoopHandler::new();
            if handler.validate(tokens) && handler.extract(tokens) {
                if let Some(code) = handler.convert() {
                    set_routing_result(routing_id, code.clone(), handler.confidence(), "LoopHandler");
                    return code;
                }
            }
        }

        // Printf-style functions
        if matches!(first.as_str(), "printf" | "fprintf" | "sprintf" | "snprintf" | "puts") {
            use crate::handlers::expressions::StringFormatHandler;
            let routing_id = create_routing(source_handler, "string_format", 0..tokens.len(), tokens.to_vec(), "printf-style call");
            let mut handler = StringFormatHandler::new();
            if handler.validate(tokens) && handler.extract(tokens) {
                if let Some(code) = handler.convert() {
                    set_routing_result(routing_id, code.clone(), handler.confidence(), "StringFormatHandler");
                    return format!("{};", code);
                }
            }
        }

        // Check for compound assignment early - these should go to ExpressionHandler, not Array/Variable
        let has_compound_assign = tokens.iter().any(|t| {
            let s = t.to_string();
            matches!(s.as_str(), "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>=")
        });

        // Variable declaration with array (but NOT compound assignments like sum += values[i])
        if tokens.iter().any(|t| t.to_string() == "[") && !has_compound_assign {
            use crate::handlers::arrays::ArrayHandler;
            let routing_id = create_routing(source_handler, "array", 0..tokens.len(), tokens.to_vec(), "array declaration");
            let mut handler = ArrayHandler::new();
            if handler.validate(tokens) && handler.extract(tokens) {
                if let Some(code) = handler.convert() {
                    set_routing_result(routing_id, code.clone(), handler.confidence(), "ArrayHandler");
                    return code;
                }
            }
        }

        // Variable declaration (has standalone = but NOT compound assignment like +=, -=, etc.)
        let has_simple_assign = tokens.iter().any(|t| {
            let s = t.to_string();
            s == "=" // Only match standalone =, not +=, -=, *=, /=, etc.
        });
        if (has_simple_assign && !has_compound_assign) || self.is_type_keyword(&first) {
            use crate::handlers::variables::VariableHandler;
            let routing_id = create_routing(source_handler, "variable", 0..tokens.len(), tokens.to_vec(), "variable declaration");
            let mut handler = VariableHandler::new();
            if handler.validate(tokens) && handler.extract(tokens) {
                if let Some(code) = handler.convert() {
                    set_routing_result(routing_id, code.clone(), handler.confidence(), "VariableHandler");
                    return code;
                }
            }
        }

        // Function call (identifier followed by parentheses) - checked AFTER variable declarations
        if tokens.len() >= 3 {
            if let Some(paren_pos) = tokens.iter().position(|t| t.to_string() == "(") {
                if paren_pos > 0 {
                    use crate::handlers::functions::CallHandler;
                    let routing_id = create_routing(source_handler, "call", 0..tokens.len(), tokens.to_vec(), "function call");
                    let mut handler = CallHandler::new();
                    if handler.validate(tokens) && handler.extract(tokens) {
                        if let Some(code) = handler.convert() {
                            set_routing_result(routing_id, code.clone(), handler.confidence(), "CallHandler");
                            return format!("{};", code);
                        }
                    }
                }
            }
        }

        // Fallback: expression
        let expr_tokens: Vec<Token> = tokens.iter().filter(|t| t.to_string() != ";").cloned().collect();
        if !expr_tokens.is_empty() {
            use crate::handlers::expressions::ExpressionHandler;
            let routing_id = create_routing(source_handler, "expression", 0..expr_tokens.len(), expr_tokens.clone(), "expression");
            let mut handler = ExpressionHandler::new();
            if handler.validate(&expr_tokens) && handler.extract(&expr_tokens) {
                if let Some(code) = handler.convert() {
                    set_routing_result(routing_id, code.clone(), handler.confidence(), "ExpressionHandler");
                    return format!("{};", code);
                }
            }
        }

        // Ultimate fallback
        let strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        format!("// {}", strs.join(" "))
    }

    /// Check if a token is a C type keyword
    fn is_type_keyword(&self, token: &str) -> bool {
        matches!(
            token,
            "int" | "char" | "short" | "long" | "float" | "double" | "unsigned" | "signed" 
            | "void" | "size_t" | "ssize_t" | "uint8_t" | "uint16_t" | "uint32_t" | "uint64_t"
            | "int8_t" | "int16_t" | "int32_t" | "int64_t" | "bool" | "struct" | "enum" | "union"
        ) || crate::system::system().lookup_type(token).is_some()
    }

    /// Main processing function called iteratively from the processing loop.
    /// Decides whether to proceed to the next stage or return early.
    ///
    /// This is the entry point called by the main process loop.
    /// Default implementation provides the standard pipeline flow.
    fn process(&mut self, tokens: &[Token]) -> ProcessDecision {
        match self.current_stage() {
            ProcessStage::Pending => {
                if self.validate(tokens) {
                    self.set_stage(ProcessStage::Validated);
                    ProcessDecision::Continue {
                        stage: ProcessStage::Validated,
                        reason: "Validation passed".to_string(),
                    }
                } else {
                    self.set_stage(ProcessStage::Failed);
                    ProcessDecision::Fail {
                        stage: ProcessStage::Pending,
                        reason: "Validation failed - tokens not suitable for this handler"
                            .to_string(),
                    }
                }
            }
            ProcessStage::Validated => {
                // Check routing before extraction
                if let Some(target) = self.route() {
                    self.set_stage(ProcessStage::Routed);
                    return ProcessDecision::Route {
                        target_handler: target,
                        reason: "Routing to more specific handler".to_string(),
                    };
                }

                if self.extract(tokens) {
                    self.set_stage(ProcessStage::Extracted);
                    ProcessDecision::Continue {
                        stage: ProcessStage::Extracted,
                        reason: "Extraction successful".to_string(),
                    }
                } else {
                    self.set_stage(ProcessStage::Failed);
                    ProcessDecision::Fail {
                        stage: ProcessStage::Validated,
                        reason: "Extraction failed - could not parse token structure".to_string(),
                    }
                }
            }
            ProcessStage::Extracted => {
                if let Some(rust_code) = self.convert() {
                    self.set_stage(ProcessStage::Converted);
                    self.set_output(rust_code.clone());
                    ProcessDecision::Continue {
                        stage: ProcessStage::Converted,
                        reason: "Conversion successful".to_string(),
                    }
                } else {
                    self.set_stage(ProcessStage::Failed);
                    ProcessDecision::Fail {
                        stage: ProcessStage::Extracted,
                        reason: "Conversion failed - could not generate Rust code".to_string(),
                    }
                }
            }
            ProcessStage::Converted => {
                self.set_stage(ProcessStage::Complete);
                ProcessDecision::Complete {
                    rust_code: self.output().unwrap_or_default(),
                    confidence: self.confidence(),
                }
            }
            ProcessStage::Complete => ProcessDecision::Complete {
                rust_code: self.output().unwrap_or_default(),
                confidence: self.confidence(),
            },
            ProcessStage::Routed => ProcessDecision::Route {
                target_handler: self.route().unwrap_or_default(),
                reason: "Already routed".to_string(),
            },
            ProcessStage::Failed => ProcessDecision::Fail {
                stage: ProcessStage::Failed,
                reason: self.error().unwrap_or("Unknown error".to_string()),
            },
        }
    }

    // === State accessors (must be implemented) ===

    /// Get current processing stage
    fn current_stage(&self) -> ProcessStage;

    /// Set processing stage
    fn set_stage(&mut self, stage: ProcessStage);

    /// Get the generated output (if any)
    fn output(&self) -> Option<String>;

    /// Set the output
    fn set_output(&mut self, output: String);

    /// Get error message (if failed)
    fn error(&self) -> Option<String>;

    /// Set error message
    fn set_error(&mut self, error: String);

    /// Get confidence score (0.0 - 1.0)
    fn confidence(&self) -> f64;

    /// Set confidence score
    fn set_confidence(&mut self, confidence: f64);
}
