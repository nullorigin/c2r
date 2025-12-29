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
                        if rule.repeatable != REPEAT_NONE {
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
                        if rule.repeatable != REPEAT_NONE {
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
    fn route(&self, tokens: &[Token], source: &str) -> Option<String> {
        use crate::db::routing::Route;
        use crate::system::system;

        if tokens.is_empty() {
            return None;
        }

        let first = tokens[0].to_string();

        match first.as_str() {
            "break" => Some("break;".to_string()),
            "continue" => Some("continue;".to_string()),
            "return" => {
                let expr_tokens: Vec<Token> = tokens[1..]
                    .iter()
                    .filter(|t| t.to_string() != ";")
                    .cloned()
                    .collect();

                if expr_tokens.is_empty() {
                    return Some("return;".to_string());
                }

                use crate::handlers::expressions::ExpressionHandler;
                let route = Route::new(source, "ExpressionHandler").with_reason("return expression").with_range(0, expr_tokens.len());
                let _id = system().register_route(route);

                let mut handler = ExpressionHandler::new();
                if handler.validate(&expr_tokens) && handler.extract(&expr_tokens) {
                    if let Some(code) = handler.convert() {
                        return Some(format!("return {};", code));
                    }
                }

                let expr: Vec<String> = expr_tokens.iter().map(|t| t.to_string()).collect();
                Some(format!("return {};", expr.join(" ")))
            }
            "switch" => {
                use crate::handlers::conditionals::SwitchCaseHandler;
                let route = Route::new(source, "SwitchCaseHandler").with_reason("switch statement").with_range(0, tokens.len());
                let _id = system().register_route(route);
                let mut handler = SwitchCaseHandler::new();
                if handler.validate(tokens) && handler.extract(tokens) {
                    if let Some(code) = handler.convert() { return Some(code); }
                }
                None
            }
            "if" => {
                use crate::handlers::conditionals::IfElseHandler;
                let route = Route::new(source, "IfElseHandler").with_reason("if statement").with_range(0, tokens.len());
                let _id = system().register_route(route);
                let mut handler = IfElseHandler::new();
                if handler.validate(tokens) && handler.extract(tokens) {
                    if let Some(code) = handler.convert() { return Some(code); }
                }
                None
            }
            "for" | "while" | "do" => {
                use crate::handlers::loops::LoopHandler;
                let route = Route::new(source, "LoopHandler").with_reason("loop statement").with_range(0, tokens.len());
                let _id = system().register_route(route);
                let mut handler = LoopHandler::new();
                if handler.validate(tokens) && handler.extract(tokens) {
                    if let Some(code) = handler.convert() { return Some(code); }
                }
                None
            }
            "printf" | "fprintf" | "sprintf" | "snprintf" | "puts" => {
                use crate::handlers::expressions::StringFormatHandler;
                let route = Route::new(source, "StringFormatHandler").with_reason("printf-style call").with_range(0, tokens.len());
                let _id = system().register_route(route);
                let mut handler = StringFormatHandler::new();
                if handler.validate(tokens) && handler.extract(tokens) {
                    if let Some(code) = handler.convert() { return Some(format!("{};", code)); }
                }
                None
            }
            _ => {
                // Check for increment/decrement operators - route to expression handler
                let has_increment = tokens.iter().any(|t| {
                    let s = t.to_string();
                    matches!(s.as_str(), "++" | "--")
                });
                
                if has_increment {
                    let expr_tokens: Vec<Token> = tokens.iter().filter(|t| t.to_string() != ";").cloned().collect();
                    if !expr_tokens.is_empty() {
                        use crate::handlers::expressions::ExpressionHandler;
                        let route = Route::new(source, "ExpressionHandler").with_reason("increment/decrement").with_range(0, expr_tokens.len());
                        let _id = system().register_route(route);
                        let mut handler = ExpressionHandler::new();
                        if handler.validate(&expr_tokens) && handler.extract(&expr_tokens) {
                            if let Some(code) = handler.convert() { return Some(format!("{};", code)); }
                        }
                    }
                }

                // Check for compound assignment early
                let has_compound_assign = tokens.iter().any(|t| {
                    let s = t.to_string();
                    matches!(s.as_str(), "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>=")
                });

                // Check positions of [ and = to distinguish array declarations from array access
                let bracket_pos = tokens.iter().position(|t| t.to_string() == "[");
                let eq_pos = tokens.iter().position(|t| t.to_string() == "=");
                let arrow_pos = tokens.iter().position(|t| t.to_string() == "->");
                let spaced_arrow = tokens.windows(2).position(|w| w[0].to_string() == "-" && w[1].to_string() == ">");
                
                // Array declaration: [ must appear before = (or no =) - this is type name[size]
                // If [ appears after =, it's an array access in an initializer (e.g., x = arr[i])
                // If -> appears before [, it's a member array access (e.g., ptr->field[i] = val)
                let has_arrow_before_bracket = match (arrow_pos.or(spaced_arrow), bracket_pos) {
                    (Some(a), Some(b)) => a < b,
                    _ => false,
                };
                let is_array_decl = match (bracket_pos, eq_pos) {
                    (Some(b), Some(e)) => b < e && !has_arrow_before_bracket,  // [ before = means array declaration, unless -> before [
                    (Some(_), None) => !has_arrow_before_bracket,               // [ with no = is also array declaration, unless -> before [
                    _ => false,
                };

                // Variable declaration with array (but NOT compound assignments, NOT array access)
                if is_array_decl && !has_compound_assign {
                    use crate::handlers::arrays::ArrayHandler;
                    let route = Route::new(source, "ArrayHandler").with_reason("array declaration").with_range(0, tokens.len());
                    let _id = system().register_route(route);
                    let mut handler = ArrayHandler::new();
                    if handler.validate(tokens) && handler.extract(tokens) {
                        if let Some(code) = handler.convert() { return Some(code); }
                    }
                }

                // Variable declaration
                let has_simple_assign = tokens.iter().any(|t| t.to_string() == "=");
                if (has_simple_assign && !has_compound_assign) || self.is_type_keyword(&first) {
                    use crate::handlers::variables::VariableHandler;
                    let route = Route::new(source, "VariableHandler").with_reason("variable declaration").with_range(0, tokens.len());
                    let _id = system().register_route(route);
                    let mut handler = VariableHandler::new();
                    if handler.validate(tokens) && handler.extract(tokens) {
                        if let Some(code) = handler.convert() { return Some(code); }
                    }
                }

                // Function call
                if tokens.len() >= 3 {
                    if let Some(paren_pos) = tokens.iter().position(|t| t.to_string() == "(") {
                        if paren_pos > 0 {
                            use crate::handlers::functions::CallHandler;
                            let route = Route::new(source, "CallHandler").with_reason("function call").with_range(0, tokens.len());
                            let _id = system().register_route(route);
                            let mut handler = CallHandler::new();
                            if handler.validate(tokens) && handler.extract(tokens) {
                                if let Some(code) = handler.convert() { return Some(format!("{};", code)); }
                            }
                        }
                    }
                }

                // Fallback: expression
                let expr_tokens: Vec<Token> = tokens.iter().filter(|t| t.to_string() != ";").cloned().collect();
                if !expr_tokens.is_empty() {
                    use crate::handlers::expressions::ExpressionHandler;
                    let route = Route::new(source, "ExpressionHandler").with_reason("expression").with_range(0, expr_tokens.len());
                    let _id = system().register_route(route);
                    let mut handler = ExpressionHandler::new();
                    if handler.validate(&expr_tokens) && handler.extract(&expr_tokens) {
                        if let Some(code) = handler.convert() { return Some(format!("{};", code)); }
                    }
                }

                // Ultimate fallback
                let strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
                Some(format!("// {}", strs.join(" ")))
            }
        }
    }

    /// Check if a token is a C type keyword
    fn is_type_keyword(&self, token: &str) -> bool {
        crate::db::keyword::is_c_type_keyword(token)
            || matches!(token, "struct" | "enum" | "union")
            || crate::system::system().lookup_type(token).is_some()
    }

    /// Route a statement to the appropriate handler.
    /// Wrapper around route() that returns String instead of Option<String>.
    fn route_statement(&self, tokens: &[Token], source: &str) -> String {
        self.route(tokens, source).unwrap_or_else(|| {
            let strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
            format!("// {}", strs.join(" "))
        })
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
                // Proceed directly to extraction (routing is handled by route_statement for sub-constructs)
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
                // Register this handler to the database if auto-registration is enabled
                self.register();
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
                target_handler: self.route(tokens, self.name().unwrap_or("unknown")).unwrap_or_default(),
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
