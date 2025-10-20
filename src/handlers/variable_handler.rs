//! Variable handler implementation for C variable declarations and assignments
//! Specialized subhandler with adaptive learning capabilities
//! Handles: int x = 5; char* name = "test"; float values[10]; etc.

use crate::{
    gen_name, C2RError, Context, ConvertedElement,
    ConvertedVariable, ExtractedElement, ExtractedVariable,
    Handler, HandlerResult, Id, Phase, Reason, RedirectRequest,
    Report, ReportLevel, Result, VariableInfo,
};
use crate::{Token, TYPE_CONVERSION_MAP};
use std::collections::HashMap;
use std::ops::Range;

/// Variable pattern types we can detect
#[derive(Debug, Clone, PartialEq)]
enum VariablePattern {
    Declaration,        // int x;
    Initialization,     // int x = 5;
    Assignment,         // x = 5;
    ArrayDeclaration,   // int arr[10];
    PointerDeclaration, // int* ptr;
    Unknown,
}

/// Variable handler with adaptive learning from parent handlers
#[derive(Debug, Clone)]
pub struct VariableHandler {
    /// Track success rates with different calling handlers
    caller_success_rates: HashMap<String, f64>,
}

impl VariableHandler {
    pub fn new() -> Self {
        Self {
            caller_success_rates: HashMap::new(),
        }
    }

    /// Detect variable patterns with caller-aware adaptive learning
    fn detect_pattern(
        &self,
        context: &mut Context,
        tokens: &[Token],
        token_range: Range<usize>,
        caller_id: Option<&str>,
    ) -> Result<(bool, f64, VariablePattern)> {
        // Try multiple registered patterns for variable declarations
        let mut pattern_success = false;
        let pattern_names = vec!["variable", "global_variable", "static_variable"];

        for pattern_name in &pattern_names {
            let pattern_match = context.patternizer.match_pattern(pattern_name, tokens);
            if !matches!(pattern_match, crate::PatternResult::NoMatch { .. }) {
                pattern_success = true;
                break;
            }
        }

        // Use Samplizer for confidence scoring with available patterns
        let patterns = vec![
            "variable".to_string(),
            "global_variable".to_string(),
        ];
        let confidence = context
            .samplizer
            .analyze_with_range(&patterns, tokens, token_range)?;

        // Apply caller-specific confidence adjustment
        let adjusted_confidence = if let Some(caller) = caller_id {
            if let Some(&success_rate) = self.caller_success_rates.get(caller) {
                confidence * (0.5 + success_rate) // Scale based on historical success
            } else {
                confidence * 0.8 // New caller - slightly conservative
            }
        } else {
            confidence
        };

        // Determine if processing is viable based on pattern match and confidence
        let can_process = pattern_success || adjusted_confidence > 0.6;

        // Determine actual pattern from token analysis
        let detected_pattern = self.detect_pattern_type(tokens);

        Ok((can_process, adjusted_confidence, detected_pattern))
    }

    /// Determine variable pattern from tokens and extract complete variable info
    fn detect_pattern_type(&self, tokens: &[Token]) -> VariablePattern {
        if tokens.len() < 2 {
            return VariablePattern::Unknown;
        }

        // Reject if first token is a number (not a type or keyword)
        let first_token = tokens[0].to_string();
        if first_token.chars().all(|c| c.is_numeric()) {
            return VariablePattern::Unknown;
        }

        // Reject common non-variable keywords
        if matches!(first_token.as_str(), "return" | "if" | "while" | "for" | "switch" | "case") {
            return VariablePattern::Unknown;
        }

        // Check for pointer
        if tokens.iter().any(|t| t.to_string() == "*") {
            return VariablePattern::PointerDeclaration;
        }

        // Check for array
        if tokens.iter().any(|t| t.to_string() == "[") {
            return VariablePattern::ArrayDeclaration;
        }

        // Check for initialization vs declaration
        if tokens.iter().any(|t| t.to_string() == "=") {
            // Check if first token looks like a type (indicates declaration with init)
            if tokens.len() >= 3 {
                return VariablePattern::Initialization;
            } else {
                return VariablePattern::Assignment;
            }
        }

        VariablePattern::Declaration
    }

    /// Extract comprehensive variable information from tokens
    pub fn extract_info(&self, context: &mut Context, tokens: &[Token]) -> Result<VariableInfo> {
        if tokens.is_empty() {
            return Err(C2RError::new(
                crate::Kind::Empty,
                Reason::Input("token slice"),
                Some("No tokens provided for variable extraction".to_string()),
            ));
        }

        let pattern = self.detect_pattern_type(tokens);
        let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        let mut var_info = VariableInfo::default();

        // Extract variable name with improved pattern matching
        var_info.name = match pattern {
            VariablePattern::Declaration | VariablePattern::Initialization => {
                if let Some(eq_pos) = token_strings.iter().position(|t| t == "=") {
                    // Initialization: name is before =
                    token_strings.get(eq_pos.saturating_sub(1))
                        .unwrap_or(&"unknown".to_string())
                        .clone()
                } else {
                    // Declaration: name is last meaningful token
                    token_strings.last()
                        .unwrap_or(&"unknown".to_string())
                        .replace(";", "")
                }
            }
            VariablePattern::Assignment => {
                token_strings.first()
                    .unwrap_or(&"unknown".to_string())
                    .clone()
            }
            VariablePattern::ArrayDeclaration => {
                if let Some(bracket_pos) = token_strings.iter().position(|t| t == "[") {
                    token_strings.get(bracket_pos.saturating_sub(1))
                        .unwrap_or(&"unknown".to_string())
                        .clone()
                } else {
                    "unknown".to_string()
                }
            }
            VariablePattern::PointerDeclaration => {
                token_strings.last()
                    .unwrap_or(&"unknown".to_string())
                    .replace(";", "")
            }
            _ => {
                token_strings.get(1)
                    .unwrap_or(&"unknown".to_string())
                    .clone()
            }
        };

        // Extract type information with better error handling
        var_info.var_type = match pattern {
            VariablePattern::Declaration | VariablePattern::Initialization => {
                if let Some(eq_pos) = token_strings.iter().position(|t| t == "=") {
                    if eq_pos >= 2 {
                        let type_tokens = &tokens[0..eq_pos - 1];
                        self.convert_type(type_tokens)
                    } else {
                        "i32".to_string() // Default fallback
                    }
                } else if tokens.len() >= 2 {
                    let type_tokens = &tokens[0..tokens.len() - 1];
                    self.convert_type(type_tokens)
                } else {
                    "i32".to_string()
                }
            }
            VariablePattern::ArrayDeclaration => {
                if let Some(bracket_pos) = token_strings.iter().position(|t| t == "[") {
                    if bracket_pos > 1 {
                        let type_tokens = &tokens[0..bracket_pos - 1];
                        format!("[{}; N]", self.convert_type(type_tokens))
                    } else {
                        "[i32; N]".to_string()
                    }
                } else {
                    "[i32; N]".to_string()
                }
            }
            VariablePattern::PointerDeclaration => {
                if tokens.len() >= 2 {
                    let type_tokens = &tokens[0..tokens.len() - 1];
                    format!("*mut {}", self.convert_type(type_tokens).replace("*", "").trim())
                } else {
                    "*mut i32".to_string()
                }
            }
            _ => {
                if !tokens.is_empty() {
                    self.convert_type(&tokens[0..1])
                } else {
                    "i32".to_string()
                }
            }
        };

        // Set boolean flags
        var_info.is_const = token_strings.contains(&"const".to_string());
        var_info.is_static = token_strings.contains(&"static".to_string());
        var_info.is_extern = token_strings.contains(&"extern".to_string());
        var_info.is_volatile = token_strings.contains(&"volatile".to_string());
        var_info.is_array = matches!(pattern, VariablePattern::ArrayDeclaration);

        // Extract initializer value
        var_info.initializer = if let Some(eq_pos) = token_strings.iter().position(|t| t == "=") {
            if eq_pos + 1 < token_strings.len() {
                Some(token_strings[eq_pos + 1..].join(" ").replace(";", ""))
            } else {
                None
            }
        } else {
            None
        };

        // Set metadata fields
        var_info.kind_description = format!("{:?}", pattern);
        var_info.complexity = if tokens.len() > 5 { "complex" } else { "simple" }.to_string();
        var_info.storage_class = if var_info.is_static {
            "static"
        } else if var_info.is_extern {
            "extern"
        } else {
            "auto"
        }.to_string();
        var_info.global_kind = "variable".to_string();

        Ok(var_info)
    }


    /// Convert C variable construct to Rust
    fn convert_tokens(
        &self,
        pattern: &VariablePattern,
        tokens: &[Token],
    ) -> Result<String> {
        if tokens.is_empty() {
            return Err(C2RError::new(
                crate::Kind::Empty,
                Reason::Input("tokens"),
                Some("No tokens provided for variable conversion".to_string()),
            ));
        }

        let result = match pattern {
            VariablePattern::Declaration => self.convert_declaration(tokens),
            VariablePattern::Initialization => self.convert_initialization(tokens),
            VariablePattern::Assignment => self.convert_assignment(tokens),
            VariablePattern::ArrayDeclaration => self.convert_array_declaration(tokens),
            VariablePattern::PointerDeclaration => self.convert_pointer_declaration(tokens),
            VariablePattern::Unknown => {
                eprintln!("⚠️  Unknown variable pattern for tokens: {:?}",
                          tokens.iter().map(|t| t.to_string()).collect::<Vec<_>>());
                Ok("// Unknown variable pattern - manual conversion needed".to_string())
            }
        };

        // Log the conversion for debugging
        if let Ok(ref code) = result {
            eprintln!("   🔄 Pattern {:?} converted to: {}", pattern, code);
        }

        result
    }

    /// Convert C declaration: int x; -> let x: i32;
    fn convert_declaration(&self, tokens: &[Token]) -> Result<String> {
        // Need at least type + name + semicolon (3 tokens minimum)
        if tokens.len() < 3 {
            return Err(C2RError::new(
                crate::Kind::Invalid,
                Reason::Input("tokens"),
                Some(format!("Insufficient tokens for variable declaration: {}", tokens.len())),
            ));
        }

        // Filter out semicolons and get actual tokens
        let actual_tokens: Vec<Token> = tokens.iter()
            .filter(|t| t.to_string() != ";")
            .cloned()
            .collect();

        if actual_tokens.len() < 2 {
            return Err(C2RError::new(
                crate::Kind::Invalid,
                Reason::Input("tokens"),
                Some("Need at least type and name".to_string()),
            ));
        }

        let c_type = &actual_tokens[0..actual_tokens.len() - 1];
        let var_name = &actual_tokens[actual_tokens.len() - 1];
        let rust_type = self.convert_type(c_type);
        Ok(format!("let {}: {};", var_name, rust_type))
    }

    /// Convert C initialization: int x = 5; -> let x: i32 = 5;
    fn convert_initialization(&self, tokens: &[Token]) -> Result<String> {
        if let Some(eq_pos) = tokens.iter().position(|t| t.to_string() == "=") {
            if eq_pos >= 2 {
                let c_type = &tokens[0..eq_pos - 1];
                let var_name = &tokens[eq_pos - 1];
                let rust_type = self.convert_type(c_type);

                // Extract value after =
                let value_tokens = &tokens[eq_pos + 1..];
                let value = value_tokens.iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
                    .replace(";", "");

                Ok(format!("let {}: {} = {};", var_name, rust_type, value))
            } else {
                Ok("let variable: i32 = value; // Conversion needs manual review".to_string())
            }
        } else {
            self.convert_declaration(tokens)
        }
    }

    /// Convert C assignment: x = 5; -> x = 5;
    fn convert_assignment(&self, tokens: &[Token]) -> Result<String> {
        if tokens.is_empty() {
            return Ok("// Empty assignment - manual conversion needed".to_string());
        }

        // Find the assignment operator
        if let Some(eq_pos) = tokens.iter().position(|t| t == &Token::l("=")) {
            if eq_pos > 0 && eq_pos + 1 < tokens.len() {
                let var_name = &tokens[0];
                let value_tokens = &tokens[eq_pos + 1..];
                let value = value_tokens.iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
                    .replace(";", "");

                Ok(format!("{} = {};", var_name, value))
            } else {
                Ok("// Invalid assignment - manual conversion needed".to_string())
            }
        } else {
            // No assignment operator found
            let assignment = tokens.iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(" ")
                .replace(";", "");
            Ok(format!("{};", assignment))
        }
    }

    /// Convert C array: int arr[10]; -> let arr: [i32; 10] = [0; 10];
    fn convert_array_declaration(&self, tokens: &[Token]) -> Result<String> {
        if tokens.len() >= 4 {
            // Find the variable name (token before the opening bracket)
            if let Some(bracket_pos) = tokens.iter().position(|t| t.to_string() == "[") {
                if bracket_pos > 0 && bracket_pos + 2 < tokens.len() {
                    let c_type = &tokens[0..bracket_pos];
                    let var_name = &tokens[bracket_pos - 1];
                    let size = &tokens[bracket_pos + 1];
                    let rust_type = self.convert_type(c_type);

                    return Ok(format!(
                        "let {}: [{}; {}] = [Default::default(); {}];",
                        var_name, rust_type, size, size
                    ));
                }
            }
        }

        Ok("let array: [i32; 10] = [0; 10]; // Array conversion needs manual review".to_string())
    }

    /// Convert C pointer: int* ptr; -> let ptr: *mut i32;
    fn convert_pointer_declaration(&self, tokens: &[Token]) -> Result<String> {
        if tokens.len() >= 3 {
            let var_name = &tokens[tokens.len() - 1];
            let c_type = &tokens[0..tokens.len() - 2];
            let rust_type = self.convert_type(c_type);
            Ok(format!("let {}: {} = std::ptr::null_mut();", var_name, rust_type))
        } else {
            Ok("let ptr: *mut i32; // Pointer conversion needs manual review".to_string())
        }
    }

    /// Convert C types to Rust types using comprehensive type map
    fn convert_type(&self, tokens: &[Token]) -> String {
        // Use the comprehensive type conversion map from common.rs
        let mut c_type = "".to_string();
        for token in tokens {
            let c_string = token.to_string();
            match c_string.as_str() {
                "*" | "**" | "&" | "&&" => c_type.push_str(&c_string),
                _ => if !c_string.is_empty() {
                    c_type.push_str(&c_string);
                    c_type.push(' ');
                } else {
                    continue;
                },
            }
        }
        let c_type = c_type.trim();
        TYPE_CONVERSION_MAP.convert_type(c_type)
            .unwrap_or_else(|| {
                eprintln!("⚠️  Unknown C type '{}', defaulting to i32", c_type);
                "i32".to_string()
            })
    }
}

impl Handler for VariableHandler {
    fn id(&self) -> Id {
        Id::get("variable_handler")
    }

    fn role(&self) -> String {
        "variable".to_string()
    }

    fn priority(&self) -> u64 {
        140
    }

    fn supported_patterns(&self) -> Vec<String> {
        vec![
            "variable_declaration".to_string(),
            "variable_assignment".to_string(),
            "array_declaration".to_string(),
            "pointer_declaration".to_string(),
            "initialization".to_string(),
        ]
    }

    fn supported_keywords(&self) -> Vec<String> {
        vec![
            "int".to_string(),
            "char".to_string(),
            "float".to_string(),
            "double".to_string(),
            "long".to_string(),
            "short".to_string(),
            "unsigned".to_string(),
            "signed".to_string(),
            "auto".to_string(),
            "register".to_string(),
        ]
    }

    fn detect(
        &self,
        context: &mut Context,
        token_slot: usize,
        token_range: Range<usize>,
    ) -> Result<bool> {
        let tokens = context
            .tokenizer
            .get_tokens(token_slot, token_range.clone());

        if tokens.is_empty() {
            return Ok(false);
        }

        let (can_process, confidence, pattern) =
            self.detect_pattern(context, &tokens, token_range.clone(), None)?;

        context.registry.add_report(
            Report::new(
                Id::get(&format!("variable_handler_can_process_{}", token_slot)),
                Some(self.id()),
                "can_process".to_string(),
                format!(
                    "Variable detection: {} ({:?}, confidence: {:.2})",
                    can_process, pattern, confidence
                ),
                ReportLevel::Info,
                Phase::Process(None),
            )
                .with_tokens(tokens.len(), if can_process { tokens.len() } else { 0 })
                .with_success(can_process),
        );

        Ok(can_process)
    }

    fn extract(
        &self,
        context: &mut Context,
        token_slot: usize,
        token_range: Range<usize>,
    ) -> Result<Option<ExtractedElement>> {
        eprintln!("📦 variable_handler::extract called! slot={}, range={:?}", token_slot, token_range);

        let tokens = context
            .tokenizer
            .get_tokens(token_slot, token_range.clone());
        if tokens.is_empty() {
            eprintln!("   ❌ No tokens for extraction");
            return Ok(None);
        }

        let var_info = self.extract_info(context, &tokens)?;
        let (_, _, pattern) = self.detect_pattern(context, &tokens, token_range.clone(), None)?;

        let extracted_var = ExtractedVariable {
            id: Id::get(&gen_name(&self.id().name()).as_str()),
            info: var_info,
            tokens: tokens.clone(),
            token_range,
            metadata: vec![
                ("handler".to_string(), "variable_handler".to_string()),
                ("pattern_type".to_string(), format!("{:?}", pattern)),
                ("adaptive_learning".to_string(), "enabled".to_string()),
            ],
        };

        eprintln!("   ✅ Extraction complete: variable '{}'", extracted_var.info.name);
        Ok(Some(ExtractedElement::Variable(extracted_var)))
    }

    fn convert(
        &self,
        context: &mut Context,
        element: ExtractedElement,
    ) -> Result<Option<ConvertedElement>> {
        eprintln!("🔄 variable_handler::convert called!");

        if let ExtractedElement::Variable(extracted) = element {
            let tokens = extracted.tokens.clone();
            if tokens.is_empty() {
                return Ok(None);
            }

            let (_, _, pattern) = self.detect_pattern(
                context,
                &tokens,
                extracted.token_range.clone(),
                None,
            )?;

            let rust_code = self.convert_tokens(&pattern, &tokens)?;
            eprintln!("   ✅ Conversion complete: {}", rust_code);

            let converted_var = ConvertedVariable {
                code: rust_code,
                metadata: vec![
                    ("original_c_type".to_string(), extracted.info.type_name.clone()),
                    ("rust_type".to_string(), extracted.info.var_type.clone()),
                    ("pattern_type".to_string(), format!("{:?}", pattern)),
                ],
            };

            Ok(Some(ConvertedElement::Variable(converted_var)))
        } else {
            Ok(None)
        }
    }

    fn report(&self, context: &mut Context) -> Result<Report> {
        let reports = context.registry.get_reports_by_handler(&self.id().name());

        if reports.is_empty() {
            return Ok(Report::new(
                Id::get("variable_handler_report"),
                Some(self.id()),
                "report".to_string(),
                "No variable processing reports found".to_string(),
                ReportLevel::Info,
                Phase::Report(None),
            )
                .with_success(true));
        }

        let (info_count, warning_count, error_count) =
            reports
                .iter()
                .fold((0, 0, 0), |(info, warn, err), report| match report.level {
                    ReportLevel::Info => (info + 1, warn, err),
                    ReportLevel::Warning => (info, warn + 1, err),
                    ReportLevel::Error => (info, warn, err + 1),
                    ReportLevel::Debug => (info + 1, warn, err),
                });

        let total_tokens_processed = reports.iter().map(|r| r.tokens_processed).sum::<usize>();
        let caller_count = self.caller_success_rates.len();

        Ok(Report::new(
            Id::get("variable_handler_report"),
            Some(self.id()),
            "report".to_string(),
            format!(
                "Variable handler processed {} variables: {} info, {} warnings, {} errors (adaptive learning from {} callers)",
                reports.len(),
                info_count,
                warning_count,
                error_count,
                caller_count
            ),
            if error_count > 0 {
                ReportLevel::Error
            } else if warning_count > 0 {
                ReportLevel::Warning
            } else {
                ReportLevel::Info
            },
            Phase::Report(None),
        )
            .with_success(error_count == 0)
            .with_tokens(total_tokens_processed, total_tokens_processed))
    }

    fn route(
        &self,
        context: &mut Context,
        token_slot: usize,
        token_range: Range<usize>,
        result: HandlerResult,
    ) -> Result<HandlerResult> {
        // Variable handler is quite specialized - suggest statement or expression handlers
        let redirect_request = RedirectRequest {
            from_handler: self.id(),
            token_range: token_range.clone(),
            failed_patterns: self.supported_patterns(),
            suggested_handler: Some(Id::get("statement_handler")), // Fallback to statement handler
            metadata: Vec::new(),
        };

        // Return the result as-is to avoid deadlock in redirect workflow
        Ok(result)
    }
}
impl Default for VariableHandler {
    fn default() -> Self {
        Self::new()
    }
}
