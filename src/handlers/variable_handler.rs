//! Variable handler implementation for C variable declarations and assignments
//! Specialized subhandler with adaptive learning capabilities
//! Handles: int x = 5; char* name = "test"; float values[10]; etc.

use crate::{
    report, Context, ConvertedElement, ConvertedGlobal, ExtractedElement, ExtractedGlobal, GlobalInfo, Handler, HandlerPhase, HandlerReport, HandlerResult, Handlizer, Id, RedirectRequest, ReportLevel, Result
};
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
    fn detect_variable_pattern(
        &self,
        context: &mut Context,
        tokens: &[crate::Token],
        token_range: Range<usize>,
        caller_id: Option<&str>,
    ) -> Result<(bool, f64, VariablePattern)> {
        // First use Patternizer for structural detection
        let pattern_match = context.patternizer.match_pattern("variable", tokens);

        // Then use Samplizer for confidence scoring
        let patterns = vec![
            "variable_declaration".to_string(),
            "variable_assignment".to_string(),
            "array_declaration".to_string(),
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

        // Combined heuristics for variable detection
        let (has_variable_structure, var_pattern) = self.analyze_variable_structure(tokens);
        let enhanced_confidence = if has_variable_structure {
            (adjusted_confidence * 1.3).min(0.95)
        } else {
            adjusted_confidence * 0.6
        };

        let pattern_success = !matches!(pattern_match, crate::PatternResult::NoMatch { .. });
        let can_process = pattern_success || (has_variable_structure && enhanced_confidence > 0.6);

        Ok((can_process, enhanced_confidence, var_pattern))
    }

    /// Analyze tokens for variable declaration/assignment patterns
    fn analyze_variable_structure(&self, tokens: &[crate::Token]) -> (bool, VariablePattern) {
        if tokens.len() < 2 {
            return (false, VariablePattern::Unknown);
        }

        let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        // Look for C type keywords at start
        for (i, token_str) in token_strings.iter().enumerate() {
            match token_str.as_str() {
                // C type declarations
                "int" | "long" | "short" | "char" | "float" | "double" | "void" | "unsigned"
                | "signed" | "const" | "static" | "extern" => {
                    if i + 1 < token_strings.len() {
                        let next_token = &token_strings[i + 1];

                        // Check for pointer declaration: int* ptr
                        if next_token == "*" && i + 2 < token_strings.len() {
                            return (true, VariablePattern::PointerDeclaration);
                        }

                        // Check for identifier
                        if next_token.chars().all(|c| c.is_alphanumeric() || c == '_') {
                            // Check what comes after identifier
                            if i + 2 < token_strings.len() {
                                match token_strings[i + 2].as_str() {
                                    "[" => return (true, VariablePattern::ArrayDeclaration),
                                    "=" => return (true, VariablePattern::Initialization),
                                    ";" => return (true, VariablePattern::Declaration),
                                    _ => {}
                                }
                            }
                            return (true, VariablePattern::Declaration);
                        }
                    }
                }
                _ => {}
            }
        }

        // Look for assignments (no type keyword)
        for (i, token_str) in token_strings.iter().enumerate() {
            if token_str == "=" && i > 0 && i + 1 < token_strings.len() {
                // Check if previous token is identifier
                let prev_token = &token_strings[i - 1];
                if prev_token.chars().all(|c| c.is_alphanumeric() || c == '_') {
                    return (true, VariablePattern::Assignment);
                }
            }
        }

        (false, VariablePattern::Unknown)
    }

    /// Convert C variable construct to Rust
    fn convert_variable_to_rust(
        &self,
        pattern: &VariablePattern,
        tokens: &[crate::Token],
    ) -> Result<String> {
        match pattern {
            VariablePattern::Declaration => self.convert_declaration(tokens),
            VariablePattern::Initialization => self.convert_initialization(tokens),
            VariablePattern::Assignment => self.convert_assignment(tokens),
            VariablePattern::ArrayDeclaration => self.convert_array_declaration(tokens),
            VariablePattern::PointerDeclaration => self.convert_pointer_declaration(tokens),
            VariablePattern::Unknown => {
                Ok("// Unknown variable pattern - manual conversion needed".to_string())
            }
        }
    }

    /// Convert C declaration: int x; -> let x: i32;
    fn convert_declaration(&self, tokens: &[crate::Token]) -> Result<String> {
        let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        if token_strings.len() >= 2 {
            let c_type = &token_strings[0];
            let var_name = &token_strings[1];
            let rust_type = self.convert_c_type_to_rust(c_type);
            Ok(format!("let {}: {};", var_name, rust_type))
        } else {
            Ok("let variable: i32; // Conversion needs manual review".to_string())
        }
    }

    /// Convert C initialization: int x = 5; -> let x: i32 = 5;
    fn convert_initialization(&self, tokens: &[crate::Token]) -> Result<String> {
        let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        if let Some(eq_pos) = token_strings.iter().position(|t| t == "=") {
            if eq_pos >= 2 {
                let c_type = &token_strings[0];
                let var_name = &token_strings[1];
                let rust_type = self.convert_c_type_to_rust(c_type);

                // Extract value after =
                let value_tokens = &token_strings[eq_pos + 1..];
                let value = value_tokens.join(" ").replace(";", "");

                Ok(format!("let {}: {} = {};", var_name, rust_type, value))
            } else {
                Ok("let variable: i32 = value; // Conversion needs manual review".to_string())
            }
        } else {
            self.convert_declaration(tokens)
        }
    }

    /// Convert C assignment: x = 5; -> x = 5;
    fn convert_assignment(&self, tokens: &[crate::Token]) -> Result<String> {
        let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        let assignment = token_strings.join(" ").replace(";", "");
        Ok(format!("{};", assignment))
    }

    /// Convert C array: int arr[10]; -> let arr: [i32; 10] = [0; 10];
    fn convert_array_declaration(&self, tokens: &[crate::Token]) -> Result<String> {
        let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        if token_strings.len() >= 4 {
            let c_type = &token_strings[0];
            let var_name = &token_strings[1];
            let rust_type = self.convert_c_type_to_rust(c_type);

            // Extract array size from [size]
            if let Some(bracket_pos) = token_strings.iter().position(|t| t == "[") {
                if bracket_pos + 1 < token_strings.len() {
                    let size = &token_strings[bracket_pos + 1];
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
    fn convert_pointer_declaration(&self, tokens: &[crate::Token]) -> Result<String> {
        let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        if token_strings.len() >= 3 {
            let c_type = &token_strings[0];
            let var_name = &token_strings[2]; // Skip the * token
            let rust_type = self.convert_c_type_to_rust(c_type);
            Ok(format!("let {}: *mut {};", var_name, rust_type))
        } else {
            Ok("let ptr: *mut i32; // Pointer conversion needs manual review".to_string())
        }
    }

    /// Convert C types to Rust types
    fn convert_c_type_to_rust(&self, c_type: &str) -> String {
        match c_type {
            "int" => "i32".to_string(),
            "long" => "i64".to_string(),
            "short" => "i16".to_string(),
            "char" => "i8".to_string(),
            "float" => "f32".to_string(),
            "double" => "f64".to_string(),
            "unsigned int" => "u32".to_string(),
            "unsigned long" => "u64".to_string(),
            "unsigned short" => "u16".to_string(),
            "unsigned char" => "u8".to_string(),
            "void" => "()".to_string(),
            _ => "i32".to_string(), // Default fallback
        }
    }

    /// Update success rate for specific caller
    pub fn update_caller_success(&mut self, caller_id: &str, success: bool) {
        let current_rate = self
            .caller_success_rates
            .get(caller_id)
            .copied()
            .unwrap_or(0.5);

        // Exponential moving average for adaptive learning
        let new_rate = if success {
            current_rate * 0.9 + 0.1 // Move toward 1.0 if successful
        } else {
            current_rate * 0.9 // Move toward 0.0 if failed
        };

        self.caller_success_rates
            .insert(caller_id.to_string(), new_rate);

        println!(
            "🧠 VariableHandler learning: {} success rate with {} = {:.2}",
            if success { "✅" } else { "❌" },
            caller_id,
            new_rate
        );
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

    fn can_process(
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
        let tokens = tokens;
        let (can_process, confidence, pattern) =
            self.detect_variable_pattern(context, &tokens, token_range, None)?;

        report!(
            context,
            "variable_handler",
            "can_process",
            ReportLevel::Info,
            HandlerPhase::Process,
            &format!(
                "Variable detection: {} ({:?}, confidence: {:.2})",
                can_process, pattern, confidence
            ),
            can_process
        );

        Ok(can_process)
    }

    fn extract(
        &self,
        context: &mut Context,
        token_slot: usize,
        token_range: Range<usize>,
    ) -> Result<Option<ExtractedElement>> {
        let tokens = context
            .tokenizer
            .get_tokens(token_slot, token_range.clone());
        if tokens.is_empty() {
            return Ok(None);
        }
        let (_, _, pattern) =
            self.detect_variable_pattern(context, &tokens, token_range.clone(), None)?;

        // Extract variable information from tokens
        let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        let var_name = token_strings.get(1).unwrap_or(&"unknown".to_string()).clone();
        let c_type = token_strings.get(0).unwrap_or(&"int".to_string()).clone();
        let rust_type = self.convert_c_type_to_rust(&c_type);
        
        // Generate Rust code based on pattern
        let rust_code = self.convert_variable_to_rust(&pattern, &tokens)?;

        let extracted_var = ExtractedGlobal {
            id: Id::get(Id::gen_name(&self.id().name()).as_str()),
            code: rust_code,
            info: GlobalInfo {
                name: var_name.clone(),
                var_type: rust_type.clone(),
                is_extern: false,
                is_static: false,
                is_const: matches!(pattern, VariablePattern::Declaration) && token_strings.contains(&"const".to_string()),
                storage_class: "auto".to_string(),
                initializer: if matches!(pattern, VariablePattern::Initialization | VariablePattern::Assignment) {
                    Some("value".to_string())
                } else {
                    None
                },
                complexity: "simple".to_string(),
                global_kind: "variable".to_string(),
                is_array: matches!(pattern, VariablePattern::ArrayDeclaration),
                is_volatile: token_strings.contains(&"volatile".to_string()),
                kind_description: format!("{:?}", pattern),
                type_name: c_type.clone(),
                dimensions: if matches!(pattern, VariablePattern::ArrayDeclaration) {
                    1
                } else {
                    0
                },
            },
            tokens: tokens.clone(),
            token_range,
            metadata: vec![
                ("handler".to_string(), "variable_handler".to_string()),
                ("pattern_type".to_string(), format!("{:?}", pattern)),
                ("original_c_type".to_string(), c_type),
                ("rust_type".to_string(), rust_type),
                ("adaptive_learning".to_string(), "enabled".to_string()),
            ],
        };

        Ok(Some(ExtractedElement::Global(extracted_var)))
    }

    fn convert(
        &self,
        context: &mut Context,
        token_slot: usize,
        token_range: Range<usize>,
    ) -> Result<Option<ConvertedElement>> {
        let tokens = context
            .tokenizer
            .get_tokens(token_slot, token_range.clone());
        if tokens.is_empty() {
            return Ok(None);
        }
        let (_, _, pattern) =
            self.detect_variable_pattern(context, &tokens, token_range.clone(), None)?;

        if tokens.is_empty() {
            return Ok(None);
        }
        let tokens = tokens;
        let (_, _, pattern) = self.detect_variable_pattern(context, &tokens, token_range, None)?;
        let rust_code = self.convert_variable_to_rust(&pattern, &tokens)?;

        let converted_var = ConvertedGlobal {
            var_type: format!("{:?}", pattern),
            initializer: Some("value".to_string()),
            code: rust_code,
            is_const: false,
            is_static: false,
            is_public: false,
            metadata: Vec::new(),
        };

        Ok(Some(ConvertedElement::Global(converted_var)))
    }

    fn report(&self, context: &mut Context) -> Result<HandlerReport> {
        let caller_count = self.caller_success_rates.len();

        Ok(HandlerReport::new(
            "variable_handler_report",
            self.id().into(),
            "variable_handler".to_string(),
            "report".to_string(),
            format!(
                "Variable handler with adaptive learning from {} callers",
                caller_count
            ),
            ReportLevel::Info,
            HandlerPhase::Report,
        )
        .with_success(true))
    }

    fn handle_redirect(
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

    fn process(
        &self,
        context: &mut Context,
        token_slot: usize,
        token_range: Range<usize>,
    ) -> Result<HandlerResult> {
        // Specialized variable processing with caller-aware adaptive learning
        let tokens = context
            .tokenizer
            .get_tokens(token_slot, token_range.clone());
        if tokens.is_empty() {
            return Ok(HandlerResult::NotHandled(
                None,
                token_range.clone(),
                self.id(),
            ));
        }
        let (can_process, confidence, pattern) = self.detect_variable_pattern(
            context,
            &tokens,
            token_range.clone(),
            Some("hierarchical_caller"),
        )?;

        if can_process {
            // Process the variable construct
            let extracted = self.extract(context, token_slot, token_range.clone())?;
            let converted = self.convert(context, token_slot, token_range.clone())?;

            // Log successful processing for adaptive learning
            println!(
                "🧠 VariableHandler successfully processed {:?} pattern with {:.2} confidence",
                pattern, confidence
            );

            // Generate rust code from the converted element
            let rust_code = if let Some(ConvertedElement::Global(var_element)) = converted {
                var_element.code
            } else {
                "// Variable conversion failed".to_string()
            };

            Ok(HandlerResult::Processed(
                Some(tokens),
                token_range,
                rust_code,
                self.id(),
            ))
        } else {
            // Redirect to fallback handler
            self.handle_redirect(
                context,
                token_slot,
                token_range.clone(),
                HandlerResult::NotHandled(None, token_range.clone(), self.id()),
            )
        }
    }
}

impl Default for VariableHandler {
    fn default() -> Self {
        Self::new()
    }
}

/// Factory function to create a variable handler
pub fn create_variable_handler() -> VariableHandler {
    VariableHandler::new()
}
