//! Array handler implementation using trait-based system
//! Uses Patternizer and Samplizer for intelligent array pattern detection and processing

use crate::{
    ArrayInfo, Context, ConvertedArray, ConvertedElement, ElementInfo, ExtractedArray, ExtractedElement, HandlerPhase, HandlerReport, HandlerResult, Handlizer, Id, RedirectRequest, ReportLevel, Result, Token, context, handler::Handler, report
};
use core::{option::Option::Some, result::Result::Ok};
use std::{ops::Range, sync::Arc};
/// Array handler struct implementing the Handler trait
#[derive(Debug, Clone)]
pub struct ArrayHandler;

impl ArrayHandler {
    pub fn new() -> Self {
        Self
    }
    
    /// Enhanced pattern detection using both Patternizer and Samplizer
    fn detect_array_pattern(
        &self,
        patternizer: &mut crate::Patternizer,
        samplizer: &crate::Samplizer,
        tokens: &[Token],
        token_range: Range<usize>,
    ) -> Result<(bool, f64)> {
        // First use Patternizer for structural detection
        let pattern_match = patternizer.match_pattern("array", tokens);
        
        // Then use Samplizer for confidence scoring
        let patterns = vec!["array_declaration".to_string(), "array_initialization".to_string()];
        let confidence = samplizer.analyze_with_range(&patterns, tokens, token_range)?;
        
        // Combined heuristics for array detection
        let has_array_structure = self.has_array_structure(tokens);
        let enhanced_confidence = if has_array_structure {
            confidence * 1.3  // Boost confidence if structure looks array-like
        } else {
            confidence * 0.7  // Reduce if structure doesn't match
        };
        
        let pattern_success = !matches!(pattern_match, crate::PatternResult::NoMatch { .. });
        
        Ok((
            pattern_success || enhanced_confidence > 0.6,
            enhanced_confidence.min(1.0),
        ))
    }
    
    /// Analyze token structure for array-like patterns
    fn has_array_structure(&self, tokens: &[Token]) -> bool {
        if tokens.len() < 3 {
            return false;
        }
        
        let mut has_brackets = false;
        let mut has_identifier = false;
        let mut bracket_after_identifier = false;
        let mut has_type_keyword = false;
        
        for (i, token) in tokens.iter().enumerate() {
            let token_str = token.to_string();
            
            // Look for square brackets
            if token_str == "[" || token_str == "]" {
                has_brackets = true;
            }
            
            // Check for C type keywords
            if matches!(
                token_str.as_str(),
                "int" | "char" | "float" | "double" | "long" | "short" | "void" | "signed" | "unsigned"
            ) {
                has_type_keyword = true;
            }
            
            // Look for identifier followed by brackets
            if self.is_valid_identifier(&token_str) {
                has_identifier = true;
                
                // Check if brackets appear after identifier
                for j in (i + 1)..tokens.len() {
                    let next_token = tokens[j].to_string();
                    if next_token == "[" {
                        bracket_after_identifier = true;
                        break;
                    }
                    if !self.is_whitespace(&next_token) {
                        break;
                    }
                }
            }
        }
        
        has_brackets && has_identifier && bracket_after_identifier && has_type_keyword
    }
    
    /// Check if token is a valid identifier
    fn is_valid_identifier(&self, token: &str) -> bool {
        if token.is_empty() {
            return false;
        }
        
        let first_char = token.chars().next().unwrap();
        if !first_char.is_alphabetic() && first_char != '_' {
            return false;
        }
        
        token.chars().all(|c| c.is_alphanumeric() || c == '_') &&
        !matches!(
            token,
            "int" | "char" | "float" | "double" | "long" | "short" | "void" |
            "static" | "const" | "extern" | "signed" | "unsigned" | "struct" |
            "union" | "enum" | "typedef"
        )
    }
    
    /// Check if token is whitespace
    fn is_whitespace(&self, token: &str) -> bool {
        token.chars().all(|c| c.is_whitespace())
    }
    
    /// Extract array information from tokens
    fn extract_array_info(&self, tokens: &[crate::Token]) -> Result<ArrayInfo> {
        let mut array_info = ArrayInfo::default();
        
        let mut in_brackets = false;
        let mut bracket_content = String::new();
        let mut found_type = false;
        
        for token in tokens {
            let token_str = token.to_string();
            
            match token_str.as_str() {
                "static" | "const" | "extern" => {
                    array_info.storage_class = Some(token_str);
                }
                "[" => {
                    in_brackets = true;
                    bracket_content.clear();
                }
                "]" => {
                    in_brackets = false;
                    if !bracket_content.trim().is_empty() {
                        array_info.size = Some(bracket_content.trim().to_string());
                    }
                }
                "=" => {
                    array_info.has_initializer = true;
                }
                _ => {
                    if in_brackets {
                        bracket_content.push_str(&token_str);
                    } else if !found_type && self.is_c_type(&token_str) {
                        found_type = true;
                    } else if array_info.name.is_empty() && self.is_valid_identifier(&token_str) {
                        array_info.name = token_str;
                    }
                }
            }
        }
        
        Ok(array_info)
    }
    
    /// Check if token is a C type
    fn is_c_type(&self, token: &str) -> bool {
        matches!(
            token,
            "int" | "char" | "float" | "double" | "long" | "short" | "void" |
            "signed" | "unsigned"
        ) || token.starts_with("unsigned ")
    }
    
    /// Convert array to Rust code
    fn convert_to_rust(&self, array_info: &ArrayInfo, tokens: &[crate::Token]) -> Result<String> {
        let rust_name = self.convert_identifier_to_rust(&array_info.name);
        
        // Extract element type from tokens
        let element_type = self.extract_element_type(tokens)?;
        let rust_element_type = self.convert_c_type_to_rust(&element_type);
        
        let array_type = if let Some(size) = &array_info.size {
            if size.parse::<usize>().is_ok() {
                format!("[{}; {}]", rust_element_type, size)
            } else {
                format!("Vec<{}>", rust_element_type)
            }
        } else {
            format!("Vec<{}>", rust_element_type)
        };
        
        let initializer = if array_info.has_initializer {
            if let Some(size) = &array_info.size {
                if size.parse::<usize>().is_ok() {
                    format!(" = [Default::default(); {}]", size)
                } else {
                    " = Vec::new()".to_string()
                }
            } else {
                " = Vec::new()".to_string()
            }
        } else {
            String::new()
        };
        
        let (storage_modifier, mutability) = match array_info.storage_class.as_deref() {
            Some("static") => ("static ", "mut "),
            Some("const") => ("const ", ""),
            _ => ("let ", "mut "),
        };
        
        Ok(format!(
            "{}{}{}: {}{}",
            storage_modifier, mutability, rust_name, array_type, initializer
        ))
    }
    
    /// Extract element type from array declaration tokens
    fn extract_element_type(&self, tokens: &[crate::Token]) -> Result<String> {
        let mut found_unsigned = false;
        let mut found_signed = false;
        let mut found_long = false;
        let mut found_short = false;
        
        for token in tokens {
            let token_str = token.to_string();
            match token_str.as_str() {
                "unsigned" => {
                    found_unsigned = true;
                }
                "signed" => {
                    found_signed = true;
                }
                "long" => {
                    found_long = true;
                }
                "short" => {
                    found_short = true;
                }
                "int" => {
                    return Ok(self.build_type_string(found_unsigned, found_signed, found_long, found_short, "int"));
                }
                "char" => {
                    return Ok(if found_unsigned {
                        "unsigned char".to_string()
                    } else if found_signed {
                        "signed char".to_string()
                    } else {
                        "char".to_string()
                    });
                }
                "float" | "double" | "void" => {
                    return Ok(token_str);
                }
                _ => continue,
            }
        }
        
        // Handle implicit int with modifiers
        if found_unsigned || found_signed || found_long || found_short {
            Ok(self.build_type_string(found_unsigned, found_signed, found_long, found_short, "int"))
        } else {
            Ok("int".to_string())
        }
    }
    
    /// Build type string from modifiers
    fn build_type_string(&self, unsigned: bool, signed: bool, long: bool, short: bool, base_type: &str) -> String {
        let mut parts = Vec::new();
        
        if unsigned {
            parts.push("unsigned");
        } else if signed {
            parts.push("signed");
        }
        
        if long {
            parts.push("long");
        } else if short {
            parts.push("short");
        }
        
        parts.push(base_type);
        parts.join(" ")
    }
    
    /// Convert C type to Rust type
    fn convert_c_type_to_rust(&self, c_type: &str) -> String {
        match c_type.trim() {
            "int" | "signed int" => "i32",
            "long" | "long int" | "signed long" | "signed long int" => "i64",
            "short" | "short int" | "signed short" | "signed short int" => "i16",
            "char" | "signed char" => "i8",
            "unsigned int" => "u32",
            "unsigned long" | "unsigned long int" => "u64",
            "unsigned short" | "unsigned short int" => "u16",
            "unsigned char" => "u8",
            "float" => "f32",
            "double" => "f64",
            "void" => "()",
            _ => {
                // Handle pointer types
                if c_type.ends_with('*') {
                    "*mut std::os::raw::c_void"
                } else {
                    "i32" // Default fallback
                }
            }
        }.to_string()
    }
    
    /// Convert C identifier to Rust naming convention (snake_case)
    fn convert_identifier_to_rust(&self, identifier: &str) -> String {
        if identifier.is_empty() {
            return "unnamed".to_string();
        }
        
        // Handle reserved Rust keywords
        let result = identifier
            .chars()
            .enumerate()
            .flat_map(|(i, ch)| {
                if ch.is_uppercase() && i > 0 {
                    vec!['_', ch.to_ascii_lowercase()]
                } else {
                    vec![ch.to_ascii_lowercase()]
                }
            })
            .collect::<String>();
        
        // Check for Rust keywords and prefix with 'r#' if needed
        match result.as_str() {
            "type" | "match" | "loop" | "fn" | "let" | "mut" | "const" | "static" |
            "struct" | "enum" | "impl" | "trait" | "use" | "mod" | "pub" | "super" |
            "self" | "Self" | "crate" | "extern" | "unsafe" | "async" | "await" |
            "move" | "ref" | "dyn" | "abstract" | "become" | "box" | "do" | "final" |
            "macro" | "override" | "priv" | "typeof" | "unsized" | "virtual" | "yield" => {
                format!("r#{}", result)
            }
            _ => result,
        }
    }
}
impl Handler for ArrayHandler {
    fn id(&self) -> Id {
        Id::get("array_handler")
    }
    
    fn role(&self) -> String {
        "array".to_string()
    }
    
    fn priority(&self) -> u64 {
        180
    }
    
    fn can_process(&self, context: &mut Context, token_slot: usize, token_range: Range<usize>) -> Result<bool> {
        let tokens = context
            .tokenizer
            .get_tokens(token_slot, token_range.clone());
        
        if tokens.is_empty() {
            return Ok(false);
        }
        
        let (can_process, confidence) = self.detect_array_pattern(
            &mut context.patternizer, 
            &context.samplizer, 
            &tokens, 
            token_range.clone()
        )?;
        
        report!(
            context,
            "array_handler",
            "can_process", 
            ReportLevel::Info,
            HandlerPhase::Process,
            &format!("Array detection: {} (confidence: {:.2})", can_process, confidence),
            can_process
        );
        
        Ok(can_process)
    }
    
    fn process(&self, context: &mut Context, token_slot: usize, token_range: Range<usize>) -> Result<HandlerResult> {
        let tokens = context
            .tokenizer
            .get_tokens(token_slot, token_range.clone());
        
        if tokens.is_empty() {
            return Ok(HandlerResult::NotHandled(None, token_range, self.id()));
        }
        
        context.registry.add_report(
            HandlerReport::new(
                &format!("array_handler_process_{}", token_slot),
                std::sync::Arc::new(self.id()),
                "Array Handler (Trait)".to_string(),
                "process".to_string(),
                format!("Processing array with {} tokens", tokens.len()),
                ReportLevel::Info,
                HandlerPhase::Process,
            )
            .with_tokens(tokens.len(), tokens.len())
            .with_success(true),
        );
        
        let array_info = self.extract_array_info(&tokens)?;
        let rust_code = self.convert_to_rust(&array_info, &tokens)?;
        
        Ok(HandlerResult::Processed(
            Some(tokens),
            token_range,
            rust_code,
            self.id(),
        ))
    }
    
    fn extract(&self, context: &mut Context, token_slot: usize, token_range: Range<usize>) -> Result<Option<ExtractedElement>> {
        let tokens = context
            .tokenizer
            .get_tokens(token_slot, token_range.clone());
        
        if tokens.is_empty() {
            return Ok(None);
        }
        
        let array_info = self.extract_array_info(&tokens)?;
        let element_type = self.extract_element_type(&tokens)?;
        let code = self.convert_to_rust(&array_info, &tokens)?;
        
        let extracted_array = ExtractedArray {
            id: Id::get(Id::gen_name(&self.id().name()).as_str()),
            code,
            info: array_info.clone(),
            tokens: tokens.clone(),
            token_range: token_range,
            metadata: vec![
                ("original_element_type".to_string(), element_type),
                ("has_size_specifier".to_string(), array_info.size.is_some().to_string()),
            ],
        };
        
        Ok(Some(ExtractedElement::Array(extracted_array)))
    }
    
    fn convert(&self, context: &mut Context, token_slot: usize, token_range: Range<usize>) -> Result<Option<ConvertedElement>> {
        let tokens = context
            .tokenizer
            .get_tokens(token_slot, token_range.clone());
        
        if tokens.is_empty() {
            return Ok(None);
        }
        
        let array_info = self.extract_array_info(&tokens)?;
        let element_type = self.extract_element_type(&tokens)?;
        let rust_element_type = self.convert_c_type_to_rust(&element_type);
        let rust_code = self.convert_to_rust(&array_info, &tokens)?;
        
        let converted_array = ConvertedArray {
            element_type: rust_element_type.clone(),
            size: array_info.size.as_ref().unwrap_or(&"0".to_string()).clone(),
            code: rust_code,
            is_declaration: !array_info.has_initializer,
            metadata: vec![
                ("original_c_type".to_string(), element_type),
                ("rust_type".to_string(), rust_element_type),
                ("storage_class".to_string(), array_info.storage_class.unwrap_or_else(|| "auto".to_string())),
                ("has_initializer".to_string(), array_info.has_initializer.to_string()),
            ],
        };
        
        Ok(Some(ConvertedElement::Array(converted_array)))
    }
    
    fn document(&self, context: &mut Context, info: ElementInfo) -> Result<Option<String>> {
        if let ElementInfo::Array(array_info) = info {
            let size_info = if let Some(size) = &array_info.size {
                format!(" with size {}", size)
            } else {
                " with dynamic size".to_string()
            };
            
            let storage_info = if let Some(storage) = &array_info.storage_class {
                format!(" ({} storage)", storage)
            } else {
                String::new()
            };
            
            let doc = format!(
                "/// Array: {}{}{}\n/// Converted from C array declaration\n/// Original C type information preserved in metadata\n",
                array_info.name, size_info, storage_info
            );
            Ok(Some(doc))
        } else {
            Ok(None)
        }
    }
    
    fn report(&self, context: &mut Context) -> Result<HandlerReport> {
        let reports = context.registry.get_reports_by_handler("array_handler");
        
        if reports.is_empty() {
            return Ok(HandlerReport::new(
                "array_handler_report",
                std::sync::Arc::new(self.id()),
                "Array Handler (Trait)".to_string(),
                "report".to_string(),
                "No array processing reports found".to_string(),
                ReportLevel::Info,
                HandlerPhase::Report,
            ).with_success(true));
        }
        
        let (info_count, warning_count, error_count) = reports
            .iter()
            .fold((0, 0, 0), |(info, warn, err), report| {
                match report.level {
                    ReportLevel::Info => (info + 1, warn, err),
                    ReportLevel::Warning => (info, warn + 1, err),
                    ReportLevel::Error => (info, warn, err + 1),
                    ReportLevel::Debug => (info + 1, warn, err),
                }
            });
        
        let total_tokens_processed = reports
            .iter()
            .map(|r| r.tokens_processed)
            .sum::<usize>();
        
        Ok(HandlerReport::new(
            "array_handler_report",
            std::sync::Arc::new(self.id()),
            "Array Handler (Trait)".to_string(),
            "report".to_string(),
            format!(
                "Array handler processed {} arrays: {} info, {} warnings, {} errors",
                reports.len(), info_count, warning_count, error_count
            ),
            if error_count > 0 { ReportLevel::Error } else if warning_count > 0 { ReportLevel::Warning } else { ReportLevel::Info },
            HandlerPhase::Report,
        )
        .with_success(error_count == 0)
        .with_tokens(total_tokens_processed, total_tokens_processed)
        .with_metadata("total_reports".to_string(), reports.len().to_string())
        .with_metadata("arrays_processed".to_string(), reports.len().to_string()))
    }
    
    fn supported_patterns(&self) -> Vec<String> {
        vec![
            "array_declaration".to_string(),
            "array_definition".to_string(),
            "array_initialization".to_string(),
            "static_array".to_string(),
            "const_array".to_string(),
            "multidimensional_array".to_string(),
            "variable_length_array".to_string(),
        ]
    }
    
    fn handle_redirect(&self, context: &mut Context, token_slot: usize, token_range: Range<usize>, result: HandlerResult) -> Result<HandlerResult> {
        match result {
            HandlerResult::NotHandled(_, _, _) => {
                let redirect_request = RedirectRequest {
                    from_handler: self.id(),
                    token_range: token_range.clone(),
                    failed_patterns: self.supported_patterns(),
                    suggested_handler: Some(Id::get("variable_handler")), // Variables are closely related to arrays
                    metadata: vec![
                        ("reason".to_string(), "array_pattern_not_matched".to_string()),
                        ("fallback_strategy".to_string(), "try_variable_handler".to_string()),
                    ],
                };
                
                // Log the redirect attempt
                report!(
                    context,
                    "array_handler",
                    "handle_redirect",
                    ReportLevel::Info,
                    HandlerPhase::Redirect,
                    "Redirecting unhandled array-like tokens to variable handler",
                    false
                );
                
                Ok(result)
            }
            _ => Ok(result), // Pass through successful results unchanged
        }
    }
}

impl Default for ArrayHandler {
    fn default() -> Self {
        Self::new()
    }
}

/// Factory function to create an array handler
pub fn create_array_handler() -> ArrayHandler {
    ArrayHandler::new()
}
