//! Macro handler implementation using trait-based system
//! Handles C preprocessor directives like #define, #include, #ifdef, etc.

use crate::info::MacroInfo;
use crate::{
    gen_name, Context, ConvertedElement, ConvertedMacro, ExtractedElement, ExtractedMacro,
    Handler, HandlerResult, Id, Phase, Report, ReportLevel, Result,
};
use std::ops::Range;

/// Macro handler implementing the Handler trait
#[derive(Debug, Clone)]
pub struct MacroHandler;

impl MacroHandler {
    pub fn new() -> Self {
        Self
    }

    /// Enhanced pattern detection using both Patternizer and Samplizer
    fn detect_pattern(
        &self,
        patternizer: &mut crate::Patternizer,
        samplizer: &crate::Samplizer,
        tokens: &[crate::Token],
        token_range: Range<usize>,
    ) -> Result<(bool, f64)> {
        if tokens.is_empty() {
            return Ok((false, 0.0));
        }

        // Try multiple registered patterns for macro/preprocessor directives
        let mut pattern_success = false;
        let pattern_names = vec!["define_macro", "include_directive", "conditional_macro", "macro"];

        for pattern_name in &pattern_names {
            let pattern_match = patternizer.match_pattern(pattern_name, tokens);
            if !matches!(pattern_match, crate::PatternResult::NoMatch { .. }) {
                pattern_success = true;
                break;
            }
        }

        // Enhanced macro-specific patterns for Samplizer
        let patterns = vec![
            "define_macro".to_string(),
            "include_directive".to_string(),
            "conditional_macro".to_string(),
        ];
        let base_confidence = samplizer.analyze_with_range(&patterns, tokens, token_range)?;

        // Enhanced macro detection with preprocessor directive validation
        let first_token = tokens.first().map(|t| t.to_string()).unwrap_or_default();
        let second_token = tokens.get(1).map(|t| t.to_string()).unwrap_or_default();
        let has_hash_prefix = first_token == "#" || first_token.starts_with('#');

        let has_macro_keywords = tokens.iter().any(|t| {
            matches!(t.to_string().as_str(), 
                "define" | "include" | "ifdef" | "ifndef" | "endif" | "undef" | 
                "pragma" | "if" | "else" | "elif" | "warning" | "error")
        });

        // Check if this is a valid preprocessor directive
        // Handle both cases: "#include" as one token OR "#" + "include" as two tokens
        let directive = if first_token.starts_with('#') && first_token.len() > 1 {
            first_token.clone()
        } else if first_token == "#" && !second_token.is_empty() {
            format!("{}{}", first_token, second_token)
        } else {
            first_token.clone()
        };

        // Additional structural validation for better accuracy
        // MUST have # as FIRST token
        let has_valid_structure = if first_token != "#" && !first_token.starts_with('#') {
            false  // # must be the very first token
        } else {
            match directive.as_str() {
                "#define" => tokens.len() >= 2,
                "#include" => tokens.len() >= 2,
                "#ifdef" | "#ifndef" | "#if" => tokens.len() >= 2,
                "#pragma" => tokens.len() >= 2,
                "#else" | "#endif" | "#undef" => tokens.len() >= 1,
                _ if has_macro_keywords && first_token == "#" => true,
                _ => false,
            }
        };

        // Calculate enhanced confidence with multi-factor analysis
        let structural_bonus = match (has_hash_prefix, has_macro_keywords, has_valid_structure) {
            (true, true, true) => 0.3,
            (true, true, false) => 0.2,
            (true, false, _) => 0.1,
            _ => 0.0,
        };

        let enhanced_confidence = (base_confidence + structural_bonus).min(1.0);

        // Require BOTH pattern match AND valid structure for reliable detection
        // Or very high confidence with hash prefix
        let can_process = (pattern_success && has_valid_structure)
            || (has_hash_prefix && has_valid_structure && enhanced_confidence > 0.7);

        Ok((can_process, enhanced_confidence))
    }


    /// Extract macro information from tokens
    fn extract_info(&self, tokens: &[crate::Token]) -> Result<MacroInfo> {
        let mut macro_info = MacroInfo::default();

        if tokens.is_empty() {
            return Ok(macro_info);
        }

        let directive = tokens[0].to_string();
        macro_info.directive_type = self.determine_directive(&directive);
        macro_info.raw_directive = tokens
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<_>>()
            .join(" ");

        match macro_info.directive_type.as_str() {
            "define" => self.extract_define_info(tokens, &mut macro_info),
            "include" => self.extract_include_info(tokens, &mut macro_info),
            "conditional" => self.extract_conditional_info(tokens, &mut macro_info),
            "pragma" => self.extract_pragma_info(tokens, &mut macro_info),
            _ => {}
        }

        Ok(macro_info)
    }

    /// Determine the type of preprocessor directive
    fn determine_directive(&self, directive: &str) -> String {
        match directive {
            "#define" => "define".to_string(),
            "#undef" => "undef".to_string(),
            "#include" => "include".to_string(),
            "#ifdef" | "#ifndef" | "#if" | "#else" | "#elif" | "#endif" => {
                "conditional".to_string()
            }
            "#pragma" => "pragma".to_string(),
            "#line" => "line".to_string(),
            "#error" | "#warning" => "diagnostic".to_string(),
            _ => "unknown".to_string(),
        }
    }

    /// Extract #define directive information
    fn extract_define_info(&self, tokens: &[crate::Token], macro_info: &mut MacroInfo) {
        if tokens.len() >= 2 {
            macro_info.name = tokens[1].to_string();
        }

        if tokens.len() >= 3 {
            macro_info.value = tokens[2..]
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(" ");
        }

        // Check if it's a function-like macro
        if tokens.len() >= 3 && tokens[2].to_string() == "(" {
            macro_info.is_function_like = true;
            // Extract parameters
            let mut in_params = true;
            let mut current_param = String::new();

            for token in &tokens[3..] {
                let token_str = token.to_string();
                match token_str.as_str() {
                    ")" => {
                        if !current_param.trim().is_empty() {
                            macro_info.parameters.push(current_param.trim().to_string());
                        }
                        in_params = false;
                        break;
                    }
                    "," => {
                        if !current_param.trim().is_empty() {
                            macro_info.parameters.push(current_param.trim().to_string());
                            current_param.clear();
                        }
                    }
                    _ => {
                        if in_params {
                            if !current_param.is_empty() {
                                current_param.push(' ');
                            }
                            current_param.push_str(&token_str);
                        }
                    }
                }
            }
        }
    }

    /// Extract #include directive information
    fn extract_include_info(&self, tokens: &[crate::Token], macro_info: &mut MacroInfo) {
        if tokens.len() >= 2 {
            let include_path = tokens[1].to_string();
            macro_info.include_path = include_path.clone();

            // Determine if it's system include (<>) or local include ("")
            if include_path.starts_with('<') && include_path.ends_with('>') {
                macro_info.is_system_include = true;
            }
        }
    }

    /// Extract conditional directive information
    fn extract_conditional_info(&self, tokens: &[crate::Token], macro_info: &mut MacroInfo) {
        if tokens.len() >= 2 {
            macro_info.condition = tokens[1..]
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(" ");
        }
    }

    /// Extract pragma directive information
    fn extract_pragma_info(&self, tokens: &[crate::Token], macro_info: &mut MacroInfo) {
        if tokens.len() >= 2 {
            macro_info.pragma_directive = tokens[1..]
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(" ");
        }
    }

    /// Convert macro to Rust code
    fn convert_info(&self, macro_info: &MacroInfo) -> Result<String> {
        match macro_info.directive_type.as_str() {
            "define" => self.convert_define(macro_info),
            "include" => self.convert_include(macro_info),
            "conditional" => self.convert_conditional(macro_info),
            "pragma" => self.convert_pragma(macro_info),
            _ => Ok(format!(
                "// TODO: Unsupported directive: {}",
                macro_info.directive
            )),
        }
    }

    /// Convert #define to Rust
    fn convert_define(&self, macro_info: &MacroInfo) -> Result<String> {
        if macro_info.is_function_like {
            // Convert function-like macro to Rust macro
            let params = if macro_info.parameters.is_empty() {
                String::new()
            } else {
                macro_info
                    .parameters
                    .iter()
                    .map(|p| format!("${}: expr", p))
                    .collect::<Vec<_>>()
                    .join(", ")
            };

            let macro_name = macro_info.name.to_lowercase();
            let body = if macro_info.value.is_empty() {
                "()".to_string()
            } else {
                // Clean up macro body for Rust syntax
                let cleaned_body = macro_info.value
                    .replace("\\", "")
                    .trim()
                    .to_string();
                cleaned_body
            };

            Ok(format!(
                "macro_rules! {} {{\n    ({}) => {{\n        {}\n    }};\n}}",
                macro_name, params, body
            ))
        } else if macro_info.value.is_empty() {
            // Simple flag define - convert to feature gate or unit constant
            let rust_name = macro_info.name.to_uppercase();
            Ok(format!("pub const {}: () = ();", rust_name))
        } else {
            // Constant define - infer type from value
            let rust_name = macro_info.name.to_uppercase();
            let (rust_type, rust_value) = self.convert_inferred_value(&macro_info.value);

            Ok(format!(
                "pub const {}: {} = {};",
                rust_name, rust_type, rust_value
            ))
        }
    }

    /// Infer Rust type and convert C value
    fn convert_inferred_value(&self, c_value: &str) -> (String, String) {
        let trimmed = c_value.trim();

        // Handle NULL pointer
        if trimmed.eq_ignore_ascii_case("null") {
            return ("*const ()".to_string(), "std::ptr::null()".to_string());
        }

        // String literals
        if trimmed.starts_with('"') && trimmed.ends_with('"') {
            return ("&str".to_string(), trimmed.to_string());
        }

        // Character literals
        if trimmed.starts_with('\'') && trimmed.ends_with('\'') {
            return ("char".to_string(), trimmed.to_string());
        }

        // Boolean values
        if trimmed == "0" {
            return ("bool".to_string(), "false".to_string());
        }
        if trimmed == "1" {
            return ("bool".to_string(), "true".to_string());
        }

        // Hexadecimal numbers
        if trimmed.starts_with("0x") || trimmed.starts_with("0X") {
            let value = if trimmed.len() <= 10 { // 0x + 8 hex digits
                "u32"
            } else {
                "u64"
            };
            return (value.to_string(), trimmed.to_string());
        }

        // Octal numbers
        if trimmed.starts_with('0') && trimmed.len() > 1 && trimmed.chars().skip(1).all(|c| c.is_ascii_digit()) {
            return ("u32".to_string(), format!("0o{}", &trimmed[1..]));
        }

        // Floating point numbers
        if trimmed.contains('.') || trimmed.contains('e') || trimmed.contains('E') {
            let rust_value = if trimmed.ends_with('f') || trimmed.ends_with('F') {
                let num = trimmed.trim_end_matches(['f', 'F']);
                return ("f32".to_string(), format!("{}f32", num));
            } else if trimmed.ends_with('l') || trimmed.ends_with('L') {
                trimmed.trim_end_matches(['l', 'L'])
            } else {
                trimmed
            };
            return ("f64".to_string(), rust_value.to_string());
        }

        // Integer suffixes
        if let Some(suffix_pos) = trimmed.find(|c: char| c.is_alphabetic()) {
            let (number, suffix) = trimmed.split_at(suffix_pos);
            let rust_type = match suffix.to_lowercase().as_str() {
                "u" => "u32",
                "ul" => "u64",
                "ull" => "u64",
                "l" => "i64",
                "ll" => "i64",
                _ => "i32",
            };
            return (rust_type.to_string(), number.to_string());
        }

        // Plain integers
        if trimmed.chars().all(|c| c.is_ascii_digit() || c == '-' || c == '+') {
            let value: i64 = trimmed.parse().unwrap_or(0);
            let rust_type = if value >= 0 && value <= u32::MAX as i64 {
                "u32"
            } else if value < 0 && value >= i32::MIN as i64 {
                "i32"
            } else {
                "i64"
            };
            return (rust_type.to_string(), trimmed.to_string());
        }

        // Expressions with operators
        if trimmed.contains(['(', ')', '+', '-', '*', '/', '%', '&', '|', '^', '<', '>', '!']) {
            return ("/* expr */".to_string(), format!("({})", trimmed));
        }

        // Default case - treat as identifier or expression
        ("/* inferred */".to_string(), trimmed.to_string())
    }

    /// Convert #include to Rust
    fn convert_include(&self, macro_info: &MacroInfo) -> Result<String> {
        if macro_info.is_system_include {
            // System includes might map to external crates
            let header_name = macro_info
                .include_path
                .trim_start_matches('<')
                .trim_end_matches('>')
                .replace(".h", "");

            match header_name.as_str() {
                "stdio" => Ok("use std::io::{self, Write};".to_string()),
                "stdlib" => Ok("use std::{process, mem, ptr};".to_string()),
                "string" => Ok("use std::ffi::{CString, CStr};".to_string()),
                "math" => Ok("use std::f64::consts;".to_string()),
                "stdint" => Ok("// Standard integer types are built-in".to_string()),
                "stdbool" => Ok("// Boolean type is built-in".to_string()),
                "stddef" => Ok("use std::{mem, ptr};".to_string()),
                "errno" => Ok("use std::io::Error;".to_string()),
                "time" => Ok("use std::time::{SystemTime, Duration};".to_string()),
                _ => Ok(format!(
                    "// TODO: Map system header: {} to appropriate Rust crate",
                    macro_info.include_path
                )),
            }
        } else {
            // Local includes become module declarations
            let module_name = macro_info
                .include_path
                .trim_start_matches('"')
                .trim_end_matches('"')
                .replace(".h", "")
                .replace(['/', '\\'], "::")
                .replace('-', "_");

            Ok(format!("pub mod {};", module_name))
        }
    }

    /// Convert conditional directives to Rust
    fn convert_conditional(&self, macro_info: &MacroInfo) -> Result<String> {
        let directive = macro_info
            .raw_directive
            .split_whitespace()
            .next()
            .unwrap_or("");

        match directive {
            "#ifdef" => {
                let feature_name = macro_info.condition.to_lowercase().replace('_', "-");
                Ok(format!("#[cfg(feature = \"{}\")]", feature_name))
            }
            "#ifndef" => {
                let feature_name = macro_info.condition.to_lowercase().replace('_', "-");
                Ok(format!("#[cfg(not(feature = \"{}\"))]", feature_name))
            }
            "#if" => {
                // Try to convert common C conditions to Rust cfg
                let condition = if macro_info.condition.contains("defined(") {
                    let cleaned = macro_info.condition
                        .replace("defined(", "feature = \"")
                        .replace(')', "\"")
                        .replace("&&", ", ")
                        .replace("||", ", any(");
                    if cleaned.contains("any(") {
                        format!("any({})", cleaned)
                    } else {
                        cleaned
                    }
                } else {
                    format!("/* {} */", macro_info.condition)
                };
                Ok(format!("#[cfg({})]", condition))
            }
            "#else" => Ok("#[cfg(not(...))] // TODO: Invert previous condition".to_string()),
            "#elif" => Ok(format!("#[cfg({})] // TODO: Chain with previous conditions", macro_info.condition)),
            "#endif" => Ok("// End conditional compilation".to_string()),
            _ => Ok(format!("// Unsupported conditional: {}", macro_info.raw_directive)),
        }
    }

    /// Convert pragma directives to Rust
    fn convert_pragma(&self, macro_info: &MacroInfo) -> Result<String> {
        let pragma_parts: Vec<&str> = macro_info.pragma_directive.split_whitespace().collect();

        if pragma_parts.is_empty() {
            return Ok("// Empty pragma directive".to_string());
        }

        match pragma_parts[0] {
            "once" => Ok("#![allow(clippy::module_inception)] // Equivalent to #pragma once".to_string()),
            "pack" => {
                if pragma_parts.len() > 1 {
                    Ok(format!("#[repr(packed({}))]", pragma_parts[1]))
                } else {
                    Ok("#[repr(packed)]".to_string())
                }
            }
            "warning" => {
                if pragma_parts.len() > 1 {
                    Ok(format!("#[warn({})]", pragma_parts[1..].join("_")))
                } else {
                    Ok("#[warn(warnings)]".to_string())
                }
            }
            "GCC" | "clang" => {
                Ok(format!("// Compiler-specific pragma: {}", macro_info.pragma_directive))
            }
            _ => Ok(format!(
                "// TODO: Convert pragma '{}' to appropriate Rust attribute",
                macro_info.pragma_directive
            )),
        }
    }
}

impl Handler for MacroHandler {
    fn id(&self) -> Id {
        Id::get("macro_handler")
    }

    fn role(&self) -> String {
        "macro".to_string()
    }

    fn priority(&self) -> u64 {
        180
    }

    fn supported_patterns(&self) -> Vec<String> {
        vec![
            "preprocessor_directive".to_string(),
            "macro_definition".to_string(),
            "include_directive".to_string(),
            "conditional_directive".to_string(),
            "pragma_directive".to_string(),
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

        let (can_process, confidence) =
            self.detect_pattern(&mut context.patternizer, &context.samplizer, &tokens, token_range.clone())?;

        context.registry.add_report(
            Report::new(
                Id::get(&format!("macro_handler_can_process_{}", token_slot)),
                Some(self.id()),
                "can_process".to_string(),
                format!(
                    "Macro detection: {} (confidence: {:.2})",
                    can_process, confidence
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
        let tokens = context
            .tokenizer
            .get_tokens(token_slot, token_range.clone());

        if tokens.is_empty() {
            return Ok(None);
        }

        let macro_info = self.extract_info(&tokens)?;

        let extracted_macro = ExtractedMacro {
            id: Id::get(&gen_name(&self.id().name()).as_str()),
            info: macro_info.clone(),
            tokens: tokens.clone(),
            token_range,
            metadata: vec![
                (
                    "directive_type".to_string(),
                    macro_info.directive_type.clone(),
                ),
                (
                    "is_function_like".to_string(),
                    macro_info.is_function_like.to_string(),
                ),
                (
                    "is_system_include".to_string(),
                    macro_info.is_system_include.to_string(),
                ),
                ("original_name".to_string(), macro_info.name.clone()),
            ],
        };

        Ok(Some(ExtractedElement::Macro(extracted_macro)))
    }

    fn convert(
        &self,
        context: &mut Context,
        element: ExtractedElement,
    ) -> Result<Option<ConvertedElement>> {
        if let ExtractedElement::Macro(extracted) = element {
            let tokens = extracted.tokens.clone();
            if tokens.is_empty() {
                return Ok(None);
            }

            let macro_info = extracted.info.clone();
            let rust_code = self.convert_info(&macro_info)?;

            let converted_macro = ConvertedMacro {
                code: rust_code,
                metadata: Vec::new(),
            };

            Ok(Some(ConvertedElement::Macro(converted_macro)))
        } else {
            Ok(None)
        }
    }

    fn document(
        &self,
        context: &mut Context,
        extracted_element: ExtractedElement,
        converted_element: ConvertedElement,
    ) -> Result<Option<String>> {
        let extracted_macro = if let ExtractedElement::Macro(extracted) = extracted_element {
            extracted
        } else {
            return Ok(None);
        };

        let _converted_macro = if let ConvertedElement::Macro(converted) = converted_element {
            converted
        } else {
            return Ok(None);
        };

        let mut doc = format!(
            "/// Preprocessor directive: {}\n/// Type: {}\n/// Raw: {}\n",
            extracted_macro.info.name,
            extracted_macro.info.directive_type,
            extracted_macro.info.raw_directive
        );

        if extracted_macro.info.is_function_like {
            doc.push_str("/// Function-like macro\n");
            if !extracted_macro.info.parameters.is_empty() {
                doc.push_str("/// Parameters:\n");
                for param in &extracted_macro.info.parameters {
                    doc.push_str(&format!("/// - {}\n", param));
                }
            }
        }

        if !extracted_macro.info.value.is_empty() {
            doc.push_str(&format!("/// Value: {}\n", extracted_macro.info.value));
        }

        if extracted_macro.info.is_system_include {
            doc.push_str("/// System include\n");
        }

        if !extracted_macro.info.include_path.is_empty() {
            doc.push_str(&format!(
                "/// Include path: {}\n",
                extracted_macro.info.include_path
            ));
        }

        if !extracted_macro.info.condition.is_empty() {
            doc.push_str(&format!(
                "/// Condition: {}\n",
                extracted_macro.info.condition
            ));
        }

        Ok(Some(doc))
    }

    fn report(&self, context: &mut Context) -> Result<Report> {
        let reports = context.registry.get_reports_by_handler(&self.id().name());

        if reports.is_empty() {
            return Ok(Report::new(
                Id::get("macro_handler_report"),
                Some(self.id()),
                "report".to_string(),
                "No macro processing reports found".to_string(),
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

        Ok(Report::new(
            Id::get("macro_handler_report"),
            Some(self.id()),
            "report".to_string(),
            format!(
                "Macro handler processed {} macros: {} info, {} warnings, {} errors",
                reports.len(),
                info_count,
                warning_count,
                error_count
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

    fn supported_keywords(&self) -> Vec<String> {
        vec![
            // Preprocessor directives - HIGHEST PRIORITY for # detection
            "#define".to_string(),
            "#include".to_string(),
            "#ifdef".to_string(),
            "#ifndef".to_string(),
            "#if".to_string(),
            "#endif".to_string(),
            "#else".to_string(),
            "#elif".to_string(),
            "#pragma".to_string(),
            "#undef".to_string(),
            "#error".to_string(),
            "#warning".to_string(),
            "#line".to_string(),
            "#".to_string(), // Catch-all for any # directive
        ]
    }

    fn route(
        &self,
        context: &mut Context,
        token_slot: usize,
        token_range: Range<usize>,
        result: HandlerResult,
    ) -> Result<HandlerResult> {
        // Log redirect attempt for debugging
        context.registry.add_report(
            Report::new(
                Id::get(&format!("macro_handler_redirect_{}", token_slot)),
                Some(self.id()),
                "redirect".to_string(),
                format!("Redirect requested for token range {:?}", token_range),
                ReportLevel::Debug,
                Phase::Process(None),
            )
                .with_tokens(token_range.len(), 0)
                .with_success(false),
        );

        Ok(result)
    }
}

impl Default for MacroHandler {
    fn default() -> Self {
        Self::new()
    }
}

pub fn create_macro_handler() -> MacroHandler {
    MacroHandler::new()
}
