//! Macro handler implementation using trait-based system
//! Handles C preprocessor directives like #define, #include, #ifdef, etc.

use crate::{
    Context, ConvertedElement, ConvertedMacro, ElementInfo, ExtractedElement, ExtractedMacro, Handler, HandlerPhase, HandlerReport, HandlerResult, Handlizer, Id, RedirectRequest, ReportLevel, Result   
};
use crate::info::MacroInfo;
use std::ops::Range;

/// Macro handler implementing the Handler trait
#[derive(Debug, Clone)]
pub struct MacroHandler;

impl MacroHandler {
    pub fn new() -> Self {
        Self
    }
    
    /// Enhanced pattern detection using both Patternizer and Samplizer
    fn detect_macro_pattern(&self,context: &mut Context, tokens: &[crate::Token], token_range: Range<usize>) -> Result<(bool, f64)> {
        // First use Patternizer for structural detection
        let pattern_match = context.patternizer.match_pattern("macro", tokens);

        // Then use Samplizer for confidence scoring
        let patterns = vec!["define".to_string(), "include".to_string(), "ifdef".to_string(), "ifndef".to_string()];
        let confidence = context.samplizer.analyze_with_range(&patterns, tokens, token_range)?;

        // Combined heuristics for macro detection
        let has_macro_structure = self.has_macro_structure(tokens);
        let enhanced_confidence = if has_macro_structure {
            confidence * 1.5  // Boost confidence if structure looks macro-like
        } else {
            confidence * 0.4  // Reduce if structure doesn't match
        };

        let pattern_success = !matches!(pattern_match, crate::PatternResult::NoMatch { .. });

        Ok((pattern_success || enhanced_confidence > 0.8, enhanced_confidence))
    }
    
    /// Analyze token structure for macro/preprocessor patterns
    fn has_macro_structure(&self, tokens: &[crate::Token]) -> bool {
        if tokens.is_empty() {
            return false;
        }
        
        let first_token = tokens[0].to_string();
        
        // Check for preprocessor directives starting with #
        if first_token.starts_with('#') {
            return true;
        }
        
        // Check for common preprocessor keywords
        let macro_keywords = [
            "#define", "#include", "#ifdef", "#ifndef", "#if", "#else", 
            "#elif", "#endif", "#pragma", "#undef", "#line", "#error", "#warning"
        ];
        
        macro_keywords.iter().any(|&keyword| first_token == keyword)
    }
    
    /// Extract macro information from tokens
    fn extract_macro_info(&self, tokens: &[crate::Token]) -> Result<MacroInfo> {
        let mut macro_info = MacroInfo::default();
        
        if tokens.is_empty() {
            return Ok(macro_info);
        }
        
        let directive = tokens[0].to_string();
        macro_info.directive_type = self.determine_directive_type(&directive);
        macro_info.raw_directive = tokens.iter()
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
    fn determine_directive_type(&self, directive: &str) -> String {
        match directive {
            "#define" => "define".to_string(),
            "#undef" => "undef".to_string(),
            "#include" => "include".to_string(),
            "#ifdef" | "#ifndef" | "#if" | "#else" | "#elif" | "#endif" => "conditional".to_string(),
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
            macro_info.value = tokens[2..].iter()
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
            macro_info.condition = tokens[1..].iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(" ");
        }
    }
    
    /// Extract pragma directive information
    fn extract_pragma_info(&self, tokens: &[crate::Token], macro_info: &mut MacroInfo) {
        if tokens.len() >= 2 {
            macro_info.pragma_directive = tokens[1..].iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(" ");
        }
    }
    
    /// Convert macro to Rust code
    fn convert_to_rust(&self, macro_info: &MacroInfo) -> Result<String> {
        match macro_info.directive_type.as_str() {
            "define" => self.convert_define_to_rust(macro_info),
            "include" => self.convert_include_to_rust(macro_info),
            "conditional" => self.convert_conditional_to_rust(macro_info),
            "pragma" => self.convert_pragma_to_rust(macro_info),
            _ => Ok(format!("// TODO: Unsupported directive: {}", macro_info.directive)),
        }
    }
    
    /// Convert #define to Rust
    fn convert_define_to_rust(&self, macro_info: &MacroInfo) -> Result<String> {
        if macro_info.is_function_like {
            // Convert function-like macro to Rust macro
            let params = if macro_info.parameters.is_empty() {
                String::new()
            } else {
                macro_info.parameters.iter()
                    .map(|p| format!("${}: expr", p))
                    .collect::<Vec<_>>()
                    .join(", ")
            };
            
            let macro_name = macro_info.name.to_lowercase();
            let body = if macro_info.value.is_empty() {
                "()".to_string()
            } else {
                macro_info.value.clone()
            };
            
            Ok(format!(
                "macro_rules! {} {{\n    ({}) => {{\n        {}\n    }};\n}}",
                macro_name,
                params,
                body
            ))
        } else if macro_info.value.is_empty() {
            // Simple flag define - convert to feature gate or unit constant
            let rust_name = macro_info.name.to_uppercase();
            Ok(format!("pub const {}: () = ();", rust_name))
        } else {
            // Constant define - infer type from value
            let rust_name = macro_info.name.to_uppercase();
            let (rust_type, rust_value) = self.infer_type_and_convert_value(&macro_info.value);
            
            Ok(format!(
                "pub const {}: {} = {};",
                rust_name,
                rust_type,
                rust_value
            ))
        }
    }
    
    /// Infer Rust type and convert C value
    fn infer_type_and_convert_value(&self, c_value: &str) -> (String, String) {
        let trimmed = c_value.trim();
        
        // String literals
        if trimmed.starts_with('"') && trimmed.ends_with('"') {
            return ("&str".to_string(), trimmed.to_string());
        }
        
        // Character literals
        if trimmed.starts_with('\'') && trimmed.ends_with('\'') {
            return ("char".to_string(), trimmed.to_string());
        }
        
        // Hexadecimal numbers
        if trimmed.starts_with("0x") || trimmed.starts_with("0X") {
            return ("u32".to_string(), trimmed.to_string());
        }
        
        // Floating point numbers
        if trimmed.contains('.') || trimmed.contains('e') || trimmed.contains('E') {
            let rust_value = if trimmed.ends_with('f') || trimmed.ends_with('F') {
                trimmed.trim_end_matches(['f', 'F'])
            } else {
                trimmed
            };
            return ("f64".to_string(), rust_value.to_string());
        }
        
        // Integer suffixes
        if let Some(suffix_pos) = trimmed.find(|c: char| c.is_alphabetic()) {
            let (number, suffix) = trimmed.split_at(suffix_pos);
            let rust_type = match suffix.to_lowercase().as_str() {
                "u" | "ul" | "ull" => "u64",
                "l" | "ll" => "i64",
                _ => "i32",
            };
            return (rust_type.to_string(), number.to_string());
        }
        
        // Plain integers
        if trimmed.chars().all(|c| c.is_ascii_digit() || c == '-' || c == '+') {
            return ("i32".to_string(), trimmed.to_string());
        }
        
        // Default case - treat as expression
        ("/* inferred */".to_string(), trimmed.to_string())
    }
    
    /// Convert #include to Rust
    fn convert_include_to_rust(&self, macro_info: &MacroInfo) -> Result<String> {
        if macro_info.is_system_include {
            // System includes might map to external crates
            let header_name = macro_info.include_path
                .trim_start_matches('<')
                .trim_end_matches('>')
                .replace(".h", "");
            
            match header_name.as_str() {
                "stdio" => Ok("// use std::io;".to_string()),
                "stdlib" => Ok("// use std::process;".to_string()),
                "string" => Ok("// use std::ffi::CString;".to_string()),
                "math" => Ok("// use std::f64::consts;".to_string()),
                _ => Ok(format!("// TODO: Map system header: {}", macro_info.include_path)),
            }
        } else {
            // Local includes become module declarations
            let module_name = macro_info.include_path
                .trim_start_matches('"')
                .trim_end_matches('"')
                .replace(".h", "")
                .replace('/', "::");
            
            Ok(format!("mod {};", module_name))
        }
    }
    
    /// Convert conditional directives to Rust
    fn convert_conditional_to_rust(&self, macro_info: &MacroInfo) -> Result<String> {
        match macro_info.raw_directive.split_whitespace().next().unwrap_or("") {
            "#ifdef" => Ok(format!("// #[cfg(feature = \"{}\")]", macro_info.condition)),
            "#ifndef" => Ok(format!("// #[cfg(not(feature = \"{}\"))]", macro_info.condition)),
            "#if" => Ok(format!("// #[cfg({})]", macro_info.condition)),
            "#else" => Ok("// #[cfg(not(...))]".to_string()),
            "#endif" => Ok("// End conditional".to_string()),
            _ => Ok(format!("// {}", macro_info.raw_directive)),
        }
    }
    
    /// Convert pragma directives to Rust
    fn convert_pragma_to_rust(&self, macro_info: &MacroInfo) -> Result<String> {
        // Most pragmas don't have direct Rust equivalents
        Ok(format!("// #[{}] // TODO: Convert pragma", macro_info.pragma_directive))
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
        vec!["preprocessor_directive".to_string(), "macro_definition".to_string(), "include_directive".to_string(), "conditional_directive".to_string(), "pragma_directive".to_string()]
    }
    
    
    fn can_process(&self, context: &mut Context, token_slot: usize, token_range: Range<usize>) -> Result<bool> {
        let tokens = context.tokenizer.get_tokens(token_slot, token_range.clone());
        
        if tokens.is_empty() {
            return Ok(false);
        }
        let tokens = tokens;
        
        let (can_process, confidence) = self.detect_macro_pattern(context, &tokens, token_range.clone())?;
        
        context.registry.add_report(HandlerReport::new(
                &format!("macro_handler_can_process_{}", token_slot),
                std::sync::Arc::new(self.id()),
                "Macro Handler (Trait)".to_string(),
                "can_process".to_string(),
                format!("Macro detection: {} (confidence: {:.2})", can_process, confidence),
                ReportLevel::Info,
                HandlerPhase::Process,
            ).with_tokens(tokens.len(), if can_process { tokens.len() } else { 0 })
             .with_success(can_process));
        
        Ok(can_process)
    }
    
    fn process(&self, context: &mut Context, token_slot: usize, token_range: Range<usize>) -> Result<HandlerResult> {
        let tokens = context.tokenizer.get_tokens(token_slot, token_range.clone());
        
        if tokens.is_empty() {
            return Ok(HandlerResult::NotHandled(None, token_range.clone(), self.id()));
        }
        let tokens = tokens;
        
        context.    registry.add_report(HandlerReport::new(
                &format!("macro_handler_process_{}", token_slot),
                std::sync::Arc::new(self.id()),
                "Macro Handler (Trait)".to_string(),
                "process".to_string(),
                format!("Processing macro with {} tokens", tokens.len()),
                ReportLevel::Info,
                HandlerPhase::Process,
            ).with_tokens(tokens.len(), tokens.len())
             .with_success(true));
        
        let macro_info = self.extract_macro_info(&tokens)?;
        let rust_code = self.convert_to_rust(&macro_info)?;
        
        Ok(HandlerResult::Processed(
            Some(tokens),
            token_range,
            rust_code,
            self.id(),
        ))
    }
    
    fn extract(&self, context: &mut Context, token_slot: usize, token_range: Range<usize>) -> Result<Option<ExtractedElement>> {
    let tokens = context.tokenizer.get_tokens(token_slot, token_range.clone());
    
    if tokens.is_empty() {
        return Ok(None);
    }
    
    let macro_info = self.extract_macro_info(&tokens)?;
    let rust_code = self.convert_to_rust(&macro_info)?;
    
    let extracted_macro = ExtractedMacro {
        id: Id::get(Id::gen_name(&self.id().name()).as_str()),
        code: rust_code,
        info: macro_info.clone(),
        tokens: tokens.clone(),
        token_range,
        metadata: vec![
            ("directive_type".to_string(), macro_info.directive_type.clone()),
            ("is_function_like".to_string(), macro_info.is_function_like.to_string()),
            ("is_system_include".to_string(), macro_info.is_system_include.to_string()),
            ("original_name".to_string(), macro_info.name.clone()),
        ],
    };
    
    Ok(Some(ExtractedElement::Macro(extracted_macro)))
}
    
    fn convert(&self, context: &mut Context, token_slot: usize, token_range: Range<usize>) -> Result<Option<ConvertedElement>> {
        let tokens = context.tokenizer.get_tokens(token_slot, token_range.clone());
        
        if tokens.is_empty() {
            return Ok(None);
        }
        let tokens = tokens;
        let macro_info = self.extract_macro_info(&tokens)?;
        let rust_code = self.convert_to_rust(&macro_info)?;
        
        let converted_macro = ConvertedMacro {
            parameters: Vec::new(), // TODO: Extract parameters from macro_info
            body: rust_code.clone(),
            code: rust_code.clone(),
            is_function_like: false,
            metadata: Vec::new(),
        };
        
        Ok(Some(ConvertedElement::Macro(converted_macro)))
    }
    
    fn document(&self, context: &mut Context, info: ElementInfo) -> Result<Option<String>> {
        if let ElementInfo::Macro(macro_info) = info {
            let doc = format!(
                "/// Preprocessor directive: {}\n/// Type: {}\n/// Raw: {}\n",
                macro_info.name, macro_info.kind_description, macro_info.directive
            );
            Ok(Some(doc))
        } else {
            Ok(None)
        }
    }
    
    fn report(&self, context: &mut Context) -> Result<HandlerReport> {
        let reports = context.registry.get_reports_by_handler("macro_handler_trait");
        if reports.is_empty() {
            return Ok(HandlerReport::new(
                "macro_handler_report",
                std::sync::Arc::new(self.id()),
                "Macro Handler (Trait)".to_string(),
                "report".to_string(),
                "No reports found".to_string(),
                ReportLevel::Info,
                HandlerPhase::Report,
            ));
        }
        let reports = reports; 
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
        
        Ok(HandlerReport::new(
            "macro_handler_report",
            std::sync::Arc::new(self.id()),
            "Macro Handler (Trait)".to_string(),
            "report".to_string(),
            format!(
                "Macro handler processed {} info, {} warnings, {} errors",
                info_count, warning_count, error_count
            ),
            ReportLevel::Info,
            HandlerPhase::Report,
        ).with_success(error_count == 0)
         .with_metadata("total_reports".to_string(), reports.len().to_string()))
    }
    
    
    fn handle_redirect(&self, context: &mut Context, token_slot: usize, token_range: Range<usize>, result: HandlerResult) -> Result<HandlerResult> {
        let redirect_request = RedirectRequest {
            from_handler: self.id(),
            token_range: token_range.clone(),
            failed_patterns: vec!["macro".to_string()],
            suggested_handler: None,
            metadata: Vec::new(),
        };
        // Return the result as-is to avoid deadlock in redirect workflow
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
