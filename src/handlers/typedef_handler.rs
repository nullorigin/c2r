//! Typedef handler implementation using trait-based system
//! Uses Patternizer and Samplizer for intelligent typedef pattern detection and processing

use crate::{Handlizer, RedirectRequest};
use crate::{
    Context, ConvertedElement, ConvertedTypedef, ElementInfo, ExtractedElement, 
    ExtractedTypedef, HandlerReport, HandlerResult, Id, Result,
    handler::Handler, ReportLevel, HandlerPhase,
};
use crate::info::TypedefInfo;
use std::ops::Range;

/// Typedef handler implementing the Handler trait
#[derive(Debug, Clone)]
pub struct TypedefHandler;

impl TypedefHandler {
    pub fn new() -> Self {
        Self
    }
    
    /// Enhanced pattern detection using both Patternizer and Samplizer
    fn detect_typedef_pattern(&self, context: &mut Context, tokens: &[crate::Token], token_range: Range<usize>) -> Result<(bool, f64)> {
        // First use Patternizer for structural detection
        let pattern_match = context.patternizer.match_pattern("typedef", tokens);

        // Then use Samplizer for confidence scoring
        let patterns = vec!["typedef_declaration".to_string(), "typedef_struct".to_string(), "typedef_enum".to_string()];
        let confidence = context.samplizer.analyze_with_range(&patterns, tokens, token_range)?;

        // Combined heuristics for typedef detection
        let has_typedef_structure = self.has_typedef_structure(tokens);
        let enhanced_confidence = if has_typedef_structure {
            confidence * 1.3  // Boost confidence if structure looks typedef-like
        } else {
            confidence * 0.6  // Reduce if structure doesn't match
        };

        let pattern_success = !matches!(pattern_match, crate::PatternResult::NoMatch { .. });

        Ok((pattern_success || enhanced_confidence > 0.7, enhanced_confidence))
    }
    
    /// Analyze token structure for typedef-like patterns
    fn has_typedef_structure(&self, tokens: &[crate::Token]) -> bool {
        if tokens.len() < 3 {
            return false;
        }
        
        // Must start with typedef
        if tokens[0].to_string() != "typedef" {
            return false;
        }
        
        // Must end with semicolon typically
        let last_token = tokens.last().unwrap().to_string();
        if last_token != ";" {
            return false;
        }
        
        true
    }
    
    /// Extract typedef information from tokens
    fn extract_typedef_info(&self, tokens: &[crate::Token]) -> Result<TypedefInfo> {
        let mut typedef_info = TypedefInfo::default();
        let mut parsing_type = true;
        let mut type_tokens = Vec::new();
        
        for (i, token) in tokens.iter().enumerate() {
            let token_str = token.to_string();
            
            if i == 0 && token_str == "typedef" {
                continue; // Skip typedef keyword
            }
            
            if token_str == ";" {
                break; // End of typedef
            }
            
            // Check if this is the alias name (usually the last identifier before semicolon)
            if i == tokens.len() - 2 && tokens[tokens.len() - 1].to_string() == ";" {
                typedef_info.alias_name = token_str;
                parsing_type = false;
            } else if parsing_type {
                type_tokens.push(token_str);
            }
        }
        
        typedef_info.original_type = type_tokens.join(" ");
        typedef_info.typedef_type = self.determine_typedef_type(&typedef_info.original_type);
        
        Ok(typedef_info)
    }
    
    /// Determine the type of typedef based on the original type
    fn determine_typedef_type(&self, original_type: &str) -> String {
        if original_type.contains("struct") {
            "struct".to_string()
        } else if original_type.contains("enum") {
            "enum".to_string()
        } else if original_type.contains("union") {
            "union".to_string()
        } else if original_type.contains("(") && original_type.contains("*") {
            "function_pointer".to_string()
        } else {
            "primitive".to_string()
        }
    }
    
    /// Convert typedef to Rust code
    fn convert_to_rust(&self, typedef_info: &TypedefInfo) -> Result<String> {
        let rust_code = match typedef_info.typedef_type.as_str() {
            "struct" => self.convert_struct_typedef(typedef_info),
            "enum" => self.convert_enum_typedef(typedef_info),
            "union" => self.convert_union_typedef(typedef_info),
            "function_pointer" => self.convert_function_pointer_typedef(typedef_info),
            "primitive" => self.convert_primitive_typedef(typedef_info),
            _ => format!("// TODO: Unsupported typedef: {}", typedef_info.original_type),
        };
        
        Ok(rust_code)
    }
    
    /// Convert struct typedef to Rust
    fn convert_struct_typedef(&self, typedef_info: &TypedefInfo) -> String {
        let rust_name = self.convert_identifier_to_rust(&typedef_info.alias_name);
        
        if typedef_info.original_type.contains("{") {
            // Anonymous struct definition
            format!("pub type {} = /* TODO: Implement struct fields */;", rust_name)
        } else {
            // Named struct reference
            let struct_name = typedef_info.original_type.replace("struct ", "");
            let rust_struct_name = self.convert_identifier_to_rust(&struct_name.trim());
            format!("pub type {} = {};", rust_name, rust_struct_name)
        }
    }
    
    /// Convert enum typedef to Rust
    fn convert_enum_typedef(&self, typedef_info: &TypedefInfo) -> String {
        let rust_name = self.convert_identifier_to_rust(&typedef_info.alias_name);
        
        if typedef_info.original_type.contains("{") {
            // Anonymous enum definition
            format!("pub type {} = /* TODO: Implement enum variants */;", rust_name)
        } else {
            // Named enum reference
            let enum_name = typedef_info.original_type.replace("enum ", "");
            let rust_enum_name = self.convert_identifier_to_rust(&enum_name.trim());
            format!("pub type {} = {};", rust_name, rust_enum_name)
        }
    }
    
    /// Convert union typedef to Rust
    fn convert_union_typedef(&self, typedef_info: &TypedefInfo) -> String {
        let rust_name = self.convert_identifier_to_rust(&typedef_info.alias_name);
        format!("pub type {} = /* TODO: Implement union as enum or alternative */;", rust_name)
    }
    
    /// Convert function pointer typedef to Rust
    fn convert_function_pointer_typedef(&self, typedef_info: &TypedefInfo) -> String {
        let rust_name = self.convert_identifier_to_rust(&typedef_info.alias_name);
        // Simplified function pointer conversion - would need more sophisticated parsing
        format!("pub type {} = fn() -> (); // TODO: Parse function signature", rust_name)
    }
    
    /// Convert primitive typedef to Rust
    fn convert_primitive_typedef(&self, typedef_info: &TypedefInfo) -> String {
        let rust_name = self.convert_identifier_to_rust(&typedef_info.alias_name);
        let rust_type = self.convert_c_type_to_rust(&typedef_info.original_type);
        format!("pub type {} = {};", rust_name, rust_type)
    }
    
    /// Convert C type to Rust equivalent
    fn convert_c_type_to_rust(&self, c_type: &str) -> String {
        match c_type.trim() {
            "char" => "i8".to_string(),
            "unsigned char" => "u8".to_string(),
            "short" => "i16".to_string(),
            "unsigned short" => "u16".to_string(),
            "int" => "i32".to_string(),
            "unsigned int" => "u32".to_string(),
            "long" => "i64".to_string(),
            "unsigned long" => "u64".to_string(),
            "long long" => "i64".to_string(),
            "unsigned long long" => "u64".to_string(),
            "float" => "f32".to_string(),
            "double" => "f64".to_string(),
            "void" => "()".to_string(),
            "size_t" => "usize".to_string(),
            "ssize_t" => "isize".to_string(),
            _ => format!("/* TODO: {} */", c_type),
        }
    }
    
    /// Convert C identifier to Rust naming convention
    fn convert_identifier_to_rust(&self, identifier: &str) -> String {
        // Convert to PascalCase for type aliases
        let mut result = String::new();
        let mut capitalize_next = true;
        
        for ch in identifier.chars() {
            if ch == '_' {
                capitalize_next = true;
            } else if capitalize_next {
                result.push(ch.to_uppercase().next().unwrap_or(ch));
                capitalize_next = false;
            } else {
                result.push(ch.to_lowercase().next().unwrap_or(ch));
            }
        }
        
        result
    }
}

impl Handler for TypedefHandler {
    fn id(&self) -> Id {
        Id::get("typedef_handler")
    }
    
    fn role(&self) -> String {
        "typedef".to_string()
    }
    
    fn priority(&self) -> u64 {
        140
    }
    
    fn supported_patterns(&self) -> Vec<String> {
        vec!["typedef_declaration".to_string(), "typedef_struct".to_string(), "typedef_enum".to_string(), "typedef_union".to_string(), "typedef_function_pointer".to_string(), "typedef_primitive".to_string()]
    }
    
    
    fn can_process(&self, context: &mut Context, token_slot: usize, token_range: Range<usize>) -> Result<bool> {
        let tokens = context.tokenizer.get_tokens(token_slot, token_range.clone());
        
        if tokens.is_empty() {
            return Ok(false);
        }
        let tokens = tokens;
        let (can_process, confidence) = self.detect_typedef_pattern(context, &tokens, token_range.clone())?;
        
        context.registry.add_report(HandlerReport::new(
                &format!("typedef_handler_can_process_{}", token_slot),
                std::sync::Arc::new(self.id()),
                "Typedef Handler (Trait)".to_string(),
                "can_process".to_string(),
                format!("Typedef detection: {} (confidence: {:.2})", can_process, confidence),
                ReportLevel::Info,
                HandlerPhase::Process,
            ).with_tokens(tokens.len(), if can_process { tokens.len() } else { 0 })
             .with_success(can_process));
        
        Ok(can_process)
    }
    
    fn process(&self, context: &mut Context, token_slot: usize, token_range: Range<usize>) -> Result<HandlerResult> {
        let tokens = context.tokenizer.get_tokens(token_slot, token_range.clone());
        
        if tokens.is_empty() {
            return Ok(HandlerResult::NotHandled(None, token_range, self.id()));
        }
        let tokens = tokens;
        context.registry.add_report(HandlerReport::new(
                &format!("typedef_handler_process_{}", token_slot),
                std::sync::Arc::new(self.id()),
                "Typedef Handler (Trait)".to_string(),
                "process".to_string(),
                format!("Processing typedef with {} tokens", tokens.len()),
                ReportLevel::Info,
                HandlerPhase::Process,
            ).with_tokens(tokens.len(), tokens.len())
             .with_success(true));
        
        let typedef_info = self.extract_typedef_info(&tokens)?;
        let rust_code = self.convert_to_rust(&typedef_info)?;
        
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

    let typedef_info = self.extract_typedef_info(&tokens)?;
    let rust_code = self.convert_to_rust(&typedef_info)?;

    let extracted_typedef = ExtractedTypedef {
        id: Id::get(Id::gen_name(&self.id().name()).as_str()),
        code: rust_code,
        info: typedef_info.clone(),
        tokens: tokens.clone(),
        token_range,
        metadata: vec![
            ("typedef_type".to_string(), typedef_info.typedef_type.clone()),
            ("original_type".to_string(), typedef_info.original_type.clone()),
            ("alias_name".to_string(), typedef_info.alias_name.clone()),
        ],
    };

    Ok(Some(ExtractedElement::Typedef(extracted_typedef)))
}
    
    fn convert(&self, context: &mut Context, token_slot: usize, token_range: Range<usize>) -> Result<Option<ConvertedElement>> {
        let tokens = context.tokenizer.get_tokens(token_slot, token_range.clone());

        
        if tokens.is_empty() {
            return Ok(None);
        }
        let tokens = tokens;
        let typedef_info = self.extract_typedef_info(&tokens)?;
        let rust_code = self.convert_to_rust(&typedef_info)?;
        
        let converted_typedef = ConvertedTypedef {
            target_type: rust_code.clone(),
            code: rust_code.clone(),
            is_public: false,
            metadata: Vec::new(),
        };
        
        Ok(Some(ConvertedElement::Typedef(converted_typedef)))
    }
    
    fn document(&self, context: &mut Context, info: ElementInfo) -> Result<Option<String>> {
        if let ElementInfo::Typedef(typedef_info) = info {
            let doc = format!(
                "/// Type alias: {}\n/// Original C type: {}\n/// Typedef type: {}\n",
                typedef_info.name, typedef_info.source_type, typedef_info.kind_description
            );
            Ok(Some(doc))
        } else {
            Ok(None)
        }
    }
    
    fn report(&self, context: &mut Context) -> Result<HandlerReport> {
        let reports = context.registry.get_reports_by_handler("typedef_handler_trait");
        if reports.is_empty() {
            return Ok(HandlerReport::new(
                "typedef_handler_report",
                std::sync::Arc::new(self.id()),
                "Typedef Handler (Trait)".to_string(),
                "report".to_string(),
                "No reports found".to_string(),
                ReportLevel::Info,
                HandlerPhase::Process,
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
            "typedef_handler_report",
            std::sync::Arc::new(self.id()),
            "Typedef Handler (Trait)".to_string(),
            "report".to_string(),
            format!(
                "Typedef handler processed {} info, {} warnings, {} errors",
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
            failed_patterns: vec!["typedef".to_string()],
            suggested_handler: None,
            metadata: Vec::new(),
        };
        // Return the result as-is to avoid deadlock in redirect workflow
        Ok(result)
    }
}

impl Default for TypedefHandler {
    fn default() -> Self {
        Self::new()
    }
}

pub fn create_typedef_handler() -> TypedefHandler {
    TypedefHandler::new()
}
