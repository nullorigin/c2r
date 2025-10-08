//! Struct handler implementation using trait-based system
//! Uses Patternizer and Samplizer for intelligent struct pattern detection and processing

use crate::{
    Context, ConvertedElement, ConvertedStruct, ElementInfo, ExtractedElement, ExtractedStruct, HandlerPhase, HandlerReport, HandlerResult, Handlizer, Id, RedirectRequest, ReportLevel, Result, context, handler::Handler, handlers::common::filter_tokens_for_handler, info::StructInfo
};
use std::ops::Range;

/// Struct handler implementing the Handler trait
#[derive(Debug, Clone)]
pub struct StructHandler;

impl StructHandler {
    pub fn new() -> Self {
        Self
    }
    
    /// Enhanced pattern detection using both Patternizer and Samplizer
    fn detect_struct_pattern(&self, context: &mut Context, tokens: &[crate::Token], token_range: Range<usize>) -> Result<(bool, f64)> {
        // First use Patternizer for structural detection
        let pattern_match = context.patternizer.match_pattern("struct", tokens);

        // Then use Samplizer for confidence scoring
        let patterns = vec!["struct_declaration".to_string(), "struct_definition".to_string()];
        let confidence = context.samplizer.analyze_with_range(&patterns, tokens, token_range)?;

        // Combined heuristics for struct detection
        let has_struct_structure = self.has_struct_structure(tokens);
        let enhanced_confidence = if has_struct_structure {
            confidence * 1.4  // Boost confidence if structure looks struct-like
        } else {
            confidence * 0.6  // Reduce if structure doesn't match
        };

        let pattern_success = !matches!(pattern_match, crate::PatternResult::NoMatch { .. });

        Ok((pattern_success || enhanced_confidence > 0.65, enhanced_confidence))
    }
    
    /// Analyze token structure for struct-like patterns
    fn has_struct_structure(&self, tokens: &[crate::Token]) -> bool {
        if tokens.len() < 2 {
            return false;
        }
        
        let mut has_struct_keyword = false;
        let mut has_braces = false;
        let mut brace_count = 0;
        
        for token in tokens {
            let token_str = token.to_string();
            
            if token_str == "struct" {
                has_struct_keyword = true;
            }
            
            if token_str == "{" {
                has_braces = true;
                brace_count += 1;
            } else if token_str == "}" {
                brace_count -= 1;
            }
        }
        
        has_struct_keyword && (has_braces || brace_count == 0)
    }
    
    /// Extract struct information from tokens
    fn extract_struct_info(&self, tokens: &[crate::Token]) -> Result<StructInfo> {
    let mut struct_info = StructInfo::default();
    let mut in_body = false;
    let mut field_tokens = Vec::new();
    let mut brace_count = 0;
    
    for (i, token) in tokens.iter().enumerate() {
        let token_str = token.to_string();
        
        match token_str.as_str() {
            "struct" => {
                // Next token might be the struct name
                if i + 1 < tokens.len() {
                    let next_token = tokens[i + 1].to_string();
                    if next_token != "{" && !next_token.is_empty() {
                        struct_info.name = next_token;
                    }
                }
            }
            "typedef" => {
                struct_info.is_typedef = true;
            }
            "{" => {
                brace_count += 1;
                if brace_count == 1 {
                    in_body = true;
                    struct_info.has_body = true;
                }
            }
            "}" => {
                brace_count -= 1;
                if brace_count == 0 {
                    in_body = false;
                    // Add any remaining field
                    if !field_tokens.is_empty() {
                        let field_decl = field_tokens.join(" ");
                        struct_info.fields.push((field_decl, "".to_string()));
                        field_tokens.clear();
                    }
                }
            }
            ";" => {
                if in_body && !field_tokens.is_empty() {
                    // End of field declaration
                    let field_decl = field_tokens.join(" ");
                    struct_info.fields.push((field_decl, "".to_string()));
                    field_tokens.clear();
                }
            }
            _ => {
                if in_body {
                    field_tokens.push(token_str);
                }
            }
        }
    }
    
    Ok(struct_info)
}
    
    /// Convert struct to Rust code
    fn convert_to_rust(&self, struct_info: &StructInfo, tokens: &[crate::Token]) -> Result<String> {
        let mut rust_code = String::new();
        
        // Add derives and attributes
        rust_code.push_str("#[derive(Debug, Clone)]\n");
        
        // Add struct visibility
        rust_code.push_str("pub struct ");
        
        // Add struct name
        let rust_name = self.convert_identifier_to_rust(&struct_info.name);
        rust_code.push_str(&rust_name);
        
        if struct_info.has_body {
            rust_code.push_str(" {\n");
            
            // Convert fields
            for field in &struct_info.fields {
                rust_code.push_str("    pub ");
                let rust_field = self.convert_field_to_rust(field.0.as_str());
                rust_code.push_str(&rust_field);
                rust_code.push_str(",\n");
            }
            
            rust_code.push('}');
        } else {
            rust_code.push(';');
        }
        
        Ok(rust_code)
    }
    
    /// Convert C field declaration to Rust
    fn convert_field_to_rust(&self, field: &str) -> String {
        // Simple field conversion - more sophisticated parsing needed
        let parts: Vec<&str> = field.trim().split_whitespace().collect();
        if parts.len() >= 2 {
            let field_name = self.convert_identifier_to_rust(parts.last().unwrap());
            format!("{}: /* {} */", field_name, parts[..parts.len()-1].join(" "))
        } else {
            format!("field: /* {} */", field)
        }
    }
    
    /// Convert C identifier to Rust naming convention
    fn convert_identifier_to_rust(&self, identifier: &str) -> String {
        let mut result = String::new();
        let mut chars = identifier.chars().peekable();
        
        while let Some(ch) = chars.next() {
            if ch.is_uppercase() && !result.is_empty() {
                result.push('_');
            }
            result.push(ch.to_lowercase().next().unwrap_or(ch));
        }
        
        result
    }
}

impl Handler for StructHandler {
    fn id(&self) -> Id {
        Id::get("struct_handler")
    }
    
    fn role(&self) -> String {
        "struct".to_string()
    }
    
    fn priority(&self) -> u64 {
        140
    }
    
    fn supported_patterns(&self) -> Vec<String> {
        vec!["struct_declaration".to_string(), "struct_definition".to_string(), "struct_initialization".to_string()]
    }
    
    
    fn can_process(&self, mut context: &mut Context, token_slot: usize, token_range: Range<usize>) -> Result<bool> {
        let tokens = context.tokenizer.get_tokens(token_slot, token_range.clone());
        
        if tokens.is_empty() {
            return Ok(false);
        }
        let tokens = tokens;
        
        let (can_process, confidence) = self.detect_struct_pattern(context, &tokens, token_range.clone())?;
        
        context.registry.add_report(HandlerReport::new(
                &format!("struct_handler_can_process_{}", token_slot),
                std::sync::Arc::new(self.id()),
                "Struct Handler (Trait)".to_string(),
                "can_process".to_string(),
                format!("Struct detection: {} (confidence: {:.2})", can_process, confidence),
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
        
       
            context.registry.add_report(HandlerReport::new(
                &format!("struct_handler_process_{}", token_slot),
                std::sync::Arc::new(self.id()),
                "Struct Handler (Trait)".to_string(),
                "process".to_string(),
                format!("Processing struct with {} tokens", tokens.len()),
                ReportLevel::Info,
                HandlerPhase::Process,
            ).with_tokens(tokens.len(), tokens.len())
             .with_success(true));
        
        let struct_info = self.extract_struct_info(&tokens)?;
        let rust_code = self.convert_to_rust(&struct_info, &tokens)?;
        
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
    
    let struct_info = self.extract_struct_info(&tokens)?;
    let rust_code = self.convert_to_rust(&struct_info, &tokens)?;
    
    let extracted_struct = ExtractedStruct {
        id: Id::get(Id::gen_name(&self.id().name()).as_str()),
        code: rust_code,
        info: struct_info.clone(),
        tokens: tokens.clone(),
        token_range,
        metadata: vec![
            ("is_typedef".to_string(), struct_info.is_typedef.to_string()),
            ("has_body".to_string(), struct_info.has_body.to_string()),
            ("field_count".to_string(), struct_info.fields.len().to_string()),
            ("original_name".to_string(), struct_info.name.clone()),
        ],
    };
    
    Ok(Some(ExtractedElement::Struct(extracted_struct)))
}
    
    fn convert(&self, context: &mut Context,     token_slot: usize, token_range: Range<usize>) -> Result<Option<ConvertedElement>> {
        let tokens = context.tokenizer.get_tokens(token_slot, token_range.clone());
        if tokens.is_empty() {
            return Ok(None);
        }
        let tokens = tokens;
        
        let filtered_tokens = filter_tokens_for_handler(&tokens, token_range.clone());
        let struct_info = self.extract_struct_info(&filtered_tokens)?;
        let rust_code = self.convert_to_rust(&struct_info, &filtered_tokens)?;
        
        let converted_struct = ConvertedStruct {
            fields: struct_info.fields.iter().map(|f| (f.0.clone(), f.1.to_string())).collect(),
            code: rust_code.clone(),
            is_public: false,
            derives: vec!["Debug".to_string(), "Clone".to_string()],
        };
        
        Ok(Some(ConvertedElement::Struct(converted_struct)))
    }
    fn document(&self, context: &mut Context, info: ElementInfo) -> Result<Option<String>> {
        if let ElementInfo::Struct(struct_info) = info {
            let doc = format!(
                "/// Struct: {}\n/// Converted from C struct definition\n",
                struct_info.name
            );
            Ok(Some(doc))
        } else {
            Ok(None)
        }
    }
    
    fn report(&self, context: &mut Context) -> Result<HandlerReport> {
        let reports = context.registry.get_reports_by_handler(&self.id().name());
        
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
        
        Ok(HandlerReport {
            report_id: std::sync::Arc::new(Id::get("struct_handler_report")),
            handler_id: std::sync::Arc::new(self.id()),
            handler_name: "Struct Handler (Trait)".to_string(),
            function_name: "report".to_string(),
            message: format!(
                "Struct handler processed {} info, {} warnings, {} errors",
                info_count, warning_count, error_count
            ),
            details: Some(format!("Processed {} total reports", reports.len())),
            level: ReportLevel::Info,
            tokens_processed: 0,
            tokens_consumed: 0,
            phase: HandlerPhase::Report,
            success: error_count == 0,
            metadata: std::collections::HashMap::new(),
        })
    }
    
    
    fn handle_redirect(&self, context: &mut Context, token_slot: usize, token_range: Range<usize>, result: HandlerResult) -> Result<HandlerResult> {
        // Return the result as-is to avoid deadlock in redirect workflow
        // The redirect should be handled at a higher level in the processing pipeline
        Ok(result)
    }
}

impl Default for StructHandler {
    fn default() -> Self {
        Self::new()
    }
}

