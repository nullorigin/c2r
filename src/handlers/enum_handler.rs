//! Enum handler implementation using trait-based system
//! Uses Patternizer and Samplizer for intelligent enum pattern detection and processing

use crate::{
    Context, ConvertedElement, ConvertedEnum, ElementInfo, ExtractedElement, ExtractedEnum, Handler, HandlerPhase, HandlerReport, HandlerResult, Id, RedirectRequest, ReportLevel, Result, Token, info::EnumInfo
};
use std::ops::Range;

/// Enum handler struct implementing the Handler trait
#[derive(Debug, Clone)]
pub struct EnumHandler;

impl EnumHandler {
    pub fn new() -> Self {
        Self
    }
    
    /// Enhanced pattern detection using both Patternizer and Samplizer
fn detect_enum_pattern(&self, context: &mut Context, tokens: &[Token], token_range: Range<usize>) -> Result<(bool, f64)> {
    // First use Patternizer for structural detection
    let pattern_match = context.patternizer.match_pattern("enum", tokens);
    
    // Then use Samplizer for confidence scoring
    let patterns = vec!["enum_declaration".to_string(), "typedef_enum".to_string()];
    let confidence = context.samplizer.analyze_with_range(&patterns, tokens, token_range)?;
    
    // Combined heuristics for enum detection
    let has_enum_structure = self.has_enum_structure(tokens);
    let enhanced_confidence = if has_enum_structure {
        confidence * 1.4  // Boost confidence if structure looks enum-like
    } else {
        confidence * 0.6  // Reduce if structure doesn't match
    };
    
    let pattern_success = !matches!(pattern_match, crate::PatternResult::NoMatch { .. });
    
    Ok((pattern_success || enhanced_confidence > 0.7, enhanced_confidence))
}
    
    /// Analyze token structure for enum-like patterns
    fn has_enum_structure(&self, tokens: &[Token]) -> bool {
        if tokens.len() < 2 {
            return false;
        }
        
        let mut has_enum_keyword = false;
        let mut has_braces = false;
        let mut has_typedef = false;
        
        for token in tokens {
            let token_str = token.to_string();
            
            match token_str.as_str() {
                "enum" => has_enum_keyword = true,
                "typedef" => has_typedef = true,
                "{" | "}" => has_braces = true,
                _ => {}
            }
        }
        
        has_enum_keyword && (has_braces || has_typedef)
    }
    
    /// Extract enum information from tokens
    fn extract_enum_info(&self, tokens: &[Token]) -> Result<EnumInfo> {
    let mut enum_info = EnumInfo::default();
    let mut in_body = false;
    let mut current_value = String::new();
    let mut brace_count = 0;
    
    for (i, token) in tokens.iter().enumerate() {
        let token_str = token.to_string();
        
        match token_str.as_str() {
            "typedef" => {
                enum_info.is_typedef = true;
            }
            "enum" => {
                // Next token might be the enum name
                if i + 1 < tokens.len() {
                    let next_token = tokens[i + 1].to_string();
                    if next_token != "{" && !next_token.is_empty() {
                        enum_info.name = next_token;
                    }
                }
            }
            "{" => {
                in_body = true;
                enum_info.has_body = true;
                brace_count += 1;
            }
            "}" => {
                brace_count -= 1;
                if brace_count == 0 {
                    in_body = false;
                    // Add any remaining value
                    if !current_value.trim().is_empty() {
                        enum_info.variants.push((current_value.trim().to_string(), None));
                        current_value.clear();
                    }
                }
            }
            "," => {
                if in_body && !current_value.trim().is_empty() {
                    enum_info.variants.push((current_value.trim().to_string(), None));
                    current_value.clear();
                }
            }
            _ => {
                if in_body {
                    if !current_value.is_empty() {
                        current_value.push(' ');
                    }
                    current_value.push_str(&token_str);
                }
            }
        }
    }
    
    Ok(enum_info)
}
    
    /// Convert enum to Rust code   
    fn convert_to_rust(&self, enum_info: &EnumInfo, tokens: &[Token]) -> Result<String> {
        let mut rust_code = String::new();
        
        // Add derives and attributes
        rust_code.push_str("#[derive(Debug, Clone, Copy, PartialEq, Eq)]\n");
        
        // Add enum visibility
        rust_code.push_str("pub enum ");
        
        // Add enum name
        let rust_name = self.convert_identifier_to_rust(&enum_info.name);
        rust_code.push_str(&rust_name);
        
        if enum_info.has_body && !enum_info.variants.is_empty() {
            rust_code.push_str(" {\n");
            
            // Convert enum values
            for (i, value) in enum_info.variants.iter().enumerate() {
                rust_code.push_str("    ");
                let rust_value = self.convert_enum_value_to_rust(value.1.clone().unwrap().as_str());
                rust_code.push_str(&rust_value);
                
                if i < enum_info.variants.len() - 1 {
                    rust_code.push(',');
                }
                rust_code.push('\n');
            }
            
            rust_code.push('}');
        } else {
            rust_code.push(';');
        }
        
        Ok(rust_code)
    }
    
    /// Convert C enum value to Rust
    fn convert_enum_value_to_rust(&self, value: &str) -> String {
        // Parse value which might be "NAME" or "NAME = expression"
        let parts: Vec<&str> = value.split('=').collect();
        let name = parts[0].trim();
        let rust_variant = self.convert_identifier_to_rust(name);
        
        if parts.len() > 1 {
            let expr = parts[1].trim();
            format!("{} = {}", rust_variant, expr)
        } else {
            rust_variant
        }
    }
    
    /// Convert C identifier to Rust naming convention
    fn convert_identifier_to_rust(&self, identifier: &str) -> String {
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

impl Handler for EnumHandler {
    fn id(&self) -> Id {
        Id::get("enum_handler")
    }
    
    fn role(&self) -> String {
        "enum".to_string()
    }
    
    fn priority(&self) -> u64 {
        160
    }
    
    fn can_process(&self, context: &mut Context, token_slot: usize, token_range: Range<usize>) -> Result<bool> {
        let tokens = context.tokenizer.get_tokens(token_slot, token_range.clone());
        
        if tokens.is_empty() {
            return Ok(false);
        }
        
        let (can_process, confidence) = self.detect_enum_pattern(context, &tokens, token_range)?;
        
        context.registry.add_report(
            HandlerReport::new(
                &format!("enum_handler_can_process_{}", token_slot),
                std::sync::Arc::new(self.id()),
                "Enum Handler (Trait)".to_string(),
                "can_process".to_string(),
                format!("Enum detection: {} (confidence: {:.2})", can_process, confidence),
                ReportLevel::Info,
                HandlerPhase::Process,
            )
            .with_tokens(tokens.len(), if can_process { tokens.len() } else { 0 })
            .with_success(can_process)
        );
        
        Ok(can_process)
    }

    fn process(&self, context: &mut Context, token_slot: usize, token_range: Range<usize>) -> Result<HandlerResult> {
        let tokens = context.tokenizer.get_tokens(token_slot, token_range.clone());
        
        if tokens.is_empty() {
            return Ok(HandlerResult::NotHandled(None, token_range, self.id()));
        }
        
        context.registry.add_report(
            HandlerReport::new(
                &format!("enum_handler_process_{}", token_slot),
                std::sync::Arc::new(self.id()),
                "Enum Handler (Trait)".to_string(),
                "process".to_string(),
                format!("Processing enum with {} tokens", tokens.len()),
                ReportLevel::Info,
                HandlerPhase::Process,
            )
            .with_tokens(tokens.len(), tokens.len())
            .with_success(true)
        );
        
        let enum_info = self.extract_enum_info(&tokens)?;
        let rust_code = self.convert_to_rust(&enum_info, &tokens)?;
        
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
    
    let enum_info = self.extract_enum_info(&tokens)?;
    let rust_code = self.convert_to_rust(&enum_info, &tokens)?;
    
    let extracted_enum = ExtractedEnum {
        id: Id::get(Id::gen_name(&self.id().name()).as_str()),
        code: rust_code,
        info: enum_info.clone(),
        tokens: tokens.clone(),
        token_range,
        metadata: vec![
            ("is_typedef".to_string(), enum_info.is_typedef.to_string()),
            ("has_body".to_string(), enum_info.has_body.to_string()),
            ("value_count".to_string(), enum_info.variants.len().to_string()),
            ("original_name".to_string(), enum_info.name.clone()),
        ],
    };
    
    Ok(Some(ExtractedElement::Enum(extracted_enum)))
}

fn convert(&self, context: &mut Context, token_slot: usize, token_range: Range<usize>) -> Result<Option<ConvertedElement>> {
    let tokens = context.tokenizer.get_tokens(token_slot, token_range.clone());
    
    if tokens.is_empty() {
        return Ok(None);
    }
    
    let enum_info = self.extract_enum_info(&tokens)?;
    let rust_code = self.convert_to_rust(&enum_info, &tokens)?;
    
    let converted_enum = ConvertedEnum {
        variants: enum_info.variants,
        code: rust_code,
        is_public: true,
        repr: todo!(),
    };
    
    Ok(Some(ConvertedElement::Enum(converted_enum)))
}
    
    fn document(&self, context: &mut Context, info: ElementInfo) -> Result<Option<String>> {
        if let ElementInfo::Enum(enum_info) = info {
            let mut doc = format!(
                "/// Enum: {}\n/// Converted from C enum definition\n/// Values:\n",
                enum_info.name
            );
            
            for (variant_name, variant_value) in &enum_info.variants {
                if let Some(value) = variant_value {
                    doc.push_str(&format!("/// - {}: {}\n", variant_name, value));
                } else {
                    doc.push_str(&format!("/// - {}\n", variant_name));
                }
            }
            
            Ok(Some(doc))
        } else {
            Ok(None)
        }
    }
    
    fn report(&self, context: &mut Context) -> Result<HandlerReport> {
let reports = context.registry.get_reports_by_handler("enum_handler_trait");
        if reports.is_empty() {
            return Ok(HandlerReport::new(
                "enum_handler_report",
                std::sync::Arc::new(self.id()),
                "Enum Handler (Trait)".to_string(),
                "report".to_string(),
                "No reports found".to_string(),
                ReportLevel::Info,
                HandlerPhase::Report,
            ).with_success(true)
             .with_metadata("total_reports".to_string(), "0".to_string()));
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
        
        Ok(HandlerReport::new(
            "enum_handler_report",
            std::sync::Arc::new(self.id()),
            "Enum Handler (Trait)".to_string(),
            "report".to_string(),
            format!(
                "Enum handler processed {} info, {} warnings, {} errors",
                info_count, warning_count, error_count
            ),
            ReportLevel::Info,
            HandlerPhase::Report,
        ).with_success(error_count == 0)
         .with_metadata("total_reports".to_string(), reports.len().to_string()))
    }
    
    fn supported_patterns(&self) -> Vec<String> {
        vec![
            "enum_declaration".to_string(),
            "enum_definition".to_string(),
            "enum_value".to_string(),
        ]
    }
    
    fn handle_redirect(&self, context: &mut Context, token_slot: usize, token_range: Range<usize>, result: HandlerResult) -> Result<HandlerResult> {
        let redirect_request = RedirectRequest {
            from_handler: self.id(),
            token_range: token_range.clone(),
            failed_patterns: vec!["enum".to_string()],
            suggested_handler: None,
            metadata: Vec::new(),
        };
        // Return the result as-is to avoid deadlock in redirect workflow
        Ok(result)
    }
}

impl Default for EnumHandler {
    fn default() -> Self {
        Self::new()
    }
}

pub fn create_enum_handler() -> EnumHandler {
    EnumHandler::new()
}
