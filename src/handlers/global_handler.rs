//! Global handler implementation using trait-based system
//! Handles global variables, constants, and other global scope declarations

use crate::{
    Context, ConvertedElement, ConvertedGlobal, ElementInfo, ExtractedElement, ExtractedGlobal, GlobalInfo, HandlerPhase, HandlerReport, HandlerResult, Handlizer, Id, RedirectRequest, ReportLevel, Result, Token, context, handler::Handler, report
};
use core::result::Result::Ok;
use std::ops::Range;
#[derive(Debug, Clone)]
pub struct GlobalHandler;

impl GlobalHandler {
    pub fn new() -> Self {
        Self
    }
    
    /// Enhanced pattern detection using both Patternizer and Samplizer
fn detect_global_pattern(context: &mut Context, token_range: Range<usize>) -> Result<(bool, f64)> {
    let token_vec = context.tokenizer.get_tokens(0,token_range.clone());
    let tokens = token_vec.as_slice();
    // First use Patternizer for structural detection
    
    let pattern_match = context.patternizer.match_pattern("global", tokens);

    // Then use Samplizer for confidence scoring
    let patterns = vec!["global_declaration".to_string(), "global_variable".to_string()];
    let confidence = context.samplizer.analyze_with_range(&patterns, tokens, token_range)?;

    // Combined heuristics for global detection
    let has_global_structure = Self::has_global_structure(tokens);
    let enhanced_confidence = if has_global_structure {
        confidence * 1.2  // Boost confidence if structure looks global-like
    } else {
        confidence * 0.7  // Reduce if structure doesn't match
    };

    let pattern_success = !matches!(pattern_match, crate::PatternResult::NoMatch { .. });

    Ok((pattern_success || enhanced_confidence > 0.5, enhanced_confidence))
}
    
    /// Analyze token structure for global declaration patterns
    fn has_global_structure(tokens: &[crate::Token]) -> bool {
        if tokens.is_empty() {
            return false;
        }
        
        let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        
        // Check for global indicators
        let has_extern = token_strings.contains(&"extern".to_string());
        let has_static = token_strings.contains(&"static".to_string());
        let has_const = token_strings.contains(&"const".to_string());
        let ends_with_semicolon = token_strings.last() == Some(&";".to_string());
        
        // Check for variable declaration pattern (type identifier = value;)
        let has_assignment = token_strings.contains(&"=".to_string());
        let has_type_identifier = Self::has_type_identifier_pattern(&token_strings);
        
        (has_extern || has_static || has_const || has_assignment || has_type_identifier) && 
        ends_with_semicolon
    }
    
    /// Check for type-identifier pattern typical in global declarations
    fn has_type_identifier_pattern(tokens: &[String]) -> bool {
    if tokens.len() < 2 {
        return false;
    }
    
    let c_types = [
        "int", "char", "short", "long", "float", "double", "void",
        "unsigned", "signed", "const", "volatile", "struct", "enum", "union"
    ];
    
    // Look for patterns like: type identifier; or type identifier = value;
    tokens.windows(2).any(|pair| {
        let is_type = c_types.contains(&pair[0].as_str()) || pair[0].ends_with("_t");
        let is_identifier = !pair[1].is_empty() && 
                           pair[1].chars().next().map_or(false, |c| c.is_alphabetic()) && 
                           pair[1].chars().all(|c| c.is_alphanumeric() || c == '_');
        is_type && is_identifier
    })
}

/// Extract global information from tokens
fn extract_global_info(&self, tokens: &[crate::Token]) -> Result<GlobalInfo> {
    let mut global_info = GlobalInfo::default();
    let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
    
    let c_types = [
        "int", "char", "short", "long", "float", "double", "void",
        "unsigned", "signed", "struct", "enum", "union", "size_t", "ssize_t"
    ];
    
    for (i, token_str) in token_strings.iter().enumerate() {
        match token_str.as_str() {
            "extern" => {
                global_info.is_extern = true;
                global_info.storage_class = "extern".to_string();
            }
            "static" => {
                global_info.is_static = true;
                global_info.storage_class = "static".to_string();
            }
            "const" => global_info.is_const = true,
            "volatile" => global_info.is_volatile = true,
            "=" => {
                global_info.initializer = Some(token_strings[i+1..].iter()
                    .take_while(|&t| t != ";")
                    .cloned()
                    .collect::<Vec<_>>()
                    .join(" "));
                break;
            }
            ";" => break,
            _ => {
                if global_info.var_type.is_empty() && (c_types.contains(&token_str.as_str()) || token_str.ends_with("_t")) {
                    global_info.var_type = token_str.clone();
                } else if global_info.name.is_empty() && !token_str.is_empty() && 
                         token_str.chars().next().map_or(false, |c| c.is_alphabetic()) {
                    global_info.name = token_str.clone();
                }
            }
        }
    }
    
    Ok(global_info)
}

/// Convert global declaration to Rust code
fn convert_to_rust(&self, global_info: &GlobalInfo) -> Result<String> {
    let rust_name = global_info.name.to_uppercase();
    let rust_type = self.convert_c_type_to_rust(&global_info.var_type);
    
    let rust_code = match (global_info.is_extern, global_info.is_const, global_info.is_static) {
        (true, _, _) => format!("extern \"C\" {{\n    static {}: {};\n}}", rust_name, rust_type),
        (false, true, _) => {
            let initializer = if let Some(ref init) = global_info.initializer {
                init.trim()
            } else {
                self.get_default_value(rust_type)
            };
            format!("pub const {}: {} = {};", rust_name, rust_type, initializer)
        },
        (false, false, true) => {
            let initializer = if let Some(ref init) = global_info.initializer {
                init.trim()
            } else {
                self.get_default_value(rust_type)
            };
            format!("pub static mut {}: {} = {};", rust_name, rust_type, initializer)
        },
        _ => {
            let initializer = if let Some(ref init) = global_info.initializer {
                init.trim()
            } else {
                self.get_default_value(rust_type)
            };
            format!("pub static mut {}: {} = {};", rust_name, rust_type, initializer)
        }
    };
    
    Ok(rust_code)
}

/// Convert C type to Rust equivalent
fn convert_c_type_to_rust(&self, c_type: &str) -> &'static str {
    match c_type.trim() {
        "char" => "i8",
        "unsigned char" => "u8",
        "short" => "i16",
        "unsigned short" => "u16",
        "int" => "i32",
        "unsigned int" => "u32",
        "long" => "i64",
        "unsigned long" => "u64",
        "long long" => "i64",
        "unsigned long long" => "u64",
        "float" => "f32",
        "double" => "f64",
        "void" => "()",
        "size_t" => "usize",
        "ssize_t" => "isize",
        _ => "/* TODO: convert unknown type */",
    }
}

/// Get default value for Rust type
fn get_default_value(&self, rust_type: &str) -> &'static str {
    match rust_type {
        "i8" | "i16" | "i32" | "i64" | "isize" | "u8" | "u16" | "u32" | "u64" | "usize" => "0",
        "f32" | "f64" => "0.0",
        "bool" => "false",
        "()" => "()",
        _ => "Default::default()",
    }
}

}

impl Handler for GlobalHandler {
    fn id(&self) -> Id {
        Id::get("global_handler")
    }
    
    fn role(&self) -> String {
        "global".to_string()
    }
    
    fn priority(&self) -> u64 {
        220
    }
    
    fn can_process(&self,context: &mut Context, token_slot: usize, token_range: Range<usize>) -> Result<bool> {
        let tokens = context.tokenizer.slots()[token_slot].tokens()[token_range.clone()].to_vec();
        
        if tokens.is_empty() {
            return Ok(false);
        }
        let tokens = tokens;
        
        let (can_process, confidence) = GlobalHandler::detect_global_pattern(context, token_range)?;
        
        context.registry.add_report(HandlerReport::new(
                &format!("global_handler_can_process_{}", token_slot),
                std::sync::Arc::new(self.id()),
                "Global Handler".to_string(),
                "can_process".to_string(),
                format!("Global detection: {} (confidence: {:.2})", can_process, confidence),
                ReportLevel::Info,
                HandlerPhase::Process,
            ).with_tokens(tokens.len(), if can_process { tokens.len() } else { 0 })
             .with_success(can_process));
            
        Ok(can_process)
    }
    
    fn process(&self, context: &mut Context, token_slot: usize, token_range: Range<usize>) -> Result<HandlerResult> {
        let tokens = context.tokenizer.slots()[token_slot].tokens()[token_range.clone()].to_vec();
        if tokens.is_empty() {
            return Ok(HandlerResult::Processed(None, token_range, "".to_string(), self.id()));
        }
        let tokens = tokens;   
        let global_info = self.extract_global_info(&tokens)?;
        let rust_code = self.convert_to_rust(&global_info)?;
        context.registry.add_report(HandlerReport::new(
                &format!("global_handler_process_{}", token_slot),
                std::sync::Arc::new(self.id()),
                "Global Handler".to_string(),
                "process".to_string(),
                format!("Processing global with {} tokens", tokens.len()),
                ReportLevel::Info,
                HandlerPhase::Process,
            ).with_tokens(tokens.len(), tokens.len())
             .with_success(true));
            

        
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
    
    let global_info = self.extract_global_info(&tokens)?;
    let rust_code = self.convert_to_rust(&global_info)?;
    
    let extracted_global = ExtractedGlobal {
        id: Id::get(Id::gen_name(&self.id().name()).as_str()),
        code: rust_code,
        info: global_info.clone(),
        tokens: tokens.clone(),
        token_range,
        metadata: vec![
            ("is_extern".to_string(), global_info.is_extern.to_string()),
            ("is_static".to_string(), global_info.is_static.to_string()),
            ("is_const".to_string(), global_info.is_const.to_string()),
            ("storage_class".to_string(), global_info.storage_class.clone()),
            ("has_initializer".to_string(), global_info.initializer.is_some().to_string()),
        ],
    };
    
    Ok(Some(ExtractedElement::Global(extracted_global)))
}
    
    fn convert(&self, context: &mut Context, token_slot: usize, token_range: Range<usize>) -> Result<Option<ConvertedElement>> {
        let tokens = context.tokenizer.slots()[token_slot].tokens()[token_range.clone()].to_vec();
        if tokens.is_empty() {
            return Ok(None);
        }
        let global_info = self.extract_global_info(&tokens)?;
        let rust_code = self.convert_to_rust(&global_info)?;
        
        let converted_global = ConvertedGlobal {
            var_type: global_info.var_type.clone(),
            initializer: Some(global_info.initializer.unwrap()),
            code: rust_code.clone(),
            is_const: false, // TODO: Extract from global_info
            is_static: false,
            is_public: false,
            metadata: Vec::new(),
        };
        
        Ok(Some(ConvertedElement::Global(converted_global)))
    }
    
    fn document(&self,context: &mut Context, info: ElementInfo) -> Result<Option<String>> {
        if let ElementInfo::Global(global_info) = info {
            let doc = format!(
                "/// Global variable: {}\n/// Type: {}\n/// Storage class: {}\n",
                global_info.name, global_info.type_name, global_info.storage_class
            );
            Ok(Some(doc))
        } else { 
            Ok(None)
        }
    }
    
    fn report(&self,context: &mut Context) -> Result<HandlerReport> {
        let reports = context.registry.get_reports_by_handler("global_handler_trait");
        if reports.is_empty() {
            return Ok(HandlerReport::new(
                "global_handler_report",
                std::sync::Arc::new(self.id()),
                "Global Handler".to_string(),
                "report".to_string(),
                "No reports found".to_string(),
                ReportLevel::Info,
                HandlerPhase::Report,
            ));
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
            "global_handler_report",
            std::sync::Arc::new(self.id()),
            "Global Handler".to_string(),
            "report".to_string(),
            format!(
                "Global handler processed {} info, {} warnings, {} errors",
                info_count, warning_count, error_count
            ),
            ReportLevel::Info,
            HandlerPhase::Report,
        ).with_success(error_count == 0)
         .with_metadata("total_reports".to_string(), reports.len().to_string()))
    }
    
    fn supported_patterns(&self) -> Vec<String> {
        vec!["global_variable".to_string(),
        "global_constant".to_string(),
        "external_declaration".to_string(),
        "static_declaration".to_string(),
        "extern_declaration".to_string()]  
    }
    
    fn handle_redirect(&self, context: &mut Context, token_slot: usize, token_range: Range<usize>, result: HandlerResult) -> Result<HandlerResult> {
        let redirect_request = RedirectRequest {
            from_handler: self.id(),
            token_range: token_range.clone(),
            failed_patterns: vec!["global".to_string()],
            suggested_handler: None,
            metadata: Vec::new(),
        };
        // Return the result as-is to avoid deadlock in redirect workflow
        Ok(result)
    }
}

impl Default for GlobalHandler {
    fn default() -> Self {
        Self::new()
    }
}
