//! Function Definition Handler
//!
//! Converts C function definitions to Rust function definitions.
//! This handles the full function structure including signature and body.

use crate::db::web::{Entry, Build};
use crate::db::token::Token;
use crate::db::convert::{TypeConverter, IdentifierConverter};
use crate::db::pattern::{Pattern, PatternRule};
use crate::handlers::process::{ProcessStage, Processor};

// ============================================================================
// Function Definition Handler Implementation
// ============================================================================

/// Extracted function definition information
#[derive(Debug, Clone, Default)]
pub struct DefinitionData {
    /// Function name
    pub name: String,
    /// Return type (C type)
    pub return_type: String,
    /// Parameters: (name, type, is_pointer)
    pub parameters: Vec<(String, String, bool)>,
    /// Function body tokens (between { and })
    pub body_tokens: Vec<Token>,
    /// Whether this is static
    pub is_static: bool,
    /// Whether this is inline
    pub is_inline: bool,
    /// Whether this is extern
    pub is_extern: bool,
}

/// Handler for C function definitions
#[derive(Debug)]
pub struct DefinitionHandler {
    stage: ProcessStage,
    confidence: f64,
    error: Option<String>,
    output: Option<String>,
    data: DefinitionData,
    type_converter: TypeConverter,
    id_converter: IdentifierConverter,
}

impl DefinitionHandler {
    pub fn new() -> Self {
        Self {
            stage: ProcessStage::Pending,
            confidence: 0.0,
            error: None,
            output: None,
            data: DefinitionData::default(),
            type_converter: TypeConverter::new(),
            id_converter: IdentifierConverter::new(),
        }
    }
}

impl Default for DefinitionHandler {
    fn default() -> Self {
        Self::new()
    }
}

impl Processor for DefinitionHandler {
    fn name(&self) -> &str { "DefinitionHandler" }
    
    fn supported_patterns(&self) -> &[&str] {
        &["function_definition", "function"]
    }
    
    fn patterns(&self) -> Vec<Pattern> {
        vec![
            // Standard function: type name(...) {
            Pattern::definition(600, "function_definition", vec![
                PatternRule::type_keyword(),
                PatternRule::identifier(),
                PatternRule::exact("("),
                PatternRule::any(),
                PatternRule::exact(")"),
                PatternRule::exact("{"),
            ])
            .with_category("function")
            .with_priority(150)
            .with_min_tokens(6)
            .with_description("Function definition with body"),
            
            // Static function: static type name(...) {
            Pattern::definition(601, "static_function_definition", vec![
                PatternRule::exact("static"),
                PatternRule::type_keyword(),
                PatternRule::identifier(),
                PatternRule::exact("("),
                PatternRule::any(),
                PatternRule::exact(")"),
                PatternRule::exact("{"),
            ])
            .with_category("function")
            .with_priority(155)
            .with_min_tokens(7)
            .with_description("Static function definition"),
            
            // Inline function: inline type name(...) {
            Pattern::definition(602, "inline_function_definition", vec![
                PatternRule::one_of(vec!["inline".to_string(), "__inline".to_string(), "__inline__".to_string()]),
                PatternRule::type_keyword(),
                PatternRule::identifier(),
                PatternRule::exact("("),
                PatternRule::any(),
                PatternRule::exact(")"),
                PatternRule::exact("{"),
            ])
            .with_category("function")
            .with_priority(155)
            .with_min_tokens(7)
            .with_description("Inline function definition"),
            
            // Static inline function: static inline type name(...) {
            Pattern::definition(603, "static_inline_function_definition", vec![
                PatternRule::exact("static"),
                PatternRule::one_of(vec!["inline".to_string(), "__inline".to_string(), "__inline__".to_string()]),
                PatternRule::type_keyword(),
                PatternRule::identifier(),
                PatternRule::exact("("),
                PatternRule::any(),
                PatternRule::exact(")"),
                PatternRule::exact("{"),
            ])
            .with_category("function")
            .with_priority(160)
            .with_min_tokens(8)
            .with_description("Static inline function definition"),
        ]
    }
    
    fn validate(&mut self, tokens: &[Token]) -> bool {
        if tokens.len() < 6 {
            return false;
        }
        
        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        
        // Must have ( and )
        let open_paren = token_strs.iter().position(|t| t == "(");
        let close_paren = token_strs.iter().position(|t| t == ")");
        
        if open_paren.is_none() || close_paren.is_none() {
            return false;
        }
        
        // Must have { after ) - this distinguishes definition from declaration
        let close_pos = close_paren.unwrap();
        if close_pos + 1 >= token_strs.len() {
            return false;
        }
        
        // Look for { after )
        let has_body = token_strs[close_pos + 1..].iter().any(|t| t == "{");
        if !has_body {
            return false;
        }
        
        self.confidence = 0.8;
        true
    }
    
    fn extract(&mut self, tokens: &[Token]) -> bool {
        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        
        let mut idx = 0;
        
        // Check for modifiers
        while idx < token_strs.len() {
            match token_strs[idx].as_str() {
                "static" => {
                    self.data.is_static = true;
                    idx += 1;
                }
                "inline" | "__inline" | "__inline__" => {
                    self.data.is_inline = true;
                    idx += 1;
                }
                "extern" => {
                    self.data.is_extern = true;
                    idx += 1;
                }
                _ => break,
            }
        }
        
        // Find open paren
        let paren_pos = match token_strs[idx..].iter().position(|t| t == "(") {
            Some(p) => idx + p,
            None => return false,
        };
        
        // Return type is from idx to paren_pos - 1
        // Function name is at paren_pos - 1
        if paren_pos < idx + 1 {
            return false;
        }
        
        self.data.name = token_strs[paren_pos - 1].clone();
        
        // Return type (may be multiple tokens like "unsigned int")
        if paren_pos > idx + 1 {
            self.data.return_type = token_strs[idx..paren_pos - 1].join(" ");
        } else {
            self.data.return_type = "void".to_string();
        }
        
        // Find close paren
        let close_paren = match token_strs.iter().position(|t| t == ")") {
            Some(p) => p,
            None => return false,
        };
        
        // Extract parameters
        if close_paren > paren_pos + 1 {
            let param_tokens: Vec<String> = token_strs[paren_pos + 1..close_paren].to_vec();
            self.parse_parameters(&param_tokens);
        }
        
        // Find body (between { and })
        let open_brace = token_strs.iter().position(|t| t == "{");
        let close_brace = token_strs.iter().rposition(|t| t == "}");
        
        if let (Some(ob), Some(cb)) = (open_brace, close_brace) {
            if cb > ob + 1 {
                self.data.body_tokens = tokens[ob + 1..cb].to_vec();
            }
        }
        
        self.confidence = 0.85;
        true
    }
    
    fn convert(&mut self) -> Option<String> {
        // Convert return type
        let rust_return_type = if self.data.return_type == "void" || self.data.return_type.is_empty() {
            "()".to_string()
        } else {
            self.type_converter.convert(&self.data.return_type)
                .unwrap_or_else(|| self.data.return_type.clone())
        };
        let rust_return = if rust_return_type == "()" {
            String::new()
        } else {
            format!(" -> {}", rust_return_type)
        };
        
        // Convert parameters and collect for registration
        let mut param_info: Vec<(String, String)> = Vec::new();
        let params: Vec<String> = self.data.parameters.iter().map(|(name, c_type, is_ptr)| {
            let rust_type = TypeConverter::new().convert(c_type)
                .unwrap_or_else(|| c_type.clone());
            let full_type = if *is_ptr {
                format!("*const {}", rust_type)
            } else {
                rust_type.clone()
            };
            param_info.push((name.clone(), full_type.clone()));
            format!("{}: {}", name, full_type)
        }).collect();
        let params_str = params.join(", ");
        
        // Register the function in the System database
        crate::system::system_mut().register_function(
            &self.data.name,
            &self.data.name, // Keep same name for now (could convert to snake_case)
            &rust_return_type,
            param_info,
        );
        
        // Build function signature
        let visibility = if self.data.is_static { "" } else { "pub " };
        let inline_attr = if self.data.is_inline { "#[inline]\n" } else { "" };
        
        // Convert body
        let body = if self.data.body_tokens.is_empty() {
            "    todo!()".to_string()
        } else {
            self.convert_body()
        };
        
        let rust_code = format!(
            "{}{}fn {}({}){} {{\n{}\n}}",
            inline_attr, visibility, self.data.name, params_str, rust_return, body
        );
        
        self.confidence = 0.85;
        Some(rust_code)
    }
    
    fn current_stage(&self) -> ProcessStage { self.stage }
    fn set_stage(&mut self, stage: ProcessStage) { self.stage = stage; }
    fn output(&self) -> Option<String> { self.output.clone() }
    fn set_output(&mut self, output: String) { self.output = Some(output); }
    fn error(&self) -> Option<String> { self.error.clone() }
    fn set_error(&mut self, error: String) { self.error = Some(error); }
    fn confidence(&self) -> f64 { self.confidence }
    fn set_confidence(&mut self, confidence: f64) { self.confidence = confidence; }
}

impl DefinitionHandler {
    fn parse_parameters(&mut self, tokens: &[String]) {
        let mut current: Vec<&str> = Vec::new();
        
        for token in tokens {
            if token == "," {
                if !current.is_empty() {
                    self.add_parameter(&current);
                    current.clear();
                }
            } else {
                current.push(token);
            }
        }
        
        if !current.is_empty() {
            self.add_parameter(&current);
        }
    }
    
    fn add_parameter(&mut self, tokens: &[&str]) {
        if tokens.is_empty() || tokens == &["void"] {
            return;
        }
        
        let mut is_pointer = tokens.iter().any(|t| *t == "*");
        
        // Last non-* token is the name
        let name_idx = tokens.iter().rposition(|t| *t != "*").unwrap_or(0);
        let name = tokens[name_idx].to_string();
        
        // Type is everything before the name, excluding *
        let type_tokens: Vec<&str> = tokens[..name_idx]
            .iter()
            .filter(|t| **t != "*")
            .copied()
            .collect();
        let c_type = if type_tokens.is_empty() {
            "int".to_string()
        } else {
            type_tokens.join(" ")
        };
        
        self.data.parameters.push((name, c_type, is_pointer));
    }
    
    fn convert_body(&mut self) -> String {
        let mut lines: Vec<String> = Vec::new();
        let token_strs: Vec<String> = self.data.body_tokens.iter()
            .map(|t| t.to_string())
            .collect();
        
        // Segment body into statements
        let mut current: Vec<&str> = Vec::new();
        let mut brace_depth: usize = 0;
        
        for token in &token_strs {
            match token.as_str() {
                "{" => {
                    current.push(token);
                    brace_depth += 1;
                }
                "}" => {
                    current.push(token);
                    brace_depth = brace_depth.saturating_sub(1);
                }
                ";" => {
                    current.push(token);
                    if brace_depth == 0 && !current.is_empty() {
                        let stmt = self.convert_statement(&current);
                        lines.push(stmt);
                        current.clear();
                    }
                }
                _ => {
                    current.push(token);
                }
            }
        }
        
        if !current.is_empty() {
            let stmt = self.convert_statement(&current);
            lines.push(stmt);
        }
        
        if lines.is_empty() {
            "    todo!()".to_string()
        } else {
            lines.iter()
                .map(|l| format!("    {}", l))
                .collect::<Vec<_>>()
                .join("\n")
        }
    }
    
    fn convert_statement(&mut self, tokens: &[&str]) -> String {
        if tokens.is_empty() {
            return String::new();
        }
        
        // Return statement
        if tokens.first() == Some(&"return") {
            let expr: Vec<&str> = tokens[1..].iter()
                .filter(|t| **t != ";")
                .copied()
                .collect();
            return format!("return {};", expr.join(" "));
        }
        
        // Variable declaration with array
        if tokens.iter().any(|t| *t == "[") {
            return self.convert_array_decl(tokens);
        }
        
        // Variable declaration with initialization
        if tokens.iter().any(|t| *t == "=") {
            return self.convert_var_decl(tokens);
        }
        
        // Function call (identifier followed by parentheses)
        if tokens.len() >= 3 {
            if let Some(paren_pos) = tokens.iter().position(|t| *t == "(") {
                if paren_pos > 0 {
                    return self.convert_call(tokens);
                }
            }
        }
        
        // Default: pass through as comment
        format!("// {}", tokens.join(" "))
    }
    
    fn convert_array_decl(&mut self, tokens: &[&str]) -> String {
        let bracket_pos = tokens.iter().position(|t| *t == "[");
        let close_bracket = tokens.iter().position(|t| *t == "]");
        let equals_pos = tokens.iter().position(|t| *t == "=");
        
        if let (Some(bp), Some(cb)) = (bracket_pos, close_bracket) {
            let c_type = if bp > 0 { tokens[0] } else { "int" };
            let name = if bp > 1 { tokens[bp - 1] } else { "arr" };
            let size = if cb > bp + 1 { tokens[bp + 1] } else { "" };
            
            let rust_type = TypeConverter::new().convert(c_type)
                .unwrap_or_else(|| c_type.to_string());
            
            if let Some(_eq) = equals_pos {
                let brace_start = tokens.iter().position(|t| *t == "{");
                let brace_end = tokens.iter().rposition(|t| *t == "}");
                
                if let (Some(bs), Some(be)) = (brace_start, brace_end) {
                    let init_tokens: Vec<&str> = tokens[bs..=be].to_vec();
                    let init = init_tokens.join(" ")
                        .replace("{ ", "[")
                        .replace(" }", "]")
                        .replace("{", "[")
                        .replace("}", "]");
                    
                    return format!("let {}: [{}; {}] = {};", name, rust_type, size, init);
                }
            }
            
            format!("let {}: [{}; {}];", name, rust_type, size)
        } else {
            format!("// array: {}", tokens.join(" "))
        }
    }
    
    fn convert_var_decl(&mut self, tokens: &[&str]) -> String {
        let eq_pos = tokens.iter().position(|t| *t == "=").unwrap_or(0);
        
        if eq_pos < 2 {
            return format!("// var: {}", tokens.join(" "));
        }
        
        let c_type = tokens[0];
        let name = tokens[eq_pos - 1];
        let value: Vec<&str> = tokens[eq_pos + 1..]
            .iter()
            .filter(|t| **t != ";")
            .copied()
            .collect();
        
        let rust_type = TypeConverter::new().convert(c_type)
            .unwrap_or_else(|| c_type.to_string());
        
        format!("let {}: {} = {};", name, rust_type, value.join(" "))
    }
    
    fn convert_call(&mut self, tokens: &[&str]) -> String {
        // Get function name (first token before parenthesis)
        let paren_pos = tokens.iter().position(|t| *t == "(").unwrap_or(tokens.len());
        let func_name = if paren_pos > 0 { tokens[0] } else { "" };
        
        // Convert function name using IdentifierConverter
        let rust_func = self.id_converter.convert(func_name)
            .unwrap_or_else(|| func_name.to_string());
        
        // Convert arguments
        let args: Vec<String> = tokens[paren_pos..].iter()
            .filter(|t| **t != ";" && **t != "(" && **t != ")")
            .map(|t| {
                self.id_converter.convert(t)
                    .unwrap_or_else(|| t.to_string())
            })
            .collect();
        
        format!("{}({});" , rust_func, args.join(", "))
    }
}

impl Build for DefinitionHandler {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("Handler", "DefinitionHandler");
        entry.set_attr("stage", Entry::string(self.stage.as_str()));
        entry.set_attr("confidence", Entry::f64(self.confidence));
        if let Some(ref output) = self.output {
            entry.set_attr("output", Entry::string(output));
        }
        entry.set_attr("function_name", Entry::string(&self.data.name));
        entry.set_attr("return_type", Entry::string(&self.data.return_type));
        entry
    }
    
    fn kind(&self) -> &str { "Handler" }
    fn name(&self) -> Option<&str> { Some("DefinitionHandler") }
    fn category(&self) -> Option<&str> { Some("function") }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    fn make_tokens(strs: &[&str]) -> Vec<Token> {
        strs.iter().map(|s| Token::from_string(*s)).collect()
    }
    
    #[test]
    fn test_simple_function() {
        let tokens = make_tokens(&["int", "main", "(", ")", "{", "return", "0", ";", "}"]);
        let mut handler = DefinitionHandler::new();
        
        assert!(handler.validate(&tokens));
        assert!(handler.extract(&tokens));
        assert_eq!(handler.data.name, "main");
        assert_eq!(handler.data.return_type, "int");
        
        let output = handler.convert();
        assert!(output.is_some());
        let code = output.unwrap();
        assert!(code.contains("fn main()"));
        assert!(code.contains("-> i32"));
    }
    
    #[test]
    fn test_void_function() {
        let tokens = make_tokens(&["void", "setup", "(", ")", "{", "}"]);
        let mut handler = DefinitionHandler::new();
        
        assert!(handler.validate(&tokens));
        assert!(handler.extract(&tokens));
        assert_eq!(handler.data.return_type, "void");
        
        let output = handler.convert();
        assert!(output.is_some());
        let code = output.unwrap();
        assert!(!code.contains("->"));
    }
    
    #[test]
    fn test_function_with_params() {
        let tokens = make_tokens(&["int", "add", "(", "int", "a", ",", "int", "b", ")", "{", "return", "a", "+", "b", ";", "}"]);
        let mut handler = DefinitionHandler::new();
        
        assert!(handler.validate(&tokens));
        assert!(handler.extract(&tokens));
        assert_eq!(handler.data.parameters.len(), 2);
        
        let output = handler.convert();
        assert!(output.is_some());
        let code = output.unwrap();
        assert!(code.contains("a: i32"));
        assert!(code.contains("b: i32"));
    }
    
    #[test]
    fn test_static_function() {
        let tokens = make_tokens(&["static", "int", "helper", "(", ")", "{", "return", "1", ";", "}"]);
        let mut handler = DefinitionHandler::new();
        
        assert!(handler.validate(&tokens));
        assert!(handler.extract(&tokens));
        assert!(handler.data.is_static);
        
        let output = handler.convert();
        assert!(output.is_some());
        let code = output.unwrap();
        assert!(!code.contains("pub "));
    }
    
    #[test]
    fn test_inline_function() {
        let tokens = make_tokens(&["inline", "int", "square", "(", "int", "x", ")", "{", "return", "x", "*", "x", ";", "}"]);
        let mut handler = DefinitionHandler::new();
        
        assert!(handler.validate(&tokens));
        assert!(handler.extract(&tokens));
        assert!(handler.data.is_inline);
        
        let output = handler.convert();
        assert!(output.is_some());
        let code = output.unwrap();
        assert!(code.contains("#[inline]"));
    }
}
