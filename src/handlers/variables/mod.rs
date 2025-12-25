//! Variable Handler
//!
//! Converts C variable declarations to Rust.

use crate::db::web::{Entry, Build};
use crate::db::token::Token;
use crate::db::convert::TypeConverter;
use crate::db::pattern::{Pattern, PatternRule};
use crate::handlers::process::{ProcessStage, Processor};
use crate::handlers::validation::SequenceValidator;

// ============================================================================
// Variable Handler Implementation
// ============================================================================

/// Extracted variable information
#[derive(Debug, Clone, Default)]
pub struct VariableData {
    pub name: String,
    pub c_type: String,
    pub initial_value: Option<String>,
    pub is_const: bool,
    pub is_static: bool,
    pub is_pointer: bool,
}

/// Handler for C variable declarations
#[derive(Debug)]
pub struct VariableHandler {
    stage: ProcessStage,
    confidence: f64,
    error: Option<String>,
    output: Option<String>,
    data: VariableData,
    type_converter: TypeConverter,
}

impl VariableHandler {
    pub fn new() -> Self {
        Self {
            stage: ProcessStage::Pending,
            confidence: 0.0,
            error: None,
            output: None,
            data: VariableData::default(),
            type_converter: TypeConverter::new(),
        }
    }
}

impl Processor for VariableHandler {
    fn name(&self) -> &str { "VariableHandler" }
    
    fn supported_patterns(&self) -> &[&str] {
        &["variable_declaration", "variable"]
    }
    
    fn patterns(&self) -> Vec<Pattern> {
        vec![
            Pattern::definition(300, "variable_declaration", vec![
                PatternRule::type_keyword(),
                PatternRule::identifier(),
                PatternRule::one_of(vec![";".to_string(), "=".to_string()]),
            ])
            .with_category("variable")
            .with_priority(85)
            .with_min_tokens(3)
            .with_description("Variable declaration"),
        ]
    }
    
    fn validate(&mut self, tokens: &[Token]) -> bool {
        if tokens.len() < 3 {
            return false;
        }
        
        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        
        // Must end with semicolon
        if token_strs.last() != Some(&";".to_string()) {
            return false;
        }
        
        // Should not have ( ) which would indicate function
        if token_strs.iter().any(|t| t == "(") {
            return false;
        }
        
        // Should not have [ ] which would indicate array
        if token_strs.iter().any(|t| t == "[") {
            return false;
        }
        
        // Try matching against our patterns
        let mut best_confidence = 0.0;
        for pattern in self.patterns() {
            if let Some(confidence) = pattern.matches_tokens(&token_strs) {
                if confidence > best_confidence {
                    best_confidence = confidence;
                }
            }
        }
        
        // Use keyword validation to verify variable declaration sequence
        let validator = SequenceValidator::new();
        let validation_result = validator.validate_variable(tokens);
        
        // Adjust confidence based on both pattern matching and keyword validation
        let base_confidence = if best_confidence > 0.0 { best_confidence } else { 0.7 };
        self.confidence = validation_result.adjust_confidence(base_confidence);
        
        true
    }
    
    fn extract(&mut self, tokens: &[Token]) -> bool {
        let token_strs: Vec<String> = tokens.iter()
            .map(|t| t.to_string())
            .filter(|t| t != ";")
            .collect();
        
        if token_strs.is_empty() {
            return false;
        }
        
        let mut idx = 0;
        
        // Check for const
        if token_strs.get(idx) == Some(&"const".to_string()) {
            self.data.is_const = true;
            idx += 1;
        }
        
        // Check for static
        if token_strs.get(idx) == Some(&"static".to_string()) {
            self.data.is_static = true;
            idx += 1;
        }
        
        // Find equals sign for initialization
        let eq_pos = token_strs.iter().position(|t| t == "=");
        
        if let Some(eq) = eq_pos {
            // Has initializer
            // Type is from idx to eq-1, name is at eq-1
            if eq < idx + 2 {
                return false;
            }
            
            // Check for pointer
            if token_strs[idx..eq].iter().any(|t| t == "*") {
                self.data.is_pointer = true;
            }
            
            self.data.name = token_strs[eq - 1].clone();
            let type_tokens: Vec<&str> = token_strs[idx..eq - 1]
                .iter()
                .filter(|t| *t != "*")
                .map(|s| s.as_str())
                .collect();
            self.data.c_type = type_tokens.join(" ");
            
            // Collect initializer
            let init_tokens: Vec<&str> = token_strs[eq + 1..]
                .iter()
                .map(|s| s.as_str())
                .collect();
            self.data.initial_value = Some(init_tokens.join(" "));
        } else {
            // No initializer
            if token_strs.len() < idx + 2 {
                return false;
            }
            
            // Check for pointer
            if token_strs[idx..].iter().any(|t| t == "*") {
                self.data.is_pointer = true;
            }
            
            self.data.name = token_strs.last().unwrap().clone();
            let type_tokens: Vec<&str> = token_strs[idx..token_strs.len() - 1]
                .iter()
                .filter(|t| *t != "*")
                .map(|s| s.as_str())
                .collect();
            self.data.c_type = type_tokens.join(" ");
        }
        
        self.confidence = 0.8;
        true
    }
    
    fn convert(&mut self) -> Option<String> {
        let rust_type = self.type_converter.convert(&self.data.c_type)
            .unwrap_or_else(|| self.data.c_type.clone());
        
        let full_type = if self.data.is_pointer {
            format!("*const {}", rust_type)
        } else {
            rust_type
        };
        
        let prefix = if self.data.is_const {
            "const"
        } else if self.data.is_static {
            "static"
        } else {
            "let mut"
        };
        
        let rust_code = if let Some(ref init) = self.data.initial_value {
            format!("{} {}: {} = {};", prefix, self.data.name, full_type, init)
        } else {
            // Uninitialized - need default or explicit type
            format!("{} {}: {};", prefix, self.data.name, full_type)
        };
        
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

impl Build for VariableHandler {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("Handler", "VariableHandler");
        entry.set_attr("stage", Entry::string(self.stage.as_str()));
        entry.set_attr("confidence", Entry::f64(self.confidence));
        if let Some(ref output) = self.output {
            entry.set_attr("output", Entry::string(output));
        }
        entry.set_attr("variable_name", Entry::string(&self.data.name));
        entry.set_attr("c_type", Entry::string(&self.data.c_type));
        entry
    }
    
    fn kind(&self) -> &str { "Handler" }
    fn name(&self) -> Option<&str> { Some("VariableHandler") }
    fn category(&self) -> Option<&str> { Some("variable") }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    fn make_tokens(strs: &[&str]) -> Vec<Token> {
        strs.iter().map(|s| Token::from_string(*s)).collect()
    }
    
    #[test]
    fn test_simple_variable() {
        let tokens = make_tokens(&["int", "x", ";"]);
        let mut handler = VariableHandler::new();
        
        assert!(handler.validate(&tokens));
        assert!(handler.extract(&tokens));
        
        let output = handler.convert();
        assert!(output.is_some());
        let code = output.unwrap();
        assert!(code.contains("x"));
        assert!(code.contains("i32"));
    }
    
    #[test]
    fn test_variable_with_init() {
        let tokens = make_tokens(&["int", "count", "=", "10", ";"]);
        let mut handler = VariableHandler::new();
        
        assert!(handler.validate(&tokens));
        assert!(handler.extract(&tokens));
        
        let output = handler.convert();
        assert!(output.is_some());
        let code = output.unwrap();
        assert!(code.contains("count"));
        assert!(code.contains("= 10"));
    }
    
    #[test]
    fn test_const_variable() {
        let tokens = make_tokens(&["const", "int", "MAX", "=", "100", ";"]);
        let mut handler = VariableHandler::new();
        
        assert!(handler.validate(&tokens));
        assert!(handler.extract(&tokens));
        assert!(handler.data.is_const);
        
        let output = handler.convert();
        assert!(output.is_some());
        assert!(output.unwrap().starts_with("const"));
    }
}
