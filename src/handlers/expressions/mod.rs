//! Expression Handler
//!
//! Converts C expressions to Rust expressions.

pub mod comparisons;
pub mod string_format;

pub use comparisons::ComparisonConverter;
pub use string_format::StringFormatHandler;

use crate::db::web::{Entry, Build};
use crate::db::token::Token;
use crate::db::pattern::{Pattern, PatternRule};
use crate::handlers::process::{ProcessStage, Processor};

// ============================================================================
// Expression Handler Implementation
// ============================================================================

/// Extracted expression information
#[derive(Debug, Clone, Default)]
pub struct ExpressionData {
    /// The original expression tokens
    pub tokens: Vec<String>,
    /// Operator if binary expression
    pub operator: Option<String>,
    /// Left operand
    pub left: Option<String>,
    /// Right operand  
    pub right: Option<String>,
}

/// Handler for C expressions
#[derive(Debug)]
pub struct ExpressionHandler {
    stage: ProcessStage,
    confidence: f64,
    error: Option<String>,
    output: Option<String>,
    data: ExpressionData,
}

impl ExpressionHandler {
    pub fn new() -> Self {
        Self {
            stage: ProcessStage::Pending,
            confidence: 0.0,
            error: None,
            output: None,
            data: ExpressionData::default(),
        }
    }
    
    /// Check if token is an operator
    fn is_operator(token: &str) -> bool {
        matches!(token, "+" | "-" | "*" | "/" | "%" | 
                       "==" | "!=" | "<" | ">" | "<=" | ">=" |
                       "&&" | "||" | "&" | "|" | "^" |
                       "<<" | ">>" | "+=" | "-=" | "*=" | "/=")
    }
    
    /// Convert C operator to Rust (most are the same)
    fn convert_operator(op: &str) -> &str {
        match op {
            // Most operators are the same in Rust
            _ => op
        }
    }
}

impl Processor for ExpressionHandler {
    fn name(&self) -> &str { "ExpressionHandler" }
    
    fn supported_patterns(&self) -> &[&str] {
        &["binary_expression", "expression"]
    }
    
    fn patterns(&self) -> Vec<Pattern> {
        vec![
            Pattern::definition(400, "binary_expression", vec![
                PatternRule::any(), // left operand
                PatternRule::one_of(vec![
                    "+".to_string(), "-".to_string(), "*".to_string(), "/".to_string(),
                    "==".to_string(), "!=".to_string(), "<".to_string(), ">".to_string(),
                ]),
                PatternRule::any(), // right operand
            ])
            .with_category("expression")
            .with_priority(40)
            .with_min_tokens(3)
            .with_description("Binary expression"),
        ]
    }
    
    fn validate(&mut self, tokens: &[Token]) -> bool {
        if tokens.len() < 3 {
            return false;
        }
        
        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        
        // Must contain an operator
        let has_operator = token_strs.iter().any(|t| Self::is_operator(t));
        if !has_operator {
            return false;
        }
        
        self.confidence = 0.6;
        true
    }
    
    fn extract(&mut self, tokens: &[Token]) -> bool {
        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        self.data.tokens = token_strs.clone();
        
        // Find the main operator (simple case: first operator found)
        for (i, token) in token_strs.iter().enumerate() {
            if Self::is_operator(token) {
                self.data.operator = Some(token.clone());
                
                // Collect left operand
                if i > 0 {
                    self.data.left = Some(token_strs[..i].join(" "));
                }
                
                // Collect right operand
                if i + 1 < token_strs.len() {
                    self.data.right = Some(token_strs[i + 1..].join(" "));
                }
                
                break;
            }
        }
        
        self.confidence = 0.7;
        true
    }
    
    fn convert(&mut self) -> Option<String> {
        // For expressions, just join the tokens with proper spacing
        // Most C expressions work in Rust as-is
        let rust_expr = if let (Some(left), Some(op), Some(right)) = 
            (&self.data.left, &self.data.operator, &self.data.right) 
        {
            let rust_op = Self::convert_operator(op);
            format!("{} {} {}", left, rust_op, right)
        } else {
            // Fallback: just join tokens
            self.data.tokens.join(" ")
        };
        
        self.confidence = 0.8;
        Some(rust_expr)
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

impl Build for ExpressionHandler {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("Handler", "ExpressionHandler");
        entry.set_attr("stage", Entry::string(self.stage.as_str()));
        entry.set_attr("confidence", Entry::f64(self.confidence));
        if let Some(ref output) = self.output {
            entry.set_attr("output", Entry::string(output));
        }
        if let Some(ref op) = self.data.operator {
            entry.set_attr("operator", Entry::string(op));
        }
        entry
    }
    
    fn kind(&self) -> &str { "Handler" }
    fn name(&self) -> Option<&str> { Some("ExpressionHandler") }
    fn category(&self) -> Option<&str> { Some("expression") }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    fn make_tokens(strs: &[&str]) -> Vec<Token> {
        strs.iter().map(|s| Token::from_string(*s)).collect()
    }
    
    #[test]
    fn test_simple_addition() {
        let tokens = make_tokens(&["1", "+", "2"]);
        let mut handler = ExpressionHandler::new();
        
        assert!(handler.validate(&tokens));
        assert!(handler.extract(&tokens));
        
        let output = handler.convert();
        assert!(output.is_some());
        let code = output.unwrap();
        assert!(code.contains("+"));
    }
    
    #[test]
    fn test_subtraction() {
        let tokens = make_tokens(&["x", "-", "1"]);
        let mut handler = ExpressionHandler::new();
        
        assert!(handler.validate(&tokens));
        assert!(handler.extract(&tokens));
        assert_eq!(handler.data.operator, Some("-".to_string()));
    }
    
    #[test]
    fn test_comparison() {
        let tokens = make_tokens(&["a", "==", "b"]);
        let mut handler = ExpressionHandler::new();
        
        assert!(handler.validate(&tokens));
        assert!(handler.extract(&tokens));
        
        let output = handler.convert();
        assert!(output.is_some());
        assert!(output.unwrap().contains("=="));
    }
}
