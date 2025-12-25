//! If-Else Handler
//!
//! Converts C if/else statements to Rust if/else expressions.

use crate::db::web::{Entry, Build};
use crate::db::token::Token;
use crate::db::pattern::{Pattern, PatternRule};
use crate::handlers::process::{ProcessStage, Processor};
use crate::handlers::expressions::ComparisonConverter;

// ============================================================================
// If-Else Handler Implementation
// ============================================================================

/// Extracted if-else information
#[derive(Debug, Clone, Default)]
pub struct IfElseData {
    /// Main condition
    pub condition: String,
    /// If body tokens
    pub if_body: Vec<Token>,
    /// Else-if conditions and bodies
    pub else_ifs: Vec<(String, Vec<Token>)>,
    /// Else body tokens (if any)
    pub else_body: Option<Vec<Token>>,
}

/// Handler for C if/else statements
#[derive(Debug)]
pub struct IfElseHandler {
    stage: ProcessStage,
    confidence: f64,
    error: Option<String>,
    output: Option<String>,
    data: IfElseData,
    cmp_converter: ComparisonConverter,
}

impl IfElseHandler {
    pub fn new() -> Self {
        Self {
            stage: ProcessStage::Pending,
            confidence: 0.0,
            error: None,
            output: None,
            data: IfElseData::default(),
            cmp_converter: ComparisonConverter::new(),
        }
    }
}

impl Default for IfElseHandler {
    fn default() -> Self {
        Self::new()
    }
}

impl Processor for IfElseHandler {
    fn name(&self) -> &str {
        "IfElseHandler"
    }
    
    fn supported_patterns(&self) -> &[&str] {
        &["if", "if_else", "conditional"]
    }
    
    fn patterns(&self) -> Vec<Pattern> {
        vec![
            // if (cond) { body }
            Pattern::definition(400, "if_statement", vec![
                PatternRule::exact("if"),
                PatternRule::exact("("),
                PatternRule::any().optional().with_can_repeat(true),
                PatternRule::exact(")"),
                PatternRule::exact("{"),
            ])
            .with_category("conditional")
            .with_priority(840)
            .with_min_tokens(5)
            .with_description("C if statement"),
            
            // if (cond) { } else { }
            Pattern::definition(401, "if_else_statement", vec![
                PatternRule::exact("if"),
                PatternRule::exact("("),
                PatternRule::any().optional().with_can_repeat(true),
                PatternRule::exact(")"),
                PatternRule::exact("{"),
                PatternRule::any().optional().with_can_repeat(true),
                PatternRule::exact("}"),
                PatternRule::exact("else"),
            ])
            .with_category("conditional")
            .with_priority(845)
            .with_min_tokens(8)
            .with_description("C if-else statement"),
        ]
    }
    
    fn validate(&mut self, tokens: &[Token]) -> bool {
        if tokens.len() < 5 {
            return false;
        }
        
        let first = tokens[0].to_string();
        if first != "if" {
            return false;
        }
        
        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        if !token_strs.contains(&"(".to_string()) {
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
        
        self.confidence = if best_confidence > 0.0 { best_confidence } else { 0.8 };
        true
    }
    
    fn extract(&mut self, tokens: &[Token]) -> bool {
        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        
        // Manual extraction
        let paren_pos = token_strs.iter().position(|t| t == "(");
        if paren_pos.is_none() {
            return false;
        }
        
        let mut paren_depth = 0;
        let mut close_paren = None;
        for (i, t) in token_strs.iter().enumerate().skip(paren_pos.unwrap()) {
            match t.as_str() {
                "(" => paren_depth += 1,
                ")" => {
                    paren_depth -= 1;
                    if paren_depth == 0 {
                        close_paren = Some(i);
                        break;
                    }
                }
                _ => {}
            }
        }
        
        let close_paren = match close_paren {
            Some(cp) => cp,
            None => return false,
        };
        
        let cond_tokens = &token_strs[paren_pos.unwrap() + 1..close_paren];
        self.data.condition = cond_tokens.join(" ");
        
        // Find if body
        let brace_pos = token_strs[close_paren..].iter().position(|t| t == "{");
        if let Some(bp) = brace_pos {
            let bp = close_paren + bp;
            let mut brace_depth = 0;
            let mut close_brace = None;
            for (i, t) in token_strs.iter().enumerate().skip(bp) {
                match t.as_str() {
                    "{" => brace_depth += 1,
                    "}" => {
                        brace_depth -= 1;
                        if brace_depth == 0 {
                            close_brace = Some(i);
                            break;
                        }
                    }
                    _ => {}
                }
            }
            
            if let Some(cb) = close_brace {
                self.data.if_body = tokens[bp + 1..cb].to_vec();
                
                // Check for else
                if cb + 1 < token_strs.len() && token_strs[cb + 1] == "else" {
                    self.extract_else(&token_strs, tokens, cb + 1);
                }
            }
        }
        
        self.confidence = 0.85;
        true
    }
    
    fn convert(&mut self) -> Option<String> {
        let cond = self.convert_condition(&self.data.condition.clone());
        let if_body = self.convert_body(&self.data.if_body.clone());
        
        let mut result = format!("if {} {{\n{}\n}}", cond, if_body);
        
        // Add else-ifs
        for (else_cond, else_body) in &self.data.else_ifs.clone() {
            let conv_cond = self.convert_condition(else_cond);
            let conv_body = self.convert_body(else_body);
            result.push_str(&format!(" else if {} {{\n{}\n}}", conv_cond, conv_body));
        }
        
        // Add else
        if let Some(ref else_body) = self.data.else_body.clone() {
            let conv_body = self.convert_body(else_body);
            result.push_str(&format!(" else {{\n{}\n}}", conv_body));
        }
        
        self.confidence = 0.9;
        Some(result)
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

impl IfElseHandler {
    /// Extract else/else-if parts
    fn extract_else(&mut self, token_strs: &[String], tokens: &[Token], else_pos: usize) {
        // Check if else if
        if else_pos + 1 < token_strs.len() && token_strs[else_pos + 1] == "if" {
            // else if - extract condition and body
            let paren_pos = token_strs[else_pos..].iter().position(|t| t == "(");
            if let Some(pp) = paren_pos {
                let pp = else_pos + pp;
                let mut paren_depth = 0;
                let mut close_paren = None;
                for (i, t) in token_strs.iter().enumerate().skip(pp) {
                    match t.as_str() {
                        "(" => paren_depth += 1,
                        ")" => {
                            paren_depth -= 1;
                            if paren_depth == 0 {
                                close_paren = Some(i);
                                break;
                            }
                        }
                        _ => {}
                    }
                }
                
                if let Some(cp) = close_paren {
                    let cond = token_strs[pp + 1..cp].join(" ");
                    
                    // Find body
                    let brace_pos = token_strs[cp..].iter().position(|t| t == "{");
                    if let Some(bp) = brace_pos {
                        let bp = cp + bp;
                        let mut brace_depth = 0;
                        let mut close_brace = None;
                        for (i, t) in token_strs.iter().enumerate().skip(bp) {
                            match t.as_str() {
                                "{" => brace_depth += 1,
                                "}" => {
                                    brace_depth -= 1;
                                    if brace_depth == 0 {
                                        close_brace = Some(i);
                                        break;
                                    }
                                }
                                _ => {}
                            }
                        }
                        
                        if let Some(cb) = close_brace {
                            let body = tokens[bp + 1..cb].to_vec();
                            self.data.else_ifs.push((cond, body));
                            
                            // Check for more else
                            if cb + 1 < token_strs.len() && token_strs[cb + 1] == "else" {
                                self.extract_else(token_strs, tokens, cb + 1);
                            }
                        }
                    }
                }
            }
        } else {
            // Plain else - extract body
            let brace_pos = token_strs[else_pos..].iter().position(|t| t == "{");
            if let Some(bp) = brace_pos {
                let bp = else_pos + bp;
                let mut brace_depth = 0;
                let mut close_brace = None;
                for (i, t) in token_strs.iter().enumerate().skip(bp) {
                    match t.as_str() {
                        "{" => brace_depth += 1,
                        "}" => {
                            brace_depth -= 1;
                            if brace_depth == 0 {
                                close_brace = Some(i);
                                break;
                            }
                        }
                        _ => {}
                    }
                }
                
                if let Some(cb) = close_brace {
                    self.data.else_body = Some(tokens[bp + 1..cb].to_vec());
                }
            }
        }
    }
    
    /// Convert condition using ComparisonConverter
    fn convert_condition(&mut self, cond: &str) -> String {
        let tokens: Vec<&str> = cond.split_whitespace().collect();
        self.cmp_converter.convert_logical(&tokens)
    }
    
    /// Convert body tokens
    fn convert_body(&mut self, tokens: &[Token]) -> String {
        if tokens.is_empty() {
            return "    todo!()".to_string();
        }
        
        let token_strs: Vec<String> = tokens.iter()
            .map(|t| t.to_string())
            .collect();
        
        let body = token_strs.join(" ")
            .replace(" ;", ";")
            .replace("( ", "(")
            .replace(" )", ")");
        
        format!("    {}", body)
    }
}

impl Build for IfElseHandler {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("Handler", "IfElseHandler");
        entry.set_attr("stage", Entry::string(self.stage.as_str()));
        entry.set_attr("confidence", Entry::f64(self.confidence));
        entry.set_attr("condition", Entry::string(&self.data.condition));
        if let Some(ref output) = self.output {
            entry.set_attr("output", Entry::string(output));
        }
        entry
    }
    
    fn kind(&self) -> &str { "Handler" }
    fn name(&self) -> Option<&str> { Some("IfElseHandler") }
    fn category(&self) -> Option<&str> { Some("conditional") }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    fn make_tokens(strs: &[&str]) -> Vec<Token> {
        strs.iter().map(|s| Token::from_string(*s)).collect()
    }
    
    #[test]
    fn test_simple_if() {
        let tokens = make_tokens(&[
            "if", "(", "x", ">", "0", ")", "{", "return", "1", ";", "}"
        ]);
        let mut handler = IfElseHandler::new();
        
        assert!(handler.validate(&tokens));
        assert!(handler.extract(&tokens));
        assert_eq!(handler.data.condition, "x > 0");
    }
    
    #[test]
    fn test_if_else() {
        let tokens = make_tokens(&[
            "if", "(", "x", ">", "0", ")", "{", "return", "1", ";", "}",
            "else", "{", "return", "0", ";", "}"
        ]);
        let mut handler = IfElseHandler::new();
        
        assert!(handler.validate(&tokens));
        assert!(handler.extract(&tokens));
        assert!(handler.data.else_body.is_some());
    }
    
    #[test]
    fn test_if_conversion() {
        let tokens = make_tokens(&[
            "if", "(", "running", ")", "{", "stop", "(", ")", ";", "}"
        ]);
        let mut handler = IfElseHandler::new();
        
        assert!(handler.validate(&tokens));
        assert!(handler.extract(&tokens));
        
        let result = handler.convert();
        assert!(result.is_some());
        let code = result.unwrap();
        assert!(code.contains("if running"));
    }
}
