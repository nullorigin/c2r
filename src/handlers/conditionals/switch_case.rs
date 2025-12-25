//! Switch-Case Handler
//!
//! Converts C switch/case statements to Rust match expressions.

use crate::db::web::{Entry, Build};
use crate::db::token::Token;
use crate::db::convert::IdentifierConverter;
use crate::db::pattern::{Pattern, PatternRule};
use crate::handlers::process::{ProcessStage, Processor};

// ============================================================================
// Switch-Case Handler Implementation
// ============================================================================

/// A single case in a switch statement
#[derive(Debug, Clone)]
pub struct CaseData {
    /// Case value (None for default)
    pub value: Option<String>,
    /// Case body tokens
    pub body: Vec<Token>,
    /// Whether this case falls through
    pub falls_through: bool,
}

/// Extracted switch information
#[derive(Debug, Clone, Default)]
pub struct SwitchData {
    /// Switch expression
    pub expression: String,
    /// Cases
    pub cases: Vec<CaseData>,
    /// Default case body (if any)
    pub default_body: Option<Vec<Token>>,
}

/// Handler for C switch/case statements
#[derive(Debug)]
pub struct SwitchCaseHandler {
    stage: ProcessStage,
    confidence: f64,
    error: Option<String>,
    output: Option<String>,
    data: SwitchData,
    id_converter: IdentifierConverter,
}

impl SwitchCaseHandler {
    pub fn new() -> Self {
        Self {
            stage: ProcessStage::Pending,
            confidence: 0.0,
            error: None,
            output: None,
            data: SwitchData::default(),
            id_converter: IdentifierConverter::new(),
        }
    }
}

impl Default for SwitchCaseHandler {
    fn default() -> Self {
        Self::new()
    }
}

impl Processor for SwitchCaseHandler {
    fn name(&self) -> &str {
        "SwitchCaseHandler"
    }
    
    fn supported_patterns(&self) -> &[&str] {
        &["switch", "case", "switch_case"]
    }
    
    fn patterns(&self) -> Vec<Pattern> {
        vec![
            // switch (expr) { case ... }
            Pattern::definition(410, "switch_statement", vec![
                PatternRule::exact("switch"),
                PatternRule::exact("("),
                PatternRule::any().optional().with_can_repeat(true),
                PatternRule::exact(")"),
                PatternRule::exact("{"),
            ])
            .with_category("conditional")
            .with_priority(830)
            .with_min_tokens(6)
            .with_description("C switch statement"),
        ]
    }
    
    fn validate(&mut self, tokens: &[Token]) -> bool {
        if tokens.len() < 6 {
            return false;
        }
        
        let first = tokens[0].to_string();
        if first != "switch" {
            return false;
        }
        
        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        
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
        
        // Find expression in parentheses
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
        
        // Extract switch expression
        let expr_tokens = &token_strs[paren_pos.unwrap() + 1..close_paren];
        self.data.expression = expr_tokens.join(" ");
        
        // Find switch body
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
                // Extract cases from body
                self.extract_cases(&token_strs[bp + 1..cb], &tokens[bp + 1..cb]);
            }
        }
        
        self.confidence = 0.85;
        true
    }
    
    fn convert(&mut self) -> Option<String> {
        let expr = self.convert_expression(&self.data.expression.clone());
        
        let mut arms: Vec<String> = Vec::new();
        
        for case in &self.data.cases.clone() {
            if let Some(ref value) = case.value {
                let body = self.convert_case_body(&case.body.clone());
                arms.push(format!("    {} => {{\n{}\n    }}", value, body));
            }
        }
        
        // Add default case
        if let Some(ref default_body) = self.data.default_body.clone() {
            let body = self.convert_case_body(default_body);
            arms.push(format!("    _ => {{\n{}\n    }}", body));
        } else {
            // Add empty default if no cases cover all possibilities
            arms.push("    _ => {}".to_string());
        }
        
        let result = format!("match {} {{\n{}\n}}", expr, arms.join(",\n"));
        
        self.confidence = 0.85;
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

impl SwitchCaseHandler {
    /// Extract cases from switch body
    fn extract_cases(&mut self, token_strs: &[String], tokens: &[Token]) {
        let mut i = 0;
        
        while i < token_strs.len() {
            if token_strs[i] == "case" {
                // Find case value (up to :)
                let colon_pos = token_strs[i..].iter().position(|t| t == ":");
                if let Some(cp) = colon_pos {
                    let value = token_strs[i + 1..i + cp].join(" ");
                    
                    // Find case body (up to next case, default, or break)
                    let start = i + cp + 1;
                    let mut end = start;
                    let mut has_break = false;
                    
                    for j in start..token_strs.len() {
                        if token_strs[j] == "case" || token_strs[j] == "default" {
                            end = j;
                            break;
                        }
                        if token_strs[j] == "break" {
                            has_break = true;
                            end = j;
                            break;
                        }
                        end = j + 1;
                    }
                    
                    let body = tokens[start..end.min(tokens.len())].to_vec();
                    
                    self.data.cases.push(CaseData {
                        value: Some(value),
                        body,
                        falls_through: !has_break,
                    });
                    
                    i = if has_break { end + 2 } else { end }; // Skip break and ;
                    continue;
                }
            } else if token_strs[i] == "default" {
                // Find default body
                let colon_pos = token_strs[i..].iter().position(|t| t == ":");
                if let Some(cp) = colon_pos {
                    let start = i + cp + 1;
                    let mut end = start;
                    
                    for j in start..token_strs.len() {
                        if token_strs[j] == "break" {
                            end = j;
                            break;
                        }
                        end = j + 1;
                    }
                    
                    self.data.default_body = Some(tokens[start..end.min(tokens.len())].to_vec());
                    break;
                }
            }
            
            i += 1;
        }
    }
    
    /// Convert switch expression
    fn convert_expression(&mut self, expr: &str) -> String {
        self.id_converter.convert(expr)
            .unwrap_or_else(|| expr.to_string())
    }
    
    /// Convert case body
    fn convert_case_body(&mut self, tokens: &[Token]) -> String {
        if tokens.is_empty() {
            return "        todo!()".to_string();
        }
        
        let token_strs: Vec<String> = tokens.iter()
            .map(|t| t.to_string())
            .filter(|t| t != "break")
            .collect();
        
        let body = token_strs.join(" ")
            .replace(" ;", ";")
            .replace("( ", "(")
            .replace(" )", ")");
        
        format!("        {}", body)
    }
}

impl Build for SwitchCaseHandler {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("Handler", "SwitchCaseHandler");
        entry.set_attr("stage", Entry::string(self.stage.as_str()));
        entry.set_attr("confidence", Entry::f64(self.confidence));
        entry.set_attr("expression", Entry::string(&self.data.expression));
        entry.set_attr("case_count", Entry::usize(self.data.cases.len()));
        if let Some(ref output) = self.output {
            entry.set_attr("output", Entry::string(output));
        }
        entry
    }
    
    fn kind(&self) -> &str { "Handler" }
    fn name(&self) -> Option<&str> { Some("SwitchCaseHandler") }
    fn category(&self) -> Option<&str> { Some("conditional") }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    fn make_tokens(strs: &[&str]) -> Vec<Token> {
        strs.iter().map(|s| Token::from_string(*s)).collect()
    }
    
    #[test]
    fn test_simple_switch() {
        let tokens = make_tokens(&[
            "switch", "(", "x", ")", "{",
            "case", "1", ":", "return", "a", ";", "break", ";",
            "case", "2", ":", "return", "b", ";", "break", ";",
            "default", ":", "return", "c", ";",
            "}"
        ]);
        let mut handler = SwitchCaseHandler::new();
        
        assert!(handler.validate(&tokens));
        assert!(handler.extract(&tokens));
        assert_eq!(handler.data.expression, "x");
        assert_eq!(handler.data.cases.len(), 2);
    }
    
    #[test]
    fn test_switch_conversion() {
        let tokens = make_tokens(&[
            "switch", "(", "cmd", ")", "{",
            "case", "0", ":", "start", "(", ")", ";", "break", ";",
            "case", "1", ":", "stop", "(", ")", ";", "break", ";",
            "}"
        ]);
        let mut handler = SwitchCaseHandler::new();
        
        assert!(handler.validate(&tokens));
        assert!(handler.extract(&tokens));
        
        let result = handler.convert();
        assert!(result.is_some());
        let code = result.unwrap();
        assert!(code.contains("match cmd"));
        assert!(code.contains("0 =>"));
        assert!(code.contains("1 =>"));
    }
}
