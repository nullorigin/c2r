//! Conditional Handlers
//!
//! Converts C conditional constructs (if/else, switch/case) to Rust equivalents.

use crate::db::pattern::{Pattern, PatternRule};
use crate::db::token::Token;
use crate::db::web::{Build, Entry};
use crate::handlers::expressions::ComparisonConverter;
use crate::handlers::process::{ProcessStage, Processor};
use crate::system::system;

// ============================================================================
// Shared Helpers
// ============================================================================

/// Find matching closing delimiter (paren or brace) starting from open position
fn find_matching_close(tokens: &[String], open_pos: usize, open: &str, close: &str) -> Option<usize> {
    let mut depth = 0;
    for (i, t) in tokens.iter().enumerate().skip(open_pos) {
        if t == open {
            depth += 1;
        } else if t == close {
            depth -= 1;
            if depth == 0 {
                return Some(i);
            }
        }
    }
    None
}

/// Split tokens into statements, handling nested control structures
fn split_statements(tokens: &[Token], filter_breaks: bool) -> Vec<Vec<Token>> {
    let filtered: Vec<Token> = if filter_breaks {
        tokens.iter().filter(|t| t.to_string() != "break").cloned().collect()
    } else {
        tokens.to_vec()
    };

    let mut statements = Vec::new();
    let mut current = Vec::new();
    let mut brace_depth = 0;
    let mut in_control = false;

    for token in filtered {
        let s = token.to_string();

        if current.is_empty() {
            in_control = matches!(s.as_str(), "switch" | "if" | "for" | "while" | "do");
        }

        match s.as_str() {
            "{" => brace_depth += 1,
            "}" => brace_depth -= 1,
            _ => {}
        }

        current.push(token);

        if (s == ";" && brace_depth == 0) || (s == "}" && brace_depth == 0 && in_control) {
            if !current.is_empty() {
                statements.push(std::mem::take(&mut current));
            }
            in_control = false;
        }
    }

    if !current.is_empty() {
        statements.push(current);
    }

    statements
}

// ============================================================================
// If-Else Handler
// ============================================================================

/// Extracted if-else information
#[derive(Debug, Clone, Default)]
pub struct IfElseData {
    pub condition: String,
    pub if_body: Vec<Token>,
    pub else_ifs: Vec<(String, Vec<Token>)>,
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

    fn convert_condition(&mut self, cond: &str) -> String {
        // Convert arrow operators first
        let converted = Self::convert_arrow_operator(cond);
        let tokens: Vec<&str> = converted.split_whitespace().collect();
        self.cmp_converter.convert_logical(&tokens)
    }

    /// Convert C arrow operator (ptr->field or ptr -> field) to Rust (*ptr).field
    fn convert_arrow_operator(expr: &str) -> String {
        let mut result = expr.to_string();
        
        // Handle spaced arrow: "ptr -> field" -> "(*ptr).field"
        while let Some(arrow_pos) = result.find(" -> ") {
            let before = &result[..arrow_pos];
            let after = &result[arrow_pos + 4..];
            
            let ptr_name = before.split_whitespace().last().unwrap_or("");
            let prefix = if before.len() > ptr_name.len() {
                &before[..before.len() - ptr_name.len()]
            } else {
                ""
            };
            
            let field_end = after.find(|c: char| !c.is_alphanumeric() && c != '_').unwrap_or(after.len());
            let field = &after[..field_end];
            let suffix = &after[field_end..];
            
            result = format!("{}(*{}).{}{}", prefix, ptr_name, field, suffix);
        }
        
        // Handle non-spaced arrow: "ptr->field" -> "(*ptr).field"
        while let Some(arrow_pos) = result.find("->") {
            if arrow_pos > 0 && result.chars().nth(arrow_pos - 1) == Some(')') {
                break;
            }
            
            let before = &result[..arrow_pos];
            let after = &result[arrow_pos + 2..];
            
            let ptr_start = before.rfind(|c: char| !c.is_alphanumeric() && c != '_').map(|p| p + 1).unwrap_or(0);
            let ptr_name = &before[ptr_start..];
            let prefix = &before[..ptr_start];
            
            let field_end = after.find(|c: char| !c.is_alphanumeric() && c != '_').unwrap_or(after.len());
            let field = &after[..field_end];
            let suffix = &after[field_end..];
            
            result = format!("{}(*{}).{}{}", prefix, ptr_name, field, suffix);
        }
        
        result
    }

    fn convert_body(&mut self, tokens: &[Token]) -> String {
        system().indent_push();
        let indent = system().indent_str();

        if tokens.is_empty() {
            system().indent_pop();
            return format!("{}todo!()", indent);
        }

        let statements: Vec<String> = split_statements(tokens, false)
            .iter()
            .map(|stmt| self.route_statement(stmt, "IfElseHandler"))
            .filter(|s| !s.is_empty())
            .collect();

        system().indent_pop();

        if statements.is_empty() {
            return format!("{}todo!()", indent);
        }

        statements
            .iter()
            .map(|s| if s.contains('\n') { s.clone() } else { format!("{}{}", indent, s) })
            .collect::<Vec<_>>()
            .join("\n")
    }

    fn extract_else(&mut self, token_strs: &[String], tokens: &[Token], else_pos: usize) {
        if else_pos + 1 < token_strs.len() && token_strs[else_pos + 1] == "if" {
            // else if
            if let Some(pp) = token_strs[else_pos..].iter().position(|t| t == "(") {
                let pp = else_pos + pp;
                if let Some(cp) = find_matching_close(token_strs, pp, "(", ")") {
                    let cond = token_strs[pp + 1..cp].join(" ");
                    if let Some(bp) = token_strs[cp..].iter().position(|t| t == "{") {
                        let bp = cp + bp;
                        if let Some(cb) = find_matching_close(token_strs, bp, "{", "}") {
                            self.data.else_ifs.push((cond, tokens[bp + 1..cb].to_vec()));
                            if cb + 1 < token_strs.len() && token_strs[cb + 1] == "else" {
                                self.extract_else(token_strs, tokens, cb + 1);
                            }
                        }
                    }
                }
            }
        } else {
            // plain else
            if let Some(bp) = token_strs[else_pos..].iter().position(|t| t == "{") {
                let bp = else_pos + bp;
                if let Some(cb) = find_matching_close(token_strs, bp, "{", "}") {
                    self.data.else_body = Some(tokens[bp + 1..cb].to_vec());
                }
            }
        }
    }
}

impl Default for IfElseHandler {
    fn default() -> Self { Self::new() }
}

impl Processor for IfElseHandler {
    fn supported_patterns(&self) -> &[&str] {
        &["validate_if_statement", "extract_if_statement", "validate_if_else_statement", "extract_if_else_statement"]
    }

    fn patterns(&self) -> Vec<(Pattern, Pattern)> {
        vec![
            (
                Pattern::definition(400, "validate_if_statement", vec![
                    PatternRule::exact("if"),
                    PatternRule::exact("("),
                    PatternRule::any().optional().repeat(-1),
                    PatternRule::exact(")"),
                    PatternRule::exact("{"),
                ])
                .with_category("conditional")
                .with_priority(840)
                .with_min_tokens(5)
                .with_description("C if statement"),
                Pattern::definition(400, "extract_if_statement", vec![
                    PatternRule::exact("if"),
                    PatternRule::exact("("),
                    PatternRule::any().optional().repeat(-1),
                    PatternRule::exact(")"),
                    PatternRule::exact("{"),
                ])
                .with_category("conditional")
                .with_priority(840)
                .with_min_tokens(5),
            ),
            (
                Pattern::definition(401, "validate_if_else_statement", vec![
                    PatternRule::exact("if"),
                    PatternRule::exact("("),
                    PatternRule::any().optional().repeat(-1),
                    PatternRule::exact(")"),
                    PatternRule::exact("{"),
                    PatternRule::any().optional().repeat(-1),
                    PatternRule::exact("}"),
                    PatternRule::exact("else"),
                ])
                .with_category("conditional")
                .with_priority(845)
                .with_min_tokens(8)
                .with_description("C if-else statement"),
                Pattern::definition(401, "extract_if_else_statement", vec![
                    PatternRule::exact("if"),
                    PatternRule::exact("("),
                    PatternRule::any().optional().repeat(-1),
                    PatternRule::exact(")"),
                    PatternRule::exact("{"),
                    PatternRule::any().optional().repeat(-1),
                    PatternRule::exact("}"),
                    PatternRule::exact("else"),
                ])
                .with_category("conditional")
                .with_priority(845)
                .with_min_tokens(8),
            ),
        ]
    }

    fn validate(&mut self, tokens: &[Token]) -> bool {
        if tokens.len() < 5 { return false; }
        let first = tokens[0].to_string();
        if first != "if" { return false; }
        if !tokens.iter().any(|t| t.to_string() == "(") { return false; }

        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        self.confidence = self.patterns().iter()
            .filter_map(|(p, _)| p.matches_tokens(&token_strs))
            .fold(0.8, f64::max);
        true
    }

    fn extract(&mut self, tokens: &[Token]) -> bool {
        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        let paren_pos = match token_strs.iter().position(|t| t == "(") {
            Some(p) => p,
            None => return false,
        };

        let close_paren = match find_matching_close(&token_strs, paren_pos, "(", ")") {
            Some(cp) => cp,
            None => return false,
        };

        self.data.condition = token_strs[paren_pos + 1..close_paren].join(" ");

        // Check if there's a brace after the condition
        if let Some(bp) = token_strs[close_paren..].iter().position(|t| t == "{") {
            let bp = close_paren + bp;
            if let Some(cb) = find_matching_close(&token_strs, bp, "{", "}") {
                self.data.if_body = tokens[bp + 1..cb].to_vec();
                if cb + 1 < token_strs.len() && token_strs[cb + 1] == "else" {
                    self.extract_else(&token_strs, tokens, cb + 1);
                }
            }
        } else {
            // No brace - single-line if statement: if (cond) statement;
            // Body is everything from close_paren+1 to semicolon or end
            let body_start = close_paren + 1;
            if body_start < token_strs.len() {
                // Find the end of this single statement
                let mut body_end = body_start;
                let mut brace_depth = 0;
                for i in body_start..token_strs.len() {
                    match token_strs[i].as_str() {
                        "{" => brace_depth += 1,
                        "}" => {
                            if brace_depth > 0 {
                                brace_depth -= 1;
                            }
                        }
                        ";" if brace_depth == 0 => {
                            body_end = i + 1; // Include the semicolon
                            break;
                        }
                        _ => {}
                    }
                    body_end = i + 1;
                }
                self.data.if_body = tokens[body_start..body_end].to_vec();
            }
        }

        self.confidence = 0.85;
        true
    }

    fn convert(&mut self) -> Option<String> {
        let base_indent = system().indent_str();
        let cond = self.convert_condition(&self.data.condition.clone());
        let if_body = self.convert_body(&self.data.if_body.clone());

        let mut result = format!("{}if {} {{\n{}\n{}}}", base_indent, cond, if_body, base_indent);

        for (else_cond, else_body) in &self.data.else_ifs.clone() {
            let conv_cond = self.convert_condition(else_cond);
            let conv_body = self.convert_body(else_body);
            result.push_str(&format!(" else if {} {{\n{}\n{}}}", conv_cond, conv_body, base_indent));
        }

        if let Some(ref else_body) = self.data.else_body.clone() {
            let conv_body = self.convert_body(else_body);
            result.push_str(&format!(" else {{\n{}\n{}}}", conv_body, base_indent));
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

// ============================================================================
// Switch-Case Handler
// ============================================================================

/// A single case in a switch statement
#[derive(Debug, Clone)]
pub struct CaseData {
    pub value: Option<String>,
    pub body: Vec<Token>,
    pub falls_through: bool,
}

/// Extracted switch information
#[derive(Debug, Clone, Default)]
pub struct SwitchData {
    pub expression: String,
    pub cases: Vec<CaseData>,
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
}

impl SwitchCaseHandler {
    pub fn new() -> Self {
        Self {
            stage: ProcessStage::Pending,
            confidence: 0.0,
            error: None,
            output: None,
            data: SwitchData::default(),
        }
    }

    fn extract_cases(&mut self, token_strs: &[String], tokens: &[Token]) {
        let len = token_strs.len();
        let mut i = 0;

        while i < len {
            let is_case = token_strs[i] == "case";
            let is_default = token_strs[i] == "default";

            if !is_case && !is_default {
                i += 1;
                continue;
            }

            let Some(cp) = token_strs[i..].iter().position(|t| t == ":") else {
                i += 1;
                continue;
            };
            let colon_pos = i + cp;

            let value = if is_case {
                Some(token_strs[i + 1..colon_pos].join(" "))
            } else {
                None
            };

            let start = colon_pos + 1;
            let mut end = len;
            let mut has_break = false;
            let mut brace_depth = 0;

            for j in start..len {
                match token_strs[j].as_str() {
                    "{" => brace_depth += 1,
                    "}" => brace_depth -= 1,
                    "case" | "default" if brace_depth == 0 => { end = j; break; }
                    "break" if brace_depth == 0 => { has_break = true; end = j; break; }
                    _ => {}
                }
            }

            let body = if start < end.min(tokens.len()) {
                tokens[start..end.min(tokens.len())].to_vec()
            } else {
                Vec::new()
            };

            if is_default {
                self.data.default_body = Some(body);
            } else {
                self.data.cases.push(CaseData { value, body, falls_through: !has_break });
            }

            i = if has_break {
                if end + 1 < len && token_strs.get(end + 1).map(|s| s == ";").unwrap_or(false) {
                    end + 2
                } else {
                    end + 1
                }
            } else {
                end
            };
        }
    }

    fn convert_case_body(&mut self, tokens: &[Token]) -> String {
        if tokens.is_empty() {
            return "{}".to_string();
        }

        let statements: Vec<String> = split_statements(tokens, true)
            .iter()
            .map(|stmt| self.route_statement(stmt, "SwitchCaseHandler"))
            .filter(|s| !s.is_empty())
            .collect();

        if statements.is_empty() { "{}".to_string() } else { statements.join("\n") }
    }
}

impl Default for SwitchCaseHandler {
    fn default() -> Self { Self::new() }
}

impl Processor for SwitchCaseHandler {
    fn supported_patterns(&self) -> &[&str] {
        &["validate_switch_statement", "extract_switch_statement", "validate_case", "extract_case"]
    }

    fn patterns(&self) -> Vec<(Pattern, Pattern)> {
        vec![(
            Pattern::definition(410, "validate_switch_statement", vec![
                PatternRule::exact("switch"),
                PatternRule::exact("("),
                PatternRule::any().optional().repeat(-1),
                PatternRule::exact(")"),
                PatternRule::exact("{"),
            ])
            .with_category("conditional")
            .with_priority(830)
            .with_min_tokens(6)
            .with_description("C switch statement"),
            Pattern::definition(410, "extract_switch_statement", vec![
                PatternRule::exact("switch"),
                PatternRule::exact("("),
                PatternRule::any().optional().repeat(-1),
                PatternRule::exact(")"),
                PatternRule::exact("{"),
            ])
            .with_category("conditional")
            .with_priority(830)
            .with_min_tokens(6),
        )]
    }

    fn validate(&mut self, tokens: &[Token]) -> bool {
        if tokens.len() < 6 { return false; }
        if tokens[0].to_string() != "switch" { return false; }

        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        self.confidence = self.patterns().iter()
            .filter_map(|(p, _)| p.matches_tokens(&token_strs))
            .fold(0.8, f64::max);
        true
    }

    fn extract(&mut self, tokens: &[Token]) -> bool {
        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        let paren_pos = match token_strs.iter().position(|t| t == "(") {
            Some(p) => p,
            None => return false,
        };

        let close_paren = match find_matching_close(&token_strs, paren_pos, "(", ")") {
            Some(cp) => cp,
            None => return false,
        };

        self.data.expression = token_strs[paren_pos + 1..close_paren].join(" ");

        if let Some(bp) = token_strs[close_paren..].iter().position(|t| t == "{") {
            let bp = close_paren + bp;
            if let Some(cb) = find_matching_close(&token_strs, bp, "{", "}") {
                self.extract_cases(&token_strs[bp + 1..cb].to_vec(), &tokens[bp + 1..cb]);
            }
        }

        self.confidence = 0.85;
        true
    }

    fn convert(&mut self) -> Option<String> {
        let expr = system().lookup_identifier(&self.data.expression)
            .unwrap_or_else(|| self.data.expression.clone());

        let base_indent = system().indent_str();
        let arm_indent = format!("{}    ", base_indent);
        let body_indent = format!("{}        ", base_indent);

        let mut arms: Vec<String> = Vec::new();

        for case in &self.data.cases.clone() {
            if let Some(ref value) = case.value {
                let body = self.convert_case_body(&case.body.clone());
                let indented = body.lines()
                    .map(|l| if l.trim().is_empty() { String::new() } else { format!("{}{}", body_indent, l.trim()) })
                    .collect::<Vec<_>>()
                    .join("\n");
                let converted_value = system().lookup_enum_variant(value).unwrap_or_else(|| value.clone());
                arms.push(format!("{}{} => {{\n{}\n{}}}", arm_indent, converted_value, indented, arm_indent));
            }
        }

        if let Some(ref default_body) = self.data.default_body.clone() {
            let body = self.convert_case_body(default_body);
            let indented = body.lines()
                .map(|l| if l.trim().is_empty() { String::new() } else { format!("{}{}", body_indent, l.trim()) })
                .collect::<Vec<_>>()
                .join("\n");
            arms.push(format!("{}_ => {{\n{}\n{}}}", arm_indent, indented, arm_indent));
        } else {
            arms.push(format!("{}_ => {{}}", arm_indent));
        }

        self.confidence = 0.85;
        Some(format!("{}match {} {{\n{}\n{}}}", base_indent, expr, arms.join(",\n"), base_indent))
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

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    fn make_tokens(strs: &[&str]) -> Vec<Token> {
        strs.iter().map(|s| Token::from_string(*s)).collect()
    }

    #[test]
    fn test_simple_if() {
        let tokens = make_tokens(&["if", "(", "x", ">", "0", ")", "{", "return", "1", ";", "}"]);
        let mut handler = IfElseHandler::new();
        assert!(handler.validate(&tokens));
        assert!(handler.extract(&tokens));
        assert_eq!(handler.data.condition, "x > 0");
    }

    #[test]
    fn test_if_else() {
        let tokens = make_tokens(&[
            "if", "(", "x", ">", "0", ")", "{", "return", "1", ";", "}", "else", "{", "return", "0", ";", "}",
        ]);
        let mut handler = IfElseHandler::new();
        assert!(handler.validate(&tokens));
        assert!(handler.extract(&tokens));
        assert!(handler.data.else_body.is_some());
    }

    #[test]
    fn test_simple_switch() {
        let tokens = make_tokens(&[
            "switch", "(", "x", ")", "{", "case", "1", ":", "return", "a", ";", "break", ";",
            "case", "2", ":", "return", "b", ";", "break", ";", "default", ":", "return", "c", ";", "}",
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
            "switch", "(", "cmd", ")", "{", "case", "0", ":", "start", "(", ")", ";", "break", ";",
            "case", "1", ":", "stop", "(", ")", ";", "break", ";", "}",
        ]);
        let mut handler = SwitchCaseHandler::new();
        assert!(handler.validate(&tokens));
        assert!(handler.extract(&tokens));
        let result = handler.convert();
        assert!(result.is_some());
        let code = result.unwrap();
        assert!(code.contains("match cmd"));
    }
}
