//! Expression Handler
//!
//! Converts C expressions to Rust expressions.

pub mod comparisons;
pub mod string_format;

pub use comparisons::ComparisonConverter;
pub use string_format::StringFormatHandler;

use crate::db::convert::TypeConverter;
use crate::db::pattern::{Pattern, PatternRule};
use crate::db::token::Token;
use crate::db::web::{Build, Entry};
use crate::handlers::process::{ProcessStage, Processor};
use crate::system;

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
        matches!(
            token,
            "+" | "-"
                | "*"
                | "/"
                | "%"
                | "=="
                | "!="
                | "<"
                | ">"
                | "<="
                | ">="
                | "&&"
                | "||"
                | "&"
                | "|"
                | "^"
                | "<<"
                | ">>"
                | "+="
                | "-="
                | "*="
                | "/="
        )
    }

    /// Convert C operator to Rust (most are the same)
    fn convert_operator(op: &str) -> &str {
        match op {
            // Most operators are the same in Rust
            _ => op,
        }
    }

    /// Check if tokens look like a C-style type cast
    fn is_type_cast(tokens: &[&str]) -> bool {
        if tokens.is_empty() {
            return false;
        }

        let first = tokens[0];

        // Common C types
        let c_types = [
            "int", "char", "float", "double", "long", "short", "unsigned", "signed", "void",
            "size_t", "uint8_t", "uint16_t", "uint32_t", "uint64_t", "int8_t", "int16_t",
            "int32_t", "int64_t",
        ];

        if c_types.contains(&first) {
            return true;
        }

        // Check for pointer cast: type *
        if tokens.len() >= 2 && tokens.last() == Some(&"*") {
            return true;
        }

        // Check for struct/enum/union cast
        if first == "struct" || first == "enum" || first == "union" {
            return true;
        }

        false
    }

    /// Convert expression tokens handling casts, operators, etc.
    fn convert_tokens(&mut self, tokens: &[String]) -> String {
        if tokens.is_empty() {
            return String::new();
        }

        let mut result: Vec<String> = Vec::new();
        let mut i = 0;

        while i < tokens.len() {
            let token = &tokens[i];

            // Check for C-style cast: ( type ) value
            if token == "(" && i + 2 < tokens.len() {
                // Look for pattern: ( type )
                let close_paren = tokens[i..].iter().position(|t| t == ")");
                if let Some(cp) = close_paren {
                    let cast_tokens: Vec<&str> =
                        tokens[i + 1..i + cp].iter().map(|s| s.as_str()).collect();

                    // Check if this looks like a type cast
                    if Self::is_type_cast(&cast_tokens) {
                        let cast_type = cast_tokens.join(" ");
                        let rust_type = system().lookup_type(&cast_type)
                            .unwrap_or_else(|| cast_type.clone());

                        // Get the value being cast (next token after closing paren)
                        if i + cp + 1 < tokens.len() {
                            let cast_value = &tokens[i + cp + 1];
                            result.push(format!("{} as {}", cast_value, rust_type));
                            i = i + cp + 2;
                            continue;
                        }
                    }
                }
            }

            // Handle ++ and -- operators (postfix: x++ or prefix: ++x)
            if token == "++" {
                if let Some(prev) = result.last_mut() {
                    // Postfix: x++
                    *prev = format!("{} += 1", prev);
                } else if i + 1 < tokens.len() {
                    // Prefix: ++x
                    let next = &tokens[i + 1];
                    result.push(format!("{} += 1", next));
                    i += 2;
                    continue;
                }
                i += 1;
                continue;
            }
            if token == "--" {
                if let Some(prev) = result.last_mut() {
                    // Postfix: x--
                    *prev = format!("{} -= 1", prev);
                } else if i + 1 < tokens.len() {
                    // Prefix: --x
                    let next = &tokens[i + 1];
                    result.push(format!("{} -= 1", next));
                    i += 2;
                    continue;
                }
                i += 1;
                continue;
            }

            // Handle arrow operator: ptr->field becomes (*ptr).field
            if token == "->" || token == "- >" {
                if let Some(prev) = result.pop() {
                    // Get the field name (next token)
                    if i + 1 < tokens.len() {
                        let field = &tokens[i + 1];
                        result.push(format!("(*{}).{}", prev, field));
                        i += 2;
                        continue;
                    } else {
                        // No field name, just push back
                        result.push(prev);
                    }
                }
                i += 1;
                continue;
            }

            // Handle NULL
            if token == "NULL" {
                result.push("std::ptr::null()".to_string());
                i += 1;
                continue;
            }

            // Handle sizeof
            if token == "sizeof" {
                // Next token should be ( type )
                if i + 1 < tokens.len() && tokens[i + 1] == "(" {
                    let close = tokens[i..].iter().position(|t| t == ")");
                    if let Some(cp) = close {
                        let inner = tokens[i + 2..i + cp].join(" ");
                        let rust_type = system().lookup_type(&inner)
                            .unwrap_or_else(|| inner.clone());
                        result.push(format!("std::mem::size_of::<{}>()", rust_type));
                        i = i + cp + 1;
                        continue;
                    }
                }
            }

            result.push(token.clone());
            i += 1;
        }

        result.join(" ")
    }
}

impl Processor for ExpressionHandler {
    fn name(&self) -> &str {
        "ExpressionHandler"
    }

    fn supported_patterns(&self) -> &[&str] {
        &["validate_binary_expression", "extract_binary_expression"]
    }

    fn patterns(&self) -> Vec<(Pattern, Pattern)> {
        vec![(
            Pattern::definition(
                400,
                "validate_binary_expression",
                vec![
                    PatternRule::any(), // left operand
                    PatternRule::one_of(vec![
                        "+".to_string(),
                        "-".to_string(),
                        "*".to_string(),
                        "/".to_string(),
                        "==".to_string(),
                        "!=".to_string(),
                        "<".to_string(),
                        ">".to_string(),
                    ]),
                    PatternRule::any(), // right operand
                ],
            )
            .with_category("expression")
            .with_priority(40)
            .with_min_tokens(3)
            .with_description("Binary expression"),
            Pattern::definition(
                400,
                "extract_binary_expression",
                vec![
                    PatternRule::any().with_extract(|rule, ctx| {
                        let token = ctx.current_token.clone();
                        if ctx.did_match {
                            ctx.set_value("left", &token);
                        }
                        rule.clone()
                    }),
                    PatternRule::one_of(vec![
                        "+".to_string(),
                        "-".to_string(),
                        "*".to_string(),
                        "/".to_string(),
                        "==".to_string(),
                        "!=".to_string(),
                        "<".to_string(),
                        ">".to_string(),
                    ])
                    .with_extract(|rule, ctx| {
                        let token = ctx.current_token.clone();
                        if ctx.did_match {
                            ctx.set_value("operator", &token);
                        }
                        rule.clone()
                    }),
                    PatternRule::any().with_extract(|rule, ctx| {
                        let token = ctx.current_token.clone();
                        if ctx.did_match {
                            ctx.set_value("right", &token);
                        }
                        rule.clone()
                    }),
                ],
            )
            .with_category("expression")
            .with_priority(40)
            .with_min_tokens(3)
            .with_description("Binary expression"),
        )]
    }

    fn validate(&mut self, tokens: &[Token]) -> bool {
        if tokens.is_empty() {
            return false;
        }

        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        // Check for increment/decrement operators (2 tokens like "x ++" or "++ x")
        let has_inc_dec = token_strs.iter().any(|t| t == "++" || t == "--");
        if has_inc_dec && tokens.len() >= 2 {
            self.confidence = 0.7;
            return true;
        }

        // For other expressions, need at least 3 tokens
        if tokens.len() < 3 {
            return false;
        }

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

        // Try extraction patterns first
        for (_, extract_pattern) in self.patterns() {
            if let Some(ctx) = extract_pattern.run_extraction(&token_strs) {
                // Get left operand from pattern extraction
                if let Some(left) = ctx.value("left") {
                    self.data.left = Some(left.to_string());
                }

                // Get operator from pattern extraction
                if let Some(op) = ctx.value("operator") {
                    self.data.operator = Some(op.to_string());
                }

                // Get right operand from pattern extraction
                if let Some(right) = ctx.value("right") {
                    self.data.right = Some(right.to_string());
                }

                if self.data.operator.is_some() {
                    self.confidence = 0.7;
                    return true;
                }
            }
        }

        // Fallback to manual extraction
        for (i, token) in token_strs.iter().enumerate() {
            if Self::is_operator(token) {
                self.data.operator = Some(token.clone());

                if i > 0 {
                    self.data.left = Some(token_strs[..i].join(" "));
                }

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
        // Use convert_tokens to handle casts, ++/--, NULL, sizeof, etc.
        let rust_expr = self.convert_tokens(&self.data.tokens.clone());

        self.confidence = 0.8;
        Some(rust_expr)
    }

    fn current_stage(&self) -> ProcessStage {
        self.stage
    }
    fn set_stage(&mut self, stage: ProcessStage) {
        self.stage = stage;
    }
    fn output(&self) -> Option<String> {
        self.output.clone()
    }
    fn set_output(&mut self, output: String) {
        self.output = Some(output);
    }
    fn error(&self) -> Option<String> {
        self.error.clone()
    }
    fn set_error(&mut self, error: String) {
        self.error = Some(error);
    }
    fn confidence(&self) -> f64 {
        self.confidence
    }
    fn set_confidence(&mut self, confidence: f64) {
        self.confidence = confidence;
    }
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

    fn kind(&self) -> &str {
        "Handler"
    }
    fn name(&self) -> Option<&str> {
        Some("ExpressionHandler")
    }
    fn category(&self) -> Option<&str> {
        Some("expression")
    }
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
