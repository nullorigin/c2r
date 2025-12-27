//! Comparison Expression Handler
//!
//! Converts C comparison expressions to Rust equivalents.
//! Handles complex comparisons including pointer comparisons, NULL checks, etc.

use crate::db::convert::IdentifierConverter;

// ============================================================================
// Comparison Converter
// ============================================================================

/// Types of comparison operations
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ComparisonOp {
    Equal,          // ==
    NotEqual,       // !=
    LessThan,       // <
    GreaterThan,    // >
    LessOrEqual,    // <=
    GreaterOrEqual, // >=
}

impl ComparisonOp {
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "==" => Some(Self::Equal),
            "!=" => Some(Self::NotEqual),
            "<" => Some(Self::LessThan),
            ">" => Some(Self::GreaterThan),
            "<=" => Some(Self::LessOrEqual),
            ">=" => Some(Self::GreaterOrEqual),
            _ => None,
        }
    }

    pub fn as_str(&self) -> &str {
        match self {
            Self::Equal => "==",
            Self::NotEqual => "!=",
            Self::LessThan => "<",
            Self::GreaterThan => ">",
            Self::LessOrEqual => "<=",
            Self::GreaterOrEqual => ">=",
        }
    }
}

/// Comparison expression data
#[derive(Debug, Clone)]
pub struct Comparison {
    pub left: String,
    pub op: ComparisonOp,
    pub right: String,
}

/// Comparison expression converter
#[derive(Debug)]
pub struct ComparisonConverter {
    id_converter: IdentifierConverter,
}

impl ComparisonConverter {
    pub fn new() -> Self {
        Self {
            id_converter: IdentifierConverter::new(),
        }
    }

    /// Parse a comparison expression from tokens
    pub fn parse(&self, tokens: &[&str]) -> Option<Comparison> {
        // Find comparison operator
        let op_pos = tokens
            .iter()
            .position(|t| matches!(*t, "==" | "!=" | "<" | ">" | "<=" | ">="))?;

        if op_pos == 0 || op_pos >= tokens.len() - 1 {
            return None;
        }

        let left = tokens[..op_pos].join(" ");
        let op = ComparisonOp::from_str(tokens[op_pos])?;
        let right = tokens[op_pos + 1..].join(" ");

        Some(Comparison { left, op, right })
    }

    /// Convert a comparison expression to Rust
    pub fn convert(&mut self, tokens: &[&str]) -> String {
        if let Some(cmp) = self.parse(tokens) {
            self.convert_comparison(&cmp)
        } else {
            // Not a comparison, just return joined tokens
            tokens.join(" ")
        }
    }

    /// Convert a parsed comparison to Rust
    pub fn convert_comparison(&mut self, cmp: &Comparison) -> String {
        let left = self.convert_operand(&cmp.left);
        let right = self.convert_operand(&cmp.right);

        // Handle NULL comparisons specially
        if right == "std::ptr::null()" || right == "NULL" {
            match cmp.op {
                ComparisonOp::Equal => return format!("{}.is_null()", left),
                ComparisonOp::NotEqual => return format!("!{}.is_null()", left),
                _ => {}
            }
        }
        if left == "std::ptr::null()" || left == "NULL" {
            match cmp.op {
                ComparisonOp::Equal => return format!("{}.is_null()", right),
                ComparisonOp::NotEqual => return format!("!{}.is_null()", right),
                _ => {}
            }
        }

        format!("{} {} {}", left, cmp.op.as_str(), right)
    }

    /// Convert a single operand
    fn convert_operand(&mut self, operand: &str) -> String {
        let trimmed = operand.trim();

        // Use IdentifierConverter for known identifiers
        if let Some(converted) = self.id_converter.convert(trimmed) {
            return converted;
        }

        trimmed.to_string()
    }

    /// Check if tokens contain a comparison operator
    pub fn is_comparison(tokens: &[&str]) -> bool {
        tokens
            .iter()
            .any(|t| matches!(*t, "==" | "!=" | "<" | ">" | "<=" | ">="))
    }

    /// Convert a logical expression (with && and ||)
    pub fn convert_logical(&mut self, tokens: &[&str]) -> String {
        let joined = tokens.join(" ");

        // Split by logical operators while preserving them
        let mut result = String::new();
        let mut current: Vec<&str> = Vec::new();

        for token in tokens {
            match *token {
                "&&" | "||" => {
                    if !current.is_empty() {
                        result.push_str(&self.convert(&current));
                        current.clear();
                    }
                    result.push_str(&format!(" {} ", token));
                }
                _ => {
                    current.push(token);
                }
            }
        }

        if !current.is_empty() {
            result.push_str(&self.convert(&current));
        }

        if result.is_empty() { joined } else { result }
    }

    /// Convert negation (!)
    pub fn convert_negation(&mut self, tokens: &[&str]) -> String {
        if tokens.first() == Some(&"!") {
            let inner = &tokens[1..];

            // Check for !(expr) pattern
            if inner.first() == Some(&"(") && inner.last() == Some(&")") {
                let inner_expr = &inner[1..inner.len() - 1];
                return format!("!({})", self.convert(inner_expr));
            }

            return format!("!{}", self.convert(inner));
        }

        self.convert(tokens)
    }
}

impl Default for ComparisonConverter {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_comparison() {
        let mut conv = ComparisonConverter::new();
        let tokens = ["x", ">", "0"];
        let result = conv.convert(&tokens);
        assert_eq!(result, "x > 0");
    }

    #[test]
    fn test_null_comparison() {
        let mut conv = ComparisonConverter::new();
        let tokens = ["ptr", "==", "NULL"];
        let result = conv.convert(&tokens);
        assert_eq!(result, "ptr.is_null()");
    }

    #[test]
    fn test_not_null_comparison() {
        let mut conv = ComparisonConverter::new();
        let tokens = ["ptr", "!=", "NULL"];
        let result = conv.convert(&tokens);
        assert_eq!(result, "!ptr.is_null()");
    }

    #[test]
    fn test_logical_expression() {
        let mut conv = ComparisonConverter::new();
        let tokens = ["x", ">", "0", "&&", "x", "<", "10"];
        let result = conv.convert_logical(&tokens);
        assert_eq!(result, "x > 0 && x < 10");
    }

    #[test]
    fn test_is_comparison() {
        assert!(ComparisonConverter::is_comparison(&["x", "==", "y"]));
        assert!(ComparisonConverter::is_comparison(&["a", "!=", "b"]));
        assert!(!ComparisonConverter::is_comparison(&["x", "+", "y"]));
    }
}
