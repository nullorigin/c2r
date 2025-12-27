//! Handler Validation Utilities
//!
//! Integrates keyword.rs Order system into handlers for token sequence validation.
//! This helps prevent misvalidations by ensuring tokens follow valid C grammar patterns.

use crate::db::keyword::{
    is_control_flow, is_preprocessor, is_storage_class, is_type_keyword, is_type_qualifier, Order,
};
use crate::db::token::Token;

// ============================================================================
// Token Sequence Validator
// ============================================================================

/// Validates token sequences using keyword ordering rules
#[derive(Debug)]
pub struct SequenceValidator {
    order: Order,
}

impl SequenceValidator {
    /// Create a new validator with C language rules
    pub fn new() -> Self {
        Self {
            order: Order::with_c_rules(),
        }
    }

    /// Create with custom Order rules
    pub fn with_order(order: Order) -> Self {
        Self { order }
    }

    /// Validate that a token sequence follows valid C grammar
    pub fn validate_sequence(&self, tokens: &[Token]) -> ValidationResult {
        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        if token_strs.is_empty() {
            return ValidationResult::valid();
        }

        // Check each pair of adjacent tokens
        let mut violations: Vec<OrderViolation> = Vec::new();

        for (i, window) in token_strs.windows(2).enumerate() {
            if !self.order.can_follow(&window[0], &window[1]) {
                violations.push(OrderViolation {
                    position: i,
                    current: window[0].clone(),
                    next: window[1].clone(),
                    suggestion: self.order.suggest(&window[0]),
                });
            }
        }

        if violations.is_empty() {
            ValidationResult::valid()
        } else {
            ValidationResult::invalid(violations)
        }
    }

    /// Check if a specific token pair is valid
    pub fn can_follow(&self, current: &str, next: &str) -> bool {
        self.order.can_follow(current, next)
    }

    /// Get suggestions for what can follow a token
    pub fn suggest_next(&self, current: &str) -> Vec<String> {
        self.order.suggest(current)
    }

    /// Validate preprocessor directive sequence
    pub fn validate_preprocessor(&self, tokens: &[Token]) -> ValidationResult {
        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        if token_strs.is_empty() {
            return ValidationResult::invalid_with_reason("Empty preprocessor directive");
        }

        let first = &token_strs[0];

        // Must start with #
        if first != "#" && !first.starts_with('#') {
            return ValidationResult::invalid_with_reason("Preprocessor must start with #");
        }

        // If separate tokens, check # is followed by valid directive
        if first == "#" {
            if token_strs.len() < 2 {
                return ValidationResult::invalid_with_reason("# not followed by directive");
            }

            let directive = &token_strs[1];
            if !is_preprocessor(directive) {
                return ValidationResult::invalid_with_reason(&format!(
                    "'{}' is not a valid preprocessor directive",
                    directive
                ));
            }

            // Use keyword ordering to validate
            if !self.order.can_follow("#", directive) {
                return ValidationResult::invalid_with_reason(&format!(
                    "# cannot be followed by '{}'",
                    directive
                ));
            }
        }

        ValidationResult::valid()
    }

    /// Validate struct/union/enum declaration sequence
    pub fn validate_type_declaration(&self, tokens: &[Token]) -> ValidationResult {
        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        if token_strs.is_empty() {
            return ValidationResult::invalid_with_reason("Empty type declaration");
        }

        let mut i = 0;

        // Optional: typedef
        if i < token_strs.len() && token_strs[i] == "typedef" {
            i += 1;
        }

        // Required: struct/union/enum
        if i >= token_strs.len() {
            return ValidationResult::invalid_with_reason("Missing type keyword");
        }

        let type_kw = &token_strs[i];
        if !matches!(type_kw.as_str(), "struct" | "union" | "enum") {
            return ValidationResult::invalid_with_reason(&format!(
                "Expected struct/union/enum, found '{}'",
                type_kw
            ));
        }

        // Optional: identifier (name)
        // Then: { body } or just ;

        // Validate the sequence using Order
        self.validate_sequence(tokens)
    }

    /// Validate function declaration/definition sequence
    pub fn validate_function(&self, tokens: &[Token]) -> ValidationResult {
        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        if token_strs.len() < 4 {
            return ValidationResult::invalid_with_reason("Function declaration too short");
        }

        // Check for return type
        let mut has_type = false;
        for (i, token) in token_strs.iter().enumerate() {
            if is_type_keyword(token) || is_storage_class(token) || is_type_qualifier(token) {
                has_type = true;
            }

            // Found function name followed by (
            if i + 1 < token_strs.len() && token_strs[i + 1] == "(" {
                if !has_type && !is_type_keyword(token) {
                    // Identifier followed by ( - could be function name
                    break;
                }
            }
        }

        // Must have ( and )
        let has_open_paren = token_strs.iter().any(|t| t == "(");
        let has_close_paren = token_strs.iter().any(|t| t == ")");

        if !has_open_paren || !has_close_paren {
            return ValidationResult::invalid_with_reason("Function missing parentheses");
        }

        self.validate_sequence(tokens)
    }

    /// Validate variable declaration sequence
    pub fn validate_variable(&self, tokens: &[Token]) -> ValidationResult {
        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        if token_strs.is_empty() {
            return ValidationResult::invalid_with_reason("Empty variable declaration");
        }

        // Should have a type and an identifier
        let mut has_type = false;

        for token in &token_strs {
            if is_type_keyword(token) || is_storage_class(token) || is_type_qualifier(token) {
                has_type = true;
                break;
            }
        }

        if !has_type {
            // Could be a user-defined type
        }

        // Should end with ;
        if !token_strs.last().map(|t| t == ";").unwrap_or(false) {
            return ValidationResult::invalid_with_reason("Variable declaration should end with ;");
        }

        self.validate_sequence(tokens)
    }

    /// Validate control flow statement sequence
    pub fn validate_control_flow(&self, tokens: &[Token]) -> ValidationResult {
        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        if token_strs.is_empty() {
            return ValidationResult::invalid_with_reason("Empty control flow statement");
        }

        let first = &token_strs[0];

        if !is_control_flow(first) {
            return ValidationResult::invalid_with_reason(&format!(
                "'{}' is not a control flow keyword",
                first
            ));
        }

        // Control flow with condition (if, while, for, switch) must have (
        if matches!(first.as_str(), "if" | "while" | "for" | "switch") {
            if token_strs.len() < 2 || token_strs[1] != "(" {
                return ValidationResult::invalid_with_reason(&format!(
                    "'{}' must be followed by '('",
                    first
                ));
            }
        }

        self.validate_sequence(tokens)
    }
}

impl Default for SequenceValidator {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// Validation Result
// ============================================================================

/// Result of sequence validation
#[derive(Debug, Clone)]
pub struct ValidationResult {
    pub is_valid: bool,
    pub violations: Vec<OrderViolation>,
    pub reason: Option<String>,
    pub confidence_penalty: f64,
}

impl ValidationResult {
    /// Create a valid result
    pub fn valid() -> Self {
        Self {
            is_valid: true,
            violations: Vec::new(),
            reason: None,
            confidence_penalty: 0.0,
        }
    }

    /// Create an invalid result with violations
    pub fn invalid(violations: Vec<OrderViolation>) -> Self {
        let penalty = (violations.len() as f64 * 0.1).min(0.5);
        Self {
            is_valid: false,
            violations,
            reason: None,
            confidence_penalty: penalty,
        }
    }

    /// Create an invalid result with a reason
    pub fn invalid_with_reason(reason: &str) -> Self {
        Self {
            is_valid: false,
            violations: Vec::new(),
            reason: Some(reason.to_string()),
            confidence_penalty: 0.3,
        }
    }

    /// Adjust confidence based on validation result
    pub fn adjust_confidence(&self, base_confidence: f64) -> f64 {
        if self.is_valid {
            base_confidence
        } else {
            (base_confidence - self.confidence_penalty).max(0.0)
        }
    }
}

/// A single ordering violation
#[derive(Debug, Clone)]
pub struct OrderViolation {
    pub position: usize,
    pub current: String,
    pub next: String,
    pub suggestion: Vec<String>,
}

impl std::fmt::Display for OrderViolation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "at position {}: '{}' cannot be followed by '{}'",
            self.position, self.current, self.next
        )?;
        if !self.suggestion.is_empty() {
            write!(f, " (expected: {})", self.suggestion.join(", "))?;
        }
        Ok(())
    }
}

// ============================================================================
// Handler Validation Trait
// ============================================================================

/// Trait for handlers that use keyword validation
pub trait ValidatesWithKeywords {
    /// Get the sequence validator
    fn validator(&self) -> &SequenceValidator;

    /// Validate tokens and return adjusted confidence
    fn validate_with_keywords(&self, tokens: &[Token], base_confidence: f64) -> (bool, f64) {
        let result = self.validator().validate_sequence(tokens);
        (result.is_valid, result.adjust_confidence(base_confidence))
    }
}

// ============================================================================
// Global Validator Instance
// ============================================================================

use std::sync::LazyLock;

/// Global sequence validator with C rules
pub static VALIDATOR: LazyLock<SequenceValidator> = LazyLock::new(SequenceValidator::new);

/// Convenience function to validate a token sequence
pub fn validate_tokens(tokens: &[Token]) -> ValidationResult {
    VALIDATOR.validate_sequence(tokens)
}

/// Convenience function to check if two tokens can be adjacent
pub fn can_follow(current: &str, next: &str) -> bool {
    VALIDATOR.can_follow(current, next)
}
