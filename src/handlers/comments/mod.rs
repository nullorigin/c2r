//! Comment Handler
//!
//! Converts C comments to Rust comments.
//! - Line comments (`// ...`) pass through unchanged
//! - Block comments (`/* ... */`) pass through unchanged (Rust supports them)

use std::collections::HashMap;
use std::ops::Range;

use crate::db::pattern::{Pattern, PatternRule};
use crate::db::token::Token;
use crate::db::web::{Build, Entry};
use crate::handlers::process::{ProcessDecision, ProcessStage, Processor};

// ============================================================================
// Comment Handler Implementation
// ============================================================================

/// Comment type
#[derive(Debug, Clone, PartialEq)]
pub enum CommentKind {
    /// Line comment: // ...
    Line,
    /// Block comment: /* ... */
    Block,
    /// Doc comment: /// ... or /** ... */
    Doc,
}

impl Default for CommentKind {
    fn default() -> Self {
        CommentKind::Line
    }
}

/// Extracted comment information
#[derive(Debug, Clone, Default)]
pub struct CommentData {
    /// The comment text (without delimiters)
    pub text: String,
    /// The kind of comment
    pub kind: CommentKind,
    /// Whether this is a doc comment
    pub is_doc: bool,
}

/// Comment handler state for processing
#[derive(Debug, Clone)]
pub struct CommentHandler {
    /// Handler name
    name: String,
    /// Current processing stage
    stage: ProcessStage,
    /// Extracted comment data
    data: CommentData,
    /// Generated Rust output
    output: Option<String>,
    /// Error message if failed
    error: Option<String>,
    /// Confidence score
    confidence: f64,
    /// Token range processed
    range: Range<usize>,
    /// Input tokens (stored for Build trait)
    input_tokens: Vec<String>,
}

impl CommentHandler {
    pub fn new() -> Self {
        Self {
            name: "comment".to_string(),
            stage: ProcessStage::Pending,
            data: CommentData::default(),
            output: None,
            error: None,
            confidence: 0.0,
            range: 0..0,
            input_tokens: Vec::new(),
        }
    }
}

impl Default for CommentHandler {
    fn default() -> Self {
        Self::new()
    }
}

impl Processor for CommentHandler {
    fn supported_patterns(&self) -> &[&str] {
        &[
            "validate_line_comment",
            "extract_line_comment",
            "validate_block_comment",
            "extract_block_comment",
        ]
    }

    fn patterns(&self) -> Vec<(Pattern, Pattern)> {
        vec![
            (
                Pattern::definition(
                    0,
                    "validate_line_comment",
                    vec![PatternRule::custom("starts_with_line_comment")],
                )
                .with_category("comment")
                .with_priority(100)
                .with_min_tokens(1)
                .with_description("C line comment"),
                Pattern::definition(
                    0,
                    "extract_line_comment",
                    vec![
                        PatternRule::custom("starts_with_line_comment").with_extract(
                            |rule, ctx| {
                                let token = ctx.current_token.clone();
                                if ctx.did_match {
                                    ctx.set_value("comment_type", "line");
                                    ctx.set_value("content", &token);
                                }
                                rule.clone()
                            },
                        ),
                    ],
                )
                .with_category("comment")
                .with_priority(100)
                .with_min_tokens(1)
                .with_description("C line comment"),
            ),
            (
                Pattern::definition(
                    1,
                    "validate_block_comment",
                    vec![PatternRule::custom("starts_with_block_comment")],
                )
                .with_category("comment")
                .with_priority(100)
                .with_min_tokens(1)
                .with_description("C block comment"),
                Pattern::definition(
                    1,
                    "extract_block_comment",
                    vec![
                        PatternRule::custom("starts_with_block_comment").with_extract(
                            |rule, ctx| {
                                let token = ctx.current_token.clone();
                                if ctx.did_match {
                                    ctx.set_value("comment_type", "block");
                                    ctx.set_value("content", &token);
                                }
                                rule.clone()
                            },
                        ),
                    ],
                )
                .with_category("comment")
                .with_priority(100)
                .with_min_tokens(1)
                .with_description("C block comment"),
            ),
        ]
    }

    fn validate(&mut self, tokens: &[Token]) -> bool {
        if tokens.is_empty() {
            self.error = Some("No tokens to process".to_string());
            return false;
        }

        // Store tokens for later
        self.input_tokens = tokens.iter().map(|t| t.to_string()).collect();
        self.range = 0..tokens.len();

        // Try matching against our patterns
        let mut best_confidence = 0.0;
        for (pattern, _) in self.patterns() {
            if let Some(confidence) = pattern.matches_tokens(&self.input_tokens) {
                if confidence > best_confidence {
                    best_confidence = confidence;
                }
            }
        }

        // Check if the first token is a comment
        let first = tokens[0].to_string();

        if first.starts_with("//") {
            self.data.kind = if first.starts_with("///") {
                self.data.is_doc = true;
                CommentKind::Doc
            } else {
                CommentKind::Line
            };
            self.confidence = if best_confidence > 0.0 {
                best_confidence
            } else {
                1.0
            };
            true
        } else if first.starts_with("/*") {
            self.data.kind = if first.starts_with("/**") {
                self.data.is_doc = true;
                CommentKind::Doc
            } else {
                CommentKind::Block
            };
            self.confidence = if best_confidence > 0.0 {
                best_confidence
            } else {
                1.0
            };
            true
        } else {
            self.error = Some(format!("Not a comment: '{}'", first));
            false
        }
    }

    fn extract(&mut self, tokens: &[Token]) -> bool {
        if tokens.is_empty() {
            self.error = Some("No tokens to extract".to_string());
            return false;
        }

        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        // Try extraction patterns first
        for (_, extract_pattern) in self.patterns() {
            if let Some(ctx) = extract_pattern.run_extraction(&token_strs) {
                // Get comment type from pattern extraction
                if let Some(comment_type) = ctx.value("comment_type") {
                    match comment_type {
                        "line" => self.data.kind = CommentKind::Line,
                        "block" => self.data.kind = CommentKind::Block,
                        "doc" => {
                            self.data.kind = CommentKind::Doc;
                            self.data.is_doc = true;
                        }
                        _ => {}
                    }
                }

                // Get content from pattern extraction - strip the comment prefix
                if let Some(content) = ctx.value("content") {
                    let prefix_len = if self.data.is_doc { 3 } else { 2 };
                    self.data.text = content
                        .chars()
                        .skip(prefix_len)
                        .collect::<String>()
                        .trim()
                        .to_string();
                    self.confidence = 1.0;
                    return true;
                }
            }
        }

        // Fallback to manual extraction
        let full_comment = tokens[0].to_string();

        match self.data.kind {
            CommentKind::Line | CommentKind::Doc => {
                let prefix_len = if self.data.is_doc { 3 } else { 2 };
                self.data.text = full_comment
                    .chars()
                    .skip(prefix_len)
                    .collect::<String>()
                    .trim()
                    .to_string();
            }
            CommentKind::Block => {
                let start = if self.data.is_doc { 3 } else { 2 };
                let text = &full_comment[start..];
                self.data.text = text.strip_suffix("*/").unwrap_or(text).trim().to_string();
            }
        }

        self.confidence = 1.0;
        true
    }

    fn convert(&mut self) -> Option<String> {
        // C comments are compatible with Rust
        // Line comments: // ... -> // ...
        // Block comments: /* ... */ -> /* ... */ (Rust supports these)
        // Doc comments: /// ... -> /// ... (or /** ... */ -> /// ...)

        let rust_code = match self.data.kind {
            CommentKind::Line => {
                format!("// {}", self.data.text)
            }
            CommentKind::Doc => {
                format!("/// {}", self.data.text)
            }
            CommentKind::Block => {
                // Convert block comments to line comments for cleaner Rust style
                // Multi-line block comments become multiple line comments
                let lines: Vec<&str> = self.data.text.lines().collect();
                if lines.len() == 1 {
                    format!("// {}", self.data.text)
                } else {
                    lines
                        .iter()
                        .map(|line| format!("// {}", line.trim()))
                        .collect::<Vec<_>>()
                        .join("\n")
                }
            }
        };

        self.confidence = 1.0;
        Some(rust_code)
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

// ============================================================================
// Build Trait Implementation
// ============================================================================

impl Build for CommentHandler {
    fn to_entry(&self) -> Entry {
        let mut attrs = HashMap::new();

        attrs.insert("stage".to_string(), Entry::string(self.stage.as_str()));
        attrs.insert("confidence".to_string(), Entry::f64(self.confidence));
        attrs.insert("text".to_string(), Entry::string(&self.data.text));
        attrs.insert(
            "kind".to_string(),
            Entry::string(match self.data.kind {
                CommentKind::Line => "line",
                CommentKind::Block => "block",
                CommentKind::Doc => "doc",
            }),
        );
        attrs.insert("is_doc".to_string(), Entry::bool(self.data.is_doc));

        if let Some(ref output) = self.output {
            attrs.insert("rust_code".to_string(), Entry::string(output));
        }

        if let Some(ref error) = self.error {
            attrs.insert("error".to_string(), Entry::string(error));
        }

        let tokens: Vec<Entry> = self.input_tokens.iter().map(|t| Entry::string(t)).collect();
        attrs.insert("input_tokens".to_string(), Entry::vec(tokens));

        attrs.insert("range_start".to_string(), Entry::usize(self.range.start));
        attrs.insert("range_end".to_string(), Entry::usize(self.range.end));

        Entry::node_with_attrs("CommentHandler", "comment", attrs)
    }

    fn kind(&self) -> &str {
        "CommentHandler"
    }

    fn name(&self) -> Option<&str> {
        Some("comment")
    }

    fn category(&self) -> Option<&str> {
        Some("comment")
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    fn make_tokens(strings: &[&str]) -> Vec<Token> {
        strings.iter().map(|s| Token::from_string(*s)).collect()
    }

    #[test]
    fn test_line_comment() {
        let mut handler = CommentHandler::new();
        let tokens = make_tokens(&["// This is a comment"]);

        // Process through all stages
        while !matches!(
            handler.current_stage(),
            ProcessStage::Complete | ProcessStage::Failed
        ) {
            handler.process(&tokens);
        }

        assert_eq!(handler.output(), Some("// This is a comment".to_string()));
        assert!(handler.confidence() > 0.9);
    }

    #[test]
    fn test_block_comment() {
        let mut handler = CommentHandler::new();
        let tokens = make_tokens(&["/* Block comment */"]);

        while !matches!(
            handler.current_stage(),
            ProcessStage::Complete | ProcessStage::Failed
        ) {
            handler.process(&tokens);
        }

        assert_eq!(handler.output(), Some("// Block comment".to_string()));
    }

    #[test]
    fn test_doc_comment() {
        let mut handler = CommentHandler::new();
        let tokens = make_tokens(&["/// Documentation comment"]);

        while !matches!(
            handler.current_stage(),
            ProcessStage::Complete | ProcessStage::Failed
        ) {
            handler.process(&tokens);
        }

        assert_eq!(
            handler.output(),
            Some("/// Documentation comment".to_string())
        );
    }

    #[test]
    fn test_non_comment_fails() {
        let mut handler = CommentHandler::new();
        let tokens = make_tokens(&["int", "x", ";"]);

        let decision = handler.process(&tokens);
        assert!(matches!(decision, ProcessDecision::Fail { .. }));
    }

    #[test]
    fn test_build_trait() {
        let mut handler = CommentHandler::new();
        let tokens = make_tokens(&["// Test comment"]);

        while !matches!(
            handler.current_stage(),
            ProcessStage::Complete | ProcessStage::Failed
        ) {
            handler.process(&tokens);
        }

        let entry = handler.to_entry();
        assert_eq!(entry.kind(), Some("CommentHandler"));
    }
}
