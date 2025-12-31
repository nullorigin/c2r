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
use crate::handlers::process::{ProcessorStage, ProcessorDecision, Processor, ProcessorState, ProcessorStats};
use crate::system::system;

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

impl CommentKind {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Line => "line",
            Self::Block => "block",
            Self::Doc => "doc",
        }
    }
}

impl Build for CommentKind {
    fn to_entry(&self) -> Entry { Entry::string(self.as_str()) }
    fn kind(&self) -> &str { "CommentKind" }
    fn name(&self) -> Option<&str> { Some(self.as_str()) }
    fn category(&self) -> Option<&str> { Some("comment") }
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

impl Build for CommentData {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("CommentData", &self.text);
        entry.set_attr("text", Entry::string(&self.text));
        entry.set_attr("kind", self.kind.to_entry());
        entry.set_attr("is_doc", Entry::bool(self.is_doc));
        entry
    }

    fn kind(&self) -> &str {
        "CommentData"
    }

    fn name(&self) -> Option<&str> {
        Some(&self.text)
    }

    fn category(&self) -> Option<&str> {
        Some("comment")
    }
}

/// Comment handler state for processing
#[derive(Debug, Clone)]
pub struct CommentHandler {
    /// Generic processor state (stage, confidence, error, output, data)
    state: ProcessorState<CommentData>,
    /// Token range processed
    range: Range<usize>,
    /// Input tokens (stored for Build trait)
    input_tokens: Vec<String>,
}

impl CommentHandler {
    pub fn new() -> Self {
        Self {
            state: ProcessorState::new("CommentHandler"),
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

    fn stats(&self) -> &ProcessorStats {
        system().process_stats(&self.state.stats);
        &self.state.stats
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
            self.state.set_error("No tokens to process");
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
            self.state.data.kind = if first.starts_with("///") {
                self.state.data.is_doc = true;
                CommentKind::Doc
            } else {
                CommentKind::Line
            };
            self.state.set_confidence(if best_confidence > 0.0 {
                best_confidence
            } else {
                1.0
            });
            true
        } else if first.starts_with("/*") {
            self.state.data.kind = if first.starts_with("/**") {
                self.state.data.is_doc = true;
                CommentKind::Doc
            } else {
                CommentKind::Block
            };
            self.state.set_confidence(if best_confidence > 0.0 {
                best_confidence
            } else {
                1.0
            });
            true
        } else {
            self.state.set_error(format!("Not a comment: '{}'", first));
            false
        }
    }

    fn extract(&mut self, tokens: &[Token]) -> bool {
        if tokens.is_empty() {
            self.state.set_error("No tokens to extract");
            return false;
        }

        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        // Try extraction patterns first
        for (_, extract_pattern) in self.patterns() {
            if let Some(ctx) = extract_pattern.run_extraction(&token_strs) {
                // Get comment type from pattern extraction
                if let Some(comment_type) = ctx.value("comment_type") {
                    match comment_type {
                        "line" => self.state.data.kind = CommentKind::Line,
                        "block" => self.state.data.kind = CommentKind::Block,
                        "doc" => {
                            self.state.data.kind = CommentKind::Doc;
                            self.state.data.is_doc = true;
                        }
                        _ => {}
                    }
                }

                // Get content from pattern extraction - strip the comment prefix
                if let Some(content) = ctx.value("content") {
                    let prefix_len = if self.state.data.is_doc { 3 } else { 2 };
                    self.state.data.text = content
                        .chars()
                        .skip(prefix_len)
                        .collect::<String>()
                        .trim()
                        .to_string();
                    self.state.set_confidence(1.0);
                    return true;
                }
            }
        }

        // Fallback to manual extraction
        let full_comment = tokens[0].to_string();

        match self.state.data.kind {
            CommentKind::Line | CommentKind::Doc => {
                let prefix_len = if self.state.data.is_doc { 3 } else { 2 };
                self.state.data.text = full_comment
                    .chars()
                    .skip(prefix_len)
                    .collect::<String>()
                    .trim()
                    .to_string();
            }
            CommentKind::Block => {
                let start = if self.state.data.is_doc { 3 } else { 2 };
                let text = &full_comment[start..];
                self.state.data.text = text.strip_suffix("*/").unwrap_or(text).trim().to_string();
            }
        }

        self.state.set_confidence(1.0);
        true
    }

    fn convert(&mut self) -> Option<String> {
        // C comments are compatible with Rust
        // Line comments: // ... -> // ...
        // Block comments: /* ... */ -> /* ... */ (Rust supports these)
        // Doc comments: /// ... -> /// ... (or /** ... */ -> /// ...)

        let rust_code = match self.state.data.kind {
            CommentKind::Line => {
                format!("// {}", self.state.data.text)
            }
            CommentKind::Doc => {
                format!("/// {}", self.state.data.text)
            }
            CommentKind::Block => {
                // Convert block comments to line comments for cleaner Rust style
                // Multi-line block comments become multiple line comments
                let lines: Vec<&str> = self.state.data.text.lines().collect();
                if lines.len() == 1 {
                    format!("// {}", self.state.data.text)
                } else {
                    lines
                        .iter()
                        .map(|line| format!("// {}", line.trim()))
                        .collect::<Vec<_>>()
                        .join("\n")
                }
            }
        };

        self.state.set_confidence(1.0);
        Some(rust_code)
    }

    fn current_stage(&self) -> ProcessorStage {
        self.state.stage()
    }

    fn set_stage(&mut self, stage: ProcessorStage) {
        self.state.set_stage(stage);
    }

    fn output(&self) -> Option<String> {
        self.state.output.clone()
    }

    fn set_output(&mut self, output: String) {
        self.state.set_output(output);
    }

    fn error(&self) -> Option<String> {
        self.state.error.clone()
    }

    fn set_error(&mut self, error: String) {
        self.state.set_error(error);
    }

    fn confidence(&self) -> f64 {
        self.state.confidence
    }

    fn set_confidence(&mut self, confidence: f64) {
        self.state.set_confidence(confidence);
    }
}

// ============================================================================
// Build Trait Implementation
// ============================================================================

impl Build for CommentHandler {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("Handler", "CommentHandler");
        
        entry.set_attr("stage", self.state.stage().to_entry());
        entry.set_attr("confidence", Entry::f64(self.state.confidence));
        entry.set_attr("text", Entry::string(&self.state.data.text));
        entry.set_attr("kind", self.state.data.kind.to_entry());
        entry.set_attr("is_doc", Entry::bool(self.state.data.is_doc));

        if let Some(ref output) = self.state.output {
            entry.set_attr("rust_code", Entry::string(output));
        }

        if let Some(ref error) = self.state.error {
            entry.set_attr("error", Entry::string(error));
        }

        let tokens: Vec<Entry> = self.input_tokens.iter().map(|t| Entry::string(t)).collect();
        entry.set_attr("input_tokens", Entry::vec(tokens));

        entry.set_attr("range_start", Entry::usize(self.range.start));
        entry.set_attr("range_end", Entry::usize(self.range.end));

        entry
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
            ProcessorStage::Complete | ProcessorStage::Failed
        ) {
            handler.process(&tokens, None);
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
            ProcessorStage::Complete | ProcessorStage::Failed
        ) {
            handler.process(&tokens, None);
        }

        assert_eq!(handler.output(), Some("// Block comment".to_string()));
    }

    #[test]
    fn test_doc_comment() {
        let mut handler = CommentHandler::new();
        let tokens = make_tokens(&["/// Documentation comment"]);

        while !matches!(
            handler.current_stage(),
            ProcessorStage::Complete | ProcessorStage::Failed
        ) {
            handler.process(&tokens, None);
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

        let decision = handler.process(&tokens, None);
        assert!(matches!(decision, ProcessorDecision::Fail { .. }));
    }

    #[test]
    fn test_build_trait() {
        let mut handler = CommentHandler::new();
        let tokens = make_tokens(&["// Test comment"]);

        while !matches!(
            handler.current_stage(),
            ProcessorStage::Complete | ProcessorStage::Failed
        ) {
            handler.process(&tokens, None);
        }

        let entry = handler.to_entry();
        assert_eq!(entry.kind(), Some("CommentHandler"));
    }
}
