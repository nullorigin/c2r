//! Comment handler implementation using trait-based system
//! Handles C-style comments (/* */) and C++-style comments (//)

use crate::info::CommentInfo;
use crate::report;
use crate::{
    gen_name, Context, ConvertedComment, ConvertedElement, ExtractedComment, ExtractedElement,
    Handler, HandlerResult, Id, Phase, RedirectRequest, Report, ReportLevel, Result,
};
use std::ops::Range;

/// Comment handler implementing the Handler trait
#[derive(Debug, Clone)]
pub struct CommentHandler;

impl CommentHandler {
    pub fn new() -> Self {
        Self
    }

    /// Enhanced pattern detection using both Patternizer and Samplizer
    fn detect_pattern(
        &self,
        patternizer: &mut crate::Patternizer,
        samplizer: &crate::Samplizer,
        tokens: &[crate::Token],
        token_range: Range<usize>,
    ) -> Result<(bool, f64)> {
        if tokens.is_empty() {
            return Ok((false, 0.0));
        }

        // Try registered pattern for comments
        let pattern_match = patternizer.match_pattern("comment", tokens);
        let pattern_success = !matches!(pattern_match, crate::PatternResult::NoMatch { .. });

        // Simple heuristic detection for comments
        let has_comment_marker = tokens.iter().any(|t| {
            let s = t.to_string();
            s == "//" || s == "/*" || s == "*/"
        });

        // Confidence based on pattern match and heuristics
        let confidence = if pattern_success && has_comment_marker {
            0.95
        } else if has_comment_marker {
            0.85
        } else if pattern_success {
            0.75
        } else {
            0.0
        };

        Ok((pattern_success || has_comment_marker, confidence))
    }


    /// Extract comment information from tokens using pattern-based analysis
    fn extract_info(&self, tokens: &[crate::Token], patternizer: &crate::Patternizer) -> Result<CommentInfo> {
        let full_text = tokens
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<_>>()
            .join("");

        // Use Patternizer to classify comment type
        let comment_type = self.classify_type(&full_text, patternizer);
        let content = self.extract_content_by_type(&full_text, &comment_type);
        let is_documentation = comment_type.starts_with("doc_") || self.has_doc_indicators(&content);
        let is_multiline = full_text.contains("\n");
        let line_count = full_text.lines().count();

        Ok(CommentInfo {
            raw_comment: full_text,
            comment_type,
            content,
            is_documentation,
            is_multiline,
            line_count,
        })
    }

    /// Classify comment type using pattern-based approach
    fn classify_type(&self, text: &str, _patternizer: &crate::Patternizer) -> String {
        // Direct pattern matching for comment types
        match text {
            t if t.starts_with("/**") => "doc_block".to_string(),
            t if t.starts_with("///") => "doc_line".to_string(),
            t if t.starts_with("//!") => "doc_inner".to_string(),
            t if t.starts_with("//") => "line".to_string(),
            t if t.starts_with("/*") && t.ends_with("*/") => "block".to_string(),
            t if t.starts_with("/*") => "incomplete_block".to_string(),
            _ => "unknown".to_string(),
        }
    }

    /// Extract content based on comment type
    fn extract_content_by_type(&self, text: &str, comment_type: &str) -> String {
        let content = match comment_type {
            "line" => text.strip_prefix("//").unwrap_or(text),
            "doc_line" => text.strip_prefix("///").unwrap_or(text),
            "doc_inner" => text.strip_prefix("//!").unwrap_or(text),
            "doc_block" => text.strip_prefix("/**")
                .or_else(|| text.strip_prefix("/*"))
                .unwrap_or(text),
            "block" | "incomplete_block" => text.strip_prefix("/*").unwrap_or(text),
            _ => text,
        };

        // Strip trailing close for block comments
        let content = if comment_type.contains("block") && content.ends_with("*/") {
            content.strip_suffix("*/").unwrap_or(content)
        } else {
            content
        };

        // Clean up common formatting artifacts
        content
            .lines()
            .map(|line| {
                line.trim_start_matches('*')
                    .trim_start_matches(' ')
                    .trim_end()
            })
            .collect::<Vec<_>>()
            .join("\n")
            .trim()
            .to_string()
    }

    /// Check for documentation indicators in content
    fn has_doc_indicators(&self, content: &str) -> bool {
        const DOC_INDICATORS: &[&str] = &[
            "@param", "@return", "@brief", "@author", "@date", "@version",
            "@file", "@see", "@throws", "@since", "@deprecated", "@todo",
            "\\param", "\\return", "\\brief", "\\author", "\\date",
            "\\version", "\\file", "\\throws", "\\see", "\\since",
            "\\deprecated", "\\todo",
        ];

        let content_lower = content.to_lowercase();
        DOC_INDICATORS
            .iter()
            .any(|indicator| content_lower.contains(&indicator.to_lowercase()))
    }

    /// Convert comment to Rust code
    fn convert_to_rust(&self, comment_info: &CommentInfo) -> Result<String> {
        match comment_info.comment_type.as_str() {
            "line" => self.convert_line(comment_info),
            "doc_line" | "doc_inner" => Ok(self.format_doc_comment(&comment_info.content)),
            "block" => self.convert_block(comment_info),
            "doc_block" => self.convert_doc_block(comment_info),
            "incomplete_block" => self.convert_incomplete_block(comment_info),
            _ => Ok(self.format_regular(&comment_info.content)),
        }
    }

    /// Convert single line comment to Rust
    fn convert_line(&self, comment_info: &CommentInfo) -> Result<String> {
        if comment_info.is_documentation {
            Ok(self.format_doc_comment(&comment_info.content))
        } else {
            Ok(self.format_regular(&comment_info.content))
        }
    }

    /// Convert block comment to Rust
    fn convert_block(&self, comment_info: &CommentInfo) -> Result<String> {
        if comment_info.content.is_empty() {
            return Ok("//".to_string());
        }

        let lines = self.split_and_clean_lines(&comment_info.content);

        if lines.len() == 1 {
            if comment_info.is_documentation {
                Ok(self.format_doc_comment(&lines[0]))
            } else {
                Ok(self.format_regular(&lines[0]))
            }
        } else {
            self.format_multiline(&lines, comment_info.is_documentation)
        }
    }

    /// Convert documentation block comment to Rust
    fn convert_doc_block(&self, comment_info: &CommentInfo) -> Result<String> {
        if comment_info.content.is_empty() {
            return Ok("///".to_string());
        }

        let lines = self.split_and_clean_lines(&comment_info.content);

        if lines.len() == 1 {
            Ok(self.format_doc_comment(&lines[0]))
        } else {
            self.format_multiline(&lines, true)
        }
    }

    /// Convert incomplete block comment to Rust
    fn convert_incomplete_block(&self, comment_info: &CommentInfo) -> Result<String> {
        let base_comment = if comment_info.is_documentation {
            self.format_doc_comment(&comment_info.content)
        } else {
            self.format_regular(&comment_info.content)
        };

        Ok(format!("{} // FIXME: Incomplete block comment", base_comment))
    }

    /// Helper: Format a single line documentation comment
    fn format_doc_comment(&self, content: &str) -> String {
        if content.is_empty() {
            "///".to_string()
        } else {
            format!("/// {}", content.trim())
        }
    }

    /// Helper: Format a single line regular comment
    fn format_regular(&self, content: &str) -> String {
        if content.is_empty() {
            "//".to_string()
        } else {
            format!("// {}", content.trim())
        }
    }

    /// Helper: Split content into lines and clean each line
    fn split_and_clean_lines(&self, content: &str) -> Vec<String> {
        content
            .lines()
            .map(|line| line.trim().to_string())
            .filter(|line| !line.is_empty())
            .collect()
    }

    /// Helper: Format multiple lines as comments
    fn format_multiline(&self, lines: &[String], is_doc: bool) -> Result<String> {
        let prefix = if is_doc { "/// " } else { "// " };
        let formatted_lines: Vec<String> = lines
            .iter()
            .map(|line| {
                if line.is_empty() {
                    prefix.trim_end().to_string()
                } else {
                    format!("{}{}", prefix, line)
                }
            })
            .collect();
        Ok(formatted_lines.join("\n"))
    }
}

impl Handler for CommentHandler {
    fn id(&self) -> Id {
        Id::get("comment_handler")
    }

    fn role(&self) -> String {
        "comment".to_string()
    }

    fn priority(&self) -> u64 {
        170
    }

    fn supported_patterns(&self) -> Vec<String> {
        vec![
            "single_line_comment".to_string(),
            "multi_line_comment".to_string(),
            "block_comment".to_string(),
            "documentation_comment".to_string(),
            "incomplete_block_comment".to_string(),
            "nested_comment".to_string(),
        ]
    }

    fn detect(
        &self,
        context: &mut Context,
        token_slot: usize,
        token_range: Range<usize>,
    ) -> Result<bool> {
        let tokens = context
            .tokenizer
            .get_tokens(token_slot, token_range.clone());

        if tokens.is_empty() {
            return Ok(false);
        }

        let (can_process, confidence) =
            self.detect_pattern(&mut context.patternizer, &context.samplizer, &tokens, token_range)?;

        context.registry.add_report(
            Report::new(
                Id::get(&format!("comment_handler_can_process_{}", token_slot)),
                Some(self.id()),
                "can_process".to_string(),
                format!(
                    "Comment detection: {} (confidence: {:.2})",
                    can_process, confidence
                ),
                ReportLevel::Info,
                Phase::Process(None),
            )
                .with_tokens(tokens.len(), if can_process { tokens.len() } else { 0 })
                .with_success(can_process),
        );

        Ok(can_process)
    }

    fn extract(
        &self,
        context: &mut Context,
        token_slot: usize,
        token_range: Range<usize>,
    ) -> Result<Option<ExtractedElement>> {
        let tokens = context
            .tokenizer
            .get_tokens(token_slot, token_range.clone());

        if tokens.is_empty() {
            return Ok(None);
        }

        let comment_info = self.extract_info(&tokens, &context.patternizer)?;

        let extracted_comment = ExtractedComment {
            id: Id::get(&gen_name(&self.id().name())),
            info: comment_info.clone(),
            tokens: tokens.clone(),
            token_range,
            metadata: vec![
                (
                    "comment_type".to_string(),
                    comment_info.comment_type.clone(),
                ),
                (
                    "is_documentation".to_string(),
                    comment_info.is_documentation.to_string(),
                ),
                (
                    "is_multiline".to_string(),
                    comment_info.is_multiline.to_string(),
                ),
                (
                    "line_count".to_string(),
                    comment_info.line_count.to_string(),
                ),
            ],
        };

        Ok(Some(ExtractedElement::Comment(extracted_comment)))
    }

    fn convert(
        &self,
        context: &mut Context,
        element: ExtractedElement,
    ) -> Result<Option<ConvertedElement>> {
        if let ExtractedElement::Comment(extracted) = element {
            let tokens = extracted.tokens.clone();
            if tokens.is_empty() {
                return Ok(None);
            }

            let comment_info = extracted.info.clone();
            let rust_code = self.convert_to_rust(&comment_info)?;

            let converted_comment = ConvertedComment {
                code: rust_code,
                metadata: vec![
                    (
                        "original_c_comment".to_string(),
                        comment_info.raw_comment.clone(),
                    ),
                    (
                        "rust_comment_style".to_string(),
                        if comment_info.is_documentation {
                            "doc"
                        } else {
                            "regular"
                        }
                            .to_string(),
                    ),
                    (
                        "conversion_method".to_string(),
                        comment_info.comment_type.clone(),
                    ),
                    ("preserved_formatting".to_string(), "true".to_string()),
                ],
            };

            Ok(Some(ConvertedElement::Comment(converted_comment)))
        } else {
            Ok(None)
        }
    }

    fn document(
        &self,
        _context: &mut Context,
        extracted_element: ExtractedElement,
        _converted_element: ConvertedElement,
    ) -> Result<Option<String>> {
        if let ExtractedElement::Comment(comment_info) = extracted_element {
            let preview = if comment_info.info.content.len() > 50 {
                format!(
                    "{}...",
                    comment_info
                        .info
                        .content
                        .chars()
                        .take(47)
                        .collect::<String>()
                )
            } else {
                comment_info.info.content.clone()
            };

            let doc = format!(
                "/// Comment Analysis\n/// Type: {}\n/// Content: {}\n/// Documentation: {}\n/// Lines: {}\n",
                comment_info.info.comment_type,
                preview,
                comment_info.info.is_documentation,
                comment_info.info.content.lines().count()
            );
            Ok(Some(doc))
        } else {
            Ok(None)
        }
    }

    fn report(&self, context: &mut Context) -> Result<Report> {
        let reports = context.registry.get_reports_by_handler(&self.id().name());

        if reports.is_empty() {
            return Ok(Report::new(
                Id::get("comment_handler_report"),
                Some(self.id()),
                "report".to_string(),
                "No comment processing reports found".to_string(),
                ReportLevel::Info,
                Phase::Report(None),
            ));
        }

        let (info_count, warning_count, error_count) =
            reports
                .iter()
                .fold((0, 0, 0), |(info, warn, err), report| match report.level {
                    ReportLevel::Info => (info + 1, warn, err),
                    ReportLevel::Warning => (info, warn + 1, err),
                    ReportLevel::Error => (info, warn, err + 1),
                    ReportLevel::Debug => (info + 1, warn, err),
                });

        let success_rate = if !reports.is_empty() {
            (reports.len() - error_count) as f64 / reports.len() as f64 * 100.0
        } else {
            100.0
        };

        Ok(Report::new(
            Id::get("comment_handler_report"),
            Some(self.id()),
            "report".to_string(),
            format!(
                "Comment handler: {} total reports, {} info, {} warnings, {} errors (success rate: {:.1}%)",
                reports.len(), info_count, warning_count, error_count, success_rate
            ),
            if error_count > 0 { ReportLevel::Warning } else { ReportLevel::Info },
            Phase::Report(None),
        )
            .with_tokens(reports.len(), error_count)
            .with_success(error_count == 0))
    }

    fn supported_keywords(&self) -> Vec<String> {
        vec!["//".to_string(), "/*".to_string()]
    }

    fn route(
        &self,
        context: &mut Context,
        _token_slot: usize,
        token_range: Range<usize>,
        result: HandlerResult,
    ) -> Result<HandlerResult> {
        match result {
            HandlerResult::NotHandled(_, _, _) => {
                let _redirect_request = RedirectRequest {
                    from_handler: self.id(),
                    token_range: token_range.clone(),
                    failed_patterns: self.supported_patterns(),
                    suggested_handler: Some(Id::get("string_handler")),
                    metadata: vec![
                        (
                            "reason".to_string(),
                            "comment_pattern_not_matched".to_string(),
                        ),
                        (
                            "fallback_strategy".to_string(),
                            "try_string_or_preprocessor_handler".to_string(),
                        ),
                        (
                            "token_analysis".to_string(),
                            "possible_string_literal_or_preprocessor_directive".to_string(),
                        ),
                    ],
                };

                report!(
                    context,
                    "comment_handler",
                    "redirect",
                    ReportLevel::Info,
                    Phase::Redirect(None),
                    "Redirecting unhandled comment-like tokens to string handler",
                    false
                );

                Ok(result)
            }
            _ => Ok(result),
        }
    }
}

impl Default for CommentHandler {
    fn default() -> Self {
        Self::new()
    }
}

pub fn create_comment_handler() -> CommentHandler {
    CommentHandler::new()
}
