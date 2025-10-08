//! Comment handler implementation using trait-based system
//! Handles C-style comments (/* */) and C++-style comments (//)

use crate::info::CommentInfo;
use crate::{
    Context, ConvertedComment, ConvertedElement, ElementInfo, ExtractedComment, ExtractedElement, Handler, HandlerPhase, HandlerReport, HandlerResult, Handlizer, Id, RedirectRequest, ReportLevel, Result
};
use crate::report;
use std::ops::Range;

/// Comment handler implementing the Handler trait
#[derive(Debug, Clone)]
pub struct CommentHandler;

impl CommentHandler {
    pub fn new() -> Self {
        Self
    }

    /// Enhanced pattern detection using both Patternizer and Samplizer
    fn detect_comment_pattern(
        &self,
        context: &mut Context,
        tokens: &[crate::Token],
        token_range: Range<usize>,
    ) -> Result<(bool, f64)> {
        // First use Patternizer for structural detection
        let pattern_match = context.patternizer.match_pattern("comment", tokens);

        // Then use Samplizer for confidence scoring
        let patterns = vec![
            "single_line_comment".to_string(),
            "multi_line_comment".to_string(),
            "block_comment".to_string(),
            "documentation_comment".to_string(),
        ];
        let confidence = context.samplizer.analyze_with_range(&patterns, tokens, token_range)?;

        // Combined heuristics for comment detection
        let has_comment_structure = self.has_comment_structure(tokens);
        let enhanced_confidence = if has_comment_structure {
            confidence * 1.5 // Boost confidence if structure looks comment-like
        } else {
            confidence * 0.6 // Reduce if structure doesn't match
        };

        let pattern_success = !matches!(pattern_match, crate::PatternResult::NoMatch { .. });

        Ok((
            pattern_success || enhanced_confidence > 0.7,
            enhanced_confidence.min(1.0),
        ))
    }

    /// Analyze token structure for comment patterns
    fn has_comment_structure(&self, tokens: &[crate::Token]) -> bool {
        if tokens.is_empty() {
            return false;
        }

        let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        let full_text = token_strings.join("");

        // Check for C-style single line comments
        if full_text.starts_with("//") {
            return true;
        }

        // Check for C-style block comments
        if full_text.starts_with("/*") {
            return true;
        }

        // Check for documentation comments
        if full_text.starts_with("/**") || full_text.starts_with("///") {
            return true;
        }

        false
    }

    /// Extract comment information from tokens
    fn extract_comment_info(&self, tokens: &[crate::Token]) -> Result<CommentInfo> {
        let full_text = tokens
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<_>>()
            .join("");

        let comment_type = self.determine_comment_type(&full_text);
        let content = self.extract_comment_content(&full_text, &comment_type);
        let is_documentation = self.is_documentation_comment(&full_text, &content);
        let is_multiline = full_text.contains("\n");
        let line_count = full_text.lines().count();
        Ok(CommentInfo {
            raw_comment: full_text,
            comment_type,
            content,
            is_documentation,
            is_multiline,
            line_count
        })
    }

    /// Determine the type of comment
    fn determine_comment_type(&self, text: &str) -> String {
        if text.starts_with("/**") {
            "doc_block".to_string()
        } else if text.starts_with("///") {
            "doc_line".to_string()
        } else if text.starts_with("//") {
            "line".to_string()
        } else if text.starts_with("/*") && text.ends_with("*/") {
            "block".to_string()
        } else if text.starts_with("/*") {
            "incomplete_block".to_string()
        } else {
            "unknown".to_string()
        }
    }

    /// Extract the actual content from the comment
    fn extract_comment_content(&self, text: &str, comment_type: &str) -> String {
        match comment_type {
            "line" => text.strip_prefix("//").unwrap_or(text).trim().to_string(),
            "doc_line" => text.strip_prefix("///").unwrap_or(text).trim().to_string(),
            "block" | "doc_block" => {
                let content = if text.starts_with("/**") {
                    text.strip_prefix("/**").unwrap_or(text)
                } else {
                    text.strip_prefix("/*").unwrap_or(text)
                };
                content.strip_suffix("*/").unwrap_or(content).trim().to_string()
            }
            "incomplete_block" => text.strip_prefix("/*").unwrap_or(text).trim().to_string(),
            _ => text.trim().to_string(),
        }
    }

    /// Check if comment looks like documentation
    fn is_documentation_comment(&self, raw_text: &str, content: &str) -> bool {
        // Check for explicit documentation comment syntax
        if raw_text.starts_with("/**") || raw_text.starts_with("///") {
            return true;
        }

        // Check for documentation indicators
        let doc_indicators = [
            "@param", "@return", "@brief", "@author", "@date", "@version", "@file",
            "\\param", "\\return", "\\brief", "\\author", "\\date", "\\version", "\\file",
            "@throws", "@see", "@since", "@deprecated", "@todo",
            "\\throws", "\\see", "\\since", "\\deprecated", "\\todo",
        ];

        doc_indicators
            .iter()
            .any(|indicator| content.contains(indicator))
    }

    /// Convert comment to Rust code
    fn convert_to_rust(&self, comment_info: &CommentInfo) -> Result<String> {
        match comment_info.comment_type.as_str() {
            "line" => self.convert_line_comment(comment_info),
            "doc_line" => Ok(format!("/// {}", comment_info.content)),
            "block" => self.convert_block_comment(comment_info),
            "doc_block" => self.convert_doc_block_comment(comment_info),
            "incomplete_block" => self.convert_incomplete_block_comment(comment_info),
            _ => Ok(format!("// {}", comment_info.content)),
        }
    }

    /// Convert single line comment to Rust
    fn convert_line_comment(&self, comment_info: &CommentInfo) -> Result<String> {
        if comment_info.is_documentation {
            Ok(format!("/// {}", comment_info.content))
        } else {
            Ok(format!("// {}", comment_info.content))
        }
    }

    /// Convert block comment to Rust
    fn convert_block_comment(&self, comment_info: &CommentInfo) -> Result<String> {
        let lines: Vec<&str> = comment_info.content.lines().collect();
        
        if lines.is_empty() {
            return Ok("//".to_string());
        }

        if lines.len() == 1 {
            let prefix = if comment_info.is_documentation { "/// " } else { "// " };
            Ok(format!("{}{}", prefix, lines[0].trim()))
        } else {
            let prefix = if comment_info.is_documentation { "/// " } else { "// " };
            let rust_lines: Vec<String> = lines
                .iter()
                .map(|line| format!("{}{}", prefix, line.trim()))
                .collect();
            Ok(rust_lines.join("\n"))
        }
    }

    /// Convert documentation block comment to Rust
    fn convert_doc_block_comment(&self, comment_info: &CommentInfo) -> Result<String> {
        let lines: Vec<&str> = comment_info.content.lines().collect();
        
        if lines.is_empty() {
            return Ok("///".to_string());
        }

        if lines.len() == 1 {
            Ok(format!("/// {}", lines[0].trim()))
        } else {
            let rust_lines: Vec<String> = lines
                .iter()
                .map(|line| format!("/// {}", line.trim()))
                .collect();
            Ok(rust_lines.join("\n"))
        }
    }

    /// Convert incomplete block comment to Rust
    fn convert_incomplete_block_comment(&self, comment_info: &CommentInfo) -> Result<String> {
        let prefix = if comment_info.is_documentation { "/// " } else { "// " };
        Ok(format!(
            "{}{} // FIXME: Incomplete block comment",
            prefix,
            comment_info.content
        ))
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

    fn can_process(
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

        let (can_process, confidence) = self.detect_comment_pattern(context, &tokens, token_range)?;

        report!(
            context,
            "comment_handler",
            "can_process",
            ReportLevel::Info,
            HandlerPhase::Process,
            &format!(
                "Comment detection: {} (confidence: {:.2})",
                can_process, confidence
            ),
            can_process
        );

        Ok(can_process)
    }

    fn process(
        &self,
        context: &mut Context,
        token_slot: usize,
        token_range: Range<usize>,
    ) -> Result<HandlerResult> {
        let tokens = context
            .tokenizer
            .get_tokens(token_slot, token_range.clone());

        if tokens.is_empty() {
            return Ok(HandlerResult::NotHandled(None, token_range, self.id()));
        }

        context.registry.add_report(
            HandlerReport::new(
                &format!("comment_handler_process_{}", token_slot),
                std::sync::Arc::new(self.id()),
                "Comment Handler (Trait)".to_string(),
                "process".to_string(),
                format!("Processing comment with {} tokens", tokens.len()),
                ReportLevel::Info,
                HandlerPhase::Process,
            )
            .with_tokens(tokens.len(), tokens.len())
            .with_success(true),
        );

        let comment_info = self.extract_comment_info(&tokens)?;
        let rust_code = self.convert_to_rust(&comment_info)?;

        Ok(HandlerResult::Processed(
            Some(tokens),
            token_range,
            rust_code,
            self.id(),
        ))
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

        let comment_info = self.extract_comment_info(&tokens)?;
        let rust_code = self.convert_to_rust(&comment_info)?;

        let extracted_comment = ExtractedComment {
            id: Id::get("comment_handler"),
            code: rust_code,
            info: comment_info.clone(),
            tokens,
            token_range,
            metadata: vec![],
        };
        Ok(Some(ExtractedElement::Comment(extracted_comment)))
    }

    fn convert(
        &self,
        context: &mut Context,
        token_slot: usize,
        token_range: Range<usize>,
    ) -> Result<Option<ConvertedElement>> {
        let tokens = context
            .tokenizer
            .get_tokens(token_slot, token_range.clone());

        if tokens.is_empty() {
            return Ok(None);
        }

        let comment_info = self.extract_comment_info(&tokens)?;
        let rust_code = self.convert_to_rust(&comment_info)?;

        let converted_comment = ConvertedComment {
            is_block: comment_info.comment_type == "block" || comment_info.comment_type == "doc_block" || comment_info.comment_type == "incomplete_block",
            is_doc_comment: comment_info.is_documentation,
            code: rust_code,
            metadata: vec![
                ("original_c_comment".to_string(), comment_info.raw_comment.clone()),
                ("rust_comment_style".to_string(), if comment_info.is_documentation { "doc" } else { "regular" }.to_string()),
                ("conversion_method".to_string(), comment_info.comment_type.clone()),
                ("preserved_formatting".to_string(), "true".to_string()),
            ],
        };

        Ok(Some(ConvertedElement::Comment(converted_comment)))
    }

    fn document(&self, _context: &mut Context, info: ElementInfo) -> Result<Option<String>> {
        if let ElementInfo::Comment(comment_info) = info {
            let preview = if comment_info.content.len() > 50 {
                format!("{}...", comment_info.content.chars().take(47).collect::<String>())
            } else {
                comment_info.content.clone()
            };

            let doc = format!(
                "/// Comment Analysis\n/// Type: {}\n/// Content: {}\n/// Documentation: {}\n/// Lines: {}\n",
                comment_info.comment_type,
                preview,
                comment_info.is_documentation,
                comment_info.content.lines().count()
            );
            Ok(Some(doc))
        } else {
            Ok(None)
        }
    }

    fn report(&self, context: &mut Context) -> Result<HandlerReport> {
        let reports = context
            .registry
            .get_reports_by_handler("comment_handler");
        
        if reports.is_empty() {
            return Ok(HandlerReport::new(
                "comment_handler_report",
                std::sync::Arc::new(self.id()),
                "Comment Handler (Trait)".to_string(),
                "report".to_string(),
                "No comment processing reports found".to_string(),
                ReportLevel::Info,
                HandlerPhase::Report,
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

        Ok(HandlerReport::new(
            "comment_handler_report",
            std::sync::Arc::new(self.id()),
            "Comment Handler (Trait)".to_string(),
            "report".to_string(),
            format!(
                "Comment handler: {} total reports, {} info, {} warnings, {} errors (success rate: {:.1}%)",
                reports.len(), info_count, warning_count, error_count, success_rate
            ),
            if error_count > 0 { ReportLevel::Warning } else { ReportLevel::Info },
            HandlerPhase::Report,
        )
        .with_tokens(reports.len(), error_count)
        .with_success(error_count == 0)
        .with_metadata("total_reports".to_string(), reports.len().to_string())
        .with_metadata("success_rate".to_string(), format!("{:.1}", success_rate))
        .with_metadata("comment_types_processed".to_string(), "line,block,doc".to_string()))
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

    fn handle_redirect(
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
                        ("reason".to_string(), "comment_pattern_not_matched".to_string()),
                        ("fallback_strategy".to_string(), "try_string_or_preprocessor_handler".to_string()),
                        ("token_analysis".to_string(), "possible_string_literal_or_preprocessor_directive".to_string()),
                    ],
                };

                report!(
                    context,
                    "comment_handler",
                    "handle_redirect",
                    ReportLevel::Info,
                    HandlerPhase::Redirect,
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
