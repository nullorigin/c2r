//! Comment handler for C to Rust comment conversion
//! Uses Patternizer exclusively for detection and conversion

use crate::{
    C2RError, CommentInfo, ConvertedComment, ConvertedElement, ElementInfo, ExtractedComment,
    ExtractedElement,
    HandlerPhase::{self, Report},
    HandlerReport, HandlerResult, Id, Kind, PatternResult, Reason,
    ReportLevel::{self, Info, Warning},
    Result, Token, context, not_handled, report,
};
use std::ops::Range;

/// Report callback: Collects comment handler reports
pub fn report_comment() -> Result<HandlerReport> {
    let context = context!();
    let reports = context.get_reports_by_handler("comment_handler");

    let (info_count, warning_count, error_count) =
        reports
            .iter()
            .fold((0, 0, 0), |acc, report| match report.level {
                ReportLevel::Error => (acc.0, acc.1, acc.2 + 1),
                ReportLevel::Warning => (acc.0, acc.1 + 1, acc.2),
                _ => (acc.0 + 1, acc.1, acc.2),
            });

    Ok(HandlerReport {
        report_id: Box::new(Id::get(&Id::gen_name("comment_handler"))),
        handler_id: Box::new(Id::get("comment_handler")),
        handler_name: "comment_handler".to_string(),
        function_name: "report_comment".to_string(),
        message: format!(
            "Comment handler summary: {} reports ({} info, {} warnings, {} errors)",
            reports.len(),
            info_count,
            warning_count,
            error_count
        ),
        level: match (error_count, warning_count) {
            (0, 0) => ReportLevel::Info,
            (0, _) => ReportLevel::Warning,
            _ => ReportLevel::Error,
        },
        tokens_processed: reports.len(),
        tokens_consumed: 0,
        phase: HandlerPhase::Report,
        success: error_count == 0,
        metadata: std::collections::HashMap::new(),
    })
}

/// Handle callback: Sets up patternizer and registers comment patterns
fn handle_comment(token_range: Range<usize>) -> Result<HandlerResult> {
    let mut context = crate::context!();
    context.pull();
    let tokens = &context.tokens[token_range.clone()];
    let filtered_tokens: Vec<Token> = tokens
        .iter()
        .filter(|token| !matches!(token, Token::n()))
        .cloned()
        .collect();
    let tokens = &filtered_tokens;

    // Register comment patterns in the global registry via context
    let comment_patterns = ["comment_line", "comment_block", "comment_doc", "comment"];

    for pattern in &comment_patterns {
        match context.patternizer.match_pattern(pattern, tokens) {
            PatternResult::Match { consumed_tokens } => {
                report!(
                    "comment_handler",
                    "handle_comment",
                    ReportLevel::Info,
                    HandlerPhase::Handle,
                    format!(
                        "Processing {} comment with {} tokens",
                        pattern, consumed_tokens
                    ),
                    true
                );

                // Convert using pattern data
                match convert_comment(token_range.clone())? {
                    Some(ConvertedElement::Comment(converted_comment)) => {
                        let code = converted_comment.code.clone();
                        return Ok(HandlerResult::Converted(
                            ConvertedElement::Comment(converted_comment),
                            0..consumed_tokens,
                            code,
                            Id::get("comment_handler"),
                        ));
                    }
                    _ => {
                        report!(
                            "comment_handler",
                            "handle_comment",
                            ReportLevel::Error,
                            HandlerPhase::Handle,
                            "Failed to convert comment",
                            false
                        );
                        return not_handled();
                    }
                }
            }
            _ => continue,
        }
    }

    not_handled()
}

/// Process callback: Uses Patternizer to detect comment patterns
fn process_comment(token_range: Range<usize>) -> Result<bool> {
    let mut context = crate::context!();
    context.pull();

    // Add bounds checking to prevent slice index out of bounds
    if token_range.end > context.tokens.len() || token_range.start >= context.tokens.len() {
        eprintln!(
            "🚨 comment_handler: token_range {:?} exceeds tokens length {}",
            token_range,
            context.tokens.len()
        );
        return Ok(false);
    }

    let tokens = &context.tokens[token_range.clone()];
    let filtered_tokens: Vec<Token> = tokens
        .iter()
        .filter(|token| !matches!(token, Token::n()))
        .cloned()
        .collect();
    let tokens = &filtered_tokens;
    if tokens.is_empty() {
        return Ok(false);
    }

    // Check for comment patterns
    let comment_patterns = ["comment_line", "comment_block", "comment_doc", "comment"];

    for pattern in &comment_patterns {
        if let PatternResult::Match { consumed_tokens: _ } =
            context.patternizer.match_pattern(pattern, tokens)
        {
            return Ok(true);
        }
    }

    // Fallback: check for comment markers
    let first_token = tokens[0].to_string();
    Ok(first_token.starts_with("//") || first_token.starts_with("/*"))
}

/// Creates a comment handler that uses Patternizer exclusively
pub fn create_comment_handler() -> crate::handler::Handler {
    let handler_id = Id::get("comment_handler");
    let handler_role = "comment";
    let priority = 200; // High priority to catch comments early

    super::create_handler(
        handler_id,
        handler_role,
        priority,
        Some(process_comment),
        Some(handle_comment),
        Some(extract_comment),
        Some(convert_comment),
        Some(document_comment),
        Some(report_comment),
        Some(result_comment),   // Result callback for final processing
        Some(redirect_comment), // Redirect callback for nested structures
    )
}

/// Result callback: Postprocesses comment conversion with documentation
fn result_comment(token_range: Range<usize>, result: HandlerResult) -> Result<HandlerResult> {
    let mut context = crate::context!();
    context.pull();
    let tokens = &context.tokens[token_range.clone()];
    let filtered_tokens: Vec<Token> = tokens
        .iter()
        .filter(|token| !matches!(token, crate::Token::n()))
        .cloned()
        .collect();
    let tokens = &filtered_tokens;
    let _id = Id::get("result_comment");

    report!(
        "comment_handler",
        "result_comment",
        Info,
        Report,
        "Postprocessing comment conversion result",
        true
    );

    match result {
        HandlerResult::Converted(element, token_range, code, id) => {
            // Extract comment information from tokens for documentation
            let comment_info = extract_comment_info(tokens);

            // Generate documentation about the comment conversion
            let doc_comment =
                document_comment(ElementInfo::Comment(comment_info.clone()))?.unwrap_or_default();

            // Enhance the Rust code with documentation and metadata
            let mut enhanced_code = String::new();

            // Add metadata comment for traceability
            let metadata_comment = format!(
                "// [C2R] Comment converted from C to Rust - {}: {}\n",
                comment_info.comment_type,
                comment_info
                    .content
                    .lines()
                    .next()
                    .unwrap_or("[empty]")
                    .chars()
                    .take(50)
                    .collect::<String>()
            );
            enhanced_code.push_str(&metadata_comment);

            // Add the documentation comment if substantial
            if !doc_comment.trim().is_empty() {
                enhanced_code.push_str(&doc_comment);
                enhanced_code.push('\n');
            }

            // Add the original converted code
            enhanced_code.push_str(&code);

            report!(
                "comment_handler",
                "result_comment",
                Info,
                Report,
                format!(
                    "Enhanced comment with {} lines of documentation",
                    doc_comment.lines().count()
                ),
                true
            );

            Ok(HandlerResult::Converted(
                element,
                token_range,
                enhanced_code,
                id,
            ))
        }
        _ => {
            report!(
                "comment_handler",
                "result_comment",
                Warning,
                Report,
                "Comment result was not in Completed state, returning as-is",
                true
            );

            Ok(result)
        }
    }
}

/// Extract comment information from tokens for result processing
fn extract_comment_info(tokens: &[Token]) -> CommentInfo {
    let mut content = String::new();
    let mut comment_type = String::new();
    let mut is_documentation = false;
    let mut is_multiline = false;
    let mut line_count = 1;

    // Basic parsing to extract comment information
    for token in tokens {
        match token {
            Token::s(text) if text.starts_with("//") || text.starts_with("/*") => {
                content = text.clone();
                if text.starts_with("/*") {
                    comment_type = "block comment".to_string();
                    is_multiline = text.contains('\n');
                    line_count = text.lines().count();
                } else if text.starts_with("//") {
                    comment_type = "line comment".to_string();
                }

                // Check if it's documentation
                if text.starts_with("/**")
                    || text.starts_with("/*!")
                    || text.contains("@param")
                    || text.contains("@return")
                {
                    is_documentation = true;
                }
            }
            _ => {}
        }
    }

    if content.is_empty() {
        content = "[empty comment]".to_string();
    }
    if comment_type.is_empty() {
        comment_type = "unknown comment".to_string();
    }

    CommentInfo {
        comment_type,
        content,
        is_documentation,
        is_multiline,
        line_count,
    }
}

/// Document callback: Generates documentation for comments
fn document_comment(info: ElementInfo) -> Result<Option<String>> {
    if let ElementInfo::Comment(comment_info) = info {
        let mut doc = format!(
            "/// Converted C comment: {}\n",
            comment_info.content.lines().next().unwrap_or("[empty]")
        );

        doc.push_str(&format!("/// Type: {}\n", comment_info.comment_type));

        if comment_info.is_documentation {
            doc.push_str("/// Category: Documentation\n");
        }

        if comment_info.is_multiline {
            doc.push_str(&format!("/// Lines: {}\n", comment_info.line_count));
        }

        if comment_info.content.len() > 100 {
            doc.push_str("/// Note: Long comment - may contain detailed explanation\n");
        }

        // Analyze content for special markers
        if comment_info.content.contains("TODO") {
            doc.push_str("/// Contains: TODO items\n");
        }
        if comment_info.content.contains("FIXME") {
            doc.push_str("/// Contains: FIXME items\n");
        }
        if comment_info.content.contains("NOTE") {
            doc.push_str("/// Contains: Note items\n");
        }

        Ok(Some(doc))
    } else {
        Ok(None)
    }
}

/// Redirect callback: Handles nested structures within comments (typically none)
#[allow(unused_variables)]
pub fn redirect_comment(token_range: Range<usize>, result: HandlerResult) -> Result<HandlerResult> {
    report!(
        "comment_handler",
        "redirect_comment",
        Info,
        HandlerPhase::Convert,
        "Checking comment for nested structures (typically none expected)",
        true
    );

    // Comments typically don't contain nested structures that need redirection
    // This is mostly a no-op but included for consistency with other handlers
    // Comments are self-contained and don't usually need to delegate processing

    report!(
        "comment_handler",
        "redirect_comment",
        Info,
        HandlerPhase::Convert,
        "No redirection needed for comment, returning unchanged",
        true
    );

    Ok(result)
}

/// Extract comment information from tokens using Patternizer exclusively
fn extract_comment(token_range: Range<usize>) -> Result<Option<ExtractedElement>> {
    let mut context = crate::context!();
    context.pull();
    let tokens = &context.tokens[token_range.clone()];
    let filtered_tokens: Vec<Token> = tokens
        .iter()
        .filter(|token| !matches!(token, Token::n()))
        .cloned()
        .collect();
    let tokens = &filtered_tokens;

    // Try to match comment patterns
    let comment_patterns = ["comment_line", "comment_block", "comment_doc", "comment"];

    for pattern in &comment_patterns {
        match context.patternizer.match_pattern(pattern, tokens) {
            PatternResult::Match { consumed_tokens: _ } => {
                // Extract comment components
                let (content, comment_type) = parse_comment_tokens(tokens)?;

                let extracted_comment = ExtractedComment {
                    code: content,
                    is_block: comment_type == "block",
                    is_doc_comment: comment_type == "doc",
                };

                return Ok(Some(ExtractedElement::Comment(extracted_comment)));
            }
            _ => continue,
        }
    }

    // Fallback for basic comment detection
    if tokens.len() >= 1 {
        let first_token = tokens[0].to_string();
        if first_token.starts_with("//") || first_token.starts_with("/*") {
            let (content, comment_type) = parse_comment_tokens(tokens)?;

            let extracted_comment = ExtractedComment {
                code: content,
                is_block: comment_type == "block",
                is_doc_comment: comment_type == "doc",
            };

            return Ok(Some(ExtractedElement::Comment(extracted_comment)));
        }
    }

    Ok(None)
}

/// Helper function to parse comment tokens
fn parse_comment_tokens(tokens: &[Token]) -> Result<(String, String)> {
    if tokens.is_empty() {
        return Err(C2RError::new(
            Kind::Other,
            Reason::Unexpected("empty comment token list"),
            None,
        ));
    }

    let full_comment = tokens
        .iter()
        .map(|t| t.to_string())
        .collect::<Vec<_>>()
        .join(" ");

    let comment_type = if full_comment.starts_with("//") {
        "line".to_string()
    } else if full_comment.starts_with("/*") {
        "block".to_string()
    } else {
        "unknown".to_string()
    };

    // Extract comment content (remove comment markers)
    let content = if full_comment.starts_with("//") {
        full_comment
            .strip_prefix("//")
            .unwrap_or(&full_comment)
            .trim()
            .to_string()
    } else if full_comment.starts_with("/*") && full_comment.ends_with("*/") {
        let stripped = full_comment.strip_prefix("/*").unwrap_or(&full_comment);
        let stripped = stripped.strip_suffix("*/").unwrap_or(stripped);
        stripped.trim().to_string()
    } else {
        full_comment
    };

    Ok((content, comment_type))
}

/// Convert extracted comment to Rust code using Patternizer exclusively
fn convert_comment(token_range: Range<usize>) -> Result<Option<ConvertedElement>> {
    // Use cloned context to get the actual tokens from global context
    let mut context = crate::context!();
    context.pull();
    let tokens_in_range = &context.tokens[token_range.clone()];

    // Filter out Token::n() from the retrieved tokens
    let filtered_tokens: Vec<Token> = tokens_in_range
        .iter()
        .filter(|token| !matches!(token, Token::n()))
        .cloned()
        .collect();

    let mut processed_tokens = filtered_tokens.to_vec();

    // Try to match comment patterns
    let comment_patterns = ["comment_line", "comment_block", "comment_doc", "comment"];

    for pattern in &comment_patterns {
        match context.patternizer.match_pattern(pattern, &filtered_tokens) {
            PatternResult::Match { consumed_tokens } => {
                // Parse comment components
                let (content, comment_type) = parse_comment_tokens(&filtered_tokens)?;

                // Mark processed tokens as consumed
                for i in 0..consumed_tokens.min(processed_tokens.len()) {
                    processed_tokens[i] = Token::n();
                }

                let code = convert_comment_to_rust(&content, &comment_type)?;

                let converted_comment = ConvertedComment {
                    code,
                    is_block: comment_type == "block",
                    is_doc_comment: comment_type == "doc",
                };

                return Ok(Some(ConvertedElement::Comment(converted_comment)));
            }
            _ => continue,
        }
    }

    // Fallback conversion for basic comments
    if filtered_tokens.len() >= 1 {
        let first_token = filtered_tokens[0].to_string();
        if first_token.starts_with("//") || first_token.starts_with("/*") {
            let (content, comment_type) = parse_comment_tokens(&filtered_tokens)?;
            let code = convert_comment_to_rust(&content, &comment_type)?;

            let converted_comment = ConvertedComment {
                code,
                is_block: comment_type == "block",
                is_doc_comment: comment_type == "doc",
            };

            return Ok(Some(ConvertedElement::Comment(converted_comment)));
        }
    }

    Ok(None)
}

/// Convert comment to Rust syntax
fn convert_comment_to_rust(content: &str, comment_type: &str) -> Result<String> {
    match comment_type {
        "line" => {
            // C line comment -> Rust line comment
            Ok(format!("// {}", content))
        }
        "block" => {
            // C block comment -> Rust block comment or line comment
            if content.contains('\n') {
                // Multi-line block comment
                let lines: Vec<&str> = content.split('\n').collect();
                let rust_lines: Vec<String> = lines
                    .iter()
                    .map(|line| format!("// {}", line.trim()))
                    .collect();
                Ok(rust_lines.join("\n"))
            } else {
                // Single-line block comment -> line comment
                Ok(format!("// {}", content))
            }
        }
        "doc" => {
            // Documentation comment
            Ok(format!("/// {}", content))
        }
        _ => {
            // Unknown comment type, default to line comment
            Ok(format!("// {}", content))
        }
    }
}
