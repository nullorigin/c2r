use super::common::{not_handled, replace_with_range};
use crate::config::HandlerPhase::{Convert, Extract, Handle, Process, Report};
use crate::config::HandlerReport;
use crate::config::ReportLevel::{Info, Warning};
use crate::config::{HandlerPhase, ReportLevel};
use crate::error::ConversionError;
use crate::extract::{ExtractedComment, ExtractedElement};
use crate::handler::HandlerResult;
use crate::lock::Id;
use crate::Token;
use crate::{context, report};
use crate::{ConvertedComment, ConvertedElement};

/// Creates a comment handler that can detect and convert C comments
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
        Some(convert_comment_callback),
        Some(report_comment),
        Some(result_comment),
        Some(redirect_comment),
    )
}

/// Determines if the token sequence represents a comment
fn process_comment(tokens: &[Token]) -> Result<bool, ConversionError> {
    let _id = Id::get("process_comment");
    if tokens.is_empty() {
        return Ok(false);
    }

    // Check for inline comments: //...
    if tokens[0].to_string() == "//" {
        return Ok(true);
    }

    // Check for block comments: /* ... */
    if tokens.len() >= 3 && tokens[0].to_string() == "/*" {
        // Find the matching closing token
        for token in tokens.iter().skip(1) {
            if token.to_string() == "*/" {
                return Ok(true);
            }
        }
    }

    Ok(false)
}

/// Handles the conversion of comments
fn handle_comment(
    tokens: &[Token]) -> Result<HandlerResult, ConversionError> {
    let _id = Id::get("handle_comment");
    report!(
        "comment_handler",
        "handle_comment",
        Info,
        Handle,
        "Comment handler processing tokens",
        true
    );

    if tokens.is_empty() {
        return not_handled();
    }

    // Handle inline comments: //...
    if tokens[0].to_string() == "//" {
        return handle_inline_comment(tokens);
    }

    // Handle block comments: /* ... */
    if tokens.len() >= 3 && tokens[0].to_string() == "/*" {
        return handle_block_comment(tokens);
    }

    not_handled()
}

/// Handles inline comments: //...
fn handle_inline_comment(
    tokens: &[Token]) -> Result<HandlerResult, ConversionError> {
    if tokens.len() < 2 {
        return not_handled();
    }

    report!(
        "comment_handler",
        "handle_comment",
        Info,
        Process,
        "Processing inline comment",
        true
    );

    // Extract the comment content
    let mut comment = String::new();

    for token in tokens.iter().skip(1) {
        comment.push_str(&token.to_string());
        comment.push(' ');
    }

    // Convert to Rust comment
    let rust_comment = convert_comment(&format!("//{}", comment));

    let id = Id::get("inline_comment");
    // Inline comments process all input tokens
    let token_range = 0..tokens.len();
    replace_with_range(rust_comment, token_range, id)
}

/// Handles block comments: /* ... */
fn handle_block_comment(
    tokens: &[Token]) -> Result<HandlerResult, ConversionError> {
    if tokens.len() < 3 {
        return not_handled();
    }

    report!(
        "comment_handler",
        "handle_comment",
        Info,
        Process,
        "Processing block comment",
        true
    );

    // Find the end of the comment
    let mut end_pos = 0;
    for (i, token) in tokens.iter().enumerate().skip(1) {
        if token.to_string() == "*/" {
            end_pos = i;
            break;
        }
    }

    if end_pos == 0 {
        return Err(ConversionError::new("Unclosed block comment"));
    }

    // Extract the comment content
    let mut comment = String::new();
    for token in tokens.iter().skip(1).take(end_pos - 1) {
        comment.push_str(&token.to_string());
        comment.push(' ');
    }

    // Convert to Rust comment
    let rust_comment = convert_comment(&format!("/* {} */", comment));

    let id = Id::get("block_comment");
    // Block comments process all input tokens
    let token_range = 0..tokens.len();
    replace_with_range(rust_comment, token_range, id)
}

/// Converts a C comment to Rust comment
fn convert_comment(comment: &str) -> String {
    // Remove C-style comment delimiters
    let comment = comment.trim();

    // Handle block comments /* ... */
    if comment.starts_with("/*") && comment.ends_with("*/") {
        let inner = &comment[2..comment.len() - 2].trim();

        // Check if it's a documentation comment
        if inner.starts_with("*") {
            // Convert to Rust doc comment format
            let doc_lines: Vec<&str> = inner.lines().collect();
            if doc_lines.len() > 1 {
                // Multi-line doc comment
                let formatted_lines: Vec<String> = doc_lines
                    .iter()
                    .map(|line| {
                        let trimmed = line.trim_start().trim_start_matches('*').trim_start();
                        format!("/// {}", trimmed)
                    })
                    .collect();
                formatted_lines.join("\n")
            } else {
                // Single line doc comment
                let content = inner.trim_start().trim_start_matches('*').trim();
                format!("/// {}", content)
            }
        } else {
            // Regular block comment
            // Check if it's multi-line
            if inner.contains('\n') {
                format!("/* {} */", inner)
            } else {
                format!("// {}", inner)
            }
        }
    }
    // Handle line comments // ...
    else if comment.starts_with("//") {
        let inner = &comment[2..].trim();

        // Check if it's a documentation comment
        if inner.starts_with("/") {
            // Triple slash doc comment in C
            let content = inner.trim_start_matches('/').trim();
            format!("/// {}", content)
        } else {
            // Regular line comment
            format!("// {}", inner)
        }
    } else {
        // Return the comment as-is if it doesn't match known patterns
        comment.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_block_comment_conversion() {
        assert_eq!(convert_comment("/* Simple comment */"), "// Simple comment");
        assert_eq!(
            convert_comment("/* Multi-\nline\ncomment */"),
            "/* Multi-\nline\ncomment */"
        );
    }

    #[test]
    fn test_line_comment_conversion() {
        assert_eq!(convert_comment("// Simple comment"), "// Simple comment");
    }

    #[test]
    fn test_doc_comment_conversion() {
        assert_eq!(
            convert_comment("/* * Documentation */"),
            "/// Documentation"
        );
        assert_eq!(
            convert_comment("/* * Line 1\n * Line 2 */"),
            "/// Line 1\n/// Line 2"
        );
        assert_eq!(convert_comment("/// Doc comment"), "/// Doc comment");
    }
}

/// Extract comment information from tokens
fn extract_comment(
    tokens: &[Token]) -> Result<Option<ExtractedElement>, ConversionError> {
    let _id = Id::get("extract_comment");
    report!(
        "comment_handler",
        "extract_comment",
        Info,
        Extract,
        "Extracting comment from tokens",
        true
    );

    if tokens.is_empty() {
        return Ok(None);
    }

    // Handle inline comments: //...
    if tokens[0].to_string() == "//" {
        return extract_inline_comment(tokens);
    }

    // Handle block comments: /* ... */
    if tokens.len() >= 3 && tokens[0].to_string() == "/*" {
        return extract_block_comment(tokens);
    }

    Ok(None)
}

/// Extract inline comment information
fn extract_inline_comment(
    tokens: &[Token]) -> Result<Option<ExtractedElement>, ConversionError> {
    if tokens.len() < 2 {
        return Ok(None);
    }

    report!(
        "comment_handler",
        "extract_comment",
        Info,
        Extract,
        "Extracting inline comment",
        true
    );

    // Extract the comment content
    let mut content = String::new();
    for token in tokens.iter().skip(1) {
        content.push_str(&token.to_string());
        content.push(' ');
    }
    content = content.trim().to_string();

    let is_doc_comment =
        content.starts_with('/') || content.contains("@param") || content.contains("@return");

    let extracted = ExtractedComment {
        content,
        is_block: false,
        is_doc_comment,
    };

    Ok(Some(ExtractedElement::Comment(extracted)))
}

/// Extract block comment information
fn extract_block_comment(
    tokens: &[Token]) -> Result<Option<ExtractedElement>, ConversionError> {
    if tokens.len() < 3 {
        return Ok(None);
    }

    report!(
        "comment_handler",
        "extract_comment",
        Info,
        Extract,
        "Extracting block comment",
        true
    );

    // Find the end of the comment
    let mut end_pos = 0;
    for (i, token) in tokens.iter().enumerate().skip(1) {
        if token.to_string() == "*/" {
            end_pos = i;
            break;
        }
    }

    if end_pos == 0 {
        return Err(ConversionError::new("Unclosed block comment"));
    }

    // Extract the comment content
    let mut content = String::new();
    for token in tokens.iter().skip(1).take(end_pos - 1) {
        content.push_str(&token.to_string());
        content.push(' ');
    }
    content = content.trim().to_string();

    let is_doc_comment =
        content.starts_with('*') || content.contains("@param") || content.contains("@return");

    let extracted = ExtractedComment {
        content,
        is_block: true,
        is_doc_comment,
    };

    Ok(Some(ExtractedElement::Comment(extracted)))
}

/// Convert extracted comment to Rust code - callback version to avoid name conflict
fn convert_comment_callback(
    tokens: &[Token]) -> Result<Option<ConvertedElement>, ConversionError> {
    let _id = Id::get("convert_comment_callback");
    report!(
        "comment_handler",
        "convert_comment_callback",
        Info,
        Convert,
        "Converting comment to Rust",
        true
    );

    // First extract the comment information from tokens
    if let Some(ExtractedElement::Comment(comment)) = extract_comment(tokens)? {
        let rust_code = if comment.is_block {
            convert_comment(&format!("/* {} */", comment.content))
        } else {
            convert_comment(&format!("// {}", comment.content))
        };

        let converted = ConvertedComment {
            content: comment.content.clone(),
            rust_code,
            is_block: comment.is_block,
            is_doc_comment: comment.is_doc_comment,
        };

        Ok(Some(ConvertedElement::Comment(converted)))
    } else {
        Ok(None)
    }
}

/// Handle redirection for comment processing
fn redirect_comment(
    tokens: &[Token],
    result: HandlerResult) -> Result<HandlerResult, ConversionError> {
    let _id = Id::get("redirect_comment");
    report!(
        "comment_handler",
        "redirect_comment",
        Info,
        Report,
        "Redirecting comment handler result",
        true
    );

    // Comments are typically handled independently and don't need redirection
    // They could potentially be redirected to documentation generators
    Ok(result)
}

/// Result callback: Postprocesses generated comment code, adds documentation, and enhances formatting
fn result_comment(
    tokens: &[Token],
    result: HandlerResult) -> Result<HandlerResult, ConversionError> {
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
        HandlerResult::Completed(tokens_opt, _, rust_code, id) => {
            // Extract comment information for documentation
            let (comment_type, comment_content) = extract_comment_info_from_tokens(tokens);

            // Generate documentation about the comment conversion
            let doc_comment =
                generate_comment_documentation(tokens, &comment_type, &comment_content);

            // Enhance the Rust code with documentation and metadata
            let mut enhanced_code = String::new();

            // Add metadata comment for traceability
            let metadata_comment = format!(
                "// [C2R] Comment converted from C to Rust - Type: {}, Length: {} chars\n",
                comment_type,
                comment_content.len()
            );
            enhanced_code.push_str(&metadata_comment);

            // Add the documentation comment if substantial
            if !doc_comment.trim().is_empty() {
                enhanced_code.push_str(&doc_comment);
                enhanced_code.push('\n');
            }

            // Add the original converted code
            enhanced_code.push_str(&rust_code);

            report!(
                "comment_handler",
                "result_comment",
                Info,
                Report,
                &format!(
                    "Enhanced comment conversion: {} type, {} chars",
                    comment_type,
                    comment_content.len()
                ),
                true
            );

            // CRITICAL: Return HandlerResult::Completed to preserve the code!
            Ok(HandlerResult::Completed(
                tokens_opt,
                0..1,
                enhanced_code,
                id,
            ))
        }
        HandlerResult::Converted(element, _, rust_code, id) => {
            // Handle converted elements - enhance the code and preserve the variant
            let (comment_type, comment_content) = extract_comment_info_from_tokens(tokens);
            let doc_comment =
                generate_comment_documentation(tokens, &comment_type, &comment_content);

            let mut enhanced_code = String::new();
            let metadata_comment = format!(
                "// [C2R] Comment converted from C to Rust - Type: {}, Length: {} chars\n",
                comment_type,
                comment_content.len()
            );
            enhanced_code.push_str(&metadata_comment);

            if !doc_comment.trim().is_empty() {
                enhanced_code.push_str(&doc_comment);
                enhanced_code.push('\n');
            }
            enhanced_code.push_str(&rust_code);

            report!(
                "comment_handler",
                "result_comment",
                Info,
                Report,
                &format!(
                    "Enhanced converted comment: {} type, {} chars",
                    comment_type,
                    comment_content.len()
                ),
                true
            );

            // CRITICAL: Return HandlerResult::Converted to preserve the code!
            Ok(HandlerResult::Converted(element, 0..1, enhanced_code, id))
        }
        HandlerResult::Extracted(element, _, rust_code, id) => {
            // Handle extracted elements - enhance the code and preserve the variant
            let (comment_type, comment_content) = extract_comment_info_from_tokens(tokens);
            let doc_comment =
                generate_comment_documentation(tokens, &comment_type, &comment_content);

            let mut enhanced_code = String::new();
            let metadata_comment = format!(
                "// [C2R] Comment converted from C to Rust - Type: {}, Length: {} chars\n",
                comment_type,
                comment_content.len()
            );
            enhanced_code.push_str(&metadata_comment);

            if !doc_comment.trim().is_empty() {
                enhanced_code.push_str(&doc_comment);
                enhanced_code.push('\n');
            }
            enhanced_code.push_str(&rust_code);

            report!(
                "comment_handler",
                "result_comment",
                Info,
                Report,
                &format!(
                    "Enhanced extracted comment: {} type, {} chars",
                    comment_type,
                    comment_content.len()
                ),
                true
            );

            // CRITICAL: Return HandlerResult::Extracted to preserve the code!
            Ok(HandlerResult::Extracted(element, 0..1, enhanced_code, id))
        }
        HandlerResult::Handled(Some(converted_tokens), _, handler_id) => {
            // Legacy support for token-based results
            // Extract comment information for documentation
            let (comment_type, comment_content) = extract_comment_info_from_tokens(tokens);

            // Generate documentation about the comment conversion
            let doc_comment =
                generate_comment_documentation(tokens, &comment_type, &comment_content);

            // Postprocess the converted Rust code for better formatting
            let mut enhanced_result = postprocess_comment_code(converted_tokens);

            // Add the documentation comment before the converted comment
            if !doc_comment.is_empty() {
                enhanced_result.insert(0, Token::s(doc_comment));
            }

            // Add metadata comment for traceability
            let metadata_comment = format!(
                "// [C2R] Comment converted from C to Rust - Type: {}, Length: {} chars",
                comment_type,
                comment_content.len()
            );
            enhanced_result.insert(0, Token::s(metadata_comment));

            report!(
                "comment_handler",
                "result_comment",
                Info,
                Report,
                &format!(
                    "Enhanced comment conversion: {} type, {} chars",
                    comment_type,
                    comment_content.len()
                ),
                true
            );

            Ok(HandlerResult::Handled(
                Some(enhanced_result),
                0..1,
                handler_id,
            ))
        }
        HandlerResult::Handled(None, _, handler_id) => {
            report!(
                "comment_handler",
                "result_comment",
                Warning,
                Report,
                "Comment handler returned empty result",
                true
            );
            Ok(HandlerResult::Handled(None, 0..0, handler_id))
        }
        other_result => {
            // Pass through non-handled results unchanged
            Ok(other_result)
        }
    }
}

/// Extracts comment information from the original tokens for documentation purposes
fn extract_comment_info_from_tokens(tokens: &[Token]) -> (String, String) {
    if tokens.is_empty() {
        return ("unknown".to_string(), String::new());
    }

    let first_token = tokens[0].to_string();

    if first_token == "//" {
        // Inline comment
        let mut content = String::new();
        for token in tokens.iter().skip(1) {
            content.push_str(&token.to_string());
            content.push(' ');
        }
        ("inline".to_string(), content.trim().to_string())
    } else if first_token == "/*" {
        // Block comment
        let mut content = String::new();
        let mut found_end = false;

        for token in tokens.iter().skip(1) {
            let token_str = token.to_string();
            if token_str == "*/" {
                found_end = true;
                break;
            }
            content.push_str(&token_str);
            content.push(' ');
        }

        let comment_type = if found_end { "block" } else { "unclosed_block" };
        (comment_type.to_string(), content.trim().to_string())
    } else {
        (
            "unknown".to_string(),
            tokens
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(" "),
        )
    }
}

/// Generates documentation comments for the comment conversion
fn generate_comment_documentation(
    tokens: &[Token],
    comment_type: &str,
    comment_content: &str) -> String {
    let mut doc_lines = Vec::new();

    // Add main documentation header
    doc_lines.push("/**".to_string());
    doc_lines.push(" * Comment Conversion Documentation".to_string());
    doc_lines.push(" *".to_string());

    // Add comment type information
    doc_lines.push(format!(" * Type: {} comment", comment_type));

    // Add content preview (truncated if too long)
    let preview = if comment_content.len() > 60 {
        format!("{}...", &comment_content[..60])
    } else {
        comment_content.to_string()
    };
    doc_lines.push(format!(" * Content: \"{}\"", preview));

    // Add conversion details
    doc_lines.push(format!(" * Original tokens: {}", tokens.len()));
    doc_lines.push(" *".to_string());

    // Add conversion notes based on comment type
    match comment_type {
        "inline" => {
            doc_lines
                .push(" * Conversion: C inline comment (//) -> Rust line comment (//)".to_string());
            if comment_content.starts_with('/') {
                doc_lines.push(
                    " * Note: Detected potential doc comment, converted to /// format".to_string(),
                );
            }
        }
        "block" => {
            doc_lines.push(" * Conversion: C block comment (/* */) -> Rust format".to_string());
            if comment_content.starts_with('*') {
                doc_lines
                    .push(" * Note: Detected doc comment, converted to /// format".to_string());
            } else if comment_content.contains('\n') {
                doc_lines
                    .push(" * Note: Multi-line comment preserved as block comment".to_string());
            } else {
                doc_lines.push(
                    " * Note: Single-line block comment converted to line comment".to_string(),
                );
            }
        }
        "unclosed_block" => {
            doc_lines.push(" * Warning: Unclosed block comment detected".to_string());
        }
        _ => {
            doc_lines
                .push(" * Note: Unknown comment format, conversion may be imperfect".to_string());
        }
    }

    doc_lines.push(" */".to_string());

    report!(
        "comment_handler",
        "generate_comment_documentation",
        Info,
        Report,
        &format!("Generated documentation for {} comment", comment_type),
        true
    );

    doc_lines.join("\n")
}

/// Postprocesses the converted comment code for better formatting
fn postprocess_comment_code(mut tokens: Vec<Token>) -> Vec<Token> {
    let original_count = tokens.len();

    // Clean up and format the converted comment tokens
    for token in tokens.iter_mut() {
        let mut content = token.to_string();

        // Clean up extra whitespace
        content = content.trim().to_string();

        // Ensure proper spacing after comment delimiters
        if content.starts_with("//")
            && content.len() > 2
            && !content.chars().nth(2).unwrap().is_whitespace()
        {
            content = format!("// {}", &content[2..]);
        }

        // Ensure proper formatting for doc comments
        if content.starts_with("///")
            && content.len() > 3
            && !content.chars().nth(3).unwrap().is_whitespace()
        {
            content = format!("/// {}", &content[3..]);
        }

        *token = Token::s(content);
    }

    // Remove any empty tokens
    tokens.retain(|token| !token.to_string().trim().is_empty());

    report!(
        "comment_handler",
        "postprocess_comment_code",
        Info,
        Report,
        &format!(
            "Postprocessed {} tokens -> {} tokens",
            original_count,
            tokens.len()
        ),
        true
    );

    tokens
}

/// Report callback: Collects and summarizes all reports from the context for this handler
fn report_comment(
    _tokens: &[Token]) -> Result<HandlerReport, ConversionError> {
    let context = context!();
    let handler_reports = context.get_reports_by_handler("comment_handler");

    // Count reports by level
    let info_count = handler_reports
        .iter()
        .filter(|r| matches!(r.level, ReportLevel::Info))
        .count();
    let error_count = handler_reports
        .iter()
        .filter(|r| matches!(r.level, ReportLevel::Error))
        .count();
    let warning_count = handler_reports
        .iter()
        .filter(|r| matches!(r.level, ReportLevel::Warning))
        .count();

    // Create summary message
    let summary_message = format!(
        "Comment Handler Summary: {} total reports (Info: {}, Warnings: {}, Errors: {})",
        handler_reports.len(),
        info_count,
        warning_count,
        error_count
    );

    // Create consolidated report using your gen_name function for unique report_id
    let summary_report = HandlerReport {
        report_id: Box::new(Id::get(&Id::gen_name("comment_handler_summary"))),
        handler_id: Box::new(Id::get("comment_handler")),
        handler_name: "comment_handler".to_string(),
        function_name: "report_comment".to_string(),
        level: ReportLevel::Info,
        phase: HandlerPhase::Report,
        message: summary_message,
        success: error_count == 0, // Success if no errors
        tokens_processed: handler_reports.iter().map(|r| r.tokens_processed).sum(),
        tokens_consumed: handler_reports.iter().map(|r| r.tokens_consumed).sum(),
        metadata: std::collections::HashMap::new(),
    };

    Ok(summary_report)
}
