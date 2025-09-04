use super::common::{not_handled, replace_with};
use crate::context::{Context, HandlerReport, ReportLevel, HandlerPhase};
use crate::context::ReportLevel::{Info, Error, Warning, Debug};
use crate::context::HandlerPhase::{Process, Handle, Extract, Convert, Report};
use crate::report;
use crate::debug;
use crate::error::ConversionError;
use crate::extract::{ExtractedElement, ExtractedComment};
use crate::handler::HandlerResult;
use crate::{get_id, Token};
use crate::{ConvertedElement, ConvertedComment};

/// Creates a comment handler that can detect and convert C comments
pub fn create_comment_handler() -> crate::handler::Handler {
    let handler_id = get_id("comment_handler");
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
        None,
        None,
        Some(redirect_comment),
    )
}

/// Determines if the token sequence represents a comment
fn process_comment(tokens: &[Token], _context: &mut Context) -> Result<bool,ConversionError> {
    let _id = get_id("process_comment");
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
fn handle_comment(tokens: &[Token], context: &mut Context) -> Result<HandlerResult, ConversionError> {
    let _id = get_id("handle_comment");
    report!(context, "comment_handler", Info, Handle, 
        "Comment handler processing tokens", true);
    
    if tokens.is_empty() {
        return not_handled();
    }
    
    // Handle inline comments: //...
    if tokens[0].to_string() == "//" {
        return handle_inline_comment(tokens, context);
    }
    
    // Handle block comments: /* ... */
    if tokens.len() >= 3 && tokens[0].to_string() == "/*" {
        return handle_block_comment(tokens, context);
    }
    
    not_handled()
}

/// Handles inline comments: //...
fn handle_inline_comment(tokens: &[Token], context: &mut Context) -> Result<HandlerResult, ConversionError> {
    if tokens.len() < 2 {
        return not_handled();
    }
    
    report!(context, "comment_handler", Info, Process, 
        "Processing inline comment", true);
    
    // Extract the comment content
    let mut comment = String::new();
    
    for token in tokens.iter().skip(1) {
        comment.push_str(&token.to_string());
        comment.push(' ');
    }
    
    // Convert to Rust comment
    let rust_comment = convert_comment(&format!("//{}", comment));
    
    let id = get_id("inline_comment");
    replace_with(rust_comment, id)
}

/// Handles block comments: /* ... */
fn handle_block_comment(tokens: &[Token], context: &mut Context) -> Result<HandlerResult, ConversionError> {
    if tokens.len() < 3 {
        return not_handled();
    }
    
    report!(context, "comment_handler", Info, Process, 
        "Processing block comment", true);
    
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
    
    let id = get_id("block_comment");
    replace_with(rust_comment, id)
}

/// Converts a C comment to Rust comment
fn convert_comment(comment: &str) -> String {
    // Remove C-style comment delimiters
    let comment = comment.trim();
    
    // Handle block comments /* ... */
    if comment.starts_with("/*") && comment.ends_with("*/") {
        let inner = &comment[2..comment.len()-2].trim();
        
        // Check if it's a documentation comment
        if inner.starts_with("*") {
            // Convert to Rust doc comment format
            let doc_lines: Vec<&str> = inner.lines().collect();
            if doc_lines.len() > 1 {
                // Multi-line doc comment
                let formatted_lines: Vec<String> = doc_lines.iter()
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
        assert_eq!(
            convert_comment("/// Doc comment"),
            "/// Doc comment"
        );
    }
}

/// Extract comment information from tokens
fn extract_comment(tokens: &[Token], context: &mut Context) -> Result<Option<ExtractedElement>, ConversionError> {
    let _id = get_id("extract_comment");
    report!(context, "comment_handler", Info, Extract, 
        "Extracting comment from tokens", true);
    
    if tokens.is_empty() {
        return Ok(None);
    }
    
    // Handle inline comments: //...
    if tokens[0].to_string() == "//" {
        return extract_inline_comment(tokens, context);
    }
    
    // Handle block comments: /* ... */
    if tokens.len() >= 3 && tokens[0].to_string() == "/*" {
        return extract_block_comment(tokens, context);
    }
    
    Ok(None)
}

/// Extract inline comment information
fn extract_inline_comment(tokens: &[Token], context: &mut Context) -> Result<Option<ExtractedElement>, ConversionError> {
    if tokens.len() < 2 {
        return Ok(None);
    }
    
    report!(context, "comment_handler", Info, Extract, 
        "Extracting inline comment", true);
    
    // Extract the comment content
    let mut content = String::new();
    for token in tokens.iter().skip(1) {
        content.push_str(&token.to_string());
        content.push(' ');
    }
    content = content.trim().to_string();
    
    let is_doc_comment = content.starts_with('/') || content.contains("@param") || content.contains("@return");
    
    let extracted = ExtractedComment {
        content,
        is_block: false,
        is_doc_comment,
    };
    
    Ok(Some(ExtractedElement::Comment(extracted)))
}

/// Extract block comment information
fn extract_block_comment(tokens: &[Token], context: &mut Context) -> Result<Option<ExtractedElement>, ConversionError> {
    if tokens.len() < 3 {
        return Ok(None);
    }
    
    report!(context, "comment_handler", Info, Extract, 
        "Extracting block comment", true);
    
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
    
    let is_doc_comment = content.starts_with('*') || content.contains("@param") || content.contains("@return");
    
    let extracted = ExtractedComment {
        content,
        is_block: true,
        is_doc_comment,
    };
    
    Ok(Some(ExtractedElement::Comment(extracted)))
}

/// Convert extracted comment to Rust code - callback version to avoid name conflict
fn convert_comment_callback(tokens: &[Token], context: &mut Context) -> Result<Option<ConvertedElement>, ConversionError> {
    let _id = get_id("convert_comment_callback");
    report!(context, "comment_handler", Info, Convert, 
        "Converting comment to Rust", true);

    // First extract the comment information from tokens
    if let Some(ExtractedElement::Comment(comment)) = extract_comment(tokens, context)? {
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
fn redirect_comment(tokens:&[Token], result: HandlerResult, context: &mut Context) -> Result<HandlerResult, ConversionError> {
    let _id = get_id("redirect_comment");
    report!(context, "comment_handler", Info, Report, 
        "Redirecting comment handler result", true);
    
    // Comments are typically handled independently and don't need redirection
    // They could potentially be redirected to documentation generators
    Ok(result)
}
