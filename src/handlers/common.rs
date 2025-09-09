use crate::error::ConversionError;
use crate::handler::HandlerResult;
use crate::{Id, Token, context};

/// Common utility functions for handlers

/// Check if a token is a specific keyword or identifier
pub fn is_token(token: &Token, value: &str) -> bool {
    token.to_string() == value
}

/// Check if a token sequence starts with a specific token
pub fn starts_with(tokens: &[Token], value: &str) -> bool {
    if tokens.is_empty() {
        return false;
    }
    is_token(&tokens[0], value)
}

/// Check if a token sequence has a minimum length
pub fn has_min_length(tokens: &[Token], min_length: usize) -> bool {
    tokens.len() >= min_length
}

/// Find the position of a specific token in a sequence
pub fn find_token(tokens: &[Token], value: &str) -> Option<usize> {
    tokens.iter().position(|token| is_token(token, value))
}

/// Find the closing token that matches a given opening token
/// For example, find_matching_token(tokens, "(", ")") finds the matching closing parenthesis
pub fn find_matching_token(tokens: &[Token], open_token: &str, close_token: &str) -> Option<usize> {
    if tokens.is_empty() {
        return None;
    }

    let mut depth = 0;
    for (i, token) in tokens.iter().enumerate() {
        if is_token(token, open_token) {
            depth += 1;
        } else if is_token(token, close_token) {
            depth -= 1;
            if depth == 0 {
                return Some(i);
            }
        }
    }

    None
}

/// Extract a block of tokens between delimiters (e.g., { and })
pub fn extract_block(tokens: &[Token], open_token: &str, close_token: &str) -> Option<Vec<Token>> {
    if tokens.is_empty() {
        return None;
    }

    if !starts_with(tokens, open_token) {
        return None;
    }

    match find_matching_token(&tokens[1..], open_token, close_token) {
        Some(pos) => {
            // Add 1 because we started searching from index 1
            let end_pos = pos + 1;
            // Extract tokens between delimiters, excluding the delimiters themselves
            Some(tokens[1..end_pos].to_vec())
        }
        None => None,
    }
}

/// Parse a sequence of tokens as a name (identifier)
pub fn parse_name(tokens: &[Token]) -> Option<String> {
    if tokens.is_empty() {
        return None;
    }

    // For simple identifier
    if tokens.len() == 1 {
        return Some(tokens[0].to_string());
    }

    // For qualified names (e.g., namespace::name)
    let mut name = String::new();
    for token in tokens {
        name.push_str(&token.to_string());
    }

    Some(name)
}

/// Count consumed tokens until a specific token is encountered
pub fn count_until(tokens: &[Token], end_token: &str) -> usize {
    for (i, token) in tokens.iter().enumerate() {
        if token.to_string() == end_token {
            return i + 1; // Include the end token itself
        }
    }

    tokens.len()
}

/// Check if a token sequence represents a type
pub fn is_type(tokens: &[Token]) -> bool {
    if tokens.is_empty() {
        return false;
    }

    let first_token = &tokens[0];

    // Check if it's a primitive type
    if is_primitive_type(first_token) {
        return true;
    }

    // Check for 'const' qualifier
    if is_token(first_token, "const") && tokens.len() > 1 {
        return is_type(&tokens[1..]);
    }

    // Check for typedefs in the registry
    if let Some(typedefs) = context!()
        .registry
        .entries
        .get(&Id::new(0, "typedefs".to_string()))
    {
        match typedefs {
            crate::Entry::StrMap(map) => {
                if map.contains_key(&first_token.to_string()) {
                    return true;
                }
            }
            _ => {}
        }
    }

    // Check for structs, unions, and enums
    is_token(first_token, "struct")
        || is_token(first_token, "union")
        || is_token(first_token, "enum")
}

/// Check if a token represents a primitive C type
pub fn is_primitive_type(token: &Token) -> bool {
    matches!(
        token.to_string().as_str(),
        "void"
            | "char"
            | "short"
            | "int"
            | "long"
            | "float"
            | "double"
            | "signed"
            | "unsigned"
            | "bool"
            | "_Bool"
            | "int8_t"
            | "int16_t"
            | "int32_t"
            | "int64_t"
            | "uint8_t"
            | "uint16_t"
            | "uint32_t"
            | "uint64_t"
            | "size_t"
            | "ssize_t"
            | "ptrdiff_t"
            | "intptr_t"
            | "uintptr_t"
    )
}

/// Create a simple handler result that doesn't handle tokens
pub fn not_handled() -> Result<HandlerResult, ConversionError> {
    Ok(HandlerResult::NotHandled(
        None,
        0..0,
        Id::get("not_handled"),
    ))
}

/// Create a handler result that replaces the processed tokens with the given Rust code
pub fn replace_with(rust_code: String, id: Id) -> Result<HandlerResult, ConversionError> {
    Ok(HandlerResult::Completed(None, 0..1, rust_code, id))
}

/// Create a handler result that replaces the processed tokens with the given Rust code
/// and specifies the exact range of tokens consumed
pub fn replace_with_range(
    rust_code: String,
    token_range: std::ops::Range<usize>,
    id: Id,
) -> Result<HandlerResult, ConversionError> {
    Ok(HandlerResult::Completed(None, token_range, rust_code, id))
}

/// Extract documentation comments preceding a token sequence
pub fn extract_docs(tokens: &[Token]) -> Option<String> {
    let mut doc_lines = Vec::new();
    let mut i = 0;

    while i < tokens.len() {
        match tokens[i].to_string().as_str() {
            "//" => {
                // Single line comment
                if i + 1 < tokens.len() {
                    doc_lines.push(tokens[i + 1].to_string());
                }
                i += 2; // Skip comment token and content
            }
            "/*" => {
                // Multi-line comment
                let mut comment = String::new();
                i += 1; // Skip the opening /*
                while i < tokens.len() && tokens[i].to_string() != "*/" {
                    comment.push_str(&tokens[i].to_string());
                    comment.push(' ');
                    i += 1;
                }
                if i < tokens.len() {
                    i += 1; // Skip the closing */
                }
                doc_lines.push(comment);
            }
            _ => {
                // Not a comment token, stop collecting
                break;
            }
        }
    }

    if doc_lines.is_empty() {
        None
    } else {
        Some(doc_lines.join("\n"))
    }
}
