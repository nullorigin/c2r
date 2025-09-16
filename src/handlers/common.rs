use crate::{
    HandlerPattern, Id, Kind, PatternResult, PatternRule, Reason, Token, TokenPattern, context,
    error::C2RError, handler::HandlerResult,
};
use core::{ops::Range, option::Option::None};

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
/// Improved with better bounds checking and nested structure handling
pub fn find_matching_token(tokens: &[Token], open_token: &str, close_token: &str) -> Option<usize> {
    if tokens.is_empty() {
        return None;
    }

    let mut depth = 0;
    let mut found_opening = false;

    for (i, token) in tokens.iter().enumerate() {
        let token_str = token.to_string();

        if token_str == open_token {
            depth += 1;
            found_opening = true;
        } else if token_str == close_token {
            if found_opening {
                depth -= 1;
                if depth == 0 {
                    return Some(i);
                }
            }
        }
    }

    None
}
/// Find matching parenthesis with improved error handling
pub fn find_matching_paren(tokens: &[Token], start: usize) -> Result<usize, C2RError> {
    if start >= tokens.len() {
        return Err(C2RError::new(
            Kind::Other,
            Reason::Unexpected("start position is out of bounds"),
            None,
        ));
    }

    find_matching_token(&tokens[start..], "(", ")")
        .map(|pos| start + pos)
        .ok_or_else(|| {
            C2RError::new(
                Kind::Other,
                Reason::Unexpected("no matching parenthesis found"),
                None,
            )
        })
}

/// Find matching brace with improved error handling
pub fn find_matching_brace(tokens: &[Token], start: usize) -> Result<usize, C2RError> {
    if start >= tokens.len() {
        return Err(C2RError::new(
            Kind::Other,
            Reason::Unexpected("start position is out of bounds"),
            None,
        ));
    }

    find_matching_token(&tokens[start..], "{", "}")
        .map(|pos| start + pos)
        .ok_or_else(|| {
            C2RError::new(
                Kind::Other,
                Reason::Unexpected("no matching brace found"),
                None,
            )
        })
}
/// Find the end of a C statement (usually semicolon or closing brace)
pub fn find_statement_end(tokens: &[Token], start: usize) -> usize {
    let mut brace_depth = 0;
    let mut paren_depth = 0;

    for i in start..tokens.len() {
        let token_str = tokens[i].to_string();

        match token_str.as_str() {
            "{" => brace_depth += 1,
            "}" => {
                brace_depth -= 1;
                if brace_depth <= 0 {
                    return i + 1;
                }
            }
            "(" => paren_depth += 1,
            ")" => paren_depth -= 1,
            ";" => {
                if brace_depth == 0 && paren_depth == 0 {
                    return i + 1;
                }
            }
            _ => {}
        }
    }

    tokens.len()
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
pub fn not_handled() -> Result<HandlerResult, C2RError> {
    Ok(HandlerResult::NotHandled(
        None,
        0..0,
        Id::get("not_handled"),
    ))
}

/// Create a handler result that replaces the processed tokens with the given Rust code
pub fn replace_with(rust_code: String, id: Id) -> Result<HandlerResult, C2RError> {
    Ok(HandlerResult::Completed(None, 0..1, rust_code, id))
}

/// Create a handler result that replaces the processed tokens with the given Rust code
/// and specifies the exact range of tokens consumed
pub fn replace_with_range(
    rust_code: String,
    token_range: std::ops::Range<usize>,
    id: Id,
) -> Result<HandlerResult, C2RError> {
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
/// Pattern-aware type conversion helper
pub fn convert_type_with_pattern(c_type: &str, pattern: &str) -> String {
    let clean_type = c_type.trim();

    let mut context = crate::context!();

    // Add C type patterns to context patternizer
    let c_int_pattern = HandlerPattern::new("c_int".to_string(), "C integer types".to_string())
        .with_rules(vec![PatternRule::new(TokenPattern::OneOf(vec![
            "int".to_string(),
            "unsigned int".to_string(),
            "signed int".to_string(),
        ]))]);

    let c_char_pattern = HandlerPattern::new("c_char".to_string(), "C character types".to_string())
        .with_rules(vec![PatternRule::new(TokenPattern::OneOf(vec![
            "char".to_string(),
            "unsigned char".to_string(),
            "signed char".to_string(),
        ]))]);

    let c_pointer_pattern =
        HandlerPattern::new("c_pointer".to_string(), "C pointer types".to_string()).with_rules(
            vec![PatternRule::new(TokenPattern::OneOf(vec![
                "void*".to_string(),
                "char*".to_string(),
            ]))],
        );

    context
        .patternizer
        .register_pattern("c_int".to_string(), c_int_pattern);
    context
        .patternizer
        .register_pattern("c_char".to_string(), c_char_pattern);
    context
        .patternizer
        .register_pattern("c_pointer".to_string(), c_pointer_pattern);

    // Create token for pattern matching
    let token = Token::s(clean_type.to_string());
    let tokens = vec![token];

    let conversion_result = match pattern {
        "extern_function" => {
            if let PatternResult::Match { .. } = context.patternizer.match_pattern("c_int", &tokens)
            {
                match clean_type {
                    "int" => "i32",
                    "unsigned int" => "u32",
                    _ => "i32",
                }
            } else if let PatternResult::Match { .. } =
                context.patternizer.match_pattern("c_char", &tokens)
            {
                match clean_type {
                    "char" => "i8",
                    "unsigned char" => "u8",
                    _ => "i8",
                }
            } else if let PatternResult::Match { .. } =
                context.patternizer.match_pattern("c_pointer", &tokens)
            {
                match clean_type {
                    "void*" => "*mut std::ffi::c_void",
                    "char*" => "*const std::ffi::c_char",
                    _ => clean_type,
                }
            } else {
                match clean_type {
                    "float" => "f32",
                    "double" => "f64",
                    _ => clean_type,
                }
            }
        }
        _ => {
            if let PatternResult::Match { .. } = context.patternizer.match_pattern("c_int", &tokens)
            {
                match clean_type {
                    "int" => "i32",
                    "unsigned int" => "u32",
                    _ => "i32",
                }
            } else if let PatternResult::Match { .. } =
                context.patternizer.match_pattern("c_char", &tokens)
            {
                match clean_type {
                    "char" => "char",
                    "unsigned char" => "u8",
                    _ => "char",
                }
            } else if let PatternResult::Match { .. } =
                context.patternizer.match_pattern("c_pointer", &tokens)
            {
                match clean_type {
                    "void*" => "*mut ()",
                    "char*" => "&str",
                    _ => clean_type,
                }
            } else {
                match clean_type {
                    "float" => "f32",
                    "double" => "f64",
                    _ => clean_type,
                }
            }
        }
    };

    conversion_result.to_string()
}
/// Optimized C type to Rust type conversion with caching and improved logic
pub fn convert_c_type_to_rust(type_tokens: &[Token]) -> String {
    if type_tokens.is_empty() {
        return "()".to_string();
    }

    // Pre-allocate and collect once to avoid multiple iterations
    let type_parts: Vec<String> = type_tokens.iter().map(|t| t.to_string()).collect();

    // Single pass analysis of type modifiers
    let mut is_const = false;
    let mut pointer_count = 0;
    let mut base_parts = Vec::new();

    for part in &type_parts {
        match part.as_str() {
            "const" => is_const = true,
            "*" => pointer_count += 1,
            "static" | "extern" | "inline" | "__inline" | "__inline__" => {
                // Skip function modifiers that don't affect parameter types
            }
            _ => base_parts.push(part.clone()),
        }
    }

    let base_type_str = base_parts.join(" ");

    // Use patternizer for type conversion
    let rust_base_type = convert_type_with_pattern(&base_type_str, "default");

    // Efficient pointer type construction
    let final_type = match pointer_count {
        0 => rust_base_type,
        1 => format!(
            "*{} {}",
            if is_const { "const" } else { "mut" },
            rust_base_type
        ),
        n => {
            let mut result = rust_base_type;
            for i in 0..n {
                result = format!(
                    "*{} {}",
                    if i == 0 && is_const { "const" } else { "mut" },
                    result
                );
            }
            result
        }
    };

    final_type
}
/// Helper function to convert tokens to a string representation
pub fn tokens_to_string(tokens: &[Token]) -> Result<String, C2RError> {
    let mut result = String::new();

    for (i, token) in tokens.iter().enumerate() {
        let token_str = token.to_string();

        // Add space between tokens if needed
        if i > 0 {
            let prev_token = tokens[i - 1].to_string();

            // Add space if both tokens are identifiers or literals
            if (is_identifier(&prev_token) || is_literal(&prev_token))
                && (is_identifier(&token_str) || is_literal(&token_str))
            {
                result.push(' ');
            }

            // Add space around operators
            if is_operator(&prev_token) || is_operator(&token_str) {
                result.push(' ');
            }
        }

        result.push_str(&token_str);
    }

    Ok(result)
}
/// Check if a token is an identifier
pub fn is_identifier(token: &str) -> bool {
    if let Some(first_char) = token.chars().next() {
        first_char.is_alphabetic() || first_char == '_'
    } else {
        false
    }
}

/// Check if a token is a literal
pub fn is_literal(token: &str) -> bool {
    if let Some(first_char) = token.chars().next() {
        first_char.is_digit(10) || token.starts_with('\"') || token.starts_with('\'')
    } else {
        false
    }
}

/// Check if a token is an operator
pub fn is_operator(token: &str) -> bool {
    match token {
        "+" | "-" | "*" | "/" | "%" | "=" | "==" | "!=" | "<" | ">" | "<=" | ">=" | "+=" | "-="
        | "*=" | "/=" | "%=" => true,
        _ => false,
    }
}
/// Check if a token string represents an assignment operator
pub fn is_assignment_operator(token: &str) -> bool {
    match token {
        "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>=" => true,
        _ => false,
    }
}
/// Check if a token string represents a binary operator
pub fn is_binary_operator(token: &str) -> bool {
    match token {
        "+" | "-" | "*" | "/" | "%" | "&" | "|" | "^" | "<<" | ">>" | "==" | "!=" | "<" | ">"
        | "<=" | ">=" | "&&" | "||" => true,
        _ => false,
    }
}
/// Convert a C operator to its Rust equivalent
pub fn convert_operator(op: &str) -> String {
    match op {
        // Most operators are the same in Rust
        "+" | "-" | "*" | "/" | "%" | "&" | "|" | "^" | "<<" | ">>" | "==" | "!=" | "<" | ">"
        | "<=" | ">=" | "&&" | "||" | "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|="
        | "^=" | "<<=" | ">>=" => op.to_string(),
        // Some C operators need translation
        "!" => "!".to_string(),
        "~" => "!".to_string(), // Bitwise NOT is different in Rust
        _ => op.to_string(),
    }
}
pub fn filter_tokens(
    tokens: &[Token],
    filter: &[Token],
    token_range: Range<usize>,
    offset: usize,
) -> (usize, Vec<Token>) {
    let mut result_tokens = Vec::with_capacity(tokens.len());
    let mut consumed_count = 0;

    for (i, token) in tokens.iter().enumerate() {
        let adjusted_index = i + offset;

        if token_range.contains(&adjusted_index) {
            if matches!(token, Token::n()) {
                consumed_count += 1;
                result_tokens.push(Token::n());
            } else if !token.match_any(filter) {
                result_tokens.push(token.clone());
            } else {
                result_tokens.push(Token::n());
                consumed_count += 1;
            }
        } else {
            result_tokens.push(token.clone());
        }
    }

    (consumed_count, result_tokens)
}
