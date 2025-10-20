use crate::{
    error::C2RError, handler::HandlerResult, Context, Entry, Id, Kind, Reason, Result, Token,
};
use core::{ops::Range, option::Option::None};
use std::{collections::HashMap, sync::LazyLock};

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
pub fn find_matching_paren(tokens: &[Token], start: usize) -> Result<usize> {
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
pub fn find_matching_brace(tokens: &[Token], start: usize) -> Result<usize> {
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
pub fn is_type(context: &Context, tokens: &[Token]) -> bool {
    if tokens.is_empty() {
        return false;
    }

    let first_token = &tokens[0];

    // Check if it's a type using the MAP
    if is_type_token(first_token) {
        return true;
    }

    // Check for 'const' qualifier
    if is_token(first_token, "const") && tokens.len() > 1 {
        return is_type(context, &tokens[1..]);
    }

    // Check for typedefs in the registry
    let typedefs = context.registry.entries.get(&Id::get("typedefs")).cloned();
    if let Some(Entry::StrMap(map)) = typedefs {
        if map.contains_key(&first_token.to_string()) {
            return true;
        }
    }

    // Check for structs, unions, and enums
    is_token(first_token, "struct")
        || is_token(first_token, "union")
        || is_token(first_token, "enum")
}

/// Check if a token represents a primitive C type
pub fn is_primitive_type(token: &Token) -> bool {
    let token_str = token.to_string();
    matches!(
        token_str.as_str(),
        // Basic C types
        "void" | "char" | "short" | "int" | "long" | "float" | "double"
        // Type modifiers
        | "signed" | "unsigned"
        // Boolean types
        | "bool" | "_Bool"
        // Fixed-width integer types
        | "int8_t" | "int16_t" | "int32_t" | "int64_t"
        | "uint8_t" | "uint16_t" | "uint32_t" | "uint64_t"
        // Size and pointer types
        | "size_t" | "ssize_t" | "ptrdiff_t" | "intptr_t" | "uintptr_t"
        // Additional standard types
        | "intmax_t" | "uintmax_t"
        | "int_fast8_t" | "int_fast16_t" | "int_fast32_t" | "int_fast64_t"
        | "uint_fast8_t" | "uint_fast16_t" | "uint_fast32_t" | "uint_fast64_t"
        | "int_least8_t" | "int_least16_t" | "int_least32_t" | "int_least64_t"
        | "uint_least8_t" | "uint_least16_t" | "uint_least32_t" | "uint_least64_t"
    )
}
/// Create a simple handler result that doesn't handle tokens
pub fn not_handled(token_range: Range<usize>, handler_id: Id) -> Result<HandlerResult> {
    Ok(HandlerResult::NotHandled(
        None,
        token_range,
        handler_id,
    ))
}

/// Extract documentation comments preceding a token sequence
pub fn extract_docs(tokens: &[Token]) -> Option<String> {
    let mut doc_lines = Vec::new();
    let mut i = 0;

    while i < tokens.len() {
        let token_str = tokens[i].to_string();

        match token_str.as_str() {
            "//" => {
                // Single line comment - collect all content until newline or end
                i += 1;
                let mut line_content = String::new();

                while i < tokens.len() {
                    let content = tokens[i].to_string();
                    if content == "\n" || content == "\r\n" {
                        break;
                    }
                    if !line_content.is_empty() {
                        line_content.push(' ');
                    }
                    line_content.push_str(&content);
                    i += 1;
                }

                if !line_content.is_empty() {
                    doc_lines.push(line_content.trim().to_string());
                }

                if i < tokens.len() {
                    i += 1; // Skip newline
                }
            }
            "/*" => {
                // Multi-line comment - preserve structure and handle nested comments
                i += 1; // Skip opening /*
                let mut comment = String::new();
                let mut nesting_level = 1;

                while i < tokens.len() && nesting_level > 0 {
                    let content = tokens[i].to_string();

                    match content.as_str() {
                        "/*" => {
                            nesting_level += 1;
                            comment.push_str(&content);
                        }
                        "*/" => {
                            nesting_level -= 1;
                            if nesting_level > 0 {
                                comment.push_str(&content);
                            }
                        }
                        "\n" | "\r\n" => {
                            comment.push('\n');
                        }
                        _ => {
                            if !comment.is_empty() && !comment.ends_with(' ') && !comment.ends_with('\n') {
                                comment.push(' ');
                            }
                            comment.push_str(&content);
                        }
                    }
                    i += 1;
                }

                let cleaned_comment = comment
                    .lines()
                    .map(|line| line.trim())
                    .filter(|line| !line.is_empty())
                    .collect::<Vec<_>>()
                    .join("\n");

                if !cleaned_comment.is_empty() {
                    doc_lines.push(cleaned_comment);
                }
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
        Some(doc_lines.join("\n\n"))
    }
}

/// Centralized C identifier to Rust naming convention converter
pub fn convert_identifier_to_rust(identifier: &str, target_case: IdentifierCase) -> String {
    match target_case {
        IdentifierCase::SnakeCase => to_snake_case(identifier),
        IdentifierCase::PascalCase => to_pascal_case(identifier),
        IdentifierCase::ScreamingSnakeCase => to_screaming_snake_case(identifier),
    }
}

/// Target case types for identifier conversion
pub enum IdentifierCase {
    SnakeCase,          // for functions, variables
    PascalCase,         // for types, enums, structs
    ScreamingSnakeCase, // for constants
}

fn to_snake_case(s: &str) -> String {
    let mut result = String::new();
    let mut chars = s.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch.is_uppercase() && !result.is_empty() {
            result.push('_');
        }
        result.push(ch.to_lowercase().next().unwrap_or(ch));
    }
    result
}

fn to_pascal_case(s: &str) -> String {
    let mut result = String::new();
    let mut capitalize_next = true;

    for ch in s.chars() {
        if ch == '_' {
            capitalize_next = true;
        } else if capitalize_next {
            result.push(ch.to_uppercase().next().unwrap_or(ch));
            capitalize_next = false;
        } else {
            result.push(ch.to_lowercase().next().unwrap_or(ch));
        }
    }
    result
}

fn to_screaming_snake_case(s: &str) -> String {
    to_snake_case(s).to_uppercase()
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

    // Use existing type conversion system
    let rust_base_type = if let Some(converted) = convert_type_tokens(type_tokens.to_vec()) {
        converted.to_string()
    } else {
        // Fallback to direct type mapping
        convert_type(&base_type_str).unwrap_or("()".to_string())
    };

    // Efficient pointer type construction
    match pointer_count {
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
    }
}
/// Helper function to convert tokens to a string representation
pub fn tokens_to_string(tokens: &[Token]) -> Result<String> {
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
    !token.is_empty()
        && token
        .chars()
        .next()
        .map_or(false, |c| c.is_alphabetic() || c == '_')
        && token.chars().all(|c| c.is_alphanumeric() || c == '_')
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

/// Helper function for pattern-based token filtering (moved from BaseHandler)
pub fn filter_tokens_for_handler(tokens: &[Token], token_range: Range<usize>) -> Vec<Token> {
    if token_range.end > tokens.len() || token_range.start >= tokens.len() {
        return Vec::new();
    }

    tokens[token_range.clone()]
        .iter()
        .filter(|token| !matches!(**token, Token::n()))
        .cloned()
        .collect()
}
// NOTE: Redirecting to the centralized TYPE_CONVERSION_MAP in pattern.rs for consistency
pub const MAP: LazyLock<HashMap<String, String>> = LazyLock::new(|| {
    use crate::pattern::TYPE_CONVERSION_MAP;
    TYPE_CONVERSION_MAP.type_map.clone()
});

pub fn is_type_token(token: &Token) -> bool {
    use crate::pattern::TYPE_CONVERSION_MAP;
    TYPE_CONVERSION_MAP.is_c_type(&token.to_string())
}

pub fn convert_type(c_type: &str) -> Option<String> {
    use crate::pattern::TYPE_CONVERSION_MAP;
    TYPE_CONVERSION_MAP.convert_type(c_type)
}

pub fn convert_type_token(c_token: &Token) -> Option<String> {
    use crate::pattern::TYPE_CONVERSION_MAP;
    TYPE_CONVERSION_MAP.convert_type(&c_token.to_string().trim())
}

pub fn convert_type_tokens(c_type: Vec<Token>) -> Option<Token> {
    use crate::pattern::TYPE_CONVERSION_MAP;
    let mut c_type_str: String = String::new();
    for t in c_type.iter() {
        let s = t.to_string();
        if s.trim() != "**"
            && s.trim() != "&&"
            && s.trim() != "*"
            && s.trim() != "&"
            && s.trim() != ""
        {
            c_type_str.push_str(s.trim());
            c_type_str.push(' ');
        } else {
            c_type_str.push_str(s.trim());
        }
    }
    TYPE_CONVERSION_MAP.convert_type(&c_type_str)
        .map(|rust_type| Token::s(rust_type))
}
pub fn is_c_keyword(token: Token) -> bool {
    [
        "_Alignas",
        "_Alignof",
        "_Atomic",
        "_Bool",
        "_Complex",
        "_Generic",
        "_Imaginary",
        "_Noreturn",
        "_Static_assert",
        "_Thread_local",
        "__FUNCTION__",
        "__PRETTY_FUNCTION__",
        "__alignof",
        "__alignof__",
        "__asm",
        "__asm__",
        "__attribute__",
        "__builtin_offsetof",
        "__builtin_types_compatible_p",
        "__builtin_va_list",
        "__const",
        "__const__",
        "__extension__",
        "__func__",
        "__inline",
        "__inline__",
        "__restrict",
        "__restrict__",
        "__signed",
        "__signed__",
        "__typeof",
        "__typeof__",
        "__unsigned",
        "__unsigned__",
        "__volatile",
        "__volatile__",
        "auto",
        "break",
        "case",
        "const",
        "continue",
        "default",
        "do",
        "else",
        "enum",
        "extern",
        "for",
        "goto",
        "if",
        "inline",
        "register",
        "restrict",
        "return",
        "signed",
        "sizeof",
        "static",
        "struct",
        "switch",
        "union",
        "unsigned",
        "volatile",
        "while",
        "asm",
        "catch",
        "class",
        "const_cast",
        "delete",
        "dynamic_cast",
        "explicit",
        "export",
        "false",
        "friend",
        "mutable",
        "namespace",
        "new",
        "operator",
        "private",
        "protected",
        "public",
        "reinterpret_cast",
        "static_cast",
        "template",
        "this",
        "throw",
        "true",
        "try",
        "typeid",
        "typename",
        "using",
        "virtual",
        "wchar_t",
    ]
        .contains(&token.to_string().trim())
}
/// DEPRECATED: This function is kept for backwards compatibility only.
/// Use TYPE_CONVERSION_MAP from pattern.rs instead, which includes metadata,
/// categories, and scoring capabilities for better pattern matching.
#[deprecated(since = "0.1.0", note = "Use TYPE_CONVERSION_MAP from pattern.rs instead")]
pub fn create_map() -> HashMap<String, String> {
    let mut hm1: HashMap<&str, &str> = HashMap::new();
    // Basic C types
    hm1.insert("bool", "bool");
    hm1.insert("_Bool", "bool");
    hm1.insert("char", "i8");
    hm1.insert("short", "i16");
    hm1.insert("int", "i32");
    hm1.insert("long", "i64");
    hm1.insert("long long", "i64");
    hm1.insert("float", "f32");
    hm1.insert("double", "f64");
    hm1.insert("void", "()");

    // Size types
    hm1.insert("size_t", "usize");
    hm1.insert("ssize_t", "isize");
    hm1.insert("ptrdiff_t", "isize");
    hm1.insert("intptr_t", "isize");
    hm1.insert("uintptr_t", "usize");

    // Fixed width integer types
    hm1.insert("int8_t", "i8");
    hm1.insert("int16_t", "i16");
    hm1.insert("int32_t", "i32");
    hm1.insert("int64_t", "i64");
    hm1.insert("uint8_t", "u8");
    hm1.insert("uint16_t", "u16");
    hm1.insert("uint32_t", "u32");
    hm1.insert("uint64_t", "u64");
    hm1.insert("intmax_t", "i64");
    hm1.insert("uintmax_t", "u64");

    // Fast integer types
    hm1.insert("int_fast8_t", "i8");
    hm1.insert("int_fast16_t", "i16");
    hm1.insert("int_fast32_t", "i32");
    hm1.insert("int_fast64_t", "i64");
    hm1.insert("uint_fast8_t", "u8");
    hm1.insert("uint_fast16_t", "u16");
    hm1.insert("uint_fast32_t", "u32");
    hm1.insert("uint_fast64_t", "u64");

    // Least integer types
    hm1.insert("int_least8_t", "i8");
    hm1.insert("int_least16_t", "i16");
    hm1.insert("int_least32_t", "i32");
    hm1.insert("int_least64_t", "i64");
    hm1.insert("uint_least8_t", "u8");
    hm1.insert("uint_least16_t", "u16");
    hm1.insert("uint_least32_t", "u32");
    hm1.insert("uint_least64_t", "u64");

    // Unsigned types
    hm1.insert("unsigned char", "u8");
    hm1.insert("unsigned short", "u16");
    hm1.insert("unsigned int", "u32");
    hm1.insert("unsigned long", "u64");
    hm1.insert("unsigned long long", "u64");

    // Common C library types
    hm1.insert("FILE", "libc::FILE");
    hm1.insert("DIR", "libc::DIR");
    hm1.insert("time_t", "libc::time_t");
    hm1.insert("clock_t", "libc::clock_t");
    hm1.insert("tm", "libc::tm");
    hm1.insert("timespec", "libc::timespec");
    hm1.insert("timeval", "libc::timeval");
    hm1.insert("va_list", "libc::va_list");
    hm1.insert("pthread_t", "libc::pthread_t");
    hm1.insert("pthread_mutex_t", "libc::pthread_mutex_t");
    hm1.insert("pthread_cond_t", "libc::pthread_cond_t");
    hm1.insert("complex", "libc::complex");
    hm1.insert("_Complex", "libc::complex");
    hm1.insert("_Imaginary", "libc::imaginary");
    // Socket and network types
    hm1.insert("sockaddr", "libc::sockaddr");
    hm1.insert("sockaddr_in", "libc::sockaddr_in");
    hm1.insert("sockaddr_in6", "libc::sockaddr_in6");
    hm1.insert("socklen_t", "libc::socklen_t");
    hm1.insert("in_addr", "libc::in_addr");
    hm1.insert("in6_addr", "libc::in6_addr");

    // Common POSIX types
    hm1.insert("mode_t", "libc::mode_t");
    hm1.insert("pid_t", "libc::pid_t");
    hm1.insert("uid_t", "libc::uid_t");
    hm1.insert("gid_t", "libc::gid_t");
    hm1.insert("off_t", "libc::off_t");
    hm1.insert("dev_t", "libc::dev_t");
    hm1.insert("ino_t", "libc::ino_t");
    hm1.insert("nlink_t", "libc::nlink_t");
    hm1.insert("blkcnt_t", "libc::blkcnt_t");
    hm1.insert("blksize_t", "libc::blksize_t");
    let mut hm2 = HashMap::<String, String>::new();
    // Add derived pointer and reference types for all basic mappings
    for (c_type, rust_type) in hm1.iter_mut() {
        hm2.insert(c_type.to_string(), rust_type.to_string());
        // Generate pointer variants
        hm2.insert(format!("{}*", c_type), format!("*mut {}", rust_type));
        hm2.insert(
            format!("const {}*", c_type),
            format!("*const {}", rust_type),
        );

        // Generate double pointer variants
        hm2.insert(format!("{}**", c_type), format!("*mut *mut {}", rust_type));
        hm2.insert(
            format!("const {}**", c_type),
            format!("*const *const {}", rust_type),
        );

        // Generate reference variants (C++ style)
        hm2.insert(format!("{}&", c_type), format!("&mut {}", rust_type));
        hm2.insert(format!("const {}&", c_type), format!("&{}", rust_type));

        // Generate pointer-reference combinations
        hm2.insert(format!("*{}&", c_type), format!("&*mut {}", rust_type));
        hm2.insert(
            format!("const *{}&", c_type),
            format!("&*const {}", rust_type),
        );
    }
    // Implementation logic for type conversion
    hm2
}

//===============================================================================
// Pattern Detection Utilities
//===============================================================================

/// Check if a string is a valid C identifier
/// Used by global_handler, function_handler, etc.
pub fn is_valid_c_identifier(s: &str) -> bool {
    !s.is_empty()
        && s.chars().next().map_or(false, |c| c.is_alphabetic() || c == '_')
        && s.chars().all(|c| c.is_alphanumeric() || c == '_')
}

/// Multi-pattern detection helper
/// Tries multiple pattern names and returns true if any matches
pub fn try_multiple_patterns(
    patternizer: &mut crate::Patternizer,
    pattern_names: &[&str],
    tokens: &[Token],
) -> bool {
    for pattern_name in pattern_names {
        let pattern_match = patternizer.match_pattern(pattern_name, tokens);
        if !matches!(pattern_match, crate::PatternResult::NoMatch { .. }) {
            return true;
        }
    }
    false
}

/// Calculate combined confidence score from pattern success and heuristics
/// Standardized scoring used across handlers
pub fn calculate_confidence_score(
    pattern_success: bool,
    base_confidence: f64,
    has_primary_keyword: bool,
    has_secondary_markers: bool,
) -> f64 {
    let mut confidence = base_confidence;

    // Boost for primary keyword (e.g., "struct", "enum", "typedef")
    if has_primary_keyword {
        confidence += 0.1;
    }

    // Additional boost for secondary markers (e.g., braces, semicolons)
    if has_secondary_markers {
        confidence += 0.1;
    }

    // Pattern match provides baseline confidence
    if pattern_success && confidence < 0.7 {
        confidence = 0.7;
    }

    confidence.min(1.0)
}

/// Check if tokens contain any of the specified keywords
pub fn contains_any_keyword(tokens: &[Token], keywords: &[&str]) -> bool {
    tokens.iter().any(|t| {
        let token_str = t.to_string();
        keywords.contains(&token_str.as_str())
    })
}

/// Check if tokens contain all of the specified keywords
pub fn contains_all_keywords(tokens: &[Token], keywords: &[&str]) -> bool {
    keywords.iter().all(|&keyword| {
        tokens.iter().any(|t| t.to_string() == keyword)
    })
}

/// Extract tokens between opening and closing markers (e.g., parentheses, braces)
pub fn extract_tokens_between(
    tokens: &[Token],
    open_marker: &str,
    close_marker: &str,
) -> Option<Vec<Token>> {
    let open_pos = tokens.iter().position(|t| t.to_string() == open_marker)?;
    let close_pos = tokens.iter().skip(open_pos + 1).position(|t| t.to_string() == close_marker)?;

    Some(tokens[open_pos + 1..open_pos + 1 + close_pos].to_vec())
}