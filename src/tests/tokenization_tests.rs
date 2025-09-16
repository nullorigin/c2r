//! Comprehensive tests for tokenization functionality
//! Tests the tokenizer's ability to process C code and generate proper Token sequences

use crate::error::C2RError;
use crate::token::{Token, Tokenizer};

/// Test basic tokenizer creation and initialization
#[test]
fn test_tokenizer_creation() {
    let tokenizer = Tokenizer::new("test_tokenizer");

    // Tokenizer should be properly initialized
    // We can't directly test private fields, but we can test basic functionality
    assert_eq!(
        std::mem::size_of_val(&tokenizer),
        std::mem::size_of::<Tokenizer>()
    );
}

/// Test tokenizing simple C identifiers
#[test]
fn test_tokenize_identifiers() -> Result<(), C2RError> {
    let mut tokenizer = Tokenizer::new("test_tokenizer");

    let c_code = b"int main variable_name".to_vec();
    let tokens = tokenizer.tokenize(c_code)?;

    // Should have at least the identifiers we expect
    assert!(tokens.len() >= 3, "Should have at least 3 tokens");

    // Check for Token::s() variants which identifiers become
    let token_s_strings: Vec<String> = tokens
        .iter()
        .filter_map(|t| {
            if let Token::s(s) = t {
                Some(s.clone())
            } else {
                None
            }
        })
        .collect();

    assert!(
        token_s_strings.contains(&"int".to_string()),
        "Should contain 'int' token"
    );
    assert!(
        token_s_strings.contains(&"main".to_string()),
        "Should contain 'main' token"
    );
    assert!(
        token_s_strings.contains(&"variable_name".to_string()),
        "Should contain 'variable_name' token"
    );

    Ok(())
}

/// Test tokenizing C keywords
#[test]
fn test_tokenize_keywords() -> Result<(), C2RError> {
    let mut tokenizer = Tokenizer::new("test_tokenizer");

    let c_code = b"if while for return struct typedef".to_vec();
    let tokens = tokenizer.tokenize(c_code)?;

    let token_strings: Vec<String> = tokens
        .iter()
        .filter_map(|t| {
            if let Token::s(s) = t {
                Some(s.clone())
            } else {
                None
            }
        })
        .collect();

    // Check for C keywords
    assert!(
        token_strings.contains(&"if".to_string()),
        "Should tokenize 'if' keyword"
    );
    assert!(
        token_strings.contains(&"while".to_string()),
        "Should tokenize 'while' keyword"
    );
    assert!(
        token_strings.contains(&"for".to_string()),
        "Should tokenize 'for' keyword"
    );
    assert!(
        token_strings.contains(&"return".to_string()),
        "Should tokenize 'return' keyword"
    );
    assert!(
        token_strings.contains(&"struct".to_string()),
        "Should tokenize 'struct' keyword"
    );
    assert!(
        token_strings.contains(&"typedef".to_string()),
        "Should tokenize 'typedef' keyword"
    );

    Ok(())
}

/// Test tokenizing numeric literals
#[test]
fn test_tokenize_numbers() -> Result<(), C2RError> {
    let mut tokenizer = Tokenizer::new("test_tokenizer");

    let c_code = b"42 3.14 0x1A 077".to_vec();
    let tokens = tokenizer.tokenize(c_code)?;

    // The tokenizer should now produce proper Token::i() and Token::f() variants
    let has_integers = tokens.iter().any(|t| matches!(t, Token::i(_)));
    let has_floats = tokens.iter().any(|t| matches!(t, Token::f(_)));
    assert!(
        has_integers || has_floats,
        "Should contain proper numeric tokens"
    );

    // Check for specific numeric values
    let integer_values: Vec<i128> = tokens
        .iter()
        .filter_map(|t| if let Token::i(n) = t { Some(*n) } else { None })
        .collect();

    let float_values: Vec<f64> = tokens
        .iter()
        .filter_map(|t| if let Token::f(n) = t { Some(*n) } else { None })
        .collect();

    // Look for specific numbers from "42 3.14 0x1A 077"
    assert!(integer_values.contains(&42), "Should tokenize integer 42");
    assert!(
        float_values
            .iter()
            .any(|&f| (f - 3.14).abs() < f64::EPSILON),
        "Should tokenize float 3.14"
    );

    Ok(())
}

/// Test tokenizing operators and punctuation
#[test]
fn test_tokenize_operators() -> Result<(), C2RError> {
    let mut tokenizer = Tokenizer::new("test_tokenizer");

    let c_code = b"+ - * / = == != < > <= >= && ||".to_vec();
    let tokens = tokenizer.tokenize(c_code)?;

    // The tokenizer produces operators as Token::l() variants
    let literal_tokens: Vec<&str> = tokens
        .iter()
        .filter_map(|t| if let Token::l(s) = t { Some(*s) } else { None })
        .collect();

    // Check for basic operator characters as literals
    assert!(
        literal_tokens.contains(&"+"),
        "Should tokenize '+' operator"
    );
    assert!(
        literal_tokens.contains(&"-"),
        "Should tokenize '-' operator"
    );
    assert!(
        literal_tokens.contains(&"*"),
        "Should tokenize '*' operator"
    );
    assert!(
        literal_tokens.contains(&"/"),
        "Should tokenize '/' operator"
    );
    assert!(
        literal_tokens.contains(&"="),
        "Should tokenize '=' operator"
    );
    assert!(
        literal_tokens.contains(&"<"),
        "Should tokenize '<' operator"
    );
    assert!(
        literal_tokens.contains(&">"),
        "Should tokenize '>' operator"
    );

    Ok(())
}

/// Test tokenizing string literals
#[test]
fn test_tokenize_string_literals() -> Result<(), C2RError> {
    let mut tokenizer = Tokenizer::new("test_tokenizer");

    let c_code = b"\"hello world\" 'x' \"escape\\\"test\"".to_vec();
    let tokens = tokenizer.tokenize(c_code)?;

    // Check what types of tokens we have
    let string_tokens: Vec<String> = tokens
        .iter()
        .filter_map(|t| {
            if let Token::s(s) = t {
                Some(s.clone())
            } else {
                None
            }
        })
        .collect();
    let char_tokens: Vec<char> = tokens
        .iter()
        .filter_map(|t| if let Token::c(c) = t { Some(*c) } else { None })
        .collect();

    // Should have tokens representing the parsed string and character literals
    assert!(!tokens.is_empty(), "Should have tokens");

    // Check for string literal content (tokenizer parses strings and returns content)
    assert!(
        string_tokens.contains(&"hello world".to_string()),
        "Should tokenize string literal content"
    );
    assert!(
        string_tokens.contains(&"escape\\\"test".to_string()),
        "Should tokenize escaped string literal content"
    );

    // Check for character literal content
    assert!(
        char_tokens.contains(&'x'),
        "Should tokenize character literal"
    );

    Ok(())
}

/// Test tokenizing C comments
#[test]
fn test_tokenize_comments() -> Result<(), C2RError> {
    let mut tokenizer = Tokenizer::new("test_tokenizer");

    let c_code = b"// single line comment\n/* multi line\n   comment */\nint x;".to_vec();
    let tokens = tokenizer.tokenize(c_code)?;

    // Should still tokenize the 'int' and 'x' after comments
    let token_strings: Vec<String> = tokens
        .iter()
        .filter_map(|t| {
            if let Token::s(s) = t {
                Some(s.clone())
            } else {
                None
            }
        })
        .collect();

    assert!(
        token_strings.contains(&"int".to_string()),
        "Should tokenize 'int' after comments"
    );

    Ok(())
}

/// Test tokenizing preprocessor directives
#[test]
fn test_tokenize_preprocessor() -> Result<(), C2RError> {
    let mut tokenizer = Tokenizer::new("test_tokenizer");

    let c_code = b"#include <stdio.h>\n#define MAX 100\n#ifdef DEBUG".to_vec();
    let tokens = tokenizer.tokenize(c_code)?;

    // Check what types of tokens we have
    let literal_tokens: Vec<&str> = tokens
        .iter()
        .filter_map(|t| if let Token::l(s) = t { Some(*s) } else { None })
        .collect();
    let string_tokens: Vec<String> = tokens
        .iter()
        .filter_map(|t| {
            if let Token::s(s) = t {
                Some(s.clone())
            } else {
                None
            }
        })
        .collect();
    let int_tokens: Vec<i128> = tokens
        .iter()
        .filter_map(|t| if let Token::i(n) = t { Some(*n) } else { None })
        .collect();

    // Should have tokens for preprocessor directives
    assert!(
        literal_tokens.contains(&"#"),
        "Should tokenize '#' for preprocessor"
    );
    assert!(
        string_tokens.contains(&"include".to_string()),
        "Should tokenize 'include'"
    );
    assert!(
        string_tokens.contains(&"define".to_string()),
        "Should tokenize 'define'"
    );
    assert!(
        int_tokens.contains(&100),
        "Should tokenize MAX value as integer"
    );

    Ok(())
}

/// Test tokenizing function declarations
#[test]
fn test_tokenize_function_declaration() -> Result<(), C2RError> {
    let mut tokenizer = Tokenizer::new("test_tokenizer");

    let c_code = b"int main(int argc, char* argv[]) { return 0; }".to_vec();
    let tokens = tokenizer.tokenize(c_code)?;

    // Check what types of tokens we have
    let literal_tokens: Vec<&str> = tokens
        .iter()
        .filter_map(|t| if let Token::l(s) = t { Some(*s) } else { None })
        .collect();
    let string_tokens: Vec<&str> = tokens
        .iter()
        .filter_map(|t| {
            if let Token::s(s) = t {
                Some(s.as_str())
            } else {
                None
            }
        })
        .collect();
    let int_tokens: Vec<&i128> = tokens
        .iter()
        .filter_map(|t| if let Token::i(n) = t { Some(n) } else { None })
        .collect();

    // Should have tokens representing the function declaration
    assert!(!tokens.is_empty(), "Should have tokens");

    // Check for function declaration components
    assert!(string_tokens.contains(&"int"), "Should tokenize 'int' type");
    assert!(
        string_tokens.contains(&"main"),
        "Should tokenize 'main' function name"
    );
    assert!(
        string_tokens.contains(&"argc"),
        "Should tokenize 'argc' parameter"
    );
    assert!(
        string_tokens.contains(&"return"),
        "Should tokenize 'return' keyword"
    );

    // Check for punctuation as literal tokens
    assert!(
        literal_tokens.contains(&"("),
        "Should have '(' for parameter list"
    );
    assert!(
        literal_tokens.contains(&")"),
        "Should have ')' for parameter list"
    );
    assert!(
        literal_tokens.contains(&"{"),
        "Should have '{{' for function body"
    );
    assert!(
        literal_tokens.contains(&"}"),
        "Should have '}}' for function body"
    );

    // Check for integer literal
    assert!(int_tokens.contains(&&0), "Should tokenize '0' as integer");

    Ok(())
}
/// Test tokenizing struct definitions
#[test]
fn test_tokenize_struct_definition() -> Result<(), C2RError> {
    let mut tokenizer = Tokenizer::new("test_tokenizer");

    let c_code = b"struct Point { int x; int y; };".to_vec();
    let tokens = tokenizer.tokenize(c_code)?;

    // Check what types of tokens we have
    let literal_tokens: Vec<&str> = tokens
        .iter()
        .filter_map(|t| if let Token::l(s) = t { Some(*s) } else { None })
        .collect();
    let string_tokens: Vec<String> = tokens
        .iter()
        .filter_map(|t| {
            if let Token::s(s) = t {
                Some(s.clone())
            } else {
                None
            }
        })
        .collect();
    let char_tokens: Vec<char> = tokens
        .iter()
        .filter_map(|t| if let Token::c(c) = t { Some(*c) } else { None })
        .collect();

    // Check struct definition components
    assert!(
        string_tokens.contains(&"struct".to_string()),
        "Should have 'struct' keyword"
    );
    assert!(
        string_tokens.contains(&"Point".to_string()),
        "Should have struct name 'Point'"
    );
    assert!(
        string_tokens.contains(&"int".to_string()),
        "Should have 'int' type"
    );

    // Single-character identifiers are tokenized as character literals
    assert!(
        char_tokens.contains(&'x'),
        "Should have field 'x' as character literal"
    );
    assert!(
        char_tokens.contains(&'y'),
        "Should have field 'y' as character literal"
    );

    // Check for punctuation as literal tokens
    assert!(
        literal_tokens.contains(&"{"),
        "Should have '{{' for struct body"
    );
    assert!(
        literal_tokens.contains(&"}"),
        "Should have '}}' for struct body"
    );
    assert!(
        literal_tokens.contains(&";"),
        "Should have ';' for field terminators"
    );

    Ok(())
}

/// Test tokenizing array declarations
#[test]
fn test_tokenize_array_declaration() -> Result<(), C2RError> {
    let mut tokenizer = Tokenizer::new("test_tokenizer");

    let c_code = b"int arr[10]; char buffer[256];".to_vec();
    let tokens = tokenizer.tokenize(c_code)?;

    // Check what types of tokens we have
    let literal_tokens: Vec<&str> = tokens
        .iter()
        .filter_map(|t| if let Token::l(s) = t { Some(*s) } else { None })
        .collect();
    let string_tokens: Vec<&str> = tokens
        .iter()
        .filter_map(|t| {
            if let Token::s(s) = t {
                Some(s.as_str())
            } else {
                None
            }
        })
        .collect();
    let int_tokens: Vec<&i128> = tokens
        .iter()
        .filter_map(|t| if let Token::i(n) = t { Some(n) } else { None })
        .collect();

    // Check array declaration components
    assert!(string_tokens.contains(&"int"), "Should have 'int' type");
    assert!(
        string_tokens.contains(&"arr"),
        "Should have 'arr' identifier"
    );
    assert!(string_tokens.contains(&"char"), "Should have 'char' type");
    assert!(
        string_tokens.contains(&"buffer"),
        "Should have 'buffer' identifier"
    );

    // Check for numeric literals
    assert!(int_tokens.contains(&&10), "Should have array size 10");
    assert!(int_tokens.contains(&&256), "Should have array size 256");

    // Check for punctuation
    assert!(
        literal_tokens.contains(&"["),
        "Should have '[' for array brackets"
    );
    assert!(
        literal_tokens.contains(&"]"),
        "Should have ']' for array brackets"
    );
    assert!(
        literal_tokens.contains(&";"),
        "Should have ';' for statement terminators"
    );

    Ok(())
}

/// Test tokenizing complex expressions
#[test]
fn test_tokenize_complex_expression() -> Result<(), C2RError> {
    let mut tokenizer = Tokenizer::new("test_tokenizer");

    let c_code = b"result = (a + b) * c / d - e % f;".to_vec();
    let tokens = tokenizer.tokenize(c_code)?;

    // Check what types of tokens we have
    let literal_tokens: Vec<&str> = tokens
        .iter()
        .filter_map(|t| if let Token::l(s) = t { Some(*s) } else { None })
        .collect();
    let string_tokens: Vec<&str> = tokens
        .iter()
        .filter_map(|t| {
            if let Token::s(s) = t {
                Some(s.as_str())
            } else {
                None
            }
        })
        .collect();
    let char_tokens: Vec<char> = tokens
        .iter()
        .filter_map(|t| if let Token::c(c) = t { Some(*c) } else { None })
        .collect();

    // Check expression variables - multi-character identifiers are string tokens
    assert!(
        string_tokens.contains(&"result"),
        "Should have 'result' variable"
    );

    // Single-character identifiers are tokenized as character literals
    assert!(
        char_tokens.contains(&'a'),
        "Should have 'a' variable as character literal"
    );
    assert!(
        char_tokens.contains(&'b'),
        "Should have 'b' variable as character literal"
    );
    assert!(
        char_tokens.contains(&'c'),
        "Should have 'c' variable as character literal"
    );
    assert!(
        char_tokens.contains(&'d'),
        "Should have 'd' variable as character literal"
    );
    assert!(
        char_tokens.contains(&'e'),
        "Should have 'e' variable as character literal"
    );
    assert!(
        char_tokens.contains(&'f'),
        "Should have 'f' variable as character literal"
    );

    // Check operators as literal tokens
    assert!(literal_tokens.contains(&"+"), "Should have '+' operator");
    assert!(literal_tokens.contains(&"*"), "Should have '*' operator");
    assert!(literal_tokens.contains(&"/"), "Should have '/' operator");
    assert!(literal_tokens.contains(&"-"), "Should have '-' operator");
    assert!(literal_tokens.contains(&"%"), "Should have '%' operator");
    assert!(literal_tokens.contains(&"="), "Should have '=' operator");

    // Check punctuation as literal tokens
    assert!(literal_tokens.contains(&"("), "Should have '(' parenthesis");
    assert!(literal_tokens.contains(&")"), "Should have ')' parenthesis");
    assert!(literal_tokens.contains(&";"), "Should have ';' semicolon");

    Ok(())
}

/// Test empty input handling
#[test]
fn test_tokenize_empty_input() -> Result<(), C2RError> {
    let mut tokenizer = Tokenizer::new("test_tokenizer");

    let c_code = Vec::new();
    let tokens = tokenizer.tokenize(c_code)?;

    // Empty input should produce empty or minimal token list
    // The exact behavior depends on tokenizer implementation
    assert!(
        tokens.len() <= 1,
        "Empty input should produce minimal tokens"
    );

    Ok(())
}

/// Test whitespace handling
#[test]
fn test_tokenize_whitespace_handling() -> Result<(), C2RError> {
    let mut tokenizer = Tokenizer::new("test_tokenizer");

    let c_code = b"   int   main   (   )   {   }   ".to_vec();
    let tokens = tokenizer.tokenize(c_code)?;

    let token_strings: Vec<String> = tokens
        .iter()
        .filter_map(|t| {
            if let Token::s(s) = t {
                Some(s.clone())
            } else {
                None
            }
        })
        .collect();

    // Should still recognize tokens despite extra whitespace
    assert!(
        token_strings.contains(&"int".to_string()),
        "Should tokenize 'int' despite whitespace"
    );
    assert!(
        token_strings.contains(&"main".to_string()),
        "Should tokenize 'main' despite whitespace"
    );

    Ok(())
}
