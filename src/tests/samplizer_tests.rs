//! Comprehensive tests for Samplizer confidence scoring
//! Tests pattern analysis and confidence calculation to identify scoring issues

use crate::{Samplizer, Token};
use std::ops::Range;

#[test]
fn test_samplizer_function_confidence() {
    let samplizer = Samplizer::new();

    let tokens = vec![
        Token::s("int".to_string()),
        Token::s("main".to_string()),
        Token::s("(".to_string()),
        Token::s(")".to_string()),
        Token::s("{".to_string()),
        Token::s("return".to_string()),
        Token::s("0".to_string()),
        Token::s(";".to_string()),
        Token::s("}".to_string()),
    ];

    let patterns = vec!["function_declaration".to_string(), "function_definition".to_string()];
    let range: Range<usize> = 0..tokens.len();

    match samplizer.analyze_with_range(&patterns, &tokens, range) {
        Ok(confidence) => {
            println!("Function confidence: {:.2}", confidence);
            assert!(confidence > 0.0, "Should have some confidence for function");
            assert!(confidence <= 1.0, "Confidence should be <= 1.0");
        }
        Err(e) => panic!("Samplizer error: {}", e),
    }
}

#[test]
fn test_samplizer_struct_confidence() {
    let samplizer = Samplizer::new();

    let tokens = vec![
        Token::s("struct".to_string()),
        Token::s("Point".to_string()),
        Token::s("{".to_string()),
        Token::s("int".to_string()),
        Token::s("x".to_string()),
        Token::s(";".to_string()),
        Token::s("int".to_string()),
        Token::s("y".to_string()),
        Token::s(";".to_string()),
        Token::s("}".to_string()),
    ];

    let patterns = vec!["struct_declaration".to_string(), "struct_definition".to_string()];
    let range: Range<usize> = 0..tokens.len();

    match samplizer.analyze_with_range(&patterns, &tokens, range) {
        Ok(confidence) => {
            println!("Struct confidence: {:.2}", confidence);
            assert!(confidence > 0.0);
        }
        Err(e) => panic!("Samplizer error: {}", e),
    }
}

#[test]
fn test_samplizer_loop_confidence() {
    let samplizer = Samplizer::new();

    let tokens = vec![
        Token::s("for".to_string()),
        Token::s("(".to_string()),
        Token::s("int".to_string()),
        Token::s("i".to_string()),
        Token::s("=".to_string()),
        Token::s("0".to_string()),
        Token::s(";".to_string()),
        Token::s("i".to_string()),
        Token::s("<".to_string()),
        Token::s("10".to_string()),
        Token::s(";".to_string()),
        Token::s("i".to_string()),
        Token::s("++".to_string()),
        Token::s(")".to_string()),
        Token::s("{".to_string()),
        Token::s("}".to_string()),
    ];

    let patterns = vec!["for_loop".to_string(), "loop_construct".to_string()];
    let range: Range<usize> = 0..tokens.len();

    match samplizer.analyze_with_range(&patterns, &tokens, range) {
        Ok(confidence) => {
            println!("Loop confidence: {:.2}", confidence);
            assert!(confidence > 0.0);
        }
        Err(e) => panic!("Samplizer error: {}", e),
    }
}

#[test]
fn test_samplizer_empty_tokens() {
    let samplizer = Samplizer::new();

    let tokens: Vec<Token> = vec![];
    let patterns = vec!["function_declaration".to_string()];
    let range: Range<usize> = 0..0;

    match samplizer.analyze_with_range(&patterns, &tokens, range) {
        Ok(confidence) => {
            println!("Empty tokens confidence: {:.2}", confidence);
            assert_eq!(confidence, 0.0, "Empty tokens should have 0 confidence");
        }
        Err(_) => {
            println!("✅ Correctly rejected empty tokens");
        }
    }
}

#[test]
fn test_samplizer_invalid_pattern() {
    let samplizer = Samplizer::new();

    let tokens = vec![Token::s("int".to_string()), Token::s("x".to_string())];
    let patterns = vec!["nonexistent_pattern".to_string()];
    let range: Range<usize> = 0..tokens.len();

    match samplizer.analyze_with_range(&patterns, &tokens, range) {
        Ok(confidence) => {
            println!("Invalid pattern confidence: {:.2}", confidence);
            // Should return low confidence for unknown patterns
            assert!(confidence < 0.5);
        }
        Err(_) => {
            println!("✅ Correctly handled invalid pattern");
        }
    }
}

#[test]
fn test_samplizer_multiple_patterns() {
    let samplizer = Samplizer::new();

    let tokens = vec![
        Token::s("typedef".to_string()),
        Token::s("struct".to_string()),
        Token::s("Point".to_string()),
        Token::s("{".to_string()),
        Token::s("int".to_string()),
        Token::s("x".to_string()),
        Token::s(";".to_string()),
        Token::s("}".to_string()),
        Token::s("Point".to_string()),
        Token::s(";".to_string()),
    ];

    let patterns = vec![
        "typedef_struct".to_string(),
        "struct_definition".to_string(),
        "typedef".to_string(),
    ];
    let range: Range<usize> = 0..tokens.len();

    match samplizer.analyze_with_range(&patterns, &tokens, range) {
        Ok(confidence) => {
            println!("Multiple patterns confidence: {:.2}", confidence);
            assert!(confidence > 0.0);
        }
        Err(e) => panic!("Samplizer error: {}", e),
    }
}

#[test]
fn test_samplizer_confidence_bounds() {
    let samplizer = Samplizer::new();

    let test_cases = vec![
        (vec![Token::s("int".to_string()), Token::s("x".to_string()), Token::s(";".to_string())], vec!["variable_declaration".to_string()]),
        (vec![Token::s("extern".to_string()), Token::s("int".to_string()), Token::s("g".to_string())], vec!["global_declaration".to_string()]),
        (vec![Token::s("#define".to_string()), Token::s("MAX".to_string()), Token::s("100".to_string())], vec!["define".to_string()]),
    ];

    for (tokens, patterns) in test_cases {
        let range: Range<usize> = 0..tokens.len();
        match samplizer.analyze_with_range(&patterns, &tokens, range) {
            Ok(confidence) => {
                println!("Pattern {:?} confidence: {:.2}", patterns[0], confidence);
                assert!(confidence >= 0.0 && confidence <= 1.0,
                        "Confidence must be between 0.0 and 1.0, got {}", confidence);
            }
            Err(e) => {
                println!("Pattern {:?} failed: {}", patterns[0], e);
            }
        }
    }
}

#[test]
fn test_samplizer_range_sensitivity() {
    let samplizer = Samplizer::new();

    let tokens = vec![
        Token::s("int".to_string()),
        Token::s("add".to_string()),
        Token::s("(".to_string()),
        Token::s("int".to_string()),
        Token::s("a".to_string()),
        Token::s(",".to_string()),
        Token::s("int".to_string()),
        Token::s("b".to_string()),
        Token::s(")".to_string()),
        Token::s("{".to_string()),
        Token::s("return".to_string()),
        Token::s("a".to_string()),
        Token::s("+".to_string()),
        Token::s("b".to_string()),
        Token::s(";".to_string()),
        Token::s("}".to_string()),
    ];

    let patterns = vec!["function_definition".to_string()];

    // Test different ranges
    let full_range: Range<usize> = 0..tokens.len();
    let partial_range: Range<usize> = 0..9; // Just signature

    let full_confidence = samplizer.analyze_with_range(&patterns, &tokens, full_range).unwrap_or(0.0);
    let partial_confidence = samplizer.analyze_with_range(&patterns, &tokens, partial_range).unwrap_or(0.0);

    println!("Full function confidence: {:.2}", full_confidence);
    println!("Partial function confidence: {:.2}", partial_confidence);

    // Full function should have higher confidence than partial
    assert!(full_confidence >= partial_confidence,
            "Full function should have >= confidence than partial");
}

#[test]
fn test_samplizer_with_captures() {
    let samplizer = Samplizer::new();

    let tokens = vec![
        Token::s("int".to_string()),
        Token::s("main".to_string()),
        Token::s("(".to_string()),
        Token::s(")".to_string()),
        Token::s("{".to_string()),
        Token::s("}".to_string()),
    ];

    let pattern_name = "function_declaration";

    match samplizer.analyze_with_captures(&tokens, pattern_name) {
        Ok(result) => {
            println!("Function with captures:");
            println!("  Success: {}", result.success);
            println!("  Confidence: {:.2}", result.confidence);
            println!("  Tokens consumed: {}", result.captures.pattern_metadata.tokens_consumed);

            assert!(result.confidence > 0.0);
        }
        Err(e) => {
            println!("analyze_with_captures error: {}", e);
        }
    }
}

#[test]
fn test_samplizer_pattern_priority() {
    let samplizer = Samplizer::new();

    let tokens = vec![
        Token::s("typedef".to_string()),
        Token::s("struct".to_string()),
        Token::s("Point".to_string()),
        Token::s("Point".to_string()),
        Token::s(";".to_string()),
    ];

    // Test each pattern individually to see which has highest confidence
    let patterns = vec!["typedef", "struct", "typedef_struct"];
    let range: Range<usize> = 0..tokens.len();

    for pattern in patterns {
        match samplizer.analyze_with_range(&vec![pattern.to_string()], &tokens, range.clone()) {
            Ok(confidence) => {
                println!("Pattern '{}' confidence: {:.2}", pattern, confidence);
            }
            Err(e) => {
                println!("Pattern '{}' error: {}", pattern, e);
            }
        }
    }
}
