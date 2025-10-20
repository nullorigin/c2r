//! Comprehensive tests for Patternizer pattern detection
//! Tests individual pattern matching capabilities to identify detection issues

use crate::{PatternResult, Patternizer, Token};

#[test]
fn test_patternizer_function_detection() {
    let mut patternizer = Patternizer::new();

    // Test simple function declaration
    let tokens = vec![
        Token::s("int".to_string()),
        Token::s("main".to_string()),
        Token::s("(".to_string()),
        Token::s(")".to_string()),
        Token::s("{".to_string()),
        Token::s("}".to_string()),
    ];

    let result = patternizer.match_pattern("function", &tokens);
    match result {
        PatternResult::Match { consumed_tokens } => {
            println!("✅ Function pattern matched, consumed: {}", consumed_tokens);
            assert!(consumed_tokens > 0, "Should consume tokens");
        }
        PatternResult::NoMatch { reason } => {
            panic!("❌ Function pattern failed: {}", reason);
        }
        _ => {
            panic!("❌ Function pattern returned unexpected result: {:?}", result);
        }
    }
}

#[test]
fn test_patternizer_struct_detection() {
    let mut patternizer = Patternizer::new();

    let tokens = vec![
        Token::s("struct".to_string()),
        Token::s("Point".to_string()),
        Token::s("{".to_string()),
        Token::s("int".to_string()),
        Token::s("x".to_string()),
        Token::s(";".to_string()),
        Token::s("}".to_string()),
    ];

    let result = patternizer.match_pattern("struct", &tokens);
    match result {
        PatternResult::Match { consumed_tokens } => {
            println!("✅ Struct pattern matched, consumed: {}", consumed_tokens);
        }
        PatternResult::NoMatch { reason } => {
            panic!("❌ Struct pattern failed: {}", reason);
        }
        _ => {
            panic!("❌ Struct pattern returned unexpected result: {:?}", result);
        }
    }
}

#[test]
fn test_patternizer_enum_detection() {
    let mut patternizer = Patternizer::new();

    let tokens = vec![
        Token::s("enum".to_string()),
        Token::s("Color".to_string()),
        Token::s("{".to_string()),
        Token::s("RED".to_string()),
        Token::s(",".to_string()),
        Token::s("GREEN".to_string()),
        Token::s("}".to_string()),
    ];

    let result = patternizer.match_pattern("enum", &tokens);
    match result {
        PatternResult::Match { consumed_tokens } => {
            println!("✅ Enum pattern matched, consumed: {}", consumed_tokens);
        }
        PatternResult::NoMatch { reason } => {
            panic!("❌ Enum pattern failed: {}", reason);
        }
        _ => {
            panic!("❌ Enum pattern returned unexpected result: {:?}", result);
        }
    }
}

#[test]
fn test_patternizer_typedef_detection() {
    let mut patternizer = Patternizer::new();

    let tokens = vec![
        Token::s("typedef".to_string()),
        Token::s("struct".to_string()),
        Token::s("Point".to_string()),
        Token::s("Point".to_string()),
        Token::s(";".to_string()),
    ];

    let result = patternizer.match_pattern("typedef", &tokens);
    match result {
        PatternResult::Match { consumed_tokens } => {
            println!("✅ Typedef pattern matched, consumed: {}", consumed_tokens);
        }
        PatternResult::NoMatch { reason } => {
            panic!("❌ Typedef pattern failed: {}", reason);
        }
        _ => {
            panic!("❌ Typedef pattern returned unexpected result: {:?}", result);
        }
    }
}

#[test]
fn test_patternizer_loop_detection() {
    let mut patternizer = Patternizer::new();

    // For loop
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

    let result = patternizer.match_pattern("loop", &tokens);
    match result {
        PatternResult::Match { consumed_tokens } => {
            println!("✅ Loop pattern matched, consumed: {}", consumed_tokens);
        }
        PatternResult::NoMatch { reason } => {
            panic!("❌ Loop pattern failed: {}", reason);
        }
        _ => {
            panic!("❌ Loop pattern returned unexpected result: {:?}", result);
        }
    }
}

#[test]
fn test_patternizer_array_detection() {
    let mut patternizer = Patternizer::new();

    let tokens = vec![
        Token::s("int".to_string()),
        Token::s("arr".to_string()),
        Token::s("[".to_string()),
        Token::s("10".to_string()),
        Token::s("]".to_string()),
        Token::s(";".to_string()),
    ];

    let result = patternizer.match_pattern("array", &tokens);
    match result {
        PatternResult::Match { consumed_tokens } => {
            println!("✅ Array pattern matched, consumed: {}", consumed_tokens);
        }
        PatternResult::NoMatch { reason } => {
            panic!("❌ Array pattern failed: {}", reason);
        }
        _ => {
            panic!("❌ Array pattern returned unexpected result: {:?}", result);
        }
    }
}

#[test]
fn test_patternizer_macro_detection() {
    let mut patternizer = Patternizer::new();

    let tokens = vec![
        Token::s("#define".to_string()),
        Token::s("MAX".to_string()),
        Token::s("100".to_string()),
    ];

    let result = patternizer.match_pattern("macro", &tokens);
    match result {
        PatternResult::Match { consumed_tokens } => {
            println!("✅ Macro pattern matched, consumed: {}", consumed_tokens);
        }
        PatternResult::NoMatch { reason } => {
            panic!("❌ Macro pattern failed: {}", reason);
        }
        _ => {
            panic!("❌ Macro pattern returned unexpected result: {:?}", result);
        }
    }
}

#[test]
fn test_patternizer_comment_detection() {
    let mut patternizer = Patternizer::new();

    let tokens = vec![
        Token::s("//".to_string()),
        Token::s("This".to_string()),
        Token::s("is".to_string()),
        Token::s("a".to_string()),
        Token::s("comment".to_string()),
    ];

    let result = patternizer.match_pattern("comment", &tokens);
    match result {
        PatternResult::Match { consumed_tokens } => {
            println!("✅ Comment pattern matched, consumed: {}", consumed_tokens);
        }
        PatternResult::NoMatch { reason } => {
            panic!("❌ Comment pattern failed: {}", reason);
        }
        _ => {
            panic!("❌ Comment pattern returned unexpected result: {:?}", result);
        }
    }
}

#[test]
fn test_patternizer_global_detection() {
    let mut patternizer = Patternizer::new();

    let tokens = vec![
        Token::s("extern".to_string()),
        Token::s("int".to_string()),
        Token::s("global_var".to_string()),
        Token::s(";".to_string()),
    ];

    let result = patternizer.match_pattern("global", &tokens);
    match result {
        PatternResult::Match { consumed_tokens } => {
            println!("✅ Global pattern matched, consumed: {}", consumed_tokens);
        }
        PatternResult::NoMatch { reason } => {
            panic!("❌ Global pattern failed: {}", reason);
        }
        _ => {
            panic!("❌ Global pattern returned unexpected result: {:?}", result);
        }
    }
}

#[test]
fn test_patternizer_type_detection() {
    let patternizer = Patternizer::new();

    let types = vec!["int", "char", "float", "double", "void", "long", "short"];

    for type_str in types {
        let token = Token::s(type_str.to_string());
        assert!(patternizer.is_c_type(&token), "Should detect {} as C type", type_str);
    }

    println!("✅ All basic C types detected correctly");
}

#[test]
fn test_patternizer_scoring_functions() {
    let patternizer = Patternizer::new();

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

    let structure_score = patternizer.calculate_structure_score(&tokens);
    let type_score = patternizer.calculate_type_score(&tokens);
    let syntax_score = patternizer.calculate_syntax_score(&tokens);

    println!("Patternizer Scores:");
    println!("  Structure: {:.2}", structure_score);
    println!("  Type: {:.2}", type_score);
    println!("  Syntax: {:.2}", syntax_score);

    assert!(structure_score >= 0.0 && structure_score <= 1.0);
    assert!(type_score >= 0.0 && type_score <= 1.0);
    assert!(syntax_score >= 0.0 && syntax_score <= 1.0);
}

#[test]
fn test_patternizer_empty_tokens() {
    let mut patternizer = Patternizer::new();

    let tokens: Vec<Token> = vec![];
    let result = patternizer.match_pattern("function", &tokens);

    match result {
        PatternResult::NoMatch { reason } => {
            println!("✅ Correctly rejected empty tokens: {}", reason);
        }
        PatternResult::Match { .. } => {
            panic!("❌ Should not match empty tokens");
        }
        _ => {
            println!("✅ Correctly rejected empty tokens: {:?}", result);
        }
    }
}

#[test]
fn test_patternizer_single_token() {
    let mut patternizer = Patternizer::new();

    let tokens = vec![Token::s("int".to_string())];
    let result = patternizer.match_pattern("function", &tokens);

    match result {
        PatternResult::NoMatch { reason } => {
            println!("✅ Correctly rejected single token for function: {}", reason);
        }
        PatternResult::Match { .. } => {
            panic!("❌ Should not match function with single token");
        }
        _ => {
            println!("✅ Correctly rejected single token for function: {:?}", result);
        }
    }
}
