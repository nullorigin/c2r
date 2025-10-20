//! Integration tests for handler detection and processing
//! Tests full handler pipeline to identify where failures occur

use crate::{
    array_handler::ArrayHandler, comment_handler::CommentHandler, enum_handler::EnumHandler,
    expression_handler::ExpressionHandler, function_handler::FunctionHandler, global_handler::GlobalHandler,
    loop_handler::LoopHandler, macro_handler::MacroHandler, struct_handler::StructHandler,
    typedef_handler::TypedefHandler, variable_handler::VariableHandler, Context, Handler, Token,
};

/// Helper to create a test context
fn create_test_context() -> Context {
    let mut context = Context::new();
    context.initialize();
    context
}

/// Helper to add tokens to context
fn add_tokens_to_context(context: &mut Context, tokens: Vec<Token>) -> (usize, std::ops::Range<usize>) {
    let token_slot = context.tokenizer.active_slot();
    context.tokenizer.insert_tokens(tokens);
    let range = 0..context.tokenizer.slots()[token_slot].tokens().len();
    (token_slot, range)
}

#[test]
fn test_function_handler_detection() {
    let mut context = create_test_context();
    let handler = FunctionHandler::new();

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

    let (token_slot, range) = add_tokens_to_context(&mut context, tokens);

    match handler.detect(&mut context, token_slot, range.clone()) {
        Ok(detected) => {
            println!("✅ Function detect: {}", detected);
            if !detected {
                println!("❌ ISSUE: Function handler failed to detect simple main function");
            }
            assert!(detected, "Function handler should detect main function");
        }
        Err(e) => {
            panic!("❌ Function handler error: {}", e);
        }
    }
}

#[test]
fn test_struct_handler_detection() {
    let mut context = create_test_context();
    let handler = StructHandler::new();

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
        Token::s(";".to_string()),
    ];

    let (token_slot, range) = add_tokens_to_context(&mut context, tokens);

    match handler.detect(&mut context, token_slot, range) {
        Ok(detected) => {
            println!("Struct detect: {}", detected);
            if !detected {
                println!("❌ ISSUE: Struct handler failed to detect simple struct");
            }
        }
        Err(e) => {
            panic!("❌ Struct handler error: {}", e);
        }
    }
}

#[test]
fn test_enum_handler_detection() {
    let mut context = create_test_context();
    let handler = EnumHandler::new();

    let tokens = vec![
        Token::s("enum".to_string()),
        Token::s("Color".to_string()),
        Token::s("{".to_string()),
        Token::s("RED".to_string()),
        Token::s(",".to_string()),
        Token::s("GREEN".to_string()),
        Token::s(",".to_string()),
        Token::s("BLUE".to_string()),
        Token::s("}".to_string()),
        Token::s(";".to_string()),
    ];

    let (token_slot, range) = add_tokens_to_context(&mut context, tokens);

    match handler.detect(&mut context, token_slot, range) {
        Ok(detected) => {
            println!("Enum detect: {}", detected);
            if !detected {
                println!("❌ ISSUE: Enum handler failed to detect simple enum");
            }
        }
        Err(e) => {
            panic!("❌ Enum handler error: {}", e);
        }
    }
}

#[test]
fn test_typedef_handler_detection() {
    let mut context = create_test_context();
    let handler = TypedefHandler::new();

    let tokens = vec![
        Token::s("typedef".to_string()),
        Token::s("unsigned".to_string()),
        Token::s("int".to_string()),
        Token::s("uint32_t".to_string()),
        Token::s(";".to_string()),
    ];

    let (token_slot, range) = add_tokens_to_context(&mut context, tokens);

    match handler.detect(&mut context, token_slot, range) {
        Ok(detected) => {
            println!("Typedef detect: {}", detected);
            if !detected {
                println!("❌ ISSUE: Typedef handler failed to detect simple typedef");
            }
        }
        Err(e) => {
            panic!("❌ Typedef handler error: {}", e);
        }
    }
}

#[test]
fn test_comment_handler_detection() {
    let mut context = create_test_context();
    let handler = CommentHandler::new();

    let tokens = vec![
        Token::s("//".to_string()),
        Token::s("This".to_string()),
        Token::s("is".to_string()),
        Token::s("a".to_string()),
        Token::s("comment".to_string()),
    ];

    let (token_slot, range) = add_tokens_to_context(&mut context, tokens);

    match handler.detect(&mut context, token_slot, range) {
        Ok(detected) => {
            println!("Comment detect: {}", detected);
            if !detected {
                println!("❌ ISSUE: Comment handler failed to detect simple comment");
            }
        }
        Err(e) => {
            panic!("❌ Comment handler error: {}", e);
        }
    }
}

#[test]
fn test_loop_handler_detection() {
    let mut context = create_test_context();
    let handler = LoopHandler::new();

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

    let (token_slot, range) = add_tokens_to_context(&mut context, tokens);

    match handler.detect(&mut context, token_slot, range) {
        Ok(detected) => {
            println!("Loop detect: {}", detected);
            if !detected {
                println!("❌ ISSUE: Loop handler failed to detect simple for loop");
            }
        }
        Err(e) => {
            panic!("❌ Loop handler error: {}", e);
        }
    }
}

#[test]
fn test_array_handler_detection() {
    let mut context = create_test_context();
    let handler = ArrayHandler::new();

    let tokens = vec![
        Token::s("int".to_string()),
        Token::s("arr".to_string()),
        Token::s("[".to_string()),
        Token::s("10".to_string()),
        Token::s("]".to_string()),
        Token::s(";".to_string()),
    ];

    let (token_slot, range) = add_tokens_to_context(&mut context, tokens);

    match handler.detect(&mut context, token_slot, range) {
        Ok(detected) => {
            println!("Array detect: {}", detected);
            if !detected {
                println!("❌ ISSUE: Array handler failed to detect simple array");
            }
        }
        Err(e) => {
            panic!("❌ Array handler error: {}", e);
        }
    }
}

#[test]
fn test_macro_handler_detection() {
    let mut context = create_test_context();
    let handler = MacroHandler::new();

    let tokens = vec![
        Token::s("#define".to_string()),
        Token::s("MAX".to_string()),
        Token::s("100".to_string()),
    ];

    let (token_slot, range) = add_tokens_to_context(&mut context, tokens);

    match handler.detect(&mut context, token_slot, range) {
        Ok(detected) => {
            println!("Macro detect: {}", detected);
            if !detected {
                println!("❌ ISSUE: Macro handler failed to detect simple #define");
            }
        }
        Err(e) => {
            panic!("❌ Macro handler error: {}", e);
        }
    }
}

#[test]
fn test_global_handler_detection() {
    let mut context = create_test_context();
    let handler = GlobalHandler::new();

    let tokens = vec![
        Token::s("extern".to_string()),
        Token::s("int".to_string()),
        Token::s("global_var".to_string()),
        Token::s(";".to_string()),
    ];

    let (token_slot, range) = add_tokens_to_context(&mut context, tokens);

    match handler.detect(&mut context, token_slot, range) {
        Ok(detected) => {
            println!("Global detect: {}", detected);
            if !detected {
                println!("❌ ISSUE: Global handler failed to detect extern declaration");
            }
        }
        Err(e) => {
            panic!("❌ Global handler error: {}", e);
        }
    }
}

#[test]
fn test_variable_handler_detection() {
    let mut context = create_test_context();
    let handler = VariableHandler::new();

    let tokens = vec![
        Token::s("int".to_string()),
        Token::s("x".to_string()),
        Token::s("=".to_string()),
        Token::s("5".to_string()),
        Token::s(";".to_string()),
    ];

    let (token_slot, range) = add_tokens_to_context(&mut context, tokens);

    match handler.detect(&mut context, token_slot, range) {
        Ok(detected) => {
            println!("Variable detect: {}", detected);
            if !detected {
                println!("❌ ISSUE: Variable handler failed to detect simple variable");
            }
        }
        Err(e) => {
            panic!("❌ Variable handler error: {}", e);
        }
    }
}

#[test]
fn test_expression_handler_detection() {
    let mut context = create_test_context();
    let handler = ExpressionHandler::new();

    let tokens = vec![
        Token::s("x".to_string()),
        Token::s("=".to_string()),
        Token::s("y".to_string()),
        Token::s("+".to_string()),
        Token::s("z".to_string()),
        Token::s(";".to_string()),
    ];

    let (token_slot, range) = add_tokens_to_context(&mut context, tokens);

    match handler.detect(&mut context, token_slot, range) {
        Ok(detected) => {
            println!("Expression detect: {}", detected);
            if !detected {
                println!("❌ ISSUE: Expression handler failed to detect simple expression");
            }
        }
        Err(e) => {
            panic!("❌ Expression handler error: {}", e);
        }
    }
}

#[test]
fn test_handler_confidence_thresholds() {
    println!("\n=== Testing Confidence Thresholds ===");

    let mut context = create_test_context();

    let test_cases = vec![
        ("Function", vec![
            Token::s("int".to_string()), Token::s("func".to_string()), Token::s("(".to_string()), Token::s(")".to_string()), Token::s("{".to_string()), Token::s("}".to_string()),
        ]),
        ("Struct", vec![
            Token::s("struct".to_string()), Token::s("S".to_string()), Token::s("{".to_string()), Token::s("int".to_string()), Token::s("x".to_string()), Token::s(";".to_string()), Token::s("}".to_string()),
        ]),
        ("Macro", vec![
            Token::s("#define".to_string()), Token::s("X".to_string()), Token::s("1".to_string()),
        ]),
    ];

    for (name, tokens) in test_cases {
        let (token_slot, range) = add_tokens_to_context(&mut context, tokens);

        let result = match name {
            "Function" => {
                let handler = FunctionHandler::new();
                handler.detect(&mut context, token_slot, range)
            }
            "Struct" => {
                let handler = StructHandler::new();
                handler.detect(&mut context, token_slot, range)
            }
            "Macro" => {
                let handler = MacroHandler::new();
                handler.detect(&mut context, token_slot, range)
            }
            _ => Ok(false),
        };

        match result {
            Ok(detected) => {
                println!("{}: {}", name, if detected { "✅ PASS" } else { "❌ FAIL" });
            }
            Err(e) => {
                println!("{}: ❌ ERROR: {}", name, e);
            }
        }
    }
}

#[test]
fn test_handler_process_pipeline() {
    println!("\n=== Testing Full Process Pipeline ===");

    let mut context = create_test_context();
    let handler = FunctionHandler::new();

    let tokens = vec![
        Token::s("void".to_string()),
        Token::s("test".to_string()),
        Token::s("(".to_string()),
        Token::s(")".to_string()),
        Token::s("{".to_string()),
        Token::s("}".to_string()),
    ];

    let (token_slot, range) = add_tokens_to_context(&mut context, tokens);

    // Test detect
    match handler.detect(&mut context, token_slot, range.clone()) {
        Ok(true) => {
            println!("✅ detect: PASS");

            // Test extract
            match handler.extract(&mut context, token_slot, range.clone()) {
                Ok(Some(extracted)) => {
                    println!("✅ extract: {:?}", std::mem::discriminant(&extracted));

                    // Test convert
                    match handler.convert(&mut context, extracted) {
                        Ok(Some(_converted)) => {
                            println!("✅ convert: SUCCESS");
                            println!("Pipeline complete!");
                        }
                        Ok(None) => println!("⚠️ convert returned None"),
                        Err(e) => println!("❌ convert error: {}", e),
                    }
                }
                Ok(None) => println!("⚠️ extract returned None"),
                Err(e) => println!("❌ extract error: {}", e),
            }
        }
        Ok(false) => {
            println!("❌ detect: FAIL - Handler rejected valid function");
        }
        Err(e) => {
            println!("❌ detect error: {}", e);
        }
    }
}

#[test]
fn test_edge_cases() {
    println!("\n=== Testing Edge Cases ===");

    let mut context = create_test_context();

    // Empty tokens
    let handler = FunctionHandler::new();
    let tokens: Vec<Token> = vec![];
    let (token_slot, range) = add_tokens_to_context(&mut context, tokens);

    match handler.detect(&mut context, token_slot, range) {
        Ok(false) | Err(_) => println!("✅ Empty tokens: Correctly rejected"),
        Ok(true) => println!("❌ Empty tokens: Should not accept"),
    }

    // Single token
    let tokens = vec![Token::s("int".to_string())];
    let (token_slot, range) = add_tokens_to_context(&mut context, tokens);

    match handler.detect(&mut context, token_slot, range) {
        Ok(false) | Err(_) => println!("✅ Single token: Correctly rejected"),
        Ok(true) => println!("❌ Single token: Should not accept as function"),
    }
}
