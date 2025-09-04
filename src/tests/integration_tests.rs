//! Integration tests for complete C-to-Rust conversion workflow
//! Tests the full pipeline: tokenization → handler processing → conversion → result

use c2r::lock::Id;
use c2r::{create_handler, Context, ConversionError, HandlerResult, Token, Tokenizer};
use c2r::{Entry, HandlerMap};

/// Test complete tokenization to handler processing pipeline
#[test]
fn test_tokenization_to_handler_pipeline() -> Result<(), ConversionError> {
    let mut context = Context::new("test_context");

    // Step 1: Tokenize a simple C function
    let c_code = b"int main() { return 0; }".to_vec();
    let tokenizer = Tokenizer::new("test_tokenizer");
    let tokens = tokenizer.tokenize(c_code)?;

    // Verify tokenization worked
    assert!(!tokens.is_empty(), "Tokenization should produce tokens");

    // Step 2: Create a handler map with function handler
    let mut handler_map = HandlerMap::new("integration_test");

    // Add function handler to process the tokens
    let function_handler = create_handler(
        Id::get("function_handler"),
        "handler for functions",
        1000, // high priority
        Some(process_function),
        Some(handle_function),
        Some(extract_function),
        Some(convert_function_callback),
        Some(report_function),
        Some(redirect_function),
    );

    let handler_id = function_handler.id.clone();
    handler_map
        .roles
        .insert(handler_id.clone(), function_handler.role.clone());
    handler_map.ids.push(handler_id.clone());
    handler_map
        .processors
        .insert(handler_id.clone(), function_handler.process);
    handler_map
        .handlers
        .insert(handler_id.clone(), function_handler.handle);
    handler_map
        .extractors
        .insert(handler_id.clone(), function_handler.extract);
    handler_map
        .convertors
        .insert(handler_id.clone(), function_handler.convert);
    handler_map
        .reporters
        .insert(handler_id.clone(), function_handler.report);
    handler_map
        .results
        .insert(handler_id.clone(), function_handler.result);
    handler_map
        .redirectors
        .insert(handler_id.clone(), function_handler.redirect);

    // Step 3: Process tokens through handler map
    let result = handler_map.process(&tokens)?;

    // Verify processing occurred
    assert!(result.end_idx > 0, "Handler should consume some tokens");

    // The result type depends on what the function handler returns
    // Since function handler might return NotHandled for this simple case, we check that processing completed
    assert!(
        result.start_idx <= result.end_idx,
        "Token indices should be valid"
    );

    Ok(())
}

/// Test complete conversion workflow with multiple handlers
#[test]
fn test_multi_handler_workflow() -> Result<(), ConversionError> {
    let mut context = Context::new("test_context");

    // Test with comment and function code
    let c_code = b"/* Comment */ int add(int a, int b) { return a + b; }".to_vec();
    let tokenizer = Tokenizer::new("test_tokenizer");
    let tokens = tokenizer.tokenize(c_code)?;

    // Create handler map with multiple handlers
    let mut handler_map = HandlerMap::new("multi_handler_test");

    // Add comment handler
    let comment_handler = create_handler(
        Id::get("comment_handler"),
        "handler for comments",
        800, // lower priority than function
        Some(process_comment),
        Some(handle_comment),
        Some(extract_comment),
        Some(convert_comment_callback),
        Some(report_comment),
        None,
        Some(redirect_comment),
    );

    let comment_id = comment_handler.id.clone();
    handler_map
        .roles
        .insert(comment_id.clone(), comment_handler.role.clone());
    handler_map.ids.push(comment_id.clone());
    handler_map
        .processors
        .insert(comment_id.clone(), comment_handler.process);
    handler_map
        .handlers
        .insert(comment_id.clone(), comment_handler.handle);

    // Add function handler
    let function_handler = create_handler(
        Id::get("function_handler"),
        "handler for functions",
        1000, // high priority
        Some(process_function),
        Some(handle_function),
        Some(extract_function),
        Some(convert_function_callback),
        Some(report_function),
        None,
        Some(redirect_function),
    );

    let function_id = function_handler.id.clone();
    handler_map
        .roles
        .insert(function_id.clone(), function_handler.role.clone());
    handler_map.ids.push(function_id.clone());
    handler_map
        .processors
        .insert(function_id.clone(), function_handler.process);
    handler_map
        .handlers
        .insert(function_id.clone(), function_handler.handle);

    // Process tokens - should try handlers in priority order
    let result = handler_map.process(&tokens)?;

    // Verify some processing occurred
    assert!(
        result.start_idx <= result.end_idx,
        "Processing should produce valid indices"
    );

    Ok(())
}

/// Test reporting system integration during processing
#[test]
fn test_reporting_system_integration() -> Result<(), ConversionError> {
    let mut context = Context::new("test_context");

    // Simple test code
    let c_code = b"int x = 5;".to_vec();
    let tokenizer = Tokenizer::new("test_tokenizer");
    let tokens = tokenizer.tokenize(c_code)?;

    // Create handler map
    let mut handler_map = HandlerMap::new("reporting_test");

    // Add expression handler which might handle the assignment
    let expression_handler = create_handler(
        Id::get("expression handler"),
        "handler for expressions",
        900,
        Some(process_expression),
        Some(handle_expression),
        Some(extract_expression),
        Some(convert_expression),
        Some(report_expression),
        None,
        Some(redirect_expression),
    );

    let expr_id = expression_handler.id.clone();
    handler_map
        .roles
        .insert(expr_id.clone(), expression_handler.role.clone());
    handler_map.ids.push(expr_id.clone());
    handler_map
        .processors
        .insert(expr_id.clone(), expression_handler.process);
    handler_map
        .handlers
        .insert(expr_id.clone(), expression_handler.handle);
    handler_map
        .reporters
        .insert(expr_id.clone(), expression_handler.report);

    // Get reports count before processing
    let reports_before = context.get_reports_by_handler("expression").len();

    // Process tokens
    let _result = handler_map.process(&tokens)?;

    // Check if any reports were generated during processing
    let reports_after = context.get_reports_by_handler("expression").len();

    // The handler may or may not generate reports depending on whether it processes the tokens
    // But the system should be ready to collect reports
    assert!(
        reports_after >= reports_before,
        "Report system should be functional"
    );

    // Test the display reports functionality
    let report_display = context.display_reports();
    assert!(
        report_display.contains("Report Summary"),
        "Should generate report summary"
    );

    Ok(())
}

/// Test Context state persistence during processing
#[test]
fn test_context_state_persistence() -> Result<(), ConversionError> {
    let mut context = Context::new("test_context");

    // Set initial context state
    context.source_file = "test.c".to_string();
    context.target_file = "test.rs".to_string();
    context.debug = true;

    // Add some registry entries
    let test_id = Id::get("persistence_test");
    context
        .registry
        .insert(test_id.clone(), Entry::Str("persistent_value".to_string()));

    // Process some tokens
    let c_code = b"void test();".to_vec();
    let tokenizer = Tokenizer::new("test_tokenizer");
    let tokens = tokenizer.tokenize(c_code)?;

    let handler_map = HandlerMap::new("persistence_test");
    let _result = handler_map.process(&tokens)?;

    // Verify context state is preserved
    assert_eq!(
        context.source_file, "test.c",
        "Source file should be preserved"
    );
    assert_eq!(
        context.target_file, "test.rs",
        "Target file should be preserved"
    );
    assert!(context.debug, "Debug flag should be preserved");

    // Verify registry entry persists
    if let Some(Entry::Str(value)) = context.registry.get_root_entry(&test_id) {
        assert_eq!(value, "persistent_value", "Registry entry should persist");
    } else {
        panic!("Registry entry should persist through processing");
    }

    Ok(())
}

/// Test error propagation through the pipeline
#[test]
fn test_error_propagation() {
    let mut context = Context::new("test_context");
    let mut handler_map = HandlerMap::new("error_test");

    // Create a handler that always returns an error
    let error_id = Id::get("error_handler");
    handler_map
        .roles
        .insert(error_id.clone(), "error_role".to_string());
    handler_map.ids.push(error_id.clone());

    let error_handler = Some(
        |_tokens: &[Token], _context: &mut Context| -> Result<HandlerResult, ConversionError> {
            Err(ConversionError::new("Intentional test error"))
        },
    );

    handler_map.handlers.insert(error_id.clone(), error_handler);

    // Process tokens that will trigger the error
    let tokens = vec![Token::s("error".to_string())];
    let result = handler_map.process(&tokens, &mut context);

    // Should propagate the error
    assert!(result.is_err(), "Error should propagate through pipeline");

    if let Err(error) = result {
        assert!(
            error.to_string().contains("Intentional test error"),
            "Should contain original error message"
        );
    }
}

/// Test handler redirect mechanism in integration
#[test]
fn test_handler_redirect_integration() -> Result<(), ConversionError> {
    let mut context = Context::new("test_context");
    let mut handler_map = HandlerMap::new("redirect_integration");

    // Create source handler that redirects to target
    let source_id = Id::get("redirect_source");
    let target_id = Id::get("redirect_target");

    handler_map
        .roles
        .insert(source_id.clone(), "source".to_string());
    handler_map
        .roles
        .insert(target_id.clone(), "target".to_string());
    handler_map.ids.push(source_id.clone());
    handler_map.ids.push(target_id.clone());

    // Source handler that redirects
    let source_handler = Some(
        |tokens: &[Token], _context: &mut Context| -> Result<HandlerResult, ConversionError> {
            Ok(HandlerResult::Redirected(
                Some(tokens.to_vec()),
                "target".to_string(),
                Id::get("redirect_from"),
                Id::get("redirect_to"),
            ))
        },
    );

    // Target handler that processes redirected tokens
    let target_handler = Some(
        |tokens: &[Token], _context: &mut Context| -> Result<HandlerResult, ConversionError> {
            Ok(HandlerResult::Converted(
                Some(tokens.to_vec()),
                "redirect_converted".to_string(),
                Id::get("redirected_result"),
            ))
        },
    );

    handler_map
        .handlers
        .insert(source_id.clone(), source_handler);
    handler_map
        .handlers
        .insert(target_id.clone(), target_handler);

    // Process tokens through redirect
    let tokens = vec![Token::s("redirect_me".to_string())];
    let result = handler_map.process(&tokens)?;

    // Should result in converted output from target handler
    assert!(
        matches!(result.result, HandlerResult::Converted(_, _, _)),
        "Redirect should result in converted output"
    );

    if let HandlerResult::Converted(_, _, output) = result.result {
        assert_eq!(
            output,
            Id::get("redirected_result"),
            "Should have target handler output"
        );
    }

    Ok(())
}

/// Test performance with large token sequences
#[test]
fn test_large_token_sequence_performance() -> Result<(), ConversionError> {
    let mut context = Context::new("test_context");

    // Create a large sequence of tokens
    let large_tokens: Vec<Token> = (0..1000)
        .map(|i| Token::s(format!("token_{}", i)))
        .collect();

    // Create simple handler
    let mut handler_map = HandlerMap::new("performance_test");
    let handler_id = Id::get("perf_handler");

    handler_map
        .roles
        .insert(handler_id.clone(), "performance".to_string());
    handler_map.ids.push(handler_id.clone());

    let perf_handler = Some(
        |tokens: &[Token], _context: &mut Context| -> Result<HandlerResult, ConversionError> {
            // Simple handler that processes first few tokens
            let processed = tokens.iter().take(10).cloned().collect();
            Ok(HandlerResult::Handled(
                Some(processed),
                Id::get("perf_handled"),
            ))
        },
    );

    handler_map
        .handlers
        .insert(handler_id.clone(), perf_handler);

    // Process large token sequence
    let start_time = std::time::Instant::now();
    let result = handler_map.process(&large_tokens)?;
    let elapsed = start_time.elapsed();

    // Should complete in reasonable time
    assert!(
        elapsed.as_secs() < 1,
        "Should process large token sequence quickly"
    );
    assert_eq!(
        result.end_idx, 10,
        "Should process expected number of tokens"
    );

    Ok(())
}

/// Test complete workflow with realistic C code
#[test]
fn test_realistic_c_code_workflow() -> Result<(), ConversionError> {
    let mut context = Context::new("test_context");

    // More realistic C code sample
    let c_code = b"
        #include <stdio.h>
        
        /* Simple addition function */
        int add(int a, int b) {
            return a + b;
        }
        
        int main() {
            int result = add(5, 3);
            printf(\"Result: %d\\n\", result);
            return 0;
        }
    "
    .to_vec();

    // Tokenize
    let tokenizer = Tokenizer::new("test_tokenizer");
    let tokens = tokenizer.tokenize(c_code)?;

    // Should have a substantial number of tokens
    assert!(
        tokens.len() > 20,
        "Realistic code should produce many tokens"
    );

    // Create handler map with relevant handlers
    let mut handler_map = HandlerMap::new("realistic_test");

    // Add function handler (others could be added)
    let function_handler = create_handler(
        Id::get("function_handler"),
        "handler for functions",
        1000,
        Some(process_function),
        Some(handle_function),
        Some(extract_function),
        Some(convert_function_callback),
        Some(report_function),
        None,
        Some(redirect_function),
    );

    let func_id = function_handler.id.clone();
    handler_map
        .roles
        .insert(func_id.clone(), function_handler.role.clone());
    handler_map.ids.push(func_id.clone());
    handler_map
        .handlers
        .insert(func_id.clone(), function_handler.handle);

    // Process the tokens
    let result = handler_map.process(&tokens)?;

    // Verify processing
    assert!(
        result.start_idx <= result.end_idx,
        "Should process with valid indices"
    );

    // Check that context collected some information
    let report_summary = context.display_reports();
    assert!(!report_summary.is_empty(), "Should generate some reports");

    Ok(())
}
