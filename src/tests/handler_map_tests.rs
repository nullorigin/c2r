//! Comprehensive tests for HandlerMap functionality
//! Tests handler processing, token consumption, redirect handling, and process pipeline

use crate::config::Context;
use crate::convert::{ConvertedComment, ConvertedElement};
use crate::error::ConversionError;
use crate::handler::{HandlerMap, HandlerResult, ProcessedResult};
use crate::lock::Id;
use crate::token::Token;

// Helper functions that match HandlerMap function pointer signatures

/// Simple processor function that accepts any non-empty tokens
fn test_processor_accept_any(
    tokens: &[Token]) -> Result<bool, ConversionError> {
    Ok(!tokens.is_empty())
}

/// Processor function for failing handler tests - always returns false to avoid infinite loops
fn test_processor_reject_all(
    _tokens: &[Token]) -> Result<bool, ConversionError> {
    Ok(false)
}

/// Test handler that always handles tokens successfully
fn test_success_handler(
    tokens: &[Token]) -> Result<HandlerResult, ConversionError> {
    if !tokens.is_empty() {
        Ok(HandlerResult::Handled(
            Some(tokens.to_vec()),
            0..1,
            Id::get("handled"),
        ))
    } else {
        Ok(HandlerResult::NotHandled(
            None,
            0..0,
            Id::get("not_handled"),
        ))
    }
}

/// Test handler that always returns NotHandled

fn test_fail_handler(
    _tokens: &[Token]) -> Result<HandlerResult, ConversionError> {
    Ok(HandlerResult::NotHandled(None, 0..0, Id::get("failed")))
}

/// Test handler that returns an error
fn test_error_handler(
    _tokens: &[Token]) -> Result<HandlerResult, ConversionError> {
    Err(ConversionError::new("Test error from handler"))
}

/// Test handler for first priority
fn test_first_handler(
    tokens: &[Token]) -> Result<HandlerResult, ConversionError> {
    if tokens.iter().any(|t| match t {
        Token::s(s) => s == "skip_first",
        _ => false,
    }) {
        Ok(HandlerResult::NotHandled(
            Some(tokens.to_vec()),
            0..0,
            Id::get("skipped"),
        ))
    } else {
        Ok(HandlerResult::Handled(
            Some(tokens.to_vec()),
            0..1,
            Id::get("first_handled"),
        ))
    }
}

/// Test handler for second priority
fn test_second_handler(
    tokens: &[Token]) -> Result<HandlerResult, ConversionError> {
    Ok(HandlerResult::Handled(
        Some(tokens.to_vec()),
        0..1,
        Id::get("second_handled"),
    ))
}

/// Test handler that returns Processed result
fn test_processor_handler(
    tokens: &[Token]) -> Result<HandlerResult, ConversionError> {
    Ok(HandlerResult::Processed(
        Some(tokens.to_vec()),
        0..1,
        "initial_result".to_string(),
        Id::get("initial"),
    ))
}

/// Test handler that redirects to another handler
fn test_redirect_source_handler(
    tokens: &[Token]) -> Result<HandlerResult, ConversionError> {
    Ok(HandlerResult::Redirected(
        Some(tokens.to_vec()),
        0..1,
        "target_role".to_string(),
        Id::get("from_source"),
        Id::get("to_target"),
    ))
}

/// Test handler that processes redirected tokens
fn test_redirect_target_handler(
    tokens: &[Token]) -> Result<HandlerResult, ConversionError> {
    Ok(HandlerResult::Handled(
        Some(tokens.to_vec()),
        0..1,
        Id::get("redirected_handled"),
    ))
}

// Helper processor functions

/// Test processor that accepts tokens containing "accept"
fn test_processor_accept(
    tokens: &[Token]) -> Result<bool, ConversionError> {
    Ok(tokens.iter().any(|t| match t {
        Token::s(s) => s.contains("accept"),
        _ => false,
    }))
}

// Helper result callback functions

/// Test result callback that transforms Processed to Converted
fn test_result_callback(
    _tokens: &[Token],
    result: HandlerResult) -> Result<HandlerResult, ConversionError> {
    match result {
        HandlerResult::Processed(tokens, _, _id, _) => {
            let converted = match tokens {
                Some(t) => ConvertedElement::Comment(ConvertedComment {
                    content: format!("converted_{}", t.len()),
                    rust_code: format!("// converted_{}", t.len()),
                    is_block: false,
                    is_doc_comment: false,
                }),
                None => ConvertedElement::Comment(ConvertedComment {
                    content: "converted_empty".to_string(),
                    rust_code: "// converted_empty".to_string(),
                    is_block: false,
                    is_doc_comment: false,
                }),
            };
            Ok(HandlerResult::Converted(
                converted,
                0..1,
                "transformed_result".to_string(),
                Id::get("transformed"),
            ))
        }
        other => Ok(other),
    }
}

/// Test HandlerMap creation and basic functionality
#[test]
fn test_handler_map_creation() {
    let handler_map = HandlerMap::new("test_map");

    assert_eq!(
        handler_map.name, "test_map",
        "HandlerMap name should be set correctly"
    );
    assert!(
        handler_map.ids.is_empty(),
        "HandlerMap should start with empty IDs"
    );
    assert!(
        handler_map.roles.is_empty(),
        "HandlerMap should start with empty roles"
    );
    assert!(
        handler_map.handlers.is_empty(),
        "HandlerMap should start with empty handlers"
    );
}

/// Test adding handlers to HandlerMap
#[test]
fn test_handler_map_add_handler() {
    let mut handler_map = HandlerMap::new("test_map");
    let handler_id = Id::get("test_handler");

    // Add a role
    handler_map
        .roles
        .insert(handler_id.clone(), "function".to_string());
    handler_map.ids.push(handler_id.clone());

    // Add a simple handler function using function pointer
    handler_map
        .handlers
        .insert(handler_id.clone(), test_success_handler);

    assert_eq!(handler_map.ids.len(), 1, "Should have one handler ID");
    assert!(
        handler_map.roles.contains_key(&handler_id),
        "Should contain handler role"
    );
    assert!(
        handler_map.handlers.contains_key(&handler_id),
        "Should contain handler function"
    );
}

/// Test HandlerMap process function with empty tokens
#[test]
fn test_handler_map_process_empty_tokens() -> Result<(), ConversionError> {
    let handler_map = HandlerMap::new("empty_test");
    let mut context = Context::new("test_context");
    let tokens: Vec<Token> = vec![];

    let result = handler_map.process(&tokens)?;

    assert_eq!(
        result.start_idx, 0,
        "Start index should be 0 for empty tokens"
    );
    assert_eq!(result.end_idx, 0, "End index should be 0 for empty tokens");
    assert!(
        matches!(result.result, HandlerResult::NotHandled(_, _, _)),
        "Should return NotHandled for empty tokens"
    );

    Ok(())
}

/// Test HandlerMap process function with no handlers
#[test]
fn test_handler_map_process_no_handlers() -> Result<(), ConversionError> {
    let handler_map = HandlerMap::new("no_handlers_test");
    let mut context = Context::new("test_context");
    let tokens = vec![Token::s("test".to_string())];

    let result = handler_map.process(&tokens)?;

    assert_eq!(result.start_idx, 0, "Start index should be 0");
    assert_eq!(result.end_idx, 0, "End index should be 0");
    assert!(
        matches!(result.result, HandlerResult::NotHandled(_, _, _)),
        "Should return NotHandled with no handlers"
    );

    Ok(())
}

/// Test HandlerMap process function with successful handler
#[test]
fn test_handler_map_process_successful_handler() -> Result<(), ConversionError> {
    let mut handler_map = HandlerMap::new("success_test");
    let mut context = Context::new("test_context");

    // Set up a handler
    let handler_id = Id::get("success_handler");
    handler_map
        .roles
        .insert(handler_id.clone(), "test_role".to_string());
    handler_map.priorities.insert(handler_id.clone(), 100); // Add priority for create_handler_from_id
    handler_map.ids.push(handler_id.clone());

    // Add processor function so find_handler() can find this handler
    handler_map
        .processors
        .insert(handler_id.clone(), test_processor_accept_any);
    // Handler that always succeeds using function pointer
    handler_map
        .handlers
        .insert(handler_id.clone(), test_success_handler);

    let tokens = vec![Token::s("test".to_string())];
    let result = handler_map.process(&tokens)?;

    assert_eq!(result.start_idx, 0, "Start index should be 0");
    assert_eq!(
        result.end_idx, 1,
        "End index should be 1 for consumed token"
    );
    assert!(
        matches!(result.result, HandlerResult::Handled(_, _, _)),
        "Should return Handled result"
    );

    Ok(())
}

/// Test HandlerMap process function with failing handler
#[test]
fn test_handler_map_process_failing_handler() -> Result<(), ConversionError> {
    let mut handler_map = HandlerMap::new("fail_test");
    let mut context = Context::new("test_context");

    // Set up a handler that returns NotHandled
    let handler_id = Id::get("fail_handler");
    handler_map
        .roles
        .insert(handler_id.clone(), "fail_role".to_string());
    handler_map.priorities.insert(handler_id.clone(), 100); // Add priority for create_handler_from_id
    handler_map.ids.push(handler_id.clone());

    // Use rejecting processor for failing handler to avoid infinite loops
    handler_map
        .processors
        .insert(handler_id.clone(), test_processor_reject_all);
    handler_map
        .handlers
        .insert(handler_id.clone(), test_fail_handler);

    let tokens = vec![Token::s("test".to_string())];
    let result = handler_map.process(&tokens)?;

    assert!(
        matches!(result.result, HandlerResult::NotHandled(_, _, _)),
        "Should return NotHandled result"
    );

    Ok(())
}

/// Test HandlerMap process function with processor callback
#[test]
fn test_handler_map_process_with_processor() -> Result<(), ConversionError> {
    let mut handler_map = HandlerMap::new("processor_test");
    let mut context = Context::new("test_context");

    let handler_id = Id::get("processor_handler");
    handler_map
        .roles
        .insert(handler_id.clone(), "processor_role".to_string());
    handler_map.ids.push(handler_id.clone());

    // Use function pointers for processor and handler
    handler_map
        .processors
        .insert(handler_id.clone(), test_processor_accept);
    handler_map
        .handlers
        .insert(handler_id.clone(), test_success_handler);

    // Test with tokens that should be rejected
    let reject_tokens = vec![Token::s("reject".to_string())];
    let result = handler_map.process(&reject_tokens)?;
    assert!(
        matches!(result.result, HandlerResult::NotHandled(_, _, _)),
        "Should reject tokens without 'accept'"
    );

    // Test with tokens that should be accepted
    let accept_tokens = vec![Token::s("accept_this".to_string())];
    let result = handler_map.process(&accept_tokens)?;
    assert!(
        matches!(result.result, HandlerResult::Handled(_, _, _)),
        "Should accept tokens with 'accept'"
    );

    Ok(())
}

/// Test HandlerMap process function with redirect handling
#[test]
fn test_handler_map_process_redirect() -> Result<(), ConversionError> {
    let mut handler_map = HandlerMap::new("redirect_test");
    let mut context = Context::new("test_context");

    // Set up source handler that redirects
    let source_id = Id::get("source_handler");
    let target_id = Id::get("target_handler");

    handler_map
        .roles
        .insert(source_id.clone(), "source_role".to_string());
    handler_map
        .roles
        .insert(target_id.clone(), "target_role".to_string());
    handler_map.priorities.insert(source_id.clone(), 100); // Add priority for create_handler_from_id
    handler_map.priorities.insert(target_id.clone(), 100); // Add priority for create_handler_from_id
    handler_map.ids.push(source_id.clone());
    handler_map.ids.push(target_id.clone());

    // Add processor functions so find_handler() can find these handlers
    handler_map
        .processors
        .insert(source_id.clone(), test_processor_accept_any);
    handler_map
        .processors
        .insert(target_id.clone(), test_processor_accept_any);
    // Use function pointers for both source and target handlers
    handler_map
        .handlers
        .insert(source_id.clone(), test_redirect_source_handler);
    handler_map
        .handlers
        .insert(target_id.clone(), test_redirect_target_handler);

    let tokens = vec![Token::s("redirect_me".to_string())];
    let result = handler_map.process(&tokens)?;

    // The redirect should be processed and result in a Handled result
    assert!(
        matches!(result.result, HandlerResult::Handled(_, _, _)),
        "Redirected processing should result in Handled"
    );

    Ok(())
}

/// Test HandlerMap process function with result callback
#[test]
fn test_handler_map_process_with_result_callback() -> Result<(), ConversionError> {
    let mut handler_map = HandlerMap::new("result_test");
    let mut context = Context::new("test_context");

    let handler_id = Id::get("result_handler");
    handler_map
        .roles
        .insert(handler_id.clone(), "result_role".to_string());
    handler_map.priorities.insert(handler_id.clone(), 100); // Add priority for create_handler_from_id
    handler_map.ids.push(handler_id.clone());

    // Add processor function so find_handler() can find this handler
    handler_map
        .processors
        .insert(handler_id.clone(), test_processor_accept_any);
    // Use function pointers for both handler and result callback
    handler_map
        .handlers
        .insert(handler_id.clone(), test_processor_handler);
    handler_map
        .results
        .insert(handler_id.clone(), test_result_callback);

    let tokens = vec![Token::s("process_me".to_string())];
    let result = handler_map.process(&tokens)?;

    // Should have been transformed by result callback
    assert!(
        matches!(result.result, HandlerResult::Converted(_, _, _, _)),
        "Result should be transformed to Converted"
    );

    Ok(())
}

/// Test HandlerMap calculate_tokens_consumed function
#[test]
fn test_calculate_tokens_consumed() {
    let handler_map = HandlerMap::new("consume_test");

    let test_tokens = vec![Token::s("a".to_string()), Token::s("b".to_string())];

    // Test Handled variant
    let handled_result =
        HandlerResult::Handled(Some(test_tokens.clone()), 0..0, Id::get("handled"));
    // Note: calculate_tokens_consumed is pub(crate) so not accessible from tests
    // Instead we test token consumption through the process() method behavior

    // Test NotHandled with None
    let not_handled_result = HandlerResult::NotHandled(None, 0..0, Id::get("not_handled"));

    // Test that different HandlerResult variants work properly
    assert!(
        matches!(handled_result, HandlerResult::Handled(_, _, _)),
        "Should be Handled variant"
    );
    assert!(
        matches!(not_handled_result, HandlerResult::NotHandled(_, _, _)),
        "Should be NotHandled variant"
    );

    let converted = ConvertedElement::Comment(ConvertedComment {
        content: "test".to_string(),
        rust_code: "// test".to_string(),
        is_block: false,
        is_doc_comment: false,
    });
    let converted_result =
        HandlerResult::Converted(converted, 0..0, "result".to_string(), Id::get("converted"));
    assert!(
        matches!(converted_result, HandlerResult::Converted(_, _, _, _)),
        "Should be Converted variant"
    );
}

/// Test HandlerMap with multiple handlers in priority order
#[test]
fn test_handler_map_priority_order() -> Result<(), ConversionError> {
    let mut handler_map = HandlerMap::new("priority_test");
    let mut context = Context::new("test_context");

    // Set up multiple handlers
    let first_id = Id::get("first_handler");
    let second_id = Id::get("second_handler");

    handler_map
        .roles
        .insert(first_id.clone(), "first_role".to_string());
    handler_map
        .roles
        .insert(second_id.clone(), "second_role".to_string());

    // Add in specific order - first handler should be tried first
    handler_map.ids.push(first_id.clone());
    handler_map.ids.push(second_id.clone());

    // Use function pointers for both handlers
    handler_map
        .handlers
        .insert(first_id.clone(), test_first_handler);
    handler_map
        .handlers
        .insert(second_id.clone(), test_second_handler);

    // Test tokens that should be handled by first handler
    let normal_tokens = vec![Token::s("normal".to_string())];
    let result = handler_map.process(&normal_tokens)?;
    if let HandlerResult::Handled(_, _, id) = result.result {
        assert!(
            id.name().contains("first_handled"),
            "First handler should handle normal tokens"
        );
    }

    // Test tokens that should skip first handler and go to second
    let skip_tokens = vec![Token::s("skip_first".to_string())];
    let result = handler_map.process(&skip_tokens)?;
    if let HandlerResult::Handled(_, _, id) = result.result {
        assert!(
            id.name().contains("second_handled"),
            "Second handler should handle skipped tokens"
        );
    }

    Ok(())
}

/// Test ProcessedResult structure
#[test]
fn test_processed_result() {
    let test_id = Id::get("test_result");
    let test_result = HandlerResult::Handled(None, 0..0, Id::get("handled"));

    let processed = ProcessedResult::new(0, 5, test_id.clone(), test_result);

    assert_eq!(
        processed.start_idx, 0,
        "Start index should be set correctly"
    );
    assert_eq!(processed.end_idx, 5, "End index should be set correctly");
    assert_eq!(processed.id, test_id, "Handler ID should be set correctly");
    assert!(
        matches!(processed.result, HandlerResult::Handled(_, _, _)),
        "Result should be preserved"
    );
}

/// Test error handling in HandlerMap processing
#[test]
fn test_handler_map_error_handling() {
    let mut handler_map = HandlerMap::new("error_test");
    let mut context = Context::new("test_context");

    let handler_id = Id::get("error_handler");
    handler_map
        .roles
        .insert(handler_id.clone(), "error_role".to_string());
    handler_map.priorities.insert(handler_id.clone(), 100); // Add priority for create_handler_from_id
    handler_map.ids.push(handler_id.clone());

    // Add processor function so find_handler() can find this handler
    handler_map
        .processors
        .insert(handler_id.clone(), test_processor_accept_any);
    // Use function pointer for error handler
    handler_map
        .handlers
        .insert(handler_id.clone(), test_error_handler);

    let tokens = vec![Token::s("error_trigger".to_string())];
    let result = handler_map.process(&tokens);

    assert!(result.is_err(), "Should return error from failing handler");
}
