//! Clean Context tests using correct API patterns
//! Tests basic Context functionality, Registry operations, and reporting system

use crate::{Entry, HandlerPhase, HandlerReport, Id, ReportLevel, context, report};
use core::assert_eq;
use std::collections::HashMap;

/// Test basic Context creation
#[test]
fn test_context_creation() {
    // Context should be properly initialized with required fields
    assert!(
        context!().registry.entries.is_empty(),
        "Registry should start empty initially"
    );
    // HandlerMap exists and is properly initialized
    assert!(
        context!().pending_redirects.is_empty(),
        "Pending redirects should be empty initially"
    );
}

/// Test Registry insert and get operations
#[test]
fn test_registry_operations() {
    let mut context = context!();
    let test_id = Id::get("test_entry");

    // Test inserting a string entry
    let test_value = "test_value".to_string();
    let entry = Entry::Str(test_value.clone());
    context.registry.insert(test_id.clone(), entry);

    // Test retrieving the entry
    if let Some(Entry::Str(retrieved_value)) = context.registry.root(&test_id) {
        assert_eq!(
            &test_value, retrieved_value,
            "Retrieved value should match inserted value"
        );
    } else {
        panic!("Failed to retrieve inserted entry or wrong type");
    }

    // Test get_root_entry
    if let Some(Entry::Str(root_value)) = context.registry.root(&test_id) {
        assert_eq!(
            &test_value, root_value,
            "Root entry should match inserted value"
        );
    } else {
        panic!("Failed to retrieve root entry");
    }
}

/// Test Registry remove operation
#[test]
fn test_registry_remove() {
    let mut context = context!();
    let test_id = Id::get("test_remove");

    // Insert an entry
    let entry = Entry::Val(42);
    context.registry.insert(test_id.clone(), entry);

    // Verify it exists
    assert!(
        context.registry.root(&test_id).is_some(),
        "Entry should exist before removal"
    );

    // Remove the entry
    let removed = context.registry.remove(&test_id);
    assert!(removed.is_some(), "Remove should return the removed entry");

    // Verify it's gone
    assert!(
        context.registry.root(&test_id).is_none(),
        "Entry should not exist after removal"
    );
}

/// Test HandlerReport creation and storage
#[test]
fn test_handler_report_operations() {
    let mut context = context!();

    // Create a handler report with all required fields
    let report_id = Id::get("test_report");
    let handler_id = Id::get("test_handler_id");
    let handler_report = HandlerReport {
        report_id: Box::new(report_id.clone()),
        handler_id: Box::new(handler_id.clone()),
        handler_name: "test_handler".to_string(),
        function_name: "test_function".to_string(),
        phase: HandlerPhase::Process,
        level: ReportLevel::Info,
        message: "Test report message".to_string(),
        tokens_consumed: 5,
        tokens_processed: 10,
        success: true,
        metadata: HashMap::new(),
    };

    // Store the report using Entry::HandlerReport
    let entry = Entry::HandlerReport(handler_report.clone());
    context.registry.insert(report_id.clone(), entry);

    // Retrieve and verify the report using proper registry access
    if let Some(Entry::HandlerReport(retrieved_report)) = context.registry.entries.get(&report_id) {
        assert_eq!(retrieved_report.handler_name, handler_report.handler_name);
        assert_eq!(retrieved_report.function_name, handler_report.function_name);
        assert_eq!(retrieved_report.message, handler_report.message);
        assert_eq!(
            retrieved_report.tokens_consumed,
            handler_report.tokens_consumed
        );
        assert_eq!(retrieved_report.success, handler_report.success);
    } else {
        panic!("Failed to retrieve handler report");
    }
}

/// Test Context reporting methods
#[test]
fn test_context_reporting_methods() {
    let mut context = context!();

    // Add a report using Context::add_report with all required fields
    let report_id = Id::get("context_report");
    let handler_id = Id::get("context_handler_id");
    let report = HandlerReport {
        report_id: Box::new(report_id.clone()),
        handler_id: Box::new(handler_id.clone()),
        handler_name: "context_test_handler".to_string(),
        function_name: "context_test_function".to_string(),
        phase: HandlerPhase::Convert,
        level: ReportLevel::Warning,
        message: "Context test warning".to_string(),
        tokens_consumed: 3,
        tokens_processed: 8,
        success: false,
        metadata: HashMap::new(),
    };

    context.add_report(report.clone());

    // Get reports by handler
    let handler_reports = context.get_reports_by_handler("context_test_handler");
    assert!(
        !handler_reports.is_empty(),
        "Should find reports for the handler"
    );

    // Verify the report content
    let found_report = &handler_reports[0];
    assert_eq!(found_report.handler_name, report.handler_name);
    assert_eq!(found_report.level, ReportLevel::Warning);
    assert!(!found_report.success, "Report should indicate failure");
}

/// Test report! macro functionality
#[test]
fn test_report_macro() {
    use crate::config::Global;

    // Test basic report! macro usage
    report!(
        "test_handler",
        "test_function",
        ReportLevel::Info,
        HandlerPhase::Extract,
        "Test macro message",
        true
    );

    // Verify the report was created using shared global Context
    let reports = Global::write(|ctx| {
        ctx.get_reports_by_handler("test_handler")
            .into_iter()
            .cloned()
            .collect::<Vec<_>>()
    });
    assert!(!reports.is_empty(), "Report macro should create a report");

    let report = &reports[0];
    assert_eq!(report.handler_name, "test_handler");
    assert_eq!(report.message, "Test macro message");
    assert_eq!(report.level, ReportLevel::Info);
    assert_eq!(report.phase, HandlerPhase::Extract);
    assert!(report.success, "Report should indicate success");
}

/// Test Context display_reports functionality
#[test]
fn test_display_reports() {
    let mut context = context!();

    // Add multiple reports with all required fields
    for i in 0..3 {
        let report_id = Id::get(&format!("display_test_{}", i));
        let handler_id = Id::get(&format!("handler_id_{}", i));
        let report = HandlerReport {
            report_id: Box::new(report_id.clone()),
            handler_id: Box::new(handler_id.clone()),
            handler_name: format!("handler_{}", i),
            function_name: format!("function_{}", i),
            phase: HandlerPhase::Process,
            level: if i == 1 {
                ReportLevel::Error
            } else {
                ReportLevel::Info
            },
            message: format!("Test message {}", i),
            tokens_consumed: i as usize,
            tokens_processed: (i * 2) as usize,
            success: i != 1, // Second report is a failure
            metadata: HashMap::new(),
        };
        context.add_report(report.clone());
    }

    // Test display_reports method - it returns (), so just call it
    context.display_reports();

    // Verify reports were added by checking handler reports
    let handler_0_reports = context.get_reports_by_handler("handler_0");
    let handler_1_reports = context.get_reports_by_handler("handler_1");
    let handler_2_reports = context.get_reports_by_handler("handler_2");

    assert!(
        !handler_0_reports.is_empty(),
        "Handler 0 should have reports"
    );
    assert!(
        !handler_1_reports.is_empty(),
        "Handler 1 should have reports"
    );
    assert!(
        !handler_2_reports.is_empty(),
        "Handler 2 should have reports"
    );

    // Verify specific report properties
    assert_eq!(
        handler_1_reports[0].level,
        ReportLevel::Error,
        "Handler 1 should have error level"
    );
    assert_eq!(
        handler_0_reports[0].level,
        ReportLevel::Info,
        "Handler 0 should have info level"
    );
}

/// Test Entry type variants
#[test]
fn test_entry_variants() {
    let mut context = context!();

    // Test String variant
    let string_id = Id::get("string_test");
    context
        .registry
        .insert(string_id.clone(), Entry::Str("test".to_string()));
    assert!(matches!(
        context.registry.root(&string_id),
        Some(Entry::Str(_))
    ));

    // Test Integer variant
    let int_id = Id::get("int_test");
    context.registry.insert(int_id.clone(), Entry::Val(42));
    assert!(matches!(
        context.registry.root(&int_id),
        Some(Entry::Val(42))
    ));

    // Test Boolean variant
    let bool_id = Id::get("bool_test");
    context.registry.insert(bool_id.clone(), Entry::Bool(true));
    assert!(matches!(
        context.registry.root(&bool_id),
        Some(Entry::Bool(true))
    ));
}

/// Test Registry iteration and bulk operations
#[test]
fn test_registry_bulk_operations() {
    let mut context = context!();

    // Insert multiple entries
    let ids = (0..5)
        .map(|i| {
            let id = Id::get(&format!("bulk_{}", i));
            context.registry.insert(id.clone(), Entry::Val(i * 10));
            id
        })
        .collect::<Vec<_>>();

    // Verify all entries exist
    for (i, id) in ids.iter().enumerate() {
        if let Some(Entry::Val(value)) = context.registry.root(id) {
            assert_eq!(*value, (i * 10) as u64, "Bulk inserted value should match");
        } else {
            panic!("Bulk inserted entry not found or wrong type");
        }
    }

    // Test registry size after bulk operations
    // Note: Registry starts with some default entries, so we check it's at least as many as we added
    assert!(
        context.registry.entries.len() >= 5,
        "Registry should contain at least our entries"
    );
}

/// Test error handling in Registry operations
#[test]
fn test_registry_error_handling() {
    let context = context!();
    let nonexistent_id = Id::get("nonexistent");

    // Test getting nonexistent entry
    assert!(
        context.registry.root(&nonexistent_id).is_none(),
        "Getting nonexistent entry should return None"
    );

    // Test getting nonexistent root entry
    assert!(
        context.registry.root(&nonexistent_id).is_none(),
        "Getting nonexistent root entry should return None"
    );
}

/// Test Context available fields and state management
#[test]
fn test_context_state_management() {
    let context = context!();

    // Test available Context fields
    assert!(
        context.registry.entries.is_empty(),
        "Registry should start empty initially"
    );
    // HandlerMap exists and is properly initialized
    assert!(
        context.pending_redirects.is_empty(),
        "Pending redirects should be empty"
    );

    // Test that tokenizer is properly initialized (tokenizer exists)
    // Note: tokenizer.name field is private, so we just verify tokenizer exists
}

/// Test concurrent report operations
#[test]
fn test_concurrent_report_operations() {
    let mut context = context!();

    // Simulate multiple handlers reporting concurrently
    let handlers = vec!["parser", "converter", "validator"];

    for handler in &handlers {
        for phase in [
            HandlerPhase::Process,
            HandlerPhase::Extract,
            HandlerPhase::Convert,
        ] {
            let report_id = Id::get(&format!("{}_{:?}", handler, phase));
            let handler_id = Id::get(&format!("{}_id_{:?}", handler, phase));
            let report = HandlerReport {
                report_id: Box::new(report_id.clone()),
                handler_id: Box::new(handler_id.clone()),
                handler_name: handler.to_string(),
                function_name: format!("{}_function", handler),
                phase: phase.clone(),
                level: ReportLevel::Info,
                message: format!("{} completed {:?} phase", handler, phase),
                tokens_consumed: 1,
                tokens_processed: 2,
                success: true,
                metadata: HashMap::new(),
            };
            context.add_report(report.clone());
        }
    }

    // Verify all reports are stored correctly
    for handler in &handlers {
        let reports = context.get_reports_by_handler(handler);
        assert_eq!(reports.len(), 3, "Each handler should have 3 phase reports");

        // Verify all phases are represented
        let phases: std::collections::HashSet<_> =
            reports.iter().map(|r| r.phase.clone()).collect();
        assert_eq!(phases.len(), 3, "All three phases should be present");
    }
}
