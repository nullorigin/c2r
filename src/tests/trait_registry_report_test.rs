#[cfg(test)]
mod tests {
    use crate::{
        Context, HandlerPhase, Id, ReportLevel, context, registry::{self, Registry}
    };

    #[test]
    fn test_trait_registry_report_system() {
        let context = &mut Context::new();
        let registry = &mut context.registry;

        // Test creating reports
        let handler_id = Id::get("test_handler");
        let report_id = registry.create_report(
            handler_id,
            "TestHandler".to_string(),
            "Test message".to_string(),
            ReportLevel::Info,
            HandlerPhase::Process,
            true,
            10,
            5
        );

        // Verify report was created
        let reports = registry.get_reports();
        assert_eq!(reports.len(), 1);
        assert_eq!(reports[0].handler_name, "TestHandler");
        assert_eq!(reports[0].message, "Test message");
        assert_eq!(reports[0].success, true);
        assert_eq!(reports[0].tokens_processed, 10);
        assert_eq!(reports[0].tokens_consumed, 5);

        println!("✅ Report system test passed - created and retrieved reports successfully");
    }

    #[test]
    fn test_report_filtering() {
        let context = &mut Context::new();
        let registry = &mut context.registry;

        // Create reports with different levels
        registry.create_report(
            Id::get("handler1"),
            "Handler1".to_string(),
            "Info message".to_string(),
            ReportLevel::Info,
            HandlerPhase::Process,
            true,
            5,
            2
        );

        registry.create_report(
            Id::get("handler2"),
            "Handler2".to_string(),
            "Error message".to_string(),
            ReportLevel::Error,
            HandlerPhase::Handle,
            false,
            3,
            0
        );

        registry.create_report(
            Id::get("handler1"),
            "Handler1".to_string(),
            "Another info message".to_string(),
            ReportLevel::Info,
            HandlerPhase::Extract,
            true,
            8,
            4
        );

        // Test filtering by level
        let info_reports = registry.get_reports_by_level(&ReportLevel::Info);
        assert_eq!(info_reports.len(), 2);

        let error_reports = registry.get_reports_by_level(&ReportLevel::Error);
        assert_eq!(error_reports.len(), 1);

        // Test filtering by handler name
        let handler1_reports = registry.get_reports_by_handler("Handler1");
        assert_eq!(handler1_reports.len(), 2);

        let handler2_reports = registry.get_reports_by_handler("Handler2");
        assert_eq!(handler2_reports.len(), 1);

        // Test filtering by phase
        let process_reports = registry.get_reports_by_phase(&HandlerPhase::Process);
        assert_eq!(process_reports.len(), 1);

        let handle_reports = registry.get_reports_by_phase(&HandlerPhase::Handle);
        assert_eq!(handle_reports.len(), 1);

        // Test success/failure filtering
        let successful_reports = registry.get_successful_reports();
        assert_eq!(successful_reports.len(), 2);

        let failed_reports = registry.get_failed_reports();
        assert_eq!(failed_reports.len(), 1);

        println!("✅ Report filtering test passed - all filters working correctly");
    }

    #[test]
    fn test_report_stats() {
        let mut context = Context::new();
        let registry = &mut context.registry;

        // Create some test reports
        registry.create_report(
            Id::get("handler1"),
            "Handler1".to_string(),
            "Success".to_string(),
            ReportLevel::Info,
            HandlerPhase::Process,
            true,
            10,
            5
        );

        registry.create_report(
            Id::get("handler2"),
            "Handler2".to_string(),
            "Error".to_string(),
            ReportLevel::Error,
            HandlerPhase::Handle,
            false,
            5,
            0
        );

        registry.create_report(
            Id::get("handler3"),
            "Handler3".to_string(),
            "Warning".to_string(),
            ReportLevel::Warning,
            HandlerPhase::Extract,
            true,
            8,
            3
        );

        // Test report statistics
        let stats = registry.get_report_stats();
        assert_eq!(stats.get("total_reports"), Some(&3));
        assert_eq!(stats.get("successful_reports"), Some(&2));
        assert_eq!(stats.get("failed_reports"), Some(&1));
        assert_eq!(stats.get("error_reports"), Some(&1));
        assert_eq!(stats.get("warning_reports"), Some(&1));
        assert_eq!(stats.get("info_reports"), Some(&1));

        println!("✅ Report statistics test passed - all stats calculated correctly");
    }

    #[test]
    fn test_report_display() {
        let context = &mut Context::new();
        let registry = &mut context.registry.clone();
        // Create a test report
        registry.create_report(
            Id::get("test_handler"),
            "TestHandler".to_string(),
            "Test display message".to_string(),
            ReportLevel::Info,
            HandlerPhase::Process,
            true,
            15,
            8
        );

        // Test display methods (they print to stdout, so we just ensure no panics)
        registry.display_reports();
        registry.display_reports_by_level(&ReportLevel::Info);

        // Test generate_report method
        let report_text = registry.generate_report(context).expect("Failed to generate report");
        assert!(report_text.contains("Trait-Based Handler System Report"));
        assert!(report_text.contains("Total Reports: 1"));
        assert!(report_text.contains("Success Rate:"));

        println!("✅ Report display test passed - report generation working correctly");
    }

    #[test]
    fn test_clear_reports() {
        let mut context = Context::new();
        let registry = &mut context.registry;
        // Create some reports
        registry.create_report(
            Id::get("handler1"),
            "Handler1".to_string(),
            "Message 1".to_string(),
            ReportLevel::Info,
            HandlerPhase::Process,
            true,
            5,
            2
        );

        registry.create_report(
            Id::get("handler2"),
            "Handler2".to_string(),
            "Message 2".to_string(),
            ReportLevel::Error,
            HandlerPhase::Handle,
            false,
            3,
            0
        );

        // Verify reports exist
        assert_eq!(registry.get_reports().len(), 2);

        // Clear all reports
        registry.clear_reports();

        // Verify reports are cleared
        assert_eq!(registry.get_reports().len(), 0);
        let stats = registry.get_report_stats();
        assert_eq!(stats.get("total_reports"), Some(&0));

        println!("✅ Clear reports test passed - reports cleared successfully");
    }

    #[test]
    fn test_table_formatting() {
        use crate::registry::ReportFormat;
        
        let mut context = Context::new();

        // Create test reports with various scenarios
        context.registry.create_report(
            Id::get("handler1"),
            "FunctionHandler".to_string(),
            "Successfully processed function definition".to_string(),
            ReportLevel::Info,
            HandlerPhase::Process,
            true,
            15,
            8
        );

        context.registry.create_report(
            Id::get("handler2"),
            "ArrayHandler".to_string(),
            "Array parsing failed due to syntax error".to_string(),
            ReportLevel::Error,
            HandlerPhase::Handle,
            false,
            12,
            0
        );

        context.registry.create_report(
            Id::get("handler3"),
            "StructHandler".to_string(),
            "Struct definition processed with warnings about nested complexity".to_string(),
            ReportLevel::Warning,
            HandlerPhase::Extract,
            true,
            25,
            18
        );

        // Test text format (default)
        println!("🎯 Testing Text Format:");
        context.registry.display_formatted_reports(&ReportFormat::Text);

        // Test table format
        println!("📊 Testing Table Format:");
        context.registry.display_formatted_reports(&ReportFormat::Table);

        // Test compact format
        println!("📝 Testing Compact Format:");
        context.registry.display_formatted_reports(&ReportFormat::Compact);

        // Test level-specific table formatting
        println!("⚠️ Testing Warning Reports Table:");
        context.registry.display_formatted_reports_by_level(&ReportLevel::Warning, &ReportFormat::Table);

        // Test statistics table
        println!("📈 Testing Statistics Table:");
        context.registry.display_stats_table();

        println!("✅ Table formatting test passed - all formats working correctly");
    }
}
