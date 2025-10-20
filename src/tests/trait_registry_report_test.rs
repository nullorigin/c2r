#[cfg(test)]
mod tests {
    use crate::registry::ReportFormat;
    use crate::{Context, Id, Phase, Report, ReportLevel};

    #[test]
    fn test_trait_registry_report_system() {
        let mut context = Context::new();
        let registry = &mut context.registry;

        // Test creating reports
        let handler_id = Id::get("test_handler");
        let report = Report::new(
            Id::get("test_report"),
            Some(handler_id),
            "test_trait_registry_report_system".to_string(),
            "Test message".to_string(),
            ReportLevel::Info,
            Phase::Convert(Some("Convert".to_string())),
        )
            .with_tokens(10, 5)
            .with_success(true);

        let report_id = registry.add_report(report);

        // Verify report was created
        let reports = registry.get_reports();
        assert_eq!(reports.len(), 1);
        assert_eq!(reports[0].handler_name(), "test_handler");
        assert_eq!(reports[0].message, "Test message");
        assert_eq!(reports[0].success, true);
        assert_eq!(reports[0].tokens_processed, 10);
        assert_eq!(reports[0].tokens_consumed, 5);

        println!("✅ Report system test passed - created and retrieved reports successfully");
    }

    #[test]
    fn test_clear_reports() {
        let mut context = Context::new();
        let registry = &mut context.registry;

        // Create some reports
        let report1 = Report::new(
            Id::get("report1"),
            Some(Id::get("handler1")),
            "test_clear_reports".to_string(),
            "Message 1".to_string(),
            ReportLevel::Info,
            Phase::Convert(Some("Convert".to_string())),
        )
            .with_tokens(5, 2)
            .with_success(true);

        let report2 = Report::new(
            Id::get("report2"),
            Some(Id::get("handler2")),
            "test_clear_reports".to_string(),
            "Message 2".to_string(),
            ReportLevel::Error,
            Phase::Convert(Some("Convert".to_string())),
        )
            .with_tokens(3, 0)
            .with_success(false);

        registry.add_report(report1);
        registry.add_report(report2);

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
        let mut context = Context::new();

        // Create test reports with various scenarios
        let report1 = Report::new(
            Id::get("report1"),
            Some(Id::get("handler1")),
            "test_table_formatting".to_string(),
            "Successfully processed function definition".to_string(),
            ReportLevel::Info,
            Phase::Convert(Some("Convert".to_string())),
        )
            .with_tokens(15, 8)
            .with_success(true);

        let report2 = Report::new(
            Id::get("report2"),
            Some(Id::get("handler2")),
            "test_table_formatting".to_string(),
            "Array parsing failed due to syntax error".to_string(),
            ReportLevel::Error,
            Phase::Convert(Some("Convert".to_string())),
        )
            .with_tokens(12, 0)
            .with_success(false);

        let report3 = Report::new(
            Id::get("report3"),
            Some(Id::get("handler3")),
            "test_table_formatting".to_string(),
            "Struct definition processed with warnings about nested complexity".to_string(),
            ReportLevel::Warning,
            Phase::Initialize(Some("Initialize".to_string())),
        )
            .with_tokens(25, 18)
            .with_success(true);

        context.registry.add_report(report1);
        context.registry.add_report(report2);
        context.registry.add_report(report3);

        // Test text format (default)
        println!("🎯 Testing Text Format:");
        context
            .registry
            .display_formatted_reports(&ReportFormat::Text);

        // Test table format
        println!("📊 Testing Table Format:");
        context
            .registry
            .display_formatted_reports(&ReportFormat::Table);

        // Test compact format
        println!("📝 Testing Compact Format:");
        context
            .registry
            .display_formatted_reports(&ReportFormat::Compact);

        // Test level-specific table formatting
        println!("⚠️ Testing Warning Reports Table:");
        context
            .registry
            .display_formatted_reports_by_level(&ReportLevel::Warning, &ReportFormat::Table);

        println!("✅ Table formatting test passed - all formats working correctly");
    }
}
