//! Trait-based handler registry implementation
//! Integrates the new Handler trait system with the existing context and registry

use crate::Handler;
use crate::{
    info, pattern::Pattern, Context, Entry, Id, Justification, Phase, Report, ReportLevel,
    Result, Table, TableCell, Token,
};
use core::{
    option::Option::{self, None, Some},
    result::Result::Ok,
};
use std::collections::HashMap;
use std::ops::Range;
use std::{any::Any, path::PathBuf};

/// Display formatting options for report output
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReportFormat {
    /// Simple text format (default)
    Text,
    /// Formatted table display
    Table,
    /// Compact single-line format
    Compact,
}

impl Default for ReportFormat {
    fn default() -> Self {
        ReportFormat::Text
    }
}

/// Enhanced registry that manages trait-based handlers with integrated report system
#[derive(Debug)]
pub struct Registry {
    pub id: Id,
    pub reports: HashMap<Id, Report>, // Direct report storage for fast access
    // Entry system - core storage for all registry data
    pub entries: HashMap<Id, Entry>,
    pub entry_count: u64,
}
impl Clone for Registry {
    fn clone(&self) -> Self {
        Self {
            id: self.id.clone(),
            reports: self
                .reports
                .iter()
                .map(|(id, report)| (id.clone(), report.clone()))
                .collect(),
            entries: self
                .entries
                .iter()
                .map(|(id, entry)| (id.clone(), entry.clone()))
                .collect(),
            entry_count: self.entry_count.clone(),
        }
    }
}
impl PartialEq for Registry {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
            && self.entry_count == other.entry_count
            && self.entries == other.entries
            && self.reports == other.reports
    }
}
impl Eq for Registry {}
impl std::hash::Hash for Registry {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        self.entry_count.hash(state);
        self.entries.iter().for_each(|(id, entry)| {
            id.hash(state);
            entry.hash(state);
        });
        self.reports.iter().for_each(|(id, report)| {
            id.hash(state);
            report.hash(state);
        });
    }
}
impl Registry {
    pub fn new() -> Self {
        Self {
            id: Id::get("registry"),
            reports: HashMap::new(),
            entries: HashMap::new(),
            entry_count: 0,
        }
    }

    /// Get handlers supporting a specific pattern
    /// Note: This method should not be called during processing that already holds context locks
    pub fn get_handlers_for_pattern(&self, pattern: &str) -> Vec<Id> {
        let mut result = Vec::new();
        for (_, entry) in self.entries.iter() {
            if let Entry::Handler(handler) = entry {
                if handler.supported_patterns().contains(&pattern.to_string()) {
                    result.push(handler.id());
                }
            }
        }
        result
    }
    /// Add a handler report to the centralized reporting system
    pub fn add_report(&mut self, report: Report) -> Id {
        let report_id = report.report_id().clone();

        // Store in our Entry system
        self.insert(report_id.clone(), Entry::HandlerReport(report.clone()));

        // Also keep in direct storage for fast access
        self.reports.insert(report_id.clone(), report);
        report_id
    }

    /// Create and add a new handler report
    /// Create a new handler report and add it to the registry
    ///
    /// # Parameters
    ///
    /// * `handler_id`: The ID of the handler that generated the report
    /// * `handler_name`: The name of the handler that generated the report
    /// * `message`: The message associated with the report
    /// * `level`: The level of the report (Info, Warning, Error, Debug)
    /// * `phase`: The phase of the handler that generated the report (Process, Success, Failure)
    /// * `success`: Whether the handler was successful or not
    /// * `tokens_processed`: The number of tokens processed by the handler
    /// * `tokens_consumed`: The number of tokens consumed by the handler
    ///
    /// # Returns
    ///
    /// The ID of the newly created report
    pub fn create_report(
        &mut self,
        handler_id: Option<Id>,
        function_name: String,
        message: String,
        level: ReportLevel,
        phase: Phase,
        success: bool,
        tokens_processed: usize,
        tokens_consumed: usize,
    ) -> Id {
        let report_id = Id::get(&format!("report_{}_{}",
                                         handler_id.as_ref().map(|id| id.name()).unwrap_or("unknown".to_string()),
                                         function_name
        ));

        let report = Report::new(
            report_id.clone(),
            handler_id,
            function_name,
            message,
            level,
            phase,
        )
            .with_tokens(tokens_processed, tokens_consumed)
            .with_success(success);

        self.add_report(report);
        report_id
    }
    /// Get all handler reports from the registry
    pub fn get_reports(&self) -> Vec<Report> {
        self.reports.values().cloned().collect()
    }

    /// Get reports filtered by handler name
    pub fn get_reports_by_handler(&self, handler_name: &str) -> Vec<Report> {
        self.reports
            .values()
            .filter(|report| report.handler_name() == handler_name)
            .cloned()
            .collect()
    }

    /// Get reports filtered by report level
    pub fn get_reports_by_level(&self, level: &ReportLevel) -> Vec<Report> {
        self.reports
            .values()
            .filter(|report| &report.level == level)
            .cloned()
            .collect()
    }

    /// Get reports filtered by handler phase
    pub fn get_reports_by_phase(&self, phase: &Phase) -> Vec<Report> {
        self.reports
            .values()
            .filter(|report| &report.phase == phase)
            .cloned()
            .collect()
    }

    /// Get reports by success status
    pub fn get_successful_reports(&self) -> Vec<Report> {
        self.reports
            .values()
            .filter(|report| report.success)
            .cloned()
            .collect()
    }
    /// Get reports by failure status
    pub fn get_failed_reports(&self) -> Vec<Report> {
        self.reports
            .values()
            .filter(|report| !report.success)
            .cloned()
            .collect()
    }

    /// Clear all reports from the registry
    pub fn clear_reports(&mut self) {
        self.reports.clear();

        // Remove all HandlerReport entries from our Entry system efficiently
        let report_ids: Vec<Id> = self
            .entries
            .iter()
            .filter_map(|(id, entry)| match entry {
                Entry::HandlerReport(_) => Some(id.clone()),
                _ => None,
            })
            .collect();

        for id in report_ids {
            self.remove(&id);
        }
    }

    /// Display all reports in a formatted way
    pub fn display_reports(&self) {
        self.display_formatted_reports(&ReportFormat::Text);
    }

    /// Display all reports with specified formatting
    pub fn display_formatted_reports(&self, format: &ReportFormat) {
        if self.reports.is_empty() {
            println!("No handler reports available.");
            return;
        }

        let reports: Vec<&Report> = self.reports.values().collect();
        let stats = self.get_report_stats();
        let total_reports = stats.get("total_reports").unwrap_or(&0);
        let successful_reports = stats.get("successful_reports").unwrap_or(&0);
        let error_reports = stats.get("error_reports").unwrap_or(&0);
        let warning_reports = stats.get("warning_reports").unwrap_or(&0);

        match format {
            ReportFormat::Text => {
                println!("\n=== TRAIT HANDLER SYSTEM REPORTS ===");
                println!(
                    "📊 Summary: {} total | ✅ {} success | ❌ {} errors | ⚠️ {} warnings",
                    total_reports, successful_reports, error_reports, warning_reports
                );

                // Group reports by handler for better organization
                let mut handler_groups: HashMap<String, Vec<&Report>> = HashMap::new();
                for report in &reports {
                    handler_groups
                        .entry(report.handler_name().clone())
                        .or_insert_with(Vec::new)
                        .push(report);
                }

                for (handler_name, handler_reports) in handler_groups {
                    println!("\n🔧 Handler: {}", handler_name);
                    println!("   Reports: {}", handler_reports.len());
                    for report in handler_reports {
                        println!("   ├─ {}", self.format_report_summary(report));
                    }
                }
                println!("\n=== END REPORTS ({} total) ===\n", total_reports);
            }
            ReportFormat::Table => {
                println!("\n📋 HANDLER REPORTS TABLE");
                println!(
                    "📊 {} total reports | Success rate: {:.1}%",
                    total_reports,
                    if *total_reports > 0 {
                        (*successful_reports as f64 / *total_reports as f64) * 100.0
                    } else {
                        0.0
                    }
                );

                let reports_vec = self.get_reports();
                let table = self.create_reports_table(&reports_vec);
                println!("{}", table.display_formatted());

                // Display statistics table as well
                println!("\n📈 STATISTICS");
                self.display_stats_table();
            }
            ReportFormat::Compact => {
                println!("\n=== COMPACT REPORTS SUMMARY ===");
                println!(
                    "📊 {} total | ✅ {} OK | ❌ {} ERR | ⚠️ {} WARN | 🔵 {} INFO",
                    total_reports,
                    successful_reports,
                    error_reports,
                    warning_reports,
                    stats.get("info_reports").unwrap_or(&0)
                );

                // Sort reports by level priority (Error > Warning > Info > Debug)
                let mut sorted_reports = reports;
                sorted_reports.sort_by(|a, b| {
                    use ReportLevel::*;
                    let priority = |level: &ReportLevel| match level {
                        Error => 0,
                        Warning => 1,
                        Info => 2,
                        Debug => 3,
                    };
                    priority(&a.level)
                        .cmp(&priority(&b.level))
                        .then_with(|| a.handler_name().cmp(&b.handler_name()))
                });

                for report in sorted_reports {
                    let level_icon = match report.level {
                        ReportLevel::Error => "🔴",
                        ReportLevel::Warning => "🟡",
                        ReportLevel::Info => "🔵",
                        ReportLevel::Debug => "⚪",
                    };
                    let status_icon = if report.success { "✅" } else { "❌" };

                    println!(
                        "{} {} [{}] {}: {}",
                        status_icon,
                        level_icon,
                        report.handler_name(),
                        format!("{:?}", report.phase()).to_uppercase(),
                        report.message.chars().take(80).collect::<String>()
                            + if report.message.len() > 80 { "..." } else { "" }
                    );
                }
                println!("=== END COMPACT SUMMARY ===\n");
            }
        }
    }

    /// Helper method to format a single report summary
    fn format_report_summary(&self, report: &Report) -> String {
        let status = if report.success { "✅" } else { "❌" };
        let level_icon = match report.level {
            ReportLevel::Error => "🔴",
            ReportLevel::Warning => "🟡",
            ReportLevel::Info => "🔵",
            ReportLevel::Debug => "⚪",
        };

        format!(
            "{} {} [{:?}] {}",
            status,
            level_icon,
            report.phase(),
            report.message.chars().take(60).collect::<String>()
                + if report.message.len() > 60 { "..." } else { "" }
        )
    }

    /// Display reports filtered by level
    pub fn display_reports_by_level(&self, level: &ReportLevel) {
        self.display_formatted_reports_by_level(level, &ReportFormat::Text);
    }

    /// Display reports filtered by level with specified formatting
    pub fn display_formatted_reports_by_level(&self, level: &ReportLevel, format: &ReportFormat) {
        let reports = self.get_reports_by_level(level);
        if reports.is_empty() {
            println!("No {:?} level reports available.", level);
            return;
        }

        let report_count = reports.len();
        match format {
            ReportFormat::Text => {
                println!("\n=== {:?} LEVEL REPORTS ===", level);
                for report in &reports {
                    self.display_report(report);
                }
                println!("=== END {:?} REPORTS ({} total) ===\n", level, report_count);
            }
            ReportFormat::Table => {
                let table = self.create_reports_table(&reports);
                println!("\n{}", table);
            }
            ReportFormat::Compact => {
                println!(
                    "\n=== {:?} LEVEL SUMMARY ({} reports) ===",
                    level, report_count
                );
                for report in &reports {
                    println!(
                        "[{}] {}: {}",
                        if report.success() { "✓" } else { "✗" },
                        report.handler_name(),
                        report.message()
                    );
                }
                println!("=== END {:?} SUMMARY ===\n", level);
            }
        }
    }

    /// Display a single report in formatted way
    pub fn display_report(&self, report: &Report) {
        let status_icon = if report.success { "✓" } else { "✗" };
        let level_color = match report.level() {
            ReportLevel::Error => "🔴",
            ReportLevel::Warning => "🟡",
            ReportLevel::Info => "🔵",
            ReportLevel::Debug => "⚪",
        };

        // Header line with enhanced formatting
        println!(
            "[{}] {} {:?} | {} | {:?} | {}",
            report.report_id.name(),
            status_icon,
            report.level(),
            report.handler_name(),
            report.phase(),
            level_color
        );

        // Message with proper indentation
        println!("  📝 Message: {}", report.message);

        // Function name if available
        if !report.function_name.is_empty() && report.function_name != "process" {
            println!("  🔧 Function: {}", report.function_name);
        }

        // Token information with better formatting
        if report.tokens_processed > 0 || report.tokens_consumed > 0 {
            let efficiency = if report.tokens_processed > 0 {
                (report.tokens_consumed as f64 / report.tokens_processed as f64) * 100.0
            } else {
                0.0
            };
            println!(
                "  🔢 Tokens: processed={}, consumed={} ({:.1}% efficiency)",
                report.tokens_processed, report.tokens_consumed, efficiency
            );
        }

        // Details section if available
        if let Some(ref details) = report.details {
            println!("  📋 Details: {}", details);
        }

        // Metadata with improved formatting
        if !report.metadata.is_empty() {
            println!("  📊 Metadata:");
            for (key, value) in &report.metadata {
                println!("    • {}={}", key, value);
            }
        }

        // Success status with visual indicator
        println!("  ✅ Success: {} {}\n", report.success, status_icon);
    }

    /// Generate comprehensive report from all handlers
    pub fn generate_report(&self, context: &Context) -> Result<String> {
        let mut report = String::new();
        report.push_str("=== Handler System Report ===\n\n");

        let stats = self.get_report_stats();
        let total_reports = stats.get("total_reports").unwrap_or(&0);
        let successful_reports = stats.get("successful_reports").unwrap_or(&0);
        let error_reports = stats.get("error_reports").unwrap_or(&0);
        let warning_reports = stats.get("warning_reports").unwrap_or(&0);
        let info_reports = stats.get("info_reports").unwrap_or(&0);

        // Report statistics section
        report.push_str(&format!("Total Reports: {}\n", total_reports));
        report.push_str(&format!(
            "Success Rate: {:.1}%\n",
            if *total_reports == 0 {
                0.0
            } else {
                (*successful_reports as f64 / *total_reports as f64) * 100.0
            }
        ));
        report.push_str(&format!("Error Reports: {}\n", error_reports));
        report.push_str(&format!("Warning Reports: {}\n", warning_reports));
        report.push_str(&format!("Info Reports: {}\n\n", info_reports));

        // Handler details section
        let handler_data: Vec<_> = context
            .handlizer
            .handlers
            .iter()
            .map(|(id, handler)| {
                let handler_name = id.name().to_string();
                let handler_reports: Vec<Report> =
                    self.get_reports_by_handler(&handler_name);
                let success_count = self.get_successful_reports().len();
                let success_rate = if handler_reports.is_empty() {
                    0.0
                } else {
                    (success_count as f64 / handler_reports.len() as f64) * 100.0
                };

                (
                    handler_name,
                    handler.role(),
                    handler.supported_patterns().to_vec(),
                    handler_reports.len(),
                    success_rate,
                )
            })
            .collect();

        for (name, role, patterns, report_count, success_rate) in handler_data {
            report.push_str(&format!("Handler: {} ({})\n", name, role));
            report.push_str(&format!("  Reports: {}\n", report_count));
            report.push_str(&format!("  Patterns: {:?}\n", patterns));

            if report_count > 0 {
                report.push_str(&format!("  Success Rate: {:.1}%\n", success_rate));
            }
            report.push('\n');
        }

        Ok(report)
    }

    /// Get report statistics
    pub fn get_report_stats(&self) -> HashMap<String, u64> {
        let mut stats = HashMap::new();
        stats.insert("total_reports".to_string(), self.reports.len() as u64);
        stats.insert(
            "successful_reports".to_string(),
            self.get_successful_reports().len() as u64,
        );
        stats.insert(
            "failed_reports".to_string(),
            self.get_failed_reports().len() as u64,
        );
        stats.insert(
            "error_reports".to_string(),
            self.get_reports_by_level(&ReportLevel::Error).len() as u64,
        );
        stats.insert(
            "warning_reports".to_string(),
            self.get_reports_by_level(&ReportLevel::Warning).len() as u64,
        );
        stats.insert(
            "info_reports".to_string(),
            self.get_reports_by_level(&ReportLevel::Info).len() as u64,
        );
        stats.insert(
            "debug_reports".to_string(),
            self.get_reports_by_level(&ReportLevel::Debug).len() as u64,
        );
        stats
    }

    /// Get all handler reports from the Entry system
    /// Get all handler reports from the Entry system
    ///
    /// This function will filter all entries of type `HandlerReport` and return a vector of cloned `HandlerReport`s.
    pub fn get_handler_reports_from_entries(&self) -> Vec<Report> {
        self.entries
            .values()
            .filter_map(|entry| match entry {
                Entry::HandlerReport(report) => Some(report.clone()),
                _ => None,
            })
            .collect::<Vec<_>>()
    }

    /// Add a report directly to the Entry system
    pub fn add_report_entry(&mut self, name: &str, report: Report) -> Id {
        let id = Id::get(name);
        self.insert(id.clone(), Entry::HandlerReport(report));
        id
    }

    /// Create a formatted table for displaying reports
    fn create_reports_table(&self, reports: &[Report]) -> Table {
        let mut table = Table::new();
        table.set_dimensions(reports.len() + 1, 6); // +1 for header row, 6 columns

        // Set table ID
        let table_id = Id::get("handler_reports_table");

        // Create header row
        let headers = [
            "Status",
            "Handler",
            "Level",
            "Phase",
            "Tokens P/C",
            "Message",
        ];
        for (col, header) in headers.iter().enumerate() {
            let mut cell = TableCell::new_with_justification(0, col, header, Justification::Center);
            // Add header text as entry
            if let Some(entry) = self.create_table_entry(header.to_string()) {
                cell.add_entry(entry);
            }
            table.set_cell(0, col, cell);
        }

        // Add report data rows
        for (row, report) in reports.iter().enumerate() {
            let data_row = row + 1; // +1 to account for header

            // Status column
            let status = if report.success { "✓" } else { "✗" };
            let mut status_cell = TableCell::new_with_justification(
                data_row,
                0,
                &format!("status_{}", row),
                Justification::Center,
            );
            if let Some(entry) = self.create_table_entry(status.to_string()) {
                status_cell.add_entry(entry);
            }
            table.set_cell(data_row, 0, status_cell);

            // Handler column
            let mut handler_cell = TableCell::new_with_justification(
                data_row,
                1,
                &format!("handler_{}", row),
                Justification::Left,
            );
            if let Some(entry) = self.create_table_entry(report.handler_name()) {
                handler_cell.add_entry(entry);
            }
            table.set_cell(data_row, 1, handler_cell);

            // Level column
            let mut level_cell = TableCell::new_with_justification(
                data_row,
                2,
                &format!("level_{}", row),
                Justification::Center,
            );
            if let Some(entry) = self.create_table_entry(format!("{:?}", report.level())) {
                level_cell.add_entry(entry);
            }
            table.set_cell(data_row, 2, level_cell);

            // Phase column
            let mut phase_cell = TableCell::new_with_justification(
                data_row,
                3,
                &format!("phase_{}", row),
                Justification::Center,
            );
            if let Some(entry) = self.create_table_entry(format!("{:?}", report.phase())) {
                phase_cell.add_entry(entry);
            }
            table.set_cell(data_row, 3, phase_cell);

            // Tokens processed/consumed column
            let tokens_info = if report.tokens_processed > 0 || report.tokens_consumed > 0 {
                format!("{}/{}", report.tokens_processed, report.tokens_consumed)
            } else {
                "-".to_string()
            };
            let mut tokens_cell = TableCell::new_with_justification(
                data_row,
                4,
                &format!("tokens_{}", row),
                Justification::Right,
            );
            if let Some(entry) = self.create_table_entry(tokens_info) {
                tokens_cell.add_entry(entry);
            }
            table.set_cell(data_row, 4, tokens_cell);

            // Message column (truncate if too long)
            let message = if report.message.len() > 50 {
                format!("{}...", &report.message[..47])
            } else {
                report.message.clone()
            };
            let mut message_cell = TableCell::new_with_justification(
                data_row,
                5,
                &format!("message_{}", row),
                Justification::Left,
            );
            if let Some(entry) = self.create_table_entry(message) {
                message_cell.add_entry(entry);
            }
            table.set_cell(data_row, 5, message_cell);
        }

        // Dynamically resize based on content
        table.dynamic_resize();
        table
    }

    /// Create a table entry from a string (helper method)
    fn create_table_entry(&self, content: String) -> Option<Entry> {
        Some(Entry::Str(content))
    }

    /// Create a statistics table for report analysis
    pub fn create_stats_table(&self) -> Table {
        let stats: HashMap<String, u64> = self.get_report_stats();
        let mut table = Table::new();
        table.set_dimensions(stats.len() + 1, 2); // +1 for header, 2 columns (stat name, value)

        // Header row
        let mut stat_name_header =
            TableCell::new_with_justification(0, 0, "stat_name_header", Justification::Center);
        if let Some(entry) = self.create_table_entry("Statistic".to_string()) {
            stat_name_header.add_entry(entry);
        }
        let mut stat_value_header =
            TableCell::new_with_justification(0, 1, "stat_value_header", Justification::Center);
        if let Some(entry) = self.create_table_entry("Count".to_string()) {
            stat_value_header.add_entry(entry);
        }
        table.set_cell(0, 0, stat_name_header);
        table.set_cell(0, 1, stat_value_header);

        // Add stats data
        let stat_order = [
            "total_reports",
            "successful_reports",
            "failed_reports",
            "error_reports",
            "warning_reports",
            "info_reports",
            "debug_reports",
        ];
        let stat_labels = [
            "Total Reports",
            "Successful",
            "Failed",
            "Errors",
            "Warnings",
            "Info",
            "Debug",
        ];

        for (row, (stat_key, label)) in stat_order.iter().zip(stat_labels.iter()).enumerate() {
            let data_row = row + 1;
            let value = stats.get(*stat_key).unwrap_or(&0);

            // Stat name cell
            let mut name_cell = TableCell::new_with_justification(
                data_row,
                0,
                &format!("stat_name_{}", row),
                Justification::Left,
            );
            if let Some(entry) = self.create_table_entry(label.to_string()) {
                name_cell.add_entry(entry);
            }
            table.set_cell(data_row, 0, name_cell);

            // Stat value cell
            let mut value_cell = TableCell::new_with_justification(
                data_row,
                1,
                &format!("stat_value_{}", row),
                Justification::Right,
            );
            if let Some(entry) = self.create_table_entry(value.to_string()) {
                value_cell.add_entry(entry);
            }
            table.set_cell(data_row, 1, value_cell);
        }

        table.dynamic_resize();
        table
    }

    /// Display handler statistics in table format
    pub fn display_stats_table(&self) {
        let table = self.create_stats_table();
        let table_str = table.display_formatted();
        println!("\n{}", table_str);
    }

    /// Save pattern cache to JSON file for persistent storage
    pub fn save_pattern_cache_to_file(&self, context: &mut Context, cache_dir: &str) -> Result<()> {
        use std::fs;
        use std::path::Path;

        // Create cache directory if it doesn't exist
        fs::create_dir_all(cache_dir)?;
        // Generate cache file path
        let cache_file = Path::new(cache_dir).join("pattern_cache.json");
        let cache_path = cache_file.to_string_lossy();
        context.patternizer.save_cache_to_json(&cache_path)?;

        info!("Pattern cache saved to: {}", cache_path);
        Ok(())
    }

    /// Load pattern cache from JSON file if it exists
    pub fn load_pattern_cache_from_file(
        &mut self,
        context: &mut Context,
        cache_dir: &str,
    ) -> Result<()> {
        use std::path::Path;

        let cache_file = Path::new(cache_dir).join("pattern_cache.json");
        let cache_path = cache_file.to_string_lossy();

        if cache_file.exists() {
            context.patternizer.load_cache_from_json(&cache_path)?;
            info!("Pattern cache loaded from: {}", cache_path);
        } else {
            info!("No pattern cache found at: {}", cache_path);
        }

        Ok(())
    }

    /// Get cached tokens for a specific range - optimized version with better error handling
    fn get_cached_tokens_for_range(
        &self,
        _active_slot: usize,
        token_range: Range<usize>,
    ) -> Vec<Token> {
        // Validate range bounds first
        if token_range.start >= token_range.end {
            return Vec::new();
        }

        // Try to get actual tokens from stored entries
        if let Some(Entry::TokenList(stored_tokens)) = self.entries.get(&Id::get("current_tokens"))
        {
            // Ensure we don't exceed bounds
            if token_range.start < stored_tokens.len() {
                let end_idx = std::cmp::min(token_range.end, stored_tokens.len());
                return stored_tokens[token_range.start..end_idx].to_vec();
            }
        }

        // Check alternative token storage locations
        if let Some(Entry::TokenList(backup_tokens)) = self.entries.get(&Id::get("backup_tokens")) {
            if token_range.start < backup_tokens.len() {
                let end_idx = std::cmp::min(token_range.end, backup_tokens.len());
                return backup_tokens[token_range.start..end_idx].to_vec();
            }
        }

        // Return empty vector for proper downstream error handling
        Vec::new()
    }
    // ================================================================================
    // Entry System - Storage and Retrieval Methods
    // ================================================================================

    /// Get a string value from the registry
    pub fn get_string(&self, name: &str) -> Option<String> {
        let id = Id::get(name);
        match self.entries.get(&id) {
            Some(Entry::Str(s)) => Some(s.clone()),
            _ => None,
        }
    }

    /// Get a boolean value from the registry  
    pub fn get_bool(&self, name: &str) -> Option<bool> {
        let id = Id::get(name);
        match self.entries.get(&id) {
            Some(Entry::Bool(b)) => Some(*b),
            _ => None,
        }
    }

    /// Get a u64 value from the registry
    pub fn get_u64(&self, name: &str) -> Option<u64> {
        let id = Id::get(name);
        match self.entries.get(&id) {
            Some(Entry::Val(v)) => Some(*v),
            _ => None,
        }
    }

    /// Set a value in the registry with type conversion
    pub fn set_value<T: Any + Clone + std::fmt::Debug>(&mut self, name: &str, value: T) {
        let id = Id::get(name);
        let entry = self.from_value(value);
        self.entries.insert(id, entry);
        self.entry_count += 1;
    }

    /// Convert a value to an Entry
    fn from_value<T: Any + Clone + std::fmt::Debug>(&self, value: T) -> Entry {
        let any_value = &value as &dyn Any;

        if let Some(v) = any_value.downcast_ref::<Id>() {
            Entry::Id(v.clone())
        } else if let Some(v) = any_value.downcast_ref::<u64>() {
            Entry::Val(*v)
        } else if let Some(v) = any_value.downcast_ref::<String>() {
            Entry::Str(v.clone())
        } else if let Some(v) = any_value.downcast_ref::<bool>() {
            Entry::Bool(*v)
        } else if let Some(v) = any_value.downcast_ref::<PathBuf>() {
            Entry::Path(v.clone())
        } else if let Some(v) = any_value.downcast_ref::<Report>() {
            Entry::HandlerReport(v.clone())
        } else if let Some(v) = any_value.downcast_ref::<Table>() {
            Entry::Table(v.clone())
        } else if let Some(v) = any_value.downcast_ref::<TableCell>() {
            Entry::TableCell(v.clone())
        } else if let Some(v) = any_value.downcast_ref::<Vec<String>>() {
            Entry::List(v.iter().map(|s| Entry::Str(s.clone())).collect())
        } else if let Some(v) = any_value.downcast_ref::<HashMap<String, String>>() {
            Entry::StrMap(
                v.iter()
                    .map(|(k, v)| (k.clone(), Entry::Str(v.clone())))
                    .collect(),
            )
        } else if let Some(v) = any_value.downcast_ref::<Pattern>() {
            Entry::Pattern(v.clone())
        } else {
            panic!(
                "Unsupported type for Entry conversion: {:?}",
                any_value.type_id()
            )
        }
    }
    pub fn insert_pattern(&mut self, pattern_name: &str, pattern: Pattern) {
        let key = format!("pattern_{}", pattern_name);
        let id = Id::get(&key);
        self.insert(id, Entry::Pattern(pattern));
    }

    /// Retrieve a Pattern directly from the registry with enhanced error handling
    pub fn get_pattern(&self, pattern_name: &str) -> Option<Pattern> {
        if pattern_name.is_empty() {
            return None;
        }

        let key = format!("pattern_{}", pattern_name);
        let id = Id::get(&key);

        match self.entries.get(&id) {
            Some(Entry::Pattern(pattern)) => {
                info!("✅ Retrieved pattern '{}' from registry", pattern_name);
                Some(pattern.clone())
            }
            Some(_) => {
                info!(
                    "⚠️ Entry '{}' exists but is not a Pattern type",
                    pattern_name
                );
                None
            }
            None => {
                info!("❌ Pattern '{}' not found in registry", pattern_name);
                None
            }
        }
    }
    /// Get all patterns stored in the registry with enhanced filtering and validation
    pub fn get_patterns(&self) -> HashMap<Id, Pattern> {
        self.entries
            .iter()
            .filter_map(|(id, entry)| {
                match entry {
                    Entry::Pattern(pattern) => {
                        // Validate pattern before including it
                        if !pattern.name.is_empty() {
                            Some((id.clone(), pattern.clone()))
                        } else {
                            None
                        }
                    }
                    _ => None,
                }
            })
            .collect()
    }
    /// Get all entries in the registry
    pub fn entries(&self) -> HashMap<Id, Entry> {
        self.entries.clone()
    }

    /// Insert an entry into the registry
    pub fn insert(&mut self, id: Id, entry: Entry) -> Option<Entry> {
        let old_entry = self.entries.insert(id, entry);
        if old_entry.is_none() {
            self.entry_count += 1;
        }
        old_entry
    }

    /// Remove an entry from the registry
    pub fn remove(&mut self, id: &Id) -> Option<Entry> {
        let result = self.entries.remove(id);
        if result.is_some() {
            self.entry_count -= 1;
        }
        result
    }

    /// Get entry count
    pub fn entry_count(&self) -> u64 {
        self.entry_count
    }

    /// Clear all entries from the registry
    pub fn clear_entries(&mut self) {
        self.entries.clear();
        self.entry_count = 0;
    }

    /// Check if registry contains an entry with the given ID
    pub fn contains_entry(&self, id: &Id) -> bool {
        self.entries.contains_key(id)
    }

    /// Get an entry by ID
    pub fn get_entry(&self, id: Id) -> &Entry {
        self.entries.get(&id).unwrap()
    }

    /// Get a mutable entry by ID
    pub fn get_entry_mut(&mut self, id: Id) -> &mut Entry {
        self.entries.get_mut(&id).unwrap()
    }

    /// Get an entry by ID safely, returning None if not found
    pub fn get_entry_safe(&self, id: &Id) -> Option<&Entry> {
        self.entries.get(id)
    }

    /// Get a mutable entry by ID safely, returning None if not found
    pub fn get_entry_mut_safe(&mut self, id: &Id) -> Option<&mut Entry> {
        self.entries.get_mut(id)
    }

    /// Get all entry IDs
    pub fn get_entry_ids(&self) -> Vec<Id> {
        self.entries.keys().cloned().collect()
    }

    /// Get entries by type
    pub fn get_entries_by_type<F>(&self, filter: F) -> HashMap<Id, Entry>
    where
        F: Fn(&Entry) -> bool,
    {
        self.entries
            .iter()
            .filter(|(_, entry)| filter(entry))
            .map(|(id, entry)| (id.clone(), entry.clone()))
            .collect()
    }

    /// Get all string entries
    pub fn get_string_entries(&self) -> HashMap<Id, String> {
        self.entries
            .iter()
            .filter_map(|(id, entry)| match entry {
                Entry::Str(s) => Some((id.clone(), s.clone())),
                _ => None,
            })
            .collect()
    }

    /// Get all boolean entries
    pub fn get_bool_entries(&self) -> HashMap<Id, bool> {
        self.entries
            .iter()
            .filter_map(|(id, entry)| match entry {
                Entry::Bool(b) => Some((id.clone(), *b)),
                _ => None,
            })
            .collect()
    }

    /// Get all numeric entries
    pub fn get_numeric_entries(&self) -> HashMap<Id, u64> {
        self.entries
            .iter()
            .filter_map(|(id, entry)| match entry {
                Entry::Val(v) => Some((id.clone(), *v)),
                _ => None,
            })
            .collect()
    }

    /// Check if registry is empty
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// Batch insert entries
    pub fn insert_batch(&mut self, entries: HashMap<Id, Entry>) {
        for (id, entry) in entries {
            self.insert(id, entry);
        }
    }

    /// Remove entries by pattern matching on ID
    pub fn remove_patterns(&mut self, pattern: &str) -> Vec<Entry> {
        let ids_to_remove: Vec<Id> = self
            .entries
            .keys()
            .filter(|id| id.name().contains(pattern))
            .cloned()
            .collect();

        ids_to_remove
            .into_iter()
            .filter_map(|id| self.remove(&id))
            .collect()
    }

    /// Update an existing entry, returning success status
    pub fn update_entry(&mut self, id: &Id, entry: Entry) -> bool {
        if self.entries.contains_key(id) {
            self.entries.insert(id.clone(), entry);
            true
        } else {
            false
        }
    }
}

impl Default for Registry {
    fn default() -> Self {
        Self::new()
    }
}
