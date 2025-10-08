#![allow(
    unsafe_code,
    static_mut_refs,
    unused_variables,
    unused_assignments,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals
)]

use crate::{
    Entry, Handler, HandlerResult, Handlizer, Id, Maybe, MaybeLock, OptionLock, Pattern,
    PatternResult, Patternizer, ReadGuard, Registry, ReportLevel, Result, Samplizer, Table, Token,
    TokenBox, Tokenizer, WriteGuard, array_handler::ArrayHandler, comment_handler::CommentHandler,
    enum_handler::EnumHandler, expression_handler::ExpressionHandler,
    function_handler::FunctionHandler, global_handler::GlobalHandler, handlers, logging,
    loop_handler::LoopHandler, macro_handler::MacroHandler, maybe::lock::MaybeLockWriteGuard,
    registry, struct_handler::StructHandler, token, typedef_handler::TypedefHandler,
    variable_handler::VariableHandler,
};
use core::{
    ops::FnOnce,
    option::Option::{None, Some},
};
use std::{
    cmp::Ordering,
    collections::HashMap,
    fmt::Debug,
    hash::{Hash, Hasher},
    ops::Range,
    path::PathBuf,
    result,
    sync::LazyLock,
    time::{SystemTime, UNIX_EPOCH},
};

const TOKEN_MAX: usize = 8192;

#[macro_export]
#[allow(static_mut_refs, unsafe_code)]
macro_rules! context {
    [w] => {
        *$crate::config::context()
    };
    [r] => {
        $crate::config::context()
    };
    [r,$field:ident] => {
        $crate::config::context_with(|ctx| &ctx.$field)
    };
    [$field:ident: $value:expr] => {
        $crate::config::context_with_mut(|ctx| ctx.$field = $value)
    };
}

#[allow(static_mut_refs, unsafe_code)]
pub fn context() -> MaybeLockWriteGuard<'static, Context> {
    unsafe {
        static mut CONTEXT: MaybeLock<Context> = MaybeLock::new();
        if let Maybe::Some(guard) = CONTEXT.try_write() {
            guard
        } else {
            CONTEXT.force();
            CONTEXT
                .try_write()
                .unwrap_or_else(|| panic!("Failed to initialize context"))
        }
    }
}
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Context {
    pub tokenizer: Tokenizer,
    pub tokenbox: TokenBox,
    pub patternizer: Patternizer,
    pub samplizer: Samplizer,
    pub handlizer: Handlizer, // Now includes adaptive, analysis, and redirect functionality
    pub registry: Registry,
}
impl Context {
    #[allow(static_mut_refs, unsafe_code)]
    pub fn new() -> Self {
        Context {
            tokenizer: Tokenizer::new(),
            tokenbox: TokenBox::new(Id::get("tokenbox"), TOKEN_MAX),
            patternizer: Patternizer::new(),
            samplizer: Samplizer::new(),
            handlizer: Handlizer::new(), // Handlizer now contains adaptive, analysis, and redirect functionality
            registry: Registry::new(),
        }
    }
    pub fn sync_tokenizer(&self) {
        context().tokenizer = self.tokenizer.clone();
    }

    pub fn sync_tokenbox(&self) {
        context().tokenbox = self.tokenbox.clone();
    }

    pub fn sync_patternizer(&self) {
        context().patternizer = self.patternizer.clone();
    }

    pub fn sync_samplizer(&self) {
        context().samplizer = self.samplizer.clone();
    }

    pub fn sync_analyzer(&self) {
        context().handlizer = self.handlizer.clone();
    }

    pub fn sync_registry(&self) {
        context().registry = self.registry.clone();
    }

    /// Shutdown all background threads and resources
    pub fn shutdown(&mut self) {
        // Shutdown the samplizer's thread pool
        self.samplizer.shutdown();

        // Shutdown global thread pool
        crate::thread::shutdown_global_pool();
    }

    /// Get the current verbosity level
    pub fn get_verbosity(&self) -> u8 {
        self.registry
            .get_u64("verbosity")
            .map(|v| v as u8)
            .unwrap_or(0)
    }

    /// Set the verbosity level
    pub fn set_verbosity(&mut self, level: u8) {
        self.registry.set_value("verbosity", level as u64);
    }

    pub fn get_base_dir(&self) -> Option<PathBuf> {
        let id = Id::get("base_dir");
        self.registry
            .entries
            .get(&id)
            .and_then(|entry| match entry {
                Entry::Path(path) => Some(path.clone()),
                _ => None,
            })
    }

    /// Set the base directory
    pub fn set_base_dir(&mut self, dir: PathBuf) {
        let id = Id::get("base_dir");
        self.registry.entries.insert(id, Entry::Path(dir));
    }

    /// Set verbosity level
    pub fn with_verbosity(&mut self, level: u8) {
        self.registry.set_value("verbosity", level as u64);
    }

    /// Get system includes setting
    pub fn get_system_includes(&self) -> bool {
        self.registry.get_bool("system_includes").unwrap_or(false)
    }

    /// Set a feature flag with the given name and value
    pub fn set_feature(&mut self, name: &str, value: bool) -> Option<bool> {
        let features_id = Id::get("features");
        match self.registry.entries.get_mut(&features_id) {
            Some(Entry::StrMap(map)) => {
                let previous = map.insert(name.to_string(), Entry::Bool(value));
                previous.and_then(|entry| match entry {
                    Entry::Bool(prev_value) => Some(prev_value),
                    _ => None,
                })
            }
            _ => {
                let mut map = HashMap::new();
                map.insert(name.to_string(), Entry::Bool(value));
                self.registry
                    .entries
                    .insert(features_id, Entry::StrMap(map));
                None
            }
        }
    }

    /// Get the value of a feature flag
    pub fn get_feature(&self, name: &str) -> Option<bool> {
        let features_id = Id::get("features");
        match self.registry.entries.get(&features_id) {
            Some(Entry::StrMap(map)) => match map.get(name) {
                Some(Entry::Bool(value)) => Some(*value),
                _ => None,
            },
            _ => None,
        }
    }

    /// Get an entry by name
    pub fn get_entry(&self, id: &Id) -> Option<Entry> {
        self.registry.entries.get(id).cloned()
    }

    pub fn get_entry_count(&self) -> usize {
        self.registry.entries.len()
    }

    /// Set an entry by name
    pub fn set_entry(&mut self, id: Id, entry: Entry) {
        self.registry.insert(id, entry);
    }
    pub fn initialize(&mut self) {
        // Clear any existing handlers to ensure clean initialization
        self.handlizer.handlers.clear();
        
        let start_time = std::time::Instant::now();
        let mut handler_count = 0;
        
        // Register handlers with proper priority ordering
        // Core language construct handlers - highest priority
        self.handlizer.register(FunctionHandler::new()); // Priority: 200
        self.handlizer.register(StructHandler::new());   // Priority: 180
        self.handlizer.register(EnumHandler::new());     // Priority: 160
        handler_count += 3;

        // Specialized pattern handlers - high priority for specific constructs
        self.handlizer.register(LoopHandler::new());     // Priority: 150
        self.handlizer.register(TypedefHandler::new());  // Priority: 140
        self.handlizer.register(VariableHandler::new()); // Priority: 140
        handler_count += 3;

        // Expression and syntax handlers - medium-high priority
        self.handlizer.register(ExpressionHandler::new()); // Priority: 120
        self.handlizer.register(ArrayHandler::new());      // Priority: 110
        self.handlizer.register(MacroHandler::new());      // Priority: 100
        handler_count += 3;

        // Documentation and metadata handlers - lower priority
        self.handlizer.register(CommentHandler::new());  // Priority: 170
        self.handlizer.register(GlobalHandler::new());   // Priority: 60
        handler_count += 2;

        let elapsed = start_time.elapsed();
        
        // Create comprehensive initialization report
        let init_report = HandlerReport::new(
            "handler_initialization",
            std::sync::Arc::new(Id::get("initialize_handlers")),
            "Context".to_string(),
            "initialize".to_string(),
            format!(
                "Successfully initialized {} handlers in {:?} with priority ordering",
                handler_count, elapsed
            ),
            ReportLevel::Info,
            HandlerPhase::Initialize,
        )
        .with_tokens(0, 0)
        .with_success(true)
        .with_metadata("handler_count".to_string(), handler_count.to_string())
        .with_metadata("initialization_time_ms".to_string(), elapsed.as_millis().to_string());

        self.registry.add_report(init_report);
        
        // Log initialization success
        self.report(
            "context",
            "initialize",
            ReportLevel::Info,
            HandlerPhase::Initialize,
            &format!("Handler initialization completed with {} registered handlers", handler_count),
            true,
        );
    }
    pub fn process(
        &mut self,
        token_slot: usize,
        token_range: Range<usize>,
    ) -> Result<Vec<HandlerResult>> {
        let mut results = Vec::new();
        let start_time = std::time::Instant::now();
        
        // Initial validation and reporting
        if token_range.is_empty() {
            self.report(
                "handlizer",
                "process",
                ReportLevel::Warning,
                HandlerPhase::Process,
                "Empty token range provided",
                false,
            );
            return Ok(results);
        }

        let tokens = self.tokenizer.get_tokens(token_slot, token_range.clone());
        if tokens.is_empty() {
            self.report(
                "handlizer",
                "process",
                ReportLevel::Warning,
                HandlerPhase::Process,
                "No tokens found in range",
                false,
            );
            return Ok(results);
        }

        self.report_with_tokens(
            "handlizer",
            "process",
            ReportLevel::Info,
            HandlerPhase::Process,
            &format!(
                "Starting token processing for {} tokens in range {:?}",
                tokens.len(),
                token_range
            ),
            true,
            tokens.len(),
            0,
        );

        let mut handlers_attempted = 0;
        let mut handlers_successful = 0;
        let sorted_handlers: Vec<_> = self
            .handlizer
            .get_sorted_handlers()
            .into_iter()
            .cloned()
            .collect();

        for handler in sorted_handlers {
            handlers_attempted += 1;

            match handler.can_process(self, token_slot, token_range.clone()) {
                Ok(can_process) => {
                    if can_process {
                        self.report_with_tokens(
                            &handler.id().name(),
                            "process",
                            ReportLevel::Info,
                            HandlerPhase::Process,
                            "Attempting processing",
                            true,
                            tokens.len(),
                            0,
                        );

                        match handler.process(self, token_slot, token_range.clone()) {
                            Ok(result) => {
                                handlers_successful += 1;

                                self.report_with_tokens(
                                    &handler.id().name(),
                                    "process",
                                    ReportLevel::Info,
                                    HandlerPhase::Process,
                                    &format!("Successfully processed tokens: {:?}", result),
                                    true,
                                    tokens.len(),
                                    result.token_count(),
                                );

                                results.push(result.clone());

                                // Stop processing if a handler completed successfully
                                if matches!(result, HandlerResult::Completed(_, _, _, _)) {
                                    self.report(
                                        "handlizer",
                                        "process",
                                        ReportLevel::Info,
                                        HandlerPhase::Process,
                                        "Processing completed successfully, stopping further attempts",
                                        true,
                                    );
                                    break;
                                }
                            }
                            Err(e) => {
                                self.report(
                                    &handler.id().name(),
                                    "process",
                                    ReportLevel::Warning,
                                    HandlerPhase::Process,
                                    &format!("Handler processing failed: {}", e),
                                    false,
                                );

                                // Try redirect on processing failure
                                if let Ok(redirect_result) = self.try_redirect(
                                    token_slot,
                                    token_range.clone(),
                                    HandlerResult::NotHandled(
                                        Some(tokens.clone()),
                                        token_range.clone(),
                                        handler.id(),
                                    ),
                                    &handler.id(),
                                ) {
                                    if !matches!(
                                        redirect_result,
                                        HandlerResult::NotHandled(_, _, _)
                                    ) {
                                        results.push(redirect_result);
                                        handlers_successful += 1;
                                    }
                                }
                            }
                        }
                    }
                }
                Err(e) => {
                    self.report(
                        &handler.id().name(),
                        "can_process",
                        ReportLevel::Error,
                        HandlerPhase::Process,
                        &format!("Error checking handler capability: {}", e),
                        false,
                    );
                }
            }
        }

        let elapsed = start_time.elapsed();
        self.report_with_tokens(
            "handlizer",
            "process",
            if handlers_successful > 0 {
                ReportLevel::Info
            } else {
                ReportLevel::Warning
            },
            HandlerPhase::Process,
            &format!(
                "Processing complete: {}/{} handlers successful, {} results, took {:?}",
                handlers_successful,
                handlers_attempted,
                results.len(),
                elapsed
            ),
            handlers_successful > 0,
            tokens.len(),
            results.iter().map(|r| r.token_count()).sum(),
        );

        Ok(results)
    }
    /// Enhanced redirect system with intelligent routing and learning
    pub fn try_redirect(
        &mut self,
        token_slot: usize,
        token_range: Range<usize>,
        result: HandlerResult,
        handler_id: &Id,
    ) -> Result<HandlerResult> {
        let start_time = std::time::Instant::now();
        let tokens = self.tokenizer.get_tokens(token_slot, token_range.clone());

        self.report_with_tokens(
            "handlizer",
            "try_redirect",
            ReportLevel::Info,
            HandlerPhase::Process,
            &format!(
                "Attempting intelligent redirect for failed handler: {}",
                handler_id.name()
            ),
            true,
            tokens.len(),
            0,
        );

        // Create redirect request
        let redirect_request = crate::handler::RedirectRequest {
            from_handler: handler_id.clone(),
            token_range: token_range.clone(),
            failed_patterns: Vec::new(),
            suggested_handler: None,
            metadata: Vec::new(),
        };

        // Use the workflow helper to handle redirect with proper borrowing
        match Handlizer::handle_redirect_workflow(
            self,
            token_slot,
            token_range.clone(),
            redirect_request,
        ) {
            Ok(redirect_result) => {
                if !matches!(redirect_result, HandlerResult::NotHandled(_, _, _)) {
                    let elapsed = start_time.elapsed();
                    self.report_with_tokens(
                        "handlizer",
                        "try_redirect",
                        ReportLevel::Info,
                        HandlerPhase::Process,
                        &format!("Redirect successful in {:?}", elapsed),
                        true,
                        tokens.len(),
                        redirect_result.token_count(),
                    );
                    return Ok(redirect_result);
                }
            }
            Err(e) => {
                self.report_with_tokens(
                    "handlizer",
                    "try_redirect",
                    ReportLevel::Debug,
                    HandlerPhase::Process,
                    &format!("Redirect workflow failed: {}", e),
                    false,
                    tokens.len(),
                    0,
                );
            }
        }

        // If all redirects failed, return the original result
        let elapsed = start_time.elapsed();
        self.report_with_tokens(
            "handlizer",
            "try_redirect",
            ReportLevel::Warning,
            HandlerPhase::Process,
            &format!("All redirect strategies exhausted in {:?}", elapsed),
            false,
            tokens.len(),
            0,
        );

        Ok(HandlerResult::NotHandled(
            Some(tokens),
            token_range,
            handler_id.clone(),
        ))
    }
    /// Match patterns for a handler using the patternizer
    pub fn match_pattern(&mut self, handler_type: &str, tokens: &[crate::Token]) -> PatternResult {
        self.patternizer.match_pattern(handler_type, tokens)
    }

    /// Check if tokens should be rejected by any handler
    pub fn should_reject_tokens(&mut self, handler_type: &str, tokens: &[crate::Token]) -> bool {
        self.patternizer.should_reject_tokens(handler_type, tokens)
    }

    /// Get pattern statistics
    pub fn get_pattern_stats(&self) -> std::collections::HashMap<String, usize> {
        self.patternizer.get_stats()
    }

    /// Save pattern cache to JSON file
    pub fn save_pattern_cache(&self, file_path: &str) -> crate::error::Result<()> {
        self.patternizer.save_cache_to_json(file_path)
    }

    /// Load pattern cache from JSON file
    pub fn load_pattern_cache(&mut self, file_path: &str) -> crate::error::Result<()> {
        self.patternizer.load_cache_from_json(file_path)
    }

    /// Get a string value from the registry
    pub fn get_string(&self, name: &str) -> Option<String> {
        self.registry.get_string(name)
    }

    /// Get a boolean value from the registry
    pub fn get_bool(&self, name: &str) -> Option<bool> {
        self.registry.get_bool(name)
    }

    /// Get a u64 value from the registry
    pub fn get_u64(&self, name: &str) -> Option<u64> {
        self.registry.get_u64(name)
    }

    /// Set a value in the registry
    pub fn set_value(&mut self, name: &str, value: Entry) {
        let id = Id::get(name);
        self.registry.insert(id, value);
    }

    /// Get include directories
    pub fn get_include_dirs(&self) -> Vec<PathBuf> {
        self.registry
            .entries
            .get(&Id::get("include_directories"))
            .and_then(|entry| match entry {
                Entry::List(paths) => Some(
                    paths
                        .iter()
                        .filter_map(|e| match e {
                            Entry::Path(p) => Some(p.clone()),
                            _ => None,
                        })
                        .collect(),
                ),
                _ => None,
            })
            .unwrap_or_else(Vec::new)
    }

    /// Set include directories
    pub fn set_include_dirs(&mut self, paths: Vec<PathBuf>) {
        self.registry.insert(
            Id::get("include_directories"),
            Entry::List(paths.into_iter().map(Entry::Path).collect()),
        );
    }

    /// Configure system includes
    pub fn with_system_includes(&mut self, enabled: bool) {
        self.registry
            .insert(Id::get("system_includes"), Entry::Bool(enabled));
    }

    /// Register a handler with the context
    pub fn register_handler(&mut self, handler: Box<dyn Handler>) {
        let id = handler.id();
        self.registry.insert(id, Entry::Handler(handler));
    }

    // ===== SAMPLIZER DELEGATION FUNCTIONS =====

    /// Get validation results from the global Samplizer
    pub fn get_validation_results(self) -> HashMap<String, crate::ValidationResult> {
        self.samplizer.validation_results.clone()
    }

    /// Collect tokens from TokenBox for processing
    pub fn collect_tokens(&self, max_tokens: Option<usize>) -> Result<Vec<Token>> {
        self.tokenbox.collect_tokens(max_tokens)
    }

    /// Check TokenBox status
    pub fn tokenbox_status(&self) -> crate::TokenBoxStatus {
        self.tokenbox.status()
    }

    // ===== ADAPTIVE ANALYSIS DELEGATION FUNCTIONS =====
    // Analysis functionality now integrated into Handlizer

    /// Record successful pattern matching for learning
    pub fn record_pattern_success(
        &mut self,
        handler_id: &Id,
        tokens_consumed: usize,
        confidence: f64,
    ) {
        self.handlizer
            .record_success(handler_id, tokens_consumed, confidence);
    }

    /// Record failed pattern matching for learning  
    pub fn record_pattern_failure(
        &mut self,
        handler_id: &Id,
        tokens_attempted: usize,
        reason: String,
    ) {
        self.handlizer
            .record_failure(handler_id, tokens_attempted, reason);
    }

    /// Store a Pattern directly in the registry
    pub fn insert_pattern(&mut self, pattern_name: &str, pattern: Pattern) {
        self.registry.insert_pattern(pattern_name, pattern);
    }

    /// Retrieve a Pattern directly from the registry
    pub fn get_pattern(&self, pattern_name: &str) -> Option<Pattern> {
        self.registry.get_pattern(pattern_name)
    }

    /// Add a handler report to the centralized reporting system
    pub fn add_report(&mut self, report: HandlerReport) {
        self.registry.add_report(report);
    }

    /// Create and add a report
    pub fn report(
        &mut self,
        handler_name: &str,
        function_name: &str,
        level: ReportLevel,
        phase: HandlerPhase,
        message: &str,
        success: bool,
    ) {
        let report_id = Id::get(&Id::gen_name(&format!(
            "report_{}_{}",
            handler_name, function_name
        )));
        let handler_id = Id::get(handler_name);

        let report = HandlerReport {
            report_id: std::sync::Arc::new(report_id),
            handler_id: std::sync::Arc::new(handler_id),
            handler_name: handler_name.to_string(),
            function_name: function_name.to_string(),
            message: message.to_string(),
            details: None,
            level,
            tokens_processed: 0,
            tokens_consumed: 0,
            phase,
            success,
            metadata: std::collections::HashMap::new(),
        };

        self.add_report(report);
    }

    /// Create and add a report with token counts
    pub fn report_with_tokens(
        &mut self,
        handler_name: &str,
        function_name: &str,
        level: ReportLevel,
        phase: HandlerPhase,
        message: &str,
        success: bool,
        tokens_processed: usize,
        tokens_consumed: usize,
    ) {
        let report_id = Id::get(&Id::gen_name(&format!(
            "report_{}_{}",
            handler_name, function_name
        )));
        let handler_id = Id::get(handler_name);

        let report = HandlerReport {
            report_id: std::sync::Arc::new(report_id),
            handler_id: std::sync::Arc::new(handler_id),
            handler_name: handler_name.to_string(),
            function_name: function_name.to_string(),
            message: message.to_string(),
            details: None,
            level,
            tokens_processed,
            tokens_consumed,
            phase,
            success,
            metadata: std::collections::HashMap::new(),
        };

        self.registry.add_report(report);
    }

    /// Get all handler reports from the registry
    pub fn get_reports(&self) -> Vec<HandlerReport> {
        self.registry.get_reports()
    }

    /// Get reports filtered by handler name
    pub fn get_reports_by_handler(&self, handler_name: &str) -> Vec<HandlerReport> {
        self.registry.get_reports_by_handler(handler_name)
    }

    /// Get reports filtered by report level
    pub fn get_reports_by_level(&self, level: &ReportLevel) -> Vec<HandlerReport> {
        self.registry.get_reports_by_level(level)
    }

    /// Get reports filtered by handler phase
    pub fn get_reports_by_phase(&self, phase: &HandlerPhase) -> Vec<HandlerReport> {
        self.registry.get_reports_by_phase(phase)
    }

    /// Display all reports using the sophisticated Range-based table system
    pub fn display_reports(&self) {
        let reports = self.registry.get_reports();
        self.display_reports_with_table(&reports);
    }

    /// Display reports using the new Range-based table system
    fn display_reports_with_table(&self, reports: &[HandlerReport]) {
        if reports.is_empty() {
            println!("No reports to display");
            return;
        }

        // Create table using the new Range-based coordinate system
        let mut table = crate::Table::new_registry_table("Handler System Reports");

        // Add headers with better spacing
        table.add_header(vec![
            "Handler", "Status", "Function", "Phase", "Tokens", "Message",
        ]);

        // Add report data (limit to 20 for readability)
        for report in reports.iter().take(20) {
            let handler_name = truncate_string(&report.handler_name, 19);
            let status = if report.success { "✅" } else { "❌" };
            let function_name = truncate_string(&report.function_name, 15);
            let phase = format!("{}", report.phase);
            let message = truncate_string(&report.message, 25);

            table.add_row(vec![
                Entry::Str(handler_name),
                Entry::Str(status.to_string()),
                Entry::Str(function_name),
                Entry::Str(phase),
                Entry::Val(report.tokens_processed as u64),
                Entry::Str(message),
            ]);
        }

        // Display the formatted table
        println!("{}", table.display_formatted());

        // Show additional info
        if reports.len() > 20 {
            println!("📝 Showing first 20 of {} reports", reports.len());
        }

        let success_count = reports.iter().filter(|r| r.success).count();
        let success_rate = (success_count as f64 / reports.len() as f64) * 100.0;
        println!(
            "📈 Success Rate: {:.1}% ({}/{} successful)\n",
            success_rate,
            success_count,
            reports.len()
        );
    }

    /// Simple report display that doesn't require Context access
    fn display_reports_simple(&self, reports: &[HandlerReport]) {
        if reports.is_empty() {
            println!("📚 No reports available");
            return;
        }

        println!("\n=== 🔧 HANDLER SYSTEM REPORTS ===");
        println!("📊 Total Reports: {}", reports.len());

        let success_count = reports.iter().filter(|r| r.success).count();
        let success_rate = (success_count as f64 / reports.len() as f64) * 100.0;
        println!("✅ Success Rate: {:.1}%\n", success_rate);

        // Group by handler for cleaner display
        let mut handler_groups: std::collections::HashMap<String, Vec<&HandlerReport>> =
            std::collections::HashMap::new();
        for report in reports {
            handler_groups
                .entry(report.handler_name.clone())
                .or_default()
                .push(report);
        }

        for (handler_name, handler_reports) in handler_groups {
            println!("🔨 {}: {} reports", handler_name, handler_reports.len());
            for report in handler_reports.iter().take(3) {
                // Show max 3 per handler
                let status = if report.success { "✅" } else { "❌" };
                println!("   {} {}: {}", status, report.function_name, report.message);
                if report.tokens_processed > 0 {
                    println!(
                        "      📊 Tokens: {} processed, {} consumed",
                        report.tokens_processed, report.tokens_consumed
                    );
                }
            }
            if handler_reports.len() > 3 {
                println!("   ... and {} more reports", handler_reports.len() - 3);
            }
            println!();
        }

        println!("=== END REPORTS ===\n");
    }

    /// Display reports filtered by level (delegates to Registry)
    pub fn display_reports_by_level(&self, level: &ReportLevel) {
        let reports = self.get_reports_by_level(level);
        self.display_reports_simple(&reports);
    }

    /// Display reports in table format - NEW FUNCTION for better formatting
    pub fn display_reports_table(&self) {
        // Get reports in one atomic operation
        let reports = self.get_reports();

        // Process reports without holding any Context locks
        self.display_reports_table_format(&reports);
    }

    /// Create and display a formatted table from reports using Range-based coordinates
    fn display_reports_table_format(&self, reports: &[HandlerReport]) {
        if reports.is_empty() {
            println!("📋 No reports available for table display\n");
            return;
        }

        // Use the sophisticated Range-based table system
        self.display_reports_with_table(reports);
    }

    /// Clear all reports from the registry (delegates to Registry)
    pub fn clear_reports(&mut self) {
        self.registry.clear_reports();
    }

    /// Display the entire registry in a database-like format for debugging
    pub fn display_registry_database(&self) {
        println!("🗃️  Context Registry Database View:");
        let reports = self.registry.get_reports();
        self.display_reports_table_format(&reports);
    }

    /// Display registry statistics for debugging
    pub fn display_registry_stats(&self) {
        println!("📊 Context Registry Statistics:");
        let reports = self.registry.get_reports();

        if reports.is_empty() {
            println!("No reports available for statistics\n");
            return;
        }

        let mut handler_counts: std::collections::HashMap<String, usize> =
            std::collections::HashMap::new();
        let mut phase_counts: std::collections::HashMap<HandlerPhase, usize> =
            std::collections::HashMap::new();
        let mut level_counts: std::collections::HashMap<ReportLevel, usize> =
            std::collections::HashMap::new();

        for report in &reports {
            *handler_counts
                .entry(report.handler_name.clone())
                .or_insert(0) += 1;
            *phase_counts.entry(report.phase.clone()).or_insert(0) += 1;
            *level_counts.entry(report.level.clone()).or_insert(0) += 1;
        }

        println!("By Handler:");
        for (handler, count) in handler_counts {
            println!("  - {}: {}", handler, count);
        }

        println!("By Phase:");
        for (phase, count) in phase_counts {
            println!("  - {}: {}", phase, count);
        }

        println!("By Level:");
        for (level, count) in level_counts {
            println!("  - {}: {}", level, count);
        }
        println!();
    }

    /// Display all context information including registry and reports
    pub fn display_full_context(&self) {
        println!("FULL CONTEXT DISPLAY");
        println!("===============================================================");

        // Display basic context info
        let reports = self.registry.get_reports();
        println!("Context Overview:");
        println!("  - Reports: {}", reports.len());
        println!();

        // Display registry database view
        self.display_registry_database();
        println!();

        // Display registry statistics
        self.display_registry_stats();
        println!();

        // Display reports summary
        if !reports.is_empty() {
            println!("Reports Summary:");
            let mut level_counts: HashMap<ReportLevel, i32> = HashMap::new();
            for report in reports {
                *level_counts.entry(report.level.clone()).or_insert(0) += 1;
            }
            for (level, count) in level_counts {
                println!("  - {}: {}", level, count);
            }
            println!();
        }

        println!("===============================================================");
    }
}

/// Helper function to truncate strings for table display
fn truncate_string(s: &str, max_len: usize) -> String {
    if s.len() <= max_len {
        format!("{:<width$}", s, width = max_len)
    } else {
        format!(
            "{:<width$}",
            format!("{}...", &s[..max_len - 3]),
            width = max_len
        )
    }
}

/// Comprehensive handler reporting structure for centralized debugging and statistics
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HandlerReport {
    pub report_id: std::sync::Arc<Id>,
    pub handler_id: std::sync::Arc<Id>,
    pub handler_name: String,
    pub function_name: String,
    pub message: String,
    pub details: Option<String>,
    pub level: ReportLevel,
    pub tokens_processed: usize,
    pub tokens_consumed: usize,
    pub phase: HandlerPhase,
    pub success: bool,
    pub metadata: HashMap<String, String>,
}

impl HandlerReport {
    pub fn timestamp(&self) -> u128 {
        self.handler_id.timestamp()
    }

    pub fn report_id(&self) -> &Id {
        &self.report_id
    }

    pub fn handler_id(&self) -> &Id {
        &self.handler_id
    }

    pub fn handler_name(&self) -> &str {
        &self.handler_name
    }

    pub fn function_name(&self) -> &str {
        &self.function_name
    }

    pub fn message(&self) -> &str {
        &self.message
    }

    pub fn level(&self) -> &ReportLevel {
        &self.level
    }

    pub fn tokens_processed(&self) -> usize {
        self.tokens_processed
    }

    pub fn tokens_consumed(&self) -> usize {
        self.tokens_consumed
    }

    pub fn phase(&self) -> &HandlerPhase {
        &self.phase
    }

    pub fn success(&self) -> bool {
        self.success
    }

    pub fn metadata(&self) -> &HashMap<String, String> {
        &self.metadata
    }
}

impl PartialOrd for HandlerReport {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for HandlerReport {
    fn cmp(&self, other: &Self) -> Ordering {
        self.handler_id
            .cmp(&other.handler_id)
            .then_with(|| self.phase.cmp(&other.phase))
            .then_with(|| self.tokens_processed.cmp(&other.tokens_processed))
            .then_with(|| self.level.cmp(&other.level))
            .then_with(|| self.handler_name.cmp(&other.handler_name))
    }
}

impl Hash for HandlerReport {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.handler_id.hash(state);
        self.handler_name.hash(state);
        self.function_name.hash(state);
        self.message.hash(state);
        self.level.hash(state);
        self.tokens_processed.hash(state);
        self.tokens_consumed.hash(state);
        self.phase.hash(state);
        self.success.hash(state);
        for (k, v) in &self.metadata {
            k.hash(state);
            v.hash(state);
        }
    }
}
impl std::fmt::Display for HandlerReport {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{}] {} | {} | {} | {}",
            self.timestamp(),
            self.level(),
            self.handler_name(),
            self.phase(),
            self.message()
        )
    }
}

/// Handler processing phases
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum HandlerPhase {
    Initialize,
    Process,
    Handle,
    Extract,
    Convert,
    Report,
    Result,
    Redirect,
}

impl std::fmt::Display for HandlerPhase {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HandlerPhase::Initialize => write!(f, "INITIALIZE"),
            HandlerPhase::Process => write!(f, "PROCESS"),
            HandlerPhase::Handle => write!(f, "HANDLE"),
            HandlerPhase::Extract => write!(f, "EXTRACT"),
            HandlerPhase::Convert => write!(f, "CONVERT"),
            HandlerPhase::Report => write!(f, "REPORT"),
            HandlerPhase::Result => write!(f, "RESULT"),
            HandlerPhase::Redirect => write!(f, "REDIRECT"),
        }
    }
}

/// Pending redirect structure for deferred handler processing
/// This allows redirect functions to queue up handler calls without direct function access
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HandlerRedirect {
    pub source_handler: String,
    pub target_handler: String,
    pub directive_type: String,
    pub tokens: Vec<Token>,
    pub original_result: String, // Serialized HandlerResult for later processing
    timestamp: u128,
}

impl HandlerRedirect {
    pub fn new(
        source_handler: String,
        target_handler: String,
        directive_type: String,
        tokens: Vec<Token>,
        original_result: String,
    ) -> Self {
        Self {
            source_handler,
            target_handler,
            directive_type,
            tokens,
            original_result,
            timestamp: SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap_or_default()
                .as_nanos(),
        }
    }
    pub fn timestamp(&self) -> u128 {
        self.timestamp.clone()
    }
    pub fn source_handler(&self) -> &str {
        &self.source_handler
    }
    pub fn target_handler(&self) -> &str {
        &self.target_handler
    }
    pub fn directive_type(&self) -> &str {
        &self.directive_type
    }
    pub fn tokens(&self) -> &Vec<crate::Token> {
        &self.tokens
    }
    pub fn original_result(&self) -> &str {
        &self.original_result
    }
}

impl std::fmt::Display for HandlerRedirect {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Redirect({} -> {} | {} | {} tokens)",
            self.source_handler(),
            self.target_handler(),
            self.directive_type(),
            self.tokens().len().to_string()
        )
    }
}

impl HandlerReport {
    pub fn new(
        report_name: &str,
        handler_id: std::sync::Arc<Id>,
        handler_name: String,
        function_name: String,
        message: String,
        level: ReportLevel,
        phase: HandlerPhase,
    ) -> Self {
        let timestamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_millis() as u64;

        HandlerReport {
            report_id: std::sync::Arc::new(Id::get(report_name)),
            handler_id,
            handler_name,
            function_name,
            message,
            details: None,
            level,
            tokens_processed: 0,
            tokens_consumed: 0,
            phase,
            success: true,
            metadata: HashMap::new(),
        }
    }
    /// Set token statistics
    pub fn with_tokens(mut self, processed: usize, consumed: usize) -> Self {
        self.tokens_processed = processed;
        self.tokens_consumed = consumed;
        self
    }

    /// Set success status
    pub fn with_success(mut self, success: bool) -> Self {
        self.success = success;
        self
    }

    /// Add metadata key-value pair
    pub fn with_metadata(mut self, key: String, value: String) -> Self {
        self.metadata.insert(key, value);
        self
    }
}
