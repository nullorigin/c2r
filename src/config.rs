#![allow(
    unused_variables,
    unused_assignments,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals
)]

use crate::{
    Entry, Handler, HandlerMap, Id, Lock, Pattern, Patternizer, Registry, ReportLevel, Token,
    Tokenizer, VERBOSITY_LEVEL, time,
};
use std::{
    any::Any,
    cmp::Ordering,
    collections::HashMap,
    convert::Into,
    fmt::Debug,
    hash::{Hash, Hasher},
    ops::{FnMut, Range},
    option::Option::None,
    path::PathBuf,
    time::{SystemTime, UNIX_EPOCH},
};
// Range operators used in tokens! macro indexing

pub static CONTEXT: Lock<Context> = Lock::<Context>::new();
pub struct Global;
impl Global {
    pub fn write<F, R>(mut f: F) -> R
    where
        F: FnMut(&mut Context) -> R,
    {
        let guard: &mut Context = &mut CONTEXT.lock_mut();
        f(guard)
    }

    pub fn read<F, R>(mut f: F) -> R
    where
        F: FnMut(&Context) -> R,
    {
        let guard: &Context = &CONTEXT.lock();
        f(guard)
    }

    pub fn update<F: FnMut(&mut Context) -> Context>(mut f: F) {
        Self::write(|ctx: &mut Context| {
            let mut ctx = ctx.clone();
            f(&mut ctx)
        });
    }
    pub fn registry() -> Registry {
        Self::read(|ctx: &Context| ctx.registry.clone())
    }
    pub fn handler_map() -> HandlerMap {
        Self::read(|ctx: &Context| ctx.handlers.clone())
    }
    pub fn tokens() -> Vec<Token> {
        Self::read(|ctx: &Context| ctx.tokens.clone())
    }
    pub fn tokenizer() -> Tokenizer {
        Self::read(|ctx: &Context| ctx.tokenizer.clone())
    }
    pub fn patternizer() -> Patternizer {
        Self::read(|ctx: &Context| ctx.patternizer.clone())
    }
    pub fn pending() -> Vec<HandlerRedirect> {
        Self::read(|ctx: &Context| ctx.pending_redirects.clone())
    }
}

#[macro_export]
macro_rules! context {
    () => {
        {
            $crate::config::Global::read(|ctx: &$crate::config::Context| ctx.clone())
        }
    };
    ($cmd:ident($($args:expr),+)) => {
        {
            $crate::config::Global::write(|ctx: &mut $crate::config::Context| ctx.$cmd($($args),+))
        }
    };
    ($cmd:ident()) => {
        {
            $crate::config::Global::write(|ctx: &mut $crate::config::Context| ctx.$cmd())
        }
    };
    ($context:expr) => {
        {
            $crate::config::Global::write(|ctx| *ctx = $context)
        }
    };
    ($field:ident, $value:expr, $op:tt) => {
        {
            $crate::config::Global::write(|ctx| ctx.$field $op $value)
        }
    };
}

#[macro_export]
macro_rules! tokens {
    () => {{ $crate::config::Global::tokens() }};
    ($range:expr) => {
        $crate::config::Global::read(|ctx| {
            let tokens = &ctx.tokens();
            let range = $range;
            if range.end <= tokens.len() {
                tokens[range].to_vec()
            } else {
                Vec::new()
            }
        })
    };
    ($range1:expr, $value:expr) => {
        $crate::config::Global::write(|ctx: &mut $crate::config::Context| {
            ctx.tokens[$range1] = $value.clone()
        })
    };
    ($range1:expr, $range2:expr) => {
        $crate::config::Global::write(|ctx: &mut $crate::config::Context| {
            ctx.tokens[$range1] = ctx.tokens[$range2].clone()
        })
    };
}

impl From<Context> for Registry {
    fn from(value: Context) -> Self {
        value.registry
    }
}

impl From<Context> for HandlerMap {
    fn from(value: Context) -> Self {
        value.handlers
    }
}

impl From<&mut Context> for Option<Context> {
    fn from(value: &mut Context) -> Self {
        Some(value.clone())
    }
}
/// Configuration options for the C2R converter
#[derive(Debug)]
pub struct Context {
    pub handlers: HandlerMap,
    pub patternizer: Patternizer,
    pub pending_redirects: Vec<HandlerRedirect>,
    pub registry: Registry,
    pub tokenizer: Tokenizer,
    pub tokens: Vec<Token>,
    pub original_tokens: Vec<Token>,
}
impl Clone for Context {
    fn clone(&self) -> Self {
        Self {
            handlers: self.handlers.clone(),
            tokenizer: self.tokenizer.clone(),
            patternizer: self.patternizer.clone(),
            registry: self.registry.clone(),
            pending_redirects: self.pending_redirects.clone(),
            tokens: self.tokens.clone(),
            original_tokens: self.original_tokens.clone(),
        }
    }
}
impl Eq for Context {}
impl PartialEq for Context {
    fn eq(&self, other: &Self) -> bool {
        self.registry == other.registry
            && self.handlers == other.handlers
            && self.tokenizer == other.tokenizer
            && self.patternizer == other.patternizer
            && self.pending_redirects == other.pending_redirects
    }
}

impl Context {
    /// Create a new configuration with the given name
    pub fn new(name: &str) -> Self {
        Context {
            handlers: HandlerMap::new(name),
            tokenizer: Tokenizer::new(name),
            patternizer: Patternizer::new(name),
            registry: Registry::new(name),
            pending_redirects: Vec::<HandlerRedirect>::new(),
            tokens: Vec::<Token>::new(),
            original_tokens: Vec::<Token>::new(),
        }
    }
    /// Get the current verbosity level
    pub fn get_verbosity(&self) -> u8 {
        let entry = self.get_entry("verbosity");
        if let Some(Entry::Val(verbosity)) = entry {
            verbosity.clone() as u8
        } else {
            0
        }
    }

    /// Set the verbosity level
    pub fn set_verbosity(&mut self, level: u8) {
        self.set_entry("verbosity", Entry::Val(level as u64));
    }

    /// Get a reference to the registry
    pub fn get_registry(&self) -> &Registry {
        &self.registry
    }

    /// Get a mutable reference to the registry
    pub fn get_registry_mut(&mut self) -> &mut Registry {
        &mut self.registry
    }

    /// Replace the current registry
    pub fn set_registry(&mut self, registry: Registry) {
        self.registry = registry;
    }

    /// Add an include directory
    pub fn add_include_dir(&mut self, dir: impl Into<PathBuf>) -> &mut Self {
        let id = Id::get("include_directories");
        let paths = match self.registry.entries.get(&id) {
            Some(Entry::List(paths)) => {
                let mut paths = paths.clone();
                paths.push(Entry::Path(dir.into()));
                paths
            }
            Some(Entry::Path(existing)) => {
                let mut paths = Vec::new();
                paths.push(Entry::Path(existing.clone()));
                paths.push(Entry::Path(dir.into()));
                paths
            }
            _ => vec![Entry::Path(dir.into())],
        };

        self.registry.insert(id, Entry::List(paths));
        self
    }

    /// Set the base directory and return self for builder pattern
    pub fn with_base_dir(&mut self, dir: impl Into<PathBuf>) -> &mut Context {
        let id = Id::get("base_dir");
        self.registry.insert(id, Entry::Path(dir.into()));
        self
    }

    /// Set verbosity level and return self for builder pattern
    pub fn with_verbosity(&mut self, level: u8) -> &mut Context {
        let _id = Id::get("verbosity");
        self.set_verbosity(level);
        self
    }

    /// Get system includes setting
    pub fn get_system_includes(&self) -> bool {
        let id = Id::get("system_includes");
        self.registry
            .entries
            .get(&id)
            .and_then(|entry| match entry {
                Entry::Bool(value) => Some(*value),
                _ => None,
            })
            .unwrap_or(false)
    }

    /// Apply this configuration globally
    pub fn push(&mut self) {
        Global::write(|ctx| *ctx = self.clone());
    }

    pub fn pull(&mut self) {
        Global::read(|ctx| *self = ctx.clone());
    }

    /// Set a feature flag with the given name and value
    pub fn set_feature(&mut self, name: &str, value: bool) -> Option<bool> {
        let id = Id::get(name);
        let features_entry = self
            .registry
            .entries
            .entry(id)
            .or_insert_with(|| Entry::StrMap(HashMap::new()));

        if let Entry::StrMap(features_map) = features_entry {
            let previous = features_map.insert(name.to_string(), Entry::Bool(value));
            previous.and_then(|entry| match entry {
                Entry::Bool(prev_value) => Some(prev_value),
                _ => None,
            })
        } else {
            None
        }
    }

    /// Get the value of a feature flag
    pub fn get_feature(&self, name: &str) -> Option<bool> {
        let id = Id::get("features");
        self.registry
            .entries
            .get(&id)
            .and_then(|entry| match entry {
                Entry::StrMap(features_map) => {
                    features_map.get(name).and_then(|value| match value {
                        Entry::Bool(b) => Some(*b),
                        _ => None,
                    })
                }
                _ => None,
            })
    }

    pub fn get_entry(&self, name: &str) -> Option<&Entry> {
        let id = Id::get(name);
        println!("🔍 Getting entry '{}' with ID: {}", name, id);
        let registry_entries_count = self.registry.entries.len();
        println!("🔍 Registry has {} entries", registry_entries_count);

        if name.starts_with("pattern_") {
            println!("🔍 Registry contents for pattern lookup:");
            for (entry_id, entry) in self.registry.entries.iter() {
                let clean_id = entry_id.name().trim_end_matches("\n");
                let clean_entry = match entry {
                    Entry::Patternizer(p) => {
                        format!("Patternizer({})", p.name().replace("\n", " | "))
                    }
                    _ => "Other".to_string(),
                };
                println!(" - ID: {} -> {}", clean_id, clean_entry);
            }
        }

        let result = self.registry.entries.get(&id);
        if result.is_some() {
            println!("✅ Found entry '{}' in registry", name);
        } else {
            println!("❌ Entry '{}' NOT found in registry", name);
        }
        result
    }
    /// Set a value in the registry
    pub fn set_entry(&mut self, name: &str, entry: Entry) {
        let id = Id::get(name);
        let entry_count = self.registry.entries.len();
        println!("🔧 Setting entry '{}' with ID: {}", name, id);
        println!("🔧 Registry before insert has {} entries", entry_count);
        self.registry.entries.insert(id.clone(), entry);
        let entry_count_after = self.registry.entries.len();
        println!("🔧 Registry after insert has {} entries", entry_count_after);

        // Verify the entry was actually stored
        if self.registry.entries.contains_key(&id) {
            println!("✅ Verified entry '{}' was stored successfully", name);
        } else {
            println!("❌ Entry '{}' was NOT stored!", name);
        }
    }
    pub fn id(&self, name: &str) -> Option<Id> {
        let entry = self.registry.entries.get(&Id::get(name));
        if let Some(Entry::Id(value)) = entry {
            return Some(value.clone());
        } else {
            None
        }
    }
    pub fn pattern(&self, name: &str) -> Option<Pattern> {
        let entry = self.registry.entries.get(&Id::get(name));
        if let Some(Entry::Patternizer(value)) = entry {
            return Some(value.clone());
        } else {
            None
        }
    }
    pub fn original_tokens(&self) -> &[Token] {
        &self.original_tokens
    }
    pub fn tokens(&self) -> &[Token] {
        &self.tokens
    }
    pub fn tokens_mut(&mut self) -> &mut [Token] {
        &mut self.tokens
    }
    pub fn token_at(&self, index: usize) -> Option<&Token> {
        self.tokens.get(index)
    }
    pub fn token_at_mut(&mut self, index: usize) -> Option<&mut Token> {
        self.tokens.get_mut(index)
    }
    pub fn token_range(&self, range: Range<usize>) -> &[Token] {
        &self.tokens[range]
    }
    pub fn token_range_mut(&mut self, range: Range<usize>) -> &mut [Token] {
        &mut self.tokens[range]
    }
    /// Initialize common patterns in the patternizer (proper Context method)
    pub fn initialize_patterns(&mut self) {
        self.patternizer.initialize_common_patterns();
    }

    /// Match patterns for a handler using the patternizer (proper Context method)
    pub fn match_pattern(
        &mut self,
        handler_type: &str,
        tokens: &[crate::Token],
    ) -> crate::pattern::PatternResult {
        self.patternizer.match_pattern(handler_type, tokens)
    }

    /// Check if tokens should be rejected by any handler (proper Context method)
    pub fn should_reject_tokens(&mut self, handler_type: &str, tokens: &[crate::Token]) -> bool {
        self.patternizer.should_reject_tokens(handler_type, tokens)
    }

    /// Get pattern statistics (read-only Context method)
    pub fn get_pattern_stats(&self) -> std::collections::HashMap<String, usize> {
        self.patternizer.get_stats()
    }
    /// Get a value from the registry
    /// Get a value from the registry
    /// Get a value from the registry (delegates to Registry)
    pub fn get_value<T: Any + Clone + Debug>(&self, name: &str) -> Box<T> {
        self.registry.get_value(name)
    }

    /// Set a value in the registry (delegates to Registry)
    pub fn set_value<T: Any + Clone + Debug>(&mut self, name: &str, value: T) {
        self.registry.set_value(name, value);
    }

    /// Get the base directory
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

    /// Get include directories
    pub fn get_include_dirs(&self) -> Vec<PathBuf> {
        match self
            .registry
            .entries
            .get(&Id::new(0, "include_directories".to_string()))
        {
            Some(Entry::Path(path)) => vec![path.clone()],
            Some(Entry::List(paths)) => paths
                .iter()
                .filter_map(|e| match e {
                    Entry::Path(p) => Some(p.clone()),
                    _ => None,
                })
                .collect(),
            _ => Vec::new(),
        }
    }

    /// Set include directories
    pub fn set_include_dirs(&mut self, paths: Vec<PathBuf>) -> &mut Self {
        self.registry.insert(
            Id::get("include_directories"),
            Entry::List(paths.into_iter().map(Entry::Path).collect()),
        );
        self
    }

    /// Configure system includes and return self for builder pattern
    pub fn with_system_includes(&mut self, enabled: bool) -> &mut Context {
        self.registry
            .insert(Id::get("system_includes"), Entry::Bool(enabled));
        self
    }

    /// Register a handler with the context
    pub fn register_handler(&mut self, handler: Handler) {
        let id = handler.id.clone();
        self.handlers.register_shared(handler, &id);
    }

    /// Store a Patternizer directly in the registry (bypasses dyn Any)
    pub fn store_pattern(&mut self, pattern_id: &str, pattern: Pattern) {
        let key = format!("pattern_{}", pattern_id);
        let id = Id::get(&key);
        let pattern_name: String = pattern.name.clone(); // Clone name before move
        self.registry.insert(id, Entry::Patternizer(pattern));
        println!(
            "📋 Stored pattern '{}' in registry with key '{}'",
            pattern_name, key
        );
    }

    /// Retrieve a Patternizer directly from the registry (bypasses dyn Any)
    pub fn get_pattern(&self, pattern_id: &str) -> Option<&Pattern> {
        let key = format!("pattern_{}", pattern_id);
        match self.get_entry(&key) {
            Some(Entry::Patternizer(pattern)) => {
                let pattern_name: String = pattern.name.clone(); // Clone name before move
                println!(
                    "✅ Found pattern '{}' in registry with key '{}'",
                    pattern_name, key
                );
                Some(pattern)
            }
            _ => {
                println!(
                    "❌ Pattern '{}' not found in registry (key: '{}')",
                    pattern_id, key
                );
                None
            }
        }
    }

    /// Add a handler report to the centralized reporting system
    pub fn add_report(&mut self, report: HandlerReport) {
        // Use nanosecond timestamp for unique report IDs to avoid conflicts
        let timestamp = time();
        let report_id = Id::get(&format!("report_{}", timestamp));
        self.registry
            .insert(report_id, Entry::HandlerReport(report));
    }

    /// Get all handler reports from the registry
    pub fn get_reports(&self) -> Vec<&HandlerReport> {
        let mut reports = Vec::new();
        // Iterate through all registry entries and filter for HandlerReport entries
        for (_id, entry) in &self.registry.entries {
            if let Entry::HandlerReport(report) = entry {
                reports.push(report);
            }
        }
        // Sort by timestamp for chronological order
        reports.sort_by(|a: &&HandlerReport, b: &&HandlerReport| {
            a.report_id.name.cmp(&b.report_id.name)
        });
        reports
    }

    /// Get reports filtered by handler name
    pub fn get_reports_by_handler(&self, handler_name: &str) -> Vec<&HandlerReport> {
        self.get_reports()
            .into_iter()
            .filter(|report: &&HandlerReport| report.handler_name == handler_name)
            .collect()
    }

    /// Get reports filtered by report level
    pub fn get_reports_by_level(&self, level: &ReportLevel) -> Vec<&HandlerReport> {
        self.get_reports()
            .into_iter()
            .filter(|report: &&HandlerReport| &report.level == level)
            .collect()
    }

    /// Get reports filtered by handler phase
    pub fn get_reports_by_phase(&self, phase: &HandlerPhase) -> Vec<&HandlerReport> {
        self.get_reports()
            .into_iter()
            .filter(|report: &&HandlerReport| &report.phase == phase)
            .collect()
    }

    /// Display all reports in a formatted way
    pub fn display_reports(&self) {
        let reports = self.get_reports();
        if reports.is_empty() {
            println!("No handler reports available.");
            return;
        }

        println!("\n=== HANDLER REPORTS ===");
        for report in reports.clone() {
            self.display_single_report(report);
        }
        println!("=== END REPORTS ({} total) ===\n", reports.len());
    }

    /// Display reports filtered by level
    pub fn display_reports_by_level(&self, level: &ReportLevel) {
        let reports = self.get_reports_by_level(level);
        if reports.is_empty() {
            println!("No {} level reports available.", level);
            return;
        }

        println!("\n=== {} LEVEL REPORTS ===", level);
        for report in reports.clone() {
            self.display_single_report(report);
        }
        println!("=== END {} REPORTS ({} total) ===\n", level, reports.len());
    }

    /// Display a single report in formatted way
    fn display_single_report(&self, report: &HandlerReport) {
        println!(
            "[{}] {} | {} | {}",
            report.timestamp(),
            report.level(),
            report.handler_name(),
            report.phase()
        );
        println!("  Message: {}", report.message());
        if report.tokens_processed > 0 || report.tokens_consumed > 0 {
            println!(
                "  Tokens: processed={}, consumed={}",
                report.tokens_processed, report.tokens_consumed
            );
        }
        if !report.metadata.is_empty() {
            print!("  Metadata: ");
            for (i, (key, value)) in report.metadata.iter().enumerate() {
                if i > 0 {
                    print!(", ");
                }
                print!("{}={}", key, value);
            }
            println!();
        }
        println!("  Success: {}\n", report.success);
    }

    /// Clear all reports from the registry
    pub fn clear_reports(&mut self) {
        // Use proper Registry API to remove report entries
        let mut report_keys = Vec::new();
        let registry_len = self.registry.len();
        for i in 0..registry_len {
            let report_id = Id::get(&format!("report_{}", i));
            if let Some(Entry::HandlerReport(_)) = self.registry.root(&report_id) {
                report_keys.push(report_id);
            }
        }

        for key in report_keys {
            self.registry.remove(&key);
        }
    }

    /// Display the entire registry in a database-like format for debugging
    pub fn display_registry_database(&self) {
        println!("🗃️  Context Registry Database View:");
        self.registry.display_database();
    }

    /// Display registry statistics for debugging
    pub fn display_registry_stats(&self) {
        println!("📊 Context Registry Statistics:");
        self.registry.display_stats();
    }

    /// Display all context information including registry and reports
    pub fn display_full_context(&self) {
        let handlers_count = self.handlers.handlers.len();
        let pending_redirects_count = self.pending_redirects.len();
        let registry_entries_count = self.registry.entries.len();
        let reports_count = self.get_reports().len();
        println!("🌐 FULL CONTEXT DISPLAY");
        println!(
            "═══════════════════════════════════════════════════════════════════════════════════════"
        );
        // Display basic context info
        println!("📋 Context Overview:");
        println!("  - Handlers: {} registered", handlers_count);
        println!("  - Pending Redirects: {}", pending_redirects_count);
        println!("  - Registry Entries: {}", registry_entries_count);
        println!("  - Reports: {}", reports_count);
        println!();

        // Display registry database view
        self.display_registry_database();
        println!();

        // Display registry statistics
        self.display_registry_stats();
        println!();

        // Display reports summary
        let reports = self.get_reports();
        if !reports.is_empty() {
            println!("📝 Reports Summary:");
            let mut level_counts: HashMap<&ReportLevel, i32> = std::collections::HashMap::new();
            for report in &reports {
                *level_counts.entry(&report.level()).or_insert(0) += 1;
            }
            for (level, count) in level_counts {
                println!("  - {}: {}", level, count);
            }
            println!();
        }

        println!(
            "═══════════════════════════════════════════════════════════════════════════════════════"
        );
    }
}
impl Default for Context {
    fn default() -> Self {
        Context::new("default")
    }
}
/// Comprehensive handler reporting structure for centralized debugging and statistics
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HandlerReport {
    pub report_id: Box<Id>,
    pub handler_id: Box<Id>,
    pub handler_name: String,
    pub function_name: String,
    pub message: String,
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
            .then_with(|| self.tokens_processed.cmp(&self.tokens_processed))
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
        self.metadata.iter().for_each(|(k, v)| k.hash(state));
        self.metadata.iter().for_each(|(k, v)| v.hash(state));
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
/// Reporting levels for handler messages

/// Handler processing phases
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum HandlerPhase {
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
            self.tokens().len()
        )
    }
}

impl HandlerReport {
    pub fn new(
        report_name: &str,
        handler_id: Box<Id>,
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
            report_id: Box::new(Id::get(report_name)),
            handler_id,
            handler_name,
            function_name,
            message,
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

/// Sets the global verbosity level
pub fn set_verbosity(level: u8) {
    VERBOSITY_LEVEL.store(level, std::sync::atomic::Ordering::SeqCst);
}

/// Gets the current verbosity level
pub fn get_verbosity() -> u8 {
    VERBOSITY_LEVEL.load(std::sync::atomic::Ordering::SeqCst)
}

/// Determines if messages at the given level should be displayed
pub fn should_log(level: u8) -> bool {
    get_verbosity() >= level
}

/// Macro for logging messages with verbosity control
#[macro_export]
macro_rules! log {
    // Error messages (level 1)
    (error, $($arg:tt)*) => {
        if $crate::config::should_log($crate::config::VERBOSITY_ERROR) {
            eprintln!("[ERROR] {}", format!($($arg)*));
        }
    };
    (warn, $($arg:tt)*) => {
        if $crate::config::should_log($crate::config::VERBOSITY_WARN) {
            eprintln!("[WARN] {}", format!($($arg)*));
        }
    };
    // Debug messages (level 2)
    (debug, $($arg:tt)*) => {
        if $crate::config::should_log($crate::config::VERBOSITY_DEBUG) {
            println!("[DEBUG] {}", format!($($arg)*));
        }
    };

    // Info messages (level 3)
    (info, $($arg:tt)*) => {
        if $crate::config::should_log($crate::config::VERBOSITY_INFO) {
            println!("[INFO] {}", format!($($arg)*));
        }
    };

    // Default to debug level
    ($($arg:tt)*) => {
        $crate::debug!($($arg)*);
    };
}
