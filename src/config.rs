#![allow(
    unused_variables,
    unused_assignments,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals
)]

use crate::lock::Id;
use crate::{info, Handler, HandlerMap, Tokenizer};
use crate::pattern::Pattern;
use crate::pattern::Patternizer;
use std::any::{Any, TypeId};
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt::Debug;
use std::fs::copy;
use std::hash::{Hash, Hasher};
use std::mem::transmute;
use std::ops::{Deref, DerefMut};
use std::path::{Path, PathBuf};
use std::result;
use std::sync::atomic::AtomicU8;
use std::sync::{LazyLock, OnceLock, RwLock};
use std::time::{SystemTime, UNIX_EPOCH};

/// Convenient macro for creating and adding handler reports to the centralized system
///
/// Usage examples:
/// ```
//// report!(context, "function_handler", Info, Processing, "Processing function declaration", true);
//// report!(context, "array_handler", Warning, Conversion, "Array size inference failed", false,
////        ("tokens_processed", 5), ("fallback_used", true));
/// ```
#[macro_export]
macro_rules! report {
    // Basic report with just message and success
    ($handler_name:expr, $function_name:expr, $level:expr, $phase:expr, $message:expr, $success:expr) => {
        {
            let report = $crate::config::HandlerReport {
                report_id: Box::new($crate::Id::get(&$crate::Id::gen_name(&format!("report_{}_{}", $handler_name, $function_name)))),
                handler_id: Box::new($crate::Id::get($handler_name)),
                handler_name: $handler_name.to_string(),
                function_name: $function_name.to_string(),
                message: $message.to_string(),
                level: $level,
                tokens_processed: 0,
                tokens_consumed: 0,
                phase: $phase,
                success: $success,
                metadata: std::collections::HashMap::new(),
            };
            $crate::config::Global::context_fn(|ctx| ctx.add_report(report));
        }
    };

    // Report with token counts
    ($handler_name:expr, $function_name:expr, $level:expr, $phase:expr, $message:expr, $success:expr,
     $tokens_processed:expr, $tokens_consumed:expr) => {
        {
            let report = $crate::config::HandlerReport {
                report_id: Box::new($crate::Id::get(&$crate::Id::gen_name(&format!("report_{}_{}", $handler_name, $function_name)))),
                handler_id: Box::new($crate::Id::get($handler_name)),
                handler_name: $handler_name.to_string(),
                function_name: $function_name.to_string(),
                message: $message.to_string(),
                level: $level,
                tokens_processed: $tokens_processed,
                tokens_consumed: $tokens_consumed,
                phase: $phase,
                success: $success,
                metadata: std::collections::HashMap::new(),
            };
             $crate::config::Global::context_fn(|ctx| ctx.add_report(report));
        }
    };

    // Report with metadata key-value pairs
    ($handler_name:expr, $function_name:expr, $level:expr, $phase:expr, $message:expr, $success:expr,
     $(($key:expr, $value:expr)),+) => {
        {
            let mut metadata = std::collections::HashMap::new();
            $(
                metadata.insert($key.to_string(), $value.to_string());
            )+
            let report = $crate::config::HandlerReport {
                report_id: Box::new($crate::Id::get(&$crate::Id::gen_name(&format!("report_{}_{}", $handler_name, $function_name)))),
                handler_id: Box::new($crate::Id::get($handler_name)),
                handler_name: $handler_name.to_string(),
                function_name: $function_name.to_string(),
                message: $message.to_string(),
                level: $level,
                tokens_processed: 0,
                tokens_consumed: 0,
                phase: $phase,
                success: $success,
                metadata,
            };
            $crate::config::Global::context_fn(|ctx| ctx.add_report(report));
        }
    };

    // Full report with token counts and metadata
    ($handler_name:expr, $function_name:expr, $level:expr, $phase:expr, $message:expr, $success:expr,
     $tokens_processed:expr, $tokens_consumed:expr, $(($key:expr, $value:expr)),+) => {
        {
            let mut metadata = std::collections::HashMap::new();
            $(
                metadata.insert($key.to_string(), $value.to_string());
            )+
            let report = $crate::config::HandlerReport {
                report_id: Box::new($crate::Id::get(&$crate::Id::gen_name(&format!("report_{}_{}", $handler_name, $function_name)))),
                handler_id: Box::new($crate::Id::get($handler_name)),
                handler_name: $handler_name.to_string(),
                function_name: $function_name.to_string(),
                message: $message.to_string(),
                level: $level,
                tokens_processed: $tokens_processed,
                tokens_consumed: $tokens_consumed,
                phase: $phase,
                success: $success,
                metadata,
            };
             $crate::config::Global::context_fn(|ctx| ctx.add_report(report));
        }
    };
}

// Global verbosity level
// 0: No output
// 1: Errors only
// 2: Errors + Debug messages
// 3: All messages (Errors + Debug + Info)
pub static VERBOSITY_LEVEL: AtomicU8 = AtomicU8::new(1); // Default to errors only

// Verbosity level constants
pub const VERBOSITY_NONE: u8 = 0;
pub const VERBOSITY_ERROR: u8 = 1;

pub const VERBOSITY_WARN: u8 = 2;
pub const VERBOSITY_DEBUG: u8 = 3;
pub const VERBOSITY_INFO: u8 = 4;


static GLOBAL_CONTEXT: RwLock<Option<Context>> = RwLock::new(None);
pub struct Global;
impl Global {
    pub const fn new() -> Global {
        Self
    }
    pub fn context() -> Context {
        Self::context_fn(|ctx| ctx.clone())
    }
    pub fn context_fn<F, R>(f: F) -> R
    where
        F: FnOnce(&mut Context) -> R,
    {
        let mut guard = GLOBAL_CONTEXT.write().unwrap();
        f(guard.get_or_insert_default())
    }

    pub fn update<F>(f: F)
    where
        F: FnOnce(&mut Context),
    {
        Self::context_fn(f);
    }
    pub fn registry() -> Registry {
        Self::context_fn(|ctx| ctx.registry.clone())
    }

    pub fn handler_map() -> HandlerMap {
        Self::context_fn(|ctx| ctx.handlers.clone())
    }

    pub fn tokenizer() -> Tokenizer {
        Self::context_fn(|ctx| ctx.tokenizer.clone())
    }
    pub fn patternizer() -> Patternizer {
        Self::context_fn(|ctx| ctx.patternizer.clone())
    }

    pub fn pending() -> Vec<HandlerRedirect> {
        Self::context_fn(|ctx| ctx.pending_redirects.clone())
    }
}
#[macro_export]
macro_rules! context {
    () => {
        $crate::config::Global::context()
    };
    ($cmd:literal) => {
        $crate::config::Global::context_fn(|ctx| ctx.$cmd)
    };
    ($context:expr) => {
        $crate::config::Global::context_fn(|ctx| *ctx = $context)
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
    pub tokenizer: Tokenizer,
    pub patternizer: Patternizer,
    pub registry: Registry,
    pub pending_redirects: Vec<HandlerRedirect>,
}
impl Clone for Context {
    fn clone(&self) -> Self {
        Self {
            handlers: self.handlers.clone(),
            tokenizer: self.tokenizer.clone(),
            patternizer: self.patternizer.clone(),
            registry: self.registry.clone(),
            pending_redirects: self.pending_redirects.clone(),
        }
    }
}
impl Eq for Context {}
impl PartialEq for Context {
    fn eq(&self, other: &Self) -> bool {
        self.registry == other.registry &&
            self.handlers == other.handlers &&
            self.tokenizer == other.tokenizer &&
            self.patternizer == other.patternizer &&
            self.pending_redirects == other.pending_redirects
    }
}


impl Context {
    /// Create a new configuration with the given name
    pub fn new(name: &str) -> Self {
        Context {
            handlers: HandlerMap::default(),
            tokenizer: Tokenizer::default(),
            patternizer: Patternizer::default(),
            registry: Registry::default(),
            pending_redirects: Vec::new(),
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
    pub fn apply(&self) {
        let verbosity = self.get_verbosity();
        info!("Applying settings from registry");
        VERBOSITY_LEVEL.store(verbosity, std::sync::atomic::Ordering::Relaxed);
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
        println!("🔍 Getting entry '{}' with ID: {:?}", name, id);
        println!("🔍 Registry has {} entries", self.registry.entries.len());
        
        if name.starts_with("pattern_") {
            println!("🔍 Registry contents for pattern lookup:");
            for (entry_id, entry) in self.registry.entries.iter() {
                println!("  - ID: {:?} -> {:?}", entry_id, match entry {
                    Entry::Patternizer(p) => format!("Patternizer({})", p.name),
                    _ => "Other".to_string(),
                });
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
        println!("🔧 Setting entry '{}' with ID: {:?}", name, id);
        println!("🔧 Registry before insert has {} entries", self.registry.entries.len());
        self.registry.entries.insert(id.clone(), entry);
        println!("🔧 Registry after insert has {} entries", self.registry.entries.len());
        
        // Verify the entry was actually stored
        if self.registry.entries.contains_key(&id) {
            println!("✅ Verified entry '{}' was stored successfully", name);
        } else {
            println!("❌ Entry '{}' was NOT stored!", name);
        }
    }
    pub fn id(&self,name: &str) -> Option<Id> {
        let mut entry = self.registry.entries.get(&Id::get(name));
        if let Some(Entry::Id(value)) = entry {
            return Some(value.clone())
        } else {
            None
        }
    }
    pub fn pattern(&self,name: &str) -> Option<Pattern> {
        let mut entry = self.registry.entries.get(&Id::get(name));
        if let Some(Entry::Patternizer(value)) = entry {
            return Some(value.clone())
        } else {
            None
        }
    }

    /// Initialize common patterns in the patternizer (proper Context method)
    pub fn initialize_common_patterns(&mut self) {
        self.patternizer.initialize_common_patterns();
    }

    /// Match patterns for a handler using the patternizer (proper Context method)
    pub fn match_pattern(&mut self, handler_type: &str, tokens: &[crate::Token]) -> crate::pattern::PatternResult {
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
        self.handlers.register(handler);
    }

    /// Store a Patternizer directly in the registry (bypasses dyn Any)
    pub fn store_pattern(&mut self, pattern_id: &str, pattern: Pattern) {
        let key = format!("pattern_{}", pattern_id);
        let id = Id::get(&key);
        let pattern_name = pattern.name.clone(); // Clone name before move
        self.registry.insert(id, Entry::Patternizer(pattern));
        println!("📋 Stored pattern '{}' in registry with key '{}'", pattern_name, key);
    }

    /// Retrieve a Patternizer directly from the registry (bypasses dyn Any)
    pub fn get_pattern(&self, pattern_id: &str) -> Option<&Pattern> {
        let key = format!("pattern_{}", pattern_id);
        match self.get_entry(&key) {
            Some(Entry::Patternizer(pattern)) => {
                println!("✅ Found pattern '{}' in registry with key '{}'", pattern.name, key);
                Some(pattern)
            },
            _ => {
                println!("❌ Pattern '{}' not found in registry (key: '{}')", pattern_id, key);
                None
            }
        }
    }

    /// Add a handler report to the centralized reporting system
    pub fn add_report(&mut self, report: HandlerReport) {
        // Use nanosecond timestamp for unique report IDs to avoid conflicts
        let timestamp = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos();
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
        reports.sort_by(|a, b| a.report_id.name.cmp(&b.report_id.name));
        reports
    }

    /// Get reports filtered by handler name
    pub fn get_reports_by_handler(&self, handler_name: &str) -> Vec<&HandlerReport> {
        self.get_reports()
            .into_iter()
            .filter(|report| report.handler_name == handler_name)
            .collect()
    }

    /// Get reports filtered by report level
    pub fn get_reports_by_level(&self, level: &ReportLevel) -> Vec<&HandlerReport> {
        self.get_reports()
            .into_iter()
            .filter(|report| &report.level == level)
            .collect()
    }

    /// Get reports filtered by handler phase
    pub fn get_reports_by_phase(&self, phase: &HandlerPhase) -> Vec<&HandlerReport> {
        self.get_reports()
            .into_iter()
            .filter(|report| &report.phase == phase)
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
            println!("No {:?} level reports available.", level);
            return;
        }

        println!("\n=== {:?} LEVEL REPORTS ===", level);
        for report in reports.clone() {
            self.display_single_report(report);
        }
        println!(
            "=== END {:?} REPORTS ({} total) ===\n",
            level,
            reports.len()
        );
    }

    /// Display a single report in formatted way
    fn display_single_report(&self, report: &HandlerReport) {
        println!(
            "[{}] {:?} | {} | {:?}",
            report.timestamp(),
            report.level,
            report.handler_name,
            report.phase
        );
        println!("  Message: {}", report.message);
        if report.tokens_processed > 0 || report.tokens_consumed > 0 {
            println!(
                "  Tokens: processed={}, consumed={}",
                report.tokens_processed, report.tokens_consumed
            );
        }
        if !report.metadata.is_empty() {
            println!("  Metadata: {:?}", report.metadata);
        }
        println!("  Success: {}\n", report.success);
    }

    /// Clear all reports from the registry
    pub fn clear_reports(&mut self) {
        // Use proper Registry API to remove report entries
        let mut report_keys = Vec::new();
        for i in 0..self.registry.len() {
            let report_id = Id::get(&format!("report_{}", i));
            if let Some(Entry::HandlerReport(_)) = self.registry.get_root_entry(&report_id) {
                report_keys.push(report_id);
            }
        }

        for key in report_keys {
            self.registry.remove(&key);
        }
    }
}
impl Default for Context {
    fn default() -> Self {
        Context {
            handlers: HandlerMap::default(),
            tokenizer: Tokenizer::default(),
            patternizer: Patternizer::default(),
            registry: Registry::default(),
            pending_redirects: Vec::new(),
        }
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
    pub fn contains(&self) -> bool {
        todo!()
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
/// Reporting levels for handler messages
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ReportLevel {
    Debug,
    Info,
    Warning,
    Error,
}

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

/// Pending redirect structure for deferred handler processing
/// This allows redirect functions to queue up handler calls without direct function access
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HandlerRedirect {
    pub source_handler: String,
    pub target_handler: String,
    pub directive_type: String,
    pub tokens: Vec<crate::Token>,
    pub original_result: String, // Serialized HandlerResult for later processing
    timestamp: u128,
}

impl HandlerRedirect {
    pub fn new(
        source_handler: String,
        target_handler: String,
        directive_type: String,
        tokens: Vec<crate::Token>,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Registry {
    pub id: Id,
    pub entries: HashMap<Id, Entry>,
    pub entry_count: u64,
}
impl Registry {
    pub fn new(name: &str) -> Self {
        Registry {
            id: Id::get("registry"),
            entries: HashMap::new(),
            entry_count: 0,
        }
    }

    /// Set an entry in the registry by name
    pub fn set_entry(&mut self, name: &str, entry: Entry) {
        let id = Id::get(name);
        self.entries.insert(id, entry);
        self.entry_count += 1;
    }

    /// Get a value from the registry with type casting
    pub fn get_value<T: Any + Clone + Debug>(&self, name: &str) -> Box<T> {
        let id = Id::get(name);
        if let Some(entry) = self.entries.get(&id) {
            let entry_type: Box<dyn Any> = match entry {
                Entry::Id(id) => Box::new(id.clone()),
                Entry::Val(val) => Box::new(val.clone()),
                Entry::Str(string) => Box::new(string.clone()),
                Entry::Bool(boolean) => Box::new(boolean.clone()),
                Entry::Func(func) => Box::new(*func),
                Entry::Path(path) => Box::new(path.clone()),
                Entry::Handler(handler) => Box::new(handler.clone()),
                Entry::HandlerReport(report) => Box::new(report.clone()),
                Entry::Pair(pair) => Box::new(pair.clone()),
                Entry::List(list) => Box::new(list.clone()),
                Entry::Any(any) => any.clone(),
                Entry::IdMap(map) => Box::new(map.clone()),
                Entry::ValMap(map) => Box::new(map.clone()),
                Entry::StrMap(map) => Box::new(map.clone()),
                Entry::BoolMap(map) => Box::new(map.clone()),
                Entry::FuncMap(map) => Box::new(map.clone()),
                Entry::PathMap(map) => Box::new(map.clone()),
                Entry::HandlerMap(map) => Box::new(map.clone()),
                Entry::PairMap(map) => Box::new(map.clone()),
                Entry::AnyMap(map) => Box::new(map.clone()),
                Entry::Patternizer(pattern) => Box::new(pattern.clone()),
            };
            entry_type.downcast::<T>().unwrap_or_else(|_| panic!("Failed to downcast entry for name: {}", name))
        } else {
            panic!("Entry not found for name: {}", name)
        }
    }

    /// Set a value in the registry with type conversion
    pub fn set_value<T: Any + Clone + Debug>(&mut self, name: &str, value: T) {
        let id = Id::get(name);
        let entry = self.from_value(value);
        self.entries.insert(id, entry);
        self.entry_count += 1;
    }

    /// Convert a value to an Entry
    fn from_value<T: Any + Clone + Debug>(&self, value: T) -> Entry {
        let any_value: &dyn Any = &value;
        let type_id = any_value.type_id();

        if type_id == TypeId::of::<Id>() {
            Entry::Id(any_value.downcast_ref::<Id>().unwrap().clone())
        } else if type_id == TypeId::of::<u64>() {
            Entry::Val(any_value.downcast_ref::<u64>().unwrap().clone())
        } else if type_id == TypeId::of::<String>() {
            Entry::Str(any_value.downcast_ref::<String>().unwrap().clone())
        } else if type_id == TypeId::of::<bool>() {
            Entry::Bool(any_value.downcast_ref::<bool>().unwrap().clone())
        } else if type_id == TypeId::of::<PathBuf>() {
            Entry::Path(any_value.downcast_ref::<PathBuf>().unwrap().clone())
        } else if type_id == TypeId::of::<Handler>() {
            Entry::Handler(any_value.downcast_ref::<Handler>().unwrap().clone())
        } else if type_id == TypeId::of::<HandlerReport>() {
            Entry::HandlerReport(any_value.downcast_ref::<HandlerReport>().unwrap().clone())
        } else if type_id == TypeId::of::<Vec<String>>() {
            let vec = any_value.downcast_ref::<Vec<String>>().unwrap();
            let entries: Vec<Entry> = vec.iter().map(|s| Entry::Str(s.clone())).collect();
            Entry::List(entries)
        } else if type_id == TypeId::of::<HashMap<String, String>>() {
            let map = any_value.downcast_ref::<HashMap<String, String>>().unwrap();
            let entry_map: HashMap<String, Entry> = map.iter()
                .map(|(k, v)| (k.clone(), Entry::Str(v.clone())))
                .collect();
            Entry::StrMap(entry_map)
        } else if type_id == TypeId::of::<Pattern>() {
            Entry::Patternizer(any_value.downcast_ref::<Pattern>().unwrap().clone())
        } else {
            panic!("Unsupported type for Entry conversion: {:?}", type_id)
        }
    }

    /// Store a Patternizer directly in the registry (bypasses dyn Any)
    pub fn store_pattern(&mut self, pattern_id: &str, pattern: Pattern) {
        let key = format!("pattern_{}", pattern_id);
        let id = Id::get(&key);
        self.entries.insert(id, Entry::Patternizer(pattern.clone()));
        self.entry_count += 1;
        println!("📋 Stored pattern '{}' in registry with key '{}'", pattern.name, key);
    }

    /// Retrieve a Patternizer directly from the registry (bypasses dyn Any)
    pub fn get_pattern(&self, pattern_id: &str) -> Option<Pattern> {
        let key = format!("pattern_{}", pattern_id);
        match self.entries.get(&Id::get(&key)) {
            Some(Entry::Patternizer(pattern)) => {
                println!("✅ Found pattern '{}' in registry with key '{}'", pattern.name, key);
                Some(pattern.clone())
            },
            _ => {
                println!("❌ Pattern '{}' not found in registry (key: '{}')", pattern_id, key);
                None
            }
        }
    }

    /// Get all entries in the registry
    pub fn get_all_entries(&self) -> &HashMap<Id, Entry> {
        &self.entries
    }
    pub fn insert(&mut self, id: Id, entry: Entry) -> Option<Entry> {
        let result = self.entries.insert(id, entry);
        if result.is_none() {
            self.entry_count += 1;
        }
        result
    }

    pub fn remove(&mut self, id: &Id) -> Option<Entry> {
        let result = self.entries.remove(id);
        if result.is_some() {
            self.entry_count -= 1;
        }
        result
    }

    pub fn clear(&mut self) {
        self.entries.clear();
        self.entry_count = 0;
    }
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty() && self.entry_count == 0 && self.id.is_empty()
    }
    pub fn len(&self) -> u64 {
        self.entry_count
    }

    pub fn get_root_entry(&self, id: &Id) -> Option<&Entry> {
        self.entries.get(id)
    }

    pub fn get_root_entry_mut(&mut self, id: &Id) -> Option<&mut Entry> {
        self.entries.get_mut(id)
    }
    pub fn get_entry(&self, name: &str, depth: u64) -> Option<Entry> {
        let parts: Vec<&str> = name.split('/').collect();
        if parts.is_empty() {
            return None;
        }

        let root_key = parts[0];
        let root_id = Id::get(root_key);
        
        // Debug: Print registry state for pattern lookups
        if name.starts_with("pattern_") {
            println!("🔍 Registry lookup for '{}', root_id: {:?}", name, root_id);
            println!("🔍 Registry has {} entries:", self.entries.len());
            for (id, entry) in self.entries.iter() {
                println!("  - ID: {:?} -> {:?}", id, match entry {
                    Entry::Patternizer(p) => format!("Patternizer({})", p.name),
                    _ => "Other".to_string(),
                });
            }
        }
        
        let entry = self.entries.get(&root_id)?;

        // If we're at depth 0 or have no more path parts, return the current entry
        if depth == 0 || parts.len() == 1 {
            return Some(entry.clone());
        }

        let remaining_path = &parts[1..];
        self.traverse_entry(entry, remaining_path, depth - 1)
    }

    fn traverse_entry(&self, entry: &Entry, path: &[&str], remaining_depth: u64) -> Option<Entry> {
        if path.is_empty() {
            return Some(entry.clone());
        }

        let current_key = path[0];
        let next_path = &path[1..];

        match entry {
            Entry::StrMap(map) => {
                if let Some(nested_entry) = map.get(current_key) {
                    if next_path.is_empty() || remaining_depth == 0 {
                        Some(nested_entry.clone())
                    } else {
                        self.traverse_entry(nested_entry, next_path, remaining_depth - 1)
                    }
                } else {
                    None
                }
            },
            Entry::List(entries) => {
                if let Ok(index) = current_key.parse::<usize>() {
                    if index < entries.len() {
                        let nested_entry = &entries[index];
                        if next_path.is_empty() || remaining_depth == 0 {
                            Some(nested_entry.clone())
                        } else {
                            self.traverse_entry(nested_entry, next_path, remaining_depth - 1)
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            },
            Entry::Pair(pair) => {
                match current_key {
                    "left" => {
                        if next_path.is_empty() || remaining_depth == 0 {
                            Some(pair.0.clone())
                        } else {
                            self.traverse_entry(&pair.0, next_path, remaining_depth - 1)
                        }
                    },
                    "right" => {
                        if next_path.is_empty() || remaining_depth == 0 {
                            Some(pair.1.clone())
                        } else {
                            self.traverse_entry(&pair.1, next_path, remaining_depth - 1)
                        }
                    },
                    _ => None
                }
            },
            Entry::IdMap(map) => {
                let id = Id::get(current_key);
                if let Some(nested_entry) = map.get(&id) {
                    if next_path.is_empty() || remaining_depth == 0 {
                        Some(nested_entry.clone())
                    } else {
                        self.traverse_entry(nested_entry, next_path, remaining_depth - 1)
                    }
                } else {
                    None
                }
            },
            Entry::ValMap(map) => {
                if let Ok(val) = current_key.parse::<u64>() {
                    if let Some(nested_entry) = map.get(&val) {
                        if next_path.is_empty() || remaining_depth == 0 {
                            Some(nested_entry.clone())
                        } else {
                            self.traverse_entry(nested_entry, next_path, remaining_depth - 1)
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            },
            Entry::BoolMap(map) => {
                if let Ok(bool_val) = current_key.parse::<bool>() {
                    if let Some(nested_entry) = map.get(&bool_val) {
                        if next_path.is_empty() || remaining_depth == 0 {
                            Some(nested_entry.clone())
                        } else {
                            self.traverse_entry(nested_entry, next_path, remaining_depth - 1)
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            },
            Entry::PathMap(map) => {
                let path_key = std::path::PathBuf::from(current_key);
                if let Some(nested_entry) = map.get(&path_key) {
                    if next_path.is_empty() || remaining_depth == 0 {
                        Some(nested_entry.clone())
                    } else {
                        self.traverse_entry(nested_entry, next_path, remaining_depth - 1)
                    }
                } else {
                    None
                }
            },
            Entry::HandlerMap(map) => {
                if let Some(handler) = map.get(current_key) {
                    if next_path.is_empty() || remaining_depth == 0 {
                        Some(Entry::Handler(handler.clone()))
                    } else {
                        None // Handlers are leaf nodes
                    }
                } else {
                    None
                }
            },
            Entry::PairMap(map) => {
                if let Some(pair) = map.get(current_key) {
                    if next_path.is_empty() || remaining_depth == 0 {
                        Some(Entry::Pair(pair.clone()))
                    } else {
                        // Try to traverse into the pair
                        if next_path.len() > 0 {
                            match next_path[0] {
                                "left" => self.traverse_entry(&pair.0, &next_path[1..], remaining_depth - 1),
                                "right" => self.traverse_entry(&pair.1, &next_path[1..], remaining_depth - 1),
                                _ => None
                            }
                        } else {
                            None
                        }
                    }
                } else {
                    None
                }
            },
            Entry::AnyMap(map) => {
                // Find first matching key
                for (k, v) in map {
                    if let Entry::Str(s) = k {
                        if s == current_key {
                            if next_path.is_empty() || remaining_depth == 0 {
                                return Some(v.clone());
                            } else {
                                return self.traverse_entry(v, next_path, remaining_depth - 1);
                            }
                        }
                    }
                }
                None
            },
            Entry::Any(any) => {
                if remaining_depth > 0 {
                    self.traverse_entry(any, path, remaining_depth - 1)
                } else {
                    None
                }
            },
            // Leaf nodes - return None for further traversal
            Entry::Id(_) | Entry::Val(_) | Entry::Str(_) | Entry::Bool(_) |
            Entry::Func(_) | Entry::Path(_) | Entry::Handler(_) |
            Entry::HandlerReport(_) | Entry::Patternizer(_) | Entry::FuncMap(_) => None,
        }
    }
    pub fn get_str_map(&self, name: &str, depth: u64) -> Option<HashMap<String, Entry>> {
        match self.get_entry(name, depth)? {
            Entry::StrMap(map) => Some(map),
            _ => None,
        }
    }

    pub fn get_str(&self, name: &str, depth: u64) -> Option<String> {
        match self.get_entry(name, depth)? {
            Entry::Str(s) => Some(s),
            _ => None,
        }
    }

    pub fn get_bool(&self, name: &str, depth: u64) -> Option<bool> {
        match self.get_entry(name, depth)? {
            Entry::Bool(b) => Some(b),
            _ => None,
        }
    }

    pub fn get_val(&self, name: &str, depth: u64) -> Option<u64> {
        match self.get_entry(name, depth)? {
            Entry::Val(v) => Some(v),
            _ => None,
        }
    }

    pub fn get_path(&self, name: &str, depth: u64) -> Option<PathBuf> {
        match self.get_entry(name, depth)? {
            Entry::Path(p) => Some(p),
            _ => None,
        }
    }

    pub fn get_list(&self, name: &str, depth: u64) -> Option<Vec<Entry>> {
        match self.get_entry(name, depth)? {
            Entry::List(list) => Some(list),
            _ => None,
        }
    }
}
impl Default for Registry {
    fn default() -> Self {
        Registry::new("default")
    }
}
#[derive(Debug, Clone)]
pub enum Entry {
    Id(Id),
    Val(u64),
    Str(String),
    Bool(bool),
    Func(fn(Entry) -> Entry),
    Path(PathBuf),
    Handler(Handler),
    HandlerReport(HandlerReport),
    Pair(Box<(Entry, Entry)>),
    List(Vec<Entry>),
    Any(Box<Entry>),
    IdMap(HashMap<Id, Entry>),
    ValMap(HashMap<u64, Entry>),
    StrMap(HashMap<String, Entry>),
    BoolMap(HashMap<bool, Entry>),
    FuncMap(HashMap<fn(Entry) -> Entry, Entry>),
    PathMap(HashMap<PathBuf, Entry>),
    HandlerMap(HashMap<String, Handler>),
    PairMap(HashMap<String, Box<(Entry, Entry)>>),
    AnyMap(HashMap<Entry, Entry>),
    Patternizer(Pattern),
}
impl Default for Entry {
    fn default() -> Self {
        Entry::Id(Id::default())
    }
}
impl Hash for Entry {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Entry::Id(id) => id.hash(state),
            Entry::Val(v) => v.hash(state),
            Entry::Str(s) => s.hash(state),
            Entry::Bool(b) => b.hash(state),
            Entry::Func(f) => f.hash(state),
            Entry::Path(p) => p.hash(state),
            Entry::Handler(h) => h.hash(state),
            Entry::HandlerReport(h) => h.hash(state),
            Entry::Pair(p) => p.hash(state),
            Entry::List(l) => l.hash(state),
            Entry::Any(a) => a.hash(state),
            Entry::IdMap(m) => m.iter().for_each(|(_k, v)| v.hash(state)),
            Entry::ValMap(m) => m.iter().for_each(|(_k, v)| v.hash(state)),
            Entry::StrMap(m) => m.iter().for_each(|(_k, v)| v.hash(state)),
            Entry::BoolMap(m) => m.iter().for_each(|(_k, v)| v.hash(state)),
            Entry::FuncMap(m) => m.iter().for_each(|(_k, v)| v.hash(state)),
            Entry::PathMap(m) => m.iter().for_each(|(_k, v)| v.hash(state)),
            Entry::HandlerMap(m) => m.iter().for_each(|(_k, v)| v.hash(state)),
            Entry::PairMap(m) => m.iter().for_each(|(_k, v)| v.hash(state)),
            Entry::AnyMap(m) => m.iter().for_each(|(_k, v)| v.hash(state)),
            Entry::Patternizer(p) => p.id.hash(state),
        }
    }
}
impl Entry {
    pub fn none() -> Self {
        Entry::Id(Id::get("none"))
    }
    pub fn get(id: Id) -> Entry {
        Entry::Id(id)
    }
    pub fn val(v: u64) -> Entry {
        Entry::Val(v)
    }
    pub fn str(s: &str) -> Entry {
        Entry::Str(s.to_string())
    }

    /// Create a HandlerReport entry
    pub fn handler_report(report: HandlerReport) -> Entry {
        Entry::HandlerReport(report)
    }
    pub fn bool(b: bool) -> Entry {
        Entry::Bool(b)
    }
    pub fn func(f: fn(Entry) -> Entry) -> Entry {
        Entry::Func(f)
    }
    pub fn path(p: &Path) -> Entry {
        Entry::Path(p.to_path_buf())
    }
    pub fn handler(h: Handler) -> Entry {
        Entry::Handler(h)
    }
    pub fn pair(left: Entry, right: Entry) -> Entry {
        Entry::Pair(Box::new((left, right)))
    }
    pub fn list(entries: Vec<Entry>) -> Entry {
        Entry::List(entries)
    }
    pub fn id_map(entries: HashMap<Id, Entry>) -> Entry {
        Entry::IdMap(entries)
    }
    pub fn val_map(entries: HashMap<u64, Entry>) -> Entry {
        Entry::ValMap(entries)
    }
    pub fn str_map(entries: HashMap<String, Entry>) -> Entry {
        Entry::StrMap(entries)
    }
    pub fn bool_map(entries: HashMap<bool, Entry>) -> Entry {
        Entry::BoolMap(entries)
    }
    pub fn func_map(entries: HashMap<fn(Entry) -> Entry, Entry>) -> Entry {
        Entry::FuncMap(entries)
    }
    pub fn path_map(entries: HashMap<PathBuf, Entry>) -> Entry {
        Entry::PathMap(entries)
    }
    pub fn handler_map(map: HashMap<String, Handler>) -> Entry {
        Entry::HandlerMap(map)
    }
    pub fn pair_map(entries: HashMap<String, Box<(Entry, Entry)>>) -> Entry {
        Entry::PairMap(entries)
    }
    pub fn pattern(entry: Pattern) -> Entry {
        Entry::Patternizer(entry)
    }
    pub fn any(entry: Entry) -> Entry {
        Entry::Any(Box::new(entry))
    }
}
impl PartialEq for Entry {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Entry::Id(left), Entry::Id(right)) => left == right,
            (Entry::Val(left), Entry::Val(right)) => left == right,
            (Entry::Str(left), Entry::Str(right)) => left == right,
            (Entry::Bool(left), Entry::Bool(right)) => left == right,
            (Entry::Func(left), Entry::Func(right)) => std::ptr::fn_addr_eq(*left, *right),
            (Entry::Path(left), Entry::Path(right)) => left == right,
            (Entry::Handler(left), Entry::Handler(right)) => left == right,
            (Entry::HandlerReport(left), Entry::HandlerReport(right)) => left == right,
            (Entry::Pair(left), Entry::Pair(right)) => left == right,
            (Entry::List(left), Entry::List(right)) => left == right,
            (Entry::IdMap(left), Entry::IdMap(right)) => left == right,
            (Entry::StrMap(left), Entry::StrMap(right)) => left == right,
            (Entry::BoolMap(left), Entry::BoolMap(right)) => left == right,
            (Entry::FuncMap(left), Entry::FuncMap(right)) => left == right,
            (Entry::PathMap(left), Entry::PathMap(right)) => left == right,
            (Entry::HandlerMap(left), Entry::HandlerMap(right)) => left == right,
            (Entry::PairMap(left), Entry::PairMap(right)) => left == right,
            (Entry::Any(left), Entry::Any(right)) => left == right,
            _ => false,
        }
    }
}
impl Eq for Entry {}

impl Registry {

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
