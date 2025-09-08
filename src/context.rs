use std::any::Any;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU8, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};
use crate::{get_id, info, Handler, HandlerMap, Id, Tokenizer};

/// Convenient macro for creating and adding handler reports to the centralized system
/// 
/// Usage examples:
/// ```
/// report!(context, "function_handler", Info, Processing, "Processing function declaration", true);
/// report!(context, "array_handler", Warning, Conversion, "Array size inference failed", false, 
///         ("tokens_processed", 5), ("fallback_used", true));
/// ```
#[macro_export]
macro_rules! report {
    // Basic report with just message and success
    ($context:expr, $handler_name:expr, $level:expr, $phase:expr, $message:expr, $success:expr) => {
        {
            let report = $crate::context::HandlerReport {
                handler_id: $crate::get_id($handler_name),
                handler_name: $handler_name.to_string(),
                timestamp: std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap_or_default()
                    .as_secs(),
                message: $message.to_string(),
                level: $level,
                tokens_processed: 0,
                tokens_consumed: 0,
                phase: $phase,
                success: $success,
                metadata: std::collections::HashMap::new(),
            };
            $context.add_report(report);
        }
    };
    
    // Report with token counts
    ($context:expr, $handler_name:expr, $level:expr, $phase:expr, $message:expr, $success:expr, 
     $tokens_processed:expr, $tokens_consumed:expr) => {
        {
            let report = $crate::context::HandlerReport {
                handler_id: $crate::get_id($handler_name),
                handler_name: $handler_name.to_string(),
                timestamp: std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap_or_default()
                    .as_secs(),
                message: $message.to_string(),
                level: $level,
                tokens_processed: $tokens_processed,
                tokens_consumed: $tokens_consumed,
                phase: $phase,
                success: $success,
                metadata: std::collections::HashMap::new(),
            };
            $context.add_report(report);
        }
    };
    
    // Report with metadata key-value pairs
    ($context:expr, $handler_name:expr, $level:expr, $phase:expr, $message:expr, $success:expr, 
     $(($key:expr, $value:expr)),+) => {
        {
            let mut metadata = std::collections::HashMap::new();
            $(
                metadata.insert($key.to_string(), $value.to_string());
            )+
            let report = $crate::context::HandlerReport {
                handler_id: $crate::get_id($handler_name),
                handler_name: $handler_name.to_string(),
                timestamp: std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap_or_default()
                    .as_secs(),
                message: $message.to_string(),
                level: $level,
                tokens_processed: 0,
                tokens_consumed: 0,
                phase: $phase,
                success: $success,
                metadata,
            };
            $context.add_report(report);
        }
    };
    
    // Full report with token counts and metadata
    ($context:expr, $handler_name:expr, $level:expr, $phase:expr, $message:expr, $success:expr, 
     $tokens_processed:expr, $tokens_consumed:expr, $(($key:expr, $value:expr)),+) => {
        {
            let mut metadata = std::collections::HashMap::new();
            $(
                metadata.insert($key.to_string(), $value.to_string());
            )+
            let report = $crate::context::HandlerReport {
                handler_id: $crate::get_id($handler_name),
                handler_name: $handler_name.to_string(),
                timestamp: std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap_or_default()
                    .as_secs(),
                message: $message.to_string(),
                level: $level,
                tokens_processed: $tokens_processed,
                tokens_consumed: $tokens_consumed,
                phase: $phase,
                success: $success,
                metadata,
            };
            $context.add_report(report);
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

/// Configuration options for the C2R converter
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Context {
    pub handlers: Box<HandlerMap>,
    pub tokenizer: Box<Tokenizer>,
    pub registry: Box<Registry>,
}


impl Context {
    /// Create a new configuration with the given name
    pub fn new(name: &str) -> Self {
        Context {
            handlers: Box::new(HandlerMap::new(name)),
            tokenizer: Box::new(Tokenizer::new(name)),
            registry: Box::new(Registry::new(name)),
        }
    }

    /// Get the current verbosity level
    pub fn get_verbosity(&self) -> u8 {
        let entry = self.get_entry("verbosity");
        if let Some(Entry::Val(verbosity)) = entry {
               verbosity.clone() as u8
        } else {
            return 0;
        }
    }

    /// Set the verbosity level
    pub fn set_verbosity(&mut self, level: u8) {
        self.set_entry("verbosity",Entry::Val(level as u64));
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
    pub fn set_registry(&mut self, registry: &Registry) {
        self.registry = Box::new(registry.clone());
    }

    /// Add an include directory
    pub fn add_include_dir(&mut self, dir: impl Into<PathBuf>) -> &mut Self {
        let id = get_id("include_directories");
        let paths = match self.registry.entries.get(&id) {
            Some(Entry::List(paths)) => {
                let mut paths = paths.clone();
                paths.push(Entry::Path(dir.into()));
                paths
            },
            Some(Entry::Path(existing)) => {
                let mut paths = Vec::new();
                paths.push(Entry::Path(existing.clone()));
                paths.push(Entry::Path(dir.into()));
                paths
            },
            _ => vec![Entry::Path(dir.into())]
        };

        self.registry.insert(
            id,
            Entry::List(paths)
        );
        self
    }

    /// Set the base directory and return self for builder pattern
    pub fn with_base_dir(&mut self, dir: impl Into<PathBuf>) -> &mut Context {
        let id = get_id("base_dir");
        self.registry.insert(id, Entry::Path(dir.into()));
        self
    }

    /// Set verbosity level and return self for builder pattern
    pub fn with_verbosity(&mut self, level: u8) -> &mut Context {
        let _id = get_id("verbosity");
            self.set_verbosity(level);
        self
        
    }

    /// Get system includes setting
    pub fn get_system_includes(&self) -> bool {
        let id = get_id("system_includes");
        self.registry.entries.get(&id)
            .and_then(|entry| match entry {
                Entry::Bool(value) => Some(*value),
                _ => None
            })
            .unwrap_or(false)
    }

    /// Apply this configuration globally
    pub fn apply(&self) {
        let verbosity = self.get_verbosity();
        info!("Applying settings from registry");
        VERBOSITY_LEVEL.store(verbosity, Ordering::Relaxed);
    }

    /// Set a feature flag with the given name and value
    pub fn set_feature(&mut self, name: &str, value: bool) -> Option<bool> {
        let id = get_id(name);
        let features_entry = self.registry.entries
            .entry(id)
            .or_insert_with(|| Entry::StrMap(HashMap::new()));

        if let Entry::StrMap(features_map) = features_entry {
            let previous = features_map.insert(name.to_string(), Entry::Bool(value));
            previous.and_then(|entry| match entry {
                Entry::Bool(prev_value) => Some(prev_value),
                _ => None
            })
        } else {
            None
        }
    }

    /// Get the value of a feature flag
    pub fn get_feature(&self, name: &str) -> Option<bool> {
        let id = get_id("features");
        self.registry.entries.get(&id)
            .and_then(|entry| match entry {
                Entry::StrMap(features_map) => features_map
                    .get(name)
                    .and_then(|value| match value {
                        Entry::Bool(b) => Some(*b),
                        _ => None
                    }),
                _ => None
            })
    }

    pub fn get_entry(&self, name: &str) -> Option<&Entry> {
        let id = get_id(name);
        let entry = self.registry.entries.get(&id);
        return entry.clone();
    }
    /// Set a value in the registry
    pub fn set_entry(&mut self, name: &str, entry: Entry) {
        self.registry.entries.insert(get_id(name), entry);
    }

    /// Get a value from the registry
    pub fn get_value<T: Any>(&self, name: &str) -> Box<T> {
        let id = get_id(name);
        let entry = self.registry.entries.get(&id);
        let entry_type: Box<dyn Any> = match entry {
            Some(Entry::Id(id)) => Box::new(id.clone()),
            Some(Entry::Val(val)) => Box::new(val.clone()),
            Some(Entry::Str(str)) => Box::new(str.clone()),
            Some(Entry::Bool(b)) => Box::new(b.clone()),
            Some(Entry::Func(f)) => Box::new(*f),
            Some(Entry::Path(path)) => Box::new(path.clone()),
            Some(Entry::Handler(handler)) => Box::new(handler.clone()),
            Some(Entry::Pair(pair)) => Box::new(pair.clone()),
            Some(Entry::List(list)) => Box::new(list.clone()),
            Some(Entry::Any(any)) => any.clone(),
            Some(Entry::IdMap(map)) => Box::new(map.clone()),
            Some(Entry::ValMap(map)) => Box::new(map.clone()),
            Some(Entry::StrMap(map)) => Box::new(map.clone()),
            Some(Entry::BoolMap(map)) => Box::new(map.clone()),
            Some(Entry::FuncMap(map)) => Box::new(map.clone()),
            Some(Entry::PathMap(map)) => Box::new(map.clone()),
            Some(Entry::HandlerMap(map)) => Box::new(map.clone()),
            Some(Entry::PairMap(map)) => Box::new(map.clone()),
            Some(Entry::AnyMap(map)) => Box::new(map.clone()),
            Some(Entry::HandlerReport(report)) => Box::new(report.clone()),
            None => Box::new(id),
        };
        entry_type.downcast::<T>().expect("Failed to downcast entry")
    }
    pub fn set_value(&mut self,name: &str,value : &dyn Any) -> Box<Option<Entry>> {
        let id = get_id(name);
        let previous  = match self.get_entry(name).unwrap() {
            Entry::Val(val) => Box::new(self.registry.insert(id,Entry::Val(val.clone()))),
            Entry::Str(str) => Box::new(self.registry.insert(id,Entry::Str(str.clone()))),
            Entry::Bool(b) => Box::new(self.registry.insert(id,Entry::Bool(b.clone()))),
            Entry::Path(path) => Box::new(self.registry.insert(id,Entry::Path(value.downcast_ref::<PathBuf>().unwrap().clone()))),
            Entry::Handler(handler) => Box::new(self.registry.insert(id,Entry::Handler(value.downcast_ref::<Handler>().unwrap().clone()))),
            Entry::Pair(pair) => Box::new(self.registry.insert(id,Entry::Pair(value.downcast_ref::<Box<(Entry,Entry)>>().unwrap().clone()))),
            Entry::List(list) => Box::new(self.registry.insert(id,Entry::List(value.downcast_ref::<Vec<Entry>>().unwrap().clone()))),
            Entry::Any(any) => Box::new(self.registry.insert(id,Entry::Any(value.downcast_ref::<Box<Entry>>().unwrap().clone()))),
            Entry::IdMap(map) => Box::new(self.registry.insert(id,Entry::IdMap(value.downcast_ref::<HashMap<Id,Entry>>().unwrap().clone()))),
            Entry::StrMap(map) => Box::new(self.registry.insert(id,Entry::StrMap(value.downcast_ref::<HashMap<String,Entry>>().unwrap().clone()))),
            Entry::BoolMap(map) => Box::new(self.registry.insert(id,Entry::BoolMap(value.downcast_ref::<HashMap<bool,Entry>>().unwrap().clone()))),
            Entry::HandlerMap(map) => Box::new(self.registry.insert(id,Entry::HandlerMap(value.downcast_ref::<HashMap<String,Handler>>().unwrap().clone()))),
            Entry::PairMap(map) => Box::new(self.registry.insert(id,Entry::PairMap(value.downcast_ref::<HashMap<String,Box<(Entry,Entry)>>>().unwrap().clone()))),
            Entry::AnyMap(map) => Box::new(self.registry.insert(id,Entry::AnyMap(map.clone()))),
            _ => Box::new(Some(Entry::none())),
        };
        return previous;
    }
    /// Get the base directory
    pub fn get_base_dir(&self) -> Option<PathBuf> {
        let id = get_id("base_dir");
        self.registry.entries.get( &id)
            .and_then(|entry| match entry {
                Entry::Path(path) => Some(path.clone()),
                _ => None
            })
    }

    /// Get include directories
    pub fn get_include_dirs(&self) -> Vec<PathBuf> {
        match self.registry.entries.get(&Id::new(0,"include_directories".to_string())) {
            Some(Entry::Path(path)) => vec![path.clone()],
            Some(Entry::List(paths)) => paths.iter()
                .filter_map(|e| match e {
                    Entry::Path(p) => Some(p.clone()),
                    _ => None
                })
                .collect(),
            _ => Vec::new()
        }
    }

    /// Set include directories
    pub fn set_include_dirs(&mut self, paths: Vec<PathBuf>) -> &mut Self {
        self.registry.insert(
            get_id("include_directories"),
            Entry::List(paths.into_iter().map(Entry::Path).collect())
        );
        self
    }

    /// Configure system includes and return self for builder pattern
    pub fn with_system_includes(&mut self, enabled: bool) -> &mut Context {
        self.registry.insert(get_id("system_includes"), Entry::Bool(enabled));
        self
    }

    /// Register a handler with the context
    pub fn register_handler(&mut self, handler: Handler) {
        self.handlers.register(handler);
    }

    /// Add a handler report to the centralized reporting system
    pub fn add_report(&mut self, report: HandlerReport) {
        let report_id = get_id(&format!("report_{}", self.registry.len()));
        self.registry.insert(report_id, Entry::handler_report(report));
    }

    /// Get all handler reports from the registry
    pub fn get_reports(&self) -> Vec<&HandlerReport> {
        let mut reports = Vec::new();
        // Use proper Registry API to access entries
        for i in 0..self.registry.len() {
            let report_id = get_id(&format!("report_{}", i));
            if let Some(Entry::HandlerReport(report)) = self.registry.get_root_entry(&report_id) {
                reports.push(report);
            }
        }
        // Sort by timestamp for chronological order
        reports.sort_by(|a, b| a.timestamp.cmp(&b.timestamp));
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
        println!("=== END {:?} REPORTS ({} total) ===\n", level, reports.len());
    }

    /// Display a single report in formatted way
    fn display_single_report(&self, report: &HandlerReport) {
        println!("[{}] {:?} | {} | {:?}", 
            report.timestamp, 
            report.level, 
            report.handler_name, 
            report.phase
        );
        println!("  Message: {}", report.message);
        if report.tokens_processed > 0 || report.tokens_consumed > 0 {
            println!("  Tokens: processed={}, consumed={}", 
                report.tokens_processed, 
                report.tokens_consumed
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
            let report_id = get_id(&format!("report_{}", i));
            if let Some(Entry::HandlerReport(_)) = self.registry.get_root_entry(&report_id) {
                report_keys.push(report_id);
            }
        }
        
        for key in report_keys {
            self.registry.remove(&key);
        }
    }
}

/// Comprehensive handler reporting structure for centralized debugging and statistics
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HandlerReport {
    pub handler_id: Id,
    pub handler_name: String,
    pub timestamp: u64,
    pub message: String,
    pub level: ReportLevel,
    pub tokens_processed: usize,
    pub tokens_consumed: usize,
    pub phase: HandlerPhase,
    pub success: bool,
    pub metadata: HashMap<String, String>,
}

/// Reporting levels for handler messages
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReportLevel {
    Debug,
    Info,
    Warning,
    Error,
}

/// Handler processing phases
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HandlerPhase {
    Process,
    Handle,
    Extract,
    Convert,
    Report,
    Result,
    Redirect,
}

impl HandlerReport {
    pub fn new(
        handler_id: Id,
        handler_name: String,
        message: String,
        level: ReportLevel,
        phase: HandlerPhase,
    ) -> Self {
        let timestamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_millis() as u64;
        
        HandlerReport {
            handler_id,
            handler_name,
            timestamp,
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

#[derive(Debug,Clone,PartialEq,Eq)]
pub struct Registry {
    pub id: Id,
    pub entries: HashMap<Id,Entry>,
    pub entry_count: u64,
}
impl Registry {
    pub fn new(name:&str) -> Self {
        Registry {
            id: get_id("registry"),
            entries: HashMap::new(),
            entry_count: 0,
        }
    }
}
impl Default for Registry {
    fn default() -> Self {
        Registry::new("default")
    }
}
#[derive(Debug,Clone)]
pub enum Entry {
    Id(Id),
    Val(u64),
    Str(String),
    Bool(bool),
    Func(fn(Entry) -> Entry),
    Path(PathBuf),
    Handler(Handler),
    Pair(Box<(Entry,Entry)>),
    List(Vec<Entry>),
    Any(Box<Entry>),
    IdMap(HashMap<Id,Entry>),
    ValMap(HashMap<u64,Entry>),
    StrMap(HashMap<String,Entry>),
    BoolMap(HashMap<bool,Entry>),
    FuncMap(HashMap<fn(Entry) -> Entry, Entry>),
    PathMap(HashMap<PathBuf,Entry>),
    HandlerMap(HashMap<String,Handler>),
    PairMap(HashMap<String,Box<(Entry,Entry)>>),
    AnyMap(HashMap<Entry,Entry>),
    HandlerReport(HandlerReport),
}
impl Entry {
    pub fn none() -> Self {
        self::Entry::Id(get_id("none"))
    }
    pub fn id(id :Id) -> Entry { return Entry::Id(id); }
    pub fn val(v: u64) -> Entry { return Entry::Val(v); }
    pub fn str(s: &str) -> Entry {
        return Entry::Str(s.to_string());
    }
    
    /// Create a HandlerReport entry
    pub fn handler_report(report: HandlerReport) -> Entry {
        Entry::HandlerReport(report)
    }
    pub fn bool(b: bool) -> Entry {
        return Entry::Bool(b);
    }
    pub fn func(f: fn(Entry) -> Entry) -> Entry {
        return Entry::Func(f);
    }
    pub fn path(p: &Path) -> Entry {
        return Entry::Path(p.to_path_buf());
    }
    pub fn handler(h: Handler) -> Entry {
        return Entry::Handler(h);
    }
    pub fn pair(left: Entry, right: Entry) -> Entry {
        return Entry::Pair(Box::new((left,right)));
    }
    pub fn list(entries: Vec<Entry>) -> Entry {
        return Entry::List(entries);
    }
    pub fn id_map(entries: HashMap<Id,Entry>) -> Entry {
        return Entry::IdMap(entries);
    }
    pub fn val_map(entries:HashMap<u64,Entry>) -> Entry { return Entry::ValMap(entries); }
    pub fn str_map(entries: HashMap<String,Entry>) -> Entry {
        return Entry::StrMap(entries);
    }
    pub fn bool_map(entries: HashMap<bool,Entry>) -> Entry {
        return Entry::BoolMap(entries);
    }
    pub fn func_map(entries: HashMap<fn(Entry) -> Entry, Entry>) -> Entry {
        return Entry::FuncMap(entries);
    }
    pub fn path_map(entries: HashMap<PathBuf,Entry>) -> Entry {
        return Entry::PathMap(entries);
    }
    pub fn handler_map(map: HashMap<String,Handler>) -> Entry {
        return Entry::HandlerMap(map);
    }
    pub fn pair_map(entries: HashMap<String,Box<(Entry,Entry)>>) -> Entry {
        return Entry::PairMap(entries);
    }
    pub fn any(entry: Entry) -> Entry {
        return Entry::Any(Box::new(entry));
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

    pub fn len(&self) -> u64 {
        self.entry_count
    }

    pub fn get_root_entry(&self, id: &Id) -> Option<&Entry> {
        self.entries.get(id)
    }

    pub fn get_root_entry_mut(&mut self, id: &Id) -> Option<&mut Entry> {
        self.entries.get_mut(id)
    }
    pub fn get_entry(&self, name:&str, depth: u64) -> Option<Entry> {
        let parts: Vec<&str> = name.split('/').collect();
        if parts.is_empty() {
            return None;
        }

        let key = parts[0];
        let entry = self.entries.get(&get_id(name))?;

        if depth == 0 || parts.len() == 1 {
            return Some(entry.clone());
        }

        let rest = parts[1..].join("/");
        if rest.is_empty() {
            return None;
        }

        match entry {
            Entry::Id(id) => Some(Entry::Id(id.clone())),
            Entry::Val(v) => Some(Entry::Val(*v)),
            Entry::Str(s) => Some(Entry::Str(s.clone())),
            Entry::Bool(b) => Some(Entry::Bool(*b)),
            Entry::Func(f) => Some(Entry::Func(*f)),
            Entry::Path(p) => Some(Entry::Path(p.clone())),
            Entry::Handler(h) => Some(Entry::Handler(h.clone())),
            Entry::HandlerReport(report) => Some(Entry::HandlerReport(report.clone())),
            Entry::Pair(p) => {
                match rest.trim() {
                    "left" => Some(p.0.clone()),
                    "right" => Some(p.1.clone()),
                    _ => if depth > 1 {
                        match &p.0 {
                            Entry::StrMap(map) => map.get(&rest).cloned(),
                            _ => match &p.1 {
                                Entry::StrMap(map) => map.get(&rest).cloned(),
                                _ => None
                            }
                        }
                    } else {
                        None
                    }
                }
            },
            Entry::List(entries) => {
                match rest.parse::<usize>().ok() {
                    Some(index) if index < entries.len() => Some(entries[index].clone()),
                    _ => if depth > 1 {
                        for entry in entries {
                            if let Entry::StrMap(map) = entry {
                                if let Some(value) = map.get(&rest) {
                                    return Some(value.clone());
                                }
                            }
                        }
                        None
                    } else {
                        None
                    }
                }
            },
            Entry::Any(any) => {
                // Try to find in current entries first
                if let Some(entry) = self.entries.get(&get_id(name)) {
                    return Some(entry.clone());
                }
                // Recursively try with reduced depth
                if depth > 1 {
                    match &**any {
                        Entry::StrMap(map) => map.get(&rest).cloned(),
                        _ => self.get_entry(name, depth - 1)
                    }
                } else {
                    None
                }
            },
            Entry::IdMap(map) => {
                let id = get_id(&rest);
                if let Some(value) = map.get(&id) {
                    return Some(value.clone());
                }

                if depth > 1 {
                    for value in map.values() {
                        if let Entry::StrMap(inner_map) = value {
                            if let Some(inner_value) = inner_map.get(&rest) {
                                return Some(inner_value.clone());
                            }
                        }
                    }
                }
                None
            },
            Entry::StrMap(map) => map.get(&rest).cloned(),
            Entry::BoolMap(map) => {
                match rest.parse::<bool>().ok() {
                    Some(b) => map.get(&b).cloned(),
                    None => if depth > 1 {
                        for value in map.values() {
                            if let Entry::StrMap(inner_map) = value {
                                if let Some(inner_value) = inner_map.get(&rest) {
                                    return Some(inner_value.clone());
                                }
                            }
                        }
                        None
                    } else {
                        None
                    }
                }
            },
            Entry::FuncMap(map) => {
                match rest.parse::<usize>().ok() {
                    Some(index) if index < map.len() => {
                        let func = map.keys().nth(index)?;
                        Some(Entry::Func(*func))
                    },
                    _ => if depth > 1 {
                        for value in map.values() {
                            if let Entry::StrMap(inner_map) = value {
                                if let Some(inner_value) = inner_map.get(&rest) {
                                    return Some(inner_value.clone());
                                }
                            }
                        }
                        None
                    } else {
                        None
                    }
                }
            },
            Entry::PathMap(map) => {
                let path = std::path::PathBuf::from(&rest);
                if let Some(value) = map.get(&path) {
                    Some(value.clone())
                } else if depth > 1 {
                    for value in map.values() {
                        if let Entry::StrMap(inner_map) = value {
                            if let Some(inner_value) = inner_map.get(&rest) {
                                return Some(inner_value.clone());
                            }
                        }
                    }
                    None
                } else {
                    None
                }
            },
            Entry::HandlerMap(map) => {
                if let Some(h) = map.get(&rest) {
                    Some(Entry::Handler(h.clone()))
                } else if depth > 1 {
                    for (_, handler) in map {
                        if let Some(value) = Some(Entry::Handler(handler.clone())) {
                            return Some(value);
                        }
                    }
                    None
                } else {
                    None
                }
            },
            Entry::PairMap(map) => {
                let parts: Vec<&str> = rest.split(':').collect();
                if parts.len() == 2 {
                    if let Some(pair) = map.get(parts[0]) {
                        match (parts[1]) {
                            "left" => Some(pair.0.clone()),
                            "right" => Some(pair.1.clone()),
                            _ => if depth > 1 {
                                match pair.0.clone() {
                                    Entry::StrMap(inner_map) => inner_map.get(parts[1]).cloned(),
                                    _ => match pair.1.clone() {
                                        Entry::StrMap(inner_map) => inner_map.get(parts[1]).cloned(),
                                        _ => None
                                    }
                                }
                            } else {
                                None
                            }
                        }
                    } else {
                        None
                    }
                } else if let Some(pair) = map.get(&rest) {
                    Some(Entry::Pair(pair.clone()))
                } else if depth > 1 {
                    for (_, pair) in map {
                        if let Entry::StrMap(inner_map) = pair.0.clone() {
                            if let Some(inner_value) = inner_map.get(&rest) {
                                return Some(inner_value.clone());
                            }
                        }
                        if let Entry::StrMap(inner_map) = pair.1.clone() {
                            if let Some(inner_value) = inner_map.get(&rest) {
                                return Some(inner_value.clone());
                            }
                        }
                    }
                    None
                } else {
                    None
                }
            },
            Entry::AnyMap(map) => {
                // Try to find by key as string first
                for (k, v) in map {
                    if let Entry::Str(s) = k {
                        if s == &rest {
                            return Some(v.clone());
                        }
                    }
                }
                // Try to look deeper if depth allows
                if depth > 1 {
                    for v in map.values() {
                        if let Entry::StrMap(inner_map) = v {
                            if let Some(inner_value) = inner_map.get(&rest) {
                                return Some(inner_value.clone());
                            }
                        }
                    }
                }
                None
            },
            Entry::ValMap(map) => {
                match rest.parse::<u64>().ok() {
                    Some(val) => map.get(&val).cloned(),
                    None => if depth > 1 {
                        for value in map.values() {
                            if let Entry::StrMap(inner_map) = value {
                                if let Some(inner_value) = inner_map.get(&rest) {
                                    return Some(inner_value.clone());
                                }
                            }
                        }
                        None
                    } else {
                        None
                    }
                }
            }
        }
    }
    pub fn get_str_map(&self, name: &str, depth: u64) -> Option<&Entry> {
        let parts: Vec<&str> = name.split('.').collect();
        if parts.is_empty() {
            return None;
        }

         let key = parts[0];
      let entry = self.entries.get(&get_id(name))?;

        if depth == 0 || parts.len() == 1 {
            return Some(entry);
        }

        let rest = parts[1..].join(".");
        if rest.is_empty() {
            return None;
        }

        match entry {
            Entry::StrMap(map) => map.get(&rest),
            _ => None
        }
    }

    pub fn get_str_map_mut(&mut self, name: &str, depth: u64) -> Option<&mut Entry> {
        let parts: Vec<&str> = name.split('.').collect();
        if parts.is_empty() {
            return None;
        }

          let key = parts[0];
      let entry = self.entries.get_mut(&get_id(name)).unwrap();

        if depth == 0 || parts.len() == 1 {
            return Some(entry);
        }

        let rest = parts[1..].join(".");
        if rest.is_empty() {
            return None;
        }

        match entry {
            Entry::StrMap(map) => map.get_mut(&rest),
            _ => None
        }
    }
}
/// Sets the global verbosity level
pub fn set_verbosity(level: u8) {
    VERBOSITY_LEVEL.store(level, Ordering::SeqCst);
}

/// Gets the current verbosity level
pub fn get_verbosity() -> u8 {
    VERBOSITY_LEVEL.load(Ordering::SeqCst)
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
        if $crate::context::should_log($crate::context::VERBOSITY_ERROR) {
            eprintln!("[ERROR] {}", format!($($arg)*));
        }
    };
    (warn, $($arg:tt)*) => {
        if $crate::context::should_log($crate::context::VERBOSITY_WARN) {
            eprintln!("[WARN] {}", format!($($arg)*));
        }
    };
    // Debug messages (level 2)
    (debug, $($arg:tt)*) => {
        if $crate::context::should_log($crate::context::VERBOSITY_DEBUG) {
            println!("[DEBUG] {}", format!($($arg)*));
        }
    };

    // Info messages (level 3)
    (info, $($arg:tt)*) => {
        if $crate::context::should_log($crate::context::VERBOSITY_INFO) {
            println!("[INFO] {}", format!($($arg)*));
        }
    };

    // Default to debug level
    ($($arg:tt)*) => {
        $crate::debug!($($arg)*);
    };
}
