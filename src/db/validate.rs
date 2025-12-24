//! Entry Validation System
//!
//! Provides validation for Entry structures including:
//! - Depth limit checking with dynamic thresholds
//! - Loop/cycle detection
//! - Invalid data detection
//! - Recovery mechanisms (culling invalid children)
//! - Report generation for failures

use crate::db::web::Entry;
use crate::db::report::{Report, format_report};
use crate::db::format::OutputFormat;
use std::collections::{HashSet, HashMap};

// ============================================================================
// Validation Configuration
// ============================================================================

/// Default depth limits per Entry type
pub mod limits {
    /// Primitives can be nested to any depth (they're leaf nodes)
    pub const PRIMITIVE_MAX_DEPTH: usize = 256;
    /// Strings can be nested to any depth (they're leaf nodes)
    pub const STRING_MAX_DEPTH: usize = 256;
    /// Vec nesting limit
    pub const VEC_MAX_DEPTH: usize = 64;
    /// HashMap nesting limit
    pub const HASHMAP_MAX_DEPTH: usize = 64;
    /// Node nesting limit (deepest for AST structures)
    pub const NODE_MAX_DEPTH: usize = 128;
    /// Branch nesting limit
    pub const BRANCH_MAX_DEPTH: usize = 64;
    /// Box nesting limit
    pub const BOX_MAX_DEPTH: usize = 32;
    /// Pair nesting limit
    pub const PAIR_MAX_DEPTH: usize = 32;
    
    /// Default threshold ratio (fraction of max before warning)
    pub const DEFAULT_THRESHOLD_RATIO: f64 = 0.75;
    
    /// Minimum successful validations before adjusting threshold
    pub const MIN_SAMPLES_FOR_ADJUSTMENT: usize = 10;
}

/// Configuration for validation behavior
#[derive(Debug, Clone)]
pub struct ValidationConfig {
    /// Maximum depth allowed per type (overrides defaults)
    pub max_depths: HashMap<String, usize>,
    /// Dynamic threshold state
    pub threshold_state: ThresholdState,
    /// Maximum errors before stopping validation
    pub max_errors: usize,
    /// Whether to attempt recovery (culling invalid children)
    pub enable_recovery: bool,
    /// Whether to log to stderr
    pub log_to_stderr: bool,
    /// Custom log writer (if not using stderr)
    pub log_file: Option<String>,
}

impl Default for ValidationConfig {
    fn default() -> Self {
        Self {
            max_depths: HashMap::new(),
            threshold_state: ThresholdState::new(),
            max_errors: 100,
            enable_recovery: true,
            log_to_stderr: true,
            log_file: None,
        }
    }
}

impl ValidationConfig {
    pub fn new() -> Self {
        Self::default()
    }

    /// Set max depth for a specific type
    pub fn with_max_depth(mut self, type_name: &str, depth: usize) -> Self {
        self.max_depths.insert(type_name.to_string(), depth);
        self
    }

    /// Set maximum error count
    pub fn with_max_errors(mut self, count: usize) -> Self {
        self.max_errors = count;
        self
    }

    /// Enable or disable recovery
    pub fn with_recovery(mut self, enabled: bool) -> Self {
        self.enable_recovery = enabled;
        self
    }

    /// Get max depth for an entry type
    pub fn max_depth_for(&self, type_name: &str) -> usize {
        if let Some(&depth) = self.max_depths.get(type_name) {
            return depth;
        }
        match type_name {
            "i8" | "i16" | "i32" | "i64" | "i128" | "isize" |
            "u8" | "u16" | "u32" | "u64" | "u128" | "usize" |
            "f32" | "f64" | "char" | "bool" | "()" => limits::PRIMITIVE_MAX_DEPTH,
            "String" | "PathBuf" | "Range" => limits::STRING_MAX_DEPTH,
            "Vec" => limits::VEC_MAX_DEPTH,
            "HashMap" => limits::HASHMAP_MAX_DEPTH,
            "Node" => limits::NODE_MAX_DEPTH,
            "Branch" => limits::BRANCH_MAX_DEPTH,
            "Box" => limits::BOX_MAX_DEPTH,
            "Pair" => limits::PAIR_MAX_DEPTH,
            _ => limits::NODE_MAX_DEPTH,
        }
    }

    /// Get dynamic threshold for an entry type
    pub fn threshold_for(&self, type_name: &str) -> usize {
        let max = self.max_depth_for(type_name);
        let ratio = self.threshold_state.get_ratio(type_name);
        (max as f64 * ratio) as usize
    }
}

// ============================================================================
// Dynamic Threshold State
// ============================================================================

/// Tracks successful/failed validations to adjust thresholds dynamically
#[derive(Debug, Clone, Default)]
pub struct ThresholdState {
    /// Successful validation depths per type
    success_depths: HashMap<String, Vec<usize>>,
    /// Failed validation depths per type
    failure_depths: HashMap<String, Vec<usize>>,
    /// Current threshold ratios per type
    ratios: HashMap<String, f64>,
}

impl ThresholdState {
    pub fn new() -> Self {
        Self::default()
    }

    /// Record a successful validation at a given depth
    pub fn record_success(&mut self, type_name: &str, depth: usize) {
        self.success_depths
            .entry(type_name.to_string())
            .or_default()
            .push(depth);
        self.update_ratio(type_name);
    }

    /// Record a failed validation at a given depth
    pub fn record_failure(&mut self, type_name: &str, depth: usize) {
        self.failure_depths
            .entry(type_name.to_string())
            .or_default()
            .push(depth);
        self.update_ratio(type_name);
    }

    /// Get current threshold ratio for a type
    pub fn get_ratio(&self, type_name: &str) -> f64 {
        *self.ratios.get(type_name).unwrap_or(&limits::DEFAULT_THRESHOLD_RATIO)
    }

    /// Update threshold ratio based on collected data
    fn update_ratio(&mut self, type_name: &str) {
        let successes = self.success_depths.get(type_name);
        let failures = self.failure_depths.get(type_name);
        
        let total_samples = successes.map(|v| v.len()).unwrap_or(0)
            + failures.map(|v| v.len()).unwrap_or(0);
        
        if total_samples < limits::MIN_SAMPLES_FOR_ADJUSTMENT {
            return;
        }

        // Calculate average successful depth
        let avg_success = successes
            .map(|v| {
                if v.is_empty() { 0.0 } 
                else { v.iter().sum::<usize>() as f64 / v.len() as f64 }
            })
            .unwrap_or(0.0);

        // Calculate minimum failure depth
        let min_failure = failures
            .and_then(|v| v.iter().min().copied())
            .unwrap_or(usize::MAX);

        // Set threshold just below the minimum failure depth
        // but not below 50% of max or above 90%
        if min_failure < usize::MAX && avg_success > 0.0 {
            let suggested = (min_failure as f64 - 1.0).max(avg_success);
            let ratio = (suggested / 100.0).clamp(0.5, 0.9);
            self.ratios.insert(type_name.to_string(), ratio);
        }
    }

    /// Clear all recorded data
    pub fn clear(&mut self) {
        self.success_depths.clear();
        self.failure_depths.clear();
        self.ratios.clear();
    }
}

// ============================================================================
// Validation Errors
// ============================================================================

/// Types of validation errors
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValidationErrorKind {
    /// Entry exceeds maximum allowed depth
    MaxDepthExceeded { depth: usize, max: usize },
    /// Entry exceeds threshold (warning level)
    ThresholdExceeded { depth: usize, threshold: usize },
    /// Circular reference detected
    CycleDetected { path: Vec<usize> },
    /// Invalid entry data
    InvalidData { reason: String },
    /// Empty required field
    EmptyRequired { field: String },
    /// Type mismatch
    TypeMismatch { expected: String, found: String },
}

/// A validation error with context
#[derive(Debug, Clone)]
pub struct ValidationError {
    /// Kind of error
    pub kind: ValidationErrorKind,
    /// Path to the error (indices)
    pub path: Vec<usize>,
    /// Entry type name
    pub type_name: String,
    /// Whether this error is recoverable
    pub recoverable: bool,
}

impl ValidationError {
    pub fn new(kind: ValidationErrorKind, path: Vec<usize>, type_name: &str) -> Self {
        let recoverable = matches!(
            kind,
            ValidationErrorKind::ThresholdExceeded { .. } |
            ValidationErrorKind::InvalidData { .. } |
            ValidationErrorKind::EmptyRequired { .. }
        );
        Self {
            kind,
            path,
            type_name: type_name.to_string(),
            recoverable,
        }
    }

    pub fn max_depth(depth: usize, max: usize, path: Vec<usize>, type_name: &str) -> Self {
        Self::new(
            ValidationErrorKind::MaxDepthExceeded { depth, max },
            path,
            type_name,
        )
    }

    pub fn threshold(depth: usize, threshold: usize, path: Vec<usize>, type_name: &str) -> Self {
        Self::new(
            ValidationErrorKind::ThresholdExceeded { depth, threshold },
            path,
            type_name,
        )
    }

    pub fn cycle(cycle_path: Vec<usize>, path: Vec<usize>, type_name: &str) -> Self {
        Self::new(
            ValidationErrorKind::CycleDetected { path: cycle_path },
            path,
            type_name,
        )
    }

    pub fn invalid_data(reason: &str, path: Vec<usize>, type_name: &str) -> Self {
        Self::new(
            ValidationErrorKind::InvalidData { reason: reason.to_string() },
            path,
            type_name,
        )
    }

    /// Convert to a Report for logging
    pub fn to_report(&self) -> Report {
        let message = match &self.kind {
            ValidationErrorKind::MaxDepthExceeded { depth, max } => {
                format!("Maximum depth exceeded: {} > {} for type {}", depth, max, self.type_name)
            }
            ValidationErrorKind::ThresholdExceeded { depth, threshold } => {
                format!("Depth threshold exceeded: {} > {} for type {}", depth, threshold, self.type_name)
            }
            ValidationErrorKind::CycleDetected { path } => {
                format!("Cycle detected in {} at path {:?}", self.type_name, path)
            }
            ValidationErrorKind::InvalidData { reason } => {
                format!("Invalid data in {}: {}", self.type_name, reason)
            }
            ValidationErrorKind::EmptyRequired { field } => {
                format!("Empty required field '{}' in {}", field, self.type_name)
            }
            ValidationErrorKind::TypeMismatch { expected, found } => {
                format!("Type mismatch: expected {}, found {}", expected, found)
            }
        };

        let id = format!("validation_{}", self.path.iter().map(|i| i.to_string()).collect::<Vec<_>>().join("_"));
        Report::new(id, "validation_error", &message)
            .with_tag(format!("path:{:?}", self.path))
            .with_tag(format!("type:{}", self.type_name))
            .with_tag(format!("recoverable:{}", self.recoverable))
    }
}

// ============================================================================
// Validation Result
// ============================================================================

/// Result of validating an entry tree
#[derive(Debug, Clone, Default)]
pub struct ValidationResult {
    /// All errors found
    pub errors: Vec<ValidationError>,
    /// Whether validation passed (no fatal errors)
    pub valid: bool,
    /// Number of entries validated
    pub entries_checked: usize,
    /// Maximum depth encountered
    pub max_depth_seen: usize,
    /// Indices of entries that were culled during recovery
    pub culled_indices: Vec<usize>,
}

impl ValidationResult {
    pub fn new() -> Self {
        Self {
            valid: true,
            ..Default::default()
        }
    }

    pub fn add_error(&mut self, error: ValidationError) {
        if !error.recoverable {
            self.valid = false;
        }
        self.errors.push(error);
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn error_count(&self) -> usize {
        self.errors.len()
    }

    pub fn fatal_error_count(&self) -> usize {
        self.errors.iter().filter(|e| !e.recoverable).count()
    }
}

// ============================================================================
// Validator
// ============================================================================

/// Entry tree validator with depth tracking and cycle detection
pub struct Validator {
    config: ValidationConfig,
    result: ValidationResult,
    /// Track visited entry pointer addresses for cycle detection
    visited: HashSet<usize>,
    /// Current path for error reporting
    current_path: Vec<usize>,
}

impl Validator {
    pub fn new(config: ValidationConfig) -> Self {
        Self {
            config,
            result: ValidationResult::new(),
            visited: HashSet::new(),
            current_path: Vec::new(),
        }
    }

    pub fn with_default_config() -> Self {
        Self::new(ValidationConfig::default())
    }

    /// Validate an entry tree starting from a root entry
    pub fn validate(&mut self, entry: &Entry) -> &ValidationResult {
        self.visited.clear();
        self.current_path.clear();
        self.result = ValidationResult::new();
        
        self.validate_entry(entry, 0, 0);
        
        &self.result
    }

    /// Validate a collection of entries (e.g., from a Site)
    pub fn validate_entries(&mut self, entries: &[Entry]) -> &ValidationResult {
        self.visited.clear();
        self.current_path.clear();
        self.result = ValidationResult::new();

        for (idx, entry) in entries.iter().enumerate() {
            if self.result.error_count() >= self.config.max_errors {
                break;
            }
            self.current_path.push(idx);
            self.validate_entry(entry, 0, idx);
            self.current_path.pop();
        }

        &self.result
    }

    fn validate_entry(&mut self, entry: &Entry, depth: usize, _index: usize) {
        self.result.entries_checked += 1;
        self.result.max_depth_seen = self.result.max_depth_seen.max(depth);

        // Check for cycle using pointer address
        let ptr_addr = entry as *const Entry as usize;
        if !self.visited.insert(ptr_addr) {
            let cycle_path: Vec<usize> = self.current_path.clone();
            self.result.add_error(ValidationError::cycle(
                cycle_path.clone(),
                self.current_path.clone(),
                entry.type_name(),
            ));
            return;
        }

        let type_name = entry.type_name();
        let max_depth = self.config.max_depth_for(type_name);
        let threshold = self.config.threshold_for(type_name);

        // Check depth limits
        if depth > max_depth {
            self.result.add_error(ValidationError::max_depth(
                depth,
                max_depth,
                self.current_path.clone(),
                type_name,
            ));
            self.config.threshold_state.record_failure(type_name, depth);
            self.visited.remove(&ptr_addr);
            return;
        }

        if depth > threshold {
            self.result.add_error(ValidationError::threshold(
                depth,
                threshold,
                self.current_path.clone(),
                type_name,
            ));
        }

        // Validate entry-specific data
        if let Some(error) = self.validate_entry_data(entry) {
            self.result.add_error(error);
        }

        // Recursively validate children
        self.validate_children(entry, depth);

        // Record success
        self.config.threshold_state.record_success(type_name, depth);
        self.visited.remove(&ptr_addr);
    }

    fn validate_entry_data(&self, entry: &Entry) -> Option<ValidationError> {
        match entry {
            Entry::Node { kind, name, .. } => {
                if kind.is_empty() {
                    return Some(ValidationError::new(
                        ValidationErrorKind::EmptyRequired { field: "kind".to_string() },
                        self.current_path.clone(),
                        "Node",
                    ));
                }
                if name.is_empty() {
                    return Some(ValidationError::new(
                        ValidationErrorKind::EmptyRequired { field: "name".to_string() },
                        self.current_path.clone(),
                        "Node",
                    ));
                }
            }
            Entry::Range(range, _) => {
                if range.start > range.end {
                    return Some(ValidationError::invalid_data(
                        &format!("Invalid range: start ({}) > end ({})", range.start, range.end),
                        self.current_path.clone(),
                        "Range",
                    ));
                }
            }
            Entry::String(s, _) => {
                // Check for invalid UTF-8 sequences (shouldn't happen in Rust, but check anyway)
                if s.chars().any(|c| c == '\0') {
                    return Some(ValidationError::invalid_data(
                        "String contains null bytes",
                        self.current_path.clone(),
                        "String",
                    ));
                }
            }
            _ => {}
        }
        None
    }

    fn validate_children(&mut self, entry: &Entry, depth: usize) {
        match entry {
            Entry::Vec(items, _) => {
                for (i, item) in items.iter().enumerate() {
                    self.current_path.push(i);
                    self.validate_entry(item, depth + 1, i);
                    self.current_path.pop();
                }
            }
            Entry::HashMap(map, _) => {
                for (i, (_, item)) in map.iter().enumerate() {
                    self.current_path.push(i);
                    self.validate_entry(item, depth + 1, i);
                    self.current_path.pop();
                }
            }
            Entry::Node { attrs, .. } => {
                for (i, (_, item)) in attrs.iter().enumerate() {
                    self.current_path.push(i);
                    self.validate_entry(item, depth + 1, i);
                    self.current_path.pop();
                }
            }
            Entry::Branch { true_path, false_path, .. } => {
                for (i, item) in true_path.iter().enumerate() {
                    self.current_path.push(i);
                    self.validate_entry(item, depth + 1, i);
                    self.current_path.pop();
                }
                let offset = true_path.len();
                for (i, item) in false_path.iter().enumerate() {
                    self.current_path.push(offset + i);
                    self.validate_entry(item, depth + 1, offset + i);
                    self.current_path.pop();
                }
            }
            Entry::Box(inner, _) => {
                self.current_path.push(0);
                self.validate_entry(inner, depth + 1, 0);
                self.current_path.pop();
            }
            Entry::Pair(pair, _) => {
                self.current_path.push(0);
                self.validate_entry(&pair.0, depth + 1, 0);
                self.current_path.pop();
                self.current_path.push(1);
                self.validate_entry(&pair.1, depth + 1, 1);
                self.current_path.pop();
            }
            _ => {}
        }
    }

    /// Log validation errors
    pub fn log_errors(&self) {
        for error in &self.result.errors {
            let report = error.to_report();
            if self.config.log_to_stderr {
                eprintln!("{}", format_report(&report, &OutputFormat::Text));
            }
        }
    }

    /// Get reports for all errors
    pub fn get_reports(&self) -> Vec<Report> {
        self.result.errors.iter().map(|e| e.to_report()).collect()
    }

    /// Get the validation result
    pub fn result(&self) -> &ValidationResult {
        &self.result
    }

    /// Consume and return the result
    pub fn into_result(self) -> ValidationResult {
        self.result
    }
}

// ============================================================================
// Recovery Functions
// ============================================================================

/// Attempt to recover an entry tree by culling invalid children
pub fn recover_entry(entry: &mut Entry, config: &ValidationConfig) -> Vec<usize> {
    let mut culled = Vec::new();
    recover_entry_recursive(entry, 0, config, &mut culled, &mut HashSet::new());
    culled
}

fn recover_entry_recursive(
    entry: &mut Entry,
    depth: usize,
    config: &ValidationConfig,
    culled: &mut Vec<usize>,
    visited: &mut HashSet<usize>,
) {
    let type_name = entry.type_name();
    let max_depth = config.max_depth_for(type_name);

    if depth > max_depth {
        return;
    }

    match entry {
        Entry::Vec(items, _) => {
            let mut i = 0;
            while i < items.len() {
                let child_type = items[i].type_name();
                let child_max = config.max_depth_for(child_type);
                
                if depth + 1 > child_max {
                    culled.push(i);
                    items.remove(i);
                } else {
                    recover_entry_recursive(&mut items[i], depth + 1, config, culled, visited);
                    i += 1;
                }
            }
        }
        Entry::HashMap(map, _) => {
            let keys_to_remove: Vec<String> = map
                .iter()
                .filter(|(_, v)| {
                    let child_type = v.type_name();
                    let child_max = config.max_depth_for(child_type);
                    depth + 1 > child_max
                })
                .map(|(k, _)| k.clone())
                .collect();

            for key in keys_to_remove {
                map.remove(&key);
            }

            for (_, item) in map.iter_mut() {
                recover_entry_recursive(item, depth + 1, config, culled, visited);
            }
        }
        Entry::Node { attrs, .. } => {
            let keys_to_remove: Vec<String> = attrs
                .iter()
                .filter(|(_, v)| {
                    let child_type = v.type_name();
                    let child_max = config.max_depth_for(child_type);
                    depth + 1 > child_max
                })
                .map(|(k, _)| k.clone())
                .collect();

            for key in keys_to_remove {
                attrs.remove(&key);
            }

            for (_, item) in attrs.iter_mut() {
                recover_entry_recursive(item, depth + 1, config, culled, visited);
            }
        }
        Entry::Branch { true_path, false_path, .. } => {
            let mut i = 0;
            while i < true_path.len() {
                let child_type = true_path[i].type_name();
                let child_max = config.max_depth_for(child_type);
                
                if depth + 1 > child_max {
                    culled.push(i);
                    true_path.remove(i);
                } else {
                    recover_entry_recursive(&mut true_path[i], depth + 1, config, culled, visited);
                    i += 1;
                }
            }

            let mut i = 0;
            while i < false_path.len() {
                let child_type = false_path[i].type_name();
                let child_max = config.max_depth_for(child_type);
                
                if depth + 1 > child_max {
                    culled.push(i);
                    false_path.remove(i);
                } else {
                    recover_entry_recursive(&mut false_path[i], depth + 1, config, culled, visited);
                    i += 1;
                }
            }
        }
        Entry::Box(inner, _) => {
            recover_entry_recursive(inner.as_mut(), depth + 1, config, culled, visited);
        }
        Entry::Pair(pair, _) => {
            recover_entry_recursive(&mut pair.0, depth + 1, config, culled, visited);
            recover_entry_recursive(&mut pair.1, depth + 1, config, culled, visited);
        }
        _ => {}
    }
}

// ============================================================================
// Convenience Functions
// ============================================================================

/// Quick validation of an entry with default config
pub fn validate(entry: &Entry) -> ValidationResult {
    let mut validator = Validator::with_default_config();
    validator.validate(entry);
    validator.into_result()
}

/// Quick validation with error logging
pub fn validate_and_log(entry: &Entry) -> ValidationResult {
    let mut validator = Validator::with_default_config();
    validator.validate(entry);
    validator.log_errors();
    validator.into_result()
}

/// Validate and attempt recovery
pub fn validate_and_recover(entry: &mut Entry) -> ValidationResult {
    let config = ValidationConfig::default();
    let mut validator = Validator::new(config.clone());
    validator.validate(entry);
    
    if validator.result.has_errors() && config.enable_recovery {
        let culled = recover_entry(entry, &config);
        validator.result.culled_indices = culled;
    }
    
    validator.log_errors();
    validator.into_result()
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_validate_simple_entry() {
        let entry = Entry::string("hello");
        let result = validate(&entry);
        assert!(result.valid);
        assert_eq!(result.errors.len(), 0);
    }

    #[test]
    fn test_validate_node() {
        let entry = Entry::node("Function", "test_func");
        let result = validate(&entry);
        assert!(result.valid);
    }

    #[test]
    fn test_validate_empty_node_kind() {
        let entry = Entry::node("", "test_func");
        let result = validate(&entry);
        assert!(result.has_errors());
    }

    #[test]
    fn test_validate_invalid_range() {
        let entry = Entry::range(100..50); // Invalid: start > end
        let result = validate(&entry);
        assert!(result.has_errors());
    }

    #[test]
    fn test_validate_nested_vec() {
        let inner = vec![Entry::i32(1), Entry::i32(2), Entry::i32(3)];
        let entry = Entry::vec(inner);
        let result = validate(&entry);
        assert!(result.valid);
        assert_eq!(result.max_depth_seen, 1);
    }

    #[test]
    fn test_threshold_state() {
        let mut state = ThresholdState::new();
        
        // Record some successes
        for _ in 0..10 {
            state.record_success("Node", 5);
        }
        
        // Should have adjusted the ratio
        let ratio = state.get_ratio("Node");
        assert!(ratio >= 0.5 && ratio <= 0.9);
    }

    #[test]
    fn test_validation_config() {
        let config = ValidationConfig::new()
            .with_max_depth("Node", 50)
            .with_max_errors(10);
        
        assert_eq!(config.max_depth_for("Node"), 50);
        assert_eq!(config.max_errors, 10);
    }

    #[test]
    fn test_recovery() {
        let mut entry = Entry::vec(vec![
            Entry::node("Test", "item1"),
            Entry::node("Test", "item2"),
        ]);
        
        let config = ValidationConfig::new().with_max_depth("Node", 0);
        let culled = recover_entry(&mut entry, &config);
        
        // Should have culled the nodes since depth 1 > max 0
        assert_eq!(entry.as_vec().unwrap().len(), 0);
        assert!(!culled.is_empty());
    }
}
