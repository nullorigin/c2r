//! Configuration Database
//!
//! Generic configuration storage with type-safe access and feature flags.
//! Implements the Build trait for unified database storage.

use crate::db::web::{Entry, Build};
use std::collections::HashMap;
use std::path::PathBuf;

// ============================================================================
// Configuration Value Types
// ============================================================================

/// Type-safe configuration value enum
#[derive(Debug, Clone, PartialEq)]
pub enum ConfigValue {
    String(String),
    Bool(bool),
    I64(i64),
    F64(f64),
    Usize(usize),
}

impl ConfigValue {
    /// Convert to Entry for serialization
    pub fn to_entry(&self) -> Entry {
        match self {
            ConfigValue::String(s) => Entry::string(s),
            ConfigValue::Bool(b) => Entry::bool(*b),
            ConfigValue::I64(n) => Entry::i64(*n),
            ConfigValue::F64(n) => Entry::f64(*n),
            ConfigValue::Usize(n) => Entry::usize(*n),
        }
    }
    
    /// Create from Entry
    pub fn from_entry(entry: &Entry) -> Option<Self> {
        match entry {
            Entry::String(s, _) => Some(ConfigValue::String(s.clone())),
            Entry::Bool(b, _) => Some(ConfigValue::Bool(*b)),
            Entry::I64(n, _) => Some(ConfigValue::I64(*n)),
            Entry::I32(n, _) => Some(ConfigValue::I64(*n as i64)),
            Entry::F64(n, _) => Some(ConfigValue::F64(*n)),
            Entry::Usize(n, _) => Some(ConfigValue::Usize(*n)),
            _ => None,
        }
    }
    
    /// Get as string
    pub fn as_string(&self) -> Option<&str> {
        match self {
            ConfigValue::String(s) => Some(s),
            _ => None,
        }
    }
    
    /// Get as bool
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            ConfigValue::Bool(b) => Some(*b),
            _ => None,
        }
    }
    
    /// Get as i64
    pub fn as_i64(&self) -> Option<i64> {
        match self {
            ConfigValue::I64(n) => Some(*n),
            ConfigValue::Usize(n) => Some(*n as i64),
            ConfigValue::F64(n) => Some(*n as i64),
            _ => None,
        }
    }
    
    /// Get as f64
    pub fn as_f64(&self) -> Option<f64> {
        match self {
            ConfigValue::F64(n) => Some(*n),
            ConfigValue::I64(n) => Some(*n as f64),
            ConfigValue::Usize(n) => Some(*n as f64),
            _ => None,
        }
    }
    
    /// Get as usize
    pub fn as_usize(&self) -> Option<usize> {
        match self {
            ConfigValue::Usize(n) => Some(*n),
            ConfigValue::I64(n) if *n >= 0 => Some(*n as usize),
            ConfigValue::F64(n) if *n >= 0.0 => Some(*n as usize),
            _ => None,
        }
    }
}

// ============================================================================
// Configuration Structure
// ============================================================================

/// Configuration database for runtime options and settings.
/// Implements Build trait for storage in Web database.
#[derive(Debug, Clone, Default)]
pub struct Config {
    /// Unique configuration name/identifier
    name: String,
    
    /// Configuration category (e.g., "compiler", "runtime", "output")
    category: String,
    
    /// Configuration values (type-safe)
    values: HashMap<String, ConfigValue>,
    
    /// Feature flags
    features: HashMap<String, bool>,
    
    /// Include directories
    include_dirs: Vec<PathBuf>,
    
    /// Base directory
    base_dir: Option<PathBuf>,
}

impl Config {
    /// Create a new configuration with name and category
    pub fn new(name: impl Into<String>, category: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            category: category.into(),
            values: HashMap::new(),
            features: HashMap::new(),
            include_dirs: Vec::new(),
            base_dir: None,
        }
    }
    
    /// Create a default configuration
    pub fn default_config() -> Self {
        Self::new("default", "general")
    }
    
    /// Get the configuration name
    pub fn name(&self) -> &str {
        &self.name
    }
    
    /// Get the configuration category
    pub fn category(&self) -> &str {
        &self.category
    }
    
    /// Set the configuration name
    pub fn set_name(&mut self, name: impl Into<String>) {
        self.name = name.into();
    }
    
    /// Set the configuration category
    pub fn set_category(&mut self, category: impl Into<String>) {
        self.category = category.into();
    }
    
    // ========================================================================
    // Value Access
    // ========================================================================
    
    /// Check if a key exists
    pub fn contains(&self, key: &str) -> bool {
        self.values.contains_key(key)
    }
    
    /// Get all configuration keys
    pub fn keys(&self) -> impl Iterator<Item = &String> {
        self.values.keys()
    }
    
    /// Get number of configuration values
    pub fn len(&self) -> usize {
        self.values.len()
    }
    
    /// Check if configuration is empty
    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }
    
    /// Clear all configuration values
    pub fn clear(&mut self) {
        self.values.clear();
    }
    
    /// Remove a configuration value
    pub fn remove(&mut self, key: &str) -> bool {
        self.values.remove(key).is_some()
    }
    
    // ========================================================================
    // Type-Safe Getters
    // ========================================================================
    
    /// Get a value by key
    pub fn get(&self, key: &str) -> Option<&ConfigValue> {
        self.values.get(key)
    }
    
    /// Get a string value
    pub fn get_string(&self, key: &str) -> Option<String> {
        self.values.get(key).and_then(|v| v.as_string().map(|s| s.to_string()))
    }
    
    /// Get a boolean value
    pub fn get_bool(&self, key: &str) -> Option<bool> {
        self.values.get(key).and_then(|v| v.as_bool())
    }
    
    /// Get a number as f64
    pub fn get_f64(&self, key: &str) -> Option<f64> {
        self.values.get(key).and_then(|v| v.as_f64())
    }
    
    /// Get a number as i64
    pub fn get_i64(&self, key: &str) -> Option<i64> {
        self.values.get(key).and_then(|v| v.as_i64())
    }
    
    /// Get a number as usize
    pub fn get_usize(&self, key: &str) -> Option<usize> {
        self.values.get(key).and_then(|v| v.as_usize())
    }
    
    // ========================================================================
    // Type-Safe Setters
    // ========================================================================
    
    /// Set a string value
    pub fn set_string(&mut self, key: impl Into<String>, value: impl Into<String>) {
        self.values.insert(key.into(), ConfigValue::String(value.into()));
    }
    
    /// Set a boolean value
    pub fn set_bool(&mut self, key: impl Into<String>, value: bool) {
        self.values.insert(key.into(), ConfigValue::Bool(value));
    }
    
    /// Set a number value
    pub fn set_f64(&mut self, key: impl Into<String>, value: f64) {
        self.values.insert(key.into(), ConfigValue::F64(value));
    }
    
    /// Set an integer value
    pub fn set_i64(&mut self, key: impl Into<String>, value: i64) {
        self.values.insert(key.into(), ConfigValue::I64(value));
    }
    
    /// Set a usize value
    pub fn set_usize(&mut self, key: impl Into<String>, value: usize) {
        self.values.insert(key.into(), ConfigValue::Usize(value));
    }
    
    // ========================================================================
    // Feature Flags
    // ========================================================================
    
    /// Enable a feature
    pub fn enable_feature(&mut self, feature: impl Into<String>) {
        self.features.insert(feature.into(), true);
    }
    
    /// Disable a feature
    pub fn disable_feature(&mut self, feature: impl Into<String>) {
        self.features.insert(feature.into(), false);
    }
    
    /// Check if a feature is enabled
    pub fn is_enabled(&self, feature: &str) -> bool {
        self.features.get(feature).copied().unwrap_or(false)
    }
    
    /// Get all features
    pub fn features(&self) -> &HashMap<String, bool> {
        &self.features
    }
    
    // ========================================================================
    // Directory Management
    // ========================================================================
    
    /// Add an include directory
    pub fn add_include_dir(&mut self, dir: impl Into<PathBuf>) {
        self.include_dirs.push(dir.into());
    }
    
    /// Get include directories
    pub fn include_dirs(&self) -> &[PathBuf] {
        &self.include_dirs
    }
    
    /// Clear include directories
    pub fn clear_include_dirs(&mut self) {
        self.include_dirs.clear();
    }
    
    /// Set base directory
    pub fn set_base_dir(&mut self, dir: impl Into<PathBuf>) {
        self.base_dir = Some(dir.into());
    }
    
    /// Get base directory
    pub fn base_dir(&self) -> Option<&PathBuf> {
        self.base_dir.as_ref()
    }
    
    // ========================================================================
    // Common Settings
    // ========================================================================
    
    /// Get verbosity level (0-5)
    pub fn verbosity(&self) -> u8 {
        self.get_usize("verbosity").unwrap_or(0) as u8
    }
    
    /// Set verbosity level
    pub fn set_verbosity(&mut self, level: u8) {
        self.set_usize("verbosity", level as usize);
    }
    
    /// Check if debug mode is enabled
    pub fn is_debug(&self) -> bool {
        self.is_enabled("debug") || self.verbosity() >= 4
    }
    
    /// Check if verbose mode is enabled
    pub fn is_verbose(&self) -> bool {
        self.verbosity() >= 2
    }
    
    /// Check if quiet mode is enabled
    pub fn is_quiet(&self) -> bool {
        self.verbosity() == 0
    }

    /// Builder: set a string
    pub fn with_string(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.set_string(key, value);
        self
    }
    
    /// Builder: set a boolean
    pub fn with_bool(mut self, key: impl Into<String>, value: bool) -> Self {
        self.set_bool(key, value);
        self
    }
    
    /// Builder: enable a feature
    pub fn with_feature(mut self, feature: impl Into<String>) -> Self {
        self.enable_feature(feature);
        self
    }
    
    /// Builder: set verbosity
    pub fn with_verbosity(mut self, level: u8) -> Self {
        self.set_verbosity(level);
        self
    }
    
    /// Builder: set base directory
    pub fn with_base_dir(mut self, dir: impl Into<PathBuf>) -> Self {
        self.set_base_dir(dir);
        self
    }
    
    /// Builder: add include directory
    pub fn with_include_dir(mut self, dir: impl Into<PathBuf>) -> Self {
        self.add_include_dir(dir);
        self
    }
    
    /// Reconstruct a Config from an Entry
    pub fn from_entry(entry: &Entry) -> Option<Config> {
        if entry.kind() != Some("Config") {
            return None;
        }
        
        let name = entry.name()?.to_string();
        let category = entry.get_string_attr("category").unwrap_or("general").to_string();
        
        let mut config = Config::new(name, category);
        
        // Reconstruct values (convert Entry to ConfigValue)
        if let Some(Entry::HashMap(values_map, _)) = entry.attr("values") {
            for (key, val) in values_map {
                if let Some(config_val) = ConfigValue::from_entry(val) {
                    config.values.insert(key.clone(), config_val);
                }
            }
        }
        
        // Reconstruct features
        if let Some(Entry::HashMap(features_map, _)) = entry.attr("features") {
            for (key, val) in features_map {
                if let Entry::Bool(b, _) = val {
                    config.features.insert(key.clone(), *b);
                }
            }
        }
        
        // Reconstruct include directories
        if let Some(dirs_vec) = entry.get_vec_attr("include_dirs") {
            for dir_entry in dirs_vec {
                if let Entry::String(s, _) = dir_entry {
                    config.include_dirs.push(PathBuf::from(s));
                }
            }
        }
        
        // Reconstruct base directory
        if let Some(base) = entry.get_string_attr("base_dir") {
            config.base_dir = Some(PathBuf::from(base));
        }
        
        Some(config)
    }
}

// ============================================================================
// Build Trait Implementation
// ============================================================================

impl Build for Config {
    fn to_entry(&self) -> Entry {
        let mut node = Entry::node("Config", &self.name);
        node.set_attr("category", Entry::string(&self.category));
        
        // Store values as nested entries (convert ConfigValue to Entry)
        if !self.values.is_empty() {
            let values_map: HashMap<String, Entry> = self.values.iter()
                .map(|(k, v)| (k.clone(), v.to_entry()))
                .collect();
            node.set_attr("values", Entry::hashmap(values_map));
        }
        
        // Store features
        if !self.features.is_empty() {
            let features_map: HashMap<String, Entry> = self.features.iter()
                .map(|(k, v)| (k.clone(), Entry::bool(*v)))
                .collect();
            node.set_attr("features", Entry::hashmap(features_map));
        }
        
        // Store include directories
        if !self.include_dirs.is_empty() {
            let dirs: Vec<Entry> = self.include_dirs.iter()
                .map(|p| Entry::string(p.to_string_lossy()))
                .collect();
            node.set_attr("include_dirs", Entry::vec(dirs));
        }
        
        // Store base directory
        if let Some(ref base) = self.base_dir {
            node.set_attr("base_dir", Entry::string(base.to_string_lossy()));
        }
        
        node
    }
    
    fn kind(&self) -> &str {
        "Config"
    }
    
    fn name(&self) -> Option<&str> {
        Some(&self.name)
    }
    
    fn category(&self) -> Option<&str> {
        if self.category.is_empty() { None } else { Some(&self.category) }
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_config_basic() {
        let mut config = Config::new("test", "general");
        
        config.set_string("name", "test");
        config.set_bool("enabled", true);
        config.set_i64("count", 42);
        
        assert_eq!(config.get_string("name"), Some("test".to_string()));
        assert_eq!(config.get_bool("enabled"), Some(true));
        assert_eq!(config.get_i64("count"), Some(42));
    }

    #[test]
    fn test_config_features() {
        let mut config = Config::new("features_test", "general");
        
        config.enable_feature("async");
        config.disable_feature("legacy");
        
        assert!(config.is_enabled("async"));
        assert!(!config.is_enabled("legacy"));
        assert!(!config.is_enabled("unknown"));
    }

    #[test]
    fn test_config_builder() {
        let config = Config::new("builder_test", "compiler")
            .with_string("output", "file.rs")
            .with_bool("optimize", true)
            .with_feature("unsafe")
            .with_verbosity(3);
        
        assert_eq!(config.get_string("output"), Some("file.rs".to_string()));
        assert!(config.is_enabled("unsafe"));
        assert!(config.is_verbose());
    }

    #[test]
    fn test_config_directories() {
        let config = Config::new("dirs_test", "paths")
            .with_base_dir("/home/user/project")
            .with_include_dir("/usr/include")
            .with_include_dir("/usr/local/include");
        
        assert!(config.base_dir().is_some());
        assert_eq!(config.include_dirs().len(), 2);
    }

    #[test]
    fn test_config_to_entry() {
        let config = Config::new("my_config", "runtime")
            .with_string("output", "out.rs")
            .with_feature("debug")
            .with_base_dir("/home/user");
        
        let entry = config.to_entry();
        
        assert_eq!(entry.kind(), Some("Config"));
        assert_eq!(entry.name(), Some("my_config"));
        assert_eq!(entry.get_string_attr("category"), Some("runtime"));
    }

    #[test]
    fn test_config_from_entry() {
        let config = Config::new("roundtrip", "test")
            .with_string("key", "value")
            .with_feature("enabled");
        
        let entry = config.to_entry();
        let restored = Config::from_entry(&entry).unwrap();
        
        assert_eq!(restored.name(), "roundtrip");
        assert_eq!(restored.category(), "test");
    }

    #[test]
    fn test_config_build_trait() {
        let config = Config::new("build_test", "compiler");
        
        // Test Build trait methods
        assert_eq!(Build::kind(&config), "Config");
        assert_eq!(Build::name(&config), Some("build_test"));
        assert_eq!(Build::category(&config), Some("compiler"));
    }
}
