//! Configuration Database
//!
//! Generic configuration storage with type-safe access and feature flags.

use crate::db::web::Entry;
use std::collections::HashMap;
use std::path::PathBuf;

// ============================================================================
// Configuration Database
// ============================================================================

/// Configuration database for runtime options and settings
#[derive(Debug, Clone, Default)]
pub struct Config {
    /// Configuration values
    values: HashMap<String, Entry>,
    
    /// Feature flags
    features: HashMap<String, bool>,
    
    /// Include directories
    include_dirs: Vec<PathBuf>,
    
    /// Base directory
    base_dir: Option<PathBuf>,
}

impl Config {
    /// Create a new configuration database
    pub fn new() -> Self {
        Self::default()
    }
    
    // ========================================================================
    // Generic Value Storage
    // ========================================================================
    
    /// Set a configuration value
    pub fn set(&mut self, key: impl Into<String>, value: Entry) {
        self.values.insert(key.into(), value);
    }
    
    /// Get a configuration value
    pub fn get(&self, key: &str) -> Option<&Entry> {
        self.values.get(key)
    }
    
    /// Remove a configuration value
    pub fn remove(&mut self, key: &str) -> Option<Entry> {
        self.values.remove(key)
    }
    
    /// Check if a key exists
    pub fn contains(&self, key: &str) -> bool {
        self.values.contains_key(key)
    }
    
    /// Get all configuration keys
    pub fn keys(&self) -> impl Iterator<Item = &String> {
        self.values.keys()
    }
    
    /// Clear all configuration values
    pub fn clear(&mut self) {
        self.values.clear();
    }
    
    // ========================================================================
    // Type-Safe Getters
    // ========================================================================
    
    /// Get a string value
    pub fn get_string(&self, key: &str) -> Option<String> {
        self.get(key).and_then(|v| match v {
            Entry::String(s, _) => Some(s.clone()),
            _ => None,
        })
    }
    
    /// Get a boolean value
    pub fn get_bool(&self, key: &str) -> Option<bool> {
        self.get(key).and_then(|v| match v {
            Entry::Bool(b, _) => Some(*b),
            _ => None,
        })
    }
    
    /// Get a number as f64
    pub fn get_f64(&self, key: &str) -> Option<f64> {
        self.get(key).and_then(|v| match v {
            Entry::F64(n, _) => Some(*n),
            Entry::I32(n, _) => Some(*n as f64),
            Entry::I64(n, _) => Some(*n as f64),
            Entry::Usize(n, _) => Some(*n as f64),
            _ => None,
        })
    }
    
    /// Get a number as i64
    pub fn get_i64(&self, key: &str) -> Option<i64> {
        self.get_f64(key).map(|n| n as i64)
    }
    
    /// Get a number as usize
    pub fn get_usize(&self, key: &str) -> Option<usize> {
        self.get_f64(key).map(|n| n as usize)
    }
    
    // ========================================================================
    // Type-Safe Setters
    // ========================================================================
    
    /// Set a string value
    pub fn set_string(&mut self, key: impl Into<String>, value: impl Into<String>) {
        self.set(key, Entry::string(value.into()));
    }
    
    /// Set a boolean value
    pub fn set_bool(&mut self, key: impl Into<String>, value: bool) {
        self.set(key, Entry::bool(value));
    }
    
    /// Set a number value
    pub fn set_f64(&mut self, key: impl Into<String>, value: f64) {
        self.set(key, Entry::f64(value));
    }
    
    /// Set an integer value
    pub fn set_i64(&mut self, key: impl Into<String>, value: i64) {
        self.set(key, Entry::i64(value));
    }
    
    /// Set a usize value
    pub fn set_usize(&mut self, key: impl Into<String>, value: usize) {
        self.set(key, Entry::usize(value));
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

    /// Builder: set a value
    pub fn with_value(mut self, key: impl Into<String>, value: Entry) -> Self {
        self.set(key, value);
        self
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
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_config_basic() {
        let mut config = Config::new();
        
        config.set_string("name", "test");
        config.set_bool("enabled", true);
        config.set_i64("count", 42);
        
        assert_eq!(config.get_string("name"), Some("test".to_string()));
        assert_eq!(config.get_bool("enabled"), Some(true));
        assert_eq!(config.get_i64("count"), Some(42));
    }

    #[test]
    fn test_config_features() {
        let mut config = Config::new();
        
        config.enable_feature("async");
        config.disable_feature("legacy");
        
        assert!(config.is_enabled("async"));
        assert!(!config.is_enabled("legacy"));
        assert!(!config.is_enabled("unknown"));
    }

    #[test]
    fn test_config_builder() {
        let config = Config::new()
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
        let config = Config::new()
            .with_base_dir("/home/user/project")
            .with_include_dir("/usr/include")
            .with_include_dir("/usr/local/include");
        
        assert!(config.base_dir().is_some());
        assert_eq!(config.include_dirs().len(), 2);
    }
}
