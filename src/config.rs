use std::collections::HashMap;
use std::path::Path;
use std::sync::atomic::{AtomicU8, Ordering};

// Global verbosity level
// 0: No output
// 1: Errors only
// 2: Errors + Debug messages
// 3: All messages (Errors + Debug + Info)
pub static VERBOSITY_LEVEL: AtomicU8 = AtomicU8::new(1); // Default to errors only

// Verbosity level constants
pub const VERBOSITY_NONE: u8 = 0;
pub const VERBOSITY_ERROR: u8 = 1;
pub const VERBOSITY_DEBUG: u8 = 2;
pub const VERBOSITY_INFO: u8 = 3;

/// Configuration options for the C2R converter
#[derive(Clone)]
pub struct Config {
    /// Base directory for resolving relative paths
    pub base_dir: String,

    /// List of include directories to search for header files
    pub include_dirs: Vec<String>,

    /// Verbosity level for logging
    pub verbosity: u8,

    /// Whether to treat system includes (<>) differently than local includes ("")
    pub process_system_includes: bool,
    pub features: HashMap<String, bool>,
}

impl Default for Config {
    fn default() -> Self {
        Self::new()
    }
}

impl Config {
    /// Create a new configuration with default settings
    pub fn new() -> Self {
        Config {
            base_dir: ".".to_string(),
            include_dirs: Vec::new(),
            verbosity: VERBOSITY_ERROR,
            process_system_includes: false,
            features: Default::default(),
        }
    }

    /// Add an include directory
    pub fn add_include_dir<P: AsRef<Path>>(&mut self, dir: P) -> &mut Self {
        self.include_dirs
            .push(dir.as_ref().to_string_lossy().to_string());
        self
    }

    /// Set the base directory
    pub fn with_base_dir<P: AsRef<Path>>(mut self, dir: P) -> Self {
        self.base_dir = dir.as_ref().to_string_lossy().to_string();
        self
    }

    /// Set verbosity level
    pub fn with_verbosity(mut self, level: u8) -> Self {
        self.verbosity = level;
        self
    }

    /// Enable or disable system include processing
    pub fn with_system_includes(mut self, enabled: bool) -> Self {
        self.process_system_includes = enabled;
        self
    }

    /// Apply this configuration globally
    pub fn apply(&self) {
        set_verbosity(self.verbosity);
    }

    pub fn set_feature(&mut self, name: &str, value: bool) {
        self.features.insert(name.to_string(), value);
    }

    pub fn get_feature(&self, name: &str) -> Option<bool> {
        self.features.get(name).cloned()
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
        if $crate::config::should_log($crate::config::VERBOSITY_ERROR) {
            eprintln!("[ERROR] {}", format!($($arg)*));
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
        $crate::log!(debug, $($arg)*);
    };
}
