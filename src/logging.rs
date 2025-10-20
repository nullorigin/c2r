use std::sync::atomic::Ordering;
pub const ERROR: u8 = 0;
pub const WARN: u8 = 1;
pub const INFO: u8 = 2;
pub const DEBUG: u8 = 3;
pub const TRACE: u8 = 4;
pub const LEVELS: [u8; 5] = [ERROR, WARN, INFO, DEBUG, TRACE];
/// Sets the global verbosity level
pub fn set_verbosity_level(level: u8) {
    VERBOSITY_LEVEL.store(level, Ordering::SeqCst);
}

/// Gets the current verbosity level
pub fn get_verbosity_level() -> u8 {
    VERBOSITY_LEVEL.load(Ordering::SeqCst)
}
use std::sync::atomic::AtomicU8;

// Note: time!(), function_name!(), and report!() macros have been moved to macros.rs

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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ReportLevel {
    Debug,
    Info,
    Warning,
    Error,
}

impl std::fmt::Display for ReportLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ReportLevel::Debug => write!(f, "DEBUG"),
            ReportLevel::Info => write!(f, "INFO"),
            ReportLevel::Warning => write!(f, "WARNING"),
            ReportLevel::Error => write!(f, "ERROR"),
        }
    }
}
// Macro for error-level logging (always shown)
#[macro_export]
macro_rules! error {
    ($($arg:tt)*) => {
        if $crate::logging::get_verbosity_level() >= $crate::logging::ERROR {
            eprintln!("[ERROR] {}", format!($($arg)*));
        }
    };
}

// Macro for warning-level logging
#[macro_export]
macro_rules! warn {
    ($($arg:tt)*) => {
        if $crate::logging::get_verbosity_level() >= $crate::logging::WARN {
            eprintln!("[WARNING] {}", format!($($arg)*));
        }
    };
}

// Macro for info-level logging
#[macro_export]
macro_rules! info {
    ($($arg:tt)*) => {
        if $crate::logging::get_verbosity_level() >= $crate::logging::INFO {
            println!("[INFO] {}", format!($($arg)*));
        }
    };
}

// Macro for debug-level logging
#[macro_export]
macro_rules! debug {
    ($($arg:tt)*) => {
        if $crate::logging::get_verbosity_level() >= $crate::logging::DEBUG {
            println!("[DEBUG] {}", format!($($arg)*));
        }
    };
}

// Macro for trace-level logging (most verbose)
#[macro_export]
macro_rules! trace {
    ($($arg:tt)*) => {
        if $crate::logging::get_verbosity_level() >= $crate::logging::TRACE {
            println!("[TRACE] {}", format!($($arg)*));
        }
    };
}

// Function to initialize logging based on command line args
pub fn initialize(verbose_count: u64) {
    let level = LEVELS[std::cmp::min(verbose_count as usize, LEVELS.len() - 1)];
    set_verbosity_level(level);
    debug!("Logging initialized at level {}", level);
}
