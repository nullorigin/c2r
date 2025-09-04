use std::sync::atomic::{AtomicUsize, Ordering};

// Verbosity levels
pub const LEVEL_ERROR: usize = 0;
pub const LEVEL_WARN: usize = 1;
pub const LEVEL_INFO: usize = 2;
pub const LEVEL_DEBUG: usize = 3;
pub const LEVEL_TRACE: usize = 4;

// Static verbosity level for the application
static VERBOSITY_LEVEL: AtomicUsize = AtomicUsize::new(LEVEL_WARN);

/// Sets the global verbosity level
pub fn set_verbosity_level(level: usize) {
    VERBOSITY_LEVEL.store(level, Ordering::SeqCst);
}

/// Gets the current verbosity level
pub fn get_verbosity_level() -> usize {
    VERBOSITY_LEVEL.load(Ordering::SeqCst)
}

// Macro for error-level logging (always shown)
#[macro_export]
macro_rules! error {
    ($($arg:tt)*) => {
        if $crate::logging::get_verbosity_level() >= $crate::logging::LEVEL_ERROR {
            eprintln!("[ERROR] {}", format!($($arg)*));
        }
    };
}

// Macro for warning-level logging
#[macro_export]
macro_rules! warn {
    ($($arg:tt)*) => {
        if $crate::logging::get_verbosity_level() >= $crate::logging::LEVEL_WARN {
            eprintln!("[WARNING] {}", format!($($arg)*));
        }
    };
}

// Macro for info-level logging
#[macro_export]
macro_rules! info {
    ($($arg:tt)*) => {
        if $crate::logging::get_verbosity_level() >= $crate::logging::LEVEL_INFO {
            println!("[INFO] {}", format!($($arg)*));
        }
    };
}

// Macro for debug-level logging
#[macro_export]
macro_rules! debug {
    ($($arg:tt)*) => {
        if $crate::logging::get_verbosity_level() >= $crate::logging::LEVEL_DEBUG {
            println!("[DEBUG] {}", format!($($arg)*));
        }
    };
}

// Macro for trace-level logging (most verbose)
#[macro_export]
macro_rules! trace {
    ($($arg:tt)*) => {
        if $crate::logging::get_verbosity_level() >= $crate::logging::LEVEL_TRACE {
            println!("[TRACE] {}", format!($($arg)*));
        }
    };
}

// Function to initialize logging based on command line args
pub fn initialize(verbose_count: u64) {
    let level = match verbose_count {
        0 => LEVEL_ERROR,
        1 => LEVEL_WARN,
        2 => LEVEL_INFO,
        3 => LEVEL_DEBUG,
        _ => LEVEL_TRACE,
    };

    set_verbosity_level(level);

    debug!("Logging initialized at level {}", level);
}
