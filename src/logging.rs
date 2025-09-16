use std::sync::atomic::Ordering;

// Verbosity levels
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
            $crate::config::Global::write(|ctx| ctx.add_report(report.clone()));
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
             $crate::config::Global::write(|ctx| ctx.add_report(report));
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
            $crate::config::Global::write(|ctx| ctx.add_report(report));
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
             $crate::context!(add_report(report));
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
    let level = LEVELS[verbose_count as usize];
    set_verbosity_level(level);
    debug!("Logging initialized at level {}", level);
}
