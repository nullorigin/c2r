// Library module declarations
// Core modules (adaptive and analysis functionality now integrated into handler.rs)
pub mod config;
pub mod convert;
pub mod entry;
pub mod error;
pub mod extract;
pub mod file_utils;
pub mod handler; // Now contains integrated adaptive and analysis functionality
pub mod handlers;
pub mod info;
pub mod lock;
pub mod logging;
pub mod pattern;
pub mod registry;
pub mod sample;
pub mod table;
pub mod thread;
pub mod token;
pub mod json;
pub mod util;
pub mod option_lock;
pub mod maybe;


// Deprecated modules - functionality moved to handler.rs
// pub mod adaptive; // DEPRECATED - moved to handler.rs
// pub mod analysis; // DEPRECATED - moved to handler.rs
// Re-export public APIs for external use
pub use crate::handler::*; // Now includes adaptive and analysis types
pub use crate::info::*;
pub use crate::config::*;
pub use crate::convert::*;
pub use crate::entry::*;
pub use crate::error::*;
pub use crate::extract::*;
pub use crate::file_utils::*;
pub use crate::handlers::*;
pub use crate::lock::*;
pub use crate::logging::*;
pub use crate::pattern::*;
pub use crate::registry::*;
pub use crate::sample::*;
pub use crate::table::*;
pub use crate::token::*;
pub use crate::util::*;
pub use crate::maybe::*;
pub use crate::option_lock::*;
// Tests module (only compiled during testing)
#[cfg(test)]
pub mod tests;
