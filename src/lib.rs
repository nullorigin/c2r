// Library module declarations
// Core modules (adaptive and analysis functionality now integrated into handler.rs)
pub mod atomic;
pub mod config;
pub mod convert;
pub mod entry;
pub mod error;
pub mod extract;
pub mod file_utils;
pub mod handler; // Now contains integrated adaptive and analysis functionality
pub mod handlers;
pub mod info;
pub mod json;
pub mod lock;
pub mod logging;
pub mod macros;
pub mod map;
pub mod maybe;
pub mod pattern;
pub mod registry;
pub mod routing;
pub mod sample;
pub mod table;
pub mod thread;
pub mod token;
pub mod util;

pub use crate::config::*;
pub use crate::convert::*;
pub use crate::entry::*;
pub use crate::error::*;
pub use crate::extract::*;
pub use crate::file_utils::*;
// Deprecated modules - functionality moved to handler.rs
// pub mod adaptive; // DEPRECATED - moved to handler.rs
// pub mod analysis; // DEPRECATED - moved to handler.rs
// Re-export public APIs for external use
pub use crate::handler::*;
pub use crate::handlers::*;
// Now includes adaptive and analysis types
pub use crate::info::*;
pub use crate::lock::*;
pub use crate::logging::*;
pub use crate::macros::*;
pub use crate::map::*;
pub use crate::maybe::*;
pub use crate::pattern::*;
pub use crate::registry::*;
pub use crate::routing::*;
pub use crate::sample::*;
pub use crate::table::*;
pub use crate::token::*;
pub use crate::util::*;
// Tests module (only compiled during testing)
#[cfg(test)]
pub mod tests;
