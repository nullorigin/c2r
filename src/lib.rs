// Library module declarations
pub mod analysis;
pub mod config;
pub mod convert;
pub mod document;
pub mod entry;
pub mod error;
pub mod extract;
pub mod file_utils;
pub mod handler;
pub mod handlers;
pub mod lock;
pub mod logging;
pub mod pattern;
pub mod registry;
pub mod sample;
pub mod table;
pub mod thread;
pub mod token;
// Re-export public APIs for external use
pub use crate::analysis::*;
pub use crate::config::*;
pub use crate::convert::*;
pub use crate::document::*;
pub use crate::entry::*;
pub use crate::error::*;
pub use crate::extract::*;
pub use crate::file_utils::*;
pub use crate::handler::*;
pub use crate::handlers::*;
pub use crate::lock::*;
pub use crate::logging::*;
pub use crate::pattern::*;
pub use crate::registry::*;
pub use crate::sample::*;
pub use crate::table::*;
pub use crate::token::*;
// Tests module (only compiled during testing)
#[cfg(test)]
pub mod tests;
