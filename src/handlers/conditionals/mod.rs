//! Conditional Handlers
//!
//! Converts C conditional constructs to Rust equivalents.

pub mod if_else;
pub mod switch_case;

pub use if_else::IfElseHandler;
pub use switch_case::SwitchCaseHandler;
