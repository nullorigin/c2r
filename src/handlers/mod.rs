pub mod arrays;
pub mod comments;
pub mod common;
pub mod conditionals;
pub mod enums;
pub mod expressions;
pub mod functions;
pub mod globals;
pub mod loops;
pub mod macros;
pub mod process;
pub mod structs;
pub mod typedefs;
pub mod unions;
pub mod validation;
pub mod variables;
pub mod data;

// Re-export process types for convenience
pub use crate::handlers::process::{ProcessorDecision, ProcessorStage, ProcessorState, Processor};

// Re-export handler types from db module for convenience
pub use crate::db::handler::{
    CodeSegment, FailureRecord, GlobalHandlerStats, Handler, HandlerMetadata, HandlerResult,
    HandlerState, HandlerStats, PatternRoutingSuggestion, PatternUsage, RedirectRequest,
    RedirectResponse, RoutingCondition, RoutingRule, RoutingStats, Selection, SelectionStrategy,
    SuccessRecord, TokenRange,
};
