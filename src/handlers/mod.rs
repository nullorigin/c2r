use crate::config::HandlerReport;
use crate::document::ElementInfo;
use crate::error::C2RError;
use crate::extract::ExtractedElement;
use crate::handler::HandlerResult;
pub use crate::handlers::common::*;
use crate::lock::Id;
use crate::{ConvertedElement, Handler, context};
use std::ops::Range;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;
// Global counter for handler IDs to ensure uniqueness
static HANDLER_ID_COUNTER: AtomicU64 = AtomicU64::new(100);

// Function to get the next handler ID
pub fn next_handler_id() -> Id {
    let _num = HANDLER_ID_COUNTER.fetch_add(1, Ordering::SeqCst);
    let mut id = Id::get("next_handler_id");
    id.increment();
    id
}

/// Create a complete handler with all function types
pub fn create_handler(
    id: Id,
    role: &str,
    priority: u64,
    process: Option<fn(Range<usize>) -> Result<bool, C2RError>>,
    handle: Option<fn(Range<usize>) -> Result<HandlerResult, C2RError>>,
    extract: Option<fn(Range<usize>) -> Result<Option<ExtractedElement>, C2RError>>,
    convert: Option<fn(Range<usize>) -> Result<Option<ConvertedElement>, C2RError>>,
    document: Option<fn(ElementInfo) -> Result<Option<String>, C2RError>>,
    report: Option<fn() -> Result<HandlerReport, C2RError>>,
    result: Option<fn(Range<usize>, HandlerResult) -> Result<HandlerResult, C2RError>>,
    redirect: Option<fn(Range<usize>, HandlerResult) -> Result<HandlerResult, C2RError>>,
) -> Handler {
    let mut handler = Handler::new(id.clone(), role.to_string(), priority.clone());
    handler.process = process;
    handler.handle = handle;
    handler.extract = extract;
    handler.convert = convert;
    handler.document = document;
    handler.report = report;
    handler.result = result;
    handler.redirect = redirect;
    handler
}

// Export submodules
pub mod array_handler;
pub mod comment_handler;
pub mod common;
pub mod control_flow_handler;
pub mod enum_handler;
pub mod expression_handler;
pub mod function_handler;
pub mod global_handler;
pub mod include_handler;
pub mod macro_handler;
pub mod struct_handler;
pub mod typedef_handler;

// New specialized handlers
pub mod conditional_macro_handler;
pub mod define_macro_handler;
pub mod extern_declaration_handler;
pub mod function_declaration_handler;
pub mod function_definition_handler;
pub mod global_constant_handler;
pub mod global_variable_handler;
pub mod include_directive_handler;
pub mod main_function_handler;
pub mod pattern_router;
pub mod smart_redirect;
pub mod static_variable_handler;

/// Creates all handlers and returns them in a Vec
pub fn create_all_handlers() -> Vec<Handler> {
    let mut handlers = Vec::new();

    // Add handlers in order of priority (highest priority first)
    // Specialized function handlers
    handlers.push(main_function_handler::create_main_function_handler()); // Priority: 1000
    handlers.push(function_declaration_handler::create_function_declaration_handler()); // Priority: 900
    handlers.push(function_definition_handler::create_function_definition_handler()); // Priority: 850

    // Specialized macro handlers
    handlers.push(include_directive_handler::create_include_directive_handler()); // Priority: 950
    handlers.push(define_macro_handler::create_define_macro_handler()); // Priority: 920
    handlers.push(conditional_macro_handler::create_conditional_macro_handler()); // Priority: 910

    // Specialized global handlers
    handlers.push(extern_declaration_handler::create_extern_declaration_handler()); // Priority: 880
    handlers.push(static_variable_handler::create_static_variable_handler()); // Priority: 870
    handlers.push(global_constant_handler::create_global_constant_handler()); // Priority: 850
    handlers.push(global_variable_handler::create_global_variable_handler()); // Priority: 800

    // Original handlers (maintaining their existing priorities, but lower than specialized ones)
    handlers.push(struct_handler::create_struct_handler()); // Priority: ~200
    handlers.push(enum_handler::create_enum_handler()); // Priority: ~200  
    handlers.push(typedef_handler::create_typedef_handler()); // Priority: ~200
    handlers.push(control_flow_handler::create_control_flow_handler()); // Priority: ~150
    handlers.push(array_handler::create_array_handler()); // Priority: ~100
    handlers.push(comment_handler::create_comment_handler()); // Priority: ~50

    // Keep original handlers as fallbacks with lower priority
    handlers.push(include_handler::create_include_handler()); // Fallback for includes
    handlers.push(macro_handler::create_macro_handler()); // Fallback for macros
    handlers.push(global_handler::create_global_handler()); // Fallback for globals  
    handlers.push(function_handler::create_function_handler()); // Fallback for functions
    handlers.push(expression_handler::create_expression_handler()); // Lowest priority fallback

    handlers
}

/// Registers all handlers with the provided context
pub fn register_all_handlers() {
    // Create all handlers
    let handlers = create_all_handlers();

    // Register each handler
    for handler in handlers {
        context!(register_handler(handler.clone()))
    }
}
