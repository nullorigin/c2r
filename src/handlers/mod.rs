use crate::config::HandlerReport;
use crate::error::ConversionError;
use crate::extract::ExtractedElement;
pub(crate) use crate::handler::{Handler, HandlerResult};
use crate::{context, Token};
use crate::{ConvertedElement, Id};
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
    process: Option<fn(&[Token]) -> Result<bool, ConversionError>>,
    handle: Option<fn(&[Token]) -> Result<HandlerResult, ConversionError>>,
    extract: Option<
        fn(&[Token]) -> Result<Option<ExtractedElement>, ConversionError>,
    >,
    convert: Option<
        fn(&[Token]) -> Result<Option<ConvertedElement>, ConversionError>,
    >,
    report: Option<fn(&[Token]) -> Result<HandlerReport, ConversionError>>,
    result: Option<
        fn(&[Token], HandlerResult) -> Result<HandlerResult, ConversionError>,
    >,
    redirect: Option<
        fn(&[Token], HandlerResult) -> Result<HandlerResult, ConversionError>,
    >,
) -> Handler {
    let mut handler = Handler::new(id.clone(), role.to_string(), priority.clone());
    handler.process = process;
    handler.handle = handle;
    handler.extract = extract;
    handler.convert = convert;
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

/// Creates all handlers and returns them in a Vec
pub fn create_all_handlers() -> Vec<Handler> {
    let mut handlers = Vec::new();

    // Add handlers in order of priority
    handlers.push(include_handler::create_include_handler());
    handlers.push(macro_handler::create_macro_handler());
    handlers.push(control_flow_handler::create_control_flow_handler());
    handlers.push(expression_handler::create_expression_handler());
    handlers.push(struct_handler::create_struct_handler());
    handlers.push(enum_handler::create_enum_handler());
    handlers.push(typedef_handler::create_typedef_handler());
    handlers.push(function_handler::create_function_handler());
    handlers.push(array_handler::create_array_handler());
    handlers.push(comment_handler::create_comment_handler());
    handlers.push(global_handler::create_global_handler());

    handlers
}

/// Registers all handlers with the provided context
pub fn register_all_handlers() {
    // Create all handlers
    let handlers = create_all_handlers();

    // Register each handler
    for handler in handlers {
        context!().register_handler(handler.clone());
    }
}
