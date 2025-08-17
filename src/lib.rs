// Export key modules for the handler architecture
pub mod config;
pub mod convert;
pub mod error;
pub mod extract;
pub mod file_utils;
pub mod handler;
pub mod handler_test;
pub mod handlers;
pub mod tests;
pub mod token_parser;

// Re-export key types and traits
pub use error::ConversionError;
pub use handler::{HandlerResult, ParserContext, TokenHandler};
pub use token_parser::{Token, Tokenizer};

// Re-export handlers for convenience
pub use handlers::array_handler::ArrayHandler;
pub use handlers::composite_handler::CompositeHandler;
pub use handlers::function_call::FunctionCallHandler;
pub use handlers::pointer_declaration::PointerDeclarationHandler;
pub use handlers::struct_member_access::StructMemberAccessHandler;
pub use handlers::type_cast_handler::TypeCastHandler;
