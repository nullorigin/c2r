// Register all available handlers
pub mod array_handler;
pub mod composite_handler;
pub mod define_const_handler;
pub mod define_handler;
pub mod define_macro_handler;
pub mod enum_handler;
pub mod function_call;
pub mod function_handler;
pub mod ifdef_handler;
pub mod include_handler;
pub mod loop_handler;
pub mod multiline_macro_handler;
pub mod pointer_declaration;
pub mod struct_declaration;
pub mod struct_member_access;
pub mod type_cast_handler;
pub mod typedef_handler;
pub mod union_handler;

pub use array_handler::ArrayHandler;
pub use composite_handler::CompositeHandler;
pub use define_const_handler::DefineConstHandler;
pub use define_handler::DefineHandler;
pub use define_macro_handler::DefineMacroHandler;
pub use enum_handler::EnumHandler;
pub use function_call::FunctionCallHandler;
pub use function_handler::FunctionHandler;
pub use ifdef_handler::IfdefHandler;
pub use include_handler::IncludeHandler;
pub use loop_handler::LoopHandler;
pub use multiline_macro_handler::MultilineMacroHandler;
pub use pointer_declaration::PointerDeclarationHandler;
pub use struct_declaration::StructDeclarationHandler;
pub use struct_member_access::StructMemberAccessHandler;
pub use type_cast_handler::TypeCastHandler;
pub use typedef_handler::TypedefHandler;
pub use union_handler::UnionHandler;

use crate::config::Config;
use crate::handler::HandlerRegistry;

/// Register all standard handlers with the registry
pub fn register_handlers(registry: &mut HandlerRegistry, config: &Config) {
    // Register struct declaration handler
    registry.register(Box::new(StructDeclarationHandler::new()));

    // Register pointer declaration handler
    registry.register(Box::new(PointerDeclarationHandler::new()));

    // Register struct member access handler
    registry.register(Box::new(StructMemberAccessHandler::new()));

    // Register function call handler
    registry.register(Box::new(FunctionCallHandler::new()));

    // Register array handler
    registry.register(Box::new(ArrayHandler::new()));

    // Register type cast handler
    registry.register(Box::new(TypeCastHandler::new()));

    // Register define directive handlers
    registry.register(Box::new(DefineConstHandler::new())); // For simple constants
    registry.register(Box::new(DefineMacroHandler::new())); // For complex macros
    registry.register(Box::new(DefineHandler::new())); // Kept for backward compatibility

    // Register preprocessor directive handlers
    registry.register(Box::new(IfdefHandler::new()));

    // Register enum and union handlers
    registry.register(Box::new(EnumHandler::new()));
    registry.register(Box::new(UnionHandler::new()));

    // Register loop handler for converting C loops to idiomatic Rust
    registry.register(Box::new(LoopHandler::new()));

    // Register function handler for the new architecture
    registry.register(Box::new(FunctionHandler::new()));

    // Register typedef handler
    registry.register(Box::new(TypedefHandler::new()));

    // Register multi-line macro handler
    registry.register(Box::new(MultilineMacroHandler::new()));

    // Register include handler with configuration
    registry.register(Box::new(IncludeHandler::new(config.clone())));

    // Create and register a composite handler for complex expressions
    // This demonstrates how handlers can be chained together
    let mut complex_expr_handler = CompositeHandler::new("ComplexExpressionHandler");
    complex_expr_handler
        .add_handler(Box::new(StructMemberAccessHandler::new()))
        .add_handler(Box::new(ArrayHandler::new()))
        .add_handler(Box::new(FunctionCallHandler::new()));

    registry.register(Box::new(complex_expr_handler));
    // Add more handlers here as they are implemented
}

/// Register all standard handlers with the registry using default configuration
/// This is kept for backward compatibility
pub fn register_handlers_default(registry: &mut HandlerRegistry) {
    register_handlers(registry, &Config::new());
}
