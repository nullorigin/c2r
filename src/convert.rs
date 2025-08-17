use crate::config::Config;
use crate::error::ConversionError;
use crate::extract::{
    ExtractedElement, ExtractedEnum, ExtractedFunction, ExtractedGlobal, ExtractedMacro,
    ExtractedStruct, ExtractedTypedef,
};
use crate::handler::{HandlerRegistry, HandlerResult, ParserContext};
use crate::log;
use crate::token_parser::Token;

/// Main converter that processes extracted elements and converts them to Rust
pub struct Converter {
    registry: HandlerRegistry,
    context: ParserContext,
    config: Config,
}

impl Converter {
    /// Create a new converter with the provided handler registry
    pub fn new(registry: HandlerRegistry) -> Self {
        let config = Config::new();
        let mut context = ParserContext::new();
        // Store config in context for handlers to access
        context.set_value("config", Box::new(config.clone()));

        Converter {
            registry,
            context,
            config,
        }
    }

    /// Create a new converter with the provided handler registry and configuration
    pub fn new_with_config(registry: HandlerRegistry, config: Config) -> Self {
        let mut context = ParserContext::new();
        // Store config in context for handlers to access
        context.set_value("config", Box::new(config.clone()));

        Converter {
            registry,
            context,
            config,
        }
    }

    /// Get a reference to the current config
    pub fn config(&self) -> &Config {
        &self.config
    }

    /// Get a mutable reference to the current config
    pub fn config_mut(&mut self) -> &mut Config {
        &mut self.config
    }

    /// Set a new configuration
    pub fn set_config(&mut self, config: Config) {
        self.config = config.clone();
        // Update the config in the context as well
        self.context.set_value("config", Box::new(config));
    }

    /// Convert an extracted element to Rust code
    pub fn convert(&mut self, element: &ExtractedElement) -> Result<String, ConversionError> {
        match element {
            ExtractedElement::Function(function) => self.convert_function(function),
            ExtractedElement::Struct(struct_def) => self.convert_struct(struct_def),
            ExtractedElement::Enum(enum_def) => self.convert_enum(enum_def),
            ExtractedElement::Typedef(typedef) => self.convert_typedef(typedef),
            ExtractedElement::Global(global) => self.convert_global(global),
            ExtractedElement::Macro(macro_def) => self.convert_macro(macro_def),
        }
    }

    /// Convert multiple extracted elements to Rust code
    pub fn convert_all(
        &mut self,
        elements: &[ExtractedElement],
    ) -> Result<String, ConversionError> {
        let mut result = String::new();

        // First, convert all type definitions (structs, enums, typedefs)
        for element in elements {
            match element {
                ExtractedElement::Struct(_)
                | ExtractedElement::Enum(_)
                | ExtractedElement::Typedef(_) => {
                    let rust_code = self.convert(element)?;
                    result.push_str(&rust_code);
                    result.push_str("\n\n");
                }
                _ => {}
            }
        }

        // Then convert globals
        for element in elements {
            if let ExtractedElement::Global(_) = element {
                let rust_code = self.convert(element)?;
                result.push_str(&rust_code);
                result.push_str("\n\n");
            }
        }

        // Then convert macros
        for element in elements {
            if let ExtractedElement::Macro(_) = element {
                let rust_code = self.convert(element)?;
                result.push_str(&rust_code);
                result.push_str("\n\n");
            }
        }

        // Finally convert functions
        for element in elements {
            if let ExtractedElement::Function(_) = element {
                let rust_code = self.convert(element)?;
                result.push_str(&rust_code);
                result.push_str("\n\n");
            }
        }

        Ok(result)
    }

    /// Convert a C source file to Rust
    pub fn convert_file(&mut self, _c_code: &str) -> Result<String, ConversionError> {
        // This would use the Extractor to extract elements and then convert them
        // For now, we'll return a placeholder
        Ok("// Not yet implemented".to_string())
    }

    /// Convert an extracted function to Rust code
    fn convert_function(
        &mut self,
        function: &ExtractedFunction,
    ) -> Result<String, ConversionError> {
        // Use FunctionHandler from the registry
        let tokens = &function.tokens;

        // Try to find a handler for this function
        match self.for_handler(tokens) {
            Ok(rust_code) => return Ok(rust_code),
            Err(e) => {
                log!(
                    error,
                    "Error finding handler for function: {} falling back to default conversion",
                    e
                )
            }
        }
        // If no handler was found or none could handle it, generate default conversion
        let mut result = String::new();
        // Function visibility and qualifiers
        if function.is_static {
            result.push_str("fn ");
        } else {
            result.push_str("pub fn ");
        }

        // Function name
        result.push_str(&function.name);

        // Function parameters
        result.push('(');
        for (i, (param_name, param_type)) in function.parameters.iter().enumerate() {
            let param_type_str = param_type
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<String>>()
                .join("");

            result.push_str(&format!("{}: {}", param_name, param_type_str));

            if i < function.parameters.len() - 1 {
                result.push_str(", ");
            }
        }

        if function.is_variadic {
            if !function.parameters.is_empty() {
                result.push_str(", ");
            }
            result.push_str("/* variadic args */");
        }

        result.push(')');

        // Return type
        let return_type_str = function
            .return_type
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<String>>()
            .join("");

        if !return_type_str.is_empty() && return_type_str != "void" {
            result.push_str(&format!(" -> {}", return_type_str));
        }

        // Function body (if available)
        if !function.body.is_empty() {
            result.push_str(" {\n");

            // Convert each statement in the body
            for stmt in &function.body {
                // This is a simplified approach; actual implementation would use handlers
                result.push_str(&format!("    // {:?}\n", stmt));
            }

            result.push_str("}\n");
        } else {
            // Just a function declaration
            result.push_str(";");
        }

        Ok(result)
    }

    /// Convert an extracted struct to Rust code
    fn convert_struct(&mut self, struct_def: &ExtractedStruct) -> Result<String, ConversionError> {
        // Use the struct handler from the registry if available
        let tokens = &struct_def.tokens;

        // Try to find a handler for this struct
        match self.for_handler(tokens) {
            Ok(rust_code) => return Ok(rust_code),
            Err(e) => {
                log!(
                    error,
                    "Error finding handler for struct: {} falling back to default conversion",
                    e
                )
            }
        }

        // If no handler was found, generate default conversion
        let mut result = String::new();

        // Struct declaration
        result.push_str("#[derive(Debug, Clone)]\n");
        result.push_str(&format!("pub struct {} {{\n", struct_def.name));

        // Fields
        for (field_name, field_type) in &struct_def.fields {
            let field_type_str = field_type
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<String>>()
                .join("");

            result.push_str(&format!("    pub {}: {},\n", field_name, field_type_str));
        }

        result.push_str("}\n");

        Ok(result)
    }

    /// Convert an extracted enum to Rust code
    fn convert_enum(&mut self, enum_def: &ExtractedEnum) -> Result<String, ConversionError> {
        // Use the enum handler from the registry if available
        let tokens = &enum_def.tokens;

        // Try to find a handler for this enum
        match self.for_handler(tokens) {
            Ok(rust_code) => return Ok(rust_code),
            Err(e) => {
                log!(
                    error,
                    "Error finding handler for enum: {} falling back to default conversion",
                    e
                )
            }
        }

        // If no handler was found, generate default conversion
        let mut result = String::new();

        // Enum declaration
        result.push_str("#[derive(Debug, Clone, PartialEq, Eq)]\n");
        result.push_str(&format!("pub enum {} {{\n", enum_def.name));

        // Values
        for (value_name, value_opt) in &enum_def.values {
            if let Some(value) = value_opt {
                result.push_str(&format!("    {} = {},\n", value_name, value));
            } else {
                result.push_str(&format!("    {},\n", value_name));
            }
        }

        result.push_str("}\n");

        Ok(result)
    }

    /// Convert an extracted typedef to Rust code
    fn convert_typedef(&mut self, typedef: &ExtractedTypedef) -> Result<String, ConversionError> {
        // Use the typedef handler from the registry if available
        let tokens = &typedef.tokens;

        // Try to find a handler for this typedef
        match self.for_handler(tokens) {
            Ok(rust_code) => return Ok(rust_code),
            Err(e) => {
                log!(
                    error,
                    "Error finding handler for typedef: {} falling back to default conversion",
                    e
                )
            }
        }

        // If no handler was found, generate default conversion
        let original_type_str = typedef
            .original_type
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<String>>()
            .join("");

        Ok(format!(
            "pub type {} = {};\n",
            typedef.name, original_type_str
        ))
    }

    /// Convert an extracted global variable to Rust code
    fn convert_global(&mut self, global: &ExtractedGlobal) -> Result<String, ConversionError> {
        // Use a handler from the registry if available
        let tokens = &global.tokens;

        // Try to find a handler for this global
        match self.for_handler(tokens) {
            Ok(rust_code) => return Ok(rust_code),
            Err(e) => {
                log!(
                    error,
                    "Error finding handler for global: {} falling back to default conversion",
                    e
                )
            }
        }

        // If no handler was found, generate default conversion
        let mut result = String::new();

        // Global variable declaration
        if global.is_static {
            result.push_str("static ");
        } else {
            result.push_str("pub static ");
        }

        if global.is_const {
            result.push_str("const ");
        }

        let var_type_str = global
            .var_type
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<String>>()
            .join("");

        result.push_str(&format!("{}: {}", global.name, var_type_str));

        if let Some(ref initial_value) = global.initial_value {
            let value_str = initial_value
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<String>>()
                .join("");

            result.push_str(&format!(" = {}", value_str));
        }

        result.push_str(";");

        Ok(result)
    }

    /// Convert an extracted macro to Rust code
    fn convert_macro(&mut self, macro_def: &ExtractedMacro) -> Result<String, ConversionError> {
        // Use a handler from the registry if available
        let tokens = &macro_def.tokens;

        // Try to find a handler for this macro
        match self.for_handler(tokens) {
            Ok(rust_code) => return Ok(rust_code),
            Err(e) => {
                log!(
                    error,
                    "Error finding handler for macro: {} falling back to default conversion",
                    e
                )
            }
        }

        // If no handler was found, generate default conversion for simple constant macros
        if macro_def.params.is_empty() && !macro_def.body.is_empty() {
            let body_str = macro_def
                .body
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<String>>()
                .join("");

            return Ok(format!("pub const {}: _ = {};\n", macro_def.name, body_str));
        }

        // For function-like macros, use macro_rules!
        let mut result = String::new();
        result.push_str("#[macro_export]\n");
        result.push_str("macro_rules! ");
        result.push_str(&macro_def.name);
        result.push_str(" {\n    (");

        // Macro parameters
        for (i, param) in macro_def.params.iter().enumerate() {
            result.push_str(&format!("${}: expr", param));

            if i < macro_def.params.len() - 1 {
                result.push_str(", ");
            }
        }

        result.push_str(") => {\n        ");

        // Macro body - simplified for now
        let body_str = macro_def
            .body
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<String>>()
            .join("");

        result.push_str(&body_str);

        result.push_str("\n    };\n}\n");

        Ok(result)
    }
    fn for_handler(&mut self, tokens: &[Token]) -> Result<String, ConversionError> {
        for handler in self.registry.get_handlers() {
            if handler.can_handle(tokens, &self.context) {
                match handler.handle(tokens, &mut self.context)? {
                    HandlerResult::RustCode(rust_code) => {
                        return Ok(rust_code);
                    }
                    HandlerResult::NotHandled => continue,
                    HandlerResult::Statement(stmt) => {
                        // Convert the statement to rust code
                        return Ok(format!(
                            "// Statement conversion not yet implemented\n// {:?}",
                            stmt
                        ));
                    }
                    HandlerResult::Replace(replacement) => {
                        return Ok(replacement);
                    }
                }
            }
        }
        Err(ConversionError::general("No handler found"))
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::extract::Extractor;
    use crate::handlers::register_handlers;

    #[test]
    fn test_convert_simple_function() {
        // Create a test function
        let c_code = "int add(int a, int b) { return a + b; }";
        let mut tokenizer = crate::token_parser::Tokenizer::new(c_code);
        let tokens = tokenizer.tokenize();

        let extractor = Extractor::new();
        let extracted = extractor.extract_function(&tokens).unwrap().unwrap();
        let element = ExtractedElement::Function(extracted);

        // Create converter with handlers
        let mut registry = HandlerRegistry::new();
        register_handlers(&mut registry, &Config::new());

        let mut converter = Converter::new(registry);
        let result = converter.convert(&element).unwrap();

        assert!(result.contains("fn add"));
        assert!(result.contains("a: i32"));
        assert!(result.contains("b: i32"));
    }
}
