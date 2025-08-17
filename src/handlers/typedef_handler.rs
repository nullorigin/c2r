use crate::error::ConversionError;
use crate::handler::{HandlerResult, ParserContext, TokenHandler};
use crate::token_parser::Token;

/// Handler for typedef declarations.
/// Converts C typedefs to Rust type aliases.
pub struct TypedefHandler;

impl TokenHandler for TypedefHandler {
    fn can_handle(&self, tokens: &[Token], _context: &ParserContext) -> bool {
        tokens.len() >= 3 && tokens[0] == Token::Identifier("typedef".to_string())
    }

    fn handle(
        &self,
        tokens: &[Token],
        _context: &mut ParserContext,
    ) -> Result<HandlerResult, ConversionError> {
        if !self.can_handle(tokens, _context) {
            return Ok(HandlerResult::NotHandled);
        }

        // Collect all tokens from the start to the semicolon
        let mut end_pos = tokens.len();
        for (i, token) in tokens.iter().enumerate() {
            if *token == Token::Semicolon {
                end_pos = i;
                break;
            }
        }

        if end_pos == tokens.len() {
            return Err(ConversionError::general(
                "Missing semicolon in typedef declaration",
            ));
        }

        // Skip the 'typedef' token
        let typedef_tokens = &tokens[1..end_pos];
        if typedef_tokens.is_empty() {
            return Err(ConversionError::general("Empty typedef declaration"));
        }

        // The last identifier is the new type name
        let new_type_name =
            if let Token::Identifier(name) = &typedef_tokens[typedef_tokens.len() - 1] {
                name.clone()
            } else {
                return Err(ConversionError::general(
                    "Last token is not an identifier in typedef",
                ));
            };

        // Extract original type tokens (everything except the last token)
        let original_type_tokens = &typedef_tokens[0..typedef_tokens.len() - 1];
        if original_type_tokens.is_empty() {
            return Err(ConversionError::general(
                "No original type specified in typedef",
            ));
        }

        // Special case for struct and enum
        if original_type_tokens.len() >= 2 {
            if let Token::Identifier(keyword) = &original_type_tokens[0] {
                if keyword == "struct" || keyword == "enum" {
                    if let Token::Identifier(name) = &original_type_tokens[1] {
                        let rust_type = format!("{} {}", keyword, name);
                        let rust_code = format!("type {} = {};", new_type_name, rust_type);
                        return Ok(HandlerResult::RustCode(rust_code));
                    }
                }
            }
        }

        // Handle basic types
        let mut is_const = false;
        let mut is_pointer = false;
        let mut base_type = String::new();

        // Process original type tokens
        for token in original_type_tokens {
            match token {
                Token::Identifier(id) if id == "const" => {
                    is_const = true;
                }
                Token::Identifier(id) => {
                    if !base_type.is_empty() {
                        base_type.push(' ');
                    }
                    base_type.push_str(id);
                }
                Token::Asterisk => {
                    is_pointer = true;
                }
                _ => {}
            }
        }

        // Convert base C type to Rust type
        let rust_base_type = self.convert_c_type_to_rust(&base_type);

        // Format final type based on modifiers
        let rust_type = if is_pointer {
            let pointer_type = if is_const { "*const" } else { "*mut" };

            // Special case for void*
            if rust_base_type == "()" {
                if is_const {
                    "*const std::ffi::c_void".to_string()
                } else {
                    "*mut std::ffi::c_void".to_string()
                }
            } else {
                format!("{} {}", pointer_type, rust_base_type)
            }
        } else {
            rust_base_type
        };

        // Format as Rust type alias
        let rust_code = format!("type {} = {};", new_type_name, rust_type);

        Ok(HandlerResult::RustCode(rust_code))
    }
}

impl TypedefHandler {
    pub fn new() -> Self {
        TypedefHandler {}
    }

    /// Helper method to convert C types to Rust types
    fn convert_c_type_to_rust(&self, c_type: &str) -> String {
        match c_type.trim() {
            "char" => "i8".to_string(),
            "short" => "i16".to_string(),
            "int" => "i32".to_string(),
            "long" => "i64".to_string(),
            "long long" => "i64".to_string(),
            "unsigned char" => "u8".to_string(),
            "unsigned short" => "u16".to_string(),
            "unsigned int" => "u32".to_string(),
            "unsigned long" => "u64".to_string(),
            "unsigned long long" => "u64".to_string(),
            "float" => "f32".to_string(),
            "double" => "f64".to_string(),
            "void" => "()".to_string(),
            "size_t" => "usize".to_string(),
            "ssize_t" => "isize".to_string(),
            _ => c_type.to_string(), // Use as-is for unknown types
        }
    }
}
