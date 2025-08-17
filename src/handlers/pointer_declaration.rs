use crate::error::ConversionError;
use crate::handler::{HandlerResult, ParserContext, TokenHandler};
use crate::token_parser::Token;

/// Handler for pointer variable declarations like FILE *fptr
pub struct PointerDeclarationHandler;

impl PointerDeclarationHandler {
    pub fn new() -> Self {
        Self {}
    }

    /// Check if tokens contain a pointer declaration
    fn can_process(&self, tokens: &[Token]) -> bool {
        // Must have enough tokens to be a declaration
        if tokens.len() < 3 {
            return false;
        }

        // Check for patterns like "TYPE * IDENTIFIER" or "TYPE *IDENTIFIER"
        for i in 0..tokens.len() - 2 {
            // Check for TYPE * IDENTIFIER pattern
            if i + 2 < tokens.len()
                && matches!(tokens[i], Token::Identifier(_))
                && tokens[i + 1] == Token::Asterisk
                && matches!(tokens[i + 2], Token::Identifier(_))
            {
                return true;
            }

            // Check for TYPE *IDENTIFIER pattern (without space)
            if i + 1 < tokens.len()
                && matches!(tokens[i], Token::Identifier(_))
                && tokens[i + 1] == Token::Asterisk
            {
                return true;
            }
        }

        false
    }

    /// Extract type name from token sequence, handling const and type combinations
    fn extract_type(&self, tokens: &[Token]) -> (String, bool) {
        // Check for const keyword
        let mut is_const = false;
        let mut type_parts = Vec::new();

        // Collect all parts of the type (excluding 'const' for now)
        for token in tokens {
            if let Token::Identifier(id) = token {
                if id == "const" {
                    is_const = true;
                } else if id != "*" {
                    // Exclude asterisk from type name
                    type_parts.push(id.clone());
                }
            }

            // Stop once we hit the asterisk or end of type
            if *token == Token::Asterisk {
                break;
            }
        }

        // If no type found, return empty
        if type_parts.is_empty() {
            return ("".to_string(), is_const);
        }

        // Join all type parts (for multi-word types like "unsigned int")
        let combined_type = type_parts.join(" ");
        (combined_type, is_const)
    }

    /// Convert C type to Rust type
    fn convert_c_type_to_rust(&self, c_type: &str) -> String {
        match c_type {
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
            "void" => "c_void".to_string(),
            "size_t" => "usize".to_string(),
            "ssize_t" => "isize".to_string(),
            "FILE" => "std::fs::File".to_string(),
            _ => c_type.to_string(),
        }
    }

    /// Process pointer variable declarations
    fn process_variables(&self, tokens: &[Token]) -> Result<String, ConversionError> {
        // Early return if we don't have enough tokens
        if tokens.len() < 3 {
            return Ok("".to_string());
        }

        // Extract basic type and check for const
        let (type_name, is_const) = self.extract_type(tokens);
        if type_name.is_empty() {
            return Ok("".to_string());
        }

        // Convert C type to Rust type
        let rust_type = self.convert_c_type_to_rust(&type_name);

        // Find variable name (comes after asterisk)
        let mut var_name = String::new();
        let mut ptr_count = 0;

        // First find the first asterisk
        if let Some(asterisk_pos) = tokens.iter().position(|t| *t == Token::Asterisk) {
            // Count consecutive asterisks for multiple pointer levels
            ptr_count = tokens[asterisk_pos..]
                .iter()
                .take_while(|t| **t == Token::Asterisk)
                .count();

            // Get the variable name (identifier after the asterisks)
            let var_pos = asterisk_pos + ptr_count;
            if var_pos < tokens.len() && matches!(tokens[var_pos], Token::Identifier(_)) {
                if let Token::Identifier(name) = &tokens[var_pos] {
                    var_name = name.clone();
                }
            }
        }

        if var_name.is_empty() {
            return Ok("".to_string()); // No variable name found
        }

        // Generate the Rust declaration
        let mut final_type = rust_type.to_string();
        for _ in 0..ptr_count {
            if is_const {
                final_type = format!("*const {}", final_type);
            } else {
                final_type = format!("*mut {}", final_type);
            }
        }

        // Generate declaration without initialization
        let declaration = if is_const {
            format!("let {}: {}", var_name, final_type)
        } else {
            format!("let mut {}: {}", var_name, final_type)
        };

        Ok(declaration)
    }
}

impl TokenHandler for PointerDeclarationHandler {
    fn can_handle(&self, tokens: &[Token], _context: &ParserContext) -> bool {
        self.can_process(tokens)
    }

    fn handle(
        &self,
        tokens: &[Token],
        _context: &mut ParserContext,
    ) -> Result<HandlerResult, ConversionError> {
        let result = self.process_variables(tokens)?;

        if !result.is_empty() {
            Ok(HandlerResult::RustCode(result))
        } else {
            Ok(HandlerResult::NotHandled)
        }
    }
}
