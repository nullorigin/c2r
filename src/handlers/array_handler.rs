use crate::error::ConversionError;
use crate::handler::{HandlerResult, ParserContext, TokenHandler};
use crate::token_parser::Token;

/// Handler for array declarations and array access
/// Converts C-style array declarations and access to Rust syntax
/// Examples:
/// - int arr[10] -> let mut arr: [i32; 10] = [0; 10]
/// - arr[i] -> arr[i]
pub struct ArrayHandler;

impl ArrayHandler {
    pub fn new() -> Self {
        Self {}
    }

    /// Check if tokens represent an array declaration or access
    fn can_process(&self, tokens: &[Token]) -> bool {
        // Need at least 4 tokens for array declaration: type name [ size ]
        // or at least 4 tokens for array access: name [ index ]
        if tokens.len() < 4 {
            return false;
        }

        // Check for array declaration pattern: type identifier [ number ]
        for i in 0..tokens.len() - 3 {
            if matches!(tokens[i], Token::Identifier(_)) && // Type
                matches!(tokens[i+1], Token::Identifier(_)) && // Name
                tokens[i + 2] == Token::OpenBracket
            {
                return true;
            }
        }

        // Check for array access pattern: identifier [ expression ]
        for i in 0..tokens.len() - 2 {
            if matches!(tokens[i], Token::Identifier(_)) && // Array name
                tokens[i + 1] == Token::OpenBracket
            {
                // Look for closing bracket
                let mut j = i + 2;
                while j < tokens.len() {
                    if tokens[j] == Token::CloseBracket {
                        return true;
                    }
                    j += 1;
                }
            }
        }

        false
    }

    /// Process array declarations and access
    fn process(&self, tokens: &[Token]) -> Result<String, ConversionError> {
        // First check if it's an array declaration
        if self.is_array_declaration(tokens) {
            return self.process_array_declaration(tokens);
        }

        // Otherwise check if it's an array access
        if self.is_array_access(tokens) {
            return self.process_array_access(tokens);
        }

        Ok("".to_string()) // Not handled
    }

    /// Check if tokens represent an array declaration
    fn is_array_declaration(&self, tokens: &[Token]) -> bool {
        // We look for a pattern like: type name [ size ]
        if tokens.len() < 4 {
            return false;
        }

        for i in 0..tokens.len() - 3 {
            if matches!(tokens[i], Token::Identifier(_)) && // Type
                matches!(tokens[i+1], Token::Identifier(_)) && // Name
                tokens[i + 2] == Token::OpenBracket &&
                (matches!(tokens[i+3], Token::Number(_)) ||
                    matches!(tokens[i+3], Token::Identifier(_)))
            {
                // Size or variable

                // Look for closing bracket
                let mut j = i + 4;
                while j < tokens.len() {
                    if tokens[j] == Token::CloseBracket {
                        return true;
                    }
                    j += 1;
                }
            }
        }

        false
    }

    /// Process array declaration
    fn process_array_declaration(&self, tokens: &[Token]) -> Result<String, ConversionError> {
        let mut i = 0;

        // Find the type, name, and size
        while i < tokens.len() - 3 {
            if matches!(tokens[i], Token::Identifier(_)) && // Type
                matches!(tokens[i+1], Token::Identifier(_)) && // Name
                tokens[i + 2] == Token::OpenBracket
            {
                // Store the original C type name before conversion
                let c_type = if let Token::Identifier(name) = &tokens[i] {
                    name.clone()
                } else {
                    return Err(ConversionError::general("Expected type identifier"));
                };

                let type_name = self.convert_c_type_to_rust(&c_type);

                let var_name = match &tokens[i + 1] {
                    Token::Identifier(name) => name.clone(),
                    _ => return Err(ConversionError::general("Expected variable identifier")),
                };

                // Find the array size
                let mut j = i + 3;
                let mut size_str = String::new();
                while j < tokens.len() && tokens[j] != Token::CloseBracket {
                    match &tokens[j] {
                        Token::Number(num) => size_str.push_str(num),
                        Token::Identifier(id) => size_str.push_str(id),
                        Token::Plus => size_str.push('+'),
                        Token::Minus => size_str.push('-'),
                        Token::Asterisk => size_str.push('*'),
                        Token::Slash => size_str.push('/'),
                        Token::OpenParen => size_str.push('('),
                        Token::CloseParen => size_str.push(')'),
                        _ => {}
                    }
                    j += 1;
                }

                // Generate Rust array declaration
                // Special case for char arrays: use '\0' instead of 0
                let default_value = if c_type == "char" {
                    "'\\0'"
                } else {
                    match type_name.as_str() {
                        "i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64" | "isize"
                        | "usize" => "0",
                        "f32" | "f64" => "0.0",
                        "bool" => "false",
                        "char" => "'\\0'",
                        _ => "Default::default()",
                    }
                };

                return Ok(format!(
                    "let mut {}: [{}; {}] = [{}; {}];",
                    var_name, type_name, size_str, default_value, size_str
                ));
            }
            i += 1;
        }

        Ok("".to_string()) // Not handled
    }

    /// Check if tokens represent an array access
    fn is_array_access(&self, tokens: &[Token]) -> bool {
        if tokens.len() < 4 {
            // Need at least: name [ index ]
            return false;
        }

        for i in 0..tokens.len() - 2 {
            if matches!(tokens[i], Token::Identifier(_)) && // Array name
                tokens[i + 1] == Token::OpenBracket
            {
                // Look for closing bracket
                let mut j = i + 2;
                while j < tokens.len() {
                    if tokens[j] == Token::CloseBracket {
                        return true;
                    }
                    j += 1;
                }
            }
        }

        false
    }

    /// Process array access
    fn process_array_access(&self, tokens: &[Token]) -> Result<String, ConversionError> {
        let mut result = String::new();
        let mut i = 0;

        while i < tokens.len() {
            match &tokens[i] {
                Token::Identifier(id) => {
                    // Check if this identifier is followed by array access
                    if i + 1 < tokens.len() && tokens[i + 1] == Token::OpenBracket {
                        // This is an array access
                        result.push_str(id);
                        result.push('[');

                        // Extract index expression
                        let mut j = i + 2;
                        let mut index_expr = String::new();
                        let mut bracket_count = 1;

                        while j < tokens.len() && bracket_count > 0 {
                            match tokens[j] {
                                Token::OpenBracket => bracket_count += 1,
                                Token::CloseBracket => bracket_count -= 1,
                                _ => {}
                            }

                            if bracket_count > 0 {
                                // Add token to index expression
                                match &tokens[j] {
                                    Token::Identifier(idx) => index_expr.push_str(idx),
                                    Token::Number(num) => index_expr.push_str(num),
                                    Token::Plus => index_expr.push('+'),
                                    Token::Minus => index_expr.push('-'),
                                    Token::Asterisk => index_expr.push('*'),
                                    Token::Slash => index_expr.push('/'),
                                    Token::OpenParen => index_expr.push('('),
                                    Token::CloseParen => index_expr.push(')'),
                                    _ => {}
                                }
                            }
                            j += 1;
                        }

                        result.push_str(&index_expr);
                        result.push(']');

                        // Update position
                        i = j;
                    } else {
                        // Regular identifier
                        result.push_str(id);
                        i += 1;
                    }
                }
                other => {
                    // Handle other tokens by keeping them as is
                    result.push_str(&format!("{}", other));
                    i += 1;
                }
            }
        }

        Ok(result)
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
}

impl TokenHandler for ArrayHandler {
    fn can_handle(&self, tokens: &[Token], _context: &ParserContext) -> bool {
        self.can_process(tokens)
    }

    fn handle(
        &self,
        tokens: &[Token],
        _context: &mut ParserContext,
    ) -> Result<HandlerResult, ConversionError> {
        let result = self.process(tokens)?;

        if !result.is_empty() {
            Ok(HandlerResult::RustCode(result))
        } else {
            Ok(HandlerResult::NotHandled)
        }
    }
}
