use crate::error::ConversionError;
use crate::handler::{HandlerResult, ParserContext, TokenHandler};
use crate::token_parser::Token;

/// Handler for C-style type casting expressions: (type)expr -> expr as type
pub struct TypeCastHandler;

impl TypeCastHandler {
    /// Create a new type casting handler
    pub fn new() -> Self {
        Self {}
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

    /// Check if a token represents the 'const' keyword
    fn is_const_token(&self, token: &Token) -> bool {
        if let Token::Identifier(id) = token {
            id == "const"
        } else {
            false
        }
    }

    /// Check if a token represents a type (approximation based on common C types)
    fn is_type_token(&self, token: &Token) -> bool {
        if let Token::Identifier(id) = token {
            matches!(
                id.as_str(),
                "int"
                    | "char"
                    | "float"
                    | "double"
                    | "void"
                    | "long"
                    | "short"
                    | "unsigned"
                    | "signed"
                    | "FILE"
                    | "size_t"
                    | "ssize_t"
            )
        } else {
            false
        }
    }

    /// Check if a token is an asterisk (pointer indicator)
    fn is_star_token(&self, token: &Token) -> bool {
        *token == Token::Asterisk
    }

    /// Determine the mutability of a cast (mut or const)
    fn determine_mutability(&self, type_tokens: &[Token]) -> &'static str {
        // Check if the type cast includes "const"
        if type_tokens.iter().any(|t| self.is_const_token(t)) {
            "*const"
        } else {
            "*mut"
        }
    }

    /// Extract the complete type from tokens, handling compound types like "unsigned int"
    fn extract_type(&self, type_tokens: &[Token]) -> String {
        let mut type_parts = Vec::new();

        // Collect all type-related tokens
        for token in type_tokens {
            if self.is_type_token(token) {
                if let Token::Identifier(id) = token {
                    type_parts.push(id.clone());
                }
            }
        }

        // Handle compound types
        if type_parts.len() > 1 {
            let compound_type = type_parts.join(" ");
            return self.convert_c_type_to_rust(&compound_type);
        } else if !type_parts.is_empty() {
            return self.convert_c_type_to_rust(&type_parts[0]);
        }

        "unknown_type".to_string()
    }

    /// Convert tokens to string representation for expression with proper formatting
    fn tokens_to_string(&self, tokens: &[Token]) -> String {
        let mut result = String::new();
        let mut i = 0;

        while i < tokens.len() {
            let token = &tokens[i];

            match token {
                Token::Identifier(id) => result.push_str(id),
                Token::Number(num) => result.push_str(num),
                Token::OpenParen => result.push('('),
                Token::CloseParen => result.push(')'),
                Token::OpenBrace => result.push('{'),
                Token::CloseBrace => result.push('}'),
                Token::OpenBracket => result.push('['),
                Token::CloseBracket => result.push(']'),
                Token::Semicolon => result.push(';'),
                Token::Comma => result.push_str(", "),
                Token::Asterisk => result.push('*'),
                Token::Equal => result.push_str(" = "),
                Token::DoubleEqual => result.push_str(" == "),
                Token::Plus => {
                    if i > 0 && tokens[i - 1] == Token::OpenParen {
                        result.push('+');
                    } else {
                        result.push_str(" + ");
                    }
                }
                Token::Minus => {
                    if i > 0 && tokens[i - 1] == Token::OpenParen {
                        result.push('-');
                    } else {
                        result.push_str(" - ");
                    }
                }
                Token::Slash => result.push_str(" / "),
                Token::Arrow => result.push_str("->"),
                Token::Dot => result.push('.'),
                Token::Ampersand => result.push('&'),
                Token::Pipe => result.push('|'),
                Token::LogicalAnd => result.push_str(" && "),
                Token::LogicalOr => result.push_str(" || "),

                // Special handling for bit shift operators (>> and <<)
                Token::GreaterThan => {
                    if i + 1 < tokens.len() && tokens[i + 1] == Token::GreaterThan {
                        // This is a >> operator
                        result.push_str(" >> ");
                        i += 1; // Skip the next '>' token since we've handled it
                    } else {
                        result.push_str(" > ");
                    }
                }
                Token::LessThan => {
                    if i + 1 < tokens.len() && tokens[i + 1] == Token::LessThan {
                        // This is a << operator
                        result.push_str(" << ");
                        i += 1; // Skip the next '<' token since we've handled it
                    } else {
                        result.push_str(" < ");
                    }
                }

                Token::StringLiteral(s) => {
                    result.push('"');
                    result.push_str(s);
                    result.push('"');
                }
                _ => result.push_str(&format!("{}", token)),
            }

            i += 1;
        }

        result
    }
}

impl TokenHandler for TypeCastHandler {
    fn can_handle(&self, tokens: &[Token], _context: &ParserContext) -> bool {
        // We need at least 4 tokens: (, type, ), and expression
        if tokens.len() < 4 {
            return false;
        }

        // Check for opening parenthesis
        if tokens[0] != Token::OpenParen {
            return false;
        }

        // Find matching closing parenthesis
        let mut paren_count = 1;
        let mut close_index = 0;

        for i in 1..tokens.len() {
            match tokens[i] {
                Token::OpenParen => paren_count += 1,
                Token::CloseParen => {
                    paren_count -= 1;
                    if paren_count == 0 {
                        close_index = i;
                        break;
                    }
                }
                _ => {}
            }
        }

        // Verify we found a matching closing parenthesis and there's content after it
        if close_index > 0 && close_index < tokens.len() - 1 {
            // Verify there's a type within the parentheses
            for i in 1..close_index {
                if self.is_type_token(&tokens[i]) {
                    return true;
                }
            }
        }

        false
    }

    fn handle(
        &self,
        tokens: &[Token],
        _context: &mut ParserContext,
    ) -> Result<HandlerResult, ConversionError> {
        // Find the closing parenthesis of the type cast
        let mut paren_count = 1;
        let mut close_index = 0;

        for i in 1..tokens.len() {
            match tokens[i] {
                Token::OpenParen => paren_count += 1,
                Token::CloseParen => {
                    paren_count -= 1;
                    if paren_count == 0 {
                        close_index = i;
                        break;
                    }
                }
                _ => {}
            }
        }

        if close_index == 0 || close_index == tokens.len() - 1 {
            return Ok(HandlerResult::NotHandled);
        }

        // Extract the type tokens and expression tokens
        let type_tokens = &tokens[1..close_index];
        let expr_tokens = &tokens[(close_index + 1)..];

        // Build the expression part with proper formatting
        let expr = self.tokens_to_string(expr_tokens);

        // Check for pointer cast
        let is_pointer_cast = type_tokens.iter().any(|t| self.is_star_token(t));

        // Handle different types of casts
        if is_pointer_cast {
            // Handle pointer cast: (type*) -> as *mut/const type
            let mut_or_const = self.determine_mutability(type_tokens);
            let base_type = self.extract_type(type_tokens);

            Ok(HandlerResult::RustCode(format!(
                "{} as {} {}",
                expr, mut_or_const, base_type
            )))
        } else {
            // Handle simple type cast: (type) -> as type
            let type_str = self.extract_type(type_tokens);

            if type_str.is_empty() || type_str == "unknown_type" {
                return Ok(HandlerResult::NotHandled);
            }

            Ok(HandlerResult::RustCode(format!("{} as {}", expr, type_str)))
        }
    }
}
