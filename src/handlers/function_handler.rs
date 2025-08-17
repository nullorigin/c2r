use crate::error::ConversionError;
use crate::handler::{HandlerResult, ParserContext, TokenHandler};
use crate::token_parser::Token;

/// Handler for converting C functions to Rust functions
pub struct FunctionHandler;

impl FunctionHandler {
    pub fn new() -> Self {
        FunctionHandler
    }

    /// Map C type tokens to Rust type tokens
    fn map_type(&self, c_type: &[Token]) -> Vec<Token> {
        // This is a simplified implementation - would need to handle more complex type mapping
        let mut _rust_type: Vec<Token> = Vec::new();

        // Check for special cases
        if c_type.is_empty() {
            return vec![Token::Identifier("()".to_string())]; // void -> ()
        }

        if c_type.len() == 1 {
            if let Token::Identifier(ref id) = c_type[0] {
                match id.as_str() {
                    "void" => return vec![Token::Identifier("()".to_string())],
                    "int" => return vec![Token::Identifier("i32".to_string())],
                    "char" => return vec![Token::Identifier("i8".to_string())],
                    "float" => return vec![Token::Identifier("f32".to_string())],
                    "double" => return vec![Token::Identifier("f64".to_string())],
                    "short" => return vec![Token::Identifier("i16".to_string())],
                    "long" => return vec![Token::Identifier("i64".to_string())],
                    "size_t" => return vec![Token::Identifier("usize".to_string())],
                    _ => {}
                }
            }
        }

        // Handle unsigned types
        if c_type.len() >= 2 {
            if let (Token::Identifier(id1), Token::Identifier(id2)) = (&c_type[0], &c_type[1]) {
                match (id1.as_str(), id2.as_str()) {
                    ("unsigned", "int") => return vec![Token::Identifier("u32".to_string())],
                    ("unsigned", "char") => return vec![Token::Identifier("u8".to_string())],
                    ("unsigned", "short") => return vec![Token::Identifier("u16".to_string())],
                    ("unsigned", "long") => return vec![Token::Identifier("u64".to_string())],
                    ("const", "char") => {
                        // Special case for const char* (common C string pattern)
                        if c_type.len() > 2 && c_type[2] == Token::Asterisk {
                            return vec![Token::Identifier("*const i8".to_string())];
                        }
                    }
                    _ => {}
                }
            }
        }

        // Handle pointers
        let mut is_pointer = false;
        let mut is_const = false;
        let mut base_type = Vec::new();

        // First pass to detect const and pointers
        for i in 0..c_type.len() {
            match &c_type[i] {
                Token::Identifier(id) if id == "const" => {
                    is_const = true;
                }
                Token::Asterisk => {
                    is_pointer = true;
                }
                _ => {
                    // Only add non-const, non-pointer tokens to the base type
                    base_type.push(c_type[i].clone());
                }
            }
        }

        // Map the base type first
        let mapped_base_type = if base_type.len() == 1 {
            if let Token::Identifier(ref id) = base_type[0] {
                match id.as_str() {
                    "int" => vec![Token::Identifier("i32".to_string())],
                    "char" => vec![Token::Identifier("i8".to_string())],
                    "float" => vec![Token::Identifier("f32".to_string())],
                    "double" => vec![Token::Identifier("f64".to_string())],
                    "short" => vec![Token::Identifier("i16".to_string())],
                    "long" => vec![Token::Identifier("i64".to_string())],
                    "size_t" => vec![Token::Identifier("usize".to_string())],
                    _ => base_type.clone(),
                }
            } else {
                base_type.clone()
            }
        } else {
            base_type.clone()
        };

        // For now we'll just join tokens as a simplification
        // A proper implementation would handle more complex types
        let mut result = Vec::new();

        if is_pointer {
            if is_const {
                result.push(Token::Identifier("*const ".to_string()));
            } else {
                result.push(Token::Identifier("*mut ".to_string()));
            }
        }

        // Add the mapped base type tokens
        result.extend(mapped_base_type);

        result
    }

    /// Extract and convert a function from tokens
    fn extract_and_convert_function(
        &self,
        tokens: &[Token],
    ) -> Result<HandlerResult, ConversionError> {
        // Find function name
        let mut name_pos = 0;
        for i in 0..tokens.len() {
            if tokens[i] == Token::OpenParen {
                if i > 0 {
                    // The token before the opening parenthesis should be the function name
                    if let Token::Identifier(_) = tokens[i - 1] {
                        name_pos = i - 1;
                        break;
                    }
                }
            }
        }

        // If no function name was found, this is not a function
        if name_pos == 0 {
            return Ok(HandlerResult::NotHandled);
        }

        // Get function name
        let name = if let Token::Identifier(ref id) = tokens[name_pos] {
            id.clone()
        } else {
            return Ok(HandlerResult::NotHandled);
        };

        // Extract return type (tokens before function name)
        let mut return_type = Vec::new();
        let mut is_static = false;

        for i in 0..name_pos {
            match &tokens[i] {
                Token::Identifier(id) if id == "static" => {
                    is_static = true;
                }
                Token::Identifier(id) if id == "inline" => {
                    // Skip inline keyword
                }
                _ => {
                    return_type.push(tokens[i].clone());
                }
            }
        }

        // Find parameters
        let param_start = name_pos + 1;
        let mut param_end = param_start + 1;
        let mut param_tokens = Vec::new();

        // Count parentheses to find the closing one
        let mut paren_count = 1;

        while param_end < tokens.len() && paren_count > 0 {
            match tokens[param_end] {
                Token::OpenParen => paren_count += 1,
                Token::CloseParen => paren_count -= 1,
                _ => {}
            }

            if paren_count > 0 {
                param_tokens.push(tokens[param_end].clone());
            }
            param_end += 1;
        }

        // Convert parameters
        let parameters = self.parse_parameters(&param_tokens)?;

        // Check for function body
        let mut has_body = false;
        let mut body_start = param_end;

        while body_start < tokens.len() {
            if tokens[body_start] == Token::OpenBrace {
                has_body = true;
                break;
            }
            body_start += 1;
        }

        // Generate Rust function signature
        let mut rust_code = String::new();

        // Add visibility and other qualifiers
        if is_static {
            rust_code.push_str("fn ");
        } else {
            rust_code.push_str("pub fn ");
        }

        // Add function name
        rust_code.push_str(&name);

        // Add parameters
        rust_code.push('(');
        for (i, (param_name, param_type)) in parameters.iter().enumerate() {
            // Map the C type to Rust type
            let rust_type = self.map_type(param_type);
            let type_str = rust_type
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<String>>()
                .join("");

            rust_code.push_str(&format!("{}: {}", param_name, type_str));

            if i < parameters.len() - 1 {
                rust_code.push_str(", ");
            }
        }
        rust_code.push(')');

        // Add return type if not void
        if !return_type.is_empty() {
            let rust_return_type = self.map_type(&return_type);
            let type_str = rust_return_type
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<String>>()
                .join("");

            if type_str != "()" {
                rust_code.push_str(&format!(" -> {}", type_str));
            }
        }

        // If there's a body, add a placeholder for now
        if has_body {
            rust_code.push_str(" {\n    // Function body conversion not yet implemented\n}");
        } else {
            rust_code.push(';');
        }

        Ok(HandlerResult::RustCode(rust_code))
    }

    /// Parse function parameters
    fn parse_parameters(
        &self,
        tokens: &[Token],
    ) -> Result<Vec<(String, Vec<Token>)>, ConversionError> {
        if tokens.is_empty() {
            return Ok(Vec::new());
        }

        let mut parameters = Vec::new();
        let mut param_start = 0;

        // Split by commas
        for i in 0..tokens.len() {
            if tokens[i] == Token::Comma {
                if i > param_start {
                    // Extract one parameter
                    if let Some((name, param_type)) =
                        self.parse_parameter(&tokens[param_start..i])?
                    {
                        parameters.push((name, param_type));
                    }
                }
                param_start = i + 1;
            }
        }

        // Handle the last parameter
        if param_start < tokens.len() {
            if let Some((name, param_type)) = self.parse_parameter(&tokens[param_start..])? {
                parameters.push((name, param_type));
            }
        }

        Ok(parameters)
    }

    /// Parse a single parameter
    fn parse_parameter(
        &self,
        tokens: &[Token],
    ) -> Result<Option<(String, Vec<Token>)>, ConversionError> {
        if tokens.is_empty() {
            return Ok(None);
        }

        // Look for parameter name (usually the last identifier)
        let mut name = String::new();
        let mut name_pos = 0;

        // Handle special case for "void" parameter
        if tokens.len() == 1 {
            if let Token::Identifier(ref id) = tokens[0] {
                if id == "void" {
                    return Ok(None); // void parameter means no parameters
                }
            }
        }

        // Find parameter name
        for i in (0..tokens.len()).rev() {
            if let Token::Identifier(ref id) = tokens[i] {
                // Skip type keywords
                let type_keywords = [
                    "int", "char", "float", "double", "void", "short", "long", "unsigned",
                    "signed", "const", "volatile", "struct", "enum", "union",
                ];

                if !type_keywords.contains(&id.as_str()) {
                    name = id.clone();
                    name_pos = i;
                    break;
                }
            }
        }

        // If we couldn't find a name, use a generated one
        if name.is_empty() {
            name = "param".to_string();
            name_pos = tokens.len();
        }

        // Extract parameter type (all tokens before the name)
        let param_type = if name_pos > 0 {
            tokens[0..name_pos].to_vec()
        } else {
            Vec::new()
        };

        Ok(Some((name, param_type)))
    }
}

impl TokenHandler for FunctionHandler {
    fn can_handle(&self, tokens: &[Token], _context: &ParserContext) -> bool {
        // Check if this looks like a function declaration or definition
        if tokens.len() < 3 {
            return false;
        }

        // Look for pattern: [tokens] identifier(
        for i in 1..tokens.len() - 1 {
            if let Token::Identifier(_) = tokens[i] {
                if tokens[i + 1] == Token::OpenParen {
                    // Make sure this isn't a function call within an expression
                    // by checking that it's at the start of a statement
                    if i == 0 || self.is_likely_function_start(&tokens[0..i]) {
                        return true;
                    }
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
        self.extract_and_convert_function(tokens)
    }
}

impl FunctionHandler {
    /// Check if tokens are likely to be the start of a function declaration
    /// This is a heuristic and not exhaustive
    fn is_likely_function_start(&self, tokens: &[Token]) -> bool {
        // Commonly used type and modifier keywords at the start of function declarations
        let type_keywords = [
            "void", "int", "char", "float", "double", "short", "long", "unsigned", "signed",
            "const", "static", "inline", "struct", "enum", "union", "auto", "register", "extern",
        ];

        for token in tokens {
            if let Token::Identifier(id) = token {
                if type_keywords.contains(&id.as_str()) {
                    return true;
                }
            }
        }

        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_function_handler_simple_function() {
        let c_code = "int add(int a, int b) { return a + b; }";
        let mut tokenizer = crate::token_parser::Tokenizer::new(c_code);
        let tokens = tokenizer.tokenize();

        let handler = FunctionHandler::new();
        let context = &mut ParserContext::new();

        assert!(handler.can_handle(&tokens, context));

        if let HandlerResult::RustCode(rust_code) = handler.handle(&tokens, context).unwrap() {
            assert!(rust_code.contains("pub fn add"));
            assert!(rust_code.contains("a: i32"));
            assert!(rust_code.contains("b: i32"));
            assert!(rust_code.contains("-> i32"));
        } else {
            panic!("Expected RustCode result");
        }
    }

    #[test]
    fn test_function_handler_void_function() {
        let c_code = "void print_message(const char* msg) { }";
        let mut tokenizer = crate::token_parser::Tokenizer::new(c_code);
        let tokens = tokenizer.tokenize();

        let handler = FunctionHandler::new();
        let context = &mut ParserContext::new();

        assert!(handler.can_handle(&tokens, context));

        if let HandlerResult::RustCode(rust_code) = handler.handle(&tokens, context).unwrap() {
            assert!(rust_code.contains("pub fn print_message"));
            assert!(rust_code.contains("msg: *const i8"));
            assert!(!rust_code.contains("->"));
        } else {
            panic!("Expected RustCode result");
        }
    }
}
