use crate::error::ConversionError;
use crate::handler::{HandlerResult, ParserContext, TokenHandler};
use crate::token_parser::Token;

/// Handler for C preprocessor #define directives
/// Converts simple #define statements to Rust pub const declarations
/// and complex expressions to macro_rules! macros
///
/// Example:
/// - #define MAX_SIZE 100 -> pub const MAX_SIZE: i32 = 100;
/// - #define VERSION "1.0" -> pub const VERSION: &'static str = "1.0";
/// - #define DEBUG 1 -> pub const DEBUG: bool = true;
/// - #define MAX(a, b) ((a) > (b) ? (a) : (b)) -> macro_rules! MAX { ($a:expr, $b:expr) => { if $a > $b { $a } else { $b } }; }
/// - #define SQUARE(x) ((x) * (x)) -> macro_rules! SQUARE { ($x:expr) => { $x * $x }; }
pub struct DefineHandler;

impl DefineHandler {
    pub fn new() -> Self {
        Self {}
    }

    /// Check if tokens represent a #define directive
    fn can_process(&self, tokens: &[Token]) -> bool {
        if tokens.len() < 3 {
            return false;
        }

        // Look for #define pattern: # define NAME VALUE
        if let Token::Hash = tokens[0] {
            if let Token::Identifier(ident) = &tokens[1] {
                return ident == "define" && tokens.len() >= 3;
            }
        }

        false
    }

    /// Process a #define directive and convert it to a Rust constant or macro
    fn process_expression(&self, tokens: &[Token]) -> Result<String, ConversionError> {
        // Skip the # define tokens
        let directive_tokens = &tokens[2..];
        if directive_tokens.is_empty() {
            return Err(ConversionError::general(
                "Invalid #define directive: missing name",
            ));
        }

        // Extract the name
        let name = if let Token::Identifier(name) = &directive_tokens[0] {
            name
        } else {
            return Err(ConversionError::general(
                "Invalid #define directive: name must be an identifier",
            ));
        };

        // Check if this looks like a macro function with parameters: NAME(param1, param2)
        if directive_tokens.len() > 1 {
            if let Token::OpenParen = directive_tokens[1] {
                // This is likely a parameterized macro
                return self.process_parameterized_macro(&directive_tokens);
            }
        }

        // Handle complex expressions as macro_rules! if they contain operators or multiple tokens
        if directive_tokens.len() > 2 && self.is_complex_expression(&directive_tokens[1..]) {
            return self.process_complex_macro(name, &directive_tokens[1..]);
        }

        // Handle simple case - no value provided (typically used for feature flags)
        if directive_tokens.len() == 1 {
            return Ok(format!("pub const {}: bool = true;", name));
        }

        // Extract the constant value and determine its type
        let value_tokens = &directive_tokens[1..];
        let (value, type_name) = self.determine_value_and_type(value_tokens)?;

        Ok(format!("pub const {}: {} = {};", name, type_name, value))
    }

    /// Check if the tokens represent a complex expression (with operators, etc.)
    fn is_complex_expression(&self, tokens: &[Token]) -> bool {
        // Consider it complex if it contains operators or multiple tokens of different types
        for token in tokens {
            match token {
                Token::Plus
                | Token::Minus
                | Token::Asterisk
                | Token::Slash
                | Token::Ampersand
                | Token::Pipe
                | Token::LogicalAnd
                | Token::LogicalOr
                | Token::LessThan
                | Token::GreaterThan
                | Token::GreaterEqual
                | Token::LessEqual
                | Token::OpenParen => return true,
                _ => {}
            }
        }

        // Also consider it complex if there are multiple tokens of different types
        if tokens.len() > 1 {
            let first_token_type = std::mem::discriminant(&tokens[0]);
            for token in &tokens[1..] {
                if std::mem::discriminant(token) != first_token_type {
                    return true;
                }
            }
        }

        false
    }

    /// Process a complex expression into a macro_rules! macro
    fn process_complex_macro(
        &self,
        name: &str,
        tokens: &[Token],
    ) -> Result<String, ConversionError> {
        // Convert the tokens to a string representation for the macro body
        let expr = tokens
            .iter()
            .map(|t| format!("{}", t))
            .collect::<Vec<String>>()
            .join(" ");

        // Format as a simple macro_rules! with no parameters
        Ok(format!("macro_rules! {} {{ () => {{ {} }}; }}", name, expr))
    }

    /// Process a parameterized macro function (not implemented yet)
    fn process_parameterized_macro(&self, _tokens: &[Token]) -> Result<String, ConversionError> {
        Err(ConversionError::general(
            "Parameterized macros are not yet supported",
        ))
    }

    /// Determine the value and type for a Rust constant
    fn determine_value_and_type(
        &self,
        tokens: &[Token],
    ) -> Result<(String, String), ConversionError> {
        if tokens.is_empty() {
            return Ok(("true".to_string(), "bool".to_string()));
        }

        match &tokens[0] {
            Token::Number(num) => {
                // Check if it's an integer or float
                if num.contains('.') {
                    Ok((num.clone(), "f64".to_string()))
                } else {
                    // Check if it's a boolean (0/1)
                    if num == "0" {
                        Ok(("false".to_string(), "bool".to_string()))
                    } else if num == "1" {
                        Ok(("true".to_string(), "bool".to_string()))
                    } else {
                        Ok((num.clone(), "i32".to_string()))
                    }
                }
            }
            Token::StringLiteral(s) => Ok((format!("\"{}\"", s), "&'static str".to_string())),
            Token::Identifier(id) => {
                // Handle special cases
                match id.as_str() {
                    "NULL" => Ok((
                        "std::ptr::null()".to_string(),
                        "*const std::ffi::c_void".to_string(),
                    )),
                    "true" | "false" => Ok((id.clone(), "bool".to_string())),
                    _ => {
                        // Default to using the identifier as-is
                        // This is for cases like #define X Y where Y is another constant
                        Ok((id.clone(), "/* unknown type */".to_string()))
                    }
                }
            }
            _ => {
                // For more complex expressions, collect all tokens
                let value = tokens
                    .iter()
                    .map(|t| format!("{}", t))
                    .collect::<Vec<String>>()
                    .join(" ");

                // Try to infer a type, default to i32 for expressions
                Ok((value, "i32".to_string()))
            }
        }
    }
}

impl TokenHandler for DefineHandler {
    fn can_handle(&self, tokens: &[Token], _context: &ParserContext) -> bool {
        self.can_process(tokens)
    }

    fn handle(
        &self,
        tokens: &[Token],
        _context: &mut ParserContext,
    ) -> Result<HandlerResult, ConversionError> {
        let result = self.process_expression(tokens)?;

        if !result.is_empty() {
            Ok(HandlerResult::RustCode(result))
        } else {
            Ok(HandlerResult::NotHandled)
        }
    }
}
