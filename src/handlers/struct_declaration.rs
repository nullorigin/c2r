use crate::error::ConversionError;
use crate::handler::{HandlerResult, ParserContext, TokenHandler};
use crate::token_parser::{Keyword, Token};

/// Handler for struct declarations, particularly those with arrays
pub struct StructDeclarationHandler;

impl StructDeclarationHandler {
    pub fn new() -> Self {
        Self {}
    }

    /// Check if token is "struct" (either as keyword or identifier)
    fn is_struct_token(&self, token: &Token) -> bool {
        match token {
            Token::Keyword(Keyword::Struct) => true,
            Token::Identifier(name) if name == "struct" => true,
            _ => false,
        }
    }

    /// Extract struct name from tokens (follows struct keyword)
    fn extract_name(&self, tokens: &[Token]) -> Option<String> {
        if tokens.len() >= 2 && self.is_struct_token(&tokens[0]) {
            if let Token::Identifier(name) = &tokens[1] {
                return Some(name.clone());
            }
        }
        None
    }

    /// Process struct variable declarations
    fn process_variables(
        &self,
        struct_name: &str,
        var_tokens: &[Token],
    ) -> Result<String, ConversionError> {
        let mut declarations = Vec::new();

        // Process each variable declaration (they're separated by commas)
        for var_part in var_tokens.split(|t| *t == Token::Comma) {
            if var_part.is_empty() {
                continue;
            }

            let mut name = String::new();
            let mut array_size = None;

            // Extract variable name and array size if present
            let mut i = 0;
            while i < var_part.len() {
                match &var_part[i] {
                    Token::Identifier(n) => {
                        name = n.clone();
                        i += 1;
                    }
                    Token::OpenBracket => {
                        // Find the array size between brackets
                        if i + 2 < var_part.len() && var_part[i + 2] == Token::CloseBracket {
                            if let Token::Number(size) = &var_part[i + 1] {
                                array_size = Some(size.clone());
                            }
                            i += 3; // Skip [size]
                        } else {
                            i += 1;
                        }
                    }
                    _ => i += 1,
                }
            }

            if name.is_empty() {
                continue;
            }

            // Generate the Rust declaration
            if let Some(size) = array_size {
                // For array declarations, create proper Rust array with Default initialization
                declarations.push(format!(
                    "let mut {}: [{struct_name}; {size}] = [Default::default(); {size}];",
                    name
                ));
            } else {
                // For single variable, use Default::default()
                declarations.push(format!(
                    "let mut {}: {struct_name} = Default::default();",
                    name
                ));
            }
        }

        Ok(declarations.join("\n    "))
    }
}

impl TokenHandler for StructDeclarationHandler {
    fn can_handle(&self, tokens: &[Token], _context: &ParserContext) -> bool {
        // Check if this is a struct declaration with either struct keyword or identifier
        tokens.len() >= 2 && self.is_struct_token(&tokens[0])
    }

    fn handle(
        &self,
        tokens: &[Token],
        _context: &mut ParserContext,
    ) -> Result<HandlerResult, ConversionError> {
        // Extract the struct name
        if let Some(struct_name) = self.extract_name(tokens) {
            // Skip the struct keyword and struct name
            let var_tokens = &tokens[2..];

            if !var_tokens.is_empty() {
                let rust_code = self.process_variables(&struct_name, var_tokens)?;
                return Ok(HandlerResult::RustCode(rust_code));
            }
        }

        // If we couldn't handle this properly
        Ok(HandlerResult::NotHandled)
    }
}
