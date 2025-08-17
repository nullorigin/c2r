use crate::error::ConversionError;
use crate::handler::{HandlerResult, ParserContext, TokenHandler};
use crate::token_parser::{Keyword, Token};

/// Handler for C enum declarations
/// Converts C enums to Rust enums with appropriate representation
///
/// Example:
/// ```c
/// enum Color { RED, GREEN, BLUE };
/// ```
///
/// Becomes:
/// ```rust
/// #[repr(C)]
/// enum Color {
///     RED = 0,
///     GREEN = 1,
///     BLUE = 2,
/// }
/// ```
///
/// If a specific representation is needed:
/// ```c
/// enum Status : uint8_t { OK, ERROR };
/// // or
/// typedef enum { OK, ERROR } Status;
/// uint8_t status = OK;
/// ```
///
/// Becomes:
/// ```rust
/// #[repr(u8)]
/// enum Status {
///     OK = 0,
///     ERROR = 1,
/// }
/// ```
pub struct EnumHandler;

impl EnumHandler {
    pub fn new() -> Self {
        Self {}
    }

    /// Check if token is "enum" (either as keyword or identifier)
    fn is_enum_token(&self, token: &Token) -> bool {
        match token {
            Token::Keyword(Keyword::Enum) => true,
            Token::Identifier(name) if name == "enum" => true,
            _ => false,
        }
    }

    /// Extract the name of the enum from the tokens
    fn extract_enum_name(&self, tokens: &[Token]) -> Result<String, ConversionError> {
        // Find the 'enum' keyword
        let enum_index = tokens
            .iter()
            .position(|t| match t {
                Token::Identifier(name) => name == "enum",
                _ => false,
            })
            .ok_or(ConversionError::parse_error(
                "",
                0,
                0,
                "No enum keyword found",
            ))?;

        // The identifier after the 'enum' keyword is the name
        if enum_index + 1 < tokens.len() {
            if let Token::Identifier(name) = &tokens[enum_index + 1] {
                return Ok(name.clone());
            }
        }

        Err(ConversionError::parse_error(
            "",
            0,
            0,
            "Could not extract enum name",
        ))
    }

    /// Determine the representation to use based on tokens and context
    fn determine_representation(&self, tokens: &[Token], context: &ParserContext) -> String {
        // Look for explicit representation like "enum Color : uint8_t"
        let mut i = 0;
        while i < tokens.len() {
            if tokens[i] == Token::Colon && i + 1 < tokens.len() {
                return self.type_to_repr(&tokens[i + 1..]);
            }
            i += 1;
        }

        // Look for usage in the context to determine representation
        if let Some(enum_type) = context.get("enum_type") {
            return self.map_type_to_repr(enum_type);
        }

        // Default to C representation
        "C".to_string()
    }

    /// Map a C type token sequence to a Rust representation
    fn type_to_repr(&self, tokens: &[Token]) -> String {
        let type_str = tokens
            .iter()
            .take_while(|t| **t != Token::OpenBrace && **t != Token::Semicolon)
            .filter_map(|t| match t {
                Token::Identifier(s) => Some(s.as_str()),
                _ => None,
            })
            .collect::<Vec<&str>>()
            .join(" ");

        self.map_type_to_repr(&type_str)
    }

    /// Map a type string to a Rust representation attribute
    fn map_type_to_repr(&self, type_str: &str) -> String {
        match type_str.trim() {
            "uint8_t" | "unsigned char" => "u8".to_string(),
            "int8_t" | "char" => "i8".to_string(),
            "uint16_t" | "unsigned short" => "u16".to_string(),
            "int16_t" | "short" => "i16".to_string(),
            "uint32_t" | "unsigned int" => "u32".to_string(),
            "int32_t" | "int" => "i32".to_string(),
            "uint64_t" | "unsigned long" | "unsigned long long" => "u64".to_string(),
            "int64_t" | "long" | "long long" => "i64".to_string(),
            _ => "C".to_string(), // Default to C representation
        }
    }

    /// Process the body of the enum definition
    fn process_enum_body(&self, tokens: &[Token]) -> Result<String, ConversionError> {
        // Find the opening brace
        let brace_index = tokens.iter().position(|t| *t == Token::OpenBrace).ok_or(
            ConversionError::parse_error("", 0, 0, "No opening brace found in enum definition"),
        )?;

        // Find the closing brace
        let close_brace_index = tokens.iter().position(|t| *t == Token::CloseBrace).ok_or(
            ConversionError::parse_error("", 0, 0, "No closing brace found in enum definition"),
        )?;

        if close_brace_index <= brace_index {
            return Err(ConversionError::parse_error(
                "",
                0,
                0,
                "Invalid enum body structure",
            ));
        }

        // Extract the enum body tokens between the braces
        let body_tokens = &tokens[brace_index + 1..close_brace_index];

        // Process each enum member
        let mut result = String::new();
        let mut current_tokens = Vec::new();
        let mut current_value = None;
        let mut members = Vec::new();

        for token in body_tokens {
            if *token == Token::Comma {
                // End of an enum member
                if !current_tokens.is_empty() {
                    let member = self.process_enum_member(&current_tokens, &mut current_value)?;
                    members.push(member);
                    current_tokens.clear();
                }
            } else {
                current_tokens.push(token.clone());
            }
        }

        // Process the last member if there's no trailing comma
        if !current_tokens.is_empty() {
            let member = self.process_enum_member(&current_tokens, &mut current_value)?;
            members.push(member);
        }

        // Format the members with commas
        for (i, member) in members.iter().enumerate() {
            result.push_str(member);
            if i < members.len() - 1 {
                result.push_str(",\n");
            }
        }

        Ok(result)
    }

    /// Process a single enum member
    fn process_enum_member(
        &self,
        tokens: &[Token],
        current_value: &mut Option<i32>,
    ) -> Result<String, ConversionError> {
        if tokens.is_empty() {
            return Err(ConversionError::parse_error("", 0, 0, "Empty enum member"));
        }

        // Get the enum member name
        let name = if let Token::Identifier(name) = &tokens[0] {
            name
        } else {
            return Err(ConversionError::parse_error(
                "",
                0,
                0,
                &format!("Expected identifier, found {:?}", tokens[0]),
            ));
        };

        // Check if there's an explicit value assignment
        let mut value_str = String::new();
        let mut has_explicit_value = false;

        for i in 1..tokens.len() {
            if tokens[i] == Token::Equal && i + 1 < tokens.len() {
                // Extract the value after the equals sign
                match &tokens[i + 1] {
                    Token::Number(num) => {
                        // Parse the value
                        let value = if num.starts_with("0x") || num.starts_with("0X") {
                            i32::from_str_radix(&num[2..], 16).map_err(|_| {
                                ConversionError::parse_error(
                                    "",
                                    0,
                                    0,
                                    &format!("Invalid hex value: {}", num),
                                )
                            })?
                        } else {
                            num.parse::<i32>().map_err(|_| {
                                ConversionError::parse_error(
                                    "",
                                    0,
                                    0,
                                    &format!("Invalid numeric value: {}", num),
                                )
                            })?
                        };

                        *current_value = Some(value);
                        value_str = format!(" = {}", value);
                        has_explicit_value = true;
                        break;
                    }
                    _ => {
                        return Err(ConversionError::parse_error(
                            "",
                            0,
                            0,
                            &format!("Expected numeric value, found {:?}", tokens[i + 1]),
                        ));
                    }
                }
            }
        }

        // If no explicit value, use the next sequential value
        if !has_explicit_value {
            let value = match *current_value {
                Some(val) => {
                    let new_val = val + 1;
                    *current_value = Some(new_val);
                    new_val
                }
                None => {
                    // First member without explicit value starts at 0
                    *current_value = Some(0);
                    0
                }
            };
            value_str = format!(" = {}", value);
        }

        Ok(format!("    {}{}", name, value_str))
    }
}

impl TokenHandler for EnumHandler {
    fn can_handle(&self, tokens: &[Token], _context: &ParserContext) -> bool {
        // Check if this is an enum declaration
        tokens.len() >= 2 && self.is_enum_token(&tokens[0])
    }

    fn handle(
        &self,
        tokens: &[Token],
        context: &mut ParserContext,
    ) -> Result<HandlerResult, ConversionError> {
        // Make sure this is an enum declaration
        if !self.can_handle(tokens, context) {
            return Ok(HandlerResult::NotHandled);
        }

        // Extract the enum name
        let name = self.extract_enum_name(tokens)?;

        // Determine the representation based on any explicit type specification
        let representation = self.determine_representation(tokens, context);

        // Process the enum body to generate Rust enum variants
        let body = self.process_enum_body(tokens)?;

        // Build the Rust enum declaration with representation attribute
        let rust_code = format!(
            "#[repr({})]\nenum {} {{\n{}\n}}",
            representation, name, body
        );

        Ok(HandlerResult::RustCode(rust_code))
    }
}
