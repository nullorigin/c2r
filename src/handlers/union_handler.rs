use crate::error::ConversionError;
use crate::handler::{HandlerResult, ParserContext, TokenHandler};
use crate::token_parser::{Keyword, Token};

/// Handler for C union declarations
/// Converts C unions to Rust unions with unsafe accessors
///
/// Example:
/// ```c
/// union Data {
///     int i;
///     float f;
///     char str[20];
/// };
/// ```
///
/// Becomes:
/// ```rust
/// #[repr(C)]
/// union Data {
///     i: i32,
///     f: f32,
///     str: [i8; 20],
/// }
///
/// // With safe accessor methods
/// impl Data {
///     pub fn get_i(&self) -> i32 {
///         unsafe { self.i }
///     }
///     
///     pub fn set_i(&mut self, val: i32) {
///         unsafe { self.i = val; }
///     }
///     
///     // Similar methods for other fields
/// }
/// ```
pub struct UnionHandler;

impl UnionHandler {
    pub fn new() -> Self {
        Self {}
    }

    /// Check if token is "union" (either as keyword or identifier)
    fn is_union_token(&self, token: &Token) -> bool {
        match token {
            Token::Keyword(Keyword::Union) => true,
            Token::Identifier(name) if name == "union" => true,
            _ => false,
        }
    }

    /// Extract the name of the union from the tokens
    fn extract_union_name(&self, tokens: &[Token]) -> Result<String, ConversionError> {
        // Find the 'union' keyword
        let union_index = tokens
            .iter()
            .position(|t| match t {
                Token::Identifier(name) => name == "union",
                _ => false,
            })
            .ok_or(ConversionError::parse_error(
                "",
                0,
                0,
                "No union keyword found",
            ))?;

        // The identifier after the 'union' keyword is the name
        if union_index + 1 < tokens.len() {
            if let Token::Identifier(name) = &tokens[union_index + 1] {
                return Ok(name.clone());
            }
        }

        Err(ConversionError::parse_error(
            "",
            0,
            0,
            "Could not extract union name",
        ))
    }

    /// Check for any alignment or packing attributes
    fn determine_attributes(&self, tokens: &[Token], context: &ParserContext) -> Vec<String> {
        let mut attributes = vec!["#[repr(C)]".to_string()];

        // Look for __attribute__((packed)) or #pragma pack directives in the context
        if let Some(pack) = context.get("pack") {
            if let Ok(pack_value) = pack.parse::<usize>() {
                if pack_value > 0 {
                    attributes.push(format!("#[repr(packed({})]", pack_value));
                } else {
                    attributes.push("#[repr(packed)]".to_string());
                }
            }
        }

        // Check for __attribute__((aligned(N))) in the tokens
        let attr_pos = tokens.iter().position(|t| {
            if let Token::Identifier(name) = t {
                name == "__attribute__"
            } else {
                false
            }
        });

        if let Some(pos) = attr_pos {
            // Simple check for aligned attribute with a number
            for i in pos..tokens.len() {
                if let Token::Identifier(name) = &tokens[i] {
                    if name == "aligned" && i + 3 < tokens.len() {
                        if let Token::Number(align_value) = &tokens[i + 2] {
                            attributes.push(format!("#[repr(align({}))]", align_value));
                        }
                    }
                }
            }
        }

        attributes
    }

    /// Process the body of the union definition to extract fields and create accessors
    fn process_union_body(
        &self,
        tokens: &[Token],
    ) -> Result<(String, Vec<(String, String)>), ConversionError> {
        // Find the opening brace
        let brace_index = tokens.iter().position(|t| *t == Token::OpenBrace).ok_or(
            ConversionError::parse_error("", 0, 0, "No opening brace found in union definition"),
        )?;

        // Find the closing brace
        let close_brace_index = tokens.iter().position(|t| *t == Token::CloseBrace).ok_or(
            ConversionError::parse_error("", 0, 0, "No closing brace found in union definition"),
        )?;

        if close_brace_index <= brace_index {
            return Err(ConversionError::parse_error(
                "",
                0,
                0,
                "Invalid union body structure",
            ));
        }

        // Extract the union body tokens between the braces
        let body_tokens = &tokens[brace_index + 1..close_brace_index];

        // Process each union member field
        let mut result = String::new();
        let mut current_tokens = Vec::new();
        let mut members = Vec::new();
        let mut field_info = Vec::new();

        for token in body_tokens {
            if *token == Token::Semicolon {
                // End of a union member
                if !current_tokens.is_empty() {
                    let (field, info) = self.process_union_member(&current_tokens)?;
                    members.push(field);
                    if let Some(info) = info {
                        field_info.push(info);
                    }
                    current_tokens.clear();
                }
            } else {
                current_tokens.push(token.clone());
            }
        }

        // Format the members with proper spacing
        for (i, member) in members.iter().enumerate() {
            result.push_str(member);
            if i < members.len() - 1 {
                result.push_str(",\n");
            }
        }

        Ok((result, field_info))
    }

    /// Process a single union member field
    fn process_union_member(
        &self,
        tokens: &[Token],
    ) -> Result<(String, Option<(String, String)>), ConversionError> {
        // Extract the type and name from the tokens
        if tokens.is_empty() {
            return Err(ConversionError::parse_error("", 0, 0, "Empty union member"));
        }

        // Find where the member name starts
        let mut type_tokens = Vec::new();
        let mut name_index = 0;
        let mut array_size = None;

        for (i, token) in tokens.iter().enumerate() {
            if let Token::Identifier(_) = token {
                // Assume the last identifier is the member name
                // and everything before is part of the type
                name_index = i;

                // Check if it's an array declaration
                if i + 1 < tokens.len() && tokens[i + 1] == Token::OpenBracket {
                    // Look for the array size
                    if i + 3 < tokens.len() && tokens[i + 3] == Token::CloseBracket {
                        if let Token::Number(size_str) = &tokens[i + 2] {
                            array_size = Some(size_str.clone());
                        }
                    }
                }
            }
        }

        // Extract the type tokens
        for i in 0..name_index {
            type_tokens.push(tokens[i].clone());
        }

        // Get the name
        let name = if let Token::Identifier(name) = &tokens[name_index] {
            name.clone()
        } else {
            return Err(ConversionError::parse_error(
                "",
                0,
                0,
                &format!(
                    "Expected identifier for member name, got {:?}",
                    tokens[name_index]
                ),
            ));
        };

        // Map the C type to Rust type
        let rust_type = self.map_c_type_to_rust(&type_tokens)?;

        // If it's an array, append the size
        let final_type = if let Some(size) = array_size {
            format!("[{}; {}]", rust_type, size)
        } else {
            rust_type.clone()
        };

        // Generate the field string
        let field_str = format!("    {}: {}", name, final_type);

        // Also return the name and type for accessor method generation
        Ok((field_str, Some((name, final_type))))
    }

    /// Map C type tokens to Rust type string with proper representation
    fn map_c_type_to_rust(&self, tokens: &[Token]) -> Result<String, ConversionError> {
        if tokens.is_empty() {
            return Err(ConversionError::parse_error("", 0, 0, "Empty type"));
        }

        let mut type_parts: Vec<String> = Vec::new();

        for token in tokens {
            match token {
                Token::Identifier(id) => {
                    type_parts.push(id.clone());
                }
                Token::Asterisk => {
                    // Check if we have "const" before the asterisk
                    if !type_parts.is_empty() && type_parts.last().unwrap() == "const" {
                        type_parts.pop(); // Remove "const"
                        type_parts.push("*const".to_string());
                    } else {
                        type_parts.push("*mut".to_string());
                    }
                }
                Token::Keyword(_) => {
                    // Convert keyword to string
                    let keyword_str = token.to_string();
                    type_parts.push(keyword_str);
                }
                _ => {
                    // Ignore other tokens for simplicity
                }
            }
        }

        // Join the parts and convert to Rust type
        let c_type = type_parts.join(" ");

        // Map common C types to Rust types
        Ok(match c_type.as_str() {
            "int" => "i32".to_string(),
            "unsigned int" => "u32".to_string(),
            "short" => "i16".to_string(),
            "unsigned short" => "u16".to_string(),
            "long" => "i64".to_string(),
            "unsigned long" => "u64".to_string(),
            "char" => "i8".to_string(),
            "unsigned char" => "u8".to_string(),
            "float" => "f32".to_string(),
            "double" => "f64".to_string(),
            "void" => "c_void".to_string(),
            "uint8_t" => "u8".to_string(),
            "uint16_t" => "u16".to_string(),
            "uint32_t" => "u32".to_string(),
            "uint64_t" => "u64".to_string(),
            "int8_t" => "i8".to_string(),
            "int16_t" => "i16".to_string(),
            "int32_t" => "i32".to_string(),
            "int64_t" => "i64".to_string(),
            "size_t" => "usize".to_string(),
            _ => c_type, // For custom types, use the original type name
        })
    }

    /// Generate accessor methods for safe access to union fields
    fn generate_accessors(&self, _union_name: &str, field_info: &[(String, String)]) -> String {
        let mut accessors = String::new();

        for (field_name, field_type) in field_info {
            // Generate getter method
            accessors.push_str(&format!(
                "    pub fn get_{}(&self) -> {} {{\n",
                field_name, field_type
            ));
            accessors.push_str(&format!("        unsafe {{ self.{} }}\n", field_name));
            accessors.push_str("    }\n    \n");

            // Generate setter method
            accessors.push_str(&format!(
                "    pub fn set_{}(&mut self, val: {}) {{\n",
                field_name, field_type
            ));
            accessors.push_str(&format!(
                "        unsafe {{ self.{} = val; }}\n",
                field_name
            ));
            accessors.push_str("    }\n    \n");
        }

        // Remove trailing newlines if any
        if !accessors.is_empty() {
            accessors = accessors.trim_end().to_string();
        }

        accessors
    }
}

impl TokenHandler for UnionHandler {
    fn can_handle(&self, tokens: &[Token], _context: &ParserContext) -> bool {
        // Check if this is a union declaration
        tokens.len() >= 2 && self.is_union_token(&tokens[0])
    }

    fn handle(
        &self,
        tokens: &[Token],
        context: &mut ParserContext,
    ) -> Result<HandlerResult, ConversionError> {
        // Make sure this is a union declaration
        if !self.can_handle(tokens, context) {
            return Ok(HandlerResult::NotHandled);
        }

        // Extract the union name
        let name = self.extract_union_name(tokens)?;

        // Determine attributes like packed or aligned
        let attributes = self.determine_attributes(tokens, context);
        let attributes_str = attributes.join("\n");

        // Process the union body to generate Rust union fields and accessors
        let (body, field_info) = self.process_union_body(tokens)?;

        // Generate accessor methods for safe access to the union fields
        let accessors = self.generate_accessors(&name, &field_info);

        // Build the Rust union declaration with attributes
        let rust_code = format!("{}\nunion {} {{\n{}\n}}", attributes_str, name, body);

        // Add accessor implementation with proper newlines
        let final_code = if !accessors.is_empty() {
            format!("{}\n\nimpl {} {{\n{}\n}}", rust_code, name, accessors)
        } else {
            rust_code
        };

        Ok(HandlerResult::RustCode(final_code))
    }
}
