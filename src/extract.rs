use crate::error::ConversionError;
use crate::token_parser::{Statement, StatementParser, Token};
use std::collections::HashMap;
use std::fmt;

/// Represents different types of extractable C elements
#[derive(Debug, Clone, PartialEq)]
pub enum ExtractedElement {
    Function(ExtractedFunction),
    Struct(ExtractedStruct),
    Enum(ExtractedEnum),
    Typedef(ExtractedTypedef),
    Global(ExtractedGlobal),
    Macro(ExtractedMacro),
}

/// Represents an extracted C function
#[derive(Debug, Clone, PartialEq)]
pub struct ExtractedFunction {
    pub name: String,
    pub return_type: Vec<Token>,
    pub parameters: Vec<(String, Vec<Token>)>, // (param_name, param_type)
    pub body: Vec<Statement>,
    pub is_variadic: bool,
    pub is_static: bool,
    pub is_inline: bool,
    pub tokens: Vec<Token>,
}

impl fmt::Display for ExtractedFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fn {}(", self.name)?;

        // Format parameters
        let mut params = Vec::new();
        for (i, (param_name, param_type)) in self.parameters.iter().enumerate() {
            let param_type_str = param_type
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<String>>()
                .join("");

            params.push(format!("{}: {}", param_name, param_type_str));

            if i < self.parameters.len() - 1 || self.is_variadic {
                params.push(", ".to_string());
            }
        }

        if self.is_variadic {
            params.push("...".to_string());
        }

        write!(f, "{}", params.join(""))?;

        // Format return type
        let return_type_str = self
            .return_type
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<String>>()
            .join("");

        if !return_type_str.is_empty() && return_type_str != "void" {
            write!(f, ") -> {}", return_type_str)?;
        } else {
            write!(f, ")")?;
        }

        Ok(())
    }
}

/// Represents an extracted C struct
#[derive(Debug, Clone, PartialEq)]
pub struct ExtractedStruct {
    pub name: String,
    pub fields: Vec<(String, Vec<Token>)>, // (field_name, field_type)
    pub tokens: Vec<Token>,
}

/// Represents an extracted C enum
#[derive(Debug, Clone, PartialEq)]
pub struct ExtractedEnum {
    pub name: String,
    pub values: Vec<(String, Option<i32>)>, // (name, value)
    pub tokens: Vec<Token>,
}

/// Represents an extracted C typedef
#[derive(Debug, Clone, PartialEq)]
pub struct ExtractedTypedef {
    pub name: String,
    pub original_type: Vec<Token>,
    pub tokens: Vec<Token>,
}

/// Represents an extracted global variable
#[derive(Debug, Clone, PartialEq)]
pub struct ExtractedGlobal {
    pub name: String,
    pub var_type: Vec<Token>,
    pub initial_value: Option<Vec<Token>>,
    pub is_const: bool,
    pub is_static: bool,
    pub tokens: Vec<Token>,
}

/// Represents an extracted macro
#[derive(Debug, Clone, PartialEq)]
pub struct ExtractedMacro {
    pub name: String,
    pub params: Vec<String>,
    pub body: Vec<Token>,
    pub tokens: Vec<Token>,
}

/// Main element extractor which can locate and identify various C code elements
pub struct Extractor {
    // Store extracted elements by type for easy access
    functions: HashMap<String, ExtractedFunction>,
    structs: HashMap<String, ExtractedStruct>,
    enums: HashMap<String, ExtractedEnum>,
    typedefs: HashMap<String, ExtractedTypedef>,
    globals: HashMap<String, ExtractedGlobal>,
    macros: HashMap<String, ExtractedMacro>,
}

impl Extractor {
    /// Create a new extractor instance
    pub fn new() -> Self {
        Extractor {
            functions: HashMap::new(),
            structs: HashMap::new(),
            enums: HashMap::new(),
            typedefs: HashMap::new(),
            globals: HashMap::new(),
            macros: HashMap::new(),
        }
    }

    /// Extract all elements from a given source code
    pub fn extract_all(
        &mut self,
        tokens: &[Token],
    ) -> Result<Vec<ExtractedElement>, ConversionError> {
        // Extract all functions
        self.extract_functions(tokens)?;

        // Extract all structures
        self.extract_structs(tokens)?;

        // Extract all enums
        self.extract_enums(tokens)?;

        // Extract all typedefs
        self.extract_typedefs(tokens)?;

        // Extract all global variables
        self.extract_globals(tokens)?;

        // Extract all macros
        self.extract_macros(tokens)?;

        // Combine all extracted elements into a single vector
        let mut all_elements = Vec::new();

        for (_, function) in &self.functions {
            all_elements.push(ExtractedElement::Function(function.clone()));
        }

        for (_, struct_def) in &self.structs {
            all_elements.push(ExtractedElement::Struct(struct_def.clone()));
        }

        for (_, enum_def) in &self.enums {
            all_elements.push(ExtractedElement::Enum(enum_def.clone()));
        }

        for (_, typedef) in &self.typedefs {
            all_elements.push(ExtractedElement::Typedef(typedef.clone()));
        }

        for (_, global) in &self.globals {
            all_elements.push(ExtractedElement::Global(global.clone()));
        }

        for (_, macro_def) in &self.macros {
            all_elements.push(ExtractedElement::Macro(macro_def.clone()));
        }

        Ok(all_elements)
    }

    /// Extract functions from the token stream
    pub fn extract_functions(&mut self, tokens: &[Token]) -> Result<(), ConversionError> {
        let mut i = 0;
        while i < tokens.len() {
            // Look for potential function signatures
            if self.is_function_start(&tokens[i..]) {
                if let Some(extracted_fn) = self.extract_function(&tokens[i..])? {
                    self.functions
                        .insert(extracted_fn.name.clone(), extracted_fn);

                    // Skip to the end of this function to continue searching
                    // For simplicity, we'll just increment i and continue for now
                    i += 1;
                    continue;
                }
            }
            i += 1;
        }

        Ok(())
    }

    /// Extract a single function from tokens starting at a potential function declaration
    pub(crate) fn extract_function(
        &self,
        tokens: &[Token],
    ) -> Result<Option<ExtractedFunction>, ConversionError> {
        // Check if we have enough tokens for a minimal function
        if tokens.len() < 5 {
            // At least: return_type name ( ) {
            return Ok(None);
        }

        // Find the function name and return type
        let mut name_pos = 0;
        let mut found_name = false;

        // Find opening parenthesis to determine function name position
        for i in 0..tokens.len() {
            if tokens[i] == Token::OpenParen {
                if i > 0 {
                    // The token before the opening parenthesis should be the function name
                    if let Token::Identifier(ref id) = tokens[i - 1] {
                        name_pos = i - 1;
                        found_name = true;
                        break;
                    }
                }
            }
        }

        if !found_name {
            return Ok(None);
        }

        // Extract function name
        let name = if let Token::Identifier(ref id) = tokens[name_pos] {
            id.clone()
        } else {
            return Ok(None);
        };

        // Determine return type (everything before the function name)
        let mut return_type = Vec::new();
        let mut is_static = false;
        let mut is_inline = false;

        for i in 0..name_pos {
            match &tokens[i] {
                Token::Identifier(id) if id == "static" => {
                    is_static = true;
                }
                Token::Identifier(id) if id == "inline" => {
                    is_inline = true;
                }
                _ => {
                    return_type.push(tokens[i].clone());
                }
            }
        }

        // Extract parameters
        let param_start = name_pos + 1; // After function name
        if param_start >= tokens.len() || tokens[param_start] != Token::OpenParen {
            return Ok(None);
        }

        let mut paren_count = 1;
        let mut param_end = param_start + 1;

        while param_end < tokens.len() && paren_count > 0 {
            match tokens[param_end] {
                Token::OpenParen => paren_count += 1,
                Token::CloseParen => paren_count -= 1,
                _ => {}
            }

            if paren_count > 0 {
                param_end += 1;
            }
        }

        if paren_count != 0 {
            return Err(ConversionError::general(
                "Unmatched parentheses in function declaration",
            ));
        }

        // Parse parameters
        let param_tokens = &tokens[param_start + 1..param_end];
        let (parameters, is_variadic) = self.parse_parameters(param_tokens)?;

        // Find function body
        let mut body_start = param_end + 1;
        while body_start < tokens.len() && tokens[body_start] != Token::OpenBrace {
            body_start += 1;
        }

        if body_start >= tokens.len() {
            // This might be just a function declaration without a body
            // or an error, but we'll assume it's just the type for now
            return Ok(Some(ExtractedFunction {
                name,
                return_type,
                parameters,
                body: vec![],
                is_variadic,
                is_static,
                is_inline,
                tokens: tokens[0..param_end + 1].to_vec(),
            }));
        }

        let mut brace_count = 1;
        let mut body_end = body_start + 1;

        while body_end < tokens.len() && brace_count > 0 {
            match tokens[body_end] {
                Token::OpenBrace => brace_count += 1,
                Token::CloseBrace => brace_count -= 1,
                _ => {}
            }

            if brace_count > 0 {
                body_end += 1;
            }
        }

        if brace_count != 0 {
            return Err(ConversionError::general(
                "Unmatched braces in function body",
            ));
        }

        // Parse function body into statements
        let body_tokens = &tokens[body_start + 1..body_end];
        let mut parser = StatementParser::new(body_tokens.to_vec());
        let body = parser.parse_statements();

        Ok(Some(ExtractedFunction {
            name,
            return_type,
            parameters,
            body,
            is_variadic,
            is_static,
            is_inline,
            tokens: tokens[0..body_end + 1].to_vec(),
        }))
    }

    /// Check if tokens start with a potential function declaration
    fn is_function_start(&self, tokens: &[Token]) -> bool {
        // For simplicity, we'll look for pattern: [identifier(s)] [identifier] (
        if tokens.len() < 3 {
            return false;
        }

        // Find first opening parenthesis
        for i in 2..tokens.len() {
            if tokens[i] == Token::OpenParen {
                // Check if preceding token is an identifier (potential function name)
                if let Token::Identifier(_) = tokens[i - 1] {
                    return true;
                }
            }
        }

        false
    }

    /// Parse function parameters
    fn parse_parameters(
        &self,
        tokens: &[Token],
    ) -> Result<(Vec<(String, Vec<Token>)>, bool), ConversionError> {
        let mut parameters = Vec::new();
        let mut is_variadic = false;

        // Empty parameter list
        if tokens.is_empty() {
            return Ok((parameters, is_variadic));
        }

        // Check for void parameter
        if tokens.len() == 1 {
            if let Token::Identifier(ref id) = tokens[0] {
                if id == "void" {
                    return Ok((parameters, is_variadic));
                }
            }
        }

        // Split by commas
        let mut param_start = 0;
        let mut i = 0;
        while i < tokens.len() {
            // Check for ellipsis as a standalone parameter (variadic function)
            if i == tokens.len() - 1 && tokens[i] == Token::Ellipsis {
                is_variadic = true;
                break; // No need to process further
            }

            // Skip over nested parentheses, brackets, etc.
            match tokens[i] {
                Token::Comma => {
                    if i > param_start {
                        let param_tokens = &tokens[param_start..i];
                        // Check if this parameter contains an ellipsis (shouldn't happen, but let's be safe)
                        if param_tokens.iter().any(|t| *t == Token::Ellipsis) {
                            is_variadic = true;
                        } else {
                            if let Some((name, param_type)) = self.parse_parameter(param_tokens)? {
                                parameters.push((name, param_type));
                            }
                        }
                    }
                    param_start = i + 1;
                }
                Token::Ellipsis => {
                    // Mark the function as variadic
                    is_variadic = true;

                    // If the ellipsis is part of a parameter (unlikely), skip it
                    // This shouldn't normally happen in standard C, but handle it just in case
                    if param_start < i {
                        let param_tokens = &tokens[param_start..i];
                        if let Some((name, param_type)) = self.parse_parameter(param_tokens)? {
                            parameters.push((name, param_type));
                        }
                    }

                    // Skip the ellipsis
                    param_start = i + 1;
                }
                Token::OpenParen | Token::OpenBracket | Token::OpenBrace => {
                    // Skip over nested structures
                    let closing_token = match tokens[i] {
                        Token::OpenParen => Token::CloseParen,
                        Token::OpenBracket => Token::CloseBracket,
                        Token::OpenBrace => Token::CloseBrace,
                        _ => unreachable!(),
                    };

                    let mut nesting = 1;
                    i += 1;
                    while i < tokens.len() && nesting > 0 {
                        if tokens[i] == tokens[param_start - 1] {
                            nesting += 1;
                        } else if tokens[i] == closing_token {
                            nesting -= 1;
                        }
                        i += 1;
                    }

                    continue; // Skip the normal increment at the end of the loop
                }
                _ => {}
            }

            i += 1;
        }

        // Handle the last parameter if it's not an ellipsis
        if param_start < tokens.len() && !is_variadic {
            let param_tokens = &tokens[param_start..];

            // Check if the last parameter contains an ellipsis
            if param_tokens.iter().any(|t| *t == Token::Ellipsis) {
                is_variadic = true;

                // If the ellipsis is in the middle of the parameter, try to parse what comes before it
                if let Some(ellipsis_pos) = param_tokens.iter().position(|t| *t == Token::Ellipsis)
                {
                    if ellipsis_pos > 0 {
                        let before_ellipsis = &param_tokens[0..ellipsis_pos];
                        if let Some((name, param_type)) = self.parse_parameter(before_ellipsis)? {
                            parameters.push((name, param_type));
                        }
                    }
                }
            } else {
                // Normal parameter without ellipsis
                if let Some((name, param_type)) = self.parse_parameter(param_tokens)? {
                    parameters.push((name, param_type));
                }
            }
        }

        Ok((parameters, is_variadic))
    }

    /// Parse a single parameter into name and type
    fn parse_parameter(
        &self,
        tokens: &[Token],
    ) -> Result<Option<(String, Vec<Token>)>, ConversionError> {
        if tokens.is_empty() {
            return Ok(None);
        }

        // Look for the parameter name (usually the last identifier before any array brackets or at the end)
        let mut name = String::new();
        let mut name_pos = 0;

        // First, try to find an = sign which would indicate a default value
        let mut default_value_pos = None;
        for i in 0..tokens.len() {
            if tokens[i] == Token::Equal {
                default_value_pos = Some(i);
                break;
            }
        }

        // Look for the parameter name
        for i in (0..tokens.len()).rev() {
            if default_value_pos.is_some() && i >= default_value_pos.unwrap() {
                continue; // Skip tokens after the = sign
            }

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

        if name.is_empty() {
            // Couldn't find a parameter name, this might be a type-only declaration
            // or an error, but we'll assume it's just the type for now
            return Ok(Some((String::new(), tokens.to_vec())));
        }

        // Extract the type (all tokens before the name)
        let param_type = tokens[0..name_pos].to_vec();

        Ok(Some((name, param_type)))
    }

    /// Extract structures from the token stream
    pub fn extract_structs(&mut self, tokens: &[Token]) -> Result<(), ConversionError> {
        let mut i = 0;
        while i < tokens.len() {
            // Look for "struct" keyword
            if let Token::Identifier(ref id) = tokens[i] {
                if id == "struct" && i + 1 < tokens.len() {
                    if let Some(extracted_struct) = self.extract_struct(&tokens[i..])? {
                        self.structs
                            .insert(extracted_struct.name.clone(), extracted_struct);
                    }
                }
            }
            i += 1;
        }

        Ok(())
    }

    /// Extract a single struct definition
    fn extract_struct(&self, tokens: &[Token]) -> Result<Option<ExtractedStruct>, ConversionError> {
        // Check if we have enough tokens
        if tokens.len() < 4 {
            // At least: struct name { }
            return Ok(None);
        }

        // Verify it starts with "struct"
        if let Token::Identifier(ref id) = tokens[0] {
            if id != "struct" {
                return Ok(None);
            }
        } else {
            return Ok(None);
        }

        // Get struct name
        let struct_name = if let Token::Identifier(ref id) = tokens[1] {
            id.clone()
        } else {
            return Ok(None);
        };

        // Find opening brace
        let mut body_start = 2;
        while body_start < tokens.len() && tokens[body_start] != Token::OpenBrace {
            body_start += 1;
        }

        if body_start >= tokens.len() {
            // This might be just a forward declaration
            return Ok(Some(ExtractedStruct {
                name: struct_name,
                fields: vec![],
                tokens: tokens[0..body_start].to_vec(),
            }));
        }

        // Find closing brace
        let mut brace_count = 1;
        let mut body_end = body_start + 1;

        while body_end < tokens.len() && brace_count > 0 {
            match tokens[body_end] {
                Token::OpenBrace => brace_count += 1,
                Token::CloseBrace => brace_count -= 1,
                _ => {}
            }

            if brace_count > 0 {
                body_end += 1;
            }
        }

        if brace_count != 0 {
            return Err(ConversionError::general(
                "Unmatched braces in struct definition",
            ));
        }

        // Parse field declarations
        let body_tokens = &tokens[body_start + 1..body_end];
        let fields = self.parse_struct_fields(body_tokens)?;

        Ok(Some(ExtractedStruct {
            name: struct_name,
            fields,
            tokens: tokens[0..body_end + 1].to_vec(),
        }))
    }

    /// Parse struct field declarations
    fn parse_struct_fields(
        &self,
        tokens: &[Token],
    ) -> Result<Vec<(String, Vec<Token>)>, ConversionError> {
        let mut fields = Vec::new();
        let mut i = 0;

        while i < tokens.len() {
            // Skip to next semicolon
            let mut semicolon_pos = i;
            while semicolon_pos < tokens.len() && tokens[semicolon_pos] != Token::Semicolon {
                semicolon_pos += 1;
            }

            if semicolon_pos >= tokens.len() {
                break;
            }

            // Parse field declaration
            let field_tokens = &tokens[i..semicolon_pos];
            if !field_tokens.is_empty() {
                if let Some((name, field_type)) = self.parse_field(field_tokens)? {
                    fields.push((name, field_type));
                }
            }

            i = semicolon_pos + 1;
        }

        Ok(fields)
    }

    /// Parse a single field declaration
    fn parse_field(
        &self,
        tokens: &[Token],
    ) -> Result<Option<(String, Vec<Token>)>, ConversionError> {
        // Similar to parameter parsing, but for struct fields
        if tokens.is_empty() {
            return Ok(None);
        }

        // Look for the field name (usually the last identifier before any array brackets or at the end)
        let mut name = String::new();
        let mut name_pos = 0;

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

        if name.is_empty() {
            return Ok(None);
        }

        // Extract the type (all tokens before the name)
        let field_type = tokens[0..name_pos].to_vec();

        Ok(Some((name, field_type)))
    }

    /// Extract enums from the token stream
    pub fn extract_enums(&mut self, _tokens: &[Token]) -> Result<(), ConversionError> {
        // Not fully implemented yet
        Ok(())
    }

    /// Extract a single enum definition
    fn extract_enum(&self, _tokens: &[Token]) -> Result<Option<ExtractedEnum>, ConversionError> {
        // Not fully implemented yet
        Ok(None)
    }

    /// Extract typedefs from the token stream
    pub fn extract_typedefs(&mut self, tokens: &[Token]) -> Result<(), ConversionError> {
        let mut i = 0;
        while i < tokens.len() {
            // Look for "typedef" keyword
            if let Token::Identifier(ref id) = tokens[i] {
                if id == "typedef" && i + 1 < tokens.len() {
                    if let Some(extracted_typedef) = self.extract_typedef(&tokens[i..])? {
                        self.typedefs
                            .insert(extracted_typedef.name.clone(), extracted_typedef);
                    }
                }
            }
            i += 1;
        }

        Ok(())
    }

    /// Extract a single typedef
    fn extract_typedef(
        &self,
        tokens: &[Token],
    ) -> Result<Option<ExtractedTypedef>, ConversionError> {
        // Check if we have enough tokens
        if tokens.len() < 3 {
            // At least: typedef type name;
            return Ok(None);
        }

        // Verify it starts with "typedef"
        if let Token::Identifier(ref id) = tokens[0] {
            if id != "typedef" {
                return Ok(None);
            }
        } else {
            return Ok(None);
        }

        // Find the semicolon that ends the typedef
        let mut end_pos = tokens.len() - 1;
        while end_pos > 0 && tokens[end_pos] != Token::Semicolon {
            end_pos -= 1;
        }

        if end_pos == 0 {
            return Err(ConversionError::general("Unterminated typedef definition"));
        }

        // For complex typedefs (struct/union/enum), the name is the identifier right before the semicolon
        // For simple typedefs, it's also the identifier right before the semicolon
        let typedef_name;

        // The name is the identifier right before the semicolon
        if let Token::Identifier(ref id) = tokens[end_pos - 1] {
            typedef_name = id.clone();
        } else {
            return Err(ConversionError::general(
                "Invalid typedef, could not find type name",
            ));
        }

        // Extract original type (everything between typedef and name)
        let original_type = tokens[1..end_pos - 1].to_vec();

        // Handle empty original type
        if original_type.is_empty() {
            return Err(ConversionError::general(
                "Invalid typedef, empty original type",
            ));
        }

        Ok(Some(ExtractedTypedef {
            name: typedef_name,
            original_type,
            tokens: tokens[0..=end_pos].to_vec(),
        }))
    }

    /// Extract global variables from the token stream
    pub fn extract_globals(&mut self, _tokens: &[Token]) -> Result<(), ConversionError> {
        // Not fully implemented yet
        Ok(())
    }

    /// Extract macros from the token stream
    pub fn extract_macros(&mut self, _tokens: &[Token]) -> Result<(), ConversionError> {
        // Not fully implemented yet
        Ok(())
    }

    /// Get all extracted functions
    pub fn get_functions(&self) -> &HashMap<String, ExtractedFunction> {
        &self.functions
    }

    /// Get all extracted structs
    pub fn get_structs(&self) -> &HashMap<String, ExtractedStruct> {
        &self.structs
    }

    /// Get all extracted enums
    pub fn get_enums(&self) -> &HashMap<String, ExtractedEnum> {
        &self.enums
    }

    /// Get all extracted typedefs
    pub fn get_typedefs(&self) -> &HashMap<String, ExtractedTypedef> {
        &self.typedefs
    }

    /// Get all extracted globals
    pub fn get_globals(&self) -> &HashMap<String, ExtractedGlobal> {
        &self.globals
    }

    /// Get all extracted macros
    pub fn get_macros(&self) -> &HashMap<String, ExtractedMacro> {
        &self.macros
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token_parser::Tokenizer;

    #[test]
    fn test_extract_simple_function() {
        let c_code = "int add(int a, int b) { return a + b; }";
        let mut tokenizer = Tokenizer::new(c_code);
        let tokens = tokenizer.tokenize();

        let extractor = Extractor::new();
        let function = extractor.extract_function(&tokens).unwrap().unwrap();

        assert_eq!(function.name, "add");
        assert_eq!(function.parameters.len(), 2);
        assert_eq!(function.parameters[0].0, "a");
        assert_eq!(function.parameters[1].0, "b");
        assert!(!function.is_variadic);
    }

    #[test]
    fn test_extract_variadic_function() {
        let c_code = "void printf(const char* format, ...) { /* body */ }";
        let mut tokenizer = Tokenizer::new(c_code);
        let tokens = tokenizer.tokenize();

        let extractor = Extractor::new();
        let function = extractor.extract_function(&tokens).unwrap().unwrap();

        assert_eq!(function.name, "printf");
        assert_eq!(function.parameters.len(), 1);
        assert_eq!(function.parameters[0].0, "format");
        assert!(function.is_variadic);
    }

    #[test]
    fn test_extract_simple_typedef() {
        let c_code = "typedef unsigned int uint32_t;";
        let mut tokenizer = Tokenizer::new(c_code);
        let tokens = tokenizer.tokenize();

        let extractor = Extractor::new();
        let typedef = extractor.extract_typedef(&tokens).unwrap().unwrap();

        assert_eq!(typedef.name, "uint32_t");
        assert_eq!(typedef.original_type.len(), 2);

        if let Token::Identifier(ref id) = typedef.original_type[0] {
            assert_eq!(id, "unsigned");
        } else {
            panic!("Expected identifier token");
        }

        if let Token::Identifier(ref id) = typedef.original_type[1] {
            assert_eq!(id, "int");
        } else {
            panic!("Expected identifier token");
        }
    }

    #[test]
    fn test_extract_complex_typedef() {
        let c_code = "typedef struct Point { int x; int y; } Point_t;";
        let mut tokenizer = Tokenizer::new(c_code);
        let tokens = tokenizer.tokenize();

        // Debug: Print the tokens to help us understand what we're working with
        println!("Tokens:");
        for (i, token) in tokens.iter().enumerate() {
            println!("{}: {:?}", i, token);
        }

        let extractor = Extractor::new();
        let typedef = extractor.extract_typedef(&tokens).unwrap().unwrap();

        assert_eq!(typedef.name, "Point_t");

        // First token should be "struct"
        if let Token::Identifier(ref id) = typedef.original_type[0] {
            assert_eq!(id, "struct");
        } else {
            panic!("Expected identifier token");
        }

        // Second token should be "Point"
        if let Token::Identifier(ref id) = typedef.original_type[1] {
            assert_eq!(id, "Point");
        } else {
            panic!("Expected identifier token");
        }
    }
}
