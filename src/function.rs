use crate::{error::ConversionError, types::Type, token_parser::{Statement, StatementParser, Token, Tokenizer}};
use std::collections::HashMap;
use std::hash::{Hash, Hasher};

/// Represents a C function to be converted to Rust
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    pub name: String,
    pub return_type: String,
    pub args: Vec<(String, String)>,
    pub body: Option<String>,
    pub content: String,
    pub variables: HashMap<String, String>,
}
impl Hash for Function {

    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.return_type.hash(state);
        self.args.hash(state);
        self.body.hash(state);
        self.content.hash(state);
        for (key, value) in self.variables.iter() {
            key.hash(state);
            value.hash(state);
        }
    }
}
impl Function {
    pub fn new() -> Self {
        Function {
            name: String::new(),
            return_type: "()".to_string(),
            args: Vec::new(),
            body: None,
            content: String::new(),
            variables: std::collections::HashMap::new(),
        }
    }

    /// Set the function name
    pub fn set_name(&mut self, content: &str) -> Result<String, ConversionError> {
        self.name = self.extract_name(content).unwrap_or_default();
        Ok(self.name.clone())
    }

    /// Output the function as a Rust function
    pub fn to_rust(&mut self) -> Result<String, ConversionError> {
        let mut rust_code = String::new();

        // Function signature
        rust_code.push_str(&format!("pub fn {}(", self.name));
        for (arg_name, arg_type) in &self.args {
            rust_code.push_str(&format!("{}: {}, ", arg_name, arg_type));
        }
        if !self.args.is_empty() {
            rust_code.pop(); // Remove trailing comma and space
            rust_code.pop();
        }
        rust_code.push(')');

        if self.return_type != "()" {
            rust_code.push_str(&format!(" -> {}", self.return_type));
        }
        rust_code.push_str(" {\n");

        // Function body
        let body_str = self.convert_body_to_rust()?;
        rust_code.push_str(&body_str);

        rust_code.push_str("}\n");
        Ok(rust_code)
    }

    /// Convert the function body to Rust
    fn convert_body_to_rust(&mut self) -> Result<String, ConversionError> {
        if self.body.is_none() {
            return Ok(String::new());
        }
        let body = self.body.as_ref().unwrap();

        // Tokenize the function body
        let tokens = Tokenizer::new(body).tokenize();

        // Parse statements
        let statements = StatementParser::new(tokens).parse();

        // Convert statements to Rust code
        Ok(self.convert_statements_to_rust(statements, 1))
    }

    fn convert_statements_to_rust(&mut self, statements: Vec<Statement>, indent_level: usize) -> String {
        let mut rust_body = String::new();
        for stmt in statements {
            rust_body.push_str(&self.convert_statement_to_rust(&stmt, indent_level));
        }
        rust_body
    }

    fn convert_statement_to_rust(&mut self, stmt: &Statement, indent_level: usize) -> String {
        let indent = "    ".repeat(indent_level);
        match stmt {
            Statement::Empty => "".to_string(),
            Statement::Expression(tokens) => {
                if let (
                    Some(Token::OpenParen),
                    Some(Token::Identifier(id)),
                    Some(Token::CloseParen),
                    Some(Token::Identifier(var_name))
                ) = (tokens.get(0), tokens.get(1), tokens.get(2), tokens.get(3)) {
                    if id == "void" && tokens.len() == 4 {
                        return format!("{}let _ = {};\n", indent, var_name);
                    }
                }
                format!("{}{};\n", indent, self.tokens_to_string(tokens))
            }
            Statement::Return(tokens) => format!("{}return {};\n", indent, self.tokens_to_string(tokens)),
            Statement::If(condition, if_body, else_body) => {
                let rust_condition = self.convert_condition_to_rust(condition);
                let mut if_statement = format!("{}if {} {{\n", indent, rust_condition);
                if_statement.push_str(&self.convert_statements_to_rust(if_body.to_vec(), indent_level + 1));
                if_statement.push_str(&format!("{}}}\n", indent));

                if !else_body.is_empty() {
                    if_statement.push_str(&format!("{}else {{\n", indent));
                    if_statement.push_str(&self.convert_statements_to_rust(else_body.to_vec(), indent_level + 1));
                    if_statement.push_str(&format!("{}}}\n", indent));
                }
                if_statement
            }
            Statement::Declaration(tokens) => {
                // Parse to get info for the type map
                let (var_name, c_type) = self.parse_declaration_info(tokens);
                if let (Some(name), Some(ty)) = (var_name, c_type) {
                    self.variables.insert(name, ty);
                }

                // Convert the declaration to a Rust statement
                match self.convert_declaration(tokens) {
                    Ok(decl_str) => format!("{}{}\n", indent, decl_str),
                    Err(_) => format!("{}// Failed to convert declaration: {:?}\n", indent, tokens),
                }
            }
            Statement::Block(statements) => {
                let mut block_str = format!("{}{{\n", indent);
                block_str.push_str(&self.convert_statements_to_rust(statements.to_vec(), indent_level + 1));
                block_str.push_str(&format!("{}}}\n", indent));
                block_str
            }
            _ => format!("{}// Unhandled statement: {:?}\n", indent, stmt),
        }
    }

    fn convert_condition_to_rust(&self, tokens: &[Token]) -> String {
        let mut condition_parts = Vec::new();
        let mut i = 0;
        while i < tokens.len() {
            let token = &tokens[i];
            if let Token::Identifier(var_name) = token {
                let is_part_of_comparison = (i > 0 && self.is_comparison_operator(&tokens[i - 1])) || 
                                            (i < tokens.len() - 1 && self.is_comparison_operator(&tokens[i + 1]));

                if !is_part_of_comparison {
                    if let Some(c_type) = self.variables.get(var_name) {
                        if c_type.contains('*') {
                            condition_parts.push(format!("!{}.is_null()", var_name));
                        } else {
                            condition_parts.push(format!("{} != 0", var_name));
                        }
                    } else {
                        // If type info is not available, assume numeric check as a fallback
                        condition_parts.push(format!("{} != 0", var_name));
                    }
                } else {
                    condition_parts.push(var_name.clone());
                }
            } else {
                condition_parts.push(self.token_to_string(token));
            }
            i += 1;
        }
        condition_parts.join(" ")
    }

    fn is_comparison_operator(&self, token: &Token) -> bool {
        matches!(token, Token::EqualsEquals | Token::LessThan | Token::GreaterThan)
    }

    fn tokens_to_string(&self, tokens: &[Token]) -> String {
        let mut result = Vec::new();
        let mut i = 0;
        while i < tokens.len() {
            let token = &tokens[i];

            if let Token::Identifier(id) = token {
                if id == "L" {
                    if let Some(Token::StringLiteral(_)) = tokens.get(i + 1) {
                        i += 1; // Skip 'L' identifier
                        continue;
                    }
                }
            }

            if *token == Token::Sizeof {
                if let (Some(Token::OpenParen), Some(Token::Identifier(var_name)), Some(Token::CloseParen)) = 
                    (tokens.get(i + 1), tokens.get(i + 2), tokens.get(i + 3)) {
                    result.push(format!("{}.len()", var_name));
                    i += 4; // Skip sizeof, (, var, )
                    continue;
                }
            }
            result.push(self.token_to_string(token));
            i += 1;
        }
        result.join(" ")
    }

    fn token_to_string(&self, token: &Token) -> String {
        match token {
            Token::Identifier(id) => id.clone(),
            Token::Number(n) => n.clone(),
            Token::StringLiteral(lit) => format!("\"{}\"", lit),
            Token::If => "if".to_string(),
            Token::Else => "else".to_string(),
            Token::For => "for".to_string(),
            Token::While => "while".to_string(),
            Token::Return => "return".to_string(),
            Token::Sizeof => "sizeof".to_string(),
            Token::OpenParen => "(".to_string(),
            Token::CloseParen => ")".to_string(),
            Token::OpenBrace => "{".to_string(),
            Token::CloseBrace => "}".to_string(),
            Token::OpenBracket => "[".to_string(),
            Token::CloseBracket => "]".to_string(),
            Token::Semicolon => ";".to_string(),
            Token::Comma => ",".to_string(),
            Token::Asterisk => "*".to_string(),
            Token::Equals => "=".to_string(),
            Token::EqualsEquals => "==".to_string(),
            Token::Plus => "+".to_string(),
            Token::Minus => "-".to_string(),
            Token::Slash => "/".to_string(),
            Token::Arrow => "->".to_string(),
            Token::Dot => ".".to_string(),
            Token::Ampersand => "&".to_string(),
            Token::LessThan => "<".to_string(),
            Token::GreaterThan => ">".to_string(),
            Token::Pipe => "|".to_string(),
            Token::LogicalAnd => "&&".to_string(),
            Token::LogicalOr => "||".to_string(),
        }
    }

    /// Extracts variable name and C type from a declaration token stream.
    fn parse_declaration_info(&self, tokens: &[Token]) -> (Option<String>, Option<String>) {
        let mut type_tokens = Vec::new();
        let mut name = None;

        // Find the position of the variable name. It's typically the first identifier
        // that is not a recognized C type, followed by a terminator like ';', '=', or '['.
        let mut _type_part_ended = false;
        for (_i, token) in tokens.iter().enumerate() {
            if _type_part_ended {
                if let Token::Identifier(id) = token {
                    name = Some(id.clone());
                    break;
                }
            } else {
                match token {
                    Token::Identifier(id) => {
                        if self.is_c_type(id) {
                            type_tokens.push(token.clone());
                        } else {
                            _type_part_ended = true;
                            name = Some(id.clone());
                            break;
                        }
                    }
                    Token::Asterisk => type_tokens.push(token.clone()),
                    _ => _type_part_ended = true,
                }
            }
        }

        if name.is_some() {
            (name, Some(self.tokens_to_string(&type_tokens)))
        } else {
            (None, None)
        }
    }

    /// Convert a single C variable declaration to Rust
    fn convert_declaration(&self, tokens: &[Token]) -> Result<String, ConversionError> {
        if tokens.is_empty() {
            return Ok("".to_string());
        }

        let equals_pos = tokens.iter().position(|t| *t == Token::Equals);
        let declaration_tokens = if let Some(pos) = equals_pos { &tokens[..pos] } else { tokens };
        let initialization_tokens = if let Some(pos) = equals_pos { Some(&tokens[pos + 1..]) } else { None };

        // Handle arrays: find brackets
        let mut array_spec = None;
        if let Some(bracket_pos) = declaration_tokens.iter().position(|t| *t == Token::OpenBracket) {
            if let Some(end_bracket_pos) = declaration_tokens.iter().rposition(|t| *t == Token::CloseBracket) {
                array_spec = Some(&declaration_tokens[bracket_pos..=end_bracket_pos]);
            }
        }

        let (type_and_name_tokens, array_size_str) = if let Some(spec) = array_spec {
            // Everything before the array spec is type and name
            let type_name_part = &declaration_tokens[..spec.first().map_or(0, |t| tokens.iter().position(|x| x == t).unwrap_or(0))];
            let size_tokens = &spec[1..spec.len()-1];
            (type_name_part, self.tokens_to_string(size_tokens))
        } else {
            (declaration_tokens, "".to_string())
        };

        let mut name_pos = 0;
        for (_i, token) in type_and_name_tokens.iter().enumerate().rev() {
            if let Token::Identifier(_) = token {
                name_pos = _i;
                break;
            }
        }

        let type_tokens = &type_and_name_tokens[..name_pos];
        let var_name = self.token_to_string(&type_and_name_tokens[name_pos]);

        let mut is_pointer = false;
        let mut is_const = false;
        let mut type_parts = Vec::new();
        for token in type_tokens {
            match token {
                Token::Asterisk => is_pointer = true,
                Token::Identifier(id) if id == "const" => is_const = true,
                _ => type_parts.push(self.token_to_string(token)),
            }
        }
        let c_type = type_parts.join(" ").trim().to_string();

        let mut rust_type = Type::convert(&c_type);

        let mut is_array_of_strings = false;
        if let Some(init_tokens) = initialization_tokens {
            if init_tokens.iter().all(|t| matches!(t, Token::StringLiteral(_) | Token::Comma | Token::OpenBrace | Token::CloseBrace)) {
                if is_pointer && (c_type == "char" || c_type == "i8") {
                    is_array_of_strings = true;
                }
            }
        }

        if is_array_of_strings {
            rust_type = "&'static str".to_string();
        } else if is_pointer {
            if is_const {
                rust_type = format!("*const {}", rust_type);
            } else {
                rust_type = format!("*mut {}", rust_type);
            }
        }

        let mut rust_decl = format!("let mut {}: {}", Type::escape(&var_name), rust_type);

        if !array_size_str.is_empty() {
            rust_decl = format!(
                "let mut {}: [{}; {}]",
                Type::escape(&var_name),
                rust_type,
                array_size_str
            );
        }

        if let Some(init_tokens) = initialization_tokens {
            let init_str = self.tokens_to_string(init_tokens);
            rust_decl.push_str(&format!(" = {};", self.convert_initializer(&init_str)));
        } else if !array_size_str.is_empty() {
            rust_decl.push_str(&format!(" = [Default::default(); {}];", array_size_str));
        } else if is_pointer {
            rust_decl.push_str(" = ptr::null_mut();");
        } else {
            rust_decl.push_str(" = Default::default();");
        }

        Ok(rust_decl)
    }

    fn convert_initializer(&self, init: &str) -> String {
        // This is a simplified conversion. A more robust solution would parse expressions.
        init.replace("{", "[").replace("}", "]")
    }

    fn is_c_type(&self, s: &str) -> bool {
        let s = s.trim();
        // This is a simple heuristic. A more robust solution would be a proper parser.
        let known_types = [
            "int", "char", "void", "long", "short", "float", "double", "size_t",
            "ssize_t", "uint8_t", "int8_t", "uint16_t", "int16_t", "uint32_t",
            "int32_t", "uint64_t", "int64_t",
        ];
        known_types.contains(&s)
            || s.starts_with("struct")
            || s.starts_with("union")
            || s.starts_with("enum")
            || s.ends_with("_t")
    }

    /// Extract function name from a function definition
    pub fn extract_name(&self, content: &str) -> Option<String> {
        let content = content.trim();

        // Skip any leading keywords like 'static', 'extern', 'inline', etc.
        let mut start_idx = 0;
        let words: Vec<&str> = content.split_whitespace().collect();

        for word in words {
            match word {
                "static" | "extern" | "inline" | "const" | "EXPORT_SYMBOL" => {
                    // Skip the keyword and any following whitespace
                    start_idx += word.len() + 1; // +1 for the space
                },
                _ => break,
            }
        }

        if start_idx >= content.len() {
            return None;
        }

        // Now look for the function name
        let content = &content[start_idx..];

        // Find the opening parenthesis which indicates the start of arguments
        if let Some(paren_pos) = content.find('(') {
            let before_paren = &content[..paren_pos].trim_end();

            // Function name is the last word before the opening parenthesis
            if let Some(last_space) = before_paren.rfind(|c: char| c.is_whitespace()) {
                let name = &before_paren[last_space + 1..];

                // Clean the name from any invalid characters
                let clean_name = name.trim()
                    .trim_start_matches('(')
                    .trim_end_matches(')')
                    .trim();

                // Check for invalid function names (containing operators or special characters)
                if clean_name.contains("->") ||
                    clean_name.contains('{') ||
                    clean_name.contains('}') ||
                    clean_name.contains('*') {
                    // This is likely not a valid function name
                    return None;
                }

                return Some(clean_name.to_string());
            } else {
                // No space found, the entire string before parenthesis is the name
                let clean_name = before_paren
                    .trim_start_matches('(')
                    .trim_end_matches(')')
                    .trim();

                if clean_name.contains("->") ||
                    clean_name.contains('{') ||
                    clean_name.contains('}') ||
                    clean_name.contains('*') {
                    // This is likely not a valid function name
                    return None;
                }

                return Some(clean_name.to_string());
            }
        }

        None
    }

    /// Extracts return type from function definition
    pub fn extract_return_type(&self, content: &str) -> Option<String> {
        // Find the position of the function name
        let mut start_idx = 0;
        let chars: Vec<char> = content.chars().collect();

        // Skip any leading keywords like 'static', 'extern', 'inline', etc.
        while start_idx < chars.len() {
            let remaining = &content[start_idx..];
            let next_word_end = remaining.find(|c: char| !c.is_alphabetic() && c != '_')
                .unwrap_or(remaining.len());

            if next_word_end == 0 {
                // Not at a word, skip this character
                start_idx += 1;
                continue;
            }

            let word = &remaining[..next_word_end];
            match word {
                "static" | "extern" | "inline" | "const" | "EXPORT_SYMBOL" => {
                    // Skip the keyword and any following whitespace
                    start_idx += next_word_end;
                    while start_idx < chars.len() && chars[start_idx].is_whitespace() {
                        start_idx += 1;
                    }
                },
                _ => break, // Not a keyword to skip, so stop
            }
        }

        if start_idx >= chars.len() {
            return Some("()".to_string()); // Default to unit if we reached the end
        }

        // Now find the return type and function name
        let content = &content[start_idx..];

        // Look for the function name followed by opening parenthesis
        if let Some(paren_pos) = content.find('(') {
            let before_paren = &content[..paren_pos].trim_end();

            // Find the last word which is the function name
            if let Some(last_space) = before_paren.rfind(|c: char| c.is_whitespace()) {
                // Extract return type (everything before the function name)
                let c_return_type = before_paren[..last_space].trim();

                // Convert to Rust type
                return Some(Type::convert(c_return_type));
            }
        }

        // Default return type is unit/void if not found
        Some("()".to_string())
    }

    /// Extracts arguments from function definition
    pub fn extract_args(&self, content: &str) -> Option<Vec<(String, String)>> {
        // First locate the function signature
        let mut in_signature = false;
        let mut _signature_start = 0;
        let mut paren_level = 0;
        let mut args_start = 0;
        let mut args_end = 0;

        let chars: Vec<char> = content.chars().collect();
        
        for (_i, c) in chars.iter().enumerate() {
            if *c == '(' && !in_signature {
                // This might be the start of function arguments
                in_signature = true;
                _signature_start = _i;
                paren_level = 1;
                args_start = _i + 1;
                continue;
            }
            
            if in_signature {
                if *c == '(' {
                    paren_level += 1;
                } else if *c == ')' {
                    paren_level -= 1;
                    if paren_level == 0 {
                        // We found the end of the argument list
                        args_end = _i;
                        break;
                    }
                }
            }
        }
        
        if args_start >= args_end {
            // No arguments or invalid format
            return Some(Vec::new());
        }
        
        // Extract the arguments string
        let args_str = &content[args_start..args_end];
        
        // Split the arguments
        let mut result = Vec::new();
        let mut current_arg = String::new();
        let mut nested_level = 0;
        
        for c in args_str.chars() {
            match c {
                '(' | '{' | '[' => {
                    nested_level += 1;
                    current_arg.push(c);
                },
                ')' | '}' | ']' => {
                    nested_level -= 1;
                    current_arg.push(c);
                },
                ',' if nested_level == 0 => {
                    // End of an argument
                    if !current_arg.trim().is_empty() {
                        result.push(current_arg.trim().to_string());
                        current_arg = String::new();
                    }
                },
                _ => current_arg.push(c)
            }
        }
        
        // Add the last argument if any
        if !current_arg.trim().is_empty() {
            result.push(current_arg.trim().to_string());
        }
        
        // Clean up arguments
        let cleaned_args: Vec<(String, String)> = result.iter()
            .map(|arg| {
                // Remove C-style type declarations
                let mut cleaned = arg.to_string();
                
                // Convert C types to Rust types
                if cleaned.contains(' ') {
                    let parts: Vec<&str> = cleaned.splitn(2, ' ').collect();
                    if parts.len() == 2 {
                        let type_part = parts[0].trim();
                        let name_part = parts[1].trim();
                        
                        // Handle pointers
                        if type_part.contains('*') || name_part.contains('*') {
                            let rust_type = Type::convert(type_part);
                            if name_part.contains('*') {
                                // The pointer is part of the name - extract it
                                let name = name_part.replace("*", "").trim().to_string();
                                cleaned = format!("{}: *mut {}", name, rust_type);
                            } else {
                                cleaned = format!("{}: {}", name_part, rust_type);
                            }
                        } else {
                            // Regular type
                            let rust_type = Type::convert(type_part);
                            cleaned = format!("{}: {}", name_part, rust_type);
                        }
                    }
                }
                
                cleaned
            })
            .map(|arg| {
                let parts: Vec<&str> = arg.splitn(2, ':').collect();
                if parts.len() == 2 {
                    (parts[0].trim().to_string(), parts[1].trim().to_string())
                } else {
                    (arg, "c_void".to_string())
                }
            })
            .collect();

        Some(cleaned_args)
    }

    /// Extracts the function body from the content
    pub fn extract_body(&self, content: &str) -> Option<String> {
        // This is a simplified implementation. A more robust solution would
        // need to handle nested blocks and other C syntax complexities.
        if let Some(start) = content.find('{') {
            if let Some(end) = content.rfind('}') {
                return Some(content[start + 1..end].to_string());
            }
        }
        None
    }

    /// Convert C function to Rust
    pub fn convert_with_errors(&mut self, content: &str) -> Result<(), ConversionError> {
        self.parse(content)
    }

    /// Parse the function
    pub fn parse(&self, content: &str) -> Result<(), ConversionError> {
        let mut function = Function::new();
        function.content = content.to_string();

        // Extract function name
        function.name = self.extract_name(content).unwrap_or_default();

        // Extract return type
        function.return_type = self.extract_return_type(content).unwrap_or_default();

        // Extract arguments
        function.args = self.extract_args(content).unwrap_or_default();

        // Extract body
        if let Some(body) = self.extract_body(content) {
            function.body = Some(body);
        }

        Ok(())
    }
}