use crate::error::ConversionError;
use crate::function::Function;
use crate::struct_def::Struct;
use crate::common::{find_matching_brace, find_matching_paren, convert_type};
use crate::comment::Comment;
use std::fs;
use std::io::{Read, Write};

/// The main parser for C to Rust code conversion
///
/// Parser is the central struct responsible for reading C code,
/// parsing it into various components, and outputting equivalent
/// Rust code.
#[derive(Debug, Clone)]
pub struct Parser {
    /// Input file handle
    pub in_file: *mut fs::File,
    /// Output file handle
    pub out_file: *mut fs::File,
    /// Functions found during parsing
    pub functions: Vec<Function>,
    /// Structs found during parsing
    pub structs: Vec<Struct>,
    /// Comments found during parsing
    pub comments: Vec<Comment>,
    /// Total length of parsed content
    pub len: usize,
    /// Current parsing position
    pub position: usize,
    /// Offset in the input
    pub offset: usize,
    /// Collection of errors encountered during parsing
    pub errors: Vec<ConversionError>,
}

impl Parser {
    /// Creates a new empty Parser
    pub fn new() -> Parser {
        Parser {
            in_file: std::ptr::null_mut(),
            out_file: std::ptr::null_mut(),
            functions: Vec::new(),
            structs: Vec::new(),
            comments: Vec::new(),
            len: 0,
            position: 0,
            offset: 0,
            errors: Vec::new(),
        }
    }

    /// Maps a C expression to its Rust equivalent
    pub fn convert_expr(&self, c_expr: &str) -> String {
        // Simple direct replacements
        let mut result = c_expr.to_string();
        
        for (c, rust) in crate::common::expr_map() {
            if result.contains(c) {
                result = result.replace(c, rust);
            }
        }
        
        // Handle more complex patterns
        result = self.handle_complex_expr_patterns(&result);
        
        result
    }
    
    /// Handle complex expression patterns that need more than simple replacement
    fn handle_complex_expr_patterns(&self, expr: &str) -> String {
        // This is a placeholder - a real implementation would have more 
        // sophisticated pattern handling for complex C expressions
        expr.to_string()
    }
    
    /// Adds a error to the errors list
    pub fn add_error(&mut self, error: ConversionError) {
        self.errors.push(error);
    }
    
    /// Gets all errors encountered during parsing
    pub fn get_errors(&self) -> &[ConversionError] {
        &self.errors
    }
    
    /// Checks if there are any errors
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
    
    /// Generates Rust code from the parsed components
    fn generate_rust_code(&mut self) -> String {
        let mut output = String::new();
        
        // Add standard imports
        output.push_str("use std::os::raw::*;\n");
        output.push_str("use std::ptr;\n");
        output.push_str("use std::mem;\n\n");
        
        // Add comments at the top of the file
        for comment in &self.comments {
            if comment.content.contains("/**") {
                output += &comment.output();
                output.push_str("\n");
            }
        }
        
        // Add all structs
        for struct_ in &self.structs {
            output += &struct_.to_rust();
            output.push_str("\n");
        }
        
        // Convert each function to Rust
        for function in self.functions.iter_mut() {
            match function.to_rust() {
                Ok(rust_fn) => output += &rust_fn,
                Err(e) => {
                    // Log the error and continue
                    eprintln!("Failed to convert function {}: {}\n", function.name, e);
                }
            }
        }

        // Append any remaining content (comments, etc.)
        output
    }
    
    /// Reads a C file and parses it
    pub fn read(&mut self, path: &str) -> Result<(), ConversionError> {
        let mut buffer = String::new();
        let mut file = fs::File::open(path)?;
        file.read_to_string(&mut buffer)?;
        self.parse(&buffer.trim()).map(|_| ())
    }
    
    /// Writes the generated Rust code to a file
    pub fn write(&mut self, path: &str) -> Result<(), ConversionError> {
        let mut file = fs::File::create(path)?;
        let s = self.generate_rust_code();
        file.write_all(s.as_bytes())?;
        Ok(())
    }
    
    /// Parse and convert the input file to Rust
    pub fn parse_file(&mut self, file_path: &str) -> Result<String, ConversionError> {
        let buffer = fs::read_to_string(file_path)
            .map_err(|e| ConversionError::general(&format!("Failed to read file: {}", e)))?;
        
        // Remove any BOM at the start of the file
        let buffer = if buffer.starts_with('\u{FEFF}') {
            buffer[3..].to_string()
        } else {
            buffer
        };
        
        self.parse(&buffer)
    }
    
    /// Parses C code into components
    pub fn parse(&mut self, content: &str) -> Result<String, ConversionError> {
        if content.is_empty() {
            return Ok(String::new());
        }
        
        // First, preprocess the content to remove comments and normalize whitespace
        let preprocessed_content = self.preprocess_content(content);
        
        // Extract structs and functions
        let mut i = 0;
        let len = preprocessed_content.len();
        
        while i < len {
            let _c = preprocessed_content.chars().nth(i).unwrap_or(' ');
            
            // Extract struct
            if self.extract_struct(&mut i, &preprocessed_content) {
                // Struct extraction was successful, skip to the next part
                continue;
            }
            // Extract function
            else if self.extract_function(&mut i, &preprocessed_content) {
                // Function extraction was successful, skip to the next part
                continue;
            }
            else {
                i += 1;
            }
        }
        
        Ok(self.generate_rust_code())
    }
    
    /// Preprocess content to remove comments and normalize whitespace
    fn preprocess_content(&self, content: &str) -> String {
        let mut result = String::new();
        let mut i = 0;
        let chars: Vec<char> = content.chars().collect();
        
        // First pass: strip comments and handle preprocessor directives
        while i < chars.len() {
            // Handle preprocessor directives (starts with #)
            if chars[i] == '#' {
                // Skip the whole line with the preprocessor directive
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                }
                if i < chars.len() {
                    i += 1; // Skip past newline
                }
                continue;
            }
            
            // Handle single-line comments
            if i + 1 < chars.len() && chars[i] == '/' && chars[i + 1] == '/' {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                }
                if i < chars.len() {
                    result.push('\n'); // Keep newline for line numbering
                    i += 1; // Skip past newline
                }
                continue;
            }
            
            // Handle multi-line comments
            if i + 1 < chars.len() && chars[i] == '/' && chars[i + 1] == '*' {
                i += 2; // Skip past /*
                while i + 1 < chars.len() && !(chars[i] == '*' && chars[i + 1] == '/') {
                    if chars[i] == '\n' {
                        result.push('\n'); // Keep newlines for line numbering
                    }
                    i += 1;
                }
                if i + 1 < chars.len() {
                    i += 2; // Skip past */
                }
                continue;
            }
            
            // Handle string literals - replace with placeholders to avoid issues
            if chars[i] == '"' {
                result.push('"');
                i += 1;
                while i < chars.len() && chars[i] != '"' {
                    // Handle escape sequences
                    if chars[i] == '\\' && i + 1 < chars.len() {
                        result.push('\\');
                        i += 1;
                        result.push(chars[i]);
                        i += 1;
                    } else {
                        result.push(chars[i]);
                        i += 1;
                    }
                }
                if i < chars.len() {
                    result.push('"');
                    i += 1;
                }
                continue;
            }
            
            // Handle character literals
            if chars[i] == '\'' {
                result.push('\'');
                i += 1;
                while i < chars.len() && chars[i] != '\'' {
                    // Handle escape sequences
                    if chars[i] == '\\' && i + 1 < chars.len() {
                        result.push('\\');
                        i += 1;
                        result.push(chars[i]);
                        i += 1;
                    } else {
                        result.push(chars[i]);
                        i += 1;
                    }
                }
                if i < chars.len() {
                    result.push('\'');
                    i += 1;
                }
                continue;
            }
            
            // Add normal character
            result.push(chars[i]);
            i += 1;
        }
        
        // Second pass: additional cleanup
        // Remove multiple consecutive empty lines and normalize whitespace
        let lines: Vec<&str> = result.split('\n').collect();
        let mut cleaned_lines = Vec::new();
        let mut prev_empty = false;
        
        for line in lines {
            let trimmed = line.trim();
            
            // Skip EXPORT_SYMBOL directives and other common non-convertible patterns
            if trimmed.starts_with("EXPORT_SYMBOL") || 
               trimmed.starts_with("#") ||
               trimmed.contains("__attribute__") {
                continue;
            }
            
            if trimmed.is_empty() {
                if !prev_empty {
                    cleaned_lines.push("");
                    prev_empty = true;
                }
            } else {
                cleaned_lines.push(line);
                prev_empty = false;
            }
        }
        
        cleaned_lines.join("\n")
    }
    
    /// Extract a struct definition
    fn extract_struct(&mut self, i: &mut usize, content: &str) -> bool {
        let start = *i;
        
        // Check if this is actually a struct
        if !content[start..].starts_with("struct") &&
           !content[start..].starts_with("typedef struct") &&
           !content[start..].starts_with("static struct") &&
           !content[start..].starts_with("const struct") {
            return false;
        }
        
        // Find struct name and opening brace
        let chars: Vec<char> = content[start..].chars().collect();
        let mut found_name = false;
        let name_start = 0;
        let mut name_end = 0;
        let mut found_brace = false;
        let mut brace_pos = 0;
        let mut in_tag = false;
        let mut in_whitespace = true;
        let mut seen_struct_keyword = false;
        let mut j = 0;
        
        while j < chars.len() {
            let c = chars[j];
            
            // Skip whitespace before name
            if in_whitespace && c != ' ' && c != '\t' && c != '\n' && c != '\r' {
                in_whitespace = false;
                
                // Check if we're seeing "struct" keyword
                if j + 5 < chars.len() && 
                   &chars[j..j+6] == &['s', 't', 'r', 'u', 'c', 't'] {
                    seen_struct_keyword = true;
                    j += 6; // Skip "struct"
                    in_whitespace = true;
                    continue;
                }
            }
            
            // After struct keyword, find the name
            if seen_struct_keyword && !found_name && !in_whitespace && 
               (c == ' ' || c == '{' || c == ';' || c == ':') {
                found_name = true;
                name_end = j;
            }
            
            // Handle tags (like __attribute__)
            if c == '<' || c == '(' || c == '[' {
                in_tag = true;
            }
            if in_tag && (c == '>' || c == ')' || c == ']') {
                in_tag = false;
                j += 1;
                continue;
            }
            if in_tag {
                j += 1;
                continue;
            }
            
            // Find opening brace
            if c == '{' {
                found_brace = true;
                brace_pos = start + j;
                break;
            }
            
            // If we hit a semicolon before a brace, this is just a forward declaration
            if c == ';' && !found_brace {
                *i = start + j + 1;
                return false;
            }
            
            j += 1;
        }
        
        if !found_brace {
            // No opening brace, not a struct definition
            return false;
        }
        
        // Find the matching closing brace
        if let Some(closing_brace) = find_matching_brace(content, brace_pos) {
            // We have a full struct definition
            
            // Extract struct name
            let struct_content = &content[start..brace_pos];
            let mut name = if found_name {
                struct_content[name_start..name_end].trim().to_string()
            } else {
                "unnamed_struct".to_string()
            };
            
            // Check for typedef struct
            let is_typedef = struct_content.trim().starts_with("typedef");
            
            // Handle typedef struct Name { ... } Name; pattern
            if is_typedef {
                // Look for the name after the closing brace
                let after_brace = &content[closing_brace+1..];
                if let Some(typedef_end) = after_brace.find(';') {
                    let typedef_name = &after_brace[..typedef_end].trim();
                    if !typedef_name.is_empty() {
                        // Use typedef name instead
                        name = typedef_name.to_string();
                    }
                }
            }
            
            // Create a new struct
            let mut s = Struct::new();
            s.name = name;
            s.is_typedef = is_typedef;
            s.content = content[start..=closing_brace].to_string();
            
            // Parse struct body
            let body = content[brace_pos+1..closing_brace].trim().to_string();
            s.fields = self.parse_struct_fields(&body);
            
            // Add to structs list
            self.structs.push(s);
            
            // Update position to after struct definition
            *i = closing_brace + 1;
            
            // Look for semicolon after struct (especially for typedefs)
            let mut j = closing_brace + 1;
            while j < content.len() && content.as_bytes()[j] != b';' {
                j += 1;
                if j < content.len() && content.as_bytes()[j] == b'}' {
                    break; // Another closing brace means we're in nested definitions
                }
            }
            
            if j < content.len() && content.as_bytes()[j] == b';' {
                *i = j + 1;
            }
            
            return true;
        }
        
        // Couldn't find matching brace
        *i += 1;
        return false;
    }
    
    /// Extract a function definition
    fn extract_function(&mut self, i: &mut usize, content: &str) -> bool {
        let full_content = &content[*i..];
        if let Some(open_paren_pos) = full_content.find('(') {
            if let Some(close_paren_pos) = find_matching_paren(full_content, open_paren_pos) {
                if let Some(body_start_pos) = full_content.find('{') {
                    if body_start_pos > close_paren_pos {
                        if let Some(body_end_pos) = find_matching_brace(full_content, body_start_pos) {
                            let signature = &full_content[..open_paren_pos];
                            let name = self.extract_function_name(signature).unwrap_or_default();

                            if name.is_empty() || self.is_control_keyword(&name) {
                                return false;
                            }

                            let return_type = self.extract_function_return_type(signature);
                            let args = self.extract_function_args(&full_content[open_paren_pos + 1..close_paren_pos]);

                            if !self.functions.iter().any(|f| f.name == name) {
                                let mut func = Function::new();
                                func.name = name;
                                func.return_type = return_type.unwrap_or_else(|| "()".to_string());
                                func.args = args;
                                func.body = Some(full_content[body_start_pos + 1..body_end_pos].trim().to_string());
                                func.content = full_content[..=body_end_pos].to_string();
                                self.push_function(func);
                            }

                            *i += body_end_pos;
                            return true;
                        }
                    }
                }
            }
        }
        false
    }

    fn push_function(&mut self, function: Function) {
        self.functions.push(function);
    }

    fn extract_function_return_type(&self, header: &str) -> Option<String> {
        let parts: Vec<&str> = header.split_whitespace().collect();
        if parts.len() > 1 {
            let return_type_str = parts[..parts.len() - 1].join(" ");
            Some(convert_type(&return_type_str))
        } else {
            Some("()".to_string()) // Default to unit type for main or void
        }
    }

    fn extract_function_args(&self, header: &str) -> Vec<(String, String)> {
        let mut args = Vec::new();
        let args_str = header.trim();

        if args_str == "void" || args_str.is_empty() {
            return args;
        }

        for arg in args_str.split(',') {
            let arg_trimmed = arg.trim();
            if arg_trimmed.is_empty() {
                continue;
            }

            let mut type_parts = Vec::new();
            let mut name = "";
            let parts: Vec<&str> = arg_trimmed.split_whitespace().collect();

            if let Some(last_part) = parts.last() {
                let mut temp_name = *last_part;
                while temp_name.starts_with('*') {
                    temp_name = &temp_name[1..];
                }
                while temp_name.ends_with(']') {
                    if let Some(bracket_pos) = temp_name.rfind('[') {
                        temp_name = &temp_name[..bracket_pos];
                    } else {
                        break;
                    }
                }
                name = temp_name;
            }

            if parts.len() > 1 {
                type_parts.extend_from_slice(&parts[..parts.len() - 1]);
            } else {
                type_parts.extend_from_slice(&parts);
            }

            let mut type_str = type_parts.join(" ");
            let mut is_array = false;
            if let Some(last_part) = parts.last() {
                if last_part.contains('*') {
                    type_str.push_str(" *");
                }
                if last_part.contains('[') {
                    is_array = true;
                }
            }

            let mut rust_type = convert_type(&type_str.trim());
            if is_array {
                rust_type = format!("*mut {}", rust_type);
            }
            args.push((name.to_string(), rust_type));
        }
        args
    }

    fn extract_function_name(&self, header: &str) -> Option<String> {
        let parts: Vec<&str> = header.split_whitespace().collect();
        if let Some(last) = parts.last() {
            let mut name = *last;
            while name.starts_with('*') {
                name = &name[1..];
            }
            Some(name.to_string())
        } else {
            None
        }
    }

    fn is_control_keyword(&self, name: &str) -> bool {
        matches!(name, "if" | "for" | "while" | "switch" | "do" | "else")
    }

    fn parse_struct_fields(&self, body: &str) -> Vec<String> {
        body.split(';')
            .map(|s| s.trim().to_string())
            .filter(|s| !s.is_empty())
            .collect()
    }
}
