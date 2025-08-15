use crate::error::{ConversionError, ConversionResult};

/// Represents a C variable and its Rust equivalent
///
/// The Variable struct holds information about a C variable,
/// including its name, type, and value.
#[derive(Debug, Clone)]
pub struct Variable {
    /// Original content of the variable declaration
    pub content: String,
    /// Name of the variable
    pub name: String,
    /// Type of the variable
    pub var_type: String,
    /// Value of the variable, if any
    pub value: String,
    /// Length of the variable declaration in characters
    pub len: usize,
}

impl Variable {
    /// Creates a new empty Variable
    pub fn new() -> Variable {
        Variable {
            content: String::from(""),
            name: String::from(""),
            var_type: String::from(""),
            value: String::from(""),
            len: 0,
        }
    }

    /// Generates Rust code for this variable
    pub fn output(&self) -> String {
        if self.is_const() {
            // Constants in Rust
            if self.value.is_empty() {
                format!("pub const {}: {}; // Warning: Uninitialized const\n", self.name, self.var_type)
            } else {
                format!("pub const {}: {} = {};\n", self.name, self.var_type, self.value)
            }
        } else if self.is_static() {
            // Static variables in Rust
            if self.value.is_empty() {
                format!("pub static {}: {}; // Warning: Uninitialized static\n", self.name, self.var_type)
            } else {
                format!("pub static {}: {} = {};\n", self.name, self.var_type, self.value)
            }
        } else {
            // Regular variables - these would normally be in functions, not at module level
            if self.value.is_empty() {
                format!("// Regular variable: let mut {}: {};\n", self.name, self.var_type)
            } else {
                format!("// Regular variable: let mut {}: {} = {};\n", self.name, self.var_type, self.value)
            }
        }
    }

    /// Checks if this is a const variable
    pub fn is_const(&self) -> bool {
        self.content.contains("const ") && !self.content.contains("static ")
    }

    /// Checks if this is a static variable
    pub fn is_static(&self) -> bool {
        self.content.contains("static ") || 
        self.content.contains("EXPORT_SYMBOL") || 
        self.name.starts_with("MNL_")
    }

    /// Converts a C variable to Rust with error handling
    pub fn convert_with_errors(&mut self, s: &str) -> ConversionResult<()> {
        self.content = s.to_string();
        
        // Extract variable details
        if s.contains("=") {
            // Variable with initialization
            let parts: Vec<&str> = s.split('=').collect();
            if parts.len() < 2 {
                return Err(ConversionError::ParseError {
                    source: s.to_string(),
                    line: 0,
                    position: 0,
                    message: "Failed to parse variable declaration with initialization".to_string(),
                });
            }
            
            self.extract_name_and_type(parts[0])?;
            self.value = parts[1].trim().trim_end_matches(';').to_string();
        } else {
            // Variable without initialization
            self.extract_name_and_type(s)?;
        }
        
        Ok(())
    }
    
    /// Converts a C variable to Rust
    pub fn convert(&mut self, s: &str) {
        if let Err(e) = self.convert_with_errors(s) {
            eprintln!("Error converting variable: {}", e);
        }
    }
    
    /// Extracts the variable name and type from the declaration
    fn extract_name_and_type(&mut self, decl: &str) -> ConversionResult<()> {
        let clean_decl = decl.trim()
            .replace("EXPORT_SYMBOL", "")
            .trim()
            .to_string();
        
        let parts: Vec<&str> = clean_decl.split_whitespace().collect();
        if parts.is_empty() {
            return Err(ConversionError::ParseError {
                source: decl.to_string(),
                line: 0,
                position: 0,
                message: "Invalid variable declaration format".to_string(),
            });
        }
        
        // Last part might contain the variable name, possibly with array syntax
        let last_part = parts.last().unwrap().trim();
        
        // Handle array declarations like "int array[10];"
        if last_part.contains('[') {
            let name_parts: Vec<&str> = last_part.split('[').collect();
            self.name = name_parts[0].trim().to_string();
            
            // Everything before the variable name is the type
            let type_str = if parts.len() > 1 {
                parts[..parts.len() - 1].join(" ")
            } else {
                String::new()
            };
            
            // Extract array dimensions
            let array_dims = last_part
                .trim_start_matches(|c| c != '[')
                .trim_end_matches(';');
            
            // Convert to Rust array type
            let base_type = self.convert_type(&type_str);
            let array_size = array_dims
                .trim_start_matches('[')
                .trim_end_matches(']');
            
            self.var_type = format!("[{}; {}]", base_type, array_size);
        } else {
            // Regular variable without array syntax
            self.name = last_part.trim_end_matches(';').to_string();
            
            // Check for pointer variables (name contains *)
            if self.name.contains('*') {
                let name_parts: Vec<&str> = self.name.split('*').collect();
                self.name = name_parts.last().unwrap_or(&"").trim().to_string();
                
                // Adjust the parts list to include the pointer in the type
                let mut type_parts = parts[..parts.len() - 1].to_vec();
                type_parts.push("*");
                let type_str = type_parts.join(" ");
                self.var_type = self.convert_type(&type_str);
            } else {
                // Regular non-pointer variable
                let type_str = if parts.len() > 1 {
                    parts[..parts.len() - 1].join(" ")
                } else {
                    String::new()
                };
                self.var_type = self.convert_type(&type_str);
            }
        }
        
        Ok(())
    }
    
    /// Converts a C type to Rust type
    fn convert_type(&self, type_str: &str) -> String {
        // Handle complex pointer types first
        if type_str.contains('*') {
            return self.convert_pointer_type(type_str);
        }
        
        // Strip qualifiers and extract base type
        let mut cleaned_type = type_str.to_string();
        
        // Remove common C qualifiers
        for qualifier in &["const", "volatile", "restrict", "static", "extern"] {
            cleaned_type = cleaned_type.replace(&format!("{} ", qualifier), "");
        }
        
        let cleaned_type = cleaned_type.trim();
        
        // Map to Rust types
        match cleaned_type {
            "void" => "()".to_string(),
            "char" => "i8".to_string(),
            "unsigned char" => "u8".to_string(),
            "short" => "i16".to_string(),
            "unsigned short" => "u16".to_string(),
            "int" => "i32".to_string(),
            "unsigned int" => "u32".to_string(),
            "long" => "i64".to_string(),
            "unsigned long" => "u64".to_string(),
            "long long" => "i64".to_string(), 
            "unsigned long long" => "u64".to_string(),
            "float" => "f32".to_string(),
            "double" => "f64".to_string(),
            "size_t" => "usize".to_string(),
            "ssize_t" => "isize".to_string(),
            "int8_t" => "i8".to_string(),
            "uint8_t" => "u8".to_string(),
            "int16_t" => "i16".to_string(),
            "uint16_t" => "u16".to_string(),
            "int32_t" => "i32".to_string(),
            "uint32_t" => "u32".to_string(),
            "int64_t" => "i64".to_string(),
            "uint64_t" => "u64".to_string(),
            // Handle structs
            t if t.starts_with("struct ") => {
                t.strip_prefix("struct ").unwrap_or(t).trim().to_string()
            },
            // Default case - use as is
            _ => cleaned_type.to_string(),
        }
    }
    
    /// Converts a C pointer type to a Rust pointer type
    fn convert_pointer_type(&self, type_str: &str) -> String {
        let is_const = type_str.contains("const");
        let pointer_count = type_str.chars().filter(|&c| c == '*').count();
        
        // Remove pointer stars and const keywords
        let base_type = type_str
            .replace("*", "")
            .replace("const", "")
            .trim()
            .to_string();
        
        // Convert the base type
        let rust_base_type = self.convert_type(&base_type);
        
        // Build the pointer type string
        let mut result = rust_base_type;
        for _ in 0..pointer_count {
            result = format!("*{} {}", 
                if is_const { "const" } else { "mut" }, 
                result);
        }
        
        result
    }
    
    /// Gets the length of the variable declaration
    pub fn len(&self) -> usize {
        self.len
    }
    
    /// Checks if the variable is empty
    pub fn is_empty(&self) -> bool {
        self.name.is_empty() && self.value.is_empty()
    }
}
