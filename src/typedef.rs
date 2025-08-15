use crate::error::{ConversionError, ConversionResult};

/// Represents a C typedef and its Rust equivalent
///
/// The Typedef struct holds information about a C typedef,
/// including the alias name and the original type.
#[derive(Debug, Clone)]
pub struct Typedef {
    /// Original content of the typedef declaration
    pub content: String,
    /// Alias name (the new type name)
    pub name: String,
    /// Original type being aliased
    pub original_type: String,
    /// Length of the typedef declaration in characters
    pub len: usize,
}

impl Typedef {
    /// Creates a new empty Typedef
    pub fn new() -> Typedef {
        Typedef {
            content: String::from(""),
            name: String::from(""),
            original_type: String::from(""),
            len: 0,
        }
    }

    /// Generates Rust code for this typedef
    pub fn output(&self) -> String {
        // Simple type alias for basic typedefs
        format!("pub type {} = {};\n", self.name, self.original_type)
    }
    
    /// Converts a C typedef to Rust with error handling
    pub fn convert_with_errors(&mut self, s: &str) -> ConversionResult<()> {
        self.content = s.to_string();
        
        if s.starts_with("typedef") {
            self.parse_typedef(s)?;
            Ok(())
        } else {
            Err(ConversionError::ParseError {
                source: s.to_string(),
                line: 0,
                position: 0,
                message: "Invalid typedef declaration".to_string(),
            })
        }
    }
    
    /// Converts a C typedef to Rust
    pub fn convert(&mut self, s: &str) {
        if let Err(e) = self.convert_with_errors(s) {
            eprintln!("Error converting typedef: {}", e);
        }
    }
    /// Parses a C typedef declaration
    fn parse_typedef(&mut self, s: &str) -> ConversionResult<()> {
        // Strip 'typedef' and semicolon
        let content = s.trim_start_matches("typedef").trim();
        let content = content.trim_end_matches(';').trim();
        
        // Check for struct typedefs like: typedef struct { ... } Name;
        if content.starts_with("struct") && content.contains('{') && content.contains('}') {
            return self.parse_struct_typedef(content);
        }
        
        // Check for enum typedefs like: typedef enum { ... } Name;
        if content.starts_with("enum") && content.contains('{') && content.contains('}') {
            return self.parse_enum_typedef(content);
        }
        
        // Handle simple typedefs like: typedef int NewType;
        let parts: Vec<&str> = content.split_whitespace().collect();
        if parts.len() >= 2 {
            // Last part is usually the new type name
            self.name = parts.last().unwrap().to_string();
            
            // Everything before the last part is the original type
            let original_type = &parts[..parts.len() - 1].join(" ");
            self.original_type = self.convert_type(original_type);
            
            return Ok(());
        }
        
        Err(ConversionError::ParseError {
            source: s.to_string(),
            line: 0,
            position: 0,
            message: "Failed to parse typedef".to_string(),
        })
    }
    
    /// Parses a struct typedef declaration
    fn parse_struct_typedef(&mut self, s: &str) -> ConversionResult<()> {
        // For struct typedefs, we'll extract the name after the closing brace
        if let Some(after_brace) = s.split('}').nth(1) {
            let name = after_brace.trim();
            if !name.is_empty() {
                self.name = name.to_string();
                self.original_type = "struct".to_string(); // Simplified - real impl would extract full struct definition
                return Ok(());
            }
        }
        
        Err(ConversionError::ParseError {
            source: s.to_string(),
            line: 0,
            position: 0,
            message: "Failed to parse struct typedef".to_string(),
        })
    }
    
    /// Parses an enum typedef declaration
    fn parse_enum_typedef(&mut self, s: &str) -> ConversionResult<()> {
        // For enum typedefs, we'll extract the name after the closing brace
        if let Some(after_brace) = s.split('}').nth(1) {
            let name = after_brace.trim();
            if !name.is_empty() {
                self.name = name.to_string();
                self.original_type = "enum".to_string(); // Simplified - real impl would extract full enum definition
                return Ok(());
            }
        }
        
        Err(ConversionError::ParseError {
            source: s.to_string(),
            line: 0,
            position: 0,
            message: "Failed to parse enum typedef".to_string(),
        })
    }
    
    /// Converts a C type to Rust type
    fn convert_type(&self, type_str: &str) -> String {
        // Basic type conversion, would be more sophisticated in real implementation
        match type_str.trim() {
            "int" => "i32".to_string(),
            "unsigned int" => "u32".to_string(),
            "char" => "i8".to_string(),
            "unsigned char" => "u8".to_string(),
            "short" => "i16".to_string(),
            "unsigned short" => "u16".to_string(),
            "long" => "i64".to_string(),
            "unsigned long" => "u64".to_string(),
            "float" => "f32".to_string(),
            "double" => "f64".to_string(),
            "void" => "()".to_string(),
            "void*" => "*mut c_void".to_string(),
            _ => {
                if type_str.contains("*") {
                    // Handle pointers
                    let base_type = type_str.replace("*", "").trim().to_string();
                    format!("*mut {}", self.convert_type(&base_type))
                } else if type_str.starts_with("struct") {
                    // Handle struct types
                    type_str.replace("struct", "").trim().to_string()
                } else if type_str.starts_with("enum") {
                    // Handle enum types
                    type_str.replace("enum", "").trim().to_string()
                } else if type_str.starts_with("const") {
                    // Handle const types
                    let base_type = type_str.replace("const", "").trim().to_string();
                    format!("const {}", self.convert_type(&base_type))
                } else {
                    // Pass through as-is
                    type_str.to_string()
                }
            }
        }
    }
    
    /// Gets the length of the typedef declaration
    pub fn len(&self) -> usize {
        self.len
    }
    
    /// Checks if the typedef is empty
    pub fn is_empty(&self) -> bool {
        self.name.is_empty() && self.original_type.is_empty()
    }
}
