use crate::error::{ConversionError, ConversionResult};

/// Represents a C enum and its Rust equivalent
///
/// The Enum struct holds information about a C enum,
/// including its name and variants.
#[derive(Debug, Clone)]
pub struct Enum {
    /// Original content of the enum definition
    pub content: String,
    /// Name of the enum
    pub name: String,
    /// Variants of the enum
    pub variants: Vec<(String, Option<String>)>,
    /// Length of the enum definition in characters
    pub len: usize,
}

impl Enum {
    /// Creates a new empty Enum
    pub fn new() -> Enum {
        Enum {
            content: String::from(""),
            name: String::from(""),
            variants: Vec::new(),
            len: 0,
        }
    }

    /// Generates Rust code for this enum
    pub fn output(&self) -> String {
        let mut s = String::new();
        
        // Add derive attributes
        s.push_str("#[derive(Debug, Clone, Copy, PartialEq, Eq)]\n");
        
        // Add representation (C enums are usually i32 in Rust)
        s.push_str("#[repr(C)]\n");
        
        // Define the enum
        s.push_str(&format!("pub enum {} {{\n", self.name));
        
        // Add all variants
        for (variant, value) in &self.variants {
            if let Some(val) = value {
                s.push_str(&format!("    {} = {},\n", variant, val));
            } else {
                s.push_str(&format!("    {},\n", variant));
            }
        }
        
        s.push_str("}\n");
        s
    }
    
    /// Converts a C enum to Rust with error handling
    pub fn convert_with_errors(&mut self, s: &str) -> ConversionResult<()> {
        self.content = s.to_string();
        
        if s.contains("enum") && s.contains("{") && s.contains("}") {
            self.parse_enum_definition(s)?;
            Ok(())
        } else {
            Err(ConversionError::ParseError {
                source: s.to_string(),
                line: 0,
                position: 0,
                message: "Invalid enum definition".to_string(),
            })
        }
    }
    
    /// Converts a C enum to Rust
    pub fn convert(&mut self, s: &str) {
        if let Err(e) = self.convert_with_errors(s) {
            eprintln!("Error converting enum: {}", e);
        }
    }
    
    /// Parses a C enum definition
    fn parse_enum_definition(&mut self, s: &str) -> ConversionResult<()> {
        self.extract_enum_name(s)?;
        self.extract_enum_variants(s)?;
        
        Ok(())
    }
    
    /// Extracts the enum name from a C enum definition
    fn extract_enum_name(&mut self, s: &str) -> ConversionResult<()> {
        if let Some(name_part) = s.split('{').next() {
            if let Some(name) = name_part.trim().strip_prefix("enum").map(|n| n.trim()) {
                self.name = name.to_string();
                return Ok(());
            }
        }
        
        Err(ConversionError::ParseError {
            source: s.to_string(),
            line: 0,
            position: 0,
            message: "Failed to extract enum name".to_string(),
        })
    }
    
    /// Extracts the enum variants from a C enum definition
    fn extract_enum_variants(&mut self, s: &str) -> ConversionResult<()> {
        if let Some(variants_part) = s.split('{').nth(1) {
            if let Some(variants_content) = variants_part.split('}').next() {
                for variant in variants_content.split(',') {
                    let variant = variant.trim();
                    if variant.is_empty() {
                        continue;
                    }
                    
                    // Check if variant has a value
                    if variant.contains('=') {
                        let parts: Vec<&str> = variant.split('=').collect();
                        if parts.len() >= 2 {
                            let name = parts[0].trim().to_string();
                            let value = parts[1].trim().to_string();
                            self.variants.push((name, Some(value)));
                        }
                    } else {
                        // Simple variant without value
                        self.variants.push((variant.to_string(), None));
                    }
                }
                return Ok(());
            }
        }
        
        Err(ConversionError::ParseError {
            source: s.to_string(),
            line: 0,
            position: 0,
            message: "Failed to extract enum variants".to_string(),
        })
    }
    
    /// Gets the length of the enum definition
    pub fn len(&self) -> usize {
        self.len
    }
    
    /// Checks if the enum is empty
    pub fn is_empty(&self) -> bool {
        self.name.is_empty() && self.variants.is_empty()
    }
}
