use crate::error::{ConversionError, ConversionResult};
use std::collections::HashSet;
use crate::types::Type;

/// Represents the type of field in a struct
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FieldType {
    /// Regular struct field
    Regular,
    /// Bitfield (field: bits)
    Bitfield,
    /// Function pointer
    FunctionPointer,
}

/// Represents a C struct and its Rust equivalent
///
/// The Struct struct holds information about a C struct,
/// including its name, fields, and supports converting bitfields.
#[derive(Debug, Clone)]
pub struct Struct {
    /// The name of the struct
    pub name: String,
    /// Fields in the struct
    pub fields: Vec<String>,
    /// Bitfields in the struct (field, size in bits)
    pub bitfields: Vec<(String, usize)>,
    /// Original C struct definition
    pub content: String,
    /// Field types for tracking what kind of fields exist
    pub field_types: Vec<FieldType>,
    /// Is this struct defined as a typedef
    pub is_typedef: bool,
}

impl Struct {
    /// Creates a new empty Struct
    pub fn new() -> Struct {
        Struct {
            name: String::new(),
            fields: Vec::new(),
            bitfields: Vec::new(),
            content: String::new(),
            field_types: Vec::new(),
            is_typedef: false,
        }
    }

    /// Converts a struct to Rust
    pub fn to_rust(&self) -> String {
        let mut output = String::new();
        
        // Add documentation
        output.push_str("#[derive(Debug, Clone, Copy)]\n");
        // Clean struct name - remove any "struct " prefix if present
        let mut clean_name = if self.name.starts_with("struct ") {
            self.name[7..].trim().to_string()
        } else if self.name.starts_with("union ") {
            self.name[6..].trim().to_string()
        } else if self.name.starts_with("enum ") {
            self.name[5..].trim().to_string()
        } else {
            self.name.clone()
        };
        
        // Additional cleaning to ensure no trailing parentheses or invalid characters
        clean_name = clean_name.trim_end_matches(|c: char| !c.is_alphanumeric() && c != '_').to_string();
        
        // Start struct definition - ensure the name is valid
        output.push_str(&format!("pub struct {} {{\n", clean_name));

        // Add fields with proper type conversion
        if self.fields.is_empty() {
            // For empty structs, add a placeholder field to ensure valid Rust syntax
            // or leave it empty based on preference
            // Leaving it empty is valid in Rust, unlike C where empty structs are not allowed
        } else {
            for i in 0..self.fields.len() {
                if i < self.field_types.len() {
                    let field_name = &self.fields[i];
                    let field_type = self.field_types[i];
                    
                    // Handle bitfield fields
                    if field_name.contains(':') {
                        let parts: Vec<&str> = field_name.split(':').collect();
                        if !parts.is_empty() {
                            let name = self.clean_field_name(parts[0]);
                            // Use appropriate Rust integer type based on field_type
                            output.push_str(&format!("    pub {}: {},\n", name, 
                                                    self.map_field_type_to_rust(&field_type)));
                        }
                    } else if field_name.contains('(') {
                        // This is likely a function pointer
                        output.push_str(&format!("    {}\n", self.handle_function_pointer(field_name)));
                    } else {
                        let name = self.clean_field_name(field_name);
                        output.push_str(&format!("    pub {}: {},\n", name, 
                                                self.map_field_type_to_rust(&field_type)));
                    }
                }
            }
        }
        
        // Close struct
        output.push_str("}\n\n");
        
        output
    }

    /// Converts a C struct to Rust with error handling
    pub fn convert_with_errors(&mut self, s: &str) -> ConversionResult<()> {
        self.content = s.to_string();
        let s = s.trim_start_matches("static");
        if s.contains("struct") && s.contains("{") && s.contains("}") {
            self.parse_struct_definition(s)?;
            Ok(())
        } else {
            Err(ConversionError::ParseError {
                source: s.to_string(),
                line: 0,
                position: 0,
                message: "Invalid struct definition".to_string(),
            })
        }
    }
    
    /// Converts a C struct to Rust
    pub fn convert(&mut self, s: &str) {
        if let Err(e) = self.convert_with_errors(s) {
            eprintln!("Error converting struct: {}", e);
        }
    }

    /// Parses a C struct definition
    fn parse_struct_definition(&mut self, s: &str) -> ConversionResult<()> {
        if !s.contains("struct") {
            return Err(ConversionError::ParseError {
                source: s.to_string(),
                line: 0,
                position: 0,
                message: "Invalid struct definition".to_string(),
            });
        }
        self.extract_struct_name(s)?;
        self.extract_struct_fields(s)?;
        self.handle_bitfields();
        Ok(())
    }
    
    /// Handles bitfields in the struct
    pub fn handle_bitfields(&mut self) {
        // Clone fields to avoid borrow checker issues
        let fields = self.fields.clone();
        
        // Collect bitfields to process
        let mut bitfield_indices = Vec::new();
        for (i, field) in fields.iter().enumerate() {
            // Use character literal for colon check
            if field.contains(':') {
                if let Some(bitfield_info) = self.parse_bitfield(field) {
                    self.bitfields.push(bitfield_info);
                    bitfield_indices.push(i);
                }
            }
        }
        
        // Mark fields as bitfields
        for i in bitfield_indices {
            if i < self.field_types.len() {
                self.field_types[i] = FieldType::Bitfield;
            }
        }
    }

    /// Extracts the struct name from a C struct definition
    fn extract_struct_name(&mut self, s: &str) -> ConversionResult<()> {
        if let Some(name_part) = s.split('{').next() {
            let trimmed = name_part.trim();
            
            // Extract the basic name without any trailing non-alphanumeric characters
            let extract_clean_name = |raw_name: &str| -> String {
                // First trim the string
                let name = raw_name.trim();
                
                // Remove any trailing parenthesis, brackets, and other non-alphanumeric chars
                let name = name.trim_end_matches(|c: char| !c.is_alphanumeric() && c != '_');
                
                // Handle type name declarations like "example_t"
                let name = if name.ends_with("_t") {
                    // Remove _t suffix for more idiomatic Rust
                    name.trim_end_matches("_t")
                } else {
                    name
                };

                // Clean the struct name - keep only alphanumeric and underscore characters
                // and ensure it's a valid Rust identifier
                let clean_name: String = name.chars()
                    .take_while(|&c| c.is_alphanumeric() || c == '_')
                    .collect();
                
                if clean_name.is_empty() {
                    "UnnamedStruct".to_string()
                } else {
                    clean_name
                }
            };
            if trimmed.starts_with("union") {
                let name = trimmed.trim_start_matches("union").trim();
                self.name = extract_clean_name(name);
                return Ok(());
            }
            // Handle "const struct" case
            if trimmed.starts_with("const struct") || trimmed.starts_with("struct const") {
                let name = trimmed.trim_start_matches("const struct").trim_start_matches("struct const").trim();
                self.name = extract_clean_name(name);
                return Ok(());
            }
            // Handle regular "struct" case
            else if let Some(name) = trimmed.strip_prefix("struct").map(|n| n.trim()) {
                self.name = extract_clean_name(name);
                return Ok(());
            } 
            // Handle typedef struct case
            else if trimmed.starts_with("typedef struct") {
                // Mark this as a typedef struct
                self.is_typedef = true;
                
                // Extract the name after typedef struct
                let name = trimmed.trim_start_matches("typedef struct").trim();
                self.name = extract_clean_name(name);
                return Ok(());
            }
            // Handle anonymous struct with typedef name at end
            else if s.contains('}') && s.contains(';') {
                let parts: Vec<&str> = s.split('}').collect();
                if parts.len() > 1 {
                    let after_closing_brace = parts[1].trim();
                    if after_closing_brace.starts_with(';') {
                        // Anonymous struct, use generic name
                        self.name = "AnonymousStruct".to_string();
                    } else {
                        // typedef struct { ... } TypeName;
                        let typedef_name = extract_clean_name(after_closing_brace.split(';').next().unwrap_or(""));
                        if !typedef_name.is_empty() {
                            self.name = typedef_name;
                            self.is_typedef = true;
                        } else {
                            self.name = "AnonymousStruct".to_string();
                        }
                    }
                    return Ok(());
                }
            }
        }
        
        // If we got here, we couldn't extract a name properly
        // Use a generic name to avoid errors
        self.name = "UnnamedStruct".to_string();
        Ok(())
    }

    /// Extracts the struct fields from a C struct definition
    fn extract_struct_fields(&mut self, s: &str) -> ConversionResult<()> {
        if let Some(fields_part) = s.split('{').nth(1) {
            if let Some(fields_content) = fields_part.split('}').nth(0) {
                for field_line in fields_content.split(';') {
                    let field = field_line.trim();
                    if field.is_empty() {
                        continue;
                    }
                    
                    // Check if it's a bitfield
                    if field.contains(':') || field.contains('?') {
                        if let Some(bitfield) = self.convert_bitfield(field) {
                            let bit_size = self.extract_bitfield_size(field);
                            self.fields.push(bitfield);
                            self.bitfields.push((field.to_string(), bit_size));
                            self.field_types.push(FieldType::Bitfield);
                        }
                    } else {
                        // Regular field
                        if self.parse_field(field) {
                            continue;
                        }
                        
                        if let Some(converted_field) = self.convert_field(field) {
                            self.fields.push(converted_field);
                            self.field_types.push(FieldType::Regular);
                        }
                    }
                }
                return Ok(());
            }
        }
        
        Err(ConversionError::ParseError {
            source: s.to_string(),
            line: 0,
            position: 0,
            message: "Failed to extract struct fields".to_string(),
        })
    }
    
    /// Converts a C struct field to Rust
    fn convert_bitfield(&self, field: &str) -> Option<String> {
        let parts: Vec<&str> = field.split(':').collect();
        if parts.len() < 2 {
            return None;
        }
        
        let field_def = parts[0].trim();
        let bits = parts[1].trim().parse::<usize>().unwrap_or(1);
        
        // Convert to appropriate sized integer based on bit count
        let field_parts: Vec<&str> = field_def.split_whitespace().collect();
        if field_parts.len() < 2 {
            return None;
        }
        
        let field_name = field_parts.last().unwrap();
        let int_type = match bits {
            0..=8 => "u8",
            9..=16 => "u16",
            17..=32 => "u32",
            33..=64 => "u64",
            _ => "u128"
        };
        
        Some(format!("pub {}: {} /* {} bits */", field_name, int_type, bits))
    }
    
    /// Extracts the size of a bitfield in bits
    fn extract_bitfield_size(&self, field: &str) -> usize {
        let parts: Vec<&str> = field.split(':').collect();
        if parts.len() < 2 {
            return 0;
        }
        
        parts[1].trim().parse::<usize>().unwrap_or(0)
    }
    
    /// Extracts bitfield information from field declaration
    fn parse_bitfield(&mut self, field: &str) -> Option<(String, usize)> {
        // Bitfields are in format: type field_name : bit_size;
        // Extract name and bit_size
        
        // Parse bitfield information
        let parts: Vec<&str> = field.split(':').collect();
        if parts.len() != 2 {
            return None;
        }
        
        // Extract field name (it's the last word before the colon)
        let field_parts: Vec<&str> = parts[0].trim().split_whitespace().collect();
        if field_parts.is_empty() {
            return None;
        }
        
        let field_name = field_parts.last().unwrap();
        
        // Clean the field name
        let field_name = self.clean_field_name(field_name);
        
        // Extract bit size (it's the first number after the colon)
        let size_part = parts[1].trim();
        let bits: usize = size_part.split_whitespace()
            .next()
            .unwrap_or("0")
            .parse()
            .unwrap_or(0);
        
        Some((field_name.to_string(), bits))
    }

    /// Clean field name for Rust output
    pub fn clean_field_name(&self, name: &str) -> String {
        // Check for "field.return something" pattern and fix it to "return field.something"
        let name_parts: Vec<&str> = name.split('.').collect();
        if name_parts.len() == 2 && name_parts[1].starts_with("return") {
            let field_name = name_parts[0];
            // Extract what comes after "return" in the second part
            let return_content = name_parts[1].trim_start_matches("return").trim_start();
            // Return fixed version
            return format!("return {}.{}", field_name, return_content);
        }

        let name = name.trim();

        // Handle numeric-only field names by adding an underscore prefix
        if !name.is_empty() && name.chars().all(|c| c.is_numeric()) {
            return format!("_{}", name);
        }

        // Escape Rust keywords
        Type::escape(name)
    }

    /// Parse field from C struct definition and add to fields list
    pub fn parse_field(&mut self, field: &str) -> bool {
        // Skip empty fields
        if field.trim().is_empty() {
            return true;
        }
        
        // Skip comments
        if field.trim().starts_with("//") || field.trim().starts_with("/*") {
            return true;
        }
        
        // Remove struct keyword from field type declarations
        let field = field.replace("struct ", "");
        
        // Clean up the field to get the Rust representation
        let field_name = self.extract_field_name(&field);
        let field_type = self.extract_field_type(&field);
        
        if !field_name.is_empty() && !field_type.is_empty() {
            self.fields.push(format!("{}: {}", field_name, field_type));
            self.field_types.push(FieldType::Regular);
            return true;
        }
        
        false
    }
    
    /// Extract field name from C field declaration
    fn extract_field_name(&self, field: &str) -> String {
        // Strip comments
        let field = field.split("//").next().unwrap_or("").trim();
        let field = field.split("/*").next().unwrap_or("").trim();
        
        // Extract field name - it's usually the last part before any array brackets or bitfield colons
        let mut parts = field.split_whitespace().collect::<Vec<&str>>();
        
        if parts.is_empty() {
            return String::new();
        }
        
        // Last part is the field name
        let mut field_name = parts.pop().unwrap_or("").trim().to_string();
        
        // Remove array brackets if present
        if field_name.contains('[') {
            field_name = field_name.split('[').next().unwrap_or("").trim().to_string();
        }
        
        // Remove colon and after for bitfields
        if field_name.contains(':') {
            field_name = field_name.split(':').next().unwrap_or("").trim().to_string();
        }
        
        // Clean the field name
        self.clean_field_name(&field_name)
    }
    
    /// Extract field type from C field declaration
    fn extract_field_type(&self, field: &str) -> String {
        // Strip comments
        let field = field.split("//").next().unwrap_or("").trim();
        let field = field.split("/*").next().unwrap_or("").trim();
        
        // Split by whitespace
        let parts = field.split_whitespace().collect::<Vec<&str>>();
        
        if parts.len() < 2 {
            return String::new();
        }
        
        // Type is everything except the last part (field name)
        let type_parts = &parts[0..parts.len() - 1];
        let c_type = type_parts.join(" ");
        
        // Convert C type to Rust type
        self.map_c_type_to_rust(&c_type)
    }

    /// Converts a C field to Rust
    fn convert_field(&self, field: &str) -> Option<String> {
        // Split the field into parts (e.g., "int foo" -> ["int", "foo"])
        let parts: Vec<&str> = field.trim().split_whitespace().collect();
        if parts.is_empty() {
            return None;
        }
        
        // Handle bitfields
        if field.contains(':') {
            return None;  // Skip bitfields as they're handled separately
        }
        
        // Handle function pointers which often have parentheses
        if field.contains('(') && field.contains(')') {
            return Some(self.handle_function_pointer(field));
        }
        
        // Handle standard fields
        let mut field_name = parts.last().unwrap_or(&"").trim().to_string();
        
        // Clean the field name before proceeding
        field_name = self.clean_field_name(&field_name);
        
        // Handle pointer types and extract actual field name
        if field_name.contains('*') {
            // Field name is the part after the last *
            let name_parts: Vec<&str> = field_name.split('*').collect();
            field_name = name_parts.last().unwrap_or(&"").trim().to_string();
        }
        
        // Remove trailing comma or other punctuation
        field_name = field_name.trim_end_matches(|c: char| !c.is_alphanumeric() && c != '_').to_string();
        
        // Handle C arrow notation (ptr->field)
        if field_name.contains("->") {
            let arrow_parts: Vec<&str> = field_name.split("->").collect();
            field_name = arrow_parts.last().unwrap_or(&"").trim().to_string();
        }
        
        // Remove C modifiers like "pub" that shouldn't be part of the field name
        let rust_modifiers = ["pub", "mut", "const", "static", "unsafe"];
        for modifier in rust_modifiers.iter() {
            if field_name.starts_with(modifier) {
                field_name = field_name[modifier.len()..].trim().to_string();
            }
        }
        
        // Handle empty field names
        if field_name.is_empty() {
            return Some(format!("pub unnamed_field: {}", self.map_c_type_to_rust(&parts[..parts.len() - 1].join(" "))));
        }
        
        // Handle numeric-only field names (not valid in Rust)
        if field_name.chars().all(|c| c.is_numeric()) {
            field_name = format!("field_{}", field_name);
        }
        
        // Handle Rust keywords
        let field_name = self.escape_keyword(&field_name);
        
        let type_parts = &parts[..parts.len() - 1];
        let c_type_str = type_parts.join(" ");
        
        // Map C type to Rust type
        let rust_type = self.map_c_type_to_rust(&c_type_str);
        
        // Handle arrays
        if field_name.contains('[') {
            let name_parts: Vec<&str> = field_name.split('[').collect();
            let name = name_parts[0].trim();
            let array_parts = &name_parts[1..].join("[");
            let array_size = array_parts.trim_end_matches(']').trim();
            
            Some(format!("pub {}: [{}; {}]", self.escape_keyword(name), rust_type, array_size))
        } else {
            Some(format!("pub {}: {}", field_name, rust_type))
        }
    }

    /// Handles a function pointer field in a struct
    fn handle_function_pointer(&self, field: &str) -> String {
        // Extract function name, args and return type
        let field_parts: Vec<&str> = field.trim().split('(').collect();
        
        if field_parts.len() < 2 {
            return format!("pub unknown_func: Option<unsafe extern \"C\" fn()>");
        }
        
        // Get return type and name
        let prefix = field_parts[0].trim();
        let prefix_parts: Vec<&str> = prefix.split_whitespace().collect();
        
        if prefix_parts.is_empty() {
            return format!("pub unknown_func: Option<unsafe extern \"C\" fn()>");
        }
        
        let name = prefix_parts.last().unwrap().trim();
        let return_type = if prefix_parts.len() > 1 {
            prefix_parts[..prefix_parts.len()-1].join(" ")
        } else {
            "void".to_string()
        };
        
        // Convert to Rust function pointer type
        let rust_return_type = self.map_c_type_to_rust(&return_type);
        
        // Simplified: just use a generic function pointer type
        format!("pub {}: Option<unsafe extern \"C\" fn() -> {}>", name, rust_return_type)
    }

    /// Maps field type to Rust type
    fn map_field_type_to_rust(&self, field_type: &FieldType) -> String {
        match field_type {
            FieldType::Regular => "u32".to_string(),
            FieldType::Bitfield => "u8".to_string(),
            FieldType::FunctionPointer => "Option<unsafe extern \"C\" fn() -> ()>".to_string(),
        }
    }

    /// Maps C types to Rust types
    fn map_c_type_to_rust(&self, c_type: &str) -> String {
        Type::convert(c_type)
    }

    /// Escapes Rust keywords by adding an underscore
    fn escape_keyword(&self, name: &str) -> String {
        Type::escape(name)
    }
    
    /// Gets the length of the struct definition
    pub fn len(&self) -> usize {
        self.content.len()
    }
    
    /// Checks if the struct is empty
    pub fn is_empty(&self) -> bool {
        self.name.is_empty() && self.fields.is_empty()
    }
    
    /// Merges fields from another struct definition with the same name
    pub fn merge(&mut self, other: &Struct) {
        // Only merge if the names match
        if self.name != other.name {
            return;
        }
        
        // Add any fields that don't already exist
        let existing_fields: HashSet<_> = self.fields.iter().cloned().collect();
        
        for (i, field) in other.fields.iter().enumerate() {
            if !existing_fields.contains(field) {
                self.fields.push(field.clone());
                
                // Add corresponding field type if available
                if i < other.field_types.len() {
                    self.field_types.push(other.field_types[i].clone());
                } else {
                    // Default to regular field if type info is missing
                    self.field_types.push(FieldType::Regular);
                }
            }
        }
        
        // Add bitfields
        for bitfield in &other.bitfields {
            if !self.bitfields.contains(bitfield) {
                self.bitfields.push(bitfield.clone());
            }
        }
        
        // Append content for reference
        if !self.content.contains(&other.content) {
            self.content.push_str("\n/* Additional definition */\n");
            self.content.push_str(&other.content);
        }
    }
}
