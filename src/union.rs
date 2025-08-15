#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Union {
    pub name: String,
    pub fields: Vec<String>,
    pub len: usize,
    pub position: usize,
    pub offset: usize
}

impl Union {
    pub fn new() -> Union {
        Union {
            name: String::new(),
            fields: Vec::new(),
            len: 0,
            position: 0,
            offset: 0
        }
    }

    pub fn output(&self) -> String {
        let mut s = String::new();
        // Add derive attributes for common traits
        s.push_str("#[derive(Debug, Clone, Copy)]\n");
        // Add repr(C) to maintain memory layout compatibility with C unions
        s.push_str("#[repr(C)]\n");
        s.push_str(&format!("pub union {} {{\n", self.name));

        for field in &self.fields {
            s.push_str(&format!("    {},\n", field));
        }

        s.push_str("}\n"); // Added newline character here
        s
    }

    pub fn convert(&mut self, s: &str) {
        if s.contains("union") && s.contains("{") && s.contains("}") {
            self.parse_union_definition(s);
        } else {
            self.parse_simple_format(s);
        }
        self.len = self.fields.len();
    }

    fn parse_union_definition(&mut self, s: &str) {
        self.extract_union_name(s);
        self.extract_union_fields(s);
    }

    fn extract_union_name(&mut self, s: &str) {
        if let Some(name_part) = s.split('{').next() {
            if let Some(name) = name_part.trim().strip_prefix("union").map(|n| n.trim()) {
                self.name = name.to_string();
            }
        }
    }

    fn extract_union_fields(&mut self, s: &str) {
        if let Some(fields_part) = s.split('{').nth(1).and_then(|s| s.split('}').next()) {
            // Split by semicolons to get individual field declarations
            self.fields = fields_part
                .split(';')
                .filter_map(|field| {
                    let field = field.trim();
                    if !field.is_empty() {
                        Some(self.convert_field(field).unwrap_or_else(|| field.to_string()))
                    } else {
                        None
                    }
                })
                .collect();
        }
    }

    fn convert_field(&self, field: &str) -> Option<String> {
        // Remove any comments
        let field = field.split("//").next()?.trim();
        
        // Check if field has a type and name
        let parts: Vec<&str> = field.rsplitn(2, ' ').collect();
        if parts.len() < 2 {
            return None;
        }
        
        let name = parts[0].trim();
        let type_str = parts[1].trim();
        
        // Convert the C type to Rust type
        let rust_type = self.convert_type(type_str);
        
        Some(format!("pub {}: {}", name, rust_type))
    }

    fn convert_type(&self, c_type: &str) -> String {
        // Check for pointers, arrays, structs, etc.
        if c_type.starts_with("struct") {
            c_type.replace("struct", "").trim().to_string()
        } else if c_type.contains("*") {
            self.convert_pointer_type(c_type)
        } else if c_type.contains("[") && c_type.contains("]") {
            self.convert_array_type(c_type)
        } else {
            // Use the type mapping from Parser
            // This would require a reference to Parser, so we'll leave as is for now
            c_type.to_string()
        }
    }

    fn convert_pointer_type(&self, c_type: &str) -> String {
        let is_const = c_type.contains("const");
        let base_type = c_type.replace("const", "").replace("*", "").trim().to_string();
        format!("*{} {}", if is_const { "const" } else { "mut" }, base_type)
    }

    fn convert_array_type(&self, c_type: &str) -> String {
        let parts: Vec<&str> = c_type.split('[').collect();
        let base_type = parts[0].trim();
        let size_part = parts[1];
        let size = size_part.trim_end_matches(']').trim();
        format!("[{}; {}]", base_type, size)
    }

    fn parse_simple_format(&mut self, s: &str) {
        let parts: Vec<&str> = s.trim().split(':').collect();
        if !parts.is_empty() {
            self.name = parts[0].trim().to_string();
            if parts.len() > 1 {
                self.fields = parts[1].trim().split(',').map(|s| s.trim().to_string()).collect();
            }
        }
    }

    pub fn len(&mut self) -> usize {
        self.len = self.output().len();
        self.len
    }
}
