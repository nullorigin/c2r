use crate::types::Type;

/// Represents a C macro definition
#[derive(Debug, Clone)]
pub struct Macro {
    /// Macro name
    pub name: String,
    /// Macro parameters
    pub params: Vec<String>,
    /// Macro body
    pub body: String,
    /// Original macro definition in C
    pub content: String,
}

impl Macro {
    /// Creates a new macro
    pub fn new() -> Macro {
        Macro {
            name: String::new(),
            params: Vec::new(),
            body: String::new(),
            content: String::new(),
        }
    }

    /// Parses a macro from C code
    pub fn parse(&mut self, content: &str) {
        self.content = content.to_string();
        
        // Check if it's a macro definition
        if content.starts_with("#define") {
            let line = content.lines().next().unwrap_or("");
            let parts: Vec<&str> = line.split_whitespace().collect();
            
            if parts.len() >= 2 {
                // Extract macro name
                if parts[1].contains('(') {
                    // This is a function-like macro
                    let name_and_params = parts[1].split('(').collect::<Vec<&str>>();
                    if name_and_params.len() >= 1 {
                        self.name = name_and_params[0].to_string();
                        
                        // Extract parameters
                        if name_and_params.len() >= 2 {
                            let params_str = name_and_params[1].trim_end_matches(')');
                            self.params = params_str
                                .split(',')
                                .map(|s| s.trim().to_string())
                                .collect();
                        }
                    }
                } else {
                    // This is an object-like macro
                    self.name = parts[1].to_string();
                }
                
                // Extract macro body
                if parts.len() >= 3 {
                    self.body = parts[2..].join(" ");
                }
            }
        }
    }

    /// Converts the macro to Rust code
    pub fn output(&self) -> String {
        if self.name.is_empty() {
            return String::new();
        }

        // Escape any Rust keywords in the macro name
        let safe_name = Type::escape(&self.name);
        
        let mut output = String::new();
        
        if self.params.is_empty() {
            // Object-like macro - convert to a constant
            output.push_str(&format!("// C macro: #define {} {}\n", self.name, self.body));
            output.push_str(&format!("pub const {}: &str = \"{}\";\n\n", safe_name, self.body));
        } else {
            // Function-like macro - convert to a function or just add as a comment
            output.push_str(&format!("// C macro: #define {}({}) {}\n", 
                self.name, 
                self.params.join(", "), 
                self.body));
            
            // For complex macros, we'll just add a comment and let the user handle it manually
            output.push_str("// TODO: Function-like macro needs manual conversion to Rust\n\n");
        }
        
        output
    }
}
