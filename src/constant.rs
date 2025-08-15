/// Represents a C constant or define
#[derive(Debug, Clone)]
pub struct Constant {
    /// Constant name
    pub name: String,
    /// Constant value
    pub value: String,
    /// Original constant definition in C
    pub content: String,
}

impl Constant {
    /// Creates a new constant
    pub fn new(name: &str, value: &str, content: &str) -> Constant {
        Constant {
            name: name.to_string(),
            value: value.to_string(),
            content: content.to_string(),
        }
    }

    /// Converts the constant to Rust code
    pub fn output(&self) -> String {
        if self.name.is_empty() {
            return String::new();
        }

        // Escape any Rust keywords in the constant name
        let safe_name = self.escape_rust_keyword(&self.name);

        // Generate Rust constant
        format!("pub const {}: i32 = {};\n", safe_name, self.value)
    }

    /// Escapes Rust keywords in identifiers by appending an underscore
    fn escape_rust_keyword(&self, name: &str) -> String {
        match name {
            "as" | "break" | "const" | "continue" | "crate" | "else" | "enum" |
            "extern" | "false" | "fn" | "for" | "if" | "impl" | "in" | "let" |
            "loop" | "match" | "mod" | "move" | "mut" | "pub" | "ref" |
            "return" | "self" | "Self" | "static" | "struct" | "super" |
            "trait" | "true" | "type" | "unsafe" | "use" | "where" | "while" |
            "abstract" | "become" | "box" | "do" | "final" | "macro" |
            "override" | "priv" | "typeof" | "unsized" | "virtual" | "yield" |
            "try" | "async" | "await" | "union" => format!("{}_", name),
            _ => name.to_string(),
        }
    }

    /// Parses a constant from C code
    pub fn parse(&mut self, content: &str) {
        self.content = content.to_string();
        
        // Extract #define NAME VALUE
        if content.starts_with("#define") {
            let parts: Vec<&str> = content.split_whitespace().collect();
            if parts.len() >= 3 {
                self.name = parts[1].to_string();
                self.value = parts[2..].join(" ");
            }
        }
    }
}
