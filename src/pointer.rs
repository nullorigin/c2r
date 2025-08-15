use std::cell::LazyCell;
use std::collections::HashMap;
use std::str::FromStr;
use crate::error::ConversionError;
pub const PTR_TYPE: [&str; 8] = ["*const", "*mut", "*const mut", "*mut mut", "*const", "const *mut", "const *const", "mut *mut"];
pub const REF_TYPE: [&str; 6] = ["&", "&mut", "&mut mut", "&const", "&mut const", "&const mut"];
pub const PTR_REF_TYPE: [&str; 14] = ["&mut *mut", "&mut *const", "&const *mut", "&const *const", "*const &mut", "*const &const", "*mut &mut", "*mut &const", "*const &mut", "*const &const", "*mut &mut", "*mut &const","&*mut", "&*const"];
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum Type {
    None = 0,
    Ptr = 1,
    Ref = 2,
    Const = 4,
    Mut = 8,
    Deref = 16
}
impl Type {
    pub const MAP: LazyCell<HashMap<&'static str, (Type, Type, Type, Type)>> = LazyCell::new(||HashMap::from([
            ("", (Type::None, Type::None, Type::None, Type::None)),
            ("&", (Type::None, Type::None, Type::None, Type::Ref)),
            ("&mut", (Type::None, Type::None, Type::Ref, Type::Mut)),
            ("&mut mut", (Type::None, Type::Ref, Type::Mut, Type::Mut)),
            ("&const", (Type::None, Type::None, Type::Ref, Type::Const)),
            ("&mut const", (Type::None, Type::Ref, Type::Mut, Type::Const)),
            ("&const mut", (Type::None, Type::Ref, Type::Const, Type::Mut)),
            ("*", (Type::None, Type::None, Type::None, Type::Ptr)),
            ("*const", (Type::None, Type::None, Type::Ptr, Type::Const)),
            ("*mut", (Type::None, Type::None, Type::Ptr, Type::Mut)),
            ("*const mut", (Type::None, Type::Ptr, Type::Const, Type::Mut)),
            ("*mut mut", (Type::None, Type::Ptr, Type::Mut, Type::Mut)),
            ("const *mut", (Type::None, Type::Const, Type::Ptr, Type::Mut)),
            ("const *const", (Type::None, Type::Const, Type::Ptr, Type::Const)),
            ("mut *mut", (Type::None, Type::Mut, Type::Ptr, Type::Mut)),
            ("mut *const", (Type::None, Type::Mut, Type::Ptr, Type::Const)),
            ("*const &mut", (Type::Ptr, Type::Const, Type::Ref, Type::Mut)),
            ("*const &const", (Type::Ptr, Type::Const, Type::Ref, Type::Const)),
            ("*mut &mut", (Type::Ptr, Type::Mut, Type::Ref, Type::Mut)),
            ("*mut &const", (Type::Ptr, Type::Mut, Type::Ref, Type::Const)),
            ("&*mut", (Type::None, Type::Ref, Type::Ptr, Type::Mut)),
            ("&*const", (Type::None, Type::Ref, Type::Ptr, Type::Const)),
            ("&mut *", (Type::None, Type::Ref, Type::Mut, Type::Ptr)),
            ("&const *", (Type::None, Type::Ref, Type::Const, Type::Ptr)),
            ("&mut *const", (Type::Ref, Type::Mut, Type::Ptr, Type::Const)),
            ("&const *mut", (Type::Ref, Type::Const, Type::Ptr, Type::Mut)),
            ("&const *const", (Type::Ref, Type::Const, Type::Ptr, Type::Const)),
            ("&mut *mut", (Type::Ref, Type::Mut, Type::Ptr, Type::Mut)),
        ]));
    pub fn convert_mao(a: Type, b: Type, c: Type, d: Type) -> &'static str {
        HashMap::from([
            ((Type::None, Type::None, Type::None, Type::None), ""),
            ((Type::None, Type::None, Type::None, Type::Ref), "&"),
            ((Type::None, Type::None, Type::Ref, Type::Mut), "&mut"),
            ((Type::None, Type::Ref, Type::Mut, Type::Mut), "&mut mut"),
            ((Type::None, Type::Ref, Type::Const, Type::Mut), "&const mut"),
            ((Type::None, Type::Ref, Type::Mut, Type::Const), "&mut const"),
            ((Type::None, Type::None, Type::None, Type::Ptr), "*"),
            ((Type::None, Type::None, Type::Ptr, Type::Const), "*const"),
            ((Type::None, Type::None, Type::Ptr, Type::Mut), "*mut"),
            ((Type::None, Type::Ptr, Type::Const, Type::Mut), "*const mut"),
            ((Type::None, Type::Ptr, Type::Mut, Type::Mut), "*mut mut"),
            ((Type::None, Type::Const, Type::Ptr, Type::Mut), "const *mut"),
            ((Type::None, Type::Const, Type::Ptr, Type::Const), "const *const"),
            ((Type::None, Type::Mut, Type::Ptr, Type::Mut), "mut *mut"),
            ((Type::None, Type::Mut, Type::Ptr, Type::Const), "mut *const"),
            ((Type::Ptr, Type::Const, Type::Ref, Type::Mut), "*const &mut"),
            ((Type::Ptr, Type::Const, Type::Ref, Type::Const), "*const &const"),
            ((Type::Ptr, Type::Mut, Type::Ref, Type::Mut), "*mut &mut"),
            ((Type::Ptr, Type::Mut, Type::Ref, Type::Const), "*mut &const"),
            ((Type::None, Type::Ref, Type::Ptr, Type::Mut), "&*mut"),
            ((Type::None, Type::Ref, Type::Ptr, Type::Const), "&*const"),
            ((Type::None, Type::Ref, Type::Mut, Type::Ptr), "&mut *"),
            ((Type::Ref, Type::Mut, Type::Ptr, Type::Mut), "&mut *mut"),
            ((Type::Ref, Type::Mut, Type::Ptr, Type::Const), "&mut *const"),
            ((Type::Ref, Type::Const, Type::Ptr, Type::Mut), "&const *mut"),
            ((Type::Ref, Type::Const, Type::Ptr, Type::Const), "&const *const"),
        ]).get(&(a, b, c, d)).unwrap_or(&"")
    }

    pub fn convert(c_type: &str) -> String {
        // Extract the base type (part before the *) and clean it up
        let base_type = c_type.trim().replace("const", "").replace("*", "").trim().to_string();
        
        // Handle special case of void*
        if c_type.contains("void") && c_type.contains('*') {
            return "Option<*mut ()>".to_string();
        }
        
        // Check if the pointer is const
        if c_type.contains("const") {
            return format!("*const {}", Type::convert(&base_type));
        } else {
            return format!("*mut {}", Type::convert(&base_type));
        }
    }
}
pub struct Pointer {
    pub type_: (Type, Type, Type, Type),
    pub name: String,
    pub value: String,
}
impl Pointer {
    pub fn new() -> Pointer {
        Pointer {
            type_: (Type::None, Type::None, Type::None, Type::None),
            name: String::new(),
            value: String::new(),
        }
    }

    pub fn convert(&self, body: &str) -> String {
        let mut result = String::new();
        let mut lines = body.lines();

        while let Some(line) = lines.next() {
            // Create a copy of the line to avoid borrowing issues
            let mut processed_line = line.to_string();

            // Convert C-style pointer casts like "(type*)var" to Rust "var as *mut type"
            processed_line = self.convert_casts(&processed_line);

            // Add the processed line to the result
            result.push_str(&processed_line);
            result.push('\n');
        }

        result
    }

     pub fn convert_casts(&self, body: &str) -> String {
        let mut result = String::new();
        let mut lines = body.lines();

        while let Some(line) = lines.next() {
            // Create a copy of the line to avoid borrowing issues
            let mut processed_line = line.to_string();

            // Fix malformed pointer assignments like "char *b = ) nlh;"
            if processed_line.contains(" *") && processed_line.contains(" = )") {
                processed_line = processed_line.replace(" = )", " = (");

                // Find the closing parenthesis or semicolon
                if let Some(pos) = processed_line.find(';') {
                    if pos > 0 {
                        processed_line.insert(pos, ')');
                    }
                }
            }

            // Fix malformed struct pointer assignments
            if processed_line.contains("struct") && processed_line.contains("*") && processed_line.contains(" = )") {
                processed_line = processed_line.replace("struct ", "");
                processed_line = processed_line.replace(" = )", " = (");

                // Find the closing parenthesis or semicolon
                if let Some(pos) = processed_line.find(';') {
                    if pos > 0 {
                        processed_line.insert(pos, ')');
                    }
                }
            }

            // Convert C-style pointer casts like "(type*)var" to Rust "var as *mut type"
            if processed_line.contains('(') && processed_line.contains(')') {
                // Create a new string for the processed output
                let mut output = String::new();
                let original_line = processed_line.clone();
                let chars: Vec<char> = original_line.chars().collect();
                let mut i = 0;

                while i < chars.len() {
                    if chars[i] == '(' && i + 1 < chars.len() {
                        // Check if this looks like a cast by finding the closing parenthesis
                        let mut j = i + 1;
                        let mut paren_level = 1;
                        let mut found_closing = false;
                        let cast_start = i;

                        while j < chars.len() && paren_level > 0 {
                            if chars[j] == '(' {
                                paren_level += 1;
                            } else if chars[j] == ')' {
                                paren_level -= 1;
                                if paren_level == 0 {
                                    found_closing = true;
                                    break;
                                }
                            }
                            j += 1;
                        }

                        if found_closing {
                            // Extract the cast expression
                            let cast_expr: String = chars[cast_start + 1..j].iter().collect();

                            // Check if this looks like a C cast
                            if cast_expr.contains('*') && !cast_expr.contains('(') && !cast_expr.contains(')') {
                                // Find what comes after the cast
                                if j + 1 < chars.len() {
                                    // Extract the variable being cast
                                    let mut var_end = j + 1;
                                    while var_end < chars.len() && (chars[var_end].is_alphanumeric() || chars[var_end] == '_') {
                                        var_end += 1;
                                    }

                                    if var_end > j + 1 {
                                        let var_name: String = chars[j + 1..var_end].iter().collect();

                                        if !var_name.is_empty() {
                                            // Check if it's a pointer cast
                                            if cast_expr.ends_with('*') {
                                                // Rust cast: "var as *mut type"
                                                let type_name = cast_expr.trim_end_matches('*').trim();
                                                let rust_type = Pointer::convert_type(type_name);
                                                let rust_cast = format!("{} as *mut {}", var_name, rust_type);

                                                // Add the Rust cast to the output
                                                output.push_str(&rust_cast);

                                                // Skip past the processed parts
                                                i = var_end;
                                                continue;
                                            } else if cast_expr.contains("const") && cast_expr.contains('*') {
                                                // Const pointer cast: "var as *const type"
                                                let cleaned_type = cast_expr.replace("const", "").replace("*", "").trim().to_string();
                                                let rust_type = Pointer::convert_type(&cleaned_type);
                                                let rust_cast = format!("{} as *const {}", var_name, rust_type);

                                                // Add the Rust cast to the output
                                                output.push_str(&rust_cast);

                                                // Skip past the processed parts
                                                i = var_end;
                                                continue;
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }

                    // If we didn't process a cast, just add the character
                    output.push(chars[i]);
                    i += 1;
                }

                // Use the processed output
                processed_line = output;
            }

            result.push_str(&processed_line);
            result.push('\n');
        }

        // Remove the trailing newline if the original didn't have one
        if !body.ends_with('\n') && result.ends_with('\n') {
            result.pop();
        }

        result
    }

    pub fn convert_dereferences(body: &str) -> String {
        let mut new_body = body.to_string();
        let mut last_pos = 0;
        while let Some(pos) = new_body[last_pos..].find("->") {
            let pos = last_pos + pos;
            let (var, field) = Self::get_arrow_op_parts(&new_body, pos);
            let new_text = format!("(*{}).{}", var, field);
            let old_text = format!("{}->{}", var, field);
            new_body = new_body.replace(&old_text, &new_text);
            last_pos = pos + 1;
        }
        new_body
    }
    
    fn get_arrow_op_parts(body: &str, pos: usize) -> (String, String) {
        let mut var_end = pos;
        while var_end > 0 && (body.as_bytes()[var_end - 1] as char).is_alphanumeric() || (body.as_bytes()[var_end - 1] as char) == '_' {
            var_end -= 1;
        }
        let var = body[var_end..pos].trim().to_string();

        let mut field_start = pos + 2;
        while field_start < body.len() && (body.as_bytes()[field_start] as char).is_whitespace() {
            field_start += 1;
        }

        let mut field_end = field_start;
        while field_end < body.len() && (body.as_bytes()[field_end] as char).is_alphanumeric() || (body.as_bytes()[field_end] as char) == '_' {
            field_end += 1;
        }
        let field = body[field_start..field_end].trim().to_string();

        (var, field)
    }

    /// Static method to convert a C pointer type to Rust
    pub fn convert_type(c_type: &str) -> String {
        // Extract the base type (part before the *) and clean it up
        let base_type = c_type.trim().replace("const", "").replace("*", "").trim().to_string();
        
        // Handle special case of void*
        if c_type.contains("void") && c_type.contains('*') {
            return "Option<*mut ()>".to_string();
        }
        
        // Check if the pointer is const
        if c_type.contains("const") {
            return format!("*const {}", Type::convert(&base_type));
        } else {
            return format!("*mut {}", Type::convert(&base_type));
        }
    }
}

impl Default for Pointer {
    fn default() -> Self {
        Pointer {
            type_: (Type::None, Type::None, Type::None, Type::None),
            name: String::new(),
            value: String::new(),
        }
    }
}
impl FromStr for Pointer {
    type Err = ConversionError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let pointer = Pointer::default();
        pointer.convert(s);
        Ok(pointer)
    }
}