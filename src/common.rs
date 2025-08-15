use std::fs::File;
use std::io::Read;
use crate::ConversionError;

pub fn find_matching_bracket(s: &str, open: char, close: char) -> Option<usize> {
    let mut depth = 0;
    let mut in_string = false;
    let mut in_char = false;
    let mut prev_char = '\0';

    for (i, c) in s.char_indices() {
        // Skip characters in string or character literals
        match c {
            '"' if prev_char != '\\' => in_string = !in_string,
            '\'' if prev_char != '\\' => in_char = !in_char,
            _ if in_string || in_char => {},
            _ => {
                if c == open {
                    depth += 1;
                } else if c == close {
                    depth -= 1;
                    if depth == 0 {
                        return Some(i + 1); // Return position after the closing bracket
                    }
                }
            }
        }

        prev_char = c;
    }

    None // No matching bracket found
}

/// Find the position of the matching closing brace
///
/// This helper function finds the position of a matching closing brace '}'
/// starting from the position of an opening brace '{' in the string.
/// Returns None if no matching brace is found.
///
/// # Arguments
/// * `s` - The string to search in
/// * `start_pos` - The position where the opening brace was found
///
/// # Returns
/// * `Option<usize>` - The position of the matching brace or None
pub fn find_matching_brace(s: &str, start_pos: usize) -> Option<usize> {
    let mut depth = 0;
    let mut in_string = false;
    let mut in_char = false;
    let mut in_comment = false;
    let mut in_multiline_comment = false;
    let mut prev_char = '\0';

    let chars: Vec<char> = s.chars().collect();

    // Start from the opening brace position
    let mut i = start_pos;

    while i < chars.len() {
        let c = chars[i];

        // Handle comments and string literals
        if i + 1 < chars.len() {
            let next_char = chars[i + 1];

            // Start of a single line comment
            if c == '/' && next_char == '/' && !in_string && !in_char && !in_comment && !in_multiline_comment {
                in_comment = true;
            }

            // Start of a multi-line comment
            if c == '/' && next_char == '*' && !in_string && !in_char && !in_comment && !in_multiline_comment {
                in_multiline_comment = true;
                i += 1; // Skip next char
            }

            // End of a multi-line comment
            if c == '*' && next_char == '/' && in_multiline_comment {
                in_multiline_comment = false;
                i += 1; // Skip next char
            }
        }

        // End of a single line comment
        if c == '\n' && in_comment {
            in_comment = false;
        }

        // Skip comment content
        if in_comment || in_multiline_comment {
            i += 1;
            continue;
        }

        // Handle string and character literals
        match c {
            '"' if prev_char != '\\' => in_string = !in_string,
            '\'' if prev_char != '\\' => in_char = !in_char,
            '{' if !in_string && !in_char => depth += 1,
            '}' if !in_string && !in_char => {
                depth -= 1;
                if depth == 0 {
                    return Some(i); // Return position of the closing brace
                }
            },
            _ => {}
        }

        prev_char = c;
        i += 1;
    }

    None // No matching brace found
}

pub fn find_matching_paren(s: &str, start_pos: usize) -> Option<usize> {
    let mut depth = 1;
    for (i, c) in s.chars().enumerate().skip(start_pos + 1) {
        match c {
            '(' => depth += 1,
            ')' => {
                depth -= 1;
                if depth == 0 {
                    return Some(i);
                }
            }
            _ => {}
        }
    }
    None
}

/// Checks if the given character is a C identifier character
///
/// # Arguments
/// * `c` - The character to check
///
/// # Returns
/// * `bool` - True if the character is a valid C identifier character
pub fn is_identifier_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

/// Extracts a C identifier starting at the given position
///
/// # Arguments
/// * `s` - The string to extract from
/// * `start` - The starting position
///
/// # Returns
/// * `String` - The extracted identifier
pub fn extract_identifier(s: &str, start: usize) -> String {
    if start >= s.len() {
        return String::new();
    }

    let mut end = start;
    for (i, c) in s[start..].char_indices() {
        if !is_identifier_char(c) {
            break;
        }
        end = start + i + 1;
    }

    s[start..end].to_string()
}
/// Preprocess content to remove comments and normalize whitespace
pub fn preprocess_content(content: &str) -> String {
    let mut result = String::new();
    let mut i = 0;
    let chars: Vec<char> = content.chars().collect();
    let mut in_string = false;
    
    while i < chars.len() {
        // Handle preprocessor directives (#include, #define, etc.)
        if chars[i] == '#' && !in_string {
            // Find the end of the line to skip the entire directive
            let start_pos = i;
            while i < chars.len() && chars[i] != '\n' {
                i += 1;
            }
            
            // Replace the directive with a space to maintain position integrity
            result.push_str(&" ".repeat(i - start_pos));
            
            if i < chars.len() {
                result.push('\n');
                i += 1; // Skip past newline
            }
            continue;
        }
        
        // Handle string literals - make sure we track when we're inside a string
        if chars[i] == '"' && (i == 0 || chars[i-1] != '\\') {
            if !in_string {
                // Start of a string literal
                in_string = true;
            } else {
                // End of string literal
                
                // Replace the string literal with a placeholder to avoid parsing issues
                // Only add opening and closing quotes with a placeholder in between
                result.push('"');
                result.push_str("string_literal_placeholder");
                result.push('"');
                
                i += 1; // Skip past the closing quote
                in_string = false;
                continue;
            }
        }
        
        if in_string {
            // Skip characters while we're inside a string without adding them
            i += 1;
            continue;
        }
        
        // Handle single-line comments
        if !in_string && i + 1 < chars.len() && chars[i] == '/' && chars[i + 1] == '/' {
            while i < chars.len() && chars[i] != '\n' {
                i += 1;
            }
            if i < chars.len() {
                result.push('\n');
                i += 1; // Skip past newline
            }
            continue;
        }
        
        // Handle multi-line comments
        if !in_string && i + 1 < chars.len() && chars[i] == '/' && chars[i + 1] == '*' {
            i += 2; // Skip past /*
            while i + 1 < chars.len() && !(chars[i] == '*' && chars[i + 1] == '/') {
                i += 1;
            }
            if i + 1 < chars.len() {
                i += 2; // Skip past */
            }
            continue;
        }
        
        // Add normal character
        result.push(chars[i]);
        i += 1;
    }
    
    result
}

/// Check if current position could be the start of a function
pub fn is_function_start(content: &str, pos: usize) -> bool {
    // Skip to the next non-whitespace character
    let mut i = pos;
    let chars: Vec<char> = content.chars().collect();
    
    // Skip whitespace
    while i < chars.len() && chars[i].is_whitespace() {
        i += 1;
    }
    
    // Check if this could be a function declaration
    // (this is a simplified check, might need improvement)
    let mut is_valid_start = false;
    
    if i < chars.len() {
        // Could be a function if it starts with a letter or underscore
        is_valid_start = chars[i].is_alphabetic() || chars[i] == '_';
    }
    
    is_valid_start
}

/// Extract function name from declaration
pub fn extract_function_name(declaration: &str) -> String {
    // Simple implementation to extract function name
    // For simplicity, assume function name is the word before the first '('
    let parts: Vec<&str> = declaration.split('(').collect();
    if parts.is_empty() {
        return String::new();
    }
    
    let first_part = parts[0];
    let words: Vec<&str> = first_part.split_whitespace().collect();
    
    if words.is_empty() {
        return String::new();
    }
    
    words.last().unwrap_or(&"").to_string()
}

/// Extract function return type from declaration
pub fn extract_function_return_type(declaration: &str) -> String {
    // Simple implementation for extracting return type
    // Assume return type is everything before the function name
    let parts: Vec<&str> = declaration.split('(').collect();
    if parts.is_empty() {
        return String::new();
    }
    
    let first_part = parts[0];
    let words: Vec<&str> = first_part.split_whitespace().collect();
    
    if words.len() <= 1 {
        return "void".to_string(); // Default to void if no return type specified
    }
    
    // Return type is everything except the last word (function name)
    words[..words.len() - 1].join(" ")
}

/// Extract function arguments from declaration
pub fn extract_function_args(declaration: &str) -> Vec<String> {
    let mut args = Vec::new();
    
    // Extract content between first '(' and last ')'
    let start = declaration.find('(');
    let end = declaration.rfind(')');
    
    if let (Some(start_pos), Some(end_pos)) = (start, end) {
        if start_pos < end_pos {
            let args_str = &declaration[start_pos + 1..end_pos];
            
            // Split by commas, but handle the case of empty arguments
            if !args_str.trim().is_empty() {
                for arg in args_str.split(',') {
                    args.push(arg.trim().to_string());
                }
            }
        }
    }
    
    args
}

/// Read file to string
pub fn read_file_to_string(path: &str) -> Result<String, ConversionError> {
    let mut buffer = String::new();
    let mut file = File::open(path)?;
    file.read_to_string(&mut buffer)?;
    Ok(buffer)
}

/// List of Rust keywords to be escaped in identifiers
pub fn rust_keywords() -> Vec<&'static str> {
    vec![
        "as", "break", "const", "continue", "crate", "else", "enum", "extern",
        "false", "fn", "for", "if", "impl", "in", "let", "loop", "match", "mod",
        "move", "mut", "pub", "ref", "return", "self", "Self", "static", "struct",
        "super", "trait", "true", "type", "unsafe", "use", "where", "while",
        "async", "await", "dyn", "abstract", "become", "box", "do", "final", "macro",
        "override", "priv", "typeof", "unsized", "virtual", "yield", "try"
    ]
}

/// Check if a name is a Rust keyword
pub fn is_rust_keyword(name: &str) -> bool {
    rust_keywords().contains(&name)
}

/// Escape a Rust keyword by appending an underscore
pub fn escape_rust_keyword(name: &str) -> String {
    if is_rust_keyword(name) {
        format!("{}_", name)
    } else {
        name.to_string()
    }
}

pub fn expr_map() -> Vec<(&'static str, &'static str)> {
    vec![
        // Basic spacing for readability
        ("(", " ( "), 
        (")", " ) "), 
        ("{", " { "), 
        ("}", " } "), 
        ("[", " [ "), 
        ("]", " ] "),
        
        // Linux kernel specific
        ("EXPORT_SYMBOL", ""),
        
        // Basic pointer syntax
        ("x->", "(*x)."),
        ("&x->", "&(*x)."),
        
        // Variable declaration
        ("ty x = y", "let x: ty = y"),
        
        // Increment/decrement
        ("x++", "x += 1"),
        ("x--", "x -= 1"),
        ("++x", "x += 1"),
        ("--x", "x -= 1"),
        
        // OO syntax
        ("this", "self"),
        
        // Null pointer
        ("NULL", "None"),
        ("nullptr", "None"),
        
        // Type casting
        ("(ty*)x", "x as *mut ty"),
        ("(const ty*)x", "x as *const ty"),
        
        // Control flow
        ("switch", "match"),
        ("case: x { y }", "x => { y }"),
        ("case default: x", "_ => x"),
        ("default: x", "_ => x"),
        ("if (x) { y }", "if x { y }"),
        ("if (x) { y } else { z }", "if x { y } else { z }"),
        ("if (x) y = z", "if x { y = z }"),
        ("if (x) y = z; else w = v", "if x { y = z } else { w = v }"),
        
        // Loops
        ("while true { y }", "loop { y }"),
        ("while true y = z", "loop { y = z }"),
        ("while (x) { y }", "while x { y }"),
        ("while (x) y = z", "while x { y = z }"),
        ("do { y } while (x)", "while x { y }"),
        ("for (i = 0; i < n; i++) { y }", "for i in 0..n { y }"),
        ("for (i = 0; i < n; i++) y = z", "for i in 0..n { y = z }"),
        ("for (i = n - 1; i >= 0; i--) { y }", "for i in (0..n).rev() { y }"),
        ("for (i = n - 1; i >= 0; i--) y = z", "for i in (0..n).rev() { y = z }"),
        
        // Memory management
        ("malloc(x)", "Box::new([0u8; x])"),
        ("calloc(n, size)", "Box::new([0u8; n * size])"),
        ("free(x)", "drop(x)"),
        ("realloc(ptr, size)", "/* realloc not directly supported in Rust */"),
        
        // Standard library functions
        ("memcpy(dest, src, n)", "ptr::copy_nonoverlapping(src, dest, n)"),
        ("memmove(dest, src, n)", "ptr::copy(src, dest, n)"),
        ("memset(s, c, n)", "ptr::write_bytes(s, c, n)"),
        ("printf", "println!"),
        ("sprintf", "format!"),
        ("fprintf", "write!"),
        
        // Ternary operator
        ("x ? y : z", "if x { y } else { z }"),
        
        // Error handling
        ("errno", "std::io::Error::last_os_error()"),
        ("perror(x)", "eprintln!(\"{}: {}\", x, std::io::Error::last_os_error())"),
        
        // Variable argument macros
        ("va_start", "/* va_start not directly supported in Rust */"),
        ("va_end", "/* va_end not directly supported in Rust */"),
        ("va_arg", "/* va_arg not directly supported in Rust */"),
        
        // Compound assignments
        ("+= ", "+= "),
        ("-= ", "-= "),
        ("*= ", "*= "),
        ("/= ", "/= "),
        ("%= ", "%= "),
        ("&= ", "&= "),
        ("|= ", "|= "),
        ("^= ", "^= "),
        ("<<= ", "<<= "),
        (">>= ", ">>= "),
        
        // Preprocessor directives (basic handling)
        ("#define", "// #define"),
        ("#include", "// #include"),
        ("#ifdef", "// #ifdef"),
        ("#ifndef", "// #ifndef"),
        ("#endif", "// #endif"),
        ("#pragma", "// #pragma"),
        
        // Function pointer type
        ("ty (*name)(args)", "type name = fn(args) -> ty"),
        
        // Array/slice operations
        ("arr[i]", "arr[i]"),
        
        // Struct initialization
        (".field = val", "field: val"),
        ("{.field = val}", "{ field: val }"),
        
        // Common C struct member access idioms
        ("->next", ".next"),
        ("->prev", ".prev"),
        ("->data", ".data"),
        ("->size", ".size"),
        ("->length", ".length"),
        ("->value", ".value"),
        ("->type", ".type_"),
        ("->name", ".name"),
        ("->id", ".id"),
    ]
}

/// Returns the standard C to Rust type mappings
pub fn type_map() -> Vec<(&'static str, &'static str)> {
    vec![
        // Basic types
        ("char", "i8"),
        ("short", "i16"),
        ("int", "i32"),
        ("long", "i64"),
        ("long long", "i64"),
        ("unsigned char", "u8"),
        ("unsigned short", "u16"),
        ("unsigned int", "u32"),
        ("unsigned long", "u64"),
        ("unsigned long long", "u64"),
        ("bool", "bool"),
        ("float", "f32"),
        ("double", "f64"),
        ("void", "()"),
        ("size_t", "usize"),
        ("ssize_t", "isize"),
        ("FILE", "File"),
        
        // Integer types with explicit size
        ("int8_t", "i8"),
        ("int16_t", "i16"),
        ("int32_t", "i32"),
        ("int64_t", "i64"),
        ("uint8_t", "u8"),
        ("uint16_t", "u16"),
        ("uint32_t", "u32"),
        ("uint64_t", "u64"),
        
        // Pointer types
        ("void *", "*mut c_void"),
        ("char *", "*mut i8"),
        ("const char *", "*const i8"),
        
        // Other common C types
        ("NULL", "null"),
        ("nullptr", "None"),
    ]
}

/// Convert C type to Rust type
pub fn convert_type(c_type: &str) -> String {
    let c_type = c_type.trim();
    
    // Check direct mappings first
    for (c, rust) in type_map() {
        if c_type == c {
            return rust.to_string();
        }
    }
    
    // Handle pointers
    if c_type.contains('*') {
        return convert_pointer_type(c_type);
    }
    
    // If no direct mapping, return as is
    c_type.to_string()
}

/// Convert a C pointer type to Rust pointer type
pub fn convert_pointer_type(c_type: &str) -> String {
    let c_type = c_type.trim();
    
    // Handle const pointers
    if c_type.contains("const") && c_type.contains('*') {
        let base_type = c_type.replace("const", "").replace("*", "").trim().to_string();
        let rust_base_type = convert_type(&base_type);
        return format!("*const {}", rust_base_type);
    }
    
    // Handle non-const pointers
    if c_type.contains('*') {
        let base_type = c_type.replace("*", "").trim().to_string();
        let rust_base_type = convert_type(&base_type);
        return format!("*mut {}", rust_base_type);
    }
    
    // Fallback
    c_type.to_string()
}

/// Convert C arrow operator (obj->field) to Rust syntax ((*obj).field)
pub fn convert_arrow_operator(body: &str) -> String {
    // Arrow operator is C's way to access fields of a pointer to struct
    // In Rust, we'd use (*ptr).field syntax
    let mut result = String::new();
    let mut pos = 0;
    let chars: Vec<char> = body.chars().collect();
    
    while pos < chars.len() - 1 {
        if pos + 1 < chars.len() && chars[pos] == '-' && chars[pos + 1] == '>' {
            // Found an arrow operator, find what's before it (the pointer variable)
            let mut var_start = pos;
            
            while var_start > 0 && (
                chars[var_start - 1].is_alphanumeric() || 
                chars[var_start - 1] == '_' || 
                chars[var_start - 1] == ')')
            {
                var_start -= 1;
            }
            
            // Extract the variable name
            let var_name = &body[var_start..pos];
            
            // Only process if we have a valid variable name
            if !var_name.is_empty() {
                // Check if the variable already contains a dereference to avoid duplication
                if var_name.contains("(*") && var_name.contains(").") {
                    // This already has dereference syntax, don't wrap it again
                    // Just replace -> with .
                    result.push_str(&body[pos - var_name.len()..var_start]);
                    result.push_str(var_name);
                    result.push('.');
                } else if var_name.contains("nlh(*") {
                    // Special handling for nested pointer dereferences that are causing problems
                    result.push_str(&body[pos - var_name.len()..var_start]);
                    result.push_str(var_name.trim_start_matches("nlh(*"));
                    result.push('.');
                } else {
                    // Replace -> with (*var).
                    result.push_str(&body[pos - var_name.len()..var_start]);
                    result.push_str("(*");
                    result.push_str(var_name);
                    result.push_str(").");
                }
                
                // Skip over the arrow operator
                pos += 2;
            } else {
                // If we couldn't find a valid variable, just add the current char
                result.push(chars[pos]);
                pos += 1;
            }
        } else {
            // No arrow operator, just add the current char
            result.push(chars[pos]);
            pos += 1;
        }
    }
    
    // Add the last character if we haven't processed it
    if pos < chars.len() {
        result.push(chars[pos]);
    }
    
    result
}

/// Convert C style ternary operators to Rust if-else expressions
pub fn convert_ternary_operator(body: &str) -> String {
    let mut result = String::new();
    let mut pos = 0;
    let chars: Vec<char> = body.chars().collect();
    
    // Look for ? character that might indicate a ternary operator
    while pos < chars.len() {
        let c = chars[pos];
        
        if c == '?' {
            // Potential ternary operator found
            // First, find the condition (before the ?)
            let mut condition_start = pos;
            let mut paren_level = 0;
            
            // Go backwards to find the start of the condition
            while condition_start > 0 {
                condition_start -= 1;
                if chars[condition_start] == ')' {
                    paren_level += 1;
                } else if chars[condition_start] == '(' {
                    paren_level -= 1;
                    if paren_level < 0 {
                        // Found the outermost opening parenthesis
                        condition_start += 1;
                        break;
                    }
                } else if paren_level == 0 && 
                          (chars[condition_start] == ';' || 
                           chars[condition_start] == ',' || 
                           chars[condition_start] == '=' || 
                           chars[condition_start] == '{') {
                    // Reached the beginning of the statement
                    condition_start += 1;
                    break;
                }
            }
            
            // Extract the condition
            let condition = &body[condition_start..pos].trim();
            
            // Now find the "true" part (between ? and :)
            let mut true_part_end = pos + 1;
            let mut nested_level = 0;
            
            while true_part_end < chars.len() {
                if chars[true_part_end] == '(' || chars[true_part_end] == '[' || chars[true_part_end] == '{' {
                    nested_level += 1;
                } else if chars[true_part_end] == ')' || chars[true_part_end] == ']' || chars[true_part_end] == '}' {
                    nested_level -= 1;
                } else if chars[true_part_end] == ':' && nested_level == 0 {
                    // Found the colon separating the "true" and "false" parts
                    break;
                }
                true_part_end += 1;
            }
            
            if true_part_end >= chars.len() {
                // No matching colon found, not a valid ternary
                result.push(c);
                pos += 1;
                continue;
            }
            
            // Extract the "true" part
            let true_part = &body[(pos + 1)..true_part_end].trim();
            
            // Now find the "false" part (after the :)
            let mut false_part_end = true_part_end + 1;
            nested_level = 0;
            
            while false_part_end < chars.len() {
                if chars[false_part_end] == '(' || chars[false_part_end] == '[' || chars[false_part_end] == '{' {
                    nested_level += 1;
                } else if chars[false_part_end] == ')' || chars[false_part_end] == ']' || chars[false_part_end] == '}' {
                    nested_level -= 1;
                    if nested_level < 0 {
                        break;
                    }
                } else if nested_level == 0 && 
                         (chars[false_part_end] == ';' || 
                          chars[false_part_end] == ',' || 
                          chars[false_part_end] == ')') {
                    // End of the "false" part
                    break;
                }
                false_part_end += 1;
            }
            
            // Extract the "false" part
            let false_part = &body[(true_part_end + 1)..false_part_end].trim();
            
            // Build the Rust if-else expression
            let rust_if_else = format!("if {} {{ {} }} else {{ {} }}", condition, true_part, false_part);
            
            // Add everything before the condition to the result - with bounds checking
            if condition_start > 0 && pos >= condition.len() {
                // Safe to subtract and slice
                result.push_str(&body[..condition_start]);
            } else {
                // Just add what we have so far without trying to add more context
                // This might result in less ideal output but won't panic
            }
            
            // Add the new if-else expression
            result.push_str(&rust_if_else);
            
            // Move past the processed ternary
            pos = false_part_end;
        } else {
            // Not a ternary operator, add current character and move on
            result.push(c);
            pos += 1;
        }
    }
    
    result
}
#[macro_export]
macro_rules! impl_enum {
    (
        #[derive($($derives:tt),*)]
        #[repr($ty:ty)]
        $vis:vis enum $name:ident {
            $(
                $variant:ident = $val:expr$(,)?
            )*
        }
    ) => {
        #[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
        #[repr($ty)]
        $vis enum $name {
            $(
                $variant = $val,
            )*
            __MAX
        }

        impl From<$ty> for $name {
            fn from(val: u32) -> Self {
                match val {
                $(
                    $val => $name::$variant,
                )*
                    _ => $name::__MAX
                }
            }
        }

        impl From<$name> for $ty {
            fn from(val: $name) -> Self {
                val as $ty
            }
        }

        impl core::ops::BitAnd for $name {
            type Output = Self;
            fn bitand(self, other: Self) -> Self {
                Self::from((self as $ty) & (other as $ty))
            }
        }
        impl core::ops::BitAnd<$ty> for $name {
            type Output = Self;
            fn bitand(self, other: $ty) -> Self {
                Self::from((self as $ty) & other)
            }
        }
        impl core::ops::BitOr for $name {
            type Output = Self;
            fn bitor(self, other: Self) -> Self {
                Self::from((self as $ty) | (other as $ty))
            }
        }
        impl core::ops::BitOr<$ty> for $name {
            type Output = Self;
            fn bitor(self, other: $ty) -> Self {
                Self::from((self as $ty) | other)
            }
        }
        impl core::ops::BitXor for $name {
            type Output = Self;
            fn bitxor(self, other: Self) -> Self {
                Self::from((self as $ty) ^ (other as $ty))
            }
        }
        impl core::ops::BitXor<$ty> for $name {
            type Output = Self;
            fn bitxor(self, other: $ty) -> Self {
                Self::from((self as $ty) ^ other)
            }
        }
        impl core::ops::BitAndAssign for $name {
            fn bitand_assign(&mut self, other: Self) {
                *self = *self & other;
            }
        }
        impl core::ops::BitOrAssign for $name {
            fn bitor_assign(&mut self, other: Self) {
                *self = *self | other;
            }
        }
        impl core::ops::BitXorAssign for $name {
            fn bitxor_assign(&mut self, other: Self) {
                *self = *self ^ other;
            }
        }
        impl core::ops::Not for $name {
            type Output = Self;
            fn not(self) -> Self {
                Self::from(!(self as $ty))
            }
        }
        impl core::ops::Add for $name {
            type Output = Self;
            fn add(self, other: Self) -> Self {
                Self::from((self as $ty) + (other as $ty))
            }
        }

        impl core::ops::Sub for $name {
            type Output = Self;
            fn sub(self, other: Self) -> Self {
                Self::from((self as $ty) - (other as $ty))
            }
        }
        impl core::ops::Mul for $name {
            type Output = Self;
            fn mul(self, other: Self) -> Self {
                Self::from((self as $ty) * (other as $ty))
            }
        }
        impl core::ops::Div for $name {
            type Output = Self;
            fn div(self, other: Self) -> Self {
                Self::from((self as $ty) / (other as $ty))
            }
        }
        impl core::ops::Rem for $name {
            type Output = Self;
            fn rem(self, other: Self) -> Self {
                Self::from((self as $ty) % (other as $ty))
            }
        }
        impl core::ops::Shl for $name {
            type Output = Self;
            fn shl(self, other: Self) -> Self {
                Self::from((self as $ty) << (other as $ty))
            }
        }
        impl core::ops::Shr for $name {
            type Output = Self;
            fn shr(self, other: Self) -> Self {
                Self::from((self as $ty) >> (other as $ty))
            }
        }
        impl core::ops::ShlAssign for $name {
            fn shl_assign(&mut self, other: Self) {
                *self = *self << other;
            }
        }
        impl core::ops::ShrAssign for $name {
            fn shr_assign(&mut self, other: Self) {
                *self = *self >> other;
            }
        }
        impl core::ops::AddAssign for $name {
            fn add_assign(&mut self, other: Self) {
                *self = *self + other;
            }
        }
        impl core::ops::SubAssign for $name {
            fn sub_assign(&mut self, other: Self) {
                *self = *self - other;
            }
        }
        impl core::ops::MulAssign for $name {
            fn mul_assign(&mut self, other: Self) {
                *self = *self * other;
            }
        }
        impl core::ops::DivAssign for $name {
            fn div_assign(&mut self, other: Self) {
                *self = *self / other;
            }
        }
        impl core::ops::RemAssign for $name {
            fn rem_assign(&mut self, other: Self) {
                *self = *self % other;
            }
        }
    }
}