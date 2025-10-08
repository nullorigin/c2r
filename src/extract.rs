#![allow(
    unused_variables,
    unused_assignments,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals
)]

use crate::{convert, ArrayInfo, CommentInfo, EnumInfo, ExpressionInfo, FunctionInfo, GlobalInfo, Id, IncludeInfo, MacroInfo, StructInfo, TypedefInfo};
use crate::{Token, tok};
use std::fmt;
use std::ops::Range;

/// Represents different types of extractable C elements
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExtractedElement {
    Function(ExtractedFunction),
    Struct(ExtractedStruct),
    Enum(ExtractedEnum),
    Typedef(ExtractedTypedef),
    Global(ExtractedGlobal),
    Macro(ExtractedMacro),
    Include(ExtractedInclude),
    Array(ExtractedArray),
    Comment(ExtractedComment),
    Expression(ExtractedExpression),
    ControlFlow(ExtractedControlFlow),
}

/// Represents an extracted C function
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExtractedFunction {
    pub id: Id,
    pub code: String,
    pub info: FunctionInfo,
    pub tokens: Vec<Token>,
    pub token_range: Range<usize>,
    pub metadata: Vec<(String, String)>,
}

impl fmt::Display for ExtractedFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fn {}(", self.info.name)?;

        // Format parameters
        let mut first = true;
        for param_str in self.info.parameters.clone() {
            if !param_str.is_empty() {
                if !first {
                    write!(f, ", ")?;
                }
                first = false;
                write!(f, "{}", param_str)?;
            }
        }

        if self.info.is_variadic {
            if !first {
                write!(f, ", ")?;
            }
            write!(f, "...")?;
        }

        // Format return type
        let return_type_str = &self.info.return_type;
        if !return_type_str.is_empty() && return_type_str != "void" {
            write!(f, ") -> {}", return_type_str)?;
        } else {
            write!(f, ")")?;
        }

        Ok(())
    }
}

/// Represents an extracted C struct
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExtractedStruct {
    pub id: Id,
    pub code: String,
    pub info: StructInfo,
    pub tokens: Vec<Token>,
    pub token_range: Range<usize>,
    pub metadata: Vec<(String, String)>,
}

impl fmt::Display for ExtractedStruct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "pub struct {} {{\n", self.info.name)?;
        for (field_name, field_type) in &self.info.fields {
            write!(f, "    pub {}: {},\n", field_name, field_type)?;
        }
        write!(f, "}}\n")
    }
}

/// Represents an extracted C enum
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExtractedEnum {
    pub id: Id,
    pub code: String,
    pub info: EnumInfo,
    pub tokens: Vec<Token>,
    pub token_range: std::ops::Range<usize>,
    pub metadata: Vec<(String, String)>,
}

impl fmt::Display for ExtractedEnum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "pub enum {} {{\n", self.info.name)?;
        for (variant, val) in &self.info.variants {
            if let Some(v) = val {
                write!(f, "    {} = {},\n", variant, v)?;
            } else {
                write!(f, "    {},\n", variant)?;
            }
        }
        write!(f, "}}\n")
    }
}

/// Represents an extracted C typedef
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExtractedTypedef {
    pub id: Id,
    pub code: String,
    pub info: TypedefInfo,
    pub tokens: Vec<Token>,
    pub token_range: std::ops::Range<usize>,
    pub metadata: Vec<(String, String)>,
}

impl fmt::Display for ExtractedTypedef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let orig_type = tokens_to_rust(&self.tokens);
        write!(f, "type {} = {};\n", self.info.name, orig_type)
    }
}

/// Represents an extracted global variable
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExtractedGlobal {
    pub id: Id,
    pub code: String,
    pub info: GlobalInfo,
    pub tokens: Vec<Token>,
    pub token_range: Range<usize>,
    pub metadata: Vec<(String, String)>,
}

impl fmt::Display for ExtractedGlobal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let type_str = &self.info.type_name;
        let mut result = String::new();

        if self.info.is_const {
            result.push_str("pub const ");
        } else if self.info.is_static {
            result.push_str("pub static ");
        } else {
            result.push_str("pub static mut ");
        }

        result.push_str(&self.info.name);
        result.push_str(": ");

        // Handle array dimensions
        if self.info.dimensions == 0 {
            result.push_str(&type_str);
        } else {
            // Format arrays as [type; size]
            for dim in 0..self.info.dimensions {
                result = format!("[{}; {}]", type_str, dim);
            }
        }

        write!(f, "{}", result)?;

        if let Some(init)    = &self.info.initializer {
            let init_tokens: Vec<String> = init.chars().map(|t| t.to_string()).collect();
            let init_str = init_tokens.join(" ");
            write!(f, " = {}", init_str)?;
        }

        write!(f, ";\n")
    }
}

/// Represents an extracted macro
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExtractedMacro {
    pub id: Id,
    pub code: String,
    pub info: MacroInfo,
    pub tokens: Vec<Token>,
    pub token_range: std::ops::Range<usize>,
    pub metadata: Vec<(String, String)>,
}

impl fmt::Display for ExtractedMacro {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "// macro {}", self.info.name)?;

        if !self.info.params.is_empty() {
            write!(f, "({})", self.info.params.join(", "))?;
        }

        if !self.info.body.is_empty() {
            let body_text = self
                .info.body
                .chars()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(" ");
            write!(f, " {}", body_text)?;
        }

        writeln!(f)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExtractedInclude {
    pub id: Id,
    pub code: String,
    pub info: IncludeInfo,
    pub tokens: Vec<Token>,
    pub token_range: Range<usize>,
    pub metadata: Vec<(String, String)>,
}

impl fmt::Display for ExtractedInclude {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "// include {}\n", self.info.path)
    }
}

/// Represents an extracted C array
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExtractedArray {
    pub id: Id,
    pub code: String,
    pub info: ArrayInfo,
    pub tokens: Vec<Token>,
    pub token_range: std::ops::Range<usize>,
    pub metadata: Vec<(String, String)>,
}

impl fmt::Display for ExtractedArray {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.info.is_declaration {
            write!(
                f,
                "let mut {}: [{}; {}] = [Default::default(); {}];",
                self.info.name, self.info.element_type, self.info.size.as_ref().unwrap(), self.info.size.as_ref().unwrap()
            )
        } else {
            write!(f, "{}[{}]", self.info.name, self.info.size.as_ref().unwrap())
        }
    }
}

/// Represents an extracted C comment
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExtractedComment {
    pub id: Id,
    pub code: String,
    pub info: CommentInfo,
    pub tokens: Vec<Token>,
    pub token_range: Range<usize>,
    pub metadata: Vec<(String, String)>,
}
impl fmt::Display for ExtractedComment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.info.is_documentation {
            write!(f, "/// {}", self.code)
        } else if self.info.is_multiline {
            if self.code.contains('\n') {
                write!(f, "/* {} */", self.code)
            } else {
                write!(f, "// {}", self.code)
            }
        } else {
            write!(f, "// {}", self.code)
        }
    }
}

/// Represents an extracted C expression
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExtractedExpression {
    pub id: Id,
    pub code: String,
    pub info: ExpressionInfo,
    pub tokens: Vec<Token>,
    pub token_range: Range<usize>,
    pub metadata: Vec<(String, String)>,
}

impl fmt::Display for ExtractedExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.info.expression_type.as_str() {
            "ternary" => {
                let parts: Vec<&str> = self.info.right_operand.split(" : ").collect();
                if parts.len() == 2 {
                    write!(
                        f,
                        "if {} {{ {} }} else {{ {} }}",
                        self.info.left_operand, parts[0], parts[1]
                    )
                } else {
                    write!(
                        f,
                        "{} {} {}",
                        self.info.left_operand, self.info.operator, self.info.right_operand
                    )
                }
            }
            _ => write!(
                f,
                "{} {} {}",
                self.info.left_operand, self.info.operator, self.info.right_operand
            ),
        }
    }
}

/// Represents an extracted C control flow statement
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExtractedControlFlow {
    pub control_type: String,
    pub condition: Vec<Token>,
    pub body: Vec<Token>,
    pub tokens: Vec<Token>,
    pub code: String,
    pub metadata: Vec<(String, String)>,
}

impl fmt::Display for ExtractedControlFlow {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let condition_str = self
            .condition
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<_>>()
            .join(" ");
        let body_str = self
            .body
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<_>>()
            .join(" ");

        match self.control_type.as_str() {
            "if" => write!(f, "if {} {{ {} }}", condition_str, body_str),
            "while" => write!(f, "while {} {{ {} }}", condition_str, body_str),
            "for" => write!(f, "for {} {{ {} }}", condition_str, body_str),
            _ => write!(
                f,
                "{} {} {{ {} }}",
                self.control_type, condition_str, body_str
            ),
        }
    }
}

// Helper for mapping C type strings to Rust types

pub fn tokens_to_rust(tokens: &[Token]) -> String {
    // Enhanced: handle pointers, arrays, and complex type specifications
    let mut base = None;
    let mut pointer_depth = 0;
    let mut is_const = false;
    let mut array_dims = Vec::new();
    let mut i = 0;
    let mut is_function_ptr = false;
    let mut fn_params = Vec::new();
    let fn_return = None;

    while i < tokens.len() {
        match &tokens[i] {
            tok!('*') => pointer_depth += 1,
            tok!('[') => {
                // Array: expect number or variable next
                if i + 1 < tokens.len() {
                    match &tokens[i + 1] {
                        tok!(n, i) => {
                            array_dims.push(n.to_string());
                            i += 1;
                        }
                        tok!(var) => {
                            array_dims.push(var.to_string());
                            i += 1;
                        }
                        _ => {}
                    }
                }
            }
            tok!('(') => {
                if pointer_depth > 0 {
                    // Potential function pointer
                    is_function_ptr = true;
                    // Parse parameters
                    let param_start = i + 1;
                    let mut nesting = 1;
                    let mut param_end = param_start;

                    while param_end < tokens.len() && nesting > 0 {
                        match &tokens[param_end] {
                            tok!('(') => nesting += 1,
                            tok!(')') => nesting -= 1,
                            _ => {}
                        }
                        if nesting > 0 {
                            param_end += 1;
                        }
                    }

                    if param_end < tokens.len() {
                        // Extract parameter tokens and parse them
                        let param_tokens = &tokens[param_start..param_end];
                        // Process parameters
                        if !param_tokens.is_empty() {
                            fn_params = vec!["_".to_string()]; // Placeholder for function pointer params
                        }
                        i = param_end;
                    }
                }
            }
            tok!(id) => {
                if *id == "const" {
                    is_const = true;
                } else if *id == "struct" && i + 1 < tokens.len() {
                    // Handle struct types
                    if let tok!(struct_name) = &tokens[i + 1] {
                        base = Some(struct_name);
                        i += 1;
                    }
                } else if *id == "enum" && i + 1 < tokens.len() {
                    // Handle enum types
                    if let tok!(enum_name) = &tokens[i + 1] {
                        base = Some(enum_name);
                        i += 1;
                    }
                } else if base.is_none() {
                    // Handle unsigned types
                    if *id == "unsigned" && i + 1 < tokens.len() {
                        if let tok!(next_id) = tokens[i + 1] {
                            match next_id {
                                "int" => {
                                    base = Some(&"u32");
                                    i += 1;
                                }
                                "char" => {
                                    base = Some(&"u8");
                                    i += 1;
                                }
                                "short" => {
                                    base = Some(&"u16");
                                }
                                "long" => {
                                    base = Some(&"u64");
                                    i += 1;
                                }
                                "long long" => {
                                    base = Some(&"u64");
                                    i += 1;
                                }
                                _ => base = Some(&"u32"),
                            }
                        }
                    } else if *id == "signed" && i + 1 < tokens.len() {
                        if let tok!(next_id) = &tokens[i + 1] {
                            match *next_id {
                                "int" => {
                                    base = Some(&"i32");
                                    i += 1;
                                }
                                "char" => {
                                    base = Some(&"i8");
                                    i += 1;
                                }
                                "short" => {
                                    base = Some(&"i16");
                                    i += 1;
                                }
                                "long" => {
                                    base = Some(&"i64");
                                    i += 1;
                                }
                                "long long" => {
                                    base = Some(&"i64");
                                    i += 1;
                                }
                                "double" => {
                                    base = Some(&"f64");
                                    i += 1;
                                }
                                "float" => {
                                    base = Some(&"f32");
                                    i += 1;
                                }
                                _ => {
                                    base = Some(&"i32");
                                    i += 1;
                                }
                            }
                        }
                    }
                }
            }
            _ => {}
        }
        i += 1;
    }

    let mut ty = base.unwrap_or(&"_").to_string();

    if is_function_ptr {
        // Handle function pointers
        let ret_type = fn_return.unwrap_or("()".to_string());
        let params = fn_params.join(", ");
        ty = format!("fn({}) -> {}", params, ret_type);
    } else {
        // Handle normal types
        for _ in 0..pointer_depth {
            ty = if is_const {
                format!("*const {}", ty)
            } else {
                format!("*mut {}", ty)
            };
        }

        if !array_dims.is_empty() {
            for dim in array_dims.iter().rev() {
                ty = format!("[{}; {}]", ty, dim);
            }
        }
    }

    ty
}

#[allow(dead_code)]
fn token_to_rust(token: &Token) -> String {
    match token {
        tok!(n, i) => n.to_string(),
        tok!('(') => "(".to_string(),
        tok!(')') => ")".to_string(),
        tok!('[') => "[".to_string(),
        tok!(']') => "]".to_string(),
        tok!('{') => "{".to_string(),
        tok!('}') => "}".to_string(),
        tok!(';') => ";".to_string(),
        tok!(',') => ",".to_string(),
        tok!('*') => "*".to_string(),
        tok!(id)
            if *id == "=="
                || *id == "!="
                || *id == "<="
                || *id == ">="
                || *id == "&&"
                || *id == "||" =>
        {
            id.to_string()
        }
        tok!(id) if *id == "++" => {
            // C's ++ operator has no direct equivalent in Rust
            // Best approximation is += 1
            "+= 1".to_string()
        }
        tok!(id) if *id == "--" => {
            // C's -- operator has no direct equivalent in Rust
            // Best approximation is -= 1
            "-= 1".to_string()
        }
        tok!(id) if *id == "->" => {
            // Convert C's arrow operator to Rust's dot operator with dereferencing
            ".".to_string() // Caller needs to handle the dereferencing part
        }
        tok!('=') => "=".to_string(),
        tok!('+') => "+".to_string(),
        tok!('-') => "-".to_string(),
        tok!('/') => "/".to_string(),
        tok!('%') => "%".to_string(),
        tok!('&') => "&".to_string(),
        tok!('|') => "|".to_string(),
        tok!('^') => "^".to_string(),
        tok!('!') => "!".to_string(),
        tok!('~') => "!".to_string(), // C's bitwise NOT is ~ but Rust uses ! for bitwise NOT
        tok!('<') => "<".to_string(),
        tok!('>') => ">".to_string(),
        tok!('.') => ".".to_string(),
        tok!('?') => "if".to_string(),   // Start of ternary operator
        tok!(':') => "else".to_string(), // Middle of ternary operator
        tok!('#') => "#".to_string(),
        tok!(id) => {
            // Properly handle escape sequences in string literals
            let mut escaped = String::new();
            let mut chars = id.chars().peekable();

            while let Some(c) = chars.next() {
                if c == '\\' {
                    if let Some(next) = chars.peek() {
                        match next {
                            'n' | 'r' | 't' | '\\' | '\'' | '\"' | '0' => {
                                // Standard escapes work the same in Rust
                                escaped.push(c);
                                escaped.push(*next);
                                chars.next();
                            }
                            'x' => {
                                // Handle hex escape
                                escaped.push('\\');
                                escaped.push('x');
                                chars.next();

                                // Get up to 2 hex digits
                                for _ in 0..2 {
                                    if let Some(digit) = chars.peek() {
                                        if digit.is_ascii_hexdigit() {
                                            escaped.push(*digit);
                                            chars.next();
                                        } else {
                                            break;
                                        }
                                    }
                                }
                            }
                            // Handle octal escapes (convert to hex for Rust)
                            '0'..='7' => {
                                // Collect up to 3 octal digits
                                let mut octal = String::new();
                                octal.push(*next);
                                chars.next();

                                for _ in 0..2 {
                                    if let Some(digit) = chars.peek() {
                                        if ('0'..='7').contains(digit) {
                                            octal.push(*digit);
                                            chars.next();
                                        } else {
                                            break;
                                        }
                                    }
                                }

                                // Convert octal to u8
                                if let Ok(byte) = u8::from_str_radix(&octal, 8) {
                                    // Format as hex escape for Rust
                                    escaped.push_str(&format!("\\x{:02x}", byte));
                                } else {
                                    // Fallback if parsing fails
                                    escaped.push('\\');
                                    escaped.push_str(&octal);
                                }
                            }
                            // Handle other escapes
                            _ => {
                                escaped.push('\\');
                                escaped.push(*next);
                                chars.next();
                            }
                        }
                    } else {
                        // Trailing backslash
                        escaped.push('\\');
                    }
                } else {
                    escaped.push(c);
                }
            }

            format!("\"{}\"", escaped)
        }
        _ => " ".to_string(), // Default for any other tokens
    }
}

/// Check if a token is one of the specified C qualifiers or specifiers
pub fn is_c_qualifier_or_specifier(token: &Token) -> bool {
    !token
        .matches(vec![
            tok!("const"),
            tok!("volatile"),
            tok!("static"),
            tok!("extern"),
            tok!("register"),
            tok!("inline"),
        ])
        .iter()
        .all(|x| x.is_none())
}

/// Check if a token is a C keyword that can be used in control flow
pub fn is_c_control_keyword(token: &Token) -> bool {
    !token
        .matches(vec![
            tok!("if"),
            tok!("else"),
            tok!("while"),
            tok!("for"),
            tok!("do"),
            tok!("switch"),
            tok!("case"),
            tok!("default"),
            tok!("return"),
            tok!("break"),
            tok!("continue"),
            tok!("goto"),
        ])
        .iter()
        .all(|x| x.is_none())
}

/// Try to convert C type tokens to Rust using the existing mapping system
/// This can be used by handlers when processing token patterns
pub fn to_rust_string(tokens: &[Token]) -> Option<String> {
    // Single token type
    if tokens.len() == 1 {
        return convert::convert_type_token(&tokens[0]);
    }
    Token::tokens_to_string(
        convert::convert_type_tokens(tokens.to_vec()).as_slice(),
        0,
        tokens.len(),
    )
}

/// Try to find multi-token type pattern starting at the given position
/// Returns a tuple of (matched_length, converted_type) if found, or None if not found
pub fn find_c_type_pattern(tokens: &[Token], start_pos: usize) -> Option<(usize, String)> {
    if start_pos >= tokens.len() {
        return None;
    }

    // Try different lengths of token sequences (up to 3 tokens for cases like "const unsigned int")
    for len in (1..=3).rev() {
        if start_pos + len > tokens.len() {
            continue;
        }

        let type_tokens = &tokens[start_pos..start_pos + len];
        if let Some(rust_type) = convert::convert_type_tokens(type_tokens.to_vec()) {
            return Some((len, rust_type.to_string()));
        }
    }

    // Try single token type conversion
    if convert::is_type_token(&tokens[start_pos]) {
        if let Some(rust_type) = convert::convert_type_token(&tokens[start_pos]) {
            return Some((1, rust_type));
        }
    }

    None
}
