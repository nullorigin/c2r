use crate::Token;
use std::cell::LazyCell;
use std::collections::HashMap;
use std::fmt;
/// Represents different types of converted Rust elements
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ConvertedElement {
    Function(ConvertedFunction),
    Struct(ConvertedStruct),
    Enum(ConvertedEnum),
    Typedef(ConvertedTypedef),
    Global(ConvertedGlobal),
    Macro(ConvertedMacro),
    Include(ConvertedInclude),
    Array(ConvertedArray),
    Comment(ConvertedComment),
    Expression(ConvertedExpression),
    ControlFlow(ConvertedControlFlow),
}

/// Represents a converted Rust function
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConvertedFunction {
    pub name: String,
    pub return_type: String,
    pub parameters: Vec<String>,
    pub body: String,
    pub code: String,
    pub is_unsafe: bool,
    pub is_public: bool,
    pub is_definition: bool,
}

impl fmt::Display for ConvertedFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.code)
    }
}

/// Represents a converted Rust struct
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConvertedStruct {
    pub fields: Vec<(String, String)>, // (field_name, field_type)
    pub code: String,
    pub is_public: bool,
    pub derives: Vec<String>,
}

impl fmt::Display for ConvertedStruct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.code)
    }
}

/// Represents a converted Rust enum
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConvertedEnum {
    pub variants: Vec<(String, Option<String>)>, // (name, value)
    pub code: String,
    pub is_public: bool,
    pub repr: Option<String>,
}

impl fmt::Display for ConvertedEnum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.code)
    }
}

/// Represents a converted Rust type alias
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConvertedTypedef {
    pub target_type: String,
    pub code: String,
    pub is_public: bool,
}

impl fmt::Display for ConvertedTypedef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.code)
    }
}

/// Represents a converted Rust global variable
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConvertedGlobal {
    pub var_type: String,
    pub initializer: Option<String>,
    pub code: String,
    pub is_const: bool,
    pub is_static: bool,
    pub is_public: bool,
}

impl fmt::Display for ConvertedGlobal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.code)
    }
}

/// Represents a converted Rust macro
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConvertedMacro {
    pub parameters: Vec<String>,
    pub body: String,
    pub code: String,
    pub is_function_like: bool,
}

impl fmt::Display for ConvertedMacro {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.code)
    }
}

/// Represents a converted Rust use statement
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConvertedInclude {
    pub path: String,
    pub code: String,
    pub is_external_crate: bool,
}

impl fmt::Display for ConvertedInclude {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.code)
    }
}

/// Represents a converted Rust array
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConvertedArray {
    pub element_type: String,
    pub size: String,
    pub code: String,
    pub is_declaration: bool,
}

impl fmt::Display for ConvertedArray {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.code)
    }
}

/// Represents a converted Rust comment
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConvertedComment {
    pub code: String,
    pub is_block: bool,
    pub is_doc_comment: bool,
}

impl fmt::Display for ConvertedComment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.code)
    }
}

/// Represents a converted Rust expression
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConvertedExpression {
    pub expression_type: String,
    pub left_operand: String,
    pub operator: String,
    pub right_operand: String,
    pub code: String,
    pub result_type: Option<String>,
}

impl fmt::Display for ConvertedExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.code)
    }
}

/// Represents a converted Rust control flow statement
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConvertedControlFlow {
    pub control_type: String,
    pub condition: String,
    pub body: String,
    pub code: String,
    pub has_else: bool,
}

impl fmt::Display for ConvertedControlFlow {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.code)
    }
}

pub const MAP: LazyCell<HashMap<String, String>> = LazyCell::new(|| create_map());
pub fn is_type_token(token: &Token) -> bool {
    MAP.contains_key(token.to_string().as_str())
}
pub fn convert_type(c_type: &str) -> Option<String> {
    MAP.get(c_type).map(|rust_type| rust_type.to_string())
}

pub fn convert_type_token(c_token: &Token) -> Option<String> {
    let type_str = c_token.to_string();
    MAP.get(type_str.trim())
        .map(|rust_type| rust_type.to_string())
}

pub fn convert_type_tokens(c_type: Vec<Token>) -> Option<Token> {
    let mut c_type_str = String::new();
    for t in c_type.iter() {
        let s = t.to_string();
        if s.trim() != "**"
            && s.trim() != "&&"
            && s.trim() != "*"
            && s.trim() != "&"
            && s.trim() != ""
        {
            c_type_str.push_str(s.trim());
            c_type_str.push(' ');
        } else {
            c_type_str.push_str(s.trim());
        }
    }
    MAP.get(&c_type_str)
        .map(|rust_type| Token::s(rust_type.to_string()))
}

pub fn is_c_keyword(token: Token) -> bool {
    [
        "auto".to_string(),
        "break".to_string(),
        "case".to_string(),
        "char".to_string(),
        "const".to_string(),
        "continue".to_string(),
        "default".to_string(),
        "do".to_string(),
        "else".to_string(),
        "enum".to_string(),
        "extern".to_string(),
        "float".to_string(),
        "for".to_string(),
        "goto".to_string(),
        "if".to_string(),
        "inline".to_string(),
        "int".to_string(),
        "long".to_string(),
        "register".to_string(),
        "return".to_string(),
        "signed".to_string(),
        "sizeof".to_string(),
        "static".to_string(),
        "struct".to_string(),
        "switch".to_string(),
        "typedef".to_string(),
        "union".to_string(),
        "unsigned".to_string(),
        "volatile".to_string(),
        "while".to_string(),
    ]
    .contains(&token.to_string())
}
pub fn create_map() -> HashMap<String, String> {
    let mut hm1: HashMap<&str, &str> = HashMap::new();
    // Basic C types
    hm1.insert("bool", "bool");
    hm1.insert("_Bool", "bool");
    hm1.insert("char", "i8");
    hm1.insert("short", "i16");
    hm1.insert("int", "i32");
    hm1.insert("long", "i64");
    hm1.insert("long long", "i64");
    hm1.insert("float", "f32");
    hm1.insert("double", "f64");
    hm1.insert("void", "()");

    // Size types
    hm1.insert("size_t", "usize");
    hm1.insert("ssize_t", "isize");
    hm1.insert("ptrdiff_t", "isize");
    hm1.insert("intptr_t", "isize");
    hm1.insert("uintptr_t", "usize");

    // Fixed width integer types
    hm1.insert("int8_t", "i8");
    hm1.insert("int16_t", "i16");
    hm1.insert("int32_t", "i32");
    hm1.insert("int64_t", "i64");
    hm1.insert("uint8_t", "u8");
    hm1.insert("uint16_t", "u16");
    hm1.insert("uint32_t", "u32");
    hm1.insert("uint64_t", "u64");
    hm1.insert("intmax_t", "i64");
    hm1.insert("uintmax_t", "u64");

    // Fast integer types
    hm1.insert("int_fast8_t", "i8");
    hm1.insert("int_fast16_t", "i16");
    hm1.insert("int_fast32_t", "i32");
    hm1.insert("int_fast64_t", "i64");
    hm1.insert("uint_fast8_t", "u8");
    hm1.insert("uint_fast16_t", "u16");
    hm1.insert("uint_fast32_t", "u32");
    hm1.insert("uint_fast64_t", "u64");

    // Least integer types
    hm1.insert("int_least8_t", "i8");
    hm1.insert("int_least16_t", "i16");
    hm1.insert("int_least32_t", "i32");
    hm1.insert("int_least64_t", "i64");
    hm1.insert("uint_least8_t", "u8");
    hm1.insert("uint_least16_t", "u16");
    hm1.insert("uint_least32_t", "u32");
    hm1.insert("uint_least64_t", "u64");

    // Unsigned types
    hm1.insert("unsigned char", "u8");
    hm1.insert("unsigned short", "u16");
    hm1.insert("unsigned int", "u32");
    hm1.insert("unsigned long", "u64");
    hm1.insert("unsigned long long", "u64");

    // Common C library types
    hm1.insert("FILE", "libc::FILE");
    hm1.insert("DIR", "libc::DIR");
    hm1.insert("time_t", "libc::time_t");
    hm1.insert("clock_t", "libc::clock_t");
    hm1.insert("tm", "libc::tm");
    hm1.insert("timespec", "libc::timespec");
    hm1.insert("timeval", "libc::timeval");
    hm1.insert("va_list", "libc::va_list");
    hm1.insert("pthread_t", "libc::pthread_t");
    hm1.insert("pthread_mutex_t", "libc::pthread_mutex_t");
    hm1.insert("pthread_cond_t", "libc::pthread_cond_t");
    hm1.insert("complex", "libc::complex");
    hm1.insert("_Complex", "libc::complex");
    hm1.insert("_Imaginary", "libc::imaginary");
    // Socket and network types
    hm1.insert("sockaddr", "libc::sockaddr");
    hm1.insert("sockaddr_in", "libc::sockaddr_in");
    hm1.insert("sockaddr_in6", "libc::sockaddr_in6");
    hm1.insert("socklen_t", "libc::socklen_t");
    hm1.insert("in_addr", "libc::in_addr");
    hm1.insert("in6_addr", "libc::in6_addr");

    // Common POSIX types
    hm1.insert("mode_t", "libc::mode_t");
    hm1.insert("pid_t", "libc::pid_t");
    hm1.insert("uid_t", "libc::uid_t");
    hm1.insert("gid_t", "libc::gid_t");
    hm1.insert("off_t", "libc::off_t");
    hm1.insert("dev_t", "libc::dev_t");
    hm1.insert("ino_t", "libc::ino_t");
    hm1.insert("nlink_t", "libc::nlink_t");
    hm1.insert("blkcnt_t", "libc::blkcnt_t");
    hm1.insert("blksize_t", "libc::blksize_t");
    let mut hm2 = HashMap::<String, String>::new();
    // Add derived pointer and reference types for all basic mappings
    for (c_type, rust_type) in hm1.iter_mut() {
        hm2.insert(c_type.to_string(), rust_type.to_string());
        // Generate pointer variants
        hm2.insert(format!("{}*", c_type), format!("*mut {}", rust_type));
        hm2.insert(
            format!("const {}*", c_type),
            format!("*const {}", rust_type),
        );

        // Generate double pointer variants
        hm2.insert(format!("{}**", c_type), format!("*mut *mut {}", rust_type));
        hm2.insert(
            format!("const {}**", c_type),
            format!("*const *const {}", rust_type),
        );

        // Generate reference variants (C++ style)
        hm2.insert(format!("{}&", c_type), format!("&mut {}", rust_type));
        hm2.insert(format!("const {}&", c_type), format!("&{}", rust_type));

        // Generate pointer-reference combinations
        hm2.insert(format!("*{}&", c_type), format!("&*mut {}", rust_type));
        hm2.insert(
            format!("const *{}&", c_type),
            format!("&*const {}", rust_type),
        );
    }
    // Implementation logic for type conversion
    hm2
}
