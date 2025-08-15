use std::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Not};
use crate::pointer::Pointer;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(u128)]
pub enum Attribute {
    None = 0,
    Const = 1 << 0,
    Mut = 1 << 1,
    Ref = 1 << 2,
    Ptr = 1 << 3,
    Value = 1 << 4,
    Array = 1 << 5,
    Static = 1 << 6,
    Deref = 1 << 7,
    Expr = 1 << 8,
    Void = 1 << 9,
    Define = 1 << 10,
    Macro = 1 << 11,
    Inline = 1 << 12,
    Return = 1 << 13,
    Arg = 1 << 14,
    Type = 1 << 15,
    Enum = 1 << 16,
    Struct = 1 << 17,
    Union = 1 << 18,
    Field = 1 << 19,
    Function = 1 << 20,
    Label = 1 << 21,
    Ctor = 1 << 22,
    Dtor = 1 << 23,
    Vtable = 1 << 24,
    Bitfield = 1 << 25,
    Assignment = 1 << 26,
    Operator = 1 << 27,
    Cast = 1 << 28,
    Call = 1 << 29,
    New = 1 << 30,
    Delete = 1 << 31,
    Comment = 1 << 32,
    Error = 1 << 33,
    Deprecated = 1 << 34,
    Extern = 1 << 35,
    Friend = 1 << 36,
    Virtual = 1 << 37,
    Override = 1 << 38,
    Final = 1 << 39,
    Private = 1 << 40,
    Protected = 1 << 41,
    Public = 1 << 42,
    Assertion = 1 << 43,
    True = 1 << 44,
    False = 1 << 45,
    Null = 1 << 46,
    Bool = 1 << 47,
    Char = 1 << 48,
    Signed = 1 << 49,
    Unsigned = 1 << 50,
    Float = 1 << 51,
    Double = 1 << 52,
    Complex = 1 << 53,
    Integer = 1 << 54,
    Real = 1 << 55,
    Builtin = 1 << 56,
    Typeof = 1 << 57,
    Cloneable = 1 << 58,
    Copyable = 1 << 59,
    Swappable = 1 << 60,
    Movable = 1 << 61,
    Equal = 1 << 62,
    NotEqual = 1 << 63,
    LessThan = 1 << 64,
    GreaterThan = 1 << 65,
    LessThanOrEqual = 1 << 66,
    GreaterThanOrEqual = 1 << 67,
    All = !0,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(u128)]
pub enum Operatotion {
    Assign = 1 << 0,
    AddAssign = 1 << 1,
    SubtractAssign = 1 << 2,
    MultiplyAssign = 1 << 3,
    DivideAssign = 1 << 4,
    AndAssign = 1 << 5,
    OrAssign = 1 << 6,
    XorAssign = 1 << 7,
    LeftShiftAssign = 1 << 8,
    RightShiftAssign = 1 << 9,
    ModuloAssign = 1 << 10,
    BitAndAssign = 1 << 11,
    BitOrAssign = 1 << 12,
    BitXorAssign = 1 << 13,
    Add = 1 << 14,
    Subtract = 1 << 15,
    Multiply = 1 << 16,
    Divide = 1 << 17,
    And = 1 << 18,
    Or = 1 << 19,
    Xor = 1 << 20,
    LeftShift = 1 << 21,
    RightShift = 1 << 22,
    Modulo = 1 << 23,
    BitAnd = 1 << 24,
    BitOr = 1 << 25,
    BitXor = 1 << 26,
    Not = 1 << 27,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(u128)]
pub enum Comparison {
    Equal = 1 << 0,
    NotEqual = 1 << 1,
    LessThan = 1 << 2,
    GreaterThan = 1 << 3,
    LessThanOrEqual = 1 << 4,
    GreaterThanOrEqual = 1 << 5,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(u128)]
pub enum Symbol {
    Plus = 1 << 0,
    Minus = 1 << 1,
    Star = 1 << 2,
    Slash = 1 << 3,
    Percent = 1 << 4,
    Caret = 1 << 5,
    Ampersand = 1 << 6,
    Pipe = 1 << 7,
    Equals = 1 << 8,
    Exclamation = 1 << 9,
    Tilde = 1 << 10,
    DoubleQuote = 1 << 11,
    SingleQuote = 1 << 12,
    Backtick = 1 << 13,
    OpenParen = 1 << 14,
    CloseParen = 1 << 15,
    OpenBrace = 1 << 16,
    CloseBrace = 1 << 17,
    OpenBracket = 1 << 18,
    CloseBracket = 1 << 19,
    OpenAngle = 1 << 20,
    CloseAngle = 1 << 21,
    Comma = 1 << 22,
    Dot = 1 << 23,
    Semicolon = 1 << 24,
    Colon = 1 << 25,
    Question = 1 << 26,
    Backslash = 1 << 27,
    Hash = 1 << 28,
    At = 1 << 29,
    Underscore = 1 << 30,
    All = !0,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(u128)]
pub enum Keyword {
    As = 1 << 0,
    Break = 1 << 1,
    Const = 1 << 2,
    Continue = 1 << 3,
    Crate = 1 << 4,
    Else = 1 << 5,
    Enum = 1 << 6,
    Extern = 1 << 7,
    False = 1 << 8,
    Fn = 1 << 9,
    For = 1 << 10,
    If = 1 << 11,
    Impl = 1 << 12,
    In = 1 << 13,
    Let = 1 << 14,
    Loop = 1 << 15,
    Match = 1 << 16,
    Mod = 1 << 17,
    Move = 1 << 18,
    Mut = 1 << 19,
    Pub = 1 << 20,
    Ref = 1 << 21,
    Return = 1 << 22,
    Self_ = 1 << 23,
    Static = 1 << 24,
    Struct = 1 << 25,
    Super = 1 << 26,
    Trait = 1 << 27,
    True = 1 << 28,
    Type = 1 << 29,
    Unsafe = 1 << 30,
    Use = 1 << 31,
    Where = 1 << 32,
    While = 1 << 33,
    Async = 1 << 34,
    Await = 1 << 35,
    Dyn = 1 << 36,
    Abstract = 1 << 37,
    Become = 1 << 38,
    Box = 1 << 39,
    Do = 1 << 40,
    Final = 1 << 41,
    Macro = 1 << 42,
    Override = 1 << 43,
    Priv = 1 << 44,
    Typeof = 1 << 45,
    Unsized = 1 << 46,
    Virtual = 1 << 47,
    Yield = 1 << 48,
    Try = 1 << 49,
    Union = 1 << 50,
    Assert = 1 << 51,
    MacroRules = 1 << 52,
    MacroExport = 1 << 53,
    MacroUse = 1 << 54,
    Boolean = 1 << 55,
    Or = 1 << 56,
    And = 1 << 57,
    Not = 1 << 58,
    IfLet = 1 << 59,
    ElseIf = 1 << 60,
    LetElse = 1 << 61,
    MatchArm = 1 << 62,
    All = !0,
}
impl Attribute {
    pub fn none() -> Attribute {
        Attribute::None
    }
    pub fn toggle(&mut self) {
        *self = !*self;
    }
    pub fn set(&mut self, flag: Attribute) {
        *self |= flag;
    }
    pub fn unset(&mut self, flag: Attribute) {
        *self &= !flag;
    }
    pub fn reset(&mut self) {
        *self = Attribute::None;
    }
    pub fn has(&self, flag: Attribute) -> bool {
        *self & flag == flag
    }
    pub fn is(&self, flag: Attribute) -> bool {
        *self == flag
    }
    pub fn is_not(&self, flag: Attribute) -> bool {
        *self != flag
    }
}

impl From<Attribute> for u64 {
    fn from(value: Attribute) -> u64 {
        value as u64
    }
}

impl From<u128> for Attribute {
    fn from(value: u128) -> Attribute {
        unsafe { std::mem::transmute(value) }
    }
}

impl BitOr for Attribute {
    type Output = Attribute;
    fn bitor(self, rhs: Self) -> Self::Output {
        Attribute::from(self as u128 | rhs as u128)
    }
}

impl BitOrAssign for Attribute {
    fn bitor_assign(&mut self, rhs: Self) {
        *self = *self | rhs;
    }
}

impl BitAnd for Attribute {
    type Output = Attribute;
    fn bitand(self, rhs: Self) -> Self::Output {
        Attribute::from(self as u128 & rhs as u128)
    }
}

impl BitAndAssign for Attribute {
    fn bitand_assign(&mut self, rhs: Self) {
        *self = *self & rhs;
    }
}

impl Not for Attribute {
    type Output = Attribute;
    fn not(self) -> Self::Output {
        Attribute::from(!(self as u128))
    }
}

impl BitXor for Attribute {
    type Output = Attribute;
    fn bitxor(self, rhs: Self) -> Self::Output {
        Attribute::from(self as u128 ^ rhs as u128)
    }
}

impl BitXorAssign for Attribute {
    fn bitxor_assign(&mut self, rhs: Self) {
        *self = *self ^ rhs;
    }
}

pub struct Type {
    pub name: String,
    pub flags: Attribute,
    pub value: Option<String>,
    pub callback: Option<fn(&str) -> String>
}

impl Type {
    pub fn new(name: &str) -> Type {
        Type {
            name: name.to_string(),
            flags: Attribute::None,
            value: None,
            callback: None
        }
    }
    pub fn set_callback(mut self, callback: fn(&str) -> String) -> Type {
        self.callback = Some(callback);
        self
    }
    pub fn set_value(mut self, value: String) -> Type {
        self.value = Some(value);
        self
    }
    pub fn add_flag(mut self, flag: Attribute) -> Type {
        self.flags |= flag;
        self
    }
    pub fn del_flag(mut self, flag: Attribute) -> Type {
        self.flags &= !flag;
        self
    }
    pub fn toggle_flag(mut self, flag: Attribute) -> Type {
        self.flags ^= flag;
        self
    }
    pub fn has_flag(self, flag: Attribute) -> bool {
        self.flags & flag == flag
    }
    pub fn is_flag(self, flag: Attribute) -> bool {
        self.flags == flag
    }
    pub fn callback(self) -> Option<fn(&str) -> String> {
        self.callback
    }
    pub fn convert(c_type: &str) -> String {
        // Check for pointers first
        if c_type.contains('*') || c_type.contains('&') {
            return Pointer::convert_type(&c_type);
        }
        // Remove qualifiers
        let base_type = c_type
            .trim_start_matches("const").trim()
            .trim_start_matches("static").trim()
            .trim_start_matches("volatile").trim()
            .trim_start_matches("struct").trim()
            .trim_start_matches("union").trim()
            .trim_start_matches("enum").trim();
        // Map common C types to Rust types
        let base_type = match base_type {
            "void" => "()",
            "char" => "i8",
            "unsigned char" => "u8",
            "short" => "i16",
            "unsigned short" => "u16",
            "int" => "i32",
            "unsigned int" => "u32",
            "long" => "i64",
            "unsigned long" => "u64",
            "long long" => "i64",
            "unsigned long long" => "u64",
            "float" => "f32",
            "double" => "f64",
            "size_t" => "usize",
            "ssize_t" => "isize",
            "int8_t" => "i8",
            "uint8_t" => "u8",
            "int16_t" => "i16",
            "uint16_t" => "u16",
            "int32_t" => "i32",
            "uint32_t" => "u32",
            "int64_t" => "i64",
            "uint64_t" => "u64",
            // Handle struct types - remove "struct " prefix
            t if t.starts_with("struct ") => t.strip_prefix("struct ").unwrap_or(t),
            // Default - keep as is
            _ => base_type
        }.to_string();
        return base_type;
    }

    /// Check if a name is a Rust keyword and escape it if necessary
    pub fn escape(name: &str) -> String {
        match name {
            "as" | "break" | "const" | "continue" | "crate" | "else" | "enum" |
            "extern" | "false" | "fn" | "for" | "if" | "impl" | "in" | "let" |
            "loop" | "match" | "mod" | "move" | "mut" | "pub" | "ref" | "return" |
            "self" | "Self" | "static" | "struct" | "super" | "trait" | "true" |
            "type" | "unsafe" | "use" | "where" | "while" | "async" | "await" |
            "dyn" | "abstract" | "become" | "box" | "do" | "final" | "macro" |
            "override" | "priv" | "typeof" | "unsized" | "virtual" | "yield" |
            "try" | "union" => format!("{}_", name),
            _ => name.to_string(),
        }
    }
    pub fn sanitize_name(name: &str) -> String {
        let name = name.replace(" ", "_");
        let name = name.replace("*", "");
        let name = name.replace("const", "");
        let name = name.replace("volatile", "");
        let name = name.replace("unsigned", "");
        let name = name.replace("static", "");
        let name = name.replace("struct", "");
        let name = name.replace("union", "");
        let name = name.replace("enum", "");

        let mut name = name.split_whitespace().collect::<Vec<&str>>();
        name.retain(|s| !s.is_empty());
        let name = name.join("_");

        // If the name is a number, prefix it with an underscore
        if name.chars().all(char::is_numeric) {
            return format!("_{}", name);
        }

        name
    }
}