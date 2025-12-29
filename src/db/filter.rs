//! Filter Enums and String Arrays
//!
//! Provides strongly-typed enums for built-in kinds and categories,
//! with conversions to/from string representations.

use std::borrow::Cow;

// ============================================================================
// Kind Enum and String Array
// ============================================================================

/// All built-in kind strings indexed by Kind enum variants
pub const KIND_STRINGS: &[&str] = &[
    "Function",
    "Struct",
    "Enum",
    "Variant",
    "Field",
    "Param",
    "Variable",
    "Constant",
    "Typedef",
    "Macro",
    "Include",
    "Comment",
    "Expression",
    "Statement",
    "Block",
    "Loop",
    "Conditional",
    "Switch",
    "Case",
    "Return",
    "Break",
    "Continue",
    "Goto",
    "Label",
    "Array",
    "Pointer",
    "Reference",
    "Cast",
    "Call",
    "Assignment",
    "BinaryOp",
    "UnaryOp",
    "Literal",
    "Identifier",
    "Type",
    "Attribute",
    "Annotation",
    "Transform",
    "Report",
    "Pattern",
    "Sample",
    "Keyword",
    "Token",
    "TokenSet",
    "Config",
    "Node",
    "Branch",
    "Root",
    "Unknown",
    // Handler-related kinds
    "Handler",
    "Route",
    "Union",
    "Global",
    "Preprocessor",
    "TypeMetadata",
    "PatternRuleCallback",
    "PatternMatch",
    "CodeSegment",
    "HandlerResult",
    "HandlerStats",
    "HandlerMetadata",
    "SuccessRecord",
    "FailureRecord",
    "RoutingRule",
    "PatternUsage",
    "RoutingStats",
    "GlobalHandlerStats",
    "TokenRange",
    "Order",
    "Relationship",
];

/// Strongly-typed enum for built-in kinds
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(usize)]
pub enum Kind {
    Function = 0,
    Struct = 1,
    Enum = 2,
    Variant = 3,
    Field = 4,
    Param = 5,
    Variable = 6,
    Constant = 7,
    Typedef = 8,
    Macro = 9,
    Include = 10,
    Comment = 11,
    Expression = 12,
    Statement = 13,
    Block = 14,
    Loop = 15,
    Conditional = 16,
    Switch = 17,
    Case = 18,
    Return = 19,
    Break = 20,
    Continue = 21,
    Goto = 22,
    Label = 23,
    Array = 24,
    Pointer = 25,
    Reference = 26,
    Cast = 27,
    Call = 28,
    Assignment = 29,
    BinaryOp = 30,
    UnaryOp = 31,
    Literal = 32,
    Identifier = 33,
    Type = 34,
    Attribute = 35,
    Annotation = 36,
    Transform = 37,
    Report = 38,
    Pattern = 39,
    Sample = 40,
    Keyword = 41,
    Token = 42,
    TokenSet = 43,
    Config = 44,
    Node = 45,
    Branch = 46,
    Root = 47,
    Unknown = 48,
    // Handler-related kinds
    Handler = 49,
    Route = 50,
    Union = 51,
    Global = 52,
    Preprocessor = 53,
    TypeMetadata = 54,
    PatternRuleCallback = 55,
    PatternMatch = 56,
    CodeSegment = 57,
    HandlerResult = 58,
    HandlerStats = 59,
    HandlerMetadata = 60,
    SuccessRecord = 61,
    FailureRecord = 62,
    RoutingRule = 63,
    PatternUsage = 64,
    RoutingStats = 65,
    GlobalHandlerStats = 66,
    TokenRange = 67,
    Order = 68,
    Relationship = 69,
}

impl Kind {
    /// Get the string representation of this kind
    pub fn as_str(&self) -> &'static str {
        KIND_STRINGS[*self as usize]
    }

    /// Get all available kinds
    pub fn all() -> &'static [&'static str] {
        KIND_STRINGS
    }

    /// Try to parse a kind from a string
    pub fn from_str(s: &str) -> Option<Self> {
        KIND_STRINGS
            .iter()
            .position(|&k| k.eq_ignore_ascii_case(s))
            .and_then(Self::from_index)
    }

    /// Create from index
    pub fn from_index(index: usize) -> Option<Self> {
        if index < KIND_STRINGS.len() {
            // Safety: index is checked to be within bounds
            Some(unsafe { std::mem::transmute::<usize, Kind>(index) })
        } else {
            None
        }
    }

    /// Get the index of this kind
    pub fn index(&self) -> usize {
        *self as usize
    }
}

impl From<Kind> for &'static str {
    fn from(kind: Kind) -> Self {
        kind.as_str()
    }
}

impl From<Kind> for String {
    fn from(kind: Kind) -> Self {
        kind.as_str().to_string()
    }
}

impl<'a> From<Kind> for Cow<'a, str> {
    fn from(kind: Kind) -> Self {
        Cow::Borrowed(kind.as_str())
    }
}

impl std::fmt::Display for Kind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

// ============================================================================
// Category Enum and String Array
// ============================================================================

/// All built-in category strings indexed by Category enum variants
pub const CATEGORY_STRINGS: &[&str] = &[
    "code",
    "declaration",
    "definition",
    "statement",
    "expression",
    "type",
    "modifier",
    "operator",
    "literal",
    "identifier",
    "keyword",
    "preprocessor",
    "comment",
    "whitespace",
    "punctuation",
    "control_flow",
    "loop",
    "conditional",
    "jump",
    "function",
    "struct",
    "enum",
    "union",
    "typedef",
    "variable",
    "constant",
    "parameter",
    "field",
    "array",
    "pointer",
    "reference",
    "cast",
    "call",
    "assignment",
    "comparison",
    "arithmetic",
    "bitwise",
    "logical",
    "memory",
    "io",
    "error",
    "warning",
    "info",
    "debug",
    "analysis",
    "transform",
    "report",
    "pattern",
    "sample",
    "token",
    "config",
    "metadata",
    "annotation",
    "attribute",
    "unknown",
    "success",
    "skipped",
    "handler",
    "routing",
    "callback",
    "macro",
    "global",
    "integer",
    "float",
    "boolean",
    "size",
    "system",
    "storage",
    "function_pointer",
];

/// Strongly-typed enum for built-in categories
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(usize)]
pub enum Category {
    Code = 0,
    Declaration = 1,
    Definition = 2,
    Statement = 3,
    Expression = 4,
    Type = 5,
    Modifier = 6,
    Operator = 7,
    Literal = 8,
    Identifier = 9,
    Keyword = 10,
    Preprocessor = 11,
    Comment = 12,
    Whitespace = 13,
    Punctuation = 14,
    ControlFlow = 15,
    Loop = 16,
    Conditional = 17,
    Jump = 18,
    Function = 19,
    Struct = 20,
    Enum = 21,
    Union = 22,
    Typedef = 23,
    Variable = 24,
    Constant = 25,
    Parameter = 26,
    Field = 27,
    Array = 28,
    Pointer = 29,
    Reference = 30,
    Cast = 31,
    Call = 32,
    Assignment = 33,
    Comparison = 34,
    Arithmetic = 35,
    Bitwise = 36,
    Logical = 37,
    Memory = 38,
    Io = 39,
    Error = 40,
    Warning = 41,
    Info = 42,
    Debug = 43,
    Analysis = 44,
    Transform = 45,
    Report = 46,
    Pattern = 47,
    Sample = 48,
    Token = 49,
    Config = 50,
    Metadata = 51,
    Annotation = 52,
    Attribute = 53,
    Unknown = 54,
    Success = 55,
    Skipped = 56,
    Handler = 57,
    Routing = 58,
    Callback = 59,
    Macro = 60,
    Global = 61,
    Integer = 62,
    FloatCategory = 63,
    BooleanCategory = 64,
    Size = 65,
    System = 66,
    Storage = 67,
    FunctionPointer = 68,
}

impl Category {
    /// Get the string representation of this category
    pub fn as_str(&self) -> &'static str {
        CATEGORY_STRINGS[*self as usize]
    }

    /// Get all available categories
    pub fn all() -> &'static [&'static str] {
        CATEGORY_STRINGS
    }

    /// Try to parse a category from a string
    pub fn from_str(s: &str) -> Option<Self> {
        CATEGORY_STRINGS
            .iter()
            .position(|&c| c.eq_ignore_ascii_case(s))
            .and_then(Self::from_index)
    }

    /// Create from index
    pub fn from_index(index: usize) -> Option<Self> {
        if index < CATEGORY_STRINGS.len() {
            // Safety: index is checked to be within bounds
            Some(unsafe { std::mem::transmute::<usize, Category>(index) })
        } else {
            None
        }
    }

    /// Get the index of this category
    pub fn index(&self) -> usize {
        *self as usize
    }
}

impl From<Category> for &'static str {
    fn from(cat: Category) -> Self {
        cat.as_str()
    }
}

impl From<Category> for String {
    fn from(cat: Category) -> Self {
        cat.as_str().to_string()
    }
}

impl<'a> From<Category> for Cow<'a, str> {
    fn from(cat: Category) -> Self {
        Cow::Borrowed(cat.as_str())
    }
}

impl std::fmt::Display for Category {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

// ============================================================================
// KindFilter - Accepts Kind, &str, or String
// ============================================================================

/// A filter value that can be created from Kind, &str, or String
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct KindFilter(String);

impl KindFilter {
    pub fn new(s: impl AsRef<str>) -> Self {
        KindFilter(s.as_ref().to_string())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl From<Kind> for KindFilter {
    fn from(kind: Kind) -> Self {
        KindFilter(kind.as_str().to_string())
    }
}

impl From<&str> for KindFilter {
    fn from(s: &str) -> Self {
        KindFilter(s.to_string())
    }
}

impl From<String> for KindFilter {
    fn from(s: String) -> Self {
        KindFilter(s)
    }
}

impl From<&String> for KindFilter {
    fn from(s: &String) -> Self {
        KindFilter(s.clone())
    }
}

impl AsRef<str> for KindFilter {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

// ============================================================================
// CategoryFilter - Accepts Category, &str, or String
// ============================================================================

/// A filter value that can be created from Category, &str, or String
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CategoryFilter(String);

impl CategoryFilter {
    pub fn new(s: impl AsRef<str>) -> Self {
        CategoryFilter(s.as_ref().to_string())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl From<Category> for CategoryFilter {
    fn from(cat: Category) -> Self {
        CategoryFilter(cat.as_str().to_string())
    }
}

impl From<&str> for CategoryFilter {
    fn from(s: &str) -> Self {
        CategoryFilter(s.to_string())
    }
}

impl From<String> for CategoryFilter {
    fn from(s: String) -> Self {
        CategoryFilter(s)
    }
}

impl From<&String> for CategoryFilter {
    fn from(s: &String) -> Self {
        CategoryFilter(s.clone())
    }
}

impl AsRef<str> for CategoryFilter {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

// ============================================================================
// Convenience Functions
// ============================================================================

/// Check if a string is a valid built-in kind
pub fn is_builtin_kind(s: &str) -> bool {
    Kind::from_str(s).is_some()
}

/// Check if a string is a valid built-in category
pub fn is_builtin_category(s: &str) -> bool {
    Category::from_str(s).is_some()
}

/// Get all built-in kind strings
pub fn builtin_kinds() -> &'static [&'static str] {
    KIND_STRINGS
}

/// Get all built-in category strings
pub fn builtin_categories() -> &'static [&'static str] {
    CATEGORY_STRINGS
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_kind_as_str() {
        assert_eq!(Kind::Function.as_str(), "Function");
        assert_eq!(Kind::Report.as_str(), "Report");
        assert_eq!(Kind::Unknown.as_str(), "Unknown");
    }

    #[test]
    fn test_kind_from_str() {
        assert_eq!(Kind::from_str("Function"), Some(Kind::Function));
        assert_eq!(Kind::from_str("function"), Some(Kind::Function)); // case insensitive
        assert_eq!(Kind::from_str("REPORT"), Some(Kind::Report));
        assert_eq!(Kind::from_str("invalid"), None);
    }

    #[test]
    fn test_kind_index() {
        assert_eq!(Kind::Function.index(), 0);
        assert_eq!(Kind::Struct.index(), 1);
        assert_eq!(Kind::from_index(0), Some(Kind::Function));
        assert_eq!(Kind::from_index(100), None);
    }

    #[test]
    fn test_category_as_str() {
        assert_eq!(Category::Code.as_str(), "code");
        assert_eq!(Category::Function.as_str(), "function");
        assert_eq!(Category::Unknown.as_str(), "unknown");
    }

    #[test]
    fn test_category_from_str() {
        assert_eq!(Category::from_str("code"), Some(Category::Code));
        assert_eq!(Category::from_str("CODE"), Some(Category::Code)); // case insensitive
        assert_eq!(Category::from_str("function"), Some(Category::Function));
        assert_eq!(Category::from_str("invalid"), None);
    }

    #[test]
    fn test_kind_filter() {
        let f1: KindFilter = Kind::Function.into();
        let f2: KindFilter = "Function".into();
        let f3: KindFilter = String::from("Function").into();

        assert_eq!(f1.as_str(), "Function");
        assert_eq!(f2.as_str(), "Function");
        assert_eq!(f3.as_str(), "Function");
    }

    #[test]
    fn test_category_filter() {
        let f1: CategoryFilter = Category::Code.into();
        let f2: CategoryFilter = "code".into();
        let f3: CategoryFilter = String::from("code").into();

        assert_eq!(f1.as_str(), "code");
        assert_eq!(f2.as_str(), "code");
        assert_eq!(f3.as_str(), "code");
    }

    #[test]
    fn test_kind_into_string() {
        let s: String = Kind::Function.into();
        assert_eq!(s, "Function");

        let s: &str = Kind::Struct.into();
        assert_eq!(s, "Struct");
    }

    #[test]
    fn test_category_into_string() {
        let s: String = Category::Code.into();
        assert_eq!(s, "code");

        let s: &str = Category::Function.into();
        assert_eq!(s, "function");
    }

    #[test]
    fn test_builtin_checks() {
        assert!(is_builtin_kind("Function"));
        assert!(is_builtin_kind("function"));
        assert!(!is_builtin_kind("NotAKind"));

        assert!(is_builtin_category("code"));
        assert!(is_builtin_category("CODE"));
        assert!(!is_builtin_category("NotACategory"));
    }
}
