//! Type and Identifier Conversion
//!
//! Provides utilities for converting C types, identifiers, and operators to their Rust equivalents.
//!
//! Key components:
//! - `TypeCategory`: Classification of C types
//! - `TypeMetadata`: Metadata for type conversions
//! - `TypeConverter`: Generic type conversion engine
//! - `IdentifierCase`: Case conversion utilities for identifiers
//! - Operator conversion utilities

use crate::db::web::Entry;
use std::collections::HashMap;

// ============================================================================
// Type Conversion System
// ============================================================================

/// Category classification for types
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeCategory {
    /// Basic integer types (int, char, short, long)
    BasicInteger,
    /// Floating point types (float, double)
    BasicFloat,
    /// Fixed-width integer types (int32_t, uint64_t, etc.)
    FixedWidthInteger,
    /// Size-related types (size_t, ssize_t, ptrdiff_t)
    SizeType,
    /// System-specific types
    SystemType,
    /// Pointer types
    PointerType,
    /// Storage qualifiers (static, const, extern)
    StorageQualifier,
    /// User-defined types
    UserDefined,
    /// Function pointer types
    FunctionPointer,
    /// Struct or union types
    StructUnion,
    /// Custom category
    Custom(String),
}

impl TypeCategory {
    pub fn as_str(&self) -> &str {
        match self {
            TypeCategory::BasicInteger => "basic_integer",
            TypeCategory::BasicFloat => "basic_float",
            TypeCategory::FixedWidthInteger => "fixed_width_integer",
            TypeCategory::SizeType => "size_type",
            TypeCategory::SystemType => "system_type",
            TypeCategory::PointerType => "pointer_type",
            TypeCategory::StorageQualifier => "storage_qualifier",
            TypeCategory::UserDefined => "user_defined",
            TypeCategory::FunctionPointer => "function_pointer",
            TypeCategory::StructUnion => "struct_union",
            TypeCategory::Custom(s) => s.as_str(),
        }
    }
}

/// Metadata for type conversions
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeMetadata {
    /// Source type name
    pub source_type: String,
    /// Target type name
    pub target_type: String,
    /// Type category
    pub category: TypeCategory,
    /// Whether the type is signed
    pub is_signed: bool,
    /// Whether this is a pointer type
    pub is_pointer: bool,
    /// Pointer indirection level
    pub pointer_level: usize,
    /// Whether const qualified
    pub is_const: bool,
    /// Size in bytes (if known)
    pub size_bytes: Option<usize>,
}

impl TypeMetadata {
    /// Create new type metadata
    pub fn new(source: &str, target: &str, category: TypeCategory, is_signed: bool) -> Self {
        Self {
            source_type: source.to_string(),
            target_type: target.to_string(),
            category,
            is_signed,
            is_pointer: false,
            pointer_level: 0,
            is_const: false,
            size_bytes: None,
        }
    }

    /// Create type metadata with size
    pub fn with_size(source: &str, target: &str, category: TypeCategory, is_signed: bool, size: usize) -> Self {
        let mut meta = Self::new(source, target, category, is_signed);
        meta.size_bytes = Some(size);
        meta
    }

    /// Set pointer level and mark as pointer type
    pub fn as_pointer(mut self, level: usize) -> Self {
        self.is_pointer = true;
        self.pointer_level = level;
        self.category = TypeCategory::PointerType;
        self
    }

    /// Mark as const
    pub fn as_const(mut self) -> Self {
        self.is_const = true;
        self
    }

    /// Generate the appropriate target type with qualifiers
    pub fn to_target_type(&self) -> String {
        let mut result = self.target_type.clone();

        if self.is_pointer {
            for _ in 0..self.pointer_level {
                result = if self.is_const {
                    format!("*const {}", result)
                } else {
                    format!("*mut {}", result)
                };
            }
        }

        result
    }

    /// Convert to an Entry representation
    pub fn to_entry(&self) -> Entry {
        let mut node = Entry::node("TypeMetadata", &self.source_type);
        node.set_attr("target_type", Entry::string(&self.target_type));
        node.set_attr("category", Entry::string(self.category.as_str()));
        node.set_attr("is_signed", Entry::bool(self.is_signed));
        node.set_attr("is_pointer", Entry::bool(self.is_pointer));
        node.set_attr("pointer_level", Entry::usize(self.pointer_level));
        node.set_attr("is_const", Entry::bool(self.is_const));
        if let Some(size) = self.size_bytes {
            node.set_attr("size_bytes", Entry::usize(size));
        }
        node
    }
}

/// Generic type conversion engine
#[derive(Debug, Clone)]
pub struct TypeConverter {
    /// Type mappings (source -> metadata)
    type_map: HashMap<String, TypeMetadata>,
    /// Category index for quick lookups
    category_index: HashMap<TypeCategory, Vec<String>>,
    /// Cached conversions for performance
    conversion_cache: HashMap<String, String>,
    /// Type aliases
    type_aliases: HashMap<String, String>,
    /// Function pointer patterns for detection
    fn_ptr_patterns: Vec<String>,
}

impl TypeConverter {
    /// Create a new type converter
    pub fn new() -> Self {
        let mut converter = Self {
            type_map: HashMap::new(),
            category_index: HashMap::new(),
            conversion_cache: HashMap::new(),
            type_aliases: HashMap::new(),
            fn_ptr_patterns: Vec::new(),
        };
        converter.initialize_c_types();
        converter.initialize_fn_ptr_patterns();
        converter
    }

    /// Initialize standard C to Rust type mappings
    fn initialize_c_types(&mut self) {
        // Basic integer types
        self.add_type_sized("bool", "bool", TypeCategory::BasicInteger, false, 1);
        self.add_type_sized("char", "i8", TypeCategory::BasicInteger, true, 1);
        self.add_type_sized("short", "i16", TypeCategory::BasicInteger, true, 2);
        self.add_type_sized("int", "i32", TypeCategory::BasicInteger, true, 4);
        self.add_type_sized("long", "i64", TypeCategory::BasicInteger, true, 8);
        self.add_type_sized("long long", "i64", TypeCategory::BasicInteger, true, 8);

        // Unsigned integer types
        self.add_type_sized("unsigned char", "u8", TypeCategory::BasicInteger, false, 1);
        self.add_type_sized("unsigned short", "u16", TypeCategory::BasicInteger, false, 2);
        self.add_type_sized("unsigned int", "u32", TypeCategory::BasicInteger, false, 4);
        self.add_type_sized("unsigned long", "u64", TypeCategory::BasicInteger, false, 8);
        self.add_type_sized("unsigned long long", "u64", TypeCategory::BasicInteger, false, 8);

        // Floating point types
        self.add_type_sized("float", "f32", TypeCategory::BasicFloat, true, 4);
        self.add_type_sized("double", "f64", TypeCategory::BasicFloat, true, 8);
        self.add_type_sized("long double", "f64", TypeCategory::BasicFloat, true, 16);

        // Special types
        self.add_type_sized("void", "()", TypeCategory::BasicInteger, false, 0);

        // Size and system types
        self.add_type("size_t", "usize", TypeCategory::SizeType, false);
        self.add_type("ssize_t", "isize", TypeCategory::SizeType, true);
        self.add_type("ptrdiff_t", "isize", TypeCategory::SizeType, true);
        self.add_type("intptr_t", "isize", TypeCategory::SizeType, true);
        self.add_type("uintptr_t", "usize", TypeCategory::SizeType, false);

        // Fixed-width integer types
        self.add_type_sized("int8_t", "i8", TypeCategory::FixedWidthInteger, true, 1);
        self.add_type_sized("int16_t", "i16", TypeCategory::FixedWidthInteger, true, 2);
        self.add_type_sized("int32_t", "i32", TypeCategory::FixedWidthInteger, true, 4);
        self.add_type_sized("int64_t", "i64", TypeCategory::FixedWidthInteger, true, 8);
        self.add_type_sized("uint8_t", "u8", TypeCategory::FixedWidthInteger, false, 1);
        self.add_type_sized("uint16_t", "u16", TypeCategory::FixedWidthInteger, false, 2);
        self.add_type_sized("uint32_t", "u32", TypeCategory::FixedWidthInteger, false, 4);
        self.add_type_sized("uint64_t", "u64", TypeCategory::FixedWidthInteger, false, 8);

        // Storage qualifiers
        self.add_type("static", "static", TypeCategory::StorageQualifier, false);
        self.add_type("const", "const", TypeCategory::StorageQualifier, false);
        self.add_type("extern", "extern", TypeCategory::StorageQualifier, false);
        self.add_type("volatile", "volatile", TypeCategory::StorageQualifier, false);
        self.add_type("register", "", TypeCategory::StorageQualifier, false);
        self.add_type("auto", "", TypeCategory::StorageQualifier, false);
        self.add_type("inline", "inline", TypeCategory::StorageQualifier, false);
        self.add_type("restrict", "", TypeCategory::StorageQualifier, false);
    }

    /// Initialize function pointer patterns
    fn initialize_fn_ptr_patterns(&mut self) {
        self.fn_ptr_patterns.extend([
            r"\w+\s*\(\s*\*\s*\w+\s*\)\s*\(.*\)".to_string(),
            r"\w+\s+\(\s*\*\s*\)\s*\(.*\)".to_string(),
        ]);
    }

    /// Add a type mapping with size
    fn add_type_sized(&mut self, source: &str, target: &str, category: TypeCategory, is_signed: bool, size: usize) {
        let metadata = TypeMetadata::with_size(source, target, category, is_signed, size);
        self.add_type_metadata(source, metadata);
    }

    /// Add a basic type mapping
    fn add_type(&mut self, source: &str, target: &str, category: TypeCategory, is_signed: bool) {
        let metadata = TypeMetadata::new(source, target, category, is_signed);
        self.add_type_metadata(source, metadata);
    }

    /// Add a type mapping with metadata
    fn add_type_metadata(&mut self, source: &str, metadata: TypeMetadata) {
        let category = metadata.category.clone();
        self.type_map.insert(source.to_string(), metadata);
        self.category_index
            .entry(category)
            .or_default()
            .push(source.to_string());
    }

    /// Convert a source type to target type
    pub fn convert(&mut self, source_type: &str) -> Option<String> {
        let normalized = self.normalize_type(source_type);

        // Check cache first
        if let Some(cached) = self.conversion_cache.get(&normalized) {
            return Some(cached.clone());
        }

        let result = self.convert_internal(&normalized)?;

        // Cache the result
        self.conversion_cache.insert(normalized, result.clone());
        Some(result)
    }

    /// Internal conversion logic
    fn convert_internal(&self, source: &str) -> Option<String> {
        // Handle function pointers first
        if self.is_function_pointer(source) {
            return self.convert_function_pointer(source);
        }

        // Handle pointer types
        if source.contains('*') {
            return self.convert_pointer_type(source);
        }

        // Handle array types
        if source.contains('[') && source.contains(']') {
            return self.convert_array_type(source);
        }

        // Handle const qualifier
        if source.starts_with("const ") {
            let base_type = source.strip_prefix("const ")?.trim();
            return self.convert_internal(base_type);
        }

        // Check type aliases
        if let Some(alias) = self.type_aliases.get(source) {
            return Some(alias.clone());
        }

        // Standard type lookup
        self.type_map.get(source).map(|meta| meta.to_target_type())
    }

    /// Normalize type string
    fn normalize_type(&self, source: &str) -> String {
        let mut result = String::with_capacity(source.len());
        let mut chars = source.trim().chars().peekable();

        while let Some(ch) = chars.next() {
            match ch {
                ' ' | '\t' | '\n' | '\r' => {
                    if !result.is_empty() && !result.ends_with(' ') {
                        if chars.peek().map_or(true, |&next| next != '*' && next != '&') {
                            result.push(' ');
                        }
                    }
                }
                '*' | '&' => {
                    if result.ends_with(' ') {
                        result.pop();
                    }
                    result.push(ch);
                }
                _ => result.push(ch),
            }
        }

        result.trim_end().to_string()
    }

    /// Check if type is a function pointer
    fn is_function_pointer(&self, source: &str) -> bool {
        source.contains("(*") && source.contains(')')
    }

    /// Convert function pointer types
    fn convert_function_pointer(&self, source: &str) -> Option<String> {
        let mut parts = source.splitn(2, '(');
        let return_type = parts.next()?.trim();

        let remaining = parts.next()?.trim_end_matches(')');
        let params_start = remaining.find(')')?;
        let params = remaining[params_start + 1..]
            .trim_start_matches('(')
            .trim_end_matches(')');

        // Convert parameter types
        let param_types: Vec<String> = if params.is_empty() {
            Vec::new()
        } else {
            params
                .split(',')
                .map(|p| p.trim())
                .filter(|p| !p.is_empty())
                .map(|p| self.convert_internal(p).unwrap_or_else(|| p.to_string()))
                .collect()
        };

        let return_rust = self
            .convert_internal(return_type)
            .unwrap_or_else(|| return_type.to_string());

        Some(format!("fn({}) -> {}", param_types.join(", "), return_rust))
    }

    /// Convert pointer types
    fn convert_pointer_type(&self, source: &str) -> Option<String> {
        let star_count = source.matches('*').count();
        let base_part = source.trim_end_matches('*').trim();

        let (is_const, base_type) = if base_part.starts_with("const ") {
            (true, base_part.strip_prefix("const ")?.trim())
        } else {
            (false, base_part)
        };

        let rust_base = self.convert_internal(base_type)?;

        let mut result = rust_base;
        for _ in 0..star_count {
            result = if is_const {
                format!("*const {}", result)
            } else {
                format!("*mut {}", result)
            };
        }

        Some(result)
    }

    /// Convert array types
    fn convert_array_type(&self, source: &str) -> Option<String> {
        let bracket_start = source.find('[')?;
        let base_type = source[..bracket_start].trim();
        let array_part = &source[bracket_start..];

        let rust_base = self.convert_internal(base_type)?;
        let size = array_part.trim_start_matches('[').trim_end_matches(']');

        if size.is_empty() {
            Some(format!("&[{}]", rust_base))
        } else if size.parse::<usize>().is_ok() {
            Some(format!("[{}; {}]", rust_base, size))
        } else {
            Some(format!("Vec<{}>", rust_base))
        }
    }

    /// Get metadata for a type
    pub fn get_metadata(&self, source: &str) -> Option<&TypeMetadata> {
        self.type_map.get(source)
    }

    /// Check if a string is a known type
    pub fn is_known_type(&mut self, type_str: &str) -> bool {
        if self.type_map.contains_key(type_str) {
            return true;
        }

        if self.type_aliases.contains_key(type_str) {
            return true;
        }

        if type_str.contains('*') || type_str.contains('[') || self.is_function_pointer(type_str) {
            return self.convert(type_str).is_some();
        }

        false
    }

    /// Get all types in a category
    pub fn get_by_category(&self, category: &TypeCategory) -> Vec<&str> {
        self.category_index
            .get(category)
            .map(|types| types.iter().map(String::as_str).collect())
            .unwrap_or_default()
    }

    /// Add a custom type mapping
    pub fn add_custom_type(&mut self, source: String, target: String, category: TypeCategory, is_signed: bool) {
        self.add_type(&source, &target, category, is_signed);
    }

    /// Add a type alias
    pub fn add_alias(&mut self, alias: String, target: String) {
        self.type_aliases.insert(alias, target);
    }

    /// Clear conversion cache
    pub fn clear_cache(&mut self) {
        self.conversion_cache.clear();
    }

    /// Convert an Entry to target type (if it represents a type)
    pub fn convert_entry(&mut self, entry: &Entry) -> Option<String> {
        let type_str = entry.name()?;
        self.convert(type_str)
    }
}

impl Default for TypeConverter {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// Identifier Case Conversion
// ============================================================================

/// Target case types for identifier conversion
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IdentifierCase {
    /// snake_case for functions, variables, modules
    SnakeCase,
    /// PascalCase for types, enums, structs, traits
    PascalCase,
    /// SCREAMING_SNAKE_CASE for constants, static variables
    ScreamingSnakeCase,
    /// camelCase for maintaining original style when needed
    CamelCase,
}

/// Convert identifier to target case
pub fn convert_identifier(identifier: &str, target_case: IdentifierCase) -> String {
    if identifier.is_empty() {
        return String::new();
    }

    match target_case {
        IdentifierCase::SnakeCase => to_snake_case(identifier),
        IdentifierCase::PascalCase => to_pascal_case(identifier),
        IdentifierCase::ScreamingSnakeCase => to_screaming_snake_case(identifier),
        IdentifierCase::CamelCase => to_camel_case(identifier),
    }
}

/// Convert to snake_case
pub fn to_snake_case(s: &str) -> String {
    if s.is_empty() {
        return String::new();
    }

    let mut result = String::with_capacity(s.len() + s.len() / 2);
    let mut chars = s.chars().peekable();
    let mut prev_was_lowercase = false;

    while let Some(ch) = chars.next() {
        if ch.is_uppercase() {
            if !result.is_empty() && (prev_was_lowercase || chars.peek().map_or(false, |c| c.is_lowercase())) {
                result.push('_');
            }
            result.extend(ch.to_lowercase());
            prev_was_lowercase = false;
        } else if ch == '_' || ch.is_whitespace() {
            if !result.is_empty() && !result.ends_with('_') {
                result.push('_');
            }
            prev_was_lowercase = false;
        } else {
            result.push(ch);
            prev_was_lowercase = ch.is_lowercase();
        }
    }

    result
}

/// Convert to SCREAMING_SNAKE_CASE
pub fn to_screaming_snake_case(s: &str) -> String {
    to_snake_case(s).to_uppercase()
}

/// Convert to PascalCase
pub fn to_pascal_case(s: &str) -> String {
    if s.is_empty() {
        return String::new();
    }

    let mut result = String::with_capacity(s.len());
    let mut capitalize_next = true;

    for ch in s.chars() {
        if ch == '_' || ch.is_whitespace() {
            capitalize_next = true;
        } else if capitalize_next {
            result.extend(ch.to_uppercase());
            capitalize_next = false;
        } else {
            result.extend(ch.to_lowercase());
        }
    }

    result
}

/// Convert to camelCase
pub fn to_camel_case(s: &str) -> String {
    if s.is_empty() {
        return String::new();
    }

    let pascal = to_pascal_case(s);
    let mut chars = pascal.chars();

    match chars.next() {
        Some(first_char) => {
            let mut result = String::with_capacity(pascal.len());
            result.extend(first_char.to_lowercase());
            result.push_str(chars.as_str());
            result
        }
        None => pascal,
    }
}

/// Sanitize identifier for Rust keywords
pub fn sanitize_rust_identifier(identifier: &str) -> String {
    const RUST_KEYWORDS: &[&str] = &[
        "as", "break", "const", "continue", "crate", "else", "enum", "extern", "false", "fn",
        "for", "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut", "pub", "ref",
        "return", "self", "Self", "static", "struct", "super", "trait", "true", "type", "unsafe",
        "use", "where", "while", "async", "await", "dyn", "abstract", "become", "box", "do",
        "final", "macro", "override", "priv", "typeof", "unsized", "virtual", "yield",
    ];

    if identifier.is_empty() {
        return String::new();
    }

    if RUST_KEYWORDS.contains(&identifier) {
        format!("r#{}", identifier)
    } else {
        identifier.to_string()
    }
}

/// Check if a string is a valid C identifier
pub fn is_valid_c_identifier(s: &str) -> bool {
    if s.is_empty() {
        return false;
    }

    let mut chars = s.chars();

    match chars.next() {
        Some(first) => {
            if !first.is_ascii_alphabetic() && first != '_' {
                return false;
            }
        }
        None => return false,
    }

    chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
}

/// Check if a string is a C keyword
pub fn is_c_keyword(s: &str) -> bool {
    matches!(
        s,
        "if" | "else" | "while" | "for" | "do" | "switch" | "case" | "default"
            | "break" | "continue" | "return" | "goto"
            | "struct" | "union" | "enum" | "typedef"
            | "auto" | "register" | "static" | "extern"
            | "const" | "volatile" | "restrict"
            | "void" | "char" | "short" | "int" | "long" | "float" | "double"
            | "signed" | "unsigned"
            | "inline" | "sizeof"
            | "_Bool" | "_Complex" | "_Imaginary"
            | "_Alignas" | "_Alignof" | "_Atomic" | "_Generic" | "_Noreturn"
            | "_Static_assert" | "_Thread_local" | "typeof" | "typeof_unqual"
            | "_BitInt" | "_Decimal128" | "_Decimal32" | "_Decimal64"
    )
}

// ============================================================================
// Operator Utilities
// ============================================================================

const OPERATORS: &[&str] = &[
    "+", "-", "*", "/", "%", "=", "==", "!=", "<", ">", "<=", ">=", "+=", "-=", "*=", "/=", "%=",
    "&&", "||", "!", "&", "|", "^", "~", "<<", ">>", "&=", "|=", "^=", "<<=", ">>=",
];

const ASSIGNMENT_OPERATORS: &[&str] = &[
    "=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>=",
];

const BINARY_OPERATORS: &[&str] = &[
    "+", "-", "*", "/", "%", "&", "|", "^", "<<", ">>", "==", "!=", "<", ">", "<=", ">=", "&&", "||",
];

/// Check if a token string is an operator
pub fn is_operator(token: &str) -> bool {
    OPERATORS.contains(&token)
}

/// Check if a token string is an assignment operator
pub fn is_assignment_operator(token: &str) -> bool {
    ASSIGNMENT_OPERATORS.contains(&token)
}

/// Check if a token string is a binary operator
pub fn is_binary_operator(token: &str) -> bool {
    BINARY_OPERATORS.contains(&token)
}

/// Convert a C operator to its Rust equivalent
pub fn convert_operator(op: &str) -> &str {
    match op {
        "~" => "!",
        _ => op,
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_converter_basic() {
        let mut converter = TypeConverter::new();
        
        assert_eq!(converter.convert("int"), Some("i32".to_string()));
        assert_eq!(converter.convert("unsigned int"), Some("u32".to_string()));
        assert_eq!(converter.convert("float"), Some("f32".to_string()));
        assert_eq!(converter.convert("double"), Some("f64".to_string()));
        assert_eq!(converter.convert("void"), Some("()".to_string()));
    }

    #[test]
    fn test_type_converter_fixed_width() {
        let mut converter = TypeConverter::new();
        
        assert_eq!(converter.convert("int32_t"), Some("i32".to_string()));
        assert_eq!(converter.convert("uint64_t"), Some("u64".to_string()));
        assert_eq!(converter.convert("size_t"), Some("usize".to_string()));
    }

    #[test]
    fn test_type_converter_pointers() {
        let mut converter = TypeConverter::new();
        
        assert_eq!(converter.convert("int*"), Some("*mut i32".to_string()));
        assert_eq!(converter.convert("const int*"), Some("*const i32".to_string()));
    }

    #[test]
    fn test_type_converter_arrays() {
        let mut converter = TypeConverter::new();
        
        assert_eq!(converter.convert("int[10]"), Some("[i32; 10]".to_string()));
        assert_eq!(converter.convert("int[]"), Some("&[i32]".to_string()));
    }

    #[test]
    fn test_type_metadata() {
        let meta = TypeMetadata::with_size("int", "i32", TypeCategory::BasicInteger, true, 4);
        assert_eq!(meta.source_type, "int");
        assert_eq!(meta.target_type, "i32");
        assert!(meta.is_signed);
        assert_eq!(meta.size_bytes, Some(4));
    }

    #[test]
    fn test_snake_case() {
        assert_eq!(to_snake_case("MyFunction"), "my_function");
        assert_eq!(to_snake_case("myFunction"), "my_function");
        assert_eq!(to_snake_case("XMLParser"), "xml_parser");
        assert_eq!(to_snake_case("getHTTPResponse"), "get_http_response");
    }

    #[test]
    fn test_pascal_case() {
        assert_eq!(to_pascal_case("my_function"), "MyFunction");
        assert_eq!(to_pascal_case("my_struct"), "MyStruct");
        assert_eq!(to_pascal_case("xml_parser"), "XmlParser");
    }

    #[test]
    fn test_screaming_snake_case() {
        assert_eq!(to_screaming_snake_case("MyConstant"), "MY_CONSTANT");
        assert_eq!(to_screaming_snake_case("maxValue"), "MAX_VALUE");
    }

    #[test]
    fn test_camel_case() {
        assert_eq!(to_camel_case("my_function"), "myFunction");
        assert_eq!(to_camel_case("MY_CONSTANT"), "myConstant");
    }

    #[test]
    fn test_sanitize_rust_identifier() {
        assert_eq!(sanitize_rust_identifier("type"), "r#type");
        assert_eq!(sanitize_rust_identifier("match"), "r#match");
        assert_eq!(sanitize_rust_identifier("my_var"), "my_var");
    }

    #[test]
    fn test_is_valid_c_identifier() {
        assert!(is_valid_c_identifier("my_var"));
        assert!(is_valid_c_identifier("_private"));
        assert!(is_valid_c_identifier("var123"));
        assert!(!is_valid_c_identifier("123var"));
        assert!(!is_valid_c_identifier(""));
    }

    #[test]
    fn test_is_c_keyword() {
        assert!(is_c_keyword("if"));
        assert!(is_c_keyword("while"));
        assert!(is_c_keyword("struct"));
        assert!(!is_c_keyword("my_var"));
    }

    #[test]
    fn test_operators() {
        assert!(is_operator("+"));
        assert!(is_operator("=="));
        assert!(is_assignment_operator("="));
        assert!(is_assignment_operator("+="));
        assert!(is_binary_operator("&&"));
        assert!(!is_binary_operator("!"));
    }

    #[test]
    fn test_convert_operator() {
        assert_eq!(convert_operator("~"), "!");
        assert_eq!(convert_operator("+"), "+");
    }
}
