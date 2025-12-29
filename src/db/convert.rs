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

use crate::db::{Build, web::Entry};
use std::collections::HashMap;

// ============================================================================
// Type Conversion System
// ============================================================================

/// Category classification for types
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeCategory {
    Boolean,
    /// Basic integer types (int, char, short, long)
    Integer,
    /// Floating point types (float, double)
    Float,
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
    Struct,
    Union,
    Enum,
    /// Custom category
    Custom(String),
}

impl TypeCategory {
    pub fn as_str(&self) -> &str {
        match self {
            TypeCategory::Boolean => "boolean",
            TypeCategory::Integer => "integer",
            TypeCategory::Float => "float",
            TypeCategory::SizeType => "size",
            TypeCategory::SystemType => "system",
            TypeCategory::PointerType => "pointer",
            TypeCategory::StorageQualifier => "storage",
            TypeCategory::UserDefined => "type",
            TypeCategory::FunctionPointer => "function_pointer",
            TypeCategory::Struct => "struct",
            TypeCategory::Union => "union",
            TypeCategory::Enum => "enum",
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
    /// Whether this type is const qualified
    pub is_const: bool,
    /// Pointer indirection level
    pub pointer_level: usize,
    /// if this is an array specifies dimension count
    pub dimensions: usize,
    /// Size in bytes (if known)
    pub size_bytes: Option<usize>,
}

impl TypeMetadata {
    /// Create new type metadata
    pub fn new(source: &str, target: &str, category: TypeCategory) -> Self {
        Self {
            source_type: source.to_string(),
            target_type: target.to_string(),
            category,
            is_signed: false,
            is_const: false,
            pointer_level: 0,
            dimensions: 0,
            size_bytes: None,
        }
    }
    pub fn is_pointer(&self) -> bool {
        self.pointer_level > 0
    }
    pub fn is_array(&self) -> bool {
        self.dimensions > 0
    }
    /// Create type metadata with size
    pub fn with_size(
        source: &str,
        target: &str,
        category: TypeCategory,
        size: usize,
    ) -> Self {
        let mut meta = Self::new(source, target, category);
        meta.size_bytes = Some(size);
        meta
    }

    /// Set pointer level and mark as pointer type
    pub fn as_pointer(mut self, level: usize) -> Self {
        if level > 0 {
            self.pointer_level = level;
            self.category = TypeCategory::PointerType;
        }
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

        if self.is_pointer() {
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
}
impl Build for TypeMetadata {
    fn kind(&self) -> &str {
        "TypeMetadata"
    }

    fn name(&self) -> Option<&str> {
        Some(&self.source_type)
    }
    
    /// Convert to an Entry representation
    fn to_entry(&self) -> Entry {
        let mut node = Entry::node("TypeMetadata", &self.source_type);
        node.set_attr("target_type", Entry::string(&self.target_type));
        node.set_attr("category", Entry::string(self.category.as_str()));
        node.set_attr("is_signed", Entry::bool(self.is_signed));
        node.set_attr("is_const", Entry::bool(self.is_const));
        node.set_attr("is_ptr", Entry::bool(self.is_pointer()));
        node.set_attr("is_array", Entry::bool(self.is_array()));
        node.set_attr("pointer_level", Entry::usize(self.pointer_level));
        node.set_attr("dimensions", Entry::usize(self.dimensions));
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
        let sized_types = vec![
            // Boolean type
            ("bool", "bool", TypeCategory::Boolean, 1),
            // Basic integer types
            ("char", "i8", TypeCategory::Integer, 1),
            ("short", "i16", TypeCategory::Integer, 2),
            ("int", "i32", TypeCategory::Integer, 4),
            ("long", "i64", TypeCategory::Integer, 8),
            ("long long", "i64", TypeCategory::Integer, 8),
            // Unsigned integer types
            ("unsigned char", "u8", TypeCategory::Integer, 1),
            ("unsigned short", "u16", TypeCategory::Integer, 2),
            ("unsigned int", "u32", TypeCategory::Integer, 4),
            ("unsigned long", "u64", TypeCategory::Integer, 8),
            ("unsigned long long", "u64", TypeCategory::Integer, 8),
            // Floating point types
            ("float", "f32", TypeCategory::Float, 4),
            ("double", "f64", TypeCategory::Float, 8),
            ("long double", "f64", TypeCategory::Float, 16),
            // Special types
            ("void", "()", TypeCategory::Integer, 0),
            // Fixed-width integer types
            ("int8_t", "i8", TypeCategory::Integer, 1),
            ("int16_t", "i16", TypeCategory::Integer, 2),
            ("int32_t", "i32", TypeCategory::Integer, 4),
            ("int64_t", "i64", TypeCategory::Integer, 8),
            ("uint8_t", "u8", TypeCategory::Integer, 1),
            ("uint16_t", "u16", TypeCategory::Integer, 2),
            ("uint32_t", "u32", TypeCategory::Integer, 4),
            ("uint64_t", "u64", TypeCategory::Integer, 8),
        ];

        // Storage class specifiers and modifiers
        let storage_modifiers = vec!["", "static ", "extern ", "register ", "auto "];
        let cv_modifiers = vec!["", "const ", "volatile ", "const volatile "];

        for (source, target, category, size) in sized_types {
            // Base type with all storage and cv-qualifier combinations
            for storage in &storage_modifiers {
                for cv in &cv_modifiers {
                    let src = format!("{}{}{}", storage, cv, source);
                    let tgt = if cv.contains("const") {
                        format!("const {}", target)
                    } else {
                        target.to_string()
                    };
                    self.add_type_sized(&src.trim().to_string(), &tgt, category.clone(), size);
                }
            }
            
            // Pointer variations with all modifier combinations
            for storage in &storage_modifiers {
                for cv in &cv_modifiers {
                    // Single pointer
                    self.add_type_sized(
                        &format!("{}{}{}*", storage, cv, source).trim().to_string(),
                        &format!("*{} {}", if cv.contains("const") { "const" } else { "mut" }, target),
                        TypeCategory::PointerType, 8
                    );
                    self.add_type_sized(
                        &format!("{}{}{} *", storage, cv, source).trim().to_string(),
                        &format!("*{} {}", if cv.contains("const") { "const" } else { "mut" }, target),
                        TypeCategory::PointerType, 8
                    );
                    
                    // Double pointer
                    self.add_type_sized(
                        &format!("{}{}{}**", storage, cv, source).trim().to_string(),
                        &format!("*mut *{} {}", if cv.contains("const") { "const" } else { "mut" }, target),
                        TypeCategory::PointerType, 8
                    );
                    self.add_type_sized(
                        &format!("{}{}{} **", storage, cv, source).trim().to_string(),
                        &format!("*mut *{} {}", if cv.contains("const") { "const" } else { "mut" }, target),
                        TypeCategory::PointerType, 8
                    );
                }
            }
            
            // Pointer to const variations
            self.add_type_sized(&format!("{}* const", source), &format!("*mut {}", target), TypeCategory::PointerType, 8);
            self.add_type_sized(&format!("{} * const", source), &format!("*mut {}", target), TypeCategory::PointerType, 8);
            self.add_type_sized(&format!("static {}* const", source), &format!("*mut {}", target), TypeCategory::PointerType, 8);
            self.add_type_sized(&format!("extern {}* const", source), &format!("*mut {}", target), TypeCategory::PointerType, 8);
            
            // Const pointer to const
            self.add_type_sized(&format!("const {}* const", source), &format!("*const {}", target), TypeCategory::PointerType, 8);
            self.add_type_sized(&format!("const {} * const", source), &format!("*const {}", target), TypeCategory::PointerType, 8);
            self.add_type_sized(&format!("static const {}* const", source), &format!("*const {}", target), TypeCategory::PointerType, 8);
            self.add_type_sized(&format!("extern const {}* const", source), &format!("*const {}", target), TypeCategory::PointerType, 8);
            
            // Volatile pointer variations
            self.add_type_sized(&format!("volatile {}* const", source), &format!("*mut {}", target), TypeCategory::PointerType, 8);
            self.add_type_sized(&format!("const volatile {}*", source), &format!("*const {}", target), TypeCategory::PointerType, 8);
            self.add_type_sized(&format!("const volatile {} *", source), &format!("*const {}", target), TypeCategory::PointerType, 8);
            
            // Reference variations (C++ style, but useful for compatibility)
            for storage in &storage_modifiers {
                self.add_type_sized(&format!("{}{}&", storage, source).trim().to_string(), &format!("&mut {}", target), category.clone(), size);
                self.add_type_sized(&format!("{}{} &", storage, source).trim().to_string(), &format!("&mut {}", target), category.clone(), size);
                self.add_type_sized(&format!("{}const {}&", storage, source).trim().to_string(), &format!("&{}", target), category.clone(), size);
                self.add_type_sized(&format!("{}const {} &", storage, source).trim().to_string(), &format!("&{}", target), category.clone(), size);
            }
            
            // Restrict qualifier (C99)
            self.add_type_sized(&format!("{} * restrict", source), &format!("*mut {}", target), TypeCategory::PointerType, 8);
            self.add_type_sized(&format!("{}* restrict", source), &format!("*mut {}", target), TypeCategory::PointerType, 8);
            self.add_type_sized(&format!("const {} * restrict", source), &format!("*const {}", target), TypeCategory::PointerType, 8);
            self.add_type_sized(&format!("const {}* restrict", source), &format!("*const {}", target), TypeCategory::PointerType, 8);
            
            // _Atomic qualifier (C11)
            self.add_type_sized(&format!("_Atomic {}", source), target, category.clone(), size);
            self.add_type_sized(&format!("_Atomic({})", source), target, category.clone(), size);
            
            // Thread local storage
            self.add_type_sized(&format!("_Thread_local {}", source), target, category.clone(), size);
            self.add_type_sized(&format!("thread_local {}", source), target, category.clone(), size);
            
            // Inline with types (for inline variables in C++)
            self.add_type_sized(&format!("inline {}", source), target, category.clone(), size);
            self.add_type_sized(&format!("inline static {}", source), target, category.clone(), size);
            self.add_type_sized(&format!("static inline {}", source), target, category.clone(), size);
        }
        
        // Size and system types
        self.add_type("size_t", "usize", TypeCategory::SizeType);
        self.add_type("ssize_t", "isize", TypeCategory::SizeType);
        self.add_type("ptrdiff_t", "isize", TypeCategory::SizeType);
        self.add_type("intptr_t", "isize", TypeCategory::SizeType);
        self.add_type("uintptr_t", "usize", TypeCategory::SizeType);
        
        // Size type with all modifier combinations
        for (source, target) in [("size_t", "usize"), ("ssize_t", "isize"), ("ptrdiff_t", "isize"), ("intptr_t", "isize"), ("uintptr_t", "usize")] {
            for storage in &storage_modifiers {
                for cv in &cv_modifiers {
                    let src = format!("{}{}{}", storage, cv, source);
                    self.add_type(&src.trim().to_string(), target, TypeCategory::SizeType);
                }
            }
            
            // Pointer variations for size types
            for storage in &storage_modifiers {
                self.add_type(&format!("{}{}*", storage, source).trim().to_string(), &format!("*mut {}", target), TypeCategory::PointerType);
                self.add_type(&format!("{}{} *", storage, source).trim().to_string(), &format!("*mut {}", target), TypeCategory::PointerType);
                self.add_type(&format!("{}const {}*", storage, source).trim().to_string(), &format!("*const {}", target), TypeCategory::PointerType);
                self.add_type(&format!("{}const {} *", storage, source).trim().to_string(), &format!("*const {}", target), TypeCategory::PointerType);
                self.add_type(&format!("{}volatile {}*", storage, source).trim().to_string(), &format!("*mut {}", target), TypeCategory::PointerType);
                self.add_type(&format!("{}volatile {} *", storage, source).trim().to_string(), &format!("*mut {}", target), TypeCategory::PointerType);
            }
        }
        
        // Special string type mappings (idiomatic Rust conversions)
        // const char * -> &str
        self.add_type("const char *", "&str", TypeCategory::PointerType);
        self.add_type("const char*", "&str", TypeCategory::PointerType);
        self.add_type("char const *", "&str", TypeCategory::PointerType);
        self.add_type("char const*", "&str", TypeCategory::PointerType);
        // char * -> String
        self.add_type("char *", "String", TypeCategory::PointerType);
        self.add_type("char*", "String", TypeCategory::PointerType);
        // const char ** -> Vec<&str> or &[&str]
        self.add_type("const char **", "&[&str]", TypeCategory::PointerType);
        self.add_type("const char**", "&[&str]", TypeCategory::PointerType);
        self.add_type("char **", "Vec<String>", TypeCategory::PointerType);
        self.add_type("char**", "Vec<String>", TypeCategory::PointerType);
    }

    /// Initialize function pointer patterns
    fn initialize_fn_ptr_patterns(&mut self) {
        self.fn_ptr_patterns.extend([
            r"\w+\s*\(\s*\*\s*\w+\s*\)\s*\(.*\)".to_string(),
            r"\w+\s+\(\s*\*\s*\)\s*\(.*\)".to_string(),
        ]);
    }

    /// Add a type mapping with size
    fn add_type_sized(
        &mut self,
        source: &str,
        target: &str,
        category: TypeCategory,
        size: usize,
    ) {
        let metadata = TypeMetadata::with_size(source, target, category, size);
        self.add_type_metadata(source, metadata);
    }

    /// Add a basic type mapping
    fn add_type(&mut self, source: &str, target: &str, category: TypeCategory) {
        let metadata = TypeMetadata::new(source, target, category);
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
        if let Some(meta) = self.type_map.get(source) {
            return Some(meta.to_target_type());
        }

        // Check for registered user-defined types in System database
        if let Some(rust_name) = self.lookup_registered_type(source) {
            return Some(rust_name);
        }

        None
    }

    /// Look up a type in the System's registered types database
    fn lookup_registered_type(&self, source: &str) -> Option<String> {
        // Use system() to look up registered types
        crate::system::system().lookup_type(source)
    }

    /// Normalize type string
    fn normalize_type(&self, source: &str) -> String {
        let mut result = String::with_capacity(source.len());
        let mut chars = source.trim().chars().peekable();

        while let Some(ch) = chars.next() {
            match ch {
                ' ' | '\t' | '\n' | '\r' => {
                    if !result.is_empty() && !result.ends_with(' ') {
                        if chars
                            .peek()
                            .map_or(true, |&next| next != '*' && next != '&')
                        {
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

        let (is_const, base_type) = if base_part.starts_with("const ") && star_count > 0 {
            (true, base_part.strip_prefix("const ")?.trim())
        } else {
            (false, base_part)
        };

        // Special case: const char * -> &str (idiomatic Rust string slice)
        if is_const && (base_type == "char" || base_type == "signed char") && star_count == 1 {
            return Some("&str".to_string());
        }
        
        // Special case: char * -> String (owned string)
        if !is_const && base_type == "char" && star_count == 1 {
            return Some("String".to_string());
        }

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
    pub fn add_custom_type(
        &mut self,
        source: &str,
        target: &str,
        category: TypeCategory,
        is_signed: bool,
        is_const: bool,
        pointer_level: usize,
        dimensions: usize,
        size: usize,
    ) {
        self.add_type_metadata(&source,TypeMetadata {
        source_type: source.to_string(),
        target_type: target.to_string(),
        category,
        is_signed,
        is_const,
        pointer_level,
        dimensions,
        size_bytes: Some(size)
        });
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
    
    /// Get all type mappings as (c_name, rust_name) pairs
    pub fn get_all_mappings(&self) -> Vec<(&str, &str,&str)> {
        let type_map = self.type_map
            .iter()
            .map(|(k, v)| (k.as_str(), v.target_type.as_str(),v.category.as_str()))
            .collect();
        type_map
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
            if !result.is_empty()
                && (prev_was_lowercase || chars.peek().map_or(false, |c| c.is_lowercase()))
            {
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
        // Rust primitive types that conflict with C variable names
        "str", "bool", "char", "i8", "i16", "i32", "i64", "i128", "isize",
        "u8", "u16", "u32", "u64", "u128", "usize", "f32", "f64",
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
        "if" | "else"
            | "while"
            | "for"
            | "do"
            | "switch"
            | "case"
            | "default"
            | "break"
            | "continue"
            | "return"
            | "goto"
            | "struct"
            | "union"
            | "enum"
            | "typedef"
            | "auto"
            | "register"
            | "static"
            | "extern"
            | "const"
            | "volatile"
            | "restrict"
            | "void"
            | "char"
            | "short"
            | "int"
            | "long"
            | "float"
            | "double"
            | "signed"
            | "unsigned"
            | "inline"
            | "sizeof"
            | "_Bool"
            | "_Complex"
            | "_Imaginary"
            | "_Alignas"
            | "_Alignof"
            | "_Atomic"
            | "_Generic"
            | "_Noreturn"
            | "_Static_assert"
            | "_Thread_local"
            | "typeof"
            | "typeof_unqual"
            | "_BitInt"
            | "_Decimal128"
            | "_Decimal32"
            | "_Decimal64"
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
    "+", "-", "*", "/", "%", "&", "|", "^", "<<", ">>", "==", "!=", "<", ">", "<=", ">=", "&&",
    "||",
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
// Identifier Conversion System
// ============================================================================

/// Category classification for identifiers
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IdentifierCategory {
    /// Function names
    Function,
    /// Variable names
    Variable,
    /// Constant names
    Constant,
    /// Type/struct/enum names
    TypeName,
    /// Macro names
    Macro,
    /// Standard library function
    StdLibFunction,
    /// Custom category
    Custom(String),
}

impl IdentifierCategory {
    pub fn as_str(&self) -> &str {
        match self {
            IdentifierCategory::Function => "function",
            IdentifierCategory::Variable => "variable",
            IdentifierCategory::Constant => "constant",
            IdentifierCategory::TypeName => "type_name",
            IdentifierCategory::Macro => "macro",
            IdentifierCategory::StdLibFunction => "stdlib_function",
            IdentifierCategory::Custom(s) => s.as_str(),
        }
    }

    /// Get the target case for this category
    pub fn target_case(&self) -> IdentifierCase {
        match self {
            IdentifierCategory::Function => IdentifierCase::SnakeCase,
            IdentifierCategory::Variable => IdentifierCase::SnakeCase,
            IdentifierCategory::Constant => IdentifierCase::ScreamingSnakeCase,
            IdentifierCategory::TypeName => IdentifierCase::PascalCase,
            IdentifierCategory::Macro => IdentifierCase::SnakeCase, // Rust macros use snake_case!
            IdentifierCategory::StdLibFunction => IdentifierCase::SnakeCase,
            IdentifierCategory::Custom(_) => IdentifierCase::SnakeCase,
        }
    }
}

/// Metadata for identifier conversions
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IdentifierMetadata {
    /// Source identifier name
    pub source_name: String,
    /// Target identifier name
    pub target_name: String,
    /// Identifier category
    pub category: IdentifierCategory,
    /// Whether this requires special handling
    pub is_special: bool,
    /// Notes about the conversion
    pub notes: Option<String>,
}

impl IdentifierMetadata {
    /// Create new identifier metadata
    pub fn new(source: &str, target: &str, category: IdentifierCategory) -> Self {
        Self {
            source_name: source.to_string(),
            target_name: target.to_string(),
            category,
            is_special: false,
            notes: None,
        }
    }

    /// Mark as special (requires attention)
    pub fn as_special(mut self) -> Self {
        self.is_special = true;
        self
    }

    /// Add notes
    pub fn with_notes(mut self, notes: &str) -> Self {
        self.notes = Some(notes.to_string());
        self
    }

    /// Convert to an Entry representation
    pub fn to_entry(&self) -> Entry {
        let mut node = Entry::node("IdentifierMetadata", &self.source_name);
        node.set_attr("target_name", Entry::string(&self.target_name));
        node.set_attr("category", Entry::string(self.category.as_str()));
        node.set_attr("is_special", Entry::bool(self.is_special));
        if let Some(ref notes) = self.notes {
            node.set_attr("notes", Entry::string(notes));
        }
        node
    }
}

/// Identifier conversion engine
///
/// Converts C identifiers to Rust equivalents, handling:
/// - Standard library function name mappings (printf -> print!, etc.)
/// - Case conversion based on category (snake_case, PascalCase, etc.)
/// - Rust keyword sanitization
/// - Common C naming conventions
#[derive(Debug, Clone)]
pub struct IdentifierConverter {
    /// Identifier mappings (source -> metadata)
    id_map: HashMap<String, IdentifierMetadata>,
    /// Category index for quick lookups
    category_index: HashMap<IdentifierCategory, Vec<String>>,
    /// Cached conversions for performance
    conversion_cache: HashMap<String, String>,
    /// Identifier aliases
    aliases: HashMap<String, String>,
    /// Struct field names: struct_name -> Vec<field_name>
    struct_fields: HashMap<String, Vec<String>>,
}

impl IdentifierConverter {
    /// Create a new identifier converter
    pub fn new() -> Self {
        let mut converter = Self {
            id_map: HashMap::new(),
            category_index: HashMap::new(),
            conversion_cache: HashMap::new(),
            aliases: HashMap::new(),
            struct_fields: HashMap::new(),
        };
        converter.initialize_stdlib_mappings();
        converter.initialize_common_mappings();
        converter
    }

    /// Initialize standard library function mappings
    fn initialize_stdlib_mappings(&mut self) {
        // I/O functions
        self.add_identifier("printf", "print!", IdentifierCategory::StdLibFunction);
        self.add_identifier("fprintf", "write!", IdentifierCategory::StdLibFunction);
        self.add_identifier("sprintf", "format!", IdentifierCategory::StdLibFunction);
        self.add_identifier("snprintf", "format!", IdentifierCategory::StdLibFunction);
        self.add_identifier("puts", "println!", IdentifierCategory::StdLibFunction);
        self.add_identifier("putchar", "print!", IdentifierCategory::StdLibFunction);
        self.add_identifier(
            "getchar",
            "std::io::stdin().bytes().next()",
            IdentifierCategory::StdLibFunction,
        );
        self.add_identifier(
            "fgets",
            "std::io::stdin().read_line",
            IdentifierCategory::StdLibFunction,
        );

        // Memory functions
        self.add_identifier("malloc", "Box::new", IdentifierCategory::StdLibFunction);
        self.add_identifier("calloc", "vec![0; n]", IdentifierCategory::StdLibFunction);
        self.add_identifier("realloc", "Vec::resize", IdentifierCategory::StdLibFunction);
        self.add_identifier("free", "drop", IdentifierCategory::StdLibFunction);
        self.add_identifier(
            "memcpy",
            "copy_from_slice",
            IdentifierCategory::StdLibFunction,
        );
        self.add_identifier("memmove", "copy_within", IdentifierCategory::StdLibFunction);
        self.add_identifier("memset", "fill", IdentifierCategory::StdLibFunction);
        self.add_identifier("memcmp", "eq", IdentifierCategory::StdLibFunction);

        // String functions
        self.add_identifier("strlen", "len", IdentifierCategory::StdLibFunction);
        self.add_identifier("strcpy", "clone", IdentifierCategory::StdLibFunction);
        self.add_identifier("strncpy", "clone", IdentifierCategory::StdLibFunction);
        self.add_identifier("strcat", "push_str", IdentifierCategory::StdLibFunction);
        self.add_identifier("strcmp", "cmp", IdentifierCategory::StdLibFunction);
        self.add_identifier("strncmp", "starts_with", IdentifierCategory::StdLibFunction);
        self.add_identifier("strchr", "find", IdentifierCategory::StdLibFunction);
        self.add_identifier("strstr", "contains", IdentifierCategory::StdLibFunction);
        self.add_identifier("strtok", "split", IdentifierCategory::StdLibFunction);

        // Math functions
        self.add_identifier("abs", "abs", IdentifierCategory::StdLibFunction);
        self.add_identifier("fabs", "abs", IdentifierCategory::StdLibFunction);
        self.add_identifier("sqrt", "sqrt", IdentifierCategory::StdLibFunction);
        self.add_identifier("pow", "powf", IdentifierCategory::StdLibFunction);
        self.add_identifier("floor", "floor", IdentifierCategory::StdLibFunction);
        self.add_identifier("ceil", "ceil", IdentifierCategory::StdLibFunction);
        self.add_identifier("round", "round", IdentifierCategory::StdLibFunction);
        self.add_identifier("sin", "sin", IdentifierCategory::StdLibFunction);
        self.add_identifier("cos", "cos", IdentifierCategory::StdLibFunction);
        self.add_identifier("tan", "tan", IdentifierCategory::StdLibFunction);
        self.add_identifier("log", "ln", IdentifierCategory::StdLibFunction);
        self.add_identifier("log10", "log10", IdentifierCategory::StdLibFunction);
        self.add_identifier("exp", "exp", IdentifierCategory::StdLibFunction);

        // Utility functions
        self.add_identifier(
            "sizeof",
            "std::mem::size_of",
            IdentifierCategory::StdLibFunction,
        );
        self.add_identifier(
            "offsetof",
            "std::mem::offset_of!",
            IdentifierCategory::StdLibFunction,
        );
        self.add_identifier(
            "exit",
            "std::process::exit",
            IdentifierCategory::StdLibFunction,
        );
        self.add_identifier(
            "abort",
            "std::process::abort",
            IdentifierCategory::StdLibFunction,
        );
        self.add_identifier("assert", "assert!", IdentifierCategory::StdLibFunction);
        self.add_identifier("atoi", "parse::<i32>", IdentifierCategory::StdLibFunction);
        self.add_identifier("atof", "parse::<f64>", IdentifierCategory::StdLibFunction);
        self.add_identifier("atol", "parse::<i64>", IdentifierCategory::StdLibFunction);
    }

    /// Initialize common identifier mappings
    fn initialize_common_mappings(&mut self) {
        // Common constants
        self.add_identifier("NULL", "std::ptr::null()", IdentifierCategory::Constant);
        self.add_identifier("TRUE", "true", IdentifierCategory::Constant);
        self.add_identifier("FALSE", "false", IdentifierCategory::Constant);
        self.add_identifier("EOF", "None", IdentifierCategory::Constant);

        // Common macros
        self.add_identifier("MIN", "min", IdentifierCategory::Macro);
        self.add_identifier("MAX", "max", IdentifierCategory::Macro);
        self.add_identifier("ABS", "abs", IdentifierCategory::Macro);

        // File handles
        self.add_identifier("stdin", "std::io::stdin()", IdentifierCategory::Variable);
        self.add_identifier("stdout", "std::io::stdout()", IdentifierCategory::Variable);
        self.add_identifier("stderr", "std::io::stderr()", IdentifierCategory::Variable);
    }

    /// Add an identifier mapping
    fn add_identifier(&mut self, source: &str, target: &str, category: IdentifierCategory) {
        let metadata = IdentifierMetadata::new(source, target, category.clone());

        self.category_index
            .entry(category)
            .or_default()
            .push(source.to_string());

        self.id_map.insert(source.to_string(), metadata);
    }

    /// Convert an identifier to its Rust equivalent
    pub fn convert(&mut self, source: &str) -> Option<String> {
        // Check cache first
        if let Some(cached) = self.conversion_cache.get(source) {
            return Some(cached.clone());
        }

        let result = self.convert_internal(source)?;

        // Cache the result
        self.conversion_cache
            .insert(source.to_string(), result.clone());

        Some(result)
    }

    /// Internal conversion logic
    fn convert_internal(&self, source: &str) -> Option<String> {
        // Check for direct mapping
        if let Some(metadata) = self.id_map.get(source) {
            return Some(metadata.target_name.clone());
        }

        // Check aliases
        if let Some(alias) = self.aliases.get(source) {
            return Some(alias.clone());
        }

        // Check System database for registered functions
        if let Some(rust_name) = self.lookup_registered_function(source) {
            return Some(rust_name);
        }

        // No direct mapping - just sanitize for Rust keywords
        Some(sanitize_rust_identifier(source))
    }

    /// Look up a function in the System's registered functions database
    fn lookup_registered_function(&self, source: &str) -> Option<String> {
        crate::system::system().lookup_function(source)
    }

    /// Convert with category-appropriate case conversion
    pub fn convert_with_case(&mut self, source: &str, category: IdentifierCategory) -> String {
        // Check for direct mapping first
        if let Some(metadata) = self.id_map.get(source) {
            return metadata.target_name.clone();
        }

        // Apply case conversion based on category
        let case = category.target_case();
        let converted = convert_identifier(source, case);

        // Sanitize for Rust keywords
        sanitize_rust_identifier(&converted)
    }

    /// Get metadata for an identifier
    pub fn get_metadata(&self, source: &str) -> Option<&IdentifierMetadata> {
        self.id_map.get(source)
    }

    /// Check if an identifier has a known mapping
    pub fn has_mapping(&self, source: &str) -> bool {
        self.id_map.contains_key(source)
    }

    /// Get all identifiers in a category
    pub fn by_category(&self, category: &IdentifierCategory) -> Option<&Vec<String>> {
        self.category_index.get(category)
    }

    /// Add a custom identifier mapping
    pub fn add_custom(&mut self, source: String, target: String, category: IdentifierCategory) {
        self.add_identifier(&source, &target, category);
    }

    /// Add an alias
    pub fn add_alias(&mut self, alias: String, target: String) {
        self.aliases.insert(alias, target);
    }

    /// Clear the conversion cache
    pub fn clear_cache(&mut self) {
        self.conversion_cache.clear();
    }

    /// Check if source is a stdlib function
    pub fn is_stdlib_function(&self, source: &str) -> bool {
        self.id_map
            .get(source)
            .map(|m| m.category == IdentifierCategory::StdLibFunction)
            .unwrap_or(false)
    }

    /// Register struct field names
    pub fn register_struct_fields(&mut self, struct_name: &str, fields: Vec<String>) {
        self.struct_fields.insert(struct_name.to_string(), fields);
    }

    /// Get struct field names
    pub fn get_struct_fields(&self, struct_name: &str) -> Option<&Vec<String>> {
        self.struct_fields.get(struct_name)
    }

    /// Check if a struct is registered
    pub fn has_struct(&self, struct_name: &str) -> bool {
        self.struct_fields.contains_key(struct_name)
    }
    
    /// Get all identifier mappings as (c_name, rust_name, category) tuples
    pub fn get_all_mappings(&self) -> Vec<(&str, &str, &IdentifierCategory)> {
        self.id_map
            .iter()
            .map(|(k, v)| (k.as_str(), v.target_name.as_str(), &v.category))
            .collect()
    }
}

impl Default for IdentifierConverter {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// Variable Conversion System
// ============================================================================

/// Metadata for variable registrations
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VariableMetadata {
    /// Variable name
    pub name: String,
    /// C type of the variable
    pub c_type: String,
    /// Rust type of the variable
    pub rust_type: String,
    /// Whether this is a pointer
    pub is_pointer: bool,
    /// Pointer indirection level
    pub pointer_level: usize,
    /// Whether this is an array
    pub is_array: bool,
    /// Array dimensions (if applicable)
    pub array_dims: Vec<usize>,
    /// Whether this is const
    pub is_const: bool,
    /// Whether this is static
    pub is_static: bool,
    /// Scope level (0 = global, higher = nested)
    pub scope_level: usize,
    /// Size in bytes (if known)
    pub size_bytes: Option<usize>,
}

impl VariableMetadata {
    /// Create new variable metadata
    pub fn new(name: &str, c_type: &str, rust_type: &str) -> Self {
        Self {
            name: name.to_string(),
            c_type: c_type.to_string(),
            rust_type: rust_type.to_string(),
            is_pointer: false,
            pointer_level: 0,
            is_array: false,
            array_dims: Vec::new(),
            is_const: false,
            is_static: false,
            scope_level: 0,
            size_bytes: None,
        }
    }

    /// Mark as pointer with level
    pub fn as_pointer(mut self, level: usize) -> Self {
        self.is_pointer = level > 0;
        self.pointer_level = level;
        self
    }

    /// Mark as array with dimensions
    pub fn as_array(mut self, dims: Vec<usize>) -> Self {
        self.is_array = !dims.is_empty();
        self.array_dims = dims;
        self
    }

    /// Mark as const
    pub fn as_const(mut self) -> Self {
        self.is_const = true;
        self
    }

    /// Mark as static
    pub fn as_static(mut self) -> Self {
        self.is_static = true;
        self
    }

    /// Set scope level
    pub fn with_scope(mut self, level: usize) -> Self {
        self.scope_level = level;
        self
    }

    /// Set size in bytes
    pub fn with_size(mut self, size: usize) -> Self {
        self.size_bytes = Some(size);
        self
    }

    /// Get the base type (without pointer/array modifiers)
    pub fn base_type(&self) -> &str {
        &self.rust_type
    }

    /// Convert to an Entry representation
    pub fn to_entry(&self) -> Entry {
        let mut node = Entry::node("Variable", &self.name);
        node.set_attr("c_type", Entry::string(&self.c_type));
        node.set_attr("rust_type", Entry::string(&self.rust_type));
        node.set_attr("is_pointer", Entry::bool(self.is_pointer));
        node.set_attr("pointer_level", Entry::usize(self.pointer_level));
        node.set_attr("is_array", Entry::bool(self.is_array));
        node.set_attr("is_const", Entry::bool(self.is_const));
        node.set_attr("is_static", Entry::bool(self.is_static));
        node.set_attr("scope_level", Entry::usize(self.scope_level));
        if let Some(size) = self.size_bytes {
            node.set_attr("size_bytes", Entry::usize(size));
        }
        if !self.array_dims.is_empty() {
            let dims: Vec<Entry> = self.array_dims.iter().map(|&d| Entry::usize(d)).collect();
            node.set_attr("array_dims", Entry::vec(dims));
        }
        node
    }
}

/// Variable conversion engine
///
/// Tracks declared variables and their types for:
/// - sizeof() conversions (needs to know type of variable)
/// - Type inference in expressions
/// - Pointer/array access validation
#[derive(Debug, Clone)]
pub struct VariableConverter {
    /// Variable mappings (name -> metadata)
    var_map: HashMap<String, VariableMetadata>,
    /// Scope stack for nested scopes (function bodies, blocks)
    scope_stack: Vec<HashMap<String, VariableMetadata>>,
    /// Current scope level
    current_scope: usize,
    /// Cached lookups for performance
    lookup_cache: HashMap<String, String>,
}

impl VariableConverter {
    /// Create a new variable converter
    pub fn new() -> Self {
        Self {
            var_map: HashMap::new(),
            scope_stack: vec![HashMap::new()], // Global scope
            current_scope: 0,
            lookup_cache: HashMap::new(),
        }
    }

    /// Register a variable with its type information
    pub fn register(&mut self, name: &str, c_type: &str, rust_type: &str) {
        let metadata = VariableMetadata::new(name, c_type, rust_type)
            .with_scope(self.current_scope);
        self.register_metadata(name, metadata);
    }

    /// Register a variable with full metadata
    pub fn register_metadata(&mut self, name: &str, metadata: VariableMetadata) {
        // Add to current scope
        if let Some(scope) = self.scope_stack.last_mut() {
            scope.insert(name.to_string(), metadata.clone());
        }
        // Also add to global map for quick lookup
        self.var_map.insert(name.to_string(), metadata);
        // Invalidate cache
        self.lookup_cache.remove(name);
    }

    /// Register a pointer variable
    pub fn register_pointer(&mut self, name: &str, base_c_type: &str, base_rust_type: &str, level: usize) {
        let rust_ptr = if level == 1 {
            format!("*mut {}", base_rust_type)
        } else {
            let mut result = base_rust_type.to_string();
            for _ in 0..level {
                result = format!("*mut {}", result);
            }
            result
        };
        let metadata = VariableMetadata::new(name, &format!("{}*", base_c_type), &rust_ptr)
            .as_pointer(level)
            .with_scope(self.current_scope);
        self.register_metadata(name, metadata);
    }

    /// Register an array variable
    pub fn register_array(&mut self, name: &str, element_c_type: &str, element_rust_type: &str, dims: Vec<usize>) {
        let rust_type = if dims.len() == 1 {
            format!("[{}; {}]", element_rust_type, dims[0])
        } else {
            // Multi-dimensional array
            let mut result = element_rust_type.to_string();
            for &dim in dims.iter().rev() {
                result = format!("[{}; {}]", result, dim);
            }
            result
        };
        let metadata = VariableMetadata::new(name, &format!("{}[]", element_c_type), &rust_type)
            .as_array(dims)
            .with_scope(self.current_scope);
        self.register_metadata(name, metadata);
    }

    /// Look up a variable's Rust type by name
    pub fn lookup(&self, name: &str) -> Option<String> {
        // Check cache first
        if let Some(cached) = self.lookup_cache.get(name) {
            return Some(cached.clone());
        }

        // Search from innermost scope to outermost
        for scope in self.scope_stack.iter().rev() {
            if let Some(meta) = scope.get(name) {
                return Some(meta.rust_type.clone());
            }
        }

        // Fall back to global map
        self.var_map.get(name).map(|m| m.rust_type.clone())
    }

    /// Look up a variable's C type by name
    pub fn lookup_c_type(&self, name: &str) -> Option<String> {
        // Search from innermost scope to outermost
        for scope in self.scope_stack.iter().rev() {
            if let Some(meta) = scope.get(name) {
                return Some(meta.c_type.clone());
            }
        }
        self.var_map.get(name).map(|m| m.c_type.clone())
    }

    /// Get full metadata for a variable
    pub fn get_metadata(&self, name: &str) -> Option<&VariableMetadata> {
        // Search from innermost scope to outermost
        for scope in self.scope_stack.iter().rev() {
            if let Some(meta) = scope.get(name) {
                return Some(meta);
            }
        }
        self.var_map.get(name)
    }

    /// Check if a variable is registered
    pub fn is_registered(&self, name: &str) -> bool {
        self.lookup(name).is_some()
    }

    /// Enter a new scope (e.g., function body, block)
    pub fn push_scope(&mut self) {
        self.current_scope += 1;
        self.scope_stack.push(HashMap::new());
    }

    /// Exit current scope
    pub fn pop_scope(&mut self) {
        if self.current_scope > 0 {
            if let Some(popped) = self.scope_stack.pop() {
                // Remove variables from global map that were in this scope
                for name in popped.keys() {
                    self.var_map.remove(name);
                    self.lookup_cache.remove(name);
                }
            }
            self.current_scope -= 1;
        }
    }

    /// Get current scope level
    pub fn scope_level(&self) -> usize {
        self.current_scope
    }

    /// Clear all variables (reset state)
    pub fn clear(&mut self) {
        self.var_map.clear();
        self.scope_stack.clear();
        self.scope_stack.push(HashMap::new());
        self.current_scope = 0;
        self.lookup_cache.clear();
    }

    /// Get all registered variables as (name, rust_type) pairs
    pub fn get_all_mappings(&self) -> Vec<(&str, &str, &str)> {
        self.var_map
            .iter()
            .map(|(k, v)| (k.as_str(), v.rust_type.as_str(), v.c_type.as_str()))
            .collect()
    }

    /// Get all variables in current scope
    pub fn get_current_scope_vars(&self) -> Vec<&str> {
        self.scope_stack
            .last()
            .map(|scope| scope.keys().map(|k| k.as_str()).collect())
            .unwrap_or_default()
    }

    /// Get the size of a variable's type in bytes (if known)
    pub fn get_size(&self, name: &str) -> Option<usize> {
        self.get_metadata(name).and_then(|m| m.size_bytes)
    }

    /// Estimate size based on Rust type (for sizeof conversions)
    pub fn estimate_size_for_type(rust_type: &str) -> Option<usize> {
        match rust_type {
            "i8" | "u8" | "bool" => Some(1),
            "i16" | "u16" => Some(2),
            "i32" | "u32" | "f32" => Some(4),
            "i64" | "u64" | "f64" | "isize" | "usize" => Some(8),
            "i128" | "u128" => Some(16),
            "()" => Some(0),
            _ if rust_type.starts_with("*") => Some(8), // Pointers are 8 bytes on 64-bit
            _ => None,
        }
    }
}

impl Default for VariableConverter {
    fn default() -> Self {
        Self::new()
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
        assert_eq!(
            converter.convert("const int*"),
            Some("*const i32".to_string())
        );
    }

    #[test]
    fn test_type_converter_arrays() {
        let mut converter = TypeConverter::new();

        assert_eq!(converter.convert("int[10]"), Some("[i32; 10]".to_string()));
        assert_eq!(converter.convert("int[]"), Some("&[i32]".to_string()));
    }

    #[test]
    fn test_type_metadata() {
        let meta = TypeMetadata::with_size("int", "i32", TypeCategory::Integer, 4);
        assert_eq!(meta.source_type, "int");
        assert_eq!(meta.target_type, "i32");
        // Note: is_signed is not set by with_size(), defaults to false
        assert!(!meta.is_signed);
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

    #[test]
    fn test_identifier_converter_stdlib() {
        let mut converter = IdentifierConverter::new();

        assert_eq!(converter.convert("printf"), Some("print!".to_string()));
        assert_eq!(converter.convert("malloc"), Some("Box::new".to_string()));
        assert_eq!(converter.convert("strlen"), Some("len".to_string()));
        assert_eq!(
            converter.convert("sizeof"),
            Some("std::mem::size_of".to_string())
        );
    }

    #[test]
    fn test_identifier_converter_constants() {
        let mut converter = IdentifierConverter::new();

        assert_eq!(
            converter.convert("NULL"),
            Some("std::ptr::null()".to_string())
        );
        assert_eq!(converter.convert("TRUE"), Some("true".to_string()));
        assert_eq!(converter.convert("FALSE"), Some("false".to_string()));
    }

    #[test]
    fn test_identifier_converter_keywords() {
        let mut converter = IdentifierConverter::new();

        // Rust keywords should be prefixed with r#
        assert_eq!(converter.convert("type"), Some("r#type".to_string()));
        assert_eq!(converter.convert("match"), Some("r#match".to_string()));

        // Non-keywords pass through
        assert_eq!(converter.convert("my_var"), Some("my_var".to_string()));
    }

    #[test]
    fn test_identifier_converter_with_case() {
        let mut converter = IdentifierConverter::new();

        // Function names should be snake_case
        assert_eq!(
            converter.convert_with_case("MyFunction", IdentifierCategory::Function),
            "my_function"
        );

        // Type names should be PascalCase
        assert_eq!(
            converter.convert_with_case("my_struct", IdentifierCategory::TypeName),
            "MyStruct"
        );

        // Constants should be SCREAMING_SNAKE_CASE
        assert_eq!(
            converter.convert_with_case("maxValue", IdentifierCategory::Constant),
            "MAX_VALUE"
        );
    }

    #[test]
    fn test_identifier_converter_is_stdlib() {
        let converter = IdentifierConverter::new();

        assert!(converter.is_stdlib_function("printf"));
        assert!(converter.is_stdlib_function("malloc"));
        assert!(!converter.is_stdlib_function("my_function"));
    }
}
