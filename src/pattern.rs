use crate::common::is_c_keyword;
use crate::entry::Entry;
use crate::error::{C2RError, Kind, Reason, Result};
use crate::json::Value;
use crate::lock::Id;
// Adaptivizer functionality moved to handler::Handlizer
use crate::Context;
use crate::Token;
use crate::{debug, info};
use crate::{error, object, report, warn};
use core::clone::Clone;
use core::option::Option::{self, None, Some};
use core::result::Result::Ok;
use std::collections::HashMap;
use std::fmt::Display;
use std::hash::{DefaultHasher, Hash, Hasher};
use std::ops::Range;
use std::sync::LazyLock;
use std::time::{Duration, Instant};

// ===== TYPE CONVERSION SYSTEM WITH METADATA =====

/// Category classification for C types
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypeCategory {
    BasicInteger,
    BasicFloat,
    FixedWidthInteger,
    SizeType,
    SystemType,
    PointerType,
    StorageQualifier,
}

/// Metadata for type conversions with scoring weights
#[derive(Debug, Clone)]
pub struct TypeMetadata {
    pub c_type: String,
    pub rust_type: String,
    pub category: TypeCategory,
    pub score_weight: f64,
    pub is_signed: bool,
    pub is_pointer: bool,
}

impl TypeMetadata {
    pub fn new(
        c_type: &str,
        rust_type: &str,
        category: TypeCategory,
        score_weight: f64,
        is_signed: bool,
    ) -> Self {
        Self {
            c_type: c_type.to_string(),
            rust_type: rust_type.to_string(),
            category,
            score_weight,
            is_signed,
            is_pointer: false,
        }
    }

    pub fn pointer(mut self) -> Self {
        self.is_pointer = true;
        self
    }
}

/// Enhanced type conversion map with metadata and scoring
pub struct TypeConversionMap {
    /// C type string -> Rust type string
    pub type_map: HashMap<String, String>,
    /// C type string -> TypeMetadata
    pub metadata_map: HashMap<String, TypeMetadata>,
    /// Category -> list of C types in that category
    pub category_index: HashMap<TypeCategory, Vec<String>>,
}

impl TypeConversionMap {
    /// Check if a string is a known C type
    pub fn is_c_type(&self, type_str: &str) -> bool {
        self.type_map.contains_key(type_str)
    }

    /// Convert a C type to its Rust equivalent
    pub fn convert_type(&self, c_type: &str) -> Option<String> {
        self.type_map.get(c_type).cloned()
    }

    /// Get metadata for a C type
    pub fn get_metadata(&self, c_type: &str) -> Option<&TypeMetadata> {
        self.metadata_map.get(c_type)
    }

    /// Get all C types in a specific category
    pub fn get_types_by_category(&self, category: TypeCategory) -> Vec<String> {
        self.category_index
            .get(&category)
            .cloned()
            .unwrap_or_default()
    }

    /// Calculate type score for tokens (for pattern matching)
    pub fn calculate_type_score(&self, tokens: &[Token]) -> f64 {
        let mut score = 0.0;
        for token in tokens {
            let token_str = token.to_string();
            if let Some(metadata) = self.get_metadata(&token_str) {
                score += metadata.score_weight;
            }
        }
        score.clamp(0.0, 1.0)
    }

    /// Check if tokens contain storage qualifiers
    pub fn has_storage_qualifier(&self, tokens: &[Token]) -> bool {
        tokens.iter().any(|token| {
            let token_str = token.to_string();
            self.get_metadata(&token_str)
                .map(|m| m.category == TypeCategory::StorageQualifier)
                .unwrap_or(false)
        })
    }
}

/// Global type conversion map (lazy initialized)
pub static TYPE_CONVERSION_MAP: LazyLock<TypeConversionMap> = LazyLock::new(|| {
    let mut type_map = HashMap::new();
    let mut metadata_map = HashMap::new();
    let mut category_index: HashMap<TypeCategory, Vec<String>> = HashMap::new();

    // Helper macro to add type with metadata
    macro_rules! add_type {
        ($c_type:expr, $rust_type:expr, $category:expr, $weight:expr, $signed:expr) => {
            type_map.insert($c_type.to_string(), $rust_type.to_string());
            let metadata = TypeMetadata::new($c_type, $rust_type, $category, $weight, $signed);
            metadata_map.insert($c_type.to_string(), metadata);
            category_index
                .entry($category)
                .or_insert_with(Vec::new)
                .push($c_type.to_string());
        };
    }

    // Basic C types (high scoring weight)
    add_type!("bool", "bool", TypeCategory::BasicInteger, 0.35, false);
    add_type!("_Bool", "bool", TypeCategory::BasicInteger, 0.35, false);
    add_type!("char", "i8", TypeCategory::BasicInteger, 0.35, true);
    add_type!("short", "i16", TypeCategory::BasicInteger, 0.35, true);
    add_type!("int", "i32", TypeCategory::BasicInteger, 0.35, true);
    add_type!("long", "i64", TypeCategory::BasicInteger, 0.35, true);
    add_type!("long long", "i64", TypeCategory::BasicInteger, 0.35, true);
    add_type!("float", "f32", TypeCategory::BasicFloat, 0.35, true);
    add_type!("double", "f64", TypeCategory::BasicFloat, 0.35, true);
    add_type!("void", "()", TypeCategory::BasicInteger, 0.30, false);

    // Unsigned types
    add_type!("unsigned char", "u8", TypeCategory::BasicInteger, 0.35, false);
    add_type!("unsigned short", "u16", TypeCategory::BasicInteger, 0.35, false);
    add_type!("unsigned int", "u32", TypeCategory::BasicInteger, 0.35, false);
    add_type!("unsigned long", "u64", TypeCategory::BasicInteger, 0.35, false);
    add_type!("unsigned long long", "u64", TypeCategory::BasicInteger, 0.35, false);

    // Size types (medium-high scoring weight)
    add_type!("size_t", "usize", TypeCategory::SizeType, 0.35, false);
    add_type!("ssize_t", "isize", TypeCategory::SizeType, 0.35, true);
    add_type!("ptrdiff_t", "isize", TypeCategory::SizeType, 0.35, true);
    add_type!("intptr_t", "isize", TypeCategory::SizeType, 0.35, true);
    add_type!("uintptr_t", "usize", TypeCategory::SizeType, 0.35, false);

    // Fixed-width integer types (medium-high scoring weight)
    add_type!("int8_t", "i8", TypeCategory::FixedWidthInteger, 0.35, true);
    add_type!("int16_t", "i16", TypeCategory::FixedWidthInteger, 0.35, true);
    add_type!("int32_t", "i32", TypeCategory::FixedWidthInteger, 0.35, true);
    add_type!("int64_t", "i64", TypeCategory::FixedWidthInteger, 0.35, true);
    add_type!("uint8_t", "u8", TypeCategory::FixedWidthInteger, 0.35, false);
    add_type!("uint16_t", "u16", TypeCategory::FixedWidthInteger, 0.35, false);
    add_type!("uint32_t", "u32", TypeCategory::FixedWidthInteger, 0.35, false);
    add_type!("uint64_t", "u64", TypeCategory::FixedWidthInteger, 0.35, false);
    add_type!("intmax_t", "i64", TypeCategory::FixedWidthInteger, 0.35, true);
    add_type!("uintmax_t", "u64", TypeCategory::FixedWidthInteger, 0.35, false);

    // Fast integer types
    add_type!("int_fast8_t", "i8", TypeCategory::FixedWidthInteger, 0.35, true);
    add_type!("int_fast16_t", "i16", TypeCategory::FixedWidthInteger, 0.35, true);
    add_type!("int_fast32_t", "i32", TypeCategory::FixedWidthInteger, 0.35, true);
    add_type!("int_fast64_t", "i64", TypeCategory::FixedWidthInteger, 0.35, true);
    add_type!("uint_fast8_t", "u8", TypeCategory::FixedWidthInteger, 0.35, false);
    add_type!("uint_fast16_t", "u16", TypeCategory::FixedWidthInteger, 0.35, false);
    add_type!("uint_fast32_t", "u32", TypeCategory::FixedWidthInteger, 0.35, false);
    add_type!("uint_fast64_t", "u64", TypeCategory::FixedWidthInteger, 0.35, false);

    // Least integer types
    add_type!("int_least8_t", "i8", TypeCategory::FixedWidthInteger, 0.35, true);
    add_type!("int_least16_t", "i16", TypeCategory::FixedWidthInteger, 0.35, true);
    add_type!("int_least32_t", "i32", TypeCategory::FixedWidthInteger, 0.35, true);
    add_type!("int_least64_t", "i64", TypeCategory::FixedWidthInteger, 0.35, true);
    add_type!("uint_least8_t", "u8", TypeCategory::FixedWidthInteger, 0.35, false);
    add_type!("uint_least16_t", "u16", TypeCategory::FixedWidthInteger, 0.35, false);
    add_type!("uint_least32_t", "u32", TypeCategory::FixedWidthInteger, 0.35, false);
    add_type!("uint_least64_t", "u64", TypeCategory::FixedWidthInteger, 0.35, false);

    // Storage qualifiers (lower scoring weight)
    add_type!("static", "static", TypeCategory::StorageQualifier, 0.15, false);
    add_type!("const", "const", TypeCategory::StorageQualifier, 0.15, false);
    add_type!("extern", "extern", TypeCategory::StorageQualifier, 0.15, false);
    add_type!("register", "register", TypeCategory::StorageQualifier, 0.15, false);
    add_type!("auto", "auto", TypeCategory::StorageQualifier, 0.15, false);
    add_type!("volatile", "volatile", TypeCategory::StorageQualifier, 0.15, false);

    // Common C library types (medium scoring weight)
    add_type!("FILE", "libc::FILE", TypeCategory::SystemType, 0.30, false);
    add_type!("DIR", "libc::DIR", TypeCategory::SystemType, 0.30, false);
    add_type!("time_t", "libc::time_t", TypeCategory::SystemType, 0.30, false);
    add_type!("clock_t", "libc::clock_t", TypeCategory::SystemType, 0.30, false);
    add_type!("tm", "libc::tm", TypeCategory::SystemType, 0.30, false);
    add_type!("timespec", "libc::timespec", TypeCategory::SystemType, 0.30, false);
    add_type!("timeval", "libc::timeval", TypeCategory::SystemType, 0.30, false);
    add_type!("va_list", "libc::va_list", TypeCategory::SystemType, 0.30, false);
    add_type!("pthread_t", "libc::pthread_t", TypeCategory::SystemType, 0.30, false);
    add_type!("pthread_mutex_t", "libc::pthread_mutex_t", TypeCategory::SystemType, 0.30, false);
    add_type!("pthread_cond_t", "libc::pthread_cond_t", TypeCategory::SystemType, 0.30, false);

    // Generate pointer variants
    let base_types: Vec<(String, String)> = type_map.iter()
        .map(|(k, v)| (k.clone(), v.clone()))
        .collect();

    for (c_type, rust_type) in &base_types {
        // Single pointer
        type_map.insert(format!("{}*", c_type), format!("*mut {}", rust_type));
        type_map.insert(format!("const {}*", c_type), format!("*const {}", rust_type));

        // Double pointer
        type_map.insert(format!("{}**", c_type), format!("*mut *mut {}", rust_type));
        type_map.insert(format!("const {}**", c_type), format!("*const *const {}", rust_type));
    }

    // Generate array variants (including pointer arrays like char*[])
    // Arrays in function parameters decay to pointers in C
    for (c_type, rust_type) in &base_types {
        // Unknown size array: type[] -> *mut type (in parameters, arrays decay to pointers)
        type_map.insert(format!("{}[]", c_type), format!("*mut {}", rust_type));
        type_map.insert(format!("const {}[]", c_type), format!("*const {}", rust_type));

        // Pointer array: type*[] -> *mut *mut type (common in argv)
        type_map.insert(format!("{}*[]", c_type), format!("*mut *mut {}", rust_type));
        type_map.insert(format!("const {}*[]", c_type), format!("*const *const {}", rust_type));
    }

    TypeConversionMap {
        type_map,
        metadata_map,
        category_index,
    }
});

/// Hardcoded map of delimiter pairs for the Pair pattern
/// Maps opening delimiter to closing delimiter, or self-pairs for identical delimiters
fn get_delimiter_pairs() -> HashMap<char, char> {
    let mut pairs = HashMap::new();

    // Standard bracket pairs
    pairs.insert('(', ')');
    pairs.insert('[', ']');
    pairs.insert('{', '}');
    pairs.insert('<', '>');

    // Quote pairs (identical)
    pairs.insert('"', '"');
    pairs.insert('\'', '\'');
    pairs.insert('`', '`');

    // Programming-specific pairs
    pairs.insert('|', '|'); // Bitwise OR, absolute value
    pairs.insert('*', '*'); // Markdown emphasis, C comments
    pairs.insert('#', '#'); // Preprocessor, comments

    // Mathematical/scientific pairs
    pairs.insert('∥', '∥'); // Parallel lines
    pairs.insert('‖', '‖'); // Double vertical bar

    // Extended bracket variants
    pairs.insert('⟨', '⟩'); // Angle brackets
    pairs.insert('⟦', '⟧'); // Double brackets
    pairs.insert('⦃', '⦄'); // Fancy braces

    pairs
}

/// Check if a character is a valid delimiter (opening or closing)
fn is_delimiter_char(c: char) -> bool {
    let pairs = get_delimiter_pairs();
    pairs.contains_key(&c) || pairs.values().any(|&v| v == c)
}

/// Validate that delimiters are properly paired in a token sequence
/// Supports nested delimiters and both opposite pairs and identical pairs
fn validate_delimiter_pairs(tokens: &[Token]) -> Result<()> {
    let delimiter_map = get_delimiter_pairs();
    let mut stack: Vec<(char, usize)> = Vec::new(); // (delimiter, position)
    let mut identical_pair_states: HashMap<char, bool> = HashMap::new(); // track state of identical pairs

    for (pos, token) in tokens.iter().enumerate() {
        let token_str = token.to_string();

        // Only process single-character delimiter tokens
        if token_str.len() != 1 {
            continue;
        }

        let c = token_str.chars().next().unwrap();

        if let Some(&closing_char) = delimiter_map.get(&c) {
            if closing_char == c {
                // Handle identical pairs (quotes, pipes, etc.)
                let is_open = identical_pair_states.get(&c).unwrap_or(&false);
                if *is_open {
                    // This is a closing delimiter
                    if let Some((last_char, _)) = stack.last() {
                        if *last_char == c {
                            stack.pop();
                            identical_pair_states.insert(c, false);
                        } else {
                            return Err(C2RError::new(
                                Kind::Logic,
                                Reason::Invalid("delimiter pairing"),
                                Some(format!(
                                    "Mismatched delimiter at position {}: expected '{}', found '{}'",
                                    pos, last_char, c
                                )),
                            ));
                        }
                    } else {
                        return Err(C2RError::new(
                            Kind::Logic,
                            Reason::Invalid("delimiter pairing"),
                            Some(format!(
                                "Unmatched closing delimiter '{}' at position {}",
                                c, pos
                            )),
                        ));
                    }
                } else {
                    // This is an opening delimiter
                    stack.push((c, pos));
                    identical_pair_states.insert(c, true);
                }
            } else {
                // Handle opposite pairs (brackets, braces, etc.)
                stack.push((c, pos));
            }
        } else if delimiter_map.values().any(|&v| v == c) {
            // This is a closing delimiter for opposite pairs
            if let Some((last_opener, _)) = stack.last() {
                if let Some(&expected_closer) = delimiter_map.get(last_opener) {
                    if expected_closer == c && *last_opener != c {
                        stack.pop();
                    } else {
                        return Err(C2RError::new(
                            Kind::Logic,
                            Reason::Invalid("delimiter pairing"),
                            Some(format!(
                                "Mismatched delimiter at position {}: expected '{}', found '{}'",
                                pos, expected_closer, c
                            )),
                        ));
                    }
                } else {
                    return Err(C2RError::new(
                        Kind::Logic,
                        Reason::Invalid("delimiter pairing"),
                        Some(format!(
                            "Unexpected closing delimiter '{}' at position {}",
                            c, pos
                        )),
                    ));
                }
            } else {
                return Err(C2RError::new(
                    Kind::Logic,
                    Reason::Invalid("delimiter pairing"),
                    Some(format!(
                        "Unmatched closing delimiter '{}' at position {}",
                        c, pos
                    )),
                ));
            }
        }
    }

    // Check for unclosed delimiters
    if !stack.is_empty() {
        let unclosed: Vec<String> = stack
            .iter()
            .map(|(c, pos)| format!("'{}' at position {}", c, pos))
            .collect();
        return Err(C2RError::new(
            Kind::Logic,
            Reason::Invalid("delimiter pairing"),
            Some(format!("Unclosed delimiter(s): {}", unclosed.join(", "))),
        ));
    }

    Ok(())
}

/// Represents different types of token patterns for validation
#[derive(Debug, Clone, Eq, Hash)]
pub enum TokenPattern {
    /// Exact token match
    Exact(String),
    /// Any token from a set of options
    OneOf(Vec<String>),
    /// Count exact occurrences of a pattern in a token sequence (sequence-level validation)
    CountOf(String, usize),
    /// Any token NOT from a set of options
    NotOneOf(Vec<String>),
    /// Any identifier-like token (alphanumeric, not punctuation)
    Identifier,
    /// Any type keyword (int, char, struct, etc.)
    TypeKeyword,
    /// Any C keyword
    CKeyword,
    /// Any punctuation token
    Punctuation,
    /// Any numeric literal
    Number,
    /// Any token (wildcard)
    Any,
    /// End of token sequence
    End,
    /// Paired delimiters (brackets, braces, quotes, etc.) with proper opening/closing validation
    Pair,

    // === ADVANCED SUBSTRING MATCHING VARIANTS ===
    /// Token contains specified substring (case-sensitive)
    SubstringContains(String),
    /// Token contains any of the specified substrings
    SubstringContainsAny(Vec<String>),
    /// Token matches path-like pattern (contains / or \ or .)
    PathLike,
    /// Complex identifier with embedded special characters (-, _, etc.)
    IdentifierWithChars(Vec<char>),
    /// Operator character but in identifier context (not standalone operator)
    OperatorInIdentifier(char),
    /// Token starts with specified prefix
    StartsWith(String),
    /// Token ends with specified suffix
    EndsWith(String),
    /// Token matches regex pattern
    RegexPattern(String),
    /// Token is fragmented across multiple tokens (for reconstruction)
    FragmentedToken {
        parts: Vec<String>,
        separators: Vec<char>,
    },

    // === NEW: TOKEN TYPE-AWARE PATTERNS ===
    /// Match any numeric token (integers, floats, numeric strings)
    NumericToken,
    /// Match specifically integer tokens (signed/unsigned)
    IntegerToken,
    /// Match specifically float/double tokens
    FloatToken,
    /// Match whitespace tokens (spaces, tabs, newlines)
    WhitespaceToken,
    /// Match tokens by their internal type variant
    TokenType(TokenTypeVariant),
    /// Match tokens using any of the provided token instances directly
    TokenInstances(Vec<Token>),
    /// Match tokens that satisfy a custom predicate function
    TokenPredicate(fn(&Token) -> bool),
}
impl PartialEq for TokenPattern {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (TokenPattern::Exact(s1), TokenPattern::Exact(s2)) => s1 == s2,
            (TokenPattern::OneOf(v1), TokenPattern::OneOf(v2)) => v1 == v2,
            (TokenPattern::CountOf(v1, n1), TokenPattern::CountOf(v2, n2)) => v1 == v2 && n1 == n2,
            (TokenPattern::NotOneOf(v1), TokenPattern::NotOneOf(v2)) => v1 == v2,
            (TokenPattern::Identifier, TokenPattern::Identifier) => true,
            (TokenPattern::TypeKeyword, TokenPattern::TypeKeyword) => true,
            (TokenPattern::CKeyword, TokenPattern::CKeyword) => true,
            (TokenPattern::Punctuation, TokenPattern::Punctuation) => true,
            (TokenPattern::Number, TokenPattern::Number) => true,
            (TokenPattern::Any, TokenPattern::Any) => true,
            (TokenPattern::End, TokenPattern::End) => true,
            _ => false,
        }
    }
}
/// Represents Token type variants for type-aware matching
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenTypeVariant {
    /// Token::a (byte array)
    Array,
    /// Token::b (single byte)
    Byte,
    /// Token::c (character)
    Char,
    /// Token::f (float)
    Float,
    /// Token::i (signed integer)
    SignedInt,
    /// Token::s (string)
    String,
    /// Token::l (static string literal)
    Literal,
    /// Token::u (unsigned integer)
    UnsignedInt,
    /// Token::v (byte vector)
    Vector,
    /// Token::w (whitespace sequence)
    Whitespace,
    /// Token::d (delimiter character sequence)
    Delimiter,
}
impl Display for TokenTypeVariant {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TokenTypeVariant::Array => write!(f, "array"),
            TokenTypeVariant::Byte => write!(f, "byte"),
            TokenTypeVariant::Char => write!(f, "char"),
            TokenTypeVariant::Float => write!(f, "float"),
            TokenTypeVariant::SignedInt => write!(f, "signed int"),
            TokenTypeVariant::String => write!(f, "string"),
            TokenTypeVariant::Literal => write!(f, "literal"),
            TokenTypeVariant::UnsignedInt => write!(f, "unsigned int"),
            TokenTypeVariant::Vector => write!(f, "vector"),
            TokenTypeVariant::Whitespace => write!(f, "whitespace"),
            TokenTypeVariant::Delimiter => write!(f, "delimiter"),
        }
    }
}
/// Represents the result of pattern matching
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PatternResult {
    /// Pattern matches successfully
    Match { consumed_tokens: usize },
    /// a given number of tokens match in the correct order but at offset indices.
    CountOf { offsets: Vec<Range<usize>> },
    /// a series of tokens in order match but not all
    Sequence { range: Range<usize> },
    /// a number or more match, but they are out of order somewhat
    Fuzzy { offsets: Vec<Range<usize>> },
    /// Pattern does not match
    NoMatch { reason: String },
    /// Pattern explicitly rejects this sequence
    Reject { reason: String },

    // === NEW: ENHANCED MISMATCH RESULTS FOR NEGATIVE CACHING ===
    /// Token type mismatch (expected int but got string, etc.)
    TypeMismatch {
        expected_type: TokenTypeVariant,
        actual_type: TokenTypeVariant,
        position: usize,
        reason: String,
    },
    /// Value mismatch (correct type but wrong value)
    ValueMismatch {
        expected_value: String,
        actual_value: String,
        position: usize,
        reason: String,
    },
    /// Structural mismatch (wrong sequence length, order, etc.)
    StructureMismatch {
        expected_pattern: String,
        actual_structure: String,
        reason: String,
    },
    /// Cached negative result (this pattern is known not to match this token sequence)
    CachedNegative {
        pattern_id: String,
        cache_hit_count: usize,
        reason: String,
    },
    /// Cached positive result (this pattern is known to match this token sequence)
    CachedPositive {
        pattern_id: String,
        cache_hit_count: usize,
        reason: String,
    },
}
impl Default for PatternResult {
    fn default() -> Self {
        PatternResult::NoMatch {
            reason: "No match".to_string(),
        }
    }
}
impl Display for PatternResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PatternResult::Match { consumed_tokens } => {
                write!(f, "Match(consumed: {})", consumed_tokens)
            }
            PatternResult::CountOf { offsets } => {
                write!(f, "CountOf({} offsets)", offsets.len())
            }
            PatternResult::Sequence { range } => {
                write!(f, "Sequence({}..{})", range.start, range.end)
            }
            PatternResult::Fuzzy { offsets } => {
                write!(f, "Fuzzy({} offsets)", offsets.len())
            }
            PatternResult::NoMatch { reason } => {
                write!(f, "NoMatch({})", reason.replace('\n', " | "))
            }
            PatternResult::Reject { reason } => {
                write!(f, "Reject({})", reason.replace('\n', " | "))
            }
            PatternResult::TypeMismatch {
                expected_type,
                actual_type,
                position,
                reason,
            } => {
                write!(
                    f,
                    "TypeMismatch(expected: {:?}, actual: {:?}, pos: {}, {})",
                    expected_type,
                    actual_type,
                    position,
                    reason.replace('\n', " | ")
                )
            }
            PatternResult::ValueMismatch {
                expected_value,
                actual_value,
                position,
                reason,
            } => {
                write!(
                    f,
                    "ValueMismatch(expected: '{}', actual: '{}', pos: {}, {})",
                    expected_value,
                    actual_value,
                    position,
                    reason.replace('\n', " | ")
                )
            }
            PatternResult::StructureMismatch {
                expected_pattern,
                actual_structure,
                reason,
            } => {
                write!(
                    f,
                    "StructureMismatch(expected: '{}', actual: '{}', {})",
                    expected_pattern,
                    actual_structure,
                    reason.replace('\n', " | ")
                )
            }
            PatternResult::CachedNegative {
                pattern_id,
                cache_hit_count,
                reason,
            } => {
                write!(
                    f,
                    "CachedNegative(pattern: '{}', hits: {}, {})",
                    pattern_id,
                    cache_hit_count,
                    reason.replace('\n', " | ")
                )
            }
            PatternResult::CachedPositive {
                pattern_id,
                cache_hit_count,
                reason,
            } => {
                write!(
                    f,
                    "CachedPositive(pattern: '{}', hits: {}, {})",
                    pattern_id,
                    cache_hit_count,
                    reason.replace('\n', " | ")
                )
            }
        }
    }
}

/// Token fingerprint for generating efficient cache keys based on token types and properties
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TokenFingerprint {
    /// The type variant of the token
    pub token_type: TokenTypeVariant,
    /// Hash of the token's string representation (for value-based matching)
    pub value_hash: u64,
    /// Length of the token's string representation
    pub length: usize,
    /// Additional properties for specialized matching
    pub properties: TokenProperties,
}

/// Additional token properties for fingerprinting
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TokenProperties {
    /// Whether the token is numeric (for quick numeric pattern matching)
    pub is_numeric: bool,
    /// Whether the token is an integer (subset of numeric)
    pub is_integer: bool,
    /// Whether the token is a float (subset of numeric)
    pub is_float: bool,
    /// Whether the token is whitespace
    pub is_whitespace: bool,
    /// First character (for prefix matching optimization)
    pub first_char: Option<char>,
    /// Last character (for suffix matching optimization)
    pub last_char: Option<char>,
}

/// Sequence fingerprint for caching entire token sequences
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SequenceFingerprint {
    /// Fingerprints of individual tokens in the sequence
    pub token_fingerprints: Vec<TokenFingerprint>,
    /// Total sequence length
    pub sequence_length: usize,
    /// Compact representation for fast comparison
    pub signature: u64,
}

/// A single pattern matching rule
#[derive(Debug, Clone, Eq, Hash)]
pub struct PatternRule {
    /// The pattern to match at this position
    pub pattern: TokenPattern,
    /// What patterns can follow this one (cascading)
    pub allowed_next: Vec<TokenPattern>,
    /// What patterns explicitly cannot follow this one
    pub forbidden_next: Vec<TokenPattern>,
    /// Whether this pattern is optional
    pub optional: bool,
    /// Custom validation function for complex logic
    pub custom_validator: Option<fn(&[Token], usize) -> bool>,
}

impl PartialEq for PatternRule {
    fn eq(&self, other: &Self) -> bool {
        self.pattern == other.pattern
            && self.allowed_next == other.allowed_next
            && self.forbidden_next == other.forbidden_next
            && self.optional == other.optional
    }
}

/// A complete pattern matching configuration for a handler
#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct HandlerPattern {
    /// Unique identifier for this pattern
    pub id: Id,
    /// Human-readable description
    pub description: Option<String>,
    /// Sequence of pattern rules to match
    pub rules: Vec<PatternRule>,
    /// Minimum number of tokens required
    pub min_tokens: usize,
    /// Maximum number of tokens to consider (None = unlimited)
    pub max_tokens: Option<usize>,
    /// Priority level for pattern matching (higher = more priority)
    pub priority: i32,
}

/// Central pattern matching engine
#[derive(Debug, Default, Clone, Eq)]
pub struct Patternizer {
    /// Registered patterns by handler type
    patterns: HashMap<String, Vec<HandlerPattern>>,
    /// Cache for performance optimization
    pub match_cache: HashMap<String, PatternResult>,
}
impl PartialEq for Patternizer {
    fn eq(&self, other: &Self) -> bool {
        // Compare patterns HashMap by checking each key-value pair individually
        if self.patterns.len() != other.patterns.len() {
            return false;
        }

        for (key, value) in &self.patterns {
            match other.patterns.get(key) {
                Some(other_value) => {
                    if value != other_value {
                        return false;
                    }
                }
                None => return false,
            }
        }

        // Compare match_cache HashMap by checking each key-value pair individually
        if self.match_cache.len() != other.match_cache.len() {
            return false;
        }

        for (key, value) in &self.match_cache {
            match other.match_cache.get(key) {
                Some(other_value) => {
                    if value != other_value {
                        return false;
                    }
                }
                None => return false,
            }
        }

        true
    }
}
impl Hash for Patternizer {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.patterns.iter().for_each(|(key, value)| {
            key.hash(state);
            value.hash(state);
        });
        self.match_cache.iter().for_each(|(key, value)| {
            key.hash(state);
            value.hash(state);
        });
    }
}

/// Helper function to determine the TokenTypeVariant from a Token
pub fn get_token_type_variant(token: &Token) -> TokenTypeVariant {
    match token {
        Token::a(_, _) => TokenTypeVariant::Array,
        Token::b(_) => TokenTypeVariant::Byte,
        Token::c(_) => TokenTypeVariant::Char,
        Token::f(_) => TokenTypeVariant::Float,
        Token::i(_) => TokenTypeVariant::SignedInt,
        Token::s(_) => TokenTypeVariant::String,
        Token::l(_) => TokenTypeVariant::Literal,
        Token::u(_) => TokenTypeVariant::UnsignedInt,
        Token::v(_) => TokenTypeVariant::Vector,
        Token::w(_) => TokenTypeVariant::Whitespace,
        Token::d(_) => TokenTypeVariant::Delimiter,
        Token::n() => TokenTypeVariant::String, // Treat consumed tokens as empty strings
    }
}

/// Create a safe fallback fingerprint for corrupted tokens
fn create_safe_fallback_fingerprint() -> TokenFingerprint {
    TokenFingerprint {
        token_type: TokenTypeVariant::String,
        value_hash: 0xDEADBEEF, // Unique hash for corrupted tokens
        length: 0,
        properties: TokenProperties {
            is_numeric: false,
            is_integer: false,
            is_float: false,
            is_whitespace: false,
            first_char: Some('?'),
            last_char: Some('?'),
        },
    }
}

/// Generate a TokenFingerprint for efficient caching and comparison
pub fn generate_token_fingerprint(token: &Token) -> TokenFingerprint {
    // ULTRA-SAFE: Try to detect corrupted tokens before any operations
    match std::panic::catch_unwind(|| {
        let debug_str = format!("{:?}", token);
        debug_str.len() < 1000 && !debug_str.contains("�")
    }) {
        Ok(true) => {} // Token seems safe, continue
        Ok(false) | Err(_) => {
            eprintln!("⚠️  CRITICAL: Detected corrupted/oversized token, using safe fallback");
            return create_safe_fallback_fingerprint();
        }
    }

    // Safe token string conversion with panic protection
    let token_str = match std::panic::catch_unwind(|| token.to_string()) {
        Ok(s) => {
            // Additional check after to_string
            if s.contains('�') || s.len() > 500 || s.as_bytes().contains(&0xFF) {
                eprintln!(
                    "⚠️  WARNING: Token string contains suspicious data, sanitizing: {}",
                    s.chars().take(20).collect::<String>()
                );
                s.chars()
                    .filter(|&c| c != '�' && c.is_ascii_graphic() || c.is_whitespace())
                    .collect()
            } else {
                s
            }
        }
        Err(_) => {
            eprintln!("⚠️  ERROR: Token.to_string() panicked, using safe fallback");
            return create_safe_fallback_fingerprint();
        }
    };

    // Final safety check
    let safe_token_str = if token_str.is_empty() {
        "empty".to_string()
    } else {
        token_str
    };

    let mut hasher: DefaultHasher = DefaultHasher::new();
    safe_token_str.hash(&mut hasher);
    let value_hash: u64 = hasher.finish();

    // Safe character extraction with panic protection
    let (first_char, last_char) = match std::panic::catch_unwind(|| {
        let chars: Vec<char> = safe_token_str.chars().collect();
        let first = chars.first().copied();
        let last = chars.last().copied();
        (first, last)
    }) {
        Ok((first, last)) => (first, last),
        Err(_) => {
            // Fallback for corrupted UTF-8: try to extract safely
            eprintln!(
                "⚠️  WARNING: Corrupted chars in token, using byte fallback: {:?}",
                safe_token_str
            );
            let bytes = safe_token_str.as_bytes();
            let first_char = if !bytes.is_empty() {
                Some(bytes[0] as char)
            } else {
                None
            };
            let last_char = if !bytes.is_empty() {
                Some(bytes[bytes.len() - 1] as char)
            } else {
                None
            };
            (first_char, last_char)
        }
    };

    // Safe token method calls with panic protection
    let (is_numeric, is_integer, is_float, is_whitespace) = match std::panic::catch_unwind(|| {
        (
            token.is_numeric(),
            token.is_int(),
            token.is_float(),
            token.is_whitespace(),
        )
    }) {
        Ok(values) => values,
        Err(_) => {
            eprintln!(
                "⚠️  WARNING: Token method calls failed, using safe defaults for: {:?}",
                token
            );
            (false, false, false, false)
        }
    };

    TokenFingerprint {
        token_type: get_token_type_variant(token),
        value_hash,
        length: safe_token_str.len(),
        properties: TokenProperties {
            is_numeric,
            is_integer,
            is_float,
            is_whitespace,
            first_char,
            last_char,
        },
    }
}

/// Generate a SequenceFingerprint for caching entire token sequences
pub fn generate_sequence_fingerprint(tokens: &[Token]) -> SequenceFingerprint {
    // Safety check: Prevent stack overflow with very large token sequences
    if tokens.len() > 10000 {
        eprintln!(
            "⚠️  WARNING: Very large token sequence ({} tokens) - truncating for safety",
            tokens.len()
        );
        return generate_sequence_fingerprint(&tokens[..10000]);
    }

    // Safety check: Handle empty token sequences
    if tokens.is_empty() {
        return SequenceFingerprint {
            token_fingerprints: Vec::new(),
            sequence_length: 0,
            signature: 0,
        };
    }

    // Debug: Log token sequence info for debugging
    if tokens.len() > 100 {
        println!(
            "🔍 DEBUG: Processing large token sequence with {} tokens",
            tokens.len()
        );
    }

    // Pre-scan for corrupted tokens
    let mut corrupted_count = 0;
    for (i, token) in tokens.iter().enumerate().take(50) {
        // Check first 50 tokens
        if std::panic::catch_unwind(|| generate_token_fingerprint(token)).is_err() {
            corrupted_count += 1;
        }
        // Early abort if too many corrupted tokens detected
        if corrupted_count > 5 {
            eprintln!(
                "🚨 CRITICAL: Too many corrupted tokens detected early ({} in first 50), using safe fallback sequence",
                corrupted_count
            );
            return SequenceFingerprint {
                token_fingerprints: vec![create_safe_fallback_fingerprint()],
                sequence_length: tokens.len(),
                signature: 0xBADDC0DE, // Special signature for corrupted sequences
            };
        }
    }

    let token_fingerprints: Vec<TokenFingerprint> = tokens
        .iter()
        .enumerate()
        .map(|(i, token)| {
            // Add error handling for individual token fingerprinting
            match std::panic::catch_unwind(|| generate_token_fingerprint(token)) {
                Ok(fingerprint) => fingerprint,
                Err(_) => {
                    eprintln!("⚠️  ERROR: Failed to generate fingerprint for token at index {} (corrupted data)", i);
                    create_safe_fallback_fingerprint()
                }
            }
        })
        .collect();

    // Generate a compact signature by combining fingerprint hashes
    let mut hasher: DefaultHasher = DefaultHasher::new();
    for fingerprint in &token_fingerprints {
        fingerprint.hash(&mut hasher);
    }
    let signature: u64 = hasher.finish();

    SequenceFingerprint {
        token_fingerprints,
        sequence_length: tokens.len(),
        signature,
    }
}

impl TokenPattern {
    /// Check if a token matches this pattern (ENHANCED with token type awareness)
    pub fn matches(&self, token: &Token) -> bool {
        let token_str = token.to_string();

        match self {
            TokenPattern::Exact(expected) => token_str == *expected,
            TokenPattern::OneOf(options) => options.contains(&token_str),
            TokenPattern::CountOf(pattern, _count) => {
                // For single token matching, check if this token matches the pattern
                // The actual count validation happens at sequence level in match_single_pattern
                token_str == *pattern
            }
            TokenPattern::NotOneOf(forbidden) => !forbidden.contains(&token_str),
            TokenPattern::Identifier => {
                !token_str.is_empty()
                    && !token_str.chars().all(|c| c.is_ascii_punctuation())
                    && token_str
                    .chars()
                    .next()
                    .map_or(false, |c| c.is_alphanumeric())
            }
            TokenPattern::TypeKeyword => {
                is_c_keyword(token.clone())
                    || matches!(
                        token_str.as_str(),
                        "struct"
                            | "enum"
                            | "union"
                            | "typedef"
                            | "const"
                            | "static"
                            | "extern"
                            | "inline"
                    )
            }
            TokenPattern::CKeyword => is_c_keyword(token.clone()),
            TokenPattern::Punctuation => {
                !token_str.is_empty() && token_str.chars().all(|c| c.is_ascii_punctuation())
            }
            TokenPattern::Number => {
                if token_str.is_empty() {
                    return false;
                }
                // Handle various numeric formats: integers, floats, scientific notation, hex
                token_str.parse::<f64>().is_ok()
                    || token_str.parse::<i64>().is_ok()
                    || (token_str.starts_with("0x")
                    && token_str[2..].chars().all(|c| c.is_ascii_hexdigit()))
            }
            TokenPattern::Any => true,
            TokenPattern::End => false,
            TokenPattern::Pair => {
                // For single token matching, check if token is a valid delimiter character
                // Actual pair validation happens at sequence level
                token_str.len() <= 2 && token_str.chars().all(|c| is_delimiter_char(c))
            }

            // Advanced substring matching
            TokenPattern::SubstringContains(substring) => token_str.contains(substring),
            TokenPattern::SubstringContainsAny(substrings) => {
                substrings.iter().any(|s| token_str.contains(s))
            }
            TokenPattern::PathLike => {
                token_str.contains('/')
                    || token_str.contains('\\')
                    || (token_str.contains('.')
                    && (token_str.ends_with(".c")
                    || token_str.ends_with(".h")
                    || token_str.ends_with(".cpp")
                    || token_str.ends_with(".hpp")
                    || token_str.contains("./")
                    || token_str.contains("../")))
            }
            TokenPattern::IdentifierWithChars(allowed_chars) => {
                if token_str.is_empty() {
                    return false;
                }

                let mut chars = token_str.chars();
                if let Some(first_char) = chars.next() {
                    (first_char.is_alphanumeric() || first_char == '_')
                        && chars
                        .all(|c| c.is_alphanumeric() || c == '_' || allowed_chars.contains(&c))
                } else {
                    false
                }
            }
            TokenPattern::OperatorInIdentifier(op_char) => {
                token_str.len() > 1
                    && token_str.contains(*op_char)
                    && !token_str.chars().all(|c| c.is_ascii_punctuation())
                    && token_str.chars().any(|c| c.is_alphanumeric() || c == '_')
            }
            TokenPattern::StartsWith(prefix) => token_str.starts_with(prefix),
            TokenPattern::EndsWith(suffix) => token_str.ends_with(suffix),
            TokenPattern::RegexPattern(pattern) => Self::simple_regex_match(&token_str, pattern),
            TokenPattern::FragmentedToken {
                parts,
                separators: _,
            } => parts.iter().any(|part| token_str == *part),

            // === NEW: TOKEN TYPE-AWARE PATTERN MATCHING ===
            TokenPattern::NumericToken => token.is_numeric(),
            TokenPattern::IntegerToken => token.is_int(),
            TokenPattern::FloatToken => token.is_float(),
            TokenPattern::WhitespaceToken => token.is_whitespace(),
            TokenPattern::TokenType(expected_type) => {
                get_token_type_variant(token) == *expected_type
            }
            TokenPattern::TokenInstances(instances) => token.match_any(instances),
            TokenPattern::TokenPredicate(predicate_fn) => predicate_fn(token),
        }
    }

    /// Enhanced regex-like pattern matching with extended wildcard and character class support
    /// Supports: * (any chars), ? (single char), [abc] (character classes), [!abc] (negated classes), literal matching
    fn simple_regex_match(text: &str, pattern: &str) -> bool {
        if pattern.is_empty() {
            return text.is_empty();
        }

        // Handle simple wildcard patterns
        if pattern == "*" {
            return true;
        }

        // Handle prefix wildcard: "prefix*"
        if pattern.ends_with('*') && !pattern.starts_with('*') && !pattern.contains('[') {
            let prefix: &str = &pattern[..pattern.len() - 1];
            return text.starts_with(prefix);
        }

        // Handle suffix wildcard: "*suffix"
        if pattern.starts_with('*') && !pattern.ends_with('*') && !pattern.contains('[') {
            let suffix: &str = &pattern[1..];
            return text.ends_with(suffix);
        }

        // Handle contains wildcard: "*substring*"
        if pattern.starts_with('*')
            && pattern.ends_with('*')
            && pattern.len() > 2
            && !pattern.contains('[')
        {
            let substring: &str = &pattern[1..pattern.len() - 1];
            return text.contains(substring);
        }

        // Enhanced pattern matching with character classes and ? wildcards
        Self::advanced_pattern_match(text.chars().collect(), pattern.chars().collect(), 0, 0)
    }

    /// Advanced pattern matching supporting ?, *, and character classes [abc] or [!abc]
    fn advanced_pattern_match(
        text: Vec<char>,
        pattern: Vec<char>,
        text_idx: usize,
        pattern_idx: usize,
    ) -> bool {
        if pattern_idx >= pattern.len() {
            return text_idx >= text.len();
        }

        if text_idx >= text.len() {
            // Check if remaining pattern is all wildcards
            return pattern[pattern_idx..].iter().all(|&c| c == '*');
        }

        match pattern[pattern_idx] {
            '*' => {
                // Try matching zero or more characters
                Self::advanced_pattern_match(
                    text.clone(),
                    pattern.clone(),
                    text_idx,
                    pattern_idx + 1,
                ) || Self::advanced_pattern_match(text, pattern, text_idx + 1, pattern_idx)
            }
            '?' => {
                // Match exactly one character
                Self::advanced_pattern_match(text, pattern, text_idx + 1, pattern_idx + 1)
            }
            '[' => {
                // Character class matching
                if let Some(class_end) = pattern[pattern_idx..].iter().position(|&c| c == ']') {
                    let class_end: usize = pattern_idx + class_end;
                    let class_content: String =
                        pattern[pattern_idx + 1..class_end].iter().collect();

                    let negated: bool = class_content.starts_with('!');
                    let chars_to_match: &str = if negated {
                        &class_content[1..]
                    } else {
                        &class_content
                    };

                    let matches = chars_to_match.contains(text[text_idx]);
                    let result = if negated { !matches } else { matches };

                    if result {
                        Self::advanced_pattern_match(text, pattern, text_idx + 1, class_end + 1)
                    } else {
                        false
                    }
                } else {
                    // Malformed character class, treat as literal
                    text[text_idx] == '['
                        && Self::advanced_pattern_match(
                        text,
                        pattern,
                        text_idx + 1,
                        pattern_idx + 1,
                    )
                }
            }
            c => {
                // Literal character match
                text[text_idx] == c
                    && Self::advanced_pattern_match(text, pattern, text_idx + 1, pattern_idx + 1)
            }
        }
    }

    /// Detect if a token sequence represents a fragmented path or identifier
    pub fn detect_fragmented_sequence(
        tokens: &[Token],
        start_idx: usize,
    ) -> Option<(String, usize)> {
        if start_idx >= tokens.len() {
            return None;
        }

        let mut reconstructed: String = String::new();
        let mut consumed: usize = 0;
        let mut i: usize = start_idx;

        // Common fragmentation patterns in C tokenization
        let path_separators: [char; 3] = ['/', '\\', '.'];
        let identifier_chars: [char; 2] = ['-', '_'];

        while i < tokens.len() {
            let token_str: String = tokens[i].to_string();

            // Add current token
            reconstructed.push_str(&token_str);
            consumed += 1;
            i += 1;

            // Check if next token could be part of fragmented sequence
            if i < tokens.len() {
                let next_token: String = tokens[i].to_string();

                // Path continuation: "./", "../", "/path", "file.ext"
                if (token_str.ends_with('.') && (next_token == "/" || next_token.starts_with('/')))
                    || (path_separators.contains(&token_str.chars().last().unwrap_or(' '))
                    && next_token.chars().next().unwrap_or(' ').is_alphanumeric())
                    || (token_str.chars().all(|c| c.is_alphanumeric() || c == '_')
                    && next_token.len() == 1
                    && path_separators.contains(&next_token.chars().next().unwrap()))
                {
                    continue; // Keep consuming tokens
                }

                // Identifier continuation: "multi-word", "snake_case"
                if (token_str.chars().all(|c| c.is_alphanumeric() || c == '_')
                    && next_token.len() == 1
                    && identifier_chars.contains(&next_token.chars().next().unwrap()))
                    || (token_str.len() == 1
                    && identifier_chars.contains(&token_str.chars().next().unwrap())
                    && next_token.chars().next().unwrap_or(' ').is_alphanumeric())
                {
                    continue; // Keep consuming tokens
                }

                // Stop if no continuation pattern detected
                break;
            }
        }

        // Return result if we consumed more than 1 token (indicating fragmentation)
        if consumed > 1 {
            Some((reconstructed, consumed))
        } else {
            None
        }
    }
}

impl PatternRule {
    /// Create a new pattern rule with default settings
    pub fn new(pattern: TokenPattern) -> Self {
        Self {
            pattern,
            allowed_next: vec![],
            forbidden_next: vec![],
            optional: false,
            custom_validator: None,
        }
    }

    /// Add allowed next patterns (builder pattern)
    pub fn allow_next(mut self, patterns: Vec<TokenPattern>) -> Self {
        self.allowed_next.extend(patterns);
        self
    }

    /// Add forbidden next patterns (builder pattern)
    pub fn forbid_next(mut self, patterns: Vec<TokenPattern>) -> Self {
        self.forbidden_next.extend(patterns);
        self
    }

    /// Make this rule optional (builder pattern)
    pub fn optional(mut self) -> Self {
        self.optional = true;
        self
    }

    /// Add custom validation logic (builder pattern)
    pub fn with_validator(mut self, validator: fn(&[Token], usize) -> bool) -> Self {
        self.custom_validator = Some(validator);
        self
    }
}

impl HandlerPattern {
    /// Create a new handler pattern
    pub fn new(id: Id) -> Self {
        Self {
            id: id.clone(),
            description: Some(format!(
                "Pattern: {} Timestamp: {}",
                id.name(),
                id.timestamp()
            )),
            rules: vec![],
            min_tokens: 1,
            max_tokens: None,
            priority: 100,
        }
    }

    /// Add pattern rules (builder pattern)
    pub fn with_rules(mut self, rules: Vec<PatternRule>) -> Self {
        self.rules = rules;
        self
    }

    /// Set minimum tokens (builder pattern)
    pub fn min_tokens(mut self, min: usize) -> Self {
        self.min_tokens = min;
        self
    }

    /// Set maximum tokens (builder pattern)
    pub fn max_tokens(mut self, max: usize) -> Self {
        self.max_tokens = Some(max);
        self
    }

    /// Set priority (builder pattern)
    pub fn priority(mut self, priority: i32) -> Self {
        self.priority = priority;
        self
    }
}

impl Patternizer {
    /// Create a new pattern matcher
    pub fn new() -> Self {
        Self {
            patterns: HashMap::new(),
            match_cache: HashMap::new(),
        }
    }

    /// Export patterns to adaptive system for training
    pub fn export_patterns_for_adaptation(&self) -> Vec<(String, Vec<HandlerPattern>)> {
        let mut exported = Vec::new();
        for (handler_type, patterns) in &self.patterns {
            exported.push((handler_type.clone(), patterns.clone()));
        }
        exported
    }

    /// Learn from successful adaptive variations
    pub fn learn_from_adaptive_success(
        &mut self,
        context: &mut Context,
        handler_type: &str,
        successful_pattern: HandlerPattern,
        confidence: f64,
    ) {
        if confidence >= 0.8 {
            // High confidence adaptations can be promoted to regular patterns
            report!(
                context,
                "patternizer",
                "learn_from_adaptive_success",
                crate::ReportLevel::Info,
                crate::Phase::Process(Some("learn_from_adaptive_success".to_string())),
                &format!(
                    "Promoting adaptive pattern '{}' to regular pattern with confidence {:.2}",
                    successful_pattern.id.name(),
                    confidence
                ),
                true
            );

            // Create enhanced pattern with boosted priority based on confidence
            let enhanced_pattern = HandlerPattern {
                id: successful_pattern.id.clone(),
                description: successful_pattern.description.clone(),
                rules: successful_pattern.rules.clone(),
                min_tokens: successful_pattern.min_tokens,
                max_tokens: successful_pattern.max_tokens,
                priority: successful_pattern.priority + ((confidence - 0.8) * 100.0) as i32,
            };

            self.register_pattern(Id::get(handler_type), enhanced_pattern);
        } else if confidence >= 0.6 {
            // Medium confidence patterns get cached for future reference
            let cache_key = format!("{}_{}", handler_type, successful_pattern.id.name());
            self.match_cache.insert(cache_key, PatternResult::Match {
                consumed_tokens: successful_pattern.min_tokens
            });
        }
    }

    /// Create a new pattern matcher with common patterns pre-loaded
    pub fn with_common_patterns(context: &mut Context) {
        context.patternizer.initialize_common_patterns();
    }

    /// Register a pattern for a handler type
    pub fn register_pattern(&mut self, pattern_id: Id, pattern: HandlerPattern) {
        self.patterns
            .entry(pattern_id.name().to_string())
            .or_insert_with(Vec::<HandlerPattern>::new)
            .push(pattern);

        // Sort by priority (highest first)
        if let Some(patterns) = self.patterns.get_mut(pattern_id.clone().name().trim()) {
            patterns.sort_by(|a: &HandlerPattern, b: &HandlerPattern| b.priority.cmp(&a.priority));
        }
    }

    // ===== TYPE CONVERSION INTEGRATION =====

    /// Get reference to the global type conversion map
    pub fn type_map(&self) -> &TypeConversionMap {
        &TYPE_CONVERSION_MAP
    }

    /// Check if a token is a known C type
    pub fn is_c_type(&self, token: &Token) -> bool {
        TYPE_CONVERSION_MAP.is_c_type(&token.to_string())
    }

    /// Convert C type token to Rust type
    pub fn convert_c_to_rust(&self, token: &Token) -> Option<String> {
        TYPE_CONVERSION_MAP.convert_type(&token.to_string())
    }

    /// Calculate type score for tokens (for handlers to use)
    pub fn calculate_type_score(&self, tokens: &[Token]) -> f64 {
        TYPE_CONVERSION_MAP.calculate_type_score(tokens)
    }

    /// Check if tokens contain storage qualifiers
    pub fn has_storage_qualifier(&self, tokens: &[Token]) -> bool {
        TYPE_CONVERSION_MAP.has_storage_qualifier(tokens)
    }

    /// Get all types by category
    pub fn get_types_by_category(&self, category: TypeCategory) -> Vec<String> {
        TYPE_CONVERSION_MAP.get_types_by_category(category)
    }

    /// Calculate structure score for array/struct patterns
    pub fn calculate_structure_score(&self, tokens: &[Token]) -> f64 {
        let mut score: f64 = 0.0;
        let mut bracket_balance = 0;
        let mut has_brackets = false;
        let mut has_type = false;
        let mut has_identifier = false;

        for token in tokens {
            let token_str = token.to_string();
            match token_str.as_str() {
                "[" => {
                    bracket_balance += 1;
                    has_brackets = true;
                    score += 0.2;
                }
                "]" => {
                    bracket_balance -= 1;
                    score += 0.2;
                }
                s if self.is_c_type(token) => {
                    has_type = true;
                    score += 0.15;
                }
                s if !s.is_empty()
                    && s.chars()
                    .next()
                    .map_or(false, |c| c.is_alphabetic() || c == '_') =>
                    {
                        has_identifier = true;
                        score += 0.1;
                    }
                _ => {}
            }
        }

        // Bonus for complete structure
        if has_brackets && has_type && has_identifier {
            score += 0.3;
        }

        // Penalty for unbalanced brackets
        if bracket_balance != 0 {
            score -= 0.2;
        }

        score.clamp(0.0, 1.0)
    }

    /// Calculate syntax score for array/declaration patterns
    pub fn calculate_syntax_score(&self, tokens: &[Token]) -> f64 {
        let mut score: f64 = 0.0;
        let mut bracket_pairs = 0;
        let mut open_brackets = 0;
        let mut has_assignment = false;
        let mut has_semicolon = false;

        for token in tokens {
            let token_str = token.to_string();
            match token_str.as_str() {
                "[" => open_brackets += 1,
                "]" => {
                    if open_brackets > 0 {
                        bracket_pairs += 1;
                        open_brackets -= 1;
                    }
                }
                "=" => {
                    has_assignment = true;
                    score += 0.15;
                }
                ";" => {
                    has_semicolon = true;
                    score += 0.1;
                }
                "," => score += 0.05,
                "{" | "}" => score += 0.1,
                _ => {}
            }
        }

        score += bracket_pairs as f64 * 0.25;

        // Bonus for complete statement structure
        if has_assignment && has_semicolon {
            score += 0.1;
        }

        score.clamp(0.0, 1.0)
    }

    /// Calculate semantic score for array/declaration patterns
    pub fn calculate_semantic_score(&self, tokens: &[Token]) -> f64 {
        let mut score: f64 = 0.0;
        let mut consecutive_identifiers = 0;
        let mut has_size_specifier = false;

        for (i, token) in tokens.iter().enumerate() {
            let token_str = token.to_string();

            // Check for numeric size specifiers
            if token_str.chars().all(|c| c.is_numeric()) && i > 0 {
                if let Some(prev_token) = tokens.get(i - 1) {
                    if prev_token.to_string() == "[" {
                        has_size_specifier = true;
                        score += 0.2;
                    }
                }
            }

            // Count consecutive identifiers
            if token_str.chars().all(|c| c.is_alphanumeric() || c == '_') {
                consecutive_identifiers += 1;
            } else {
                consecutive_identifiers = 0;
            }

            // Penalty for too many consecutive identifiers
            if consecutive_identifiers > 3 {
                score -= 0.1;
            }
        }

        if has_size_specifier {
            score += 0.3;
        }

        score.clamp(0.0, 1.0)
    }

    /// Initialize common patterns for all handlers (moved from standalone function)
    pub fn initialize_common_patterns(&mut self) {
        // Function patterns
        let function_pattern: HandlerPattern = HandlerPattern::new(Id::get("function_declaration"))
            .with_rules(vec![
                // Return type (optional static/inline/extern)
                PatternRule::new(TokenPattern::OneOf(vec![
                    "static".to_string(),
                    "inline".to_string(),
                    "extern".to_string(),
                ]))
                    .optional(),
                PatternRule::new(TokenPattern::TypeKeyword)
                    .forbid_next(vec![TokenPattern::Exact("(".to_string())]), // Reject if type followed by (
                // Function name
                PatternRule::new(TokenPattern::Identifier)
                    .allow_next(vec![TokenPattern::Exact("(".to_string())]), // Must be followed by (
                // Opening parenthesis
                PatternRule::new(TokenPattern::Exact("(".to_string())),
                // Parameters (simplified - any tokens until closing paren)
                PatternRule::new(TokenPattern::Any).optional(), // Flexible parameter matching
            ])
            .min_tokens(4)
            .priority(200); // High priority for functions

        self.register_pattern(Id::get("function"), function_pattern);

        // Array patterns - improved with multiple variations
        let array_declaration_pattern: HandlerPattern =
            HandlerPattern::new(Id::get("array_declaration"))
                .with_rules(vec![
                    // Optional storage class
                    PatternRule::new(TokenPattern::OneOf(vec![
                        "static".to_string(),
                        "const".to_string(),
                        "extern".to_string(),
                    ]))
                        .optional(),
                    // Type
                    PatternRule::new(TokenPattern::TypeKeyword)
                        .forbid_next(vec![TokenPattern::Exact("(".to_string())]), // Reject function calls
                    // Identifier
                    PatternRule::new(TokenPattern::Identifier).with_validator(|tokens, pos| {
                        // Ensure identifier is followed by [ (not function call)
                        if pos + 1 < tokens.len() {
                            tokens[pos + 1].to_string() == "["
                        } else {
                            false
                        }
                    }),
                    // Opening bracket
                    PatternRule::new(TokenPattern::Exact("[".to_string())),
                    // Size (optional - empty brackets or number/identifier)
                    PatternRule::new(TokenPattern::Any).optional(),
                    // Closing bracket
                    PatternRule::new(TokenPattern::Exact("]".to_string())),
                ])
                .min_tokens(4)
                .priority(150);

        self.register_pattern(Id::get("array_declaration"), array_declaration_pattern);

        // Multi-dimensional array pattern
        let array_multi_pattern = HandlerPattern::new(Id::get("array_multi"))
            .with_rules(vec![
                // Type
                PatternRule::new(TokenPattern::TypeKeyword),
                // Identifier
                PatternRule::new(TokenPattern::Identifier),
                // Multiple bracket pairs
                PatternRule::new(TokenPattern::Exact("[".to_string())),
                PatternRule::new(TokenPattern::Any).optional(), // Size
                PatternRule::new(TokenPattern::Exact("]".to_string())),
                PatternRule::new(TokenPattern::Exact("[".to_string())),
                PatternRule::new(TokenPattern::Any).optional(), // Second dimension
                PatternRule::new(TokenPattern::Exact("]".to_string())),
            ])
            .min_tokens(6)
            .priority(160); // Higher priority than single arrays

        self.register_pattern(Id::get("array_multi"), array_multi_pattern);

        // General "array" pattern - catch-all for array declarations
        let array_pattern = HandlerPattern::new(Id::get("array"))
            .with_rules(vec![
                // Optional storage class
                PatternRule::new(TokenPattern::OneOf(vec![
                    "static".to_string(),
                    "const".to_string(),
                    "extern".to_string(),
                ])).optional(),
                // Type
                PatternRule::new(TokenPattern::TypeKeyword),
                // Array name
                PatternRule::new(TokenPattern::Identifier),
                // Opening bracket
                PatternRule::new(TokenPattern::Exact("[".to_string())),
            ])
            .min_tokens(4)
            .priority(145); // Between specific array patterns

        self.register_pattern(Id::get("array"), array_pattern);

        // Struct patterns
        let struct_declaration_pattern: HandlerPattern =
            HandlerPattern::new(Id::get("struct_declaration"))
                .with_rules(vec![
                    // struct keyword
                    PatternRule::new(TokenPattern::Exact("struct".to_string())),
                    // Optional struct name/tag
                    PatternRule::new(TokenPattern::Identifier).optional(),
                    // Either { for definition or ; for forward declaration
                    PatternRule::new(TokenPattern::OneOf(vec!["{".to_string(), ";".to_string()])),
                ])
                .min_tokens(2)
                .priority(140);

        self.register_pattern(Id::get("struct_declaration"), struct_declaration_pattern);

        // Struct definition pattern (with body)
        let struct_definition_pattern: HandlerPattern =
            HandlerPattern::new(Id::get("struct_definition"))
                .with_rules(vec![
                    // struct keyword
                    PatternRule::new(TokenPattern::Exact("struct".to_string())),
                    // Optional struct name/tag
                    PatternRule::new(TokenPattern::Identifier).optional(),
                    // Opening brace
                    PatternRule::new(TokenPattern::Exact("{".to_string())),
                    // Body content (flexible)
                    PatternRule::new(TokenPattern::Any).optional(),
                ])
                .min_tokens(3)
                .priority(150); // Higher priority than declaration

        self.register_pattern(Id::get("struct_definition"), struct_definition_pattern);

        // General "struct" pattern - catch-all
        let struct_pattern = HandlerPattern::new(Id::get("struct"))
            .with_rules(vec![
                PatternRule::new(TokenPattern::Exact("struct".to_string())),
                PatternRule::new(TokenPattern::Identifier).optional(),
                PatternRule::new(TokenPattern::OneOf(vec!["{".to_string(), ";".to_string()])),
            ])
            .min_tokens(2)
            .priority(135);

        self.register_pattern(Id::get("struct"), struct_pattern);

        // Enum patterns
        let enum_declaration_pattern: HandlerPattern =
            HandlerPattern::new(Id::get("enum_declaration"))
                .with_rules(vec![
                    // enum keyword
                    PatternRule::new(TokenPattern::Exact("enum".to_string())),
                    // Optional enum name/tag
                    PatternRule::new(TokenPattern::Identifier).optional(),
                    // Either { for definition or ; for forward declaration
                    PatternRule::new(TokenPattern::OneOf(vec!["{".to_string(), ";".to_string()])),
                ])
                .min_tokens(2)
                .priority(140);

        self.register_pattern(Id::get("enum_declaration"), enum_declaration_pattern);

        // Typedef enum pattern
        let typedef_enum_pattern: HandlerPattern = HandlerPattern::new(Id::get("typedef_enum"))
            .with_rules(vec![
                // typedef keyword
                PatternRule::new(TokenPattern::Exact("typedef".to_string())),
                // enum keyword
                PatternRule::new(TokenPattern::Exact("enum".to_string())),
                // Optional enum name/tag
                PatternRule::new(TokenPattern::Identifier).optional(),
                // Opening brace for definition
                PatternRule::new(TokenPattern::Exact("{".to_string())),
            ])
            .min_tokens(4)
            .priority(160); // Higher priority than simple enum

        self.register_pattern(Id::get("typedef_enum"), typedef_enum_pattern);

        // General "enum" pattern - catch-all
        let enum_pattern = HandlerPattern::new(Id::get("enum"))
            .with_rules(vec![
                PatternRule::new(TokenPattern::Exact("enum".to_string())),
                PatternRule::new(TokenPattern::Identifier).optional(),
                PatternRule::new(TokenPattern::OneOf(vec!["{".to_string(), ";".to_string()])),
            ])
            .min_tokens(2)
            .priority(135);

        self.register_pattern(Id::get("enum"), enum_pattern);

        // Typedef patterns
        let typedef_simple_pattern: HandlerPattern = HandlerPattern::new(Id::get("typedef_simple"))
            .with_rules(vec![
                // typedef keyword
                PatternRule::new(TokenPattern::Exact("typedef".to_string())),
                // Type (could be basic type or struct/enum)
                PatternRule::new(TokenPattern::OneOf(vec![
                    "int".to_string(),
                    "char".to_string(),
                    "float".to_string(),
                    "double".to_string(),
                    "void".to_string(),
                    "struct".to_string(),
                    "enum".to_string(),
                    "union".to_string(),
                ])),
                // Optional struct/enum name
                PatternRule::new(TokenPattern::Identifier).optional(),
                // New type name
                PatternRule::new(TokenPattern::Identifier),
                // Semicolon
                PatternRule::new(TokenPattern::Exact(";".to_string())),
            ])
            .min_tokens(4)
            .priority(130);

        self.register_pattern(Id::get("typedef_simple"), typedef_simple_pattern);

        // Typedef struct pattern (not captured by typedef_enum)
        let typedef_struct_pattern: HandlerPattern = HandlerPattern::new(Id::get("typedef_struct"))
            .with_rules(vec![
                // typedef keyword
                PatternRule::new(TokenPattern::Exact("typedef".to_string())),
                // struct keyword
                PatternRule::new(TokenPattern::Exact("struct".to_string())),
                // Optional struct name/tag
                PatternRule::new(TokenPattern::Identifier).optional(),
                // Either opening brace or new type name
                PatternRule::new(TokenPattern::OneOf(vec![
                    "{".to_string(),
                    "identifier".to_string(),
                ])),
            ])
            .min_tokens(3)
            .priority(150); // Higher priority than simple typedef

        self.register_pattern(Id::get("typedef_struct"), typedef_struct_pattern);

        // General "typedef" pattern - catch-all
        let typedef_pattern = HandlerPattern::new(Id::get("typedef"))
            .with_rules(vec![
                PatternRule::new(TokenPattern::Exact("typedef".to_string())),
                PatternRule::new(TokenPattern::Identifier),
                PatternRule::new(TokenPattern::Exact(";".to_string())),
            ])
            .min_tokens(3)
            .priority(125);

        self.register_pattern(Id::get("typedef"), typedef_pattern);

        // Control flow patterns
        let control_flow_if_pattern: HandlerPattern =
            HandlerPattern::new(Id::get("control_flow_if"))
                .with_rules(vec![
                    // if keyword
                    PatternRule::new(TokenPattern::Exact("if".to_string())),
                    // Opening parenthesis
                    PatternRule::new(TokenPattern::Exact("(".to_string())),
                    // Condition (flexible)
                    PatternRule::new(TokenPattern::Any).optional(),
                ])
                .min_tokens(3)
                .priority(120);

        self.register_pattern(Id::get("control_flow_if"), control_flow_if_pattern);

        let control_flow_for_pattern: HandlerPattern =
            HandlerPattern::new(Id::get("control_flow_for"))
                .with_rules(vec![
                    // for keyword
                    PatternRule::new(TokenPattern::Exact("for".to_string())),
                    // Opening parenthesis
                    PatternRule::new(TokenPattern::Exact("(".to_string())),
                    // Loop content (flexible)
                    PatternRule::new(TokenPattern::Any).optional(),
                ])
                .min_tokens(3)
                .priority(120);

        self.register_pattern(Id::get("control_flow_for"), control_flow_for_pattern);

        let control_flow_while_pattern: HandlerPattern =
            HandlerPattern::new(Id::get("control_flow_while"))
                .with_rules(vec![
                    // while keyword
                    PatternRule::new(TokenPattern::Exact("while".to_string())),
                    // Opening parenthesis
                    PatternRule::new(TokenPattern::Exact("(".to_string())),
                    // Condition (flexible)
                    PatternRule::new(TokenPattern::Any).optional(),
                ])
                .min_tokens(3)
                .priority(120);

        self.register_pattern(Id::get("control_flow_while"), control_flow_while_pattern);

        // General "loop" pattern - catch-all for any loop construct
        let loop_pattern = HandlerPattern::new(Id::get("loop"))
            .with_rules(vec![
                PatternRule::new(TokenPattern::OneOf(vec![
                    "for".to_string(),
                    "while".to_string(),
                    "do".to_string(),
                ])),
                PatternRule::new(TokenPattern::Exact("(".to_string())).optional(),
            ])
            .min_tokens(2)
            .priority(115);

        self.register_pattern(Id::get("loop"), loop_pattern);

        // Include directive patterns
        let include_directive_pattern: HandlerPattern =
            HandlerPattern::new(Id::get("include_directive"))
                .with_rules(vec![
                    // #include (as single token or separate # and include)
                    PatternRule::new(TokenPattern::OneOf(vec![
                        "#include".to_string(),
                        "#".to_string(),
                    ])),
                    PatternRule::new(TokenPattern::Exact("include".to_string())).optional(),
                    // Header file path in <> or ""
                    PatternRule::new(TokenPattern::OneOf(vec!["<".to_string(), "\"".to_string()]))
                        .optional(),
                    PatternRule::new(TokenPattern::Identifier).optional(),
                    PatternRule::new(TokenPattern::OneOf(vec![">".to_string(), "\"".to_string()]))
                        .optional(),
                ])
                .min_tokens(2)
                .priority(950);

        self.register_pattern(Id::get("include_directive"), include_directive_pattern);

        // Define macro patterns
        let define_macro_pattern: HandlerPattern = HandlerPattern::new(Id::get("define_macro"))
            .with_rules(vec![
                // #define (as single token or separate # and define)
                PatternRule::new(TokenPattern::OneOf(vec![
                    "#define".to_string(),
                    "#".to_string(),
                ])),
                PatternRule::new(TokenPattern::Exact("define".to_string())).optional(),
                // Macro name
                PatternRule::new(TokenPattern::Identifier),
                // Optional parameters or body
                PatternRule::new(TokenPattern::Any).optional(),
            ])
            .min_tokens(2)
            .priority(920);

        self.register_pattern(Id::get("define_macro"), define_macro_pattern);

        // Conditional macro patterns
        let conditional_macro_pattern: HandlerPattern =
            HandlerPattern::new(Id::get("conditional_macro"))
                .with_rules(vec![
                    // Conditional directives
                    PatternRule::new(TokenPattern::OneOf(vec![
                        "#ifdef".to_string(),
                        "#ifndef".to_string(),
                        "#endif".to_string(),
                        "#else".to_string(),
                        "#elif".to_string(),
                        "#undef".to_string(),
                        "#".to_string(),
                    ])),
                    PatternRule::new(TokenPattern::OneOf(vec![
                        "ifdef".to_string(),
                        "ifndef".to_string(),
                        "endif".to_string(),
                        "else".to_string(),
                        "elif".to_string(),
                        "undef".to_string(),
                    ]))
                        .optional(),
                    // Optional condition
                    PatternRule::new(TokenPattern::Identifier).optional(),
                ])
                .min_tokens(1)
                .priority(910);

        self.register_pattern(Id::get("conditional_macro"), conditional_macro_pattern);

        // General "macro" pattern - catch-all for preprocessor directives
        // Note: This should match AFTER specific patterns, so lower priority
        let macro_pattern = HandlerPattern::new(Id::get("macro"))
            .with_rules(vec![
                PatternRule::new(TokenPattern::OneOf(vec![
                    "#define".to_string(),
                    "#include".to_string(),
                    "#ifdef".to_string(),
                    "#ifndef".to_string(),
                    "#if".to_string(),
                    "#endif".to_string(),
                    "#pragma".to_string(),
                ])),
            ])
            .min_tokens(2)  // At least directive + something
            .priority(200);  // Lower than specific macro patterns (define_macro=920)

        self.register_pattern(Id::get("macro"), macro_pattern);

        // General "comment" pattern - catch-all for comments
        // Should only match when there's actual comment content
        let comment_pattern = HandlerPattern::new(Id::get("comment"))
            .with_rules(vec![
                PatternRule::new(TokenPattern::OneOf(vec![
                    "//".to_string(),
                    "/*".to_string(),
                ])),
            ])
            .min_tokens(2)  // Comment marker + at least some content
            .priority(95);   // Very low priority

        self.register_pattern(Id::get("comment"), comment_pattern);

        // General "variable" pattern - catch-all for variable declarations
        let variable_pattern = HandlerPattern::new(Id::get("variable"))
            .with_rules(vec![
                // Optional storage class
                PatternRule::new(TokenPattern::OneOf(vec![
                    "static".to_string(),
                    "const".to_string(),
                    "auto".to_string(),
                    "register".to_string(),
                ])).optional(),
                // Type
                PatternRule::new(TokenPattern::TypeKeyword),
                // Variable name
                PatternRule::new(TokenPattern::Identifier),
                // Semicolon or assignment (not function call)
                PatternRule::new(TokenPattern::OneOf(vec![";".to_string(), "=".to_string()]))
                    .forbid_next(vec![TokenPattern::Exact("(".to_string())]),
            ])
            .min_tokens(3)
            .priority(85);

        self.register_pattern(Id::get("variable"), variable_pattern);

        // General "expression" pattern - catch-all for expressions
        let expression_pattern = HandlerPattern::new(Id::get("expression"))
            .with_rules(vec![
                PatternRule::new(TokenPattern::Identifier),
                PatternRule::new(TokenPattern::OneOf(vec![
                    "+".to_string(),
                    "-".to_string(),
                    "*".to_string(),
                    "/".to_string(),
                    "=".to_string(),
                    "==".to_string(),
                    "!=".to_string(),
                    "<".to_string(),
                    ">".to_string(),
                ])),
                PatternRule::new(TokenPattern::Any),
            ])
            .min_tokens(3)
            .priority(40);

        self.register_pattern(Id::get("expression"), expression_pattern);

        // Main function pattern (very specific)
        let main_function_pattern: HandlerPattern = HandlerPattern::new(Id::get("main_function"))
            .with_rules(vec![
                // Optional storage class
                PatternRule::new(TokenPattern::OneOf(vec![
                    "static".to_string(),
                    "extern".to_string(),
                ]))
                    .optional(),
                // Return type
                PatternRule::new(TokenPattern::OneOf(vec![
                    "int".to_string(),
                    "void".to_string(),
                ])),
                // Main identifier
                PatternRule::new(TokenPattern::Exact("main".to_string())),
                // Opening parenthesis
                PatternRule::new(TokenPattern::Exact("(".to_string())),
                // Parameters (optional)
                PatternRule::new(TokenPattern::Any).optional(),
            ])
            .min_tokens(4)
            .priority(1000);

        self.register_pattern(Id::get("main_function"), main_function_pattern);

        // Function declaration pattern
        let function_declaration_pattern: HandlerPattern =
            HandlerPattern::new(Id::get("function_declaration"))
                .with_rules(vec![
                    // Optional storage class
                    PatternRule::new(TokenPattern::OneOf(vec![
                        "static".to_string(),
                        "inline".to_string(),
                        "extern".to_string(),
                    ]))
                        .optional(),
                    // Return type
                    PatternRule::new(TokenPattern::TypeKeyword),
                    // Function name (not main)
                    PatternRule::new(TokenPattern::Identifier).with_validator(|tokens, pos| {
                        if pos < tokens.len() {
                            tokens[pos].to_string() != "main"
                        } else {
                            false
                        }
                    }),
                    // Opening parenthesis
                    PatternRule::new(TokenPattern::Exact("(".to_string())),
                    // Parameters
                    PatternRule::new(TokenPattern::Any).optional(),
                    // Closing parenthesis
                    PatternRule::new(TokenPattern::Exact(")".to_string())),
                    // Semicolon (declaration, not definition)
                    PatternRule::new(TokenPattern::Exact(";".to_string())),
                ])
                .min_tokens(5)
                .priority(900);

        self.register_pattern(
            Id::get("function_declaration"),
            function_declaration_pattern,
        );

        // Function definition pattern
        let function_definition_pattern: HandlerPattern =
            HandlerPattern::new(Id::get("function_definition"))
                .with_rules(vec![
                    // Optional storage class
                    PatternRule::new(TokenPattern::OneOf(vec![
                        "static".to_string(),
                        "inline".to_string(),
                    ]))
                        .optional(),
                    // Return type
                    PatternRule::new(TokenPattern::TypeKeyword),
                    // Function name (not main)
                    PatternRule::new(TokenPattern::Identifier).with_validator(|tokens, pos| {
                        if pos < tokens.len() {
                            tokens[pos].to_string() != "main"
                        } else {
                            false
                        }
                    }),
                    // Opening parenthesis
                    PatternRule::new(TokenPattern::Exact("(".to_string())),
                    // Parameters
                    PatternRule::new(TokenPattern::Any).optional(),
                    // Closing parenthesis
                    PatternRule::new(TokenPattern::Exact(")".to_string())),
                    // Opening brace (definition has body)
                    PatternRule::new(TokenPattern::Exact("{".to_string())),
                ])
                .min_tokens(6)
                .priority(850);

        self.register_pattern(Id::get("function_definition"), function_definition_pattern);

        // Function prototype pattern (declaration without body)
        let function_prototype_pattern: HandlerPattern =
            HandlerPattern::new(Id::get("function_prototype"))
                .with_rules(vec![
                    // Optional storage class
                    PatternRule::new(TokenPattern::OneOf(vec![
                        "static".to_string(),
                        "extern".to_string(),
                        "inline".to_string(),
                    ])).optional(),
                    // Return type
                    PatternRule::new(TokenPattern::TypeKeyword),
                    // Optional pointer
                    PatternRule::new(TokenPattern::Exact("*".to_string())).optional(),
                    // Function name
                    PatternRule::new(TokenPattern::Identifier),
                    // Opening parenthesis
                    PatternRule::new(TokenPattern::Exact("(".to_string())),
                    // Closing parenthesis
                    PatternRule::new(TokenPattern::Exact(")".to_string())),
                    // Semicolon (not opening brace)
                    PatternRule::new(TokenPattern::Exact(";".to_string())),
                ])
                .min_tokens(5)
                .priority(890);

        self.register_pattern(Id::get("function_prototype"), function_prototype_pattern);

        // Extern declaration pattern
        let extern_declaration_pattern: HandlerPattern =
            HandlerPattern::new(Id::get("extern_declaration"))
                .with_rules(vec![
                    // Extern keyword
                    PatternRule::new(TokenPattern::Exact("extern".to_string())),
                    // Type or function signature
                    PatternRule::new(TokenPattern::OneOf(vec![
                        "int".to_string(),
                        "char".to_string(),
                        "float".to_string(),
                        "double".to_string(),
                        "void".to_string(),
                        "struct".to_string(),
                    ])),
                    // Identifier
                    PatternRule::new(TokenPattern::Identifier),
                    // Optional parentheses for function or semicolon for variable
                    PatternRule::new(TokenPattern::OneOf(vec!["(".to_string(), ";".to_string()])),
                ])
                .min_tokens(4)
                .priority(880);

        self.register_pattern(Id::get("extern_declaration"), extern_declaration_pattern);

        // Static variable pattern
        let static_variable_pattern: HandlerPattern =
            HandlerPattern::new(Id::get("static_variable"))
                .with_rules(vec![
                    // Static keyword
                    PatternRule::new(TokenPattern::Exact("static".to_string())),
                    // Type
                    PatternRule::new(TokenPattern::TypeKeyword),
                    // Variable name
                    PatternRule::new(TokenPattern::Identifier),
                    // Semicolon or assignment (not function call)
                    PatternRule::new(TokenPattern::OneOf(vec![";".to_string(), "=".to_string()]))
                        .forbid_next(vec![TokenPattern::Exact("(".to_string())]),
                ])
                .min_tokens(4)
                .priority(870);

        self.register_pattern(Id::get("static_variable"), static_variable_pattern);

        // Global constant pattern
        let global_constant_pattern: HandlerPattern =
            HandlerPattern::new(Id::get("global_constant"))
                .with_rules(vec![
                    // Const keyword
                    PatternRule::new(TokenPattern::Exact("const".to_string())),
                    // Type
                    PatternRule::new(TokenPattern::TypeKeyword),
                    // Variable name
                    PatternRule::new(TokenPattern::Identifier),
                    // Assignment or semicolon
                    PatternRule::new(TokenPattern::OneOf(vec!["=".to_string(), ";".to_string()])),
                ])
                .min_tokens(4)
                .priority(850);

        self.register_pattern(Id::get("global_constant"), global_constant_pattern);

        // Global variable pattern
        let global_variable_pattern: HandlerPattern =
            HandlerPattern::new(Id::get("global_variable"))
                .with_rules(vec![
                    // Type (not preceded by static, const, extern)
                    PatternRule::new(TokenPattern::TypeKeyword).with_validator(|tokens, pos| {
                        if pos > 0 {
                            let prev_token = tokens[pos - 1].to_string();
                            !matches!(prev_token.as_str(), "static" | "const" | "extern")
                        } else {
                            true
                        }
                    }),
                    // Variable name
                    PatternRule::new(TokenPattern::Identifier),
                    // Semicolon or assignment (not function)
                    PatternRule::new(TokenPattern::OneOf(vec![";".to_string(), "=".to_string()]))
                        .forbid_next(vec![TokenPattern::Exact("(".to_string())]),
                ])
                .min_tokens(3)
                .priority(800);

        self.register_pattern(Id::get("global_variable"), global_variable_pattern);

        // General "global" pattern - matches any global declaration
        // This is a catch-all pattern with lower priority for handlers that use generic "global" matching
        let global_pattern: HandlerPattern =
            HandlerPattern::new(Id::get("global"))
                .with_rules(vec![
                    // Optional storage class or qualifier
                    PatternRule::new(TokenPattern::OneOf(vec![
                        "extern".to_string(),
                        "static".to_string(),
                        "const".to_string(),
                        "volatile".to_string(),
                    ])).optional(),
                    // Type
                    PatternRule::new(TokenPattern::TypeKeyword),
                    // Variable name
                    PatternRule::new(TokenPattern::Identifier),
                    // Semicolon or assignment (not function)
                    PatternRule::new(TokenPattern::OneOf(vec![";".to_string(), "=".to_string()]))
                        .forbid_next(vec![TokenPattern::Exact("(".to_string())]),
                ])
                .min_tokens(3)
                .priority(790); // Slightly lower than specific patterns

        self.register_pattern(Id::get("global"), global_pattern);

        // Expression patterns (more restrictive)
        let binary_expression_pattern: HandlerPattern =
            HandlerPattern::new(Id::get("binary_expression"))
                .with_rules(vec![
                    // Left operand (not a type keyword)
                    PatternRule::new(TokenPattern::Identifier)
                        .forbid_next(vec![TokenPattern::Exact("(".to_string())]) // Reject function calls
                        .with_validator(|tokens, pos| {
                            // Custom validation: ensure this isn't a type declaration
                            if pos > 0 {
                                let prev_token = tokens[pos - 1].to_string();
                                ![
                                    "int", "char", "float", "double", "struct", "enum", "const",
                                    "static",
                                ]
                                    .contains(&prev_token.as_str())
                            } else {
                                true
                            }
                        }),
                    // Binary operator
                    PatternRule::new(TokenPattern::OneOf(vec![
                        "+".to_string(),
                        "-".to_string(),
                        "*".to_string(),
                        "/".to_string(),
                        "==".to_string(),
                        "!=".to_string(),
                        "<".to_string(),
                        ">".to_string(),
                        "<=".to_string(),
                        ">=".to_string(),
                        "&&".to_string(),
                        "||".to_string(),
                        "&".to_string(),
                        "|".to_string(),
                        "^".to_string(),
                        "<<".to_string(),
                        ">>".to_string(),
                    ])),
                    // Right operand
                    PatternRule::new(TokenPattern::OneOf(vec![
                        "identifier".to_string(),
                        "number".to_string(),
                    ])),
                ])
                .min_tokens(3)
                .priority(50); // Lower priority than functions/arrays

        self.register_pattern(Id::get("binary_expression"), binary_expression_pattern);
    }

    /// Check if tokens should be rejected by any handler (moved from standalone function)
    pub fn should_reject_tokens(&mut self, handler_type: &str, tokens: &[Token]) -> bool {
        match self.match_pattern(handler_type, tokens) {
            PatternResult::Reject { .. } => true,
            _ => false,
        }
    }

    /// Match patterns for multiple handler types and return the best match
    pub fn match_best_pattern(
        &mut self,
        handler_types: &[&str],
        tokens: &[Token],
    ) -> Option<(String, PatternResult)> {
        let mut best_match = None;
        let mut best_priority = -1;

        for handler_type in handler_types {
            match self.match_pattern(handler_type, tokens) {
                PatternResult::Match { consumed_tokens } => {
                    // Get the priority of the matched pattern
                    if let Some(patterns) = self.patterns.get(*handler_type) {
                        for pattern in patterns {
                            if pattern.priority > best_priority {
                                best_priority = pattern.priority;
                                best_match = Some((
                                    handler_type.to_string(),
                                    PatternResult::Match { consumed_tokens },
                                ));
                            }
                        }
                    }
                }
                result @ (PatternResult::CountOf { .. }
                | PatternResult::Sequence { .. }
                | PatternResult::Fuzzy { .. }) => {
                    // Consider partial matches if no full match found
                    if best_match.is_none() {
                        best_match = Some((handler_type.to_string(), result));
                    }
                }
                _ => {}
            }
        }

        best_match
    }

    /// Get all registered handler types
    pub fn get_handler_types(&self) -> Vec<String> {
        self.patterns.keys().cloned().collect()
    }

    /// Get patterns for a specific handler type
    pub fn get_patterns_for_handler(&self, handler_type: &str) -> Option<&Vec<HandlerPattern>> {
        self.patterns.get(handler_type)
    }

    /// Remove all patterns for a handler type
    pub fn remove_handler_patterns(&mut self, handler_type: &str) -> Option<Vec<HandlerPattern>> {
        self.patterns.remove(handler_type)
    }

    /// Get cache statistics
    pub fn get_cache_info(&self) -> (usize, usize) {
        (self.match_cache.len(), self.patterns.len())
    }

    /// Match tokens against patterns for a specific handler type
    pub fn match_pattern(&mut self, handler_type: &str, tokens: &[Token]) -> PatternResult {
        // Check cache first - use a more efficient cache key based on token hashes
        let token_hash = {
            let mut hasher = DefaultHasher::new();
            handler_type.hash(&mut hasher);
            tokens.len().hash(&mut hasher);
            // Hash first few and last few tokens to avoid long concatenations
            let sample_size = std::cmp::min(tokens.len(), 10);
            for token in tokens.iter().take(sample_size / 2) {
                token.to_string().hash(&mut hasher);
            }
            for token in tokens.iter().rev().take(sample_size / 2) {
                token.to_string().hash(&mut hasher);
            }
            hasher.finish()
        };
        let cache_key = format!("{}:{}", handler_type, token_hash);

        if let Some(cached_result) = self.match_cache.get(&cache_key) {
            return cached_result.clone();
        }

        // Get patterns for this handler type
        let patterns = match self.patterns.get(handler_type) {
            Some(patterns) => patterns,
            None => {
                let result: PatternResult = PatternResult::NoMatch {
                    reason: format!("No patterns registered for handler: {}", handler_type),
                };
                self.match_cache.insert(cache_key, result.clone());
                return result;
            }
        };

        // Try each pattern in priority order
        for pattern in patterns {
            let pattern_result: PatternResult = self.match_single_pattern(pattern, tokens);

            match pattern_result {
                PatternResult::Match { consumed_tokens } => {
                    let result: PatternResult = PatternResult::Match { consumed_tokens };
                    self.match_cache.insert(cache_key, result.clone());
                    return result;
                }
                PatternResult::CountOf { offsets } => {
                    let result: PatternResult = PatternResult::CountOf { offsets };
                    self.match_cache.insert(cache_key, result.clone());
                    return result;
                }
                PatternResult::Sequence { range } => {
                    let result: PatternResult = PatternResult::Sequence { range };
                    self.match_cache.insert(cache_key, result.clone());
                    return result;
                }
                PatternResult::Fuzzy { offsets } => {
                    let result: PatternResult = PatternResult::Fuzzy { offsets };
                    self.match_cache.insert(cache_key, result.clone());
                    return result;
                }
                PatternResult::Reject { reason } => {
                    let result: PatternResult = PatternResult::Reject { reason };
                    self.match_cache.insert(cache_key, result.clone());
                    return result;
                }
                PatternResult::NoMatch { .. } => {
                    // Continue to next pattern
                    continue;
                }
                // Handle new enhanced mismatch results
                PatternResult::TypeMismatch { .. } => {
                    // Continue to next pattern, but this could be cached as negative result
                    continue;
                }
                PatternResult::ValueMismatch { .. } => {
                    // Continue to next pattern, but this could be cached as negative result
                    continue;
                }
                PatternResult::StructureMismatch { .. } => {
                    // Continue to next pattern, but this could be cached as negative result
                    continue;
                }
                PatternResult::CachedNegative { .. } => {
                    // This is a negative cache hit - continue to next pattern
                    continue;
                }
                PatternResult::CachedPositive {
                    pattern_id,
                    cache_hit_count,
                    reason,
                } => {
                    // This is a positive cache hit - continue to next pattern
                    return PatternResult::CachedPositive {
                        pattern_id,
                        cache_hit_count: cache_hit_count + 1,
                        reason,
                    };
                }
            }
        }

        // No patterns matched
        let result = PatternResult::NoMatch {
            reason: format!("No patterns matched for handler: {}", handler_type),
        };
        self.match_cache.insert(cache_key, result.clone());
        result
    }

    /// Match tokens against a single pattern
    pub fn match_single_pattern(
        &self,
        pattern: &HandlerPattern,
        tokens: &[Token],
    ) -> PatternResult {
        // Check basic length requirements
        if tokens.len() < pattern.min_tokens {
            return PatternResult::NoMatch {
                reason: format!("Too few tokens: {} < {}", tokens.len(), pattern.min_tokens),
            };
        }

        if let Some(max) = pattern.max_tokens {
            if tokens.len() > max {
                return PatternResult::NoMatch {
                    reason: format!("Too many tokens: {} > {}", tokens.len(), max),
                };
            }
        }

        // Pre-validate CountOf patterns by counting occurrences across the entire sequence
        for rule in &pattern.rules {
            if let TokenPattern::CountOf(pattern_to_count, expected_count) = &rule.pattern {
                let actual_count = tokens
                    .iter()
                    .filter(|token| token.to_string() == *pattern_to_count)
                    .count();

                if actual_count != *expected_count {
                    return PatternResult::NoMatch {
                        reason: format!(
                            "CountOf validation failed: expected {} occurrences of '{}', found {}",
                            expected_count, pattern_to_count, actual_count
                        ),
                    };
                }
            }
        }

        // Pre-validate Pair patterns by checking delimiter pairing across the entire sequence
        for rule in &pattern.rules {
            if let TokenPattern::Pair = &rule.pattern {
                if let Err(err) = validate_delimiter_pairs(tokens) {
                    return PatternResult::NoMatch {
                        reason: err.to_string(),
                    };
                }
            }
        }

        let mut pos = 0;
        let mut consumed = 0;

        // Match each rule in sequence
        for (rule_idx, rule) in pattern.rules.iter().enumerate() {
            if pos >= tokens.len() {
                if rule.optional {
                    continue;
                } else {
                    return PatternResult::NoMatch {
                        reason: format!("End of tokens at rule {}", rule_idx),
                    };
                }
            }

            let token: &Token = &tokens[pos];

            // Check if current token matches the rule pattern
            if !rule.pattern.matches(token) {
                if rule.optional {
                    continue;
                } else {
                    return PatternResult::NoMatch {
                        reason: format!(
                            "Token '{}' doesn't match pattern at rule {}",
                            token.to_string(),
                            rule_idx
                        ),
                    };
                }
            }

            // Check custom validator if present
            if let Some(validator) = rule.custom_validator {
                if !validator(tokens, pos) {
                    return PatternResult::Reject {
                        reason: format!("Custom validator failed at position {}", pos),
                    };
                }
            }

            // Check forbidden next patterns
            if pos + 1 < tokens.len() {
                let next_token: &Token = &tokens[pos + 1];
                for forbidden_pattern in &rule.forbidden_next {
                    if forbidden_pattern.matches(next_token) {
                        return PatternResult::Reject {
                            reason: format!(
                                "Forbidden pattern '{}' found after '{}'",
                                next_token.to_string(),
                                token.to_string()
                            ),
                        };
                    }
                }

                // Check allowed next patterns (if specified)
                if !rule.allowed_next.is_empty() {
                    let mut allowed: bool = false;
                    for allowed_pattern in &rule.allowed_next {
                        if allowed_pattern.matches(next_token) {
                            allowed = true;
                            break;
                        }
                    }
                    if !allowed {
                        return PatternResult::Reject {
                            reason: format!(
                                "Token '{}' not in allowed next patterns after '{}'",
                                next_token.to_string(),
                                token.to_string()
                            ),
                        };
                    }
                }
            }

            pos += 1;
            consumed += 1;
        }

        PatternResult::Match {
            consumed_tokens: consumed,
        }
    }

    /// Clear the match cache (useful for testing or memory management)
    pub fn clear_cache(&mut self) {
        self.match_cache.clear();
    }

    /// Save pattern cache to JSON file
    pub fn save_cache_to_json(&self, file_path: &str) -> Result<()> {
        use std::fs;

        // Convert cache to JSON-serializable format
        let mut cache_json = object! {};

        for (key, result) in &self.match_cache {
            let result_json = self.pattern_result_to_json(result);
            cache_json[key] = result_json;
        }

        // Create metadata object
        let timestamp = match std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH) {
            Ok(duration) => format!("{}.{:09}", duration.as_secs(), duration.subsec_nanos()),
            Err(_) => "unknown".to_string(),
        };

        let metadata = object! {
            "version": Value::String("1.0".into()),
            "timestamp": Value::String(timestamp),
            "cache_size": Value::String(self.match_cache.len().to_string()),
            "pattern_count": Value::String(self.patterns.len().to_string())
        };

        let final_json = object! {
            "metadata": metadata,
            "cache": cache_json
        };

        // Write to file with proper error handling
        match fs::write(file_path, crate::json::stringify_pretty(final_json, 2)) {
            Ok(()) => {
                debug!("Pattern cache saved to: {}", file_path);
                info!(
                    "Saved {} cache entries to {}",
                    self.match_cache.len(),
                    file_path
                );
                Ok(())
            }
            Err(e) => {
                error!("Failed to save pattern cache to {}: {}", file_path, e);
                Err(e.into())
            }
        }
    }

    /// Convert PatternResult to JSON format
    fn pattern_result_to_json(&self, result: &PatternResult) -> Value {
        match result {
            PatternResult::Match { consumed_tokens } => {
                object! {
                    "type": Value::String("Match".to_string()),
                    "consumed_tokens": Value::String(consumed_tokens.to_string())
                }
            }
            PatternResult::NoMatch { reason } => {
                object! {
                    "type": Value::String("NoMatch".to_string()),
                    "reason": Value::String(reason.clone())
                }
            }
            PatternResult::CountOf { offsets } => {
                let offsets_array: Vec<Value> = offsets
                    .iter()
                    .map(|offset| {
                        Value::Array(vec![
                            Value::String(offset.start.to_string()),
                            Value::String(offset.end.to_string()),
                        ])
                    })
                    .collect();
                object! {
                    "type": Value::String("CountOf".to_string()),
                    "offsets": Value::Array(offsets_array)
                }
            }
            PatternResult::Sequence { range } => {
                object! {
                    "type": Value::String("Sequence".to_string()),
                    "start": Value::String(range.start.to_string()),
                    "end": Value::String(range.end.to_string())
                }
            }
            PatternResult::Fuzzy { offsets } => {
                let offsets_array: Vec<Value> = offsets
                    .iter()
                    .map(|offset| {
                        Value::Array(vec![
                            Value::String(offset.start.to_string()),
                            Value::String(offset.end.to_string()),
                        ])
                    })
                    .collect();
                object! {
                    "type": Value::String("Fuzzy".to_string()),
                    "offsets": Value::Array(offsets_array)
                }
            }
            PatternResult::Reject { reason } => {
                object! {
                    "type": Value::String("Reject".to_string()),
                    "reason": Value::String(reason.clone())
                }
            }
            PatternResult::TypeMismatch {
                expected_type,
                actual_type,
                position,
                reason,
            } => {
                object! {
                    "type": Value::String("TypeMismatch".to_string()),
                    "expected_type": Value::String(format!("{:?}", expected_type)),
                    "actual_type": Value::String(format!("{:?}", actual_type)),
                    "position": Value::String(position.to_string()),
                    "reason": Value::String(reason.clone())
                }
            }
            PatternResult::ValueMismatch {
                expected_value,
                actual_value,
                position,
                reason,
            } => {
                object! {
                    "type": Value::String("ValueMismatch".to_string()),
                    "expected_value": Value::String(expected_value.clone()),
                    "actual_value": Value::String(actual_value.clone()),
                    "position": Value::String(position.to_string()),
                    "reason": Value::String(reason.clone())
                }
            }
            PatternResult::StructureMismatch {
                expected_pattern,
                actual_structure,
                reason,
            } => {
                object! {
                    "type": Value::String("StructureMismatch".to_string()),
                    "expected_pattern": Value::String(expected_pattern.clone()),
                    "actual_structure": Value::String(actual_structure.clone()),
                    "reason": Value::String(reason.clone())
                }
            }
            PatternResult::CachedNegative {
                pattern_id,
                cache_hit_count,
                reason,
            } => {
                object! {
                    "type": Value::String("CachedNegative".to_string()),
                    "pattern_id": Value::String(pattern_id.clone()),
                    "cache_hit_count": Value::String(cache_hit_count.to_string()),
                    "reason": Value::String(reason.clone())
                }
            }
            PatternResult::CachedPositive {
                pattern_id,
                cache_hit_count,
                reason,
            } => {
                object! {
                    "type": Value::String("CachedPositive".to_string()),
                    "pattern_id": Value::String(pattern_id.clone()),
                    "cache_hit_count": Value::String(cache_hit_count.to_string()),
                    "reason": Value::String(reason.clone())
                }
            }
        }
    }
    /// Load pattern cache from JSON file
    pub fn load_cache_from_json(&mut self, file_path: &str) -> Result<()> {
        use std::fs;

        if !std::path::Path::new(file_path).exists() {
            info!("Pattern cache file not found: {}", file_path);
            return Ok(()); // Not an error - just no cache to load
        }

        let content = fs::read_to_string(file_path).map_err(|e| {
            C2RError::new(
                Kind::Io,
                Reason::Failed("to read cache file"),
                Some(e.to_string()),
            )
        })?;

        if content.trim().is_empty() {
            info!("Pattern cache file is empty: {}", file_path);
            return Ok(());
        }

        let json_data = crate::json::parse(&content).map_err(|e| {
            C2RError::new(
                Kind::Json,
                Reason::Invalid("JSON"),
                Some(format!("Failed to parse {}: {}", file_path, e)),
            )
        })?;

        // Validate structure
        if !json_data.is_object() {
            return Err(C2RError::new(
                Kind::Json,
                Reason::Invalid("JSON structure"),
                Some("Root must be an object".to_string()),
            ));
        }

        // Validate and log metadata
        if let Some(metadata) = json_data["metadata"].as_object() {
            if let Some(version) = metadata.get("version") {
                info!("Loading pattern cache version: {}", version);
            }
            if let Some(cache_size) = metadata.get("cache_size") {
                info!("Expected cache size: {}", cache_size);
            }
            if let Some(timestamp) = metadata.get("timestamp") {
                debug!("Cache timestamp: {}", timestamp);
            }
        } else {
            warn!("Pattern cache missing metadata section");
        }

        // Load cache entries with enhanced error handling
        if let Some(cache_obj) = json_data["cache"].as_object() {
            let mut loaded_count = 0;
            let mut failed_count = 0;

            for node in cache_obj.iter() {
                let key = node.key.as_str();
                match self.json_to_pattern_result(&node.value) {
                    Some(result) => {
                        self.match_cache.insert(key.to_string(), result);
                        loaded_count += 1;
                    }
                    None => {
                        debug!("Failed to parse cache entry for key: {}", key);
                        failed_count += 1;
                    }
                }
            }

            info!(
                "Loaded {} pattern cache entries from {}",
                loaded_count, file_path
            );
            if failed_count > 0 {
                warn!("Failed to parse {} cache entries", failed_count);
            }
        } else {
            warn!("Pattern cache missing cache section");
        }

        Ok(())
    }

    /// Convert JSON value to PatternResult
    fn json_to_pattern_result(&self, json_value: &Value) -> Option<PatternResult> {
        let result_type = json_value["type"].as_str()?;

        match result_type {
            "Match" => {
                let consumed_tokens = json_value["consumed_tokens"]
                    .as_str()?
                    .parse::<usize>()
                    .ok()?;
                Some(PatternResult::Match { consumed_tokens })
            }
            "NoMatch" => {
                let reason = json_value["reason"].as_str()?.to_string();
                Some(PatternResult::NoMatch { reason })
            }
            "CountOf" => {
                let offsets_array = json_value["offsets"].as_array()?;
                let offsets: Vec<Range<usize>> = offsets_array
                    .iter()
                    .filter_map(|v| {
                        let array = v.as_array()?;
                        if array.len() >= 2 {
                            let start = array[0].as_str()?.parse::<usize>().ok()?;
                            let end = array[1].as_str()?.parse::<usize>().ok()?;
                            Some(start..end)
                        } else {
                            None
                        }
                    })
                    .collect();
                Some(PatternResult::CountOf { offsets })
            }
            "Sequence" => {
                let start = json_value["start"].as_str()?.parse::<usize>().ok()?;
                let end = json_value["end"].as_str()?.parse::<usize>().ok()?;
                Some(PatternResult::Sequence { range: start..end })
            }
            "Fuzzy" => {
                let offsets_array = json_value["offsets"].as_array()?;
                let offsets: Vec<Range<usize>> = offsets_array
                    .iter()
                    .filter_map(|v| {
                        let array = v.as_array()?;
                        if array.len() >= 2 {
                            let start = array[0].as_str()?.parse::<usize>().ok()?;
                            let end = array[1].as_str()?.parse::<usize>().ok()?;
                            Some(start..end)
                        } else {
                            None
                        }
                    })
                    .collect();
                Some(PatternResult::Fuzzy { offsets })
            }
            "Reject" => {
                let reason = json_value["reason"].as_str()?.to_string();
                Some(PatternResult::Reject { reason })
            }
            "TypeMismatch" => {
                let expected_type = json_value["expected"].as_str()?;
                let actual_type = json_value["found"].as_str()?;
                let position = json_value["position"].as_str()?.parse::<usize>().ok()?;
                let reason = json_value["reason"].as_str()?.to_string();

                // Parse the expected_type and actual_type strings back to TokenTypeVariant
                let expected_variant = self.parse_token_type_variant(expected_type)?;
                let actual_variant = self.parse_token_type_variant(actual_type)?;

                Some(PatternResult::TypeMismatch {
                    expected_type: expected_variant,
                    actual_type: actual_variant,
                    position,
                    reason,
                })
            }
            "ValueMismatch" => {
                let expected_value = json_value["expected"].as_str()?.to_string();
                let actual_value = json_value["found"].as_str()?.to_string();
                let position = json_value["position"].as_str()?.parse::<usize>().ok()?;
                let reason = json_value["reason"].as_str()?.to_string();

                Some(PatternResult::ValueMismatch {
                    expected_value,
                    actual_value,
                    position,
                    reason,
                })
            }
            "StructureMismatch" => {
                let expected_pattern = json_value["expected_pattern"].as_str()?.to_string();
                let actual_structure = json_value["actual_structure"].as_str()?.to_string();
                let reason = json_value["reason"].as_str()?.to_string();

                Some(PatternResult::StructureMismatch {
                    expected_pattern,
                    actual_structure,
                    reason,
                })
            }
            "CachedNegative" => {
                let pattern_id = json_value["pattern_id"].as_str()?.to_string();
                let cache_hit_count = json_value["cache_hit_count"]
                    .as_str()?
                    .parse::<usize>()
                    .ok()?;
                let reason = json_value["reason"].as_str()?.to_string();
                Some(PatternResult::CachedNegative {
                    pattern_id,
                    cache_hit_count,
                    reason,
                })
            }
            _ => None,
        }
    }

    /// Helper function to parse TokenTypeVariant from string
    fn parse_token_type_variant(&self, type_str: &str) -> Option<TokenTypeVariant> {
        match type_str {
            "array" => Some(TokenTypeVariant::Array),
            "byte" => Some(TokenTypeVariant::Byte),
            "char" => Some(TokenTypeVariant::Char),
            "float" => Some(TokenTypeVariant::Float),
            "signed int" => Some(TokenTypeVariant::SignedInt),
            "string" => Some(TokenTypeVariant::String),
            "literal" => Some(TokenTypeVariant::Literal),
            "unsigned int" => Some(TokenTypeVariant::UnsignedInt),
            "vector" => Some(TokenTypeVariant::Vector),
            "whitespace" => Some(TokenTypeVariant::Whitespace),
            "delimiter" => Some(TokenTypeVariant::Delimiter),
            _ => None,
        }
    }
    pub fn process_handler_patterns(
        &mut self,
        tokens: &[Token],
        handler_type: &str,
    ) -> Result<f64> {
        let mut matched = 0.0;
        let mut total_tests = 0.0;

        // Test different pattern types based on handler
        match handler_type {
            "function_handler" => {
                if matches!(
                    self.match_pattern("function", tokens),
                    PatternResult::Match { .. }
                ) {
                    matched += 1.0;
                }
                total_tests += 1.0;

                if matches!(
                    self.match_pattern("function_declaration", tokens),
                    PatternResult::Match { .. }
                ) {
                    matched += 0.8; // Slightly lower weight
                }
                total_tests += 1.0;
            }
            "struct_handler" => {
                if matches!(
                    self.match_pattern("struct_definition", tokens),
                    PatternResult::Match { .. }
                ) {
                    matched += 1.0;
                }
                total_tests += 1.0;
            }
            "enum_handler" => {
                if matches!(
                    self.match_pattern("enum_definition", tokens),
                    PatternResult::Match { .. }
                ) {
                    matched += 1.0;
                }
                total_tests += 1.0;
            }
            "typedef_handler" => {
                if matches!(
                    self.match_pattern("typedef_definition", tokens),
                    PatternResult::Match { .. }
                ) {
                    matched += 1.0;
                }
                total_tests += 1.0;
            }
            "array_handler" => {
                if matches!(
                    self.match_pattern("array_declaration", tokens),
                    PatternResult::Match { .. }
                ) {
                    matched += 1.0;
                }
                total_tests += 1.0;
            }
            "global_handler" => {
                if matches!(
                    self.match_pattern("global_variable", tokens),
                    PatternResult::Match { .. }
                ) {
                    matched += 1.0;
                }
                total_tests += 1.0;
            }
            "expression_handler" => {
                if matches!(
                    self.match_pattern("expression_assignment", tokens),
                    PatternResult::Match { .. }
                ) {
                    matched += 1.0;
                }
                total_tests += 1.0;
            }
            _ => {
                // Default pattern testing
                total_tests = 1.0;
            }
        }

        if total_tests > 0.0 {
            Ok(matched / total_tests)
        } else {
            Ok(0.0)
        }
    }

    /// Get statistics about registered patterns
    pub fn get_stats(&self) -> HashMap<String, usize> {
        self.patterns
            .iter()
            .map(|(handler, patterns)| (handler.clone(), patterns.len()))
            .collect()
    }
}

/// Iterator implementation for Patternizer - allows iteration over different aspects
pub struct PatternIterator<'a> {
    patternizer: &'a Patternizer,
    mode: IterationMode,
    handler_keys: Vec<String>,
    current_handler_idx: usize,
    current_pattern_idx: usize,
}

/// Different modes for iterating over Patternizer
#[derive(Debug, Clone, PartialEq)]
pub enum IterationMode {
    /// Iterate over all handler types
    HandlerTypes,
    /// Iterate over all patterns across all handlers
    AllPatterns,
    /// Iterate over patterns for a specific handler type
    HandlerPatterns(String),
    /// Iterate over cached match results
    CachedResults,
}

impl<'a> PatternIterator<'a> {
    fn new(patternizer: &'a Patternizer, mode: IterationMode) -> Self {
        let handler_keys: Vec<String> = patternizer.patterns.keys().cloned().collect();
        Self {
            patternizer,
            mode,
            handler_keys,
            current_handler_idx: 0,
            current_pattern_idx: 0,
        }
    }
}

/// Iterator items based on iteration mode
#[derive(Debug, Clone)]
pub enum PatternIteratorItem<'a> {
    /// Handler type name
    HandlerType(String),
    /// Pattern with its handler type
    Pattern(&'a HandlerPattern, String),
    /// Cached match result with key
    CachedResult(String, &'a PatternResult),
}

impl<'a> Iterator for PatternIterator<'a> {
    type Item = PatternIteratorItem<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        match &self.mode {
            IterationMode::HandlerTypes => {
                if self.current_handler_idx < self.handler_keys.len() {
                    let handler_type: String = self.handler_keys[self.current_handler_idx].clone();
                    self.current_handler_idx += 1;
                    Some(PatternIteratorItem::HandlerType(handler_type))
                } else {
                    None
                }
            }
            IterationMode::AllPatterns => {
                // Iterate through all patterns across all handlers
                while self.current_handler_idx < self.handler_keys.len() {
                    let handler_type: &String = &self.handler_keys[self.current_handler_idx];
                    if let Some(patterns) = self.patternizer.patterns.get(handler_type) {
                        if self.current_pattern_idx < patterns.len() {
                            let pattern: &HandlerPattern = &patterns[self.current_pattern_idx];
                            self.current_pattern_idx += 1;
                            return Some(PatternIteratorItem::Pattern(
                                pattern,
                                handler_type.clone(),
                            ));
                        }
                    }
                    // Move to next handler
                    self.current_handler_idx += 1;
                    self.current_pattern_idx = 0;
                }
                None
            }
            IterationMode::HandlerPatterns(target_handler) => {
                if let Some(patterns) = self.patternizer.patterns.get(target_handler) {
                    if self.current_pattern_idx < patterns.len() {
                        let pattern: &HandlerPattern = &patterns[self.current_pattern_idx];
                        self.current_pattern_idx += 1;
                        return Some(PatternIteratorItem::Pattern(
                            pattern,
                            target_handler.clone(),
                        ));
                    }
                }
                None
            }
            IterationMode::CachedResults => {
                // Implementation for cached results iteration
                // This would require additional state tracking for cache keys
                // For now, return None (can be enhanced later)
                None
            }
        }
    }
}

impl Patternizer {
    /// Create an iterator over handler types
    pub fn iter_handler_types(&self) -> PatternIterator<'_> {
        PatternIterator::new(self, IterationMode::HandlerTypes)
    }

    /// Create an iterator over all patterns
    pub fn iter_all_patterns(&self) -> PatternIterator<'_> {
        PatternIterator::new(self, IterationMode::AllPatterns)
    }

    /// Create an iterator over patterns for a specific handler
    pub fn iter_handler_patterns(&self, handler_type: &str) -> PatternIterator<'_> {
        PatternIterator::new(
            self,
            IterationMode::HandlerPatterns(handler_type.to_string()),
        )
    }

    /// Create an iterator over cached results
    pub fn iter_cached_results(&self) -> PatternIterator<'_> {
        PatternIterator::new(self, IterationMode::CachedResults)
    }

    /// Collect all patterns with their handler types
    pub fn collect_all_patterns(&self) -> Vec<(String, &HandlerPattern)> {
        self.iter_all_patterns()
            .filter_map(|item: PatternIteratorItem<'_>| match item {
                PatternIteratorItem::Pattern(pattern, handler_type) => {
                    Some((handler_type, pattern))
                }
                _ => None,
            })
            .collect()
    }

    /// Find patterns matching a predicate
    pub fn find_patterns<F>(&self, predicate: F) -> Vec<(String, &HandlerPattern)>
    where
        F: Fn(&HandlerPattern) -> bool,
    {
        self.iter_all_patterns()
            .filter_map(|item: PatternIteratorItem<'_>| match item {
                PatternIteratorItem::Pattern(pattern, handler_type) => {
                    if predicate(pattern) {
                        Some((handler_type, pattern))
                    } else {
                        None
                    }
                }
                _ => None,
            })
            .collect()
    }

    /// Count patterns by priority level
    pub fn count_patterns_by_priority(&self) -> HashMap<i32, usize> {
        let mut priority_counts = HashMap::new();
        for item in self.iter_all_patterns() {
            if let PatternIteratorItem::Pattern(pattern, _) = item {
                *priority_counts.entry(pattern.priority).or_insert(0) += 1;
            }
        }
        priority_counts
    }

    /// Get all unique pattern IDs
    pub fn get_pattern_ids(&self) -> Vec<Id> {
        self.iter_all_patterns()
            .filter_map(|item: PatternIteratorItem<'_>| match item {
                PatternIteratorItem::Pattern(pattern, _) => Some(pattern.id.clone()),
                _ => None,
            })
            .collect::<Vec<Id>>()
    }
}

// === CONTEXT REGISTRY INTEGRATION FOR PATTERN CACHING ===

/// Enhanced pattern cache entry for storing both positive and negative matches
#[derive(Debug, Clone)]
pub struct CachedPatternMatch {
    pub pattern_id: String,
    pub token_sequence: Vec<String>,
    pub match_result: PatternResult,
    pub hit_count: u64,
    pub last_used: Instant,
    pub performance_metrics: PatternMetrics,
    /// Sequence fingerprint for fast comparison and better cache keys
    pub sequence_fingerprint: SequenceFingerprint,
    /// Whether this is a positive (match) or negative (no match) cache entry
    pub is_positive: bool,
    /// For negative cache entries, detailed reason why pattern didn't match
    pub negative_reason: Option<String>,
    /// Number of times this cache entry has been accessed
    pub access_count: usize,
    /// Hash of the pattern configuration for cache invalidation
    pub pattern_hash: u64,
}

/// Specialized negative cache entry for efficient storage of failed matches
#[derive(Debug, Clone)]
pub struct NegativeCacheEntry {
    pub pattern_id: String,
    pub sequence_signature: u64,
    pub mismatch_type: PatternResult,
    pub timestamp: Instant,
    pub access_count: usize,
}

/// Cache statistics for monitoring performance
#[derive(Debug, Clone)]
pub struct CacheStatistics {
    pub positive_hits: usize,
    pub negative_hits: usize,
    pub cache_misses: usize,
    pub total_lookups: usize,
    pub hit_ratio: f64,
    pub negative_hit_ratio: f64,
}

/// Performance metrics for pattern matching optimization
#[derive(Debug, Clone, PartialEq)]
pub struct PatternMetrics {
    pub total_matches: u64,
    pub total_misses: u64,
    pub average_match_time: Duration,
    pub cache_hit_ratio: f64,
}

impl Default for PatternMetrics {
    fn default() -> Self {
        Self {
            total_matches: 0,
            total_misses: 0,
            average_match_time: Duration::from_nanos(0),
            cache_hit_ratio: 0.0,
        }
    }
}

/// Pattern engine for multi-token pattern matching (matches Tokenizer naming convention)
#[derive(Debug, Clone, PartialEq)]
pub struct Pattern {
    pub id: Id,
    pub name: String,
    pub description: String,
    pub token_patterns: Vec<TokenPattern>,
    pub priority: u32,
    pub handler_types: Vec<String>,
    pub created_at: Instant,
    pub usage_metrics: PatternMetrics,
}
impl Pattern {
    pub fn new(
        id: Id,
        name: String,
        description: String,
        token_patterns: Vec<TokenPattern>,
        priority: u32,
        handler_types: Vec<String>,
        created_at: Instant,
        usage_metrics: PatternMetrics,
    ) -> Self {
        Pattern {
            id,
            name,
            description,
            token_patterns,
            priority,
            handler_types,
            created_at,
            usage_metrics,
        }
    }
    pub fn id(&self) -> &Id {
        &self.id
    }
    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn description(&self) -> &str {
        &self.description
    }
    pub fn token_patterns(&self) -> &Vec<TokenPattern> {
        &self.token_patterns
    }
    pub fn priority(&self) -> u32 {
        self.priority
    }
    pub fn handler_types(&self) -> &Vec<String> {
        &self.handler_types
    }
    pub fn created_at(&self) -> Instant {
        self.created_at
    }
    pub fn usage_metrics(&self) -> &PatternMetrics {
        &self.usage_metrics
    }
}

impl Default for Pattern {
    fn default() -> Self {
        Pattern {
            id: Id::get("default_pattern"),
            name: "default_pattern".to_string(),
            description: "A pattern for the Patternizer to process".to_string(),
            token_patterns: Vec::new(),
            priority: 100,
            handler_types: Vec::new(),
            created_at: Instant::now(),
            usage_metrics: PatternMetrics::default(),
        }
    }
}

impl Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

// === CONTEXT REGISTRY INTEGRATION FUNCTIONS ===

/// Store a pattern in the context registry (simplified with set_value)
pub fn store_pattern(context: &mut Context, pattern_id: &str, pattern: Pattern) {
    // Use the same key format that get_pattern() expects: "pattern_" + pattern_id
    let pattern_key: String = format!("pattern_{}", pattern_id);

    // Use GlobalContext::with_context for proper shared access to the same global instance
    context.set_entry(Id::get(&pattern_key), Entry::Pattern(pattern.clone()));
    println!(
        "📋 Stored pattern '{}' in registry with key '{}'",
        pattern.id.name(),
        pattern_key
    );
}

/// Cache a pattern match result (both positive and negative) in the context registry
pub fn cache_pattern_match(
    context: &mut Context,
    pattern_id: &str,
    cached_match: CachedPatternMatch,
) {
    let cache_key: String = format!("pattern_cache_{}", pattern_id);
    // Extract consumed_tokens from the match_result
    let consumed_tokens = match &cached_match.match_result {
        PatternResult::Match { consumed_tokens } => *consumed_tokens,
        _ => 0,
    };

    let cache_data: Entry = Entry::StrMap(
        [
            (
                "pattern_id".to_string(),
                Entry::Str(cached_match.pattern_id),
            ),
            (
                "token_sequence".to_string(),
                Entry::StrMap(
                    cached_match
                        .token_sequence
                        .iter()
                        .enumerate()
                        .map(|(i, t)| (i.to_string(), Entry::Str(t.clone())))
                        .collect(),
                ),
            ),
            (
                "consumed_tokens".to_string(),
                Entry::Val(consumed_tokens as u64),
            ),
            ("hit_count".to_string(), Entry::Val(cached_match.hit_count)),
            (
                "total_matches".to_string(),
                Entry::Val(cached_match.performance_metrics.total_matches),
            ),
            (
                "total_misses".to_string(),
                Entry::Val(cached_match.performance_metrics.total_misses),
            ),
            (
                "cache_hit_ratio".to_string(),
                Entry::Str(format!(
                    "{:.2}",
                    cached_match.performance_metrics.cache_hit_ratio
                )),
            ),
        ]
            .into_iter()
            .collect(),
    );
    context.registry.set_value(&cache_key, cache_data);
    println!("🔍 Cache stored for key: '{}'", cache_key);
}

/// Retrieve a cached pattern match from the context registry
pub fn get_cached_pattern_match(
    context: &mut Context,
    pattern_id: &str,
    token_sequence: &[Token],
) -> Option<PatternResult> {
    let cache_key = format!("pattern_cache_{}", pattern_id);
    println!("🔍 Looking for cache entry with key: '{}'", cache_key);

    println!("🔍 Registry accessed for cache lookup");
    let cache_entry = context.get_entry(&Id::get(&cache_key)).clone();

    if let Some(entry) = cache_entry {
        println!("✅ Cache entry found for key '{}'", cache_key);
        match entry {
            Entry::StrMap(cache_data) => {
                println!("✅ Cache entry is StrMap with {} fields", cache_data.len());
                // Verify token sequence matches
                if let Some(Entry::StrMap(cached_tokens_map)) = cache_data.get("token_sequence") {
                    println!(
                        "✅ Found cached token sequence with {} tokens",
                        cached_tokens_map.len()
                    );
                    let token_strings: Vec<String> =
                        token_sequence.iter().map(|t| t.to_string()).collect();

                    // Extract tokens from StrMap by sorting indices
                    let mut indexed_tokens: Vec<(usize, String)> = cached_tokens_map
                        .iter()
                        .filter_map(|(key, value)| {
                            let index = key.parse::<usize>().ok()?;
                            match value {
                                Entry::Str(s) => Some((index, s.clone())),
                                _ => None,
                            }
                        })
                        .collect();
                    indexed_tokens.sort_by_key(|(index, _)| *index);
                    let cached_token_strings: Vec<String> =
                        indexed_tokens.into_iter().map(|(_, token)| token).collect();

                    println!("🔍 Current tokens: {}", token_strings.join(", "));
                    println!("🔍 Cached tokens: {}", cached_token_strings.join(", "));

                    if token_strings == cached_token_strings {
                        println!(
                            "🎯 CACHE HIT! Returning cached result for pattern '{}'",
                            pattern_id
                        );
                        // Return the cached consumed_tokens value, not the full sequence length
                        if let Some(Entry::Val(cached_consumed)) = cache_data.get("consumed_tokens")
                        {
                            Some(PatternResult::Match {
                                consumed_tokens: *cached_consumed as usize,
                            })
                        } else {
                            // Fallback if no consumed_tokens found (should not happen)
                            Some(PatternResult::Match {
                                consumed_tokens: token_sequence.len(),
                            })
                        }
                    } else {
                        println!("❌ Token sequences don't match - cache miss");
                        None
                    }
                } else {
                    println!("❌ No token_sequence found in cache data");
                    None
                }
            }
            _ => {
                println!("❌ Cache entry is not StrMap: {:?}", entry);
                None
            }
        }
    } else {
        println!("❌ No cache entry found for key '{}'", cache_key);
        None
    }
}

/// Cache a negative pattern match result (pattern that failed to match)
pub fn cache_negative_result(
    context: &mut Context,
    pattern_id: &str,
    tokens: &[Token],
    mismatch_result: PatternResult,
) {
    let sequence_fingerprint: SequenceFingerprint = generate_sequence_fingerprint(tokens);
    let cache_key: String = format!("negative_cache_{}", pattern_id);

    let negative_entry: NegativeCacheEntry = NegativeCacheEntry {
        pattern_id: pattern_id.to_string(),
        sequence_signature: sequence_fingerprint.signature,
        mismatch_type: mismatch_result,
        timestamp: Instant::now(),
        access_count: 1,
    };

    println!(
        "🚫 Caching negative result for pattern '{}' with signature {}",
        pattern_id, sequence_fingerprint.signature
    );

    // Store as a list of negative entries for this pattern
    let current_negatives: Vec<Entry> = context
        .get_entry(&Id::get(&cache_key))
        .and_then(|entry| match entry {
            Entry::List(list) => Some(list.clone()),
            _ => None,
        })
        .unwrap_or_else(Vec::new);

    let mut updated_negatives: Vec<Entry> = current_negatives;
    updated_negatives.push(Entry::Str(format!(
        "{}:{}",
        sequence_fingerprint.signature, negative_entry.pattern_id
    )));

    context
        .registry
        .set_value(&cache_key, Entry::List(updated_negatives));
}

/// Check if a token sequence is in the negative cache for a pattern
pub fn check_negative_cache(
    context: &mut Context,
    pattern_id: &str,
    tokens: &[Token],
) -> Option<PatternResult> {
    // ULTRA-SAFE: Skip negative cache entirely if tokens look corrupted
    if tokens.len() > 1000 {
        eprintln!(
            "⚠️  WARNING: Skipping negative cache for oversized token sequence ({} tokens)",
            tokens.len()
        );
        return None;
    }

    // Quick corruption check on first few tokens
    for (i, token) in tokens.iter().enumerate().take(5) {
        if std::panic::catch_unwind(|| {
            let _ = format!("{:?}", token);
            let _ = token.to_string();
        })
            .is_err()
        {
            eprintln!(
                "🚨 CRITICAL: Corrupted token detected at index {}, disabling negative cache for this sequence",
                i
            );
            return None;
        }
    }

    let sequence_fingerprint: SequenceFingerprint = generate_sequence_fingerprint(tokens);
    let cache_key: String = format!("negative_cache_{}", pattern_id);

    if let Some(Entry::List(negative_entries)) = context.get_entry(&Id::get(&cache_key)) {
        let signature_str: String = sequence_fingerprint.signature.to_string();

        for entry in negative_entries {
            if let Entry::Str(entry_str) = entry {
                if entry_str.starts_with(&signature_str) {
                    println!(
                        "🚫 NEGATIVE CACHE HIT! Pattern '{}' known not to match signature {}",
                        pattern_id, sequence_fingerprint.signature
                    );
                    return Some(PatternResult::CachedNegative {
                        pattern_id: pattern_id.to_string(),
                        cache_hit_count: 1,
                        reason: format!(
                            "Previously failed to match token sequence with signature {}",
                            sequence_fingerprint.signature
                        ),
                    });
                }
            }
        }
    }
    None
}

/// Get cache statistics for monitoring performance
pub fn get_cache_statistics(context: &mut Context) -> CacheStatistics {
    let mut positive_hits: usize = 0;
    let mut negative_hits: usize = 0;
    let mut total_entries: usize = 0;

    context
        .registry
        .entries()
        .into_iter()
        .for_each(|(id, entry)| {
            let id_str = id.name();
            if id_str.starts_with("pattern_cache_") {
                positive_hits += 1;
                total_entries += 1;
            } else if id_str.starts_with("negative_cache_") {
                if let Entry::List(entries) = entry {
                    negative_hits += entries.len();
                    total_entries += entries.len();
                }
            }
        });
    let hit_ratio = if total_entries > 0 {
        positive_hits as f64 / total_entries as f64
    } else {
        0.0
    };

    let negative_hit_ratio = if total_entries > 0 {
        negative_hits as f64 / total_entries as f64
    } else {
        0.0
    };

    CacheStatistics {
        positive_hits,
        negative_hits,
        cache_misses: 0, // Would need more sophisticated tracking
        total_lookups: total_entries,
        hit_ratio,
        negative_hit_ratio,
    }
}

/// Register common multi-token patterns with intelligent caching
pub fn register_common_multi_token_patterns(context: &mut Context) {
    // C function declaration pattern: "type identifier ( params )"
    let function_pattern = Pattern {
        id: Id::get("c_function_declaration"),
        name: "C Function Declaration".to_string(),
        description: "Matches C function declarations and definitions".to_string(),
        token_patterns: vec![
            TokenPattern::TypeKeyword,
            TokenPattern::Identifier,
            TokenPattern::Exact("(".to_string()),
            TokenPattern::Any, // Parameters
            TokenPattern::Exact(")".to_string()),
        ],
        priority: 100,
        handler_types: vec!["function".to_string()],
        created_at: Instant::now(),
        usage_metrics: PatternMetrics::default(),
    };
    context.registry.insert_pattern("function", function_pattern);

    // Struct declaration pattern: "struct identifier { ... }"
    let struct_pattern = Pattern {
        id: Id::get("c_struct_declaration"),
        name: "C Struct Declaration".to_string(),
        description: "Matches C struct declarations and definitions".to_string(),
        token_patterns: vec![
            TokenPattern::Exact("struct".to_string()),
            TokenPattern::Identifier,
            TokenPattern::Exact("{".to_string()),
            TokenPattern::Any, // Fields
            TokenPattern::Exact("}".to_string()),
        ],
        priority: 95,
        handler_types: vec!["struct".to_string()],
        created_at: Instant::now(),
        usage_metrics: PatternMetrics::default(),
    };
    context.registry.insert_pattern("struct", struct_pattern);

    // Enum declaration pattern: "enum identifier { ... }"
    let enum_pattern = Pattern {
        id: Id::get("c_enum_declaration"),
        name: "C Enum Declaration".to_string(),
        description: "Matches C enum declarations and definitions".to_string(),
        token_patterns: vec![
            TokenPattern::Exact("enum".to_string()),
            TokenPattern::Identifier,
            TokenPattern::Exact("{".to_string()),
            TokenPattern::Any, // Values
            TokenPattern::Exact("}".to_string()),
        ],
        priority: 90,
        handler_types: vec!["enum".to_string()],
        created_at: Instant::now(),
        usage_metrics: PatternMetrics::default(),
    };
    context.registry.insert_pattern("enum", enum_pattern);

    // Array declaration pattern: "type identifier [ size ]"
    let array_pattern = Pattern {
        id: Id::get("array_declaration"),
        name: "Array Declaration".to_string(),
        description: "Matches C array declarations".to_string(),
        token_patterns: vec![
            TokenPattern::TypeKeyword,
            TokenPattern::Identifier,
            TokenPattern::Exact("[".to_string()),
            TokenPattern::Any, // Size expression
            TokenPattern::Exact("]".to_string()),
        ],
        priority: 85,
        handler_types: vec!["array".to_string()],
        created_at: Instant::now(),
        usage_metrics: PatternMetrics::default(),
    };
    context.registry.insert_pattern("array", array_pattern);

    // If statement pattern: "if ( condition )"
    let if_pattern = Pattern {
        id: Id::get("c_if_statement"),
        name: "C If Statement".to_string(),
        description: "Matches C if statements".to_string(),
        token_patterns: vec![
            TokenPattern::Exact("if".to_string()),
            TokenPattern::Exact("(".to_string()),
            TokenPattern::Any, // Condition
            TokenPattern::Exact(")".to_string()),
        ],
        priority: 80,
        handler_types: vec!["if".to_string()],
        created_at: Instant::now(),
        usage_metrics: PatternMetrics::default(),
    };
    context.registry.insert_pattern("if", if_pattern);
}

/// Enhanced pattern matching with context registry caching (simplified with get_value)
pub fn match_pattern_with_registry_cache(
    context: &mut Context,
    pattern_name: &str,
    tokens: &[Token],
) -> PatternResult {
    let start_time: Instant = Instant::now();

    // Check negative cache first - fastest path
    if let Some(negative_result) = check_negative_cache(context, pattern_name, tokens) {
        return negative_result;
    }

    // Check positive cache
    if let Some(cached_result) = get_cached_pattern_match(context, pattern_name, tokens) {
        return cached_result;
    }

    // Look up the pattern using the new dedicated get_pattern function
    match context.registry.get_pattern(pattern_name) {
        Some(pattern) => {
            // Match this pattern against the tokens
            let result: PatternResult = match_multi_token_pattern(&pattern, tokens);
            let match_time: Duration = start_time.elapsed();

            match &result {
                PatternResult::Match { .. } => {
                    // Cache successful matches
                    let sequence_fingerprint = generate_sequence_fingerprint(tokens);
                    let mut hasher = DefaultHasher::new();
                    pattern_name.hash(&mut hasher);
                    let pattern_hash = hasher.finish();

                    let cached_match = CachedPatternMatch {
                        pattern_id: pattern_name.to_string(),
                        token_sequence: tokens.iter().map(|t| t.to_string()).collect(),
                        match_result: result.clone(),
                        hit_count: 1,
                        last_used: Instant::now(),
                        performance_metrics: PatternMetrics {
                            total_matches: 1,
                            total_misses: 0,
                            average_match_time: match_time,
                            cache_hit_ratio: 1.0,
                        },
                        sequence_fingerprint,
                        is_positive: true,
                        negative_reason: None,
                        access_count: 1,
                        pattern_hash,
                    };
                    cache_pattern_match(context, pattern_name, cached_match);
                }
                _ => {
                    // Cache negative results for failed matches
                    cache_negative_result(context, pattern_name, tokens, result.clone());
                }
            }
            result
        }
        None => {
            let no_match_result: PatternResult = PatternResult::NoMatch {
                reason: format!("Pattern '{}' not registered", pattern_name),
            };
            // Cache this negative result too
            cache_negative_result(context, pattern_name, tokens, no_match_result.clone());
            no_match_result
        }
    }
}
/// Match a Patternizer against a token sequence
fn match_multi_token_pattern(pattern: &Pattern, tokens: &[Token]) -> PatternResult {
    println!(
        "🔍 Matching pattern '{}' against {} tokens",
        pattern.name,
        tokens.len()
    );

    // For now, implement a simple pattern matching logic based on the pattern ID
    // In a full implementation, we would use the token_patterns vector for sophisticated matching
    match pattern.name.as_str() {
        "function_declaration" => {
            // Look for: type identifier ( ... )
            if tokens.len() >= 4 {
                let has_type: bool = matches!(
                    tokens[0].to_string().as_str(),
                    "int" | "void" | "char" | "float" | "double" | "long" | "short" | "unsigned"
                );
                let has_identifier: bool =
                    !tokens[1].to_string().contains('(') && !tokens[1].to_string().contains(')');
                let has_open_paren: bool = tokens[2].to_string() == "(";

                if has_type && has_identifier && has_open_paren {
                    // Find closing paren
                    for (i, token) in tokens.iter().enumerate().skip(3) {
                        if token.to_string() == ")" {
                            return PatternResult::Match {
                                consumed_tokens: i + 1,
                            };
                        }
                    }
                }
            }
        }
        "include_statement" => {
            // Look for: # include <...> or # include "..."
            if tokens.len() >= 2
                && tokens[0].to_string() == "#"
                && tokens[1].to_string() == "include"
            {
                return PatternResult::Match {
                    consumed_tokens: tokens.len().min(5),
                };
            }
        }
        "array_declaration" => {
            // Look for: type identifier [ size ]
            if tokens.len() >= 5 {
                let has_type: bool = matches!(
                    tokens[0].to_string().as_str(),
                    "int" | "char" | "float" | "double"
                );
                let has_identifier: bool = !tokens[1].to_string().contains('[');
                let has_open_bracket: bool = tokens[2].to_string() == "[";

                if has_type && has_identifier && has_open_bracket {
                    // Find closing bracket
                    for (i, token) in tokens.iter().enumerate().skip(3) {
                        if token.to_string() == "]" {
                            return PatternResult::Match {
                                consumed_tokens: i + 1,
                            };
                        }
                    }
                }
            }
        }
        "complex_identifier" => {
            // Look for identifiers with underscores, dashes, etc.
            if tokens.len() >= 1 {
                let token_str = tokens[0].to_string();
                if token_str.contains('_') || token_str.len() > 8 {
                    return PatternResult::Match { consumed_tokens: 1 };
                }
            }
        }
        _ => {}
    }

    PatternResult::NoMatch {
        reason: format!("Pattern '{}' did not match token sequence", pattern.name),
    }
}
/// Helper function for pattern matching using context patternizer (moved from BaseHandler)
pub fn match_patterns(
    context: &mut Context,
    tokens: &[Token],
    patterns: &Vec<String>,
) -> bool {
    // Use both registry cache AND patternizer cache for complete coverage
    for pattern in patterns {
        // First try registry cache (fast)
        let result = match_pattern_with_registry_cache(context, pattern, tokens);
        if matches!(result, PatternResult::Match { .. }) {
            return true;
        }
    }

    // If no registry cache hit, try patternizer directly to build cache
    for pattern in patterns {
        let result = context.patternizer.match_pattern(pattern, tokens);
        if matches!(result, PatternResult::Match { .. }) {
            return true;
        }
    }

    false
}
/// Match a registry-stored pattern against tokens
#[allow(dead_code)]
fn match_registry_pattern_against_tokens(
    pattern_data: &HashMap<String, Entry>,
    tokens: &[Token],
) -> PatternResult {
    // Extract pattern information from the registry data
    let pattern_id = match pattern_data.get("id") {
        Some(Entry::Str(id)) => id.clone(),
        _ => "unknown".to_string(),
    };
    let _description = match pattern_data.get("description") {
        Some(Entry::Str(desc)) => desc.clone(),
        _ => "".to_string(),
    };

    // For now, implement a simple pattern matching logic based on the pattern ID
    // In a full implementation, we would deserialize the full Patternizer and use it
    match pattern_id.as_str() {
        "function_declaration" => {
            // Look for: type identifier ( ... )
            if tokens.len() >= 4 {
                let has_type: bool = matches!(
                    tokens[0].to_string().as_str(),
                    "int" | "void" | "char" | "float" | "double" | "long" | "short" | "unsigned"
                );
                let has_identifier: bool =
                    !tokens[1].to_string().contains('(') && !tokens[1].to_string().contains(')');
                let has_open_paren: bool = tokens[2].to_string() == "(";

                if has_type && has_identifier && has_open_paren {
                    // Find closing paren
                    for (i, token) in tokens.iter().enumerate().skip(3) {
                        if token.to_string() == ")" {
                            return PatternResult::Match {
                                consumed_tokens: i + 1,
                            };
                        }
                    }
                }
            }
        }
        "include_statement" => {
            // Look for: # include <...> or # include "..."
            if tokens.len() >= 2
                && tokens[0].to_string() == "#"
                && tokens[1].to_string() == "include"
            {
                return PatternResult::Match {
                    consumed_tokens: tokens.len().min(5),
                };
            }
        }
        "array_declaration" => {
            // Look for: type identifier [ size ]
            if tokens.len() >= 5 {
                let has_type = matches!(
                    tokens[0].to_string().as_str(),
                    "int" | "char" | "float" | "double"
                );
                let has_identifier = !tokens[1].to_string().contains('[');
                let has_open_bracket = tokens[2].to_string() == "[";

                if has_type && has_identifier && has_open_bracket {
                    // Find closing bracket
                    for (i, token) in tokens.iter().enumerate().skip(3) {
                        if token.to_string() == "]" {
                            return PatternResult::Match {
                                consumed_tokens: i + 1,
                            };
                        }
                    }
                }
            }
        }
        "complex_identifier" => {
            // Look for identifiers with underscores, dashes, etc.
            if tokens.len() >= 1 {
                let token_str: String = tokens[0].to_string();
                if token_str.contains('_') || token_str.len() > 8 {
                    return PatternResult::Match { consumed_tokens: 1 };
                }
            }
        }
        _ => {}
    }

    PatternResult::NoMatch {
        reason: format!("Pattern '{}' did not match token sequence", pattern_id),
    }
}
