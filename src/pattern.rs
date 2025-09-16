// =============================================================================
// PATTERN MATCHING UTILITY - Lightweight cascading token validation system
// =============================================================================

use crate::config::Global;
use crate::convert::{is_c_keyword, is_type_token};
use crate::entry::Entry;
use crate::lock::Id;
use crate::token::Token;
use core::option::Option::None;
use std::collections::HashMap;
use std::collections::hash_map::DefaultHasher;
use std::fmt::Display;
use std::hash::{Hash, Hasher};
use std::ops::Range;
use std::time::{Duration, Instant};

/// Represents different types of token patterns for validation
#[derive(Debug, Clone, Eq, Hash)]
pub enum TokenPattern {
    /// Exact token match
    Exact(String),
    /// Any token from a set of options
    OneOf(Vec<String>),
    /// A certain number of tokens from a set of options
    CountOf(Vec<String>, usize),
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

/// Represents the result of pattern matching
#[derive(Debug, Clone, PartialEq, Eq)]
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
}

impl std::fmt::Display for PatternResult {
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
#[derive(Debug, Clone, Eq)]
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HandlerPattern {
    /// Unique identifier for this pattern
    pub id: String,
    /// Human-readable description
    pub description: String,
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
#[derive(Debug, Clone, Eq)]
pub struct Patternizer {
    pub name: String,
    /// Registered patterns by handler type
    patterns: HashMap<String, Vec<HandlerPattern>>,
    /// Cache for performance optimization
    match_cache: HashMap<String, PatternResult>,
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
impl Default for Patternizer {
    fn default() -> Self {
        Patternizer {
            name: "patternizer".to_string(),
            patterns: HashMap::new(),
            match_cache: HashMap::new(),
        }
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

/// Generate a TokenFingerprint for efficient caching and comparison
pub fn generate_token_fingerprint(token: &Token) -> TokenFingerprint {
    let token_str = token.to_string();
    let mut hasher = DefaultHasher::new();
    token_str.hash(&mut hasher);
    let value_hash = hasher.finish();

    let first_char = token_str.chars().next();
    let last_char = token_str.chars().last();

    TokenFingerprint {
        token_type: get_token_type_variant(token),
        value_hash,
        length: token_str.len(),
        properties: TokenProperties {
            is_numeric: token.is_numeric(),
            is_integer: token.is_int(),
            is_float: token.is_float(),
            is_whitespace: token.is_whitespace(),
            first_char,
            last_char,
        },
    }
}

/// Generate a SequenceFingerprint for caching entire token sequences
pub fn generate_sequence_fingerprint(tokens: &[Token]) -> SequenceFingerprint {
    let token_fingerprints: Vec<TokenFingerprint> =
        tokens.iter().map(generate_token_fingerprint).collect();

    // Generate a compact signature by combining fingerprint hashes
    let mut hasher = DefaultHasher::new();
    for fingerprint in &token_fingerprints {
        fingerprint.hash(&mut hasher);
    }
    let signature = hasher.finish();

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
            TokenPattern::CountOf(options, count) => {
                options
                    .iter()
                    .filter(|&option| option == &token_str)
                    .count()
                    == *count
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
                is_type_token(token)
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
                !token_str.is_empty() && token_str.chars().all(|c| c.is_ascii_digit() || c == '.')
            }
            TokenPattern::Any => true,
            TokenPattern::End => false,

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
                let first_char = chars.next().unwrap();

                (first_char.is_alphanumeric() || first_char == '_')
                    && chars.all(|c| c.is_alphanumeric() || c == '_' || allowed_chars.contains(&c))
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
            let prefix = &pattern[..pattern.len() - 1];
            return text.starts_with(prefix);
        }

        // Handle suffix wildcard: "*suffix"
        if pattern.starts_with('*') && !pattern.ends_with('*') && !pattern.contains('[') {
            let suffix = &pattern[1..];
            return text.ends_with(suffix);
        }

        // Handle contains wildcard: "*substring*"
        if pattern.starts_with('*')
            && pattern.ends_with('*')
            && pattern.len() > 2
            && !pattern.contains('[')
        {
            let substring = &pattern[1..pattern.len() - 1];
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
                    let class_end = pattern_idx + class_end;
                    let class_content: String =
                        pattern[pattern_idx + 1..class_end].iter().collect();

                    let negated = class_content.starts_with('!');
                    let chars_to_match = if negated {
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

        let mut reconstructed = String::new();
        let mut consumed = 0;
        let mut i = start_idx;

        // Common fragmentation patterns in C tokenization
        let path_separators = ['/', '\\', '.'];
        let identifier_chars = ['-', '_'];

        while i < tokens.len() {
            let token_str = tokens[i].to_string();

            // Add current token
            reconstructed.push_str(&token_str);
            consumed += 1;
            i += 1;

            // Check if next token could be part of fragmented sequence
            if i < tokens.len() {
                let next_token = tokens[i].to_string();

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
    pub fn new(id: String, description: String) -> Self {
        Self {
            id,
            description,
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
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            patterns: HashMap::new(),
            match_cache: HashMap::new(),
        }
    }

    /// Create a new pattern matcher with common patterns pre-loaded
    pub fn with_common_patterns() -> Self {
        let mut patternizer = Self::new("patternizer");
        patternizer.initialize_common_patterns();
        patternizer
    }

    /// Register a pattern for a handler type
    pub fn register_pattern(&mut self, handler_type: String, pattern: HandlerPattern) {
        self.patterns
            .entry(handler_type.clone())
            .or_insert_with(Vec::new)
            .push(pattern);

        // Sort by priority (highest first)
        if let Some(patterns) = self.patterns.get_mut(&handler_type) {
            patterns.sort_by(|a, b| b.priority.cmp(&a.priority));
        }
    }

    /// Initialize common patterns for all handlers (moved from standalone function)
    pub fn initialize_common_patterns(&mut self) {
        // Function patterns
        let function_pattern = HandlerPattern::new(
            "function_declaration".to_string(),
            "C function declaration or definition".to_string(),
        )
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

        self.register_pattern("function".to_string(), function_pattern);

        // Array patterns - improved with multiple variations
        let array_declaration_pattern = HandlerPattern::new(
            "array_declaration".to_string(),
            "C array declaration with brackets".to_string(),
        )
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

        self.register_pattern("array_declaration".to_string(), array_declaration_pattern);

        // Multi-dimensional array pattern
        let array_multi_pattern = HandlerPattern::new(
            "array_multi".to_string(),
            "C multi-dimensional array declaration".to_string(),
        )
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

        self.register_pattern("array_multi".to_string(), array_multi_pattern);

        // Struct patterns
        let struct_declaration_pattern = HandlerPattern::new(
            "struct_declaration".to_string(),
            "C struct declaration or definition".to_string(),
        )
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

        self.register_pattern("struct_declaration".to_string(), struct_declaration_pattern);

        // Struct definition pattern (with body)
        let struct_definition_pattern = HandlerPattern::new(
            "struct_definition".to_string(),
            "C struct definition with body".to_string(),
        )
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

        self.register_pattern("struct_definition".to_string(), struct_definition_pattern);

        // Enum patterns
        let enum_declaration_pattern = HandlerPattern::new(
            "enum_declaration".to_string(),
            "C enum declaration".to_string(),
        )
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

        self.register_pattern("enum_declaration".to_string(), enum_declaration_pattern);

        // Typedef enum pattern
        let typedef_enum_pattern = HandlerPattern::new(
            "typedef_enum".to_string(),
            "C typedef enum declaration".to_string(),
        )
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

        self.register_pattern("typedef_enum".to_string(), typedef_enum_pattern);

        // Typedef patterns
        let typedef_simple_pattern = HandlerPattern::new(
            "typedef_simple".to_string(),
            "C simple typedef declaration".to_string(),
        )
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

        self.register_pattern("typedef_simple".to_string(), typedef_simple_pattern);

        // Typedef struct pattern (not captured by typedef_enum)
        let typedef_struct_pattern = HandlerPattern::new(
            "typedef_struct".to_string(),
            "C typedef struct declaration".to_string(),
        )
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

        self.register_pattern("typedef_struct".to_string(), typedef_struct_pattern);

        // Control flow patterns
        let control_flow_if_pattern =
            HandlerPattern::new("control_flow_if".to_string(), "C if statement".to_string())
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

        self.register_pattern("control_flow_if".to_string(), control_flow_if_pattern);

        let control_flow_for_pattern =
            HandlerPattern::new("control_flow_for".to_string(), "C for loop".to_string())
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

        self.register_pattern("control_flow_for".to_string(), control_flow_for_pattern);

        let control_flow_while_pattern =
            HandlerPattern::new("control_flow_while".to_string(), "C while loop".to_string())
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

        self.register_pattern("control_flow_while".to_string(), control_flow_while_pattern);

        // Include directive patterns
        let include_directive_pattern = HandlerPattern::new(
            "include_directive".to_string(),
            "C #include preprocessor directive".to_string(),
        )
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

        self.register_pattern("include_directive".to_string(), include_directive_pattern);

        // Define macro patterns
        let define_macro_pattern = HandlerPattern::new(
            "define_macro".to_string(),
            "C #define preprocessor directive".to_string(),
        )
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

        self.register_pattern("define_macro".to_string(), define_macro_pattern);

        // Conditional macro patterns
        let conditional_macro_pattern = HandlerPattern::new(
            "conditional_macro".to_string(),
            "C conditional preprocessor directives".to_string(),
        )
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

        self.register_pattern("conditional_macro".to_string(), conditional_macro_pattern);

        // Main function pattern (very specific)
        let main_function_pattern = HandlerPattern::new(
            "main_function".to_string(),
            "C main function declaration or definition".to_string(),
        )
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

        self.register_pattern("main_function".to_string(), main_function_pattern);

        // Function declaration pattern
        let function_declaration_pattern = HandlerPattern::new(
            "function_declaration".to_string(),
            "C function declaration (no body)".to_string(),
        )
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
            "function_declaration".to_string(),
            function_declaration_pattern,
        );

        // Function definition pattern
        let function_definition_pattern = HandlerPattern::new(
            "function_definition".to_string(),
            "C function definition (with body)".to_string(),
        )
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

        self.register_pattern(
            "function_definition".to_string(),
            function_definition_pattern,
        );

        // Extern declaration pattern
        let extern_declaration_pattern = HandlerPattern::new(
            "extern_declaration".to_string(),
            "C extern declaration".to_string(),
        )
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

        self.register_pattern("extern_declaration".to_string(), extern_declaration_pattern);

        // Static variable pattern
        let static_variable_pattern = HandlerPattern::new(
            "static_variable".to_string(),
            "C static variable declaration".to_string(),
        )
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

        self.register_pattern("static_variable".to_string(), static_variable_pattern);

        // Global constant pattern
        let global_constant_pattern = HandlerPattern::new(
            "global_constant".to_string(),
            "C global constant declaration".to_string(),
        )
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

        self.register_pattern("global_constant".to_string(), global_constant_pattern);

        // Global variable pattern
        let global_variable_pattern = HandlerPattern::new(
            "global_variable".to_string(),
            "C global variable declaration".to_string(),
        )
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

        self.register_pattern("global_variable".to_string(), global_variable_pattern);

        // Expression patterns (more restrictive)
        let binary_expression_pattern = HandlerPattern::new(
            "binary_expression".to_string(),
            "Binary expression (not function call)".to_string(),
        )
        .with_rules(vec![
            // Left operand (not a type keyword)
            PatternRule::new(TokenPattern::Identifier)
                .forbid_next(vec![TokenPattern::Exact("(".to_string())]) // Reject function calls
                .with_validator(|tokens, pos| {
                    // Custom validation: ensure this isn't a type declaration
                    if pos > 0 {
                        let prev_token = tokens[pos - 1].to_string();
                        ![
                            "int", "char", "float", "double", "struct", "enum", "const", "static",
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

        self.register_pattern("expression".to_string(), binary_expression_pattern);
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
        // Check cache first
        let cache_key = format!(
            "{}:{}",
            handler_type,
            tokens
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join("|")
        );

        if let Some(cached_result) = self.match_cache.get(&cache_key) {
            return cached_result.clone();
        }

        // Get patterns for this handler type
        let patterns = match self.patterns.get(handler_type) {
            Some(patterns) => patterns,
            None => {
                let result = PatternResult::NoMatch {
                    reason: format!("No patterns registered for handler: {}", handler_type),
                };
                self.match_cache.insert(cache_key, result.clone());
                return result;
            }
        };

        // Try each pattern in priority order
        for pattern in patterns {
            let pattern_result = self.match_single_pattern(pattern, tokens);

            match pattern_result {
                PatternResult::Match { consumed_tokens } => {
                    let result = PatternResult::Match { consumed_tokens };
                    self.match_cache.insert(cache_key, result.clone());
                    return result;
                }
                PatternResult::CountOf { offsets } => {
                    let result = PatternResult::CountOf { offsets };
                    self.match_cache.insert(cache_key, result.clone());
                    return result;
                }
                PatternResult::Sequence { range } => {
                    let result = PatternResult::Sequence { range };
                    self.match_cache.insert(cache_key, result.clone());
                    return result;
                }
                PatternResult::Fuzzy { offsets } => {
                    let result = PatternResult::Fuzzy { offsets };
                    self.match_cache.insert(cache_key, result.clone());
                    return result;
                }
                PatternResult::Reject { reason } => {
                    let result = PatternResult::Reject { reason };
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
    fn match_single_pattern(&self, pattern: &HandlerPattern, tokens: &[Token]) -> PatternResult {
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

            let token = &tokens[pos];

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
                let next_token = &tokens[pos + 1];
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
                    let mut allowed = false;
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
                    let handler_type = self.handler_keys[self.current_handler_idx].clone();
                    self.current_handler_idx += 1;
                    Some(PatternIteratorItem::HandlerType(handler_type))
                } else {
                    None
                }
            }
            IterationMode::AllPatterns => {
                // Iterate through all patterns across all handlers
                while self.current_handler_idx < self.handler_keys.len() {
                    let handler_type = &self.handler_keys[self.current_handler_idx];
                    if let Some(patterns) = self.patternizer.patterns.get(handler_type) {
                        if self.current_pattern_idx < patterns.len() {
                            let pattern = &patterns[self.current_pattern_idx];
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
                        let pattern = &patterns[self.current_pattern_idx];
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
            .filter_map(|item| match item {
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
            .filter_map(|item| match item {
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
    pub fn get_pattern_ids(&self) -> Vec<String> {
        self.iter_all_patterns()
            .filter_map(|item| match item {
                PatternIteratorItem::Pattern(pattern, _) => Some(pattern.id.clone()),
                _ => None,
            })
            .collect()
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

    // === NEW: ENHANCED CACHING FEATURES ===
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
/* <<<<<<<<<<<<<<  ✨ Windsurf Command 🌟 >>>>>>>>>>>>>>>> */
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
/* <<<<<<<<<<  06a28124-6dea-4e1f-b292-1ffa0ea94961  >>>>>>>>>>> */
// === CONTEXT REGISTRY INTEGRATION FUNCTIONS ===

/// Store a pattern in the context registry (simplified with set_value)
pub fn store_pattern(pattern: Pattern) {
    // Use the same key format that get_pattern() expects: "pattern_" + pattern_id
    let pattern_key = format!("pattern_{}", pattern.id.name());

    // Use GlobalContext::with_context for proper shared access to the same global instance
    Global::write(|ctx| {
        let entry = Entry::pattern(pattern.clone());
        ctx.set_entry(&pattern_key, entry);
        println!(
            "📋 Stored pattern '{}' in registry with key '{}'",
            pattern.id.name(),
            pattern_key
        );
    });
}

/// Cache a pattern match result (both positive and negative) in the context registry
pub fn cache_pattern_match(cached_match: CachedPatternMatch) {
    let cache_key = format!("pattern_cache_{}", cached_match.pattern_id);
    // Extract consumed_tokens from the match_result
    let consumed_tokens = match &cached_match.match_result {
        PatternResult::Match { consumed_tokens } => *consumed_tokens,
        _ => 0,
    };

    let cache_data = Entry::StrMap(
        [
            (
                "pattern_id".to_string(),
                Entry::Str(cached_match.pattern_id),
            ),
            (
                "token_sequence".to_string(),
                Entry::List(
                    cached_match
                        .token_sequence
                        .iter()
                        .map(|t| Entry::Str(t.clone()))
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

    // Use shared context for consistent cache storage
    use crate::config::Global;
    Global::write(|ctx| {
        ctx.set_entry(&cache_key, cache_data.clone());
    });
}

/// Retrieve a cached pattern match from the context registry
pub fn get_cached_pattern_match(
    pattern_id: &str,
    token_sequence: &[Token],
) -> Option<PatternResult> {
    let cache_key = format!("pattern_cache_{}", pattern_id);

    let cache_entry = Global::write(|ctx| ctx.get_entry(&cache_key).cloned());

    if let Some(cache_entry) = cache_entry {
        println!("🔍 Cache entry found for key '{}'", cache_key);
        match cache_entry {
            crate::entry::Entry::StrMap(cache_data) => {
                println!("✅ Cache entry is StrMap with {} fields", cache_data.len());
                // Verify token sequence matches
                if let Some(crate::entry::Entry::List(cached_tokens)) =
                    cache_data.get("token_sequence")
                {
                    println!(
                        "✅ Found cached token sequence with {} tokens",
                        cached_tokens.len()
                    );
                    let token_strings: Vec<String> =
                        token_sequence.iter().map(|t| t.to_string()).collect();
                    let cached_token_strings: Vec<String> = cached_tokens
                        .iter()
                        .filter_map(|e| match e {
                            crate::entry::Entry::Str(s) => Some(s.clone()),
                            _ => None,
                        })
                        .collect();

                    println!("🔍 Current tokens: {}", token_strings.join(", "));
                    println!("🔍 Cached tokens: {}", cached_token_strings.join(", "));

                    if token_strings == cached_token_strings {
                        println!(
                            "🎯 CACHE HIT! Returning cached result for pattern '{}'",
                            pattern_id
                        );
                        // Return the cached consumed_tokens value, not the full sequence length
                        if let Some(crate::entry::Entry::Val(cached_consumed)) =
                            cache_data.get("consumed_tokens")
                        {
                            return Some(PatternResult::Match {
                                consumed_tokens: *cached_consumed as usize,
                            });
                        } else {
                            // Fallback if no consumed_tokens found (should not happen)
                            return Some(PatternResult::Match {
                                consumed_tokens: token_sequence.len(),
                            });
                        }
                    } else {
                        println!("❌ Token sequences don't match - cache miss");
                    }
                } else {
                    println!("❌ No token_sequence found in cache data");
                }
            }
            _ => {
                println!("❌ Cache entry is not StrMap: {}", cache_entry);
            }
        }
    }

    None
}

/// Cache a negative pattern match result (pattern that failed to match)
pub fn cache_negative_result(pattern_id: &str, tokens: &[Token], mismatch_result: PatternResult) {
    let sequence_fingerprint = generate_sequence_fingerprint(tokens);
    let cache_key = format!("negative_cache_{}", pattern_id);

    let negative_entry = NegativeCacheEntry {
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

    Global::write(|ctx| {
        // Store as a list of negative entries for this pattern
        let current_negatives = ctx
            .get_entry(&cache_key)
            .and_then(|entry| match entry {
                Entry::List(list) => Some(list.clone()),
                _ => None,
            })
            .unwrap_or_else(Vec::new);

        let mut updated_negatives = current_negatives;
        updated_negatives.push(Entry::Str(format!(
            "{}:{}",
            sequence_fingerprint.signature, negative_entry.pattern_id
        )));

        ctx.set_entry(&cache_key, Entry::List(updated_negatives));
    });
}

/// Check if a token sequence is in the negative cache for a pattern
pub fn check_negative_cache(pattern_id: &str, tokens: &[Token]) -> Option<PatternResult> {
    use crate::config::Global;

    let sequence_fingerprint = generate_sequence_fingerprint(tokens);
    let cache_key = format!("negative_cache_{}", pattern_id);

    Global::write(|ctx| {
        if let Some(Entry::List(negative_entries)) = ctx.get_entry(&cache_key) {
            let signature_str = sequence_fingerprint.signature.to_string();

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
    })
}

/// Get cache statistics for monitoring performance
pub fn get_cache_statistics() -> CacheStatistics {
    use crate::config::Global;

    let mut positive_hits = 0;
    let mut negative_hits = 0;
    let mut total_entries = 0;

    Global::write(|ctx| {
        // Count positive cache entries
        for i in 0..1000 {
            // Check first 1000 potential cache keys
            let cache_key = format!("pattern_cache_{}", i);
            if ctx.get_entry(&cache_key).is_some() {
                positive_hits += 1;
            }
        }

        // Count negative cache entries
        for i in 0..1000 {
            // Check first 1000 potential negative cache keys
            let negative_key = format!("negative_cache_{}", i);
            if let Some(Entry::List(entries)) = ctx.get_entry(&negative_key) {
                negative_hits += entries.len();
            }
        }

        total_entries = positive_hits + negative_hits;
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
pub fn register_common_multi_token_patterns() {
    let mut context = crate::context!();

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
    context.store_pattern("function", function_pattern);

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
    context.store_pattern("struct", struct_pattern);

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
    context.store_pattern("enum", enum_pattern);

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
    context.store_pattern("array", array_pattern);

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
    context.store_pattern("if", if_pattern);
}

/// Enhanced pattern matching with context registry caching (simplified with get_value)
pub fn match_pattern_with_registry_cache(pattern_id: &str, tokens: &[Token]) -> PatternResult {
    let start_time = Instant::now();

    // Check negative cache first - fastest path
    if let Some(negative_result) = check_negative_cache(pattern_id, tokens) {
        return negative_result;
    }

    // Check positive cache
    if let Some(cached_result) = get_cached_pattern_match(pattern_id, tokens) {
        return cached_result;
    }

    // Look up the pattern using the new dedicated get_pattern function
    match crate::context!().get_pattern(pattern_id) {
        Some(pattern) => {
            // Match this pattern against the tokens
            let result = match_multi_token_pattern(pattern, tokens);
            let match_time = start_time.elapsed();

            match &result {
                PatternResult::Match { .. } => {
                    // Cache successful matches
                    let sequence_fingerprint = generate_sequence_fingerprint(tokens);
                    let mut hasher = std::collections::hash_map::DefaultHasher::new();
                    pattern_id.hash(&mut hasher);
                    let pattern_hash = hasher.finish();

                    let cached_match = CachedPatternMatch {
                        pattern_id: pattern_id.to_string(),
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
                    cache_pattern_match(cached_match);
                }
                _ => {
                    // Cache negative results for failed matches
                    cache_negative_result(pattern_id, tokens, result.clone());
                }
            }
            result
        }
        None => {
            let no_match_result = PatternResult::NoMatch {
                reason: format!("Pattern '{}' not registered", pattern_id),
            };
            // Cache this negative result too
            cache_negative_result(pattern_id, tokens, no_match_result.clone());
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
        "c_function_declaration" => {
            // Look for: type identifier ( ... )
            if tokens.len() >= 4 {
                let has_type = matches!(
                    tokens[0].to_string().as_str(),
                    "int" | "void" | "char" | "float" | "double" | "long" | "short" | "unsigned"
                );
                let has_identifier =
                    !tokens[1].to_string().contains('(') && !tokens[1].to_string().contains(')');
                let has_open_paren = tokens[2].to_string() == "(";

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
        "c_include_statement" => {
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
        "c_array_declaration" => {
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

/// Match a registry-stored pattern against tokens
#[allow(dead_code)]
fn match_registry_pattern_against_tokens(
    pattern_data: &HashMap<String, crate::entry::Entry>,
    tokens: &[Token],
) -> PatternResult {
    // Extract pattern information from the registry data
    let pattern_id = match pattern_data.get("id") {
        Some(crate::entry::Entry::Str(id)) => id.clone(),
        _ => "unknown".to_string(),
    };
    let _description = match pattern_data.get("description") {
        Some(crate::entry::Entry::Str(desc)) => desc.clone(),
        _ => "".to_string(),
    };

    // For now, implement a simple pattern matching logic based on the pattern ID
    // In a full implementation, we would deserialize the full Patternizer and use it
    match pattern_id.as_str() {
        "c_function_declaration" => {
            // Look for: type identifier ( ... )
            if tokens.len() >= 4 {
                let has_type = matches!(
                    tokens[0].to_string().as_str(),
                    "int" | "void" | "char" | "float" | "double" | "long" | "short" | "unsigned"
                );
                let has_identifier =
                    !tokens[1].to_string().contains('(') && !tokens[1].to_string().contains(')');
                let has_open_paren = tokens[2].to_string() == "(";

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
        "c_include_statement" => {
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
        "c_array_declaration" => {
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
                let token_str = tokens[0].to_string();
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

/// Create a new Patternizer instance with common patterns (recommended for standalone use cases)
pub fn create_patternizer_with_common_patterns() -> Patternizer {
    Patternizer::with_common_patterns()
}

// === CONTEXT-BASED PATTERN ACCESS FUNCTIONS ===
// These functions use the GlobalContext struct methods to access the patternizer

/// Initialize common patterns in the global context patternizer
pub fn initialize_common_patterns() {
    Global::write(|ctx| ctx.patternizer.initialize_common_patterns());
}

/// Match patterns for a handler using the global context patternizer  
pub fn match_handler_pattern(handler_type: &str, tokens: &[Token]) -> PatternResult {
    Global::write(|ctx| ctx.patternizer.match_pattern(handler_type, tokens))
}

/// Check if tokens should be rejected by any handler using the global context patternizer
pub fn should_reject_tokens(handler_type: &str, tokens: &[Token]) -> bool {
    Global::write(|ctx| ctx.patternizer.should_reject_tokens(handler_type, tokens))
}

/// Get pattern statistics from the global context patternizer (read-only)
pub fn get_pattern_stats() -> std::collections::HashMap<String, usize> {
    Global::write(|ctx| ctx.patternizer.get_stats())
}
