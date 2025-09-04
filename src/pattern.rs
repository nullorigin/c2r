// =============================================================================
// PATTERN MATCHING UTILITY - Lightweight cascading token validation system
// =============================================================================

use crate::Global;
use std::collections::HashMap;
use std::ops::Range;
use std::time::{Duration, Instant};
use crate::{context, is_c_keyword, is_type_token, Context, Entry, Id, Token};

/// Represents different types of token patterns for validation
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
        separators: Vec<char>
    },
}

/// Represents the result of pattern matching
#[derive(Debug, Clone, PartialEq,Eq)]
pub enum PatternResult {
    /// Pattern matches successfully
    Match { consumed_tokens: usize },
    /// a given number of tokens match in the correct order but at offset indices.
    CountOf{ offsets:Vec<Range<usize>> },
    /// a series of tokens in order match but not all
    Sequence{range:Range<usize>},
    /// a number or more match, but they are out of order somewhat
    Fuzzy{ offsets:Vec<Range<usize>>},
    /// Pattern does not match
    NoMatch { reason: String },
    /// Pattern explicitly rejects this sequence
    Reject { reason: String },
}

/// A single pattern matching rule
#[derive(Debug, Clone,PartialEq,Eq)]
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

/// A complete pattern matching configuration for a handler
#[derive(Debug, Clone,PartialEq,Eq)]
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
#[derive(Debug,Clone,Eq)]
pub struct Patternizer {
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
            patterns: HashMap::new(),
            match_cache: HashMap::new(),
        }
    }
}
impl TokenPattern {
    /// Check if a token matches this pattern
    pub fn matches(&self, token: &Token) -> bool {
        let token_str = token.to_string();

        match self {
            TokenPattern::Exact(expected) => token_str == *expected,
            TokenPattern::OneOf(options) => options.contains(&token_str),
            TokenPattern::CountOf(options, count) => {
                options.iter().filter(|&option| option == &token_str).count() == *count
            },
            TokenPattern::NotOneOf(forbidden) => !forbidden.contains(&token_str),
            TokenPattern::Identifier => {
                !token_str.is_empty() &&
                    !token_str.chars().all(|c| c.is_ascii_punctuation()) &&
                    token_str.chars().next().map_or(false, |c| c.is_alphanumeric())
            },
            TokenPattern::TypeKeyword => {
                is_type_token(token) ||
                    matches!(token_str.as_str(), "struct" | "enum" | "union" | "typedef" | "const" | "static" | "extern" | "inline")
            },
            TokenPattern::CKeyword => is_c_keyword(token.clone()),
            TokenPattern::Punctuation => {
                !token_str.is_empty() && token_str.chars().all(|c| c.is_ascii_punctuation())
            },
            TokenPattern::Number => {
                !token_str.is_empty() && token_str.chars().all(|c| c.is_ascii_digit() || c == '.')
            },
            TokenPattern::Any => true,
            TokenPattern::End => false,

            // Advanced substring matching
            TokenPattern::SubstringContains(substring) => token_str.contains(substring),
            TokenPattern::SubstringContainsAny(substrings) => {
                substrings.iter().any(|s| token_str.contains(s))
            },
            TokenPattern::PathLike => {
                token_str.contains('/') || token_str.contains('\\') ||
                    (token_str.contains('.') &&
                        (token_str.ends_with(".c") || token_str.ends_with(".h") ||
                            token_str.ends_with(".cpp") || token_str.ends_with(".hpp") ||
                            token_str.contains("./") || token_str.contains("../")))
            },
            TokenPattern::IdentifierWithChars(allowed_chars) => {
                if token_str.is_empty() { return false; }

                let mut chars = token_str.chars();
                let first_char = chars.next().unwrap();

                (first_char.is_alphanumeric() || first_char == '_') &&
                    chars.all(|c| c.is_alphanumeric() || c == '_' || allowed_chars.contains(&c))
            },
            TokenPattern::OperatorInIdentifier(op_char) => {
                token_str.len() > 1 &&
                    token_str.contains(*op_char) &&
                    !token_str.chars().all(|c| c.is_ascii_punctuation()) &&
                    token_str.chars().any(|c| c.is_alphanumeric() || c == '_')
            },
            TokenPattern::StartsWith(prefix) => token_str.starts_with(prefix),
            TokenPattern::EndsWith(suffix) => token_str.ends_with(suffix),
            TokenPattern::RegexPattern(pattern) => {
                Self::simple_regex_match(&token_str, pattern)
            },
            TokenPattern::FragmentedToken { parts, separators: _ } => {
                parts.iter().any(|part| token_str == *part)
            }
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
        if pattern.starts_with('*') && pattern.ends_with('*') && pattern.len() > 2 && !pattern.contains('[') {
            let substring = &pattern[1..pattern.len() - 1];
            return text.contains(substring);
        }

        // Enhanced pattern matching with character classes and ? wildcards
        Self::advanced_pattern_match(text.chars().collect(), pattern.chars().collect(), 0, 0)
    }

    /// Advanced pattern matching supporting ?, *, and character classes [abc] or [!abc]
    fn advanced_pattern_match(text: Vec<char>, pattern: Vec<char>, text_idx: usize, pattern_idx: usize) -> bool {
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
                Self::advanced_pattern_match(text.clone(), pattern.clone(), text_idx, pattern_idx + 1) ||
                Self::advanced_pattern_match(text, pattern, text_idx + 1, pattern_idx)
            }
            '?' => {
                // Match exactly one character
                Self::advanced_pattern_match(text, pattern, text_idx + 1, pattern_idx + 1)
            }
            '[' => {
                // Character class matching
                if let Some(class_end) = pattern[pattern_idx..].iter().position(|&c| c == ']') {
                    let class_end = pattern_idx + class_end;
                    let class_content: String = pattern[pattern_idx + 1..class_end].iter().collect();

                    let negated = class_content.starts_with('!');
                    let chars_to_match = if negated { &class_content[1..] } else { &class_content };

                    let matches = chars_to_match.contains(text[text_idx]);
                    let result = if negated { !matches } else { matches };

                    if result {
                        Self::advanced_pattern_match(text, pattern, text_idx + 1, class_end + 1)
                    } else {
                        false
                    }
                } else {
                    // Malformed character class, treat as literal
                    text[text_idx] == '[' &&
                    Self::advanced_pattern_match(text, pattern, text_idx + 1, pattern_idx + 1)
                }
            }
            c => {
                // Literal character match
                text[text_idx] == c &&
                Self::advanced_pattern_match(text, pattern, text_idx + 1, pattern_idx + 1)
            }
        }
    }

    /// Detect if a token sequence represents a fragmented path or identifier
    pub fn detect_fragmented_sequence(tokens: &[Token], start_idx: usize) -> Option<(String, usize)> {
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
                if (token_str.ends_with('.') && (next_token == "/" || next_token.starts_with('/'))) ||
                    (path_separators.contains(&token_str.chars().last().unwrap_or(' ')) &&
                        next_token.chars().next().unwrap_or(' ').is_alphanumeric()) ||
                    (token_str.chars().all(|c| c.is_alphanumeric() || c == '_') &&
                        next_token.len() == 1 && path_separators.contains(&next_token.chars().next().unwrap())) {
                    continue; // Keep consuming tokens
                }

                // Identifier continuation: "multi-word", "snake_case"
                if (token_str.chars().all(|c| c.is_alphanumeric() || c == '_') &&
                    next_token.len() == 1 && identifier_chars.contains(&next_token.chars().next().unwrap())) ||
                    (token_str.len() == 1 && identifier_chars.contains(&token_str.chars().next().unwrap()) &&
                        next_token.chars().next().unwrap_or(' ').is_alphanumeric()) {
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
    pub fn new() -> Self {
        Self {
            patterns: HashMap::new(),
            match_cache: HashMap::new(),
        }
    }

    /// Create a new pattern matcher with common patterns pre-loaded
    pub fn with_common_patterns() -> Self {
        let mut patternizer = Self::new();
        patternizer.initialize_common_patterns();
        patternizer
    }

    /// Register a pattern for a handler type
    pub fn register_pattern(&mut self, handler_type: String, pattern: HandlerPattern) {
        self.patterns.entry(handler_type.clone())
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
            "C function declaration or definition".to_string()
        )
        .with_rules(vec![
            // Return type (optional static/inline/extern)
            PatternRule::new(TokenPattern::OneOf(vec!["static".to_string(), "inline".to_string(), "extern".to_string()]))
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

        // Array patterns
        let array_declaration_pattern = HandlerPattern::new(
            "array_declaration".to_string(),
            "C array declaration".to_string()
        )
        .with_rules(vec![
            // Type
            PatternRule::new(TokenPattern::TypeKeyword)
                .forbid_next(vec![TokenPattern::Exact("(".to_string())]), // Reject function calls
            // Identifier
            PatternRule::new(TokenPattern::Identifier)
                .allow_next(vec![TokenPattern::Exact("[".to_string())]), // Must be followed by [
            // Opening bracket
            PatternRule::new(TokenPattern::Exact("[".to_string())),
            // Size (number or identifier)
            PatternRule::new(TokenPattern::OneOf(vec!["number".to_string(), "identifier".to_string()])),
            // Closing bracket
            PatternRule::new(TokenPattern::Exact("]".to_string())),
        ])
        .min_tokens(5)
        .priority(150);

        self.register_pattern("array".to_string(), array_declaration_pattern);

        // Expression patterns (more restrictive)
        let binary_expression_pattern = HandlerPattern::new(
            "binary_expression".to_string(),
            "Binary expression (not function call)".to_string()
        )
        .with_rules(vec![
            // Left operand (not a type keyword)
            PatternRule::new(TokenPattern::Identifier)
                .forbid_next(vec![TokenPattern::Exact("(".to_string())]) // Reject function calls
                .with_validator(|tokens, pos| {
                    // Custom validation: ensure this isn't a type declaration
                    if pos > 0 {
                        let prev_token = tokens[pos - 1].to_string();
                        !["int", "char", "float", "double", "struct", "enum", "const", "static"]
                            .contains(&prev_token.as_str())
                    } else {
                        true
                    }
                }),
            // Binary operator
            PatternRule::new(TokenPattern::OneOf(vec![
                "+".to_string(), "-".to_string(), "*".to_string(), "/".to_string(),
                "==".to_string(), "!=".to_string(), "<".to_string(), ">".to_string(),
                "<=".to_string(), ">=".to_string(), "&&".to_string(), "||".to_string(),
                "&".to_string(), "|".to_string(), "^".to_string(), "<<".to_string(), ">>".to_string()
            ])),
            // Right operand
            PatternRule::new(TokenPattern::OneOf(vec!["identifier".to_string(), "number".to_string()])),
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
    pub fn match_best_pattern(&mut self, handler_types: &[&str], tokens: &[Token]) -> Option<(String, PatternResult)> {
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
                                best_match = Some((handler_type.to_string(), PatternResult::Match { consumed_tokens }));
                            }
                        }
                    }
                }
                result @ (PatternResult::CountOf { .. } | PatternResult::Sequence { .. } | PatternResult::Fuzzy { .. }) => {
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
        let cache_key = format!("{}:{}", handler_type, tokens.iter()
            .map(|t| t.to_string()).collect::<Vec<_>>().join("|"));

        if let Some(cached_result) = self.match_cache.get(&cache_key) {
            return cached_result.clone();
        }

        // Get patterns for this handler type
        let patterns = match self.patterns.get(handler_type) {
            Some(patterns) => patterns,
            None => {
                let result = PatternResult::NoMatch {
                    reason: format!("No patterns registered for handler: {}", handler_type)
                };
                self.match_cache.insert(cache_key, result.clone());
                return result;
            }
        };

        // Try each pattern in priority order
        for pattern in patterns {
            match self.match_single_pattern(pattern, tokens) {
                PatternResult::Match { consumed_tokens } => {
                    let result = PatternResult::Match { consumed_tokens };
                    self.match_cache.insert(cache_key, result.clone());
                    return result;
                },
                PatternResult::CountOf { offsets } => {
                    let result = PatternResult::CountOf { offsets };
                    self.match_cache.insert(cache_key, result.clone());
                    return result;
                },
                PatternResult::Sequence { range } => {
                    let result = PatternResult::Sequence { range };
                    self.match_cache.insert(cache_key, result.clone());
                    return result;
                },
                PatternResult::Fuzzy { offsets } => {
                    let result = PatternResult::Fuzzy { offsets };
                    self.match_cache.insert(cache_key, result.clone());
                    return result;
                },
                PatternResult::Reject { reason } => {
                    let result = PatternResult::Reject { reason };
                    self.match_cache.insert(cache_key, result.clone());
                    return result;
                },
                PatternResult::NoMatch { .. } => {
                    // Continue to next pattern
                    continue;
                }
            }
        }

        // No patterns matched
        let result = PatternResult::NoMatch {
            reason: format!("No patterns matched for handler: {}", handler_type)
        };
        self.match_cache.insert(cache_key, result.clone());
        result
    }

    /// Match tokens against a single pattern
    fn match_single_pattern(&self, pattern: &HandlerPattern, tokens: &[Token]) -> PatternResult {
        // Check basic length requirements
        if tokens.len() < pattern.min_tokens {
            return PatternResult::NoMatch {
                reason: format!("Too few tokens: {} < {}", tokens.len(), pattern.min_tokens)
            };
        }

        if let Some(max) = pattern.max_tokens {
            if tokens.len() > max {
                return PatternResult::NoMatch {
                    reason: format!("Too many tokens: {} > {}", tokens.len(), max)
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
                        reason: format!("End of tokens at rule {}", rule_idx)
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
                        reason: format!("Token '{}' doesn't match pattern at rule {}",
                                        token.to_string(), rule_idx)
                    };
                }
            }

            // Check custom validator if present
            if let Some(validator) = rule.custom_validator {
                if !validator(tokens, pos) {
                    return PatternResult::Reject {
                        reason: format!("Custom validator failed at position {}", pos)
                    };
                }
            }

            // Check forbidden next patterns
            if pos + 1 < tokens.len() {
                let next_token = &tokens[pos + 1];
                for forbidden_pattern in &rule.forbidden_next {
                    if forbidden_pattern.matches(next_token) {
                        return PatternResult::Reject {
                            reason: format!("Forbidden pattern '{}' found after '{}'",
                                            next_token.to_string(), token.to_string())
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
                            reason: format!("Token '{}' not in allowed next patterns after '{}'",
                                            next_token.to_string(), token.to_string())
                        };
                    }
                }
            }

            pos += 1;
            consumed += 1;
        }

        PatternResult::Match { consumed_tokens: consumed }
    }

    /// Clear the match cache (useful for testing or memory management)
    pub fn clear_cache(&mut self) {
        self.match_cache.clear();
    }

    /// Get statistics about registered patterns
    pub fn get_stats(&self) -> HashMap<String, usize> {
        self.patterns.iter()
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
            },
            IterationMode::AllPatterns => {
                // Iterate through all patterns across all handlers
                while self.current_handler_idx < self.handler_keys.len() {
                    let handler_type = &self.handler_keys[self.current_handler_idx];
                    if let Some(patterns) = self.patternizer.patterns.get(handler_type) {
                        if self.current_pattern_idx < patterns.len() {
                            let pattern = &patterns[self.current_pattern_idx];
                            self.current_pattern_idx += 1;
                            return Some(PatternIteratorItem::Pattern(pattern, handler_type.clone()));
                        }
                    }
                    // Move to next handler
                    self.current_handler_idx += 1;
                    self.current_pattern_idx = 0;
                }
                None
            },
            IterationMode::HandlerPatterns(target_handler) => {
                if let Some(patterns) = self.patternizer.patterns.get(target_handler) {
                    if self.current_pattern_idx < patterns.len() {
                        let pattern = &patterns[self.current_pattern_idx];
                        self.current_pattern_idx += 1;
                        return Some(PatternIteratorItem::Pattern(pattern, target_handler.clone()));
                    }
                }
                None
            },
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
        PatternIterator::new(self, IterationMode::HandlerPatterns(handler_type.to_string()))
    }

    /// Create an iterator over cached results
    pub fn iter_cached_results(&self) -> PatternIterator<'_> {
        PatternIterator::new(self, IterationMode::CachedResults)
    }

    /// Collect all patterns with their handler types
    pub fn collect_all_patterns(&self) -> Vec<(String, &HandlerPattern)> {
        self.iter_all_patterns()
            .filter_map(|item| match item {
                PatternIteratorItem::Pattern(pattern, handler_type) => Some((handler_type, pattern)),
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
                },
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

/// Pattern cache entry for storing successful matches in context registry
#[derive(Debug, Clone)]
pub struct CachedPatternMatch {
    pub pattern_id: String,
    pub token_sequence: Vec<String>,
    pub match_result: PatternResult,
    pub hit_count: u64,
    pub last_used: Instant,
    pub performance_metrics: PatternMetrics,
}

/// Performance metrics for pattern matching optimization
#[derive(Debug, Clone)]
#[derive(PartialEq)]
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

impl Default for Pattern {
    fn default() -> Self {
        Pattern {
            id: Id::get("default_pattern"),
            name: "default_pattern".to_string(),
            description: "A pattern for the Patternizer to process".to_string(),
            token_patterns: Vec::new(),
            priority:100,
            handler_types:Vec::new(),
            created_at: Instant::now(),
            usage_metrics: PatternMetrics::default(),
        }
    }
}
// === CONTEXT REGISTRY INTEGRATION FUNCTIONS ===

/// Store a pattern in the context registry (simplified with set_value)
pub fn store_pattern_in_registry(pattern: Pattern) {
    use crate::config::{Global, Entry};
    
    // Use the same key format that get_pattern() expects: "pattern_" + pattern_id
    let pattern_key = format!("pattern_{}", pattern.id.name());

    // Use GlobalContext::with_context for proper shared access to the same global instance
    Global::context_fn(|ctx| {
        let entry = Entry::pattern(pattern.clone());
        ctx.set_entry(&pattern_key, entry);
        println!("📋 Stored pattern '{}' in registry with key '{}'", pattern.id.name(), pattern_key);
    });
}

/// Cache a successful pattern match in the context registry
pub fn cache_pattern_match(cached_match: CachedPatternMatch) {
    use crate::config::Entry;

    let cache_key = format!("pattern_cache_{}", cached_match.pattern_id);
    // Extract consumed_tokens from the match_result
    let consumed_tokens = match &cached_match.match_result {
        PatternResult::Match { consumed_tokens } => *consumed_tokens,
        _ => 0,
    };
    
    let cache_data = Entry::StrMap(
        [
            ("pattern_id".to_string(), Entry::Str(cached_match.pattern_id)),
            ("token_sequence".to_string(), Entry::List(
                cached_match.token_sequence.iter()
                    .map(|t| Entry::Str(t.clone()))
                    .collect()
            )),
            ("consumed_tokens".to_string(), Entry::Val(consumed_tokens as u64)),
            ("hit_count".to_string(), Entry::Val(cached_match.hit_count)),
            ("total_matches".to_string(), Entry::Val(cached_match.performance_metrics.total_matches)),
            ("total_misses".to_string(), Entry::Val(cached_match.performance_metrics.total_misses)),
            ("cache_hit_ratio".to_string(), Entry::Str(format!("{:.2}", cached_match.performance_metrics.cache_hit_ratio))),
        ].into_iter().collect()
    );

    // Use shared context for consistent cache storage
    use crate::config::Global;
    Global::context_fn(|ctx| {
        ctx.set_entry(&cache_key, cache_data);
    });
}

/// Retrieve a cached pattern match from the context registry
pub fn get_cached_pattern_match(pattern_id: &str, token_sequence: &[Token]) -> Option<PatternResult> {
    use crate::config::Global;
    
    let cache_key = format!("pattern_cache_{}", pattern_id);

    let cache_entry = Global::context_fn(|ctx| {
        ctx.get_entry(&cache_key).cloned()
    });
    
    if let Some(cache_entry) = cache_entry {
        println!("🔍 Cache entry found for key '{}'", cache_key);
        match cache_entry {
            crate::config::Entry::StrMap(cache_data) => {
                println!("✅ Cache entry is StrMap with {} fields", cache_data.len());
                // Verify token sequence matches
                if let Some(crate::config::Entry::List(cached_tokens)) = cache_data.get("token_sequence") {
                    println!("✅ Found cached token sequence with {} tokens", cached_tokens.len());
                    let token_strings: Vec<String> = token_sequence.iter().map(|t| t.to_string()).collect();
                    let cached_token_strings: Vec<String> = cached_tokens.iter()
                        .filter_map(|e| match e {
                            crate::config::Entry::Str(s) => Some(s.clone()),
                            _ => None,
                        })
                        .collect();

                    println!("🔍 Current tokens: {:?}", token_strings);
                    println!("🔍 Cached tokens: {:?}", cached_token_strings);
                    
                    if token_strings == cached_token_strings {
                        println!("🎯 CACHE HIT! Returning cached result for pattern '{}'", pattern_id);
                        // Return the cached consumed_tokens value, not the full sequence length
                        if let Some(crate::config::Entry::Val(cached_consumed)) = cache_data.get("consumed_tokens") {
                            return Some(PatternResult::Match { consumed_tokens: *cached_consumed as usize });
                        } else {
                            // Fallback if no consumed_tokens found (should not happen)
                            return Some(PatternResult::Match { consumed_tokens: token_sequence.len() });
                        }
                    } else {
                        println!("❌ Token sequences don't match - cache miss");
                    }
                } else {
                    println!("❌ No token_sequence found in cache data");
                }
            },
            _ => {
                println!("❌ Cache entry is not StrMap: {:?}", cache_entry);
            }
        }
    }

    None
}

/// Register common multi-token patterns with intelligent caching
pub fn register_common_multi_token_patterns() {
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
    store_pattern_in_registry(function_pattern);

    // Path-like include pattern: "#include" "path"
    let include_pattern = Pattern {
        id: Id::get("c_include_statement"),
        name: "C Include Statement".to_string(),
        description: "Matches #include statements with paths".to_string(),
        token_patterns: vec![
            TokenPattern::Exact("#".to_string()),
            TokenPattern::Exact("include".to_string()),
            TokenPattern::PathLike,
        ],
        priority: 90,
        handler_types: vec!["preprocessor".to_string()],
        created_at: Instant::now(),
        usage_metrics: PatternMetrics::default(),
    };
    store_pattern_in_registry(include_pattern);

    // Complex identifier with embedded operators
    let complex_identifier_pattern = Pattern {
        id: Id::get("complex_identifier"),
        name: "Complex Identifier".to_string(),
        description: "Matches identifiers with embedded special characters".to_string(),
        token_patterns: vec![
            TokenPattern::IdentifierWithChars(vec!['-', '_']),
        ],
        priority: 80,
        handler_types: vec!["variable".to_string(), "function".to_string()],
        created_at: Instant::now(),
        usage_metrics: PatternMetrics::default(),
    };
    store_pattern_in_registry(complex_identifier_pattern);

    // Array declaration pattern: "type identifier [ size ]"
    let array_pattern = Pattern {
        id: Id::get("c_array_declaration"),
        name: "C Array Declaration".to_string(),
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
    store_pattern_in_registry(array_pattern);
}

/// Enhanced pattern matching with context registry caching (simplified with get_value)
pub fn match_pattern_with_registry_cache(pattern_id: &str, tokens: &[Token]) -> PatternResult {
    let start_time = Instant::now();

    // Check cache first
    if let Some(cached_result) = get_cached_pattern_match(pattern_id, tokens) {
        // Update hit metrics (simplified)
        return cached_result;
    }

    // Look up the pattern using the new dedicated get_pattern function

    // Use the new dedicated get_pattern function that bypasses dyn Any entirely
    match Global::context().get_pattern(pattern_id) {
        Some(pattern) => {
            // Match this pattern against the tokens
            let result = match_multi_token_pattern(pattern, tokens);
            let match_time = start_time.elapsed();

            // Cache successful matches
            if let PatternResult::Match { .. } = result {
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
                };
                cache_pattern_match(cached_match);
            }

            result
        }
        _ => {
            println!("❌ Pattern '{}' not found in registry", pattern_id);
            PatternResult::NoMatch {
                reason: format!("No patterns registered for handler: {}", pattern_id)
            }
        }
    }
}

/// Match a Patternizer against a token sequence
fn match_multi_token_pattern(pattern: &Pattern, tokens: &[Token]) -> PatternResult {
    println!("🔍 Matching pattern '{}' against {} tokens", pattern.name, tokens.len());

    // For now, implement a simple pattern matching logic based on the pattern ID
    // In a full implementation, we would use the token_patterns vector for sophisticated matching
    match pattern.name.as_str() {
        "c_function_declaration" => {
            // Look for: type identifier ( ... )
            if tokens.len() >= 4 {
                let has_type = matches!(tokens[0].to_string().as_str(), "int" | "void" | "char" | "float" | "double" | "long" | "short" | "unsigned");
                let has_identifier = !tokens[1].to_string().contains('(') && !tokens[1].to_string().contains(')');
                let has_open_paren = tokens[2].to_string() == "(";

                if has_type && has_identifier && has_open_paren {
                    // Find closing paren
                    for (i, token) in tokens.iter().enumerate().skip(3) {
                        if token.to_string() == ")" {
                            return PatternResult::Match { consumed_tokens: i + 1 };
                        }
                    }
                }
            }
        },
        "c_include_statement" => {
            // Look for: # include <...> or # include "..."
            if tokens.len() >= 2 && tokens[0].to_string() == "#" && tokens[1].to_string() == "include" {
                return PatternResult::Match { consumed_tokens: tokens.len().min(5) };
            }
        },
        "c_array_declaration" => {
            // Look for: type identifier [ size ]
            if tokens.len() >= 5 {
                let has_type = matches!(tokens[0].to_string().as_str(), "int" | "char" | "float" | "double");
                let has_identifier = !tokens[1].to_string().contains('[');
                let has_open_bracket = tokens[2].to_string() == "[";

                if has_type && has_identifier && has_open_bracket {
                    // Find closing bracket
                    for (i, token) in tokens.iter().enumerate().skip(3) {
                        if token.to_string() == "]" {
                            return PatternResult::Match { consumed_tokens: i + 1 };
                        }
                    }
                }
            }
        },
        "complex_identifier" => {
            // Look for identifiers with underscores, dashes, etc.
            if tokens.len() >= 1 {
                let token_str = tokens[0].to_string();
                if token_str.contains('_') || token_str.len() > 8 {
                    return PatternResult::Match { consumed_tokens: 1 };
                }
            }
        },
        _ => {}
    }

    PatternResult::NoMatch {
        reason: format!("Pattern '{}' did not match token sequence", pattern.name)
    }
}

/// Match a registry-stored pattern against tokens
fn match_registry_pattern_against_tokens(pattern_data: &HashMap<String, crate::config::Entry>, tokens: &[Token]) -> PatternResult {
    // Extract pattern information from the registry data
    let pattern_id = match pattern_data.get("id") {
        Some(crate::config::Entry::Str(id)) => id.clone(),
        _ => "unknown".to_string(),
    };
    let _description = match pattern_data.get("description") {
        Some(crate::config::Entry::Str(desc)) => desc.clone(),
        _ => "".to_string(),
    };

    // For now, implement a simple pattern matching logic based on the pattern ID
    // In a full implementation, we would deserialize the full Patternizer and use it
    match pattern_id.as_str() {
        "c_function_declaration" => {
            // Look for: type identifier ( ... )
            if tokens.len() >= 4 {
                let has_type = matches!(tokens[0].to_string().as_str(), "int" | "void" | "char" | "float" | "double" | "long" | "short" | "unsigned");
                let has_identifier = !tokens[1].to_string().contains('(') && !tokens[1].to_string().contains(')');
                let has_open_paren = tokens[2].to_string() == "(";

                if has_type && has_identifier && has_open_paren {
                    // Find closing paren
                    for (i, token) in tokens.iter().enumerate().skip(3) {
                        if token.to_string() == ")" {
                            return PatternResult::Match { consumed_tokens: i + 1 };
                        }
                    }
                }
            }
        },
        "c_include_statement" => {
            // Look for: # include <...> or # include "..."
            if tokens.len() >= 2 && tokens[0].to_string() == "#" && tokens[1].to_string() == "include" {
                return PatternResult::Match { consumed_tokens: tokens.len().min(5) };
            }
        },
        "c_array_declaration" => {
            // Look for: type identifier [ size ]
            if tokens.len() >= 5 {
                let has_type = matches!(tokens[0].to_string().as_str(), "int" | "char" | "float" | "double");
                let has_identifier = !tokens[1].to_string().contains('[');
                let has_open_bracket = tokens[2].to_string() == "[";

                if has_type && has_identifier && has_open_bracket {
                    // Find closing bracket
                    for (i, token) in tokens.iter().enumerate().skip(3) {
                        if token.to_string() == "]" {
                            return PatternResult::Match { consumed_tokens: i + 1 };
                        }
                    }
                }
            }
        },
        "complex_identifier" => {
            // Look for identifiers with underscores, dashes, etc.
            if tokens.len() >= 1 {
                let token_str = tokens[0].to_string();
                if token_str.contains('_') || token_str.len() > 8 {
                    return PatternResult::Match { consumed_tokens: 1 };
                }
            }
        },
        _ => {}
    }

    PatternResult::NoMatch {
        reason: format!("Pattern '{}' did not match token sequence", pattern_id)
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
    Global::context_fn(|ctx| ctx.patternizer.initialize_common_patterns());
}

/// Match patterns for a handler using the global context patternizer  
pub fn match_handler_pattern(handler_type: &str, tokens: &[Token]) -> PatternResult {
    Global::context_fn(|ctx| ctx.patternizer.match_pattern(handler_type, tokens))
}

/// Check if tokens should be rejected by any handler using the global context patternizer
pub fn should_reject_tokens(handler_type: &str, tokens: &[Token]) -> bool {
    Global::context_fn(|ctx| ctx.patternizer.should_reject_tokens(handler_type, tokens))
}

/// Get pattern statistics from the global context patternizer (read-only)
pub fn get_pattern_stats() -> std::collections::HashMap<String, usize> {
    Global::context_fn(|ctx|ctx.patternizer.get_stats())
}