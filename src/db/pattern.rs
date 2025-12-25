//! Pattern Matching System for DB Framework
//!
//! Provides pattern definition, storage, and matching using a hybrid automaton.
//! Combines Aho-Corasick (fast literal matching) with NFA (flexible patterns).
//!
//! Key components:
//! - `Pattern`: Unified enum for all pattern types (Definition, Literal, Predicate, Sequence)
//! - `PatternRule`: Individual rules within Definition patterns
//! - `PatternMachine`: Hybrid automaton for pattern matching
//! - `PatternMatch`: Result of a successful match
//!
//! Patterns are stored in Web database via the Build trait.

use crate::db::web::{Entry, Build};
use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::{self, Debug};
use std::hash::Hash;
use std::time::SystemTime;

// ============================================================================
// Rule Types
// ============================================================================

/// Types of pattern rules
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RuleType {
    /// Match exact token
    Exact,
    /// Match any non-type keyword (if, while, etc.)
    Keyword,
    /// Match any type keyword (int, char, etc.)
    TypeKeyword,
    /// Match any identifier
    Identifier,
    /// Match one of several options
    OneOf,
    /// Match any token
    Any,
    /// Custom validator function (stored as name)
    Custom(String),
}

/// Logical operations for branch patterns
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LogicalOp {
    /// Both operands must match (AND)
    And,
    /// Either operand must match (OR)
    Or,
    /// Neither operand must match (NOR)
    Nor,
    /// Not both operands match (NAND)
    Nand,
    /// Exactly one operand matches (XOR)
    Xor,
    /// Left operand matches but not right (AND NOT)
    AndNot,
    /// Right operand matches but not left (NOT AND)
    NotAnd,
}

impl LogicalOp {
    /// Get all available operations
    pub fn all() -> Vec<Self> {
        vec![
            Self::And,
            Self::Or,
            Self::Nor,
            Self::Nand,
            Self::Xor,
            Self::AndNot,
            Self::NotAnd,
        ]
    }

    /// Get operation name as string
    pub fn name(&self) -> &'static str {
        match self {
            Self::And => "AND",
            Self::Or => "OR",
            Self::Nor => "NOR",
            Self::Nand => "NAND",
            Self::Xor => "XOR",
            Self::AndNot => "AND_NOT",
            Self::NotAnd => "NOT_AND",
        }
    }

    /// Parse operation from string
    pub fn from_str(s: &str) -> Option<Self> {
        match s.to_uppercase().as_str() {
            "AND" => Some(Self::And),
            "OR" => Some(Self::Or),
            "NOR" => Some(Self::Nor),
            "NAND" => Some(Self::Nand),
            "XOR" => Some(Self::Xor),
            "AND_NOT" | "ANDNOT" => Some(Self::AndNot),
            "NOT_AND" | "NOTAND" => Some(Self::NotAnd),
            _ => None,
        }
    }

    /// Evaluate the logical operation
    pub fn evaluate(&self, left: bool, right: bool) -> bool {
        match self {
            LogicalOp::And => left && right,
            LogicalOp::Or => left || right,
            LogicalOp::Nor => !(left || right),
            LogicalOp::Nand => !(left && right),
            LogicalOp::Xor => left ^ right,
            LogicalOp::AndNot => left && !right,
            LogicalOp::NotAnd => !left && right,
        }
    }
}

// ============================================================================
// Pattern Rule
// ============================================================================

/// A single pattern matching rule
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PatternRule {
    pub rule_type: RuleType,
    pub value: String,
    pub optional: bool,
    pub can_repeat: bool,
    pub forbidden_next: Vec<String>,
    pub required_next: Vec<String>,
}

impl PatternRule {
    /// Create exact match rule
    pub fn exact(value: &str) -> Self {
        Self {
            rule_type: RuleType::Exact,
            value: value.to_string(),
            optional: false,
            can_repeat: false,
            forbidden_next: Vec::new(),
            required_next: Vec::new(),
        }
    }

    /// Create type keyword rule
    pub fn type_keyword() -> Self {
        Self {
            rule_type: RuleType::TypeKeyword,
            value: String::new(),
            optional: false,
            can_repeat: false,
            forbidden_next: Vec::new(),
            required_next: Vec::new(),
        }
    }

    /// Create keyword rule
    pub fn keyword() -> Self {
        Self {
            rule_type: RuleType::Keyword,
            value: String::new(),
            optional: false,
            can_repeat: false,
            forbidden_next: Vec::new(),
            required_next: Vec::new(),
        }
    }

    /// Create identifier rule
    pub fn identifier() -> Self {
        Self {
            rule_type: RuleType::Identifier,
            value: String::new(),
            optional: false,
            can_repeat: false,
            forbidden_next: Vec::new(),
            required_next: Vec::new(),
        }
    }

    /// Create one-of rule
    pub fn one_of(options: Vec<String>) -> Self {
        Self {
            rule_type: RuleType::OneOf,
            value: options.join("|"),
            optional: false,
            can_repeat: false,
            forbidden_next: Vec::new(),
            required_next: Vec::new(),
        }
    }

    /// Create any token rule
    pub fn any() -> Self {
        Self {
            rule_type: RuleType::Any,
            value: String::new(),
            optional: false,
            can_repeat: false,
            forbidden_next: Vec::new(),
            required_next: Vec::new(),
        }
    }

    /// Create custom validator rule
    pub fn custom(name: impl Into<String>) -> Self {
        Self {
            rule_type: RuleType::Custom(name.into()),
            value: String::new(),
            optional: false,
            can_repeat: false,
            forbidden_next: Vec::new(),
            required_next: Vec::new(),
        }
    }

    /// Make rule optional
    pub fn optional(mut self) -> Self {
        self.optional = true;
        self
    }

    /// Allow rule to repeat
    pub fn with_can_repeat(mut self, can_repeat: bool) -> Self {
        self.can_repeat = can_repeat;
        self
    }

    /// Add forbidden next tokens
    pub fn with_forbidden_next(mut self, forbidden: Vec<String>) -> Self {
        self.forbidden_next = forbidden;
        self
    }

    /// Add required next tokens
    pub fn with_required_next(mut self, required: Vec<String>) -> Self {
        self.required_next = required;
        self
    }
}

// ============================================================================
// Pattern Definition
// ============================================================================

/// Statistical analysis data for a pattern
#[derive(Debug, Clone, Default)]
pub struct PatternStats {
    /// Confidence score (0.0 - 1.0)
    pub confidence: f64,
    /// Number of times pattern was matched
    pub match_count: u32,
    /// Number of successful validations
    pub validation_success: u32,
    /// Number of failed validations
    pub validation_failures: u32,
    /// Success rate
    pub success_rate: f64,
    /// Average tokens consumed per match
    pub avg_tokens_consumed: f64,
    /// Minimum tokens consumed
    pub min_tokens_consumed: usize,
    /// Maximum tokens consumed
    pub max_tokens_consumed: usize,
    /// Timestamp of last update
    pub last_updated: Option<SystemTime>,
    /// Match quality indicator
    pub match_quality: Option<String>,
    /// Additional metadata
    pub metadata: HashMap<String, String>,
}

// ============================================================================
// Unified Pattern System
// ============================================================================

/// Unified pattern enum containing all pattern variants with common fields inline.
#[derive(Debug, Clone)]
pub enum Pattern<T = String> {
    /// Rule-based pattern definition
    Definition {
        id: usize,
        name: String,
        category: String,
        priority: i32,
        min_tokens: usize,
        description: String,
        rules: Vec<PatternRule>,
    },
    /// Literal/exact sequence match
    Literal {
        id: usize,
        name: String,
        category: String,
        priority: i32,
        min_tokens: usize,
        description: String,
        elements: Vec<T>,
    },
    /// Predicate-based pattern
    Predicate {
        id: usize,
        name: String,
        category: String,
        priority: i32,
        min_tokens: usize,
        description: String,
        predicate_name: &'static str,
    },
    /// Wildcard pattern - matches any sequence
    Wildcard {
        id: usize,
        name: String,
        category: String,
        priority: i32,
        min_tokens: usize,
        description: String,
    },
    /// Sequence of sub-patterns
    Sequence {
        id: usize,
        name: String,
        category: String,
        priority: i32,
        min_tokens: usize,
        description: String,
        patterns: Vec<Pattern<T>>,
    },
    /// Choice between patterns (matches first successful)
    Choice {
        id: usize,
        name: String,
        category: String,
        priority: i32,
        min_tokens: usize,
        description: String,
        alternatives: Vec<Pattern<T>>,
    },
    /// Optional pattern (zero or one)
    Optional {
        id: usize,
        name: String,
        category: String,
        priority: i32,
        min_tokens: usize,
        description: String,
        inner: Box<Pattern<T>>,
    },
    /// Repeating pattern (zero or more)
    Repeat {
        id: usize,
        name: String,
        category: String,
        priority: i32,
        min_tokens: usize,
        description: String,
        inner: Box<Pattern<T>>,
        min_repeat: usize,
        max_repeat: Option<usize>,
    },
    /// Branch pattern - conditional matching with logical operations
    Branch {
        id: usize,
        name: String,
        category: String,
        priority: i32,
        min_tokens: usize,
        description: String,
        /// The logical operation to evaluate
        operation: LogicalOp,
        /// Left pattern to evaluate
        left: Box<Pattern<T>>,
        /// Right pattern to evaluate
        right: Box<Pattern<T>>,
        /// Pattern to follow if condition is true
        true_path: Box<Pattern<T>>,
        /// Pattern to follow if condition is false
        false_path: Box<Pattern<T>>,
    },
}

impl<T> Pattern<T> {
    /// Convenience accessors for common fields
    pub fn id(&self) -> usize {
        match self {
            Pattern::Definition { id, .. } | Pattern::Literal { id, .. } |
            Pattern::Predicate { id, .. } | Pattern::Wildcard { id, .. } |
            Pattern::Sequence { id, .. } | Pattern::Choice { id, .. } |
            Pattern::Optional { id, .. } | Pattern::Repeat { id, .. } |
            Pattern::Branch { id, .. } => *id,
        }
    }
    
    pub fn name(&self) -> &str {
        match self {
            Pattern::Definition { name, .. } | Pattern::Literal { name, .. } |
            Pattern::Predicate { name, .. } | Pattern::Wildcard { name, .. } |
            Pattern::Sequence { name, .. } | Pattern::Choice { name, .. } |
            Pattern::Optional { name, .. } | Pattern::Repeat { name, .. } |
            Pattern::Branch { name, .. } => name,
        }
    }
    
    pub fn category(&self) -> &str {
        match self {
            Pattern::Definition { category, .. } | Pattern::Literal { category, .. } |
            Pattern::Predicate { category, .. } | Pattern::Wildcard { category, .. } |
            Pattern::Sequence { category, .. } | Pattern::Choice { category, .. } |
            Pattern::Optional { category, .. } | Pattern::Repeat { category, .. } |
            Pattern::Branch { category, .. } => category,
        }
    }
    
    pub fn priority(&self) -> i32 {
        match self {
            Pattern::Definition { priority, .. } | Pattern::Literal { priority, .. } |
            Pattern::Predicate { priority, .. } | Pattern::Wildcard { priority, .. } |
            Pattern::Sequence { priority, .. } | Pattern::Choice { priority, .. } |
            Pattern::Optional { priority, .. } | Pattern::Repeat { priority, .. } |
            Pattern::Branch { priority, .. } => *priority,
        }
    }
    
    pub fn min_tokens(&self) -> usize {
        match self {
            Pattern::Definition { min_tokens, .. } | Pattern::Literal { min_tokens, .. } |
            Pattern::Predicate { min_tokens, .. } | Pattern::Wildcard { min_tokens, .. } |
            Pattern::Sequence { min_tokens, .. } | Pattern::Choice { min_tokens, .. } |
            Pattern::Optional { min_tokens, .. } | Pattern::Repeat { min_tokens, .. } |
            Pattern::Branch { min_tokens, .. } => *min_tokens,
        }
    }
    
    pub fn description(&self) -> &str {
        match self {
            Pattern::Definition { description, .. } | Pattern::Literal { description, .. } |
            Pattern::Predicate { description, .. } | Pattern::Wildcard { description, .. } |
            Pattern::Sequence { description, .. } | Pattern::Choice { description, .. } |
            Pattern::Optional { description, .. } | Pattern::Repeat { description, .. } |
            Pattern::Branch { description, .. } => description,
        }
    }

    /// Check if this is a definition pattern
    pub fn is_definition(&self) -> bool { matches!(self, Pattern::Definition { .. }) }
    /// Check if this is a literal pattern
    pub fn is_literal(&self) -> bool { matches!(self, Pattern::Literal { .. }) }
    /// Check if this is a predicate pattern
    pub fn is_predicate(&self) -> bool { matches!(self, Pattern::Predicate { .. }) }
    /// Check if this is a wildcard pattern
    pub fn is_wildcard(&self) -> bool { matches!(self, Pattern::Wildcard { .. }) }

    /// Create a definition pattern
    pub fn definition(id: usize, name: impl Into<String>, rules: Vec<PatternRule>) -> Self {
        Pattern::Definition {
            id,
            name: name.into(),
            category: String::new(),
            priority: 0,
            min_tokens: 1,
            description: String::new(),
            rules,
        }
    }

    /// Create a literal pattern
    pub fn literal(id: usize, name: impl Into<String>, elements: Vec<T>) -> Self {
        Pattern::Literal {
            id,
            name: name.into(),
            category: String::new(),
            priority: 0,
            min_tokens: 1,
            description: String::new(),
            elements,
        }
    }

    /// Create a wildcard pattern
    pub fn wildcard(id: usize, name: impl Into<String>) -> Self {
        Pattern::Wildcard {
            id,
            name: name.into(),
            category: String::new(),
            priority: 0,
            min_tokens: 1,
            description: String::new(),
        }
    }

    /// Builder: set category
    pub fn with_category(mut self, cat: impl Into<String>) -> Self {
        match &mut self {
            Pattern::Definition { category, .. } | Pattern::Literal { category, .. } |
            Pattern::Predicate { category, .. } | Pattern::Wildcard { category, .. } |
            Pattern::Sequence { category, .. } | Pattern::Choice { category, .. } |
            Pattern::Optional { category, .. } | Pattern::Repeat { category, .. } |
            Pattern::Branch { category, .. } => {
                *category = cat.into();
            }
        }
        self
    }

    /// Builder: set priority
    pub fn with_priority(mut self, pri: i32) -> Self {
        match &mut self {
            Pattern::Definition { priority, .. } | Pattern::Literal { priority, .. } |
            Pattern::Predicate { priority, .. } | Pattern::Wildcard { priority, .. } |
            Pattern::Sequence { priority, .. } | Pattern::Choice { priority, .. } |
            Pattern::Optional { priority, .. } | Pattern::Repeat { priority, .. } |
            Pattern::Branch { priority, .. } => {
                *priority = pri;
            }
        }
        self
    }

    /// Builder: set minimum tokens
    pub fn with_min_tokens(mut self, min: usize) -> Self {
        match &mut self {
            Pattern::Definition { min_tokens, .. } | Pattern::Literal { min_tokens, .. } |
            Pattern::Predicate { min_tokens, .. } | Pattern::Wildcard { min_tokens, .. } |
            Pattern::Sequence { min_tokens, .. } | Pattern::Choice { min_tokens, .. } |
            Pattern::Optional { min_tokens, .. } | Pattern::Repeat { min_tokens, .. } |
            Pattern::Branch { min_tokens, .. } => {
                *min_tokens = min;
            }
        }
        self
    }

    /// Builder: set description
    pub fn with_description(mut self, desc: impl Into<String>) -> Self {
        match &mut self {
            Pattern::Definition { description, .. } | Pattern::Literal { description, .. } |
            Pattern::Predicate { description, .. } | Pattern::Wildcard { description, .. } |
            Pattern::Sequence { description, .. } | Pattern::Choice { description, .. } |
            Pattern::Optional { description, .. } | Pattern::Repeat { description, .. } |
            Pattern::Branch { description, .. } => {
                *description = desc.into();
            }
        }
        self
    }

    /// Convert to Entry representation
    pub fn to_entry(&self) -> Entry {
        let mut node = Entry::node("Pattern", self.name());
        node.set_attr("id", Entry::usize(self.id()));
        node.set_attr("category", Entry::string(self.category()));
        node.set_attr("priority", Entry::i32(self.priority()));
        node.set_attr("min_tokens", Entry::usize(self.min_tokens()));
        
        let variant_name = match self {
            Pattern::Definition { .. } => "definition",
            Pattern::Literal { .. } => "literal",
            Pattern::Predicate { .. } => "predicate",
            Pattern::Wildcard { .. } => "wildcard",
            Pattern::Sequence { .. } => "sequence",
            Pattern::Choice { .. } => "choice",
            Pattern::Optional { .. } => "optional",
            Pattern::Repeat { .. } => "repeat",
            Pattern::Branch { .. } => "branch",
        };
        node.set_attr("type", Entry::string(variant_name));

        let desc = self.description();
        if !desc.is_empty() {
            node.set_attr("description", Entry::string(desc));
        }

        node
    }

    /// Reconstruct a Pattern from an Entry
    pub fn from_entry(entry: &Entry) -> Option<Pattern<T>> 
    where T: Default 
    {
        if entry.kind() != Some("Pattern") {
            return None;
        }
        
        let name = entry.name()?.to_string();
        let id = entry.get_number_attr("id").map(|n| n as usize).unwrap_or(0);
        let category = entry.get_string_attr("category").unwrap_or("").to_string();
        let priority = entry.get_number_attr("priority").map(|n| n as i32).unwrap_or(0);
        let min_tokens = entry.get_number_attr("min_tokens").map(|n| n as usize).unwrap_or(1);
        let description = entry.get_string_attr("description").unwrap_or("").to_string();
        let pattern_type = entry.get_string_attr("type").unwrap_or("wildcard");
        
        Some(match pattern_type {
            "definition" => Pattern::Definition { id, name, category, priority, min_tokens, description, rules: Vec::new() },
            "literal" => Pattern::Literal { id, name, category, priority, min_tokens, description, elements: Vec::new() },
            "predicate" => Pattern::Predicate { id, name, category, priority, min_tokens, description, predicate_name: "unknown" },
            "sequence" => Pattern::Sequence { id, name, category, priority, min_tokens, description, patterns: Vec::new() },
            "choice" => Pattern::Choice { id, name, category, priority, min_tokens, description, alternatives: Vec::new() },
            "optional" => Pattern::Optional { id, name: name.clone(), category: category.clone(), priority, min_tokens, description: description.clone(), 
                inner: Box::new(Pattern::Wildcard { id: 0, name, category, priority: 0, min_tokens: 1, description }) },
            "repeat" => Pattern::Repeat { id, name: name.clone(), category: category.clone(), priority, min_tokens, description: description.clone(), 
                inner: Box::new(Pattern::Wildcard { id: 0, name, category, priority: 0, min_tokens: 1, description }), min_repeat: 0, max_repeat: None },
            _ => Pattern::Wildcard { id, name, category, priority, min_tokens, description },
        })
    }
}

/// Pattern matching implementation for Definition patterns
impl Pattern<String> {
    /// Check if tokens match this pattern's rules (for Definition patterns)
    /// Returns confidence score (0.0 - 1.0) or None if no match
    pub fn matches_tokens(&self, tokens: &[String]) -> Option<f64> {
        let rules = match self {
            Pattern::Definition { rules, min_tokens, .. } => {
                if tokens.len() < *min_tokens {
                    return None;
                }
                rules
            }
            _ => return Some(0.5), // Non-definition patterns use simple matching
        };
        
        // If no rules defined, use simple first-token matching
        if rules.is_empty() {
            return Some(0.5);
        }
        
        let mut token_idx = 0;
        let mut rule_idx = 0;
        let mut matched_rules = 0;
        
        while rule_idx < rules.len() && token_idx < tokens.len() {
            let rule = &rules[rule_idx];
            let token = &tokens[token_idx];
            
            let matches = Self::rule_matches_token(rule, token, tokens.get(token_idx + 1));
            
            if matches {
                matched_rules += 1;
                token_idx += 1;
                
                // Handle repeating rules
                if rule.can_repeat && token_idx < tokens.len() {
                    while token_idx < tokens.len() {
                        if Self::rule_matches_token(rule, &tokens[token_idx], tokens.get(token_idx + 1)) {
                            token_idx += 1;
                        } else {
                            break;
                        }
                    }
                }
                rule_idx += 1;
            } else if rule.optional {
                rule_idx += 1;
            } else {
                return None;
            }
        }
        
        // Check remaining rules are optional
        while rule_idx < rules.len() {
            if !rules[rule_idx].optional {
                return None;
            }
            rule_idx += 1;
        }
        
        // Calculate confidence based on how many rules matched
        let total_rules = rules.iter().filter(|r| !r.optional).count();
        if total_rules == 0 {
            Some(0.7)
        } else {
            Some((matched_rules as f64 / total_rules as f64).min(1.0) * 0.9 + 0.1)
        }
    }
    
    /// Check if a single rule matches a token
    fn rule_matches_token(rule: &PatternRule, token: &str, next_token: Option<&String>) -> bool {
        if let Some(next) = next_token {
            if rule.forbidden_next.contains(next) {
                return false;
            }
        }
        
        match &rule.rule_type {
            RuleType::Exact => token == rule.value,
            RuleType::TypeKeyword => {
                crate::db::keyword::is_type_keyword(token) ||
                matches!(token, "bool" | "size_t" | 
                        "uint8_t" | "uint16_t" | "uint32_t" | "uint64_t" |
                        "int8_t" | "int16_t" | "int32_t" | "int64_t")
            }
            RuleType::Keyword => {
                crate::db::keyword::is_control_flow(token) ||
                crate::db::keyword::is_storage_class(token) ||
                crate::db::keyword::is_type_qualifier(token) ||
                matches!(token, "typedef" | "inline")
            }
            RuleType::Identifier => {
                !token.is_empty() && 
                (token.chars().next().unwrap().is_alphabetic() || token.starts_with('_')) &&
                token.chars().all(|c| c.is_alphanumeric() || c == '_')
            }
            RuleType::OneOf => {
                rule.value.split('|').any(|opt| opt == token)
            }
            RuleType::Any => true,
            RuleType::Custom(_) => true,
        }
    }
}

/// Implement Build trait for Pattern<T>
impl<T: Clone + Debug> Build for Pattern<T> {
    fn to_entry(&self) -> Entry {
        Pattern::to_entry(self)
    }
    
    fn kind(&self) -> &str {
        "Pattern"
    }
    
    fn name(&self) -> Option<&str> {
        Some(Pattern::name(self))
    }
    
    fn category(&self) -> Option<&str> {
        let cat = Pattern::category(self);
        if cat.is_empty() { None } else { Some(cat) }
    }
    
    fn priority(&self) -> i16 {
        Pattern::priority(self) as i16
    }
}

// Note: Use Web for unified pattern storage:
//   let mut web = Web::new();
//   web.add(&pattern);  // Add Pattern via Build trait
//   web.by_kind("Pattern");  // Query all patterns
//   web.by_category("function");  // Query by category
//   web.by_name("function_definition");  // Query by name

// ============================================================================
// Pattern Matcher Helper
// ============================================================================

use crate::db::token::Token;

/// Result of pattern matching with extracted segments
#[derive(Debug, Clone)]
pub struct MatchResult {
    /// Pattern that matched
    pub pattern_name: String,
    /// Confidence score (0.0 - 1.0)
    pub confidence: f64,
    /// Token indices consumed by the match
    pub token_range: std::ops::Range<usize>,
    /// Extracted segments (e.g., condition, body, etc.)
    pub segments: Vec<MatchSegment>,
}

/// A segment extracted from matched tokens
#[derive(Debug, Clone)]
pub struct MatchSegment {
    /// Name of the segment (e.g., "condition", "body", "init")
    pub name: String,
    /// Token indices for this segment
    pub range: std::ops::Range<usize>,
    /// The tokens in this segment
    pub tokens: Vec<Token>,
}

// ============================================================================
// Matcher for Automaton Edges
// ============================================================================

/// A transition matcher - determines what input triggers this transition
#[derive(Clone)]
pub enum Matcher<T> {
    /// Match exact value
    Exact(T),
    /// Match any value (wildcard)
    Any,
    /// Match if predicate returns true
    Predicate(fn(&T) -> bool, &'static str),
    /// Match any of these values
    OneOf(Vec<T>),
    /// Match none of these values
    NoneOf(Vec<T>),
    /// Match a range (for ordered types)
    Range(T, T),
    /// Epsilon - no input consumed, always succeeds
    Epsilon,
}

impl<T: PartialEq> Matcher<T> {
    /// Check if this matcher matches the given input
    pub fn matches(&self, input: &T) -> bool
    where
        T: PartialOrd,
    {
        match self {
            Matcher::Exact(v) => input == v,
            Matcher::Any => true,
            Matcher::Predicate(f, _) => f(input),
            Matcher::OneOf(vs) => vs.contains(input),
            Matcher::NoneOf(vs) => !vs.contains(input),
            Matcher::Range(lo, hi) => input >= lo && input <= hi,
            Matcher::Epsilon => true,
        }
    }

    /// Check if this is an epsilon transition
    pub fn is_epsilon(&self) -> bool {
        matches!(self, Matcher::Epsilon)
    }
}

impl<T: Debug> Debug for Matcher<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Matcher::Exact(v) => write!(f, "={:?}", v),
            Matcher::Any => write!(f, "*"),
            Matcher::Predicate(_, name) => write!(f, "?{}", name),
            Matcher::OneOf(vs) => write!(f, "∈{:?}", vs),
            Matcher::NoneOf(vs) => write!(f, "∉{:?}", vs),
            Matcher::Range(lo, hi) => write!(f, "[{:?}..{:?}]", lo, hi),
            Matcher::Epsilon => write!(f, "ε"),
        }
    }
}

impl<T: PartialEq> PartialEq for Matcher<T> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Matcher::Exact(a), Matcher::Exact(b)) => a == b,
            (Matcher::Any, Matcher::Any) => true,
            (Matcher::Epsilon, Matcher::Epsilon) => true,
            (Matcher::OneOf(a), Matcher::OneOf(b)) => a == b,
            (Matcher::NoneOf(a), Matcher::NoneOf(b)) => a == b,
            (Matcher::Range(a1, a2), Matcher::Range(b1, b2)) => a1 == b1 && a2 == b2,
            (Matcher::Predicate(_, n1), Matcher::Predicate(_, n2)) => std::ptr::eq(*n1, *n2),
            _ => false,
        }
    }
}

impl<T: Eq> Eq for Matcher<T> {}

impl<T: Hash> Hash for Matcher<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            Matcher::Exact(v) => v.hash(state),
            Matcher::OneOf(vs) | Matcher::NoneOf(vs) => vs.hash(state),
            Matcher::Range(lo, hi) => {
                lo.hash(state);
                hi.hash(state);
            }
            Matcher::Predicate(_, name) => name.hash(state),
            _ => {}
        }
    }
}

// ============================================================================
// Edge and Node for Pattern Machine
// ============================================================================

/// A transition edge in the automaton
#[derive(Debug, Clone)]
pub struct Edge<T> {
    pub matcher: Matcher<T>,
    pub target: usize,
    pub priority: i16,
    pub action: Option<u32>,
}

impl<T> Edge<T> {
    pub fn new(matcher: Matcher<T>, target: usize) -> Self {
        Self {
            matcher,
            target,
            priority: 0,
            action: None,
        }
    }

    pub fn with_priority(mut self, priority: i16) -> Self {
        self.priority = priority;
        self
    }

    pub fn with_action(mut self, action: u32) -> Self {
        self.action = Some(action);
        self
    }
}

/// A node in the pattern machine
#[derive(Debug, Clone)]
pub struct MachineNode<T> {
    pub id: usize,
    pub depth: usize,
    pub goto: HashMap<T, usize>,
    pub failure: usize,
    pub output_link: Option<usize>,
    pub edges: Vec<Edge<T>>,
    pub accepting: bool,
    pub patterns: Vec<usize>,
    pub label: Option<String>,
}

impl<T> MachineNode<T> {
    pub fn new(id: usize) -> Self {
        Self {
            id,
            depth: 0,
            goto: HashMap::new(),
            failure: 0,
            output_link: None,
            edges: Vec::new(),
            accepting: false,
            patterns: Vec::new(),
            label: None,
        }
    }
}

/// A pattern registered with the machine
#[derive(Debug, Clone)]
pub struct MachinePattern<T> {
    pub id: usize,
    pub name: String,
    pub elements: Vec<T>,
    pub priority: i16,
    pub data: u64,
}

/// Result of a pattern match
#[derive(Debug, Clone)]
pub struct PatternMatch {
    pub start: usize,
    pub end: usize,
    pub pattern_id: usize,
    pub actions: Vec<u32>,
}

impl PatternMatch {
    /// Convert to Entry representation
    pub fn to_entry(&self) -> Entry {
        let mut node = Entry::node("PatternMatch", &format!("match_{}", self.pattern_id));
        node.set_attr("start", Entry::usize(self.start));
        node.set_attr("end", Entry::usize(self.end));
        node.set_attr("pattern_id", Entry::usize(self.pattern_id));
        
        if !self.actions.is_empty() {
            let actions: Vec<Entry> = self.actions.iter().map(|&a| Entry::u32(a)).collect();
            node.set_attr("actions", Entry::vec(actions));
        }
        
        node
    }

    /// Reconstruct a PatternMatch from an Entry
    pub fn from_entry(entry: &Entry) -> Option<PatternMatch> {
        if entry.kind() != Some("PatternMatch") {
            return None;
        }
        
        let start = entry.get_number_attr("start").map(|n| n as usize).unwrap_or(0);
        let end = entry.get_number_attr("end").map(|n| n as usize).unwrap_or(0);
        let pattern_id = entry.get_number_attr("pattern_id").map(|n| n as usize).unwrap_or(0);
        
        let mut actions = Vec::new();
        if let Some(actions_vec) = entry.get_vec_attr("actions") {
            for a in actions_vec {
                if let Entry::U32(v, _) = a {
                    actions.push(*v);
                }
            }
        }
        
        Some(PatternMatch {
            start,
            end,
            pattern_id,
            actions,
        })
    }
}

/// Implement Build trait for PatternMatch
impl Build for PatternMatch {
    fn to_entry(&self) -> Entry {
        PatternMatch::to_entry(self)
    }
    
    fn kind(&self) -> &str {
        "PatternMatch"
    }
    
    fn name(&self) -> Option<&str> {
        None
    }
}

// ============================================================================
// Pattern Machine - Hybrid Automaton
// ============================================================================

/// A single execution thread in NFA simulation
#[derive(Debug, Clone)]
struct Thread {
    node: usize,
    start: usize,
    actions: Vec<u32>,
}

/// Hybrid pattern matching automaton combining AC and NFA
#[derive(Clone)]
pub struct PatternMachine<T> {
    nodes: Vec<MachineNode<T>>,
    patterns: Vec<MachinePattern<T>>,
    built: bool,
}

impl<T: Clone + Eq + Hash + Debug + PartialOrd> PatternMachine<T> {
    /// Create a new pattern machine with root node
    pub fn new() -> Self {
        let root = MachineNode::new(0);
        Self {
            nodes: vec![root],
            patterns: Vec::new(),
            built: false,
        }
    }

    /// Number of nodes
    pub fn len(&self) -> usize {
        self.nodes.len()
    }

    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }

    /// Number of patterns
    pub fn pattern_count(&self) -> usize {
        self.patterns.len()
    }

    /// Get node by ID
    pub fn node(&self, id: usize) -> Option<&MachineNode<T>> {
        self.nodes.get(id)
    }

    /// Add a new node, returns its ID
    pub fn add_node(&mut self) -> usize {
        let id = self.nodes.len();
        self.nodes.push(MachineNode::new(id));
        id
    }

    /// Add an accepting node
    pub fn add_accepting_node(&mut self, pattern_id: usize) -> usize {
        let id = self.add_node();
        self.nodes[id].accepting = true;
        self.nodes[id].patterns.push(pattern_id);
        id
    }

    /// Add a literal pattern (exact sequence match)
    pub fn add_literal_pattern(&mut self, name: &str, elements: &[T]) -> usize {
        self.built = false;
        let pattern_id = self.patterns.len();

        self.patterns.push(MachinePattern {
            id: pattern_id,
            name: name.to_string(),
            elements: elements.to_vec(),
            priority: 0,
            data: 0,
        });

        let mut current = 0;
        for (i, element) in elements.iter().enumerate() {
            let depth = i + 1;
            current = if let Some(&next) = self.nodes[current].goto.get(element) {
                next
            } else {
                let new_id = self.add_node();
                self.nodes[new_id].depth = depth;
                self.nodes[current].goto.insert(element.clone(), new_id);
                self.nodes[current]
                    .edges
                    .push(Edge::new(Matcher::Exact(element.clone()), new_id));
                new_id
            };
        }

        self.nodes[current].accepting = true;
        self.nodes[current].patterns.push(pattern_id);

        pattern_id
    }

    /// Add an edge between nodes
    pub fn add_edge(&mut self, from: usize, edge: Edge<T>) {
        self.built = false;
        if from < self.nodes.len() {
            self.nodes[from].edges.push(edge);
        }
    }

    /// Add exact match edge
    pub fn add_exact_edge(&mut self, from: usize, value: T, to: usize) {
        self.add_edge(from, Edge::new(Matcher::Exact(value), to));
    }

    /// Add wildcard edge
    pub fn add_any_edge(&mut self, from: usize, to: usize) {
        self.add_edge(from, Edge::new(Matcher::Any, to));
    }

    /// Add predicate edge
    pub fn add_predicate_edge(
        &mut self,
        from: usize,
        pred: fn(&T) -> bool,
        name: &'static str,
        to: usize,
    ) {
        self.add_edge(from, Edge::new(Matcher::Predicate(pred, name), to));
    }

    /// Add epsilon edge
    pub fn add_epsilon_edge(&mut self, from: usize, to: usize) {
        self.add_edge(from, Edge::new(Matcher::Epsilon, to));
    }

    /// Mark a node as accepting for a pattern
    pub fn set_accepting(&mut self, node: usize, pattern_id: usize) {
        if let Some(n) = self.nodes.get_mut(node) {
            n.accepting = true;
            if !n.patterns.contains(&pattern_id) {
                n.patterns.push(pattern_id);
            }
        }
    }

    /// Build failure links (AC-style)
    pub fn build(&mut self) {
        if self.built {
            return;
        }

        let mut queue = VecDeque::new();

        let root_children: Vec<(T, usize)> = self.nodes[0]
            .goto
            .iter()
            .map(|(k, &v)| (k.clone(), v))
            .collect();

        for (_, child) in &root_children {
            self.nodes[*child].failure = 0;
            queue.push_back(*child);
        }

        while let Some(current) = queue.pop_front() {
            let current_failure = self.nodes[current].failure;
            let children: Vec<(T, usize)> = self.nodes[current]
                .goto
                .iter()
                .map(|(k, &v)| (k.clone(), v))
                .collect();

            for (symbol, child) in children {
                queue.push_back(child);

                let mut failure = current_failure;
                loop {
                    if let Some(&next) = self.nodes[failure].goto.get(&symbol) {
                        self.nodes[child].failure = next;
                        break;
                    }
                    if failure == 0 {
                        self.nodes[child].failure = 0;
                        break;
                    }
                    failure = self.nodes[failure].failure;
                }

                let child_failure = self.nodes[child].failure;
                if !self.nodes[child_failure].patterns.is_empty() {
                    self.nodes[child].output_link = Some(child_failure);
                } else {
                    self.nodes[child].output_link = self.nodes[child_failure].output_link;
                }
            }
        }

        for node in &mut self.nodes {
            node.edges.sort_by(|a, b| b.priority.cmp(&a.priority));
        }

        self.built = true;
    }

    /// Compute epsilon closure
    fn epsilon_closure(&self, states: &[usize]) -> HashSet<usize> {
        let mut closure = HashSet::new();
        let mut stack: Vec<usize> = states.to_vec();

        while let Some(state) = stack.pop() {
            if closure.insert(state) {
                for edge in &self.nodes[state].edges {
                    if edge.matcher.is_epsilon() && !closure.contains(&edge.target) {
                        stack.push(edge.target);
                    }
                }
            }
        }
        closure
    }

    /// AC-style next state
    fn ac_next_state(&self, current: usize, input: &T) -> usize {
        let mut state = current;
        loop {
            if let Some(&next) = self.nodes[state].goto.get(input) {
                return next;
            }
            if state == 0 {
                return 0;
            }
            state = self.nodes[state].failure;
        }
    }

    /// Hybrid next states
    fn hybrid_next_states(&self, current: usize, input: &T) -> Vec<(usize, Option<u32>)> {
        let mut results = Vec::new();

        if let Some(&next) = self.nodes[current].goto.get(input) {
            results.push((next, None));
        }

        for edge in &self.nodes[current].edges {
            if !edge.matcher.is_epsilon() && edge.matcher.matches(input) {
                if !results.iter().any(|(t, _)| *t == edge.target) {
                    results.push((edge.target, edge.action));
                }
            }
        }

        if results.is_empty() && current != 0 {
            let failure = self.nodes[current].failure;
            results.extend(self.hybrid_next_states(failure, input));
        }

        results
    }

    /// Collect all patterns at a state
    fn collect_patterns(&self, state: usize) -> Vec<usize> {
        let mut patterns = Vec::new();
        let mut current = Some(state);

        while let Some(s) = current {
            patterns.extend(&self.nodes[s].patterns);
            current = self.nodes[s].output_link;
        }

        patterns
    }

    /// AC-style search
    pub fn search_ac(&self, input: &[T]) -> Vec<(usize, usize)> {
        let mut matches = Vec::new();
        let mut state = 0;

        for (pos, symbol) in input.iter().enumerate() {
            state = self.ac_next_state(state, symbol);
            for pattern_id in self.collect_patterns(state) {
                matches.push((pos, pattern_id));
            }
        }

        matches
    }

    /// Full search with position info
    pub fn search(&self, input: &[T]) -> Vec<PatternMatch> {
        let has_nfa_edges = self.nodes.iter().any(|n| {
            n.edges
                .iter()
                .any(|e| !matches!(e.matcher, Matcher::Exact(_)))
        });

        if has_nfa_edges {
            self.search_nfa(input)
        } else {
            self.search_ac_full(input)
        }
    }

    /// AC search with full info
    fn search_ac_full(&self, input: &[T]) -> Vec<PatternMatch> {
        let mut matches = Vec::new();
        let mut state = 0;

        for (pos, symbol) in input.iter().enumerate() {
            state = self.ac_next_state(state, symbol);

            for pattern_id in self.collect_patterns(state) {
                let pattern_len = self
                    .patterns
                    .get(pattern_id)
                    .map(|p| p.elements.len())
                    .unwrap_or(self.nodes[state].depth);

                matches.push(PatternMatch {
                    start: pos + 1 - pattern_len,
                    end: pos + 1,
                    pattern_id,
                    actions: Vec::new(),
                });
            }
        }

        matches
    }

    /// NFA search
    fn search_nfa(&self, input: &[T]) -> Vec<PatternMatch> {
        let mut all_matches = Vec::new();

        for start_pos in 0..=input.len() {
            let initial_states = self.epsilon_closure(&[0]);
            let mut threads: Vec<Thread> = initial_states
                .iter()
                .map(|&node| Thread {
                    node,
                    start: start_pos,
                    actions: Vec::new(),
                })
                .collect();

            for (rel_pos, symbol) in input[start_pos..].iter().enumerate() {
                let pos = start_pos + rel_pos;
                let mut next_threads = Vec::new();

                for thread in threads {
                    if self.nodes[thread.node].accepting {
                        for &pattern_id in &self.nodes[thread.node].patterns {
                            all_matches.push(PatternMatch {
                                start: thread.start,
                                end: pos,
                                pattern_id,
                                actions: thread.actions.clone(),
                            });
                        }
                    }

                    for (next_node, action) in self.hybrid_next_states(thread.node, symbol) {
                        let mut new_actions = thread.actions.clone();
                        if let Some(a) = action {
                            new_actions.push(a);
                        }

                        for &eps_node in &self.epsilon_closure(&[next_node]) {
                            next_threads.push(Thread {
                                node: eps_node,
                                start: thread.start,
                                actions: new_actions.clone(),
                            });
                        }
                    }
                }

                threads = next_threads;
                if threads.is_empty() {
                    break;
                }
            }

            let end_pos = input.len();
            for thread in threads {
                if self.nodes[thread.node].accepting {
                    for &pattern_id in &self.nodes[thread.node].patterns {
                        all_matches.push(PatternMatch {
                            start: thread.start,
                            end: end_pos,
                            pattern_id,
                            actions: thread.actions.clone(),
                        });
                    }
                }
            }
        }

        all_matches.sort_by(|a, b| {
            a.start.cmp(&b.start).then_with(|| a.end.cmp(&b.end)).then_with(|| {
                let pa = self
                    .patterns
                    .get(a.pattern_id)
                    .map(|p| p.priority)
                    .unwrap_or(0);
                let pb = self
                    .patterns
                    .get(b.pattern_id)
                    .map(|p| p.priority)
                    .unwrap_or(0);
                pb.cmp(&pa)
            })
        });

        all_matches
    }

    /// Check if input matches any pattern from the start
    pub fn matches_prefix(&self, input: &[T]) -> Option<PatternMatch> {
        // Check if we have NFA edges that need special handling
        let has_nfa_edges = self.nodes.iter().any(|n| {
            n.edges
                .iter()
                .any(|e| !matches!(e.matcher, Matcher::Exact(_)))
        });

        if has_nfa_edges {
            // Use NFA search for patterns with wildcards/predicates
            self.search_nfa(input)
                .into_iter()
                .filter(|m| m.start == 0)
                .max_by(|a, b| {
                    a.end.cmp(&b.end).then_with(|| {
                        let pa = self.patterns.get(a.pattern_id).map(|p| p.priority).unwrap_or(0);
                        let pb = self.patterns.get(b.pattern_id).map(|p| p.priority).unwrap_or(0);
                        pa.cmp(&pb)
                    })
                })
        } else {
            // Use fast AC-style matching for literal patterns
            let mut state = 0;
            let mut best_match: Option<PatternMatch> = None;

            for (pos, symbol) in input.iter().enumerate() {
                state = self.ac_next_state(state, symbol);

                if self.nodes[state].accepting {
                    for &pattern_id in &self.nodes[state].patterns {
                        let m = PatternMatch {
                            start: 0,
                            end: pos + 1,
                            pattern_id,
                            actions: Vec::new(),
                        };
                        if best_match.as_ref().map_or(true, |prev| {
                            m.end > prev.end
                                || (m.end == prev.end
                                    && self
                                        .patterns
                                        .get(pattern_id)
                                        .map(|p| p.priority)
                                        .unwrap_or(0)
                                        > self
                                            .patterns
                                            .get(prev.pattern_id)
                                            .map(|p| p.priority)
                                            .unwrap_or(0))
                        }) {
                            best_match = Some(m);
                        }
                    }
                }
            }

            best_match
        }
    }

    /// Check if entire input matches a pattern
    pub fn matches_full(&self, input: &[T]) -> Option<PatternMatch> {
        self.matches_prefix(input).filter(|m| m.end == input.len())
    }

    /// Get pattern info by ID
    pub fn pattern(&self, id: usize) -> Option<&MachinePattern<T>> {
        self.patterns.get(id)
    }

    /// Get all patterns
    pub fn patterns(&self) -> &[MachinePattern<T>] {
        &self.patterns
    }
}

impl<T> Default for PatternMachine<T>
where
    T: Clone + Eq + Hash + Debug + PartialOrd,
{
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// Pattern Builder
// ============================================================================

/// Builder for creating complex patterns
pub struct PatternBuilder<'a, T> {
    machine: &'a mut PatternMachine<T>,
    current: usize,
    pattern_id: usize,
}

impl<'a, T: Clone + Eq + Hash + Debug + PartialOrd> PatternBuilder<'a, T> {
    /// Start building a new pattern
    pub fn new(machine: &'a mut PatternMachine<T>, name: &str) -> Self {
        let pattern_id = machine.patterns.len();
        machine.patterns.push(MachinePattern {
            id: pattern_id,
            name: name.to_string(),
            elements: Vec::new(),
            priority: 0,
            data: 0,
        });

        Self {
            machine,
            current: 0,
            pattern_id,
        }
    }

    /// Match exact value
    pub fn exact(mut self, value: T) -> Self {
        let next = self.machine.add_node();
        self.machine.add_exact_edge(self.current, value, next);
        self.current = next;
        self
    }

    /// Match any value
    pub fn any(mut self) -> Self {
        let next = self.machine.add_node();
        self.machine.add_any_edge(self.current, next);
        self.current = next;
        self
    }

    /// Match with predicate
    pub fn predicate(self, pred: fn(&T) -> bool, name: &'static str) -> Self {
        let next = self.machine.add_node();
        self.machine
            .add_predicate_edge(self.current, pred, name, next);
        Self { current: next, ..self }
    }

    /// Optional element
    pub fn optional(mut self, value: T) -> Self {
        let next = self.machine.add_node();
        self.machine.add_epsilon_edge(self.current, next);
        self.machine.add_exact_edge(self.current, value, next);
        self.current = next;
        self
    }

    /// Zero or more
    pub fn zero_or_more(mut self, value: T) -> Self {
        let next = self.machine.add_node();
        self.machine.add_epsilon_edge(self.current, next);
        self.machine.add_exact_edge(self.current, value, self.current);
        self.current = next;
        self
    }

    /// Set priority
    pub fn priority(self, priority: i16) -> Self {
        self.machine.patterns[self.pattern_id].priority = priority;
        self
    }

    /// Finish and mark as accepting
    pub fn finish(self) -> usize {
        self.machine.set_accepting(self.current, self.pattern_id);
        self.machine.built = false;
        self.pattern_id
    }
}

impl<T: Clone + Eq + Hash + Debug + PartialOrd> PatternMachine<T> {
    /// Start building a pattern with builder API
    pub fn build_pattern(&mut self, name: &str) -> PatternBuilder<'_, T> {
        PatternBuilder::new(self, name)
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pattern_rule_creation() {
        let rule = PatternRule::exact("int").optional();
        assert!(rule.optional);
        assert_eq!(rule.value, "int");
    }

    #[test]
    fn test_logical_op() {
        assert!(LogicalOp::And.evaluate(true, true));
        assert!(!LogicalOp::And.evaluate(true, false));
        assert!(LogicalOp::Or.evaluate(true, false));
        assert!(LogicalOp::Xor.evaluate(true, false));
        assert!(!LogicalOp::Xor.evaluate(true, true));
    }

    #[test]
    fn test_pattern_machine_literal() {
        let mut machine: PatternMachine<char> = PatternMachine::new();
        machine.add_literal_pattern("he", &['h', 'e']);
        machine.add_literal_pattern("she", &['s', 'h', 'e']);
        machine.build();

        let text: Vec<char> = "ushers".chars().collect();
        let matches = machine.search(&text);
        assert!(matches.iter().any(|m| m.pattern_id == 1)); // she
        assert!(matches.iter().any(|m| m.pattern_id == 0)); // he
    }

    #[test]
    fn test_pattern_machine_wildcard() {
        let mut machine: PatternMachine<char> = PatternMachine::new();
        machine
            .build_pattern("a*c")
            .exact('a')
            .any()
            .exact('c')
            .finish();
        machine.build();

        let text1: Vec<char> = "abc".chars().collect();
        let text2: Vec<char> = "aXc".chars().collect();
        assert!(machine.matches_full(&text1).is_some());
        assert!(machine.matches_full(&text2).is_some());
    }

    #[test]
    fn test_pattern_match_to_entry() {
        let m = PatternMatch {
            start: 0,
            end: 5,
            pattern_id: 1,
            actions: vec![],
        };
        let entry = m.to_entry();
        assert!(entry.kind().is_some());
    }

    // ========== Unified Pattern System Tests ==========

    #[test]
    fn test_pattern_builder() {
        let pattern: Pattern<String> = Pattern::definition(0, "test", vec![])
            .with_category("cat")
            .with_priority(100)
            .with_min_tokens(3)
            .with_description("A test pattern");

        assert_eq!(pattern.name(), "test");
        assert_eq!(pattern.category(), "cat");
        assert_eq!(pattern.priority(), 100);
        assert_eq!(pattern.min_tokens(), 3);
        assert_eq!(pattern.description(), "A test pattern");
    }

    #[test]
    fn test_unified_pattern_definition() {
        let pattern: Pattern<String> = Pattern::definition(
            0,
            "func_def",
            vec![PatternRule::type_keyword(), PatternRule::identifier()],
        );

        assert!(pattern.is_definition());
        assert!(!pattern.is_literal());
        assert_eq!(pattern.name(), "func_def");
        assert_eq!(pattern.id(), 0);

        if let Pattern::Definition { rules, .. } = &pattern {
            assert_eq!(rules.len(), 2);
        }
    }

    #[test]
    fn test_unified_pattern_literal() {
        let pattern: Pattern<char> = Pattern::literal(1, "abc", vec!['a', 'b', 'c']);

        assert!(pattern.is_literal());
        assert!(!pattern.is_definition());
        assert_eq!(pattern.name(), "abc");

        if let Pattern::Literal { elements, .. } = &pattern {
            assert_eq!(elements, &vec!['a', 'b', 'c']);
        }
    }

    #[test]
    fn test_unified_pattern_common_accessors() {
        let pattern: Pattern<String> = Pattern::wildcard(5, "wild")
            .with_category("test")
            .with_priority(50);

        assert!(pattern.is_wildcard());
        assert_eq!(pattern.id(), 5);
        assert_eq!(pattern.name(), "wild");
        assert_eq!(pattern.category(), "test");
        assert_eq!(pattern.priority(), 50);
    }

    // Note: Patterns container tests removed - use Web for unified storage
    // See web.rs tests for Web-based pattern storage tests

    #[test]
    fn test_pattern_to_entry() {
        let pattern: Pattern<String> = Pattern::definition(1, "test_entry", vec![])
            .with_category("testing")
            .with_priority(75)
            .with_description("Entry test");

        let entry = pattern.to_entry();
        assert_eq!(entry.name(), Some("test_entry"));
    }
}
