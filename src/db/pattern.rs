//! Pattern Matching System for DB Framework
//!
//! Provides pattern definition, storage, and matching using a hybrid automaton.
//! Combines Aho-Corasick (fast literal matching) with NFA (flexible patterns).
//!
//! Key components:
//! - `Pattern`: Unified enum for all pattern types (Definition, Literal, Predicate, Sequence)
//! - `PatternCore`: Common fields shared across all pattern variants
//! - `Patterns`: Container for pattern collections with builder methods
//! - `PatternDB`: Database of pattern definitions
//! - `PatternMachine`: Hybrid automaton for pattern matching
//! - `PatternMatch`: Result of a successful match

use crate::db::store::IndexedStore;
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
    /// Branch pattern - takes different paths based on logical operations
    Branch(BranchPattern),
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

/// Branch pattern for non-linear pattern matching
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BranchPattern {
    /// The logical operation to perform
    pub operation: LogicalOp,
    /// Left operand
    pub left: Box<BranchOperand>,
    /// Right operand
    pub right: Box<BranchOperand>,
    /// Pattern to follow if condition is true
    pub true_path: Vec<PatternRule>,
    /// Pattern to follow if condition is false
    pub false_path: Vec<PatternRule>,
    /// Optional description for debugging
    pub description: Option<String>,
}

/// Operand for branch operations
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BranchOperand {
    /// Reference to another pattern by name
    PatternRef(String),
    /// Nested branch operation
    NestedBranch(BranchPattern),
    /// Direct rule evaluation
    Rule(PatternRule),
}

impl BranchPattern {
    /// Create a new branch pattern
    pub fn new(operation: LogicalOp, left: BranchOperand, right: BranchOperand) -> Self {
        Self {
            operation,
            left: Box::new(left),
            right: Box::new(right),
            true_path: Vec::new(),
            false_path: Vec::new(),
            description: None,
        }
    }

    /// Add true path rules
    pub fn with_true_path(mut self, rules: Vec<PatternRule>) -> Self {
        self.true_path = rules;
        self
    }

    /// Add false path rules
    pub fn with_false_path(mut self, rules: Vec<PatternRule>) -> Self {
        self.false_path = rules;
        self
    }

    /// Add description
    pub fn with_description(mut self, description: impl Into<String>) -> Self {
        self.description = Some(description.into());
        self
    }

    /// Evaluate the logical operation
    pub fn evaluate(&self, left_result: bool, right_result: bool) -> bool {
        self.operation.evaluate(left_result, right_result)
    }
}

impl BranchOperand {
    pub fn pattern_ref(name: impl Into<String>) -> Self {
        Self::PatternRef(name.into())
    }

    pub fn nested_branch(branch: BranchPattern) -> Self {
        Self::NestedBranch(branch)
    }

    pub fn rule(rule: PatternRule) -> Self {
        Self::Rule(rule)
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

    /// Create branch rule
    pub fn branch(
        operation: LogicalOp,
        left: BranchOperand,
        right: BranchOperand,
        true_path: Vec<PatternRule>,
        false_path: Vec<PatternRule>,
    ) -> Self {
        Self {
            rule_type: RuleType::Branch(BranchPattern {
                operation,
                left: Box::new(left),
                right: Box::new(right),
                true_path,
                false_path,
                description: None,
            }),
            value: format!("{:?}", operation),
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

/// Common fields shared across all pattern types.
/// Extract these to avoid duplicating code for each variant.
#[derive(Debug, Clone)]
pub struct PatternCore {
    /// Unique pattern identifier
    pub id: usize,
    /// Pattern name
    pub name: String,
    /// Category for grouping
    pub category: String,
    /// Priority for matching order (higher = matched first)
    pub priority: i32,
    /// Minimum tokens required to match
    pub min_tokens: usize,
    /// Optional description
    pub description: String,
    /// Example usages
    pub examples: Vec<String>,
    /// Runtime statistics
    pub stats: Option<PatternStats>,
    /// User-defined data
    pub data: u64,
}

impl PatternCore {
    /// Create a new pattern core with required fields
    pub fn new(id: usize, name: impl Into<String>) -> Self {
        Self {
            id,
            name: name.into(),
            category: String::new(),
            priority: 0,
            min_tokens: 1,
            description: String::new(),
            examples: Vec::new(),
            stats: None,
            data: 0,
        }
    }

    /// Set category
    pub fn with_category(mut self, category: impl Into<String>) -> Self {
        self.category = category.into();
        self
    }

    /// Set priority
    pub fn with_priority(mut self, priority: i32) -> Self {
        self.priority = priority;
        self
    }

    /// Set minimum tokens
    pub fn with_min_tokens(mut self, min_tokens: usize) -> Self {
        self.min_tokens = min_tokens;
        self
    }

    /// Set description
    pub fn with_description(mut self, desc: impl Into<String>) -> Self {
        self.description = desc.into();
        self
    }

    /// Add examples
    pub fn with_examples(mut self, examples: Vec<String>) -> Self {
        self.examples = examples;
        self
    }

    /// Set user data
    pub fn with_data(mut self, data: u64) -> Self {
        self.data = data;
        self
    }
}

impl Default for PatternCore {
    fn default() -> Self {
        Self::new(0, "")
    }
}

/// Unified pattern enum containing all pattern variants.
/// Each variant shares common fields via PatternCore.
#[derive(Debug, Clone)]
pub enum Pattern<T = String> {
    /// Rule-based pattern definition (from PatternDef)
    Definition {
        core: PatternCore,
        rules: Vec<PatternRule>,
    },
    /// Literal/exact sequence match (from MachinePattern)
    Literal {
        core: PatternCore,
        elements: Vec<T>,
    },
    /// Predicate-based pattern
    Predicate {
        core: PatternCore,
        predicate_name: &'static str,
    },
    /// Wildcard pattern - matches any sequence
    Wildcard {
        core: PatternCore,
    },
    /// Sequence of sub-patterns
    Sequence {
        core: PatternCore,
        patterns: Vec<Pattern<T>>,
    },
    /// Choice between patterns (matches first successful)
    Choice {
        core: PatternCore,
        alternatives: Vec<Pattern<T>>,
    },
    /// Optional pattern (zero or one)
    Optional {
        core: PatternCore,
        inner: Box<Pattern<T>>,
    },
    /// Repeating pattern (zero or more)
    Repeat {
        core: PatternCore,
        inner: Box<Pattern<T>>,
        min: usize,
        max: Option<usize>,
    },
}

impl<T> Pattern<T> {
    /// Get the core fields (common across all variants)
    pub fn core(&self) -> &PatternCore {
        match self {
            Pattern::Definition { core, .. } => core,
            Pattern::Literal { core, .. } => core,
            Pattern::Predicate { core, .. } => core,
            Pattern::Wildcard { core } => core,
            Pattern::Sequence { core, .. } => core,
            Pattern::Choice { core, .. } => core,
            Pattern::Optional { core, .. } => core,
            Pattern::Repeat { core, .. } => core,
        }
    }

    /// Get mutable core fields
    pub fn core_mut(&mut self) -> &mut PatternCore {
        match self {
            Pattern::Definition { core, .. } => core,
            Pattern::Literal { core, .. } => core,
            Pattern::Predicate { core, .. } => core,
            Pattern::Wildcard { core } => core,
            Pattern::Sequence { core, .. } => core,
            Pattern::Choice { core, .. } => core,
            Pattern::Optional { core, .. } => core,
            Pattern::Repeat { core, .. } => core,
        }
    }

    /// Convenience accessors for common fields
    pub fn id(&self) -> usize { self.core().id }
    pub fn name(&self) -> &str { &self.core().name }
    pub fn category(&self) -> &str { &self.core().category }
    pub fn priority(&self) -> i32 { self.core().priority }
    pub fn min_tokens(&self) -> usize { self.core().min_tokens }
    pub fn description(&self) -> &str { &self.core().description }

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
            core: PatternCore::new(id, name),
            rules,
        }
    }

    /// Create a literal pattern
    pub fn literal(id: usize, name: impl Into<String>, elements: Vec<T>) -> Self {
        Pattern::Literal {
            core: PatternCore::new(id, name),
            elements,
        }
    }

    /// Create a wildcard pattern
    pub fn wildcard(id: usize, name: impl Into<String>) -> Self {
        Pattern::Wildcard {
            core: PatternCore::new(id, name),
        }
    }

    /// Convert to Entry representation
    pub fn to_entry(&self) -> Entry {
        let core = self.core();
        let mut node = Entry::node("Pattern", &core.name);
        node.set_attr("id", Entry::usize(core.id));
        node.set_attr("category", Entry::string(&core.category));
        node.set_attr("priority", Entry::i32(core.priority));
        node.set_attr("min_tokens", Entry::usize(core.min_tokens));
        
        let variant_name = match self {
            Pattern::Definition { .. } => "definition",
            Pattern::Literal { .. } => "literal",
            Pattern::Predicate { .. } => "predicate",
            Pattern::Wildcard { .. } => "wildcard",
            Pattern::Sequence { .. } => "sequence",
            Pattern::Choice { .. } => "choice",
            Pattern::Optional { .. } => "optional",
            Pattern::Repeat { .. } => "repeat",
        };
        node.set_attr("type", Entry::string(variant_name));

        if !core.description.is_empty() {
            node.set_attr("description", Entry::string(&core.description));
        }
        if !core.examples.is_empty() {
            let examples: Vec<Entry> = core.examples.iter().map(|e| Entry::string(e)).collect();
            node.set_attr("examples", Entry::vec(examples));
        }

        node
    }

    /// Reconstruct a Pattern from an Entry (partial reconstruction - core fields only)
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
        
        let core = PatternCore {
            id,
            name,
            category,
            priority,
            min_tokens,
            description,
            examples: Vec::new(),
            stats: None,
            data: 0,
        };
        
        // Reconstruct variant based on type
        Some(match pattern_type {
            "definition" => Pattern::Definition { core, rules: Vec::new() },
            "literal" => Pattern::Literal { core, elements: Vec::new() },
            "predicate" => Pattern::Predicate { core, predicate_name: "unknown" },
            "sequence" => Pattern::Sequence { core, patterns: Vec::new() },
            "choice" => Pattern::Choice { core, alternatives: Vec::new() },
            "optional" => Pattern::Optional { core, inner: Box::new(Pattern::Wildcard { core: PatternCore::default() }) },
            "repeat" => Pattern::Repeat { core, inner: Box::new(Pattern::Wildcard { core: PatternCore::default() }), min: 0, max: None },
            _ => Pattern::Wildcard { core },
        })
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

// Note: The Patterns container has been removed. Use Web for unified storage:
//   let mut web = Web::new();
//   web.add(&pattern);  // Add Pattern via Build trait
//   web.by_kind("Pattern");  // Query all patterns
//   web.by_category("function");  // Query by category
//   web.by_name("function_definition");  // Query by name

// ============================================================================
// Legacy Pattern Definition (kept for backward compatibility)
// ============================================================================

/// A pattern definition with all its rules and metadata
#[derive(Debug, Clone)]
pub struct PatternDef {
    pub name: String,
    pub category: String,
    pub priority: usize,
    pub min_tokens: usize,
    pub rules: Vec<PatternRule>,
    pub description: String,
    pub examples: Vec<String>,
    pub stats: Option<PatternStats>,
}

impl PatternDef {
    /// Create a new pattern definition
    pub fn new(name: impl Into<String>, category: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            category: category.into(),
            priority: 100,
            min_tokens: 1,
            rules: Vec::new(),
            description: String::new(),
            examples: Vec::new(),
            stats: None,
        }
    }

    /// Set priority
    pub fn with_priority(mut self, priority: usize) -> Self {
        self.priority = priority;
        self
    }

    /// Set minimum tokens
    pub fn with_min_tokens(mut self, min_tokens: usize) -> Self {
        self.min_tokens = min_tokens;
        self
    }

    /// Add rules
    pub fn with_rules(mut self, rules: Vec<PatternRule>) -> Self {
        self.rules = rules;
        self
    }

    /// Set description
    pub fn with_description(mut self, description: impl Into<String>) -> Self {
        self.description = description.into();
        self
    }

    /// Add examples
    pub fn with_examples(mut self, examples: Vec<String>) -> Self {
        self.examples = examples;
        self
    }

    /// Convert to Entry representation
    pub fn to_entry(&self) -> Entry {
        let mut node = Entry::node("Pattern", &self.name);
        node.set_attr("category", Entry::string(&self.category));
        node.set_attr("priority", Entry::usize(self.priority));
        node.set_attr("min_tokens", Entry::usize(self.min_tokens));
        node.set_attr("description", Entry::string(&self.description));

        if !self.examples.is_empty() {
            let examples: Vec<Entry> = self.examples.iter().map(|e| Entry::string(e)).collect();
            node.set_attr("examples", Entry::vec(examples));
        }

        node
    }

    /// Reconstruct a PatternDef from an Entry
    pub fn from_entry(entry: &Entry) -> Option<PatternDef> {
        if entry.kind() != Some("Pattern") {
            return None;
        }
        
        let name = entry.name()?.to_string();
        let category = entry.get_string_attr("category").unwrap_or("").to_string();
        let priority = entry.get_number_attr("priority").map(|n| n as usize).unwrap_or(100);
        let min_tokens = entry.get_number_attr("min_tokens").map(|n| n as usize).unwrap_or(1);
        let description = entry.get_string_attr("description").unwrap_or("").to_string();
        
        let mut pattern = PatternDef::new(name, category)
            .with_priority(priority)
            .with_min_tokens(min_tokens)
            .with_description(description);
        
        // Reconstruct examples
        if let Some(examples_vec) = entry.get_vec_attr("examples") {
            pattern.examples = examples_vec.iter()
                .filter_map(|e| if let Entry::String(s, _) = e { Some(s.clone()) } else { None })
                .collect();
        }
        
        Some(pattern)
    }
}

/// Implement Build trait for PatternDef
impl Build for PatternDef {
    fn to_entry(&self) -> Entry {
        PatternDef::to_entry(self)
    }
    
    fn kind(&self) -> &str {
        "Pattern"
    }
    
    fn name(&self) -> Option<&str> {
        Some(&self.name)
    }
    
    fn category(&self) -> Option<&str> {
        if self.category.is_empty() { None } else { Some(&self.category) }
    }
    
    fn priority(&self) -> i16 {
        self.priority as i16
    }
}

// ============================================================================
// Pattern Database
// ============================================================================

/// Pattern database - stores all pattern definitions using IndexedStore
#[derive(Debug, Clone)]
pub struct PatternDB {
    /// Patterns stored using generic IndexedStore
    store: IndexedStore<PatternDef>,
}

impl PatternDB {
    /// Create a new empty pattern database
    pub fn new() -> Self {
        Self {
            store: IndexedStore::new(),
        }
    }

    /// Create a pattern database with C language patterns
    pub fn new_c_patterns() -> Self {
        let mut db = Self::new();
        db.populate_c_patterns();
        db.rebuild_priority_index();
        db
    }

    /// Populate database with C language patterns
    fn populate_c_patterns(&mut self) {
        self.add_function_patterns();
        self.add_struct_patterns();
        self.add_enum_patterns();
        self.add_typedef_patterns();
        self.add_variable_patterns();
        self.add_macro_patterns();
        self.add_array_patterns();
        self.add_control_flow_patterns();
    }

    fn add_function_patterns(&mut self) {
        self.add_pattern(
            PatternDef::new("function_definition", "function")
                .with_priority(920)
                .with_min_tokens(6)
                .with_rules(vec![
                    PatternRule::type_keyword(),
                    PatternRule::identifier(),
                    PatternRule::exact("("),
                    PatternRule::any().optional().with_can_repeat(true),
                    PatternRule::exact(")"),
                    PatternRule::exact("{"),
                ])
                .with_description("Function definition with body")
                .with_examples(vec![
                    "int main() { }".to_string(),
                    "void foo(int x) { return; }".to_string(),
                ]),
        );

        self.add_pattern(
            PatternDef::new("function_declaration", "function")
                .with_priority(910)
                .with_min_tokens(5)
                .with_rules(vec![
                    PatternRule::type_keyword(),
                    PatternRule::identifier(),
                    PatternRule::exact("("),
                    PatternRule::any().optional().with_can_repeat(true),
                    PatternRule::exact(")"),
                    PatternRule::exact(";"),
                ])
                .with_description("Function declaration without body")
                .with_examples(vec!["int add(int a, int b);".to_string()]),
        );
    }

    fn add_struct_patterns(&mut self) {
        self.add_pattern(
            PatternDef::new("struct_definition", "struct")
                .with_priority(140)
                .with_min_tokens(4)
                .with_rules(vec![
                    PatternRule::exact("struct"),
                    PatternRule::identifier().optional(),
                    PatternRule::exact("{"),
                    PatternRule::any().with_can_repeat(true),
                    PatternRule::exact("}"),
                    PatternRule::exact(";").optional(),
                ])
                .with_description("Struct definition with fields")
                .with_examples(vec!["struct Point { int x; int y; };".to_string()]),
        );
    }

    fn add_enum_patterns(&mut self) {
        self.add_pattern(
            PatternDef::new("enum_definition", "enum")
                .with_priority(140)
                .with_min_tokens(4)
                .with_rules(vec![
                    PatternRule::exact("enum"),
                    PatternRule::identifier().optional(),
                    PatternRule::exact("{"),
                    PatternRule::any().with_can_repeat(true),
                    PatternRule::exact("}"),
                    PatternRule::exact(";").optional(),
                ])
                .with_description("Enum definition with values")
                .with_examples(vec!["enum Status { OK, ERROR };".to_string()]),
        );
    }

    fn add_typedef_patterns(&mut self) {
        self.add_pattern(
            PatternDef::new("typedef_struct", "typedef")
                .with_priority(130)
                .with_min_tokens(6)
                .with_rules(vec![
                    PatternRule::exact("typedef"),
                    PatternRule::exact("struct"),
                    PatternRule::identifier().optional(),
                    PatternRule::exact("{"),
                    PatternRule::any().with_can_repeat(true),
                    PatternRule::exact("}"),
                    PatternRule::identifier(),
                    PatternRule::exact(";"),
                ])
                .with_description("Typedef struct definition")
                .with_examples(vec![
                    "typedef struct Point { int x; int y; } Point;".to_string()
                ]),
        );

        self.add_pattern(
            PatternDef::new("typedef_alias", "typedef")
                .with_priority(125)
                .with_min_tokens(4)
                .with_rules(vec![
                    PatternRule::exact("typedef"),
                    PatternRule::type_keyword(),
                    PatternRule::identifier(),
                    PatternRule::exact(";"),
                ])
                .with_description("Simple typedef alias")
                .with_examples(vec!["typedef int MyInt;".to_string()]),
        );
    }

    fn add_variable_patterns(&mut self) {
        self.add_pattern(
            PatternDef::new("global_variable", "global")
                .with_priority(800)
                .with_min_tokens(3)
                .with_rules(vec![
                    PatternRule::type_keyword(),
                    PatternRule::identifier(),
                    PatternRule::one_of(vec![";".to_string(), "=".to_string()])
                        .with_forbidden_next(vec!["(".to_string()]),
                ])
                .with_description("Global variable declaration")
                .with_examples(vec![
                    "int count;".to_string(),
                    "float pi = 3.14;".to_string(),
                ]),
        );

        self.add_pattern(
            PatternDef::new("local_variable", "variable")
                .with_priority(110)
                .with_min_tokens(3)
                .with_rules(vec![
                    PatternRule::type_keyword(),
                    PatternRule::identifier(),
                    PatternRule::one_of(vec![";".to_string(), "=".to_string()]),
                ])
                .with_description("Local variable declaration")
                .with_examples(vec![
                    "int x;".to_string(),
                    "char buffer[256];".to_string(),
                ]),
        );
    }

    fn add_macro_patterns(&mut self) {
        self.add_pattern(
            PatternDef::new("define_macro", "macro")
                .with_priority(920)
                .with_min_tokens(3)
                .with_rules(vec![
                    PatternRule::exact("#define"),
                    PatternRule::identifier(),
                    PatternRule::any().optional().with_can_repeat(true),
                ])
                .with_description("Macro definition")
                .with_examples(vec![
                    "#define PI 3.14".to_string(),
                    "#define MAX(a,b) ((a)>(b)?(a):(b))".to_string(),
                ]),
        );

        self.add_pattern(
            PatternDef::new("include_directive", "macro")
                .with_priority(950)
                .with_min_tokens(2)
                .with_rules(vec![PatternRule::exact("#include"), PatternRule::any()])
                .with_description("Include directive")
                .with_examples(vec![
                    "#include <stdio.h>".to_string(),
                    "#include \"myheader.h\"".to_string(),
                ]),
        );
    }

    fn add_array_patterns(&mut self) {
        self.add_pattern(
            PatternDef::new("array_declaration", "array")
                .with_priority(150)
                .with_min_tokens(5)
                .with_rules(vec![
                    PatternRule::type_keyword(),
                    PatternRule::identifier(),
                    PatternRule::exact("["),
                    PatternRule::any().optional(),
                    PatternRule::exact("]"),
                    PatternRule::exact(";").optional(),
                ])
                .with_description("Array declaration")
                .with_examples(vec![
                    "int numbers[5];".to_string(),
                    "char buffer[MAX_SIZE];".to_string(),
                ]),
        );
    }

    fn add_control_flow_patterns(&mut self) {
        self.add_pattern(
            PatternDef::new("for_loop", "loop")
                .with_priority(120)
                .with_min_tokens(8)
                .with_rules(vec![
                    PatternRule::exact("for"),
                    PatternRule::exact("("),
                    PatternRule::any().with_can_repeat(true),
                    PatternRule::exact(")"),
                    PatternRule::exact("{").optional(),
                ])
                .with_description("For loop structure")
                .with_examples(vec!["for(int i=0; i<10; i++) sum += i;".to_string()]),
        );

        self.add_pattern(
            PatternDef::new("while_loop", "loop")
                .with_priority(120)
                .with_min_tokens(4)
                .with_rules(vec![
                    PatternRule::exact("while"),
                    PatternRule::exact("("),
                    PatternRule::any().with_can_repeat(true),
                    PatternRule::exact(")"),
                ])
                .with_description("While loop structure")
                .with_examples(vec!["while(x > 0) x--;".to_string()]),
        );

        self.add_pattern(
            PatternDef::new("if_statement", "conditional")
                .with_priority(125)
                .with_min_tokens(4)
                .with_rules(vec![
                    PatternRule::exact("if"),
                    PatternRule::exact("("),
                    PatternRule::any().with_can_repeat(true),
                    PatternRule::exact(")"),
                ])
                .with_description("If statement")
                .with_examples(vec!["if(x > 0) return x;".to_string()]),
        );

        self.add_pattern(
            PatternDef::new("switch_statement", "conditional")
                .with_priority(120)
                .with_min_tokens(5)
                .with_rules(vec![
                    PatternRule::exact("switch"),
                    PatternRule::exact("("),
                    PatternRule::any().with_can_repeat(true),
                    PatternRule::exact(")"),
                    PatternRule::exact("{"),
                ])
                .with_description("Switch statement")
                .with_examples(vec!["switch(value) { case 1: break; }".to_string()]),
        );
    }

    /// Add a pattern to the database
    pub fn add_pattern(&mut self, pattern: PatternDef) {
        let name = pattern.name.clone();
        let category = pattern.category.clone();
        let priority = pattern.priority as i32;
        self.store.add(name, category, priority, pattern);
    }

    /// Get a pattern by name
    pub fn get_pattern(&self, name: &str) -> Option<&PatternDef> {
        self.store.get(name)
    }

    /// Get mutable pattern
    pub fn get_pattern_mut(&mut self, name: &str) -> Option<&mut PatternDef> {
        self.store.get_mut(name)
    }

    /// Get all patterns in a category
    pub fn get_category_patterns(&self, category: &str) -> Vec<&PatternDef> {
        self.store.get_group(category)
    }

    /// Get patterns sorted by priority
    pub fn get_by_priority(&mut self) -> Vec<&PatternDef> {
        self.store.by_priority()
    }

    /// Rebuild the priority index
    fn rebuild_priority_index(&mut self) {
        self.store.rebuild_priority_index(|p| p.priority as i32);
    }

    /// Get all pattern names
    pub fn pattern_names(&self) -> Vec<&str> {
        self.store.ids().map(|s| s.as_str()).collect()
    }

    /// Get all categories
    pub fn category_names(&self) -> Vec<&str> {
        self.store.group_names().map(|s| s.as_str()).collect()
    }

    /// Number of patterns
    pub fn len(&self) -> usize {
        self.store.len()
    }

    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.store.is_empty()
    }

    /// Remove a pattern by name
    pub fn remove_pattern(&mut self, name: &str) -> Option<PatternDef> {
        self.store.remove(name)
    }

    /// Iterate over all patterns
    pub fn iter(&self) -> impl Iterator<Item = (&String, &PatternDef)> {
        self.store.iter()
    }
}

impl Default for PatternDB {
    fn default() -> Self {
        Self::new()
    }
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
    fn test_pattern_db_creation() {
        let db = PatternDB::new_c_patterns();
        assert!(db.get_pattern("function_definition").is_some());
        assert!(db.get_pattern("struct_definition").is_some());
    }

    #[test]
    fn test_category_patterns() {
        let db = PatternDB::new_c_patterns();
        let function_patterns = db.get_category_patterns("function");
        assert!(!function_patterns.is_empty());
    }

    #[test]
    fn test_priority_ordering() {
        let mut db = PatternDB::new_c_patterns();
        let by_priority = db.get_by_priority();
        for window in by_priority.windows(2) {
            assert!(window[0].priority >= window[1].priority);
        }
    }

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
    fn test_pattern_def_to_entry() {
        let pattern = PatternDef::new("test_pattern", "test")
            .with_priority(100)
            .with_description("A test pattern");
        let entry = pattern.to_entry();
        assert_eq!(entry.name(), Some("test_pattern"));
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
    fn test_pattern_core_builder() {
        let core = PatternCore::new(0, "test")
            .with_category("cat")
            .with_priority(100)
            .with_min_tokens(3)
            .with_description("A test pattern")
            .with_examples(vec!["ex1".to_string(), "ex2".to_string()])
            .with_data(42);

        assert_eq!(core.name, "test");
        assert_eq!(core.category, "cat");
        assert_eq!(core.priority, 100);
        assert_eq!(core.min_tokens, 3);
        assert_eq!(core.description, "A test pattern");
        assert_eq!(core.examples.len(), 2);
        assert_eq!(core.data, 42);
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
        let mut pattern: Pattern<String> = Pattern::Wildcard {
            core: PatternCore::new(5, "wild")
                .with_category("test")
                .with_priority(50),
        };

        assert!(pattern.is_wildcard());
        assert_eq!(pattern.id(), 5);
        assert_eq!(pattern.name(), "wild");
        assert_eq!(pattern.category(), "test");
        assert_eq!(pattern.priority(), 50);

        // Test mutable access
        pattern.core_mut().priority = 100;
        assert_eq!(pattern.priority(), 100);
    }

    // Note: Patterns container tests removed - use Web for unified storage
    // See web.rs tests for Web-based pattern storage tests

    #[test]
    fn test_pattern_to_entry() {
        let pattern: Pattern<String> = Pattern::Definition {
            core: PatternCore::new(1, "test_entry")
                .with_category("testing")
                .with_priority(75)
                .with_description("Entry test"),
            rules: vec![],
        };

        let entry = pattern.to_entry();
        assert_eq!(entry.name(), Some("test_entry"));
    }
}
