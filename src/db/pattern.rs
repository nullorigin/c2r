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

use crate::db::web::{Build, Entry};
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
    /// Match any token except those in the exclusion list (stored in value as pipe-separated)
    AnyExcept,
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
// Extract Context for Callbacks
// ============================================================================

/// Mutable context passed to extraction callbacks.
/// Handlers can extend this with their own data via the `extra` field.
#[derive(Debug, Clone, Default)]
pub struct ExtractContext {
    /// Current token being matched
    pub current_token: String,
    /// Current token index
    pub token_idx: usize,
    /// All tokens being matched
    pub tokens: Vec<String>,
    /// Matched token (if rule matched)
    pub matched: Option<String>,
    /// Whether the rule matched
    pub did_match: bool,
    /// Modifiers detected (const, static, extern, etc.)
    pub modifiers: HashSet<String>,
    /// Named values extracted (e.g., "type" -> "int", "name" -> "foo")
    pub values: HashMap<String, String>,
    /// List values (e.g., "params" -> ["int a", "int b"])
    pub lists: HashMap<String, Vec<String>>,
    /// Nested pattern results
    pub nested: HashMap<String, Box<ExtractContext>>,
    /// Generic flags for handler-specific use
    pub flags: HashMap<String, bool>,
    /// Generic counters for handler-specific use  
    pub counters: HashMap<String, i32>,
}

impl ExtractContext {
    pub fn new(tokens: &[String]) -> Self {
        Self {
            tokens: tokens.to_vec(),
            ..Default::default()
        }
    }

    /// Set a flag
    pub fn set_flag(&mut self, name: &str, value: bool) {
        self.flags.insert(name.to_string(), value);
    }

    /// Get a flag (defaults to false)
    pub fn flag(&self, name: &str) -> bool {
        self.flags.get(name).copied().unwrap_or(false)
    }

    /// Set a value
    pub fn set_value(&mut self, name: &str, value: &str) {
        self.values.insert(name.to_string(), value.to_string());
    }

    /// Get a value
    pub fn value(&self, name: &str) -> Option<&str> {
        self.values.get(name).map(|s| s.as_str())
    }

    /// Add to a list
    pub fn push_list(&mut self, name: &str, value: &str) {
        self.lists
            .entry(name.to_string())
            .or_default()
            .push(value.to_string());
    }

    /// Get a list
    pub fn list(&self, name: &str) -> Option<&Vec<String>> {
        self.lists.get(name)
    }

    /// Add modifier
    pub fn add_modifier(&mut self, modifier: &str) {
        self.modifiers.insert(modifier.to_string());
    }

    /// Check modifier
    pub fn has_modifier(&self, modifier: &str) -> bool {
        self.modifiers.contains(modifier)
    }

    /// Increment counter
    pub fn inc(&mut self, name: &str) {
        *self.counters.entry(name.to_string()).or_insert(0) += 1;
    }

    /// Get counter
    pub fn count(&self, name: &str) -> i32 {
        self.counters.get(name).copied().unwrap_or(0)
    }

    /// Store nested context
    pub fn set_nested(&mut self, name: &str, ctx: ExtractContext) {
        self.nested.insert(name.to_string(), Box::new(ctx));
    }

    /// Get nested context
    pub fn get_nested(&self, name: &str) -> Option<&ExtractContext> {
        self.nested.get(name).map(|b| b.as_ref())
    }
}

/// Callback operation type for pattern rule transformations
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CallbackOp {
    /// Apply the callback (with = true)
    With,
    /// Invert/negate the callback (without = false)
    Without,
    /// Check condition only, don't modify
    Check,
    /// Apply if condition matches
    ApplyIf,
    /// Apply unless condition matches
    ApplyUnless,
}

impl CallbackOp {
    pub fn name(&self) -> &'static str {
        match self {
            Self::With => "with",
            Self::Without => "without",
            Self::Check => "check",
            Self::ApplyIf => "apply_if",
            Self::ApplyUnless => "apply_unless",
        }
    }

    pub fn is_positive(&self) -> bool {
        matches!(self, Self::With | Self::ApplyIf)
    }
}

/// Callback function type for pattern rules.
///
/// Enables functional transformations of PatternRule based on conditions.
/// Can be used to replace if/else chains in handler extract functions.
///
/// # Example
/// ```ignore
/// let optional_cb = PatternRuleCallback::new("optional", |rule, flag| {
///     rule.clone().with_optional(flag)
/// });
///
/// // Apply the callback
/// let optional_rule = optional_cb.with(&base_rule);
/// // Invert the callback  
/// let required_rule = optional_cb.without(&base_rule);
/// ```
pub struct PatternRuleCallback {
    /// The callback function: takes a rule and bool flag, returns modified rule
    inner: Box<dyn Fn(&PatternRule, bool) -> PatternRule + Send + Sync>,
    /// Name identifier for this callback
    name: String,
    /// Description of what this callback does
    description: String,
    /// Default operation when called without explicit op
    default_op: CallbackOp,
}

impl PatternRuleCallback {
    /// Create a new callback with a function
    pub fn new<F>(name: &str, callback: F) -> Self
    where
        F: Fn(&PatternRule, bool) -> PatternRule + Send + Sync + 'static,
    {
        Self {
            inner: Box::new(callback),
            name: name.to_string(),
            description: String::new(),
            default_op: CallbackOp::With,
        }
    }

    /// Create from a function pointer (for const-like usage)
    pub fn from_fn(name: &str, callback: fn(&PatternRule, bool) -> PatternRule) -> Self {
        Self {
            inner: Box::new(callback),
            name: name.to_string(),
            description: String::new(),
            default_op: CallbackOp::With,
        }
    }

    /// Add a description
    pub fn with_description(mut self, desc: &str) -> Self {
        self.description = desc.to_string();
        self
    }

    /// Set the default operation
    pub fn with_default_op(mut self, op: CallbackOp) -> Self {
        self.default_op = op;
        self
    }

    /// Apply the callback with flag = true (positive application)
    pub fn with(&self, rule: &PatternRule) -> PatternRule {
        (self.inner)(rule, true)
    }

    /// Apply the callback with flag = false (negative/inverted application)
    pub fn without(&self, rule: &PatternRule) -> PatternRule {
        (self.inner)(rule, false)
    }

    /// Apply with explicit operation
    pub fn apply(&self, rule: &PatternRule, op: CallbackOp) -> PatternRule {
        match op {
            CallbackOp::With => self.with(rule),
            CallbackOp::Without => self.without(rule),
            CallbackOp::Check => rule.clone(), // No modification, just return
            CallbackOp::ApplyIf => self.with(rule),
            CallbackOp::ApplyUnless => self.without(rule),
        }
    }

    /// Apply based on a condition
    pub fn apply_if(&self, rule: &PatternRule, condition: bool) -> PatternRule {
        if condition {
            self.with(rule)
        } else {
            rule.clone()
        }
    }

    /// Apply inverted based on a condition
    pub fn apply_unless(&self, rule: &PatternRule, condition: bool) -> PatternRule {
        if !condition {
            self.with(rule)
        } else {
            rule.clone()
        }
    }

    /// Chain multiple callbacks together
    pub fn chain<'a>(&'a self, other: &'a PatternRuleCallback) -> PatternRuleCallbackChain<'a> {
        PatternRuleCallbackChain {
            callbacks: vec![self, other],
        }
    }

    /// Get the callback name
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Get the description
    pub fn description(&self) -> &str {
        &self.description
    }

    /// Get the default operation
    pub fn default_op(&self) -> CallbackOp {
        self.default_op
    }

    /// Apply with default operation
    pub fn apply_default(&self, rule: &PatternRule) -> PatternRule {
        self.apply(rule, self.default_op)
    }
}

/// Extraction callback that can modify context during pattern matching.
///
/// This is the key type for inline extraction logic. The callback receives:
/// - `rule`: The current PatternRule being matched
/// - `ctx`: Mutable context to store extracted data
///
/// Returns the (possibly modified) rule.
///
/// # Example
/// ```ignore
/// // In handler patterns() method:
/// PatternRule::type_keyword()
///     .with_extract(|rule, ctx| {
///         if ctx.did_match {
///             ctx.set_value("type", &ctx.matched.clone().unwrap_or_default());
///         }
///         rule.clone()
///     })
///
/// // With modifiers:
/// PatternRule::one_of("const|static|extern")
///     .with_extract(|rule, ctx| {
///         if let Some(m) = &ctx.matched {
///             ctx.add_modifier(m);
///             ctx.set_flag("is_const", m == "const");
///         }
///         rule.clone()
///     })
///
/// // Nested pattern:
/// PatternRule::exact("{")
///     .with_extract(|rule, ctx| {
///         // Parse body between braces as nested pattern
///         let body_tokens = extract_between(&ctx.tokens, ctx.token_idx, "{", "}");
///         let mut body_ctx = ExtractContext::new(&body_tokens);
///         // ... match nested pattern against body_ctx ...
///         ctx.set_nested("body", body_ctx);
///         rule.clone()
///     })
/// ```
pub struct ExtractCallback {
    inner: Box<dyn Fn(&PatternRule, &mut ExtractContext) -> PatternRule + Send + Sync>,
    name: String,
}

impl ExtractCallback {
    /// Create a new extraction callback
    pub fn new<F>(name: &str, callback: F) -> Self
    where
        F: Fn(&PatternRule, &mut ExtractContext) -> PatternRule + Send + Sync + 'static,
    {
        Self {
            inner: Box::new(callback),
            name: name.to_string(),
        }
    }

    /// Apply the callback
    pub fn apply(&self, rule: &PatternRule, ctx: &mut ExtractContext) -> PatternRule {
        (self.inner)(rule, ctx)
    }

    /// Get callback name
    pub fn name(&self) -> &str {
        &self.name
    }
}

impl Debug for ExtractCallback {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ExtractCallback")
            .field("name", &self.name)
            .finish()
    }
}

impl Clone for ExtractCallback {
    fn clone(&self) -> Self {
        // Can't clone the boxed fn, create a no-op placeholder
        Self {
            inner: Box::new(|rule, _| rule.clone()),
            name: self.name.clone(),
        }
    }
}

/// Chain of callbacks to apply in sequence
pub struct PatternRuleCallbackChain<'a> {
    callbacks: Vec<&'a PatternRuleCallback>,
}

impl<'a> PatternRuleCallbackChain<'a> {
    /// Add another callback to the chain
    pub fn then(mut self, callback: &'a PatternRuleCallback) -> Self {
        self.callbacks.push(callback);
        self
    }

    /// Apply all callbacks in sequence with flag = true
    pub fn with(&self, rule: &PatternRule) -> PatternRule {
        let mut result = rule.clone();
        for cb in &self.callbacks {
            result = cb.with(&result);
        }
        result
    }

    /// Apply all callbacks in sequence with flag = false
    pub fn without(&self, rule: &PatternRule) -> PatternRule {
        let mut result = rule.clone();
        for cb in &self.callbacks {
            result = cb.without(&result);
        }
        result
    }
}

impl Debug for PatternRuleCallback {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("PatternRuleCallback")
            .field("name", &self.name)
            .field("description", &self.description)
            .field("default_op", &self.default_op)
            .finish()
    }
}

impl Clone for PatternRuleCallback {
    fn clone(&self) -> Self {
        // We can't clone the boxed Fn directly, so we create a default wrapper
        // For full functionality, callbacks should be created fresh or wrapped in Arc
        Self {
            inner: Box::new(|rule: &PatternRule, flag: bool| {
                // Default behavior for cloned callbacks - modify optional flag
                let mut new_rule = rule.clone();
                if flag {
                    new_rule.optional = true;
                }
                new_rule
            }),
            name: self.name.clone(),
            description: self.description.clone(),
            default_op: self.default_op,
        }
    }
}

impl PartialEq for PatternRuleCallback {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.default_op == other.default_op
    }
}

impl Eq for PatternRuleCallback {}

impl Build for PatternRuleCallback {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("PatternRuleCallback", &self.name);
        entry.set_attr("description", Entry::string(&self.description));
        entry.set_attr("default_op", Entry::string(self.default_op.name()));
        entry
    }

    fn kind(&self) -> &str {
        "PatternRuleCallback"
    }

    fn name(&self) -> Option<&str> {
        Some(&self.name)
    }

    fn category(&self) -> Option<&str> {
        Some("callback")
    }
}

// ============================================================================
// Pattern Rule
// ============================================================================

/// Constant for infinite repeats
pub const REPEAT_INFINITE: i32 = -1;
/// Constant for no repeats (false)
pub const REPEAT_NONE: i32 = 0;

/// A single pattern matching rule
#[derive(Debug, Clone)]
pub struct PatternRule {
    pub rule_type: RuleType,
    pub value: String,
    pub optional: bool,
    /// Number of times this rule can repeat:
    /// - 0 = no repeat (false)
    /// - 1+ = specific number of repeats allowed
    /// - -1 = infinite repeats allowed
    pub repeatable: i32,
    pub forbid_next: Vec<String>,
    pub require_next: Vec<String>,
    /// Tokens that must NOT appear anywhere before this rule
    pub forbid_previous: Vec<String>,
    /// Tokens that MUST appear anywhere before this rule
    pub require_previous: Vec<String>,
    /// Token that must be IMMEDIATELY before this rule (None = no constraint)
    pub directly_before: Option<String>,
    /// Token that must NOT be immediately before this rule
    pub not_directly_before: Option<String>,
    /// Token that must be IMMEDIATELY after this rule (None = no constraint)
    pub directly_after: Option<String>,
    /// Token that must NOT be immediately after this rule
    pub not_directly_after: Option<String>,
    /// Named callbacks for rule transformations: Vec<(name, callback)>
    pub callbacks: Vec<(String, PatternRuleCallback)>,
    /// Extraction callback - called during matching to populate context
    pub extract_cb: Option<ExtractCallback>,
    /// Nested pattern to match (for recursive structures)
    pub nested_pattern: Option<Box<Pattern<String>>>,
}

impl PatternRule {
    /// Create exact match rule
    pub fn exact(value: &str) -> Self {
        Self {
            rule_type: RuleType::Exact,
            value: value.to_string(),
            optional: false,
            repeatable: REPEAT_NONE,
            forbid_next: Vec::new(),
            require_next: Vec::new(),
            forbid_previous: Vec::new(),
            require_previous: Vec::new(),
            directly_before: None,
            not_directly_before: None,
            directly_after: None,
            not_directly_after: None,
            callbacks: Vec::new(),
            extract_cb: None,
            nested_pattern: None,
        }
    }

    /// Create type keyword rule
    pub fn type_keyword() -> Self {
        Self {
            rule_type: RuleType::TypeKeyword,
            value: String::new(),
            optional: false,
            repeatable: REPEAT_NONE,
            forbid_next: Vec::new(),
            require_next: Vec::new(),
            forbid_previous: Vec::new(),
            require_previous: Vec::new(),
            directly_before: None,
            not_directly_before: None,
            directly_after: None,
            not_directly_after: None,
            callbacks: Vec::new(),
            extract_cb: None,
            nested_pattern: None,
        }
    }

    /// Create keyword rule
    pub fn keyword() -> Self {
        Self {
            rule_type: RuleType::Keyword,
            value: String::new(),
            optional: false,
            repeatable: REPEAT_NONE,
            forbid_next: Vec::new(),
            require_next: Vec::new(),
            forbid_previous: Vec::new(),
            require_previous: Vec::new(),
            directly_before: None,
            not_directly_before: None,
            directly_after: None,
            not_directly_after: None,
            callbacks: Vec::new(),
            extract_cb: None,
            nested_pattern: None,
        }
    }

    /// Create identifier rule
    pub fn identifier() -> Self {
        Self {
            rule_type: RuleType::Identifier,
            value: String::new(),
            optional: false,
            repeatable: REPEAT_NONE,
            forbid_next: Vec::new(),
            require_next: Vec::new(),
            forbid_previous: Vec::new(),
            require_previous: Vec::new(),
            directly_before: None,
            not_directly_before: None,
            directly_after: None,
            not_directly_after: None,
            callbacks: Vec::new(),
            extract_cb: None,
            nested_pattern: None,
        }
    }

    /// Create one-of rule
    pub fn one_of(options: Vec<String>) -> Self {
        Self {
            rule_type: RuleType::OneOf,
            value: options.join("|"),
            optional: false,
            repeatable: REPEAT_NONE,
            forbid_next: Vec::new(),
            require_next: Vec::new(),
            forbid_previous: Vec::new(),
            require_previous: Vec::new(),
            directly_before: None,
            not_directly_before: None,
            directly_after: None,
            not_directly_after: None,
            callbacks: Vec::new(),
            extract_cb: None,
            nested_pattern: None,
        }
    }

    /// Create any token rule
    pub fn any() -> Self {
        Self {
            rule_type: RuleType::Any,
            value: String::new(),
            optional: false,
            repeatable: REPEAT_NONE,
            forbid_next: Vec::new(),
            require_next: Vec::new(),
            forbid_previous: Vec::new(),
            require_previous: Vec::new(),
            directly_before: None,
            not_directly_before: None,
            directly_after: None,
            not_directly_after: None,
            callbacks: Vec::new(),
            extract_cb: None,
            nested_pattern: None,
        }
    }

    /// Create any-except rule that matches any token except those specified
    /// Pass exclusions as pipe-separated string, e.g., "}|;|{"
    pub fn any_except(exclusions: impl Into<String>) -> Self {
        Self {
            rule_type: RuleType::AnyExcept,
            value: exclusions.into(),
            optional: false,
            repeatable: REPEAT_NONE,
            forbid_next: Vec::new(),
            require_next: Vec::new(),
            forbid_previous: Vec::new(),
            require_previous: Vec::new(),
            directly_before: None,
            not_directly_before: None,
            directly_after: None,
            not_directly_after: None,
            callbacks: Vec::new(),
            extract_cb: None,
            nested_pattern: None,
        }
    }

    /// Create custom validator rule
    pub fn custom(name: impl Into<String>) -> Self {
        Self {
            rule_type: RuleType::Custom(name.into()),
            value: String::new(),
            optional: false,
            repeatable: REPEAT_NONE,
            forbid_next: Vec::new(),
            require_next: Vec::new(),
            forbid_previous: Vec::new(),
            require_previous: Vec::new(),
            directly_before: None,
            not_directly_before: None,
            directly_after: None,
            not_directly_after: None,
            callbacks: Vec::new(),
            extract_cb: None,
            nested_pattern: None,
        }
    }

    /// Make rule optional
    pub fn optional(mut self) -> Self {
        self.optional = true;
        self
    }

    /// Get/check repeat status:
    /// - Returns `None` if rule cannot repeat (false)
    /// - Returns `Some(count)` if rule can repeat (true), where count is:
    ///   - `-1` for infinite repeats
    ///   - `1+` for specific number of repeats
    pub fn repeatable(&self) -> Option<i32> {
        if self.repeatable == REPEAT_NONE {
            None
        } else {
            Some(self.repeatable)
        }
    }

    /// Set repeat count (builder pattern)
    /// - `0` = no repeat
    /// - `1+` = specific number of repeats
    /// - `-1` = infinite repeats
    pub fn repeat(mut self, count: i32) -> Self {
        self.repeatable = count;
        self
    }

    /// Convenience: check if rule can repeat (any non-zero value)
    pub fn can_repeat(&self) -> bool {
        self.repeatable().is_some()
    }

    /// Convenience: check if rule can repeat infinitely
    pub fn is_infinite(&self) -> bool {
        self.repeatable() == Some(REPEAT_INFINITE)
    }

    /// Convenience: get max repeats (None for infinite, Some(0) for no repeat)
    pub fn max_repeats(&self) -> Option<i32> {
        match self.repeatable() {
            None => Some(0),
            Some(REPEAT_INFINITE) => None,
            Some(n) => Some(n),
        }
    }

    /// Add forbidden next tokens
    pub fn with_forbid_next(mut self, forbidden: Vec<String>) -> Self {
        self.forbid_next = forbidden;
        self
    }

    /// Add required next tokens
    pub fn with_require_next(mut self, required: Vec<String>) -> Self {
        self.require_next = required;
        self
    }

    /// Add tokens that cannot precede this rule (validation constraint)
    /// If any of these tokens appear before this rule, the pattern fails to match
    pub fn with_forbid_previous(mut self, tokens: Vec<String>) -> Self {
        self.forbid_previous = tokens;
        self
    }

    /// Add tokens that must precede this rule (validation constraint)
    /// At least one of these tokens must appear before this rule for pattern to match
    pub fn with_require_previous(mut self, tokens: Vec<String>) -> Self {
        self.require_previous = tokens;
        self
    }

    /// Add a named callback function
    pub fn with_callback(mut self, name: &str, callback: PatternRuleCallback) -> Self {
        // Remove existing callback with same name if present
        self.callbacks.retain(|(n, _)| n != name);
        self.callbacks.push((name.to_string(), callback));
        self
    }

    /// Add a callback using its internal name
    pub fn add_callback(mut self, callback: PatternRuleCallback) -> Self {
        let name = callback.name().to_string();
        self.callbacks.retain(|(n, _)| n != &name);
        self.callbacks.push((name, callback));
        self
    }

    /// Check if this rule has any callbacks
    pub fn has_callbacks(&self) -> bool {
        !self.callbacks.is_empty()
    }

    /// Check if this rule has a specific named callback
    pub fn has_callback(&self, name: &str) -> bool {
        self.callbacks.iter().any(|(n, _)| n == name)
    }

    /// Get a callback by name
    pub fn get_callback(&self, name: &str) -> Option<&PatternRuleCallback> {
        self.callbacks
            .iter()
            .find(|(n, _)| n == name)
            .map(|(_, cb)| cb)
    }

    /// Get all callback names
    pub fn callback_names(&self) -> Vec<&str> {
        self.callbacks.iter().map(|(n, _)| n.as_str()).collect()
    }

    /// Get all callbacks
    pub fn get_callbacks(&self) -> &[(String, PatternRuleCallback)] {
        &self.callbacks
    }

    /// Remove a callback by name
    pub fn remove_callback(mut self, name: &str) -> Self {
        self.callbacks.retain(|(n, _)| n != name);
        self
    }

    /// Add an inline extraction callback
    ///
    /// The callback is called during pattern matching and can modify
    /// the ExtractContext to store extracted data.
    ///
    /// # Example
    /// ```ignore
    /// PatternRule::type_keyword()
    ///     .with_extract(|rule, ctx| {
    ///         if ctx.did_match {
    ///             ctx.set_value("type", &ctx.matched.clone().unwrap_or_default());
    ///         }
    ///         rule.clone()
    ///     })
    /// ```
    pub fn with_extract<F>(mut self, callback: F) -> Self
    where
        F: Fn(&PatternRule, &mut ExtractContext) -> PatternRule + Send + Sync + 'static,
    {
        self.extract_cb = Some(ExtractCallback::new("extract", callback));
        self
    }

    /// Add a named extraction callback
    pub fn with_named_extract<F>(mut self, name: &str, callback: F) -> Self
    where
        F: Fn(&PatternRule, &mut ExtractContext) -> PatternRule + Send + Sync + 'static,
    {
        self.extract_cb = Some(ExtractCallback::new(name, callback));
        self
    }

    /// Add a nested pattern to match within this rule
    ///
    /// Useful for recursive structures like nested braces or parentheses.
    pub fn with_nested(mut self, pattern: Pattern<String>) -> Self {
        self.nested_pattern = Some(Box::new(pattern));
        self
    }

    /// Check if this rule has an extraction callback
    pub fn has_extract(&self) -> bool {
        self.extract_cb.is_some()
    }

    /// Check if this rule has a nested pattern
    pub fn has_nested(&self) -> bool {
        self.nested_pattern.is_some()
    }

    /// Apply the extraction callback if present
    pub fn apply_extract(&self, ctx: &mut ExtractContext) -> PatternRule {
        if let Some(cb) = &self.extract_cb {
            cb.apply(self, ctx)
        } else {
            self.clone()
        }
    }

    /// Apply a specific named callback with flag = true
    pub fn apply_callback(&self, name: &str) -> Self {
        match self.get_callback(name) {
            Some(cb) => cb.with(self),
            None => self.clone(),
        }
    }

    /// Apply a specific named callback with flag = false
    pub fn apply_callback_inverted(&self, name: &str) -> Self {
        match self.get_callback(name) {
            Some(cb) => cb.without(self),
            None => self.clone(),
        }
    }

    /// Apply a named callback based on a condition
    pub fn apply_callback_if(&self, name: &str, condition: bool) -> Self {
        match self.get_callback(name) {
            Some(cb) => cb.apply_if(self, condition),
            None => self.clone(),
        }
    }

    /// Apply a specific callback operation by name
    pub fn apply_callback_op(&self, name: &str, op: CallbackOp) -> Self {
        match self.get_callback(name) {
            Some(cb) => cb.apply(self, op),
            None => self.clone(),
        }
    }

    /// Apply all callbacks in sequence with flag = true
    pub fn apply_all_callbacks(&self) -> Self {
        let mut result = self.clone();
        for (_, cb) in &self.callbacks {
            result = cb.with(&result);
        }
        result
    }

    /// Apply all callbacks in sequence with flag = false
    pub fn apply_all_callbacks_inverted(&self) -> Self {
        let mut result = self.clone();
        for (_, cb) in &self.callbacks {
            result = cb.without(&result);
        }
        result
    }

    /// Apply callbacks by name list with flag = true
    pub fn apply_callbacks(&self, names: &[&str]) -> Self {
        let mut result = self.clone();
        for name in names {
            if let Some(cb) = self.get_callback(name) {
                result = cb.with(&result);
            }
        }
        result
    }

    /// Set the optional flag directly (for callback use)
    pub fn with_optional(mut self, optional: bool) -> Self {
        self.optional = optional;
        self
    }

    /// Alias for repeat() - set the can_repeat value directly
    /// - 0 = no repeat, 1+ = specific count, -1 = infinite
    pub fn with_repeat(mut self, count: i32) -> Self {
        self.repeatable = count;
        self
    }

    /// Add a single forbidden next token
    pub fn forbid_next(mut self, token: &str) -> Self {
        if !self.forbid_next.contains(&token.to_string()) {
            self.forbid_next.push(token.to_string());
        }
        self
    }

    /// Add a single required next token
    pub fn require_next(mut self, token: &str) -> Self {
        if !self.require_next.contains(&token.to_string()) {
            self.require_next.push(token.to_string());
        }
        self
    }
    pub fn forbid_previous(mut self, token: &str) -> Self {
        if !self.forbid_previous.contains(&token.to_string()) {
            self.forbid_previous.push(token.to_string());
        }
        self
    }

    /// Add a single required previous token
    pub fn require_previous(mut self, token: &str) -> Self {
        if !self.require_previous.contains(&token.to_string()) {
            self.require_previous.push(token.to_string());
        }
        self
    }

    /// Forbid a token from appearing ANYWHERE before the current token.
    /// If the forbidden token exists at any position before this rule, matching fails.
    pub fn forbid_before(self, token: &str) -> Self {
        self.forbid_previous(token)
    }

    /// Require a token to appear ANYWHERE before the current token.
    /// If the required token doesn't exist at some position before this rule, matching fails.
    pub fn require_before(self, token: &str) -> Self {
        self.require_previous(token)
    }

    /// Require the IMMEDIATELY previous token to be this exact token.
    pub fn directly_before(mut self, token: &str) -> Self {
        self.directly_before = Some(token.to_string());
        self
    }

    /// Forbid the IMMEDIATELY previous token from being this exact token.
    pub fn not_directly_before(mut self, token: &str) -> Self {
        self.not_directly_before = Some(token.to_string());
        self
    }

    /// Require the IMMEDIATELY next token to be this exact token.
    pub fn directly_after(mut self, token: &str) -> Self {
        self.directly_after = Some(token.to_string());
        self
    }

    /// Forbid the IMMEDIATELY next token from being this exact token.
    pub fn not_directly_after(mut self, token: &str) -> Self {
        self.not_directly_after = Some(token.to_string());
        self
    }

    /// Create a modified copy using an external callback (not stored)
    pub fn modify_by(self, callback: &PatternRuleCallback) -> Self {
        callback.with(&self)
    }

    /// Create a modified copy using inverted external callback (not stored)
    pub fn modify_without(self, callback: &PatternRuleCallback) -> Self {
        callback.without(&self)
    }

    // ========================================================================
    // Callback Factory Methods
    // ========================================================================

    /// Create a callback that makes rules optional
    pub fn optional_callback() -> PatternRuleCallback {
        PatternRuleCallback::new("optional", |rule, flag| {
            let mut r = rule.clone();
            r.optional = flag;
            r
        })
        .with_description("Makes the rule optional (can match zero times)")
    }

    /// Create a callback that makes rules repeatable (infinite)
    pub fn repeatable_callback() -> PatternRuleCallback {
        PatternRuleCallback::new("repeatable", |rule, flag| {
            let mut r = rule.clone();
            r.repeatable = if flag { REPEAT_INFINITE } else { REPEAT_NONE };
            r
        })
        .with_description("Makes the rule repeatable (infinite repeats when true)")
    }

    /// Create a callback that sets a specific repeat count
    pub fn repeat_count_callback(count: i32) -> PatternRuleCallback {
        PatternRuleCallback::new("repeat_count", move |rule, flag| {
            let mut r = rule.clone();
            r.repeatable = if flag { count } else { REPEAT_NONE };
            r
        })
        .with_description("Sets specific repeat count when true, disables when false")
    }

    /// Create a callback that adds/removes a forbidden next token
    pub fn forbid_next_callback(token: &str) -> PatternRuleCallback {
        let token = token.to_string();
        PatternRuleCallback::new("forbid_next", move |rule, flag| {
            let mut r = rule.clone();
            if flag {
                if !r.forbid_next.contains(&token) {
                    r.forbid_next.push(token.clone());
                }
            } else {
                r.forbid_next.retain(|t| t != &token);
            }
            r
        })
        .with_description("Adds/removes a forbidden next token")
    }

    /// Create a callback that adds/removes a required next token
    pub fn require_next_callback(token: &str) -> PatternRuleCallback {
        let token = token.to_string();
        PatternRuleCallback::new("require_next", move |rule, flag| {
            let mut r = rule.clone();
            if flag {
                if !r.require_next.contains(&token) {
                    r.require_next.push(token.clone());
                }
            } else {
                r.require_next.retain(|t| t != &token);
            }
            r
        })
        .with_description("Adds/removes a required next token")
    }

    /// Create a callback that sets the rule type
    pub fn set_type_callback(rule_type: RuleType) -> PatternRuleCallback {
        PatternRuleCallback::new("set_type", move |rule, flag| {
            let mut r = rule.clone();
            if flag {
                r.rule_type = rule_type.clone();
            }
            r
        })
        .with_description("Sets the rule type")
    }

    /// Create a callback that sets the rule value
    pub fn set_value_callback(value: &str) -> PatternRuleCallback {
        let value = value.to_string();
        PatternRuleCallback::new("set_value", move |rule, flag| {
            let mut r = rule.clone();
            if flag {
                r.value = value.clone();
            }
            r
        })
        .with_description("Sets the rule value")
    }
}

// ============================================================================
// Pattern Capture (for extraction)
// ============================================================================

/// A captured segment from pattern matching
#[derive(Debug, Clone, Default)]
pub struct Capture {
    /// The rule index that captured this
    pub rule_idx: usize,
    /// Start token index (inclusive)
    pub start: usize,
    /// End token index (exclusive)
    pub end: usize,
    /// The captured tokens
    pub tokens: Vec<String>,
    /// Rule type that matched
    pub rule_type: Option<RuleType>,
    /// Whether this was from a repeating rule
    pub repeated: bool,
}

impl Capture {
    /// Get single token (if exactly one captured)
    pub fn single(&self) -> Option<&str> {
        if self.tokens.len() == 1 {
            Some(&self.tokens[0])
        } else {
            None
        }
    }

    /// Get first token
    pub fn first(&self) -> Option<&str> {
        self.tokens.first().map(|s| s.as_str())
    }

    /// Get all tokens joined
    pub fn joined(&self, sep: &str) -> String {
        self.tokens.join(sep)
    }

    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.tokens.is_empty()
    }

    /// Get token count
    pub fn len(&self) -> usize {
        self.tokens.len()
    }
}

impl Build for Capture {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("Capture", &format!("rule_{}", self.rule_idx));
        entry.set_attr("start", Entry::usize(self.start));
        entry.set_attr("end", Entry::usize(self.end));
        entry.set_attr("tokens", Entry::vec(self.tokens.iter().map(|s| Entry::string(s)).collect()));
        entry.set_attr("repeated", Entry::bool(self.repeated));
        if let Some(ref rt) = self.rule_type {
            entry.set_attr("rule_type", Entry::string(&format!("{:?}", rt)));
        }
        entry
    }

    fn kind(&self) -> &str { "Capture" }
    fn name(&self) -> Option<&str> { None }
    fn category(&self) -> Option<&str> { Some("pattern") }
}

/// Extracted data from a pattern match (for handler use)
#[derive(Debug, Clone, Default)]
pub struct ExtractedMatch {
    /// Pattern name that matched
    pub pattern_name: String,
    /// Confidence score (0.0 - 1.0)
    pub confidence: f64,
    /// Total tokens consumed
    pub tokens_consumed: usize,
    /// Captures indexed by rule index
    pub captures: Vec<Capture>,
    /// Named captures (rule value -> capture)
    pub named: HashMap<String, Capture>,
    /// Tokens between delimiters (key = "open:close", e.g., "{:}")
    pub delimited: HashMap<String, Vec<String>>,
    /// Modifiers found (const, static, extern, etc.)
    pub modifiers: HashSet<String>,
    /// The full matched token sequence
    pub matched_tokens: Vec<String>,
}

impl ExtractedMatch {
    /// Create new empty match
    pub fn new(pattern_name: &str) -> Self {
        Self {
            pattern_name: pattern_name.to_string(),
            ..Default::default()
        }
    }

    /// Get capture by rule index
    pub fn get(&self, rule_idx: usize) -> Option<&Capture> {
        self.captures.get(rule_idx)
    }

    /// Get capture by name (exact match on rule value)
    pub fn get_named(&self, name: &str) -> Option<&Capture> {
        self.named.get(name)
    }

    /// Get single token by rule index
    pub fn token(&self, rule_idx: usize) -> Option<&str> {
        self.captures.get(rule_idx).and_then(|c| c.single())
    }

    /// Get first identifier captured
    pub fn identifier(&self) -> Option<&str> {
        self.captures
            .iter()
            .find(|c| matches!(c.rule_type, Some(RuleType::Identifier)))
            .and_then(|c| c.first())
    }

    /// Get first type keyword captured
    pub fn type_keyword(&self) -> Option<&str> {
        self.captures
            .iter()
            .find(|c| matches!(c.rule_type, Some(RuleType::TypeKeyword)))
            .and_then(|c| c.first())
    }

    /// Get all type keywords captured (handles repeated type rules)
    pub fn type_keywords(&self) -> Vec<&str> {
        self.captures
            .iter()
            .filter(|c| matches!(c.rule_type, Some(RuleType::TypeKeyword)))
            .flat_map(|c| c.tokens.iter().map(|s| s.as_str()))
            .collect()
    }

    /// Get tokens between delimiters (e.g., between "{" and "}")
    pub fn between(&self, open: &str, close: &str) -> Option<&Vec<String>> {
        let key = format!("{}:{}", open, close);
        self.delimited.get(&key)
    }

    /// Check if a modifier is present
    pub fn has_modifier(&self, modifier: &str) -> bool {
        self.modifiers.contains(modifier)
    }

    /// Get all captures of a specific rule type
    pub fn by_type(&self, rule_type: &RuleType) -> Vec<&Capture> {
        self.captures
            .iter()
            .filter(|c| c.rule_type.as_ref() == Some(rule_type))
            .collect()
    }

    /// Get tokens after a specific exact match
    pub fn after(&self, exact: &str) -> Option<Vec<&str>> {
        let mut found = false;
        let mut result = Vec::new();
        for cap in &self.captures {
            if found {
                result.extend(cap.tokens.iter().map(|s| s.as_str()));
            } else if cap.tokens.iter().any(|t| t == exact) {
                found = true;
            }
        }
        if found && !result.is_empty() {
            Some(result)
        } else {
            None
        }
    }

    /// Get tokens before a specific exact match
    pub fn before(&self, exact: &str) -> Option<Vec<&str>> {
        let mut result = Vec::new();
        for cap in &self.captures {
            if cap.tokens.iter().any(|t| t == exact) {
                break;
            }
            result.extend(cap.tokens.iter().map(|s| s.as_str()));
        }
        if !result.is_empty() {
            Some(result)
        } else {
            None
        }
    }

    /// Find position of an exact token match
    pub fn position(&self, exact: &str) -> Option<usize> {
        self.matched_tokens.iter().position(|t| t == exact)
    }

    /// Get slice of matched tokens
    pub fn slice(&self, start: usize, end: usize) -> &[String] {
        let end = end.min(self.matched_tokens.len());
        let start = start.min(end);
        &self.matched_tokens[start..end]
    }
}

impl Build for ExtractedMatch {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("ExtractedMatch", &self.pattern_name);
        entry.set_attr("confidence", Entry::f64(self.confidence));
        entry.set_attr("tokens_consumed", Entry::usize(self.tokens_consumed));
        entry.set_attr("captures", Entry::vec(self.captures.iter().map(|c| c.to_entry()).collect()));
        entry.set_attr("matched_tokens", Entry::vec(self.matched_tokens.iter().map(|s| Entry::string(s)).collect()));
        entry.set_attr("modifiers", Entry::vec(self.modifiers.iter().map(|s| Entry::string(s)).collect()));
        entry
    }

    fn kind(&self) -> &str { "ExtractedMatch" }
    fn name(&self) -> Option<&str> { Some(&self.pattern_name) }
    fn category(&self) -> Option<&str> { Some("pattern") }
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

impl Build for PatternStats {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("PatternStats", "stats");
        entry.set_attr("confidence", Entry::f64(self.confidence));
        entry.set_attr("match_count", Entry::usize(self.match_count as usize));
        entry.set_attr("validation_success", Entry::usize(self.validation_success as usize));
        entry.set_attr("validation_failures", Entry::usize(self.validation_failures as usize));
        entry.set_attr("success_rate", Entry::f64(self.success_rate));
        entry.set_attr("avg_tokens_consumed", Entry::f64(self.avg_tokens_consumed));
        entry.set_attr("min_tokens_consumed", Entry::usize(self.min_tokens_consumed));
        entry.set_attr("max_tokens_consumed", Entry::usize(self.max_tokens_consumed));
        if let Some(ref quality) = self.match_quality {
            entry.set_attr("match_quality", Entry::string(quality));
        }
        entry
    }

    fn kind(&self) -> &str { "PatternStats" }
    fn name(&self) -> Option<&str> { None }
    fn category(&self) -> Option<&str> { Some("pattern") }
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
            Pattern::Definition { id, .. }
            | Pattern::Literal { id, .. }
            | Pattern::Predicate { id, .. }
            | Pattern::Wildcard { id, .. }
            | Pattern::Sequence { id, .. }
            | Pattern::Choice { id, .. }
            | Pattern::Optional { id, .. }
            | Pattern::Repeat { id, .. }
            | Pattern::Branch { id, .. } => *id,
        }
    }

    pub fn name(&self) -> &str {
        match self {
            Pattern::Definition { name, .. }
            | Pattern::Literal { name, .. }
            | Pattern::Predicate { name, .. }
            | Pattern::Wildcard { name, .. }
            | Pattern::Sequence { name, .. }
            | Pattern::Choice { name, .. }
            | Pattern::Optional { name, .. }
            | Pattern::Repeat { name, .. }
            | Pattern::Branch { name, .. } => name,
        }
    }

    pub fn category(&self) -> &str {
        match self {
            Pattern::Definition { category, .. }
            | Pattern::Literal { category, .. }
            | Pattern::Predicate { category, .. }
            | Pattern::Wildcard { category, .. }
            | Pattern::Sequence { category, .. }
            | Pattern::Choice { category, .. }
            | Pattern::Optional { category, .. }
            | Pattern::Repeat { category, .. }
            | Pattern::Branch { category, .. } => category,
        }
    }

    pub fn priority(&self) -> i32 {
        match self {
            Pattern::Definition { priority, .. }
            | Pattern::Literal { priority, .. }
            | Pattern::Predicate { priority, .. }
            | Pattern::Wildcard { priority, .. }
            | Pattern::Sequence { priority, .. }
            | Pattern::Choice { priority, .. }
            | Pattern::Optional { priority, .. }
            | Pattern::Repeat { priority, .. }
            | Pattern::Branch { priority, .. } => *priority,
        }
    }

    pub fn min_tokens(&self) -> usize {
        match self {
            Pattern::Definition { min_tokens, .. }
            | Pattern::Literal { min_tokens, .. }
            | Pattern::Predicate { min_tokens, .. }
            | Pattern::Wildcard { min_tokens, .. }
            | Pattern::Sequence { min_tokens, .. }
            | Pattern::Choice { min_tokens, .. }
            | Pattern::Optional { min_tokens, .. }
            | Pattern::Repeat { min_tokens, .. }
            | Pattern::Branch { min_tokens, .. } => *min_tokens,
        }
    }

    pub fn description(&self) -> &str {
        match self {
            Pattern::Definition { description, .. }
            | Pattern::Literal { description, .. }
            | Pattern::Predicate { description, .. }
            | Pattern::Wildcard { description, .. }
            | Pattern::Sequence { description, .. }
            | Pattern::Choice { description, .. }
            | Pattern::Optional { description, .. }
            | Pattern::Repeat { description, .. }
            | Pattern::Branch { description, .. } => description,
        }
    }

    /// Check if this is a definition pattern
    pub fn is_definition(&self) -> bool {
        matches!(self, Pattern::Definition { .. })
    }
    /// Check if this is a literal pattern
    pub fn is_literal(&self) -> bool {
        matches!(self, Pattern::Literal { .. })
    }
    /// Check if this is a predicate pattern
    pub fn is_predicate(&self) -> bool {
        matches!(self, Pattern::Predicate { .. })
    }
    /// Check if this is a wildcard pattern
    pub fn is_wildcard(&self) -> bool {
        matches!(self, Pattern::Wildcard { .. })
    }

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
            Pattern::Definition { category, .. }
            | Pattern::Literal { category, .. }
            | Pattern::Predicate { category, .. }
            | Pattern::Wildcard { category, .. }
            | Pattern::Sequence { category, .. }
            | Pattern::Choice { category, .. }
            | Pattern::Optional { category, .. }
            | Pattern::Repeat { category, .. }
            | Pattern::Branch { category, .. } => {
                *category = cat.into();
            }
        }
        self
    }

    /// Builder: set priority
    pub fn with_priority(mut self, pri: i32) -> Self {
        match &mut self {
            Pattern::Definition { priority, .. }
            | Pattern::Literal { priority, .. }
            | Pattern::Predicate { priority, .. }
            | Pattern::Wildcard { priority, .. }
            | Pattern::Sequence { priority, .. }
            | Pattern::Choice { priority, .. }
            | Pattern::Optional { priority, .. }
            | Pattern::Repeat { priority, .. }
            | Pattern::Branch { priority, .. } => {
                *priority = pri;
            }
        }
        self
    }

    /// Builder: set minimum tokens
    pub fn with_min_tokens(mut self, min: usize) -> Self {
        match &mut self {
            Pattern::Definition { min_tokens, .. }
            | Pattern::Literal { min_tokens, .. }
            | Pattern::Predicate { min_tokens, .. }
            | Pattern::Wildcard { min_tokens, .. }
            | Pattern::Sequence { min_tokens, .. }
            | Pattern::Choice { min_tokens, .. }
            | Pattern::Optional { min_tokens, .. }
            | Pattern::Repeat { min_tokens, .. }
            | Pattern::Branch { min_tokens, .. } => {
                *min_tokens = min;
            }
        }
        self
    }

    /// Builder: set description
    pub fn with_description(mut self, desc: impl Into<String>) -> Self {
        match &mut self {
            Pattern::Definition { description, .. }
            | Pattern::Literal { description, .. }
            | Pattern::Predicate { description, .. }
            | Pattern::Wildcard { description, .. }
            | Pattern::Sequence { description, .. }
            | Pattern::Choice { description, .. }
            | Pattern::Optional { description, .. }
            | Pattern::Repeat { description, .. }
            | Pattern::Branch { description, .. } => {
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
    where
        T: Default,
    {
        if entry.kind() != Some("Pattern") {
            return None;
        }

        let name = entry.name()?.to_string();
        let id = entry.get_number_attr("id").map(|n| n as usize).unwrap_or(0);
        let category = entry.get_string_attr("category").unwrap_or("").to_string();
        let priority = entry
            .get_number_attr("priority")
            .map(|n| n as i32)
            .unwrap_or(0);
        let min_tokens = entry
            .get_number_attr("min_tokens")
            .map(|n| n as usize)
            .unwrap_or(1);
        let description = entry
            .get_string_attr("description")
            .unwrap_or("")
            .to_string();
        let pattern_type = entry.get_string_attr("type").unwrap_or("wildcard");

        Some(match pattern_type {
            "definition" => Pattern::Definition {
                id,
                name,
                category,
                priority,
                min_tokens,
                description,
                rules: Vec::new(),
            },
            "literal" => Pattern::Literal {
                id,
                name,
                category,
                priority,
                min_tokens,
                description,
                elements: Vec::new(),
            },
            "predicate" => Pattern::Predicate {
                id,
                name,
                category,
                priority,
                min_tokens,
                description,
                predicate_name: "unknown",
            },
            "sequence" => Pattern::Sequence {
                id,
                name,
                category,
                priority,
                min_tokens,
                description,
                patterns: Vec::new(),
            },
            "choice" => Pattern::Choice {
                id,
                name,
                category,
                priority,
                min_tokens,
                description,
                alternatives: Vec::new(),
            },
            "optional" => Pattern::Optional {
                id,
                name: name.clone(),
                category: category.clone(),
                priority,
                min_tokens,
                description: description.clone(),
                inner: Box::new(Pattern::Wildcard {
                    id: 0,
                    name,
                    category,
                    priority: 0,
                    min_tokens: 1,
                    description,
                }),
            },
            "repeat" => Pattern::Repeat {
                id,
                name: name.clone(),
                category: category.clone(),
                priority,
                min_tokens,
                description: description.clone(),
                inner: Box::new(Pattern::Wildcard {
                    id: 0,
                    name,
                    category,
                    priority: 0,
                    min_tokens: 1,
                    description,
                }),
                min_repeat: 0,
                max_repeat: None,
            },
            _ => Pattern::Wildcard {
                id,
                name,
                category,
                priority,
                min_tokens,
                description,
            },
        })
    }
}

/// Pattern matching implementation for Definition patterns
impl Pattern<String> {
    /// Check if tokens match this pattern's rules (for Definition patterns)
    /// Returns confidence score (0.0 - 1.0) or None if no match
    pub fn matches_tokens(&self, tokens: &[String]) -> Option<f64> {
        let rules = match self {
            Pattern::Definition {
                rules, min_tokens, ..
            } => {
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
            let preceding = &tokens[..token_idx];

            let matches = Self::rule_matches_token(rule, token, tokens.get(token_idx + 1), preceding);

            if matches {
                matched_rules += 1;
                token_idx += 1;

                // Handle repeating rules (non-greedy: stop when next rule can match)
                if rule.repeatable != REPEAT_NONE && token_idx < tokens.len() {
                    let next_rule = rules.get(rule_idx + 1);
                    let mut repeat_count = 1; // Already matched once
                    
                    while token_idx < tokens.len() {
                        // Check if next rule can match current token - if so, stop repeating
                        if let Some(nr) = next_rule {
                            let preceding = &tokens[..token_idx];
                            if Self::rule_matches_token(
                                nr,
                                &tokens[token_idx],
                                tokens.get(token_idx + 1),
                                preceding,
                            ) {
                                break; // Let next rule handle this token
                            }
                        }
                        
                        // Check repeat limit (-1 = infinite, otherwise limit)
                        if rule.repeatable > 0 && repeat_count >= rule.repeatable {
                            break;
                        }
                        
                        let preceding = &tokens[..token_idx];
                        if Self::rule_matches_token(
                            rule,
                            &tokens[token_idx],
                            tokens.get(token_idx + 1),
                            preceding,
                        ) {
                            token_idx += 1;
                            repeat_count += 1;
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
    /// `preceding_tokens` contains all tokens that came before the current position
    fn rule_matches_token(
        rule: &PatternRule,
        token: &str,
        next_token: Option<&String>,
        preceding_tokens: &[String],
    ) -> bool {
        // Check forbidden_next constraint
        if let Some(next) = next_token {
            if rule.forbid_next.contains(next) {
                return false;
            }
        }

        // Check cannot_precede constraint: fail if any forbidden token appears before
        if !rule.forbid_previous.is_empty() {
            for forbidden in &rule.forbid_previous {
                if preceding_tokens.iter().any(|t| t == forbidden) {
                    return false;
                }
            }
        }

        // Check must_precede constraint: fail if none of the required tokens appear before
        if !rule.require_previous.is_empty() {
            let has_required = rule.require_previous.iter().any(|required| {
                preceding_tokens.iter().any(|t| t == required)
            });
            if !has_required {
                return false;
            }
        }

        // Check directly_before constraint: immediately previous token must match
        if let Some(ref required) = rule.directly_before {
            let prev_token = preceding_tokens.last();
            if prev_token.map(|t| t.as_str()) != Some(required.as_str()) {
                return false;
            }
        }

        // Check not_directly_before constraint: immediately previous token must NOT match
        if let Some(ref forbidden) = rule.not_directly_before {
            if let Some(prev_token) = preceding_tokens.last() {
                if prev_token == forbidden {
                    return false;
                }
            }
        }

        // Check directly_after constraint: immediately next token must match
        if let Some(ref required) = rule.directly_after {
            if next_token.map(|t| t.as_str()) != Some(required.as_str()) {
                return false;
            }
        }

        // Check not_directly_after constraint: immediately next token must NOT match
        if let Some(ref forbidden) = rule.not_directly_after {
            if let Some(next) = next_token {
                if next == forbidden {
                    return false;
                }
            }
        }

        match &rule.rule_type {
            RuleType::Exact => token == rule.value,
            RuleType::TypeKeyword => {
                crate::db::keyword::is_c_type_keyword(token)
                    || matches!(
                        token,
                        "bool"
                            | "size_t"
                            | "uint8_t"
                            | "uint16_t"
                            | "uint32_t"
                            | "uint64_t"
                            | "int8_t"
                            | "int16_t"
                            | "int32_t"
                            | "int64_t"
                    )
                    // Also match registered custom types (typedefs, structs, enums, unions)
                    || crate::system::system().is_custom_type(token)
            }
            RuleType::Keyword => {
                crate::db::keyword::is_c_control_flow(token)
                    || crate::db::keyword::is_c_storage_class(token)
                    || crate::db::keyword::is_c_type_qualifier(token)
                    || matches!(token, "typedef" | "inline")
            }
            RuleType::Identifier => {
                !token.is_empty()
                    && (token.chars().next().unwrap().is_alphabetic() || token.starts_with('_'))
                    && token.chars().all(|c| c.is_alphanumeric() || c == '_')
            }
            RuleType::OneOf => rule.value.split('|').any(|opt| opt == token),
            RuleType::Any => true,
            RuleType::AnyExcept => {
                // Exclusions stored as pipe-separated in rule.value
                !rule.value.split('|').any(|excl| excl == token)
            }
            RuleType::Custom(_) => true,
        }
    }

    /// Extract tokens with captures - returns ExtractedMatch with captured data per rule
    /// This allows handlers to easily access matched tokens by rule index or type
    pub fn extract_with_captures(&self, tokens: &[String]) -> Option<ExtractedMatch> {
        let (rules, pattern_name) = match self {
            Pattern::Definition {
                rules,
                min_tokens,
                name,
                ..
            } => {
                if tokens.len() < *min_tokens {
                    return None;
                }
                (rules, name.as_str())
            }
            _ => return None,
        };

        if rules.is_empty() {
            return None;
        }

        let mut result = ExtractedMatch::new(pattern_name);
        result.matched_tokens = tokens.to_vec();

        let mut token_idx = 0;
        let mut rule_idx = 0;

        // Common C modifiers to detect
        let modifiers = [
            "const", "static", "extern", "volatile", "inline", "register", "unsigned", "signed",
        ];

        while rule_idx < rules.len() && token_idx < tokens.len() {
            let rule = &rules[rule_idx];
            let token = &tokens[token_idx];
            let preceding = &tokens[..token_idx];

            // Track modifiers
            if modifiers.contains(&token.as_str()) {
                result.modifiers.insert(token.clone());
            }

            let matches = Self::rule_matches_token(rule, token, tokens.get(token_idx + 1), preceding);

            if matches {
                let start = token_idx;
                token_idx += 1;

                // Handle repeating rules - collect all matching tokens
                let mut repeated = false;
                if rule.repeatable != REPEAT_NONE && token_idx < tokens.len() {
                    while token_idx < tokens.len() {
                        let preceding = &tokens[..token_idx];
                        if Self::rule_matches_token(
                            rule,
                            &tokens[token_idx],
                            tokens.get(token_idx + 1),
                            preceding,
                        ) {
                            // Track modifiers in repeated tokens too
                            if modifiers.contains(&tokens[token_idx].as_str()) {
                                result.modifiers.insert(tokens[token_idx].clone());
                            }
                            token_idx += 1;
                            repeated = true;
                        } else {
                            break;
                        }
                    }
                }

                // Create capture for this rule
                let capture = Capture {
                    rule_idx,
                    start,
                    end: token_idx,
                    tokens: tokens[start..token_idx].to_vec(),
                    rule_type: Some(rule.rule_type.clone()),
                    repeated,
                };

                // Add to named captures if rule has a value (for Exact rules)
                if !rule.value.is_empty() {
                    result.named.insert(rule.value.clone(), capture.clone());
                }

                result.captures.push(capture);
                rule_idx += 1;
            } else if rule.optional {
                // Add empty capture for optional rules that didn't match
                result.captures.push(Capture {
                    rule_idx,
                    start: token_idx,
                    end: token_idx,
                    tokens: Vec::new(),
                    rule_type: Some(rule.rule_type.clone()),
                    repeated: false,
                });
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
            result.captures.push(Capture {
                rule_idx,
                start: token_idx,
                end: token_idx,
                tokens: Vec::new(),
                rule_type: Some(rules[rule_idx].rule_type.clone()),
                repeated: false,
            });
            rule_idx += 1;
        }

        result.tokens_consumed = token_idx;

        // Extract delimited sections (content between matching brackets)
        Self::extract_delimited(&mut result, tokens);

        // Calculate confidence
        let total_rules = rules.iter().filter(|r| !r.optional).count();
        result.confidence = if total_rules == 0 {
            0.7
        } else {
            let matched = result
                .captures
                .iter()
                .filter(|c| !c.tokens.is_empty())
                .count();
            (matched as f64 / total_rules as f64).min(1.0) * 0.9 + 0.1
        };

        Some(result)
    }

    /// Execute extraction pattern with callbacks, returning populated ExtractContext
    /// This runs the ExtractCallback closures attached to each rule as tokens are matched
    pub fn run_extraction(&self, tokens: &[String]) -> Option<ExtractContext> {
        let rules = match self {
            Pattern::Definition {
                rules, min_tokens, ..
            } => {
                if tokens.len() < *min_tokens {
                    return None;
                }
                rules
            }
            _ => return None,
        };

        if rules.is_empty() {
            return None;
        }

        let mut ctx = ExtractContext::new(tokens);
        let mut token_idx = 0;
        let mut rule_idx = 0;

        while rule_idx < rules.len() && token_idx < tokens.len() {
            let rule = &rules[rule_idx];
            let token = &tokens[token_idx];
            let preceding = &tokens[..token_idx];

            // Set current token in context for callback access
            ctx.current_token = token.clone();
            ctx.token_idx = token_idx;

            let matches = Self::rule_matches_token(rule, token, tokens.get(token_idx + 1), preceding);
            ctx.did_match = matches;

            if matches {
                ctx.matched = Some(token.clone());

                // Execute extraction callback if present
                if rule.has_extract() {
                    rule.apply_extract(&mut ctx);
                }

                token_idx += 1;

                // Handle repeating rules
                if rule.repeatable != REPEAT_NONE && token_idx < tokens.len() {
                    while token_idx < tokens.len() {
                        let next_token = &tokens[token_idx];
                        let preceding = &tokens[..token_idx];
                        if Self::rule_matches_token(rule, next_token, tokens.get(token_idx + 1), preceding) {
                            ctx.current_token = next_token.clone();
                            ctx.token_idx = token_idx;
                            ctx.did_match = true;
                            ctx.matched = Some(next_token.clone());

                            if rule.has_extract() {
                                rule.apply_extract(&mut ctx);
                            }

                            token_idx += 1;
                        } else {
                            break;
                        }
                    }
                }

                rule_idx += 1;
            } else if rule.optional {
                ctx.matched = None;
                ctx.did_match = false;

                // Still call callback for optional rules that didn't match
                if rule.has_extract() {
                    rule.apply_extract(&mut ctx);
                }

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

        Some(ctx)
    }

    /// Extract content between matching delimiters
    fn extract_delimited(result: &mut ExtractedMatch, tokens: &[String]) {
        let pairs = [("{", "}"), ("(", ")"), ("[", "]")];

        for (open, close) in pairs {
            let mut depth = 0;
            let mut start_idx = None;
            let mut content = Vec::new();

            for (idx, token) in tokens.iter().enumerate() {
                if token == open {
                    if depth == 0 {
                        start_idx = Some(idx);
                    }
                    depth += 1;
                } else if token == close {
                    depth -= 1;
                    if depth == 0 && start_idx.is_some() {
                        let key = format!("{}:{}", open, close);
                        result.delimited.insert(key, content.clone());
                        content.clear();
                        start_idx = None;
                    }
                } else if depth > 0 {
                    content.push(token.clone());
                }
            }
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

        let start = entry
            .get_number_attr("start")
            .map(|n| n as usize)
            .unwrap_or(0);
        let end = entry
            .get_number_attr("end")
            .map(|n| n as usize)
            .unwrap_or(0);
        let pattern_id = entry
            .get_number_attr("pattern_id")
            .map(|n| n as usize)
            .unwrap_or(0);

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

    fn category(&self) -> Option<&str> {
        Some("pattern")
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
            a.start
                .cmp(&b.start)
                .then_with(|| a.end.cmp(&b.end))
                .then_with(|| {
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
        Self {
            current: next,
            ..self
        }
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
        self.machine
            .add_exact_edge(self.current, value, self.current);
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
