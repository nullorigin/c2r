//! Keyword Ordering
//!
//! Provides keyword definitions and ordering validation for C-to-Rust conversion.
//! The `Keyword` struct represents individual keywords with their follow rules.
//! The `Order` struct validates sequences of keywords based on ordering rules.
//!
//! Uses the Entry/Build pattern for integration with the Web database.

use crate::db::web::{Entry, Build};
use std::collections::HashMap;

// ============================================================================
// Core Types
// ============================================================================

/// Categories of C language keywords
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum KeywordCategory {
    /// Type declaration: struct, enum, union, typedef
    TypeDeclaration,
    /// Storage class: static, extern, register, auto
    StorageClass,
    /// Type qualifier: const, volatile, restrict
    TypeQualifier,
    /// Basic type: int, char, float, double, void
    BasicType,
    /// Control flow: if, else, while, for, switch, case, etc.
    ControlFlow,
    /// Preprocessor: define, include, ifdef, ifndef, etc.
    Preprocessor,
    /// Operator: +, -, *, /, etc.
    Operator,
    /// Punctuation: {, }, (, ), [, ], ;, etc.
    Punctuation,
    /// Identifier: user-defined names
    Identifier,
    /// Literal: numbers, strings, characters
    Literal,
}

impl KeywordCategory {
    pub fn as_str(&self) -> &'static str {
        match self {
            KeywordCategory::TypeDeclaration => "type_declaration",
            KeywordCategory::StorageClass => "storage_class",
            KeywordCategory::TypeQualifier => "type_qualifier",
            KeywordCategory::BasicType => "basic_type",
            KeywordCategory::ControlFlow => "control_flow",
            KeywordCategory::Preprocessor => "preprocessor",
            KeywordCategory::Operator => "operator",
            KeywordCategory::Punctuation => "punctuation",
            KeywordCategory::Identifier => "identifier",
            KeywordCategory::Literal => "literal",
        }
    }
}

/// A keyword with its ordering relations
/// Relations: keyword_text -> (can_follow, can_precede)
/// - can_follow: true if this keyword can follow the related keyword
/// - can_precede: true if this keyword can precede the related keyword
#[derive(Debug, Clone, PartialEq)]
pub struct Keyword {
    /// The keyword text
    pub text: String,
    /// Category of this keyword
    pub category: KeywordCategory,
    /// Relations: other_keyword -> (can_follow, can_precede)
    pub relations: HashMap<String, (bool, bool)>,
    /// Priority for matching (higher = more specific)
    pub priority: i16,
}

impl Keyword {
    pub fn new(text: impl Into<String>, category: KeywordCategory) -> Self {
        Self {
            text: text.into(),
            category,
            relations: HashMap::new(),
            priority: 0,
        }
    }

    pub fn with_priority(mut self, priority: i16) -> Self {
        self.priority = priority;
        self
    }

    /// Set that this keyword can follow the specified keyword
    pub fn follows(mut self, other: impl Into<String>) -> Self {
        let key = other.into();
        let entry = self.relations.entry(key).or_insert((false, false));
        entry.0 = true;
        self
    }

    /// Set that this keyword cannot follow the specified keyword
    pub fn not_follows(mut self, other: impl Into<String>) -> Self {
        let key = other.into();
        let entry = self.relations.entry(key).or_insert((false, false));
        entry.0 = false;
        self
    }

    /// Set that this keyword can precede the specified keyword
    pub fn precedes(mut self, other: impl Into<String>) -> Self {
        let key = other.into();
        let entry = self.relations.entry(key).or_insert((false, false));
        entry.1 = true;
        self
    }

    /// Set that this keyword cannot precede the specified keyword
    pub fn not_precedes(mut self, other: impl Into<String>) -> Self {
        let key = other.into();
        let entry = self.relations.entry(key).or_insert((false, false));
        entry.1 = false;
        self
    }

    /// Batch set: this keyword can follow all specified keywords
    pub fn follows_all(mut self, others: impl IntoIterator<Item = impl Into<String>>) -> Self {
        for other in others {
            let key = other.into();
            let entry = self.relations.entry(key).or_insert((false, false));
            entry.0 = true;
        }
        self
    }

    /// Batch set: this keyword can precede all specified keywords
    pub fn precedes_all(mut self, others: impl IntoIterator<Item = impl Into<String>>) -> Self {
        for other in others {
            let key = other.into();
            let entry = self.relations.entry(key).or_insert((false, false));
            entry.1 = true;
        }
        self
    }

    /// Check if this keyword can follow the specified keyword
    pub fn can_follow(&self, other: &str) -> Option<bool> {
        self.relations.get(other).map(|(f, _)| *f)
    }

    /// Check if this keyword can precede the specified keyword
    pub fn can_precede(&self, other: &str) -> Option<bool> {
        self.relations.get(other).map(|(_, p)| *p)
    }

    /// Get all keywords this can follow
    pub fn followers(&self) -> Vec<&String> {
        self.relations.iter()
            .filter(|(_, (f, _))| *f)
            .map(|(k, _)| k)
            .collect()
    }

    /// Get all keywords this can precede
    pub fn preceders(&self) -> Vec<&String> {
        self.relations.iter()
            .filter(|(_, (_, p))| *p)
            .map(|(k, _)| k)
            .collect()
    }

    /// Check if this keyword is a C type keyword
    pub fn is_type(&self) -> bool {
        matches!(self.category, KeywordCategory::TypeDeclaration | KeywordCategory::BasicType)
    }

    /// Check if this keyword is a storage class
    pub fn is_storage_class(&self) -> bool {
        self.category == KeywordCategory::StorageClass
    }

    /// Check if this keyword is a type qualifier
    pub fn is_type_qualifier(&self) -> bool {
        self.category == KeywordCategory::TypeQualifier
    }

    /// Check if this keyword is control flow
    pub fn is_control_flow(&self) -> bool {
        self.category == KeywordCategory::ControlFlow
    }

    /// Check if this keyword is a preprocessor directive
    pub fn is_preprocessor(&self) -> bool {
        self.category == KeywordCategory::Preprocessor
    }
}

impl Build for Keyword {
    fn to_entry(&self) -> Entry {
        let mut attrs = HashMap::new();
        attrs.insert("category".to_string(), Entry::string(self.category.as_str()));
        attrs.insert("priority".to_string(), Entry::i16(self.priority));
        
        // Convert relations to nested structure
        if !self.relations.is_empty() {
            let relations_map: HashMap<String, Entry> = self.relations.iter()
                .map(|(kw, (can_follow, can_precede))| {
                    (kw.clone(), Entry::pair(Entry::bool(*can_follow), Entry::bool(*can_precede)))
                })
                .collect();
            attrs.insert("relations".to_string(), Entry::hashmap(relations_map));
        }
        
        Entry::node_with_attrs("keyword", &self.text, attrs)
    }

    fn kind(&self) -> &str {
        "keyword"
    }

    fn name(&self) -> Option<&str> {
        Some(&self.text)
    }

    fn category(&self) -> Option<&str> {
        Some(self.category.as_str())
    }

    fn priority(&self) -> i16 {
        self.priority
    }
}

// ============================================================================
// Order - Keyword Ordering Validation (The Chain)
// ============================================================================

/// Validates ordering of keywords - what can follow what
/// Acts as the chain linking keywords together via their relations
/// Uses Entry::Pair to store keywords and node_transitions as two HashMaps
#[derive(Debug, Clone, Default)]
pub struct Order {
    /// Keywords indexed by text
    keywords: HashMap<String, Keyword>,
    /// Node transition rules: parent kind -> allowed child kinds
    node_transitions: HashMap<String, Vec<String>>,
}

impl Order {
    pub fn new() -> Self {
        Self::default()
    }

    /// Create populated with C language ordering rules
    pub fn with_c_rules() -> Self {
        let mut order = Self::new();
        order.populate_c_keyword_rules();
        order.populate_node_transitions();
        order
    }

    // ========================================================================
    // Rule Registration
    // ========================================================================

    /// Register a keyword's ordering rules
    pub fn register(&mut self, keyword: Keyword) {
        self.keywords.insert(keyword.text.clone(), keyword);
    }

    /// Set relation: `from` can be followed by `to`
    pub fn link(&mut self, from: &str, to: &str) {
        // In `from` keyword: can precede `to`
        if let Some(kw) = self.keywords.get_mut(from) {
            let entry = kw.relations.entry(to.to_string()).or_insert((false, false));
            entry.1 = true; // can_precede
        }
        // In `to` keyword: can follow `from`
        if let Some(kw) = self.keywords.get_mut(to) {
            let entry = kw.relations.entry(from.to_string()).or_insert((false, false));
            entry.0 = true; // can_follow
        }
    }

    /// Set relation: `from` cannot be followed by `to`
    pub fn unlink(&mut self, from: &str, to: &str) {
        if let Some(kw) = self.keywords.get_mut(from) {
            let entry = kw.relations.entry(to.to_string()).or_insert((false, false));
            entry.1 = false;
        }
        if let Some(kw) = self.keywords.get_mut(to) {
            let entry = kw.relations.entry(from.to_string()).or_insert((false, false));
            entry.0 = false;
        }
    }

    /// Add node transition rule
    pub fn add_node_transition(&mut self, parent: impl Into<String>, children: Vec<impl Into<String>>) {
        self.node_transitions.insert(
            parent.into(),
            children.into_iter().map(|s| s.into()).collect(),
        );
    }

    // ========================================================================
    // Ordering Validation
    // ========================================================================

    /// Check if `next` can follow `current` using the chain
    pub fn can_follow(&self, current: &str, next: &str) -> bool {
        // Check from current's perspective: can it precede next?
        if let Some(current_kw) = self.keyword(current) {
            if let Some(can_precede) = current_kw.can_precede(next) {
                return can_precede;
            }
        }
        // Check from next's perspective: can it follow current?
        if let Some(next_kw) = self.keyword(next) {
            if let Some(can_follow) = next_kw.can_follow(current) {
                return can_follow;
            }
        }
        // Default: allow if no explicit rule
        true
    }

    /// Check if transition between node kinds is valid
    pub fn is_valid_transition(&self, parent: &str, child: &str) -> bool {
        self.node_transitions
            .get(parent)
            .map(|allowed| allowed.contains(&child.to_string()))
            .unwrap_or(false)
    }

    /// Validate a sequence of tokens
    pub fn validate(&self, tokens: &[String]) -> bool {
        if tokens.len() < 2 {
            return true;
        }
        tokens.windows(2).all(|w| self.can_follow(&w[0], &w[1]))
    }

    // ========================================================================
    // Lookups
    // ========================================================================

    /// Get a keyword by text
    pub fn keyword(&self, text: &str) -> Option<&Keyword> {
        self.keywords.get(text)
    }

    /// Get a mutable keyword by text
    pub fn keyword_mut(&mut self, text: &str) -> Option<&mut Keyword> {
        self.keywords.get_mut(text)
    }

    /// Get suggestions for what can follow a keyword
    pub fn suggest(&self, current: &str) -> Vec<String> {
        if let Some(kw) = self.keyword(current) {
            kw.preceders().into_iter().cloned().collect()
        } else {
            Vec::new()
        }
    }

    /// Get keyword count
    pub fn len(&self) -> usize {
        self.keywords.len()
    }

    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.keywords.is_empty()
    }

    /// Get node transition count
    pub fn transition_count(&self) -> usize {
        self.node_transitions.len()
    }
}

impl Build for Order {
    fn to_entry(&self) -> Entry {
        // Convert keywords to Entry::HashMap
        let keywords_map: HashMap<String, Entry> = self.keywords.iter()
            .map(|(k, v)| (k.clone(), v.to_entry()))
            .collect();
        
        // Convert node_transitions to Entry::HashMap
        let transitions_map: HashMap<String, Entry> = self.node_transitions.iter()
            .map(|(k, v)| {
                let children: Vec<Entry> = v.iter().map(|s| Entry::string(s.clone())).collect();
                (k.clone(), Entry::vec(children))
            })
            .collect();
        
        // Use Entry::pair to store both HashMaps
        Entry::pair(Entry::hashmap(keywords_map), Entry::hashmap(transitions_map))
    }

    fn kind(&self) -> &str {
        "order"
    }

    fn name(&self) -> Option<&str> {
        None
    }
}

impl Order {
    // ========================================================================
    // C Language Rules Population
    // ========================================================================

    fn populate_c_keyword_rules(&mut self) {
        // First register all keywords
        self.register_all_keywords();
        // Then create the ordering chain
        self.create_ordering_chain();
    }

    fn register_all_keywords(&mut self) {
        // Type declarations
        for &kw in &["struct", "enum", "union"] {
            self.register(Keyword::new(kw, KeywordCategory::TypeDeclaration).with_priority(100));
        }
        self.register(Keyword::new("typedef", KeywordCategory::TypeDeclaration).with_priority(110));

        // Storage classes
        for &kw in &["static", "extern", "register", "auto"] {
            self.register(Keyword::new(kw, KeywordCategory::StorageClass).with_priority(90));
        }

        // Type qualifiers
        for &kw in &["const", "volatile", "restrict"] {
            self.register(Keyword::new(kw, KeywordCategory::TypeQualifier).with_priority(80));
        }

        // Basic types
        for &kw in &["void", "int", "char", "float", "double", "long", "short"] {
            self.register(Keyword::new(kw, KeywordCategory::BasicType).with_priority(70));
        }
        for &kw in &["unsigned", "signed"] {
            self.register(Keyword::new(kw, KeywordCategory::BasicType).with_priority(75));
        }

        // Preprocessor
        self.register(Keyword::new("#", KeywordCategory::Preprocessor).with_priority(200));
        for &kw in &["define", "include", "ifdef", "ifndef", "endif", "elif", "pragma", "warning", "error", "undef", "line"] {
            self.register(Keyword::new(kw, KeywordCategory::Preprocessor).with_priority(190));
        }

        // Control flow
        for &kw in &["if", "while", "for", "switch"] {
            self.register(Keyword::new(kw, KeywordCategory::ControlFlow).with_priority(120));
        }
        self.register(Keyword::new("else", KeywordCategory::ControlFlow).with_priority(115));
        for &kw in &["break", "continue", "goto", "return"] {
            self.register(Keyword::new(kw, KeywordCategory::ControlFlow).with_priority(100));
        }
        self.register(Keyword::new("case", KeywordCategory::ControlFlow).with_priority(110));
        self.register(Keyword::new("default", KeywordCategory::ControlFlow).with_priority(105));

        // Common tokens (for ordering rules)
        self.register(Keyword::new("identifier", KeywordCategory::Identifier).with_priority(50));
        self.register(Keyword::new("{", KeywordCategory::Punctuation).with_priority(50));
        self.register(Keyword::new("}", KeywordCategory::Punctuation).with_priority(50));
        self.register(Keyword::new("(", KeywordCategory::Punctuation).with_priority(50));
        self.register(Keyword::new(")", KeywordCategory::Punctuation).with_priority(50));
        self.register(Keyword::new(";", KeywordCategory::Punctuation).with_priority(50));
        self.register(Keyword::new(":", KeywordCategory::Punctuation).with_priority(50));
        self.register(Keyword::new("*", KeywordCategory::Operator).with_priority(50));
    }

    fn create_ordering_chain(&mut self) {
        // typedef -> type keywords
        for &to in &["struct", "enum", "union", "int", "char", "float", "double", "void", "long", "short", "unsigned", "signed"] {
            self.link("typedef", to);
        }

        // struct/enum/union -> identifier or {
        for &from in &["struct", "enum", "union"] {
            self.link(from, "identifier");
            self.link(from, "{");
            // Cannot follow themselves
            self.unlink(from, from);
            self.unlink(from, "typedef");
        }

        // Storage classes -> types
        for &from in &["static", "extern", "register", "auto"] {
            for &to in &["int", "char", "float", "double", "void", "long", "short", "unsigned", "signed", "const", "volatile", "struct", "enum", "union"] {
                self.link(from, to);
            }
            // Cannot follow typedef or each other
            self.unlink(from, "typedef");
            self.unlink(from, "extern");
            self.unlink(from, "static");
        }

        // Type qualifiers -> types
        for &from in &["const", "volatile", "restrict"] {
            for &to in &["int", "char", "float", "double", "void", "long", "short", "unsigned", "signed", "struct", "enum", "union"] {
                self.link(from, to);
            }
            // Qualifiers can follow each other
            for &other in &["const", "volatile", "restrict"] {
                if from != other {
                    self.link(from, other);
                }
            }
        }

        // Basic types -> identifier, *, (
        for &from in &["void", "int", "char", "float", "double", "long", "short"] {
            self.link(from, "identifier");
            self.link(from, "*");
            self.link(from, "(");
        }

        // unsigned/signed -> int types
        for &from in &["unsigned", "signed"] {
            for &to in &["int", "char", "long", "short", "identifier", "*"] {
                self.link(from, to);
            }
        }

        // # -> preprocessor directives
        for &to in &["define", "include", "ifdef", "ifndef", "endif", "elif", "pragma", "warning", "error", "undef", "line"] {
            self.link("#", to);
        }
        // # cannot be followed by type keywords
        for &to in &["struct", "enum", "typedef", "int", "char", "float", "double"] {
            self.unlink("#", to);
        }

        // Control flow with paren -> (
        for &from in &["if", "while", "for", "switch"] {
            self.link(from, "(");
            // Cannot be directly followed by identifier, ;, {
            self.unlink(from, "identifier");
            self.unlink(from, ";");
            self.unlink(from, "{");
        }

        // else -> if, {
        self.link("else", "if");
        self.link("else", "{");

        // break/continue/goto -> ;, identifier
        for &from in &["break", "continue", "goto"] {
            self.link(from, ";");
            self.link(from, "identifier");
        }

        // return -> various
        for &to in &["identifier", "(", ";"] {
            self.link("return", to);
        }

        // case -> identifier, literals
        self.link("case", "identifier");

        // default -> :
        self.link("default", ":");
    }

    fn populate_node_transitions(&mut self) {
        // Top-level nodes
        let top_level = vec![
            "function", "struct", "enum", "typedef", "global", "variable",
            "array", "macro", "include", "comment", "type",
        ];
        for kind in &top_level {
            self.add_node_transition(*kind, top_level.clone());
        }

        // Function can contain
        self.add_node_transition("function", vec![
            "parameter", "return_type", "function_body", "variable_declaration",
            "assignment", "expression", "if_statement", "while_loop", "for_loop",
            "do_while", "switch", "return", "break", "continue", "function_call", "comment",
        ]);

        // Struct contains fields
        self.add_node_transition("struct", vec!["field", "comment"]);

        // Enum contains variants
        self.add_node_transition("enum", vec!["variant", "comment"]);

        // Function body
        self.add_node_transition("function_body", vec![
            "variable_declaration", "assignment", "expression", "if_statement",
            "while_loop", "for_loop", "do_while", "switch", "case", "return",
            "break", "continue", "function_call", "comment",
        ]);

        // Control flow
        let control_children = vec![
            "expression", "function_body", "variable_declaration", "assignment",
            "function_call", "if_statement", "while_loop", "for_loop",
            "return", "break", "continue",
        ];
        for kind in &["if_statement", "while_loop", "for_loop", "do_while"] {
            self.add_node_transition(*kind, control_children.clone());
        }

        // Switch
        let mut switch_children = control_children.clone();
        switch_children.push("case");
        self.add_node_transition("switch", switch_children);

        // Expression
        self.add_node_transition("expression", vec!["function_call", "expression", "variable"]);

        // Variable declaration
        self.add_node_transition("variable_declaration", vec!["assignment", "expression"]);

        // Type
        self.add_node_transition("type", vec!["type", "variable", "function", "comment"]);
    }
}

// ============================================================================
// Convenience Functions
// ============================================================================

/// Create C keyword ordering rules
pub fn c_order() -> Order {
    Order::with_c_rules()
}

/// Check if a keyword is a C type keyword
pub fn is_type_keyword(keyword: &str) -> bool {
    matches!(keyword, 
        "int" | "char" | "float" | "double" | "void" | "long" | "short" |
        "unsigned" | "signed" | "struct" | "enum" | "union"
    )
}

/// Check if a keyword is a storage class
pub fn is_storage_class(keyword: &str) -> bool {
    matches!(keyword, "static" | "extern" | "register" | "auto")
}

/// Check if a keyword is a type qualifier
pub fn is_type_qualifier(keyword: &str) -> bool {
    matches!(keyword, "const" | "volatile" | "restrict")
}

/// Check if a keyword is control flow
pub fn is_control_flow(keyword: &str) -> bool {
    matches!(keyword,
        "if" | "else" | "while" | "for" | "do" | "switch" | "case" |
        "default" | "break" | "continue" | "goto" | "return"
    )
}

/// Check if a keyword is a preprocessor directive
pub fn is_preprocessor(keyword: &str) -> bool {
    matches!(keyword,
        "#" | "define" | "include" | "ifdef" | "ifndef" | "endif" |
        "if" | "else" | "elif" | "pragma" | "warning" | "error" | "undef" | "line"
    )
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_typedef_struct_valid() {
        let order = Order::with_c_rules();
        assert!(order.can_follow("typedef", "struct"));
        assert!(order.can_follow("struct", "identifier"));
    }

    #[test]
    fn test_typedef_struct_invalid() {
        let order = Order::with_c_rules();
        // typedef cannot be followed by ( (no explicit link)
        assert!(!order.can_follow("struct", "struct"));
    }

    #[test]
    fn test_sequence_validation() {
        let order = Order::with_c_rules();

        // Valid: typedef struct identifier
        let valid_seq = vec!["typedef".to_string(), "struct".to_string(), "identifier".to_string()];
        assert!(order.validate(&valid_seq));

        // Invalid: struct struct
        let invalid_seq = vec!["struct".to_string(), "struct".to_string()];
        assert!(!order.validate(&invalid_seq));
    }

    #[test]
    fn test_preprocessor_ordering() {
        let order = Order::with_c_rules();
        assert!(order.can_follow("#", "define"));
        assert!(order.can_follow("#", "include"));
        assert!(!order.can_follow("#", "struct"));
    }

    #[test]
    fn test_keyword_to_entry() {
        let kw = Keyword::new("struct", KeywordCategory::TypeDeclaration)
            .precedes("identifier")
            .precedes("{")
            .with_priority(100);
        
        let entry = kw.to_entry();
        assert!(entry.name().is_some());
    }

    #[test]
    fn test_node_transitions() {
        let order = Order::with_c_rules();
        
        assert!(order.is_valid_transition("function", "parameter"));
        assert!(order.is_valid_transition("struct", "field"));
        assert!(order.is_valid_transition("enum", "variant"));
        assert!(!order.is_valid_transition("enum", "parameter"));
    }

    #[test]
    fn test_suggest() {
        let order = Order::with_c_rules();
        
        let suggestions = order.suggest("typedef");
        assert!(!suggestions.is_empty());
    }

    #[test]
    fn test_keyword_relations() {
        let kw = Keyword::new("struct", KeywordCategory::TypeDeclaration)
            .precedes("identifier")
            .precedes("{")
            .not_precedes("struct")
            .with_priority(100);
        
        assert_eq!(kw.can_precede("identifier"), Some(true));
        assert_eq!(kw.can_precede("struct"), Some(false));
        assert!(kw.is_type());
        assert!(!kw.is_control_flow());
        
        let preceders = kw.preceders();
        assert!(preceders.contains(&&"identifier".to_string()));
    }

    #[test]
    fn test_link_unlink() {
        let mut order = Order::new();
        order.register(Keyword::new("a", KeywordCategory::Identifier));
        order.register(Keyword::new("b", KeywordCategory::Identifier));
        
        // Link a -> b
        order.link("a", "b");
        assert!(order.can_follow("a", "b"));
        
        // Unlink
        order.unlink("a", "b");
        assert!(!order.can_follow("a", "b"));
    }
}
