//! Keyword Ordering
//!
//! Provides keyword definitions and ordering validation for C-to-Rust conversion.
//! The `Keyword` struct represents individual keywords with their follow rules.
//! The `Order` struct validates sequences of keywords based on ordering rules.
//!
//! Uses the Entry/Build pattern for integration with the Web database.

use crate::db::web::{Build, Entry};
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
        self.relations
            .iter()
            .filter(|(_, (f, _))| *f)
            .map(|(k, _)| k)
            .collect()
    }

    /// Get all keywords this can precede
    pub fn preceders(&self) -> Vec<&String> {
        self.relations
            .iter()
            .filter(|(_, (_, p))| *p)
            .map(|(k, _)| k)
            .collect()
    }

    /// Check if this keyword is a C type keyword
    pub fn is_type(&self) -> bool {
        matches!(
            self.category,
            KeywordCategory::TypeDeclaration | KeywordCategory::BasicType
        )
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
        attrs.insert(
            "category".to_string(),
            Entry::string(self.category.as_str()),
        );
        attrs.insert("priority".to_string(), Entry::i16(self.priority));

        // Convert relations to nested structure
        if !self.relations.is_empty() {
            let relations_map: HashMap<String, Entry> = self
                .relations
                .iter()
                .map(|(kw, (can_follow, can_precede))| {
                    (
                        kw.clone(),
                        Entry::pair(Entry::bool(*can_follow), Entry::bool(*can_precede)),
                    )
                })
                .collect();
            attrs.insert("relations".to_string(), Entry::hashmap(relations_map));
        }

        Entry::node_with_attrs("Keyword", &self.text, attrs)
    }

    fn kind(&self) -> &str {
        "Keyword"
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
#[derive(Debug, Clone)]
pub struct Order {
    /// Language this order is for ("c" or "rust")
    language: String,
    /// Keywords indexed by text
    keywords: HashMap<String, Keyword>,
    /// Node transition rules: parent kind -> allowed child kinds
    node_transitions: HashMap<String, Vec<String>>,
}

impl Default for Order {
    fn default() -> Self {
        Self {
            language: "c".to_string(),
            keywords: HashMap::new(),
            node_transitions: HashMap::new(),
        }
    }
}

impl Order {
    pub fn new() -> Self {
        Self::default()
    }

    /// Create populated with C language ordering rules
    pub fn with_c_rules() -> Self {
        let mut order = Self::new();
        order.populate_c_keyword_rules();
        order.populate_c_node_transitions();
        order
    }

    // ========================================================================
    // Rule Registration
    // ========================================================================

    /// Register a keyword's ordering rules
    pub fn register_keyword(&mut self, keyword: Keyword) {
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
            let entry = kw
                .relations
                .entry(from.to_string())
                .or_insert((false, false));
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
            let entry = kw
                .relations
                .entry(from.to_string())
                .or_insert((false, false));
            entry.0 = false;
        }
    }

    /// Add node transition rule
    pub fn add_node_transition(
        &mut self,
        parent: impl Into<String>,
        children: Vec<impl Into<String>>,
    ) {
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
        let mut attrs = HashMap::new();

        // Add language identifier
        attrs.insert("language".to_string(), Entry::string(&self.language));

        // Add statistics
        attrs.insert("keyword_count".to_string(), Entry::usize(self.keywords.len()));
        attrs.insert("transition_count".to_string(), Entry::usize(self.node_transitions.len()));

        // Convert keywords to Entry::HashMap with full details
        let keywords_map: HashMap<String, Entry> = self
            .keywords
            .iter()
            .map(|(k, v)| (k.clone(), v.to_entry()))
            .collect();
        attrs.insert("keywords".to_string(), Entry::hashmap(keywords_map));

        // Convert node_transitions to Entry::HashMap
        let transitions_map: HashMap<String, Entry> = self
            .node_transitions
            .iter()
            .map(|(k, v)| {
                let children: Vec<Entry> = v.iter().map(|s| Entry::string(s.clone())).collect();
                (k.clone(), Entry::vec(children))
            })
            .collect();
        attrs.insert("node_transitions".to_string(), Entry::hashmap(transitions_map));

        // Group keywords by category for easier analysis
        let mut by_category: HashMap<String, Vec<Entry>> = HashMap::new();
        for kw in self.keywords.values() {
            let cat = kw.category.as_str().to_string();
            by_category
                .entry(cat)
                .or_default()
                .push(Entry::string(&kw.text));
        }
        let category_map: HashMap<String, Entry> = by_category
            .into_iter()
            .map(|(k, v)| (k, Entry::vec(v)))
            .collect();
        attrs.insert("keywords_by_category".to_string(), Entry::hashmap(category_map));

        // Use language-specific name
        let order_name = format!("{}_keyword_ordering", self.language);
        Entry::node_with_attrs("Order", &order_name, attrs)
    }

    fn kind(&self) -> &str {
        "Order"
    }

    fn name(&self) -> Option<&str> {
        // Return language-specific name
        // Note: We can't return a reference to a computed string, so we use a static approach
        None // The actual name is set in to_entry()
    }

    fn category(&self) -> Option<&str> {
        Some("language_rules")
    }

    fn priority(&self) -> i16 {
        200 // High priority for language rules
    }
}

impl Order {
    // ========================================================================
    // C Language Rules Population
    // ========================================================================

    fn populate_c_keyword_rules(&mut self) {
        // First register all keywords
        self.register_all_c_keywords();
        // Then create the ordering chain
        self.create_c_ordering_chain();
    }

    fn register_all_c_keywords(&mut self) {
        // Type declarations (highest priority for type definitions)
        self.register_keyword(Keyword::new("typedef", KeywordCategory::TypeDeclaration).with_priority(110));
        for &kw in &["struct", "enum", "union"] {
            self.register_keyword(Keyword::new(kw, KeywordCategory::TypeDeclaration).with_priority(100));
        }

        // Storage classes (appear before types in declarations)
        for &kw in &[
            "static", "extern", "register", "auto", "inline", "__inline", "__inline__",
        ] {
            self.register_keyword(Keyword::new(kw, KeywordCategory::StorageClass).with_priority(90));
        }

        // Type qualifiers (modify types)
        for &kw in &["const", "volatile", "restrict"] {
            self.register_keyword(Keyword::new(kw, KeywordCategory::TypeQualifier).with_priority(80));
        }

        // Basic types - sign modifiers have higher priority as they come first
        for &kw in &["unsigned", "signed"] {
            self.register_keyword(Keyword::new(kw, KeywordCategory::BasicType).with_priority(75));
        }
        // Size modifiers
        for &kw in &["long", "short"] {
            self.register_keyword(Keyword::new(kw, KeywordCategory::BasicType).with_priority(72));
        }
        // Base types
        for &kw in &["void", "int", "char", "float", "double"] {
            self.register_keyword(Keyword::new(kw, KeywordCategory::BasicType).with_priority(70));
        }

        // Preprocessor directives (highest priority - processed first)
        self.register_keyword(Keyword::new("#", KeywordCategory::Preprocessor).with_priority(200));
        for &kw in &[
            "define", "include", "ifdef", "ifndef", "endif", "if", "else", "elif", 
            "pragma", "warning", "error", "undef", "line",
        ] {
            self.register_keyword(Keyword::new(kw, KeywordCategory::Preprocessor).with_priority(190));
        }

        // Control flow - conditional statements
        for &kw in &["if", "while", "for", "switch", "do"] {
            self.register_keyword(Keyword::new(kw, KeywordCategory::ControlFlow).with_priority(120));
        }
        self.register_keyword(Keyword::new("else", KeywordCategory::ControlFlow).with_priority(115));
        self.register_keyword(Keyword::new("case", KeywordCategory::ControlFlow).with_priority(110));
        self.register_keyword(Keyword::new("default", KeywordCategory::ControlFlow).with_priority(105));
        // Jump statements
        for &kw in &["break", "continue", "goto", "return"] {
            self.register_keyword(Keyword::new(kw, KeywordCategory::ControlFlow).with_priority(100));
        }

        // Operators - basic
        for &kw in &["*", "&", "+", "-", "/", "%", "=", "==", "!=", "<", ">", "<=", ">=", "&&", "||", "!", "~", "^", "|", "<<", ">>"] {
            self.register_keyword(Keyword::new(kw, KeywordCategory::Operator).with_priority(50));
        }

        // Operators - compound assignment
        for &kw in &["+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>="] {
            self.register_keyword(Keyword::new(kw, KeywordCategory::Operator).with_priority(50));
        }

        // Operators - increment/decrement
        for &kw in &["++", "--"] {
            self.register_keyword(Keyword::new(kw, KeywordCategory::Operator).with_priority(50));
        }

        // Operators - member access and ternary
        for &kw in &["->", "?", "sizeof"] {
            self.register_keyword(Keyword::new(kw, KeywordCategory::Operator).with_priority(60));
        }

        // C99/C11 operators
        for &kw in &["_Alignof", "alignof", "_Generic", "_Static_assert", "static_assert"] {
            self.register_keyword(Keyword::new(kw, KeywordCategory::Operator).with_priority(60));
        }

        // Punctuation tokens
        for &kw in &["{", "}", "(", ")", "[", "]", ";", ":", ",", ".", "...", "##"] {
            self.register_keyword(Keyword::new(kw, KeywordCategory::Punctuation).with_priority(50));
        }

        // C99/C11 type keywords
        for &kw in &["_Bool", "_Complex", "_Imaginary"] {
            self.register_keyword(Keyword::new(kw, KeywordCategory::BasicType).with_priority(70));
        }

        // C11 type qualifiers
        for &kw in &["_Atomic", "__restrict", "__restrict__"] {
            self.register_keyword(Keyword::new(kw, KeywordCategory::TypeQualifier).with_priority(80));
        }

        // C11 storage class specifiers
        for &kw in &["_Thread_local", "thread_local", "_Noreturn", "noreturn"] {
            self.register_keyword(Keyword::new(kw, KeywordCategory::StorageClass).with_priority(90));
        }

        // Common fixed-width integer types (from stdint.h)
        for &kw in &[
            "size_t", "ptrdiff_t", "intptr_t", "uintptr_t",
            "int8_t", "int16_t", "int32_t", "int64_t",
            "uint8_t", "uint16_t", "uint32_t", "uint64_t",
            "intmax_t", "uintmax_t",
        ] {
            self.register_keyword(Keyword::new(kw, KeywordCategory::BasicType).with_priority(70));
        }

        // Additional preprocessor
        for &kw in &["defined", "elifdef", "elifndef", "__has_include", "__has_c_attribute", "_Pragma"] {
            self.register_keyword(Keyword::new(kw, KeywordCategory::Preprocessor).with_priority(190));
        }

        // Identifier placeholder (for ordering rules)
        self.register_keyword(Keyword::new("identifier", KeywordCategory::Identifier).with_priority(50));

        // Literal placeholder (for ordering rules)
        self.register_keyword(Keyword::new("literal", KeywordCategory::Literal).with_priority(50));
    }

    fn create_c_ordering_chain(&mut self) {
        // ====================================================================
        // typedef ordering rules
        // ====================================================================
        // typedef -> type keywords, qualifiers, storage classes
        for &to in &[
            "struct", "enum", "union", "int", "char", "float", "double", "void", "long", "short",
            "unsigned", "signed", "const", "volatile", "restrict",
        ] {
            self.link("typedef", to);
        }
        // typedef cannot follow itself or other type declarations
        self.unlink("typedef", "typedef");
        self.unlink("typedef", "struct");
        self.unlink("typedef", "enum");
        self.unlink("typedef", "union");

        // ====================================================================
        // struct/enum/union ordering rules
        // ====================================================================
        for &from in &["struct", "enum", "union"] {
            // Can be followed by identifier (named type) or { (anonymous)
            self.link(from, "identifier");
            self.link(from, "{");
            // Can be followed by * for pointer types
            self.link(from, "*");
            // Cannot follow themselves or typedef
            self.unlink(from, from);
            self.unlink(from, "typedef");
            // Cannot directly follow other type declarations
            for &other in &["struct", "enum", "union"] {
                if from != other {
                    self.unlink(from, other);
                }
            }
        }

        // ====================================================================
        // Storage class ordering rules
        // ====================================================================
        let storage_classes = ["static", "extern", "register", "auto", "inline", "__inline", "__inline__"];
        for &from in &storage_classes {
            // Storage classes -> types and qualifiers
            for &to in &[
                "int", "char", "float", "double", "void", "long", "short", "unsigned", "signed",
                "const", "volatile", "restrict", "struct", "enum", "union",
            ] {
                self.link(from, to);
            }
            // Storage classes can precede typedef
            self.link(from, "typedef");
            // Storage classes -> identifier (for custom types)
            self.link(from, "identifier");
            // Cannot follow typedef
            self.unlink(from, "typedef");
            // Storage classes generally don't follow each other (except inline can follow others)
            for &other in &["static", "extern", "register", "auto"] {
                if from != other {
                    self.unlink(from, other);
                }
            }
        }
        // inline can follow static/extern
        self.link("static", "inline");
        self.link("static", "__inline");
        self.link("static", "__inline__");
        self.link("extern", "inline");
        self.link("extern", "__inline");
        self.link("extern", "__inline__");

        // ====================================================================
        // Type qualifier ordering rules
        // ====================================================================
        let type_qualifiers = ["const", "volatile", "restrict"];
        for &from in &type_qualifiers {
            // Qualifiers -> types
            for &to in &[
                "int", "char", "float", "double", "void", "long", "short", "unsigned", "signed",
                "struct", "enum", "union",
            ] {
                self.link(from, to);
            }
            // Qualifiers can follow each other
            for &other in &type_qualifiers {
                if from != other {
                    self.link(from, other);
                }
            }
            // Qualifiers -> identifier (for custom types), *, (
            self.link(from, "identifier");
            self.link(from, "*");
            self.link(from, "(");
            // Qualifiers can follow storage classes (handled above)
            // Qualifiers can follow types (for pointer qualifiers)
            for &type_kw in &["int", "char", "float", "double", "void", "long", "short"] {
                self.link(type_kw, from);
            }
        }

        // ====================================================================
        // Sign modifier ordering rules (unsigned/signed)
        // ====================================================================
        for &from in &["unsigned", "signed"] {
            // Sign modifiers -> integer types
            for &to in &["int", "char", "long", "short"] {
                self.link(from, to);
            }
            // Sign modifiers -> identifier, *, (
            self.link(from, "identifier");
            self.link(from, "*");
            self.link(from, "(");
            // Sign modifiers can follow qualifiers
            for &qual in &type_qualifiers {
                self.link(qual, from);
            }
            // Sign modifiers can follow storage classes (handled above)
            // Sign modifiers cannot follow each other
            self.unlink("unsigned", "signed");
            self.unlink("signed", "unsigned");
        }

        // ====================================================================
        // Size modifier ordering rules (long/short)
        // ====================================================================
        for &from in &["long", "short"] {
            // Size modifiers -> int, double (long double), identifier
            self.link(from, "int");
            self.link(from, "identifier");
            self.link(from, "*");
            self.link(from, "(");
            // long -> long (for long long)
            self.link("long", "long");
            // long -> double (for long double)
            self.link("long", "double");
            // Size modifiers can follow sign modifiers
            self.link("unsigned", from);
            self.link("signed", from);
            // short cannot follow long and vice versa
            self.unlink("long", "short");
            self.unlink("short", "long");
        }

        // ====================================================================
        // Basic type ordering rules
        // ====================================================================
        for &from in &["void", "int", "char", "float", "double"] {
            // Basic types -> identifier, *, (, )
            self.link(from, "identifier");
            self.link(from, "*");
            self.link(from, "(");
            self.link(from, ")");
            // Basic types -> , (in parameter lists)
            self.link(from, ",");
            // Basic types cannot follow each other (except through modifiers)
            for &other in &["void", "int", "char", "float", "double"] {
                if from != other {
                    self.unlink(from, other);
                }
            }
        }
        // Special: long can follow int (but we use long int, not int long)
        self.unlink("int", "long");
        self.unlink("int", "short");

        // ====================================================================
        // Preprocessor directive ordering rules
        // ====================================================================
        // # -> preprocessor directives
        for &to in &[
            "define", "include", "ifdef", "ifndef", "endif", "if", "else", "elif",
            "pragma", "warning", "error", "undef", "line",
        ] {
            self.link("#", to);
        }
        // # cannot be followed by type keywords or storage classes
        for &to in &[
            "struct", "enum", "union", "typedef", "int", "char", "float", "double",
            "void", "long", "short", "unsigned", "signed", "static", "extern",
            "const", "volatile", "restrict",
        ] {
            self.unlink("#", to);
        }
        // Preprocessor directives -> identifier, <, ", (
        for &from in &["define", "include", "ifdef", "ifndef", "undef", "pragma", "if", "elif"] {
            self.link(from, "identifier");
            self.link(from, "(");
        }
        self.link("include", "<");
        // endif, else (preprocessor) are terminal or followed by newline

        // ====================================================================
        // Control flow ordering rules
        // ====================================================================
        // Conditional statements with parentheses -> (
        for &from in &["if", "while", "for", "switch"] {
            self.link(from, "(");
            // Cannot be directly followed by identifier, ;, { without (
            self.unlink(from, "identifier");
            self.unlink(from, ";");
            self.unlink(from, "{");
        }

        // do -> { (do-while loop)
        self.link("do", "{");
        self.unlink("do", "(");
        self.unlink("do", "identifier");

        // else -> if (else if), { (else block), statement
        self.link("else", "if");
        self.link("else", "{");
        self.link("else", "return");
        self.link("else", "break");
        self.link("else", "continue");
        self.link("else", "goto");
        self.link("else", "identifier");

        // case -> identifier, literal values, :
        self.link("case", "identifier");
        self.link("case", "literal");
        self.link("case", ":");
        // case cannot be followed by {
        self.unlink("case", "{");

        // default -> :
        self.link("default", ":");
        self.unlink("default", "identifier");
        self.unlink("default", "{");

        // Jump statements
        // break/continue -> ;
        for &from in &["break", "continue"] {
            self.link(from, ";");
            // Cannot be followed by identifier or expression
            self.unlink(from, "identifier");
            self.unlink(from, "(");
        }

        // goto -> identifier (label), ;
        self.link("goto", "identifier");
        self.link("goto", ";");
        self.unlink("goto", "(");
        self.unlink("goto", "{");

        // return -> expression or ;
        for &to in &["identifier", "(", ";", "*", "&", "!", "-", "+", "~"] {
            self.link("return", to);
        }
        self.link("return", "literal");
        self.unlink("return", "{");

        // ====================================================================
        // Punctuation ordering rules
        // ====================================================================
        // { -> types, control flow, identifiers, }, other statements
        for &to in &[
            "int", "char", "float", "double", "void", "long", "short", "unsigned", "signed",
            "const", "volatile", "struct", "enum", "union", "typedef",
            "static", "extern", "register", "auto", "inline",
            "if", "while", "for", "switch", "do", "case", "default",
            "return", "break", "continue", "goto",
            "identifier", "}", "*", "&", "(", ";",
        ] {
            self.link("{", to);
        }

        // } -> else, while (do-while), ;, }, identifier, control flow
        for &to in &[
            "else", "while", ";", "}", "identifier", "if", "for", "switch",
            "return", "break", "continue", "case", "default",
        ] {
            self.link("}", to);
        }

        // ( -> types, identifiers, operators, expressions, )
        for &to in &[
            "int", "char", "float", "double", "void", "long", "short", "unsigned", "signed",
            "const", "volatile", "struct", "enum", "union",
            "identifier", "*", "&", "(", ")", "!", "-", "+", "~",
        ] {
            self.link("(", to);
        }
        self.link("(", "literal");

        // ) -> {, ;, ,, ), ], operators, identifier
        for &to in &["{", ";", ",", ")", "]", "identifier", "*", "&", "+", "-", "/", "%", "=", ".", "->"] {
            self.link(")", to);
        }

        // [ -> identifier, literal, expression, ]
        for &to in &["identifier", "]", "*", "&", "(", "+", "-"] {
            self.link("[", to);
        }
        self.link("[", "literal");

        // ] -> [, ), ;, ,, =, operators
        for &to in &["[", ")", ";", ",", "=", "+", "-", "*", "/", ".", "->", "identifier"] {
            self.link("]", to);
        }

        // ; -> types, control flow, identifiers, }
        for &to in &[
            "int", "char", "float", "double", "void", "long", "short", "unsigned", "signed",
            "const", "volatile", "struct", "enum", "union", "typedef",
            "static", "extern", "register", "auto", "inline",
            "if", "while", "for", "switch", "do", "case", "default",
            "return", "break", "continue", "goto",
            "identifier", "}", "*", "&",
        ] {
            self.link(";", to);
        }

        // : -> statement (after case/default/label)
        for &to in &[
            "identifier", "return", "break", "continue", "goto", "if", "while", "for",
            "{", "(", "*", "&",
        ] {
            self.link(":", to);
        }

        // , -> types, identifiers, expressions
        for &to in &[
            "int", "char", "float", "double", "void", "long", "short", "unsigned", "signed",
            "const", "volatile", "struct", "enum", "union",
            "identifier", "*", "&", "(", "!", "-", "+", "~",
        ] {
            self.link(",", to);
        }
        self.link(",", "literal");

        // . and -> -> identifier
        self.link(".", "identifier");
        self.link("->", "identifier");

        // ====================================================================
        // Operator ordering rules
        // ====================================================================
        // Binary operators -> identifier, (, literal, unary operators
        let binary_ops = ["+", "-", "*", "/", "%", "=", "==", "!=", "<", ">", "<=", ">=", "&&", "||", "^", "|", "&", "<<", ">>"];
        for &from in &binary_ops {
            for &to in &["identifier", "(", "!", "-", "+", "~", "*", "&"] {
                self.link(from, to);
            }
            self.link(from, "literal");
        }

        // Unary operators -> identifier, (, literal
        let unary_ops = ["!", "~"];
        for &from in &unary_ops {
            for &to in &["identifier", "(", "!", "~", "-", "+", "*", "&"] {
                self.link(from, to);
            }
            self.link(from, "literal");
        }

        // * (dereference or pointer) -> identifier, (, *, const, volatile
        self.link("*", "identifier");
        self.link("*", "(");
        self.link("*", "*");
        self.link("*", "const");
        self.link("*", "volatile");
        self.link("*", "restrict");

        // & (address-of or reference) -> identifier, (
        self.link("&", "identifier");
        self.link("&", "(");
        self.link("&", "*");

        // ====================================================================
        // Identifier ordering rules
        // ====================================================================
        // identifier -> operators, punctuation, type qualifiers (for casts)
        for &to in &[
            "(", ")", "[", "]", ";", ",", ".", "->", ":",
            "+", "-", "*", "/", "%", "=", "==", "!=", "<", ">", "<=", ">=",
            "&&", "||", "^", "|", "&", "<<", ">>", "!", "~",
            "{", "}",
        ] {
            self.link("identifier", to);
        }
        // identifier -> identifier (for multi-word types or expressions)
        self.link("identifier", "identifier");

        // ====================================================================
        // Literal ordering rules
        // ====================================================================
        // literal -> operators, punctuation
        for &to in &[
            ")", "]", ";", ",",
            "+", "-", "*", "/", "%", "==", "!=", "<", ">", "<=", ">=",
            "&&", "||", "^", "|", "&", "<<", ">>",
        ] {
            self.link("literal", to);
        }

        // ====================================================================
        // Ternary operator rules
        // ====================================================================
        // ? -> identifier, literal, (
        self.link("?", "identifier");
        self.link("?", "literal");
        self.link("?", "(");
        self.link("?", "!");
        self.link("?", "-");
        self.link("?", "+");

        // : (in ternary) -> identifier, literal, (
        self.link(":", "identifier");
        self.link(":", "literal");
        self.link(":", "(");
        self.link(":", "!");
        self.link(":", "-");
        self.link(":", "+");

        // ====================================================================
        // Sizeof operator rules
        // ====================================================================
        self.link("sizeof", "(");
        self.link("sizeof", "identifier");
        for &basic_type in &["void", "int", "char", "float", "double", "long", "short", "unsigned", "signed"] {
            self.link("sizeof", basic_type);
        }
        // sizeof -> C99/C11 types
        for &c99_type in &["_Bool", "_Complex", "_Imaginary"] {
            self.link("sizeof", c99_type);
        }
        // sizeof -> fixed-width types
        for &fixed_type in &[
            "size_t", "ptrdiff_t", "intptr_t", "uintptr_t",
            "int8_t", "int16_t", "int32_t", "int64_t",
            "uint8_t", "uint16_t", "uint32_t", "uint64_t",
        ] {
            self.link("sizeof", fixed_type);
        }
        // sizeof -> struct/enum/union
        for &type_decl in &["struct", "enum", "union"] {
            self.link("sizeof", type_decl);
        }

        // ====================================================================
        // _Alignof/alignof operator rules (C11)
        // ====================================================================
        for &alignof_kw in &["_Alignof", "alignof"] {
            self.link(alignof_kw, "(");
            for &basic_type in &["void", "int", "char", "float", "double", "long", "short", "unsigned", "signed"] {
                self.link(alignof_kw, basic_type);
            }
            for &type_decl in &["struct", "enum", "union"] {
                self.link(alignof_kw, type_decl);
            }
            self.link(alignof_kw, "identifier");
        }

        // ====================================================================
        // _Generic expression rules (C11)
        // ====================================================================
        self.link("_Generic", "(");

        // ====================================================================
        // _Static_assert/static_assert rules (C11)
        // ====================================================================
        for &assert_kw in &["_Static_assert", "static_assert"] {
            self.link(assert_kw, "(");
        }

        // ====================================================================
        // C99/C11 type ordering rules
        // ====================================================================
        // _Bool, _Complex, _Imaginary follow same rules as basic types
        for &c99_type in &["_Bool", "_Complex", "_Imaginary"] {
            self.link(c99_type, "identifier");
            self.link(c99_type, "*");
            self.link(c99_type, "(");
            self.link(c99_type, ")");
            self.link(c99_type, ",");
            // Can follow qualifiers
            for &qual in &["const", "volatile", "restrict", "_Atomic"] {
                self.link(qual, c99_type);
            }
            // Can follow storage classes
            for &storage in &["static", "extern", "register", "auto", "inline"] {
                self.link(storage, c99_type);
            }
        }

        // ====================================================================
        // Fixed-width type ordering rules
        // ====================================================================
        let fixed_types = [
            "size_t", "ptrdiff_t", "intptr_t", "uintptr_t",
            "int8_t", "int16_t", "int32_t", "int64_t",
            "uint8_t", "uint16_t", "uint32_t", "uint64_t",
            "intmax_t", "uintmax_t",
        ];
        for &fixed_type in &fixed_types {
            self.link(fixed_type, "identifier");
            self.link(fixed_type, "*");
            self.link(fixed_type, "(");
            self.link(fixed_type, ")");
            self.link(fixed_type, ",");
            // Can follow qualifiers
            for &qual in &["const", "volatile", "restrict", "_Atomic"] {
                self.link(qual, fixed_type);
            }
            // Can follow storage classes
            for &storage in &["static", "extern", "register", "auto", "inline", "typedef"] {
                self.link(storage, fixed_type);
            }
        }

        // ====================================================================
        // C11 qualifier ordering rules (_Atomic, __restrict)
        // ====================================================================
        for &c11_qual in &["_Atomic", "__restrict", "__restrict__"] {
            // Qualifiers -> types
            for &to in &[
                "int", "char", "float", "double", "void", "long", "short", "unsigned", "signed",
                "struct", "enum", "union", "_Bool", "_Complex",
            ] {
                self.link(c11_qual, to);
            }
            // Qualifiers can follow each other
            for &other in &["const", "volatile", "restrict", "_Atomic", "__restrict", "__restrict__"] {
                if c11_qual != other {
                    self.link(c11_qual, other);
                    self.link(other, c11_qual);
                }
            }
            self.link(c11_qual, "identifier");
            self.link(c11_qual, "*");
            self.link(c11_qual, "(");
        }

        // ====================================================================
        // C11 storage class ordering rules (_Thread_local, _Noreturn)
        // ====================================================================
        for &c11_storage in &["_Thread_local", "thread_local"] {
            // Thread local -> types
            for &to in &[
                "int", "char", "float", "double", "void", "long", "short", "unsigned", "signed",
                "const", "volatile", "struct", "enum", "union",
            ] {
                self.link(c11_storage, to);
            }
            self.link(c11_storage, "identifier");
            // Can combine with static/extern
            self.link("static", c11_storage);
            self.link("extern", c11_storage);
            self.link(c11_storage, "static");
            self.link(c11_storage, "extern");
        }

        for &noreturn in &["_Noreturn", "noreturn"] {
            // _Noreturn typically precedes function declarations
            self.link(noreturn, "void");
            self.link(noreturn, "int");
            self.link(noreturn, "identifier");
            // Can follow static/extern
            self.link("static", noreturn);
            self.link("extern", noreturn);
        }

        // ====================================================================
        // Variadic ellipsis rules
        // ====================================================================
        // ... appears after last named parameter
        self.link(",", "...");
        self.link("...", ")");
        // Cannot be followed by anything except )
        self.unlink("...", "identifier");
        self.unlink("...", ",");

        // ====================================================================
        // Member access operator rules
        // ====================================================================
        // -> (arrow) already linked to identifier, add more
        self.link("->", "(");  // method-like call: ptr->func()
        self.link("identifier", "->");
        self.link(")", "->");
        self.link("]", "->");

        // ====================================================================
        // Typedef rules
        // ====================================================================
        // typedef -> storage classes, type qualifiers, basic types, struct/enum/union, identifier
        for &to in &["const", "volatile", "restrict"] {
            self.link("typedef", to);
        }
        for &to in &["void", "int", "char", "float", "double", "long", "short", "unsigned", "signed"] {
            self.link("typedef", to);
        }
        for &to in &["struct", "enum", "union"] {
            self.link("typedef", to);
        }
        self.link("typedef", "identifier");

        // ====================================================================
        // Cast expression rules
        // ====================================================================
        // After closing paren of cast -> identifier, literal, (, unary ops
        // This is implicit through ( -> type and ) -> identifier/literal rules

        // ====================================================================
        // Assignment operator rules
        // ====================================================================
        let assignment_ops = ["=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>="];
        for &from in &assignment_ops {
            for &to in &["identifier", "(", "!", "-", "+", "~", "*", "&"] {
                self.link(from, to);
            }
            self.link(from, "literal");
        }

        // ====================================================================
        // Increment/Decrement operators
        // ====================================================================
        // ++ and -- can be prefix or postfix
        self.link("++", "identifier");
        self.link("++", "(");
        self.link("--", "identifier");
        self.link("--", "(");
        // identifier -> ++ or -- (postfix)
        self.link("identifier", "++");
        self.link("identifier", "--");
        // After postfix ++ or -- -> operators, punctuation
        for &to in &[")", "]", ";", ",", "+", "-", "*", "/", "%", "==", "!=", "<", ">", "<=", ">=", "&&", "||"] {
            self.link("++", to);
            self.link("--", to);
        }
    }

    fn populate_c_node_transitions(&mut self) {
        // Function can contain
        self.add_node_transition(
            "function",
            vec![
                "parameter",
                "return_type",
                "function_body",
                "variable_declaration",
                "assignment",
                "expression",
                "if_statement",
                "while_loop",
                "for_loop",
                "do_while",
                "switch",
                "return",
                "break",
                "continue",
                "function_call",
                "comment",
                "variable",
                "array",
                "type",
                "loop",
            ],
        );

        // Struct contains fields
        self.add_node_transition("struct", vec!["field", "comment", "type", "nested_struct", "nested_union"]);

        // Union contains fields (similar to struct)
        self.add_node_transition("union", vec!["field", "comment", "type", "nested_struct", "nested_union"]);

        // Enum contains variants
        self.add_node_transition("enum", vec!["variant", "enum_variant", "comment"]);

        // Typedef can contain
        self.add_node_transition("typedef", vec!["type", "struct", "enum", "union", "comment"]);

        // Type alias
        self.add_node_transition("type_alias", vec!["type", "comment"]);

        // Global/Variable
        self.add_node_transition("global", vec!["expression", "assignment", "comment"]);
        self.add_node_transition("variable", vec!["expression", "assignment", "type"]);

        // Array
        self.add_node_transition("array", vec!["expression", "variable", "type"]);

        // Macro
        self.add_node_transition("macro", vec!["expression", "comment"]);

        // Include
        self.add_node_transition("include", vec!["comment"]);

        // Function body
        self.add_node_transition(
            "function_body",
            vec![
                "variable_declaration",
                "assignment",
                "expression",
                "if_statement",
                "while_loop",
                "for_loop",
                "do_while",
                "switch",
                "case",
                "return",
                "break",
                "continue",
                "function_call",
                "comment",
                "variable",
                "array",
                "loop",
            ],
        );

        // Parameter
        self.add_node_transition("parameter", vec!["type", "expression"]);

        // Return type
        self.add_node_transition("return_type", vec!["type"]);

        // Field (struct member)
        self.add_node_transition("field", vec!["type", "expression", "comment"]);

        // Variant (enum member)
        self.add_node_transition("variant", vec!["expression", "comment"]);
        self.add_node_transition("enum_variant", vec!["expression", "comment"]);

        // Control flow statements
        let control_children = vec![
            "expression",
            "function_body",
            "variable_declaration",
            "assignment",
            "function_call",
            "if_statement",
            "while_loop",
            "for_loop",
            "do_while",
            "return",
            "break",
            "continue",
            "comment",
            "variable",
            "array",
            "loop",
        ];
        for kind in &["if_statement", "while_loop", "for_loop", "do_while", "loop"] {
            self.add_node_transition(*kind, control_children.clone());
        }

        // Switch
        let mut switch_children = control_children.clone();
        switch_children.push("case");
        switch_children.push("default");
        self.add_node_transition("switch", switch_children);

        // Case
        self.add_node_transition(
            "case",
            vec![
                "expression",
                "variable_declaration",
                "assignment",
                "function_call",
                "if_statement",
                "while_loop",
                "for_loop",
                "return",
                "break",
                "continue",
                "comment",
                "case",  // fallthrough to next case
            ],
        );

        // Default (similar to case)
        self.add_node_transition(
            "default",
            vec![
                "expression",
                "variable_declaration",
                "assignment",
                "function_call",
                "if_statement",
                "while_loop",
                "for_loop",
                "return",
                "break",
                "continue",
                "comment",
            ],
        );

        // Else branch
        self.add_node_transition(
            "else",
            vec![
                "expression",
                "variable_declaration",
                "assignment",
                "function_call",
                "if_statement",  // else if
                "while_loop",
                "for_loop",
                "return",
                "break",
                "continue",
                "comment",
                "variable",
                "array",
            ],
        );

        // Goto statement
        self.add_node_transition("goto", vec!["label", "comment"]);

        // Label (target for goto)
        self.add_node_transition(
            "label",
            vec![
                "expression",
                "variable_declaration",
                "assignment",
                "function_call",
                "if_statement",
                "while_loop",
                "for_loop",
                "return",
                "break",
                "continue",
                "goto",
                "comment",
            ],
        );

        // Expression
        self.add_node_transition(
            "expression",
            vec!["function_call", "expression", "variable", "type", "comment"],
        );

        // Variable declaration
        self.add_node_transition(
            "variable_declaration",
            vec!["assignment", "expression", "type", "comment"],
        );

        // Assignment
        self.add_node_transition(
            "assignment",
            vec!["expression", "function_call", "variable", "comment"],
        );

        // Return statement
        self.add_node_transition("return", vec!["expression", "function_call", "variable"]);

        // Break/Continue
        self.add_node_transition("break", vec!["comment"]);
        self.add_node_transition("continue", vec!["comment"]);

        // Function call
        self.add_node_transition(
            "function_call",
            vec!["expression", "variable", "function_call", "comment"],
        );

        // Type
        self.add_node_transition(
            "type",
            vec!["type", "variable", "function", "comment", "field"],
        );

        // Comment can be anywhere (no children typically)
        self.add_node_transition("comment", Vec::<String>::new());
    }
}

impl Order {
    // ========================================================================
    // Rust Language Rules Population
    // ========================================================================

    /// Create populated with Rust language ordering rules
    pub fn with_rust_rules() -> Self {
        let mut order = Self::new();
        order.language = "rust".to_string();
        order.populate_rust_keyword_rules();
        order.populate_rust_node_transitions();
        order
    }

    fn populate_rust_keyword_rules(&mut self) {
        self.register_all_rust_keywords();
        self.create_rust_ordering_chain();
    }

    fn register_all_rust_keywords(&mut self) {
        // Visibility modifiers (appear first in declarations)
        for &kw in &["pub", "pub(crate)", "pub(super)", "pub(self)"] {
            self.register_keyword(Keyword::new(kw, KeywordCategory::StorageClass).with_priority(100));
        }

        // Type declarations
        for &kw in &["struct", "enum", "union", "trait", "type", "impl"] {
            self.register_keyword(Keyword::new(kw, KeywordCategory::TypeDeclaration).with_priority(95));
        }

        // Storage/mutability
        for &kw in &["static", "const", "mut", "ref", "move", "box"] {
            self.register_keyword(Keyword::new(kw, KeywordCategory::StorageClass).with_priority(90));
        }

        // Type qualifiers
        for &kw in &["dyn", "impl"] {
            self.register_keyword(Keyword::new(kw, KeywordCategory::TypeQualifier).with_priority(85));
        }

        // Function-related
        for &kw in &["fn", "async", "await", "unsafe", "where"] {
            self.register_keyword(Keyword::new(kw, KeywordCategory::TypeDeclaration).with_priority(80));
        }

        // Module/use
        for &kw in &["mod", "use", "crate", "super", "self", "as", "extern"] {
            self.register_keyword(Keyword::new(kw, KeywordCategory::StorageClass).with_priority(75));
        }

        // Control flow
        for &kw in &["if", "else", "match", "loop", "while", "for", "break", "continue", "return", "yield"] {
            self.register_keyword(Keyword::new(kw, KeywordCategory::ControlFlow).with_priority(70));
        }

        // Boolean literals
        for &kw in &["true", "false"] {
            self.register_keyword(Keyword::new(kw, KeywordCategory::Literal).with_priority(50));
        }

        // Primitive types
        for &kw in &[
            "bool", "char", "str",
            "i8", "i16", "i32", "i64", "i128", "isize",
            "u8", "u16", "u32", "u64", "u128", "usize",
            "f32", "f64",
        ] {
            self.register_keyword(Keyword::new(kw, KeywordCategory::BasicType).with_priority(60));
        }

        // Other keywords
        for &kw in &["let", "in", "Self"] {
            self.register_keyword(Keyword::new(kw, KeywordCategory::StorageClass).with_priority(65));
        }

        // Reserved keywords
        for &kw in &["abstract", "become", "do", "final", "macro", "override", "priv", "try", "typeof", "unsized", "virtual"] {
            self.register_keyword(Keyword::new(kw, KeywordCategory::StorageClass).with_priority(40));
        }

        // Operators
        for &kw in &["+", "-", "*", "/", "%", "=", "==", "!=", "<", ">", "<=", ">=", "&&", "||", "!", "&", "|", "^", "~", "<<", ">>"] {
            self.register_keyword(Keyword::new(kw, KeywordCategory::Operator).with_priority(50));
        }

        // Compound assignment
        for &kw in &["+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>="] {
            self.register_keyword(Keyword::new(kw, KeywordCategory::Operator).with_priority(50));
        }

        // Special operators
        for &kw in &["=>", "->", "::", "..", "..=", "?", "@", "#"] {
            self.register_keyword(Keyword::new(kw, KeywordCategory::Operator).with_priority(55));
        }

        // Punctuation
        for &kw in &["{", "}", "(", ")", "[", "]", ";", ":", ",", ".", "'"] {
            self.register_keyword(Keyword::new(kw, KeywordCategory::Punctuation).with_priority(50));
        }

        // Identifier and literal placeholders
        self.register_keyword(Keyword::new("identifier", KeywordCategory::Identifier).with_priority(50));
        self.register_keyword(Keyword::new("literal", KeywordCategory::Literal).with_priority(50));
    }

    fn create_rust_ordering_chain(&mut self) {
        // ====================================================================
        // Visibility rules
        // ====================================================================
        // pub -> fn, struct, enum, trait, type, mod, use, const, static
        for &to in &["fn", "struct", "enum", "trait", "type", "mod", "use", "const", "static", "unsafe", "async", "extern"] {
            self.link("pub", to);
        }
        for &vis in &["pub(crate)", "pub(super)", "pub(self)"] {
            for &to in &["fn", "struct", "enum", "trait", "type", "mod", "use", "const", "static", "unsafe", "async"] {
                self.link(vis, to);
            }
        }

        // ====================================================================
        // Function declaration rules
        // ====================================================================
        // async -> fn, unsafe
        self.link("async", "fn");
        self.link("async", "unsafe");
        // unsafe -> fn, impl, trait
        self.link("unsafe", "fn");
        self.link("unsafe", "impl");
        self.link("unsafe", "trait");
        // const -> fn
        self.link("const", "fn");
        // extern -> fn, crate, "C"
        self.link("extern", "fn");
        self.link("extern", "crate");
        self.link("extern", "literal");

        // fn -> identifier
        self.link("fn", "identifier");
        // identifier -> ( (function parameters)
        self.link("identifier", "(");
        // ) -> -> (return type)
        self.link(")", "->");
        // -> -> type
        for &ty in &["bool", "char", "str", "i8", "i16", "i32", "i64", "i128", "isize", "u8", "u16", "u32", "u64", "u128", "usize", "f32", "f64", "Self", "()", "identifier"] {
            self.link("->", ty);
        }
        // ) -> where
        self.link(")", "where");
        // ) -> {
        self.link(")", "{");

        // ====================================================================
        // Let binding rules
        // ====================================================================
        // let -> mut, identifier, pattern
        self.link("let", "mut");
        self.link("let", "identifier");
        self.link("let", "(");
        // mut -> identifier
        self.link("mut", "identifier");
        // identifier -> : (type annotation)
        self.link("identifier", ":");
        // : -> type
        for &ty in &["bool", "char", "str", "i8", "i16", "i32", "i64", "i128", "isize", "u8", "u16", "u32", "u64", "u128", "usize", "f32", "f64", "&", "*", "[", "(", "identifier", "Self", "dyn", "impl"] {
            self.link(":", ty);
        }
        // identifier -> = (initialization)
        self.link("identifier", "=");

        // ====================================================================
        // Type declaration rules
        // ====================================================================
        // struct/enum/union/trait -> identifier
        for &decl in &["struct", "enum", "union", "trait"] {
            self.link(decl, "identifier");
        }
        // identifier -> < (generics)
        self.link("identifier", "<");
        // > -> { or where
        self.link(">", "{");
        self.link(">", "where");
        self.link(">", ";");
        // identifier -> {, ;, where
        self.link("identifier", "{");
        self.link("identifier", ";");
        self.link("identifier", "where");

        // impl -> identifier, < (generics), for
        self.link("impl", "identifier");
        self.link("impl", "<");
        self.link("impl", "dyn");
        // impl Trait for Type
        self.link("identifier", "for");
        self.link("for", "identifier");

        // type -> identifier = (type alias)
        self.link("type", "identifier");

        // ====================================================================
        // Module/use rules
        // ====================================================================
        // mod -> identifier
        self.link("mod", "identifier");
        // use -> identifier, crate, super, self, ::
        self.link("use", "identifier");
        self.link("use", "crate");
        self.link("use", "super");
        self.link("use", "self");
        self.link("use", "::");
        // :: -> identifier, {, *
        self.link("::", "identifier");
        self.link("::", "{");
        self.link("::", "*");
        // identifier -> ::
        self.link("identifier", "::");
        // as -> identifier
        self.link("as", "identifier");
        self.link("identifier", "as");

        // ====================================================================
        // Control flow rules
        // ====================================================================
        // if/while -> expression
        for &kw in &["if", "while"] {
            self.link(kw, "identifier");
            self.link(kw, "literal");
            self.link(kw, "(");
            self.link(kw, "!");
            self.link(kw, "let");
        }
        // match -> expression
        self.link("match", "identifier");
        self.link("match", "literal");
        self.link("match", "(");
        self.link("match", "&");
        // for -> identifier/pattern in
        self.link("for", "identifier");
        self.link("for", "(");
        self.link("for", "_");
        self.link("identifier", "in");
        self.link("in", "identifier");
        self.link("in", "literal");
        // loop -> {
        self.link("loop", "{");
        // else -> { or if
        self.link("else", "{");
        self.link("else", "if");
        // return/break/continue -> expression or ;
        for &kw in &["return", "break", "continue"] {
            self.link(kw, "identifier");
            self.link(kw, "literal");
            self.link(kw, "(");
            self.link(kw, ";");
        }

        // ====================================================================
        // Match arm rules
        // ====================================================================
        // pattern => expression
        self.link("identifier", "=>");
        self.link("literal", "=>");
        self.link(")", "=>");
        self.link("}", "=>");
        self.link("_", "=>");
        self.link("=>", "identifier");
        self.link("=>", "literal");
        self.link("=>", "{");
        self.link("=>", "(");

        // ====================================================================
        // Reference and pointer rules
        // ====================================================================
        // & -> mut, identifier, literal
        self.link("&", "mut");
        self.link("&", "identifier");
        self.link("&", "literal");
        self.link("&", "(");
        self.link("&", "[");
        // * -> mut, const, identifier (raw pointers)
        self.link("*", "mut");
        self.link("*", "const");
        self.link("*", "identifier");

        // ====================================================================
        // Generic and lifetime rules
        // ====================================================================
        // < -> type, lifetime
        self.link("<", "identifier");
        self.link("<", "'");
        for &ty in &["bool", "char", "str", "i8", "i16", "i32", "i64", "i128", "isize", "u8", "u16", "u32", "u64", "u128", "usize", "f32", "f64"] {
            self.link("<", ty);
        }
        // type -> , or >
        self.link("identifier", ",");
        self.link("identifier", ">");
        // ' -> identifier (lifetime)
        self.link("'", "identifier");

        // ====================================================================
        // Array/slice rules
        // ====================================================================
        // [ -> type, expression, ]
        self.link("[", "identifier");
        self.link("[", "literal");
        for &ty in &["bool", "char", "str", "i8", "i16", "i32", "i64", "i128", "isize", "u8", "u16", "u32", "u64", "u128", "usize", "f32", "f64"] {
            self.link("[", ty);
        }
        // ; -> expression (array size)
        // ] -> ; or expression
        self.link("]", ";");
        self.link("]", ".");
        self.link("]", ")");

        // ====================================================================
        // Range operator rules
        // ====================================================================
        self.link("identifier", "..");
        self.link("literal", "..");
        self.link("..", "identifier");
        self.link("..", "literal");
        self.link("identifier", "..=");
        self.link("literal", "..=");
        self.link("..=", "identifier");
        self.link("..=", "literal");

        // ====================================================================
        // Method call rules
        // ====================================================================
        self.link("identifier", ".");
        self.link(")", ".");
        self.link("]", ".");
        self.link(".", "identifier");
        self.link(".", "await");

        // ====================================================================
        // Closure rules
        // ====================================================================
        self.link("|", "identifier");
        self.link("|", "mut");
        self.link("|", "&");
        self.link("|", ")"); // empty closure
        self.link("identifier", "|");
        self.link(":", "|"); // end of params

        // ====================================================================
        // Try operator rules
        // ====================================================================
        self.link("identifier", "?");
        self.link(")", "?");
        self.link("?", ".");
        self.link("?", ";");

        // ====================================================================
        // Arithmetic operators
        // ====================================================================
        for &op in &["+", "-", "*", "/", "%"] {
            self.link("identifier", op);
            self.link("literal", op);
            self.link(")", op);
            self.link("]", op);
            self.link(op, "identifier");
            self.link(op, "literal");
            self.link(op, "(");
        }

        // ====================================================================
        // Comparison operators
        // ====================================================================
        for &op in &["==", "!=", "<=", ">="] {
            self.link("identifier", op);
            self.link("literal", op);
            self.link(")", op);
            self.link(op, "identifier");
            self.link(op, "literal");
            self.link(op, "(");
        }

        // ====================================================================
        // Logical operators
        // ====================================================================
        for &op in &["&&", "||"] {
            self.link("identifier", op);
            self.link("literal", op);
            self.link(")", op);
            self.link(op, "identifier");
            self.link(op, "literal");
            self.link(op, "(");
            self.link(op, "!");
        }

        // ====================================================================
        // Bitwise operators
        // ====================================================================
        for &op in &["<<", ">>", "^"] {
            self.link("identifier", op);
            self.link("literal", op);
            self.link(")", op);
            self.link(op, "identifier");
            self.link(op, "literal");
            self.link(op, "(");
        }
        // ~ is unary only
        self.link("~", "identifier");
        self.link("~", "literal");
        self.link("~", "(");

        // ====================================================================
        // Compound assignment operators
        // ====================================================================
        for &op in &["+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>="] {
            self.link("identifier", op);
            self.link("]", op);
            self.link(op, "identifier");
            self.link(op, "literal");
            self.link(op, "(");
        }

        // ====================================================================
        // Reference modifiers (ref, box, move)
        // ====================================================================
        self.link("ref", "identifier");
        self.link("ref", "mut");
        self.link("let", "ref");
        self.link("|", "ref");
        
        self.link("box", "identifier");
        self.link("box", "(");
        self.link("box", "literal");
        
        self.link("move", "|");
        self.link("move", "||");

        // ====================================================================
        // Literal values (true, false)
        // ====================================================================
        for &lit in &["true", "false"] {
            self.link("=", lit);
            self.link("=>", lit);
            self.link("(", lit);
            self.link(",", lit);
            self.link("return", lit);
            self.link("if", lit);
            self.link("while", lit);
            self.link(lit, ";");
            self.link(lit, ")");
            self.link(lit, ",");
            self.link(lit, "&&");
            self.link(lit, "||");
        }

        // ====================================================================
        // Yield (async/generators)
        // ====================================================================
        self.link("yield", "identifier");
        self.link("yield", "literal");
        self.link("yield", "(");
        self.link("yield", ";");

        // ====================================================================
        // Attribute marker (#)
        // ====================================================================
        self.link("#", "[");
        self.link("#", "!");

        // ====================================================================
        // Negation operator (!)
        // ====================================================================
        self.link("!", "identifier");
        self.link("!", "literal");
        self.link("!", "(");
        self.link("!", "true");
        self.link("!", "false");

        // ====================================================================
        // Reserved/Future keywords (for completeness)
        // These are reserved but not yet used - add placeholder relations
        // ====================================================================
        
        // macro - macro_rules! style
        self.link("macro", "identifier");
        self.link("macro", "!");
        
        // try - future try blocks
        self.link("try", "{");
        self.link("try", "identifier");
        
        // do - future do notation
        self.link("do", "{");
        self.link("do", "identifier");
        
        // abstract/final/override/virtual - OOP-style modifiers
        for &kw in &["abstract", "final", "override", "virtual"] {
            self.link(kw, "fn");
            self.link(kw, "struct");
            self.link(kw, "trait");
            self.link("pub", kw);
        }
        
        // become - tail call
        self.link("become", "identifier");
        self.link("become", "(");
        
        // priv - private visibility (deprecated)
        self.link("priv", "fn");
        self.link("priv", "struct");
        self.link("priv", "mod");
        
        // typeof - type introspection
        self.link("typeof", "(");
        self.link("typeof", "identifier");
        
        // unsized - unsized types
        self.link("unsized", "identifier");
        self.link("unsized", "type");
        
        // @ - pattern binding
        self.link("@", "identifier");
        self.link("identifier", "@");
        self.link("@", "_");
        
        // ====================================================================
        // Additional common patterns
        // ====================================================================
        
        // static/const -> identifier (for variable declarations)
        self.link("static", "identifier");
        self.link("static", "mut");
        self.link("const", "identifier");
        
        // union -> identifier (like struct)
        self.link("pub", "union");
        for &vis in &["pub(crate)", "pub(super)", "pub(self)"] {
            self.link(vis, "union");
        }
    }

    fn populate_rust_node_transitions(&mut self) {
        // Function can contain
        self.add_node_transition(
            "rust_function",
            vec![
                "parameter",
                "return_type",
                "function_body",
                "generic",
                "lifetime",
                "where_clause",
                "expression",
                "statement",
                "let_binding",
                "if_expression",
                "match_expression",
                "loop_expression",
                "while_expression",
                "for_expression",
                "return",
                "break",
                "continue",
                "function_call",
                "method_call",
                "comment",
            ],
        );

        // Struct/Enum/Union
        self.add_node_transition("rust_struct", vec!["field", "generic", "lifetime", "where_clause", "comment"]);
        self.add_node_transition("rust_enum", vec!["variant", "generic", "lifetime", "where_clause", "comment"]);
        self.add_node_transition("rust_union", vec!["field", "generic", "lifetime", "where_clause", "comment"]);

        // Trait
        self.add_node_transition("rust_trait", vec!["method", "type_alias", "const", "generic", "lifetime", "where_clause", "comment"]);

        // Impl block
        self.add_node_transition("rust_impl", vec!["method", "type_alias", "const", "generic", "lifetime", "where_clause", "comment"]);

        // Module
        self.add_node_transition("rust_mod", vec!["function", "struct", "enum", "trait", "impl", "mod", "use", "const", "static", "type_alias", "comment"]);

        // Match expression
        self.add_node_transition("rust_match", vec!["match_arm", "pattern", "expression", "comment"]);
        self.add_node_transition("match_arm", vec!["pattern", "guard", "expression", "comment"]);

        // Let binding
        self.add_node_transition("let_binding", vec!["pattern", "type", "expression", "comment"]);

        // Expression types
        self.add_node_transition("rust_expression", vec!["expression", "function_call", "method_call", "literal", "identifier", "operator", "comment"]);

        // Closure
        self.add_node_transition("rust_closure", vec!["parameter", "return_type", "expression", "comment"]);

        // Generic parameters
        self.add_node_transition("generic", vec!["type_param", "lifetime", "const_param", "bound", "comment"]);

        // Where clause
        self.add_node_transition("where_clause", vec!["predicate", "bound", "lifetime", "comment"]);
    }
}

// ============================================================================
// Convenience Functions
// ============================================================================

/// Create C keyword ordering rules
pub fn c_order() -> Order {
    Order::with_c_rules()
}

/// Create Rust keyword ordering rules
pub fn rust_order() -> Order {
    Order::with_rust_rules()
}

/// Check if a keyword is a C type keyword
pub fn is_c_type_keyword(keyword: &str) -> bool {
    matches!(
        keyword,
        "int"
            | "char"
            | "float"
            | "double"
            | "void"
            | "long"
            | "short"
            | "unsigned"
            | "signed"
            | "struct"
            | "enum"
            | "union"
            | "typedef"
            | "_Bool"
            | "_Complex"
            | "_Imaginary"
            | "size_t"
            | "ptrdiff_t"
            | "intptr_t"
            | "uintptr_t"
            | "int8_t"
            | "int16_t"
            | "int32_t"
            | "int64_t"
            | "uint8_t"
            | "uint16_t"
            | "uint32_t"
            | "uint64_t"
    )
}

/// Check if a keyword is a storage class
pub fn is_c_storage_class(keyword: &str) -> bool {
    matches!(
        keyword,
        "static"
            | "extern"
            | "register"
            | "auto"
            | "inline"
            | "__inline"
            | "__inline__"
            | "_Thread_local"
            | "thread_local"
            | "_Noreturn"
            | "noreturn"
    )
}

/// Check if a keyword is a type qualifier
pub fn is_c_type_qualifier(keyword: &str) -> bool {
    matches!(
        keyword,
        "const" | "volatile" | "restrict" | "_Atomic" | "__restrict" | "__restrict__"
    )
}

/// Check if a keyword is control flow
pub fn is_c_control_flow(keyword: &str) -> bool {
    matches!(
        keyword,
        "if" | "else"
            | "while"
            | "for"
            | "do"
            | "switch"
            | "case"
            | "default"
            | "break"
            | "continue"
            | "goto"
            | "return"
    )
}

/// Check if a keyword is a preprocessor directive
pub fn is_preprocessor(keyword: &str) -> bool {
    matches!(
        keyword,
        "#" | "define"
            | "include"
            | "ifdef"
            | "ifndef"
            | "endif"
            | "if"
            | "else"
            | "elif"
            | "elifdef"
            | "elifndef"
            | "pragma"
            | "warning"
            | "error"
            | "undef"
            | "line"
            | "defined"
            | "__has_include"
            | "__has_c_attribute"
            | "_Pragma"
    )
}

/// Check if a keyword is an operator
pub fn is_c_operator(keyword: &str) -> bool {
    matches!(
        keyword,
        "+" | "-"
            | "*"
            | "/"
            | "%"
            | "++"
            | "--"
            | "=="
            | "!="
            | "<"
            | ">"
            | "<="
            | ">="
            | "&&"
            | "||"
            | "!"
            | "&"
            | "|"
            | "^"
            | "~"
            | "<<"
            | ">>"
            | "="
            | "+="
            | "-="
            | "*="
            | "/="
            | "%="
            | "&="
            | "|="
            | "^="
            | "<<="
            | ">>="
            | "?"
            | ":"
            | ","
            | "."
            | "->"
            | "sizeof"
            | "_Alignof"
            | "alignof"
            | "_Generic"
            | "_Static_assert"
            | "static_assert"
    )
}

/// Check if a keyword is punctuation
pub fn is_c_punctuation(keyword: &str) -> bool {
    matches!(
        keyword,
        "{" | "}" | "(" | ")" | "[" | "]" | ";" | "," | "." | "..." | "#" | "##"
    )
}

/// Check if a string is a type declaration keyword
pub fn is_c_type_declaration(keyword: &str) -> bool {
    matches!(keyword, "typedef" | "struct" | "enum" | "union")
}

/// Check if a string is any recognized C keyword (of any category)
/// This is a comprehensive check that covers all keyword types:
/// - Type keywords (int, char, void, etc.)
/// - Storage classes (static, extern, etc.)
/// - Type qualifiers (const, volatile, etc.)
/// - Control flow (if, while, for, etc.)
/// - Type declarations (typedef, struct, enum, union)
/// - Operators (sizeof, _Alignof, etc.)
/// - Preprocessor directives
/// - Punctuation
pub fn is_c_keyword(keyword: &str) -> bool {
    is_c_type_keyword(keyword)
        || is_c_storage_class(keyword)
        || is_c_type_qualifier(keyword)
        || is_c_control_flow(keyword)
        || is_c_type_declaration(keyword)
        || is_preprocessor(keyword)
        || is_c_operator(keyword)
        || is_c_punctuation(keyword)
}

/// Get all C keywords as a static slice
pub fn all_c_keywords() -> &'static [&'static str] {
    &[
        // Type declarations
        "typedef", "struct", "enum", "union",
        // Storage classes
        "static", "extern", "register", "auto", "inline", "__inline", "__inline__",
        "_Thread_local", "thread_local", "_Noreturn", "noreturn",
        // Type qualifiers
        "const", "volatile", "restrict", "_Atomic", "__restrict", "__restrict__",
        // Basic types
        "void", "int", "char", "float", "double", "long", "short", "unsigned", "signed",
        "_Bool", "_Complex", "_Imaginary",
        // Fixed-width types
        "size_t", "ptrdiff_t", "intptr_t", "uintptr_t",
        "int8_t", "int16_t", "int32_t", "int64_t",
        "uint8_t", "uint16_t", "uint32_t", "uint64_t",
        "intmax_t", "uintmax_t",
        // Control flow
        "if", "else", "while", "for", "do", "switch", "case", "default",
        "break", "continue", "goto", "return",
        // Operators (keyword-like)
        "sizeof", "_Alignof", "alignof", "_Generic", "_Static_assert", "static_assert",
        // Preprocessor
        "define", "include", "ifdef", "ifndef", "endif", "elif", "elifdef", "elifndef",
        "pragma", "warning", "error", "undef", "line", "defined",
        "__has_include", "__has_c_attribute", "_Pragma",
    ]
}

// ============================================================================
// Rust Keyword Functions
// ============================================================================

/// Check if a string is a Rust primitive type
pub fn is_rust_primitive_type(keyword: &str) -> bool {
    matches!(
        keyword,
        "bool"
            | "char"
            | "str"
            | "i8"
            | "i16"
            | "i32"
            | "i64"
            | "i128"
            | "isize"
            | "u8"
            | "u16"
            | "u32"
            | "u64"
            | "u128"
            | "usize"
            | "f32"
            | "f64"
            | "()"
    )
}

/// Check if a string is a Rust smart pointer or container type
pub fn is_rust_container_type(keyword: &str) -> bool {
    matches!(
        keyword,
        "Box"
            | "Rc"
            | "Arc"
            | "Cell"
            | "RefCell"
            | "Mutex"
            | "RwLock"
            | "Vec"
            | "VecDeque"
            | "LinkedList"
            | "HashMap"
            | "BTreeMap"
            | "HashSet"
            | "BTreeSet"
            | "String"
            | "Option"
            | "Result"
            | "Cow"
            | "Pin"
    )
}

/// Check if a string is a Rust visibility modifier
pub fn is_rust_visibility(keyword: &str) -> bool {
    matches!(keyword, "pub" | "pub(crate)" | "pub(super)" | "pub(self)")
}

/// Check if a string is a Rust storage/lifetime keyword
pub fn is_rust_storage(keyword: &str) -> bool {
    matches!(
        keyword,
        "static" | "const" | "mut" | "ref" | "move" | "box"
    )
}

/// Check if a string is a Rust type qualifier
pub fn is_rust_type_qualifier(keyword: &str) -> bool {
    matches!(keyword, "mut" | "const" | "ref" | "dyn" | "impl")
}

/// Check if a string is a Rust control flow keyword
pub fn is_rust_control_flow(keyword: &str) -> bool {
    matches!(
        keyword,
        "if"
            | "else"
            | "match"
            | "loop"
            | "while"
            | "for"
            | "break"
            | "continue"
            | "return"
            | "yield"
    )
}

/// Check if a string is a Rust type declaration keyword
pub fn is_rust_type_declaration(keyword: &str) -> bool {
    matches!(
        keyword,
        "struct" | "enum" | "union" | "trait" | "type" | "impl"
    )
}

/// Check if a string is a Rust module/use keyword
pub fn is_rust_module(keyword: &str) -> bool {
    matches!(keyword, "mod" | "use" | "crate" | "super" | "self" | "as" | "extern")
}

/// Check if a string is a Rust function-related keyword
pub fn is_rust_function(keyword: &str) -> bool {
    matches!(keyword, "fn" | "async" | "await" | "unsafe" | "where")
}

/// Check if a string is a Rust operator keyword
pub fn is_rust_operator_keyword(keyword: &str) -> bool {
    matches!(
        keyword,
        "as" | "in" | "sizeof" | "typeof" | "offsetof"
    )
}

/// Check if a string is a Rust macro-related keyword
pub fn is_rust_macro(keyword: &str) -> bool {
    matches!(keyword, "macro_rules" | "macro")
}

/// Check if a string is a Rust attribute keyword
pub fn is_rust_attribute(keyword: &str) -> bool {
    matches!(
        keyword,
        "derive"
            | "cfg"
            | "test"
            | "bench"
            | "allow"
            | "warn"
            | "deny"
            | "forbid"
            | "deprecated"
            | "must_use"
            | "inline"
            | "cold"
            | "repr"
            | "no_mangle"
            | "link"
            | "feature"
    )
}

/// Check if a string is a Rust reserved keyword
pub fn is_rust_reserved(keyword: &str) -> bool {
    matches!(
        keyword,
        "abstract"
            | "become"
            | "do"
            | "final"
            | "macro"
            | "override"
            | "priv"
            | "try"
            | "typeof"
            | "unsized"
            | "virtual"
            | "yield"
    )
}

/// Check if a string is any Rust keyword (strict keywords only)
pub fn is_rust_strict_keyword(keyword: &str) -> bool {
    matches!(
        keyword,
        "as"
            | "async"
            | "await"
            | "break"
            | "const"
            | "continue"
            | "crate"
            | "dyn"
            | "else"
            | "enum"
            | "extern"
            | "false"
            | "fn"
            | "for"
            | "if"
            | "impl"
            | "in"
            | "let"
            | "loop"
            | "match"
            | "mod"
            | "move"
            | "mut"
            | "pub"
            | "ref"
            | "return"
            | "self"
            | "Self"
            | "static"
            | "struct"
            | "super"
            | "trait"
            | "true"
            | "type"
            | "unsafe"
            | "use"
            | "where"
            | "while"
    )
}

/// Check if a string is any recognized Rust keyword (of any category)
pub fn is_rust_keyword(keyword: &str) -> bool {
    is_rust_strict_keyword(keyword)
        || is_rust_reserved(keyword)
        || is_rust_primitive_type(keyword)
}

/// Get all Rust strict keywords as a static slice
pub fn all_rust_keywords() -> &'static [&'static str] {
    &[
        // Strict keywords
        "as", "async", "await", "break", "const", "continue", "crate", "dyn",
        "else", "enum", "extern", "false", "fn", "for", "if", "impl", "in",
        "let", "loop", "match", "mod", "move", "mut", "pub", "ref", "return",
        "self", "Self", "static", "struct", "super", "trait", "true", "type",
        "unsafe", "use", "where", "while",
        // Reserved keywords
        "abstract", "become", "box", "do", "final", "macro", "override",
        "priv", "try", "typeof", "unsized", "virtual", "yield",
        // Primitive types
        "bool", "char", "str", "i8", "i16", "i32", "i64", "i128", "isize",
        "u8", "u16", "u32", "u64", "u128", "usize", "f32", "f64",
    ]
}

/// Get Rust primitive types as a static slice
pub fn all_rust_primitive_types() -> &'static [&'static str] {
    &[
        "bool", "char", "str",
        "i8", "i16", "i32", "i64", "i128", "isize",
        "u8", "u16", "u32", "u64", "u128", "usize",
        "f32", "f64", "()",
    ]
}

/// Check if a Rust identifier needs to be raw (prefixed with r#)
pub fn needs_raw_identifier(name: &str) -> bool {
    is_rust_strict_keyword(name) || is_rust_reserved(name)
}

/// Convert a name to a raw identifier if needed
pub fn to_raw_identifier(name: &str) -> String {
    if needs_raw_identifier(name) {
        format!("r#{}", name)
    } else {
        name.to_string()
    }
}

/// Map C type to Rust type
pub fn c_to_rust_type(c_type: &str) -> Option<&'static str> {
    match c_type {
        // Basic types
        "void" => Some("()"),
        "char" | "signed char" => Some("i8"),
        "unsigned char" => Some("u8"),
        "short" | "signed short" | "short int" | "signed short int" => Some("i16"),
        "unsigned short" | "unsigned short int" => Some("u16"),
        "int" | "signed" | "signed int" => Some("i32"),
        "unsigned" | "unsigned int" => Some("u32"),
        "long" | "signed long" | "long int" | "signed long int" => Some("i64"),
        "unsigned long" | "unsigned long int" => Some("u64"),
        "long long" | "signed long long" | "long long int" => Some("i64"),
        "unsigned long long" | "unsigned long long int" => Some("u64"),
        "float" => Some("f32"),
        "double" | "long double" => Some("f64"),
        "_Bool" | "bool" => Some("bool"),
        // Fixed-width types
        "int8_t" => Some("i8"),
        "int16_t" => Some("i16"),
        "int32_t" => Some("i32"),
        "int64_t" => Some("i64"),
        "uint8_t" => Some("u8"),
        "uint16_t" => Some("u16"),
        "uint32_t" => Some("u32"),
        "uint64_t" => Some("u64"),
        "size_t" | "uintptr_t" => Some("usize"),
        "ssize_t" | "intptr_t" | "ptrdiff_t" => Some("isize"),
        "intmax_t" => Some("i64"),
        "uintmax_t" => Some("u64"),
        _ => None,
    }
}

/// Map C storage class to Rust equivalent
pub fn c_to_rust_storage(c_storage: &str) -> Option<&'static str> {
    match c_storage {
        "static" => Some("static"),
        "const" => Some("const"),
        "extern" => Some("extern"),
        "register" => None, // No Rust equivalent
        "auto" => None,     // No Rust equivalent (default in Rust)
        "inline" | "__inline" | "__inline__" => Some("#[inline]"),
        "_Thread_local" | "thread_local" => Some("thread_local!"),
        "_Noreturn" | "noreturn" => Some("-> !"),
        _ => None,
    }
}

/// Map C type qualifier to Rust equivalent
pub fn c_to_rust_qualifier(c_qualifier: &str) -> Option<&'static str> {
    match c_qualifier {
        "const" => Some(""), // const by default in Rust
        "volatile" => None,  // No direct equivalent, use atomic types
        "restrict" | "__restrict" | "__restrict__" => None, // No equivalent
        "_Atomic" => Some("Atomic"), // std::sync::atomic types
        _ => None,
    }
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
        let valid_seq = vec![
            "typedef".to_string(),
            "struct".to_string(),
            "identifier".to_string(),
        ];
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
        order.register_keyword(Keyword::new("a", KeywordCategory::Identifier));
        order.register_keyword(Keyword::new("b", KeywordCategory::Identifier));

        // Link a -> b
        order.link("a", "b");
        assert!(order.can_follow("a", "b"));

        // Unlink
        order.unlink("a", "b");
        assert!(!order.can_follow("a", "b"));
    }
}
