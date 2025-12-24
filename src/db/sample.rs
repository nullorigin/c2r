//! Sample System - Pattern Generation and Reverse Matching
//!
//! This module provides a sampling system for the DB that operates as the inverse of pattern
//! matching. While matching finds patterns in token sequences, sampling generates token
//! sequences from patterns using the Entry-based system.
//!
//! # Key Components
//! - **Sample**: Core sample data using Entry for storage
//! - **SampleDB**: Database of samples with indexing and statistics
//! - **Generator**: Engine for generating/sampling from patterns
//!
//! # Integration with DB System
//! - Uses `Entry` for node data storage
//! - Uses `Links` for graph connectivity
//! - Uses `BranchOp` for conditional generation
//! - Compatible with `Pattern` and `PatternCore` from pattern module

use std::collections::HashMap;
use std::fmt::{self, Debug};
use std::hash::Hash;

use crate::db::web::Entry;

// ============================================================================
// Sample Node Kind
// ============================================================================

/// Kind of sample node
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SampleKind {
    /// Root node of a pattern
    Root,
    /// Literal value to emit
    Literal,
    /// Choice point with weighted alternatives
    Choice,
    /// Sequence marker
    Sequence,
    /// Optional element (may or may not appear)
    Optional,
    /// Repetition with (min, max) bounds
    Repeat { min: usize, max: Option<usize> },
    /// Reference to another pattern by name
    PatternRef(String),
    /// Terminal/accepting node
    Terminal,
    /// Placeholder for template substitution
    Placeholder(String),
    /// Conditional branch
    Branch,
}

impl Default for SampleKind {
    fn default() -> Self {
        Self::Literal
    }
}

// ============================================================================
// Sample Edge Kind
// ============================================================================

/// Edge kind for sample connections
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum SampleEdgeKind {
    /// Direct sequence transition
    #[default]
    Sequence,
    /// Alternative choice (weighted)
    Choice,
    /// Optional element
    Optional,
    /// Repetition edge
    Repeat,
    /// Back-reference to earlier node
    BackRef,
    /// Epsilon transition (no output)
    Epsilon,
    /// Constraint edge (conditional)
    Constraint,
}

// ============================================================================
// Sample Edge
// ============================================================================

/// Edge in the sample graph with weight and metadata
#[derive(Clone)]
pub struct SampleEdge {
    pub target: usize,
    pub kind: SampleEdgeKind,
    pub weight: f64,
    pub priority: i16,
    pub label: Option<String>,
    pub constraint: Option<fn(&Entry) -> bool>,
}

impl SampleEdge {
    pub fn new(target: usize, kind: SampleEdgeKind) -> Self {
        Self {
            target,
            kind,
            weight: 1.0,
            priority: 0,
            label: None,
            constraint: None,
        }
    }

    pub fn sequence(target: usize) -> Self {
        Self::new(target, SampleEdgeKind::Sequence)
    }

    pub fn choice(target: usize, weight: f64) -> Self {
        Self {
            weight,
            ..Self::new(target, SampleEdgeKind::Choice)
        }
    }

    pub fn optional(target: usize) -> Self {
        Self::new(target, SampleEdgeKind::Optional)
    }

    pub fn repeat(target: usize) -> Self {
        Self::new(target, SampleEdgeKind::Repeat)
    }

    pub fn epsilon(target: usize) -> Self {
        Self::new(target, SampleEdgeKind::Epsilon)
    }

    pub fn with_weight(mut self, weight: f64) -> Self {
        self.weight = weight;
        self
    }

    pub fn with_priority(mut self, priority: i16) -> Self {
        self.priority = priority;
        self
    }

    pub fn with_label(mut self, label: impl Into<String>) -> Self {
        self.label = Some(label.into());
        self
    }

    pub fn with_constraint(mut self, f: fn(&Entry) -> bool) -> Self {
        self.constraint = Some(f);
        self
    }
}

impl Debug for SampleEdge {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "SampleEdge({:?}->{}, w={:.2})", self.kind, self.target, self.weight)
    }
}

// ============================================================================
// Sample Node
// ============================================================================

/// A node in the sample graph, using Entry for data storage
#[derive(Clone)]
pub struct SampleNode {
    pub id: usize,
    pub kind: SampleKind,
    pub data: Entry,
    pub edges: Vec<SampleEdge>,
    pub depth: usize,
    pub pattern_id: Option<usize>,
    pub terminal: bool,
    pub visit_count: usize,
    pub emit_count: usize,
}

impl SampleNode {
    pub fn new(id: usize, kind: SampleKind, data: Entry) -> Self {
        Self {
            id,
            kind,
            data,
            edges: Vec::new(),
            depth: 0,
            pattern_id: None,
            terminal: false,
            visit_count: 0,
            emit_count: 0,
        }
    }

    pub fn root(id: usize) -> Self {
        Self::new(id, SampleKind::Root, Entry::unit())
    }

    pub fn literal(id: usize, value: Entry) -> Self {
        Self::new(id, SampleKind::Literal, value)
    }

    pub fn choice(id: usize) -> Self {
        Self::new(id, SampleKind::Choice, Entry::unit())
    }

    pub fn sequence(id: usize) -> Self {
        Self::new(id, SampleKind::Sequence, Entry::unit())
    }

    pub fn terminal(id: usize) -> Self {
        let mut n = Self::new(id, SampleKind::Terminal, Entry::unit());
        n.terminal = true;
        n
    }

    pub fn optional(id: usize) -> Self {
        Self::new(id, SampleKind::Optional, Entry::unit())
    }

    pub fn repeat(id: usize, min: usize, max: Option<usize>) -> Self {
        Self::new(id, SampleKind::Repeat { min, max }, Entry::unit())
    }

    pub fn pattern_ref(id: usize, name: impl Into<String>) -> Self {
        Self::new(id, SampleKind::PatternRef(name.into()), Entry::unit())
    }

    pub fn placeholder(id: usize, name: impl Into<String>) -> Self {
        Self::new(id, SampleKind::Placeholder(name.into()), Entry::unit())
    }

    pub fn branch(id: usize) -> Self {
        Self::new(id, SampleKind::Branch, Entry::unit())
    }

    pub fn add_edge(&mut self, edge: SampleEdge) {
        self.edges.push(edge);
    }

    pub fn is_terminal(&self) -> bool {
        self.terminal
    }

    pub fn is_emitting(&self) -> bool {
        matches!(self.kind, SampleKind::Literal)
    }

    pub fn total_weight(&self) -> f64 {
        self.edges.iter().map(|e| e.weight).sum()
    }

    pub fn normalized_weights(&self) -> Vec<f64> {
        let total = self.total_weight();
        if total == 0.0 {
            vec![1.0 / self.edges.len().max(1) as f64; self.edges.len()]
        } else {
            self.edges.iter().map(|e| e.weight / total).collect()
        }
    }
}

impl Debug for SampleNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "SampleNode({}, {:?}, {} edges)", self.id, self.kind, self.edges.len())
    }
}

// ============================================================================
// Sample Pattern
// ============================================================================

/// Pattern definition for sample generation
#[derive(Debug, Clone)]
pub struct SamplePattern {
    pub id: usize,
    pub name: String,
    pub category: String,
    pub root_node: usize,
    pub terminal_nodes: Vec<usize>,
    pub priority: i16,
    pub metadata: HashMap<String, Entry>,
    pub stats: SamplePatternStats,
}

impl SamplePattern {
    pub fn new(id: usize, name: impl Into<String>, root: usize) -> Self {
        Self {
            id,
            name: name.into(),
            category: String::new(),
            root_node: root,
            terminal_nodes: Vec::new(),
            priority: 0,
            metadata: HashMap::new(),
            stats: SamplePatternStats::default(),
        }
    }

    pub fn with_category(mut self, category: impl Into<String>) -> Self {
        self.category = category.into();
        self
    }

    pub fn with_priority(mut self, priority: i16) -> Self {
        self.priority = priority;
        self
    }

    pub fn with_metadata(mut self, key: impl Into<String>, value: Entry) -> Self {
        self.metadata.insert(key.into(), value);
        self
    }
}

/// Statistics for a sample pattern
#[derive(Debug, Clone, Default)]
pub struct SamplePatternStats {
    pub generation_count: u32,
    pub success_count: u32,
    pub failure_count: u32,
    pub avg_tokens: f64,
    pub min_tokens: usize,
    pub max_tokens: usize,
}

impl SamplePatternStats {
    pub fn success_rate(&self) -> f64 {
        let total = self.success_count + self.failure_count;
        if total == 0 {
            0.0
        } else {
            self.success_count as f64 / total as f64
        }
    }

    pub fn record_generation(&mut self, token_count: usize, success: bool) {
        self.generation_count += 1;
        if success {
            self.success_count += 1;
        } else {
            self.failure_count += 1;
        }
        // Update token stats
        if self.generation_count == 1 {
            self.min_tokens = token_count;
            self.max_tokens = token_count;
            self.avg_tokens = token_count as f64;
        } else {
            self.min_tokens = self.min_tokens.min(token_count);
            self.max_tokens = self.max_tokens.max(token_count);
            let n = self.generation_count as f64;
            self.avg_tokens = self.avg_tokens * (n - 1.0) / n + token_count as f64 / n;
        }
    }
}

// ============================================================================
// Generation Result
// ============================================================================

/// Result of sample generation
#[derive(Debug, Clone)]
pub struct GenerationResult {
    pub entries: Vec<Entry>,
    pub pattern_id: usize,
    pub path: Vec<usize>,
    pub confidence: f64,
    pub quality: MatchQuality,
    pub alternatives: Vec<Vec<Entry>>,
}

impl GenerationResult {
    pub fn new(pattern_id: usize) -> Self {
        Self {
            entries: Vec::new(),
            pattern_id,
            path: Vec::new(),
            confidence: 1.0,
            quality: MatchQuality::None,
            alternatives: Vec::new(),
        }
    }

    pub fn with_entries(mut self, entries: Vec<Entry>) -> Self {
        self.entries = entries;
        self
    }

    pub fn with_path(mut self, path: Vec<usize>) -> Self {
        self.path = path;
        self
    }

    pub fn with_confidence(mut self, conf: f64) -> Self {
        self.confidence = conf.clamp(0.0, 1.0);
        self.quality = MatchQuality::from_confidence(self.confidence);
        self
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Convert entries to a Vec of a specific type if possible
    pub fn as_strings(&self) -> Vec<String> {
        self.entries
            .iter()
            .filter_map(|e| match e {
                Entry::String(s, _) => Some(s.clone()),
                _ => None,
            })
            .collect()
    }

    /// Convert to Entry::Vec
    pub fn to_entry(&self) -> Entry {
        Entry::vec(self.entries.clone())
    }
}

// ============================================================================
// Match Quality
// ============================================================================

/// Quality level of a pattern match or generation
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum MatchQuality {
    Exact,    // 100% match
    High,     // 90%+ match
    Good,     // 70%+ match
    Partial,  // 50%+ match
    Low,      // 30%+ match
    #[default]
    None,     // No match
}

impl MatchQuality {
    pub fn from_confidence(conf: f64) -> Self {
        if conf >= 1.0 {
            Self::Exact
        } else if conf >= 0.9 {
            Self::High
        } else if conf >= 0.7 {
            Self::Good
        } else if conf >= 0.5 {
            Self::Partial
        } else if conf >= 0.3 {
            Self::Low
        } else {
            Self::None
        }
    }

    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Exact => "exact",
            Self::High => "high",
            Self::Good => "good",
            Self::Partial => "partial",
            Self::Low => "low",
            Self::None => "none",
        }
    }

    pub fn to_confidence(&self) -> f64 {
        match self {
            Self::Exact => 1.0,
            Self::High => 0.95,
            Self::Good => 0.8,
            Self::Partial => 0.6,
            Self::Low => 0.4,
            Self::None => 0.0,
        }
    }
}

// ============================================================================
// Confidence Result
// ============================================================================

/// Detailed confidence calculation result
#[derive(Debug, Clone)]
pub struct ConfidenceResult {
    pub overall: f64,
    pub token_score: f64,
    pub pattern_score: f64,
    pub frequency_score: f64,
    pub quality: MatchQuality,
    pub reasons: Vec<String>,
}

impl ConfidenceResult {
    pub fn new(overall: f64) -> Self {
        let overall = overall.clamp(0.0, 1.0);
        Self {
            overall,
            token_score: 0.0,
            pattern_score: 0.0,
            frequency_score: 0.0,
            quality: MatchQuality::from_confidence(overall),
            reasons: Vec::new(),
        }
    }

    pub fn with_scores(mut self, token: f64, pattern: f64, freq: f64) -> Self {
        self.token_score = token;
        self.pattern_score = pattern;
        self.frequency_score = freq;
        self
    }

    pub fn add_reason(mut self, reason: impl Into<String>) -> Self {
        self.reasons.push(reason.into());
        self
    }

    pub fn to_entry(&self) -> Entry {
        let mut attrs = HashMap::new();
        attrs.insert("overall".to_string(), Entry::f64(self.overall));
        attrs.insert("token_score".to_string(), Entry::f64(self.token_score));
        attrs.insert("pattern_score".to_string(), Entry::f64(self.pattern_score));
        attrs.insert("frequency_score".to_string(), Entry::f64(self.frequency_score));
        attrs.insert("quality".to_string(), Entry::string(self.quality.as_str()));
        Entry::node_with_attrs("confidence", "result", attrs)
    }
}

// ============================================================================
// Sample Statistics
// ============================================================================

/// Statistics for the sample database
#[derive(Debug, Clone, Default)]
pub struct SampleStats {
    pub total_nodes: usize,
    pub total_patterns: usize,
    pub total_edges: usize,
    pub unique_entries: usize,
    pub unique_bigrams: usize,
    pub avg_path_length: f64,
    pub generation_count: u32,
}

impl SampleStats {
    pub fn to_entry(&self) -> Entry {
        let mut attrs = HashMap::new();
        attrs.insert("total_nodes".to_string(), Entry::usize(self.total_nodes));
        attrs.insert("total_patterns".to_string(), Entry::usize(self.total_patterns));
        attrs.insert("total_edges".to_string(), Entry::usize(self.total_edges));
        attrs.insert("unique_entries".to_string(), Entry::usize(self.unique_entries));
        attrs.insert("unique_bigrams".to_string(), Entry::usize(self.unique_bigrams));
        attrs.insert("avg_path_length".to_string(), Entry::f64(self.avg_path_length));
        attrs.insert("generation_count".to_string(), Entry::u32(self.generation_count));
        Entry::node_with_attrs("sample_stats", "statistics", attrs)
    }
}

// ============================================================================
// Sample Database
// ============================================================================

// Note: The Samples container has been removed. Use Web for unified storage:
//   let mut web = Web::new();
//   web.add(&sample);  // Add Sample via Build trait
//   web.by_kind("Sample");  // Query all samples
//   web.by_category("function");  // Query by category
//   web.by_name("sample_name");  // Query by name

// ============================================================================
// Sample - Data holder that implements Build trait
// ============================================================================

use crate::db::web::Build;

/// Sample element types for building patterns
#[derive(Debug, Clone)]
pub enum SampleElement {
    /// Literal entry value
    Literal(Entry),
    /// Choice group with alternatives
    Choice(Vec<Vec<SampleElement>>),
    /// Optional elements
    Optional(Vec<SampleElement>),
    /// Sequence of elements
    Sequence(Vec<SampleElement>),
}

/// A sample pattern that can be built and added to a Web database.
/// Replaces SampleBuilder - holds its own data and implements Build.
#[derive(Debug, Clone)]
pub struct Sample {
    pub name: String,
    pub category: String,
    pub elements: Vec<SampleElement>,
    pub priority: i16,
    pub metadata: HashMap<String, Entry>,
}

impl Sample {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            category: String::new(),
            elements: Vec::new(),
            priority: 0,
            metadata: HashMap::new(),
        }
    }

    /// Set category
    pub fn with_category(mut self, category: impl Into<String>) -> Self {
        self.category = category.into();
        self
    }

    /// Set priority
    pub fn with_priority(mut self, priority: i16) -> Self {
        self.priority = priority;
        self
    }

    /// Add a literal entry
    pub fn literal(mut self, value: Entry) -> Self {
        self.elements.push(SampleElement::Literal(value));
        self
    }

    /// Add a string literal
    pub fn string(self, s: impl Into<String>) -> Self {
        self.literal(Entry::string(s))
    }

    /// Add sequence of entries
    pub fn entries(mut self, values: &[Entry]) -> Self {
        for v in values {
            self.elements.push(SampleElement::Literal(v.clone()));
        }
        self
    }

    /// Add sequence of strings
    pub fn strings(mut self, values: &[&str]) -> Self {
        for s in values {
            self.elements.push(SampleElement::Literal(Entry::string(*s)));
        }
        self
    }

    /// Add a choice group with alternatives
    pub fn choice(mut self, alternatives: Vec<Vec<Entry>>) -> Self {
        let alt_elements: Vec<Vec<SampleElement>> = alternatives
            .into_iter()
            .map(|alt| alt.into_iter().map(SampleElement::Literal).collect())
            .collect();
        self.elements.push(SampleElement::Choice(alt_elements));
        self
    }

    /// Add string choice alternatives
    pub fn choice_strings(self, alternatives: &[&[&str]]) -> Self {
        let alt_entries: Vec<Vec<Entry>> = alternatives
            .iter()
            .map(|alt| alt.iter().map(|s| Entry::string(*s)).collect())
            .collect();
        self.choice(alt_entries)
    }

    /// Add optional elements
    pub fn optional(mut self, values: &[Entry]) -> Self {
        let elements: Vec<SampleElement> = values.iter()
            .map(|v| SampleElement::Literal(v.clone()))
            .collect();
        self.elements.push(SampleElement::Optional(elements));
        self
    }

    /// Add optional strings
    pub fn optional_strings(self, values: &[&str]) -> Self {
        let entries: Vec<Entry> = values.iter().map(|s| Entry::string(*s)).collect();
        self.optional(&entries)
    }

    /// Add metadata
    pub fn with_metadata(mut self, key: impl Into<String>, value: Entry) -> Self {
        self.metadata.insert(key.into(), value);
        self
    }

    /// Convert elements to Entry representation
    fn elements_to_entry(&self) -> Entry {
        let entries: Vec<Entry> = self.elements.iter()
            .map(|e| self.element_to_entry(e))
            .collect();
        Entry::vec(entries)
    }

    fn element_to_entry(&self, element: &SampleElement) -> Entry {
        match element {
            SampleElement::Literal(e) => e.clone(),
            SampleElement::Choice(alts) => {
                let alt_entries: Vec<Entry> = alts.iter()
                    .map(|alt| {
                        let items: Vec<Entry> = alt.iter()
                            .map(|e| self.element_to_entry(e))
                            .collect();
                        Entry::vec(items)
                    })
                    .collect();
                Entry::node("Choice", "")
                    .with_attr("alternatives", Entry::vec(alt_entries))
            }
            SampleElement::Optional(elements) => {
                let items: Vec<Entry> = elements.iter()
                    .map(|e| self.element_to_entry(e))
                    .collect();
                Entry::node("Optional", "")
                    .with_attr("elements", Entry::vec(items))
            }
            SampleElement::Sequence(elements) => {
                let items: Vec<Entry> = elements.iter()
                    .map(|e| self.element_to_entry(e))
                    .collect();
                Entry::vec(items)
            }
        }
    }
}

impl Build for Sample {
    fn to_entry(&self) -> Entry {
        let mut node = Entry::node("Sample", &self.name);
        node.set_attr("elements", self.elements_to_entry());
        if !self.category.is_empty() {
            node.set_attr("category", Entry::string(&self.category));
        }
        node.set_attr("priority", Entry::i16(self.priority));
        for (k, v) in &self.metadata {
            node.set_attr(k, v.clone());
        }
        node
    }

    fn kind(&self) -> &str {
        "Sample"
    }

    fn name(&self) -> Option<&str> {
        Some(&self.name)
    }

    fn category(&self) -> Option<&str> {
        if self.category.is_empty() {
            None
        } else {
            Some(&self.category)
        }
    }

    fn priority(&self) -> i16 {
        self.priority
    }
}

// Helper trait for Entry to use with_attr in builder pattern
trait EntryBuilderExt {
    fn with_attr(self, key: &str, value: Entry) -> Self;
}

impl EntryBuilderExt for Entry {
    fn with_attr(mut self, key: &str, value: Entry) -> Self {
        self.set_attr(key, value);
        self
    }
}


// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    // Note: Samples container tests removed - use Web for unified storage
    // See web.rs tests for Web-based sample storage tests

    #[test]
    fn test_sample_build_trait() {
        let sample = Sample::new("test_pattern")
            .with_category("testing")
            .with_priority(5)
            .strings(&["hello", "world"]);
        
        assert_eq!(Build::kind(&sample), "Sample");
        assert_eq!(Build::name(&sample), Some("test_pattern"));
        assert_eq!(Build::category(&sample), Some("testing"));
        assert_eq!(Build::priority(&sample), 5);
        
        let entry = sample.to_entry();
        assert_eq!(entry.kind(), Some("Sample"));
        assert_eq!(entry.name(), Some("test_pattern"));
    }

    #[test]
    fn test_match_quality() {
        assert_eq!(MatchQuality::from_confidence(1.0), MatchQuality::Exact);
        assert_eq!(MatchQuality::from_confidence(0.95), MatchQuality::High);
        assert_eq!(MatchQuality::from_confidence(0.8), MatchQuality::Good);
        assert_eq!(MatchQuality::from_confidence(0.6), MatchQuality::Partial);
        assert_eq!(MatchQuality::from_confidence(0.4), MatchQuality::Low);
        assert_eq!(MatchQuality::from_confidence(0.1), MatchQuality::None);
    }

    #[test]
    fn test_generation_result_to_entry() {
        let mut result = GenerationResult::new(0);
        result.entries = vec![Entry::string("hello"), Entry::string("world")];
        let entry = result.to_entry();
        assert!(matches!(entry, Entry::Vec(_, _)));
    }
}
