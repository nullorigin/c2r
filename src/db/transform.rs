//! Transformation Tracking
//!
//! Tracks transformations applied to tokens and code elements during processing.
//! Provides history, rollback capability, and transformation analysis.
//!
//! Key components:
//! - `Transform`: Records a single transformation
//! - `TransformType`: Types of transformations
//! - `Relationship`: Tracks relationships between tokens/elements
//! - `RelationType`: Types of relationships
//!
//! For type conversion utilities, see the `convert` module.

use crate::db::web::{Build, Entry};
use std::collections::HashMap;
use std::ops::Range;
use std::time::SystemTime;

// Re-export conversion types for backward compatibility
pub use crate::db::convert::{
    convert_identifier, convert_operator, is_assignment_operator, is_binary_operator, is_c_keyword,
    is_operator, is_valid_c_identifier, sanitize_rust_identifier, to_camel_case, to_pascal_case,
    to_screaming_snake_case, to_snake_case, IdentifierCase, TypeCategory,
    TypeConverter, TypeMetadata,
};

// ============================================================================
// Transformation Types
// ============================================================================

/// A recorded transformation
#[derive(Debug, Clone, PartialEq)]
pub struct Transform {
    /// Unique transformation ID
    pub id: String,

    /// Type of transformation
    pub transform_type: TransformType,

    /// Slot ID this transformation applies to
    pub slot_id: usize,

    /// Token range affected
    pub range: Range<usize>,

    /// Original tokens before transformation
    pub original: Vec<String>,

    /// Resulting tokens after transformation
    pub result: Vec<String>,

    /// Handler that performed the transformation
    pub handler: Option<String>,

    /// Transformation timestamp
    pub timestamp: SystemTime,

    /// Additional metadata
    pub metadata: HashMap<String, String>,

    /// Whether this transformation can be undone
    pub reversible: bool,
}

/// Types of transformations
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TransformType {
    /// Tokens consumed (replaced with placeholder)
    Consume,

    /// Tokens replaced with different tokens
    Replace,

    /// Tokens expanded into more tokens
    Expand,

    /// Multiple tokens merged into fewer
    Merge,

    /// Single token split into multiple
    Split,

    /// Tokens reordered
    Reorder,

    /// Tokens inserted
    Insert,

    /// Tokens deleted
    Delete,

    /// C to Rust conversion
    Convert,

    /// Custom transformation
    Custom(String),
}

impl TransformType {
    pub fn as_str(&self) -> &str {
        match self {
            TransformType::Consume => "consume",
            TransformType::Replace => "replace",
            TransformType::Expand => "expand",
            TransformType::Merge => "merge",
            TransformType::Split => "split",
            TransformType::Reorder => "reorder",
            TransformType::Insert => "insert",
            TransformType::Delete => "delete",
            TransformType::Convert => "convert",
            TransformType::Custom(s) => s.as_str(),
        }
    }
}

// ============================================================================
// Transform Builder
// ============================================================================

impl Transform {
    /// Create a new transformation record
    pub fn new(
        id: impl Into<String>,
        transform_type: TransformType,
        slot_id: usize,
        range: Range<usize>,
    ) -> Self {
        Self {
            id: id.into(),
            transform_type,
            slot_id,
            range,
            original: Vec::new(),
            result: Vec::new(),
            handler: None,
            timestamp: SystemTime::now(),
            metadata: HashMap::new(),
            reversible: true,
        }
    }

    /// Create a consume transformation
    pub fn consume(id: impl Into<String>, slot_id: usize, range: Range<usize>) -> Self {
        Self::new(id, TransformType::Consume, slot_id, range)
    }

    /// Create a replace transformation
    pub fn replace(id: impl Into<String>, slot_id: usize, range: Range<usize>) -> Self {
        Self::new(id, TransformType::Replace, slot_id, range)
    }

    /// Create a convert (C to Rust) transformation
    pub fn convert(id: impl Into<String>, slot_id: usize, range: Range<usize>) -> Self {
        Self::new(id, TransformType::Convert, slot_id, range)
    }

    /// Set original tokens
    pub fn with_original(mut self, tokens: Vec<String>) -> Self {
        self.original = tokens;
        self
    }

    /// Set result tokens
    pub fn with_result(mut self, tokens: Vec<String>) -> Self {
        self.result = tokens;
        self
    }

    /// Set handler name
    pub fn by_handler(mut self, handler: impl Into<String>) -> Self {
        self.handler = Some(handler.into());
        self
    }

    /// Add metadata
    pub fn with_metadata(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.metadata.insert(key.into(), value.into());
        self
    }

    /// Mark as non-reversible
    pub fn irreversible(mut self) -> Self {
        self.reversible = false;
        self
    }

    /// Check if tokens changed
    pub fn tokens_changed(&self) -> bool {
        self.original != self.result
    }

    /// Get token count delta
    pub fn token_delta(&self) -> i32 {
        self.result.len() as i32 - self.original.len() as i32
    }

    // ========================================================================
    // Entry Conversion
    // ========================================================================

    /// Convert to Entry representation
    pub fn to_entry(&self) -> Entry {
        let mut node = Entry::node("Transform", &self.id);
        node.set_attr(
            "transform_type",
            Entry::string(self.transform_type.as_str()),
        );
        node.set_attr("slot_id", Entry::usize(self.slot_id));
        node.set_attr("range_start", Entry::usize(self.range.start));
        node.set_attr("range_end", Entry::usize(self.range.end));
        node.set_attr("reversible", Entry::bool(self.reversible));

        if !self.original.is_empty() {
            let orig: Vec<Entry> = self.original.iter().map(|s| Entry::string(s)).collect();
            node.set_attr("original", Entry::vec(orig));
        }

        if !self.result.is_empty() {
            let res: Vec<Entry> = self.result.iter().map(|s| Entry::string(s)).collect();
            node.set_attr("result", Entry::vec(res));
        }

        if let Some(ref h) = self.handler {
            node.set_attr("handler", Entry::string(h));
        }

        if !self.metadata.is_empty() {
            let meta: Vec<Entry> = self
                .metadata
                .iter()
                .map(|(k, v)| {
                    let mut m = Entry::node("Meta", k);
                    m.set_attr("value", Entry::string(v));
                    m
                })
                .collect();
            node.set_attr("metadata", Entry::vec(meta));
        }

        node
    }

    /// Reconstruct a Transform from an Entry
    pub fn from_entry(entry: &Entry) -> Option<Transform> {
        if entry.kind() != Some("Transform") {
            return None;
        }

        let id = entry.name()?.to_string();
        let type_str = entry.get_string_attr("transform_type").unwrap_or("custom");
        let slot_id = entry
            .get_number_attr("slot_id")
            .map(|n| n as usize)
            .unwrap_or(0);
        let range_start = entry
            .get_number_attr("range_start")
            .map(|n| n as usize)
            .unwrap_or(0);
        let range_end = entry
            .get_number_attr("range_end")
            .map(|n| n as usize)
            .unwrap_or(0);
        let reversible = entry.get_bool_attr("reversible").unwrap_or(true);
        let handler = entry.get_string_attr("handler").map(|s| s.to_string());

        let transform_type = match type_str {
            "consume" => TransformType::Consume,
            "replace" => TransformType::Replace,
            "expand" => TransformType::Expand,
            "merge" => TransformType::Merge,
            "split" => TransformType::Split,
            "reorder" => TransformType::Reorder,
            "insert" => TransformType::Insert,
            "delete" => TransformType::Delete,
            "convert" => TransformType::Convert,
            s => TransformType::Custom(s.to_string()),
        };

        let mut transform = Transform::new(id, transform_type, slot_id, range_start..range_end);
        transform.reversible = reversible;
        transform.handler = handler;

        // Reconstruct original tokens
        if let Some(orig_vec) = entry.get_vec_attr("original") {
            transform.original = orig_vec
                .iter()
                .filter_map(|e| {
                    if let Entry::String(s, _) = e {
                        Some(s.clone())
                    } else {
                        None
                    }
                })
                .collect();
        }

        // Reconstruct result tokens
        if let Some(res_vec) = entry.get_vec_attr("result") {
            transform.result = res_vec
                .iter()
                .filter_map(|e| {
                    if let Entry::String(s, _) = e {
                        Some(s.clone())
                    } else {
                        None
                    }
                })
                .collect();
        }

        Some(transform)
    }
}

/// Implement Build trait for Transform
impl Build for Transform {
    fn to_entry(&self) -> Entry {
        Transform::to_entry(self)
    }

    fn kind(&self) -> &str {
        "Transform"
    }

    fn name(&self) -> Option<&str> {
        Some(&self.id)
    }

    fn category(&self) -> Option<&str> {
        Some(self.transform_type.as_str())
    }
}

// Note: The Transforms container has been removed. Use Web for unified storage:
//   let mut web = Web::new();
//   web.add(&transform);  // Add Transform via Build trait
//   web.by_kind("Transform");  // Query all transforms
//   web.by_category("replace");  // Query by transform_type (stored as category)
//   web.by_name("transform_id");  // Query by ID

// ============================================================================
// Relationship Tracking
// ============================================================================

/// A relationship between tokens or elements
#[derive(Debug, Clone, PartialEq)]
pub struct Relationship {
    /// Source element ID
    pub from: String,

    /// Target element ID
    pub to: String,

    /// Relationship type
    pub rel_type: RelationType,

    /// Confidence score (0.0 - 1.0)
    pub confidence: f64,

    /// Additional metadata
    pub metadata: HashMap<String, String>,
}

/// Types of relationships
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RelationType {
    /// Elements form a sequence
    Sequential,

    /// Element depends on another
    Dependency,

    /// Elements are part of same group
    GroupMember,

    /// Element was derived from another
    Derivation,

    /// Elements are semantically related
    Semantic,

    /// Parent-child relationship
    Parent,

    /// Sibling relationship
    Sibling,

    /// Reference relationship
    Reference,

    /// Custom relationship
    Custom(String),
}

impl RelationType {
    pub fn as_str(&self) -> &str {
        match self {
            RelationType::Sequential => "sequential",
            RelationType::Dependency => "dependency",
            RelationType::GroupMember => "group_member",
            RelationType::Derivation => "derivation",
            RelationType::Semantic => "semantic",
            RelationType::Parent => "parent",
            RelationType::Sibling => "sibling",
            RelationType::Reference => "reference",
            RelationType::Custom(s) => s.as_str(),
        }
    }

    /// Check if this is a bidirectional relationship
    pub fn is_bidirectional(&self) -> bool {
        matches!(
            self,
            RelationType::GroupMember | RelationType::Semantic | RelationType::Sibling
        )
    }
}

impl Relationship {
    /// Create a new relationship
    pub fn new(from: impl Into<String>, to: impl Into<String>, rel_type: RelationType) -> Self {
        Self {
            from: from.into(),
            to: to.into(),
            rel_type,
            confidence: 1.0,
            metadata: HashMap::new(),
        }
    }

    /// Set confidence
    pub fn with_confidence(mut self, confidence: f64) -> Self {
        self.confidence = confidence.clamp(0.0, 1.0);
        self
    }

    /// Add metadata
    pub fn with_metadata(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.metadata.insert(key.into(), value.into());
        self
    }
}

impl Build for Relationship {
    fn to_entry(&self) -> Entry {
        let name = format!("{}->{}", self.from, self.to);
        let mut entry = Entry::node("Relationship", &name);
        entry.set_attr("from", Entry::string(&self.from));
        entry.set_attr("to", Entry::string(&self.to));
        entry.set_attr("rel_type", Entry::string(self.rel_type.as_str()));
        entry.set_attr("confidence", Entry::f64(self.confidence));

        if !self.metadata.is_empty() {
            let meta: Vec<Entry> = self
                .metadata
                .iter()
                .map(|(k, v)| {
                    let mut m = Entry::node("Meta", k);
                    m.set_attr("value", Entry::string(v));
                    m
                })
                .collect();
            entry.set_attr("metadata", Entry::vec(meta));
        }

        entry
    }

    fn kind(&self) -> &str {
        "Relationship"
    }

    fn name(&self) -> Option<&str> {
        None
    }

    fn category(&self) -> Option<&str> {
        Some(self.rel_type.as_str())
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_transform_creation() {
        let t = Transform::consume("t1", 0, 5..10)
            .with_original(vec!["a".into(), "b".into()])
            .with_result(vec!["n".into()])
            .by_handler("test_handler");

        assert_eq!(t.id, "t1");
        assert_eq!(t.slot_id, 0);
        assert_eq!(t.range, 5..10);
        assert!(t.tokens_changed());
        assert_eq!(t.token_delta(), -1);
    }

    // Note: Transforms container tests removed - use Web for unified storage
    // See web.rs tests for Web-based transform storage tests
    // Note: Type conversion tests moved to convert.rs

    #[test]
    fn test_relationship() {
        let rel =
            Relationship::new("token1", "token2", RelationType::Dependency).with_confidence(0.9);

        assert_eq!(rel.from, "token1");
        assert_eq!(rel.to, "token2");
        assert_eq!(rel.confidence, 0.9);
        assert!(!rel.rel_type.is_bidirectional());
    }
}
