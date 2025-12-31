//! Generic Query System
//!
//! Provides a flexible query interface for Entry-based databases using
//! Entry patterns as query specifications. Works with any Build implementor
//! without requiring type-specific query methods.
//!
//! # Query Specification
//!
//! Queries are defined as Entry nodes with matching criteria:
//! - `kind`: Match entries of this kind
//! - `name`: Match entries with this name
//! - `category` attr: Match entries in this category
//! - Other attrs: Match entries with these attribute values

use crate::db::tree::Tree;
use crate::db::web::Entry;
use std::collections::HashMap;

// ============================================================================
// QuerySpec - Entry-based query specification
// ============================================================================

/// A query specification built from Entry patterns.
/// Allows building complex queries that can be stored and reused.
#[derive(Debug, Clone)]
pub struct QuerySpec {
    /// Kind to match (optional)
    pub kind: Option<String>,
    /// Name to match (optional)
    pub name: Option<String>,
    /// Category to match (optional)
    pub category: Option<String>,
    /// Attributes to match (all must match)
    pub attrs: HashMap<String, AttrMatch>,
    /// Range constraint (optional)
    pub range: Option<std::ops::Range<usize>>,
    /// Custom predicate name (if stored)
    pub predicate_name: Option<String>,
}

/// Attribute matching criteria
#[derive(Debug, Clone)]
pub enum AttrMatch {
    /// Exact string match
    Equals(String),
    /// String contains pattern
    Contains(String),
    /// Boolean value
    Bool(bool),
    /// Numeric comparison
    Number(i64),
    /// Presence check (has attribute)
    Exists,
    /// Absence check (does not have attribute)
    NotExists,
}

impl QuerySpec {
    /// Create an empty query (matches everything)
    pub fn new() -> Self {
        Self {
            kind: None,
            name: None,
            category: None,
            attrs: HashMap::new(),
            range: None,
            predicate_name: None,
        }
    }

    /// Create a query matching a specific kind
    pub fn kind(kind: impl Into<String>) -> Self {
        Self {
            kind: Some(kind.into()),
            ..Self::new()
        }
    }

    /// Create a query matching a specific name
    pub fn name(name: impl Into<String>) -> Self {
        Self {
            name: Some(name.into()),
            ..Self::new()
        }
    }

    /// Add kind filter
    pub fn with_kind(mut self, kind: impl Into<String>) -> Self {
        self.kind = Some(kind.into());
        self
    }

    /// Add name filter
    pub fn with_name(mut self, name: impl Into<String>) -> Self {
        self.name = Some(name.into());
        self
    }

    /// Add category filter
    pub fn with_category(mut self, category: impl Into<String>) -> Self {
        self.category = Some(category.into());
        self
    }

    /// Add attribute equals filter
    pub fn with_attr(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.attrs
            .insert(key.into(), AttrMatch::Equals(value.into()));
        self
    }

    /// Add attribute contains filter
    pub fn with_attr_contains(
        mut self,
        key: impl Into<String>,
        pattern: impl Into<String>,
    ) -> Self {
        self.attrs
            .insert(key.into(), AttrMatch::Contains(pattern.into()));
        self
    }

    /// Add boolean attribute filter
    pub fn with_bool_attr(mut self, key: impl Into<String>, value: bool) -> Self {
        self.attrs.insert(key.into(), AttrMatch::Bool(value));
        self
    }

    /// Add attribute exists filter
    pub fn with_attr_exists(mut self, key: impl Into<String>) -> Self {
        self.attrs.insert(key.into(), AttrMatch::Exists);
        self
    }

    /// Add range filter
    pub fn with_range(mut self, range: std::ops::Range<usize>) -> Self {
        self.range = Some(range);
        self
    }

    /// Set predicate name for storage
    pub fn named(mut self, name: impl Into<String>) -> Self {
        self.predicate_name = Some(name.into());
        self
    }

    /// Check if an Entry matches this query
    pub fn matches(&self, entry: &Entry) -> bool {
        // Check kind
        if let Some(ref query_kind) = self.kind {
            match entry.kind() {
                Some(k) if k == query_kind => {}
                _ => return false,
            }
        }

        // Check name
        if let Some(ref query_name) = self.name {
            match entry.name() {
                Some(n) if n == query_name => {}
                _ => return false,
            }
        }

        // Check category
        if let Some(ref query_cat) = self.category {
            match entry.get_string_attr("category") {
                Some(c) if c == query_cat => {}
                _ => return false,
            }
        }

        // Check attributes
        for (key, matcher) in &self.attrs {
            match matcher {
                AttrMatch::Equals(val) => {
                    if entry.get_string_attr(key) != Some(val.as_str()) {
                        return false;
                    }
                }
                AttrMatch::Contains(pattern) => match entry.get_string_attr(key) {
                    Some(v) if v.contains(pattern.as_str()) => {}
                    _ => return false,
                },
                AttrMatch::Bool(val) => {
                    if entry.get_bool_attr(key) != Some(*val) {
                        return false;
                    }
                }
                AttrMatch::Number(val) => {
                    if entry.get_i64_attr(key) != Some(*val) {
                        return false;
                    }
                }
                AttrMatch::Exists => {
                    if entry.get_attr(key).is_none() {
                        return false;
                    }
                }
                AttrMatch::NotExists => {
                    if entry.get_attr(key).is_some() {
                        return false;
                    }
                }
            }
        }

        // Check range
        if let Some(ref range) = self.range {
            match entry.source_range() {
                Some(r) if r.start >= range.start && r.end <= range.end => {}
                Some(_) => return false,
                None => return false,
            }
        }

        true
    }

    /// Convert to an Entry for storage
    pub fn to_entry(&self) -> Entry {
        let mut node = Entry::node(
            "QuerySpec",
            self.predicate_name.as_deref().unwrap_or("unnamed"),
        );

        if let Some(ref k) = self.kind {
            node.set_attr("query_kind", Entry::string(k));
        }
        if let Some(ref n) = self.name {
            node.set_attr("query_name", Entry::string(n));
        }
        if let Some(ref c) = self.category {
            node.set_attr("query_category", Entry::string(c));
        }
        if let Some(ref r) = self.range {
            node.set_attr("range_start", Entry::usize(r.start));
            node.set_attr("range_end", Entry::usize(r.end));
        }

        // Store attr matchers
        for (key, matcher) in &self.attrs {
            let matcher_entry = match matcher {
                AttrMatch::Equals(v) => Entry::string(format!("eq:{}", v)),
                AttrMatch::Contains(v) => Entry::string(format!("contains:{}", v)),
                AttrMatch::Bool(v) => Entry::string(format!("bool:{}", v)),
                AttrMatch::Number(v) => Entry::string(format!("num:{}", v)),
                AttrMatch::Exists => Entry::string("exists"),
                AttrMatch::NotExists => Entry::string("not_exists"),
            };
            node.set_attr(&format!("attr_{}", key), matcher_entry);
        }

        node
    }

    /// Create from an Entry (reverse of to_entry)
    pub fn from_entry(entry: &Entry) -> Option<Self> {
        if entry.kind() != Some("QuerySpec") {
            return None;
        }

        let mut spec = Self::new();
        spec.predicate_name = entry.name().map(|s| s.to_string());
        spec.kind = entry.get_string_attr("query_kind").map(|s| s.to_string());
        spec.name = entry.get_string_attr("query_name").map(|s| s.to_string());
        spec.category = entry
            .get_string_attr("query_category")
            .map(|s| s.to_string());

        if let (Some(start), Some(end)) = (
            entry.get_usize_attr("range_start"),
            entry.get_usize_attr("range_end"),
        ) {
            spec.range = Some(start..end);
        }

        // Parse attr matchers (attrs starting with "attr_")
        if let Entry::Node { attrs, .. } = entry {
            for (key, value) in attrs {
                if let Some(attr_key) = key.strip_prefix("attr_") {
                    if let Some(matcher_str) = value.as_str() {
                        let matcher = if matcher_str == "exists" {
                            AttrMatch::Exists
                        } else if matcher_str == "not_exists" {
                            AttrMatch::NotExists
                        } else if let Some(v) = matcher_str.strip_prefix("eq:") {
                            AttrMatch::Equals(v.to_string())
                        } else if let Some(v) = matcher_str.strip_prefix("contains:") {
                            AttrMatch::Contains(v.to_string())
                        } else if let Some(v) = matcher_str.strip_prefix("bool:") {
                            AttrMatch::Bool(v == "true")
                        } else if let Some(v) = matcher_str.strip_prefix("num:") {
                            AttrMatch::Number(v.parse().unwrap_or(0))
                        } else {
                            continue;
                        };
                        spec.attrs.insert(attr_key.to_string(), matcher);
                    }
                }
            }
        }

        Some(spec)
    }
}

impl Default for QuerySpec {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// Query - Generic query interface
// ============================================================================

/// Generic query interface for entry trees.
/// Works with any Entry collection without type-specific methods.
pub struct Query<'a> {
    tree: &'a Tree,
}

impl<'a> Query<'a> {
    /// Create a new query interface.
    pub fn new(tree: &'a Tree) -> Self {
        Self { tree }
    }

    // ========================================================================
    // Generic Query Methods
    // ========================================================================

    /// Find all entries matching a QuerySpec
    pub fn find(&self, spec: &QuerySpec) -> Vec<&'a Entry> {
        self.tree.iter().filter(|e| spec.matches(e)).collect()
    }

    /// Find first entry matching a QuerySpec
    pub fn find_first(&self, spec: &QuerySpec) -> Option<&'a Entry> {
        self.tree.iter().find(|e| spec.matches(e))
    }

    /// Find all entries matching a predicate function
    pub fn find_where<F>(&self, predicate: F) -> Vec<&'a Entry>
    where
        F: Fn(&Entry) -> bool,
    {
        self.tree.iter().filter(|n| predicate(n)).collect()
    }

    /// Find first entry matching a predicate function
    pub fn find_first_where<F>(&self, predicate: F) -> Option<&'a Entry>
    where
        F: Fn(&Entry) -> bool,
    {
        self.tree.iter().find(|n| predicate(n))
    }

    // ========================================================================
    // Core Query Methods (by id, kind, data)
    // ========================================================================

    /// Find all entries by kind
    pub fn by_kind(&self, kind: &str) -> Vec<&'a Entry> {
        self.find(&QuerySpec::kind(kind))
    }

    /// Find entry by kind and name
    pub fn by_kind_and_name(&self, kind: &str, name: &str) -> Option<&'a Entry> {
        self.find_first(&QuerySpec::kind(kind).with_name(name))
    }

    /// Find all entries by name (across all kinds)
    pub fn by_name(&self, name: &str) -> Vec<&'a Entry> {
        self.find(&QuerySpec::name(name))
    }

    /// Find all entries in a category
    pub fn by_category(&self, category: &str) -> Vec<&'a Entry> {
        self.find(&QuerySpec::new().with_category(category))
    }

    /// Find entries with a specific attribute value
    pub fn by_attr(&self, key: &str, value: &str) -> Vec<&'a Entry> {
        self.find(&QuerySpec::new().with_attr(key, value))
    }

    /// Find entries with a boolean attribute
    pub fn by_bool_attr(&self, key: &str, value: bool) -> Vec<&'a Entry> {
        self.find(&QuerySpec::new().with_bool_attr(key, value))
    }

    /// Find entries that have a specific attribute (any value)
    pub fn with_attr(&self, key: &str) -> Vec<&'a Entry> {
        self.find(&QuerySpec::new().with_attr_exists(key))
    }

    // ========================================================================
    // Range Queries
    // ========================================================================

    /// Find entries within a source range
    pub fn in_range(&self, start: usize, end: usize) -> Vec<&'a Entry> {
        self.find(&QuerySpec::new().with_range(start..end))
    }

    /// Find entries at a specific position
    pub fn at_position(&self, pos: usize) -> Vec<&'a Entry> {
        self.find_where(|n| {
            if let Some(range) = n.source_range() {
                range.start <= pos && pos < range.end
            } else {
                false
            }
        })
    }

    // ========================================================================
    // Counting
    // ========================================================================

    /// Count entries matching a QuerySpec
    pub fn count(&self, spec: &QuerySpec) -> usize {
        self.tree.iter().filter(|e| spec.matches(e)).count()
    }

    /// Count entries by kind
    pub fn count_kind(&self, kind: &str) -> usize {
        self.by_kind(kind).len()
    }

    // ========================================================================
    // Tree Access
    // ========================================================================

    /// Get all entries (iterate)
    pub fn all(&self) -> impl Iterator<Item = &'a Entry> {
        self.tree.iter()
    }

    /// Get count of all entries
    pub fn len(&self) -> usize {
        self.tree.len()
    }

    /// Check if tree is empty
    pub fn is_empty(&self) -> bool {
        self.tree.is_empty()
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::db::node::NodeKind;
    use crate::db::tree::Tree;

    #[test]
    fn test_query_spec_kind() {
        let spec = QuerySpec::kind("Function");

        let func = Entry::node("Function", "test_func");
        let struc = Entry::node("Struct", "TestStruct");

        assert!(spec.matches(&func));
        assert!(!spec.matches(&struc));
    }

    #[test]
    fn test_query_spec_name() {
        let spec = QuerySpec::kind("Function").with_name("calculate");

        let func1 = Entry::node("Function", "calculate");
        let func2 = Entry::node("Function", "process");

        assert!(spec.matches(&func1));
        assert!(!spec.matches(&func2));
    }

    #[test]
    fn test_query_spec_attr() {
        let spec = QuerySpec::new().with_bool_attr("is_static", true);

        let mut static_fn = Entry::node("Function", "static_fn");
        static_fn.set_attr("is_static", Entry::bool(true));

        let normal_fn = Entry::node("Function", "normal_fn");

        assert!(spec.matches(&static_fn));
        assert!(!spec.matches(&normal_fn));
    }

    #[test]
    fn test_query_spec_serialization() {
        let spec = QuerySpec::kind("Function")
            .with_name("test")
            .with_bool_attr("is_static", true)
            .named("static_functions");

        let entry = spec.to_entry();
        let restored = QuerySpec::from_entry(&entry).unwrap();

        assert_eq!(restored.kind, Some("Function".to_string()));
        assert_eq!(restored.name, Some("test".to_string()));
        assert_eq!(
            restored.predicate_name,
            Some("static_functions".to_string())
        );
    }

    #[test]
    fn test_query_by_kind() {
        let tree = Tree::new()
            .with_function("func1", 0..50, |f| f)
            .with_function("func2", 51..100, |f| f)
            .with_struct("MyStruct", 101..150, |s| s);

        let query = Query::new(&tree);
        assert_eq!(query.by_kind(NodeKind::Function.into()).len(), 2);
        assert_eq!(query.by_kind(NodeKind::Struct.into()).len(), 1);
    }

    #[test]
    fn test_query_by_kind_and_name() {
        let tree = Tree::new()
            .with_function("calculate", 0..50, |f| f)
            .with_function("process", 51..100, |f| f);

        let query = Query::new(&tree);
        assert!(
            query
                .by_kind_and_name(NodeKind::Function.into(), "calculate")
                .is_some()
        );
        assert!(
            query
                .by_kind_and_name(NodeKind::Function.into(), "nonexistent")
                .is_none()
        );
    }

    #[test]
    fn test_query_with_bool_attr() {
        let tree = Tree::new()
            .with_function("static_fn", 0..50, |f| f.is_static())
            .with_function("normal_fn", 51..100, |f| f);

        let query = Query::new(&tree);
        let static_fns = query.by_bool_attr("is_static", true);
        assert_eq!(static_fns.len(), 1);
    }

    #[test]
    fn test_query_in_range() {
        let tree = Tree::new()
            .with_function("f1", 0..50, |f| f)
            .with_function("f2", 51..100, |f| f)
            .with_function("f3", 101..150, |f| f);

        let query = Query::new(&tree);
        let in_range = query.in_range(0, 100);
        assert_eq!(in_range.len(), 2);
    }

    #[test]
    fn test_query_find_where() {
        let tree = Tree::new()
            .with_function("func1", 0..50, |f| f)
            .with_function("func2", 51..100, |f| f);

        let query = Query::new(&tree);
        let results = query.find_where(|e| e.name() == Some("func1"));
        assert_eq!(results.len(), 1);
    }
}
