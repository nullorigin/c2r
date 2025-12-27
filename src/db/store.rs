//! Generic Storage Traits and Structures
//!
//! Provides reusable database/storage patterns for the DB framework.
//! These traits and structures form the foundation for specialized databases.

use std::collections::HashMap;
use std::hash::Hash;
use std::time::SystemTime;

// ============================================================================
// Core Storage Traits
// ============================================================================

/// A generic store that maps keys to values
pub trait Store<K, V> {
    /// Get a value by key
    fn get(&self, key: &K) -> Option<&V>;

    /// Get a mutable value by key
    fn get_mut(&mut self, key: &K) -> Option<&mut V>;

    /// Insert a value, returning the old value if present
    fn insert(&mut self, key: K, value: V) -> Option<V>;

    /// Remove a value by key
    fn remove(&mut self, key: &K) -> Option<V>;

    /// Check if key exists
    fn contains(&self, key: &K) -> bool;

    /// Get number of entries
    fn len(&self) -> usize;

    /// Check if empty
    fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Clear all entries
    fn clear(&mut self);
}

/// A store that supports querying
pub trait Queryable<Q, V> {
    /// Find all values matching a query
    fn query(&self, query: &Q) -> Vec<&V>;

    /// Find first value matching a query
    fn query_first(&self, query: &Q) -> Option<&V> {
        self.query(query).into_iter().next()
    }

    /// Count values matching a query
    fn query_count(&self, query: &Q) -> usize {
        self.query(query).len()
    }
}

/// A store that supports indexing for fast lookups
pub trait Indexed<K, V> {
    /// Add an index on a field
    fn add_index(&mut self, field: &str);

    /// Remove an index
    fn remove_index(&mut self, field: &str);

    /// Get values by indexed field
    fn get_by_index(&self, field: &str, value: &str) -> Vec<&V>;
}

/// A store that tracks statistics
pub trait StatsTracking {
    type Stats;

    /// Get current statistics
    fn stats(&self) -> &Self::Stats;

    /// Reset statistics
    fn reset_stats(&mut self);
}

// ============================================================================
// Record with Metadata
// ============================================================================

/// A record with common metadata fields
#[derive(Debug, Clone, PartialEq)]
pub struct Record<T> {
    /// Unique identifier
    pub id: String,

    /// The actual data
    pub data: T,

    /// Creation timestamp
    pub created_at: SystemTime,

    /// Last modification timestamp
    pub modified_at: SystemTime,

    /// Additional metadata
    pub metadata: HashMap<String, String>,

    /// Tags for categorization
    pub tags: Vec<String>,
}

impl<T> Record<T> {
    /// Create a new record
    pub fn new(id: impl Into<String>, data: T) -> Self {
        let now = SystemTime::now();
        Self {
            id: id.into(),
            data,
            created_at: now,
            modified_at: now,
            metadata: HashMap::new(),
            tags: Vec::new(),
        }
    }

    /// Add metadata
    pub fn with_metadata(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.metadata.insert(key.into(), value.into());
        self
    }

    /// Add a tag
    pub fn with_tag(mut self, tag: impl Into<String>) -> Self {
        self.tags.push(tag.into());
        self
    }

    /// Add multiple tags
    pub fn with_tags(mut self, tags: impl IntoIterator<Item = impl Into<String>>) -> Self {
        self.tags.extend(tags.into_iter().map(|t| t.into()));
        self
    }

    /// Mark as modified
    pub fn touch(&mut self) {
        self.modified_at = SystemTime::now();
    }

    /// Update the data
    pub fn update(&mut self, data: T) {
        self.data = data;
        self.touch();
    }

    /// Check if has a specific tag
    pub fn has_tag(&self, tag: &str) -> bool {
        self.tags.iter().any(|t| t == tag)
    }

    /// Get metadata value
    pub fn get_metadata(&self, key: &str) -> Option<&String> {
        self.metadata.get(key)
    }
}

// ============================================================================
// HashMap-based Store Implementation
// ============================================================================

/// A simple HashMap-based store
#[derive(Debug, Clone)]
pub struct HashStore<K: Eq + Hash, V> {
    data: HashMap<K, V>,
    indices: HashMap<String, HashMap<String, Vec<K>>>,
}

impl<K: Eq + Hash + Clone, V> HashStore<K, V> {
    pub fn new() -> Self {
        Self {
            data: HashMap::new(),
            indices: HashMap::new(),
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            data: HashMap::with_capacity(capacity),
            indices: HashMap::new(),
        }
    }

    /// Iterate over all values
    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.data.values()
    }

    /// Iterate over all key-value pairs
    pub fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        self.data.iter()
    }

    /// Get all keys
    pub fn keys(&self) -> impl Iterator<Item = &K> {
        self.data.keys()
    }
}

impl<K: Eq + Hash + Clone, V> Default for HashStore<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K: Eq + Hash + Clone, V> Store<K, V> for HashStore<K, V> {
    fn get(&self, key: &K) -> Option<&V> {
        self.data.get(key)
    }

    fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        self.data.get_mut(key)
    }

    fn insert(&mut self, key: K, value: V) -> Option<V> {
        self.data.insert(key, value)
    }

    fn remove(&mut self, key: &K) -> Option<V> {
        self.data.remove(key)
    }

    fn contains(&self, key: &K) -> bool {
        self.data.contains_key(key)
    }

    fn len(&self) -> usize {
        self.data.len()
    }

    fn clear(&mut self) {
        self.data.clear();
        self.indices.clear();
    }
}

// ============================================================================
// Grouped Store
// ============================================================================

/// A store that groups values by category
#[derive(Debug, Clone)]
pub struct GroupedStore<V> {
    /// All values by ID
    values: HashMap<String, V>,

    /// Values grouped by category
    groups: HashMap<String, Vec<String>>,
}

impl<V> GroupedStore<V> {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            groups: HashMap::new(),
        }
    }

    /// Add a value to a group
    pub fn add(&mut self, id: impl Into<String>, group: impl Into<String>, value: V) {
        let id = id.into();
        let group = group.into();

        self.groups.entry(group).or_default().push(id.clone());
        self.values.insert(id, value);
    }

    /// Get a value by ID
    pub fn get(&self, id: &str) -> Option<&V> {
        self.values.get(id)
    }

    /// Get all values in a group
    pub fn get_group(&self, group: &str) -> Vec<&V> {
        self.groups
            .get(group)
            .map(|ids| ids.iter().filter_map(|id| self.values.get(id)).collect())
            .unwrap_or_default()
    }

    /// Get all group names
    pub fn groups(&self) -> impl Iterator<Item = &String> {
        self.groups.keys()
    }

    /// Get count in a group
    pub fn group_count(&self, group: &str) -> usize {
        self.groups.get(group).map(|v| v.len()).unwrap_or(0)
    }

    /// Total value count
    pub fn len(&self) -> usize {
        self.values.len()
    }

    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    /// Clear all
    pub fn clear(&mut self) {
        self.values.clear();
        self.groups.clear();
    }
}

impl<V> Default for GroupedStore<V> {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// Priority Queue Store
// ============================================================================

/// A store with priority ordering
#[derive(Debug, Clone)]
pub struct PriorityStore<V> {
    /// Values by ID
    values: HashMap<String, V>,

    /// Priority index: (priority, id)
    priority_index: Vec<(i32, String)>,

    /// Whether index needs rebuilding
    dirty: bool,
}

impl<V> PriorityStore<V> {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            priority_index: Vec::new(),
            dirty: false,
        }
    }

    /// Add a value with priority
    pub fn add(&mut self, id: impl Into<String>, priority: i32, value: V) {
        let id = id.into();
        self.priority_index.push((priority, id.clone()));
        self.values.insert(id, value);
        self.dirty = true;
    }

    /// Get by ID
    pub fn get(&self, id: &str) -> Option<&V> {
        self.values.get(id)
    }

    /// Get all values in priority order (highest first)
    pub fn by_priority(&mut self) -> Vec<&V> {
        if self.dirty {
            self.priority_index.sort_by(|a, b| b.0.cmp(&a.0));
            self.dirty = false;
        }

        self.priority_index
            .iter()
            .filter_map(|(_, id)| self.values.get(id))
            .collect()
    }

    /// Get top N by priority
    pub fn top(&mut self, n: usize) -> Vec<&V> {
        self.by_priority().into_iter().take(n).collect()
    }

    /// Get count
    pub fn len(&self) -> usize {
        self.values.len()
    }

    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }
}

impl<V> Default for PriorityStore<V> {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// Indexed Store (Combines Grouped + Priority + Multi-Index)
// ============================================================================

/// A versatile store with grouping, priority ordering, and multi-index support.
/// This consolidates the patterns from GroupedStore and PriorityStore into a
/// single, flexible storage structure suitable for PatternDB, Annotations, etc.
#[derive(Debug, Clone)]
pub struct IndexedStore<V> {
    /// All values by ID
    values: HashMap<String, V>,
    /// Values grouped by category/group
    groups: HashMap<String, Vec<String>>,
    /// Priority index: (priority, id) - sorted highest first
    priority_index: Vec<(i32, String)>,
    /// Additional string indices
    indices: HashMap<String, HashMap<String, Vec<String>>>,
    /// Whether priority index needs rebuilding
    dirty: bool,
}

impl<V> IndexedStore<V> {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            groups: HashMap::new(),
            priority_index: Vec::new(),
            indices: HashMap::new(),
            dirty: false,
        }
    }

    /// Add a value with ID, group, and priority
    pub fn add(
        &mut self,
        id: impl Into<String>,
        group: impl Into<String>,
        priority: i32,
        value: V,
    ) {
        let id = id.into();
        let group = group.into();

        self.groups.entry(group).or_default().push(id.clone());
        self.priority_index.push((priority, id.clone()));
        self.values.insert(id, value);
        self.dirty = true;
    }

    /// Add with just ID (no group, default priority)
    pub fn add_simple(&mut self, id: impl Into<String>, value: V) {
        let id = id.into();
        self.priority_index.push((0, id.clone()));
        self.values.insert(id, value);
        self.dirty = true;
    }

    /// Get value by ID
    pub fn get(&self, id: &str) -> Option<&V> {
        self.values.get(id)
    }

    /// Get mutable value by ID
    pub fn get_mut(&mut self, id: &str) -> Option<&mut V> {
        self.values.get_mut(id)
    }

    /// Get all values in a group
    pub fn get_group(&self, group: &str) -> Vec<&V> {
        self.groups
            .get(group)
            .map(|ids| ids.iter().filter_map(|id| self.values.get(id)).collect())
            .unwrap_or_default()
    }

    /// Get all group names
    pub fn group_names(&self) -> impl Iterator<Item = &String> {
        self.groups.keys()
    }

    /// Get count in a group
    pub fn group_count(&self, group: &str) -> usize {
        self.groups.get(group).map(|v| v.len()).unwrap_or(0)
    }

    /// Get all values sorted by priority (highest first)
    pub fn by_priority(&mut self) -> Vec<&V> {
        if self.dirty {
            self.priority_index.sort_by(|a, b| b.0.cmp(&a.0));
            self.dirty = false;
        }
        self.priority_index
            .iter()
            .filter_map(|(_, id)| self.values.get(id))
            .collect()
    }

    /// Get top N by priority
    pub fn top(&mut self, n: usize) -> Vec<&V> {
        self.by_priority().into_iter().take(n).collect()
    }

    /// Add a secondary index on a field
    pub fn add_to_index(
        &mut self,
        index_name: &str,
        key: impl Into<String>,
        id: impl Into<String>,
    ) {
        self.indices
            .entry(index_name.to_string())
            .or_default()
            .entry(key.into())
            .or_default()
            .push(id.into());
    }

    /// Get values by secondary index
    pub fn get_by_index(&self, index_name: &str, key: &str) -> Vec<&V> {
        self.indices
            .get(index_name)
            .and_then(|idx| idx.get(key))
            .map(|ids| ids.iter().filter_map(|id| self.values.get(id)).collect())
            .unwrap_or_default()
    }

    /// Remove a value by ID
    pub fn remove(&mut self, id: &str) -> Option<V> {
        if let Some(value) = self.values.remove(id) {
            // Clean up from groups
            for group_ids in self.groups.values_mut() {
                group_ids.retain(|i| i != id);
            }
            self.groups.retain(|_, v| !v.is_empty());
            // Clean up from priority index
            self.priority_index.retain(|(_, i)| i != id);
            // Clean up from indices
            for idx in self.indices.values_mut() {
                for ids in idx.values_mut() {
                    ids.retain(|i| i != id);
                }
            }
            Some(value)
        } else {
            None
        }
    }

    /// Total count
    pub fn len(&self) -> usize {
        self.values.len()
    }

    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    /// Iterate over all values
    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.values.values()
    }

    /// Iterate over all (id, value) pairs
    pub fn iter(&self) -> impl Iterator<Item = (&String, &V)> {
        self.values.iter()
    }

    /// Get all IDs
    pub fn ids(&self) -> impl Iterator<Item = &String> {
        self.values.keys()
    }

    /// Clear all data
    pub fn clear(&mut self) {
        self.values.clear();
        self.groups.clear();
        self.priority_index.clear();
        self.indices.clear();
        self.dirty = false;
    }

    /// Rebuild priority index from scratch using a priority extractor function
    pub fn rebuild_priority_index<F>(&mut self, priority_fn: F)
    where
        F: Fn(&V) -> i32,
    {
        self.priority_index = self
            .values
            .iter()
            .map(|(id, v)| (priority_fn(v), id.clone()))
            .collect();
        self.priority_index.sort_by(|a, b| b.0.cmp(&a.0));
        self.dirty = false;
    }
}

impl<V> Default for IndexedStore<V> {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// Statistics Tracking
// ============================================================================

/// Generic statistics structure
#[derive(Debug, Clone, Default)]
pub struct StoreStats {
    /// Counter values
    pub counters: HashMap<String, u64>,

    /// Gauge values (current value)
    pub gauges: HashMap<String, f64>,

    /// Histogram data
    pub histograms: HashMap<String, Vec<f64>>,

    /// Timestamps
    pub timestamps: HashMap<String, SystemTime>,
}

impl StoreStats {
    pub fn new() -> Self {
        Self::default()
    }

    /// Increment a counter
    pub fn inc(&mut self, name: &str) {
        *self.counters.entry(name.to_string()).or_default() += 1;
    }

    /// Increment by amount
    pub fn inc_by(&mut self, name: &str, amount: u64) {
        *self.counters.entry(name.to_string()).or_default() += amount;
    }

    /// Get counter value
    pub fn counter(&self, name: &str) -> u64 {
        self.counters.get(name).copied().unwrap_or(0)
    }

    /// Set a gauge value
    pub fn set_gauge(&mut self, name: &str, value: f64) {
        self.gauges.insert(name.to_string(), value);
    }

    /// Get gauge value
    pub fn gauge(&self, name: &str) -> f64 {
        self.gauges.get(name).copied().unwrap_or(0.0)
    }

    /// Record a histogram value
    pub fn record(&mut self, name: &str, value: f64) {
        self.histograms
            .entry(name.to_string())
            .or_default()
            .push(value);
    }

    /// Get histogram average
    pub fn histogram_avg(&self, name: &str) -> f64 {
        self.histograms
            .get(name)
            .map(|v| {
                if v.is_empty() {
                    0.0
                } else {
                    v.iter().sum::<f64>() / v.len() as f64
                }
            })
            .unwrap_or(0.0)
    }

    /// Mark a timestamp
    pub fn mark(&mut self, name: &str) {
        self.timestamps.insert(name.to_string(), SystemTime::now());
    }

    /// Get a timestamp
    pub fn timestamp(&self, name: &str) -> Option<&SystemTime> {
        self.timestamps.get(name)
    }

    /// Reset all statistics
    pub fn reset(&mut self) {
        self.counters.clear();
        self.gauges.clear();
        self.histograms.clear();
        self.timestamps.clear();
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_record() {
        let record = Record::new("test-1", "data")
            .with_metadata("key", "value")
            .with_tag("important");

        assert_eq!(record.id, "test-1");
        assert_eq!(record.data, "data");
        assert!(record.has_tag("important"));
        assert_eq!(record.get_metadata("key"), Some(&"value".to_string()));
    }

    #[test]
    fn test_hash_store() {
        let mut store: HashStore<String, i32> = HashStore::new();
        store.insert("a".to_string(), 1);
        store.insert("b".to_string(), 2);

        assert_eq!(store.get(&"a".to_string()), Some(&1));
        assert_eq!(store.len(), 2);
        assert!(store.contains(&"b".to_string()));
    }

    #[test]
    fn test_grouped_store() {
        let mut store: GroupedStore<String> = GroupedStore::new();
        store.add("item1", "group_a", "value1".to_string());
        store.add("item2", "group_a", "value2".to_string());
        store.add("item3", "group_b", "value3".to_string());

        assert_eq!(store.group_count("group_a"), 2);
        assert_eq!(store.group_count("group_b"), 1);
        assert_eq!(store.len(), 3);
    }

    #[test]
    fn test_priority_store() {
        let mut store: PriorityStore<String> = PriorityStore::new();
        store.add("low", 1, "low".to_string());
        store.add("high", 100, "high".to_string());
        store.add("med", 50, "med".to_string());

        let ordered: Vec<_> = store.by_priority();
        assert_eq!(ordered[0], &"high".to_string());
        assert_eq!(ordered[1], &"med".to_string());
        assert_eq!(ordered[2], &"low".to_string());
    }

    #[test]
    fn test_stats() {
        let mut stats = StoreStats::new();
        stats.inc("requests");
        stats.inc("requests");
        stats.set_gauge("cpu", 75.5);
        stats.record("latency", 10.0);
        stats.record("latency", 20.0);

        assert_eq!(stats.counter("requests"), 2);
        assert_eq!(stats.gauge("cpu"), 75.5);
        assert_eq!(stats.histogram_avg("latency"), 15.0);
    }

    #[test]
    fn test_indexed_store() {
        let mut store: IndexedStore<String> = IndexedStore::new();
        store.add("pattern1", "function", 100, "fn_def".to_string());
        store.add("pattern2", "function", 50, "fn_decl".to_string());
        store.add("pattern3", "struct", 75, "struct_def".to_string());

        // Test get
        assert_eq!(store.get("pattern1"), Some(&"fn_def".to_string()));

        // Test group
        let functions = store.get_group("function");
        assert_eq!(functions.len(), 2);

        // Test priority ordering
        let by_priority = store.by_priority();
        assert_eq!(by_priority[0], &"fn_def".to_string()); // priority 100
        assert_eq!(by_priority[1], &"struct_def".to_string()); // priority 75
        assert_eq!(by_priority[2], &"fn_decl".to_string()); // priority 50

        // Test remove
        assert!(store.remove("pattern2").is_some());
        assert_eq!(store.len(), 2);
        assert_eq!(store.get_group("function").len(), 1);
    }
}
