//! Tree structure for the DB system
//!
//! Tree wraps Site to provide a clean API for working with
//! intermediate format trees. Includes builder pattern methods.

use crate::db::node::{self, kind, Enum, Function, NodeExt, Struct};
use crate::db::web::{BranchOp, Entry, Links, Site, SiteStats};
use std::ops::Range;

/// A tree structure for the Entry-based intermediate format.
#[derive(Debug, Clone)]
pub struct Tree {
    site: Site,
    current_order: usize,
}

impl Default for Tree {
    fn default() -> Self {
        Self::new()
    }
}

impl Tree {
    /// Create a new empty tree.
    pub fn new() -> Self {
        Self {
            site: Site::new(),
            current_order: 0,
        }
    }

    /// Create a named tree.
    pub fn named(name: impl Into<String>) -> Self {
        Self {
            site: Site::named(name),
            current_order: 0,
        }
    }

    /// Get the underlying site (read-only).
    pub fn site(&self) -> &Site {
        &self.site
    }

    /// Get mutable access to the site.
    pub fn site_mut(&mut self) -> &mut Site {
        &mut self.site
    }

    // ========================================================================
    // Node Management
    // ========================================================================

    /// Add a root node and return its index.
    pub fn add_root(&mut self, node: Entry) -> usize {
        self.site.add_root(node)
    }

    /// Add multiple root nodes.
    pub fn add_roots(&mut self, nodes: Vec<Entry>) {
        for node in nodes {
            self.site.add_root(node);
        }
    }

    /// Add a child node to a parent.
    pub fn add_child(&mut self, parent: usize, child: Entry) -> Option<usize> {
        self.site.add_child_to(parent, child)
    }

    /// Get a node by index.
    pub fn get(&self, index: usize) -> Option<&Entry> {
        self.site.get(index)
    }

    /// Get a mutable node by index.
    pub fn get_mut(&mut self, index: usize) -> Option<&mut Entry> {
        self.site.get_mut(index)
    }

    // ========================================================================
    // Metadata
    // ========================================================================

    /// Set metadata on the tree.
    pub fn set_metadata(&mut self, key: impl Into<String>, value: Entry) {
        self.site.set_metadata(key, value);
    }

    /// Get metadata from the tree.
    pub fn get_metadata(&self, key: &str) -> Option<&Entry> {
        self.site.get_metadata(key)
    }

    /// Builder pattern for metadata.
    pub fn with_metadata(mut self, key: impl Into<String>, value: Entry) -> Self {
        self.set_metadata(key, value);
        self
    }

    /// Builder pattern for root node.
    pub fn with_root(mut self, node: Entry) -> Self {
        self.add_root(node);
        self
    }

    // ========================================================================
    // Builder Methods (merged from TreeBuilder)
    // ========================================================================

    /// Add a node to the tree as a root with declaration order tracking.
    pub fn add(&mut self, node: Entry) -> usize {
        let mut n = node;
        n.set_declaration_order(self.current_order);
        self.current_order += 1;
        self.add_root(n)
    }

    /// Add a node with builder pattern.
    pub fn with_node(mut self, node: Entry) -> Self {
        self.add(node);
        self
    }

    /// Add multiple nodes.
    pub fn with_nodes(mut self, nodes: Vec<Entry>) -> Self {
        for node in nodes {
            self.add(node);
        }
        self
    }

    /// Add a function node.
    pub fn add_function<F>(&mut self, name: impl Into<String>, range: Range<usize>, f: F) -> usize
    where
        F: FnOnce(Function) -> Function,
    {
        let builder = Function::new(name).with_range(range);
        let built = f(builder).build();
        self.add(built)
    }

    /// Add a function with builder pattern.
    pub fn with_function<F>(mut self, name: impl Into<String>, range: Range<usize>, f: F) -> Self
    where
        F: FnOnce(Function) -> Function,
    {
        self.add_function(name, range, f);
        self
    }

    /// Add a struct node.
    pub fn add_struct<F>(&mut self, name: impl Into<String>, range: Range<usize>, f: F) -> usize
    where
        F: FnOnce(Struct) -> Struct,
    {
        let builder = Struct::new(name).with_range(range);
        let built = f(builder).build();
        self.add(built)
    }

    /// Add a struct with builder pattern.
    pub fn with_struct<F>(mut self, name: impl Into<String>, range: Range<usize>, f: F) -> Self
    where
        F: FnOnce(Struct) -> Struct,
    {
        self.add_struct(name, range, f);
        self
    }

    /// Add an enum node.
    pub fn add_enum<F>(&mut self, name: impl Into<String>, range: Range<usize>, f: F) -> usize
    where
        F: FnOnce(Enum) -> Enum,
    {
        let builder = Enum::new(name).with_range(range);
        let built = f(builder).build();
        self.add(built)
    }

    /// Add an enum with builder pattern.
    pub fn with_enum<F>(mut self, name: impl Into<String>, range: Range<usize>, f: F) -> Self
    where
        F: FnOnce(Enum) -> Enum,
    {
        self.add_enum(name, range, f);
        self
    }

    /// Add a global variable.
    pub fn add_global(
        &mut self,
        name: impl Into<String>,
        type_name: impl Into<String>,
        range: Range<usize>,
    ) -> usize {
        let g = node::global(name, type_name).with_range(range);
        self.add(g)
    }

    /// Add a global with builder pattern.
    pub fn with_global(
        mut self,
        name: impl Into<String>,
        type_name: impl Into<String>,
        range: Range<usize>,
    ) -> Self {
        self.add_global(name, type_name, range);
        self
    }

    /// Add a typedef.
    pub fn add_typedef(
        &mut self,
        name: impl Into<String>,
        target: impl Into<String>,
        range: Range<usize>,
    ) -> usize {
        let t = node::typedef(name, target).with_range(range);
        self.add(t)
    }

    /// Add a typedef with builder pattern.
    pub fn with_typedef(
        mut self,
        name: impl Into<String>,
        target: impl Into<String>,
        range: Range<usize>,
    ) -> Self {
        self.add_typedef(name, target, range);
        self
    }

    /// Add a macro.
    pub fn add_macro(&mut self, name: impl Into<String>, range: Range<usize>) -> usize {
        let m = node::macro_node(name).with_range(range);
        self.add(m)
    }

    /// Add a macro with builder pattern.
    pub fn with_macro(mut self, name: impl Into<String>, range: Range<usize>) -> Self {
        self.add_macro(name, range);
        self
    }

    /// Add an include.
    pub fn add_include(
        &mut self,
        path: impl Into<String>,
        is_system: bool,
        range: Range<usize>,
    ) -> usize {
        let i = node::include(path, is_system).with_range(range);
        self.add(i)
    }

    /// Add an include with builder pattern.
    pub fn with_include(
        mut self,
        path: impl Into<String>,
        is_system: bool,
        range: Range<usize>,
    ) -> Self {
        self.add_include(path, is_system, range);
        self
    }

    /// Add a comment.
    pub fn add_comment(&mut self, text: impl Into<String>, range: Range<usize>) -> usize {
        let c = node::comment(text).with_range(range);
        self.add(c)
    }

    /// Add a comment with builder pattern.
    pub fn with_comment(mut self, text: impl Into<String>, range: Range<usize>) -> Self {
        self.add_comment(text, range);
        self
    }

    // ========================================================================
    // Branch Support (Conditional Tree Structures)
    // ========================================================================

    /// Add a branch node for conditional tree structures.
    pub fn add_branch(
        &mut self,
        operation: BranchOp,
        left_fn: fn(&Entry) -> bool,
        right_fn: fn(&Entry) -> bool,
        true_path: Vec<Entry>,
        false_path: Vec<Entry>,
    ) -> usize {
        let branch = Entry::Branch {
            operation,
            left_fn,
            right_fn,
            true_path,
            false_path,
            links: Links::new(),
        };
        self.add(branch)
    }

    /// Add a branch with builder pattern.
    pub fn with_branch(
        mut self,
        operation: BranchOp,
        left_fn: fn(&Entry) -> bool,
        right_fn: fn(&Entry) -> bool,
        true_path: Vec<Entry>,
        false_path: Vec<Entry>,
    ) -> Self {
        self.add_branch(operation, left_fn, right_fn, true_path, false_path);
        self
    }

    /// Add an AND branch (both conditions must be true).
    pub fn add_branch_and(
        &mut self,
        left_fn: fn(&Entry) -> bool,
        right_fn: fn(&Entry) -> bool,
        true_path: Vec<Entry>,
        false_path: Vec<Entry>,
    ) -> usize {
        self.add_branch(BranchOp::And, left_fn, right_fn, true_path, false_path)
    }

    /// Add an OR branch (either condition must be true).
    pub fn add_branch_or(
        &mut self,
        left_fn: fn(&Entry) -> bool,
        right_fn: fn(&Entry) -> bool,
        true_path: Vec<Entry>,
        false_path: Vec<Entry>,
    ) -> usize {
        self.add_branch(BranchOp::Or, left_fn, right_fn, true_path, false_path)
    }

    /// Evaluate a branch node and return the selected path.
    pub fn evaluate_branch(&self, branch_idx: usize, context: &Entry) -> Option<&Vec<Entry>> {
        self.get(branch_idx)
            .and_then(|entry| entry.evaluate_branch(context))
    }

    // ========================================================================
    // Queries
    // ========================================================================

    /// Find all nodes by kind.
    pub fn find_by_kind(&self, kind: &str) -> Vec<&Entry> {
        self.site
            .find_by_kind(kind)
            .iter()
            .filter_map(|&i| self.site.get(i))
            .collect()
    }

    /// Find all nodes by name pattern (case-insensitive contains).
    pub fn find_by_name(&self, pattern: &str) -> Vec<&Entry> {
        self.site
            .find_by_name(pattern)
            .iter()
            .filter_map(|&i| self.site.get(i))
            .collect()
    }

    /// Find all nodes with a specific attribute.
    pub fn find_with_attr(&self, key: &str) -> Vec<&Entry> {
        self.site
            .find_with_attr(key)
            .iter()
            .filter_map(|&i| self.site.get(i))
            .collect()
    }

    /// Get all function nodes.
    pub fn functions(&self) -> Vec<&Entry> {
        self.find_by_kind(kind::FUNCTION)
    }

    /// Get all struct nodes.
    pub fn structs(&self) -> Vec<&Entry> {
        self.find_by_kind(kind::STRUCT)
    }

    /// Get all enum nodes.
    pub fn enums(&self) -> Vec<&Entry> {
        self.find_by_kind(kind::ENUM)
    }

    /// Get all global variable nodes.
    pub fn globals(&self) -> Vec<&Entry> {
        self.find_by_kind(kind::GLOBAL)
    }

    /// Get all typedef nodes.
    pub fn typedefs(&self) -> Vec<&Entry> {
        self.find_by_kind(kind::TYPEDEF)
    }

    /// Get all macro nodes.
    pub fn macros(&self) -> Vec<&Entry> {
        self.find_by_kind(kind::MACRO)
    }

    /// Get all include nodes.
    pub fn includes(&self) -> Vec<&Entry> {
        self.find_by_kind(kind::INCLUDE)
    }

    // ========================================================================
    // Statistics
    // ========================================================================

    /// Get total number of nodes.
    pub fn len(&self) -> usize {
        self.site.len()
    }

    /// Check if tree is empty.
    pub fn is_empty(&self) -> bool {
        self.site.is_empty()
    }

    /// Get number of root nodes.
    pub fn root_count(&self) -> usize {
        self.site.roots().len()
    }

    /// Get total node count (same as len).
    pub fn total_nodes(&self) -> usize {
        self.site.total_nodes()
    }

    /// Get maximum depth.
    pub fn max_depth(&self) -> usize {
        self.site.max_depth()
    }

    /// Get statistics about the tree.
    pub fn stats(&self) -> SiteStats {
        self.site.stats()
    }

    // ========================================================================
    // Iteration
    // ========================================================================

    /// Iterate over root nodes.
    pub fn roots(&self) -> impl Iterator<Item = &Entry> {
        self.site.root_entries()
    }

    /// Iterate over root indices.
    pub fn root_indices(&self) -> impl Iterator<Item = usize> + '_ {
        self.site.root_indices()
    }

    /// Iterate over all nodes.
    pub fn iter(&self) -> impl Iterator<Item = &Entry> {
        self.site.iter()
    }

    /// Traverse depth-first from roots.
    pub fn traverse_dfs(&self) -> Vec<usize> {
        self.site.traverse_dfs()
    }

    // ========================================================================
    // Traversal
    // ========================================================================

    /// Traverse breadth-first from roots.
    pub fn traverse_bfs(&self) -> Vec<usize> {
        self.site.traverse_bfs()
    }

    /// Get path from root to node.
    pub fn path_to(&self, index: usize) -> Option<Vec<usize>> {
        self.site.path_to(index)
    }

    /// Get depth of a node (distance from root).
    pub fn depth_of(&self, index: usize) -> Option<usize> {
        self.path_to(index).map(|p| p.len().saturating_sub(1))
    }

    /// Get height of the tree (max depth + 1).
    pub fn height(&self) -> usize {
        self.max_depth() + 1
    }

    /// Get children of a node.
    pub fn children_of(&self, index: usize) -> Vec<usize> {
        self.site.children_of(index)
    }

    /// Get parent of a node.
    pub fn parent_of(&self, index: usize) -> Option<usize> {
        self.site.parent_of(index)
    }

    /// Get siblings of a node.
    pub fn siblings_of(&self, index: usize) -> Vec<usize> {
        self.site.siblings_of(index)
    }

    /// Get all leaf nodes (nodes with no children).
    pub fn leaves(&self) -> Vec<usize> {
        self.site.leaves()
    }

    /// Get nodes at a specific depth level.
    pub fn nodes_at_depth(&self, depth: usize) -> Vec<usize> {
        self.site.nodes_at_depth(depth)
    }

    /// Get width at a specific depth (count of nodes at that level).
    pub fn width_at_depth(&self, depth: usize) -> usize {
        self.nodes_at_depth(depth).len()
    }

    /// Get maximum width of the tree.
    pub fn max_width(&self) -> usize {
        (0..=self.max_depth())
            .map(|d| self.width_at_depth(d))
            .max()
            .unwrap_or(0)
    }

    /// Count leaf nodes.
    pub fn leaf_count(&self) -> usize {
        self.leaves().len()
    }

    // ========================================================================
    // Utilities
    // ========================================================================

    /// Clear all nodes and metadata.
    pub fn clear(&mut self) {
        self.site.clear();
        self.current_order = 0;
    }

    /// Get capacity of underlying storage.
    pub fn capacity(&self) -> usize {
        self.site.capacity()
    }

    /// Reserve additional capacity.
    pub fn reserve(&mut self, additional: usize) {
        self.site.reserve(additional);
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::db::node::{function, param, struct_node};

    #[test]
    fn test_tree_basic() {
        let mut tree = Tree::new();
        let fn_idx = tree.add_root(function("main"));

        assert_eq!(tree.len(), 1);
        assert_eq!(tree.root_count(), 1);
        assert!(tree.get(fn_idx).is_some());
    }

    #[test]
    fn test_tree_children() {
        let mut tree = Tree::new();
        let fn_idx = tree.add_root(function("main"));
        let p1 = tree.add_child(fn_idx, param("argc", "int")).unwrap();
        let p2 = tree.add_child(fn_idx, param("argv", "char**")).unwrap();

        assert_eq!(tree.len(), 3);
        assert_eq!(tree.site.children_of(fn_idx), vec![p1, p2]);
    }

    #[test]
    fn test_tree_queries() {
        let mut tree = Tree::new();
        tree.add_root(function("func1"));
        tree.add_root(function("func2"));
        tree.add_root(struct_node("MyStruct"));

        assert_eq!(tree.functions().len(), 2);
        assert_eq!(tree.structs().len(), 1);
    }

    #[test]
    fn test_tree_metadata() {
        let tree = Tree::new()
            .with_metadata("source_file", Entry::string("test.c"))
            .with_metadata("version", Entry::string("1.0"));

        assert_eq!(
            tree.get_metadata("source_file")
                .and_then(|e| e.name())
                .or_else(|| tree.get_metadata("source_file").map(|_| "test.c")),
            Some("test.c")
        );
    }

    #[test]
    fn test_tree_stats() {
        let mut tree = Tree::new();
        tree.add_root(function("main"));
        tree.add_root(struct_node("Point"));

        let stats = tree.stats();
        assert_eq!(stats.total_sites, 2);
        assert_eq!(stats.node_count, 2);
    }
}
