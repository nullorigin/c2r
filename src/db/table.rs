//! Table Formatter for DB System
//!
//! Provides table-based visualization for Entry and Tree structures.
//! Follows the same pattern as json.rs - takes Entry/Tree as input
//! and produces formatted table output for terminal display.
//!
//! Features:
//! - Dynamic column sizing based on content
//! - Unicode-aware width calculations
//! - Support for nested structures
//! - Multiple output styles (compact, detailed, tree-view)

use crate::db::tree::Tree;
use crate::db::web::{Entry, Links, Site};
use std::collections::HashMap;
use std::fmt::Write;

// ============================================================================
// Configuration
// ============================================================================

/// Table style configuration
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TableStyle {
    /// Compact single-line borders
    Compact,
    /// Detailed with headers and separators
    Detailed,
    /// Tree-like hierarchical view
    TreeView,
    /// Minimal with no borders
    Minimal,
}

impl Default for TableStyle {
    fn default() -> Self {
        Self::Detailed
    }
}

/// Text alignment within cells
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Alignment {
    #[default]
    Left,
    Center,
    Right,
}

/// Table configuration
#[derive(Debug, Clone)]
pub struct TableConfig {
    pub style: TableStyle,
    pub min_col_width: usize,
    pub max_col_width: usize,
    pub padding: usize,
    pub show_types: bool,
    pub show_indices: bool,
    pub max_depth: usize,
    pub truncate_strings: usize,
}

impl Default for TableConfig {
    fn default() -> Self {
        Self {
            style: TableStyle::Detailed,
            min_col_width: 4,
            max_col_width: 60,
            padding: 1,
            show_types: true,
            show_indices: true,
            max_depth: 10,
            truncate_strings: 50,
        }
    }
}

// ============================================================================
// Unicode Width Calculation
// ============================================================================

/// Calculate display width of a string (handles Unicode properly)
fn unicode_width(s: &str) -> usize {
    s.chars()
        .map(|c| {
            match c {
                // Wide Unicode characters (emoji, CJK, etc.)
                '✅' | '❌' | '✓' | '✗' | '→' | '←' | '↔' | '↑' | '↓' | '⑂' | '⇢' | '✕' | '↻'
                | '◉' | '⤺' | '∠' => 2,
                // Zero-width characters
                '\u{200b}' | '\u{200c}' | '\u{200d}' | '\u{feff}' => 0,
                // CJK ranges (approximate)
                c if ('\u{4e00}'..='\u{9fff}').contains(&c) => 2,
                c if ('\u{3000}'..='\u{303f}').contains(&c) => 2,
                c if ('\u{ff00}'..='\u{ffef}').contains(&c) => 2,
                // ASCII and most other characters
                _ => 1,
            }
        })
        .sum()
}

/// Truncate string to fit within max width
fn truncate_to_width(s: &str, max_width: usize) -> String {
    if unicode_width(s) <= max_width {
        return s.to_string();
    }

    let mut result = String::new();
    let mut width = 0;

    for c in s.chars() {
        let char_width = unicode_width(&c.to_string());
        if width + char_width + 3 > max_width {
            // +3 for "..."
            result.push_str("...");
            break;
        }
        result.push(c);
        width += char_width;
    }

    result
}

/// Pad string to target width with given alignment
fn pad_to_width(s: &str, target_width: usize, alignment: Alignment) -> String {
    let current_width = unicode_width(s);
    if current_width >= target_width {
        return s.to_string();
    }

    let padding = target_width - current_width;
    match alignment {
        Alignment::Left => format!("{}{}", s, " ".repeat(padding)),
        Alignment::Right => format!("{}{}", " ".repeat(padding), s),
        Alignment::Center => {
            let left = padding / 2;
            let right = padding - left;
            format!("{}{}{}", " ".repeat(left), s, " ".repeat(right))
        }
    }
}

// ============================================================================
// Table Row
// ============================================================================

/// A row in the table
#[derive(Debug, Clone)]
pub struct TableRow {
    pub cells: Vec<String>,
    pub depth: usize,
    pub is_header: bool,
}

impl TableRow {
    pub fn new(cells: Vec<String>) -> Self {
        Self {
            cells,
            depth: 0,
            is_header: false,
        }
    }

    pub fn header(cells: Vec<String>) -> Self {
        Self {
            cells,
            depth: 0,
            is_header: true,
        }
    }

    pub fn with_depth(mut self, depth: usize) -> Self {
        self.depth = depth;
        self
    }
}

// ============================================================================
// Table Generator
// ============================================================================

/// Generates table output from Entry/Tree structures
pub struct TableGenerator {
    rows: Vec<TableRow>,
    config: TableConfig,
    column_widths: Vec<usize>,
}

impl TableGenerator {
    pub fn new(config: TableConfig) -> Self {
        Self {
            rows: Vec::new(),
            config,
            column_widths: Vec::new(),
        }
    }

    pub fn with_default_config() -> Self {
        Self::new(TableConfig::default())
    }

    /// Add a row to the table
    pub fn add_row(&mut self, row: TableRow) {
        // Update column widths
        for (i, cell) in row.cells.iter().enumerate() {
            let cell_width = unicode_width(cell).min(self.config.max_col_width);
            if i >= self.column_widths.len() {
                self.column_widths
                    .push(cell_width.max(self.config.min_col_width));
            } else {
                self.column_widths[i] = self.column_widths[i].max(cell_width);
            }
        }
        self.rows.push(row);
    }

    /// Add Entry data to table
    pub fn add_entry(&mut self, entry: &Entry, depth: usize) {
        if depth > self.config.max_depth {
            return;
        }

        match entry {
            Entry::Node {
                kind,
                name,
                attrs,
                links,
            } => {
                self.add_node_row(kind, name, attrs, links, depth);
            }
            Entry::Vec(items, _) => {
                for (i, item) in items.iter().enumerate() {
                    let prefix = if depth == 0 {
                        format!("[{}]", i)
                    } else {
                        format!("  [{}]", i)
                    };
                    self.add_entry_with_prefix(&prefix, item, depth);
                }
            }
            Entry::HashMap(map, _) => {
                for (key, value) in map {
                    self.add_entry_with_prefix(key, value, depth);
                }
            }
            _ => {
                self.add_primitive_row(entry, depth);
            }
        }
    }

    fn add_node_row(
        &mut self,
        kind: &str,
        name: &str,
        attrs: &HashMap<String, Entry>,
        links: &Links,
        depth: usize,
    ) {
        let indent = "  ".repeat(depth);

        // Main node row
        let mut cells = vec![format!("{}{}", indent, kind), name.to_string()];

        if self.config.show_types {
            cells.push("Node".to_string());
        }

        // Add link count if any
        if !links.is_empty() {
            cells.push(format!("{} links", links.len()));
        } else {
            cells.push(String::new());
        }

        self.add_row(TableRow::new(cells).with_depth(depth));

        // Add attributes as sub-rows
        for (key, value) in attrs {
            self.add_attr_row(key, value, depth + 1);
        }
    }

    fn add_attr_row(&mut self, key: &str, value: &Entry, depth: usize) {
        let indent = "  ".repeat(depth);
        let value_str = self.entry_to_string(value);
        let truncated = truncate_to_width(&value_str, self.config.truncate_strings);

        let mut cells = vec![format!("{}.{}", indent, key), truncated];

        if self.config.show_types {
            cells.push(value.type_name().to_string());
        }

        cells.push(String::new()); // Empty links column

        self.add_row(TableRow::new(cells).with_depth(depth));

        // Recurse for nested containers
        if let Entry::Vec(items, _) = value {
            if items.len() <= 5 {
                // Only expand small arrays inline
                for (i, item) in items.iter().enumerate() {
                    self.add_entry_with_prefix(&format!("[{}]", i), item, depth + 1);
                }
            }
        }
    }

    fn add_entry_with_prefix(&mut self, prefix: &str, entry: &Entry, depth: usize) {
        let indent = "  ".repeat(depth);
        let value_str = self.entry_to_string(entry);
        let truncated = truncate_to_width(&value_str, self.config.truncate_strings);

        let mut cells = vec![format!("{}{}", indent, prefix), truncated];

        if self.config.show_types {
            cells.push(entry.type_name().to_string());
        }

        cells.push(String::new());

        self.add_row(TableRow::new(cells).with_depth(depth));
    }

    fn add_primitive_row(&mut self, entry: &Entry, depth: usize) {
        let indent = "  ".repeat(depth);
        let value_str = self.entry_to_string(entry);
        let truncated = truncate_to_width(&value_str, self.config.truncate_strings);

        let mut cells = vec![format!("{}value", indent), truncated];

        if self.config.show_types {
            cells.push(entry.type_name().to_string());
        }

        cells.push(String::new());

        self.add_row(TableRow::new(cells).with_depth(depth));
    }

    fn entry_to_string(&self, entry: &Entry) -> String {
        match entry {
            Entry::String(s, _) => s.clone(),
            Entry::I8(n, _) => n.to_string(),
            Entry::I16(n, _) => n.to_string(),
            Entry::I32(n, _) => n.to_string(),
            Entry::I64(n, _) => n.to_string(),
            Entry::I128(n, _) => n.to_string(),
            Entry::Isize(n, _) => n.to_string(),
            Entry::U8(n, _) => n.to_string(),
            Entry::U16(n, _) => n.to_string(),
            Entry::U32(n, _) => n.to_string(),
            Entry::U64(n, _) => n.to_string(),
            Entry::U128(n, _) => n.to_string(),
            Entry::Usize(n, _) => n.to_string(),
            Entry::F32(n, _) => format!("{:.4}", n),
            Entry::F64(n, _) => format!("{:.4}", n),
            Entry::Bool(b, _) => if *b { "true" } else { "false" }.to_string(),
            Entry::Char(c, _) => format!("'{}'", c),
            Entry::Unit(_) => "()".to_string(),
            Entry::PathBuf(p, _) => p.display().to_string(),
            Entry::Range(r, _) => format!("{}..{}", r.start, r.end),
            Entry::Vec(v, _) => format!("[{} items]", v.len()),
            Entry::HashMap(m, _) => format!("{{{} entries}}", m.len()),
            Entry::Pair(p, _) => format!(
                "({}, {})",
                self.entry_to_string(&p.0),
                self.entry_to_string(&p.1)
            ),
            Entry::Box(inner, _) => self.entry_to_string(inner),
            Entry::Node { kind, name, .. } => format!("{}:{}", kind, name),
            Entry::Fn(_, _) => "<function>".to_string(),
            Entry::Branch {
                operation,
                true_path,
                false_path,
                ..
            } => format!(
                "Branch({}, t:{}, f:{})",
                operation.name(),
                true_path.len(),
                false_path.len()
            ),
        }
    }

    /// Generate table output
    pub fn generate(&self) -> String {
        match self.config.style {
            TableStyle::Detailed => self.generate_detailed(),
            TableStyle::Compact => self.generate_compact(),
            TableStyle::TreeView => self.generate_tree_view(),
            TableStyle::Minimal => self.generate_minimal(),
        }
    }

    fn generate_detailed(&self) -> String {
        if self.rows.is_empty() {
            return "(empty table)\n".to_string();
        }

        let mut output = String::new();
        let _total_width: usize = self.column_widths.iter().sum::<usize>()
            + (self.column_widths.len() * (self.config.padding * 2 + 1))
            + 1;

        // Top border
        output.push('┌');
        for (i, &width) in self.column_widths.iter().enumerate() {
            output.push_str(&"─".repeat(width + self.config.padding * 2));
            if i < self.column_widths.len() - 1 {
                output.push('┬');
            }
        }
        output.push_str("┐\n");

        // Rows
        for (row_idx, row) in self.rows.iter().enumerate() {
            output.push('│');
            for (i, cell) in row.cells.iter().enumerate() {
                let width = self
                    .column_widths
                    .get(i)
                    .copied()
                    .unwrap_or(self.config.min_col_width);
                let truncated = truncate_to_width(cell, width);
                let padded = pad_to_width(&truncated, width, Alignment::Left);
                let _ = write!(
                    output,
                    "{}{}{}",
                    " ".repeat(self.config.padding),
                    padded,
                    " ".repeat(self.config.padding)
                );
                output.push('│');
            }
            // Fill remaining columns if row is shorter
            for i in row.cells.len()..self.column_widths.len() {
                let width = self.column_widths[i];
                let _ = write!(
                    output,
                    "{}{}",
                    " ".repeat(width + self.config.padding * 2),
                    "│"
                );
            }
            output.push('\n');

            // Row separator (except for last row)
            if row_idx < self.rows.len() - 1 {
                let is_header_separator = row.is_header;
                output.push('├');
                for (i, &width) in self.column_widths.iter().enumerate() {
                    let ch = if is_header_separator { "═" } else { "─" };
                    output.push_str(&ch.repeat(width + self.config.padding * 2));
                    if i < self.column_widths.len() - 1 {
                        output.push(if is_header_separator { '╪' } else { '┼' });
                    }
                }
                output.push_str("┤\n");
            }
        }

        // Bottom border
        output.push('└');
        for (i, &width) in self.column_widths.iter().enumerate() {
            output.push_str(&"─".repeat(width + self.config.padding * 2));
            if i < self.column_widths.len() - 1 {
                output.push('┴');
            }
        }
        output.push_str("┘\n");

        output
    }

    fn generate_compact(&self) -> String {
        let mut output = String::new();

        for row in &self.rows {
            let line = row.cells.join(" | ");
            output.push_str(&line);
            output.push('\n');
        }

        output
    }

    fn generate_tree_view(&self) -> String {
        let mut output = String::new();

        for row in &self.rows {
            let prefix = if row.depth > 0 {
                format!("{}├── ", "│   ".repeat(row.depth - 1))
            } else {
                String::new()
            };

            let content = if row.cells.len() >= 2 {
                format!("{}: {}", row.cells[0].trim(), row.cells[1])
            } else if !row.cells.is_empty() {
                row.cells[0].clone()
            } else {
                String::new()
            };

            output.push_str(&prefix);
            output.push_str(&content);
            output.push('\n');
        }

        output
    }

    fn generate_minimal(&self) -> String {
        let mut output = String::new();

        for row in &self.rows {
            let indent = "  ".repeat(row.depth);
            let line = row.cells.join("\t");
            output.push_str(&indent);
            output.push_str(&line);
            output.push('\n');
        }

        output
    }
}

// ============================================================================
// Public API Functions
// ============================================================================

/// Convert an Entry to a table string with default config
pub fn to_table(entry: &Entry) -> String {
    to_table_with_config(entry, TableConfig::default())
}

/// Convert an Entry to a table string with custom config
pub fn to_table_with_config(entry: &Entry, config: TableConfig) -> String {
    let mut generator = TableGenerator::new(config.clone());

    // Add header
    let mut header_cells = vec!["Kind/Key".to_string(), "Value".to_string()];
    if config.show_types {
        header_cells.push("Type".to_string());
    }
    header_cells.push("Links".to_string());
    generator.add_row(TableRow::header(header_cells));

    // Add entry data
    generator.add_entry(entry, 0);

    generator.generate()
}

/// Convert a Tree to a table string
pub fn tree_to_table(tree: &Tree) -> String {
    tree_to_table_with_config(tree, TableConfig::default())
}

/// Convert a Tree to a table string with custom config
pub fn tree_to_table_with_config(tree: &Tree, config: TableConfig) -> String {
    let mut generator = TableGenerator::new(config.clone());

    // Add header
    let mut header_cells = vec!["Index".to_string(), "Kind".to_string(), "Name".to_string()];
    if config.show_types {
        header_cells.push("Attributes".to_string());
    }
    generator.add_row(TableRow::header(header_cells));

    // Add nodes
    for (idx, entry) in tree.iter().enumerate() {
        let mut cells = vec![format!("{}", idx)];

        if let Some(kind) = entry.kind() {
            cells.push(kind.to_string());
        } else {
            cells.push(entry.type_name().to_string());
        }

        if let Some(name) = entry.name() {
            cells.push(name.to_string());
        } else {
            cells.push(format!("{}", entry));
        }

        if config.show_types {
            if let Some(attrs) = entry.attrs() {
                let attr_summary: Vec<String> =
                    attrs.keys().take(3).map(|k| k.to_string()).collect();
                let summary = if attrs.len() > 3 {
                    format!("{} (+{})", attr_summary.join(", "), attrs.len() - 3)
                } else {
                    attr_summary.join(", ")
                };
                cells.push(summary);
            } else {
                cells.push(String::new());
            }
        }

        generator.add_row(TableRow::new(cells));
    }

    generator.generate()
}

/// Convert a Site to a table string
pub fn site_to_table(site: &Site) -> String {
    site_to_table_with_config(site, TableConfig::default())
}

/// Convert a Site to a table string with custom config
pub fn site_to_table_with_config(site: &Site, config: TableConfig) -> String {
    let mut generator = TableGenerator::new(config.clone());

    // Add title info
    let title = format!(
        "Site: {} ({} nodes, {} roots)",
        if site.name().is_empty() {
            "<unnamed>"
        } else {
            site.name()
        },
        site.len(),
        site.roots().len()
    );
    generator.add_row(TableRow::header(vec![title]));

    // Add column headers
    let header_cells = vec![
        "Idx".to_string(),
        "Kind".to_string(),
        "Name".to_string(),
        "Depth".to_string(),
        "Children".to_string(),
    ];
    generator.add_row(TableRow::header(header_cells));

    // Add nodes
    for idx in 0..site.len() {
        if let Some(entry) = site.get(idx) {
            let kind = entry.kind().unwrap_or(entry.type_name());
            let name = entry
                .name()
                .map(|s| s.to_string())
                .unwrap_or_else(|| format!("{}", entry));
            let depth = entry.depth();
            let children: Vec<usize> = entry.children().collect();

            let cells = vec![
                format!("{}", idx),
                kind.to_string(),
                truncate_to_width(&name, 30),
                format!("{}", depth),
                if children.is_empty() {
                    "-".to_string()
                } else {
                    format!("{:?}", children)
                },
            ];

            generator.add_row(TableRow::new(cells));
        }
    }

    generator.generate()
}

// ============================================================================
// Entry Extension Methods
// ============================================================================

impl Entry {
    /// Convert to table string with default config
    pub fn to_table(&self) -> String {
        to_table(self)
    }

    /// Convert to table string with custom config
    pub fn to_table_with_config(&self, config: TableConfig) -> String {
        to_table_with_config(self, config)
    }

    /// Convert to compact table string
    pub fn to_table_compact(&self) -> String {
        let mut config = TableConfig::default();
        config.style = TableStyle::Compact;
        to_table_with_config(self, config)
    }

    /// Convert to tree-view table string
    pub fn to_table_tree(&self) -> String {
        let mut config = TableConfig::default();
        config.style = TableStyle::TreeView;
        to_table_with_config(self, config)
    }
}

impl Tree {
    /// Convert to table string
    pub fn to_table(&self) -> String {
        tree_to_table(self)
    }
}

impl Site {
    /// Convert to table string
    pub fn to_table(&self) -> String {
        site_to_table(self)
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_unicode_width() {
        assert_eq!(unicode_width("hello"), 5);
        assert_eq!(unicode_width("✅"), 2);
        assert_eq!(unicode_width("test✅"), 6);
    }

    #[test]
    fn test_truncate() {
        assert_eq!(truncate_to_width("hello", 10), "hello");
        assert_eq!(truncate_to_width("hello world", 8), "hello...");
    }

    #[test]
    fn test_pad_to_width() {
        assert_eq!(pad_to_width("hi", 5, Alignment::Left), "hi   ");
        assert_eq!(pad_to_width("hi", 5, Alignment::Right), "   hi");
        assert_eq!(pad_to_width("hi", 6, Alignment::Center), "  hi  ");
    }

    #[test]
    fn test_entry_to_table() {
        let entry = Entry::node("Function", "main");
        let table = entry.to_table();
        assert!(table.contains("Function"));
        assert!(table.contains("main"));
    }

    #[test]
    fn test_tree_to_table() {
        let tree = Tree::new()
            .with_function("main", 0..100, |f| f.returns("int"))
            .with_struct("Point", 101..150, |s| s.field("x", "int").field("y", "int"));

        let table = tree.to_table();
        assert!(table.contains("main"));
        assert!(table.contains("Point"));
        assert!(table.contains("Function"));
        assert!(table.contains("Struct"));
    }

    #[test]
    fn test_compact_style() {
        let entry = Entry::i32(42);
        let table = entry.to_table_compact();
        assert!(table.contains("42"));
    }

    #[test]
    fn test_tree_view_style() {
        let mut entry = Entry::node("Root", "root");
        entry.set_attr("child", Entry::string("value"));
        let table = entry.to_table_tree();
        assert!(table.contains("Root"));
    }

    #[test]
    fn test_nested_entry() {
        let mut config = TableConfig::default();
        config.max_depth = 3;

        let entry = Entry::vec(vec![Entry::i32(1), Entry::i32(2), Entry::i32(3)]);

        let table = to_table_with_config(&entry, config);
        assert!(table.contains("[0]"));
        assert!(table.contains("[1]"));
        assert!(table.contains("[2]"));
    }
}
