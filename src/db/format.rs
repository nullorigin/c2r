//! Unified Formatting System
//!
//! Provides a consistent formatting interface for all database types.
//! Supports multiple output formats: Table, JSON, Text, Tree, Compact, and Custom.

use crate::db::json::{DumpGenerator, JsonGenerator};
use crate::db::web::{Entry, Web, WebStats};
use std::collections::HashMap;
use std::fmt::{self, Write};

// ============================================================================
// Output Format
// ============================================================================

/// Output format specification for any displayable type
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum OutputFormat {
    /// Plain text with minimal formatting
    #[default]
    Text,
    /// Formatted table with borders
    Table,
    /// Pretty-printed JSON with indentation
    Json,
    /// Compact JSON (no whitespace)
    JsonCompact,
    /// Tree/hierarchy view with branches
    Tree,
    /// Single-line compact summary
    Compact,
    /// Markdown format
    Markdown,
    /// CSV format
    Csv,
    /// Custom template with placeholders
    Custom(String),
}

impl OutputFormat {
    pub fn from_str(s: &str) -> Self {
        match s.to_lowercase().as_str() {
            "text" | "txt" => Self::Text,
            "table" | "tbl" => Self::Table,
            "json" => Self::Json,
            "json-compact" | "jsonc" => Self::JsonCompact,
            "tree" => Self::Tree,
            "compact" | "short" => Self::Compact,
            "markdown" | "md" => Self::Markdown,
            "csv" => Self::Csv,
            _ => Self::Text,
        }
    }

    pub fn extension(&self) -> &'static str {
        match self {
            Self::Text => "txt",
            Self::Table => "txt",
            Self::Json | Self::JsonCompact => "json",
            Self::Tree => "txt",
            Self::Compact => "txt",
            Self::Markdown => "md",
            Self::Csv => "csv",
            Self::Custom(_) => "txt",
        }
    }
}

// ============================================================================
// Formatter Trait
// ============================================================================

/// Trait for types that can be formatted in multiple output formats
pub trait Formatter {
    /// Format the value using the specified output format
    fn format(&self, fmt: &OutputFormat) -> String;

    /// Format as plain text (default implementation)
    fn to_text(&self) -> String {
        self.format(&OutputFormat::Text)
    }

    /// Format as table
    fn to_table(&self) -> String {
        self.format(&OutputFormat::Table)
    }

    /// Format as pretty JSON
    fn to_json(&self) -> String {
        self.format(&OutputFormat::Json)
    }

    /// Format as compact JSON
    fn to_json_compact(&self) -> String {
        self.format(&OutputFormat::JsonCompact)
    }

    /// Format as tree view
    fn to_tree(&self) -> String {
        self.format(&OutputFormat::Tree)
    }

    /// Format as compact single line
    fn to_compact(&self) -> String {
        self.format(&OutputFormat::Compact)
    }

    /// Format as markdown
    fn to_markdown(&self) -> String {
        self.format(&OutputFormat::Markdown)
    }

    /// Format as CSV
    fn to_csv(&self) -> String {
        self.format(&OutputFormat::Csv)
    }
}

// ============================================================================
// FormatConfig - Configuration for formatting
// ============================================================================

/// Configuration options for formatting
#[derive(Debug, Clone)]
pub struct FormatConfig {
    /// Indentation string (spaces or tabs)
    pub indent: String,
    /// Maximum width for table columns
    pub max_width: usize,
    /// Whether to include headers in tables
    pub show_headers: bool,
    /// Whether to show empty fields
    pub show_empty: bool,
    /// Date/time format string
    pub datetime_format: String,
    /// Number of decimal places for floats
    pub float_precision: usize,
    /// Whether to use colors (ANSI)
    pub use_colors: bool,
    /// Tree branch characters
    pub tree_chars: TreeChars,
}

impl Default for FormatConfig {
    fn default() -> Self {
        Self {
            indent: "  ".to_string(),
            max_width: 80,
            show_headers: true,
            show_empty: false,
            datetime_format: "%Y-%m-%d %H:%M:%S".to_string(),
            float_precision: 2,
            use_colors: false,
            tree_chars: TreeChars::default(),
        }
    }
}

/// Characters used for tree formatting
#[derive(Debug, Clone)]
pub struct TreeChars {
    pub branch: &'static str,
    pub last_branch: &'static str,
    pub vertical: &'static str,
    pub empty: &'static str,
}

impl Default for TreeChars {
    fn default() -> Self {
        Self {
            branch: "├─ ",
            last_branch: "└─ ",
            vertical: "│  ",
            empty: "   ",
        }
    }
}

// ============================================================================
// Entry Formatter Implementation
// ============================================================================

impl Formatter for Entry {
    fn format(&self, fmt: &OutputFormat) -> String {
        match fmt {
            OutputFormat::Text => format_entry_text(self, 0),
            OutputFormat::Table => format_entry_table(self),
            OutputFormat::Json => self.to_json_pretty(2),
            OutputFormat::JsonCompact => {
                let mut generator = DumpGenerator::new();
                let _ = generator.write_entry(self);
                generator.consume()
            }
            OutputFormat::Tree => format_entry_tree(self, 0, &FormatConfig::default()),
            OutputFormat::Compact => format_entry_compact(self),
            OutputFormat::Markdown => format_entry_markdown(self, 0),
            OutputFormat::Csv => format_entry_csv(self),
            OutputFormat::Custom(template) => format_entry_custom(self, template),
        }
    }
}

fn format_entry_text(entry: &Entry, indent: usize) -> String {
    let prefix = "  ".repeat(indent);
    let mut out = String::new();

    match entry {
        Entry::Node {
            kind, name, attrs, ..
        } => {
            writeln!(out, "{}[{}] {}", prefix, kind, name).unwrap();
            for (key, value) in attrs {
                writeln!(out, "{}  {}: {}", prefix, key, entry_value_string(value)).unwrap();
            }
        }
        Entry::Vec(items, _) => {
            for (i, item) in items.iter().enumerate() {
                writeln!(out, "{}[{}]:", prefix, i).unwrap();
                out.push_str(&format_entry_text(item, indent + 1));
            }
        }
        Entry::HashMap(map, _) => {
            for (key, value) in map {
                writeln!(out, "{}{}: {}", prefix, key, entry_value_string(value)).unwrap();
            }
        }
        _ => {
            writeln!(out, "{}{}", prefix, entry).unwrap();
        }
    }
    out
}

fn format_entry_table(entry: &Entry) -> String {
    let mut out = String::new();
    let width = 78;

    writeln!(out, "+{}+", "-".repeat(width)).unwrap();

    match entry {
        Entry::Node {
            kind, name, attrs, ..
        } => {
            writeln!(
                out,
                "| {:^width$} |",
                format!("{}: {}", kind, name),
                width = width - 2
            )
            .unwrap();
            writeln!(out, "+{}+", "-".repeat(width)).unwrap();

            if !attrs.is_empty() {
                for (key, value) in attrs {
                    let val_str = entry_value_string(value);
                    writeln!(
                        out,
                        "| {:20} | {:width$} |",
                        key,
                        val_str,
                        width = width - 25
                    )
                    .unwrap();
                }
                writeln!(out, "+{}+", "-".repeat(width)).unwrap();
            }
        }
        Entry::Vec(items, _) => {
            writeln!(
                out,
                "| {:^width$} |",
                format!("Vec[{}]", items.len()),
                width = width - 2
            )
            .unwrap();
            writeln!(out, "+{}+", "-".repeat(width)).unwrap();
            for (i, item) in items.iter().enumerate().take(10) {
                writeln!(
                    out,
                    "| {:4} | {:width$} |",
                    i,
                    entry_value_string(item),
                    width = width - 9
                )
                .unwrap();
            }
            if items.len() > 10 {
                writeln!(
                    out,
                    "| {:^width$} |",
                    format!("... and {} more", items.len() - 10),
                    width = width - 2
                )
                .unwrap();
            }
            writeln!(out, "+{}+", "-".repeat(width)).unwrap();
        }
        _ => {
            writeln!(out, "| {:^width$} |", entry.to_string(), width = width - 2).unwrap();
            writeln!(out, "+{}+", "-".repeat(width)).unwrap();
        }
    }
    out
}

fn format_entry_tree(entry: &Entry, depth: usize, config: &FormatConfig) -> String {
    let mut out = String::new();
    let prefix = if depth == 0 {
        String::new()
    } else {
        format!(
            "{}{}",
            config.tree_chars.vertical.repeat(depth - 1),
            config.tree_chars.branch
        )
    };

    match entry {
        Entry::Node {
            kind,
            name,
            attrs,
            links,
            ..
        } => {
            writeln!(
                out,
                "{}[{}] {} (links: {})",
                prefix,
                kind,
                name,
                links.len()
            )
            .unwrap();
            let attr_count = attrs.len();
            for (i, (key, value)) in attrs.iter().enumerate() {
                let branch = if i == attr_count - 1 {
                    config.tree_chars.last_branch
                } else {
                    config.tree_chars.branch
                };
                let child_prefix =
                    format!("{}{}", config.tree_chars.vertical.repeat(depth), branch);
                writeln!(
                    out,
                    "{}{}: {}",
                    child_prefix,
                    key,
                    entry_value_string(value)
                )
                .unwrap();
            }
        }
        Entry::Vec(items, _) => {
            writeln!(out, "{}Vec[{}]", prefix, items.len()).unwrap();
            for (i, item) in items.iter().enumerate() {
                let is_last = i == items.len() - 1;
                let branch = if is_last {
                    config.tree_chars.last_branch
                } else {
                    config.tree_chars.branch
                };
                let child_prefix =
                    format!("{}{}", config.tree_chars.vertical.repeat(depth), branch);
                write!(out, "{}", child_prefix).unwrap();
                out.push_str(&format_entry_tree(item, depth + 1, config));
            }
        }
        _ => {
            writeln!(out, "{}{}", prefix, entry).unwrap();
        }
    }
    out
}

fn format_entry_compact(entry: &Entry) -> String {
    match entry {
        Entry::Node {
            kind, name, attrs, ..
        } => {
            if attrs.is_empty() {
                format!("[{}]{}", kind, name)
            } else {
                format!("[{}]{}({})", kind, name, attrs.len())
            }
        }
        Entry::Vec(items, _) => format!("Vec[{}]", items.len()),
        Entry::HashMap(map, _) => format!("Map{{{}}}", map.len()),
        _ => entry.to_string(),
    }
}

fn format_entry_markdown(entry: &Entry, level: usize) -> String {
    let mut out = String::new();
    let heading = "#".repeat(level.min(6).max(1));

    match entry {
        Entry::Node {
            kind, name, attrs, ..
        } => {
            writeln!(out, "{} {} `{}`\n", heading, kind, name).unwrap();
            if !attrs.is_empty() {
                writeln!(out, "| Field | Value |").unwrap();
                writeln!(out, "|-------|-------|").unwrap();
                for (key, value) in attrs {
                    writeln!(out, "| {} | {} |", key, entry_value_string(value)).unwrap();
                }
                writeln!(out).unwrap();
            }
        }
        Entry::Vec(items, _) => {
            for (i, item) in items.iter().enumerate() {
                writeln!(out, "{}. {}", i + 1, entry_value_string(item)).unwrap();
            }
        }
        _ => {
            writeln!(out, "{}", entry).unwrap();
        }
    }
    out
}

fn format_entry_csv(entry: &Entry) -> String {
    let mut out = String::new();

    match entry {
        Entry::Node {
            kind, name, attrs, ..
        } => {
            // Header row
            let mut headers: Vec<&str> = vec!["kind", "name"];
            let keys: Vec<&String> = attrs.keys().collect();
            headers.extend(keys.iter().map(|k| k.as_str()));
            writeln!(out, "{}", headers.join(",")).unwrap();

            // Data row
            let mut values: Vec<String> = vec![kind.clone(), name.clone()];
            for key in &keys {
                values.push(csv_escape(&entry_value_string(attrs.get(*key).unwrap())));
            }
            writeln!(out, "{}", values.join(",")).unwrap();
        }
        Entry::Vec(items, _) => {
            writeln!(out, "index,type,value").unwrap();
            for (i, item) in items.iter().enumerate() {
                writeln!(
                    out,
                    "{},{},{}",
                    i,
                    item.type_name(),
                    csv_escape(&entry_value_string(item))
                )
                .unwrap();
            }
        }
        _ => {
            writeln!(out, "type,value").unwrap();
            writeln!(
                out,
                "{},{}",
                entry.type_name(),
                csv_escape(&entry.to_string())
            )
            .unwrap();
        }
    }
    out
}

fn format_entry_custom(entry: &Entry, template: &str) -> String {
    let mut result = template.to_string();

    result = result.replace("{type}", entry.type_name());
    result = result.replace("{value}", &entry.to_string());

    if let Entry::Node {
        kind, name, attrs, ..
    } = entry
    {
        result = result.replace("{kind}", kind);
        result = result.replace("{name}", name);
        for (key, value) in attrs {
            result = result.replace(&format!("{{attr:{}}}", key), &entry_value_string(value));
        }
    }

    result
}

fn entry_value_string(entry: &Entry) -> String {
    match entry {
        Entry::String(s, _) => s.clone(),
        Entry::Bool(b, _) => b.to_string(),
        Entry::I32(n, _) => n.to_string(),
        Entry::I64(n, _) => n.to_string(),
        Entry::F64(n, _) => format!("{:.2}", n),
        Entry::Usize(n, _) => n.to_string(),
        Entry::Unit(_) => "null".to_string(),
        Entry::Vec(v, _) => format!("[{}]", v.len()),
        Entry::HashMap(m, _) => format!("{{{}}}", m.len()),
        Entry::Node { kind, name, .. } => format!("{}:{}", kind, name),
        _ => entry.to_string(),
    }
}

fn csv_escape(s: &str) -> String {
    if s.contains(',') || s.contains('"') || s.contains('\n') {
        format!("\"{}\"", s.replace('"', "\"\""))
    } else {
        s.to_string()
    }
}

// ============================================================================
// Web Formatter Implementation
// ============================================================================

impl Formatter for Web {
    fn format(&self, fmt: &OutputFormat) -> String {
        match fmt {
            OutputFormat::Text => format_web_text(self),
            OutputFormat::Table => format_web_table(self),
            OutputFormat::Json => format_web_json(self, true),
            OutputFormat::JsonCompact => format_web_json(self, false),
            OutputFormat::Tree => format_web_tree(self),
            OutputFormat::Compact => format_web_compact(self),
            OutputFormat::Markdown => format_web_markdown(self),
            OutputFormat::Csv => format_web_csv(self),
            OutputFormat::Custom(template) => format_web_custom(self, template),
        }
    }
}

fn format_web_text(web: &Web) -> String {
    let mut out = String::new();
    let stats = web.stats();

    writeln!(out, "Web Database").unwrap();
    writeln!(out, "============").unwrap();
    writeln!(out, "Total Entries: {}", stats.total_entries).unwrap();
    writeln!(out, "Total Kinds: {}", stats.total_kinds).unwrap();
    writeln!(out, "Total Categories: {}", stats.total_categories).unwrap();
    writeln!(out, "Build Count: {}", stats.build_count).unwrap();
    writeln!(out).unwrap();

    // Show entries by kind
    for kind in web.kinds() {
        let entries = web.by_kind(kind);
        writeln!(out, "[{}] ({} entries)", kind, entries.len()).unwrap();
        for entry in entries.iter().take(5) {
            writeln!(out, "  - {}", entry_value_string(entry)).unwrap();
        }
        if entries.len() > 5 {
            writeln!(out, "  ... and {} more", entries.len() - 5).unwrap();
        }
    }

    out
}

fn format_web_table(web: &Web) -> String {
    let mut out = String::new();
    let stats = web.stats();
    let width = 78;

    writeln!(out, "+{}+", "=".repeat(width)).unwrap();
    writeln!(out, "| {:^width$} |", "WEB DATABASE", width = width - 2).unwrap();
    writeln!(out, "+{}+", "=".repeat(width)).unwrap();

    writeln!(
        out,
        "| {:30} | {:width$} |",
        "Total Entries",
        stats.total_entries,
        width = width - 35
    )
    .unwrap();
    writeln!(
        out,
        "| {:30} | {:width$} |",
        "Total Kinds",
        stats.total_kinds,
        width = width - 35
    )
    .unwrap();
    writeln!(
        out,
        "| {:30} | {:width$} |",
        "Total Categories",
        stats.total_categories,
        width = width - 35
    )
    .unwrap();
    writeln!(
        out,
        "| {:30} | {:width$} |",
        "Build Count",
        stats.build_count,
        width = width - 35
    )
    .unwrap();
    writeln!(out, "+{}+", "-".repeat(width)).unwrap();

    // Entries by kind
    writeln!(out, "| {:^width$} |", "ENTRIES BY KIND", width = width - 2).unwrap();
    writeln!(out, "+{}+", "-".repeat(width)).unwrap();

    for kind in web.kinds() {
        let count = web.by_kind(kind).len();
        writeln!(
            out,
            "| {:30} | {:width$} |",
            kind,
            count,
            width = width - 35
        )
        .unwrap();
    }

    writeln!(out, "+{}+", "=".repeat(width)).unwrap();
    out
}

fn format_web_json(web: &Web, pretty: bool) -> String {
    let mut map = HashMap::new();

    // Stats
    let stats = web.stats();
    map.insert(
        "total_entries".to_string(),
        Entry::usize(stats.total_entries),
    );
    map.insert("total_kinds".to_string(), Entry::usize(stats.total_kinds));
    map.insert(
        "total_categories".to_string(),
        Entry::usize(stats.total_categories),
    );
    map.insert("build_count".to_string(), Entry::usize(stats.build_count));

    // Entries by kind
    let mut kinds_map = HashMap::new();
    for kind in web.kinds() {
        let entries: Vec<Entry> = web.by_kind(kind).iter().map(|e| (*e).clone()).collect();
        kinds_map.insert(kind.to_string(), Entry::vec(entries));
    }
    map.insert("entries".to_string(), Entry::hashmap(kinds_map));

    let root = Entry::hashmap(map);
    if pretty {
        root.to_json_pretty(2)
    } else {
        root.to_json()
    }
}

fn format_web_tree(web: &Web) -> String {
    let mut out = String::new();
    let stats = web.stats();

    writeln!(out, "📦 Web Database").unwrap();
    writeln!(out, "├─ 📊 Stats").unwrap();
    writeln!(out, "│  ├─ Entries: {}", stats.total_entries).unwrap();
    writeln!(out, "│  ├─ Kinds: {}", stats.total_kinds).unwrap();
    writeln!(out, "│  ├─ Categories: {}", stats.total_categories).unwrap();
    writeln!(out, "│  └─ Builds: {}", stats.build_count).unwrap();

    let kinds: Vec<&str> = web.kinds();
    let kinds_count = kinds.len();

    for (i, kind) in kinds.iter().enumerate() {
        let is_last = i == kinds_count - 1;
        let branch = if is_last { "└─" } else { "├─" };
        let prefix = if is_last { "   " } else { "│  " };

        let entries = web.by_kind(kind);
        writeln!(out, "{} 📁 {} ({})", branch, kind, entries.len()).unwrap();

        for (j, entry) in entries.iter().enumerate().take(3) {
            let entry_branch = if j == entries.len().min(3) - 1 {
                "└─"
            } else {
                "├─"
            };
            writeln!(
                out,
                "{}  {} {}",
                prefix,
                entry_branch,
                entry_value_string(entry)
            )
            .unwrap();
        }
        if entries.len() > 3 {
            writeln!(out, "{}  └─ ... {} more", prefix, entries.len() - 3).unwrap();
        }
    }

    out
}

fn format_web_compact(web: &Web) -> String {
    let stats = web.stats();
    format!(
        "Web({} entries, {} kinds, {} categories)",
        stats.total_entries, stats.total_kinds, stats.total_categories
    )
}

fn format_web_markdown(web: &Web) -> String {
    let mut out = String::new();
    let stats = web.stats();

    writeln!(out, "# Web Database\n").unwrap();
    writeln!(out, "## Statistics\n").unwrap();
    writeln!(out, "| Metric | Value |").unwrap();
    writeln!(out, "|--------|-------|").unwrap();
    writeln!(out, "| Total Entries | {} |", stats.total_entries).unwrap();
    writeln!(out, "| Total Kinds | {} |", stats.total_kinds).unwrap();
    writeln!(out, "| Total Categories | {} |", stats.total_categories).unwrap();
    writeln!(out, "| Build Count | {} |", stats.build_count).unwrap();
    writeln!(out).unwrap();

    writeln!(out, "## Entries by Kind\n").unwrap();
    for kind in web.kinds() {
        let entries = web.by_kind(kind);
        writeln!(out, "### {} ({})\n", kind, entries.len()).unwrap();
        for entry in entries.iter().take(5) {
            writeln!(out, "- {}", entry_value_string(entry)).unwrap();
        }
        if entries.len() > 5 {
            writeln!(out, "- *... and {} more*\n", entries.len() - 5).unwrap();
        }
        writeln!(out).unwrap();
    }

    out
}

fn format_web_csv(web: &Web) -> String {
    let mut out = String::new();

    writeln!(out, "kind,name,attributes").unwrap();
    for entry in web.iter() {
        if let Entry::Node {
            kind, name, attrs, ..
        } = entry
        {
            let attr_count = attrs.len();
            writeln!(
                out,
                "{},{},{}",
                csv_escape(kind),
                csv_escape(name),
                attr_count
            )
            .unwrap();
        } else {
            writeln!(
                out,
                "{},{},{}",
                entry.type_name(),
                csv_escape(&entry.to_string()),
                0
            )
            .unwrap();
        }
    }

    out
}

fn format_web_custom(web: &Web, template: &str) -> String {
    let stats = web.stats();
    let mut result = template.to_string();

    result = result.replace("{entries}", &stats.total_entries.to_string());
    result = result.replace("{kinds}", &stats.total_kinds.to_string());
    result = result.replace("{categories}", &stats.total_categories.to_string());
    result = result.replace("{builds}", &stats.build_count.to_string());

    result
}

// ============================================================================
// WebStats Formatter Implementation
// ============================================================================

impl Formatter for WebStats {
    fn format(&self, fmt: &OutputFormat) -> String {
        match fmt {
            OutputFormat::Text => format!(
                "Entries: {}, Kinds: {}, Categories: {}, Builds: {}",
                self.total_entries, self.total_kinds, self.total_categories, self.build_count
            ),
            OutputFormat::Table => {
                let mut out = String::new();
                writeln!(out, "+------------------+--------+").unwrap();
                writeln!(out, "| Metric           | Value  |").unwrap();
                writeln!(out, "+------------------+--------+").unwrap();
                writeln!(out, "| Total Entries    | {:6} |", self.total_entries).unwrap();
                writeln!(out, "| Total Kinds      | {:6} |", self.total_kinds).unwrap();
                writeln!(out, "| Total Categories | {:6} |", self.total_categories).unwrap();
                writeln!(out, "| Build Count      | {:6} |", self.build_count).unwrap();
                writeln!(out, "+------------------+--------+").unwrap();
                out
            }
            OutputFormat::Json | OutputFormat::JsonCompact => {
                format!(
                    r#"{{"total_entries":{},"total_kinds":{},"total_categories":{},"build_count":{}}}"#,
                    self.total_entries, self.total_kinds, self.total_categories, self.build_count
                )
            }
            OutputFormat::Compact => format!(
                "Stats({}/{}/{})",
                self.total_entries, self.total_kinds, self.total_categories
            ),
            _ => self.format(&OutputFormat::Text),
        }
    }
}

// ============================================================================
// Display implementations using Formatter
// ============================================================================

/// Display wrapper that uses a specific format
pub struct Formatted<'a, T: Formatter> {
    value: &'a T,
    format: OutputFormat,
}

impl<'a, T: Formatter> Formatted<'a, T> {
    pub fn new(value: &'a T, format: OutputFormat) -> Self {
        Self { value, format }
    }
}

impl<T: Formatter> fmt::Display for Formatted<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value.format(&self.format))
    }
}

/// Extension trait to easily format values
pub trait FormatExt: Formatter + Sized {
    fn formatted(&self, format: OutputFormat) -> Formatted<'_, Self> {
        Formatted::new(self, format)
    }

    fn display_as(&self, format: &str) -> String {
        self.format(&OutputFormat::from_str(format))
    }
}

impl<T: Formatter + Sized> FormatExt for T {}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_output_format_from_str() {
        assert_eq!(OutputFormat::from_str("json"), OutputFormat::Json);
        assert_eq!(OutputFormat::from_str("TABLE"), OutputFormat::Table);
        assert_eq!(OutputFormat::from_str("md"), OutputFormat::Markdown);
    }

    #[test]
    fn test_entry_formatter() {
        let entry = Entry::node("Test", "example");

        let text = entry.to_text();
        assert!(text.contains("[Test]"));

        let compact = entry.to_compact();
        assert!(compact.contains("Test"));
    }

    #[test]
    fn test_web_formatter() {
        let web = Web::new();

        let text = web.to_text();
        assert!(text.contains("Web Database"));

        let compact = web.to_compact();
        assert!(compact.starts_with("Web("));
    }

    #[test]
    fn test_format_ext() {
        let entry = Entry::i32(42);
        let formatted = entry.display_as("json");
        assert!(formatted.contains("42"));
    }
}
