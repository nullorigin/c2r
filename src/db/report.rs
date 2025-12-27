//! Generic Report System
//!
//! Flexible report storage, querying, and formatting for any type of structured data.

use crate::db::web::{Build, Entry};
use std::collections::HashMap;
use std::fmt::Write;
use std::time::SystemTime;

// ============================================================================
// Report Structure
// ============================================================================

/// A generic report that can represent any structured data
#[derive(Debug, Clone)]
pub struct Report {
    /// Unique report identifier
    pub id: String,

    /// Report type/category
    pub report_type: String,

    /// Primary message or title
    pub message: String,

    /// Structured fields
    pub fields: HashMap<String, Entry>,

    /// Related entity IDs
    pub relations: HashMap<String, String>,

    /// Nested sub-reports
    pub children: Vec<Report>,

    /// Tags for categorization
    pub tags: Vec<String>,

    /// Creation timestamp
    pub created_at: SystemTime,
}

impl Report {
    /// Create a new report
    pub fn new(
        id: impl Into<String>,
        report_type: impl Into<String>,
        message: impl Into<String>,
    ) -> Self {
        Self {
            id: id.into(),
            report_type: report_type.into(),
            message: message.into(),
            fields: HashMap::new(),
            relations: HashMap::new(),
            children: Vec::new(),
            tags: Vec::new(),
            created_at: SystemTime::now(),
        }
    }

    /// Add a field
    pub fn with_field(mut self, key: impl Into<String>, value: Entry) -> Self {
        self.fields.insert(key.into(), value);
        self
    }

    /// Add a string field
    pub fn with_string(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.fields.insert(key.into(), Entry::string(value.into()));
        self
    }

    /// Add a number field
    pub fn with_number(mut self, key: impl Into<String>, value: f64) -> Self {
        self.fields.insert(key.into(), Entry::f64(value));
        self
    }

    /// Add a boolean field
    pub fn with_bool(mut self, key: impl Into<String>, value: bool) -> Self {
        self.fields.insert(key.into(), Entry::bool(value));
        self
    }

    /// Add a relation
    pub fn with_relation(
        mut self,
        rel_type: impl Into<String>,
        entity_id: impl Into<String>,
    ) -> Self {
        self.relations.insert(rel_type.into(), entity_id.into());
        self
    }

    /// Add a child report
    pub fn with_child(mut self, child: Report) -> Self {
        self.children.push(child);
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

    /// Get a field value
    pub fn get_field(&self, key: &str) -> Option<&Entry> {
        self.fields.get(key)
    }

    /// Get a string field
    pub fn get_string(&self, key: &str) -> Option<&str> {
        self.fields.get(key).and_then(|v| match v {
            Entry::String(s, _) => Some(s.as_str()),
            _ => None,
        })
    }

    /// Check if has a tag
    pub fn has_tag(&self, tag: &str) -> bool {
        self.tags.iter().any(|t| t == tag)
    }

    /// Get total child count (recursive)
    pub fn total_children(&self) -> usize {
        self.children.len()
            + self
                .children
                .iter()
                .map(|c| c.total_children())
                .sum::<usize>()
    }

    /// Convert to Entry representation
    pub fn to_entry(&self) -> Entry {
        let mut node = Entry::node("Report", &self.id);
        node.set_attr("report_type", Entry::string(&self.report_type));
        node.set_attr("message", Entry::string(&self.message));

        // Store fields
        if !self.fields.is_empty() {
            let fields_vec: Vec<Entry> = self
                .fields
                .iter()
                .map(|(k, v)| {
                    let mut field = Entry::node("Field", k);
                    field.set_attr("value", v.clone());
                    field
                })
                .collect();
            node.set_attr("fields", Entry::vec(fields_vec));
        }

        // Store relations
        if !self.relations.is_empty() {
            let rels_vec: Vec<Entry> = self
                .relations
                .iter()
                .map(|(k, v)| {
                    let mut rel = Entry::node("Relation", k);
                    rel.set_attr("entity_id", Entry::string(v));
                    rel
                })
                .collect();
            node.set_attr("relations", Entry::vec(rels_vec));
        }

        // Store tags
        if !self.tags.is_empty() {
            let tags: Vec<Entry> = self.tags.iter().map(|t| Entry::string(t)).collect();
            node.set_attr("tags", Entry::vec(tags));
        }

        // Store children recursively
        if !self.children.is_empty() {
            let children: Vec<Entry> = self.children.iter().map(|c| c.to_entry()).collect();
            node.set_attr("children", Entry::vec(children));
        }

        node
    }

    /// Reconstruct a Report from an Entry
    pub fn from_entry(entry: &Entry) -> Option<Report> {
        if entry.kind() != Some("Report") {
            return None;
        }

        let id = entry.name()?.to_string();
        let report_type = entry
            .get_string_attr("report_type")
            .unwrap_or("")
            .to_string();
        let message = entry.get_string_attr("message").unwrap_or("").to_string();

        let mut report = Report::new(id, report_type, message);

        // Reconstruct tags
        if let Some(tags_vec) = entry.get_vec_attr("tags") {
            for tag_entry in tags_vec {
                if let Entry::String(s, _) = tag_entry {
                    report.tags.push(s.clone());
                }
            }
        }

        // Fields and relations would need more complex reconstruction
        // For now, return basic structure
        Some(report)
    }
}

/// Implement Build trait for Report
impl Build for Report {
    fn to_entry(&self) -> Entry {
        Report::to_entry(self)
    }

    fn kind(&self) -> &str {
        "Report"
    }

    fn name(&self) -> Option<&str> {
        Some(&self.id)
    }

    fn category(&self) -> Option<&str> {
        if self.report_type.is_empty() {
            None
        } else {
            Some(&self.report_type)
        }
    }
}

// Note: The Reports container has been removed. Use Web for unified storage:
//   let mut web = Web::new();
//   web.add(&report);  // Add Report via Build trait
//   web.by_kind("Report");  // Query all reports
//   web.by_category("analysis");  // Query by report_type (stored as category)
//   web.by_name("report_id");  // Query by ID

// ============================================================================
// Report Formatting
// ============================================================================

// OutputFormat is now defined in format.rs and re-exported from mod.rs
use crate::db::format::OutputFormat;

/// Format a report for output
pub fn format_report(report: &Report, format: &OutputFormat) -> String {
    match format {
        OutputFormat::Text => format_text(report, 0),
        OutputFormat::Table => format_table(report),
        OutputFormat::Json | OutputFormat::JsonCompact => format_json(report),
        OutputFormat::Compact => format_compact(report),
        OutputFormat::Tree => format_tree(report, 0),
        OutputFormat::Markdown => format_markdown(report),
        OutputFormat::Csv => format_csv(report),
        OutputFormat::Custom(template) => format_custom(report, template),
    }
}

fn format_text(report: &Report, indent: usize) -> String {
    let mut out = String::new();
    let prefix = "  ".repeat(indent);

    writeln!(out, "{}[{}] {}", prefix, report.report_type, report.message).unwrap();

    for (key, value) in &report.fields {
        writeln!(out, "{}  {}: {}", prefix, key, entry_to_string(value)).unwrap();
    }

    if !report.tags.is_empty() {
        writeln!(out, "{}  tags: {}", prefix, report.tags.join(", ")).unwrap();
    }

    for child in &report.children {
        out.push_str(&format_text(child, indent + 1));
    }

    out
}

fn format_table(report: &Report) -> String {
    let mut out = String::new();

    writeln!(out, "+{}+", "-".repeat(78)).unwrap();
    writeln!(
        out,
        "| {:76} |",
        format!("{}: {}", report.report_type, report.message)
    )
    .unwrap();
    writeln!(out, "+{}+", "-".repeat(78)).unwrap();

    for (key, value) in &report.fields {
        writeln!(out, "| {:20} | {:53} |", key, entry_to_string(value)).unwrap();
    }

    writeln!(out, "+{}+", "-".repeat(78)).unwrap();
    out
}

fn format_json(report: &Report) -> String {
    let mut out = String::new();
    out.push_str("{\n");

    writeln!(out, "  \"id\": \"{}\",", report.id).unwrap();
    writeln!(out, "  \"type\": \"{}\",", report.report_type).unwrap();
    writeln!(out, "  \"message\": \"{}\",", escape_json(&report.message)).unwrap();

    if !report.fields.is_empty() {
        out.push_str("  \"fields\": {\n");
        let fields: Vec<_> = report.fields.iter().collect();
        for (i, (key, value)) in fields.iter().enumerate() {
            write!(out, "    \"{}\": {}", key, entry_to_json(value)).unwrap();
            if i < fields.len() - 1 {
                out.push(',');
            }
            out.push('\n');
        }
        out.push_str("  },\n");
    }

    if !report.tags.is_empty() {
        writeln!(
            out,
            "  \"tags\": [{}]",
            report
                .tags
                .iter()
                .map(|t| format!("\"{}\"", t))
                .collect::<Vec<_>>()
                .join(", ")
        )
        .unwrap();
    }

    out.push('}');
    out
}

fn format_compact(report: &Report) -> String {
    format!(
        "[{}] {} ({})",
        report.report_type, report.message, report.id
    )
}

fn format_tree(report: &Report, depth: usize) -> String {
    let mut out = String::new();
    let prefix = if depth == 0 {
        ""
    } else {
        &"│  ".repeat(depth - 1)
    };
    let branch = if depth == 0 { "" } else { "├─ " };

    writeln!(
        out,
        "{}{}[{}] {}",
        prefix, branch, report.report_type, report.message
    )
    .unwrap();

    for child in &report.children {
        out.push_str(&format_tree(child, depth + 1));
    }

    out
}

fn format_custom(report: &Report, template: &str) -> String {
    let mut result = template.to_string();
    result = result.replace("{id}", &report.id);
    result = result.replace("{type}", &report.report_type);
    result = result.replace("{message}", &report.message);

    for (key, value) in &report.fields {
        result = result.replace(&format!("{{field:{}}}", key), &entry_to_string(value));
    }

    result
}

fn format_markdown(report: &Report) -> String {
    let mut out = String::new();

    writeln!(out, "## {} `{}`\n", report.report_type, report.id).unwrap();
    writeln!(out, "{}\n", report.message).unwrap();

    if !report.fields.is_empty() {
        writeln!(out, "| Field | Value |").unwrap();
        writeln!(out, "|-------|-------|").unwrap();
        for (key, value) in &report.fields {
            writeln!(out, "| {} | {} |", key, entry_to_string(value)).unwrap();
        }
        writeln!(out).unwrap();
    }

    if !report.tags.is_empty() {
        writeln!(out, "**Tags:** {}\n", report.tags.join(", ")).unwrap();
    }

    for child in &report.children {
        out.push_str(&format_markdown(child));
    }

    out
}

fn format_csv(report: &Report) -> String {
    let mut out = String::new();

    // Header
    let mut headers = vec!["id", "type", "message"];
    let field_keys: Vec<&String> = report.fields.keys().collect();
    headers.extend(field_keys.iter().map(|k| k.as_str()));
    writeln!(out, "{}", headers.join(",")).unwrap();

    // Data row
    let mut values = vec![
        csv_escape_report(&report.id),
        csv_escape_report(&report.report_type),
        csv_escape_report(&report.message),
    ];
    for key in &field_keys {
        if let Some(val) = report.fields.get(*key) {
            values.push(csv_escape_report(&entry_to_string(val)));
        }
    }
    writeln!(out, "{}", values.join(",")).unwrap();

    out
}

fn csv_escape_report(s: &str) -> String {
    if s.contains(',') || s.contains('"') || s.contains('\n') {
        format!("\"{}\"", s.replace('"', "\"\""))
    } else {
        s.to_string()
    }
}

fn entry_to_string(entry: &Entry) -> String {
    match entry {
        Entry::String(s, _) => s.clone(),
        Entry::Bool(b, _) => b.to_string(),
        Entry::I32(n, _) => n.to_string(),
        Entry::I64(n, _) => n.to_string(),
        Entry::F64(n, _) => n.to_string(),
        Entry::Usize(n, _) => n.to_string(),
        Entry::Unit(_) => "null".to_string(),
        _ => format!("<{}>", entry.type_name()),
    }
}

fn entry_to_json(entry: &Entry) -> String {
    match entry {
        Entry::String(s, _) => format!("\"{}\"", escape_json(s)),
        Entry::Bool(b, _) => b.to_string(),
        Entry::I32(n, _) => n.to_string(),
        Entry::I64(n, _) => n.to_string(),
        Entry::F64(n, _) => n.to_string(),
        Entry::Usize(n, _) => n.to_string(),
        Entry::Unit(_) => "null".to_string(),
        _ => format!("\"<{}>\"", entry.type_name()),
    }
}

fn escape_json(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
        .replace('\t', "\\t")
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_report_creation() {
        let report = Report::new("r1", "handler", "Function converted successfully")
            .with_string("function", "main")
            .with_number("tokens", 42.0)
            .with_tag("success");

        assert_eq!(report.id, "r1");
        assert_eq!(report.report_type, "handler");
        assert!(report.has_tag("success"));
        assert_eq!(report.get_string("function"), Some("main"));
    }

    // Note: Reports container tests removed - use Web for unified storage
    // See web.rs tests for Web-based report storage tests

    #[test]
    fn test_report_formatting() {
        let report = Report::new("r1", "test", "Test message").with_string("key", "value");

        let text = format_report(&report, &OutputFormat::Text);
        assert!(text.contains("[test]"));
        assert!(text.contains("Test message"));

        let json = format_report(&report, &OutputFormat::Json);
        assert!(json.contains("\"type\": \"test\""));
    }
}
