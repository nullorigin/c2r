//! Annotation and Metadata Storage
//!
//! Provides structures for storing annotations, comments, and metadata
//! associated with code elements.

use crate::db::web::{Entry, Build};
use std::collections::HashMap;
use std::ops::Range;
use std::time::SystemTime;

// ============================================================================
// Annotation Types
// ============================================================================

/// An annotation attached to a code element
#[derive(Debug, Clone, PartialEq)]
pub struct Annotation {
    /// Unique annotation ID
    pub id: String,
    
    /// Type of annotation
    pub annotation_type: AnnotationType,
    
    /// The annotation message or content
    pub content: String,
    
    /// Source location (slot_id, range)
    pub location: Option<AnnotationLocation>,
    
    /// Severity level
    pub severity: Severity,
    
    /// Additional metadata
    pub metadata: HashMap<String, String>,
    
    /// Creation timestamp
    pub created_at: SystemTime,
}

/// Location of an annotation in source
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AnnotationLocation {
    /// Slot ID this annotation belongs to
    pub slot_id: usize,
    
    /// Token range
    pub range: Range<usize>,
    
    /// Source file
    pub source_file: Option<String>,
    
    /// Line number
    pub line: Option<usize>,
    
    /// Column number
    pub column: Option<usize>,
}

impl AnnotationLocation {
    pub fn new(slot_id: usize, range: Range<usize>) -> Self {
        Self {
            slot_id,
            range,
            source_file: None,
            line: None,
            column: None,
        }
    }
    
    pub fn with_file(mut self, file: impl Into<String>) -> Self {
        self.source_file = Some(file.into());
        self
    }
    
    pub fn with_line(mut self, line: usize) -> Self {
        self.line = Some(line);
        self
    }
    
    pub fn with_column(mut self, column: usize) -> Self {
        self.column = Some(column);
        self
    }
    
    /// Create a location key for indexing
    pub fn key(&self) -> String {
        format!("{}::{:?}", self.slot_id, self.range)
    }
}

/// Types of annotations
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AnnotationType {
    /// General comment
    Comment,
    
    /// Warning message
    Warning,
    
    /// Error message
    Error,
    
    /// Informational note
    Info,
    
    /// Hint or suggestion
    Hint,
    
    /// Documentation
    Doc,
    
    /// TODO marker
    Todo,
    
    /// FIXME marker
    Fixme,
    
    /// Handler-generated annotation
    Handler(String),
    
    /// Custom type
    Custom(String),
}

impl AnnotationType {
    pub fn as_str(&self) -> &str {
        match self {
            AnnotationType::Comment => "comment",
            AnnotationType::Warning => "warning",
            AnnotationType::Error => "error",
            AnnotationType::Info => "info",
            AnnotationType::Hint => "hint",
            AnnotationType::Doc => "doc",
            AnnotationType::Todo => "todo",
            AnnotationType::Fixme => "fixme",
            AnnotationType::Handler(s) => s.as_str(),
            AnnotationType::Custom(s) => s.as_str(),
        }
    }
}

/// Severity levels
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Severity {
    Debug = 0,
    Info = 1,
    Notice = 2,
    Warning = 3,
    Error = 4,
    Critical = 5,
}

impl Severity {
    pub fn as_str(&self) -> &str {
        match self {
            Severity::Debug => "debug",
            Severity::Info => "info",
            Severity::Notice => "notice",
            Severity::Warning => "warning",
            Severity::Error => "error",
            Severity::Critical => "critical",
        }
    }
}

impl Default for Severity {
    fn default() -> Self {
        Severity::Info
    }
}

// ============================================================================
// Annotation Builder
// ============================================================================

impl Annotation {
    /// Create a new annotation
    pub fn new(id: impl Into<String>, annotation_type: AnnotationType, content: impl Into<String>) -> Self {
        Self {
            id: id.into(),
            annotation_type,
            content: content.into(),
            location: None,
            severity: Severity::Info,
            metadata: HashMap::new(),
            created_at: SystemTime::now(),
        }
    }
    
    /// Create a comment annotation
    pub fn comment(id: impl Into<String>, content: impl Into<String>) -> Self {
        Self::new(id, AnnotationType::Comment, content)
    }
    
    /// Create a warning annotation
    pub fn warning(id: impl Into<String>, content: impl Into<String>) -> Self {
        Self::new(id, AnnotationType::Warning, content).with_severity(Severity::Warning)
    }
    
    /// Create an error annotation
    pub fn error(id: impl Into<String>, content: impl Into<String>) -> Self {
        Self::new(id, AnnotationType::Error, content).with_severity(Severity::Error)
    }
    
    /// Create an info annotation
    pub fn info(id: impl Into<String>, content: impl Into<String>) -> Self {
        Self::new(id, AnnotationType::Info, content)
    }
    
    /// Set location
    pub fn with_location(mut self, location: AnnotationLocation) -> Self {
        self.location = Some(location);
        self
    }
    
    /// Set location from slot and range
    pub fn at(mut self, slot_id: usize, range: Range<usize>) -> Self {
        self.location = Some(AnnotationLocation::new(slot_id, range));
        self
    }
    
    /// Set severity
    pub fn with_severity(mut self, severity: Severity) -> Self {
        self.severity = severity;
        self
    }
    
    /// Add metadata
    pub fn with_metadata(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.metadata.insert(key.into(), value.into());
        self
    }

    /// Convert to Entry representation
    pub fn to_entry(&self) -> Entry {
        let mut node = Entry::node("Annotation", &self.id);
        node.set_attr("annotation_type", Entry::string(self.annotation_type.as_str()));
        node.set_attr("content", Entry::string(&self.content));
        node.set_attr("severity", Entry::string(self.severity.as_str()));
        
        // Store location if present
        if let Some(ref loc) = self.location {
            let mut loc_node = Entry::node("Location", &loc.key());
            loc_node.set_attr("slot_id", Entry::usize(loc.slot_id));
            loc_node.set_attr("range_start", Entry::usize(loc.range.start));
            loc_node.set_attr("range_end", Entry::usize(loc.range.end));
            if let Some(ref file) = loc.source_file {
                loc_node.set_attr("source_file", Entry::string(file));
            }
            if let Some(line) = loc.line {
                loc_node.set_attr("line", Entry::usize(line));
            }
            if let Some(col) = loc.column {
                loc_node.set_attr("column", Entry::usize(col));
            }
            node.set_attr("location", loc_node);
        }
        
        // Store metadata
        if !self.metadata.is_empty() {
            let meta_vec: Vec<Entry> = self.metadata.iter()
                .map(|(k, v)| {
                    let mut m = Entry::node("Meta", k);
                    m.set_attr("value", Entry::string(v));
                    m
                })
                .collect();
            node.set_attr("metadata", Entry::vec(meta_vec));
        }
        
        node
    }

    /// Reconstruct an Annotation from an Entry
    pub fn from_entry(entry: &Entry) -> Option<Annotation> {
        if entry.kind() != Some("Annotation") {
            return None;
        }
        
        let id = entry.name()?.to_string();
        let type_str = entry.get_string_attr("annotation_type").unwrap_or("comment");
        let content = entry.get_string_attr("content").unwrap_or("").to_string();
        let severity_str = entry.get_string_attr("severity").unwrap_or("info");
        
        let annotation_type = match type_str {
            "comment" => AnnotationType::Comment,
            "warning" => AnnotationType::Warning,
            "error" => AnnotationType::Error,
            "info" => AnnotationType::Info,
            "hint" => AnnotationType::Hint,
            "doc" => AnnotationType::Doc,
            "todo" => AnnotationType::Todo,
            "fixme" => AnnotationType::Fixme,
            s => AnnotationType::Custom(s.to_string()),
        };
        
        let severity = match severity_str {
            "debug" => Severity::Debug,
            "info" => Severity::Info,
            "notice" => Severity::Notice,
            "warning" => Severity::Warning,
            "error" => Severity::Error,
            "critical" => Severity::Critical,
            _ => Severity::Info,
        };
        
        Some(Annotation::new(id, annotation_type, content).with_severity(severity))
    }
}

/// Implement Build trait for Annotation
impl Build for Annotation {
    fn to_entry(&self) -> Entry {
        Annotation::to_entry(self)
    }
    
    fn kind(&self) -> &str {
        "Annotation"
    }
    
    fn name(&self) -> Option<&str> {
        Some(&self.id)
    }
    
    fn category(&self) -> Option<&str> {
        Some(self.annotation_type.as_str())
    }
    
    fn priority(&self) -> i16 {
        self.severity as i16
    }
}

// Note: The Annotations container has been removed. Use Web for unified storage:
//   let mut web = Web::new();
//   web.add(&annotation);  // Add Annotation via Build trait
//   web.by_kind("Annotation");  // Query all annotations
//   web.by_category("warning");  // Query by annotation_type (stored as category)
//   web.by_name("annotation_id");  // Query by ID
//
// For severity-based queries, filter the results:
//   web.by_kind("Annotation").iter()
//      .filter_map(|e| Annotation::from_entry(e))
//      .filter(|a| a.severity == Severity::Error)

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_annotation_creation() {
        let ann = Annotation::warning("w1", "Unused variable")
            .at(0, 10..20)
            .with_metadata("variable", "x");
        
        assert_eq!(ann.id, "w1");
        assert_eq!(ann.severity, Severity::Warning);
        assert!(ann.location.is_some());
    }

    // Note: Annotations container tests removed - use Web for unified storage
    // See web.rs tests for Web-based annotation storage tests
}
