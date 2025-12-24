#![allow(
    unsafe_code,
    static_mut_refs,
    unused_variables,
    unused_assignments,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals
)]
use std::path::PathBuf;
use std::fmt::{self, Write};

use crate::{Error, db::{Entry, Report, Web, WebStats}, lock::LazyRwLock, thread::shutdown_global_pool};
use crate::db::format::{Formatter, OutputFormat};
use crate::db::filter::{Kind, Category, KindFilter, CategoryFilter};

#[macro_export]
#[allow(static_mut_refs, unsafe_code)]
macro_rules! system {
    [w] => {
        $crate::config::system()
    };
    [r] => {
        &*$crate::config::system()
    };
    [r,$field:ident] => {
        &$crate::config::system().$field
    };
    [$field:ident: $value:expr] => {
        $crate::config::system().$field = $value
    };
}

// Global database accessors using LazyRwLock for lazy initialization
// LazyRwLock is unconditionally Sync (like std::Mutex) since it provides
// its own synchronization, allowing regular static usage.

static SYSTEM: LazyRwLock<System> = LazyRwLock::new();

pub fn system() -> &'static System {
    SYSTEM.get_or_init(System::new)
}

pub fn system_mut() -> &'static mut System {
    SYSTEM.get_or_init_mut(System::new)
}
#[derive(Debug, Clone)]
pub struct System {
    db: Web,
}
impl System {
    #[allow(static_mut_refs, unsafe_code)]
    pub fn new() -> Self {
        System { db: Web::new() }
    }
    pub fn update(self) -> crate::Result<Self> {
        if self.db.is_empty() {
            return Err(Error::new(crate::Kind::Database, crate::Reason::Empty("system database"), Some("Cannot sync an empty database. Did you call the 'initialize' function yet?".to_string())));
        }
        *system_mut() = self.clone();
        Ok(self)
    }
    /// Shutdown all background threads and resources
    pub fn shutdown(&mut self, export: bool) {
        // Shutdown global thread pool
        shutdown_global_pool();
        if export {
            let file_path = PathBuf::new();
            self.db.save(file_path, "json");
        }
    }
    /// Initialize the system
    pub fn initialize(&mut self, import: bool) {
        if import {
            let file_path = PathBuf::new();
            self.db.load(file_path, "json");
        }
        self.db.init();
    }

    /// Clear the system database completely
    pub fn clear(&mut self) {
        if !self.db.is_empty() {
            self.db.clear();
        }
    }

    /// Display the entire database and all other system information in a specified format for debugging
    pub fn display(&self) {
        self.db.display();
    }

    /// Display the system database's statistics for debugging
    pub fn display_stats(&self) {
        let stats = self.db.stats();
        println!("
        Total Entries: {}\n 
        Total Categories: {}\n
        Total Kinds: {}\n
        Build Count: {}",stats.total_entries,stats.total_categories,stats.total_kinds,stats.build_count);
    }

    /// Display all reports contained within the system database
    pub fn reports(&self) -> Vec<Report> {
        let report_entries = self.db.by_kind("Report");
        let mut reports = Vec::<Report>::with_capacity(report_entries.len() * 2);
        report_entries.iter().for_each(|entry| {
            if let Some(report) = Report::from_entry(entry) {
                reports.push(report);
            }
        });
        reports
    }
    
    // ========================================================================
    // Data Retrieval and Filtering Functions
    // ========================================================================
    
    /// Get direct access to the underlying Web database
    pub fn db(&self) -> &Web {
        &self.db
    }
    
    /// Get mutable access to the underlying Web database
    pub fn db_mut(&mut self) -> &mut Web {
        &mut self.db
    }
    
    /// Get database statistics
    pub fn stats(&self) -> WebStats {
        self.db.stats().clone()
    }
    
    /// Get all available kinds in the database
    pub fn kinds(&self) -> Vec<&str> {
        self.db.kinds()
    }
    
    /// Get all available categories in the database
    pub fn categories(&self) -> Vec<&str> {
        self.db.categories()
    }
    
    /// Filter entries by kind (e.g., "Function", "Report", "Struct")
    /// Accepts Kind enum, &str, or String
    pub fn by_kind(&self, kind: impl Into<KindFilter>) -> Vec<&Entry> {
        let filter: KindFilter = kind.into();
        self.db.by_kind(filter.as_str())
    }
    
    /// Filter entries by category
    /// Accepts Category enum, &str, or String
    pub fn by_category(&self, category: impl Into<CategoryFilter>) -> Vec<&Entry> {
        let filter: CategoryFilter = category.into();
        self.db.by_category(filter.as_str())
    }
    
    /// Filter entries by name
    pub fn by_name(&self, name: &str) -> Vec<&Entry> {
        self.db.by_name(name)
    }
    
    /// Filter entries by multiple kinds
    /// Accepts slice of Kind enums
    pub fn by_kinds_enum(&self, kinds: &[Kind]) -> Vec<&Entry> {
        kinds.iter()
            .flat_map(|k| self.db.by_kind(k.as_str()))
            .collect()
    }
    
    /// Filter entries by multiple kind strings
    pub fn by_kinds(&self, kinds: &[&str]) -> Vec<&Entry> {
        kinds.iter()
            .flat_map(|k| self.db.by_kind(k))
            .collect()
    }
    
    /// Filter entries by multiple categories
    /// Accepts slice of Category enums
    pub fn by_categories_enum(&self, categories: &[Category]) -> Vec<&Entry> {
        categories.iter()
            .flat_map(|c| self.db.by_category(c.as_str()))
            .collect()
    }
    
    /// Filter entries by multiple category strings
    pub fn by_categories(&self, categories: &[&str]) -> Vec<&Entry> {
        categories.iter()
            .flat_map(|c| self.db.by_category(c))
            .collect()
    }
    
    /// Filter entries by kind and category (intersection)
    /// Accepts Kind/Category enums, &str, or String
    pub fn by_kind_and_category(
        &self, 
        kind: impl Into<KindFilter>, 
        category: impl Into<CategoryFilter>
    ) -> Vec<&Entry> {
        let kind_filter: KindFilter = kind.into();
        let cat_filter: CategoryFilter = category.into();
        self.db.by_kind(kind_filter.as_str())
            .into_iter()
            .filter(|e| e.get_string_attr("category") == Some(cat_filter.as_str()))
            .collect()
    }
    
    /// Get all entries as an iterator
    pub fn entries(&self) -> impl Iterator<Item = &Entry> {
        self.db.iter()
    }
    
    /// Count entries by kind
    pub fn count_by_kind(&self, kind: &str) -> usize {
        self.db.by_kind(kind).len()
    }
    
    /// Count entries by category
    pub fn count_by_category(&self, category: &str) -> usize {
        self.db.by_category(category).len()
    }
    
    /// Get summary counts for all kinds
    pub fn kind_counts(&self) -> Vec<(&str, usize)> {
        self.kinds().into_iter()
            .map(|k| (k, self.count_by_kind(k)))
            .collect()
    }
    
    /// Get summary counts for all categories
    pub fn category_counts(&self) -> Vec<(&str, usize)> {
        self.categories().into_iter()
            .map(|c| (c, self.count_by_category(c)))
            .collect()
    }
    
    /// Check if database is empty
    pub fn is_empty(&self) -> bool {
        self.db.is_empty()
    }
    
    /// Get total entry count
    pub fn len(&self) -> usize {
        self.db.len()
    }
    
    // ========================================================================
    // Display Functions with Format Support
    // ========================================================================
    
    /// Display the system using specified output format
    pub fn display_as(&self, format: &str) {
        println!("{}", self.format(&OutputFormat::from_str(format)));
    }
    
    /// Display entries filtered by kind
    pub fn display_kind(&self, kind: &str, format: &OutputFormat) {
        let entries = self.by_kind(kind);
        println!("=== {} ({}) ===", kind, entries.len());
        for entry in entries {
            println!("{}", entry.format(format));
        }
    }
    
    /// Display entries filtered by category
    pub fn display_category(&self, category: &str, format: &OutputFormat) {
        let entries = self.by_category(category);
        println!("=== Category: {} ({}) ===", category, entries.len());
        for entry in entries {
            println!("{}", entry.format(format));
        }
    }
}

// ============================================================================
// Formatter Implementation for System
// ============================================================================

impl Formatter for System {
    fn format(&self, fmt: &OutputFormat) -> String {
        match fmt {
            OutputFormat::Text => format_system_text(self),
            OutputFormat::Table => format_system_table(self),
            OutputFormat::Json | OutputFormat::JsonCompact => format_system_json(self, matches!(fmt, OutputFormat::Json)),
            OutputFormat::Tree => format_system_tree(self),
            OutputFormat::Compact => format_system_compact(self),
            OutputFormat::Markdown => format_system_markdown(self),
            OutputFormat::Csv => format_system_csv(self),
            OutputFormat::Custom(template) => format_system_custom(self, template),
        }
    }
}

fn format_system_text(system: &System) -> String {
    let mut out = String::new();
    let stats = system.stats();
    
    writeln!(out, "SYSTEM DATABASE").unwrap();
    writeln!(out, "================").unwrap();
    writeln!(out, "Total Entries: {}", stats.total_entries).unwrap();
    writeln!(out, "Total Kinds: {}", stats.total_kinds).unwrap();
    writeln!(out, "Total Categories: {}", stats.total_categories).unwrap();
    writeln!(out, "Build Count: {}", stats.build_count).unwrap();
    writeln!(out).unwrap();
    
    // List kinds with counts
    writeln!(out, "ENTRIES BY KIND:").unwrap();
    for (kind, count) in system.kind_counts() {
        writeln!(out, "  [{}]: {} entries", kind, count).unwrap();
    }
    writeln!(out).unwrap();
    
    // List categories with counts
    if !system.categories().is_empty() {
        writeln!(out, "ENTRIES BY CATEGORY:").unwrap();
        for (cat, count) in system.category_counts() {
            writeln!(out, "  <{}>: {} entries", cat, count).unwrap();
        }
    }
    
    out
}

fn format_system_table(system: &System) -> String {
    let mut out = String::new();
    let stats = system.stats();
    let width = 78;
    
    writeln!(out, "+{}+", "=".repeat(width)).unwrap();
    writeln!(out, "| {:^width$} |", "SYSTEM DATABASE", width = width - 2).unwrap();
    writeln!(out, "+{}+", "=".repeat(width)).unwrap();
    
    // Stats section
    writeln!(out, "| {:30} | {:width$} |", "Total Entries", stats.total_entries, width = width - 35).unwrap();
    writeln!(out, "| {:30} | {:width$} |", "Total Kinds", stats.total_kinds, width = width - 35).unwrap();
    writeln!(out, "| {:30} | {:width$} |", "Total Categories", stats.total_categories, width = width - 35).unwrap();
    writeln!(out, "| {:30} | {:width$} |", "Build Count", stats.build_count, width = width - 35).unwrap();
    writeln!(out, "+{}+", "-".repeat(width)).unwrap();
    
    // Kinds section
    writeln!(out, "| {:^width$} |", "ENTRIES BY KIND", width = width - 2).unwrap();
    writeln!(out, "+{}+", "-".repeat(width)).unwrap();
    for (kind, count) in system.kind_counts() {
        writeln!(out, "| {:30} | {:width$} |", kind, count, width = width - 35).unwrap();
    }
    writeln!(out, "+{}+", "-".repeat(width)).unwrap();
    
    // Categories section
    if !system.categories().is_empty() {
        writeln!(out, "| {:^width$} |", "ENTRIES BY CATEGORY", width = width - 2).unwrap();
        writeln!(out, "+{}+", "-".repeat(width)).unwrap();
        for (cat, count) in system.category_counts() {
            writeln!(out, "| {:30} | {:width$} |", cat, count, width = width - 35).unwrap();
        }
        writeln!(out, "+{}+", "=".repeat(width)).unwrap();
    }
    
    out
}

fn format_system_json(system: &System, pretty: bool) -> String {
    use std::collections::HashMap;
    
    let stats = system.stats();
    let mut root = HashMap::new();
    
    // Stats
    let mut stats_map = HashMap::new();
    stats_map.insert("total_entries".to_string(), Entry::usize(stats.total_entries));
    stats_map.insert("total_kinds".to_string(), Entry::usize(stats.total_kinds));
    stats_map.insert("total_categories".to_string(), Entry::usize(stats.total_categories));
    stats_map.insert("build_count".to_string(), Entry::usize(stats.build_count));
    root.insert("stats".to_string(), Entry::hashmap(stats_map));
    
    // Kinds with counts
    let mut kinds_map = HashMap::new();
    for (kind, count) in system.kind_counts() {
        kinds_map.insert(kind.to_string(), Entry::usize(count));
    }
    root.insert("kinds".to_string(), Entry::hashmap(kinds_map));
    
    // Categories with counts
    let mut cats_map = HashMap::new();
    for (cat, count) in system.category_counts() {
        cats_map.insert(cat.to_string(), Entry::usize(count));
    }
    root.insert("categories".to_string(), Entry::hashmap(cats_map));
    
    // Entries by kind
    let mut entries_map = HashMap::new();
    for kind in system.kinds() {
        let entries: Vec<Entry> = system.db.by_kind(kind).iter().map(|e| (*e).clone()).collect();
        entries_map.insert(kind.to_string(), Entry::vec(entries));
    }
    root.insert("entries".to_string(), Entry::hashmap(entries_map));
    
    let root_entry = Entry::hashmap(root);
    if pretty {
        root_entry.to_json_pretty(2)
    } else {
        root_entry.to_json()
    }
}

fn format_system_tree(system: &System) -> String {
    let mut out = String::new();
    let stats = system.stats();
    
    writeln!(out, "🖥️  System Database").unwrap();
    writeln!(out, "├─ 📊 Stats").unwrap();
    writeln!(out, "│  ├─ Entries: {}", stats.total_entries).unwrap();
    writeln!(out, "│  ├─ Kinds: {}", stats.total_kinds).unwrap();
    writeln!(out, "│  ├─ Categories: {}", stats.total_categories).unwrap();
    writeln!(out, "│  └─ Builds: {}", stats.build_count).unwrap();
    
    let kinds = system.kinds();
    let kinds_count = kinds.len();
    
    if kinds_count > 0 {
        writeln!(out, "├─ 📁 Kinds").unwrap();
        for (i, kind) in kinds.iter().enumerate() {
            let is_last = i == kinds_count - 1;
            let branch = if is_last { "└─" } else { "├─" };
            let prefix = if is_last { "   " } else { "│  " };
            
            let entries = system.db.by_kind(kind);
            writeln!(out, "│  {} {} ({})", branch, kind, entries.len()).unwrap();
            
            // Show first few entries
            for (j, entry) in entries.iter().enumerate().take(3) {
                let entry_branch = if j == entries.len().min(3) - 1 { "└─" } else { "├─" };
                let name = entry.name().unwrap_or("unnamed");
                writeln!(out, "│  {}  {} {}", prefix, entry_branch, name).unwrap();
            }
            if entries.len() > 3 {
                writeln!(out, "│  {}  └─ ... {} more", prefix, entries.len() - 3).unwrap();
            }
        }
    }
    
    let cats = system.categories();
    if !cats.is_empty() {
        writeln!(out, "└─ 🏷️  Categories").unwrap();
        for (i, cat) in cats.iter().enumerate() {
            let is_last = i == cats.len() - 1;
            let branch = if is_last { "└─" } else { "├─" };
            let count = system.count_by_category(cat);
            writeln!(out, "   {} {} ({})", branch, cat, count).unwrap();
        }
    }
    
    out
}

fn format_system_compact(system: &System) -> String {
    let stats = system.stats();
    format!("System({} entries, {} kinds, {} categories)", 
        stats.total_entries, stats.total_kinds, stats.total_categories)
}

fn format_system_markdown(system: &System) -> String {
    let mut out = String::new();
    let stats = system.stats();
    
    writeln!(out, "# System Database\n").unwrap();
    
    writeln!(out, "## Statistics\n").unwrap();
    writeln!(out, "| Metric | Value |").unwrap();
    writeln!(out, "|--------|-------|").unwrap();
    writeln!(out, "| Total Entries | {} |", stats.total_entries).unwrap();
    writeln!(out, "| Total Kinds | {} |", stats.total_kinds).unwrap();
    writeln!(out, "| Total Categories | {} |", stats.total_categories).unwrap();
    writeln!(out, "| Build Count | {} |", stats.build_count).unwrap();
    writeln!(out).unwrap();
    
    writeln!(out, "## Entries by Kind\n").unwrap();
    writeln!(out, "| Kind | Count |").unwrap();
    writeln!(out, "|------|-------|").unwrap();
    for (kind, count) in system.kind_counts() {
        writeln!(out, "| {} | {} |", kind, count).unwrap();
    }
    writeln!(out).unwrap();
    
    if !system.categories().is_empty() {
        writeln!(out, "## Entries by Category\n").unwrap();
        writeln!(out, "| Category | Count |").unwrap();
        writeln!(out, "|----------|-------|").unwrap();
        for (cat, count) in system.category_counts() {
            writeln!(out, "| {} | {} |", cat, count).unwrap();
        }
    }
    
    out
}

fn format_system_csv(system: &System) -> String {
    let mut out = String::new();
    
    writeln!(out, "type,name,count").unwrap();
    
    // Stats row
    let stats = system.stats();
    writeln!(out, "stats,total_entries,{}", stats.total_entries).unwrap();
    writeln!(out, "stats,total_kinds,{}", stats.total_kinds).unwrap();
    writeln!(out, "stats,total_categories,{}", stats.total_categories).unwrap();
    writeln!(out, "stats,build_count,{}", stats.build_count).unwrap();
    
    // Kinds
    for (kind, count) in system.kind_counts() {
        writeln!(out, "kind,{},{}", kind, count).unwrap();
    }
    
    // Categories
    for (cat, count) in system.category_counts() {
        writeln!(out, "category,{},{}", cat, count).unwrap();
    }
    
    out
}

fn format_system_custom(system: &System, template: &str) -> String {
    let stats = system.stats();
    let mut result = template.to_string();
    
    result = result.replace("{entries}", &stats.total_entries.to_string());
    result = result.replace("{kinds}", &stats.total_kinds.to_string());
    result = result.replace("{categories}", &stats.total_categories.to_string());
    result = result.replace("{builds}", &stats.build_count.to_string());
    
    result
}

// ============================================================================
// Display Implementation for System
// ============================================================================

impl fmt::Display for System {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Default to table format for Display
        write!(f, "{}", self.format(&OutputFormat::Table))
    }
}
