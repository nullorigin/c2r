use crate::Entry;
use crate::Handler;
use crate::HandlerReport;
use crate::Id;
use crate::Table;
use crate::TableCell;
use crate::pattern::Pattern;
use std::any::{Any, TypeId};
use std::collections::HashMap;
use std::fmt::Debug;
use std::path::PathBuf;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Registry {
    pub id: Id,
    pub entries: HashMap<Id, Entry>,
    pub entry_count: u64,
}
impl Registry {
    pub fn new(name: &str) -> Self {
        Registry {
            id: Id::get(name),
            entries: HashMap::<Id, Entry>::new(),
            entry_count: 0,
        }
    }

    /// Set an entry in the registry by name
    pub fn set_entry(&mut self, name: &str, entry: Entry) {
        let id = Id::get(name);
        self.entries.insert(id, entry);
        self.entry_count += 1;
    }

    /// Get a value from the registry with type casting
    pub fn get_value<T: Any + Clone + Debug>(&self, name: &str) -> Box<T> {
        let id = Id::get(name);
        if let Some(entry) = self.entries.get(&id) {
            let entry_type: Box<dyn Any> = match entry {
                Entry::Id(id) => Box::new(id.clone()),
                Entry::Val(val) => Box::new(val.clone()),
                Entry::Str(string) => Box::new(string.clone()),
                Entry::Bool(boolean) => Box::new(boolean.clone()),
                Entry::Func(func) => Box::new(*func),
                Entry::Path(path) => Box::new(path.clone()),
                Entry::Handler(handler) => Box::new(handler.clone()),
                Entry::HandlerReport(report) => Box::new(report.clone()),
                Entry::Pair(pair) => Box::new(pair.clone()),
                Entry::List(list) => Box::new(list.clone()),
                Entry::Table(table) => Box::new(table.clone()),
                Entry::TableCell(cell) => Box::new(cell.clone()),
                Entry::Any(any) => any.clone(),
                Entry::IdMap(map) => Box::new(map.clone()),
                Entry::ValMap(map) => Box::new(map.clone()),
                Entry::StrMap(map) => Box::new(map.clone()),
                Entry::BoolMap(map) => Box::new(map.clone()),
                Entry::FuncMap(map) => Box::new(map.clone()),
                Entry::PathMap(map) => Box::new(map.clone()),
                Entry::HandlerMap(map) => Box::new(map.clone()),
                Entry::PairMap(map) => Box::new(map.clone()),
                Entry::AnyMap(map) => Box::new(map.clone()),
                Entry::Patternizer(pattern) => Box::new(pattern.clone()),
                Entry::PatternRule(rule) => Box::new(rule.clone()),
                Entry::PatternMetrics(metrics) => Box::new(metrics.clone()),
                Entry::TokenPattern(token_pattern) => Box::new(token_pattern.clone()),
                Entry::SamplizerPattern(pattern) => Box::new(pattern.clone()),
                Entry::SamplizerFilePair(pair) => Box::new(pair.clone()),
                Entry::SamplizerValidation(validation) => Box::new(validation.clone()),
                Entry::SamplizerStats(stats) => Box::new(stats.clone()),
            };
            entry_type
                .downcast::<T>()
                .unwrap_or_else(|_| panic!("Failed to downcast entry for name: {}", name))
        } else {
            panic!("Entry not found for name: {}", name)
        }
    }

    /// Set a value in the registry with type conversion
    pub fn set_value<T: Any + Clone + Debug>(&mut self, name: &str, value: T) {
        let id = Id::get(name);
        let entry = self.from_value(value);
        self.entries.insert(id, entry);
        self.entry_count += 1;
    }

    /// Convert a value to an Entry
    fn from_value<T: Any + Clone + Debug>(&self, value: T) -> Entry {
        let any_value: &dyn Any = &value;
        let type_id = any_value.type_id();

        if type_id == TypeId::of::<Id>() {
            Entry::Id(any_value.downcast_ref::<Id>().unwrap().clone())
        } else if type_id == TypeId::of::<u64>() {
            Entry::Val(any_value.downcast_ref::<u64>().unwrap().clone())
        } else if type_id == TypeId::of::<String>() {
            Entry::Str(any_value.downcast_ref::<String>().unwrap().clone())
        } else if type_id == TypeId::of::<bool>() {
            Entry::Bool(any_value.downcast_ref::<bool>().unwrap().clone())
        } else if type_id == TypeId::of::<PathBuf>() {
            Entry::Path(any_value.downcast_ref::<PathBuf>().unwrap().clone())
        } else if type_id == TypeId::of::<Handler>() {
            Entry::Handler(any_value.downcast_ref::<Handler>().unwrap().clone())
        } else if type_id == TypeId::of::<HandlerReport>() {
            Entry::HandlerReport(any_value.downcast_ref::<HandlerReport>().unwrap().clone())
        } else if type_id == TypeId::of::<Table>() {
            Entry::Table(any_value.downcast_ref::<Table>().unwrap().clone())
        } else if type_id == TypeId::of::<Table>() {
            Entry::TableCell(any_value.downcast_ref::<TableCell>().unwrap().clone())
        } else if type_id == TypeId::of::<Vec<String>>() {
            let vec = any_value.downcast_ref::<Vec<String>>().unwrap();
            let entries: Vec<Entry> = vec.iter().map(|s| Entry::Str(s.clone())).collect();
            Entry::List(entries)
        } else if type_id == TypeId::of::<HashMap<String, String>>() {
            let map = any_value.downcast_ref::<HashMap<String, String>>().unwrap();
            let entry_map: HashMap<String, Entry> = map
                .iter()
                .map(|(k, v)| (k.clone(), Entry::Str(v.clone())))
                .collect();
            Entry::StrMap(entry_map)
        } else if type_id == TypeId::of::<Pattern>() {
            Entry::Patternizer(any_value.downcast_ref::<Pattern>().unwrap().clone())
        } else {
            panic!("Unsupported type for Entry conversion: {:?}", type_id)
        }
    }

    /// Store a Patternizer directly in the registry (bypasses dyn Any)
    pub fn store_pattern(&mut self, pattern_name: &str, pattern: Pattern) {
        let key = format!("pattern_{}", pattern_name);
        let id = Id::get(&key);
        let name = pattern.name.clone();
        self.entries.insert(id, Entry::Patternizer(pattern.clone()));
        self.entry_count += 1;
        println!(
            "📋 Stored pattern '{}' in registry with key '{}'",
            name, key
        );
    }

    /// Retrieve a Patternizer directly from the registry (bypasses dyn Any)
    pub fn get_pattern(&self, pattern_name: &str) -> Option<Pattern> {
        let key = format!("pattern_{}", pattern_name);
        let id = Id::get(&key);
        let name = pattern_name;
        match self.entries.get(&id) {
            Some(Entry::Patternizer(pattern)) => {
                println!("✅ Found pattern '{}' in registry with key '{}'", name, key);
                Some(pattern.clone())
            }
            _ => {
                println!(
                    "❌ Pattern '{}' not found in registry (key: '{}')",
                    name, key
                );
                None
            }
        }
    }

    /// Get all entries in the registry
    pub fn entries(&self) -> &HashMap<Id, Entry> {
        &self.entries
    }
    pub fn insert(&mut self, id: Id, entry: Entry) -> Option<Entry> {
        let result = self.entries.insert(id, entry);
        if result.is_none() {
            self.entry_count += 1;
        }
        result
    }

    pub fn remove(&mut self, id: &Id) -> Option<Entry> {
        let result = self.entries.remove(id);
        if result.is_some() {
            self.entry_count -= 1;
        }
        result
    }

    pub fn clear(&mut self) {
        self.entries.clear();
        self.entry_count = 0;
    }
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty() && self.entry_count == 0 && self.id.is_empty()
    }
    pub fn len(&self) -> u64 {
        self.entry_count
    }

    pub fn root(&self, id: &Id) -> Option<&Entry> {
        self.entries.get(id)
    }

    pub fn root_mut(&mut self, id: &Id) -> Option<&mut Entry> {
        self.entries.get_mut(id)
    }
    pub fn nested(&self, name: &str, depth: u64) -> Option<Entry> {
        let parts: Vec<&str> = name.split('/').collect();
        if parts.is_empty() {
            return None;
        }

        let root_key = parts[0];
        let root_id = Id::get(root_key);

        // Debug: Print registry state for pattern lookups
        if name.starts_with("pattern_") {
            println!("🔍 Registry lookup for '{}', root_name: {}", name, root_id);
            println!("🔍 Registry has {} entries:", self.entries.len());
            for (id, entry) in self.entries.iter() {
                let clean_name = id.name().to_string().replace('\n', " | ");
                let clean_entry = match entry {
                    Entry::Patternizer(p) => {
                        format!("Patternizer({})", p.name.replace('\n', " | "))
                    }
                    _ => "Other".to_string(),
                };
                println!("  - ID: {} -> {}", clean_name, clean_entry);
            }
        }

        let entry = self.entries.get(&root_id)?;

        // If we're at depth 0 or have no more path parts, return the current entry
        if depth == 0 || parts.len() == 1 {
            return Some(entry.clone());
        }

        let remaining_path = &parts[1..];
        self.traverse(entry, remaining_path, depth - 1)
    }

    fn traverse(&self, entry: &Entry, path: &[&str], remaining_depth: u64) -> Option<Entry> {
        if path.is_empty() {
            return Some(entry.clone());
        }

        let current_key = path[0];
        let next_path = &path[1..];

        match entry {
            Entry::Table(table) => {
                if let Ok(row) = current_key.parse::<usize>() {
                    if next_path.is_empty() || remaining_depth == 0 {
                        None
                    } else if let Ok(col) = next_path[0].parse::<usize>() {
                        if let Some(cell) = table.get_cell(row, col) {
                            if next_path.len() == 1 || remaining_depth == 1 {
                                Some(Entry::TableCell(cell.clone()))
                            } else {
                                self.traverse(
                                    &Entry::TableCell(cell.clone()),
                                    &next_path[1..],
                                    remaining_depth - 1,
                                )
                            }
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            Entry::TableCell(cell) => {
                if let Ok(index) = current_key.parse::<usize>() {
                    if index < cell.entries().len() {
                        let nested_entry = &cell.entries()[index];
                        if next_path.is_empty() || remaining_depth == 0 {
                            Some(nested_entry.clone())
                        } else {
                            self.traverse(nested_entry, next_path, remaining_depth - 1)
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            Entry::StrMap(map) => {
                if let Some(nested_entry) = map.get(current_key) {
                    if next_path.is_empty() || remaining_depth == 0 {
                        Some(nested_entry.clone())
                    } else {
                        self.traverse(nested_entry, next_path, remaining_depth - 1)
                    }
                } else {
                    None
                }
            }
            Entry::List(entries) => {
                if let Ok(index) = current_key.parse::<usize>() {
                    if index < entries.len() {
                        let nested_entry = &entries[index];
                        if next_path.is_empty() || remaining_depth == 0 {
                            Some(nested_entry.clone())
                        } else {
                            self.traverse(nested_entry, next_path, remaining_depth - 1)
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            Entry::Pair(pair) => match current_key {
                "left" => {
                    if next_path.is_empty() || remaining_depth == 0 {
                        Some(pair.0.clone())
                    } else {
                        self.traverse(&pair.0, next_path, remaining_depth - 1)
                    }
                }
                "right" => {
                    if next_path.is_empty() || remaining_depth == 0 {
                        Some(pair.1.clone())
                    } else {
                        self.traverse(&pair.1, next_path, remaining_depth - 1)
                    }
                }
                _ => None,
            },
            Entry::IdMap(map) => {
                let id = Id::get(current_key);
                if let Some(nested_entry) = map.get(&id) {
                    if next_path.is_empty() || remaining_depth == 0 {
                        Some(nested_entry.clone())
                    } else {
                        self.traverse(nested_entry, next_path, remaining_depth - 1)
                    }
                } else {
                    None
                }
            }
            Entry::ValMap(map) => {
                if let Ok(val) = current_key.parse::<u64>() {
                    if let Some(nested_entry) = map.get(&val) {
                        if next_path.is_empty() || remaining_depth == 0 {
                            Some(nested_entry.clone())
                        } else {
                            self.traverse(nested_entry, next_path, remaining_depth - 1)
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            Entry::BoolMap(map) => {
                if let Ok(bool_val) = current_key.parse::<bool>() {
                    if let Some(nested_entry) = map.get(&bool_val) {
                        if next_path.is_empty() || remaining_depth == 0 {
                            Some(nested_entry.clone())
                        } else {
                            self.traverse(nested_entry, next_path, remaining_depth - 1)
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            Entry::PathMap(map) => {
                let path_key = std::path::PathBuf::from(current_key);
                if let Some(nested_entry) = map.get(&path_key) {
                    if next_path.is_empty() || remaining_depth == 0 {
                        Some(nested_entry.clone())
                    } else {
                        self.traverse(nested_entry, next_path, remaining_depth - 1)
                    }
                } else {
                    None
                }
            }
            Entry::HandlerMap(map) => {
                if let Some(handler) = map.get(current_key) {
                    if next_path.is_empty() || remaining_depth == 0 {
                        Some(Entry::Handler(handler.clone()))
                    } else {
                        None // Handlers are leaf nodes
                    }
                } else {
                    None
                }
            }
            Entry::PairMap(map) => {
                if let Some(pair) = map.get(current_key) {
                    if next_path.is_empty() || remaining_depth == 0 {
                        Some(Entry::Pair(pair.clone()))
                    } else {
                        // Try to traverse into the pair
                        if next_path.len() > 0 {
                            match next_path[0] {
                                "left" => {
                                    self.traverse(&pair.0, &next_path[1..], remaining_depth - 1)
                                }
                                "right" => {
                                    self.traverse(&pair.1, &next_path[1..], remaining_depth - 1)
                                }
                                _ => None,
                            }
                        } else {
                            None
                        }
                    }
                } else {
                    None
                }
            }
            Entry::AnyMap(map) => {
                // Find first matching key
                for (k, v) in map {
                    if let Entry::Str(s) = k {
                        if s == current_key {
                            if next_path.is_empty() || remaining_depth == 0 {
                                return Some(v.clone());
                            } else {
                                return self.traverse(v, next_path, remaining_depth - 1);
                            }
                        }
                    }
                }
                None
            }
            Entry::Any(any) => {
                if remaining_depth > 0 {
                    self.traverse(any, path, remaining_depth - 1)
                } else {
                    None
                }
            }
            // Leaf nodes - return None for further traversal
            Entry::Id(_)
            | Entry::Val(_)
            | Entry::Str(_)
            | Entry::Bool(_)
            | Entry::Func(_)
            | Entry::Path(_)
            | Entry::Handler(_)
            | Entry::HandlerReport(_)
            | Entry::Patternizer(_)
            | Entry::PatternRule(_)
            | Entry::PatternMetrics(_)
            | Entry::TokenPattern(_)
            | Entry::SamplizerPattern(_)
            | Entry::SamplizerFilePair(_)
            | Entry::SamplizerValidation(_)
            | Entry::SamplizerStats(_)
            | Entry::FuncMap(_) => None,
        }
    }
    pub fn get_str_map(&self, name: &str, depth: u64) -> Option<HashMap<String, Entry>> {
        match self.nested(name, depth)? {
            Entry::StrMap(map) => Some(map),
            _ => None,
        }
    }

    pub fn get_str(&self, name: &str, depth: u64) -> Option<String> {
        match self.nested(name, depth)? {
            Entry::Str(s) => Some(s),
            _ => None,
        }
    }

    pub fn get_bool(&self, name: &str, depth: u64) -> Option<bool> {
        match self.nested(name, depth)? {
            Entry::Bool(b) => Some(b),
            _ => None,
        }
    }

    pub fn get_val(&self, name: &str, depth: u64) -> Option<u64> {
        match self.nested(name, depth)? {
            Entry::Val(v) => Some(v),
            _ => None,
        }
    }

    pub fn get_path(&self, name: &str, depth: u64) -> Option<PathBuf> {
        match self.nested(name, depth)? {
            Entry::Path(p) => Some(p),
            _ => None,
        }
    }

    pub fn get_list(&self, name: &str, depth: u64) -> Option<Vec<Entry>> {
        match self.nested(name, depth)? {
            Entry::List(list) => Some(list),
            _ => None,
        }
    }
    /// Display the entire registry in a database-like format using beautiful tables
    pub fn display_database(&self) {
        let entries = self.entries.clone();
        let entry_count = entries.len();
        println!("🗃️  REGISTRY DATABASE VIEW");
        println!(
            "═══════════════════════════════════════════════════════════════════════════════════════"
        );
        println!(
            "📊 Total Entries: {}  |  Entry Count: {}",
            entries.len(),
            entry_count
        );

        if entries.is_empty() {
            println!("(Registry is empty)");
            return;
        }

        // Group entries by type for better organization
        let mut handlers = Vec::new();
        let mut reports = Vec::new();
        let mut patterns = Vec::new();
        let mut redirects = Vec::new();
        let mut configs = Vec::new();
        let mut others = Vec::new();

        for (id, entry) in &entries {
            let entry_info = (id, entry);
            match entry {
                Entry::Handler(_) => handlers.push(entry_info),
                Entry::HandlerReport(_) => reports.push(entry_info),
                Entry::Patternizer(_) => patterns.push(entry_info),
                _ if id.name().starts_with("redirect_") => redirects.push(entry_info),
                _ if id.name().starts_with("config_") || id.name().starts_with("verbosity") => {
                    configs.push(entry_info)
                }
                _ => others.push(entry_info),
            }
        }

        // Display each category using our beautiful table system
        self.display_category("Handler", &handlers);
        self.display_category("HandlerReport", &reports);
        self.display_category("Pattern", &patterns);
        self.display_category("Redirect", &redirects);
        self.display_category("Config", &configs);
        self.display_category("Other", &others);

        println!(
            "═══════════════════════════════════════════════════════════════════════════════════════"
        );
    }

    /// Display a category of registry entries using our beautiful table system
    fn display_category(&self, entry_type: &str, entries: &[(&Id, &Entry)]) {
        if entries.is_empty() {
            return;
        }

        // Create a new table for this category
        let mut table = Table::new_registry_table(entry_type);

        // Add appropriate headers based on the first entry type
        if let Some((_, first_entry)) = entries.first() {
            match first_entry {
                Entry::Handler(_) => {
                    table.add_header(vec!["Name", "Role", "Priority", "Status"]);
                }
                Entry::HandlerReport(_) => {
                    table.add_header(vec!["Name", "Handler", "Level", "Message", "Success"]);
                }
                _ => {
                    table.add_header(vec!["Name", "Value", "Type"]);
                }
            }
        }

        // Add entries to the table based on their type
        for (id, entry) in entries {
            match entry {
                Entry::Handler(handler) => {
                    let row_idx = table.add_empty_row();

                    let mut name_cell = TableCell::new(row_idx, 0, &format!("name_{}", row_idx));
                    name_cell.add_entry(Entry::Str(id.name().clone()));
                    table.set_cell(row_idx, 0, name_cell);

                    let mut role_cell = TableCell::new(row_idx, 1, &format!("role_{}", row_idx));
                    role_cell.add_entry(Entry::Str(handler.role.clone()));
                    table.set_cell(row_idx, 1, role_cell);

                    let mut priority_cell =
                        TableCell::new(row_idx, 2, &format!("priority_{}", row_idx));
                    priority_cell.add_entry(Entry::Str(handler.priority.to_string()));
                    table.set_cell(row_idx, 2, priority_cell);

                    let mut status_cell =
                        TableCell::new(row_idx, 3, &format!("status_{}", row_idx));
                    status_cell.add_entry(Entry::Str("Active".to_string()));
                    table.set_cell(row_idx, 3, status_cell);
                }
                Entry::HandlerReport(report) => {
                    let row_idx = table.add_empty_row();

                    let mut name_cell = TableCell::new(row_idx, 0, &format!("name_{}", row_idx));
                    name_cell.add_entry(Entry::Str(id.name().clone()));
                    table.set_cell(row_idx, 0, name_cell);

                    let mut handler_cell =
                        TableCell::new(row_idx, 1, &format!("handler_{}", row_idx));
                    handler_cell.add_entry(Entry::Str(report.handler_id.name().to_string()));
                    table.set_cell(row_idx, 1, handler_cell);

                    let mut level_cell = TableCell::new(row_idx, 2, &format!("level_{}", row_idx));
                    level_cell.add_entry(Entry::Str(format!("{:?}", report.level)));
                    table.set_cell(row_idx, 2, level_cell);

                    let mut message_cell =
                        TableCell::new(row_idx, 3, &format!("message_{}", row_idx));
                    message_cell.add_entry(Entry::Str(report.message.clone()));
                    table.set_cell(row_idx, 3, message_cell);

                    let mut success_cell =
                        TableCell::new(row_idx, 4, &format!("success_{}", row_idx));
                    success_cell.add_entry(Entry::Str(report.success.to_string()));
                    table.set_cell(row_idx, 4, success_cell);
                }
                _ => {
                    // For other entry types, use a generic format
                    let row_idx = table.add_empty_row();

                    let mut name_cell = TableCell::new(row_idx, 0, &format!("name_{}", row_idx));
                    name_cell.add_entry(Entry::Str(id.name().clone()));
                    table.set_cell(row_idx, 0, name_cell);

                    let mut content_cell =
                        TableCell::new(row_idx, 1, &format!("content_{}", row_idx));
                    content_cell.add_entry(Entry::Str(format!("{}", entry)));
                    table.set_cell(row_idx, 1, content_cell);

                    let mut type_cell = TableCell::new(row_idx, 2, &format!("type_{}", row_idx));
                    type_cell.add_entry(Entry::Str(entry_type.to_string()));
                    table.set_cell(row_idx, 2, type_cell);
                }
            }
        }

        // Display the table with perfect formatting
        println!("\n📂 {} REGISTRY ENTRIES", entry_type.to_uppercase());
        println!("------------------------------------------------------------");
        print!("{}", table.display_formatted());
    }

    /// Display registry statistics
    pub fn display_stats(&self) {
        // Create a new table for registry statistics
        let mut stats_table = Table::new_registry_table("Registry Statistics");
        stats_table.add_header(vec!["Entry Type", "Count"]);

        // Count entry types
        let mut type_counts = std::collections::HashMap::new();
        for entry in self.entries.values() {
            let type_name = match entry {
                Entry::Id(_) => "Id",
                Entry::Val(_) => "Val",
                Entry::Str(_) => "Str",
                Entry::Bool(_) => "Bool",
                Entry::Func(_) => "Func",
                Entry::Path(_) => "Path",
                Entry::Handler(_) => "Handler",
                Entry::HandlerReport(_) => "HandlerReport",
                Entry::Table(_) => "Table",
                Entry::TableCell(_) => "TableCell",
                Entry::Pair(_) => "Pair",
                Entry::List(_) => "List",
                Entry::Any(_) => "Any",
                Entry::IdMap(_) => "IdMap",
                Entry::ValMap(_) => "ValMap",
                Entry::StrMap(_) => "StrMap",
                Entry::BoolMap(_) => "BoolMap",
                Entry::FuncMap(_) => "FuncMap",
                Entry::PathMap(_) => "PathMap",
                Entry::HandlerMap(_) => "HandlerMap",
                Entry::PairMap(_) => "PairMap",
                Entry::AnyMap(_) => "AnyMap",
                Entry::Patternizer(_) => "Patternizer",
                Entry::PatternRule(_) => "PatternRule",
                Entry::PatternMetrics(_) => "PatternMetrics",
                Entry::TokenPattern(_) => "TokenPattern",
                Entry::SamplizerPattern(_) => "SamplizerPattern",
                Entry::SamplizerFilePair(_) => "SamplizerFilePair",
                Entry::SamplizerValidation(_) => "SamplizerValidation",
                Entry::SamplizerStats(_) => "SamplizerStats",
            };
            *type_counts.entry(type_name).or_insert(0) += 1;
        }

        // Add type counts to table
        for (type_name, count) in type_counts.iter() {
            let row_idx = stats_table.add_empty_row();

            let mut type_cell = TableCell::new(row_idx, 0, &format!("type_{}", row_idx));
            type_cell.add_entry(Entry::Str(type_name.to_string()));
            stats_table.set_cell(row_idx, 0, type_cell);

            let mut count_cell = TableCell::new(row_idx, 1, &format!("count_{}", row_idx));
            count_cell.add_entry(Entry::Str(count.to_string()));
            stats_table.set_cell(row_idx, 1, count_cell);
        }

        // Add summary rows
        let total_row = stats_table.add_empty_row();
        let mut total_type_cell =
            TableCell::new(total_row, 0, &format!("total_type_{}", total_row));
        total_type_cell.add_entry(Entry::Str("Total Entries".to_string()));
        stats_table.set_cell(total_row, 0, total_type_cell);

        let mut total_count_cell =
            TableCell::new(total_row, 1, &format!("total_count_{}", total_row));
        total_count_cell.add_entry(Entry::Str(self.entries.len().to_string()));
        stats_table.set_cell(total_row, 1, total_count_cell);

        // Display the table
        println!("📊 REGISTRY STATISTICS");
        println!("═══════════════════════");
        print!("{}", stats_table.display_formatted());
    }
}
impl Default for Registry {
    fn default() -> Self {
        Registry::new("default")
    }
}
