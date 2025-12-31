#![allow(
    unsafe_code,
    static_mut_refs,
    unused_variables,
    unused_assignments,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals
)]
use std::collections::HashMap;
use std::fmt::{self, Write};
use std::path::PathBuf;
use std::result;

use crate::db::convert::{IdentifierConverter, VariableConverter};
use crate::db::filter::{Category, CategoryFilter, Kind, KindFilter};
use crate::db::format::{Formatter, OutputFormat};
use crate::db::keyword::Order;
use crate::db::{TypeConverter, is_c_keyword, is_rust_keyword, keyword};
use crate::handlers::arrays::ArrayHandler;
use crate::handlers::comments::CommentHandler;
use crate::handlers::conditionals::{IfElseHandler, SwitchCaseHandler};
use crate::handlers::enums::EnumHandler;
use crate::handlers::expressions::ExpressionHandler;
use crate::handlers::functions::{CallHandler, FunctionHandler};
use crate::handlers::globals::GlobalHandler;
use crate::handlers::loops::LoopHandler;
use crate::handlers::macros::{DefineHandler, IncludeHandler, PreprocessorHandler};
use crate::handlers::process::{ProcessorDecision, Processor};
use crate::handlers::structs::StructHandler;
use crate::handlers::typedefs::TypedefHandler;
use crate::handlers::unions::UnionHandler;
use crate::handlers::variables::VariableHandler;
use crate::handlers::process::ProcessorStats;
use crate::{
    Error,
    db::{Build, Config, Entry, Pattern, Report, Token, TokenSet, Tokenizer, Web, WebStats},
    lock::LazyRwLock,
    thread::shutdown_global_pool,
};

/// Processing mode for the system
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProcessMode {
    /// Process once and return
    OneShot,
    /// Keep processing in a loop until explicitly stopped
    Interactive,
}

/// Result of a single processing step
#[derive(Debug, Clone)]
pub struct ProcessResult {
    /// Number of tokens processed
    pub tokens_processed: usize,
    /// Number of patterns matched
    pub patterns_matched: usize,
    /// Number of handlers invoked
    pub handlers_invoked: usize,
    /// Number of reports generated
    pub reports_generated: usize,
    /// Whether processing should continue (for interactive mode)
    pub should_continue: bool,
    /// Any errors encountered
    pub errors: Vec<String>,
}

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

pub fn system() -> &'static mut System {
    SYSTEM.get_or_init_mut(System::new)
}
#[derive(Debug, Clone)]
pub struct System {
    db: Web,
    /// Index of the system config entry in the database
    config_idx: Option<usize>,
    /// Keyword ordering rules
    c_order: Order,
    rust_order: Order,
    type_converter: TypeConverter,
    identifier_converter: IdentifierConverter,
    variable_converter: VariableConverter,
    processor_stats: ProcessorStats,
    /// Whether Build objects should auto-register to database when completing
    register_builds: bool,
}

/// Name used for the system configuration entry
const SYSTEM_CONFIG_NAME: &str = "system";

impl System {
    #[allow(static_mut_refs, unsafe_code)]
    pub fn new() -> Self {
        let mut system = System {
            db: Web::new(),
            config_idx: None,
            c_order: Order::with_c_rules(),
            rust_order: Order::with_rust_rules(),
            type_converter: TypeConverter::new(),
            identifier_converter: IdentifierConverter::new(),
            variable_converter: VariableConverter::new(),
            processor_stats: ProcessorStats::new("system"),
            register_builds: false,
        };
        // Register all handlers and their patterns
        system.register_handlers();
        // Load standard types and identifiers from converters
        system.register_types();
        system.register_identifiers();
        system.register_order();
        system
    }
    pub fn register_type(&mut self, c_type: &str, rust_type: &str, category: &str) {
        // Validate type name - skip invalid registrations
        let trimmed = c_type.trim();
        if trimmed.is_empty() 
            || trimmed.starts_with('"')  // string literal
            || trimmed.starts_with('\'') // char literal
            || trimmed.chars().next().map(|c| c.is_ascii_digit()).unwrap_or(false) // starts with number
            || trimmed.len() == 1 && !trimmed.chars().next().unwrap().is_alphabetic() // single non-alpha char
        {
            return; // Skip invalid type names
        }
        
        let mut entry = Entry::node("Type", c_type);
        entry.set_attr("c_type", Entry::string(c_type));
        entry.set_attr("rust_type", Entry::string(rust_type));
        entry.set_attr("category", Entry::string(category));
        self.db.add_entry(entry);
    }
    /// Load type mappings from TypeConverter into the database
    fn register_types(&mut self) {
        let converter = self.type_converter.clone();
        for (c_type, rust_type, category) in converter.get_all_mappings() {
            self.register_type(c_type, rust_type, category);
        }
    }
    pub fn register_identifier(
        &mut self,
        c_identifier: &str,
        rust_identifier: &str,
        category: &str,
    ) {
        let mut entry = Entry::node("Identifier", c_identifier);
        entry.set_attr("c_identifier", Entry::string(c_identifier));
        entry.set_attr("rust_identifier", Entry::string(rust_identifier));
        entry.set_attr("category", Entry::string(category));
        self.db.add_entry(entry);
    }

    /// Load identifier mappings from IdentifierConverter into the database
    fn register_identifiers(&mut self) {
        let converter = self.identifier_converter.clone();
        for (c_identifier, rust_identifier, category) in converter.get_all_mappings() {
            self.register_identifier(c_identifier, rust_identifier, category.as_str());
        }
    }

    /// Register the keyword ordering rules to the database
    pub fn register_order(&mut self) {
        self.db.add(&self.c_order);
        self.db.add(&self.rust_order);
    }
    /// Lookup if 2 keywords can follow and precede each other from the database
    /// Returns (keyword1_can_follow_keyword2, keyword1_can_precede_keyword2)
    pub fn lookup_order_keyword(
        &self,
        keyword1: &str,
        keyword2: &str,
        lang: &str,
    ) -> Option<(bool, bool)> {
        let keyword1_can_precede_keyword2: bool;
        let keyword2_can_precede_keyword1: bool;
        match lang {
            "c" => {
                if !(keyword::is_c_keyword(keyword1) && keyword::is_c_keyword(keyword2)) {
                    return None;
                }
                // can_follow(a, b) means b can follow a, i.e., a can precede b
                keyword1_can_precede_keyword2 = self.c_order.can_follow(keyword1, keyword2);
                keyword2_can_precede_keyword1 = self.c_order.can_follow(keyword2, keyword1);
            },
            "rust" => {
                if !(keyword::is_rust_keyword(keyword1) && keyword::is_rust_keyword(keyword2)) {
                    return None;
                }
                // can_follow(a, b) means b can follow a, i.e., a can precede b
                keyword1_can_precede_keyword2 = self.rust_order.can_follow(keyword1, keyword2);
                keyword2_can_precede_keyword1 = self.rust_order.can_follow(keyword2, keyword1);
            },
            _ => return None,
        }
        // keyword1 can follow keyword2 if keyword2 can precede keyword1
        Some((keyword2_can_precede_keyword1, keyword1_can_precede_keyword2))
    }

    /// Lookup if a chain of keywords can all follow in order (left to right)
    /// and if they can all precede in order (right to left)
    /// Returns (all_can_follow_in_order, all_can_precede_in_order)
    pub fn lookup_order_chain(&self, keywords: Vec<&str>, lang: &str) -> Option<(bool, bool)> {
        if keywords.len() < 2 {
            return Some((true, true));
        }
        let mut all_follow = true;
        let mut all_precede = true;
        let mut can_follow_forward = false;
        let mut can_follow_reverse = false;
        for index in 0..(keywords.len() - 1) {
            let keyword1 = keywords[index];
            let keyword2 = keywords[index + 1];
            match lang {
                "c" => {
                    can_follow_forward = self.c_order.can_follow(keyword1, keyword2);
                    can_follow_reverse = self.c_order.can_follow(keyword2, keyword1);
                },
                "rust" => {
                    can_follow_forward = self.rust_order.can_follow(keyword1, keyword2);
                    can_follow_reverse = self.rust_order.can_follow(keyword2, keyword1);
                },
                _ => return None,
            }
            // Check if keyword1 can precede keyword2 (same as above, but we track separately)
            if !can_follow_forward {
                all_follow = false;
            }
            // For precede chain: check if keyword1 can follow keyword2 (reverse direction
            if !can_follow_reverse {
                all_precede = false;
            }
            // Early exit if both are already false
            if !all_follow && !all_precede {
                return Some((false, false));
            }
        }
        Some((all_follow, all_precede))
    }
    /// Check if a node transition is valid using the built-in Order
    pub fn is_valid_node_transition(&self, parent: &str, child: &str, lang: &str) -> bool {
        match lang {
            "c" => self.c_order.is_valid_transition(parent, child),
            "rust" => self.rust_order.is_valid_transition(parent, child),
            _ => false,
        }
    }

    /// Get keyword suggestions for what can follow a keyword
    pub fn suggest_keywords(&self, current: &str, lang: &str) -> Vec<String> {
        match lang {
            "c" => self.c_order.suggest(current),
            "rust" => self.rust_order.suggest(current),
            _ => Vec::<String>::new(),
        }
    }

    // ========================================================================
    // Conversion Helper Methods (for handlers)
    // ========================================================================

    /// Validate that a sequence of C tokens follows valid keyword ordering.
    /// Filters to only check keywords, skips identifiers/literals.
    /// Returns (is_valid, first_invalid_pair) where first_invalid_pair is (kw1, kw2) that failed.
    pub fn validate_c_token_order(&self, tokens: &[&str]) -> (bool, Option<(String, String)>) {
        let keywords: Vec<&str> = tokens
            .iter()
            .filter(|t| keyword::is_c_keyword(t))
            .copied()
            .collect();
        
        if keywords.len() < 2 {
            return (true, None);
        }

        for i in 0..keywords.len() - 1 {
            if !self.c_order.can_follow(keywords[i], keywords[i + 1]) {
                return (false, Some((keywords[i].to_string(), keywords[i + 1].to_string())));
            }
        }
        (true, None)
    }

    /// Validate that a sequence of Rust tokens follows valid keyword ordering.
    /// Returns (is_valid, first_invalid_pair).
    pub fn validate_rust_token_order(&self, tokens: &[&str]) -> (bool, Option<(String, String)>) {
        let keywords: Vec<&str> = tokens
            .iter()
            .filter(|t| keyword::is_rust_keyword(t))
            .copied()
            .collect();
        
        if keywords.len() < 2 {
            return (true, None);
        }

        for i in 0..keywords.len() - 1 {
            if !self.rust_order.can_follow(keywords[i], keywords[i + 1]) {
                return (false, Some((keywords[i].to_string(), keywords[i + 1].to_string())));
            }
        }
        (true, None)
    }

    /// Check if a C declaration prefix is valid (e.g., "static const int")
    /// Returns true if keywords can appear in this order.
    pub fn is_valid_c_declaration_prefix(&self, tokens: &[&str]) -> bool {
        self.validate_c_token_order(tokens).0
    }

    /// Check if a Rust declaration prefix is valid (e.g., "pub const")
    /// Returns true if keywords can appear in this order.
    pub fn is_valid_rust_declaration_prefix(&self, tokens: &[&str]) -> bool {
        self.validate_rust_token_order(tokens).0
    }

    /// Get valid Rust keywords that can follow after a given keyword.
    /// Useful for building valid Rust output sequences.
    pub fn valid_rust_followers(&self, keyword: &str) -> Vec<String> {
        self.rust_order.suggest(keyword)
    }

    /// Get valid C keywords that can follow after a given keyword.
    pub fn valid_c_followers(&self, keyword: &str) -> Vec<String> {
        self.c_order.suggest(keyword)
    }

    /// Validate and potentially fix a Rust keyword sequence.
    /// Returns the original if valid, or attempts to reorder if possible.
    pub fn validate_or_fix_rust_order(&self, tokens: &[&str]) -> Vec<String> {
        let (is_valid, _) = self.validate_rust_token_order(tokens);
        if is_valid {
            return tokens.iter().map(|s| s.to_string()).collect();
        }

        // Try common reorderings for Rust declarations
        // Priority order: pub > async > unsafe > const > fn/struct/enum/trait
        let mut visibility = Vec::new();
        let mut modifiers = Vec::new();
        let mut decl_type = Vec::new();
        let mut rest = Vec::new();

        for &token in tokens {
            match token {
                "pub" | "pub(crate)" | "pub(super)" | "pub(self)" => visibility.push(token),
                "async" | "unsafe" | "const" | "mut" | "static" => modifiers.push(token),
                "fn" | "struct" | "enum" | "trait" | "type" | "mod" | "use" | "impl" => decl_type.push(token),
                _ => rest.push(token),
            }
        }

        // Rebuild in correct order
        let mut result: Vec<String> = Vec::new();
        result.extend(visibility.iter().map(|s| s.to_string()));
        result.extend(modifiers.iter().map(|s| s.to_string()));
        result.extend(decl_type.iter().map(|s| s.to_string()));
        result.extend(rest.iter().map(|s| s.to_string()));
        result
    }

    /// Check if converting C keywords to Rust keywords maintains valid ordering.
    /// Takes C tokens and their Rust equivalents, validates both sides.
    pub fn validate_conversion_order(&self, c_tokens: &[&str], rust_tokens: &[&str]) -> bool {
        self.validate_c_token_order(c_tokens).0 && self.validate_rust_token_order(rust_tokens).0
    }

    /// Get the Rust keyword that should follow a C keyword in conversion.
    /// Uses type mapping and keyword ordering to suggest valid conversions.
    pub fn suggest_rust_for_c_context(&self, c_keyword: &str, context: &[&str]) -> Option<String> {
        // First, get the direct Rust equivalent
        let rust_equiv = keyword::c_to_rust_type(c_keyword)
            .or_else(|| keyword::c_to_rust_storage(c_keyword))
            .or_else(|| keyword::c_to_rust_qualifier(c_keyword));

        if let Some(equiv) = rust_equiv {
            // Validate it can follow the context
            if context.is_empty() {
                return Some(equiv.to_string());
            }
            let last = context.last()?;
            if self.rust_order.can_follow(last, equiv) {
                return Some(equiv.to_string());
            }
        }

        None
    }

    /// Register all handlers and add their patterns to the database
    fn register_handlers(&mut self) {
        // Register TypedefHandler patterns
        let typedef_handler = TypedefHandler::new();
        for pattern in typedef_handler.patterns() {
            self.db.add(&pattern.0);
            self.db.add(&pattern.1);
        }

        // Register CommentHandler patterns
        let comment_handler = CommentHandler::new();
        for pattern in comment_handler.patterns() {
            self.db.add(&pattern.0);
            self.db.add(&pattern.1);
        }

        // Register ArrayHandler patterns
        let array_handler = ArrayHandler::new();
        for pattern in array_handler.patterns() {
            self.db.add(&pattern.0);
            self.db.add(&pattern.1);
        }

        // Register FunctionHandler patterns
        let function_handler = FunctionHandler::new();
        for pattern in function_handler.patterns() {
            self.db.add(&pattern.0);
            self.db.add(&pattern.1);
        }

        // Register CallHandler patterns
        let call_handler = CallHandler::new();
        for pattern in call_handler.patterns() {
            self.db.add(&pattern.0);
            self.db.add(&pattern.1);
        }

        // Register VariableHandler patterns
        let variable_handler = VariableHandler::new();
        for pattern in variable_handler.patterns() {
            self.db.add(&pattern.0);
            self.db.add(&pattern.1);
        }

        // Register ExpressionHandler patterns
        let expression_handler = ExpressionHandler::new();
        for pattern in expression_handler.patterns() {
            self.db.add(&pattern.0);
            self.db.add(&pattern.1);
        }

        // Register LoopHandler patterns
        let loop_handler = LoopHandler::new();
        for pattern in loop_handler.patterns() {
            self.db.add(&pattern.0);
            self.db.add(&pattern.1);
        }

        // Register IfElseHandler patterns
        let if_else_handler = IfElseHandler::new();
        for pattern in if_else_handler.patterns() {
            self.db.add(&pattern.0);
            self.db.add(&pattern.1);
        }

        // Register SwitchCaseHandler patterns
        let switch_case_handler = SwitchCaseHandler::new();
        for pattern in switch_case_handler.patterns() {
            self.db.add(&pattern.0);
            self.db.add(&pattern.1);
        }

        // Register StructHandler patterns
        let struct_handler = StructHandler::new();
        for pattern in struct_handler.patterns() {
            self.db.add(&pattern.0);
            self.db.add(&pattern.1);
        }

        // Register EnumHandler patterns
        let enum_handler = EnumHandler::new();
        for pattern in enum_handler.patterns() {
            self.db.add(&pattern.0);
            self.db.add(&pattern.1);
        }

        // Register UnionHandler patterns
        let union_handler = UnionHandler::new();
        for pattern in union_handler.patterns() {
            self.db.add(&pattern.0);
            self.db.add(&pattern.1);
        }

        // Register PreprocessorHandler patterns
        let preprocessor_handler = PreprocessorHandler::new();
        for pattern in preprocessor_handler.patterns() {
            self.db.add(&pattern.0);
            self.db.add(&pattern.1);
        }

        // Register DefineHandler patterns
        let define_handler = DefineHandler::new();
        for pattern in define_handler.patterns() {
            self.db.add(&pattern.0);
            self.db.add(&pattern.1);
        }

        // Register IncludeHandler patterns
        let include_handler = IncludeHandler::new();
        for pattern in include_handler.patterns() {
            self.db.add(&pattern.0);
            self.db.add(&pattern.1);
        }

        // Register GlobalHandler patterns
        let global_handler = GlobalHandler::new();
        for pattern in global_handler.patterns() {
            self.db.add(&pattern.0);
            self.db.add(&pattern.1);
        }
    }

    /// Ensure the system config exists in the database, creating it if needed
    fn ensure_config(&mut self) {
        if self.config_idx.is_some() {
            return;
        }
        // Try to find existing config in database
        if let Some(idx) = self.find_config_index() {
            self.config_idx = Some(idx);
            return;
        }
        // Create new config and store in database
        let config = Config::new(SYSTEM_CONFIG_NAME, "runtime");
        let idx = self.db.add(&config);
        self.config_idx = Some(idx);
    }

    /// Find the config entry index by name
    fn find_config_index(&self) -> Option<usize> {
        // Search through entries to find config by name and kind
        for (idx, entry) in self.db.iter().enumerate() {
            if entry.kind() == Some("Config") && entry.name() == Some(SYSTEM_CONFIG_NAME) {
                return Some(idx);
            }
        }
        None
    }

    /// Get the config entry from the database
    fn get_config_entry(&self) -> Option<&Entry> {
        self.config_idx.and_then(|idx| self.db.get(idx))
    }

    /// Get mutable config entry from the database
    fn get_config_entry_mut(&mut self) -> Option<&mut Entry> {
        self.config_idx.and_then(|idx| self.db.get_mut(idx))
    }
    pub fn update(self) -> crate::Result<Self> {
        if self.db.is_empty() {
            return Err(Error::new(
                crate::Kind::Database,
                crate::Reason::Empty("system database"),
                Some(
                    "Cannot sync an empty database. Did you call the 'initialize' function yet?"
                        .to_string(),
                ),
            ));
        }
        *system() = self.clone();
        Ok(self)
    }
    /// Shutdown all background threads and resources
    ///
    /// # Arguments
    /// * `export` - Whether to export the database before shutdown
    /// * `path` - Optional path for export. If None, uses database's configured path or default
    pub fn shutdown(&mut self, export: bool, path: Option<PathBuf>) {
        // Shutdown global thread pool
        shutdown_global_pool();
        if export {
            let file_path = path.unwrap_or_else(|| self.get_db_path());
            self.db.save(file_path, "json");
        }
    }

    /// Startup the system
    ///
    /// # Arguments
    /// * `import` - Whether to import from a database file
    /// * `path` - Optional path for import. If None, uses database's configured path or default
    pub fn startup(&mut self, import: bool, path: Option<PathBuf>) {
        if import {
            let file_path = path.unwrap_or_else(|| self.get_db_path());
            self.db.load(file_path, "json");
        } else if self.db.is_empty() {
            // Only initialize if database is empty (preserves registered types)
            self.db.init();
        }
        // Re-register types and identifiers after any initialization
        self.register_types();
        self.register_identifiers();
    }

    /// Get the database file path from config or default
    fn get_db_path(&self) -> PathBuf {
        // First check for explicit db_path in config values
        if let Some(path) = self.get_config_string("db_path") {
            return PathBuf::from(path);
        }
        // Fall back to base_dir if set
        if let Some(base) = self.get_config_string("base_dir") {
            return PathBuf::from(base).join("c2r_database.json");
        }
        // Default
        PathBuf::from("c2r_database.json")
    }

    /// Set the database file path in config
    pub fn set_db_path(&mut self, path: impl Into<PathBuf>) {
        let path: PathBuf = path.into();
        self.set_config_value("db_path", Entry::string(path.to_string_lossy()));
    }

    /// Get the current database path (if explicitly set)
    pub fn db_path(&self) -> Option<PathBuf> {
        self.get_config_string("db_path").map(PathBuf::from)
    }

    // ========================================================================
    // Configuration Access (Database-backed)
    // ========================================================================

    /// Get a string value from the config entry in the database
    pub fn get_config_string(&self, key: &str) -> Option<String> {
        let entry = self.get_config_entry()?;
        // Check in the "values" attribute hashmap
        if let Some(Entry::HashMap(values, _)) = entry.attr("values") {
            if let Some(Entry::String(s, _)) = values.get(key) {
                return Some(s.clone());
            }
        }
        None
    }

    /// Set a value in the config entry in the database
    pub fn set_config_value(&mut self, key: &str, value: Entry) {
        self.ensure_config();
        if let Some(entry) = self.get_config_entry_mut() {
            // Get or create the values hashmap
            let values = if let Some(Entry::HashMap(existing, _)) = entry.attr("values") {
                let mut new_values = existing.clone();
                new_values.insert(key.to_string(), value);
                Entry::hashmap(new_values)
            } else {
                let mut new_values = std::collections::HashMap::new();
                new_values.insert(key.to_string(), value);
                Entry::hashmap(new_values)
            };
            entry.set_attr("values", values);
        }
    }

    /// Get a boolean value from config
    pub fn get_config_bool(&self, key: &str) -> Option<bool> {
        let entry = self.get_config_entry()?;
        if let Some(Entry::HashMap(values, _)) = entry.attr("values") {
            if let Some(Entry::Bool(b, _)) = values.get(key) {
                return Some(*b);
            }
        }
        None
    }

    /// Check if a feature is enabled in config
    pub fn is_feature_enabled(&self, feature: &str) -> bool {
        if let Some(entry) = self.get_config_entry() {
            if let Some(Entry::HashMap(features, _)) = entry.attr("features") {
                if let Some(Entry::Bool(b, _)) = features.get(feature) {
                    return *b;
                }
            }
        }
        false
    }

    /// Enable a feature in config
    pub fn enable_feature(&mut self, feature: &str) {
        self.ensure_config();
        if let Some(entry) = self.get_config_entry_mut() {
            let features = if let Some(Entry::HashMap(existing, _)) = entry.attr("features") {
                let mut new_features = existing.clone();
                new_features.insert(feature.to_string(), Entry::bool(true));
                Entry::hashmap(new_features)
            } else {
                let mut new_features = std::collections::HashMap::new();
                new_features.insert(feature.to_string(), Entry::bool(true));
                Entry::hashmap(new_features)
            };
            entry.set_attr("features", features);
        }
    }

    /// Disable a feature in config
    pub fn disable_feature(&mut self, feature: &str) {
        self.ensure_config();
        if let Some(entry) = self.get_config_entry_mut() {
            let features = if let Some(Entry::HashMap(existing, _)) = entry.attr("features") {
                let mut new_features = existing.clone();
                new_features.insert(feature.to_string(), Entry::bool(false));
                Entry::hashmap(new_features)
            } else {
                let mut new_features = std::collections::HashMap::new();
                new_features.insert(feature.to_string(), Entry::bool(false));
                Entry::hashmap(new_features)
            };
            entry.set_attr("features", features);
        }
    }

    /// Get the config as a Config struct (reconstructed from database)
    pub fn config(&self) -> Option<Config> {
        self.get_config_entry().and_then(Config::from_entry)
    }

    /// Replace the entire config in the database
    pub fn set_config(&mut self, config: Config) {
        if let Some(idx) = self.config_idx {
            // Replace existing entry
            if let Some(entry) = self.db.get_mut(idx) {
                *entry = config.to_entry();
            }
        } else {
            // Add new config
            let idx = self.db.add(&config);
            self.config_idx = Some(idx);
        }
    }

    /// Load configuration from the database by name (for loading non-system configs)
    pub fn load_config_by_name(&self, name: &str) -> Option<Config> {
        self.db
            .by_name(name)
            .into_iter()
            .find(|e| e.kind() == Some("Config"))
            .and_then(Config::from_entry)
    }

    /// Clear the system database completely
    pub fn clear(&mut self) {
        if !self.db.is_empty() {
            self.db.clear();
        }
    }

    /// Clear entries of a specific kind
    pub fn clear_kind(&mut self, _kind: &str) {
        // TODO: Implement remove_by_kind on Web
        // self.db.remove_by_kind(kind);
    }

    /// Clear entries of a specific category
    pub fn clear_category(&mut self, _category: &str) {
        // TODO: Implement remove_by_category on Web
        // self.db.remove_by_category(category);
    }

    /// Reset the system to initial state (clears db and reinitializes)
    pub fn reset(&mut self) {
        self.db.clear();
        self.db.init();
    }

    /// Display the entire database in the default format
    pub fn display(&self) {
        println!("{}", self.format(&OutputFormat::Text));
    }

    /// Display the entire database in tree format (more visual)
    pub fn display_tree(&self) {
        println!("{}", self.format(&OutputFormat::Tree));
    }

    /// Display the entire database in table format
    pub fn display_table(&self) {
        println!("{}", self.format(&OutputFormat::Table));
    }

    /// Display the system database's statistics
    pub fn display_stats(&self) {
        let stats = self.db.stats();
        println!("╔══════════════════════════════════════╗");
        println!("║         SYSTEM DATABASE STATS        ║");
        println!("╠══════════════════════════════════════╣");
        println!("║  Total Entries:    {:>16}  ║", stats.total_entries);
        println!("║  Total Kinds:      {:>16}  ║", stats.total_kinds);
        println!("║  Total Categories: {:>16}  ║", stats.total_categories);
        println!("║  Build Count:      {:>16}  ║", stats.build_count);
        println!("╚══════════════════════════════════════╝");
    }

    /// Display a summary of entry counts by kind
    pub fn display_summary(&self) {
        println!("╔══════════════════════════════════════╗");
        println!("║           ENTRIES BY KIND            ║");
        println!("╠══════════════════════════════════════╣");
        for (kind, count) in self.kind_counts() {
            if count > 0 {
                println!("║  {:24} {:>8}    ║", kind, count);
            }
        }
        println!("╚══════════════════════════════════════╝");
    }

    // ========================================================================
    // Built-in Database Queries
    // ========================================================================

    /// Get all reports from the database
    pub fn reports(&self) -> Vec<Report> {
        self.db
            .by_kind("Report")
            .iter()
            .filter_map(|entry| Report::from_entry(entry))
            .collect()
    }

    /// Get reports filtered by a predicate
    pub fn reports_filtered<F>(&self, predicate: F) -> Vec<Report>
    where
        F: Fn(&Report) -> bool,
    {
        self.reports().into_iter().filter(predicate).collect()
    }

    /// Get all patterns from the database
    pub fn patterns(&self) -> Vec<Pattern> {
        self.db
            .by_kind("Pattern")
            .iter()
            .filter_map(|entry| Pattern::from_entry(entry))
            .collect()
    }

    /// Get a pattern by name
    pub fn pattern_by_name(&self, name: &str) -> Option<Pattern> {
        self.db
            .by_name(name)
            .iter()
            .filter(|e| e.kind() == Some("Pattern"))
            .find_map(|entry| Pattern::from_entry(entry))
    }

    /// Get all samples from the database
    pub fn samples(&self) -> Vec<&Entry> {
        // TODO: Add Sample::from_entry when Sample supports it
        self.db.by_kind("Sample")
    }

    /// Get a sample by name
    pub fn sample_by_name(&self, name: &str) -> Option<&Entry> {
        // TODO: Return Sample when Sample::from_entry is implemented
        self.db
            .by_name(name)
            .into_iter()
            .find(|e| e.kind() == Some("Sample"))
    }

    /// Get all tokens from the database (as Token structs)
    pub fn tokens(&self) -> Vec<Token> {
        self.db
            .by_kind("Token")
            .iter()
            .filter_map(|entry| Token::from_entry(entry))
            .collect()
    }

    /// Get all token sets from the database
    pub fn token_sets(&self) -> Vec<TokenSet> {
        self.db
            .by_kind("TokenSet")
            .iter()
            .filter_map(|entry| TokenSet::from_entry(entry))
            .collect()
    }

    // ========================================================================
    // Database Modification
    // ========================================================================

    /// Add a report to the database
    pub fn add_report(&mut self, report: &Report) {
        self.db.add(report);
    }

    /// Add a pattern to the database
    pub fn add_pattern(&mut self, pattern: &Pattern) {
        self.db.add(pattern);
    }

    /// Add a sample to the database (via Entry)
    pub fn add_sample_entry(&mut self, sample: &Entry) {
        // TODO: Change to Sample when Sample implements Build
        self.db.add_entry(sample.clone());
    }

    /// Add a token set to the database
    pub fn add_token_set(&mut self, token_set: &TokenSet) {
        self.db.add(token_set);
    }

    /// Add any Build-able item to the database
    pub fn add<T: Build>(&mut self, item: &T) {
        self.db.add(item);
    }

    /// Add an entry directly to the database
    pub fn add_entry(&mut self, entry: Entry) {
        self.db.add_entry(entry);
    }

    // ========================================================================
    // Indentation Level (for code generation)
    // ========================================================================

    /// Get current indentation level
    pub fn indent_level(&self) -> usize {
        self.db
            .metadata("indent_level")
            .and_then(|e| e.get_usize_attr("value"))
            .unwrap_or(0)
    }

    /// Set indentation level
    pub fn set_indent_level(&mut self, level: usize) {
        let mut entry = Entry::node("metadata", "indent_level");
        entry.set_attr("value", Entry::usize(level));
        self.db.set_metadata("indent_level", entry);
    }

    /// Increment indentation level
    pub fn indent_push(&mut self) {
        let current = self.indent_level();
        self.set_indent_level(current + 1);
    }

    /// Decrement indentation level
    pub fn indent_pop(&mut self) {
        let current = self.indent_level();
        if current > 0 {
            self.set_indent_level(current - 1);
        }
    }

    /// Get indentation string for current level
    pub fn indent_str(&self) -> String {
        "    ".repeat(self.indent_level())
    }

    // ========================================================================
    // Tokenization
    // ========================================================================

    /// Tokenize input source into a vector of tokens
    ///
    /// # Arguments
    /// * `source` - The source code to tokenize (as string)
    ///
    /// # Returns
    /// Vector of tokens, or error message
    pub fn tokenize(&self, source: &str) -> Result<Vec<Token>, String> {
        if source.is_empty() {
            return Ok(Vec::new());
        }

        let mut tokenizer = Tokenizer::new();
        tokenizer.set_content(source.as_bytes().to_vec());
        let slot_id = tokenizer.tokenize();

        tokenizer
            .get_slot(slot_id)
            .cloned()
            .ok_or_else(|| "Failed to retrieve tokenized slot".to_string())
    }

    /// Tokenize input from a file path
    pub fn tokenize_file(&self, path: &str) -> Result<Vec<Token>, String> {
        let content = std::fs::read_to_string(path)
            .map_err(|e| format!("Failed to read file '{}': {}", path, e))?;
        self.tokenize(&content)
    }

    // ========================================================================
    // Processing Functions
    // ========================================================================

    /// Main processing loop - the core function that handlers are called from
    ///
    /// # Arguments
    /// * `mode` - Processing mode (OneShot or Interactive)
    /// * `input` - Optional input source (file path, stdin, etc.)
    ///
    /// # Returns
    /// Final ProcessResult summarizing all processing
    pub fn process(&mut self, mode: ProcessMode, input: Option<&str>) -> ProcessResult {
        match mode {
            ProcessMode::OneShot => self.process_once(input),
            ProcessMode::Interactive => self.process_interactive(input),
        }
    }
    pub fn process_stats(&mut self, stats: &ProcessorStats) {
        self.processor_stats.merge(stats);
        if self.is_build_registration_enabled() {
            self.db.add_entry(self.processor_stats.to_entry());
        }
    }
    /// Process input once and return
    fn process_once(&mut self, input: Option<&str>) -> ProcessResult {
        let mut result = ProcessResult {
            tokens_processed: 0,
            patterns_matched: 0,
            handlers_invoked: 0,
            reports_generated: 0,
            should_continue: false,
            errors: Vec::new(),
        };

        // Step 1: Tokenize input
        let tokens = match input {
            Some(source) => {
                // Check if it's a file path or raw source
                let tokens_result = if std::path::Path::new(source).exists() {
                    self.tokenize_file(source)
                } else {
                    self.tokenize(source)
                };

                match tokens_result {
                    Ok(t) => t,
                    Err(e) => {
                        result.errors.push(e);
                        return result;
                    }
                }
            }
            None => {
                result.errors.push("No input provided".to_string());
                return result;
            }
        };

        result.tokens_processed = tokens.len();

        if tokens.is_empty() {
            return result;
        }

        // Register tokens to database if build registration is enabled
        if self.register_builds {
            let len = tokens.len();
            let token_set = TokenSet::new(0..len, 0..len, tokens.clone());
            self.db.add(&token_set);
        }

        // Step 2: Process tokens with handlers
        let process_result = self.process_tokens(&tokens);
        result.patterns_matched = process_result.patterns_matched;
        result.handlers_invoked = process_result.handlers_invoked;
        result.reports_generated = process_result.reports_generated;
        result.errors.extend(process_result.errors);

        result
    }

    /// Interactive processing loop - keeps running until stopped
    fn process_interactive(&mut self, input: Option<&str>) -> ProcessResult {
        let mut cumulative = ProcessResult {
            tokens_processed: 0,
            patterns_matched: 0,
            handlers_invoked: 0,
            reports_generated: 0,
            should_continue: true,
            errors: Vec::new(),
        };

        // First process initial input if provided
        if let Some(source) = input {
            let initial_result = self.process_once(Some(source));
            cumulative.tokens_processed += initial_result.tokens_processed;
            cumulative.patterns_matched += initial_result.patterns_matched;
            cumulative.handlers_invoked += initial_result.handlers_invoked;
            cumulative.reports_generated += initial_result.reports_generated;
            cumulative.errors.extend(initial_result.errors);
        }

        // Interactive loop - continues until should_continue is false
        while cumulative.should_continue {
            // Process one step (reads from pending buffer)
            let step_result = self.process_step();

            // Accumulate results
            cumulative.tokens_processed += step_result.tokens_processed;
            cumulative.patterns_matched += step_result.patterns_matched;
            cumulative.handlers_invoked += step_result.handlers_invoked;
            cumulative.reports_generated += step_result.reports_generated;
            cumulative.errors.extend(step_result.errors);
            cumulative.should_continue = step_result.should_continue;
        }

        cumulative
    }

    /// Process a single step (used by interactive mode)
    fn process_step(&mut self) -> ProcessResult {
        ProcessResult {
            tokens_processed: 0,
            patterns_matched: 0,
            handlers_invoked: 0,
            reports_generated: 0,
            should_continue: false, // No more pending input
            errors: Vec::new(),
        }
    }

    /// Process tokens through the handler pipeline
    ///
    /// This is the core function that matches patterns and invokes handlers.
    /// It segments tokens by statement boundaries and processes each segment.
    fn process_tokens(&mut self, tokens: &[Token]) -> ProcessResult {
        let mut result = ProcessResult {
            tokens_processed: tokens.len(),
            patterns_matched: 0,
            handlers_invoked: 0,
            reports_generated: 0,
            should_continue: false,
            errors: Vec::new(),
        };

        // Segment tokens into processable chunks (statements, blocks, etc.)
        let segments = self.segment_tokens(tokens);

        for segment in segments {
            if segment.is_empty() {
                continue;
            }

            // Find all matching handlers ordered by priority
            let matching_handlers = self.match_all_handler_patterns(&segment);

            if !matching_handlers.is_empty() {
                result.patterns_matched += 1;
                let mut handler_succeeded = false;

                // Try handlers in order until one succeeds
                for (handler_name, _confidence) in &matching_handlers {
                    match self.run_handler(handler_name, &segment) {
                        Ok(handler_result) => {
                            result.handlers_invoked += 1;
                            handler_succeeded = true;

                            // Store result in database if successful
                            if let ProcessorDecision::Complete {
                                rust_code,
                                confidence: _,
                            } = handler_result
                            {
                                // Create a report entry for the successful conversion
                                let report = Report::new(
                                    format!("{}_conversion", handler_name),
                                    "conversion",
                                    format!("Converted to: {}", rust_code),
                                );

                                self.db.add(&report);
                                result.reports_generated += 1;
                            }
                            break; // Handler succeeded, stop trying others
                        }
                        Err(_e) => {
                            // Handler failed, try next one
                            continue;
                        }
                    }
                }

                // If all handlers failed, record error with the first handler's name
                if !handler_succeeded && !matching_handlers.is_empty() {
                    let first_handler = &matching_handlers[0].0;
                    result.errors.push(format!(
                        "All handlers failed for segment starting with '{}'",
                        segment.first().map(|t| t.to_string()).unwrap_or_default()
                    ));
                }
            }
        }

        result
    }
    /// Segment tokens into processable chunks based on statement boundaries
    ///
    /// This finds statement-ending tokens (;, }, etc.) and splits accordingly.
    /// It also handles block structures like functions, structs, etc.
    fn segment_tokens(&self, tokens: &[Token]) -> Vec<Vec<Token>> {
        let mut segments: Vec<Vec<Token>> = Vec::new();
        let mut current: Vec<Token> = Vec::new();
        let mut brace_depth: usize = 0;
        let mut paren_depth: usize = 0;

        for token in tokens {
            let token_str = token.to_string();

            // Handle comments as their own segment (only at top level)
            if token_str.starts_with("//") || token_str.starts_with("/*") {
                if brace_depth == 0 {
                    // Top-level comment - save any current segment first
                    if !current.is_empty() {
                        segments.push(std::mem::take(&mut current));
                    }
                    // Comment is its own segment
                    segments.push(vec![token.clone()]);
                    continue;
                } else {
                    // Comment inside a block - keep it as part of current segment
                    current.push(token.clone());
                    continue;
                }
            }

            // Handle preprocessor directives as their own segment
            // Preprocessor lines don't end with ; so we need special handling
            if token_str == "#" {
                // Save any current segment first
                if !current.is_empty() {
                    segments.push(std::mem::take(&mut current));
                }
                // Start collecting preprocessor directive
                current.push(token.clone());
                continue;
            }

            // Check if we're in a preprocessor directive (current starts with #)
            if !current.is_empty() && current[0].to_string() == "#" {
                // Check if this is a #define directive
                let is_define = current.len() >= 2 && current[1].to_string() == "define";

                // Track braces within preprocessor for #define with struct/enum bodies
                if token_str == "{" {
                    brace_depth += 1;
                } else if token_str == "}" {
                    brace_depth = brace_depth.saturating_sub(1);
                }

                // #define macros can contain struct/enum/etc. - only end on new # or
                // balanced braces followed by new top-level construct
                let starts_new_construct = if is_define {
                    // Only split on new preprocessor directive when braces balanced
                    token_str == "#" && brace_depth == 0 && current.len() > 3
                } else {
                    // Other directives (#include, #ifdef) end on new constructs
                    matches!(
                        token_str.as_str(),
                        "int"
                            | "char"
                            | "void"
                            | "float"
                            | "double"
                            | "long"
                            | "short"
                            | "unsigned"
                            | "signed"
                            | "struct"
                            | "enum"
                            | "union"
                            | "typedef"
                            | "static"
                            | "extern"
                            | "inline"
                            | "const"
                            | "volatile"
                            | "if"
                            | "for"
                            | "while"
                            | "do"
                            | "switch"
                            | "return"
                            | "break"
                            | "continue"
                            | "#"
                    ) && current.len() > 2
                };

                if starts_new_construct {
                    // Save preprocessor directive and start new segment
                    brace_depth = 0;
                    segments.push(std::mem::take(&mut current));
                    current.push(token.clone());
                } else {
                    // Continue collecting preprocessor tokens
                    current.push(token.clone());
                }
                continue;
            }

            match token_str.as_str() {
                "{" => {
                    current.push(token.clone());
                    brace_depth += 1;
                }
                "}" => {
                    current.push(token.clone());
                    brace_depth = brace_depth.saturating_sub(1);
                    // End of block at top level - BUT check for constructs that need ; to end
                    if brace_depth == 0 && !current.is_empty() {
                        let current_strs: Vec<String> =
                            current.iter().map(|t| t.to_string()).collect();
                        let first_token = current_strs.first().cloned().unwrap_or_default();

                        // Type definitions need ; after }
                        // BUT: "struct Node* funcName(...) {" is a FUNCTION, not a type def
                        // Check: if there's a ( before the first {, it's likely a function
                        let paren_pos = current_strs.iter().position(|t| t == "(");
                        let brace_pos = current_strs.iter().position(|t| t == "{");
                        let is_function = match (paren_pos, brace_pos) {
                            (Some(p), Some(b)) => p < b,  // ( before { means function
                            _ => false,
                        };
                        
                        let is_type_def = !is_function && matches!(
                            first_token.as_str(),
                            "typedef" | "enum" | "struct" | "union"
                        );

                        // Array initializers (type name[] = {...};) also need ; after }
                        let has_bracket = current_strs.iter().any(|t| t == "[");
                        let has_equals = current_strs.iter().any(|t| t == "=");
                        let is_array_init = has_bracket && has_equals;

                        if !is_type_def && !is_array_init {
                            segments.push(std::mem::take(&mut current));
                        }
                        // For type definitions and array initializers, continue until ;
                    }
                }
                "(" => {
                    current.push(token.clone());
                    paren_depth += 1;
                }
                ")" => {
                    current.push(token.clone());
                    paren_depth = paren_depth.saturating_sub(1);
                }
                ";" => {
                    current.push(token.clone());
                    // End of statement at top level (not inside braces)
                    if brace_depth == 0 && !current.is_empty() {
                        segments.push(std::mem::take(&mut current));
                    }
                }
                _ => {
                    current.push(token.clone());
                }
            }
        }

        // Don't forget any remaining tokens
        if !current.is_empty() {
            segments.push(current);
        }

        segments
    }

    /// Match all handler patterns and return all matches ordered by priority
    ///
    /// This allows trying multiple handlers when the first one fails
    fn match_all_handler_patterns(&self, tokens: &[Token]) -> Vec<(String, f64)> {
        use crate::db::pattern::Pattern;

        if tokens.is_empty() {
            return Vec::new();
        }

        // Convert tokens to strings for pattern matching
        let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        // Collect all matching patterns with their priorities
        let mut matches: Vec<(String, f64, usize)> = Vec::new(); // (category, confidence, priority)

        // Special case: #include directives (single-token format from tokenizer)
        if !token_strings.is_empty() && token_strings[0].starts_with("#include ") {
            return vec![("include".to_string(), 0.95)];
        }

        // Special case: preprocessor directives starting with #
        if !token_strings.is_empty() && token_strings[0] == "#" && token_strings.len() > 1 {
            match token_strings[1].as_str() {
                "define" => return vec![("define".to_string(), 0.95)],
                "ifdef" | "ifndef" | "endif" | "else" | "elif" | "if" => {
                    return vec![("preprocessor".to_string(), 0.95)];
                }
                _ => return vec![("macro".to_string(), 0.9)],
            }
        }

        // Query patterns from database by kind
        for entry in self.db.by_kind("Pattern") {
            if let Some(pattern) = Pattern::<String>::from_entry(entry) {
                if let Some(confidence) = pattern.matches_tokens(&token_strings) {
                    let priority = pattern.priority() as usize;
                    let category = pattern.category().to_string();

                    // Avoid duplicates - only add if this category isn't already in matches
                    if !matches.iter().any(|(cat, _, _)| cat == &category) {
                        matches.push((category, confidence, priority));
                    }
                }
            }
        }

        // Special case: struct/enum/union DEFINITIONS should be handled by their specific handlers
        // But struct/enum/union TYPE declarations (like `struct Point p;` or `struct Point arr[3]`)
        // should NOT be routed to struct handler - they're variable/array declarations.
        // Detect definitions: "struct Name {" or "typedef struct ..."
        let first = &token_strings[0];
        let is_type_definition = if first == "typedef" && token_strings.len() >= 2 {
            // typedef struct/enum/union - always a definition
            matches!(token_strings[1].as_str(), "struct" | "enum" | "union")
        } else if matches!(first.as_str(), "struct" | "enum" | "union") {
            // Check if this is a definition (has { after the name) vs a declaration
            // struct Name { ... } - definition
            // struct Name varname; - declaration (variable)
            // struct Name arr[3]; - declaration (array)
            // Look for { appearing before any [ or ;
            let brace_pos = token_strings.iter().position(|t| t == "{");
            let bracket_pos = token_strings.iter().position(|t| t == "[");
            let semi_pos = token_strings.iter().position(|t| t == ";");

            match (brace_pos, bracket_pos, semi_pos) {
                (Some(b), Some(br), _) => b < br, // { before [ means definition
                (Some(_), None, _) => true,       // Has { but no [ means definition
                (None, _, _) => false,            // No { means declaration
            }
        } else {
            false
        };

        if is_type_definition {
            let category = if first == "typedef" {
                token_strings[1].as_str()
            } else {
                first.as_str()
            };
            // Remove any function matches - structs with function pointers are NOT functions
            matches.retain(|(cat, _, _)| cat != "function" && cat != "function_definition");
            matches.insert(0, (category.to_string(), 0.95, 200));
        }

        // Special case: function definitions (with body) should use FunctionHandler
        // Check if this looks like a function definition: has ( and { but is NOT a struct/enum/union
        let has_paren = token_strings.iter().any(|t| t == "(");
        let has_brace = token_strings.iter().any(|t| t == "{");

        // Detect struct-returning functions: struct Name funcName(...) {
        // The ( comes AFTER two identifiers (struct name, function name)
        let is_struct_returning_function = if first == "struct" && has_paren && has_brace {
            // Check pattern: struct <name> <name> (
            if let Some(paren_pos) = token_strings.iter().position(|t| t == "(") {
                // Need at least: struct Name funcName (
                paren_pos >= 3
                    && token_strings
                        .get(1)
                        .map(|s| !s.starts_with("{") && s != "=")
                        .unwrap_or(false)
                    && token_strings
                        .get(2)
                        .map(|s| !s.starts_with("{") && s != "(")
                        .unwrap_or(false)
            } else {
                false
            }
        } else {
            false
        };

        if is_struct_returning_function {
            // This is a struct-returning function, route to FunctionHandler
            matches.retain(|(cat, _, _)| cat != "struct" && cat != "function");
            matches.insert(0, ("function_definition".to_string(), 0.98, 950));
        } else if has_paren && has_brace && !is_type_definition {
            // This is a function definition, not a declaration or struct
            // Remove any "function" match and ensure "function_definition" is first
            matches.retain(|(cat, _, _)| cat != "function");
            if !matches
                .iter()
                .any(|(cat, _, _)| cat == "function_definition")
            {
                matches.insert(0, ("function_definition".to_string(), 0.95, 200));
            }
        }

        // Special case: arrays with [ should be routed to ArrayHandler with high priority
        let has_bracket = token_strings.iter().any(|t| t == "[");
        let has_semicolon = token_strings.iter().any(|t| t == ";");
        if has_bracket && has_semicolon {
            // This is an array declaration - ensure it goes to ArrayHandler
            // Remove any conflicting "variable" or "global" matches
            matches.retain(|(cat, _, _)| cat != "variable" && cat != "global");
            matches.insert(0, ("array".to_string(), 0.9, 150));
        }

        // If no patterns matched, try keyword-based fallback
        if matches.is_empty() {
            if let Some(fallback) = self.match_handler_by_keyword(&token_strings) {
                matches.push(fallback);
            }
        }

        // Sort by priority (descending) then confidence (descending)
        matches.sort_by(|a, b| {
            b.2.cmp(&a.2)
                .then_with(|| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal))
        });

        // Return just category and confidence
        matches
            .into_iter()
            .map(|(cat, conf, _)| (cat, conf))
            .collect()
    }

    /// Fallback keyword-based matching when no pattern matches
    fn match_handler_by_keyword(&self, tokens: &[String]) -> Option<(String, f64, usize)> {
        if tokens.is_empty() {
            return None;
        }

        let first_token = &tokens[0];

        // Use keyword module for classification
        if let Some(kw) = self.c_order.keyword(first_token) {
            let category = match kw.category {
                crate::db::keyword::KeywordCategory::TypeDeclaration => {
                    match first_token.as_str() {
                        "typedef" => "typedef",
                        "struct" => "struct",
                        "enum" => "enum",
                        "union" => "union",
                        _ => "type",
                    }
                }
                crate::db::keyword::KeywordCategory::Preprocessor => {
                    // First token is "#", check second token for directive type
                    if first_token == "#" && tokens.len() > 1 {
                        match tokens[1].as_str() {
                            "ifdef" | "ifndef" | "endif" | "else" | "elif" | "if" => "preprocessor",
                            "define" => "define",
                            "include" => "include",
                            _ => "macro",
                        }
                    } else {
                        // Tokenizer may combine them - check for prefixes
                        if first_token.starts_with("#include ") || first_token == "#include" {
                            "include"
                        } else if first_token.starts_with("#define ") || first_token == "#define" {
                            "define"
                        } else {
                            match first_token.as_str() {
                                "#ifdef" | "#ifndef" | "#endif" | "#else" | "#elif" | "#if" => {
                                    "preprocessor"
                                }
                                _ => "macro",
                            }
                        }
                    }
                }
                crate::db::keyword::KeywordCategory::ControlFlow => match first_token.as_str() {
                    "for" | "while" | "do" => "loop",
                    _ => "control",
                },
                crate::db::keyword::KeywordCategory::StorageClass => {
                    // static, extern, inline at file scope
                    let has_paren = tokens.iter().any(|t| t == "(");
                    let has_brace = tokens.iter().any(|t| t == "{");
                    let has_semicolon = tokens.iter().any(|t| t == ";");

                    if has_paren && has_brace {
                        // Function definition (static/inline/extern function with body)
                        "function_definition"
                    } else if has_paren && has_semicolon {
                        // Function declaration
                        "function"
                    } else if has_semicolon {
                        // Global variable with storage class
                        "global"
                    } else {
                        return None;
                    }
                }
                crate::db::keyword::KeywordCategory::BasicType => {
                    // Need more context for basic types
                    if tokens.len() > 2 {
                        let has_paren = tokens.iter().any(|t| t == "(");
                        let has_brace = tokens.iter().any(|t| t == "{");
                        let has_bracket = tokens.iter().any(|t| t == "[");
                        let has_semicolon = tokens.iter().any(|t| t == ";");

                        if has_paren && has_brace {
                            // Function definition with body
                            "function_definition"
                        } else if has_paren && has_semicolon {
                            // Function declaration
                            "function"
                        } else if has_bracket && has_semicolon {
                            // Array declaration (int arr[10]; or int arr[] = {...};)
                            "array"
                        } else if has_brace && has_semicolon {
                            // Struct initialization
                            "global"
                        } else if has_semicolon {
                            // At file scope, this is a global variable
                            "global"
                        } else {
                            return None;
                        }
                    } else {
                        return None;
                    }
                }
                _ => return None,
            };

            Some((category.to_string(), 0.7, kw.priority as usize))
        } else {
            // Check for comment tokens (may include full comment text)
            if first_token.starts_with("//") || first_token.starts_with("/*") {
                Some(("comment".to_string(), 0.95, 50))
            } else {
                None
            }
        }
    }

    /// Run a specific handler on the given tokens
    ///
    /// Supports nested routing: when a handler encounters a nested construct,
    /// it returns RouteNested, we process that with the target handler,
    /// then return the result to continue the parent handler.
    fn run_handler(
        &mut self,
        handler_name: &str,
        tokens: &[Token],
    ) -> Result<ProcessorDecision, String> {
        // Collected results from nested handlers
        let mut nested_results: Vec<(std::ops::Range<usize>, String)> = Vec::new();

        let decision = match handler_name {
            "typedef" | "TypedefHandler" => {
                use crate::handlers::typedefs::TypedefHandler;
                self.run_handler_loop(TypedefHandler::new(), tokens, &mut nested_results)
            }
            "comment" | "CommentHandler" => {
                use crate::handlers::comments::CommentHandler;
                self.run_handler_loop(CommentHandler::new(), tokens, &mut nested_results)
            }
            "array" | "ArrayHandler" => {
                use crate::handlers::arrays::ArrayHandler;
                self.run_handler_loop(
                    ArrayHandler::new().with_global(true),
                    tokens,
                    &mut nested_results,
                )
            }
            "function"
            | "function_definition"
            | "static_function_definition"
            | "inline_function_definition"
            | "static_inline_function_definition"
            | "FunctionHandler" => {
                self.run_handler_loop(FunctionHandler::new(), tokens, &mut nested_results)
            }
            "call" | "CallHandler" => {
                use crate::handlers::functions::CallHandler;
                self.run_handler_loop(CallHandler::new(), tokens, &mut nested_results)
            }
            "variable" | "VariableHandler" => {
                use crate::handlers::variables::VariableHandler;
                self.run_handler_loop(VariableHandler::new(), tokens, &mut nested_results)
            }
            "expression" | "ExpressionHandler" => {
                use crate::handlers::expressions::ExpressionHandler;
                self.run_handler_loop(ExpressionHandler::new(), tokens, &mut nested_results)
            }
            "loop" | "for_loop" | "while_loop" | "do_while_loop" | "LoopHandler" => {
                use crate::handlers::loops::LoopHandler;
                self.run_handler_loop(LoopHandler::new(), tokens, &mut nested_results)
            }
            "conditional" | "if" | "if_statement" | "if_else_statement" | "IfElseHandler" => {
                use crate::handlers::conditionals::IfElseHandler;
                self.run_handler_loop(IfElseHandler::new(), tokens, &mut nested_results)
            }
            "switch" | "switch_statement" | "SwitchCaseHandler" => {
                use crate::handlers::conditionals::SwitchCaseHandler;
                self.run_handler_loop(SwitchCaseHandler::new(), tokens, &mut nested_results)
            }
            "string_format" | "printf" | "sprintf" | "StringFormatHandler" => {
                use crate::handlers::expressions::StringFormatHandler;
                self.run_handler_loop(StringFormatHandler::new(), tokens, &mut nested_results)
            }
            "struct" | "struct_definition" | "typedef_struct" | "StructHandler" => {
                use crate::handlers::structs::StructHandler;
                self.run_handler_loop(StructHandler::new(), tokens, &mut nested_results)
            }
            "enum" | "enum_definition" | "typedef_enum" | "EnumHandler" => {
                use crate::handlers::enums::EnumHandler;
                self.run_handler_loop(EnumHandler::new(), tokens, &mut nested_results)
            }
            "union" | "union_definition" | "typedef_union" | "UnionHandler" => {
                use crate::handlers::unions::UnionHandler;
                self.run_handler_loop(UnionHandler::new(), tokens, &mut nested_results)
            }
            "preprocessor"
            | "ifdef"
            | "ifndef"
            | "endif"
            | "else_directive"
            | "elif"
            | "if_defined"
            | "PreprocessorHandler" => {
                use crate::handlers::macros::PreprocessorHandler;
                self.run_handler_loop(PreprocessorHandler::new(), tokens, &mut nested_results)
            }
            "define" | "define_const" | "define_macro" | "macro" | "DefineHandler" => {
                use crate::handlers::macros::DefineHandler;
                self.run_handler_loop(DefineHandler::new(), tokens, &mut nested_results)
            }
            "include" | "include_system" | "include_local" | "IncludeHandler" => {
                use crate::handlers::macros::IncludeHandler;
                self.run_handler_loop(IncludeHandler::new(), tokens, &mut nested_results)
            }
            "global" | "global_const" | "global_static" | "global_extern" | "global_init"
            | "global_decl" | "GlobalHandler" => {
                use crate::handlers::globals::GlobalHandler;
                self.run_handler_loop(GlobalHandler::new(), tokens, &mut nested_results)
            }
            // Handlers not yet implemented - return placeholder
            "control" => Ok(ProcessorDecision::Complete {
                rust_code: format!("// TODO: {} handler not yet implemented", handler_name),
                confidence: 0.1,
            }),
            _ => Err(format!("Unknown handler: {}", handler_name)),
        }?;

        Ok(decision)
    }

    /// Run a handler through its processing loop, handling nested routing
    fn run_handler_loop<H: Processor>(
        &mut self,
        mut handler: H,
        tokens: &[Token],
        nested_results: &mut Vec<(std::ops::Range<usize>, String)>,
    ) -> Result<ProcessorDecision, String> {
        loop {
            let decision = handler.process(tokens, None);
            match &decision {
                ProcessorDecision::Continue { .. } => continue,
                ProcessorDecision::Complete { .. } => return Ok(decision),
                ProcessorDecision::Route { target_handler, .. } => {
                    // Recursively call the target handler (replaces current)
                    return self.run_handler(target_handler, tokens);
                }
                ProcessorDecision::RouteNested {
                    target_handler,
                    token_range,
                    reason,
                } => {
                    // Process nested construct with target handler
                    let nested_tokens = &tokens[token_range.clone()];
                    match self.run_handler(target_handler, nested_tokens) {
                        Ok(ProcessorDecision::Complete {
                            rust_code,
                            confidence,
                        }) => {
                            // Store nested result for parent handler
                            nested_results.push((token_range.clone(), rust_code.clone()));
                            // Continue parent handler processing
                            // TODO: Need to notify parent handler of nested result
                            continue;
                        }
                        Ok(other) => {
                            // Nested handler didn't complete normally
                            return Ok(other);
                        }
                        Err(e) => {
                            return Err(format!(
                                "Nested handler '{}' failed: {}",
                                target_handler, e
                            ));
                        }
                    }
                }
                ProcessorDecision::NestedComplete { .. } => {
                    // This shouldn't happen at top level - return as-is
                    return Ok(decision);
                }
                ProcessorDecision::Pause { reason, .. } => {
                    return Err(format!("Processing paused: {}", reason));
                }
                ProcessorDecision::Fail { reason, .. } => {
                    return Err(reason.clone());
                }
            }
        }
    }

    /// Check if there's more input to process
    pub fn has_pending_input(&self) -> bool {
        // TODO: Check tokenizer buffer, input queue, etc.
        false
    }

    /// Stop interactive processing
    pub fn stop_processing(&mut self) {
        // TODO: Set flag to stop interactive loop
        // This would be called from signal handler or user input
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

    /// Enable auto-registration of Build objects to system database
    pub fn enable_build_registration(&mut self) {
        self.register_builds = true;
    }

    /// Disable auto-registration of Build objects to system database  
    pub fn disable_build_registration(&mut self) {
        self.register_builds = false;
    }

    /// Check if auto-registration is enabled
    pub fn is_build_registration_enabled(&self) -> bool {
        self.register_builds
    }

    /// Look up struct field names from registered struct constructor
    pub fn lookup_struct_fields(&self, struct_name: &str) -> Option<Vec<String>> {
        // Look for StructConstructor entry with this name
        for entry in self.db.iter() {
            // Use the kind() method to check the Node's kind field
            if let Some(kind) = entry.kind() {
                if kind == "StructConstructor" {
                    // Use the name() method to check the Node's name field
                    if let Some(name) = entry.name() {
                        if name == struct_name {
                            if let Some(fields) = entry.get_attr("fields") {
                                if let Entry::Vec(vec, _) = fields {
                                    return Some(
                                        vec.iter()
                                            .filter_map(|e| {
                                                if let Entry::String(s, _) = e {
                                                    Some(s.clone())
                                                } else {
                                                    None
                                                }
                                            })
                                            .collect(),
                                    );
                                }
                            }
                        }
                    }
                }
            }
        }
        None
    }

    /// Look up struct field types from registered struct constructor
    pub fn lookup_struct_field_types(&self, struct_name: &str) -> Option<Vec<String>> {
        for entry in self.db.iter() {
            // Use the kind() method to check the Node's kind field
            if let Some(kind) = entry.kind() {
                if kind == "StructConstructor" {
                    // Use the name() method to check the Node's name field
                    if let Some(name) = entry.name() {
                        if name == struct_name {
                            if let Some(types) = entry.get_attr("field_types") {
                                if let Entry::Vec(vec, _) = types {
                                    return Some(
                                        vec.iter()
                                            .filter_map(|e| {
                                                if let Entry::String(s, _) = e {
                                                    Some(s.clone())
                                                } else {
                                                    None
                                                }
                                            })
                                            .collect(),
                                    );
                                }
                            }
                        }
                    }
                }
            }
        }
        None
    }

    // ========================================================================
    // Type Registration
    // ========================================================================

    /// Register a struct type in the database
    /// Also registers type mappings for both "StructName" and "struct StructName"
    pub fn register_struct(&mut self, c_name: &str, rust_name: &str) {
        use crate::db::node::{NodeExt, struct_node};
        let entry = struct_node(c_name)
            .with_attr("c_type", Entry::string(c_name))
            .with_attr("rust_type", Entry::string(rust_name));
        self.db.add_entry(entry);

        // Register type mappings for lookup_type()
        self.register_type(c_name, rust_name, "struct");
        self.register_type(&format!("struct {}", c_name), rust_name, "struct");
        // Register pointer variants
        self.register_type(
            &format!("{} *", c_name),
            &format!("*mut {}", rust_name),
            "pointer",
        );
        self.register_type(
            &format!("{}*", c_name),
            &format!("*mut {}", rust_name),
            "pointer",
        );
        self.register_type(
            &format!("struct {} *", c_name),
            &format!("*mut {}", rust_name),
            "pointer",
        );
        self.register_type(
            &format!("struct {}*", c_name),
            &format!("*mut {}", rust_name),
            "pointer",
        );
        self.register_type(
            &format!("const {} *", c_name),
            &format!("*const {}", rust_name),
            "pointer",
        );
        self.register_type(
            &format!("const {}*", c_name),
            &format!("*const {}", rust_name),
            "pointer",
        );
        self.register_type(
            &format!("const struct {} *", c_name),
            &format!("*const {}", rust_name),
            "pointer",
        );
        self.register_type(
            &format!("const struct {}*", c_name),
            &format!("*const {}", rust_name),
            "pointer",
        );
    }

    /// Register an enum type in the database
    /// Also registers type mappings for both "EnumName" and "enum EnumName"
    pub fn register_enum(&mut self, c_name: &str, rust_name: &str) {
        self.register_enum_with_variants(c_name, rust_name, &[]);
    }

    /// Register an enum type with its variants in the database
    /// Variants will be registered for lookup to add enum prefix
    pub fn register_enum_with_variants(
        &mut self,
        c_name: &str,
        rust_name: &str,
        variants: &[&str],
    ) {
        use crate::db::node::{NodeExt, enum_node};

        // Convert variants to Entry::Vec
        let variant_entries: Vec<Entry> = variants.iter().map(|v| Entry::string(*v)).collect();

        let entry = enum_node(c_name)
            .with_attr("c_type", Entry::string(c_name))
            .with_attr("rust_type", Entry::string(rust_name))
            .with_attr("variants", Entry::vec(variant_entries));
        self.db.add_entry(entry);

        // Register type mappings for lookup_type()
        self.register_type(c_name, rust_name, "enum");
        self.register_type(&format!("enum {}", c_name), rust_name, "enum");
        // Register pointer variants
        self.register_type(
            &format!("{} *", c_name),
            &format!("*mut {}", rust_name),
            "pointer",
        );
        self.register_type(
            &format!("{}*", c_name),
            &format!("*mut {}", rust_name),
            "pointer",
        );
        self.register_type(
            &format!("enum {} *", c_name),
            &format!("*mut {}", rust_name),
            "pointer",
        );
        self.register_type(
            &format!("enum {}*", c_name),
            &format!("*mut {}", rust_name),
            "pointer",
        );

        // Register each variant for lookup (variant_name -> EnumName::variant_name)
        for variant in variants {
            self.register_enum_variant(c_name, rust_name, variant);
        }
    }

    /// Register a single enum variant for lookup
    fn register_enum_variant(&mut self, _enum_c_name: &str, enum_rust_name: &str, variant: &str) {
        use crate::db::node::{NodeExt, node};
        let entry = node("EnumVariant", variant)
            .with_attr("enum_name", Entry::string(enum_rust_name))
            .with_attr(
                "rust_value",
                Entry::string(&format!("{}::{}", enum_rust_name, variant)),
            );
        self.db.add_entry(entry);
    }

    /// Look up an enum variant and return the qualified name (EnumName::Variant)
    pub fn lookup_enum_variant(&self, variant: &str) -> Option<String> {
        for entry in self.db.by_kind("EnumVariant") {
            if let Some(name) = entry.name() {
                if name == variant {
                    if let Some(rust_value) = entry.get_attr("rust_value") {
                        if let Entry::String(s, _) = rust_value {
                            return Some(s.clone());
                        }
                    }
                }
            }
        }
        None
    }

    /// Register a union type in the database
    /// Also registers type mappings for both "UnionName" and "union UnionName"
    pub fn register_union(&mut self, c_name: &str, rust_name: &str) {
        use crate::db::node::{NodeExt, node};
        let entry = node("Union", c_name)
            .with_attr("c_type", Entry::string(c_name))
            .with_attr("rust_type", Entry::string(rust_name));
        self.db.add_entry(entry);

        // Register type mappings for lookup_type()
        self.register_type(c_name, rust_name, "union");
        self.register_type(&format!("union {}", c_name), rust_name, "union");
        // Register pointer variants
        self.register_type(
            &format!("{} *", c_name),
            &format!("*mut {}", rust_name),
            "pointer",
        );
        self.register_type(
            &format!("{}*", c_name),
            &format!("*mut {}", rust_name),
            "pointer",
        );
        self.register_type(
            &format!("union {} *", c_name),
            &format!("*mut {}", rust_name),
            "pointer",
        );
        self.register_type(
            &format!("union {}*", c_name),
            &format!("*mut {}", rust_name),
            "pointer",
        );
    }

    /// Register a typedef in the database
    /// Also registers type mapping for the alias
    pub fn register_typedef(&mut self, alias: &str, rust_type: &str, underlying: &str) {
        use crate::db::node::{NodeExt, typedef};
        let entry = typedef(alias, underlying)
            .with_attr("c_type", Entry::string(alias))
            .with_attr("rust_type", Entry::string(rust_type));
        self.db.add_entry(entry);

        // Register type mapping for lookup_type()
        self.register_type(alias, rust_type, "typedef");
        // Register pointer variants
        self.register_type(
            &format!("{} *", alias),
            &format!("*mut {}", rust_type),
            "pointer",
        );
        self.register_type(
            &format!("{}*", alias),
            &format!("*mut {}", rust_type),
            "pointer",
        );
        self.register_type(
            &format!("const {} *", alias),
            &format!("*const {}", rust_type),
            "pointer",
        );
        self.register_type(
            &format!("const {}*", alias),
            &format!("*const {}", rust_type),
            "pointer",
        );
    }
    /// Look up a registered identifier by C name
    pub fn lookup_identifier(&self, c_identifier: &str) -> Option<String> {
        // Try direct lookup
        let entries = self.db.by_name(c_identifier);
        for entry in &entries {
            // Check for rust_name attribute on functions, variables, constants
            if let Some(kind) = entry.kind() {
                if matches!(
                    kind,
                    "Function" | "Variable" | "Constant" | "Global" | "Parameter"
                ) {
                    if let Some(rust_identifier) = entry.get_string_attr("rust_identifier") {
                        return Some(rust_identifier.to_string());
                    }
                }
            }
        }

        // Try looking up as a function
        let entries = self.db.by_kind("Function");
        for entry in entries {
            if entry.name() == Some(c_identifier) {
                if let Some(rust_identifier) = entry.get_string_attr("rust_identifier") {
                    return Some(rust_identifier.to_string());
                }
                // If no rust_name, return the C name as-is
                return Some(c_identifier.to_string());
            }
        }

        // Try looking up as a variable
        let entries = self.db.by_kind("Variable");
        for entry in entries {
            if entry.name() == Some(c_identifier) {
                if let Some(rust_identifier) = entry.get_string_attr("rust_identifier") {
                    return Some(rust_identifier.to_string());
                }
                return Some(c_identifier.to_string());
            }
        }

        // Try looking up as a constant/macro
        let entries = self.db.by_kind("Constant");
        for entry in entries {
            if entry.name() == Some(c_identifier) {
                if let Some(rust_identifier) = entry.get_string_attr("rust_identifier") {
                    return Some(rust_identifier.to_string());
                }
                return Some(c_identifier.to_string());
            }
        }

        None
    }
    /// Look up a registered type by C name
    pub fn lookup_type(&self, c_type: &str) -> Option<String> {
        // Try direct lookup - only match actual type entries, not variables
        let type_kinds = ["Type", "Struct", "Enum", "Union", "Typedef"];
        let entries = self.db.by_name(c_type);
        for entry in &entries {
            if let Some(kind) = entry.kind() {
                if type_kinds.contains(&kind) {
                    if let Some(rust_type) = entry.get_string_attr("rust_type") {
                        return Some(rust_type.to_string());
                    }
                }
            }
        }

        // Try with "struct " prefix removed
        if c_type.starts_with("struct ") {
            let name = c_type.strip_prefix("struct ").unwrap();
            let entries = self.db.by_name(name);
            for entry in &entries {
                if entry.kind() == Some("Struct") {
                    if let Some(rust_type) = entry.get_string_attr("rust_type") {
                        return Some(rust_type.to_string());
                    }
                }
            }
        }

        // Try with "enum " prefix removed
        if c_type.starts_with("enum ") {
            let name = c_type.strip_prefix("enum ").unwrap();
            let entries = self.db.by_name(name);
            for entry in &entries {
                if entry.kind() == Some("Enum") {
                    if let Some(rust_type) = entry.get_string_attr("rust_type") {
                        return Some(rust_type.to_string());
                    }
                }
            }
        }

        // Try with "union " prefix removed
        if c_type.starts_with("union ") {
            let name = c_type.strip_prefix("union ").unwrap();
            let entries = self.db.by_name(name);
            for entry in &entries {
                if entry.kind() == Some("Union") {
                    if let Some(rust_type) = entry.get_string_attr("rust_type") {
                        return Some(rust_type.to_string());
                    }
                }
            }
        }

        None
    }

    /// Check if a type is registered
    pub fn is_type_registered(&self, c_type: &str) -> bool {
        self.lookup_type(c_type).is_some()
    }

    /// Get all registered structs
    pub fn registered_structs(&self) -> Vec<&Entry> {
        self.db.by_kind("Struct")
    }

    /// Get all registered enums
    pub fn registered_enums(&self) -> Vec<&Entry> {
        self.db.by_kind("Enum")
    }

    /// Get all registered typedefs
    pub fn registered_typedefs(&self) -> Vec<&Entry> {
        self.db.by_kind("Typedef")
    }

    /// Check if a name is a registered typedef
    pub fn is_typedef(&self, name: &str) -> bool {
        let entries = self.db.by_name(name);
        for entry in &entries {
            if entry.kind() == Some("Typedef") {
                return true;
            }
        }
        false
    }

    /// Check if a name is a registered custom type (struct, enum, union, or typedef)
    pub fn is_custom_type(&self, name: &str) -> bool {
        let entries = self.db.by_name(name);
        for entry in &entries {
            if let Some(kind) = entry.kind() {
                if matches!(kind, "Typedef" | "Struct" | "Enum" | "Union") {
                    return true;
                }
            }
        }
        false
    }

    // ========================================================================
    // Function Registration
    // ========================================================================

    /// Register a function definition in the database
    ///
    /// # Arguments
    /// * `c_name` - The C function name
    /// * `rust_name` - The Rust function name (converted to snake_case)
    /// * `return_type` - The Rust return type
    /// * `params` - Vector of (param_name, param_type) pairs
    pub fn register_function(
        &mut self,
        c_identifier: &str,
        rust_identifier: &str,
        return_type: &str,
        params: Vec<(String, String)>,
    ) {
        use crate::db::node::{NodeExt, node};
        let mut entry = node("Function", c_identifier)
            .with_attr("c_identifier", Entry::string(c_identifier))
            .with_attr("rust_identifier", Entry::string(rust_identifier))
            .with_attr("return_type", Entry::string(return_type))
            .with_attr("param_count", Entry::usize(params.len()));

        // Store parameters as a list of entries
        let param_entries: Vec<Entry> = params
            .iter()
            .enumerate()
            .map(|(i, (param_name, param_type))| {
                let mut pe = Entry::node("Param", param_name);
                pe.set_attr("index", Entry::usize(i));
                pe.set_attr("name", Entry::string(param_name));
                pe.set_attr("type", Entry::string(param_type));
                pe
            })
            .collect();
        entry.set_attr("params", Entry::vec(param_entries));

        self.db.add_entry(entry);
    }

    /// Register a simple function (no params/return type tracked)
    pub fn register_function_simple(&mut self, c_identifier: &str, rust_identifier: &str) {
        use crate::db::node::{NodeExt, node};
        let entry = node("Function", c_identifier)
            .with_attr("c_identifier", Entry::string(c_identifier))
            .with_attr("rust_identifier", Entry::string(rust_identifier));
        self.db.add_entry(entry);
    }

    /// Look up a registered function by C name
    pub fn lookup_function(&self, c_identifier: &str) -> Option<String> {
        let entries = self.db.by_name(c_identifier);
        for entry in &entries {
            if entry.kind() == Some("Function") {
                if let Some(rust_identifier) = entry.get_string_attr("rust_identifier") {
                    return Some(rust_identifier.to_string());
                }
            }
        }
        None
    }

    /// Check if a function is registered
    pub fn is_function_registered(&self, c_identifier: &str) -> bool {
        self.lookup_function(c_identifier).is_some()
    }

    /// Get all registered functions
    pub fn registered_functions(&self) -> Vec<&Entry> {
        self.db.by_kind("Function")
    }

    /// Get function metadata by name
    pub fn get_function_metadata(&self, c_identifier: &str) -> Option<&Entry> {
        self.db
            .by_name(c_identifier)
            .into_iter()
            .find(|e| e.kind() == Some("Function"))
    }

    // ========================================================================
    // Variable Registration and Lookup
    // ========================================================================

    /// Register a variable with its type information
    /// Used for sizeof() and type inference
    pub fn register_variable(&mut self, name: &str, c_type: &str, rust_type: &str) {
        use crate::db::node::{NodeExt, node};
        // Register in converter for quick lookup
        self.variable_converter.register(name, c_type, rust_type);
        
        // Also add to database for persistence
        let entry = node("Variable", name)
            .with_attr("c_type", Entry::string(c_type))
            .with_attr("rust_type", Entry::string(rust_type));
        self.db.add_entry(entry);
    }

    /// Register a pointer variable
    pub fn register_pointer_variable(&mut self, name: &str, base_c_type: &str, base_rust_type: &str, level: usize) {
        self.variable_converter.register_pointer(name, base_c_type, base_rust_type, level);
        
        // Compute the full pointer type
        let rust_type = if level == 1 {
            format!("*mut {}", base_rust_type)
        } else {
            let mut result = base_rust_type.to_string();
            for _ in 0..level {
                result = format!("*mut {}", result);
            }
            result
        };
        
        use crate::db::node::{NodeExt, node};
        let entry = node("Variable", name)
            .with_attr("c_type", Entry::string(&format!("{}*", base_c_type)))
            .with_attr("rust_type", Entry::string(&rust_type))
            .with_attr("is_pointer", Entry::bool(true))
            .with_attr("pointer_level", Entry::usize(level));
        self.db.add_entry(entry);
    }

    /// Register an array variable
    pub fn register_array_variable(&mut self, name: &str, element_c_type: &str, element_rust_type: &str, dims: Vec<usize>) {
        self.variable_converter.register_array(name, element_c_type, element_rust_type, dims.clone());
        
        let rust_type = if dims.len() == 1 {
            format!("[{}; {}]", element_rust_type, dims[0])
        } else {
            let mut result = element_rust_type.to_string();
            for &dim in dims.iter().rev() {
                result = format!("[{}; {}]", result, dim);
            }
            result
        };
        
        use crate::db::node::{NodeExt, node};
        let dim_entries: Vec<Entry> = dims.iter().map(|&d| Entry::usize(d)).collect();
        let entry = node("Variable", name)
            .with_attr("c_type", Entry::string(&format!("{}[]", element_c_type)))
            .with_attr("rust_type", Entry::string(&rust_type))
            .with_attr("is_array", Entry::bool(true))
            .with_attr("array_dims", Entry::vec(dim_entries));
        self.db.add_entry(entry);
    }

    /// Look up a registered variable's Rust type by name
    pub fn lookup_variable(&self, name: &str) -> Option<String> {
        // Try converter first (fast path)
        if let Some(rust_type) = self.variable_converter.lookup(name) {
            return Some(rust_type);
        }
        
        // Fall back to database lookup
        let entries = self.db.by_name(name);
        for entry in &entries {
            if entry.kind() == Some("Variable") {
                return entry.get_string_attr("rust_type").map(|s| s.to_string());
            }
        }
        None
    }

    /// Look up a registered variable's C type by name
    pub fn lookup_variable_c_type(&self, name: &str) -> Option<String> {
        // Try converter first (fast path)
        if let Some(c_type) = self.variable_converter.lookup_c_type(name) {
            return Some(c_type);
        }
        
        // Fall back to database lookup
        let entries = self.db.by_name(name);
        for entry in &entries {
            if entry.kind() == Some("Variable") {
                return entry.get_string_attr("c_type").map(|s| s.to_string());
            }
        }
        None
    }

    /// Check if a variable is registered
    pub fn is_variable_registered(&self, name: &str) -> bool {
        self.lookup_variable(name).is_some()
    }

    /// Enter a new scope (for function bodies, blocks)
    pub fn push_variable_scope(&mut self) {
        self.variable_converter.push_scope();
    }

    /// Exit current scope
    pub fn pop_variable_scope(&mut self) {
        self.variable_converter.pop_scope();
    }

    /// Get all registered variables
    pub fn registered_variables(&self) -> Vec<&Entry> {
        self.db.by_kind("Variable")
    }

    // ========================================================================
    // Routing Management
    // ========================================================================

    /// Get next route ID from metadata
    fn next_route_id(&self) -> u64 {
        self.db
            .metadata("route_next_id")
            .and_then(|e| e.get_usize_attr("value"))
            .map(|v| v as u64)
            .unwrap_or(1)
    }

    /// Set next route ID in metadata
    fn set_next_route_id(&mut self, id: u64) {
        let mut entry = Entry::node("metadata", "route_next_id");
        entry.set_attr("value", Entry::usize(id as usize));
        self.db.set_metadata("route_next_id", entry);
    }

    /// Register a new route and return its assigned ID
    pub fn register_route(&mut self, mut route: crate::db::routing::Route) -> u64 {
        let id = self.next_route_id();
        self.set_next_route_id(id + 1);
        route.id = id;
        self.db.add(&route);
        id
    }

    /// Lookup routes matching the given query
    pub fn lookup_route(
        &self,
        query: crate::db::routing::RouteQuery,
    ) -> Vec<crate::db::routing::Route> {
        self.db
            .by_kind("Route")
            .into_iter()
            .filter(|e| query.matches(e))
            .filter_map(|e| crate::db::routing::Route::from_entry(e))
            .collect()
    }

    /// Lookup a single route by ID
    pub fn lookup_route_by_id(&self, id: u64) -> Option<crate::db::routing::Route> {
        self.lookup_route(crate::db::routing::RouteQuery::by_id(id))
            .into_iter()
            .next()
    }

    /// Update an existing route (adds new version with updated data)
    pub fn update_route(&mut self, route: &crate::db::routing::Route) {
        self.db.add(route);
    }

    /// Get route statistics
    pub fn route_stats(&self) -> crate::db::routing::RouteStats {
        let entries: Vec<&Entry> = self.db.by_kind("Route");
        crate::db::routing::RouteStats::from_entries(&entries)
    }

    // ========================================================================

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
        kinds
            .iter()
            .flat_map(|k| self.db.by_kind(k.as_str()))
            .collect()
    }

    /// Filter entries by multiple kind strings
    pub fn by_kinds(&self, kinds: &[&str]) -> Vec<&Entry> {
        kinds.iter().flat_map(|k| self.db.by_kind(k)).collect()
    }

    /// Filter entries by multiple categories
    /// Accepts slice of Category enums
    pub fn by_categories_enum(&self, categories: &[Category]) -> Vec<&Entry> {
        categories
            .iter()
            .flat_map(|c| self.db.by_category(c.as_str()))
            .collect()
    }

    /// Filter entries by multiple category strings
    pub fn by_categories(&self, categories: &[&str]) -> Vec<&Entry> {
        categories
            .iter()
            .flat_map(|c| self.db.by_category(c))
            .collect()
    }

    /// Filter entries by kind and category (intersection)
    /// Accepts Kind/Category enums, &str, or String
    pub fn by_kind_and_category(
        &self,
        kind: impl Into<KindFilter>,
        category: impl Into<CategoryFilter>,
    ) -> Vec<&Entry> {
        let kind_filter: KindFilter = kind.into();
        let cat_filter: CategoryFilter = category.into();
        self.db
            .by_kind(kind_filter.as_str())
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
        self.kinds()
            .into_iter()
            .map(|k| (k, self.count_by_kind(k)))
            .collect()
    }

    /// Get summary counts for all categories
    pub fn category_counts(&self) -> Vec<(&str, usize)> {
        self.categories()
            .into_iter()
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
            OutputFormat::Json | OutputFormat::JsonCompact => {
                format_system_json(self, matches!(fmt, OutputFormat::Json))
            }
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

    // Kinds section
    writeln!(out, "| {:^width$} |", "ENTRIES BY KIND", width = width - 2).unwrap();
    writeln!(out, "+{}+", "-".repeat(width)).unwrap();
    for (kind, count) in system.kind_counts() {
        writeln!(
            out,
            "| {:30} | {:width$} |",
            kind,
            count,
            width = width - 35
        )
        .unwrap();
    }
    writeln!(out, "+{}+", "-".repeat(width)).unwrap();

    // Categories section
    if !system.categories().is_empty() {
        writeln!(
            out,
            "| {:^width$} |",
            "ENTRIES BY CATEGORY",
            width = width - 2
        )
        .unwrap();
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
    stats_map.insert(
        "total_entries".to_string(),
        Entry::usize(stats.total_entries),
    );
    stats_map.insert("total_kinds".to_string(), Entry::usize(stats.total_kinds));
    stats_map.insert(
        "total_categories".to_string(),
        Entry::usize(stats.total_categories),
    );
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
        let entries: Vec<Entry> = system
            .db
            .by_kind(kind)
            .iter()
            .map(|e| (*e).clone())
            .collect();
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
                let entry_branch = if j == entries.len().min(3) - 1 {
                    "└─"
                } else {
                    "├─"
                };
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
    format!(
        "System({} entries, {} kinds, {} categories)",
        stats.total_entries, stats.total_kinds, stats.total_categories
    )
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
