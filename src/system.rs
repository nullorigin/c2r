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

use crate::{Error, db::{Entry, Report, Web, WebStats, Pattern, Token, TokenSet, Tokenizer, Build, Config}, lock::LazyRwLock, thread::shutdown_global_pool};
use crate::db::keyword::Order;
use crate::handlers::process::{ProcessDecision, Processor};
use crate::db::format::{Formatter, OutputFormat};
use crate::db::filter::{Kind, Category, KindFilter, CategoryFilter};

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

pub fn system() -> &'static System {
    SYSTEM.get_or_init(System::new)
}

pub fn system_mut() -> &'static mut System {
    SYSTEM.get_or_init_mut(System::new)
}
#[derive(Debug, Clone)]
pub struct System {
    db: Web,
    /// Index of the system config entry in the database
    config_idx: Option<usize>,
    /// Keyword ordering rules
    keyword_order: Order,
    /// Next routing decision ID (stored here for fast allocation)
    routing_next_id: u64,
}

/// Name used for the system configuration entry
const SYSTEM_CONFIG_NAME: &str = "system";

impl System {
    #[allow(static_mut_refs, unsafe_code)]
    pub fn new() -> Self {
        let mut system = System { 
            db: Web::new(),
            config_idx: None,
            keyword_order: Order::with_c_rules(),
            routing_next_id: 1,
        };
        // Register all handlers and their patterns
        system.register_handlers();
        system
    }
    
    /// Register all handlers and add their patterns to the database
    fn register_handlers(&mut self) {
        use crate::handlers::typedefs::TypedefHandler;
        use crate::handlers::comments::CommentHandler;
        use crate::handlers::arrays::ArrayHandler;
        use crate::handlers::functions::FunctionHandler;
        use crate::handlers::variables::VariableHandler;
        use crate::handlers::expressions::ExpressionHandler;
        
        // Register TypedefHandler patterns
        let typedef_handler = TypedefHandler::new();
        for pattern in typedef_handler.patterns() {
            self.db.add(&pattern);
        }
        
        // Register CommentHandler patterns
        let comment_handler = CommentHandler::new();
        for pattern in comment_handler.patterns() {
            self.db.add(&pattern);
        }
        
        // Register ArrayHandler patterns
        let array_handler = ArrayHandler::new();
        for pattern in array_handler.patterns() {
            self.db.add(&pattern);
        }
        
        // Register FunctionHandler patterns
        let function_handler = FunctionHandler::new();
        for pattern in function_handler.patterns() {
            self.db.add(&pattern);
        }
        
        // Register CallHandler patterns
        use crate::handlers::functions::CallHandler;
        let call_handler = CallHandler::new();
        for pattern in call_handler.patterns() {
            self.db.add(&pattern);
        }
        
        // Register DefinitionHandler patterns
        use crate::handlers::functions::DefinitionHandler;
        let definition_handler = DefinitionHandler::new();
        for pattern in definition_handler.patterns() {
            self.db.add(&pattern);
        }
        
        // Register VariableHandler patterns
        let variable_handler = VariableHandler::new();
        for pattern in variable_handler.patterns() {
            self.db.add(&pattern);
        }
        
        // Register ExpressionHandler patterns
        let expression_handler = ExpressionHandler::new();
        for pattern in expression_handler.patterns() {
            self.db.add(&pattern);
        }
        
        // Register LoopHandler patterns
        use crate::handlers::loops::LoopHandler;
        let loop_handler = LoopHandler::new();
        for pattern in loop_handler.patterns() {
            self.db.add(&pattern);
        }
        
        // Register IfElseHandler patterns
        use crate::handlers::conditionals::IfElseHandler;
        let if_else_handler = IfElseHandler::new();
        for pattern in if_else_handler.patterns() {
            self.db.add(&pattern);
        }
        
        // Register SwitchCaseHandler patterns
        use crate::handlers::conditionals::SwitchCaseHandler;
        let switch_case_handler = SwitchCaseHandler::new();
        for pattern in switch_case_handler.patterns() {
            self.db.add(&pattern);
        }
        
        // Register StructHandler patterns
        use crate::handlers::structs::StructHandler;
        let struct_handler = StructHandler::new();
        for pattern in struct_handler.patterns() {
            self.db.add(&pattern);
        }
        
        // Register EnumHandler patterns
        use crate::handlers::enums::EnumHandler;
        let enum_handler = EnumHandler::new();
        for pattern in enum_handler.patterns() {
            self.db.add(&pattern);
        }
        
        // Register UnionHandler patterns
        use crate::handlers::unions::UnionHandler;
        let union_handler = UnionHandler::new();
        for pattern in union_handler.patterns() {
            self.db.add(&pattern);
        }
        
        // Register PreprocessorHandler patterns
        use crate::handlers::macros::PreprocessorHandler;
        let preprocessor_handler = PreprocessorHandler::new();
        for pattern in preprocessor_handler.patterns() {
            self.db.add(&pattern);
        }
        
        // Register DefineHandler patterns
        use crate::handlers::macros::DefineHandler;
        let define_handler = DefineHandler::new();
        for pattern in define_handler.patterns() {
            self.db.add(&pattern);
        }
        
        // Register IncludeHandler patterns
        use crate::handlers::macros::IncludeHandler;
        let include_handler = IncludeHandler::new();
        for pattern in include_handler.patterns() {
            self.db.add(&pattern);
        }
        
        // Register GlobalHandler patterns
        use crate::handlers::globals::GlobalHandler;
        let global_handler = GlobalHandler::new();
        for pattern in global_handler.patterns() {
            self.db.add(&pattern);
        }
        
        // For now, add base patterns for handlers not yet implemented
        self.add_placeholder_patterns();
    }
    
    /// Add placeholder patterns for handlers not yet implemented
    /// These will be removed as handlers are migrated to provide their own patterns
    fn add_placeholder_patterns(&mut self) {
        use crate::db::pattern::{Pattern, PatternRule};
        
        let mut id = 100; // Start IDs at 100 for placeholder patterns
        
        // Function patterns (no handler yet)
        self.db.add(&Pattern::<String>::definition(id, "function_definition", vec![
                PatternRule::type_keyword(),
                PatternRule::identifier(),
                PatternRule::exact("("),
                PatternRule::any().optional().with_can_repeat(true),
                PatternRule::exact(")"),
                PatternRule::exact("{"),
            ])
            .with_category("function")
            .with_priority(920)
            .with_min_tokens(6)
            .with_description("Function definition with body"));
        id += 1;
        
        self.db.add(&Pattern::<String>::definition(id, "function_declaration", vec![
                PatternRule::type_keyword(),
                PatternRule::identifier(),
                PatternRule::exact("("),
                PatternRule::any().optional().with_can_repeat(true),
                PatternRule::exact(")"),
                PatternRule::exact(";"),
            ])
            .with_category("function")
            .with_priority(910)
            .with_min_tokens(5)
            .with_description("Function declaration without body"));
        id += 1;
        
        // Struct/Enum patterns (no handler yet)
        self.db.add(&Pattern::<String>::definition(id, "struct_definition", vec![
                PatternRule::exact("struct"),
                PatternRule::identifier().optional(),
                PatternRule::exact("{"),
                PatternRule::any().with_can_repeat(true),
                PatternRule::exact("}"),
            ])
            .with_category("struct")
            .with_priority(140)
            .with_min_tokens(4)
            .with_description("Struct definition"));
        id += 1;
        
        self.db.add(&Pattern::<String>::definition(id, "enum_definition", vec![
                PatternRule::exact("enum"),
                PatternRule::identifier().optional(),
                PatternRule::exact("{"),
                PatternRule::any().with_can_repeat(true),
                PatternRule::exact("}"),
            ])
            .with_category("enum")
            .with_priority(140)
            .with_min_tokens(4)
            .with_description("Enum definition"));
        id += 1;
        
        // Control flow patterns (no handler yet)
        self.db.add(&Pattern::<String>::definition(id, "for_loop", vec![
                PatternRule::exact("for"),
                PatternRule::exact("("),
                PatternRule::any().with_can_repeat(true),
                PatternRule::exact(")"),
            ])
            .with_category("loop")
            .with_priority(120)
            .with_min_tokens(6)
            .with_description("For loop structure"));
        id += 1;
        
        self.db.add(&Pattern::<String>::definition(id, "while_loop", vec![
                PatternRule::exact("while"),
                PatternRule::exact("("),
                PatternRule::any().with_can_repeat(true),
                PatternRule::exact(")"),
            ])
            .with_category("loop")
            .with_priority(120)
            .with_min_tokens(4)
            .with_description("While loop structure"));
        id += 1;
        
        self.db.add(&Pattern::<String>::definition(id, "if_statement", vec![
                PatternRule::exact("if"),
                PatternRule::exact("("),
                PatternRule::any().with_can_repeat(true),
                PatternRule::exact(")"),
            ])
            .with_category("conditional")
            .with_priority(125)
            .with_min_tokens(4)
            .with_description("If statement"));
        id += 1;
        
        // Macro patterns (no handler yet)
        self.db.add(&Pattern::<String>::definition(id, "define_macro", vec![
                PatternRule::exact("#"),
                PatternRule::exact("define"),
                PatternRule::identifier(),
            ])
            .with_category("macro")
            .with_priority(200)
            .with_min_tokens(3)
            .with_description("Preprocessor define"));
        id += 1;
        
        self.db.add(&Pattern::<String>::definition(id, "include_directive", vec![
                PatternRule::exact("#"),
                PatternRule::exact("include"),
            ])
            .with_category("macro")
            .with_priority(200)
            .with_min_tokens(3)
            .with_description("Include directive"));
        id += 1;
        
        // Variable patterns (no handler yet)
        self.db.add(&Pattern::<String>::definition(id, "variable_declaration", vec![
                PatternRule::type_keyword(),
                PatternRule::identifier(),
                PatternRule::one_of(vec![";".to_string(), "=".to_string()]),
            ])
            .with_category("variable")
            .with_priority(85)
            .with_min_tokens(3)
            .with_description("Variable declaration"));
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
            return Err(Error::new(crate::Kind::Database, crate::Reason::Empty("system database"), Some("Cannot sync an empty database. Did you call the 'initialize' function yet?".to_string())));
        }
        *system_mut() = self.clone();
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
        }
        self.db.init();
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
        self.db.by_name(name)
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
        self.db.by_kind("Report")
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
        self.db.by_kind("Pattern")
            .iter()
            .filter_map(|entry| Pattern::from_entry(entry))
            .collect()
    }
    
    /// Get a pattern by name
    pub fn pattern_by_name(&self, name: &str) -> Option<Pattern> {
        self.db.by_name(name)
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
        self.db.by_name(name)
            .into_iter()
            .find(|e| e.kind() == Some("Sample"))
    }
    
    /// Get all tokens from the database (as Token structs)
    pub fn tokens(&self) -> Vec<Token> {
        self.db.by_kind("Token")
            .iter()
            .filter_map(|entry| Token::from_entry(entry))
            .collect()
    }
    
    /// Get all token sets from the database
    pub fn token_sets(&self) -> Vec<TokenSet> {
        self.db.by_kind("TokenSet")
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
        
        tokenizer.get_slot(slot_id)
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
            
            // Find matching handler based on patterns
            if let Some((handler_name, confidence)) = self.match_handler_pattern(&segment) {
                result.patterns_matched += 1;
                
                // Create and run the appropriate handler
                match self.run_handler(&handler_name, &segment) {
                    Ok(handler_result) => {
                        result.handlers_invoked += 1;
                        
                        // Store result in database if successful
                        if let ProcessDecision::Complete { rust_code, confidence: _ } = handler_result {
                            // Create a report entry for the successful conversion
                            let report = Report::new(
                                format!("{}_conversion", handler_name),
                                "conversion",
                                format!("Converted to: {}", rust_code),
                            );
                            
                            self.db.add(&report);
                            result.reports_generated += 1;
                        }
                    }
                    Err(e) => {
                        result.errors.push(format!("Handler '{}' failed: {}", handler_name, e));
                    }
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
            
            // Handle comments as their own segment
            if token_str.starts_with("//") || token_str.starts_with("/*") {
                // Save any current segment first
                if !current.is_empty() {
                    segments.push(std::mem::take(&mut current));
                }
                // Comment is its own segment
                segments.push(vec![token.clone()]);
                continue;
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
                // Preprocessor directives end at newline, but since we don't have newline tokens,
                // we end when we see a token that clearly starts a new top-level construct
                let starts_new_construct = matches!(token_str.as_str(),
                    // Type keywords that start declarations
                    "int" | "char" | "void" | "float" | "double" | "long" | "short" |
                    "unsigned" | "signed" | "struct" | "enum" | "union" | "typedef" |
                    // Storage classes that start declarations
                    "static" | "extern" | "inline" | "const" | "volatile" |
                    // Control flow
                    "if" | "for" | "while" | "do" | "switch" | "return" | "break" | "continue" |
                    // New preprocessor directive
                    "#"
                ) && current.len() > 2; // Only trigger if we have more than just "# directive"
                
                if starts_new_construct {
                    // Save preprocessor directive and start new segment
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
                    // End of block at top level
                    if brace_depth == 0 && !current.is_empty() {
                        segments.push(std::mem::take(&mut current));
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
    
    /// Match tokens against registered patterns stored in the database
    /// 
    /// Returns the handler category and confidence score for the best matching pattern
    fn match_handler_pattern(&self, tokens: &[Token]) -> Option<(String, f64)> {
        use crate::db::pattern::Pattern;
        
        if tokens.is_empty() {
            return None;
        }
        
        // Convert tokens to strings for pattern matching
        let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        
        // Validate token ordering with keyword rules
        if !self.keyword_order.validate(&token_strings) {
            // Tokens don't follow valid C ordering - might still be valid but lower confidence
        }
        
        // Try to match against all patterns stored in db, find best match by priority
        let mut best_match: Option<(String, f64, usize)> = None; // (category, confidence, priority)
        
        // Query patterns from database by kind
        for entry in self.db.by_kind("Pattern") {
            if let Some(pattern) = Pattern::<String>::from_entry(entry) {
                if let Some(confidence) = pattern.matches_tokens(&token_strings) {
                    let priority = pattern.priority() as usize;
                    
                    // Keep best match (highest priority, then highest confidence)
                    let dominated = best_match.as_ref().map_or(false, |(_, _, best_pri)| {
                        *best_pri > priority || (*best_pri == priority && best_match.as_ref().unwrap().1 > confidence)
                    });
                    
                    if !dominated {
                        best_match = Some((pattern.category().to_string(), confidence, priority));
                    }
                }
            }
        }
        
        // If no pattern matched, try keyword-based fallback
        if best_match.is_none() {
            best_match = self.match_handler_by_keyword(&token_strings);
        }
        
        best_match.map(|(cat, conf, _)| (cat, conf))
    }
    
    /// Fallback keyword-based matching when no pattern matches
    fn match_handler_by_keyword(&self, tokens: &[String]) -> Option<(String, f64, usize)> {
        if tokens.is_empty() {
            return None;
        }
        
        let first_token = &tokens[0];
        
        // Use keyword module for classification
        if let Some(kw) = self.keyword_order.keyword(first_token) {
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
                        // Tokenizer may combine them
                        match first_token.as_str() {
                            "#ifdef" | "#ifndef" | "#endif" | "#else" | "#elif" | "#if" => "preprocessor",
                            "#define" => "define",
                            "#include" => "include",
                            _ => "macro",
                        }
                    }
                }
                crate::db::keyword::KeywordCategory::ControlFlow => {
                    match first_token.as_str() {
                        "for" | "while" | "do" => "loop",
                        _ => "control",
                    }
                }
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
                        let has_semicolon = tokens.iter().any(|t| t == ";");
                        
                        if has_paren && has_brace {
                            "function"
                        } else if has_paren && has_semicolon {
                            "function"
                        } else if has_semicolon {
                            "variable"
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
    fn run_handler(&mut self, handler_name: &str, tokens: &[Token]) -> Result<ProcessDecision, String> {
        // Collected results from nested handlers
        let mut nested_results: Vec<(std::ops::Range<usize>, String)> = Vec::new();
        
        let decision = match handler_name {
            "typedef" => {
                use crate::handlers::typedefs::TypedefHandler;
                self.run_handler_loop(TypedefHandler::new(), tokens, &mut nested_results)
            }
            "comment" => {
                use crate::handlers::comments::CommentHandler;
                self.run_handler_loop(CommentHandler::new(), tokens, &mut nested_results)
            }
            "array" => {
                use crate::handlers::arrays::ArrayHandler;
                self.run_handler_loop(ArrayHandler::new(), tokens, &mut nested_results)
            }
            "function" => {
                use crate::handlers::functions::FunctionHandler;
                self.run_handler_loop(FunctionHandler::new(), tokens, &mut nested_results)
            }
            "function_definition" | "static_function_definition" | 
            "inline_function_definition" | "static_inline_function_definition" => {
                use crate::handlers::functions::DefinitionHandler;
                self.run_handler_loop(DefinitionHandler::new(), tokens, &mut nested_results)
            }
            "call" => {
                use crate::handlers::functions::CallHandler;
                self.run_handler_loop(CallHandler::new(), tokens, &mut nested_results)
            }
            "variable" => {
                use crate::handlers::variables::VariableHandler;
                self.run_handler_loop(VariableHandler::new(), tokens, &mut nested_results)
            }
            "expression" => {
                use crate::handlers::expressions::ExpressionHandler;
                self.run_handler_loop(ExpressionHandler::new(), tokens, &mut nested_results)
            }
            "loop" | "for_loop" | "while_loop" | "do_while_loop" => {
                use crate::handlers::loops::LoopHandler;
                self.run_handler_loop(LoopHandler::new(), tokens, &mut nested_results)
            }
            "conditional" | "if" | "if_statement" | "if_else_statement" => {
                use crate::handlers::conditionals::IfElseHandler;
                self.run_handler_loop(IfElseHandler::new(), tokens, &mut nested_results)
            }
            "switch" | "switch_statement" => {
                use crate::handlers::conditionals::SwitchCaseHandler;
                self.run_handler_loop(SwitchCaseHandler::new(), tokens, &mut nested_results)
            }
            "struct" | "struct_definition" | "typedef_struct" => {
                use crate::handlers::structs::StructHandler;
                self.run_handler_loop(StructHandler::new(), tokens, &mut nested_results)
            }
            "enum" | "enum_definition" | "typedef_enum" => {
                use crate::handlers::enums::EnumHandler;
                self.run_handler_loop(EnumHandler::new(), tokens, &mut nested_results)
            }
            "union" | "union_definition" | "typedef_union" => {
                use crate::handlers::unions::UnionHandler;
                self.run_handler_loop(UnionHandler::new(), tokens, &mut nested_results)
            }
            "preprocessor" | "ifdef" | "ifndef" | "endif" | "else_directive" | "elif" | "if_defined" => {
                use crate::handlers::macros::PreprocessorHandler;
                self.run_handler_loop(PreprocessorHandler::new(), tokens, &mut nested_results)
            }
            "define" | "define_const" | "define_macro" => {
                use crate::handlers::macros::DefineHandler;
                self.run_handler_loop(DefineHandler::new(), tokens, &mut nested_results)
            }
            "include" | "include_system" | "include_local" => {
                use crate::handlers::macros::IncludeHandler;
                self.run_handler_loop(IncludeHandler::new(), tokens, &mut nested_results)
            }
            "global" | "global_const" | "global_static" | "global_extern" | "global_init" | "global_decl" => {
                use crate::handlers::globals::GlobalHandler;
                self.run_handler_loop(GlobalHandler::new(), tokens, &mut nested_results)
            }
            // Handlers not yet implemented - return placeholder
            "macro" | "control" => {
                Ok(ProcessDecision::Complete {
                    rust_code: format!("// TODO: {} handler not yet implemented", handler_name),
                    confidence: 0.1,
                })
            }
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
    ) -> Result<ProcessDecision, String> {
        loop {
            let decision = handler.process(tokens);
            match &decision {
                ProcessDecision::Continue { .. } => continue,
                ProcessDecision::Complete { .. } => return Ok(decision),
                ProcessDecision::Route { target_handler, .. } => {
                    // Recursively call the target handler (replaces current)
                    return self.run_handler(target_handler, tokens);
                }
                ProcessDecision::RouteNested { target_handler, token_range, reason } => {
                    // Process nested construct with target handler
                    let nested_tokens = &tokens[token_range.clone()];
                    match self.run_handler(target_handler, nested_tokens) {
                        Ok(ProcessDecision::Complete { rust_code, confidence }) => {
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
                            return Err(format!("Nested handler '{}' failed: {}", target_handler, e));
                        }
                    }
                }
                ProcessDecision::NestedComplete { .. } => {
                    // This shouldn't happen at top level - return as-is
                    return Ok(decision);
                }
                ProcessDecision::Pause { reason, .. } => {
                    return Err(format!("Processing paused: {}", reason));
                }
                ProcessDecision::Fail { reason, .. } => {
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
    
    // ========================================================================
    // Type Registration
    // ========================================================================
    
    /// Register a struct type in the database
    pub fn register_struct(&mut self, c_name: &str, rust_name: &str) {
        use crate::db::node::{struct_node, NodeExt};
        let entry = struct_node(c_name)
            .with_attr("rust_name", Entry::string(rust_name))
            .with_attr("c_name", Entry::string(c_name));
        self.db.add_entry(entry);
    }
    
    /// Register an enum type in the database
    pub fn register_enum(&mut self, c_name: &str, rust_name: &str) {
        use crate::db::node::{enum_node, NodeExt};
        let entry = enum_node(c_name)
            .with_attr("rust_name", Entry::string(rust_name))
            .with_attr("c_name", Entry::string(c_name));
        self.db.add_entry(entry);
    }
    
    /// Register a union type in the database
    pub fn register_union(&mut self, c_name: &str, rust_name: &str) {
        use crate::db::node::{node, NodeExt};
        let entry = node("Union", c_name)
            .with_attr("rust_name", Entry::string(rust_name))
            .with_attr("c_name", Entry::string(c_name));
        self.db.add_entry(entry);
    }
    
    /// Register a typedef in the database
    pub fn register_typedef(&mut self, alias: &str, rust_name: &str, underlying: &str) {
        use crate::db::node::{typedef, NodeExt};
        let entry = typedef(alias, underlying)
            .with_attr("rust_name", Entry::string(rust_name))
            .with_attr("c_name", Entry::string(alias));
        self.db.add_entry(entry);
    }
    
    /// Look up a registered type by C name
    pub fn lookup_type(&self, c_name: &str) -> Option<String> {
        // Try direct lookup
        let entries = self.db.by_name(c_name);
        for entry in &entries {
            if let Some(rust_name) = entry.get_string_attr("rust_name") {
                return Some(rust_name.to_string());
            }
        }
        
        // Try with "struct " prefix removed
        if c_name.starts_with("struct ") {
            let name = c_name.strip_prefix("struct ").unwrap();
            let entries = self.db.by_name(name);
            for entry in &entries {
                if entry.kind() == Some("Struct") {
                    if let Some(rust_name) = entry.get_string_attr("rust_name") {
                        return Some(rust_name.to_string());
                    }
                }
            }
        }
        
        // Try with "enum " prefix removed
        if c_name.starts_with("enum ") {
            let name = c_name.strip_prefix("enum ").unwrap();
            let entries = self.db.by_name(name);
            for entry in &entries {
                if entry.kind() == Some("Enum") {
                    if let Some(rust_name) = entry.get_string_attr("rust_name") {
                        return Some(rust_name.to_string());
                    }
                }
            }
        }
        
        // Try with "union " prefix removed  
        if c_name.starts_with("union ") {
            let name = c_name.strip_prefix("union ").unwrap();
            let entries = self.db.by_name(name);
            for entry in &entries {
                if entry.kind() == Some("Union") {
                    if let Some(rust_name) = entry.get_string_attr("rust_name") {
                        return Some(rust_name.to_string());
                    }
                }
            }
        }
        
        None
    }
    
    /// Check if a type is registered
    pub fn is_type_registered(&self, name: &str) -> bool {
        self.lookup_type(name).is_some()
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
    pub fn register_function(&mut self, c_name: &str, rust_name: &str, return_type: &str, params: Vec<(String, String)>) {
        use crate::db::node::{node, NodeExt};
        let mut entry = node("Function", c_name)
            .with_attr("c_name", Entry::string(c_name))
            .with_attr("rust_name", Entry::string(rust_name))
            .with_attr("return_type", Entry::string(return_type))
            .with_attr("param_count", Entry::usize(params.len()));
        
        // Store parameters as a list of entries
        let param_entries: Vec<Entry> = params.iter().enumerate().map(|(i, (name, ty))| {
            let mut pe = Entry::node("Param", name);
            pe.set_attr("index", Entry::usize(i));
            pe.set_attr("name", Entry::string(name));
            pe.set_attr("type", Entry::string(ty));
            pe
        }).collect();
        entry.set_attr("params", Entry::vec(param_entries));
        
        self.db.add_entry(entry);
    }
    
    /// Register a simple function (no params/return type tracked)
    pub fn register_function_simple(&mut self, c_name: &str, rust_name: &str) {
        use crate::db::node::{node, NodeExt};
        let entry = node("Function", c_name)
            .with_attr("c_name", Entry::string(c_name))
            .with_attr("rust_name", Entry::string(rust_name));
        self.db.add_entry(entry);
    }
    
    /// Look up a registered function by C name
    pub fn lookup_function(&self, c_name: &str) -> Option<String> {
        let entries = self.db.by_name(c_name);
        for entry in &entries {
            if entry.kind() == Some("Function") {
                if let Some(rust_name) = entry.get_string_attr("rust_name") {
                    return Some(rust_name.to_string());
                }
            }
        }
        None
    }
    
    /// Check if a function is registered
    pub fn is_function_registered(&self, name: &str) -> bool {
        self.lookup_function(name).is_some()
    }
    
    /// Get all registered functions
    pub fn registered_functions(&self) -> Vec<&Entry> {
        self.db.by_kind("Function")
    }
    
    /// Get function metadata by name
    pub fn get_function_metadata(&self, c_name: &str) -> Option<&Entry> {
        self.db.by_name(c_name)
            .into_iter()
            .find(|e| e.kind() == Some("Function"))
    }
    
    // ========================================================================
    // Routing Management
    // ========================================================================
    
    /// Allocate a new routing decision ID
    pub fn allocate_routing_id(&mut self) -> u64 {
        let id = self.routing_next_id;
        self.routing_next_id += 1;
        id
    }
    
    /// Add a routing decision to the database
    pub fn add_routing_decision(&mut self, decision: &crate::db::routing::RoutingDecision) -> u64 {
        let id = decision.id;
        self.db.add(decision);
        id
    }
    
    /// Get a routing decision Entry by ID
    pub fn get_routing_entry(&self, id: u64) -> Option<&Entry> {
        self.db.by_kind("RoutingDecision")
            .into_iter()
            .find(|e| e.get_usize_attr("id") == Some(id as usize))
    }
    
    /// Get all routing decision entries
    pub fn get_all_routings(&self) -> Vec<&Entry> {
        self.db.by_kind("RoutingDecision")
    }
    
    /// Get routing entries by source handler
    pub fn routings_by_source(&self, handler: &str) -> Vec<&Entry> {
        self.db.by_kind("RoutingDecision")
            .into_iter()
            .filter(|e| e.get_string_attr("source_handler") == Some(handler))
            .collect()
    }
    
    /// Get routing entries by target handler
    pub fn routings_by_target(&self, handler: &str) -> Vec<&Entry> {
        self.db.by_kind("RoutingDecision")
            .into_iter()
            .filter(|e| e.get_string_attr("target_handler") == Some(handler))
            .collect()
    }
    
    /// Get pending routing entries
    pub fn pending_routings(&self) -> Vec<&Entry> {
        self.db.by_kind("RoutingDecision")
            .into_iter()
            .filter(|e| e.get_string_attr("status") == Some("pending"))
            .collect()
    }
    
    /// Update routing status in db (requires re-adding with new status)
    pub fn update_routing_status(&mut self, id: u64, status: &str) {
        let name = format!("routing_{}", id);
        if let Some(entry) = self.db.by_name(&name).into_iter().next() {
            let mut new_entry = entry.clone();
            new_entry.set_attr("status", Entry::string(status));
            // Note: Web doesn't have update, so we add a new version
            // The latest one will be found first in queries
        }
    }
    
    /// Get routing statistics from database
    pub fn routing_stats(&self) -> crate::db::routing::RoutingStats {
        let entries = self.db.by_kind("RoutingDecision");
        let mut pending = 0;
        let mut completed = 0;
        let mut failed = 0;
        
        for entry in &entries {
            match entry.get_string_attr("status") {
                Some("pending") | Some("in_progress") => pending += 1,
                Some("completed") => completed += 1,
                Some("failed") => failed += 1,
                _ => {}
            }
        }
        
        crate::db::routing::RoutingStats {
            total: entries.len(),
            pending,
            completed,
            failed,
        }
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
