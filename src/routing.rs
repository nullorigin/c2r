/// Keyword-based routing system for efficient handler selection
/// Routes tokens to appropriate handlers based on leading keywords instead of blind iteration
/// Routes are built dynamically from handlers' supported_keywords() implementations

use crate::{Handler, Id, Token};
use std::collections::HashMap;
use std::sync::Arc;


/// Router that maps keywords to handler IDs for O(1) lookup
pub struct KeywordRouter {
    /// Map of keyword -> prioritized list of handler IDs
    keyword_map: HashMap<String, Vec<Id>>,
    /// Default handlers to try when no keyword match
    fallback_handlers: Vec<Id>,
}

impl KeywordRouter {
    /// Build router from registered handlers
    pub fn from_handlers(handlers: &[Arc<dyn Handler>]) -> Self {
        let mut router = Self {
            keyword_map: HashMap::new(),
            fallback_handlers: Vec::new(),
        };

        // Query each handler for its supported keywords
        for handler in handlers {
            let keywords = handler.supported_keywords();

            if keywords.is_empty() {
                // No specific keywords - add to fallback list
                router.fallback_handlers.push(handler.id());
            } else {
                // Register each keyword
                for keyword in keywords {
                    router.keyword_map
                        .entry(keyword)
                        .or_insert_with(Vec::new)
                        .push(handler.id());
                }
            }
        }

        router
    }

    /// Legacy method for backward compatibility - creates empty router
    pub fn new() -> Self {
        Self {
            keyword_map: HashMap::new(),
            fallback_handlers: Vec::new(),
        }
    }

    /// Manually initialize routes (for testing or when handlers aren't available)
    fn initialize_routes(&mut self) {
        // Function keywords
        self.add_route("int", Id::get("function_handler"));
        self.add_route("void", Id::get("function_handler"));
        self.add_route("char", Id::get("function_handler"));
        self.add_route("float", Id::get("function_handler"));
        self.add_route("double", Id::get("function_handler"));
        self.add_route("long", Id::get("function_handler"));
        self.add_route("short", Id::get("function_handler"));
        self.add_route("unsigned", Id::get("function_handler"));
        self.add_route("signed", Id::get("function_handler"));

        // Also route these to global_handler (second priority)
        self.add_route("int", Id::get("global_handler"));
        self.add_route("char", Id::get("global_handler"));
        self.add_route("float", Id::get("global_handler"));
        self.add_route("double", Id::get("global_handler"));

        // Also route to variable_handler (third priority)
        self.add_route("int", Id::get("variable_handler"));
        self.add_route("char", Id::get("variable_handler"));
        self.add_route("float", Id::get("variable_handler"));
        self.add_route("double", Id::get("variable_handler"));

        // Struct keywords
        self.add_route("struct", Id::get("struct_handler"));

        // Enum keywords
        self.add_route("enum", Id::get("enum_handler"));

        // Typedef keywords
        self.add_route("typedef", Id::get("typedef_handler"));

        // Loop keywords
        self.add_route("for", Id::get("loop_handler"));
        self.add_route("while", Id::get("loop_handler"));
        self.add_route("do", Id::get("loop_handler"));

        // Macro keywords (preprocessor)
        self.add_route("#define", Id::get("macro_handler"));
        self.add_route("#include", Id::get("macro_handler"));
        self.add_route("#ifdef", Id::get("macro_handler"));
        self.add_route("#ifndef", Id::get("macro_handler"));
        self.add_route("#if", Id::get("macro_handler"));
        self.add_route("#endif", Id::get("macro_handler"));
        self.add_route("#else", Id::get("macro_handler"));
        self.add_route("#elif", Id::get("macro_handler"));
        self.add_route("#pragma", Id::get("macro_handler"));
        self.add_route("#undef", Id::get("macro_handler"));
        self.add_route("#", Id::get("macro_handler"));

        // Comment keywords
        self.add_route("//", Id::get("comment_handler"));
        self.add_route("/*", Id::get("comment_handler"));

        // Storage class specifiers (global/variable)
        self.add_route("static", Id::get("global_handler"));
        self.add_route("extern", Id::get("global_handler"));
        self.add_route("const", Id::get("global_handler"));

        // Fallback handlers when no keyword match
        self.fallback_handlers = vec![
            Id::get("expression_handler"),
            Id::get("variable_handler"),
        ];
    }

    /// Add a keyword -> handler route
    fn add_route(&mut self, keyword: &str, handler_id: Id) {
        self.keyword_map
            .entry(keyword.to_string())
            .or_insert_with(Vec::new)
            .push(handler_id);
    }

    /// Route tokens to appropriate handlers based on first token(s)
    /// Returns prioritized list of handler IDs to try
    pub fn route(&self, tokens: &[Token]) -> Vec<Id> {
        if tokens.is_empty() {
            return self.fallback_handlers.clone();
        }

        let first_token = tokens[0].to_string();
        let second_token = tokens.get(1).map(|t| t.to_string());

        // Try two-token keywords first (like "#define")
        if let Some(second) = second_token {
            let two_token_key = format!("{}{}", first_token, second);
            if let Some(handlers) = self.keyword_map.get(&two_token_key) {
                return handlers.clone();
            }
        }

        // Try single token keyword
        if let Some(handlers) = self.keyword_map.get(&first_token) {
            let mut result = handlers.clone();
            // Add fallbacks at the end
            result.extend(self.fallback_handlers.clone());
            return result;
        }

        // No keyword match - use fallbacks
        self.fallback_handlers.clone()
    }

    /// Check if a keyword is routable
    pub fn has_route(&self, keyword: &str) -> bool {
        self.keyword_map.contains_key(keyword)
    }

    /// Get statistics about routing
    pub fn stats(&self) -> RoutingStats {
        RoutingStats {
            total_keywords: self.keyword_map.len(),
            total_routes: self.keyword_map.values().map(|v| v.len()).sum(),
            fallback_count: self.fallback_handlers.len(),
        }
    }
}

impl Default for KeywordRouter {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
pub struct RoutingStats {
    pub total_keywords: usize,
    pub total_routes: usize,
    pub fallback_count: usize,
}
