//! Intelligent Pattern Router for C to Rust Converter
//! Routes token streams to the most appropriate specialized handlers
//! Based on advanced pattern analysis and priority scoring

use crate::pattern::TokenPattern;
use crate::token::Token;
use crate::{HandlerPhase, ReportLevel, context, report};
use core::option::Option::None;
use std::collections::HashMap;
use std::ops::Range;

/// Priority scores for different handler types
#[derive(Debug, Clone, PartialEq)]
pub struct HandlerPriority {
    pub handler_name: String,
    pub priority_score: u32,
    pub confidence: f64,
    pub pattern_matches: Vec<String>,
}

/// Smart pattern routing system
pub struct PatternRouter {
    pub name: String,
    pub handler_priorities: HashMap<String, u32>,
    pub pattern_cache: HashMap<String, Vec<TokenPattern>>,
}

impl PatternRouter {
    /// Create a new pattern router with default priorities
    pub fn new(name: &str) -> Self {
        let mut router = Self {
            name: name.to_string(),
            handler_priorities: HashMap::new(),
            pattern_cache: HashMap::new(),
        };

        router.initialize_default_priorities();
        router.initialize_pattern_cache();
        router
    }

    /// Initialize default handler priorities (higher = more specific)
    fn initialize_default_priorities(&mut self) {
        // Function handlers (most specific first)
        self.handler_priorities
            .insert("main_function_handler".to_string(), 1000);
        self.handler_priorities
            .insert("function_declaration_handler".to_string(), 900);
        self.handler_priorities
            .insert("function_definition_handler".to_string(), 850);
        self.handler_priorities
            .insert("function_call_handler".to_string(), 800);

        // Global declaration handlers
        self.handler_priorities
            .insert("extern_declaration_handler".to_string(), 950);
        self.handler_priorities
            .insert("static_variable_handler".to_string(), 920);
        self.handler_priorities
            .insert("global_constant_handler".to_string(), 910);
        self.handler_priorities
            .insert("global_variable_handler".to_string(), 700);

        // Preprocessor handlers
        self.handler_priorities
            .insert("include_directive_handler".to_string(), 980);
        self.handler_priorities
            .insert("define_macro_handler".to_string(), 970);
        self.handler_priorities
            .insert("conditional_macro_handler".to_string(), 960);
        self.handler_priorities
            .insert("pragma_directive_handler".to_string(), 940);

        // Type definition handlers
        self.handler_priorities
            .insert("struct_handler".to_string(), 880);
        self.handler_priorities
            .insert("enum_handler".to_string(), 870);
        self.handler_priorities
            .insert("typedef_handler".to_string(), 860);

        // Statement handlers
        self.handler_priorities
            .insert("control_flow_handler".to_string(), 750);
        self.handler_priorities
            .insert("array_handler".to_string(), 720);
        self.handler_priorities
            .insert("expression_handler".to_string(), 600);
        self.handler_priorities
            .insert("comment_handler".to_string(), 500);
    }

    /// Initialize pattern cache with discriminating patterns for each handler
    fn initialize_pattern_cache(&mut self) {
        // Function patterns
        self.pattern_cache.insert(
            "main_function_handler".to_string(),
            vec![
                TokenPattern::Exact("int".to_string()),
                TokenPattern::Exact("main".to_string()),
                TokenPattern::Exact("(".to_string()),
            ],
        );

        self.pattern_cache.insert(
            "function_declaration_handler".to_string(),
            vec![
                TokenPattern::TypeKeyword,
                TokenPattern::Identifier,
                TokenPattern::Exact("(".to_string()),
                TokenPattern::Any,
                TokenPattern::Exact(")".to_string()),
                TokenPattern::Exact(";".to_string()),
            ],
        );

        self.pattern_cache.insert(
            "function_definition_handler".to_string(),
            vec![
                TokenPattern::TypeKeyword,
                TokenPattern::Identifier,
                TokenPattern::Exact("(".to_string()),
                TokenPattern::Any,
                TokenPattern::Exact(")".to_string()),
                TokenPattern::Exact("{".to_string()),
            ],
        );

        // Global declaration patterns
        self.pattern_cache.insert(
            "extern_declaration_handler".to_string(),
            vec![TokenPattern::Exact("extern".to_string())],
        );

        self.pattern_cache.insert(
            "static_variable_handler".to_string(),
            vec![TokenPattern::Exact("static".to_string())],
        );

        self.pattern_cache.insert(
            "global_constant_handler".to_string(),
            vec![TokenPattern::Exact("const".to_string())],
        );

        // Preprocessor patterns
        self.pattern_cache.insert(
            "include_directive_handler".to_string(),
            vec![TokenPattern::Exact("#include".to_string())],
        );

        self.pattern_cache.insert(
            "define_macro_handler".to_string(),
            vec![TokenPattern::Exact("#define".to_string())],
        );

        self.pattern_cache.insert(
            "conditional_macro_handler".to_string(),
            vec![TokenPattern::OneOf(vec![
                "#ifdef".to_string(),
                "#ifndef".to_string(),
                "#if".to_string(),
            ])],
        );
    }

    /// Analyze tokens and return prioritized list of candidate handlers
    pub fn route_tokens(&self, token_range: Range<usize>) -> Vec<HandlerPriority> {
        let mut context = context!();
        context.pull();

        if token_range.end > context.tokens.len() || token_range.is_empty() {
            return Vec::new();
        }

        let tokens = &context.tokens[token_range.clone()];
        let mut candidates = Vec::new();

        report!(
            "pattern_router",
            "route_tokens",
            ReportLevel::Debug,
            HandlerPhase::Process,
            format!("Analyzing {} tokens for handler routing", tokens.len()),
            true
        );

        // Analyze each handler's pattern compatibility
        for (handler_name, base_priority) in &self.handler_priorities {
            let mut confidence = self.calculate_pattern_confidence(handler_name, tokens);
            let pattern_matches = self.get_matching_patterns(handler_name, tokens);

            // Apply contextual boosting
            confidence = self.apply_contextual_boosting(handler_name, tokens, confidence);

            // Only include handlers with reasonable confidence
            if confidence > 0.1 {
                let adjusted_priority = (*base_priority as f64 * confidence) as u32;

                candidates.push(HandlerPriority {
                    handler_name: handler_name.clone(),
                    priority_score: adjusted_priority,
                    confidence,
                    pattern_matches,
                });
            }
        }

        // Sort by priority score (highest first)
        candidates.sort_by(|a, b| b.priority_score.cmp(&a.priority_score));

        report!(
            "pattern_router",
            "route_tokens",
            ReportLevel::Info,
            HandlerPhase::Process,
            format!(
                "Found {} candidate handlers, top match: {} (score: {}, confidence: {:.2})",
                candidates.len(),
                candidates
                    .first()
                    .map(|c| c.handler_name.as_str())
                    .unwrap_or("none"),
                candidates.first().map(|c| c.priority_score).unwrap_or(0),
                candidates.first().map(|c| c.confidence).unwrap_or(0.0)
            ),
            true
        );

        candidates
    }

    /// Calculate pattern confidence for a specific handler
    fn calculate_pattern_confidence(&self, handler_name: &str, tokens: &[Token]) -> f64 {
        let patterns = match self.pattern_cache.get(handler_name) {
            Some(patterns) => patterns,
            None => return 0.0,
        };

        if tokens.is_empty() || patterns.is_empty() {
            return 0.0;
        }

        let mut matches = 0;
        let mut total_checks = 0;

        // Check pattern compatibility
        for (i, pattern) in patterns.iter().enumerate() {
            if i >= tokens.len() {
                break;
            }

            total_checks += 1;
            if self.token_matches_pattern(&tokens[i], pattern) {
                matches += 1;
            }
        }

        if total_checks == 0 {
            return 0.0;
        }

        let base_confidence = matches as f64 / total_checks as f64;

        // Apply length penalty for patterns that are too short for the token sequence
        let length_penalty = if patterns.len() < tokens.len() {
            0.9 // Slight penalty for incomplete pattern coverage
        } else {
            1.0
        };

        base_confidence * length_penalty
    }

    /// Check if a token matches a specific pattern
    fn token_matches_pattern(&self, token: &Token, pattern: &TokenPattern) -> bool {
        match pattern {
            TokenPattern::Exact(expected) => token.to_string() == *expected,
            TokenPattern::TypeKeyword => {
                let token_str = token.to_string();
                [
                    "int", "char", "float", "double", "void", "short", "long", "unsigned", "signed",
                ]
                .contains(&token_str.as_str())
            }
            TokenPattern::Identifier => {
                let token_str = token.to_string();
                token_str.chars().all(|c| c.is_alphanumeric() || c == '_')
                    && token_str
                        .chars()
                        .next()
                        .map(|c| c.is_alphabetic() || c == '_')
                        .unwrap_or(false)
            }
            TokenPattern::OneOf(options) => {
                let token_str = token.to_string();
                options.contains(&token_str)
            }
            TokenPattern::Any => true,
            _ => false, // Handle other pattern types
        }
    }

    /// Get list of matching patterns for debugging
    fn get_matching_patterns(&self, handler_name: &str, tokens: &[Token]) -> Vec<String> {
        let patterns = match self.pattern_cache.get(handler_name) {
            Some(patterns) => patterns,
            None => return Vec::new(),
        };

        let mut matches = Vec::new();
        for (i, pattern) in patterns.iter().enumerate() {
            if i < tokens.len() && self.token_matches_pattern(&tokens[i], pattern) {
                matches.push(format!("{}:{:?}", i, pattern));
            }
        }

        matches
    }

    /// Apply contextual boosting based on surrounding tokens and handler-specific logic
    fn apply_contextual_boosting(
        &self,
        handler_name: &str,
        tokens: &[Token],
        base_confidence: f64,
    ) -> f64 {
        let mut confidence = base_confidence;

        if tokens.is_empty() {
            return confidence;
        }

        let first_token = tokens[0].to_string();
        let last_token = tokens.last().unwrap().to_string();

        match handler_name.as_ref() {
            "main_function_handler" => {
                // Boost for main function pattern
                if tokens.len() >= 3 && first_token == "int" && tokens[1].to_string() == "main" {
                    confidence *= 2.0;
                }
            }
            "function_declaration_handler" => {
                // Boost for semicolon ending
                if last_token == ";" {
                    confidence *= 1.5;
                }
            }
            "function_definition_handler" => {
                // Boost for opening brace
                if tokens.iter().any(|t| t.to_string() == "{") {
                    confidence *= 1.5;
                }
            }
            "extern_declaration_handler" => {
                // Strong boost for extern keyword
                if first_token == "extern" {
                    confidence *= 3.0;
                }
            }
            "static_variable_handler" => {
                // Boost for static keyword
                if first_token == "static" {
                    confidence *= 2.5;
                }
            }
            "include_directive_handler" => {
                // Very strong boost for include directive
                if first_token == "#include" {
                    confidence *= 5.0;
                }
            }
            "define_macro_handler" => {
                // Very strong boost for define directive
                if first_token == "#define" {
                    confidence *= 5.0;
                }
            }
            _ => {}
        }

        // Cap confidence at 1.0
        confidence.min(1.0)
    }

    /// Get the best handler for the given tokens
    pub fn get_best_handler(&self, token_range: Range<usize>) -> Option<String> {
        let candidates = self.route_tokens(token_range);
        candidates.first().map(|c| c.handler_name.clone())
    }

    /// Get top N handlers for the given tokens
    pub fn get_top_handlers(&self, token_range: Range<usize>, n: usize) -> Vec<String> {
        let candidates = self.route_tokens(token_range);
        candidates
            .into_iter()
            .take(n)
            .map(|c| c.handler_name)
            .collect()
    }
}

impl Default for PatternRouter {
    fn default() -> Self {
        Self::new("default_pattern_router")
    }
}
