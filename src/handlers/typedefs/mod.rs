//! Typedef Handler
//!
//! Converts C typedef declarations to Rust type aliases.
//! Example: `typedef int MyInt;` -> `type MyInt = i32;`

use std::collections::HashMap;
use std::ops::Range;

use crate::db::convert::TypeConverter;
use crate::db::pattern::{Pattern, PatternRule};
use crate::db::token::Token;
use crate::db::web::{Build, Entry};
use crate::handlers::process::{ProcessDecision, ProcessStage, Processor};
use crate::system;

// ============================================================================
// Typedef Handler Implementation
// ============================================================================

/// Extracted typedef information
#[derive(Debug, Clone, Default)]
pub struct TypedefData {
    /// The new type name (alias)
    pub alias_name: String,
    /// The original C type being aliased
    pub original_type: String,
    /// Whether this is a pointer typedef
    pub is_pointer: bool,
    /// Array dimensions if any
    pub array_dims: Vec<usize>,
    /// Whether it's a function pointer
    pub is_func_ptr: bool,
}

impl Build for TypedefData {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("Typedef", &self.alias_name);
        entry.set_attr("c_name", Entry::string(&self.alias_name));
        entry.set_attr("rust_name", Entry::string(&self.alias_name));
        entry.set_attr("underlying_type", Entry::string(&self.original_type));
        entry.set_attr("is_pointer", Entry::bool(self.is_pointer));
        entry.set_attr("is_func_ptr", Entry::bool(self.is_func_ptr));
        entry
    }

    fn kind(&self) -> &str {
        "Typedef"
    }
    fn name(&self) -> Option<&str> {
        Some(&self.alias_name)
    }
    fn category(&self) -> Option<&str> {
        Some("type")
    }
}

/// Typedef handler state for processing
#[derive(Debug, Clone)]
pub struct TypedefHandler {
    stage: ProcessStage,
    confidence: f64,
    error: Option<String>,
    output: Option<String>,
    data: TypedefData,
}

impl TypedefHandler {
    pub fn new() -> Self {
        Self {
            stage: ProcessStage::Pending,
            confidence: 0.0,
            error: None,
            output: None,
            data: TypedefData::default(),
        }
    }
}

impl Default for TypedefHandler {
    fn default() -> Self {
        Self::new()
    }
}

impl Processor for TypedefHandler {
    fn supported_patterns(&self) -> &[&str] {
        &[
            "validate_typedef_struct",
            "extract_typedef_struct",
            "validate_typedef_alias",
            "extract_typedef_alias",
        ]
    }

    fn patterns(&self) -> Vec<(Pattern, Pattern)> {
        vec![
            (
                Pattern::definition(
                    0,
                    "validate_typedef_struct",
                    vec![
                        PatternRule::exact("typedef"),
                        PatternRule::exact("struct"),
                        PatternRule::identifier().optional(),
                        PatternRule::exact("{"),
                        PatternRule::any().repeat(-1),
                        PatternRule::exact("}"),
                        PatternRule::identifier(),
                        PatternRule::exact(";"),
                    ],
                )
                .with_category("typedef")
                .with_priority(130)
                .with_min_tokens(6)
                .with_description("Typedef struct definition"),
                Pattern::definition(
                    0,
                    "extract_typedef_struct",
                    vec![
                        PatternRule::exact("typedef").with_extract(|rule, ctx| {
                            ctx.add_modifier("typedef");
                            rule.clone()
                        }),
                        PatternRule::exact("struct"),
                        PatternRule::identifier()
                            .optional()
                            .with_extract(|rule, ctx| {
                                let token = ctx.current_token.clone();
                                if ctx.did_match {
                                    ctx.set_value("tag_name", &token);
                                }
                                rule.clone()
                            }),
                        PatternRule::exact("{").with_extract(|rule, ctx| {
                            ctx.set_flag("in_body", true);
                            rule.clone()
                        }),
                        PatternRule::any().repeat(-1).with_extract(|rule, ctx| {
                            let token = ctx.current_token.clone();
                            if ctx.did_match && ctx.flag("in_body") {
                                ctx.push_list("fields", &token);
                            }
                            rule.clone()
                        }),
                        PatternRule::exact("}").with_extract(|rule, ctx| {
                            ctx.set_flag("in_body", false);
                            rule.clone()
                        }),
                        PatternRule::identifier().with_extract(|rule, ctx| {
                            let token = ctx.current_token.clone();
                            if ctx.did_match {
                                ctx.set_value("type_name", &token);
                            }
                            rule.clone()
                        }),
                        PatternRule::exact(";"),
                    ],
                )
                .with_category("typedef")
                .with_priority(130)
                .with_min_tokens(6)
                .with_description("Typedef struct definition"),
            ),
            (
                Pattern::definition(
                    1,
                    "validate_typedef_alias",
                    vec![
                        PatternRule::exact("typedef"),
                        PatternRule::type_keyword(),
                        PatternRule::identifier(),
                        PatternRule::exact(";"),
                    ],
                )
                .with_category("typedef")
                .with_priority(125)
                .with_min_tokens(4)
                .with_description("Simple typedef alias"),
                Pattern::definition(
                    1,
                    "extract_typedef_alias",
                    vec![
                        PatternRule::exact("typedef").with_extract(|rule, ctx| {
                            ctx.add_modifier("typedef");
                            rule.clone()
                        }),
                        PatternRule::type_keyword().with_extract(|rule, ctx| {
                            let token = ctx.current_token.clone();
                            if ctx.did_match {
                                ctx.set_value("base_type", &token);
                            }
                            rule.clone()
                        }),
                        PatternRule::identifier().with_extract(|rule, ctx| {
                            let token = ctx.current_token.clone();
                            if ctx.did_match {
                                ctx.set_value("alias_name", &token);
                            }
                            rule.clone()
                        }),
                        PatternRule::exact(";"),
                    ],
                )
                .with_category("typedef")
                .with_priority(125)
                .with_min_tokens(4)
                .with_description("Simple typedef alias"),
            ),
        ]
    }

    fn validate(&mut self, tokens: &[Token]) -> bool {
        // Need at least: typedef <type> <name> ;
        if tokens.len() < 4 {
            self.error = Some("Too few tokens for typedef".to_string());
            return false;
        }

        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        // First token should be "typedef"
        let first = tokens[0].to_string();
        if first != "typedef" {
            self.error = Some(format!("Expected 'typedef', got '{}'", first));
            return false;
        }

        // Last token should be semicolon
        let last = tokens[tokens.len() - 1].to_string();
        if last != ";" {
            self.error = Some("Typedef must end with semicolon".to_string());
            return false;
        }

        // Check for typedef struct/enum/union with body - these should go to their respective handlers
        let second = tokens.get(1).map(|t| t.to_string()).unwrap_or_default();
        if matches!(second.as_str(), "struct" | "enum" | "union") {
            // If there's a { in the tokens, this is a definition with body - reject
            if token_strs.iter().any(|t| t == "{") {
                self.error = Some(format!("typedef {} with body should be handled by {}Handler", second, second));
                return false;
            }
        }

        // Try matching against our patterns
        let mut best_confidence = 0.0;
        for (pattern, _) in self.patterns() {
            if let Some(confidence) = pattern.matches_tokens(&token_strs) {
                if confidence > best_confidence {
                    best_confidence = confidence;
                }
            }
        }

        self.confidence = if best_confidence > 0.0 {
            best_confidence
        } else {
            0.7
        };
        true
    }

    fn extract(&mut self, tokens: &[Token]) -> bool {
        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        // Try extraction patterns first
        for (_, extract_pattern) in self.patterns() {
            if let Some(ctx) = extract_pattern.run_extraction(&token_strs) {
                // Get base type from pattern extraction
                if let Some(base_type) = ctx.value("base_type") {
                    self.data.original_type = base_type.to_string();
                }

                // Get alias name from pattern extraction
                if let Some(alias) = ctx.value("alias_name") {
                    self.data.alias_name = alias.to_string();
                }

                // Get type_name for typedef struct/enum
                if let Some(type_name) = ctx.value("type_name") {
                    self.data.alias_name = type_name.to_string();
                }

                // Get tag_name for typedef struct/enum
                if let Some(tag_name) = ctx.value("tag_name") {
                    if self.data.original_type.is_empty() {
                        self.data.original_type = tag_name.to_string();
                    }
                }

                if !self.data.alias_name.is_empty() && !self.data.original_type.is_empty() {
                    self.confidence = 0.85;
                    return true;
                }
            }
        }

        // Fallback to manual extraction
        let inner_tokens: Vec<String> = tokens[1..tokens.len() - 1]
            .iter()
            .map(|t| t.to_string())
            .collect();

        if inner_tokens.is_empty() {
            self.error = Some("No type information in typedef".to_string());
            return false;
        }

        if inner_tokens.len() < 2 {
            self.error = Some("Typedef needs at least type and name".to_string());
            return false;
        }

        // Check for function pointer typedef: typedef return_type (*name)(params)
        if let Some((alias, original)) = self.parse_function_pointer_typedef(&inner_tokens) {
            self.data.alias_name = alias;
            self.data.original_type = original;
            self.data.is_func_ptr = true;
            self.confidence = 0.85;
            return true;
        }

        self.data.alias_name = inner_tokens.last().unwrap().clone();

        let mut type_parts: Vec<&str> = Vec::new();
        for token in inner_tokens.iter().take(inner_tokens.len() - 1) {
            if *token == "*" {
                self.data.is_pointer = true;
            } else {
                type_parts.push(token);
            }
        }

        self.data.original_type = type_parts.join(" ");

        if self.data.alias_name.is_empty() || self.data.original_type.is_empty() {
            self.error = Some("Could not extract typedef name or type".to_string());
            return false;
        }

        self.confidence = 0.85;
        true
    }

    fn convert(&mut self) -> Option<String> {
        // Use TypeConverter for C to Rust type mapping
        let rust_type = system().lookup_type(&self.data.original_type)
            .unwrap_or_else(|| self.data.original_type.clone());

        // Register the typedef
        crate::system::system().register_typedef(
            &self.data.alias_name,
            &self.data.alias_name,
            &self.data.original_type,
        );

        let rust_code = if self.data.is_pointer {
            format!("type {} = *mut {};", self.data.alias_name, rust_type)
        } else {
            format!("type {} = {};", self.data.alias_name, rust_type)
        };

        self.confidence = 0.95;
        Some(rust_code)
    }

    fn current_stage(&self) -> ProcessStage {
        self.stage
    }

    fn set_stage(&mut self, stage: ProcessStage) {
        self.stage = stage;
    }

    fn output(&self) -> Option<String> {
        self.output.clone()
    }

    fn set_output(&mut self, output: String) {
        self.output = Some(output);
    }

    fn error(&self) -> Option<String> {
        self.error.clone()
    }

    fn set_error(&mut self, error: String) {
        self.error = Some(error);
    }

    fn confidence(&self) -> f64 {
        self.confidence
    }

    fn set_confidence(&mut self, confidence: f64) {
        self.confidence = confidence;
    }
}

impl TypedefHandler {
    /// Parse function pointer typedef: return_type (*name)(param_types)
    fn parse_function_pointer_typedef(&self, tokens: &[String]) -> Option<(String, String)> {
        // Look for pattern: type ( * name ) ( params )
        let mut paren_positions: Vec<usize> = Vec::new();
        for (i, t) in tokens.iter().enumerate() {
            if t == "(" {
                paren_positions.push(i);
            }
        }

        // Need at least 2 opening parens for function pointer
        if paren_positions.len() < 2 {
            return None;
        }

        let first_paren = paren_positions[0];

        // Check if this is function pointer: ( * name )
        if first_paren + 1 >= tokens.len() || tokens[first_paren + 1] != "*" {
            return None;
        }

        // Find closing paren for the (*name) part
        let close_first = tokens.iter().position(|t| t == ")")?;
        if close_first <= first_paren + 2 {
            return None;
        }

        // Name is between (* and )
        let name = tokens[first_paren + 2].clone();

        // Return type is before first (
        let return_type: String = tokens[..first_paren].join(" ");

        // Params are between second ( and last )
        let second_paren = paren_positions.get(1)?;
        let last_close = tokens.iter().rposition(|t| t == ")")?;

        let param_tokens: Vec<&str> = tokens[second_paren + 1..last_close]
            .iter()
            .map(|s| s.as_str())
            .collect();

        // Convert to Rust funtion type
        let rust_return = system().lookup_type(&return_type)
            .unwrap_or_else(|| return_type.clone());

        // Parse parameter types
        let mut rust_params: Vec<String> = Vec::new();
        let mut current_param: Vec<&str> = Vec::new();

        for token in &param_tokens {
            if *token == "," {
                if !current_param.is_empty() {
                    let param_type = current_param.join(" ");
                    let rust_type = system().lookup_type(&param_type).unwrap_or_else(|| param_type);
                    rust_params.push(rust_type);
                    current_param.clear();
                }
            } else {
                current_param.push(token);
            }
        }
        if !current_param.is_empty() {
            let param_type = current_param.join(" ");
            let rust_type = system().lookup_type(&param_type).unwrap_or_else(|| param_type);
            rust_params.push(rust_type);
        }

        // Build Rust function pointer type
        let original_type = if rust_return == "()" || rust_return == "void" {
            format!("Option<fn({})>", rust_params.join(", "))
        } else {
            format!("Option<fn({}) -> {}>", rust_params.join(", "), rust_return)
        };

        Some((name, original_type))
    }
}

// ============================================================================
// Build Trait Implementation - Stores handler state incrementally
// ============================================================================

impl Build for TypedefHandler {
    fn to_entry(&self) -> Entry {
        let mut attrs = HashMap::new();

        // Store stage
        attrs.insert("stage".to_string(), Entry::string(self.stage.as_str()));

        // Store confidence
        attrs.insert("confidence".to_string(), Entry::f64(self.confidence));

        // Store extracted data
        attrs.insert(
            "alias_name".to_string(),
            Entry::string(&self.data.alias_name),
        );
        attrs.insert(
            "original_type".to_string(),
            Entry::string(&self.data.original_type),
        );
        attrs.insert("is_pointer".to_string(), Entry::bool(self.data.is_pointer));

        // Store output if available
        if let Some(ref output) = self.output {
            attrs.insert("rust_code".to_string(), Entry::string(output));
        }

        // Store error if any
        if let Some(ref error) = self.error {
            attrs.insert("error".to_string(), Entry::string(error));
        }

        Entry::node_with_attrs("Handler", "TypedefHandler", attrs)
    }

    fn kind(&self) -> &str {
        "TypedefHandler"
    }

    fn name(&self) -> Option<&str> {
        if self.data.alias_name.is_empty() {
            None
        } else {
            Some(&self.data.alias_name)
        }
    }

    fn category(&self) -> Option<&str> {
        Some("typedef")
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    fn make_tokens(strings: &[&str]) -> Vec<Token> {
        strings.iter().map(|s| Token::from_string(*s)).collect()
    }

    #[test]
    fn test_simple_typedef() {
        let mut handler = TypedefHandler::new();
        let tokens = make_tokens(&["typedef", "int", "MyInt", ";"]);

        // Process through all stages
        let decision = handler.process(&tokens);
        assert!(matches!(
            decision,
            ProcessDecision::Continue {
                stage: ProcessStage::Validated,
                ..
            }
        ));

        let decision = handler.process(&tokens);
        assert!(matches!(
            decision,
            ProcessDecision::Continue {
                stage: ProcessStage::Extracted,
                ..
            }
        ));

        let decision = handler.process(&tokens);
        assert!(matches!(
            decision,
            ProcessDecision::Continue {
                stage: ProcessStage::Converted,
                ..
            }
        ));

        let decision = handler.process(&tokens);
        if let ProcessDecision::Complete {
            rust_code,
            confidence,
        } = decision
        {
            assert_eq!(rust_code, "type MyInt = i32;");
            assert!(confidence > 0.9);
        } else {
            panic!("Expected Complete decision");
        }
    }

    #[test]
    fn test_pointer_typedef() {
        let mut handler = TypedefHandler::new();
        let tokens = make_tokens(&["typedef", "int", "*", "IntPtr", ";"]);

        // Run through all stages
        while !matches!(
            handler.current_stage(),
            ProcessStage::Complete | ProcessStage::Failed
        ) {
            handler.process(&tokens);
        }

        assert_eq!(
            handler.output(),
            Some("type IntPtr = *mut i32;".to_string())
        );
    }

    #[test]
    fn test_validation_failure() {
        let mut handler = TypedefHandler::new();
        let tokens = make_tokens(&["int", "x", ";"]);

        let decision = handler.process(&tokens);
        assert!(matches!(decision, ProcessDecision::Fail { .. }));
    }

    #[test]
    fn test_build_trait() {
        let mut handler = TypedefHandler::new();
        let tokens = make_tokens(&["typedef", "unsigned", "long", "Size", ";"]);

        // Process to completion
        while !matches!(
            handler.current_stage(),
            ProcessStage::Complete | ProcessStage::Failed
        ) {
            handler.process(&tokens);
        }

        let entry = handler.to_entry();
        assert_eq!(entry.kind(), Some("TypedefHandler"));
        assert_eq!(entry.name(), Some("Size"));
    }
}
