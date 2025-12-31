//! Enum Handler
//!
//! Converts C enum definitions to Rust enums.
//! Handles nested enums by flattening them (Rust doesn't support nested enum definitions).

use crate::db::pattern::{Pattern, PatternRule};
use crate::db::token::Token;
use crate::db::web::{Build, Entry};
use crate::handlers::process::{ProcessorStage, Processor, ProcessorState, ProcessorStats};
use crate::system::system;

// ============================================================================
// Enum Handler Implementation
// ============================================================================

/// A single enum variant
#[derive(Debug, Clone, Default)]
pub struct EnumVariant {
    /// Variant name
    pub name: String,
    /// Explicit value (if any)
    pub value: Option<i64>,
    /// Value expression string (if not a simple integer)
    pub value_expr: Option<String>,
}

/// An enum definition
#[derive(Debug, Clone, Default)]
pub struct EnumDef {
    /// Enum name
    pub name: String,
    /// Variants
    pub variants: Vec<EnumVariant>,
    /// Is this an anonymous enum
    pub is_anonymous: bool,
}

impl Build for EnumDef {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("Enum", &self.name);
        entry.set_attr("c_type", Entry::string(&self.name));
        entry.set_attr("rust_type", Entry::string(&self.name));
        entry.set_attr("is_anonymous", Entry::bool(self.is_anonymous));
        entry.set_attr("variant_count", Entry::usize(self.variants.len()));

        let variant_entries: Vec<Entry> = self
            .variants
            .iter()
            .map(|v| {
                let mut ve = Entry::node("Variant", &v.name);
                if let Some(val) = v.value {
                    ve.set_attr("value", Entry::i64(val));
                }
                ve
            })
            .collect();
        entry.set_attr("variants", Entry::vec(variant_entries));

        entry
    }

    fn kind(&self) -> &str {
        "Enum"
    }
    fn name(&self) -> Option<&str> {
        Some(&self.name)
    }
    fn category(&self) -> Option<&str> {
        Some("type")
    }
}

/// Extracted enum information
#[derive(Debug, Clone, Default)]
pub struct EnumData {
    /// Main enum definition
    pub main_enum: EnumDef,
    /// Typedef alias if any
    pub typedef_alias: Option<String>,
}

impl Build for EnumData {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("EnumData", &self.main_enum.name);
        entry.set_attr("enum_name", Entry::string(&self.main_enum.name));
        entry.set_attr("variant_count", Entry::usize(self.main_enum.variants.len()));
        if let Some(ref alias) = self.typedef_alias {
            entry.set_attr("typedef_alias", Entry::string(alias));
        }
        entry
    }

    fn kind(&self) -> &str {
        "EnumData"
    }

    fn name(&self) -> Option<&str> {
        Some(&self.main_enum.name)
    }

    fn category(&self) -> Option<&str> {
        Some("enum")
    }
}

/// Handler for C enum definitions
#[derive(Debug)]
pub struct EnumHandler {
    /// Generic processor state
    state: ProcessorState<EnumData>,
}

impl EnumHandler {
    pub fn new() -> Self {
        Self { state: ProcessorState::new("EnumHandler") }
    }
}

impl Default for EnumHandler {
    fn default() -> Self {
        Self::new()
    }
}

impl Processor for EnumHandler {
    fn supported_patterns(&self) -> &[&str] {
        &[
            "validate_enum_definition",
            "extract_enum_definition",
            "validate_typedef_enum",
            "extract_typedef_enum",
        ]
    }

    fn stats(&self) -> &ProcessorStats {
        system().process_stats(&self.state.stats);
        &self.state.stats
    }

    fn patterns(&self) -> Vec<(Pattern, Pattern)> {
        vec![
            (
                Pattern::definition(
                    500,
                    "validate_enum_definition",
                    vec![
                        PatternRule::exact("enum"),
                        PatternRule::identifier().optional(),
                        PatternRule::exact("{"),
                        PatternRule::any_except("}").repeat(-1),
                        PatternRule::exact("}"),
                        PatternRule::identifier().optional(),
                        PatternRule::exact(";"),
                    ],
                )
                .with_category("enum")
                .with_priority(135)
                .with_min_tokens(4)
                .with_description("C enum definition"),
                Pattern::definition(
                    500,
                    "extract_enum_definition",
                    vec![
                        PatternRule::exact("enum"),
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
                        PatternRule::any_except("}")
                            .repeat(-1)
                            .with_extract(|rule, ctx| {
                                let token = ctx.current_token.clone();
                                if ctx.did_match && ctx.flag("in_body") {
                                    ctx.push_list("variants", &token);
                                }
                                rule.clone()
                            }),
                        PatternRule::exact("}").with_extract(|rule, ctx| {
                            ctx.set_flag("in_body", false);
                            rule.clone()
                        }),
                        PatternRule::identifier()
                            .optional()
                            .with_extract(|rule, ctx| {
                                let token = ctx.current_token.clone();
                                if ctx.did_match {
                                    ctx.set_value("var_name", &token);
                                }
                                rule.clone()
                            }),
                        PatternRule::exact(";"),
                    ],
                )
                .with_category("enum")
                .with_priority(135)
                .with_min_tokens(4)
                .with_description("C enum definition"),
            ),
            (
                Pattern::definition(
                    501,
                    "validate_typedef_enum",
                    vec![
                        PatternRule::exact("typedef"),
                        PatternRule::exact("enum"),
                        PatternRule::identifier().optional(),
                        PatternRule::exact("{"),
                        PatternRule::any_except("}").repeat(-1),
                        PatternRule::exact("}"),
                        PatternRule::identifier(),
                        PatternRule::exact(";"),
                    ],
                )
                .with_category("enum")
                .with_priority(140)
                .with_min_tokens(6)
                .with_description("Typedef enum definition"),
                Pattern::definition(
                    501,
                    "extract_typedef_enum",
                    vec![
                        PatternRule::exact("typedef").with_extract(|rule, ctx| {
                            ctx.add_modifier("typedef");
                            rule.clone()
                        }),
                        PatternRule::exact("enum"),
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
                        PatternRule::any_except("}")
                            .repeat(-1)
                            .with_extract(|rule, ctx| {
                                let token = ctx.current_token.clone();
                                if ctx.did_match && ctx.flag("in_body") {
                                    ctx.push_list("variants", &token);
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
                .with_category("enum")
                .with_priority(140)
                .with_min_tokens(6)
                .with_description("Typedef enum definition"),
            ),
        ]
    }

    fn validate(&mut self, tokens: &[Token]) -> bool {
        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        // Pattern matching handles min_tokens, "enum" keyword, and "{" requirement
        let mut best_confidence = 0.0;
        for (pattern, _) in self.patterns() {
            if let Some(confidence) = pattern.matches_tokens(&token_strs) {
                if confidence > best_confidence {
                    best_confidence = confidence;
                }
            }
        }

        if best_confidence == 0.0 {
            self.state.set_error("Not an enum definition".to_string());
            return false;
        }

        self.state.set_confidence(best_confidence);
        true
    }

    fn extract(&mut self, tokens: &[Token]) -> bool {
        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        // Try extraction patterns in order
        for (_, extract_pattern) in self.patterns() {
            if let Some(ctx) = extract_pattern.run_extraction(&token_strs) {
                // Get tag name from pattern extraction
                if let Some(tag_name) = ctx.value("tag_name") {
                    self.state.data.main_enum.name = tag_name.to_string();
                }

                // Get type_name (typedef alias) from pattern extraction
                if let Some(type_name) = ctx.value("type_name") {
                    self.state.data.typedef_alias = Some(type_name.to_string());
                    if self.state.data.main_enum.name.is_empty() {
                        self.state.data.main_enum.name = type_name.to_string();
                    }
                }

                // Get var_name (variable instance) from pattern extraction
                if let Some(var_name) = ctx.value("var_name") {
                    if self.state.data.main_enum.name.is_empty() {
                        self.state.data.main_enum.name = var_name.to_string();
                    }
                }

                // Check for typedef modifier
                if ctx.has_modifier("typedef") {
                    // Already handled via type_name
                }

                // Get variants from pattern extraction
                if let Some(variants) = ctx.list("variants") {
                    self.parse_enum_body(variants);
                }

                // If still no name, generate one
                if self.state.data.main_enum.name.is_empty() {
                    self.state.data.main_enum.name = "AnonymousEnum".to_string();
                    self.state.data.main_enum.is_anonymous = true;
                }

                return true;
            }
        }

        // Fallback to manual parsing if patterns don't match
        let mut idx = 0;
        let is_typedef = token_strs.get(0) == Some(&"typedef".to_string());

        if is_typedef {
            idx += 1;
        }

        // Skip "enum"
        if token_strs.get(idx) != Some(&"enum".to_string()) {
            self.state.set_error("Expected 'enum' keyword".to_string());
            return false;
        }
        idx += 1;

        // Get enum name (optional before brace)
        let brace_pos = match token_strs.iter().position(|t| t == "{") {
            Some(p) => p,
            None => {
                self.state.set_error("No opening brace found".to_string());
                return false;
            }
        };

        // Name is between enum and {
        if idx < brace_pos {
            self.state.data.main_enum.name = token_strs[idx].clone();
        }

        // Find matching close brace
        let close_brace = self.find_matching_brace(&token_strs, brace_pos);
        if close_brace.is_none() {
            self.state.set_error("No matching close brace".to_string());
            return false;
        }
        let close_brace = close_brace.unwrap();

        // Extract body tokens
        let body_tokens: Vec<String> = token_strs[brace_pos + 1..close_brace].to_vec();

        // Parse variants
        self.parse_enum_body(&body_tokens);

        // Check for typedef alias after closing brace
        if is_typedef && close_brace + 1 < token_strs.len() {
            let alias = &token_strs[close_brace + 1];
            if alias != ";" {
                self.state.data.typedef_alias = Some(alias.clone());
                if self.state.data.main_enum.name.is_empty() {
                    self.state.data.main_enum.name = alias.clone();
                }
            }
        }

        // If still no name, generate one
        if self.state.data.main_enum.name.is_empty() {
            self.state.data.main_enum.name = "AnonymousEnum".to_string();
            self.state.data.main_enum.is_anonymous = true;
        }

        true
    }

    fn convert(&mut self) -> Option<String> {
        let enum_name = &self.state.data.main_enum.name;
        let variants = &self.state.data.main_enum.variants;
        
        // Register the enum type with its variants
        let variant_names: Vec<&str> = variants.iter().map(|v| v.name.as_str()).collect();
        crate::system::system().register_enum_with_variants(enum_name, enum_name, &variant_names);

        // Register typedef alias if present
        if let Some(ref alias) = self.state.data.typedef_alias {
            if alias != enum_name {
                crate::system::system().register_typedef(alias, alias, enum_name);
            }
        }

        // Pre-calculate capacity for output string
        let estimated_size = 80 + variants.len() * 32;
        let mut output = String::with_capacity(estimated_size);

        // Generate Rust enum with repr(C) for FFI compatibility
        output.push_str("#[repr(C)]\n#[derive(Debug, Clone, Copy, PartialEq, Eq)]\npub enum ");
        output.push_str(enum_name);
        output.push_str(" {\n");

        for variant in variants {
            output.push_str("    ");
            output.push_str(&variant.name);
            match (&variant.value, &variant.value_expr) {
                (Some(value), _) => {
                    output.push_str(" = ");
                    output.push_str(&value.to_string());
                }
                (None, Some(expr)) => {
                    output.push_str(" = ");
                    output.push_str(expr);
                    output.push_str(" // TODO: evaluate expression");
                }
                _ => {}
            }
            output.push_str(",\n");
        }

        output.push('}');

        // Add type alias if typedef was used with a different name
        if let Some(ref alias) = self.state.data.typedef_alias {
            if alias != enum_name {
                output.push_str("\npub type ");
                output.push_str(alias);
                output.push_str(" = ");
                output.push_str(enum_name);
                output.push(';');
            }
        }

        self.state.set_output(output.clone());
        Some(output)
    }

    fn current_stage(&self) -> ProcessorStage {
        self.state.stage()
    }
    fn set_stage(&mut self, stage: ProcessorStage) {
        self.state.set_stage(stage);
    }
    fn output(&self) -> Option<String> {
        self.state.output.clone()
    }
    fn set_output(&mut self, output: String) {
        self.state.set_output(output);
    }
    fn error(&self) -> Option<String> {
        self.state.error.clone()
    }
    fn set_error(&mut self, error: String) {
        self.state.set_error(error);
    }
    fn confidence(&self) -> f64 {
        self.state.confidence
    }
    fn set_confidence(&mut self, confidence: f64) {
        self.state.set_confidence(confidence);
    }
}

impl EnumHandler {
    /// Find matching closing brace
    fn find_matching_brace(&self, tokens: &[String], open_pos: usize) -> Option<usize> {
        let mut depth = 0;
        for (i, t) in tokens.iter().enumerate().skip(open_pos) {
            match t.as_str() {
                "{" => depth += 1,
                "}" => {
                    depth -= 1;
                    if depth == 0 {
                        return Some(i);
                    }
                }
                _ => {}
            }
        }
        None
    }

    /// Parse enum body, extracting variants
    fn parse_enum_body(&mut self, tokens: &[String]) {
        // Split by comma to get individual variants
        let mut current_variant = Vec::new();

        for token in tokens {
            if token == "," {
                if !current_variant.is_empty() {
                    if let Some(variant) = self.parse_variant(&current_variant) {
                        self.state.data.main_enum.variants.push(variant);
                    }
                    current_variant.clear();
                }
            } else {
                current_variant.push(token.clone());
            }
        }

        // Handle last variant (no trailing comma)
        if !current_variant.is_empty() {
            if let Some(variant) = self.parse_variant(&current_variant) {
                self.state.data.main_enum.variants.push(variant);
            }
        }
    }

    /// Parse a single variant
    fn parse_variant(&self, tokens: &[String]) -> Option<EnumVariant> {
        if tokens.is_empty() {
            return None;
        }

        let name = tokens[0].clone();

        // Check for = value
        if tokens.len() >= 3 && tokens[1] == "=" {
            let value_str = tokens[2..].join(" ");

            // Try to parse as integer
            if let Ok(value) = tokens[2].parse::<i64>() {
                return Some(EnumVariant {
                    name,
                    value: Some(value),
                    value_expr: None,
                });
            } else {
                // Complex expression
                return Some(EnumVariant {
                    name,
                    value: None,
                    value_expr: Some(value_str),
                });
            }
        }

        Some(EnumVariant {
            name,
            value: None,
            value_expr: None,
        })
    }
}

impl Build for EnumHandler {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("Handler", "EnumHandler");
        entry.set_attr("stage", self.state.stage().to_entry());
        entry.set_attr("confidence", Entry::f64(self.state.confidence));
        entry.set_attr("enum_name", Entry::string(&self.state.data.main_enum.name));
        entry.set_attr(
            "variant_count",
            Entry::i64(self.state.data.main_enum.variants.len() as i64),
        );
        if let Some(ref output) = self.state.output {
            entry.set_attr("output", Entry::string(output));
        }
        entry
    }

    fn kind(&self) -> &str {
        "Handler"
    }
    fn name(&self) -> Option<&str> {
        Some("EnumHandler")
    }
    fn category(&self) -> Option<&str> {
        Some("enum")
    }
}
