//! Enum Handler
//!
//! Converts C enum definitions to Rust enums.
//! Handles nested enums by flattening them (Rust doesn't support nested enum definitions).

use crate::db::pattern::{Pattern, PatternRule};
use crate::db::token::Token;
use crate::db::web::{Build, Entry};
use crate::handlers::process::{ProcessStage, Processor};
use std::ops::Range;

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

/// Handler for C enum definitions
#[derive(Debug)]
pub struct EnumHandler {
    name: String,
    stage: ProcessStage,
    confidence: f64,
    error: Option<String>,
    output: Option<String>,
    data: EnumData,
    range: Range<usize>,
    input_tokens: Vec<String>,
}

impl EnumHandler {
    pub fn new() -> Self {
        Self {
            name: "enum".to_string(),
            stage: ProcessStage::Pending,
            confidence: 0.0,
            error: None,
            output: None,
            data: EnumData::default(),
            range: 0..0,
            input_tokens: Vec::new(),
        }
    }
}

impl Default for EnumHandler {
    fn default() -> Self {
        Self::new()
    }
}

impl Processor for EnumHandler {
    fn name(&self) -> &str {
        &self.name
    }

    fn supported_patterns(&self) -> &[&str] {
        &[
            "validate_enum_definition",
            "extract_enum_definition",
            "validate_typedef_enum",
            "extract_typedef_enum",
        ]
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
        if tokens.len() < 4 {
            self.error = Some("Too few tokens for enum".to_string());
            return false;
        }

        self.input_tokens = tokens.iter().map(|t| t.to_string()).collect();
        self.range = 0..tokens.len();

        // Try matching against our patterns
        let mut best_confidence = 0.0;
        for (pattern, _) in self.patterns() {
            if let Some(confidence) = pattern.matches_tokens(&self.input_tokens) {
                if confidence > best_confidence {
                    best_confidence = confidence;
                }
            }
        }

        // Check for enum keyword
        let first = tokens[0].to_string();
        let has_enum = first == "enum"
            || (first == "typedef" && tokens.len() > 1 && tokens[1].to_string() == "enum");

        if !has_enum {
            self.error = Some("Not an enum definition".to_string());
            return false;
        }

        // Must have braces for definition
        let has_brace = self.input_tokens.iter().any(|t| t == "{");
        if !has_brace {
            self.error = Some("Enum must have body".to_string());
            return false;
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

        // Try extraction patterns in order
        for (_, extract_pattern) in self.patterns() {
            if let Some(ctx) = extract_pattern.run_extraction(&token_strs) {
                // Get tag name from pattern extraction
                if let Some(tag_name) = ctx.value("tag_name") {
                    self.data.main_enum.name = tag_name.to_string();
                }

                // Get type_name (typedef alias) from pattern extraction
                if let Some(type_name) = ctx.value("type_name") {
                    self.data.typedef_alias = Some(type_name.to_string());
                    if self.data.main_enum.name.is_empty() {
                        self.data.main_enum.name = type_name.to_string();
                    }
                }

                // Get var_name (variable instance) from pattern extraction
                if let Some(var_name) = ctx.value("var_name") {
                    if self.data.main_enum.name.is_empty() {
                        self.data.main_enum.name = var_name.to_string();
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
                if self.data.main_enum.name.is_empty() {
                    self.data.main_enum.name = format!("AnonymousEnum{}", self.range.start);
                    self.data.main_enum.is_anonymous = true;
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
            self.error = Some("Expected 'enum' keyword".to_string());
            return false;
        }
        idx += 1;

        // Get enum name (optional before brace)
        let brace_pos = match token_strs.iter().position(|t| t == "{") {
            Some(p) => p,
            None => {
                self.error = Some("No opening brace found".to_string());
                return false;
            }
        };

        // Name is between enum and {
        if idx < brace_pos {
            self.data.main_enum.name = token_strs[idx].clone();
        }

        // Find matching close brace
        let close_brace = self.find_matching_brace(&token_strs, brace_pos);
        if close_brace.is_none() {
            self.error = Some("No matching close brace".to_string());
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
                self.data.typedef_alias = Some(alias.clone());
                if self.data.main_enum.name.is_empty() {
                    self.data.main_enum.name = alias.clone();
                }
            }
        }

        // If still no name, generate one
        if self.data.main_enum.name.is_empty() {
            self.data.main_enum.name = format!("AnonymousEnum{}", self.range.start);
            self.data.main_enum.is_anonymous = true;
        }

        true
    }

    fn convert(&mut self) -> Option<String> {
        let mut output = String::new();

        // Register the enum type with its variants
        let variant_names: Vec<&str> = self.data.main_enum.variants
            .iter()
            .map(|v| v.name.as_str())
            .collect();
        crate::system::system()
            .register_enum_with_variants(&self.data.main_enum.name, &self.data.main_enum.name, &variant_names);

        // Register typedef alias if present
        if let Some(ref alias) = self.data.typedef_alias {
            if alias != &self.data.main_enum.name {
                crate::system::system().register_typedef(
                    alias,
                    alias,
                    &self.data.main_enum.name,
                );
            }
        }

        // Generate Rust enum with repr(C) for FFI compatibility
        output.push_str("#[repr(C)]\n");
        output.push_str("#[derive(Debug, Clone, Copy, PartialEq, Eq)]\n");
        output.push_str(&format!("pub enum {} {{\n", self.data.main_enum.name));

        for variant in &self.data.main_enum.variants {
            if let Some(value) = variant.value {
                output.push_str(&format!("    {} = {},\n", variant.name, value));
            } else if let Some(ref expr) = variant.value_expr {
                output.push_str(&format!(
                    "    {} = {}, // TODO: evaluate expression\n",
                    variant.name, expr
                ));
            } else {
                output.push_str(&format!("    {},\n", variant.name));
            }
        }

        output.push_str("}");

        // Add type alias if typedef was used with a different name
        if let Some(ref alias) = self.data.typedef_alias {
            if alias != &self.data.main_enum.name {
                output.push_str(&format!(
                    "\npub type {} = {};",
                    alias, self.data.main_enum.name
                ));
            }
        }

        self.output = Some(output.clone());
        Some(output)
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
                        self.data.main_enum.variants.push(variant);
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
                self.data.main_enum.variants.push(variant);
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
        entry.set_attr("stage", Entry::string(self.stage.as_str()));
        entry.set_attr("confidence", Entry::f64(self.confidence));
        entry.set_attr("enum_name", Entry::string(&self.data.main_enum.name));
        entry.set_attr(
            "variant_count",
            Entry::i64(self.data.main_enum.variants.len() as i64),
        );
        if let Some(ref output) = self.output {
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
