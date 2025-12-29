//! Global Handler
//!
//! Converts C global variable declarations to Rust statics/consts.
//! Examples:
//! - `int global_var = 10;` -> `static mut GLOBAL_VAR: i32 = 10;`
//! - `const int CONSTANT = 42;` -> `const CONSTANT: i32 = 42;`
//! - `static int counter = 0;` -> `static mut COUNTER: i32 = 0;`

use crate::db::pattern::{Pattern, PatternRule};
use crate::db::token::Token;
use crate::db::web::{Build, Entry};
use crate::handlers::process::{ProcessStage, Processor};
use crate::handlers::validation::SequenceValidator;
use crate::system;

// ============================================================================
// Global Handler Implementation
// ============================================================================

/// Extracted global variable information
#[derive(Debug, Clone, Default)]
pub struct GlobalData {
    /// Variable name
    pub name: String,
    /// C type
    pub c_type: String,
    /// Rust type (converted)
    pub rust_type: String,
    /// Initial value if present
    pub initial_value: Option<String>,
    /// Is this a const (immutable)
    pub is_const: bool,
    /// Is this explicitly static
    pub is_static: bool,
    /// Is this extern
    pub is_extern: bool,
    /// Is this a pointer
    pub is_pointer: bool,
    /// Array dimensions if any
    pub array_dims: Vec<usize>,
    /// Original line for reference
    pub original_line: String,
}

/// Handler for C global variable declarations
#[derive(Debug)]
pub struct GlobalHandler {
    stage: ProcessStage,
    confidence: f64,
    error: Option<String>,
    output: Option<String>,
    data: GlobalData,
}

impl GlobalHandler {
    pub fn new() -> Self {
        Self {
            stage: ProcessStage::Pending,
            confidence: 0.0,
            error: None,
            output: None,
            data: GlobalData::default(),
        }
    }
}

impl Default for GlobalHandler {
    fn default() -> Self {
        Self::new()
    }
}

impl Processor for GlobalHandler {
    fn supported_patterns(&self) -> &[&str] {
        &["global", "global_const", "global_static", "global_extern"]
    }

    fn patterns(&self) -> Vec<(Pattern, Pattern)> {
        vec![
            // const type name = value;
            (
                Pattern::definition(
                    800,
                    "validate_global_const",
                    vec![
                        PatternRule::exact("const"),
                        PatternRule::type_keyword(),
                        PatternRule::identifier(),
                        PatternRule::exact("="),
                        PatternRule::any().repeat(-1),
                        PatternRule::exact(";"),
                    ],
                )
                .with_category("global")
                .with_priority(850)
                .with_min_tokens(5)
                .with_description("C const global variable"),
                Pattern::definition(
                    800,
                    "extract_global_const",
                    vec![
                        PatternRule::exact("const").with_extract(|rule, ctx| {
                            ctx.add_modifier("const");
                            rule.clone()
                        }),
                        PatternRule::type_keyword().with_extract(|rule, ctx| {
                            let token = ctx.current_token.clone();
                            if ctx.did_match {
                                ctx.set_value("type", &token);
                            }
                            rule.clone()
                        }),
                        PatternRule::identifier().with_extract(|rule, ctx| {
                            let token = ctx.current_token.clone();
                            if ctx.did_match {
                                ctx.set_value("name", &token);
                            }
                            rule.clone()
                        }),
                        PatternRule::exact("="),
                        PatternRule::any().repeat(-1).with_extract(|rule, ctx| {
                            let token = ctx.current_token.clone();
                            if ctx.did_match {
                                ctx.push_list("value_tokens", &token);
                            }
                            rule.clone()
                        }),
                        PatternRule::exact(";"),
                    ],
                )
                .with_category("global")
                .with_priority(850)
                .with_min_tokens(5)
                .with_description("C const global variable"),
            ),
            // static type name = value;
            (
                Pattern::definition(
                    801,
                    "validate_global_static",
                    vec![
                        PatternRule::exact("static"),
                        PatternRule::type_keyword(),
                        PatternRule::identifier(),
                        PatternRule::exact("=").optional(),
                        PatternRule::any().optional().repeat(-1),
                        PatternRule::exact(";"),
                    ],
                )
                .with_category("global")
                .with_priority(845)
                .with_min_tokens(4)
                .with_description("C static global variable"),
                Pattern::definition(
                    801,
                    "extract_global_static",
                    vec![
                        PatternRule::exact("static").with_extract(|rule, ctx| {
                            ctx.add_modifier("static");
                            rule.clone()
                        }),
                        PatternRule::type_keyword().with_extract(|rule, ctx| {
                            let token = ctx.current_token.clone();
                            if ctx.did_match {
                                ctx.set_value("type", &token);
                            }
                            rule.clone()
                        }),
                        PatternRule::identifier().with_extract(|rule, ctx| {
                            let token = ctx.current_token.clone();
                            if ctx.did_match {
                                ctx.set_value("name", &token);
                            }
                            rule.clone()
                        }),
                        PatternRule::exact("=")
                            .optional()
                            .with_extract(|rule, ctx| {
                                if ctx.did_match {
                                    ctx.set_flag("has_init", true);
                                }
                                rule.clone()
                            }),
                        PatternRule::any()
                            .optional()
                            .repeat(-1)
                            .with_extract(|rule, ctx| {
                                let token = ctx.current_token.clone();
                                if ctx.did_match && ctx.flag("has_init") {
                                    ctx.push_list("value_tokens", &token);
                                }
                                rule.clone()
                            }),
                        PatternRule::exact(";"),
                    ],
                )
                .with_category("global")
                .with_priority(845)
                .with_min_tokens(4)
                .with_description("C static global variable"),
            ),
            // extern type name;
            (
                Pattern::definition(
                    802,
                    "validate_global_extern",
                    vec![
                        PatternRule::exact("extern"),
                        PatternRule::type_keyword(),
                        PatternRule::identifier(),
                        PatternRule::exact(";"),
                    ],
                )
                .with_category("global")
                .with_priority(840)
                .with_min_tokens(4)
                .with_description("C extern declaration"),
                Pattern::definition(
                    802,
                    "extract_global_extern",
                    vec![
                        PatternRule::exact("extern").with_extract(|rule, ctx| {
                            ctx.add_modifier("extern");
                            rule.clone()
                        }),
                        PatternRule::type_keyword().with_extract(|rule, ctx| {
                            let token = ctx.current_token.clone();
                            if ctx.did_match {
                                ctx.set_value("type", &token);
                            }
                            rule.clone()
                        }),
                        PatternRule::identifier().with_extract(|rule, ctx| {
                            let token = ctx.current_token.clone();
                            if ctx.did_match {
                                ctx.set_value("name", &token);
                            }
                            rule.clone()
                        }),
                        PatternRule::exact(";"),
                    ],
                )
                .with_category("global")
                .with_priority(840)
                .with_min_tokens(4)
                .with_description("C extern declaration"),
            ),
            // type name = value; (at file scope)
            (
                Pattern::definition(
                    803,
                    "validate_global_init",
                    vec![
                        PatternRule::type_keyword(),
                        PatternRule::identifier(),
                        PatternRule::exact("="),
                        PatternRule::any().repeat(-1),
                        PatternRule::exact(";"),
                    ],
                )
                .with_category("global")
                .with_priority(830)
                .with_min_tokens(4)
                .with_description("C global variable with initializer"),
                Pattern::definition(
                    803,
                    "extract_global_init",
                    vec![
                        PatternRule::type_keyword().with_extract(|rule, ctx| {
                            let token = ctx.current_token.clone();
                            if ctx.did_match {
                                ctx.set_value("type", &token);
                            }
                            rule.clone()
                        }),
                        PatternRule::identifier().with_extract(|rule, ctx| {
                            let token = ctx.current_token.clone();
                            if ctx.did_match {
                                ctx.set_value("name", &token);
                            }
                            rule.clone()
                        }),
                        PatternRule::exact("="),
                        PatternRule::any().repeat(-1).with_extract(|rule, ctx| {
                            let token = ctx.current_token.clone();
                            if ctx.did_match {
                                ctx.push_list("value_tokens", &token);
                            }
                            rule.clone()
                        }),
                        PatternRule::exact(";"),
                    ],
                )
                .with_category("global")
                .with_priority(830)
                .with_min_tokens(4)
                .with_description("C global variable with initializer"),
            ),
            // type name; (uninitialized global)
            (
                Pattern::definition(
                    804,
                    "validate_global_decl",
                    vec![
                        PatternRule::type_keyword(),
                        PatternRule::identifier(),
                        PatternRule::exact(";"),
                    ],
                )
                .with_category("global")
                .with_priority(820)
                .with_min_tokens(3)
                .with_description("C global variable declaration"),
                Pattern::definition(
                    804,
                    "extract_global_decl",
                    vec![
                        PatternRule::type_keyword().with_extract(|rule, ctx| {
                            let token = ctx.current_token.clone();
                            if ctx.did_match {
                                ctx.set_value("type", &token);
                            }
                            rule.clone()
                        }),
                        PatternRule::identifier().with_extract(|rule, ctx| {
                            let token = ctx.current_token.clone();
                            if ctx.did_match {
                                ctx.set_value("name", &token);
                            }
                            rule.clone()
                        }),
                        PatternRule::exact(";"),
                    ],
                )
                .with_category("global")
                .with_priority(820)
                .with_min_tokens(3)
                .with_description("C global variable declaration"),
            ),
        ]
    }

    fn validate(&mut self, tokens: &[Token]) -> bool {
        if tokens.len() < 3 {
            self.error = Some("Too few tokens for global declaration".to_string());
            return false;
        }

        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        // Reject forward declarations: "struct Name;" or "enum Name;" (only 3 tokens)
        // These are type declarations, not global variables
        if token_strs.len() == 3 {
            let first = token_strs[0].as_str();
            if matches!(first, "struct" | "enum" | "union") {
                self.error = Some("Forward declaration - not a global variable".to_string());
                return false;
            }
        }

        // Reject if pattern is just "struct/enum/union Name ;" with no type or value
        // Use lookup_order_chain to validate this is a variable declaration
        let tokens_before_semi: Vec<&str> = token_strs.iter()
            .filter(|s| *s != ";")
            .map(|s| s.as_str())
            .collect();
        
        if !tokens_before_semi.is_empty() {
            let first = tokens_before_semi[0];
            // If it starts with struct/enum/union and has no = or type after the name, reject
            if matches!(first, "struct" | "enum" | "union") {
                // Check if there's an = sign indicating initialization
                let has_equals = token_strs.iter().any(|t| t == "=");
                // Check if there's a type keyword before the struct/enum/union (like "static struct")
                if !has_equals && tokens_before_semi.len() <= 2 {
                    self.error = Some("Type declaration - not a global variable".to_string());
                    return false;
                }
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

        // Must end with semicolon
        if token_strs.last() != Some(&";".to_string()) {
            self.error = Some("Global declaration must end with semicolon".to_string());
            return false;
        }

        // Should not have ( ) which would indicate function
        if token_strs.iter().any(|t| t == "(") {
            self.error = Some("Has parentheses - likely a function".to_string());
            return false;
        }

        // Use keyword validation
        let validator = SequenceValidator::new();
        let validation_result = validator.validate_variable(tokens);

        let base_confidence = if best_confidence > 0.0 {
            best_confidence
        } else {
            0.7
        };
        self.confidence = validation_result.adjust_confidence(base_confidence);

        true
    }

    fn extract(&mut self, tokens: &[Token]) -> bool {
        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        self.data.original_line = token_strs.join(" ");

        // Try extraction patterns first
        for (_, extract_pattern) in self.patterns() {
            if let Some(ctx) = extract_pattern.run_extraction(&token_strs) {
                // Get type from pattern extraction
                if let Some(var_type) = ctx.value("type") {
                    self.data.c_type = var_type.to_string();
                    self.data.rust_type = system().lookup_type(&self.data.c_type)
                        .unwrap_or_else(|| self.data.c_type.clone());
                }

                // Get name from pattern extraction
                if let Some(name) = ctx.value("name") {
                    self.data.name = name.to_string();
                }

                // Check for modifiers
                if ctx.has_modifier("const") {
                    self.data.is_const = true;
                }
                if ctx.has_modifier("static") {
                    self.data.is_static = true;
                }
                if ctx.has_modifier("extern") {
                    self.data.is_extern = true;
                }

                // Get value tokens
                if let Some(values) = ctx.list("value_tokens") {
                    self.data.initial_value = Some(values.join(" "));
                }

                if !self.data.name.is_empty() && !self.data.c_type.is_empty() {
                    return true;
                }
            }
        }

        // Fallback to manual extraction
        let mut idx = 0;

        while idx < token_strs.len() {
            match token_strs[idx].as_str() {
                "const" => {
                    self.data.is_const = true;
                    idx += 1;
                }
                "static" => {
                    self.data.is_static = true;
                    idx += 1;
                }
                "extern" => {
                    self.data.is_extern = true;
                    idx += 1;
                }
                "volatile" => {
                    idx += 1;
                }
                _ => break,
            }
        }

        let mut type_parts: Vec<String> = Vec::new();
        while idx < token_strs.len() {
            let token = &token_strs[idx];

            if self.is_type_token(token) {
                type_parts.push(token.clone());
                idx += 1;
            } else if token == "*" {
                self.data.is_pointer = true;
                type_parts.push(token.clone());
                idx += 1;
            } else {
                break;
            }
        }

        if type_parts.is_empty() {
            self.error = Some("No type found".to_string());
            return false;
        }

        self.data.c_type = type_parts.join(" ");
        self.data.rust_type = system().lookup_type(&self.data.c_type)
            .unwrap_or_else(|| self.data.c_type.clone());

        // Next should be the variable name
        if idx >= token_strs.len() {
            self.error = Some("No variable name found".to_string());
            return false;
        }

        self.data.name = token_strs[idx].clone();
        idx += 1;

        // Check for array brackets
        while idx < token_strs.len() && token_strs[idx] == "[" {
            idx += 1; // skip [
            if idx < token_strs.len() && token_strs[idx] != "]" {
                if let Ok(size) = token_strs[idx].parse::<usize>() {
                    self.data.array_dims.push(size);
                }
                idx += 1;
            }
            if idx < token_strs.len() && token_strs[idx] == "]" {
                idx += 1; // skip ]
            }
        }

        // Check for initializer
        if idx < token_strs.len() && token_strs[idx] == "=" {
            idx += 1; // skip =

            // Collect everything until semicolon
            let mut value_parts: Vec<String> = Vec::new();
            while idx < token_strs.len() && token_strs[idx] != ";" {
                value_parts.push(token_strs[idx].clone());
                idx += 1;
            }

            if !value_parts.is_empty() {
                self.data.initial_value = Some(value_parts.join(" "));
            }
        }

        true
    }

    fn convert(&mut self) -> Option<String> {
        let rust_name = self.convert_name_to_rust();
        let rust_type = &self.data.rust_type;
        
        // For const string types, use &'static str (String can't be const in Rust)
        let effective_type = if self.data.is_const && (rust_type == "String" || rust_type.contains("str")) {
            "&'static str".to_string()
        } else {
            rust_type.clone()
        };

        let output = if self.data.is_const {
            // const in C -> const in Rust
            let value = self
                .data
                .initial_value
                .as_ref()
                .map(|v| self.convert_value(v))
                .unwrap_or_else(|| self.default_value(&effective_type));

            format!("pub const {}: {} = {};", rust_name, effective_type, value)
        } else if self.data.is_extern {
            // extern declaration - use extern block
            format!(
                "extern \"C\" {{\n    pub static {}: {};\n}}",
                rust_name, rust_type
            )
        } else {
            // Regular global - use static mut (unsafe in Rust)
            let value = self
                .data
                .initial_value
                .as_ref()
                .map(|v| self.convert_value(v))
                .unwrap_or_else(|| self.default_value(rust_type));

            if self.data.is_static || !self.data.is_const {
                format!(
                    "// SAFETY: Global mutable state - ensure proper synchronization\npub static mut {}: {} = {};",
                    rust_name, rust_type, value
                )
            } else {
                format!("pub const {}: {} = {};", rust_name, rust_type, value)
            }
        };

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

impl GlobalHandler {
    /// Check if a token looks like a type
    fn is_type_token(&self, token: &str) -> bool {
        matches!(
            token,
            "int"
                | "char"
                | "float"
                | "double"
                | "void"
                | "long"
                | "short"
                | "unsigned"
                | "signed"
                | "size_t"
                | "ssize_t"
                | "int8_t"
                | "int16_t"
                | "int32_t"
                | "int64_t"
                | "uint8_t"
                | "uint16_t"
                | "uint32_t"
                | "uint64_t"
                | "bool"
                | "_Bool"
        ) || token.ends_with("_t")
    }

    /// Convert C variable name to Rust convention (SCREAMING_SNAKE_CASE for globals)
    fn convert_name_to_rust(&self) -> String {
        let name = &self.data.name;

        // If already uppercase, keep it
        if name
            .chars()
            .all(|c| c.is_uppercase() || c == '_' || c.is_numeric())
        {
            return name.clone();
        }

        // Convert camelCase or snake_case to SCREAMING_SNAKE_CASE
        let mut result = String::new();
        let mut prev_lower = false;

        for ch in name.chars() {
            if ch.is_uppercase() && prev_lower {
                result.push('_');
            }
            result.push(ch.to_ascii_uppercase());
            prev_lower = ch.is_lowercase();
        }

        result
    }

    /// Convert C value to Rust value
    fn convert_value(&self, value: &str) -> String {
        let trimmed = value.trim();

        // Handle NULL
        if trimmed == "NULL" || trimmed == "0" && self.data.is_pointer {
            return "std::ptr::null_mut()".to_string();
        }

        // Handle character literals
        if trimmed.starts_with('\'') && trimmed.ends_with('\'') {
            return trimmed.to_string();
        }

        // Handle string literals - keep quotes
        if trimmed.starts_with('"') && trimmed.ends_with('"') {
            return trimmed.to_string();
        }

        // Check if this is a string type that needs quotes added
        // (string literals may have lost quotes during tokenization)
        let is_string_type = self.data.rust_type.contains("str") 
            || self.data.rust_type == "String"
            || self.data.c_type.contains("char") && self.data.c_type.contains("*");
        if is_string_type && !trimmed.is_empty() 
            && !trimmed.starts_with('"') 
            && !trimmed.chars().next().map(|c| c.is_numeric() || c == '-').unwrap_or(false) {
            return format!("\"{}\"", trimmed);
        }

        // Handle numeric suffixes
        let value = trimmed
            .trim_end_matches('L')
            .trim_end_matches('l')
            .trim_end_matches('U')
            .trim_end_matches('u')
            .trim_end_matches('F')
            .trim_end_matches('f');

        value.to_string()
    }

    /// Get default value for a Rust type
    fn default_value(&self, rust_type: &str) -> String {
        match rust_type {
            "i8" | "i16" | "i32" | "i64" | "i128" | "isize" => "0".to_string(),
            "u8" | "u16" | "u32" | "u64" | "u128" | "usize" => "0".to_string(),
            "f32" => "0.0_f32".to_string(),
            "f64" => "0.0_f64".to_string(),
            "bool" => "false".to_string(),
            "char" => "'\\0'".to_string(),
            t if t.starts_with("*") => "std::ptr::null_mut()".to_string(),
            _ => "Default::default()".to_string(),
        }
    }
}

impl Build for GlobalHandler {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("Handler", "GlobalHandler");
        entry.set_attr("stage", Entry::string(self.stage.as_str()));
        entry.set_attr("confidence", Entry::f64(self.confidence));
        entry.set_attr("name", Entry::string(&self.data.name));
        entry.set_attr("c_type", Entry::string(&self.data.c_type));
        entry.set_attr("rust_type", Entry::string(&self.data.rust_type));
        entry.set_attr("is_const", Entry::bool(self.data.is_const));
        entry.set_attr("is_static", Entry::bool(self.data.is_static));
        entry.set_attr("is_extern", Entry::bool(self.data.is_extern));
        if let Some(ref value) = self.data.initial_value {
            entry.set_attr("initial_value", Entry::string(value));
        }
        if let Some(ref output) = self.output {
            entry.set_attr("output", Entry::string(output));
        }
        entry
    }

    fn kind(&self) -> &str {
        "Handler"
    }
    fn name(&self) -> Option<&str> {
        Some("GlobalHandler")
    }
    fn category(&self) -> Option<&str> {
        Some("global")
    }
}
