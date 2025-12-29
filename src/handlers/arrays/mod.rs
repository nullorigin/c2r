//! Array Handler
//!
//! Converts C array declarations to Rust arrays.
//! Examples:
//! - `int arr[5];` -> `let arr: [i32; 5] = [0; 5];`
//! - `int arr[] = {1, 2, 3};` -> `let arr = [1, 2, 3];`
//! - `int arr[3] = {1, 2, 3};` -> `let arr: [i32; 3] = [1, 2, 3];`

use std::collections::HashMap;
use std::ops::Range;

use crate::db::convert::TypeConverter;
use crate::db::pattern::{Pattern, PatternRule};
use crate::db::token::Token;
use crate::db::web::{Build, Entry};
use crate::handlers::process::{ProcessDecision, ProcessStage, Processor};
use crate::system;

// ============================================================================
// Helper Functions
// ============================================================================

/// Get default value for a Rust type
fn default_value_for_type(rust_type: &str) -> &'static str {
    match rust_type {
        "i8" | "i16" | "i32" | "i64" | "i128" | "isize" => "0",
        "u8" | "u16" | "u32" | "u64" | "u128" | "usize" => "0",
        "f32" | "f64" => "0.0",
        "bool" => "false",
        "char" => "'\\0'",
        "()" => "()",
        _ => "Default::default()",
    }
}

// ============================================================================
// Array Handler Implementation
// ============================================================================

/// Extracted array information
#[derive(Debug, Clone, Default)]
pub struct ArrayData {
    /// Variable name
    pub name: String,
    /// Element type (C type)
    pub element_type: String,
    /// Array dimensions (supports multidimensional: [3][4][5] -> [3, 4, 5])
    pub dimensions: Vec<usize>,
    /// Initial values if any (nested for multidimensional)
    pub init_values: Vec<String>,
    /// Whether size is inferred from initializer
    pub size_inferred: bool,
    /// Whether it's a const array
    pub is_const: bool,
    /// Whether it's a static array
    pub is_static: bool,
    /// Whether this is at global/file scope (use static, not let)
    pub is_global: bool,
}

/// Array handler state for processing
#[derive(Debug, Clone)]
pub struct ArrayHandler {
    stage: ProcessStage,
    confidence: f64,
    error: Option<String>,
    output: Option<String>,
    data: ArrayData,
}

impl ArrayHandler {
    pub fn new() -> Self {
        Self {
            stage: ProcessStage::Pending,
            confidence: 0.0,
            error: None,
            output: None,
            data: ArrayData::default(),
        }
    }

    /// Set whether this array is at global scope (builder pattern)
    pub fn with_global(mut self, is_global: bool) -> Self {
        self.data.is_global = is_global;
        self
    }

    /// Build nested array type string for multidimensional arrays
    fn build_nested_array_type(&self, base_type: &str) -> String {
        if self.data.dimensions.is_empty() {
            return base_type.to_string();
        }

        let mut result = base_type.to_string();
        for dim in self.data.dimensions.iter().rev() {
            result = format!("[{}; {}]", result, dim);
        }
        result
    }

    /// Build default initialization for array
    fn build_default_init(&self, default_val: &str) -> String {
        if self.data.dimensions.is_empty() {
            return default_val.to_string();
        }

        let mut result = default_val.to_string();
        for dim in self.data.dimensions.iter().rev() {
            result = format!("[{}; {}]", result, dim);
        }
        result
    }

    /// Convert init values, handling struct array initialization
    fn convert_init_values(&self, rust_type: &str) -> String {
        // Check if this is a struct type by looking up fields
        if let Some(fields) = system().lookup_struct_fields(rust_type) {
            // This is a struct array - convert nested brace init to struct literals
            self.convert_struct_array_init(rust_type, &fields)
        } else if self.is_string_array_type() {
            // String array - add quotes to string literals
            self.data.init_values.iter()
                .map(|v| {
                    if v == "NULL" || v == "std::ptr::null()" {
                        "None".to_string()  // Use Option<&str> None for NULL
                    } else if v.starts_with('"') && v.ends_with('"') {
                        v.clone()  // Already quoted
                    } else {
                        format!("\"{}\"", v)  // Add quotes
                    }
                })
                .collect::<Vec<_>>()
                .join(", ")
        } else {
            // Simple array - just join values
            self.data.init_values.join(", ")
        }
    }

    /// Check if element type is a string/char* type
    fn is_string_array_type(&self) -> bool {
        let elem = self.data.element_type.to_lowercase();
        elem.contains("char") && (elem.contains("*") || elem.contains("[]"))
            || elem == "string"
    }

    /// Convert struct array initialization: {{a, b}, {c, d}} -> [Type { f1: a, f2: b }, ...]
    fn convert_struct_array_init(&self, struct_name: &str, fields: &[String]) -> String {
        let mut struct_inits: Vec<String> = Vec::new();
        let mut current_values: Vec<String> = Vec::new();
        let mut brace_depth = 0;

        for val in &self.data.init_values {
            match val.as_str() {
                "{" => {
                    brace_depth += 1;
                    if brace_depth == 1 {
                        current_values.clear();
                    }
                }
                "}" => {
                    brace_depth -= 1;
                    if brace_depth == 0 && !current_values.is_empty() {
                        // Build struct literal from current values
                        let mut field_inits: Vec<String> = Vec::new();
                        for (i, field) in fields.iter().enumerate() {
                            if i < current_values.len() {
                                field_inits.push(format!("{}: {}", field, current_values[i]));
                            }
                        }
                        struct_inits.push(format!("{} {{ {} }}", struct_name, field_inits.join(", ")));
                        current_values.clear();
                    }
                }
                _ => {
                    if brace_depth > 0 {
                        current_values.push(val.clone());
                    }
                }
            }
        }

        struct_inits.join(", ")
    }
}

impl Default for ArrayHandler {
    fn default() -> Self {
        Self::new()
    }
}

impl Processor for ArrayHandler {
    fn supported_patterns(&self) -> &[&str] {
        &[
            "validate_array_declaration",
            "extract_array_declaration",
            "validate_array_init",
            "extract_array_init",
        ]
    }

    fn patterns(&self) -> Vec<(Pattern, Pattern)> {
        vec![
            (
                Pattern::definition(
                    0,
                    "validate_array_declaration",
                    vec![
                        PatternRule::type_keyword(),
                        PatternRule::identifier(),
                        PatternRule::exact("["),
                        PatternRule::any().optional(),
                        PatternRule::exact("]"),
                        PatternRule::exact(";"),
                    ],
                )
                .with_category("array")
                .with_priority(145)
                .with_min_tokens(5)
                .with_description("C array declaration"),
                Pattern::definition(
                    0,
                    "extract_array_declaration",
                    vec![
                        PatternRule::type_keyword().with_extract(|rule, ctx| {
                            let token = ctx.current_token.clone();
                            if ctx.did_match {
                                ctx.set_value("element_type", &token);
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
                        PatternRule::exact("[").with_extract(|rule, ctx| {
                            ctx.set_flag("in_size", true);
                            rule.clone()
                        }),
                        PatternRule::any().optional().with_extract(|rule, ctx| {
                            let token = ctx.current_token.clone();
                            if ctx.did_match && ctx.flag("in_size") {
                                ctx.set_value("size", &token);
                            }
                            rule.clone()
                        }),
                        PatternRule::exact("]").with_extract(|rule, ctx| {
                            ctx.set_flag("in_size", false);
                            rule.clone()
                        }),
                        PatternRule::exact(";"),
                    ],
                )
                .with_category("array")
                .with_priority(145)
                .with_min_tokens(5)
                .with_description("C array declaration"),
            ),
            (
                Pattern::definition(
                    1,
                    "validate_array_init",
                    vec![
                        PatternRule::type_keyword(),
                        PatternRule::identifier(),
                        PatternRule::exact("["),
                        PatternRule::any().optional(),
                        PatternRule::exact("]"),
                        PatternRule::exact("="),
                        PatternRule::exact("{"),
                        PatternRule::any().repeat(-1),
                        PatternRule::exact("}"),
                        PatternRule::exact(";"),
                    ],
                )
                .with_category("array")
                .with_priority(150)
                .with_min_tokens(8)
                .with_description("C array with initializer"),
                Pattern::definition(
                    1,
                    "extract_array_init",
                    vec![
                        PatternRule::type_keyword().with_extract(|rule, ctx| {
                            let token = ctx.current_token.clone();
                            if ctx.did_match {
                                ctx.set_value("element_type", &token);
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
                        PatternRule::exact("[").with_extract(|rule, ctx| {
                            ctx.set_flag("in_size", true);
                            rule.clone()
                        }),
                        PatternRule::any().optional().with_extract(|rule, ctx| {
                            let token = ctx.current_token.clone();
                            if ctx.did_match && ctx.flag("in_size") {
                                ctx.set_value("size", &token);
                            }
                            rule.clone()
                        }),
                        PatternRule::exact("]").with_extract(|rule, ctx| {
                            ctx.set_flag("in_size", false);
                            rule.clone()
                        }),
                        PatternRule::exact("="),
                        PatternRule::exact("{"),
                        PatternRule::any().repeat(-1).with_extract(|rule, ctx| {
                            let token = ctx.current_token.clone();
                            if ctx.did_match {
                                ctx.push_list("init_values", &token);
                            }
                            rule.clone()
                        }),
                        PatternRule::exact("}"),
                        PatternRule::exact(";"),
                    ],
                )
                .with_category("array")
                .with_priority(150)
                .with_min_tokens(8)
                .with_description("C array with initializer"),
            ),
        ]
    }

    fn validate(&mut self, tokens: &[Token]) -> bool {
        // Need at least: type name [ ] ;
        if tokens.len() < 5 {
            self.error = Some("Too few tokens for array".to_string());
            return false;
        }

        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        // Must have [ somewhere
        if !token_strs.iter().any(|t| t == "[") {
            self.error = Some("Not an array declaration (no '[')".to_string());
            return false;
        }

        // Last token should be semicolon
        if tokens[tokens.len() - 1].to_string() != ";" {
            self.error = Some("Array declaration must end with semicolon".to_string());
            return false;
        }

        // Check for const/static modifiers
        let first = tokens[0].to_string();
        if first == "const" {
            self.data.is_const = true;
        } else if first == "static" {
            self.data.is_static = true;
            if tokens.len() > 1 && tokens[1].to_string() == "const" {
                self.data.is_const = true;
            }
        }

        // Try pattern matching for confidence
        let mut best_confidence = 0.0;
        for (pattern, _) in self.patterns() {
            if let Some(confidence) = pattern.matches_tokens(&token_strs) {
                if confidence > best_confidence {
                    best_confidence = confidence;
                }
            }
        }

        self.confidence = if best_confidence > 0.0 { best_confidence } else { 0.7 };
        true
    }

    fn extract(&mut self, tokens: &[Token]) -> bool {
        let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        // Try extraction patterns first
        for (_, extract_pattern) in self.patterns() {
            if let Some(ctx) = extract_pattern.run_extraction(&token_strings) {
                // Get element type from pattern extraction
                if let Some(element_type) = ctx.value("element_type") {
                    self.data.element_type = element_type.to_string();
                }

                // Get array name from pattern extraction
                if let Some(name) = ctx.value("name") {
                    self.data.name = name.to_string();
                }

                // Get array size from pattern extraction
                if let Some(size) = ctx.value("size") {
                    if let Ok(s) = size.parse::<usize>() {
                        self.data.dimensions.push(s);
                    }
                }

                // Get init values from pattern extraction
                if let Some(values) = ctx.list("init_values") {
                    for val in values {
                        if val != "," {
                            self.data.init_values.push(val.clone());
                        }
                    }
                    // If no explicit size, infer from initializer
                    if self.data.dimensions.is_empty() && !self.data.init_values.is_empty() {
                        self.data.dimensions.push(self.data.init_values.len());
                        self.data.size_inferred = true;
                    }
                }

                // Check for modifiers
                if ctx.has_modifier("const") {
                    self.data.is_const = true;
                }
                if ctx.has_modifier("static") {
                    self.data.is_static = true;
                }

                if !self.data.name.is_empty() && !self.data.element_type.is_empty() {
                    self.confidence = 0.85;
                    return true;
                }
            }
        }

        // Fallback to manual extraction
        let bracket_pos = token_strings.iter().position(|t| t == "[");
        if bracket_pos.is_none() {
            self.error = Some("Could not find '[' in array declaration".to_string());
            return false;
        }
        let bracket_pos = bracket_pos.unwrap();

        // Skip const/static if present
        let mut type_start = 0;
        if token_strings[0] == "const" || token_strings[0] == "static" {
            type_start = 1;
            if token_strings.len() > 1 && token_strings[1] == "const" {
                type_start = 2;
            }
        }

        // Type is everything before the name (which is right before the bracket)
        if bracket_pos < type_start + 2 {
            self.error = Some("Invalid array declaration format".to_string());
            return false;
        }

        // Name is the token right before [
        self.data.name = token_strings[bracket_pos - 1].clone();

        // Type is everything from type_start to name position
        self.data.element_type = token_strings[type_start..bracket_pos - 1].join(" ");

        // Parse all dimensions (supports multidimensional arrays like arr[3][4][5])
        let mut i = bracket_pos;
        while i < token_strings.len() {
            if token_strings[i] == "[" {
                // Find matching ]
                let mut j = i + 1;
                while j < token_strings.len() && token_strings[j] != "]" {
                    j += 1;
                }
                if j < token_strings.len() {
                    // Extract dimension size if present
                    if j > i + 1 {
                        let size_str = &token_strings[i + 1];
                        if let Ok(size) = size_str.parse::<usize>() {
                            self.data.dimensions.push(size);
                        }
                    }
                    i = j + 1;
                } else {
                    break;
                }
            } else {
                i += 1;
            }
        }

        // Check for initializer
        let equals_pos = token_strings.iter().position(|t| t == "=");
        if let Some(eq_pos) = equals_pos {
            let brace_start = token_strings.iter().position(|t| t == "{");
            let brace_end = token_strings.iter().rposition(|t| t == "}");

            if let (Some(start), Some(end)) = (brace_start, brace_end) {
                if end > start + 1 {
                    // Extract values between braces
                    for i in (start + 1)..end {
                        let val = &token_strings[i];
                        if val != "," {
                            self.data.init_values.push(val.clone());
                        }
                    }

                    // If no explicit size, infer from initializer
                    if self.data.dimensions.is_empty() && !self.data.init_values.is_empty() {
                        self.data.dimensions.push(self.data.init_values.len());
                        self.data.size_inferred = true;
                    }
                }
            }
        }

        if self.data.name.is_empty() || self.data.element_type.is_empty() {
            self.error = Some("Could not extract array name or type".to_string());
            return false;
        }

        self.confidence = 0.85;
        true
    }

    fn convert(&mut self) -> Option<String> {
        // Convert C type to Rust type
        let rust_type = system().lookup_type(&self.data.element_type)
            .unwrap_or_else(|| self.data.element_type.clone());

        // Build nested array type for multidimensional arrays
        // C: int arr[3][4] -> Rust: [[i32; 4]; 3]
        let full_type = self.build_nested_array_type(&rust_type);

        let rust_code = if !self.data.init_values.is_empty() {
            // Array with initializer
            let values = self.convert_init_values(&rust_type);
            // Global arrays use static, local arrays use let
            let prefix = if self.data.is_global {
                if self.data.is_const { "static " } else { "static " }
            } else {
                if self.data.is_const { "let " } else { "let mut " }
            };
            if self.data.size_inferred || self.data.dimensions.is_empty() {
                format!("{}{}: {} = [{}];", prefix, self.data.name, full_type, values)
            } else {
                format!(
                    "{}{}: {} = [{}];",
                    prefix, self.data.name, full_type, values
                )
            }
        } else if !self.data.dimensions.is_empty() {
            // Array declaration without initializer - use default
            let prefix = if self.data.is_global {
                if self.data.is_const { "static " } else { "static mut " }
            } else {
                if self.data.is_const { "let " } else { "let mut " }
            };
            let default_val = default_value_for_type(&rust_type);
            let default_init = self.build_default_init(default_val);
            format!(
                "{}{}: {} = {};",
                prefix, self.data.name, full_type, default_init
            )
        } else {
            // Unknown size, use Vec
            let prefix = if self.data.is_global {
                if self.data.is_const { "static " } else { "static mut " }
            } else {
                if self.data.is_const { "let " } else { "let mut " }
            };
            format!(
                "{}{}: Vec<{}> = Vec::new();",
                prefix, self.data.name, rust_type
            )
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

// ============================================================================
// Build Trait Implementation
// ============================================================================

impl Build for ArrayHandler {
    fn to_entry(&self) -> Entry {
        let mut attrs = HashMap::new();

        attrs.insert("stage".to_string(), Entry::string(self.stage.as_str()));
        attrs.insert("confidence".to_string(), Entry::f64(self.confidence));
        attrs.insert("name".to_string(), Entry::string(&self.data.name));
        attrs.insert(
            "element_type".to_string(),
            Entry::string(&self.data.element_type),
        );
        attrs.insert("is_const".to_string(), Entry::bool(self.data.is_const));
        attrs.insert("is_static".to_string(), Entry::bool(self.data.is_static));

        if !self.data.dimensions.is_empty() {
            let dims: Vec<Entry> = self
                .data
                .dimensions
                .iter()
                .map(|d| Entry::usize(*d))
                .collect();
            attrs.insert("dimensions".to_string(), Entry::vec(dims));
        }

        if !self.data.init_values.is_empty() {
            let values: Vec<Entry> = self
                .data
                .init_values
                .iter()
                .map(|v| Entry::string(v))
                .collect();
            attrs.insert("init_values".to_string(), Entry::vec(values));
        }

        if let Some(ref output) = self.output {
            attrs.insert("rust_code".to_string(), Entry::string(output));
        }

        if let Some(ref error) = self.error {
            attrs.insert("error".to_string(), Entry::string(error));
        }

        Entry::node_with_attrs("Handler", "ArrayHandler", attrs)
    }

    fn kind(&self) -> &str {
        "ArrayHandler"
    }

    fn name(&self) -> Option<&str> {
        if self.data.name.is_empty() {
            None
        } else {
            Some(&self.data.name)
        }
    }

    fn category(&self) -> Option<&str> {
        Some("array")
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
    fn test_simple_array() {
        let mut handler = ArrayHandler::new();
        let tokens = make_tokens(&["int", "arr", "[", "5", "]", ";"]);

        // Process through all stages
        while !matches!(
            handler.current_stage(),
            ProcessStage::Complete | ProcessStage::Failed
        ) {
            handler.process(&tokens);
        }

        let output = handler.output().unwrap();
        assert!(output.contains("arr"));
        assert!(output.contains("[i32; 5]"));
    }

    #[test]
    fn test_array_with_initializer() {
        let mut handler = ArrayHandler::new();
        let tokens = make_tokens(&[
            "int", "arr", "[", "3", "]", "=", "{", "1", ",", "2", ",", "3", "}", ";",
        ]);

        while !matches!(
            handler.current_stage(),
            ProcessStage::Complete | ProcessStage::Failed
        ) {
            handler.process(&tokens);
        }

        let output = handler.output().unwrap();
        assert!(output.contains("1, 2, 3"));
    }

    #[test]
    fn test_inferred_size_array() {
        let mut handler = ArrayHandler::new();
        let tokens = make_tokens(&["int", "arr", "[", "]", "=", "{", "1", ",", "2", "}", ";"]);

        while !matches!(
            handler.current_stage(),
            ProcessStage::Complete | ProcessStage::Failed
        ) {
            handler.process(&tokens);
        }

        let output = handler.output().unwrap();
        assert!(output.contains("[1, 2]"));
    }

    #[test]
    fn test_non_array_fails() {
        let mut handler = ArrayHandler::new();
        let tokens = make_tokens(&["int", "x", ";"]);

        let decision = handler.process(&tokens);
        assert!(matches!(decision, ProcessDecision::Fail { .. }));
    }

    #[test]
    fn test_build_trait() {
        let mut handler = ArrayHandler::new();
        let tokens = make_tokens(&["int", "numbers", "[", "10", "]", ";"]);

        while !matches!(
            handler.current_stage(),
            ProcessStage::Complete | ProcessStage::Failed
        ) {
            handler.process(&tokens);
        }

        let entry = handler.to_entry();
        assert_eq!(entry.kind(), Some("ArrayHandler"));
        assert_eq!(entry.name(), Some("numbers"));
    }
}
