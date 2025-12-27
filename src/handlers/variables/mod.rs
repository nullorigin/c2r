//! Variable Handler
//!
//! Converts C variable declarations to Rust.

use crate::db::convert::TypeConverter;
use crate::db::pattern::{Pattern, PatternRule};
use crate::db::token::Token;
use crate::db::web::{Build, Entry};
use crate::handlers::process::{ProcessStage, Processor};
use crate::handlers::validation::SequenceValidator;
use crate::system;

// ============================================================================
// Variable Handler Implementation
// ============================================================================

/// Extracted variable information
#[derive(Debug, Clone, Default)]
pub struct VariableData {
    pub name: String,
    pub c_type: String,
    pub initial_value: Option<String>,
    pub is_const: bool,
    pub is_static: bool,
    pub is_pointer: bool,
    /// Whether this is a struct initialization with brace syntax
    pub is_struct_init: bool,
    /// Struct field values for brace initialization
    pub struct_init_values: Vec<String>,
}

/// Handler for C variable declarations
#[derive(Debug)]
pub struct VariableHandler {
    stage: ProcessStage,
    confidence: f64,
    error: Option<String>,
    output: Option<String>,
    data: VariableData,
}

impl VariableHandler {
    pub fn new() -> Self {
        Self {
            stage: ProcessStage::Pending,
            confidence: 0.0,
            error: None,
            output: None,
            data: VariableData::default(),
        }
    }

    /// Convert variable declaration from tokens (called from DefinitionHandler via routing)
    pub fn convert_from_tokens(tokens: &[Token]) -> String {
        let mut handler = Self::new();
        if handler.validate(tokens) && handler.extract(tokens) {
            handler.convert().unwrap_or_else(|| {
                let strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
                format!("// var: {}", strs.join(" "))
            })
        } else {
            let strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
            format!("// var: {}", strs.join(" "))
        }
    }

    /// Parse brace initialization values: { val1, val2, ... }
    fn parse_brace_init(&self, tokens: &[&str]) -> Vec<String> {
        let mut values = Vec::new();
        let mut depth = 0;
        let mut current = String::new();

        for token in tokens {
            match *token {
                "{" => {
                    if depth > 0 {
                        if !current.is_empty() {
                            current.push(' ');
                        }
                        current.push_str(token);
                    }
                    depth += 1;
                }
                "}" => {
                    depth -= 1;
                    if depth > 0 {
                        if !current.is_empty() {
                            current.push(' ');
                        }
                        current.push_str(token);
                    } else if depth == 0 && !current.is_empty() {
                        values.push(current.trim().to_string());
                        current.clear();
                    }
                }
                "," => {
                    if depth == 1 {
                        if !current.is_empty() {
                            values.push(current.trim().to_string());
                            current.clear();
                        }
                    } else if depth > 1 {
                        if !current.is_empty() {
                            current.push(' ');
                        }
                        current.push_str(token);
                    }
                }
                _ => {
                    if !current.is_empty() {
                        current.push(' ');
                    }
                    current.push_str(token);
                }
            }
        }
        values
    }

    /// Convert struct initialization to Rust syntax with field names
    fn convert_struct_init(&self, struct_type: &str, values: &[String]) -> String {
        // Look up struct field names and types from database
        let field_names = system().lookup_struct_fields(struct_type)
            .unwrap_or_else(|| (0..values.len()).map(|i| format!("field{}", i)).collect());
        let field_types = system().lookup_struct_field_types(struct_type)
            .unwrap_or_else(|| vec!["".to_string(); values.len()]);

        let fields: Vec<String> = values
            .iter()
            .enumerate()
            .map(|(i, v)| {
                let field_name = field_names.get(i).cloned().unwrap_or_else(|| format!("field{}", i));
                let field_type = field_types.get(i).cloned().unwrap_or_default();
                let value = self.convert_nested_value(v.trim(), &field_type);
                format!("{}: {}", field_name, value)
            })
            .collect();

        format!("{} {{ {} }}", struct_type, fields.join(", "))
    }

    /// Convert a value, handling nested struct initialization and string literals
    fn convert_nested_value(&self, value: &str, field_type: &str) -> String {
        let trimmed = value.trim();
        
        // Check if this looks like a brace initialization (starts with { and ends with })
        if trimmed.starts_with('{') && trimmed.ends_with('}') {
            // This is a nested struct initialization
            let inner = &trimmed[1..trimmed.len()-1];
            let inner_values: Vec<String> = self.parse_nested_values(inner);
            
            // Look up the field type to get the struct name
            let struct_name = field_type.replace("struct ", "").trim().to_string();
            if !struct_name.is_empty() {
                return self.convert_struct_init(&struct_name, &inner_values);
            }
        }
        
        // Check if field type is a string/char* and value needs quotes
        let is_string_field = (field_type.contains("char") && field_type.contains("*"))
            || field_type.contains("&str")
            || field_type.contains("&'static str")
            || field_type.to_lowercase() == "string"
            || field_type.to_lowercase().contains("string");
        if is_string_field && !trimmed.starts_with('"') && !trimmed.ends_with('"') && trimmed != "None" {
            // Add quotes to string literal
            return format!("\"{}\"", trimmed);
        }
        
        value.to_string()
    }

    /// Parse nested values from a comma-separated string, respecting brace nesting
    fn parse_nested_values(&self, s: &str) -> Vec<String> {
        let mut values = Vec::new();
        let mut current = String::new();
        let mut brace_depth = 0;
        
        for c in s.chars() {
            match c {
                '{' => {
                    brace_depth += 1;
                    current.push(c);
                }
                '}' => {
                    brace_depth -= 1;
                    current.push(c);
                }
                ',' if brace_depth == 0 => {
                    if !current.trim().is_empty() {
                        values.push(current.trim().to_string());
                    }
                    current.clear();
                }
                _ => current.push(c),
            }
        }
        if !current.trim().is_empty() {
            values.push(current.trim().to_string());
        }
        values
    }
}

impl Default for VariableHandler {
    fn default() -> Self {
        Self::new()
    }
}

impl Processor for VariableHandler {
    fn name(&self) -> &str {
        "VariableHandler"
    }

    fn supported_patterns(&self) -> &[&str] {
        &[
            "validate_variable_declaration",
            "extract_variable_declaration",
        ]
    }

    fn patterns(&self) -> Vec<(Pattern, Pattern)> {
        vec![(
            Pattern::definition(
                300,
                "validate_variable_declaration",
                vec![
                    PatternRule::type_keyword(),
                    PatternRule::identifier(),
                    PatternRule::one_of(vec![";".to_string(), "=".to_string()]),
                ],
            )
            .with_category("variable")
            .with_priority(85)
            .with_min_tokens(3)
            .with_description("Variable declaration"),
            Pattern::definition(
                300,
                "extract_variable_declaration",
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
                    PatternRule::one_of(vec![";".to_string(), "=".to_string()]).with_extract(
                        |rule, ctx| {
                            let token = ctx.current_token.clone();
                            if ctx.did_match && token == "=" {
                                ctx.set_flag("has_init", true);
                            }
                            rule.clone()
                        },
                    ),
                ],
            )
            .with_category("variable")
            .with_priority(85)
            .with_min_tokens(3)
            .with_description("Variable declaration"),
        )]
    }

    fn validate(&mut self, tokens: &[Token]) -> bool {
        if tokens.len() < 3 {
            return false;
        }

        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        // Must end with semicolon
        if token_strs.last() != Some(&";".to_string()) {
            return false;
        }

        // Check for function definition: ( before = indicates function, not variable
        let eq_pos = token_strs.iter().position(|t| t == "=");
        let paren_pos = token_strs.iter().position(|t| t == "(");
        let bracket_pos = token_strs.iter().position(|t| t == "[");

        // If ( appears before = (or no =), this is a function definition
        if let Some(paren) = paren_pos {
            match eq_pos {
                Some(eq) if paren > eq => {} // ( after = is fine (function call initializer)
                _ => return false,           // ( before = or no = means function definition
            }
        }

        // If [ appears before = (or no =), this is an array declaration
        if let Some(bracket) = bracket_pos {
            match eq_pos {
                Some(eq) if bracket > eq => {} // [ after = is fine (array initializer)
                _ => return false,             // [ before = or no = means array declaration
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

        // Use keyword validation to verify variable declaration sequence
        let validator = SequenceValidator::new();
        let validation_result = validator.validate_variable(tokens);

        // Adjust confidence based on both pattern matching and keyword validation
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

        // Try extraction patterns first
        for (_, extract_pattern) in self.patterns() {
            if let Some(ctx) = extract_pattern.run_extraction(&token_strs) {
                // Get type from pattern extraction
                if let Some(var_type) = ctx.value("type") {
                    self.data.c_type = var_type.to_string();
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

                // Check for initializer - extract from tokens if has_init flag is set
                if ctx.flag("has_init") {
                    // Find = position and extract everything after it
                    if let Some(eq_pos) = token_strs.iter().position(|t| t == "=") {
                        let init_tokens: Vec<&str> = token_strs[eq_pos + 1..]
                            .iter()
                            .filter(|t| *t != ";")
                            .map(|s| s.as_str())
                            .collect();
                        if init_tokens.first() == Some(&"{") {
                            self.data.is_struct_init = true;
                            self.data.struct_init_values = self.parse_brace_init(&init_tokens);
                        } else {
                            self.data.initial_value = Some(init_tokens.join(" "));
                        }
                    }
                }

                if !self.data.name.is_empty() && !self.data.c_type.is_empty() {
                    self.confidence = 0.8;
                    return true;
                }
            }
        }

        // Fallback to manual extraction
        let token_strs: Vec<String> = tokens
            .iter()
            .map(|t| t.to_string())
            .filter(|t| t != ";")
            .collect();

        if token_strs.is_empty() {
            return false;
        }

        let mut idx = 0;

        if token_strs.get(idx) == Some(&"const".to_string()) {
            self.data.is_const = true;
            idx += 1;
        }

        if token_strs.get(idx) == Some(&"static".to_string()) {
            self.data.is_static = true;
            idx += 1;
        }

        let eq_pos = token_strs.iter().position(|t| t == "=");

        if let Some(eq) = eq_pos {
            if eq < idx + 2 {
                return false;
            }

            if token_strs[idx..eq].iter().any(|t| t == "*") {
                self.data.is_pointer = true;
            }

            self.data.name = token_strs[eq - 1].clone();
            let type_tokens: Vec<&str> = token_strs[idx..eq - 1]
                .iter()
                .filter(|t| *t != "*")
                .map(|s| s.as_str())
                .collect();
            self.data.c_type = type_tokens.join(" ");

            // Check for struct initialization with brace syntax: Type name = { ... }
            let init_tokens: Vec<&str> = token_strs[eq + 1..].iter().map(|s| s.as_str()).collect();
            if init_tokens.first() == Some(&"{") {
                // This is a struct initialization
                self.data.is_struct_init = true;
                // Parse the values between braces
                self.data.struct_init_values = self.parse_brace_init(&init_tokens);
            } else {
                self.data.initial_value = Some(init_tokens.join(" "));
            }
        } else {
            if token_strs.len() < idx + 2 {
                return false;
            }

            if token_strs[idx..].iter().any(|t| t == "*") {
                self.data.is_pointer = true;
            }

            self.data.name = token_strs.last().unwrap().clone();
            let type_tokens: Vec<&str> = token_strs[idx..token_strs.len() - 1]
                .iter()
                .filter(|t| *t != "*")
                .map(|s| s.as_str())
                .collect();
            self.data.c_type = type_tokens.join(" ");
        }

        self.confidence = 0.8;
        true
    }

    fn convert(&mut self) -> Option<String> {
        let rust_type = system().lookup_type(&self.data.c_type)
            .unwrap_or_else(|| self.data.c_type.clone());

        let full_type = if self.data.is_pointer {
            format!("*const {}", rust_type)
        } else {
            rust_type.clone()
        };

        let prefix = if self.data.is_const {
            "const"
        } else if self.data.is_static {
            "static"
        } else {
            "let mut"
        };

        let rust_code = if self.data.is_struct_init {
            // Struct initialization: Type name = { val1, val2 } -> Type { field1: val1, field2: val2 }
            let init_value = self.convert_struct_init(&rust_type, &self.data.struct_init_values);
            format!("{} {}: {} = {};", prefix, self.data.name, full_type, init_value)
        } else if let Some(ref init) = self.data.initial_value {
            // Check if the init value is an enum variant that needs prefixing
            let converted_init = self.convert_enum_variants_in_expr(init);
            format!("{} {}: {} = {};", prefix, self.data.name, full_type, converted_init)
        } else {
            // Uninitialized - need default or explicit type
            format!("{} {}: {};", prefix, self.data.name, full_type)
        };

        self.confidence = 0.85;
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

impl VariableHandler {
    /// Convert enum variants in an expression to qualified names (EnumName::Variant)
    fn convert_enum_variants_in_expr(&self, expr: &str) -> String {
        let mut result = String::new();
        let mut current_word = String::new();
        
        for c in expr.chars() {
            if c.is_alphanumeric() || c == '_' {
                current_word.push(c);
            } else {
                if !current_word.is_empty() {
                    // Check if this word is an enum variant
                    if let Some(qualified) = system().lookup_enum_variant(&current_word) {
                        result.push_str(&qualified);
                    } else {
                        result.push_str(&current_word);
                    }
                    current_word.clear();
                }
                result.push(c);
            }
        }
        
        // Handle remaining word
        if !current_word.is_empty() {
            if let Some(qualified) = system().lookup_enum_variant(&current_word) {
                result.push_str(&qualified);
            } else {
                result.push_str(&current_word);
            }
        }
        
        result
    }
}

impl Build for VariableHandler {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("Handler", "VariableHandler");
        entry.set_attr("stage", Entry::string(self.stage.as_str()));
        entry.set_attr("confidence", Entry::f64(self.confidence));
        if let Some(ref output) = self.output {
            entry.set_attr("output", Entry::string(output));
        }
        entry.set_attr("variable_name", Entry::string(&self.data.name));
        entry.set_attr("c_type", Entry::string(&self.data.c_type));
        entry
    }

    fn kind(&self) -> &str {
        "Handler"
    }
    fn name(&self) -> Option<&str> {
        Some("VariableHandler")
    }
    fn category(&self) -> Option<&str> {
        Some("variable")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_tokens(strs: &[&str]) -> Vec<Token> {
        strs.iter().map(|s| Token::from_string(*s)).collect()
    }

    #[test]
    fn test_simple_variable() {
        let tokens = make_tokens(&["int", "x", ";"]);
        let mut handler = VariableHandler::new();

        assert!(handler.validate(&tokens));
        assert!(handler.extract(&tokens));

        let output = handler.convert();
        assert!(output.is_some());
        let code = output.unwrap();
        assert!(code.contains("x"));
        assert!(code.contains("i32"));
    }

    #[test]
    fn test_variable_with_init() {
        let tokens = make_tokens(&["int", "count", "=", "10", ";"]);
        let mut handler = VariableHandler::new();

        assert!(handler.validate(&tokens));
        assert!(handler.extract(&tokens));

        let output = handler.convert();
        assert!(output.is_some());
        let code = output.unwrap();
        assert!(code.contains("count"));
        assert!(code.contains("= 10"));
    }

    #[test]
    fn test_const_variable() {
        let tokens = make_tokens(&["const", "int", "MAX", "=", "100", ";"]);
        let mut handler = VariableHandler::new();

        assert!(handler.validate(&tokens));
        assert!(handler.extract(&tokens));
        assert!(handler.data.is_const);

        let output = handler.convert();
        assert!(output.is_some());
        assert!(output.unwrap().starts_with("const"));
    }
}
