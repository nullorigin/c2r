//! Variable Handler
//!
//! Converts C variable declarations to Rust.

use crate::db::convert::sanitize_rust_identifier;
use crate::db::pattern::{Pattern, PatternRule};
use crate::db::token::Token;
use crate::db::web::{Build, Entry};
use crate::handlers::process::{ProcessorStage, ProcessorDecision, Processor, ProcessorState, ProcessorStats};
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

impl Build for VariableData {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("VariableData", &self.name);
        entry.set_attr("name", Entry::string(&self.name));
        entry.set_attr("c_type", Entry::string(&self.c_type));
        entry.set_attr("is_const", Entry::bool(self.is_const));
        entry.set_attr("is_static", Entry::bool(self.is_static));
        entry.set_attr("is_pointer", Entry::bool(self.is_pointer));
        if let Some(ref val) = self.initial_value {
            entry.set_attr("initial_value", Entry::string(val));
        }
        entry
    }

    fn kind(&self) -> &str {
        "VariableData"
    }

    fn name(&self) -> Option<&str> {
        if self.name.is_empty() { None } else { Some(&self.name) }
    }

    fn category(&self) -> Option<&str> {
        Some("variable")
    }
}

/// Handler for C variable declarations
#[derive(Debug)]
pub struct VariableHandler {
    /// Generic processor state
    state: ProcessorState<VariableData>,
}

impl VariableHandler {
    pub fn new() -> Self {
        Self { state: ProcessorState::new("VariableHandler") }
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

    /// Smart join tokens: no space around parentheses, no space before ','
    fn smart_join_tokens(tokens: &[&str]) -> String {
        let mut result = String::new();
        for (i, token) in tokens.iter().enumerate() {
            if i > 0 {
                let prev = tokens[i - 1];
                // No space after '(' or before ')' or ',' or ';' or before '('
                let needs_space = prev != "(" && *token != ")" && *token != "," && *token != ";" && *token != "(";
                if needs_space {
                    result.push(' ');
                }
            }
            result.push_str(token);
        }
        result
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
    fn supported_patterns(&self) -> &[&str] {
        &[
            "validate_variable_declaration",
            "extract_variable_declaration",
        ]
    }

    fn stats(&self) -> &ProcessorStats {
        system().process_stats(&self.state.stats);
        &self.state.stats
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
            self.state.stats.record_failure(None, "validated");
            self.stats();
            return false;
        }

        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        // Must end with semicolon
        if token_strs.last() != Some(&";".to_string()) {
            return false;
        }

        // Reject compound assignments (+=, -=, &=, etc.) - these are expressions, not declarations
        let compound_ops = ["+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>="];
        if token_strs.iter().any(|t| compound_ops.contains(&t.as_str())) {
            return false;
        }

        // Reject pointer dereference assignments: * name = value
        // These start with * but have no type before the identifier
        if token_strs.first() == Some(&"*".to_string()) {
            // If first token is *, check if second token is an identifier (not a type)
            // A declaration would be: type * name = value (type first, then *)
            // A dereference is: * name = value (* first, then identifier)
            if token_strs.len() >= 2 {
                let second = &token_strs[1];
                // If second token is not a type keyword, this is a dereference assignment
                let is_type = crate::db::keyword::is_c_type_keyword(second)
                    || matches!(second.as_str(), "struct" | "enum" | "union" | "const")
                    || system().lookup_type(second).is_some();
                if !is_type {
                    return false; // This is a dereference assignment, not a declaration
                }
            }
        }

        // Reject pointer member access: identifier->field = value or identifier.field = value
        // These are assignments, not declarations
        let eq_pos = token_strs.iter().position(|t| t == "=");
        if let Some(eq) = eq_pos {
            // Check if there's -> or . before the =
            let before_eq: Vec<&str> = token_strs[..eq].iter().map(|s| s.as_str()).collect();
            if before_eq.contains(&"->") || before_eq.contains(&".") {
                return false; // This is a member assignment, not a declaration
            }
            // Also check for spaced arrow: "- >"
            for i in 0..before_eq.len().saturating_sub(1) {
                if before_eq[i] == "-" && before_eq.get(i + 1) == Some(&">") {
                    return false;
                }
            }
        }

        // Check for function definition: ( before = indicates function, not variable
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

        // Validate C keyword ordering using System
        let token_refs: Vec<&str> = token_strs.iter().map(|s| s.as_str()).collect();
        let (c_order_valid, invalid_pair) = system().validate_c_token_order(&token_refs);
        
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

        // Adjust confidence based on pattern matching, keyword validation, and C ordering
        let base_confidence = if best_confidence > 0.0 {
            best_confidence
        } else {
            0.7
        };
        
        // Reduce confidence if C keyword ordering is invalid
        let order_penalty = if c_order_valid { 0.0 } else { 
            if let Some((kw1, kw2)) = invalid_pair {
                // Log the invalid pair for debugging
                eprintln!("Warning: Invalid C keyword order: {} -> {}", kw1, kw2);
            }
            0.1 
        };
        
        self.state.set_confidence(validation_result.adjust_confidence(base_confidence) - order_penalty);
        self.state.stats.record_success(self.state.confidence, tokens.len(), 0, Some("variable"), "validated");
        self.stats();
        true
    }

    fn extract(&mut self, tokens: &[Token]) -> bool {
        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        // Try extraction patterns first
        for (_, extract_pattern) in self.patterns() {
            if let Some(ctx) = extract_pattern.run_extraction(&token_strs) {
                // Get type from pattern extraction
                if let Some(var_type) = ctx.value("type") {
                    self.state.data.c_type = var_type.to_string();
                }

                // Get name from pattern extraction
                if let Some(name) = ctx.value("name") {
                    self.state.data.name = name.to_string();
                }

                // Check for modifiers
                if ctx.has_modifier("const") {
                    self.state.data.is_const = true;
                }
                if ctx.has_modifier("static") {
                    self.state.data.is_static = true;
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
                            self.state.data.is_struct_init = true;
                            self.state.data.struct_init_values = self.parse_brace_init(&init_tokens);
                        } else {
                            self.state.data.initial_value = Some(Self::smart_join_tokens(&init_tokens));
                        }
                    }
                }

                if !self.state.data.name.is_empty() && !self.state.data.c_type.is_empty() {
                    self.state.set_confidence(0.8);
                    self.state.stats.record_success(self.state.confidence, 0, 0, Some("variable"), "extracted");
                    self.stats();
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
            self.state.data.is_const = true;
            idx += 1;
        }

        if token_strs.get(idx) == Some(&"static".to_string()) {
            self.state.data.is_static = true;
            idx += 1;
        }

        let eq_pos = token_strs.iter().position(|t| t == "=");

        if let Some(eq) = eq_pos {
            if eq < idx + 2 {
                return false;
            }

            if token_strs[idx..eq].iter().any(|t| t == "*") {
                self.state.data.is_pointer = true;
            }

            self.state.data.name = token_strs[eq - 1].clone();
            let type_tokens: Vec<&str> = token_strs[idx..eq - 1]
                .iter()
                .filter(|t| *t != "*")
                .map(|s| s.as_str())
                .collect();
            self.state.data.c_type = type_tokens.join(" ");

            // Check for struct initialization with brace syntax: Type name = { ... }
            let init_tokens: Vec<&str> = token_strs[eq + 1..].iter().map(|s| s.as_str()).collect();
            if init_tokens.first() == Some(&"{") {
                // This is a struct initialization
                self.state.data.is_struct_init = true;
                // Parse the values between braces
                self.state.data.struct_init_values = self.parse_brace_init(&init_tokens);
            } else {
                self.state.data.initial_value = Some(Self::smart_join_tokens(&init_tokens));
            }
        } else {
            if token_strs.len() < idx + 2 {
                return false;
            }

            if token_strs[idx..].iter().any(|t| t == "*") {
                self.state.data.is_pointer = true;
            }

            self.state.data.name = token_strs.last().unwrap().clone();
            let type_tokens: Vec<&str> = token_strs[idx..token_strs.len() - 1]
                .iter()
                .filter(|t| *t != "*")
                .map(|s| s.as_str())
                .collect();
            self.state.data.c_type = type_tokens.join(" ");
        }

        self.state.set_confidence(0.8);
        self.state.stats.record_success(self.state.confidence, 0, 0, Some("variable"), "extracted");
        self.stats();
        true
    }

    fn convert(&mut self) -> Option<String> {
        let rust_type = system().lookup_type(&self.state.data.c_type)
            .unwrap_or_else(|| self.state.data.c_type.clone());

        // For char* with string literal initializer, use &str instead of *const i8
        let full_type = if self.state.data.is_pointer {
            let is_char_type = matches!(self.state.data.c_type.as_str(), "char" | "const char");
            let has_string_init = self.state.data.initial_value.as_ref()
                .map(|v| v.trim().starts_with('"'))
                .unwrap_or(false);
            if is_char_type && has_string_init {
                "&str".to_string()
            } else {
                format!("*const {}", rust_type)
            }
        } else {
            rust_type.clone()
        };

        // Build prefix keywords and validate their order
        let mut prefix_keywords = Vec::new();
        if self.state.data.is_const {
            prefix_keywords.push("const");
        } else if self.state.data.is_static {
            prefix_keywords.push("static");
        } else {
            prefix_keywords.push("let");
            prefix_keywords.push("mut");
        }
        
        // Validate and fix Rust keyword ordering if needed
        let fixed_prefix = system().validate_or_fix_rust_order(&prefix_keywords);
        let prefix = fixed_prefix.join(" ");

        // Sanitize variable name for Rust keywords (e.g., str -> r#str)
        let var_name = sanitize_rust_identifier(&self.state.data.name);

        let rust_code = if self.state.data.is_struct_init {
            // Struct initialization: Type name = { val1, val2 } -> Type { field1: val1, field2: val2 }
            let init_value = self.convert_struct_init(&rust_type, &self.state.data.struct_init_values);
            format!("{} {}: {} = {};", prefix, var_name, full_type, init_value)
        } else if let Some(ref init) = self.state.data.initial_value {
            // Convert type casts and enum variants in the expression
            let converted_init = self.convert_expression(init);
            format!("{} {}: {} = {};", prefix, var_name, full_type, converted_init)
        } else {
            // Uninitialized - need default or explicit type
            format!("{} {}: {};", prefix, var_name, full_type)
        };

        // Register the variable in the system for sizeof() and type inference
        // Skip registration for string literals (names starting with quotes)
        let name = &self.state.data.name;
        if !name.starts_with('"') && !name.starts_with('\'') && !name.is_empty() {
            if self.state.data.is_pointer {
                // Extract base type for pointer registration
                system().register_pointer_variable(name, &self.state.data.c_type, &rust_type, 1);
            } else {
                system().register_variable(name, &self.state.data.c_type, &full_type);
            }
        }

        // Validate the generated Rust code keywords
        let rust_tokens: Vec<&str> = rust_code.split_whitespace().collect();
        let (rust_valid, _) = system().validate_rust_token_order(&rust_tokens);
        if !rust_valid {
            self.state.set_confidence(0.75); // Lower confidence for potentially invalid output
        } else {
            self.state.set_confidence(0.85);
        }
        self.state.stats.record_success(self.state.confidence, 0, 0, Some("variable"), "converted");
        self.stats();
        Some(rust_code)
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

impl VariableHandler {
    /// Convert an expression, handling type casts, enum variants, arrow operators, and Rust keyword identifiers
    fn convert_expression(&self, expr: &str) -> String {
        // First handle type casts like (struct Node *) or (Type *)
        let result = self.convert_type_casts(expr);
        // Convert arrow operators: ptr -> field -> (*ptr).field
        let result = Self::convert_arrow_operator(&result);
        // Then handle enum variants
        let result = self.convert_enum_variants_in_expr(&result);
        // Finally sanitize identifiers that are Rust keywords (str -> r#str)
        self.sanitize_identifiers_in_expr(&result)
    }

    /// Convert C arrow operator (ptr->field or ptr -> field) to Rust (*ptr).field
    fn convert_arrow_operator(expr: &str) -> String {
        let mut result = expr.to_string();
        
        // Handle spaced arrow: "ptr -> field" -> "(*ptr).field"
        while let Some(arrow_pos) = result.find(" -> ") {
            let before = &result[..arrow_pos];
            let after = &result[arrow_pos + 4..];
            
            // Find the identifier before the arrow (last word)
            let ptr_name = before.split_whitespace().last().unwrap_or("");
            let prefix = if before.len() > ptr_name.len() {
                &before[..before.len() - ptr_name.len()]
            } else {
                ""
            };
            
            // Find the field after the arrow (first word or until next operator)
            let field_end = after.find(|c: char| !c.is_alphanumeric() && c != '_').unwrap_or(after.len());
            let field = &after[..field_end];
            let suffix = &after[field_end..];
            
            result = format!("{}(*{}).{}{}", prefix, ptr_name, field, suffix);
        }
        
        // Handle non-spaced arrow: "ptr->field" -> "(*ptr).field"
        while let Some(arrow_pos) = result.find("->") {
            // Skip if already converted (would have parentheses)
            if arrow_pos > 0 && result.chars().nth(arrow_pos - 1) == Some(')') {
                break;
            }
            
            let before = &result[..arrow_pos];
            let after = &result[arrow_pos + 2..];
            
            // Find the identifier before the arrow
            let ptr_start = before.rfind(|c: char| !c.is_alphanumeric() && c != '_').map(|p| p + 1).unwrap_or(0);
            let ptr_name = &before[ptr_start..];
            let prefix = &before[..ptr_start];
            
            // Find the field after the arrow
            let field_end = after.find(|c: char| !c.is_alphanumeric() && c != '_').unwrap_or(after.len());
            let field = &after[..field_end];
            let suffix = &after[field_end..];
            
            result = format!("{}(*{}).{}{}", prefix, ptr_name, field, suffix);
        }
        
        result
    }

    /// Sanitize identifiers in an expression that are Rust keywords
    /// Skip sanitization inside type brackets <...> to avoid escaping type names
    fn sanitize_identifiers_in_expr(&self, expr: &str) -> String {
        let mut result = String::new();
        let mut current_word = String::new();
        let mut angle_bracket_depth = 0;
        
        for c in expr.chars() {
            if c == '<' {
                if !current_word.is_empty() {
                    // Sanitize the identifier before the bracket
                    if angle_bracket_depth == 0 {
                        result.push_str(&sanitize_rust_identifier(&current_word));
                    } else {
                        result.push_str(&current_word);
                    }
                    current_word.clear();
                }
                result.push(c);
                angle_bracket_depth += 1;
            } else if c == '>' {
                if !current_word.is_empty() {
                    // Don't sanitize type names inside angle brackets
                    result.push_str(&current_word);
                    current_word.clear();
                }
                result.push(c);
                if angle_bracket_depth > 0 {
                    angle_bracket_depth -= 1;
                }
            } else if c.is_alphanumeric() || c == '_' {
                current_word.push(c);
            } else {
                if !current_word.is_empty() {
                    // Only sanitize if not inside angle brackets
                    if angle_bracket_depth == 0 {
                        result.push_str(&sanitize_rust_identifier(&current_word));
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
            if angle_bracket_depth == 0 {
                result.push_str(&sanitize_rust_identifier(&current_word));
            } else {
                result.push_str(&current_word);
            }
        }
        
        result
    }

    /// Convert C-style type casts, stripping struct/enum/union keywords
    fn convert_type_casts(&self, expr: &str) -> String {
        let mut result = String::new();
        let chars: Vec<char> = expr.chars().collect();
        let mut i = 0;
        
        while i < chars.len() {
            if chars[i] == '(' {
                // Check if preceded by sizeof - if so, convert the type inside but keep sizeof
                let before: String = result.chars().rev().take(20).collect::<String>().chars().rev().collect();
                let is_sizeof = before.trim_end().ends_with("sizeof");
                
                // Look for closing paren
                let mut depth = 1;
                let mut j = i + 1;
                while j < chars.len() && depth > 0 {
                    if chars[j] == '(' { depth += 1; }
                    if chars[j] == ')' { depth -= 1; }
                    j += 1;
                }
                
                if depth == 0 {
                    let inner: String = chars[i+1..j-1].iter().collect();
                    let inner_trimmed = inner.trim();
                    
                    if is_sizeof {
                        // sizeof(struct Node) -> std::mem::size_of::<Node>()
                        // Use TypeConverter for C builtin types first
                        let mut type_converter = crate::db::convert::TypeConverter::new();
                        let type_name = if let Some(t) = type_converter.convert(inner_trimmed) {
                            // TypeConverter handles C builtin types (int -> i32, etc.)
                            t
                        } else if let Some(t) = system().lookup_type(inner_trimmed) {
                            // Then try registered custom types
                            t
                        } else if !crate::db::keyword::is_c_type_keyword(inner_trimmed) {
                            // Only check variable lookup if NOT a C type keyword
                            if let Some(var_type) = system().lookup_variable(inner_trimmed) {
                                var_type
                            } else {
                                self.convert_cast_type(inner_trimmed)
                            }
                        } else {
                            // Fallback to converting as type cast pattern
                            self.convert_cast_type(inner_trimmed)
                        };
                        // Replace "sizeof" at end of result with size_of call
                        let trimmed_result = result.trim_end();
                        if trimmed_result.ends_with("sizeof") {
                            result = trimmed_result[..trimmed_result.len()-6].to_string();
                            result.push_str(&format!("std::mem::size_of::<{}>()", type_name));
                        } else {
                            result.push_str(&format!("std::mem::size_of::<{}>()", type_name));
                        }
                        i = j;
                        continue;
                    }
                    
                    // Check if this looks like a type cast (ends with *, has type keywords)
                    if self.is_type_cast_pattern(inner_trimmed) {
                        // Convert the type cast - for Rust we remove C-style prefix casts
                        // The type is already declared in the variable declaration
                        let _converted = self.convert_cast_type(inner_trimmed);
                        i = j;
                        // Skip whitespace after the cast
                        while i < chars.len() && chars[i].is_whitespace() {
                            i += 1;
                        }
                        continue;
                    }
                }
                result.push(chars[i]);
            } else {
                result.push(chars[i]);
            }
            i += 1;
        }
        
        result
    }

    /// Check if a string looks like a C type cast pattern
    fn is_type_cast_pattern(&self, s: &str) -> bool {
        let s = s.trim();
        // Type cast patterns: "Type *", "struct Type *", "int", etc.
        if s.ends_with('*') || s.ends_with("* const") {
            return true;
        }
        // Check for struct/enum/union type
        if s.starts_with("struct ") || s.starts_with("enum ") || s.starts_with("union ") {
            return true;
        }
        // Check for basic C types
        let c_types = ["int", "char", "float", "double", "long", "short", "unsigned", 
                       "signed", "void", "size_t", "uint8_t", "uint16_t", "uint32_t", 
                       "uint64_t", "int8_t", "int16_t", "int32_t", "int64_t"];
        c_types.iter().any(|t| s.starts_with(t))
    }

    /// Convert a cast type, removing struct/enum/union and converting pointer syntax
    fn convert_cast_type(&self, s: &str) -> String {
        let mut t = s.to_string();
        // Remove struct/enum/union keywords
        t = t.replace("struct ", "").replace("enum ", "").replace("union ", "");
        // Convert pointer syntax
        if t.ends_with(" *") || t.ends_with("*") {
            let base = t.trim_end_matches(" *").trim_end_matches('*').trim();
            // Try to look up the type
            if let Some(rust_type) = system().lookup_type(base) {
                format!("*mut {}", rust_type)
            } else {
                format!("*mut {}", base)
            }
        } else {
            // Try to look up the type
            system().lookup_type(&t).unwrap_or(t)
        }
    }

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
        entry.set_attr("stage", self.state.stage().to_entry());
        entry.set_attr("confidence", Entry::f64(self.state.confidence));
        if let Some(ref output) = self.state.output {
            entry.set_attr("output", Entry::string(output));
        }
        entry.set_attr("variable_name", Entry::string(&self.state.data.name));
        entry.set_attr("c_type", Entry::string(&self.state.data.c_type));
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
        assert!(handler.state.data.is_const);

        let output = handler.convert();
        assert!(output.is_some());
        assert!(output.unwrap().starts_with("const"));
    }
}
