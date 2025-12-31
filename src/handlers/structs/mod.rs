//! Struct Handler
//!
//! Converts C struct definitions to Rust structs.
//! Handles nested structs by flattening them (Rust doesn't support nested struct definitions).

use crate::db::pattern::{Pattern, PatternRule};
use crate::db::token::Token;
use crate::db::web::{Build, Entry};
use crate::handlers::process::{ProcessorStage, Processor, ProcessorState, ProcessorStats};
use crate::handlers::validation::SequenceValidator;
use crate::system::system;

// ============================================================================
// Struct Handler Implementation
// ============================================================================

/// A single struct field
#[derive(Debug, Clone, Default)]
pub struct StructField {
    /// Field name
    pub name: String,
    /// Field type (C type)
    pub c_type: String,
    /// Field type (Rust type)
    pub rust_type: String,
    /// Is this a pointer field
    pub is_pointer: bool,
    /// Array dimensions if any
    pub array_dims: Vec<usize>,
    /// Bitfield width (if this is a bitfield)
    pub bitfield_width: Option<u8>,
    /// Default value for constructor (if any)
    pub default_value: Option<String>,
}

/// A struct definition (possibly extracted from nesting)
#[derive(Debug, Clone, Default)]
pub struct StructDef {
    /// Struct name
    pub name: String,
    /// Fields
    pub fields: Vec<StructField>,
    /// Is this an anonymous struct
    pub is_anonymous: bool,
    /// Original nesting depth (0 = top level)
    pub nesting_depth: usize,
}

impl Build for StructDef {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("Struct", &self.name);
        entry.set_attr("c_type", Entry::string(&self.name));
        entry.set_attr("rust_type", Entry::string(&self.name));
        entry.set_attr("is_anonymous", Entry::bool(self.is_anonymous));
        entry.set_attr("nesting_depth", Entry::usize(self.nesting_depth));
        entry.set_attr("field_count", Entry::usize(self.fields.len()));

        // Store field info
        let field_entries: Vec<Entry> = self
            .fields
            .iter()
            .map(|f| {
                let mut fe = Entry::node("Field", &f.name);
                fe.set_attr("c_type", Entry::string(&f.c_type));
                fe.set_attr("rust_type", Entry::string(&f.rust_type));
                fe.set_attr("is_pointer", Entry::bool(f.is_pointer));
                fe
            })
            .collect();
        entry.set_attr("fields", Entry::vec(field_entries));

        entry
    }

    fn kind(&self) -> &str {
        "Struct"
    }
    fn name(&self) -> Option<&str> {
        Some(&self.name)
    }
    fn category(&self) -> Option<&str> {
        Some("type")
    }
}

/// Extracted struct information
#[derive(Debug, Clone, Default)]
pub struct StructData {
    /// Main struct definition
    pub main_struct: StructDef,
    /// Nested structs that were extracted (flattened)
    pub nested_structs: Vec<StructDef>,
    /// Typedef alias if any
    pub typedef_alias: Option<String>,
    /// Is this a forward declaration (struct Name;)
    pub is_forward_declaration: bool,
}

impl Build for StructData {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("StructData", &self.main_struct.name);
        entry.set_attr("struct_name", Entry::string(&self.main_struct.name));
        entry.set_attr("field_count", Entry::usize(self.main_struct.fields.len()));
        entry.set_attr("nested_count", Entry::usize(self.nested_structs.len()));
        entry.set_attr("is_forward_declaration", Entry::bool(self.is_forward_declaration));
        if let Some(ref alias) = self.typedef_alias {
            entry.set_attr("typedef_alias", Entry::string(alias));
        }
        entry
    }

    fn kind(&self) -> &str {
        "StructData"
    }

    fn name(&self) -> Option<&str> {
        Some(&self.main_struct.name)
    }

    fn category(&self) -> Option<&str> {
        Some("struct")
    }
}

/// Handler for C struct definitions
#[derive(Debug)]
pub struct StructHandler {
    /// Generic processor state
    state: ProcessorState<StructData>,
}

impl StructHandler {
    pub fn new() -> Self {
        Self { state: ProcessorState::new("StructHandler") }
    }
    /// Convert struct initialization from tokens (called from DefinitionHandler)
    /// Converts C-style { val1, val2 } to Rust StructName { field1: val1, field2: val2 }
    pub fn convert_init_from_tokens(struct_name: &str, tokens: &[&str]) -> String {
        // Extract values between braces
        let mut values: Vec<String> = Vec::new();
        let mut depth = 0;
        let mut current: Vec<String> = Vec::new();

        for token in tokens {
            match *token {
                "{" => {
                    depth += 1;
                    if depth > 1 {
                        current.push(token.to_string());
                    }
                }
                "}" => {
                    depth -= 1;
                    if depth > 0 {
                        current.push(token.to_string());
                    } else if !current.is_empty() {
                        values.push(current.join(" "));
                        current.clear();
                    }
                }
                "," if depth == 1 => {
                    if !current.is_empty() {
                        values.push(current.join(" "));
                        current.clear();
                    }
                }
                _ => current.push(Self::convert_literal(token)),
            }
        }

        let sys = crate::system::system();
        let field_names = sys.lookup_struct_fields(struct_name)
            .unwrap_or_else(|| (0..values.len()).map(|i| format!("field{}", i)).collect());
        let field_types = sys.lookup_struct_field_types(struct_name)
            .unwrap_or_else(|| vec![String::new(); values.len()]);

        let fields: Vec<String> = values.iter()
            .zip(field_names.iter())
            .zip(field_types.iter())
            .map(|((value, field), field_type)| {
                let trimmed = value.trim();
                if trimmed.starts_with('{') && field_type.contains(struct_name) {
                    let nested_tokens: Vec<&str> = trimmed.split_whitespace().collect();
                    let refs: Vec<&str> = nested_tokens.iter().map(|s| s.clone()).collect();
                    format!("{}: {}", field, Self::convert_init_from_tokens(field_type, &refs))
                } else {
                    format!("{}: {}", field, trimmed)
                }
            })
            .collect();

        format!("{} {{ {} }}", struct_name, fields.join(", "))
    }

    /// Convert C literals to Rust literals (e.g., float suffixes)
    fn convert_literal(token: &str) -> String {
        let token = token.trim();
        
        // Handle C float suffix (e.g., 1.0f -> 1.0_f32)
        if let Some(stripped) = token.strip_suffix('f').or_else(|| token.strip_suffix('F')) {
            if stripped.parse::<f64>().is_ok() {
                return format!("{}_f32", stripped);
            }
        }
        
        // Check if it's an unsuffixed float literal
        if token.contains('.') && !token.ends_with("f32") && !token.ends_with("f64") {
            if token.parse::<f64>().is_ok() {
                return format!("{}_f64", token);
            }
        }
        
        // Handle NULL -> std::ptr::null() or std::ptr::null_mut()
        if token == "NULL" {
            return "std::ptr::null_mut()".to_string();
        }
        
        token.to_string()
    }
}

impl Default for StructHandler {
    fn default() -> Self {
        Self::new()
    }
}

impl Processor for StructHandler {
    fn supported_patterns(&self) -> &[&str] {
        &["struct_forward_declaration", "struct_definition", "struct_declaration", "typedef_struct"]
    }

    fn stats(&self) -> &ProcessorStats {
        system().process_stats(&self.state.stats);
        &self.state.stats
    }

    fn patterns(&self) -> Vec<(Pattern, Pattern)> {
        vec![
            // Forward declaration pattern: struct Name; (high priority, exactly 3 tokens)
            (
                Pattern::definition(
                    399,
                    "struct_forward_declaration",
                    vec![
                        PatternRule::exact("struct"),
                        PatternRule::identifier(),
                        PatternRule::exact(";"),
                    ],
                )
                .with_category("struct")
                .with_priority(150)  // Higher than variable handler (85)
                .with_min_tokens(3)
                .with_description("C struct forward declaration"),
                Pattern::definition(
                    399,
                    "extract_struct_forward_declaration",
                    vec![
                        PatternRule::exact("struct"),
                        PatternRule::identifier().with_extract(|rule, ctx| {
                            let token = ctx.current_token.clone();
                            if ctx.did_match {
                                ctx.set_value("tag_name", &token);
                            }
                            rule.clone()
                        }),
                        PatternRule::exact(";"),
                    ],
                )
                .with_category("struct")
                .with_priority(150)
                .with_min_tokens(3)
                .with_description("C struct forward declaration"),
            ),
            (
                Pattern::definition(
                    400,
                    "struct_definition",
                    vec![
                        PatternRule::exact("struct"),
                        PatternRule::identifier().optional(),
                        PatternRule::exact("{"),
                        PatternRule::any_except("}").repeat(-1),
                        PatternRule::exact("}"),
                        PatternRule::identifier().optional(),
                        PatternRule::exact(";"),
                    ],
                )
                .with_category("struct")
                .with_priority(135)
                .with_min_tokens(4)
                .with_description("C struct definition"),
                Pattern::definition(
                    400,
                    "extract_struct_definition",
                    vec![
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
                        PatternRule::any_except("}")
                            .repeat(-1)
                            .with_extract(|rule, ctx| {
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
                .with_category("struct")
                .with_priority(135)
                .with_min_tokens(4)
                .with_description("C struct definition"),
            ),
            (
                Pattern::definition(
                    401,
                    "typedef_struct",
                    vec![
                        PatternRule::exact("typedef"),
                        PatternRule::exact("struct"),
                        PatternRule::identifier().optional(),
                        PatternRule::exact("{"),
                        PatternRule::any_except("}").repeat(-1),
                        PatternRule::exact("}"),
                        PatternRule::identifier(),
                        PatternRule::exact(";"),
                    ],
                )
                .with_category("struct")
                .with_priority(140)
                .with_min_tokens(6)
                .with_description("Typedef struct definition"),
                Pattern::definition(
                    401,
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
                        PatternRule::any_except("}")
                            .repeat(-1)
                            .with_extract(|rule, ctx| {
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
                .with_category("struct")
                .with_priority(140)
                .with_min_tokens(6)
                .with_description("Typedef struct definition"),
            ),
        ]
    }

    fn validate(&mut self, tokens: &[Token]) -> bool {
        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        // Check for forward declaration: "struct Name ;" (3 tokens, no body)
        if token_strs.len() == 3 && token_strs[0] == "struct" && token_strs[2] == ";" {
            // This is a forward declaration - mark it and register the type
            self.state.data.is_forward_declaration = true;
            self.state.data.main_struct.name = token_strs[1].clone();
            self.state.set_confidence(0.9);
            return true;
        }

        // Pattern matching handles min_tokens and "struct" keyword
        let mut best_confidence = 0.0;
        for (pattern, _) in self.patterns() {
            if let Some(confidence) = pattern.matches_tokens(&token_strs) {
                if confidence > best_confidence {
                    best_confidence = confidence;
                }
            }
        }

        if best_confidence == 0.0 {
            self.state.set_error("Not a struct definition".to_string());
            return false;
        }

        // Additional checks: reject function returns and variable inits
        let brace_pos = token_strs.iter().position(|t| t == "{");
        let paren_pos = token_strs.iter().position(|t| t == "(");
        let equals_pos = token_strs.iter().position(|t| t == "=");

        // If ( before {, this is a function returning struct
        if let (Some(pp), Some(br)) = (paren_pos, brace_pos) {
            if pp < br {
                self.state.set_error("Function returning struct, not struct definition".to_string());
                return false;
            }
        }

        // If = before {, this is variable initialization
        if let (Some(eq), Some(br)) = (equals_pos, brace_pos) {
            if eq < br {
                self.state.set_error("Struct variable initialization, not definition".to_string());
                return false;
            }
        }

        // Use keyword validation to adjust confidence
        let validator = SequenceValidator::new();
        let validation_result = validator.validate_type_declaration(tokens);
        self.state.set_confidence(validation_result.adjust_confidence(best_confidence));

        true
    }

    fn extract(&mut self, tokens: &[Token]) -> bool {
        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        // Try extraction patterns in order
        for (_, extract_pattern) in self.patterns() {
            if let Some(ctx) = extract_pattern.run_extraction(&token_strs) {
                // Get tag name from pattern extraction
                if let Some(tag_name) = ctx.value("tag_name") {
                    self.state.data.main_struct.name = tag_name.to_string();
                }

                // Get type_name (typedef alias) from pattern extraction
                if let Some(type_name) = ctx.value("type_name") {
                    self.state.data.typedef_alias = Some(type_name.to_string());
                    if self.state.data.main_struct.name.is_empty() {
                        self.state.data.main_struct.name = type_name.to_string();
                    }
                }

                // Get var_name (variable instance) from pattern extraction
                if let Some(var_name) = ctx.value("var_name") {
                    if self.state.data.main_struct.name.is_empty() {
                        self.state.data.main_struct.name = var_name.to_string();
                    }
                }

                // Get fields from pattern extraction
                if let Some(fields) = ctx.list("fields") {
                    self.parse_struct_body(fields, 0);
                }

                // If still no name, generate one
                if self.state.data.main_struct.name.is_empty() {
                    self.state.data.main_struct.name = "AnonymousStruct".to_string();
                    self.state.data.main_struct.is_anonymous = true;
                }

                return true;
            }
        }

        // Fallback to manual parsing
        let mut idx = 0;
        let is_typedef = token_strs.get(0) == Some(&"typedef".to_string());

        if is_typedef {
            idx += 1;
        }

        // Skip "struct"
        if token_strs.get(idx) != Some(&"struct".to_string()) {
            self.state.set_error("Expected 'struct' keyword".to_string());
            return false;
        }
        idx += 1;

        // Get struct name (optional before brace)
        let brace_pos = match token_strs.iter().position(|t| t == "{") {
            Some(p) => p,
            None => {
                self.state.set_error("No opening brace found".to_string());
                return false;
            }
        };

        // Name is between struct and {
        if idx < brace_pos {
            self.state.data.main_struct.name = token_strs[idx].clone();
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

        // Parse fields
        self.parse_struct_body(&body_tokens, 0);

        // Check for typedef alias after closing brace
        if is_typedef && close_brace + 1 < token_strs.len() {
            let alias = &token_strs[close_brace + 1];
            if alias != ";" {
                self.state.data.typedef_alias = Some(alias.clone());
                if self.state.data.main_struct.name.is_empty() {
                    self.state.data.main_struct.name = alias.clone();
                }
            }
        }

        // If still no name, generate one
        if self.state.data.main_struct.name.is_empty() {
            self.state.data.main_struct.name = "AnonymousStruct".to_string();
            self.state.data.main_struct.is_anonymous = true;
        }

        true
    }

    fn convert(&mut self) -> Option<String> {
        // Handle forward declarations: register type but don't output anything
        // The type registration allows pointer types like *mut Node to work before struct is defined
        if self.state.data.is_forward_declaration {
            let name = &self.state.data.main_struct.name;
            // Register the struct type so it can be used in pointer types
            crate::system::system().register_struct(name, name);
            // Return empty string - forward declarations don't need output in Rust
            self.state.set_output(String::new());
            return self.state.output.clone();
        }

        let mut output = String::new();

        // Register nested structs first
        for nested in &self.state.data.nested_structs {
            crate::system::system().register_struct(&nested.name, &nested.name);
            // Register struct constructor pattern for this struct
            self.register_struct_constructor(&nested.name, &nested.fields);
        }

        // Register the main struct
        crate::system::system()
            .register_struct(&self.state.data.main_struct.name, &self.state.data.main_struct.name);
        // Register struct constructor pattern for main struct
        self.register_struct_constructor(
            &self.state.data.main_struct.name,
            &self.state.data.main_struct.fields,
        );

        // Register typedef alias if present
        if let Some(ref alias) = self.state.data.typedef_alias {
            if alias != &self.state.data.main_struct.name {
                crate::system::system().register_typedef(
                    alias,
                    alias,
                    &self.state.data.main_struct.name,
                );
            }
        }

        // First, output any nested structs (they must be defined before use)
        for nested in &self.state.data.nested_structs {
            output.push_str(&self.convert_struct_def(nested));
            output.push_str("\n\n");
        }

        // Then output the main struct
        output.push_str(&self.convert_struct_def(&self.state.data.main_struct));

        // Add type alias if typedef was used with a different name
        if let Some(ref alias) = self.state.data.typedef_alias {
            if alias != &self.state.data.main_struct.name {
                output.push_str(&format!(
                    "\npub type {} = {};",
                    alias, self.state.data.main_struct.name
                ));
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

impl StructHandler {
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

    /// Parse struct body, extracting fields and nested structs
    fn parse_struct_body(&mut self, tokens: &[String], nesting_depth: usize) {
        let mut i = 0;

        while i < tokens.len() {
            // Check for nested struct/union/enum
            if tokens[i] == "struct" || tokens[i] == "union" || tokens[i] == "enum" {
                let nested_kind = tokens[i].clone();
                i += 1;

                // Check if this is a nested definition (has brace)
                let has_brace = tokens[i..].iter().take(3).any(|t| t == "{");

                if has_brace {
                    // Extract the nested definition
                    let nested_name = if i < tokens.len() && tokens[i] != "{" {
                        let name = tokens[i].clone();
                        i += 1;
                        name
                    } else {
                        format!(
                            "Nested{}_{}",
                            nested_kind.to_uppercase(),
                            self.state.data.nested_structs.len()
                        )
                    };

                    // Find the brace
                    while i < tokens.len() && tokens[i] != "{" {
                        i += 1;
                    }

                    if i >= tokens.len() {
                        break;
                    }

                    let brace_start = i;
                    let brace_end = match self.find_matching_brace(tokens, brace_start) {
                        Some(e) => e,
                        None => break,
                    };

                    // Create nested struct definition
                    let mut nested_def = StructDef {
                        name: nested_name.clone(),
                        fields: Vec::new(),
                        is_anonymous: false,
                        nesting_depth: nesting_depth + 1,
                    };

                    // Parse nested body recursively
                    let nested_body = &tokens[brace_start + 1..brace_end];

                    // Temporarily swap to parse into nested
                    let main_struct = std::mem::take(&mut self.state.data.main_struct);
                    self.state.data.main_struct = nested_def;
                    self.parse_struct_body(&nested_body.to_vec(), nesting_depth + 1);
                    nested_def = std::mem::take(&mut self.state.data.main_struct);
                    self.state.data.main_struct = main_struct;

                    self.state.data.nested_structs.push(nested_def);

                    i = brace_end + 1;

                    // Check for field name after nested definition
                    if i < tokens.len() && tokens[i] != ";" && tokens[i] != "}" {
                        let field_name = tokens[i].clone();
                        let field = StructField {
                            name: field_name,
                            c_type: nested_name.clone(),
                            rust_type: nested_name,
                            is_pointer: false,
                            array_dims: Vec::new(),
                            bitfield_width: None,
                            default_value: None,
                        };
                        self.state.data.main_struct.fields.push(field);
                        i += 1;
                    }

                    // Skip semicolon
                    if i < tokens.len() && tokens[i] == ";" {
                        i += 1;
                    }
                    continue;
                }
            }

            // Regular field: type [*] name [array] ;
            let field = self.parse_field(&tokens[i..]);
            if let Some((f, consumed)) = field {
                self.state.data.main_struct.fields.push(f);
                i += consumed;
            } else {
                i += 1;
            }
        }
    }

    /// Parse a single field declaration
    fn parse_field(&self, tokens: &[String]) -> Option<(StructField, usize)> {
        if tokens.is_empty() {
            return None;
        }

        // Find semicolon to determine field extent
        let semi_pos = tokens.iter().position(|t| t == ";")?;
        if semi_pos < 2 {
            return None;
        }

        let field_tokens = &tokens[..semi_pos];

        // Check for function pointer: return_type (*name)(params)
        // Pattern: has (*identifier) followed by (
        if let Some(func_ptr) = self.parse_function_pointer_field(field_tokens) {
            return Some((func_ptr, semi_pos + 1));
        }

        // Check for initialization value: type name = value
        let equals_pos = field_tokens.iter().position(|t| t == "=");
        let mut default_value: Option<String> = None;
        let init_end = if let Some(eq) = equals_pos {
            // Capture everything after = as the initial value
            if eq + 1 < field_tokens.len() {
                let value_tokens: Vec<&str> = field_tokens[eq + 1..]
                    .iter()
                    .map(|s| s.as_str())
                    .collect();
                default_value = Some(value_tokens.join(" "));
            }
            eq // Field declaration ends at equals sign
        } else {
            field_tokens.len()
        };

        // Check for bitfield: type name : width (within the non-init portion)
        let colon_pos = field_tokens[..init_end].iter().position(|t| t == ":");
        let mut bitfield_width: Option<u8> = None;
        let effective_end = if let Some(cp) = colon_pos {
            // Parse bitfield width
            if cp + 1 < init_end {
                if let Ok(width) = field_tokens[cp + 1].parse::<u8>() {
                    bitfield_width = Some(width);
                }
            }
            cp // Field name is before the colon
        } else {
            init_end
        };

        // Check for pointer
        let is_pointer = field_tokens[..effective_end].iter().any(|t| t == "*");

        // Check for array
        let bracket_pos = field_tokens[..effective_end].iter().position(|t| t == "[");
        let mut array_dims = Vec::new();

        if let Some(bp) = bracket_pos {
            // Parse array dimensions
            let mut j = bp;
            while j < effective_end {
                if field_tokens[j] == "[" {
                    j += 1;
                    if j < effective_end && field_tokens[j] != "]" {
                        if let Ok(dim) = field_tokens[j].parse::<usize>() {
                            array_dims.push(dim);
                        }
                        j += 1;
                    }
                } else {
                    j += 1;
                }
            }
        }

        // Name is before [ or : or at end
        let name_pos = bracket_pos.unwrap_or(effective_end) - 1;
        if name_pos == 0 {
            return None;
        }

        let name = field_tokens[name_pos].clone();

        // Type is everything before name (excluding *)
        let type_tokens: Vec<&str> = field_tokens[..name_pos]
            .iter()
            .filter(|t| *t != "*")
            .map(|s| s.as_str())
            .collect();

        let c_type = type_tokens.join(" ");
        let rust_type = system().lookup_type(&c_type).unwrap_or_else(|| c_type.clone());

        Some((
            StructField {
                name,
                c_type,
                rust_type,
                is_pointer,
                array_dims,
                bitfield_width,
                default_value,
            },
            semi_pos + 1,
        ))
    }

    /// Parse function pointer field: return_type (*name)(param_types)
    fn parse_function_pointer_field(&self, tokens: &[String]) -> Option<StructField> {
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
        // First paren should be followed by *
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

        // Convert params to Rust types
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

        // Build Rust function pointer type: fn(params) -> return
        let rust_type = if rust_return == "()" || rust_return == "void" {
            format!("Option<fn({})>", rust_params.join(", "))
        } else {
            format!("Option<fn({}) -> {}>", rust_params.join(", "), rust_return)
        };

        Some(StructField {
            name,
            c_type: format!("{}(*)({})", return_type, param_tokens.join(" ")),
            rust_type,
            is_pointer: false,
            array_dims: Vec::new(),
            bitfield_width: None,
            default_value: None,
        })
    }

    /// Register a struct constructor pattern for later use in struct initialization
    fn register_struct_constructor(&self, struct_name: &str, fields: &[StructField]) {
        // Store field names and types in the system for lookup during struct initialization
        let field_names: Vec<String> = fields.iter().map(|f| f.name.clone()).collect();
        // Compute field types matching convert_struct_def logic
        let field_types: Vec<String> = fields.iter().map(|f| {
            if f.is_pointer {
                format!("*mut {}", f.rust_type)
            } else if !f.array_dims.is_empty() {
                // Special case: char arrays are string buffers, convert to &str
                let is_char_array = f.c_type == "char" || f.c_type == "signed char" 
                    || f.c_type == "unsigned char";
                if is_char_array {
                    "&'static str".to_string()
                } else {
                    let mut arr_type = f.rust_type.clone();
                    for dim in f.array_dims.iter().rev() {
                        arr_type = format!("[{}; {}]", arr_type, dim);
                    }
                    arr_type
                }
            } else {
                f.rust_type.clone()
            }
        }).collect();

        // Register a pattern for struct initialization: StructName { field1, field2, ... }
        // This is stored in the database for use by other handlers
        use crate::db::node::{node, NodeExt};
        use crate::db::web::Entry;

        let field_entries: Vec<Entry> = field_names.iter().map(|f| Entry::string(f)).collect();

        let type_entries: Vec<Entry> = field_types.iter().map(|t| Entry::string(t)).collect();

        let entry = node("StructConstructor", struct_name)
            .with_attr("fields", Entry::vec(field_entries))
            .with_attr("field_types", Entry::vec(type_entries))
            .with_attr("field_count", Entry::usize(fields.len()));

        crate::system::system().db_mut().add_entry(entry);
    }

    /// Convert a struct definition to Rust code
    fn convert_struct_def(&self, def: &StructDef) -> String {
        let mut output = String::new();

        // Check if struct has bitfields
        let has_bitfields = def.fields.iter().any(|f| f.bitfield_width.is_some());

        output.push_str("#[repr(C)]\n");
        output.push_str("#[derive(Debug, Clone)]\n");
        output.push_str(&format!("pub struct {} {{\n", def.name));

        for field in &def.fields {
            let rust_type = if field.is_pointer {
                format!("*mut {}", field.rust_type)
            } else if !field.array_dims.is_empty() {
                // Special case: char arrays are string buffers, convert to &str
                let is_char_array = field.c_type == "char" || field.c_type == "signed char" 
                    || field.c_type == "unsigned char";
                if is_char_array {
                    "&'static str".to_string()
                } else {
                    // Convert array dimensions
                    let mut arr_type = field.rust_type.clone();
                    for dim in field.array_dims.iter().rev() {
                        arr_type = format!("[{}; {}]", arr_type, dim);
                    }
                    arr_type
                }
            } else {
                field.rust_type.clone()
            };

            // Add bitfield comment if applicable
            if let Some(width) = field.bitfield_width {
                output.push_str(&format!("    /// Bitfield: {} bits\n", width));
            }
            output.push_str(&format!("    pub {}: {},\n", field.name, rust_type));
        }

        output.push_str("}\n");

        // Generate new() constructor if struct has bitfields (need default initialization)
        if has_bitfields {
            output.push_str(&self.generate_constructor(def));
        }

        output
    }

    /// Generate constructors for the struct (new() with defaults and new_with() with parameters)
    fn generate_constructor(&self, def: &StructDef) -> String {
        let mut output = String::new();
        
        output.push_str(&format!("\nimpl {} {{\n", def.name));
        
        // Generate new() with default values
        output.push_str("    /// Create a new instance with default values\n");
        output.push_str("    pub fn new() -> Self {\n");
        output.push_str("        Self {\n");

        for field in &def.fields {
            let default_val = if let Some(ref val) = field.default_value {
                val.clone()
            } else if field.is_pointer {
                "std::ptr::null_mut()".to_string()
            } else if !field.array_dims.is_empty() {
                // Check if char array
                let is_char_array = field.c_type == "char" || field.c_type == "signed char" 
                    || field.c_type == "unsigned char";
                if is_char_array {
                    "\"\"".to_string()
                } else {
                    // Default array
                    let inner_default = self.get_default_for_type(&field.rust_type);
                    format!("[{}; {}]", inner_default, field.array_dims.first().unwrap_or(&0))
                }
            } else if field.bitfield_width.is_some() {
                // Bitfields default to 0
                "0".to_string()
            } else {
                self.get_default_for_type(&field.rust_type)
            };
            output.push_str(&format!("            {}: {},\n", field.name, default_val));
        }

        output.push_str("        }\n");
        output.push_str("    }\n\n");

        // Generate new_with() that takes parameters for all fields
        output.push_str("    /// Create a new instance with specified values\n");
        let params: Vec<String> = def.fields.iter().map(|f| {
            let rust_type = self.get_field_rust_type(f);
            format!("{}: {}", f.name, rust_type)
        }).collect();
        output.push_str(&format!("    pub fn new_with({}) -> Self {{\n", params.join(", ")));
        output.push_str("        Self {\n");
        for field in &def.fields {
            output.push_str(&format!("            {},\n", field.name));
        }
        output.push_str("        }\n");
        output.push_str("    }\n");
        
        output.push_str("}\n");

        output
    }

    /// Get the Rust type for a field (for constructor parameter)
    fn get_field_rust_type(&self, field: &StructField) -> String {
        if field.is_pointer {
            format!("*mut {}", field.rust_type)
        } else if !field.array_dims.is_empty() {
            let is_char_array = field.c_type == "char" || field.c_type == "signed char" 
                || field.c_type == "unsigned char";
            if is_char_array {
                "&'static str".to_string()
            } else {
                let mut arr_type = field.rust_type.clone();
                for dim in field.array_dims.iter().rev() {
                    arr_type = format!("[{}; {}]", arr_type, dim);
                }
                arr_type
            }
        } else {
            field.rust_type.clone()
        }
    }

    /// Get default value for a Rust type
    fn get_default_for_type(&self, rust_type: &str) -> String {
        match rust_type {
            "i8" | "i16" | "i32" | "i64" | "i128" | "isize" => "0".to_string(),
            "u8" | "u16" | "u32" | "u64" | "u128" | "usize" => "0".to_string(),
            "f32" | "f64" => "0.0".to_string(),
            "bool" => "false".to_string(),
            "char" => "'\\0'".to_string(),
            t if t.starts_with("*") => "std::ptr::null_mut()".to_string(),
            t if t.starts_with("Option<") => "None".to_string(),
            _ => format!("{}::new()", rust_type), // Assume struct with new()
        }
    }
}

impl Build for StructHandler {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("Handler", "StructHandler");
        entry.set_attr("stage", self.state.stage().to_entry());
        entry.set_attr("confidence", Entry::f64(self.state.confidence));
        entry.set_attr("struct_name", Entry::string(&self.state.data.main_struct.name));
        entry.set_attr(
            "field_count",
            Entry::i64(self.state.data.main_struct.fields.len() as i64),
        );
        entry.set_attr(
            "nested_count",
            Entry::i64(self.state.data.nested_structs.len() as i64),
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
        Some("StructHandler")
    }
    fn category(&self) -> Option<&str> {
        Some("struct")
    }
}
