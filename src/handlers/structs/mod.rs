//! Struct Handler
//!
//! Converts C struct definitions to Rust structs.
//! Handles nested structs by flattening them (Rust doesn't support nested struct definitions).

use crate::db::convert::TypeConverter;
use crate::db::pattern::{Pattern, PatternRule};
use crate::db::token::Token;
use crate::db::web::{Build, Entry};
use crate::handlers::process::{ProcessStage, Processor};
use crate::handlers::validation::SequenceValidator;
use crate::{system};
use std::ops::Range;

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
}

/// Handler for C struct definitions
#[derive(Debug)]
pub struct StructHandler {
    name: String,
    stage: ProcessStage,
    confidence: f64,
    error: Option<String>,
    output: Option<String>,
    data: StructData,
    range: Range<usize>,
    input_tokens: Vec<String>,
}

impl StructHandler {
    pub fn new() -> Self {
        Self {
            name: "struct".to_string(),
            stage: ProcessStage::Pending,
            confidence: 0.0,
            error: None,
            output: None,
            data: StructData::default(),
            range: 0..0,
            input_tokens: Vec::new(),
        }
    }

    /// Convert struct initialization from tokens (called from DefinitionHandler)
    /// Converts C-style { val1, val2 } to Rust StructName { field1: val1, field2: val2 }
    pub fn convert_init_from_tokens(struct_name: &str, tokens: &[&str]) -> String {
        Self::convert_struct_init_recursive(struct_name, tokens)
    }

    /// Recursive struct initialization conversion
    fn convert_struct_init_recursive(struct_name: &str, tokens: &[&str]) -> String {
        // Extract values between braces, keeping track of nested braces
        let mut values: Vec<(String, bool)> = Vec::new(); // (value, is_nested_struct)
        let mut depth = 0;
        let mut current: Vec<String> = Vec::new();
        let mut current_is_nested = false;

        for token in tokens {
            match *token {
                "{" => {
                    if depth > 0 {
                        current.push(token.to_string());
                        current_is_nested = true;
                    }
                    depth += 1;
                }
                "}" => {
                    depth -= 1;
                    if depth > 0 {
                        current.push(token.to_string());
                    } else if depth == 0 && !current.is_empty() {
                        values.push((current.join(" "), current_is_nested));
                        current.clear();
                        current_is_nested = false;
                    }
                }
                "," => {
                    if depth == 1 {
                        if !current.is_empty() {
                            values.push((current.join(" "), current_is_nested));
                            current.clear();
                            current_is_nested = false;
                        }
                    } else {
                        current.push(token.to_string());
                    }
                }
                _ => {
                    current.push(Self::convert_literal(token));
                }
            }
        }

        // Look up struct fields and types from registered struct constructors
        let field_names = crate::system::system()
            .lookup_struct_fields(struct_name)
            .unwrap_or_else(|| (0..values.len()).map(|i| format!("field{}", i)).collect());

        let field_types = crate::system::system()
            .lookup_struct_field_types(struct_name)
            .unwrap_or_else(|| (0..values.len()).map(|_| String::new()).collect());

        // Build Rust struct initialization with field names
        let fields: Vec<String> = values
            .iter()
            .zip(field_names.iter())
            .zip(field_types.iter())
            .map(|(((v, is_nested), f), t)| {
                let value = v.trim();
                if *is_nested && !t.is_empty() {
                    // This is a nested struct initialization - recursively convert
                    // The value already contains the braces, so just split and convert
                    let nested_tokens_owned: Vec<String> =
                        value.split_whitespace().map(|s| s.to_string()).collect();

                    // If it doesn't start with brace, wrap it
                    let wrapped: Vec<String>;
                    let nested_refs: Vec<&str> =
                        if nested_tokens_owned.first().map(|s| s.as_str()) == Some("{") {
                            nested_tokens_owned.iter().map(|s| s.as_str()).collect()
                        } else {
                            wrapped = std::iter::once("{".to_string())
                                .chain(nested_tokens_owned.iter().cloned())
                                .chain(std::iter::once("}".to_string()))
                                .collect();
                            wrapped.iter().map(|s| s.as_str()).collect()
                        };

                    let nested_init = Self::convert_struct_init_recursive(t, &nested_refs);
                    format!("{}: {}", f, nested_init)
                } else {
                    format!("{}: {}", f, value)
                }
            })
            .collect();

        format!("{} {{ {} }}", struct_name, fields.join(", "))
    }

    /// Convert C literals to Rust literals (e.g., float suffixes)
    fn convert_literal(token: &str) -> String {
        let token = token.trim();
        // Check if it's a float literal (contains . but not already suffixed)
        if token.contains('.') && !token.ends_with("f32") && !token.ends_with("f64") {
            // Check if it looks like a number
            let is_float = token
                .chars()
                .all(|c| c.is_ascii_digit() || c == '.' || c == '-' || c == '+');
            if is_float {
                return format!("{}f64", token);
            }
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
    fn name(&self) -> &str {
        &self.name
    }

    fn supported_patterns(&self) -> &[&str] {
        &["struct_definition", "struct_declaration", "typedef_struct"]
    }

    fn patterns(&self) -> Vec<(Pattern, Pattern)> {
        vec![
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
        if tokens.len() < 4 {
            self.error = Some("Too few tokens for struct".to_string());
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

        // Check for struct keyword
        let first = tokens[0].to_string();
        let has_struct = first == "struct"
            || (first == "typedef" && tokens.len() > 1 && tokens[1].to_string() == "struct");

        if !has_struct {
            self.error = Some("Not a struct definition".to_string());
            return false;
        }

        // Must have braces for definition (not just for initialization)
        let brace_pos = self.input_tokens.iter().position(|t| t == "{");
        let paren_pos = self.input_tokens.iter().position(|t| t == "(");
        let equals_pos = self.input_tokens.iter().position(|t| t == "=");

        // If there's a ( before {, this is a function returning a struct, not a struct definition
        if let (Some(pp), Some(br)) = (paren_pos, brace_pos) {
            if pp < br {
                self.error = Some("Function returning struct, not struct definition".to_string());
                return false;
            }
        }

        // If there's an = before {, this is a variable initialization, not a struct definition
        if let (Some(eq), Some(br)) = (equals_pos, brace_pos) {
            if eq < br {
                self.error = Some("Struct variable initialization, not definition".to_string());
                return false;
            }
        }

        if brace_pos.is_none() {
            self.error = Some("Struct must have body".to_string());
            return false;
        }

        // Use keyword validation to check sequence
        let validator = SequenceValidator::new();
        let validation_result = validator.validate_type_declaration(tokens);

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

        // Try extraction patterns in order
        for (_, extract_pattern) in self.patterns() {
            if let Some(ctx) = extract_pattern.run_extraction(&token_strs) {
                // Get tag name from pattern extraction
                if let Some(tag_name) = ctx.value("tag_name") {
                    self.data.main_struct.name = tag_name.to_string();
                }

                // Get type_name (typedef alias) from pattern extraction
                if let Some(type_name) = ctx.value("type_name") {
                    self.data.typedef_alias = Some(type_name.to_string());
                    if self.data.main_struct.name.is_empty() {
                        self.data.main_struct.name = type_name.to_string();
                    }
                }

                // Get var_name (variable instance) from pattern extraction
                if let Some(var_name) = ctx.value("var_name") {
                    if self.data.main_struct.name.is_empty() {
                        self.data.main_struct.name = var_name.to_string();
                    }
                }

                // Get fields from pattern extraction
                if let Some(fields) = ctx.list("fields") {
                    self.parse_struct_body(fields, 0);
                }

                // If still no name, generate one
                if self.data.main_struct.name.is_empty() {
                    self.data.main_struct.name = format!("AnonymousStruct{}", self.range.start);
                    self.data.main_struct.is_anonymous = true;
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
            self.error = Some("Expected 'struct' keyword".to_string());
            return false;
        }
        idx += 1;

        // Get struct name (optional before brace)
        let brace_pos = match token_strs.iter().position(|t| t == "{") {
            Some(p) => p,
            None => {
                self.error = Some("No opening brace found".to_string());
                return false;
            }
        };

        // Name is between struct and {
        if idx < brace_pos {
            self.data.main_struct.name = token_strs[idx].clone();
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

        // Parse fields
        self.parse_struct_body(&body_tokens, 0);

        // Check for typedef alias after closing brace
        if is_typedef && close_brace + 1 < token_strs.len() {
            let alias = &token_strs[close_brace + 1];
            if alias != ";" {
                self.data.typedef_alias = Some(alias.clone());
                if self.data.main_struct.name.is_empty() {
                    self.data.main_struct.name = alias.clone();
                }
            }
        }

        // If still no name, generate one
        if self.data.main_struct.name.is_empty() {
            self.data.main_struct.name = format!("AnonymousStruct{}", self.range.start);
            self.data.main_struct.is_anonymous = true;
        }

        true
    }

    fn convert(&mut self) -> Option<String> {
        let mut output = String::new();

        // Register nested structs first
        for nested in &self.data.nested_structs {
            crate::system::system().register_struct(&nested.name, &nested.name);
            // Register struct constructor pattern for this struct
            self.register_struct_constructor(&nested.name, &nested.fields);
        }

        // Register the main struct
        crate::system::system()
            .register_struct(&self.data.main_struct.name, &self.data.main_struct.name);
        // Register struct constructor pattern for main struct
        self.register_struct_constructor(
            &self.data.main_struct.name,
            &self.data.main_struct.fields,
        );

        // Register typedef alias if present
        if let Some(ref alias) = self.data.typedef_alias {
            if alias != &self.data.main_struct.name {
                crate::system::system().register_typedef(
                    alias,
                    alias,
                    &self.data.main_struct.name,
                );
            }
        }

        // First, output any nested structs (they must be defined before use)
        for nested in &self.data.nested_structs {
            output.push_str(&self.convert_struct_def(nested));
            output.push_str("\n\n");
        }

        // Then output the main struct
        output.push_str(&self.convert_struct_def(&self.data.main_struct));

        // Add type alias if typedef was used with a different name
        if let Some(ref alias) = self.data.typedef_alias {
            if alias != &self.data.main_struct.name {
                output.push_str(&format!(
                    "\npub type {} = {};",
                    alias, self.data.main_struct.name
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
                            self.data.nested_structs.len()
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
                    let main_struct = std::mem::take(&mut self.data.main_struct);
                    self.data.main_struct = nested_def;
                    self.parse_struct_body(&nested_body.to_vec(), nesting_depth + 1);
                    nested_def = std::mem::take(&mut self.data.main_struct);
                    self.data.main_struct = main_struct;

                    self.data.nested_structs.push(nested_def);

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
                        };
                        self.data.main_struct.fields.push(field);
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
                self.data.main_struct.fields.push(f);
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

        // Check for pointer
        let is_pointer = field_tokens.iter().any(|t| t == "*");

        // Check for array
        let bracket_pos = field_tokens.iter().position(|t| t == "[");
        let mut array_dims = Vec::new();

        if let Some(bp) = bracket_pos {
            // Parse array dimensions
            let mut j = bp;
            while j < field_tokens.len() {
                if field_tokens[j] == "[" {
                    j += 1;
                    if j < field_tokens.len() && field_tokens[j] != "]" {
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

        // Name is before [ or at end
        let name_pos = bracket_pos.unwrap_or(field_tokens.len()) - 1;
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

            output.push_str(&format!("    pub {}: {},\n", field.name, rust_type));
        }

        output.push_str("}");
        output
    }
}

impl Build for StructHandler {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("Handler", "StructHandler");
        entry.set_attr("stage", Entry::string(self.stage.as_str()));
        entry.set_attr("confidence", Entry::f64(self.confidence));
        entry.set_attr("struct_name", Entry::string(&self.data.main_struct.name));
        entry.set_attr(
            "field_count",
            Entry::i64(self.data.main_struct.fields.len() as i64),
        );
        entry.set_attr(
            "nested_count",
            Entry::i64(self.data.nested_structs.len() as i64),
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
        Some("StructHandler")
    }
    fn category(&self) -> Option<&str> {
        Some("struct")
    }
}
