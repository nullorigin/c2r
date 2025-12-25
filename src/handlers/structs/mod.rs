//! Struct Handler
//!
//! Converts C struct definitions to Rust structs.
//! Handles nested structs by flattening them (Rust doesn't support nested struct definitions).

use crate::db::web::{Entry, Build};
use crate::db::token::Token;
use crate::db::convert::TypeConverter;
use crate::db::pattern::{Pattern, PatternRule};
use crate::handlers::process::{ProcessStage, Processor};
use crate::handlers::validation::SequenceValidator;
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
        entry.set_attr("c_name", Entry::string(&self.name));
        entry.set_attr("rust_name", Entry::string(&self.name));
        entry.set_attr("is_anonymous", Entry::bool(self.is_anonymous));
        entry.set_attr("nesting_depth", Entry::usize(self.nesting_depth));
        entry.set_attr("field_count", Entry::usize(self.fields.len()));
        
        // Store field info
        let field_entries: Vec<Entry> = self.fields.iter().map(|f| {
            let mut fe = Entry::node("Field", &f.name);
            fe.set_attr("c_type", Entry::string(&f.c_type));
            fe.set_attr("rust_type", Entry::string(&f.rust_type));
            fe.set_attr("is_pointer", Entry::bool(f.is_pointer));
            fe
        }).collect();
        entry.set_attr("fields", Entry::vec(field_entries));
        
        entry
    }
    
    fn kind(&self) -> &str { "Struct" }
    fn name(&self) -> Option<&str> { Some(&self.name) }
    fn category(&self) -> Option<&str> { Some("type") }
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
    type_converter: TypeConverter,
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
            type_converter: TypeConverter::new(),
            range: 0..0,
            input_tokens: Vec::new(),
        }
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
    
    fn patterns(&self) -> Vec<Pattern> {
        vec![
            Pattern::definition(400, "struct_definition", vec![
                PatternRule::exact("struct"),
                PatternRule::identifier().optional(),
                PatternRule::exact("{"),
                PatternRule::any().with_can_repeat(true),
                PatternRule::exact("}"),
                PatternRule::identifier().optional(),
                PatternRule::exact(";"),
            ])
            .with_category("struct")
            .with_priority(135)
            .with_min_tokens(4)
            .with_description("C struct definition"),
            
            Pattern::definition(401, "typedef_struct", vec![
                PatternRule::exact("typedef"),
                PatternRule::exact("struct"),
                PatternRule::identifier().optional(),
                PatternRule::exact("{"),
                PatternRule::any().with_can_repeat(true),
                PatternRule::exact("}"),
                PatternRule::identifier(),
                PatternRule::exact(";"),
            ])
            .with_category("struct")
            .with_priority(140)
            .with_min_tokens(6)
            .with_description("Typedef struct definition"),
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
        for pattern in self.patterns() {
            if let Some(confidence) = pattern.matches_tokens(&self.input_tokens) {
                if confidence > best_confidence {
                    best_confidence = confidence;
                }
            }
        }
        
        // Check for struct keyword
        let first = tokens[0].to_string();
        let has_struct = first == "struct" || 
            (first == "typedef" && tokens.len() > 1 && tokens[1].to_string() == "struct");
        
        if !has_struct {
            self.error = Some("Not a struct definition".to_string());
            return false;
        }
        
        // Must have braces for definition
        let has_brace = self.input_tokens.iter().any(|t| t == "{");
        if !has_brace {
            self.error = Some("Struct must have body".to_string());
            return false;
        }
        
        // Use keyword validation to check sequence
        let validator = SequenceValidator::new();
        let validation_result = validator.validate_type_declaration(tokens);
        
        // Adjust confidence based on both pattern matching and keyword validation
        let base_confidence = if best_confidence > 0.0 { best_confidence } else { 0.7 };
        self.confidence = validation_result.adjust_confidence(base_confidence);
        
        true
    }
    
    fn extract(&mut self, tokens: &[Token]) -> bool {
        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        
        let mut idx = 0;
        let is_typedef = token_strs.get(0) == Some(&"typedef".to_string());
        
        if is_typedef {
            idx += 1; // skip "typedef"
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
        let body_tokens = &token_strs[brace_pos + 1..close_brace];
        
        // Parse fields, handling nested structs
        self.parse_struct_body(body_tokens, 0);
        
        // Check for typedef alias after closing brace
        if is_typedef && close_brace + 1 < token_strs.len() {
            let alias = &token_strs[close_brace + 1];
            if alias != ";" {
                self.data.typedef_alias = Some(alias.clone());
                // Use typedef alias as name if struct was anonymous
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
            crate::system::system_mut().register_struct(&nested.name, &nested.name);
        }
        
        // Register the main struct
        crate::system::system_mut().register_struct(
            &self.data.main_struct.name,
            &self.data.main_struct.name
        );
        
        // Register typedef alias if present
        if let Some(ref alias) = self.data.typedef_alias {
            if alias != &self.data.main_struct.name {
                crate::system::system_mut().register_typedef(
                    alias,
                    alias,
                    &self.data.main_struct.name
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
                output.push_str(&format!("\npub type {} = {};", alias, self.data.main_struct.name));
            }
        }
        
        self.output = Some(output.clone());
        Some(output)
    }
    
    fn current_stage(&self) -> ProcessStage { self.stage }
    fn set_stage(&mut self, stage: ProcessStage) { self.stage = stage; }
    fn output(&self) -> Option<String> { self.output.clone() }
    fn set_output(&mut self, output: String) { self.output = Some(output); }
    fn error(&self) -> Option<String> { self.error.clone() }
    fn set_error(&mut self, error: String) { self.error = Some(error); }
    fn confidence(&self) -> f64 { self.confidence }
    fn set_confidence(&mut self, confidence: f64) { self.confidence = confidence; }
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
                        format!("Nested{}_{}", nested_kind.to_uppercase(), self.data.nested_structs.len())
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
        let mut converter = self.type_converter.clone();
        let rust_type = converter.convert(&c_type).unwrap_or_else(|| c_type.clone());
        
        Some((StructField {
            name,
            c_type,
            rust_type,
            is_pointer,
            array_dims,
        }, semi_pos + 1))
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
                // Convert array dimensions
                let mut arr_type = field.rust_type.clone();
                for dim in field.array_dims.iter().rev() {
                    arr_type = format!("[{}; {}]", arr_type, dim);
                }
                arr_type
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
        entry.set_attr("field_count", Entry::i64(self.data.main_struct.fields.len() as i64));
        entry.set_attr("nested_count", Entry::i64(self.data.nested_structs.len() as i64));
        if let Some(ref output) = self.output {
            entry.set_attr("output", Entry::string(output));
        }
        entry
    }
    
    fn kind(&self) -> &str { "Handler" }
    fn name(&self) -> Option<&str> { Some("StructHandler") }
    fn category(&self) -> Option<&str> { Some("struct") }
}
