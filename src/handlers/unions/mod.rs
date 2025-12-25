//! Union Handler
//!
//! Converts C union definitions to Rust unions.
//! Handles nested unions by flattening them (Rust doesn't support nested union definitions).

use crate::db::web::{Entry, Build};
use crate::db::token::Token;
use crate::db::convert::TypeConverter;
use crate::db::pattern::{Pattern, PatternRule};
use crate::handlers::process::{ProcessStage, Processor};
use std::ops::Range;

// ============================================================================
// Union Handler Implementation
// ============================================================================

/// A single union field
#[derive(Debug, Clone, Default)]
pub struct UnionField {
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

/// A union definition
#[derive(Debug, Clone, Default)]
pub struct UnionDef {
    /// Union name
    pub name: String,
    /// Fields
    pub fields: Vec<UnionField>,
    /// Is this an anonymous union
    pub is_anonymous: bool,
}

impl Build for UnionDef {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("Union", &self.name);
        entry.set_attr("c_name", Entry::string(&self.name));
        entry.set_attr("rust_name", Entry::string(&self.name));
        entry.set_attr("is_anonymous", Entry::bool(self.is_anonymous));
        entry.set_attr("field_count", Entry::usize(self.fields.len()));
        
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
    
    fn kind(&self) -> &str { "Union" }
    fn name(&self) -> Option<&str> { Some(&self.name) }
    fn category(&self) -> Option<&str> { Some("type") }
}

/// Extracted union information
#[derive(Debug, Clone, Default)]
pub struct UnionData {
    /// Main union definition
    pub main_union: UnionDef,
    /// Nested types that were extracted
    pub nested_types: Vec<UnionDef>,
    /// Typedef alias if any
    pub typedef_alias: Option<String>,
}

/// Handler for C union definitions
#[derive(Debug)]
pub struct UnionHandler {
    name: String,
    stage: ProcessStage,
    confidence: f64,
    error: Option<String>,
    output: Option<String>,
    data: UnionData,
    type_converter: TypeConverter,
    range: Range<usize>,
    input_tokens: Vec<String>,
}

impl UnionHandler {
    pub fn new() -> Self {
        Self {
            name: "union".to_string(),
            stage: ProcessStage::Pending,
            confidence: 0.0,
            error: None,
            output: None,
            data: UnionData::default(),
            type_converter: TypeConverter::new(),
            range: 0..0,
            input_tokens: Vec::new(),
        }
    }
}

impl Default for UnionHandler {
    fn default() -> Self {
        Self::new()
    }
}

impl Processor for UnionHandler {
    fn name(&self) -> &str {
        &self.name
    }
    
    fn supported_patterns(&self) -> &[&str] {
        &["union_definition", "typedef_union"]
    }
    
    fn patterns(&self) -> Vec<Pattern> {
        vec![
            Pattern::definition(600, "union_definition", vec![
                PatternRule::exact("union"),
                PatternRule::identifier().optional(),
                PatternRule::exact("{"),
                PatternRule::any().with_can_repeat(true),
                PatternRule::exact("}"),
                PatternRule::identifier().optional(),
                PatternRule::exact(";"),
            ])
            .with_category("union")
            .with_priority(135)
            .with_min_tokens(4)
            .with_description("C union definition"),
            
            Pattern::definition(601, "typedef_union", vec![
                PatternRule::exact("typedef"),
                PatternRule::exact("union"),
                PatternRule::identifier().optional(),
                PatternRule::exact("{"),
                PatternRule::any().with_can_repeat(true),
                PatternRule::exact("}"),
                PatternRule::identifier(),
                PatternRule::exact(";"),
            ])
            .with_category("union")
            .with_priority(140)
            .with_min_tokens(6)
            .with_description("Typedef union definition"),
        ]
    }
    
    fn validate(&mut self, tokens: &[Token]) -> bool {
        if tokens.len() < 4 {
            self.error = Some("Too few tokens for union".to_string());
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
        
        // Check for union keyword
        let first = tokens[0].to_string();
        let has_union = first == "union" || 
            (first == "typedef" && tokens.len() > 1 && tokens[1].to_string() == "union");
        
        if !has_union {
            self.error = Some("Not a union definition".to_string());
            return false;
        }
        
        // Must have braces for definition
        let has_brace = self.input_tokens.iter().any(|t| t == "{");
        if !has_brace {
            self.error = Some("Union must have body".to_string());
            return false;
        }
        
        self.confidence = if best_confidence > 0.0 { best_confidence } else { 0.7 };
        true
    }
    
    fn extract(&mut self, tokens: &[Token]) -> bool {
        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        
        let mut idx = 0;
        let is_typedef = token_strs.get(0) == Some(&"typedef".to_string());
        
        if is_typedef {
            idx += 1;
        }
        
        // Skip "union"
        if token_strs.get(idx) != Some(&"union".to_string()) {
            self.error = Some("Expected 'union' keyword".to_string());
            return false;
        }
        idx += 1;
        
        // Get union name (optional before brace)
        let brace_pos = match token_strs.iter().position(|t| t == "{") {
            Some(p) => p,
            None => {
                self.error = Some("No opening brace found".to_string());
                return false;
            }
        };
        
        // Name is between union and {
        if idx < brace_pos {
            self.data.main_union.name = token_strs[idx].clone();
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
        
        // Parse fields
        self.parse_union_body(body_tokens);
        
        // Check for typedef alias after closing brace
        if is_typedef && close_brace + 1 < token_strs.len() {
            let alias = &token_strs[close_brace + 1];
            if alias != ";" {
                self.data.typedef_alias = Some(alias.clone());
                if self.data.main_union.name.is_empty() {
                    self.data.main_union.name = alias.clone();
                }
            }
        }
        
        // If still no name, generate one
        if self.data.main_union.name.is_empty() {
            self.data.main_union.name = format!("AnonymousUnion{}", self.range.start);
            self.data.main_union.is_anonymous = true;
        }
        
        true
    }
    
    fn convert(&mut self) -> Option<String> {
        let mut output = String::new();
        
        // Register nested types first
        for nested in &self.data.nested_types {
            crate::system::system_mut().register_union(&nested.name, &nested.name);
        }
        
        // Register the main union
        crate::system::system_mut().register_union(
            &self.data.main_union.name,
            &self.data.main_union.name
        );
        
        // Register typedef alias if present
        if let Some(ref alias) = self.data.typedef_alias {
            if alias != &self.data.main_union.name {
                crate::system::system_mut().register_typedef(
                    alias,
                    alias,
                    &self.data.main_union.name
                );
            }
        }
        
        // First, output any nested types
        for nested in &self.data.nested_types {
            output.push_str(&self.convert_union_def(nested));
            output.push_str("\n\n");
        }
        
        // Then output the main union
        output.push_str(&self.convert_union_def(&self.data.main_union));
        
        // Add type alias if typedef was used with a different name
        if let Some(ref alias) = self.data.typedef_alias {
            if alias != &self.data.main_union.name {
                output.push_str(&format!("\npub type {} = {};", alias, self.data.main_union.name));
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

impl UnionHandler {
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
    
    /// Parse union body, extracting fields
    fn parse_union_body(&mut self, tokens: &[String]) {
        let mut i = 0;
        
        while i < tokens.len() {
            // Check for nested struct/union
            if tokens[i] == "struct" || tokens[i] == "union" {
                let nested_kind = tokens[i].clone();
                i += 1;
                
                let has_brace = tokens[i..].iter().take(3).any(|t| t == "{");
                
                if has_brace {
                    // Extract nested definition
                    let nested_name = if i < tokens.len() && tokens[i] != "{" {
                        let name = tokens[i].clone();
                        i += 1;
                        name
                    } else {
                        format!("Nested{}_{}", nested_kind.to_uppercase(), self.data.nested_types.len())
                    };
                    
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
                    
                    // Create nested definition (as union for simplicity)
                    let nested_def = UnionDef {
                        name: nested_name.clone(),
                        fields: Vec::new(),
                        is_anonymous: false,
                    };
                    
                    self.data.nested_types.push(nested_def);
                    
                    i = brace_end + 1;
                    
                    // Check for field name after nested definition
                    if i < tokens.len() && tokens[i] != ";" && tokens[i] != "}" {
                        let field_name = tokens[i].clone();
                        let field = UnionField {
                            name: field_name,
                            c_type: nested_name.clone(),
                            rust_type: nested_name,
                            is_pointer: false,
                            array_dims: Vec::new(),
                        };
                        self.data.main_union.fields.push(field);
                        i += 1;
                    }
                    
                    if i < tokens.len() && tokens[i] == ";" {
                        i += 1;
                    }
                    continue;
                }
            }
            
            // Regular field
            let field = self.parse_field(&tokens[i..]);
            if let Some((f, consumed)) = field {
                self.data.main_union.fields.push(f);
                i += consumed;
            } else {
                i += 1;
            }
        }
    }
    
    /// Parse a single field declaration
    fn parse_field(&self, tokens: &[String]) -> Option<(UnionField, usize)> {
        if tokens.is_empty() {
            return None;
        }
        
        let semi_pos = tokens.iter().position(|t| t == ";")?;
        if semi_pos < 2 {
            return None;
        }
        
        let field_tokens = &tokens[..semi_pos];
        let is_pointer = field_tokens.iter().any(|t| t == "*");
        
        let bracket_pos = field_tokens.iter().position(|t| t == "[");
        let mut array_dims = Vec::new();
        
        if let Some(bp) = bracket_pos {
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
        
        let name_pos = bracket_pos.unwrap_or(field_tokens.len()) - 1;
        if name_pos == 0 {
            return None;
        }
        
        let name = field_tokens[name_pos].clone();
        
        let type_tokens: Vec<&str> = field_tokens[..name_pos]
            .iter()
            .filter(|t| *t != "*")
            .map(|s| s.as_str())
            .collect();
        
        let c_type = type_tokens.join(" ");
        let mut converter = self.type_converter.clone();
        let rust_type = converter.convert(&c_type).unwrap_or_else(|| c_type.clone());
        
        Some((UnionField {
            name,
            c_type,
            rust_type,
            is_pointer,
            array_dims,
        }, semi_pos + 1))
    }
    
    /// Convert a union definition to Rust code
    fn convert_union_def(&self, def: &UnionDef) -> String {
        let mut output = String::new();
        
        output.push_str("#[repr(C)]\n");
        output.push_str("#[derive(Clone, Copy)]\n");
        output.push_str(&format!("pub union {} {{\n", def.name));
        
        for field in &def.fields {
            let rust_type = if field.is_pointer {
                format!("*mut {}", field.rust_type)
            } else if !field.array_dims.is_empty() {
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

impl Build for UnionHandler {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("Handler", "UnionHandler");
        entry.set_attr("stage", Entry::string(self.stage.as_str()));
        entry.set_attr("confidence", Entry::f64(self.confidence));
        entry.set_attr("union_name", Entry::string(&self.data.main_union.name));
        entry.set_attr("field_count", Entry::i64(self.data.main_union.fields.len() as i64));
        if let Some(ref output) = self.output {
            entry.set_attr("output", Entry::string(output));
        }
        entry
    }
    
    fn kind(&self) -> &str { "Handler" }
    fn name(&self) -> Option<&str> { Some("UnionHandler") }
    fn category(&self) -> Option<&str> { Some("union") }
}
