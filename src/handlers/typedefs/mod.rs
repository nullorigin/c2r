//! Typedef Handler
//!
//! Converts C typedef declarations to Rust type aliases.
//! Example: `typedef int MyInt;` -> `type MyInt = i32;`

use std::collections::HashMap;
use std::ops::Range;

use crate::db::web::{Entry, Build};
use crate::db::token::Token;
use crate::db::convert::TypeConverter;
use crate::db::pattern::{Pattern, PatternRule};
use crate::handlers::process::{ProcessStage, ProcessDecision, Processor};

// ============================================================================
// Typedef Handler Implementation
// ============================================================================

/// Extracted typedef information
#[derive(Debug, Clone, Default)]
pub struct TypedefData {
    /// The new type name (alias)
    pub alias_name: String,
    /// The original C type being aliased
    pub original_type: String,
    /// Whether this is a pointer typedef
    pub is_pointer: bool,
    /// Array dimensions if any
    pub array_dims: Vec<usize>,
    /// Whether it's a function pointer
    pub is_func_ptr: bool,
}

impl Build for TypedefData {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("Typedef", &self.alias_name);
        entry.set_attr("c_name", Entry::string(&self.alias_name));
        entry.set_attr("rust_name", Entry::string(&self.alias_name));
        entry.set_attr("underlying_type", Entry::string(&self.original_type));
        entry.set_attr("is_pointer", Entry::bool(self.is_pointer));
        entry.set_attr("is_func_ptr", Entry::bool(self.is_func_ptr));
        entry
    }
    
    fn kind(&self) -> &str { "Typedef" }
    fn name(&self) -> Option<&str> { Some(&self.alias_name) }
    fn category(&self) -> Option<&str> { Some("type") }
}

/// Typedef handler state for processing
#[derive(Debug, Clone)]
pub struct TypedefHandler {
    /// Handler name
    name: String,
    /// Current processing stage
    stage: ProcessStage,
    /// Extracted typedef data
    data: TypedefData,
    /// Generated Rust output
    output: Option<String>,
    /// Error message if failed
    error: Option<String>,
    /// Confidence score
    confidence: f64,
    /// Token range processed
    range: Range<usize>,
    /// Input tokens (stored for Build trait)
    input_tokens: Vec<String>,
    /// Type converter for C to Rust type mapping
    type_converter: TypeConverter,
}

impl TypedefHandler {
    pub fn new() -> Self {
        Self {
            name: "typedef".to_string(),
            stage: ProcessStage::Pending,
            data: TypedefData::default(),
            output: None,
            error: None,
            confidence: 0.0,
            range: 0..0,
            input_tokens: Vec::new(),
            type_converter: TypeConverter::new(),
        }
    }
}

impl Default for TypedefHandler {
    fn default() -> Self {
        Self::new()
    }
}

impl Processor for TypedefHandler {
    fn name(&self) -> &str {
        &self.name
    }
    
    fn supported_patterns(&self) -> &[&str] {
        &["typedef_alias", "typedef_struct"]
    }
    
    fn patterns(&self) -> Vec<Pattern> {
        vec![
            Pattern::definition(0, "typedef_struct", vec![
                    PatternRule::exact("typedef"),
                    PatternRule::exact("struct"),
                    PatternRule::identifier().optional(),
                    PatternRule::exact("{"),
                    PatternRule::any().with_can_repeat(true),
                    PatternRule::exact("}"),
                    PatternRule::identifier(),
                    PatternRule::exact(";"),
                ])
                .with_category("typedef")
                .with_priority(130)
                .with_min_tokens(6)
                .with_description("Typedef struct definition"),
            
            Pattern::definition(1, "typedef_alias", vec![
                    PatternRule::exact("typedef"),
                    PatternRule::type_keyword(),
                    PatternRule::identifier(),
                    PatternRule::exact(";"),
                ])
                .with_category("typedef")
                .with_priority(125)
                .with_min_tokens(4)
                .with_description("Simple typedef alias"),
        ]
    }
    
    fn validate(&mut self, tokens: &[Token]) -> bool {
        // Need at least: typedef <type> <name> ;
        if tokens.len() < 4 {
            self.error = Some("Too few tokens for typedef".to_string());
            return false;
        }
        
        // Store tokens for later
        self.input_tokens = tokens.iter().map(|t| t.to_string()).collect();
        self.range = 0..tokens.len();
        
        // First token should be "typedef"
        let first = tokens[0].to_string();
        if first != "typedef" {
            self.error = Some(format!("Expected 'typedef', got '{}'", first));
            return false;
        }
        
        // Last token should be semicolon
        let last = tokens[tokens.len() - 1].to_string();
        if last != ";" {
            self.error = Some("Typedef must end with semicolon".to_string());
            return false;
        }
        
        // Try matching against our patterns
        let mut best_confidence = 0.0;
        for pattern in self.patterns() {
            if let Some(confidence) = pattern.matches_tokens(&self.input_tokens) {
                if confidence > best_confidence {
                    best_confidence = confidence;
                }
            }
        }
        
        self.confidence = if best_confidence > 0.0 { best_confidence } else { 0.7 };
        true
    }
    
    fn extract(&mut self, tokens: &[Token]) -> bool {
        // Skip "typedef" and final ";"
        let inner_tokens: Vec<String> = tokens[1..tokens.len()-1]
            .iter()
            .map(|t| t.to_string())
            .collect();
        
        if inner_tokens.is_empty() {
            self.error = Some("No type information in typedef".to_string());
            return false;
        }
        
        // Simple case: typedef <type> <name>
        // The last token is the alias name, everything else is the type
        if inner_tokens.len() < 2 {
            self.error = Some("Typedef needs at least type and name".to_string());
            return false;
        }
        
        // The alias name is always the last token
        self.data.alias_name = inner_tokens.last().unwrap().clone();
        
        // Check for pointer in any position and collect type parts
        let mut type_parts: Vec<&str> = Vec::new();
        for token in inner_tokens.iter().take(inner_tokens.len() - 1) {
            if *token == "*" {
                self.data.is_pointer = true;
            } else {
                type_parts.push(token);
            }
        }
        
        self.data.original_type = type_parts.join(" ");
        
        if self.data.alias_name.is_empty() || self.data.original_type.is_empty() {
            self.error = Some("Could not extract typedef name or type".to_string());
            return false;
        }
        
        self.confidence = 0.85;
        true
    }
    
    fn convert(&mut self) -> Option<String> {
        // Use TypeConverter for C to Rust type mapping
        let rust_type = self.type_converter
            .convert(&self.data.original_type)
            .unwrap_or_else(|| self.data.original_type.clone());
        
        // Register the typedef
        crate::system::system_mut().register_typedef(
            &self.data.alias_name,
            &self.data.alias_name,
            &self.data.original_type
        );
        
        let rust_code = if self.data.is_pointer {
            format!("type {} = *mut {};", self.data.alias_name, rust_type)
        } else {
            format!("type {} = {};", self.data.alias_name, rust_type)
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
// Build Trait Implementation - Stores handler state incrementally
// ============================================================================

impl Build for TypedefHandler {
    fn to_entry(&self) -> Entry {
        let mut attrs = HashMap::new();
        
        // Store stage
        attrs.insert("stage".to_string(), Entry::string(self.stage.as_str()));
        
        // Store confidence
        attrs.insert("confidence".to_string(), Entry::f64(self.confidence));
        
        // Store extracted data
        attrs.insert("alias_name".to_string(), Entry::string(&self.data.alias_name));
        attrs.insert("original_type".to_string(), Entry::string(&self.data.original_type));
        attrs.insert("is_pointer".to_string(), Entry::bool(self.data.is_pointer));
        
        // Store output if available
        if let Some(ref output) = self.output {
            attrs.insert("rust_code".to_string(), Entry::string(output));
        }
        
        // Store error if any
        if let Some(ref error) = self.error {
            attrs.insert("error".to_string(), Entry::string(error));
        }
        
        // Store input tokens
        let tokens: Vec<Entry> = self.input_tokens.iter()
            .map(|t| Entry::string(t))
            .collect();
        attrs.insert("input_tokens".to_string(), Entry::vec(tokens));
        
        // Store range
        attrs.insert("range_start".to_string(), Entry::usize(self.range.start));
        attrs.insert("range_end".to_string(), Entry::usize(self.range.end));
        
        Entry::node_with_attrs("TypedefHandler", &self.data.alias_name, attrs)
    }
    
    fn kind(&self) -> &str {
        "TypedefHandler"
    }
    
    fn name(&self) -> Option<&str> {
        if self.data.alias_name.is_empty() {
            None
        } else {
            Some(&self.data.alias_name)
        }
    }
    
    fn category(&self) -> Option<&str> {
        Some("typedef")
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
    fn test_simple_typedef() {
        let mut handler = TypedefHandler::new();
        let tokens = make_tokens(&["typedef", "int", "MyInt", ";"]);
        
        // Process through all stages
        let decision = handler.process(&tokens);
        assert!(matches!(decision, ProcessDecision::Continue { stage: ProcessStage::Validated, .. }));
        
        let decision = handler.process(&tokens);
        assert!(matches!(decision, ProcessDecision::Continue { stage: ProcessStage::Extracted, .. }));
        
        let decision = handler.process(&tokens);
        assert!(matches!(decision, ProcessDecision::Continue { stage: ProcessStage::Converted, .. }));
        
        let decision = handler.process(&tokens);
        if let ProcessDecision::Complete { rust_code, confidence } = decision {
            assert_eq!(rust_code, "type MyInt = i32;");
            assert!(confidence > 0.9);
        } else {
            panic!("Expected Complete decision");
        }
    }
    
    #[test]
    fn test_pointer_typedef() {
        let mut handler = TypedefHandler::new();
        let tokens = make_tokens(&["typedef", "int", "*", "IntPtr", ";"]);
        
        // Run through all stages
        while !matches!(handler.current_stage(), ProcessStage::Complete | ProcessStage::Failed) {
            handler.process(&tokens);
        }
        
        assert_eq!(handler.output(), Some("type IntPtr = *mut i32;".to_string()));
    }
    
    #[test]
    fn test_validation_failure() {
        let mut handler = TypedefHandler::new();
        let tokens = make_tokens(&["int", "x", ";"]);
        
        let decision = handler.process(&tokens);
        assert!(matches!(decision, ProcessDecision::Fail { .. }));
    }
    
    #[test]
    fn test_build_trait() {
        let mut handler = TypedefHandler::new();
        let tokens = make_tokens(&["typedef", "unsigned", "long", "Size", ";"]);
        
        // Process to completion
        while !matches!(handler.current_stage(), ProcessStage::Complete | ProcessStage::Failed) {
            handler.process(&tokens);
        }
        
        let entry = handler.to_entry();
        assert_eq!(entry.kind(), Some("TypedefHandler"));
        assert_eq!(entry.name(), Some("Size"));
    }
}
