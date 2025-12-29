//! Function Handler
//!
//! Converts C functions to Rust functions.
//! Handles both function declarations and definitions.

pub mod calls;

pub use calls::CallHandler;

use crate::db::convert::sanitize_rust_identifier;
use crate::db::pattern::{Pattern, PatternRule};
use crate::db::routing::{create_routing, set_routing_result};
use crate::db::token::Token;
use crate::db::web::{Build, Entry};
use crate::handlers::arrays::ArrayHandler;
use crate::handlers::conditionals::{IfElseHandler, SwitchCaseHandler};
use crate::handlers::loops::LoopHandler;
use crate::handlers::process::{ProcessStage, Processor};
use crate::handlers::variables::VariableHandler;
use crate::system::system;

// ============================================================================
// Function Handler Implementation
// ============================================================================

/// Extracted function information
#[derive(Debug, Clone, Default)]
pub struct FunctionData {
    pub name: String,
    pub return_type: String,
    pub parameters: Vec<(String, String, bool, bool)>, // (name, type, is_pointer, is_array)
    pub body_tokens: Vec<Token>,
    pub is_declaration: bool,
    pub is_static: bool,
    pub is_inline: bool,
    pub is_extern: bool,
}

/// Handler for C function definitions and declarations
#[derive(Debug)]
pub struct FunctionHandler {
    stage: ProcessStage,
    confidence: f64,
    error: Option<String>,
    output: Option<String>,
    data: FunctionData,
}

impl Default for FunctionHandler {
    fn default() -> Self { Self::new() }
}

impl FunctionHandler {
    pub fn new() -> Self {
        Self {
            stage: ProcessStage::Pending,
            confidence: 0.0,
            error: None,
            output: None,
            data: FunctionData::default(),
        }
    }
}

impl Processor for FunctionHandler {
    fn supported_patterns(&self) -> &[&str] {
        &[
            "validate_function_definition",
            "extract_function_definition",
            "validate_function_declaration",
            "extract_function_declaration",
            "validate_static_function",
            "extract_static_function",
            "validate_inline_function",
            "extract_inline_function",
        ]
    }

    fn patterns(&self) -> Vec<(Pattern, Pattern)> {
        vec![
            // Standard function definition: type name(...) {
            (
                Pattern::definition(200, "validate_function_definition", vec![
                    PatternRule::type_keyword(),
                    PatternRule::identifier(),
                    PatternRule::exact("(").with_forbid_previous(vec!["=".to_string()]),
                    PatternRule::any_except(")").optional().repeat(-1),
                    PatternRule::exact(")"),
                    PatternRule::exact("{"),
                ])
                .with_category("function")
                .with_priority(920)
                .with_min_tokens(6)
                .with_description("Function definition with body"),
                Pattern::definition(200, "extract_function_definition", vec![
                    PatternRule::type_keyword().with_extract(|rule, ctx| {
                        let token = ctx.current_token.clone();
                        if ctx.did_match { ctx.set_value("return_type", &token); }
                        rule.clone()
                    }),
                    PatternRule::identifier().with_extract(|rule, ctx| {
                        let token = ctx.current_token.clone();
                        if ctx.did_match { ctx.set_value("name", &token); }
                        rule.clone()
                    }),
                    PatternRule::exact("(").with_forbid_previous(vec!["=".to_string()])
                        .with_extract(|rule, ctx| { ctx.set_flag("in_params", true); rule.clone() }),
                    PatternRule::any_except(")").optional().repeat(-1)
                        .with_extract(|rule, ctx| {
                            let token = ctx.current_token.clone();
                            if ctx.did_match && ctx.flag("in_params") { ctx.push_list("params", &token); }
                            rule.clone()
                        }),
                    PatternRule::exact(")").with_extract(|rule, ctx| { ctx.set_flag("in_params", false); rule.clone() }),
                    PatternRule::exact("{").with_extract(|rule, ctx| { ctx.set_flag("has_body", true); rule.clone() }),
                ])
                .with_category("function")
                .with_priority(920)
                .with_min_tokens(6),
            ),
            // Function declaration: type name(...);
            (
                Pattern::definition(201, "validate_function_declaration", vec![
                    PatternRule::type_keyword(),
                    PatternRule::identifier(),
                    PatternRule::exact("(").with_forbid_previous(vec!["=".to_string()]),
                    PatternRule::any_except(")").optional().repeat(-1),
                    PatternRule::exact(")"),
                    PatternRule::exact(";"),
                ])
                .with_category("function")
                .with_priority(910)
                .with_min_tokens(5)
                .with_description("Function declaration without body"),
                Pattern::definition(201, "extract_function_declaration", vec![
                    PatternRule::type_keyword().with_extract(|rule, ctx| {
                        let token = ctx.current_token.clone();
                        if ctx.did_match { ctx.set_value("return_type", &token); }
                        rule.clone()
                    }),
                    PatternRule::identifier().with_extract(|rule, ctx| {
                        let token = ctx.current_token.clone();
                        if ctx.did_match { ctx.set_value("name", &token); }
                        rule.clone()
                    }),
                    PatternRule::exact("(").with_forbid_previous(vec!["=".to_string()])
                        .with_extract(|rule, ctx| { ctx.set_flag("in_params", true); rule.clone() }),
                    PatternRule::any_except(")").optional().repeat(-1)
                        .with_extract(|rule, ctx| {
                            let token = ctx.current_token.clone();
                            if ctx.did_match && ctx.flag("in_params") { ctx.push_list("params", &token); }
                            rule.clone()
                        }),
                    PatternRule::exact(")").with_extract(|rule, ctx| { ctx.set_flag("in_params", false); rule.clone() }),
                    PatternRule::exact(";"),
                ])
                .with_category("function")
                .with_priority(910)
                .with_min_tokens(5),
            ),
            // Static function: static type name(...) {
            (
                Pattern::definition(202, "validate_static_function", vec![
                    PatternRule::exact("static"),
                    PatternRule::type_keyword(),
                    PatternRule::identifier(),
                    PatternRule::exact("("),
                    PatternRule::any_except(")").optional().repeat(-1),
                    PatternRule::exact(")"),
                    PatternRule::exact("{"),
                ])
                .with_category("function")
                .with_priority(925)
                .with_min_tokens(7),
                Pattern::definition(202, "extract_static_function", vec![
                    PatternRule::exact("static").with_extract(|rule, ctx| { ctx.add_modifier("static"); rule.clone() }),
                    PatternRule::type_keyword().with_extract(|rule, ctx| {
                        let token = ctx.current_token.clone();
                        if ctx.did_match { ctx.set_value("return_type", &token); }
                        rule.clone()
                    }),
                    PatternRule::identifier().with_extract(|rule, ctx| {
                        let token = ctx.current_token.clone();
                        if ctx.did_match { ctx.set_value("name", &token); }
                        rule.clone()
                    }),
                    PatternRule::exact("(").with_extract(|rule, ctx| { ctx.set_flag("in_params", true); rule.clone() }),
                    PatternRule::any_except(")").optional().repeat(-1)
                        .with_extract(|rule, ctx| {
                            let token = ctx.current_token.clone();
                            if ctx.did_match && ctx.flag("in_params") { ctx.push_list("params", &token); }
                            rule.clone()
                        }),
                    PatternRule::exact(")").with_extract(|rule, ctx| { ctx.set_flag("in_params", false); rule.clone() }),
                    PatternRule::exact("{").with_extract(|rule, ctx| { ctx.set_flag("has_body", true); rule.clone() }),
                ])
                .with_category("function")
                .with_priority(925)
                .with_min_tokens(7),
            ),
            // Inline function: inline type name(...) {
            (
                Pattern::definition(203, "validate_inline_function", vec![
                    PatternRule::one_of(vec!["inline".to_string(), "__inline".to_string(), "__inline__".to_string()]),
                    PatternRule::type_keyword(),
                    PatternRule::identifier(),
                    PatternRule::exact("("),
                    PatternRule::any_except(")").optional().repeat(-1),
                    PatternRule::exact(")"),
                    PatternRule::exact("{"),
                ])
                .with_category("function")
                .with_priority(925)
                .with_min_tokens(7),
                Pattern::definition(203, "extract_inline_function", vec![
                    PatternRule::one_of(vec!["inline".to_string(), "__inline".to_string(), "__inline__".to_string()])
                        .with_extract(|rule, ctx| { ctx.add_modifier("inline"); rule.clone() }),
                    PatternRule::type_keyword().with_extract(|rule, ctx| {
                        let token = ctx.current_token.clone();
                        if ctx.did_match { ctx.set_value("return_type", &token); }
                        rule.clone()
                    }),
                    PatternRule::identifier().with_extract(|rule, ctx| {
                        let token = ctx.current_token.clone();
                        if ctx.did_match { ctx.set_value("name", &token); }
                        rule.clone()
                    }),
                    PatternRule::exact("(").with_extract(|rule, ctx| { ctx.set_flag("in_params", true); rule.clone() }),
                    PatternRule::any_except(")").optional().repeat(-1)
                        .with_extract(|rule, ctx| {
                            let token = ctx.current_token.clone();
                            if ctx.did_match && ctx.flag("in_params") { ctx.push_list("params", &token); }
                            rule.clone()
                        }),
                    PatternRule::exact(")").with_extract(|rule, ctx| { ctx.set_flag("in_params", false); rule.clone() }),
                    PatternRule::exact("{").with_extract(|rule, ctx| { ctx.set_flag("has_body", true); rule.clone() }),
                ])
                .with_category("function")
                .with_priority(925)
                .with_min_tokens(7),
            ),
            // Struct-returning function: struct Name functionName(...) {
            (
                Pattern::definition(204, "validate_struct_returning_function", vec![
                    PatternRule::exact("struct"),
                    PatternRule::identifier(), // struct name
                    PatternRule::identifier(), // function name
                    PatternRule::exact("("),
                    PatternRule::any_except(")").optional().repeat(-1),
                    PatternRule::exact(")"),
                    PatternRule::exact("{"),
                ])
                .with_category("function")
                .with_priority(930)
                .with_min_tokens(7),
                Pattern::definition(204, "extract_struct_returning_function", vec![
                    PatternRule::exact("struct").with_extract(|rule, ctx| {
                        ctx.set_flag("is_struct_return", true);
                        rule.clone()
                    }),
                    PatternRule::identifier().with_extract(|rule, ctx| {
                        let token = ctx.current_token.clone();
                        if ctx.did_match { ctx.set_value("struct_name", &token); }
                        rule.clone()
                    }),
                    PatternRule::identifier().with_extract(|rule, ctx| {
                        let token = ctx.current_token.clone();
                        if ctx.did_match { ctx.set_value("name", &token); }
                        rule.clone()
                    }),
                    PatternRule::exact("(").with_extract(|rule, ctx| { ctx.set_flag("in_params", true); rule.clone() }),
                    PatternRule::any_except(")").optional().repeat(-1)
                        .with_extract(|rule, ctx| {
                            let token = ctx.current_token.clone();
                            if ctx.did_match && ctx.flag("in_params") { ctx.push_list("params", &token); }
                            rule.clone()
                        }),
                    PatternRule::exact(")").with_extract(|rule, ctx| { ctx.set_flag("in_params", false); rule.clone() }),
                    PatternRule::exact("{").with_extract(|rule, ctx| { ctx.set_flag("has_body", true); rule.clone() }),
                ])
                .with_category("function")
                .with_priority(930)
                .with_min_tokens(7),
            ),
        ]
    }

    fn validate(&mut self, tokens: &[Token]) -> bool {
        if tokens.len() < 5 { return false; }

        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        // Reject typedefs - they should go to TypedefHandler
        if token_strs.first().map(|s| s.as_str()) == Some("typedef") {
            return false;
        }

        // Validate C keyword ordering for function declaration prefix
        let token_refs: Vec<&str> = token_strs.iter().map(|s| s.as_str()).collect();
        let (c_order_valid, _) = system().validate_c_token_order(&token_refs);

        // Try patterns first
        let mut best_confidence = 0.0;
        let mut pattern_matched = false;
        for (validate_pattern, _) in self.patterns() {
            if let Some(confidence) = validate_pattern.matches_tokens(&token_strs) {
                if confidence > best_confidence {
                    best_confidence = confidence;
                    pattern_matched = true;
                }
            }
        }

        // If pattern matched with good confidence, accept it
        if pattern_matched && best_confidence >= 0.7 {
            // Determine if declaration or definition from tokens
            let has_open_brace = token_strs.iter().any(|t| t == "{");
            self.data.is_declaration = !has_open_brace;
            // Adjust confidence based on C keyword ordering validity
            self.confidence = if c_order_valid { best_confidence } else { best_confidence - 0.1 };
            return true;
        }

        // Fallback to manual validation
        // Must have ( and )
        let open_paren = token_strs.iter().position(|t| t == "(");
        let close_paren = token_strs.iter().position(|t| t == ")");
        if open_paren.is_none() || close_paren.is_none() { return false; }

        let close_pos = close_paren.unwrap();
        if close_pos + 1 >= token_strs.len() { return false; }

        // Determine if declaration or definition
        let next_after_paren = &token_strs[close_pos + 1];
        self.data.is_declaration = next_after_paren == ";";

        let base_conf = if best_confidence > 0.0 { best_confidence } else { 0.75 };
        self.confidence = if c_order_valid { base_conf } else { base_conf - 0.1 };
        true
    }

    fn extract(&mut self, tokens: &[Token]) -> bool {
        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        // Try extraction patterns first
        for (_, extract_pattern) in self.patterns() {
            if let Some(ctx) = extract_pattern.run_extraction(&token_strs) {
                // Handle struct return type
                if ctx.flag("is_struct_return") {
                    if let Some(struct_name) = ctx.value("struct_name") {
                        self.data.return_type = format!("struct {}", struct_name);
                    }
                } else if let Some(return_type) = ctx.value("return_type") {
                    self.data.return_type = return_type.to_string();
                }
                if let Some(name) = ctx.value("name") { self.data.name = name.to_string(); }
                if ctx.has_modifier("static") { self.data.is_static = true; }
                if ctx.has_modifier("inline") { self.data.is_inline = true; }
                if ctx.has_modifier("extern") { self.data.is_extern = true; }
                if let Some(params) = ctx.list("params") {
                    self.parse_parameters(&params.iter().map(|s| s.clone()).collect::<Vec<_>>());
                }
                self.data.is_declaration = !ctx.flag("has_body");

                if !self.data.name.is_empty() && !self.data.is_declaration {
                    // Extract body tokens - find matching closing brace (not last brace!)
                    if let Some(ob) = token_strs.iter().position(|t| t == "{") {
                        let mut brace_depth = 0;
                        let mut cb = ob;
                        for (idx, tok) in token_strs[ob..].iter().enumerate() {
                            match tok.as_str() {
                                "{" => brace_depth += 1,
                                "}" => {
                                    brace_depth -= 1;
                                    if brace_depth == 0 {
                                        cb = ob + idx;
                                        break;
                                    }
                                }
                                _ => {}
                            }
                        }
                        if cb > ob + 1 { self.data.body_tokens = tokens[ob + 1..cb].to_vec(); }
                    }
                }

                if !self.data.name.is_empty() {
                    self.confidence = 0.85;
                    return true;
                }
            }
        }

        // Fallback to manual extraction
        let mut idx = 0;
        while idx < token_strs.len() {
            match token_strs[idx].as_str() {
                "static" => { self.data.is_static = true; idx += 1; }
                "inline" | "__inline" | "__inline__" => { self.data.is_inline = true; idx += 1; }
                "extern" => { self.data.is_extern = true; idx += 1; }
                _ => break,
            }
        }

        let paren_pos = match token_strs[idx..].iter().position(|t| t == "(") {
            Some(p) => idx + p,
            None => return false,
        };

        if paren_pos < idx + 1 { return false; }

        self.data.name = token_strs[paren_pos - 1].clone();
        self.data.return_type = if paren_pos > idx + 1 {
            token_strs[idx..paren_pos - 1].join(" ")
        } else { "void".to_string() };

        let close_paren = match token_strs.iter().position(|t| t == ")") {
            Some(p) => p,
            None => return false,
        };

        if close_paren > paren_pos + 1 {
            self.parse_parameters(&token_strs[paren_pos + 1..close_paren].to_vec());
        }

        if !self.data.is_declaration {
            if let (Some(ob), Some(cb)) = (token_strs.iter().position(|t| t == "{"), token_strs.iter().rposition(|t| t == "}")) {
                if cb > ob + 1 { self.data.body_tokens = tokens[ob + 1..cb].to_vec(); }
            }
        }

        self.confidence = 0.85;
        true
    }

    fn convert(&mut self) -> Option<String> {
        // Convert return type
        let rust_return_type = if self.data.return_type == "void" || self.data.return_type.is_empty() {
            "()".to_string()
        } else {
            system().lookup_type(&self.data.return_type).unwrap_or_else(|| self.data.return_type.clone())
        };
        let rust_return = if rust_return_type == "()" { String::new() } else { format!(" -> {}", rust_return_type) };

        // Convert parameters and register them as variables for sizeof/type inference
        let mut param_info: Vec<(String, String)> = Vec::new();
        
        // Push a new scope for function parameters
        system().push_variable_scope();
        
        let params: Vec<String> = self.data.parameters.iter().map(|(name, c_type, is_ptr, is_array)| {
            let full_c_type = if *is_ptr && !c_type.contains('*') { format!("{} *", c_type) } else { c_type.clone() };
            let base_rust_type = system().lookup_type(&full_c_type).unwrap_or_else(|| {
                let base = system().lookup_type(c_type).unwrap_or_else(|| c_type.clone());
                if *is_ptr { format!("*mut {}", base) } else { base }
            });
            let rust_type = if *is_array { format!("Vec<{}>", base_rust_type) } else { base_rust_type.clone() };
            // Sanitize parameter name for Rust keywords (str -> r#str)
            let safe_name = sanitize_rust_identifier(name);
            param_info.push((safe_name.clone(), rust_type.clone()));
            
            // Register parameter as a variable for sizeof() and type inference
            if *is_ptr {
                system().register_pointer_variable(name, c_type, &base_rust_type, 1);
            } else if *is_array {
                // For arrays, we don't know the size, so just register as the base type
                system().register_variable(name, c_type, &rust_type);
            } else {
                system().register_variable(name, c_type, &rust_type);
            }
            
            format!("{}: {}", safe_name, rust_type)
        }).collect();
        let params_str = params.join(", ");

        // Register function in System
        system().register_function(&self.data.name, &self.data.name, &rust_return_type, param_info);

        // Build function
        let visibility = if self.data.is_static { "" } else { "pub " };
        let inline_attr = if self.data.is_inline { "#[inline]\n" } else { "" };

        let result = if self.data.is_declaration {
            self.confidence = 0.9;
            Some(format!("{}{}fn {}({}){};", inline_attr, visibility, self.data.name, params_str, rust_return))
        } else {
            let body = if self.data.body_tokens.is_empty() { "    todo!()".to_string() } else { self.convert_body() };
            self.confidence = 0.85;
            Some(format!("{}{}fn {}({}){} {{\n{}\n}}", inline_attr, visibility, self.data.name, params_str, rust_return, body))
        };
        
        // Pop the function scope (parameters go out of scope)
        system().pop_variable_scope();
        
        result
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

// ============================================================================
// Helper Methods
// ============================================================================

impl FunctionHandler {
    fn parse_parameters(&mut self, tokens: &[String]) {
        if tokens.is_empty() || (tokens.len() == 1 && tokens[0] == "void") { return; }
        tokens.split(|t| t == ",").filter(|g| !g.is_empty()).for_each(|group| {
            let refs: Vec<&str> = group.iter().map(|s| s.as_str()).collect();
            self.add_parameter(&refs);
        });
    }

    fn add_parameter(&mut self, tokens: &[&str]) {
        if tokens.is_empty() || tokens == &["void"] { return; }

        let mut is_pointer = tokens.iter().any(|t| *t == "*");
        let has_brackets = tokens.iter().any(|t| *t == "[");
        if has_brackets { is_pointer = true; }

        // Filter out brackets, array sizes, and struct/enum/union keywords
        let filtered: Vec<&str> = tokens.iter()
            .filter(|t| **t != "[" && **t != "]" && !t.parse::<usize>().is_ok())
            .filter(|t| !matches!(**t, "struct" | "enum" | "union"))
            .copied().collect();

        let name_idx = filtered.iter().rposition(|t| *t != "*").unwrap_or(0);
        let name = filtered.get(name_idx).unwrap_or(&"arg").to_string();

        let type_tokens: Vec<&str> = filtered[..name_idx].iter().filter(|t| **t != "*").copied().collect();
        let c_type = if type_tokens.is_empty() { "int".to_string() } else { type_tokens.join(" ") };

        self.data.parameters.push((name, c_type, is_pointer, has_brackets));
    }

    fn convert_body(&mut self) -> String {
        system().indent_push();
        let mut lines: Vec<String> = Vec::new();
        let body_tokens = self.data.body_tokens.clone();
        let token_strs: Vec<String> = body_tokens.iter().map(|t| t.to_string()).collect();

        let mut i = 0;
        while i < token_strs.len() {
            let token = &token_strs[i];

            // Skip comments
            if token.starts_with("//") || token.starts_with("/*") {
                lines.push(token.clone());
                i += 1;
                continue;
            }

            // Control structures
            match token.as_str() {
                "for" | "while" => {
                    if let Some((end_idx, loop_tokens)) = self.extract_control_block(&body_tokens, i) {
                        lines.push(self.route_to_loop_handler(&loop_tokens));
                        i = end_idx + 1;
                        continue;
                    }
                }
                "do" => {
                    if let Some((end_idx, loop_tokens)) = self.extract_do_while_block(&body_tokens, i) {
                        lines.push(self.route_to_loop_handler(&loop_tokens));
                        i = end_idx + 1;
                        continue;
                    }
                }
                "if" => {
                    if let Some((end_idx, if_tokens)) = self.extract_if_else_block(&body_tokens, i) {
                        lines.push(self.route_to_if_handler(&if_tokens));
                        i = end_idx + 1;
                        continue;
                    }
                }
                "switch" => {
                    if let Some((end_idx, switch_tokens)) = self.extract_control_block(&body_tokens, i) {
                        lines.push(self.route_to_switch_handler(&switch_tokens));
                        i = end_idx + 1;
                        continue;
                    }
                }
                _ => {}
            }

            // Regular statement
            let mut current: Vec<&str> = Vec::new();
            while i < token_strs.len() {
                current.push(&token_strs[i]);
                if token_strs[i] == ";" { i += 1; break; }
                i += 1;
            }

            if !current.is_empty() {
                let stmt = self.convert_statement(&current);
                if !stmt.is_empty() { lines.push(stmt); }
            }
        }

        let indent = system().indent_str();
        let result = if lines.is_empty() {
            format!("{}todo!()", indent)
        } else {
            lines.iter().map(|l| if l.contains('\n') { l.clone() } else { format!("{}{}", indent, l) })
                .collect::<Vec<_>>().join("\n")
        };

        system().indent_pop();
        result
    }

    fn extract_control_block(&self, tokens: &[Token], start: usize) -> Option<(usize, Vec<Token>)> {
        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        let mut i = start;
        while i < token_strs.len() && token_strs[i] != "{" { i += 1; }
        if i >= token_strs.len() { return None; }

        let mut brace_depth = 0;
        let mut end = i;
        while end < token_strs.len() {
            match token_strs[end].as_str() {
                "{" => brace_depth += 1,
                "}" => { brace_depth -= 1; if brace_depth == 0 { return Some((end, tokens[start..=end].to_vec())); } }
                _ => {}
            }
            end += 1;
        }
        None
    }

    fn extract_do_while_block(&self, tokens: &[Token], start: usize) -> Option<(usize, Vec<Token>)> {
        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        let mut i = start + 1;
        while i < token_strs.len() && token_strs[i] != "{" { i += 1; }
        if i >= token_strs.len() { return None; }

        let mut brace_depth = 0;
        let mut end = i;
        while end < token_strs.len() {
            match token_strs[end].as_str() {
                "{" => brace_depth += 1,
                "}" => { brace_depth -= 1; if brace_depth == 0 { break; } }
                _ => {}
            }
            end += 1;
        }

        while end < token_strs.len() && token_strs[end] != ";" { end += 1; }
        if end < token_strs.len() { Some((end, tokens[start..=end].to_vec())) } else { None }
    }

    fn extract_if_else_block(&self, tokens: &[Token], start: usize) -> Option<(usize, Vec<Token>)> {
        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        let mut i = start;
        while i < token_strs.len() && token_strs[i] != "{" { i += 1; }
        if i >= token_strs.len() { return None; }

        let mut brace_depth = 0;
        let mut end = i;
        while end < token_strs.len() {
            match token_strs[end].as_str() {
                "{" => brace_depth += 1,
                "}" => { brace_depth -= 1; if brace_depth == 0 { break; } }
                _ => {}
            }
            end += 1;
        }

        // Check for else/else-if
        while end + 1 < token_strs.len() && token_strs[end + 1] == "else" {
            end += 1;
            if end + 1 < token_strs.len() && token_strs[end + 1] == "if" { end += 1; }
            while end < token_strs.len() && token_strs[end] != "{" { end += 1; }
            if end >= token_strs.len() { break; }
            brace_depth = 0;
            while end < token_strs.len() {
                match token_strs[end].as_str() {
                    "{" => brace_depth += 1,
                    "}" => { brace_depth -= 1; if brace_depth == 0 { break; } }
                    _ => {}
                }
                end += 1;
            }
        }

        Some((end, tokens[start..=end].to_vec()))
    }

    // ========================================================================
    // Routing Methods
    // ========================================================================

    fn route_to_loop_handler(&self, tokens: &[Token]) -> String {
        let routing_id = create_routing("FunctionHandler", "LoopHandler", 0..tokens.len(), tokens.to_vec(), "nested loop");
        let mut handler = LoopHandler::new();
        if handler.validate(tokens) && handler.extract(tokens) {
            if let Some(code) = handler.convert() {
                set_routing_result(routing_id, code.clone(), handler.confidence(), "LoopHandler");
                handler.register(); // Register nested handler
                return code;
            }
        }
        format!("// loop: {}", tokens.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(" "))
    }

    fn route_to_if_handler(&self, tokens: &[Token]) -> String {
        let routing_id = create_routing("FunctionHandler", "IfElseHandler", 0..tokens.len(), tokens.to_vec(), "nested if/else");
        let mut handler = IfElseHandler::new();
        if handler.validate(tokens) && handler.extract(tokens) {
            if let Some(code) = handler.convert() {
                set_routing_result(routing_id, code.clone(), handler.confidence(), "IfElseHandler");
                handler.register(); // Register nested handler
                return code;
            }
        }
        format!("// if: {}", tokens.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(" "))
    }

    fn route_to_switch_handler(&self, tokens: &[Token]) -> String {
        let routing_id = create_routing("FunctionHandler", "SwitchCaseHandler", 0..tokens.len(), tokens.to_vec(), "nested switch");
        let mut handler = SwitchCaseHandler::new();
        if handler.validate(tokens) && handler.extract(tokens) {
            if let Some(code) = handler.convert() {
                set_routing_result(routing_id, code.clone(), handler.confidence(), "SwitchCaseHandler");
                handler.register(); // Register nested handler
                return code;
            }
        }
        format!("// switch: {}", tokens.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(" "))
    }

    fn route_to_expression_handler(&self, tokens: &[Token]) -> String {
        use crate::handlers::expressions::ExpressionHandler;
        let routing_id = create_routing("FunctionHandler", "expression", 0..tokens.len(), tokens.to_vec(), "expression");
        let mut handler = ExpressionHandler::new();
        if handler.validate(tokens) && handler.extract(tokens) {
            if let Some(code) = handler.convert() {
                set_routing_result(routing_id, code.clone(), handler.confidence(), "ExpressionHandler");
                handler.register(); // Register nested handler
                return code;
            }
        }
        tokens.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(" ")
    }

    fn route_to_array_handler(&self, tokens: &[Token]) -> String {
        let routing_id = create_routing("FunctionHandler", "array", 0..tokens.len(), tokens.to_vec(), "array declaration");
        let mut handler = ArrayHandler::new();
        if handler.validate(tokens) && handler.extract(tokens) {
            if let Some(code) = handler.convert() {
                set_routing_result(routing_id, code.clone(), handler.confidence(), "ArrayHandler");
                handler.register(); // Register nested handler
                return code;
            }
        }
        format!("// array: {}", tokens.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(" "))
    }

    fn route_to_variable_handler(&self, tokens: &[Token]) -> String {
        let routing_id = create_routing("FunctionHandler", "variable", 0..tokens.len(), tokens.to_vec(), "variable declaration");
        let mut handler = VariableHandler::new();
        if handler.validate(tokens) && handler.extract(tokens) {
            if let Some(code) = handler.convert() {
                set_routing_result(routing_id, code.clone(), handler.confidence(), "VariableHandler");
                handler.register(); // Register nested handler
                return code;
            }
        }
        // VariableHandler failed - try ExpressionHandler (for pointer dereference assignments, etc.)
        let expr_result = self.route_to_expression_handler(tokens);
        if !expr_result.is_empty() {
            // ExpressionHandler result may already end with semicolon
            if expr_result.ends_with(';') {
                return expr_result;
            }
            return format!("{};", expr_result);
        }
        format!("// var: {}", tokens.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(" "))
    }

    fn route_to_string_format_handler(&self, tokens: &[Token]) -> String {
        use crate::handlers::expressions::StringFormatHandler;
        let routing_id = create_routing("FunctionHandler", "string_format", 0..tokens.len(), tokens.to_vec(), "printf-style call");
        let mut handler = StringFormatHandler::new();
        if handler.validate(tokens) && handler.extract(tokens) {
            if let Some(code) = handler.convert() {
                set_routing_result(routing_id, code.clone(), handler.confidence(), "StringFormatHandler");
                handler.register(); // Register nested handler
                return format!("{};", code);
            }
        }
        format!("// printf: {};", tokens.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(" "))
    }

    fn route_to_call_handler(&self, tokens: &[Token]) -> String {
        let routing_id = create_routing("FunctionHandler", "call", 0..tokens.len(), tokens.to_vec(), "function call");
        let mut handler = CallHandler::new();
        if handler.validate(tokens) && handler.extract(tokens) {
            if let Some(code) = handler.convert() {
                set_routing_result(routing_id, code.clone(), handler.confidence(), "CallHandler");
                handler.register(); // Register nested handler
                return format!("{};", code);
            }
        }
        format!("// call: {};", tokens.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(" "))
    }

    // ========================================================================
    // Statement Conversion
    // ========================================================================

    fn convert_statement(&mut self, tokens: &[&str]) -> String {
        if tokens.is_empty() { return String::new(); }

        let first = *tokens.first().unwrap_or(&"");

        // Return statement
        if first == "return" {
            let expr_tokens: Vec<Token> = tokens[1..].iter()
                .filter(|t| **t != ";")
                .map(|s| Token::from_string(*s))
                .collect();
            let converted = self.route_to_expression_handler(&expr_tokens);
            return format!("return {};", converted);
        }

        // Check for increment/decrement operators - these are expressions, not declarations
        let has_increment = tokens.iter().any(|t| matches!(*t, "++" | "--"));
        
        // If increment/decrement, route directly to expression handler
        if has_increment {
            let expr_tokens: Vec<Token> = tokens.iter()
                .filter(|t| **t != ";")
                .map(|s| Token::from_string(*s))
                .collect();
            if !expr_tokens.is_empty() {
                let code = self.route_to_expression_handler(&expr_tokens);
                return format!("{};", code);
            }
        }

        // Check for compound assignment operators - these are expressions, not declarations
        let has_compound_assign = tokens.iter().any(|t| 
            matches!(*t, "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>=")
        );
        
        // If compound assignment, route directly to expression handler
        if has_compound_assign {
            let expr_tokens: Vec<Token> = tokens.iter()
                .filter(|t| **t != ";")
                .map(|s| Token::from_string(*s))
                .collect();
            if !expr_tokens.is_empty() {
                let code = self.route_to_expression_handler(&expr_tokens);
                return format!("{};", code);
            }
        }
        
        let has_equals = tokens.iter().any(|t| *t == "=");
        let has_paren = tokens.iter().position(|t| *t == "(");
        let bracket_pos = tokens.iter().position(|t| *t == "[");
        let eq_pos = tokens.iter().position(|t| *t == "=");
        let arrow_pos = tokens.iter().position(|t| *t == "->");
        let spaced_arrow = tokens.windows(2).position(|w| w[0] == "-" && w[1] == ">");

        // Array declaration: [ must appear before = (or no =) - this is type name[size]
        // If [ appears after =, it's an array access in an initializer (e.g., x = arr[i])
        // If -> appears before [, it's a member array access (e.g., ptr->field[i] = val)
        let has_arrow_before_bracket = match (arrow_pos.or(spaced_arrow), bracket_pos) {
            (Some(a), Some(b)) => a < b,
            _ => false,
        };
        let is_array_decl = match (bracket_pos, eq_pos) {
            (Some(b), Some(e)) => b < e && !has_arrow_before_bracket,
            (Some(_), None) => !has_arrow_before_bracket,
            _ => false,
        };

        if is_array_decl {
            let token_objs: Vec<Token> = tokens.iter().map(|s| Token::from_string(*s)).collect();
            return self.route_to_array_handler(&token_objs);
        }

        // Variable declaration with initialization (but NOT compound assignments)
        if has_equals && !has_compound_assign {
            let token_objs: Vec<Token> = tokens.iter().map(|s| Token::from_string(*s)).collect();
            return self.route_to_variable_handler(&token_objs);
        }

        // Variable declaration without initialization
        let is_type = crate::db::keyword::is_c_type_keyword(first)
            || system().lookup_type(first).is_some();

        if is_type && tokens.len() >= 2 && has_paren.is_none() {
            let token_objs: Vec<Token> = tokens.iter().map(|s| Token::from_string(*s)).collect();
            return self.route_to_variable_handler(&token_objs);
        }

        // Function call
        if let Some(paren_pos) = has_paren {
            if paren_pos > 0 && tokens.len() >= 3 {
                let token_objs: Vec<Token> = tokens.iter().map(|s| Token::from_string(*s)).collect();
                if matches!(first, "printf" | "fprintf" | "sprintf" | "snprintf" | "puts") {
                    return self.route_to_string_format_handler(&token_objs);
                }
                return self.route_to_call_handler(&token_objs);
            }
        }

        // Expression (assignments, increments, etc.)
        let expr_tokens: Vec<Token> = tokens.iter()
            .filter(|t| **t != ";")
            .map(|s| Token::from_string(*s))
            .collect();

        if !expr_tokens.is_empty() {
            let code = self.route_to_expression_handler(&expr_tokens);
            if !code.is_empty() { return format!("{};", code); }
        }

        format!("// {}", tokens.join(" "))
    }
}

// ============================================================================
// Build Trait
// ============================================================================

impl Build for FunctionHandler {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("Handler", "FunctionHandler");
        entry.set_attr("stage", Entry::string(self.stage.as_str()));
        entry.set_attr("confidence", Entry::f64(self.confidence));
        if let Some(ref output) = self.output { entry.set_attr("output", Entry::string(output)); }
        if let Some(ref error) = self.error { entry.set_attr("error", Entry::string(error)); }
        entry.set_attr("function_name", Entry::string(&self.data.name));
        entry.set_attr("return_type", Entry::string(&self.data.return_type));
        entry.set_attr("is_declaration", Entry::bool(self.data.is_declaration));
        entry
    }

    fn kind(&self) -> &str { "Handler" }
    fn name(&self) -> Option<&str> { Some("FunctionHandler") }
    fn category(&self) -> Option<&str> { Some("function") }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    fn make_tokens(strs: &[&str]) -> Vec<Token> {
        strs.iter().map(|s| Token::from_string(*s)).collect()
    }

    #[test]
    fn test_simple_function() {
        let tokens = make_tokens(&["int", "main", "(", ")", "{", "return", "0", ";", "}"]);
        let mut handler = FunctionHandler::new();
        assert!(handler.validate(&tokens));
        assert!(handler.extract(&tokens));
        assert_eq!(handler.data.name, "main");
        let output = handler.convert();
        assert!(output.is_some());
        let code = output.unwrap();
        assert!(code.contains("fn main()"));
        assert!(code.contains("-> i32"));
    }

    #[test]
    fn test_void_function() {
        let tokens = make_tokens(&["void", "setup", "(", ")", "{", "}"]);
        let mut handler = FunctionHandler::new();
        assert!(handler.validate(&tokens));
        assert!(handler.extract(&tokens));
        let output = handler.convert();
        assert!(output.is_some());
        assert!(!output.unwrap().contains("->"));
    }

    #[test]
    fn test_function_with_params() {
        let tokens = make_tokens(&["int", "add", "(", "int", "a", ",", "int", "b", ")", "{", "return", "a", "+", "b", ";", "}"]);
        let mut handler = FunctionHandler::new();
        assert!(handler.validate(&tokens));
        assert!(handler.extract(&tokens));
        assert_eq!(handler.data.parameters.len(), 2);
        let output = handler.convert();
        assert!(output.is_some());
        let code = output.unwrap();
        assert!(code.contains("a: i32"));
        assert!(code.contains("b: i32"));
    }

    #[test]
    fn test_static_function() {
        let tokens = make_tokens(&["static", "int", "helper", "(", ")", "{", "return", "1", ";", "}"]);
        let mut handler = FunctionHandler::new();
        assert!(handler.validate(&tokens));
        assert!(handler.extract(&tokens));
        assert!(handler.data.is_static);
        let output = handler.convert();
        assert!(output.is_some());
        assert!(!output.unwrap().contains("pub "));
    }

    #[test]
    fn test_inline_function() {
        let tokens = make_tokens(&["inline", "int", "square", "(", "int", "x", ")", "{", "return", "x", "*", "x", ";", "}"]);
        let mut handler = FunctionHandler::new();
        assert!(handler.validate(&tokens));
        assert!(handler.extract(&tokens));
        assert!(handler.data.is_inline);
        let output = handler.convert();
        assert!(output.is_some());
        assert!(output.unwrap().contains("#[inline]"));
    }

    #[test]
    fn test_function_declaration() {
        let tokens = make_tokens(&["void", "forward_decl", "(", "int", "x", ")", ";"]);
        let mut handler = FunctionHandler::new();
        assert!(handler.validate(&tokens));
        assert!(handler.extract(&tokens));
        assert!(handler.data.is_declaration);
        let output = handler.convert();
        assert!(output.is_some());
        assert!(output.unwrap().ends_with(";"));
    }
}
