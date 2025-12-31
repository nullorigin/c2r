//! Function Handler
//!
//! Converts C functions to Rust functions.
//! Handles both function declarations and definitions.

use crate::db::convert::sanitize_rust_identifier;
use crate::db::pattern::{Pattern, PatternRule};
use crate::db::token::Token;
use crate::db::web::{Build, Entry};
use crate::handlers::process::{ProcessorStage, Processor, ProcessorState, ProcessorStats};
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

impl Build for FunctionData {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("FunctionData", &self.name);
        entry.set_attr("name", Entry::string(&self.name));
        entry.set_attr("return_type", Entry::string(&self.return_type));
        entry.set_attr("is_declaration", Entry::bool(self.is_declaration));
        entry.set_attr("is_static", Entry::bool(self.is_static));
        entry.set_attr("is_inline", Entry::bool(self.is_inline));
        entry.set_attr("param_count", Entry::usize(self.parameters.len()));
        entry
    }

    fn kind(&self) -> &str { "FunctionData" }
    fn name(&self) -> Option<&str> { if self.name.is_empty() { None } else { Some(&self.name) } }
    fn category(&self) -> Option<&str> { Some("function") }
}

/// Handler for C function definitions and declarations
#[derive(Debug)]
pub struct FunctionHandler {
    /// Generic processor state
    state: ProcessorState<FunctionData>,
}

impl Default for FunctionHandler {
    fn default() -> Self { Self::new() }
}

impl FunctionHandler {
    pub fn new() -> Self {
        Self { state: ProcessorState::new("FunctionHandler") }
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

    fn stats(&self) -> &ProcessorStats {
        system().process_stats(&self.state.stats);
        &self.state.stats
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
        if tokens.len() < 5 {
            self.state.stats.record_failure(None, "validated");
            self.stats();
            return false;
        }

        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        // Reject typedefs - they should go to TypedefHandler
        if token_strs.first().map(|s| s.as_str()) == Some("typedef") {
            self.state.stats.record_failure(None, "validated");
            self.stats();
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
            self.state.data.is_declaration = !has_open_brace;
            // Adjust confidence based on C keyword ordering validity
            self.state.set_confidence(if c_order_valid { best_confidence } else { best_confidence - 0.1 });
            self.state.stats.record_success(self.state.confidence, tokens.len(), 0, Some("function"), "validated");
            self.stats();
            return true;
        }

        // Fallback to manual validation
        // Must have ( and )
        let open_paren = token_strs.iter().position(|t| t == "(");
        let close_paren = token_strs.iter().position(|t| t == ")");
        if open_paren.is_none() || close_paren.is_none() {
            self.state.stats.record_failure(Some("function"), "validated");
            self.stats();
            return false;
        }

        // Reject variable declarations with function call initializers
        // e.g., "size_t h_len = strlen(haystack);" has = before (
        let eq_pos = token_strs.iter().position(|t| t == "=");
        if let (Some(eq), Some(paren)) = (eq_pos, open_paren) {
            if eq < paren {
                // This is a variable declaration, not a function
                self.state.stats.record_failure(Some("function"), "validated");
                self.stats();
                return false;
            }
        }

        let close_pos = close_paren.unwrap();
        if close_pos + 1 >= token_strs.len() {
            self.state.stats.record_failure(Some("function"), "validated");
            self.stats();
            return false;
        }

        // Determine if declaration or definition
        let next_after_paren = &token_strs[close_pos + 1];
        self.state.data.is_declaration = next_after_paren == ";";

        let base_conf = if best_confidence > 0.0 { best_confidence } else { 0.75 };
        self.state.set_confidence(if c_order_valid { base_conf } else { base_conf - 0.1 });
        self.state.stats.record_success(self.state.confidence, tokens.len(), 0, Some("function"), "validated");
        self.stats();
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
                        self.state.data.return_type = format!("struct {}", struct_name);
                    }
                } else if let Some(return_type) = ctx.value("return_type") {
                    self.state.data.return_type = return_type.to_string();
                }
                if let Some(name) = ctx.value("name") { self.state.data.name = name.to_string(); }
                if ctx.has_modifier("static") { self.state.data.is_static = true; }
                if ctx.has_modifier("inline") { self.state.data.is_inline = true; }
                if ctx.has_modifier("extern") { self.state.data.is_extern = true; }
                if let Some(params) = ctx.list("params") {
                    self.parse_parameters(&params.iter().map(|s| s.clone()).collect::<Vec<_>>());
                }
                self.state.data.is_declaration = !ctx.flag("has_body");

                if !self.state.data.name.is_empty() && !self.state.data.is_declaration {
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
                        if cb > ob + 1 { self.state.data.body_tokens = tokens[ob + 1..cb].to_vec(); }
                    }
                }

                if !self.state.data.name.is_empty() {
                    self.state.set_confidence(0.85);
                    self.state.stats.record_success(self.state.confidence, 0, 0, Some("function"), "extracted");
                    self.stats();
                    return true;
                }
            }
        }

        // Fallback to manual extraction
        let mut idx = 0;
        while idx < token_strs.len() {
            match token_strs[idx].as_str() {
                "static" => { self.state.data.is_static = true; idx += 1; }
                "inline" | "__inline" | "__inline__" => { self.state.data.is_inline = true; idx += 1; }
                "extern" => { self.state.data.is_extern = true; idx += 1; }
                _ => break,
            }
        }

        let paren_pos = match token_strs[idx..].iter().position(|t| t == "(") {
            Some(p) => idx + p,
            None => return false,
        };

        if paren_pos < idx + 1 { return false; }

        self.state.data.name = token_strs[paren_pos - 1].clone();
        self.state.data.return_type = if paren_pos > idx + 1 {
            token_strs[idx..paren_pos - 1].join(" ")
        } else { "void".to_string() };

        let close_paren = match token_strs.iter().position(|t| t == ")") {
            Some(p) => p,
            None => return false,
        };

        if close_paren > paren_pos + 1 {
            self.parse_parameters(&token_strs[paren_pos + 1..close_paren].to_vec());
        }

        if !self.state.data.is_declaration {
            if let (Some(ob), Some(cb)) = (token_strs.iter().position(|t| t == "{"), token_strs.iter().rposition(|t| t == "}")) {
                if cb > ob + 1 { self.state.data.body_tokens = tokens[ob + 1..cb].to_vec(); }
            }
        }

        self.state.set_confidence(0.85);
        self.state.stats.record_success(self.state.confidence, 0, 0, Some("function"), "extracted");
        self.stats();
        true
    }

    fn convert(&mut self) -> Option<String> {
        // Convert return type
        let rust_return_type = if self.state.data.return_type == "void" || self.state.data.return_type.is_empty() {
            "()".to_string()
        } else {
            system().lookup_type(&self.state.data.return_type).unwrap_or_else(|| self.state.data.return_type.clone())
        };
        let rust_return = if rust_return_type == "()" { String::new() } else { format!(" -> {}", rust_return_type) };

        // Convert parameters and register them as variables for sizeof/type inference
        let mut param_info: Vec<(String, String)> = Vec::new();
        
        // Push a new scope for function parameters
        system().push_variable_scope();
        
        let params: Vec<String> = self.state.data.parameters.iter().map(|(name, c_type, is_ptr, is_array)| {
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
        system().register_function(&self.state.data.name, &self.state.data.name, &rust_return_type, param_info);

        // Build function
        let visibility = if self.state.data.is_static { "" } else { "pub " };
        let inline_attr = if self.state.data.is_inline { "#[inline]\n" } else { "" };

        let result = if self.state.data.is_declaration {
            self.state.set_confidence(0.9);
            Some(format!("{}{}fn {}({}){};", inline_attr, visibility, self.state.data.name, params_str, rust_return))
        } else {
            let body = if self.state.data.body_tokens.is_empty() { "    todo!()".to_string() } else { self.convert_body() };
            self.state.set_confidence(0.85);
            Some(format!("{}{}fn {}({}){} {{\n{}\n}}", inline_attr, visibility, self.state.data.name, params_str, rust_return, body))
        };
        
        // Pop the function scope (parameters go out of scope)
        system().pop_variable_scope();
        
        if result.is_some() {
            self.state.stats.record_success(self.state.confidence, 0, 0, Some("function"), "converted");
        } else {
            self.state.stats.record_failure(Some("function"), "converted");
        }
        self.stats();
        result
    }

    fn current_stage(&self) -> ProcessorStage { self.state.stage() }
    fn set_stage(&mut self, stage: ProcessorStage) { self.state.set_stage(stage); }
    fn output(&self) -> Option<String> { self.state.output.clone() }
    fn set_output(&mut self, output: String) { self.state.set_output(output); }
    fn error(&self) -> Option<String> { self.state.error.clone() }
    fn set_error(&mut self, error: String) { self.state.set_error(error); }
    fn confidence(&self) -> f64 { self.state.confidence }
    fn set_confidence(&mut self, confidence: f64) { self.state.set_confidence(confidence); }
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

        self.state.data.parameters.push((name, c_type, is_pointer, has_brackets));
    }

    fn convert_body(&mut self) -> String {
        system().indent_push();
        let mut lines: Vec<String> = Vec::new();
        let body_tokens = self.state.data.body_tokens.clone();
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
                        lines.push(self.route_statement(&loop_tokens, "FunctionHandler"));
                        i = end_idx + 1;
                        continue;
                    }
                }
                "do" => {
                    if let Some((end_idx, loop_tokens)) = self.extract_do_while_block(&body_tokens, i) {
                        lines.push(self.route_statement(&loop_tokens, "FunctionHandler"));
                        i = end_idx + 1;
                        continue;
                    }
                }
                "if" => {
                    if let Some((end_idx, if_tokens)) = self.extract_if_else_block(&body_tokens, i) {
                        lines.push(self.route_statement(&if_tokens, "FunctionHandler"));
                        i = end_idx + 1;
                        continue;
                    }
                }
                "switch" => {
                    if let Some((end_idx, switch_tokens)) = self.extract_control_block(&body_tokens, i) {
                        lines.push(self.route_statement(&switch_tokens, "FunctionHandler"));
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
                // Check for multi-variable declaration: type x = 1, y = 2, z = 3;
                // Split into separate statements
                let split_stmts = self.split_multi_var_declaration(&current);
                for stmt_tokens in split_stmts {
                    let token_objs: Vec<Token> = stmt_tokens.iter().map(|s| Token::from_string(s)).collect();
                    let stmt = self.route_statement(&token_objs, "FunctionHandler");
                    if !stmt.is_empty() { lines.push(stmt); }
                }
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
        
        // Find closing paren of condition
        let mut i = start;
        let mut paren_depth = 0;
        let mut found_open_paren = false;
        while i < token_strs.len() {
            match token_strs[i].as_str() {
                "(" => { paren_depth += 1; found_open_paren = true; }
                ")" => { 
                    paren_depth -= 1; 
                    if paren_depth == 0 && found_open_paren { 
                        i += 1; // Move past )
                        break; 
                    }
                }
                _ => {}
            }
            i += 1;
        }
        
        // Check what comes after the condition
        if i >= token_strs.len() { return None; }
        
        // Single-line if without braces: if (cond) statement;
        if token_strs[i] != "{" {
            // Find the semicolon ending this single statement
            while i < token_strs.len() && token_strs[i] != ";" { i += 1; }
            if i >= token_strs.len() { return None; }
            return Some((i, tokens[start..=i].to_vec()));
        }
        
        // Braced if statement

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

    /// Split multi-variable declarations like "int x = 10, y = 20, z = 30;" into separate statements
    fn split_multi_var_declaration<'a>(&self, tokens: &[&'a str]) -> Vec<Vec<String>> {
        // Check if this looks like a multi-variable declaration
        // Must start with a type keyword and contain commas outside of parentheses/braces
        if tokens.is_empty() {
            return vec![tokens.iter().map(|s| s.to_string()).collect()];
        }

        let first = tokens[0];
        if !crate::db::keyword::is_c_type_keyword(first) && 
           !matches!(first, "const" | "static" | "unsigned" | "signed" | "struct" | "enum") &&
           crate::system::system().lookup_type(first).is_none() {
            // Not a type declaration, return as-is
            return vec![tokens.iter().map(|s| s.to_string()).collect()];
        }

        // Find commas at depth 0 (not inside parens or braces)
        let mut comma_positions = Vec::new();
        let mut paren_depth: i32 = 0;
        let mut brace_depth: i32 = 0;
        for (i, t) in tokens.iter().enumerate() {
            match *t {
                "(" => paren_depth += 1,
                ")" => paren_depth = (paren_depth - 1).max(0),
                "{" => brace_depth += 1,
                "}" => brace_depth = (brace_depth - 1).max(0),
                "," if paren_depth == 0 && brace_depth == 0 => comma_positions.push(i),
                _ => {}
            }
        }

        if comma_positions.is_empty() {
            // No multi-variable declaration
            return vec![tokens.iter().map(|s| s.to_string()).collect()];
        }

        // Extract the base type (everything before first variable name and = or ,)
        let mut base_type = Vec::new();
        let mut type_end = 0;
        for (i, t) in tokens.iter().enumerate() {
            if *t == "=" || *t == "," {
                // Previous token is the variable name, everything before that is the type
                type_end = i.saturating_sub(1);
                break;
            }
        }
        if type_end > 0 {
            base_type = tokens[..type_end].iter().map(|s| s.to_string()).collect();
        }

        // Split into separate declarations
        let mut result = Vec::new();
        let mut start = type_end; // Start from the first variable name
        
        for comma_pos in &comma_positions {
            // Extract tokens from start to comma
            let mut decl: Vec<String> = base_type.clone();
            for i in start..*comma_pos {
                decl.push(tokens[i].to_string());
            }
            decl.push(";".to_string());
            result.push(decl);
            start = comma_pos + 1;
        }

        // Handle the last declaration (after the last comma)
        let mut decl: Vec<String> = base_type.clone();
        for i in start..tokens.len() {
            decl.push(tokens[i].to_string());
        }
        // Ensure it ends with semicolon
        if decl.last().map(|s| s.as_str()) != Some(";") {
            decl.push(";".to_string());
        }
        result.push(decl);

        result
    }
}

// ============================================================================
// Build Trait
// ============================================================================

impl Build for FunctionHandler {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("Handler", "FunctionHandler");
        entry.set_attr("stage", self.state.stage().to_entry());
        entry.set_attr("confidence", Entry::f64(self.state.confidence));
        if let Some(ref output) = self.state.output { entry.set_attr("output", Entry::string(output)); }
        if let Some(ref error) = self.state.error { entry.set_attr("error", Entry::string(error)); }
        entry.set_attr("function_name", Entry::string(&self.state.data.name));
        entry.set_attr("return_type", Entry::string(&self.state.data.return_type));
        entry.set_attr("is_declaration", Entry::bool(self.state.data.is_declaration));
        entry
    }

    fn kind(&self) -> &str { "Handler" }
    fn name(&self) -> Option<&str> { Some("FunctionHandler") }
    fn category(&self) -> Option<&str> { Some("function") }
}

// ============================================================================
// Function Call Handler
// ============================================================================

/// Extracted function call information
#[derive(Debug, Clone, Default)]
pub struct CallData {
    /// Function name being called
    pub name: String,
    /// Arguments as token strings
    pub arguments: Vec<Vec<String>>,
    /// Whether this is a method call (obj.method())
    pub is_method_call: bool,
    /// Object for method calls
    pub object: Option<String>,
}

impl Build for CallData {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("CallData", &self.name);
        entry.set_attr("name", Entry::string(&self.name));
        entry.set_attr("is_method_call", Entry::bool(self.is_method_call));
        entry.set_attr("arg_count", Entry::usize(self.arguments.len()));
        entry
    }
    fn kind(&self) -> &str { "CallData" }
    fn name(&self) -> Option<&str> { if self.name.is_empty() { None } else { Some(&self.name) } }
    fn category(&self) -> Option<&str> { Some("call") }
}

/// Handler for C function calls
#[derive(Debug)]
pub struct CallHandler {
    state: ProcessorState<CallData>,
}

impl CallHandler {
    pub fn new() -> Self {
        Self { state: ProcessorState::new("CallHandler") }
    }

    /// Convert function call from tokens (called from DefinitionHandler via routing)
    pub fn convert_from_tokens(tokens: &[Token]) -> String {
        let mut handler = Self::new();
        if handler.validate(tokens) && handler.extract(tokens) {
            handler.convert().unwrap_or_else(|| {
                let strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
                format!("// call: {}", strs.join(" "))
            })
        } else {
            let strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
            format!("// call: {}", strs.join(" "))
        }
    }

    fn parse_arguments(&mut self, tokens: &[String]) {
        let mut current: Vec<String> = Vec::new();
        let mut paren_depth = 0;

        for token in tokens {
            match token.as_str() {
                "(" => {
                    current.push(token.clone());
                    paren_depth += 1;
                }
                ")" => {
                    current.push(token.clone());
                    paren_depth -= 1;
                }
                "," if paren_depth == 0 => {
                    if !current.is_empty() {
                        self.state.data.arguments.push(std::mem::take(&mut current));
                    }
                }
                _ => {
                    current.push(token.clone());
                }
            }
        }

        if !current.is_empty() {
            self.state.data.arguments.push(current);
        }
    }

    fn convert_argument(&mut self, tokens: &[String]) -> String {
        let converted: Vec<String> = tokens
            .iter()
            .map(|t| system().lookup_identifier(t).unwrap_or_else(|| t.clone()))
            .collect();

        let joined = converted.join(" ").replace("& ", "&").replace("* ", "*");
        Self::convert_arrow_operator(&joined)
    }

    /// Convert C arrow operator (ptr->field or ptr -> field) to Rust (*ptr).field
    fn convert_arrow_operator(expr: &str) -> String {
        let mut result = expr.to_string();
        
        // Handle spaced arrow: "ptr -> field" -> "(*ptr).field"
        while let Some(arrow_pos) = result.find(" -> ") {
            let before = &result[..arrow_pos];
            let after = &result[arrow_pos + 4..];
            let ptr_name = before.split_whitespace().last().unwrap_or("");
            let prefix = if before.len() > ptr_name.len() {
                &before[..before.len() - ptr_name.len()]
            } else { "" };
            let field_end = after.find(|c: char| !c.is_alphanumeric() && c != '_').unwrap_or(after.len());
            let field = &after[..field_end];
            let suffix = &after[field_end..];
            result = format!("{}(*{}).{}{}", prefix, ptr_name, field, suffix);
        }
        
        // Handle non-spaced arrow: "ptr->field" -> "(*ptr).field"
        while let Some(arrow_pos) = result.find("->") {
            if arrow_pos > 0 && result.chars().nth(arrow_pos - 1) == Some(')') {
                break;
            }
            let before = &result[..arrow_pos];
            let after = &result[arrow_pos + 2..];
            let ptr_start = before.rfind(|c: char| !c.is_alphanumeric() && c != '_').map(|p| p + 1).unwrap_or(0);
            let ptr_name = &before[ptr_start..];
            let prefix = &before[..ptr_start];
            let field_end = after.find(|c: char| !c.is_alphanumeric() && c != '_').unwrap_or(after.len());
            let field = &after[..field_end];
            let suffix = &after[field_end..];
            result = format!("{}(*{}).{}{}", prefix, ptr_name, field, suffix);
        }
        
        result
    }
}

impl Default for CallHandler {
    fn default() -> Self { Self::new() }
}

impl Processor for CallHandler {
    fn supported_patterns(&self) -> &[&str] {
        &["validate_function_call", "extract_function_call"]
    }

    fn stats(&self) -> &ProcessorStats {
        system().process_stats(&self.state.stats);
        &self.state.stats
    }

    fn patterns(&self) -> Vec<(Pattern, Pattern)> {
        vec![(
            Pattern::definition(500, "validate_function_call", vec![
                PatternRule::identifier(),
                PatternRule::exact("("),
                PatternRule::any(),
                PatternRule::exact(")"),
            ])
            .with_category("call")
            .with_priority(60)
            .with_min_tokens(3)
            .with_description("Function call"),
            Pattern::definition(500, "extract_function_call", vec![
                PatternRule::identifier().with_extract(|rule, ctx| {
                    let token = ctx.current_token.clone();
                    if ctx.did_match { ctx.set_value("function_name", &token); }
                    rule.clone()
                }),
                PatternRule::exact("(").with_extract(|rule, ctx| {
                    ctx.set_flag("in_args", true);
                    rule.clone()
                }),
                PatternRule::any().with_extract(|rule, ctx| {
                    let token = ctx.current_token.clone();
                    if ctx.did_match && ctx.flag("in_args") { ctx.push_list("args", &token); }
                    rule.clone()
                }),
                PatternRule::exact(")").with_extract(|rule, ctx| {
                    ctx.set_flag("in_args", false);
                    rule.clone()
                }),
            ])
            .with_category("call")
            .with_priority(60)
            .with_min_tokens(3)
            .with_description("Function call"),
        )]
    }

    fn validate(&mut self, tokens: &[Token]) -> bool {
        if tokens.len() < 3 { return false; }

        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        let paren_pos = token_strs.iter().position(|t| t == "(");
        if paren_pos.is_none() || paren_pos == Some(0) { return false; }
        let paren_idx = paren_pos.unwrap();

        let before_paren: Vec<&str> = token_strs[..paren_idx].iter().map(|s| s.as_str()).collect();
        for kw in &before_paren {
            if matches!(*kw, "struct" | "enum" | "union" | "typedef") { return false; }
        }

        if before_paren.len() >= 2 {
            if let Some((can_follow, _)) = system().lookup_order_chain(before_paren.clone(), "c") {
                if !can_follow { self.state.set_confidence(0.5); }
            }
        }

        let close_paren = token_strs.iter().rposition(|t| t == ")");
        if close_paren.is_none() { return false; }

        let close_pos = close_paren.unwrap();
        if close_pos + 1 < token_strs.len() && token_strs[close_pos + 1] == "{" { return false; }

        if self.state.confidence == 0.0 { self.state.set_confidence(0.7); }
        true
    }

    fn extract(&mut self, tokens: &[Token]) -> bool {
        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        for (_, extract_pattern) in self.patterns() {
            if let Some(ctx) = extract_pattern.run_extraction(&token_strs) {
                if let Some(func_name) = ctx.value("function_name") {
                    self.state.data.name = func_name.to_string();
                }
                if let Some(args) = ctx.list("args") {
                    self.parse_arguments(args);
                }
                if !self.state.data.name.is_empty() {
                    self.state.set_confidence(0.8);
                    return true;
                }
            }
        }

        let paren_pos = match token_strs.iter().position(|t| t == "(") {
            Some(p) => p,
            None => return false,
        };

        if paren_pos >= 2 {
            let sep = &token_strs[paren_pos - 2];
            if sep == "." || sep == "->" {
                self.state.data.is_method_call = true;
                self.state.data.object = Some(token_strs[..paren_pos - 2].join(" "));
                self.state.data.name = token_strs[paren_pos - 1].clone();
            } else {
                self.state.data.name = token_strs[..paren_pos].join("");
            }
        } else {
            self.state.data.name = token_strs[..paren_pos].join("");
        }

        let close_paren = match token_strs.iter().rposition(|t| t == ")") {
            Some(p) => p,
            None => return false,
        };

        if close_paren > paren_pos + 1 {
            let arg_tokens = &token_strs[paren_pos + 1..close_paren];
            self.parse_arguments(arg_tokens);
        }

        self.state.set_confidence(0.8);
        true
    }

    fn convert(&mut self) -> Option<String> {
        let arguments = self.state.data.arguments.clone();
        let args: Vec<String> = arguments.iter().map(|arg| self.convert_argument(arg)).collect();
        let args_str = args.join(", ");

        let rust_code = if self.state.data.is_method_call {
            if let Some(ref obj) = self.state.data.object {
                format!("{}.{}({})", obj, self.state.data.name, args_str)
            } else {
                format!("{}({})", self.state.data.name, args_str)
            }
        } else {
            let func_name = system().lookup_identifier(&self.state.data.name)
                .unwrap_or_else(|| self.state.data.name.clone());
            format!("{}({})", func_name, args_str)
        };

        self.state.set_confidence(0.85);
        Some(rust_code)
    }

    fn current_stage(&self) -> ProcessorStage { self.state.stage() }
    fn set_stage(&mut self, stage: ProcessorStage) { self.state.set_stage(stage); }
    fn output(&self) -> Option<String> { self.state.output.clone() }
    fn set_output(&mut self, output: String) { self.state.set_output(output); }
    fn error(&self) -> Option<String> { self.state.error.clone() }
    fn set_error(&mut self, error: String) { self.state.set_error(error); }
    fn confidence(&self) -> f64 { self.state.confidence }
    fn set_confidence(&mut self, confidence: f64) { self.state.set_confidence(confidence); }
}

impl Build for CallHandler {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("Handler", "CallHandler");
        entry.set_attr("stage", self.state.stage().to_entry());
        entry.set_attr("confidence", Entry::f64(self.state.confidence));
        if let Some(ref output) = self.state.output {
            entry.set_attr("output", Entry::string(output));
        }
        entry.set_attr("function_name", Entry::string(&self.state.data.name));
        entry
    }
    fn kind(&self) -> &str { "Handler" }
    fn name(&self) -> Option<&str> { Some("CallHandler") }
    fn category(&self) -> Option<&str> { Some("call") }
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
    fn test_simple_call() {
        let tokens = make_tokens(&["foo", "(", ")"]);
        let mut handler = CallHandler::new();
        assert!(handler.validate(&tokens));
        assert!(handler.extract(&tokens));
        assert_eq!(handler.state.data.name, "foo");
        let output = handler.convert();
        assert!(output.is_some());
        assert_eq!(output.unwrap(), "foo()");
    }

    #[test]
    fn test_call_with_args() {
        let tokens = make_tokens(&["bar", "(", "x", ",", "y", ")"]);
        let mut handler = CallHandler::new();
        assert!(handler.validate(&tokens));
        assert!(handler.extract(&tokens));
        assert_eq!(handler.state.data.arguments.len(), 2);
        let output = handler.convert();
        assert!(output.is_some());
        assert_eq!(output.unwrap(), "bar(x, y)");
    }

    #[test]
    fn test_method_call() {
        let tokens = make_tokens(&["obj", ".", "method", "(", "arg", ")"]);
        let mut handler = CallHandler::new();
        assert!(handler.validate(&tokens));
        assert!(handler.extract(&tokens));
        assert!(handler.state.data.is_method_call);
        assert_eq!(handler.state.data.name, "method");
    }

    #[test]
    fn test_simple_function() {
        let tokens = make_tokens(&["int", "main", "(", ")", "{", "return", "0", ";", "}"]);
        let mut handler = FunctionHandler::new();
        assert!(handler.validate(&tokens));
        assert!(handler.extract(&tokens));
        assert_eq!(handler.state.data.name, "main");
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
        assert_eq!(handler.state.data.parameters.len(), 2);
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
        assert!(handler.state.data.is_static);
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
        assert!(handler.state.data.is_inline);
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
        assert!(handler.state.data.is_declaration);
        let output = handler.convert();
        assert!(output.is_some());
        assert!(output.unwrap().ends_with(";"));
    }
}
