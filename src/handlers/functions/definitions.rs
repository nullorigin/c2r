//! Function Definition Handler
//!
//! Converts C function definitions to Rust function definitions.
//! This handles the full function structure including signature and body.

use crate::db::pattern::{Pattern, PatternRule};
use crate::db::routing::{create_routing, set_routing_result};
use crate::db::token::Token;
use crate::db::web::{Build, Entry};
use crate::handlers::arrays::ArrayHandler;
use crate::handlers::conditionals::{IfElseHandler, SwitchCaseHandler};
use crate::handlers::functions::CallHandler;
use crate::handlers::loops::LoopHandler;
use crate::handlers::process::{ProcessStage, Processor};
use crate::handlers::variables::VariableHandler;
use crate::system;

// ============================================================================
// Function Definition Handler Implementation
// ============================================================================

/// Extracted function definition information
#[derive(Debug, Clone, Default)]
pub struct DefinitionData {
    /// Function name
    pub name: String,
    /// Return type (C type)
    pub return_type: String,
    /// Parameters: (name, type, is_pointer, is_array)
    pub parameters: Vec<(String, String, bool, bool)>,
    /// Function body tokens (between { and })
    pub body_tokens: Vec<Token>,
    /// Whether this is static
    pub is_static: bool,
    /// Whether this is inline
    pub is_inline: bool,
    /// Whether this is extern
    pub is_extern: bool,
}

/// Handler for C function definitions
#[derive(Debug)]
pub struct DefinitionHandler {
    stage: ProcessStage,
    confidence: f64,
    error: Option<String>,
    output: Option<String>,
    data: DefinitionData,
}

impl Default for DefinitionHandler {
    fn default() -> Self {
        Self::new()
    }
}

impl Processor for DefinitionHandler {
    fn name(&self) -> &str {
        "DefinitionHandler"
    }

    fn supported_patterns(&self) -> &[&str] {
        &[
            "validate_function_definition",
            "extract_function_definition",
            "validate_static_function_definition",
            "extract_static_function_definition",
            "validate_inline_function_definition",
            "extract_inline_function_definition",
            "validate_static_inline_function_definition",
            "extract_static_inline_function_definition",
        ]
    }

    fn patterns(&self) -> Vec<(Pattern, Pattern)> {
        vec![
            // Standard function: type name(...) {
            (
                Pattern::definition(
                    600,
                    "validate_function_definition",
                    vec![
                        PatternRule::type_keyword(),
                        PatternRule::identifier(),
                        PatternRule::exact("("),
                        PatternRule::any(),
                        PatternRule::exact(")"),
                        PatternRule::exact("{"),
                    ],
                )
                .with_category("function_definition")
                .with_priority(150)
                .with_min_tokens(6)
                .with_description("Function definition with body"),
                Pattern::definition(
                    600,
                    "extract_function_definition",
                    vec![
                        PatternRule::type_keyword().with_extract(|rule, ctx| {
                            let token = ctx.current_token.clone();
                            if ctx.did_match {
                                ctx.set_value("return_type", &token);
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
                        PatternRule::exact("(").with_extract(|rule, ctx| {
                            ctx.set_flag("in_params", true);
                            rule.clone()
                        }),
                        PatternRule::any().with_extract(|rule, ctx| {
                            let token = ctx.current_token.clone();
                            if ctx.did_match && ctx.flag("in_params") {
                                ctx.push_list("params", &token);
                            }
                            rule.clone()
                        }),
                        PatternRule::exact(")").with_extract(|rule, ctx| {
                            ctx.set_flag("in_params", false);
                            rule.clone()
                        }),
                        PatternRule::exact("{"),
                    ],
                )
                .with_category("function_definition")
                .with_priority(150)
                .with_min_tokens(6)
                .with_description("Function definition with body"),
            ),
            // Static function: static type name(...) {
            (
                Pattern::definition(
                    601,
                    "validate_static_function_definition",
                    vec![
                        PatternRule::exact("static"),
                        PatternRule::type_keyword(),
                        PatternRule::identifier(),
                        PatternRule::exact("("),
                        PatternRule::any(),
                        PatternRule::exact(")"),
                        PatternRule::exact("{"),
                    ],
                )
                .with_category("function_definition")
                .with_priority(155)
                .with_min_tokens(7)
                .with_description("Static function definition"),
                Pattern::definition(
                    601,
                    "extract_static_function_definition",
                    vec![
                        PatternRule::exact("static").with_extract(|rule, ctx| {
                            ctx.add_modifier("static");
                            rule.clone()
                        }),
                        PatternRule::type_keyword().with_extract(|rule, ctx| {
                            let token = ctx.current_token.clone();
                            if ctx.did_match {
                                ctx.set_value("return_type", &token);
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
                        PatternRule::exact("(").with_extract(|rule, ctx| {
                            ctx.set_flag("in_params", true);
                            rule.clone()
                        }),
                        PatternRule::any().with_extract(|rule, ctx| {
                            let token = ctx.current_token.clone();
                            if ctx.did_match && ctx.flag("in_params") {
                                ctx.push_list("params", &token);
                            }
                            rule.clone()
                        }),
                        PatternRule::exact(")").with_extract(|rule, ctx| {
                            ctx.set_flag("in_params", false);
                            rule.clone()
                        }),
                        PatternRule::exact("{"),
                    ],
                )
                .with_category("function_definition")
                .with_priority(155)
                .with_min_tokens(7)
                .with_description("Static function definition"),
            ),
            // Inline function: inline type name(...) {
            (
                Pattern::definition(
                    602,
                    "validate_inline_function_definition",
                    vec![
                        PatternRule::one_of(vec![
                            "inline".to_string(),
                            "__inline".to_string(),
                            "__inline__".to_string(),
                        ]),
                        PatternRule::type_keyword(),
                        PatternRule::identifier(),
                        PatternRule::exact("("),
                        PatternRule::any(),
                        PatternRule::exact(")"),
                        PatternRule::exact("{"),
                    ],
                )
                .with_category("function_definition")
                .with_priority(155)
                .with_min_tokens(7)
                .with_description("Inline function definition"),
                Pattern::definition(
                    602,
                    "extract_inline_function_definition",
                    vec![
                        PatternRule::one_of(vec![
                            "inline".to_string(),
                            "__inline".to_string(),
                            "__inline__".to_string(),
                        ])
                        .with_extract(|rule, ctx| {
                            ctx.add_modifier("inline");
                            rule.clone()
                        }),
                        PatternRule::type_keyword().with_extract(|rule, ctx| {
                            let token = ctx.current_token.clone();
                            if ctx.did_match {
                                ctx.set_value("return_type", &token);
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
                        PatternRule::exact("(").with_extract(|rule, ctx| {
                            ctx.set_flag("in_params", true);
                            rule.clone()
                        }),
                        PatternRule::any().with_extract(|rule, ctx| {
                            let token = ctx.current_token.clone();
                            if ctx.did_match && ctx.flag("in_params") {
                                ctx.push_list("params", &token);
                            }
                            rule.clone()
                        }),
                        PatternRule::exact(")").with_extract(|rule, ctx| {
                            ctx.set_flag("in_params", false);
                            rule.clone()
                        }),
                        PatternRule::exact("{"),
                    ],
                )
                .with_category("function_definition")
                .with_priority(155)
                .with_min_tokens(7)
                .with_description("Inline function definition"),
            ),
            // Static inline function: static inline type name(...) {
            (
                Pattern::definition(
                    603,
                    "validate_static_inline_function_definition",
                    vec![
                        PatternRule::exact("static"),
                        PatternRule::one_of(vec![
                            "inline".to_string(),
                            "__inline".to_string(),
                            "__inline__".to_string(),
                        ]),
                        PatternRule::type_keyword(),
                        PatternRule::identifier(),
                        PatternRule::exact("("),
                        PatternRule::any(),
                        PatternRule::exact(")"),
                        PatternRule::exact("{"),
                    ],
                )
                .with_category("function_definition")
                .with_priority(160)
                .with_min_tokens(8)
                .with_description("Static inline function definition"),
                Pattern::definition(
                    603,
                    "extract_static_inline_function_definition",
                    vec![
                        PatternRule::exact("static").with_extract(|rule, ctx| {
                            ctx.add_modifier("static");
                            rule.clone()
                        }),
                        PatternRule::one_of(vec![
                            "inline".to_string(),
                            "__inline".to_string(),
                            "__inline__".to_string(),
                        ])
                        .with_extract(|rule, ctx| {
                            ctx.add_modifier("inline");
                            rule.clone()
                        }),
                        PatternRule::type_keyword().with_extract(|rule, ctx| {
                            let token = ctx.current_token.clone();
                            if ctx.did_match {
                                ctx.set_value("return_type", &token);
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
                        PatternRule::exact("(").with_extract(|rule, ctx| {
                            ctx.set_flag("in_params", true);
                            rule.clone()
                        }),
                        PatternRule::any().with_extract(|rule, ctx| {
                            let token = ctx.current_token.clone();
                            if ctx.did_match && ctx.flag("in_params") {
                                ctx.push_list("params", &token);
                            }
                            rule.clone()
                        }),
                        PatternRule::exact(")").with_extract(|rule, ctx| {
                            ctx.set_flag("in_params", false);
                            rule.clone()
                        }),
                        PatternRule::exact("{"),
                    ],
                )
                .with_category("function_definition")
                .with_priority(160)
                .with_min_tokens(8)
                .with_description("Static inline function definition"),
            ),
        ]
    }

    fn validate(&mut self, tokens: &[Token]) -> bool {
        if tokens.len() < 6 {
            return false;
        }

        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        // Must have ( and )
        let open_paren = token_strs.iter().position(|t| t == "(");
        let close_paren = token_strs.iter().position(|t| t == ")");

        if open_paren.is_none() || close_paren.is_none() {
            return false;
        }

        // Must have { after ) - this distinguishes definition from declaration
        let close_pos = close_paren.unwrap();
        if close_pos + 1 >= token_strs.len() {
            return false;
        }

        // Look for { after )
        let has_body = token_strs[close_pos + 1..].iter().any(|t| t == "{");
        if !has_body {
            return false;
        }

        self.confidence = 0.8;
        true
    }

    fn extract(&mut self, tokens: &[Token]) -> bool {
        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        // Try extraction patterns first
        for (_, extract_pattern) in self.patterns() {
            if let Some(ctx) = extract_pattern.run_extraction(&token_strs) {
                // Get return type from pattern extraction
                if let Some(return_type) = ctx.value("return_type") {
                    self.data.return_type = return_type.to_string();
                }

                // Get function name from pattern extraction
                if let Some(name) = ctx.value("name") {
                    self.data.name = name.to_string();
                }

                // Check for modifiers
                if ctx.has_modifier("static") {
                    self.data.is_static = true;
                }
                if ctx.has_modifier("inline") {
                    self.data.is_inline = true;
                }
                if ctx.has_modifier("extern") {
                    self.data.is_extern = true;
                }

                // Get parameters from pattern extraction
                if let Some(params) = ctx.list("params") {
                    self.parse_parameters(&params.iter().map(|s| s.clone()).collect::<Vec<_>>());
                }

                if !self.data.name.is_empty() {
                    // Still need to extract body
                    let open_brace = token_strs.iter().position(|t| t == "{");
                    let close_brace = token_strs.iter().rposition(|t| t == "}");
                    if let (Some(ob), Some(cb)) = (open_brace, close_brace) {
                        if cb > ob + 1 {
                            self.data.body_tokens = tokens[ob + 1..cb].to_vec();
                        }
                    }
                    self.confidence = 0.85;
                    return true;
                }
            }
        }

        // Fallback to manual extraction
        let mut idx = 0;

        while idx < token_strs.len() {
            match token_strs[idx].as_str() {
                "static" => {
                    self.data.is_static = true;
                    idx += 1;
                }
                "inline" | "__inline" | "__inline__" => {
                    self.data.is_inline = true;
                    idx += 1;
                }
                "extern" => {
                    self.data.is_extern = true;
                    idx += 1;
                }
                _ => break,
            }
        }

        let paren_pos = match token_strs[idx..].iter().position(|t| t == "(") {
            Some(p) => idx + p,
            None => return false,
        };

        if paren_pos < idx + 1 {
            return false;
        }

        self.data.name = token_strs[paren_pos - 1].clone();

        if paren_pos > idx + 1 {
            self.data.return_type = token_strs[idx..paren_pos - 1].join(" ");
        } else {
            self.data.return_type = "void".to_string();
        }

        let close_paren = match token_strs.iter().position(|t| t == ")") {
            Some(p) => p,
            None => return false,
        };

        if close_paren > paren_pos + 1 {
            let param_tokens: Vec<String> = token_strs[paren_pos + 1..close_paren].to_vec();
            self.parse_parameters(&param_tokens);
        }

        let open_brace = token_strs.iter().position(|t| t == "{");
        let close_brace = token_strs.iter().rposition(|t| t == "}");

        if let (Some(ob), Some(cb)) = (open_brace, close_brace) {
            if cb > ob + 1 {
                self.data.body_tokens = tokens[ob + 1..cb].to_vec();
            }
        }

        self.confidence = 0.85;
        true
    }

    fn convert(&mut self) -> Option<String> {
        // Convert return type
        let rust_return_type =
            if self.data.return_type == "void" || self.data.return_type.is_empty() {
                "()".to_string()
            } else {system().lookup_type(&self.data.return_type)
                    .unwrap_or_else(|| self.data.return_type.clone())
            };
        let rust_return = if rust_return_type == "()" {
            String::new()
        } else {
            format!(" -> {}", rust_return_type)
        };

        // Convert parameters and collect for registration
        let mut param_info: Vec<(String, String)> = Vec::new();
        let params: Vec<String> = self
            .data
            .parameters
            .iter()
            .map(|(name, c_type, is_ptr, is_array)| {
                // Build full C type including pointer if applicable
                let full_c_type = if *is_ptr && !c_type.contains('*') {
                    format!("{} *", c_type)
                } else {
                    c_type.clone()
                };
                // Use TypeConverter for proper conversion (handles char* -> String, etc.)
                let base_rust_type = system().lookup_type(&full_c_type)
                    .unwrap_or_else(|| {
                        // Fallback: lookup base type and add pointer
                        let base = system().lookup_type(c_type).unwrap_or_else(|| c_type.clone());
                        if *is_ptr { format!("*const {}", base) } else { base }
                    });
                // Wrap in Vec if it's an array parameter
                let rust_type = if *is_array {
                    format!("Vec<{}>", base_rust_type)
                } else {
                    base_rust_type
                };
                param_info.push((name.clone(), rust_type.clone()));
                format!("{}: {}", name, rust_type)
            })
            .collect();
        let params_str = params.join(", ");

        // Register the function in the System database
        crate::system::system().register_function(
            &self.data.name,
            &self.data.name, // Keep same name for now (could convert to snake_case)
            &rust_return_type,
            param_info,
        );

        // Build function signature
        let visibility = if self.data.is_static { "" } else { "pub " };
        let inline_attr = if self.data.is_inline {
            "#[inline]\n"
        } else {
            ""
        };

        // Convert body
        let body = if self.data.body_tokens.is_empty() {
            "    todo!()".to_string()
        } else {
            self.convert_body()
        };

        let rust_code = format!(
            "{}{}fn {}({}){} {{\n{}\n}}",
            inline_attr, visibility, self.data.name, params_str, rust_return, body
        );

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

impl DefinitionHandler {

    pub fn new() -> Self {
        Self {
            stage: ProcessStage::Pending,
            confidence: 0.0,
            error: None,
            output: None,
            data: DefinitionData::default(),
        }
    }
    fn parse_parameters(&mut self, tokens: &[String]) {
        let mut current: Vec<&str> = Vec::new();

        for token in tokens {
            if token == "," {
                if !current.is_empty() {
                    self.add_parameter(&current);
                    current.clear();
                }
            } else {
                current.push(token);
            }
        }

        if !current.is_empty() {
            self.add_parameter(&current);
        }
    }

    fn add_parameter(&mut self, tokens: &[&str]) {
        if tokens.is_empty() || tokens == &["void"] {
            return;
        }

        let mut is_pointer = tokens.iter().any(|t| *t == "*");

        // Check for array parameter: type name[] or type name[size]
        let has_brackets = tokens.iter().any(|t| *t == "[");
        if has_brackets {
            is_pointer = true; // Arrays decay to pointers
        }

        // Filter out brackets and their contents for name finding
        let filtered: Vec<&str> = tokens
            .iter()
            .filter(|t| **t != "[" && **t != "]" && !t.parse::<usize>().is_ok())
            .copied()
            .collect();

        // Last non-* non-bracket token is the name
        let name_idx = filtered.iter().rposition(|t| *t != "*").unwrap_or(0);
        let name = filtered.get(name_idx).unwrap_or(&"arg").to_string();

        // Type is everything before the name, excluding * and brackets
        let type_tokens: Vec<&str> = filtered[..name_idx]
            .iter()
            .filter(|t| **t != "*")
            .copied()
            .collect();
        let c_type = if type_tokens.is_empty() {
            "int".to_string()
        } else {
            type_tokens.join(" ")
        };

        self.data.parameters.push((name, c_type, is_pointer, has_brackets));
    }

    fn convert_body(&mut self) -> String {

        // Push indent level for function body
        system().indent_push();

        let mut lines: Vec<String> = Vec::new();
        let body_tokens = self.data.body_tokens.clone();
        let token_strs: Vec<String> = body_tokens.iter().map(|t| t.to_string()).collect();

        // Segment body into statements, handling control structures
        let mut i = 0;
        while i < token_strs.len() {
            let token = &token_strs[i];

            // Skip comments - output them as-is
            if token.starts_with("//") || token.starts_with("/*") {
                lines.push(token.clone());
                i += 1;
                continue;
            }

            // Check for control structure keywords - route to appropriate handlers
            match token.as_str() {
                "for" | "while" => {
                    // Extract full loop construct and route to LoopHandler
                    if let Some((end_idx, loop_tokens)) =
                        self.extract_control_block(&body_tokens, i)
                    {
                        let converted = self.route_to_loop_handler(&loop_tokens);
                        lines.push(converted);
                        i = end_idx + 1;
                        continue;
                    }
                }
                "do" => {
                    // do-while loop - route to LoopHandler
                    if let Some((end_idx, loop_tokens)) =
                        self.extract_do_while_block(&body_tokens, i)
                    {
                        let converted = self.route_to_loop_handler(&loop_tokens);
                        lines.push(converted);
                        i = end_idx + 1;
                        continue;
                    }
                }
                "if" => {
                    // if/else construct - route to IfElseHandler
                    if let Some((end_idx, if_tokens)) = self.extract_if_else_block(&body_tokens, i)
                    {
                        let converted = self.route_to_if_handler(&if_tokens);
                        lines.push(converted);
                        i = end_idx + 1;
                        continue;
                    }
                }
                "switch" => {
                    // switch/case construct - route to SwitchCaseHandler
                    if let Some((end_idx, switch_tokens)) =
                        self.extract_control_block(&body_tokens, i)
                    {
                        let converted = self.route_to_switch_handler(&switch_tokens);
                        lines.push(converted);
                        i = end_idx + 1;
                        continue;
                    }
                }
                _ => {}
            }

            // Regular statement - collect until semicolon
            let mut current: Vec<&str> = Vec::new();
            while i < token_strs.len() {
                current.push(&token_strs[i]);
                if token_strs[i] == ";" {
                    i += 1;
                    break;
                }
                i += 1;
            }

            if !current.is_empty() {
                let stmt = self.convert_statement(&current);
                if !stmt.is_empty() {
                    lines.push(stmt);
                }
            }
        }

        let indent = system().indent_str();

        let result = if lines.is_empty() {
            format!("{}todo!()", indent)
        } else {
            lines
                .iter()
                .map(|l| {
                    // Check if this is a multi-line block (from a handler)
                    // If so, it already has correct indentation from System
                    if l.contains('\n') {
                        l.clone()
                    } else {
                        // Single line - add indent
                        format!("{}{}", indent, l)
                    }
                })
                .collect::<Vec<_>>()
                .join("\n")
        };

        // Pop indent level after body conversion
        system().indent_pop();
        result
    }

    /// Extract a control block (for, while, switch) - finds matching braces
    fn extract_control_block(&self, tokens: &[Token], start: usize) -> Option<(usize, Vec<Token>)> {
        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        // Find opening brace
        let mut i = start;
        while i < token_strs.len() && token_strs[i] != "{" {
            i += 1;
        }
        if i >= token_strs.len() {
            return None;
        }

        // Find matching closing brace
        let mut brace_depth = 0;
        let mut end = i;
        while end < token_strs.len() {
            match token_strs[end].as_str() {
                "{" => brace_depth += 1,
                "}" => {
                    brace_depth -= 1;
                    if brace_depth == 0 {
                        return Some((end, tokens[start..=end].to_vec()));
                    }
                }
                _ => {}
            }
            end += 1;
        }
        None
    }

    /// Extract do-while block: do { ... } while (cond);
    fn extract_do_while_block(
        &self,
        tokens: &[Token],
        start: usize,
    ) -> Option<(usize, Vec<Token>)> {
        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        // Find opening brace after 'do'
        let mut i = start + 1;
        while i < token_strs.len() && token_strs[i] != "{" {
            i += 1;
        }
        if i >= token_strs.len() {
            return None;
        }

        // Find matching closing brace
        let mut brace_depth = 0;
        let mut end = i;
        while end < token_strs.len() {
            match token_strs[end].as_str() {
                "{" => brace_depth += 1,
                "}" => {
                    brace_depth -= 1;
                    if brace_depth == 0 {
                        break;
                    }
                }
                _ => {}
            }
            end += 1;
        }

        // Continue to find while(...);
        while end < token_strs.len() && token_strs[end] != ";" {
            end += 1;
        }

        if end < token_strs.len() {
            Some((end, tokens[start..=end].to_vec()))
        } else {
            None
        }
    }

    /// Extract if/else block including chained else-if
    fn extract_if_else_block(&self, tokens: &[Token], start: usize) -> Option<(usize, Vec<Token>)> {
        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        // Find opening brace after condition
        let mut i = start;
        while i < token_strs.len() && token_strs[i] != "{" {
            i += 1;
        }
        if i >= token_strs.len() {
            return None;
        }

        // Find matching closing brace
        let mut brace_depth = 0;
        let mut end = i;
        while end < token_strs.len() {
            match token_strs[end].as_str() {
                "{" => brace_depth += 1,
                "}" => {
                    brace_depth -= 1;
                    if brace_depth == 0 {
                        break;
                    }
                }
                _ => {}
            }
            end += 1;
        }

        // Check for else/else-if
        while end + 1 < token_strs.len() && token_strs[end + 1] == "else" {
            end += 1; // skip else

            // Check for else-if
            if end + 1 < token_strs.len() && token_strs[end + 1] == "if" {
                end += 1; // skip if
                // Find brace
                while end < token_strs.len() && token_strs[end] != "{" {
                    end += 1;
                }
            }

            // Find opening brace
            while end < token_strs.len() && token_strs[end] != "{" {
                end += 1;
            }
            if end >= token_strs.len() {
                break;
            }

            // Find matching closing brace
            brace_depth = 0;
            while end < token_strs.len() {
                match token_strs[end].as_str() {
                    "{" => brace_depth += 1,
                    "}" => {
                        brace_depth -= 1;
                        if brace_depth == 0 {
                            break;
                        }
                    }
                    _ => {}
                }
                end += 1;
            }
        }

        Some((end, tokens[start..=end].to_vec()))
    }

    /// Route tokens to LoopHandler and get converted code
    fn route_to_loop_handler(&self, tokens: &[Token]) -> String {
        let routing_id = create_routing(
            "DefinitionHandler",
            "LoopHandler",
            0..tokens.len(),
            tokens.to_vec(),
            "nested loop in function body",
        );

        let mut handler = LoopHandler::new();
        if handler.validate(tokens) && handler.extract(tokens) {
            if let Some(code) = handler.convert() {
                set_routing_result(
                    routing_id,
                    code.clone(),
                    handler.confidence(),
                    "LoopHandler",
                );
                return code;
            }
        }

        let strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        format!("// loop: {}", strs.join(" "))
    }

    /// Route tokens to IfElseHandler and get converted code
    fn route_to_if_handler(&self, tokens: &[Token]) -> String {
        let routing_id = create_routing(
            "DefinitionHandler",
            "IfElseHandler",
            0..tokens.len(),
            tokens.to_vec(),
            "nested if/else in function body",
        );

        let mut handler = IfElseHandler::new();
        if handler.validate(tokens) && handler.extract(tokens) {
            if let Some(code) = handler.convert() {
                set_routing_result(
                    routing_id,
                    code.clone(),
                    handler.confidence(),
                    "IfElseHandler",
                );
                return code;
            }
        }

        let strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        format!("// if: {}", strs.join(" "))
    }

    /// Route tokens to SwitchCaseHandler and get converted code
    fn route_to_switch_handler(&self, tokens: &[Token]) -> String {
        let routing_id = create_routing(
            "DefinitionHandler",
            "SwitchCaseHandler",
            0..tokens.len(),
            tokens.to_vec(),
            "nested switch in function body",
        );

        let mut handler = SwitchCaseHandler::new();
        if handler.validate(tokens) && handler.extract(tokens) {
            if let Some(code) = handler.convert() {
                set_routing_result(
                    routing_id,
                    code.clone(),
                    handler.confidence(),
                    "SwitchCaseHandler",
                );
                return code;
            }
        }

        let strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        format!("// switch: {}", strs.join(" "))
    }

    fn convert_statement(&mut self, tokens: &[&str]) -> String {
        if tokens.is_empty() {
            return String::new();
        }

        // Return statement
        if tokens.first() == Some(&"return") {
            let expr_tokens: Vec<Token> = tokens[1..]
                .iter()
                .filter(|t| **t != ";")
                .map(|s| Token::from_string(*s))
                .collect();
            let converted_expr = self.route_to_expression_handler(&expr_tokens);
            return format!("return {};", converted_expr);
        }

        // Variable declaration with array
        if tokens.iter().any(|t| *t == "[") {
            let token_objs: Vec<Token> = tokens.iter().map(|s| Token::from_string(*s)).collect();
            return self.route_to_array_handler(&token_objs);
        }

        // Variable declaration with initialization
        if tokens.iter().any(|t| *t == "=") {
            let token_objs: Vec<Token> = tokens.iter().map(|s| Token::from_string(*s)).collect();
            return self.route_to_variable_handler(&token_objs);
        }

        // Variable declaration without initialization (e.g., "int j;")
        // Check if first token is a type keyword or registered user-defined type
        let first = tokens.first().map(|s| *s).unwrap_or("");
        let is_builtin_type = matches!(
            first,
            "int"
                | "char"
                | "short"
                | "long"
                | "float"
                | "double"
                | "unsigned"
                | "signed"
                | "void"
                | "size_t"
                | "ssize_t"
                | "uint8_t"
                | "uint16_t"
                | "uint32_t"
                | "uint64_t"
                | "int8_t"
                | "int16_t"
                | "int32_t"
                | "int64_t"
                | "bool"
        );
        // Also check if it's a registered user-defined type (struct, enum, typedef)
        let is_user_type = system::system().lookup_type(first).is_some();
        let is_type = is_builtin_type || is_user_type;
        if is_type && tokens.len() >= 2 && !tokens.iter().any(|t| *t == "(") {
            // Route to VariableHandler
            let token_objs: Vec<Token> = tokens.iter().map(|s| Token::from_string(*s)).collect();
            return self.route_to_variable_handler(&token_objs);
        }

        // Function call (identifier followed by parentheses)
        if tokens.len() >= 3 {
            if let Some(paren_pos) = tokens.iter().position(|t| *t == "(") {
                if paren_pos > 0 {
                    let token_objs: Vec<Token> = tokens.iter().map(|s| Token::from_string(*s)).collect();
                    // Check if this is a printf-style function - route to StringFormatHandler
                    let first = tokens.first().map(|s| *s).unwrap_or("");
                    if matches!(first, "printf" | "fprintf" | "sprintf" | "snprintf" | "puts") {
                        return self.route_to_string_format_handler(&token_objs);
                    }
                    return self.route_to_call_handler(&token_objs);
                }
            }
        }

        // Route expressions (assignments, increments, etc.) to ExpressionHandler
        let expr_tokens: Vec<Token> = tokens
            .iter()
            .filter(|t| **t != ";")
            .map(|s| Token::from_string(*s))
            .collect();

        if !expr_tokens.is_empty() {
            let code = self.route_to_expression_handler(&expr_tokens);
            if !code.is_empty() {
                return format!("{};", code);
            }
        }

        // Default: pass through as comment
        format!("// {}", tokens.join(" "))
    }

    /// Route expression to ExpressionHandler for conversion
    fn route_to_expression_handler(&self, tokens: &[Token]) -> String {
        use crate::db::routing::{create_routing, set_routing_result};
        use crate::handlers::expressions::ExpressionHandler;

        let routing_id = create_routing(
            "DefinitionHandler",
            "expression",
            0..tokens.len(),
            tokens.to_vec(),
            "expression in function body",
        );

        let mut handler = ExpressionHandler::new();
        if handler.validate(tokens) && handler.extract(tokens) {
            if let Some(code) = handler.convert() {
                set_routing_result(
                    routing_id,
                    code.clone(),
                    handler.confidence(),
                    "ExpressionHandler",
                );
                return code;
            }
        }

        // Fallback: join tokens
        let strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        strs.join(" ")
    }

    /// Route tokens to ArrayHandler and get converted code
    fn route_to_array_handler(&self, tokens: &[Token]) -> String {
        let routing_id = create_routing(
            "DefinitionHandler",
            "array",
            0..tokens.len(),
            tokens.to_vec(),
            "array declaration in function body",
        );

        // Run ArrayHandler directly
        let mut handler = ArrayHandler::new();
        if handler.validate(tokens) && handler.extract(tokens) {
            if let Some(code) = handler.convert() {
                set_routing_result(
                    routing_id,
                    code.clone(),
                    handler.confidence(),
                    "ArrayHandler",
                );
                return code;
            }
        }

        let strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        format!("// array: {}", strs.join(" "))
    }

    /// Route tokens to VariableHandler and get converted code
    fn route_to_variable_handler(&self, tokens: &[Token]) -> String {
        let routing_id = create_routing(
            "DefinitionHandler",
            "variable",
            0..tokens.len(),
            tokens.to_vec(),
            "variable declaration in function body",
        );

        // Run VariableHandler directly
        let mut handler = VariableHandler::new();
        if handler.validate(tokens) && handler.extract(tokens) {
            if let Some(code) = handler.convert() {
                set_routing_result(
                    routing_id,
                    code.clone(),
                    handler.confidence(),
                    "VariableHandler",
                );
                return code;
            }
        }

        let strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        format!("// var: {}", strs.join(" "))
    }

    /// Route tokens to StringFormatHandler and get converted code
    fn route_to_string_format_handler(&self, tokens: &[Token]) -> String {
        use crate::handlers::expressions::StringFormatHandler;

        let routing_id = create_routing(
            "DefinitionHandler",
            "string_format",
            0..tokens.len(),
            tokens.to_vec(),
            "printf-style function call",
        );

        // Run StringFormatHandler directly
        let mut handler = StringFormatHandler::new();
        if handler.validate(tokens) && handler.extract(tokens) {
            if let Some(code) = handler.convert() {
                set_routing_result(
                    routing_id,
                    code.clone(),
                    handler.confidence(),
                    "StringFormatHandler",
                );
                return format!("{};", code);
            }
        }

        let strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        format!("// printf: {};", strs.join(" "))
    }

    /// Route tokens to CallHandler and get converted code
    fn route_to_call_handler(&self, tokens: &[Token]) -> String {
        let routing_id = create_routing(
            "DefinitionHandler",
            "call",
            0..tokens.len(),
            tokens.to_vec(),
            "function call in function body",
        );

        // Run CallHandler directly
        let mut handler = CallHandler::new();
        if handler.validate(tokens) && handler.extract(tokens) {
            if let Some(code) = handler.convert() {
                set_routing_result(
                    routing_id,
                    code.clone(),
                    handler.confidence(),
                    "CallHandler",
                );
                return format!("{};", code);
            }
        }

        let strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        format!("// call: {};", strs.join(" "))
    }
}

impl Build for DefinitionHandler {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("Handler", "DefinitionHandler");
        entry.set_attr("stage", Entry::string(self.stage.as_str()));
        entry.set_attr("confidence", Entry::f64(self.confidence));
        if let Some(ref output) = self.output {
            entry.set_attr("output", Entry::string(output));
        }
        entry.set_attr("function_name", Entry::string(&self.data.name));
        entry.set_attr("return_type", Entry::string(&self.data.return_type));
        entry
    }

    fn kind(&self) -> &str {
        "Handler"
    }
    fn name(&self) -> Option<&str> {
        Some("DefinitionHandler")
    }
    fn category(&self) -> Option<&str> {
        Some("function")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_tokens(strs: &[&str]) -> Vec<Token> {
        strs.iter().map(|s| Token::from_string(*s)).collect()
    }

    #[test]
    fn test_simple_function() {
        let tokens = make_tokens(&["int", "main", "(", ")", "{", "return", "0", ";", "}"]);
        let mut handler = DefinitionHandler::new();

        assert!(handler.validate(&tokens));
        assert!(handler.extract(&tokens));
        assert_eq!(handler.data.name, "main");
        assert_eq!(handler.data.return_type, "int");

        let output = handler.convert();
        assert!(output.is_some());
        let code = output.unwrap();
        assert!(code.contains("fn main()"));
        assert!(code.contains("-> i32"));
    }

    #[test]
    fn test_void_function() {
        let tokens = make_tokens(&["void", "setup", "(", ")", "{", "}"]);
        let mut handler = DefinitionHandler::new();

        assert!(handler.validate(&tokens));
        assert!(handler.extract(&tokens));
        assert_eq!(handler.data.return_type, "void");

        let output = handler.convert();
        assert!(output.is_some());
        let code = output.unwrap();
        assert!(!code.contains("->"));
    }

    #[test]
    fn test_function_with_params() {
        let tokens = make_tokens(&[
            "int", "add", "(", "int", "a", ",", "int", "b", ")", "{", "return", "a", "+", "b", ";",
            "}",
        ]);
        let mut handler = DefinitionHandler::new();

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
        let tokens = make_tokens(&[
            "static", "int", "helper", "(", ")", "{", "return", "1", ";", "}",
        ]);
        let mut handler = DefinitionHandler::new();

        assert!(handler.validate(&tokens));
        assert!(handler.extract(&tokens));
        assert!(handler.data.is_static);

        let output = handler.convert();
        assert!(output.is_some());
        let code = output.unwrap();
        assert!(!code.contains("pub "));
    }

    #[test]
    fn test_inline_function() {
        let tokens = make_tokens(&[
            "inline", "int", "square", "(", "int", "x", ")", "{", "return", "x", "*", "x", ";", "}",
        ]);
        let mut handler = DefinitionHandler::new();

        assert!(handler.validate(&tokens));
        assert!(handler.extract(&tokens));
        assert!(handler.data.is_inline);

        let output = handler.convert();
        assert!(output.is_some());
        let code = output.unwrap();
        assert!(code.contains("#[inline]"));
    }
}
