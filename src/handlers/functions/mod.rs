//! Function Handler
//!
//! Converts C functions to Rust functions.

pub mod calls;
pub mod definitions;

pub use calls::CallHandler;
pub use definitions::DefinitionHandler;

use std::ops::Range;

use crate::db::convert::{IdentifierConverter, TypeConverter};
use crate::db::pattern::{Pattern, PatternRule};
use crate::db::routing::{create_routing, set_routing_result};
use crate::db::token::Token;
use crate::db::web::{Build, Entry};
use crate::handlers::conditionals::{IfElseHandler, SwitchCaseHandler};
use crate::handlers::loops::LoopHandler;
use crate::handlers::process::{ProcessStage, Processor};
use crate::handlers::validation::SequenceValidator;
use crate::system;

// ============================================================================
// Function Handler Implementation
// ============================================================================

/// Extracted function information
#[derive(Debug, Clone, Default)]
pub struct FunctionData {
    /// Function name
    pub name: String,
    /// Return type (C type)
    pub return_type: String,
    /// Parameters: (name, type, is_pointer, is_array)
    pub parameters: Vec<(String, String, bool, bool)>,
    /// Function body tokens
    pub body_tokens: Vec<Token>,
    /// Whether this is a declaration (no body)
    pub is_declaration: bool,
    /// Whether this is static
    pub is_static: bool,
    /// Whether this is inline
    pub is_inline: bool,
}

/// Handler for C function definitions and declarations
#[derive(Debug)]
pub struct FunctionHandler {
    stage: ProcessStage,
    confidence: f64,
    error: Option<String>,
    output: Option<String>,
    token_range: Option<Range<usize>>,
    data: FunctionData,
}

impl FunctionHandler {
    pub fn new() -> Self {
        Self {
            stage: ProcessStage::Pending,
            confidence: 0.0,
            error: None,
            output: None,
            token_range: None,
            data: FunctionData::default(),
        }
    }
}

impl Processor for FunctionHandler {
    fn name(&self) -> &str {
        "FunctionHandler"
    }

    fn supported_patterns(&self) -> &[&str] {
        &[
            "validate_function_definition",
            "extract_function_definition",
            "validate_function_declaration",
            "extract_function_declaration",
        ]
    }

    fn patterns(&self) -> Vec<(Pattern, Pattern)> {
        vec![
            (
                Pattern::definition(
                    200,
                    "validate_function_definition",
                    vec![
                        PatternRule::type_keyword(),
                        PatternRule::identifier(),
                        PatternRule::exact("("),
                        PatternRule::any_except(")").optional().repeat(-1),
                        PatternRule::exact(")"),
                        PatternRule::exact("{"),
                    ],
                )
                .with_category("function")
                .with_priority(920)
                .with_min_tokens(6)
                .with_description("Function definition with body"),
                Pattern::definition(
                    200,
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
                        PatternRule::any_except(")")
                            .optional()
                            .repeat(-1)
                            .with_extract(|rule, ctx| {
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
                        PatternRule::exact("{").with_extract(|rule, ctx| {
                            ctx.set_flag("has_body", true);
                            rule.clone()
                        }),
                    ],
                )
                .with_category("function")
                .with_priority(920)
                .with_min_tokens(6)
                .with_description("Function definition with body"),
            ),
            (
                Pattern::definition(
                    201,
                    "validate_function_declaration",
                    vec![
                        PatternRule::type_keyword(),
                        PatternRule::identifier(),
                        PatternRule::exact("("),
                        PatternRule::any_except(")").optional().repeat(-1),
                        PatternRule::exact(")"),
                        PatternRule::exact(";"),
                    ],
                )
                .with_category("function")
                .with_priority(910)
                .with_min_tokens(5)
                .with_description("Function declaration without body"),
                Pattern::definition(
                    201,
                    "extract_function_declaration",
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
                        PatternRule::any_except(")")
                            .optional()
                            .repeat(-1)
                            .with_extract(|rule, ctx| {
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
                        PatternRule::exact(";"),
                    ],
                )
                .with_category("function")
                .with_priority(910)
                .with_min_tokens(5)
                .with_description("Function declaration without body"),
            ),
        ]
    }

    fn validate(&mut self, tokens: &[Token]) -> bool {
        if tokens.len() < 4 {
            return false;
        }

        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        // Try matching against our patterns
        let mut best_confidence = 0.0;
        for (pattern, _) in self.patterns() {
            if let Some(confidence) = pattern.matches_tokens(&token_strs) {
                if confidence > best_confidence {
                    best_confidence = confidence;
                }
            }
        }

        // Basic structural validation
        let paren_pos = token_strs.iter().position(|t| t == "(");
        if paren_pos.is_none() || paren_pos.unwrap() < 2 {
            return false;
        }

        // If there's an = before (, this is a variable init with function call, not a function definition
        let equals_pos = token_strs.iter().position(|t| t == "=");
        if let Some(eq) = equals_pos {
            if eq < paren_pos.unwrap() {
                return false;
            }
        }

        let close_paren = token_strs.iter().position(|t| t == ")");
        if close_paren.is_none() {
            return false;
        }

        let after_paren = close_paren.unwrap() + 1;
        if after_paren >= token_strs.len() {
            return false;
        }

        let next = &token_strs[after_paren];
        if next != "{" && next != ";" {
            return false;
        }

        self.data.is_declaration = next == ";";

        // Use keyword validation to verify function sequence
        let validator = SequenceValidator::new();
        let validation_result = validator.validate_function(tokens);

        // Adjust confidence based on both pattern matching and keyword validation
        let base_confidence = if best_confidence > 0.0 {
            best_confidence
        } else {
            0.8
        };
        self.confidence = validation_result.adjust_confidence(base_confidence);

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

                // Get parameters from pattern extraction
                if let Some(params) = ctx.list("params") {
                    self.parse_parameters(params);
                }

                // Check if it's a declaration or definition
                if ctx.flag("has_body") {
                    self.data.is_declaration = false;
                } else {
                    self.data.is_declaration = true;
                }

                self.confidence = 0.85;
                return true;
            }
        }

        // Fallback to manual extraction
        let paren_pos = match token_strs.iter().position(|t| t == "(") {
            Some(p) => p,
            None => return false,
        };

        let close_paren = match token_strs.iter().position(|t| t == ")") {
            Some(p) => p,
            None => return false,
        };

        let mut idx = 0;

        if token_strs.get(idx) == Some(&"static".to_string()) {
            self.data.is_static = true;
            idx += 1;
        }

        if token_strs.get(idx) == Some(&"inline".to_string()) {
            self.data.is_inline = true;
            idx += 1;
        }

        let name_pos = if paren_pos > 0 && token_strs.get(paren_pos - 1) == Some(&"*".to_string()) {
            paren_pos - 1
        } else {
            paren_pos - 1
        };

        if name_pos <= idx {
            return false;
        }

        self.data.name = token_strs[name_pos].clone();

        let return_type_tokens: Vec<&str> = token_strs[idx..name_pos]
            .iter()
            .map(|s| s.as_str())
            .collect();
        self.data.return_type = return_type_tokens.join(" ");

        if close_paren > paren_pos + 1 {
            let param_tokens = &token_strs[paren_pos + 1..close_paren];
            self.parse_parameters(param_tokens);
        }

        if !self.data.is_declaration {
            // Find the opening brace after the closing paren
            let open_brace_pos = token_strs[close_paren..]
                .iter()
                .position(|t| t == "{")
                .map(|p| close_paren + p);

            if let Some(obp) = open_brace_pos {
                let body_start = obp + 1; // Start after the {
                if body_start < tokens.len() {
                    let mut brace_depth = 1;
                    let mut body_end = body_start;
                    for (i, t) in token_strs[body_start..].iter().enumerate() {
                        match t.as_str() {
                            "{" => brace_depth += 1,
                            "}" => {
                                brace_depth -= 1;
                                if brace_depth == 0 {
                                    body_end = body_start + i;
                                    break;
                                }
                            }
                            _ => {}
                        }
                    }
                    self.data.body_tokens = tokens[body_start..body_end].to_vec();
                }
            }
        }

        self.confidence = 0.85;
        true
    }

    fn convert(&mut self) -> Option<String> {
        // Convert return type
        let rust_return = if self.data.return_type == "void" {
            String::new()
        } else {
            let converted = system().lookup_type(&self.data.return_type)
                .unwrap_or_else(|| self.data.return_type.clone());
            format!(" -> {}", converted)
        };

        // Convert parameters
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
                // Try to look up the full type (including pointer)
                let base_rust_type = system().lookup_type(&full_c_type)
                    .unwrap_or_else(|| {
                        // Fallback: lookup base type and add pointer manually
                        let base = system().lookup_type(c_type).unwrap_or_else(|| c_type.clone());
                        if *is_ptr { format!("*mut {}", base) } else { base }
                    });
                // Wrap in Vec if it's an array parameter
                let rust_type = if *is_array {
                    format!("Vec<{}>", base_rust_type)
                } else {
                    base_rust_type
                };
                format!("{}: {}", name, rust_type)
            })
            .collect();
        let params_str = params.join(", ");

        // Build function signature
        let visibility = if self.data.is_static { "" } else { "pub " };

        if self.data.is_declaration {
            // Just a declaration
            let rust_code = format!(
                "{}fn {}({}){};",
                visibility, self.data.name, params_str, rust_return
            );
            self.confidence = 0.9;
            Some(rust_code)
        } else {
            // Convert function body statements
            let body = if self.data.body_tokens.is_empty() {
                "    todo!()".to_string()
            } else {
                self.convert_body()
            };
            let rust_code = format!(
                "{}fn {}({}){} {{\n{}\n}}",
                visibility, self.data.name, params_str, rust_return, body
            );
            self.confidence = 0.85;
            Some(rust_code)
        }
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

impl FunctionHandler {
    fn parse_parameters(&mut self, param_tokens: &[String]) {
        // Split by comma and parse each parameter
        let mut current_param: Vec<&str> = Vec::new();

        for token in param_tokens {
            if token == "," {
                if !current_param.is_empty() {
                    self.add_parameter(&current_param);
                    current_param.clear();
                }
            } else {
                current_param.push(token);
            }
        }

        // Don't forget the last parameter
        if !current_param.is_empty() {
            self.add_parameter(&current_param);
        }
    }

    fn add_parameter(&mut self, tokens: &[&str]) {
        if tokens.is_empty() || tokens == &["void"] {
            return;
        }

        // Check for array parameter: type name[] or type *name[]
        // Pattern: ends with ] or has [ in tokens
        let has_array = tokens.iter().any(|t| *t == "[" || *t == "]");
        let mut is_pointer = tokens.iter().any(|t| *t == "*");

        if has_array {
            // Find the name - it's before [
            let bracket_pos = tokens.iter().position(|t| *t == "[");
            if let Some(bp) = bracket_pos {
                // Name is just before [
                let name_idx = bp.saturating_sub(1);
                let name = tokens.get(name_idx).unwrap_or(&"arg").to_string();

                // Type is everything before name (excluding * and array parts)
                let type_tokens: Vec<&str> = tokens[..name_idx]
                    .iter()
                    .filter(|t| **t != "*")
                    .copied()
                    .collect();
                let c_type = type_tokens.join(" ");

                // Arrays decay to pointers in function parameters
                is_pointer = true;

                self.data.parameters.push((name, c_type, is_pointer, true));
                return;
            }
        }

        // Last token is the name (unless it's a pointer or ])
        let name_idx = if tokens.last() == Some(&"*") || tokens.last() == Some(&"]") {
            // Find first identifier from the end that's not a symbol
            let mut idx = tokens.len().saturating_sub(1);
            while idx > 0 {
                let t = tokens[idx];
                if t != "*" && t != "[" && t != "]" && !t.chars().all(|c| c.is_numeric()) {
                    break;
                }
                idx -= 1;
            }
            idx
        } else {
            tokens.len() - 1
        };

        let name = tokens.get(name_idx).unwrap_or(&"arg").to_string();
        let type_tokens: Vec<&str> = tokens[..name_idx]
            .iter()
            .filter(|t| **t != "*" && **t != "[" && **t != "]")
            .copied()
            .collect();
        let c_type = type_tokens.join(" ");

        self.data.parameters.push((name, c_type, is_pointer, false));
    }

    /// Convert function body tokens to Rust
    fn convert_body(&mut self) -> String {
        let mut lines: Vec<String> = Vec::new();
        let body_tokens = self.data.body_tokens.clone();
        let token_strs: Vec<String> = body_tokens.iter().map(|t| t.to_string()).collect();

        // Segment body into statements, handling control structures
        let mut i = 0;
        while i < token_strs.len() {
            let token = &token_strs[i];

            // Check for control structure keywords
            match token.as_str() {
                "for" | "while" => {
                    // Extract full loop construct
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
                    // do-while loop
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
                    // if/else construct
                    if let Some((end_idx, if_tokens)) = self.extract_if_else_block(&body_tokens, i)
                    {
                        let converted = self.route_to_if_handler(&if_tokens);
                        lines.push(converted);
                        i = end_idx + 1;
                        continue;
                    }
                }
                "switch" => {
                    // switch/case construct
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

        if lines.is_empty() {
            "    todo!()".to_string()
        } else {
            lines
                .iter()
                .map(|l| format!("    {}", l))
                .collect::<Vec<_>>()
                .join("\n")
        }
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
        let mut brace_end = i;
        while brace_end < token_strs.len() {
            match token_strs[brace_end].as_str() {
                "{" => brace_depth += 1,
                "}" => {
                    brace_depth -= 1;
                    if brace_depth == 0 {
                        break;
                    }
                }
                _ => {}
            }
            brace_end += 1;
        }

        // Find 'while' and then the semicolon
        let mut end = brace_end + 1;
        while end < token_strs.len() && token_strs[end] != ";" {
            end += 1;
        }

        if end < token_strs.len() {
            Some((end, tokens[start..=end].to_vec()))
        } else {
            None
        }
    }

    /// Extract if/else block including all else-if and else branches
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

        // Check for else/else-if after closing brace
        while end + 1 < token_strs.len() && token_strs[end + 1] == "else" {
            end += 1; // Move past 'else'

            // Skip 'if' if present (else-if)
            if end + 1 < token_strs.len() && token_strs[end + 1] == "if" {
                end += 1;
            }

            // Find the opening brace of else/else-if
            while end < token_strs.len() && token_strs[end] != "{" {
                end += 1;
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
        // Create routing decision in database
        let routing_id = create_routing(
            "FunctionHandler",
            "LoopHandler",
            0..tokens.len(),
            tokens.to_vec(),
            "nested loop in function body",
        );

        // Process with handler
        let mut handler = LoopHandler::new();
        if handler.validate(tokens) && handler.extract(tokens) {
            if let Some(code) = handler.convert() {
                // Store result in routing database
                set_routing_result(
                    routing_id,
                    code.clone(),
                    handler.confidence(),
                    "LoopHandler",
                );
                return code;
            }
        }

        // Fallback: comment out the original
        let strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        format!("// loop: {}", strs.join(" "))
    }

    /// Route tokens to IfElseHandler and get converted code
    fn route_to_if_handler(&self, tokens: &[Token]) -> String {
        // Create routing decision in database
        let routing_id = create_routing(
            "FunctionHandler",
            "IfElseHandler",
            0..tokens.len(),
            tokens.to_vec(),
            "nested if/else in function body",
        );

        // Process with handler
        let mut handler = IfElseHandler::new();
        if handler.validate(tokens) && handler.extract(tokens) {
            if let Some(code) = handler.convert() {
                // Store result in routing database
                set_routing_result(
                    routing_id,
                    code.clone(),
                    handler.confidence(),
                    "IfElseHandler",
                );
                return code;
            }
        }

        // Fallback: comment out the original
        let strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        format!("// if: {}", strs.join(" "))
    }

    /// Route tokens to SwitchCaseHandler and get converted code
    fn route_to_switch_handler(&self, tokens: &[Token]) -> String {
        // Create routing decision in database
        let routing_id = create_routing(
            "FunctionHandler",
            "SwitchCaseHandler",
            0..tokens.len(),
            tokens.to_vec(),
            "nested switch in function body",
        );

        // Process with handler
        let mut handler = SwitchCaseHandler::new();
        if handler.validate(tokens) && handler.extract(tokens) {
            if let Some(code) = handler.convert() {
                // Store result in routing database
                set_routing_result(
                    routing_id,
                    code.clone(),
                    handler.confidence(),
                    "SwitchCaseHandler",
                );
                return code;
            }
        }

        // Fallback: comment out the original
        let strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        format!("// switch: {}", strs.join(" "))
    }

    /// Convert a single statement
    fn convert_statement(&mut self, tokens: &[&str]) -> String {
        if tokens.is_empty() {
            return String::new();
        }

        // Check for return statement
        if tokens.first() == Some(&"return") {
            let expr: Vec<&str> = tokens[1..].iter().filter(|t| **t != ";").copied().collect();
            let converted_expr = self.convert_expression(&expr);
            return format!("return {};", converted_expr);
        }

        // Check for array declaration: type name [ size ] = { ... } ;
        if tokens.iter().any(|t| *t == "[") {
            return self.convert_array_decl(tokens);
        }

        // Check for variable declaration: type name = value ;
        if tokens.iter().any(|t| *t == "=") {
            return self.convert_var_decl(tokens);
        }

        // Check for function call: identifier ( ... ) ;
        if tokens.len() >= 3 {
            if let Some(paren_pos) = tokens.iter().position(|t| *t == "(") {
                if paren_pos > 0 {
                    return self.convert_call(tokens);
                }
            }
        }

        // Default: pass through as comment
        format!("// {}", tokens.join(" "))
    }

    /// Convert function call
    fn convert_call(&mut self, tokens: &[&str]) -> String {
        let paren_pos = tokens
            .iter()
            .position(|t| *t == "(")
            .unwrap_or(tokens.len());
        let func_name = if paren_pos > 0 { tokens[0] } else { "" };

        // Use IdentifierConverter for function name
        let rust_func = system().lookup_identifier(func_name)
            .unwrap_or_else(|| func_name.to_string());

        // Convert arguments
        let args: Vec<String> = tokens[paren_pos..]
            .iter()
            .filter(|t| **t != ";" && **t != "(" && **t != ")")
            .map(|t| {system().lookup_identifier(t)
                    .unwrap_or_else(|| t.to_string())
            })
            .collect();

        format!("{}({});", rust_func, args.join(", "))
    }

    /// Convert array declaration
    fn convert_array_decl(&self, tokens: &[&str]) -> String {
        // Find key positions
        let bracket_pos = tokens.iter().position(|t| *t == "[");
        let close_bracket = tokens.iter().position(|t| *t == "]");
        let equals_pos = tokens.iter().position(|t| *t == "=");

        if let (Some(bp), Some(cb)) = (bracket_pos, close_bracket) {
            let c_type = if bp > 0 { tokens[0] } else { "int" };
            let name = if bp > 1 { tokens[bp - 1] } else { "arr" };
            let size = if cb > bp + 1 { tokens[bp + 1] } else { "" };

            let rust_type = TypeConverter::new()
                .convert(c_type)
                .unwrap_or_else(|| c_type.to_string());

            // Check for initializer
            if let Some(eq) = equals_pos {
                // Find { and }
                let brace_start = tokens.iter().position(|t| *t == "{");
                let brace_end = tokens.iter().rposition(|t| *t == "}");

                if let (Some(bs), Some(be)) = (brace_start, brace_end) {
                    let init_tokens: Vec<&str> = tokens[bs..=be].to_vec();
                    let init = init_tokens
                        .join(" ")
                        .replace("{ ", "[")
                        .replace(" }", "]")
                        .replace("{", "[")
                        .replace("}", "]");

                    return format!("let {}: [{}; {}] = {};", name, rust_type, size, init);
                }
            }

            format!("let {}: [{}; {}];", name, rust_type, size)
        } else {
            format!("// array: {}", tokens.join(" "))
        }
    }

    /// Convert variable declaration
    fn convert_var_decl(&self, tokens: &[&str]) -> String {
        let eq_pos = tokens.iter().position(|t| *t == "=").unwrap_or(0);

        if eq_pos < 2 {
            return format!("// var: {}", tokens.join(" "));
        }

        let c_type = tokens[0];
        let name = tokens[eq_pos - 1];
        let value: Vec<&str> = tokens[eq_pos + 1..]
            .iter()
            .filter(|t| **t != ";")
            .copied()
            .collect();

        let rust_type = TypeConverter::new()
            .convert(c_type)
            .unwrap_or_else(|| c_type.to_string());

        // Convert the value expression (handles casts, etc.)
        let converted_value = self.convert_expression(&value);

        format!("let {}: {} = {};", name, rust_type, converted_value)
    }

    /// Convert a C expression to Rust, handling casts, operators, etc.
    fn convert_expression(&self, tokens: &[&str]) -> String {
        if tokens.is_empty() {
            return String::new();
        }

        let mut result = Vec::new();
        let mut i = 0;

        while i < tokens.len() {
            let token = tokens[i];

            // Check for C-style cast: ( type ) value
            if token == "(" && i + 2 < tokens.len() {
                // Look for pattern: ( type )
                let close_paren = tokens[i..].iter().position(|t| *t == ")");
                if let Some(cp) = close_paren {
                    let cast_tokens: Vec<&str> = tokens[i + 1..i + cp].to_vec();

                    // Check if this looks like a type cast (single type keyword or pointer)
                    if self.is_type_cast(&cast_tokens) {
                        let cast_type = cast_tokens.join(" ");
                        let rust_type = TypeConverter::new()
                            .convert(&cast_type)
                            .unwrap_or_else(|| cast_type.clone());

                        // Get the value being cast (next token after closing paren)
                        if i + cp + 1 < tokens.len() {
                            let cast_value = tokens[i + cp + 1];
                            result.push(format!("{} as {}", cast_value, rust_type));
                            i = i + cp + 2;
                            continue;
                        }
                    }
                }
            }

            // Handle pointer dereference: *ptr -> *ptr (same in Rust, but may need unsafe)
            // Handle address-of: &var -> &var (same in Rust for references)

            // Handle increment/decrement
            if token == "++" {
                if !result.is_empty() {
                    let last = result.pop().unwrap();
                    result.push(format!("{} += 1", last));
                    i += 1;
                    continue;
                }
            }
            if token == "--" {
                if !result.is_empty() {
                    let last = result.pop().unwrap();
                    result.push(format!("{} -= 1", last));
                    i += 1;
                    continue;
                }
            }

            // Handle arrow operator: ptr->field -> (*ptr).field or ptr.field for Rust
            if token == "->" && i + 1 < tokens.len() {
                if !result.is_empty() {
                    let ptr = result.pop().unwrap();
                    let field = tokens[i + 1];
                    // In Rust, use unsafe { (*ptr).field } or just ptr.field if using references
                    result.push(format!("(*{}).{}", ptr, field));
                    i += 2;
                    continue;
                }
            }

            // Handle NULL -> None or std::ptr::null()
            if token == "NULL" {
                result.push("std::ptr::null()".to_string());
                i += 1;
                continue;
            }

            // Handle sizeof
            if token == "sizeof" && i + 1 < tokens.len() {
                // sizeof(type) or sizeof(expr)
                if tokens.get(i + 1) == Some(&"(") {
                    let close = tokens[i + 1..].iter().position(|t| *t == ")");
                    if let Some(cp) = close {
                        let inner: Vec<&str> = tokens[i + 2..i + 1 + cp].to_vec();
                        let inner_str = inner.join(" ");
                        let rust_type = TypeConverter::new()
                            .convert(&inner_str)
                            .unwrap_or_else(|| inner_str);
                        result.push(format!("std::mem::size_of::<{}>()", rust_type));
                        i = i + 2 + cp;
                        continue;
                    }
                }
            }

            // Default: keep token as-is
            result.push(token.to_string());
            i += 1;
        }

        result.join(" ")
    }

    /// Check if tokens look like a type cast
    fn is_type_cast(&self, tokens: &[&str]) -> bool {
        if tokens.is_empty() {
            return false;
        }

        // Single type keyword
        let first = tokens[0];
        let type_keywords = [
            "int",
            "char",
            "short",
            "long",
            "float",
            "double",
            "void",
            "unsigned",
            "signed",
            "size_t",
            "ssize_t",
            "int8_t",
            "int16_t",
            "int32_t",
            "int64_t",
            "uint8_t",
            "uint16_t",
            "uint32_t",
            "uint64_t",
            "intptr_t",
            "uintptr_t",
            "ptrdiff_t",
        ];

        // Check for type keyword or pointer to type
        if type_keywords.contains(&first) {
            return true;
        }

        // Check for pointer cast: type *
        if tokens.len() >= 2 && tokens.last() == Some(&"*") {
            return true;
        }

        // Check for struct/enum/union cast
        if first == "struct" || first == "enum" || first == "union" {
            return true;
        }

        false
    }
}

impl Build for FunctionHandler {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("Handler", "FunctionHandler");
        entry.set_attr("stage", Entry::string(self.stage.as_str()));
        entry.set_attr("confidence", Entry::f64(self.confidence));
        if let Some(ref output) = self.output {
            entry.set_attr("output", Entry::string(output));
        }
        if let Some(ref error) = self.error {
            entry.set_attr("error", Entry::string(error));
        }
        entry.set_attr("function_name", Entry::string(&self.data.name));
        entry.set_attr("return_type", Entry::string(&self.data.return_type));
        entry.set_attr("is_declaration", Entry::bool(self.data.is_declaration));
        entry
    }

    fn kind(&self) -> &str {
        "Handler"
    }
    fn name(&self) -> Option<&str> {
        Some("FunctionHandler")
    }
    fn category(&self) -> Option<&str> {
        Some("function")
    }
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

        let output = handler.convert();
        assert!(output.is_some());
        let code = output.unwrap();
        assert!(code.contains("fn main()"));
        assert!(code.contains("-> i32"));
    }

    #[test]
    fn test_void_function() {
        let tokens = make_tokens(&["void", "do_something", "(", ")", "{", "}"]);
        let mut handler = FunctionHandler::new();

        assert!(handler.validate(&tokens));
        assert!(handler.extract(&tokens));

        let output = handler.convert();
        assert!(output.is_some());
        let code = output.unwrap();
        assert!(code.contains("fn do_something()"));
        assert!(!code.contains("->"));
    }

    #[test]
    fn test_function_with_params() {
        let tokens = make_tokens(&[
            "int", "add", "(", "int", "a", ",", "int", "b", ")", "{", "}",
        ]);
        let mut handler = FunctionHandler::new();

        assert!(handler.validate(&tokens));
        assert!(handler.extract(&tokens));

        let output = handler.convert();
        assert!(output.is_some());
        let code = output.unwrap();
        assert!(code.contains("fn add("));
        assert!(code.contains("a: i32"));
        assert!(code.contains("b: i32"));
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
