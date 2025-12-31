//! Loop Handler
//!
//! Converts C loop constructs (for, while, do-while) to Rust equivalents.

use crate::db::convert::sanitize_rust_identifier;
use crate::db::pattern::{Pattern, PatternRule};
use crate::db::token::Token;
use crate::db::web::{Build, Entry};
use crate::handlers::process::{ProcessorDecision, ProcessorStage, Processor, ProcessorState, ProcessorStats};
use crate::handlers::validation::SequenceValidator;
use crate::system;

// ============================================================================
// Loop Handler Implementation
// ============================================================================

/// Type of loop construct
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum LoopType {
    #[default]
    For,
    While,
    DoWhile,
}

impl LoopType {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::For => "for",
            Self::While => "while",
            Self::DoWhile => "do_while",
        }
    }
}

impl Build for LoopType {
    fn to_entry(&self) -> Entry { Entry::string(self.as_str()) }
    fn kind(&self) -> &str { "LoopType" }
    fn name(&self) -> Option<&str> { Some(self.as_str()) }
    fn category(&self) -> Option<&str> { Some("loop") }
}

/// Extracted loop information
#[derive(Debug, Clone, Default)]
pub struct LoopData {
    /// Type of loop
    pub loop_type: LoopType,
    /// Initialization expression (for loops)
    pub init: Option<String>,
    /// Loop condition
    pub condition: Option<String>,
    /// Update expression (for loops)
    pub update: Option<String>,
    /// Loop body tokens
    pub body_tokens: Vec<Token>,
}

impl Build for LoopData {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("LoopData", "loop");
        entry.set_attr("loop_type", self.loop_type.to_entry());
        if let Some(ref init) = self.init { entry.set_attr("init", Entry::string(init)); }
        if let Some(ref cond) = self.condition { entry.set_attr("condition", Entry::string(cond)); }
        if let Some(ref upd) = self.update { entry.set_attr("update", Entry::string(upd)); }
        entry
    }

    fn kind(&self) -> &str { "LoopData" }
    fn category(&self) -> Option<&str> { Some("loop") }
}

/// Handler for C loop constructs
#[derive(Debug)]
pub struct LoopHandler {
    /// Generic processor state
    state: ProcessorState<LoopData>,
}

impl LoopHandler {
    pub fn new() -> Self {
        Self { state: ProcessorState::new("LoopHandler") }
    }
}

impl Default for LoopHandler {
    fn default() -> Self {
        Self::new()
    }
}

impl Processor for LoopHandler {
    fn supported_patterns(&self) -> &[&str] {
        &[
            "validate_for_loop",
            "extract_for_loop",
            "validate_while_loop",
            "extract_while_loop",
            "validate_do_while_loop",
            "extract_do_while_loop",
        ]
    }

    fn stats(&self) -> &ProcessorStats {
        system().process_stats(&self.state.stats);
        &self.state.stats
    }

    fn patterns(&self) -> Vec<(Pattern, Pattern)> {
        vec![
            // for (init; cond; update) { body }
            (
                Pattern::definition(
                    300,
                    "validate_for_loop",
                    vec![
                        PatternRule::exact("for"),
                        PatternRule::exact("("),
                        PatternRule::any().optional().repeat(-1),
                        PatternRule::exact(")"),
                        PatternRule::exact("{"),
                    ],
                )
                .with_category("loop")
                .with_priority(850)
                .with_min_tokens(6)
                .with_description("C for loop"),
                Pattern::definition(
                    300,
                    "extract_for_loop",
                    vec![
                        PatternRule::exact("for").with_extract(|rule, ctx| {
                            ctx.set_value("loop_type", "for");
                            rule.clone()
                        }),
                        PatternRule::exact("(").with_extract(|rule, ctx| {
                            ctx.set_flag("in_header", true);
                            rule.clone()
                        }),
                        PatternRule::any()
                            .optional()
                            .repeat(-1)
                            .with_extract(|rule, ctx| {
                                let token = ctx.current_token.clone();
                                if ctx.did_match && ctx.flag("in_header") {
                                    ctx.push_list("header_tokens", &token);
                                }
                                rule.clone()
                            }),
                        PatternRule::exact(")").with_extract(|rule, ctx| {
                            ctx.set_flag("in_header", false);
                            rule.clone()
                        }),
                        PatternRule::exact("{"),
                    ],
                )
                .with_category("loop")
                .with_priority(850)
                .with_min_tokens(6)
                .with_description("C for loop"),
            ),
            // while (cond) { body }
            (
                Pattern::definition(
                    301,
                    "validate_while_loop",
                    vec![
                        PatternRule::exact("while"),
                        PatternRule::exact("("),
                        PatternRule::any().optional().repeat(-1),
                        PatternRule::exact(")"),
                        PatternRule::exact("{"),
                    ],
                )
                .with_category("loop")
                .with_priority(850)
                .with_min_tokens(5)
                .with_description("C while loop"),
                Pattern::definition(
                    301,
                    "extract_while_loop",
                    vec![
                        PatternRule::exact("while").with_extract(|rule, ctx| {
                            ctx.set_value("loop_type", "while");
                            rule.clone()
                        }),
                        PatternRule::exact("(").with_extract(|rule, ctx| {
                            ctx.set_flag("in_condition", true);
                            rule.clone()
                        }),
                        PatternRule::any()
                            .optional()
                            .repeat(-1)
                            .with_extract(|rule, ctx| {
                                let token = ctx.current_token.clone();
                                if ctx.did_match && ctx.flag("in_condition") {
                                    ctx.push_list("condition_tokens", &token);
                                }
                                rule.clone()
                            }),
                        PatternRule::exact(")").with_extract(|rule, ctx| {
                            ctx.set_flag("in_condition", false);
                            rule.clone()
                        }),
                        PatternRule::exact("{"),
                    ],
                )
                .with_category("loop")
                .with_priority(850)
                .with_min_tokens(5)
                .with_description("C while loop"),
            ),
            // do { body } while (cond);
            (
                Pattern::definition(
                    302,
                    "validate_do_while_loop",
                    vec![
                        PatternRule::exact("do"),
                        PatternRule::exact("{"),
                        PatternRule::any().optional().repeat(-1),
                        PatternRule::exact("}"),
                        PatternRule::exact("while"),
                    ],
                )
                .with_category("loop")
                .with_priority(860)
                .with_min_tokens(7)
                .with_description("C do-while loop"),
                Pattern::definition(
                    302,
                    "extract_do_while_loop",
                    vec![
                        PatternRule::exact("do").with_extract(|rule, ctx| {
                            ctx.set_value("loop_type", "do_while");
                            rule.clone()
                        }),
                        PatternRule::exact("{"),
                        PatternRule::any()
                            .optional()
                            .repeat(-1)
                            .with_extract(|rule, ctx| {
                                let token = ctx.current_token.clone();
                                if ctx.did_match {
                                    ctx.push_list("body_tokens", &token);
                                }
                                rule.clone()
                            }),
                        PatternRule::exact("}"),
                        PatternRule::exact("while").with_extract(|rule, ctx| {
                            ctx.set_flag("at_condition", true);
                            rule.clone()
                        }),
                    ],
                )
                .with_category("loop")
                .with_priority(860)
                .with_min_tokens(7)
                .with_description("C do-while loop"),
            ),
        ]
    }

    fn validate(&mut self, tokens: &[Token]) -> bool {
        if tokens.len() < 5 {
            self.state.stats.record_failure(None, "validated");
            self.stats();
            return false;
        }

        // Determine loop type from first token
        let first = tokens[0].to_string();
        match first.as_str() {
            "for" => self.state.data.loop_type = LoopType::For,
            "while" => self.state.data.loop_type = LoopType::While,
            "do" => self.state.data.loop_type = LoopType::DoWhile,
            _ => {
                self.state.stats.record_failure(None, "validated");
                self.stats();
                return false;
            }
        }

        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        // Try pattern matching for confidence
        let mut best_confidence = 0.0;
        for (pattern, _) in self.patterns() {
            if let Some(confidence) = pattern.matches_tokens(&token_strs) {
                if confidence > best_confidence {
                    best_confidence = confidence;
                }
            }
        }

        // Use keyword validation to adjust confidence
        let validator = SequenceValidator::new();
        let validation_result = validator.validate_control_flow(tokens);
        let base = if best_confidence > 0.0 { best_confidence } else { 0.8 };
        self.state.set_confidence(validation_result.adjust_confidence(base));

        self.state.stats.record_success(self.state.confidence, tokens.len(), 0, Some("loop"), "validated");
        self.stats();
        true
    }

    fn extract(&mut self, tokens: &[Token]) -> bool {
        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        // Try extraction patterns first
        for (_, extract_pattern) in self.patterns() {
            if let Some(ctx) = extract_pattern.run_extraction(&token_strs) {
                // Get loop type from pattern extraction
                if let Some(loop_type) = ctx.value("loop_type") {
                    self.state.data.loop_type = match loop_type {
                        "for" => LoopType::For,
                        "while" => LoopType::While,
                        "do_while" => LoopType::DoWhile,
                        _ => continue,
                    };
                }

                // Get header tokens (for loop init;cond;update)
                if let Some(header) = ctx.list("header_tokens") {
                    // Parse for loop header into init, condition, update
                    let header_str = header.join(" ");
                    let parts: Vec<&str> = header_str.split(';').collect();
                    if parts.len() >= 1 {
                        self.state.data.init = Some(parts[0].trim().to_string());
                    }
                    if parts.len() >= 2 {
                        self.state.data.condition = Some(parts[1].trim().to_string());
                    }
                    if parts.len() >= 3 {
                        self.state.data.update = Some(parts[2].trim().to_string());
                    }
                }

                // Get condition tokens (while loop)
                if let Some(cond) = ctx.list("condition_tokens") {
                    self.state.data.condition = Some(cond.join(" "));
                }

                // Get body tokens
                if let Some(body) = ctx.list("body_tokens") {
                    // Convert strings back to tokens for body
                    self.state.data.body_tokens = tokens
                        .iter()
                        .filter(|t| body.contains(&t.to_string()))
                        .cloned()
                        .collect();
                }

                self.state.set_confidence(0.85);
                self.state.stats.record_success(self.state.confidence, 0, 0, Some("loop"), "extracted");
                self.stats();
                return true;
            }
        }

        // Fallback to handler-specific extraction based on loop type
        let result = match self.state.data.loop_type {
            LoopType::For => self.extract_for(&token_strs, tokens),
            LoopType::While => self.extract_while(&token_strs, tokens),
            LoopType::DoWhile => self.extract_do_while(&token_strs, tokens),
        };
        if result {
            self.state.stats.record_success(self.state.confidence, 0, 0, Some("loop"), "extracted");
        } else {
            self.state.stats.record_failure(Some("loop"), "extracted");
        }
        self.stats();
        result
    }

    fn convert(&mut self) -> Option<String> {
        let result = match self.state.data.loop_type {
            LoopType::For => self.convert_for(),
            LoopType::While => self.convert_while(),
            LoopType::DoWhile => self.convert_do_while(),
        };
        if result.is_some() {
            self.state.stats.record_success(self.state.confidence, 0, 0, Some("loop"), "converted");
        } else {
            self.state.stats.record_failure(Some("loop"), "converted");
        }
        self.stats();
        result
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

impl LoopHandler {
    /// Extract for loop components: for (init; cond; update) { body }
    fn extract_for(&mut self, token_strs: &[String], tokens: &[Token]) -> bool {
        let paren_pos = token_strs.iter().position(|t| t == "(");
        if paren_pos.is_none() {
            return false;
        }

        // Find closing paren
        let mut paren_depth = 0;
        let mut close_paren = None;
        for (i, t) in token_strs.iter().enumerate().skip(paren_pos.unwrap()) {
            match t.as_str() {
                "(" => paren_depth += 1,
                ")" => {
                    paren_depth -= 1;
                    if paren_depth == 0 {
                        close_paren = Some(i);
                        break;
                    }
                }
                _ => {}
            }
        }

        let close_paren = match close_paren {
            Some(cp) => cp,
            None => return false,
        };
        let header_tokens = &token_strs[paren_pos.unwrap() + 1..close_paren];

        // Split by semicolons
        let mut parts: Vec<Vec<&str>> = Vec::new();
        let mut current: Vec<&str> = Vec::new();

        for t in header_tokens {
            if t == ";" {
                parts.push(std::mem::take(&mut current));
            } else {
                current.push(t);
            }
        }
        if !current.is_empty() {
            parts.push(current);
        }

        if parts.len() >= 1 && !parts[0].is_empty() {
            self.state.data.init = Some(parts[0].join(" "));
        }
        if parts.len() >= 2 && !parts[1].is_empty() {
            self.state.data.condition = Some(parts[1].join(" "));
        }
        if parts.len() >= 3 && !parts[2].is_empty() {
            self.state.data.update = Some(parts[2].join(" "));
        }

        // Find body
        let brace_pos = token_strs.iter().position(|t| t == "{");
        if let Some(bp) = brace_pos {
            let mut brace_depth = 0;
            let mut close_brace = None;
            for (i, t) in token_strs.iter().enumerate().skip(bp) {
                match t.as_str() {
                    "{" => brace_depth += 1,
                    "}" => {
                        brace_depth -= 1;
                        if brace_depth == 0 {
                            close_brace = Some(i);
                            break;
                        }
                    }
                    _ => {}
                }
            }

            if let Some(cb) = close_brace {
                self.state.data.body_tokens = tokens[bp + 1..cb].to_vec();
            }
        }

        self.state.set_confidence(0.85);
        true
    }

    /// Extract while loop: while (cond) { body }
    fn extract_while(&mut self, token_strs: &[String], tokens: &[Token]) -> bool {
        let paren_pos = token_strs.iter().position(|t| t == "(");
        if paren_pos.is_none() {
            return false;
        }

        let mut paren_depth = 0;
        let mut close_paren = None;
        for (i, t) in token_strs.iter().enumerate().skip(paren_pos.unwrap()) {
            match t.as_str() {
                "(" => paren_depth += 1,
                ")" => {
                    paren_depth -= 1;
                    if paren_depth == 0 {
                        close_paren = Some(i);
                        break;
                    }
                }
                _ => {}
            }
        }

        let close_paren = match close_paren {
            Some(cp) => cp,
            None => return false,
        };
        let cond_tokens = &token_strs[paren_pos.unwrap() + 1..close_paren];
        self.state.data.condition = Some(cond_tokens.join(" "));

        let brace_pos = token_strs.iter().position(|t| t == "{");
        if let Some(bp) = brace_pos {
            let mut brace_depth = 0;
            let mut close_brace = None;
            for (i, t) in token_strs.iter().enumerate().skip(bp) {
                match t.as_str() {
                    "{" => brace_depth += 1,
                    "}" => {
                        brace_depth -= 1;
                        if brace_depth == 0 {
                            close_brace = Some(i);
                            break;
                        }
                    }
                    _ => {}
                }
            }

            if let Some(cb) = close_brace {
                self.state.data.body_tokens = tokens[bp + 1..cb].to_vec();
            }
        }

        self.state.set_confidence(0.85);
        true
    }

    /// Extract do-while loop: do { body } while (cond);
    fn extract_do_while(&mut self, token_strs: &[String], tokens: &[Token]) -> bool {
        let brace_pos = token_strs.iter().position(|t| t == "{");
        if brace_pos.is_none() {
            return false;
        }

        let bp = brace_pos.unwrap();
        let mut brace_depth = 0;
        let mut close_brace = None;
        for (i, t) in token_strs.iter().enumerate().skip(bp) {
            match t.as_str() {
                "{" => brace_depth += 1,
                "}" => {
                    brace_depth -= 1;
                    if brace_depth == 0 {
                        close_brace = Some(i);
                        break;
                    }
                }
                _ => {}
            }
        }

        let cb = match close_brace {
            Some(cb) => cb,
            None => return false,
        };
        self.state.data.body_tokens = tokens[bp + 1..cb].to_vec();

        let while_pos = token_strs[cb..].iter().position(|t| t == "while");
        if let Some(wp) = while_pos {
            let wp = cb + wp;
            let paren_pos = token_strs[wp..].iter().position(|t| t == "(");
            if let Some(pp) = paren_pos {
                let pp = wp + pp;
                let mut paren_depth = 0;
                let mut close_paren = None;
                for (i, t) in token_strs.iter().enumerate().skip(pp) {
                    match t.as_str() {
                        "(" => paren_depth += 1,
                        ")" => {
                            paren_depth -= 1;
                            if paren_depth == 0 {
                                close_paren = Some(i);
                                break;
                            }
                        }
                        _ => {}
                    }
                }

                if let Some(cp) = close_paren {
                    let cond_tokens = &token_strs[pp + 1..cp];
                    self.state.data.condition = Some(cond_tokens.join(" "));
                }
            }
        }

        self.state.set_confidence(0.85);
        true
    }

    /// Convert for loop to Rust
    fn convert_for(&mut self) -> Option<String> {
        let body = self.convert_body();

        if let (Some(init), Some(cond), Some(update)) =
            (&self.state.data.init, &self.state.data.condition, &self.state.data.update)
        {
            if let Some(range_loop) = self.try_convert_range_for(init, cond, update, &body) {
                return Some(range_loop);
            }
        }

        let init_str = self.state.data.init.clone().unwrap_or_default();
        let cond_str = self.state.data.condition.clone().unwrap_or_default();
        let update_str = self.state.data.update.clone().unwrap_or_default();

        let init = if init_str.is_empty() {
            String::new()
        } else {
            self.convert_init(&init_str)
        };
        let cond = if cond_str.is_empty() {
            "true".to_string()
        } else {
            self.convert_condition(&cond_str)
        };
        let update = if update_str.is_empty() {
            String::new()
        } else {
            self.convert_update(&update_str)
        };
        let base_indent = system().indent_str();

        let mut result = String::new();
        if !init.is_empty() {
            // Add semicolon if the init doesn't already end with one
            let init_with_semi = if init.ends_with(';') {
                init
            } else {
                format!("{};", init)
            };
            result.push_str(&format!("{}{}\n", base_indent, init_with_semi));
        }
        result.push_str(&format!("{}while {} {{\n{}", base_indent, cond, body));
        if !update.is_empty() {
            // Get body indent for update statement
            system().indent_push();
            let body_indent = system().indent_str();
            system().indent_pop();
            result.push_str(&format!("\n{}{}", body_indent, update));
        }
        result.push_str(&format!("\n{}}}", base_indent));

        self.state.set_confidence(0.8);
        Some(result)
    }

    /// Try to convert to Rust range-based for loop
    fn try_convert_range_for(
        &self,
        init: &str,
        cond: &str,
        update: &str,
        body: &str,
    ) -> Option<String> {
        let init_parts: Vec<&str> = init.split_whitespace().collect();
        if init_parts.len() < 3 {
            return None;
        }

        let (var_name, start) = if init_parts.len() >= 4 && init_parts[2] == "=" {
            (init_parts[1], init_parts[3])
        } else if init_parts.len() >= 3 && init_parts[1] == "=" {
            (init_parts[0], init_parts[2])
        } else {
            return None;
        };

        let cond_parts: Vec<&str> = cond.split_whitespace().collect();
        if cond_parts.len() < 3 {
            return None;
        }

        let cond_var = cond_parts[0];
        let cmp_op = cond_parts[1];
        let end_val = cond_parts[2..].join(" ");

        if cond_var != var_name {
            return None;
        }

        let is_increment = update == format!("{} ++", var_name)
            || update == format!("++ {}", var_name)
            || update == format!("{}++", var_name)
            || update == format!("++{}", var_name)
            || update == format!("{} += 1", var_name);

        if !is_increment {
            return None;
        }

        let range = match cmp_op {
            "<" => format!("{}..{}", start, end_val),
            "<=" => format!("{}..={}", start, end_val),
            _ => return None,
        };
        let base_indent = system().indent_str();
        Some(format!(
            "{}for {} in {} {{\n{}\n{}}}",
            base_indent, var_name, range, body, base_indent
        ))
    }

    /// Convert while loop to Rust
    fn convert_while(&mut self) -> Option<String> {

        // Get base indent (current level before body push)
        let base_indent = system().indent_str();

        let cond_str = self.state.data.condition.clone().unwrap_or_default();
        let body = self.convert_body();

        // Check for infinite loop patterns: while(1), while(true), while 1
        let trimmed_cond = cond_str.trim();
        if trimmed_cond == "1" || trimmed_cond == "true" || trimmed_cond.is_empty() {
            self.state.set_confidence(0.95);
            return Some(format!(
                "{}loop {{\n{}\n{}}}",
                base_indent, body, base_indent
            ));
        }

        let cond = self.convert_condition(&cond_str);

        self.state.set_confidence(0.9);
        Some(format!(
            "{}while {} {{\n{}\n{}}}",
            base_indent, cond, body, base_indent
        ))
    }

    /// Convert do-while loop to Rust
    fn convert_do_while(&mut self) -> Option<String> {

        let base_indent = system().indent_str();
        let cond_str = self.state.data.condition.clone().unwrap_or_default();
        let cond = if cond_str.is_empty() {
            "true".to_string()
        } else {
            self.convert_condition(&cond_str)
        };
        let body = self.convert_body();

        // Get body-level indent for the break statement
        system().indent_push();
        let body_indent = system().indent_str();
        system().indent_pop();

        self.state.set_confidence(0.85);
        Some(format!(
            "{}loop {{\n{}\n{}if !({}) {{ break; }}\n{}}}",
            base_indent, body, body_indent, cond, base_indent
        ))
    }

    /// Convert loop body by routing to appropriate handlers
    fn convert_body(&mut self) -> String {

        // Push indent level for loop body
        system().indent_push();
        let indent = system().indent_str();

        if self.state.data.body_tokens.is_empty() {
            system().indent_pop();
            return format!("{}todo!()", indent);
        }

        // Register route for tracking
        use crate::db::routing::Route;
        let _route_id = crate::system::system().register_route(
            Route::new("LoopHandler", "FunctionHandler")
                .with_reason("loop body statements")
                .with_range(0, self.state.data.body_tokens.len())
        );

        // Split into statements and convert each
        let mut statements: Vec<String> = Vec::new();
        let mut current_stmt: Vec<Token> = Vec::new();
        let mut brace_depth = 0;
        let mut paren_depth = 0;
        let mut in_control_struct = false;

        let body_tokens = &self.state.data.body_tokens;
        let mut i = 0;
        while i < body_tokens.len() {
            let token = &body_tokens[i];
            let token_str = token.to_string();

            // Track if we're in a control structure (switch, if, for, while)
            if current_stmt.is_empty()
                && matches!(token_str.as_str(), "switch" | "if" | "for" | "while")
            {
                in_control_struct = true;
            }

            if token_str == "{" {
                brace_depth += 1;
            }
            if token_str == "}" {
                brace_depth -= 1;
            }
            if token_str == "(" {
                paren_depth += 1;
            }
            if token_str == ")" {
                paren_depth -= 1;
            }

            current_stmt.push(token.clone());

            // End of statement: semicolon at depth 0 (both braces and parens), OR closing brace of control structure
            let mut is_end = (token_str == ";" && brace_depth == 0 && paren_depth == 0)
                || (token_str == "}" && brace_depth == 0 && in_control_struct);

            // Check if next token is 'else' - if so, continue collecting (if-else block)
            if is_end && token_str == "}" && i + 1 < body_tokens.len() {
                let next_token = body_tokens[i + 1].to_string();
                if next_token == "else" {
                    is_end = false; // Continue collecting for if-else
                }
            }

            if is_end {
                let stmt = self.route_statement(&current_stmt, "LoopHandler");
                if !stmt.is_empty() {
                    statements.push(stmt);
                }
                current_stmt.clear();
                in_control_struct = false;
            }
            
            i += 1;
        }

        // Handle any remaining tokens
        if !current_stmt.is_empty() {
            let stmt = self.route_statement(&current_stmt, "LoopHandler");
            if !stmt.is_empty() {
                statements.push(stmt);
            }
        }

        if statements.is_empty() {
            system().indent_pop();
            return format!("{}todo!()", indent);
        }

        let result = statements
            .iter()
            .map(|s| {
                // Multi-line blocks already have correct indentation from handlers
                if s.contains('\n') {
                    s.clone()
                } else {
                    format!("{}{}", indent, s)
                }
            })
            .collect::<Vec<_>>()
            .join("\n");

        // Pop indent level after body conversion
        system().indent_pop();
        result
    }

    /// Convert initialization statement
    fn convert_init(&mut self, init: &str) -> String {
        let parts: Vec<&str> = init.split_whitespace().collect();

        // Pattern: type name = value (e.g., "int i = 0")
        if parts.len() >= 4 && parts[2] == "=" {
            let c_type = parts[0];
            let name = parts[1];
            let value = parts[3..].join(" ");

            let rust_type = system().lookup_type(c_type)
                .unwrap_or_else(|| c_type.to_string());

            // Convert identifiers in value
            let converted_value = self.convert_identifiers_in_expr(&value);

            return format!("let mut {}: {} = {}", name, rust_type, converted_value);
        }

        // Pattern: name = value (e.g., "j = 10") - simple assignment
        if parts.len() >= 3 && parts[1] == "=" {
            let name = parts[0];
            let value = parts[2..].join(" ");
            let converted_value = self.convert_identifiers_in_expr(&value);
            return format!("{} = {}", name, converted_value);
        }

        // Fallback: return as-is but convert identifiers
        self.convert_identifiers_in_expr(init)
    }

    /// Convert condition expression
    fn convert_condition(&mut self, cond: &str) -> String {
        // Convert arrow operators first
        let converted = Self::convert_arrow_operator(cond);
        let converted = self.convert_identifiers_in_expr(&converted);
        converted.replace("NULL", "std::ptr::null()")
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
            } else {
                ""
            };
            
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

    /// Convert update expression
    fn convert_update(&self, update: &str) -> String {
        let trimmed = update.trim();
        if trimmed.ends_with("++") {
            let var = trimmed.trim_end_matches("++").trim();
            return format!("{} += 1;", var);
        }
        if trimmed.starts_with("++") {
            let var = trimmed.trim_start_matches("++").trim();
            return format!("{} += 1;", var);
        }
        if trimmed.ends_with("--") {
            let var = trimmed.trim_end_matches("--").trim();
            return format!("{} -= 1;", var);
        }
        if trimmed.starts_with("--") {
            let var = trimmed.trim_start_matches("--").trim();
            return format!("{} -= 1;", var);
        }

        format!("{};", update)
    }

    /// Convert identifiers in an expression using the id_converter and sanitize Rust keywords
    fn convert_identifiers_in_expr(&mut self, expr: &str) -> String {
        let tokens: Vec<&str> = expr.split_whitespace().collect();
        let converted: Vec<String> = tokens
            .iter()
            .map(|t| {
                // Try to convert known identifiers
                let converted = system().lookup_identifier(t)
                    .unwrap_or_else(|| t.to_string());
                // Sanitize Rust keywords (str -> r#str)
                sanitize_rust_identifier(&converted)
            })
            .collect();
        converted.join(" ")
    }
}

impl Build for LoopHandler {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("Handler", "LoopHandler");
        entry.set_attr("stage", self.state.stage().to_entry());
        entry.set_attr("confidence", Entry::f64(self.state.confidence));
        entry.set_attr("loop_type", self.state.data.loop_type.to_entry());
        if let Some(ref output) = self.state.output {
            entry.set_attr("output", Entry::string(output));
        }
        entry
    }

    fn kind(&self) -> &str {
        "Handler"
    }
    fn name(&self) -> Option<&str> {
        Some("LoopHandler")
    }
    fn category(&self) -> Option<&str> {
        Some("loop")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_tokens(strs: &[&str]) -> Vec<Token> {
        strs.iter().map(|s| Token::from_string(*s)).collect()
    }

    #[test]
    fn test_for_loop() {
        let tokens = make_tokens(&[
            "for", "(", "int", "i", "=", "0", ";", "i", "<", "10", ";", "i", "++", ")", "{",
            "printf", "(", "i", ")", ";", "}",
        ]);
        let mut handler = LoopHandler::new();

        assert!(handler.validate(&tokens));
        assert_eq!(handler.state.data.loop_type, LoopType::For);
        assert!(handler.extract(&tokens));
        assert_eq!(handler.state.data.init, Some("int i = 0".to_string()));
        assert_eq!(handler.state.data.condition, Some("i < 10".to_string()));
    }

    #[test]
    fn test_while_loop() {
        let tokens = make_tokens(&["while", "(", "x", ">", "0", ")", "{", "x", "--", ";", "}"]);
        let mut handler = LoopHandler::new();

        assert!(handler.validate(&tokens));
        assert_eq!(handler.state.data.loop_type, LoopType::While);
        assert!(handler.extract(&tokens));
        assert_eq!(handler.state.data.condition, Some("x > 0".to_string()));
    }

    #[test]
    fn test_do_while_loop() {
        let tokens = make_tokens(&[
            "do", "{", "x", "++", ";", "}", "while", "(", "x", "<", "10", ")", ";",
        ]);
        let mut handler = LoopHandler::new();

        assert!(handler.validate(&tokens));
        assert_eq!(handler.state.data.loop_type, LoopType::DoWhile);
        assert!(handler.extract(&tokens));
        assert_eq!(handler.state.data.condition, Some("x < 10".to_string()));
    }

    #[test]
    fn test_while_conversion() {
        let tokens = make_tokens(&[
            "while", "(", "running", ")", "{", "process", "(", ")", ";", "}",
        ]);
        let mut handler = LoopHandler::new();

        assert!(handler.validate(&tokens));
        assert!(handler.extract(&tokens));

        let result = handler.convert();
        assert!(result.is_some());
        let code = result.unwrap();
        assert!(code.contains("while running"));
    }
}
