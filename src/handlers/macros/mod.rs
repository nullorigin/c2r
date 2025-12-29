//! Preprocessor/Macro Handler
//!
//! Converts C preprocessor directives to Rust equivalents:
//! - `#ifdef SYMBOL` / `#endif` -> `#[cfg(feature = "symbol")]`
//! - `#ifndef SYMBOL` / `#endif` -> `#[cfg(not(feature = "symbol"))]`
//! - `#if defined(SYMBOL)` -> `#[cfg(feature = "symbol")]`
//! - `#else` -> handled as part of conditional blocks
//! - `#elif` -> `#[cfg(all(not(...), ...))]`

use crate::db::pattern::{Pattern, PatternRule};
use crate::db::token::Token;
use crate::db::web::{Build, Entry};
use crate::handlers::process::{ProcessStage, Processor};
use crate::handlers::validation::SequenceValidator;
use std::ops::Range;

// ============================================================================
// Preprocessor Conditional Handler
// ============================================================================

/// Type of preprocessor conditional
#[derive(Debug, Clone, PartialEq)]
pub enum ConditionalType {
    /// #ifdef SYMBOL
    IfDef,
    /// #ifndef SYMBOL
    IfNDef,
    /// #if expression
    If,
    /// #elif expression
    ElIf,
    /// #else
    Else,
    /// #endif
    EndIf,
}

/// A preprocessor conditional block
#[derive(Debug, Clone, Default)]
pub struct ConditionalBlock {
    /// Type of conditional
    pub cond_type: Option<ConditionalType>,
    /// Symbol or expression being tested
    pub condition: String,
    /// Whether this is negated (ifndef or !defined)
    pub is_negated: bool,
    /// Nested depth level
    pub depth: usize,
}

/// Extracted preprocessor data
#[derive(Debug, Clone, Default)]
pub struct PreprocessorData {
    /// The directive type (#ifdef, #endif, etc.)
    pub directive: String,
    /// The symbol/expression being tested
    pub symbol: String,
    /// Whether negated
    pub is_negated: bool,
    /// Full original line
    pub original_line: String,
}

/// Handler for C preprocessor conditionals
#[derive(Debug)]
pub struct PreprocessorHandler {
    name: String,
    stage: ProcessStage,
    confidence: f64,
    error: Option<String>,
    output: Option<String>,
    data: PreprocessorData,
    range: Range<usize>,
    input_tokens: Vec<String>,
}

impl PreprocessorHandler {
    pub fn new() -> Self {
        Self {
            name: "preprocessor".to_string(),
            stage: ProcessStage::Pending,
            confidence: 0.0,
            error: None,
            output: None,
            data: PreprocessorData::default(),
            range: 0..0,
            input_tokens: Vec::new(),
        }
    }

    /// Convert C preprocessor symbol to Rust feature name
    fn symbol_to_feature(symbol: &str) -> String {
        // Convert common C macros to Rust conventions
        let s = symbol.to_lowercase();

        // Handle platform-specific macros
        match symbol {
            "_WIN32" | "_WIN64" | "WIN32" | "WIN64" => "windows".to_string(),
            "__linux__" | "__linux" | "linux" => "linux".to_string(),
            "__APPLE__" | "__MACH__" => "macos".to_string(),
            "__unix__" | "__unix" => "unix".to_string(),
            "__FreeBSD__" => "freebsd".to_string(),
            "__ANDROID__" => "android".to_string(),
            "_MSC_VER" => "msvc".to_string(),
            "__GNUC__" => "gnu".to_string(),
            "__clang__" => "clang".to_string(),
            "DEBUG" | "_DEBUG" | "NDEBUG" => {
                if symbol == "NDEBUG" {
                    "not(debug_assertions)".to_string()
                } else {
                    "debug_assertions".to_string()
                }
            }
            // Default: convert to snake_case feature
            _ => s.replace("__", "_").trim_matches('_').to_string(),
        }
    }

    /// Check if a symbol maps to a target_os cfg
    fn is_target_os(symbol: &str) -> bool {
        matches!(
            symbol,
            "_WIN32"
                | "_WIN64"
                | "WIN32"
                | "WIN64"
                | "__linux__"
                | "__linux"
                | "linux"
                | "__APPLE__"
                | "__MACH__"
                | "__unix__"
                | "__unix"
                | "__FreeBSD__"
                | "__ANDROID__"
        )
    }

    /// Check if a symbol maps to a target_env cfg
    fn is_target_env(symbol: &str) -> bool {
        matches!(symbol, "_MSC_VER" | "__GNUC__" | "__clang__")
    }
}

impl Default for PreprocessorHandler {
    fn default() -> Self {
        Self::new()
    }
}

impl Processor for PreprocessorHandler {

    fn supported_patterns(&self) -> &[&str] {
        &["ifdef", "ifndef", "endif", "else", "elif", "if_defined"]
    }

    fn patterns(&self) -> Vec<(Pattern, Pattern)> {
        vec![
            // #ifdef SYMBOL
            (
                Pattern::definition(
                    700,
                    "ifdef",
                    vec![PatternRule::exact("#ifdef"), PatternRule::identifier()],
                )
                .with_category("preprocessor")
                .with_priority(900)
                .with_min_tokens(2)
                .with_description("C #ifdef directive"),
                Pattern::definition(
                    700,
                    "ifdef",
                    vec![PatternRule::exact("#ifdef"), PatternRule::identifier()],
                )
                .with_category("preprocessor")
                .with_priority(900)
                .with_min_tokens(2)
                .with_description("C #ifdef directive"),
            ),
            // #ifndef SYMBOL
            (
                Pattern::definition(
                    701,
                    "ifndef",
                    vec![PatternRule::exact("#ifndef"), PatternRule::identifier()],
                )
                .with_category("preprocessor")
                .with_priority(900)
                .with_min_tokens(2)
                .with_description("C #ifndef directive"),
                Pattern::definition(
                    701,
                    "ifndef",
                    vec![PatternRule::exact("#ifndef"), PatternRule::identifier()],
                )
                .with_category("preprocessor")
                .with_priority(900)
                .with_min_tokens(2)
                .with_description("C #ifndef directive"),
            ),
            // #endif
            (
                Pattern::definition(702, "endif", vec![PatternRule::exact("#endif")])
                    .with_category("preprocessor")
                    .with_priority(900)
                    .with_min_tokens(1)
                    .with_description("C #endif directive"),
                Pattern::definition(702, "endif", vec![PatternRule::exact("#endif")])
                    .with_category("preprocessor")
                    .with_priority(900)
                    .with_min_tokens(1)
                    .with_description("C #endif directive"),
            ),
            // #else
            (
                Pattern::definition(703, "else_directive", vec![PatternRule::exact("#else")])
                    .with_category("preprocessor")
                    .with_priority(900)
                    .with_min_tokens(1)
                    .with_description("C #else directive"),
                Pattern::definition(703, "else_directive", vec![PatternRule::exact("#else")])
                    .with_category("preprocessor")
                    .with_priority(900)
                    .with_min_tokens(1)
                    .with_description("C #else directive"),
            ),
            // #elif expression
            (
                Pattern::definition(
                    704,
                    "elif",
                    vec![PatternRule::exact("#elif"), PatternRule::any().repeat(-1)],
                )
                .with_category("preprocessor")
                .with_priority(900)
                .with_min_tokens(2)
                .with_description("C #elif directive"),
                Pattern::definition(
                    704,
                    "elif",
                    vec![PatternRule::exact("#elif"), PatternRule::any().repeat(-1)],
                )
                .with_category("preprocessor")
                .with_priority(900)
                .with_min_tokens(2)
                .with_description("C #elif directive"),
            ),
            // #if defined(SYMBOL)
            (
                Pattern::definition(
                    705,
                    "if_defined",
                    vec![PatternRule::exact("#if"), PatternRule::any().repeat(-1)],
                )
                .with_category("preprocessor")
                .with_priority(900)
                .with_min_tokens(2)
                .with_description("C #if directive"),
                Pattern::definition(
                    705,
                    "if_defined",
                    vec![PatternRule::exact("#if"), PatternRule::any().repeat(-1)],
                )
                .with_category("preprocessor")
                .with_priority(900)
                .with_min_tokens(2)
                .with_description("C #if directive"),
            ),
        ]
    }

    fn validate(&mut self, tokens: &[Token]) -> bool {
        if tokens.is_empty() {
            self.error = Some("No tokens for preprocessor".to_string());
            return false;
        }

        self.input_tokens = tokens.iter().map(|t| t.to_string()).collect();
        self.range = 0..tokens.len();

        let first = &self.input_tokens[0];

        // Check if it's a preprocessor directive
        let is_preprocessor = first.starts_with('#')
            || first == "#"
            || matches!(
                first.as_str(),
                "#ifdef" | "#ifndef" | "#endif" | "#else" | "#elif" | "#if"
            );

        if !is_preprocessor {
            self.error = Some("Not a preprocessor directive".to_string());
            return false;
        }

        // Use keyword validation for sequence checking
        let validator = SequenceValidator::new();
        let validation_result = validator.validate_preprocessor(tokens);

        // Adjust confidence based on validation
        self.confidence = validation_result.adjust_confidence(0.9);

        if !validation_result.is_valid {
            if let Some(reason) = &validation_result.reason {
                self.error = Some(reason.clone());
            }
            // Still return true - we can try to process, just with lower confidence
        }

        true
    }

    fn extract(&mut self, tokens: &[Token]) -> bool {
        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        self.data.original_line = token_strs.join(" ");

        if token_strs.is_empty() {
            self.error = Some("Empty token list".to_string());
            return false;
        }

        let first = &token_strs[0];

        // Parse directive type - handle both "# ifdef" and "#ifdef" formats
        let (directive, symbol_start_idx) = if first == "#" && token_strs.len() > 1 {
            (format!("#{}", token_strs[1]), 2)
        } else if first.starts_with('#') {
            (first.clone(), 1)
        } else {
            // Might be just the directive name without #
            (format!("#{}", first), 1)
        };

        self.data.directive = directive.clone();

        match directive.as_str() {
            "#ifdef" => {
                self.data.is_negated = false;
                if token_strs.len() > symbol_start_idx {
                    self.data.symbol = token_strs[symbol_start_idx].clone();
                }
            }
            "#ifndef" => {
                self.data.is_negated = true;
                if token_strs.len() > symbol_start_idx {
                    self.data.symbol = token_strs[symbol_start_idx].clone();
                }
            }
            "#if" => {
                // Parse #if defined(SYMBOL) or #if !defined(SYMBOL)
                let rest = token_strs[symbol_start_idx..].join(" ");
                if rest.contains("defined") {
                    self.data.is_negated = rest.contains('!') || rest.contains("!defined");
                    // Extract symbol from defined(SYMBOL)
                    if let Some(start) = rest.find('(') {
                        if let Some(end) = rest.find(')') {
                            self.data.symbol = rest[start + 1..end].trim().to_string();
                        }
                    }
                } else {
                    // Generic #if expression - keep as-is
                    self.data.symbol = rest;
                }
            }
            "#elif" => {
                let rest = token_strs[symbol_start_idx..].join(" ");
                self.data.is_negated = rest.contains('!');
                if rest.contains("defined") {
                    if let Some(start) = rest.find('(') {
                        if let Some(end) = rest.find(')') {
                            self.data.symbol = rest[start + 1..end].trim().to_string();
                        }
                    }
                } else {
                    self.data.symbol = rest;
                }
            }
            "#else" | "#endif" => {
                // No symbol needed
            }
            _ => {
                self.error = Some(format!("Unknown directive: {}", directive));
                return false;
            }
        }

        true
    }

    fn convert(&mut self) -> Option<String> {
        let output = match self.data.directive.as_str() {
            "#ifdef" | "#ifndef" => {
                let feature = Self::symbol_to_feature(&self.data.symbol);

                // Determine the appropriate cfg type
                let cfg_expr = if Self::is_target_os(&self.data.symbol) {
                    format!("target_os = \"{}\"", feature)
                } else if Self::is_target_env(&self.data.symbol) {
                    format!("target_env = \"{}\"", feature)
                } else if feature == "debug_assertions" || feature.starts_with("not(") {
                    feature.clone()
                } else {
                    format!("feature = \"{}\"", feature)
                };

                if self.data.is_negated {
                    if cfg_expr.starts_with("not(") {
                        // Already negated (like NDEBUG)
                        format!("#[cfg({})]", cfg_expr)
                    } else {
                        format!("#[cfg(not({}))]", cfg_expr)
                    }
                } else {
                    format!("#[cfg({})]", cfg_expr)
                }
            }
            "#if" => {
                if !self.data.symbol.is_empty() {
                    let feature = Self::symbol_to_feature(&self.data.symbol);

                    let cfg_expr = if Self::is_target_os(&self.data.symbol) {
                        format!("target_os = \"{}\"", feature)
                    } else if Self::is_target_env(&self.data.symbol) {
                        format!("target_env = \"{}\"", feature)
                    } else {
                        format!("feature = \"{}\"", feature)
                    };

                    if self.data.is_negated {
                        format!("#[cfg(not({}))]", cfg_expr)
                    } else {
                        format!("#[cfg({})]", cfg_expr)
                    }
                } else {
                    format!(
                        "// TODO: Convert #if expression: {}",
                        self.data.original_line
                    )
                }
            }
            "#elif" => {
                // For #elif, we need context from the previous #if
                // For now, generate a cfg with a comment
                if !self.data.symbol.is_empty() {
                    let feature = Self::symbol_to_feature(&self.data.symbol);
                    format!("// #elif converted:\n#[cfg(feature = \"{}\")]", feature)
                } else {
                    format!(
                        "// TODO: Convert #elif expression: {}",
                        self.data.original_line
                    )
                }
            }
            "#else" => {
                // #else in Rust would require knowing the previous condition
                // Generate a comment for manual review
                "// #else - requires manual cfg inversion from previous condition".to_string()
            }
            "#endif" => {
                // #endif has no direct Rust equivalent - it's implicit in the item scope
                "// #endif".to_string()
            }
            _ => {
                format!("// Unknown preprocessor: {}", self.data.original_line)
            }
        };

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

impl Build for PreprocessorHandler {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("Handler", "PreprocessorHandler");
        entry.set_attr("stage", Entry::string(self.stage.as_str()));
        entry.set_attr("confidence", Entry::f64(self.confidence));
        entry.set_attr("directive", Entry::string(&self.data.directive));
        entry.set_attr("symbol", Entry::string(&self.data.symbol));
        entry.set_attr("is_negated", Entry::bool(self.data.is_negated));
        if let Some(ref output) = self.output {
            entry.set_attr("output", Entry::string(output));
        }
        entry
    }

    fn kind(&self) -> &str {
        "Handler"
    }
    fn name(&self) -> Option<&str> {
        Some("PreprocessorHandler")
    }
    fn category(&self) -> Option<&str> {
        Some("preprocessor")
    }
}

// ============================================================================
// Define Handler - converts #define to Rust const/macro
// ============================================================================

/// Extracted define data
#[derive(Debug, Clone, Default)]
pub struct DefineData {
    /// Macro name
    pub name: String,
    /// Macro parameters (for function-like macros)
    pub params: Vec<String>,
    /// Macro body/value
    pub value: String,
    /// Is this a function-like macro
    pub is_function_like: bool,
    /// Original line
    pub original_line: String,
}

/// Handler for #define directives
#[derive(Debug)]
pub struct DefineHandler {
    name: String,
    stage: ProcessStage,
    confidence: f64,
    error: Option<String>,
    output: Option<String>,
    data: DefineData,
    range: Range<usize>,
    input_tokens: Vec<String>,
}

impl DefineHandler {
    pub fn new() -> Self {
        Self {
            name: "define".to_string(),
            stage: ProcessStage::Pending,
            confidence: 0.0,
            error: None,
            output: None,
            data: DefineData::default(),
            range: 0..0,
            input_tokens: Vec::new(),
        }
    }

    /// Convert a function-like C macro to Rust macro_rules!
    fn convert_function_like_macro(&self) -> String {
        let macro_name = self.data.name.to_lowercase();

        // Build parameter pattern: $a:expr, $b:expr, etc.
        let param_pattern: String = self
            .data
            .params
            .iter()
            .map(|p| format!("${}:expr", p))
            .collect::<Vec<_>>()
            .join(", ");

        // Convert body - replace parameter references with $param
        let mut body = self.data.value.clone();
        for param in &self.data.params {
            // Replace bare parameter names with $param
            // Handle cases like: (a) > (b) ? (a) : (b)
            body = self.replace_param_in_body(&body, param);
        }

        // Convert common C operators/patterns to Rust
        body = self.convert_c_expr_to_rust(&body);

        if body.is_empty() {
            body = "/* empty */".to_string();
        }

        format!(
            "macro_rules! {} {{\n    ({}) => {{\n        {}\n    }};\n}}",
            macro_name, param_pattern, body
        )
    }

    /// Replace a parameter name in the macro body with $param
    fn replace_param_in_body(&self, body: &str, param: &str) -> String {
        let mut result = String::new();
        let chars: Vec<char> = body.chars().collect();
        let param_chars: Vec<char> = param.chars().collect();
        let mut i = 0;

        while i < chars.len() {
            // Check if we're at the start of the parameter name
            let mut matches = true;
            if i + param_chars.len() <= chars.len() {
                for (j, &pc) in param_chars.iter().enumerate() {
                    if chars[i + j] != pc {
                        matches = false;
                        break;
                    }
                }

                // Make sure it's a whole word (not part of a larger identifier)
                if matches {
                    let before_ok =
                        i == 0 || !chars[i - 1].is_alphanumeric() && chars[i - 1] != '_';
                    let after_ok = i + param_chars.len() >= chars.len()
                        || !chars[i + param_chars.len()].is_alphanumeric()
                            && chars[i + param_chars.len()] != '_';

                    if before_ok && after_ok {
                        result.push('$');
                        result.push_str(param);
                        i += param_chars.len();
                        continue;
                    }
                }
            }

            result.push(chars[i]);
            i += 1;
        }

        result
    }

    /// Convert C expression patterns to Rust equivalents
    fn convert_c_expr_to_rust(&self, expr: &str) -> String {
        let mut result = expr.to_string();

        // Ternary operator: (cond) ? (a) : (b) -> if cond { a } else { b }
        // This is a simplified conversion - complex ternaries may need manual fixes
        if result.contains('?') && result.contains(':') {
            if let Some(converted) = self.convert_ternary(&result) {
                result = converted;
            }
        }

        result
    }

    /// Attempt to convert C ternary operator to Rust if/else
    fn convert_ternary(&self, expr: &str) -> Option<String> {
        // Simple pattern: cond ? true_val : false_val
        let q_pos = expr.find('?')?;
        let c_pos = expr[q_pos..].find(':')? + q_pos;

        let condition = expr[..q_pos].trim();
        let true_val = expr[q_pos + 1..c_pos].trim();
        let false_val = expr[c_pos + 1..].trim();

        // Remove outer parens if present
        let condition = condition
            .trim_start_matches('(')
            .trim_end_matches(')')
            .trim();
        let true_val = true_val
            .trim_start_matches('(')
            .trim_end_matches(')')
            .trim();
        let false_val = false_val
            .trim_start_matches('(')
            .trim_end_matches(')')
            .trim();

        Some(format!(
            "if {} {{ {} }} else {{ {} }}",
            condition, true_val, false_val
        ))
    }

    /// Convert a type-defining macro (struct/enum/union) to macro_rules!
    fn convert_type_macro(&self, body: &str) -> String {
        let macro_name = self.data.name.to_lowercase();
        let rust_body = self.convert_c_to_rust(body);
        
        format!(
            "macro_rules! {} {{\n    () => {{\n        {}\n    }};\n}}",
            macro_name, rust_body
        )
    }

    /// Convert C type syntax to Rust
    fn convert_c_to_rust(&self, c_code: &str) -> String {
        let mut result = c_code.to_string();
        
        // Basic C to Rust type conversions
        result = result.replace("int", "i32");
        result = result.replace("float", "f32");
        result = result.replace("double", "f64");
        result = result.replace("char", "i8");
        result = result.replace("void", "()");
        
        // Clean up whitespace
        result.split_whitespace().collect::<Vec<_>>().join(" ")
    }

    /// Convert a simple single-line define to a Rust constant
    fn convert_simple_constant(&self, value: &str) -> String {
        // Determine type from value
        let (rust_type, rust_value) = if value.parse::<i64>().is_ok() {
            ("i32", value.to_string())
        } else if value.parse::<f64>().is_ok() {
            ("f64", value.to_string())
        } else if value.starts_with('"') && value.ends_with('"') {
            ("&str", value.to_string())
        } else if value.starts_with('\'') && value.ends_with('\'') {
            ("char", value.to_string())
        } else if value.starts_with("0x") || value.starts_with("0X") {
            // Hex literal
            ("i32", value.to_string())
        } else if value == "true" || value == "false" {
            ("bool", value.to_string())
        } else if value == "NULL" || value == "nullptr" {
            // Null pointer - use Option
            ("Option<*const ()>", "None".to_string())
        } else {
            // Unknown - keep as-is with unknown type comment
            ("/* TODO: determine type */", value.to_string())
        };

        format!("pub const {}: {} = {};", self.data.name, rust_type, rust_value)
    }
}

impl Default for DefineHandler {
    fn default() -> Self {
        Self::new()
    }
}

impl Processor for DefineHandler {

    fn supported_patterns(&self) -> &[&str] {
        &["validate_define_constant", "extract_define_constant"]
    }

    fn patterns(&self) -> Vec<(Pattern, Pattern)> {
        vec![(
            Pattern::definition(
                711,
                "validate_define_const",
                vec![
                    PatternRule::exact("#"),
                    PatternRule::exact("define"),
                    PatternRule::identifier(),
                    PatternRule::any().optional().repeat(-1),
                ],
            )
            .with_category("macro")
            .with_priority(890)
            .with_min_tokens(3)
            .with_description("C #define"),
            Pattern::definition(
                711,
                "extract_define_const",
                vec![
                    PatternRule::exact("#"),
                    PatternRule::exact("define").with_extract(|rule, ctx| {
                        ctx.set_value("directive", "define");
                        rule.clone()
                    }),
                    PatternRule::identifier().with_extract(|rule, ctx| {
                        let token = ctx.current_token.clone();
                        if ctx.did_match {
                            ctx.set_value("macro_name", &token);
                        }
                        rule.clone()
                    }),
                    PatternRule::any()
                        .optional()
                        .repeat(-1)
                        .with_extract(|rule, ctx| {
                            let token = ctx.current_token.clone();
                            if ctx.did_match {
                                ctx.push_list("value_tokens", &token);
                            }
                            rule.clone()
                        }),
                ],
            )
            .with_category("macro")
            .with_priority(890)
            .with_min_tokens(3)
            .with_description("C #define"),
        )]
    }

    fn validate(&mut self, tokens: &[Token]) -> bool {
        // Filter out backslash tokens (line continuations) - join onto one logical line
        let filtered_tokens: Vec<Token> = tokens
            .iter()
            .filter(|t| t.to_string() != "\\")
            .cloned()
            .collect();
        
        if filtered_tokens.len() < 3 {
            self.error = Some("Too few tokens for #define".to_string());
            return false;
        }

        self.input_tokens = filtered_tokens.iter().map(|t| t.to_string()).collect();
        self.range = 0..filtered_tokens.len();

        // Check for #define pattern
        let has_define = (self.input_tokens[0] == "#"
            && self.input_tokens.len() > 1
            && self.input_tokens[1] == "define")
            || self.input_tokens[0] == "#define";

        if !has_define {
            self.error = Some("Not a #define directive".to_string());
            return false;
        }

        self.confidence = 0.9;
        true
    }

    fn extract(&mut self, tokens: &[Token]) -> bool {
        // Filter out backslash tokens (line continuations)
        let token_strs: Vec<String> = tokens
            .iter()
            .map(|t| t.to_string())
            .filter(|s| s != "\\")
            .collect();
        self.data.original_line = token_strs.join(" ");

        // Find where macro name starts
        let name_idx = if token_strs[0] == "#" { 2 } else { 1 };

        if token_strs.len() <= name_idx {
            self.error = Some("No macro name found".to_string());
            return false;
        }

        let name_token = &token_strs[name_idx];

        // Check for function-like macro: NAME(params) or NAME ( params )
        if let Some(paren_pos) = name_token.find('(') {
            // Params are in the same token as name: MAX(a,b)
            self.data.name = name_token[..paren_pos].to_string();
            self.data.is_function_like = true;

            // Extract parameters - they might be split across tokens
            if let Some(close_paren) = name_token.find(')') {
                // All params in one token: MAX(a,b)
                let params_str = &name_token[paren_pos + 1..close_paren];
                self.data.params = params_str
                    .split(',')
                    .map(|s| s.trim().to_string())
                    .filter(|s| !s.is_empty())
                    .collect();
                // Value is everything after this token
                if token_strs.len() > name_idx + 1 {
                    self.data.value = token_strs[name_idx + 1..].join(" ");
                }
            } else {
                // Params continue in following tokens: MAX(a, b)
                // Find matching )
                let mut depth = 1; // Already saw (
                let mut params: Vec<String> = Vec::new();
                let rest_of_name = &name_token[paren_pos + 1..];
                if !rest_of_name.is_empty() {
                    for p in rest_of_name.split(',') {
                        let trimmed = p.trim();
                        if !trimmed.is_empty() {
                            params.push(trimmed.to_string());
                        }
                    }
                }

                let mut value_start = token_strs.len();
                for (i, t) in token_strs.iter().enumerate().skip(name_idx + 1) {
                    if t == "(" {
                        depth += 1;
                    } else if t == ")" {
                        depth -= 1;
                        if depth == 0 {
                            value_start = i + 1;
                            break;
                        }
                    } else if depth == 1 && t != "," {
                        params.push(t.clone());
                    }
                }
                self.data.params = params;
                if value_start < token_strs.len() {
                    self.data.value = token_strs[value_start..].join(" ");
                }
            }
        } else {
            self.data.name = name_token.clone();

            // Check if next token is (
            if token_strs.len() > name_idx + 1 && token_strs[name_idx + 1] == "(" {
                self.data.is_function_like = true;
                // Find matching )
                let mut depth = 0;
                let param_start = name_idx + 2;
                for (i, t) in token_strs.iter().enumerate().skip(name_idx + 1) {
                    if t == "(" {
                        depth += 1;
                    } else if t == ")" {
                        depth -= 1;
                        if depth == 0 {
                            // Params are between ( and )
                            let params: Vec<String> = token_strs[param_start..i]
                                .iter()
                                .filter(|s| *s != ",")
                                .cloned()
                                .collect();
                            self.data.params = params;
                            // Value starts after )
                            if i + 1 < token_strs.len() {
                                self.data.value = token_strs[i + 1..].join(" ");
                            }
                            break;
                        }
                    }
                }
            } else {
                // Simple constant: value is everything after name
                if token_strs.len() > name_idx + 1 {
                    self.data.value = token_strs[name_idx + 1..].join(" ");
                }
            }
        }

        true
    }

    fn convert(&mut self) -> Option<String> {
        let output = if self.data.is_function_like {
            // Convert to Rust macro_rules!
            self.convert_function_like_macro()
        } else if !self.data.value.is_empty() {
            let value = self.data.value.trim();
            
            // Check if this is a complex/multi-line define (has braces or type keywords)
            let is_complex = value.contains('{') || value.contains('}') ||
                value.starts_with("struct") || value.starts_with("enum") || value.starts_with("union");
            
            if is_complex {
                // Complex type definition - convert to macro_rules!
                self.convert_type_macro(value)
            } else {
                // Single-line define - convert to constant
                self.convert_simple_constant(value)
            }
        } else {
            // Empty define - use as feature flag
            format!(
                "// Feature flag: {}\n#[cfg(feature = \"{}\")]",
                self.data.name,
                self.data.name.to_lowercase()
            )
        };

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

impl Build for DefineHandler {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("Handler", "DefineHandler");
        entry.set_attr("stage", Entry::string(self.stage.as_str()));
        entry.set_attr("confidence", Entry::f64(self.confidence));
        entry.set_attr("macro_name", Entry::string(&self.data.name));
        entry.set_attr("is_function_like", Entry::bool(self.data.is_function_like));
        if let Some(ref output) = self.output {
            entry.set_attr("output", Entry::string(output));
        }
        entry
    }

    fn kind(&self) -> &str {
        "Handler"
    }
    fn name(&self) -> Option<&str> {
        Some("DefineHandler")
    }
    fn category(&self) -> Option<&str> {
        Some("macro")
    }
}

// ============================================================================
// Include Handler - converts #include to Rust use/mod
// ============================================================================

/// Extracted include data
#[derive(Debug, Clone, Default)]
pub struct IncludeData {
    /// Include path
    pub path: String,
    /// Is system include (<...>) vs local ("...")
    pub is_system: bool,
    /// Original line
    pub original_line: String,
}

/// Handler for #include directives
#[derive(Debug)]
pub struct IncludeHandler {
    name: String,
    stage: ProcessStage,
    confidence: f64,
    error: Option<String>,
    output: Option<String>,
    data: IncludeData,
    range: Range<usize>,
    input_tokens: Vec<String>,
}

impl IncludeHandler {
    pub fn new() -> Self {
        Self {
            name: "include".to_string(),
            stage: ProcessStage::Pending,
            confidence: 0.0,
            error: None,
            output: None,
            data: IncludeData::default(),
            range: 0..0,
            input_tokens: Vec::new(),
        }
    }

    /// Map C standard library headers to Rust equivalents
    fn map_header_to_rust(header: &str) -> Option<String> {
        match header {
            "stdio.h" => Some("use std::io::{self, Write, Read};".to_string()),
            "stdlib.h" => Some("use std::process; // and other stdlib functions".to_string()),
            "string.h" => {
                Some("// String functions are built into Rust's String/str types".to_string())
            }
            "math.h" => Some("// Math functions available via f32/f64 methods".to_string()),
            "stdint.h" | "inttypes.h" => Some(
                "// Fixed-width integers are built-in: i8, i16, i32, i64, u8, u16, u32, u64"
                    .to_string(),
            ),
            "stdbool.h" => Some("// bool is a built-in type in Rust".to_string()),
            "stddef.h" => Some("// size_t is usize in Rust".to_string()),
            "assert.h" => Some("// Use assert! or debug_assert! macros".to_string()),
            "errno.h" => Some("use std::io::Error;".to_string()),
            "time.h" => Some("use std::time::{Duration, Instant, SystemTime};".to_string()),
            "unistd.h" => Some("// Unix-specific: use std::os::unix or libc crate".to_string()),
            "pthread.h" => Some("use std::thread;".to_string()),
            "signal.h" => Some("// Use signal-hook or libc crate for signal handling".to_string()),
            "fcntl.h" => Some("use std::fs::OpenOptions;".to_string()),
            "sys/types.h" | "sys/stat.h" => Some("use std::fs;".to_string()),
            "limits.h" => Some("// Use i32::MAX, u64::MAX, etc.".to_string()),
            "float.h" => Some("// Use f32::MAX, f64::EPSILON, etc.".to_string()),
            "ctype.h" => {
                Some("// Use char methods: is_alphabetic(), is_numeric(), etc.".to_string())
            }
            "memory.h" => {
                Some("// Memory operations are handled by Rust's ownership system".to_string())
            }
            _ => None,
        }
    }
}

impl Default for IncludeHandler {
    fn default() -> Self {
        Self::new()
    }
}

impl Processor for IncludeHandler {

    fn supported_patterns(&self) -> &[&str] {
        &["include", "include_system", "include_local"]
    }

    fn patterns(&self) -> Vec<(Pattern, Pattern)> {
        vec![(
            Pattern::definition(
                720,
                "include",
                vec![
                    PatternRule::exact("#"),
                    PatternRule::exact("include"),
                    PatternRule::any(),
                ],
            )
            .with_category("macro")
            .with_priority(895)
            .with_min_tokens(3)
            .with_description("C #include directive"),
            Pattern::definition(
                720,
                "extract_include",
                vec![
                    PatternRule::exact("#"),
                    PatternRule::exact("include").with_extract(|rule, ctx| {
                        ctx.set_value("directive", "include");
                        rule.clone()
                    }),
                    PatternRule::any().with_extract(|rule, ctx| {
                        let token = ctx.current_token.clone();
                        if ctx.did_match {
                            ctx.set_value("path", &token);
                        }
                        rule.clone()
                    }),
                ],
            )
            .with_category("macro")
            .with_priority(895)
            .with_min_tokens(3)
            .with_description("C #include directive"),
        )]
    }

    fn validate(&mut self, tokens: &[Token]) -> bool {
        if tokens.is_empty() {
            self.error = Some("No tokens for #include".to_string());
            return false;
        }

        self.input_tokens = tokens.iter().map(|t| t.to_string()).collect();
        self.range = 0..tokens.len();

        // Check for single-token format: "#include <stdio.h>" or "#include \"header.h\""
        let first = &self.input_tokens[0];
        let has_include = first.starts_with("#include ")
            || first == "#include"
            || (self.input_tokens[0] == "#"
                && self.input_tokens.len() > 1
                && self.input_tokens[1] == "include");

        if !has_include {
            self.error = Some("Not an #include directive".to_string());
            return false;
        }

        self.confidence = 0.9;
        true
    }

    fn extract(&mut self, tokens: &[Token]) -> bool {
        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        self.data.original_line = token_strs.join(" ");

        let first = &token_strs[0];
        
        // Handle single-token format: "#include <stdio.h>" or "#include \"header.h\""
        if first.starts_with("#include ") {
            let path_part = first.trim_start_matches("#include ").trim();
            if path_part.starts_with('<') && path_part.ends_with('>') {
                self.data.is_system = true;
                self.data.path = path_part[1..path_part.len()-1].to_string();
                return true;
            } else if path_part.starts_with('"') && path_part.ends_with('"') {
                self.data.is_system = false;
                self.data.path = path_part[1..path_part.len()-1].to_string();
                return true;
            }
        }

        // Fallback: Find include path - starts after "include" (multi-token format)
        let path_idx = if token_strs[0] == "#" { 2 } else { 1 };

        if token_strs.len() <= path_idx {
            self.error = Some("No include path found".to_string());
            return false;
        }

        let path_token = &token_strs[path_idx];

        // Handle angle bracket includes: < header . h >
        if path_token == "<" {
            self.data.is_system = true;
            // Collect tokens until we find >
            let mut path_parts: Vec<String> = Vec::new();
            for t in &token_strs[path_idx + 1..] {
                if t == ">" {
                    break;
                }
                // Don't add space before . or /
                if !path_parts.is_empty() && !t.starts_with('.') && !t.starts_with('/') && *t != "."
                {
                    path_parts.push(t.clone());
                } else {
                    path_parts.push(t.clone());
                }
            }
            // Join without spaces - tokens like "unistd", ".", "h" become "unistd.h"
            self.data.path = path_parts.join("");
        } else if path_token.starts_with('<') && path_token.ends_with('>') {
            // Already combined: <header.h>
            self.data.is_system = true;
            self.data.path = path_token[1..path_token.len() - 1].to_string();
        } else if path_token.starts_with('<') {
            self.data.is_system = true;
            // Partial: <header - collect rest
            let full_path = token_strs[path_idx..].join("");
            if let (Some(start), Some(end)) = (full_path.find('<'), full_path.find('>')) {
                self.data.path = full_path[start + 1..end].to_string();
            } else {
                self.data.path = path_token.trim_start_matches('<').to_string();
            }
        } else if path_token.starts_with('"') {
            self.data.is_system = false;
            // Handle quoted includes
            if path_token.ends_with('"') && path_token.len() > 2 {
                self.data.path = path_token[1..path_token.len() - 1].to_string();
            } else {
                // Collect tokens until closing quote
                let full_path = token_strs[path_idx..].join("");
                if let (Some(start), Some(end)) = (full_path.find('"'), full_path.rfind('"')) {
                    if start != end {
                        self.data.path = full_path[start + 1..end].to_string();
                    }
                }
            }
        } else {
            // Unknown format - use as-is
            self.data.path = path_token.clone();
        }

        true
    }

    fn convert(&mut self) -> Option<String> {
        let output = if self.data.is_system {
            // System include - map to Rust pub use
            let module_name = self.data.path
                .trim_end_matches(".h")
                .replace('.', "_");
            
            if let Some(rust_equiv) = Self::map_header_to_rust(&self.data.path) {
                rust_equiv
            } else {
                // Output as pub use with a comment about the original
                format!("pub use {}; // from <{}>", module_name, self.data.path)
            }
        } else {
            // Local include - convert to pub use mod
            let module_name = self
                .data
                .path
                .trim_end_matches(".h")
                .trim_end_matches(".c")
                .replace('/', "::")
                .replace('\\', "::");

            format!("pub use {};", module_name)
        };

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

impl Build for IncludeHandler {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("Handler", "IncludeHandler");
        entry.set_attr("stage", Entry::string(self.stage.as_str()));
        entry.set_attr("confidence", Entry::f64(self.confidence));
        entry.set_attr("path", Entry::string(&self.data.path));
        entry.set_attr("is_system", Entry::bool(self.data.is_system));
        if let Some(ref output) = self.output {
            entry.set_attr("output", Entry::string(output));
        }
        entry
    }

    fn kind(&self) -> &str {
        "Handler"
    }
    fn name(&self) -> Option<&str> {
        Some("IncludeHandler")
    }
    fn category(&self) -> Option<&str> {
        Some("macro")
    }
}
