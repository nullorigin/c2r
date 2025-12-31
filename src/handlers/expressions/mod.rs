//! Expression Handlers
//!
//! Converts C expressions to Rust expressions.
//! Includes comparison expressions and printf-style format strings.

use crate::db::convert::IdentifierConverter;
use crate::db::pattern::{Pattern, PatternRule};
use crate::db::token::Token;
use crate::db::web::{Build, Entry};
use crate::handlers::process::{ProcessorStage, Processor, ProcessorState, ProcessorStats};
use crate::system::system;

// ============================================================================
// Comparison Converter (helper for conditional expressions)
// ============================================================================

/// Types of comparison operations
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ComparisonOp {
    Equal, NotEqual, LessThan, GreaterThan, LessOrEqual, GreaterOrEqual,
}

impl ComparisonOp {
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "==" => Some(Self::Equal),
            "!=" => Some(Self::NotEqual),
            "<" => Some(Self::LessThan),
            ">" => Some(Self::GreaterThan),
            "<=" => Some(Self::LessOrEqual),
            ">=" => Some(Self::GreaterOrEqual),
            _ => None,
        }
    }

    pub fn as_str(&self) -> &str {
        match self {
            Self::Equal => "==",
            Self::NotEqual => "!=",
            Self::LessThan => "<",
            Self::GreaterThan => ">",
            Self::LessOrEqual => "<=",
            Self::GreaterOrEqual => ">=",
        }
    }
}

/// Comparison expression data
#[derive(Debug, Clone)]
pub struct Comparison {
    pub left: String,
    pub op: ComparisonOp,
    pub right: String,
}

/// Comparison expression converter
#[derive(Debug)]
pub struct ComparisonConverter {
    id_converter: IdentifierConverter,
}

impl ComparisonConverter {
    pub fn new() -> Self {
        Self { id_converter: IdentifierConverter::new() }
    }

    pub fn parse(&self, tokens: &[&str]) -> Option<Comparison> {
        let op_pos = tokens.iter()
            .position(|t| matches!(*t, "==" | "!=" | "<" | ">" | "<=" | ">="))?;
        if op_pos == 0 || op_pos >= tokens.len() - 1 { return None; }

        Some(Comparison {
            left: tokens[..op_pos].join(" "),
            op: ComparisonOp::from_str(tokens[op_pos])?,
            right: tokens[op_pos + 1..].join(" "),
        })
    }

    pub fn convert(&mut self, tokens: &[&str]) -> String {
        if let Some(cmp) = self.parse(tokens) {
            self.convert_comparison(&cmp)
        } else {
            tokens.join(" ")
        }
    }

    pub fn convert_comparison(&mut self, cmp: &Comparison) -> String {
        let left = self.convert_operand(&cmp.left);
        let right = self.convert_operand(&cmp.right);

        // Handle NULL comparisons
        if right == "std::ptr::null()" || right == "NULL" {
            match cmp.op {
                ComparisonOp::Equal => return format!("{}.is_null()", left),
                ComparisonOp::NotEqual => return format!("!{}.is_null()", left),
                _ => {}
            }
        }
        if left == "std::ptr::null()" || left == "NULL" {
            match cmp.op {
                ComparisonOp::Equal => return format!("{}.is_null()", right),
                ComparisonOp::NotEqual => return format!("!{}.is_null()", right),
                _ => {}
            }
        }

        format!("{} {} {}", left, cmp.op.as_str(), right)
    }

    fn convert_operand(&mut self, operand: &str) -> String {
        let trimmed = operand.trim();
        self.id_converter.convert(trimmed).unwrap_or_else(|| trimmed.to_string())
    }

    pub fn is_comparison(tokens: &[&str]) -> bool {
        tokens.iter().any(|t| matches!(*t, "==" | "!=" | "<" | ">" | "<=" | ">="))
    }

    pub fn convert_logical(&mut self, tokens: &[&str]) -> String {
        let joined = tokens.join(" ");
        let mut result = String::new();
        let mut current: Vec<&str> = Vec::new();

        for token in tokens {
            match *token {
                "&&" | "||" => {
                    if !current.is_empty() {
                        result.push_str(&self.convert(&current));
                        current.clear();
                    }
                    result.push_str(&format!(" {} ", token));
                }
                _ => current.push(token),
            }
        }

        if !current.is_empty() {
            result.push_str(&self.convert(&current));
        }

        if result.is_empty() { joined } else { result }
    }
}

impl Default for ComparisonConverter {
    fn default() -> Self { Self::new() }
}

// ============================================================================
// Format Specifier (for printf-style strings)
// ============================================================================

#[derive(Debug, Clone)]
pub struct FormatSpecifier {
    pub original: String,
    pub width: Option<usize>,
    pub precision: Option<usize>,
    pub left_justify: bool,
    pub zero_pad: bool,
    pub conversion: char,
}

impl FormatSpecifier {
    pub fn to_rust(&self) -> String {
        let mut result = String::from("{");
        if self.left_justify || self.zero_pad || self.width.is_some() || self.precision.is_some() {
            result.push(':');
            if self.left_justify { result.push('<'); }
            if self.zero_pad { result.push('0'); }
            if let Some(w) = self.width { result.push_str(&w.to_string()); }
            if let Some(p) = self.precision { result.push('.'); result.push_str(&p.to_string()); }
            match self.conversion {
                'e' | 'E' => result.push('e'),
                'x' => result.push('x'),
                'X' => result.push('X'),
                'o' => result.push('o'),
                'b' => result.push('b'),
                _ => {}
            }
        }
        result.push('}');
        result
    }
}

fn parse_format_specifier(s: &str, start: usize) -> Option<(FormatSpecifier, usize)> {
    let chars: Vec<char> = s[start..].chars().collect();
    if chars.is_empty() || chars[0] != '%' { return None; }

    if chars.len() > 1 && chars[1] == '%' {
        return Some((FormatSpecifier {
            original: "%%".to_string(), width: None, precision: None,
            left_justify: false, zero_pad: false, conversion: '%',
        }, 2));
    }

    let mut idx = 1;
    let mut left_justify = false;
    let mut zero_pad = false;
    let mut width: Option<usize> = None;
    let mut precision: Option<usize> = None;

    // Parse flags
    while idx < chars.len() {
        match chars[idx] {
            '-' => { left_justify = true; idx += 1; }
            '+' | ' ' | '#' => { idx += 1; }
            '0' => { zero_pad = true; idx += 1; }
            _ => break,
        }
    }

    // Parse width
    let width_start = idx;
    while idx < chars.len() && chars[idx].is_ascii_digit() { idx += 1; }
    if idx > width_start { width = s[start + width_start..start + idx].parse().ok(); }

    // Parse precision
    if idx < chars.len() && chars[idx] == '.' {
        idx += 1;
        let prec_start = idx;
        while idx < chars.len() && chars[idx].is_ascii_digit() { idx += 1; }
        precision = if idx > prec_start { s[start + prec_start..start + idx].parse().ok() } else { Some(0) };
    }

    // Skip length modifiers
    while idx < chars.len() && matches!(chars[idx], 'h' | 'l' | 'L' | 'z' | 'j' | 't') { idx += 1; }

    if idx >= chars.len() { return None; }
    let conversion = chars[idx];
    if !matches!(conversion, 'd' | 'i' | 'u' | 'o' | 'x' | 'X' | 'f' | 'F' | 'e' | 'E' | 'g' | 'G' | 'c' | 's' | 'p' | 'n') {
        return None;
    }
    idx += 1;

    Some((FormatSpecifier {
        original: s[start..start + idx].to_string(),
        width, precision, left_justify, zero_pad, conversion,
    }, idx))
}

// ============================================================================
// Expression Handler
// ============================================================================

#[derive(Debug, Clone, Default)]
pub struct ExpressionData {
    pub tokens: Vec<String>,
    pub operator: Option<String>,
    pub left: Option<String>,
    pub right: Option<String>,
}

impl Build for ExpressionData {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("ExpressionData", "expression");
        if let Some(ref op) = self.operator { entry.set_attr("operator", Entry::string(op)); }
        entry
    }
    fn kind(&self) -> &str { "ExpressionData" }
    fn category(&self) -> Option<&str> { Some("expression") }
}

#[derive(Debug)]
pub struct ExpressionHandler {
    state: ProcessorState<ExpressionData>,
}

impl ExpressionHandler {
    pub fn new() -> Self {
        Self { state: ProcessorState::new("ExpressionHandler") }
    }

    fn is_operator(token: &str) -> bool {
        matches!(token,
            "+" | "-" | "*" | "/" | "%" | "=" | "==" | "!=" | "<" | ">" | "<=" | ">=" |
            "&&" | "||" | "&" | "|" | "^" | "<<" | ">>" |
            "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>="
        )
    }

    fn is_type_cast(tokens: &[&str]) -> bool {
        if tokens.is_empty() { return false; }
        let first = tokens[0];
        let c_types = ["int", "char", "float", "double", "long", "short", "unsigned", "signed",
            "void", "size_t", "uint8_t", "uint16_t", "uint32_t", "uint64_t",
            "int8_t", "int16_t", "int32_t", "int64_t"];
        c_types.contains(&first) ||
            (tokens.len() >= 2 && tokens.last() == Some(&"*")) ||
            matches!(first, "struct" | "enum" | "union")
    }

    /// Convert C ternary operator (cond ? true_val : false_val) to Rust if-else
    /// Recursively handles nested ternaries similar to function pointer parsing
    fn convert_ternary(&self, tokens: &[String]) -> Option<String> {
        // Find first ? position at depth 0 (not inside parentheses)
        let mut paren_depth: i32 = 0;
        let mut q_pos = None;
        for (i, t) in tokens.iter().enumerate() {
            match t.as_str() {
                "(" => paren_depth += 1,
                ")" => paren_depth = (paren_depth - 1).max(0),
                "?" if paren_depth == 0 && q_pos.is_none() => {
                    q_pos = Some(i);
                }
                _ => {}
            }
        }
        let q_pos = q_pos?;
        
        // Find matching : (account for nested ternaries and parentheses)
        let mut ternary_depth: i32 = 0;
        paren_depth = 0;
        let mut c_pos = None;
        for (i, t) in tokens.iter().enumerate().skip(q_pos + 1) {
            match t.as_str() {
                "(" => paren_depth += 1,
                ")" => paren_depth = (paren_depth - 1).max(0),
                "?" if paren_depth == 0 => ternary_depth += 1,
                ":" if paren_depth == 0 && ternary_depth == 0 => {
                    c_pos = Some(i);
                    break;
                }
                ":" if paren_depth == 0 => ternary_depth -= 1,
                _ => {}
            }
        }
        let c_pos = c_pos?;

        // Extract the three parts
        let condition_tokens: Vec<String> = tokens[..q_pos].to_vec();
        let true_tokens: Vec<String> = tokens[q_pos + 1..c_pos].to_vec();
        let false_tokens: Vec<String> = tokens[c_pos + 1..].to_vec();

        // Recursively convert nested ternaries in true_val and false_val
        let condition = self.convert_ternary_part(&condition_tokens);
        let true_val = self.convert_ternary_part(&true_tokens);
        let false_val = self.convert_ternary_part(&false_tokens);

        Some(format!("if {} {{ {} }} else {{ {} }}", condition, true_val, false_val))
    }

    /// Convert a ternary part, recursively handling nested ternaries
    fn convert_ternary_part(&self, tokens: &[String]) -> String {
        // First, strip outer parentheses from tokens if present
        let stripped = self.strip_outer_parens(tokens);
        
        // Check if this part contains a ternary operator
        if stripped.iter().any(|t| t == "?") {
            // Recursively convert nested ternary
            if let Some(converted) = self.convert_ternary(&stripped) {
                return converted;
            }
        }
        
        // No ternary - just join
        stripped.join(" ")
    }

    /// Strip matching outer parentheses from token slice
    fn strip_outer_parens(&self, tokens: &[String]) -> Vec<String> {
        if tokens.len() < 2 {
            return tokens.to_vec();
        }
        
        // Check if tokens start with ( and end with )
        if tokens.first().map(|s| s.as_str()) != Some("(") || 
           tokens.last().map(|s| s.as_str()) != Some(")") {
            return tokens.to_vec();
        }
        
        // Check if these are matching outer parens
        let mut depth = 0;
        for (i, t) in tokens.iter().enumerate() {
            match t.as_str() {
                "(" => depth += 1,
                ")" => {
                    depth -= 1;
                    // If depth hits 0 before the last token, parens don't match
                    if depth == 0 && i < tokens.len() - 1 {
                        return tokens.to_vec();
                    }
                }
                _ => {}
            }
        }
        
        // Outer parens match - strip them and recursively check
        if tokens.len() > 2 {
            self.strip_outer_parens(&tokens[1..tokens.len()-1])
        } else {
            Vec::new()
        }
    }
}

impl Default for ExpressionHandler {
    fn default() -> Self { Self::new() }
}

impl Processor for ExpressionHandler {
    fn supported_patterns(&self) -> &[&str] {
        &[
            "validate_expression",
            "extract_expression",
            "validate_assignment",
            "extract_assignment",
        ]
    }

    fn stats(&self) -> &ProcessorStats {
        system().process_stats(&self.state.stats);
        &self.state.stats
    }

    fn patterns(&self) -> Vec<(Pattern, Pattern)> {
        vec![(
            Pattern::definition(
                400,
                "validate_binary_expression",
                vec![
                    PatternRule::any(), // left operand
                    PatternRule::one_of(vec![
                        "+".to_string(),
                        "-".to_string(),
                        "*".to_string(),
                        "/".to_string(),
                        "==".to_string(),
                        "!=".to_string(),
                        "<".to_string(),
                        ">".to_string(),
                    ]),
                    PatternRule::any(), // right operand
                ],
            )
            .with_category("expression")
            .with_priority(40)
            .with_min_tokens(3)
            .with_description("Binary expression"),
            Pattern::definition(
                400,
                "extract_binary_expression",
                vec![
                    PatternRule::any().with_extract(|rule, ctx| {
                        let token = ctx.current_token.clone();
                        if ctx.did_match {
                            ctx.set_value("left", &token);
                        }
                        rule.clone()
                    }),
                    PatternRule::one_of(vec![
                        "+".to_string(),
                        "-".to_string(),
                        "*".to_string(),
                        "/".to_string(),
                        "==".to_string(),
                        "!=".to_string(),
                        "<".to_string(),
                        ">".to_string(),
                    ])
                    .with_extract(|rule, ctx| {
                        let token = ctx.current_token.clone();
                        if ctx.did_match {
                            ctx.set_value("operator", &token);
                        }
                        rule.clone()
                    }),
                    PatternRule::any().with_extract(|rule, ctx| {
                        let token = ctx.current_token.clone();
                        if ctx.did_match {
                            ctx.set_value("right", &token);
                        }
                        rule.clone()
                    }),
                ],
            )
            .with_category("expression")
            .with_priority(40)
            .with_min_tokens(3)
            .with_description("Binary expression"),
        )]
    }

    fn validate(&mut self, tokens: &[Token]) -> bool {
        let len = tokens.len();
        if len == 0 {
            return false;
        }

        // Check for increment/decrement operators (2 tokens like "x ++" or "++ x")
        if len >= 2 {
            for t in tokens {
                let s = t.to_string();
                if s == "++" || s == "--" {
                    self.state.set_confidence(0.7);
                    return true;
                }
            }
        }

        // For other expressions, need at least 3 tokens
        if len < 3 {
            return false;
        }

        // Must contain an operator
        for t in tokens {
            if Self::is_operator(&t.to_string()) {
                self.state.set_confidence(0.6);
                return true;
            }
        }

        false
    }

    fn extract(&mut self, tokens: &[Token]) -> bool {
        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        self.state.data.tokens = token_strs.clone();

        // Try extraction patterns first
        for (_, extract_pattern) in self.patterns() {
            if let Some(ctx) = extract_pattern.run_extraction(&token_strs) {
                self.state.data.left = ctx.value("left").map(String::from);
                self.state.data.operator = ctx.value("operator").map(String::from);
                self.state.data.right = ctx.value("right").map(String::from);

                if self.state.data.operator.is_some() {
                    self.state.set_confidence(0.7);
                    return true;
                }
            }
        }

        // Fallback to manual extraction
        if let Some((i, op)) = token_strs.iter().enumerate().find(|(_, t)| Self::is_operator(t)) {
            self.state.data.operator = Some(op.clone());
            self.state.data.left = (i > 0).then(|| token_strs[..i].join(" "));
            self.state.data.right = (i + 1 < token_strs.len()).then(|| token_strs[i + 1..].join(" "));
        }

        self.state.set_confidence(0.7);
        true
    }

    fn convert(&mut self) -> Option<String> {
        let tokens = &self.state.data.tokens;
        if tokens.is_empty() {
            return None;
        }

        // Handle pointer dereference assignment: * name = value -> *name = value
        if tokens.len() >= 4 && tokens[0] == "*" {
            if let Some(eq_pos) = tokens.iter().position(|t| t == "=") {
                if eq_pos >= 2 {
                    // * identifier = value
                    let ptr_name = tokens[1..eq_pos].join("");
                    let value_tokens = &tokens[eq_pos + 1..];
                    let value = value_tokens.join(" ");
                    self.state.set_confidence(0.85);
                    return Some(format!("*{} = {}", ptr_name, value));
                }
            }
        }

        // Check for ternary operator and convert first
        if let Some(ternary_result) = self.convert_ternary(tokens) {
            self.state.set_confidence(0.85);
            return Some(ternary_result);
        }

        let mut result = Vec::with_capacity(tokens.len());
        let mut i = 0;
        let len = tokens.len();

        while i < len {
            let token = &tokens[i];

            match token.as_str() {
                // Handle character literals (single chars that should be quoted)
                t if t.len() == 1 && t.chars().next().map(|c| c.is_alphabetic()).unwrap_or(false) => {
                    // Check context - if preceded by comparison with a char-like identifier
                    if i > 0 && i + 1 < len {
                        let prev = &tokens[i - 1];
                        if matches!(prev.as_str(), ">=" | "<=" | ">" | "<" | "==" | "!=") {
                            // This might be a character literal comparison
                            result.push(format!("'{}'", token));
                            i += 1;
                            continue;
                        }
                    }
                    result.push(token.clone());
                }
                // Handle literal suffixes like 1u, 1UL, etc.
                t if t.ends_with('u') || t.ends_with('U') => {
                    let num_part = t.trim_end_matches(|c| c == 'u' || c == 'U' || c == 'L' || c == 'l');
                    if num_part.parse::<i64>().is_ok() {
                        result.push(format!("{}u32", num_part));
                        i += 1;
                        continue;
                    }
                    result.push(token.clone());
                }
                "(" if i + 2 < len => {
                    if let Some(cp) = tokens[i..].iter().position(|t| t == ")") {
                        let cast_tokens: Vec<&str> =
                            tokens[i + 1..i + cp].iter().map(|s| s.as_str()).collect();

                        if Self::is_type_cast(&cast_tokens) && i + cp + 1 < len {
                            let cast_type = cast_tokens.join(" ");
                            // Try lookup with the full type first
                            let rust_type = system().lookup_type(&cast_type)
                                .or_else(|| {
                                    // If not found and starts with struct/enum/union, try without it
                                    let stripped = cast_type
                                        .trim_start_matches("struct ")
                                        .trim_start_matches("enum ")
                                        .trim_start_matches("union ");
                                    if stripped != cast_type {
                                        system().lookup_type(stripped)
                                    } else {
                                        None
                                    }
                                })
                                .unwrap_or_else(|| {
                                    // Fallback: strip struct/enum/union and handle pointer
                                    let mut t = cast_type.clone();
                                    t = t.replace("struct ", "").replace("enum ", "").replace("union ", "");
                                    if t.ends_with(" *") || t.ends_with("*") {
                                        let base = t.trim_end_matches(" *").trim_end_matches("*").trim();
                                        format!("*mut {}", base)
                                    } else {
                                        t
                                    }
                                });
                            result.push(format!("{} as {}", &tokens[i + cp + 1], rust_type));
                            i += cp + 2;
                            continue;
                        }
                    }
                    result.push(token.clone());
                }
                // C bitwise NOT ~ becomes Rust !
                "~" => {
                    result.push("!".to_string());
                }
                "++" | "--" => {
                    let op = if token == "++" { "+=" } else { "-=" };
                    if let Some(prev) = result.last_mut() {
                        *prev = format!("{} {} 1", prev, op);
                    } else if i + 1 < len {
                        result.push(format!("{} {} 1", &tokens[i + 1], op));
                        i += 2;
                        continue;
                    }
                }
                "->" | "- >" => {
                    if let Some(prev) = result.pop() {
                        if i + 1 < len {
                            result.push(format!("(*{}).{}", prev, &tokens[i + 1]));
                            i += 2;
                            continue;
                        }
                        result.push(prev);
                    }
                }
                "NULL" => result.push("std::ptr::null()".to_string()),
                "sizeof" => {
                    if i + 1 < len && tokens[i + 1] == "(" {
                        if let Some(cp) = tokens[i..].iter().position(|t| t == ")") {
                            let inner = tokens[i + 2..i + cp].join(" ");
                            // Use TypeConverter for proper C to Rust type conversion
                            let mut type_converter = crate::db::convert::TypeConverter::new();
                            let rust_type = if let Some(t) = type_converter.convert(&inner) {
                                // TypeConverter handles C builtin types (int -> i32, etc.)
                                t
                            } else if let Some(t) = system().lookup_type(&inner) {
                                // Then try registered custom types (structs, enums, typedefs)
                                t
                            } else if !crate::db::keyword::is_c_type_keyword(&inner) {
                                // Only check variable lookup if it's NOT a C type keyword
                                // (prevents "int" parameter from shadowing the type)
                                if let Some(var_type) = system().lookup_variable(&inner) {
                                    var_type
                                } else {
                                    inner
                                }
                            } else {
                                // Fall back to using the inner value as-is
                                inner
                            };
                            result.push(format!("std::mem::size_of::<{}>()", rust_type));
                            i += cp + 1;
                            continue;
                        }
                    }
                    result.push(token.clone());
                }
                _ => result.push(token.clone()),
            }
            i += 1;
        }

        self.state.set_confidence(0.8);
        Some(result.join(" "))
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

impl Build for ExpressionHandler {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("Handler", "ExpressionHandler");
        entry.set_attr("stage", self.state.stage().to_entry());
        entry.set_attr("confidence", Entry::f64(self.state.confidence));
        if let Some(ref output) = self.state.output {
            entry.set_attr("output", Entry::string(output));
        }
        if let Some(ref op) = self.state.data.operator {
            entry.set_attr("operator", Entry::string(op));
        }
        entry
    }
    fn kind(&self) -> &str { "Handler" }
    fn name(&self) -> Option<&str> { Some("ExpressionHandler") }
    fn category(&self) -> Option<&str> { Some("expression") }
}

// ============================================================================
// String Format Handler
// ============================================================================

#[derive(Debug, Clone, Default)]
pub struct StringFormatData {
    pub function_name: String,
    pub format_string: String,
    pub arguments: Vec<String>,
}

impl Build for StringFormatData {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("StringFormatData", "string_format");
        entry.set_attr("function_name", Entry::string(&self.function_name));
        entry
    }
    fn kind(&self) -> &str { "StringFormatData" }
    fn category(&self) -> Option<&str> { Some("expression") }
}

#[derive(Debug)]
pub struct StringFormatHandler {
    state: ProcessorState<StringFormatData>,
}

impl StringFormatHandler {
    pub fn new() -> Self {
        Self { state: ProcessorState::new("StringFormatHandler") }
    }

    fn convert_argument(arg: &str) -> String {
        use crate::handlers::common::{convert_arrow_operator, convert_c_style_cast};
        
        // First convert arrow operators, then C-style casts
        let result = convert_arrow_operator(arg);
        convert_c_style_cast(&result)
    }


    fn convert_format_string(&self, format: &str) -> String {
        let mut result = String::new();
        let mut chars = format.chars().peekable();
        let mut pos = 0;

        while pos < format.len() {
            if format[pos..].starts_with('%') {
                if let Some((spec, len)) = parse_format_specifier(format, pos) {
                    if spec.conversion == '%' { result.push('%'); }
                    else { result.push_str(&spec.to_rust()); }
                    pos += len;
                    for _ in 0..len { chars.next(); }
                    continue;
                }
            }
            if let Some(c) = chars.next() {
                if c == '\\' {
                    if let Some(&next) = chars.peek() {
                        match next {
                            'n' => { result.push('\n'); chars.next(); pos += 1; }
                            't' => { result.push('\t'); chars.next(); pos += 1; }
                            'r' => { result.push('\r'); chars.next(); pos += 1; }
                            '\\' => { result.push('\\'); chars.next(); pos += 1; }
                            '"' => { result.push('"'); chars.next(); pos += 1; }
                            _ => result.push(c),
                        }
                    } else { result.push(c); }
                } else { result.push(c); }
            }
            pos += 1;
        }
        result
    }

    fn get_rust_macro(&self) -> &str {
        match self.state.data.function_name.as_str() {
            "printf" => "print!",
            "puts" | "println" => "println!",
            "fprintf" => "write!",
            "sprintf" | "snprintf" => "format!",
            "eprintf" | "perror" => "eprint!",
            _ => "print!",
        }
    }

    fn is_println_style(&self) -> bool {
        self.state.data.format_string.ends_with("\\n") || self.state.data.format_string.ends_with('\n')
    }
}

impl Default for StringFormatHandler {
    fn default() -> Self { Self::new() }
}

impl Processor for StringFormatHandler {
    fn supported_patterns(&self) -> &[&str] {
        &["validate_printf_call", "extract_printf_call"]
    }

    fn stats(&self) -> &ProcessorStats {
        system().process_stats(&self.state.stats);
        &self.state.stats
    }

    fn patterns(&self) -> Vec<(Pattern, Pattern)> {
        vec![(
            Pattern::definition(700, "validate_printf_call", vec![
                PatternRule::one_of(vec!["printf".into(), "fprintf".into(), "sprintf".into(), "snprintf".into(), "puts".into()]),
                PatternRule::exact("("),
                PatternRule::any().optional().repeat(-1),
                PatternRule::exact(")"),
            ]).with_category("expression").with_priority(180).with_min_tokens(4),
            Pattern::definition(700, "extract_printf_call", vec![
                PatternRule::one_of(vec!["printf".into(), "fprintf".into(), "sprintf".into(), "snprintf".into(), "puts".into()]),
                PatternRule::exact("("),
                PatternRule::any().optional().repeat(-1),
                PatternRule::exact(")"),
            ]).with_category("expression").with_priority(180).with_min_tokens(4),
        )]
    }

    fn validate(&mut self, tokens: &[Token]) -> bool {
        if tokens.len() < 4 { return false; }
        let first = tokens[0].to_string();
        if !matches!(first.as_str(), "printf" | "fprintf" | "sprintf" | "snprintf" | "puts") { return false; }

        let mut has_open = false;
        let mut has_close = false;
        for token in tokens.iter().skip(1) {
            let s = token.to_string();
            if s == "(" { has_open = true; }
            else if s == ")" { has_close = true; break; }
        }
        if !has_open || !has_close { return false; }
        self.state.set_confidence(0.9);
        true
    }

    fn extract(&mut self, tokens: &[Token]) -> bool {
        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        self.state.data.function_name = token_strs[0].clone();

        let open_paren = match token_strs.iter().position(|t| t == "(") { Some(p) => p, None => return false };
        let close_paren = match token_strs.iter().rposition(|t| t == ")") { Some(p) => p, None => return false };
        if close_paren <= open_paren + 1 { return false; }

        let args = &token_strs[open_paren + 1..close_paren];
        if args.is_empty() { return false; }

        let format_start = if matches!(self.state.data.function_name.as_str(), "fprintf" | "snprintf") {
            args.iter().position(|t| t == ",").map(|p| p + 1).unwrap_or(0)
        } else { 0 };

        if format_start >= args.len() { return false; }

        let format_args = &args[format_start..];
        let (format_str, format_end) = if format_args.is_empty() { (String::new(), 0) }
        else {
            let first = &format_args[0];
            if first.starts_with('"') && first.ends_with('"') && first.len() > 1 {
                (first[1..first.len() - 1].to_string(), 0)
            } else if first.starts_with('"') {
                let mut format = first[1..].to_string();
                let mut end_idx = format_args.len() - 1;
                for (i, arg) in format_args.iter().enumerate().skip(1) {
                    if arg.ends_with('"') && !arg.ends_with("\\\"") {
                        format.push_str(&arg[..arg.len() - 1]);
                        end_idx = i;
                        break;
                    } else if arg != "," { format.push_str(arg); }
                }
                (format, end_idx)
            } else { (first.clone(), 0) }
        };
        self.state.data.format_string = format_str;

        let arg_start = format_start + format_end + 1;
        if arg_start < args.len() {
            let arg_tokens = &args[arg_start..];
            let mut current_arg = String::new();
            let mut paren_depth = 0;

            for token in arg_tokens {
                match token.as_str() {
                    "(" => { 
                        paren_depth += 1; 
                        // No space before '('
                        current_arg.push_str(token); 
                    }
                    ")" => { 
                        paren_depth -= 1; 
                        // No space before ')'
                        current_arg.push_str(token); 
                    }
                    "," if paren_depth == 0 => {
                        let trimmed = current_arg.trim().to_string();
                        if !trimmed.is_empty() { self.state.data.arguments.push(trimmed); }
                        current_arg.clear();
                    }
                    "*" | "&" => {
                        // No space before/after pointer/reference operators when adjacent to parens
                        current_arg.push_str(token);
                    }
                    _ => { 
                        // Add space only if not after '(' or before special chars
                        let last_char = current_arg.chars().last();
                        if !current_arg.is_empty() && last_char != Some('(') && last_char != Some('*') && last_char != Some('&') {
                            current_arg.push(' '); 
                        }
                        current_arg.push_str(token); 
                    }
                }
            }
            let trimmed = current_arg.trim().to_string();
            if !trimmed.is_empty() { self.state.data.arguments.push(trimmed); }
        }

        self.state.set_confidence(0.85);
        true
    }

    fn convert(&mut self) -> Option<String> {
        let format_string = self.state.data.format_string.clone();
        let function_name = self.state.data.function_name.clone();
        let arguments = self.state.data.arguments.clone();
        
        let rust_format = self.convert_format_string(&format_string);
        let is_println = format_string.ends_with("\\n") || format_string.ends_with('\n');
        let macro_name = if is_println && function_name == "printf" { "println!" } else {
            match function_name.as_str() {
                "printf" => "print!",
                "puts" | "println" => "println!",
                "fprintf" => "write!",
                "sprintf" | "snprintf" => "format!",
                "eprintf" | "perror" => "eprint!",
                _ => "print!",
            }
        };
        let format_str = if is_println && rust_format.ends_with('\n') { 
            rust_format[..rust_format.len() - 1].to_string() 
        } else { rust_format };

        let converted_args: Vec<String> = arguments.iter().map(|arg| Self::convert_argument(arg)).collect();
        let args = if converted_args.is_empty() { String::new() } else { format!(", {}", converted_args.join(", ")) };

        self.state.set_confidence(0.9);
        Some(format!("{}(\"{}\"{})", macro_name, format_str, args))
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

impl Build for StringFormatHandler {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("Handler", "StringFormatHandler");
        entry.set_attr("stage", self.state.stage().to_entry());
        entry.set_attr("confidence", Entry::f64(self.state.confidence));
        if let Some(ref output) = self.state.output { entry.set_attr("output", Entry::string(output)); }
        entry
    }
    fn kind(&self) -> &str { "Handler" }
    fn name(&self) -> Option<&str> { Some("StringFormatHandler") }
    fn category(&self) -> Option<&str> { Some("expression") }
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
    fn test_simple_addition() {
        let tokens = make_tokens(&["1", "+", "2"]);
        let mut handler = ExpressionHandler::new();
        assert!(handler.validate(&tokens));
        assert!(handler.extract(&tokens));
        assert!(handler.convert().unwrap().contains("+"));
    }

    #[test]
    fn test_comparison_converter() {
        let mut conv = ComparisonConverter::new();
        assert_eq!(conv.convert(&["x", ">", "0"]), "x > 0");
        assert_eq!(conv.convert(&["ptr", "==", "NULL"]), "ptr.is_null()");
        assert_eq!(conv.convert(&["ptr", "!=", "NULL"]), "!ptr.is_null()");
    }

    #[test]
    fn test_logical_expression() {
        let mut conv = ComparisonConverter::new();
        assert_eq!(conv.convert_logical(&["x", ">", "0", "&&", "x", "<", "10"]), "x > 0 && x < 10");
    }

    #[test]
    fn test_format_specifier() {
        let (spec, len) = parse_format_specifier("%d", 0).unwrap();
        assert_eq!(spec.conversion, 'd');
        assert_eq!(len, 2);
    }

    #[test]
    fn test_format_conversion() {
        let handler = StringFormatHandler::new();
        assert_eq!(handler.convert_format_string("Value: %d"), "Value: {}");
        assert_eq!(handler.convert_format_string("%s = %d"), "{} = {}");
    }
}
