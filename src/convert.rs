use crate::common::{convert_type, convert_type_tokens};
use crate::error::Result;
use crate::{Context, Token};
use std::fmt;
/// Represents different types of converted Rust elements
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ConvertedElement {
    Function(ConvertedFunction),
    Struct(ConvertedStruct),
    Enum(ConvertedEnum),
    Typedef(ConvertedTypedef),
    Global(ConvertedGlobal),
    Macro(ConvertedMacro),
    Include(ConvertedInclude),
    Array(ConvertedArray),
    Comment(ConvertedComment),
    Expression(ConvertedExpression),
    ControlFlow(ConvertedControlFlow),
    Variable(ConvertedVariable),
}

/// Represents a converted Rust function
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConvertedFunction {
    pub code: String,
    pub metadata: Vec<(String, String)>,
}

impl fmt::Display for ConvertedFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.code)
    }
}

/// Represents a converted Rust struct
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConvertedStruct {
    pub code: String,
    pub metadata: Vec<(String, String)>,
}

impl fmt::Display for ConvertedStruct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.code)
    }
}

/// Represents a converted Rust enum
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConvertedEnum {
    pub code: String,
    pub metadata: Vec<(String, String)>,
}

impl fmt::Display for ConvertedEnum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.code)
    }
}

/// Represents a converted Rust type alias
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConvertedTypedef {
    pub code: String,
    pub metadata: Vec<(String, String)>,
}

impl fmt::Display for ConvertedTypedef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.code)
    }
}

/// Represents a converted Rust global variable
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConvertedGlobal {
    pub code: String,
    pub metadata: Vec<(String, String)>,
}

impl fmt::Display for ConvertedGlobal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.code)
    }
}

/// Represents a converted Rust macro
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConvertedMacro {
    pub code: String,
    pub metadata: Vec<(String, String)>,
}

impl fmt::Display for ConvertedMacro {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.code)
    }
}

/// Represents a converted Rust use statement
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConvertedInclude {
    pub code: String,
    pub metadata: Vec<(String, String)>,
}

impl fmt::Display for ConvertedInclude {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.code)
    }
}

/// Represents a converted Rust array
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConvertedArray {
    pub code: String,
    pub metadata: Vec<(String, String)>,
}

impl fmt::Display for ConvertedArray {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.code)
    }
}

/// Represents a converted Rust comment
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConvertedComment {
    pub code: String,
    pub metadata: Vec<(String, String)>,
}

impl fmt::Display for ConvertedComment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.code)
    }
}

/// Represents a converted Rust expression
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConvertedExpression {
    pub code: String,
    pub metadata: Vec<(String, String)>,
}

impl fmt::Display for ConvertedExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.code)
    }
}

/// Represents a converted Rust control flow statement
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConvertedControlFlow {
    pub code: String,
    pub metadata: Vec<(String, String)>,
}

impl fmt::Display for ConvertedControlFlow {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.code)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConvertedVariable {
    pub code: String,
    pub metadata: Vec<(String, String)>,
}

impl fmt::Display for ConvertedVariable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.code)
    }
}


impl fmt::Display for ConvertedElement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ConvertedElement::Function(func) => write!(f, "{}", func),
            ConvertedElement::Struct(s) => write!(f, "{}", s),
            ConvertedElement::Enum(e) => write!(f, "{}", e),
            ConvertedElement::Typedef(t) => write!(f, "{}", t),
            ConvertedElement::Global(g) => write!(f, "{}", g),
            ConvertedElement::Macro(m) => write!(f, "{}", m.code),
            ConvertedElement::Include(i) => write!(f, "{}", i.code),
            ConvertedElement::Array(a) => write!(f, "{}", a.code),
            ConvertedElement::Comment(c) => write!(f, "{}", c.code),
            ConvertedElement::Expression(e) => write!(f, "{}", e.code),
            ConvertedElement::ControlFlow(cf) => write!(f, "{}", cf.code),
            ConvertedElement::Variable(v) => write!(f, "{}", v.code),
        }
    }
}
/// Convert using pattern analysis from Patternizer, Analyzer, and Samplizer
/// Convert using pattern analysis from Patternizer, Analyzer, and Samplizer
pub fn convert_all(
    context: &mut Context,
    handler_type: &str,
    tokens: &[Token],
    samplizer_result: Option<Result<crate::ConfidenceResult>>,
    patterns: Option<&Vec<crate::SamplizerPattern>>,
    adaptive_result: Option<&crate::AdaptivePatternResult>,
) -> String {
    let mut rust_code = String::with_capacity(512);
    let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

    // Use Patternizer for enhanced pattern detection
    let best_match = context.patternizer.match_best_pattern(
        &["function", "struct", "enum", "array", "typedef", "global", "expression"],
        tokens,
    );

    // Fast path: High-confidence adaptive result with Patternizer validation
    if let Some(adaptive_result) = adaptive_result {
        if adaptive_result.confidence > 0.8 {
            if let crate::PatternResult::Match { consumed_tokens } = &adaptive_result.base_result {
                // Learn from successful adaptive pattern
                if let Some((matched_handler, _)) = &best_match {
                    if let Some(patterns) = context.patternizer.get_patterns_for_handler(matched_handler) {
                        let mut patternizer = context.patternizer.clone();
                        if let Some(pattern) = patterns.first() {
                            patternizer.learn_from_adaptive_success(
                                context,
                                matched_handler,
                                pattern.clone(),
                                adaptive_result.confidence,
                            );
                            context.patternizer = patternizer;
                        }
                    }
                }

                rust_code.push_str(&format!(
                    "// High-confidence adaptive match: {}/{} tokens (confidence: {:.2})\n",
                    consumed_tokens, tokens.len(), adaptive_result.confidence
                ));
                return rust_code;
            }
        }

        rust_code.push_str(&format!(
            "// Adaptive match: {} tokens consumed (confidence: {:.2})\n",
            adaptive_result.tokens_consumed, adaptive_result.confidence
        ));
    }

    // Use best pattern match from Patternizer
    if let Some((matched_handler, pattern_result)) = best_match {
        rust_code.push_str(&format!(
            "// Patternizer matched: {} with {:?}\n",
            matched_handler, pattern_result
        ));

        return rust_code + &match matched_handler.as_str() {
            "function" => convert_function_tokens(tokens),
            "struct" => convert_struct_tokens(tokens),
            "enum" => convert_enum_tokens(tokens),
            "array" => convert_array_tokens(tokens),
            "typedef" => convert_typedef_tokens(tokens),
            "global" => convert_global_tokens(tokens),
            "expression" => convert_expression_tokens(tokens),
            _ => convert_generic_tokens(tokens),
        };
    }

    // Fallback: High-confidence samplizer pattern
    if let Some(patterns) = patterns {
        if let Some(best_pattern) = patterns.iter()
            .filter(|p| p.confidence_score > 0.7)
            .max_by(|a, b| a.confidence_score.partial_cmp(&b.confidence_score).unwrap())
        {
            let pattern_relevance = calculate_pattern_relevance(&best_pattern.c_tokens, &token_strings);
            if pattern_relevance > 0.6 {
                rust_code.push_str(&format!(
                    "// Samplizer pattern: {} (confidence: {:.2}, relevance: {:.2})\n",
                    best_pattern.pattern_name, best_pattern.confidence_score, pattern_relevance
                ));

                return rust_code + &match best_pattern.pattern_type {
                    crate::SegmentType::Function => convert_function_tokens(tokens),
                    crate::SegmentType::Struct => convert_struct_tokens(tokens),
                    crate::SegmentType::Enum => convert_enum_tokens(tokens),
                    crate::SegmentType::Array => convert_array_tokens(tokens),
                    crate::SegmentType::TypeDef => convert_typedef_tokens(tokens),
                    crate::SegmentType::Variable => convert_global_tokens(tokens),
                    crate::SegmentType::Expression => convert_expression_tokens(tokens),
                    _ => convert_generic_tokens(tokens),
                };
            }
        }
    }

    // Final fallback: Handler-specific conversion
    rust_code.push_str(&format!("// Handler type: {}\n", handler_type));

    if let Some(Ok(confidence_result)) = &samplizer_result {
        rust_code.push_str(&format!(
            "// Samplizer confidence: {:.2}\n",
            confidence_result.overall_confidence
        ));
    }

    rust_code.push_str(&match handler_type {
        "function" => convert_function_tokens(tokens),
        "struct" => convert_struct_tokens(tokens),
        "enum" => convert_enum_tokens(tokens),
        "array" => convert_array_tokens(tokens),
        "typedef" => convert_typedef_tokens(tokens),
        "global" => convert_global_tokens(tokens),
        "expression" => convert_expression_tokens(tokens),
        _ => {
            let unknown_msg = format!("// Unknown pattern type: {}\n", handler_type);
            unknown_msg + &convert_generic_tokens(tokens)
        }
    });

    rust_code
}

fn calculate_pattern_relevance(c_tokens: &[String], token_strings: &[String]) -> f64 {
    if c_tokens.is_empty() || token_strings.is_empty() {
        return 0.0;
    }

    let mut matches = 0;
    let mut total_comparisons = 0;

    // Calculate token overlap
    for c_token in c_tokens {
        for token in token_strings {
            total_comparisons += 1;
            if c_token == token {
                matches += 1;
            }
        }
    }

    // Calculate relevance score based on matches and sequence similarity
    let overlap_score = if total_comparisons > 0 {
        matches as f64 / total_comparisons as f64
    } else {
        0.0
    };

    // Bonus for similar sequence length
    let length_similarity = 1.0 - ((c_tokens.len() as f64 - token_strings.len() as f64).abs() / c_tokens.len().max(token_strings.len()) as f64);

    // Weighted average
    (overlap_score * 0.7 + length_similarity * 0.3).clamp(0.0, 1.0)
}
/// Convert function tokens
fn convert_function_tokens(tokens: &[Token]) -> String {
    if tokens.is_empty() {
        return "// Empty function tokens\n".to_string();
    }

    let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
    let mut rust_code = String::with_capacity(256);

    // Fast path for common function patterns
    if let Some(result) = convert_function_pattern(&token_strings) {
        rust_code.push_str(&result);
        return rust_code;
    }

    // Fallback with type analysis
    if let Some(converted) = convert_type_tokens(tokens.to_vec()) {
        rust_code.push_str(&format!("// Type converted: {}\n", converted));
    }

    rust_code.push_str(&format!(
        "// Function conversion failed for: {}\n",
        token_strings.join(" ")
    ));

    rust_code
}

/// Convert struct tokens
fn convert_struct_tokens(tokens: &[Token]) -> String {
    let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
    convert_struct_pattern(&token_strings).unwrap_or_else(|| {
        format!(
            "// Struct conversion failed for: {}\n",
            token_strings.join(" ")
        )
    })
}

/// Convert enum tokens  
fn convert_enum_tokens(tokens: &[Token]) -> String {
    let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
    convert_enum_pattern(&token_strings).unwrap_or_else(|| {
        format!(
            "// Enum conversion failed for: {}\n",
            token_strings.join(" ")
        )
    })
}

/// Convert array tokens
fn convert_array_tokens(tokens: &[Token]) -> String {
    let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
    convert_array_pattern(&token_strings).unwrap_or_else(|| {
        format!(
            "// Array conversion failed for: {}\n",
            token_strings.join(" ")
        )
    })
}

/// Convert typedef tokens
fn convert_typedef_tokens(tokens: &[Token]) -> String {
    let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
    convert_typedef_pattern(&token_strings).unwrap_or_else(|| {
        format!(
            "// Typedef conversion failed for: {}\n",
            token_strings.join(" ")
        )
    })
}

/// Convert global variable tokens
fn convert_global_tokens(tokens: &[Token]) -> String {
    let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
    convert_variable_pattern(&token_strings).unwrap_or_else(|| {
        format!(
            "// Global variable conversion failed for: {}\n",
            token_strings.join(" ")
        )
    })
}

/// Convert expression tokens
fn convert_expression_tokens(tokens: &[Token]) -> String {
    let mut rust_code = String::new();
    let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

    // Check for common C expressions and convert them
    if token_strings.contains(&"=".to_string()) {
        rust_code.push_str(&convert_assignment_expression(&token_strings));
    } else if token_strings.contains(&"++".to_string()) || token_strings.contains(&"--".to_string())
    {
        rust_code.push_str(&convert_increment_expression(&token_strings));
    } else {
        rust_code.push_str(&format!("// Expression: {}\n", token_strings.join(" ")));
    }

    rust_code
}

/// Convert assignment expressions
fn convert_assignment_expression(tokens: &[String]) -> String {
    if let Some(eq_pos) = tokens.iter().position(|t| t == "=") {
        if eq_pos > 0 && eq_pos + 1 < tokens.len() {
            let var_name = &tokens[eq_pos - 1];
            let value = &tokens[eq_pos + 1];
            return format!("{} = {};\n", var_name, value);
        }
    }
    format!("// Assignment: {}\n", tokens.join(" "))
}

/// Convert increment/decrement expressions
fn convert_increment_expression(tokens: &[String]) -> String {
    let tokens_str = tokens.join(" ");
    if tokens_str.contains("++") {
        format!(
            "// Increment: {} // TODO: Convert C++ to Rust += 1\n",
            tokens_str
        )
    } else if tokens_str.contains("--") {
        format!(
            "// Decrement: {} // TODO: Convert C-- to Rust -= 1\n",
            tokens_str
        )
    } else {
        format!("// Increment/Decrement: {}\n", tokens_str)
    }
}

/// Convert with contextual information

/// Convert function declarations and definitions
fn convert_function_pattern(tokens: &[String]) -> Option<String> {
    if tokens.len() < 4 {
        return None;
    }

    // Pattern: return_type function_name ( params ) { ... }
    // Handle main function specially
    if tokens.len() >= 4 && tokens[0] == "int" && tokens[1] == "main" {
        let mut result = String::from("fn main() -> i32 {\n");

        // Find opening and closing braces
        let open_brace = tokens.iter().position(|t| t == "{")?;
        let close_brace = tokens.iter().rposition(|t| t == "}")?;

        if open_brace < close_brace {
            let body: Vec<String> = tokens[(open_brace + 1)..close_brace].to_vec();
            result.push_str(&convert_function_body(&body));
        }

        result.push_str("}\n");
        return Some(result);
    }

    // Generic function pattern: type name ( ... )
    if let Some(open_paren) = tokens.iter().position(|t| t == "(") {
        if open_paren >= 2 {
            let return_type = convert_c_type(&tokens[0]);
            let function_name = &tokens[1];

            let mut result = String::new();
            result.push_str(&format!("fn {}(", function_name));

            // Find closing parenthesis and extract parameters
            if let Some(close_paren) = tokens.iter().position(|t| t == ")") {
                if close_paren > open_paren + 1 {
                    let params: Vec<String> = tokens[(open_paren + 1)..close_paren].to_vec();
                    result.push_str(&convert_parameters(&params));
                }
            }

            result.push_str(&format!(") -> {} {{\n", return_type));

            // Add function body if present
            if let Some(open_brace) = tokens.iter().position(|t| t == "{") {
                if let Some(close_brace) = tokens.iter().rposition(|t| t == "}") {
                    if open_brace < close_brace {
                        let body: Vec<String> = tokens[(open_brace + 1)..close_brace].to_vec();
                        result.push_str(&convert_function_body(&body));
                    }
                }
            }

            result.push_str("}\n");
            return Some(result);
        }
    }

    None
}

/// Convert struct definitions
fn convert_struct_pattern(tokens: &[String]) -> Option<String> {
    // Pattern: struct name { ... } or typedef struct { ... } name
    if tokens.is_empty() || tokens[0] != "struct" {
        return None;
    }

    let mut result = String::new();

    if tokens.len() > 1 {
        let struct_name = &tokens[1];
        result.push_str(&format!(
            "#[derive(Debug, Clone)]\npub struct {} {{\n",
            struct_name
        ));

        // Find struct body
        if let Some(open_brace) = tokens.iter().position(|t| t == "{") {
            if let Some(close_brace) = tokens.iter().rposition(|t| t == "}") {
                if open_brace < close_brace {
                    let fields: Vec<String> = tokens[(open_brace + 1)..close_brace].to_vec();
                    result.push_str(&convert_struct_fields(&fields));
                }
            }
        }

        result.push_str("}\n");
        return Some(result);
    }

    None
}

/// Convert enum definitions
fn convert_enum_pattern(tokens: &[String]) -> Option<String> {
    // Pattern: enum name { ... }
    if tokens.is_empty() || tokens[0] != "enum" {
        return None;
    }

    if tokens.len() > 1 {
        let enum_name = &tokens[1];
        let mut result = format!(
            "#[derive(Debug, Clone, Copy, PartialEq, Eq)]\npub enum {} {{\n",
            enum_name
        );

        // Find enum body
        if let Some(open_brace) = tokens.iter().position(|t| t == "{") {
            if let Some(close_brace) = tokens.iter().rposition(|t| t == "}") {
                if open_brace < close_brace {
                    let variants: Vec<String> = tokens[(open_brace + 1)..close_brace].to_vec();
                    result.push_str(&convert_enum_variants(&variants));
                }
            }
        }

        result.push_str("}\n");
        return Some(result);
    }

    None
}

/// Convert typedef statements
fn convert_typedef_pattern(tokens: &[String]) -> Option<String> {
    // Pattern: typedef existing_type new_type;
    if tokens.is_empty() || tokens[0] != "typedef" {
        return None;
    }

    if tokens.len() >= 3 {
        let existing_type = convert_c_type(&tokens[1]);
        let new_type = &tokens[2];
        return Some(format!("pub type {} = {};\n", new_type, existing_type));
    }

    None
}

/// Convert variable declarations
fn convert_variable_pattern(tokens: &[String]) -> Option<String> {
    if tokens.len() < 2 {
        return None;
    }

    // Simple pattern: type name; or type name = value;
    let rust_type = convert_c_type(&tokens[0]);
    let var_name = &tokens[1];

    if tokens.len() >= 4 && tokens[2] == "=" {
        let value = &tokens[3];
        Some(format!("let {}: {} = {};\n", var_name, rust_type, value))
    } else {
        Some(format!("let {}: {};\n", var_name, rust_type))
    }
}

/// Convert array declarations
fn convert_array_pattern(tokens: &[String]) -> Option<String> {
    if tokens.len() < 4 {
        return None;
    }

    // Pattern: type name[size] or type name[]
    if let Some(bracket_pos) = tokens.iter().position(|t| t.contains('[')) {
        if bracket_pos >= 2 {
            let element_type = convert_c_type(&tokens[0]);
            let array_name = &tokens[1];

            if tokens[bracket_pos].contains(']') {
                // Extract size if present
                let size_part = &tokens[bracket_pos];
                if let Some(start) = size_part.find('[') {
                    if let Some(end) = size_part.find(']') {
                        let size_str = &size_part[start + 1..end];
                        return if size_str.is_empty() {
                            Some(format!(
                                "let {}: Vec<{}> = Vec::new();\n",
                                array_name, element_type
                            ))
                        } else {
                            Some(format!(
                                "let {}: [{}; {}] = [Default::default(); {}];\n",
                                array_name, element_type, size_str, size_str
                            ))
                        }
                    }
                }
            }
        }
    }

    None
}

/// Convert include statements  
fn convert_include_pattern(tokens: &[String]) -> Option<String> {
    if tokens.is_empty() || tokens[0] != "#include" {
        return None;
    }

    if tokens.len() > 1 {
        let header = &tokens[1];
        if header.starts_with('<') && header.ends_with('>') {
            let header_name = &header[1..header.len() - 1];
            return Some(convert_include_to_use(header_name));
        } else if header.starts_with('"') && header.ends_with('"') {
            let header_name = &header[1..header.len() - 1];
            return Some(format!("mod {};\n", header_name.replace(".h", "")));
        }
    }

    None
}

/// Generic pattern conversion with improved formatting
fn convert_generic_tokens(tokens: &[Token]) -> String {
    if tokens.is_empty() {
        return String::from("// Empty token sequence\n");
    }

    let mut result = String::new();
    result.push_str(&format!("// Original C code ({} tokens):\n", tokens.len()));
    result.push_str("// ");

    let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
    result.push_str(&token_strings.join(" "));
    result.push_str("\n// TODO: Implement proper conversion for this pattern\n");

    result
}

/// Convert C types to Rust types
fn convert_c_type(c_type: &str) -> String {
    // First check if we have a direct mapping in our type map
    if let Some(rust_type) = convert_type(c_type) {
        return rust_type;
    }

    let trimmed = c_type.trim();

    // Handle pointer types
    if trimmed.ends_with('*') {
        let base_type = trimmed[..trimmed.len() - 1].trim();

        // Special cases for common pointer patterns
        return match base_type {
            "char" => "String".to_string(),
            "const char" => "&str".to_string(),
            "void" => "*mut ()".to_string(),
            _ => {
                let converted_base = convert_c_type(base_type);
                format!("*mut {}", converted_base)
            }
        }
    }

    // Handle array types
    if let Some(bracket_pos) = trimmed.find('[') {
        let base_type = trimmed[..bracket_pos].trim();
        let converted_base = convert_c_type(base_type);
        return format!("[{}]", converted_base);
    }

    // Handle const qualifier
    if trimmed.starts_with("const ") {
        let base_type = &trimmed[6..];
        return convert_c_type(base_type);
    }

    // Handle unsigned/signed qualifiers
    if trimmed.starts_with("unsigned ") {
        let base_type = &trimmed[9..];
        return convert_c_type(&format!("u{}", base_type));
    }

    if trimmed.starts_with("signed ") {
        let base_type = &trimmed[7..];
        return convert_c_type(base_type);
    }

    // Fallback to original type name
    trimmed.to_string()
}

/// Convert function body statements
fn convert_function_body(body_tokens: &[String]) -> String {
    let mut result = String::new();
    let mut i = 0;

    while i < body_tokens.len() {
        if body_tokens[i] == "return" {
            if i + 1 < body_tokens.len() {
                let return_value = &body_tokens[i + 1];
                result.push_str(&format!("    {}\n", return_value));
                i += 2;
            } else {
                result.push_str("    return;\n");
                i += 1;
            }
        } else if body_tokens[i] == "printf" {
            // Convert printf to println!
            result.push_str("    println!(");
            i += 1;
            // Skip opening parenthesis if present
            if i < body_tokens.len() && body_tokens[i] == "(" {
                i += 1;
            }
            // Collect arguments until closing parenthesis or semicolon
            while i < body_tokens.len() && body_tokens[i] != ")" && body_tokens[i] != ";" {
                result.push_str(&body_tokens[i]);
                i += 1;
                if i < body_tokens.len() && body_tokens[i] != ")" && body_tokens[i] != ";" {
                    result.push_str(", ");
                }
            }
            result.push_str(");\n");
            i += 1;
        } else {
            // Generic statement conversion
            result.push_str("    ");
            result.push_str(&body_tokens[i]);
            if i + 1 < body_tokens.len() && body_tokens[i + 1] != ";" {
                result.push(' ');
            }
            i += 1;
        }
    }

    if result.is_empty() {
        result.push_str("    // TODO: Convert function body\n");
    }

    result
}

/// Convert function parameters
fn convert_parameters(param_tokens: &[String]) -> String {
    if param_tokens.is_empty() || (param_tokens.len() == 1 && param_tokens[0] == "void") {
        return String::new();
    }

    let mut result = String::new();
    let mut i = 0;
    let mut param_count = 0;

    while i < param_tokens.len() {
        if i + 1 < param_tokens.len() {
            let param_type = convert_c_type(&param_tokens[i]);
            let param_name = &param_tokens[i + 1];

            if param_count > 0 {
                result.push_str(", ");
            }
            result.push_str(&format!("{}: {}", param_name, param_type));
            param_count += 1;
            i += 2;

            // Skip comma if present
            if i < param_tokens.len() && param_tokens[i] == "," {
                i += 1;
            }
        } else {
            i += 1;
        }
    }

    result
}

/// Convert struct fields
fn convert_struct_fields(field_tokens: &[String]) -> String {
    let mut result = String::new();
    let mut i = 0;

    while i < field_tokens.len() {
        if i + 1 < field_tokens.len() {
            let field_type = convert_c_type(&field_tokens[i]);
            let field_name = &field_tokens[i + 1];

            result.push_str(&format!("    pub {}: {},\n", field_name, field_type));
            i += 2;

            // Skip semicolon if present
            if i < field_tokens.len() && field_tokens[i] == ";" {
                i += 1;
            }
        } else {
            i += 1;
        }
    }

    if result.is_empty() {
        result.push_str("    // TODO: Convert struct fields\n");
    }

    result
}

/// Convert enum variants
fn convert_enum_variants(variant_tokens: &[String]) -> String {
    let mut result = String::new();
    let mut i = 0;

    while i < variant_tokens.len() {
        let variant_name = &variant_tokens[i];

        // Skip commas and values for now, just extract names
        if variant_name != ","
            && variant_name != "="
            && !variant_name.chars().all(|c| c.is_ascii_digit())
        {
            result.push_str(&format!("    {},\n", variant_name));
        }
        i += 1;
    }

    if result.is_empty() {
        result.push_str("    // TODO: Convert enum variants\n");
    }

    result
}

/// Convert include statements to Rust use statements
fn convert_include_to_use(header_name: &str) -> String {
    match header_name {
        "stdio.h" => "use std::io::*;\n".to_string(),
        "stdlib.h" => "use std::process;\nuse std::mem;\n".to_string(),
        "string.h" => "use std::ffi::CString;\n".to_string(),
        "math.h" => "use std::f64::consts::*;\n".to_string(),
        "time.h" => "use std::time::*;\n".to_string(),
        "unistd.h" => "use std::os::unix::*;\n".to_string(),
        _ => format!("// TODO: Convert #include <{}>\n", header_name),
    }
}
