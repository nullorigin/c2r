use crate::{Context, SegmentType, Token};
use std::cell::LazyCell;
use std::collections::HashMap;
use std::fmt;
use std::ops::Range;
use crate::error::Result;
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
}

/// Represents a converted Rust function
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConvertedFunction {
    pub name: String,
    pub return_type: String,
    pub parameters: Vec<String>,
    pub body: String,
    pub code: String,
    pub is_unsafe: bool,
    pub is_public: bool,
    pub is_definition: bool,
}

impl fmt::Display for ConvertedFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.code)
    }
}

/// Represents a converted Rust struct
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConvertedStruct {
    pub fields: Vec<(String, String)>, // (field_name, field_type)
    pub code: String,
    pub is_public: bool,
    pub derives: Vec<String>,
}

impl fmt::Display for ConvertedStruct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.code)
    }
}

/// Represents a converted Rust enum
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConvertedEnum {
    pub variants: Vec<(String, Option<String>)>, // (name, value)
    pub code: String,
    pub is_public: bool,
    pub repr: Option<String>,
}

impl fmt::Display for ConvertedEnum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.code)
    }
}

/// Represents a converted Rust type alias
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConvertedTypedef {
    pub target_type: String,
    pub code: String,
    pub is_public: bool,
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
    pub var_type: String,
    pub initializer: Option<String>,
    pub code: String,
    pub is_const: bool,
    pub is_static: bool,
    pub is_public: bool,
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
    pub parameters: Vec<String>,
    pub body: String,
    pub code: String,
    pub is_function_like: bool,
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
    pub path: String,
    pub code: String,
    pub is_external_crate: bool,
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
    pub element_type: String,
    pub size: String,
    pub code: String,
    pub is_declaration: bool,
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
    pub is_block: bool,
    pub is_doc_comment: bool,
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
    pub expression_type: String,
    pub left_operand: String,
    pub operator: String,
    pub right_operand: String,
    pub code: String,
    pub result_type: Option<String>,
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
    pub control_type: String,
    pub condition: String,
    pub body: String,
    pub code: String,
    pub has_else: bool,
    pub metadata: Vec<(String, String)>,
}

impl fmt::Display for ConvertedControlFlow {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.code)
    }
}

pub const MAP: LazyCell<HashMap<String, String>> = LazyCell::new(|| create_map());
pub fn is_type_token(token: &Token) -> bool {
    MAP.contains_key(token.to_string().as_str())
}
pub fn convert_type(c_type: &str) -> Option<String> {
    MAP.get(c_type).map(|rust_type| rust_type.to_string())
}

pub fn convert_type_token(c_token: &Token) -> Option<String> {
    let type_str = c_token.to_string();
    MAP.get(type_str.trim())
        .map(|rust_type| rust_type.to_string())
}

pub fn convert_type_tokens(c_type: Vec<Token>) -> Option<Token> {
    let mut c_type_str: String = String::new();
    for t in c_type.iter() {
        let s = t.to_string();
        if s.trim() != "**"
            && s.trim() != "&&"
            && s.trim() != "*"
            && s.trim() != "&"
            && s.trim() != ""
        {
            c_type_str.push_str(s.trim());
            c_type_str.push(' ');
        } else {
            c_type_str.push_str(s.trim());
        }
    }
    MAP.get(&c_type_str)
        .map(|rust_type| Token::s(rust_type.to_string()))
}

pub fn is_c_keyword(token: Token) -> bool {
    [
        "auto".to_string(),
        "break".to_string(),
        "case".to_string(),
        "char".to_string(),
        "const".to_string(),
        "continue".to_string(),
        "default".to_string(),
        "do".to_string(),
        "else".to_string(),
        "enum".to_string(),
        "extern".to_string(),
        "float".to_string(),
        "for".to_string(),
        "goto".to_string(),
        "if".to_string(),
        "inline".to_string(),
        "int".to_string(),
        "long".to_string(),
        "register".to_string(),
        "return".to_string(),
        "signed".to_string(),
        "sizeof".to_string(),
        "static".to_string(),
        "struct".to_string(),
        "switch".to_string(),
        "typedef".to_string(),
        "union".to_string(),
        "unsigned".to_string(),
        "volatile".to_string(),
        "while".to_string(),
    ]
    .contains(&token.to_string())
}
pub fn create_map() -> HashMap<String, String> {
    let mut hm1: HashMap<&str, &str> = HashMap::new();
    // Basic C types
    hm1.insert("bool", "bool");
    hm1.insert("_Bool", "bool");
    hm1.insert("char", "i8");
    hm1.insert("short", "i16");
    hm1.insert("int", "i32");
    hm1.insert("long", "i64");
    hm1.insert("long long", "i64");
    hm1.insert("float", "f32");
    hm1.insert("double", "f64");
    hm1.insert("void", "()");

    // Size types
    hm1.insert("size_t", "usize");
    hm1.insert("ssize_t", "isize");
    hm1.insert("ptrdiff_t", "isize");
    hm1.insert("intptr_t", "isize");
    hm1.insert("uintptr_t", "usize");

    // Fixed width integer types
    hm1.insert("int8_t", "i8");
    hm1.insert("int16_t", "i16");
    hm1.insert("int32_t", "i32");
    hm1.insert("int64_t", "i64");
    hm1.insert("uint8_t", "u8");
    hm1.insert("uint16_t", "u16");
    hm1.insert("uint32_t", "u32");
    hm1.insert("uint64_t", "u64");
    hm1.insert("intmax_t", "i64");
    hm1.insert("uintmax_t", "u64");

    // Fast integer types
    hm1.insert("int_fast8_t", "i8");
    hm1.insert("int_fast16_t", "i16");
    hm1.insert("int_fast32_t", "i32");
    hm1.insert("int_fast64_t", "i64");
    hm1.insert("uint_fast8_t", "u8");
    hm1.insert("uint_fast16_t", "u16");
    hm1.insert("uint_fast32_t", "u32");
    hm1.insert("uint_fast64_t", "u64");

    // Least integer types
    hm1.insert("int_least8_t", "i8");
    hm1.insert("int_least16_t", "i16");
    hm1.insert("int_least32_t", "i32");
    hm1.insert("int_least64_t", "i64");
    hm1.insert("uint_least8_t", "u8");
    hm1.insert("uint_least16_t", "u16");
    hm1.insert("uint_least32_t", "u32");
    hm1.insert("uint_least64_t", "u64");

    // Unsigned types
    hm1.insert("unsigned char", "u8");
    hm1.insert("unsigned short", "u16");
    hm1.insert("unsigned int", "u32");
    hm1.insert("unsigned long", "u64");
    hm1.insert("unsigned long long", "u64");

    // Common C library types
    hm1.insert("FILE", "libc::FILE");
    hm1.insert("DIR", "libc::DIR");
    hm1.insert("time_t", "libc::time_t");
    hm1.insert("clock_t", "libc::clock_t");
    hm1.insert("tm", "libc::tm");
    hm1.insert("timespec", "libc::timespec");
    hm1.insert("timeval", "libc::timeval");
    hm1.insert("va_list", "libc::va_list");
    hm1.insert("pthread_t", "libc::pthread_t");
    hm1.insert("pthread_mutex_t", "libc::pthread_mutex_t");
    hm1.insert("pthread_cond_t", "libc::pthread_cond_t");
    hm1.insert("complex", "libc::complex");
    hm1.insert("_Complex", "libc::complex");
    hm1.insert("_Imaginary", "libc::imaginary");
    // Socket and network types
    hm1.insert("sockaddr", "libc::sockaddr");
    hm1.insert("sockaddr_in", "libc::sockaddr_in");
    hm1.insert("sockaddr_in6", "libc::sockaddr_in6");
    hm1.insert("socklen_t", "libc::socklen_t");
    hm1.insert("in_addr", "libc::in_addr");
    hm1.insert("in6_addr", "libc::in6_addr");

    // Common POSIX types
    hm1.insert("mode_t", "libc::mode_t");
    hm1.insert("pid_t", "libc::pid_t");
    hm1.insert("uid_t", "libc::uid_t");
    hm1.insert("gid_t", "libc::gid_t");
    hm1.insert("off_t", "libc::off_t");
    hm1.insert("dev_t", "libc::dev_t");
    hm1.insert("ino_t", "libc::ino_t");
    hm1.insert("nlink_t", "libc::nlink_t");
    hm1.insert("blkcnt_t", "libc::blkcnt_t");
    hm1.insert("blksize_t", "libc::blksize_t");
    let mut hm2 = HashMap::<String, String>::new();
    // Add derived pointer and reference types for all basic mappings
    for (c_type, rust_type) in hm1.iter_mut() {
        hm2.insert(c_type.to_string(), rust_type.to_string());
        // Generate pointer variants
        hm2.insert(format!("{}*", c_type), format!("*mut {}", rust_type));
        hm2.insert(
            format!("const {}*", c_type),
            format!("*const {}", rust_type),
        );

        // Generate double pointer variants
        hm2.insert(format!("{}**", c_type), format!("*mut *mut {}", rust_type));
        hm2.insert(
            format!("const {}**", c_type),
            format!("*const *const {}", rust_type),
        );

        // Generate reference variants (C++ style)
        hm2.insert(format!("{}&", c_type), format!("&mut {}", rust_type));
        hm2.insert(format!("const {}&", c_type), format!("&{}", rust_type));

        // Generate pointer-reference combinations
        hm2.insert(format!("*{}&", c_type), format!("&*mut {}", rust_type));
        hm2.insert(
            format!("const *{}&", c_type),
            format!("&*const {}", rust_type),
        );
    }
    // Implementation logic for type conversion
    hm2
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
        }
    }
}
    /// Convert using pattern analysis from Patternizer, Analyzer, and Samplizer
    fn convert_with_pattern_analysis(
        handler_type: &str,
        _pattern_result: &crate::PatternResult,
        tokens: &[Token],
        analysis: Option<Result<crate::AnalysisResult>>,
        samplizer_result: Option<Result<crate::ConfidenceResult>>
    ) -> String {
        let mut rust_code = String::new();

        match handler_type {
            "function" => {
                rust_code.push_str("// Function identified by Patternizer\n");
                if let Some(analysis) = analysis {
                    rust_code.push_str(&format!("// Analysis: {:?}\n", analysis));
                }
                // Use samplizer result for conversion if available
                if let Some(Ok(confidence_result)) = samplizer_result {
                    rust_code.push_str(&format!("// Samplizer confidence: {:.2}\n", confidence_result.overall_confidence));
                    rust_code.push_str(&convert_function_tokens(tokens));
                } else {
                    rust_code.push_str(&convert_function_tokens(tokens));
                }
            }
            "struct" => {
                rust_code.push_str("// Struct identified by Patternizer\n");
                rust_code.push_str(&convert_struct_tokens(tokens));
            }
            "enum" => {
                rust_code.push_str("// Enum identified by Patternizer\n");
                rust_code.push_str(&convert_enum_tokens(tokens));
            }
            "array" => {
                rust_code.push_str("// Array identified by Patternizer\n");
                rust_code.push_str(&convert_array_tokens(tokens));
            }
            "typedef" => {
                rust_code.push_str("// Typedef identified by Patternizer\n");
                rust_code.push_str(&convert_typedef_tokens(tokens));
            }
            "global" => {
                rust_code.push_str("// Global variable identified by Patternizer\n");
                rust_code.push_str(&convert_global_tokens(tokens));
            }
            "expression" => {
                rust_code.push_str("// Expression identified by Patternizer\n");
                rust_code.push_str(&convert_expression_tokens(tokens));
            }
            _ => {
                rust_code.push_str(&format!("// Unknown pattern type: {}\n", handler_type));
                rust_code.push_str(&convert_generic_with_context(tokens));
            }
        }

        rust_code
    }
        /// Apply Samplizer patterns for conversion
        fn convert_with_samplizer_patterns(tokens: &[Token], patterns: &Vec<crate::SamplizerPattern>) -> String {
            let mut rust_code = String::new();
            
            // Apply high-confidence patterns first
            for pattern in patterns {
                if pattern.confidence_score > 0.7 {
                    rust_code.push_str(&format!("// High-confidence pattern: {} ({:.2})\n", 
                                              pattern.pattern_name, pattern.confidence_score));
                    // Use pattern-specific conversion based on type
                    match pattern.pattern_type {
                        SegmentType::Function => {
                            rust_code.push_str(&convert_function_tokens(tokens));
                        }
                        SegmentType::Struct => {
                            rust_code.push_str(&convert_struct_tokens(tokens));
                        }
                        SegmentType::Enum => {
                            rust_code.push_str(&convert_enum_tokens(tokens));
                        }
                        SegmentType::Array => {
                            rust_code.push_str(&convert_array_tokens(tokens));
                        }
                        SegmentType::TypeDef => {
                            rust_code.push_str(&convert_typedef_tokens(tokens));
                        }
                        SegmentType::Variable => {
                            rust_code.push_str(&convert_global_tokens(tokens));
                        }
                        SegmentType::Expression => {
                            rust_code.push_str(&convert_expression_tokens(tokens));
                        }
                        _ => {
                            rust_code.push_str(&convert_generic_with_context(tokens));
                        }
                    }
                    break; // Use first high-confidence pattern
                }
            }
            
            if rust_code.is_empty() {
                rust_code.push_str("// No high-confidence samplizer patterns found\n");
                rust_code.push_str(&convert_generic_with_context(tokens));
            }
            
            rust_code
        }
    /// Convert using adaptive system result
    fn convert_with_adaptive_result(tokens: &[Token], adaptive_result: &crate::AdaptivePatternResult) -> String {
        let mut rust_code = String::new();
        rust_code.push_str(&format!("// Adaptive match (confidence: {:.2})\n", adaptive_result.confidence));
        
        // Use the base_result from adaptive system
        match &adaptive_result.base_result {
            crate::PatternResult::Match { consumed_tokens } => {
                rust_code.push_str(&format!("// Consumed {} tokens\n", consumed_tokens));
            }
            _ => {}
        }
        
        // Apply adaptive conversion logic
        rust_code.push_str(&convert_generic_with_context(tokens));
        rust_code
    }
fn convert_tokens_to_rust(context: &mut Context, tokens: &[Token], range: Range<usize>) -> Result<String> {
    // Use Patternizer to identify the code construct
    let mut pattern_result = None;
    let mut matched_handler_type = None;

    // Try different handler patterns to identify the construct
    for handler_type in ["function", "struct", "enum", "typedef", "array", "global", "expression"].into_iter() {
        let result = context.patternizer.match_pattern(handler_type, tokens);
        if !matches!(result, crate::PatternResult::NoMatch { .. }) {
            pattern_result = Some(result);
            matched_handler_type = Some(handler_type);
            break;
        }
    }

    let rust_code = if let (Some(pattern_match), Some(handler_type)) = (pattern_result, matched_handler_type) {
        // Use Analyzer to get semantic information
        let analysis = context.samplizer.analyze_with_captures(tokens, &handler_type);

        // Use Samplizer to get conversion patterns
        let samplizer_result = context.samplizer.analyze_tokens(tokens);

        // Convert based on identified pattern and analysis
        convert_with_pattern_analysis(&handler_type, &pattern_match, tokens, Some(analysis), Some(samplizer_result))
    } else {
        // Fallback: Use adaptive system for unknown patterns
        let handler_pattern = crate::HandlerPattern::new("adaptive".to_string(), "adaptive conversion pattern".to_string());
        let adaptive_result = context.handlizer.adaptive_match(&handler_pattern, tokens);

        match adaptive_result {
            crate::AdaptivePatternResult { matched, confidence, tokens_consumed, match_data, base_result } if tokens_consumed > 0 => {
                // Convert using adaptive result with high confidence
                let adaptive_pattern_result = crate::AdaptivePatternResult {
                    matched,
                    confidence,
                    base_result,
                    tokens_consumed,
                    match_data,
                };
                convert_with_adaptive_result(tokens, &adaptive_pattern_result)
            },
            _ => {
                // Final fallback to generic conversion
                convert_generic_with_context(tokens)
            }
        }
    };

    Ok(rust_code)
}
fn convert_function_tokens(tokens: &[Token]) -> String {
    let mut rust_code = String::new();
    let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
    
    // Use existing convert.rs type mapping system
    if let Some(converted) = convert_type_tokens(token_strings.iter().map(|s| crate::Token::s(s.clone())).collect()) {
        rust_code.push_str(&format!("// Type converted: {}\n", converted.to_string()));
    }
    
    // Apply function conversion logic
    rust_code.push_str(&convert_function_pattern(&token_strings).unwrap_or_else(|| {
        format!("// Function conversion failed for: {}\n", token_strings.join(" "))
    }));
    
    rust_code
}

/// Convert struct tokens
fn convert_struct_tokens(tokens: &[Token]) -> String {
    let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
    convert_struct_pattern(&token_strings).unwrap_or_else(|| {
        format!("// Struct conversion failed for: {}\n", token_strings.join(" "))
    })
}

/// Convert enum tokens  
fn convert_enum_tokens(tokens: &[Token]) -> String {
    let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
    convert_enum_pattern(&token_strings).unwrap_or_else(|| {
        format!("// Enum conversion failed for: {}\n", token_strings.join(" "))
    })
}

/// Convert array tokens
fn convert_array_tokens(tokens: &[Token]) -> String {
    let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
    convert_array_pattern(&token_strings).unwrap_or_else(|| {
        format!("// Array conversion failed for: {}\n", token_strings.join(" "))
    })
}

/// Convert typedef tokens
fn convert_typedef_tokens(tokens: &[Token]) -> String {
    let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
    convert_typedef_pattern(&token_strings).unwrap_or_else(|| {
        format!("// Typedef conversion failed for: {}\n", token_strings.join(" "))
    })
}

/// Convert global variable tokens
fn convert_global_tokens(tokens: &[Token]) -> String {
    let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
    convert_variable_pattern(&token_strings).unwrap_or_else(|| {
        format!("// Global variable conversion failed for: {}\n", token_strings.join(" "))
    })
}

/// Convert expression tokens
fn convert_expression_tokens(tokens: &[Token]) -> String {
    let mut rust_code = String::new();
    let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
    
    // Check for common C expressions and convert them
    if token_strings.contains(&"=".to_string()) {
        rust_code.push_str(&convert_assignment_expression(&token_strings));
    } else if token_strings.contains(&"++".to_string()) || token_strings.contains(&"--".to_string()) {
        rust_code.push_str(&convert_increment_expression(&token_strings));
    } else {
        rust_code.push_str(&format!("// Expression: {}\n", token_strings.join(" ")));
    }
    
    rust_code
}

/// Convert assignment expressions
fn convert_assignment_expression(   tokens: &[String]) -> String {
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
        format!("// Increment: {} // TODO: Convert C++ to Rust += 1\n", tokens_str)
    } else if tokens_str.contains("--") {
        format!("// Decrement: {} // TODO: Convert C-- to Rust -= 1\n", tokens_str)
    } else {
        format!("// Increment/Decrement: {}\n", tokens_str)
    }
}

/// Convert with contextual information
fn convert_generic_with_context(tokens: &[Token]) -> String {
    let mut rust_code = String::new();
    let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
    
    // Use C keyword detection from convert.rs
    let c_keywords: Vec<String> = token_strings.iter()
        .filter(|s| is_c_keyword(crate::Token::s(s.to_string())))
        .cloned()
        .collect();
    
    if !c_keywords.is_empty() {
        rust_code.push_str(&format!("// C keywords detected: {}\n", c_keywords.join(", ")));
    }
    
    // Use type detection
    let type_tokens: Vec<String> = tokens.iter()
        .filter(|t| crate::convert::is_type_token(t))
        .map(|t| t.to_string())
        .collect();
        
    if !type_tokens.is_empty() {
        rust_code.push_str(&format!("// Type tokens detected: {}\n", type_tokens.join(", ")));
    }
    
    // Apply generic conversion
    rust_code.push_str(&convert_generic_pattern(&token_strings));
    
    rust_code
}

/// Convert function declarations and definitions
fn convert_function_pattern(tokens: &[String]) -> Option<String> {
    if tokens.len() < 4 { return None; }
    
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
            let return_type =   convert_c_type(&tokens[0]);
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
    if tokens.is_empty() || tokens[0] != "struct" { return None; }
    
    let mut result = String::new();
    
    if tokens.len() > 1 {
        let struct_name = &tokens[1];
        result.push_str(&format!("#[derive(Debug, Clone)]\npub struct {} {{\n", struct_name));
        
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
    if tokens.is_empty() || tokens[0] != "enum" { return None; }
    
    if tokens.len() > 1 {
        let enum_name = &tokens[1];
        let mut result = format!("#[derive(Debug, Clone, Copy, PartialEq, Eq)]\npub enum {} {{\n", enum_name);
        
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
    if tokens.is_empty() || tokens[0] != "typedef" { return None; }
    
    if tokens.len() >= 3 {
        let existing_type = convert_c_type(&tokens[1]);
        let new_type = &tokens[2];
        return Some(format!("pub type {} = {};\n", new_type, existing_type));
    }
    
    None
}

/// Convert variable declarations
fn convert_variable_pattern(tokens: &[String]) -> Option<String> {
    if tokens.len() < 2 { return None; }
    
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
    if tokens.len() < 4 { return None; }
    
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
                        let size_str = &size_part[start+1..end];
                        if size_str.is_empty() {
                            return Some(format!("let {}: Vec<{}> = Vec::new();\n", array_name, element_type));
                        } else {
                            return Some(format!("let {}: [{}; {}] = [Default::default(); {}];\n", 
                                array_name, element_type, size_str, size_str));
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
    if tokens.is_empty() || tokens[0] != "#include" { return None; }
    
    if tokens.len() > 1 {
        let header = &tokens[1];
        if header.starts_with('<') && header.ends_with('>') {
            let header_name = &header[1..header.len()-1];
            return Some(convert_include_to_use(header_name));
        } else if header.starts_with('"') && header.ends_with('"') {
            let header_name = &header[1..header.len()-1];
            return Some(format!("mod {};\n", header_name.replace(".h", "")));
        }
    }
    
    None
}

/// Generic pattern conversion with improved formatting
fn convert_generic_pattern(tokens: &[String]) -> String {
    if tokens.is_empty() {
        return String::from("// Empty token sequence\n");
    }
    
    let mut result = String::new();
    result.push_str(&format!("// Original C code ({} tokens):\n", tokens.len()));
    result.push_str("// ");
    result.push_str(&tokens.join(" "));
    result.push_str("\n// TODO: Implement proper conversion for this pattern\n");
    
    result
}

/// Convert C types to Rust types
fn convert_c_type(c_type: &str) -> String {
    match c_type {
        "int" => "i32".to_string(),
        "char" => "i8".to_string(),
        "unsigned char" => "u8".to_string(),
        "short" => "i16".to_string(),
        "unsigned short" => "u16".to_string(),
        "long" => "i64".to_string(),
        "unsigned long" => "u64".to_string(),
        "float" => "f32".to_string(),
        "double" => "f64".to_string(),
        "void" => "()".to_string(),
        "char*" | "char *" => "String".to_string(),
        "const char*" | "const char *" => "&str".to_string(),
        _ => {
            if c_type.ends_with('*') {
                format!("*mut {}", convert_c_type(&c_type[..c_type.len()-1].trim()))
            } else {
                c_type.to_string()
            }
        }
    }
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
        if variant_name != "," && variant_name != "=" && !variant_name.chars().all(|c| c.is_ascii_digit()) {
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
        _ => format!("// TODO: Convert #include <{}>\n", header_name)
    }
}