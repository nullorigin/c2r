//! Smart Redirect System for C2R Handlers
//! Enhanced pattern detection and intelligent routing to specialized handlers

use crate::{C2RError, HandlerPhase, HandlerResult, Id, ReportLevel, Token, context, report};
use std::ops::Range;

/// Pattern analysis result for smart routing decisions
#[derive(Debug, Clone)]
pub struct PatternAnalysis {
    pub pattern_type: PatternType,
    pub confidence: f32,
    pub suggested_handler: String,
    pub nested_patterns: Vec<NestedPattern>,
    pub context_info: ContextInfo,
}

/// Types of patterns detected in token sequences
#[derive(Debug, Clone, PartialEq)]
pub enum PatternType {
    FunctionDeclaration,
    FunctionDefinition,
    MainFunction,
    GlobalVariable,
    GlobalConstant,
    StaticVariable,
    ExternDeclaration,
    DefinesMacro,
    ConditionalMacro,
    IncludeDirective,
    StructDefinition,
    EnumDefinition,
    TypedefDeclaration,
    ArrayDeclaration,
    ControlFlow,
    Expression,
    Comment,
    Unknown,
}

/// Nested pattern information for complex structures
#[derive(Debug, Clone)]
pub struct NestedPattern {
    pub pattern_type: PatternType,
    pub range: Range<usize>,
    pub depth: usize,
    pub parent_context: String,
}

/// Context information for pattern analysis
#[derive(Debug, Clone)]
pub struct ContextInfo {
    pub brace_depth: usize,
    pub paren_depth: usize,
    pub bracket_depth: usize,
    pub in_function_body: bool,
    pub in_struct_body: bool,
    pub in_enum_body: bool,
    pub preceding_keywords: Vec<String>,
    pub following_tokens: Vec<String>,
}

/// Smart pattern analyzer for token sequences
pub struct SmartPatternAnalyzer;

impl SmartPatternAnalyzer {
    /// Analyze token pattern and recommend best handler
    pub fn analyze_pattern(token_range: Range<usize>) -> Result<PatternAnalysis, C2RError> {
        let mut context = context!();
        context.pull();

        if token_range.end > context.tokens.len() || token_range.start >= context.tokens.len() {
            return Ok(PatternAnalysis {
                pattern_type: PatternType::Unknown,
                confidence: 0.0,
                suggested_handler: "unknown".to_string(),
                nested_patterns: vec![],
                context_info: ContextInfo::default(),
            });
        }

        let tokens = &context.tokens[token_range.clone()];
        let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        // Build context information
        let context_info =
            Self::analyze_context(&token_strings, &context.tokens, token_range.start);

        // Detect primary pattern type
        let (pattern_type, confidence, suggested_handler) =
            Self::detect_primary_pattern(&token_strings, &context_info);

        // Detect nested patterns
        let nested_patterns =
            Self::detect_nested_patterns(&token_strings, &context_info, token_range.clone());

        Ok(PatternAnalysis {
            pattern_type,
            confidence,
            suggested_handler,
            nested_patterns,
            context_info,
        })
    }

    /// Analyze context around the token range
    fn analyze_context(
        tokens: &[String],
        all_tokens: &[Token],
        start_offset: usize,
    ) -> ContextInfo {
        let mut context = ContextInfo::default();

        // Count nesting levels in current tokens
        for token in tokens {
            match token.as_str() {
                "{" => context.brace_depth += 1,
                "}" => context.brace_depth = context.brace_depth.saturating_sub(1),
                "(" => context.paren_depth += 1,
                ")" => context.paren_depth = context.paren_depth.saturating_sub(1),
                "[" => context.bracket_depth += 1,
                "]" => context.bracket_depth = context.bracket_depth.saturating_sub(1),
                _ => {}
            }
        }

        // Look at preceding context (up to 10 tokens before)
        let preceding_start = start_offset.saturating_sub(10);
        if preceding_start < all_tokens.len() && start_offset <= all_tokens.len() {
            let preceding_tokens: Vec<String> = all_tokens[preceding_start..start_offset]
                .iter()
                .map(|t| t.to_string())
                .collect();

            context.preceding_keywords = preceding_tokens
                .iter()
                .filter(|t| Self::is_keyword(t))
                .cloned()
                .collect();

            // Check if we're in function/struct/enum body
            let mut brace_count: i32 = 0;
            let mut in_function = false;
            let mut in_struct = false;
            let mut in_enum = false;

            for (i, token) in preceding_tokens.iter().enumerate() {
                match token.as_str() {
                    "{" => brace_count += 1,
                    "}" => brace_count = brace_count.saturating_sub(1),
                    _ => {}
                }

                if brace_count > 0 {
                    if preceding_tokens[..i]
                        .iter()
                        .any(|t| matches!(t.as_str(), "int" | "void" | "char" | "float" | "double"))
                        && preceding_tokens[..i].iter().any(|t| t.contains("("))
                    {
                        in_function = true;
                    }
                    if preceding_tokens[..i].iter().any(|t| t == "struct") {
                        in_struct = true;
                    }
                    if preceding_tokens[..i].iter().any(|t| t == "enum") {
                        in_enum = true;
                    }
                }
            }

            context.in_function_body = in_function;
            context.in_struct_body = in_struct;
            context.in_enum_body = in_enum;
        }

        // Look at following context (up to 5 tokens after)
        let following_end = (start_offset + tokens.len() + 5).min(all_tokens.len());
        if start_offset + tokens.len() < all_tokens.len() {
            let following_start = start_offset + tokens.len();
            context.following_tokens = all_tokens[following_start..following_end]
                .iter()
                .map(|t| t.to_string())
                .collect();
        }

        context
    }

    /// Detect the primary pattern type from tokens
    fn detect_primary_pattern(
        tokens: &[String],
        context: &ContextInfo,
    ) -> (PatternType, f32, String) {
        if tokens.is_empty() {
            return (PatternType::Unknown, 0.0, "unknown".to_string());
        }

        // High confidence patterns first

        // Include directives
        if Self::matches_include_pattern(tokens) {
            return (
                PatternType::IncludeDirective,
                0.95,
                "include_directive_handler".to_string(),
            );
        }

        // Define macros
        if Self::matches_define_pattern(tokens) {
            return (
                PatternType::DefinesMacro,
                0.95,
                "define_macro_handler".to_string(),
            );
        }

        // Conditional macros
        if Self::matches_conditional_macro_pattern(tokens) {
            return (
                PatternType::ConditionalMacro,
                0.95,
                "conditional_macro_handler".to_string(),
            );
        }

        // Extern declarations
        if Self::matches_extern_pattern(tokens) {
            return (
                PatternType::ExternDeclaration,
                0.90,
                "extern_declaration_handler".to_string(),
            );
        }

        // Main function (very specific)
        if Self::matches_main_function_pattern(tokens) {
            return (
                PatternType::MainFunction,
                0.95,
                "main_function_handler".to_string(),
            );
        }

        // Function declarations vs definitions
        if Self::matches_function_pattern(tokens) {
            if Self::has_function_body(tokens) {
                return (
                    PatternType::FunctionDefinition,
                    0.85,
                    "function_definition_handler".to_string(),
                );
            } else {
                return (
                    PatternType::FunctionDeclaration,
                    0.85,
                    "function_declaration_handler".to_string(),
                );
            }
        }

        // Static variables
        if Self::matches_static_pattern(tokens) && !Self::matches_function_pattern(tokens) {
            return (
                PatternType::StaticVariable,
                0.80,
                "static_variable_handler".to_string(),
            );
        }

        // Global constants
        if Self::matches_const_pattern(tokens) && !context.in_function_body {
            return (
                PatternType::GlobalConstant,
                0.80,
                "global_constant_handler".to_string(),
            );
        }

        // Global variables
        if Self::matches_variable_pattern(tokens) && !context.in_function_body {
            return (
                PatternType::GlobalVariable,
                0.75,
                "global_variable_handler".to_string(),
            );
        }

        // Struct/enum/typedef patterns
        if Self::matches_struct_pattern(tokens) {
            return (
                PatternType::StructDefinition,
                0.85,
                "struct_handler".to_string(),
            );
        }

        if Self::matches_enum_pattern(tokens) {
            return (
                PatternType::EnumDefinition,
                0.85,
                "enum_handler".to_string(),
            );
        }

        if Self::matches_typedef_pattern(tokens) {
            return (
                PatternType::TypedefDeclaration,
                0.85,
                "typedef_handler".to_string(),
            );
        }

        // Array patterns
        if Self::matches_array_pattern(tokens) {
            return (
                PatternType::ArrayDeclaration,
                0.75,
                "array_handler".to_string(),
            );
        }

        // Control flow patterns
        if Self::matches_control_flow_pattern(tokens) {
            return (
                PatternType::ControlFlow,
                0.80,
                "control_flow_handler".to_string(),
            );
        }

        // Comments
        if Self::matches_comment_pattern(tokens) {
            return (PatternType::Comment, 0.90, "comment_handler".to_string());
        }

        // Expression (fallback)
        if tokens.len() > 0 {
            return (
                PatternType::Expression,
                0.30,
                "expression_handler".to_string(),
            );
        }

        (PatternType::Unknown, 0.0, "unknown".to_string())
    }

    /// Detect nested patterns within the token sequence
    fn detect_nested_patterns(
        tokens: &[String],
        context: &ContextInfo,
        range: Range<usize>,
    ) -> Vec<NestedPattern> {
        let mut nested_patterns = vec![];
        let mut i = 0;
        let mut depth: i32 = 0;

        while i < tokens.len() {
            // Track nesting depth
            match tokens[i].as_str() {
                "{" => depth += 1,
                "}" => depth = depth.saturating_sub(1),
                _ => {}
            }

            // Look for nested function calls within expressions
            if i + 2 < tokens.len() && tokens[i + 1] == "(" {
                if Self::is_function_name(&tokens[i]) {
                    nested_patterns.push(NestedPattern {
                        pattern_type: PatternType::Expression,
                        range: (range.start + i)..(range.start + i + 3),
                        depth: depth as usize,
                        parent_context: "function_call".to_string(),
                    });
                }
            }

            // Look for nested variable declarations in function bodies
            if context.in_function_body && Self::is_type_keyword(&tokens[i]) && i + 1 < tokens.len()
            {
                nested_patterns.push(NestedPattern {
                    pattern_type: PatternType::Expression,
                    range: (range.start + i)..(range.start + i + 2),
                    depth: depth as usize,
                    parent_context: "local_variable".to_string(),
                });
            }

            i += 1;
        }

        nested_patterns
    }

    // Pattern matching helper functions
    fn matches_include_pattern(tokens: &[String]) -> bool {
        if tokens.len() < 2 {
            return false;
        }
        (tokens[0] == "#include") || (tokens[0] == "#" && tokens[1] == "include")
    }

    fn matches_define_pattern(tokens: &[String]) -> bool {
        if tokens.len() < 2 {
            return false;
        }
        (tokens[0] == "#define") || (tokens[0] == "#" && tokens[1] == "define")
    }

    fn matches_conditional_macro_pattern(tokens: &[String]) -> bool {
        if tokens.is_empty() {
            return false;
        }
        matches!(
            tokens[0].as_str(),
            "#ifdef" | "#ifndef" | "#endif" | "#else" | "#elif" | "#undef"
        ) || (tokens.len() >= 2
            && tokens[0] == "#"
            && matches!(
                tokens[1].as_str(),
                "ifdef" | "ifndef" | "endif" | "else" | "elif" | "undef"
            ))
    }

    fn matches_extern_pattern(tokens: &[String]) -> bool {
        tokens.iter().any(|t| t == "extern")
    }

    fn matches_main_function_pattern(tokens: &[String]) -> bool {
        tokens.iter().any(|t| t == "main")
            && tokens.iter().any(|t| t == "(")
            && (tokens.iter().any(|t| t == "int") || tokens.iter().any(|t| t == "void"))
    }

    fn matches_function_pattern(tokens: &[String]) -> bool {
        // Has return type, identifier, and parentheses
        let has_type = tokens.iter().any(|t| Self::is_type_keyword(t));
        let has_parens = tokens.iter().any(|t| t == "(");
        let has_identifier = tokens.iter().any(|t| Self::is_identifier(t));
        has_type && has_parens && has_identifier
    }

    fn has_function_body(tokens: &[String]) -> bool {
        tokens.iter().any(|t| t == "{")
    }

    fn matches_static_pattern(tokens: &[String]) -> bool {
        tokens.iter().any(|t| t == "static")
    }

    fn matches_const_pattern(tokens: &[String]) -> bool {
        tokens.iter().any(|t| t == "const")
    }

    fn matches_variable_pattern(tokens: &[String]) -> bool {
        let has_type = tokens.iter().any(|t| Self::is_type_keyword(t));
        let has_semicolon = tokens.iter().any(|t| t == ";");
        let no_parens = !tokens.iter().any(|t| t == "(");
        has_type && has_semicolon && no_parens
    }

    fn matches_struct_pattern(tokens: &[String]) -> bool {
        tokens.iter().any(|t| t == "struct")
    }

    fn matches_enum_pattern(tokens: &[String]) -> bool {
        tokens.iter().any(|t| t == "enum")
    }

    fn matches_typedef_pattern(tokens: &[String]) -> bool {
        tokens.iter().any(|t| t == "typedef")
    }

    fn matches_array_pattern(tokens: &[String]) -> bool {
        tokens.iter().any(|t| t == "[") && tokens.iter().any(|t| t == "]")
    }

    fn matches_control_flow_pattern(tokens: &[String]) -> bool {
        tokens.iter().any(|t| {
            matches!(
                t.as_str(),
                "if" | "else"
                    | "while"
                    | "for"
                    | "switch"
                    | "case"
                    | "break"
                    | "continue"
                    | "return"
            )
        })
    }

    fn matches_comment_pattern(tokens: &[String]) -> bool {
        tokens
            .iter()
            .any(|t| t.starts_with("//") || t.starts_with("/*"))
    }

    fn is_keyword(token: &str) -> bool {
        matches!(
            token,
            "int"
                | "char"
                | "float"
                | "double"
                | "void"
                | "struct"
                | "enum"
                | "typedef"
                | "static"
                | "extern"
                | "const"
                | "if"
                | "else"
                | "while"
                | "for"
                | "switch"
                | "return"
                | "break"
                | "continue"
                | "case"
                | "default"
        )
    }

    fn is_type_keyword(token: &str) -> bool {
        matches!(
            token,
            "int"
                | "char"
                | "float"
                | "double"
                | "void"
                | "short"
                | "long"
                | "signed"
                | "unsigned"
                | "struct"
                | "enum"
                | "union"
        )
    }

    fn is_identifier(token: &str) -> bool {
        !token.is_empty()
            && token.chars().next().unwrap().is_alphabetic()
            && token.chars().all(|c| c.is_alphanumeric() || c == '_')
            && !Self::is_keyword(token)
    }

    fn is_function_name(token: &str) -> bool {
        Self::is_identifier(token)
            && !matches!(
                token,
                "if" | "while" | "for" | "switch" | "sizeof" | "return"
            )
    }
}

impl Default for ContextInfo {
    fn default() -> Self {
        ContextInfo {
            brace_depth: 0,
            paren_depth: 0,
            bracket_depth: 0,
            in_function_body: false,
            in_struct_body: false,
            in_enum_body: false,
            preceding_keywords: vec![],
            following_tokens: vec![],
        }
    }
}

/// Smart redirect callback that uses pattern analysis to route to specialized handlers
pub fn smart_redirect_callback(
    token_range: Range<usize>,
    result: HandlerResult,
) -> Result<HandlerResult, C2RError> {
    report!(
        "smart_redirect",
        "smart_redirect_callback",
        ReportLevel::Info,
        HandlerPhase::Handle,
        format!(
            "Analyzing patterns for smart redirect in range {:?}",
            token_range
        ),
        true
    );

    // Only redirect if the current result is NotHandled or needs better handling
    match &result {
        HandlerResult::NotHandled(_, _, _) | HandlerResult::Processed(_, _, _, _) => {
            // Perform pattern analysis
            match SmartPatternAnalyzer::analyze_pattern(token_range.clone()) {
                Ok(analysis) => {
                    if analysis.confidence > 0.7 {
                        report!(
                            "smart_redirect",
                            "smart_redirect_callback",
                            ReportLevel::Info,
                            HandlerPhase::Handle,
                            format!(
                                "High confidence pattern detected: {:?} -> {}",
                                analysis.pattern_type, analysis.suggested_handler
                            ),
                            true
                        );

                        // Create redirect to specialized handler
                        let from_id = Id::get("smart_redirect");
                        let to_id = Id::get(&analysis.suggested_handler);

                        return Ok(HandlerResult::Redirected(
                            None,
                            token_range,
                            analysis.suggested_handler,
                            from_id,
                            to_id,
                        ));
                    } else {
                        report!(
                            "smart_redirect",
                            "smart_redirect_callback",
                            ReportLevel::Debug,
                            HandlerPhase::Handle,
                            format!(
                                "Low confidence pattern ({:.2}): {:?}",
                                analysis.confidence, analysis.pattern_type
                            ),
                            true
                        );
                    }
                }
                Err(e) => {
                    report!(
                        "smart_redirect",
                        "smart_redirect_callback",
                        ReportLevel::Warning,
                        HandlerPhase::Handle,
                        format!("Pattern analysis failed: {}", e),
                        true
                    );
                }
            }
        }
        _ => {
            // Result is already handled successfully, no need to redirect
            report!(
                "smart_redirect",
                "smart_redirect_callback",
                ReportLevel::Debug,
                HandlerPhase::Handle,
                "Result already handled successfully, no redirect needed",
                true
            );
        }
    }

    Ok(result)
}
