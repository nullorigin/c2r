//! Function handler for C to Rust function conversion
//! Uses Patternizer exclusively for detection and conversion

use crate::pattern::{HandlerPattern, PatternResult, PatternRule, Patternizer, TokenPattern};
use crate::sample::{Samplizer, SegmentType};
use crate::token::Token;
use crate::{
    C2RError, ConvertedElement, ExtractedElement, ExtractedFunction, HandlerPhase, HandlerResult,
    Id, Kind, Reason, ReportLevel,
};
use crate::{ConvertedFunction, ElementInfo, FunctionInfo, HandlerReport};
use crate::{
    HandlerPhase::{Handle, Process, Report},
    ReportLevel::{Error, Info, Warning},
};
use crate::{context, report};
use std::ops::Range;

/// Generate function patterns using Samplizer analysis
fn generate_function_patterns_with_samplizer() -> Patternizer {
    let mut patternizer = Patternizer::new("function_handler");

    // Use samplizer to analyze our test files and generate patterns
    let mut samplizer = Samplizer::new();

    // Analyze simple function examples
    match samplizer.analyze_file_pair(
        "test_files/simple_function.c",
        "test_files/simple_function.rs",
    ) {
        Ok(patterns) => {
            report!(
                "function_handler",
                "generate_function_patterns_with_samplizer",
                Info,
                Process,
                format!(
                    "Samplizer generated {} patterns from simple_function files",
                    patterns.len()
                ),
                true
            );

            // Integrate generated patterns into patternizer
            for pattern in patterns {
                if pattern.pattern_type == SegmentType::Function && pattern.confidence_score > 0.5 {
                    if let Ok(handler_pattern) =
                        convert_samplizer_pattern_to_handler_pattern(&pattern)
                    {
                        patternizer.register_pattern("function".to_string(), handler_pattern);

                        report!(
                            "function_handler",
                            "generate_function_patterns_with_samplizer",
                            Info,
                            Process,
                            format!(
                                "Registered pattern: {} (confidence: {:.2})",
                                pattern.pattern_name, pattern.confidence_score
                            ),
                            true
                        );
                    }
                }
            }
        }
        Err(e) => {
            report!(
                "function_handler",
                "generate_function_patterns_with_samplizer",
                Info,
                Process,
                format!(
                    "Samplizer analysis failed: {}, falling back to basic patterns",
                    e
                ),
                true
            );

            // Fallback: Create basic function patterns manually
            add_basic_function_patterns(&mut patternizer);
        }
    }

    patternizer
}

/// Convert a samplizer pattern to a HandlerPattern
fn convert_samplizer_pattern_to_handler_pattern(
    pattern: &crate::sample::SamplizerPattern,
) -> Result<HandlerPattern, C2RError> {
    use crate::pattern::{HandlerPattern, PatternRule, TokenPattern};

    if let Some(extraction_pattern) = &pattern.extraction_pattern {
        let pattern_tokens: Vec<&str> = extraction_pattern.split_whitespace().collect();
        let mut rules = Vec::new();

        for token in pattern_tokens {
            let rule = match token {
                "IDENTIFIER" => PatternRule::new(TokenPattern::Identifier),
                "NUMBER" => PatternRule::new(TokenPattern::Number),
                token_str if matches!(token_str, "int" | "char" | "float" | "double" | "void") => {
                    PatternRule::new(TokenPattern::TypeKeyword)
                }
                _ => PatternRule::new(TokenPattern::Exact(token.to_string())),
            };
            rules.push(rule);
        }

        let priority = (pattern.confidence_score * 300.0) as i32;

        Ok(HandlerPattern::new(
            pattern.pattern_id.clone(),
            format!("Auto-generated: {}", pattern.pattern_name),
        )
        .with_rules(rules)
        .priority(priority))
    } else {
        Err(C2RError::new(
            Kind::Other,
            Reason::Empty("extraction pattern"),
            None,
        ))
    }
}

/// Add basic function patterns as fallback
fn add_basic_function_patterns(patternizer: &mut Patternizer) {
    // Basic function start pattern
    let function_start_pattern = HandlerPattern::new(
        "basic_function_start".to_string(),
        "Basic function signature start".to_string(),
    )
    .with_rules(vec![
        PatternRule::new(TokenPattern::TypeKeyword),
        PatternRule::new(TokenPattern::Identifier),
        PatternRule::new(TokenPattern::Exact("(".to_string())),
    ])
    .priority(200);

    patternizer.register_pattern("function".to_string(), function_start_pattern);
}

/// Creates a function handler that uses Patternizer exclusively
pub fn create_function_handler() -> crate::handler::Handler {
    let handler_id = Id::get("function_handler");
    let handler_role = "function";
    let priority = 200;

    // Handler utilities
    super::create_handler(
        handler_id,
        handler_role,
        priority,
        Some(process_function),
        Some(handle_function),
        Some(extract_function),
        Some(convert_function),
        Some(document_function),
        Some(report_function),
        Some(result_function), // No result callback
        Some(redirect_function),
    )
}

/// Process callback: Lightweight function signature detection
fn process_function(token_range: Range<usize>) -> Result<bool, C2RError> {
    let mut context = crate::context!();
    context.pull();

    // Add bounds checking to prevent slice index out of bounds
    if token_range.end > context.tokens.len() || token_range.start >= context.tokens.len() {
        return Ok(false);
    }

    let tokens = &context.tokens[token_range.clone()];
    let filtered_tokens: Vec<Token> = tokens
        .iter()
        .filter(|token| !matches!(token, Token::n()))
        .cloned()
        .collect();
    let tokens = &filtered_tokens;

    if tokens.len() < 3 {
        return Ok(false);
    }

    // Use lightweight patterns to detect potential function starts
    // This allows the extract callback to then expand the window intelligently
    let safe_end = std::cmp::min(token_range.end, tokens.len());
    let safe_range = token_range.start..safe_end;
    if safe_range.start >= tokens.len() || safe_range.is_empty() {
        return Ok(false);
    }
    let range_tokens = &tokens[safe_range];

    report!(
        "function_handler",
        "process_function",
        Info,
        Process,
        format!(
            "Lightweight check: {} tokens starting with: {}",
            range_tokens.len(),
            range_tokens
                .iter()
                .take(3)
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(" ")
        ),
        true
    );

    // Use pattern matching instead of hardcoded detection
    if range_tokens.len() >= 3 {
        // Try function patterns using patternizer
        let function_patterns = ["function", "function_declaration", "main_function"];

        for pattern in &function_patterns {
            match context.patternizer.match_pattern(pattern, range_tokens) {
                crate::pattern::PatternResult::Match { consumed_tokens: _ } => {
                    report!(
                        "function_handler",
                        "process_function",
                        Info,
                        Process,
                        format!(
                            "Pattern '{}' matched - delegating to extract for expansion",
                            pattern
                        ),
                        true
                    );
                    return Ok(true);
                }
                _result => {
                    continue;
                }
            }
        }

        // Fallback: use basic detection for patterns that might not be registered
        let potential_function = detect_function_start(range_tokens);
        if potential_function {
            report!(
                "function_handler",
                "process_function",
                Info,
                Process,
                "Fallback detection found potential function start - delegating to extract for expansion",
                true
            );
            return Ok(true);
        }
    }

    Ok(false)
}

/// Lightweight function start detection
fn detect_function_start(tokens: &[Token]) -> bool {
    if tokens.len() < 3 {
        return false;
    }

    // Look for patterns like: int main (, void print_point (, etc.
    for i in 0..tokens.len().saturating_sub(2) {
        let token_str = tokens[i].to_string();
        let next_token = tokens[i + 1].to_string();
        let third_token = tokens[i + 2].to_string();

        // Type keyword + identifier + opening paren
        if is_type_keyword(&token_str) && is_identifier(&next_token) && third_token == "(" {
            return true;
        }
    }

    false
}

/// Check if token is a type keyword
fn is_type_keyword(token: &str) -> bool {
    matches!(
        token,
        "int"
            | "void"
            | "char"
            | "float"
            | "double"
            | "long"
            | "short"
            | "unsigned"
            | "signed"
            | "struct"
            | "enum"
            | "typedef"
    )
}

/// Check if token is an identifier
fn is_identifier(token: &str) -> bool {
    !token.is_empty()
        && token.chars().next().unwrap().is_alphabetic()
        && token.chars().all(|c| c.is_alphanumeric() || c == '_')
        && !is_type_keyword(token)
}

/// Handle callback: Uses extract callback for intelligent window expansion
fn handle_function(token_range: Range<usize>) -> Result<HandlerResult, C2RError> {
    let id = Id::get("function_handler");

    report!(
        "function_handler",
        "handle_function",
        Info,
        Handle,
        format!(
            "Handle function called for range {}..{}, delegating to extract callback",
            token_range.start, token_range.end
        ),
        true
    );

    // Use the extract callback for intelligent window expansion and pattern matching
    if let Some(extracted_element) = extract_function(token_range.clone())? {
        match extracted_element {
            ExtractedElement::Function(function) => {
                report!(
                    "function_handler",
                    "handle_function",
                    Info,
                    Handle,
                    format!(
                        "Successfully extracted function: {} via intelligent expansion",
                        function.name
                    ),
                    true
                );

                // Use the actual range processed, not the extracted tokens length
                // The token_range passed in is the actual range that was processed
                let consumed_range = token_range.clone();

                Ok(HandlerResult::Extracted(
                    ExtractedElement::Function(function),
                    consumed_range,
                    "Function extracted via recursive windowing".to_string(),
                    id,
                ))
            }
            _ => {
                report!(
                    "function_handler",
                    "handle_function",
                    Info,
                    Handle,
                    "Extract callback returned non-function element".to_string(),
                    false
                );
                let mut context = crate::context!();
                context.pull();
                Ok(HandlerResult::NotHandled(
                    Some(context.tokens[token_range.clone()].to_vec()),
                    token_range,
                    id,
                ))
            }
        }
    } else {
        report!(
            "function_handler",
            "handle_function",
            Info,
            Handle,
            "Extract callback could not extract function from expanded window".to_string(),
            false
        );
        let mut context = crate::context!();
        context.pull();
        Ok(HandlerResult::NotHandled(
            Some(context.tokens[token_range.clone()].to_vec()),
            token_range,
            id,
        ))
    }
}

/// Extract callback: Intelligent function extraction with recursive window expansion
fn extract_function(token_range: Range<usize>) -> Result<Option<ExtractedElement>, C2RError> {
    let mut context = crate::context!();
    context.pull();
    let tokens = &context.tokens[token_range.clone()];
    let filtered_tokens: Vec<Token> = tokens
        .iter()
        .filter(|token| !matches!(token, crate::Token::n()))
        .cloned()
        .collect();
    let tokens = &filtered_tokens;

    if token_range.start >= tokens.len() {
        return Ok(None);
    }

    // Step 1: Find the actual function start within the range
    let function_start = find_function_start_in_range(tokens, &token_range)?;
    if function_start.is_none() {
        return Ok(None);
    }
    let start_pos = function_start.unwrap();

    // Step 2: Recursively expand window to capture complete function
    let expanded_range = expand_to_function(tokens, start_pos)?;
    if expanded_range.is_none() {
        return Ok(None);
    }
    let complete_range = expanded_range.unwrap();

    report!(
        "function_handler",
        "extract_function",
        Info,
        Process,
        format!(
            "Expanded window from {}..{} to {}..{} to capture complete function",
            token_range.start, token_range.end, complete_range.start, complete_range.end
        ),
        true
    );

    // Step 3: Use samplizer patterns on the complete function
    let mut patternizer = generate_function_patterns_with_samplizer();
    let complete_tokens = &tokens[complete_range.clone()];

    match patternizer.match_pattern("function", complete_tokens) {
        PatternResult::Match { consumed_tokens } => {
            let (return_type, name, params, body) = parse_function_tokens(complete_tokens)?;

            let is_definition = !body.is_empty();
            let extracted_function = ExtractedFunction {
                name,
                return_type,
                parameters: params,
                body,
                is_variadic: complete_tokens.iter().any(|t| t.to_string() == "..."),
                from_recovery: false,
                is_definition,
                is_static: complete_tokens.iter().any(|t| t.to_string() == "static"),
                is_inline: complete_tokens.iter().any(|t| t.to_string() == "inline"),
                is_extern: complete_tokens.iter().any(|t| t.to_string() == "extern"),
                code: complete_tokens[..consumed_tokens.min(complete_tokens.len())]
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(" "),
                tokens: complete_tokens[..consumed_tokens.min(complete_tokens.len())].to_vec(),
            };

            report!(
                "function_handler",
                "extract_function",
                Info,
                Process,
                format!(
                    "Successfully extracted function: {}",
                    extracted_function.name
                ),
                true
            );

            Ok(Some(ExtractedElement::Function(extracted_function)))
        }
        _ => {
            report!(
                "function_handler",
                "extract_function",
                Info,
                Process,
                "Samplizer patterns did not match complete function - using fallback extraction",
                true
            );

            // Fallback: try to extract basic function info even without pattern match
            extract_function_fallback(complete_tokens)
        }
    }
}

/// Find function start position within the given range
fn find_function_start_in_range(
    tokens: &[Token],
    range: &Range<usize>,
) -> Result<Option<usize>, C2RError> {
    let safe_end = std::cmp::min(range.end, tokens.len());
    let safe_range = range.start..safe_end;
    if safe_range.start >= tokens.len() || safe_range.is_empty() {
        return Ok(None);
    }
    let range_tokens = &tokens[safe_range.clone()];

    for (i, window) in range_tokens.windows(3).enumerate() {
        if detect_function_start(window) {
            return Ok(Some(range.start + i));
        }
    }
    Ok(None)
}

/// Expand window to capture complete function starting with minimum viable tokens
fn expand_to_function(
    tokens: &[Token],
    start_pos: usize,
) -> Result<Option<Range<usize>>, C2RError> {
    if start_pos >= tokens.len() {
        return Ok(None);
    }

    // Constants for minimum viable function tokens
    const MIN_DECLARATION_TOKENS: usize = 5; // int main ( ) ;
    const MIN_DEFINITION_TOKENS: usize = 7; // int main ( ) { return ; }

    // Step 1: Start with minimum declaration window (5 tokens)
    let mut current_end = (start_pos + MIN_DECLARATION_TOKENS).min(tokens.len());

    // Step 2: Find opening parenthesis within minimum window
    let mut paren_pos = None;
    for i in start_pos..current_end {
        if tokens[i].to_string() == "(" {
            paren_pos = Some(i);
            break;
        }
    }

    // Step 3: If no parenthesis in minimum window, expand progressively
    if paren_pos.is_none() {
        for expand_size in 1..=10 {
            current_end = (start_pos + MIN_DECLARATION_TOKENS + expand_size).min(tokens.len());
            if current_end >= tokens.len() {
                break;
            }

            for i in (start_pos + MIN_DECLARATION_TOKENS)..current_end {
                if tokens[i].to_string() == "(" {
                    paren_pos = Some(i);
                    break;
                }
            }
            if paren_pos.is_some() {
                break;
            }
        }

        if paren_pos.is_none() {
            return Ok(None);
        }
    }

    let paren_start = paren_pos.unwrap();

    // Step 4: Find matching closing parenthesis, expanding window as needed
    let mut paren_end = None;
    let mut depth = 0;
    current_end = (paren_start + 3).min(tokens.len()); // Start with minimal expansion

    loop {
        for i in paren_start..current_end {
            match tokens[i].to_string().as_str() {
                "(" => depth += 1,
                ")" => {
                    depth -= 1;
                    if depth == 0 {
                        paren_end = Some(i);
                        break;
                    }
                }
                _ => {}
            }
        }

        if paren_end.is_some() || current_end >= tokens.len() {
            break;
        }

        // Expand window progressively
        current_end = (current_end + 5).min(tokens.len());
    }

    if paren_end.is_none() {
        return Ok(None);
    }
    let params_end = paren_end.unwrap();

    // Step 5: Check if declaration (;) or definition ({) - start with minimum
    let final_end = params_end + 1;
    if final_end >= tokens.len() {
        return Ok(Some(start_pos..params_end + 1));
    }

    let next_token = tokens[final_end].to_string();
    if next_token == ";" {
        // Function declaration - we have complete minimum window
        return Ok(Some(start_pos..final_end + 1));
    } else if next_token == "{" {
        // Function definition - find matching brace with progressive expansion
        let mut brace_end = None;
        let mut brace_depth = 0;
        current_end =
            (final_end + MIN_DEFINITION_TOKENS - MIN_DECLARATION_TOKENS).min(tokens.len());

        loop {
            for i in final_end..current_end {
                match tokens[i].to_string().as_str() {
                    "{" => brace_depth += 1,
                    "}" => {
                        brace_depth -= 1;
                        if brace_depth == 0 {
                            brace_end = Some(i);
                            break;
                        }
                    }
                    _ => {}
                }
            }

            if brace_end.is_some() || current_end >= tokens.len() {
                break;
            }

            // Expand window for function body
            current_end = (current_end + 10).min(tokens.len());
        }

        if let Some(body_end) = brace_end {
            return Ok(Some(start_pos..body_end + 1));
        }
    }

    // Fallback: minimum viable signature
    Ok(Some(start_pos..params_end + 1))
}

/// Fallback extraction when patterns don't match
fn extract_function_fallback(tokens: &[Token]) -> Result<Option<ExtractedElement>, C2RError> {
    if tokens.len() < 3 {
        return Ok(None);
    }

    // Basic parsing: find type, name, parameters
    let mut return_type = Vec::new();
    let mut name = String::new();
    let mut found_paren = false;

    for (i, token) in tokens.iter().enumerate() {
        let token_str = token.to_string();
        if token_str == "(" {
            found_paren = true;
            if i > 0 {
                name = tokens[i - 1].to_string();
                return_type = tokens[..i - 1].to_vec();
            }
            break;
        }
    }

    if !found_paren || name.is_empty() {
        return Ok(None);
    }

    let extracted_function = ExtractedFunction {
        name: name.clone(),
        return_type,
        parameters: Vec::new(), // Simplified for fallback
        body: Vec::new(),
        is_variadic: false,
        from_recovery: true,
        is_definition: tokens.iter().any(|t| t.to_string() == "{"),
        is_static: tokens.iter().any(|t| t.to_string() == "static"),
        is_inline: tokens.iter().any(|t| t.to_string() == "inline"),
        is_extern: tokens.iter().any(|t| t.to_string() == "extern"),
        code: tokens
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<_>>()
            .join(" "),
        tokens: tokens.to_vec(),
    };

    Ok(Some(ExtractedElement::Function(extracted_function)))
}

/// Convert callback: Converts extracted function to Rust
pub fn convert_function(token_range: Range<usize>) -> Result<Option<ConvertedElement>, C2RError> {
    // Use cloned context to get the actual tokens from global context
    let mut context = crate::context!();
    context.pull();
    let tokens_in_range = &context.tokens[token_range.clone()];

    // Filter out Token::n() from the retrieved tokens
    let filtered_tokens: Vec<Token> = tokens_in_range
        .iter()
        .filter(|token| !matches!(token, crate::Token::n()))
        .cloned()
        .collect();

    if filtered_tokens.is_empty() {
        return Ok(None);
    }

    // Use the filtered tokens with a full range for extraction
    if let Some(ExtractedElement::Function(element)) = extract_function(token_range.clone())? {
        let code = generate_rust_function(&element)?;

        Ok(Some(ConvertedElement::Function(ConvertedFunction {
            name: element.name,
            return_type: element
                .return_type
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(" "),
            parameters: element.parameters.iter().map(|t| t.to_string()).collect(),
            body: element
                .body
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(" "),
            code,
            is_unsafe: false,
            is_public: true,
            is_definition: element.is_definition,
        })))
    } else {
        Ok(None)
    }
}

/// Document callback: Generates documentation for function
fn document_function(info: ElementInfo) -> Result<Option<String>, C2RError> {
    if let ElementInfo::Function(function_info) = info {
        let doc = format!(
            "/// Converted C function: {}\n/// Parameters: {}\n/// Return type: {}\n",
            function_info.name,
            function_info.parameters.join(", "),
            function_info.return_type
        );
        Ok(Some(doc))
    } else {
        Ok(None)
    }
}

/// Report callback: Collects function handler reports
pub fn report_function() -> Result<HandlerReport, C2RError> {
    let context = context!();
    let reports = context.get_reports_by_handler("function");

    let (info_count, warning_count, error_count) =
        reports
            .iter()
            .fold((0, 0, 0), |acc, report| match report.level {
                Error => (acc.0, acc.1, acc.2 + 1),
                Warning => (acc.0, acc.1 + 1, acc.2),
                _ => (acc.0 + 1, acc.1, acc.2),
            });

    Ok(HandlerReport {
        report_id: Box::new(Id::get(&Id::gen_name("function_handler"))),
        handler_id: Box::new(Id::get("function_handler")),
        handler_name: "function".to_string(),
        function_name: "report_function".to_string(),
        message: format!(
            "Function handler summary: {} reports ({} info, {} warnings, {} errors)",
            reports.len(),
            info_count,
            warning_count,
            error_count
        ),
        level: match (error_count, warning_count) {
            (0, 0) => Info,
            (0, _) => Warning,
            _ => Error,
        },
        tokens_processed: reports.len(),
        tokens_consumed: 0,
        phase: Report,
        success: error_count == 0,
        metadata: std::collections::HashMap::new(),
    })
}

/// Parse function tokens into components
fn parse_function_tokens(
    tokens: &[Token],
) -> Result<(Vec<Token>, String, Vec<Token>, Vec<Token>), C2RError> {
    let mut i = 0;
    let mut return_type = Vec::new();
    let mut name = String::new();
    let mut params = Vec::new();
    let mut body = Vec::new();

    // Find function name (identifier before opening parenthesis)
    while i < tokens.len() {
        if i + 1 < tokens.len() && tokens[i + 1].to_string() == "(" {
            name = tokens[i].to_string();
            return_type = tokens[..i].to_vec();
            break;
        }
        i += 1;
    }

    if name.is_empty() {
        return Err(C2RError::new(
            Kind::Other,
            Reason::Empty("function name"),
            None,
        ));
    }

    // Find parameters
    i += 2; // Skip name and opening parenthesis
    let mut paren_depth = 0;
    while i < tokens.len() {
        let token_str = tokens[i].to_string();
        if token_str == "(" {
            paren_depth += 1;
        } else if token_str == ")" {
            if paren_depth == 0 {
                break;
            }
            paren_depth -= 1;
        }
        params.push(tokens[i].clone());
        i += 1;
    }

    // Find body (if exists)
    i += 1; // Skip closing parenthesis
    if i < tokens.len() && tokens[i].to_string() == "{" {
        i += 1; // Skip opening brace
        let mut brace_depth = 0;
        while i < tokens.len() {
            let token_str = tokens[i].to_string();
            if token_str == "{" {
                brace_depth += 1;
            } else if token_str == "}" {
                if brace_depth == 0 {
                    break;
                }
                brace_depth -= 1;
            }
            body.push(tokens[i].clone());
            i += 1;
        }
    }

    Ok((return_type, name, params, body))
}

/// Generate Rust function code
fn generate_rust_function(func: &ExtractedFunction) -> Result<String, C2RError> {
    let mut code = String::new();

    // Add pub keyword
    code.push_str("pub ");

    // Add unsafe if needed (basic heuristic)
    if func.name.contains("unsafe") || func.body.iter().any(|t| t.to_string().contains("malloc")) {
        code.push_str("unsafe ");
    }

    code.push_str("fn ");
    code.push_str(&func.name);
    code.push('(');

    // Convert parameters
    let param_strs: Vec<String> = func
        .parameters
        .iter()
        .map(|p| convert_parameter(&p.to_string()))
        .collect();
    code.push_str(&param_strs.join(", "));

    code.push(')');

    // Add return type
    let return_type_str = func
        .return_type
        .iter()
        .map(|t| t.to_string())
        .collect::<Vec<_>>()
        .join(" ");

    if !return_type_str.is_empty() && return_type_str != "void" {
        code.push_str(" -> ");
        code.push_str(&convert_c_type_to_rust(&return_type_str));
    }

    // Add body
    if func.is_definition && !func.body.is_empty() {
        code.push_str(" {\n    // TODO: Convert function body\n    unimplemented!()\n}");
    } else {
        code.push(';');
    }

    Ok(code)
}

/// Convert C parameter to Rust parameter
fn convert_parameter(param: &str) -> String {
    let parts: Vec<&str> = param.trim().split_whitespace().collect();
    if parts.len() >= 2 {
        let param_name = parts.last().unwrap();
        let param_type = parts[..parts.len() - 1].join(" ");
        format!("{}: {}", param_name, convert_c_type_to_rust(&param_type))
    } else {
        format!("{}: i32", param) // Default fallback
    }
}

/// Convert C type to Rust type
fn convert_c_type_to_rust(c_type: &str) -> String {
    match c_type.trim() {
        "int" => "i32".to_string(),
        "char" => "i8".to_string(),
        "float" => "f32".to_string(),
        "double" => "f64".to_string(),
        "void" => "()".to_string(),
        "char*" | "char *" => "*const i8".to_string(),
        "int*" | "int *" => "*mut i32".to_string(),
        _ => c_type.to_string(), // Keep original for now
    }
}

fn extract_function_info(tokens: &[Token]) -> FunctionInfo {
    let mut function_info = FunctionInfo {
        name: String::new(),
        return_type: String::new(),
        parameter_count: 0,
        is_definition: false,
        is_static: false,
        is_unsafe: false,
        complexity: "simple".to_string(),
        has_body: false,
        is_declaration: false,
        is_extern: false,
        is_inline: false,
        parameters: Vec::new(),
    };

    // Extract function name
    for i in 0..tokens.len().saturating_sub(1) {
        if tokens[i + 1].to_string() == "(" {
            function_info.name = tokens[i].to_string();
            break;
        }
    }

    // Extract return type
    let mut return_type_tokens = Vec::new();
    for token in tokens {
        if token.to_string() == function_info.name {
            break;
        }
        if !matches!(token.to_string().as_str(), "static" | "inline" | "extern") {
            return_type_tokens.push(token.to_string());
        }
    }
    function_info.return_type = return_type_tokens.join(" ");

    // Count parameters
    if let Some(start) = tokens.iter().position(|t| t.to_string() == "(") {
        if let Some(end) = tokens.iter().position(|t| t.to_string() == ")") {
            let param_tokens = &tokens[start + 1..end];
            function_info.parameter_count = if param_tokens.is_empty()
                || (param_tokens.len() == 1 && param_tokens[0].to_string() == "void")
            {
                0
            } else {
                param_tokens.iter().filter(|t| t.to_string() == ",").count() + 1
            };
        }
    }

    // Check if it's a definition (has body)
    function_info.is_definition = tokens.iter().any(|t| t.to_string() == "{");
    function_info.has_body = function_info.is_definition;
    function_info.is_declaration = !function_info.is_definition;

    // Check modifiers
    function_info.is_static = tokens.iter().any(|t| t.to_string() == "static");
    function_info.is_extern = tokens.iter().any(|t| t.to_string() == "extern");
    function_info.is_inline = tokens.iter().any(|t| t.to_string() == "inline");
    function_info.is_unsafe = tokens.iter().any(|t| {
        let token_str = t.to_string();
        token_str.contains("*") || token_str == "malloc" || token_str == "free"
    });

    function_info
}

/// Redirect callback: Handles nested structures within function bodies
pub fn redirect_function(
    token_range: Range<usize>,
    result: HandlerResult,
) -> Result<HandlerResult, C2RError> {
    let mut context = crate::context!();
    context.pull();
    let tokens = &context.tokens[token_range.clone()];
    let mut filtered_tokens: Vec<Token> = tokens
        .iter()
        .filter(|token| !matches!(token, crate::Token::n()))
        .cloned()
        .collect();
    let tokens = &mut filtered_tokens;

    // Look for nested structures that need to be processed by other handlers
    let mut i = 0;
    while i < tokens.len() {
        let token_str = tokens[i].to_string();

        if token_str == "struct" || token_str == "enum" || token_str == "union" {
            // Found nested structure - find its boundaries
            let struct_start = i;
            let mut brace_count = 0;
            let mut struct_end = struct_start;

            // Find the end of the structure definition
            for j in struct_start..tokens.len() {
                match tokens[j].to_string().as_str() {
                    "{" => brace_count += 1,
                    "}" => {
                        brace_count -= 1;
                        if brace_count == 0 {
                            struct_end = j + 1;
                            break;
                        }
                    }
                    ";" if brace_count == 0 => {
                        struct_end = j + 1;
                        break;
                    }
                    _ => {}
                }
            }

            if struct_end > struct_start {
                // Mark tokens as consumed
                for idx in struct_start..struct_end {
                    tokens[idx] = Token::n();
                }

                report!(
                    "function_handler",
                    "redirect_function",
                    ReportLevel::Info,
                    HandlerPhase::Convert,
                    format!("Redirected nested {} to appropriate handler", token_str),
                    true
                );
            }
            i = struct_end;
        } else {
            i += 1;
        }
    }

    // Return updated result
    match result {
        HandlerResult::Converted(element, range, code, id) => {
            Ok(HandlerResult::Converted(element, range, code, id))
        }
        other => Ok(other),
    }
}

/// Result callback: Postprocesses generated function code, adds documentation, and enhances formatting
fn result_function(
    token_range: Range<usize>,
    result: HandlerResult,
) -> Result<HandlerResult, C2RError> {
    let _id = Id::get("result_function");

    report!(
        "function_handler",
        "result_function",
        Info,
        Report,
        "Postprocessing function conversion result",
        true
    );

    match result {
        HandlerResult::Converted(element, _, code, id) => {
            // Extract function information for documentation
            let context = crate::context!();
            let func_info = extract_function_info(&context.tokens[token_range.clone()]);

            // Generate documentation about the function conversion
            let doc_comment =
                document_function(ElementInfo::Function(func_info.clone()))?.unwrap_or_default();

            // Enhance the Rust code with documentation and metadata
            let mut enhanced_code = String::new();

            // Add metadata comment for traceability
            let metadata_comment = format!(
                "// [C2R] Function converted from C to Rust - {}: {}\n",
                func_info.name, func_info.return_type
            );
            enhanced_code.push_str(&metadata_comment);

            // Add the documentation comment if substantial
            if !doc_comment.trim().is_empty() {
                enhanced_code.push_str(&doc_comment);
                enhanced_code.push('\n');
            }

            // Add the original converted code
            enhanced_code.push_str(&code);

            report!(
                "function_handler",
                "result_function",
                Info,
                Report,
                format!(
                    "Enhanced function '{}' with {} lines of documentation",
                    func_info.name,
                    doc_comment.lines().count()
                ),
                true
            );

            Ok(HandlerResult::Converted(
                element,
                token_range,
                enhanced_code,
                id,
            ))
        }
        _ => {
            report!(
                "function_handler",
                "result_function",
                Warning,
                Report,
                "Function result was not in Completed state, returning as-is",
                true
            );

            Ok(result)
        }
    }
}
