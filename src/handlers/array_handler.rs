//! Array handler for C to Rust array conversion
//! Uses Patternizer exclusively for detection and conversion

use super::common::not_handled;
use crate::pattern::PatternResult;
use crate::token::Token;
use crate::{ArrayInfo, ConvertedArray, ElementInfo, HandlerReport};
use crate::{
    C2RError, ConvertedElement, ExtractedArray, ExtractedElement, HandlerPhase, HandlerResult, Id,
    ReportLevel,
};
use crate::{
    HandlerPhase::{Extract, Handle, Process, Report},
    ReportLevel::{Info, Warning},
};
use crate::{context, report};
use std::ops::Range;

/// Report callback: Collects array handler reports
pub fn report_array() -> Result<HandlerReport, C2RError> {
    let context = context!();
    let reports = context.get_reports_by_handler("array_handler");

    let (info_count, warning_count, error_count) =
        reports
            .iter()
            .fold((0, 0, 0), |acc, report| match report.level {
                ReportLevel::Error => (acc.0, acc.1, acc.2 + 1),
                ReportLevel::Warning => (acc.0, acc.1 + 1, acc.2),
                _ => (acc.0 + 1, acc.1, acc.2),
            });

    Ok(HandlerReport {
        report_id: Box::new(Id::get(&Id::gen_name("array_handler"))),
        handler_id: Box::new(Id::get("array_handler")),
        handler_name: "array_handler".to_string(),
        function_name: "report_array".to_string(),
        message: format!(
            "Array handler summary: {} reports ({} info, {} warnings, {} errors)",
            reports.len(),
            info_count,
            warning_count,
            error_count
        ),
        level: match (error_count, warning_count) {
            (0, 0) => ReportLevel::Info,
            (0, _) => ReportLevel::Warning,
            _ => ReportLevel::Error,
        },
        tokens_processed: reports.len(),
        tokens_consumed: 0,
        phase: HandlerPhase::Report,
        success: error_count == 0,
        metadata: std::collections::HashMap::new(),
    })
}

/// Handle callback: Delegates to extract for recursive window expansion
fn handle_array(token_range: Range<usize>) -> Result<HandlerResult, C2RError> {
    let mut context = context!();
    context.pull();
    let tokens = if token_range.end <= context.tokens.len() {
        &context.tokens[token_range.clone()]
    } else {
        &[]
    };
    let filtered_tokens: Vec<Token> = tokens
        .iter()
        .filter(|token| !matches!(token, Token::n()))
        .cloned()
        .collect();
    let tokens = &filtered_tokens;
    report!(
        "array_handler",
        "handle_array",
        Info,
        Handle,
        "Delegating to extract for recursive window expansion",
        true
    );

    // Delegate to extract callback for recursive processing
    match extract_array(token_range.clone())? {
        Some(ExtractedElement::Array(extracted_array)) => {
            let code = extracted_array.code.clone();
            let consumed_tokens = if code.is_empty() {
                4
            } else {
                tokens.len().min(10)
            };

            // Convert the extracted array to a ConvertedElement
            let converted_array = ConvertedArray {
                element_type: extracted_array.element_type.clone(),
                size: extracted_array.size.clone(),
                code,
                is_declaration: extracted_array.is_declaration,
            };

            Ok(HandlerResult::Converted(
                ConvertedElement::Array(converted_array),
                0..consumed_tokens,
                extracted_array.code,
                Id::get("array_handler"),
            ))
        }
        _ => {
            report!(
                "array_handler",
                "handle_array",
                Warning,
                Handle,
                "Extract did not find valid array",
                false
            );
            not_handled()
        }
    }
}

/// Process callback: Lightweight detection using minimum tokens (4 tokens: int arr[5];)
fn process_array(token_range: Range<usize>) -> Result<bool, C2RError> {
    let mut context = crate::config::Context::default();
    context.pull();
    let tokens = if token_range.end <= context.tokens.len() {
        &context.tokens[token_range.clone()]
    } else {
        &[]
    };
    let filtered_tokens: Vec<Token> = tokens
        .iter()
        .filter(|token| !matches!(token, Token::n()))
        .cloned()
        .collect();
    let tokens = &filtered_tokens;
    const MIN_ARRAY_TOKENS: usize = 4; // int arr[5];

    if tokens.len() < MIN_ARRAY_TOKENS {
        return Ok(false);
    }

    report!(
        "array_handler",
        "process_array",
        Info,
        Process,
        format!("Lightweight array detection on {} tokens", tokens.len()),
        true
    );

    // Exclude preprocessor directives and includes
    let first_token = tokens[0].to_string();
    if first_token.starts_with('#') {
        return Ok(false);
    }

    // Look for array pattern: type identifier[size] or type identifier[]
    // Must have: 1) square brackets '[' ']' 2) identifier before brackets 3) type before identifier

    // Use enhanced Patternizer patterns for arrays
    let array_decl_result = context
        .patternizer
        .match_pattern("array_declaration", tokens);
    let array_multi_result = context.patternizer.match_pattern("array_multi", tokens);

    let result = matches!(
        array_decl_result,
        PatternResult::Match { .. } | PatternResult::Sequence { .. } | PatternResult::Fuzzy { .. }
    ) || matches!(
        array_multi_result,
        PatternResult::Match { .. } | PatternResult::Sequence { .. } | PatternResult::Fuzzy { .. }
    );

    if result {
        report!(
            "array_handler",
            "process_array",
            Info,
            Process,
            format!(
                "Detected array declaration pattern in {} tokens",
                tokens.len()
            ),
            true
        );
    }

    Ok(result)
}

/// Creates an array handler that uses Patternizer exclusively
pub fn create_array_handler() -> crate::handler::Handler {
    let handler_id = Id::get("array_handler");
    let handler_role = "array";
    let priority = 120;

    super::create_handler(
        handler_id,
        handler_role,
        priority,
        Some(process_array),
        Some(handle_array),
        Some(extract_array),
        Some(convert_array),
        Some(document_array),
        Some(report_array),
        Some(result_array),   // Result callback for final processing
        Some(redirect_array), // Redirect callback for nested structures
    )
}

/// Result callback: Postprocesses array conversion with documentation
fn result_array(
    token_range: Range<usize>,
    result: HandlerResult,
) -> Result<HandlerResult, C2RError> {
    let mut context = crate::config::Context::default();
    context.pull();
    let tokens = if token_range.end <= context.tokens.len() {
        &context.tokens[token_range.clone()]
    } else {
        &[]
    };
    let filtered_tokens: Vec<Token> = tokens
        .iter()
        .filter(|token| !matches!(token, Token::n()))
        .cloned()
        .collect();
    let tokens = &filtered_tokens;
    let _id = Id::get("result_array");

    report!(
        "array_handler",
        "result_array",
        Info,
        Report,
        "Postprocessing array conversion result",
        true
    );

    match result {
        HandlerResult::Converted(element, token_range, code, id) => {
            // Extract array information from tokens for documentation
            let array_info = extract_array_info(tokens);

            // Generate documentation about the array conversion
            let doc_comment =
                document_array(ElementInfo::Array(array_info.clone()))?.unwrap_or_default();

            // Enhance the Rust code with documentation and metadata
            let mut enhanced_code = String::new();

            // Add metadata comment for traceability
            let metadata_comment = format!(
                "// [C2R] Array converted from C to Rust - {}: {}\n",
                array_info.name, array_info.element_type
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
                "array_handler",
                "result_array",
                Info,
                Report,
                format!(
                    "Enhanced array '{}' with {} lines of documentation",
                    array_info.name,
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
                "array_handler",
                "result_array",
                Warning,
                Report,
                "Array result was not in Completed state, returning as-is",
                true
            );

            Ok(result)
        }
    }
}

/// Extract array information from tokens for result processing
fn extract_array_info(tokens: &[Token]) -> ArrayInfo {
    let mut name = String::new();
    let mut element_type = String::new();
    let mut size = None;
    let mut is_static = false;
    let mut is_const = false;
    let mut has_initializer = false;

    // Basic parsing to extract array information
    for (i, token) in tokens.iter().enumerate() {
        match token {
            Token::s(keyword) if keyword == "static" => is_static = true,
            Token::s(keyword) if keyword == "const" => is_const = true,
            Token::s(keyword) if keyword == "int" || keyword == "char" || keyword == "float" => {
                element_type = keyword.clone();
            }
            Token::s(id) if element_type.is_empty() => {
                element_type = id.clone();
            }
            Token::s(id) if !element_type.is_empty() && name.is_empty() => {
                name = id.clone();
            }
            Token::l("[") => {
                if i + 1 < tokens.len() {
                    if let Token::i(num) = &tokens[i + 1] {
                        size = Some(num.to_string());
                    } else if let Token::s(num) = &tokens[i + 1] {
                        size = Some(num.clone());
                    }
                }
            }
            Token::l("=") => has_initializer = true,
            _ => {}
        }
    }

    if name.is_empty() {
        name = "unnamed_array".to_string();
    }
    if element_type.is_empty() {
        element_type = "unknown".to_string();
    }

    ArrayInfo {
        name,
        element_type,
        size,
        is_declaration: true,
        is_static,
        is_const,
        has_initializer,
        complexity: if is_static || is_const || has_initializer {
            "complex".to_string()
        } else {
            "simple".to_string()
        },
    }
}

/// Document callback: Generates documentation for arrays
fn document_array(info: ElementInfo) -> Result<Option<String>, C2RError> {
    if let ElementInfo::Array(array_info) = info {
        let mut doc = format!("/// Converted C array: {}\n", array_info.name);

        // For complex arrays, use Patternizer to determine purpose
        if array_info.complexity == "complex" {
            // Determine array purpose based on patterns
            let purpose = if array_info.is_static && array_info.is_const {
                "static constant array for lookup tables or configuration"
            } else if array_info.has_initializer && array_info.element_type.contains("char") {
                "string buffer or character array"
            } else if array_info.has_initializer {
                "initialized data array"
            } else if array_info.is_static {
                "static array for persistent storage"
            } else {
                "dynamic array for data processing"
            };

            doc.push_str(&format!("/// Purpose: {}\n", purpose));
        }

        doc.push_str(&format!("/// Element type: {}\n", array_info.element_type));

        if let Some(ref size) = array_info.size {
            doc.push_str(&format!("/// Size: {} elements\n", size));
        }

        if array_info.is_static {
            doc.push_str("/// Storage: Static (persistent across function calls)\n");
        }

        if array_info.is_const {
            doc.push_str("/// Mutability: Read-only\n");
        }

        Ok(Some(doc))
    } else {
        Ok(None)
    }
}

/// Redirect callback: Handles nested structures within array declarations
pub fn redirect_array(
    token_range: Range<usize>,
    result: HandlerResult,
) -> Result<HandlerResult, C2RError> {
    let mut context = crate::config::Context::default();
    context.pull();
    let tokens = if token_range.end <= context.tokens.len() {
        &context.tokens[token_range.clone()]
    } else {
        &[]
    };
    let filtered_tokens: Vec<Token> = tokens
        .iter()
        .filter(|token| !matches!(token, Token::n()))
        .cloned()
        .collect();
    let tokens = &filtered_tokens;
    report!(
        "array_handler",
        "redirect_array",
        Info,
        HandlerPhase::Convert,
        "Checking array for nested structures",
        true
    );

    // Arrays can contain complex initializers that might need redirection
    let mut i = 0;
    let mut processed_tokens = Vec::new();

    while i < tokens.len() {
        let token = &tokens[i];

        // Check for array initializers with function calls
        if token.to_string() == "{" {
            let mut brace_depth = 1;
            let mut init_end = i;

            for j in (i + 1)..tokens.len() {
                match tokens[j].to_string().as_str() {
                    "{" => brace_depth += 1,
                    "}" => {
                        brace_depth -= 1;
                        if brace_depth == 0 {
                            init_end = j;
                            break;
                        }
                    }
                    _ => {}
                }
            }

            if init_end > i {
                // Look for function calls within the initializer
                let mut found_function_call = false;
                for k in (i + 1)..init_end {
                    if k + 1 < init_end && tokens[k + 1].to_string() == "(" {
                        found_function_call = true;
                        break;
                    }
                }

                if found_function_call {
                    report!(
                        "array_handler",
                        "redirect_array",
                        Info,
                        HandlerPhase::Convert,
                        "Found function calls in array initializer, marking tokens as processed",
                        true
                    );

                    // Mark the initializer tokens as processed
                    for _ in i..=init_end {
                        processed_tokens.push(Token::n());
                    }
                    i = init_end + 1;
                    continue;
                }
            }
        }

        // Check for designated initializers with complex expressions
        if token.to_string() == "." && i + 1 < tokens.len() {
            // This might be a designated initializer (.field = value)
            let mut equals_pos = None;
            for j in (i + 1)..tokens.len() {
                if tokens[j].to_string() == "=" {
                    equals_pos = Some(j);
                    break;
                }
                if tokens[j].to_string() == "," || tokens[j].to_string() == "}" {
                    break;
                }
            }

            if let Some(eq_pos) = equals_pos {
                // Check if the value after = is a function call
                if eq_pos + 2 < tokens.len() && tokens[eq_pos + 2].to_string() == "(" {
                    report!(
                        "array_handler",
                        "redirect_array",
                        Info,
                        HandlerPhase::Convert,
                        "Found function call in designated initializer, marking as processed",
                        true
                    );

                    // Mark the designated initializer as processed
                    for _ in i..=eq_pos {
                        processed_tokens.push(Token::n());
                    }
                    i = eq_pos + 1;
                    continue;
                }
            }
        }

        processed_tokens.push(token.clone());
        i += 1;
    }

    Ok(result)
}

/// Extract array using recursive window expansion starting from minimum tokens
fn extract_array(token_range: Range<usize>) -> Result<Option<ExtractedElement>, C2RError> {
    let context = crate::context!();
    let filtered_tokens: Vec<Token> = context.tokens[token_range.clone()]
        .iter()
        .filter(|token| !matches!(token, Token::n()))
        .cloned()
        .collect();
    let tokens = &filtered_tokens;
    const MIN_ARRAY_TOKENS: usize = 4; // int arr[5];

    if tokens.len() < MIN_ARRAY_TOKENS {
        return Ok(None);
    }

    report!(
        "array_handler",
        "extract_array",
        Info,
        Extract,
        format!("Starting array extraction with {} tokens", tokens.len()),
        true
    );

    // Find array start within the token range
    if let Some(array_start) = find_array_start_in_range(tokens, token_range.clone()) {
        report!(
            "array_handler",
            "extract_array",
            Info,
            Extract,
            format!("Found array start at position {}", array_start),
            true
        );

        // Try recursive window expansion to capture complete array
        let window_start = array_start;
        let mut window_size = MIN_ARRAY_TOKENS;

        while window_start + window_size <= tokens.len() {
            if let Some(complete_tokens) = expand_to_array(tokens, window_start, window_size) {
                report!(
                    "array_handler",
                    "extract_array",
                    Info,
                    Extract,
                    format!(
                        "Successfully expanded to complete array with {} tokens",
                        complete_tokens.len()
                    ),
                    true
                );

                return extract_array_from_complete_tokens(&complete_tokens);
            }

            window_size += 1;
            if window_size > 15 {
                // Prevent runaway expansion
                break;
            }
        }
    }

    report!(
        "array_handler",
        "extract_array",
        Info,
        Extract,
        "Patternizer patterns did not match complete array - using fallback extraction",
        true
    );

    extract_array_fallback(tokens)
}

/// Helper function to find array start position within token range
fn find_array_start_in_range(tokens: &[Token], range: Range<usize>) -> Option<usize> {
    let start = range.start.min(tokens.len());
    let end = range.end.min(tokens.len());

    for i in start..end {
        if detect_array_start(tokens, i) {
            return Some(i);
        }
    }
    None
}

/// Helper function to detect if array starts at given position
fn detect_array_start(tokens: &[Token], pos: usize) -> bool {
    if pos >= tokens.len() {
        return false;
    }

    // Look for array bracket patterns
    for i in pos..tokens.len().min(pos + 5) {
        if tokens[i].to_string().contains('[') {
            return true;
        }
    }

    false
}

/// Helper function to expand window to capture complete array
fn expand_to_array(tokens: &[Token], start: usize, initial_size: usize) -> Option<Vec<Token>> {
    if start >= tokens.len() {
        return None;
    }

    let mut current_size = initial_size;
    let max_expansion = (tokens.len() - start).min(15);

    while current_size <= max_expansion {
        let end_pos = (start + current_size).min(tokens.len());
        let window = &tokens[start..end_pos];

        if is_complete_array_declaration(window) {
            return Some(window.to_vec());
        }

        current_size += 1;
    }

    None
}

/// Helper function to check if token window contains complete array declaration
fn is_complete_array_declaration(tokens: &[Token]) -> bool {
    if tokens.len() < 4 {
        return false;
    }

    // Check for array brackets
    let has_open_bracket = tokens.iter().any(|t| t.to_string().contains('['));
    let has_close_bracket = tokens.iter().any(|t| t.to_string().contains(']'));

    if !has_open_bracket || !has_close_bracket {
        return false;
    }

    // Check for termination
    let last_token = tokens.last().unwrap().to_string();

    // Array declaration: int arr[5];
    if last_token == ";" {
        return true;
    }

    // Array initialization: int arr[] = {...};
    if last_token == "}" {
        return tokens.iter().any(|t| t.to_string() == "=");
    }

    false
}

/// Helper function to extract array from complete token sequence
fn extract_array_from_complete_tokens(
    tokens: &[Token],
) -> Result<Option<ExtractedElement>, C2RError> {
    let (name, element_type, size, is_declaration) = parse_array_tokens(tokens, "array")?;

    let extracted_array = ExtractedArray {
        name: name.clone(),
        element_type: element_type.clone(),
        size: size.clone(),
        is_declaration,
        code: generate_rust_array_code(&name, &element_type, &size, tokens)?,
    };

    Ok(Some(ExtractedElement::Array(extracted_array)))
}

/// Helper function for fallback array extraction
fn extract_array_fallback(tokens: &[Token]) -> Result<Option<ExtractedElement>, C2RError> {
    // Simple fallback: look for brackets
    if let Some(bracket_pos) = tokens.iter().position(|t| t.to_string().contains('[')) {
        let mut name = "unknown_array".to_string();
        let mut element_type = "i32".to_string();
        let mut size = "0".to_string();

        // Try to extract name (token before bracket)
        if bracket_pos > 0 {
            name = tokens[bracket_pos - 1].to_string();
        }

        // Try to extract element type (token before name)
        if bracket_pos > 1 {
            element_type = tokens[bracket_pos - 2].to_string();
        }

        // Try to extract size from within brackets
        let bracket_token = &tokens[bracket_pos].to_string();
        if let Some(start) = bracket_token.find('[') {
            if let Some(end) = bracket_token.find(']') {
                if end > start + 1 {
                    size = bracket_token[start + 1..end].to_string();
                    if size.is_empty() {
                        size = "0".to_string();
                    }
                }
            }
        }

        let extracted_array = ExtractedArray {
            name: name.clone(),
            element_type: element_type.clone(),
            size: size.clone(),
            is_declaration: true,
            code: format!(
                "let {}: [{}; {}] = [{}::default(); {}];",
                name, element_type, size, element_type, size
            ),
        };

        return Ok(Some(ExtractedElement::Array(extracted_array)));
    }

    Ok(None)
}

/// Helper function to generate Rust array code
fn generate_rust_array_code(
    name: &str,
    element_type: &str,
    size: &str,
    tokens: &[Token],
) -> Result<String, C2RError> {
    let mut code = String::new();

    // Convert C types to Rust types
    let rust_type = match element_type {
        "int" => "i32",
        "char" => "u8",
        "float" => "f32",
        "double" => "f64",
        "long" => "i64",
        "short" => "i16",
        _ => element_type,
    };

    // Check if array has initializer
    let has_initializer = tokens.iter().any(|t| t.to_string() == "=");

    if has_initializer {
        // Try to parse initializer values
        if let Some(start_brace) = tokens.iter().position(|t| t.to_string() == "{") {
            if let Some(end_brace) = tokens[start_brace..]
                .iter()
                .position(|t| t.to_string() == "}")
            {
                let end_brace = start_brace + end_brace;
                code.push_str(&format!("let {}: [{}; {}] = [", name, rust_type, size));

                let mut i = start_brace + 1;
                let mut first = true;
                while i < end_brace {
                    if tokens[i].to_string() != "," {
                        if !first {
                            code.push_str(", ");
                        }
                        code.push_str(&tokens[i].to_string());
                        first = false;
                    }
                    i += 1;
                }

                code.push_str("];");
            } else {
                // Fallback if no closing brace found
                code.push_str(&format!(
                    "let {}: [{}; {}] = [{}::default(); {}];",
                    name, rust_type, size, rust_type, size
                ));
            }
        } else {
            // Single value initializer
            code.push_str(&format!(
                "let {}: [{}; {}] = [{}::default(); {}];",
                name, rust_type, size, rust_type, size
            ));
        }
    } else {
        // No initializer - create default array
        let parsed_size = size.parse::<usize>().unwrap_or(0);
        if parsed_size == 0 {
            code.push_str(&format!("let {}: Vec<{}> = Vec::new();", name, rust_type));
        } else {
            code.push_str(&format!(
                "let {}: [{}; {}] = [{}::default(); {}];",
                name, rust_type, size, rust_type, size
            ));
        }
    }

    Ok(code)
}

/// Helper function to parse array tokens based on pattern type
fn parse_array_tokens(
    tokens: &[Token],
    _pattern: &str,
) -> Result<(String, String, String, bool), C2RError> {
    // Enhanced array parsing with proper type and size detection
    let mut name = String::new();
    let mut element_type = String::new();
    let mut size = String::new();
    let mut is_declaration = false;

    // Look for array pattern: type identifier[size]
    for (i, token) in tokens.iter().enumerate() {
        if token.to_string() == "[" {
            // Found bracket - this indicates an array
            if i > 0 {
                name = tokens[i - 1].to_string();
                // Look backwards for type - handle storage class specifiers
                if i > 1 {
                    let potential_type = tokens[i - 2].to_string();
                    // Check if this is a storage class specifier
                    if matches!(
                        potential_type.as_str(),
                        "static" | "const" | "extern" | "volatile"
                    ) && i > 2
                    {
                        // Skip storage class and get actual type
                        element_type = tokens[i - 3].to_string();
                    } else {
                        element_type = potential_type;
                    }
                    is_declaration = true;
                }
            }

            // Extract size/index between brackets
            let mut j = i + 1;
            while j < tokens.len() && tokens[j].to_string() != "]" {
                if !size.is_empty() {
                    size.push(' ');
                }
                size.push_str(&tokens[j].to_string());
                j += 1;
            }

            // If size is empty, provide a default based on context
            if size.is_empty() && is_declaration {
                size = "1".to_string(); // Default to size 1 for empty brackets like char[]
            }
            break;
        }
    }

    // Fallback: if no brackets found, might be a simple declaration
    if name.is_empty() && tokens.len() >= 2 {
        // Handle simple cases like "int x" or "char* ptr"
        if tokens.len() == 2 {
            element_type = tokens[0].to_string();
            name = tokens[1].to_string();
            is_declaration = true;
        }
    }

    Ok((name, element_type, size, is_declaration))
}

/// Convert extracted array to Rust code using Patternizer exclusively
fn convert_array(token_range: Range<usize>) -> Result<Option<ConvertedElement>, C2RError> {
    let mut context = crate::context!();
    context.pull();
    let tokens = &context.tokens[token_range.clone()];
    let filtered_tokens: Vec<Token> = tokens
        .iter()
        .filter(|token| !matches!(token, Token::n()))
        .cloned()
        .collect();
    let tokens = &filtered_tokens;
    let mut processed_tokens = tokens.to_vec();

    // Try to match array patterns
    let array_patterns = ["array", "array_declaration", "array_access"];

    for pattern in &array_patterns {
        match context.patternizer.match_pattern(pattern, tokens) {
            PatternResult::Match { consumed_tokens } => {
                // Parse array components
                let (name, element_type, size, is_declaration) =
                    parse_array_tokens(tokens, pattern)?;

                // Mark processed tokens as consumed
                for i in 0..consumed_tokens.min(processed_tokens.len()) {
                    processed_tokens[i] = Token::n();
                }

                let code = if is_declaration {
                    // Convert C type to Rust type (simplified)
                    let rust_type = match element_type.as_str() {
                        "int" => "i32",
                        "float" => "f32",
                        "double" => "f64",
                        "char" => "u8",
                        _ => "i32", // default fallback
                    };
                    format!(
                        "let mut {}: [{}; {}] = [Default::default(); {}];",
                        name, rust_type, size, size
                    )
                } else {
                    // Array access - same syntax in Rust
                    format!("{}[{}]", name, size)
                };

                let converted_array = ConvertedArray {
                    element_type: if is_declaration {
                        element_type
                    } else {
                        String::new()
                    },
                    size: size.clone(),
                    code,
                    is_declaration,
                };

                return Ok(Some(ConvertedElement::Array(converted_array)));
            }
            _ => continue,
        }
    }

    Ok(None)
}
