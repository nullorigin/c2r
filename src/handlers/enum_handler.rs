//! Enum handler for C to Rust enum conversion
//! Uses Patternizer exclusively for detection and conversion

use crate::{
    C2RError, ConvertedElement, ConvertedEnum, ElementInfo, EnumInfo, ExtractedElement,
    ExtractedEnum,
    HandlerPhase::{self, Extract, Handle, Process, Report},
    HandlerReport, HandlerResult, Id,
    ReportLevel::{self, Info, Warning},
    context, not_handled,
    pattern::{PatternResult, Patternizer},
    report,
    token::Token,
};
use std::ops::Range;

/// Report callback: Collects enum handler reports
pub fn report_enum() -> Result<HandlerReport, C2RError> {
    let context = context!();
    let reports = context.get_reports_by_handler("enum_handler");

    let (info_count, warning_count, error_count) =
        reports
            .iter()
            .fold((0, 0, 0), |acc, report| match report.level {
                ReportLevel::Error => (acc.0, acc.1, acc.2 + 1),
                ReportLevel::Warning => (acc.0, acc.1 + 1, acc.2),
                _ => (acc.0 + 1, acc.1, acc.2),
            });

    Ok(HandlerReport {
        report_id: Box::new(Id::get(&Id::gen_name("enum_handler"))),
        handler_id: Box::new(Id::get("enum_handler")),
        handler_name: "enum_handler".to_string(),
        function_name: "report_enum".to_string(),
        message: format!(
            "Enum handler summary: {} reports ({} info, {} warnings, {} errors)",
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
fn handle_enum(token_range: Range<usize>) -> Result<HandlerResult, C2RError> {
    report!(
        "enum_handler",
        "handle_enum",
        Info,
        Handle,
        "Delegating to extract for recursive window expansion",
        true
    );

    // Delegate to extract callback for recursive processing
    match extract_enum(token_range.clone())? {
        Some(ExtractedElement::Enum(extracted_enum)) => {
            let code = extracted_enum.code.clone();
            let consumed_tokens = extracted_enum.tokens.len();

            // Convert the extracted enum to a ConvertedElement
            let converted_enum = ConvertedEnum {
                variants: Vec::new(),
                code,
                is_public: true,
                repr: Some("C".to_string()),
            };

            Ok(HandlerResult::Converted(
                ConvertedElement::Enum(converted_enum),
                0..consumed_tokens,
                extracted_enum.code,
                Id::get("enum_handler"),
            ))
        }
        _ => {
            report!(
                "enum_handler",
                "handle_enum",
                Warning,
                Handle,
                "Extract did not find valid enum",
                false
            );
            not_handled()
        }
    }
}

/// Process callback: Lightweight detection using Patternizer
fn process_enum(token_range: Range<usize>) -> Result<bool, C2RError> {
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

    if tokens.len() < 2 {
        return Ok(false);
    }

    report!(
        "enum_handler",
        "process_enum",
        Info,
        Process,
        format!("Enum pattern detection on {} tokens", tokens.len()),
        true
    );

    // Try typedef_enum pattern first (higher priority)
    let typedef_pattern_result = context.patternizer.match_pattern("typedef_enum", tokens);
    let enum_pattern_result = context
        .patternizer
        .match_pattern("enum_declaration", tokens);

    let has_enum = matches!(typedef_pattern_result, PatternResult::Match { .. })
        || matches!(enum_pattern_result, PatternResult::Match { .. });

    if has_enum {
        report!(
            "enum_handler",
            "process_enum",
            Info,
            Process,
            "Found enum pattern using Patternizer",
            true
        );
        return Ok(true);
    }

    Ok(false)
}

/// Creates an enum handler that uses Patternizer exclusively
pub fn create_enum_handler() -> crate::handler::Handler {
    let handler_id = Id::get("enum_handler");
    let handler_role = "enum";
    let priority = 100;

    super::create_handler(
        handler_id,
        handler_role,
        priority,
        Some(process_enum),
        Some(handle_enum),
        Some(extract_enum),
        Some(convert_enum),
        Some(document_enum),
        Some(report_enum),
        Some(result_enum),   // Result callback for final processing
        Some(redirect_enum), // Redirect callback for nested structures
    )
}

/// Result callback: Postprocesses enum conversion with documentation
#[allow(unused_variables)]
fn result_enum(
    token_range: Range<usize>,
    result: HandlerResult,
) -> Result<HandlerResult, C2RError> {
    let _id = Id::get("result_enum");

    report!(
        "enum_handler",
        "result_enum",
        Info,
        Report,
        "Postprocessing enum conversion result",
        true
    );

    match result {
        HandlerResult::Converted(element, token_range, code, id) => {
            // Extract enum information from tokens for documentation
            let mut context = crate::context!();
            context.pull();
            let enum_info = extract_enum_info(&context.tokens[token_range.clone()]);

            // Generate documentation about the enum conversion
            let doc_comment =
                document_enum(ElementInfo::Enum(enum_info.clone()))?.unwrap_or_default();

            // Enhance the Rust code with documentation and metadata
            let mut enhanced_code = String::new();

            // Add metadata comment for traceability
            let metadata_comment = format!(
                "// [C2R] Enum converted from C to Rust - {}: {} variants\n",
                enum_info.name, enum_info.variant_count
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
                "enum_handler",
                "result_enum",
                Info,
                Report,
                format!(
                    "Enhanced enum '{}' with {} lines of documentation",
                    enum_info.name,
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
                "enum_handler",
                "result_enum",
                Warning,
                Report,
                "Enum result was not in Completed state, returning as-is",
                true
            );

            Ok(result)
        }
    }
}

/// Extract enum information from tokens for result processing
fn extract_enum_info(tokens: &[Token]) -> EnumInfo {
    let mut name = String::new();
    let mut variant_count = 0;
    let mut is_typedef = false;
    let mut is_anonymous = false;
    let mut has_explicit_values = false;
    let mut typedef_name = None;

    // Basic parsing to extract enum information
    for (i, token) in tokens.iter().enumerate() {
        match token {
            Token::s(keyword) if keyword == "typedef" => is_typedef = true,
            Token::s(keyword) if keyword == "enum" => {
                if i + 1 < tokens.len() {
                    if let Token::s(id) = &tokens[i + 1] {
                        name = id.clone();
                    } else {
                        is_anonymous = true;
                        name = "anonymous_enum".to_string();
                    }
                }
            }
            Token::l("{") => {
                // Count variants by counting identifiers within braces
                let mut brace_count = 1;
                let mut j = i + 1;
                while j < tokens.len() && brace_count > 0 {
                    match &tokens[j] {
                        Token::l("{") => brace_count += 1,
                        Token::l("}") => brace_count -= 1,
                        Token::s(_) if brace_count == 1 => variant_count += 1,
                        Token::l("=") if brace_count == 1 => has_explicit_values = true,
                        _ => {}
                    }
                    j += 1;
                }
            }
            _ => {}
        }
    }

    if name.is_empty() {
        name = "unnamed_enum".to_string();
    }

    // Check for typedef name (appears after closing brace)
    if is_typedef {
        for (i, token) in tokens.iter().enumerate() {
            if let Token::l("}") = token {
                if i + 1 < tokens.len() {
                    if let Token::s(id) = &tokens[i + 1] {
                        typedef_name = Some(id.clone());
                    }
                }
                break;
            }
        }
    }

    EnumInfo {
        name,
        variant_count,
        is_typedef,
        is_anonymous,
        is_forward_declaration: variant_count == 0,
        has_explicit_values,
        typedef_name,
        complexity: if has_explicit_values || variant_count > 5 {
            "complex".to_string()
        } else {
            "simple".to_string()
        },
    }
}

/// Document callback: Generates documentation for enums
fn document_enum(info: ElementInfo) -> Result<Option<String>, C2RError> {
    if let ElementInfo::Enum(enum_info) = info {
        let mut doc = format!("/// Converted C enum: {}\n", enum_info.name);

        // For complex enums, use Patternizer to determine purpose
        if enum_info.complexity == "complex" {
            let _patternizer = Patternizer::with_common_patterns();

            // Determine enum purpose based on patterns
            let purpose = if enum_info.has_explicit_values {
                "enumeration with explicit values for specific mappings"
            } else if enum_info.variant_count > 10 {
                "large enumeration for comprehensive state management"
            } else if enum_info.is_typedef {
                "type-aliased enumeration for enhanced type safety"
            } else if enum_info.is_anonymous {
                "anonymous enumeration for local constants"
            } else {
                "standard enumeration for categorizing values"
            };

            doc.push_str(&format!("/// Purpose: {}\n", purpose));
        }

        doc.push_str(&format!(
            "/// Variants: {} items\n",
            enum_info.variant_count
        ));

        if enum_info.is_typedef {
            if let Some(ref typedef_name) = enum_info.typedef_name {
                doc.push_str(&format!("/// Typedef name: {}\n", typedef_name));
            }
        }

        if enum_info.has_explicit_values {
            doc.push_str("/// Note: Contains explicit value assignments\n");
        }

        if enum_info.is_forward_declaration {
            doc.push_str("/// Note: Forward declaration only\n");
        }

        Ok(Some(doc))
    } else {
        Ok(None)
    }
}

/// Redirect callback: Handles nested structures within enum definitions
pub fn redirect_enum(
    token_range: Range<usize>,
    result: HandlerResult,
) -> Result<HandlerResult, C2RError> {
    report!(
        "enum_handler",
        "redirect_enum",
        Info,
        HandlerPhase::Convert,
        "Checking enum for nested structures",
        true
    );

    // Enums typically don't contain nested structures, but they can have
    // complex value expressions that might need redirection
    let mut i = 0;
    let mut processed_tokens = Vec::new();

    let mut context = crate::context!();
    context.pull();
    let tokens = &context.tokens[token_range.clone()];
    let filtered_tokens: Vec<Token> = tokens
        .iter()
        .filter(|token| !matches!(token, Token::n()))
        .cloned()
        .collect();
    let tokens = &filtered_tokens;

    while i < tokens.len() {
        let token = &tokens[i];

        // Check for function calls in enum value expressions
        if token.to_string() != "=" {
            processed_tokens.push(token.clone());
            i += 1;
            continue;
        }

        // We found an assignment, check for complex expressions
        processed_tokens.push(token.clone());
        i += 1;

        // Look ahead for function calls or complex expressions
        while i < tokens.len() {
            let next_token = &tokens[i];

            if next_token.to_string() == "," || next_token.to_string() == "}" {
                processed_tokens.push(next_token.clone());
                i += 1;
                break;
            }

            // Check for function call patterns (identifier followed by parentheses)
            if i + 1 < tokens.len() && tokens[i + 1].to_string() == "(" {
                // This might be a function call in the enum value
                let mut paren_depth = 0;
                let mut call_end = i;

                for j in (i + 1)..tokens.len() {
                    match tokens[j].to_string().as_str() {
                        "(" => paren_depth += 1,
                        ")" => {
                            paren_depth -= 1;
                            if paren_depth == 0 {
                                call_end = j;
                                break;
                            }
                        }
                        _ => {}
                    }
                }

                if call_end > i {
                    report!(
                        "enum_handler",
                        "redirect_enum",
                        Info,
                        HandlerPhase::Convert,
                        "Found function call in enum value, marking as processed",
                        true
                    );

                    // Mark the function call tokens as processed
                    for _ in i..=call_end {
                        processed_tokens.push(Token::n());
                    }
                    i = call_end + 1;
                    continue;
                }
            }

            processed_tokens.push(next_token.clone());
            i += 1;
        }
    }

    Ok(result)
}

/// Extract enum using recursive window expansion starting from minimum tokens
fn extract_enum(token_range: Range<usize>) -> Result<Option<ExtractedElement>, C2RError> {
    const MIN_ENUM_TOKENS: usize = 3; // enum Status;

    let mut context = crate::context!();
    context.pull();
    let tokens = &context.tokens[token_range.clone()];
    let filtered_tokens: Vec<Token> = tokens
        .iter()
        .filter(|token| !matches!(token, Token::n()))
        .cloned()
        .collect();
    let tokens = &filtered_tokens;

    if tokens.len() < MIN_ENUM_TOKENS {
        return Ok(None);
    }

    report!(
        "enum_handler",
        "extract_enum",
        Info,
        Extract,
        format!("Starting enum extraction with {} tokens", tokens.len()),
        true
    );

    // Find enum start within the token range
    if let Some(enum_start) = find_enum_start_in_range(tokens, token_range.clone()) {
        report!(
            "enum_handler",
            "extract_enum",
            Info,
            Extract,
            format!("Found enum start at position {}", enum_start),
            true
        );

        // Try recursive window expansion to capture complete enum
        let window_start = enum_start;
        let mut window_size = MIN_ENUM_TOKENS;

        while window_start + window_size <= tokens.len() {
            if let Some(complete_tokens) = expand_to_enum(tokens, window_start, window_size) {
                report!(
                    "enum_handler",
                    "extract_enum",
                    Info,
                    Extract,
                    format!(
                        "Successfully expanded to complete enum with {} tokens",
                        complete_tokens.len()
                    ),
                    true
                );

                return extract_enum_from_complete_tokens(&complete_tokens);
            }

            window_size += 1;
            if window_size > 20 {
                // Prevent runaway expansion
                break;
            }
        }
    }

    report!(
        "enum_handler",
        "extract_enum",
        Info,
        Extract,
        "Patternizer patterns did not match complete enum - using fallback extraction",
        true
    );

    extract_enum_fallback(tokens)
}

/// Helper function to find enum start position within token range
fn find_enum_start_in_range(tokens: &[Token], range: Range<usize>) -> Option<usize> {
    let start = range.start.min(tokens.len());
    let end = range.end.min(tokens.len());

    for i in start..end {
        if detect_enum_start(tokens, i) {
            return Some(i);
        }
    }
    None
}

/// Helper function to detect if enum starts at given position
fn detect_enum_start(tokens: &[Token], pos: usize) -> bool {
    if pos >= tokens.len() {
        return false;
    }

    let token_str = tokens[pos].to_string();
    if token_str == "enum" {
        return true;
    }

    // Check for typedef enum pattern
    if token_str == "typedef" && pos + 1 < tokens.len() {
        return tokens[pos + 1].to_string() == "enum";
    }

    false
}

/// Helper function to expand window to capture complete enum
fn expand_to_enum(tokens: &[Token], start: usize, initial_size: usize) -> Option<Vec<Token>> {
    if start >= tokens.len() {
        return None;
    }

    let mut current_size = initial_size;
    let max_expansion = (tokens.len() - start).min(20);

    while current_size <= max_expansion {
        let end_pos = (start + current_size).min(tokens.len());
        let window = &tokens[start..end_pos];

        if is_complete_enum_declaration(window) {
            return Some(window.to_vec());
        }

        current_size += 1;
    }

    None
}

/// Helper function to check if token window contains complete enum declaration
fn is_complete_enum_declaration(tokens: &[Token]) -> bool {
    if tokens.len() < 3 {
        return false;
    }

    // Check for enum keyword
    let has_enum = tokens.iter().any(|t| t.to_string() == "enum");
    if !has_enum {
        return false;
    }

    // Check for termination patterns
    let last_token = tokens.last().unwrap().to_string();

    // Forward declaration: enum Name;
    if last_token == ";" {
        return true;
    }

    // Full definition: enum Name { ... };
    if last_token == ";" {
        let has_braces = tokens.iter().any(|t| t.to_string() == "{")
            && tokens.iter().any(|t| t.to_string() == "}");
        return has_braces;
    }

    false
}

/// Helper function to extract enum from complete token sequence
fn extract_enum_from_complete_tokens(
    tokens: &[Token],
) -> Result<Option<ExtractedElement>, C2RError> {
    let (name, values, is_typedef, typedef_name) = parse_enum_tokens(tokens, "enum")?;

    let extracted_enum = ExtractedEnum {
        name: name.clone(),
        values,
        is_typedef,
        typedef_name: typedef_name.clone(),
        is_forward_declaration: !tokens.iter().any(|t| t.to_string() == "{"),
        tokens: tokens.to_vec(),
        code: generate_rust_enum_code(&name, &typedef_name, is_typedef, tokens)?,
    };

    Ok(Some(ExtractedElement::Enum(extracted_enum)))
}

/// Helper function for fallback enum extraction
fn extract_enum_fallback(tokens: &[Token]) -> Result<Option<ExtractedElement>, C2RError> {
    // Simple fallback: look for enum keyword and basic structure
    if let Some(enum_pos) = tokens.iter().position(|t| t.to_string() == "enum") {
        let mut name = "UnknownEnum".to_string();

        // Try to extract name
        if enum_pos + 1 < tokens.len() {
            let next_token = &tokens[enum_pos + 1];
            if next_token.to_string() != "{" && next_token.to_string() != ";" {
                name = next_token.to_string();
            }
        }

        let extracted_enum = ExtractedEnum {
            name: name.clone(),
            values: Vec::new(),
            is_typedef: tokens.iter().any(|t| t.to_string() == "typedef"),
            typedef_name: None,
            is_forward_declaration: true,
            tokens: tokens.to_vec(),
            code: format!("pub enum {} {{\n    // TODO: Add variants\n}}", name),
        };

        return Ok(Some(ExtractedElement::Enum(extracted_enum)));
    }

    Ok(None)
}

/// Helper function to generate Rust enum code
fn generate_rust_enum_code(
    name: &str,
    typedef_name: &Option<String>,
    is_typedef: bool,
    tokens: &[Token],
) -> Result<String, C2RError> {
    let mut code = String::new();

    // Add derives and attributes
    code.push_str("#[derive(Debug, Clone, Copy, PartialEq, Eq)]\n");
    code.push_str("#[repr(C)]\n");

    // Use typedef name if available, otherwise use enum name
    let display_name = typedef_name.as_ref().map(|s| s.as_str()).unwrap_or(name);
    code.push_str(&format!("pub enum {} {{\n", display_name));

    // Parse enum body if present
    if let Some(start_brace) = tokens.iter().position(|t| t.to_string() == "{") {
        if let Some(end_brace_pos) = tokens[start_brace..]
            .iter()
            .position(|t| t.to_string() == "}")
        {
            let end_brace = start_brace + end_brace_pos;

            let mut i = start_brace + 1;
            while i < end_brace {
                if tokens[i].to_string() != "," {
                    let variant_name = tokens[i].to_string();

                    // Check for explicit value
                    if i + 2 < end_brace && tokens[i + 1].to_string() == "=" {
                        let value = tokens[i + 2].to_string();
                        code.push_str(&format!("    {} = {},\n", variant_name, value));
                        i += 3;
                    } else {
                        code.push_str(&format!("    {},\n", variant_name));
                        i += 1;
                    }
                } else {
                    i += 1;
                }
            }
        }
    } else {
        // Forward declaration - add placeholder
        code.push_str("    // Forward declaration - variants to be defined\n");
    }

    code.push_str("}\n");

    // Add type alias if needed
    if is_typedef && typedef_name.is_some() && typedef_name.as_ref().unwrap().as_str() != name {
        code.push_str(&format!(
            "pub type {} = {};\n",
            name,
            typedef_name.as_ref().unwrap()
        ));
    }

    Ok(code)
}

/// Helper function to parse enum tokens based on pattern type
fn parse_enum_tokens(
    tokens: &[Token],
    pattern: &str,
) -> Result<(String, Vec<(String, Option<i64>)>, bool, Option<String>), C2RError> {
    let is_typedef =
        pattern.contains("typedef") || (tokens.len() > 0 && tokens[0].to_string() == "typedef");
    let name;
    let mut values = Vec::new();
    let mut typedef_name = None;

    // Find enum keyword position
    let enum_pos = tokens
        .iter()
        .position(|t| t.to_string() == "enum")
        .unwrap_or(0);

    // Extract name (if not anonymous)
    if enum_pos + 1 < tokens.len() && tokens[enum_pos + 1].to_string() != "{" {
        name = tokens[enum_pos + 1].to_string();
    } else {
        name = "AnonymousEnum".to_string();
    }

    // Find enum body between braces
    if let Some(start_brace) = tokens.iter().position(|t| t.to_string() == "{") {
        if let Some(end_brace) = tokens[start_brace..]
            .iter()
            .position(|t| t.to_string() == "}")
        {
            let end_brace = start_brace + end_brace;

            // Parse enum values
            let mut i = start_brace + 1;
            while i < end_brace {
                if tokens[i].to_string() != "," {
                    let value_name = tokens[i].to_string();
                    let mut explicit_value = None;

                    // Check for explicit value assignment
                    if i + 2 < end_brace && tokens[i + 1].to_string() == "=" {
                        if let Ok(val) = tokens[i + 2].to_string().parse::<i64>() {
                            explicit_value = Some(val);
                        }
                        i += 3; // Skip name, =, value
                    } else {
                        i += 1; // Just skip name
                    }

                    values.push((value_name, explicit_value));
                }
                i += 1;
            }

            // Look for typedef name after closing brace
            if is_typedef && end_brace + 1 < tokens.len() {
                let mut j = end_brace + 1;
                while j < tokens.len() && tokens[j].to_string() != ";" {
                    typedef_name = Some(tokens[j].to_string());
                    j += 1;
                }
            }
        }
    }

    Ok((name, values, is_typedef, typedef_name))
}

/// Convert extracted enum to Rust code using Patternizer exclusively
fn convert_enum(token_range: Range<usize>) -> Result<Option<ConvertedElement>, C2RError> {
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

    // Try to match enum patterns
    let enum_patterns = ["enum", "typedef_enum"];

    for pattern in &enum_patterns {
        match context.patternizer.match_pattern(pattern, tokens) {
            PatternResult::Match { consumed_tokens } => {
                // Parse enum components
                let (name, values, is_typedef, typedef_name) = parse_enum_tokens(tokens, pattern)?;

                // Mark processed tokens as consumed
                for i in 0..consumed_tokens.min(processed_tokens.len()) {
                    processed_tokens[i] = Token::n();
                }

                let mut code = String::new();

                // Add derives and attributes
                code.push_str("#[derive(Debug, Clone, Copy, PartialEq, Eq)]\n");
                code.push_str("#[repr(C)]\n");

                // Use typedef name if available, otherwise use enum name
                let display_name = typedef_name.as_ref().unwrap_or(&name);
                code.push_str(&format!("pub enum {} {{\n", display_name));

                // Add enum variants
                for (value_name, explicit_value) in &values {
                    if let Some(value) = explicit_value {
                        code.push_str(&format!("    {} = {},\n", value_name, value));
                    } else {
                        code.push_str(&format!("    {},\n", value_name));
                    }
                }

                code.push_str("}\n");

                // Add type alias if needed
                if is_typedef && typedef_name.is_some() && typedef_name.as_ref().unwrap() != &name {
                    code.push_str(&format!(
                        "pub type {} = {};\n",
                        name,
                        typedef_name.as_ref().unwrap()
                    ));
                }

                let converted_enum = ConvertedEnum {
                    variants: Vec::new(),
                    code,
                    is_public: true,
                    repr: Some("C".to_string()),
                };

                return Ok(Some(ConvertedElement::Enum(converted_enum)));
            }
            _ => continue,
        }
    }

    Ok(None)
}
