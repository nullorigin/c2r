//! Macro handler for C to Rust preprocessor conversion
//! Uses Patternizer exclusively for detection and conversion

use crate::{
    C2RError, ConvertedElement, ConvertedMacro, ElementInfo, ExtractedElement, ExtractedMacro,
    HandlerPhase::{self, Extract, Handle, Process, Report},
    HandlerReport, HandlerResult, Id, Kind, MacroInfo, PatternResult, Patternizer, Reason,
    ReportLevel::{self, Info, Warning},
    Token, context, not_handled, report,
};
use std::ops::Range;

/// Report callback: Collects macro handler reports
pub fn report_macro() -> Result<HandlerReport, C2RError> {
    let context = context!();
    let reports = context.get_reports_by_handler("macro_handler");

    let (info_count, warning_count, error_count) =
        reports
            .iter()
            .fold((0, 0, 0), |acc, report| match report.level {
                ReportLevel::Error => (acc.0, acc.1, acc.2 + 1),
                ReportLevel::Warning => (acc.0, acc.1 + 1, acc.2),
                _ => (acc.0 + 1, acc.1, acc.2),
            });

    Ok(HandlerReport {
        report_id: Box::new(Id::get(&Id::gen_name("macro_handler"))),
        handler_id: Box::new(Id::get("macro_handler")),
        handler_name: "macro_handler".to_string(),
        function_name: "report_macro".to_string(),
        message: format!(
            "Macro handler summary: {} reports ({} info, {} warnings, {} errors)",
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
fn handle_macro(token_range: Range<usize>) -> Result<HandlerResult, C2RError> {
    report!(
        "macro_handler",
        "handle_macro",
        Info,
        Handle,
        "Delegating to extract for recursive window expansion",
        true
    );

    // Delegate to extract callback for recursive processing
    match extract_macro(token_range.clone())? {
        Some(ExtractedElement::Macro(extracted_macro)) => {
            let code = extracted_macro.code.clone();
            let mut context = crate::context!();
            context.pull();
            let consumed_tokens = if code.is_empty() {
                2
            } else {
                context.tokens[token_range.clone()].len().min(8)
            };

            // Convert the extracted macro to a ConvertedElement
            let definition = extracted_macro
                .body
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(" ");
            let converted_macro = ConvertedMacro {
                parameters: if extracted_macro.is_function_like {
                    extracted_macro.params.clone()
                } else {
                    Vec::new()
                },
                body: definition,
                code,
                is_function_like: extracted_macro.is_function_like,
            };

            Ok(HandlerResult::Converted(
                ConvertedElement::Macro(converted_macro),
                0..consumed_tokens,
                extracted_macro.code,
                Id::get("macro_handler"),
            ))
        }
        _ => {
            report!(
                "macro_handler",
                "handle_macro",
                Warning,
                Handle,
                "Extract did not find valid macro",
                false
            );
            not_handled()
        }
    }
}

/// Process callback: Lightweight detection using minimum tokens (2 tokens: #define, #include)
fn process_macro(token_range: Range<usize>) -> Result<bool, C2RError> {
    const MIN_MACRO_TOKENS: usize = 2; // #define, #include

    let mut context = crate::context!();
    context.pull();

    // Add bounds checking to prevent slice index out of bounds
    if token_range.end > context.tokens.len() || token_range.start >= context.tokens.len() {
        return Ok(false);
    }

    if context.tokens[token_range.clone()].len() < MIN_MACRO_TOKENS {
        return Ok(false);
    }

    report!(
        "macro_handler",
        "process_macro",
        Info,
        Process,
        format!(
            "Lightweight macro detection on {} tokens",
            context.tokens[token_range.clone()].len()
        ),
        true
    );

    // Check for specific preprocessor directives, not just any '#'
    let first_token = context.tokens[token_range.clone()][0].to_string();
    if !first_token.starts_with('#') {
        return Ok(false);
    }

    // Ensure we have a valid macro directive after the '#'
    // BUT exclude 'include' directives - let include_handler handle those
    if context.tokens[token_range.clone()].len() >= 2 {
        let second_token = context.tokens[token_range.clone()][1].to_string();
        let is_valid_directive = match second_token.as_str() {
            "define" | "ifdef" | "ifndef" | "endif" | "else" | "elif" | "undef" | "pragma"
            | "error" | "warning" | "line" => true,
            "include" => false, // Let include_handler handle this
            _ => false,
        };

        if is_valid_directive {
            report!(
                "macro_handler",
                "process_macro",
                Info,
                Process,
                format!("Found valid macro directive: #{}", second_token),
                true
            );
            return Ok(true);
        }
    }

    Ok(false)
}

/// Creates a macro handler that uses Patternizer exclusively
pub fn create_macro_handler() -> crate::handler::Handler {
    let handler_id = Id::get("macro_handler");
    let handler_role = "macro";
    let priority = 20; // High priority for macros

    super::create_handler(
        handler_id,
        handler_role,
        priority,
        Some(process_macro),
        Some(handle_macro),
        Some(extract_macro),
        Some(convert_macro),
        Some(document_macro),
        Some(report_macro),
        Some(result_macro),   // Result callback for final processing
        Some(redirect_macro), // Redirect callback for nested structures
    )
}

/// Redirect callback: Handles nested structures within macros
pub fn redirect_macro(
    token_range: Range<usize>,
    result: HandlerResult,
) -> Result<HandlerResult, C2RError> {
    report!(
        "macro_handler",
        "redirect_macro",
        Info,
        HandlerPhase::Convert,
        "Checking macro for nested structures",
        true
    );

    // Check if this macro defines or contains structs, enums, or functions
    let mut i = 0;
    let mut processed_tokens = Vec::new();
    let mut context = crate::context!();
    context.pull();

    // Add bounds checking to prevent slice index out of bounds
    if token_range.end > context.tokens.len() || token_range.start >= context.tokens.len() {
        return Ok(HandlerResult::NotHandled(
            Some(processed_tokens),
            token_range,
            Id::get("macro_bounds_check"),
        ));
    }

    let tokens = &context.tokens[token_range.clone()];

    while i < tokens.len() {
        let token = &tokens[i];

        // Check for nested struct definitions within macro
        if token.to_string() == "struct" && i + 1 < tokens.len() {
            // Find the end of the struct definition
            let mut brace_depth = 0;
            let mut struct_end = i;

            for j in (i + 1)..tokens.len() {
                match tokens[j].to_string().as_str() {
                    "{" => brace_depth += 1,
                    "}" => {
                        brace_depth -= 1;
                        if brace_depth == 0 {
                            struct_end = j;
                            break;
                        }
                    }
                    _ => {}
                }
            }

            if struct_end > i {
                report!(
                    "macro_handler",
                    "redirect_macro",
                    Info,
                    HandlerPhase::Convert,
                    "Found nested struct in macro, marking tokens as processed",
                    true
                );

                // Mark the struct tokens as processed
                for k in i..=struct_end {
                    processed_tokens.push(Token::n());
                    i = k + 1;
                }
                continue;
            }
        }

        // Check for nested enum definitions
        if token.to_string() == "enum" && i + 1 < tokens.len() {
            let mut brace_depth = 0;
            let mut enum_end = i;

            for j in (i + 1)..tokens.len() {
                match tokens[j].to_string().as_str() {
                    "{" => brace_depth += 1,
                    "}" => {
                        brace_depth -= 1;
                        if brace_depth == 0 {
                            enum_end = j;
                            break;
                        }
                    }
                    _ => {}
                }
            }

            if enum_end > i {
                report!(
                    "macro_handler",
                    "redirect_macro",
                    Info,
                    HandlerPhase::Convert,
                    "Found nested enum in macro, marking tokens as processed",
                    true
                );

                // Mark enum tokens as processed
                for _k in i..=enum_end {
                    processed_tokens.push(Token::n());
                }
                i = enum_end + 1;
                continue;
            }
        }

        processed_tokens.push(token.clone());
        i += 1;
    }

    Ok(result)
}

/// Document callback: Generates documentation for macros
fn document_macro(info: ElementInfo) -> Result<Option<String>, C2RError> {
    if let ElementInfo::Macro(macro_info) = info {
        let mut doc = format!(
            "/// Converted C {}: {}\n",
            macro_info.kind_description, macro_info.name
        );

        // For complex macros, use Patternizer to determine purpose
        if macro_info.complexity == "complex" {
            let _patternizer = Patternizer::with_common_patterns();

            // Determine macro purpose based on patterns
            let purpose = if macro_info.directive == "define" && macro_info.is_function_like {
                "function-like macro for code generation"
            } else if macro_info.directive == "define" && !macro_info.params.is_empty() {
                "parameterized constant definition"
            } else if macro_info.directive == "ifdef" || macro_info.directive == "ifndef" {
                "conditional compilation directive"
            } else if macro_info.directive == "pragma" {
                "compiler-specific directive"
            } else {
                "preprocessor directive"
            };

            doc.push_str(&format!("/// Purpose: {}\n", purpose));
        }

        doc.push_str(&format!("/// Kind: {}\n", macro_info.kind_description));
        doc.push_str(&format!("/// Directive: {}\n", macro_info.directive));

        if macro_info.is_function_like {
            doc.push_str(&format!(
                "/// Parameters: {}\n",
                macro_info.params.join(", ")
            ));
        }

        if macro_info.is_conditional {
            doc.push_str("/// Note: Conditional compilation - may affect build\n");
        }

        Ok(Some(doc))
    } else {
        Ok(None)
    }
}

/// Result callback: Postprocesses macro conversion with documentation
#[allow(unused_variables)]
fn result_macro(
    token_range: Range<usize>,
    result: HandlerResult,
) -> Result<HandlerResult, C2RError> {
    let _id = Id::get("result_macro");

    report!(
        "macro_handler",
        "result_macro",
        Info,
        Report,
        "Postprocessing macro conversion result",
        true
    );

    match result {
        HandlerResult::Converted(element, token_range, code, id) => {
            // Extract macro information from tokens for documentation
            let mut context = crate::context!();
            context.pull();
            let macro_info = extract_macro_info(&context.tokens[token_range.clone()]);

            // Generate documentation about the macro conversion
            let doc_comment =
                document_macro(ElementInfo::Macro(macro_info.clone()))?.unwrap_or_default();

            // Enhance the Rust code with documentation and metadata
            let mut enhanced_code = String::new();

            // Add metadata comment for traceability
            let metadata_comment = format!(
                "// [C2R] {} converted from C to Rust - {}: {}\n",
                macro_info.kind_description, macro_info.name, macro_info.directive
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
                "macro_handler",
                "result_macro",
                Info,
                Report,
                format!(
                    "Enhanced macro '{}' with {} lines of documentation",
                    macro_info.name,
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
                "macro_handler",
                "result_macro",
                Warning,
                Report,
                "Macro result was not in Completed state, returning as-is",
                true
            );

            Ok(result)
        }
    }
}

/// Extract macro information from tokens for result processing
fn extract_macro_info(tokens: &[Token]) -> MacroInfo {
    use crate::document::MacroKind;

    let mut name = String::new();
    let mut directive = String::new();
    let params = Vec::new();
    let mut is_function_like = false;
    let mut is_conditional = false;

    // Basic parsing to extract macro information
    let mut i = 0;
    while i < tokens.len() {
        match &tokens[i] {
            Token::s(text) if text.starts_with("#") => {
                if text.starts_with("#define") {
                    directive = "define".to_string();
                    if i + 1 < tokens.len() {
                        if let Token::s(id) = &tokens[i + 1] {
                            name = id.clone();
                        }
                    }
                } else if text.starts_with("#ifdef") {
                    directive = "ifdef".to_string();
                    is_conditional = true;
                } else if text.starts_with("#ifndef") {
                    directive = "ifndef".to_string();
                    is_conditional = true;
                } else if text.starts_with("#pragma") {
                    directive = "pragma".to_string();
                }
            }
            Token::l("(") => {
                if !name.is_empty() && i > 0 {
                    is_function_like = true;
                }
            }
            _ => {}
        }
        i += 1;
    }

    let kind = if is_conditional {
        MacroKind::Conditional
    } else if directive == "pragma" {
        MacroKind::Pragma
    } else if is_function_like {
        MacroKind::FunctionLike
    } else {
        MacroKind::ObjectLike
    };

    let kind_description = match kind {
        MacroKind::ObjectLike => "object-like macro".to_string(),
        MacroKind::FunctionLike => "function-like macro".to_string(),
        MacroKind::Conditional => "conditional macro".to_string(),
        MacroKind::Pragma => "pragma directive".to_string(),
        _ => "macro".to_string(),
    };

    MacroInfo {
        name,
        kind,
        kind_description,
        directive,
        params,
        is_function_like,
        is_conditional,
        complexity: if is_function_like || is_conditional {
            "complex".to_string()
        } else {
            "simple".to_string()
        },
    }
}

/// Extract macro with recursive window expansion starting from minimum tokens
fn extract_macro(token_range: Range<usize>) -> Result<Option<ExtractedElement>, C2RError> {
    const MIN_MACRO_TOKENS: usize = 2; // #define
    const MAX_MACRO_WINDOW: usize = 15; // Maximum expansion for macro definitions

    let mut context = crate::context!();
    context.pull();
    if context.tokens[token_range.clone()].len() < MIN_MACRO_TOKENS {
        return Ok(None);
    }

    report!(
        "macro_handler",
        "extract_macro",
        Info,
        Extract,
        format!(
            "Starting recursive extraction with {} tokens",
            context.tokens[token_range.clone()].len()
        ),
        true
    );

    // Find macro start within the token range
    if let Some(macro_start) =
        find_macro_start_in_range(&context.tokens[token_range.clone()], token_range.clone())
    {
        report!(
            "macro_handler",
            "extract_macro",
            Info,
            Extract,
            format!("Found macro start at position {}", macro_start),
            true
        );

        // Use recursive window expansion to capture complete macro
        if let Some(complete_tokens) = expand_to_macro(
            &context.tokens[token_range.clone()],
            macro_start,
            MAX_MACRO_WINDOW,
        ) {
            return extract_macro_from_complete_tokens(&complete_tokens);
        }
    }

    // Fallback extraction
    extract_macro_fallback(&context.tokens[token_range.clone()])
}

/// Find macro start position within a token range
fn find_macro_start_in_range(tokens: &[Token], range: Range<usize>) -> Option<usize> {
    let end = range.end.min(tokens.len());
    for i in range.start..end {
        if detect_macro_start(tokens, i) {
            return Some(i);
        }
    }
    None
}

/// Detect if a macro starts at the given position
fn detect_macro_start(tokens: &[Token], pos: usize) -> bool {
    if pos >= tokens.len() {
        return false;
    }

    let token_str = tokens[pos].to_string();
    token_str.starts_with('#')
}

/// Recursively expand token window to capture complete macro definition
fn expand_to_macro(tokens: &[Token], macro_start: usize, max_window: usize) -> Option<Vec<Token>> {
    let mut window_size = 2; // Start with minimum tokens

    while window_size <= max_window {
        let window_end = (macro_start + window_size).min(tokens.len());
        let current_window = &tokens[macro_start..window_end];

        if is_complete_macro_definition(current_window) {
            report!(
                "macro_handler",
                "expand_to_macro",
                Info,
                Extract,
                format!("Found complete macro with {} tokens", window_size),
                true
            );
            return Some(current_window.to_vec());
        }

        window_size += 1;
    }

    // Return the largest window if no complete definition found
    let final_end = (macro_start + max_window).min(tokens.len());
    Some(tokens[macro_start..final_end].to_vec())
}

/// Check if token window represents a complete macro definition
fn is_complete_macro_definition(tokens: &[Token]) -> bool {
    if tokens.len() < 2 {
        return false;
    }

    let first_token = tokens[0].to_string();
    if !first_token.starts_with('#') {
        return false;
    }

    if tokens.len() >= 2 {
        let directive = tokens[1].to_string();
        match directive.as_str() {
            "define" => {
                // For #define, we need at least the directive, name, and potentially definition
                tokens.len() >= 3
            }
            "include" | "ifdef" | "ifndef" | "endif" | "undef" => {
                // These directives typically need just directive + argument
                tokens.len() >= 2
            }
            "pragma" => {
                // #pragma directives can be complex but often complete with few tokens
                tokens.len() >= 3
            }
            _ => true, // Other directives considered complete as-is
        }
    } else {
        false
    }
}

/// Extract macro details from complete token window
fn extract_macro_from_complete_tokens(
    tokens: &[Token],
) -> Result<Option<ExtractedElement>, C2RError> {
    if tokens.len() < 2 {
        return Ok(None);
    }

    let directive = tokens[1].to_string();
    let name = if tokens.len() > 2 {
        tokens[2].to_string()
    } else {
        directive.clone()
    };
    let mut params = Vec::new();
    let mut is_function_like = false;

    // Check for function-like macro
    if tokens.len() > 3 && tokens[3].to_string() == "(" {
        is_function_like = true;
        // Extract parameters between parentheses
        let mut paren_depth = 0;
        let mut current_param = String::new();

        for i in 4..tokens.len() {
            let token_str = tokens[i].to_string();
            match token_str.as_str() {
                "(" => paren_depth += 1,
                ")" => {
                    paren_depth -= 1;
                    if paren_depth < 0 {
                        if !current_param.trim().is_empty() {
                            params.push(current_param.trim().to_string());
                        }
                        break;
                    }
                }
                "," if paren_depth == 0 => {
                    if !current_param.trim().is_empty() {
                        params.push(current_param.trim().to_string());
                        current_param.clear();
                    }
                }
                _ => {
                    if paren_depth >= 0 {
                        if !current_param.is_empty() {
                            current_param.push(' ');
                        }
                        current_param.push_str(&token_str);
                    }
                }
            }
        }
    }

    let body_tokens = if is_function_like {
        // Find tokens after the parameter list
        let mut found_closing = false;
        let mut body_start = 0;
        for i in 3..tokens.len() {
            if tokens[i].to_string() == ")" {
                body_start = i + 1;
                found_closing = true;
                break;
            }
        }
        if found_closing && body_start < tokens.len() {
            tokens[body_start..].to_vec()
        } else {
            Vec::new()
        }
    } else {
        // For object-like macros, body starts after the name
        if tokens.len() > 3 {
            tokens[3..].to_vec()
        } else {
            Vec::new()
        }
    };

    let definition = body_tokens
        .iter()
        .map(|t| t.to_string())
        .collect::<Vec<_>>()
        .join(" ");
    let code = generate_rust_macro_code(&name, &params, &definition, is_function_like);

    let extracted_macro = ExtractedMacro {
        name,
        params,
        body: body_tokens,
        tokens: tokens.to_vec(),
        is_function_like,
        code,
    };

    Ok(Some(ExtractedElement::Macro(extracted_macro)))
}

/// Fallback extraction when pattern matching fails
fn extract_macro_fallback(tokens: &[Token]) -> Result<Option<ExtractedElement>, C2RError> {
    if tokens.len() >= 2 && tokens[0].to_string().starts_with('#') {
        let directive = tokens[1].to_string();

        // Find the end of this macro directive by looking for line boundaries or next macro
        let mut macro_end = 2; // Start after # and directive

        // For different directive types, find appropriate ending
        match directive.as_str() {
            "include" => {
                // For #include, find closing > or " or end of line
                for i in 2..tokens.len() {
                    let token_str = tokens[i].to_string();
                    if token_str.contains('>')
                        || token_str.contains('"')
                        || token_str.contains('\n')
                    {
                        macro_end = i + 1;
                        break;
                    }
                    // Stop if we hit another preprocessor directive
                    if token_str.starts_with('#') {
                        macro_end = i;
                        break;
                    }
                    // Limit to reasonable length for includes
                    if i - 2 > 3 {
                        macro_end = i;
                        break;
                    }
                }
            }
            "define" => {
                // For #define, find end of line or reasonable limit
                for i in 2..tokens.len() {
                    let token_str = tokens[i].to_string();
                    if token_str.contains('\n') || token_str.starts_with('#') {
                        macro_end = i;
                        break;
                    }
                    // Limit define to reasonable length
                    if i - 2 > 10 {
                        macro_end = i;
                        break;
                    }
                }
            }
            _ => {
                // For other directives, limit to very short definitions
                macro_end = std::cmp::min(tokens.len(), 5);
            }
        }

        let name = if tokens.len() > 2 {
            tokens[2].to_string()
        } else {
            directive.clone()
        };
        let body_tokens = if macro_end > 3 {
            tokens[3..macro_end].to_vec()
        } else {
            Vec::new()
        };
        let definition = body_tokens
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<_>>()
            .join(" ");

        let code = generate_rust_macro_code(&name, &Vec::new(), &definition, false);

        let extracted_macro = ExtractedMacro {
            name,
            params: Vec::new(),
            body: body_tokens,
            tokens: tokens[0..macro_end].to_vec(), // Only consume tokens for this macro
            is_function_like: false,
            code,
        };

        return Ok(Some(ExtractedElement::Macro(extracted_macro)));
    }

    Ok(None)
}

/// Generate Rust macro code from extracted components
fn generate_rust_macro_code(
    name: &str,
    params: &[String],
    definition: &str,
    is_function_like: bool,
) -> String {
    if is_function_like {
        let param_list = params.join(", ");
        format!(
            "macro_rules! {} {{
    ({}) => {{
        {}
    }};
}}",
            name, param_list, definition
        )
    } else {
        if definition.is_empty() {
            format!(
                "const {}: () = (); // Empty macro definition",
                name.to_uppercase()
            )
        } else {
            format!(
                "const {}: &str = \"{}\"; // Macro definition",
                name.to_uppercase(),
                definition
            )
        }
    }
}

/// Helper function to parse macro tokens based on pattern type  
fn parse_macro_tokens(
    tokens: &[Token],
    _pattern: &str,
) -> Result<(String, Vec<String>, Vec<Token>, bool, String), C2RError> {
    if tokens.len() < 2 {
        return Err(C2RError::new(
            Kind::Other,
            Reason::IsNot("enough tokens for macro"),
            None,
        ));
    }

    let directive = tokens[1].to_string();
    let name = if tokens.len() > 2 {
        tokens[2].to_string()
    } else {
        directive.clone()
    };
    let mut params = Vec::new();
    let mut body = Vec::new();
    let mut is_function_like = false;

    match directive.as_str() {
        "define" => {
            if tokens.len() > 3 && tokens[3].to_string() == "(" {
                // Function-like macro
                is_function_like = true;

                // Find closing parenthesis and extract parameters
                let mut paren_depth = 0;
                let mut param_start = 4;
                let mut body_start = 4;

                for (i, token) in tokens.iter().enumerate().skip(3) {
                    match token.to_string().as_str() {
                        "(" => {
                            paren_depth += 1;
                            if paren_depth == 1 && i > 3 {
                                param_start = i + 1;
                            }
                        }
                        ")" => {
                            paren_depth -= 1;
                            if paren_depth == 0 {
                                // Extract parameters
                                if i > param_start {
                                    let param_tokens = &tokens[param_start..i];
                                    let mut current_param = String::new();

                                    for param_token in param_tokens {
                                        if param_token.to_string() == "," {
                                            if !current_param.trim().is_empty() {
                                                params.push(current_param.trim().to_string());
                                                current_param.clear();
                                            }
                                        } else {
                                            current_param.push_str(&param_token.to_string());
                                        }
                                    }

                                    if !current_param.trim().is_empty() {
                                        params.push(current_param.trim().to_string());
                                    }
                                }

                                body_start = i + 1;
                                break;
                            }
                        }
                        _ => {}
                    }
                }

                // Extract body
                if body_start < tokens.len() {
                    body = tokens[body_start..].to_vec();
                }
            } else {
                // Object-like macro
                if tokens.len() > 3 {
                    body = tokens[3..].to_vec();
                }
            }
        }
        _ => {
            // Other preprocessor directives
            if tokens.len() > 2 {
                body = tokens[2..].to_vec();
            }
        }
    }

    Ok((name, params, body, is_function_like, directive))
}

/// Convert extracted macro to Rust code using Patternizer exclusively
fn convert_macro(token_range: Range<usize>) -> Result<Option<ConvertedElement>, C2RError> {
    let mut context = crate::context!();
    context.pull();
    let mut processed_tokens = context.tokens[token_range.clone()].to_vec();

    // Try to match macro patterns
    let macro_patterns = [
        "macro_define",
        "macro_ifdef",
        "macro_ifndef",
        "macro_if",
        "macro_else",
        "macro_elif",
        "macro_endif",
        "macro_undef",
        "macro_pragma",
        "macro_error",
        "macro_warning",
    ];

    for pattern in &macro_patterns {
        match context
            .patternizer
            .match_pattern(pattern, &processed_tokens[token_range.clone()])
        {
            PatternResult::Match { consumed_tokens } => {
                // Extract macro components
                let (name, params, body, is_function_like, macro_type) =
                    parse_macro_tokens(&context.tokens[token_range.clone()], pattern)?;

                let code =
                    convert_macro_to_rust(&name, &params, &body, is_function_like, &macro_type);

                // Mark processed tokens as consumed
                for i in 0..consumed_tokens.min(processed_tokens.len()) {
                    processed_tokens[i] = Token::n();
                }

                let converted_macro = ConvertedMacro {
                    parameters: params,
                    body: body
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<_>>()
                        .join(" "),
                    code,
                    is_function_like,
                };

                return Ok(Some(ConvertedElement::Macro(converted_macro)));
            }
            _ => continue,
        }
    }

    // Fallback conversion for basic preprocessor directives
    if context.tokens[token_range.clone()].len() >= 2
        && context.tokens[token_range.clone()][0].to_string() == "#"
    {
        let directive = context.tokens[token_range.clone()][1].to_string();
        let name = if context.tokens[token_range.clone()].len() > 2 {
            context.tokens[token_range.clone()][2].to_string()
        } else {
            directive.clone()
        };
        let body: Vec<Token> = context.tokens[token_range.clone()][2..].to_vec();

        let code = convert_macro_to_rust(&name, &Vec::new(), &body, false, &directive);

        let converted_macro = ConvertedMacro {
            parameters: Vec::new(),
            body: body
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(" "),
            code,
            is_function_like: false,
        };

        return Ok(Some(ConvertedElement::Macro(converted_macro)));
    }

    Ok(None)
}

/// Convert macro to appropriate Rust code
fn convert_macro_to_rust(
    name: &str,
    params: &[String],
    body: &[Token],
    is_function_like: bool,
    macro_type: &str,
) -> String {
    let body_str = body
        .iter()
        .map(|t| t.to_string())
        .collect::<Vec<_>>()
        .join(" ");

    match macro_type {
        "define" => {
            if is_function_like {
                // Function-like macro -> Rust macro or inline function
                let param_list = params.join(", ");
                format!(
                    "macro_rules! {} {{\n    ({}) => {{\n        {}\n    }};\n}}",
                    name, param_list, body_str
                )
            } else {
                // Object-like macro -> const
                convert_object_macro_to_rust(name, &body_str)
            }
        }
        "ifdef" => format!("#[cfg(feature = \"{}\")]", name),
        "ifndef" => format!("#[cfg(not(feature = \"{}\"))]", name),
        "if" => format!("#[cfg({})]", convert_condition_to_cfg_expr(&body_str)),
        "else" => "#[cfg(not(any()))]".to_string(),
        "elif" => format!("#[cfg({})]", convert_condition_to_cfg_expr(&body_str)),
        "endif" => String::new(), // No direct equivalent
        "undef" => format!("// #undef {} (no direct Rust equivalent)", name),
        "pragma" => convert_pragma_to_rust(&body_str),
        "error" => format!("compile_error!(\"{}\");", body_str.replace("\"", "\\\"")),
        "warning" => format!("// #warning {} (converted to comment)", body_str),
        _ => format!("// #{} {} (converted to comment)", macro_type, body_str),
    }
}

/// Convert object-like macro to Rust const
fn convert_object_macro_to_rust(name: &str, value: &str) -> String {
    match name {
        "NULL" => "pub const NULL: *const std::ffi::c_void = std::ptr::null();".to_string(),
        "TRUE" | "true" => "pub const TRUE: bool = true;".to_string(),
        "FALSE" | "false" => "pub const FALSE: bool = false;".to_string(),
        _ => {
            if value.is_empty() {
                format!("pub const {}: () = ();", name)
            } else if value.starts_with("\"") && value.ends_with("\"") {
                format!("pub const {}: &str = {};", name, value)
            } else if value.parse::<i64>().is_ok() {
                format!("pub const {}: i32 = {};", name, value)
            } else if value.parse::<f64>().is_ok() {
                format!("pub const {}: f64 = {};", name, value)
            } else {
                format!(
                    "pub const {}: &str = \"{}\";",
                    name,
                    value.replace("\"", "\\\"")
                )
            }
        }
    }
}

/// Convert pragma to Rust equivalent
fn convert_pragma_to_rust(pragma_text: &str) -> String {
    match pragma_text.split_whitespace().next() {
        Some("once") => "#[allow(dead_code)]".to_string(),
        Some("pack") => "#[repr(packed)]".to_string(),
        _ => format!("// #pragma {} (no direct Rust equivalent)", pragma_text),
    }
}

/// Convert C preprocessor condition to Rust cfg expression
fn convert_condition_to_cfg_expr(condition: &str) -> String {
    // Simple conversion - just use the condition as-is for now
    // In a more sophisticated implementation, this would parse and convert C expressions
    condition
        .replace("defined(", "feature = \"")
        .replace(")", "\"")
}
