//! Typedef handler for C to Rust type alias conversion
//! Uses Patternizer exclusively for detection and conversion

use crate::{
    C2RError, ConvertedElement, ConvertedTypedef, ElementInfo, ExtractedElement, ExtractedTypedef,
    HandlerPhase::{self, Extract, Handle, Process, Report},
    HandlerReport, HandlerResult, Id, Kind, PatternResult, Patternizer, Reason,
    ReportLevel::{self, Error, Info, Warning},
    Result, Token, TypedefInfo, context,
    convert::convert_type,
    not_handled, report, tokens,
};
use std::ops::Range;

/// Report callback: Collects typedef handler reports
pub fn report_typedef() -> Result<HandlerReport> {
    let context = context!();
    let reports = context.get_reports_by_handler("typedef_handler");

    let (info_count, warning_count, error_count) =
        reports
            .iter()
            .fold((0, 0, 0), |acc, report| match report.level {
                ReportLevel::Error => (acc.0, acc.1, acc.2 + 1),
                ReportLevel::Warning => (acc.0, acc.1 + 1, acc.2),
                _ => (acc.0 + 1, acc.1, acc.2),
            });

    Ok(HandlerReport {
        report_id: Box::new(Id::get(&Id::gen_name("typedef_handler"))),
        handler_id: Box::new(Id::get("typedef_handler")),
        handler_name: "typedef_handler".to_string(),
        function_name: "report_typedef".to_string(),
        message: format!(
            "Typedef handler summary: {} reports ({} info, {} warnings, {} errors)",
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

/// Find typedef start position within the given range
fn find_typedef_start_in_range(tokens: &[Token], range: &Range<usize>) -> Result<Option<usize>> {
    let range_tokens = &tokens[range.clone()];

    for (i, window) in range_tokens.windows(1).enumerate() {
        if detect_typedef_start(window) {
            return Ok(Some(range.start + i));
        }
    }

    Ok(None)
}

/// Detect if token window indicates typedef start using Patternizer
fn detect_typedef_start(tokens: &[Token]) -> bool {
    if tokens.is_empty() {
        return false;
    }

    let mut context = crate::context!();
    let typedef_enum_result = context.patternizer.match_pattern("typedef_enum", tokens);
    let typedef_struct_result = context.patternizer.match_pattern("typedef_struct", tokens);
    let typedef_simple_result = context.patternizer.match_pattern("typedef_simple", tokens);

    matches!(typedef_enum_result, PatternResult::Match { .. })
        || matches!(typedef_struct_result, PatternResult::Match { .. })
        || matches!(typedef_simple_result, PatternResult::Match { .. })
}

/// Recursively expand window to capture complete typedef starting with minimum viable tokens
fn expand_to_typedef(tokens: &[Token], start_pos: usize) -> Result<Option<Range<usize>>> {
    if start_pos >= tokens.len() {
        return Ok(None);
    }

    // Validate we have typedef at the start
    if tokens[start_pos].to_string() != "typedef" {
        return Ok(None);
    }

    // Constants for minimum viable typedef tokens
    const MIN_TYPEDEF_TOKENS: usize = 4; // typedef int MyInt ;

    // Step 1: Start with minimum typedef window (4 tokens)
    let mut current_end = (start_pos + MIN_TYPEDEF_TOKENS).min(tokens.len());

    // Step 2: Check if we have complete typedef in minimum window
    if current_end <= tokens.len() {
        let window_tokens = &tokens[start_pos..current_end];
        if is_complete_typedef_declaration(window_tokens) {
            return Ok(Some(start_pos..current_end));
        }
    }

    // Step 3: Progressive expansion if minimum window is incomplete
    for expand_size in 1..=20 {
        current_end = (start_pos + MIN_TYPEDEF_TOKENS + expand_size).min(tokens.len());
        if current_end >= tokens.len() {
            break;
        }

        let window_tokens = &tokens[start_pos..current_end];
        if is_complete_typedef_declaration(window_tokens) {
            return Ok(Some(start_pos..current_end));
        }
    }

    // Fallback: use minimum viable window
    let final_end = (start_pos + MIN_TYPEDEF_TOKENS).min(tokens.len());
    Ok(Some(start_pos..final_end))
}

/// Check if token sequence forms a complete typedef declaration
fn is_complete_typedef_declaration(tokens: &[Token]) -> bool {
    if tokens.len() < 4 {
        return false;
    }

    // Must start with typedef
    if tokens[0].to_string() != "typedef" {
        return false;
    }

    // Look for semicolon to end the typedef
    for (i, token) in tokens.iter().enumerate() {
        if token.to_string() == ";" {
            // Simple typedef: typedef int MyInt;
            if i >= 3 {
                return true;
            }
        }
        // Handle complex typedefs with function pointers, arrays, etc.
        if i > 10 {
            // For very complex typedefs, assume complete if reasonably long
            if token.to_string() == ";" {
                return true;
            }
        }
    }

    false
}

/// Fallback extraction when patterns don't match
fn extract_typedef_fallback(tokens: &[Token]) -> Result<Option<ExtractedElement>> {
    if tokens.len() < 4 {
        return Ok(None);
    }

    // Must start with typedef
    if tokens[0].to_string() != "typedef" {
        return Ok(None);
    }

    // Skip typedef struct/enum/union as they're handled by specialized handlers
    if tokens.len() >= 2 {
        let second_token = tokens[1].to_string();
        if second_token == "struct" || second_token == "enum" || second_token == "union" {
            return Ok(None);
        }
    }

    let (name, source_tokens) = parse_typedef_tokens(tokens)?;

    let extracted_typedef = ExtractedTypedef {
        name,
        code: tokens
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<_>>()
            .join(" "),
        tokens: source_tokens,
    };

    Ok(Some(ExtractedElement::Typedef(extracted_typedef)))
}

/// Handle callback: Sets up patternizer and registers typedef patterns
fn handle_typedef(token_range: Range<usize>) -> Result<HandlerResult> {
    // Register typedef patterns in the global registry via context
    let typedef_patterns = [
        "typedef_struct",
        "typedef_enum",
        "typedef_function",
        "typedef",
    ];

    for pattern in &typedef_patterns {
        let mut context = crate::context!();
        context.pull();
        // Add bounds checking to prevent slice index out of bounds
        if token_range.end > context.tokens.len() || token_range.start >= context.tokens.len() {
            continue;
        }

        let tokens = &context.tokens[token_range.clone()];
        match context.patternizer.match_pattern(pattern, tokens) {
            PatternResult::Match { consumed_tokens } => {
                report!(
                    "typedef_handler",
                    "handle_typedef",
                    Info,
                    Handle,
                    format!(
                        "Processing {} typedef with {} tokens",
                        pattern, consumed_tokens
                    ),
                    true
                );

                // Convert using pattern data
                match convert_typedef(token_range.clone())? {
                    Some(ConvertedElement::Typedef(converted_typedef)) => {
                        let code = converted_typedef.code.clone();
                        return Ok(HandlerResult::Converted(
                            ConvertedElement::Typedef(converted_typedef),
                            0..consumed_tokens,
                            code,
                            Id::get("typedef_handler"),
                        ));
                    }
                    _ => {
                        report!(
                            "typedef_handler",
                            "handle_typedef",
                            Error,
                            Handle,
                            "Failed to convert typedef",
                            false
                        );
                        return not_handled();
                    }
                }
            }
            _ => continue,
        }
    }

    not_handled()
}

/// Process callback: Lightweight typedef detection using minimum tokens
fn process_typedef(token_range: Range<usize>) -> Result<bool> {
    let mut context = crate::context!();
    context.pull();

    // Add bounds checking to prevent slice index out of bounds
    if token_range.end > context.tokens.len() || token_range.start >= context.tokens.len() {
        return Ok(false);
    }

    if context.tokens[token_range.clone()].len() < 1 {
        return Ok(false);
    }

    let first_token = context.tokens[token_range.clone()][0].to_string();
    let is_typedef = first_token == "typedef";

    if is_typedef {
        report!(
            "typedef_handler",
            "process_typedef",
            Info,
            Process,
            "Detected potential typedef construct",
            true
        );
    }

    Ok(is_typedef)
}

/// Creates a typedef handler that uses Patternizer exclusively
pub fn create_typedef_handler() -> crate::handler::Handler {
    let handler_id = Id::get("typedef_handler");
    let handler_role = "typedef";
    let priority = 100;

    super::create_handler(
        handler_id,
        handler_role,
        priority,
        Some(process_typedef),
        Some(handle_typedef),
        Some(extract_typedef),
        Some(convert_typedef),
        Some(document_typedef),
        Some(report_typedef),
        Some(result_typedef),   // Result callback for final processing
        Some(redirect_typedef), // Redirect callback for nested structures
    )
}

/// Result callback: Postprocesses typedef conversion with documentation
fn result_typedef(_token_range: Range<usize>, result: HandlerResult) -> Result<HandlerResult> {
    let _id = Id::get("result_typedef");

    report!(
        "typedef_handler",
        "result_typedef",
        Info,
        Report,
        "Postprocessing typedef conversion result",
        true
    );

    match result {
        HandlerResult::Converted(element, token_range, code, id) => {
            // Extract typedef information from tokens for documentation
            let context = crate::context!();
            let typedef_info = extract_typedef_info(&context.tokens[token_range.clone()]);

            // Generate documentation about the typedef conversion
            let doc_comment =
                document_typedef(ElementInfo::Typedef(typedef_info.clone()))?.unwrap_or_default();

            // Enhance the Rust code with documentation and metadata
            let mut enhanced_code = String::new();

            // Add metadata comment for traceability
            let metadata_comment = format!(
                "// [C2R] Typedef converted from C to Rust - {}: {}\n",
                typedef_info.name, typedef_info.kind_description
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
                "typedef_handler",
                "result_typedef",
                Info,
                Report,
                format!(
                    "Enhanced typedef '{}' with {} lines of documentation",
                    typedef_info.name,
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
                "typedef_handler",
                "result_typedef",
                Warning,
                Report,
                "Typedef result was not in Completed state, returning as-is",
                true
            );

            Ok(result)
        }
    }
}

/// Extract typedef information from tokens for result processing
fn extract_typedef_info(tokens: &[Token]) -> TypedefInfo {
    use crate::document::TypedefKind;

    let mut name = String::new();
    let mut source_type = String::new();
    let mut is_function_pointer = false;
    let mut is_array = false;

    // Basic parsing to extract typedef information
    let mut found_typedef = false;
    for (i, token) in tokens.iter().enumerate() {
        match token {
            Token::s(keyword) if keyword == "typedef" => found_typedef = true,
            Token::s(keyword)
                if found_typedef
                    && (keyword == "int"
                        || keyword == "char"
                        || keyword == "float"
                        || keyword == "struct"
                        || keyword == "enum") =>
            {
                source_type = keyword.clone();
            }
            Token::s(id) if found_typedef && source_type.is_empty() => {
                source_type = id.clone();
            }
            Token::l("(") if found_typedef => {
                // Check if this is a function pointer by looking for * before (
                if i > 0 {
                    if let Token::l("*") = &tokens[i - 1] {
                        is_function_pointer = true;
                    }
                }
            }
            Token::l("[") if found_typedef => is_array = true,
            Token::s(id) if found_typedef && !source_type.is_empty() && name.is_empty() => {
                // This should be the typedef name (last identifier before semicolon)
                name = id.clone();
            }
            _ => {}
        }
    }

    // Handle case where name might be the last identifier
    if name.is_empty() && found_typedef {
        for token in tokens.iter().rev() {
            if let Token::s(id) = token {
                name = id.clone();
                break;
            }
        }
    }

    if name.is_empty() {
        name = "unnamed_typedef".to_string();
    }
    if source_type.is_empty() {
        source_type = "unknown".to_string();
    }

    let kind = if is_function_pointer {
        TypedefKind::Function
    } else if is_array {
        TypedefKind::Array
    } else if source_type == "struct" {
        TypedefKind::Struct
    } else if source_type == "enum" {
        TypedefKind::Enum
    } else if source_type.contains("*") {
        TypedefKind::Pointer
    } else {
        TypedefKind::Simple
    };

    let kind_description = match kind {
        TypedefKind::Simple => "simple type alias".to_string(),
        TypedefKind::Pointer => "pointer type alias".to_string(),
        TypedefKind::Array => "array type alias".to_string(),
        TypedefKind::Function => "function pointer type".to_string(),
        TypedefKind::Struct => "struct type alias".to_string(),
        TypedefKind::Union => "union type alias".to_string(),
        TypedefKind::Enum => "enum type alias".to_string(),
        _ => "type alias".to_string(),
    };

    TypedefInfo {
        name,
        kind,
        kind_description,
        source_type: source_type.clone(),
        is_function_pointer,
        is_array,
        complexity: if is_function_pointer
            || source_type.contains("struct")
            || source_type.contains("enum")
        {
            "complex".to_string()
        } else {
            "simple".to_string()
        },
    }
}

/// Document callback: Generates documentation for typedefs
fn document_typedef(info: ElementInfo) -> Result<Option<String>> {
    if let ElementInfo::Typedef(typedef_info) = info {
        let mut doc = format!("/// Converted C typedef: {}\n", typedef_info.name);

        // For complex typedefs, use Patternizer to determine purpose
        if typedef_info.complexity == "complex" {
            let _patternizer = Patternizer::with_common_patterns();

            // Determine typedef purpose based on patterns
            let purpose = if typedef_info.is_function_pointer {
                "function pointer type for callback mechanisms"
            } else if typedef_info.is_array {
                "array type alias for consistent data structures"
            } else {
                "type alias for code clarity and maintainability"
            };

            doc.push_str(&format!("/// Purpose: {}\n", purpose));
        }

        doc.push_str(&format!("/// Kind: {}\n", typedef_info.kind_description));
        doc.push_str(&format!("/// Source type: {}\n", typedef_info.source_type));

        if typedef_info.is_function_pointer {
            doc.push_str("/// Type: Function pointer\n");
        }

        if typedef_info.is_array {
            doc.push_str("/// Type: Array alias\n");
        }

        Ok(Some(doc))
    } else {
        Ok(None)
    }
}

/// Redirect callback: Handles nested structures within typedef definitions
pub fn redirect_typedef(token_range: Range<usize>, result: HandlerResult) -> Result<HandlerResult> {
    report!(
        "typedef_handler",
        "redirect_typedef",
        Info,
        HandlerPhase::Convert,
        "Checking typedef for nested structures",
        true
    );

    // Check if this typedef defines inline structs, enums, or function pointers
    let mut i = 0;
    let mut processed_tokens = Vec::new();
    let context = crate::context!();
    let range_len = token_range.len();

    while i < range_len {
        let token = context.tokens[token_range.start + i].clone();

        // Check for inline struct definitions in typedef
        if token.to_string() == "struct" && i + 1 < range_len {
            // Check if this is an inline struct definition (has braces)
            let mut found_brace = false;
            let mut brace_start = i;

            for j in (i + 1)..range_len {
                if context.tokens[token_range.start + j].to_string() == ";" {
                    break; // Just a struct reference, not definition
                }
            }

            if found_brace {
                let mut brace_depth = 0;
                let mut struct_end = brace_start;

                for j in brace_start..range_len {
                    match context.tokens[token_range.start + j].to_string().as_str() {
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

                if struct_end > brace_start {
                    report!(
                        "typedef_handler",
                        "redirect_typedef",
                        Info,
                        HandlerPhase::Convert,
                        "Found inline struct definition in typedef, marking tokens as processed",
                        true
                    );

                    // Mark the struct definition tokens as processed
                    for k in i..=struct_end {
                        processed_tokens.push(context.tokens[token_range.start + k].clone());
                    }
                    i = struct_end + 1;
                    continue;
                }
            }
        }

        // Check for inline enum definitions in typedef
        if token.to_string() == "enum" && i + 1 < range_len {
            let mut found_brace = false;
            let mut brace_start = i;

            for j in (i + 1)..range_len {
                if context.tokens[token_range.start + j].to_string() == "{" {
                    found_brace = true;
                    brace_start = j;
                    break;
                }
                if context.tokens[token_range.start + j].to_string() == ";" {
                    break;
                }
            }

            if found_brace {
                let mut brace_depth = 0;
                let mut enum_end = brace_start;

                for j in brace_start..range_len {
                    match context.tokens[token_range.start + j].to_string().as_str() {
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

                if enum_end > brace_start {
                    report!(
                        "typedef_handler",
                        "redirect_typedef",
                        Info,
                        HandlerPhase::Convert,
                        "Found inline enum definition in typedef, marking tokens as processed",
                        true
                    );

                    // Mark enum definition tokens as processed
                    for k in i..=enum_end {
                        processed_tokens.push(context.tokens[token_range.start + k].clone());
                    }
                    i = enum_end + 1;
                    continue;
                }
            }
        }

        // Check for function pointer typedefs
        if token.to_string() == "(" && i > 0 && i + 1 < range_len {
            // Look for function pointer pattern: (*identifier)
            if context.tokens[token_range.start + i + 1].to_string() == "*" {
                let mut paren_depth = 0;
                let mut func_ptr_end = i;

                for j in i..range_len {
                    match context.tokens[token_range.start + j].to_string().as_str() {
                        "(" => paren_depth += 1,
                        ")" => {
                            paren_depth -= 1;
                            if paren_depth == 0 {
                                func_ptr_end = j;
                                break;
                            }
                        }
                        _ => {}
                    }
                }

                if func_ptr_end > i {
                    report!(
                        "typedef_handler",
                        "redirect_typedef",
                        Info,
                        HandlerPhase::Convert,
                        "Found function pointer in typedef, marking tokens as processed",
                        true
                    );

                    // Mark function pointer tokens as processed
                    for k in i..=func_ptr_end {
                        processed_tokens.push(tokens!(token_range.clone())[k].clone());
                    }
                    i = func_ptr_end + 1;
                    continue;
                }
            }
        }

        processed_tokens.push(token.clone());
        i += 1;
    }

    Ok(result)
}

/// Extract callback: Intelligent typedef extraction with recursive window expansion
fn extract_typedef(token_range: Range<usize>) -> Result<Option<ExtractedElement>> {
    let context = crate::context!();
    if token_range.start >= context.tokens[token_range.clone()].len() {
        return Ok(None);
    }

    // Step 1: Find the actual typedef start within the range
    let mut context = crate::context!();
    let typedef_start =
        find_typedef_start_in_range(&context.tokens[token_range.clone()], &token_range)?;
    if typedef_start.is_none() {
        return Ok(None);
    }
    let start_pos = typedef_start.unwrap();

    // Step 2: Recursively expand window to capture complete typedef
    let expanded_range = expand_to_typedef(&context.tokens[token_range.clone()], start_pos)?;
    if expanded_range.is_none() {
        return Ok(None);
    }
    let complete_range = expanded_range.unwrap();

    report!(
        "typedef_handler",
        "extract_typedef",
        Info,
        Extract,
        format!(
            "Expanded window from {}..{} to {}..{} to capture complete typedef",
            token_range.start, token_range.end, complete_range.start, complete_range.end
        ),
        true
    );

    // Step 3: Use Patternizer on the complete typedef
    let complete_tokens = &context.tokens[complete_range.clone()];

    // Try to match typedef patterns
    let typedef_patterns = [
        "typedef_simple",
        "typedef_function_pointer",
        "typedef_array",
        "typedef_pointer",
        "typedef",
    ];

    for pattern in &typedef_patterns {
        match context.patternizer.match_pattern(pattern, &complete_tokens) {
            PatternResult::Match { consumed_tokens: _ } => {
                let (name, source_tokens) = parse_typedef_tokens(&complete_tokens)?;

                let extracted_typedef = ExtractedTypedef {
                    name: name.clone(),
                    code: if complete_tokens.iter().any(|t| t.to_string() == "(") {
                        complete_tokens
                            .iter()
                            .map(|t| t.to_string())
                            .collect::<Vec<_>>()
                            .join(" ")
                    } else {
                        complete_tokens
                            .iter()
                            .map(|t| t.to_string())
                            .collect::<Vec<_>>()
                            .join(" ")
                    },
                    tokens: source_tokens,
                };

                report!(
                    "typedef_handler",
                    "extract_typedef",
                    Info,
                    Extract,
                    format!("Successfully extracted typedef: {}", name),
                    true
                );

                return Ok(Some(ExtractedElement::Typedef(extracted_typedef)));
            }
            _ => continue,
        }
    }

    // Fallback: try to extract basic typedef info even without pattern match
    report!(
        "typedef_handler",
        "extract_typedef",
        Info,
        Extract,
        "Patternizer patterns did not match complete typedef - using fallback extraction",
        true
    );

    extract_typedef_fallback(&complete_tokens)
}

/// Helper function to parse typedef tokens
fn parse_typedef_tokens(tokens: &[Token]) -> Result<(String, Vec<Token>)> {
    if tokens.len() < 3 || tokens[0].to_string() != "typedef" {
        return Err(C2RError::new(
            Kind::Other,
            Reason::Other("Invalid typedef syntax"),
            None,
        ));
    }

    // Find semicolon position
    let semicolon_pos = tokens
        .iter()
        .position(|t| t.to_string() == ";")
        .unwrap_or(tokens.len());

    if semicolon_pos == tokens.len() {
        return Err(C2RError::new(
            Kind::Other,
            Reason::Other("Typedef missing semicolon"),
            None,
        ));
    }

    // The name is the token just before the semicolon
    let name_pos = semicolon_pos - 1;
    if name_pos < 1 {
        return Err(C2RError::new(
            Kind::Other,
            Reason::Other("Typedef missing name"),
            None,
        ));
    }

    let typedef_name = tokens[name_pos].to_string();

    // Source type is everything between "typedef" and the name
    let source_tokens = tokens[1..name_pos].to_vec();

    if source_tokens.is_empty() {
        return Err(C2RError::new(
            Kind::Other,
            Reason::Other("Typedef missing source type"),
            None,
        ));
    }

    Ok((typedef_name, source_tokens))
}

/// Convert extracted typedef to Rust code using Patternizer exclusively
fn convert_typedef(token_range: Range<usize>) -> Result<Option<ConvertedElement>> {
    // Use cloned context to get the actual tokens from global context
    let mut context = crate::context!();
    let tokens_in_range = &context.tokens[token_range.clone()];

    // Filter out Token::n() from the retrieved tokens
    let filtered_tokens: Vec<Token> = tokens_in_range
        .iter()
        .filter(|token| !matches!(token, crate::Token::n()))
        .cloned()
        .collect();

    let mut processed_tokens = filtered_tokens.to_vec();

    // Try to match typedef patterns
    let typedef_patterns = [
        "typedef_simple",
        "typedef_function_pointer",
        "typedef_array",
        "typedef_pointer",
        "typedef",
    ];

    for pattern in &typedef_patterns {
        match context.patternizer.match_pattern(pattern, &filtered_tokens) {
            PatternResult::Match { consumed_tokens } => {
                // Skip struct/enum/union typedefs - they're handled by dedicated handlers
                if filtered_tokens.len() > 1 {
                    let second_token = filtered_tokens[1].to_string();
                    if second_token == "struct" || second_token == "enum" || second_token == "union"
                    {
                        return Ok(None);
                    }
                }

                // Mark processed tokens as consumed
                for i in 0..consumed_tokens.min(processed_tokens.len()) {
                    processed_tokens[i] = Token::n();
                }

                // Parse typedef components
                let (name, source_tokens) = parse_typedef_tokens(&filtered_tokens)?;

                let code = convert_typedef_to_rust(&name, &source_tokens, pattern)?;

                let converted_typedef = ConvertedTypedef {
                    target_type: source_tokens
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<_>>()
                        .join(" "),
                    code,
                    is_public: true,
                };

                return Ok(Some(ConvertedElement::Typedef(converted_typedef)));
            }
            _ => continue,
        }
    }

    // Fallback conversion for basic typedef
    if filtered_tokens.len() >= 4 && filtered_tokens.iter().any(|t| t.to_string() == "typedef") {
        // Skip typedef struct/enum/union
        if filtered_tokens.len() >= 2 {
            let second_token = filtered_tokens[1].to_string();
            if second_token == "struct" || second_token == "enum" || second_token == "union" {
                return Ok(None);
            }
        }

        let (name, source_tokens) = parse_typedef_tokens(&filtered_tokens)?;

        let code = convert_typedef_to_rust(&name, &source_tokens, "typedef")?;

        let converted_typedef = ConvertedTypedef {
            target_type: source_tokens
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(" "),
            code,
            is_public: true,
        };

        return Ok(Some(ConvertedElement::Typedef(converted_typedef)));
    }

    Ok(None)
}

/// Convert typedef to appropriate Rust code based on pattern type
fn convert_typedef_to_rust(
    name: &str,
    source_tokens: &[Token],
    pattern_type: &str,
) -> Result<String> {
    let source_type_str = source_tokens
        .iter()
        .map(|t| t.to_string())
        .collect::<Vec<_>>()
        .join(" ");

    match pattern_type {
        "typedef_function_pointer" => convert_function_pointer_typedef(name, source_tokens),
        "typedef_array" => convert_array_typedef(name, source_tokens),
        _ => {
            // Standard type alias
            let rust_type = convert_type(&source_type_str).unwrap_or(source_type_str);
            Ok(format!("pub type {} = {};\n", name, rust_type))
        }
    }
}

/// Convert function pointer typedef to Rust
fn convert_function_pointer_typedef(name: &str, source_tokens: &[Token]) -> Result<String> {
    // Extract return type (before first parenthesis)
    let mut return_type_tokens = Vec::new();
    let mut i = 0;

    while i < source_tokens.len() && source_tokens[i].to_string() != "(" {
        return_type_tokens.push(&source_tokens[i]);
        i += 1;
    }

    let return_type_str = return_type_tokens
        .iter()
        .map(|t| t.to_string())
        .collect::<Vec<_>>()
        .join(" ");

    let rust_return_type = convert_type(&return_type_str).unwrap_or(return_type_str);

    // Extract parameter types (simplified parsing)
    let mut param_types = Vec::new();

    // Find parameter list after (*name) pattern
    while i < source_tokens.len() {
        if source_tokens[i].to_string() == ")"
            && i + 1 < source_tokens.len()
            && source_tokens[i + 1].to_string() == "("
        {
            i += 2; // Skip ) (
            let mut param_start = i;

            while i < source_tokens.len() && source_tokens[i].to_string() != ")" {
                if source_tokens[i].to_string() == "," {
                    if i > param_start {
                        let param_type_str = source_tokens[param_start..i]
                            .iter()
                            .map(|t| t.to_string())
                            .collect::<Vec<_>>()
                            .join(" ");

                        let rust_param_type =
                            convert_type(&param_type_str).unwrap_or(param_type_str);
                        param_types.push(rust_param_type);
                    }
                    param_start = i + 1;
                }
                i += 1;
            }

            // Add last parameter
            if i > param_start {
                let param_type_str = source_tokens[param_start..i]
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(" ");

                let rust_param_type = convert_type(&param_type_str).unwrap_or(param_type_str);
                param_types.push(rust_param_type);
            }

            break;
        }
        i += 1;
    }

    // Build Rust function pointer type
    let mut code = format!("pub type {} = fn(", name);
    code.push_str(&param_types.join(", "));

    if rust_return_type != "()" && !rust_return_type.is_empty() {
        code.push_str(&format!(") -> {};\n", rust_return_type));
    } else {
        code.push_str(");\n");
    }

    Ok(code)
}

/// Convert array typedef to Rust
fn convert_array_typedef(name: &str, source_tokens: &[Token]) -> Result<String> {
    // Find base type (before '[')
    let mut base_type_tokens = Vec::new();

    for token in source_tokens {
        if token.to_string() == "[" {
            break;
        }
        base_type_tokens.push(token);
    }

    let base_type_str = base_type_tokens
        .iter()
        .map(|t| t.to_string())
        .collect::<Vec<_>>()
        .join(" ");

    let rust_base_type = convert_type(&base_type_str).unwrap_or(base_type_str);

    // Find array size between [ and ]
    let mut array_size = String::new();

    for i in 0..source_tokens.len() {
        if source_tokens[i].to_string() == "["
            && i + 2 < source_tokens.len()
            && source_tokens[i + 2].to_string() == "]"
        {
            array_size = source_tokens[i + 1].to_string();
            break;
        }
    }

    let code = if array_size.is_empty() {
        format!("pub type {} = [{}];\n", name, rust_base_type)
    } else {
        format!(
            "pub type {} = [{}; {}];\n",
            name, rust_base_type, array_size
        )
    };

    Ok(code)
}
