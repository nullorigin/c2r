//! Struct handler for C to Rust struct conversion
//! Uses Patternizer exclusively for detection and conversion

use super::common::not_handled;
use crate::error::Result;
use crate::pattern::PatternResult;
use crate::token::Token;
use crate::{C2RError, ConvertedElement, ExtractedElement, HandlerResult, Id, Kind, Reason};
use crate::{ConvertedStruct, ElementInfo, ExtractedStruct, HandlerReport, StructInfo};
use crate::{HandlerPhase, ReportLevel};
use crate::{context, report};
use core::option::Option::None;
use std::ops::Range;

/// Process callback: Lightweight struct detection using Patternizer
fn process_struct(token_range: Range<usize>) -> Result<bool> {
    let mut context = crate::context!();
    context.pull();

    // Add bounds checking to prevent slice index out of bounds
    if token_range.end > context.tokens.len() || token_range.start >= context.tokens.len() {
        return Ok(false);
    }

    if context.tokens[token_range.clone()].len() < 1 {
        return Ok(false);
    }

    // Use Patternizer for struct detection
    let tokens = &context.tokens[token_range.clone()];
    let struct_decl_result = context
        .patternizer
        .match_pattern("struct_declaration", tokens);
    let struct_def_result = context
        .patternizer
        .match_pattern("struct_definition", tokens);

    let is_struct = matches!(struct_decl_result, PatternResult::Match { .. })
        || matches!(struct_def_result, PatternResult::Match { .. });

    if is_struct {
        report!(
            "struct_handler",
            "process_struct",
            Info,
            Process,
            "Detected struct construct using Patternizer",
            true
        );
    }

    Ok(is_struct)
}

use crate::{
    HandlerPhase::{Convert, Extract, Handle, Process, Report},
    ReportLevel::{Error, Info, Warning},
};

/// Report callback: Collects struct handler reports
pub fn report_struct() -> Result<HandlerReport> {
    let context = context!();
    let reports = context.get_reports_by_handler("struct_handler");

    let (info_count, warning_count, error_count) =
        reports
            .iter()
            .fold((0, 0, 0), |acc, report| match report.level {
                ReportLevel::Error => (acc.0, acc.1, acc.2 + 1),
                ReportLevel::Warning => (acc.0, acc.1 + 1, acc.2),
                _ => (acc.0 + 1, acc.1, acc.2),
            });

    Ok(HandlerReport {
        report_id: Box::new(Id::get(&Id::gen_name("struct_handler"))),
        handler_id: Box::new(Id::get("struct_handler")),
        handler_name: "struct_handler".to_string(),
        function_name: "report_struct".to_string(),
        message: format!(
            "Struct handler summary: {} reports ({} info, {} warnings, {} errors)",
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

/// Find struct start position within the given range
fn find_struct_start_in_range(tokens: &[Token], range: &Range<usize>) -> Result<Option<usize>> {
    let range_tokens = &tokens[range.clone()];

    for (i, window) in range_tokens.windows(1).enumerate() {
        if detect_struct_start(window) {
            return Ok(Some(range.start + i));
        }
    }

    Ok(None)
}

/// Detect if token window indicates struct start
fn detect_struct_start(tokens: &[Token]) -> bool {
    if tokens.is_empty() {
        return false;
    }

    tokens[0].to_string() == "struct"
}

/// Recursively expand window to capture complete struct starting with minimum viable tokens
#[allow(unused_variables)]
fn expand_to_struct(tokens: &[Token], start_pos: usize) -> Result<Option<Range<usize>>> {
    if start_pos >= tokens.len() {
        return Ok(None);
    }

    // Validate we have struct at the start
    if tokens[start_pos].to_string() != "struct" {
        return Ok(None);
    }

    // Constants for minimum viable struct tokens
    const MIN_STRUCT_DECLARATION: usize = 3; // struct Point ;
    #[allow(dead_code)]
    const MIN_STRUCT_DEFINITION: usize = 5; // struct Point { } ;

    // Step 1: Start with minimum declaration window (3 tokens)
    let mut current_end = (start_pos + MIN_STRUCT_DECLARATION).min(tokens.len());

    // Step 2: Check if we have struct name in minimum window
    if current_end > start_pos + 1 {
        let potential_name_pos = start_pos + 1;
        if potential_name_pos < tokens.len() {
            let next_token_pos = start_pos + 2;
            if next_token_pos < tokens.len() {
                let next_token = tokens[next_token_pos].to_string();
                if next_token == ";" {
                    // Struct declaration: struct Name;
                    return Ok(Some(start_pos..next_token_pos + 1));
                } else if next_token == "{" {
                    // Struct definition: find matching closing brace
                    let brace_end = find_matching_brace_struct(tokens, next_token_pos)?;
                    if let Some(body_end) = brace_end {
                        // Check for optional semicolon after brace
                        let final_end = if body_end + 1 < tokens.len()
                            && tokens[body_end + 1].to_string() == ";"
                        {
                            body_end + 2
                        } else {
                            body_end + 1
                        };
                        return Ok(Some(start_pos..final_end));
                    }
                }
            }
        }
    }

    // Step 3: Progressive expansion if minimum window is incomplete
    for expand_size in 1..=15 {
        current_end = (start_pos + MIN_STRUCT_DECLARATION + expand_size).min(tokens.len());
        if current_end >= tokens.len() {
            break;
        }

        // Look for completion patterns in expanded window
        for i in (start_pos + MIN_STRUCT_DECLARATION)..current_end {
            let token_str = tokens[i].to_string();
            if token_str == ";" {
                return Ok(Some(start_pos..i + 1));
            } else if token_str == "{" {
                let brace_end = find_matching_brace_struct(tokens, i)?;
                if let Some(body_end) = brace_end {
                    let final_end =
                        if body_end + 1 < tokens.len() && tokens[body_end + 1].to_string() == ";" {
                            body_end + 2
                        } else {
                            body_end + 1
                        };
                    return Ok(Some(start_pos..final_end));
                }
            }
        }
    }

    // Fallback: use minimum viable window
    let final_end = (start_pos + MIN_STRUCT_DECLARATION).min(tokens.len());
    Ok(Some(start_pos..final_end))
}

/// Find matching brace for struct definitions
fn find_matching_brace_struct(tokens: &[Token], start: usize) -> Result<Option<usize>> {
    let mut depth = 0;
    for i in start..tokens.len() {
        match tokens[i].to_string().as_str() {
            "{" => depth += 1,
            "}" => {
                depth -= 1;
                if depth == 0 {
                    return Ok(Some(i));
                }
            }
            _ => {}
        }
    }
    Ok(None)
}

/// Fallback extraction when patterns don't match
fn extract_struct_fallback(tokens: &[Token]) -> Result<Option<ExtractedElement>> {
    if tokens.len() < 2 {
        return Ok(None);
    }

    // Must start with struct
    if tokens[0].to_string() != "struct" {
        return Ok(None);
    }

    let name = if tokens.len() > 1 {
        tokens[1].to_string()
    } else {
        "UnknownStruct".to_string()
    };

    let extracted_struct = ExtractedStruct {
        name,
        fields: Vec::new(),
        tokens: tokens.to_vec(),
        is_typedef: false,
        is_forward_declaration: true,
        typedef_name: None,
        code: tokens
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<_>>()
            .join(" "),
    };

    Ok(Some(ExtractedElement::Struct(extracted_struct)))
}

/// Redirect callback: Handles nested structures within structs
pub fn redirect_struct(token_range: Range<usize>, result: HandlerResult) -> Result<HandlerResult> {
    let mut context = crate::context!();
    context.pull();
    let mut processed_tokens = context.tokens[token_range.clone()].to_vec();

    // Look for nested structures that need to be processed by other handlers
    let mut i = 0;
    while i < processed_tokens.len() {
        if processed_tokens[i].to_string() == "enum" {
            // Found nested enum - redirect to enum handler
            let enum_start = i;
            let mut brace_count = 0;
            let mut enum_end = enum_start;

            // Find the end of the enum definition
            for j in enum_start..processed_tokens.len() {
                match processed_tokens[j].to_string().as_str() {
                    "{" => brace_count += 1,
                    "}" => {
                        brace_count -= 1;
                        if brace_count == 0 {
                            enum_end = j + 1;
                            break;
                        }
                    }
                    _ => {}
                }
            }

            if enum_end > enum_start {
                // Mark tokens as consumed and log redirect
                for idx in enum_start..enum_end {
                    processed_tokens[idx] = Token::n();
                }

                report!(
                    "struct_handler",
                    "redirect_struct",
                    Info,
                    Convert,
                    "Redirected nested enum to enum handler",
                    true
                );
            }
            i = enum_end;
        } else if processed_tokens[i].to_string() == "union" {
            // Similar handling for nested unions
            // Mark tokens as consumed and redirect to appropriate handler
            i += 1;
        } else {
            i += 1;
        }
    }

    // Return updated result with processed tokens
    match result {
        HandlerResult::Converted(element, range, code, id) => {
            Ok(HandlerResult::Converted(element, range, code, id))
        }
        other => Ok(other),
    }
}

/// Creates a struct handler that uses Patternizer exclusively
pub fn create_struct_handler() -> crate::handler::Handler {
    let handler_id = Id::get("struct_handler");
    let handler_role = "struct";
    let priority = 100;

    super::create_handler(
        handler_id,
        handler_role,
        priority,
        Some(process_struct),
        Some(handle_struct),
        Some(extract_struct),
        Some(convert_struct),
        Some(document_struct),
        Some(report_struct),
        Some(result_struct), // Result callback for final processing
        Some(redirect_struct),
    )
}

/// Result callback: Postprocesses struct conversion with documentation
#[allow(unused_variables)]
fn result_struct(token_range: Range<usize>, result: HandlerResult) -> Result<HandlerResult> {
    let _id = Id::get("result_struct");

    report!(
        "struct_handler",
        "result_struct",
        Info,
        Report,
        "Postprocessing struct conversion result",
        true
    );

    match result {
        HandlerResult::Converted(element, token_range, code, id) => {
            // Extract struct information from tokens for documentation
            let mut context = crate::context!();
            context.pull();
            let struct_info = extract_struct_info(&context.tokens[token_range.clone()]);

            // Generate documentation about the struct conversion
            let doc_comment =
                document_struct(ElementInfo::Struct(struct_info.clone()))?.unwrap_or_default();

            // Enhance the Rust code with documentation and metadata
            let mut enhanced_code = String::new();

            // Add metadata comment for traceability
            let metadata_comment = format!(
                "// [C2R] Struct converted from C to Rust - {}\n",
                struct_info.name
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
                "struct_handler",
                "result_struct",
                Info,
                Report,
                format!(
                    "Enhanced struct '{}' with {} lines of documentation",
                    struct_info.name,
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
                "struct_handler",
                "result_struct",
                Warning,
                Report,
                "Struct result was not in Completed state, returning as-is",
                true
            );

            Ok(result)
        }
    }
}

/// Extract struct information from tokens for result processing
fn extract_struct_info(tokens: &[Token]) -> StructInfo {
    let mut name = String::new();
    let mut field_count = 0;
    let mut is_typedef = false;
    let mut is_anonymous = false;
    let mut is_forward_declaration = true; // Start as forward declaration
    let mut is_packed = false;
    let mut typedef_name = None;
    let mut has_nested_structs = false;
    let mut has_unions = false;

    // Basic parsing to extract struct information
    let mut i = 0;
    while i < tokens.len() {
        match &tokens[i] {
            Token::s(keyword) if keyword == "typedef" => is_typedef = true,
            Token::s(keyword) if keyword == "__packed__" || keyword == "packed" => is_packed = true,
            Token::s(keyword) if keyword == "struct" => {
                if i + 1 < tokens.len() {
                    if let Token::s(id) = &tokens[i + 1] {
                        name = id.clone();
                    } else {
                        is_anonymous = true;
                        name = "anonymous_struct".to_string();
                    }
                }
            }
            Token::l("{") => {
                // Has body, so not a forward declaration
                is_forward_declaration = false;

                // Count fields and check for nested structures
                let mut brace_count = 1;
                let mut j = i + 1;
                while j < tokens.len() && brace_count > 0 {
                    match &tokens[j] {
                        Token::l("{") => brace_count += 1,
                        Token::l("}") => brace_count -= 1,
                        Token::l(";") if brace_count == 1 => field_count += 1,
                        Token::s(keyword) if keyword == "struct" && brace_count > 1 => {
                            has_nested_structs = true
                        }
                        Token::s(keyword) if keyword == "union" => has_unions = true,
                        _ => {}
                    }
                    j += 1;
                }
            }
            _ => {}
        }
        i += 1;
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

    if name.is_empty() {
        name = "unnamed_struct".to_string();
    }

    StructInfo {
        name,
        field_count,
        is_typedef,
        is_anonymous,
        is_forward_declaration,
        is_packed,
        typedef_name,
        complexity: if has_nested_structs || has_unions || field_count > 5 {
            "complex".to_string()
        } else {
            "simple".to_string()
        },
    }
}

/// Handle callback: Processes struct tokens using Patternizer
fn handle_struct(token_range: Range<usize>) -> Result<HandlerResult> {
    let mut context = crate::context!();
    context.pull();
    match context
        .patternizer
        .match_pattern("struct", &context.tokens[token_range.clone()])
    {
        PatternResult::Match { consumed_tokens } => {
            report!(
                "struct_handler",
                "handle_struct",
                Info,
                Handle,
                format!("Processing struct with {} tokens", consumed_tokens),
                true
            );

            // Convert using pattern data
            match convert_struct(token_range.clone())? {
                Some(ConvertedElement::Struct(converted_struct)) => {
                    let code = converted_struct.code.clone();
                    Ok(HandlerResult::Converted(
                        ConvertedElement::Struct(converted_struct),
                        0..consumed_tokens,
                        code,
                        Id::get("struct_handler"),
                    ))
                }
                _ => {
                    report!(
                        "struct_handler",
                        "handle_struct",
                        Error,
                        Handle,
                        "Failed to convert struct",
                        false
                    );
                    not_handled()
                }
            }
        }
        _ => not_handled(),
    }
}

/// Extract callback: Intelligent struct extraction with recursive window expansion
pub fn extract_struct(token_range: Range<usize>) -> Result<Option<ExtractedElement>> {
    let mut context = crate::context!();
    context.pull();
    if token_range.start >= context.tokens[token_range.clone()].len() {
        return Ok(None);
    }

    // Step 1: Find the actual struct start within the range
    let struct_start =
        find_struct_start_in_range(&context.tokens[token_range.clone()], &token_range)?;
    if struct_start.is_none() {
        return Ok(None);
    }
    let start_pos = struct_start.unwrap();

    // Step 2: Recursively expand window to capture complete struct
    let expanded_range = expand_to_struct(&context.tokens[token_range.clone()], start_pos)?;
    if expanded_range.is_none() {
        return Ok(None);
    }
    let complete_range = expanded_range.unwrap();

    report!(
        "struct_handler",
        "extract_struct",
        Info,
        Extract,
        format!(
            "Expanded window from {}..{} to {}..{} to capture complete struct",
            token_range.start, token_range.end, complete_range.start, complete_range.end
        ),
        true
    );

    // Step 3: Use Patternizer on the complete struct
    let complete_tokens = &context.tokens[complete_range.clone()];

    match context.patternizer.match_pattern("struct", complete_tokens) {
        PatternResult::Match { consumed_tokens } => {
            let (name, fields, is_typedef) = parse_struct_tokens(complete_tokens)?;
            let is_forward_declaration = fields.is_empty();

            let extracted_struct = ExtractedStruct {
                name: name.clone(),
                fields,
                tokens: complete_tokens[..consumed_tokens.min(complete_tokens.len())].to_vec(),
                is_typedef,
                is_forward_declaration,
                typedef_name: None,
                code: complete_tokens[..consumed_tokens.min(complete_tokens.len())]
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(" "),
            };

            report!(
                "struct_handler",
                "extract_struct",
                Info,
                Extract,
                format!("Successfully extracted struct: {}", name),
                true
            );

            Ok(Some(ExtractedElement::Struct(extracted_struct)))
        }
        _ => {
            report!(
                "struct_handler",
                "extract_struct",
                Info,
                Extract,
                "Patternizer patterns did not match complete struct - using fallback extraction",
                true
            );

            // Fallback: try to extract basic struct info even without pattern match
            extract_struct_fallback(complete_tokens)
        }
    }
}

/// Convert callback: Converts extracted struct to Rust
pub fn convert_struct(token_range: Range<usize>) -> Result<Option<ConvertedElement>> {
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
    if let Some(ExtractedElement::Struct(element)) = extract_struct(token_range.clone())? {
        report!(
            "struct_handler",
            "convert_struct",
            Info,
            Convert,
            format!("Converting struct: {}", element.name),
            true
        );

        let code = generate_rust_struct(&element)?;

        Ok(Some(ConvertedElement::Struct(ConvertedStruct {
            fields: element
                .fields
                .iter()
                .map(|(name, field_type)| {
                    (
                        name.clone(),
                        field_type
                            .iter()
                            .map(|t| t.to_string())
                            .collect::<Vec<_>>()
                            .join(" "),
                    )
                })
                .collect(),
            code,
            is_public: true,
            derives: vec!["Debug".to_string(), "Clone".to_string()],
        })))
    } else {
        Ok(None)
    }
}

/// Document callback: Generates documentation for struct
fn document_struct(info: ElementInfo) -> Result<Option<String>> {
    if let ElementInfo::Struct(struct_info) = info {
        let doc = format!(
            "/// Converted C struct: {}\n/// Fields: {}\n",
            struct_info.name, struct_info.field_count
        );
        Ok(Some(doc))
    } else {
        Ok(None)
    }
}

/// Parse struct tokens to extract components
fn parse_struct_tokens(tokens: &[Token]) -> Result<(String, Vec<(String, Vec<Token>)>, bool)> {
    let mut i = 0;
    let mut name = String::new();
    let mut fields = Vec::new();
    let mut is_typedef = false;

    // Check for typedef
    if tokens.len() > 0 && tokens[0].to_string() == "typedef" {
        is_typedef = true;
        i = 1;
    }

    // Find struct keyword
    if i < tokens.len() && tokens[i].to_string() == "struct" {
        i += 1;
    } else {
        return Err(C2RError::new(
            Kind::Other,
            Reason::Expected("struct keyword"),
            None,
        ));
    }

    // Get struct name
    if i < tokens.len() && tokens[i].to_string() != "{" {
        name = tokens[i].to_string();
        i += 1;
    }

    // Find opening brace
    if i < tokens.len() && tokens[i].to_string() == "{" {
        i += 1;

        // Parse fields
        while i < tokens.len() && tokens[i].to_string() != "}" {
            let field_start = i;

            // Find semicolon to end the field
            while i < tokens.len() && tokens[i].to_string() != ";" {
                i += 1;
            }

            if i > field_start {
                let field_tokens = &tokens[field_start..i];
                if field_tokens.len() >= 2 {
                    let field_name = field_tokens.last().unwrap().to_string();
                    let field_type = field_tokens[..field_tokens.len() - 1].to_vec();
                    fields.push((field_name, field_type));
                }
            }

            if i < tokens.len() && tokens[i].to_string() == ";" {
                i += 1;
            }
        }
    }

    if name.is_empty() {
        name = "AnonymousStruct".to_string();
    }

    Ok((name, fields, is_typedef))
}

/// Generate Rust struct code
fn generate_rust_struct(struct_elem: &ExtractedStruct) -> Result<String> {
    let mut code = String::new();

    // Add derives
    code.push_str("#[derive(Debug, Clone)]\n");

    // Add pub keyword
    code.push_str("pub struct ");
    code.push_str(&struct_elem.name);
    code.push_str(" {\n");

    // Add fields
    for (field_name, field_type) in &struct_elem.fields {
        let rust_type = convert_c_type_to_rust(
            &field_type
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(" "),
        );
        code.push_str(&format!("    pub {}: {},\n", field_name, rust_type));
    }

    code.push_str("}\n");

    Ok(code)
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
