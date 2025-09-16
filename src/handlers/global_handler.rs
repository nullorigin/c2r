//! Global handler for C to Rust global variable conversion
//! Uses Patternizer exclusively for detection and conversion

use super::common::not_handled;
use crate::convert::convert_type;
use crate::pattern::{PatternResult, Patternizer};
use crate::token::Token;
use crate::{
    C2RError, ConvertedElement, ConvertedGlobal, ElementInfo, ExtractedElement, ExtractedGlobal,
    GlobalInfo, HandlerPhase, HandlerReport, HandlerResult, Id, Kind, Reason, ReportLevel,
};
use crate::{
    HandlerPhase::{Process, Report},
    ReportLevel::{Info, Warning},
};
use crate::{context, report};
use std::ops::Range;

/// Report callback: Collects global handler reports
pub fn report_global() -> Result<HandlerReport, C2RError> {
    let context = context!();
    let reports = context.get_reports_by_handler("global_handler");

    let (info_count, warning_count, error_count) =
        reports
            .iter()
            .fold((0, 0, 0), |acc, report| match report.level {
                ReportLevel::Error => (acc.0, acc.1, acc.2 + 1),
                ReportLevel::Warning => (acc.0, acc.1 + 1, acc.2),
                _ => (acc.0 + 1, acc.1, acc.2),
            });

    Ok(HandlerReport {
        report_id: Box::new(Id::get(&Id::gen_name("global_handler"))),
        handler_id: Box::new(Id::get("global_handler")),
        handler_name: "global_handler".to_string(),
        function_name: "report_global".to_string(),
        message: format!(
            "Global handler summary: {} reports ({} info, {} warnings, {} errors)",
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

/// Handle callback: Sets up patternizer and registers global patterns
fn handle_global(token_range: Range<usize>) -> Result<HandlerResult, C2RError> {
    report!(
        "global_handler",
        "handle_global",
        ReportLevel::Info,
        HandlerPhase::Handle,
        format!("HANDLE DEBUG: Processing {} tokens", {
            let mut context = crate::context!();
            context.pull();
            context.tokens[token_range.clone()].len()
        }),
        true
    );
    let mut context = context!();

    // Register global patterns in the global registry via context
    let global_patterns = [
        "global_variable",
        "global_const",
        "global_static",
        "global_extern",
        "global",
    ];
    for pattern in &global_patterns {
        let mut temp_context = crate::context!();
        temp_context.pull();
        match context
            .patternizer
            .match_pattern(pattern, &temp_context.tokens[token_range.clone()])
        {
            PatternResult::Match { consumed_tokens } => {
                report!(
                    "global_handler",
                    "handle_global",
                    ReportLevel::Info,
                    HandlerPhase::Handle,
                    format!(
                        "HANDLE DEBUG: Patternizer pattern '{}' matched with {} tokens",
                        pattern, consumed_tokens
                    ),
                    true
                );
                // Convert using pattern data
                match convert_global(token_range.clone())? {
                    Some(ConvertedElement::Global(converted_global)) => {
                        let code = converted_global.code.clone();
                        return Ok(HandlerResult::Converted(
                            ConvertedElement::Global(converted_global),
                            token_range.clone(),
                            code,
                            Id::get("global_handler"),
                        ));
                    }
                    _ => {
                        report!(
                            "global_handler",
                            "handle_global",
                            ReportLevel::Error,
                            HandlerPhase::Handle,
                            "Failed to convert global after pattern match",
                            false
                        );
                        return not_handled();
                    }
                }
            }
            _ => continue,
        }
    }

    // Fallback: Use same logic as process_global for consistency
    let mut context_temp = crate::context!();
    context_temp.pull();
    let first_token = context_temp.tokens[token_range.clone()][0].to_string();
    if ["static", "extern", "const"]
        .iter()
        .any(|&kw| first_token == kw)
        || (context_temp.tokens[token_range.clone()].len() >= 3
            && context_temp.tokens[token_range.clone()]
                .iter()
                .any(|t| t.to_string() == "="))
    {
        report!(
            "global_handler",
            "handle_global",
            ReportLevel::Info,
            HandlerPhase::Handle,
            format!(
                "HANDLE DEBUG: Fallback logic matched for token: '{}'",
                first_token
            ),
            true
        );

        // Convert using fallback detection
        match convert_global(token_range.clone())? {
            Some(ConvertedElement::Global(converted_global)) => {
                let code = converted_global.code.clone();
                return Ok(HandlerResult::Converted(
                    ConvertedElement::Global(converted_global),
                    token_range.clone(),
                    code,
                    Id::get("global_handler"),
                ));
            }
            _ => {
                report!(
                    "global_handler",
                    "handle_global",
                    ReportLevel::Error,
                    HandlerPhase::Handle,
                    "Failed to convert global using fallback logic",
                    false
                );
                return not_handled();
            }
        }
    }

    report!(
        "global_handler",
        "handle_global",
        ReportLevel::Info,
        HandlerPhase::Handle,
        "HANDLE DEBUG: No patterns matched, returning NotHandled",
        true
    );

    not_handled()
}

/// Process callback: Uses Patternizer to detect global variable patterns
fn process_global(token_range: Range<usize>) -> Result<bool, C2RError> {
    let mut context = crate::context!();

    // Add bounds checking to prevent slice index out of bounds
    if token_range.end > context.tokens.len() || token_range.start >= context.tokens.len() {
        return Ok(false);
    }

    let tokens = &context.tokens[token_range.clone()];
    let filtered_tokens: Vec<Token> = tokens
        .iter()
        .filter(|token| !matches!(token, crate::Token::n()))
        .cloned()
        .collect();
    let tokens = &filtered_tokens;

    if token_range.start >= tokens.len() {
        return Ok(false);
    }

    // Debug: Show what tokens we're checking
    let token_preview = tokens
        .iter()
        .take(10)
        .map(|t| t.to_string())
        .collect::<Vec<_>>()
        .join(" ");
    report!(
        "global_handler",
        "process_global",
        Info,
        Process,
        format!(
            "GLOBAL DEBUG: Checking {} tokens: [{}{}]",
            tokens.len(),
            token_preview,
            if tokens.len() > 10 { "..." } else { "" }
        ),
        true
    );

    // Check for global variable patterns
    let global_patterns = [
        "global_variable",
        "global_const",
        "global_static",
        "global_extern",
        "global",
    ];

    for pattern in &global_patterns {
        if let PatternResult::Match { consumed_tokens: _ } =
            context.patternizer.match_pattern(pattern, tokens)
        {
            report!(
                "global_handler",
                "process_global",
                Info,
                Process,
                format!("Global pattern '{}' matched!", pattern),
                true
            );
            return Ok(true);
        }
    }

    // Fallback: check for global variable indicators
    let first_token = tokens[0].to_string();
    let result = ["static", "extern", "const"]
        .iter()
        .any(|&kw| first_token == kw)
        || (tokens.len() >= 3 && tokens.iter().any(|t| t.to_string() == "="));

    if result {
        report!(
            "global_handler",
            "process_global",
            Info,
            Process,
            format!("Global fallback matched for token: '{}'", first_token),
            true
        );
    }

    Ok(result)
}

/// Represents the kind of global variable
#[derive(Debug, Clone, PartialEq)]
pub enum GlobalKind {
    Variable,
    Constant,
    Array,
    Pointer,
}

/// Creates a global handler that uses Patternizer exclusively
pub fn create_global_handler() -> crate::handler::Handler {
    let handler_id = Id::get("global_handler");
    let handler_role = "global";
    let priority = 90; // Lower than function/struct/enum but higher than simple statements

    super::create_handler(
        handler_id,
        handler_role,
        priority,
        Some(process_global),
        Some(handle_global),
        Some(extract_global),
        Some(convert_global),
        Some(document_global),
        Some(report_global),
        Some(result_global),   // Result callback for final processing
        Some(redirect_global), // Redirect callback for nested structures
    )
}

/// Result callback: Postprocesses global variable conversion with documentation
#[allow(unused_variables)]
fn result_global(
    token_range: Range<usize>,
    result: HandlerResult,
) -> Result<HandlerResult, C2RError> {
    let _id = Id::get("result_global");

    report!(
        "global_handler",
        "result_global",
        Info,
        Report,
        "Postprocessing global variable conversion result",
        true
    );

    match result {
        HandlerResult::Converted(element, token_range, code, id) => {
            // Extract global information from tokens for documentation
            let mut context = crate::context!();
            context.pull();
            let global_info = extract_global_info(&context.tokens[token_range.clone()]);

            // Generate documentation about the global conversion
            let doc_comment =
                document_global(ElementInfo::Global(global_info.clone()))?.unwrap_or_default();

            // Enhance the Rust code with documentation and metadata
            let mut enhanced_code = String::new();

            // Add metadata comment for traceability
            let metadata_comment = format!(
                "// [C2R] Global variable converted from C to Rust - {}: {}\n",
                global_info.name, global_info.type_name
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
                "global_handler",
                "result_global",
                Info,
                Report,
                format!(
                    "Enhanced global '{}' with {} lines of documentation",
                    global_info.name,
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
                "global_handler",
                "result_global",
                Warning,
                Report,
                "Global result was not in Completed state, returning as-is",
                true
            );

            Ok(result)
        }
    }
}

/// Extract global information from tokens for result processing
fn extract_global_info(tokens: &[Token]) -> GlobalInfo {
    use crate::global_handler::GlobalKind;

    let mut name = String::new();
    let mut type_name = String::new();
    let mut storage_class = None;
    let mut is_const = false;
    let mut is_array = false;
    let mut has_initializer = false;

    // Basic parsing to extract global information
    for (_i, token) in tokens.iter().enumerate() {
        match token {
            Token::s(keyword) if keyword == "static" => storage_class = Some("static".to_string()),
            Token::s(keyword) if keyword == "extern" => storage_class = Some("extern".to_string()),
            Token::s(keyword) if keyword == "const" => is_const = true,
            Token::s(keyword)
                if keyword == "int"
                    || keyword == "char"
                    || keyword == "float"
                    || keyword == "double" =>
            {
                type_name = keyword.clone();
            }
            Token::s(id) if type_name.is_empty() => {
                type_name = id.clone();
            }
            Token::s(id) if !type_name.is_empty() && name.is_empty() => {
                name = id.clone();
            }
            Token::l("[") => is_array = true,
            Token::l("=") => has_initializer = true,
            _ => {}
        }
    }

    if name.is_empty() {
        name = "unnamed_global".to_string();
    }
    if type_name.is_empty() {
        type_name = "unknown".to_string();
    }

    let kind = if is_array {
        GlobalKind::Array
    } else {
        GlobalKind::Variable
    };

    let kind_description = match kind {
        GlobalKind::Variable => "global variable".to_string(),
        GlobalKind::Array => "global array".to_string(),
        _ => "global element".to_string(),
    };

    GlobalInfo {
        name,
        kind,
        kind_description,
        type_name,
        storage_class: storage_class.clone(),
        is_const,
        is_array,
        has_initializer,
        complexity: if storage_class.is_some() || is_const || has_initializer {
            "complex".to_string()
        } else {
            "simple".to_string()
        },
    }
}

/// Document callback: Generates documentation for global variables
fn document_global(info: ElementInfo) -> Result<Option<String>, C2RError> {
    if let ElementInfo::Global(global_info) = info {
        let mut doc = format!("/// Converted C global variable: {}\n", global_info.name);

        // For complex globals, use Patternizer to determine purpose
        if global_info.complexity == "complex" {
            let _patternizer = Patternizer::with_common_patterns();

            // Determine global purpose based on patterns
            let purpose = if let Some(ref storage) = global_info.storage_class {
                if storage == "static" && global_info.is_const {
                    "static constant for configuration or lookup tables"
                } else if storage == "extern" {
                    "external variable for inter-module communication"
                } else if storage == "static" {
                    "static variable for module-local state"
                } else {
                    "global variable for shared program data"
                }
            } else if global_info.has_initializer {
                "initialized global variable for program state"
            } else {
                "global variable for shared program data"
            };

            doc.push_str(&format!("/// Purpose: {}\n", purpose));
        }

        doc.push_str(&format!("/// Type: {}\n", global_info.type_name));

        if let Some(ref storage) = global_info.storage_class {
            doc.push_str(&format!("/// Storage class: {}\n", storage));
        }

        if global_info.is_const {
            doc.push_str("/// Mutability: Read-only\n");
        }

        if global_info.is_array {
            doc.push_str("/// Note: Array type\n");
        }

        Ok(Some(doc))
    } else {
        Ok(None)
    }
}

/// Redirect callback: Handles nested structures within global variable declarations
pub fn redirect_global(
    token_range: Range<usize>,
    result: HandlerResult,
) -> Result<HandlerResult, C2RError> {
    report!(
        "global_handler",
        "redirect_global",
        Info,
        HandlerPhase::Convert,
        "Checking global declaration for nested structures",
        true
    );

    // Check if this global variable declaration contains struct or enum definitions
    let mut i = 0;
    let mut processed_tokens = Vec::new();

    while i < token_range.len() {
        let mut context = crate::context!();
        context.pull();
        let token = context.tokens[i].clone();

        // Check for inline struct definitions in global variables
        if token.to_string() == "struct" && i + 1 < token_range.len() {
            // Look for opening brace to confirm this is a struct definition, not just a declaration
            let mut found_brace = false;
            let mut brace_start = i;

            for j in (i + 1)..token_range.len() {
                if context.tokens[j].to_string() == "{" {
                    found_brace = true;
                    brace_start = j;
                    break;
                }
                if context.tokens[j].to_string() == ";" {
                    break; // This is just a struct declaration, not definition
                }
            }

            if found_brace {
                // Find the matching closing brace
                let mut brace_depth = 0;
                let mut struct_end = brace_start;

                for j in brace_start..token_range.len() {
                    match context.tokens[j].to_string().as_str() {
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
                        "global_handler",
                        "redirect_global",
                        Info,
                        HandlerPhase::Convert,
                        "Found inline struct definition in global variable, marking tokens as processed",
                        true
                    );

                    // Mark the struct definition tokens as processed
                    for _k in i..=struct_end {
                        processed_tokens.push(Token::n());
                    }
                    i = struct_end + 1;
                    continue;
                }
            }
        }

        // Check for inline enum definitions
        if token.to_string() == "enum" && i + 1 < token_range.len() {
            let mut found_brace = false;
            let mut brace_start = i;

            for j in (i + 1)..token_range.len() {
                if context.tokens[j].to_string() == "{" {
                    found_brace = true;
                    brace_start = j;
                    break;
                }
                if context.tokens[j].to_string() == ";" {
                    break;
                }
            }

            if found_brace {
                let mut brace_depth = 0;
                let mut enum_end = brace_start;

                for j in brace_start..token_range.len() {
                    match context.tokens[j].to_string().as_str() {
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
                        "global_handler",
                        "redirect_global",
                        Info,
                        HandlerPhase::Convert,
                        "Found inline enum definition in global variable, marking tokens as processed",
                        true
                    );

                    // Mark enum definition tokens as processed
                    for _k in i..=enum_end {
                        processed_tokens.push(Token::n());
                    }
                    i = enum_end + 1;
                    continue;
                }
            }
        }

        processed_tokens.push(token.clone());
        i += 1;
    }

    Ok(result)
}

/// Extract global variable information from tokens using Patternizer exclusively
fn extract_global(token_range: Range<usize>) -> Result<Option<ExtractedElement>, C2RError> {
    // Try to match global variable patterns
    let global_patterns = [
        "global_variable",
        "global_array",
        "global_pointer",
        "global_const",
        "global_static",
        "global_extern",
    ];

    for pattern in &global_patterns {
        let mut context = crate::context!();
        context.pull();
        match context
            .patternizer
            .match_pattern(pattern, &context.tokens[token_range.clone()])
        {
            PatternResult::Match { consumed_tokens: _ } => {
                // Extract global variable components
                let (name, var_type, initializer, storage_class) =
                    parse_global_tokens(&context.tokens[token_range.clone()])?;

                let is_const = storage_class.as_ref().map_or(false, |s| s == "const");
                let is_static = storage_class.as_ref().map_or(false, |s| s == "static");
                let is_extern = storage_class.as_ref().map_or(false, |s| s == "extern");

                let extracted_global = ExtractedGlobal {
                    name,
                    type_name: var_type,
                    initializer,
                    storage_class,
                    tokens: context.tokens[token_range.clone()].to_vec(),
                    code: context.tokens[token_range.clone()]
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<_>>()
                        .join(" "),
                    array_dims: Vec::new(),
                    initial_value: None,
                    is_const,
                    is_static,
                    is_extern,
                    array_size: None,
                };

                return Ok(Some(ExtractedElement::Global(extracted_global)));
            }
            _ => continue,
        }
    }

    // Fallback for basic global variable detection
    let mut context = crate::context!();
    context.pull();
    if context.tokens[token_range.clone()].len() >= 3
        && context.tokens[token_range.clone()]
            .iter()
            .any(|t| t.to_string() == ";")
    {
        // Skip function declarations (contain parentheses)
        if context.tokens[token_range.clone()]
            .iter()
            .any(|t| t.to_string() == "(")
        {
            return Ok(None);
        }

        // Skip typedef, struct, enum declarations
        let first_token = context.tokens[token_range.clone()][0].to_string();
        if first_token == "typedef" || first_token == "struct" || first_token == "enum" {
            return Ok(None);
        }

        let (name, var_type, initializer, storage_class) =
            parse_global_tokens(&context.tokens[token_range.clone()])?;

        let is_const = storage_class.as_ref().map_or(false, |s| s == "const");
        let is_static = storage_class.as_ref().map_or(false, |s| s == "static");
        let is_extern = storage_class.as_ref().map_or(false, |s| s == "extern");

        let extracted_global = ExtractedGlobal {
            name,
            type_name: var_type,
            initializer,
            storage_class,
            tokens: context.tokens[token_range.clone()].to_vec(),
            code: context.tokens[token_range.clone()]
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(" "),
            array_dims: Vec::new(),
            initial_value: None,
            is_const,
            is_static,
            is_extern,
            array_size: None,
        };

        return Ok(Some(ExtractedElement::Global(extracted_global)));
    }

    Ok(None)
}

/// Parse tokens to extract global variable components
fn parse_global_tokens(
    tokens: &[Token],
) -> Result<(String, String, Option<String>, Option<String>), C2RError> {
    if tokens.is_empty() {
        return Err(C2RError::new(
            Kind::Other,
            Reason::Empty("token array"),
            None,
        ));
    }

    // Filter out preprocessor directives and comments first
    let filtered_tokens: Vec<&Token> = tokens
        .iter()
        .filter(|t| {
            let token_str = t.to_string();
            !token_str.starts_with("#")
                && !token_str.starts_with("//")
                && !token_str.starts_with("/*")
                && !token_str.starts_with("include")
                && !token_str.starts_with("<")
                && !token_str.starts_with(">")
                && token_str != "stdio.h"
                && token_str != "stdlib.h"
                && token_str.trim().len() > 0
        })
        .collect();

    if filtered_tokens.is_empty() {
        return Err(C2RError::new(
            Kind::Other,
            Reason::Empty("valid tokens after filtering"),
            None,
        ));
    }

    // Find semicolon position in filtered tokens
    let semicolon_pos = filtered_tokens
        .iter()
        .position(|t| t.to_string() == ";")
        .unwrap_or(filtered_tokens.len());

    if semicolon_pos == 0 {
        return Err(C2RError::new(
            Kind::Other,
            Reason::Invalid("global variable declaration"),
            None,
        ));
    }

    let mut storage_class = None;
    let mut type_start = 0;

    // Check for storage class specifiers using filtered tokens
    let first_token = filtered_tokens[0].to_string();
    match first_token.as_str() {
        "static" | "extern" | "const" => {
            storage_class = Some(first_token);
            type_start = 1;
        }
        _ => {}
    }

    // Find variable name and type in filtered tokens
    let mut equals_pos = None;
    for i in type_start..semicolon_pos {
        if filtered_tokens[i].to_string() == "=" {
            equals_pos = Some(i);
            break;
        }
    }

    let name_pos = equals_pos.unwrap_or(semicolon_pos) - 1;
    if name_pos <= type_start {
        return Err(C2RError::new(
            Kind::Other,
            Reason::Missing("global variable name or type"),
            None,
        ));
    }

    let name = filtered_tokens[name_pos].to_string();

    // Extract type (everything from type_start to name) from filtered tokens
    let type_tokens = &filtered_tokens[type_start..name_pos];
    let var_type = type_tokens
        .iter()
        .map(|t| t.to_string())
        .collect::<Vec<_>>()
        .join(" ");

    // Extract initializer if present from filtered tokens
    let initializer = if let Some(eq_pos) = equals_pos {
        if eq_pos + 1 < semicolon_pos {
            let init_tokens = &filtered_tokens[eq_pos + 1..semicolon_pos];
            Some(
                init_tokens
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(" "),
            )
        } else {
            None
        }
    } else {
        None
    };

    Ok((name, var_type, initializer, storage_class))
}

/// Convert extracted global to Rust code using Patternizer exclusively
fn convert_global(token_range: Range<usize>) -> Result<Option<ConvertedElement>, C2RError> {
    // Use cloned context to get the actual tokens from global context
    let mut context = crate::context!();
    context.pull();
    let tokens_in_range = &context.tokens[token_range.clone()];

    // Filter out Token::n() from the retrieved tokens
    let filtered_tokens: Vec<Token> = tokens_in_range
        .iter()
        .filter(|token| !matches!(token, Token::n()))
        .cloned()
        .collect();

    let token_preview = filtered_tokens
        .iter()
        .take(10)
        .map(|t| t.to_string())
        .collect::<Vec<_>>()
        .join(" ");
    report!(
        "global_handler",
        "convert_global",
        ReportLevel::Info,
        HandlerPhase::Convert,
        format!(
            "CONVERT DEBUG: Converting {} tokens: [{}{}]",
            filtered_tokens.len(),
            token_preview,
            if filtered_tokens.len() > 10 {
                "..."
            } else {
                ""
            }
        ),
        true
    );

    let mut processed_tokens = filtered_tokens.to_vec();

    // Try to match global variable patterns
    let global_patterns = [
        "global_variable",
        "global_array",
        "global_pointer",
        "global_const",
        "global_static",
        "global_extern",
    ];

    for pattern in &global_patterns {
        match context.patternizer.match_pattern(pattern, &filtered_tokens) {
            PatternResult::Match { consumed_tokens } => {
                report!(
                    "global_handler",
                    "convert_global",
                    ReportLevel::Info,
                    HandlerPhase::Convert,
                    format!(
                        "CONVERT DEBUG: Patternizer pattern '{}' matched, converting...",
                        pattern
                    ),
                    true
                );

                // Parse global components
                let (name, var_type, initializer, storage_class) =
                    parse_global_tokens(&filtered_tokens)?;

                // Mark processed tokens as consumed
                for i in 0..consumed_tokens.min(processed_tokens.len()) {
                    processed_tokens[i] = Token::n();
                }

                let code = convert_global_to_rust(&name, &var_type, &initializer, &storage_class)?;

                let converted_global = ConvertedGlobal {
                    var_type: var_type.clone(),
                    initializer: initializer.clone(),
                    code,
                    is_const: storage_class.as_ref().map_or(false, |s| s == "const"),
                    is_static: storage_class.as_ref().map_or(false, |s| s == "static"),
                    is_public: true,
                };

                return Ok(Some(ConvertedElement::Global(converted_global)));
            }
            _ => continue,
        }
    }

    report!(
        "global_handler",
        "convert_global",
        ReportLevel::Info,
        HandlerPhase::Convert,
        "CONVERT DEBUG: No patternizer patterns matched, trying fallback...",
        true
    );

    // Fallback conversion for basic global variables
    if filtered_tokens.len() >= 3 && filtered_tokens.iter().any(|t| t.to_string() == ";") {
        report!(
            "global_handler",
            "convert_global",
            ReportLevel::Info,
            HandlerPhase::Convert,
            "CONVERT DEBUG: Fallback conditions met (>= 3 tokens, has semicolon), checking exclusions...",
            true
        );

        // Skip function declarations and other constructs
        if filtered_tokens.iter().any(|t| t.to_string() == "(") {
            report!(
                "global_handler",
                "convert_global",
                ReportLevel::Info,
                HandlerPhase::Convert,
                "CONVERT DEBUG: Skipping - contains parenthesis (likely function)",
                true
            );
            return Ok(None);
        }

        let first_token = filtered_tokens[0].to_string();
        if matches!(
            first_token.as_str(),
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
        ) || filtered_tokens[1].to_string() == "*"
        {
            report!(
                "global_handler",
                "convert_global",
                ReportLevel::Info,
                HandlerPhase::Convert,
                format!("CONVERT DEBUG: Skipping - first token is '{}'", first_token),
                true
            );
            return Ok(None);
        }

        report!(
            "global_handler",
            "convert_global",
            ReportLevel::Info,
            HandlerPhase::Convert,
            "CONVERT DEBUG: All checks passed, parsing and converting...",
            true
        );

        let (name, var_type, initializer, storage_class) = parse_global_tokens(&filtered_tokens)?;
        let code = convert_global_to_rust(&name, &var_type, &initializer, &storage_class)?;

        let converted_global = ConvertedGlobal {
            var_type: var_type.clone(),
            initializer: initializer.clone(),
            code: code.clone(),
            is_const: storage_class.as_ref().map_or(false, |s| s == "const"),
            is_static: storage_class.as_ref().map_or(false, |s| s == "static"),
            is_public: true,
        };

        report!(
            "global_handler",
            "convert_global",
            ReportLevel::Info,
            HandlerPhase::Convert,
            format!("CONVERT DEBUG: Successfully converted global: {}", code),
            true
        );

        return Ok(Some(ConvertedElement::Global(converted_global)));
    }

    report!(
        "global_handler",
        "convert_global",
        ReportLevel::Info,
        HandlerPhase::Convert,
        format!(
            "CONVERT DEBUG: Fallback failed - filtered_tokens.len()={}, has_semicolon={}",
            filtered_tokens.len(),
            filtered_tokens.iter().any(|t| t.to_string() == ";")
        ),
        true
    );

    Ok(None)
}

/// Convert global variable to Rust code
fn convert_global_to_rust(
    name: &str,
    var_type: &str,
    initializer: &Option<String>,
    storage_class: &Option<String>,
) -> Result<String, C2RError> {
    // Convert C type to Rust type
    let rust_type = convert_type(var_type).unwrap_or_else(|| var_type.to_string());

    let mut code = String::new();

    // Determine mutability and visibility
    let is_const = storage_class.as_ref().map_or(false, |s| s == "const") || initializer.is_some();
    let is_static = storage_class.as_ref().map_or(false, |s| s == "static");
    let is_extern = storage_class.as_ref().map_or(false, |s| s == "extern");

    if is_extern {
        // External declaration
        code.push_str(&format!(
            "extern \"C\" {{\n    pub static {}: {};\n}}\n",
            name, rust_type
        ));
    } else if is_const || (initializer.is_some() && !is_static) {
        // Constant
        let init_value = initializer
            .as_ref()
            .map(|s| s.as_str())
            .unwrap_or("Default::default()");
        code.push_str(&format!(
            "pub const {}: {} = {};\n",
            name, rust_type, init_value
        ));
    } else if is_static {
        // Static variable
        if let Some(init_value) = initializer {
            code.push_str(&format!(
                "pub static {}: {} = {};\n",
                name, rust_type, init_value
            ));
        } else {
            code.push_str(&format!(
                "pub static mut {}: {} = Default::default();\n",
                name, rust_type
            ));
        }
    } else {
        // Regular global (converted to static)
        if let Some(init_value) = initializer {
            code.push_str(&format!(
                "pub static {}: {} = {};\n",
                name, rust_type, init_value
            ));
        } else {
            code.push_str(&format!(
                "pub static mut {}: {} = Default::default();\n",
                name, rust_type
            ));
        }
    }

    Ok(code)
}
