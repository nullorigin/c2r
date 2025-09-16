//! Include Directive Handler for C to Rust Converter
//! Specialized handler for #include preprocessor directives

use crate::convert::ConvertedMacro;
use crate::pattern::PatternResult;
use crate::{
    C2RError, ConvertedElement, ExtractedElement, Handler, HandlerPhase, HandlerResult, Id,
    ReportLevel,
};
use crate::{context, report};
use std::ops::Range;

/// Create include directive handler with specialized patterns
pub fn create_include_directive_handler() -> Handler {
    let handler_id = Id::get("include_directive_handler");
    let handler_role =
        "Convert C #include directives to Rust use statements or extern crate".to_string();
    let priority = 950; // Very high priority for specific pattern matching

    let mut handler = Handler::new(handler_id, handler_role, priority);
    handler.process = Some(process_include_directive);
    handler.handle = Some(handle_include_directive);
    handler.extract = Some(extract_include_directive);
    handler.convert = Some(convert_include_directive_wrapper);
    handler.document = Some(document_include_directive);
    handler.result = Some(result_include_directive);
    handler
}

/// Process callback: Uses Patternizer to detect #include directives
fn process_include_directive(token_range: Range<usize>) -> Result<bool, C2RError> {
    let mut context = context!();
    context.pull();

    if token_range.end > context.tokens.len() || token_range.start >= context.tokens.len() {
        return Ok(false);
    }

    let tokens = &context.tokens[token_range.clone()];

    if tokens.len() < 2 {
        return Ok(false);
    }

    // Use the existing Patternizer architecture
    let pattern_result = context
        .patternizer
        .match_pattern("include_directive", tokens);

    Ok(matches!(
        pattern_result,
        PatternResult::Match { .. } | PatternResult::Sequence { .. } | PatternResult::Fuzzy { .. }
    ))
}

/// Handle callback: Processes #include directives
fn handle_include_directive(token_range: Range<usize>) -> Result<HandlerResult, C2RError> {
    let id = Id::get("include_directive_handler");

    report!(
        "include_directive_handler",
        "handle_include_directive",
        ReportLevel::Info,
        HandlerPhase::Handle,
        format!("Processing #include directive in range {:?}", token_range),
        true
    );

    if let Ok(Some(converted)) = convert_include_directive_wrapper(token_range.clone()) {
        if let ConvertedElement::Macro(rust_include) = converted {
            let mut context = context!();
            context.pull();
            return Ok(HandlerResult::Completed(
                Some(context.tokens[token_range.clone()].to_vec()),
                token_range,
                rust_include.code,
                id,
            ));
        }
    }

    Ok(HandlerResult::NotHandled(None, token_range, id))
}

/// Extract #include directive information from tokens
fn extract_include_directive(
    token_range: Range<usize>,
) -> Result<Option<ExtractedElement>, C2RError> {
    let mut context = context!();
    context.pull();
    let tokens = &context.tokens[token_range.clone()];

    if tokens.is_empty() {
        return Ok(None);
    }

    let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

    // Extract include path
    let (include_path, is_system_include) = if token_strings.get(0) == Some(&"#include".to_string())
    {
        // Combined #include token
        if let Some(path_token) = token_strings.get(1) {
            extract_include_path(path_token)
        } else {
            ("unknown".to_string(), false)
        }
    } else if token_strings.get(0) == Some(&"#".to_string())
        && token_strings.get(1) == Some(&"include".to_string())
    {
        // Separate # and include tokens
        if let Some(path_token) = token_strings.get(2) {
            extract_include_path(path_token)
        } else {
            ("unknown".to_string(), false)
        }
    } else {
        ("unknown".to_string(), false)
    };

    Ok(Some(ExtractedElement::Include(
        crate::extract::ExtractedInclude {
            path: include_path,
            tokens: tokens.to_vec(),
            is_system_include,
            code: token_strings.join(" "),
        },
    )))
}

/// Extract include path and determine if it's a system include
fn extract_include_path(path_token: &str) -> (String, bool) {
    if path_token.starts_with('<') && path_token.ends_with('>') {
        // System include: <stdio.h>
        let path = path_token
            .trim_start_matches('<')
            .trim_end_matches('>')
            .to_string();
        (path, true)
    } else if path_token.starts_with('"') && path_token.ends_with('"') {
        // Local include: "myheader.h"
        let path = path_token
            .trim_start_matches('"')
            .trim_end_matches('"')
            .to_string();
        (path, false)
    } else {
        // Malformed or unusual include
        (path_token.to_string(), false)
    }
}

/// Convert #include directives from token range
fn convert_include_directive_wrapper(
    token_range: Range<usize>,
) -> Result<Option<ConvertedElement>, C2RError> {
    let mut context = context!();
    context.pull();
    let tokens = &context.tokens[token_range.clone()];

    if tokens.is_empty() {
        return Ok(None);
    }

    let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

    // Extract include path
    let (include_path, is_system_include) = if token_strings.get(0) == Some(&"#include".to_string())
    {
        if let Some(path_token) = token_strings.get(1) {
            extract_include_path(path_token)
        } else {
            ("unknown".to_string(), false)
        }
    } else if token_strings.get(0) == Some(&"#".to_string())
        && token_strings.get(1) == Some(&"include".to_string())
    {
        if let Some(path_token) = token_strings.get(2) {
            extract_include_path(path_token)
        } else {
            ("unknown".to_string(), false)
        }
    } else {
        ("unknown".to_string(), false)
    };

    let rust_code = if is_system_include {
        // Convert system includes to appropriate Rust equivalents
        match include_path.as_str() {
            "stdio.h" => {
                "use std::io::{self, Write, BufRead, BufReader};\nuse std::fs::File;".to_string()
            }
            "stdlib.h" => "use std::process;\nuse std::env;".to_string(),
            "string.h" => {
                "// String functions available in Rust std::string and str methods".to_string()
            }
            "math.h" => {
                "use std::f64::consts;\n// Math functions available in Rust std::f64 and std::f32"
                    .to_string()
            }
            "time.h" => "use std::time::{SystemTime, Duration, Instant};".to_string(),
            "unistd.h" => "use std::process::Command;".to_string(),
            "pthread.h" => "use std::thread;\nuse std::sync::{Arc, Mutex, Condvar};".to_string(),
            _ => {
                format!(
                    "// System include: {} -> manual review needed for Rust equivalent",
                    include_path
                )
            }
        }
    } else {
        // Convert local includes to mod or use statements
        let module_name = include_path
            .strip_suffix(".h")
            .unwrap_or(&include_path)
            .replace('-', "_")
            .replace('/', "::");

        if include_path.ends_with(".h") {
            format!("mod {};\nuse {}::*;", module_name, module_name)
        } else {
            format!(
                "// Local include: {} -> mod {}; (review needed)",
                include_path, module_name
            )
        }
    };

    Ok(Some(ConvertedElement::Macro(ConvertedMacro {
        parameters: vec![include_path.clone()],
        body: format!(
            "#include {}",
            if is_system_include {
                format!("<{}>", include_path)
            } else {
                format!("\"{}\"", include_path)
            }
        ),
        is_function_like: false,
        code: rust_code,
    })))
}

/// Document #include directive
fn document_include_directive(
    info: crate::document::ElementInfo,
) -> Result<Option<String>, C2RError> {
    if let crate::document::ElementInfo::Include(include_info) = info {
        let mut doc = format!(
            "/// Converted C #include directive: {}\n",
            include_info.path
        );

        if include_info.is_system_include {
            doc.push_str("/// System include -> use std or external crate in Rust\n");
            doc.push_str(
                "/// Note: C standard library functions may have different Rust equivalents\n",
            );
        } else {
            doc.push_str("/// Local include -> mod declaration in Rust\n");
            doc.push_str("/// Note: Header files become modules in Rust\n");
        }

        Ok(Some(doc))
    } else {
        Ok(None)
    }
}

/// Result callback for #include directives
fn result_include_directive(
    _token_range: Range<usize>,
    result: HandlerResult,
) -> Result<HandlerResult, C2RError> {
    report!(
        "include_directive_handler",
        "result_include_directive",
        ReportLevel::Info,
        HandlerPhase::Report,
        "Postprocessing #include directive conversion result",
        true
    );

    Ok(result)
}
