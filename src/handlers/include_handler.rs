use crate::Kind;
use crate::Reason;
use crate::error::C2RError;
use crate::extract::ExtractedElement;
use crate::extract::ExtractedInclude;
use crate::handler::HandlerResult;
use crate::{
    ConvertedElement, ConvertedInclude, ElementInfo, Id, IncludeInfo, Token, context, report,
};
use crate::{
    HandlerPhase::{Handle, Process, Report},
    HandlerReport,
    ReportLevel::{Error, Info, Warning},
};
use std::ops::Range;
use std::path::{Path, PathBuf};

/// Creates an include handler that can detect and convert C include directives
pub fn create_include_handler() -> crate::handler::Handler {
    let handler_id = Id::get("include_handler");
    let handler_role = "include";
    let priority = 10; // Very high priority

    super::create_handler(
        handler_id,
        handler_role,
        priority,
        Some(process_include),
        Some(handle_include),
        Some(extract_include),
        Some(convert_include),
        Some(document_include),
        Some(report_include),
        Some(result_include),
        Some(redirect_include),
    )
}

/// Report callback: Collects and summarizes all include-related reports from the context
pub fn report_include() -> Result<HandlerReport, C2RError> {
    let context = context!();
    let reports = context.get_reports_by_handler("include");

    let (info_count, warning_count, error_count) =
        reports
            .iter()
            .fold((0, 0, 0), |acc, report| match report.level {
                Error => (acc.0, acc.1, acc.2 + 1),
                Warning => (acc.0, acc.1 + 1, acc.2),
                _ => (acc.0 + 1, acc.1, acc.2),
            });

    Ok(HandlerReport {
        report_id: Box::new(Id::get(&Id::gen_name("include_handler"))),
        handler_id: Box::new(Id::get("include_handler")),
        handler_name: "include".to_string(),
        function_name: "report_include".to_string(),
        message: format!(
            "Include handler summary: {} reports ({} info, {} warnings, {} errors)",
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

/// Result callback: Postprocesses generated code, adds documentation, and cleans up the result
fn result_include(
    token_range: Range<usize>,
    result: HandlerResult,
) -> Result<HandlerResult, C2RError> {
    let _id = Id::get("result_include");

    report!(
        "include_handler",
        "result_include",
        Info,
        Report,
        "Postprocessing include conversion result",
        true
    );

    if let HandlerResult::Completed(tokens_opt, _, code, id) = result {
        let mut context = crate::context!();
        context.pull();
        let include_info = extract_include_info(&context.tokens[token_range.clone()]);
        let doc_comment =
            document_include(ElementInfo::Include(include_info.clone()))?.unwrap_or_default();

        let enhanced_code = format!(
            "// [C2R] Include converted from C to Rust - {}: {}\n{}{}\n{}",
            include_info.path,
            if include_info.is_system_include {
                "system"
            } else {
                "local"
            },
            if doc_comment.trim().is_empty() {
                ""
            } else {
                &doc_comment
            },
            if doc_comment.trim().is_empty() {
                ""
            } else {
                "\n"
            },
            code
        );

        report!(
            "include_handler",
            "result_include",
            Info,
            Report,
            &format!(
                "Enhanced include conversion: {} - {}",
                include_info.path,
                if include_info.is_system_include {
                    "system"
                } else {
                    "local"
                }
            ),
            true
        );

        Ok(HandlerResult::Completed(
            tokens_opt,
            token_range,
            enhanced_code,
            id,
        ))
    } else {
        Ok(result)
    }
}

/// Helper function to extract include path from tokens
fn extract_include_info(tokens: &[Token]) -> IncludeInfo {
    if tokens.len() < 3 || tokens[0].to_string() != "#" || tokens[1].to_string() != "include" {
        return IncludeInfo {
            name: "".to_string(),
            path: "unknown_include".to_string(),
            is_system_include: false,
            is_local_include: false,
        };
    }

    // Handle system includes: <file.h>
    if let (Some(left), Some(right)) = (
        tokens.iter().position(|t| t.to_string() == "<"),
        tokens.iter().position(|t| t.to_string() == ">"),
    ) {
        if left < right && right > left + 1 {
            let path = tokens[left + 1..right]
                .iter()
                .map(|t| t.to_string())
                .collect::<String>();
            return IncludeInfo {
                name: path.clone(),
                path,
                is_system_include: true,
                is_local_include: false,
            };
        }
    }

    // Handle quoted includes: "file.h"
    for token in &tokens[2..] {
        let token_str = token.to_string();
        if token_str.starts_with('"') && token_str.ends_with('"') {
            let path = token_str.trim_matches('"').to_string();
            return IncludeInfo {
                name: path.clone(),
                path,
                is_system_include: false,
                is_local_include: true,
            };
        }
    }

    // Handle separate quote tokens: #include " file.h "
    let quotes: Vec<usize> = tokens
        .iter()
        .enumerate()
        .skip(2)
        .filter_map(|(idx, token)| {
            if token.to_string() == "\"" {
                Some(idx)
            } else {
                None
            }
        })
        .collect();

    if quotes.len() >= 2 {
        let path = tokens[quotes[0] + 1..quotes[1]]
            .iter()
            .map(|t| t.to_string())
            .collect::<String>();
        return IncludeInfo {
            name: path.clone(),
            path,
            is_system_include: false,
            is_local_include: true,
        };
    }

    // Fallback
    if tokens.len() > 2 {
        let token_str = tokens[2].to_string();
        let path = token_str
            .trim_matches('"')
            .trim_matches('<')
            .trim_matches('>')
            .to_string();
        let is_system = token_str.contains('<') || token_str.contains('>');
        IncludeInfo {
            name: path.clone(),
            path,
            is_system_include: is_system,
            is_local_include: !is_system,
        }
    } else {
        IncludeInfo {
            name: "".to_string(),
            path: "unknown_include".to_string(),
            is_system_include: false,
            is_local_include: false,
        }
    }
}

/// Helper function to generate documentation for the include
fn document_include(info: ElementInfo) -> Result<Option<String>, C2RError> {
    if let ElementInfo::Include(include_info) = info {
        let mapping = if include_info.is_system_include {
            match include_info.path.as_str() {
                "stdio.h" => "std::io and std::io::prelude",
                "stdlib.h" => "std::mem, std::ptr, libc",
                "string.h" => "std::ffi::CStr, std::ffi::CString, std::str",
                "math.h" => "std::f64::consts, libc",
                "time.h" => "std::time",
                _ => "libc (generic system include)",
            }
        } else {
            let module_name = include_info
                .path
                .strip_suffix(".h")
                .unwrap_or(&include_info.path);
            return Ok(Some(format!(
                "/// Converted C include: {}\n/// Path: {}\n/// Type: Local include\n/// Mapped to: mod {} and use crate::{}::*\n",
                include_info.name, include_info.path, module_name, module_name
            )));
        };

        let doc = format!(
            "/// Converted C include: {}\n/// Path: {}\n/// Type: System include\n/// Mapped to: {}\n",
            include_info.name, include_info.path, mapping
        );

        report!(
            "include_handler",
            "document_include",
            Info,
            Report,
            format!(
                "Generated documentation for include '{}'",
                include_info.path
            ),
            true
        );

        Ok(Some(doc))
    } else {
        Ok(None)
    }
}

/// Process callback: Lightweight include directive detection
fn process_include(token_range: Range<usize>) -> Result<bool, C2RError> {
    let mut context = crate::context!();
    context.pull();

    // Add bounds checking to prevent slice index out of bounds
    if token_range.end > context.tokens.len() || token_range.start >= context.tokens.len() {
        return Ok(false);
    }

    if token_range.start >= context.tokens[token_range.clone()].len()
        || context.tokens[token_range.clone()].len() < 2
    {
        return Ok(false);
    }

    // Use lightweight patterns to detect potential include starts
    let range_tokens = &context.tokens[token_range.clone()];

    report!(
        "include_handler",
        "process_include",
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

    // Quick heuristic: look for # include pattern
    if range_tokens.len() >= 2 {
        let potential_include = detect_include_start(range_tokens);
        if potential_include {
            report!(
                "include_handler",
                "process_include",
                Info,
                Process,
                "Detected potential include directive - delegating to extract for expansion",
                true
            );
            return Ok(true);
        }
    }

    Ok(false)
}

/// Lightweight include start detection
fn detect_include_start(tokens: &[Token]) -> bool {
    if tokens.len() < 2 {
        return false;
    }

    // Look for patterns like: # include
    tokens[0].to_string() == "#" && tokens[1].to_string() == "include"
}

/// Handle callback: Uses extract callback for intelligent window expansion
fn handle_include(token_range: Range<usize>) -> Result<HandlerResult, C2RError> {
    let id = Id::get("include_handler");

    report!(
        "include_handler",
        "handle_include",
        Info,
        Handle,
        format!(
            "Handle include called for range {}..{}, delegating to extract callback",
            token_range.start, token_range.end
        ),
        true
    );

    // Use the extract callback for intelligent window expansion and processing
    if let Some(extracted_element) = extract_include(token_range.clone())? {
        match extracted_element {
            ExtractedElement::Include(include) => {
                report!(
                    "include_handler",
                    "handle_include",
                    Info,
                    Handle,
                    format!(
                        "Successfully extracted include: {} via intelligent expansion",
                        include.path
                    ),
                    true
                );

                // Calculate the actual tokens consumed by looking at the include's token range
                let tokens_consumed = include.tokens.len();
                let consumed_range = token_range.start..token_range.start + tokens_consumed;

                // Convert the extracted include to get the final code
                match convert_include(token_range.clone())? {
                    Some(ConvertedElement::Include(converted_include)) => {
                        let code = converted_include.code.clone();
                        Ok(HandlerResult::Converted(
                            ConvertedElement::Include(converted_include),
                            consumed_range,
                            code,
                            id,
                        ))
                    }
                    _ => Ok(HandlerResult::Extracted(
                        ExtractedElement::Include(include),
                        consumed_range,
                        "Include extracted via recursive windowing".to_string(),
                        id,
                    )),
                }
            }
            _ => {
                report!(
                    "include_handler",
                    "handle_include",
                    Info,
                    Handle,
                    "Extract callback returned non-include element".to_string(),
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
            "include_handler",
            "handle_include",
            Info,
            Handle,
            "Extract callback could not extract include from expanded window".to_string(),
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

/// Helper function to parse include path from tokens
fn parse_include_path(tokens: &[Token]) -> Result<(String, bool), C2RError> {
    // Handle <file.h> format
    if tokens.len() >= 5 && tokens[2].to_string() == "<" && tokens[4].to_string() == ">" {
        return Ok((tokens[3].to_string(), true));
    }

    // Handle "file.h" format (single token)
    let token2 = tokens[2].to_string();
    if token2.starts_with("\"") && token2.ends_with("\"") {
        return Ok((token2.trim_matches('"').to_string(), false));
    }

    // Handle " file.h " format (separate tokens)
    if tokens.len() >= 5 && token2 == "\"" && tokens[4].to_string() == "\"" {
        return Ok((tokens[3].to_string(), false));
    }

    Err(C2RError::new(
        Kind::Other,
        Reason::Unknown("include format"),
        None,
    ))
}

/// Extract callback: Intelligent include extraction with recursive window expansion
pub fn extract_include(token_range: Range<usize>) -> Result<Option<ExtractedElement>, C2RError> {
    let mut context = crate::context!();
    context.pull();
    if token_range.start >= context.tokens[token_range.clone()].len() {
        return Ok(None);
    }

    // Step 1: Find the actual include start within the range
    let include_start =
        find_include_start_in_range(&context.tokens[token_range.clone()], &token_range)?;
    if include_start.is_none() {
        return Ok(None);
    }
    let start_pos = include_start.unwrap();

    // Step 2: Recursively expand window to capture complete include directive
    let expanded_range = expand_to_include(&context.tokens[token_range.clone()], start_pos)?;
    if expanded_range.is_none() {
        return Ok(None);
    }
    let complete_range = expanded_range.unwrap();

    report!(
        "include_handler",
        "extract_include",
        Info,
        Process,
        format!(
            "Expanded window from {}..{} to {}..{} to capture complete include",
            token_range.start, token_range.end, complete_range.start, complete_range.end
        ),
        true
    );

    // Step 3: Process the complete include directive
    let complete_tokens = &context.tokens[complete_range.clone()];

    // Validate it's actually an include directive
    if complete_tokens.len() < 3
        || complete_tokens[0].to_string() != "#"
        || complete_tokens[1].to_string() != "include"
    {
        return Ok(None);
    }

    let (include_path, is_system_include) = match parse_include_path(complete_tokens) {
        Ok(result) => result,
        Err(_) => {
            report!(
                "include_handler",
                "extract_include",
                Info,
                Process,
                "Failed to parse include path from complete tokens",
                true
            );
            return Ok(None);
        }
    };

    let extracted_include = ExtractedInclude {
        path: include_path.clone(),
        tokens: complete_tokens.to_vec(),
        is_system_include,
        code: complete_tokens
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<_>>()
            .join(" "),
    };

    report!(
        "include_handler",
        "extract_include",
        Info,
        Process,
        format!("Successfully extracted include: {}", include_path),
        true
    );

    Ok(Some(ExtractedElement::Include(extracted_include)))
}

/// Find include start position within the given range
fn find_include_start_in_range(
    tokens: &[Token],
    range: &Range<usize>,
) -> Result<Option<usize>, C2RError> {
    let safe_end = std::cmp::min(range.end, tokens.len());
    let safe_range = range.start..safe_end;
    if safe_range.start >= tokens.len() || safe_range.is_empty() {
        return Ok(None);
    }
    let range_tokens = &tokens[safe_range.clone()];

    for (i, window) in range_tokens.windows(2).enumerate() {
        if detect_include_start(window) {
            return Ok(Some(range.start + i));
        }
    }

    Ok(None)
}

/// Recursively expand window to capture complete include directive starting with minimum viable tokens
fn expand_to_include(tokens: &[Token], start_pos: usize) -> Result<Option<Range<usize>>, C2RError> {
    if start_pos >= tokens.len() || start_pos + 1 >= tokens.len() {
        return Ok(None);
    }

    // Validate we have # include at the start
    if tokens[start_pos].to_string() != "#" || tokens[start_pos + 1].to_string() != "include" {
        return Ok(None);
    }

    // Constants for minimum viable include tokens
    const MIN_INCLUDE_TOKENS: usize = 5; // # include < stdio.h >

    // Step 1: Start with minimum include window (5 tokens)
    let mut current_end = (start_pos + MIN_INCLUDE_TOKENS).min(tokens.len());

    // Step 2: Check if we have a complete include in minimum window
    if current_end <= tokens.len() {
        let window_tokens = &tokens[start_pos..current_end];
        if is_complete_include_directive(window_tokens) {
            return Ok(Some(start_pos..current_end));
        }
    }

    // Step 3: Progressive expansion if minimum window is incomplete
    for expand_size in 1..=10 {
        current_end = (start_pos + MIN_INCLUDE_TOKENS + expand_size).min(tokens.len());
        if current_end >= tokens.len() {
            break;
        }

        let window_tokens = &tokens[start_pos..current_end];
        if is_complete_include_directive(window_tokens) {
            return Ok(Some(start_pos..current_end));
        }
    }

    // Fallback: use minimum viable window
    let final_end = (start_pos + MIN_INCLUDE_TOKENS).min(tokens.len());
    Ok(Some(start_pos..final_end))
}

/// Check if token sequence forms a complete include directive
fn is_complete_include_directive(tokens: &[Token]) -> bool {
    if tokens.len() < 5 {
        return false;
    }

    // Must start with # include
    if tokens[0].to_string() != "#" || tokens[1].to_string() != "include" {
        return false;
    }

    // Check for < header.h > pattern
    if tokens.len() >= 5 && tokens[2].to_string() == "<" && tokens[4].to_string() == ">" {
        return true;
    }

    // Check for "header.h" pattern (single token)
    if tokens.len() >= 3
        && tokens[2].to_string().starts_with('"')
        && tokens[2].to_string().ends_with('"')
    {
        return true;
    }

    // Check for " header.h " pattern (separate tokens)
    if tokens.len() >= 5 && tokens[2].to_string() == "\"" && tokens[4].to_string() == "\"" {
        return true;
    }

    false
}

/// Converts an include directive as a ConvertedElement
#[allow(unused_variables)]
pub fn convert_include(token_range: Range<usize>) -> Result<Option<ConvertedElement>, C2RError> {
    let _id = Id::get("convert_include");

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

    if filtered_tokens.len() < 3
        || filtered_tokens[0].to_string() != "#"
        || filtered_tokens[1].to_string() != "include"
    {
        return Ok(None);
    }

    let include_info = extract_include_info(&filtered_tokens);
    if include_info.path.is_empty() || include_info.path == "unknown_include" {
        return Ok(None);
    }

    let code = if include_info.is_system_include {
        generate_system_include_code(&include_info.path)
    } else {
        generate_local_include_code(&include_info.path)
    };

    Ok(Some(ConvertedElement::Include(ConvertedInclude {
        path: include_info.path,
        code,
        is_external_crate: false,
    })))
}

fn generate_system_include_code(path: &str) -> String {
    match path {
        "stdio.h" => "// C stdio.h -> Rust std::io\n\
             use std::io;\n\
             use std::io::prelude::*;\n"
            .to_string(),
        "stdlib.h" => "// C stdlib.h -> Rust std and libc\n\
             use std::mem;\n\
             use std::ptr;\n\
             use libc;\n"
            .to_string(),
        "string.h" => "// C string.h -> Rust str, String, and CStr\n\
             use std::ffi::{CStr, CString};\n\
             use std::str;\n\
             use libc;\n"
            .to_string(),
        "math.h" => "// C math.h -> Rust std::f64 and std::f32\n\
             use std::f64::consts;\n\
             use libc;\n"
            .to_string(),
        "time.h" => "// C time.h -> Rust std::time\n\
             use std::time::{Duration, SystemTime};\n\
             use libc;\n"
            .to_string(),
        "stdbool.h" => "// C stdbool.h is not needed in Rust as bool is built-in\n".to_string(),
        "stdint.h" => "// C stdint.h fixed width integers are built into Rust\n".to_string(),
        "stddef.h" => "// C stddef.h -> Various Rust std types\n\
             use std::{ptr, mem};\n\
             use libc;\n"
            .to_string(),
        "pthread.h" => "// C pthread.h -> Rust std::thread\n\
             use std::thread;\n\
             use std::sync::{Arc, Mutex, Condvar};\n\
             use libc;\n"
            .to_string(),
        "errno.h" => "// C errno.h -> Rust std::io::Error\n\
             use std::io::{Error as IoError, ErrorKind};\n\
             use libc;\n"
            .to_string(),
        "assert.h" => "// C assert.h -> Rust assert! macro\n".to_string(),
        "ctype.h" => "// C ctype.h -> Rust char methods\n\
             use libc;\n"
            .to_string(),
        "signal.h" => "// C signal.h -> Rust signal crate\n\
             use libc;\n\
             // You may need to add: signal = \"0.7\" to your Cargo.toml\n"
            .to_string(),
        _ => {
            format!(
                "// C include: <{}>\n\
                 use libc;\n\
                 // TODO: Map <{}> to appropriate Rust imports\n",
                path, path
            )
        }
    }
}

fn generate_local_include_code(path: &str) -> String {
    let module_name = path.strip_suffix(".h").unwrap_or(path);
    let rust_module = to_snake_case(module_name);

    let mut code = format!("// C include: \"{}\"\n", path);
    code.push_str(&format!("mod {};\n", rust_module));
    code.push_str(&format!("use {}::*;\n", rust_module));

    if context!().get_feature("process_includes").unwrap_or(false) {
        let include_dirs = context!().get_include_dirs();
        if let Some(file_path) = find_include_file(path.trim(), &include_dirs) {
            code.push_str(&format!(
                "// TODO: Process included file: {:?}\n",
                file_path
            ));
        }
    }

    code
}

/// Convert callback: Does the actual conversion of C to Rust code

/// Redirect callback: Handles cases where this handler should pass tokens to a different handler
fn redirect_include(
    token_range: Range<usize>,
    result: HandlerResult,
) -> Result<HandlerResult, C2RError> {
    let id = Id::get("redirect_include");

    // Only redirect if this is a preprocessor directive that's not #include
    let mut context = crate::context!();
    context.pull();
    if context.tokens[token_range.clone()].len() >= 2
        && context.tokens[token_range.clone()][0].to_string() == "#"
    {
        let directive = context.tokens[token_range.clone()][1].to_string();

        if directive != "include" {
            report!(
                "include_handler",
                "redirect_include",
                Info,
                Report,
                format!("Redirecting #{} directive to macro handler", directive),
                true
            );

            return Ok(HandlerResult::Redirected(
                Some(context.tokens[token_range.clone()].to_vec()),
                token_range,
                "include_handler".to_string(),
                id,
                Id::get("macro_handler"),
            ));
        }
    }

    // Malformed preprocessor directive
    if context.tokens[token_range.clone()].len() == 1
        && context.tokens[token_range.clone()][0].to_string() == "#"
    {
        report!(
            "include_handler",
            "redirect_include",
            Info,
            Report,
            "Malformed preprocessor directive, redirecting to macro handler",
            true
        );

        return Ok(HandlerResult::Redirected(
            Some(context.tokens[token_range.clone()].to_vec()),
            token_range,
            "include_handler".to_string(),
            id,
            Id::get("macro_handler"),
        ));
    }

    Ok(result)
}

/// Find an include file in the include directories
fn find_include_file(path: &str, include_dirs: &[PathBuf]) -> Option<PathBuf> {
    // First try direct path
    let direct_path = Path::new(path);
    if direct_path.exists() {
        return Some(direct_path.to_path_buf());
    }

    // Then try in each include directory
    for dir in include_dirs {
        let file_path = dir.join(path);
        if file_path.exists() {
            return Some(file_path);
        }
    }

    None
}

/// Convert a string to snake_case
fn to_snake_case(s: &str) -> String {
    let mut result = String::new();
    let mut prev_is_lowercase = false;

    for c in s.chars() {
        if c.is_uppercase() {
            if prev_is_lowercase {
                result.push('_');
            }
            result.push(c.to_lowercase().next().unwrap());
            prev_is_lowercase = false;
        } else if c == '-' || c == ' ' {
            result.push('_');
            prev_is_lowercase = false;
        } else {
            result.push(c);
            prev_is_lowercase = c.is_lowercase();
        }
    }

    result
}
