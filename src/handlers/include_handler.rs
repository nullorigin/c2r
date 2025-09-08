use super::common::not_handled;
use crate::config::{
    HandlerPhase::{Handle, Process, Report},
    HandlerReport,
    ReportLevel::{Error, Info, Warning},
};
use crate::error::ConversionError;
use crate::extract::ExtractedElement;
use crate::extract::ExtractedInclude;
use crate::handler::HandlerResult;
use crate::macro_handler::{handle_define, handle_ifdef, handle_undef, process_macro};
use crate::{context, report, ConvertedElement, ConvertedInclude, Id, Token};
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
        Some(report_include),
        Some(result_include),
        Some(redirect_include),
    )
}

/// Report callback: Collects and summarizes all include-related reports from the context
fn report_include(
    _tokens: &[Token]) -> Result<HandlerReport, ConversionError> {
    // Get all reports for this handler
    let context = context!();
    let reports = context.get_reports_by_handler("include");

    // Count reports by level
    let mut info_count = 0;
    let mut warning_count = 0;
    let mut error_count = 0;

    for report in &reports {
        match report.level {
            Info => info_count += 1,
            Warning => warning_count += 1,
            Error => error_count += 1,
            _ => info_count += 1, // Handle Debug and other variants as info
        }
    }

    // Create summary report
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
        level: if error_count > 0 {
            Error
        } else if warning_count > 0 {
            Warning
        } else {
            Info
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
    tokens: &[Token],
    result: HandlerResult) -> Result<HandlerResult, ConversionError> {
    report!(
        "include_handler",
        "result_include",
        Info,
        Report,
        "Starting result postprocessing for include",
        true
    );

    match result {
        HandlerResult::Handled(Some(processed_tokens), _, id) => {
            let mut enhanced_result = processed_tokens.clone();

            // Extract include path for documentation
            let include_path = extract_include_path(tokens);

            // Generate documentation comment for the include conversion
            let doc_comment = generate_include_documentation(tokens, &include_path);

            // Add documentation as the first token if substantial
            if !doc_comment.trim().is_empty() {
                let doc_token = Token::s(doc_comment);
                enhanced_result.insert(0, doc_token);
            }

            // Postprocess the generated Rust code for better formatting
            enhanced_result = postprocess_include_code(enhanced_result);

            // Add metadata comment about the conversion
            let conversion_metadata = format!(
                "\n// Converted from C include: {}",
                tokens
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            );
            enhanced_result.push(Token::s(conversion_metadata));

            report!(
                "include_handler",
                "result_include",
                Info,
                Report,
                format!(
                    "Enhanced include result with documentation for '{}'",
                    include_path
                ),
                true
            );

            Ok(HandlerResult::Handled(Some(enhanced_result), 0..1, id))
        }
        HandlerResult::Handled(None, _, ref id) => {
            // Even without processed tokens, we can add some documentation
            let include_path = extract_include_path(tokens);
            let doc_comment = generate_include_documentation(tokens, &include_path);

            if !doc_comment.trim().is_empty() {
                let enhanced_tokens = vec![Token::s(doc_comment)];

                report!(
                    "include_handler",
                    "result_include",
                    Info,
                    Report,
                    format!("Added documentation for include '{}'", include_path),
                    true
                );

                Ok(HandlerResult::Handled(
                    Some(enhanced_tokens),
                    0..1,
                    id.clone(),
                ))
            } else {
                Ok(result)
            }
        }
        _ => Ok(result), // Pass through other result types unchanged
    }
}

/// Helper function to extract include path from tokens
fn extract_include_path(tokens: &[Token]) -> String {
    if tokens.len() < 3 || tokens[0].to_string() != "#" || tokens[1].to_string() != "include" {
        return "unknown_include".to_string();
    }

    // Handle system includes: <file.h>
    if let (Some(left), Some(right)) = (
        tokens.iter().position(|t| t.to_string() == "<"),
        tokens.iter().position(|t| t.to_string() == ">"),
    ) {
        if left < right && right > left + 1 {
            return tokens[left + 1..right]
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join("");
        }
    }

    // Handle quoted includes: "file.h"
    for token in &tokens[2..] {
        let token_str = token.to_string();
        if token_str.starts_with("\"") && token_str.ends_with("\"") {
            return token_str.trim_matches('"').to_string();
        }
    }

    // Fallback: return the third token (usually the filename)
    if tokens.len() > 2 {
        tokens[2]
            .to_string()
            .trim_matches('"')
            .trim_matches('<')
            .trim_matches('>')
            .to_string()
    } else {
        "unknown_include".to_string()
    }
}

/// Helper function to generate documentation for the include
fn generate_include_documentation(
    tokens: &[Token],
    include_path: &str) -> String {
    let mut doc = String::new();

    // Generate basic include documentation
    doc.push_str(&"/// Converted C include directive\n".to_string());
    doc.push_str("///\n");

    // Extract and document the original C include
    let c_include = tokens
        .iter()
        .map(|t| t.to_string())
        .collect::<Vec<_>>()
        .join(" ");
    doc.push_str(&format!("/// Original C include: {}\n", c_include));

    // Add specific information based on include type
    let is_system = tokens.iter().any(|t| t.to_string() == "<");
    if is_system {
        doc.push_str("/// Type: System include (mapped to Rust standard library)\n");

        // Add specific mapping information for common headers
        match include_path {
            "stdio.h" => doc.push_str("/// Mapped to: std::io and std::io::prelude\n"),
            "stdlib.h" => doc.push_str("/// Mapped to: std::mem, std::ptr, libc\n"),
            "string.h" => {
                doc.push_str("/// Mapped to: std::ffi::CStr, std::ffi::CString, std::str\n")
            }
            "math.h" => doc.push_str("/// Mapped to: std::f64::consts, libc\n"),
            "time.h" => doc.push_str("/// Mapped to: std::time\n"),
            _ => doc.push_str(&"/// Mapped to: libc (generic system include)\n".to_string()),
        }
    } else {
        doc.push_str("/// Type: Local include (mapped to Rust module)\n");
        let module_name = if include_path.ends_with(".h") {
            &include_path[0..include_path.len() - 2]
        } else {
            include_path
        };
        let rust_module = to_snake_case(module_name);
        doc.push_str(&format!(
            "/// Mapped to: mod {} and use {}::*\n",
            rust_module, rust_module
        ));
    }

    // Add note about conversion
    doc.push_str("///\n");
    doc.push_str("/// # Note\n");
    doc.push_str("/// This include directive was automatically converted from C.\n");
    doc.push_str("/// Please verify the mapping is correct for your use case.\n");

    report!(
        "include_handler",
        "generate_include_documentation",
        Info,
        Report,
        format!(
            "Generated {} lines of documentation for include '{}'",
            doc.lines().count(),
            include_path
        ),
        true
    );

    doc
}

/// Helper function to postprocess and clean up generated include code
fn postprocess_include_code(mut tokens: Vec<Token>) -> Vec<Token> {
    // Clean up formatting in the generated code
    for token in &mut tokens {
        let token_str = token.to_string();

        // Ensure proper line endings for use statements
        if token_str.starts_with("use ") && !token_str.ends_with(";\n") && !token_str.ends_with(";")
        {
            let cleaned = if token_str.ends_with('\n') {
                token_str.trim_end().to_string() + ";\n"
            } else {
                token_str.clone() + ";\n"
            };
            *token = Token::s(cleaned);
        }

        // Clean up comment formatting
        if token_str.starts_with("//") && !token_str.ends_with('\n') {
            let cleaned = token_str + "\n";
            *token = Token::s(cleaned);
        }
    }

    report!(
        "include_handler",
        "postprocess_include_code",
        Info,
        Report,
        format!(
            "Postprocessed {} tokens for better formatting",
            tokens.len()
        ),
        true
    );

    tokens
}

/// Process callback: Initializes and confirms this handler can handle the tokens
fn process_include(tokens: &[Token]) -> Result<bool, ConversionError> {
    // Validate input
    if tokens.len() < 2 {
        return Ok(false);
    }

    // Check for #include pattern
    if tokens[0].to_string() == "#" && tokens.len() >= 2 && tokens[1].to_string() == "include" {
        // Validate we have a filename
        if tokens.len() >= 3 {
            report!(
                "include_handler",
                "process_include",
                Info,
                Process,
                "Include directive detected",
                true
            );
            return Ok(true);
        }

        // Might be malformed include - let's try to handle it gracefully
        report!(
            "include_handler",
            "process_include",
            Info,
            Process,
            "Potentially malformed include directive detected",
            true
        );
        return Ok(true);
    }

    Ok(false)
}

/// Processes an include directive
fn handle_include(
    tokens: &[Token]) -> Result<HandlerResult, ConversionError> {
    report!(
        "include_handler",
        "handle_include",
        Info,
        Handle,
        "Include handler processing tokens",
        true
    );

    if tokens.len() < 3 || tokens[0].to_string() != "#" || tokens[1].to_string() != "include" {
        return not_handled();
    }

    let include_path: String;
    let is_system_include: bool;

    // Handle both <file.h> and "file.h" formats
    if tokens[2].to_string() == "<" && tokens.len() >= 5 && tokens[4].to_string() == ">" {
        // System include: #include <file.h>
        include_path = tokens[3].to_string();
        is_system_include = true;
    } else if tokens[2].to_string().starts_with("\"") && tokens[2].to_string().ends_with("\"") {
        // Local include with quotes in same token: #include "file.h"
        include_path = tokens[2].to_string().trim_matches('"').to_string();
        is_system_include = false;
    } else if tokens[2].to_string() == "\"" && tokens.len() >= 5 && tokens[4].to_string() == "\"" {
        // Local include with separate quote tokens: #include " file.h "
        include_path = tokens[3].to_string();
        is_system_include = false;
    } else {
        report!(
            "include_handler",
            "handle_include",
            Error,
            Handle,
            "Unrecognized include format",
            true
        );
        return not_handled();
    }

    report!(
        "include_handler",
        "handle_include",
        Info,
        Handle,
        format!(
            "Found include: {} (system: {})",
            include_path, is_system_include
        ),
        true
    );

    // Actually perform the conversion and return proper HandlerResult::Converted
    match convert_include(tokens)? {
        Some(ConvertedElement::Include(converted_include)) => {
            let rust_code = converted_include.rust_code.clone();
            Ok(HandlerResult::Converted(
                ConvertedElement::Include(converted_include),
                0..1,
                rust_code,
                Id::get("include_handler"),
            ))
        }
        _ => {
            report!(
                "include_handler",
                "handle_include",
                Error,
                Handle,
                "Failed to convert include directive",
                false
            );
            not_handled()
        }
    }
}

/// Extracts an include directive as an ExtractedElement
pub fn extract_include(
    tokens: &[Token]) -> Result<Option<ExtractedElement>, ConversionError> {
    if tokens.len() < 3 || tokens[0].to_string() != "#" || tokens[1].to_string() != "include" {
        return Ok(None);
    }

    let include_path: String;
    let is_system_include: bool;

    // Handle both <file.h> and "file.h" formats
    if tokens[2].to_string() == "<" && tokens.len() >= 5 && tokens[4].to_string() == ">" {
        // System include: #include <file.h>
        include_path = tokens[3].to_string();
        is_system_include = true;
    } else if tokens[2].to_string().starts_with("\"") && tokens[2].to_string().ends_with("\"") {
        // Local include with quotes in same token: #include "file.h"
        include_path = tokens[2].to_string().trim_matches('"').to_string();
        is_system_include = false;
    } else if tokens[2].to_string() == "\"" && tokens.len() >= 5 && tokens[4].to_string() == "\"" {
        // Local include with separate quote tokens: #include " file.h "
        include_path = tokens[3].to_string();
        is_system_include = false;
    } else {
        return Ok(None);
    }

    let extracted_include = ExtractedInclude {
        path: include_path,
        tokens: tokens.to_vec(),
        is_system_include,
        original_code: tokens
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<_>>()
            .join(" "),
    };

    Ok(Some(ExtractedElement::Include(extracted_include)))
}
pub fn convert_include(
    tokens: &[Token]) -> Result<Option<ConvertedElement>, ConversionError> {
    let id = Id::get("convert_include");
    let mut rust_code = String::new();
    let mut path: String = String::new();
    let mut is_system_include: bool = false;
    let left = tokens.iter().position(|t| t.to_string() == "<");
    let right = tokens.iter().position(|t| t.to_string() == ">");
    is_system_include = left.is_some() && right.is_some();
    let mut range = 0..0;
    if is_system_include {
        range.start = left.unwrap() + 1;
        range.end = right.unwrap();
    } else {
        // Handle quoted includes: "filename.h"
        let mut quote_count = 0;
        range.start = 0;
        range.end = 0;

        for (idx, token) in tokens.iter().enumerate() {
            if token.to_string() == "\"" {
                quote_count += 1;
                if quote_count == 1 {
                    range.start = idx + 1;
                } else if quote_count == 2 {
                    range.end = idx;
                    break;
                }
            }
        }

        // If we didn't find separate quote tokens, check for quoted string in single token
        if range.start == 0 && range.end == 0 {
            for (idx, token) in tokens.iter().enumerate() {
                let token_str = token.to_string();
                if token_str.starts_with("\"") && token_str.ends_with("\"") {
                    range.start = idx;
                    range.end = idx + 1;
                    break;
                }
            }
        }
    }
    path = tokens[range.start..range.end]
        .iter()
        .map(|t| t.to_string())
        .collect::<Vec<_>>()
        .join("");

    // Clean up the path by removing any remaining quotes
    if !is_system_include {
        path = path.trim_matches('"').to_string();
    }
    if is_system_include {
        // System includes are mapped to Rust equivalents if possible
        match path.as_str() {
            "stdio.h" => {
                rust_code.push_str("// C stdio.h -> Rust std::io\n");
                rust_code.push_str("use std::io;\n");
                rust_code.push_str("use std::io::prelude::*;\n");
            }
            "stdlib.h" => {
                rust_code.push_str("// C stdlib.h -> Rust std and libc\n");
                rust_code.push_str("use std::mem;\n");
                rust_code.push_str("use std::ptr;\n");
                rust_code.push_str("use libc;\n");
            }
            "string.h" => {
                rust_code.push_str("// C string.h -> Rust str, String, and CStr\n");
                rust_code.push_str("use std::ffi::CStr;\n");
                rust_code.push_str("use std::ffi::CString;\n");
                rust_code.push_str("use std::str;\n");
                rust_code.push_str("use libc;\n");
            }
            "math.h" => {
                rust_code.push_str("// C math.h -> Rust std::f64 and std::f32\n");
                rust_code.push_str("use std::f64::consts;\n");
                rust_code.push_str("use libc;\n");
            }
            "time.h" => {
                rust_code.push_str("// C time.h -> Rust std::time\n");
                rust_code.push_str("use std::time::{Duration, SystemTime};\n");
                rust_code.push_str("use libc;\n");
            }
            "stdbool.h" => {
                rust_code.push_str("// C stdbool.h is not needed in Rust as bool is built-in\n");
                // Nothing to include
            }
            "stdint.h" => {
                rust_code.push_str("// C stdint.h fixed width integers are built into Rust\n");
                // Nothing to include
            }
            "stddef.h" => {
                rust_code.push_str("// C stddef.h -> Various Rust std types\n");
                rust_code.push_str("use std::ptr;\n");
                rust_code.push_str("use std::mem;\n");
                rust_code.push_str("use libc;\n");
            }
            "pthread.h" => {
                rust_code.push_str("// C pthread.h -> Rust std::thread\n");
                rust_code.push_str("use std::thread;\n");
                rust_code.push_str("use std::sync::{Arc, Mutex, Condvar};\n");
                rust_code.push_str("use libc;\n");
            }
            "errno.h" => {
                rust_code.push_str("// C errno.h -> Rust std::io::Error\n");
                rust_code.push_str("use std::io::Error as IoError;\n");
                rust_code.push_str("use std::io::ErrorKind;\n");
                rust_code.push_str("use libc;\n");
            }
            "assert.h" => {
                rust_code.push_str("// C assert.h -> Rust assert! macro\n");
                // Nothing to include as assert! is built-in
            }
            "ctype.h" => {
                rust_code.push_str("// C ctype.h -> Rust char methods\n");
                rust_code.push_str("use libc;\n");
            }
            "signal.h" => {
                rust_code.push_str("// C signal.h -> Rust signal crate\n");
                rust_code.push_str("use libc;\n");
                rust_code.push_str("// You may need to add: signal = \"0.7\" to your Cargo.toml\n");
            }
            _ => {
                // Generic system include
                rust_code.push_str(&format!("// C include: <{}>\n", path));
                rust_code.push_str("use libc;\n");
                rust_code.push_str(&format!(
                    "// TODO: Map <{}> to appropriate Rust imports\n",
                    path
                ));
            }
        }
    } else {
        // Local includes are converted to Rust module imports
        // Strip .h extension if present
        let module_name = if path.ends_with(".h") {
            &path[0..path.len() - 2]
        } else {
            path.trim()
        };

        // Convert to snake_case if needed
        let rust_module = to_snake_case(module_name);

        rust_code.push_str(&format!("// C include: \"{}\"\n", path));
        rust_code.push_str(&format!("mod {};\n", rust_module));
        rust_code.push_str(&format!("use {}::*;\n", rust_module));
    }

    // Check if we need to process the included file
    if context!().get_feature("process_includes").unwrap_or(false) && !is_system_include {
        // Find the file in the include directories
        let include_dirs = context!().get_include_dirs();
        if let Some(file_path) = find_include_file(path.trim(), &include_dirs) {
            // Add code to process the include file later
            rust_code.push_str(&format!(
                "// TODO: Process included file: {:?}\n",
                file_path
            ));
        }
    }

    Ok(Some(ConvertedElement::Include(ConvertedInclude {
        path,
        rust_code,
        is_external_crate: false,
    })))
}

/// Convert callback: Does the actual conversion of C to Rust code

/// Redirect callback: Handles cases where this handler should pass tokens to a different handler
fn redirect_include(
    tokens: &[Token],
    result: HandlerResult) -> Result<HandlerResult, ConversionError> {
    report!(
        "include_handler",
        "redirect_include",
        Info,
        Report,
        "Checking if include tokens should be redirected",
        true
    );
    let mut result = Ok(result.clone());
    // Check if this is actually a different preprocessor directive
    if tokens.len() >= 2 && tokens[0].to_string() == "#" {
        match tokens[1].to_string().as_str() {
            "define" => {
                report!(
                    "include_handler",
                    "redirect_include",
                    Info,
                    Report,
                    "Redirecting to macro handler (#define)",
                    true
                );
                result = match process_macro(tokens) {
                    Ok(true) => handle_define(tokens),
                    Err(e) => Err(e),
                    Ok(false) => result,
                };
            }
            "ifdef" | "ifndef" | "if" | "else" | "elif" | "endif" => {
                report!(
                    "include_handler",
                    "redirect_include",
                    Info,
                    Report,
                    "Redirecting to macro handler (conditional compilation)",
                    true
                );

                result = match process_macro(tokens) {
                    Ok(true) => handle_ifdef(tokens),
                    Err(e) => Err(e),
                    Ok(false) => result,
                }
            }
            "undef" => {
                report!(
                    "include_handler",
                    "redirect_include",
                    Info,
                    Report,
                    "Redirecting to macro handler (#undef)",
                    true
                );
                result = match process_macro(tokens) {
                    Ok(true) => handle_undef(tokens),
                    Err(e) => Err(e),
                    Ok(false) => result,
                }
            }
            "pragma" => {
                report!(
                    "include_handler",
                    "redirect_include",
                    Info,
                    Report,
                    "Redirecting to macro handler (#pragma)",
                    true
                );
                return result;
            }
            "error" | "warning" => {
                report!(
                    "include_handler",
                    "redirect_include",
                    Info,
                    Report,
                    "Redirecting to macro handler (compiler directive)",
                    true
                );
                return result;
            }
            "line" => {
                report!(
                    "include_handler",
                    "redirect_include",
                    Info,
                    Report,
                    "Redirecting to macro handler (#line)",
                    true
                );
                return result;
            }
            _ => {
                // Unknown preprocessor directive, let macro handler try
                report!(
                    "include_handler",
                    "redirect_include",
                    Info,
                    Report,
                    "Unknown preprocessor directive, redirecting to macro handler",
                    true
                );
                return result;
            }
        }
    }

    // Check if this is malformed and might actually be something else
    if tokens.len() >= 1 && tokens[0].to_string() == "#" && tokens.len() < 2 {
        report!(
            "include_handler",
            "redirect_include",
            Info,
            Report,
            "Malformed preprocessor directive, redirecting to macro handler",
            true
        );
        return result;
    }

    // No redirection needed
    result
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
