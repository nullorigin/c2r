use crate::context::{Context, HandlerReport, ReportLevel::{Info, Error}, HandlerPhase::{Process, Handle, Extract, Convert, Report}};
use crate::error::ConversionError;
use crate::extract::ExtractedElement;
use crate::extract::ExtractedInclude;
use crate::handler::HandlerResult;
use crate::{get_id, token, ConvertedElement, ConvertedInclude, Id, ProcessedResult, Token, report};
use std::path::{Path, PathBuf};
use super::common::{self, is_token, not_handled, replace_with};

/// Creates an include handler that can detect and convert C include directives
pub fn create_include_handler() -> crate::handler::Handler {
    let handler_id = get_id("include_handler");
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
        None,
        None,
        None
    )
}

/// Process callback: Initializes and confirms this handler can handle the tokens
fn process_include(tokens: &[Token], context: &mut Context) -> Result<bool, ConversionError> {
    // Validate input
    if tokens.len() < 2 {
        return Ok(false);
    }
    
    // Check for #include pattern
    if tokens[0].to_string() == "#" && tokens.len() >= 2 && tokens[1].to_string() == "include" {
        // Validate we have a filename
        if tokens.len() >= 3 {
            report!(context, "include_handler", Info, Process, 
                "Include directive detected", true);
            return Ok(true);
        }
        
        // Might be malformed include - let's try to handle it gracefully
        report!(context, "include_handler", Info, Process, 
            "Potentially malformed include directive detected", true);
        return Ok(true);
    }
    
    Ok(false)
}


/// Processes an include directive
fn handle_include(tokens: &[Token], context: &mut Context) -> Result<HandlerResult, ConversionError> {
    report!(context, "include_handler", Info, Handle, 
        "Include handler processing tokens", true);

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
        report!(context, "include_handler", Error, Handle, 
            "Unrecognized include format", true);
        return not_handled();
    }

    report!(context, "include_handler", Info, Handle, 
        format!("Found include: {} (system: {})", include_path, is_system_include), true);
    Ok(HandlerResult::NotHandled(Some(tokens.to_vec()), get_id("include_handler")))
}

/// Extracts an include directive as an ExtractedElement
pub fn extract_include(tokens: &[Token], context: &mut Context) -> Result<Option<ExtractedElement>, ConversionError> {
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
        original_code: tokens.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(" "),
    };

    Ok(Some(ExtractedElement::Include(extracted_include)))
}
pub fn convert_include(tokens: &[Token], context: &mut Context) -> Result<Option<ConvertedElement>, ConversionError> {
    let id = get_id("convert_include");
    let mut rust_code = String::new();
    let mut path = String::new();
    let mut is_system_include = false;
    let left = tokens.iter().position(|t| t.to_string() == "<");
    let right = tokens.iter().position(|t| t.to_string() == ">");
    is_system_include = left.is_some() && right.is_some();
    let mut i = 0;
    let mut range = 0..0;
    if is_system_include {
        range.start = left.unwrap() + 1;
        range.end = right.unwrap();
    }
    else {
        range.start = 0;
        range.end = 0;
        while range.end == 0 {
            if tokens[i].to_string() == "\"" {
                if range.start == 0 {
                    range.start = i + 1;
                } else {
                    range.end = i;
                }
            }
            i+=1
        }
    }
    path = tokens[range].iter().map(|t|t.to_string()).collect::<Vec<_>>().join("");
    if is_system_include {
        // System includes are mapped to Rust equivalents if possible
        match path.as_str() {
            "stdio.h" => {
                rust_code.push_str("// C stdio.h -> Rust std::io\n");
                rust_code.push_str("use std::io;\n");
                rust_code.push_str("use std::io::prelude::*;\n");
            },
            "stdlib.h" => {
                rust_code.push_str("// C stdlib.h -> Rust std and libc\n");
                rust_code.push_str("use std::mem;\n");
                rust_code.push_str("use std::ptr;\n");
                rust_code.push_str("use libc;\n");
            },
            "string.h" => {
                rust_code.push_str("// C string.h -> Rust str, String, and CStr\n");
                rust_code.push_str("use std::ffi::CStr;\n");
                rust_code.push_str("use std::ffi::CString;\n");
                rust_code.push_str("use std::str;\n");
                rust_code.push_str("use libc;\n");
            },
            "math.h" => {
                rust_code.push_str("// C math.h -> Rust std::f64 and std::f32\n");
                rust_code.push_str("use std::f64::consts;\n");
                rust_code.push_str("use libc;\n");
            },
            "time.h" => {
                rust_code.push_str("// C time.h -> Rust std::time\n");
                rust_code.push_str("use std::time::{Duration, SystemTime};\n");
                rust_code.push_str("use libc;\n");
            },
            "stdbool.h" => {
                rust_code.push_str("// C stdbool.h is not needed in Rust as bool is built-in\n");
                // Nothing to include
            },
            "stdint.h" => {
                rust_code.push_str("// C stdint.h fixed width integers are built into Rust\n");
                // Nothing to include
            },
            "stddef.h" => {
                rust_code.push_str("// C stddef.h -> Various Rust std types\n");
                rust_code.push_str("use std::ptr;\n");
                rust_code.push_str("use std::mem;\n");
                rust_code.push_str("use libc;\n");
            },
            "pthread.h" => {
                rust_code.push_str("// C pthread.h -> Rust std::thread\n");
                rust_code.push_str("use std::thread;\n");
                rust_code.push_str("use std::sync::{Arc, Mutex, Condvar};\n");
                rust_code.push_str("use libc;\n");
            },
            "errno.h" => {
                rust_code.push_str("// C errno.h -> Rust std::io::Error\n");
                rust_code.push_str("use std::io::Error as IoError;\n");
                rust_code.push_str("use std::io::ErrorKind;\n");
                rust_code.push_str("use libc;\n");
            },
            "assert.h" => {
                rust_code.push_str("// C assert.h -> Rust assert! macro\n");
                // Nothing to include as assert! is built-in
            },
            "ctype.h" => {
                rust_code.push_str("// C ctype.h -> Rust char methods\n");
                rust_code.push_str("use libc;\n");
            },
            "signal.h" => {
                rust_code.push_str("// C signal.h -> Rust signal crate\n");
                rust_code.push_str("use libc;\n");
                rust_code.push_str("// You may need to add: signal = \"0.7\" to your Cargo.toml\n");
            },
            _ => {
                // Generic system include
                rust_code.push_str(&format!("// C include: <{}>\n", path));
                rust_code.push_str("use libc;\n");
                rust_code.push_str(&format!("// TODO: Map <{}> to appropriate Rust imports\n", path));
            }
        }
    } else {
        // Local includes are converted to Rust module imports
        // Strip .h extension if present
        let module_name = if path.ends_with(".h") {
            &path[0..path.len()-2]
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
    if context.get_feature("process_includes").unwrap_or(false) && !is_system_include {
        // Find the file in the include directories
        let include_dirs = context.get_include_dirs();
        if let Some(file_path) = find_include_file(path.trim(), &include_dirs) {
            // Add code to process the include file later
            rust_code.push_str(&format!("// TODO: Process included file: {:?}\n", file_path));
        }
    }

    Ok(Some(ConvertedElement::Include(ConvertedInclude {
        path: path,
        rust_code,
        is_external_crate: false,
    })))
}

/// Convert callback: Does the actual conversion of C to Rust code

/// Redirect callback: Handles cases where this handler should pass tokens to a different handler
fn redirect_include(tokens: &[Token], context: &mut Context) -> Result<Option<String>, ConversionError> {
    report!(context, "include_handler", Info, Report, 
        "Checking if include tokens should be redirected", true);
    
    // Check if this is actually a different preprocessor directive
    if tokens.len() >= 2 && tokens[0].to_string() == "#" {
        match tokens[1].to_string().as_str() {
            "define" => {
                report!(context, "include_handler", Info, Report, 
                    "Redirecting to macro handler (#define)", true);
                return Ok(Some("macro".to_string()));
            },
            "ifdef" | "ifndef" | "if" | "else" | "elif" | "endif" => {
                report!(context, "include_handler", Info, Report, 
                    "Redirecting to macro handler (conditional compilation)", true);
                return Ok(Some("macro".to_string()));
            },
            "undef" => {
                report!(context, "include_handler", Info, Report, 
                    "Redirecting to macro handler (#undef)", true);
                return Ok(Some("macro".to_string()));
            },
            "pragma" => {
                report!(context, "include_handler", Info, Report, 
                    "Redirecting to macro handler (#pragma)", true);
                return Ok(Some("macro".to_string()));
            },
            "error" | "warning" => {
                report!(context, "include_handler", Info, Report, 
                    "Redirecting to macro handler (compiler directive)", true);
                return Ok(Some("macro".to_string()));
            },
            "line" => {
                report!(context, "include_handler", Info, Report, 
                    "Redirecting to macro handler (#line)", true);
                return Ok(Some("macro".to_string()));
            },
            _ => {
                // Unknown preprocessor directive, let macro handler try
                report!(context, "include_handler", Info, Report, 
                    "Unknown preprocessor directive, redirecting to macro handler", true);
                return Ok(Some("macro".to_string()));
            }
        }
    }
    
    // Check if this is malformed and might actually be something else
    if tokens.len() >= 1 && tokens[0].to_string() == "#" && tokens.len() < 2 {
        report!(context, "include_handler", Info, Report, 
            "Malformed preprocessor directive, redirecting to macro handler", true);
        return Ok(Some("macro".to_string()));
    }
    
    // No redirection needed
    Ok(None)
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
