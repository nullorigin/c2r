extern crate core;

pub mod config;
pub mod convert;
pub mod error;
pub mod extract;
pub mod file_utils;
pub mod handler;
pub mod handlers;
pub mod lock;
pub mod logging;
pub mod tests;
pub mod token;
pub mod pattern;
pub use crate::config::Context;
// pub use crate::report; // Commented out to avoid macro conflict
pub use crate::config::*;
pub use crate::config::{Entry, ReportLevel};
pub use crate::convert::*;
pub use crate::error::ConversionError;
pub use crate::error::*;
pub use crate::extract::*;
pub use crate::handler::*;
pub use crate::handler::{Handler, HandlerMap, HandlerResult, ProcessedResults};
pub use crate::handlers::*;
pub use crate::lock::*;
pub use crate::token::Token;
pub use crate::token::Tokenizer;
pub use crate::token::*;
// Import logging macros
use std::env;
use std::fs;
use std::io::Write;
use std::path::Path;

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut args_iter = args.iter();
    let program_name = args_iter.next().unwrap_or(&"c2r".to_string()).clone();

    // Parse arguments
    let mut input_file = String::new();
    let mut output_file = String::new();
    let mut output_to_stdout = true;
    let mut command = "convert".to_string();
    let mut run_tests = false;

    // Default verbosity level is errors only (1)
    let mut verbosity = VERBOSITY_ERROR;
    let mut verbose_count = 1; // For the new logging system

    // Create a configuration object with defaults
    let mut context = &mut Context::new("default");

    // Parse command line arguments
    while let Some(arg) = args_iter.next() {
        match arg.as_str() {
            "--test-handlers" => {
                run_tests = true;
            }
            "-v" | "--verbose" => {
                // Look for a numeric value after -v
                if let Some(level_str) = args_iter.next() {
                    if let Ok(level) = level_str.parse::<u8>() {
                        verbosity = level;
                        verbose_count = level as u64;
                    } else {
                        verbosity = VERBOSITY_DEBUG;
                        verbose_count = 3; // Debug level
                    }
                } else {
                    verbosity = VERBOSITY_DEBUG;
                    verbose_count = 3; // Debug level
                }
            }
            "-I" | "--include" => {
                // Add an include directory
                if let Some(include_dir) = args_iter.next() {
                    context.add_include_dir(include_dir);
                    log!(debug, "Added include directory: {}", include_dir);
                } else {
                    log!(error, "Missing directory path after {}", arg);
                }
            }
            "--base-dir" => {
                // Set the base directory
                if let Some(base_dir) = args_iter.next() {
                    context = context.with_base_dir(base_dir);
                    log!(debug, "Set base directory: {}", base_dir);
                } else {
                    log!(error, "Missing directory path after {}", arg);
                }
            }
            "--system-includes" => {
                // Enable processing of system includes
                context = context.with_system_includes(true);
                log!(debug, "System include processing enabled");
            }
            "--output-file" | "-o" => {
                // Set output file
                if let Some(file_path) = args_iter.next() {
                    output_file = file_path.clone();
                    output_to_stdout = false;
                    log!(debug, "Output file set to: {}", output_file);
                } else {
                    log!(error, "Missing file path after {}", arg);
                }
            }
            "--stdout" => {
                // Force output to stdout
                output_to_stdout = true;
                output_file.clear();
                log!(debug, "Output set to stdout");
            }
            "--input-file" => {
                // Handle the input-file flag
                if let Some(file_path) = args_iter.next() {
                    input_file = file_path.clone();
                    log!(debug, "Input file set to: {}", input_file);

                    // Automatically set the base directory to the input file's directory
                    if let Some(parent) = Path::new(&file_path).parent() {
                        if parent.to_string_lossy() != "" {
                            context = context.with_base_dir(parent.to_string_lossy().to_string());
                            log!(
                                debug,
                                "Base directory set to input file's parent: {:?}",
                                parent
                            );
                        }
                    }
                } else {
                    log!(error, "Missing file path after --input-file");
                }
            }
            "convert" => {
                command = arg.clone();
                // The next argument should be the input file
                if let Some(file_path) = args_iter.next() {
                    input_file = file_path.clone();

                    // Automatically set the base directory to the input file's directory
                    if let Some(parent) = Path::new(&file_path).parent() {
                        if parent.to_string_lossy() != "" {
                            context = context.with_base_dir(parent);
                            log!(debug, "Auto-set base directory: {:?}", parent);
                        }
                    }
                }
            }
            _ => {
                // If not an option and no command is set, this might be a command
                if !arg.starts_with("-") && command.is_empty() {
                    command = arg.clone();
                }
                // If we already have a command but no input file, this might be an input file
                else if !arg.starts_with("-") && input_file.is_empty() {
                    input_file = arg.clone();

                    // Automatically set the base directory to the input file's directory
                    if let Some(parent) = Path::new(&arg).parent() {
                        if parent.to_string_lossy() != "" {
                            context = context.with_base_dir(parent.to_string_lossy().to_string());
                            log!(debug, "Auto-set base directory: {:?}", parent);
                        }
                    }
                }
            }
        }
    }

    // Initialize the logging system with the verbosity level
    logging::initialize(verbose_count);

    // Set verbosity in the context and apply it globally
    context = context.with_verbosity(verbosity);
    context.apply();

    // Print the current verbosity level
    match verbosity {
        VERBOSITY_NONE => log!(info, "Verbosity: None"),
        VERBOSITY_ERROR => log!(info, "Verbosity: Errors only"),
        VERBOSITY_DEBUG => log!(info, "Verbosity: Debug messages"),
        VERBOSITY_INFO => log!(info, "Verbosity: All messages"),
        _ => log!(info, "Verbosity: Custom level {}", verbosity),
    }

    // Check if we should run handler tests

    // Check if we have an input file
    if !input_file.is_empty() {
        // Process based on command
        match command.as_str() {
            "convert" => {
                println!("{}", "Converting...");
                match process_file(input_file.as_str()) {
                    Ok(results) => {
                        let converted_code = extract_converted_code_from_results(&results);
                        if !converted_code.is_empty() {
                            if output_to_stdout || output_file.is_empty() {
                                println!("\n// === Converted Rust Code ===");
                                println!("{}", converted_code);
                            } else {
                                match write_output_file(&output_file, &converted_code) {
                                    Ok(()) => {
                                        println!(
                                            "Successfully wrote converted code to: {}",
                                            output_file
                                        );
                                        println!(
                                            "Generated {} lines of Rust code",
                                            converted_code.lines().count()
                                        );
                                    }
                                    Err(e) => {
                                        log!(
                                            error,
                                            "Failed to write output file {}: {}",
                                            output_file,
                                            e
                                        );
                                    }
                                }
                            }
                        } else {
                            println!("No convertible code found in input file");
                        }
                    }
                    Err(e) => {
                        log!(error, "Conversion failed: {}", e);
                    }
                }
            }
            _ => {
                log!(error, "Unknown command: {}", command);
                log!(error, "Usage: {} [options] convert <input.c>", program_name);
            }
        }
    } else {
        log!(error, "Usage: {} [options] convert <input.c>", program_name);
        log!(info, "Options:");
        log!(
            info,
            "  -v, --verbose [level]  Set verbosity level (0-3, default: 1)"
        );
        log!(info, "    0: No output");
        log!(info, "    1: Errors only");
        log!(info, "    2: Debug messages");
        log!(info, "    3: All information");
        log!(
            info,
            "  -I, --include <dir>    Add an include directory for header files"
        );
        log!(
            info,
            "  --base-dir <dir>       Set the base directory for resolving paths"
        );
        log!(
            info,
            "  --system-includes      Enable processing of system includes"
        );
        log!(info, "  --test-handlers        Run handler tests");
        log!(
            info,
            "  -o, --output-file <file> Write converted code to file instead of stdout"
        );
        log!(
            info,
            "  --stdout               Force output to stdout (default)"
        );
    }
}

fn process_file(
    input_path: &str) -> Result<ProcessedResults, ConversionError> {
    log!(info, "Processing file {}", input_path);

    match fs::read_to_string(input_path) {
        Ok(content) => {
            let tokenizer = context!().tokenizer.clone();
            let mut handlers = context!().handlers.clone();
            handlers.register_all_shared(
                create_all_handlers(),
                Id::get("all_handlers_shared"),
            );
            let tokens = tokenizer.tokenize(content.as_bytes().to_vec())?;
            let result = handlers.process_all(tokens.as_slice());
            result
        }
        Err(e) => Err(ConversionError::new("Failed to process result")),
    }
}

/// Extract all converted Rust code from a ProcessedResults collection
fn extract_converted_code_from_results(results: &ProcessedResults) -> String {
    println!(
        "DEBUG: Processing {} results from ProcessedResults collection",
        results.len()
    );

    // Use the built-in extract_all_code method from ProcessedResults
    let code_sections = results.extract_all_code();

    // Debug: Log each result and what code was extracted
    for (i, result) in results.results.iter().enumerate() {
        match &result.result {
            HandlerResult::Handled(_, _, id) => {
                println!(
                    "DEBUG: Result {}: HandlerResult::Handled from handler {}",
                    i,
                    id.name()
                );
            }
            HandlerResult::Processed(_, _, code, id) => {
                println!(
                    "DEBUG: Result {}: HandlerResult::Processed from handler {} with code: '{}'",
                    i,
                    id.name(),
                    code
                );
            }
            HandlerResult::Completed(_, _, code, id) => {
                println!(
                    "DEBUG: Result {}: HandlerResult::Completed from handler {} with code: '{}'",
                    i,
                    id.name(),
                    code
                );
            }
            HandlerResult::NotHandled(_, _, id) => {
                println!(
                    "DEBUG: Result {}: HandlerResult::NotHandled from handler {}",
                    i,
                    id.name()
                );
            }
            HandlerResult::Redirected(_, _, target, from_id, to_id) => {
                println!(
                    "DEBUG: Result {}: HandlerResult::Redirected from {} to {} (target: {})",
                    i,
                    from_id.name(),
                    to_id.name(),
                    target
                );
            }
            HandlerResult::Extracted(element, _, code, id) => {
                println!(
                    "DEBUG: Result {}: HandlerResult::Extracted from handler {} with code: '{}'",
                    i,
                    id.name(),
                    code
                );
            }
            HandlerResult::Converted(element, _, code, id) => {
                println!(
                    "DEBUG: Result {}: HandlerResult::Converted from handler {} with code: '{}'",
                    i,
                    id.name(),
                    code
                );
            }
        }
    }

    println!(
        "DEBUG: Extracted {} code sections from {} results",
        code_sections.len(),
        results.len()
    );

    // Join all code sections with proper spacing
    if !code_sections.is_empty() {
        let joined_code = code_sections.join("\n\n");
        println!(
            "DEBUG: Final joined code sections: {} sections",
            code_sections.len()
        );
        format_final_output(&joined_code)
    } else {
        println!(
            "DEBUG: No code sections to output from {} results",
            results.len()
        );
        String::new()
    }
}

/// Extract Rust code from a HandlerResult
fn extract_code_from_handler_result(result: &HandlerResult) -> Option<String> {
    match result {
        HandlerResult::Converted(element, _, rust_code, _id) => {
            // Use the string from HandlerResult if available, otherwise extract from element
            if !rust_code.trim().is_empty() {
                Some(rust_code.clone())
            } else {
                Some(extract_code_from_converted_element(element))
            }
        }
        HandlerResult::Extracted(element, _, rust_code, _id) => {
            // For extracted elements, use the rust_code string
            if !rust_code.trim().is_empty() {
                Some(rust_code.clone())
            } else {
                None // ExtractedElement doesn't have rust_code field
            }
        }
        HandlerResult::Completed(_tokens, _, rust_code, _id) => Some(rust_code.clone()),
        HandlerResult::Processed(_tokens, _, rust_code, _id) => Some(rust_code.clone()),
        _ => None,
    }
}

/// Extract Rust code from a ConvertedElement
fn extract_code_from_converted_element(element: &ConvertedElement) -> String {
    match element {
        ConvertedElement::Function(func) => func.rust_code.clone(),
        ConvertedElement::Struct(s) => s.rust_code.clone(),
        ConvertedElement::Enum(e) => e.rust_code.clone(),
        ConvertedElement::Typedef(t) => t.rust_code.clone(),
        ConvertedElement::Global(g) => g.rust_code.clone(),
        ConvertedElement::Macro(m) => m.rust_code.clone(),
        ConvertedElement::Include(i) => i.rust_code.clone(),
        ConvertedElement::Array(a) => a.rust_code.clone(),
        ConvertedElement::Comment(c) => c.rust_code.clone(),
        ConvertedElement::Expression(expr) => expr.rust_code.clone(),
        ConvertedElement::ControlFlow(cf) => cf.rust_code.clone(),
    }
}

/// Format the final output with proper structure and comments
fn format_final_output(code: &str) -> String {
    let mut output = String::new();

    // Add file header comment
    output.push_str("// Generated by c2r (C-to-Rust converter)\n");
    output.push_str("// This file was automatically converted from C code\n");
    output.push_str("// Manual review and testing recommended\n\n");

    // Add common imports that might be needed
    output.push_str("// Common imports for converted C code\n");
    output
        .push_str("use std::os::raw::{c_char, c_int, c_long, c_uchar, c_uint, c_ulong, c_void};\n");
    output.push_str("use std::ptr;\n\n");

    // Add the converted code
    output.push_str(&code);

    // Ensure proper ending
    if !output.ends_with('\n') {
        output.push('\n');
    }

    output
}

/// Write converted code to an output file
fn write_output_file(file_path: &str, code: &str) -> Result<(), Box<dyn std::error::Error>> {
    // Create parent directories if they don't exist
    if let Some(parent) = Path::new(file_path).parent() {
        fs::create_dir_all(parent)?;
    }

    // Write the code to the file
    let mut file = fs::File::create(file_path)?;
    file.write_all(code.as_bytes())?;
    file.flush()?;

    Ok(())
}
