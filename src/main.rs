pub mod config;
pub mod convert;
pub mod entry;
pub mod error;
pub mod extract;
pub mod file_utils;
pub mod handler;
pub mod handlers;
pub mod lock;
pub mod logging;
pub mod pattern;
pub mod registry;
pub mod table;
pub mod tests;
pub mod token;

pub use crate::config::Context;
// pub use crate::report; // Commented out to avoid macro conflict
pub use crate::config::*;
pub use crate::convert::*;
pub use crate::entry::*;
pub use crate::error::*;
pub use crate::extract::*;
pub use crate::handler::*;
pub use crate::handlers::*;
pub use crate::lock::*;
pub use crate::logging::*;
pub use crate::pattern::*;
pub use crate::registry::*;
pub use crate::table::*;
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
    let mut display_registry = false;

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
            "--display-registry" => {
                display_registry = true;
                command = "display-registry".to_string();
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
                    debug!("Added include directory: {}", include_dir);
                } else {
                    debug!("Missing directory path after {}", arg);
                }
            }
            "--base-dir" => {
                // Set the base directory
                if let Some(base_dir) = args_iter.next() {
                    context = context.with_base_dir(base_dir);
                    debug!("Set base directory: {}", base_dir);
                } else {
                    debug!("Missing directory path after {}", arg);
                }
            }
            "--system-includes" => {
                // Enable processing of system includes
                context = context.with_system_includes(true);
                debug!("System include processing enabled");
            }
            "--output-file" | "-o" => {
                // Set output file
                if let Some(file_path) = args_iter.next() {
                    output_file = file_path.clone();
                    output_to_stdout = false;
                    debug!("Output file set to: {}", output_file);
                } else {
                    debug!("Missing file path after {}", arg);
                }
            }
            "--stdout" => {
                // Force output to stdout
                output_to_stdout = true;
                output_file.clear();
                debug!("Output set to stdout");
            }
            "--input-file" => {
                // Handle the input-file flag
                if let Some(file_path) = args_iter.next() {
                    input_file = file_path.clone();
                    debug!("Input file set to: {}", input_file);

                    // Automatically set the base directory to the input file's directory
                    if let Some(parent) = Path::new(&file_path).parent() {
                        if parent.to_string_lossy() != "" {
                            context = context.with_base_dir(parent.to_string_lossy().to_string());
                            debug!("Base directory set to input file's parent: {:?}", parent);
                        }
                    }
                } else {
                    error!("Missing file path after --input-file");
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
                            debug!("Auto-set base directory: {:?}", parent);
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
                            debug!("Auto-set base directory: {:?}", parent);
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
        VERBOSITY_WARN => warn!("Verbosity: Warnings and Errors"),
        VERBOSITY_ERROR => error!("Verbosity: Errors only"),
        VERBOSITY_DEBUG => debug!("Verbosity: Debug messages Warnings and Errors"),
        VERBOSITY_INFO => info!("Verbosity: All messages"),
        _ => info!("Verbosity: Custom level {}", verbosity),
    }

    // Check if we should run handler tests

    // Handle registry display command (doesn't require input file)
    if command == "display-registry" {
        println!("🗃️  Displaying Registry Database View");
        println!(
            "═══════════════════════════════════════════════════════════════════════════════════════"
        );

        // Initialize handlers and patterns to populate the registry with meaningful data
        println!("🔄 Initializing handlers and patterns for display...");

        // Register all available handlers
        let mut handlers = context.handlers.clone();
        handlers.register_all_shared(create_all_handlers(), Id::get("display_registry_handlers"));
        context.handlers = handlers;

        // Register common patterns
        pattern::register_common_multi_token_patterns();

        // Manually populate registry with sample entries for meaningful display
        let sample_handlers = create_all_handlers();
        for (i, handler) in sample_handlers.iter().enumerate() {
            let handler_id = Id::get(&format!("sample_handler_{}", i));
            context
                .registry
                .entries
                .insert(handler_id, Entry::Handler(handler.clone()));
        }

        // Add some sample patterns to registry
        let mut sample_pattern = pattern::Pattern::default();
        sample_pattern.name = "sample_c_function".to_string();
        sample_pattern.description = "C Function Pattern".to_string();
        sample_pattern.id = Id::get("sample_c_function");
        let pattern_id = Id::get("sample_pattern_c_function");
        context
            .registry
            .entries
            .insert(pattern_id, Entry::Patternizer(sample_pattern));

        // Add some sample reports
        let sample_report = HandlerReport {
            report_id: Box::new(Id::get("sample_report_1")),
            handler_id: Box::new(Id::get("sample_handler")),
            handler_name: "sample_handler".to_string(),
            function_name: "sample_function".to_string(),
            message: "Sample conversion report".to_string(),
            level: ReportLevel::Info,
            tokens_processed: 10,
            tokens_consumed: 8,
            phase: HandlerPhase::Convert,
            success: true,
            metadata: std::collections::HashMap::new(),
        };
        let report_id = Id::get("sample_report_1");
        context
            .registry
            .entries
            .insert(report_id, Entry::HandlerReport(sample_report));

        // Add some sample string entries
        context.registry.entries.insert(
            Id::get("sample_string"),
            Entry::Str("Sample C code".to_string()),
        );
        context
            .registry
            .entries
            .insert(Id::get("sample_bool"), Entry::Bool(true));
        context
            .registry
            .entries
            .insert(Id::get("sample_val"), Entry::Val(42));

        println!(
            "✅ Initialization completed - {} handlers registered, {} registry entries populated",
            context.handlers.handlers.len(),
            context.registry.entries.len()
        );
        println!();

        // Display the full context including registry
        context.display_full_context();

        println!("✅ Registry display completed successfully");
        return; // Exit after displaying registry
    }

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
                                        error!(
                                            "Failed to write output file {}: {}",
                                            output_file, e
                                        );
                                    }
                                }
                            }
                        } else {
                            println!("No convertible code found in input file");
                        }
                    }
                    Err(e) => {
                        error!("Conversion failed: {}", e);
                    }
                }
            }
            _ => {
                error!("Unknown command: {}", command);
                error!("Usage: {} [options] convert <input.c>", program_name);
            }
        }
    } else {
        error!("Usage: {} [options] convert <input.c>", program_name);
        info!("Options:");
        info!("  -v, --verbose [level]  Set verbosity level (0-3, default: 1)");
        info!("    0: No output");
        info!("    1: Errors only");
        info!("    2: Debug messages");
        info!("    3: All information");
        info!("  -I, --include <dir>    Add an include directory for header files");
        info!("  --base-dir <dir>       Set the base directory for resolving paths");
        info!("  --system-includes      Enable processing of system includes");
        info!("  --test-handlers        Run handler tests");
        info!(
            "  --display-registry     Display the registry database view with all entries and statistics"
        );
        info!("  -o, --output-file <file> Write converted code to file instead of stdout");
        info!("  --stdout               Force output to stdout (default)");
    }
}

fn process_file(input_path: &str) -> Result<ProcessedResults, ConversionError> {
    info!("Processing file {}", input_path);

    match fs::read_to_string(input_path) {
        Ok(content) => {
            let tokenizer = context!().tokenizer.clone();
            let mut handlers = context!().handlers.clone();
            handlers.register_all_shared(create_all_handlers(), Id::get("all_handlers_shared"));
            let tokens = tokenizer.tokenize(content.as_bytes().to_vec())?;
            let result = handlers.process_all(tokens.as_slice());
            result
        }
        Err(e) => Err(ConversionError::new("Failed to process result")),
    }
}

/// Extract all converted Rust code from a ProcessedResults collection
fn extract_converted_code_from_results(results: &ProcessedResults) -> String {
    debug!(
        "DEBUG: Processing {} results from ProcessedResults collection",
        results.len()
    );

    // Use the built-in extract_all_code method from ProcessedResults
    let code_sections = results.extract_all_code();

    // Debug: Log each result and what code was extracted
    for (i, result) in results.results.iter().enumerate() {
        match &result.result {
            HandlerResult::Handled(_, _, id) => {
                debug!(
                    "DEBUG: Result {}: HandlerResult::Handled from handler {}",
                    i,
                    id.name()
                );
            }
            HandlerResult::Processed(_, _, code, id) => {
                debug!(
                    "DEBUG: Result {}: HandlerResult::Processed from handler {} with code: '{}'",
                    i,
                    id.name(),
                    code
                );
            }
            HandlerResult::Completed(_, _, code, id) => {
                debug!(
                    "DEBUG: Result {}: HandlerResult::Completed from handler {} with code: '{}'",
                    i,
                    id.name(),
                    code
                );
            }
            HandlerResult::NotHandled(_, _, id) => {
                debug!(
                    "DEBUG: Result {}: HandlerResult::NotHandled from handler {}",
                    i,
                    id.name()
                );
            }
            HandlerResult::Redirected(_, _, target, from_id, to_id) => {
                debug!(
                    "DEBUG: Result {}: HandlerResult::Redirected from {} to {} (target: {})",
                    i,
                    from_id.name(),
                    to_id.name(),
                    target
                );
            }
            HandlerResult::Extracted(element, _, code, id) => {
                debug!(
                    "DEBUG: Result {}: HandlerResult::Extracted from handler {} with code: '{}'",
                    i,
                    id.name(),
                    code
                );
            }
            HandlerResult::Converted(element, _, code, id) => {
                debug!(
                    "DEBUG: Result {}: HandlerResult::Converted from handler {} with code: '{}'",
                    i,
                    id.name(),
                    code
                );
            }
        }
    }

    debug!(
        "DEBUG: Extracted {} code sections from {} results",
        code_sections.len(),
        results.len()
    );

    // Join all code sections with proper spacing
    if !code_sections.is_empty() {
        let joined_code = code_sections.join("\n\n");
        debug!(
            "DEBUG: Final joined code sections: {} sections",
            code_sections.len()
        );
        format_final_output(&joined_code)
    } else {
        debug!(
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
