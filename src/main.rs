pub mod context;
pub mod convert;
pub mod handler;
pub mod handlers;
pub mod logging;
pub mod extract;
pub mod token;
pub mod file_utils;
pub mod error;
// Import logging macros
use std::env;
use std::fs;
use std::path::Path;
pub use crate::logging::*;
pub use crate::context::*;
pub use crate::handler::*;
pub use crate::convert::*;
pub use crate::extract::*;
pub use crate::token::*;
pub use crate::file_utils::*;
pub use crate::error::*;
pub use crate::handlers::*;
fn main() {
    let args: Vec<String> = env::args().collect();
    let mut args_iter = args.iter();
    let program_name = args_iter.next().unwrap_or(&"c2r".to_string()).clone();

    // Parse arguments
    let mut input_file = String::new();
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
            "--input-file" => {
                // Handle the input-file flag
                if let Some(file_path) = args_iter.next() {
                    input_file = file_path.clone();
                    log!(debug, "Input file set to: {}", input_file);

                    // Automatically set the base directory to the input file's directory
                    if let Some(parent) = Path::new(&file_path).parent() {
                        if parent.to_string_lossy() != "" {
                            context = context.with_base_dir(parent.to_string_lossy().to_string());
                            log!(debug, "Base directory set to input file's parent: {:?}", parent);
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
              let result =  process_file(&input_file, context);
               println!("{}", result.unwrap().result.to_string());
            }
            _ => {
                log!(error, "Unknown command: {}", command);
                log!(error, "Usage: {} [options] convert <input.c>", program_name);
            }
        }
    } else {
        log!(error, "Usage: {} [options] convert <input.c>", program_name);
        log!(info, "Options:");
        log!(info, "  -v, --verbose [level]  Set verbosity level (0-3, default: 1)");
        log!(info, "    0: No output");
        log!(info, "    1: Errors only");
        log!(info, "    2: Debug messages");
        log!(info, "    3: All information");
        log!(info, "  -I, --include <dir>    Add an include directory for header files");
        log!(info, "  --base-dir <dir>       Set the base directory for resolving paths");
        log!(info, "  --system-includes      Enable processing of system includes");
        log!(info, "  --test-handlers        Run handler tests");
    }
}

fn process_file(input_path: &str, context: &mut Context) -> Result<ProcessedResult,ConversionError> {
    log!(info, "Processing file {}", input_path);

    match fs::read_to_string(input_path) {
        Ok(content) => {
            context.handlers.register_all(create_all_handlers());
            let tokenizer = context.tokenizer.clone();
            let handlers = context.handlers.clone();
            let tokens = tokenizer.tokenize(content.as_bytes().to_vec(),context).unwrap();
            let result = handlers.process(tokens.as_slice(),context);
            result
        }
        Err(e) => Err(ConversionError::new("Failed to process result"))
    }
}
