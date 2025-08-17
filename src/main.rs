// C to Rust converter modules
pub mod config;
pub mod convert;
pub mod error;
pub mod extract;
pub mod file_utils;
pub mod handler;
mod handler_test;
pub mod handlers;
mod tests;
pub mod token_parser;

// Re-export primary types
use crate::config::Config;
use crate::config::{VERBOSITY_DEBUG, VERBOSITY_ERROR, VERBOSITY_INFO, VERBOSITY_NONE};
use crate::convert::Converter;
use crate::extract::Extractor;
use crate::handler::HandlerRegistry;
use crate::handlers::register_handlers;
use crate::token_parser::Tokenizer;
use std::env;
use std::fs;
use std::path::Path;

// Import our log macro implicitly by importing the config module above
// The macro is already available via crate::log! since it's exported in lib.rs

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut args_iter = args.iter();
    let program_name = args_iter.next().unwrap_or(&"c2r".to_string()).clone();

    // Parse arguments
    let mut input_file = None;
    let mut run_tests = false;

    // Default verbosity level is errors only (1)
    let mut verbosity = VERBOSITY_ERROR;

    // Create a configuration object with defaults
    let mut config = Config::new();

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
                    } else {
                        verbosity = VERBOSITY_DEBUG; // Default to level 2 if no valid number
                    }
                } else {
                    verbosity = VERBOSITY_DEBUG; // Default to level 2 if no value provided
                }
            }
            "-I" | "--include" => {
                // Add an include directory
                if let Some(include_dir) = args_iter.next() {
                    config.add_include_dir(include_dir);
                    log!(debug, "Added include directory: {}", include_dir);
                } else {
                    log!(error, "Missing directory path after {}", arg);
                }
            }
            "--base-dir" => {
                // Set the base directory
                if let Some(base_dir) = args_iter.next() {
                    config = config.with_base_dir(base_dir);
                    log!(debug, "Set base directory: {}", base_dir);
                } else {
                    log!(error, "Missing directory path after {}", arg);
                }
            }
            "--system-includes" => {
                // Enable processing of system includes
                config = config.with_system_includes(true);
                log!(debug, "System include processing enabled");
            }
            _ => {
                // If not an option, assume it's the input file
                if !arg.starts_with("-") && input_file.is_none() {
                    input_file = Some(arg.clone());

                    // Automatically set the base directory to the input file's directory
                    if let Some(parent) = Path::new(&arg).parent() {
                        if parent.to_string_lossy() != "" {
                            config = config.with_base_dir(parent);
                            log!(debug, "Auto-set base directory: {:?}", parent);
                        }
                    }
                }
            }
        }
    }

    // Set verbosity in the config and apply it globally
    config = config.with_verbosity(verbosity);
    config.apply();

    // Print the current verbosity level
    match verbosity {
        VERBOSITY_NONE => log!(info, "Verbosity: None"),
        VERBOSITY_ERROR => log!(info, "Verbosity: Errors only"),
        VERBOSITY_DEBUG => log!(info, "Verbosity: Debug messages"),
        VERBOSITY_INFO => log!(info, "Verbosity: All messages"),
        _ => log!(info, "Verbosity: Custom level {}", verbosity),
    }

    // Check if we should run handler tests
    if run_tests {
        log!(info, "Running handler tests...");
        tests::run_handler_tests();
        return;
    }

    // Check if we have an input file
    if let Some(input_path) = input_file {
        process_file(&input_path, &config);
    } else {
        log!(error, "Usage: {} [options] <input.c>", program_name);
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
            "  --new-arch             Use new extraction/conversion architecture"
        );
    }
}

fn process_file(input_path: &str, config: &Config) {
    log!(
        info,
        "Processing file with new architecture: {}",
        input_path
    );

    // Log include directories if any are configured
    if !config.include_dirs.is_empty() {
        log!(debug, "Using include directories:");
        for dir in &config.include_dirs {
            log!(debug, "  - {}", dir);
        }
    }

    // Read the input file
    let content = match fs::read_to_string(input_path) {
        Ok(c) => c,
        Err(e) => {
            log!(error, "Error reading input file: {}", e);
            return;
        }
    };

    // Tokenize the input
    let mut tokenizer = Tokenizer::new(&content);
    let tokens = tokenizer.tokenize();

    log!(debug, "Tokenization successful");

    // Extract C elements
    let mut extractor = Extractor::new();
    let elements = match extractor.extract_all(&tokens) {
        Ok(e) => e,
        Err(e) => {
            log!(error, "Error extracting C elements: {:?}", e);
            return;
        }
    };

    log!(debug, "Extracted {} elements from C code", elements.len());

    // Create a handler registry and register all handlers
    let mut registry = HandlerRegistry::new();

    // Pass the configuration when registering handlers
    register_handlers(&mut registry, config);

    // Create a converter with the handler registry and configuration
    let mut converter = Converter::new_with_config(registry, config.clone());

    // Convert all elements to Rust
    let rust_code = match converter.convert_all(&elements) {
        Ok(code) => code,
        Err(e) => {
            log!(error, "Error converting elements to Rust: {:?}", e);
            return;
        }
    };

    log!(debug, "Conversion successful, writing to output file");

    // Write to output file
    let output_path = input_path.to_string().replace(".c", ".rs");
    if let Err(e) = fs::write(&output_path, &rust_code) {
        log!(error, "Error writing output file: {}", e);
        return;
    }

    log!(
        info,
        "Successfully converted {} to {}",
        input_path,
        output_path
    );

    // Only print the code if verbosity level is high enough
    log!(debug, "Generated Rust code:\n{}", rust_code);
}
