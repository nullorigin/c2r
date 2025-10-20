#![allow(unused, static_mut_refs, unsafe_code)]
pub mod config;
pub mod convert;
pub mod entry;
pub mod error;
pub mod extract;
pub mod file_utils;
pub mod handler;
pub mod handlers;
pub mod info;
pub mod json;
pub mod lock;
pub mod logging;
pub mod macros;
pub mod map;
pub mod maybe;
pub mod option_lock;
pub mod pattern;
pub mod registry;
pub mod routing;
pub mod sample;
pub mod table;
pub mod tests;
pub mod thread;
pub mod token;
pub mod util;
pub use crate::config::*;
pub use crate::convert::*;
pub use crate::entry::*;
pub use crate::error::*;
pub use crate::extract::*;
pub use crate::handler::*;
pub use crate::handlers::*;
pub use crate::info::*;
pub use crate::lock::*;
pub use crate::logging::*;
pub use crate::maybe::*;
pub use crate::pattern::*;
pub use crate::registry::*;
pub use crate::routing::*;
pub use crate::sample::*;
pub use crate::table::*;
pub use crate::token::*;
// HashMap import removed - no longer needed for handler management
// Import logging macros
use std::env;
use std::fs;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
pub const TOKEN_MAX: usize = 8192;

#[allow(unused, static_mut_refs, unsafe_code)]
fn main() {
    let args: Vec<String> = env::args().collect();
    let mut args_iter = args.iter().skip(1);
    let program_name = args[0].clone();

    // Parse arguments
    let mut input_file = String::new();
    let mut output_file = String::new();
    let mut verbosity = 1;
    let mut run_tests = false;
    let mut command = String::new();
    let mut use_cache = true; // Default to using cache
    let mut explicit_cache_setting = false;
    let mut verbose_count = 1; // For the new logging sys
    let mut output_to_stdout = true;
    let mut base_dir = PathBuf::new();
    let mut context = Context::new();

    initialize(verbose_count);

    // Parse command line arguments
    while let Some(arg) = args_iter.next() {
        match arg.as_str() {
            "--test-handlers" => {
                run_tests = true;
            }
            "--display-registry" => {
                command = "display-registry".to_string();
            }
            "--analyze-patterns" => {
                command = "analyze-patterns".to_string();
            }
            "--clear-cache" => {
                command = "clear-cache".to_string();
            }
            "--show-cache" => {
                command = "show-cache".to_string();
            }
            "--create-demo-cache" => {
                command = "create-demo-cache".to_string();
            }
            "--clear-analysis-cache" => {
                command = "clear-analysis-cache".to_string();
            }
            "--cache" => {
                use_cache = true;
                explicit_cache_setting = true;
            }
            "--no-cache" => {
                use_cache = false;
                explicit_cache_setting = true;
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
                initialize(verbose_count);
            }
            "--output-file" | "-o" => {
                // Set output file
                if let Some(file_path) = args_iter.next() {
                    output_file = file_path.clone();
                    output_to_stdout = false;
                    debug!("Output file set to: {}", output_file);
                } else {
                    error!("Missing file path after {}", arg);
                }
            }
            "--base-dir" => {
                // Set base directory
                if let Some(dir_path) = args_iter.next() {
                    base_dir = PathBuf::from(dir_path);
                    debug!("Base directory set to: {}", base_dir.display());
                } else {
                    error!("Missing directory path after {}", arg);
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
                        if !parent.to_string_lossy().is_empty() {
                            context.set_base_dir(parent.to_path_buf());
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
                        if !parent.to_string_lossy().is_empty() {
                            context.set_base_dir(parent.to_path_buf());
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
                        if !parent.to_string_lossy().is_empty() {
                            context.set_base_dir(parent.to_path_buf());
                            debug!("Auto-set base directory: {:?}", parent);
                        }
                    }
                }
            }
        }
    }

    // CRITICAL: Initialize the Context and all global components
    context.initialize();

    // Print the current verbosity level
    match verbosity {
        VERBOSITY_WARN => warn!("Verbosity: Warnings and Errors"),
        VERBOSITY_ERROR => error!("Verbosity: Errors only"),
        VERBOSITY_DEBUG => debug!("Verbosity: Debug messages, Warnings and Errors"),
        VERBOSITY_INFO => info!("Verbosity: All messages"),
        _ => info!("Verbosity: Custom level {}", verbosity),
    }

    // Set base directory in the context
    if !base_dir.as_os_str().is_empty() {
        context.set_base_dir(base_dir);
    }

    // Handle pattern analysis command (doesn't require input file)
    if command.contains("analyze-patterns") {
        println!("🔍 Running Samplizer Pattern Analysis");
        if explicit_cache_setting {
            println!(
                "🗂️  Cache mode: {}",
                if use_cache { "Enabled" } else { "Disabled" }
            );
        } else {
            println!("🗂️  Cache mode: Enabled (default)");
        }
        println!(
            "═══════════════════════════════════════════════════════════════════════════════════════"
        );
        match context.handlizer.analyze_patterns(use_cache) {
            Ok(()) => {
                println!("✅ Pattern analysis completed successfully");
            }
            Err(e) => {
                error!("Pattern analysis failed: {}", e);
            }
        }
        return; // Exit after analysis
    }

    // Handle cache management commands
    if command.contains("clear-cache") {
        clear_cache();
        return;
    }

    if command.contains("show-cache") {
        show_cache();
        return;
    }

    if command == "clear-analysis-cache" {
        clear_analysis_cache();
        return;
    }

    if command == "create-demo-cache" {
        create_demo_cache();
        return;
    }

    // Handle test command
    if run_tests {
        println!("🧪 Running handler tests...");
        // Add test execution logic here
        return;
    }

    // Handle display registry command
    if command == "display-registry" {
        println!("📊 Displaying registry database...");
        context.display_registry_stats();
        return;
    }

    // Check if we have an input file for conversion
    if !input_file.is_empty() {
        // Process based on command
        match command.as_str() {
            "convert" | "" => {
                convert(&mut context, &input_file, &output_file, output_to_stdout);
            }
            _ => {
                error!("Unknown command: {}", command);
                print_usage(&program_name);
            }
        }
    } else if command.is_empty() {
        error!("No command or input file specified");
        print_usage(&program_name);
    }
}

fn print_usage(program_name: &str) {
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
    info!("  --analyze-patterns     Run pattern analysis with samplizer");
    info!("  --cache                Enable analysis result caching (default)");
    info!("  --no-cache             Disable analysis result caching, force fresh analysis");
    info!("  --clear-cache          Clear pattern cache to force fresh analysis");
    info!("  --clear-analysis-cache Clear analysis result cache files");
    info!("  --show-cache           Display information about pattern cache");
    info!("  --create-demo-cache    Create a demo pattern cache for testing");
    info!("  -o, --output-file <file> Write converted code to file instead of stdout");
    info!("  --stdout               Force output to stdout (default)");
    info!("");
    info!("Environment Variables:");
    info!("  C2R_CACHE_DIR          Directory for pattern cache (default: ~/.c2r/cache)");
}
pub fn convert(context: &mut Context, input_file: &str, output_file: &str, output_to_stdout: bool) {
    println!("Converting... {}", input_file);

    // Read input file
    let content = match fs::read_to_string(&input_file) {
        Ok(content) => {
            info!("File read successfully: {} chars", content.len());
            debug!(
                "First 50 chars: {}",
                &content[..std::cmp::min(50, content.len())]
            );
            content
        }
        Err(e) => {
            error!("Failed to read input file {}: {}", input_file, e);
            return;
        }
    };

    // Tokenize content
    debug!("Starting tokenization...");
    let tokens = match context.tokenizer.tokenize(content.as_bytes().to_vec()) {
        Ok(tokens) => {
            info!("Tokenization succeeded: {} tokens generated", tokens.len());
            if tokens.is_empty() {
                error!("No tokens generated from input file");
                return;
            }
            tokens
        }
        Err(e) => {
            error!("Tokenization failed: {}", e);
            return;
        }
    };

    // Setup cache directory
    let cache_dir = env::var("C2R_CACHE_DIR").unwrap_or_else(|_| {
        format!(
            "{}/.c2r/cache",
            env::var("HOME").unwrap_or_else(|_| "/tmp".to_string())
        )
    });

    info!("Using cache directory: {}", cache_dir);
    if let Err(e) = fs::create_dir_all(&cache_dir) {
        warn!("Failed to create cache directory: {}", e);
    } else {
        debug!("Cache directory ready: {}", cache_dir);
    }

    // Prepare token processing
    let token_count = tokens.len();
    let token_range = 0..token_count;
    let token_slot = {
        context.tokenizer.current_slot().clear();
        context.tokenizer.current_slot().append(tokens.clone());
        context.tokenizer.active_slot()
    };

    // Store tokens in registry and tokenbox
    context
        .registry
        .insert(Id::get("current_tokens"), Entry::TokenList(tokens.clone()));

    if let Ok(deposited_count) = context
        .tokenbox
        .deposit_from_slot(tokens.clone(), &Id::get("main_processing"))
    {
        debug!(
            "Deposited {} tokens into TokenBox for processing",
            deposited_count
        );
    }

    // Process tokens
    match context.process(token_slot, token_range) {
        Ok(results) => {
            // Extract converted code from results
            let mut converted_code = String::new();
            for result in &results {
                match result {
                    | HandlerResult::Completed(_, _, code, _)
                    | HandlerResult::Converted(_, _, code, _)
                    | HandlerResult::Extracted(_, _, code, _) => {
                        converted_code.push_str(&code);
                        converted_code.push('\n');
                    }
                    _ => {} // Other variants don't contain code
                }
            }

            // Format the final output
            let formatted_code = if !converted_code.trim().is_empty() {
                format_final_output(&converted_code)
            } else {
                "// No convertible code found\n".to_string()
            };

            // Output results
            if output_to_stdout || output_file.is_empty() {
                println!("\n// === Converted Rust Code ===");
                println!("{}", formatted_code);
            } else {
                match write_output_file(output_file, &formatted_code) {
                    Ok(()) => {
                        println!("Successfully wrote converted code to: {}", output_file);
                        println!(
                            "Generated {} lines of Rust code",
                            formatted_code.lines().count()
                        );
                    }
                    Err(e) => {
                        error!("Failed to write output file {}: {}", output_file, e);
                    }
                }
            }

            // Display processing reports
            let report_count = context.registry.get_reports().len();
            info!("Processing completed with {} reports", report_count);
            context.display_full_context();

            if get_verbosity_level() >= VERBOSITY_DEBUG {
                context.display_reports();
            }
        }
        Err(e) => {
            error!("Processing failed: {}", e);
        }
    }

    // Cleanup
    context.shutdown();
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
fn write_output_file(file_path: &str, code: &str) -> Result<()> {
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
pub fn show_cache() {
    let cache_dir = env::var("C2R_CACHE_DIR").unwrap_or_else(|_| {
        format!(
            "{}/.c2r/cache",
            env::var("HOME").unwrap_or_else(|_| "/tmp".to_string())
        )
    });

    let cache_file = Path::new(&cache_dir).join("pattern_cache.json");

    println!("📊 Pattern Cache Information");
    println!(
        "═══════════════════════════════════════════════════════════════════════════════════════"
    );
    println!("Expected cache directory: {}", cache_dir);
    println!("Expected cache file: {}", cache_file.display());

    // Check if cache directory exists
    if !Path::new(&cache_dir).exists() {
        println!("❌ Cache directory does not exist");
    } else {
        println!("✅ Cache directory exists");
    }

    if cache_file.exists() {
        match fs::read_to_string(&cache_file) {
            Ok(content) => {
                match json::parse(&content) {
                    Ok(json_data) => {
                        println!("✅ Cache file found and parsed successfully");

                        if let Some(metadata) = json_data["metadata"].as_object() {
                            if let Some(version) = metadata.get("version") {
                                println!("Version: {}", version);
                            }
                            if let Some(patternizer_name) = metadata.get("patternizer_name") {
                                println!("Patternizer: {}", patternizer_name);
                            }
                            if let Some(timestamp) = metadata.get("timestamp") {
                                println!("Created: {}", timestamp);
                            }
                            if let Some(cache_size) = metadata.get("cache_size") {
                                println!("Cache entries: {}", cache_size);
                            }
                            if let Some(pattern_count) = metadata.get("pattern_count") {
                                println!("Registered patterns: {}", pattern_count);
                            }
                        }

                        // Show cache content summary
                        if let Some(cache_obj) = json_data["cache"].as_object() {
                            println!(
                                "─────────────────────────────────────────────────────────────────────────────────────"
                            );
                            println!("Cache Content Summary:");
                            let mut cache_types: std::collections::HashMap<String, usize> =
                                std::collections::HashMap::new();

                            for node in cache_obj.iter() {
                                let key = node.key.as_str();
                                if let Some(result_type) = node.value["type"].as_str() {
                                    *cache_types.entry(result_type.to_string()).or_insert(0) += 1;
                                }

                                // Show first few cache entries as examples
                                if cache_types.values().sum::<usize>() <= 5 {
                                    println!(
                                        "  {} -> {}",
                                        key,
                                        node.value["type"].as_str().unwrap_or("Unknown")
                                    );
                                }
                            }

                            if cache_obj.len() > 5 {
                                println!("  ... and {} more entries", cache_obj.len() - 5);
                            }

                            println!(
                                "─────────────────────────────────────────────────────────────────────────────────────"
                            );
                            println!("Result Type Distribution:");
                            for (result_type, count) in cache_types {
                                println!("  {}: {} entries", result_type, count);
                            }
                        }

                        println!("File size: {} bytes", content.len());
                    }
                    Err(e) => {
                        println!("❌ Failed to parse cache file: {}", e);
                    }
                }
            }
            Err(e) => {
                println!("❌ Failed to read cache file: {}", e);
            }
        }
    } else {
        println!("❌ No cache file found");
        println!("💡 To create a cache, run: c2r convert <some_file.c>");
        println!("   The cache will be automatically generated during conversion.");
    }
}
pub fn clear_cache() {
    let cache_dir = env::var("C2R_CACHE_DIR").unwrap_or_else(|_| {
        format!(
            "{}/.c2r/cache",
            env::var("HOME").unwrap_or_else(|_| "/tmp".to_string())
        )
    });

    match fs::remove_dir_all(&cache_dir) {
        Ok(()) => {
            println!("🗑️  Pattern cache cleared successfully");
            println!("Cache directory removed: {}", cache_dir);
        }
        Err(e) => {
            if e.kind() == std::io::ErrorKind::NotFound {
                println!("ℹ️  No cache found to clear at: {}", cache_dir);
            } else {
                error!("Failed to clear cache: {}", e);
            }
        }
    }
    return;
}
pub fn clear_analysis_cache() {
    let cache_dir = env::var("C2R_CACHE_DIR").unwrap_or_else(|_| {
        format!(
            "{}/.c2r/cache",
            env::var("HOME").unwrap_or_else(|_| "/tmp".to_string())
        )
    });

    match fs::read_dir(&cache_dir) {
        Ok(entries) => {
            let mut cleared_count = 0;
            for entry in entries {
                if let Ok(entry) = entry {
                    let path = entry.path();
                    if let Some(file_name) = path.file_name().and_then(|n| n.to_str()) {
                        if file_name.starts_with("analysis_") && file_name.ends_with(".json") {
                            match fs::remove_file(&path) {
                                Ok(()) => {
                                    cleared_count += 1;
                                    println!("🗑️  Removed analysis cache: {}", file_name);
                                }
                                Err(e) => {
                                    error!("Failed to remove {}: {}", file_name, e);
                                }
                            }
                        }
                    }
                }
            }

            if cleared_count > 0 {
                println!("✅ Cleared {} analysis cache files", cleared_count);
            } else {
                println!("ℹ️  No analysis cache files found to clear");
            }
        }
        Err(e) => {
            if e.kind() == std::io::ErrorKind::NotFound {
                println!("ℹ️  No cache directory found at: {}", cache_dir);
            } else {
                error!("Failed to read cache directory: {}", e);
            }
        }
    }
    return;
}
pub fn create_demo_cache() {
    let cache_dir = env::var("C2R_CACHE_DIR").unwrap_or_else(|_| {
        format!(
            "{}/.c2r/cache",
            env::var("HOME").unwrap_or_else(|_| "/tmp".to_string())
        )
    });

    println!("🔧 Creating demo pattern cache...");

    // Ensure cache directory exists
    if let Err(e) = fs::create_dir_all(&cache_dir) {
        error!("Failed to create cache directory: {}", e);
        return;
    }

    // Create a Patternizer with some demo patterns and cache entries
    let mut patternizer = Patternizer::new();

    // Add some realistic cache entries that would be created during conversion
    patternizer.match_cache.insert(
        "function_handler:int|main|(|)|{".to_string(),
        PatternResult::Match { consumed_tokens: 8 },
    );

    patternizer.match_cache.insert(
        "function_handler:void|printf|(|const|char*|)|;".to_string(),
        PatternResult::NoMatch {
            reason: "Function call, not declaration".to_string(),
        },
    );

    patternizer.match_cache.insert(
        "struct_handler:struct|Point|{|int|x|;|int|y|;|}|;".to_string(),
        PatternResult::Match {
            consumed_tokens: 11,
        },
    );

    patternizer.match_cache.insert(
        "array_handler:int|arr|[|10|]|;".to_string(),
        PatternResult::CountOf {
            offsets: vec![0..2, 4..6],
        },
    );

    patternizer.match_cache.insert(
        "typedef_handler:typedef|struct|Node|{|...|}|Node|;".to_string(),
        PatternResult::Sequence { range: 0..7 },
    );
    let cache_file = Path::new(&cache_dir).join("pattern_cache.json");

    match patternizer.save_cache_to_json(&cache_file.to_string_lossy()) {
        Ok(()) => {
            println!("✅ Demo cache created successfully!");
            println!("Cache file: {}", cache_file.display());
            println!("Cache entries: {}", patternizer.match_cache.len());
            println!();
            println!("Now try: c2r --show-cache");
        }
        Err(e) => {
            error!("Failed to create demo cache: {}", e);
        }
    }
}
pub fn display_registry(context: &mut Context) {
    println!("🗂️  Handler Registry Display");
    println!(
        "═══════════════════════════════════════════════════════════════════════════════════════"
    );
    context.display_full_context();
}
