#![allow(unused, static_mut_refs, unsafe_code)]

// Core modules
pub mod coord;
pub mod db;
pub mod error;
pub mod file_utils;
pub mod handlers;
pub mod info;
pub mod lock;
pub mod logging;
pub mod macros;
pub mod map;
pub mod system;
pub mod thread;
pub mod util;

// Re-exports
pub use crate::error::*;
pub use crate::logging::*;
pub use crate::system::*;

use std::env;
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};

pub const TOKEN_MAX: usize = 8192;

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut args_iter = args.iter().skip(1);
    let program_name = &args[0];

    // Configuration
    let mut input_file = String::new();
    let mut output_file = String::new();
    let mut command = String::new();
    let mut verbose_count: u64 = 1;
    let mut output_to_stdout = true;
    let mut base_dir = PathBuf::new();

    // Initialize logging
    initialize(verbose_count);

    // Parse command line arguments
    while let Some(arg) = args_iter.next() {
        match arg.as_str() {
            "--display-db" | "--display-database" => {
                command = "display-db".to_string();
            }
            "--clear-cache" => {
                command = "clear-cache".to_string();
            }
            "--show-cache" => {
                command = "show-cache".to_string();
            }
            "-v" | "--verbose" => {
                if let Some(level_str) = args_iter.next() {
                    if let Ok(level) = level_str.parse::<u64>() {
                        verbose_count = level;
                    } else {
                        verbose_count = 3;
                    }
                } else {
                    verbose_count = 3;
                }
                initialize(verbose_count);
            }
            "-o" | "--output-file" => {
                if let Some(file_path) = args_iter.next() {
                    output_file = file_path.clone();
                    output_to_stdout = false;
                    debug!("Output file: {}", output_file);
                } else {
                    error!("Missing file path after {}", arg);
                }
            }
            "--base-dir" => {
                if let Some(dir_path) = args_iter.next() {
                    base_dir = PathBuf::from(dir_path);
                    debug!("Base directory: {}", base_dir.display());
                } else {
                    error!("Missing directory path after {}", arg);
                }
            }
            "--stdout" => {
                output_to_stdout = true;
                output_file.clear();
            }
            "--help" | "-h" => {
                print_usage(program_name);
                return;
            }
            "convert" => {
                command = "convert".to_string();
                if let Some(file_path) = args_iter.next() {
                    input_file = file_path.clone();
                    if let Some(parent) = Path::new(&file_path).parent() {
                        if !parent.as_os_str().is_empty() && base_dir.as_os_str().is_empty() {
                            base_dir = parent.to_path_buf();
                            debug!("Auto-set base directory: {:?}", parent);
                        }
                    }
                }
            }
            _ => {
                if !arg.starts_with('-') {
                    if command.is_empty() {
                        command = arg.clone();
                    } else if input_file.is_empty() {
                        input_file = arg.clone();
                        if let Some(parent) = Path::new(arg).parent() {
                            if !parent.as_os_str().is_empty() && base_dir.as_os_str().is_empty() {
                                base_dir = parent.to_path_buf();
                            }
                        }
                    }
                }
            }
        }
    }

    // Initialize system
    let mut system = System::new();
    system.initialize(false);

    // Execute command
    match command.as_str() {
        "clear-cache" => {
            clear_cache();
        }
        "show-cache" => {
            show_cache();
        }
        "display-db" => {
            println!("📊 Database Contents:");
            system.display();
        }
        "convert" | "" if !input_file.is_empty() => {
            if let Err(e) = convert(&mut system, &input_file, &output_file, output_to_stdout) {
                error!("Conversion failed: {}", e);
            }
        }
        "" => {
            error!("No command specified");
            print_usage(program_name);
        }
        _ => {
            error!("Unknown command: {}", command);
            print_usage(program_name);
        }
    }

    system.shutdown(true);
}

fn print_usage(program_name: &str) {
    println!("c2r - C to Rust Converter");
    println!();
    println!("Usage: {} [options] <command> [file]", program_name);
    println!();
    println!("Commands:");
    println!("  convert <file.c>       Convert a C file to Rust");
    println!("  --display-db           Display database contents");
    println!("  --show-cache           Show cache information");
    println!("  --clear-cache          Clear the pattern cache");
    println!();
    println!("Options:");
    println!("  -v, --verbose [level]  Set verbosity (0-4, default: 1)");
    println!("  -o, --output-file      Write output to file");
    println!("  --base-dir <dir>       Set base directory for includes");
    println!("  --stdout               Force output to stdout");
    println!("  -h, --help             Show this help");
    println!();
    println!("Verbosity Levels:");
    println!("  0: Errors only");
    println!("  1: Warnings");
    println!("  2: Info");
    println!("  3: Debug");
    println!("  4: Trace");
}

fn convert(
    system: &mut System,
    input_file: &str,
    output_file: &str,
    output_to_stdout: bool,
) -> Result<()> {
    info!("Converting: {}", input_file);

    // Read input file
    let content = fs::read_to_string(input_file).map_err(|e| {
        Error::new(
            Kind::Io,
            Reason::File("could not be read"),
            Some(e.to_string()),
        )
    })?;

    info!("Read {} bytes", content.len());

    // TODO: Implement tokenization and conversion using db system
    // For now, just output a placeholder
    let converted = format!(
        "// Generated by c2r\n// Source: {}\n// TODO: Implement conversion\n\n// Original size: {} bytes\n",
        input_file,
        content.len()
    );

    // Output
    if output_to_stdout || output_file.is_empty() {
        println!("{}", converted);
    } else {
        write_output_file(output_file, &converted)?;
        info!("Wrote output to: {}", output_file);
    }

    Ok(())
}

fn write_output_file(file_path: &str, code: &str) -> Result<()> {
    if let Some(parent) = Path::new(file_path).parent() {
        fs::create_dir_all(parent)?;
    }
    let mut file = fs::File::create(file_path)?;
    file.write_all(code.as_bytes())?;
    file.flush()?;
    Ok(())
}

fn show_cache() {
    let cache_dir = get_cache_dir();
    let cache_file = Path::new(&cache_dir).join("cache.db");

    println!("📊 Cache Information");
    println!("════════════════════════════════════════");
    println!("Cache directory: {}", cache_dir);

    if !Path::new(&cache_dir).exists() {
        println!("❌ Cache directory does not exist");
        return;
    }

    println!("✅ Cache directory exists");

    if cache_file.exists() {
        if let Ok(metadata) = fs::metadata(&cache_file) {
            println!("✅ Cache file exists ({} bytes)", metadata.len());
        }
    } else {
        println!("ℹ️  No cache file found");
    }
}

fn clear_cache() {
    let cache_dir = get_cache_dir();

    match fs::remove_dir_all(&cache_dir) {
        Ok(()) => {
            println!("🗑️  Cache cleared: {}", cache_dir);
        }
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
            println!("ℹ️  No cache to clear");
        }
        Err(e) => {
            error!("Failed to clear cache: {}", e);
        }
    }
}

fn get_cache_dir() -> String {
    env::var("C2R_CACHE_DIR").unwrap_or_else(|_| {
        format!(
            "{}/.c2r/cache",
            env::var("HOME").unwrap_or_else(|_| "/tmp".to_string())
        )
    })
}
