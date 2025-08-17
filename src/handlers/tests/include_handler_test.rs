use crate::handler::{TokenHandler, ParserContext};
use crate::token_parser::Tokenizer;
use crate::handlers::include_handler::IncludeHandler;
use crate::config::Config;
use tempfile::tempdir;
use std::fs;
use std::path::Path;

#[test]
fn test_local_include_handling() {
    // Create a temporary test directory
    let test_dir = tempdir().expect("Failed to create temporary directory");
    let test_path = test_dir.path().to_str().unwrap().to_string();
    
    // Create a mock header file that we can reference
    let header_path = test_dir.path().join("myheader.h");
    fs::write(&header_path, "// Test header content").expect("Failed to write test header");
    
    // Create a mock subdirectory with another header
    let subdir_path = test_dir.path().join("utils");
    fs::create_dir(&subdir_path).expect("Failed to create subdirectory");
    let subheader_path = subdir_path.join("helper.h");
    fs::write(&subheader_path, "// Helper header content").expect("Failed to write helper header");
    
    // Test cases for different include patterns
    let test_cases = vec![
        (
            r#"#include "myheader.h""#, 
            format!("// Include: myheader.h\nmod myheader;")
        ),
        (
            r#"#include "utils/helper.h""#, 
            format!("// Include: utils/helper.h\nmod utils {{ mod helper; }}")
        ),
        (
            r#"#include <stdio.h>"#, 
            String::from("// System include: stdio.h")
        ),
        (
            r#"#include <stdlib.h>"#, 
            String::from("// System include: stdlib.h")
        ),
    ];
    
    for (c_code, expected_rust) in test_cases {
        // Create a config with our test directory as base
        let mut config = Config::new();
        config.base_dir = test_path.clone();
        
        // Create a handler with the config
        let handler = IncludeHandler::new(config);
        
        // Tokenize the C code
        let mut tokenizer = Tokenizer::new(c_code);
        let tokens = tokenizer.tokenize();
        
        // Test the handler
        let result = handler.handle(&tokens, &mut ParserContext::new()).unwrap();
        
        match result {
            crate::handler::HandlerResult::RustCode(rust_code) => {
                assert_eq!(rust_code.trim(), expected_rust.trim());
            },
            _ => panic!("Expected RustCode result, got {:?}", result),
        }
    }
}

#[test]
fn test_nested_include_paths() {
    // Create a temporary test directory
    let test_dir = tempdir().expect("Failed to create temporary directory");
    let test_path = test_dir.path().to_str().unwrap().to_string();
    
    // Create a nested directory structure
    let nested_dirs = vec![
        "include",
        "include/sys",
        "include/common",
        "include/common/utils",
    ];
    
    for dir in nested_dirs {
        let dir_path = test_dir.path().join(dir);
        fs::create_dir_all(&dir_path).expect(&format!("Failed to create directory {}", dir));
    }
    
    // Create some mock header files in the nested structure
    let headers = vec![
        ("include/common/types.h", "// Common types"),
        ("include/sys/config.h", "// System configuration"),
        ("include/common/utils/helpers.h", "// Helper functions"),
    ];
    
    for (header_path, content) in headers {
        let full_path = test_dir.path().join(header_path);
        fs::write(&full_path, content).expect(&format!("Failed to write header {}", header_path));
    }
    
    // Create a Config with include directories
    let mut config = Config::new();
    config.base_dir = test_path.clone();
    config.include_dirs = vec![
        format!("{}/include", test_path),
        format!("{}/include/common", test_path),
    ];
    
    // Create an IncludeHandler with the config
    let handler = IncludeHandler::new(config);
    
    // Test cases with different include paths
    let test_cases = vec![
        (
            r#"#include "types.h""#,
            format!("// Include: types.h\nmod types;")
        ),
        (
            r#"#include "utils/helpers.h""#,
            format!("// Include: utils/helpers.h\nmod utils {{ mod helpers; }}")
        ),
    ];
    
    for (c_code, expected_rust) in test_cases {
        // Tokenize the C code
        let mut tokenizer = Tokenizer::new(c_code);
        let tokens = tokenizer.tokenize();
        
        // Test the handler
        let result = handler.handle(&tokens, &mut ParserContext::new()).unwrap();
        
        match result {
            crate::handler::HandlerResult::RustCode(rust_code) => {
                assert_eq!(rust_code.trim(), expected_rust.trim());
            },
            _ => panic!("Expected RustCode result, got {:?}", result),
        }
    }
}

#[test]
fn test_system_includes_handling() {
    // Create a config with system includes enabled
    let mut config = Config::new();
    config.process_system_includes = true;
    
    // Create handler
    let handler = IncludeHandler::new(config);
    
    // Test cases for system includes
    let test_cases = vec![
        (
            r#"#include <stdio.h>"#,
            "System include: stdio.h"
        ),
        (
            r#"#include <stdlib.h>"#,
            "System include: stdlib.h"
        ),
    ];
    
    for (c_code, expected_content) in test_cases {
        // Tokenize the C code
        let mut tokenizer = Tokenizer::new(c_code);
        let tokens = tokenizer.tokenize();
        
        // Test the handler
        let result = handler.handle(&tokens, &mut ParserContext::new()).unwrap();
        
        match result {
            crate::handler::HandlerResult::RustCode(rust_code) => {
                // Just check that the expected content is in the result
                // since the exact format may change
                assert!(rust_code.contains(expected_content));
            },
            _ => panic!("Expected RustCode result, got {:?}", result),
        }
    }
}
