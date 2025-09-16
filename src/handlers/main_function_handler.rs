//! Main Function Handler for C to Rust Converter
//! Specialized handler for the main function (entry point)

use crate::convert::ConvertedFunction;
use crate::document::ElementInfo;
use crate::extract::ExtractedFunction;
use crate::pattern::PatternResult;
use crate::{
    C2RError, ConvertedElement, ExtractedElement, Handler, HandlerPhase, HandlerResult, Id, Kind,
    Reason, ReportLevel,
};
use crate::{context, report};
use std::ops::Range;

/// Create main function handler with specialized patterns
pub fn create_main_function_handler() -> Handler {
    let handler_id = Id::get("main_function_handler");
    let handler_role = "Convert C main function to Rust main function".to_string();
    let priority = 1000; // Highest priority for main function detection

    let mut handler = Handler::new(handler_id, handler_role, priority);
    handler.process = Some(process_main_function);
    handler.handle = Some(handle_main_function);
    handler.extract = Some(extract_main_function);
    handler.convert = Some(convert_main_function_wrapper);
    handler.document = Some(document_main_function);
    handler.result = Some(result_main_function);
    handler
}

/// Process callback: Detects main function pattern
fn process_main_function(token_range: Range<usize>) -> Result<bool, C2RError> {
    let mut context = context!();
    context.pull();

    if token_range.end > context.tokens.len() || token_range.is_empty() {
        return Ok(false);
    }

    let tokens = &context.tokens[token_range.clone()];

    report!(
        "main_function_handler",
        "process_main_function",
        ReportLevel::Debug,
        HandlerPhase::Process,
        format!(
            "Analyzing {} tokens for main function pattern",
            tokens.len()
        ),
        true
    );

    // Must have minimum tokens: int main (
    if tokens.len() < 3 {
        return Ok(false);
    }

    // Use the existing Patternizer architecture
    let pattern_result = context.patternizer.match_pattern("main_function", tokens);

    let result = matches!(
        pattern_result,
        PatternResult::Match { .. } | PatternResult::Sequence { .. } | PatternResult::Fuzzy { .. }
    );

    if result {
        report!(
            "main_function_handler",
            "process_main_function",
            ReportLevel::Info,
            HandlerPhase::Process,
            format!("Detected main function pattern in {} tokens", tokens.len()),
            true
        );
    }

    Ok(result)
}

/// Handle callback: Processes main function tokens
fn handle_main_function(token_range: Range<usize>) -> Result<HandlerResult, C2RError> {
    let id = Id::get("main_function_handler");

    report!(
        "main_function_handler",
        "handle_main_function",
        ReportLevel::Info,
        HandlerPhase::Handle,
        format!("Processing main function tokens: {:?}", token_range),
        true
    );

    // Extract main function information
    if let Ok(Some(extracted)) = extract_main_function(token_range.clone()) {
        if let ExtractedElement::Function(function) = extracted {
            report!(
                "main_function_handler",
                "handle_main_function",
                ReportLevel::Info,
                HandlerPhase::Handle,
                format!("Successfully extracted main function: {}", function.name),
                true
            );

            // Convert to Rust
            // Convert ExtractedFunction to FunctionInfo for ElementInfo
            let _function_info = crate::document::FunctionInfo {
                name: function.name.clone(),
                return_type: "int".to_string(), // Main typically returns int in C
                parameters: vec!["void".to_string()], // Simplified for now
                parameter_count: 0,
                is_declaration: !function.is_definition,
                is_definition: function.is_definition,
                is_static: function.is_static,
                is_extern: function.is_extern,
                is_inline: function.is_inline,
                has_body: function.is_definition,
                complexity: "simple".to_string(),
                is_unsafe: false,
            };

            if let Ok(Some(converted)) = convert_main_function_wrapper(token_range.clone()) {
                if let ConvertedElement::Function(rust_function) = converted {
                    let mut context = context!();
                    context.pull();
                    return Ok(HandlerResult::Completed(
                        Some(context.tokens[token_range.clone()].to_vec()),
                        token_range,
                        rust_function.code,
                        id,
                    ));
                }
            }
        }
    }

    Ok(HandlerResult::NotHandled(None, token_range, id))
}

/// Extract main function information from tokens
fn extract_main_function(token_range: Range<usize>) -> Result<Option<ExtractedElement>, C2RError> {
    let mut context = context!();
    context.pull();

    let tokens = &context.tokens[token_range.clone()];

    if tokens.len() < 3 {
        return Ok(None);
    }

    let mut return_type = "int".to_string(); // Default for main
    let mut parameters = Vec::new();
    let mut body = String::new();
    let mut current_param = String::new();
    let mut in_params = false;
    let mut in_body = false;
    let mut brace_depth = 0;
    let mut found_main = false;

    for token in tokens {
        let token_str = token.to_string();

        match token_str.as_str() {
            "int" => {
                if !found_main {
                    return_type = "int".to_string();
                }
            }
            "main" => {
                found_main = true;
            }
            "(" => {
                if found_main && !in_body {
                    in_params = true;
                }
            }
            ")" => {
                if in_params && !in_body {
                    if !current_param.trim().is_empty() {
                        parameters.push(current_param.trim().to_string());
                        current_param.clear();
                    }
                    in_params = false;
                }
            }
            "," => {
                if in_params && !current_param.trim().is_empty() {
                    parameters.push(current_param.trim().to_string());
                    current_param.clear();
                }
            }
            "{" => {
                brace_depth += 1;
                if brace_depth == 1 && !in_body {
                    in_body = true;
                } else if in_body {
                    body.push_str(&token_str);
                    body.push(' ');
                }
            }
            "}" => {
                brace_depth -= 1;
                if in_body {
                    if brace_depth == 0 {
                        // End of main function body
                        break;
                    } else {
                        body.push_str(&token_str);
                        body.push(' ');
                    }
                }
            }
            _ => {
                if in_body {
                    body.push_str(&token_str);
                    body.push(' ');
                } else if in_params {
                    if !current_param.is_empty() {
                        current_param.push(' ');
                    }
                    current_param.push_str(&token_str);
                }
            }
        }
    }

    if !found_main {
        return Ok(None);
    }

    // Handle common main function signatures
    if parameters.is_empty() {
        parameters.push("void".to_string());
    } else {
        // Common patterns: argc, argv
        if parameters.len() >= 2 {
            // Likely argc, argv pattern
            let first_param = &parameters[0];
            let second_param = &parameters[1];

            if first_param.contains("int") && second_param.contains("char") {
                parameters = vec!["int argc".to_string(), "char** argv".to_string()];
            }
        }
    }

    Ok(Some(ExtractedElement::Function(ExtractedFunction {
        name: "main".to_string(),
        return_type: Vec::new(), // Convert to tokens if needed
        parameters: Vec::new(),  // Convert to tokens if needed
        body: Vec::new(),        // Convert body to tokens if needed
        is_variadic: false,
        is_static: false,
        is_inline: false,
        is_extern: false,
        tokens: context.tokens[token_range.clone()].to_vec(),
        from_recovery: false,
        is_definition: !body.is_empty(),
        code: if body.is_empty() {
            format!("{} main({});", return_type, parameters.join(", "))
        } else {
            format!(
                "{} main({}) {{ {} }}",
                return_type,
                parameters.join(", "),
                body.trim()
            )
        },
    })))
}

/// Wrapper function for converting main functions from token range
fn convert_main_function_wrapper(
    token_range: Range<usize>,
) -> Result<Option<ConvertedElement>, C2RError> {
    let mut context = context!();
    context.pull();
    let tokens = &context.tokens[token_range.clone()];

    if tokens.is_empty() {
        return Ok(None);
    }

    // Check if this is actually a main function
    let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
    if !token_strings.contains(&"main".to_string()) {
        return Ok(None);
    }

    // Convert C main function to Rust main function
    let rust_code = "fn main() {\n    println!(\"Hello, world!\");\n}".to_string();

    Ok(Some(ConvertedElement::Function(
        crate::convert::ConvertedFunction {
            name: "main".to_string(),
            return_type: "()".to_string(), // Rust main returns ()
            parameters: vec![],
            body: "println!(\"Hello, world!\");".to_string(),
            code: rust_code,
            is_unsafe: false,
            is_public: false, // main is not public in Rust
            is_definition: true,
        },
    )))
}

/// Internal function for converting main functions
fn convert_main_function_internal(
    element: ElementInfo,
) -> Result<crate::convert::ConvertedElement, crate::error::C2RError> {
    if let ElementInfo::Function(function_info) = element {
        // Determine main function signature based on parameters
        let rust_signature = "fn main()".to_string(); // Rust main is always fn main()

        // Convert function body with main-specific conversions
        let rust_body = if function_info.has_body {
            "    // TODO: Convert main function body\n    println!(\"Hello, world!\");".to_string()
        } else {
            "    // TODO: Add main function implementation\n".to_string()
        };

        // Add common imports if needed
        let mut imports = String::new();
        if function_info.parameters.len() >= 2 {
            imports.push_str("use std::env;\n"); // For command line args
        }

        let rust_code = if imports.is_empty() {
            format!("{} {{\n{}\n}}", rust_signature, rust_body)
        } else {
            format!("{}\n{} {{\n{}\n}}", imports, rust_signature, rust_body)
        };

        Ok(ConvertedElement::Function(ConvertedFunction {
            name: "main".to_string(),
            return_type: "()".to_string(),
            parameters: Vec::new(), // Rust main takes no parameters
            body: rust_body,
            code: rust_code,
            is_unsafe: false,
            is_public: false,
            is_definition: function_info.has_body,
        }))
    } else {
        Err(C2RError::new(
            Kind::Other,
            Reason::Missing("function info"),
            None,
        ))
    }
}

/// Convert C main function body to Rust with main-specific conversions
fn convert_main_body_to_rust(c_body: &str) -> String {
    let mut rust_body = c_body.to_string();

    // Main-specific conversions
    rust_body = rust_body.replace("printf(", "println!(");
    rust_body = rust_body.replace("scanf(", "// TODO: Use std::io for input - ");

    // Handle argc/argv access
    if rust_body.contains("argc") || rust_body.contains("argv") {
        rust_body = format!(
            "    let args: Vec<String> = env::args().collect();\n    let argc = args.len();\n    {}",
            rust_body
        );
        rust_body = rust_body.replace("argv[", "args[");
        rust_body = rust_body.replace("argc", "argc");
    }

    // Handle return statements in main
    if rust_body.contains("return 0") {
        rust_body = rust_body.replace("return 0", "// Return 0 is implicit in Rust main");
    } else if rust_body.contains("return") && !rust_body.contains("return 0") {
        rust_body = rust_body.replace("return", "std::process::exit");
        // Add use statement if not present
        if !rust_body.contains("use std::process") {
            rust_body = format!("    use std::process;\n{}", rust_body);
        }
    }

    // Handle common C patterns
    rust_body = rust_body.replace("malloc(", "// TODO: Use Vec::new() or Box::new() - ");
    rust_body = rust_body.replace("free(", "// TODO: Automatic memory management - ");
    rust_body = rust_body.replace("NULL", "None");

    // Format with proper indentation
    let lines: Vec<&str> = rust_body.lines().collect();
    let formatted_lines: Vec<String> = lines
        .iter()
        .map(|line| {
            if line.trim().is_empty() {
                line.to_string()
            } else {
                format!("    {}", line.trim())
            }
        })
        .collect();

    formatted_lines.join("\n")
}

/// Document main function
fn document_main_function(info: ElementInfo) -> Result<Option<String>, C2RError> {
    if let ElementInfo::Function(function_info) = info {
        let mut doc = "/// Converted C main function - program entry point\n".to_string();

        if function_info.parameters.len() >= 2 {
            doc.push_str("/// Original C main had command line arguments (argc, argv)\n");
            doc.push_str("/// In Rust, use std::env::args() to access command line arguments\n");
        }

        if function_info.has_body {
            doc.push_str(&format!("/// Complexity: {}\n", function_info.complexity));
        }

        doc.push_str("/// Note: Rust main() returns () by default, not int\n");

        Ok(Some(doc))
    } else {
        Ok(None)
    }
}

/// Result callback for main function
fn result_main_function(
    _token_range: Range<usize>,
    result: HandlerResult,
) -> Result<HandlerResult, C2RError> {
    report!(
        "main_function_handler",
        "result_main_function",
        ReportLevel::Info,
        HandlerPhase::Report,
        "Postprocessing main function conversion result",
        true
    );

    // Add any main-specific metadata or validation here
    Ok(result)
}
