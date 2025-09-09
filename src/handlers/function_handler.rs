#![allow(
    unused_variables,
    unused_assignments,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals
)]

use super::common::{self, find_matching_token, not_handled};
use crate::ReportLevel::{Error, Info};
use crate::config::HandlerPhase::{Convert, Extract, Handle, Process, Report};
use crate::config::HandlerReport;
use crate::error::ConversionError;
use crate::extract::ExtractedElement;
use crate::extract::ExtractedFunction;
use crate::handler::HandlerResult;
use crate::lock::Id;
use crate::{ConvertedElement, ConvertedFunction, Token, convert_type};
use crate::{ReportLevel, context, report};

/// Creates a function handler that can detect and convert C function declarations and definitions
pub fn create_function_handler() -> crate::handler::Handler {
    let handler_id = Id::get("function_handler");
    let handler_role = "function";
    let priority = 200; // Higher priority than expression handler (150) - must run first

    super::create_handler(
        handler_id,
        handler_role,
        priority,
        Some(process_function),
        Some(handle_function),
        Some(extract_function),
        Some(convert_function),
        Some(report_function),
        Some(result_function),
        Some(redirect_function),
    )
}

/// Process callback: Initializes and confirms this handler can handle the tokens
fn process_function(tokens: &[Token]) -> Result<bool, ConversionError> {
    // Validate input
    if tokens.len() < 4 {
        return Ok(false);
    }

    // Look for function patterns: return_type function_name(parameters)
    for i in 1..tokens.len().saturating_sub(1) {
        if tokens[i + 1].to_string() == "(" {
            // Check if tokens before this point could form a valid type
            if common::is_type(&tokens[0..i]) {
                // Additional validation: ensure this looks like a function
                if let Some(closing_paren) = find_matching_token(&tokens[i + 1..], "(", ")") {
                    let paren_end = i + 1 + closing_paren + 1;

                    // Check for function body or semicolon
                    if paren_end < tokens.len() {
                        let next_token = &tokens[paren_end].to_string();
                        if next_token == "{" || next_token == ";" {
                            report!(
                                "function_handler",
                                "process_function",
                                Info,
                                Handle,
                                format!("Function pattern detected: {}", tokens[i].to_string()),
                                true
                            );
                            return Ok(true);
                        }
                    }
                }
            }
        }
    }
    Ok(false)
}

/// Processes a function declaration or definition
fn handle_function(tokens: &[Token]) -> Result<HandlerResult, ConversionError> {
    let id = Id::get("handle_function");
    report!(
        "function_handler",
        "handle_function",
        Info,
        Process,
        format!("Processing {} tokens for function handling", tokens.len()),
        true
    );

    // Find function name and opening parenthesis
    let mut name_pos = 0;
    for i in 0..tokens.len() - 1 {
        if matches!(tokens[i + 1].to_string().as_str(), "(") {
            name_pos = i;
            break;
        }
    }

    if name_pos == 0 {
        return not_handled();
    }

    let func_name = tokens[name_pos].to_string();
    report!(
        "function_handler",
        "handle_function",
        Info,
        Process,
        format!("Found function: {}", func_name),
        true
    );

    // Return type is everything before the name
    let return_type_tokens = &tokens[0..name_pos];

    // Find the parameter list between parentheses
    let params_start = name_pos + 1; // Opening parenthesis
    let params_end = match find_matching_token(&tokens[params_start..], "(", ")") {
        Some(pos) => params_start + pos + 1, // +1 for the closing parenthesis
        None => {
            report!(
                "function_handler",
                "handle_function",
                Error,
                Process,
                format!(
                    "Could not find closing parenthesis for function {}",
                    func_name
                ),
                false
            );
            return not_handled();
        }
    };

    // Extract parameter tokens
    let param_tokens = &tokens[params_start + 1..params_end];

    // Check if this is a function declaration or definition
    let is_definition = params_end < tokens.len() && tokens[params_end].to_string() == "{";

    if is_definition {
        // Find the body between braces
        let body_start = params_end;
        let body_end = match find_matching_token(&tokens[body_start..], "{", "}") {
            Some(pos) => body_start + pos + 1,
            None => {
                report!(
                    "function_handler",
                    "handle_function",
                    Error,
                    Process,
                    format!("Could not find closing brace for function {}", func_name),
                    false
                );
                return not_handled();
            }
        };

        // Extract body tokens
        let body_tokens = &tokens[body_start + 1..body_end - 1];

        // Convert to Rust
        if let Some(ConvertedElement::Function(converted_function)) = convert_function(tokens)? {
            // CRITICAL FIX: Use actual range from 0 to body_end to consume all processed tokens
            let token_range = 0..body_end;
            report!(
                "function_handler",
                "handle_function",
                Info,
                Process,
                format!(
                    "Function definition {} consumes tokens 0..{} ({} tokens)",
                    func_name, body_end, body_end
                ),
                true
            );
            common::replace_with_range(
                converted_function.rust_code.clone(),
                token_range,
                Id::get("handle_function"),
            )
        } else {
            Err(ConversionError::new("Failed to convert function"))
        }
    } else {
        // Function declaration only (no body) - find semicolon end
        let semicolon_pos = match tokens[params_end..]
            .iter()
            .position(|t| t.to_string() == ";")
        {
            Some(pos) => params_end + pos + 1, // +1 to include the semicolon
            None => {
                report!(
                    "function_handler",
                    "handle_function",
                    Error,
                    Process,
                    format!(
                        "Could not find semicolon for function declaration {}",
                        func_name
                    ),
                    false
                );
                return not_handled();
            }
        };

        if let Some(ConvertedElement::Function(converted_function)) = convert_function(tokens)? {
            // CRITICAL FIX: Use actual range from 0 to semicolon_pos to consume all processed tokens
            let token_range = 0..semicolon_pos;
            report!(
                "function_handler",
                "handle_function",
                Info,
                Process,
                format!(
                    "Function declaration {} consumes tokens 0..{} ({} tokens)",
                    func_name, semicolon_pos, semicolon_pos
                ),
                true
            );
            common::replace_with_range(
                converted_function.rust_code.clone(),
                token_range,
                Id::get("handle_function"),
            )
        } else {
            Err(ConversionError::new("Failed to convert function"))
        }
    }
}

/// Extract callback: Finds and returns the locations this handler should handle
pub fn extract_function(tokens: &[Token]) -> Result<Option<ExtractedElement>, ConversionError> {
    report!(
        "function_handler",
        "extract_function",
        Info,
        Extract,
        format!("Extracting function from {} tokens", tokens.len()),
        true
    );
    // Find function name and opening parenthesis
    let mut name_pos = 0;
    for i in 0..tokens.len().saturating_sub(1) {
        if matches!(tokens[i + 1].to_string().as_str(), "(") {
            name_pos = i;
            break;
        }
    }

    if name_pos == 0 {
        return Ok(None);
    }

    let func_name = tokens[name_pos].to_string();
    report!(
        "function_handler",
        "extract_function",
        Info,
        Extract,
        format!("Extracting function: {}", func_name),
        true
    );
    let return_type_tokens = tokens[0..name_pos].to_vec();

    // Find the parameter list between parentheses
    let params_start = name_pos + 1;
    let params_end = match find_matching_token(&tokens[params_start..], "(", ")") {
        Some(pos) => params_start + pos + 1,
        None => return Ok(None),
    };

    // Extract parameter tokens
    let param_tokens = tokens[params_start + 1..params_end].to_vec();

    // Check if this is a function declaration or definition
    let is_definition = params_end < tokens.len() && tokens[params_end].to_string() == "{";
    let body_tokens = if is_definition {
        // Find the body between braces
        let body_start = params_end;
        let body_end = match find_matching_token(&tokens[body_start..], "{", "}") {
            Some(pos) => body_start + pos + 1,
            None => return Ok(None),
        };

        tokens[body_start + 1..body_end - 1].to_vec()
    } else {
        vec![]
    };

    // Check for static and inline modifiers
    let is_static = return_type_tokens.iter().any(|t| t.to_string() == "static");
    let is_inline = return_type_tokens
        .iter()
        .any(|t| matches!(t.to_string().as_str(), "inline" | "__inline" | "__inline__"));

    // Create ExtractedFunction
    let extracted_function = ExtractedFunction {
        name: func_name,
        return_type: return_type_tokens,
        parameters: param_tokens,
        body: body_tokens,
        is_definition,
        is_static,
        is_inline,
        original_code: tokens
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<_>>()
            .join(" "),
        tokens: tokens.to_vec(),
        is_variadic: false,
        from_recovery: false,
    };

    Ok(Some(ExtractedElement::Function(extracted_function)))
}

/// Convert callback: Does the actual conversion of C to Rust code
fn convert_function(tokens: &[Token]) -> Result<Option<ConvertedElement>, ConversionError> {
    report!(
        "function_handler",
        "convert_function",
        Info,
        Convert,
        format!("Converting function from {} tokens", tokens.len()),
        true
    );
    let id = Id::get("convert_function");
    if let ExtractedElement::Function(element) = extract_function(tokens)?.unwrap() {
        let mut rust_code = String::new();
        // Add function visibility
        rust_code.push_str("pub ");

        // Check for special cases like 'extern "C"'
        if element
            .return_type
            .iter()
            .any(|t| t.to_string() == "extern")
        {
            rust_code.push_str("extern \"C\" ");
        }

        // Add function declaration
        rust_code.push_str("fn ");
        rust_code.push_str(element.name.as_str());
        rust_code.push('(');

        // Convert parameters using improved parsing
        let params = convert_parameters(&element.parameters);

        rust_code.push_str(&params);
        rust_code.push(')');

        // Convert return type using improved parsing
        let return_type = convert_return_type(&element.return_type);

        if return_type != "()" {
            rust_code.push_str(" -> ");
            rust_code.push_str(&return_type);
        }

        // Add function body if present
        let body = element.body.clone();
        rust_code.push_str(" {\n");

        // Convert body content - simplified version here
        rust_code.push_str("    // TODO: Convert function body\n");

        // Real implementation would convert each statement
        let body_content = body
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<_>>()
            .join(" ");

        // Simple C to Rust conversion - placeholder for actual conversion
        let rust_body = "    // Original C code: \n    // ".to_string() + &body_content;
        rust_code.push_str(&rust_body);
        rust_code.push_str("\n}");
        Ok(Some(ConvertedElement::Function(ConvertedFunction {
            return_type: "()".to_string(), // Default return type
            parameters: Vec::new(),        // Default empty parameters
            body: "".to_string(),          // Default empty body
            rust_code,
            is_unsafe: false,
            is_public: false,
        })))
    } else {
        Err(ConversionError::new("Failed to convert function"))
    }
}

/// Report callback: Collects and returns reports from context for this handler
fn report_function(_tokens: &[Token]) -> Result<HandlerReport, ConversionError> {
    let context = context!();
    let handler_reports = context.get_reports_by_handler("function_handler");

    // Create a summary report combining all function handler reports
    let total_reports = handler_reports.len();
    let info_count = handler_reports
        .iter()
        .filter(|r| matches!(r.level, Info))
        .count();
    let error_count = handler_reports
        .iter()
        .filter(|r| matches!(r.level, Error))
        .count();
    let warning_count = handler_reports
        .iter()
        .filter(|r| matches!(r.level, ReportLevel::Warning))
        .count();

    let summary_message = format!(
        "Function Handler Summary: {} total reports (Info: {}, Warnings: {}, Errors: {})",
        total_reports, info_count, warning_count, error_count
    );

    let summary_report = HandlerReport {
        report_id: Box::new(Id::get(&Id::gen_name("function_handler_summary"))),
        handler_id: Box::new(Id::get("function_handler")),
        handler_name: "function_handler".to_string(),
        function_name: "report_function".to_string(),
        level: Info,
        phase: Report,
        message: summary_message,
        success: error_count == 0, // Success if no errors
        tokens_processed: handler_reports.iter().map(|r| r.tokens_processed).sum(),
        tokens_consumed: handler_reports.iter().map(|r| r.tokens_consumed).sum(),
        metadata: std::collections::HashMap::new(),
    };

    Ok(summary_report)
}

/// Result callback: Postprocesses generated code, adds documentation, and cleans up the result
fn result_function(
    tokens: &[Token],
    result: HandlerResult,
) -> Result<HandlerResult, ConversionError> {
    report!(
        "function_handler",
        "result_function",
        Info,
        Report,
        "Starting result postprocessing for function",
        true
    );

    match result {
        HandlerResult::Completed(tokens_opt, _, rust_code, id) => {
            // Extract function name for documentation
            let function_name = extract_function_name(tokens);

            // Generate documentation comment based on C function signature
            let doc_comment = generate_function_documentation(tokens, &function_name);

            // Enhance the Rust code with documentation
            let mut enhanced_code = String::new();

            // Add documentation as the first part if substantial
            if !doc_comment.trim().is_empty() {
                enhanced_code.push_str(&doc_comment);
                enhanced_code.push('\n');
            }

            // Add the original converted code
            enhanced_code.push_str(&rust_code);

            // Add metadata comment about the conversion
            let conversion_metadata = format!(
                "\n// Converted from C function: {}",
                tokens
                    .iter()
                    .take(10)
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            );
            enhanced_code.push_str(&conversion_metadata);

            report!(
                "function_handler",
                "result_function",
                Info,
                Report,
                format!(
                    "Enhanced function result with documentation for '{}'",
                    function_name
                ),
                true
            );

            // CRITICAL: Return HandlerResult::Completed to preserve the code!
            Ok(HandlerResult::Completed(
                tokens_opt,
                0..1,
                enhanced_code,
                id,
            ))
        }
        HandlerResult::Converted(element, _, rust_code, id) => {
            // Handle converted elements - enhance the code and preserve the variant
            let function_name = extract_function_name(tokens);
            let doc_comment = generate_function_documentation(tokens, &function_name);

            let mut enhanced_code = String::new();
            if !doc_comment.trim().is_empty() {
                enhanced_code.push_str(&doc_comment);
                enhanced_code.push('\n');
            }
            enhanced_code.push_str(&rust_code);

            let conversion_metadata = format!(
                "\n// Converted from C function: {}",
                tokens
                    .iter()
                    .take(10)
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            );
            enhanced_code.push_str(&conversion_metadata);

            report!(
                "function_handler",
                "result_function",
                Info,
                Report,
                format!("Enhanced converted function result for '{}'", function_name),
                true
            );

            // CRITICAL: Return HandlerResult::Converted to preserve the code!
            Ok(HandlerResult::Converted(element, 0..1, enhanced_code, id))
        }
        HandlerResult::Extracted(element, _, rust_code, id) => {
            // Handle extracted elements - enhance the code and preserve the variant
            let function_name = extract_function_name(tokens);
            let doc_comment = generate_function_documentation(tokens, &function_name);

            let mut enhanced_code = String::new();
            if !doc_comment.trim().is_empty() {
                enhanced_code.push_str(&doc_comment);
                enhanced_code.push('\n');
            }
            enhanced_code.push_str(&rust_code);

            let conversion_metadata = format!(
                "\n// Converted from C function: {}",
                tokens
                    .iter()
                    .take(10)
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            );
            enhanced_code.push_str(&conversion_metadata);

            report!(
                "function_handler",
                "result_function",
                Info,
                Report,
                format!("Enhanced extracted function result for '{}'", function_name),
                true
            );

            // CRITICAL: Return HandlerResult::Extracted to preserve the code!
            Ok(HandlerResult::Extracted(element, 0..1, enhanced_code, id))
        }
        HandlerResult::Handled(Some(processed_tokens), _, id) => {
            // Legacy support for token-based results
            let mut enhanced_result = processed_tokens.clone();

            // Extract function name for documentation
            let function_name = extract_function_name(tokens);

            // Generate documentation comment based on C function signature
            let doc_comment = generate_function_documentation(tokens, &function_name);

            // Add documentation as the first token if it's substantial
            if !doc_comment.trim().is_empty() {
                let doc_token = Token::s(doc_comment);
                enhanced_result.insert(0, doc_token);
            }

            // Postprocess the generated Rust code
            enhanced_result = postprocess_function_code(enhanced_result);

            // Add metadata comment about the conversion
            let conversion_metadata = format!(
                "\n// Converted from C function: {}",
                tokens
                    .iter()
                    .take(10)
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            );
            enhanced_result.push(Token::s(conversion_metadata));

            report!(
                "function_handler",
                "result_function",
                Info,
                Report,
                format!(
                    "Enhanced function result with documentation for '{}'",
                    function_name
                ),
                true
            );

            Ok(HandlerResult::Handled(Some(enhanced_result), 0..1, id))
        }
        HandlerResult::Handled(None, _, ref id) => {
            // Even without processed tokens, we can add some documentation
            let function_name = extract_function_name(tokens);
            let doc_comment = generate_function_documentation(tokens, &function_name);

            if !doc_comment.trim().is_empty() {
                let enhanced_tokens = vec![Token::s(doc_comment)];

                report!(
                    "function_handler",
                    "result_function",
                    Info,
                    Report,
                    format!("Added documentation for function '{}'", function_name),
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

/// Helper function to convert C parameters to Rust parameters with proper type mapping
fn convert_parameters(param_tokens: &[Token]) -> String {
    if param_tokens.is_empty() {
        return String::new();
    }

    // Handle void parameters
    if param_tokens.len() == 1 && param_tokens[0].to_string() == "void" {
        return String::new();
    }

    // Split parameters by commas
    let mut params = Vec::new();
    let mut current_param = Vec::new();

    for token in param_tokens {
        if token.to_string() == "," {
            if !current_param.is_empty() {
                params.push(current_param.clone());
                current_param.clear();
            }
        } else {
            current_param.push(token.clone());
        }
    }

    if !current_param.is_empty() {
        params.push(current_param);
    }

    // Convert each parameter
    let mut rust_params = Vec::new();
    for param_tokens in params {
        if let Some(rust_param) = convert_single_parameter(&param_tokens) {
            rust_params.push(rust_param);
        }
    }

    rust_params.join(", ")
}

/// Helper function to convert a single parameter from C to Rust
fn convert_single_parameter(tokens: &[Token]) -> Option<String> {
    if tokens.is_empty() {
        return None;
    }

    let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

    // Find parameter name - it's the last identifier that's not a type keyword
    let mut param_name = String::new();
    let mut name_idx = tokens.len();

    // Look for the parameter name by finding the last non-type token
    for i in (0..tokens.len()).rev() {
        let token_str = &token_strings[i];

        // Skip array brackets and pointers at the end
        if token_str == "[" || token_str == "]" || token_str == "*" {
            continue;
        }

        // Skip type keywords
        if matches!(
            token_str.as_str(),
            "const"
                | "static"
                | "volatile"
                | "register"
                | "auto"
                | "extern"
                | "unsigned"
                | "signed"
                | "short"
                | "long"
                | "int"
                | "char"
                | "float"
                | "double"
                | "void"
                | "struct"
                | "union"
                | "enum"
        ) {
            continue;
        }

        // This should be the parameter name
        param_name = token_str.clone();
        name_idx = i;
        break;
    }

    if param_name.is_empty() {
        // Fallback: use last token as name
        param_name = format!("param_{}", tokens.len());
        name_idx = tokens.len();
    }

    // Extract type tokens (everything before the name)
    let type_tokens = if name_idx < tokens.len() {
        &tokens[0..name_idx]
    } else {
        &tokens[0..tokens.len().saturating_sub(1)]
    };

    // Convert the type
    let rust_type = convert_c_type_to_rust(type_tokens);

    report!(
        "function_handler",
        "convert_single_parameter",
        Info,
        Convert,
        format!(
            "Converted parameter: {} : {} -> {}: {}",
            tokens
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(" "),
            param_name,
            param_name,
            rust_type
        ),
        true
    );

    Some(format!("{}: {}", param_name, rust_type))
}

/// Helper function to convert C return type to Rust return type
fn convert_return_type(return_type_tokens: &[Token]) -> String {
    if return_type_tokens.is_empty() {
        return "()".to_string();
    }

    // Filter out function modifiers that don't affect return type
    let filtered_tokens: Vec<Token> = return_type_tokens
        .iter()
        .filter(|t| {
            !matches!(
                t.to_string().as_str(),
                "static" | "inline" | "__inline" | "__inline__" | "extern"
            )
        })
        .cloned()
        .collect();

    let rust_type = convert_c_type_to_rust(&filtered_tokens);

    // Handle void special case for return types
    if rust_type == "c_void" {
        return "()".to_string();
    }

    report!(
        "function_handler",
        "convert_return_type",
        Info,
        Convert,
        format!(
            "Return type conversion: '{}' -> '{}'",
            return_type_tokens
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(" "),
            rust_type
        ),
        true
    );

    rust_type
}

/// Helper function to convert C type tokens to Rust type using comprehensive mapping
fn convert_c_type_to_rust(type_tokens: &[Token]) -> String {
    if type_tokens.is_empty() {
        return "()".to_string();
    }

    let mut type_parts: Vec<String> = type_tokens.iter().map(|t| t.to_string()).collect();
    let original_type = type_parts.join(" ");

    // Handle const modifier
    let is_const = type_parts.contains(&"const".to_string());
    type_parts.retain(|part| part != "const");

    // Handle pointer
    let pointer_count = type_parts.iter().filter(|part| *part == "*").count();
    type_parts.retain(|part| part != "*");

    // Handle static/extern/inline (these don't affect parameter types)
    type_parts.retain(|part| {
        !matches!(
            part.as_str(),
            "static" | "extern" | "inline" | "__inline" | "__inline__"
        )
    });

    let base_type_str = type_parts.join(" ");

    // Convert base type using USER's mapping rules
    let binding = {
        // Try convert_type function for other types
        if let Some(converted) = convert_type(&base_type_str) {
            converted.to_string()
        } else {
            // Keep custom types as-is (like Point, MyInt, etc.)
            base_type_str.to_string()
        }
    };
    let rust_base_type = match base_type_str.as_str() {
        "void" => "()",
        "char" => "i8",
        "short" => "i16",
        "int" => "i32",
        "long" => "i64",
        "long long" => "i64",
        "unsigned char" => "u8",
        "unsigned short" => "u16",
        "unsigned int" => "u32",
        "unsigned long" => "u64",
        "unsigned long long" => "u64",
        "float" => "f32",
        "double" => "f64",
        "size_t" => "usize",
        "ssize_t" => "isize",
        "int8_t" => "i8",
        "int16_t" => "i16",
        "int32_t" => "i32",
        "int64_t" => "i64",
        "uint8_t" => "u8",
        "uint16_t" => "u16",
        "uint32_t" => "u32",
        "uint64_t" => "u64",
        _ => binding.as_str(),
    };

    // Apply pointer and const modifiers
    let final_type = match pointer_count {
        0 => rust_base_type.to_string(),
        1 => {
            if is_const {
                format!("*const {}", rust_base_type)
            } else {
                format!("*mut {}", rust_base_type)
            }
        }
        n => {
            let mut result = rust_base_type.to_string();
            for i in 0..n {
                if i == 0 && is_const {
                    result = format!("*const {}", result);
                } else {
                    result = format!("*mut {}", result);
                }
            }
            result
        }
    };

    report!(
        "function_handler",
        "convert_c_type_to_rust",
        Info,
        Convert,
        format!("Type conversion: '{}' -> '{}'", original_type, final_type),
        true
    );

    final_type.to_string()
}

/// Helper function to extract function name from tokens
fn extract_function_name(tokens: &[Token]) -> String {
    for i in 0..tokens.len().saturating_sub(1) {
        if tokens[i + 1].to_string() == "(" {
            return tokens[i].to_string();
        }
    }
    "unknown_function".to_string()
}

/// Helper function to generate documentation for the function
fn generate_function_documentation(tokens: &[Token], function_name: &str) -> String {
    let mut doc = String::new();

    // Generate basic function documentation
    doc.push_str(&format!("/// Converted C function: {}\n", function_name));
    doc.push_str("///\n");

    // Extract and document the original C signature
    let c_signature = tokens
        .iter()
        .take_while(|t| t.to_string() != "{" && t.to_string() != ";")
        .map(|t| t.to_string())
        .collect::<Vec<_>>()
        .join(" ");
    doc.push_str(&format!("/// Original C signature: {}\n", c_signature));

    // Add parameter documentation if we can detect parameters
    if let Some(param_start) = tokens.iter().position(|t| t.to_string() == "(") {
        if let Some(param_end) = tokens[param_start..]
            .iter()
            .position(|t| t.to_string() == ")")
        {
            let param_tokens = &tokens[param_start + 1..param_start + param_end];
            if !param_tokens.is_empty() {
                doc.push_str("/// # Parameters\n");
                let param_str = param_tokens
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(" ");
                doc.push_str(&format!("/// - Parameters: {}\n", param_str));
            }
        }
    }

    // Add safety note for converted functions
    doc.push_str("///\n");
    doc.push_str("/// # Safety\n");
    doc.push_str("/// This function was automatically converted from C.\n");
    doc.push_str("/// Please review for safety and correctness.\n");

    report!(
        "function_handler",
        "generate_function_documentation",
        Info,
        Report,
        format!(
            "Generated {} lines of documentation for function '{}'",
            doc.lines().count(),
            function_name
        ),
        true
    );

    doc
}

/// Helper function to postprocess and clean up generated function code
fn postprocess_function_code(mut tokens: Vec<Token>) -> Vec<Token> {
    // Clean up whitespace and formatting in the generated code
    for token in &mut tokens {
        let token_str = token.to_string();

        // Clean up common formatting issues
        if token_str.contains("  ") {
            // Replace multiple spaces with single spaces
            let cleaned = token_str.replace("  ", " ");
            *token = Token::s(cleaned);
        }

        // Ensure proper Rust formatting around braces
        if token_str.contains(" {") {
            let cleaned = token_str.replace(" {", " {\n");
            *token = Token::s(cleaned);
        }
    }

    report!(
        "function_handler",
        "postprocess_function_code",
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

/// Redirect callback: Handles cases where this handler should pass tokens to a different handler
fn redirect_function(
    tokens: &[Token],
    result: HandlerResult,
) -> Result<HandlerResult, ConversionError> {
    report!(
        "function_handler",
        "redirect_function",
        Info,
        Report,
        "Checking if function tokens should be redirected",
        true
    );

    // Check if this might actually be a function pointer typedef
    if tokens.iter().any(|t| t.to_string() == "typedef") {
        report!(
            "function_handler",
            "redirect_function",
            Info,
            Report,
            "Redirecting to typedef handler",
            true
        );
        return Ok(result);
    }

    // Check if this might be a macro that looks like a function
    if tokens.len() > 0 && tokens[0].to_string() == "#" {
        report!(
            "function_handler",
            "redirect_function",
            Info,
            Report,
            "Redirecting to macro handler",
            true
        );
        return Ok(result);
    }

    // Check if this is actually a struct method definition
    if tokens.iter().any(|t| t.to_string() == "struct") {
        report!(
            "function_handler",
            "redirect_function",
            Info,
            Report,
            "Redirecting to struct handler",
            true
        );
        return Ok(result);
    }

    // Check if this looks more like a variable declaration with function pointer type
    let mut paren_count = 0;
    let mut has_assignment = false;

    for token in tokens {
        match token.to_string().as_str() {
            "(" => paren_count += 1,
            ")" => paren_count -= 1,
            "=" => has_assignment = true,
            _ => {}
        }
    }

    // If we have an assignment and complex parentheses, might be a function pointer variable
    if has_assignment && paren_count == 0 {
        report!(
            "function_handler",
            "redirect_function",
            Info,
            Report,
            "Redirecting to global handler (function pointer variable)",
            true
        );
        return Ok(result);
    }

    // No redirection needed
    Ok(result)
}
