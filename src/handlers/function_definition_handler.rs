//! Function Definition Handler for C to Rust Converter
//! Specialized handler for function definitions (with body in braces)

use crate::document::ElementInfo;
use crate::extract::ExtractedFunction;
use crate::pattern::PatternResult;
use crate::{
    C2RError, ConvertedElement, ExtractedElement, Handler, HandlerPhase, HandlerResult, Id, Kind,
    Reason, ReportLevel, Result,
};
use crate::{context, report};
use std::ops::Range;

/// Create function definition handler with specialized patterns
pub fn create_function_definition_handler() -> Handler {
    let handler_id = Id::get("function_definition_handler");
    let handler_role =
        "Convert C function definitions to Rust function implementations".to_string();
    let priority = 850; // High priority for specific pattern matching

    let mut handler = Handler::new(handler_id, handler_role, priority);
    handler.process = Some(process_function_definition);
    handler.handle = Some(handle_function_definition);
    handler.extract = Some(extract_function_definition);
    handler.convert = Some(convert_function_definition_wrapper);
    handler.document = Some(document_function_definition);
    handler.result = Some(result_function_definition);
    handler
}

/// Process callback: Detects function definitions (with opening brace)
fn process_function_definition(token_range: Range<usize>) -> Result<bool> {
    let mut context = context!();
    context.pull();

    if token_range.end > context.tokens.len() || token_range.is_empty() {
        return Ok(false);
    }

    let tokens = &context.tokens[token_range.clone()];

    report!(
        "function_definition_handler",
        "process_function_definition",
        ReportLevel::Debug,
        HandlerPhase::Process,
        format!(
            "Analyzing {} tokens for function definition pattern",
            tokens.len()
        ),
        true
    );

    // Must have minimum tokens: type identifier ( ) {
    if tokens.len() < 5 {
        return Ok(false);
    }

    // Must contain opening brace for definition
    let has_opening_brace = tokens.iter().any(|t| t.to_string() == "{");
    if !has_opening_brace {
        return Ok(false);
    }

    // Use the existing Patternizer architecture
    let pattern_result = context
        .patternizer
        .match_pattern("function_definition", tokens);

    let result = matches!(
        pattern_result,
        PatternResult::Match { .. } | PatternResult::Sequence { .. } | PatternResult::Fuzzy { .. }
    );

    if result {
        report!(
            "function_definition_handler",
            "process_function_definition",
            ReportLevel::Info,
            HandlerPhase::Process,
            format!(
                "Detected function definition pattern in {} tokens",
                tokens.len()
            ),
            true
        );
    }

    Ok(result)
}

/// Handle callback: Processes function definition tokens
fn handle_function_definition(token_range: Range<usize>) -> Result<HandlerResult> {
    let id = Id::get("function_definition_handler");

    report!(
        "function_definition_handler",
        "handle_function_definition",
        ReportLevel::Info,
        HandlerPhase::Handle,
        format!("Processing function definition tokens: {:?}", token_range),
        true
    );

    // Extract function definition information
    if let Ok(Some(extracted)) = extract_function_definition(token_range.clone()) {
        if let ExtractedElement::Function(function) = extracted {
            report!(
                "function_definition_handler",
                "handle_function_definition",
                ReportLevel::Info,
                HandlerPhase::Handle,
                format!(
                    "Successfully extracted function definition: {}",
                    function.name
                ),
                true
            );

            // Convert ExtractedFunction to FunctionInfo for ElementInfo
            let _function_info = crate::document::FunctionInfo {
                name: function.name.clone(),
                return_type: "int".to_string(), // Simplified for now
                parameters: vec!["void".to_string()], // Simplified for now
                parameter_count: 0,
                is_declaration: false,
                is_definition: true,
                is_static: function.is_static,
                is_extern: function.is_extern,
                is_inline: function.is_inline,
                has_body: true,
                complexity: "simple".to_string(),
                is_unsafe: false,
            };

            if let Ok(Some(converted)) = convert_function_definition_wrapper(token_range.clone()) {
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

/// Extract function definition information from tokens
fn extract_function_definition(token_range: Range<usize>) -> Result<Option<ExtractedElement>> {
    let mut context = context!();
    context.pull();

    let tokens = &context.tokens[token_range.clone()];

    if tokens.len() < 5 {
        return Ok(None);
    }

    let mut return_type = String::new();
    let mut name = String::new();
    let mut parameters = Vec::new();
    let mut body = String::new();
    let mut current_param = String::new();
    let mut in_params = false;
    let mut in_body = false;
    let mut found_identifier = false;
    let mut brace_depth = 0;
    let mut storage_class: Option<String> = None;

    for token in tokens {
        let token_str = token.to_string();

        match token_str.as_str() {
            "static" | "inline" | "extern" => {
                if !in_body && storage_class.is_none() {
                    storage_class = Some(token_str);
                }
            }
            "(" => {
                if !in_body {
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
                        // End of function body
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
                } else if is_type_keyword(&token_str)
                    || (return_type.is_empty() && token_str == "struct")
                {
                    if !return_type.is_empty() {
                        return_type.push(' ');
                    }
                    return_type.push_str(&token_str);
                } else if !return_type.is_empty()
                    && name.is_empty()
                    && is_valid_identifier(&token_str)
                {
                    name = token_str;
                    found_identifier = true;
                }
            }
        }
    }

    if return_type.is_empty() || name.is_empty() || !found_identifier {
        return Ok(None);
    }

    // If no parameters found, it might be void
    if parameters.is_empty() {
        parameters.push("void".to_string());
    }

    // Determine complexity
    let _complexity = if body.len() > 100
        || body.contains("if")
        || body.contains("while")
        || body.contains("for")
    {
        "complex".to_string()
    } else {
        "simple".to_string()
    };

    Ok(Some(ExtractedElement::Function(ExtractedFunction {
        name: name.clone(),
        return_type: Vec::new(), // Convert to tokens if needed
        parameters: Vec::new(),  // Convert to tokens if needed
        body: Vec::new(),        // Convert body to tokens if needed
        is_variadic: false,
        is_static: storage_class.as_ref().map_or(false, |s| s == "static"),
        is_inline: storage_class.as_ref().map_or(false, |s| s == "inline"),
        is_extern: storage_class.as_ref().map_or(false, |s| s == "extern"),
        tokens: context.tokens[token_range.clone()].to_vec(),
        from_recovery: false,
        is_definition: true, // This is a definition
        code: format!(
            "{} {}({}) {{ {} }}",
            return_type,
            name,
            parameters.join(", "),
            body.trim()
        ),
    })))
}

/// Wrapper function for converting function definitions from token range
fn convert_function_definition_wrapper(
    token_range: Range<usize>,
) -> Result<Option<ConvertedElement>> {
    let mut context = context!();
    context.pull();
    let tokens = &context.tokens[token_range.clone()];

    if tokens.is_empty() {
        return Ok(None);
    }

    // Simple conversion logic - create a basic function definition
    let function_name = tokens
        .iter()
        .find(|t| {
            t.to_string() != "int"
                && t.to_string() != "void"
                && t.to_string() != "{"
                && t.to_string() != "}"
                && t.to_string() != "("
                && t.to_string() != ")"
        })
        .map(|t| t.to_string())
        .unwrap_or_else(|| "unknown_function".to_string());

    let rust_code = format!(
        "fn {}() {{ /* TODO: Convert function body */ }}",
        function_name
    );

    Ok(Some(ConvertedElement::Function(
        crate::convert::ConvertedFunction {
            name: function_name.clone(),
            return_type: "()".to_string(),
            parameters: vec![],
            body: "/* TODO: Convert function body */".to_string(),
            code: rust_code,
            is_unsafe: false,
            is_public: true,
            is_definition: true,
        },
    )))
}

/// Internal function for converting function definitions
fn convert_function_definition_internal(element: ElementInfo) -> Result<ConvertedElement> {
    if let crate::document::ElementInfo::Function(function_info) = element {
        let rust_return_type = convert_c_type_to_rust(&function_info.return_type);
        let mut rust_params = Vec::new();

        for param in &function_info.parameters {
            if param.trim() == "void" {
                continue; // Skip void parameters
            }

            let rust_param = convert_c_parameter_to_rust(param);
            rust_params.push(rust_param);
        }

        let params_str = if rust_params.is_empty() {
            "()".to_string()
        } else {
            format!("({})", rust_params.join(", "))
        };

        // Convert function body - use a placeholder since we don't have body in the struct
        let rust_body = "// TODO: Convert function body".to_string();

        let rust_code = if rust_return_type == "()" {
            format!(
                "fn {}{} {{\n    {}\n}}",
                function_info.name, params_str, rust_body
            )
        } else {
            format!(
                "fn {}{} -> {} {{\n    {}\n}}",
                function_info.name, params_str, rust_return_type, rust_body
            )
        };

        Ok(ConvertedElement::Function(
            crate::convert::ConvertedFunction {
                name: function_info.name.clone(),
                return_type: rust_return_type,
                parameters: rust_params,
                body: rust_body,
                code: rust_code,
                is_unsafe: false,
                is_public: false,
                is_definition: true,
            },
        ))
    } else {
        Err(C2RError::new(
            Kind::Other,
            Reason::Expected("function info"),
            None,
        ))
    }
}

/// Document function definition
fn document_function_definition(info: crate::document::ElementInfo) -> Result<Option<String>> {
    if let ElementInfo::Function(function_info) = info {
        let mut doc = format!(
            "/// Converted C function definition: {}\n",
            function_info.name
        );
        doc.push_str(&format!(
            "/// Return type: {} -> {}\n",
            function_info.return_type,
            convert_c_type_to_rust(&function_info.return_type)
        ));

        if !function_info.parameters.is_empty() && function_info.parameters[0] != "void" {
            doc.push_str(&format!(
                "/// Parameters: {}\n",
                function_info.parameters.join(", ")
            ));
        }

        if function_info.is_static {
            doc.push_str("/// Storage class: static\n");
        }
        if function_info.is_extern {
            doc.push_str("/// Storage class: extern\n");
        }
        if function_info.is_inline {
            doc.push_str("/// Storage class: inline\n");
        }

        doc.push_str(&format!("/// Complexity: {}\n", function_info.complexity));

        Ok(Some(doc))
    } else {
        Ok(None)
    }
}

/// Result callback for function definitions
fn result_function_definition(
    _token_range: Range<usize>,
    result: HandlerResult,
) -> Result<HandlerResult> {
    report!(
        "function_definition_handler",
        "result_function_definition",
        ReportLevel::Info,
        HandlerPhase::Report,
        "Postprocessing function definition conversion result",
        true
    );

    Ok(result)
}

/// Convert C function body to Rust (basic conversion)
fn convert_c_body_to_rust(c_body: &str) -> String {
    let mut rust_body = c_body.to_string();

    // Basic substitutions for common C patterns
    rust_body = rust_body.replace("printf(", "println!(");
    rust_body = rust_body.replace("scanf(", "// TODO: Convert scanf to Rust input - ");
    rust_body = rust_body.replace("malloc(", "// TODO: Convert malloc to Box::new or Vec - ");
    rust_body = rust_body.replace(
        "free(",
        "// TODO: Convert free to automatic memory management - ",
    );
    rust_body = rust_body.replace("NULL", "None");

    // Handle return statements
    if !rust_body.trim().ends_with("return") && !rust_body.contains("return") {
        rust_body.push_str("\n    // TODO: Add return statement if needed");
    }

    rust_body.trim().to_string()
}

/// Helper function to check if a string is a valid C identifier
fn is_valid_identifier(s: &str) -> bool {
    if s.is_empty() {
        return false;
    }

    let first_char = s.chars().next().unwrap();
    if !(first_char.is_alphabetic() || first_char == '_') {
        return false;
    }

    s.chars().all(|c| c.is_alphanumeric() || c == '_')
}

/// Helper function to check if a string is a C type keyword
fn is_type_keyword(s: &str) -> bool {
    matches!(
        s,
        "int"
            | "char"
            | "float"
            | "double"
            | "void"
            | "short"
            | "long"
            | "unsigned"
            | "signed"
            | "struct"
            | "enum"
            | "union"
            | "const"
    )
}

/// Convert C type to Rust type
fn convert_c_type_to_rust(c_type: &str) -> String {
    match c_type.trim() {
        "void" => "()".to_string(),
        "int" => "i32".to_string(),
        "char" => "i8".to_string(),
        "unsigned char" => "u8".to_string(),
        "short" => "i16".to_string(),
        "unsigned short" => "u16".to_string(),
        "unsigned int" => "u32".to_string(),
        "long" => "i64".to_string(),
        "unsigned long" => "u64".to_string(),
        "float" => "f32".to_string(),
        "double" => "f64".to_string(),
        _ => {
            // Handle more complex types
            if c_type.starts_with("struct ") {
                c_type.replace("struct ", "")
            } else {
                c_type.to_string()
            }
        }
    }
}

/// Convert C parameter to Rust parameter
fn convert_c_parameter_to_rust(c_param: &str) -> String {
    let parts: Vec<&str> = c_param.trim().split_whitespace().collect();
    if parts.is_empty() {
        return "()".to_string();
    }

    if parts.len() == 1 {
        // Just type, no name
        let rust_type = convert_c_type_to_rust(parts[0]);
        format!("_: {}", rust_type)
    } else {
        // Type and name
        let name = parts.last().unwrap();
        let type_parts = &parts[..parts.len() - 1];
        let c_type = type_parts.join(" ");
        let rust_type = convert_c_type_to_rust(&c_type);
        format!("{}: {}", name, rust_type)
    }
}
