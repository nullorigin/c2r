//! Function Declaration Handler for C to Rust Converter
//! Specialized handler for function prototypes/declarations (ending with semicolon)

use crate::convert::ConvertedFunction;
use crate::document::ElementInfo;
use crate::extract::ExtractedFunction;
use crate::pattern::PatternResult;
use crate::{
    C2RError, ConvertedElement, ExtractedElement, Handler, HandlerPhase, HandlerResult, Id, Kind,
    Reason, ReportLevel, Result,
};
use crate::{context, report};
use std::ops::Range;

/// Create function declaration handler with specialized patterns
pub fn create_function_declaration_handler() -> Handler {
    let handler_id = Id::get("function_declaration_handler");
    let handler_role = "Convert C function declarations to Rust function prototypes".to_string();
    let priority = 900; // High priority for specific pattern matching

    let mut handler = Handler::new(handler_id, handler_role, priority);
    handler.process = Some(process_function_declaration);
    handler.handle = Some(handle_function_declaration);
    handler.extract = Some(extract_function_declaration);
    handler.convert = Some(convert_function_declaration_wrapper);
    handler.document = Some(document_function_declaration);
    handler.result = Some(result_function_declaration);
    handler
}

/// Process callback: Detects function declarations (prototypes ending with semicolon)
fn process_function_declaration(token_range: Range<usize>) -> Result<bool> {
    let mut context = context!();
    context.pull();

    if token_range.end > context.tokens.len() || token_range.is_empty() {
        return Ok(false);
    }

    let tokens = &context.tokens[token_range.clone()];

    report!(
        "function_declaration_handler",
        "process_function_declaration",
        ReportLevel::Debug,
        HandlerPhase::Process,
        format!(
            "Analyzing {} tokens for function declaration pattern",
            tokens.len()
        ),
        true
    );

    // Must have minimum tokens: type identifier ( ) ;
    if tokens.len() < 5 {
        return Ok(false);
    }

    // Use the existing Patternizer architecture
    let pattern_result = context
        .patternizer
        .match_pattern("function_declaration", tokens);

    let result = matches!(
        pattern_result,
        PatternResult::Match { .. } | PatternResult::Sequence { .. } | PatternResult::Fuzzy { .. }
    );

    if result {
        report!(
            "function_declaration_handler",
            "process_function_declaration",
            ReportLevel::Info,
            HandlerPhase::Process,
            format!(
                "Detected function declaration pattern in {} tokens",
                tokens.len()
            ),
            true
        );
    }

    Ok(result)
}

/// Handle callback: Processes function declaration tokens
fn handle_function_declaration(token_range: Range<usize>) -> Result<HandlerResult> {
    let id = Id::get("function_declaration_handler");

    report!(
        "function_declaration_handler",
        "handle_function_declaration",
        ReportLevel::Info,
        HandlerPhase::Handle,
        format!("Processing function declaration tokens: {:?}", token_range),
        true
    );

    // Extract function declaration information
    if let Ok(Some(extracted)) = extract_function_declaration(token_range.clone()) {
        if let ExtractedElement::Function(function) = extracted {
            report!(
                "function_declaration_handler",
                "handle_function_declaration",
                ReportLevel::Info,
                HandlerPhase::Handle,
                format!(
                    "Successfully extracted function declaration: {}",
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
                is_declaration: true,
                is_definition: false,
                is_static: function.is_static,
                is_extern: function.is_extern,
                is_inline: function.is_inline,
                has_body: false,
                complexity: "simple".to_string(),
                is_unsafe: false,
            };

            if let Ok(Some(converted)) = convert_function_declaration_wrapper(token_range.clone()) {
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

/// Extract function declaration information from tokens
fn extract_function_declaration(token_range: Range<usize>) -> Result<Option<ExtractedElement>> {
    let mut context = context!();
    context.pull();

    let tokens = &context.tokens[token_range.clone()];

    if tokens.len() < 5 {
        return Ok(None);
    }

    let mut return_type = String::new();
    let mut name = String::new();
    let mut parameters = Vec::new();
    let mut current_param = String::new();
    let mut in_params = false;
    let mut found_identifier = false;

    for token in tokens {
        let token_str = token.to_string();

        match token_str.as_str() {
            "(" => {
                in_params = true;
            }
            ")" => {
                if !current_param.trim().is_empty() {
                    parameters.push(current_param.trim().to_string());
                    current_param.clear();
                }
                in_params = false;
            }
            "," => {
                if in_params && !current_param.trim().is_empty() {
                    parameters.push(current_param.trim().to_string());
                    current_param.clear();
                }
            }
            ";" => {
                // End of declaration
                break;
            }
            _ => {
                if in_params {
                    if !current_param.is_empty() {
                        current_param.push(' ');
                    }
                    current_param.push_str(&token_str);
                } else if matches!(
                    token_str.as_str(),
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
                ) {
                    if !return_type.is_empty() {
                        return_type.push(' ');
                    }
                    return_type.push_str(&token_str);
                } else if return_type.is_empty() && token_str == "static" {
                    // Skip storage class specifiers for now
                    continue;
                } else if !return_type.is_empty()
                    && name.is_empty()
                    && token_str
                        .chars()
                        .next()
                        .map(|c| c.is_alphabetic())
                        .unwrap_or(false)
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

    Ok(Some(ExtractedElement::Function(ExtractedFunction {
        name: name.clone(),
        return_type: vec![], // Empty for now
        parameters: vec![],
        body: vec![],
        is_variadic: false,
        is_static: false,
        is_inline: false,
        is_extern: false,
        tokens: tokens.to_vec(),
        from_recovery: false,
        is_definition: false,
        code: "/* function declaration */".to_string(),
    })))
}

/// Convert function declarations from token range
fn convert_function_declaration_wrapper(
    token_range: Range<usize>,
) -> Result<Option<ConvertedElement>> {
    let mut context = context!();
    context.pull();
    let tokens = &context.tokens[token_range.clone()];

    if tokens.is_empty() {
        return Ok(None);
    }

    // Extract function name from tokens
    let function_name = tokens
        .iter()
        .map(|t| t.to_string())
        .find(|s| {
            !matches!(
                s.as_str(),
                "int" | "void" | ";" | "(" | ")" | "static" | "extern" | "inline"
            )
        })
        .unwrap_or_else(|| "unknown_function".to_string());

    let rust_code = format!("fn {}();", function_name);

    Ok(Some(ConvertedElement::Function(
        crate::convert::ConvertedFunction {
            name: function_name.clone(),
            return_type: "()".to_string(),
            parameters: vec![],
            body: "".to_string(),
            code: rust_code,
            is_unsafe: false,
            is_public: true,
            is_definition: false,
        },
    )))
}

/// Internal function for converting function declarations
fn convert_function_declaration_internal(element: ElementInfo) -> Result<ConvertedElement> {
    if let ElementInfo::Function(function_info) = element {
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

        let rust_code = if rust_return_type == "()" {
            format!("fn {}{};", function_info.name, params_str)
        } else {
            format!(
                "fn {}{} -> {};",
                function_info.name, params_str, rust_return_type
            )
        };

        Ok(ConvertedElement::Function(ConvertedFunction {
            name: function_info.name.clone(),
            return_type: rust_return_type,
            parameters: rust_params,
            body: String::new(),
            code: rust_code,
            is_unsafe: false,
            is_public: false,
            is_definition: false,
        }))
    } else {
        Err(C2RError::new(
            Kind::Other,
            Reason::Expected("function info"),
            None,
        ))
    }
}

/// Document function declaration
fn document_function_declaration(info: ElementInfo) -> Result<Option<String>> {
    if let ElementInfo::Function(function_info) = info {
        let mut doc = format!(
            "/// Converted C function declaration: {}\n",
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

        Ok(Some(doc))
    } else {
        Ok(None)
    }
}

/// Result callback for function declarations
fn result_function_declaration(
    _token_range: Range<usize>,
    result: HandlerResult,
) -> Result<HandlerResult> {
    report!(
        "function_declaration_handler",
        "result_function_declaration",
        ReportLevel::Info,
        HandlerPhase::Report,
        "Postprocessing function declaration conversion result",
        true
    );

    Ok(result)
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

/// Helper functions
fn is_type_keyword(token: &str) -> bool {
    matches!(
        token,
        "int" | "char" | "short" | "long" | "float" | "double" | "void" | "signed" | "unsigned"
    )
}

fn is_valid_identifier(token: &str) -> bool {
    !token.is_empty() && token.chars().next().unwrap().is_alphabetic()
}
