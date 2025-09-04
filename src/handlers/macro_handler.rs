use super::common::{find_matching_token, not_handled, replace_with_range};
use crate::config::{
    Context,
    HandlerPhase::{Convert, Handle, Process, Report},
    HandlerReport,
    ReportLevel::{Error, Info, Warning},
};
use crate::error::ConversionError;
use crate::extract::ExtractedElement;
use crate::extract::ExtractedMacro;
use crate::handler::HandlerResult;
use crate::{context, report, ConvertedElement, ConvertedMacro};
use crate::{Id, Token};
use std::collections::HashMap;

/// Creates a macro handler that can detect and convert C preprocessor macros
pub fn create_macro_handler() -> crate::handler::Handler {
    let handler_id = Id::get("macro_handler");
    let handler_role = "macro";
    let priority = 20; // High priority for macros

    super::create_handler(
        handler_id,
        handler_role,
        priority,
        Some(process_macro),
        Some(handle_macro),
        Some(extract_macro),
        Some(convert_macro),
        Some(report_macro),
        Some(result_macro),
        Some(redirect_macro),
    )
}

/// Report callback: Collects and summarizes all macro-related reports from the context
fn report_macro(
    _tokens: &[Token]) -> Result<HandlerReport, ConversionError> {
    let context = context!();
    // Get all reports for this handler
    let reports = context.get_reports_by_handler("macro");

    // Count reports by level
    let mut info_count = 0;
    let mut warning_count = 0;
    let mut error_count = 0;

    for report in &reports {
        match report.level {
            Info => info_count += 1,
            Warning => warning_count += 1,
            Error => error_count += 1,
            _ => info_count += 1, // Handle Debug and other variants as info
        }
    }

    // Create summary report
    Ok(HandlerReport {
        report_id: Box::new(Id::get(&Id::gen_name("macro_handler"))),
        handler_id: Box::new(Id::get("macro_handler")),
        handler_name: "macro".to_string(),
        function_name: "report_macro".to_string(),
        message: format!(
            "Macro handler summary: {} reports ({} info, {} warnings, {} errors)",
            reports.len(),
            info_count,
            warning_count,
            error_count
        ),
        level: if error_count > 0 {
            Error
        } else if warning_count > 0 {
            Warning
        } else {
            Info
        },
        tokens_processed: reports.len(),
        tokens_consumed: 0,
        phase: Report,
        success: error_count == 0,
        metadata: HashMap::new(),
    })
}

/// Process callback: Initializes and confirms this handler can handle the tokens
pub(crate) fn process_macro(
    tokens: &[Token]) -> Result<bool, ConversionError> {
    // Validate input
    if tokens.is_empty() {
        return Ok(false);
    }

    // Check for preprocessor directives starting with #
    if tokens[0].to_string() == "#" && tokens.len() >= 2 {
        let directive = tokens[1].to_string();
        return match directive.as_str() {
            "define" => {
                report!(
                    "macro_handler",
                    "process_macro",
                    Info,
                    Process,
                    "Macro definition detected",
                    true
                );
                Ok(true)
            }
            "ifdef" | "ifndef" | "if" => {
                report!(
                    "macro_handler",
                    "process_macro",
                    Info,
                    Process,
                    format!("Conditional compilation start detected: {}", directive),
                    true
                );
                Ok(true)
            }
            "else" | "elif" | "endif" => {
                report!(
                    "macro_handler",
                    "process_macro",
                    Info,
                    Process,
                    format!(
                        "Conditional compilation continuation/end detected: {}",
                        directive
                    ),
                    true
                );
                Ok(true)
            }
            "undef" => {
                report!(
                    "macro_handler",
                    "process_macro",
                    Info,
                    Process,
                    "Macro undefinition detected",
                    true
                );
                Ok(true)
            }
            "pragma" => {
                report!(
                    "macro_handler",
                    "process_macro",
                    Info,
                    Process,
                    "Pragma directive detected",
                    true
                );
                Ok(true)
            }
            "error" | "warning" => {
                report!(
                    "macro_handler",
                    "process_macro",
                    Info,
                    Process,
                    format!("Compiler directive detected: {}", directive),
                    true
                );
                Ok(true)
            }
            "line" => {
                report!(
                    "macro_handler",
                    "process_macro",
                    Info,
                    Process,
                    "Line directive detected",
                    true
                );
                Ok(true)
            }
            _ => {
                report!(
                    "macro_handler",
                    "process_macro",
                    Info,
                    Process,
                    format!("Unknown preprocessor directive: {}", directive),
                    true
                );
                Ok(true) // Try to handle unknown directives
            }
        };
    }

    Ok(false)
}

/// Processes a preprocessor macro
pub(crate) fn handle_macro(
    tokens: &[Token]) -> Result<HandlerResult, ConversionError> {
    report!(
        "macro_handler",
        "handle_macro",
        Info,
        Handle,
        "Macro handler processing tokens",
        true
    );
    let id = Id::get("handle_macro");
    if tokens.is_empty() || tokens[0].to_string() != "#" || tokens.len() < 2 {
        return not_handled();
    }

    let directive = tokens[1].to_string();
    match directive.as_str() {
        "define" => handle_define(tokens),
        "ifdef" | "ifndef" => handle_ifdef(tokens),
        "if" => handle_if(tokens),
        "else" => handle_else(tokens),
        "elif" => handle_elif(tokens),
        "endif" => handle_endif(tokens),
        "undef" => handle_undef(tokens),
        "pragma" => handle_pragma(tokens),
        "error" | "warning" => handle_message(tokens),
        _ => not_handled(),
    }
}

/// Handles #define directive
pub(crate) fn handle_define(
    tokens: &[Token]) -> Result<HandlerResult, ConversionError> {
    let id = Id::get("handle_define");
    if tokens.len() < 3 {
        return not_handled();
    }

    let macro_name = tokens[2].to_string();
    report!(
        "macro_handler",
        "handle_macro",
        Info,
        Handle,
        format!("Found #define: {}", macro_name),
        true
    );

    // Check if this is a function-like macro
    let is_function_like = tokens.len() > 3 && tokens[3].to_string() == "(";

    if is_function_like {
        return handle_function_macro(tokens);
    }

    // Object-like macro
    let mut macro_value = String::new();

    // Extract macro value (everything after the name)
    if tokens.len() > 3 {
        for i in 3..tokens.len() {
            macro_value.push_str(&tokens[i].to_string());
            if i < tokens.len() - 1 {
                macro_value.push(' ');
            }
        }
    }

    // Convert to Rust
    let rust_code = convert_object_macro_to_rust(&macro_name, &macro_value)?;

    // Calculate token consumption range for object-like macro
    // Consume entire macro definition (from #define to end of line)
    let token_range = 0..tokens.len();
    replace_with_range(rust_code, token_range, id)
}

/// Handles function-like macros (#define FOO(x, y) ...)
fn handle_function_macro(
    tokens: &[Token]) -> Result<HandlerResult, ConversionError> {
    let macro_name = tokens[2].to_string();
    report!(
        "macro_handler",
        "handle_function_macro",
        Info,
        Handle,
        format!("Found function-like #define: {}", macro_name),
        true
    );
    let id = Id::get("handle_function_macro");
    // Find the closing parenthesis for parameters
    let open_paren_idx = 3; // Position of the opening parenthesis
    let close_paren_idx = match find_matching_token(&tokens[open_paren_idx..], "(", ")") {
        Some(pos) => open_paren_idx + pos + 1,
        None => {
            report!(
                "macro_handler",
                "handle_function_macro",
                Error,
                Handle,
                "Could not find closing parenthesis for macro parameters",
                true
            );
            return not_handled();
        }
    };

    // Extract parameter list
    let param_tokens = &tokens[open_paren_idx + 1..close_paren_idx];
    let params = extract_macro_params(param_tokens, 0);

    // Extract macro body (everything after the closing parenthesis)
    let mut macro_body = String::new();
    if close_paren_idx + 1 < tokens.len() {
        for i in close_paren_idx + 1..tokens.len() {
            macro_body.push_str(&tokens[i].to_string());
            if i < tokens.len() - 1 {
                macro_body.push(' ');
            }
        }
    }

    // Convert to Rust
    let rust_code = convert_function_macro_to_rust(&macro_name, &params, &macro_body)?;

    // Calculate token consumption range for function-like macro
    // Consume entire macro definition (from #define to end of line)
    let token_range = 0..tokens.len();
    replace_with_range(rust_code, token_range, id)
}

/// Handles #ifdef and #ifndef directives
pub(crate) fn handle_ifdef(
    tokens: &[Token]) -> Result<HandlerResult, ConversionError> {
    let id = Id::get("handle_ifdef");
    if tokens.len() < 3 {
        return not_handled();
    }

    let directive = tokens[1].to_string();
    let is_ifndef = directive == "ifndef";
    let feature_name = tokens[2].to_string();

    report!(
        "macro_handler",
        "handle_ifdef",
        Info,
        Handle,
        format!("Found #{}: {}", directive, feature_name),
        true
    );

    // Convert to Rust
    let rust_code = if is_ifndef {
        format!("#[cfg(not(feature = \"{}\"))]\n", feature_name)
    } else {
        format!("#[cfg(feature = \"{}\")]\n", feature_name)
    };
    let id = Id::get("ifdef");
    // Calculate token consumption range for #ifdef/#ifndef
    // Consume from # to feature name
    let token_range = 0..tokens.len();
    replace_with_range(rust_code, token_range, id)
}

/// Handles #if directive
fn handle_if(tokens: &[Token]) -> Result<HandlerResult, ConversionError> {
    let id = Id::get("handle_if");
    if tokens.len() < 3 {
        return not_handled();
    }

    // Extract the condition
    let mut condition = String::new();
    for i in 2..tokens.len() {
        condition.push_str(&tokens[i].to_string());
        if i < tokens.len() - 1 {
            condition.push(' ');
        }
    }

    report!(
        "macro_handler",
        "handle_if",
        Info,
        Handle,
        format!("Found #if: {}", condition),
        true
    );

    // Try to convert the condition to a Rust cfg expression
    let rust_condition = convert_condition_to_cfg_expr(&condition);

    // Convert to Rust
    let rust_code = format!("#[cfg({})]\n", rust_condition);
    let id = Id::get("if");
    // Calculate token consumption range for #if
    // Consume entire #if directive
    let token_range = 0..tokens.len();
    replace_with_range(rust_code, token_range, id)
}

/// Handles #else directive
fn handle_else(tokens: &[Token]) -> Result<HandlerResult, ConversionError> {
    report!(
        "macro_handler",
        "handle_else",
        Info,
        Handle,
        "Found #else",
        true
    );
    let id = Id::get("handle_else");
    // Convert to Rust
    // Calculate token consumption range for #else
    // Consume entire #else directive
    let token_range = 0..tokens.len();
    replace_with_range("#[cfg(not(any()))]\n".to_string(), token_range, id)
}

/// Handles #elif directive
fn handle_elif(tokens: &[Token]) -> Result<HandlerResult, ConversionError> {
    let id = Id::get("handle_elif");
    if tokens.len() < 3 {
        return not_handled();
    }

    // Extract the condition
    let mut condition = String::new();
    for i in 2..tokens.len() {
        condition.push_str(&tokens[i].to_string());
        if i < tokens.len() - 1 {
            condition.push(' ');
        }
    }

    report!(
        "macro_handler",
        "handle_elif",
        Info,
        Handle,
        format!("Found #elif: {}", condition),
        true
    );

    // Try to convert the condition to a Rust cfg expression
    let rust_condition = convert_condition_to_cfg_expr(&condition);

    // Convert to Rust
    let rust_code = format!("#[cfg({})]\n", rust_condition);
    let id = Id::get("elif");
    // Calculate token consumption range for #elif
    // Consume entire #elif directive
    let token_range = 0..tokens.len();
    replace_with_range(rust_code, token_range, id)
}

/// Handles #endif directive
fn handle_endif(tokens: &[Token]) -> Result<HandlerResult, ConversionError> {
    let id = Id::get("handle_endif");
    report!(
        "macro_handler",
        "handle_endif",
        Info,
        Handle,
        "Found #endif",
        true
    );
    // In Rust, conditional compilation is done with attributes, so #endif doesn't need a direct equivalent
    // Calculate token consumption range for #endif
    // Consume entire #endif directive
    let token_range = 0..tokens.len();
    replace_with_range("".to_string(), token_range, id)
}

/// Handles #undef directive
pub(crate) fn handle_undef(
    tokens: &[Token]) -> Result<HandlerResult, ConversionError> {
    let id = Id::get("handle_ifdef");
    if tokens.len() < 3 {
        return not_handled();
    }

    let macro_name = tokens[2].to_string();
    report!(
        "macro_handler",
        "handle_undef",
        Info,
        Handle,
        format!("Found #undef: {}", macro_name),
        true
    );

    // No direct equivalent in Rust, add a comment
    let rust_code = format!("// #undef {} (no direct Rust equivalent)\n", macro_name);

    // Calculate token consumption range for #undef
    // Consume entire #undef directive
    let token_range = 0..tokens.len();
    replace_with_range(rust_code, token_range, id)
}

/// Handles #pragma directive
fn handle_pragma(
    tokens: &[Token]) -> Result<HandlerResult, ConversionError> {
    let id = Id::get("handle_pragma");
    if tokens.len() < 3 {
        return not_handled();
    }

    // Extract the pragma text
    let mut pragma_text = String::new();
    for i in 2..tokens.len() {
        pragma_text.push_str(&tokens[i].to_string());
        if i < tokens.len() - 1 {
            pragma_text.push(' ');
        }
    }

    report!(
        "macro_handler",
        "handle_pragma",
        Info,
        Handle,
        format!("Found #pragma: {}", pragma_text),
        true
    );

    // Convert to Rust
    let rust_code = match pragma_text.split_whitespace().next() {
        Some("once") => "#[allow(dead_code)]\n".to_string(),
        Some("pack") => "#[repr(packed)]\n".to_string(),
        _ => format!("// #pragma {} (no direct Rust equivalent)\n", pragma_text),
    };

    // Calculate token consumption range for #pragma
    // Consume entire #pragma directive
    let token_range = 0..tokens.len();
    replace_with_range(rust_code, token_range, id)
}

/// Handles #error and #warning directives
fn handle_message(
    tokens: &[Token]) -> Result<HandlerResult, ConversionError> {
    let id = Id::get("handle_message");
    if tokens.len() < 3 {
        return not_handled();
    }

    let directive = tokens[1].to_string();

    // Extract the message text
    let mut message = String::new();
    for i in 2..tokens.len() {
        message.push_str(&tokens[i].to_string());
        if i < tokens.len() - 1 {
            message.push(' ');
        }
    }

    report!(
        "macro_handler",
        "handle_error_warning",
        Info,
        Handle,
        format!("Found #{}: {}", directive, message),
        true
    );

    // Convert to Rust
    let rust_code = if directive == "error" {
        format!("compile_error!(\"{}\");\n", message.replace("\"", "\\\""))
    } else {
        format!("// #warning {} (converted to comment)\n", message)
    };

    // Calculate token consumption range for #error/#warning
    // Consume entire directive
    let token_range = 0..tokens.len();
    replace_with_range(rust_code, token_range, id)
}

/// Extracts a macro as an ExtractedElement
pub fn extract_macro(
    tokens: &[Token]) -> Result<Option<ExtractedElement>, ConversionError> {
    if tokens.is_empty() || tokens[0].to_string() != "#" || tokens.len() < 2 {
        return Ok(None);
    }

    let directive = tokens[1].to_string();
    if directive != "define" {
        // Only extract #define macros for now
        return Ok(None);
    }

    if tokens.len() < 3 {
        return Ok(None);
    }

    let macro_name = tokens[2].to_string();

    // Check if this is a function-like macro
    let is_function_like = tokens.len() > 3 && tokens[3].to_string() == "(";

    let mut params = Vec::new();
    let mut macro_body = Vec::<Token>::new();

    if is_function_like {
        // Find the closing parenthesis for parameters
        let open_paren_idx = 3; // Position of the opening parenthesis
        let close_paren_idx = match find_matching_token(&tokens[open_paren_idx..], "(", ")") {
            Some(pos) => open_paren_idx + pos + 1,
            None => return Ok(None),
        };

        // Extract parameter list
        let param_tokens = &tokens[open_paren_idx + 1..close_paren_idx];
        params = extract_macro_params(param_tokens, 0);

        // Extract macro body
        if close_paren_idx + 1 < tokens.len() {
            for i in close_paren_idx + 1..tokens.len() {
                macro_body.push(tokens[i].clone());
                if i < tokens.len() - 1 {
                    macro_body.push(Token::l(" "));
                }
            }
        }
    } else {
        // Object-like macro
        // Extract macro value
        if tokens.len() > 3 {
            for i in 3..tokens.len() {
                macro_body.push(tokens[i].clone());
                if i < tokens.len() - 1 {
                    macro_body.push(Token::l(" "));
                }
            }
        }
    }

    let extracted_macro = ExtractedMacro {
        name: macro_name,
        params,
        body: macro_body,
        tokens: tokens.to_vec(),
        is_function_like,
        original_code: tokens
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<_>>()
            .join(" "),
    };

    Ok(Some(ExtractedElement::Macro(extracted_macro)))
}

/// Converts a C object-like macro to Rust
fn convert_object_macro_to_rust(
    name: &str,
    value: &str) -> Result<String, ConversionError> {
    let mut rust_code = String::new();

    // Special cases for common macros
    match name {
        "NULL" => {
            rust_code.push_str("// NULL is not needed in Rust, using None or null pointer\n");
            rust_code.push_str("pub const NULL: *const std::ffi::c_void = std::ptr::null();\n");
        }
        "TRUE" | "true" => {
            rust_code.push_str("// TRUE is built-in as true in Rust\n");
            rust_code.push_str("pub const TRUE: bool = true;\n");
        }
        "FALSE" | "false" => {
            rust_code.push_str("// FALSE is built-in as false in Rust\n");
            rust_code.push_str("pub const FALSE: bool = false;\n");
        }
        _ => {
            // Try to determine the type of the value
            if value.is_empty() {
                // Empty macro
                rust_code.push_str(&format!("// Empty macro {}\n", name));
                rust_code.push_str(&format!("pub const {}: () = ();\n", name));
            } else if value.starts_with("\"") && value.ends_with("\"") {
                // String literal
                rust_code.push_str(&format!("pub const {}: &str = {};\n", name, value));
            } else if value.parse::<i64>().is_ok() {
                // Integer literal
                rust_code.push_str(&format!("pub const {}: i32 = {};\n", name, value));
            } else if value.parse::<f64>().is_ok() {
                // Float literal
                rust_code.push_str(&format!("pub const {}: f64 = {};\n", name, value));
            } else {
                // Other/unknown type
                rust_code.push_str(&format!(
                    "// Original C macro: #define {} {}\n",
                    name, value
                ));
                rust_code.push_str(&format!(
                    "pub const {}: &str = \"{}\";\n",
                    name,
                    value.replace("\"", "\\\"")
                ));
                rust_code.push_str(&format!(
                    "// TODO: Verify the type of {} is correct\n",
                    name
                ));
            }
        }
    }

    Ok(rust_code)
}

/// Converts a C function-like macro to Rust
fn convert_function_macro_to_rust(
    name: &str,
    params: &[String],
    body: &str
) -> Result<String, ConversionError> {
    let mut rust_code = String::new();

    // Special cases for common function-like macros
    match name {
        "MIN" | "min" => {
            rust_code.push_str("#[inline]\n");
            rust_code.push_str("#[allow(dead_code)]\n");
            rust_code.push_str(&format!(
                "pub fn {}(a: impl PartialOrd, b: impl PartialOrd) -> impl PartialOrd {{\n",
                name.to_lowercase()
            ));
            rust_code.push_str("    if a < b { a } else { b }\n");
            rust_code.push_str("}\n");
        }
        "MAX" | "max" => {
            rust_code.push_str("#[inline]\n");
            rust_code.push_str("#[allow(dead_code)]\n");
            rust_code.push_str(&format!(
                "pub fn {}(a: impl PartialOrd, b: impl PartialOrd) -> impl PartialOrd {{\n",
                name.to_lowercase()
            ));
            rust_code.push_str("    if a > b { a } else { b }\n");
            rust_code.push_str("}\n");
        }
        "ABS" | "abs" => {
            rust_code.push_str("#[inline]\n");
            rust_code.push_str("#[allow(dead_code)]\n");
            rust_code.push_str(&format!("pub fn {}(a: impl PartialOrd + std::ops::Neg<Output = impl PartialOrd>) -> impl PartialOrd {{\n", name.to_lowercase()));
            rust_code.push_str("    if a < 0 { -a } else { a }\n");
            rust_code.push_str("}\n");
        }
        _ => {
            // Generic function-like macro conversion as a Rust macro
            rust_code.push_str("// Original C macro: #define ");
            rust_code.push_str(name);
            rust_code.push('(');
            rust_code.push_str(&params.join(", "));
            rust_code.push_str(") ");
            rust_code.push_str(body);
            rust_code.push_str("\n");

            rust_code.push_str("#[macro_export]\n");
            rust_code.push_str("#[allow(unused_macros)]\n");
            rust_code.push_str(&format!("macro_rules! {} {{\n", name.to_lowercase()));
            rust_code.push_str("    (");

            // Generate pattern for each parameter
            for (i, param) in params.iter().enumerate() {
                if i > 0 {
                    rust_code.push_str(", ");
                }
                rust_code.push_str(&format!("${}: expr", param));
            }

            rust_code.push_str(") => {{\n");

            // Generate the body with parameter replacements
            let mut macro_body = body.to_string();
            for param in params {
                let pattern = param.to_string();
                let replacement = format!("${}", param);
                macro_body = macro_body.replace(&pattern, &replacement);
            }

            rust_code.push_str(&format!("        {}\n", macro_body));
            rust_code.push_str("    };\n");
            rust_code.push_str("}\n");

            // Also provide a function version if possible
            rust_code.push_str("\n// Function version of the macro\n");
            rust_code.push_str("#[inline]\n");
            rust_code.push_str("#[allow(dead_code)]\n");
            rust_code.push_str(&format!("pub fn {}(", name.to_lowercase()));

            // Function parameters
            for (i, param) in params.iter().enumerate() {
                if i > 0 {
                    rust_code.push_str(", ");
                }
                rust_code.push_str(&format!("{}: impl std::fmt::Debug", param.to_lowercase()));
            }

            rust_code.push_str(") -> impl std::fmt::Debug {\n");
            rust_code.push_str("    // TODO: Implement this function based on the macro logic\n");
            rust_code.push_str("    todo!(\"Implement function version of macro\")\n");
            rust_code.push_str("}\n");
        }
    }

    Ok(rust_code)
}

/// Converts a C preprocessor condition to a Rust cfg expression
fn convert_condition_to_cfg_expr(condition: &str) -> String {
    // This is a simplified converter - a real implementation would need to handle complex expressions

    // Common patterns
    match condition.trim() {
        "defined(DEBUG)" => return "debug_assertions".to_string(),
        "defined(NDEBUG)" => return "not(debug_assertions)".to_string(),
        "DEBUG" => return "debug_assertions".to_string(),
        _ => {}
    }

    // Fallback - use the condition as a feature flag
    format!("feature = \"{}\"", condition.trim())
}

/// Convert callback: Does the actual conversion of C to Rust code
fn convert_macro(
    tokens: &[Token]) -> Result<Option<ConvertedElement>, ConversionError> {
    report!(
        "macro_handler",
        "convert_macro",
        Info,
        Convert,
        format!("Converting macro from {} tokens", tokens.len()),
        true
    );

    if tokens.is_empty() || tokens[0].to_string() != "#" || tokens.len() < 2 {
        return Ok(None);
    }

    let directive = tokens[1].to_string();
    let rust_code = match directive.as_str() {
        "define" => {
            if tokens.len() < 3 {
                return Ok(None);
            }

            let macro_name = tokens[2].to_string();
            let is_function_like = tokens.len() > 3 && tokens[3].to_string() == "(";

            if is_function_like {
                // Extract parameters and body for function-like macro
                let open_paren_idx = 3;
                let close_paren_idx = match find_matching_token(&tokens[open_paren_idx..], "(", ")")
                {
                    Some(pos) => open_paren_idx + pos + 1,
                    None => return Ok(None),
                };

                // Extract parameters
                let param_tokens = &tokens[open_paren_idx + 1..close_paren_idx];

                let params = extract_macro_params(param_tokens, 0);

                // Extract body
                let mut macro_body = String::new();
                if close_paren_idx + 1 < tokens.len() {
                    for i in close_paren_idx + 1..tokens.len() {
                        macro_body.push_str(&tokens[i].to_string());
                        if i < tokens.len() - 1 {
                            macro_body.push(' ');
                        }
                    }
                }

                convert_function_macro_to_rust(&macro_name, &params, &macro_body)?
            } else {
                // Object-like macro
                let mut macro_value = String::new();
                if tokens.len() > 3 {
                    for i in 3..tokens.len() {
                        macro_value.push_str(&tokens[i].to_string());
                        if i < tokens.len() - 1 {
                            macro_value.push(' ');
                        }
                    }
                }
                convert_object_macro_to_rust(&macro_name, &macro_value)?
            }
        }
        "ifdef" | "ifndef" => {
            if tokens.len() < 3 {
                return Ok(None);
            }
            let is_ifndef = directive == "ifndef";
            let feature_name = tokens[2].to_string();

            if is_ifndef {
                format!("#[cfg(not(feature = \"{}\"))]\n", feature_name)
            } else {
                format!("#[cfg(feature = \"{}\")]\n", feature_name)
            }
        }
        "if" | "elif" => {
            if tokens.len() < 3 {
                return Ok(None);
            }

            let mut condition = String::new();
            for i in 2..tokens.len() {
                condition.push_str(&tokens[i].to_string());
                if i < tokens.len() - 1 {
                    condition.push(' ');
                }
            }

            let rust_condition = convert_condition_to_cfg_expr(&condition);
            format!("#[cfg({})]\n", rust_condition)
        }
        "else" => "#[cfg(not(any()))]\n".to_string(),
        "endif" => "".to_string(),
        "undef" => {
            if tokens.len() < 3 {
                return Ok(None);
            }
            let macro_name = tokens[2].to_string();
            format!("// #undef {} (no direct Rust equivalent)\n", macro_name)
        }
        "pragma" => {
            if tokens.len() < 3 {
                return Ok(None);
            }

            let mut pragma_text = String::new();
            for i in 2..tokens.len() {
                pragma_text.push_str(&tokens[i].to_string());
                if i < tokens.len() - 1 {
                    pragma_text.push(' ');
                }
            }

            match pragma_text.split_whitespace().next() {
                Some("once") => "#[allow(dead_code)]\n".to_string(),
                Some("pack") => "#[repr(packed)]\n".to_string(),
                _ => format!("// #pragma {} (no direct Rust equivalent)\n", pragma_text),
            }
        }
        "error" | "warning" => {
            if tokens.len() < 3 {
                return Ok(None);
            }

            let mut message = String::new();
            for i in 2..tokens.len() {
                message.push_str(&tokens[i].to_string());
                if i < tokens.len() - 1 {
                    message.push(' ');
                }
            }

            if directive == "error" {
                format!("compile_error!(\"{}\");\n", message.replace("\"", "\\\""))
            } else {
                format!("// #warning {} (converted to comment)\n", message)
            }
        }
        _ => {
            format!("// Unknown preprocessor directive: #{}\n", directive)
        }
    };

    let id = Id::get("convert_macro_callback");
    Ok(Some(ConvertedElement::Macro(ConvertedMacro {
        parameters: Vec::new(), // Default empty parameters
        body: "".to_string(),   // Default empty body
        rust_code,
        is_function_like: false,
    })))
}

/// Result callback: Postprocesses generated macro code, adds documentation, and enhances formatting
fn result_macro(
    tokens: &[Token],
    result: HandlerResult) -> Result<HandlerResult, ConversionError> {
    let _id = Id::get("result_macro");

    report!(
        "macro_handler",
        "result_macro",
        Info,
        Report,
        "Postprocessing macro conversion result",
        true
    );

    match result {
        HandlerResult::Completed(tokens_opt, _, rust_code, id) => {
            // Extract macro information for documentation
            let macro_info = extract_macro_info_from_tokens(tokens);

            // Generate documentation about the macro conversion
            let doc_comment = generate_macro_documentation(tokens, &macro_info);

            // Enhance the Rust code with documentation and metadata
            let mut enhanced_code = String::new();

            // Add metadata comment for traceability
            let metadata_comment = format!(
                "// [C2R] Macro converted from C to Rust - {}: {}\n",
                macro_info.name, macro_info.kind_description
            );
            enhanced_code.push_str(&metadata_comment);

            // Add the documentation comment if substantial
            if !doc_comment.trim().is_empty() {
                enhanced_code.push_str(&doc_comment);
                enhanced_code.push('\n');
            }

            // Add the original converted code
            enhanced_code.push_str(&rust_code);

            report!(
                "macro_handler",
                "result_macro",
                Info,
                Report,
                &format!(
                    "Enhanced macro conversion: {} ({})",
                    macro_info.name, macro_info.kind_description
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
            let macro_info = extract_macro_info_from_tokens(tokens);
            let doc_comment = generate_macro_documentation(tokens, &macro_info);

            let mut enhanced_code = String::new();
            let metadata_comment = format!(
                "// [C2R] Macro converted from C to Rust - {}: {}\n",
                macro_info.name, macro_info.kind_description
            );
            enhanced_code.push_str(&metadata_comment);

            if !doc_comment.trim().is_empty() {
                enhanced_code.push_str(&doc_comment);
                enhanced_code.push('\n');
            }
            enhanced_code.push_str(&rust_code);

            report!(
                "macro_handler",
                "result_macro",
                Info,
                Report,
                &format!(
                    "Enhanced converted macro: {} ({})",
                    macro_info.name, macro_info.kind_description
                ),
                true
            );

            // CRITICAL: Return HandlerResult::Converted to preserve the code!
            Ok(HandlerResult::Converted(element, 0..1, enhanced_code, id))
        }
        HandlerResult::Extracted(element, _, rust_code, id) => {
            // Handle extracted elements - enhance the code and preserve the variant
            let macro_info = extract_macro_info_from_tokens(tokens);
            let doc_comment = generate_macro_documentation(tokens, &macro_info);

            let mut enhanced_code = String::new();
            let metadata_comment = format!(
                "// [C2R] Macro converted from C to Rust - {}: {}\n",
                macro_info.name, macro_info.kind_description
            );
            enhanced_code.push_str(&metadata_comment);

            if !doc_comment.trim().is_empty() {
                enhanced_code.push_str(&doc_comment);
                enhanced_code.push('\n');
            }
            enhanced_code.push_str(&rust_code);

            report!(
                "macro_handler",
                "result_macro",
                Info,
                Report,
                &format!(
                    "Enhanced extracted macro: {} ({})",
                    macro_info.name, macro_info.kind_description
                ),
                true
            );

            // CRITICAL: Return HandlerResult::Extracted to preserve the code!
            Ok(HandlerResult::Extracted(element, 0..1, enhanced_code, id))
        }
        HandlerResult::Handled(Some(converted_tokens), _, handler_id) => {
            // Legacy support for token-based results
            // Extract macro information for documentation
            let macro_info = extract_macro_info_from_tokens(tokens);

            // Generate documentation about the macro conversion
            let doc_comment = generate_macro_documentation(tokens, &macro_info);

            // Postprocess the converted Rust code for better formatting
            let mut enhanced_result = postprocess_macro_code(converted_tokens);

            // Add the documentation comment before the converted macro
            if !doc_comment.is_empty() {
                enhanced_result.insert(0, Token::s(doc_comment));
            }

            // Add metadata comment for traceability
            let metadata_comment = format!(
                "// [C2R] Macro converted from C to Rust - {}: {}",
                macro_info.name, macro_info.kind_description
            );
            enhanced_result.insert(0, Token::s(metadata_comment));

            report!(
                "macro_handler",
                "result_macro",
                Info,
                Report,
                &format!(
                    "Enhanced macro conversion: {} ({})",
                    macro_info.name, macro_info.kind_description
                ),
                true
            );

            Ok(HandlerResult::Handled(
                Some(enhanced_result),
                0..1,
                handler_id,
            ))
        }
        HandlerResult::Handled(None, _, handler_id) => {
            report!(
                "macro_handler",
                "result_macro",
                Warning,
                Report,
                "Macro handler returned empty result",
                true
            );
            Ok(HandlerResult::Handled(None, 0..0, handler_id))
        }
        other_result => {
            // Pass through non-handled results unchanged
            Ok(other_result)
        }
    }
}

/// Macro information extracted for documentation purposes
#[derive(Debug, Clone)]
struct MacroInfo {
    name: String,
    kind: MacroKind,
    kind_description: String,
    directive: String,
    params: Vec<String>,
    is_function_like: bool,
    is_conditional: bool,
    complexity: String,
}

/// Enum representing different kinds of macros for specialized handling
#[derive(Debug, Clone, PartialEq)]
pub enum MacroKind {
    ObjectLike,
    FunctionLike,
    Conditional,
    Pragma,
    Message,
    Other,
}

/// Extracts macro information from the original tokens for documentation purposes
fn extract_macro_info_from_tokens(tokens: &[Token]) -> MacroInfo {
    if tokens.is_empty() || tokens[0].to_string() != "#" {
        return MacroInfo {
            name: "unknown".to_string(),
            kind: MacroKind::Other,
            kind_description: "unknown preprocessor directive".to_string(),
            directive: "unknown".to_string(),
            params: Vec::new(),
            is_function_like: false,
            is_conditional: false,
            complexity: "unknown".to_string(),
        };
    }

    let directive = if tokens.len() > 1 {
        tokens[1].to_string()
    } else {
        "unknown".to_string()
    };

    let (kind, kind_description, is_conditional) = match directive.as_str() {
        "define" => {
            let is_function_like = tokens.len() > 3 && tokens[3].to_string() == "(";
            if is_function_like {
                (
                    MacroKind::FunctionLike,
                    "function-like macro".to_string(),
                    false,
                )
            } else {
                (
                    MacroKind::ObjectLike,
                    "object-like macro".to_string(),
                    false,
                )
            }
        }
        "ifdef" | "ifndef" | "if" | "else" | "elif" | "endif" => (
            MacroKind::Conditional,
            "conditional compilation directive".to_string(),
            true,
        ),
        "pragma" => (MacroKind::Pragma, "pragma directive".to_string(), false),
        "error" | "warning" => (
            MacroKind::Message,
            "compiler message directive".to_string(),
            false,
        ),
        _ => (
            MacroKind::Other,
            "other preprocessor directive".to_string(),
            false,
        ),
    };

    // Extract macro name
    let macro_name = if directive == "define" && tokens.len() > 2 {
        tokens[2].to_string()
    } else if (directive == "ifdef" || directive == "ifndef" || directive == "undef")
        && tokens.len() > 2
    {
        tokens[2].to_string()
    } else {
        directive.clone()
    };

    // Extract parameters for function-like macros
    let mut params = Vec::new();
    let is_function_like = matches!(kind, MacroKind::FunctionLike);

    if is_function_like && tokens.len() > 3 && tokens[3].to_string() == "(" {
        // Find closing parenthesis and extract parameters
        let open_paren_idx = 3;
        if let Some(close_paren_offset) = find_matching_token(&tokens[open_paren_idx..], "(", ")") {
            let close_paren_idx = open_paren_idx + close_paren_offset + 1;
            let param_tokens = &tokens[open_paren_idx + 1..close_paren_idx];
            params = extract_macro_params(param_tokens, 0);
        }
    }

    // Determine complexity
    let complexity = match kind {
        MacroKind::ObjectLike => "simple".to_string(),
        MacroKind::FunctionLike => {
            if params.len() <= 2 {
                "simple".to_string()
            } else {
                format!("moderate ({} parameters)", params.len())
            }
        }
        MacroKind::Conditional => "simple".to_string(),
        MacroKind::Pragma => "simple".to_string(),
        MacroKind::Message => "simple".to_string(),
        MacroKind::Other => "unknown".to_string(),
    };

    MacroInfo {
        name: macro_name,
        kind,
        kind_description,
        directive,
        params,
        is_function_like,
        is_conditional,
        complexity,
    }
}

/// Generates documentation comments for the macro conversion
fn generate_macro_documentation(
    tokens: &[Token],
    macro_info: &MacroInfo) -> String {
    let mut doc_lines = Vec::new();

    // Add main documentation header
    doc_lines.push("/**".to_string());
    doc_lines.push(" * Macro Conversion Documentation".to_string());
    doc_lines.push(" *".to_string());

    // Add macro information
    doc_lines.push(format!(" * Name: {}", macro_info.name));
    doc_lines.push(format!(" * Directive: #{}", macro_info.directive));
    doc_lines.push(format!(" * Kind: {}", macro_info.kind_description));
    doc_lines.push(format!(" * Complexity: {}", macro_info.complexity));
    doc_lines.push(format!(" * Original tokens: {}", tokens.len()));

    // Add parameters for function-like macros
    if macro_info.is_function_like && !macro_info.params.is_empty() {
        doc_lines.push(format!(" * Parameters: {}", macro_info.params.join(", ")));
    }

    // Add features information
    let mut features = Vec::new();
    if macro_info.is_function_like {
        features.push("function-like");
    }
    if macro_info.is_conditional {
        features.push("conditional compilation");
    }

    if !features.is_empty() {
        doc_lines.push(format!(" * Features: {}", features.join(", ")));
    }

    doc_lines.push(" *".to_string());

    // Add conversion notes based on macro kind
    match macro_info.kind {
        MacroKind::ObjectLike => {
            doc_lines
                .push(" * Conversion: C object-like macro -> Rust const declaration".to_string());
            doc_lines.push(" * Features:".to_string());
            doc_lines.push(" *   - Compile-time constant evaluation".to_string());
            doc_lines.push(" *   - Type safety with explicit type annotations".to_string());
            doc_lines.push(" *   - No preprocessing, direct value substitution".to_string());
            doc_lines.push(" *   - Memory-safe string and numeric constants".to_string());
            doc_lines.push(" *   - Special handling for NULL, TRUE, FALSE constants".to_string());
        }
        MacroKind::FunctionLike => {
            doc_lines.push(
                " * Conversion: C function-like macro -> Rust macro_rules! + inline function"
                    .to_string(),
            );
            doc_lines.push(" * Features:".to_string());
            doc_lines.push(" *   - Hygienic macro system prevents identifier capture".to_string());
            doc_lines.push(" *   - Type-safe parameter handling with expr patterns".to_string());
            doc_lines
                .push(" *   - Inline function alternative for better type checking".to_string());
            doc_lines.push(" *   - Compile-time expansion with zero runtime overhead".to_string());
            doc_lines
                .push(" *   - Special optimized versions for MIN, MAX, ABS functions".to_string());
        }
        MacroKind::Conditional => {
            doc_lines.push(
                " * Conversion: C conditional compilation -> Rust cfg attributes".to_string(),
            );
            doc_lines.push(" * Features:".to_string());
            doc_lines.push(" *   - Feature flag system for modular compilation".to_string());
            doc_lines.push(" *   - Debug assertions mapping for DEBUG/NDEBUG".to_string());
            doc_lines.push(" *   - Compile-time code inclusion/exclusion".to_string());
            doc_lines.push(" *   - Platform-specific and configuration-aware building".to_string());
            doc_lines.push(" *   - Zero runtime cost conditional compilation".to_string());
        }
        MacroKind::Pragma => {
            doc_lines.push(" * Conversion: C pragma directive -> Rust attributes".to_string());
            doc_lines.push(" * Features:".to_string());
            doc_lines.push(" *   - Compiler hint translation (repr, allow, etc.)".to_string());
            doc_lines.push(" *   - Memory layout control with repr attributes".to_string());
            doc_lines.push(" *   - Warning suppression with allow attributes".to_string());
            doc_lines.push(" *   - Rust-idiomatic compiler configuration".to_string());
            doc_lines.push(" *   - Special handling for pragma once, pack directives".to_string());
        }
        MacroKind::Message => {
            doc_lines.push(
                " * Conversion: C compiler message -> Rust compile_error!/comment".to_string(),
            );
            doc_lines.push(" * Features:".to_string());
            doc_lines.push(" *   - Compile-time error generation with compile_error!".to_string());
            doc_lines
                .push(" *   - Warning messages converted to documentation comments".to_string());
            doc_lines.push(" *   - Build-time failure for critical error conditions".to_string());
            doc_lines.push(" *   - Clear error messages for development debugging".to_string());
            doc_lines.push(" *   - Proper string escaping for message content".to_string());
        }
        MacroKind::Other => {
            doc_lines
                .push(" * Conversion: Unknown preprocessor directive -> Rust comment".to_string());
            doc_lines.push(" * Features:".to_string());
            doc_lines.push(" *   - Preserves original directive as documentation".to_string());
            doc_lines.push(" *   - Safe fallback for unsupported directives".to_string());
            doc_lines.push(" *   - Maintains conversion traceability".to_string());
            doc_lines.push(" *   - Manual review flagged for unknown constructs".to_string());
        }
    }

    doc_lines.push(" *".to_string());
    doc_lines.push(" * Rust macro benefits:".to_string());
    doc_lines.push(" *   - Hygienic macro system prevents naming conflicts".to_string());
    doc_lines.push(" *   - Compile-time code generation with full type checking".to_string());
    doc_lines.push(" *   - Pattern matching for flexible macro definitions".to_string());
    doc_lines.push(" *   - Zero-cost abstractions with optimized compilation".to_string());
    doc_lines.push(" *   - Better error messages and debugging support".to_string());
    doc_lines.push(" */".to_string());

    report!(
        "macro_handler",
        "generate_macro_documentation",
        Info,
        Report,
        &format!(
            "Generated documentation for {} macro: {}",
            macro_info.kind_description, macro_info.name
        ),
        true
    );

    doc_lines.join("\n")
}

/// Postprocesses the converted macro code for better formatting
fn postprocess_macro_code(mut tokens: Vec<Token>) -> Vec<Token> {
    let original_count = tokens.len();

    // Clean up and format the converted macro tokens
    for token in tokens.iter_mut() {
        let mut content = token.to_string();

        // Clean up extra whitespace
        content = content.trim().to_string();

        // Format const declarations
        if content.starts_with("pub const") {
            // Ensure proper spacing in const declarations
            content = content.replace("pub const", "pub const ");
            content = content.replace(" : ", ": ");
            content = content.replace(" = ", " = ");
            content = content.replace("  ", " ");
        }

        // Format macro_rules! declarations
        if content.contains("macro_rules!") {
            // Clean up macro rule formatting
            content = content.replace("macro_rules! ", "macro_rules! ");
            content = content.replace("macro_rules!  ", "macro_rules! ");
            content = content.replace(" {", " {");
            content = content.replace("{ ", "{");
            content = content.replace(" }", " }");
            content = content.replace("}  ", "} ");
        }

        // Format cfg attributes
        if content.starts_with("#[cfg(") {
            // Clean up cfg attribute formatting
            content = content.replace("#[cfg( ", "#[cfg(");
            content = content.replace(" )]", ")]");
            content = content.replace("( ", "(");
            content = content.replace(" )", ")");
            content = content.replace(" =", "=");
            content = content.replace("= ", "= ");
            content = content.replace("  ", " ");
        }

        // Format inline function declarations
        if content.contains("#[inline]") || content.contains("pub fn") {
            // Clean up function formatting
            content = content.replace("pub fn ", "pub fn ");
            content = content.replace("pub fn  ", "pub fn ");
            content = content.replace(" (", "(");
            content = content.replace("( ", "(");
            content = content.replace(" )", ")");
            content = content.replace(") ", ") ");
            content = content.replace(" -> ", " -> ");
            content = content.replace("->  ", "-> ");
        }

        // Format compile_error! and other macros
        if content.contains("compile_error!") || content.contains("todo!") {
            // Clean up macro call formatting
            content = content.replace("! (", "!(");
            content = content.replace("!( ", "!(");
            content = content.replace(" )", ")");
            content = content.replace("( ", "(");
        }

        // Format comments
        if content.starts_with("//") {
            // Ensure proper spacing after comment markers
            content = content.replace("//  ", "// ");
            content = content.replace("// ", "// ");
        }

        // Ensure proper semicolon formatting
        if content.ends_with(';') {
            content = content.trim_end_matches(' ').to_string();
            if !content.ends_with(';') {
                content.push(';');
            }
        }

        // Clean up multiple spaces
        while content.contains("  ") {
            content = content.replace("  ", " ");
        }

        *token = Token::s(content);
    }

    // Remove any empty tokens
    tokens.retain(|token| !token.to_string().trim().is_empty());

    report!(
        "macro_handler",
        "postprocess_macro_code",
        Info,
        Report,
        &format!(
            "Postprocessed {} tokens -> {} tokens",
            original_count,
            tokens.len()
        ),
        true
    );

    tokens
}

/// Redirect callback: Handles cases where this handler should pass tokens to a different handler
fn redirect_macro(
    tokens: &[Token],
    result: HandlerResult) -> Result<HandlerResult, ConversionError> {
    let id = Id::get("redirect_macro");
    report!(
        "macro_handler",
        "redirect_macro",
        Info,
        Report,
        "Checking if macro tokens should be redirected",
        true
    );

    // Check if this is actually an include directive
    if tokens.len() >= 2 && tokens[0].to_string() == "#" && tokens[1].to_string() == "include" {
        report!(
            "macro_handler",
            "redirect_macro",
            Info,
            Report,
            "Redirecting to include handler",
            true
        );
        return Ok(HandlerResult::Redirected(
            Some(tokens.to_vec()),
            0..1,
            "macro_handler".to_string(),
            id,
            Id::get("include_handler"),
        ));
    }

    // Check if this might be a complex macro that looks like a function definition
    if tokens.len() >= 3 && tokens[0].to_string() == "#" && tokens[1].to_string() == "define" {
        // Look for patterns that suggest this should be handled as a function
        let macro_content = tokens[2..]
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<_>>()
            .join(" ");

        // If the macro body contains complex function-like syntax, might redirect
        if macro_content.contains("return")
            && macro_content.contains("{")
            && macro_content.contains("}")
        {
            report!(
                "macro_handler",
                "redirect_macro",
                Info,
                Report,
                "Complex function-like macro detected, considering function handler",
                true
            );
            // For now, we'll handle it here, but could redirect to function handler
        }
    }

    // Check if this is malformed and might be something else entirely
    if tokens.len() >= 1 && tokens[0].to_string() == "#" {
        if tokens.len() < 2 {
            report!(
                "macro_handler",
                "redirect_macro",
                Info,
                Report,
                "Malformed preprocessor directive with just #",
                true
            );
            return Ok(result); // Let this handler try to deal with it
        }

        // Check for non-standard preprocessor directives that might be extensions
        let directive = tokens[1].to_string();
        return match directive.as_str() {
            "define" | "ifdef" | "ifndef" | "if" | "else" | "elif" | "endif" | "undef"
            | "pragma" | "error" | "warning" | "line" => {
                // Standard directives - no redirection needed
                Ok(result)
            }
            _ => {
                report!(
                    "macro_handler",
                    "redirect_macro",
                    Info,
                    Report,
                    format!(
                        "Unknown preprocessor directive: {}, handling here",
                        directive
                    ),
                    true
                );
                Ok(result)
            }
        };
    }

    // Check if this doesn't start with # - might not be a preprocessor directive
    if tokens.len() > 0 && tokens[0].to_string() != "#" {
        report!(
            "macro_handler",
            "redirect_macro",
            Info,
            Report,
            "Not a preprocessor directive, might need redirection",
            true
        );

        // Could be a macro invocation or something else
        if tokens.iter().any(|t| t.to_string() == "(")
            && tokens.iter().any(|t| t.to_string() == ")")
        {
            report!(
                "macro_handler",
                "redirect_macro",
                Info,
                Report,
                "Possible function call, redirecting to function handler",
                true
            );
            return Ok(HandlerResult::Redirected(
                Some(tokens.to_vec()),
                0..1,
                "macro_handler".to_string(),
                id,
                Id::get("function_handler"),
            ));
        }

        // Could be a variable declaration
        if tokens.iter().any(|t| t.to_string() == "=") {
            report!(
                "macro_handler",
                "redirect_macro",
                Info,
                Report,
                "Possible variable declaration, redirecting to global handler",
                true
            );
            return Ok(HandlerResult::Redirected(
                Some(tokens.to_vec()),
                0..1,
                "macro_handler".to_string(),
                id,
                Id::get("global_handler"),
            ));
        }
    }

    // No redirection needed
    Ok(result)
}
fn extract_macro_params(tokens: &[Token], start: usize) -> Vec<String> {
    let mut param_start = start.clone();
    let param_tokens = tokens;
    let mut params = Vec::<String>::new();
    for i in 0..param_tokens.len() {
        if param_tokens[i].to_string() == "," {
            if start < i {
                let param_name = param_tokens[start..i]
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join("")
                    .trim()
                    .to_string();
                if !param_name.is_empty() {
                    params.push(param_name);
                }
            }
            param_start = i + 1;
        }
    }
    if param_start < param_tokens.len() {
        let param_name = param_tokens[param_start..]
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<_>>()
            .join("")
            .trim()
            .to_string();
        if !param_name.is_empty() {
            params.push(param_name);
        }
    }
    params
}
