use super::common::{find_matching_token, not_handled, replace_with};
use crate::context::{Context, HandlerReport, ReportLevel::{Info, Error}, HandlerPhase::{Process, Handle, Extract, Convert, Report}};
use crate::{ConvertedElement, ConvertedMacro, report};
use crate::error::ConversionError;
use crate::extract::ExtractedElement;
use crate::extract::ExtractedMacro;
use crate::handler::HandlerResult;
use crate::{get_id, Token};

/// Creates a macro handler that can detect and convert C preprocessor macros
pub fn create_macro_handler() -> crate::handler::Handler {
    let handler_id = get_id("macro_handler");
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
        None,
        None,
        Some(redirect_macro)
    )
}

/// Process callback: Initializes and confirms this handler can handle the tokens
fn process_macro(tokens: &[Token], context: &mut Context) -> Result<bool, ConversionError> {
    // Validate input
    if tokens.is_empty() {
        return Ok(false);
    }
    
    // Check for preprocessor directives starting with #
    if tokens[0].to_string() == "#" && tokens.len() >= 2 {
        let directive = tokens[1].to_string();
        match directive.as_str() {
            "define" => {
                report!(context, "macro_handler", Info, Process, 
                    "Macro definition detected", true);
                return Ok(true);
            },
            "ifdef" | "ifndef" | "if" => {
                report!(context, "macro_handler", Info, Process, 
                    format!("Conditional compilation start detected: {}", directive), true);
                return Ok(true);
            },
            "else" | "elif" | "endif" => {
                report!(context, "macro_handler", Info, Process, 
                    format!("Conditional compilation continuation/end detected: {}", directive), true);
                return Ok(true);
            },
            "undef" => {
                report!(context, "macro_handler", Info, Process, 
                    "Macro undefinition detected", true);
                return Ok(true);
            },
            "pragma" => {
                report!(context, "macro_handler", Info, Process, 
                    "Pragma directive detected", true);
                return Ok(true);
            },
            "error" | "warning" => {
                report!(context, "macro_handler", Info, Process, 
                    format!("Compiler directive detected: {}", directive), true);
                return Ok(true);
            },
            "line" => {
                report!(context, "macro_handler", Info, Process, 
                    "Line directive detected", true);
                return Ok(true);
            },
            _ => {
                report!(context, "macro_handler", Info, Process, 
                    format!("Unknown preprocessor directive: {}", directive), true);
                return Ok(true); // Try to handle unknown directives
            }
        }
    }
    
    Ok(false)
}

/// Processes a preprocessor macro
fn handle_macro(tokens: &[Token], context: &mut Context) -> Result<HandlerResult, ConversionError> {
    report!(context, "macro_handler", Info, Handle, 
        "Macro handler processing tokens", true);
    let id = get_id("handle_macro");
    if tokens.is_empty() || tokens[0].to_string() != "#" || tokens.len() < 2 {
        return not_handled();
    }
    
    let directive = tokens[1].to_string();
    match directive.as_str() {
        "define" => handle_define(tokens, context),
        "ifdef" | "ifndef" => handle_ifdef(tokens, context),
        "if" => handle_if(tokens, context),
        "else" => handle_else(tokens, context),
        "elif" => handle_elif(tokens, context),
        "endif" => handle_endif(tokens, context),
        "undef" => handle_undef(tokens, context),
        "pragma" => handle_pragma(tokens, context),
        "error" | "warning" => handle_message(tokens, context),
        _ => not_handled(),
    }
}

/// Handles #define directive
fn handle_define(tokens: &[Token], context: &mut Context) -> Result<HandlerResult, ConversionError> {
    let id = get_id("handle_define");
    if tokens.len() < 3 {
        return not_handled();
    }
    
    let macro_name = tokens[2].to_string();
    report!(context, "macro_handler", Info, Handle, 
        format!("Found #define: {}", macro_name), true);
    
    // Check if this is a function-like macro
    let is_function_like = tokens.len() > 3 && tokens[3].to_string() == "(";
    
    if is_function_like {
        return handle_function_macro(tokens, context);
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
    let rust_code = convert_object_macro_to_rust(&macro_name, &macro_value, context)?;
    
    replace_with(rust_code,id)
}

/// Handles function-like macros (#define FOO(x, y) ...)
fn handle_function_macro(tokens: &[Token], context: &mut Context) -> Result<HandlerResult, ConversionError> {
    let macro_name = tokens[2].to_string();
    report!(context, "macro_handler", Info, Handle, 
        format!("Found function-like #define: {}", macro_name), true);
    let id = get_id("handle_function_macro");
    // Find the closing parenthesis for parameters
    let open_paren_idx = 3; // Position of the opening parenthesis
    let close_paren_idx = match find_matching_token(&tokens[open_paren_idx..], "(", ")") {
        Some(pos) => open_paren_idx + pos + 1,
        None => {
            report!(context, "macro_handler", Error, Handle, 
                "Could not find closing parenthesis for macro parameters", true);
            return not_handled();
        }
    };
    
    // Extract parameter list
    let param_tokens = &tokens[open_paren_idx + 1..close_paren_idx];
    let mut params = Vec::new();
    let mut param_start = 0;
    
    for i in 0..param_tokens.len() {
        if param_tokens[i].to_string() == "," {
            if param_start < i {
                // Extract parameter name
                let param_name = param_tokens[param_start..i]
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
    
    // Get the last parameter
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
    let rust_code = convert_function_macro_to_rust(
        &macro_name,
        &params,
        &macro_body,
        context,
    )?;
    
    replace_with(rust_code,id)
}

/// Handles #ifdef and #ifndef directives
fn handle_ifdef(tokens: &[Token], context: &mut Context) -> Result<HandlerResult, ConversionError> {
    let id = get_id("handle_ifdef");
    if tokens.len() < 3 {
        return not_handled();
    }
    
    let directive = tokens[1].to_string();
    let is_ifndef = directive == "ifndef";
    let feature_name = tokens[2].to_string();
    
    report!(context, "macro_handler", Info, Handle, 
        format!("Found #{}: {}", directive, feature_name), true);
    
    // Convert to Rust
    let rust_code = if is_ifndef {
        format!("#[cfg(not(feature = \"{}\"))]\n", feature_name)
    } else {
        format!("#[cfg(feature = \"{}\")]\n", feature_name)
    };
    let id = get_id("ifdef");
    replace_with(rust_code, id)
}

/// Handles #if directive
fn handle_if(tokens: &[Token], context: &mut Context) -> Result<HandlerResult, ConversionError> {
    let id = get_id("handle_if");
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
    
    report!(context, "macro_handler", Info, Handle, 
        format!("Found #if: {}", condition), true);
    
    // Try to convert the condition to a Rust cfg expression
    let rust_condition = convert_condition_to_cfg_expr(&condition);
    
    // Convert to Rust
    let rust_code = format!("#[cfg({})]\n", rust_condition);
    let id = get_id("if");
    replace_with(rust_code, id)
}

/// Handles #else directive
fn handle_else(tokens: &[Token], context: &mut Context) -> Result<HandlerResult, ConversionError> {
    report!(context, "macro_handler", Info, Handle, 
        "Found #else", true);
    let id = get_id("handle_else");
    // Convert to Rust
    replace_with("#[cfg(not(any()))]\n".to_string(),id)
}

/// Handles #elif directive
fn handle_elif(tokens: &[Token], context: &mut Context) -> Result<HandlerResult, ConversionError> {
    let id = get_id("handle_elif");
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
    
    report!(context, "macro_handler", Info, Handle, 
        format!("Found #elif: {}", condition), true);
    
    // Try to convert the condition to a Rust cfg expression
    let rust_condition = convert_condition_to_cfg_expr(&condition);
    
    // Convert to Rust
    let rust_code = format!("#[cfg({})]\n", rust_condition);
    let id = get_id("elif");
    replace_with(rust_code, id)
}

/// Handles #endif directive
fn handle_endif(tokens: &[Token], context: &mut Context) -> Result<HandlerResult, ConversionError> {
    let id = get_id("handle_endif");
    report!(context, "macro_handler", Info, Handle, 
        "Found #endif", true);
    // In Rust, conditional compilation is done with attributes, so #endif doesn't need a direct equivalent
    replace_with("".to_string(),id)
}

/// Handles #undef directive
fn handle_undef(tokens: &[Token], context: &mut Context) -> Result<HandlerResult, ConversionError> {
    let id = get_id("handle_ifdef");
    if tokens.len() < 3 {
        return not_handled();
    }
    
    let macro_name = tokens[2].to_string();
    report!(context, "macro_handler", Info, Handle, 
        format!("Found #undef: {}", macro_name), true);
    
    // No direct equivalent in Rust, add a comment
    let rust_code = format!("// #undef {} (no direct Rust equivalent)\n", macro_name);
    
    replace_with(rust_code,id)
}

/// Handles #pragma directive
fn handle_pragma(tokens: &[Token], context: &mut Context) -> Result<HandlerResult, ConversionError> {
    let id = get_id("handle_pragma");
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
    
    report!(context, "macro_handler", Info, Handle, 
        format!("Found #pragma: {}", pragma_text), true);
    
    // Convert to Rust
    let rust_code = match pragma_text.split_whitespace().next() {
        Some("once") => "#[allow(dead_code)]\n".to_string(),
        Some("pack") => "#[repr(packed)]\n".to_string(),
        _ => format!("// #pragma {} (no direct Rust equivalent)\n", pragma_text),
    };
    
    replace_with(rust_code,id)
}

/// Handles #error and #warning directives
fn handle_message(tokens: &[Token], context: &mut Context) -> Result<HandlerResult, ConversionError> {
    let id = get_id("handle_message");
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
    
    report!(context, "macro_handler", Info, Handle, 
        format!("Found #{}: {}", directive, message), true);
    
    // Convert to Rust
    let rust_code = if directive == "error" {
        format!("compile_error!(\"{}\");\n", message.replace("\"", "\\\""))
    } else {
        format!("// #warning {} (converted to comment)\n", message)
    };
    
    replace_with(rust_code,id)
}

/// Extracts a macro as an ExtractedElement
pub fn extract_macro(tokens: &[Token], context: &mut Context) -> Result<Option<ExtractedElement>, ConversionError> {
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

    let mut parameters = Vec::new();
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
        let mut param_start = 0;

        for i in 0..param_tokens.len() {
            if param_tokens[i].to_string() == "," {
                if param_start < i {
                    let param_name = param_tokens[param_start..i]
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<_>>()
                        .join("")
                        .trim()
                        .to_string();

                    if !param_name.is_empty() {
                        parameters.push(param_name);
                    }
                }
                param_start = i + 1;
            }
        }

        // Get the last parameter
        if param_start < param_tokens.len() {
            let param_name = param_tokens[param_start..]
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join("")
                .trim()
                .to_string();

            if !param_name.is_empty() {
                parameters.push(param_name);
            }
        }

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
        params: parameters,
        body: macro_body,
        tokens: tokens.to_vec(),
        is_function_like,
        original_code: tokens.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(" "),
    };

    Ok(Some(ExtractedElement::Macro(extracted_macro)))
}

/// Converts a C object-like macro to Rust
fn convert_object_macro_to_rust(
    name: &str,
    value: &str,
    context: &Context,
) -> Result<String, ConversionError> {
    let mut rust_code = String::new();
    
    // Special cases for common macros
    match name {
        "NULL" => {
            rust_code.push_str("// NULL is not needed in Rust, using None or null pointer\n");
            rust_code.push_str("pub const NULL: *const std::ffi::c_void = std::ptr::null();\n");
        },
        "TRUE" | "true" => {
            rust_code.push_str("// TRUE is built-in as true in Rust\n");
            rust_code.push_str("pub const TRUE: bool = true;\n");
        },
        "FALSE" | "false" => {
            rust_code.push_str("// FALSE is built-in as false in Rust\n");
            rust_code.push_str("pub const FALSE: bool = false;\n");
        },
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
                rust_code.push_str(&format!("// Original C macro: #define {} {}\n", name, value));
                rust_code.push_str(&format!("pub const {}: &str = \"{}\";\n", name, value.replace("\"", "\\\"")));
                rust_code.push_str(&format!("// TODO: Verify the type of {} is correct\n", name));
            }
        }
    }
    
    Ok(rust_code)
}

/// Converts a C function-like macro to Rust
fn convert_function_macro_to_rust(
    name: &str,
    params: &[String],
    body: &str,
    context: &Context,
) -> Result<String, ConversionError> {
    let mut rust_code = String::new();
    
    // Special cases for common function-like macros
    match name {
        "MIN" | "min" => {
            rust_code.push_str("#[inline]\n");
            rust_code.push_str("#[allow(dead_code)]\n");
            rust_code.push_str(&format!("pub fn {}(a: impl PartialOrd, b: impl PartialOrd) -> impl PartialOrd {{\n", name.to_lowercase()));
            rust_code.push_str("    if a < b { a } else { b }\n");
            rust_code.push_str("}\n");
        },
        "MAX" | "max" => {
            rust_code.push_str("#[inline]\n");
            rust_code.push_str("#[allow(dead_code)]\n");
            rust_code.push_str(&format!("pub fn {}(a: impl PartialOrd, b: impl PartialOrd) -> impl PartialOrd {{\n", name.to_lowercase()));
            rust_code.push_str("    if a > b { a } else { b }\n");
            rust_code.push_str("}\n");
        },
        "ABS" | "abs" => {
            rust_code.push_str("#[inline]\n");
            rust_code.push_str("#[allow(dead_code)]\n");
            rust_code.push_str(&format!("pub fn {}(a: impl PartialOrd + std::ops::Neg<Output = impl PartialOrd>) -> impl PartialOrd {{\n", name.to_lowercase()));
            rust_code.push_str("    if a < 0 { -a } else { a }\n");
            rust_code.push_str("}\n");
        },
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
fn convert_macro(tokens: &[Token], context: &mut Context) -> Result<Option<ConvertedElement>, ConversionError> {
    report!(context, "macro_handler", Info, Convert, 
        format!("Converting macro from {} tokens", tokens.len()), true);
    
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
                let close_paren_idx = match find_matching_token(&tokens[open_paren_idx..], "(", ")") {
                    Some(pos) => open_paren_idx + pos + 1,
                    None => return Ok(None),
                };
                
                // Extract parameters
                let param_tokens = &tokens[open_paren_idx + 1..close_paren_idx];
                let mut params = Vec::new();
                let mut param_start = 0;
                
                for i in 0..param_tokens.len() {
                    if param_tokens[i].to_string() == "," {
                        if param_start < i {
                            let param_name = param_tokens[param_start..i]
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
                
                convert_function_macro_to_rust(&macro_name, &params, &macro_body, context)?
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
                convert_object_macro_to_rust(&macro_name, &macro_value, context)?
            }
        },
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
        },
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
        },
        "else" => "#[cfg(not(any()))]\n".to_string(),
        "endif" => "".to_string(),
        "undef" => {
            if tokens.len() < 3 {
                return Ok(None);
            }
            let macro_name = tokens[2].to_string();
            format!("// #undef {} (no direct Rust equivalent)\n", macro_name)
        },
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
        },
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
        },
        _ => {
            format!("// Unknown preprocessor directive: #{}\n", directive)
        }
    };
    
    let id = get_id("convert_macro_callback");
    Ok(Some(ConvertedElement::Macro(ConvertedMacro {
        parameters: Vec::new(), // Default empty parameters
        body: "".to_string(), // Default empty body
        rust_code,
        is_function_like: false,
    })))
}

/// Redirect callback: Handles cases where this handler should pass tokens to a different handler
fn redirect_macro(tokens: &[Token], result:HandlerResult, context: &mut Context) -> Result<HandlerResult, ConversionError> {
    let id = get_id("redirect_macro");
    report!(context, "macro_handler", Info, Report, 
        "Checking if macro tokens should be redirected", true);
    
    // Check if this is actually an include directive
    if tokens.len() >= 2 && tokens[0].to_string() == "#" && tokens[1].to_string() == "include" {
        report!(context, "macro_handler", Info, Report, 
            "Redirecting to include handler", true);
        return Ok(HandlerResult::Redirected(Some(tokens.to_vec()),"macro_handler".to_string(),id,get_id("include_handler")));
    }
    
    // Check if this might be a complex macro that looks like a function definition
    if tokens.len() >= 3 && tokens[0].to_string() == "#" && tokens[1].to_string() == "define" {
        // Look for patterns that suggest this should be handled as a function
        let macro_content = tokens[2..].iter()
            .map(|t| t.to_string())
            .collect::<Vec<_>>()
            .join(" ");
            
        // If the macro body contains complex function-like syntax, might redirect
        if macro_content.contains("return") && macro_content.contains("{") && macro_content.contains("}") {
            report!(context, "macro_handler", Info, Report, 
                "Complex function-like macro detected, considering function handler", true);
            // For now, we'll handle it here, but could redirect to function handler
        }
    }
    
    // Check if this is malformed and might be something else entirely
    if tokens.len() >= 1 && tokens[0].to_string() == "#" {
        if tokens.len() < 2 {
            report!(context, "macro_handler", Info, Report, 
                "Malformed preprocessor directive with just #", true);
            return Ok(result); // Let this handler try to deal with it
        }
        
        // Check for non-standard preprocessor directives that might be extensions
        let directive = tokens[1].to_string();
        match directive.as_str() {
            "define" | "ifdef" | "ifndef" | "if" | "else" | "elif" | "endif" | 
            "undef" | "pragma" | "error" | "warning" | "line" => {
                // Standard directives - no redirection needed
                return Ok(result);
            },
            _ => {
                report!(context, "macro_handler", Info, Report, 
                    format!("Unknown preprocessor directive: {}, handling here", directive), true);
                return Ok(result);
            }
        }
    }
    
    // Check if this doesn't start with # - might not be a preprocessor directive
    if tokens.len() > 0 && tokens[0].to_string() != "#" {
        report!(context, "macro_handler", Info, Report, 
            "Not a preprocessor directive, might need redirection", true);
        
        // Could be a macro invocation or something else
        if tokens.iter().any(|t| t.to_string() == "(") && tokens.iter().any(|t| t.to_string() == ")") {
            report!(context, "macro_handler", Info, Report, 
                "Possible function call, redirecting to function handler", true);
            return Ok(HandlerResult::Redirected(Some(tokens.to_vec()),"macro_handler".to_string(),id,get_id("function_handler")));
        }
        
        // Could be a variable declaration
        if tokens.iter().any(|t| t.to_string() == "=") {
            report!(context, "macro_handler", Info, Report, 
                "Possible variable declaration, redirecting to global handler", true);
            return Ok(HandlerResult::Redirected(Some(tokens.to_vec()),"macro_handler".to_string(),id,get_id("global_handler")));
        }
    }
    
    // No redirection needed
    Ok(result)
}
