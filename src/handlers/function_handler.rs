use crate::context::{Context, HandlerReport, ReportLevel, HandlerPhase};
use crate::context::ReportLevel::{Info, Error, Warning, Debug};
use crate::context::HandlerPhase::{Process, Handle, Extract, Convert, Report};
use crate::report;
use crate::error::ConversionError;
use crate::extract::ExtractedElement;
use crate::extract::ExtractedFunction;
use crate::handler::HandlerResult;
use crate::{convert_type, get_id, ConvertedElement, ConvertedFunction, Token};
use crate::common::parse_name;
use crate::debug;
use super::common::{self, find_matching_token, not_handled, replace_with};

/// Creates a function handler that can detect and convert C function declarations and definitions
pub fn create_function_handler() -> crate::handler::Handler {
    let handler_id = get_id("function_handler");
    let handler_role = "function";
    let priority = 100; // Standard priority
    
    super::create_handler(
        handler_id,
        handler_role,
        priority,
        Some(process_function),
        Some(handle_function),
        Some(extract_function),
        Some(convert_function),
        None,
        None,
        Some(redirect_function)
    )
}

/// Process callback: Initializes and confirms this handler can handle the tokens
fn process_function(tokens: &[Token], context: &mut Context) -> Result<bool, ConversionError> {
    // Validate input
    if tokens.len() < 4 {
        return Ok(false);
    }
    
    // Look for function patterns: return_type function_name(parameters)
    for i in 1..tokens.len().saturating_sub(1) {
        if tokens[i + 1].to_string() == "(" {
            // Check if tokens before this point could form a valid type
            if common::is_type(&tokens[0..i], context) {
                // Additional validation: ensure this looks like a function
                if let Some(closing_paren) = find_matching_token(&tokens[i + 1..], "(", ")") {
                    let paren_end = i + 1 + closing_paren + 1;
                    
                    // Check for function body or semicolon
                    if paren_end < tokens.len() {
                        let next_token = &tokens[paren_end].to_string();
                        if next_token == "{" || next_token == ";" {
                            report!(context, "function_handler", Info, Handle, 
                                format!("Function pattern detected: {}", tokens[i].to_string()), true);
                            return Ok(true);
                        }
                    }
                }
            }
        }
    }
    
    Ok(false)
}

/// Legacy function for backwards compatibility
fn can_handle_function(tokens: &[Token], context: &mut Context) -> bool {
    if tokens.len() < 4 {
        return false;
    }
    
    // Look for common function patterns
    // Check for return type + name + open parenthesis
    
    // First, find potential function name
    // We're looking for a pattern where token is followed by an opening parenthesis
    for i in 1..tokens.len() - 1 {
        if matches!(tokens[i+1].to_string().as_str(), "(") {
            let _potential_name = &tokens[i];
            
            // Check if tokens before this point could form a type
            // This is a simplification - real implementation would do more thorough type checking
            if common::is_type(&tokens[0..i], context) {
                return true;
            }
        }
    }
    
    false
}

/// Processes a function declaration or definition
fn handle_function(tokens: &[Token], context: &mut Context) -> Result<HandlerResult, ConversionError> {
    let id = get_id("handle_function");
    report!(context, "function_handler", Info, Process, 
        format!("Processing {} tokens for function handling", tokens.len()), true);
    
    // Find function name and opening parenthesis
    let mut name_pos = 0;
    for i in 0..tokens.len() - 1 {
        if matches!(tokens[i+1].to_string().as_str(), "(") {
            name_pos = i;
            break;
        }
    }
    
    if name_pos == 0 {
        return not_handled();
    }
    
    let func_name = tokens[name_pos].to_string();
    report!(context, "function_handler", Info, Process, 
        format!("Found function: {}", func_name), true);
    
    // Return type is everything before the name
    let return_type_tokens = &tokens[0..name_pos];
    
    // Find the parameter list between parentheses
    let params_start = name_pos + 1; // Opening parenthesis
    let params_end = match find_matching_token(&tokens[params_start..], "(", ")") {
        Some(pos) => params_start + pos + 1, // +1 for the closing parenthesis
        None => {
            report!(context, "function_handler", Error, Process, 
                format!("Could not find closing parenthesis for function {}", func_name), false);
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
                report!(context, "function_handler", Error, Process, 
                    format!("Could not find closing brace for function {}", func_name), false);
                return not_handled();
            }
        };

        // Extract body tokens
        let body_tokens = &tokens[body_start + 1..body_end - 1];

        // Convert to Rust
        if let Some(ConvertedElement::Function(converted_function)) = convert_function(
            tokens,
            context,
        )? {
            return replace_with(converted_function.rust_code.clone(), get_id("handle_function"));
        } else {
            Err(ConversionError::new("Failed to convert function"))
        }
    } else {
// Function declaration only (no body)
        if let Some(ConvertedElement::Function(converted_function)) = convert_function(
            tokens,
            context,
        )? {
            return replace_with(converted_function.rust_code.clone(), get_id("handle_function"));
        } else {
            Err(ConversionError::new("Failed to convert function"))
        }
    }
}

/// Extract callback: Finds and returns the locations this handler should handle
pub fn extract_function(tokens: &[Token], context: &mut Context) -> Result<Option<ExtractedElement>, ConversionError> {
    report!(context, "function_handler", Info, Extract, 
        format!("Extracting function from {} tokens", tokens.len()), true);
    // Find function name and opening parenthesis
    let mut name_pos = 0;
    for i in 0..tokens.len().saturating_sub(1) {
        if matches!(tokens[i+1].to_string().as_str(), "(") {
            name_pos = i;
            break;
        }
    }

    if name_pos == 0 {
        return Ok(None);
    }

    let func_name = tokens[name_pos].to_string();
    report!(context, "function_handler", Info, Extract, 
        format!("Extracting function: {}", func_name), true);
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
    let is_inline = return_type_tokens.iter().any(|t|
        matches!(t.to_string().as_str(), "inline" | "__inline" | "__inline__")
    );

    // Create ExtractedFunction
    let extracted_function = ExtractedFunction {
        name: func_name,
        return_type: return_type_tokens,
        parameters: param_tokens,
        body: body_tokens,
        is_definition,
        is_static,
        is_inline,
        original_code: tokens.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(" "),
        tokens: tokens.to_vec(),
        is_variadic: false,
        from_recovery: false,
    };

    Ok(Some(ExtractedElement::Function(extracted_function)))
}

/// Convert callback: Does the actual conversion of C to Rust code
fn convert_function(
    tokens: &[Token],
    context: &mut Context,
) -> Result<Option<ConvertedElement>, ConversionError> {
    report!(context, "function_handler", Info, Convert, 
        format!("Converting function from {} tokens", tokens.len()), true);
    let id = get_id("convert_function");
    if let ExtractedElement::Function(element) = extract_function(tokens, context)?.unwrap() {
        let mut rust_code = String::new();
        // Add function visibility
        rust_code.push_str("pub ");

        // Check for special cases like 'extern "C"'
        if element.return_type.iter().any(|t| t.to_string() == "extern") {
            rust_code.push_str("extern \"C\" ");
        }

        // Add function declaration
        rust_code.push_str("fn ");
        rust_code.push_str(element.name.as_str());
        rust_code.push('(');

        // Convert parameters
        let params = if element.parameters.is_empty() {
            String::new()
        } else {
            // Parse parameters
            let mut params = Vec::new();
            let mut current_param = Vec::new();

            for token in element.parameters {
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
            for param in params {
                if param.is_empty() {
                    continue;
                }

                // Extract parameter name (typically the last token before any array brackets)
                let mut name_idx = param.len() - 1;

                for i in (0..param.len()).rev() {
                    if param[i].to_string() == "[" || param[i].to_string() == "]" {
                        continue;
                    }
                    name_idx = i;
                    break;
                }

                let param_name = param[name_idx].to_string();

                // Parameter type is everything before the name
                let param_type_tokens = &param[0..name_idx];

                // Convert type to Rust
                let mut param_type = param_type_tokens.iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(" ");

                // Look up type conversion in our registry
                if let Some(rust_type) = convert_type(&param_type) {
                    param_type = rust_type;
                }

                rust_params.push(format!("{}: {}", param_name, param_type));
            }

            rust_params.join(", ")
        };

        rust_code.push_str(&params);
        rust_code.push(')');

        // Convert return type (skip if void)
        let return_type = {
            let return_type_str = element.return_type.iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(" ");

            // Simple conversion - in a real implementation, you'd have more logic
            match return_type_str.as_str() {
                "void" => "()".to_string(),
                _ => {
                    // Try to convert using the type registry
                    if let Some(rust_type) = convert_type(&return_type_str) {
                        rust_type
                    } else {
                        // Default to keeping the original type
                        return_type_str
                    }
                }
            }
        };

        if return_type != "()" {
            rust_code.push_str(" -> ");
            rust_code.push_str(&return_type);
        }

        // Add function body if present
        if let body = element.body {
            rust_code.push_str(" {\n");

            // Convert body content - simplified version here
            rust_code.push_str("    // TODO: Convert function body\n");

            // Real implementation would convert each statement
            let body_content = body.iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(" ");

            // Simple C to Rust conversion - placeholder for actual conversion
            let rust_body = "    // Original C code: \n    // ".to_string() + &body_content;
            rust_code.push_str(&rust_body);
            rust_code.push_str("\n}");
        } else {
            // Declaration only
            rust_code.push(';');
        }
        Ok(Some(ConvertedElement::Function(ConvertedFunction {
            return_type: "()".to_string(), // Default return type
            parameters: Vec::new(), // Default empty parameters
            body: "".to_string(), // Default empty body
            rust_code,
            is_unsafe: false,
            is_public: false,
        })))
    } else {
        Err(ConversionError::new("Failed to convert function"))
    }
}

/// Redirect callback: Handles cases where this handler should pass tokens to a different handler
fn redirect_function(tokens: &[Token], result: HandlerResult, context: &mut Context) -> Result<HandlerResult, ConversionError> {
    report!(context, "function_handler", Info, Report, 
        "Checking if function tokens should be redirected", true);
    
    // Check if this might actually be a function pointer typedef
    if tokens.iter().any(|t| t.to_string() == "typedef") {
        report!(context, "function_handler", Info, Report, 
            "Redirecting to typedef handler", true);
        return Ok(result);
    }
    
    // Check if this might be a macro that looks like a function
    if tokens.len() > 0 && tokens[0].to_string() == "#" {
        report!(context, "function_handler", Info, Report, 
            "Redirecting to macro handler", true);
        return Ok(result);
    }
    
    // Check if this is actually a struct method definition
    if tokens.iter().any(|t| t.to_string() == "struct") {
        report!(context, "function_handler", Info, Report, 
            "Redirecting to struct handler", true);
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
        report!(context, "function_handler", Info, Report, 
            "Redirecting to global handler (function pointer variable)", true);
        return Ok(result);
    }
    
    // No redirection needed
    Ok(result)
}