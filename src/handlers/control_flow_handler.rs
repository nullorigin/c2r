use super::common::{find_matching_token, not_handled, replace_with};
use crate::context::{Context, HandlerReport, ReportLevel, HandlerPhase};
use crate::context::ReportLevel::{Info, Error, Warning, Debug};
use crate::context::HandlerPhase::{Process, Handle, Extract, Convert, Report};
use crate::report;
use crate::debug;
use crate::error::ConversionError;
use crate::extract::{ExtractedElement, ExtractedControlFlow};
use crate::handler::HandlerResult;
use crate::{get_id, Token, ConvertedElement, ConvertedControlFlow};

/// Creates a control flow handler that can detect and convert C control flow statements
/// This includes if-else statements, loops (for, while, do-while), and switch statements
pub fn create_control_flow_handler() -> crate::handler::Handler {
    let handler_id = get_id("control_flow_handler");
    let handler_role = "control_flow";
    let priority = 110; // Higher than function but lower than struct/enum
    
    super::create_handler(
        handler_id,
        handler_role,
        priority,
        Some(process_control_flow),
        Some(handle_control_flow),
        Some(extract_control_flow),
        Some(convert_control_flow),
        None,
        None,
        Some(redirect_control_flow)
    )
}

/// Process callback: Initializes and confirms this handler can handle the tokens
fn process_control_flow(tokens: &[Token], context: &mut Context) -> Result<bool,ConversionError> {
    let _id = get_id("process_control_flow");
    
    if tokens.len() < 2 {
        return Ok(false);
    }
    
    // Check for control flow keywords
    let first_token = tokens[0].to_string();
    match first_token.as_str() {
        "if" => {
            report!(context, "control_flow_handler", Info, Handle, 
                "If statement detected", true);
            Ok(true)
        },
        "else" => {
            report!(context, "control_flow_handler", Info, Handle, 
                "Else statement detected", true);
            Ok(true)
        },
        "for" => {
            report!(context, "control_flow_handler", Info, Handle, 
                "For loop detected", true);
            Ok(true)
        },
        "while" => {
            report!(context, "control_flow_handler", Info, Handle, 
                "While loop detected", true);
            Ok(true)
        },
        "do" => {
            report!(context, "control_flow_handler", Info, Handle, 
                "Do-while loop detected", true);
            Ok(true)
        },
        "switch" => {
            report!(context, "control_flow_handler", Info, Handle, 
                "Switch statement detected", true);
            Ok(true)
        },
        _ => Ok(false),
    }
}

/// Process control flow constructs
fn handle_control_flow(tokens: &[Token], context: &mut Context) -> Result<HandlerResult, ConversionError> {
    let _id = get_id("handle_control_flow");
    report!(context, "control_flow_handler", Info, Handle, 
        "Control flow handler processing tokens", true);
    
    let first_token = tokens[0].to_string();
    
    match first_token.as_str() {
        "if" => handle_if_statement(tokens, context),
        "for" => handle_for_loop(tokens, context),
        "while" => handle_while_loop(tokens, context),
        "do" => handle_do_while_loop(tokens, context),
        "switch" => handle_switch_statement(tokens, context),
        _ => not_handled(),
    }
}

/// Process an if statement
fn handle_if_statement(tokens: &[Token], context: &mut Context) -> Result<HandlerResult, ConversionError> {
    let _id = get_id("handle_if_statement");
    report!(context, "control_flow_handler", Info, Process, 
        "Processing if statement", true);
    
    if tokens.len() < 5 || tokens[0].to_string() != "if" {
        return not_handled();
    }
    
    // Find the condition between parentheses
    let condition_start = 1;
    if tokens[condition_start].to_string() != "(" {
        return not_handled();
    }
    
    let condition_end = match find_matching_token(&tokens[condition_start..], "(", ")") {
        Some(pos) => condition_start + pos,
        None => return not_handled(),
    };
    
    // Extract condition without parentheses
    let condition_tokens = &tokens[condition_start + 1..condition_end];
    
    // Convert condition to Rust
    let rust_condition = convert_condition(condition_tokens, context)?;
    
    // Find the body (could be a block or a single statement)
    let body_start = condition_end + 1;
    let (rust_body, body_end) = if body_start < tokens.len() && tokens[body_start].to_string() == "{" {
        // Block body
        let body_end = match find_matching_token(&tokens[body_start..], "{", "}") {
            Some(pos) => body_start + pos + 1,
            None => return not_handled(),
        };
        
        let body_tokens = &tokens[body_start + 1..body_end - 1];
        (convert_block(body_tokens, context)?, body_end)
    } else {
        // Single statement body
        // For simplicity, assume the statement ends at a semicolon
        let mut body_end = body_start + 1;
        while body_end < tokens.len() && tokens[body_end].to_string() != ";" {
            body_end += 1;
        }
        
        let body_tokens = &tokens[body_start..body_end + 1];
        (convert_statement(body_tokens, context)?, body_end + 1)
    };
    
    // Check for an else clause
    let mut rust_code = format!("if {} {}", rust_condition, rust_body);
    
    let mut current_end = body_end;
    
    // Look for "else if" or "else"
    if current_end + 1 < tokens.len() && tokens[current_end].to_string() == "else" {
        if current_end + 2 < tokens.len() && tokens[current_end + 1].to_string() == "if" {
            // Else if branch
            let else_if_tokens = &tokens[current_end..];
            let else_if_result = handle_if_statement(else_if_tokens, context)?;
            
            if let HandlerResult::Converted(_, else_if_code, _) = else_if_result {
                rust_code = format!("{} else {}", rust_code, else_if_code);
            }
        } else {
            // Else branch
            let else_body_start = current_end + 1;
            
            let (else_body, _) = if else_body_start < tokens.len() && tokens[else_body_start].to_string() == "{" {
                // Block body
                let else_body_end = match find_matching_token(&tokens[else_body_start..], "{", "}") {
                    Some(pos) => else_body_start + pos + 1,
                    None => return not_handled(),
                };
                
                let else_body_tokens = &tokens[else_body_start + 1..else_body_end - 1];
                (convert_block(else_body_tokens, context)?, else_body_end)
            } else {
                // Single statement body
                let mut  else_body_end = else_body_start + 1;
                while else_body_end < tokens.len() && tokens[else_body_end].to_string() != ";" {
                    else_body_end += 1;
                }
                
                let else_body_tokens = &tokens[else_body_start..else_body_end + 1];
                (convert_statement(else_body_tokens, context)?, else_body_end + 1)
            };
            
            rust_code = format!("{} else {}", rust_code, else_body);
        }
    }
    
    let id = get_id("if_statement");
    replace_with(rust_code, id)
}

/// Process a for loop
fn handle_for_loop(tokens: &[Token], context: &mut Context) -> Result<HandlerResult, ConversionError> {
    report!(context, "control_flow_handler", Info, Process, 
        "Processing for loop", true);
    
    if tokens.len() < 7 || tokens[0].to_string() != "for" || tokens[1].to_string() != "(" {
        return not_handled();
    }
    
    // Find the end of the for loop header
    let header_end = match find_matching_token(&tokens[1..], "(", ")") {
        Some(pos) => 1 + pos + 1,
        None => return not_handled(),
    };
    
    // Extract initialization, condition, and increment parts
    let header_tokens = &tokens[2..header_end - 1];
    
    // Split by semicolons
    let mut parts = Vec::new();
    let mut current_part = Vec::new();
    
    for token in header_tokens {
        if token.to_string() == ";" {
            parts.push(current_part);
            current_part = Vec::new();
        } else {
            current_part.push(token.clone());
        }
    }
    
    parts.push(current_part);
    
    if parts.len() != 3 {
        return not_handled();
    }
    
    let init_tokens = &parts[0];
    let cond_tokens = &parts[1];
    let incr_tokens = &parts[2];
    
    // Try to convert to a range-based for loop if possible
    if let Some(range_loop) = convert_to_range_based_loop(init_tokens, cond_tokens, incr_tokens, context)? {
        let id = get_id("for_loop");
        return replace_with(range_loop, id);
    }
    
    // Convert to a while loop as fallback
    let mut rust_code = String::new();
    
    // Add initialization before the loop
    if !init_tokens.is_empty() {
        let init_str = tokens_to_string(init_tokens)?;
        rust_code.push_str(&format!("{};\n", init_str));
    }
    
    // Add while loop with condition
    let cond_str = if cond_tokens.is_empty() {
        "true".to_string()
    } else {
        tokens_to_string(cond_tokens)?
    };
    
    rust_code.push_str(&format!("while {} {{\n", cond_str));
    
    // Find and add the loop body
    let body_start = header_end;
    let (body_str, _) = if body_start < tokens.len() && tokens[body_start].to_string() == "{" {
        // Block body
        let body_end = match find_matching_token(&tokens[body_start..], "{", "}") {
            Some(pos) => body_start + pos + 1,
            None => return not_handled(),
        };
        
        let body_tokens = &tokens[body_start + 1..body_end - 1];
        (convert_block(body_tokens, context)?, body_end)
    } else {
        // Single statement body
        let mut body_end = body_start + 1;
        while body_end < tokens.len() && tokens[body_end].to_string() != ";" {
            body_end += 1;
        }
        
        let body_tokens = &tokens[body_start..body_end + 1];
        (convert_statement(body_tokens, context)?, body_end + 1)
    };
    
    rust_code.push_str(&body_str);
    
    // Add increment at the end of the loop body
    if !incr_tokens.is_empty() {
        let incr_str = tokens_to_string(incr_tokens)?;
        rust_code.push_str(&format!("{};\n", incr_str));
    }
    
    rust_code.push_str("}");
    
    let id = get_id("for_loop");
    replace_with(rust_code, id)
}

/// Try to convert a C-style for loop to a Rust range-based for loop
fn convert_to_range_based_loop(
    init_tokens: &[Token],
    cond_tokens: &[Token],
    incr_tokens: &[Token],
    context: &Context,
) -> Result<Option<String>, ConversionError> {
    // This is a simplified implementation that handles basic cases like:
    // for (i = 0; i < 10; i++) -> for i in 0..10
    
    // Try to extract variable name and initial value
    let init_str = tokens_to_string(init_tokens)?;
    let parts: Vec<&str> = init_str.split('=').collect();
    if parts.len() != 2 {
        return Ok(None);
    }
    
    let var_name = parts[0].trim();
    let start_value = parts[1].trim();
    
    // Validate condition format (i < n or i <= n)
    let cond_str = tokens_to_string(cond_tokens)?;
    
    let (end_value, inclusive) = if cond_str.contains('<') {
        let cond_parts: Vec<&str> = if cond_str.contains("<=") {
            cond_str.split("<=").collect()
        } else {
            cond_str.split('<').collect()
        };
        
        if cond_parts.len() != 2 || cond_parts[0].trim() != var_name {
            return Ok(None);
        }
        
        (cond_parts[1].trim(), cond_str.contains("<="))
    } else {
        return Ok(None);
    };
    
    // Check increment format (i++ or i+=1)
    let incr_str = tokens_to_string(incr_tokens)?;
    let is_increment = incr_str == format!("{}++", var_name) || 
                       incr_str == format!("++{}", var_name) ||
                       incr_str == format!("{} += 1", var_name);
                       
    if !is_increment {
        return Ok(None);
    }
    
    // Build range-based for loop
    let range_op = if inclusive { "..=" } else { ".." };
    let range_loop = format!("for {} in {}{}{}", var_name, start_value, range_op, end_value);
    
    Ok(Some(range_loop))
}

/// Process a while loop
fn handle_while_loop(tokens: &[Token], context: &mut Context) -> Result<HandlerResult, ConversionError> {
    report!(context, "control_flow_handler", Info, Process, 
        "Processing while loop", true);
    
    if tokens.len() < 5 || tokens[0].to_string() != "while" || tokens[1].to_string() != "(" {
        return not_handled();
    }
    
    // Find the condition between parentheses
    let condition_start = 1;
    let condition_end = match find_matching_token(&tokens[condition_start..], "(", ")") {
        Some(pos) => condition_start + pos,
        None => return not_handled(),
    };
    
    // Extract condition without parentheses
    let condition_tokens = &tokens[condition_start + 1..condition_end];
    
    // Check for infinite loop (while(true) or while(1))
    let is_infinite = is_infinite_loop(condition_tokens);
    
    // Convert to appropriate Rust construct
    let mut rust_code = String::new();
    
    if is_infinite {
        rust_code.push_str("loop {");
    } else {
        // Convert condition to Rust
        let rust_condition = convert_condition(condition_tokens, context)?;
        rust_code.push_str(&format!("while {} {{", rust_condition));
    }
    
    // Find and add the loop body
    let body_start = condition_end + 1;
    let (body_str, _) = if body_start < tokens.len() && tokens[body_start].to_string() == "{" {
        // Block body
        let body_end = match find_matching_token(&tokens[body_start..], "{", "}") {
            Some(pos) => body_start + pos + 1,
            None => return not_handled(),
        };
        
        let body_tokens = &tokens[body_start + 1..body_end - 1];
        (convert_block(body_tokens, context)?, body_end)
    } else {
        // Single statement body
        let mut body_end = body_start + 1;
        while body_end < tokens.len() && tokens[body_end].to_string() != ";" {
            body_end += 1;
        }
        
        let body_tokens = &tokens[body_start..body_end + 1];
        (convert_statement(body_tokens, context)?, body_end + 1)
    };
    
    rust_code.push_str(&body_str);
    rust_code.push_str("}");
    
    let id = get_id("while_loop");
    replace_with(rust_code, id)
}

/// Process a do-while loop
fn handle_do_while_loop(tokens: &[Token], context: &mut Context) -> Result<HandlerResult, ConversionError> {
    report!(context, "control_flow_handler", Info, Process, 
        "Processing do-while loop", true);
    
    if tokens.len() < 7 || tokens[0].to_string() != "do" {
        return not_handled();
    }
    
    // Find the body
    let body_start = 1;
    let body_end = if tokens[body_start].to_string() == "{" {
        match find_matching_token(&tokens[body_start..], "{", "}") {
            Some(pos) => body_start + pos + 1,
            None => return not_handled(),
        }
    } else {
        // Find the end of a single statement
        let mut end = body_start;
        while end < tokens.len() && tokens[end].to_string() != ";" {
            end += 1;
        }
        end + 1
    };
    
    // Check for "while" after the body
    if body_end >= tokens.len() || tokens[body_end].to_string() != "while" {
        return not_handled();
    }
    
    // Find the condition between parentheses
    let condition_start = body_end + 1;
    if condition_start >= tokens.len() || tokens[condition_start].to_string() != "(" {
        return not_handled();
    }
    
    let condition_end = match find_matching_token(&tokens[condition_start..], "(", ")") {
        Some(pos) => condition_start + pos,
        None => return not_handled(),
    };
    
    // Extract condition without parentheses
    let condition_tokens = &tokens[condition_start + 1..condition_end];
    
    // Convert condition to Rust
    let rust_condition = convert_condition(condition_tokens, context)?;
    
    // Extract and convert body
    let body_tokens = if tokens[body_start].to_string() == "{" {
        &tokens[body_start + 1..body_end - 1]
    } else {
        &tokens[body_start..body_end]
    };
    
    let body_str = convert_block(body_tokens, context)?;
    
    // Build Rust code - use loop with a conditional break at the end
    let rust_code = format!("loop {{\n{}\nif !({}) {{ break; }}\n}}", body_str, rust_condition);
    
    let id = get_id("do_while_loop");
    replace_with(rust_code, id)
}

/// Process a switch statement
fn handle_switch_statement(tokens: &[Token], context: &mut Context) -> Result<HandlerResult, ConversionError> {
    report!(context, "control_flow_handler", Info, Process, 
        "Processing switch statement", true);
    
    if tokens.len() < 5 || tokens[0].to_string() != "switch" || tokens[1].to_string() != "(" {
        return not_handled();
    }
    
    // Find the condition between parentheses
    let condition_start = 1;
    let condition_end = match find_matching_token(&tokens[condition_start..], "(", ")") {
        Some(pos) => condition_start + pos,
        None => return not_handled(),
    };
    
    // Extract condition without parentheses
    let condition_tokens = &tokens[condition_start + 1..condition_end];
    
    // Convert condition to Rust
    let rust_condition = convert_condition(condition_tokens, context)?;
    
    // Find the switch body
    let body_start = condition_end + 1;
    if body_start >= tokens.len() || tokens[body_start].to_string() != "{" {
        return not_handled();
    }
    
    let body_end = match find_matching_token(&tokens[body_start..], "{", "}") {
        Some(pos) => body_start + pos + 1,
        None => return not_handled(),
    };
    
    // Extract body tokens without braces
    let body_tokens = &tokens[body_start + 1..body_end - 1];
    
    // Parse case statements
    let mut rust_code = format!("match {} {{\n", rust_condition);
    let mut i = 0;
    
    while i < body_tokens.len() {
        if body_tokens[i].to_string() == "case" {
            // Found a case statement
            i += 1;
            if i >= body_tokens.len() {
                return not_handled();
            }
            
            // Extract case value
            let case_value = body_tokens[i].to_string();
            i += 1;
            
            // Skip colon
            if i >= body_tokens.len() || body_tokens[i].to_string() != ":" {
                return not_handled();
            }
            i += 1;
            
            // Find the end of this case (next case, default, or end of switch)
            let mut case_end = i;
            while case_end < body_tokens.len() {
                let token_str = body_tokens[case_end].to_string();
                if token_str == "case" || token_str == "default" {
                    break;
                }
                case_end += 1;
            }
            
            // Extract and convert case body
            let case_body_tokens = &body_tokens[i..case_end];
            let case_body = convert_block(case_body_tokens, context)?;
            
            // Add to Rust code
            rust_code.push_str(&format!("    {} => {{\n{}\n    }},\n", case_value, case_body));
            
            i = case_end;
        } else if body_tokens[i].to_string() == "default" {
            // Found default case
            i += 1;
            
            // Skip colon
            if i >= body_tokens.len() || body_tokens[i].to_string() != ":" {
                return not_handled();
            }
            i += 1;
            
            // Extract to end of switch
            let default_body_tokens = &body_tokens[i..];
            let default_body = convert_block(default_body_tokens, context)?;
            
            // Add to Rust code
            rust_code.push_str(&format!("    _ => {{\n{}\n    }},\n", default_body));
            
            break;
        } else {
            i += 1;
        }
    }
    
    rust_code.push_str("}");
    
    let id = get_id("switch_statement");
    replace_with(rust_code, id)
}

/// Check if a condition represents an infinite loop
fn is_infinite_loop(condition_tokens: &[Token]) -> bool {
    if condition_tokens.len() == 1 {
        let token_str = condition_tokens[0].to_string();
        if token_str == "true" || token_str == "1" {
            return true;
        }
    }
    
    false
}

/// Convert a condition from C to Rust
fn convert_condition(tokens: &[Token], _context: &Context) -> Result<String, ConversionError> {
    // A full implementation would need more sophisticated translation
    // This is a simplified version that just concatenates tokens
    tokens_to_string(tokens)
}

/// Convert a block of statements
fn convert_block(tokens: &[Token], _context: &Context) -> Result<String, ConversionError> {
    // This is a simplified implementation
    // A full version would need to handle statement parsing and conversion
    let block_str = tokens_to_string(tokens)?;
    Ok(format!("{{\n{}\n}}", block_str))
}

/// Convert a single statement
fn convert_statement(tokens: &[Token], _context: &Context) -> Result<String, ConversionError> {
    // This is a simplified implementation
    let stmt_str = tokens_to_string(tokens)?;
    Ok(format!("{{\n{}\n}}", stmt_str))
}

/// Helper function to convert tokens to a string representation
fn tokens_to_string(tokens: &[Token]) -> Result<String, ConversionError> {
    let mut result = String::new();
    
    for (i, token) in tokens.iter().enumerate() {
        let token_str = token.to_string();
        
        // Add space between tokens if needed
        if i > 0 {
            let prev_token = tokens[i - 1].to_string();
            
            // Add space if both tokens are identifiers or literals
            if (is_identifier(&prev_token) || is_literal(&prev_token)) &&
               (is_identifier(&token_str) || is_literal(&token_str)) {
                result.push(' ');
            }
            
            // Add space around operators
            if is_operator(&prev_token) || is_operator(&token_str) {
                result.push(' ');
            }
        }
        
        result.push_str(&token_str);
    }
    
    Ok(result)
}

/// Check if a token is an identifier
fn is_identifier(token: &str) -> bool {
    if let Some(first_char) = token.chars().next() {
        first_char.is_alphabetic() || first_char == '_'
    } else {
        false
    }
}

/// Check if a token is a literal
fn is_literal(token: &str) -> bool {
    if let Some(first_char) = token.chars().next() {
        first_char.is_digit(10) || token.starts_with('\"') || token.starts_with('\'')
    } else {
        false
    }
}

/// Check if a token is an operator
fn is_operator(token: &str) -> bool {
    match token {
        "+" | "-" | "*" | "/" | "%" | "=" | "==" | "!=" | "<" | ">" | "<=" | ">=" | "+=" | "-=" | "*=" | "/=" | "%=" => true,
        _ => false,
    }
}

/// Extract callback: Extracts control flow metadata
fn extract_control_flow(tokens: &[Token], context: &mut Context) -> Result<Option<ExtractedElement>, ConversionError> {
    let _id = get_id("extract_control_flow");
    report!(context, "control_flow_handler", Info, Extract, 
        format!("Extracting control flow from {} tokens", tokens.len()), true);
    
    if tokens.is_empty() {
        return Ok(None);
    }
    
    let control_type = tokens[0].to_string();
    match control_type.as_str() {
        "if" | "for" | "while" | "do" | "switch" => {
            let extracted = ExtractedControlFlow {
                control_type: control_type.clone(),
                condition: extract_condition_tokens(tokens, &control_type)?,
                body: extract_body_tokens(tokens, &control_type)?,
                original_code: tokens.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(" "),
                tokens: tokens.to_vec(),
            };
            
            Ok(Some(ExtractedElement::ControlFlow(extracted)))
        },
        _ => Ok(None),
    }
}

/// Helper function to extract condition tokens
fn extract_condition_tokens(tokens: &[Token], control_type: &str) -> Result<Vec<Token>, ConversionError> {
    match control_type {
        "if" | "while" | "switch" => {
            // Find condition between parentheses
            if tokens.len() >= 2 && tokens[1].to_string() == "(" {
                if let Some(end_pos) = find_matching_token(&tokens[1..], "(", ")") {
                    return Ok(tokens[2..1 + end_pos].to_vec());
                }
            }
        },
        "for" => {
            // For loops have three parts separated by semicolons
            if tokens.len() >= 2 && tokens[1].to_string() == "(" {
                if let Some(end_pos) = find_matching_token(&tokens[1..], "(", ")") {
                    return Ok(tokens[2..1 + end_pos].to_vec());
                }
            }
        },
        "do" => {
            // Do-while has condition at the end
            // Find "while" keyword and extract condition after it
            for i in 0..tokens.len() {
                if tokens[i].to_string() == "while" && i + 1 < tokens.len() && tokens[i + 1].to_string() == "(" {
                    if let Some(end_pos) = find_matching_token(&tokens[i + 1..], "(", ")") {
                        return Ok(tokens[i + 2..i + 1 + end_pos].to_vec());
                    }
                }
            }
        },
        _ => {}
    }
    
    Ok(vec![])
}

/// Helper function to extract body tokens
fn extract_body_tokens(tokens: &[Token], control_type: &str) -> Result<Vec<Token>, ConversionError> {
    match control_type {
        "if" | "while" | "switch" => {
            // Find body after condition
            if tokens.len() >= 2 && tokens[1].to_string() == "(" {
                if let Some(cond_end) = find_matching_token(&tokens[1..], "(", ")") {
                    let body_start = 1 + cond_end + 1;
                    if body_start < tokens.len() {
                        if tokens[body_start].to_string() == "{" {
                            if let Some(body_end) = find_matching_token(&tokens[body_start..], "{", "}") {
                                return Ok(tokens[body_start + 1..body_start + body_end].to_vec());
                            }
                        }
                    }
                }
            }
        },
        "for" => {
            // Similar to if/while
            if tokens.len() >= 2 && tokens[1].to_string() == "(" {
                if let Some(header_end) = find_matching_token(&tokens[1..], "(", ")") {
                    let body_start = 1 + header_end + 1;
                    if body_start < tokens.len() {
                        if tokens[body_start].to_string() == "{" {
                            if let Some(body_end) = find_matching_token(&tokens[body_start..], "{", "}") {
                                return Ok(tokens[body_start + 1..body_start + body_end].to_vec());
                            }
                        }
                    }
                }
            }
        },
        "do" => {
            // Body comes first in do-while
            let body_start = 1;
            if body_start < tokens.len() {
                if tokens[body_start].to_string() == "{" {
                    if let Some(body_end) = find_matching_token(&tokens[body_start..], "{", "}") {
                        return Ok(tokens[body_start + 1..body_start + body_end].to_vec());
                    }
                }
            }
        },
        _ => {}
    }
    
    Ok(vec![])
}

/// Convert callback: Does the actual conversion of C to Rust code
fn convert_control_flow(tokens: &[Token], context: &mut Context) -> Result<Option<ConvertedElement>, ConversionError> {
    let _id = get_id("convert_control_flow");
    report!(context, "control_flow_handler", Info, Convert, 
        format!("Converting control flow from {} tokens", tokens.len()), true);
    
    if let Some(ExtractedElement::ControlFlow(extracted)) = extract_control_flow(tokens, context)? {
        let rust_code = match extracted.control_type.as_str() {
            "if" => convert_if_to_rust(&extracted, context)?,
            "for" => convert_for_to_rust(&extracted, context)?,
            "while" => convert_while_to_rust(&extracted, context)?,
            "do" => convert_do_while_to_rust(&extracted, context)?,
            "switch" => convert_switch_to_rust(&extracted, context)?,
            _ => return Err(ConversionError::new("Unsupported control flow type")),
        };
        
        let id = get_id("convert_control_flow");
        Ok(Some(ConvertedElement::ControlFlow(ConvertedControlFlow {
            control_type: "".to_string(), // Default empty control type
            condition: "".to_string(), // Default empty condition
            body: "".to_string(), // Default empty body
            rust_code,
            has_else: false,
        })))
    } else {
        Err(ConversionError::new("Could not extract control flow for conversion"))
    }
}

/// Convert if statement to Rust
fn convert_if_to_rust(extracted: &ExtractedControlFlow, context: &Context) -> Result<String, ConversionError> {
    let condition = tokens_to_string(&extracted.condition)?;
    let body = tokens_to_string(&extracted.body)?;
    
    Ok(format!("if {} {{\n{}\n}}", condition, body))
}

/// Convert for loop to Rust
fn convert_for_to_rust(extracted: &ExtractedControlFlow, context: &Context) -> Result<String, ConversionError> {
    // Try to convert to range-based for loop, fallback to while loop
    let condition_str = tokens_to_string(&extracted.condition)?;
    let body = tokens_to_string(&extracted.body)?;
    
    // Simplified conversion - a full implementation would parse the for loop components
    Ok(format!("// TODO: Convert for loop\nwhile {} {{\n{}\n}}", condition_str, body))
}

/// Convert while loop to Rust
fn convert_while_to_rust(extracted: &ExtractedControlFlow, context: &Context) -> Result<String, ConversionError> {
    let condition = tokens_to_string(&extracted.condition)?;
    let body = tokens_to_string(&extracted.body)?;
    
    // Check for infinite loop
    if condition == "true" || condition == "1" {
        Ok(format!("loop {{\n{}\n}}", body))
    } else {
        Ok(format!("while {} {{\n{}\n}}", condition, body))
    }
}

/// Convert do-while loop to Rust
fn convert_do_while_to_rust(extracted: &ExtractedControlFlow, context: &Context) -> Result<String, ConversionError> {
    let condition = tokens_to_string(&extracted.condition)?;
    let body = tokens_to_string(&extracted.body)?;
    
    Ok(format!("loop {{\n{}\nif !({}) {{ break; }}\n}}", body, condition))
}

/// Convert switch statement to Rust match
fn convert_switch_to_rust(extracted: &ExtractedControlFlow, context: &Context) -> Result<String, ConversionError> {
    let condition = tokens_to_string(&extracted.condition)?;
    let body = tokens_to_string(&extracted.body)?;
    
    // Simplified conversion - a full implementation would parse case statements
    Ok(format!("match {} {{\n// TODO: Parse case statements\n{}\n}}", condition, body))
}

/// Redirect callback: Handles cases where this handler should pass tokens to a different handler
fn redirect_control_flow(tokens: &[Token], result: HandlerResult, context: &mut Context) -> Result<HandlerResult, ConversionError> {
    let _id = get_id("redirect_control_flow");
    report!(context, "control_flow_handler", Info, Report, 
        "Checking if control flow tokens should be redirected", true);
    
    // Check if this is actually a function call that looks like a control statement
    if tokens.iter().any(|t| t.to_string() == "(") && tokens.iter().any(|t| t.to_string() == ")") {
        // Look for patterns that suggest function calls
        for i in 0..tokens.len() {
            if tokens[i].to_string() == "(" {
                // Check if there's an identifier before the parenthesis that's not a control keyword
                if i > 0 {
                    let prev_token = tokens[i - 1].to_string();
                    if !matches!(prev_token.as_str(), "if" | "while" | "for" | "switch") && 
                       is_identifier(&prev_token) {
                        report!(context, "control_flow_handler", Info, Report, 
                            "Redirecting to function handler (function call)", true);
                        return Ok(HandlerResult::Redirected(
                            Some(tokens.to_vec()),
                            "control_flow_handler".to_string(),
                            _id,
                            get_id("function_handler")
                        ));
                    }
                }
            }
        }
    }
    
    // Check if this contains variable declarations
    if tokens.iter().any(|t| t.to_string() == "=") && 
       !tokens.iter().any(|t| matches!(t.to_string().as_str(), "if" | "while" | "for" | "switch" | "do")) {
        report!(context, "control_flow_handler", Info, Report, 
            "Redirecting to global handler (variable declaration)", true);
        return Ok(HandlerResult::Redirected(
            Some(tokens.to_vec()),
            "control_flow_handler".to_string(),
            _id,
            get_id("global_handler")
        ));
    }
    
    // No redirection needed
    Ok(result)
}
