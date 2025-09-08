use super::common::{find_matching_token, not_handled, replace_with_range};
use crate::config::HandlerPhase::{Convert, Extract, Handle, Process, Report};
use crate::config::HandlerReport;
use crate::config::ReportLevel::{Info, Warning};
use crate::config::{HandlerPhase, ReportLevel};
use crate::error::ConversionError;
use crate::extract::{ExtractedControlFlow, ExtractedElement};
use crate::handler::HandlerResult;
use crate::Id;
use crate::{context, report};
use crate::{ConvertedControlFlow, ConvertedElement, Token};

/// Creates a control flow handler that can detect and convert C control flow statements
/// This includes if-else statements, loops (for, while, do-while), and switch statements
pub fn create_control_flow_handler() -> crate::handler::Handler {
    let handler_id = Id::get("control_flow_handler");
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
        Some(report_control_flow),
        Some(result_control_flow),
        Some(redirect_control_flow),
    )
}

/// Process callback: Initializes and confirms this handler can handle the tokens
fn process_control_flow(tokens: &[Token]) -> Result<bool, ConversionError> {
    let id = Id::get("process_control_flow");

    if tokens.len() < 2 {
        return Ok(false);
    }

    // Check for control flow keywords
    let first_token = tokens[0].to_string();
    match first_token.as_str() {
        "if" => {
            report!(
                "control_flow_handler",
                "process_control_flow",
                Info,
                Handle,
                "If statement detected",
                true
            );
            Ok(true)
        }
        "else" => {
            report!(
                "control_flow_handler",
                "process_control_flow",
                Info,
                Handle,
                "Else statement detected",
                true
            );
            Ok(true)
        }
        "for" => {
            report!(
                "control_flow_handler",
                "process_control_flow",
                Info,
                Handle,
                "For loop detected",
                true
            );
            Ok(true)
        }
        "while" => {
            report!(
                "control_flow_handler",
                "process_control_flow",
                Info,
                Handle,
                "While loop detected",
                true
            );
            Ok(true)
        }
        "do" => {
            report!(
                "control_flow_handler",
                "process_control_flow",
                Info,
                Handle,
                "Do-while loop detected",
                true
            );
            Ok(true)
        }
        "switch" => {
            report!(
                "control_flow_handler",
                "process_control_flow",
                Info,
                Handle,
                "Switch statement detected",
                true
            );
            Ok(true)
        }
        _ => Ok(false),
    }
}

/// Process control flow constructs
fn handle_control_flow(
    tokens: &[Token]) -> Result<HandlerResult, ConversionError> {
    let _id = Id::get("handle_control_flow");
    report!(
        "control_flow_handler",
        "handle_control_flow",
        Info,
        Handle,
        "Control flow handler processing tokens",
        true
    );

    let first_token = tokens[0].to_string();

    match first_token.as_str() {
        "if" => handle_if_statement(tokens),
        "for" => handle_for_loop(tokens),
        "while" => handle_while_loop(tokens),
        "do" => handle_do_while_loop(tokens),
        "switch" => handle_switch_statement(tokens),
        _ => not_handled(),
    }
}

/// Process an if statement
fn handle_if_statement(
    tokens: &[Token]) -> Result<HandlerResult, ConversionError> {
    let _id = Id::get("handle_if_statement");
    report!(
        "control_flow_handler",
        "handle_if_statement",
        Info,
        Process,
        "Processing if statement",
        true
    );

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
    let rust_condition = convert_condition(condition_tokens)?;

    // Find the body (could be a block or a single statement)
    let body_start = condition_end + 1;
    let (rust_body, body_end) =
        if body_start < tokens.len() && tokens[body_start].to_string() == "{" {
            // Block body
            let body_end = match find_matching_token(&tokens[body_start..], "{", "}") {
                Some(pos) => body_start + pos + 1,
                None => return not_handled(),
            };

            let body_tokens = &tokens[body_start + 1..body_end - 1];
            (convert_block(body_tokens)?, body_end)
        } else {
            // Single statement body
            // For simplicity, assume the statement ends at a semicolon
            let mut body_end = body_start + 1;
            while body_end < tokens.len() && tokens[body_end].to_string() != ";" {
                body_end += 1;
            }

            let body_tokens = &tokens[body_start..body_end + 1];
            (convert_statement(body_tokens)?, body_end + 1)
        };

    // Check for an else clause
    let mut rust_code = format!("if {} {}", rust_condition, rust_body);

    let current_end = body_end;

    // Look for "else if" or "else"
    if current_end + 1 < tokens.len() && tokens[current_end].to_string() == "else" {
        if current_end + 2 < tokens.len() && tokens[current_end + 1].to_string() == "if" {
            // Else if branch
            let else_if_tokens = &tokens[current_end..];
            let else_if_result = handle_if_statement(else_if_tokens)?;

            if let HandlerResult::Converted(_, _, else_if_code, _) = else_if_result {
                rust_code = format!("{} else {}", rust_code, else_if_code);
            }
        } else {
            // Else branch
            let else_body_start = current_end + 1;

            let (else_body, _) =
                if else_body_start < tokens.len() && tokens[else_body_start].to_string() == "{" {
                    // Block body
                    let else_body_end =
                        match find_matching_token(&tokens[else_body_start..], "{", "}") {
                            Some(pos) => else_body_start + pos + 1,
                            None => return not_handled(),
                        };

                    let else_body_tokens = &tokens[else_body_start + 1..else_body_end - 1];
                    (convert_block(else_body_tokens)?, else_body_end)
                } else {
                    // Single statement body
                    let mut else_body_end = else_body_start + 1;
                    while else_body_end < tokens.len() && tokens[else_body_end].to_string() != ";" {
                        else_body_end += 1;
                    }

                    let else_body_tokens = &tokens[else_body_start..else_body_end + 1];
                    (
                        convert_statement(else_body_tokens)?,
                        else_body_end + 1,
                    )
                };

            rust_code = format!("{} else {}", rust_code, else_body);
        }
    }

    let id = Id::get("if_statement");
    // Use the existing boundary calculation from the parsing logic above
    let end_pos = if current_end + 1 < tokens.len() && tokens[current_end].to_string() == "else" {
        // If there's an else clause, we need to find where it ends
        // This logic should already be handled above, but we need the final position
        tokens.len() // For now, consume all tokens in the if-else construct
    } else {
        current_end // Use the body_end calculated above
    };
    let token_range = 0..end_pos;
    replace_with_range(rust_code, token_range, id)
}

/// Process a for loop
fn handle_for_loop(
    tokens: &[Token]) -> Result<HandlerResult, ConversionError> {
    report!(
        "control_flow_handler",
        "handle_for_loop",
        Info,
        Process,
        "Processing for loop",
        true
    );

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
    if let Some(range_loop) =
        convert_to_range_based_loop(init_tokens, cond_tokens, incr_tokens)?
    {
        let id = Id::get("for_loop");
        // The existing parsing logic below will calculate body_end for the fallback
        // We need to calculate it here too for consistency
        let body_start = header_end; // After the closing parenthesis
        let body_end = if body_start < tokens.len() && tokens[body_start].to_string() == "{" {
            match find_matching_token(&tokens[body_start..], "{", "}") {
                Some(pos) => body_start + pos + 1,
                None => tokens.len(),
            }
        } else {
            // Single statement body - find semicolon
            let mut end = body_start + 1;
            while end < tokens.len() && tokens[end].to_string() != ";" {
                end += 1;
            }
            end + 1
        };
        let token_range = 0..body_end;
        return replace_with_range(range_loop, token_range, id);
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
    let (body_str, body_end) = if body_start < tokens.len() && tokens[body_start].to_string() == "{" {
        // Block body
        let body_end = match find_matching_token(&tokens[body_start..], "{", "}") {
            Some(pos) => body_start + pos + 1,
            None => return not_handled(),
        };

        let body_tokens = &tokens[body_start + 1..body_end - 1];
        (convert_block(body_tokens)?, body_end)
    } else {
        // Single statement body
        let mut body_end = body_start + 1;
        while body_end < tokens.len() && tokens[body_end].to_string() != ";" {
            body_end += 1;
        }

        let body_tokens = &tokens[body_start..body_end + 1];
        (convert_statement(body_tokens)?, body_end + 1)
    };

    rust_code.push_str(&body_str);

    // Add increment at the end of the loop body
    if !incr_tokens.is_empty() {
        let incr_str = tokens_to_string(incr_tokens)?;
        rust_code.push_str(&format!("{};\n", incr_str));
    }

    rust_code.push_str("}");

    let id = Id::get("for_loop");
    // Use the body_end calculated by the existing parsing logic above
    let token_range = 0..body_end;
    replace_with_range(rust_code, token_range, id)
}

/// Try to convert a C-style for loop to a Rust range-based for loop
fn convert_to_range_based_loop(
    init_tokens: &[Token],
    cond_tokens: &[Token],
    incr_tokens: &[Token],
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
    let is_increment = incr_str == format!("{}++", var_name)
        || incr_str == format!("++{}", var_name)
        || incr_str == format!("{} += 1", var_name);

    if !is_increment {
        return Ok(None);
    }

    // Build range-based for loop
    let range_op = if inclusive { "..=" } else { ".." };
    let range_loop = format!(
        "for {} in {}{}{}",
        var_name, start_value, range_op, end_value
    );

    Ok(Some(range_loop))
}

/// Process a while loop
fn handle_while_loop(
    tokens: &[Token]) -> Result<HandlerResult, ConversionError> {
    report!(
        "control_flow_handler",
        "handle_while_loop",
        Info,
        Process,
        "Processing while loop",
        true
    );

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
        let rust_condition = convert_condition(condition_tokens)?;
        rust_code.push_str(&format!("while {} {{", rust_condition));
    }

    // Find and add the loop body
    let body_start = condition_end + 1;
    let (body_str, body_end) = if body_start < tokens.len() && tokens[body_start].to_string() == "{" {
        // Block body
        let body_end = match find_matching_token(&tokens[body_start..], "{", "}") {
            Some(pos) => body_start + pos + 1,
            None => return not_handled(),
        };

        let body_tokens = &tokens[body_start + 1..body_end - 1];
        (convert_block(body_tokens)?, body_end)
    } else {
        // Single statement body
        let mut body_end = body_start + 1;
        while body_end < tokens.len() && tokens[body_end].to_string() != ";" {
            body_end += 1;
        }

        let body_tokens = &tokens[body_start..body_end + 1];
        (convert_statement(body_tokens)?, body_end + 1)
    };

    rust_code.push_str(&body_str);
    rust_code.push_str("}");

    let id = Id::get("while_loop");
    // Use the body_end calculated by the existing parsing logic above
    let token_range = 0..body_end;
    replace_with_range(rust_code, token_range, id)
}

/// Process a do-while loop
fn handle_do_while_loop(
    tokens: &[Token]) -> Result<HandlerResult, ConversionError> {
    report!(
        "control_flow_handler",
        "handle_do_while_loop",
        Info,
        Process,
        "Processing do-while loop",
        true
    );

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
    let rust_condition = convert_condition(condition_tokens)?;

    // Extract and convert body
    let body_tokens = if tokens[body_start].to_string() == "{" {
        &tokens[body_start + 1..body_end - 1]
    } else {
        &tokens[body_start..body_end]
    };

    let body_str = convert_block(body_tokens)?;

    // Build Rust code - use loop with a conditional break at the end
    let rust_code = format!(
        "loop {{\n{}\nif !({}) {{ break; }}\n}}",
        body_str, rust_condition
    );

    let id = Id::get("do_while_loop");
    // Use the condition_end calculated by the existing parsing logic above
    // Need to find the semicolon after the closing parenthesis
    let mut final_end = condition_end + 1;
    while final_end < tokens.len() && tokens[final_end].to_string() != ";" {
        final_end += 1;
    }
    if final_end < tokens.len() {
        final_end += 1; // Include the semicolon
    }
    let token_range = 0..final_end;
    replace_with_range(rust_code, token_range, id)
}

/// Process a switch statement
fn handle_switch_statement(
    tokens: &[Token]) -> Result<HandlerResult, ConversionError> {
    report!(
        "control_flow_handler",
        "handle_switch_statement",
        Info,
        Process,
        "Processing switch statement",
        true
    );

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
    let rust_condition = convert_condition(condition_tokens)?;

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
            let case_body = convert_block(case_body_tokens)?;

            // Add to Rust code
            rust_code.push_str(&format!(
                "    {} => {{\n{}\n    }},\n",
                case_value, case_body
            ));

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
            let default_body = convert_block(default_body_tokens)?;

            // Add to Rust code
            rust_code.push_str(&format!("    _ => {{\n{}\n    }},\n", default_body));

            break;
        } else {
            i += 1;
        }
    }

    rust_code.push_str("}");

    let id = Id::get("switch_statement");
    // Use the body_end calculated by the existing parsing logic above
    let token_range = 0..body_end;
    replace_with_range(rust_code, token_range, id)
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
fn convert_condition(tokens: &[Token]) -> Result<String, ConversionError> {
    // A full implementation would need more sophisticated translation
    // This is a simplified version that just concatenates tokens
    tokens_to_string(tokens)
}

/// Convert a block of statements
fn convert_block(tokens: &[Token]) -> Result<String, ConversionError> {
    // This is a simplified implementation
    // A full version would need to handle statement parsing and conversion
    let block_str = tokens_to_string(tokens)?;
    Ok(format!("{{\n{}\n}}", block_str))
}

/// Convert a single statement
fn convert_statement(tokens: &[Token]) -> Result<String, ConversionError> {
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
            if (is_identifier(&prev_token) || is_literal(&prev_token))
                && (is_identifier(&token_str) || is_literal(&token_str))
            {
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
        "+" | "-" | "*" | "/" | "%" | "=" | "==" | "!=" | "<" | ">" | "<=" | ">=" | "+=" | "-="
        | "*=" | "/=" | "%=" => true,
        _ => false,
    }
}

/// Extract callback: Extracts control flow metadata
fn extract_control_flow(
    tokens: &[Token]) -> Result<Option<ExtractedElement>, ConversionError> {
    let _id = Id::get("extract_control_flow");
    report!(
        "control_flow_handler",
        "extract_control_flow",
        Info,
        Extract,
        format!("Extracting control flow from {} tokens", tokens.len()),
        true
    );

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
                original_code: tokens
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(" "),
                tokens: tokens.to_vec(),
            };

            Ok(Some(ExtractedElement::ControlFlow(extracted)))
        }
        _ => Ok(None),
    }
}

/// Helper function to extract condition tokens
fn extract_condition_tokens(
    tokens: &[Token],
    control_type: &str,
) -> Result<Vec<Token>, ConversionError> {
    match control_type {
        "if" | "while" | "switch" => {
            // Find condition between parentheses
            if tokens.len() >= 2 && tokens[1].to_string() == "(" {
                if let Some(end_pos) = find_matching_token(&tokens[1..], "(", ")") {
                    return Ok(tokens[2..1 + end_pos].to_vec());
                }
            }
        }
        "for" => {
            // For loops have three parts separated by semicolons
            if tokens.len() >= 2 && tokens[1].to_string() == "(" {
                if let Some(end_pos) = find_matching_token(&tokens[1..], "(", ")") {
                    return Ok(tokens[2..1 + end_pos].to_vec());
                }
            }
        }
        "do" => {
            // Do-while has condition at the end
            // Find "while" keyword and extract condition after it
            for i in 0..tokens.len() {
                if tokens[i].to_string() == "while"
                    && i + 1 < tokens.len()
                    && tokens[i + 1].to_string() == "("
                {
                    if let Some(end_pos) = find_matching_token(&tokens[i + 1..], "(", ")") {
                        return Ok(tokens[i + 2..i + 1 + end_pos].to_vec());
                    }
                }
            }
        }
        _ => {}
    }

    Ok(vec![])
}

/// Helper function to extract body tokens
fn extract_body_tokens(
    tokens: &[Token],
    control_type: &str,
) -> Result<Vec<Token>, ConversionError> {
    match control_type {
        "if" | "while" | "switch" => {
            // Find body after condition
            if tokens.len() >= 2 && tokens[1].to_string() == "(" {
                if let Some(cond_end) = find_matching_token(&tokens[1..], "(", ")") {
                    let body_start = 1 + cond_end + 1;
                    if body_start < tokens.len() {
                        if tokens[body_start].to_string() == "{" {
                            if let Some(body_end) =
                                find_matching_token(&tokens[body_start..], "{", "}")
                            {
                                return Ok(tokens[body_start + 1..body_start + body_end].to_vec());
                            }
                        }
                    }
                }
            }
        }
        "for" => {
            // Similar to if/while
            if tokens.len() >= 2 && tokens[1].to_string() == "(" {
                if let Some(header_end) = find_matching_token(&tokens[1..], "(", ")") {
                    let body_start = 1 + header_end + 1;
                    if body_start < tokens.len() {
                        if tokens[body_start].to_string() == "{" {
                            if let Some(body_end) =
                                find_matching_token(&tokens[body_start..], "{", "}")
                            {
                                return Ok(tokens[body_start + 1..body_start + body_end].to_vec());
                            }
                        }
                    }
                }
            }
        }
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
        }
        _ => {}
    }

    Ok(vec![])
}

/// Convert callback: Does the actual conversion of C to Rust code
fn convert_control_flow(
    tokens: &[Token]) -> Result<Option<ConvertedElement>, ConversionError> {
    let id = Id::get("convert_control_flow");
    report!(
        "control_flow_handler",
        "convert_control_flow",
        Info,
        Convert,
        format!("Converting control flow from {} tokens", tokens.len()),
        true
    );

    if let Some(ExtractedElement::ControlFlow(extracted)) = extract_control_flow(tokens)? {
        let rust_code = match extracted.control_type.as_str() {
            "if" => convert_if_to_rust(&extracted)?,
            "for" => convert_for_to_rust(&extracted)?,
            "while" => convert_while_to_rust(&extracted)?,
            "do" => convert_do_while_to_rust(&extracted)?,
            "switch" => convert_switch_to_rust(&extracted)?,
            _ => return Err(ConversionError::new("Unsupported control flow type")),
        };

        let id = Id::get("convert_control_flow");
        Ok(Some(ConvertedElement::ControlFlow(ConvertedControlFlow {
            control_type: "".to_string(), // Default empty control type
            condition: "".to_string(),    // Default empty condition
            body: "".to_string(),         // Default empty body
            rust_code,
            has_else: false,
        })))
    } else {
        Err(ConversionError::new(
            "Could not extract control flow for conversion",
        ))
    }
}

/// Convert if statement to Rust
fn convert_if_to_rust(
    extracted: &ExtractedControlFlow
) -> Result<String, ConversionError> {
    let condition = tokens_to_string(&extracted.condition)?;
    let body = tokens_to_string(&extracted.body)?;

    Ok(format!("if {} {{\n{}\n}}", condition, body))
}

/// Convert for loop to Rust
fn convert_for_to_rust(
    extracted: &ExtractedControlFlow
) -> Result<String, ConversionError> {
    // Try to convert to range-based for loop, fallback to while loop
    let condition_str = tokens_to_string(&extracted.condition)?;
    let body = tokens_to_string(&extracted.body)?;

    // Simplified conversion - a full implementation would parse the for loop components
    Ok(format!(
        "// TODO: Convert for loop\nwhile {} {{\n{}\n}}",
        condition_str, body
    ))
}

/// Convert while loop to Rust
fn convert_while_to_rust(
    extracted: &ExtractedControlFlow
) -> Result<String, ConversionError> {
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
fn convert_do_while_to_rust(
    extracted: &ExtractedControlFlow
) -> Result<String, ConversionError> {
    let condition = tokens_to_string(&extracted.condition)?;
    let body = tokens_to_string(&extracted.body)?;

    Ok(format!(
        "loop {{\n{}\nif !({}) {{ break; }}\n}}",
        body, condition
    ))
}

/// Convert switch statement to Rust match
fn convert_switch_to_rust(
    extracted: &ExtractedControlFlow
) -> Result<String, ConversionError> {
    let condition = tokens_to_string(&extracted.condition)?;
    let body = tokens_to_string(&extracted.body)?;

    // Simplified conversion - a full implementation would parse case statements
    Ok(format!(
        "match {} {{\n// TODO: Parse case statements\n{}\n}}",
        condition, body
    ))
}

/// Redirect callback: Handles cases where this handler should pass tokens to a different handler
fn redirect_control_flow(
    tokens: &[Token],
    result: HandlerResult) -> Result<HandlerResult, ConversionError> {
    let id = Id::get("redirect_control_flow");
    report!(
        "control_flow_handler",
        "redirect_control_flow",
        Info,
        Report,
        "Checking if control flow tokens should be redirected",
        true
    );

    // Check if this is actually a function call that looks like a control statement
    if tokens.iter().any(|t| t.to_string() == "(") && tokens.iter().any(|t| t.to_string() == ")") {
        // Look for patterns that suggest function calls
        for i in 0..tokens.len() {
            if tokens[i].to_string() == "(" {
                // Check if there's an identifier before the parenthesis that's not a control keyword
                if i > 0 {
                    let prev_token = tokens[i - 1].to_string();
                    if !matches!(prev_token.as_str(), "if" | "while" | "for" | "switch")
                        && is_identifier(&prev_token)
                    {
                        report!(
                            "control_flow_handler",
                            "redirect_control_flow",
                            Info,
                            Report,
                            "Redirecting to function handler (function call)",
                            true
                        );
                        return Ok(HandlerResult::Redirected(
                            Some(tokens.to_vec()),
                            0..1,
                            "control_flow_handler".to_string(),
                            id,
                            Id::get("function_handler"),
                        ));
                    }
                }
            }
        }
    }

    // Check if this contains variable declarations
    if tokens.iter().any(|t| t.to_string() == "=")
        && !tokens.iter().any(|t| {
        matches!(
                t.to_string().as_str(),
                "if" | "while" | "for" | "switch" | "do"
            )
    })
    {
        report!(
            "control_flow_handler",
            "redirect_control_flow",
            Info,
            Report,
            "Redirecting to global handler (variable declaration)",
            true
        );
        return Ok(HandlerResult::Redirected(
            Some(tokens.to_vec()),
            0..1,
            "control_flow_handler".to_string(),
            id,
            Id::get("global_handler"),
        ));
    }

    // No redirection needed
    Ok(result)
}

/// Result callback: Postprocesses generated control flow code, adds documentation, and enhances formatting
fn result_control_flow(
    tokens: &[Token],
    result: HandlerResult) -> Result<HandlerResult, ConversionError> {
    let _id = Id::get("result_control_flow");

    report!(
        "control_flow_handler",
        "result_control_flow",
        Info,
        Report,
        "Postprocessing control flow conversion result",
        true
    );

    match result {
        HandlerResult::Completed(tokens_opt, _, rust_code, id) => {
            // Extract control flow information for documentation
            let control_flow_info = extract_control_flow_info_from_tokens(tokens);

            // Generate documentation about the control flow conversion
            let doc_comment =
                generate_control_flow_documentation(tokens, &control_flow_info);

            // Enhance the Rust code with documentation and metadata
            let mut enhanced_code = String::new();

            // Add metadata comment for traceability
            let metadata_comment = format!(
                "// [C2R] Control flow converted from C to Rust - {}: {}\n",
                control_flow_info.flow_type, control_flow_info.description
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
                "control_flow_handler",
                "result_control_flow",
                Info,
                Report,
                &format!(
                    "Enhanced control flow conversion: {} - {}",
                    control_flow_info.flow_type, control_flow_info.description
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
            let control_flow_info = extract_control_flow_info_from_tokens(tokens);
            let doc_comment =
                generate_control_flow_documentation(tokens, &control_flow_info);

            let mut enhanced_code = String::new();
            let metadata_comment = format!(
                "// [C2R] Control flow converted from C to Rust - {}: {}\n",
                control_flow_info.flow_type, control_flow_info.description
            );
            enhanced_code.push_str(&metadata_comment);

            if !doc_comment.trim().is_empty() {
                enhanced_code.push_str(&doc_comment);
                enhanced_code.push('\n');
            }
            enhanced_code.push_str(&rust_code);

            report!(
                "control_flow_handler",
                "result_control_flow",
                Info,
                Report,
                &format!(
                    "Enhanced converted control flow: {} - {}",
                    control_flow_info.flow_type, control_flow_info.description
                ),
                true
            );

            // CRITICAL: Return HandlerResult::Converted to preserve the code!
            Ok(HandlerResult::Converted(element, 0..1, enhanced_code, id))
        }
        HandlerResult::Extracted(element, _, rust_code, id) => {
            // Handle extracted elements - enhance the code and preserve the variant
            let control_flow_info = extract_control_flow_info_from_tokens(tokens);
            let doc_comment =
                generate_control_flow_documentation(tokens, &control_flow_info);

            let mut enhanced_code = String::new();
            let metadata_comment = format!(
                "// [C2R] Control flow converted from C to Rust - {}: {}\n",
                control_flow_info.flow_type, control_flow_info.description
            );
            enhanced_code.push_str(&metadata_comment);

            if !doc_comment.trim().is_empty() {
                enhanced_code.push_str(&doc_comment);
                enhanced_code.push('\n');
            }
            enhanced_code.push_str(&rust_code);

            report!(
                "control_flow_handler",
                "result_control_flow",
                Info,
                Report,
                &format!(
                    "Enhanced extracted control flow: {} - {}",
                    control_flow_info.flow_type, control_flow_info.description
                ),
                true
            );

            // CRITICAL: Return HandlerResult::Extracted to preserve the code!
            Ok(HandlerResult::Extracted(element, 0..1, enhanced_code, id))
        }
        HandlerResult::Handled(Some(converted_tokens), _, handler_id) => {
            // Legacy support for token-based results
            // Extract control flow information for documentation
            let control_flow_info = extract_control_flow_info_from_tokens(tokens);

            // Generate documentation about the control flow conversion
            let doc_comment =
                generate_control_flow_documentation(tokens, &control_flow_info);

            // Postprocess the converted Rust code for better formatting
            let mut enhanced_result = postprocess_control_flow_code(converted_tokens);

            // Add the documentation comment before the converted control flow
            if !doc_comment.is_empty() {
                enhanced_result.insert(0, Token::s(doc_comment));
            }

            // Add metadata comment for traceability
            let metadata_comment = format!(
                "// [C2R] Control flow converted from C to Rust - {}: {}",
                control_flow_info.flow_type, control_flow_info.description
            );
            enhanced_result.insert(0, Token::s(metadata_comment));

            report!(
                "control_flow_handler",
                "result_control_flow",
                Info,
                Report,
                &format!(
                    "Enhanced control flow conversion: {} - {}",
                    control_flow_info.flow_type, control_flow_info.description
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
                "control_flow_handler",
                "result_control_flow",
                Warning,
                Report,
                "Control flow handler returned empty result",
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

/// Control flow information extracted for documentation purposes
#[derive(Debug, Clone)]
struct ControlFlowInfo {
    flow_type: String,
    description: String,
    has_condition: bool,
    has_body: bool,
    has_else_clause: bool,
    is_infinite_loop: bool,
    is_range_based: bool,
    complexity: String,
}

/// Extracts control flow information from the original tokens for documentation purposes
fn extract_control_flow_info_from_tokens(tokens: &[Token]) -> ControlFlowInfo {
    if tokens.is_empty() {
        return ControlFlowInfo {
            flow_type: "unknown".to_string(),
            description: "empty token sequence".to_string(),
            has_condition: false,
            has_body: false,
            has_else_clause: false,
            is_infinite_loop: false,
            is_range_based: false,
            complexity: "none".to_string(),
        };
    }

    let first_token = tokens[0].to_string();
    let mut info = ControlFlowInfo {
        flow_type: first_token.clone(),
        description: String::new(),
        has_condition: false,
        has_body: false,
        has_else_clause: false,
        is_infinite_loop: false,
        is_range_based: false,
        complexity: "simple".to_string(),
    };

    // Analyze different control flow types
    match first_token.as_str() {
        "if" => {
            info.description = "conditional statement".to_string();
            info.has_condition = tokens.iter().any(|t| t.to_string() == "(");
            info.has_body = tokens.iter().any(|t| t.to_string() == "{");
            info.has_else_clause = tokens.iter().any(|t| t.to_string() == "else");

            // Determine complexity
            if info.has_else_clause {
                let else_if_count = tokens
                    .windows(2)
                    .filter(|w| w[0].to_string() == "else" && w[1].to_string() == "if")
                    .count();
                if else_if_count > 0 {
                    info.complexity = format!("complex ({} else-if branches)", else_if_count);
                } else {
                    info.complexity = "moderate (if-else)".to_string();
                }
            }
        }
        "for" => {
            info.description = "iteration loop".to_string();
            info.has_condition = tokens.iter().any(|t| t.to_string() == "(");
            info.has_body = tokens.iter().any(|t| t.to_string() == "{");

            // Check if it might be convertible to range-based
            if let Some(paren_start) = tokens.iter().position(|t| t.to_string() == "(") {
                if let Some(paren_end) = find_matching_token(&tokens[paren_start..], "(", ")") {
                    let header_tokens = &tokens[paren_start + 1..paren_start + paren_end];
                    let semicolon_count = header_tokens
                        .iter()
                        .filter(|t| t.to_string() == ";")
                        .count();
                    if semicolon_count == 2 {
                        // Standard C for loop
                        let header_str = tokens_to_string(header_tokens).unwrap_or_default();
                        if header_str.contains("++") || header_str.contains("+=") {
                            info.is_range_based = true;
                            info.description = "range-based iteration loop".to_string();
                        }
                    }
                }
            }

            info.complexity = if info.is_range_based {
                "simple (range-based)"
            } else {
                "moderate (C-style)"
            }
                .to_string();
        }
        "while" => {
            info.description = "conditional loop".to_string();
            info.has_condition = tokens.iter().any(|t| t.to_string() == "(");
            info.has_body = tokens.iter().any(|t| t.to_string() == "{");

            // Check for infinite loop
            if let Some(paren_start) = tokens.iter().position(|t| t.to_string() == "(") {
                if let Some(paren_end) = find_matching_token(&tokens[paren_start..], "(", ")") {
                    let condition_tokens = &tokens[paren_start + 1..paren_start + paren_end];
                    if condition_tokens.len() == 1 {
                        let cond_str = condition_tokens[0].to_string();
                        if cond_str == "true" || cond_str == "1" {
                            info.is_infinite_loop = true;
                            info.description = "infinite loop".to_string();
                            info.complexity = "infinite".to_string();
                        }
                    }
                }
            }
        }
        "do" => {
            info.description = "post-condition loop".to_string();
            info.has_condition = tokens.iter().any(|t| t.to_string() == "while");
            info.has_body = tokens.iter().any(|t| t.to_string() == "{");
            info.complexity = "moderate (do-while)".to_string();
        }
        "switch" => {
            info.description = "multi-branch selection".to_string();
            info.has_condition = tokens.iter().any(|t| t.to_string() == "(");
            info.has_body = tokens.iter().any(|t| t.to_string() == "{");

            // Count case statements
            let case_count = tokens.iter().filter(|t| t.to_string() == "case").count();
            let has_default = tokens.iter().any(|t| t.to_string() == "default");

            if case_count > 0 {
                info.complexity = format!(
                    "complex ({} cases{})",
                    case_count,
                    if has_default { ", default" } else { "" }
                );
                info.description = format!("multi-branch selection ({} cases)", case_count);
            }
        }
        _ => {
            info.description = "unknown control structure".to_string();
        }
    }

    info
}

/// Generates documentation comments for the control flow conversion
fn generate_control_flow_documentation(
    tokens: &[Token],
    flow_info: &ControlFlowInfo) -> String {
    let mut doc_lines = Vec::new();

    // Add main documentation header
    doc_lines.push("/**".to_string());
    doc_lines.push(" * Control Flow Conversion Documentation".to_string());
    doc_lines.push(" *".to_string());

    // Add control flow information
    doc_lines.push(format!(
        " * Type: {} ({})",
        flow_info.flow_type, flow_info.description
    ));
    doc_lines.push(format!(" * Complexity: {}", flow_info.complexity));
    doc_lines.push(format!(" * Original tokens: {}", tokens.len()));

    // Add structural information
    let mut features = Vec::new();
    if flow_info.has_condition {
        features.push("conditional");
    }
    if flow_info.has_body {
        features.push("body block");
    }
    if flow_info.has_else_clause {
        features.push("else clause");
    }
    if flow_info.is_infinite_loop {
        features.push("infinite loop");
    }
    if flow_info.is_range_based {
        features.push("range-based");
    }

    if !features.is_empty() {
        doc_lines.push(format!(" * Features: {}", features.join(", ")));
    }

    doc_lines.push(" *".to_string());

    // Add conversion notes based on control flow type
    match flow_info.flow_type.as_str() {
        "if" => {
            doc_lines.push(" * Conversion: C if statement -> Rust if expression".to_string());
            doc_lines.push(" * Features:".to_string());
            doc_lines.push(" *   - Conditions must be explicitly boolean in Rust".to_string());
            doc_lines.push(" *   - Braces are required for multi-statement blocks".to_string());
            doc_lines.push(" *   - Can be used as expressions (return values)".to_string());
            if flow_info.has_else_clause {
                doc_lines.push(
                    " *   - Else-if chains converted to match patterns where appropriate"
                        .to_string(),
                );
            }
        }
        "for" => {
            if flow_info.is_range_based {
                doc_lines
                    .push(" * Conversion: C for loop -> Rust range-based for loop".to_string());
                doc_lines.push(" * Features:".to_string());
                doc_lines
                    .push(" *   - Cleaner syntax with range operators (.. or ..=)".to_string());
                doc_lines.push(
                    " *   - Iterator-based implementation for better performance".to_string(),
                );
                doc_lines.push(" *   - Automatic bounds checking".to_string());
                doc_lines.push(" *   - No manual index management".to_string());
            } else {
                doc_lines.push(
                    " * Conversion: C for loop -> Rust while loop with initialization".to_string(),
                );
                doc_lines.push(" * Features:".to_string());
                doc_lines
                    .push(" *   - Preserves C semantics with explicit initialization".to_string());
                doc_lines.push(" *   - Manual increment placement at loop end".to_string());
                doc_lines.push(" *   - Consider refactoring to iterator patterns".to_string());
            }
        }
        "while" => {
            if flow_info.is_infinite_loop {
                doc_lines.push(" * Conversion: C while(true) -> Rust loop".to_string());
                doc_lines.push(" * Features:".to_string());
                doc_lines.push(" *   - More idiomatic Rust for infinite loops".to_string());
                doc_lines.push(" *   - Use break/continue for control flow".to_string());
                doc_lines.push(" *   - Can return values with break <value>".to_string());
            } else {
                doc_lines.push(" * Conversion: C while loop -> Rust while loop".to_string());
                doc_lines.push(" * Features:".to_string());
                doc_lines.push(" *   - Direct translation with boolean conditions".to_string());
                doc_lines.push(" *   - Conditions must evaluate to bool explicitly".to_string());
            }
        }
        "do" => {
            doc_lines
                .push(" * Conversion: C do-while -> Rust loop with conditional break".to_string());
            doc_lines.push(" * Features:".to_string());
            doc_lines.push(" *   - Guarantees at least one execution".to_string());
            doc_lines.push(" *   - Uses loop + conditional break pattern".to_string());
            doc_lines.push(" *   - More explicit than C do-while semantics".to_string());
        }
        "switch" => {
            doc_lines
                .push(" * Conversion: C switch statement -> Rust match expression".to_string());
            doc_lines.push(" * Features:".to_string());
            doc_lines.push(" *   - Exhaustive pattern matching".to_string());
            doc_lines.push(" *   - No fall-through by default (safer)".to_string());
            doc_lines.push(" *   - Can be used as expressions".to_string());
            doc_lines.push(" *   - Compile-time completeness checking".to_string());
        }
        _ => {
            doc_lines.push(
                " * Note: Unknown control flow type, conversion may be incomplete".to_string(),
            );
        }
    }

    doc_lines.push(" *".to_string());
    doc_lines.push(" * Rust control flow benefits:".to_string());
    doc_lines.push(" *   - Memory safety with no undefined behavior".to_string());
    doc_lines.push(" *   - Expression-based syntax enables functional patterns".to_string());
    doc_lines.push(" *   - Compile-time exhaustiveness checking".to_string());
    doc_lines.push(" *   - Zero-cost abstractions with iterator patterns".to_string());
    doc_lines.push(" */".to_string());

    report!(
        "control_flow_handler",
        "generate_control_flow_documentation",
        Info,
        Report,
        &format!(
            "Generated documentation for {} control flow: {}",
            flow_info.flow_type, flow_info.description
        ),
        true
    );

    doc_lines.join("\n")
}

/// Postprocesses the converted control flow code for better formatting
fn postprocess_control_flow_code(mut tokens: Vec<Token>) -> Vec<Token> {
    let original_count = tokens.len();

    // Clean up and format the converted control flow tokens
    for token in tokens.iter_mut() {
        let mut content = token.to_string();

        // Clean up extra whitespace
        content = content.trim().to_string();

        // Format control flow keywords properly
        if matches!(
            content.as_str(),
            "if" | "else" | "while" | "for" | "loop" | "match"
        ) {
            // Ensure proper spacing after keywords
            content = format!("{} ", content.trim_end());
        }

        // Format braces consistently
        if content == "{" {
            content = " {".to_string();
        } else if content == "}" {
            content = "}".to_string();
        }

        // Format conditions in parentheses
        if content.starts_with('(') && content.ends_with(')') && content.len() > 2 {
            let inner = &content[1..content.len() - 1];
            content = format!("({})", inner.trim());
        }

        // Format range operators properly
        content = content.replace("..", " .. ");
        content = content.replace("..=", " ..= ");
        content = content.replace(" .. =", " ..=");
        content = content.replace("  ..", " ..");
        content = content.replace("..  ", ".. ");

        // Format match arms
        if content.contains(" => ") {
            content = content.replace(" => ", " => ");
            // Clean up extra spaces
            content = content.replace("  =>", " =>");
            content = content.replace("=>  ", "=> ");
        }

        // Format loop control statements
        content = content.replace("break ;", "break;");
        content = content.replace("continue ;", "continue;");

        // Clean up multiple spaces
        while content.contains("  ") {
            content = content.replace("  ", " ");
        }

        *token = Token::s(content);
    }

    // Remove any empty tokens
    tokens.retain(|token| !token.to_string().trim().is_empty());

    report!(
        "control_flow_handler",
        "postprocess_control_flow_code",
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

/// Report callback: Collects and summarizes all reports from the context for this handler
fn report_control_flow(
    _tokens: &[Token]) -> Result<HandlerReport, ConversionError> {
    let context = context!();
    let handler_reports = context.get_reports_by_handler("control_flow_handler");

    // Count reports by level
    let info_count = handler_reports
        .iter()
        .filter(|r| matches!(r.level, ReportLevel::Info))
        .count();
    let error_count = handler_reports
        .iter()
        .filter(|r| matches!(r.level, ReportLevel::Error))
        .count();
    let warning_count = handler_reports
        .iter()
        .filter(|r| matches!(r.level, ReportLevel::Warning))
        .count();

    // Create summary message
    let summary_message = format!(
        "Control Flow Handler Summary: {} total reports (Info: {}, Warnings: {}, Errors: {})",
        handler_reports.len(),
        info_count,
        warning_count,
        error_count
    );

    // Create consolidated report using gen_name function for unique report_id
    let summary_report = HandlerReport {
        report_id: Box::new(Id::get(&crate::Id::gen_name(
            "control_flow_handler_summary",
        ))),
        handler_id: Box::new(Id::get("control_flow_handler")),
        handler_name: "control_flow_handler".to_string(),
        function_name: "report_control_flow".to_string(),
        level: ReportLevel::Info,
        phase: HandlerPhase::Report,
        message: summary_message,
        success: error_count == 0, // Success if no errors
        tokens_processed: handler_reports.iter().map(|r| r.tokens_processed).sum(),
        tokens_consumed: handler_reports.iter().map(|r| r.tokens_consumed).sum(),
        metadata: std::collections::HashMap::new(),
    };

    Ok(summary_report)
}
