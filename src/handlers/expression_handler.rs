use super::common::{not_handled, replace_with};
use crate::context::{Context, HandlerReport, ReportLevel, HandlerPhase};
use crate::context::ReportLevel::{Info, Error, Warning, Debug};
use crate::context::HandlerPhase::{Process, Handle, Extract, Convert, Report};
use crate::report;
use crate::debug;
use crate::error::ConversionError;
use crate::extract::{ExtractedElement, ExtractedExpression};
use crate::handler::HandlerResult;
use crate::{get_id, Token};
use crate::{ConvertedElement, ConvertedExpression};

/// Creates an expression handler that can detect and convert C expressions
pub fn create_expression_handler() -> crate::handler::Handler {
    let handler_id = get_id("expression_handler");
    let handler_role = "expression";
    let priority = 150; // Lower than control flow handlers
    
    super::create_handler(
        handler_id,
        handler_role,
        priority,
        Some(process_expression),
        Some(handle_expression),
        Some(extract_expression),
        Some(convert_expression),
        None,
        None,
        Some(redirect_expression),
    )
}

/// Determines if the token sequence represents a standalone expression
fn process_expression(tokens: &[Token], context: &mut Context) -> Result<bool, ConversionError> {
    let _id = get_id("process_expression");
    if tokens.len() < 3 {
        return Ok(false);
    }
    
    // Check for various expression patterns
    
    // Binary expression: left op right
    if is_binary_expression(tokens) {
        return Ok(true);
    }
    
    // Assignment expression: left = right
    if is_assignment_expression(tokens) {
        return Ok(true);
    }
    
    // Ternary expression: cond ? true_val : false_val
    if is_ternary_expression(tokens) {
        return Ok(true);
    }
    
    Ok(false)
}

/// Process expressions
fn handle_expression(tokens: &[Token], context: &mut Context) -> Result<HandlerResult, ConversionError> {
    let _id = get_id("handle_expression");
    report!(context, "expression_handler", Info, Handle, 
        "Expression handler processing tokens", true);
    
    // Try to handle as binary expression
    if is_binary_expression(tokens) {
        return handle_binary_expression(tokens, context);
    }
    
    // Try to handle as assignment expression
    if is_assignment_expression(tokens) {
        return handle_assignment_expression(tokens, context);
    }
    
    // Try to handle as ternary expression
    if is_ternary_expression(tokens) {
        return handle_ternary_expression(tokens, context);
    }
    
    not_handled()
}

/// Check if tokens represent a binary expression
fn is_binary_expression(tokens: &[Token]) -> bool {
    if tokens.len() < 3 {
        return false;
    }
    
    // Simple check for operator in the middle
    for i in 1..tokens.len() - 1 {
        let token_str = tokens[i].to_string();
        if is_binary_operator(&token_str) {
            return true;
        }
    }
    
    false
}

/// Process binary expressions
fn handle_binary_expression(tokens: &[Token], context: &mut Context) -> Result<HandlerResult, ConversionError> {
    report!(context, "expression_handler", Info, Process, 
        "Processing binary expression", true);
    
    // Find operator position
    let mut op_pos = 0;
    let mut op_str = String::new();
    
    for i in 1..tokens.len() - 1 {
        let token_str = tokens[i].to_string();
        if is_binary_operator(&token_str) {
            op_pos = i;
            op_str = token_str;
            break;
        }
    }
    
    if op_pos == 0 {
        return not_handled();
    }
    
    // Extract left and right operands
    let left_tokens = &tokens[0..op_pos];
    let right_tokens = &tokens[op_pos + 1..];
    
    // Convert operands to strings
    let left_str = tokens_to_string(left_tokens)?;
    let right_str = tokens_to_string(right_tokens)?;
    
    // Convert operator if needed
    let rust_op = convert_operator(&op_str);
    
    // Build Rust expression
    let rust_code = format!("{} {} {}", left_str, rust_op, right_str);
    
    let id = get_id("binary_expression");
    replace_with(rust_code, id)
}

/// Check if tokens represent an assignment expression
fn is_assignment_expression(tokens: &[Token]) -> bool {
    if tokens.len() < 3 {
        return false;
    }
    
    // Simple check for assignment operator
    for i in 1..tokens.len() - 1 {
        let token_str = tokens[i].to_string();
        if is_assignment_operator(&token_str) {
            return true;
        }
    }
    
    false
}

/// Process assignment expressions
fn handle_assignment_expression(tokens: &[Token], context: &mut Context) -> Result<HandlerResult, ConversionError> {
    report!(context, "expression_handler", Info, Process, 
        "Processing assignment expression", true);
    
    // Find operator position
    let mut op_pos = 0;
    let mut op_str = String::new();
    
    for i in 1..tokens.len() - 1 {
        let token_str = tokens[i].to_string();
        if is_assignment_operator(&token_str) {
            op_pos = i;
            op_str = token_str;
            break;
        }
    }
    
    if op_pos == 0 {
        return not_handled();
    }
    
    // Extract left and right operands
    let left_tokens = &tokens[0..op_pos];
    let right_tokens = &tokens[op_pos + 1..];
    
    // Convert operands to strings
    let left_str = tokens_to_string(left_tokens)?;
    let right_str = tokens_to_string(right_tokens)?;
    
    // Build Rust expression
    let rust_code = format!("{} {} {}", left_str, op_str, right_str);
    
    let id = get_id("assignment_expression");
    replace_with(rust_code, id)
}

/// Check if tokens represent a ternary expression
fn is_ternary_expression(tokens: &[Token]) -> bool {
    if tokens.len() < 5 {
        return false;
    }
    
    // Look for ? and : pattern
    for i in 1..tokens.len() - 3 {
        if tokens[i].to_string() == "?" {
            // Look for the matching colon
            for j in i + 2..tokens.len() {
                if tokens[j].to_string() == ":" {
                    return true;
                }
            }
        }
    }
    
    false
}

/// Process ternary expressions
fn handle_ternary_expression(tokens: &[Token], context: &mut Context) -> Result<HandlerResult, ConversionError> {
    report!(context, "expression_handler", Info, Process, 
        "Processing ternary expression", true);
    
    // Find ? and : positions
    let mut q_pos = 0;
    let mut c_pos = 0;
    
    for i in 1..tokens.len() - 1 {
        if tokens[i].to_string() == "?" {
            q_pos = i;
            // Find the matching colon
            for j in i + 2..tokens.len() {
                if tokens[j].to_string() == ":" {
                    c_pos = j;
                    break;
                }
            }
            break;
        }
    }
    
    if q_pos == 0 || c_pos == 0 || c_pos <= q_pos + 1 {
        return not_handled();
    }
    
    // Extract condition, true value, and false value
    let cond_tokens = &tokens[0..q_pos];
    let true_tokens = &tokens[q_pos + 1..c_pos];
    let false_tokens = &tokens[c_pos + 1..];
    
    // Convert to strings
    let cond_str = tokens_to_string(cond_tokens)?;
    let true_str = tokens_to_string(true_tokens)?;
    let false_str = tokens_to_string(false_tokens)?;
    
    // Build Rust if-else expression
    let rust_code = format!("if {} {{ {} }} else {{ {} }}", cond_str, true_str, false_str);
    
    let id = get_id("ternary_expression");
    replace_with(rust_code, id)
}

/// Check if a token string represents a binary operator
fn is_binary_operator(token: &str) -> bool {
    match token {
        "+" | "-" | "*" | "/" | "%" | "&" | "|" | "^" | "<<" | ">>" |
        "==" | "!=" | "<" | ">" | "<=" | ">=" | "&&" | "||" => true,
        _ => false,
    }
}

/// Check if a token string represents an assignment operator
fn is_assignment_operator(token: &str) -> bool {
    match token {
        "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>=" => true,
        _ => false,
    }
}

/// Convert a C operator to its Rust equivalent
fn convert_operator(op: &str) -> String {
    match op {
        // Most operators are the same in Rust
        "+" | "-" | "*" | "/" | "%" | "&" | "|" | "^" | "<<" | ">>" |
        "==" | "!=" | "<" | ">" | "<=" | ">=" | "&&" | "||" |
        "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>=" => op.to_string(),
        // Some C operators need translation
        "!" => "!".to_string(),
        "~" => "!".to_string(),  // Bitwise NOT is different in Rust
        _ => op.to_string(),
    }
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
    is_binary_operator(token) || is_assignment_operator(token) ||
    token == "!" || token == "~" || token == "?" || token == ":"
}

/// Extract expression information from tokens
fn extract_expression(tokens: &[Token], context: &mut Context) -> Result<Option<ExtractedElement>, ConversionError> {
    let _id = get_id("extract_expression");
    report!(context, "expression_handler", Info, Extract, 
        "Extracting expression from tokens", true);
    
    if tokens.len() < 3 {
        return Ok(None);
    }
    
    // Determine expression type and extract components
    if is_ternary_expression(tokens) {
        return extract_ternary_expression(tokens, context);
    }
    
    if is_assignment_expression(tokens) {
        return extract_assignment_expression(tokens, context);
    }
    
    if is_binary_expression(tokens) {
        return extract_binary_expression(tokens, context);
    }
    
    Ok(None)
}

/// Extract ternary expression information
fn extract_ternary_expression(tokens: &[Token], context: &mut Context) -> Result<Option<ExtractedElement>, ConversionError> {
    report!(context, "expression_handler", Info, Extract, 
        "Extracting ternary expression", true);
    
    // Find ? and : positions
    let mut q_pos = 0;
    let mut c_pos = 0;
    
    for i in 1..tokens.len() - 1 {
        if tokens[i].to_string() == "?" {
            q_pos = i;
            for j in i + 2..tokens.len() {
                if tokens[j].to_string() == ":" {
                    c_pos = j;
                    break;
                }
            }
            break;
        }
    }
    
    if q_pos == 0 || c_pos == 0 || c_pos <= q_pos + 1 {
        return Ok(None);
    }
    
    // Extract components
    let condition = tokens_to_string(&tokens[0..q_pos])?;
    let true_expr = tokens_to_string(&tokens[q_pos + 1..c_pos])?;
    let false_expr = tokens_to_string(&tokens[c_pos + 1..])?;
    
    let extracted = ExtractedExpression {
        expression_type: "ternary".to_string(),
        left_operand: condition,
        operator: "?:".to_string(),
        right_operand: format!("{} : {}", true_expr, false_expr),
        result_type: None,
    };
    
    Ok(Some(ExtractedElement::Expression(extracted)))
}

/// Extract assignment expression information  
fn extract_assignment_expression(tokens: &[Token], context: &mut Context) -> Result<Option<ExtractedElement>, ConversionError> {
    report!(context, "expression_handler", Info, Extract, 
        "Extracting assignment expression", true);
    
    // Find operator position
    let mut op_pos = 0;
    let mut op_str = String::new();
    
    for i in 1..tokens.len() - 1 {
        let token_str = tokens[i].to_string();
        if is_assignment_operator(&token_str) {
            op_pos = i;
            op_str = token_str;
            break;
        }
    }
    
    if op_pos == 0 {
        return Ok(None);
    }
    
    // Extract operands
    let left = tokens_to_string(&tokens[0..op_pos])?;
    let right = tokens_to_string(&tokens[op_pos + 1..])?;
    
    let extracted = ExtractedExpression {
        expression_type: "assignment".to_string(),
        left_operand: left,
        operator: op_str,
        right_operand: right,
        result_type: None,
    };
    
    Ok(Some(ExtractedElement::Expression(extracted)))
}

/// Extract binary expression information
fn extract_binary_expression(tokens: &[Token], context: &mut Context) -> Result<Option<ExtractedElement>, ConversionError> {
    report!(context, "expression_handler", Info, Extract, 
        "Extracting binary expression", true);
    
    // Find operator position
    let mut op_pos = 0;
    let mut op_str = String::new();
    
    for i in 1..tokens.len() - 1 {
        let token_str = tokens[i].to_string();
        if is_binary_operator(&token_str) {
            op_pos = i;
            op_str = token_str;
            break;
        }
    }
    
    if op_pos == 0 {
        return Ok(None);
    }
    
    // Extract operands
    let left = tokens_to_string(&tokens[0..op_pos])?;
    let right = tokens_to_string(&tokens[op_pos + 1..])?;
    
    let extracted = ExtractedExpression {
        expression_type: "binary".to_string(),
        left_operand: left,
        operator: op_str,
        right_operand: right,
        result_type: None,
    };
    
    Ok(Some(ExtractedElement::Expression(extracted)))
}

/// Convert extracted expression to Rust code
fn convert_expression(tokens: &[Token], context: &mut Context) -> Result<Option<ConvertedElement>, ConversionError> {
    let _id = get_id("convert_expression");
    report!(context, "expression_handler", Info, Convert, 
        "Converting expression to Rust", true);
    
    // First extract the expression information from tokens
    if let Some(ExtractedElement::Expression(expr)) = extract_expression(tokens, context)? {
        let rust_code = match expr.expression_type.as_str() {
            "ternary" => {
                // Parse ternary operand: "true_expr : false_expr"
                let parts: Vec<&str> = expr.right_operand.split(" : ").collect();
                if parts.len() != 2 {
                    return Err(ConversionError::InvalidExtraction("Invalid ternary expression".to_string()));
                }
                format!("if {} {{ {} }} else {{ {} }}", expr.left_operand, parts[0], parts[1])
            },
            "assignment" => {
                format!("{} {} {}", expr.left_operand, expr.operator, expr.right_operand)
            },
            "binary" => {
                let rust_op = convert_operator(&expr.operator);
                format!("{} {} {}", expr.left_operand, rust_op, expr.right_operand)
            },
            _ => {
                format!("{} {} {}", expr.left_operand, expr.operator, expr.right_operand)
            }
        };
        
        let converted = ConvertedExpression {
            expression_type: expr.expression_type.clone(),
            left_operand: expr.left_operand.clone(),
            operator: expr.operator.clone(),
            right_operand: expr.right_operand.clone(),
            rust_code,
            result_type: expr.result_type.clone(),
        };
        
        Ok(Some(ConvertedElement::Expression(converted)))
    } else {
        Ok(None)
    }
}

/// Handle redirection for expression processing
fn redirect_expression(_tokens: &[Token], result: HandlerResult, context: &mut Context) -> Result<HandlerResult, ConversionError> {
    let _id = get_id("redirect_expression");
    report!(context, "expression_handler", Info, Report, 
        "Redirecting expression handler result", true);
    
    // Expressions could be redirected to more specialized handlers
    // For example, function calls could be redirected to function_handler
    // Array accesses could be redirected to array_handler
    // For now, just return the result as-is
    Ok(result)
}
