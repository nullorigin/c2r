#![allow(
    unused_variables,
    unused_assignments,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals
)]

use super::common::{not_handled, replace_with_range};
use crate::config::HandlerPhase::{Convert, Extract, Handle, Process, Report};
use crate::config::HandlerReport;
use crate::config::ReportLevel::{Info, Warning};
use crate::config::{HandlerPhase, ReportLevel};
use crate::error::ConversionError;
use crate::extract::{ExtractedElement, ExtractedExpression};
use crate::handler::HandlerResult;
use crate::lock::Id;
use crate::Token;
use crate::{context, report};
use crate::{ConvertedElement, ConvertedExpression};

/// Creates an expression handler that can detect and convert C expressions
pub fn create_expression_handler() -> crate::handler::Handler {
    let handler_id = Id::get("expression_handler");
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
        Some(report_expression),
        Some(result_expression),
        Some(redirect_expression),
    )
}

/// Determines if the token sequence represents a standalone expression
fn process_expression(tokens: &[Token]) -> Result<bool, ConversionError> {
    let _id = Id::get("process_expression");
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
fn handle_expression(
    tokens: &[Token]) -> Result<HandlerResult, ConversionError> {
    let id = Id::get("handle_expression");
    report!(
        "expression_handler",
        "handle_expression",
        Info,
        Handle,
        "Expression handler processing tokens",
        true
    );

    // Try to handle as binary expression
    if is_binary_expression(tokens) {
        return handle_binary_expression(tokens);
    }

    // Try to handle as assignment expression
    if is_assignment_expression(tokens) {
        return handle_assignment_expression(tokens);
    }

    // Try to handle as ternary expression
    if is_ternary_expression(tokens) {
        return handle_ternary_expression(tokens);
    }

    not_handled()
}

/// Check if tokens represent a binary expression
fn is_binary_expression(tokens: &[Token]) -> bool {
    if tokens.len() < 3 {
        return false;
    }

    // Check for function-like patterns: identifier '(' - REJECT these
    for i in 0..tokens.len().saturating_sub(1) {
        if tokens[i + 1].to_string() == "(" {
            // Pattern: something followed by '(' suggests function call
            return false;
        }
    }

    // Check for function call patterns with member access: identifier '.' identifier '(' - REJECT these
    for i in 0..tokens.len().saturating_sub(3) {
        if tokens[i + 1].to_string() == "." && tokens[i + 3].to_string() == "(" {
            // Pattern: obj.method( suggests function call
            return false;
        }
    }

    // Check for variable declaration patterns: type identifier binary_op - REJECT these
    if tokens.len() >= 4 {
        for i in 0..tokens.len().saturating_sub(3) {
            let token1 = tokens[i].to_string();
            // Pattern: type_keyword ... binary_op (variable declaration)
            if (token1 == "enum" || token1 == "struct" || token1 == "int" || token1 == "char" ||
                token1 == "float" || token1 == "double" || token1 == "void" || token1 == "long" ||
                token1 == "short" || token1 == "unsigned" || token1 == "const" || token1 == "static") {
                // Skip patterns that look like variable declarations
                return false;
            }
        }
    }

    // Now check for binary operator in a standalone context
    for i in 1..tokens.len() - 1 {
        let token_str = tokens[i].to_string();
        if is_binary_operator(&token_str) {
            // Additional context check: ensure this looks like a standalone binary expression
            let prev_token = tokens[i - 1].to_string();
            let next_token = if i + 1 < tokens.len() { tokens[i + 1].to_string() } else { String::new() };

            // Skip if previous token looks like a type keyword
            if prev_token == "enum" || prev_token == "struct" || prev_token == "int" ||
                prev_token == "char" || prev_token == "float" || prev_token == "double" ||
                prev_token == "void" || prev_token == "long" || prev_token == "short" ||
                prev_token == "unsigned" || prev_token == "const" || prev_token == "static" {
                continue;
            }

            // Skip comma operators in what looks like function parameter lists
            if token_str == "," {
                // Look for surrounding parentheses suggesting function call context
                let mut paren_count = 0;
                for j in 0..i {
                    if tokens[j].to_string() == "(" {
                        paren_count += 1;
                    } else if tokens[j].to_string() == ")" {
                        paren_count -= 1;
                    }
                }
                if paren_count > 0 {
                    // We're inside parentheses, likely a function call parameter list
                    continue;
                }
            }

            // Skip member access operators in what looks like object access
            if token_str == "." {
                // Skip if this looks like object.member access
                if i > 0 && i + 1 < tokens.len() {
                    // Simple heuristic: if surrounded by identifiers, likely member access
                    continue;
                }
            }

            return true;
        }
    }

    false
}

/// Process binary expressions
fn handle_binary_expression(
    tokens: &[Token]) -> Result<HandlerResult, ConversionError> {
    report!(
        "expression_handler",
        "handle_binary_expression",
        Info,
        Process,
        "Processing binary expression",
        true
    );

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

    let id = Id::get("binary_expression");
    // Binary expressions process all input tokens
    let token_range = 0..tokens.len();
    replace_with_range(rust_code, token_range, id)
}

/// Check if tokens represent an assignment expression
fn is_assignment_expression(tokens: &[Token]) -> bool {
    if tokens.len() < 3 {
        return false;
    }

    // Check for function-like patterns: type identifier '(' - REJECT these
    for i in 0..tokens.len().saturating_sub(2) {
        if tokens[i + 1].to_string() == "(" {
            // Pattern: something followed by '(' suggests function signature
            return false;
        }
    }

    // Check for variable declaration patterns: type identifier '=' - REJECT these
    // Look for patterns like "enum Status current_status =" or "int x ="
    if tokens.len() >= 4 {
        for i in 0..tokens.len().saturating_sub(3) {
            let token1 = tokens[i].to_string();
            let token3 = tokens[i + 2].to_string();
            // Pattern: type_keyword ... = (variable declaration)
            if (token1 == "enum" || token1 == "struct" || token1 == "int" || token1 == "char" ||
                token1 == "float" || token1 == "double" || token1 == "void" || token1 == "long" ||
                token1 == "short" || token1 == "unsigned" || token1 == "const" || token1 == "static")
                && token3 == "=" {
                return false;
            }
        }
    }

    // Now check for assignment operator in a standalone context
    for i in 1..tokens.len() - 1 {
        let token_str = tokens[i].to_string();
        if is_assignment_operator(&token_str) {
            // Additional context check: ensure this looks like a standalone assignment
            // not part of a larger declaration or function signature
            let prev_token = tokens[i - 1].to_string();

            // Skip if previous token looks like a type keyword
            if prev_token == "enum" || prev_token == "struct" || prev_token == "int" ||
                prev_token == "char" || prev_token == "float" || prev_token == "double" ||
                prev_token == "void" || prev_token == "long" || prev_token == "short" ||
                prev_token == "unsigned" || prev_token == "const" || prev_token == "static" {
                continue;
            }

            return true;
        }
    }

    false
}

/// Process assignment expressions
fn handle_assignment_expression(
    tokens: &[Token]) -> Result<HandlerResult, ConversionError> {
    report!(
        "expression_handler",
        "handle_assignment_expression",
        Info,
        Process,
        "Processing assignment expression",
        true
    );

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

    let id = Id::get("assignment_expression");
    // Assignment expressions process all input tokens
    let token_range = 0..tokens.len();
    replace_with_range(rust_code, token_range, id)
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
fn handle_ternary_expression(
    tokens: &[Token]) -> Result<HandlerResult, ConversionError> {
    report!(
        "expression_handler",
        "handle_ternary_expression",
        Info,
        Process,
        "Processing ternary expression",
        true
    );

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
    let rust_code = format!(
        "if {} {{ {} }} else {{ {} }}",
        cond_str, true_str, false_str
    );

    let id = Id::get("ternary_expression");
    // Ternary expressions process all input tokens
    let token_range = 0..tokens.len();
    replace_with_range(rust_code, token_range, id)
}

/// Check if a token string represents a binary operator
fn is_binary_operator(token: &str) -> bool {
    match token {
        "+" | "-" | "*" | "/" | "%" | "&" | "|" | "^" | "<<" | ">>" | "==" | "!=" | "<" | ">"
        | "<=" | ">=" | "&&" | "||" => true,
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
        "+" | "-" | "*" | "/" | "%" | "&" | "|" | "^" | "<<" | ">>" | "==" | "!=" | "<" | ">"
        | "<=" | ">=" | "&&" | "||" | "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|="
        | "^=" | "<<=" | ">>=" => op.to_string(),
        // Some C operators need translation
        "!" => "!".to_string(),
        "~" => "!".to_string(), // Bitwise NOT is different in Rust
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
    is_binary_operator(token)
        || is_assignment_operator(token)
        || token == "!"
        || token == "~"
        || token == "?"
        || token == ":"
}

/// Extract expression information from tokens
fn extract_expression(
    tokens: &[Token]) -> Result<Option<ExtractedElement>, ConversionError> {
    let _id = Id::get("extract_expression");
    report!(
        "expression_handler",
        "extract_expression",
        Info,
        Extract,
        "Extracting expression from tokens",
        true
    );

    if tokens.len() < 3 {
        return Ok(None);
    }

    // Determine expression type and extract components
    if is_ternary_expression(tokens) {
        return extract_ternary_expression(tokens);
    }

    if is_assignment_expression(tokens) {
        return extract_assignment_expression(tokens);
    }

    if is_binary_expression(tokens) {
        return extract_binary_expression(tokens);
    }

    Ok(None)
}

/// Extract ternary expression information
fn extract_ternary_expression(
    tokens: &[Token]) -> Result<Option<ExtractedElement>, ConversionError> {
    report!(
        "expression_handler",
        "extract_ternary_expression",
        Info,
        Extract,
        "Extracting ternary expression",
        true
    );

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
fn extract_assignment_expression(
    tokens: &[Token]) -> Result<Option<ExtractedElement>, ConversionError> {
    report!(
        "expression_handler",
        "extract_assignment_expression",
        Info,
        Extract,
        "Extracting assignment expression",
        true
    );

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
fn extract_binary_expression(
    tokens: &[Token]) -> Result<Option<ExtractedElement>, ConversionError> {
    report!(
        "expression_handler",
        "extract_binary_expression",
        Info,
        Extract,
        "Extracting binary expression",
        true
    );

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
fn convert_expression(
    tokens: &[Token]) -> Result<Option<ConvertedElement>, ConversionError> {
    let _id = Id::get("convert_expression");
    report!(
        "expression_handler",
        "convert_expression",
        Info,
        Convert,
        "Converting expression to Rust",
        true
    );

    // First extract the expression information from tokens
    if let Some(ExtractedElement::Expression(expr)) = extract_expression(tokens)? {
        let rust_code = match expr.expression_type.as_str() {
            "ternary" => {
                // Parse ternary operand: "true_expr : false_expr"
                let parts: Vec<&str> = expr.right_operand.split(" : ").collect();
                if parts.len() != 2 {
                    return Err(ConversionError::InvalidExtraction(
                        "Invalid ternary expression".to_string(),
                    ));
                }
                format!(
                    "if {} {{ {} }} else {{ {} }}",
                    expr.left_operand, parts[0], parts[1]
                )
            }
            "assignment" => {
                format!(
                    "{} {} {}",
                    expr.left_operand, expr.operator, expr.right_operand
                )
            }
            "binary" => {
                let rust_op = convert_operator(&expr.operator);
                format!("{} {} {}", expr.left_operand, rust_op, expr.right_operand)
            }
            _ => {
                format!(
                    "{} {} {}",
                    expr.left_operand, expr.operator, expr.right_operand
                )
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
fn redirect_expression(
    _tokens: &[Token],
    result: HandlerResult) -> Result<HandlerResult, ConversionError> {
    let _id = Id::get("redirect_expression");
    report!(
        "expression_handler",
        "redirect_expression",
        Info,
        Report,
        "Redirecting expression handler result",
        true
    );

    // Expressions could be redirected to more specialized handlers
    // For example, function calls could be redirected to function_handler
    // Array accesses could be redirected to array_handler
    // For now, just return the result as-is
    Ok(result)
}

/// Result callback: Postprocesses generated expression code, adds documentation, and enhances formatting
fn result_expression(
    tokens: &[Token],
    result: HandlerResult) -> Result<HandlerResult, ConversionError> {
    let _id = Id::get("result_expression");

    report!(
        "expression_handler",
        "result_expression",
        Info,
        Report,
        "Postprocessing expression conversion result",
        true
    );

    match result {
        HandlerResult::Completed(tokens_opt, _, rust_code, id) => {
            // Extract expression information for documentation
            let (expr_info, expr_type) = extract_expression_info_from_tokens(tokens);

            // Generate documentation about the expression conversion
            let doc_comment =
                generate_expression_documentation(tokens, &expr_info, &expr_type);

            // Enhance the Rust code with documentation and metadata
            let mut enhanced_code = String::new();

            // Add metadata comment for traceability
            let metadata_comment = format!(
                "// [C2R] Expression converted from C to Rust - {}: {}\n",
                expr_type, expr_info
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
                "expression_handler",
                "result_expression",
                Info,
                Report,
                &format!(
                    "Enhanced expression conversion: {} - {}",
                    expr_type, expr_info
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
            let (expr_info, expr_type) = extract_expression_info_from_tokens(tokens);
            let doc_comment =
                generate_expression_documentation(tokens, &expr_info, &expr_type);

            let mut enhanced_code = String::new();
            let metadata_comment = format!(
                "// [C2R] Expression converted from C to Rust - {}: {}\n",
                expr_type, expr_info
            );
            enhanced_code.push_str(&metadata_comment);

            if !doc_comment.trim().is_empty() {
                enhanced_code.push_str(&doc_comment);
                enhanced_code.push('\n');
            }
            enhanced_code.push_str(&rust_code);

            report!(
                "expression_handler",
                "result_expression",
                Info,
                Report,
                &format!(
                    "Enhanced converted expression: {} - {}",
                    expr_type, expr_info
                ),
                true
            );

            // CRITICAL: Return HandlerResult::Converted to preserve the code!
            Ok(HandlerResult::Converted(element, 0..1, enhanced_code, id))
        }
        HandlerResult::Extracted(element, _, rust_code, id) => {
            // Handle extracted elements - enhance the code and preserve the variant
            let (expr_info, expr_type) = extract_expression_info_from_tokens(tokens);
            let doc_comment =
                generate_expression_documentation(tokens, &expr_info, &expr_type);

            let mut enhanced_code = String::new();
            let metadata_comment = format!(
                "// [C2R] Expression converted from C to Rust - {}: {}\n",
                expr_type, expr_info
            );
            enhanced_code.push_str(&metadata_comment);

            if !doc_comment.trim().is_empty() {
                enhanced_code.push_str(&doc_comment);
                enhanced_code.push('\n');
            }
            enhanced_code.push_str(&rust_code);

            report!(
                "expression_handler",
                "result_expression",
                Info,
                Report,
                &format!(
                    "Enhanced extracted expression: {} - {}",
                    expr_type, expr_info
                ),
                true
            );

            // CRITICAL: Return HandlerResult::Extracted to preserve the code!
            Ok(HandlerResult::Extracted(element, 0..1, enhanced_code, id))
        }
        HandlerResult::Handled(Some(converted_tokens), _, handler_id) => {
            // Legacy support for token-based results
            // Extract expression information for documentation
            let (expr_info, expr_type) = extract_expression_info_from_tokens(tokens);

            // Generate documentation about the expression conversion
            let doc_comment =
                generate_expression_documentation(tokens, &expr_info, &expr_type);

            // Postprocess the converted Rust code for better formatting
            let mut enhanced_result = postprocess_expression_code(converted_tokens);

            // Add the documentation comment before the converted expression
            if !doc_comment.is_empty() {
                enhanced_result.insert(0, Token::s(doc_comment));
            }

            // Add metadata comment for traceability
            let metadata_comment = format!(
                "// [C2R] Expression converted from C to Rust - {}: {}",
                expr_type, expr_info
            );
            enhanced_result.insert(0, Token::s(metadata_comment));

            report!(
                "expression_handler",
                "result_expression",
                Info,
                Report,
                &format!(
                    "Enhanced expression conversion: {} - {}",
                    expr_type, expr_info
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
                "expression_handler",
                "result_expression",
                Warning,
                Report,
                "Expression handler returned empty result",
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

/// Extracts expression information from the original tokens for documentation purposes
fn extract_expression_info_from_tokens(tokens: &[Token]) -> (String, String) {
    if tokens.is_empty() {
        return ("unknown expression".to_string(), "unknown".to_string());
    }

    // Determine expression type and extract information
    if is_ternary_expression(tokens) {
        // Extract ternary expression info: cond ? true_val : false_val
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

        if q_pos > 0 && c_pos > q_pos {
            let condition = tokens[0..q_pos]
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(" ");
            let true_val = tokens[q_pos + 1..c_pos]
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(" ");
            let false_val = tokens[c_pos + 1..]
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(" ");
            (
                format!("{} ? {} : {}", condition, true_val, false_val),
                "ternary".to_string(),
            )
        } else {
            ("malformed ternary".to_string(), "ternary".to_string())
        }
    } else if is_assignment_expression(tokens) {
        // Extract assignment expression info
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

        if op_pos > 0 {
            let left = tokens[0..op_pos]
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(" ");
            let right = tokens[op_pos + 1..]
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(" ");
            (
                format!("{} {} {}", left, op_str, right),
                "assignment".to_string(),
            )
        } else {
            ("malformed assignment".to_string(), "assignment".to_string())
        }
    } else if is_binary_expression(tokens) {
        // Extract binary expression info
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

        if op_pos > 0 {
            let left = tokens[0..op_pos]
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(" ");
            let right = tokens[op_pos + 1..]
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(" ");
            (
                format!("{} {} {}", left, op_str, right),
                "binary".to_string(),
            )
        } else {
            ("malformed binary".to_string(), "binary".to_string())
        }
    } else {
        // Unknown expression type
        let expr_str = tokens
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<_>>()
            .join(" ");
        (expr_str, "unknown".to_string())
    }
}

/// Generates documentation comments for the expression conversion
fn generate_expression_documentation(
    tokens: &[Token],
    expr_info: &str,
    expr_type: &str) -> String {
    let mut doc_lines = Vec::new();

    // Add main documentation header
    doc_lines.push("/**".to_string());
    doc_lines.push(" * Expression Conversion Documentation".to_string());
    doc_lines.push(" *".to_string());

    // Add expression information
    doc_lines.push(format!(" * Type: {} expression", expr_type));
    doc_lines.push(format!(" * Expression: {}", expr_info));

    // Add conversion details
    doc_lines.push(format!(" * Original tokens: {}", tokens.len()));
    doc_lines.push(" *".to_string());

    // Add conversion notes based on expression type
    match expr_type {
        "ternary" => {
            doc_lines.push(" * Conversion: C ternary operator (cond ? true : false) -> Rust if-else expression".to_string());
            doc_lines.push(" * Features:".to_string());
            doc_lines.push(" *   - More explicit than C ternary operator".to_string());
            doc_lines
                .push(" *   - Type checking ensures both branches return same type".to_string());
            doc_lines.push(" *   - Blocks allow multiple statements if needed".to_string());
        }
        "assignment" => {
            doc_lines.push(" * Conversion: C assignment expression -> Rust assignment".to_string());
            doc_lines.push(" * Features:".to_string());
            doc_lines.push(" *   - Same syntax as C for most operators".to_string());
            doc_lines.push(" *   - Rust enforces mutability requirements".to_string());
            doc_lines.push(" *   - Type checking prevents invalid assignments".to_string());
        }
        "binary" => {
            doc_lines
                .push(" * Conversion: C binary expression -> Rust binary expression".to_string());
            doc_lines.push(" * Features:".to_string());
            doc_lines.push(" *   - Most operators identical to C".to_string());
            doc_lines.push(" *   - Operator precedence preserved".to_string());
            doc_lines.push(" *   - Type checking prevents invalid operations".to_string());
            doc_lines.push(" *   - Some operators translated (~ -> !)".to_string());
        }
        _ => {
            doc_lines
                .push(" * Note: Unknown expression type, conversion may be incomplete".to_string());
        }
    }

    doc_lines.push(" */".to_string());

    report!(
        "expression_handler",
        "generate_expression_documentation",
        Info,
        Report,
        &format!(
            "Generated documentation for {} expression: {}",
            expr_type, expr_info
        ),
        true
    );

    doc_lines.join("\n")
}

/// Postprocesses the converted expression code for better formatting
fn postprocess_expression_code(mut tokens: Vec<Token>) -> Vec<Token> {
    let original_count = tokens.len();

    // Clean up and format the converted expression tokens
    for token in tokens.iter_mut() {
        let mut content = token.to_string();

        // Clean up extra whitespace
        content = content.trim().to_string();

        // Ensure proper spacing around operators
        content = content.replace('=', " = ");
        content = content.replace('+', " + ");
        content = content.replace('-', " - ");
        content = content.replace('*', " * ");
        content = content.replace('/', " / ");
        content = content.replace('%', " % ");
        content = content.replace('<', " < ");
        content = content.replace('>', " > ");

        // Handle compound operators properly
        content = content.replace(" = =", "==");
        content = content.replace("! =", "!=");
        content = content.replace(" < =", "<=");
        content = content.replace(" > =", ">=");
        content = content.replace("+ =", "+=");
        content = content.replace("- =", "-=");
        content = content.replace("* =", "*=");
        content = content.replace("/ =", "/=");
        content = content.replace("% =", "%=");
        content = content.replace("& &", "&&");
        content = content.replace("| |", "||");
        content = content.replace(" < <", "<<");
        content = content.replace(" > >", ">>");

        // Clean up multiple spaces
        while content.contains("  ") {
            content = content.replace("  ", " ");
        }

        // Ensure proper formatting for if-else expressions
        if content.contains("if ") && content.contains(" else ") {
            content = content.replace(" {", " {");
            content = content.replace("} ", "} ");
            content = content.replace("else{", "else {");
        }

        *token = Token::s(content.trim().to_string());
    }

    // Remove any empty tokens
    tokens.retain(|token| !token.to_string().trim().is_empty());

    report!(
        "expression_handler",
        "postprocess_expression_code",
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
fn report_expression(
    _tokens: &[Token]) -> Result<HandlerReport, ConversionError> {
    let context = context!();
    let handler_reports = context.get_reports_by_handler("expression_handler");

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
        "Expression Handler Summary: {} total reports (Info: {}, Warnings: {}, Errors: {})",
        handler_reports.len(),
        info_count,
        warning_count,
        error_count
    );

    // Create consolidated report using gen_name function for unique report_id
    let summary_report = HandlerReport {
        report_id: Box::new(Id::get(&Id::gen_name("expression_handler_summary"))),
        handler_id: Box::new(Id::get("expression_handler")),
        handler_name: "expression_handler".to_string(),
        function_name: "report_expression".to_string(),
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
