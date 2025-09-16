//! Expression handler for C to Rust expression conversion
//! Uses Patternizer exclusively for detection and conversion

use super::common::not_handled;
use crate::{
    C2RError, ConvertedElement, ConvertedExpression, ExtractedElement, ExtractedExpression,
    HandlerPhase, HandlerReport, HandlerResult, Id, Kind, Reason, ReportLevel, context,
    convert_operator, is_assignment_operator, is_binary_operator, pattern::PatternResult, report,
    token::Token,
};
use std::ops::Range;

/// Report callback: Collects expression handler reports
pub fn report_expression() -> Result<HandlerReport, C2RError> {
    let context = context!();
    let reports = context.get_reports_by_handler("expression_handler");

    let (info_count, warning_count, error_count) =
        reports
            .iter()
            .fold((0, 0, 0), |acc, report| match report.level {
                ReportLevel::Error => (acc.0, acc.1, acc.2 + 1),
                ReportLevel::Warning => (acc.0, acc.1 + 1, acc.2),
                _ => (acc.0 + 1, acc.1, acc.2),
            });

    Ok(HandlerReport {
        report_id: Box::new(Id::get(&Id::gen_name("expression_handler"))),
        handler_id: Box::new(Id::get("expression_handler")),
        handler_name: "expression_handler".to_string(),
        function_name: "report_expression".to_string(),
        message: format!(
            "Expression handler summary: {} reports ({} info, {} warnings, {} errors)",
            reports.len(),
            info_count,
            warning_count,
            error_count
        ),
        level: match (error_count, warning_count) {
            (0, 0) => ReportLevel::Info,
            (0, _) => ReportLevel::Warning,
            _ => ReportLevel::Error,
        },
        tokens_processed: reports.len(),
        tokens_consumed: 0,
        phase: HandlerPhase::Report,
        success: error_count == 0,
        metadata: std::collections::HashMap::new(),
    })
}

/// Handle callback: Sets up patternizer and registers expression patterns
fn handle_expression(token_range: Range<usize>) -> Result<HandlerResult, C2RError> {
    // Register expression patterns in the global registry via context
    let expression_patterns = [
        "expression_assignment",
        "expression_binary",
        "expression_unary",
        "expression_call",
        "expression_ternary",
        "expression",
    ];

    let mut context = crate::context!();
    context.pull();
    let tokens = &context.tokens[token_range.clone()];
    let filtered_tokens: Vec<Token> = tokens
        .iter()
        .filter(|token| !matches!(token, crate::Token::n()))
        .cloned()
        .collect();
    let tokens = &filtered_tokens;

    for pattern in &expression_patterns {
        match context.patternizer.match_pattern(pattern, tokens) {
            PatternResult::Match { consumed_tokens } => {
                report!(
                    "expression_handler",
                    "handle_expression",
                    ReportLevel::Info,
                    HandlerPhase::Handle,
                    format!(
                        "Processing {} expression with {} tokens",
                        pattern, consumed_tokens
                    ),
                    true
                );

                // Convert using pattern data
                match convert_expression(token_range.clone())? {
                    Some(ConvertedElement::Expression(converted_expression)) => {
                        let code = converted_expression.code.clone();
                        return Ok(HandlerResult::Converted(
                            ConvertedElement::Expression(converted_expression),
                            0..consumed_tokens,
                            code,
                            Id::get("expression_handler"),
                        ));
                    }
                    _ => {
                        report!(
                            "expression_handler",
                            "handle_expression",
                            ReportLevel::Error,
                            HandlerPhase::Handle,
                            "Failed to convert expression",
                            false
                        );
                        return not_handled();
                    }
                }
            }
            _ => continue,
        }
    }

    not_handled()
}

/// Process callback: Uses Patternizer to detect expression patterns
fn process_expression(token_range: Range<usize>) -> Result<bool, C2RError> {
    let mut context = crate::context!();
    context.pull();

    // Add bounds checking to prevent slice index out of bounds
    if token_range.end > context.tokens.len() || token_range.start >= context.tokens.len() {
        return Ok(false);
    }

    let tokens = &context.tokens[token_range.clone()];
    let filtered_tokens: Vec<Token> = tokens
        .iter()
        .filter(|token| !matches!(token, crate::Token::n()))
        .cloned()
        .collect();
    let tokens = &filtered_tokens;

    if tokens.len() < 2 {
        return Ok(false);
    }

    // Check for expression patterns
    let expression_patterns = [
        "expression_assignment",
        "expression_binary",
        "expression_unary",
        "expression_call",
        "expression_ternary",
        "expression",
    ];

    for pattern in &expression_patterns {
        if let PatternResult::Match { consumed_tokens: _ } =
            context.patternizer.match_pattern(pattern, tokens)
        {
            return Ok(true);
        }
    }

    // Fallback: check for expression indicators
    Ok(contains_operators(tokens) || contains_function_call(tokens))
}

/// Creates an expression handler that uses Patternizer exclusively
pub fn create_expression_handler() -> crate::handler::Handler {
    let handler_id = Id::get("expression_handler");
    let handler_role = "expression";
    let priority = 50; // Lower priority to avoid conflicts with function/control flow handlers

    super::create_handler(
        handler_id,
        handler_role,
        priority,
        Some(process_expression),
        Some(handle_expression),
        Some(extract_expression),
        Some(convert_expression),
        None, // No document callback
        Some(report_expression),
        None, // No result callback
        None, // No redirect callback
    )
}

/// Extract expression information from tokens using Patternizer exclusively
fn extract_expression(token_range: Range<usize>) -> Result<Option<ExtractedElement>, C2RError> {
    let mut context = crate::context!();
    context.pull();
    let tokens = &context.tokens[token_range.clone()];
    let filtered_tokens: Vec<Token> = tokens
        .iter()
        .filter(|token| !matches!(token, Token::n()))
        .cloned()
        .collect();
    let tokens = &filtered_tokens;

    // Try to match expression patterns
    let expression_patterns = [
        "expression_assignment",
        "expression_binary",
        "expression_unary",
        "expression_call",
        "expression_ternary",
        "expression",
    ];

    for pattern in &expression_patterns {
        match context.patternizer.match_pattern(pattern, tokens) {
            PatternResult::Match { consumed_tokens: _ } => {
                // Extract expression components
                let (expr_type, operator, left_operand, right_operand) =
                    parse_expression_tokens(tokens)?;

                let extracted_expression = ExtractedExpression {
                    expression_type: expr_type,
                    left_operand,
                    operator,
                    right_operand,
                    result_type: None,
                    code: tokens
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<_>>()
                        .join(" "),
                    tokens: tokens.to_vec(),
                };

                return Ok(Some(ExtractedElement::Expression(extracted_expression)));
            }
            _ => continue,
        }
    }

    // Fallback for basic expression detection
    if tokens.len() >= 2 {
        // Check for common expression patterns
        if contains_operators(tokens) || contains_function_call(tokens) {
            let (expr_type, operator, left_operand, right_operand) =
                parse_expression_tokens(tokens)?;

            let extracted_expression = ExtractedExpression {
                expression_type: expr_type,
                left_operand,
                operator,
                right_operand,
                result_type: None,
                code: tokens
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(" "),
                tokens: tokens.to_vec(),
            };

            return Ok(Some(ExtractedElement::Expression(extracted_expression)));
        }
    }

    Ok(None)
}

/// Helper function to check if tokens contain operators
fn contains_operators(tokens: &[Token]) -> bool {
    tokens.iter().any(|token| {
        let token_str = token.to_string();
        is_assignment_operator(&token_str)
            || is_binary_operator(&token_str)
            || token_str == "++"
            || token_str == "--"
            || token_str == "!"
            || token_str == "~"
            || token_str == "&"
            || token_str == "*"
    })
}

/// Helper function to check if tokens contain function call pattern
fn contains_function_call(tokens: &[Token]) -> bool {
    if tokens.len() >= 2 {
        for i in 0..tokens.len() - 1 {
            if tokens[i + 1].to_string() == "(" {
                return true;
            }
        }
    }
    false
}

/// Helper function to parse expression tokens
fn parse_expression_tokens(tokens: &[Token]) -> Result<(String, String, String, String), C2RError> {
    if tokens.is_empty() {
        return Err(C2RError::new(
            Kind::Other,
            Reason::Empty("expression token list"),
            None,
        ));
    }

    let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

    // Determine expression type and extract components
    let expr_type = if contains_function_call(tokens) {
        "call".to_string()
    } else if token_strings.iter().any(|s| is_assignment_operator(s)) {
        "assignment".to_string()
    } else if token_strings.iter().any(|s| is_binary_operator(s)) {
        "binary".to_string()
    } else if token_strings.contains(&"++".to_string()) || token_strings.contains(&"--".to_string())
    {
        "unary".to_string()
    } else if token_strings.contains(&"?".to_string()) && token_strings.contains(&":".to_string()) {
        "ternary".to_string()
    } else {
        "simple".to_string()
    };

    // Find operator if present
    let operator = token_strings
        .iter()
        .find(|s| {
            is_assignment_operator(s)
                || is_binary_operator(s)
                || *s == "++"
                || *s == "--"
                || *s == "!"
                || *s == "~"
                || *s == "?"
                || *s == ":"
        })
        .cloned()
        .unwrap_or_default();

    // Extract left and right operands
    let (left_operand, right_operand) =
        if let Some(op_pos) = token_strings.iter().position(|s| s == &operator) {
            let left = token_strings[..op_pos].join(" ");
            let right = token_strings[op_pos + 1..].join(" ");
            (left, right)
        } else {
            // No operator found, treat as single operand
            let operand = token_strings.join(" ");
            (operand, String::new())
        };

    Ok((expr_type, operator, left_operand, right_operand))
}

/// Convert extracted expression to Rust code using Patternizer exclusively
fn convert_expression(token_range: Range<usize>) -> Result<Option<ConvertedElement>, C2RError> {
    // Use cloned context to get the actual tokens from global context
    let mut context = crate::context!();
    context.pull();
    let tokens_in_range = &context.tokens[token_range.clone()];

    // Filter out Token::n() from the retrieved tokens
    let filtered_tokens: Vec<Token> = tokens_in_range
        .iter()
        .filter(|token| !matches!(token, crate::Token::n()))
        .cloned()
        .collect();

    // Try to match expression patterns
    let expression_patterns = [
        "expression_assignment",
        "expression_binary",
        "expression_unary",
        "expression_call",
        "expression_ternary",
        "expression",
    ];

    for pattern in &expression_patterns {
        match context.patternizer.match_pattern(pattern, &filtered_tokens) {
            PatternResult::Match { consumed_tokens: _ } => {
                // Parse expression components
                let (expr_type, operator, left_operand, right_operand) =
                    parse_expression_tokens(&filtered_tokens)?;

                let code = convert_expression_to_rust(
                    &expr_type,
                    &operator,
                    &left_operand,
                    &right_operand,
                )?;

                let converted_expression = ConvertedExpression {
                    expression_type: expr_type.clone(),
                    left_operand: left_operand.clone(),
                    operator: operator.clone(),
                    right_operand: right_operand.clone(),
                    code,
                    result_type: None,
                };

                return Ok(Some(ConvertedElement::Expression(converted_expression)));
            }
            _ => continue,
        }
    }

    // Fallback conversion for basic expressions
    if filtered_tokens.len() >= 1 {
        if contains_operators(&filtered_tokens) || contains_function_call(&filtered_tokens) {
            let (expr_type, operator, left_operand, right_operand) =
                parse_expression_tokens(&filtered_tokens)?;
            let code =
                convert_expression_to_rust(&expr_type, &operator, &left_operand, &right_operand)?;

            let converted_expression = ConvertedExpression {
                expression_type: expr_type.clone(),
                left_operand: left_operand.clone(),
                operator: operator.clone(),
                right_operand: right_operand.clone(),
                code,
                result_type: None,
            };

            return Ok(Some(ConvertedElement::Expression(converted_expression)));
        }
    }

    Ok(None)
}

/// Convert expression to Rust syntax
fn convert_expression_to_rust(
    expr_type: &str,
    operator: &str,
    left_operand: &str,
    right_operand: &str,
) -> Result<String, C2RError> {
    match expr_type {
        "assignment" | "binary" => {
            if !operator.is_empty() && !left_operand.is_empty() && !right_operand.is_empty() {
                let converted_op = convert_operator(operator);
                Ok(format!(
                    "{} {} {}",
                    left_operand, converted_op, right_operand
                ))
            } else if !left_operand.is_empty() {
                Ok(left_operand.to_string())
            } else {
                Ok(format!("{} {}", left_operand, right_operand))
            }
        }
        "unary" => {
            if !operator.is_empty() && !left_operand.is_empty() {
                let converted_op = convert_operator(operator);
                if operator == "++" || operator == "--" {
                    // Handle pre/post increment/decrement
                    Ok(format!("{}{}", left_operand, converted_op))
                } else {
                    Ok(format!("{}{}", converted_op, left_operand))
                }
            } else {
                Ok(left_operand.to_string())
            }
        }
        "call" => {
            if !left_operand.is_empty() {
                Ok(format!("{}({})", left_operand, right_operand))
            } else {
                Ok("()".to_string())
            }
        }
        "ternary" => {
            // Parse ternary from right operand which should contain "condition ? true_val : false_val"
            let parts: Vec<&str> = right_operand.split(" : ").collect();
            if parts.len() == 2 {
                Ok(format!(
                    "if {} {{ {} }} else {{ {} }}",
                    left_operand, parts[0], parts[1]
                ))
            } else {
                Ok(format!("{} {}", left_operand, right_operand))
            }
        }
        _ => {
            // Simple expression
            Ok(format!("{} {}", left_operand, right_operand)
                .trim()
                .to_string())
        }
    }
}
