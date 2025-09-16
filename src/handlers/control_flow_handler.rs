//! Control flow handler for C to Rust control flow conversion
//! Uses Patternizer exclusively for detection and conversion

use super::common::not_handled;
use crate::pattern::PatternResult;
use crate::token::Token;
use crate::{C2RError, ConvertedElement, ExtractedElement, Id, Kind, Reason};
use crate::{ConvertedControlFlow, ExtractedControlFlow};
use crate::{HandlerPhase, ReportLevel};
use crate::{HandlerReport, HandlerResult, context, report};
use std::ops::Range;

/// Report callback: Collects control flow handler reports
pub fn report_control_flow() -> Result<HandlerReport, C2RError> {
    let context = context!();
    let reports = context.get_reports_by_handler("control_flow_handler");

    let (info_count, warning_count, error_count) =
        reports
            .iter()
            .fold((0, 0, 0), |acc, report| match report.level {
                ReportLevel::Error => (acc.0, acc.1, acc.2 + 1),
                ReportLevel::Warning => (acc.0, acc.1 + 1, acc.2),
                _ => (acc.0 + 1, acc.1, acc.2),
            });

    Ok(HandlerReport {
        report_id: Box::new(Id::get(&Id::gen_name("control_flow_handler"))),
        handler_id: Box::new(Id::get("control_flow_handler")),
        handler_name: "control_flow_handler".to_string(),
        function_name: "report_control_flow".to_string(),
        message: format!(
            "Control flow handler summary: {} reports ({} info, {} warnings, {} errors)",
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

/// Handle callback: Sets up patternizer and registers control flow patterns
fn handle_control_flow(token_range: Range<usize>) -> Result<HandlerResult, C2RError> {
    let mut context = crate::context!();
    context.pull();

    // Add bounds checking to prevent slice index out of bounds
    if token_range.end > context.tokens.len() || token_range.start >= context.tokens.len() {
        return Ok(HandlerResult::NotHandled(
            Some(Vec::new()),
            token_range,
            Id::get("control_flow_bounds_check"),
        ));
    }

    let tokens = &context.tokens[token_range.clone()];
    let filtered_tokens: Vec<Token> = tokens
        .iter()
        .filter(|token| !matches!(token, crate::Token::n()))
        .cloned()
        .collect();
    let tokens = &filtered_tokens;

    // Register control flow patterns in the global registry via context
    let control_patterns = [
        "control_flow_if",
        "control_flow_for",
        "control_flow_while",
        "control_flow_switch",
        "control_flow",
    ];

    for pattern in &control_patterns {
        match context.patternizer.match_pattern(pattern, tokens) {
            PatternResult::Match { consumed_tokens } => {
                report!(
                    "control_flow_handler",
                    "handle_control_flow",
                    ReportLevel::Info,
                    HandlerPhase::Handle,
                    format!(
                        "Processing {} control flow with {} tokens",
                        pattern, consumed_tokens
                    ),
                    true
                );

                // Convert using pattern data
                match convert_control_flow(token_range.clone())? {
                    Some(ConvertedElement::ControlFlow(converted_control_flow)) => {
                        let code = converted_control_flow.code.clone();
                        return Ok(HandlerResult::Converted(
                            ConvertedElement::ControlFlow(converted_control_flow),
                            0..consumed_tokens,
                            code,
                            Id::get("control_flow_handler"),
                        ));
                    }
                    _ => {
                        report!(
                            "control_flow_handler",
                            "handle_control_flow",
                            ReportLevel::Error,
                            HandlerPhase::Handle,
                            "Failed to convert control flow",
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

/// Process callback: Uses Patternizer to detect control flow patterns
fn process_control_flow(token_range: Range<usize>) -> Result<bool, C2RError> {
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
    if tokens.is_empty() {
        return Ok(false);
    }

    // Check for control flow patterns
    let control_patterns = [
        "control_flow_if",
        "control_flow_for",
        "control_flow_while",
        "control_flow_switch",
        "control_flow",
    ];

    for pattern in &control_patterns {
        if let PatternResult::Match { consumed_tokens: _ } =
            context.patternizer.match_pattern(pattern, tokens)
        {
            return Ok(true);
        }
    }

    // Fallback: check for control flow keywords
    let first_token = tokens[0].to_string();
    Ok(["if", "for", "while", "switch", "do"]
        .iter()
        .any(|&kw| first_token == kw))
}
use crate::{HandlerPhase::Convert, ReportLevel::Info};

/// Creates a control flow handler that uses Patternizer exclusively
pub fn create_control_flow_handler() -> crate::handler::Handler {
    let handler_id = Id::get("control_flow_handler");
    let handler_role = "control_flow";
    let priority = 110;

    super::create_handler(
        handler_id,
        handler_role,
        priority,
        Some(process_control_flow),
        Some(handle_control_flow),
        Some(extract_control_flow),
        Some(convert_control_flow),
        None, // No document callback
        Some(report_control_flow),
        None, // No result callback
        None, // No redirect callback
    )
}

/// Extract callback: Extracts control flow information using Patternizer
pub fn extract_control_flow(
    token_range: Range<usize>,
) -> Result<Option<ExtractedElement>, C2RError> {
    let mut context = crate::context!();
    context.pull();
    let tokens = &context.tokens[token_range.clone()];
    let filtered_tokens: Vec<Token> = tokens
        .iter()
        .filter(|token| !matches!(token, Token::n()))
        .cloned()
        .collect();
    let tokens = &filtered_tokens;

    // Try to match control flow patterns
    let control_patterns = ["if", "for", "while", "switch", "do"];

    for pattern in &control_patterns {
        match context.patternizer.match_pattern(pattern, tokens) {
            PatternResult::Match { consumed_tokens } => {
                // Extract control flow components
                let (control_type, condition, body) = parse_control_flow_tokens(tokens, pattern)?;

                let extracted_control_flow = ExtractedControlFlow {
                    control_type,
                    condition,
                    body,
                    tokens: tokens[..consumed_tokens.min(tokens.len())].to_vec(),
                    code: tokens[..consumed_tokens.min(tokens.len())]
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<_>>()
                        .join(" "),
                };

                return Ok(Some(ExtractedElement::ControlFlow(extracted_control_flow)));
            }
            _ => continue,
        }
    }

    Ok(None)
}

/// Convert callback: Converts extracted control flow to Rust
pub fn convert_control_flow(
    token_range: Range<usize>,
) -> Result<Option<ConvertedElement>, C2RError> {
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

    if filtered_tokens.is_empty() {
        return Ok(None);
    }

    // Use the filtered tokens with a full range for extraction
    let full_range = 0..filtered_tokens.len();
    if let Some(ExtractedElement::ControlFlow(element)) = extract_control_flow(full_range)? {
        report!(
            "control_flow_handler",
            "convert_control_flow",
            Info,
            Convert,
            format!("Converting control flow: {}", element.control_type),
            true
        );

        let code = generate_rust_control_flow(&element)?;

        Ok(Some(ConvertedElement::ControlFlow(ConvertedControlFlow {
            control_type: element.control_type.clone(),
            condition: element
                .condition
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(" "),
            body: element
                .body
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(" "),
            code,
            has_else: element.control_type == "if" && element.code.contains("else"),
        })))
    } else {
        Ok(None)
    }
}

/// Parse control flow tokens to extract components
fn parse_control_flow_tokens(
    tokens: &[Token],
    control_type: &str,
) -> Result<(String, Vec<Token>, Vec<Token>), C2RError> {
    let mut condition = Vec::new();
    let mut body = Vec::new();

    match control_type {
        "if" | "while" | "switch" => {
            // Extract condition from parentheses
            if tokens.len() >= 2 && tokens[1].to_string() == "(" {
                let mut paren_count = 0;
                let mut i = 1;
                while i < tokens.len() {
                    if tokens[i].to_string() == "(" {
                        paren_count += 1;
                    } else if tokens[i].to_string() == ")" {
                        paren_count -= 1;
                        if paren_count == 0 {
                            condition = tokens[2..i].to_vec();
                            // Body starts after condition
                            if i + 1 < tokens.len() {
                                body = tokens[i + 1..].to_vec();
                            }
                            break;
                        }
                    }
                    i += 1;
                }
            }
        }
        "for" => {
            // For loops have three parts separated by semicolons in condition
            if tokens.len() >= 2 && tokens[1].to_string() == "(" {
                let mut paren_count = 0;
                let mut i = 1;
                while i < tokens.len() {
                    if tokens[i].to_string() == "(" {
                        paren_count += 1;
                    } else if tokens[i].to_string() == ")" {
                        paren_count -= 1;
                        if paren_count == 0 {
                            condition = tokens[2..i].to_vec(); // Full for header
                            if i + 1 < tokens.len() {
                                body = tokens[i + 1..].to_vec();
                            }
                            break;
                        }
                    }
                    i += 1;
                }
            }
        }
        "do" => {
            // Do-while: body comes first, condition at end
            let mut i = 1;
            let mut brace_count = 0;
            let mut _found_while = false;

            while i < tokens.len() {
                if tokens[i].to_string() == "{" {
                    brace_count += 1;
                } else if tokens[i].to_string() == "}" {
                    brace_count -= 1;
                    if brace_count == 0 {
                        // End of body block
                        body = tokens[1..i + 1].to_vec();
                        // Look for while condition
                        if i + 1 < tokens.len() && tokens[i + 1].to_string() == "while" {
                            _found_while = true;
                            // Extract condition from parentheses after while
                            let mut j = i + 2;
                            if j < tokens.len() && tokens[j].to_string() == "(" {
                                let mut paren_count = 0;
                                while j < tokens.len() {
                                    if tokens[j].to_string() == "(" {
                                        paren_count += 1;
                                    } else if tokens[j].to_string() == ")" {
                                        paren_count -= 1;
                                        if paren_count == 0 {
                                            condition = tokens[i + 3..j].to_vec();
                                            break;
                                        }
                                    }
                                    j += 1;
                                }
                            }
                        }
                        break;
                    }
                }
                i += 1;
            }
        }
        _ => {}
    }

    Ok((control_type.to_string(), condition, body))
}

/// Generate Rust control flow code
fn generate_rust_control_flow(element: &ExtractedControlFlow) -> Result<String, C2RError> {
    let mut code = String::new();

    match element.control_type.as_str() {
        "if" => {
            let condition = element
                .condition
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(" ");
            let body = element
                .body
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(" ");
            code.push_str(&format!("if {} {{\n    {}\n}}", condition, body));
        }
        "while" => {
            let condition = element
                .condition
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(" ");
            let body = element
                .body
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(" ");
            if condition == "true" || condition == "1" {
                code.push_str(&format!("loop {{\n    {}\n}}", body));
            } else {
                code.push_str(&format!("while {} {{\n    {}\n}}", condition, body));
            }
        }
        "for" => {
            // Convert C for loop to Rust while loop (simplified)
            let condition = element
                .condition
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(" ");
            let body = element
                .body
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(" ");
            code.push_str(&format!(
                "// TODO: Convert for loop\nwhile {} {{\n    {}\n}}",
                condition, body
            ));
        }
        "switch" => {
            let condition = element
                .condition
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(" ");
            let body = element
                .body
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(" ");
            code.push_str(&format!(
                "match {} {{\n    // TODO: Parse cases\n    _ => {{\n        {}\n    }}\n}}",
                condition, body
            ));
        }
        "do" => {
            let condition = element
                .condition
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(" ");
            let body = element
                .body
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(" ");
            code.push_str(&format!(
                "loop {{\n    {}\n    if !({}) {{\n        break;\n    }}\n}}",
                body, condition
            ));
        }
        _ => {
            return Err(C2RError::new(
                Kind::Other,
                Reason::Unsupported("control flow type"),
                None,
            ));
        }
    }

    Ok(code)
}
