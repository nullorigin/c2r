//! Define Macro Handler for C to Rust Converter
//! Specialized handler for #define preprocessor directives

use crate::convert::ConvertedMacro;
use crate::pattern::PatternResult;
use crate::{
    C2RError, ConvertedElement, ExtractedElement, Handler, HandlerPhase, HandlerResult, Id,
    ReportLevel,
};
use crate::{context, report};
use std::ops::Range;

/// Create define macro handler with specialized patterns
pub fn create_define_macro_handler() -> Handler {
    let handler_id = Id::get("define_macro_handler");
    let handler_role = "Convert C #define macros to Rust const or macro_rules!".to_string();
    let priority = 920; // High priority for specific pattern matching

    let mut handler = Handler::new(handler_id, handler_role, priority);
    handler.process = Some(process_define_macro);
    handler.handle = Some(handle_define_macro);
    handler.extract = Some(extract_define_macro);
    handler.convert = Some(convert_define_macro_wrapper);
    handler.document = Some(document_define_macro);
    handler.result = Some(result_define_macro);
    handler
}

/// Process callback: Detects #define directives
fn process_define_macro(token_range: Range<usize>) -> Result<bool, C2RError> {
    let mut context = context!();
    context.pull();

    if token_range.end > context.tokens.len() || token_range.start >= context.tokens.len() {
        return Ok(false);
    }

    let tokens = &context.tokens[token_range.clone()];

    if tokens.len() < 2 {
        return Ok(false);
    }

    // Use the existing Patternizer architecture
    let pattern_result = context.patternizer.match_pattern("define_macro", tokens);

    Ok(matches!(
        pattern_result,
        PatternResult::Match { .. } | PatternResult::Sequence { .. } | PatternResult::Fuzzy { .. }
    ))
}

/// Handle callback: Processes #define macros
fn handle_define_macro(token_range: Range<usize>) -> Result<HandlerResult, C2RError> {
    let id = Id::get("define_macro_handler");

    report!(
        "define_macro_handler",
        "handle_define_macro",
        ReportLevel::Info,
        HandlerPhase::Handle,
        format!("Processing #define macro in range {:?}", token_range),
        true
    );

    if let Ok(Some(converted)) = convert_define_macro_wrapper(token_range.clone()) {
        if let ConvertedElement::Macro(rust_macro) = converted {
            let mut context = context!();
            context.pull();
            return Ok(HandlerResult::Completed(
                Some(context.tokens[token_range.clone()].to_vec()),
                token_range,
                rust_macro.code,
                id,
            ));
        }
    }

    Ok(HandlerResult::NotHandled(None, token_range, id))
}

/// Extract #define macro information from tokens
fn extract_define_macro(token_range: Range<usize>) -> Result<Option<ExtractedElement>, C2RError> {
    let mut context = context!();
    context.pull();
    let tokens = &context.tokens[token_range.clone()];

    if tokens.is_empty() {
        return Ok(None);
    }

    let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

    // Extract macro name (should be third token or second if #define is combined)
    let (macro_name, body_start) = if token_strings.get(0) == Some(&"#define".to_string()) {
        (
            token_strings
                .get(1)
                .cloned()
                .unwrap_or_else(|| "UNNAMED".to_string()),
            2,
        )
    } else if token_strings.get(0) == Some(&"#".to_string())
        && token_strings.get(1) == Some(&"define".to_string())
    {
        (
            token_strings
                .get(2)
                .cloned()
                .unwrap_or_else(|| "UNNAMED".to_string()),
            3,
        )
    } else {
        ("UNNAMED".to_string(), 1)
    };

    // Check if it's function-like macro (has parentheses immediately after name)
    let is_function_like =
        body_start < token_strings.len() && token_strings.get(body_start) == Some(&"(".to_string());

    // Extract parameters if function-like
    let params = if is_function_like {
        extract_macro_parameters(&token_strings, body_start)
    } else {
        vec![]
    };

    // Extract body (everything after parameters or name)
    let body_tokens = if is_function_like {
        // Find closing paren and extract body after it
        let mut paren_depth = 0;
        let mut body_start_idx = body_start;
        for (i, token) in token_strings.iter().enumerate().skip(body_start) {
            if token == "(" {
                paren_depth += 1;
            } else if token == ")" {
                paren_depth -= 1;
                if paren_depth == 0 {
                    body_start_idx = i + 1;
                    break;
                }
            }
        }
        tokens[body_start_idx..].to_vec()
    } else {
        tokens[body_start..].to_vec()
    };

    Ok(Some(ExtractedElement::Macro(
        crate::extract::ExtractedMacro {
            name: macro_name,
            params,
            body: body_tokens,
            tokens: tokens.to_vec(),
            is_function_like,
            code: token_strings.join(" "),
        },
    )))
}

/// Extract macro parameters from token list
fn extract_macro_parameters(tokens: &[String], start_idx: usize) -> Vec<String> {
    let mut params = vec![];
    let mut current_param = String::new();
    let mut paren_depth = 0;
    let mut in_params = false;

    for token in tokens.iter().skip(start_idx) {
        if token == "(" {
            paren_depth += 1;
            in_params = true;
        } else if token == ")" {
            paren_depth -= 1;
            if paren_depth == 0 {
                if !current_param.trim().is_empty() {
                    params.push(current_param.trim().to_string());
                }
                break;
            }
        } else if in_params && paren_depth > 0 {
            if token == "," {
                if !current_param.trim().is_empty() {
                    params.push(current_param.trim().to_string());
                }
                current_param.clear();
            } else {
                if !current_param.is_empty() {
                    current_param.push(' ');
                }
                current_param.push_str(token);
            }
        }
    }

    params
}

/// Convert #define macros from token range
fn convert_define_macro_wrapper(
    token_range: Range<usize>,
) -> Result<Option<ConvertedElement>, C2RError> {
    let mut context = context!();
    context.pull();
    let tokens = &context.tokens[token_range.clone()];

    if tokens.is_empty() {
        return Ok(None);
    }

    let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

    // Extract macro components
    let (macro_name, body_start) = if token_strings.get(0) == Some(&"#define".to_string()) {
        (
            token_strings
                .get(1)
                .cloned()
                .unwrap_or_else(|| "UNNAMED".to_string()),
            2,
        )
    } else if token_strings.get(0) == Some(&"#".to_string())
        && token_strings.get(1) == Some(&"define".to_string())
    {
        (
            token_strings
                .get(2)
                .cloned()
                .unwrap_or_else(|| "UNNAMED".to_string()),
            3,
        )
    } else {
        ("UNNAMED".to_string(), 1)
    };

    let is_function_like =
        body_start < token_strings.len() && token_strings.get(body_start) == Some(&"(".to_string());

    let rust_code = if is_function_like {
        // Convert function-like macro to macro_rules!
        let params = extract_macro_parameters(&token_strings, body_start);
        let body = token_strings[body_start + params.len() + 2..].join(" "); // Skip name, (, params, )

        if params.is_empty() {
            format!(
                "macro_rules! {} {{\n    () => {{\n        {}\n    }};\n}}",
                macro_name, body
            )
        } else {
            let param_pattern = params
                .iter()
                .map(|p| format!("${}: expr", p))
                .collect::<Vec<_>>()
                .join(", ");
            let param_usage = params
                .iter()
                .map(|p| format!("${}", p))
                .collect::<Vec<_>>()
                .join(", ");
            format!(
                "macro_rules! {} {{\n    ({}) => {{\n        {}\n    }};\n}}",
                macro_name,
                param_pattern,
                body.replace(&params.join(" "), &param_usage)
            )
        }
    } else {
        // Convert simple macro to const
        let body = token_strings[body_start..].join(" ");

        // Try to determine if it's a numeric constant
        if body.chars().all(|c| {
            c.is_ascii_digit() || c == '.' || c == 'x' || c == 'X' || c.is_ascii_hexdigit()
        }) {
            if body.contains('.') {
                format!("pub const {}: f64 = {};", macro_name, body)
            } else if body.starts_with("0x") || body.starts_with("0X") {
                format!("pub const {}: i32 = {};", macro_name, body)
            } else {
                format!("pub const {}: i32 = {};", macro_name, body)
            }
        } else if body.starts_with('"') && body.ends_with('"') {
            // String literal
            format!("pub const {}: &str = {};", macro_name, body)
        } else {
            // Complex expression - use macro_rules!
            format!(
                "macro_rules! {} {{\n    () => {{\n        {}\n    }};\n}}",
                macro_name, body
            )
        }
    };

    Ok(Some(ConvertedElement::Macro(ConvertedMacro {
        parameters: if is_function_like {
            extract_macro_parameters(&token_strings, body_start)
        } else {
            vec![]
        },
        body: token_strings[body_start..].join(" "),
        is_function_like,
        code: rust_code,
    })))
}

/// Document #define macro
fn document_define_macro(info: crate::document::ElementInfo) -> Result<Option<String>, C2RError> {
    if let crate::document::ElementInfo::Macro(macro_info) = info {
        let mut doc = format!("/// Converted C #define macro: {}\n", macro_info.name);
        if macro_info.is_function_like {
            doc.push_str("/// Function-like macro -> macro_rules! in Rust\n");
            if !macro_info.params.is_empty() {
                doc.push_str(&format!(
                    "/// Parameters: {}\n",
                    macro_info.params.join(", ")
                ));
            }
        } else {
            doc.push_str("/// Object-like macro -> const or macro_rules! in Rust\n");
        }
        Ok(Some(doc))
    } else {
        Ok(None)
    }
}

/// Result callback for #define macros
fn result_define_macro(
    _token_range: Range<usize>,
    result: HandlerResult,
) -> Result<HandlerResult, C2RError> {
    report!(
        "define_macro_handler",
        "result_define_macro",
        ReportLevel::Info,
        HandlerPhase::Report,
        "Postprocessing #define macro conversion result",
        true
    );

    Ok(result)
}
