//! Conditional Macro Handler for C to Rust Converter
//! Specialized handler for #ifdef, #ifndef, #endif, #else, #elif preprocessor directives

use crate::convert::ConvertedMacro;
use crate::pattern::PatternResult;
use crate::{
    C2RError, ConvertedElement, ExtractedElement, Handler, HandlerPhase, HandlerResult, Id,
    ReportLevel,
};
use crate::{context, report};
use std::ops::Range;

/// Create conditional macro handler with specialized patterns
pub fn create_conditional_macro_handler() -> Handler {
    let handler_id = Id::get("conditional_macro_handler");
    let handler_role =
        "Convert C conditional preprocessor directives to Rust cfg attributes".to_string();
    let priority = 910; // High priority for specific pattern matching

    let mut handler = Handler::new(handler_id, handler_role, priority);
    handler.process = Some(process_conditional_macro);
    handler.handle = Some(handle_conditional_macro);
    handler.extract = Some(extract_conditional_macro);
    handler.convert = Some(convert_conditional_macro_wrapper);
    handler.document = Some(document_conditional_macro);
    handler.result = Some(result_conditional_macro);
    handler
}

/// Process callback: Detects conditional preprocessor directives
fn process_conditional_macro(token_range: Range<usize>) -> Result<bool, C2RError> {
    let mut context = context!();
    context.pull();

    if token_range.end > context.tokens.len() || token_range.start >= context.tokens.len() {
        return Ok(false);
    }

    let tokens = &context.tokens[token_range.clone()];

    if tokens.len() < 1 {
        return Ok(false);
    }

    // Use the existing Patternizer architecture
    let pattern_result = context
        .patternizer
        .match_pattern("conditional_macro", tokens);

    Ok(matches!(
        pattern_result,
        PatternResult::Match { .. } | PatternResult::Sequence { .. } | PatternResult::Fuzzy { .. }
    ))
}

/// Handle callback: Processes conditional macros
fn handle_conditional_macro(token_range: Range<usize>) -> Result<HandlerResult, C2RError> {
    let id = Id::get("conditional_macro_handler");

    report!(
        "conditional_macro_handler",
        "handle_conditional_macro",
        ReportLevel::Info,
        HandlerPhase::Handle,
        format!("Processing conditional macro in range {:?}", token_range),
        true
    );

    if let Ok(Some(converted)) = convert_conditional_macro_wrapper(token_range.clone()) {
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

/// Extract conditional macro information from tokens
fn extract_conditional_macro(
    token_range: Range<usize>,
) -> Result<Option<ExtractedElement>, C2RError> {
    let mut context = context!();
    context.pull();
    let tokens = &context.tokens[token_range.clone()];

    if tokens.is_empty() {
        return Ok(None);
    }

    let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

    // Determine directive type and extract condition
    let (directive, condition) = if let Some(first_token) = token_strings.get(0) {
        if first_token.starts_with('#') {
            let directive_name = first_token.strip_prefix('#').unwrap_or(first_token);
            let condition = if token_strings.len() > 1 {
                token_strings[1..].join(" ")
            } else {
                String::new()
            };
            (directive_name.to_string(), condition)
        } else if first_token == "#" && token_strings.len() >= 2 {
            let directive_name = token_strings[1].clone();
            let condition = if token_strings.len() > 2 {
                token_strings[2..].join(" ")
            } else {
                String::new()
            };
            (directive_name, condition)
        } else {
            ("unknown".to_string(), String::new())
        }
    } else {
        ("unknown".to_string(), String::new())
    };

    Ok(Some(ExtractedElement::Macro(
        crate::extract::ExtractedMacro {
            name: format!("#{}", directive),
            params: if condition.is_empty() {
                vec![]
            } else {
                vec![condition]
            },
            body: vec![], // Conditional directives don't have body tokens
            tokens: tokens.to_vec(),
            is_function_like: false,
            code: token_strings.join(" "),
        },
    )))
}

/// Convert conditional macros from token range
fn convert_conditional_macro_wrapper(
    token_range: Range<usize>,
) -> Result<Option<ConvertedElement>, C2RError> {
    let mut context = context!();
    context.pull();
    let tokens = &context.tokens[token_range.clone()];

    if tokens.is_empty() {
        return Ok(None);
    }

    let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

    // Determine directive type and extract condition
    let (directive, condition) = if let Some(first_token) = token_strings.get(0) {
        if first_token.starts_with('#') {
            let directive_name = first_token.strip_prefix('#').unwrap_or(first_token);
            let condition = if token_strings.len() > 1 {
                token_strings[1..].join(" ")
            } else {
                String::new()
            };
            (directive_name.to_string(), condition)
        } else if first_token == "#" && token_strings.len() >= 2 {
            let directive_name = token_strings[1].clone();
            let condition = if token_strings.len() > 2 {
                token_strings[2..].join(" ")
            } else {
                String::new()
            };
            (directive_name, condition)
        } else {
            ("unknown".to_string(), String::new())
        }
    } else {
        ("unknown".to_string(), String::new())
    };

    let rust_code = match directive.as_str() {
        "ifdef" => {
            if condition.is_empty() {
                "// #ifdef -> #[cfg(feature = \"unknown\")]".to_string()
            } else {
                format!("#[cfg(feature = \"{}\")]", condition)
            }
        }
        "ifndef" => {
            if condition.is_empty() {
                "// #ifndef -> #[cfg(not(feature = \"unknown\"))]".to_string()
            } else {
                format!("#[cfg(not(feature = \"{}\"))]", condition)
            }
        }
        "endif" => "// #endif - conditional compilation block end".to_string(),
        "else" => "// #else -> alternative cfg block".to_string(),
        "elif" => {
            if condition.is_empty() {
                "// #elif -> #[cfg(feature = \"unknown\")]".to_string()
            } else {
                format!(
                    "// #elif -> #[cfg(feature = \"{}\")] (chain with previous)",
                    condition
                )
            }
        }
        "undef" => {
            if condition.is_empty() {
                "// #undef - macro undefined".to_string()
            } else {
                format!(
                    "// #undef {} - macro undefined in Rust (const removed)",
                    condition
                )
            }
        }
        _ => {
            format!(
                "// Unknown conditional directive: #{} {}",
                directive, condition
            )
        }
    };

    Ok(Some(ConvertedElement::Macro(ConvertedMacro {
        parameters: if condition.is_empty() {
            vec![]
        } else {
            vec![condition]
        },
        body: directive,
        is_function_like: false,
        code: rust_code,
    })))
}

/// Document conditional macro
fn document_conditional_macro(
    info: crate::document::ElementInfo,
) -> Result<Option<String>, C2RError> {
    if let crate::document::ElementInfo::Macro(macro_info) = info {
        let mut doc = format!(
            "/// Converted C conditional preprocessor directive: {}\n",
            macro_info.name
        );

        match macro_info.name.as_str() {
            "#ifdef" | "#ifndef" => {
                doc.push_str("/// Conditional compilation -> cfg attribute in Rust\n");
                doc.push_str("/// Note: C preprocessor conditions may need manual review\n");
            }
            "#endif" => {
                doc.push_str("/// End of conditional compilation block\n");
            }
            "#else" | "#elif" => {
                doc.push_str("/// Alternative conditional compilation branch\n");
            }
            "#undef" => {
                doc.push_str("/// Macro undefinition -> remove const definition in Rust\n");
            }
            _ => {
                doc.push_str("/// Preprocessor directive -> manual review needed\n");
            }
        }

        Ok(Some(doc))
    } else {
        Ok(None)
    }
}

/// Result callback for conditional macros
fn result_conditional_macro(
    _token_range: Range<usize>,
    result: HandlerResult,
) -> Result<HandlerResult, C2RError> {
    report!(
        "conditional_macro_handler",
        "result_conditional_macro",
        ReportLevel::Info,
        HandlerPhase::Report,
        "Postprocessing conditional macro conversion result",
        true
    );

    Ok(result)
}
