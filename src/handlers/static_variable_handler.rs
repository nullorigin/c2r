//! Static Variable Handler for C to Rust Converter
//! Specialized handler for static variable declarations

use crate::convert::ConvertedGlobal;
use crate::pattern::PatternResult;
use crate::{
    C2RError, ConvertedElement, ExtractedElement, Handler, HandlerPhase, HandlerResult, Id,
    ReportLevel,
};
use crate::{context, report};
use std::ops::Range;

/// Create static variable handler with specialized patterns
pub fn create_static_variable_handler() -> Handler {
    let handler_id = Id::get("static_variable_handler");
    let handler_role =
        "Convert C static variable declarations to Rust static variables".to_string();
    let priority = 870; // High priority for specific pattern matching

    let mut handler = Handler::new(handler_id, handler_role, priority);
    handler.process = Some(process_static_variable);
    handler.handle = Some(handle_static_variable);
    handler.extract = Some(extract_static_variable);
    handler.convert = Some(convert_static_variable_wrapper);
    handler.document = Some(document_static_variable);
    handler.result = Some(result_static_variable);
    handler
}

/// Process callback: Detects static variable declarations (static keyword)
fn process_static_variable(token_range: Range<usize>) -> Result<bool, C2RError> {
    let mut context = context!();
    context.pull();

    if token_range.end > context.tokens.len() || token_range.start >= context.tokens.len() {
        return Ok(false);
    }

    let tokens = &context.tokens[token_range.clone()];

    if tokens.len() < 3 {
        return Ok(false);
    }

    // Use the existing Patternizer architecture
    let pattern_result = context.patternizer.match_pattern("static_variable", tokens);

    Ok(matches!(
        pattern_result,
        PatternResult::Match { .. } | PatternResult::Sequence { .. } | PatternResult::Fuzzy { .. }
    ))
}

/// Handle callback: Processes static variable declarations
fn handle_static_variable(token_range: Range<usize>) -> Result<HandlerResult, C2RError> {
    let id = Id::get("static_variable_handler");

    report!(
        "static_variable_handler",
        "handle_static_variable",
        ReportLevel::Info,
        HandlerPhase::Handle,
        format!("Processing static variable in range {:?}", token_range),
        true
    );

    if let Ok(Some(converted)) = convert_static_variable_wrapper(token_range.clone()) {
        if let ConvertedElement::Global(rust_global) = converted {
            let mut context = context!();
            context.pull();
            return Ok(HandlerResult::Completed(
                Some(context.tokens[token_range.clone()].to_vec()),
                token_range,
                rust_global.code,
                id,
            ));
        }
    }

    Ok(HandlerResult::NotHandled(None, token_range, id))
}

/// Extract static variable information from tokens
fn extract_static_variable(
    token_range: Range<usize>,
) -> Result<Option<ExtractedElement>, C2RError> {
    let mut context = context!();
    context.pull();
    let tokens = &context.tokens[token_range.clone()];

    if tokens.is_empty() {
        return Ok(None);
    }

    Ok(Some(ExtractedElement::Global(
        crate::extract::ExtractedGlobal {
            name: "static_var".to_string(),
            array_dims: vec![],
            initial_value: None,
            is_const: false,
            is_static: true,
            tokens: tokens.to_vec(),
            is_extern: false,
            array_size: None,
            initializer: None,
            type_name: "int".to_string(),
            storage_class: Some("static".to_string()),
            code: "/* static variable */".to_string(),
        },
    )))
}

/// Convert static variables from token range
fn convert_static_variable_wrapper(
    token_range: Range<usize>,
) -> Result<Option<ConvertedElement>, C2RError> {
    let mut context = context!();
    context.pull();
    let tokens = &context.tokens[token_range.clone()];

    if tokens.is_empty() {
        return Ok(None);
    }

    let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

    // Find type (after static keyword)
    let static_pos = token_strings.iter().position(|s| s == "static");
    let var_type = if let Some(pos) = static_pos {
        token_strings
            .get(pos + 1)
            .filter(|s| {
                matches!(
                    s.as_str(),
                    "int" | "char" | "float" | "double" | "short" | "long"
                )
            })
            .cloned()
            .unwrap_or_else(|| "i32".to_string())
    } else {
        "i32".to_string()
    };

    // Find variable name (token before '=' or ';')
    let var_name = if let Some(eq_pos) = token_strings.iter().position(|s| s == "=") {
        token_strings.get(eq_pos - 1).cloned()
    } else if let Some(semi_pos) = token_strings.iter().position(|s| s == ";") {
        token_strings.get(semi_pos - 1).cloned()
    } else {
        None
    }
    .unwrap_or_else(|| "unknown_static".to_string());

    // Find initializer value if present
    let initializer = token_strings
        .iter()
        .position(|s| s == "=")
        .and_then(|pos| token_strings.get(pos + 1))
        .filter(|s| s != &";")
        .cloned();

    // Convert C type to Rust type
    let rust_type = match var_type.as_str() {
        "int" => "i32",
        "char" => "i8",
        "float" => "f32",
        "double" => "f64",
        "short" => "i16",
        "long" => "i64",
        _ => "i32",
    };

    let rust_code = if let Some(init_val) = &initializer {
        format!(
            "static {}: {} = {};",
            var_name.to_uppercase(),
            rust_type,
            init_val
        )
    } else {
        format!(
            "static {}: {} = Default::default();",
            var_name.to_uppercase(),
            rust_type
        )
    };

    Ok(Some(ConvertedElement::Global(ConvertedGlobal {
        var_type: rust_type.to_string(),
        initializer: initializer.or_else(|| Some("Default::default()".to_string())),
        code: rust_code,
        is_const: false,
        is_static: true,
        is_public: true,
    })))
}

/// Document static variable
fn document_static_variable(
    info: crate::document::ElementInfo,
) -> Result<Option<String>, C2RError> {
    if let crate::document::ElementInfo::Global(global_info) = info {
        let mut doc = format!("/// Converted C static variable: {}\n", global_info.name);
        doc.push_str("/// Static storage duration, internal linkage in C -> static in Rust\n");
        doc.push_str(&format!("/// Type: {}\n", global_info.type_name));
        Ok(Some(doc))
    } else {
        Ok(None)
    }
}

/// Result callback for static variables
fn result_static_variable(
    _token_range: Range<usize>,
    result: HandlerResult,
) -> Result<HandlerResult, C2RError> {
    report!(
        "static_variable_handler",
        "result_static_variable",
        ReportLevel::Info,
        HandlerPhase::Report,
        "Postprocessing static variable conversion result",
        true
    );

    Ok(result)
}
