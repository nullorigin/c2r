//! Global Variable Handler for C to Rust Converter
//! Specialized handler for global variable declarations

use crate::convert::ConvertedGlobal;
use crate::pattern::PatternResult;
use crate::{
    C2RError, ConvertedElement, ExtractedElement, Handler, HandlerPhase, HandlerResult, Id,
    ReportLevel,
};
use crate::{context, report};
use std::ops::Range;

/// Create global variable handler with specialized patterns
pub fn create_global_variable_handler() -> Handler {
    let handler_id = Id::get("global_variable_handler");
    let handler_role =
        "Convert C global variable declarations to Rust static variables".to_string();
    let priority = 800; // High priority for specific pattern matching

    let mut handler = Handler::new(handler_id, handler_role, priority);
    handler.process = Some(process_global_variable);
    handler.handle = Some(handle_global_variable);
    handler.extract = Some(extract_global_variable);
    handler.convert = Some(convert_global_variable_wrapper);
    handler.document = Some(document_global_variable);
    handler.result = Some(result_global_variable);
    handler
}

/// Process callback: Detects global variable declarations (not const, not static, not extern)
fn process_global_variable(token_range: Range<usize>) -> Result<bool, C2RError> {
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
    let pattern_result = context.patternizer.match_pattern("global_variable", tokens);

    Ok(matches!(
        pattern_result,
        PatternResult::Match { .. } | PatternResult::Sequence { .. } | PatternResult::Fuzzy { .. }
    ))
}

/// Handle callback: Processes global variable declarations
fn handle_global_variable(token_range: Range<usize>) -> Result<HandlerResult, C2RError> {
    let id = Id::get("global_variable_handler");

    report!(
        "global_variable_handler",
        "handle_global_variable",
        ReportLevel::Info,
        HandlerPhase::Handle,
        format!("Processing global variable in range {:?}", token_range),
        true
    );

    if let Ok(Some(converted)) = convert_global_variable_wrapper(token_range.clone()) {
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

/// Extract global variable information from tokens
fn extract_global_variable(
    token_range: Range<usize>,
) -> Result<Option<ExtractedElement>, C2RError> {
    let mut context = context!();
    context.pull();
    let tokens = &context.tokens[token_range.clone()];

    if tokens.is_empty() {
        return Ok(None);
    }

    // Simple extraction - will be enhanced later
    Ok(Some(ExtractedElement::Global(
        crate::extract::ExtractedGlobal {
            name: "global_var".to_string(),
            array_dims: vec![],
            initial_value: None,
            is_const: false,
            is_static: false,
            tokens: tokens.to_vec(),
            is_extern: false,
            array_size: None,
            initializer: None,
            type_name: "int".to_string(),
            storage_class: None,
            code: "/* global variable */".to_string(),
        },
    )))
}

/// Convert global variables from token range
fn convert_global_variable_wrapper(
    token_range: Range<usize>,
) -> Result<Option<ConvertedElement>, C2RError> {
    let mut context = context!();
    context.pull();
    let tokens = &context.tokens[token_range.clone()];

    if tokens.is_empty() {
        return Ok(None);
    }

    // Extract variable name and type from tokens
    let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

    // Find type (first non-storage-class token)
    let var_type = token_strings
        .iter()
        .find(|s| {
            matches!(
                s.as_str(),
                "int" | "char" | "float" | "double" | "short" | "long"
            )
        })
        .cloned()
        .unwrap_or_else(|| "i32".to_string());

    // Find variable name (token before '=' or ';')
    let var_name = token_strings
        .iter()
        .position(|s| s == "=" || s == ";")
        .and_then(|pos| {
            if pos > 0 {
                token_strings.get(pos - 1)
            } else {
                None
            }
        })
        .cloned()
        .unwrap_or_else(|| "unknown_var".to_string());

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

    let rust_code = format!("static {}: {} = Default::default();", var_name, rust_type);

    Ok(Some(ConvertedElement::Global(ConvertedGlobal {
        var_type: rust_type.to_string(),
        initializer: Some("Default::default()".to_string()),
        code: rust_code,
        is_const: false,
        is_static: true, // Global variables become static in Rust
        is_public: true,
    })))
}

/// Document global variable
fn document_global_variable(
    info: crate::document::ElementInfo,
) -> Result<Option<String>, C2RError> {
    if let crate::document::ElementInfo::Global(global_info) = info {
        let mut doc = format!("/// Converted C global variable: {}\n", global_info.name);
        doc.push_str(&format!(
            "/// Type: {} -> {}\n",
            global_info.type_name, "static"
        ));
        Ok(Some(doc))
    } else {
        Ok(None)
    }
}

/// Result callback for global variables
fn result_global_variable(
    _token_range: Range<usize>,
    result: HandlerResult,
) -> Result<HandlerResult, C2RError> {
    report!(
        "global_variable_handler",
        "result_global_variable",
        ReportLevel::Info,
        HandlerPhase::Report,
        "Postprocessing global variable conversion result",
        true
    );

    Ok(result)
}
