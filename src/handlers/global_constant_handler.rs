//! Global Constant Handler for C to Rust Converter
//! Specialized handler for global constant declarations (const)

use crate::convert::ConvertedGlobal;
use crate::pattern::PatternResult;
use crate::{
    C2RError, ConvertedElement, ExtractedElement, Handler, HandlerPhase, HandlerResult, Id,
    ReportLevel,
};
use crate::{context, report};
use std::ops::Range;

/// Create global constant handler with specialized patterns
pub fn create_global_constant_handler() -> Handler {
    let handler_id = Id::get("global_constant_handler");
    let handler_role = "Convert C global constant declarations to Rust const items".to_string();
    let priority = 850; // High priority for specific pattern matching

    let mut handler = Handler::new(handler_id, handler_role, priority);
    handler.process = Some(process_global_constant);
    handler.handle = Some(handle_global_constant);
    handler.extract = Some(extract_global_constant);
    handler.convert = Some(convert_global_constant_wrapper);
    handler.document = Some(document_global_constant);
    handler.result = Some(result_global_constant);
    handler
}

/// Process callback: Detects global constant declarations (const keyword)
fn process_global_constant(token_range: Range<usize>) -> Result<bool, C2RError> {
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
    let pattern_result = context.patternizer.match_pattern("global_constant", tokens);

    Ok(matches!(
        pattern_result,
        PatternResult::Match { .. } | PatternResult::Sequence { .. } | PatternResult::Fuzzy { .. }
    ))
}

/// Handle callback: Processes global constant declarations
fn handle_global_constant(token_range: Range<usize>) -> Result<HandlerResult, C2RError> {
    let id = Id::get("global_constant_handler");

    report!(
        "global_constant_handler",
        "handle_global_constant",
        ReportLevel::Info,
        HandlerPhase::Handle,
        format!("Processing global constant in range {:?}", token_range),
        true
    );

    if let Ok(Some(converted)) = convert_global_constant_wrapper(token_range.clone()) {
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

/// Extract global constant information from tokens
fn extract_global_constant(
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
            name: "global_const".to_string(),
            array_dims: vec![],
            initial_value: None,
            is_const: true,
            is_static: false,
            tokens: tokens.to_vec(),
            is_extern: false,
            array_size: None,
            initializer: None,
            type_name: "int".to_string(),
            storage_class: Some("const".to_string()),
            code: "/* global constant */".to_string(),
        },
    )))
}

/// Convert global constants from token range
fn convert_global_constant_wrapper(
    token_range: Range<usize>,
) -> Result<Option<ConvertedElement>, C2RError> {
    let mut context = context!();
    context.pull();
    let tokens = &context.tokens[token_range.clone()];

    if tokens.is_empty() {
        return Ok(None);
    }

    let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

    // Find type (after const keyword)
    let const_pos = token_strings.iter().position(|s| s == "const");
    let var_type = if let Some(pos) = const_pos {
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

    // Find variable name (token before '=')
    let var_name = token_strings
        .iter()
        .position(|s| s == "=")
        .and_then(|pos| {
            if pos > 0 {
                token_strings.get(pos - 1)
            } else {
                None
            }
        })
        .cloned()
        .unwrap_or_else(|| "unknown_const".to_string());

    // Find initializer value (token after '=')
    let initializer = token_strings
        .iter()
        .position(|s| s == "=")
        .and_then(|pos| token_strings.get(pos + 1))
        .filter(|s| s != &";")
        .cloned()
        .unwrap_or_else(|| "0".to_string());

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

    let rust_code = format!(
        "const {}: {} = {};",
        var_name.to_uppercase(),
        rust_type,
        initializer
    );

    Ok(Some(ConvertedElement::Global(ConvertedGlobal {
        var_type: rust_type.to_string(),
        initializer: Some(initializer),
        code: rust_code,
        is_const: true,
        is_static: false,
        is_public: true,
    })))
}

/// Document global constant
fn document_global_constant(
    info: crate::document::ElementInfo,
) -> Result<Option<String>, C2RError> {
    if let crate::document::ElementInfo::Global(global_info) = info {
        let mut doc = format!("/// Converted C global constant: {}\n", global_info.name);
        doc.push_str(&format!(
            "/// Type: {} -> const {}\n",
            global_info.type_name, "compile-time constant"
        ));
        Ok(Some(doc))
    } else {
        Ok(None)
    }
}

/// Result callback for global constants
fn result_global_constant(
    _token_range: Range<usize>,
    result: HandlerResult,
) -> Result<HandlerResult, C2RError> {
    report!(
        "global_constant_handler",
        "result_global_constant",
        ReportLevel::Info,
        HandlerPhase::Report,
        "Postprocessing global constant conversion result",
        true
    );

    Ok(result)
}
