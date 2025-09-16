//! Extern Declaration Handler for C to Rust Converter
//! Specialized handler for extern variable and function declarations

use crate::convert::ConvertedGlobal;
use crate::pattern::PatternResult;
use crate::{
    C2RError, ConvertedElement, ExtractedElement, Handler, HandlerPhase, HandlerResult, Id,
    ReportLevel,
};
use crate::{context, report};
use std::ops::Range;

/// Create extern declaration handler with specialized patterns
pub fn create_extern_declaration_handler() -> Handler {
    let handler_id = Id::get("extern_declaration_handler");
    let handler_role = "Convert C extern declarations to Rust extern items".to_string();
    let priority = 880; // High priority for specific pattern matching

    let mut handler = Handler::new(handler_id, handler_role, priority);
    handler.process = Some(process_extern_declaration);
    handler.handle = Some(handle_extern_declaration);
    handler.extract = Some(extract_extern_declaration);
    handler.convert = Some(convert_extern_declaration_wrapper);
    handler.document = Some(document_extern_declaration);
    handler.result = Some(result_extern_declaration);
    handler
}

/// Process callback: Detects extern declarations (extern keyword)
fn process_extern_declaration(token_range: Range<usize>) -> Result<bool, C2RError> {
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
    let pattern_result = context
        .patternizer
        .match_pattern("extern_declaration", tokens);

    Ok(matches!(
        pattern_result,
        PatternResult::Match { .. } | PatternResult::Sequence { .. } | PatternResult::Fuzzy { .. }
    ))
}

/// Handle callback: Processes extern declarations
fn handle_extern_declaration(token_range: Range<usize>) -> Result<HandlerResult, C2RError> {
    let id = Id::get("extern_declaration_handler");

    report!(
        "extern_declaration_handler",
        "handle_extern_declaration",
        ReportLevel::Info,
        HandlerPhase::Handle,
        format!("Processing extern declaration in range {:?}", token_range),
        true
    );

    if let Ok(Some(converted)) = convert_extern_declaration_wrapper(token_range.clone()) {
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

/// Extract extern declaration information from tokens
fn extract_extern_declaration(
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
            name: "extern_item".to_string(),
            array_dims: vec![],
            initial_value: None,
            is_const: false,
            is_static: false,
            tokens: tokens.to_vec(),
            is_extern: true,
            array_size: None,
            initializer: None,
            type_name: "int".to_string(),
            storage_class: Some("extern".to_string()),
            code: "/* extern declaration */".to_string(),
        },
    )))
}

/// Convert extern declarations from token range
fn convert_extern_declaration_wrapper(
    token_range: Range<usize>,
) -> Result<Option<ConvertedElement>, C2RError> {
    let mut context = context!();
    context.pull();
    let tokens = &context.tokens[token_range.clone()];

    if tokens.is_empty() {
        return Ok(None);
    }

    let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

    // Check if this is a function declaration (has parentheses)
    let is_function = token_strings.contains(&"(".to_string());

    if is_function {
        // Handle extern function declaration
        let func_name = token_strings
            .iter()
            .position(|s| s == "(")
            .and_then(|pos| {
                if pos > 0 {
                    token_strings.get(pos - 1)
                } else {
                    None
                }
            })
            .cloned()
            .unwrap_or_else(|| "unknown_func".to_string());

        let rust_code = format!("extern \"C\" {{\n    fn {}();\n}}", func_name);

        Ok(Some(ConvertedElement::Global(ConvertedGlobal {
            var_type: "function".to_string(),
            initializer: None,
            code: rust_code,
            is_const: false,
            is_static: false,
            is_public: true,
        })))
    } else {
        // Handle extern variable declaration
        let extern_pos = token_strings.iter().position(|s| s == "extern");
        let var_type = if let Some(pos) = extern_pos {
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

        let var_name = token_strings
            .iter()
            .position(|s| s == ";")
            .and_then(|pos| {
                if pos > 0 {
                    token_strings.get(pos - 1)
                } else {
                    None
                }
            })
            .cloned()
            .unwrap_or_else(|| "unknown_extern".to_string());

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
            "extern \"C\" {{\n    static {}: {};\n}}",
            var_name, rust_type
        );

        Ok(Some(ConvertedElement::Global(ConvertedGlobal {
            var_type: rust_type.to_string(),
            initializer: None,
            code: rust_code,
            is_const: false,
            is_static: false,
            is_public: true,
        })))
    }
}

/// Document extern declaration
fn document_extern_declaration(
    info: crate::document::ElementInfo,
) -> Result<Option<String>, C2RError> {
    if let crate::document::ElementInfo::Global(global_info) = info {
        let mut doc = format!("/// Converted C extern declaration: {}\n", global_info.name);
        doc.push_str("/// External linkage in C -> extern block in Rust\n");
        doc.push_str("/// Provides interface to C libraries or other object files\n");
        Ok(Some(doc))
    } else {
        Ok(None)
    }
}

/// Result callback for extern declarations
fn result_extern_declaration(
    _token_range: Range<usize>,
    result: HandlerResult,
) -> Result<HandlerResult, C2RError> {
    report!(
        "extern_declaration_handler",
        "result_extern_declaration",
        ReportLevel::Info,
        HandlerPhase::Report,
        "Postprocessing extern declaration conversion result",
        true
    );

    Ok(result)
}
