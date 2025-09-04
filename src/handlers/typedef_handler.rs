use crate::config::{
    Context,
    HandlerPhase::{Convert, Extract, Handle, Process, Report},
    HandlerReport,
    ReportLevel::{Error, Info, Warning},
};
use crate::error::ConversionError;
use crate::handler::HandlerResult;
use crate::{context, report, ConvertedElement, ConvertedTypedef, Token};

use super::common::{not_handled, replace_with_range};
use crate::convert_type;
use crate::extract::ExtractedElement;
use crate::extract::ExtractedTypedef;
use crate::lock::Id;
use crate::Handler;

/// Creates a typedef handler that can detect and convert C typedef declarations
pub fn create_typedef_handler() -> Handler {
    let handler_id = Id::get("typedef_handler");
    let handler_role = "typedef";
    let priority = 100; // Standard priority

    super::create_handler(
        handler_id,
        handler_role,
        priority,
        Some(process_typedef),
        Some(handle_typedef),
        Some(extract_typedef),
        Some(convert_typedef),
        Some(report_typedef),
        Some(result_typedef),
        Some(redirect_typedef),
    )
}

/// Process callback: Initializes and confirms this handler can handle the tokens
fn process_typedef(tokens: &[Token]) -> Result<bool, ConversionError> {
    // Validate input
    if tokens.len() < 3 {
        return Ok(false);
    }

    // Check for "typedef" keyword at the beginning
    if tokens[0].to_string() != "typedef" {
        return Ok(false);
    }

    // Skip typedef struct/enum/union as those are handled by their respective handlers
    if tokens.len() >= 2 {
        let second_token = tokens[1].to_string();
        if second_token == "struct" || second_token == "enum" || second_token == "union" {
            report!(
                "typedef_handler",
                "process_typedef",
                Info,
                Process,
                format!(
                    "Typedef {} detected, redirecting to specialized handler",
                    second_token
                ),
                true
            );
            return Ok(false);
        }
    }

    // Check for function pointer typedefs
    if tokens.iter().any(|t| t.to_string().contains("(*")) {
        report!(
            "typedef_handler",
            "process_typedef",
            Info,
            Process,
            "Function pointer typedef detected",
            true
        );
        return Ok(true);
    }

    // Check for array typedefs
    if tokens.iter().any(|t| t.to_string() == "[") {
        report!(
            "typedef_handler",
            "process_typedef",
            Info,
            Process,
            "Array typedef detected",
            true
        );
        return Ok(true);
    }

    // Standard type alias
    report!(
        "typedef_handler",
        "process_typedef",
        Info,
        Process,
        "Standard typedef detected",
        true
    );
    Ok(true)
}

/// Processes a typedef declaration
fn handle_typedef(
    tokens: &[Token]) -> Result<HandlerResult, ConversionError> {
    let id = Id::get("handle_typedef");
    report!(
        "typedef_handler",
        "handle_typedef",
        Info,
        Handle,
        "Typedef handler processing tokens",
        true
    );

    if tokens[0].to_string() != "typedef" {
        return not_handled();
    }

    // Skip typedef struct/enum/union
    if tokens.len() >= 2 {
        let second_token = tokens[1].to_string();
        if second_token == "struct" || second_token == "enum" || second_token == "union" {
            return not_handled();
        }
    }

    // Find the semicolon at the end
    let mut semicolon_pos = tokens.len();
    for i in 0..tokens.len() {
        if tokens[i].to_string() == ";" {
            semicolon_pos = i;
            break;
        }
    }

    if semicolon_pos == tokens.len() {
        report!(
            "typedef_handler",
            "handle_typedef",
            Error,
            Handle,
            "Typedef missing semicolon",
            true
        );
        return not_handled();
    }

    // The name is the token just before the semicolon
    let name_pos = semicolon_pos - 1;
    if name_pos < 1 {
        report!(
            "typedef_handler",
            "handle_typedef",
            Error,
            Handle,
            "Typedef missing name",
            true
        );
        return not_handled();
    }

    let typedef_name = tokens[name_pos].to_string();
    report!(
        "typedef_handler",
        "handle_typedef",
        Info,
        Handle,
        format!("Found typedef: {}", typedef_name),
        true
    );

    // The source type is everything between "typedef" and the name
    let source_type_tokens = &tokens[1..name_pos];
    if source_type_tokens.is_empty() {
        report!(
            "typedef_handler",
            "handle_typedef",
            Error,
            Handle,
            "Typedef missing source type",
            true
        );
        return not_handled();
    }

    // Convert to Rust
    let rust_code = convert_typedef_to_rust(typedef_name.as_str(), source_type_tokens)?;

    // Typedef definitions process all input tokens
    let token_range = 0..tokens.len();
    replace_with_range(rust_code, token_range, id)
}

/// Extracts a typedef as an ExtractedElement
pub fn extract_typedef(
    tokens: &[Token]) -> Result<Option<ExtractedElement>, ConversionError> {
    if tokens.is_empty() || tokens[0].to_string() != "typedef" {
        return Ok(None);
    }

    // Skip typedef struct/enum/union as they're handled by their respective handlers
    if tokens.len() >= 2 {
        let second_token = tokens[1].to_string();
        if second_token == "struct" || second_token == "enum" || second_token == "union" {
            return Ok(None);
        }
    }

    // Find the semicolon at the end
    let semicolon_pos = tokens
        .iter()
        .position(|token| token.to_string() == ";")
        .unwrap_or(tokens.len());

    if semicolon_pos == tokens.len() {
        report!(
            "typedef_handler",
            "extract_typedef",
            Error,
            Extract,
            "Typedef missing semicolon",
            true
        );
        return Ok(None);
    }

    // The name is the token just before the semicolon
    let name_pos = semicolon_pos - 1;
    if name_pos < 1 {
        report!(
            "typedef_handler",
            "extract_typedef",
            Error,
            Extract,
            "Typedef missing name",
            true
        );
        return Ok(None);
    }

    let typedef_name = tokens[name_pos].to_string();
    report!(
        "typedef_handler",
        "extract_typedef",
        Info,
        Extract,
        format!("Found typedef: {}", typedef_name),
        true
    );

    // The source type is everything between "typedef" and the name
    let source_type_tokens = tokens[1..name_pos].to_vec();
    if source_type_tokens.is_empty() {
        report!(
            "typedef_handler",
            "extract_typedef",
            Error,
            Extract,
            "Typedef missing source type",
            true
        );
        return Ok(None);
    }

    // Extract the typedef
    let extracted_typedef = ExtractedTypedef {
        name: typedef_name,
        original_type: source_type_tokens.clone(),
        tokens: tokens[..=semicolon_pos].to_vec(),
    };

    Ok(Some(ExtractedElement::Typedef(extracted_typedef)))
}

/// Detects if tokens represent a function pointer typedef
pub fn is_function_pointer_typedef(tokens: &[Token]) -> bool {
    // Look for patterns like:
    // 1. tokens containing (*name) or (* name)
    // 2. tokens containing a ( after the name position

    // Find opening and closing parentheses
    let mut paren_positions = Vec::new();
    for (i, token) in tokens.iter().enumerate() {
        if token.to_string() == "(" {
            paren_positions.push(i);
        }
    }

    // Check for (*name) pattern
    for pos in &paren_positions {
        if *pos + 2 < tokens.len()
            && tokens[*pos + 1].to_string() == "*"
            && !crate::extract::is_c_qualifier_or_specifier(&tokens[*pos + 2])
        {
            return true;
        }
    }

    // Check for function signature with parentheses around name and parameters after
    for (i, token) in tokens.iter().enumerate() {
        // Find the closing parenthesis of a (*name) pattern
        if token.to_string() == ")" && i + 1 < tokens.len() && tokens[i + 1].to_string() == "(" {
            return true;
        }
    }

    false
}

/// Detects if tokens represent an array typedef
pub fn is_array_typedef(tokens: &[Token]) -> bool {
    // Look for patterns like:
    // 1. tokens containing [size] after the name

    // Check for square brackets
    for (i, token) in tokens.iter().enumerate() {
        if token.to_string() == "[" {
            // Make sure it's not at the start (needs a name before it)
            if i > 0 && !crate::extract::is_c_qualifier_or_specifier(&tokens[i - 1]) {
                return true;
            }
        }
    }

    false
}

/// Detects what kind of typedef we're dealing with to help choose the right conversion method
pub fn detect_typedef_kind(source_type_tokens: &[Token]) -> TypedefKind {
    if is_function_pointer_typedef(source_type_tokens) {
        TypedefKind::FunctionPointer
    } else if is_array_typedef(source_type_tokens) {
        TypedefKind::Array
    } else {
        TypedefKind::Simple
    }
}

/// Enum representing different kinds of typedefs for specialized handling
#[derive(Debug, Clone, PartialEq)]
pub enum TypedefKind {
    Simple,
    FunctionPointer,
    Array,
}

/// Converts a C typedef to Rust syntax
fn convert_typedef_to_rust(
    name: &str,
    source_type_tokens: &[Token]
) -> Result<String, ConversionError> {
    // Form the source type string
    let source_type = source_type_tokens
        .iter()
        .map(|t| t.to_string())
        .collect::<Vec<_>>()
        .join(" ");

    // Check for function pointer typedefs
    if source_type.contains("(*)") || source_type.contains("( * )") {
        return convert_function_pointer_typedef(name, source_type_tokens);
    }

    // Check if this is an array typedef
    if source_type.contains('[') && source_type.contains(']') {
        return convert_array_typedef(name, source_type_tokens);
    }

    // Standard type alias
    // Try to convert the source type using our conversion registry
    let rust_source_type = if let Some(rust_type) = convert_type(&source_type) {
        rust_type
    } else {
        // If not found in registry, keep it as is
        source_type
    };

    Ok(format!("pub type {} = {};\n", name, rust_source_type))
}

/// Converts a C function pointer typedef to Rust
fn convert_function_pointer_typedef(
    name: &str,
    source_type_tokens: &[Token]) -> Result<String, ConversionError> {
    // This is a simplified version - a real implementation would parse the function signature

    // Try to find the return type (everything before the first open parenthesis)
    let mut return_type_tokens = Vec::new();
    let mut i = 0;
    while i < source_type_tokens.len() && source_type_tokens[i].to_string() != "(" {
        return_type_tokens.push(source_type_tokens[i].clone());
        i += 1;
    }

    let return_type_str = return_type_tokens
        .iter()
        .map(|t| t.to_string())
        .collect::<Vec<_>>()
        .join(" ");

    // Convert return type
    let rust_return_type = if let Some(rt) = convert_type(&return_type_str) {
        rt
    } else {
        return_type_str
    };

    // Extract parameter types (simplified)
    let mut param_types = Vec::<String>::new();

    // Find the parameter list - looking for the pattern (*) (param_list)
    while i < source_type_tokens.len() {
        if source_type_tokens[i].to_string() == ")"
            && i + 1 < source_type_tokens.len()
            && source_type_tokens[i + 1].to_string() == "("
        {
            // Found the parameter list start
            i += 2; // Skip the ) and (
            let mut param_start = i;

            // Find parameter list end
            while i < source_type_tokens.len() && source_type_tokens[i].to_string() != ")" {
                if source_type_tokens[i].to_string() == "," {
                    // Parameter delimiter found
                    let param_type = source_type_tokens[param_start..i]
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<_>>()
                        .join(" ");

                    // Convert parameter type
                    let rust_param_type = if let Some(pt) = convert_type(&param_type) {
                        pt
                    } else {
                        param_type
                    };

                    param_types.push(rust_param_type);
                    param_start = i + 1;
                }
                i += 1;
            }

            // Add the last parameter
            if param_start < i {
                let param_type = source_type_tokens[param_start..i]
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(" ");

                // Convert parameter type
                let rust_param_type = if let Some(pt) = convert_type(&param_type) {
                    pt
                } else {
                    param_type
                };

                param_types.push(rust_param_type);
            }

            break;
        }
        i += 1;
    }

    // Build the Rust function pointer type
    let mut rust_code = format!("pub type {} = fn(", name);
    rust_code.push_str(&param_types.join(", "));

    if rust_return_type != "()" {
        rust_code.push_str(&format!(") -> {};\n", rust_return_type));
    } else {
        rust_code.push_str(");\n");
    }

    Ok(rust_code)
}

/// Converts a C array typedef to Rust
fn convert_array_typedef(
    name: &str,
    source_type_tokens: &[Token]) -> Result<String, ConversionError> {
    // Find the base type (everything before the first '[')
    let mut base_type_tokens = Vec::new();
    for token in source_type_tokens {
        if token.to_string() == "[" {
            break;
        }
        base_type_tokens.push(token.clone());
    }

    let base_type_str = base_type_tokens
        .iter()
        .map(|t| t.to_string())
        .collect::<Vec<_>>()
        .join(" ");

    // Convert base type
    let rust_base_type = if let Some(bt) = convert_type(&base_type_str) {
        bt
    } else {
        base_type_str
    };

    // Find array size
    let mut array_size = String::new(); // Default to slice if size not found
    for i in 0..source_type_tokens.len() {
        if source_type_tokens[i].to_string() == "["
            && i + 1 < source_type_tokens.len()
            && i + 2 < source_type_tokens.len()
            && source_type_tokens[i + 2].to_string() == "]"
        {
            array_size = source_type_tokens[i + 1].to_string();
            break;
        }
    }

    let rust_code = if array_size.is_empty() {
        // Use slice for unknown size
        format!("pub type {} = [{}];\n", name, rust_base_type)
    } else {
        // Use array with specific size
        format!(
            "pub type {} = [{}; {}];\n",
            name, rust_base_type, array_size
        )
    };

    Ok(rust_code)
}

/// Convert callback: Does the actual conversion of C to Rust code
fn convert_typedef(
    tokens: &[Token]) -> Result<Option<ConvertedElement>, ConversionError> {
    report!(
        "typedef_handler",
        "convert_typedef",
        Info,
        Convert,
        format!("Converting typedef from {} tokens", tokens.len()),
        true
    );

    if let Some(ExtractedElement::Typedef(extracted_typedef)) =
        extract_typedef(tokens).unwrap_or_default()
    {
        let rust_code = convert_typedef_to_rust(
            &extracted_typedef.name,
            &extracted_typedef.original_type,
        )?;

        let id = Id::get("convert_typedef");
        Ok(Some(ConvertedElement::Typedef(ConvertedTypedef {
            target_type: "".to_string(), // Default empty target type
            rust_code,
            is_public: false,
        })))
    } else {
        Err(ConversionError::new(
            "Could not extract typedef for conversion",
        ))
    }
}

/// Redirect callback: Handles cases where this handler should pass tokens to a different handler
fn redirect_typedef(
    tokens: &[Token],
    result: HandlerResult) -> Result<HandlerResult, ConversionError> {
    let id = Id::get("redirect_typedef");
    report!(
        "typedef_handler",
        "redirect_typedef",
        Info,
        Report,
        "Checking if typedef tokens should be redirected",
        true
    );

    // Check if this is actually typedef struct/enum/union
    if tokens.len() >= 3 && tokens[0].to_string() == "typedef" {
        match tokens[1].to_string().as_str() {
            "struct" => {
                report!(
                    "typedef_handler",
                    "redirect_typedef",
                    Info,
                    Report,
                    "Redirecting to struct handler (typedef struct)",
                    true
                );
                return Ok(HandlerResult::Redirected(
                    Some(tokens.to_vec()),
                    0..1,
                    "typedef_handler".to_string(),
                    id,
                    Id::get("struct_handler"),
                ));
            }
            "enum" => {
                report!(
                    "typedef_handler",
                    "redirect_typedef",
                    Info,
                    Report,
                    "Redirecting to enum handler (typedef enum)",
                    true
                );
                return Ok(HandlerResult::Redirected(
                    Some(tokens.to_vec()),
                    0..1,
                    "typedef_handler".to_string(),
                    id,
                    Id::get("enum_handler"),
                ));
            }
            "union" => {
                report!(
                    "typedef_handler",
                    "redirect_typedef",
                    Info,
                    Report,
                    "Redirecting to union handler (typedef union)",
                    true
                );
                return Ok(HandlerResult::Redirected(
                    Some(tokens.to_vec()),
                    0..1,
                    "typedef_handler".to_string(),
                    id,
                    Id::get("union_handler"),
                ));
            }
            _ => {}
        }
    }

    // Check if this contains function declarations that should go to function handler
    if tokens.iter().any(|t| t.to_string().contains("("))
        && tokens.iter().any(|t| t.to_string().contains(")"))
        && !tokens.iter().any(|t| t.to_string().contains("(*"))
    {
        // This might be a function declaration masquerading as typedef
        report!(
            "typedef_handler",
            "redirect_typedef",
            Info,
            Report,
            "Possible function declaration, redirecting to function handler",
            true
        );
        return Ok(HandlerResult::Redirected(
            Some(tokens.to_vec()),
            0..1,
            "typedef_handler".to_string(),
            id,
            Id::get("function_handler"),
        ));
    }

    // Check if this is actually a variable declaration
    if tokens.iter().any(|t| t.to_string() == "=") {
        report!(
            "typedef_handler",
            "redirect_typedef",
            Info,
            Report,
            "Variable assignment detected, redirecting to global handler",
            true
        );
        return Ok(HandlerResult::Redirected(
            Some(tokens.to_vec()),
            0..1,
            "typedef_handler".to_string(),
            id,
            Id::get("global_handler"),
        ));
    }

    // No redirection needed
    Ok(result)
}

/// Result callback: Postprocesses generated typedef code, adds documentation, and enhances formatting
fn result_typedef(
    tokens: &[Token],
    result: HandlerResult) -> Result<HandlerResult, ConversionError> {
    let _id = Id::get("result_typedef");

    report!(
        "typedef_handler",
        "result_typedef",
        Info,
        Report,
        "Postprocessing typedef conversion result",
        true
    );

    match result {
        HandlerResult::Completed(tokens_opt, _, rust_code, id) => {
            // Extract typedef information for documentation
            let typedef_info = extract_typedef_info_from_tokens(tokens);

            // Generate documentation about the typedef conversion
            let doc_comment = generate_typedef_documentation(tokens, &typedef_info);

            // Enhance the Rust code with documentation and metadata
            let mut enhanced_code = String::new();

            // Add metadata comment for traceability
            let metadata_comment = format!(
                "// [C2R] Typedef converted from C to Rust - {}: {}\n",
                typedef_info.name, typedef_info.kind_description
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
                "typedef_handler",
                "result_typedef",
                Info,
                Report,
                &format!(
                    "Enhanced typedef conversion: {} ({})",
                    typedef_info.name, typedef_info.kind_description
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
            let typedef_info = extract_typedef_info_from_tokens(tokens);
            let doc_comment = generate_typedef_documentation(tokens, &typedef_info);

            let mut enhanced_code = String::new();
            let metadata_comment = format!(
                "// [C2R] Typedef converted from C to Rust - {}: {}\n",
                typedef_info.name, typedef_info.kind_description
            );
            enhanced_code.push_str(&metadata_comment);

            if !doc_comment.trim().is_empty() {
                enhanced_code.push_str(&doc_comment);
                enhanced_code.push('\n');
            }
            enhanced_code.push_str(&rust_code);

            report!(
                "typedef_handler",
                "result_typedef",
                Info,
                Report,
                &format!(
                    "Enhanced converted typedef: {} ({})",
                    typedef_info.name, typedef_info.kind_description
                ),
                true
            );

            // CRITICAL: Return HandlerResult::Converted to preserve the code!
            Ok(HandlerResult::Converted(element, 0..1, enhanced_code, id))
        }
        HandlerResult::Extracted(element, _, rust_code, id) => {
            // Handle extracted elements - enhance the code and preserve the variant
            let typedef_info = extract_typedef_info_from_tokens(tokens);
            let doc_comment = generate_typedef_documentation(tokens, &typedef_info);

            let mut enhanced_code = String::new();
            let metadata_comment = format!(
                "// [C2R] Typedef converted from C to Rust - {}: {}\n",
                typedef_info.name, typedef_info.kind_description
            );
            enhanced_code.push_str(&metadata_comment);

            if !doc_comment.trim().is_empty() {
                enhanced_code.push_str(&doc_comment);
                enhanced_code.push('\n');
            }
            enhanced_code.push_str(&rust_code);

            report!(
                "typedef_handler",
                "result_typedef",
                Info,
                Report,
                &format!(
                    "Enhanced extracted typedef: {} ({})",
                    typedef_info.name, typedef_info.kind_description
                ),
                true
            );

            // CRITICAL: Return HandlerResult::Extracted to preserve the code!
            Ok(HandlerResult::Extracted(element, 0..1, enhanced_code, id))
        }
        HandlerResult::Handled(Some(converted_tokens), _, handler_id) => {
            // Legacy support for token-based results
            // Extract typedef information for documentation
            let typedef_info = extract_typedef_info_from_tokens(tokens);

            // Generate documentation about the typedef conversion
            let doc_comment = generate_typedef_documentation(tokens, &typedef_info);

            // Postprocess the converted Rust code for better formatting
            let mut enhanced_result = postprocess_typedef_code(converted_tokens);

            // Add the documentation comment before the converted typedef
            if !doc_comment.is_empty() {
                enhanced_result.insert(0, Token::s(doc_comment));
            }

            // Add metadata comment for traceability
            let metadata_comment = format!(
                "// [C2R] Typedef converted from C to Rust - {}: {}",
                typedef_info.name, typedef_info.kind_description
            );
            enhanced_result.insert(0, Token::s(metadata_comment));

            report!(
                "typedef_handler",
                "result_typedef",
                Info,
                Report,
                &format!(
                    "Enhanced typedef conversion: {} ({})",
                    typedef_info.name, typedef_info.kind_description
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
                "typedef_handler",
                "result_typedef",
                Warning,
                Report,
                "Typedef handler returned empty result",
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

/// Typedef information extracted for documentation purposes
#[derive(Debug, Clone)]
struct TypedefInfo {
    name: String,
    kind: TypedefKind,
    kind_description: String,
    source_type: String,
    is_function_pointer: bool,
    is_array: bool,
    complexity: String,
}

/// Extracts typedef information from the original tokens for documentation purposes
fn extract_typedef_info_from_tokens(tokens: &[Token]) -> TypedefInfo {
    if tokens.is_empty() || tokens[0].to_string() != "typedef" {
        return TypedefInfo {
            name: "unknown".to_string(),
            kind: TypedefKind::Simple,
            kind_description: "unknown typedef".to_string(),
            source_type: "unknown".to_string(),
            is_function_pointer: false,
            is_array: false,
            complexity: "unknown".to_string(),
        };
    }

    // Find the semicolon and extract name
    let semicolon_pos = tokens
        .iter()
        .position(|t| t.to_string() == ";")
        .unwrap_or(tokens.len());
    let name_pos = if semicolon_pos > 1 {
        semicolon_pos - 1
    } else {
        1
    };
    let typedef_name = if name_pos < tokens.len() {
        tokens[name_pos].to_string()
    } else {
        "unnamed".to_string()
    };

    // Extract source type tokens
    let source_type_tokens = if tokens.len() > 1 && name_pos > 1 {
        &tokens[1..name_pos]
    } else {
        &[]
    };

    let source_type = source_type_tokens
        .iter()
        .map(|t| t.to_string())
        .collect::<Vec<_>>()
        .join(" ");

    // Determine typedef kind
    let kind = detect_typedef_kind(source_type_tokens);
    let is_function_pointer = matches!(kind, TypedefKind::FunctionPointer);
    let is_array = matches!(kind, TypedefKind::Array);

    let (kind_description, complexity) = match kind {
        TypedefKind::Simple => ("simple type alias".to_string(), "simple".to_string()),
        TypedefKind::FunctionPointer => {
            // Count parameters for complexity
            let param_count = source_type.matches(',').count() + 1;
            (
                "function pointer typedef".to_string(),
                if param_count <= 1 {
                    "simple".to_string()
                } else {
                    format!("moderate ({} parameters)", param_count)
                },
            )
        }
        TypedefKind::Array => {
            // Check if it has explicit size
            let has_size = source_type.contains('[') && !source_type.contains("[]");
            (
                "array typedef".to_string(),
                if has_size {
                    "simple (fixed size)".to_string()
                } else {
                    "moderate (dynamic size)".to_string()
                },
            )
        }
    };

    TypedefInfo {
        name: typedef_name,
        kind,
        kind_description,
        source_type,
        is_function_pointer,
        is_array,
        complexity,
    }
}

/// Generates documentation comments for the typedef conversion
fn generate_typedef_documentation(
    tokens: &[Token],
    typedef_info: &TypedefInfo) -> String {
    let mut doc_lines = Vec::new();

    // Add main documentation header
    doc_lines.push("/**".to_string());
    doc_lines.push(" * Typedef Conversion Documentation".to_string());
    doc_lines.push(" *".to_string());

    // Add typedef information
    doc_lines.push(format!(" * Name: {}", typedef_info.name));
    doc_lines.push(format!(" * Kind: {}", typedef_info.kind_description));
    doc_lines.push(format!(" * Source type: {}", typedef_info.source_type));
    doc_lines.push(format!(" * Complexity: {}", typedef_info.complexity));
    doc_lines.push(format!(" * Original tokens: {}", tokens.len()));

    // Add features information
    let mut features = Vec::new();
    if typedef_info.is_function_pointer {
        features.push("function pointer");
    }
    if typedef_info.is_array {
        features.push("array type");
    }

    if !features.is_empty() {
        doc_lines.push(format!(" * Features: {}", features.join(", ")));
    }

    doc_lines.push(" *".to_string());

    // Add conversion notes based on typedef kind
    match typedef_info.kind {
        TypedefKind::Simple => {
            doc_lines.push(" * Conversion: C typedef -> Rust type alias".to_string());
            doc_lines.push(" * Features:".to_string());
            doc_lines.push(" *   - Direct type mapping with pub visibility".to_string());
            doc_lines.push(" *   - Zero-cost abstraction in Rust".to_string());
            doc_lines.push(" *   - Maintains type safety and compatibility".to_string());
            doc_lines.push(" *   - Automatic type conversion via registry".to_string());
        }
        TypedefKind::FunctionPointer => {
            doc_lines.push(
                " * Conversion: C function pointer typedef -> Rust function pointer type"
                    .to_string(),
            );
            doc_lines.push(" * Features:".to_string());
            doc_lines.push(" *   - Type-safe function pointer with explicit signature".to_string());
            doc_lines.push(" *   - Automatic parameter and return type conversion".to_string());
            doc_lines.push(" *   - Memory-safe function call indirection".to_string());
            doc_lines.push(" *   - Compatible with Rust closure systems".to_string());
            doc_lines.push(" *   - No null function pointer issues".to_string());
        }
        TypedefKind::Array => {
            doc_lines.push(" * Conversion: C array typedef -> Rust array type".to_string());
            doc_lines.push(" * Features:".to_string());
            doc_lines.push(" *   - Compile-time size checking for fixed arrays".to_string());
            doc_lines.push(" *   - Slice types for dynamic arrays".to_string());
            doc_lines.push(" *   - Automatic bounds checking in Rust".to_string());
            doc_lines.push(" *   - Memory safety with no buffer overflows".to_string());
            doc_lines.push(" *   - Iterator support for functional programming".to_string());
        }
    }

    doc_lines.push(" *".to_string());
    doc_lines.push(" * Rust typedef benefits:".to_string());
    doc_lines.push(" *   - Strong type system prevents type confusion".to_string());
    doc_lines.push(" *   - Zero-cost abstractions with compile-time optimization".to_string());
    doc_lines.push(" *   - Automatic memory management without garbage collection".to_string());
    doc_lines.push(" *   - Trait system enables powerful type extensions".to_string());
    doc_lines.push(" */".to_string());

    report!(
        "typedef_handler",
        "generate_typedef_documentation",
        Info,
        Report,
        &format!(
            "Generated documentation for {} typedef: {}",
            typedef_info.kind_description, typedef_info.name
        ),
        true
    );

    doc_lines.join("\n")
}

/// Postprocesses the converted typedef code for better formatting
fn postprocess_typedef_code(mut tokens: Vec<Token>) -> Vec<Token> {
    let original_count = tokens.len();

    // Clean up and format the converted typedef tokens
    for token in tokens.iter_mut() {
        let mut content = token.to_string();

        // Clean up extra whitespace
        content = content.trim().to_string();

        // Format type alias declarations
        if content.starts_with("pub type") {
            // Ensure proper spacing in type declarations
            content = content.replace("pub type", "pub type ");
            content = content.replace(" = ", " = ");
            content = content.replace("  ", " ");
        }

        // Format function pointer types
        if content.contains("fn(") {
            // Clean up function signature formatting
            content = content.replace("fn (", "fn(");
            content = content.replace("fn( ", "fn(");
            content = content.replace(" )", ")");
            content = content.replace("( ", "(");
            content = content.replace(" ,", ",");
            content = content.replace(",", ", ");
            content = content.replace("  ", " ");

            // Format return types
            content = content.replace(" -> ", " -> ");
            content = content.replace("->  ", "-> ");
            content = content.replace("  ->", " ->");
        }

        // Format array types
        if content.contains('[') && content.contains(']') {
            // Clean up array type formatting
            content = content.replace("[ ", "[");
            content = content.replace(" ]", "]");
            content = content.replace(" ;", ";");
            content = content.replace(";  ", "; ");
            content = content.replace("  ;", "; ");
        }

        // Format generic brackets if present
        if content.contains('<') && content.contains('>') {
            content = content.replace("< ", "<");
            content = content.replace(" >", ">");
            content = content.replace(" ,", ",");
        }

        // Ensure proper semicolon formatting
        if content.ends_with(';') {
            content = content.trim_end_matches(' ').to_string();
            if !content.ends_with(';') {
                content.push(';');
            }
        }

        // Clean up multiple spaces
        while content.contains("  ") {
            content = content.replace("  ", " ");
        }

        *token = Token::s(content);
    }

    // Remove any empty tokens
    tokens.retain(|token| !token.to_string().trim().is_empty());

    report!(
        "typedef_handler",
        "postprocess_typedef_code",
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
fn report_typedef(
    _tokens: &[Token]) -> Result<HandlerReport, ConversionError> {
    let context = context!();
    let handler_reports = context.get_reports_by_handler("typedef_handler");

    // Count reports by level
    let info_count = handler_reports
        .iter()
        .filter(|r| matches!(r.level, crate::config::ReportLevel::Info))
        .count();
    let error_count = handler_reports
        .iter()
        .filter(|r| matches!(r.level, crate::config::ReportLevel::Error))
        .count();
    let warning_count = handler_reports
        .iter()
        .filter(|r| matches!(r.level, crate::config::ReportLevel::Warning))
        .count();

    // Create summary message
    let summary_message = format!(
        "Typedef Handler Summary: {} total reports (Info: {}, Warnings: {}, Errors: {})",
        handler_reports.len(),
        info_count,
        warning_count,
        error_count
    );

    // Create consolidated report using gen_name function for unique report_id
    let summary_report = HandlerReport {
        report_id: Box::new(Id::get(&crate::Id::gen_name("typedef_handler_summary"))),
        handler_id: Box::new(Id::get("typedef_handler")),
        handler_name: "typedef_handler".to_string(),
        function_name: "report_typedef".to_string(),
        level: crate::config::ReportLevel::Info,
        phase: crate::config::HandlerPhase::Report,
        message: summary_message,
        success: error_count == 0, // Success if no errors
        tokens_processed: handler_reports.iter().map(|r| r.tokens_processed).sum(),
        tokens_consumed: handler_reports.iter().map(|r| r.tokens_consumed).sum(),
        metadata: std::collections::HashMap::new(),
    };

    Ok(summary_report)
}
