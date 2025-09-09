#![allow(
    unused_variables,
    unused_assignments,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals
)]

use super::common::{find_matching_token, not_handled, replace_with_range};
use crate::error::ConversionError;
use crate::handler::HandlerResult;
use crate::lock::Id;
use crate::{
    ConvertedElement, ConvertedStruct, ExtractedElement, ExtractedStruct, Token, context,
    convert_type_tokens, report,
};
use crate::{HandlerPhase, ReportLevel};
use crate::{
    HandlerPhase::{Extract, Handle, Process, Report},
    HandlerReport,
    ReportLevel::{Error, Info, Warning},
};
use std::str::FromStr;

/// Creates a struct handler that can detect and convert C structs
pub fn create_struct_handler() -> crate::handler::Handler {
    let handler_id = Id::get("struct_handler");
    let handler_role = "struct";
    let priority = 100; // Standard priority

    super::create_handler(
        handler_id,
        handler_role,
        priority,
        Some(process_struct),
        Some(handle_struct),
        Some(extract_struct),
        Some(convert_struct),
        Some(report_struct),
        Some(result_struct),
        Some(redirect_struct),
    )
}

/// Process callback: Initializes and confirms this handler can handle the tokens
fn process_struct(tokens: &[Token]) -> Result<bool, ConversionError> {
    // Validate input
    if tokens.len() < 2 {
        return Ok(false);
    }

    // Look for struct keyword patterns
    if tokens[0].to_string() == "struct" {
        // struct name { ... }
        if tokens.len() >= 3 && tokens[2].to_string() == "{" {
            report!(
                "struct_handler",
                "process_struct",
                Info,
                Process,
                format!("Struct definition detected: {}", tokens[1].to_string()),
                true
            );
            return Ok(true);
        }

        // struct name;
        if tokens.len() >= 3 && tokens[2].to_string() == ";" {
            report!(
                "struct_handler",
                "process_struct",
                Info,
                Process,
                format!(
                    "Struct forward declaration detected: {}",
                    tokens[1].to_string()
                ),
                true
            );
            return Ok(true);
        }

        // typedef struct { ... } name;
        if tokens.len() >= 4 {
            for i in 2..tokens.len() {
                if tokens[i].to_string() == "{" {
                    report!(
                        "struct_handler",
                        "process_struct",
                        Info,
                        Process,
                        "Typedef struct detected",
                        true
                    );
                    return Ok(true);
                }
            }
        }
    }

    // Check for typedef struct patterns
    if tokens.len() >= 3 && tokens[0].to_string() == "typedef" && tokens[1].to_string() == "struct"
    {
        report!(
            "struct_handler",
            "process_struct",
            Info,
            Process,
            "Typedef struct pattern detected",
            true
        );
        return Ok(true);
    }

    Ok(false)
}

/// Processes a struct declaration
fn handle_struct(tokens: &[Token]) -> Result<HandlerResult, ConversionError> {
    let id = Id::get("handle_struct");
    report!(
        "struct_handler",
        "handle_struct",
        Info,
        Handle,
        "Struct handler processing tokens",
        true
    );

    if tokens[0].to_string() != "struct"
        && !(tokens.len() >= 2
            && tokens[0].to_string() == "typedef"
            && tokens[1].to_string() == "struct")
    {
        return not_handled();
    }

    let is_typedef = tokens[0].to_string() == "typedef";
    let struct_token_idx = if is_typedef { 1 } else { 0 };

    // Extract the struct name
    let name_idx = struct_token_idx + 1;
    if name_idx >= tokens.len() {
        report!(
            "struct_handler",
            "handle_struct",
            Error,
            Handle,
            "Struct missing name",
            true
        );
        return not_handled();
    }

    let struct_name = tokens[name_idx].to_string();
    report!(
        "struct_handler",
        "handle_struct",
        Info,
        Handle,
        format!("Found struct: {}", struct_name),
        true
    );

    // Look for the opening brace
    let mut brace_idx = name_idx + 1;
    while brace_idx < tokens.len() && tokens[brace_idx].to_string() != "{" {
        brace_idx += 1;
    }

    if brace_idx >= tokens.len() {
        // This might be a forward declaration
        return if tokens[name_idx + 1].to_string() == ";" {
            // Forward declaration
            report!(
                "struct_handler",
                "handle_struct",
                Info,
                Handle,
                format!("Forward declaration of struct {}", struct_name),
                true
            );
            // Forward declarations process all input tokens
            let token_range = 0..tokens.len();
            replace_with_range(
                format!("// Forward declaration\npub struct {};\n", struct_name),
                token_range,
                id,
            )
        } else {
            report!(
                "struct_handler",
                "handle_struct",
                Error,
                Handle,
                format!("Could not find opening brace for struct {}", struct_name),
                true
            );
            not_handled()
        };
    }

    // Find the closing brace
    let body_start = brace_idx;
    let body_end = match find_matching_token(&tokens[body_start..], "{", "}") {
        Some(pos) => body_start + pos + 1,
        None => {
            report!(
                "struct_handler",
                "handle_struct",
                Error,
                Handle,
                format!("Could not find closing brace for struct {}", struct_name),
                true
            );
            return not_handled();
        }
    };

    // Extract body tokens (excluding braces)
    let body_tokens = &tokens[body_start + 1..body_end - 1];

    // Look for typedef name (if this is a typedef struct)
    let mut typedef_name = None;
    if is_typedef && body_end + 1 < tokens.len() {
        // The typedef name would be after the closing brace and before the semicolon
        for i in body_end..tokens.len() {
            if tokens[i].to_string() == ";" {
                if i > body_end {
                    typedef_name = Some(tokens[i - 1].to_string());
                }
                break;
            }
        }
    }

    // Convert to Rust
    if let Some(ConvertedElement::Struct(element)) = convert_struct(tokens)? {
        let rust_code = element.rust_code.clone();
        // Struct definitions process all input tokens
        let token_range = 0..tokens.len();
        replace_with_range(rust_code, token_range, id)
    } else {
        Err(ConversionError::new("Unsupported struct element"))
    }
}

/// Extracts a struct as an ExtractedElement
pub fn extract_struct(tokens: &[Token]) -> Result<Option<ExtractedElement>, ConversionError> {
    let is_typedef = tokens[0].to_string() == "typedef";
    let struct_token_idx = if is_typedef { 1 } else { 0 };

    // Extract the struct name
    let name_idx = struct_token_idx + 1;
    if name_idx >= tokens.len() || tokens[name_idx].to_string() == "{" {
        // Anonymous struct
        report!(
            "struct_handler",
            "extract_struct",
            Info,
            Extract,
            "Anonymous struct found",
            true
        );
        return Ok(None);
    }

    let struct_name = tokens[name_idx].to_string();
    report!(
        "struct_handler",
        "extract_struct",
        Info,
        Extract,
        format!("Found struct: {}", struct_name),
        true
    );

    // Look for the opening brace
    let mut brace_idx = name_idx + 1;
    while brace_idx < tokens.len() && tokens[brace_idx].to_string() != "{" {
        brace_idx += 1;
    }

    if brace_idx >= tokens.len() {
        // This might be a forward declaration
        return if name_idx + 1 < tokens.len() && tokens[name_idx + 1].to_string() == ";" {
            // Forward declaration
            report!(
                "struct_handler",
                "extract_struct",
                Info,
                Extract,
                format!("Forward declaration of struct {}", struct_name),
                true
            );
            let extracted_struct = ExtractedStruct {
                name: struct_name,
                fields: vec![],
                tokens: tokens.to_vec(),
                is_typedef,
                typedef_name: None,
                is_forward_declaration: true,
                original_code: tokens
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(" "),
            };

            Ok(Some(ExtractedElement::Struct(extracted_struct)))
        } else {
            report!(
                "struct_handler",
                "extract_struct",
                Error,
                Extract,
                format!("Could not find opening brace for struct {}", struct_name),
                true
            );
            Ok(None)
        };
    }

    // Find the closing brace
    let body_start = brace_idx;
    let body_end = match find_matching_token(&tokens[body_start..], "{", "}") {
        Some(pos) => body_start + pos + 1,
        None => return Ok(None),
    };

    // Extract body tokens (excluding braces)
    let body_tokens = &tokens[body_start + 1..body_end - 1];

    // Look for typedef name (if this is a typedef struct)
    let mut typedef_name = None;
    if is_typedef && body_end + 1 < tokens.len() {
        for i in body_end..tokens.len() {
            if tokens[i].to_string() == ";" {
                if i > body_end {
                    typedef_name = Some(tokens[i - 1].to_string());
                }
                break;
            }
        }
    }

    // Extract fields
    let mut fields = Vec::new();
    let mut i = 0;

    while i < body_tokens.len() {
        // Find the semicolon terminating this field
        let mut end = i;
        while end < body_tokens.len() && body_tokens[end].to_string() != ";" {
            end += 1;
        }

        if end >= body_tokens.len() {
            break; // No semicolon found
        }

        // Field declaration is from i to end
        let field_tokens = &body_tokens[i..end];

        if let Some((field_name, field_type)) = parse_field_declaration(field_tokens) {
            fields.push((field_name, field_type));
        } else {
            report!(
                "struct_handler",
                "extract_struct",
                Error,
                Extract,
                format!("Could not parse field declaration: {:?}", field_tokens),
                true
            );
        }

        // Move to next field
        i = end + 1;
    }

    let extracted_struct = ExtractedStruct {
        name: struct_name,
        fields,
        tokens: tokens.to_vec(),
        is_typedef,
        typedef_name,
        is_forward_declaration: false,
        original_code: tokens
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<_>>()
            .join(" "),
    };

    Ok(Some(ExtractedElement::Struct(extracted_struct)))
}

/// Parse a field declaration into name and type
fn parse_field_declaration(tokens: &[Token]) -> Option<(String, Vec<Token>)> {
    if tokens.is_empty() {
        return None;
    }

    // Handle bit field
    if let Some(bit_field) = parse_bit_field(tokens) {
        return Some(bit_field);
    }

    // Handle array field
    if let Some(array_field) = parse_array_field(tokens) {
        return Some(array_field);
    }

    // Handle pointer field
    if let Some(pointer_field) = parse_pointer_field(tokens) {
        return Some(pointer_field);
    }

    // Regular field - name is the last token before any special characters
    let mut field_name_pos = tokens.len() - 1;

    // Skip potential size/qualifier tokens
    while field_name_pos > 0 {
        let token = &tokens[field_name_pos];
        if token.to_string() == ":"
            || token.to_string() == "["
            || crate::extract::is_c_qualifier_or_specifier(token)
        {
            field_name_pos -= 1;
        } else {
            break;
        }
    }

    if field_name_pos == 0 || field_name_pos >= tokens.len() {
        return None;
    }

    let field_name = tokens[field_name_pos].to_string();

    // Field type is everything before the name
    let field_type = if field_name_pos > 0 {
        tokens[0..field_name_pos].to_vec()
    } else {
        vec![]
    };

    Some((field_name, field_type))
}

/// Parse array field declaration
fn parse_array_field(tokens: &[Token]) -> Option<(String, Vec<Token>)> {
    // Look for patterns like: "int array[10];"

    for (i, token) in tokens.iter().enumerate() {
        if token.to_string() == "[" {
            if i == 0 {
                continue; // Can't be at the start
            }

            let field_name = tokens[i - 1].to_string();

            // Find the closing bracket
            let mut close_pos = i + 1;
            while close_pos < tokens.len() && tokens[close_pos].to_string() != "]" {
                close_pos += 1;
            }

            if close_pos >= tokens.len() {
                return None; // No closing bracket found
            }

            // Field type is everything before the name
            let field_type = if i > 1 {
                let mut type_tokens = tokens[0..i - 1].to_vec();

                // Add array size information to the type tokens
                type_tokens.push(tokens[i].clone()); // [
                for j in i + 1..=close_pos {
                    type_tokens.push(tokens[j].clone());
                }

                type_tokens
            } else {
                vec![]
            };

            return Some((field_name, field_type));
        }
    }

    None
}

/// Parse pointer field declaration
fn parse_pointer_field(tokens: &[Token]) -> Option<(String, Vec<Token>)> {
    // Look for patterns like: "int *ptr;" or "int* ptr;"

    for (i, token) in tokens.iter().enumerate() {
        if token.to_string() == "*" && i + 1 < tokens.len() {
            // Field name is the token after the *
            let field_name = tokens[i + 1].to_string();

            // Field type is everything before the * plus the *
            let mut field_type = tokens[0..i].to_vec();
            field_type.push(token.clone());

            return Some((field_name, field_type));
        }
    }

    // Also check for name immediately following * without spaces
    for (i, token) in tokens.iter().enumerate() {
        if i > 0 && tokens[i - 1].to_string() == "*" {
            // Previous token was *, this might be the name
            let field_name = token.to_string();

            // Field type is everything before the name
            let field_type = tokens[0..i].to_vec();

            return Some((field_name, field_type));
        }
    }

    None
}

/// Parse bit field declaration
fn parse_bit_field(tokens: &[Token]) -> Option<(String, Vec<Token>)> {
    // Look for patterns like: "unsigned int field : 4;"

    for (i, token) in tokens.iter().enumerate() {
        if token.to_string() == ":" && i > 0 && i + 1 < tokens.len() {
            // Field name is before the colon
            let field_name = tokens[i - 1].to_string();

            // Field type is everything before the name, and we'll include the bit width info
            let mut field_type = if i > 1 {
                tokens[0..i - 1].to_vec()
            } else {
                vec![]
            };

            // Add bit width information
            field_type.push(token.clone()); // :
            field_type.push(tokens[i + 1].clone()); // bit width

            return Some((field_name, field_type));
        }
    }

    None
}

/// Legacy conversion function
fn convert_struct(tokens: &[Token]) -> Result<Option<ConvertedElement>, ConversionError> {
    // Get the TokenConverter from the Context
    let id = Id::get("convert_struct");
    if let ExtractedElement::Struct(element) = extract_struct(tokens)?.unwrap() {
        let mut rust_code = String::new();
        // Add documentation comment if available
        if let Some(docs) = super::common::extract_docs(element.tokens.as_slice()) {
            rust_code.push_str("/// ");
            rust_code.push_str(&docs.replace("\n", "\n/// "));
            rust_code.push_str("\n");
        }

        // Add struct declaration
        rust_code.push_str("#[derive(Debug, Clone, PartialEq)]\n");

        // Use typedef name if available, otherwise use the struct name
        let display_name = element.typedef_name.clone().unwrap_or(element.name.clone());

        rust_code.push_str(&format!("pub struct {} {{\n", display_name));

        // Convert fields (simplified)

        for (field, field_type_tokens) in element.fields {
            // Try to convert field type using TokenConverter
            let rust_type = if let Some(converted) = convert_type_tokens(field_type_tokens.clone())
            {
                converted
            } else {
                // Fallback to string-based conversion
                // Convert field type to Rust
                let field_type = field_type_tokens
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(" ");

                // Try to convert using the type registry
                let converted = Token::from_str(&field_type);
                converted?
            };

            rust_code.push_str(&format!("    pub {}: {},\n", field, rust_type));
        }

        rust_code.push_str("}\n");

        // If this is a typedef struct, add a type alias
        if element.typedef_name.is_some()
            && element.typedef_name.clone().unwrap() != element.name.clone()
        {
            rust_code.push_str(&format!(
                "pub type {} = {};\n",
                element.name.clone(),
                element.typedef_name.unwrap()
            ));
        }

        Ok(Some(ConvertedElement::Struct(ConvertedStruct {
            fields: Vec::new(), // Default empty fields
            rust_code,
            is_public: false,
            derives: Vec::new(),
        })))
    } else {
        Err(ConversionError::new("Unsupported struct element"))
    }
}

/// Redirect callback: Handles cases where this handler should pass tokens to a different handler
fn redirect_struct(
    tokens: &[Token],
    result: HandlerResult,
) -> Result<HandlerResult, ConversionError> {
    let id = Id::get("redirect_struct");
    report!(
        "struct_handler",
        "redirect_struct",
        Info,
        Report,
        "Checking if struct tokens should be redirected",
        true
    );

    // Check if this is actually a union (should go to a union handler if we have one)
    if tokens.iter().any(|t| t.to_string() == "union") {
        report!(
            "struct_handler",
            "redirect_struct",
            Info,
            Report,
            "Redirecting to union handler",
            true
        );
        let redirect_id = Id::get("union_redirect");
        return Ok(HandlerResult::Redirected(
            Some(tokens.to_vec()),
            0..1,
            "union".to_string(),
            id,
            redirect_id,
        ));
    }

    // Check if this is actually an enum disguised as a struct
    if tokens.iter().any(|t| t.to_string() == "enum") {
        report!(
            "struct_handler",
            "redirect_struct",
            Info,
            Report,
            "Redirecting to enum handler",
            true
        );
        let redirect_id = Id::get("enum_redirect");
        return Ok(HandlerResult::Redirected(
            Some(tokens.to_vec()),
            0..1,
            "enum".to_string(),
            id,
            redirect_id,
        ));
    }

    // Check if this is a typedef that doesn't involve struct definition
    if tokens.len() >= 2 && tokens[0].to_string() == "typedef" && tokens[1].to_string() != "struct"
    {
        report!(
            "struct_handler",
            "redirect_struct",
            Info,
            Report,
            "Redirecting to typedef handler",
            true
        );
        let redirect_id = Id::get("typedef_redirect");
        return Ok(HandlerResult::Redirected(
            Some(tokens.to_vec()),
            0..1,
            "typedef".to_string(),
            id,
            redirect_id,
        ));
    }

    // Check if this contains function pointers that might need special handling
    let has_function_pointer = tokens
        .windows(2)
        .any(|w| w[0].to_string() == "(" && w[1].to_string() == "*");

    if has_function_pointer {
        report!(
            "struct_handler",
            "redirect_struct",
            Info,
            Report,
            "Complex function pointer detected, might need special handling",
            true
        );
        // For now, we'll handle it here, but this could be redirected to a specialized handler
    }

    // Check if this is actually a variable declaration with struct type
    let mut has_assignment = false;
    let mut brace_count = 0;

    for token in tokens {
        match token.to_string().as_str() {
            "{" => brace_count += 1,
            "}" => brace_count -= 1,
            "=" => has_assignment = true,
            _ => {}
        }
    }

    // If we have an assignment but no struct definition braces, might be a variable declaration
    if has_assignment && brace_count == 0 {
        report!(
            "struct_handler",
            "redirect_struct",
            Info,
            Report,
            "Redirecting to global handler (struct variable)",
            true
        );
        let redirect_id = Id::get("global_redirect");
        return Ok(HandlerResult::Redirected(
            Some(tokens.to_vec()),
            0..1,
            "global".to_string(),
            id,
            redirect_id,
        ));
    }

    // No redirection needed
    Ok(HandlerResult::NotHandled(Some(tokens.to_vec()), 0..0, id))
}

/// Result callback: Postprocesses generated struct code, adds documentation, and enhances formatting
fn result_struct(
    tokens: &[Token],
    result: HandlerResult,
) -> Result<HandlerResult, ConversionError> {
    let _id = Id::get("result_struct");

    report!(
        "struct_handler",
        "result_struct",
        Info,
        Report,
        "Postprocessing struct conversion result",
        true
    );

    match result {
        HandlerResult::Completed(tokens_opt, _, rust_code, id) => {
            // Extract struct information for documentation
            let (struct_info, field_count) = extract_struct_info_from_tokens(tokens);

            // Generate documentation about the struct conversion
            let doc_comment = generate_struct_documentation(tokens, &struct_info, field_count);

            // Enhance the Rust code with documentation and metadata
            let mut enhanced_code = String::new();

            // Add metadata comment for traceability
            let metadata_comment = format!(
                "// [C2R] Struct converted from C to Rust - {}, {} fields\n",
                struct_info, field_count
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
                "struct_handler",
                "result_struct",
                Info,
                Report,
                &format!(
                    "Enhanced struct conversion: {}, {} fields",
                    struct_info, field_count
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
            let (struct_info, field_count) = extract_struct_info_from_tokens(tokens);
            let doc_comment = generate_struct_documentation(tokens, &struct_info, field_count);

            let mut enhanced_code = String::new();
            let metadata_comment = format!(
                "// [C2R] Struct converted from C to Rust - {}, {} fields\n",
                struct_info, field_count
            );
            enhanced_code.push_str(&metadata_comment);

            if !doc_comment.trim().is_empty() {
                enhanced_code.push_str(&doc_comment);
                enhanced_code.push('\n');
            }
            enhanced_code.push_str(&rust_code);

            report!(
                "struct_handler",
                "result_struct",
                Info,
                Report,
                &format!(
                    "Enhanced converted struct: {}, {} fields",
                    struct_info, field_count
                ),
                true
            );

            // CRITICAL: Return HandlerResult::Converted to preserve the code!
            Ok(HandlerResult::Converted(element, 0..1, enhanced_code, id))
        }
        HandlerResult::Extracted(element, _, rust_code, id) => {
            // Handle extracted elements - enhance the code and preserve the variant
            let (struct_info, field_count) = extract_struct_info_from_tokens(tokens);
            let doc_comment = generate_struct_documentation(tokens, &struct_info, field_count);

            let mut enhanced_code = String::new();
            let metadata_comment = format!(
                "// [C2R] Struct converted from C to Rust - {}, {} fields\n",
                struct_info, field_count
            );
            enhanced_code.push_str(&metadata_comment);

            if !doc_comment.trim().is_empty() {
                enhanced_code.push_str(&doc_comment);
                enhanced_code.push('\n');
            }
            enhanced_code.push_str(&rust_code);

            report!(
                "struct_handler",
                "result_struct",
                Info,
                Report,
                &format!(
                    "Enhanced extracted struct: {}, {} fields",
                    struct_info, field_count
                ),
                true
            );

            // CRITICAL: Return HandlerResult::Extracted to preserve the code!
            Ok(HandlerResult::Extracted(element, 0..1, enhanced_code, id))
        }
        HandlerResult::Handled(Some(converted_tokens), _, handler_id) => {
            // Legacy support for token-based results
            // Extract struct information for documentation
            let (struct_info, field_count) = extract_struct_info_from_tokens(tokens);

            // Generate documentation about the struct conversion
            let doc_comment = generate_struct_documentation(tokens, &struct_info, field_count);

            // Postprocess the converted Rust code for better formatting
            let mut enhanced_result = postprocess_struct_code(converted_tokens);

            // Add the documentation comment before the converted struct
            if !doc_comment.is_empty() {
                enhanced_result.insert(0, Token::s(doc_comment));
            }

            // Add metadata comment for traceability
            let metadata_comment = format!(
                "// [C2R] Struct converted from C to Rust - {}, {} fields",
                struct_info, field_count
            );
            enhanced_result.insert(0, Token::s(metadata_comment));

            report!(
                "struct_handler",
                "result_struct",
                Info,
                Report,
                &format!(
                    "Enhanced struct conversion: {}, {} fields",
                    struct_info, field_count
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
                "struct_handler",
                "result_struct",
                Warning,
                Report,
                "Struct handler returned empty result",
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

/// Extracts struct information from the original tokens for documentation purposes
fn extract_struct_info_from_tokens(tokens: &[Token]) -> (String, usize) {
    if tokens.is_empty() {
        return ("unknown struct".to_string(), 0);
    }

    let is_typedef = tokens[0].to_string() == "typedef";
    let struct_token_idx = if is_typedef { 1 } else { 0 };

    // Find struct name
    let mut struct_name = "anonymous".to_string();
    if struct_token_idx + 1 < tokens.len() {
        let name_candidate = tokens[struct_token_idx + 1].to_string();
        if name_candidate != "{" {
            struct_name = name_candidate;
        }
    }

    // Count fields by counting semicolons inside braces
    let mut field_count = 0;
    let mut in_struct = false;
    let mut brace_depth = 0;

    for token in tokens {
        let token_str = token.to_string();
        match token_str.as_str() {
            "{" => {
                brace_depth += 1;
                if brace_depth == 1 {
                    in_struct = true;
                }
            }
            "}" => {
                brace_depth -= 1;
                if brace_depth == 0 {
                    in_struct = false;
                }
            }
            ";" if in_struct && brace_depth == 1 => {
                field_count += 1;
            }
            _ => {}
        }
    }

    let struct_type = if is_typedef {
        format!("typedef struct {}", struct_name)
    } else {
        format!("struct {}", struct_name)
    };

    (struct_type, field_count)
}

/// Generates documentation comments for the struct conversion
fn generate_struct_documentation(
    tokens: &[Token],
    struct_info: &str,
    field_count: usize,
) -> String {
    let mut doc_lines = Vec::new();

    // Add main documentation header
    doc_lines.push("/**".to_string());
    doc_lines.push(" * Struct Conversion Documentation".to_string());
    doc_lines.push(" *".to_string());

    // Add struct information
    doc_lines.push(format!(" * Struct: {}", struct_info));
    doc_lines.push(format!(" * Fields: {}", field_count));

    // Add conversion details
    doc_lines.push(format!(" * Original tokens: {}", tokens.len()));
    doc_lines.push(" *".to_string());

    // Add conversion notes based on struct type
    let is_typedef = tokens.get(0).map_or(false, |t| t.to_string() == "typedef");
    let is_forward_decl =
        tokens.iter().any(|t| t.to_string() == ";") && !tokens.iter().any(|t| t.to_string() == "{");

    if is_forward_decl {
        doc_lines.push(" * Type: Forward declaration".to_string());
        doc_lines
            .push(" * Conversion: C forward declaration -> Rust struct declaration".to_string());
    } else if is_typedef {
        doc_lines.push(" * Type: Typedef struct definition".to_string());
        doc_lines
            .push(" * Conversion: C typedef struct -> Rust struct with derive macros".to_string());
        doc_lines.push(" * Features: #[derive(Debug, Clone, PartialEq)]".to_string());
    } else {
        doc_lines.push(" * Type: Regular struct definition".to_string());
        doc_lines.push(" * Conversion: C struct -> Rust struct with derive macros".to_string());
        doc_lines.push(" * Features: #[derive(Debug, Clone, PartialEq)]".to_string());
    }

    if field_count > 0 {
        doc_lines.push(format!(
            " * Note: {} fields converted with type mapping",
            field_count
        ));
    }

    doc_lines.push(" */".to_string());

    report!(
        "struct_handler",
        "generate_struct_documentation",
        Info,
        Report,
        &format!("Generated documentation for {}", struct_info),
        true
    );

    doc_lines.join("\n")
}

/// Postprocesses the converted struct code for better formatting
fn postprocess_struct_code(mut tokens: Vec<Token>) -> Vec<Token> {
    let original_count = tokens.len();

    // Clean up and format the converted struct tokens
    for token in tokens.iter_mut() {
        let mut content = token.to_string();

        // Clean up extra whitespace
        content = content.trim().to_string();

        // Ensure proper spacing around braces and brackets
        if content.contains('{') || content.contains('}') {
            content = content.replace('{', " {").replace('}', "} ");
        }

        // Ensure proper formatting for derive attributes
        if content.starts_with("#[derive") {
            // Ensure the attribute is on its own line
            content = format!("{}\n", content);
        }

        // Ensure proper indentation for struct fields
        if content.contains("pub ") && content.contains(':') {
            // This looks like a field declaration
            if !content.starts_with("    ") {
                content = format!("    {}", content);
            }
        }

        *token = Token::s(content);
    }

    // Remove any empty tokens
    tokens.retain(|token| !token.to_string().trim().is_empty());

    report!(
        "struct_handler",
        "postprocess_struct_code",
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
fn report_struct(_tokens: &[Token]) -> Result<HandlerReport, ConversionError> {
    let context = context!();
    let handler_reports = context.get_reports_by_handler("struct_handler");

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
        "Struct Handler Summary: {} total reports (Info: {}, Warnings: {}, Errors: {})",
        handler_reports.len(),
        info_count,
        warning_count,
        error_count
    );

    // Create consolidated report using gen_name function for unique report_id
    let summary_report = HandlerReport {
        report_id: Box::new(Id::get(&crate::Id::gen_name("struct_handler_summary"))),
        handler_id: Box::new(Id::get("struct_handler")),
        handler_name: "struct_handler".to_string(),
        function_name: "report_struct".to_string(),
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
