#![allow(
    unused_variables,
    unused_assignments,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals
)]

use super::common::{find_matching_token, not_handled, replace_with_range};
use crate::Id;
use crate::ReportLevel;
use crate::ReportLevel::Info;
use crate::ReportLevel::Warning;
use crate::config::HandlerPhase::{Convert, Extract, Handle, Process, Report};
use crate::config::HandlerReport;
use crate::context;
use crate::error::ConversionError;
use crate::extract::{ExtractedArray, ExtractedElement};
use crate::handler::HandlerResult;
use crate::report;
use crate::{ConvertedArray, ConvertedElement};
use crate::{Token, convert_type};

/// Creates an array handler that can detect and convert C array declarations and accesses
pub fn create_array_handler() -> crate::handler::Handler {
    let handler_id = Id::get("array_handler");
    let handler_role = "array";
    let priority = 120;

    super::create_handler(
        handler_id,
        handler_role,
        priority,
        Some(process_array),
        Some(handle_array),
        Some(extract_array),
        Some(convert_array),
        Some(report_array),
        Some(result_array),
        Some(redirect_array),
    )
}

/// Determines if the token sequence represents an array declaration or access
fn process_array(tokens: &[Token]) -> Result<bool, ConversionError> {
    let _id = Id::get("process_array");
    // Need at least 4 tokens for array declaration: type name [ size ]
    // or at least 4 tokens for array access: name [ index ]
    if tokens.len() < 4 {
        return Ok(false);
    }

    // Check for function-like patterns: identifier '(' - REJECT these
    for i in 0..tokens.len().saturating_sub(1) {
        if tokens[i + 1].to_string() == "(" {
            // Pattern: something followed by '(' suggests function call
            return Ok(false);
        }
    }

    // Check for function call patterns with member access: identifier '.' identifier '(' - REJECT these
    for i in 0..tokens.len().saturating_sub(3) {
        if tokens[i + 1].to_string() == "." && tokens[i + 3].to_string() == "(" {
            // Pattern: obj.method( suggests function call
            return Ok(false);
        }
    }

    // Check for variable assignment patterns without brackets: identifier '=' - REJECT these
    for i in 0..tokens.len().saturating_sub(1) {
        if tokens[i + 1].to_string() == "=" {
            // Look ahead to see if there are any brackets - if not, likely variable assignment
            let mut has_brackets = false;
            for j in i + 2..tokens.len() {
                if tokens[j].to_string() == "[" || tokens[j].to_string() == "]" {
                    has_brackets = true;
                    break;
                }
            }
            if !has_brackets {
                return Ok(false);
            }
        }
    }

    // Now check for legitimate array patterns

    // Check for array declaration pattern: type identifier [ size ]
    for i in 0..tokens.len() - 3 {
        if i + 2 < tokens.len() && tokens[i + 2].to_string() == "[" {
            // Additional validation: ensure this looks like a real array declaration
            let token1 = if i > 0 {
                tokens[i].to_string()
            } else {
                String::new()
            };
            let token2 = if i + 1 < tokens.len() {
                tokens[i + 1].to_string()
            } else {
                String::new()
            };

            // Check if first token looks like a type
            if token1 == "int"
                || token1 == "char"
                || token1 == "float"
                || token1 == "double"
                || token1 == "long"
                || token1 == "short"
                || token1 == "unsigned"
                || token1 == "signed"
                || (!token1.is_empty()
                    && !token1.starts_with('{')
                    && !token1.starts_with('}')
                    && !token1.starts_with(';')
                    && !token1.starts_with('='))
            {
                // Look for closing bracket to confirm array pattern
                let mut j = i + 3;
                while j < tokens.len() {
                    if tokens[j].to_string() == "]" {
                        return Ok(true);
                    }
                    j += 1;
                }
            }
        }
    }

    // Check for array access pattern: identifier [ expression ]
    for i in 0..tokens.len() - 2 {
        if i + 1 < tokens.len() && tokens[i + 1].to_string() == "[" {
            let identifier = tokens[i].to_string();

            // Additional validation: ensure identifier looks legitimate (not punctuation)
            if !identifier.is_empty()
                && !identifier.starts_with('{')
                && !identifier.starts_with('}')
                && !identifier.starts_with(';')
                && !identifier.starts_with('=')
                && !identifier.starts_with(',')
                && !identifier.starts_with('(')
                && !identifier.starts_with(')')
            {
                // Look for closing bracket
                let mut j = i + 2;
                while j < tokens.len() {
                    if tokens[j].to_string() == "]" {
                        return Ok(true);
                    }
                    j += 1;
                }
            }
        }
    }

    Ok(false)
}

/// Process array declarations and accesses
fn handle_array(tokens: &[Token]) -> Result<HandlerResult, ConversionError> {
    let id = Id::get("handle_array");
    report!(
        "array_handler",
        "handle_array",
        Info,
        Handle,
        "Array handler processing tokens",
        true
    );

    // First check if it's an array declaration
    if is_array_declaration(tokens) {
        return process_array_declaration(tokens);
    }

    // Otherwise check if it's an array access
    if is_array_access(tokens) {
        return process_array_access(tokens);
    }
    not_handled()
}

/// Check if tokens represent an array declaration
fn is_array_declaration(tokens: &[Token]) -> bool {
    // We look for a pattern like: type name [ size ]
    if tokens.len() < 4 {
        return false;
    }

    for i in 0..tokens.len() - 3 {
        if i + 3 < tokens.len() && tokens[i + 2].to_string() == "[" {
            // Look for closing bracket
            let mut j = i + 3;
            while j < tokens.len() {
                if tokens[j].to_string() == "]" {
                    return true;
                }
                j += 1;
            }
        }
    }

    false
}

/// Process array declaration
fn process_array_declaration(tokens: &[Token]) -> Result<HandlerResult, ConversionError> {
    report!(
        "array_handler",
        "process_array_declaration",
        Info,
        Process,
        "Processing array declaration",
        true
    );

    // Find type, name, and size
    let mut type_end = 0;
    let mut name_pos = 0;
    let mut bracket_start = 0;

    for i in 0..tokens.len() - 1 {
        if tokens[i + 1].to_string() == "[" {
            type_end = i;
            name_pos = i;
            bracket_start = i + 1;
            break;
        }
    }

    if bracket_start == 0 {
        return not_handled();
    }

    // Extract type tokens
    let type_tokens = &tokens[0..type_end];

    // Extract array name
    let array_name = tokens[name_pos].to_string();

    // Find closing bracket
    let bracket_end = match find_matching_token(&tokens[bracket_start..], "[", "]") {
        Some(pos) => bracket_start + pos + 1,
        None => return not_handled(),
    };

    // Extract size tokens (without brackets)
    let size_tokens = &tokens[bracket_start + 1..bracket_end - 1];
    let size_str = tokens_to_string(size_tokens)?;

    // Convert C type to Rust type
    let c_type_str = tokens_to_string(type_tokens)?;
    let rust_type = convert_type(&c_type_str).unwrap_or("i32".to_string());

    // Build Rust array declaration
    let rust_code = format!(
        "let mut {}: [{}; {}] = [Default::default(); {}];",
        array_name, rust_type, size_str, size_str
    );

    // Calculate token consumption range for array declaration
    // Find the end of the declaration (should end with semicolon)
    let mut end_pos = tokens.len();
    for i in 0..tokens.len() {
        if tokens[i].to_string() == ";" {
            end_pos = i + 1; // Include semicolon
            break;
        }
    }

    let id = Id::get("array_declaration");
    replace_with_range(rust_code, 0..end_pos, id)
}

/// Check if tokens represent an array access
fn is_array_access(tokens: &[Token]) -> bool {
    // We look for a pattern like: name [ index ]
    if tokens.len() < 4 {
        return false;
    }

    for i in 0..tokens.len() - 2 {
        if i + 1 < tokens.len() && tokens[i + 1].to_string() == "[" {
            // Look for closing bracket
            let mut j = i + 2;
            while j < tokens.len() {
                if tokens[j].to_string() == "]" {
                    return true;
                }
                j += 1;
            }
        }
    }

    false
}

/// Process array access
fn process_array_access(tokens: &[Token]) -> Result<HandlerResult, ConversionError> {
    report!(
        "array_handler",
        "process_array_access",
        Info,
        Process,
        "Processing array access",
        true
    );

    // Find array name and index
    let mut name_pos = 0;
    let mut bracket_start = 0;

    for i in 0..tokens.len() - 1 {
        if tokens[i + 1].to_string() == "[" {
            name_pos = i;
            bracket_start = i + 1;
            break;
        }
    }

    if bracket_start == 0 {
        return not_handled();
    }

    // Extract array name
    let array_name = tokens[name_pos].to_string();

    // Find closing bracket
    let bracket_end = match find_matching_token(&tokens[bracket_start..], "[", "]") {
        Some(pos) => bracket_start + pos + 1,
        None => return not_handled(),
    };

    // Extract index tokens (without brackets)
    let index_tokens = &tokens[bracket_start + 1..bracket_end - 1];
    let index_str = tokens_to_string(index_tokens)?;

    // Build Rust array access (same syntax as C)
    let rust_code = format!("{}[{}]", array_name, index_str);

    // Calculate token consumption range for array access
    // Range should cover from array name to closing bracket
    let token_range = name_pos..bracket_end;

    let id = Id::get("array_access");
    replace_with_range(rust_code, token_range, id)
}

/// Helper function to convert tokens to a string representation
fn tokens_to_string(tokens: &[Token]) -> Result<String, ConversionError> {
    let mut result = String::new();

    for (i, token) in tokens.iter().enumerate() {
        let token_str = token.to_string();

        // Add space between tokens if needed
        if i > 0 {
            let prev_token = tokens[i - 1].to_string();

            // Add space if both tokens are identifiers or literals
            if (is_identifier(&prev_token) || is_literal(&prev_token))
                && (is_identifier(&token_str) || is_literal(&token_str))
            {
                result.push(' ');
            }

            // Add space around operators
            if is_operator(&prev_token) || is_operator(&token_str) {
                result.push(' ');
            }
        }

        result.push_str(&token_str);
    }

    Ok(result)
}

/// Check if a token is an identifier
fn is_identifier(token: &str) -> bool {
    if let Some(first_char) = token.chars().next() {
        first_char.is_alphabetic() || first_char == '_'
    } else {
        false
    }
}

/// Check if a token is a literal
fn is_literal(token: &str) -> bool {
    if let Some(first_char) = token.chars().next() {
        first_char.is_digit(10) || token.starts_with('\"') || token.starts_with('\'')
    } else {
        false
    }
}

/// Check if a token is an operator
fn is_operator(token: &str) -> bool {
    match token {
        "+" | "-" | "*" | "/" | "%" | "=" | "==" | "!=" | "<" | ">" | "<=" | ">=" | "+=" | "-="
        | "*=" | "/=" | "%=" => true,
        _ => false,
    }
}

/// Extract array information from tokens
fn extract_array(tokens: &[Token]) -> Result<Option<ExtractedElement>, ConversionError> {
    let _id = Id::get("extract_array");
    report!(
        "array_handler",
        "extract_array",
        Info,
        Extract,
        "Extracting array from tokens",
        true
    );

    if is_array_declaration(tokens) {
        return extract_array_declaration(tokens);
    }

    if is_array_access(tokens) {
        return extract_array_access(tokens);
    }

    Ok(None)
}

/// Extract array declaration information
fn extract_array_declaration(
    tokens: &[Token],
) -> Result<Option<ExtractedElement>, ConversionError> {
    report!(
        "array_handler",
        "extract_array_declaration",
        Info,
        Extract,
        "Extracting array declaration",
        true
    );

    // Find type, name, and size components
    let mut type_end = 0;
    let mut name_pos = 0;
    let mut bracket_start = 0;

    for i in 0..tokens.len() - 1 {
        if tokens[i + 1].to_string() == "[" {
            type_end = i;
            name_pos = i;
            bracket_start = i + 1;
            break;
        }
    }

    if bracket_start == 0 {
        return Ok(None);
    }

    // Find closing bracket
    let bracket_end = match find_matching_token(&tokens[bracket_start..], "[", "]") {
        Some(pos) => bracket_start + pos + 1,
        None => return Ok(None),
    };

    // Extract components
    let type_tokens = &tokens[0..type_end];
    let array_name = tokens[name_pos].to_string();
    let size_tokens = &tokens[bracket_start + 1..bracket_end - 1];

    let c_type = tokens_to_string(type_tokens)?;
    let size_expr = tokens_to_string(size_tokens)?;

    let extracted = ExtractedArray {
        name: array_name,
        element_type: c_type,
        size: size_expr,
        is_declaration: true,
    };

    Ok(Some(ExtractedElement::Array(extracted)))
}

/// Extract array access information
fn extract_array_access(tokens: &[Token]) -> Result<Option<ExtractedElement>, ConversionError> {
    report!(
        "array_handler",
        "extract_array_access",
        Info,
        Extract,
        "Extracting array access",
        true
    );

    // Find array name and index
    let mut name_pos = 0;
    let mut bracket_start = 0;

    for i in 0..tokens.len() - 1 {
        if tokens[i + 1].to_string() == "[" {
            name_pos = i;
            bracket_start = i + 1;
            break;
        }
    }

    if bracket_start == 0 {
        return Ok(None);
    }

    // Find closing bracket
    let bracket_end = match find_matching_token(&tokens[bracket_start..], "[", "]") {
        Some(pos) => bracket_start + pos + 1,
        None => return Ok(None),
    };

    // Extract components
    let array_name = tokens[name_pos].to_string();
    let index_tokens = &tokens[bracket_start + 1..bracket_end - 1];
    let index_expr = tokens_to_string(index_tokens)?;

    let extracted = ExtractedArray {
        name: array_name,
        element_type: String::new(), // Not needed for access
        size: index_expr,
        is_declaration: false,
    };

    Ok(Some(ExtractedElement::Array(extracted)))
}

/// Convert extracted array to Rust code
fn convert_array(tokens: &[Token]) -> Result<Option<ConvertedElement>, ConversionError> {
    let _id = Id::get("convert_array");
    report!(
        "array_handler",
        "convert_array",
        Info,
        Convert,
        "Converting array to Rust",
        true
    );

    // First extract the array information from tokens
    if let Some(ExtractedElement::Array(array)) = extract_array(tokens)? {
        if array.is_declaration {
            // Convert C type to Rust type
            let rust_type =
                convert_type(&array.element_type).unwrap_or_else(|| array.element_type.clone());

            // Generate Rust array declaration
            let rust_code = format!(
                "let mut {}: [{}; {}] = [Default::default(); {}];",
                array.name, rust_type, array.size, array.size
            );

            let converted = ConvertedArray {
                element_type: rust_type,
                size: array.size.clone(),
                rust_code,
                is_declaration: true,
            };

            Ok(Some(ConvertedElement::Array(converted)))
        } else {
            // Array access - same syntax in Rust
            let rust_code = format!("{}[{}]", array.name, array.size);

            let converted = ConvertedArray {
                element_type: String::new(),
                size: array.size.clone(),
                rust_code,
                is_declaration: false,
            };

            Ok(Some(ConvertedElement::Array(converted)))
        }
    } else {
        Err(ConversionError::InvalidExtraction(
            "Expected ExtractedArray".to_string(),
        ))
    }
}

/// Handle redirection for array processing
fn redirect_array(
    _tokens: &[Token],
    result: HandlerResult,
) -> Result<HandlerResult, ConversionError> {
    let _id = Id::get("redirect_array");
    report!(
        "array_handler",
        "redirect_array",
        Info,
        Report,
        "Redirecting array handler result",
        true
    );

    // For now, just return the result as-is
    // Could redirect to expression handler for complex array expressions
    // or to typedef handler for array typedefs
    Ok(result)
}

/// Result callback: Postprocesses generated array code, adds documentation, and enhances formatting
fn result_array(tokens: &[Token], result: HandlerResult) -> Result<HandlerResult, ConversionError> {
    let _id = Id::get("result_array");

    report!(
        "array_handler",
        "result_array",
        Info,
        Report,
        "Postprocessing array conversion result",
        true
    );

    match result {
        HandlerResult::Completed(tokens_opt, _, rust_code, id) => {
            // Extract array information for documentation
            let (array_info, array_type) = extract_array_info_from_tokens(tokens);

            // Generate documentation about the array conversion
            let doc_comment = generate_array_documentation(tokens, &array_info, &array_type);

            // Enhance the Rust code with documentation and metadata
            let mut enhanced_code = String::new();

            // Add metadata comment for traceability
            let metadata_comment = format!(
                "// [C2R] Array converted from C to Rust - {}: {}\n",
                array_type, array_info
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
                "array_handler",
                "result_array",
                Info,
                Report,
                &format!("Enhanced array conversion: {} - {}", array_type, array_info),
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
            let (array_info, array_type) = extract_array_info_from_tokens(tokens);
            let doc_comment = generate_array_documentation(tokens, &array_info, &array_type);

            let mut enhanced_code = String::new();
            let metadata_comment = format!(
                "// [C2R] Array converted from C to Rust - {}: {}\n",
                array_type, array_info
            );
            enhanced_code.push_str(&metadata_comment);

            if !doc_comment.trim().is_empty() {
                enhanced_code.push_str(&doc_comment);
                enhanced_code.push('\n');
            }
            enhanced_code.push_str(&rust_code);

            report!(
                "array_handler",
                "result_array",
                Info,
                Report,
                &format!("Enhanced converted array: {} - {}", array_type, array_info),
                true
            );

            // CRITICAL: Return HandlerResult::Converted to preserve the code!
            Ok(HandlerResult::Converted(element, 0..1, enhanced_code, id))
        }
        HandlerResult::Extracted(element, _, rust_code, id) => {
            // Handle extracted elements - enhance the code and preserve the variant
            let (array_info, array_type) = extract_array_info_from_tokens(tokens);
            let doc_comment = generate_array_documentation(tokens, &array_info, &array_type);

            let mut enhanced_code = String::new();
            let metadata_comment = format!(
                "// [C2R] Array converted from C to Rust - {}: {}\n",
                array_type, array_info
            );
            enhanced_code.push_str(&metadata_comment);

            if !doc_comment.trim().is_empty() {
                enhanced_code.push_str(&doc_comment);
                enhanced_code.push('\n');
            }
            enhanced_code.push_str(&rust_code);

            report!(
                "array_handler",
                "result_array",
                Info,
                Report,
                &format!("Enhanced extracted array: {} - {}", array_type, array_info),
                true
            );

            // CRITICAL: Return HandlerResult::Extracted to preserve the code!
            Ok(HandlerResult::Extracted(element, 0..1, enhanced_code, id))
        }
        HandlerResult::Handled(Some(converted_tokens), _, handler_id) => {
            // Legacy support for token-based results
            // Extract array information for documentation
            let (array_info, array_type) = extract_array_info_from_tokens(tokens);

            // Generate documentation about the array conversion
            let doc_comment = generate_array_documentation(tokens, &array_info, &array_type);

            // Postprocess the converted Rust code for better formatting
            let mut enhanced_result = postprocess_array_code(converted_tokens);

            // Add the documentation comment before the converted array
            if !doc_comment.is_empty() {
                enhanced_result.insert(0, Token::s(doc_comment));
            }

            // Add metadata comment for traceability
            let metadata_comment = format!(
                "// [C2R] Array converted from C to Rust - {}: {}",
                array_type, array_info
            );
            enhanced_result.insert(0, Token::s(metadata_comment));

            report!(
                "array_handler",
                "result_array",
                Info,
                Report,
                &format!("Enhanced array conversion: {} - {}", array_type, array_info),
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
                "array_handler",
                "result_array",
                Warning,
                Report,
                "Array handler returned empty result",
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

/// Extracts array information from the original tokens for documentation purposes
fn extract_array_info_from_tokens(tokens: &[Token]) -> (String, String) {
    if tokens.is_empty() {
        return ("unknown array".to_string(), "unknown".to_string());
    }

    // Check if it's a declaration or access
    let is_declaration = is_array_declaration(tokens);
    let array_type = if is_declaration {
        "declaration"
    } else {
        "access"
    };

    if is_declaration {
        // Extract declaration info: type name[size]
        let mut type_parts = Vec::new();
        let mut array_name = "unknown".to_string();
        let mut size = "unknown".to_string();

        for i in 0..tokens.len() {
            let token_str = tokens[i].to_string();
            if i + 1 < tokens.len() && tokens[i + 1].to_string() == "[" {
                // This token is the array name
                array_name = token_str;

                // Find the size in brackets
                let mut j = i + 2;
                let mut bracket_count = 1;
                let mut size_tokens = Vec::new();

                while j < tokens.len() && bracket_count > 0 {
                    let size_token = tokens[j].to_string();
                    if size_token == "[" {
                        bracket_count += 1;
                    } else if size_token == "]" {
                        bracket_count -= 1;
                        if bracket_count == 0 {
                            break;
                        }
                    }
                    if bracket_count > 0 {
                        size_tokens.push(size_token);
                    }
                    j += 1;
                }

                size = size_tokens.join(" ");
                break;
            } else if token_str != "[" {
                type_parts.push(token_str);
            }
        }

        let element_type = if type_parts.len() > 1 {
            type_parts[..type_parts.len() - 1].join(" ")
        } else {
            "unknown".to_string()
        };

        (
            format!("{} {}[{}]", element_type, array_name, size),
            array_type.to_string(),
        )
    } else {
        // Extract access info: name[index]
        let mut array_name = "unknown".to_string();
        let mut index = "unknown".to_string();

        for i in 0..tokens.len() {
            if i + 1 < tokens.len() && tokens[i + 1].to_string() == "[" {
                array_name = tokens[i].to_string();

                // Find the index in brackets
                let mut j = i + 2;
                let mut bracket_count = 1;
                let mut index_tokens = Vec::new();

                while j < tokens.len() && bracket_count > 0 {
                    let index_token = tokens[j].to_string();
                    if index_token == "[" {
                        bracket_count += 1;
                    } else if index_token == "]" {
                        bracket_count -= 1;
                        if bracket_count == 0 {
                            break;
                        }
                    }
                    if bracket_count > 0 {
                        index_tokens.push(index_token);
                    }
                    j += 1;
                }

                index = index_tokens.join(" ");
                break;
            }
        }

        (format!("{}[{}]", array_name, index), array_type.to_string())
    }
}

/// Generates documentation comments for the array conversion
fn generate_array_documentation(tokens: &[Token], array_info: &str, array_type: &str) -> String {
    let mut doc_lines = Vec::new();

    // Add main documentation header
    doc_lines.push("/**".to_string());
    doc_lines.push(" * Array Conversion Documentation".to_string());
    doc_lines.push(" *".to_string());

    // Add array information
    doc_lines.push(format!(" * Type: Array {}", array_type));
    doc_lines.push(format!(" * Array: {}", array_info));

    // Add conversion details
    doc_lines.push(format!(" * Original tokens: {}", tokens.len()));
    doc_lines.push(" *".to_string());

    // Add conversion notes based on array type
    match array_type {
        "declaration" => {
            doc_lines.push(
                " * Conversion: C array declaration -> Rust array with Default::default()"
                    .to_string(),
            );
            doc_lines.push(" * Features:".to_string());
            doc_lines.push(" *   - Stack-allocated fixed-size array".to_string());
            doc_lines.push(" *   - Initialized with Default::default() for safety".to_string());
            doc_lines.push(" *   - Mutable declaration (let mut) for flexibility".to_string());
            doc_lines.push(" *   - Type inference with explicit size".to_string());
        }
        "access" => {
            doc_lines.push(
                " * Conversion: C array access -> Rust array access (same syntax)".to_string(),
            );
            doc_lines.push(" * Features:".to_string());
            doc_lines.push(" *   - Bounds checking enabled in debug builds".to_string());
            doc_lines.push(" *   - Same indexing syntax as C".to_string());
            doc_lines.push(" *   - Zero-cost abstraction in release builds".to_string());
        }
        _ => {
            doc_lines.push(" * Note: Unknown array operation type".to_string());
        }
    }

    doc_lines.push(" */".to_string());

    report!(
        "array_handler",
        "generate_array_documentation",
        Info,
        Report,
        &format!(
            "Generated documentation for array {}: {}",
            array_type, array_info
        ),
        true
    );

    doc_lines.join("\n")
}

/// Postprocesses the converted array code for better formatting
fn postprocess_array_code(mut tokens: Vec<Token>) -> Vec<Token> {
    let original_count = tokens.len();

    // Clean up and format the converted array tokens
    for token in tokens.iter_mut() {
        let mut content = token.to_string();

        // Clean up extra whitespace
        content = content.trim().to_string();

        // Ensure proper spacing around brackets and operators
        content = content.replace('[', " [").replace(']', "] ");
        content = content.replace('=', " = ").replace(';', "; ");

        // Clean up multiple spaces
        while content.contains("  ") {
            content = content.replace("  ", " ");
        }

        // Ensure proper formatting for array declarations
        if content.contains("let mut") {
            // Ensure proper spacing in array declarations
            content = content.replace("mut ", "mut ");
            content = content.replace(": [", ": [");
        }

        // Ensure proper formatting for Default::default()
        if content.contains("Default::default()") {
            content = content.replace("Default :: default ()", "Default::default()");
        }

        *token = Token::s(content.trim().to_string());
    }

    // Remove any empty tokens
    tokens.retain(|token| !token.to_string().trim().is_empty());

    report!(
        "array_handler",
        "postprocess_array_code",
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
fn report_array(_tokens: &[Token]) -> Result<HandlerReport, ConversionError> {
    let context = context!();
    let handler_reports = context.get_reports_by_handler("array_handler");

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
        "Array Handler Summary: {} total reports (Info: {}, Warnings: {}, Errors: {})",
        handler_reports.len(),
        info_count,
        warning_count,
        error_count
    );

    // Create consolidated report using gen_name function for unique report_id
    let summary_report = HandlerReport {
        report_id: Box::new(Id::get(&Id::gen_name("array_handler_summary"))),
        handler_id: Box::new(Id::get("array_handler")),
        handler_name: "array_handler".to_string(),
        function_name: "report_array".to_string(),
        level: Info,
        phase: Report,
        message: summary_message,
        success: error_count == 0, // Success if no errors
        tokens_processed: handler_reports.iter().map(|r| r.tokens_processed).sum(),
        tokens_consumed: handler_reports.iter().map(|r| r.tokens_consumed).sum(),
        metadata: std::collections::HashMap::new(),
    };

    Ok(summary_report)
}
