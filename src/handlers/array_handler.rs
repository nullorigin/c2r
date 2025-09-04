use super::common::{find_matching_token, not_handled, replace_with};
use crate::context::{Context, HandlerReport, ReportLevel, HandlerPhase};
use crate::context::ReportLevel::{Info, Error, Warning, Debug};
use crate::context::HandlerPhase::{Process, Handle, Extract, Convert, Report};
use crate::report;
use crate::{convert_type_tokens, debug};
use crate::error::ConversionError;
use crate::extract::{ExtractedElement, ExtractedArray};
use crate::handler::HandlerResult;
use crate::{convert_type, get_id, Token};
use crate::{ConvertedElement, ConvertedArray};

/// Creates an array handler that can detect and convert C array declarations and accesses
pub fn create_array_handler() -> crate::handler::Handler {
    let handler_id = get_id("array_handler");
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
        None,
        None,
        Some(redirect_array)
    )
}

/// Determines if the token sequence represents an array declaration or access
fn process_array(tokens: &[Token], _context: &mut Context) -> Result<bool,ConversionError> {
    let _id = get_id("process_array");
    // Need at least 4 tokens for array declaration: type name [ size ]
    // or at least 4 tokens for array access: name [ index ]
    if tokens.len() < 4 {
        return Ok(false);
    }

    // Check for array declaration pattern: type identifier [ number ]
    for i in 0..tokens.len() - 3 {
        if i + 3 < tokens.len() && tokens[i + 2].to_string() == "[" {
            return Ok(true);
        }
    }

    // Check for array access pattern: identifier [ expression ]
    for i in 0..tokens.len() - 2 {
        if i + 1 < tokens.len() && tokens[i + 1].to_string() == "[" {
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

    Ok(false)
}

/// Process array declarations and accesses
fn handle_array(tokens: &[Token], context: &mut Context) -> Result<HandlerResult, ConversionError> {
    let _id = get_id("handle_array");
    report!(context, "array_handler", Info, Handle, 
        "Array handler processing tokens", true);
    
    // First check if it's an array declaration
    if is_array_declaration(tokens) {
        return process_array_declaration(tokens, context);
    }

    // Otherwise check if it's an array access
    if is_array_access(tokens) {
        return process_array_access(tokens, context);
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
        if i + 3 < tokens.len() && 
           tokens[i + 2].to_string() == "[" {
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
fn process_array_declaration(tokens: &[Token], context: &mut Context) -> Result<HandlerResult, ConversionError> {
    report!(context, "array_handler", Info, Process, 
        "Processing array declaration", true);
    
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
    let rust_type = convert_type(&c_type_str).unwrap();
    
    // Build Rust array declaration
    let rust_code = format!("let mut {}: [{}; {}] = [Default::default(); {}];", 
                           array_name, rust_type, size_str, size_str);
    
    let id = get_id("array_declaration");
    replace_with(rust_code, id)
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
fn process_array_access(tokens: &[Token], context: &mut Context) -> Result<HandlerResult, ConversionError> {
    report!(context, "array_handler", Info, Process, 
        "Processing array access", true);
    
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
    
    let id = get_id("array_access");
    replace_with(rust_code, id)
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
            if (is_identifier(&prev_token) || is_literal(&prev_token)) &&
               (is_identifier(&token_str) || is_literal(&token_str)) {
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
        "+" | "-" | "*" | "/" | "%" | "=" | "==" | "!=" | "<" | ">" | "<=" | ">=" | "+=" | "-=" | "*=" | "/=" | "%=" => true,
        _ => false,
    }
}

/// Extract array information from tokens
fn extract_array(tokens: &[Token], context: &mut Context) -> Result<Option<ExtractedElement>, ConversionError> {
    let _id = get_id("extract_array");
    report!(context, "array_handler", Info, Extract, 
        "Extracting array from tokens", true);
    
    if is_array_declaration(tokens) {
        return extract_array_declaration(tokens, context);
    }
    
    if is_array_access(tokens) {
        return extract_array_access(tokens, context);
    }
    
    Ok(None)
}

/// Extract array declaration information
fn extract_array_declaration(tokens: &[Token], context: &mut Context) -> Result<Option<ExtractedElement>, ConversionError> {
    report!(context, "array_handler", Info, Extract, 
        "Extracting array declaration", true);
    
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
fn extract_array_access(tokens: &[Token], context: &mut Context) -> Result<Option<ExtractedElement>, ConversionError> {
    report!(context, "array_handler", Info, Extract, 
        "Extracting array access", true);
    
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
fn convert_array(tokens: &[Token], context: &mut Context) -> Result<Option<ConvertedElement>, ConversionError> {
    let _id = get_id("convert_array");
    report!(context, "array_handler", Info, Convert, 
        "Converting array to Rust", true);
    
    // First extract the array information from tokens
    if let Some(ExtractedElement::Array(array)) = extract_array(tokens, context)? {
        if array.is_declaration {
            // Convert C type to Rust type
            let rust_type = convert_type(&array.element_type).unwrap_or_else(|| array.element_type.clone());
            
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
        Err(ConversionError::InvalidExtraction("Expected ExtractedArray".to_string()))
    }
}

/// Handle redirection for array processing
fn redirect_array(_tokens: &[Token], result: HandlerResult, context: &mut Context) -> Result<HandlerResult, ConversionError> {
    let _id = get_id("redirect_array");
    report!(context, "array_handler", Info, Report, 
        "Redirecting array handler result", true);
    
    // For now, just return the result as-is
    // Could redirect to expression handler for complex array expressions
    // or to typedef handler for array typedefs
    Ok(result)
}
