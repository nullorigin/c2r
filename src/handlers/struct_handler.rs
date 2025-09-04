use std::str::FromStr;
use crate::context::{Context, HandlerReport, ReportLevel::{Info, Error}, HandlerPhase::{Process, Handle, Extract, Convert, Report}};
use crate::error::ConversionError;
use crate::handler::HandlerResult;
use crate::{convert_type, convert_type_token, convert_type_tokens, get_id, to_rust_string, ConvertedElement, ConvertedStruct, ExtractedElement, ExtractedStruct, Token, report};

use super::common::{find_matching_token, not_handled, replace_with};

/// Creates a struct handler that can detect and convert C structs
pub fn create_struct_handler() -> crate::handler::Handler {
    let handler_id = get_id("struct_handler");
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
        None,
        None,
        None
    )
}

/// Process callback: Initializes and confirms this handler can handle the tokens
fn process_struct(tokens: &[Token], context: &mut Context) -> Result<bool, ConversionError> {
    // Validate input
    if tokens.len() < 2 {
        return Ok(false);
    }
    
    // Look for struct keyword patterns
    if tokens[0].to_string() == "struct" {
        // struct name { ... }
        if tokens.len() >= 3 && tokens[2].to_string() == "{" {
            report!(context, "struct_handler", Info, Process, 
                format!("Struct definition detected: {}", tokens[1].to_string()), true);
            return Ok(true);
        }
        
        // struct name;
        if tokens.len() >= 3 && tokens[2].to_string() == ";" {
            report!(context, "struct_handler", Info, Process, 
                format!("Struct forward declaration detected: {}", tokens[1].to_string()), true);
            return Ok(true);
        }
        
        // typedef struct { ... } name;
        if tokens.len() >= 4 {
            for i in 2..tokens.len() {
                if tokens[i].to_string() == "{" {
                    report!(context, "struct_handler", Info, Process, 
                        "Typedef struct detected", true);
                    return Ok(true);
                }
            }
        }
    }
    
    // Check for typedef struct patterns
    if tokens.len() >= 3 && tokens[0].to_string() == "typedef" && tokens[1].to_string() == "struct" {
        report!(context, "struct_handler", Info, Process, 
            "Typedef struct pattern detected", true);
        return Ok(true);
    }
    
    Ok(false)
}

/// Legacy function for backwards compatibility
fn can_handle_struct(tokens: &[Token], context: &mut Context) -> bool {
    if tokens.len() < 3 {
        return false;
    }
    
    // Look for "struct" keyword followed by an identifier and opening brace
    // or "struct" keyword followed by an identifier and semicolon (forward declaration)
    
    if tokens[0].to_string() == "struct" {
        // Check if it's a complete struct definition
        if tokens.len() >= 3 && tokens[2].to_string() == "{" {
            return true;
        }
        
        // Check if it's a forward declaration
        if tokens.len() >= 3 && tokens[2].to_string() == ";" {
            return true;
        }
        
        // Check for typedef struct
        if tokens.len() >= 2 {
            // Look ahead for opening brace
            for i in 2..tokens.len() {
                if tokens[i].to_string() == "{" {
                    return true;
                }
            }
        }
    }
    
    // Check for "typedef struct" pattern
    if tokens.len() >= 2 && tokens[0].to_string() == "typedef" && tokens[1].to_string() == "struct" {
        return true;
    }
    
    false
}

/// Processes a struct declaration
fn handle_struct(tokens: &[Token], context: &mut Context) -> Result<HandlerResult, ConversionError> {
    let id = get_id("handle_struct");
    report!(context, "struct_handler", Info, Handle, 
        "Struct handler processing tokens", true);
    
    if tokens[0].to_string() != "struct" && !(tokens.len() >= 2 && tokens[0].to_string() == "typedef" && tokens[1].to_string() == "struct") {
        return not_handled();
    }
    
    let is_typedef = tokens[0].to_string() == "typedef";
    let struct_token_idx = if is_typedef { 1 } else { 0 };
    
    // Extract the struct name
    let name_idx = struct_token_idx + 1;
    if name_idx >= tokens.len() {
        report!(context, "struct_handler", Error, Handle, 
            "Struct missing name", true);
        return not_handled();
    }
    
    let struct_name = tokens[name_idx].to_string();
    report!(context, "struct_handler", Info, Handle, 
        format!("Found struct: {}", struct_name), true);
    
    // Look for the opening brace
    let mut brace_idx = name_idx + 1;
    while brace_idx < tokens.len() && tokens[brace_idx].to_string() != "{" {
        brace_idx += 1;
    }
    
    if brace_idx >= tokens.len() {
        // This might be a forward declaration
        if tokens[name_idx + 1].to_string() == ";" {
            // Forward declaration
            report!(context, "struct_handler", Info, Handle, 
                format!("Forward declaration of struct {}", struct_name), true);
            return replace_with(format!("// Forward declaration\npub struct {};\n", struct_name),id);
        } else {
            report!(context, "struct_handler", Error, Handle, 
                format!("Could not find opening brace for struct {}", struct_name), true);
            return not_handled();
        }
    }
    
    // Find the closing brace
    let body_start = brace_idx;
    let body_end = match find_matching_token(&tokens[body_start..], "{", "}") {
        Some(pos) => body_start + pos + 1,
        None => {
            report!(context, "struct_handler", Error, Handle, 
                format!("Could not find closing brace for struct {}", struct_name), true);
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
                    typedef_name = Some(tokens[i-1].to_string());
                }
                break;
            }
        }
    }
    
    // Convert to Rust
    if let Some(ConvertedElement::Struct(element)) = convert_struct(tokens,context)? {
        let rust_code = element.rust_code.clone();
        replace_with(rust_code, id)
    } else {
        Err(ConversionError::new("Unsupported struct element"))
    }
}

/// Extracts a struct as an ExtractedElement
pub fn extract_struct(tokens: &[Token], context: &mut Context) -> Result<Option<ExtractedElement>, ConversionError> {
    let is_typedef = tokens[0].to_string() == "typedef";
    let struct_token_idx = if is_typedef { 1 } else { 0 };

    // Extract the struct name
    let name_idx = struct_token_idx + 1;
    if name_idx >= tokens.len() || tokens[name_idx].to_string() == "{" {
        // Anonymous struct
        report!(context, "struct_handler", Info, Extract, 
            "Anonymous struct found", true);
        return Ok(None);
    }

    let struct_name = tokens[name_idx].to_string();
    report!(context, "struct_handler", Info, Extract, 
        format!("Found struct: {}", struct_name), true);

    // Look for the opening brace
    let mut brace_idx = name_idx + 1;
    while brace_idx < tokens.len() && tokens[brace_idx].to_string() != "{" {
        brace_idx += 1;
    }

    if brace_idx >= tokens.len() {
        // This might be a forward declaration
        if name_idx + 1 < tokens.len() && tokens[name_idx + 1].to_string() == ";" {
            // Forward declaration
            report!(context, "struct_handler", Info, Extract, 
                format!("Forward declaration of struct {}", struct_name), true);
            let extracted_struct = ExtractedStruct {
                name: struct_name,
                fields: vec![],
                tokens: tokens.to_vec(),
                is_typedef,
                typedef_name: None,
                is_forward_declaration: true,
                original_code: tokens.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(" "),
            };

            return Ok(Some(ExtractedElement::Struct(extracted_struct)));
        } else {
            report!(context, "struct_handler", Error, Extract, 
                format!("Could not find opening brace for struct {}", struct_name), true);
            return Ok(None);
        }
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
                    typedef_name = Some(tokens[i-1].to_string());
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

        if let Some((field_name, field_type)) = parse_field_declaration(field_tokens, context) {
            fields.push((field_name, field_type));
        } else {
            report!(context, "struct_handler", Error, Extract, 
                format!("Could not parse field declaration: {:?}", field_tokens), true);
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
        original_code: tokens.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(" "),
    };

    Ok(Some(ExtractedElement::Struct(extracted_struct)))
}

/// Parse a field declaration into name and type
fn parse_field_declaration(tokens: &[Token], context: &Context) -> Option<(String, Vec<Token>)> {
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
        if token.to_string() == ":" || token.to_string() == "[" || 
           crate::extract::is_c_qualifier_or_specifier(token) {
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
            
            let field_name = tokens[i-1].to_string();
            
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
                let mut type_tokens = tokens[0..i-1].to_vec();
                
                // Add array size information to the type tokens
                type_tokens.push(tokens[i].clone()); // [
                for j in i+1..=close_pos {
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
            let field_name = tokens[i+1].to_string();
            
            // Field type is everything before the * plus the *
            let mut field_type = tokens[0..i].to_vec();
            field_type.push(token.clone());
            
            return Some((field_name, field_type));
        }
    }
    
    // Also check for name immediately following * without spaces
    for (i, token) in tokens.iter().enumerate() {
        if i > 0 && tokens[i-1].to_string() == "*" {
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
            let field_name = tokens[i-1].to_string();
            
            // Field type is everything before the name, and we'll include the bit width info
            let mut field_type = if i > 1 {
                tokens[0..i-1].to_vec()
            } else {
                vec![]
            };
            
            // Add bit width information
            field_type.push(token.clone()); // :
            field_type.push(tokens[i+1].clone()); // bit width
            
            return Some((field_name, field_type));
        }
    }
    
    None
}


/// Legacy conversion function
fn convert_struct(
    tokens: &[Token],
    context: &mut Context,
) -> Result<Option<ConvertedElement>, ConversionError> {
    // Get the TokenConverter from the Context
    let id = get_id("convert_struct");
    if let ExtractedElement::Struct(element) = extract_struct(tokens, context)?.unwrap() {
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
            let rust_type = if let Some(converted) = convert_type_tokens(field_type_tokens.clone()) {
                converted
            } else {
                // Fallback to string-based conversion
                // Convert field type to Rust
                let field_type = field_type_tokens.iter()
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
        if element.typedef_name.is_some() && element.typedef_name.clone().unwrap() != element.name.clone() {
            rust_code.push_str(&format!("pub type {} = {};\n", element.name.clone(), element.typedef_name.unwrap()));
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
fn redirect_struct(tokens: &[Token], context: &mut Context) -> Result<Option<String>, ConversionError> {
    report!(context, "struct_handler", Info, Report, 
        "Checking if struct tokens should be redirected", true);
    
    // Check if this is actually a union (should go to a union handler if we have one)
    if tokens.iter().any(|t| t.to_string() == "union") {
        report!(context, "struct_handler", Info, Report, 
            "Redirecting to union handler", true);
        return Ok(Some("union".to_string()));
    }
    
    // Check if this is actually an enum disguised as a struct
    if tokens.iter().any(|t| t.to_string() == "enum") {
        report!(context, "struct_handler", Info, Report, 
            "Redirecting to enum handler", true);
        return Ok(Some("enum".to_string()));
    }
    
    // Check if this is a typedef that doesn't involve struct definition
    if tokens.len() >= 2 && tokens[0].to_string() == "typedef" && tokens[1].to_string() != "struct" {
        report!(context, "struct_handler", Info, Report, 
            "Redirecting to typedef handler", true);
        return Ok(Some("typedef".to_string()));
    }
    
    // Check if this contains function pointers that might need special handling
    let has_function_pointer = tokens.windows(2).any(|w| {
        w[0].to_string() == "(" && w[1].to_string() == "*"
    });
    
    if has_function_pointer {
        report!(context, "struct_handler", Info, Report, 
            "Complex function pointer detected, might need special handling", true);
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
        report!(context, "struct_handler", Info, Report, 
            "Redirecting to global handler (struct variable)", true);
        return Ok(Some("global".to_string()));
    }
    
    // No redirection needed
    Ok(None)
}
