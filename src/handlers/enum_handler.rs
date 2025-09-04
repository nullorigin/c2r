use crate::context::{Context, HandlerReport, ReportLevel::{Info, Error}, HandlerPhase::{Process, Handle, Extract, Convert, Report}};
use crate::error::ConversionError;
use crate::handler::HandlerResult;
use crate::{get_id, matches, ConvertedElement, ConvertedEnum, ExtractedElement, ExtractedEnum, Token, report};

use super::common::{find_matching_token, not_handled, replace_with};

/// Creates an enum handler that can detect and convert C enums
pub fn create_enum_handler() -> crate::handler::Handler {
    let handler_id = get_id("enum_handler");
    let handler_role = "enum";
    let priority = 100; // Standard priority

    super::create_handler(
        handler_id,
        handler_role,
        priority,
        Some(process_enum),
        Some(handle_enum),
        Some(extract_enum),
        Some(convert_enum),
        None,
        None,
        Some(redirect_enum)
    )
}

/// Process callback: Initializes and confirms this handler can handle the tokens
fn process_enum(tokens: &[Token], context: &mut Context) -> Result<bool,ConversionError> {
    // Validate input
    if tokens.is_empty() {
        return Ok(false);
    }
    
    // Check for enum keyword patterns
    if tokens[0].to_string() == "enum" {
        // enum name { ... }
        if tokens.len() >= 3 && tokens[2].to_string() == "{" {
            report!(context, "enum_handler", Info, Process, 
                format!("Enum definition detected: {}", tokens[1].to_string()), true);
            return Ok(true);
        }
        
        // enum name;
        if tokens.len() >= 3 && tokens[2].to_string() == ";" {
            report!(context, "enum_handler", Info, Process, 
                format!("Enum forward declaration detected: {}", tokens[1].to_string()), true);
            return Ok(true);
        }
        
        // Anonymous enum: enum { ... }
        if tokens.len() >= 2 && tokens[1].to_string() == "{" {
            report!(context, "enum_handler", Info, Process, 
                "Anonymous enum detected", true);
            return Ok(true);
        }
    }
    
    // Check for typedef enum patterns
    if tokens.len() >= 3 && tokens[0].to_string() == "typedef" && tokens[1].to_string() == "enum" {
        report!(context, "enum_handler", Info, Process, 
            "Typedef enum pattern detected", true);
        return Ok(true);
    }
    
    Ok(false)
}

/// Processes an enum declaration
fn handle_enum(tokens: &[Token], _context: &mut Context) -> Result<HandlerResult, ConversionError> {
    let id = get_id("enum");
    if let Some(enum_name_token) = tokens.get(1) {
        let enum_name = enum_name_token.to_string();

        // Find the start and end of the enum body
        if let Some(body_start) = tokens.iter().position(|t| t.to_string() == "{") {
            if let Some(body_end) = find_matching_token(tokens,"{", "}") {
                let variants = &tokens[body_start + 1..body_end];
                let rust_variants = variants
                    .iter()
                    .filter(|t| t.to_string() != ",")
                    .map(|t| t.to_string())
                    .collect::<Vec<String>>()
                    .join(",\n    ");

                let rust_code = format!(
                    "#[derive(Debug, Clone, Copy, PartialEq, Eq)]\npub enum {} {{\n    {}\n}}",
                    enum_name,
                    rust_variants
                );

                return replace_with(rust_code, id);
            }
        }
    }

    not_handled()
}

/// Extracts an enum as an ExtractedElement
pub fn extract_enum(tokens: &[Token], context: &mut Context) -> Result<Option<ExtractedElement>, ConversionError> {
    let is_typedef = tokens[0].to_string() == "typedef";
    let enum_token_idx = if is_typedef { 1 } else { 0 };

    // Extract the enum name
    let name_idx = enum_token_idx + 1;

    // Check if this is an anonymous enum
    let is_anonymous = name_idx >= tokens.len() || tokens[name_idx].to_string() == "{";
    let enum_name = if is_anonymous {
        "AnonymousEnum".to_string()
    } else {
        tokens[name_idx].to_string()
    };

    // Look for the opening brace
    let brace_idx = if is_anonymous {
        name_idx
    } else {
        name_idx + 1
    };

    if brace_idx >= tokens.len() || tokens[brace_idx].to_string() != "{" {
        // This might be a forward declaration
        if !is_anonymous && name_idx + 1 < tokens.len() && tokens[name_idx + 1].to_string() == ";" {
            // Forward declaration
            let extracted_enum = ExtractedEnum {
                name: enum_name,
                values: vec![],
                is_typedef: is_typedef,
                typedef_name: None,
                is_forward_declaration: true,
                original_code: tokens.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(" "),
                tokens: tokens.to_vec(),
            };

            return Ok(Some(ExtractedElement::Enum(extracted_enum)));
        } else {
            return Err(ConversionError::new("Could not find opening brace for enum"));
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

    // Look for typedef name (if this is a typedef enum)
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

    // Extract enum values
    let mut values = Vec::new();
    let mut i = 0;

    // Extract enum values from body tokens
    while i < body_tokens.len() {
        // Skip commas and whitespace
        if body_tokens[i].to_string() == "," {
            i += 1;
            continue;
        }

        // Extract the enum value name
        if i < body_tokens.len() {
            let enum_name = body_tokens[i].to_string();
            i += 1;

            // Check if this value has an explicit assignment
            let mut explicit_value = None;
            if i < body_tokens.len() && body_tokens[i].to_string() == "=" {
                i += 1; // Skip the equals sign

                // Parse the value - this is simplified
                // A real implementation would handle complex expressions
                if i < body_tokens.len() {
                    // Try to parse as integer
                    if let Ok(value) = body_tokens[i].to_string().parse::<i64>() {
                        explicit_value = Some(value);
                    }
                    i += 1;
                }
            }

            // Add the enum value to our list
            values.push((enum_name, explicit_value));

            // Skip to next comma or end
            while i < body_tokens.len() && body_tokens[i].to_string() != "," {
                i += 1;
            }
        }
    }

    let extracted_enum = ExtractedEnum {
        name: enum_name,
        values,
        is_typedef: is_typedef,
        typedef_name: typedef_name,
        is_forward_declaration: false,
        original_code: tokens.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(" "),
        tokens: tokens.to_vec(),
    };

    Ok(Some(ExtractedElement::Enum(extracted_enum)))
}

/// Convert callback: Does the actual conversion of C to Rust code
fn convert_enum(
    tokens: &[Token],
    context: &mut Context,
) -> Result<Option<ConvertedElement>, ConversionError> {
    report!(context, "enum_handler", Info, Convert, 
        format!("Converting enum from {} tokens", tokens.len()), true);
   if let Some(ExtractedElement::Enum(extracted_enum)) = extract_enum(tokens, context)? {
       let typedef_name = extracted_enum.typedef_name.clone().unwrap_or_else(|| extracted_enum.name.clone());
       let enum_name = extracted_enum.name.clone();
       let values = extracted_enum.values.clone();
       let mut rust_code = String::new();

       // Add documentation comment if available
       if let Some(docs) = super::common::extract_docs(extracted_enum.tokens.as_slice()) {
           rust_code.push_str("/// ");
           rust_code.push_str(&docs.replace("\n", "\n/// "));
           rust_code.push_str("\n");
       }

       // Add enum declaration with common derives
       rust_code.push_str("#[derive(Debug, Clone, Copy, PartialEq, Eq)]\n");

       // Add representation attribute to match C enum size
       rust_code.push_str("#[repr(C)]\n");

       // Use typedef name if available, otherwise use the enum name
       let display_name = typedef_name.clone();

       rust_code.push_str(&format!("pub enum {} {{\n", display_name));

       // Convert enum values
       for (value_name, explicit_value) in values.iter() {
           // Add the enum variant
           if let Some(value) = explicit_value {
               rust_code.push_str(&format!("    {} = {},\n", value_name, value));
           } else {
               rust_code.push_str(&format!("    {},\n", value_name));
           }
       }

       rust_code.push_str("}\n");

       // If this is a typedef enum, add a type alias
       if extracted_enum.is_typedef && extracted_enum.typedef_name.is_some() && !typedef_name.is_empty() && typedef_name != enum_name {
           rust_code.push_str(&format!("pub type {} = {};\n", enum_name, typedef_name));
       }
       
       let id = get_id("convert_enum");
       Ok(Some(ConvertedElement::Enum(ConvertedEnum {
           variants: Vec::new(), // Default empty variants
           rust_code,
           is_public: false,
           repr: None,
       })))
   } else {
       Err(ConversionError::new("Unsupported enum element"))
   }
}

/// Redirect callback: Handles cases where this handler should pass tokens to a different handler
fn redirect_enum(tokens: &[Token], result: HandlerResult, context: &mut Context) -> Result<HandlerResult, ConversionError> {
    let id = get_id("redirect_enum");
    report!(context, "enum_handler", Info, Report, 
        "Checking if enum tokens should be redirected", true);
    
    // Check if this is actually a struct disguised as an enum
    if tokens.iter().any(|t| t.to_string() == "struct") {
        report!(context, "enum_handler", Info, Report, 
            "Redirecting to struct handler", true);
        return Ok(HandlerResult::Redirected(
            Some(tokens.to_vec()),
            "enum_handler".to_string(),
            id,
            get_id("struct_handler")
        ));
    }
    
    // Check if this is actually a union
    if tokens.iter().any(|t| t.to_string() == "union") {
        report!(context, "enum_handler", Info, Report, 
            "Redirecting to union handler", true);
        return Ok(HandlerResult::Redirected(
            Some(tokens.to_vec()),
            "enum_handler".to_string(),
            id,
            get_id("union_handler")
        ));
    }
    
    // Check if this is a typedef that doesn't involve enum definition
    if tokens.len() >= 2 && tokens[0].to_string() == "typedef" && tokens[1].to_string() != "enum" {
        report!(context, "enum_handler", Info, Report, 
            "Redirecting to typedef handler", true);
        return Ok(HandlerResult::Redirected(
            Some(tokens.to_vec()),
            "enum_handler".to_string(),
            id,
            get_id("typedef_handler")
        ));
    }
    
    // Check if this is actually a variable declaration with enum type
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
    
    // If we have an assignment but no enum definition braces, might be a variable declaration
    if has_assignment && brace_count == 0 {
        report!(context, "enum_handler", Info, Report, 
            "Redirecting to global handler (enum variable)", true);
        return Ok(HandlerResult::Redirected(
            Some(tokens.to_vec()),
            "enum_handler".to_string(),
            id,
            get_id("global_handler")
        ));
    }
    
    // No redirection needed
    Ok(result)
}