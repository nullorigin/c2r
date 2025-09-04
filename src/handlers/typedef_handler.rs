use crate::context::{Context, HandlerReport, ReportLevel::{Info, Error}, HandlerPhase::{Process, Handle, Extract, Convert, Report}};
use crate::error::ConversionError;
use crate::handler::HandlerResult;
use crate::{get_id, Token, ConvertedElement, ConvertedTypedef, report};

use super::common::{not_handled, replace_with};
use crate::convert_type;
use crate::extract::ExtractedElement;
use crate::extract::ExtractedTypedef;
use crate::Handler;

/// Creates a typedef handler that can detect and convert C typedef declarations
pub fn create_typedef_handler() -> Handler {
    let handler_id = get_id("typedef_handler");
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
        None,
        None,
        Some(redirect_typedef)
    )
}

/// Process callback: Initializes and confirms this handler can handle the tokens
fn process_typedef(tokens: &[Token], context: &mut Context) -> Result<bool,ConversionError> {
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
            report!(context, "typedef_handler", Info, Process, 
                format!("Typedef {} detected, redirecting to specialized handler", second_token), true);
            return Ok(false);
        }
    }
    
    // Check for function pointer typedefs
    if tokens.iter().any(|t| t.to_string().contains("(*")) {
        report!(context, "typedef_handler", Info, Process, 
            "Function pointer typedef detected", true);
        return Ok(true);
    }
    
    // Check for array typedefs
    if tokens.iter().any(|t| t.to_string() == "[") {
        report!(context, "typedef_handler", Info, Process, 
            "Array typedef detected", true);
        return Ok(true);
    }
    
    // Standard type alias
    report!(context, "typedef_handler", Info, Process, 
        "Standard typedef detected", true);
    Ok(true)
}

/// Processes a typedef declaration
fn handle_typedef(tokens: &[Token], context: &mut Context) -> Result<HandlerResult, ConversionError> {
    let id = get_id("handle_typedef");
    report!(context, "typedef_handler", Info, Handle, 
        "Typedef handler processing tokens", true);
    
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
        report!(context, "typedef_handler", Error, Handle, 
            "Typedef missing semicolon", true);
        return not_handled();
    }
    
    // The name is the token just before the semicolon
    let name_pos = semicolon_pos - 1;
    if name_pos < 1 {
        report!(context, "typedef_handler", Error, Handle, 
            "Typedef missing name", true);
        return not_handled();
    }
    
    let typedef_name = tokens[name_pos].to_string();
    report!(context, "typedef_handler", Info, Handle, 
        format!("Found typedef: {}", typedef_name), true);
    
    // The source type is everything between "typedef" and the name
    let source_type_tokens = &tokens[1..name_pos];
    if source_type_tokens.is_empty() {
        report!(context, "typedef_handler", Error, Handle, 
            "Typedef missing source type", true);
        return not_handled();
    }
    
    // Convert to Rust
    let rust_code = convert_typedef_to_rust(
        typedef_name.as_str(),
        source_type_tokens,
        context,
    )?;
    
    replace_with(rust_code,id)
}

/// Extracts a typedef as an ExtractedElement
pub fn extract_typedef(tokens: &[Token], context: &mut Context) -> Result<Option<ExtractedElement>, ConversionError> {
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
    let semicolon_pos = tokens.iter()
        .position(|token| token.to_string() == ";")
        .unwrap_or(tokens.len());

    if semicolon_pos == tokens.len() {
        report!(context, "typedef_handler", Error, Extract, 
            "Typedef missing semicolon", true);
        return Ok(None);
    }

    // The name is the token just before the semicolon
    let name_pos = semicolon_pos - 1;
    if name_pos < 1 {
        report!(context, "typedef_handler", Error, Extract, 
            "Typedef missing name", true);
        return Ok(None);
    }

    let typedef_name = tokens[name_pos].to_string();
    report!(context, "typedef_handler", Info, Extract, 
        format!("Found typedef: {}", typedef_name), true);

    // The source type is everything between "typedef" and the name
    let source_type_tokens = tokens[1..name_pos].to_vec();
    if source_type_tokens.is_empty() {
        report!(context, "typedef_handler", Error, Extract, 
            "Typedef missing source type", true);
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
        if *pos + 2 < tokens.len() && 
           tokens[*pos + 1].to_string() == "*" && 
           !crate::extract::is_c_qualifier_or_specifier(&tokens[*pos + 2]) {
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
            if i > 0 && !crate::extract::is_c_qualifier_or_specifier(&tokens[i-1]) {
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
    source_type_tokens: &[Token],
    context: &Context,
) -> Result<String, ConversionError> {
    // Form the source type string
    let source_type = source_type_tokens.iter()
        .map(|t| t.to_string())
        .collect::<Vec<_>>()
        .join(" ");
    
    // Check for function pointer typedefs
    if source_type.contains("(*)") || source_type.contains("( * )") {
        return convert_function_pointer_typedef(name, source_type_tokens, context);
    }

    // Check if this is an array typedef
    if source_type.contains('[') && source_type.contains(']') {
        return convert_array_typedef(name, source_type_tokens, context);
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
    source_type_tokens: &[Token],
    context: &Context,
) -> Result<String, ConversionError> {
    // This is a simplified version - a real implementation would parse the function signature
    
    // Try to find the return type (everything before the first open parenthesis)
    let mut return_type_tokens = Vec::new();
    let mut i = 0;
    while i < source_type_tokens.len() && source_type_tokens[i].to_string() != "(" {
        return_type_tokens.push(source_type_tokens[i].clone());
        i += 1;
    }
    
    let return_type_str = return_type_tokens.iter()
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
        if source_type_tokens[i].to_string() == ")" && i + 1 < source_type_tokens.len() && source_type_tokens[i+1].to_string() == "(" {
            // Found the parameter list start
            i += 2; // Skip the ) and (
            let mut param_start = i;
            
            // Find parameter list end
            while i < source_type_tokens.len() && source_type_tokens[i].to_string() != ")" {
                if source_type_tokens[i].to_string() == "," {
                    // Parameter delimiter found
                    let param_type = source_type_tokens[param_start..i].iter()
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
                let param_type = source_type_tokens[param_start..i].iter()
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
    source_type_tokens: &[Token],
    context: &Context,
) -> Result<String, ConversionError> {
    // Find the base type (everything before the first '[')
    let mut base_type_tokens = Vec::new();
    for token in source_type_tokens {
        if token.to_string() == "[" {
            break;
        }
        base_type_tokens.push(token.clone());
    }

    let base_type_str = base_type_tokens.iter()
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
        if source_type_tokens[i].to_string() == "[" && i + 1 < source_type_tokens.len() && i + 2 < source_type_tokens.len() && source_type_tokens[i+2].to_string() == "]" {
            array_size = source_type_tokens[i+1].to_string();
            break;
        }
    }

    let rust_code = if array_size.is_empty() {
        // Use slice for unknown size
        format!("pub type {} = [{}];\n", name, rust_base_type)
    } else {
        // Use array with specific size
        format!("pub type {} = [{}; {}];\n", name, rust_base_type, array_size)
    };

    Ok(rust_code)
}

/// Convert callback: Does the actual conversion of C to Rust code
fn convert_typedef(
    tokens: &[Token],
    context: &mut Context,
) -> Result<Option<ConvertedElement>, ConversionError> {
    report!(context, "typedef_handler", Info, Convert, 
        format!("Converting typedef from {} tokens", tokens.len()), true);
    
    if let Some(ExtractedElement::Typedef(extracted_typedef)) = extract_typedef(tokens, context).unwrap_or_default() {
        let rust_code = convert_typedef_to_rust(
            &extracted_typedef.name,
            &extracted_typedef.original_type,
            context,
        )?;
        
        let id = get_id("convert_typedef");
        Ok(Some(ConvertedElement::Typedef(ConvertedTypedef {
            target_type: "".to_string(), // Default empty target type
            rust_code,
            is_public: false,
        })))
    } else {
        Err(ConversionError::new("Could not extract typedef for conversion"))
    }
}

/// Redirect callback: Handles cases where this handler should pass tokens to a different handler
fn redirect_typedef(tokens: &[Token], result: HandlerResult, context: &mut Context) -> Result<HandlerResult, ConversionError> {
    let id = get_id("redirect_typedef");
    report!(context, "typedef_handler", Info, Report, 
        "Checking if typedef tokens should be redirected", true);
    
    // Check if this is actually typedef struct/enum/union
    if tokens.len() >= 3 && tokens[0].to_string() == "typedef" {
        match tokens[1].to_string().as_str() {
            "struct" => {
                report!(context, "typedef_handler", Info, Report, 
                    "Redirecting to struct handler (typedef struct)", true);
                return Ok(HandlerResult::Redirected(
                    Some(tokens.to_vec()),
                    "typedef_handler".to_string(),
                    id,
                    get_id("struct_handler")
                ));
            },
            "enum" => {
                report!(context, "typedef_handler", Info, Report, 
                    "Redirecting to enum handler (typedef enum)", true);
                return Ok(HandlerResult::Redirected(
                    Some(tokens.to_vec()),
                    "typedef_handler".to_string(),
                    id,
                    get_id("enum_handler")
                ));
            },
            "union" => {
                report!(context, "typedef_handler", Info, Report, 
                    "Redirecting to union handler (typedef union)", true);
                return Ok(HandlerResult::Redirected(
                    Some(tokens.to_vec()),
                    "typedef_handler".to_string(),
                    id,
                    get_id("union_handler")
                ));
            },
            _ => {}
        }
    }
    
    // Check if this contains function declarations that should go to function handler
    if tokens.iter().any(|t| t.to_string().contains("(")) && 
       tokens.iter().any(|t| t.to_string().contains(")")) &&
       !tokens.iter().any(|t| t.to_string().contains("(*")) {
        // This might be a function declaration masquerading as typedef
        report!(context, "typedef_handler", Info, Report, 
            "Possible function declaration, redirecting to function handler", true);
        return Ok(HandlerResult::Redirected(
            Some(tokens.to_vec()),
            "typedef_handler".to_string(),
            id,
            get_id("function_handler")
        ));
    }
    
    // Check if this is actually a variable declaration
    if tokens.iter().any(|t| t.to_string() == "=") {
        report!(context, "typedef_handler", Info, Report, 
            "Variable assignment detected, redirecting to global handler", true);
        return Ok(HandlerResult::Redirected(
            Some(tokens.to_vec()),
            "typedef_handler".to_string(),
            id,
            get_id("global_handler")
        ));
    }
    
    // No redirection needed
    Ok(result)
}
