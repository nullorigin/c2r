use super::common::{not_handled, replace_with};
use crate::context::{Context, HandlerReport, ReportLevel::{Info, Error}, HandlerPhase::{Process, Handle, Extract, Convert, Report}};
use crate::error::ConversionError;
use crate::handler::HandlerResult;
use crate::{get_id, ConvertedGlobal, Token, ConvertedElement, convert_type, report};
use crate::extract::ExtractedElement;
use crate::extract::ExtractedGlobal;

/// Creates a global variable handler that can detect and convert C global variables
pub fn create_global_handler() -> crate::handler::Handler {
    let handler_id = get_id("global_handler");
    let handler_role = "global";
    let priority = 90; // Lower than function/struct/enum but higher than simple statements
    
    super::create_handler(
        handler_id,
        handler_role,
        priority,
        Some(process_global),
        Some(handle_global),
        Some(extract_global),
        Some(convert_global),
        None,
        None,
        Some(redirect_global)
    )
}

/// Process callback: Initializes and confirms this handler can handle the tokens
fn process_global(tokens: &[Token], context: &mut Context) -> Result<bool,ConversionError> {
    let _id = get_id("process_global");
    
    if tokens.is_empty() {
        return Ok(false);
    }
    
    // Skip function, struct, enum, typedef, etc.
    if contains_keyword(tokens, "function") || 
       contains_keyword(tokens, "struct") || 
       contains_keyword(tokens, "enum") || 
       contains_keyword(tokens, "typedef") ||
       contains_keyword(tokens, "return") {
        return Ok(false);
    }
    
    // Check if it's a variable declaration (ends with semicolon)
    // and doesn't have local storage class specifiers
    let has_semicolon = tokens.last().map_or(false, |t| t.to_string() == ";");
    
    if !has_semicolon {
        return Ok(false);
    }
    
    // Check for static or extern keywords which are common for globals
    if contains_keyword(tokens, "static") || contains_keyword(tokens, "extern") {
        return Ok(true);
    }
    
    // Check if this starts with a type name (simple check)
    // A more robust implementation would check against a type registry
    if is_type_keyword(&tokens[0].to_string()) {
        return Ok(true);
    }
    
    Ok(false)
}

/// Processes a global variable declaration
fn handle_global(tokens: &[Token], context: &mut Context) -> Result<HandlerResult, ConversionError> {
    let id = get_id("handle_global");
    report!(context, "global_handler", Info, Handle, 
        "Global handler processing tokens", true);
    
    // Skip certain patterns that aren't globals
    if contains_keyword(tokens, "function") || 
       contains_keyword(tokens, "struct") || 
       contains_keyword(tokens, "enum") || 
       contains_keyword(tokens, "typedef") ||
       contains_keyword(tokens, "return") {
        return not_handled();
    }
    
    // Ensure it ends with semicolon
    if !tokens.last().map_or(false, |t| t.to_string() == ";") {
        return not_handled();
    }
    
    // Extract the declaration components
    if let Some(ExtractedElement::Global(element)) = extract_global(tokens,context)?
    {
        report!(context, "global_handler", Info, Handle, 
            format!("Found global: {} {} {}{}",
                element.storage_class.as_deref().unwrap_or(""),
                element.type_name,
                element.name,
                if let Some(size) = &element.array_size { format!("[{}]", size) } else { String::new() }
            ), true);

        // Convert to Rust
        let rust_code = convert_global_to_rust(
            element.storage_class.as_deref(),
            element.is_const,
            &element.type_name,
            &element.name,
            element.array_size.as_deref(),
            element.initializer.as_deref(),
            context,
        )?;
        replace_with(rust_code,id)
    } else {
        Err(ConversionError::new("Global handler failed to extract global"))
    }
}
/// Extracts a global variable as an ExtractedElement
pub fn extract_global(tokens: &[Token], context: &mut Context) -> Result<Option<ExtractedElement>, ConversionError> {
    report!(context, "global_handler", Info, Extract, 
        "Global handler processing tokens", true);

    // Skip certain patterns that aren't globals
    if contains_keyword(tokens, "function") ||
       contains_keyword(tokens, "struct") ||
       contains_keyword(tokens, "enum") ||
       contains_keyword(tokens, "typedef") ||
       contains_keyword(tokens, "return") {
        return Ok(None);
    }

    // Ensure it ends with semicolon
    if !tokens.last().map_or(false, |t| t.to_string() == ";") {
        return Ok(None);
    }

    // Extract the declaration components inline
    let mut storage_class = None;
    let mut is_const = false;
    let mut type_tokens = Vec::new();
    let mut name = String::new();
    let mut array_size = None;
    let mut initializer = None;

    let mut i = 0;

    // Process storage class and type qualifiers
    while i < tokens.len() - 1 { // Skip the semicolon at the end
        let token = tokens[i].to_string();

        match token.as_str() {
            "static" | "extern" => {
                storage_class = Some(token.clone());
                i += 1;
                continue;
            },
            "const" => {
                is_const = true;
                i += 1;
                continue;
            },
            _ => {}
        }

        // Process type name until we find the variable name
        if name.is_empty() {
            // Check if this is an identifier that could be the variable name
            if i > 0 && !is_keyword(&token) && !token.starts_with("*") {
                // The previous tokens form the type
                if let Some(next_token) = tokens.get(i + 1) {
                    let next = next_token.to_string();

                    // Check if this is actually the variable name
                    if next == ";" || next == "=" || next == "[" {
                        name = token;
                        i += 1;
                        continue;
                    }
                }
            }

            // Still part of the type
            type_tokens.push(tokens[i].clone());
            i += 1;
            continue;
        }

        // Process array size if present
        if token == "[" {
            i += 1;
            if i < tokens.len() - 1 {
                array_size = Some(tokens[i].to_string());
                i += 1;

                // Skip the closing bracket
                if i < tokens.len() && tokens[i].to_string() == "]" {
                    i += 1;
                }
            }
            continue;
        }

        // Process initializer if present
        if token == "=" {
            i += 1;

            // Extract initializer (everything up to the semicolon)
            let init_start = i;
            let init_end = tokens.len() - 1; // Skip the semicolon

            let mut initializer_tokens = Vec::new();
            for j in init_start..init_end {
                initializer_tokens.push(tokens[j].to_string());
            }

            initializer = Some(initializer_tokens.join(" "));
            break;
        }

        i += 1;
    }

    // Combine type tokens into the base type
    let base_type = type_tokens.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(" ");

    // If we didn't find a name, use the last token before the semicolon as the name
    if name.is_empty() && !type_tokens.is_empty() {
        name = type_tokens.pop().unwrap().to_string();
    }

    if name.is_empty() || base_type.is_empty() {
        report!(context, "global_handler", Error, Extract, 
            "Could not extract global variable components", true);
        return Ok(None);
    }

    report!(context, "global_handler", Info, Extract, 
        format!("Found global: {} {} {}{}",
            storage_class.as_deref().unwrap_or(""),
            base_type,
            name,
            if let Some(size) = &array_size { format!("[{}]", size) } else { String::new() }
        ), true);

    let is_static = storage_class.as_deref() == Some("static");
    let is_extern = storage_class.as_deref() == Some("extern");

    let extracted_global = ExtractedGlobal {
        name,
        array_dims: array_size.as_deref().map_or_else(Vec::new, |s| vec![s.to_string()]),
        type_name: base_type,
        storage_class,
        is_const,
        is_static,
        tokens: tokens.to_vec(),
        is_extern,
        array_size,
        initializer,
        original_code: tokens.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(" "),
        initial_value: None
    };

    Ok(Some(ExtractedElement::Global(extracted_global)))
}

/// Converts a C global variable to Rust
fn convert_global_to_rust(
    storage_class: Option<&str>,
    is_const: bool,
    base_type: &str,
    name: &str,
    array_size: Option<&str>,
    initializer: Option<&str>,
    context: &Context,
) -> Result<String, ConversionError> {
    let mut rust_code = String::new();
    
    // Convert C type to Rust type
    let rust_type = if let Some(rt) = convert_type(base_type) {
        rt.to_string()
    } else {
        base_type.to_string()
    };
    
    // Determine Rust mutability
    let is_mut = !is_const;
    
    // Determine storage visibility
    let storage = match storage_class {
        Some("static") => "static",
        Some("extern") => "extern \"C\"",
        _ => "pub" // Default to public for globals
    };
    
    // Start building the Rust declaration
    rust_code.push_str(storage);
    rust_code.push(' ');
    
    if is_mut {
        rust_code.push_str("mut ");
    }
    
    rust_code.push_str(name);
    rust_code.push_str(": ");
    
    // Add array type if needed
    if let Some(size) = array_size {
        rust_code.push_str(&format!("[{}; {}]", rust_type, size));
    } else {
        rust_code.push_str(&rust_type);
    }
    
    // Add initializer if present
    if let Some(init) = initializer {
        rust_code.push_str(&format!(" = {}", init));
    } else {
        // Add default initializer if appropriate
        if storage != "extern \"C\"" {
            if let Some(size) = array_size {
                // Default array initializer
                rust_code.push_str(&format!(" = [{};  {}]", default_value_for_type(&rust_type), size));
            } else {
                // Default scalar initializer
                rust_code.push_str(&format!(" = {}", default_value_for_type(&rust_type)));
            }
        }
    }
    
    rust_code.push_str(";\n");
    Ok(rust_code)
}
/// Helper function to check if the token list contains a specific keyword
fn contains_keyword(tokens: &[Token], keyword: &str) -> bool {
    tokens.iter().any(|t| t.to_string() == keyword)
}

/// Helper function to check if a token is a keyword
fn is_keyword(token: &str) -> bool {
    match token {
        "auto" | "break" | "case" | "char" | "const" | "continue" | "default" | 
        "do" | "double" | "else" | "enum" | "extern" | "float" | "for" | 
        "goto" | "if" | "int" | "long" | "register" | "return" | "short" | 
        "signed" | "sizeof" | "static" | "struct" | "switch" | "typedef" | 
        "union" | "unsigned" | "void" | "volatile" | "while" => true,
        _ => false,
    }
}

/// Helper function to check if a token is a type keyword
fn is_type_keyword(token: &str) -> bool {
    match token {
        "void" | "char" | "short" | "int" | "long" | "float" | "double" | 
        "signed" | "unsigned" | "bool" | "complex" | "_Bool" | "_Complex" | 
        "_Imaginary" | "size_t" | "ssize_t" | "intptr_t" | "uintptr_t" | 
        "ptrdiff_t" | "FILE" | "va_list" | "int8_t" | "uint8_t" | 
        "int16_t" | "uint16_t" | "int32_t" | "uint32_t" | "int64_t" | "uint64_t" => true,
        _ => false,
    }
}

/// Get a default value for a Rust type
fn default_value_for_type(rust_type: &str) -> &'static str {
    match rust_type {
        "i8" | "i16" | "i32" | "i64" | "isize" | "u8" | "u16" | "u32" | "u64" | "usize" => "0",
        "f32" | "f64" => "0.0",
        "bool" => "false",
        "char" => "'\\0'",
        "&str" => "\"\"",
        "*mut _" | "*const _" => "std::ptr::null_mut()",
        _ => "Default::default()",
    }
}

/// Convert callback: Does the actual conversion of C to Rust code
fn convert_global(tokens: &[Token], context: &mut Context) -> Result<Option<ConvertedElement>, ConversionError> {
    let _id = get_id("convert_global");
    report!(context, "global_handler", Info, Convert, 
        format!("Converting global from {} tokens", tokens.len()), true);
    
    if let Some(ExtractedElement::Global(extracted_global)) = extract_global(tokens, context)? {
        let rust_code = convert_global_to_rust(
            extracted_global.storage_class.as_deref(),
            extracted_global.is_const,
            &extracted_global.type_name,
            &extracted_global.name,
            extracted_global.array_size.as_deref(),
            extracted_global.initializer.as_deref(),
            context,
        )?;
        
        let id = get_id("convert_global");
        Ok(Some(ConvertedElement::Global(ConvertedGlobal {
            var_type: "".to_string(), // Default empty variable type
            initializer: None, // Default no initializer
            rust_code,
            is_const: false,
            is_static: false,
            is_public: false,
        })))
    } else {
        Err(ConversionError::new("Could not extract global for conversion"))
    }
}

/// Redirect callback: Handles cases where this handler should pass tokens to a different handler
fn redirect_global(tokens: &[Token], result: HandlerResult, context: &mut Context) -> Result<HandlerResult, ConversionError> {
    let _id = get_id("redirect_global");
    report!(context, "global_handler", Info, Report, 
        "Checking if global tokens should be redirected", true);
    
    // Check if this is actually a function declaration
    if tokens.iter().any(|t| t.to_string() == "(") && tokens.iter().any(|t| t.to_string() == ")") {
        // Look for function patterns
        let mut paren_count = 0;
        let mut found_identifier_before_paren = false;
        
        for i in 0..tokens.len() {
            let token = tokens[i].to_string();
            if token == "(" {
                paren_count += 1;
                if i > 0 && !is_keyword(&tokens[i-1].to_string()) {
                    found_identifier_before_paren = true;
                }
            } else if token == ")" {
                paren_count -= 1;
            }
        }
        
        if found_identifier_before_paren && paren_count == 0 {
            report!(context, "global_handler", Info, Report, 
                "Redirecting to function handler (function declaration)", true);
            return Ok(HandlerResult::Redirected(
                Some(tokens.to_vec()),
                "global_handler".to_string(),
                _id,
                get_id("function_handler")
            ));
        }
    }
    
    // Check if this is actually a struct/enum/typedef
    if tokens.iter().any(|t| matches!(t.to_string().as_str(), "struct" | "enum" | "typedef")) {
        let first_keyword = tokens.iter()
            .find(|t| matches!(t.to_string().as_str(), "struct" | "enum" | "typedef"))
            .unwrap()
            .to_string();
            
        match first_keyword.as_str() {
            "struct" => {
                report!(context, "global_handler", Info, Report, 
                    "Redirecting to struct handler", true);
                return Ok(HandlerResult::Redirected(
                    Some(tokens.to_vec()),
                    "global_handler".to_string(),
                    _id,
                    get_id("struct_handler")
                ));
            },
            "enum" => {
                report!(context, "global_handler", Info, Report, 
                    "Redirecting to enum handler", true);
                return Ok(HandlerResult::Redirected(
                    Some(tokens.to_vec()),
                    "global_handler".to_string(),
                    _id,
                    get_id("enum_handler")
                ));
            },
            "typedef" => {
                report!(context, "global_handler", Info, Report, 
                    "Redirecting to typedef handler", true);
                return Ok(HandlerResult::Redirected(
                    Some(tokens.to_vec()),
                    "global_handler".to_string(),
                    _id,
                    get_id("typedef_handler")
                ));
            },
            _ => {}
        }
    }
    
    // Check if this contains preprocessor directives
    if tokens.iter().any(|t| t.to_string().starts_with("#")) {
        report!(context, "global_handler", Info, Report, 
            "Redirecting to macro handler (preprocessor directive)", true);
        return Ok(HandlerResult::Redirected(
            Some(tokens.to_vec()),
            "global_handler".to_string(),
            _id,
            get_id("macro_handler")
        ));
    }
    
    // No redirection needed
    Ok(result)
}
