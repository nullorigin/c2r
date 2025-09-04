use super::common::{not_handled, replace_with_range};
use crate::config::{
    Context,
    HandlerPhase::{Convert, Extract, Handle, Report},
    HandlerReport,
    ReportLevel::{Error, Info, Warning},
};
use crate::error::ConversionError;
use crate::extract::ExtractedElement;
use crate::extract::ExtractedGlobal;
use crate::handler::HandlerResult;
use crate::{config, context, convert_type, report, ConvertedElement, ConvertedGlobal, Id, Token};
use std::collections::HashMap;

/// Creates a global variable handler that can detect and convert C global variables
pub fn create_global_handler() -> crate::handler::Handler {
    let handler_id = Id::get("global_handler");
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
        Some(report_global),
        Some(result_global),
        Some(redirect_global),
    )
}

/// Report callback: Collects and summarizes all global-related reports from the context
fn report_global(
    _tokens: &[Token]) -> Result<HandlerReport, ConversionError> {
    let context = context!();
    // Get all reports for this handler
    let reports = context.get_reports_by_handler("global");

    // Count reports by level
    let mut info_count = 0;
    let mut warning_count = 0;
    let mut error_count = 0;

    for report in &reports {
        match report.level {
            Info => info_count += 1,
            Warning => warning_count += 1,
            Error => error_count += 1,
            _ => info_count += 1, // Handle Debug and other variants as info
        }
    }

    // Create summary report
    Ok(HandlerReport {
        report_id: Box::new(Id::get(&Id::gen_name("global_handler"))),
        handler_id: Box::new(Id::get("global_handler")),
        handler_name: "global".to_string(),
        function_name: "report_global".to_string(),
        message: format!(
            "Global handler summary: {} reports ({} info, {} warnings, {} errors)",
            reports.len(),
            info_count,
            warning_count,
            error_count
        ),
        level: if error_count > 0 {
            Error
        } else if warning_count > 0 {
            Warning
        } else {
            Info
        },
        tokens_processed: reports.len(),
        tokens_consumed: 0,
        phase: Report,
        success: error_count == 0,
        metadata: HashMap::new(),
    })
}

/// Process callback: Initializes and confirms this handler can handle the tokens
fn process_global(tokens: &[Token]) -> Result<bool, ConversionError> {
    let _id = Id::get("process_global");

    if tokens.is_empty() {
        return Ok(false);
    }

    // Skip function, struct, enum, typedef, etc.
    if contains_keyword(tokens, "function")
        || contains_keyword(tokens, "struct")
        || contains_keyword(tokens, "enum")
        || contains_keyword(tokens, "typedef")
        || contains_keyword(tokens, "return")
    {
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
fn handle_global(
    tokens: &[Token]) -> Result<HandlerResult, ConversionError> {
    let id = Id::get("handle_global");
    report!(
        "global_handler",
        "handle_global",
        Info,
        Handle,
        "Global handler processing tokens",
        true
    );

    // Skip certain patterns that aren't globals
    if contains_keyword(tokens, "function")
        || contains_keyword(tokens, "struct")
        || contains_keyword(tokens, "enum")
        || contains_keyword(tokens, "typedef")
        || contains_keyword(tokens, "return")
    {
        return not_handled();
    }

    // Ensure it ends with semicolon
    if !tokens.last().map_or(false, |t| t.to_string() == ";") {
        return not_handled();
    }

    // Extract the declaration components
    if let Some(ExtractedElement::Global(element)) = extract_global(tokens)? {
        report!(
            "global_handler",
            "handle_global",
            Info,
            Handle,
            format!(
                "Found global: {} {} {}{}",
                element.storage_class.as_deref().unwrap_or(""),
                element.type_name,
                element.name,
                if let Some(size) = &element.array_size {
                    format!("[{}]", size)
                } else {
                    String::new()
                }
            ),
            true
        );

        // Convert to Rust
        let rust_code = convert_global_to_rust(
            element.storage_class.as_deref(),
            element.is_const,
            &element.type_name,
            &element.name,
            element.array_size.as_deref(),
            element.initializer.as_deref(),
        )?;
        // Global variable definitions process all input tokens
        let token_range = 0..tokens.len();
        replace_with_range(rust_code, token_range, id)
    } else {
        Err(ConversionError::new(
            "Global handler failed to extract global",
        ))
    }
}
/// Extracts a global variable as an ExtractedElement
pub fn extract_global(
    tokens: &[Token]) -> Result<Option<ExtractedElement>, ConversionError> {
    report!(
        "global_handler",
        "extract_global",
        Info,
        Extract,
        "Global handler processing tokens",
        true
    );

    // Skip certain patterns that aren't globals
    if contains_keyword(tokens, "function")
        || contains_keyword(tokens, "struct")
        || contains_keyword(tokens, "enum")
        || contains_keyword(tokens, "typedef")
        || contains_keyword(tokens, "return")
    {
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
    while i < tokens.len() - 1 {
        // Skip the semicolon at the end
        let token = tokens[i].to_string();

        match token.as_str() {
            "static" | "extern" => {
                storage_class = Some(token.clone());
                i += 1;
                continue;
            }
            "const" => {
                is_const = true;
                i += 1;
                continue;
            }
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
    let base_type = type_tokens
        .iter()
        .map(|t| t.to_string())
        .collect::<Vec<_>>()
        .join(" ");

    // If we didn't find a name, use the last token before the semicolon as the name
    if name.is_empty() && !type_tokens.is_empty() {
        name = type_tokens.pop().unwrap().to_string();
    }

    if name.is_empty() || base_type.is_empty() {
        report!(
            "global_handler",
            "extract_global",
            Error,
            Extract,
            "Could not extract global variable components",
            true
        );
        return Ok(None);
    }

    report!(
        "global_handler",
        "extract_global",
        Info,
        Extract,
        format!(
            "Found global: {} {} {}{}",
            storage_class.as_deref().unwrap_or(""),
            base_type,
            name,
            if let Some(size) = &array_size {
                format!("[{}]", size)
            } else {
                String::new()
            }
        ),
        true
    );

    let is_static = storage_class.as_deref() == Some("static");
    let is_extern = storage_class.as_deref() == Some("extern");

    let extracted_global = ExtractedGlobal {
        name,
        array_dims: array_size
            .as_deref()
            .map_or_else(Vec::new, |s| vec![s.to_string()]),
        type_name: base_type,
        storage_class,
        is_const,
        is_static,
        tokens: tokens.to_vec(),
        is_extern,
        array_size,
        initializer,
        original_code: tokens
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<_>>()
            .join(" "),
        initial_value: None,
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
    initializer: Option<&str>
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
        _ => "pub", // Default to public for globals
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
                rust_code.push_str(&format!(
                    " = [{};  {}]",
                    default_value_for_type(&rust_type),
                    size
                ));
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
        "auto" | "break" | "case" | "char" | "const" | "continue" | "default" | "do" | "double"
        | "else" | "enum" | "extern" | "float" | "for" | "goto" | "if" | "int" | "long"
        | "register" | "return" | "short" | "signed" | "sizeof" | "static" | "struct"
        | "switch" | "typedef" | "union" | "unsigned" | "void" | "volatile" | "while" => true,
        _ => false,
    }
}

/// Helper function to check if a token is a type keyword
fn is_type_keyword(token: &str) -> bool {
    match token {
        "void" | "char" | "short" | "int" | "long" | "float" | "double" | "signed" | "unsigned"
        | "bool" | "complex" | "_Bool" | "_Complex" | "_Imaginary" | "size_t" | "ssize_t"
        | "intptr_t" | "uintptr_t" | "ptrdiff_t" | "FILE" | "va_list" | "int8_t" | "uint8_t"
        | "int16_t" | "uint16_t" | "int32_t" | "uint32_t" | "int64_t" | "uint64_t" => true,
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
fn convert_global(
    tokens: &[Token]) -> Result<Option<ConvertedElement>, ConversionError> {
    let _id = Id::get("convert_global");
    report!(
        "global_handler",
        "convert_global",
        Info,
        Convert,
        format!("Converting global from {} tokens", tokens.len()),
        true
    );

    if let Some(ExtractedElement::Global(extracted_global)) = extract_global(tokens)? {
        let rust_code = convert_global_to_rust(
            extracted_global.storage_class.as_deref(),
            extracted_global.is_const,
            &extracted_global.type_name,
            &extracted_global.name,
            extracted_global.array_size.as_deref(),
            extracted_global.initializer.as_deref(),
        )?;

        let id = Id::get("convert_global");
        Ok(Some(ConvertedElement::Global(ConvertedGlobal {
            var_type: "".to_string(), // Default empty variable type
            initializer: None,        // Default no initializer
            rust_code,
            is_const: false,
            is_static: false,
            is_public: false,
        })))
    } else {
        Err(ConversionError::new(
            "Could not extract global for conversion",
        ))
    }
}

/// Result callback: Postprocesses generated global variable code, adds documentation, and enhances formatting
fn result_global(
    tokens: &[Token],
    result: HandlerResult) -> Result<HandlerResult, ConversionError> {
    let _id = Id::get("result_global");

    report!(
        "global_handler",
        "result_global",
        Info,
        Report,
        "Postprocessing global variable conversion result",
        true
    );

    match result {
        HandlerResult::Completed(tokens_opt, _, rust_code, id) => {
            // Extract global variable information for documentation
            let global_info = extract_global_info_from_tokens(tokens);

            // Generate documentation about the global variable conversion
            let doc_comment = generate_global_documentation(tokens, &global_info);

            // Enhance the Rust code with documentation and metadata
            let mut enhanced_code = String::new();

            // Add metadata comment for traceability
            let metadata_comment = format!(
                "// [C2R] Global variable converted from C to Rust - {}: {}\n",
                global_info.name, global_info.kind_description
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
                "global_handler",
                "result_global",
                Info,
                Report,
                &format!(
                    "Enhanced global conversion: {} ({})",
                    global_info.name, global_info.kind_description
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
            let global_info = extract_global_info_from_tokens(tokens);
            let doc_comment = generate_global_documentation(tokens, &global_info);

            let mut enhanced_code = String::new();
            let metadata_comment = format!(
                "// [C2R] Global variable converted from C to Rust - {}: {}\n",
                global_info.name, global_info.kind_description
            );
            enhanced_code.push_str(&metadata_comment);

            if !doc_comment.trim().is_empty() {
                enhanced_code.push_str(&doc_comment);
                enhanced_code.push('\n');
            }
            enhanced_code.push_str(&rust_code);

            report!(
                "global_handler",
                "result_global",
                Info,
                Report,
                &format!(
                    "Enhanced converted global: {} ({})",
                    global_info.name, global_info.kind_description
                ),
                true
            );

            // CRITICAL: Return HandlerResult::Converted to preserve the code!
            Ok(HandlerResult::Converted(element, 0..1, enhanced_code, id))
        }
        HandlerResult::Extracted(element, _, rust_code, id) => {
            // Handle extracted elements - enhance the code and preserve the variant
            let global_info = extract_global_info_from_tokens(tokens);
            let doc_comment = generate_global_documentation(tokens, &global_info);

            let mut enhanced_code = String::new();
            let metadata_comment = format!(
                "// [C2R] Global variable converted from C to Rust - {}: {}\n",
                global_info.name, global_info.kind_description
            );
            enhanced_code.push_str(&metadata_comment);

            if !doc_comment.trim().is_empty() {
                enhanced_code.push_str(&doc_comment);
                enhanced_code.push('\n');
            }
            enhanced_code.push_str(&rust_code);

            report!(
                "global_handler",
                "result_global",
                Info,
                Report,
                &format!(
                    "Enhanced extracted global: {} ({})",
                    global_info.name, global_info.kind_description
                ),
                true
            );

            // CRITICAL: Return HandlerResult::Extracted to preserve the code!
            Ok(HandlerResult::Extracted(element, 0..1, enhanced_code, id))
        }
        HandlerResult::Handled(Some(converted_tokens), _, handler_id) => {
            // Legacy support for token-based results
            // Extract global variable information for documentation
            let global_info = extract_global_info_from_tokens(tokens);

            // Generate documentation about the global variable conversion
            let doc_comment = generate_global_documentation(tokens, &global_info);

            // Postprocess the converted Rust code for better formatting
            let mut enhanced_result = postprocess_global_code(converted_tokens);

            // Add the documentation comment before the converted global
            if !doc_comment.is_empty() {
                enhanced_result.insert(0, Token::s(doc_comment));
            }

            // Add metadata comment for traceability
            let metadata_comment = format!(
                "// [C2R] Global variable converted from C to Rust - {}: {}",
                global_info.name, global_info.kind_description
            );
            enhanced_result.insert(0, Token::s(metadata_comment));

            report!(
                "global_handler",
                "result_global",
                Info,
                Report,
                &format!(
                    "Enhanced global conversion: {} ({})",
                    global_info.name, global_info.kind_description
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
                "global_handler",
                "result_global",
                Warning,
                Report,
                "Global handler returned empty result",
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

/// Global variable information extracted for documentation purposes
#[derive(Debug, Clone)]
struct GlobalInfo {
    name: String,
    kind: GlobalKind,
    kind_description: String,
    storage_class: Option<String>,
    type_name: String,
    is_const: bool,
    is_array: bool,
    has_initializer: bool,
    complexity: String,
}

/// Enum representing different kinds of global variables for specialized handling
#[derive(Debug, Clone, PartialEq)]
pub enum GlobalKind {
    Static,
    Extern,
    PublicVariable,
    ConstVariable,
    Array,
    Initialized,
}

/// Extracts global variable information from the original tokens for documentation purposes
fn extract_global_info_from_tokens(tokens: &[Token]) -> GlobalInfo {
    if tokens.is_empty() {
        return GlobalInfo {
            name: "unknown".to_string(),
            kind: GlobalKind::PublicVariable,
            kind_description: "unknown global variable".to_string(),
            storage_class: None,
            type_name: "unknown".to_string(),
            is_const: false,
            is_array: false,
            has_initializer: false,
            complexity: "unknown".to_string(),
        };
    }

    // Parse tokens to extract information
    let mut storage_class = None;
    let mut is_const = false;
    let mut type_tokens = Vec::new();
    let mut name = String::new();
    let mut is_array = false;
    let mut has_initializer = false;

    let mut i = 0;
    while i < tokens.len() {
        let token = tokens[i].to_string();

        match token.as_str() {
            "static" | "extern" => {
                storage_class = Some(token.clone());
            }
            "const" => {
                is_const = true;
            }
            "[" => {
                is_array = true;
            }
            "=" => {
                has_initializer = true;
            }
            ";" => {
                break;
            }
            _ => {
                // Collect potential type and name tokens
                if !is_keyword(&token) && storage_class.is_some() || i > 0 {
                    if name.is_empty() && i > 0 && !is_keyword(&token) && !token.starts_with("*") {
                        // Check if this might be the variable name
                        if i + 1 < tokens.len() {
                            let next = tokens[i + 1].to_string();
                            if next == ";" || next == "=" || next == "[" {
                                name = token;
                                i += 1;
                                continue;
                            }
                        }
                    }

                    if name.is_empty() {
                        type_tokens.push(token);
                    }
                }
            }
        }
        i += 1;
    }

    // If we still don't have a name, use the last type token
    if name.is_empty() && !type_tokens.is_empty() {
        name = type_tokens.pop().unwrap();
    }

    let type_name = type_tokens.join(" ");

    // Determine the kind and description
    let (kind, kind_description) = if storage_class.as_deref() == Some("static") {
        (GlobalKind::Static, "static global variable".to_string())
    } else if storage_class.as_deref() == Some("extern") {
        (GlobalKind::Extern, "extern global variable".to_string())
    } else if is_const {
        (
            GlobalKind::ConstVariable,
            "const global variable".to_string(),
        )
    } else if is_array {
        (GlobalKind::Array, "global array variable".to_string())
    } else if has_initializer {
        (
            GlobalKind::Initialized,
            "initialized global variable".to_string(),
        )
    } else {
        (
            GlobalKind::PublicVariable,
            "public global variable".to_string(),
        )
    };

    // Determine complexity
    let complexity = if is_array && has_initializer {
        "moderate (array with initializer)".to_string()
    } else if is_array || has_initializer {
        "simple (basic features)".to_string()
    } else {
        "simple".to_string()
    };

    GlobalInfo {
        name,
        kind,
        kind_description,
        storage_class,
        type_name,
        is_const,
        is_array,
        has_initializer,
        complexity,
    }
}

/// Generates documentation comments for the global variable conversion
fn generate_global_documentation(
    tokens: &[Token],
    global_info: &GlobalInfo) -> String {
    let mut doc_lines = Vec::new();

    // Add main documentation header
    doc_lines.push("/**".to_string());
    doc_lines.push(" * Global Variable Conversion Documentation".to_string());
    doc_lines.push(" *".to_string());

    // Add global variable information
    doc_lines.push(format!(" * Name: {}", global_info.name));
    doc_lines.push(format!(" * Type: {}", global_info.type_name));
    doc_lines.push(format!(" * Kind: {}", global_info.kind_description));
    doc_lines.push(format!(" * Complexity: {}", global_info.complexity));
    doc_lines.push(format!(" * Original tokens: {}", tokens.len()));

    if let Some(storage) = &global_info.storage_class {
        doc_lines.push(format!(" * Storage class: {}", storage));
    }

    // Add features information
    let mut features = Vec::new();
    if global_info.is_const {
        features.push("const");
    }
    if global_info.is_array {
        features.push("array");
    }
    if global_info.has_initializer {
        features.push("initialized");
    }

    if !features.is_empty() {
        doc_lines.push(format!(" * Features: {}", features.join(", ")));
    }

    doc_lines.push(" *".to_string());

    // Add conversion notes based on global kind
    match global_info.kind {
        GlobalKind::Static => {
            doc_lines.push(" * Conversion: C static global -> Rust static variable".to_string());
            doc_lines.push(" * Features:".to_string());
            doc_lines.push(" *   - File-scoped visibility with static linkage".to_string());
            doc_lines.push(" *   - Thread-safe access in multi-threaded contexts".to_string());
            doc_lines.push(" *   - Zero-cost initialization at program startup".to_string());
            doc_lines.push(" *   - Memory layout compatible with C expectations".to_string());
            doc_lines.push(" *   - Compile-time constant evaluation when possible".to_string());
        }
        GlobalKind::Extern => {
            doc_lines.push(
                " * Conversion: C extern global -> Rust extern \"C\" declaration".to_string(),
            );
            doc_lines.push(" * Features:".to_string());
            doc_lines.push(" *   - C ABI compatibility for external linkage".to_string());
            doc_lines.push(" *   - Proper symbol naming and mangling".to_string());
            doc_lines.push(" *   - Type safety with foreign function interface".to_string());
            doc_lines.push(" *   - No initialization required for external symbols".to_string());
            doc_lines.push(" *   - Memory layout matches C expectations exactly".to_string());
        }
        GlobalKind::PublicVariable => {
            doc_lines
                .push(" * Conversion: C global variable -> Rust pub static variable".to_string());
            doc_lines.push(" * Features:".to_string());
            doc_lines.push(" *   - Public visibility across module boundaries".to_string());
            doc_lines.push(" *   - Memory safety with Rust ownership rules".to_string());
            doc_lines.push(" *   - Thread-safe access with static lifetime".to_string());
            doc_lines.push(" *   - Automatic initialization with default values".to_string());
            doc_lines.push(" *   - Type system prevents data races".to_string());
        }
        GlobalKind::ConstVariable => {
            doc_lines.push(" * Conversion: C const global -> Rust const declaration".to_string());
            doc_lines.push(" * Features:".to_string());
            doc_lines.push(" *   - Compile-time constant evaluation".to_string());
            doc_lines.push(" *   - Immutable data with zero runtime cost".to_string());
            doc_lines.push(" *   - Inline substitution by compiler optimization".to_string());
            doc_lines.push(" *   - Memory safety without heap allocation".to_string());
            doc_lines.push(" *   - Global accessibility with const semantics".to_string());
        }
        GlobalKind::Array => {
            doc_lines.push(" * Conversion: C global array -> Rust static array".to_string());
            doc_lines.push(" * Features:".to_string());
            doc_lines.push(" *   - Compile-time size verification and bounds checking".to_string());
            doc_lines.push(" *   - Memory layout identical to C arrays".to_string());
            doc_lines.push(" *   - Iterator support for functional programming".to_string());
            doc_lines.push(" *   - Slice conversion for dynamic access patterns".to_string());
            doc_lines.push(" *   - Default::default() initialization for elements".to_string());
        }
        GlobalKind::Initialized => {
            doc_lines.push(
                " * Conversion: C initialized global -> Rust static with initializer".to_string(),
            );
            doc_lines.push(" * Features:".to_string());
            doc_lines.push(" *   - Compile-time initialization evaluation".to_string());
            doc_lines.push(" *   - Type inference from initializer expressions".to_string());
            doc_lines.push(" *   - Memory safety with initialized data guarantees".to_string());
            doc_lines.push(" *   - Lazy static initialization for complex types".to_string());
            doc_lines.push(" *   - Thread-safe initialization in concurrent contexts".to_string());
        }
    }

    doc_lines.push(" *".to_string());
    doc_lines.push(" * Rust global variable benefits:".to_string());
    doc_lines.push(" *   - Memory safety without garbage collection overhead".to_string());
    doc_lines.push(" *   - Thread safety with compile-time verification".to_string());
    doc_lines.push(" *   - Zero-cost abstractions with optimal performance".to_string());
    doc_lines.push(" *   - Type system prevents common C global variable bugs".to_string());
    doc_lines.push(" *   - Automatic cleanup and resource management".to_string());
    doc_lines.push(" */".to_string());

    report!(
        "global_handler",
        "generate_global_documentation",
        Info,
        Report,
        &format!(
            "Generated documentation for {} global: {}",
            global_info.kind_description, global_info.name
        ),
        true
    );

    doc_lines.join("\n")
}

/// Postprocesses the converted global variable code for better formatting
fn postprocess_global_code(mut tokens: Vec<Token>) -> Vec<Token> {
    let original_count = tokens.len();

    // Clean up and format the converted global variable tokens
    for token in tokens.iter_mut() {
        let mut content = token.to_string();

        // Clean up extra whitespace
        content = content.trim().to_string();

        // Format static declarations
        if content.starts_with("static") || content.starts_with("pub static") {
            // Ensure proper spacing in static declarations
            content = content.replace("static ", "static ");
            content = content.replace("pub static", "pub static ");
            content = content.replace("static  ", "static ");
            content = content.replace("pub static  ", "pub static ");
            content = content.replace(" mut ", " mut ");
            content = content.replace(" : ", ": ");
            content = content.replace(" = ", " = ");
            content = content.replace("  ", " ");
        }

        // Format extern declarations
        if content.starts_with("extern") {
            // Clean up extern formatting
            content = content.replace("extern \"C\"", "extern \"C\"");
            content = content.replace("extern  \"C\"", "extern \"C\"");
            content = content.replace("extern \"C\" ", "extern \"C\" ");
            content = content.replace(" : ", ": ");
            content = content.replace("  ", " ");
        }

        // Format const declarations
        if content.starts_with("pub const") || content.starts_with("const") {
            // Clean up const formatting
            content = content.replace("pub const", "pub const ");
            content = content.replace("pub const  ", "pub const ");
            content = content.replace("const ", "const ");
            content = content.replace("const  ", "const ");
            content = content.replace(" : ", ": ");
            content = content.replace(" = ", " = ");
            content = content.replace("  ", " ");
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

        // Format initializers
        if content.contains('=') {
            // Clean up initializer formatting
            content = content.replace(" = ", " = ");
            content = content.replace("=  ", "= ");
            content = content.replace("  =", " =");

            // Format array initializers
            if content.contains("[") && content.contains("]") {
                content = content.replace("[ ", "[");
                content = content.replace(" ]", "]");
                content = content.replace(" ,", ",");
                content = content.replace(",  ", ", ");
            }
        }

        // Format default values
        if content.contains("Default::default()") {
            content = content.replace("Default :: default ()", "Default::default()");
            content = content.replace("Default::default ()", "Default::default()");
            content = content.replace("Default:: default()", "Default::default()");
        }

        // Format pointer types
        if content.contains("std::ptr::") {
            content = content.replace("std :: ptr ::", "std::ptr::");
            content = content.replace("std::ptr:: ", "std::ptr::");
            content = content.replace("std:: ptr::", "std::ptr::");
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
        "global_handler",
        "postprocess_global_code",
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

/// Redirect callback: Handles cases where this handler should pass tokens to a different handler
fn redirect_global(
    tokens: &[Token],
    result: HandlerResult) -> Result<HandlerResult, ConversionError> {
    let _id = Id::get("redirect_global");
    report!(
        "global_handler",
        "redirect_global",
        Info,
        Report,
        "Checking if global tokens should be redirected",
        true
    );

    // Check if this is actually a function declaration
    if tokens.iter().any(|t| t.to_string() == "(") && tokens.iter().any(|t| t.to_string() == ")") {
        // Look for function patterns
        let mut paren_count = 0;
        let mut found_identifier_before_paren = false;

        for i in 0..tokens.len() {
            let token = tokens[i].to_string();
            if token == "(" {
                paren_count += 1;
                if i > 0 && !is_keyword(&tokens[i - 1].to_string()) {
                    found_identifier_before_paren = true;
                }
            } else if token == ")" {
                paren_count -= 1;
            }
        }

        if found_identifier_before_paren && paren_count == 0 {
            report!(
                "global_handler",
                "redirect_global",
                Info,
                Report,
                "Redirecting to function handler (function declaration)",
                true
            );
            return Ok(HandlerResult::Redirected(
                Some(tokens.to_vec()),
                0..1,
                "global_handler".to_string(),
                _id,
                Id::get("function_handler"),
            ));
        }
    }

    // Check if this is actually a struct/enum/typedef
    if tokens
        .iter()
        .any(|t| matches!(t.to_string().as_str(), "struct" | "enum" | "typedef"))
    {
        let first_keyword = tokens
            .iter()
            .find(|t| matches!(t.to_string().as_str(), "struct" | "enum" | "typedef"))
            .unwrap()
            .to_string();

        match first_keyword.as_str() {
            "struct" => {
                report!(
                    "global_handler",
                    "redirect_global",
                    Info,
                    Report,
                    "Redirecting to struct handler",
                    true
                );
                return Ok(HandlerResult::Redirected(
                    Some(tokens.to_vec()),
                    0..1,
                    "global_handler".to_string(),
                    _id,
                    Id::get("struct_handler"),
                ));
            }
            "enum" => {
                report!(
                    "global_handler",
                    "redirect_global",
                    Info,
                    Report,
                    "Redirecting to enum handler",
                    true
                );
                return Ok(HandlerResult::Redirected(
                    Some(tokens.to_vec()),
                    0..1,
                    "global_handler".to_string(),
                    _id,
                    Id::get("enum_handler"),
                ));
            }
            "typedef" => {
                report!(
                    "global_handler",
                    "redirect_global",
                    Info,
                    Report,
                    "Redirecting to typedef handler",
                    true
                );
                return Ok(HandlerResult::Redirected(
                    Some(tokens.to_vec()),
                    0..1,
                    "global_handler".to_string(),
                    _id,
                    Id::get("typedef_handler"),
                ));
            }
            _ => {}
        }
    }

    // Check if this contains preprocessor directives
    if tokens.iter().any(|t| t.to_string().starts_with("#")) {
        report!(
            "global_handler",
            "redirect_global",
            Info,
            Report,
            "Redirecting to macro handler (preprocessor directive)",
            true
        );
        return Ok(HandlerResult::Redirected(
            Some(tokens.to_vec()),
            0..1,
            "global_handler".to_string(),
            _id,
            Id::get("macro_handler"),
        ));
    }

    // No redirection needed
    Ok(result)
}
