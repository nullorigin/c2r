#![allow(
    unused_variables,
    unused_assignments,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals
)]

use super::common::{find_matching_token, not_handled, replace_with_range};
use crate::config::{
    Context,
    HandlerPhase::{Convert, Process, Report},
    HandlerReport,
    ReportLevel::{Info, Warning},
};
use crate::error::ConversionError;
use crate::handler::HandlerResult;
use crate::lock::Id;
use crate::{context, ReportLevel};
use crate::{report, ConvertedElement, ConvertedEnum, ExtractedElement, ExtractedEnum, Token};
use ReportLevel::Error;

/// Creates an enum handler that can detect and convert C enums
pub fn create_enum_handler() -> crate::handler::Handler {
    let handler_id = Id::get("enum_handler");
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
        Some(report_enum),
        Some(result_enum),
        Some(redirect_enum),
    )
}

/// Process callback: Initializes and confirms this handler can handle the tokens
fn process_enum(tokens: &[Token]) -> Result<bool, ConversionError> {
    // Validate input
    if tokens.is_empty() {
        return Ok(false);
    }

    // Check for enum keyword patterns
    if tokens[0].to_string() == "enum" {
        // enum name { ... }
        if tokens.len() >= 3 && tokens[2].to_string() == "{" {
            report!(
                "enum_handler",
                "process_enum",
                Info,
                Process,
                format!("Enum definition detected: {}", tokens[1].to_string()),
                true
            );
            return Ok(true);
        }

        // enum name;
        if tokens.len() >= 3 && tokens[2].to_string() == ";" {
            report!(
                "enum_handler",
                "process_enum",
                Info,
                Process,
                format!(
                    "Enum forward declaration detected: {}",
                    tokens[1].to_string()
                ),
                true
            );
            return Ok(true);
        }

        // Anonymous enum: enum { ... }
        if tokens.len() >= 2 && tokens[1].to_string() == "{" {
            report!(
                "enum_handler",
                "process_enum",
                Info,
                Process,
                "Anonymous enum detected",
                true
            );
            return Ok(true);
        }
    }

    // Check for typedef enum patterns
    if tokens.len() >= 3 && tokens[0].to_string() == "typedef" && tokens[1].to_string() == "enum" {
        report!(
            "enum_handler",
            "process_enum",
            Info,
            Process,
            "Typedef enum pattern detected",
            true
        );
        return Ok(true);
    }

    Ok(false)
}

/// Processes an enum declaration
fn handle_enum(tokens: &[Token]) -> Result<HandlerResult, ConversionError> {
    let id = Id::get("enum");
    if let Some(enum_name_token) = tokens.get(1) {
        let enum_name = enum_name_token.to_string();

        // Find the start and end of the enum body
        if let Some(body_start) = tokens.iter().position(|t| t.to_string() == "{") {
            if let Some(body_end) = find_matching_token(tokens, "{", "}") {
                let variants = &tokens[body_start + 1..body_end];
                let rust_variants = variants
                    .iter()
                    .filter(|t| t.to_string() != ",")
                    .map(|t| t.to_string())
                    .collect::<Vec<String>>()
                    .join(",\n    ");

                let rust_code = format!(
                    "#[derive(Debug, Clone, Copy, PartialEq, Eq)]\npub enum {} {{\n    {}\n}}",
                    enum_name, rust_variants
                );

                // Enum definitions process all input tokens
                let token_range = 0..tokens.len();
                return replace_with_range(rust_code, token_range, id);
            }
        }
    }

    not_handled()
}

/// Extracts an enum as an ExtractedElement
pub fn extract_enum(
    tokens: &[Token]) -> Result<Option<ExtractedElement>, ConversionError> {
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
    let brace_idx = if is_anonymous { name_idx } else { name_idx + 1 };

    if brace_idx >= tokens.len() || tokens[brace_idx].to_string() != "{" {
        // This might be a forward declaration
        return if !is_anonymous
            && name_idx + 1 < tokens.len()
            && tokens[name_idx + 1].to_string() == ";"
        {
            // Forward declaration
            let extracted_enum = ExtractedEnum {
                name: enum_name,
                values: vec![],
                is_typedef,
                typedef_name: None,
                is_forward_declaration: true,
                original_code: tokens
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(" "),
                tokens: tokens.to_vec(),
            };

            Ok(Some(ExtractedElement::Enum(extracted_enum)))
        } else {
            Err(ConversionError::new(
                "Could not find opening brace for enum",
            ))
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

    // Look for typedef name (if this is a typedef enum)
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
        is_typedef,
        typedef_name,
        is_forward_declaration: false,
        original_code: tokens
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<_>>()
            .join(" "),
        tokens: tokens.to_vec(),
    };

    Ok(Some(ExtractedElement::Enum(extracted_enum)))
}

/// Convert callback: Does the actual conversion of C to Rust code
fn convert_enum(
    tokens: &[Token]) -> Result<Option<ConvertedElement>, ConversionError> {
    report!(
        "enum_handler",
        "convert_enum",
        Info,
        Convert,
        format!("Converting enum from {} tokens", tokens.len()),
        true
    );
    if let Some(ExtractedElement::Enum(extracted_enum)) = extract_enum(tokens)? {
        let typedef_name = extracted_enum
            .typedef_name
            .clone()
            .unwrap_or_else(|| extracted_enum.name.clone());
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
        if extracted_enum.is_typedef
            && extracted_enum.typedef_name.is_some()
            && !typedef_name.is_empty()
            && typedef_name != enum_name
        {
            rust_code.push_str(&format!("pub type {} = {};\n", enum_name, typedef_name));
        }

        let id = Id::get("convert_enum");
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
fn redirect_enum(
    tokens: &[Token],
    result: HandlerResult) -> Result<HandlerResult, ConversionError> {
    let id = Id::get("redirect_enum");
    report!(
        "enum_handler",
        "redirect_enum",
        Info,
        Report,
        "Checking if enum tokens should be redirected",
        true
    );

    // Check if this is actually a struct disguised as an enum
    if tokens.iter().any(|t| t.to_string() == "struct") {
        report!(
            "enum_handler",
            "redirect_enum",
            Info,
            Report,
            "Redirecting to struct handler",
            true
        );
        return Ok(HandlerResult::Redirected(
            Some(tokens.to_vec()),
            0..1,
            "enum_handler".to_string(),
            id,
            Id::get("struct_handler"),
        ));
    }

    // Check if this is actually a union
    if tokens.iter().any(|t| t.to_string() == "union") {
        report!(
            "enum_handler",
            "redirect_enum",
            Info,
            Report,
            "Redirecting to union handler",
            true
        );
        return Ok(HandlerResult::Redirected(
            Some(tokens.to_vec()),
            0..1,
            "enum_handler".to_string(),
            id,
            Id::get("union_handler"),
        ));
    }

    // Check if this is a typedef that doesn't involve enum definition
    if tokens.len() >= 2 && tokens[0].to_string() == "typedef" && tokens[1].to_string() != "enum" {
        report!(
            "enum_handler",
            "redirect_enum",
            Info,
            Report,
            "Redirecting to typedef handler",
            true
        );
        return Ok(HandlerResult::Redirected(
            Some(tokens.to_vec()),
            0..1,
            "enum_handler".to_string(),
            id,
            Id::get("typedef_handler"),
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
        report!(
            "enum_handler",
            "redirect_enum",
            Info,
            Report,
            "Redirecting to global handler (enum variable)",
            true
        );
        return Ok(HandlerResult::Redirected(
            Some(tokens.to_vec()),
            0..1,
            "enum_handler".to_string(),
            id,
            Id::get("global_handler"),
        ));
    }

    // No redirection needed
    Ok(result)
}

/// Result callback: Postprocesses generated enum code, adds documentation, and enhances formatting
fn result_enum(
    tokens: &[Token],
    result: HandlerResult) -> Result<HandlerResult, ConversionError> {
    let _id = Id::get("result_enum");

    report!(
        "enum_handler",
        "result_enum",
        Info,
        Report,
        "Postprocessing enum conversion result",
        true
    );

    match result {
        HandlerResult::Completed(tokens_opt, _, rust_code, id) => {
            // Extract enum information for documentation
            let enum_info = extract_enum_info_from_tokens(tokens);

            // Generate documentation about the enum conversion
            let doc_comment = generate_enum_documentation(tokens, &enum_info);

            // Enhance the Rust code with documentation and metadata
            let mut enhanced_code = String::new();

            // Add metadata comment for traceability
            let metadata_comment = format!(
                "// [C2R] Enum converted from C to Rust - {}: {} variant(s)\n",
                enum_info.name, enum_info.variant_count
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
                "enum_handler",
                "result_enum",
                Info,
                Report,
                &format!(
                    "Enhanced enum conversion: {} with {} variants",
                    enum_info.name, enum_info.variant_count
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
            let enum_info = extract_enum_info_from_tokens(tokens);
            let doc_comment = generate_enum_documentation(tokens, &enum_info);

            let mut enhanced_code = String::new();
            let metadata_comment = format!(
                "// [C2R] Enum converted from C to Rust - {}: {} variant(s)\n",
                enum_info.name, enum_info.variant_count
            );
            enhanced_code.push_str(&metadata_comment);

            if !doc_comment.trim().is_empty() {
                enhanced_code.push_str(&doc_comment);
                enhanced_code.push('\n');
            }
            enhanced_code.push_str(&rust_code);

            report!(
                "enum_handler",
                "result_enum",
                Info,
                Report,
                &format!(
                    "Enhanced converted enum: {} with {} variants",
                    enum_info.name, enum_info.variant_count
                ),
                true
            );

            // CRITICAL: Return HandlerResult::Converted to preserve the code!
            Ok(HandlerResult::Converted(element, 0..1, enhanced_code, id))
        }
        HandlerResult::Extracted(element, _, rust_code, id) => {
            // Handle extracted elements - enhance the code and preserve the variant
            let enum_info = extract_enum_info_from_tokens(tokens);
            let doc_comment = generate_enum_documentation(tokens, &enum_info);

            let mut enhanced_code = String::new();
            let metadata_comment = format!(
                "// [C2R] Enum converted from C to Rust - {}: {} variant(s)\n",
                enum_info.name, enum_info.variant_count
            );
            enhanced_code.push_str(&metadata_comment);

            if !doc_comment.trim().is_empty() {
                enhanced_code.push_str(&doc_comment);
                enhanced_code.push('\n');
            }
            enhanced_code.push_str(&rust_code);

            report!(
                "enum_handler",
                "result_enum",
                Info,
                Report,
                &format!(
                    "Enhanced extracted enum: {} with {} variants",
                    enum_info.name, enum_info.variant_count
                ),
                true
            );

            // CRITICAL: Return HandlerResult::Extracted to preserve the code!
            Ok(HandlerResult::Extracted(element, 0..1, enhanced_code, id))
        }
        HandlerResult::Handled(Some(converted_tokens), _, handler_id) => {
            // Legacy support for token-based results
            // Extract enum information for documentation
            let enum_info = extract_enum_info_from_tokens(tokens);

            // Generate documentation about the enum conversion
            let doc_comment = generate_enum_documentation(tokens, &enum_info);

            // Postprocess the converted Rust code for better formatting
            let mut enhanced_result = postprocess_enum_code(converted_tokens);

            // Add the documentation comment before the converted enum
            if !doc_comment.is_empty() {
                enhanced_result.insert(0, Token::s(doc_comment));
            }

            // Add metadata comment for traceability
            let metadata_comment = format!(
                "// [C2R] Enum converted from C to Rust - {}: {} variant(s)",
                enum_info.name, enum_info.variant_count
            );
            enhanced_result.insert(0, Token::s(metadata_comment));

            report!(
                "enum_handler",
                "result_enum",
                Info,
                Report,
                &format!(
                    "Enhanced enum conversion: {} with {} variants",
                    enum_info.name, enum_info.variant_count
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
                "enum_handler",
                "result_enum",
                Warning,
                Report,
                "Enum handler returned empty result",
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

/// Enum information extracted for documentation purposes
#[derive(Debug, Clone)]
struct EnumInfo {
    name: String,
    variant_count: usize,
    is_typedef: bool,
    is_anonymous: bool,
    is_forward_declaration: bool,
    has_explicit_values: bool,
    typedef_name: Option<String>,
}

/// Extracts enum information from the original tokens for documentation purposes
fn extract_enum_info_from_tokens(tokens: &[Token]) -> EnumInfo {
    if tokens.is_empty() {
        return EnumInfo {
            name: "unknown".to_string(),
            variant_count: 0,
            is_typedef: false,
            is_anonymous: true,
            is_forward_declaration: false,
            has_explicit_values: false,
            typedef_name: None,
        };
    }

    let is_typedef = tokens[0].to_string() == "typedef";
    let enum_token_idx = if is_typedef { 1 } else { 0 };
    let name_idx = enum_token_idx + 1;

    // Check if this is an anonymous enum
    let is_anonymous = name_idx >= tokens.len() || tokens[name_idx].to_string() == "{";
    let enum_name = if is_anonymous {
        "AnonymousEnum".to_string()
    } else {
        tokens[name_idx].to_string()
    };

    // Check for forward declaration
    let is_forward_declaration =
        tokens.iter().any(|t| t.to_string() == ";") && !tokens.iter().any(|t| t.to_string() == "{");

    // Count variants and check for explicit values
    let mut variant_count = 0;
    let mut has_explicit_values = false;

    if let Some(brace_start) = tokens.iter().position(|t| t.to_string() == "{") {
        if let Some(brace_end) = find_matching_token(tokens, "{", "}") {
            let body_tokens = &tokens[brace_start + 1..brace_end];

            // Count variants by counting identifiers before commas or closing brace
            let mut i = 0;
            while i < body_tokens.len() {
                if body_tokens[i].to_string() == "," {
                    i += 1;
                    continue;
                }

                // Found a variant name
                if i < body_tokens.len() && !body_tokens[i].to_string().trim().is_empty() {
                    variant_count += 1;
                    i += 1;

                    // Check for explicit value assignment
                    if i < body_tokens.len() && body_tokens[i].to_string() == "=" {
                        has_explicit_values = true;
                        // Skip the value
                        i += 1;
                        while i < body_tokens.len() && body_tokens[i].to_string() != "," {
                            i += 1;
                        }
                    }
                }
            }
        }
    }

    // Extract typedef name if present
    let mut typedef_name = None;
    if is_typedef {
        // Find the typedef name (should be after the closing brace and before semicolon)
        if let Some(brace_end) = find_matching_token(tokens, "{", "}") {
            if brace_end + 1 < tokens.len() {
                for i in brace_end + 1..tokens.len() {
                    if tokens[i].to_string() == ";" {
                        if i > brace_end + 1 {
                            typedef_name = Some(tokens[i - 1].to_string());
                        }
                        break;
                    }
                }
            }
        }
    }

    EnumInfo {
        name: enum_name,
        variant_count,
        is_typedef,
        is_anonymous,
        is_forward_declaration,
        has_explicit_values,
        typedef_name,
    }
}

/// Generates documentation comments for the enum conversion
fn generate_enum_documentation(
    tokens: &[Token],
    enum_info: &EnumInfo) -> String {
    let mut doc_lines = Vec::new();

    // Add main documentation header
    doc_lines.push("/**".to_string());
    doc_lines.push(" * Enum Conversion Documentation".to_string());
    doc_lines.push(" *".to_string());

    // Add enum information
    doc_lines.push(format!(" * Enum name: {}", enum_info.name));
    if let Some(ref typedef_name) = enum_info.typedef_name {
        doc_lines.push(format!(" * Typedef name: {}", typedef_name));
    }
    doc_lines.push(format!(" * Variant count: {}", enum_info.variant_count));
    doc_lines.push(format!(" * Original tokens: {}", tokens.len()));

    // Add enum type information
    let mut enum_type_info = Vec::new();
    if enum_info.is_typedef {
        enum_type_info.push("typedef enum");
    } else {
        enum_type_info.push("enum");
    }
    if enum_info.is_anonymous {
        enum_type_info.push("anonymous");
    }
    if enum_info.is_forward_declaration {
        enum_type_info.push("forward declaration");
    }
    if enum_info.has_explicit_values {
        enum_type_info.push("explicit values");
    }

    doc_lines.push(format!(" * Type: {}", enum_type_info.join(", ")));
    doc_lines.push(" *".to_string());

    // Add conversion notes
    doc_lines.push(" * Conversion features:".to_string());
    doc_lines.push(
        " *   - Added #[derive(Debug, Clone, Copy, PartialEq, Eq)] for common functionality"
            .to_string(),
    );
    doc_lines.push(" *   - Added #[repr(C)] to match C enum memory layout".to_string());
    doc_lines.push(" *   - Made enum public (pub) for accessibility".to_string());

    if enum_info.has_explicit_values {
        doc_lines.push(" *   - Preserved explicit variant values from C".to_string());
    }

    if enum_info.is_typedef {
        doc_lines.push(" *   - Converted typedef enum to Rust enum".to_string());
        if enum_info.typedef_name.is_some() {
            doc_lines.push(" *   - Added type alias for compatibility".to_string());
        }
    }

    if enum_info.is_forward_declaration {
        doc_lines.push(" *   - Forward declaration converted to opaque struct".to_string());
    }

    doc_lines.push(" *".to_string());
    doc_lines.push(" * Rust enums provide:".to_string());
    doc_lines.push(" *   - Type safety with exhaustive pattern matching".to_string());
    doc_lines.push(" *   - Memory safety with no invalid variant values".to_string());
    doc_lines.push(" *   - Rich functionality through derive macros".to_string());
    doc_lines.push(" *   - Zero-cost abstractions with C compatibility".to_string());
    doc_lines.push(" */".to_string());

    report!(
        "enum_handler",
        "generate_enum_documentation",
        Info,
        Report,
        &format!(
            "Generated documentation for enum: {} with {} variants",
            enum_info.name, enum_info.variant_count
        ),
        true
    );

    doc_lines.join("\n")
}

/// Postprocesses the converted enum code for better formatting
fn postprocess_enum_code(mut tokens: Vec<Token>) -> Vec<Token> {
    let original_count = tokens.len();

    // Clean up and format the converted enum tokens
    for token in tokens.iter_mut() {
        let mut content = token.to_string();

        // Clean up extra whitespace
        content = content.trim().to_string();

        // Ensure proper enum formatting
        if content.starts_with("#[derive(") {
            // Ensure proper spacing in derive attributes
            content = content.replace(",", ", ");
            // Clean up double spaces
            while content.contains("  ") {
                content = content.replace("  ", " ");
            }
        }

        // Format enum declaration
        if content.starts_with("pub enum") {
            // Ensure proper spacing
            content = content.replace("pub enum", "pub enum ");
            content = content.replace("  ", " ");
        }

        // Format enum variants with proper indentation
        if content.contains("    ") && (content.contains(" = ") || content.ends_with(',')) {
            // Ensure consistent indentation for variants
            content = content.replace("\t", "    ");
            // Ensure proper spacing around equals
            content = content.replace('=', " = ");
            content = content.replace("  =", " =");
            content = content.replace("=  ", "= ");
        }

        // Format closing braces
        if content == "}" || content == "};" {
            // Ensure proper brace formatting
            content = content.replace(" }", "}");
        }

        // Format type aliases
        if content.starts_with("pub type") {
            // Ensure proper spacing in type aliases
            content = content.replace("pub type", "pub type ");
            content = content.replace(" = ", " = ");
            content = content.replace("  ", " ");
        }

        *token = Token::s(content);
    }

    // Remove any empty tokens
    tokens.retain(|token| !token.to_string().trim().is_empty());

    report!(
        "enum_handler",
        "postprocess_enum_code",
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
fn report_enum(_tokens: &[Token]) -> Result<HandlerReport, ConversionError> {
    let context = context!();
    let handler_reports = context.get_reports_by_handler("enum_handler");

    // Count reports by level
    let info_count = handler_reports
        .iter()
        .filter(|r| matches!(r.level, Info))
        .count();
    let error_count = handler_reports
        .iter()
        .filter(|r| matches!(r.level, Error))
        .count();
    let warning_count = handler_reports
        .iter()
        .filter(|r| matches!(r.level, Warning))
        .count();

    // Create summary message
    let summary_message = format!(
        "Enum Handler Summary: {} total reports (Info: {}, Warnings: {}, Errors: {})",
        handler_reports.len(),
        info_count,
        warning_count,
        error_count
    );

    // Create consolidated report using gen_name function for unique report_id
    let summary_report = HandlerReport {
        report_id: Box::new(Id::get(&Id::gen_name("enum_handler_summary"))),
        handler_id: Box::new(Id::get("enum_handler")),
        handler_name: "enum_handler".to_string(),
        function_name: "report_enum".to_string(),
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
