//! Struct handler implementation using trait-based system
//! Uses Patternizer and Samplizer for intelligent struct pattern detection and processing

use crate::{
    gen_name, handler::Handler, info::StructInfo, Context, ConvertedElement, ConvertedStruct,
    ExtractedElement, ExtractedStruct, HandlerResult, Id, Phase, Report, ReportLevel, Result,
    Token,
};
use std::ops::Range;

/// Struct handler implementing the Handler trait
#[derive(Debug, Clone)]
pub struct StructHandler;

impl StructHandler {
    pub fn new() -> Self {
        Self
    }

    /// Convert C type string to Rust type using TYPE_CONVERSION_MAP
    /// Handles pointers, const qualifiers, arrays, and complex types
    fn convert_type(&self, c_type: &str) -> String {
        use crate::pattern::TYPE_CONVERSION_MAP;

        let trimmed = c_type.trim();

        // Handle arrays first - TYPE_CONVERSION_MAP doesn't handle these
        if let Some(bracket_pos) = trimmed.find('[') {
            let base_type = trimmed[..bracket_pos].trim();
            let array_part = &trimmed[bracket_pos..];

            // Convert the base type
            let rust_base = TYPE_CONVERSION_MAP.convert_type(base_type)
                .unwrap_or_else(|| base_type.to_string());

            // Try to extract array size
            if let Some(size) = self.extract_array_size(array_part) {
                return format!("[{}; {}]", rust_base, size);
            } else {
                return format!("Vec<{}>", rust_base);
            }
        }

        // Clean up the type string - normalize whitespace around *
        let normalized = trimmed
            .replace(" *", "*")
            .replace("* ", "*")
            .to_string();

        // Try direct lookup first
        if let Some(rust_type) = TYPE_CONVERSION_MAP.convert_type(&normalized) {
            return rust_type;
        }

        // Try with spaces (e.g., "const char *" vs "const char*")
        if let Some(rust_type) = TYPE_CONVERSION_MAP.convert_type(trimmed) {
            return rust_type;
        }

        // Fallback: unknown type, keep as-is
        eprintln!("⚠️  Unknown C type '{}' in struct, keeping as-is", c_type);
        c_type.to_string()
    }

    /// Extract array size from bracket notation
    fn extract_array_size(&self, array_part: &str) -> Option<String> {
        if let Some(start) = array_part.find('[') {
            if let Some(end) = array_part.find(']') {
                let size_str = array_part[start + 1..end].trim();
                if !size_str.is_empty() {
                    return Some(size_str.to_string());
                }
            }
        }
        None
    }

    /// Enhanced pattern detection using both Patternizer and Samplizer
    fn detect_pattern(
        &self,
        patternizer: &mut crate::Patternizer,
        samplizer: &crate::Samplizer,
        tokens: &[Token],
        token_range: Range<usize>,
    ) -> Result<(bool, f64)> {
        // Early exit for empty tokens
        if tokens.is_empty() {
            return Ok((false, 0.0));
        }

        // Try multiple registered patterns for struct declarations
        let mut pattern_success = false;
        let pattern_names = vec!["struct_definition", "struct_declaration", "typedef_struct", "struct"];

        for pattern_name in &pattern_names {
            let pattern_match = patternizer.match_pattern(pattern_name, tokens);
            if !matches!(pattern_match, crate::PatternResult::NoMatch { .. }) {
                pattern_success = true;
                break;
            }
        }

        // Use Samplizer for confidence scoring with registered patterns
        let patterns = vec![
            "struct_declaration".to_string(),
            "struct_definition".to_string(),
            "typedef_struct".to_string(),
        ];
        let base_confidence = samplizer.analyze_with_range(&patterns, tokens, token_range.clone())
            .unwrap_or(0.0);

        // Enhanced detection logic with multiple criteria
        let has_struct_keyword = tokens.iter().any(|t| t.to_string() == "struct");
        let has_braces = tokens.iter().any(|t| t.to_string() == "{") &&
            tokens.iter().any(|t| t.to_string() == "}");
        let has_typedef = tokens.iter().any(|t| t.to_string() == "typedef");

        let structural_match = pattern_success || has_struct_keyword;
        let confidence_threshold = if structural_match { 0.6 } else { 0.75 };

        // Boost confidence for well-formed structs
        let adjusted_confidence = if has_braces || has_typedef {
            base_confidence * 1.1
        } else {
            base_confidence
        };

        Ok((
            structural_match && adjusted_confidence > confidence_threshold,
            adjusted_confidence.min(1.0),
        ))
    }

    /// Extract struct information from tokens with enhanced parsing
    fn extract_info(&self, tokens: &[Token]) -> Result<StructInfo> {
        let mut struct_info = StructInfo::default();
        let mut in_body = false;
        let mut field_tokens = Vec::new();
        let mut brace_count = 0;
        let mut paren_count = 0;

        for (i, token) in tokens.iter().enumerate() {
            let token_str = token.to_string();

            match token_str.as_str() {
                "struct" => {
                    // Look for struct name, handling both "struct Name" and "typedef struct Name"
                    for j in (i + 1)..tokens.len() {
                        let next_token = tokens[j].to_string();
                        if next_token == "{" {
                            break;
                        }
                        if !next_token.is_empty() &&
                            next_token.chars().next().map_or(false, |c| c.is_alphabetic() || c == '_') {
                            struct_info.name = next_token;
                            break;
                        }
                    }
                }
                "typedef" => {
                    struct_info.is_typedef = true;
                    // For typedef struct, name might be at the end
                    if struct_info.name.is_empty() {
                        if let Some(last_token) = tokens.last() {
                            let last_str = last_token.to_string();
                            if last_str != ";" && !last_str.is_empty() {
                                struct_info.name = last_str;
                            }
                        }
                    }
                }
                "{" => {
                    brace_count += 1;
                    if brace_count == 1 {
                        in_body = true;
                        struct_info.has_body = true;
                    }
                }
                "}" => {
                    brace_count -= 1;
                    if brace_count == 0 && in_body {
                        in_body = false;
                        // Process any remaining field tokens
                        if !field_tokens.is_empty() {
                            self.process_field_tokens(&mut struct_info, &field_tokens);
                            field_tokens.clear();
                        }
                    }
                }
                "(" => paren_count += 1,
                ")" => paren_count -= 1,
                ";" => {
                    if in_body && paren_count == 0 && !field_tokens.is_empty() {
                        self.process_field_tokens(&mut struct_info, &field_tokens);
                        field_tokens.clear();
                    }
                }
                _ => {
                    if in_body && brace_count > 0 {
                        field_tokens.push(token_str);
                    }
                }
            }
        }

        Ok(struct_info)
    }

    /// Process field tokens into structured field information
    fn process_field_tokens(&self, struct_info: &mut StructInfo, field_tokens: &[String]) {
        if field_tokens.is_empty() {
            return;
        }

        let field_decl = field_tokens.join(" ");

        // Try to parse field name and type
        if let Some(last_token) = field_tokens.last() {
            if last_token.chars().all(|c| c.is_alphanumeric() || c == '_') {
                let field_type = if field_tokens.len() > 1 {
                    field_tokens[..field_tokens.len() - 1].join(" ")
                } else {
                    "unknown".to_string()
                };
                struct_info.fields.push((field_decl, field_type));
                return;
            }
        }

        // Fallback: store the entire declaration
        struct_info.fields.push((field_decl, "".to_string()));
    }

    /// Convert struct to Rust code with enhanced type conversion
    fn convert_to_rust(&self, struct_info: &StructInfo, tokens: &[Token], patternizer: &crate::Patternizer) -> Result<String> {
        let mut rust_code = String::new();

        // Add comprehensive derives
        rust_code.push_str("#[derive(Debug, Clone, PartialEq)]\n");

        // Add repr(C) for better C compatibility if it's a typedef
        if struct_info.is_typedef {
            rust_code.push_str("#[repr(C)]\n");
        }

        // Add struct visibility and name
        rust_code.push_str("pub struct ");
        let rust_name = self.convert_identifier(&struct_info.name);
        rust_code.push_str(&rust_name);

        if struct_info.has_body && !struct_info.fields.is_empty() {
            rust_code.push_str(" {\n");

            // Convert fields with better type handling
            for (field_decl, field_type) in &struct_info.fields {
                rust_code.push_str("    pub ");
                let rust_field = self.convert_field(field_decl, field_type, patternizer);
                rust_code.push_str(&rust_field);
                rust_code.push_str(",\n");
            }

            rust_code.push('}');
        } else {
            // Empty struct or forward declaration
            rust_code.push_str(" {\n    _phantom: std::marker::PhantomData<()>,\n}");
        }

        Ok(rust_code)
    }

    /// Enhanced field conversion with better type inference
    fn convert_field(&self, field_decl: &str, field_type: &str, patternizer: &crate::Patternizer) -> String {
        let parts: Vec<&str> = field_decl.trim().split_whitespace().collect();

        if parts.len() >= 2 {
            let field_name = self.convert_identifier(parts.last().unwrap());

            // Use field_type if available, otherwise infer from declaration
            let c_type = if !field_type.is_empty() {
                field_type
            } else {
                &parts[..parts.len() - 1].join(" ")
            };

            // Enhanced type conversion with array and pointer handling
            let rust_type = self.convert_type(&c_type);

            format!("{}: {}", field_name, rust_type)
        } else {
            // Single token - might be a type name or incomplete declaration
            let identifier = self.convert_identifier(parts.get(0).unwrap_or(&"unknown"));
            format!("{}: /* incomplete: {} */", identifier, field_decl)
        }
    }

    /// Convert C identifier to Rust snake_case convention
    fn convert_identifier(&self, identifier: &str) -> String {
        if identifier.is_empty() {
            return "unknown".to_string();
        }

        let mut result = String::new();
        let mut prev_was_upper = false;

        for (i, ch) in identifier.chars().enumerate() {
            if ch.is_uppercase() {
                if i > 0 && !prev_was_upper {
                    result.push('_');
                }
                result.push(ch.to_lowercase().next().unwrap_or(ch));
                prev_was_upper = true;
            } else {
                result.push(ch);
                prev_was_upper = false;
            }
        }

        // Ensure valid Rust identifier
        if result.chars().next().map_or(false, |c| c.is_numeric()) {
            result = format!("_{}", result);
        }

        result
    }
}

impl Handler for StructHandler {
    fn id(&self) -> Id {
        Id::get("struct_handler")
    }

    fn role(&self) -> String {
        "struct".to_string()
    }

    fn priority(&self) -> u64 {
        140
    }

    fn supported_patterns(&self) -> Vec<String> {
        vec![
            "struct_declaration".to_string(),
            "struct_definition".to_string(),
            "struct_initialization".to_string(),
        ]
    }

    fn detect(
        &self,
        context: &mut Context,
        token_slot: usize,
        token_range: Range<usize>,
    ) -> Result<bool> {
        let tokens = context
            .tokenizer
            .get_tokens(token_slot, token_range.clone());

        if tokens.is_empty() {
            return Ok(false);
        }

        let (can_process, confidence) =
            self.detect_pattern(&mut context.patternizer, &context.samplizer, &tokens, token_range.clone())?;

        context.registry.add_report(
            Report::new(
                Id::get(&format!("struct_handler_can_process_{}", token_slot)),
                Some(self.id()),
                "can_process".to_string(),
                format!(
                    "Struct detection: {} (confidence: {:.2})",
                    can_process, confidence
                ),
                ReportLevel::Info,
                Phase::Process(None),
            )
                .with_tokens(tokens.len(), if can_process { tokens.len() } else { 0 })
                .with_success(can_process),
        );

        Ok(can_process)
    }

    fn extract(
        &self,
        context: &mut Context,
        token_slot: usize,
        token_range: Range<usize>,
    ) -> Result<Option<ExtractedElement>> {
        let tokens = context
            .tokenizer
            .get_tokens(token_slot, token_range.clone());

        if tokens.is_empty() {
            return Ok(None);
        }

        let struct_info = self.extract_info(&tokens)?;

        let extracted_struct = ExtractedStruct {
            id: Id::get(&gen_name(&self.id().name()).as_str()),
            info: struct_info.clone(),
            tokens: tokens.clone(),
            token_range,
            metadata: vec![
                ("is_typedef".to_string(), struct_info.is_typedef.to_string()),
                ("has_body".to_string(), struct_info.has_body.to_string()),
                (
                    "field_count".to_string(),
                    struct_info.fields.len().to_string(),
                ),
                ("original_name".to_string(), struct_info.name.clone()),
            ],
        };

        Ok(Some(ExtractedElement::Struct(extracted_struct)))
    }

    fn convert(
        &self,
        context: &mut Context,
        element: ExtractedElement,
    ) -> Result<Option<ConvertedElement>> {
        if let ExtractedElement::Struct(extracted) = element {
            let tokens = extracted.tokens.clone();
            if tokens.is_empty() {
                return Ok(None);
            }

            let struct_info = extracted.info.clone();
            let rust_code = self.convert_to_rust(&struct_info, &tokens, &context.patternizer)?;

            let converted_struct = ConvertedStruct {
                code: rust_code,
                metadata: extracted.metadata.clone(),
            };

            Ok(Some(ConvertedElement::Struct(converted_struct)))
        } else {
            Ok(None)
        }
    }

    fn document(
        &self,
        _context: &mut Context,
        extracted_element: ExtractedElement,
        _converted_element: ConvertedElement,
    ) -> Result<Option<String>> {
        let extracted_struct = if let ExtractedElement::Struct(extracted) = extracted_element {
            extracted
        } else {
            return Ok(None);
        };

        let mut doc = format!(
            "/// Struct: {}\n/// Converted from C struct definition\n",
            extracted_struct.info.name
        );

        if !extracted_struct.info.fields.is_empty() {
            doc.push_str("/// Fields:\n");
            for (field_decl, field_type) in &extracted_struct.info.fields {
                if !field_type.is_empty() {
                    doc.push_str(&format!("/// - {}: {}\n", field_decl, field_type));
                } else {
                    doc.push_str(&format!("/// - {}\n", field_decl));
                }
            }
        }

        if extracted_struct.info.is_typedef {
            doc.push_str("/// Note: This struct was defined using typedef\n");
        }

        if !extracted_struct.info.has_body {
            doc.push_str("/// Note: Forward declaration or opaque struct\n");
        }

        Ok(Some(doc))
    }

    fn report(&self, context: &mut Context) -> Result<Report> {
        let reports = context.registry.get_reports_by_handler(&self.id().name());

        if reports.is_empty() {
            return Ok(Report::new(
                Id::get("struct_handler_report"),
                Some(self.id()),
                "report".to_string(),
                "No struct processing reports found".to_string(),
                ReportLevel::Info,
                Phase::Report(None),
            )
                .with_success(true));
        }

        let (info_count, warning_count, error_count) =
            reports
                .iter()
                .fold((0, 0, 0), |(info, warn, err), report| match report.level {
                    ReportLevel::Info => (info + 1, warn, err),
                    ReportLevel::Warning => (info, warn + 1, err),
                    ReportLevel::Error => (info, warn, err + 1),
                    ReportLevel::Debug => (info + 1, warn, err),
                });

        let total_tokens_processed = reports.iter().map(|r| r.tokens_processed).sum::<usize>();
        let successful_reports = reports.iter().filter(|r| r.success).count();

        Ok(Report::new(
            Id::get("struct_handler_report"),
            Some(self.id()),
            "report".to_string(),
            format!(
                "Struct handler processed {} structs ({} successful): {} info, {} warnings, {} errors",
                reports.len(),
                successful_reports,
                info_count,
                warning_count,
                error_count
            ),
            if error_count > 0 {
                ReportLevel::Error
            } else if warning_count > 0 {
                ReportLevel::Warning
            } else {
                ReportLevel::Info
            },
            Phase::Report(None),
        )
            .with_success(error_count == 0)
            .with_tokens(total_tokens_processed, total_tokens_processed))
    }

    fn supported_keywords(&self) -> Vec<String> {
        vec!["struct".to_string()]
    }

    fn route(
        &self,
        _context: &mut Context,
        _token_slot: usize,
        _token_range: Range<usize>,
        result: HandlerResult,
    ) -> Result<HandlerResult> {
        Ok(result)
    }
}

impl Default for StructHandler {
    fn default() -> Self {
        Self::new()
    }
}
