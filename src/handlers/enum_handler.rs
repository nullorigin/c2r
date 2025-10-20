//! Enum handler implementation using trait-based system
//! Uses Patternizer and Samplizer for intelligent enum pattern detection and processing

use crate::{
    gen_name, info::EnumInfo, Context, ConvertedElement, ConvertedEnum, ExtractedElement,
    ExtractedEnum, Handler, HandlerResult, Id, Phase, RedirectRequest, Report, ReportLevel,
    Result, Token,
};
use std::ops::Range;

/// Enum handler struct implementing the Handler trait
#[derive(Debug, Clone)]
pub struct EnumHandler;

impl EnumHandler {
    pub fn new() -> Self {
        Self
    }

    /// Enhanced pattern detection using both Patternizer and Samplizer
    fn detect_pattern(
        &self,
        patternizer: &mut crate::Patternizer,
        samplizer: &crate::Samplizer,
        tokens: &[Token],
        token_range: Range<usize>,
    ) -> Result<(bool, f64)> {
        if tokens.is_empty() {
            return Ok((false, 0.0));
        }

        // Try multiple registered patterns for enum declarations
        let mut pattern_success = false;
        let pattern_names = vec!["enum_declaration", "typedef_enum", "enum"];

        for pattern_name in &pattern_names {
            let pattern_match = patternizer.match_pattern(pattern_name, tokens);
            if !matches!(pattern_match, crate::PatternResult::NoMatch { .. }) {
                pattern_success = true;
                break;
            }
        }

        // Use Samplizer for confidence scoring with registered patterns
        let patterns = vec!["enum_declaration".to_string(), "typedef_enum".to_string()];
        let base_confidence = samplizer.analyze_with_range(&patterns, tokens, token_range)?;

        // Boost confidence with heuristics
        let has_enum_keyword = tokens.iter().any(|t| t.to_string() == "enum");
        let has_braces = tokens.iter().any(|t| t.to_string() == "{");
        let confidence_boost = if has_enum_keyword && has_braces { 0.2 } else if has_enum_keyword { 0.1 } else { 0.0 };

        let final_confidence = (base_confidence + confidence_boost).min(1.0);
        let detected = pattern_success || (has_enum_keyword && final_confidence > 0.6);

        Ok((detected, final_confidence))
    }


    /// Extract enum information from tokens using pattern-based parsing
    fn extract_info(&self, tokens: &[Token], patternizer: &crate::Patternizer) -> Result<EnumInfo> {
        let mut enum_info = EnumInfo::default();
        let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        // Check for typedef
        enum_info.is_typedef = token_strings.contains(&"typedef".to_string());

        // Find enum keyword and extract name
        if let Some(enum_pos) = token_strings.iter().position(|t| t == "enum") {
            if enum_pos + 1 < token_strings.len() {
                let next = &token_strings[enum_pos + 1];
                if next != "{" {
                    enum_info.name = next.clone();
                }
            }
        }

        // Check for body
        enum_info.has_body = token_strings.contains(&"{".to_string());

        // Extract variants if body exists
        if enum_info.has_body {
            enum_info.variants = self.extract_variants(&token_strings);
        }

        Ok(enum_info)
    }

    /// Extract enum variants from token strings
    fn extract_variants(&self, token_strings: &[String]) -> Vec<(String, Option<String>)> {
        let mut variants = Vec::new();
        let mut in_body = false;
        let mut current_variant = String::new();

        for token in token_strings {
            match token.as_str() {
                "{" => in_body = true,
                "}" => break,
                "," => {
                    if in_body && !current_variant.trim().is_empty() {
                        variants.push((current_variant.trim().to_string(), None));
                        current_variant.clear();
                    }
                }
                _ => {
                    if in_body {
                        if !current_variant.is_empty() {
                            current_variant.push(' ');
                        }
                        current_variant.push_str(token);
                    }
                }
            }
        }

        // Add final variant
        if !current_variant.trim().is_empty() {
            variants.push((current_variant.trim().to_string(), None));
        }

        variants
    }

    /// Convert enum to Rust code   
    fn convert_tokens(&self, enum_info: &EnumInfo, tokens: &[Token]) -> Result<String> {
        let mut rust_code = String::new();

        // Add derives and attributes
        rust_code.push_str("#[derive(Debug, Clone, Copy, PartialEq, Eq)]\n");

        // Add enum visibility
        rust_code.push_str("pub enum ");

        // Add enum name
        let rust_name = self.convert_identifier(&enum_info.name);
        rust_code.push_str(&rust_name);

        if enum_info.has_body && !enum_info.variants.is_empty() {
            rust_code.push_str(" {\n");

            // Convert enum values
            for (i, value) in enum_info.variants.iter().enumerate() {
                rust_code.push_str("    ");
                let rust_value = self.convert_value(value.1.clone().unwrap().as_str());
                rust_code.push_str(&rust_value);

                if i < enum_info.variants.len() - 1 {
                    rust_code.push(',');
                }
                rust_code.push('\n');
            }

            rust_code.push('}');
        } else {
            rust_code.push(';');
        }

        Ok(rust_code)
    }

    /// Convert C enum value to Rust
    fn convert_value(&self, value: &str) -> String {
        // Parse value which might be "NAME" or "NAME = expression"
        let parts: Vec<&str> = value.split('=').collect();
        let name = parts[0].trim();
        let rust_variant = self.convert_identifier(name);

        if parts.len() > 1 {
            let expr = parts[1].trim();
            format!("{} = {}", rust_variant, expr)
        } else {
            rust_variant
        }
    }

    /// Convert C identifier to Rust naming convention
    fn convert_identifier(&self, identifier: &str) -> String {
        let mut result = String::new();
        let mut capitalize_next = true;

        for ch in identifier.chars() {
            if ch == '_' {
                capitalize_next = true;
            } else if capitalize_next {
                result.push(ch.to_uppercase().next().unwrap_or(ch));
                capitalize_next = false;
            } else {
                result.push(ch.to_lowercase().next().unwrap_or(ch));
            }
        }

        result
    }
}

impl Handler for EnumHandler {
    fn id(&self) -> Id {
        Id::get("enum_handler")
    }

    fn role(&self) -> String {
        "enum".to_string()
    }

    fn priority(&self) -> u64 {
        160
    }

    fn supported_patterns(&self) -> Vec<String> {
        vec![
            "enum_declaration".to_string(),
            "enum_definition".to_string(),
            "enum_value".to_string(),
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

        let (can_process, confidence) = self.detect_pattern(&mut context.patternizer, &context.samplizer, &tokens, token_range)?;

        context.registry.add_report(
            Report::new(
                Id::get(&format!("enum_handler_can_process_{}", token_slot)),
                Some(self.id()),
                "can_process".to_string(),
                format!(
                    "Enum detection: {} (confidence: {:.2})",
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

        let enum_info = self.extract_info(&tokens, &context.patternizer)?;

        let extracted_enum = ExtractedEnum {
            id: Id::get(gen_name(&self.id().name()).as_str()),
            info: enum_info.clone(),
            tokens: tokens.clone(),
            token_range,
            metadata: vec![
                ("is_typedef".to_string(), enum_info.is_typedef.to_string()),
                ("has_body".to_string(), enum_info.has_body.to_string()),
                (
                    "value_count".to_string(),
                    enum_info.variants.len().to_string(),
                ),
                ("original_name".to_string(), enum_info.name.clone()),
            ],
        };

        Ok(Some(ExtractedElement::Enum(extracted_enum)))
    }

    fn convert(
        &self,
        context: &mut Context,
        element: ExtractedElement,
    ) -> Result<Option<ConvertedElement>> {
        if let ExtractedElement::Enum(enum_element) = element {
            if enum_element.tokens.is_empty() {
                return Ok(None);
            }

            let enum_info = enum_element.info;
            let rust_code = self.convert_tokens(&enum_info, &enum_element.tokens)?;

            let converted_enum = ConvertedEnum {
                code: rust_code,
                metadata: Vec::new(),
            };

            Ok(Some(ConvertedElement::Enum(converted_enum)))
        } else {
            Ok(None)
        }
    }
    fn document(
        &self,
        context: &mut Context,
        extracted_element: ExtractedElement,
        converted_element: ConvertedElement,
    ) -> Result<Option<String>> {
        let extracted_enum = if let ExtractedElement::Enum(extracted) = extracted_element {
            extracted
        } else {
            return Ok(None);
        };

        let converted_enum = if let ConvertedElement::Enum(converted) = converted_element {
            converted
        } else {
            return Ok(None);
        };

        let mut doc = format!(
            "/// Enum: {}\n/// Converted from C enum definition\n",
            extracted_enum.info.name
        );

        if !extracted_enum.info.variants.is_empty() {
            doc.push_str("/// Variants:\n");
            for (variant_name, variant_value) in &extracted_enum.info.variants {
                if let Some(value) = variant_value {
                    doc.push_str(&format!("/// - {}: {}\n", variant_name, value));
                } else {
                    doc.push_str(&format!("/// - {}\n", variant_name));
                }
            }
        }

        if extracted_enum.info.is_typedef {
            doc.push_str("/// Note: This enum was defined using typedef\n");
        }

        Ok(Some(doc))
    }

    fn report(&self, context: &mut Context) -> Result<Report> {
        let reports = context.registry.get_reports_by_handler(&self.id().name());

        if reports.is_empty() {
            return Ok(Report::new(
                Id::get("enum_handler_report"),
                Some(self.id()),
                "report".to_string(),
                "No enum processing reports found".to_string(),
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

        Ok(Report::new(
            Id::get("enum_handler_report"),
            Some(self.id()),
            "report".to_string(),
            format!(
                "Enum handler processed {} enums: {} info, {} warnings, {} errors",
                reports.len(),
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
        vec!["enum".to_string()]
    }

    fn route(
        &self,
        context: &mut Context,
        token_slot: usize,
        token_range: Range<usize>,
        result: HandlerResult,
    ) -> Result<HandlerResult> {
        let redirect_request = RedirectRequest {
            from_handler: self.id(),
            token_range: token_range.clone(),
            failed_patterns: vec!["enum".to_string()],
            suggested_handler: None,
            metadata: Vec::new(),
        };
        // Return the result as-is to avoid deadlock in redirect workflow
        Ok(result)
    }
}
impl Default for EnumHandler {
    fn default() -> Self {
        Self::new()
    }
}
