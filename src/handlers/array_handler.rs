//! Array handler implementation using trait-based system
//! Uses Patternizer and Samplizer for intelligent array pattern detection and processing

use crate::{
    gen_name, handler::Handler, ArrayInfo, C2RError, Context, ConvertedArray, ConvertedElement, ExtractedArray, ExtractedElement, HandlerResult, Id, Phase, RedirectRequest, Report, ReportLevel, Result, Token,
};
use core::{option::Option::Some, result::Result::Ok};
use std::ops::Range;
/// Array handler struct implementing the Handler trait
#[derive(Debug, Clone)]
pub struct ArrayHandler;

impl ArrayHandler {
    pub fn new() -> Self {
        Self
    }

    /// Convert C type string to Rust type using TYPE_CONVERSION_MAP
    fn convert_c_type_to_rust(&self, c_type: &str) -> String {
        use crate::pattern::TYPE_CONVERSION_MAP;

        let normalized = c_type
            .replace(" *", "*")
            .replace("* ", "*")
            .trim()
            .to_string();

        TYPE_CONVERSION_MAP.convert_type(&normalized)
            .or_else(|| TYPE_CONVERSION_MAP.convert_type(c_type.trim()))
            .unwrap_or_else(|| {
                eprintln!("⚠️  Unknown C type '{}' in array, defaulting to i32", c_type);
                "i32".to_string()
            })
    }

    /// Enhanced pattern detection using both Patternizer and Samplizer with improved accuracy
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

        // REJECT function definitions - arrays don't have () followed by {
        let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        if let Some(paren_pos) = token_strings.iter().position(|t| t == "(") {
            // Check if there's a closing ) followed by {
            if let Some(close_paren) = token_strings[paren_pos..].iter().position(|t| t == ")") {
                let close_pos = paren_pos + close_paren;
                if close_pos + 1 < token_strings.len() && token_strings[close_pos + 1] == "{" {
                    // This is a function definition, not an array
                    return Ok((false, 0.0));
                }
            }
        }

        // Multi-layered pattern detection with enhanced accuracy
        // Try multiple registered array patterns
        let mut pattern_success = false;
        let pattern_names = vec!["array_declaration", "array_multi"];

        for pattern_name in &pattern_names {
            let pattern_match = patternizer.match_pattern(pattern_name, tokens);
            if !matches!(pattern_match, crate::PatternResult::NoMatch { .. }) {
                pattern_success = true;
                break;
            }
        }

        // Comprehensive pattern analysis using registered patterns
        let patterns = vec![
            "array_declaration".to_string(),
            "array_multi".to_string(),
        ];

        let base_confidence = samplizer.analyze_with_range(&patterns, tokens, token_range)?;

        // Enhanced scoring system using Patternizer's integrated type system
        let structural_score = patternizer.calculate_structure_score(tokens);
        let type_score = patternizer.calculate_type_score(tokens);
        let syntax_score = patternizer.calculate_syntax_score(tokens);
        let semantic_score = patternizer.calculate_semantic_score(tokens);

        // Refined weighted confidence calculation
        let enhanced_confidence = (base_confidence * 0.35)
            + (structural_score * 0.25)
            + (type_score * 0.20)
            + (syntax_score * 0.15)
            + (semantic_score * 0.05);

        // Adaptive threshold based on token complexity and pattern strength
        let threshold = match tokens.len() {
            0..=5 => 0.55,
            6..=10 => 0.65,
            11..=20 => 0.70,
            _ => 0.75,
        };

        let final_confidence = enhanced_confidence.clamp(0.0, 1.0);
        let is_array = pattern_success || final_confidence > threshold;

        Ok((is_array, final_confidence))
    }

    /// Extract comprehensive array information from tokens with enhanced validation
    fn extract_info(&self, tokens: &[Token]) -> Result<ArrayInfo> {
        let mut array_info = ArrayInfo::default();

        let mut in_brackets = false;
        let mut bracket_content = String::new();
        let mut bracket_depth = 0u32;
        let mut found_type = false;
        let mut found_identifier = false;
        let mut type_tokens = Vec::new();

        for (i, token) in tokens.iter().enumerate() {
            let token_str = token.to_string();

            match token_str.as_str() {
                // Enhanced storage class detection
                "static" | "const" | "extern" | "auto" | "register" | "volatile" => {
                    if array_info.storage_class.is_none() {
                        array_info.storage_class = Some(token_str);
                    }
                }
                // Improved bracket handling with depth tracking
                "[" => {
                    in_brackets = true;
                    bracket_depth += 1;
                    if bracket_depth == 1 {
                        bracket_content.clear();
                    }
                }
                "]" => {
                    bracket_depth = bracket_depth.saturating_sub(1);
                    if bracket_depth == 0 {
                        in_brackets = false;
                        let content = bracket_content.trim();
                        if !content.is_empty() {
                            // Validate numeric content
                            if content.chars().all(|c| c.is_numeric()) {
                                array_info.size = Some(content.to_string());
                            } else {
                                array_info.size = Some(content.to_string()); // Could be expression
                            }
                        } else {
                            array_info.size = Some("dynamic".to_string());
                        }
                    }
                }
                // Enhanced initialization detection
                "=" => {
                    array_info.has_initializer = true;
                    // Look ahead for initializer patterns
                    if i + 1 < tokens.len() {
                        match tokens[i + 1].to_string().as_str() {
                            "{" => array_info.has_initializer = true,
                            _ => {}
                        }
                    }
                }
                // Comprehensive type detection
                "int" | "char" | "float" | "double" | "void" | "long" | "short" | "unsigned"
                | "signed" | "bool" | "size_t" | "uint8_t" | "uint16_t" | "uint32_t"
                | "uint64_t" | "int8_t" | "int16_t" | "int32_t" | "int64_t" => {
                    type_tokens.push(token_str.clone());
                    found_type = true;
                }
                // Pointer and modifier handling
                "*" => {
                    type_tokens.push(token_str);
                }
                // Enhanced identifier extraction
                _ => {
                    if in_brackets && bracket_depth > 0 {
                        if !bracket_content.is_empty() {
                            bracket_content.push(' ');
                        }
                        bracket_content.push_str(&token_str);
                    } else if !found_identifier
                        && self.is_identifier(&token_str)
                        && found_type
                    {
                        array_info.name = token_str;
                        found_identifier = true;
                    }
                }
            }
        }

        // Post-processing and validation
        if array_info.name.is_empty() {
            array_info.name = "unnamed_array".to_string();
        }

        // Validate bracket balance
        if bracket_depth != 0 {
            return Err(C2RError::new(
                crate::Kind::Unexpected,
                crate::Reason::Character("Unmatched brackets in array declaration"),
                None,
            ));
        }

        Ok(array_info)
    }

    fn is_identifier(&self, s: &str) -> bool {
        !s.is_empty()
            && s.chars()
            .next()
            .map_or(false, |c| c.is_alphabetic() || c == '_')
            && s.chars().all(|c| c.is_alphanumeric() || c == '_')
            && !matches!(
                s,
                "static" | "const" | "extern" | "auto" | "register" | "volatile"
            )
    }

    fn convert_tokens(&self, array_info: &ArrayInfo, tokens: &[Token]) -> Result<String> {
        let element_type = array_info.element_type.clone();
        let rust_element_type = self.convert_c_type_to_rust(&element_type);

        let rust_code = match array_info.size.as_ref() {
            Some(size) if size == "dynamic" => {
                format!(
                    "let mut {}: Vec<{}> = Vec::new();",
                    array_info.name, rust_element_type
                )
            }
            Some(size) => {
                // Validate size is numeric
                if size.chars().all(|c| c.is_numeric()) {
                    format!(
                        "let {}: [{}; {}] = [Default::default(); {}];",
                        array_info.name, rust_element_type, size, size
                    )
                } else {
                    // Size is an expression, use Vec as fallback
                    format!(
                        "let mut {}: Vec<{}> = vec![Default::default(); {}];",
                        array_info.name, rust_element_type, size
                    )
                }
            }
            None => {
                format!(
                    "let mut {}: Vec<{}> = Vec::new();",
                    array_info.name, rust_element_type
                )
            }
        };

        Ok(rust_code)
    }
}
impl Handler for ArrayHandler {
    fn id(&self) -> Id {
        Id::get("array_handler")
    }

    fn role(&self) -> String {
        "array".to_string()
    }

    fn priority(&self) -> u64 {
        180
    }

    fn supported_patterns(&self) -> Vec<String> {
        vec![
            "array_declaration".to_string(),
            "array_multi".to_string(),
            "array_initialization".to_string(),
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
                Id::get(&format!("array_handler_can_process_{}", token_slot)),
                Some(self.id()),
                "can_process".to_string(),
                format!(
                    "Array detection: {} (confidence: {:.2})",
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

        // Validate that this is actually an array pattern before extracting
        let (is_array, confidence) =
            self.detect_pattern(&mut context.patternizer, &context.samplizer, &tokens, token_range.clone())?;
        if !is_array {
            return Ok(None);
        }

        let array_info = self.extract_info(&tokens)?;

        // Enhanced metadata with pattern detection results
        let mut metadata = vec![
            ("original_element_type".to_string(), array_info.element_type.clone()),
            (
                "has_size_specifier".to_string(),
                array_info.size.is_some().to_string(),
            ),
            (
                "has_initializer".to_string(),
                array_info.has_initializer.to_string(),
            ),
            (
                "storage_class".to_string(),
                array_info
                    .storage_class
                    .clone()
                    .unwrap_or_else(|| "auto".to_string()),
            ),
            (
                "detection_confidence".to_string(),
                format!("{:.3}", confidence),
            ),
            ("token_count".to_string(), tokens.len().to_string()),
        ];

        // Add size information to metadata if available
        if let Some(ref size) = array_info.size {
            metadata.push(("size".to_string(), size.clone()));
        }

        let extracted_array = ExtractedArray {
            id: Id::get(&gen_name(&self.id().name()).as_str()),
            info: array_info.clone(),
            tokens: tokens.clone(),
            token_range,
            metadata,
        };

        // Log successful extraction
        context.registry.add_report(
            Report::new(
                Id::get(&format!("array_extract_{}", token_slot)),
                Some(self.id()),
                "extract".to_string(),
                format!(
                    "Array extraction successful: {} elements, confidence: {:.2}",
                    array_info.size.as_ref().unwrap_or(&"dynamic".to_string()),
                    confidence
                ),
                ReportLevel::Info,
                Phase::Extract(None),
            )
                .with_tokens(tokens.len(), tokens.len())
                .with_success(true),
        );

        Ok(Some(ExtractedElement::Array(extracted_array)))
    }

    fn convert(
        &self,
        context: &mut Context,
        extracted_element: ExtractedElement,
    ) -> Result<Option<ConvertedElement>> {
        if let ExtractedElement::Array(array_data) = extracted_element {
            let rust_element_type = self.convert_c_type_to_rust(&array_data.info.element_type);

            // Generate Rust array code based on size information
            let rust_code = if let Some(ref size) = array_data.info.size {
                if size == "dynamic" {
                    format!(
                        "let {}: Vec<{}> = Vec::new();",
                        array_data.info.name, rust_element_type
                    )
                } else {
                    format!(
                        "let {}: [{}; {}] = [Default::default(); {}];",
                        array_data.info.name, rust_element_type, size, size
                    )
                }
            } else {
                format!(
                    "let {}: Vec<{}> = Vec::new();",
                    array_data.info.name, rust_element_type
                )
            };

            let mut metadata = vec![
                ("original_c_type".to_string(), array_data.info.element_type.clone()),
                ("rust_type".to_string(), rust_element_type.clone()),
                (
                    "storage_class".to_string(),
                    array_data
                        .info
                        .storage_class
                        .unwrap_or_else(|| "auto".to_string()),
                ),
                (
                    "has_initializer".to_string(),
                    array_data.info.has_initializer.to_string(),
                ),
                (
                    "is_dynamic".to_string(),
                    array_data
                        .info
                        .size
                        .as_ref()
                        .map_or("true".to_string(), |s| (s == "dynamic").to_string()),
                ),
            ];

            // Add size metadata if available
            if let Some(ref size) = array_data.info.size {
                metadata.push(("size".to_string(), size.clone()));
            }

            let converted_array = ConvertedArray {
                code: rust_code,
                metadata,
            };

            // Log conversion success
            context.registry.add_report(
                Report::new(
                    Id::get(&format!("array_convert_{}", array_data.id.name())),
                    Some(self.id()),
                    "convert".to_string(),
                    format!(
                        "Array conversion successful: {} -> {}",
                        array_data.info.element_type, rust_element_type
                    ),
                    ReportLevel::Info,
                    Phase::Convert(None),
                )
                    .with_success(true),
            );

            Ok(Some(ConvertedElement::Array(converted_array)))
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
        if let (ExtractedElement::Array(array_data), ConvertedElement::Array(converted_array)) =
            (extracted_element, converted_element)
        {
            let size_info = if !converted_array
                .metadata
                .iter()
                .any(|(key, _)| key == "size")
            {
                format!(
                    " with size {}",
                    converted_array
                        .metadata
                        .iter()
                        .find(|(key, _)| key == "size")
                        .unwrap()
                        .1
                )
            } else {
                " with dynamic size".to_string()
            };

            let storage_info = array_data
                .metadata
                .iter()
                .find(|(key, _)| key == "storage_class")
                .map(|(_, value)| {
                    if value != "auto" {
                        format!(" ({} storage)", value)
                    } else {
                        String::new()
                    }
                })
                .unwrap_or_default();

            let doc = format!(
                "/// Array: {}{}{}\n/// Converted from C array declaration\n/// Original C type information preserved in metadata\n",
                array_data.info.name, size_info, storage_info
            );
            Ok(Some(doc))
        } else {
            Ok(None)
        }
    }

    fn report(&self, context: &mut Context) -> Result<Report> {
        let reports = context.registry.get_reports_by_handler(&self.id().name());

        if reports.is_empty() {
            return Ok(Report::new(
                Id::get("array_handler_report"),
                Some(self.id()),
                "report".to_string(),
                "No array processing reports found".to_string(),
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
            Id::get("array_handler_report"),
            Some(self.id()),
            "report".to_string(),
            format!(
                "Array handler processed {} arrays: {} info, {} warnings, {} errors",
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
        vec![
            // Array declarations start with types
            "int".to_string(),
            "char".to_string(),
            "float".to_string(),
            "double".to_string(),
            "long".to_string(),
            "short".to_string(),
            "unsigned".to_string(),
            "signed".to_string(),
        ]
    }

    fn route(
        &self,
        context: &mut Context,
        token_slot: usize,
        token_range: Range<usize>,
        result: HandlerResult,
    ) -> Result<HandlerResult> {
        match result {
            HandlerResult::NotHandled(_, _, _) => {
                let redirect_request = RedirectRequest {
                    from_handler: self.id(),
                    token_range: token_range.clone(),
                    failed_patterns: self.supported_patterns(),
                    suggested_handler: Some(Id::get("variable_handler")), // Variables are closely related to arrays
                    metadata: vec![
                        (
                            "reason".to_string(),
                            "array_pattern_not_matched".to_string(),
                        ),
                        (
                            "fallback_strategy".to_string(),
                            "try_variable_handler".to_string(),
                        ),
                    ],
                };

                // Log the redirect attempt
                context.registry.add_report(
                    Report::new(
                        Id::get(&format!("array_handler_redirect_{}", token_slot)),
                        Some(self.id()),
                        "redirect".to_string(),
                        "Redirecting unhandled array-like tokens to variable handler".to_string(),
                        ReportLevel::Info,
                        Phase::Process(None),
                    )
                        .with_success(false),
                );

                Ok(result)
            }
            _ => Ok(result), // Pass through successful results unchanged
        }
    }
}
impl Default for ArrayHandler {
    fn default() -> Self {
        Self::new()
    }
}