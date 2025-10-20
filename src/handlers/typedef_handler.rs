//! Typedef handler implementation using trait-based system
//! Uses Patternizer and Samplizer for intelligent typedef pattern detection and processing

use crate::TYPE_CONVERSION_MAP;
use crate::{
    gen_name, info::TypedefInfo, Context, ConvertedElement, ConvertedTypedef, ExtractedElement,
    ExtractedTypedef, Handler, HandlerResult, Id, Phase, RedirectRequest, Report, ReportLevel,
    Result, Token,
};
use std::ops::Range;

#[derive(Debug, Clone)]
pub struct TypedefHandler;

impl TypedefHandler {
    pub fn new() -> Self {
        Self
    }

    /// Enhanced pattern detection using both Patternizer and Samplizer
    fn detect_typedef_pattern(
        &self,
        patternizer: &mut crate::Patternizer,
        samplizer: &crate::Samplizer,
        tokens: &[Token],
        token_range: Range<usize>,
    ) -> Result<(bool, f64)> {
        if tokens.is_empty() {
            return Ok((false, 0.0));
        }

        // Try multiple registered patterns for typedef declarations
        let mut pattern_success = false;
        let pattern_names = vec!["typedef_simple", "typedef_struct", "typedef_enum", "typedef"];

        for pattern_name in &pattern_names {
            let pattern_match = patternizer.match_pattern(pattern_name, tokens);
            if !matches!(pattern_match, crate::PatternResult::NoMatch { .. }) {
                pattern_success = true;
                break;
            }
        }

        // Use Samplizer for confidence scoring with registered patterns
        let patterns = vec![
            "typedef_simple".to_string(),
            "typedef_struct".to_string(),
            "typedef_enum".to_string(),
        ];
        let base_confidence = samplizer.analyze_with_range(&patterns, tokens, token_range)?;

        // Boost confidence with heuristics
        let has_typedef = tokens.iter().any(|t| t.to_string() == "typedef");
        let has_semicolon = tokens.iter().any(|t| t.to_string() == ";");
        let confidence_boost = if has_typedef && has_semicolon { 0.2 } else if has_typedef { 0.1 } else { 0.0 };

        let final_confidence = (base_confidence + confidence_boost).min(1.0);
        let detected = pattern_success || (has_typedef && final_confidence > 0.6);

        Ok((detected, final_confidence))
    }


    /// Extract typedef information from tokens using pattern-based analysis
    fn extract_typedef_info(&self, tokens: &[Token], patternizer: &crate::Patternizer) -> Result<TypedefInfo> {
        let mut typedef_info = TypedefInfo::default();
        let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        // Skip typedef keyword and semicolon
        let relevant_tokens: Vec<&str> = token_strings
            .iter()
            .filter(|t| *t != "typedef" && *t != ";")
            .map(|s| s.as_str())
            .collect();

        if relevant_tokens.is_empty() {
            return Ok(typedef_info);
        }

        // The last token is typically the alias name
        if let Some(&alias) = relevant_tokens.last() {
            typedef_info.alias_name = alias.to_string();
        }

        // Everything before the alias is the original type
        if relevant_tokens.len() > 1 {
            typedef_info.original_type = relevant_tokens[..relevant_tokens.len() - 1].join(" ");
        }

        // Use Patternizer to determine typedef type
        typedef_info.typedef_type = self.determine_typedef_type(
            &typedef_info.original_type,
            tokens,
            patternizer,
        );

        Ok(typedef_info)
    }

    /// Determine the type of typedef using Patternizer's pattern analysis
    fn determine_typedef_type(
        &self,
        original_type: &str,
        tokens: &[Token],
        patternizer: &crate::Patternizer,
    ) -> String {
        // Check for complex types using Patternizer
        let type_str = original_type.to_lowercase();

        // Check for struct/enum/union keywords
        if type_str.contains("struct") {
            return "struct".to_string();
        }
        if type_str.contains("enum") {
            return "enum".to_string();
        }
        if type_str.contains("union") {
            return "union".to_string();
        }

        // Check for function pointer pattern
        if type_str.contains("(") && type_str.contains("*") {
            return "function_pointer".to_string();
        }

        // Check if it's a known C type using Patternizer
        for token in tokens {
            if patternizer.is_c_type(token) {
                return "primitive".to_string();
            }
        }

        "unknown".to_string()
    }

    /// Convert typedef to Rust code
    fn convert_info(&self, typedef_info: &TypedefInfo) -> Result<String> {
        let rust_code = match typedef_info.typedef_type.as_str() {
            "struct" => self.convert_struct(typedef_info),
            "enum" => self.convert_enum(typedef_info),
            "union" => self.convert_union(typedef_info),
            "function_pointer" => self.convert_function_pointer(typedef_info),
            "primitive" => self.convert_primitive(typedef_info),
            _ => format!(
                "// TODO: Unsupported typedef: {}",
                typedef_info.original_type
            ),
        };

        Ok(rust_code)
    }

    /// Convert struct typedef to Rust
    fn convert_struct(&self, typedef_info: &TypedefInfo) -> String {
        let rust_name = self.convert_identifier(&typedef_info.alias_name);

        if typedef_info.original_type.contains("{") {
            // Anonymous struct definition
            format!(
                "pub type {} = /* TODO: Implement struct fields */;",
                rust_name
            )
        } else {
            // Named struct reference
            let struct_name = typedef_info.original_type.replace("struct ", "");
            let rust_struct_name = self.convert_identifier(&struct_name.trim());
            format!("pub type {} = {};", rust_name, rust_struct_name)
        }
    }

    /// Convert enum typedef to Rust
    fn convert_enum(&self, typedef_info: &TypedefInfo) -> String {
        let rust_name = self.convert_identifier(&typedef_info.alias_name);

        if typedef_info.original_type.contains("{") {
            // Anonymous enum definition
            format!(
                "pub type {} = /* TODO: Implement enum variants */;",
                rust_name
            )
        } else {
            // Named enum reference
            let enum_name = typedef_info.original_type.replace("enum ", "");
            let rust_enum_name = self.convert_identifier(&enum_name.trim());
            format!("pub type {} = {};", rust_name, rust_enum_name)
        }
    }

    /// Convert union typedef to Rust
    fn convert_union(&self, typedef_info: &TypedefInfo) -> String {
        let rust_name = self.convert_identifier(&typedef_info.alias_name);
        format!(
            "pub type {} = /* TODO: Implement union as enum or alternative */;",
            rust_name
        )
    }

    /// Convert function pointer typedef to Rust
    fn convert_function_pointer(&self, typedef_info: &TypedefInfo) -> String {
        let rust_name = self.convert_identifier(&typedef_info.alias_name);
        // Simplified function pointer conversion - would need more sophisticated parsing
        format!(
            "pub type {} = fn() -> (); // TODO: Parse function signature",
            rust_name
        )
    }

    /// Convert primitive typedef to Rust
    fn convert_primitive(&self, typedef_info: &TypedefInfo) -> String {
        let rust_name = self.convert_identifier(&typedef_info.alias_name);
        let rust_type = self.convert_type(&typedef_info.original_type);
        format!("pub type {} = {};", rust_name, rust_type)
    }

    /// Convert C type to Rust type using TYPE_CONVERSION_MAP
    fn convert_type(&self, c_type: &str) -> String {

        let normalized = c_type
            .replace(" *", "*")
            .replace("* ", "*")
            .trim()
            .to_string();

        TYPE_CONVERSION_MAP.convert_type(&normalized)
            .or_else(|| TYPE_CONVERSION_MAP.convert_type(c_type.trim()))
            .unwrap_or_else(|| {
                eprintln!("⚠️  Unknown C type '{}' in typedef, keeping with comment", c_type);
                format!("{} /* TODO: Verify type conversion */", c_type)
            })
    }

    /// Convert C identifier to Rust naming convention
    fn convert_identifier(&self, identifier: &str) -> String {
        // Convert to PascalCase for type aliases
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

impl Handler for TypedefHandler {
    fn id(&self) -> Id {
        Id::get("typedef_handler")
    }

    fn role(&self) -> String {
        "typedef".to_string()
    }

    fn priority(&self) -> u64 {
        140
    }

    fn supported_patterns(&self) -> Vec<String> {
        vec![
            "typedef_declaration".to_string(),
            "typedef_struct".to_string(),
            "typedef_enum".to_string(),
            "typedef_union".to_string(),
            "typedef_function_pointer".to_string(),
            "typedef_primitive".to_string(),
        ]
    }

    fn supported_keywords(&self) -> Vec<String> {
        vec!["typedef".to_string()]
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
            self.detect_typedef_pattern(&mut context.patternizer, &context.samplizer, &tokens, token_range)?;

        context.registry.add_report(
            Report::new(
                Id::get(&format!("typedef_handler_can_process_{}", token_slot)),
                Some(self.id()),
                "can_process".to_string(),
                format!(
                    "Typedef detection: {} (confidence: {:.2})",
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

        let typedef_info = self.extract_typedef_info(&tokens, &context.patternizer)?;

        let extracted_typedef = ExtractedTypedef {
            id: Id::get(&gen_name(&self.id().name()).as_str()),
            info: typedef_info.clone(),
            tokens: tokens.clone(),
            token_range,
            metadata: vec![
                (
                    "typedef_type".to_string(),
                    typedef_info.typedef_type.clone(),
                ),
                (
                    "original_type".to_string(),
                    typedef_info.original_type.clone(),
                ),
                ("alias_name".to_string(), typedef_info.alias_name.clone()),
            ],
        };

        Ok(Some(ExtractedElement::Typedef(extracted_typedef)))
    }

    fn convert(
        &self,
        context: &mut Context,
        element: ExtractedElement,
    ) -> Result<Option<ConvertedElement>> {
        if let ExtractedElement::Typedef(extracted) = element {
            let tokens = extracted.tokens.clone();
            if tokens.is_empty() {
                return Ok(None);
            }

            let typedef_info = extracted.info.clone();
            let rust_code = self.convert_info(&typedef_info)?;

            let converted_typedef = ConvertedTypedef {
                code: rust_code.clone(),
                metadata: Vec::new(),
            };

            Ok(Some(ConvertedElement::Typedef(converted_typedef)))
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
        let extracted_typedef = if let ExtractedElement::Typedef(extracted) = extracted_element {
            extracted
        } else {
            return Ok(None);
        };

        let converted_typedef = if let ConvertedElement::Typedef(converted) = converted_element {
            converted
        } else {
            return Ok(None);
        };

        let mut doc = format!(
            "/// Type alias: {}\n/// Original C type: {}\n/// Typedef type: {}\n",
            extracted_typedef.info.alias_name,
            extracted_typedef.info.original_type,
            extracted_typedef.info.typedef_type
        );

        if extracted_typedef.info.original_type.contains("struct") {
            doc.push_str("/// Note: This is a struct typedef\n");
        } else if extracted_typedef.info.original_type.contains("enum") {
            doc.push_str("/// Note: This is an enum typedef\n");
        } else if extracted_typedef.info.original_type.contains("union") {
            doc.push_str("/// Note: This is a union typedef\n");
        } else if extracted_typedef.info.original_type.contains("(") {
            doc.push_str("/// Note: This is a function pointer typedef\n");
        }

        Ok(Some(doc))
    }

    fn report(&self, context: &mut Context) -> Result<Report> {
        let reports = context.registry.get_reports_by_handler(&self.id().name());

        if reports.is_empty() {
            return Ok(Report::new(
                Id::get("typedef_handler_report"),
                Some(self.id()),
                "report".to_string(),
                "No typedef processing reports found".to_string(),
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
            Id::get("typedef_handler_report"),
            Some(self.id()),
            "report".to_string(),
            format!(
                "Typedef handler processed {} typedefs: {} info, {} warnings, {} errors",
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
            failed_patterns: vec!["typedef".to_string()],
            suggested_handler: None,
            metadata: Vec::new(),
        };
        // Return the result as-is to avoid deadlock in redirect workflow
        Ok(result)
    }
}

impl Default for TypedefHandler {
    fn default() -> Self {
        Self::new()
    }
}

pub fn create_typedef_handler() -> TypedefHandler {
    TypedefHandler::new()
}
