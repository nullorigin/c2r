//! Global handler implementation using trait-based system
//! Handles global variables, constants, and other global scope declarations

use crate::{
    common::{is_primitive_type, is_valid_c_identifier}, gen_name, Context, ConvertedElement, ConvertedGlobal, ExtractedElement, ExtractedGlobal, GlobalInfo, Handler, HandlerResult, Id, Phase, RedirectRequest, RedirectResponse, Report, ReportLevel, Result, Token,
};
use std::ops::Range;

#[derive(Debug, Clone)]
pub struct GlobalHandler;

impl GlobalHandler {
    pub fn new() -> Self {
        Self
    }

    /// Convert C type string to Rust type using TYPE_CONVERSION_MAP
    /// Handles pointers, const qualifiers, and complex types
    fn convert_c_type_to_rust(&self, c_type: &str) -> String {
        use crate::pattern::TYPE_CONVERSION_MAP;

        // Clean up the type string - normalize whitespace around *
        let normalized = c_type
            .replace(" *", "*")
            .replace("* ", "*")
            .trim()
            .to_string();

        // Try direct lookup first
        if let Some(rust_type) = TYPE_CONVERSION_MAP.convert_type(&normalized) {
            return rust_type;
        }

        // Try with spaces (e.g., "const char *" vs "const char*")
        let with_spaces = c_type.trim().to_string();
        if let Some(rust_type) = TYPE_CONVERSION_MAP.convert_type(&with_spaces) {
            return rust_type;
        }

        // Fallback: unknown type, return as-is with warning
        eprintln!("⚠️  Unknown C type '{}', defaulting to i32", c_type);
        "i32".to_string()
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

        // CRITICAL: Reject if this looks like a function (has parentheses)
        // Global variables don't have parameter lists
        let has_parentheses = tokens.iter().any(|t| {
            matches!(t.to_string().as_str(), "(" | ")")
        });
        if has_parentheses {
            return Ok((false, 0.0));  // This is a function, not a global variable
        }

        // Try multiple registered patterns for global declarations
        let mut pattern_success = false;
        let pattern_names = vec!["global_variable", "extern_declaration", "static_variable", "global_constant"];

        for pattern_name in &pattern_names {
            let pattern_match = patternizer.match_pattern(pattern_name, tokens);
            if !matches!(pattern_match, crate::PatternResult::NoMatch { .. }) {
                pattern_success = true;
                break;
            }
        }

        // Enhanced pattern analysis for global declarations using registered patterns
        let patterns = vec![
            "global_variable".to_string(),
            "global_constant".to_string(),
            "extern_declaration".to_string(),
            "static_variable".to_string(),
        ];
        let base_confidence = samplizer.analyze_with_range(&patterns, tokens, token_range)?;

        // Enhanced detection logic with multiple criteria
        let has_global_keywords = tokens.iter().any(|t| {
            matches!(t.to_string().as_str(), "extern" | "static" | "const" | "volatile")
        });

        let has_type_indicators = tokens.iter().any(|t| {
            let token_str = t.to_string();
            is_primitive_type(t) || token_str.ends_with("_t") || token_str.contains("*")
        });

        let has_assignment = tokens.iter().any(|t| t.to_string() == "=");
        let has_semicolon = tokens.iter().any(|t| t.to_string() == ";");

        // Boost confidence for well-formed global declarations
        let structural_bonus = if has_global_keywords && has_type_indicators {
            0.2
        } else if has_assignment || has_semicolon {
            0.1
        } else {
            0.0
        };

        let adjusted_confidence = (base_confidence + structural_bonus).min(1.0);
        let structural_match = pattern_success || has_global_keywords || has_type_indicators;

        // Adaptive threshold based on token complexity
        let confidence_threshold = match tokens.len() {
            0..=3 => 0.4,
            4..=8 => 0.5,
            _ => 0.6,
        };

        Ok((
            structural_match && adjusted_confidence > confidence_threshold,
            adjusted_confidence,
        ))
    }

    /// Extract global information from tokens with enhanced pattern recognition
    fn extract_info(&self, tokens: &[Token], _patternizer: &crate::Patternizer) -> Result<GlobalInfo> {
        let mut global_info = GlobalInfo::default();
        let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        let mut type_tokens = Vec::new();
        let mut found_name = false;
        let mut i = 0;

        while i < token_strings.len() {
            let token_str = &token_strings[i];

            match token_str.as_str() {
                "extern" => {
                    global_info.is_extern = true;
                    global_info.storage_class = "extern".to_string();
                }
                "static" => {
                    global_info.is_static = true;
                    if global_info.storage_class.is_empty() {
                        global_info.storage_class = "static".to_string();
                    }
                }
                "const" => global_info.is_const = true,
                "volatile" => global_info.is_volatile = true,
                "=" => {
                    // Extract initializer value
                    let init_tokens: Vec<String> = token_strings[(i + 1)..]
                        .iter()
                        .take_while(|&t| t != ";" && t != ",")
                        .cloned()
                        .collect();
                    if !init_tokens.is_empty() {
                        global_info.initializer = Some(init_tokens.join(" "));
                    }
                    break;
                }
                ";" | "," => break,
                _ => {
                    // Enhanced type and name detection
                    if !found_name && (is_primitive_type(&tokens[i]) ||
                        token_str.ends_with("_t") ||
                        token_str.contains("*")) {
                        type_tokens.push(token_str.clone());
                    } else if !found_name &&
                        !type_tokens.is_empty() &&
                        global_info.name.is_empty() &&
                        is_valid_c_identifier(token_str) {
                        global_info.name = token_str.clone();
                        found_name = true;
                    } else if type_tokens.is_empty() &&
                        is_valid_c_identifier(token_str) &&
                        !["extern", "static", "const", "volatile"].contains(&token_str.as_str()) {
                        type_tokens.push(token_str.clone());
                    }
                }
            }
            i += 1;
        }

        // Construct type from collected tokens
        if !type_tokens.is_empty() {
            global_info.var_type = type_tokens.join(" ");
        }

        // Fallback name detection if still empty
        if global_info.name.is_empty() {
            for token_str in &token_strings {
                if is_valid_c_identifier(token_str) &&
                    !["extern", "static", "const", "volatile"].contains(&token_str.as_str()) &&
                    !is_primitive_type(&Token::s(token_str.clone())) {
                    global_info.name = token_str.clone();
                    break;
                }
            }
        }

        Ok(global_info)
    }


    /// Convert global declaration to Rust code with enhanced type handling
    fn convert_info(&self, global_info: &GlobalInfo) -> Result<String> {
        let rust_name = if global_info.is_const {
            global_info.name.to_uppercase()
        } else {
            global_info.name.clone()
        };

        // Use TYPE_CONVERSION_MAP instead of common.rs
        let rust_type = self.convert_c_type_to_rust(&global_info.var_type);
        let default_value = self.get_default_value(&rust_type);

        let rust_code = match (
            global_info.is_extern,
            global_info.is_const,
            global_info.is_static,
            global_info.initializer.as_ref(),
        ) {
            (true, _, _, _) => {
                format!(
                    "extern \"C\" {{\n    static {}: {};\n}}",
                    rust_name, rust_type
                )
            }
            (false, true, _, Some(init)) => {
                format!("pub const {}: {} = {};", rust_name, rust_type, init)
            }
            (false, true, _, None) => {
                format!("pub const {}: {} = {};", rust_name, rust_type, default_value)
            }
            (false, false, true, Some(init)) => {
                format!("pub static mut {}: {} = {};", rust_name, rust_type, init)
            }
            (false, false, true, None) => {
                format!("pub static mut {}: {} = {};", rust_name, rust_type, default_value)
            }
            (false, false, false, Some(init)) => {
                format!("pub static mut {}: {} = {};", rust_name, rust_type, init)
            }
            (false, false, false, None) => {
                format!("pub static mut {}: {} = {};", rust_name, rust_type, default_value)
            }
        };

        Ok(rust_code)
    }

    fn get_default_value(&self, rust_type: &str) -> String {
        match rust_type {
            "bool" => "false".to_string(),
            "i8" | "i16" | "i32" | "i64" | "isize" => "0".to_string(),
            "u8" | "u16" | "u32" | "u64" | "usize" => "0".to_string(),
            "f32" | "f64" => "0.0".to_string(),
            "()" => "()".to_string(),
            t if t.starts_with("*") => "std::ptr::null_mut()".to_string(),
            _ => "Default::default()".to_string(),
        }
    }
}
impl Handler for GlobalHandler {
    fn id(&self) -> Id {
        Id::get("global_handler")
    }

    fn role(&self) -> String {
        "global".to_string()
    }

    fn priority(&self) -> u64 {
        220
    }

    fn supported_patterns(&self) -> Vec<String> {
        vec![
            "global_variable".to_string(),
            "global_constant".to_string(),
            "extern_declaration".to_string(),
            "static_variable".to_string(),
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

        let (can_process, confidence) = self.detect_pattern(&mut context.patternizer, &context.samplizer, &tokens, token_range.clone())?;

        context.registry.add_report(
            Report::new(
                Id::get(&format!("global_handler_can_process_{}", token_slot)),
                Some(self.id()),
                "can_process".to_string(),
                format!(
                    "Global detection: {} (confidence: {:.2})",
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

        let global_info = self.extract_info(&tokens, &context.patternizer)?;

        // Validate extracted information
        if global_info.name.is_empty() || global_info.var_type.is_empty() {
            return Ok(None);
        }

        let extracted_global = ExtractedGlobal {
            id: Id::get(&gen_name(&self.id().name()).as_str()),
            info: global_info.clone(),
            tokens: tokens.clone(),
            token_range: token_range.clone(),
            metadata: vec![
                ("is_extern".to_string(), global_info.is_extern.to_string()),
                ("is_static".to_string(), global_info.is_static.to_string()),
                ("is_const".to_string(), global_info.is_const.to_string()),
                ("is_volatile".to_string(), global_info.is_volatile.to_string()),
                ("storage_class".to_string(), global_info.storage_class.clone()),
                ("has_initializer".to_string(), global_info.initializer.is_some().to_string()),
                ("original_name".to_string(), global_info.name.clone()),
                ("original_type".to_string(), global_info.var_type.clone()),
            ],
        };

        context.registry.add_report(
            Report::new(
                Id::get(&format!("global_handler_extract_{}", token_slot)),
                Some(self.id()),
                "extract".to_string(),
                format!("Extracted global variable: {}", global_info.name),
                ReportLevel::Info,
                Phase::Extract(None),
            )
                .with_tokens(tokens.len(), tokens.len())
                .with_success(true),
        );

        // Mark tokens as consumed so other handlers don't process them
        context.mark_tokens_consumed(token_slot, token_range);

        Ok(Some(ExtractedElement::Global(extracted_global)))
    }

    fn convert(
        &self,
        context: &mut Context,
        element: ExtractedElement,
    ) -> Result<Option<ConvertedElement>> {
        if let ExtractedElement::Global(extracted) = element {
            let tokens = extracted.tokens.clone();
            if tokens.is_empty() {
                return Ok(None);
            }

            let global_info = extracted.info.clone();
            let rust_code = self.convert_info(&global_info)?;

            let converted_global = ConvertedGlobal {
                code: rust_code,
                metadata: vec![
                    ("handler".to_string(), "global_handler".to_string()),
                    ("global_name".to_string(), global_info.name.clone()),
                    ("storage_class".to_string(), global_info.storage_class.clone()),
                    ("is_extern".to_string(), global_info.is_extern.to_string()),
                    ("is_static".to_string(), global_info.is_static.to_string()),
                    ("is_const".to_string(), global_info.is_const.to_string()),
                    ("is_volatile".to_string(), global_info.is_volatile.to_string()),
                    ("original_type".to_string(), global_info.var_type.clone()),
                    ("has_initializer".to_string(), global_info.initializer.is_some().to_string()),
                ],
            };

            Ok(Some(ConvertedElement::Global(converted_global)))
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
        let extracted_global = if let ExtractedElement::Global(extracted) = extracted_element {
            extracted
        } else {
            return Ok(None);
        };

        let converted_global = if let ConvertedElement::Global(converted) = converted_element {
            converted
        } else {
            return Ok(None);
        };

        let mut doc = format!(
            "/// Global variable: {}\n/// Type: {}\n/// Storage class: {}\n",
            extracted_global.info.name,
            extracted_global.info.var_type,
            extracted_global.info.storage_class
        );

        if extracted_global.info.is_extern {
            doc.push_str("/// External declaration\n");
        }

        if extracted_global.info.is_static {
            doc.push_str("/// Static storage\n");
        }

        if extracted_global.info.is_const {
            doc.push_str("/// Constant value\n");
        }

        if extracted_global.info.is_volatile {
            doc.push_str("/// Volatile qualifier\n");
        }

        if let Some(ref initializer) = extracted_global.info.initializer {
            doc.push_str(&format!("/// Initializer: {}\n", initializer));
        }

        Ok(Some(doc))
    }

    fn report(&self, context: &mut Context) -> Result<Report> {
        let reports = context
            .registry
            .get_reports_by_handler("global_handler");

        if reports.is_empty() {
            return Ok(Report::new(
                Id::get("global_handler_report"),
                Some(self.id()),
                "report".to_string(),
                "No global processing reports found".to_string(),
                ReportLevel::Info,
                Phase::Report(None),
            )
                .with_success(true)
                .with_metadata("total_reports".to_string(), "0".to_string()));
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

        Ok(Report::new(
            Id::get("global_handler_report"),
            Some(self.id()),
            "report".to_string(),
            format!(
                "Global handler processed {} globals: {} info, {} warnings, {} errors",
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
            .with_metadata("total_reports".to_string(), reports.len().to_string()))
    }

    fn supported_keywords(&self) -> Vec<String> {
        vec![
            // Storage class specifiers for globals
            "extern".to_string(),
            "static".to_string(),
            "const".to_string(),
            "volatile".to_string(),
            // Type keywords for global variables
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
        let redirect_request = RedirectRequest {
            from_handler: self.id(),
            token_range: token_range.clone(),
            failed_patterns: vec!["global_variable".to_string(), "global_constant".to_string()],
            suggested_handler: Some(Id::get("struct_handler")),
            metadata: vec![
                ("handler_type".to_string(), "global".to_string()),
                (
                    "redirect_reason".to_string(),
                    "pattern_mismatch".to_string(),
                ),
            ],
        };
        context
            .handlizer
            .process_redirect(redirect_request, RedirectResponse::default())
    }
}


impl Default for GlobalHandler {
    fn default() -> Self {
        Self::new()
    }
}
