//! Function handler implementation using trait-based system
//! Uses Patternizer and Samplizer for intelligent pattern detection and processing

use crate::common::IdentifierCase;
use crate::handlers::common::convert_identifier_to_rust;
use crate::info::FunctionInfo;
use crate::{gen_name, PatternResult, RedirectRequest, Token};
use crate::{
    Context, ConvertedElement, ConvertedFunction, ExtractedElement, ExtractedFunction, Handler,
    HandlerResult, Id, Phase, Report, ReportLevel, Result,
};
use std::collections::HashMap;
use std::ops::Range;

/// Pattern matching result for function analysis
#[derive(Debug, Clone)]
struct PatternMatch {
    pattern_name: String,
    captures: HashMap<String, String>,
    confidence: f64,
    range: Range<usize>,
}

/// Function handler struct implementing the Handler trait with constant functions
#[derive(Debug)]
pub struct FunctionHandler;

impl FunctionHandler {
    pub fn new() -> Self {
        Self
    }

    /// Convert C type string to Rust type using TYPE_CONVERSION_MAP
    /// Handles pointers, const qualifiers, arrays, and complex types
    fn convert_c_type_to_rust(&self, c_type: &str) -> String {
        use crate::pattern::TYPE_CONVERSION_MAP;

        // Clean up the type string - normalize whitespace around * and []
        let normalized = c_type
            .replace(" *", "*")
            .replace("* ", "*")
            .replace(" [", "[")
            .replace("[ ", "[")
            .replace(" ]", "]")
            .replace("] ", "]")
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
        eprintln!("⚠️  Unknown C type '{}', keeping as-is", c_type);
        c_type.to_string()
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

        // Primary detection using patternizer for function patterns
        let function_patterns = vec![
            "function_definition".to_string(),
            "function_declaration".to_string(),
            "function_prototype".to_string(),
        ];

        let mut highest_confidence: f64 = 0.0;
        let mut pattern_detected = false;

        // Use patternizer to match function patterns
        for pattern in &function_patterns {
            match patternizer.match_pattern(pattern, tokens) {
                PatternResult::Match { .. } => {
                    pattern_detected = true;
                    highest_confidence = highest_confidence.max(0.9);
                }
                PatternResult::CountOf { offsets } => {
                    if !offsets.is_empty() {
                        pattern_detected = true;
                        highest_confidence = highest_confidence.max(0.6);
                    }
                }
                _ => {}
            }
        }

        // Enhanced analysis with samplizer for code characteristics
        let samplizer_result = samplizer.analyze_with_range(
            &function_patterns,
            tokens,
            token_range.clone(),
        ).unwrap_or(0.0);

        // Detect function syntax patterns
        let has_function_syntax = self.detect_function_syntax(tokens);
        let syntax_confidence = if has_function_syntax { 0.7 } else { 0.0 };

        // Combine confidence scores with weighted average
        let combined_confidence = (highest_confidence * 0.4) + (samplizer_result * 0.2) + (syntax_confidence * 0.4);

        // Accept if pattern detected OR if syntax looks like a function (even with lower confidence)
        let final_detected = pattern_detected || (has_function_syntax && combined_confidence > 0.4) || has_function_syntax;

        Ok((final_detected, combined_confidence.max(if has_function_syntax { 0.7 } else { 0.0 })))
    }

    /// Detect function syntax patterns
    fn detect_function_syntax(&self, tokens: &[Token]) -> bool {
        let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        if token_strings.len() < 3 {
            return false; // Too short to be a function
        }

        // Check for return types
        let has_return_type = token_strings.iter().any(|t| {
            matches!(t.as_str(), "void" | "int" | "char" | "float" | "double" | "long" | "short" | "unsigned" | "signed")
        });

        // Check for function signature pattern: type identifier(params)
        // Look for pattern: <type> <identifier> (
        let has_function_signature = token_strings.windows(3).any(|window| {
            matches!(window[0].as_str(), "void" | "int" | "char" | "float" | "double" | "long" | "short" | "unsigned" | "signed") &&
                !window[1].is_empty() &&
                window[1].chars().next().map_or(false, |c| c.is_alphabetic() || c == '_') &&
                window[2] == "("
        });

        // Check for parameter parentheses
        let has_params = token_strings.iter().any(|t| t == "(") &&
            token_strings.iter().any(|t| t == ")");

        // Check for function body (opening brace)
        let has_body = token_strings.iter().any(|t| t == "{");

        // Accept if: has return type AND (has signature OR (has params AND has body))
        has_return_type && (has_function_signature || (has_params && has_body))
    }

    /// Extract function information using pattern analysis
    fn extract_info(
        &self,
        tokens: &[Token],
        pattern_results: &[PatternMatch],
    ) -> Result<FunctionInfo> {
        let mut function_info = FunctionInfo {
            name: String::new(),
            return_type: "void".to_string(),
            parameters: Vec::new(),
            is_static: false,
            is_inline: false,
            has_body: false,
            complexity: "low".to_string(),
            is_declaration: false,
            is_definition: false,
            is_extern: false,
            is_unsafe: false,
            is_variadic: false,
            parameter_count: 0,
            storage_class: None,
        };

        // Extract information from pattern matches
        for pattern_match in pattern_results {
            match pattern_match.pattern_name.as_str() {
                "function_declaration" => {
                    function_info.is_declaration = true;
                    self.extract_declaration_info(tokens, &pattern_match.range, &mut function_info)?;
                }
                "function_definition" => {
                    function_info.is_definition = true;
                    function_info.has_body = self.detect_body(tokens, &pattern_match.range);
                    self.extract_definition_info(tokens, &pattern_match.range, &mut function_info)?;
                }
                "function_prototype" => {
                    self.extract_prototype_info(tokens, &pattern_match.range, &mut function_info)?;
                }
                "storage_class" => {
                    self.extract_storage_class(tokens, &pattern_match.range, &mut function_info)?;
                }
                _ => {}
            }
        }

        // Parse function signature using pattern-based approach
        self.parse_function_signature_with_patterns(tokens, &mut function_info)?;

        // Set complexity based on code characteristics
        function_info.complexity = self.calculate_complexity(&function_info);

        Ok(function_info)
    }

    /// Extract function declaration information using pattern matching
    fn extract_declaration_info(&self, tokens: &[Token], range: &Range<usize>, info: &mut FunctionInfo) -> Result<()> {
        let range_tokens = &tokens[range.clone()];

        // Use pattern matching for storage class detection
        let storage_patterns = ["static_specifier", "extern_specifier", "inline_specifier"];
        for pattern in &storage_patterns {
            // Simulate patternizer usage for storage class detection
            if range_tokens.iter().any(|t| match pattern {
                &"static_specifier" => t.to_string() == "static",
                &"extern_specifier" => t.to_string() == "extern",
                &"inline_specifier" => t.to_string() == "inline",
                _ => false,
            }) {
                match *pattern {
                    "static_specifier" => {
                        info.is_static = true;
                        info.storage_class = Some("static".to_string());
                    }
                    "extern_specifier" => {
                        info.is_extern = true;
                        info.storage_class = Some("extern".to_string());
                    }
                    "inline_specifier" => info.is_inline = true,
                    _ => {}
                }
            }
        }

        Ok(())
    }

    /// Calculate function complexity based on characteristics
    fn calculate_complexity(&self, function_info: &FunctionInfo) -> String {
        let mut complexity_score = 0;

        // Base complexity from parameter count
        complexity_score += function_info.parameter_count as i32;

        // Add complexity for various characteristics
        if function_info.is_variadic { complexity_score += 2; }
        if function_info.has_body { complexity_score += 1; }
        if function_info.is_unsafe { complexity_score += 3; }
        if function_info.is_static { complexity_score += 1; }

        // Return type complexity
        match function_info.return_type.as_str() {
            "void" => {}
            "int" | "char" | "float" => complexity_score += 1,
            "double" | "long" => complexity_score += 2,
            _ if function_info.return_type.contains("*") => complexity_score += 3,
            _ => complexity_score += 2,
        }

        match complexity_score {
            0..=2 => "low".to_string(),
            3..=6 => "medium".to_string(),
            _ => "high".to_string(),
        }
    }

    /// Extract function definition information using pattern-based detection
    fn extract_definition_info(&self, tokens: &[Token], range: &Range<usize>, info: &mut FunctionInfo) -> Result<()> {
        let range_tokens = &tokens[range.clone()];

        // Extract return type using pattern matching approach
        if let Some(return_type) = self.extract_return_type(range_tokens) {
            info.return_type = return_type;
        }

        // Check for unsafe operations using pattern detection
        let unsafe_patterns = ["pointer_operation", "memory_function", "unsafe_cast"];
        info.is_unsafe = unsafe_patterns.iter().any(|pattern| {
            range_tokens.iter().any(|t| match *pattern {
                "pointer_operation" => t.to_string() == "*",
                "memory_function" => matches!(t.to_string().as_str(), "malloc" | "free" | "memcpy" | "strcpy"),
                "unsafe_cast" => t.to_string().contains("cast"),
                _ => false,
            })
        });

        Ok(())
    }

    /// Extract function prototype information using pattern analysis
    fn extract_prototype_info(&self, tokens: &[Token], range: &Range<usize>, info: &mut FunctionInfo) -> Result<()> {
        let range_tokens = &tokens[range.clone()];

        // Check for variadic parameters using pattern matching
        info.is_variadic = range_tokens.iter().any(|t| t.to_string() == "...");

        Ok(())
    }

    /// Extract storage class information using pattern detection
    fn extract_storage_class(&self, tokens: &[Token], range: &Range<usize>, info: &mut FunctionInfo) -> Result<()> {
        let range_tokens = &tokens[range.clone()];

        let storage_class_patterns = [
            ("static", "static"),
            ("extern", "extern"),
            ("auto", "auto"),
            ("register", "register"),
        ];

        for (pattern, class_name) in &storage_class_patterns {
            if range_tokens.iter().any(|t| t.to_string() == *pattern) {
                info.storage_class = Some(class_name.to_string());
                break;
            }
        }

        Ok(())
    }

    /// Detect function body using pattern-based brace matching
    fn detect_body(&self, tokens: &[Token], range: &Range<usize>) -> bool {
        let range_tokens = &tokens[range.clone()];
        let mut brace_count = 0;

        // Use pattern-based approach for body detection
        for token in range_tokens {
            match token.to_string().as_str() {
                "{" => {
                    brace_count += 1;
                    return true; // Found opening brace indicates function body
                }
                ";" => {
                    if brace_count == 0 {
                        return false; // Semicolon without braces indicates declaration
                    }
                }
                _ => {}
            }
        }

        false
    }

    /// Extract return type using pattern-based type detection
    fn extract_return_type(&self, tokens: &[Token]) -> Option<String> {
        let type_patterns = ["void", "int", "char", "float", "double", "long", "short", "unsigned", "signed"];

        for (i, token) in tokens.iter().enumerate() {
            let token_str = token.to_string();
            if type_patterns.contains(&token_str.as_str()) {
                // Check if next token is an identifier (function name) using pattern matching
                if i + 1 < tokens.len() {
                    let next_token = tokens[i + 1].to_string();
                    if next_token.chars().all(|c| c.is_alphanumeric() || c == '_') {
                        return Some(token_str);
                    }
                }
            }
        }

        None
    }

    /// Parse function signature using pattern-based identifier detection
    fn parse_function_signature_with_patterns(&self, tokens: &[Token], info: &mut FunctionInfo) -> Result<()> {
        let mut i = 0;
        while i < tokens.len() {
            let token_str = tokens[i].to_string();

            // Look for function name pattern followed by opening parenthesis
            if i + 1 < tokens.len() && tokens[i + 1].to_string() == "(" {
                // Use pattern matching to validate identifier
                if self.is_valid_identifier_pattern(&token_str) && !self.is_reserved_keyword_pattern(&token_str) {
                    info.name = token_str;

                    // Parse parameters using pattern-based approach
                    self.parse_parameters_with_patterns(&tokens[i + 1..], info)?;
                    break;
                }
            }
            i += 1;
        }

        Ok(())
    }

    /// Check if token matches valid identifier pattern
    fn is_valid_identifier_pattern(&self, token: &str) -> bool {
        token.chars().all(|c| c.is_alphanumeric() || c == '_') && !token.is_empty()
    }

    /// Check if token matches reserved keyword pattern
    fn is_reserved_keyword_pattern(&self, token: &str) -> bool {
        matches!(token, "void" | "int" | "char" | "float" | "double" | "if" | "while" | "for")
    }

    /// Parse function parameters using pattern-based parenthesis matching
    fn parse_parameters_with_patterns(&self, tokens: &[Token], info: &mut FunctionInfo) -> Result<()> {
        if tokens.is_empty() || tokens[0].to_string() != "(" {
            return Ok(());
        }

        let mut paren_depth = 0;
        let mut current_param = String::new();

        for token in tokens {
            let token_str = token.to_string();

            match token_str.as_str() {
                "(" => {
                    paren_depth += 1;
                    if paren_depth > 1 {
                        current_param.push_str(&token_str);
                    }
                }
                ")" => {
                    paren_depth -= 1;
                    if paren_depth == 0 {
                        if !current_param.trim().is_empty() && current_param.trim() != "void" {
                            info.parameters.push(current_param.trim().to_string());
                        }
                        break;
                    } else {
                        current_param.push_str(&token_str);
                    }
                }
                "," if paren_depth == 1 => {
                    if !current_param.trim().is_empty() && current_param.trim() != "void" {
                        info.parameters.push(current_param.trim().to_string());
                    }
                    current_param.clear();
                }
                "..." => {
                    info.is_variadic = true;
                    current_param.push_str(&token_str);
                }
                "[" | "]" if paren_depth > 0 => {
                    // Don't add space before/after brackets
                    current_param.push_str(&token_str);
                }
                _ if paren_depth > 0 => {
                    // Add space before tokens (but not before brackets which are handled above)
                    if !current_param.is_empty() && !current_param.ends_with('[') {
                        current_param.push(' ');
                    }
                    current_param.push_str(&token_str);
                }
                _ => {}
            }
        }

        info.parameter_count = info.parameters.len();
        Ok(())
    }
}
impl Handler for FunctionHandler {
    /// Constant function approach - compile-time constants
    fn id(&self) -> Id {
        Id::get("function_handler")
    }

    fn role(&self) -> String {
        "function".to_string()
    }

    fn priority(&self) -> u64 {
        200 // High priority for function detection
    }

    fn supported_patterns(&self) -> Vec<String> {
        vec![
            "function_declaration".to_string(),
            "function_definition".to_string(),
            "function_call".to_string(),
        ]
    }

    fn detect(
        &self,
        context: &mut Context,
        token_slot: usize,
        token_range: Range<usize>,
    ) -> Result<bool> {
        // RACE CONDITION PREVENTION: Check if starting token is already consumed
        let all_tokens = context.tokenizer.slots()[token_slot].tokens();
        if token_range.start < all_tokens.len() && matches!(all_tokens[token_range.start], Token::n()) {
            return Ok(false);  // Starting token already consumed
        }

        let tokens = context
            .tokenizer
            .get_tokens(token_slot, token_range.clone());

        if tokens.is_empty() {
            return Ok(false);
        }
        let tokens = tokens;
        let (can_process, confidence) = self.detect_pattern(
            &mut context.patternizer,
            &context.samplizer,
            &tokens,
            token_range,
        )?;

        // Add report using Context::registry
        context.registry.add_report(
            Report::new(
                Id::get(&format!("function_handler_can_process_{}", token_slot)),
                Some(self.id()),
                "can_process".to_string(),
                format!(
                    "Function detection: {} (confidence: {:.2})",
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
        if token_range.is_empty() {
            return Ok(None);
        }

        // Get FRESH token array (important: don't cache this as it may have been modified by previous extractions)
        let all_tokens = context.tokenizer.slots()[token_slot].tokens();

        if token_range.start >= all_tokens.len() {
            return Ok(None);
        }

        // Start from token_range.start and find the COMPLETE function
        // Look for the closing brace to determine the actual end
        let start_pos = token_range.start;

        // Quick check: must start with a type keyword or storage specifier
        let first_token = all_tokens[start_pos].to_string();
        if !matches!(first_token.as_str(),
            "int" | "void" | "char" | "float" | "double" | "long" | "short" |
            "unsigned" | "signed" | "static" | "extern" | "inline" | "bool") {
            return Ok(None);
        }

        // Find the complete function by looking for matching braces
        let mut brace_count = 0;
        let mut in_function = false;
        let mut actual_end = start_pos;

        for (idx, token) in all_tokens[start_pos..].iter().enumerate() {
            let token_str = token.to_string();
            match token_str.as_str() {
                "{" => {
                    brace_count += 1;
                    in_function = true;
                }
                "}" => {
                    brace_count -= 1;
                    if in_function && brace_count == 0 {
                        actual_end = start_pos + idx + 1;  // Include the closing brace
                        break;
                    }
                }
                ";" if !in_function => {
                    // Function declaration without body
                    actual_end = start_pos + idx + 1;
                    break;
                }
                _ => {}
            }
        }

        if actual_end == start_pos {
            return Ok(None);  // Didn't find complete function
        }

        // Extract the complete function tokens
        let function_range = start_pos..actual_end;

        // RACE CONDITION CHECK: Now that we know the complete range, check if ANY token is already consumed
        if function_range.clone().any(|pos| matches!(all_tokens[pos], Token::n())) {
            return Ok(None);  // Part of this function was already processed
        }

        let tokens_subset = &all_tokens[function_range.clone()];

        // Use detect_pattern for consistent pattern analysis
        let (pattern_detected, confidence) = self.detect_pattern(
            &mut context.patternizer,
            &context.samplizer,
            tokens_subset,
            function_range.clone(),
        )?;

        if !pattern_detected {
            return Ok(None);
        }

        let pattern_results = vec![PatternMatch {
            pattern_name: "function_definition".to_string(),
            range: 0..tokens_subset.len(),  // Relative to tokens_subset
            confidence,
            captures: HashMap::new(),
        }];

        let function_info = self.extract_info(tokens_subset, &pattern_results)?;

        let extracted_function = ExtractedFunction {
            id: Id::get(&gen_name(&self.id().name()).as_str()),
            info: function_info.clone(),
            tokens: tokens_subset.to_vec(),
            token_range: function_range.clone(),  // Use actual function range, not input range
            metadata: vec![
                ("is_static".to_string(), function_info.is_static.to_string()),
                ("is_inline".to_string(), function_info.is_inline.to_string()),
                ("is_extern".to_string(), function_info.is_extern.to_string()),
                ("has_body".to_string(), function_info.has_body.to_string()),
                (
                    "parameter_count".to_string(),
                    function_info.parameter_count.to_string(),
                ),
                (
                    "is_variadic".to_string(),
                    function_info.is_variadic.to_string(),
                ),
            ],
        };

        // Mark the ACTUAL function tokens as consumed (Token::n()) so they won't be processed again
        context.mark_tokens_consumed(token_slot, function_range);

        Ok(Some(ExtractedElement::Function(extracted_function)))
    }

    fn convert(
        &self,
        context: &mut Context,
        element: ExtractedElement,
    ) -> Result<Option<ConvertedElement>> {
        if let ExtractedElement::Function(extracted) = element {
            let tokens = extracted.tokens.clone();
            if tokens.is_empty() {
                return Ok(None);
            }

            let function_info = extracted.info.clone();

            // Convert function name to Rust naming convention
            let rust_name = convert_identifier_to_rust(&function_info.name, IdentifierCase::SnakeCase);

            // Convert return type using TYPE_CONVERSION_MAP
            let rust_return_type = self.convert_c_type_to_rust(&function_info.return_type);

            // Convert parameters from C style (type name) to Rust style (name: type)
            let rust_params: Vec<String> = function_info.parameters.iter()
                .filter_map(|param| {
                    // Parse C-style parameter: "int value" or "const char * msg" or "char * argv[]"
                    let parts: Vec<&str> = param.split_whitespace().collect();
                    if parts.is_empty() {
                        return None;
                    }

                    // Last part may be the name with array brackets
                    let last_part = parts.last()?;

                    // Check if the name has array brackets (e.g., "argv[]")
                    let (param_name, array_suffix) = if last_part.contains('[') {
                        // Split name and brackets: "argv[]" -> ("argv", "[]")
                        if let Some(bracket_pos) = last_part.find('[') {
                            let name = &last_part[..bracket_pos];
                            let brackets = &last_part[bracket_pos..];
                            (name, brackets)
                        } else {
                            (last_part.as_ref(), "")
                        }
                    } else {
                        (last_part.as_ref(), "")
                    };

                    // Build C type: everything before last part + array suffix
                    let c_type = if array_suffix.is_empty() {
                        parts[..parts.len() - 1].join(" ")
                    } else {
                        // Add array brackets to the type
                        format!("{}{}", parts[..parts.len() - 1].join(" "), array_suffix)
                    };

                    // Convert C type to Rust type using TYPE_CONVERSION_MAP
                    let rust_type = self.convert_c_type_to_rust(&c_type);

                    // Format as Rust parameter: name: type
                    Some(format!("{}: {}", param_name, rust_type))
                })
                .collect();

            let rust_code = format!(
                "pub fn {}({}) -> {} {{\n    // TODO: Implement function body\n    unimplemented!()\n}}",
                rust_name,
                rust_params.join(", "),
                rust_return_type
            );

            let converted_function = ConvertedFunction {
                code: rust_code,
                metadata: vec![
                    ("original_name".to_string(), function_info.name),
                    ("converted_name".to_string(), rust_name),
                    ("complexity".to_string(), function_info.complexity),
                ],
            };

            Ok(Some(ConvertedElement::Function(converted_function)))
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
        let extracted_function = if let ExtractedElement::Function(extracted) = extracted_element {
            extracted
        } else {
            return Ok(None);
        };

        let converted_function = if let ConvertedElement::Function(converted) = converted_element {
            converted
        } else {
            return Ok(None);
        };

        let mut doc = format!(
            "/// Function: {}\n/// Return type: {}\n/// Converted from C function definition\n",
            extracted_function.info.name, extracted_function.info.return_type
        );

        if !extracted_function.info.parameters.is_empty() {
            doc.push_str("/// Parameters:\n");
            for param in &extracted_function.info.parameters {
                doc.push_str(&format!("/// - {}\n", param));
            }
        }

        if extracted_function.info.is_static {
            doc.push_str("/// Static function\n");
        }

        if extracted_function.info.is_inline {
            doc.push_str("/// Inline function\n");
        }

        if extracted_function.info.is_extern {
            doc.push_str("/// External function\n");
        }

        if extracted_function.info.is_variadic {
            doc.push_str("/// Variadic function\n");
        }

        Ok(Some(doc))
    }

    fn report(&self, context: &mut Context) -> Result<Report> {
        let reports = context
            .registry
            .get_reports_by_handler(&self.id().name());

        if reports.is_empty() {
            return Ok(Report::new(
                Id::get("function_handler_report"),
                Some(self.id()),
                "report".to_string(),
                "No function processing reports found".to_string(),
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
            Id::get("function_handler_report"),
            Some(self.id()),
            "report".to_string(),
            format!(
                "Function handler processed {} functions: {} info, {} warnings, {} errors",
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
            .with_tokens(total_tokens_processed, total_tokens_processed)
            .with_metadata("total_reports".to_string(), reports.len().to_string()))
    }

    fn supported_keywords(&self) -> Vec<String> {
        vec![
            // C type keywords that typically start functions
            "int".to_string(),
            "void".to_string(),
            "char".to_string(),
            "float".to_string(),
            "double".to_string(),
            "long".to_string(),
            "short".to_string(),
            "unsigned".to_string(),
            "signed".to_string(),
            "bool".to_string(),
            // Storage class specifiers
            "static".to_string(),
            "extern".to_string(),
            "inline".to_string(),
        ]
    }

    fn route(
        &self,
        context: &mut Context,
        token_slot: usize,
        token_range: Range<usize>,
        result: HandlerResult,
    ) -> Result<HandlerResult> {
        let tokens = context.tokenizer.slots()[token_slot].tokens()[token_range.clone()].to_vec();

        if tokens.is_empty() {
            return Ok(result);
        }

        let suggested_handler = self.suggest_next_handler(context, &tokens);

        let mut metadata = Vec::new();
        metadata.push((
            "analysis_reason".to_string(),
            format!("Function handler analyzed {} tokens", tokens.len()),
        ));

        let redirect_request = RedirectRequest {
            from_handler: self.id(),
            token_range: token_range.clone(),
            failed_patterns: self.supported_patterns(),
            suggested_handler,
            metadata,
        };

        Ok(result)
    }
}

impl FunctionHandler {
    /// Intelligently suggest which handler should try next based on token analysis
    /// ENHANCED with hierarchical subhandler routing - PRIVATE METHOD
    fn suggest_next_handler(&self, context: &mut Context, tokens: &[Token]) -> Option<Id> {
        let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        println!(
            "🧠 FunctionHandler analyzing tokens for intelligent redirect: {:?}",
            token_strings.iter().take(5).collect::<Vec<_>>()
        );

        // PRIORITY 1: Function-internal constructs (higher accuracy subhandlers)
        for (i, token_str) in token_strings.iter().enumerate() {
            match token_str.as_str() {
                // Loop constructs - route to specialized LoopHandler
                "for" | "while" | "do" if i + 1 < token_strings.len() => {
                    println!("🎯 FunctionHandler → LoopHandler (detected: {})", token_str);
                    return Some(Id::get("loop_handler"));
                }

                // Variable declarations/assignments - route to VariableHandler
                // Use Patternizer's type system for detection
                _ if i + 1 < token_strings.len() &&
                    context.patternizer.is_c_type(&tokens[i]) =>
                    {
                        // Check if this looks like a variable declaration
                        let next_token = &token_strings[i + 1];
                        if next_token
                            .chars()
                            .all(|c| c.is_alphanumeric() || c == '_' || c == '*')
                        {
                            println!(
                                "🎯 FunctionHandler → VariableHandler (detected: {} {})",
                                token_str, next_token
                            );
                            return Some(Id::get("variable_handler"));
                        }
                    }

                // Return statements - specialized statement handler
                "return" => {
                    println!("🎯 FunctionHandler → StatementHandler (detected: return)");
                    return Some(Id::get("statement_handler"));
                }

                // Function calls - expression handler
                _ if i + 1 < token_strings.len() && token_strings[i + 1] == "(" => {
                    // This looks like a function call
                    if token_str.chars().all(|c| c.is_alphanumeric() || c == '_') {
                        println!(
                            "🎯 FunctionHandler → ExpressionHandler (detected function call: {})",
                            token_str
                        );
                        return Some(Id::get("expression_handler"));
                    }
                }
                _ => {}
            }
        }

        // PRIORITY 2: Assignment operations - route to VariableHandler
        if token_strings
            .iter()
            .any(|t| matches!(t.as_str(), "=" | "+=" | "-=" | "*=" | "/="))
        {
            println!("🎯 FunctionHandler → VariableHandler (detected assignment)");
            return Some(Id::get("variable_handler"));
        }

        // PRIORITY 3: Mathematical expressions - route to ExpressionHandler
        if token_strings.iter().any(|t| {
            matches!(
                t.as_str(),
                "+" | "-" | "*" | "/" | "%" | "&&" | "||" | "==" | "!=" | "<" | ">"
            )
        }) {
            println!("🎯 FunctionHandler → ExpressionHandler (detected operators)");
            return Some(Id::get("expression_handler"));
        }

        // PRIORITY 4: Other top-level constructs
        for token_str in &token_strings {
            match token_str.as_str() {
                "struct" | "typedef" => {
                    println!(
                        "🎯 FunctionHandler → StructHandler (detected: {})",
                        token_str
                    );
                    return Some(Id::get("struct_handler"));
                }
                "enum" => {
                    println!("🎯 FunctionHandler → EnumHandler (detected: enum)");
                    return Some(Id::get("enum_handler"));
                }
                "#include" | "#define" | "#ifdef" => {
                    println!(
                        "🎯 FunctionHandler → MacroHandler (detected: {})",
                        token_str
                    );
                    return Some(Id::get("macro_handler"));
                }
                "//" | "/*" => {
                    println!(
                        "🎯 FunctionHandler → CommentHandler (detected: {})",
                        token_str
                    );
                    return Some(Id::get("comment_handler"));
                }
                "[" | "]" => {
                    println!("🎯 FunctionHandler → ArrayHandler (detected: array)");
                    return Some(Id::get("array_handler"));
                }
                _ => {}
            }
        }

        // PRIORITY 5: Default fallback
        println!("🎯 FunctionHandler → GlobalHandler (fallback)");
        Some(Id::get("global_handler"))
    }
}

impl Default for FunctionHandler {
    fn default() -> Self {
        Self::new()
    }
}

/// Factory function to create a function handler
pub fn create_function_handler() -> FunctionHandler {
    FunctionHandler::new()
}
