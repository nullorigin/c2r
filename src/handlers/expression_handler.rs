//! Expression handler implementation using trait-based system
//! Uses Patternizer and Samplizer for intelligent expression pattern detection and processing

use crate::{
    gen_name, info::ExpressionInfo, Context, ConvertedElement, ConvertedExpression, ExtractedElement, ExtractedExpression, Handler, HandlerResult, Id, PatternResult, Patternizer, Phase, RedirectRequest, Report, ReportLevel, Result, Samplizer, Token,
};
use std::ops::Range;

/// Expression handler struct implementing the Handler trait
#[derive(Debug, Clone)]
pub struct ExpressionHandler;

impl ExpressionHandler {
    pub fn new() -> Self {
        Self
    }

    /// Enhanced pattern detection using both Patternizer and Samplizer
    fn detect_pattern(
        &self,
        patternizer: &mut Patternizer,
        samplizer: &Samplizer,
        tokens: &[Token],
        token_range: Range<usize>,
    ) -> Result<(bool, f64)> {
        if tokens.is_empty() {
            return Ok((false, 0.0));
        }

        // Try registered patterns for expressions
        let mut pattern_success = false;
        let pattern_names = vec!["binary_expression", "expression"];

        for pattern_name in &pattern_names {
            let pattern_match = patternizer.match_pattern(pattern_name, tokens);
            if !matches!(pattern_match, PatternResult::NoMatch { .. }) {
                pattern_success = true;
                break;
            }
        }

        // Simple heuristic: check for operators
        let has_operator = tokens.iter().any(|t| {
            matches!(t.to_string().as_str(), "+" | "-" | "*" | "/" | "=" | "==" | "!=" | "<" | ">" | "<=" | ">=")
        });

        // Confidence based on pattern match and heuristics
        let confidence = if pattern_success && has_operator {
            0.75
        } else if has_operator {
            0.65
        } else if pattern_success {
            0.55
        } else {
            0.0
        };

        Ok((pattern_success || has_operator, confidence))
    }


    /// Extract expression information from tokens
    fn extract_info(&self, tokens: &[Token], patternizer: &Patternizer) -> Result<ExpressionInfo> {
        let mut expression_info = ExpressionInfo::default();
        expression_info.raw_expression = tokens
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<_>>()
            .join(" ");

        // Determine expression type using Patternizer's type system
        expression_info.expression_type = self.determine_type(tokens, patternizer);

        // Extract operands and operators
        self.extract_operands_and_operators(tokens, &mut expression_info);

        Ok(expression_info)
    }

    /// Determine the type of expression
    fn determine_type(&self, tokens: &[Token], patternizer: &Patternizer) -> String {
        let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        // Check for assignment
        if token_strings.contains(&"=".to_string()) && !token_strings.contains(&"==".to_string()) {
            return "assignment".to_string();
        }

        // Check for function call
        if token_strings.contains(&"(".to_string())
            && token_strings.contains(&")".to_string())
            && !token_strings.contains(&"sizeof".to_string())
        {
            return "function_call".to_string();
        }

        // Check for sizeof
        if token_strings.contains(&"sizeof".to_string()) {
            return "sizeof".to_string();
        }

        // Check for cast
        if self.has_cast_pattern(tokens, patternizer) {
            return "cast".to_string();
        }

        // Check for binary operations
        let binary_ops = [
            "==", "!=", "<", ">", "<=", ">=", "+", "-", "*", "/", "%", "&&", "||",
        ];
        if binary_ops
            .iter()
            .any(|op| token_strings.contains(&op.to_string()))
        {
            return "binary".to_string();
        }

        // Check for unary operations
        let unary_ops = ["!", "++", "--", "+", "-"];
        if unary_ops
            .iter()
            .any(|op| token_strings.contains(&op.to_string()))
        {
            return "unary".to_string();
        }

        "statement".to_string()
    }

    /// Check for cast expression pattern like (type)expression
    fn has_cast_pattern(&self, tokens: &[Token], patternizer: &Patternizer) -> bool {
        if tokens.len() < 4 {
            return false;
        }

        // Look for pattern: ( type ) expression
        for i in 0..tokens.len() - 3 {
            if tokens[i].to_string() == "(" && tokens[i + 2].to_string() == ")" {
                // Check if the middle token looks like a type
                if self.looks_like_type(&tokens[i + 1], patternizer) {
                    // Ensure there's an expression after the cast
                    if i + 3 < tokens.len() {
                        return true;
                    }
                }
            }
        }

        false
    }

    /// Check if a token looks like a C type using Patternizer's type system
    fn looks_like_type(&self, token: &Token, patternizer: &Patternizer) -> bool {
        let token_str = token.to_string();

        // Use Patternizer's type system first
        if patternizer.is_c_type(token) {
            return true;
        }

        // Check for common C type patterns
        token_str.ends_with("_t") ||
            matches!(token_str.as_str(),
            "int" | "char" | "float" | "double" | "void" | "long" | "short" | 
            "signed" | "unsigned" | "const" | "volatile" | "struct" | "union" | "enum"
        )
    }

    /// Extract operands and operators from expression
    fn extract_operands_and_operators(&self, tokens: &[Token], info: &mut ExpressionInfo) {
        let operators = [
            "=", "==", "!=", "<", ">", "<=", ">=", "+", "-", "*", "/", "%",
            "&&", "||", "!", "++", "--", "+=", "-=", "*=", "/=", "%=", "&", "|", "^", "<<", ">>"
        ];

        for token in tokens {
            let token_str = token.to_string();
            if operators.contains(&token_str.as_str()) {
                if !info.operators.contains(&token_str) {
                    info.operators.push(token_str);
                }
            } else if !token_str.is_empty()
                && !matches!(token_str.as_str(), ";" | "(" | ")" | "{" | "}" | "[" | "]" | "," | "sizeof")
            {
                if !info.operands.contains(&token_str) {
                    info.operands.push(token_str);
                }
            }
        }
    }

    /// Convert expression to Rust code
    fn convert_to_rust(&self, expression_info: &ExpressionInfo) -> Result<String> {
        match expression_info.expression_type.as_str() {
            "assignment" => self.convert_assignment_expression(expression_info),
            "function_call" => self.convert_function_call_expression(expression_info),
            "sizeof" => self.convert_sizeof_expression(expression_info),
            "cast" => self.convert_cast_expression(expression_info),
            "binary" => self.convert_binary_expression(expression_info),
            "unary" => self.convert_unary_expression(expression_info),
            "statement" => self.convert_generic_expression(expression_info),
            _ => {
                // Fallback for unknown expression types
                Ok(format!("// TODO: Handle {} expression: {};",
                           expression_info.expression_type,
                           expression_info.raw_expression.trim()
                ))
            }
        }
    }

    /// Convert assignment expression to Rust
    fn convert_assignment_expression(&self, info: &ExpressionInfo) -> Result<String> {
        let mut rust_expr = info.raw_expression.trim().to_string();

        // Handle compound assignments
        rust_expr = rust_expr.replace("+=", " += ");
        rust_expr = rust_expr.replace("-=", " -= ");
        rust_expr = rust_expr.replace("*=", " *= ");
        rust_expr = rust_expr.replace("/=", " /= ");
        rust_expr = rust_expr.replace("%=", " %= ");

        // Handle simple assignment with proper spacing
        if !rust_expr.contains("+=") && !rust_expr.contains("-=") &&
            !rust_expr.contains("*=") && !rust_expr.contains("/=") && !rust_expr.contains("%=") {
            rust_expr = rust_expr.replace("=", " = ");
        }

        // Clean up extra whitespace
        rust_expr = rust_expr.split_whitespace().collect::<Vec<&str>>().join(" ");

        Ok(format!("{};", rust_expr))
    }

    /// Convert function call expression to Rust
    fn convert_function_call_expression(&self, info: &ExpressionInfo) -> Result<String> {
        let mut rust_expr = info.raw_expression.trim().to_string();

        // Convert common C library functions to Rust equivalents
        rust_expr = rust_expr.replace("printf(", "println!(");
        rust_expr = rust_expr.replace("malloc(", "Box::new(");
        rust_expr = rust_expr.replace("free(", "drop(");
        rust_expr = rust_expr.replace("strlen(", "str::len(");

        Ok(format!("{};", rust_expr))
    }

    /// Convert sizeof expression to Rust
    fn convert_sizeof_expression(&self, info: &ExpressionInfo) -> Result<String> {
        let mut rust_expr = info.raw_expression.trim().to_string();

        // Convert sizeof(type) to std::mem::size_of::<type>()
        if let Some(start) = rust_expr.find("sizeof(") {
            let end = rust_expr.find(')').unwrap_or(rust_expr.len());
            let type_part = &rust_expr[start + 7..end];
            let rust_type = self.convert_c_type(type_part);
            rust_expr = format!("std::mem::size_of::<{}>()", rust_type);
        } else {
            rust_expr = rust_expr.replace("sizeof", "std::mem::size_of");
        }

        Ok(format!("{};", rust_expr))
    }

    /// Convert cast expression to Rust
    fn convert_cast_expression(&self, info: &ExpressionInfo) -> Result<String> {
        let mut rust_expr = info.raw_expression.trim().to_string();

        // Convert C-style casts to Rust 'as' casts
        let c_to_rust_types = [
            ("(int)", " as i32"),
            ("(char)", " as i8"),
            ("(unsigned char)", " as u8"),
            ("(short)", " as i16"),
            ("(unsigned short)", " as u16"),
            ("(long)", " as i64"),
            ("(unsigned long)", " as u64"),
            ("(float)", " as f32"),
            ("(double)", " as f64"),
            ("(void*)", " as *mut ()"),
            ("(char*)", " as *mut i8"),
        ];

        for (c_cast, rust_cast) in c_to_rust_types.iter() {
            rust_expr = rust_expr.replace(c_cast, rust_cast);
        }

        Ok(format!("{};", rust_expr))
    }

    /// Convert binary expression to Rust
    fn convert_binary_expression(&self, info: &ExpressionInfo) -> Result<String> {
        let mut rust_expr = info.raw_expression.trim().to_string();

        // Handle logical operators
        rust_expr = rust_expr.replace("&&", " && ");
        rust_expr = rust_expr.replace("||", " || ");

        // Handle comparison operators
        rust_expr = rust_expr.replace("==", " == ");
        rust_expr = rust_expr.replace("!=", " != ");
        rust_expr = rust_expr.replace("<=", " <= ");
        rust_expr = rust_expr.replace(">=", " >= ");

        // Clean up spacing
        rust_expr = rust_expr.split_whitespace().collect::<Vec<&str>>().join(" ");

        Ok(format!("{};", rust_expr))
    }

    /// Convert unary expression to Rust
    fn convert_unary_expression(&self, info: &ExpressionInfo) -> Result<String> {
        let mut rust_expr = info.raw_expression.trim().to_string();

        // Handle increment/decrement operators
        if rust_expr.contains("++") {
            rust_expr = rust_expr.replace("++", " += 1");
        } else if rust_expr.contains("--") {
            rust_expr = rust_expr.replace("--", " -= 1");
        }

        // Handle logical not
        rust_expr = rust_expr.replace("!", "!");

        Ok(format!("{};", rust_expr))
    }

    /// Convert generic expression to Rust
    fn convert_generic_expression(&self, info: &ExpressionInfo) -> Result<String> {
        let mut rust_expr = info.raw_expression.trim().to_string();

        // Basic cleanup and normalization
        rust_expr = rust_expr.split_whitespace().collect::<Vec<&str>>().join(" ");

        if rust_expr.ends_with(';') {
            Ok(rust_expr)
        } else {
            Ok(format!("{};", rust_expr))
        }
    }

    /// Helper function to convert C types to Rust types using TYPE_CONVERSION_MAP
    fn convert_c_type(&self, c_type: &str) -> String {
        use crate::pattern::TYPE_CONVERSION_MAP;

        let normalized = c_type
            .replace(" *", "*")
            .replace("* ", "*")
            .trim()
            .to_string();

        TYPE_CONVERSION_MAP.convert_type(&normalized)
            .or_else(|| TYPE_CONVERSION_MAP.convert_type(c_type.trim()))
            .unwrap_or_else(|| c_type.to_string())
    }
}

impl Handler for ExpressionHandler {
    fn id(&self) -> Id {
        Id::get("expression_handler")
    }

    fn role(&self) -> String {
        "expression".to_string()
    }

    fn priority(&self) -> u64 {
        120
    }

    fn supported_patterns(&self) -> Vec<String> {
        vec![
            "arithmetic_expression".to_string(),
            "logical_expression".to_string(),
            "assignment_expression".to_string(),
            "function_call_expression".to_string(),
            "array_access_expression".to_string(),
        ]
    }

    fn detect(
        &self,
        context: &mut Context,
        token_slot: usize,
        token_range: Range<usize>,
    ) -> Result<bool> {
        let tokens = context.tokenizer.slots()[token_slot].tokens()[token_range.clone()].to_vec();

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

        context.registry.add_report_entry(
            "expression_handler_can_process",
            Report::new(
                Id::get(&format!("expression_handler_can_process_{}", token_slot)),
                Some(self.id()),
                "can_process".to_string(),
                format!(
                    "Expression detection: {} (confidence: {:.2})",
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
        let tokens = context.tokenizer.slots()[token_slot].tokens()[token_range.clone()].to_vec();

        if tokens.is_empty() {
            return Ok(None);
        }
        let tokens = tokens;

        let expression_info = self.extract_info(&tokens, &context.patternizer)?;

        let extracted_expression = ExtractedExpression {
            id: Id::get(&gen_name(&self.id().name()).as_str()),
            info: expression_info,
            tokens,
            token_range,
            metadata: Vec::new(),
        };

        Ok(Some(ExtractedElement::Expression(extracted_expression)))
    }

    fn convert(
        &self,
        context: &mut Context,
        ext: ExtractedElement,
    ) -> Result<Option<ConvertedElement>> {
        if let ExtractedElement::Expression(extracted) = ext {
            if extracted.tokens.is_empty() {
                return Ok(None);
            }
            let tokens = extracted.tokens.clone();
            let info = extracted.info.clone();
            let rust_code = self.convert_to_rust(&info)?;

            let converted_expression = ConvertedExpression {
                code: rust_code.clone(),
                metadata: Vec::new(),
            };
            Ok(Some(ConvertedElement::Expression(converted_expression)))
        } else {
            panic!("ExpressionHandler::convert: Invalid element type")
        }
    }

    fn document(
        &self,
        context: &mut Context,
        extracted: ExtractedElement,
        converted: ConvertedElement,
    ) -> Result<Option<String>> {
        let converted_expr = if let ConvertedElement::Expression(converted) = converted {
            converted
        } else {
            return Ok(None);
        };

        let extracted_expr = if let ExtractedElement::Expression(extracted) = extracted {
            extracted
        } else {
            return Ok(None);
        };

        let mut doc = format!(
            "/// Expression: {}\n/// Type: {}\n/// Converted to: {}\n",
            extracted_expr.info.raw_expression,
            extracted_expr.info.expression_type,
            converted_expr.code.trim_end_matches(';')
        );

        if !extracted_expr.info.operators.is_empty() {
            doc.push_str(&format!(
                "/// Operators: {}\n",
                extracted_expr.info.operators.join(", ")
            ));
        }

        if !extracted_expr.info.operands.is_empty() {
            doc.push_str(&format!(
                "/// Operands: {}\n",
                extracted_expr.info.operands.join(", ")
            ));
        }

        Ok(Some(doc))
    }

    fn report(&self, context: &mut Context) -> Result<Report> {
        let reports = context
            .registry
            .get_reports_by_handler("expression_handler");

        if reports.is_empty() {
            return Ok(Report::new(
                Id::get("expression_handler_report"),
                Some(self.id()),
                "report".to_string(),
                "No expression processing reports found".to_string(),
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

        let total_tokens_processed = reports.iter().map(|r| r.tokens_processed).sum::<usize>();

        Ok(Report::new(
            Id::get("expression_handler_report"),
            Some(self.id()),
            "report".to_string(),
            format!(
                "Expression handler processed {} expressions: {} info, {} warnings, {} errors",
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
        Vec::new() // Fallback handler - no specific keywords
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
            failed_patterns: vec!["expression".to_string()],
            suggested_handler: None,
            metadata: Vec::new(),
        };
        // Return the result as-is to avoid deadlock in redirect workflow
        Ok(result)
    }
}

impl Default for ExpressionHandler {
    fn default() -> Self {
        Self::new()
    }
}

pub fn create_expression_handler() -> ExpressionHandler {
    ExpressionHandler::new()
}
