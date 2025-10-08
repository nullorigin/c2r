//! Expression handler implementation using trait-based system
//! Uses Patternizer and Samplizer for intelligent expression pattern detection and processing

use crate::{
    Context, ConvertedElement, ConvertedExpression, ElementInfo, ExtractedElement, ExtractedExpression, Handler, HandlerPhase, HandlerReport, HandlerResult, Id, RedirectRequest, ReportLevel, Result, context
};
use crate::info::ExpressionInfo;
use std::ops::Range;

/// Expression handler struct implementing the Handler trait
#[derive(Debug, Clone)]
pub struct ExpressionHandler;

impl ExpressionHandler {
    pub fn new() -> Self {
        Self
    }
    
    /// Enhanced pattern detection using both Patternizer and Samplizer
    fn detect_expression_pattern(&self, patternizer: &mut crate::Patternizer, samplizer: &crate::Samplizer, tokens: &[crate::Token], token_range: Range<usize>) -> Result<(bool, f64)> {
        // First use Patternizer for structural detection
        let pattern_match = patternizer.match_pattern("expression", tokens);
        
        // Then use Samplizer for confidence scoring
        let patterns = vec!["expression".to_string()];
        let confidence = samplizer.analyze_with_range(&patterns, tokens, token_range)?;
        
        // Combined heuristics for expression detection
        let has_expression_structure = self.has_expression_structure(tokens);
        let enhanced_confidence = if has_expression_structure {
            confidence * 1.1  // Boost confidence if structure looks expression-like
        } else {
            confidence * 0.8  // Reduce if structure doesn't match
        };
        
        let pattern_success = !matches!(pattern_match, crate::PatternResult::NoMatch { .. });
        
        Ok((pattern_success || enhanced_confidence > 0.6, enhanced_confidence))
    }
    
    /// Analyze token structure for expression-like patterns
    fn has_expression_structure(&self, tokens: &[crate::Token]) -> bool {
        if tokens.is_empty() {
            return false;
        }
        
        // Check for common expression indicators
        let expression_indicators = [
            "=", "==", "!=", "<", ">", "<=", ">=",
            "+", "-", "*", "/", "%",
            "&&", "||", "!",
            "++", "--",
            "+=", "-=", "*=", "/=",
            "sizeof", "(",
        ];
        
        let has_operators = tokens.iter().any(|token| {
            expression_indicators.contains(&token.to_string().as_str())
        });
        
        let has_semicolon = tokens.iter().any(|token| token.to_string() == ";");
        
        has_operators || has_semicolon
    }
    
    /// Extract expression information from tokens
    fn extract_expression_info(&self, tokens: &[crate::Token]) -> Result<ExpressionInfo> {
        let mut expression_info = ExpressionInfo::default();
        expression_info.raw_expression = tokens.iter()
            .map(|t| t.to_string())
            .collect::<Vec<_>>()
            .join(" ");
        
        // Determine expression type
        expression_info.expression_type = self.determine_expression_type(tokens);
        
        // Extract operands and operators
        self.extract_operands_and_operators(tokens, &mut expression_info);
        
        Ok(expression_info)
    }
    
    /// Determine the type of expression
    fn determine_expression_type(&self, tokens: &[crate::Token]) -> String {
        let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        
        // Check for assignment
        if token_strings.contains(&"=".to_string()) && 
           !token_strings.contains(&"==".to_string()) {
            return "assignment".to_string();
        }
        
        // Check for function call
        if token_strings.contains(&"(".to_string()) && 
           token_strings.contains(&")".to_string()) &&
           !token_strings.contains(&"sizeof".to_string()) {
            return "function_call".to_string();
        }
        
        // Check for sizeof
        if token_strings.contains(&"sizeof".to_string()) {
            return "sizeof".to_string();
        }
        
        // Check for cast
        if self.has_cast_pattern(&token_strings) {
            return "cast".to_string();
        }
        
        // Check for binary operations
        let binary_ops = ["==", "!=", "<", ">", "<=", ">=", "+", "-", "*", "/", "%", "&&", "||"];
        if binary_ops.iter().any(|op| token_strings.contains(&op.to_string())) {
            return "binary".to_string();
        }
        
        // Check for unary operations
        let unary_ops = ["!", "++", "--", "+", "-"];
        if unary_ops.iter().any(|op| token_strings.contains(&op.to_string())) {
            return "unary".to_string();
        }
        
        "statement".to_string()
    }
    
    /// Check for cast expression pattern like (type)expression
    fn has_cast_pattern(&self, tokens: &[String]) -> bool {
        if tokens.len() < 4 {
            return false;
        }
        
        // Look for pattern: ( type ) expression
        for i in 0..tokens.len() - 3 {
            if tokens[i] == "(" && tokens[i + 2] == ")" {
                // Check if the middle token looks like a type
                let potential_type = &tokens[i + 1];
                if self.looks_like_type(potential_type) {
                    return true;
                }
            }
        }
        
        false
    }
    
    /// Check if a token looks like a C type
    fn looks_like_type(&self, token: &str) -> bool {
        let c_types = [
            "int", "char", "short", "long", "float", "double", "void",
            "unsigned", "signed", "const", "volatile", "struct", "enum", "union"
        ];
        
        c_types.contains(&token) || token.ends_with("_t")
    }
    
    /// Extract operands and operators from expression
    fn extract_operands_and_operators(&self, tokens: &[crate::Token], info: &mut ExpressionInfo) {
        let operators = [
            "=", "==", "!=", "<", ">", "<=", ">=",
            "+", "-", "*", "/", "%",
            "&&", "||", "!",
            "++", "--",
            "+=", "-=", "*=", "/=",
        ];
        
        for token in tokens {
            let token_str = token.to_string();
            if operators.contains(&token_str.as_str()) {
                info.operators.push(token_str);
            } else if !token_str.is_empty() && token_str != ";" && 
                     token_str != "(" && token_str != ")" {
                info.operands.push(token_str);
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
            _ => self.convert_generic_expression(expression_info),
        }
    }
    
    /// Convert assignment expression to Rust
    fn convert_assignment_expression(&self, info: &ExpressionInfo) -> Result<String> {
        // Simple assignment conversion - would need more sophisticated parsing
        let rust_expr = info.raw_expression
            .replace("=", " = ")
            .trim()
            .to_string();
        
        Ok(format!("{};", rust_expr))
    }
    
    /// Convert function call expression to Rust
    fn convert_function_call_expression(&self, info: &ExpressionInfo) -> Result<String> {
        // Convert function calls - may need argument conversion
        let rust_expr = info.raw_expression.trim().to_string();
        Ok(format!("{};", rust_expr))
    }
    
    /// Convert sizeof expression to Rust
    fn convert_sizeof_expression(&self, info: &ExpressionInfo) -> Result<String> {
        // Convert sizeof to std::mem::size_of
        let expr = info.raw_expression.replace("sizeof", "std::mem::size_of");
        Ok(format!("{};", expr))
    }
    
    /// Convert cast expression to Rust
    fn convert_cast_expression(&self, info: &ExpressionInfo) -> Result<String> {
        // Convert C-style cast to Rust as cast
        // This is simplified - real implementation would parse the cast properly
        let rust_expr = info.raw_expression
            .replace("(int)", " as i32")
            .replace("(char)", " as i8")
            .replace("(float)", " as f32")
            .replace("(double)", " as f64");
        
        Ok(format!("{};", rust_expr))
    }
    
    /// Convert binary expression to Rust
    fn convert_binary_expression(&self, info: &ExpressionInfo) -> Result<String> {
        // Most binary expressions translate directly
        let rust_expr = info.raw_expression.trim().to_string();
        Ok(format!("{};", rust_expr))
    }
    
    /// Convert unary expression to Rust
    fn convert_unary_expression(&self, info: &ExpressionInfo) -> Result<String> {
        // Most unary expressions translate directly
        let rust_expr = info.raw_expression.trim().to_string();
        Ok(format!("{};", rust_expr))
    }
    
    /// Convert generic expression to Rust
    fn convert_generic_expression(&self, info: &ExpressionInfo) -> Result<String> {
        // Generic expression conversion
        let rust_expr = info.raw_expression.trim().to_string();
        if rust_expr.ends_with(';') {
            Ok(rust_expr)
        } else {
            Ok(format!("{};", rust_expr))
        }
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
    
    fn can_process(&self, context: &mut Context, token_slot: usize, token_range: Range<usize>) -> Result<bool> {
        let tokens = context.tokenizer.slots()[token_slot].tokens()[token_range.clone()].to_vec();
        
        if tokens.is_empty() {
            return Ok(false);
        }
        let tokens = tokens;
        
        let (can_process, confidence) = self.detect_expression_pattern(&mut context.patternizer, &context.samplizer, &tokens, token_range)?;
        
        context.registry.add_report_entry("expression_handler_can_process", HandlerReport::new(
                &format!("expression_handler_can_process_{}", token_slot),
                std::sync::Arc::new(self.id()),
                "Expression Handler (Trait)".to_string(),
                "can_process".to_string(),
                format!("Expression detection: {} (confidence: {:.2})", can_process, confidence),
                ReportLevel::Info,
                HandlerPhase::Process,
            ).with_tokens(tokens.len(), if can_process { tokens.len() } else { 0 })
             .with_success(can_process));
        
        Ok(can_process)
    }
    
    fn process(&self, context: &mut Context, token_slot: usize, token_range: Range<usize>) -> Result<HandlerResult> {
        let tokens = context.tokenizer.slots()[token_slot].tokens()[token_range.clone()].to_vec();

        
        if tokens.is_empty() {
            return Ok(HandlerResult::NotHandled(None, token_range.clone(), self.id()));
        }
        let tokens = tokens;
        

        context.registry.add_report_entry("expression_handler_process", HandlerReport::new(
                &format!("expression_handler_process_{}", token_slot),
                std::sync::Arc::new(self.id()),
                "Expression Handler (Trait)".to_string(),
                "process".to_string(),
                format!("Processing expression with {} tokens", tokens.len()),
                ReportLevel::Info,
                HandlerPhase::Process,
            ).with_tokens(tokens.len(), tokens.len())
             .with_success(true));
        
        let expression_info = self.extract_expression_info(&tokens)?;
        let rust_code = self.convert_to_rust(&expression_info)?;
        
        Ok(HandlerResult::Processed(
            Some(tokens),
            token_range,
            rust_code,
            self.id(),
        ))
    }
    
    fn extract(&self, context: &mut Context, token_slot: usize, token_range: Range<usize>) -> Result<Option<ExtractedElement>> {
        let tokens = context.tokenizer.slots()[token_slot].tokens()[token_range.clone()].to_vec();

        
        if tokens.is_empty() {
            return Ok(None);
        }
        let tokens = tokens;
        
        let expression_info = self.extract_expression_info(&tokens)?;
        
        let extracted_expression = ExtractedExpression {
            id: Id::get(Id::gen_name(&self.id().name()).as_str()),
            code: "// Generated expression code".to_string(),
            info: expression_info,
            tokens: tokens,
            token_range,
            metadata: Vec::new(),
        };
        
        Ok(Some(ExtractedElement::Expression(extracted_expression)))
    }
    
    fn convert(&self, context: &mut Context, token_slot: usize, token_range: Range<usize>) -> Result<Option<ConvertedElement>> {
        let tokens = context.tokenizer.slots()[token_slot].tokens()[token_range.clone()].to_vec();
        
        if tokens.is_empty() {
            return Ok(None);
        }
        let tokens = tokens;
        
            let expression_info = self.extract_expression_info(&tokens)?;
        let rust_code = self.convert_to_rust(&expression_info)?;
        
        let converted_expression = ConvertedExpression {
            expression_type: expression_info.expression_type.clone(),
            left_operand: expression_info.operands.get(0).unwrap_or(&String::new()).clone(),
            operator: expression_info.operators.get(0).unwrap_or(&String::new()).clone(),
            right_operand: expression_info.operands.get(1).unwrap_or(&String::new()).clone(),
            code: rust_code.clone(),
            result_type: None, // TODO: Extract from expression_info
            metadata: Vec::new(),
        };
        
        Ok(Some(ConvertedElement::Expression(converted_expression)))
    }
    
    fn document(&self, context: &mut Context, info: ElementInfo) -> Result<Option<String>> {
        if let ElementInfo::Expression(expression_info) = info {
            let doc = format!(
                "/// Expression type: {}\n/// Operator: {}\n/// Left operand: {}, Right operand: {}\n",
                expression_info.expression_type,
                expression_info.operator,
                expression_info.left_operand,
                expression_info.right_operand
            );
            Ok(Some(doc))
        } else {
            Ok(None)
        }
    }
    
    fn report(&self, context: &mut Context) -> Result<HandlerReport> {
        let reports = context.registry.get_reports_by_handler("expression_handler_trait");
        
        if reports.is_empty() {
            return Ok(HandlerReport::new(
                "expression_handler_report",
                std::sync::Arc::new(self.id()),
                "Expression Handler (Trait)".to_string(),
                "report".to_string(),
                "No reports found".to_string(),
                ReportLevel::Info,
                HandlerPhase::Report,
            ).with_success(true)
             .with_metadata("total_reports".to_string(), "0".to_string()));
        }
        let reports = reports;
        let (info_count, warning_count, error_count) = reports
            .iter()
            .fold((0, 0, 0), |(info, warn, err), report| {
                match report.level {
                    ReportLevel::Info => (info + 1, warn, err),
                    ReportLevel::Warning => (info, warn + 1, err),
                    ReportLevel::Error => (info, warn, err + 1),
                    ReportLevel::Debug => (info + 1, warn, err),
                }
            });
        
        Ok(HandlerReport::new(
            "expression_handler_report",
            std::sync::Arc::new(self.id()),
            "Expression Handler (Trait)".to_string(),
            "report".to_string(),
            format!(
                "Expression handler processed {} info, {} warnings, {} errors",
                info_count, warning_count, error_count
            ),
            ReportLevel::Info,
            HandlerPhase::Report,
        ).with_success(error_count == 0)
         .with_metadata("total_reports".to_string(), reports.len().to_string()))
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
    
    fn handle_redirect(&self, context: &mut Context, token_slot: usize, token_range: Range<usize>, result: HandlerResult) -> Result<HandlerResult> {
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
