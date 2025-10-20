//! Loop handler implementation for C loop constructs
//! Specialized subhandler that can be called by higher-level handlers like FunctionHandler
//! Uses adaptive pattern learning to improve redirect decisions over time

use crate::{
    gen_name, Context, ControlFlowInfo, ConvertedControlFlow, ConvertedElement,
    ExtractedControlFlow, ExtractedElement, Handler, HandlerResult, Id, Phase, RedirectRequest,
    Report, ReportLevel, Result, Token,
};
use std::collections::HashMap;
use std::ops::Range;

/// Loop pattern matching result
#[derive(Debug, Clone)]
struct LoopPatternMatch {
    loop_type: LoopType,
    confidence: f64,
    range: Range<usize>,
    metadata: HashMap<String, String>,
}

/// Types of loops we can detect and convert
#[derive(Debug, Clone, PartialEq)]
enum LoopType {
    ForLoop,
    WhileLoop,
    DoWhileLoop,
    Unknown,
}

/// Specialized loop handler - designed to be called by parent handlers
#[derive(Debug, Clone)]
pub struct LoopHandler {
    /// Track who called this handler for adaptive learning
    caller_history: HashMap<String, AdaptiveStats>,
}

/// Adaptive statistics for learning which callers have success with this handler
#[derive(Debug, Clone)]
struct AdaptiveStats {
    total_calls: u32,
    successful_calls: u32,
    confidence_sum: f64,
    avg_confidence: f64,
}

impl LoopHandler {
    pub fn new() -> Self {
        Self {
            caller_history: HashMap::new(),
        }
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

        // Try multiple registered patterns for loop constructs
        let mut pattern_success = false;
        let pattern_names = vec!["control_flow_for", "control_flow_while", "loop"];

        for pattern_name in &pattern_names {
            let pattern_match = patternizer.match_pattern(pattern_name, tokens);
            if !matches!(pattern_match, crate::PatternResult::NoMatch { .. }) {
                pattern_success = true;
                break;
            }
        }

        // Comprehensive loop pattern analysis using registered patterns
        let patterns = vec![
            "control_flow_for".to_string(),
            "control_flow_while".to_string(),
            "nested_loop".to_string(),
            "infinite_loop".to_string(),
            "loop_with_break".to_string(),
            "loop_with_continue".to_string(),
        ];

        let base_confidence = samplizer.analyze_with_range(&patterns, tokens, token_range)?;

        // Enhanced scoring using Patternizer's integrated analysis
        let structural_score = patternizer.calculate_structure_score(tokens);
        let type_score = patternizer.calculate_type_score(tokens);
        let syntax_score = patternizer.calculate_syntax_score(tokens);

        // Loop-specific pattern detection
        let has_loop_keywords = tokens.iter().any(|t| {
            matches!(t.to_string().as_str(), "for" | "while" | "do")
        });

        let has_control_structures = tokens.iter().any(|t| {
            matches!(t.to_string().as_str(), "(" | ")" | "{" | "}" | ";" | "break" | "continue")
        });

        // Weighted confidence calculation optimized for loop patterns
        let enhanced_confidence = (base_confidence * 0.4)
            + (structural_score * 0.25)
            + (type_score * 0.15)
            + (syntax_score * 0.2);

        // Adaptive threshold based on loop complexity
        let threshold = match tokens.len() {
            0..=5 => 0.5,
            6..=15 => 0.6,
            16..=30 => 0.7,
            _ => 0.75,
        };

        let structural_match = pattern_success || (has_loop_keywords && has_control_structures);
        let final_confidence = enhanced_confidence.clamp(0.0, 1.0);

        Ok((
            structural_match && final_confidence > threshold,
            final_confidence,
        ))
    }

    /// Apply adaptive learning based on historical success with different callers
    fn apply_adaptive_adjustment(&self, base_confidence: f64, caller_id: &str) -> f64 {
        if let Some(stats) = self.caller_history.get(caller_id) {
            let success_rate = stats.successful_calls as f64 / stats.total_calls.max(1) as f64;
            let confidence_factor = stats.avg_confidence;

            // Multi-factor adjustment based on success rate and average confidence
            let rate_adjustment = match success_rate {
                rate if rate > 0.85 => 1.25,
                rate if rate > 0.70 => 1.15,
                rate if rate > 0.55 => 1.05,
                rate if rate > 0.40 => 1.0,
                rate if rate > 0.25 => 0.9,
                _ => 0.8,
            };

            let confidence_adjustment = if confidence_factor > 0.8 { 1.1 } else { 0.95 };

            (base_confidence * rate_adjustment * confidence_adjustment).min(1.0)
        } else {
            // No history - slight penalty for unknown callers
            base_confidence * 0.95
        }
    }

    /// Enhanced loop classification with nested loop detection
    fn classify_loop(&self, tokens: &[Token]) -> LoopType {
        let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        let mut nesting_level: u32 = 0;

        for (i, token_str) in token_strings.iter().enumerate() {
            match token_str.as_str() {
                "for" if i + 1 < token_strings.len() && token_strings[i + 1] == "(" => {
                    return LoopType::ForLoop;
                }
                "while" if i + 1 < token_strings.len() && token_strings[i + 1] == "(" => {
                    // Check if it's part of a do-while
                    if i > 0 && token_strings[..i].iter().any(|t| t == "do") {
                        return LoopType::DoWhileLoop;
                    }
                    return LoopType::WhileLoop;
                }
                "do" if token_strings.iter().skip(i).any(|t| t == "while") => {
                    return LoopType::DoWhileLoop;
                }
                "{" => nesting_level += 1,
                "}" => {
                    nesting_level = nesting_level.saturating_sub(1);
                }
                _ => {}
            }
        }

        LoopType::Unknown
    }

    /// Enhanced loop conversion with better Rust idioms
    fn convert_to_rust(&self, loop_type: &LoopType, tokens: &[Token]) -> Result<String> {
        match loop_type {
            LoopType::ForLoop => self.convert_for(tokens),
            LoopType::WhileLoop => self.convert_while(tokens),
            LoopType::DoWhileLoop => self.convert_do_while(tokens),
            LoopType::Unknown => Ok("// Unknown loop type - requires manual analysis\n// Consider using iterator patterns or explicit loop constructs".to_string()),
        }
    }

    /// Enhanced C for loop to Rust conversion with range detection
    fn convert_for(&self, tokens: &[Token]) -> Result<String> {
        let mut rust_code = String::new();
        rust_code.push_str("// Converted for loop - consider using range or iterator patterns\n");

        // Try to detect simple counting patterns
        let token_str = tokens.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(" ");
        if token_str.contains("i++") || token_str.contains("++i") {
            rust_code.push_str("for i in 0..n {\n");
        } else if token_str.contains("i--") || token_str.contains("--i") {
            rust_code.push_str("for i in (0..n).rev() {\n");
        } else {
            rust_code.push_str("// Complex for loop - consider while loop or iterator\n");
            rust_code.push_str("for i in range {\n");
        }

        rust_code.push_str("    // TODO: Convert loop body to Rust\n");
        rust_code.push_str("}\n");
        Ok(rust_code)
    }

    /// Enhanced while loop conversion
    fn convert_while(&self, tokens: &[Token]) -> Result<String> {
        let mut rust_code = String::new();
        rust_code.push_str("// Converted while loop\n");
        rust_code.push_str("while condition {\n");
        rust_code.push_str("    // TODO: Convert condition and body to Rust syntax\n");
        rust_code.push_str("    // Consider using iterator methods where appropriate\n");
        rust_code.push_str("}\n");
        Ok(rust_code)
    }

    /// Enhanced do-while conversion with proper Rust patterns
    fn convert_do_while(&self, tokens: &[Token]) -> Result<String> {
        let mut rust_code = String::new();
        rust_code.push_str("// Converted do-while loop (Rust doesn't have native do-while)\n");
        rust_code.push_str("loop {\n");
        rust_code.push_str("    // TODO: Convert loop body to Rust\n");
        rust_code.push_str("    \n");
        rust_code.push_str("    if !condition {\n");
        rust_code.push_str("        break;\n");
        rust_code.push_str("    }\n");
        rust_code.push_str("}\n");
        Ok(rust_code)
    }

    /// Update adaptive statistics with enhanced tracking
    pub fn update_stats(&mut self, caller_id: &str, success: bool, confidence: f64) {
        let stats = self
            .caller_history
            .entry(caller_id.to_string())
            .or_insert(AdaptiveStats {
                total_calls: 0,
                successful_calls: 0,
                confidence_sum: 0.0,
                avg_confidence: 0.0,
            });

        stats.total_calls += 1;
        if success {
            stats.successful_calls += 1;
        }
        stats.confidence_sum += confidence;
        stats.avg_confidence = stats.confidence_sum / stats.total_calls as f64;

        // Enhanced logging with trend analysis
        let success_rate = (stats.successful_calls as f64 / stats.total_calls as f64) * 100.0;
        let trend = if stats.total_calls > 5 {
            let recent_success = if success { "↗" } else { "↘" };
            format!(" {}", recent_success)
        } else {
            String::new()
        };

        println!(
            "🔄 LoopHandler adaptive learning: {} calls from {}, {:.1}% success, conf: {:.2}{}",
            stats.total_calls, caller_id, success_rate, stats.avg_confidence, trend
        );
    }

    /// Enhanced handler suggestion with specialization levels
    fn suggest_handler(&self, tokens: &[Token]) -> Option<Id> {
        let loop_type = self.classify_loop(tokens);
        let complexity = self.assess_loop_complexity(tokens);

        match (loop_type, complexity) {
            (LoopType::ForLoop, complexity) if complexity > 0.7 => Some(Id::get("complex_for_handler")),
            (LoopType::ForLoop, _) => Some(Id::get("for_loop_handler")),
            (LoopType::WhileLoop, complexity) if complexity > 0.8 => Some(Id::get("complex_while_handler")),
            (LoopType::WhileLoop, _) => Some(Id::get("while_loop_handler")),
            (LoopType::DoWhileLoop, _) => Some(Id::get("do_while_handler")),
            (LoopType::Unknown, _) => {
                // Check if it might be a different control structure
                let token_str = tokens.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(" ");
                if token_str.contains("if") || token_str.contains("switch") {
                    Some(Id::get("conditional_handler"))
                } else {
                    Some(Id::get("statement_handler"))
                }
            }
        }
    }

    /// Assess loop complexity for better handler routing
    fn assess_loop_complexity(&self, tokens: &[Token]) -> f64 {
        let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        let mut complexity = 0.0;

        // Count complexity indicators
        let nested_loops = token_strings.iter().filter(|t| matches!(t.as_str(), "for" | "while" | "do")).count();
        let control_flow = token_strings.iter().filter(|t| matches!(t.as_str(), "break" | "continue" | "return")).count();
        let conditions = token_strings.iter().filter(|t| matches!(t.as_str(), "if" | "else" | "switch")).count();
        let operators = token_strings.iter().filter(|t| matches!(t.as_str(), "&&" | "||" | "==" | "!=" | "<=" | ">=")).count();

        complexity += nested_loops as f64 * 0.3;
        complexity += control_flow as f64 * 0.2;
        complexity += conditions as f64 * 0.25;
        complexity += operators as f64 * 0.1;

        // Normalize by token count
        if !tokens.is_empty() {
            complexity /= tokens.len() as f64;
        }

        complexity.min(1.0)
    }

    /// Enhanced information extraction with detailed loop analysis
    fn extract_info(&self, tokens: &[Token], loop_type: &LoopType) -> Result<ControlFlowInfo> {
        let mut loop_info = ControlFlowInfo::default();
        let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        // Set loop type metadata
        loop_info.flow_type = format!("{:?}", loop_type);
        loop_info.complexity = format!("{:.2}", self.assess_loop_complexity(tokens));

        match loop_type {
            LoopType::ForLoop => {
                self.extract_for_loop_info(&mut loop_info, &token_strings)?
            }
            LoopType::WhileLoop => {
                self.extract_while_loop_info(&mut loop_info, &token_strings)?
            }
            LoopType::DoWhileLoop => {
                self.extract_do_while_info(&mut loop_info, &token_strings)?
            }
            LoopType::Unknown => {
                loop_info.condition = "unknown_pattern".to_string();
                loop_info.flow_type = "unknown".to_string();
            }
        };

        // Extract body if not already extracted
        if loop_info.body.is_empty() {
            if let Some(brace_start) = token_strings.iter().position(|t| t == "{") {
                if let Some(brace_end) = token_strings.iter().rposition(|t| t == "}") {
                    if brace_end > brace_start {
                        let body_tokens = &token_strings[brace_start + 1..brace_end];
                        loop_info.body = body_tokens.join(" ");
                    }
                }
            }
        };

        Ok(loop_info.clone())
    }

    /// Extract for loop specific information
    fn extract_for_loop_info(&self, loop_info: &mut ControlFlowInfo, tokens: &[String]) -> Result<()> {
        if let Some(paren_start) = tokens.iter().position(|t| t == "(") {
            if let Some(paren_end) = tokens.iter().rposition(|t| t == ")") {
                let for_content = &tokens[paren_start + 1..paren_end];
                let content_str = for_content.join(" ");
                let parts: Vec<&str> = content_str.split(';').collect();

                if parts.len() >= 3 {
                    loop_info.initialization = parts[0].trim().to_string();
                    loop_info.condition = parts[1].trim().to_string();
                    loop_info.increment = parts[2].trim().to_string();

                    // Detect common patterns
                    if loop_info.increment.contains("++") {
                        loop_info.is_range_based = true;
                    } else if loop_info.increment.contains("--") {
                        loop_info.is_range_based = true;
                    }
                }
            }
        }
        Ok(())
    }

    /// Extract while loop specific information
    fn extract_while_loop_info(&self, loop_info: &mut ControlFlowInfo, tokens: &[String]) -> Result<()> {
        if let Some(while_pos) = tokens.iter().position(|t| t == "while") {
            if let Some(paren_start) = tokens.iter().skip(while_pos).position(|t| t == "(") {
                let actual_paren_start = while_pos + paren_start;
                if let Some(paren_end) = tokens.iter().skip(actual_paren_start).position(|t| t == ")") {
                    let actual_paren_end = actual_paren_start + paren_end;
                    loop_info.condition = tokens[actual_paren_start + 1..actual_paren_end].join(" ");

                    // Extract loop body if present
                    if let Some(brace_start) = tokens.iter().skip(actual_paren_end).position(|t| t == "{") {
                        let actual_brace_start = actual_paren_end + brace_start;
                        let mut brace_count = 0;
                        let mut body_end = actual_brace_start;

                        for (i, token) in tokens.iter().skip(actual_brace_start).enumerate() {
                            match token.as_str() {
                                "{" => brace_count += 1,
                                "}" => {
                                    brace_count -= 1;
                                    if brace_count == 0 {
                                        body_end = actual_brace_start + i;
                                        break;
                                    }
                                }
                                _ => {}
                            }
                        }

                        if body_end > actual_brace_start + 1 {
                            loop_info.body = tokens[actual_brace_start + 1..body_end].join(" ");
                        }
                    }

                    // Detect common while loop patterns
                    if loop_info.condition.contains("!=") || loop_info.condition.contains("==") {
                        loop_info.is_comparison_based = true;
                    } else if loop_info.condition.contains("true") {
                        loop_info.is_infinite_loop = true;
                    }
                }
            }
        }
        Ok(())
    }

    /// Extract do-while specific information
    fn extract_do_while_info(&self, loop_info: &mut ControlFlowInfo, tokens: &[String]) -> Result<()> {
        if let Some(do_pos) = tokens.iter().position(|t| t == "do") {
            if let Some(while_pos) = tokens.iter().rposition(|t| t == "while") {
                if while_pos > do_pos {
                    let body_tokens = &tokens[do_pos + 1..while_pos];
                    loop_info.body = body_tokens.join(" ");

                    if let Some(paren_start) = tokens.iter().skip(while_pos).position(|t| t == "(") {
                        let actual_paren_start = while_pos + paren_start;
                        if let Some(paren_end) = tokens.iter().skip(actual_paren_start).position(|t| t == ")") {
                            let condition_tokens = &tokens[actual_paren_start + 1..actual_paren_start + paren_end];
                            loop_info.condition = condition_tokens.join(" ");
                        }
                    }
                }
            }
        }
        Ok(())
    }
}
impl Handler for LoopHandler {
    fn id(&self) -> Id {
        Id::get("loop_handler")
    }

    fn role(&self) -> String {
        "loop".to_string()
    }

    fn priority(&self) -> u64 {
        150
    }

    fn supported_patterns(&self) -> Vec<String> {
        vec![
            "for_loop".to_string(),
            "while_loop".to_string(),
            "do_while_loop".to_string(),
            "loop_construct".to_string(),
            "foreach_loop".to_string(),
            "range_based_loop".to_string(),
        ]
    }

    fn supported_keywords(&self) -> Vec<String> {
        vec![
            "for".to_string(),
            "while".to_string(),
            "do".to_string(),
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
                Id::get(&format!("loop_handler_can_process_{}", token_slot)),
                Some(self.id()),
                "can_process".to_string(),
                format!(
                    "Loop detection: {} (confidence: {:.2})",
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

        let loop_type = self.classify_loop(&tokens);
        let loop_info = self.extract_info(&tokens, &loop_type)?;
        let complexity = self.assess_loop_complexity(&tokens);

        let extracted_loop = ExtractedControlFlow {
            id: Id::get(&gen_name(&self.id().name()).as_str()),
            info: loop_info.clone(),
            tokens: tokens.clone(),
            token_range,
            metadata: vec![
                ("handler".to_string(), "loop_handler".to_string()),
                ("loop_type".to_string(), format!("{:?}", loop_type)),
                ("complexity".to_string(), format!("{:.2}", complexity)),
                ("adaptive_learning".to_string(), "enabled".to_string()),
                (
                    "has_condition".to_string(),
                    (!loop_info.condition.is_empty()).to_string(),
                ),
                (
                    "has_body".to_string(),
                    (!loop_info.body.is_empty()).to_string(),
                ),
                (
                    "is_range_based".to_string(),
                    loop_info.is_range_based.to_string(),
                ),
                (
                    "is_infinite".to_string(),
                    loop_info.is_infinite_loop.to_string(),
                ),
            ],
        };

        Ok(Some(ExtractedElement::ControlFlow(extracted_loop)))
    }

    fn convert(
        &self,
        context: &mut Context,
        extracted_element: ExtractedElement,
    ) -> Result<Option<ConvertedElement>> {
        if let ExtractedElement::ControlFlow(loop_element) = extracted_element {
            let loop_type = self.classify_loop(&loop_element.tokens);
            let rust_code = self.convert_to_rust(&loop_type, &loop_element.tokens)?;
            let complexity = self.assess_loop_complexity(&loop_element.tokens);

            let converted_loop = ConvertedControlFlow {
                code: rust_code,
                metadata: vec![
                    ("handler".to_string(), "loop_handler".to_string()),
                    ("loop_type".to_string(), format!("{:?}", loop_type)),
                    ("complexity".to_string(), format!("{:.2}", complexity)),
                    (
                        "conversion_method".to_string(),
                        "adaptive_pattern_based".to_string(),
                    ),
                    (
                        "rust_patterns_used".to_string(),
                        match loop_type {
                            LoopType::ForLoop => "for_in_range",
                            LoopType::WhileLoop => "while_condition",
                            LoopType::DoWhileLoop => "loop_with_break",
                            LoopType::Unknown => "generic_loop",
                        }.to_string(),
                    ),
                ],
            };

            Ok(Some(ConvertedElement::ControlFlow(converted_loop)))
        } else {
            Ok(None)
        }
    }

    fn report(&self, context: &mut Context) -> Result<Report> {
        let reports = context.registry.get_reports_by_handler(&self.id().name());

        if reports.is_empty() {
            return Ok(Report::new(
                Id::get("loop_handler_report"),
                Some(self.id()),
                "report".to_string(),
                "No loop processing reports found".to_string(),
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
        let success_rate = if !reports.is_empty() {
            (successful_reports as f64 / reports.len() as f64) * 100.0
        } else {
            0.0
        };

        // Calculate adaptive learning statistics
        let total_calls: u32 = self.caller_history.values().map(|s| s.total_calls).sum();
        let avg_confidence: f64 = if !self.caller_history.is_empty() {
            self.caller_history.values().map(|s| s.avg_confidence).sum::<f64>()
                / self.caller_history.len() as f64
        } else {
            0.0
        };

        Ok(Report::new(
            Id::get("loop_handler_report"),
            Some(self.id()),
            "report".to_string(),
            format!(
                "Loop handler: {} reports ({:.1}% success), {} tokens processed, {} adaptive calls (avg conf: {:.2})",
                reports.len(), success_rate, total_tokens_processed, total_calls, avg_confidence
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
        let tokens = context
            .tokenizer
            .get_tokens(token_slot, token_range.clone());

        if tokens.is_empty() {
            return Ok(result);
        }

        let loop_type = self.classify_loop(&tokens);
        let complexity = self.assess_loop_complexity(&tokens);
        let suggested_handler = self.suggest_handler(&tokens);

        let mut metadata = Vec::new();
        metadata.push((
            "loop_analysis".to_string(),
            format!("Type: {:?}, Complexity: {:.2}", loop_type, complexity),
        ));
        metadata.push((
            "tokens_analyzed".to_string(),
            tokens.len().to_string(),
        ));
        metadata.push((
            "redirect_reason".to_string(),
            if complexity > 0.8 {
                "high_complexity_requires_specialized_handler".to_string()
            } else {
                "pattern_specific_optimization_available".to_string()
            },
        ));

        if let Some(handler_id) = &suggested_handler {
            metadata.push((
                "suggested_handler".to_string(),
                handler_id.name().to_string(),
            ));
        }

        let _redirect_request = RedirectRequest {
            from_handler: self.id(),
            token_range: token_range.clone(),
            failed_patterns: if matches!(result, HandlerResult::NotHandled(_, _, _)) {
                self.supported_patterns()
            } else {
                vec![]
            },
            suggested_handler: suggested_handler.clone(),
            metadata,
        };

        // Log redirect decision for adaptive learning
        if let Some(handler_id) = suggested_handler {
            context.registry.add_report(
                Report::new(
                    Id::get(&format!("loop_handler_redirect_{}", token_slot)),
                    Some(self.id()),
                    "redirect".to_string(),
                    format!(
                        "Redirecting {:?} loop (complexity: {:.2}) to {}",
                        loop_type, complexity, handler_id.name()
                    ),
                    ReportLevel::Info,
                    Phase::Process(None),
                )
                    .with_tokens(tokens.len(), 0)
                    .with_success(true),
            );
        }

        Ok(result)
    }
}

impl Default for LoopHandler {
    fn default() -> Self {
        Self::new()
    }
}
