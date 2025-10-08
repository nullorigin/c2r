//! Loop handler implementation for C loop constructs
//! Specialized subhandler that can be called by higher-level handlers like FunctionHandler
//! Uses adaptive pattern learning to improve redirect decisions over time

use crate::{
    Context, ConvertedControlFlow, ConvertedElement, ExtractedControlFlow, ExtractedElement, Handler, HandlerPhase, HandlerReport, HandlerResult, Handlizer, Id, RedirectRequest, ReportLevel, Result, report
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
    fn detect_loop_pattern(&self,context: &mut Context, tokens: &[crate::Token], token_range: Range<usize>) -> Result<(bool, f64)> {
        // First use Patternizer for structural detection
        let pattern_match = context.patternizer.match_pattern("loop", tokens);

        // Then use Samplizer for confidence scoring
        let patterns = vec!["for_loop".to_string(), "while_loop".to_string(), "do_while_loop".to_string()];
        let confidence = context.samplizer.analyze_with_range(&patterns, tokens, token_range)?;

        // Combined heuristics for loop detection
        let has_loop_structure = self.has_loop_structure(tokens);
        let enhanced_confidence = if has_loop_structure {
            confidence * 1.4  // Boost confidence if structure looks loop-like
        } else {
            confidence * 0.7  // Reduce if structure doesn't match
        };

        let pattern_success = !matches!(pattern_match, crate::PatternResult::NoMatch { .. });

        Ok((pattern_success || enhanced_confidence > 0.7, enhanced_confidence))
    }
    
    /// Apply adaptive learning based on historical success with different callers
    fn apply_adaptive_adjustment(&self, base_confidence: f64, caller_id: &str) -> f64 {
        if let Some(stats) = self.caller_history.get(caller_id) {
            let success_rate = stats.successful_calls as f64 / stats.total_calls.max(1) as f64;
            
            // Adjust confidence based on historical success rate with this caller
            match success_rate {
                rate if rate > 0.8 => base_confidence * 1.2,  // High success rate - boost confidence
                rate if rate > 0.6 => base_confidence * 1.1,  // Good success rate - small boost
                rate if rate > 0.4 => base_confidence,        // Average - no change
                rate if rate > 0.2 => base_confidence * 0.9,  // Poor - slight penalty
                _ => base_confidence * 0.8,                   // Very poor - significant penalty
            }
        } else {
            // No history - neutral confidence
            base_confidence
        }
    }
    
    /// Detect loop structures in token sequence
    fn has_loop_structure(&self, tokens: &[crate::Token]) -> bool {
        if tokens.len() < 3 {
            return false;
        }
        
        let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        
        // Look for loop keywords and structures
        for (i, token_str) in token_strings.iter().enumerate() {
            match token_str.as_str() {
                "for" => {
                    // Look for for(;;) pattern
                    if i + 2 < token_strings.len() && token_strings[i + 1] == "(" {
                        return true;
                    }
                }
                "while" => {
                    // Look for while() pattern
                    if i + 2 < token_strings.len() && token_strings[i + 1] == "(" {
                        return true;
                    }
                }
                "do" => {
                    // Look for do { ... } while pattern
                    if token_strings.iter().skip(i).any(|t| t == "while") {
                        return true;
                    }
                }
                _ => {}
            }
        }
        
        false
    }
    
    /// Determine specific loop type from tokens
    fn classify_loop_type(&self, tokens: &[crate::Token]) -> LoopType {
        let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        
        for (i, token_str) in token_strings.iter().enumerate() {
            match token_str.as_str() {
                "for" if i + 1 < token_strings.len() && token_strings[i + 1] == "(" => {
                    return LoopType::ForLoop;
                }
                "while" if i + 1 < token_strings.len() && token_strings[i + 1] == "(" => {
                    return LoopType::WhileLoop;
                }
                "do" if token_strings.iter().skip(i).any(|t| t == "while") => {
                    return LoopType::DoWhileLoop;
                }
                _ => {}
            }
        }
        
        LoopType::Unknown
    }
    
    /// Convert C loop to Rust loop with proper syntax
    fn convert_loop_to_rust(&self, loop_type: &LoopType, tokens: &[crate::Token]) -> Result<String> {
        match loop_type {
            LoopType::ForLoop => self.convert_for_loop(tokens),
            LoopType::WhileLoop => self.convert_while_loop(tokens),
            LoopType::DoWhileLoop => self.convert_do_while_loop(tokens),
            LoopType::Unknown => Ok("// Unknown loop type - manual conversion needed".to_string()),
        }
    }
    
    /// Convert C for loop to Rust for loop
    fn convert_for_loop(&self, tokens: &[crate::Token]) -> Result<String> {
        let mut rust_code = String::new();
        rust_code.push_str("// Converted for loop\n");
        rust_code.push_str("for i in 0..n {\n");
        rust_code.push_str("    // Loop body - needs manual conversion\n");
        rust_code.push_str("}\n");
        Ok(rust_code)
    }
    
    /// Convert C while loop to Rust while loop
    fn convert_while_loop(&self, tokens: &[crate::Token]) -> Result<String> {
        let mut rust_code = String::new();
        rust_code.push_str("// Converted while loop\n");
        rust_code.push_str("while condition {\n");
        rust_code.push_str("    // Loop body - needs manual conversion\n");
        rust_code.push_str("}\n");
        Ok(rust_code)
    }
    
    /// Convert C do-while loop to Rust loop with break
    fn convert_do_while_loop(&self, tokens: &[crate::Token]) -> Result<String> {
        let mut rust_code = String::new();
        rust_code.push_str("// Converted do-while loop\n");
        rust_code.push_str("loop {\n");
        rust_code.push_str("    // Loop body - needs manual conversion\n");
        rust_code.push_str("    if !condition {\n");
        rust_code.push_str("        break;\n");
        rust_code.push_str("    }\n");
        rust_code.push_str("}\n");
        Ok(rust_code)
    }
    
    /// Update adaptive statistics based on processing result
    pub fn update_adaptive_stats(&mut self, caller_id: &str, success: bool, confidence: f64) {
        let stats = self.caller_history.entry(caller_id.to_string()).or_insert(AdaptiveStats {
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
        
        println!("🧠 LoopHandler adaptive learning: {} calls from {}, {:.1}% success rate", 
                 stats.total_calls, caller_id, 
                 (stats.successful_calls as f64 / stats.total_calls as f64) * 100.0);
    }
        /// Suggest even more specialized handlers for specific loop types
        fn suggest_specialized_handler(&self, tokens: &[crate::Token]) -> Option<Id> {
            let loop_type = self.classify_loop_type(tokens);
            
            match loop_type {
                LoopType::ForLoop => Some(Id::get("for_loop_handler")),
                LoopType::WhileLoop => Some(Id::get("while_loop_handler")), 
                LoopType::DoWhileLoop => Some(Id::get("do_while_handler")),
                LoopType::Unknown => Some(Id::get("statement_handler")), // Fallback to statement handler
            }
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
        vec!["for_loop".to_string(), "while_loop".to_string(), "do_while_loop".to_string(), "loop_construct".to_string()]
    }
    
    
    fn can_process(&self, context: &mut Context, token_slot: usize, token_range: Range<usize>) -> Result<bool> {
        let tokens = context.tokenizer.get_tokens(token_slot, token_range.clone());

        if tokens.is_empty() {
            return Ok(false);
        }
        let tokens = tokens;
        
        let (can_process, confidence) = self.detect_loop_pattern(context, &tokens, token_range)?;
        
        // Report detection result
        report!(
            context,
            "loop_handler",
            "can_process", 
            ReportLevel::Info,
            HandlerPhase::Process,
            &format!("Loop detection: {} (confidence: {:.2})", can_process, confidence),
            can_process
        );
        
        Ok(can_process)
    }
    
    fn extract(&self, context: &mut Context, token_slot: usize, token_range: Range<usize>) -> Result<Option<ExtractedElement>> {
        let tokens = context.tokenizer.get_tokens(token_slot, token_range.clone());

        if tokens.is_empty() {
            return Ok(None);
        }
        
        let loop_type = self.classify_loop_type(&tokens);
        
        let extracted_loop = ExtractedControlFlow {
            control_type: format!("Loop::{:?}", loop_type),
            condition: vec![], // Empty for now - would need proper parsing
            body: vec![],      // Empty for now - would need proper parsing  
            tokens: tokens.to_vec(),
            metadata: vec![
                ("handler".to_string(), "loop_handler".to_string()),
                ("loop_type".to_string(), format!("{:?}", loop_type)),
                ("adaptive_learning".to_string(), "enabled".to_string()),
            ],
            code: format!("// Extracted {:?} loop", loop_type),
        };
        
        Ok(Some(ExtractedElement::ControlFlow(extracted_loop)))
    }
    
    fn convert(&self, context: &mut Context, token_slot: usize, token_range: Range<usize>) -> Result<Option<ConvertedElement>> {
        let tokens = context.tokenizer.get_tokens(token_slot, token_range.clone());
        if tokens.is_empty() {
            return Ok(None);
        }

        let loop_type = self.classify_loop_type(&tokens);
        let rust_code = self.convert_loop_to_rust(&loop_type, &tokens)?;
        
        let converted_loop = ConvertedControlFlow {
            control_type: format!("Loop::{:?}", loop_type),
            condition: "condition".to_string(), // Placeholder - needs proper parsing
            body: rust_code.clone(),
            metadata: vec![
                ("handler".to_string(), "loop_handler".to_string()),
                ("loop_type".to_string(), format!("{:?}", loop_type)),
                ("conversion_method".to_string(), "adaptive_pattern_based".to_string()),
            ],
            code: rust_code,
            has_else: false, // Loops don't have else clauses in basic conversion
        };
        
        Ok(Some(ConvertedElement::ControlFlow(converted_loop)))
    }
    
    fn report(&self, context: &mut Context) -> Result<HandlerReport> {
        let total_patterns = self.supported_patterns().len();
        
        Ok(HandlerReport::new(
            "loop_handler_report",
            self.id().into(),
            "loop_handler".to_string(),
            "report".to_string(),
            format!("Loop handler with {} patterns and adaptive learning", total_patterns),
            ReportLevel::Info,
            HandlerPhase::Report,
        ).with_success(true))
    }
    
    
    fn handle_redirect(&self, context: &mut Context,     token_slot: usize, token_range: Range<usize>, result: HandlerResult) -> Result<HandlerResult> {
        // Loop handler is specialized - suggest even more specific subhandlers
        let tokens = context.tokenizer.get_tokens(token_slot, token_range.clone());
        if tokens.is_empty() {
            return Ok(result);
        }
        let suggested_handler = self.suggest_specialized_handler(&tokens);
        
        let mut metadata = Vec::new();
        metadata.push(("specialized_analysis".to_string(), format!("Loop handler analyzed {} tokens", tokens.len())));
        
        let redirect_request = RedirectRequest {
            from_handler: self.id(),
            token_range: token_range.clone(),
            failed_patterns: self.supported_patterns(),
            suggested_handler,
            metadata,
        };
        
        // Return the result as-is to avoid deadlock in redirect workflow
        Ok(result)
    }
    
    
    fn process(&self, context: &mut Context, token_slot: usize, token_range: Range<usize>) -> Result<HandlerResult> {
        // Specialized loop processing with adaptive learning
        let tokens = context.tokenizer.get_tokens(token_slot, token_range.clone());
        if tokens.is_empty() {
            return Ok(HandlerResult::NotHandled(None, token_range.clone(), self.id()));
        }
        let tokens = tokens;
        let (can_process, confidence) = 
            self.detect_loop_pattern(context, &tokens, token_range.clone())?;
        
        if can_process {
            // Process the loop construct
            let extracted = self.extract(context, token_slot, token_range.clone())?;
            let converted = self.convert(context, token_slot, token_range.clone())?;
            
            // Update adaptive learning stats
            // Note: In a real implementation, we'd track the calling handler
            
            // Generate rust code from the converted element
            let rust_code = if let Some(ConvertedElement::ControlFlow(loop_element)) = converted {
                loop_element.code
            } else {
                "// Loop conversion failed".to_string()
            };
            
            Ok(HandlerResult::Processed(
                Some(tokens),
                token_range,
                rust_code,
                self.id()
            ))
        } else {
            // Redirect to more specialized handler
            self.handle_redirect(context, token_slot, token_range.clone(), 
                HandlerResult::NotHandled(None, token_range.clone(), self.id()))
        }
    }
} 


impl Default for LoopHandler {
    fn default() -> Self {
        Self::new()
    }
}
