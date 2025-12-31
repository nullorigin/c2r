//! Handler Processing Pipeline
//!
//! Defines the core processing pipeline for handlers:
//! - `ProcessStage`: Current stage in the pipeline
//! - `ProcessDecision`: Decision returned by process() to indicate next action
//! - `Processor`: Trait defining the handler processing pipeline
//! - `ProcessingContext`: Manages handler stack for nested processing

use std::collections::HashMap;
use std::collections::btree_set::Union;
use std::path::Prefix;
use std::process::Output;

use crate::handlers::arrays::ArrayHandler;
use crate::db::handler::{FailureRecord, SuccessRecord};
use crate::db::{Entry, PatternRule, RuleType, is_c_type_keyword, is_valid_c_identifier};
use crate::db::pattern::{ExtractContext, Pattern, REPEAT_NONE};
use crate::db::token::Token;
use crate::db::web::Build;
use crate::handlers::expressions::ExpressionHandler;
use crate::handlers::functions::CallHandler;
use crate::system;

// ============================================================================
// Processing Stage Enum
// ============================================================================

/// Current stage in the processing pipeline
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProcessorStage {
    /// Initial state - not yet validated
    Pending,
    /// Validation passed
    Validated,
    /// Data extracted from tokens
    Extracted,
    /// Conversion to Rust completed
    Converted,
    /// Routed to another handler
    Routed,
    /// Processing complete
    Complete,
    /// Processing failed at some stage
    Failed,
}

impl ProcessorStage {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Pending => "pending",
            Self::Validated => "validated",
            Self::Extracted => "extracted",
            Self::Converted => "converted",
            Self::Routed => "routed",
            Self::Complete => "complete",
            Self::Failed => "failed",
        }
    }

    pub fn from_str(s: &str) -> Self {
        match s {
            "validated" => Self::Validated,
            "extracted" => Self::Extracted,
            "converted" => Self::Converted,
            "routed" => Self::Routed,
            "complete" => Self::Complete,
            "failed" => Self::Failed,
            _ => Self::Pending,
        }
    }
}

impl std::fmt::Display for ProcessorStage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}
impl Build for ProcessorStage {
    fn to_entry(&self) -> Entry {
        Entry::string(self.as_str())
    }
    fn kind(&self) -> &str { "ProcessorStage" }
    fn name(&self) -> Option<&str> { Some(self.as_str()) }
    fn category(&self) -> Option<&str> { Some("processing") }
}
// ============================================================================
// Process Decision - Returned by process() to indicate next action
// ============================================================================

/// Decision returned by process() indicating what happened and what to do next
#[derive(Debug, Clone)]
pub enum ProcessorDecision {
    /// Continue to next stage
    Continue { stage: ProcessorStage, reason: String },
    /// Processing is complete - store results
    Complete { rust_code: String, confidence: f64 },
    /// Route to another handler (replaces current handler)
    Route {
        target_handler: String,
        reason: String,
    },
    /// Route to nested handler, then return to current handler
    /// Used when extract() finds a nested construct (e.g., array inside function)
    RouteNested {
        /// Handler to process the nested construct
        target_handler: String,
        /// Token range for the nested construct (start..end indices)
        token_range: std::ops::Range<usize>,
        /// Reason for nesting
        reason: String,
    },
    /// Nested handler completed, return result to parent
    NestedComplete {
        /// Rust code from nested handler
        rust_code: String,
        /// Confidence from nested handler
        confidence: f64,
        /// Token range that was processed
        token_range: std::ops::Range<usize>,
    },
    /// Cannot proceed - need more information or tokens
    Pause { stage: ProcessorStage, reason: String },
    /// Processing failed
    Fail { stage: ProcessorStage, reason: String },
}

impl Build for ProcessorDecision {
    fn to_entry(&self) -> Entry {
        match self {
            Self::Continue { stage, reason } => {
                let mut entry = Entry::node("ProcessorDecision", "Continue");
                entry.set_attr("stage", stage.to_entry());
                entry.set_attr("reason", Entry::string(reason));
                entry
            }
            Self::Complete { rust_code, confidence } => {
                let mut entry = Entry::node("ProcessorDecision", "Complete");
                entry.set_attr("rust_code", Entry::string(rust_code));
                entry.set_attr("confidence", Entry::f64(*confidence));
                entry
            }
            Self::Route { target_handler, reason } => {
                let mut entry = Entry::node("ProcessorDecision", "Route");
                entry.set_attr("target_handler", Entry::string(target_handler));
                entry.set_attr("reason", Entry::string(reason));
                entry
            }
            Self::RouteNested { target_handler, token_range, reason } => {
                let mut entry = Entry::node("ProcessorDecision", "RouteNested");
                entry.set_attr("target_handler", Entry::string(target_handler));
                entry.set_attr("token_range_start", Entry::usize(token_range.start));
                entry.set_attr("token_range_end", Entry::usize(token_range.end));
                entry.set_attr("reason", Entry::string(reason));
                entry
            }
            Self::NestedComplete { rust_code, confidence, token_range } => {
                let mut entry = Entry::node("ProcessorDecision", "NestedComplete");
                entry.set_attr("rust_code", Entry::string(rust_code));
                entry.set_attr("confidence", Entry::f64(*confidence));
                entry.set_attr("token_range_start", Entry::usize(token_range.start));
                entry.set_attr("token_range_end", Entry::usize(token_range.end));
                entry
            }
            Self::Pause { stage, reason } => {
                let mut entry = Entry::node("ProcessorDecision", "Pause");
                entry.set_attr("stage", stage.to_entry());
                entry.set_attr("reason", Entry::string(reason));
                entry
            }
            Self::Fail { stage, reason } => {
                let mut entry = Entry::node("ProcessorDecision", "Fail");
                entry.set_attr("stage", stage.to_entry());
                entry.set_attr("reason", Entry::string(reason));
                entry
            }
        }
    }

    fn kind(&self) -> &str { "ProcessorDecision" }

    fn name(&self) -> Option<&str> {
        Some(match self {
            Self::Continue { .. } => "Continue",
            Self::Complete { .. } => "Complete",
            Self::Route { .. } => "Route",
            Self::RouteNested { .. } => "RouteNested",
            Self::NestedComplete { .. } => "NestedComplete",
            Self::Pause { .. } => "Pause",
            Self::Fail { .. } => "Fail",
        })
    }

    fn category(&self) -> Option<&str> { Some("processing") }
}

// ============================================================================
// ProcessorState - Generic handler state
// ============================================================================

/// Generic handler state that all handlers can use.
/// Consolidates the common (stage, confidence, error, output, data) pattern.
///
/// The type parameter `D` must implement `Build` to allow the state to
/// convert its data to Entry form for the database.
#[derive(Debug, Clone)]
pub struct ProcessorState<D: Build> {
    /// Confidence level (0.0 to 1.0)
    pub confidence: f64,
    /// Error message if processing failed
    pub error: Option<String>,
    /// Converted output code
    pub output: Option<String>,
    /// Extracted data
    pub data: D,
    /// Execution statistics (includes current_stage)
    pub stats: ProcessorStats,
}

impl<D: Build + Default> ProcessorState<D> {
    pub fn new(name: &str) -> Self {
        Self {
            confidence: 0.0,
            error: None,
            output: None,
            data: D::default(),
            stats: ProcessorStats::new(name),
        }
    }
}

impl<D: Build> ProcessorState<D> {
    /// Create a new ProcessorState with the given data
    pub fn with_data(name: &str, data: D) -> Self {
        Self {
            confidence: 0.0,
            error: None,
            output: None,
            data,
            stats: ProcessorStats::new(name),
        }
    }

    /// Get the current processing stage from stats
    pub fn stage(&self) -> ProcessorStage {
        ProcessorStage::from_str(&self.stats.current_stage)
    }

    pub fn set_stage(&mut self, stage: ProcessorStage) {
        self.stats.set_stage(stage.as_str());
    }

    pub fn set_confidence(&mut self, confidence: f64) {
        self.confidence = confidence;
    }

    pub fn set_error(&mut self, error: impl Into<String>) {
        self.error = Some(error.into());
        self.stats.set_stage("failed");
    }

    pub fn set_output(&mut self, output: impl Into<String>) {
        self.output = Some(output.into());
    }

    pub fn is_pending(&self) -> bool {
        self.stage() == ProcessorStage::Pending
    }

    pub fn is_validated(&self) -> bool {
        self.stage() == ProcessorStage::Validated
    }

    pub fn is_extracted(&self) -> bool {
        self.stage() == ProcessorStage::Extracted
    }

    pub fn is_complete(&self) -> bool {
        self.stage() == ProcessorStage::Complete
    }

    pub fn is_failed(&self) -> bool {
        self.stage() == ProcessorStage::Failed
    }

    pub fn has_error(&self) -> bool {
        self.error.is_some()
    }

    pub fn has_output(&self) -> bool {
        self.output.is_some()
    }

    /// Get the data as an Entry using the Build trait
    pub fn data_entry(&self) -> Entry {
        self.data.to_entry()
    }

    /// Get the kind of the data using the Build trait
    pub fn data_kind(&self) -> &str {
        self.data.kind()
    }

    /// Get the name of the data using the Build trait
    pub fn data_name(&self) -> Option<&str> {
        self.data.name()
    }

    /// Get the category of the data using the Build trait
    pub fn data_category(&self) -> Option<&str> {
        self.data.category()
    }
}

impl<D: Build> Build for ProcessorState<D> {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("ProcessorState", self.data.name().unwrap_or("unnamed"));
        entry.set_attr("stage", self.stage().to_entry());
        entry.set_attr("confidence", Entry::f64(self.confidence));
        if let Some(ref error) = self.error {
            entry.set_attr("error", Entry::string(error));
        }
        if let Some(ref output) = self.output {
            entry.set_attr("output", Entry::string(output));
        }
        entry.set_attr("data", self.data.to_entry());
        entry.set_attr("data_kind", Entry::string(self.data.kind()));
        if let Some(category) = self.data.category() {
            entry.set_attr("data_category", Entry::string(category));
        }
        entry.set_attr("stats", self.stats.to_entry());
        entry
    }

    fn kind(&self) -> &str {
        "ProcessorState"
    }

    fn name(&self) -> Option<&str> {
        self.data.name()
    }

    fn category(&self) -> Option<&str> {
        self.data.category()
    }

    fn priority(&self) -> i16 {
        self.data.priority()
    }
}
// ============================================================================
// Handler Statistics
// ============================================================================

/// Statistics for a specific handler
#[derive(Debug, Clone, PartialEq)]
pub struct ProcessorStats {
    pub name: String,
    pub total_executions: u32,
    pub successful: u32,
    pub failed: u32,
    pub average_confidence: f64,
    pub average_tokens: f64,
    pub average_time_ms: f64,
    pub patterns_used: HashMap<String, u32>,
    pub current_stage: String,
}

impl Default for ProcessorStats {
    fn default() -> Self {
        Self::new("unnamed")
    }
}

impl ProcessorStats {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            total_executions: 0,
            successful: 0,
            failed: 0,
            average_confidence: 0.0,
            average_tokens: 0.0,
            average_time_ms: 0.0,
            patterns_used: HashMap::new(),
            current_stage: "pending".to_string(),
        }
    }

    /// Set the current processing stage
    pub fn set_stage(&mut self, stage: &str) {
        self.current_stage = stage.to_string();
    }

    /// Calculate success rate
    pub fn success_rate(&self) -> f64 {
        if self.total_executions == 0 {
            0.0
        } else {
            self.successful as f64 / self.total_executions as f64
        }
    }

    /// Calculate failure rate
    pub fn failure_rate(&self) -> f64 {
        1.0 - self.success_rate()
    }

    /// Get top patterns by usage count
    pub fn top_patterns(&self, limit: usize) -> Vec<(&String, &u32)> {
        let mut patterns: Vec<_> = self.patterns_used.iter().collect();
        patterns.sort_by(|a, b| b.1.cmp(a.1));
        patterns.truncate(limit);
        patterns
    }

    /// Record a successful execution
    pub fn record_success(
        &mut self,
        confidence: f64,
        tokens: usize,
        time_ms: u64,
        pattern: Option<&str>,
        stage: &str,
    ) {
        self.total_executions += 1;
        self.successful += 1;
        self.current_stage = stage.to_string();

        let n = self.total_executions as f64;
        self.average_confidence = (self.average_confidence * (n - 1.0) + confidence) / n;
        self.average_tokens = (self.average_tokens * (n - 1.0) + tokens as f64) / n;
        self.average_time_ms = (self.average_time_ms * (n - 1.0) + time_ms as f64) / n;

        if let Some(p) = pattern {
            *self.patterns_used.entry(p.to_string()).or_insert(0) += 1;
        }
    }

    /// Record a failed execution
    pub fn record_failure(&mut self, pattern: Option<&str>, stage: &str) {
        self.total_executions += 1;
        self.failed += 1;
        self.current_stage = stage.to_string();

        if let Some(p) = pattern {
            *self.patterns_used.entry(p.to_string()).or_insert(0) += 1;
        }
    }

    /// Record from a SuccessRecord
    pub fn record_from_success(&mut self, record: &SuccessRecord) {
        self.record_success(
            record.confidence,
            record.token_count,
            record.time_ms,
            Some(&record.pattern_name),
            &record.stage,
        );
    }

    /// Record from a FailureRecord
    pub fn record_from_failure(&mut self, record: &FailureRecord) {
        self.record_failure(Some(&record.pattern_name), &record.stage);
    }

    /// Merge another HandlerStats into this one (for aggregation)
    pub fn merge(&mut self, other: &ProcessorStats) {
        let total = self.total_executions + other.total_executions;
        if total > 0 {
            let n1 = self.total_executions as f64;
            let n2 = other.total_executions as f64;
            let n = total as f64;

            self.average_confidence =
                (self.average_confidence * n1 + other.average_confidence * n2) / n;
            self.average_tokens = (self.average_tokens * n1 + other.average_tokens * n2) / n;
            self.average_time_ms = (self.average_time_ms * n1 + other.average_time_ms * n2) / n;
        }

        self.total_executions = total;
        self.successful += other.successful;
        self.failed += other.failed;

        for (pattern, count) in &other.patterns_used {
            *self.patterns_used.entry(pattern.clone()).or_insert(0) += count;
        }
    }

    /// Check if this handler is performing well
    pub fn is_performing_well(&self, min_success_rate: f64, min_executions: u32) -> bool {
        self.total_executions >= min_executions && self.success_rate() >= min_success_rate
    }
}

impl Build for ProcessorStats {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("ProcessorStats",&self.name);
        entry.set_attr("total_executions", Entry::u32(self.total_executions));
        entry.set_attr("successful", Entry::u32(self.successful));
        entry.set_attr("failed", Entry::u32(self.failed));
        entry.set_attr("success_rate", Entry::f64(self.success_rate()));
        entry.set_attr("average_confidence",Entry::f64(self.average_confidence));
        entry.set_attr("average_tokens",Entry::f64(self.average_tokens));
        entry.set_attr("average_time_ms",Entry::f64(self.average_time_ms));
        entry.set_attr("current_stage", Entry::string(&self.current_stage));
        entry
    }

    fn kind(&self) -> &str {
        "ProcessorStats"
    }

    fn name(&self) -> Option<&str> {
        Some(&self.name)
    }

    fn category(&self) -> Option<&str> {
        Some("processor")
    }
}
// ============================================================================
// Processor Trait - The core processing pipeline
// ============================================================================

/// Trait defining the handler processing pipeline.
/// Each handler must implement this trait to participate in the conversion process.
pub trait Processor: Build {

    /// Get supported pattern names for this handler
    fn supported_patterns(&self) -> &[&str];

    /// Get pattern pairs: (validation_pattern, extraction_pattern)
    /// - First pattern: Simple rules for fast validation (no callbacks)
    /// - Second pattern: Full rules with extraction callbacks
    ///
    /// Both patterns should match the same constructs, but:
    /// - Validation pattern is lightweight for quick matching
    /// - Extraction pattern has callbacks to populate ExtractContext
    fn patterns(&self) -> Vec<(Pattern, Pattern)>;

    /// Get the execution statistics for this handler.
    /// Stats are merged into a global database Entry for aggregate reporting.
    fn stats(&self) -> &ProcessorStats;

    /// Validate that the tokens can be handled by this processor.
    /// Uses simple validation patterns (first of pair) for speed.
    /// Returns true if tokens are valid for this handler, false otherwise.
    fn validate(&mut self, tokens: &[Token]) -> bool {
        if tokens.is_empty() {
            return false;
        }

        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        let token_len = token_strs.len();

        for (validation_pattern, _) in self.patterns() {
            if let Pattern::Definition {
                rules, min_tokens, ..
            } = &validation_pattern
            {
                if token_len < *min_tokens {
                    continue;
                }

                let mut token_idx = 0;
                let mut rule_idx = 0;

                loop {
                    if rule_idx >= rules.len() {
                        return true;
                    }

                    let rule = &rules[rule_idx];

                    if token_idx >= token_len {
                        // Check if remaining rules are all optional
                        if rules[rule_idx..].iter().all(|r| r.optional) {
                            return true;
                        }
                        break;
                    }

                    let token = &token_strs[token_idx];

                    if Self::rule_matches(rule, token) {
                        token_idx += 1;
                        if rule.repeatable != REPEAT_NONE {
                            while token_idx < token_len
                                && Self::rule_matches(rule, &token_strs[token_idx])
                            {
                                token_idx += 1;
                            }
                        }
                        rule_idx += 1;
                    } else if rule.optional {
                        rule_idx += 1;
                    } else {
                        break;
                    }
                }
            }
        }

        false
    }

    /// Extract structured data from the tokens.
    /// Called after validation passes. Populates internal state with extracted info.
    ///
    /// Default implementation uses pattern-based extraction with callbacks.
    /// Handlers can override for custom extraction logic.
    fn extract(&mut self, tokens: &[Token]) -> bool {
        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        let token_len = token_strs.len();
        let mut ctx = ExtractContext::new(&token_strs);

        for (_, extraction_pattern) in self.patterns() {
            let Pattern::Definition { rules, min_tokens, .. } = &extraction_pattern else {
                continue;
            };

            if token_len < *min_tokens {
                continue;
            }

            let rules_len = rules.len();
            let mut token_idx = 0;
            let mut rule_idx = 0;

            while rule_idx < rules_len {
                let rule = &rules[rule_idx];

                if token_idx >= token_len {
                    if rules[rule_idx..].iter().all(|r| r.optional) {
                        self.apply_context(&ctx);
                        return true;
                    }
                    break;
                }

                let token = &token_strs[token_idx];
                let rule_matches = Self::rule_matches(rule, token);

                if rule_matches {
                    ctx.current_token = token.clone();
                    ctx.token_idx = token_idx;
                    ctx.did_match = true;
                    ctx.matched = Some(token.clone());

                    if rule.has_extract() {
                        rule.apply_extract(&mut ctx);
                    }

                    token_idx += 1;

                    if rule.repeatable != REPEAT_NONE {
                        while token_idx < token_len {
                            let next_token = &token_strs[token_idx];
                            if !Self::rule_matches(rule, next_token) {
                                break;
                            }
                            ctx.current_token = next_token.clone();
                            ctx.token_idx = token_idx;
                            ctx.matched = Some(next_token.clone());

                            if rule.has_extract() {
                                rule.apply_extract(&mut ctx);
                            }
                            token_idx += 1;
                        }
                    }
                    rule_idx += 1;
                } else if rule.optional {
                    rule_idx += 1;
                } else {
                    break;
                }
            }

            if rule_idx == rules_len {
                self.apply_context(&ctx);
                return true;
            }

            ctx = ExtractContext::default();
        }

        false
    }

    /// Apply extracted context to handler's internal state.
    /// Override this to map ExtractContext values to your handler's data struct.
    fn apply_context(&mut self, _ctx: &ExtractContext) {
        // Default: no-op. Handlers should override to populate their data.
    }

    /// Check if a rule matches a token.
    /// Can be overridden for custom matching logic.
    fn rule_matches(rule: &PatternRule, token: &str) -> bool {
        match &rule.rule_type {
            RuleType::Exact => rule.value == token,
            RuleType::OneOf => rule.value.split('|').any(|opt| opt.trim() == token),
            RuleType::Identifier => {
                if !token.is_empty() {
                    return crate::system::system().lookup_identifier(token).is_some()
                }
                false
            }
            RuleType::TypeKeyword => {
                if !token.is_empty() {
                    return crate::system::system().lookup_type(token).is_some()
                }
                false
            }
            RuleType::Keyword => crate::db::keyword::is_c_keyword(token),
            RuleType::Any => true,
            RuleType::AnyExcept => !rule.value.split('|').any(|excl| excl.trim() == token),
            RuleType::Custom(matcher) => {
                if let Some(callback) = rule.get_callback(matcher) {
                    let rule = callback.apply_default(rule);
                    rule.value == token
                } else {
                    false
                }
            }
        }
    }

    /// Convert the extracted data to Rust code.
    /// Called after extraction. Produces the Rust output.
    fn convert(&mut self) -> Option<String>;

    /// Determine which handler to route to based on tokens.
    /// Returns Some((target_handler, reason)) if routing is needed, None otherwise.
    /// This is pure decision-making logic - no side effects.
    fn get_route(&self, tokens: &[Token]) -> Option<(String, String)> {
        if tokens.is_empty() {
            return None;
        }

        let first = tokens[0].to_string();

        match first.as_str() {
            "break" => Some(("_literal".to_string(), "break".to_string())),
            "continue" => Some(("_literal".to_string(), "continue".to_string())),
            "return" => Some(("ExpressionHandler".to_string(), "return expression".to_string())),
            "switch" => Some(("SwitchCaseHandler".to_string(), "switch statement".to_string())),
            "if" => Some(("IfElseHandler".to_string(), "if statement".to_string())),
            "for" | "while" | "do" => Some(("LoopHandler".to_string(), "loop statement".to_string())),
            "printf" | "fprintf" | "sprintf" | "snprintf" | "puts" => {
                Some(("StringFormatHandler".to_string(), "printf-style call".to_string()))
            }
            _ => {
                let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

                let mut has_increment = false;
                let mut has_compound_assign = false;
                let mut has_simple_assign = false;
                let mut bracket_pos = None;
                let mut eq_pos = None;
                let mut arrow_pos = None;
                let mut paren_pos = None;

                for (i, s) in token_strs.iter().enumerate() {
                    match s.as_str() {
                        "++" | "--" => has_increment = true,
                        "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>=" => {
                            has_compound_assign = true;
                        }
                        "=" => {
                            has_simple_assign = true;
                            if eq_pos.is_none() { eq_pos = Some(i); }
                        }
                        "[" if bracket_pos.is_none() => bracket_pos = Some(i),
                        "->" if arrow_pos.is_none() => arrow_pos = Some(i),
                        "(" if paren_pos.is_none() => paren_pos = Some(i),
                        _ => {}
                    }
                }

                if arrow_pos.is_none() {
                    for i in 0..token_strs.len().saturating_sub(1) {
                        if token_strs[i] == "-" && token_strs[i + 1] == ">" {
                            arrow_pos = Some(i);
                            break;
                        }
                    }
                }

                if has_increment {
                    return Some(("ExpressionHandler".to_string(), "increment/decrement".to_string()));
                }

                // Compound assignments are expressions, not declarations
                if has_compound_assign {
                    return Some(("ExpressionHandler".to_string(), "compound assignment".to_string()));
                }

                let has_arrow_before_bracket = match (arrow_pos, bracket_pos) {
                    (Some(a), Some(b)) => a < b,
                    _ => false,
                };
                let is_array_decl = match (bracket_pos, eq_pos) {
                    (Some(b), Some(e)) => b < e && !has_arrow_before_bracket,
                    (Some(_), None) => !has_arrow_before_bracket,
                    _ => false,
                };

                if is_array_decl && !has_compound_assign {
                    return Some(("ArrayHandler".to_string(), "array declaration".to_string()));
                }

                // Member access assignments (ptr->field = value or obj.field = value) are expressions
                if has_simple_assign && (arrow_pos.is_some() || token_strs.iter().any(|t| t == ".")) {
                    if let Some(eq) = eq_pos {
                        let has_member_before_eq = arrow_pos.map(|a| a < eq).unwrap_or(false)
                            || token_strs[..eq].iter().any(|t| t == ".");
                        if has_member_before_eq {
                            return Some(("ExpressionHandler".to_string(), "member assignment".to_string()));
                        }
                    }
                }

                // Variable declarations need a type keyword at the start
                // Simple assignments without type keywords (y = x) are expressions
                // Also handle const/static prefixes: const char* x, static int y
                if is_type_keyword(&first) {
                    return Some(("VariableHandler".to_string(), "variable declaration".to_string()));
                }
                if matches!(first.as_str(), "const" | "static" | "volatile") && token_strs.len() > 1 {
                    if is_type_keyword(&token_strs[1]) {
                        return Some(("VariableHandler".to_string(), "variable declaration".to_string()));
                    }
                }
                
                // Simple assignments without type keywords go to ExpressionHandler
                if has_simple_assign && !has_compound_assign {
                    return Some(("ExpressionHandler".to_string(), "assignment".to_string()));
                }

                if let Some(pp) = paren_pos {
                    if pp > 0 && tokens.len() >= 3 {
                        return Some(("CallHandler".to_string(), "function call".to_string()));
                    }
                }

                Some(("ExpressionHandler".to_string(), "expression".to_string()))
            }
        }
    }

    /// Execute routing to the target handler and return the converted code.
    /// This performs the actual routing actions after get_route decides the target.
    fn route(&self, tokens: &[Token], source: &str, target: &str, reason: &str) -> Option<String> {
        use crate::db::routing::Route;
        use crate::system::system;
        use crate::handlers::arrays::ArrayHandler;
        use crate::handlers::functions::CallHandler;

        if tokens.is_empty() {
            return None;
        }

        let sys = system();
        let first = tokens[0].to_string();

        // Handle literal returns (break, continue)
        if target == "_literal" {
            return Some(format!("{};", reason));
        }

        // Register the route
        let route = Route::new(source, target)
            .with_reason(reason)
            .with_range(0, tokens.len());
        let _id = sys.register_route(route);

        match target {
            "ExpressionHandler" => {
                use crate::handlers::expressions::ExpressionHandler;
                let expr_tokens: Vec<Token> = if first == "return" {
                    tokens[1..].iter().filter(|t| t.to_string() != ";").cloned().collect()
                } else {
                    tokens.iter().filter(|t| t.to_string() != ";").cloned().collect()
                };

                if expr_tokens.is_empty() && first == "return" {
                    return Some("return;".to_string());
                }

                let mut handler = ExpressionHandler::new();
                if handler.validate(&expr_tokens) && handler.extract(&expr_tokens) {
                    if let Some(code) = handler.convert() {
                        return if first == "return" {
                            Some(format!("return {};", code))
                        } else {
                            Some(format!("{};", code))
                        };
                    }
                }

                // Fallback: output the expression with cast conversion and enum variant lookup
                let expr = expr_tokens.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(" ");
                let converted = crate::handlers::common::convert_c_style_cast(&expr);
                // Check if the expression is a simple identifier that might be an enum variant
                let final_expr = if converted.chars().all(|c| c.is_alphanumeric() || c == '_') {
                    crate::system::system().lookup_enum_variant(&converted)
                        .unwrap_or_else(|| converted.clone())
                } else {
                    converted.clone()
                };
                if first == "return" {
                    Some(format!("return {};", final_expr))
                } else if !final_expr.is_empty() {
                    Some(format!("{};", final_expr))
                } else {
                    None
                }
            }
            "SwitchCaseHandler" => {
                use crate::handlers::conditionals::SwitchCaseHandler;
                let mut handler = SwitchCaseHandler::new();
                if handler.validate(tokens) && handler.extract(tokens) {
                    return handler.convert();
                }
                None
            }
            "IfElseHandler" => {
                use crate::handlers::conditionals::IfElseHandler;
                let mut handler = IfElseHandler::new();
                if handler.validate(tokens) && handler.extract(tokens) {
                    return handler.convert();
                }
                None
            }
            "LoopHandler" => {
                use crate::handlers::loops::LoopHandler;
                let mut handler = LoopHandler::new();
                if handler.validate(tokens) && handler.extract(tokens) {
                    return handler.convert();
                }
                None
            }
            "StringFormatHandler" => {
                use crate::handlers::expressions::StringFormatHandler;
                let mut handler = StringFormatHandler::new();
                if handler.validate(tokens) && handler.extract(tokens) {
                    if let Some(code) = handler.convert() {
                        return Some(format!("{};", code));
                    }
                }
                None
            }
            "ArrayHandler" => {
                let mut handler = ArrayHandler::new();
                if handler.validate(tokens) && handler.extract(tokens) {
                    return handler.convert();
                }
                None
            }
            "VariableHandler" => {
                use crate::handlers::variables::VariableHandler;
                let mut handler = VariableHandler::new();
                if handler.validate(tokens) && handler.extract(tokens) {
                    return handler.convert();
                }
                None
            }
            "CallHandler" => {
                let mut handler = CallHandler::new();
                if handler.validate(tokens) && handler.extract(tokens) {
                    if let Some(code) = handler.convert() {
                        return Some(format!("{};", code));
                    }
                }
                None
            }
            _ => {
                let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
                Some(format!("// {}", token_strs.join(" ")))
            }
        }
    }
    /// Check if a token is a C type keyword

    /// Route a statement to the appropriate handler.
    /// Wrapper that uses get_route to decide, then route to execute.
    fn route_statement(&self, tokens: &[Token], source: &str) -> String {
        if let Some((target, reason)) = self.get_route(tokens) {
            self.route(tokens, source, &target, &reason).unwrap_or_else(|| {
                let strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
                format!("// {}", strs.join(" "))
            })
        } else {
            let strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
            format!("// {}", strs.join(" "))
        }
    }

    /// Main processing function called iteratively from the processing loop.
    /// Decides whether to proceed to the next stage or return early.
    ///
    /// This is the entry point called by the main process loop.
    /// Default implementation provides the standard pipeline flow.
    ///
    /// # Arguments
    /// * `tokens` - The tokens to process
    /// * `route_info` - Optional (source_handler, target_handler) tuple. If Some, 
    ///   the function will route to the target handler directly instead of normal processing.
    fn process(&mut self, tokens: &[Token], route_info: Option<(&str, &str)>) -> ProcessorDecision {
        // If route_info is provided, perform direct routing
        if let Some((source, target)) = route_info {
            // Get reason from get_route or use default
            let reason = self.get_route(tokens)
                .map(|(_, r)| r)
                .unwrap_or_else(|| "direct route".to_string());
            
            if let Some(code) = self.route(tokens, source, target, &reason) {
                self.set_stage(ProcessorStage::Complete);
                self.set_output(code.clone());
                return ProcessorDecision::Complete {
                    rust_code: code,
                    confidence: self.confidence(),
                };
            } else {
                self.set_stage(ProcessorStage::Failed);
                return ProcessorDecision::Fail {
                    stage: ProcessorStage::Routed,
                    reason: format!("Routing to {} failed", target),
                };
            }
        }

        // Normal processing pipeline
        match self.current_stage() {
            ProcessorStage::Pending => {
                if self.validate(tokens) {
                    self.set_stage(ProcessorStage::Validated);
                    ProcessorDecision::Continue {
                        stage: ProcessorStage::Validated,
                        reason: "Validation passed".to_string(),
                    }
                } else {
                    self.set_stage(ProcessorStage::Failed);
                    ProcessorDecision::Fail {
                        stage: ProcessorStage::Pending,
                        reason: "Validation failed - tokens not suitable for this handler"
                            .to_string(),
                    }
                }
            }
            ProcessorStage::Validated => {
                // Proceed directly to extraction (routing is handled by route_statement for sub-constructs)
                if self.extract(tokens) {
                    self.set_stage(ProcessorStage::Extracted);
                    ProcessorDecision::Continue {
                        stage: ProcessorStage::Extracted,
                        reason: "Extraction successful".to_string(),
                    }
                } else {
                    self.set_stage(ProcessorStage::Failed);
                    ProcessorDecision::Fail {
                        stage: ProcessorStage::Validated,
                        reason: "Extraction failed - could not parse token structure".to_string(),
                    }
                }
            }
            ProcessorStage::Extracted => {
                if let Some(rust_code) = self.convert() {
                    self.set_stage(ProcessorStage::Converted);
                    self.set_output(rust_code.clone());
                    ProcessorDecision::Continue {
                        stage: ProcessorStage::Converted,
                        reason: "Conversion successful".to_string(),
                    }
                } else {
                    self.set_stage(ProcessorStage::Failed);
                    ProcessorDecision::Fail {
                        stage: ProcessorStage::Extracted,
                        reason: "Conversion failed - could not generate Rust code".to_string(),
                    }
                }
            }
            ProcessorStage::Converted => {
                self.set_stage(ProcessorStage::Complete);
                // Register this handler to the database if auto-registration is enabled
                self.register();
                ProcessorDecision::Complete {
                    rust_code: self.output().unwrap_or_default(),
                    confidence: self.confidence(),
                }
            }
            ProcessorStage::Complete => ProcessorDecision::Complete {
                rust_code: self.output().unwrap_or_default(),
                confidence: self.confidence(),
            },
            ProcessorStage::Routed => {
                if let Some((target, reason)) = self.get_route(tokens) {
                    ProcessorDecision::Route {
                        target_handler: target,
                        reason,
                    }
                } else {
                    ProcessorDecision::Fail {
                        stage: ProcessorStage::Routed,
                        reason: "No route target found".to_string(),
                    }
                }
            }
            ProcessorStage::Failed => ProcessorDecision::Fail {
                stage: ProcessorStage::Failed,
                reason: self.error().unwrap_or("Unknown error".to_string()),
            },
        }
    }

    // === State accessors (must be implemented) ===

    /// Get current processing stage
    fn current_stage(&self) -> ProcessorStage;

    /// Set processing stage
    fn set_stage(&mut self, stage: ProcessorStage);

    /// Get the generated output (if any)
    fn output(&self) -> Option<String>;

    /// Set the output
    fn set_output(&mut self, output: String);

    /// Get error message (if failed)
    fn error(&self) -> Option<String>;

    /// Set error message
    fn set_error(&mut self, error: String);

    /// Get confidence score (0.0 - 1.0)
    fn confidence(&self) -> f64;

    /// Set confidence score
    fn set_confidence(&mut self, confidence: f64);
}

pub fn lookup_type(c_type: impl Into<String>) -> Option<String> {
    crate::system::system().lookup_type(c_type.into().as_str())
}
pub fn lookup_identifier(c_identfier: impl Into<String>) -> Option<String> {
    crate::system::system().lookup_identifier(c_identfier.into().as_str())
}
pub fn is_type_keyword(token: &str) -> bool {
        // Check for struct/enum/union prefixes
        let prefixes = ["struct", "enum", "union"]; 
        for prefix in prefixes.iter() {
            if token.starts_with(prefix) {
                return true;
            }
        }
        // Check against known C type keywords
        if crate::db::keyword::is_c_type_keyword(token) {
            return true;
        }
        // Also check registered types (typedefs, structs, enums)
        crate::system::system().lookup_type(token).is_some()
    }
// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    /// Test data struct that implements Build
    #[derive(Debug, Clone, Default)]
    struct TestData {
        name: String,
        value: i32,
    }

    impl Build for TestData {
        fn to_entry(&self) -> Entry {
            let mut entry = Entry::node("TestData", &self.name);
            entry.set_attr("value", Entry::i32(self.value));
            entry
        }

        fn kind(&self) -> &str {
            "TestData"
        }

        fn name(&self) -> Option<&str> {
            Some(&self.name)
        }

        fn category(&self) -> Option<&str> {
            Some("test")
        }
    }

    #[test]
    fn test_processor_state_new() {
        let state: ProcessorState<TestData> = ProcessorState::new("TestHandler");
        assert!(state.is_pending());
        assert!(!state.has_error());
        assert!(!state.has_output());
        assert_eq!(state.confidence, 0.0);
    }

    #[test]
    fn test_processor_state_with_data() {
        let data = TestData {
            name: "test".to_string(),
            value: 42,
        };
        let state = ProcessorState::with_data("TestHandler", data);
        assert_eq!(state.data.name, "test");
        assert_eq!(state.data.value, 42);
        assert_eq!(state.data_kind(), "TestData");
        assert_eq!(state.data_name(), Some("test"));
    }

    #[test]
    fn test_processor_state_stages() {
        let mut state: ProcessorState<TestData> = ProcessorState::new("TestHandler");
        
        state.set_stage(ProcessorStage::Validated);
        assert!(state.is_validated());
        
        state.set_stage(ProcessorStage::Extracted);
        assert!(state.is_extracted());
        
        state.set_stage(ProcessorStage::Complete);
        assert!(state.is_complete());
        
        state.set_confidence(0.95);
        state.set_output("let x: i32 = 42;");
        
        assert_eq!(state.confidence, 0.95);
        assert!(state.has_output());
    }

    #[test]
    fn test_processor_state_error() {
        let mut state: ProcessorState<TestData> = ProcessorState::new("TestHandler");
        state.set_error("parse error");
        
        assert!(state.is_failed());
        assert!(state.has_error());
        assert_eq!(state.error, Some("parse error".to_string()));
    }

    #[test]
    fn test_processor_state_build() {
        let mut state = ProcessorState::with_data("TestHandler", TestData {
            name: "example".to_string(),
            value: 100,
        });
        state.set_stage(ProcessorStage::Complete);
        state.set_confidence(0.9);
        state.set_output("output code");

        let entry = state.to_entry();
        assert_eq!(state.kind(), "ProcessorState");
        assert_eq!(state.name(), Some("example"));
        assert_eq!(state.category(), Some("test"));
        
        // Verify the entry contains the expected structure
        assert!(entry.get_attr("stage").is_some());
        assert!(entry.get_attr("data").is_some());
    }
}