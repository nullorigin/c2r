//! Handler Types and Database
//!
//! This module provides a unified Handler object that contains all data for a single
//! handler, aligning with the Web database pattern where each entry is self-contained.

pub use crate::db::token::TokenRange;
use crate::db::web::{Build, Entry};
use crate::lock::Id;
use std::collections::HashMap;
use std::ops::Range;
use std::time::SystemTime;

// ============================================================================
// Code Segment
// ============================================================================

/// Code segment processed by a handler
#[derive(Debug, Clone, PartialEq)]
pub struct CodeSegment {
    pub tokens: Vec<String>,
    pub c_code: String,
    pub rust_code: Option<String>,
    pub range: TokenRange,
    pub segment_type: String,
}

impl CodeSegment {
    pub fn new(tokens: Vec<String>, c_code: String, range: TokenRange) -> Self {
        Self {
            tokens,
            c_code,
            rust_code: None,
            range,
            segment_type: "unknown".to_string(),
        }
    }

    pub fn with_rust_code(mut self, code: impl Into<String>) -> Self {
        self.rust_code = Some(code.into());
        self
    }

    pub fn with_type(mut self, segment_type: impl Into<String>) -> Self {
        self.segment_type = segment_type.into();
        self
    }
}

impl Build for CodeSegment {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("CodeSegment", &self.segment_type);
        entry.set_attr("c_code", Entry::string(&self.c_code));
        if let Some(ref rust) = self.rust_code {
            entry.set_attr("rust_code", Entry::string(rust));
        }
        entry.set_attr("segment_type", Entry::string(&self.segment_type));
        entry.set_attr("token_count", Entry::usize(self.tokens.len()));
        let tokens: Vec<Entry> = self.tokens.iter().map(|t| Entry::string(t)).collect();
        entry.set_attr("tokens", Entry::vec(tokens));
        entry
    }

    fn kind(&self) -> &str {
        "CodeSegment"
    }

    fn name(&self) -> Option<&str> {
        if self.segment_type.is_empty() {
            None
        } else {
            Some(&self.segment_type)
        }
    }

    fn category(&self) -> Option<&str> {
        Some(&self.segment_type)
    }
}

// ============================================================================
// Handler Result
// ============================================================================

/// Result of handler execution
#[derive(Debug, Clone, PartialEq)]
pub enum HandlerResult {
    /// Handler successfully completed processing and converted code
    Completed {
        tokens: Option<Vec<String>>,
        range: Range<usize>,
        rust_code: String,
        metadata: HashMap<String, String>,
    },

    /// Handler successfully extracted but did not convert yet
    Extracted {
        tokens: Option<Vec<String>>,
        range: Range<usize>,
        data: String,
        metadata: HashMap<String, String>,
    },

    /// Handler could not process these tokens
    NotHandled {
        tokens: Option<Vec<String>>,
        range: Range<usize>,
        handler_id: Id,
    },

    /// Handler failed with error
    Failed {
        tokens: Option<Vec<String>>,
        range: Range<usize>,
        error: String,
        handler_id: Id,
    },
}

impl HandlerResult {
    /// Create a completed result
    pub fn completed(range: Range<usize>, rust_code: impl Into<String>) -> Self {
        Self::Completed {
            tokens: None,
            range,
            rust_code: rust_code.into(),
            metadata: HashMap::new(),
        }
    }

    /// Create an extracted result
    pub fn extracted(range: Range<usize>, data: impl Into<String>) -> Self {
        Self::Extracted {
            tokens: None,
            range,
            data: data.into(),
            metadata: HashMap::new(),
        }
    }

    /// Create a not-handled result
    pub fn not_handled(range: Range<usize>, handler_id: Id) -> Self {
        Self::NotHandled {
            tokens: None,
            range,
            handler_id,
        }
    }

    /// Create a failed result
    pub fn failed(range: Range<usize>, error: impl Into<String>, handler_id: Id) -> Self {
        Self::Failed {
            tokens: None,
            range,
            error: error.into(),
            handler_id,
        }
    }

    /// Check if result was successful
    pub fn is_success(&self) -> bool {
        matches!(self, Self::Completed { .. } | Self::Extracted { .. })
    }

    /// Check if result was a failure
    pub fn is_failure(&self) -> bool {
        matches!(self, Self::Failed { .. })
    }

    /// Check if handler did not process
    pub fn is_not_handled(&self) -> bool {
        matches!(self, Self::NotHandled { .. })
    }

    /// Get the Rust code if conversion was successful
    pub fn rust_code(&self) -> Option<&str> {
        match self {
            Self::Completed { rust_code, .. } => Some(rust_code),
            _ => None,
        }
    }

    /// Get the token range
    pub fn range(&self) -> &Range<usize> {
        match self {
            Self::Completed { range, .. }
            | Self::Extracted { range, .. }
            | Self::NotHandled { range, .. }
            | Self::Failed { range, .. } => range,
        }
    }
}

impl Build for HandlerResult {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("HandlerResult", self.name().unwrap_or(""));

        let (status, range) = match self {
            Self::Completed {
                range, rust_code, ..
            } => {
                entry.set_attr("rust_code", Entry::string(rust_code));
                ("completed", range)
            }
            Self::Extracted { range, data, .. } => {
                entry.set_attr("data", Entry::string(data));
                ("extracted", range)
            }
            Self::NotHandled {
                range, handler_id, ..
            } => {
                entry.set_attr("handler_id", Entry::string(handler_id.name()));
                ("not_handled", range)
            }
            Self::Failed {
                range,
                error,
                handler_id,
                ..
            } => {
                entry.set_attr("error", Entry::string(error));
                entry.set_attr("handler_id", Entry::string(handler_id.name()));
                ("failed", range)
            }
        };
        entry.set_attr("status", Entry::string(status));
        entry.set_attr("range_start", Entry::usize(range.start));
        entry.set_attr("range_end", Entry::usize(range.end));
        entry
    }

    fn kind(&self) -> &str {
        "HandlerResult"
    }

    fn name(&self) -> Option<&str> {
        match self {
            Self::Completed { .. } => Some("completed"),
            Self::Extracted { .. } => Some("extracted"),
            Self::NotHandled { .. } => Some("not_handled"),
            Self::Failed { .. } => Some("failed"),
        }
    }

    fn category(&self) -> Option<&str> {
        Some(match self {
            Self::Completed { .. } => "success",
            Self::Extracted { .. } => "success",
            Self::NotHandled { .. } => "skipped",
            Self::Failed { .. } => "error",
        })
    }
}

// ============================================================================
// Handler Statistics
// ============================================================================

/// Statistics for a specific handler
#[derive(Debug, Clone, PartialEq)]
pub struct HandlerStats {
    pub name: String,
    pub total_executions: u32,
    pub successful: u32,
    pub failed: u32,
    pub average_confidence: f64,
    pub average_tokens: f64,
    pub average_time_ms: f64,
    pub patterns_used: HashMap<String, u32>,
}

impl HandlerStats {
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
        }
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
    ) {
        self.total_executions += 1;
        self.successful += 1;

        let n = self.total_executions as f64;
        self.average_confidence = (self.average_confidence * (n - 1.0) + confidence) / n;
        self.average_tokens = (self.average_tokens * (n - 1.0) + tokens as f64) / n;
        self.average_time_ms = (self.average_time_ms * (n - 1.0) + time_ms as f64) / n;

        if let Some(p) = pattern {
            *self.patterns_used.entry(p.to_string()).or_insert(0) += 1;
        }
    }

    /// Record a failed execution
    pub fn record_failure(&mut self, pattern: Option<&str>) {
        self.total_executions += 1;
        self.failed += 1;

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
        );
    }

    /// Record from a FailureRecord
    pub fn record_from_failure(&mut self, record: &FailureRecord) {
        self.record_failure(Some(&record.pattern_name));
    }

    /// Merge another HandlerStats into this one (for aggregation)
    pub fn merge(&mut self, other: &HandlerStats) {
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

impl Build for HandlerStats {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("HandlerStats", &self.name);
        entry.set_attr("total_executions", Entry::u32(self.total_executions));
        entry.set_attr("successful", Entry::u32(self.successful));
        entry.set_attr("failed", Entry::u32(self.failed));
        entry.set_attr("success_rate", Entry::f64(self.success_rate()));
        entry.set_attr("average_confidence", Entry::f64(self.average_confidence));
        entry.set_attr("average_tokens", Entry::f64(self.average_tokens));
        entry.set_attr("average_time_ms", Entry::f64(self.average_time_ms));
        entry
    }

    fn kind(&self) -> &str {
        "HandlerStats"
    }

    fn name(&self) -> Option<&str> {
        Some(&self.name)
    }

    fn category(&self) -> Option<&str> {
        Some("handler")
    }
}

// ============================================================================
// Handler Metadata
// ============================================================================

/// Metadata about a handler
#[derive(Debug, Clone, PartialEq)]
pub struct HandlerMetadata {
    pub name: String,
    pub handler_type: String,
    pub priority: i32,
    pub patterns: Vec<String>,
    pub category: String,
    pub min_tokens: usize,
    pub max_tokens: Option<usize>,
}

impl HandlerMetadata {
    pub fn new(name: impl Into<String>, handler_type: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            handler_type: handler_type.into(),
            priority: 0,
            patterns: Vec::new(),
            category: "general".to_string(),
            min_tokens: 1,
            max_tokens: None,
        }
    }

    pub fn with_priority(mut self, priority: i32) -> Self {
        self.priority = priority;
        self
    }

    pub fn with_patterns(mut self, patterns: Vec<impl Into<String>>) -> Self {
        self.patterns = patterns.into_iter().map(|p| p.into()).collect();
        self
    }

    pub fn with_category(mut self, category: impl Into<String>) -> Self {
        self.category = category.into();
        self
    }

    pub fn with_token_range(mut self, min: usize, max: Option<usize>) -> Self {
        self.min_tokens = min;
        self.max_tokens = max;
        self
    }
}

impl Build for HandlerMetadata {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("HandlerMetadata", &self.name);
        entry.set_attr("handler_type", Entry::string(&self.handler_type));
        entry.set_attr("priority", Entry::i32(self.priority));
        entry.set_attr("category", Entry::string(&self.category));
        entry.set_attr("min_tokens", Entry::usize(self.min_tokens));
        if let Some(max) = self.max_tokens {
            entry.set_attr("max_tokens", Entry::usize(max));
        }
        let patterns: Vec<Entry> = self.patterns.iter().map(|p| Entry::string(p)).collect();
        entry.set_attr("patterns", Entry::vec(patterns));
        entry
    }

    fn kind(&self) -> &str {
        "HandlerMetadata"
    }

    fn name(&self) -> Option<&str> {
        Some(&self.name)
    }

    fn category(&self) -> Option<&str> {
        Some(&self.category)
    }

    fn priority(&self) -> i16 {
        self.priority as i16
    }
}

// ============================================================================
// Execution Records
// ============================================================================

/// Record of successful handler execution
#[derive(Debug, Clone, PartialEq)]
pub struct SuccessRecord {
    pub handler_name: String,
    pub pattern_name: String,
    pub token_count: usize,
    pub confidence: f64,
    pub segment: CodeSegment,
    pub time_ms: u64,
    pub timestamp: SystemTime,
    pub stage: String,
}

impl SuccessRecord {
    pub fn new(
        handler_name: impl Into<String>,
        pattern_name: impl Into<String>,
        segment: CodeSegment,
        confidence: f64,
        time_ms: u64,
        stage: impl Into<String>,
    ) -> Self {
        Self {
            handler_name: handler_name.into(),
            pattern_name: pattern_name.into(),
            token_count: segment.tokens.len(),
            confidence,
            segment,
            time_ms,
            timestamp: SystemTime::now(),
            stage: stage.into(),
        }
    }
}

impl Build for SuccessRecord {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("SuccessRecord", &self.handler_name);
        entry.set_attr("handler_name", Entry::string(&self.handler_name));
        entry.set_attr("pattern_name", Entry::string(&self.pattern_name));
        entry.set_attr("token_count", Entry::usize(self.token_count));
        entry.set_attr("confidence", Entry::f64(self.confidence));
        entry.set_attr("time_ms", Entry::u64(self.time_ms));
        entry.set_attr("stage", Entry::string(&self.stage));
        entry
    }

    fn kind(&self) -> &str {
        "SuccessRecord"
    }

    fn name(&self) -> Option<&str> {
        Some(&self.handler_name)
    }

    fn category(&self) -> Option<&str> {
        Some("success")
    }
}

/// Record of failed handler execution
#[derive(Debug, Clone, PartialEq)]
pub struct FailureRecord {
    pub handler_name: String,
    pub pattern_name: String,
    pub token_count: usize,
    pub reason: String,
    pub segment: CodeSegment,
    pub timestamp: SystemTime,
    pub stage: String,
}

impl FailureRecord {
    pub fn new(
        handler_name: impl Into<String>,
        pattern_name: impl Into<String>,
        segment: CodeSegment,
        reason: impl Into<String>,
        stage: impl Into<String>,
    ) -> Self {
        Self {
            handler_name: handler_name.into(),
            pattern_name: pattern_name.into(),
            token_count: segment.tokens.len(),
            reason: reason.into(),
            segment,
            timestamp: SystemTime::now(),
            stage: stage.into(),
        }
    }
}

impl Build for FailureRecord {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("FailureRecord", &self.handler_name);
        entry.set_attr("handler_name", Entry::string(&self.handler_name));
        entry.set_attr("pattern_name", Entry::string(&self.pattern_name));
        entry.set_attr("token_count", Entry::usize(self.token_count));
        entry.set_attr("reason", Entry::string(&self.reason));
        entry.set_attr("stage", Entry::string(&self.stage));
        entry
    }

    fn kind(&self) -> &str {
        "FailureRecord"
    }

    fn name(&self) -> Option<&str> {
        Some(&self.handler_name)
    }

    fn category(&self) -> Option<&str> {
        Some("error")
    }
}

// ============================================================================
// Selection and Routing
// ============================================================================

/// Strategy for selecting the best handler
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SelectionStrategy {
    /// Select by priority only
    Priority,
    /// Select by confidence
    Confidence,
    /// Select by success rate
    SuccessRate,
    /// Hybrid approach
    Hybrid,
}

/// Handler selection result
#[derive(Debug, Clone)]
pub struct Selection {
    pub name: String,
    pub confidence: f64,
    pub reason: String,
    pub alternatives: Vec<String>,
}

impl Selection {
    pub fn new(name: impl Into<String>, confidence: f64, reason: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            confidence,
            reason: reason.into(),
            alternatives: Vec::new(),
        }
    }

    pub fn with_alternatives(mut self, alternatives: Vec<impl Into<String>>) -> Self {
        self.alternatives = alternatives.into_iter().map(|a| a.into()).collect();
        self
    }
}

/// Condition for routing decision
#[derive(Debug, Clone, PartialEq)]
pub enum RoutingCondition {
    /// Always route
    Always,
    /// Route if confidence below threshold
    LowConfidence(f64),
    /// Route if pattern matches
    PatternMatch(String),
    /// Route if handler fails
    OnFailure,
    /// Route if token count in range
    TokenCount { min: usize, max: usize },
}

/// Routing rule between handlers
#[derive(Debug, Clone, PartialEq)]
pub struct RoutingRule {
    pub from_handler: String,
    pub to_handler: String,
    pub condition: RoutingCondition,
    pub priority: i32,
    pub enabled: bool,
}

impl RoutingRule {
    pub fn new(
        from: impl Into<String>,
        to: impl Into<String>,
        condition: RoutingCondition,
    ) -> Self {
        Self {
            from_handler: from.into(),
            to_handler: to.into(),
            condition,
            priority: 0,
            enabled: true,
        }
    }

    pub fn with_priority(mut self, priority: i32) -> Self {
        self.priority = priority;
        self
    }
}

impl Build for RoutingRule {
    fn to_entry(&self) -> Entry {
        let name = format!("{}->{}", self.from_handler, self.to_handler);
        let mut entry = Entry::node("RoutingRule", &name);
        entry.set_attr("from_handler", Entry::string(&self.from_handler));
        entry.set_attr("to_handler", Entry::string(&self.to_handler));
        entry.set_attr("priority", Entry::i32(self.priority));
        entry.set_attr("enabled", Entry::bool(self.enabled));
        entry
    }

    fn kind(&self) -> &str {
        "RoutingRule"
    }

    fn name(&self) -> Option<&str> {
        Some(&self.from_handler)
    }

    fn category(&self) -> Option<&str> {
        Some("routing")
    }

    fn priority(&self) -> i16 {
        self.priority as i16
    }
}

// ============================================================================
// Pattern Usage Tracking
// ============================================================================

/// Pattern usage record for tracking pattern effectiveness
#[derive(Debug, Clone, PartialEq)]
pub struct PatternUsage {
    pub pattern_name: String,
    pub handler_name: String,
    pub success_count: u32,
    pub failure_count: u32,
    pub total_confidence: f64,
    pub average_confidence: f64,
    pub segments: Vec<CodeSegment>,
}

impl PatternUsage {
    pub fn new(pattern_name: impl Into<String>, handler_name: impl Into<String>) -> Self {
        Self {
            pattern_name: pattern_name.into(),
            handler_name: handler_name.into(),
            success_count: 0,
            failure_count: 0,
            total_confidence: 0.0,
            average_confidence: 0.0,
            segments: Vec::new(),
        }
    }

    pub fn record_success(&mut self, confidence: f64, segment: CodeSegment) {
        self.success_count += 1;
        self.total_confidence += confidence;
        self.average_confidence =
            self.total_confidence / (self.success_count + self.failure_count) as f64;
        self.segments.push(segment);
    }

    pub fn record_failure(&mut self, segment: CodeSegment) {
        self.failure_count += 1;
        self.average_confidence =
            self.total_confidence / (self.success_count + self.failure_count) as f64;
        self.segments.push(segment);
    }
}

impl Build for PatternUsage {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("PatternUsage", &self.pattern_name);
        entry.set_attr("pattern_name", Entry::string(&self.pattern_name));
        entry.set_attr("handler_name", Entry::string(&self.handler_name));
        entry.set_attr("success_count", Entry::u32(self.success_count));
        entry.set_attr("failure_count", Entry::u32(self.failure_count));
        entry.set_attr("average_confidence", Entry::f64(self.average_confidence));
        entry
    }

    fn kind(&self) -> &str {
        "PatternUsage"
    }

    fn name(&self) -> Option<&str> {
        Some(&self.pattern_name)
    }

    fn category(&self) -> Option<&str> {
        Some("pattern")
    }
}

// ============================================================================
// Redirect Request/Response
// ============================================================================

/// Record of a redirect request between handlers
#[derive(Debug, Clone, PartialEq)]
pub struct RedirectRequest {
    pub from_handler: String,
    pub request_reason: String,
    pub token_count: usize,
    pub segment: CodeSegment,
    pub confidence: f64,
    pub patterns_matched: Vec<String>,
    pub timestamp: SystemTime,
}

impl RedirectRequest {
    pub fn new(
        from_handler: impl Into<String>,
        reason: impl Into<String>,
        segment: CodeSegment,
        confidence: f64,
    ) -> Self {
        Self {
            from_handler: from_handler.into(),
            request_reason: reason.into(),
            token_count: segment.tokens.len(),
            segment,
            confidence,
            patterns_matched: Vec::new(),
            timestamp: SystemTime::now(),
        }
    }

    pub fn with_patterns(mut self, patterns: Vec<impl Into<String>>) -> Self {
        self.patterns_matched = patterns.into_iter().map(|p| p.into()).collect();
        self
    }
}

/// Response to a redirect request
#[derive(Debug, Clone, PartialEq)]
pub struct RedirectResponse {
    pub request: RedirectRequest,
    pub target_handler: Option<String>,
    pub routing_reason: String,
    pub should_retry: bool,
    pub alternative_handlers: Vec<String>,
    pub routing_confidence: f64,
    pub timestamp: SystemTime,
}

impl RedirectResponse {
    pub fn new(
        request: RedirectRequest,
        target: Option<String>,
        reason: impl Into<String>,
    ) -> Self {
        Self {
            request,
            target_handler: target,
            routing_reason: reason.into(),
            should_retry: false,
            alternative_handlers: Vec::new(),
            routing_confidence: 0.0,
            timestamp: SystemTime::now(),
        }
    }

    pub fn with_retry(mut self, retry: bool) -> Self {
        self.should_retry = retry;
        self
    }

    pub fn with_alternatives(mut self, alts: Vec<impl Into<String>>) -> Self {
        self.alternative_handlers = alts.into_iter().map(|a| a.into()).collect();
        self
    }

    pub fn with_confidence(mut self, confidence: f64) -> Self {
        self.routing_confidence = confidence;
        self
    }
}

// ============================================================================
// Routing Statistics
// ============================================================================

/// Statistics for routing between specific handlers
#[derive(Debug, Clone, PartialEq)]
pub struct RoutingStats {
    pub from_handler: String,
    pub to_handler: String,
    pub total_redirects: u32,
    pub successful_redirects: u32,
    pub failed_redirects: u32,
    pub success_rate: f64,
    pub avg_confidence_improvement: f64,
    pub patterns_involved: HashMap<String, u32>,
}

impl RoutingStats {
    pub fn new(from: impl Into<String>, to: impl Into<String>) -> Self {
        Self {
            from_handler: from.into(),
            to_handler: to.into(),
            total_redirects: 0,
            successful_redirects: 0,
            failed_redirects: 0,
            success_rate: 0.0,
            avg_confidence_improvement: 0.0,
            patterns_involved: HashMap::new(),
        }
    }

    pub fn record_redirect(
        &mut self,
        success: bool,
        confidence_improvement: f64,
        pattern: Option<String>,
    ) {
        self.total_redirects += 1;
        if success {
            self.successful_redirects += 1;
        } else {
            self.failed_redirects += 1;
        }

        self.success_rate = self.successful_redirects as f64 / self.total_redirects as f64;

        let current_total = self.avg_confidence_improvement * (self.total_redirects - 1) as f64;
        self.avg_confidence_improvement =
            (current_total + confidence_improvement) / self.total_redirects as f64;

        if let Some(p) = pattern {
            *self.patterns_involved.entry(p).or_insert(0) += 1;
        }
    }

    /// Record from a RedirectResponse
    pub fn record_from_response(&mut self, response: &RedirectResponse) {
        let success = response.routing_confidence > response.request.confidence;
        let improvement = response.routing_confidence - response.request.confidence;
        let pattern = response.request.patterns_matched.first().cloned();
        self.record_redirect(success, improvement, pattern);
    }

    /// Check if routing through this path is advisable
    pub fn is_advisable(&self, min_success_rate: f64) -> bool {
        self.success_rate > min_success_rate && self.avg_confidence_improvement > 0.0
    }

    /// Check if routing should be attempted based on current confidence
    pub fn should_route(&self, current_confidence: f64) -> bool {
        if self.total_redirects == 0 {
            // No history - allow routing if confidence is low
            current_confidence < 0.5
        } else {
            self.is_advisable(0.5)
        }
    }

    /// Get top patterns involved in routing
    pub fn top_patterns(&self, limit: usize) -> Vec<(&String, &u32)> {
        let mut patterns: Vec<_> = self.patterns_involved.iter().collect();
        patterns.sort_by(|a, b| b.1.cmp(a.1));
        patterns.truncate(limit);
        patterns
    }
}

impl Build for RoutingStats {
    fn to_entry(&self) -> Entry {
        let name = format!("{}->{}", self.from_handler, self.to_handler);
        let mut entry = Entry::node("RoutingStats", &name);
        entry.set_attr("from_handler", Entry::string(&self.from_handler));
        entry.set_attr("to_handler", Entry::string(&self.to_handler));
        entry.set_attr("total_redirects", Entry::u32(self.total_redirects));
        entry.set_attr("successful_redirects", Entry::u32(self.successful_redirects));
        entry.set_attr("failed_redirects", Entry::u32(self.failed_redirects));
        entry.set_attr("success_rate", Entry::f64(self.success_rate));
        entry.set_attr("avg_confidence_improvement", Entry::f64(self.avg_confidence_improvement));
        entry
    }

    fn kind(&self) -> &str {
        "RoutingStats"
    }

    fn name(&self) -> Option<&str> {
        Some(&self.from_handler)
    }

    fn category(&self) -> Option<&str> {
        Some("routing")
    }
}

/// Pattern-based routing suggestion
#[derive(Debug, Clone, PartialEq)]
pub struct PatternRoutingSuggestion {
    pub pattern_name: String,
    pub suggested_handler: String,
    pub confidence: f64,
    pub historical_success_rate: f64,
    pub sample_count: u32,
}

// ============================================================================
// Handler State
// ============================================================================

/// State of a handler in the system
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum HandlerState {
    Disabled(Option<String>),
    Enabled(Option<String>),
    Suspended(Option<String>),
    Started(Option<String>),
    Stopped(Option<String>),
    None,
}

impl HandlerState {
    pub fn new() -> Self {
        Self::None
    }

    pub fn enabled(name: impl Into<String>) -> Self {
        Self::Enabled(Some(name.into()))
    }

    pub fn disabled(name: impl Into<String>) -> Self {
        Self::Disabled(Some(name.into()))
    }

    pub fn get_name(&self) -> Option<&String> {
        match self {
            Self::Disabled(n)
            | Self::Enabled(n)
            | Self::Suspended(n)
            | Self::Started(n)
            | Self::Stopped(n) => n.as_ref(),
            Self::None => None,
        }
    }

    pub fn is_enabled(&self) -> bool {
        matches!(self, Self::Enabled(_))
    }
    pub fn is_disabled(&self) -> bool {
        matches!(self, Self::Disabled(_))
    }
    pub fn is_started(&self) -> bool {
        matches!(self, Self::Started(_))
    }
    pub fn is_stopped(&self) -> bool {
        matches!(self, Self::Stopped(_))
    }
    pub fn is_suspended(&self) -> bool {
        matches!(self, Self::Suspended(_))
    }

    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Disabled(_) => "disabled",
            Self::Enabled(_) => "enabled",
            Self::Suspended(_) => "suspended",
            Self::Started(_) => "started",
            Self::Stopped(_) => "stopped",
            Self::None => "none",
        }
    }
}

impl Default for HandlerState {
    fn default() -> Self {
        Self::None
    }
}

// ============================================================================
// Global Statistics
// ============================================================================

/// Global statistics across all handlers
#[derive(Debug, Clone, Default)]
pub struct GlobalHandlerStats {
    pub total_handlers: u32,
    pub total_executions: u32,
    pub total_successes: u32,
    pub total_failures: u32,
    pub overall_success_rate: f64,
    pub average_confidence: f64,
}

impl GlobalHandlerStats {
    /// Aggregate statistics from an iterator of HandlerStats
    pub fn from_iter<'a>(stats: impl Iterator<Item = &'a HandlerStats>) -> Self {
        let mut global = Self::default();
        let mut total_confidence = 0.0;

        for s in stats {
            global.total_handlers += 1;
            global.total_executions += s.total_executions;
            global.total_successes += s.successful;
            global.total_failures += s.failed;
            total_confidence += s.average_confidence * s.total_executions as f64;
        }

        if global.total_executions > 0 {
            global.overall_success_rate =
                global.total_successes as f64 / global.total_executions as f64;
            global.average_confidence = total_confidence / global.total_executions as f64;
        }

        global
    }

    /// Merge another GlobalHandlerStats into this one
    pub fn merge(&mut self, other: &GlobalHandlerStats) {
        let total_execs = self.total_executions + other.total_executions;
        if total_execs > 0 {
            let weighted_conf = self.average_confidence * self.total_executions as f64
                + other.average_confidence * other.total_executions as f64;
            self.average_confidence = weighted_conf / total_execs as f64;
        }

        self.total_handlers += other.total_handlers;
        self.total_executions = total_execs;
        self.total_successes += other.total_successes;
        self.total_failures += other.total_failures;

        if self.total_executions > 0 {
            self.overall_success_rate = self.total_successes as f64 / self.total_executions as f64;
        }
    }

    pub fn display(&self) -> String {
        format!(
            "Global Handler Statistics:\n\
             - Total Handlers: {}\n\
             - Total Executions: {}\n\
             - Successful: {} ({:.2}%)\n\
             - Failed: {} ({:.2}%)\n\
             - Average Confidence: {:.2}",
            self.total_handlers,
            self.total_executions,
            self.total_successes,
            self.overall_success_rate * 100.0,
            self.total_failures,
            (1.0 - self.overall_success_rate) * 100.0,
            self.average_confidence
        )
    }
}

impl Build for GlobalHandlerStats {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("GlobalHandlerStats", "global");
        entry.set_attr("total_handlers", Entry::u32(self.total_handlers));
        entry.set_attr("total_executions", Entry::u32(self.total_executions));
        entry.set_attr("total_successes", Entry::u32(self.total_successes));
        entry.set_attr("total_failures", Entry::u32(self.total_failures));
        entry.set_attr("overall_success_rate", Entry::f64(self.overall_success_rate));
        entry.set_attr("average_confidence", Entry::f64(self.average_confidence));
        entry
    }

    fn kind(&self) -> &str {
        "GlobalHandlerStats"
    }

    fn name(&self) -> Option<&str> {
        Some("global")
    }

    fn category(&self) -> Option<&str> {
        Some("handler")
    }
}

// ============================================================================
// Unified Handler Object
// ============================================================================

/// A unified Handler object containing all data for a single handler.
/// This aligns with the Web database pattern where each entry is self-contained.
#[derive(Debug, Clone)]
pub struct Handler {
    // === Identity ===
    pub name: String,
    pub handler_type: String,
    pub category: String,
    pub state: HandlerState,

    // === Configuration ===
    pub priority: i32,
    pub patterns: Vec<String>,
    pub min_tokens: usize,
    pub max_tokens: Option<usize>,

    // === Statistics (inline, not separate object) ===
    pub total_executions: u32,
    pub successful: u32,
    pub failed: u32,
    pub average_confidence: f64,
    pub average_tokens: f64,
    pub average_time_ms: f64,
    pub patterns_used: HashMap<String, u32>,

    // === Records ===
    pub success_records: Vec<SuccessRecord>,
    pub failure_records: Vec<FailureRecord>,
    pub execution_results: Vec<HandlerResult>,

    // === Pattern Usage ===
    pub pattern_usage: HashMap<String, PatternUsage>,

    // === Code Segments ===
    pub code_segments: Vec<CodeSegment>,

    // === Routing ===
    pub routing_rules: Vec<RoutingRule>,
    pub routing_stats: HashMap<String, RoutingStats>,
}

impl Handler {
    /// Create a new handler with the given name and type
    pub fn new(name: impl Into<String>, handler_type: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            handler_type: handler_type.into(),
            category: "general".to_string(),
            state: HandlerState::None,
            priority: 0,
            patterns: Vec::new(),
            min_tokens: 1,
            max_tokens: None,
            total_executions: 0,
            successful: 0,
            failed: 0,
            average_confidence: 0.0,
            average_tokens: 0.0,
            average_time_ms: 0.0,
            patterns_used: HashMap::new(),
            success_records: Vec::new(),
            failure_records: Vec::new(),
            execution_results: Vec::new(),
            pattern_usage: HashMap::new(),
            code_segments: Vec::new(),
            routing_rules: Vec::new(),
            routing_stats: HashMap::new(),
        }
    }

    // === Builder Methods ===

    pub fn with_priority(mut self, priority: i32) -> Self {
        self.priority = priority;
        self
    }

    pub fn with_patterns<S: Into<String>>(mut self, patterns: Vec<S>) -> Self {
        self.patterns = patterns.into_iter().map(|p| p.into()).collect();
        self
    }

    pub fn with_category<S: Into<String>>(mut self, category: S) -> Self {
        self.category = category.into();
        self
    }

    pub fn with_token_range(mut self, min: usize, max: Option<usize>) -> Self {
        self.min_tokens = min;
        self.max_tokens = max;
        self
    }

    pub fn with_state(mut self, state: HandlerState) -> Self {
        self.state = state;
        self
    }

    pub fn add_pattern<S: Into<String>>(&mut self, pattern: S) {
        self.patterns.push(pattern.into());
    }

    // === Statistics ===

    pub fn success_rate(&self) -> f64 {
        if self.total_executions == 0 {
            0.0
        } else {
            self.successful as f64 / self.total_executions as f64
        }
    }

    pub fn failure_rate(&self) -> f64 {
        1.0 - self.success_rate()
    }

    pub fn top_patterns(&self, limit: usize) -> Vec<(&String, &u32)> {
        let mut patterns: Vec<_> = self.patterns_used.iter().collect();
        patterns.sort_by(|a, b| b.1.cmp(a.1));
        patterns.truncate(limit);
        patterns
    }

    pub fn is_performing_well(&self, min_success_rate: f64, min_executions: u32) -> bool {
        self.total_executions >= min_executions && self.success_rate() >= min_success_rate
    }

    // === Recording ===

    pub fn record_success(
        &mut self,
        confidence: f64,
        tokens: usize,
        time_ms: u64,
        pattern: Option<&str>,
    ) {
        self.total_executions += 1;
        self.successful += 1;

        let n = self.total_executions as f64;
        self.average_confidence = (self.average_confidence * (n - 1.0) + confidence) / n;
        self.average_tokens = (self.average_tokens * (n - 1.0) + tokens as f64) / n;
        self.average_time_ms = (self.average_time_ms * (n - 1.0) + time_ms as f64) / n;

        if let Some(p) = pattern {
            *self.patterns_used.entry(p.to_string()).or_insert(0) += 1;
        }
    }

    pub fn record_failure(&mut self, pattern: Option<&str>) {
        self.total_executions += 1;
        self.failed += 1;

        if let Some(p) = pattern {
            *self.patterns_used.entry(p.to_string()).or_insert(0) += 1;
        }
    }

    pub fn add_success_record(&mut self, record: SuccessRecord) {
        self.record_success(
            record.confidence,
            record.token_count,
            record.time_ms,
            Some(&record.pattern_name),
        );

        // Update pattern usage
        let usage = self
            .pattern_usage
            .entry(record.pattern_name.clone())
            .or_insert_with(|| PatternUsage::new(&record.pattern_name, &self.name));
        usage.record_success(record.confidence, record.segment.clone());

        self.success_records.push(record);
    }

    pub fn add_failure_record(&mut self, record: FailureRecord) {
        self.record_failure(Some(&record.pattern_name));

        // Update pattern usage
        let usage = self
            .pattern_usage
            .entry(record.pattern_name.clone())
            .or_insert_with(|| PatternUsage::new(&record.pattern_name, &self.name));
        usage.record_failure(record.segment.clone());

        self.failure_records.push(record);
    }

    pub fn add_execution_result(&mut self, result: HandlerResult) {
        self.execution_results.push(result);
    }

    pub fn add_code_segment(&mut self, segment: CodeSegment) {
        self.code_segments.push(segment);
    }

    // === Routing ===

    pub fn add_routing_rule(&mut self, rule: RoutingRule) {
        self.routing_rules.push(rule);
    }

    pub fn enabled_routing_rules(&self) -> Vec<&RoutingRule> {
        self.routing_rules.iter().filter(|r| r.enabled).collect()
    }

    pub fn get_routing_stats(&self, to_handler: &str) -> Option<&RoutingStats> {
        self.routing_stats.get(to_handler)
    }

    pub fn record_redirect(&mut self, response: &RedirectResponse) {
        if let Some(ref to_handler) = response.target_handler {
            let stats = self
                .routing_stats
                .entry(to_handler.clone())
                .or_insert_with(|| RoutingStats::new(&self.name, to_handler));
            stats.record_from_response(response);
        }
    }

    pub fn should_route_to(&self, to_handler: &str, confidence: f64) -> bool {
        let has_enabled_rule = self
            .enabled_routing_rules()
            .iter()
            .any(|r| r.to_handler == to_handler);
        if !has_enabled_rule {
            return false;
        }

        match self.get_routing_stats(to_handler) {
            Some(stats) => stats.should_route(confidence),
            None => confidence < 0.5,
        }
    }

    // === State ===

    pub fn enable(&mut self) {
        self.state = HandlerState::Enabled(Some(self.name.clone()));
    }

    pub fn disable(&mut self) {
        self.state = HandlerState::Disabled(Some(self.name.clone()));
    }

    pub fn is_enabled(&self) -> bool {
        self.state.is_enabled()
    }

    // === Utility ===

    pub fn clear_records(&mut self) {
        self.success_records.clear();
        self.failure_records.clear();
        self.execution_results.clear();
    }

    /// Convert to legacy HandlerStats for compatibility
    pub fn to_stats(&self) -> HandlerStats {
        let mut stats = HandlerStats::new(&self.name);
        stats.total_executions = self.total_executions;
        stats.successful = self.successful;
        stats.failed = self.failed;
        stats.average_confidence = self.average_confidence;
        stats.average_tokens = self.average_tokens;
        stats.average_time_ms = self.average_time_ms;
        stats.patterns_used = self.patterns_used.clone();
        stats
    }

    /// Convert to legacy HandlerMetadata for compatibility
    pub fn to_metadata(&self) -> HandlerMetadata {
        HandlerMetadata {
            name: self.name.clone(),
            handler_type: self.handler_type.clone(),
            priority: self.priority,
            patterns: self.patterns.clone(),
            category: self.category.clone(),
            min_tokens: self.min_tokens,
            max_tokens: self.max_tokens,
        }
    }
}

impl Build for Handler {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("Handler", &self.name);
        
        // Identity
        entry.set_attr("handler_type", Entry::string(&self.handler_type));
        entry.set_attr("category", Entry::string(&self.category));
        entry.set_attr("state", Entry::string(self.state.as_str()));

        // Configuration
        entry.set_attr("priority", Entry::i32(self.priority));
        entry.set_attr("min_tokens", Entry::usize(self.min_tokens));
        if let Some(max) = self.max_tokens {
            entry.set_attr("max_tokens", Entry::usize(max));
        }

        // Patterns
        let patterns: Vec<Entry> = self.patterns.iter().map(|p| Entry::string(p)).collect();
        entry.set_attr("patterns", Entry::vec(patterns));

        // Statistics
        entry.set_attr("total_executions", Entry::u32(self.total_executions));
        entry.set_attr("successful", Entry::u32(self.successful));
        entry.set_attr("failed", Entry::u32(self.failed));
        entry.set_attr("success_rate", Entry::f64(self.success_rate()));
        entry.set_attr("average_confidence", Entry::f64(self.average_confidence));
        entry.set_attr("average_tokens", Entry::f64(self.average_tokens));
        entry.set_attr("average_time_ms", Entry::f64(self.average_time_ms));

        // Counts
        entry.set_attr("success_record_count", Entry::usize(self.success_records.len()));
        entry.set_attr("failure_record_count", Entry::usize(self.failure_records.len()));
        entry.set_attr("code_segment_count", Entry::usize(self.code_segments.len()));
        entry.set_attr("routing_rule_count", Entry::usize(self.routing_rules.len()));
        
        entry
    }

    fn kind(&self) -> &str {
        "Handler"
    }

    fn name(&self) -> Option<&str> {
        Some(&self.name)
    }

    fn category(&self) -> Option<&str> {
        Some(&self.category)
    }

    fn priority(&self) -> i16 {
        self.priority as i16
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_handler_result_success() {
        let result = HandlerResult::completed(0..10, "let x: i32 = 5;");
        assert!(result.is_success());
        assert!(!result.is_failure());
        assert_eq!(result.rust_code(), Some("let x: i32 = 5;"));
    }

    #[test]
    fn test_handler_stats() {
        let mut stats = HandlerStats::new("test_handler");
        stats.record_success(0.9, 10, 5, Some("function"));
        stats.record_success(0.8, 15, 3, Some("function"));
        stats.record_failure(Some("variable"));

        assert_eq!(stats.total_executions, 3);
        assert_eq!(stats.successful, 2);
        assert_eq!(stats.failed, 1);
        assert!(stats.success_rate() > 0.6);
    }

    #[test]
    fn test_handler_metadata_build() {
        let meta = HandlerMetadata::new("function_handler", "function")
            .with_priority(100)
            .with_patterns(vec!["function", "function_definition"])
            .with_category("core");

        let entry = meta.to_entry();
        assert!(entry.name().is_some());
    }

    #[test]
    fn test_code_segment() {
        let range = TokenRange::new(0, 5).with_source("test.c");
        let segment = CodeSegment::new(
            vec!["int".to_string(), "x".to_string()],
            "int x;".to_string(),
            range,
        )
        .with_rust_code("let x: i32;")
        .with_type("variable");

        assert_eq!(segment.segment_type, "variable");
        assert_eq!(segment.rust_code, Some("let x: i32;".to_string()));
    }

    #[test]
    fn test_unified_handler() {
        let mut handler = Handler::new("function_handler", "function")
            .with_priority(100)
            .with_patterns(vec![
                "function".to_string(),
                "function_definition".to_string(),
            ])
            .with_category("core")
            .with_token_range(3, Some(100));

        // Test configuration
        assert_eq!(handler.name, "function_handler");
        assert_eq!(handler.handler_type, "function");
        assert_eq!(handler.priority, 100);
        assert_eq!(handler.patterns.len(), 2);
        assert_eq!(handler.min_tokens, 3);

        // Test statistics recording
        handler.record_success(0.9, 10, 5, Some("function"));
        handler.record_success(0.8, 15, 3, Some("function"));
        handler.record_failure(Some("variable"));

        assert_eq!(handler.total_executions, 3);
        assert_eq!(handler.successful, 2);
        assert_eq!(handler.failed, 1);
        assert!(handler.success_rate() > 0.6);

        // Test Build trait
        let entry = handler.to_entry();
        assert_eq!(entry.name(), Some("function_handler"));
        assert_eq!(handler.kind(), "handler");
    }
}
