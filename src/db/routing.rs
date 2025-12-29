//! Routing - Unified route registration and lookup
//!
//! Provides a simplified routing mechanism for tracking handler routing decisions.
//! Routes combine both request and result information into a single entity that
//! is stored in the System database for global access.
//!
//! # Usage
//! ```ignore
//! // Register a route
//! system().register_route(Route::new("FunctionHandler", "LoopHandler")
//!     .with_reason("detected for loop")
//!     .with_tokens(tokens));
//!
//! // Lookup routes
//! let routes = system().lookup_route(RouteQuery::by_source("FunctionHandler"));
//! ```

use crate::db::web::{Build, Entry};

// ============================================================================
// Route Status
// ============================================================================

/// Status of a route
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum RouteStatus {
    #[default]
    Pending,
    InProgress,
    Completed,
    Failed,
}

impl RouteStatus {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Pending => "pending",
            Self::InProgress => "in_progress",
            Self::Completed => "completed",
            Self::Failed => "failed",
        }
    }

    pub fn from_str(s: &str) -> Self {
        match s {
            "in_progress" => Self::InProgress,
            "completed" => Self::Completed,
            "failed" => Self::Failed,
            _ => Self::Pending,
        }
    }
}

// ============================================================================
// Route - Unified request + result
// ============================================================================

/// A unified route that combines request and result information.
/// This is the primary type for all routing operations.
#[derive(Debug, Clone)]
pub struct Route {
    /// Unique ID (assigned on registration)
    pub id: u64,
    /// Source handler that initiated the route
    pub source: String,
    /// Target handler to process the route
    pub target: String,
    /// Reason for the routing decision
    pub reason: String,
    /// Current status
    pub status: RouteStatus,
    /// Token range (start, end) if applicable
    pub token_range: Option<(usize, usize)>,
    /// Result code (if completed)
    pub result_code: Option<String>,
    /// Confidence score (if completed)
    pub confidence: Option<f64>,
}

impl Route {
    /// Create a new route from source to target handler
    pub fn new(source: &str, target: &str) -> Self {
        Self {
            id: 0, // Assigned on registration
            source: source.to_string(),
            target: target.to_string(),
            reason: String::new(),
            status: RouteStatus::Pending,
            token_range: None,
            result_code: None,
            confidence: None,
        }
    }

    /// Add a reason for the routing decision
    pub fn with_reason(mut self, reason: &str) -> Self {
        self.reason = reason.to_string();
        self
    }

    /// Add token range information
    pub fn with_range(mut self, start: usize, end: usize) -> Self {
        self.token_range = Some((start, end));
        self
    }

    /// Set the status
    pub fn with_status(mut self, status: RouteStatus) -> Self {
        self.status = status;
        self
    }

    /// Set the result (marks as completed)
    pub fn with_result(mut self, code: String, confidence: f64) -> Self {
        self.result_code = Some(code);
        self.confidence = Some(confidence);
        self.status = RouteStatus::Completed;
        self
    }

    /// Mark as failed
    pub fn failed(mut self) -> Self {
        self.status = RouteStatus::Failed;
        self
    }

    /// Check if route is complete
    pub fn is_complete(&self) -> bool {
        self.status == RouteStatus::Completed
    }

    /// Check if route failed
    pub fn is_failed(&self) -> bool {
        self.status == RouteStatus::Failed
    }

    /// Get the result code if available
    pub fn code(&self) -> Option<&str> {
        self.result_code.as_deref()
    }
}

impl Default for Route {
    fn default() -> Self {
        Self::new("", "")
    }
}

// ============================================================================
// Build Implementation
// ============================================================================

impl Build for Route {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("Route", &format!("route_{}", self.id));
        entry.set_attr("id", Entry::usize(self.id as usize));
        entry.set_attr("source", Entry::string(&self.source));
        entry.set_attr("target", Entry::string(&self.target));
        entry.set_attr("reason", Entry::string(&self.reason));
        entry.set_attr("status", Entry::string(self.status.as_str()));
        
        if let Some((start, end)) = self.token_range {
            entry.set_attr("range_start", Entry::usize(start));
            entry.set_attr("range_end", Entry::usize(end));
        }
        if let Some(ref code) = self.result_code {
            entry.set_attr("result_code", Entry::string(code));
        }
        if let Some(conf) = self.confidence {
            entry.set_attr("confidence", Entry::f64(conf));
        }
        entry
    }

    fn kind(&self) -> &str { "Route" }
    fn name(&self) -> Option<&str> { None }
    fn category(&self) -> Option<&str> { Some("routing") }
}

// ============================================================================
// Route Query
// ============================================================================

/// Query parameters for looking up routes
#[derive(Debug, Clone, Default)]
pub struct RouteQuery {
    pub id: Option<u64>,
    pub source: Option<String>,
    pub target: Option<String>,
    pub status: Option<RouteStatus>,
}

impl RouteQuery {
    /// Create empty query (matches all)
    pub fn all() -> Self { Self::default() }

    /// Query by ID
    pub fn by_id(id: u64) -> Self {
        Self { id: Some(id), ..Default::default() }
    }

    /// Query by source handler
    pub fn by_source(source: &str) -> Self {
        Self { source: Some(source.to_string()), ..Default::default() }
    }

    /// Query by target handler
    pub fn by_target(target: &str) -> Self {
        Self { target: Some(target.to_string()), ..Default::default() }
    }

    /// Query by status
    pub fn by_status(status: RouteStatus) -> Self {
        Self { status: Some(status), ..Default::default() }
    }

    /// Add source filter
    pub fn with_source(mut self, source: &str) -> Self {
        self.source = Some(source.to_string());
        self
    }

    /// Add target filter
    pub fn with_target(mut self, target: &str) -> Self {
        self.target = Some(target.to_string());
        self
    }

    /// Add status filter
    pub fn with_status(mut self, status: RouteStatus) -> Self {
        self.status = Some(status);
        self
    }

    /// Check if an entry matches this query
    pub fn matches(&self, entry: &Entry) -> bool {
        if let Some(id) = self.id {
            if entry.get_usize_attr("id") != Some(id as usize) { return false; }
        }
        if let Some(ref source) = self.source {
            if entry.get_string_attr("source") != Some(source.as_str()) { return false; }
        }
        if let Some(ref target) = self.target {
            if entry.get_string_attr("target") != Some(target.as_str()) { return false; }
        }
        if let Some(status) = self.status {
            if entry.get_string_attr("status") != Some(status.as_str()) { return false; }
        }
        true
    }
}

// ============================================================================
// Route Statistics
// ============================================================================

/// Statistics about routes in the system
#[derive(Debug, Clone, Default)]
pub struct RouteStats {
    pub total: usize,
    pub pending: usize,
    pub in_progress: usize,
    pub completed: usize,
    pub failed: usize,
}

impl RouteStats {
    /// Calculate stats from entries
    pub fn from_entries(entries: &[&Entry]) -> Self {
        let mut stats = Self { total: entries.len(), ..Default::default() };
        for entry in entries {
            match entry.get_string_attr("status") {
                Some("pending") => stats.pending += 1,
                Some("in_progress") => stats.in_progress += 1,
                Some("completed") => stats.completed += 1,
                Some("failed") => stats.failed += 1,
                _ => {}
            }
        }
        stats
    }
}

// ============================================================================
// Conversion from Entry
// ============================================================================

impl Route {
    /// Reconstruct a Route from a database Entry
    pub fn from_entry(entry: &Entry) -> Option<Self> {
        Some(Self {
            id: entry.get_usize_attr("id")? as u64,
            source: entry.get_string_attr("source")?.to_string(),
            target: entry.get_string_attr("target")?.to_string(),
            reason: entry.get_string_attr("reason").unwrap_or("").to_string(),
            status: RouteStatus::from_str(entry.get_string_attr("status").unwrap_or("pending")),
            token_range: match (entry.get_usize_attr("range_start"), entry.get_usize_attr("range_end")) {
                (Some(s), Some(e)) => Some((s, e)),
                _ => None,
            },
            result_code: entry.get_string_attr("result_code").map(String::from),
            confidence: match entry.attr("confidence") {
                Some(Entry::F64(c, _)) => Some(*c),
                _ => None,
            },
        })
    }
}

// ============================================================================
// Compatibility Functions (for existing code)
// ============================================================================

use crate::db::token::Token;

/// Create a new route (compatibility wrapper)
pub fn create_routing(
    source_handler: &str,
    target_handler: &str,
    token_range: std::ops::Range<usize>,
    _tokens: Vec<Token>,
    reason: &str,
) -> u64 {
    crate::system::system().register_route(
        Route::new(source_handler, target_handler)
            .with_reason(reason)
            .with_range(token_range.start, token_range.end)
    )
}

/// Set the result for a route (compatibility wrapper)
pub fn set_routing_result(id: u64, rust_code: String, confidence: f64, _handler: &str) {
    if let Some(mut route) = crate::system::system().lookup_route_by_id(id) {
        route.result_code = Some(rust_code);
        route.confidence = Some(confidence);
        route.status = RouteStatus::Completed;
        crate::system::system().update_route(&route);
    }
}

/// Mark a route as failed (compatibility wrapper)
pub fn set_routing_failed(id: u64) {
    if let Some(mut route) = crate::system::system().lookup_route_by_id(id) {
        route.status = RouteStatus::Failed;
        crate::system::system().update_route(&route);
    }
}

/// Get route statistics (compatibility wrapper)  
pub fn routing_stats() -> RouteStats {
    crate::system::system().route_stats()
}

// Legacy type aliases for backward compatibility
pub type RoutingStats = RouteStats;
pub type RoutingStatus = RouteStatus;

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_route_builder() {
        let route = Route::new("FunctionHandler", "LoopHandler")
            .with_reason("detected for loop")
            .with_range(0, 10);

        assert_eq!(route.source, "FunctionHandler");
        assert_eq!(route.target, "LoopHandler");
        assert_eq!(route.reason, "detected for loop");
        assert_eq!(route.token_range, Some((0, 10)));
        assert_eq!(route.status, RouteStatus::Pending);
    }

    #[test]
    fn test_route_with_result() {
        let route = Route::new("A", "B")
            .with_result("fn test() {}".to_string(), 0.95);

        assert!(route.is_complete());
        assert_eq!(route.code(), Some("fn test() {}"));
        assert_eq!(route.confidence, Some(0.95));
    }

    #[test]
    fn test_route_to_entry() {
        let route = Route::new("Source", "Target")
            .with_reason("test reason")
            .with_range(5, 15);

        let entry = route.to_entry();
        assert_eq!(entry.get_string_attr("source"), Some("Source"));
        assert_eq!(entry.get_string_attr("target"), Some("Target"));
        assert_eq!(entry.get_string_attr("reason"), Some("test reason"));
        assert_eq!(entry.get_usize_attr("range_start"), Some(5));
        assert_eq!(entry.get_usize_attr("range_end"), Some(15));
    }

    #[test]
    fn test_route_query() {
        let route = Route::new("A", "B").with_status(RouteStatus::Completed);
        let entry = route.to_entry();

        assert!(RouteQuery::all().matches(&entry));
        assert!(RouteQuery::by_source("A").matches(&entry));
        assert!(RouteQuery::by_target("B").matches(&entry));
        assert!(RouteQuery::by_status(RouteStatus::Completed).matches(&entry));
        assert!(!RouteQuery::by_source("C").matches(&entry));
    }
}
