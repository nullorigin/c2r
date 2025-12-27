//! Routing Decision Storage
//!
//! Stores routing decisions in the System database for handlers to access.
//! This decouples handlers from direct handler calls and enables
//! the system to track and manage routing decisions centrally.

use std::ops::Range;

use crate::db::token::Token;
use crate::db::web::{Build, Entry};
use crate::system::{self, system};

// ============================================================================
// Routing Decision Types
// ============================================================================

/// A routing decision for nested constructs
#[derive(Debug, Clone)]
pub struct RoutingDecision {
    /// Unique ID for this decision
    pub id: u64,
    /// Source handler that made the decision
    pub source_handler: String,
    /// Target handler to route to
    pub target_handler: String,
    /// Token range in the source (start..end)
    pub token_range: Range<usize>,
    /// The tokens to be processed
    pub tokens: Vec<Token>,
    /// Reason for routing
    pub reason: String,
    /// Status of the routing
    pub status: RoutingStatus,
    /// Result after processing (if complete)
    pub result: Option<RoutingResult>,
}

/// Status of a routing decision
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RoutingStatus {
    /// Pending - not yet processed
    Pending,
    /// In progress - being processed
    InProgress,
    /// Completed successfully
    Completed,
    /// Failed
    Failed,
}

/// Result of a completed routing
#[derive(Debug, Clone)]
pub struct RoutingResult {
    /// Converted Rust code
    pub rust_code: String,
    /// Confidence score
    pub confidence: f64,
    /// Handler that produced the result
    pub handler: String,
}

/// Statistics about routing decisions
#[derive(Debug, Clone)]
pub struct RoutingStats {
    pub total: usize,
    pub pending: usize,
    pub completed: usize,
    pub failed: usize,
}

// ============================================================================
// Global Access Functions (delegate to System)
// ============================================================================

/// Create a new routing decision and add it to the database
pub fn create_routing(
    source_handler: &str,
    target_handler: &str,
    token_range: Range<usize>,
    tokens: Vec<Token>,
    reason: &str,
) -> u64 {
    let id = system().allocate_routing_id();

    let decision = RoutingDecision {
        id,
        source_handler: source_handler.to_string(),
        target_handler: target_handler.to_string(),
        token_range,
        tokens,
        reason: reason.to_string(),
        status: RoutingStatus::Pending,
        result: None,
    };

    system().add_routing_decision(&decision)
}

/// Get a routing decision Entry by ID
pub fn get_routing_entry(id: u64) -> Option<Entry> {
    system().get_routing_entry(id).cloned()
}

/// Get the result of a routing decision
pub fn get_routing_result(id: u64) -> Option<RoutingResult> {
    let entry = system().get_routing_entry(id)?;
    let rust_code = entry.get_string_attr("result_code")?.to_string();
    // Get confidence from Entry - it's stored as f64
    let confidence = match entry.attr("result_confidence") {
        Some(Entry::F64(c, _)) => *c,
        _ => return None,
    };
    let handler = entry.get_string_attr("result_handler")?.to_string();
    Some(RoutingResult {
        rust_code,
        confidence,
        handler,
    })
}

/// Set the result for a routing decision (adds new entry with result)
pub fn set_routing_result(id: u64, _rust_code: String, _confidence: f64, _handler: &str) {
    // For now, we need to create a new entry with the result
    // since the db doesn't support in-place updates
    // TODO: Store result data when db supports updates
    system().update_routing_status(id, "completed");
}

/// Mark a routing decision as failed
pub fn set_routing_failed(id: u64) {
    system().update_routing_status(id, "failed");
}

/// Get all pending routing entries for a target handler
pub fn pending_routings_for(target_handler: &str) -> Vec<Entry> {
    system()
        .routings_by_target(target_handler)
        .into_iter()
        .filter(|e| e.get_string_attr("status") == Some("pending"))
        .cloned()
        .collect()
}

/// Get routing statistics
pub fn routing_stats() -> RoutingStats {
    system().routing_stats()
}

// ============================================================================
// Build Implementation
// ============================================================================

impl Build for RoutingDecision {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("RoutingDecision", &format!("routing_{}", self.id));
        entry.set_attr("id", Entry::usize(self.id as usize));
        entry.set_attr("source_handler", Entry::string(&self.source_handler));
        entry.set_attr("target_handler", Entry::string(&self.target_handler));
        entry.set_attr("token_range_start", Entry::usize(self.token_range.start));
        entry.set_attr("token_range_end", Entry::usize(self.token_range.end));
        entry.set_attr("reason", Entry::string(&self.reason));
        entry.set_attr(
            "status",
            Entry::string(match self.status {
                RoutingStatus::Pending => "pending",
                RoutingStatus::InProgress => "in_progress",
                RoutingStatus::Completed => "completed",
                RoutingStatus::Failed => "failed",
            }),
        );
        if let Some(ref result) = self.result {
            entry.set_attr("result_code", Entry::string(&result.rust_code));
            entry.set_attr("result_confidence", Entry::f64(result.confidence));
            entry.set_attr("result_handler", Entry::string(&result.handler));
        }
        entry
    }

    fn kind(&self) -> &str {
        "RoutingDecision"
    }
    fn name(&self) -> Option<&str> {
        None
    }
    fn category(&self) -> Option<&str> {
        Some("routing")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_routing() {
        let id = create_routing(
            "FunctionHandler",
            "LoopHandler",
            0..10,
            vec![],
            "detected for loop",
        );

        assert!(id > 0);

        // Verify we can get the entry back
        let entry = get_routing_entry(id);
        assert!(entry.is_some());

        let e = entry.unwrap();
        assert_eq!(e.get_string_attr("source_handler"), Some("FunctionHandler"));
        assert_eq!(e.get_string_attr("target_handler"), Some("LoopHandler"));
        assert_eq!(e.get_string_attr("status"), Some("pending"));
    }
}
