//! Handler Processing Pipeline
//!
//! Defines the core processing pipeline for handlers:
//! - `ProcessStage`: Current stage in the pipeline
//! - `ProcessDecision`: Decision returned by process() to indicate next action
//! - `Processor`: Trait defining the handler processing pipeline
//! - `ProcessingContext`: Manages handler stack for nested processing

use std::ops::Range;
use crate::db::web::Build;
use crate::db::token::Token;
use crate::db::pattern::Pattern;

// ============================================================================
// Processing Stage Enum
// ============================================================================

/// Current stage in the processing pipeline
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProcessStage {
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

impl ProcessStage {
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
}

// ============================================================================
// Process Decision - Returned by process() to indicate next action
// ============================================================================

/// Decision returned by process() indicating what happened and what to do next
#[derive(Debug, Clone)]
pub enum ProcessDecision {
    /// Continue to next stage
    Continue {
        stage: ProcessStage,
        reason: String,
    },
    /// Processing is complete - store results
    Complete {
        rust_code: String,
        confidence: f64,
    },
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
    Pause {
        stage: ProcessStage,
        reason: String,
    },
    /// Processing failed
    Fail {
        stage: ProcessStage,
        reason: String,
    },
}

// ============================================================================
// Processor Trait - The core processing pipeline
// ============================================================================

/// Trait defining the handler processing pipeline.
/// Each handler must implement this trait to participate in the conversion process.
pub trait Processor: Build {
    /// Get the handler name
    fn name(&self) -> &str;
    
    /// Get supported pattern names for this handler
    fn supported_patterns(&self) -> &[&str];
    
    /// Get the patterns this handler provides
    /// These patterns will be registered in the database for matching
    fn patterns(&self) -> Vec<Pattern>;
    
    /// Validate that the tokens can be handled by this processor.
    /// Returns true if tokens are valid for this handler, false otherwise.
    fn validate(&mut self, tokens: &[Token]) -> bool;
    
    /// Extract structured data from the tokens.
    /// Called after validation passes. Populates internal state with extracted info.
    fn extract(&mut self, tokens: &[Token]) -> bool;
    
    /// Convert the extracted data to Rust code.
    /// Called after extraction. Produces the Rust output.
    fn convert(&mut self) -> Option<String>;
    
    /// Determine if this should be routed to another handler.
    /// Returns Some(handler_name) if routing is needed, None otherwise.
    fn route(&self) -> Option<String> {
        None // Default: no routing
    }
    
    /// Main processing function called iteratively from the processing loop.
    /// Decides whether to proceed to the next stage or return early.
    /// 
    /// This is the entry point called by the main process loop.
    /// Default implementation provides the standard pipeline flow.
    fn process(&mut self, tokens: &[Token]) -> ProcessDecision {
        match self.current_stage() {
            ProcessStage::Pending => {
                if self.validate(tokens) {
                    self.set_stage(ProcessStage::Validated);
                    ProcessDecision::Continue {
                        stage: ProcessStage::Validated,
                        reason: "Validation passed".to_string(),
                    }
                } else {
                    self.set_stage(ProcessStage::Failed);
                    ProcessDecision::Fail {
                        stage: ProcessStage::Pending,
                        reason: "Validation failed - tokens not suitable for this handler".to_string(),
                    }
                }
            }
            ProcessStage::Validated => {
                // Check routing before extraction
                if let Some(target) = self.route() {
                    self.set_stage(ProcessStage::Routed);
                    return ProcessDecision::Route {
                        target_handler: target,
                        reason: "Routing to more specific handler".to_string(),
                    };
                }
                
                if self.extract(tokens) {
                    self.set_stage(ProcessStage::Extracted);
                    ProcessDecision::Continue {
                        stage: ProcessStage::Extracted,
                        reason: "Extraction successful".to_string(),
                    }
                } else {
                    self.set_stage(ProcessStage::Failed);
                    ProcessDecision::Fail {
                        stage: ProcessStage::Validated,
                        reason: "Extraction failed - could not parse token structure".to_string(),
                    }
                }
            }
            ProcessStage::Extracted => {
                if let Some(rust_code) = self.convert() {
                    self.set_stage(ProcessStage::Converted);
                    self.set_output(rust_code.clone());
                    ProcessDecision::Continue {
                        stage: ProcessStage::Converted,
                        reason: "Conversion successful".to_string(),
                    }
                } else {
                    self.set_stage(ProcessStage::Failed);
                    ProcessDecision::Fail {
                        stage: ProcessStage::Extracted,
                        reason: "Conversion failed - could not generate Rust code".to_string(),
                    }
                }
            }
            ProcessStage::Converted => {
                self.set_stage(ProcessStage::Complete);
                ProcessDecision::Complete {
                    rust_code: self.output().unwrap_or_default(),
                    confidence: self.confidence(),
                }
            }
            ProcessStage::Complete => {
                ProcessDecision::Complete {
                    rust_code: self.output().unwrap_or_default(),
                    confidence: self.confidence(),
                }
            }
            ProcessStage::Routed => {
                ProcessDecision::Route {
                    target_handler: self.route().unwrap_or_default(),
                    reason: "Already routed".to_string(),
                }
            }
            ProcessStage::Failed => {
                ProcessDecision::Fail {
                    stage: ProcessStage::Failed,
                    reason: self.error().unwrap_or("Unknown error".to_string()),
                }
            }
        }
    }
    
    // === State accessors (must be implemented) ===
    
    /// Get current processing stage
    fn current_stage(&self) -> ProcessStage;
    
    /// Set processing stage
    fn set_stage(&mut self, stage: ProcessStage);
    
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
