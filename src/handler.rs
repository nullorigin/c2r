//! Trait-based handler system for C to Rust conversion
//!
//! This module defines the Handler trait and supporting types for processing C tokens
//! and converting them to Rust code. Each handler specializes in specific C constructs
//! like functions, arrays, structs, etc.

use crate::{
    Context, ConvertedElement, ElementInfo, ExtractedElement, HandlerPattern, HandlerPhase, HandlerRedirect, HandlerReport, Id, PatternResult, ReportLevel, Result, Token, TokenBox, report
};
use std::collections::{HashMap, VecDeque, HashSet};
use std::fmt::Display;
use std::hash::{Hash, Hasher};
use std::sync::{Arc, RwLock, atomic::{AtomicUsize, Ordering}};
use std::ops::Range;
use std::time::SystemTime;

// ============================================================================
// REDIRECT SYSTEM TYPES (integrated from redirect.rs)
// ============================================================================

/// Redirect request from a handler
#[derive(Debug, Clone)]
pub struct RedirectRequest {
    pub from_handler: Id,
    pub token_range: Range<usize>,
    pub failed_patterns: Vec<String>,
    pub suggested_handler: Option<Id>,
    pub metadata: Vec<(String, String)>,
}

/// Redirect response with routing decision
#[derive(Debug, Clone)]
pub struct RedirectResponse {
    pub target_handler: Option<Id>,
    pub modified_range: Option<Range<usize>>,
    pub routing_reason: String,
    pub should_retry: bool,
    pub metadata: Vec<(String, String)>,
}

impl Default for RedirectResponse {
    fn default() -> Self {
        Self {
            target_handler: None,
            modified_range: None,
            routing_reason: "No routing reason".to_string(),
            should_retry: false,
            metadata: Vec::new(),
        }
    }
}

// ============================================================================
// ADAPTIVE PATTERN MATCHING TYPES (integrated from adaptive.rs)
// ============================================================================

/// Adaptive pattern matching result (extends existing PatternResult)
#[derive(Debug, Clone)]
pub struct AdaptivePatternResult {
    /// Whether the pattern matched
    pub matched: bool,
    /// Confidence score (0.0 to 1.0)
    pub confidence: f64,
    /// Number of tokens consumed
    pub tokens_consumed: usize,
    /// Additional metadata for adaptive scoring
    pub match_data: HashMap<String, String>,
    /// The underlying pattern result from the existing system
    pub base_result: PatternResult,
}

/// Maximum deviation allowed from original pattern (bounds checking)
const MAX_DEVIATION_THRESHOLD: f64 = 0.3; // 30% maximum deviation
const MIN_CONFIDENCE_THRESHOLD: f64 = 0.1; // Minimum confidence to continue exploration
const EXPLORATION_DEPTH: usize = 5; // Maximum depth of pattern tree exploration

/// A node in the adaptive pattern exploration tree
#[derive(Debug, Clone, PartialEq)]
pub struct PatternNode {
    /// Original base pattern
    pub base_pattern: HandlerPattern,
    /// Current pattern variation
    pub current_pattern: HandlerPattern,
    /// Confidence score for this variation
    pub confidence: f64,
    /// Deviation distance from base pattern (0.0 = exact match, 1.0 = maximum deviation)
    pub deviation_distance: f64,
    /// Success rate of this pattern variation
    pub success_rate: f64,
    /// Number of times this variation was tried
    pub attempts: usize,
    /// Child variations branching from this node
    pub children: Vec<PatternNode>,
    /// Parent node (for backtracking)
    pub parent_id: Option<Id>,
    /// Unique identifier for this node
    pub node_id: Id,
}

/// Probabilistic bounds for pattern exploration
#[derive(Debug, Clone, PartialEq)]
pub struct ExplorationBounds {
    /// Maximum allowed deviation from base pattern
    pub max_deviation: f64,
    /// Minimum confidence to continue exploration
    pub min_confidence: f64,
    /// Maximum exploration depth
    pub max_depth: usize,
    /// Exploration radius (how far to branch out)
    pub exploration_radius: f64,
}

/// Fuzzy logic scoring system for pattern variations
#[derive(Debug, Clone, PartialEq)]
pub struct FuzzyScorer {
    /// Weight for pattern similarity (higher = prefer similar patterns)
    pub similarity_weight: f64,
    /// Weight for historical success rate
    pub success_weight: f64,
    /// Weight for confidence score
    pub confidence_weight: f64,
    /// Weight for deviation penalty (higher = penalize deviation more)
    pub deviation_penalty: f64,
}

/// Statistics for the adaptive system
#[derive(Debug)]
pub struct AdaptiveStats {
    pub total_explorations: usize,
    pub successful_adaptations: usize,
    pub failed_explorations: usize,
    pub average_deviation: f64,
    pub best_success_rate: f64,
    pub total_attempts: AtomicUsize,
    pub successes: AtomicUsize,
    pub failures: AtomicUsize,
}

/// Record of a handler failure for learning
#[derive(Debug, Clone)]
pub struct FailureRecord {
    pub handler_id: Id,
    pub token_count: usize,
    pub range_size: usize,
    pub reason: String,
    pub timestamp: SystemTime,
}

/// Record of a handler success for learning
#[derive(Debug, Clone)]
pub struct SuccessRecord {
    pub handler_id: Id,
    pub token_count: usize,
    pub range_size: usize,
    pub confidence: f64,
    pub timestamp: SystemTime,
}

// ============================================================================
// TRAIT IMPLEMENTATIONS FOR ADAPTIVE TYPES
// ============================================================================

impl Default for AdaptiveStats {
    fn default() -> Self {
        Self {
            total_explorations: 0,
            successful_adaptations: 0,
            failed_explorations: 0,
            average_deviation: 0.0,
            best_success_rate: 0.0,
            total_attempts: AtomicUsize::new(0),
            successes: AtomicUsize::new(0),
            failures: AtomicUsize::new(0),
        }
    }
}

impl Default for FuzzyScorer {
    fn default() -> Self {
        Self {
            similarity_weight: 0.4,
            success_weight: 0.3,
            confidence_weight: 0.2,
            deviation_penalty: 0.1,
        }
    }
}

impl Default for ExplorationBounds {
    fn default() -> Self {
        Self {
            max_deviation: MAX_DEVIATION_THRESHOLD,
            min_confidence: MIN_CONFIDENCE_THRESHOLD,
            max_depth: EXPLORATION_DEPTH,
            exploration_radius: 0.5,
        }
    }
}

impl AdaptivePatternResult {
    /// Create a "no match" result
    pub fn no_match() -> Self {
        Self {
            matched: false,
            confidence: 0.0,
            tokens_consumed: 0,
            match_data: HashMap::new(),
            base_result: PatternResult::NoMatch { 
                reason: "No adaptive match found".to_string() 
            },
        }
    }

    /// Convert from existing PatternResult
    pub fn from_existing(result: PatternResult) -> Self {
        let (matched, confidence, tokens_consumed) = match &result {
            PatternResult::Match { consumed_tokens } => (true, 1.0, *consumed_tokens),
            PatternResult::CountOf { offsets } => (true, 0.8, offsets.len()),
            PatternResult::Sequence { range } => (true, 0.7, range.len()),
            PatternResult::Fuzzy { offsets } => (true, 0.5, offsets.len()),
            PatternResult::NoMatch { .. } => (false, 0.0, 0),
            PatternResult::CachedPositive { pattern_id, cache_hit_count, reason } => (true, 0.9, *cache_hit_count),
            PatternResult::CachedNegative { pattern_id, cache_hit_count, reason } => (false, 0.0, *cache_hit_count),
            PatternResult::Reject { reason } => (false, 0.0, 0),
            PatternResult::TypeMismatch { expected_type, actual_type, position, reason } => (false, 0.0, 0),
            PatternResult::ValueMismatch { expected_value, actual_value, position, reason } => (false, 0.0, 0),
            PatternResult::StructureMismatch { expected_pattern, actual_structure, reason } => (false, 0.0, 0),
        };

        Self {
            matched,
            confidence,
            tokens_consumed,
            match_data: HashMap::new(),
            base_result: result,
        }
    }
}

impl PatternNode {
    /// Create a new root pattern node
    pub fn new_root(base_pattern: HandlerPattern) -> Self {
        Self {
            current_pattern: base_pattern.clone(),
            base_pattern,
            confidence: 1.0,
            deviation_distance: 0.0,
            success_rate: 0.0,
            attempts: 0,
            children: Vec::new(),
            parent_id: None,
            node_id: Id::get(&Id::gen_name("pattern_node")),
        }
    }

    /// Create a child variation of this pattern node
    pub fn create_variation(&self, new_pattern: HandlerPattern, deviation: f64) -> Self {
        Self {
            base_pattern: self.base_pattern.clone(),
            current_pattern: new_pattern,
            confidence: self.confidence * (1.0 - deviation), // Reduce confidence based on deviation
            deviation_distance: deviation,
            success_rate: 0.0,
            attempts: 0,
            children: Vec::new(),
            parent_id: Some(self.node_id.clone()),
            node_id: Id::get(&Id::gen_name("pattern_variation")),
        }
    }

    /// Update success statistics for this node
    pub fn update_success(&mut self, success: bool) {
        self.attempts += 1;
        let current_successes = (self.success_rate * (self.attempts - 1) as f64) as usize;
        let new_successes = current_successes + if success { 1 } else { 0 };
        self.success_rate = new_successes as f64 / self.attempts as f64;
        
        // Update confidence based on success rate
        self.confidence = self.success_rate * (1.0 - self.deviation_distance);
    }
}

// ============================================================================
// HANDLER SYSTEM TYPES
// ============================================================================

/// Result of handler processing operations
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum HandlerResult {
    /// Handler successfully processed the tokens
    Handled(Option<Vec<Token>>, Range<usize>, Id),
    /// Handler processed and generated code
    Processed(Option<Vec<Token>>, Range<usize>, String, Id),
    /// Handler completed processing successfully
    Completed(Option<Vec<Token>>, Range<usize>, String, Id),
    /// Handler could not process the tokens
    NotHandled(Option<Vec<Token>>, Range<usize>, Id),
    /// Request was redirected to another handler
    Redirected(Option<Vec<Token>>, Range<usize>, String, Id, Id),
    /// Handler extracted an element
    Extracted(ExtractedElement, Range<usize>, String, Id),
    /// Handler converted an element
    Converted(ConvertedElement, Range<usize>, String, Id),
}

/// Core trait that all handlers must implement
///
/// This trait defines the complete lifecycle of token processing:
/// - Pattern detection and processing
/// - Token handling and parsing
/// - Element extraction from tokens
/// - Conversion to Rust code
/// - Documentation generation
/// - Progress reporting
/// - Result processing and redirects
pub trait Handler: Send + Sync + 'static {
    /// Constant functions for handler metadata - zero-cost abstraction
    fn id(&self) -> Id;
    fn role(&self) -> String;
    fn priority(&self) -> u64;
    fn supported_patterns(&self) -> Vec<String>;
    /// Pattern-based processing: Determine if this handler can process the token range
    /// Uses Patternizer and Samplizer for intelligent detection
    ///
    /// # Parameters
    /// - `token_slot`: The TokenSlot number containing the tokens
    /// - `token_range`: The range within the tokens to process
    fn can_process(
        &self,
        context: &mut Context,
        token_slot: usize,
        token_range: Range<usize>,
    ) -> Result<bool>;

    /// Main processing: Parse and understand the token structure
    /// Core processing method - must be implemented by all handlers
    ///
    /// # Parameters
    /// - `token_slot`: The TokenSlot number containing the tokens
    /// - `token_range`: The range within the tokens to process
    fn process(
        &self,
        context: &mut Context,
        token_slot: usize,
        token_range: Range<usize>,
    ) -> Result<HandlerResult>;

    /// Extract structured data from processed tokens
    /// Optional: Only implement if handler supports extraction
    ///
    /// # Parameters
    /// - `token_slot`: The TokenSlot number containing the tokens
    /// - `token_range`: The range within the tokens to process
    fn extract(
        &self,
        context: &mut Context,
        token_slot: usize,
        token_range: Range<usize>,
    ) -> Result<Option<ExtractedElement>> {
        Ok(None)
    }

    /// Convert extracted elements to Rust code
    /// Optional: Only implement if handler supports conversion
    ///
    /// # Parameters
    /// - `token_slot`: The TokenSlot number containing the tokens
    /// - `token_range`: The range within the tokens to process
    fn convert(
        &self,
        context: &mut Context,
        token_slot: usize,
        token_range: Range<usize>,
    ) -> Result<Option<ConvertedElement>> {
        Ok(None)
    }

    /// Generate documentation for processed elements
    /// Optional: Only implement if handler supports documentation
    fn document(&self, context: &mut Context, info: ElementInfo) -> Result<Option<String>> {
        Ok(None)
    }

    /// Generate handler-specific reports
    /// Optional: Return handler processing statistics and debug info
    fn report(&self, context: &mut Context) -> Result<HandlerReport> {
        Ok(HandlerReport::new(
            "default_handler_report",
            std::sync::Arc::new(Id::get("default")),
            "Handler".to_string(),
            "default_function".to_string(),
            "Default handler report".to_string(),
            crate::ReportLevel::Info,
            crate::HandlerPhase::Process,
        ))
    }

    /// Post-process results from other handlers
    /// Optional: Used for result transformation and finalization
    fn process_result(
        &self,
        context: &mut Context,
        token_slot: usize,
        token_range: Range<usize>,
        result: HandlerResult,
    ) -> Result<HandlerResult> {
        Ok(result)
    }

    /// Handle redirect requests from failed pattern matching
    /// Optional: Route tokens to more appropriate handlers
    fn handle_redirect(
        &self,
        context: &mut Context,
        token_slot: usize,
        token_range: Range<usize>,
        result: HandlerResult,
    ) -> Result<HandlerResult> {
        Ok(result)
    }
}
impl Hash for dyn Handler {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id().hash(state);
    }
}
/// Example of how handlers should implement the constant function approach:
///
/// ```rust
/// #[derive(Debug)]
/// pub struct FunctionHandler;
///
/// impl Handler for FunctionHandler {
///     fn handler_id() -> Id { Id::get("function_handler") }
///     fn role() -> &'static str { "function" }
///     fn priority() -> u64 { 200 }
///     fn supported_patterns() -> &'static [&'static str] {
///         &["function_declaration", "function_definition"]
///     }
///     
///     fn can_process(&self, token_slot: usize, token_range: Range<usize>) -> Result<bool> {
///         Context::tokenizer(|tokenizer| {
///             let tokens = tokenizer.current_slot().tokens();
///             let filtered_tokens = crate::handlers::common::filter_tokens_for_handler(&tokens, token_range);
///             crate::handlers::common::match_patterns_with_context(&filtered_tokens, Self::supported_patterns())
///         }).unwrap_or(false)
///     }
///
///     fn process(&self, token_slot: usize, token_range: Range<usize>) -> Result<HandlerResult> {
///         // Handler-specific implementation
///         Ok(HandlerResult::NotHandled(None, token_range, Self::handler_id()))
///     }
/// }
/// ```

/// Trait for handlers that can be dynamically dispatched
pub trait DynHandler: Handler {
    fn as_handler(&self) -> &dyn Handler;
}

impl<T: Handler> DynHandler for T {
    fn as_handler(&self) -> &dyn Handler {
        self
    }
}

/// Handler registry for trait-based system with integrated adaptive analysis
pub struct Handlizer {
    pub handlers: HashMap<Id, Arc<dyn Handler>>,
    sorted: bool,
    
    // Integrated adaptive pattern matching fields
    /// Root patterns organized by handler type  
    pub pattern_trees: HashMap<String, Vec<PatternNode>>,
    /// Fuzzy scoring system
    pub fuzzy_scorer: FuzzyScorer,
    /// Exploration bounds and constraints
    pub bounds: ExplorationBounds,
    /// Cache for explored variations
    pub exploration_cache: HashMap<String, PatternNode>,
    /// Statistics for analysis
    pub adaptive_stats: AdaptiveStats,
    /// Failure patterns for learning
    pub failure_patterns: Arc<RwLock<HashMap<String, Vec<FailureRecord>>>>,
    /// Success patterns for learning (integrated from Adaptivizer)
    pub success_patterns: Arc<RwLock<HashMap<String, Vec<SuccessRecord>>>>,
    /// Visited handlers to prevent circular redirects
    pub visited_handlers: HashSet<Id>,
    /// Current redirect depth
    pub redirect_depth: usize,
    /// Maximum allowed redirect depth
    pub max_redirect_depth: usize,
    /// Cache of failed patterns per handler
    pub pattern_cache: HashMap<String, Vec<String>>,
    /// Routing rules based on patterns
    pub routing_rules: HashMap<String, Vec<String>>,
}
impl Clone for Handlizer {
    fn clone(&self) -> Self {
        Self {
            handlers: self.handlers.clone(),
            sorted: self.sorted,
            pattern_trees: self.pattern_trees.clone(),
            fuzzy_scorer: self.fuzzy_scorer.clone(),
            bounds: self.bounds.clone(),
            exploration_cache: self.exploration_cache.clone(),
            adaptive_stats: AdaptiveStats::default(), // Create fresh stats for clone
            failure_patterns: Arc::new(RwLock::new(HashMap::new())),
            success_patterns: Arc::new(RwLock::new(HashMap::new())),
            // Redirect fields
            visited_handlers: HashSet::new(),
            redirect_depth: 0,
            max_redirect_depth: 5,
            pattern_cache: HashMap::new(),
            routing_rules: self.routing_rules.clone(),
        }
    }
}
impl Hash for Handlizer {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.handlers.iter().for_each(|(id, handler)| {
            id.hash(state);
            handler.hash(state);
        });
        self.sorted.hash(state);
    }
}
impl PartialEq for Handlizer {
    fn eq(&self, other: &Self) -> bool {
        self.handlers
            .iter()
            .map(|(id, _)| id)
            .eq(other.handlers.iter().map(|(id, _)| id));
        self.sorted == other.sorted
    }
}
impl Eq for Handlizer {}
impl core::ops::Deref for Handlizer {
    type Target = HashMap<Id, Arc<dyn Handler>>;

    fn deref(&self) -> &Self::Target {
        &self.handlers
    }
}

impl std::fmt::Debug for Handlizer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Handlizer")
            .field("handlers", &format!("{} handlers", self.handlers.len()))
            .field("sorted", &self.sorted)
            .finish()
    }
}
impl AsRef<HashMap<Id, Arc<dyn Handler>>> for Handlizer {
    fn as_ref(&self) -> &HashMap<Id, Arc<dyn Handler>> {
        &self.handlers
    }
}
impl AsMut<HashMap<Id, Arc<dyn Handler>>> for Handlizer {
    fn as_mut(&mut self) -> &mut HashMap<Id, Arc<dyn Handler>> {
        &mut self.handlers
    }
}
impl Handlizer {
    pub fn new() -> Self {
        // Initialize pattern-based routing rules
        let mut routing_rules = HashMap::new();
        routing_rules.insert(
            "function_handler".to_string(),
            vec!["expression_handler".to_string(), "global_handler".to_string()],
        );
        routing_rules.insert(
            "array_handler".to_string(),
            vec!["global_handler".to_string()],
        );
        routing_rules.insert(
            "enum_handler".to_string(),
            vec!["typedef_handler".to_string()],
        );
        routing_rules.insert(
            "struct_handler".to_string(),
            vec!["typedef_handler".to_string()],
        );
        routing_rules.insert(
            "typedef_handler".to_string(),
            vec!["struct_handler".to_string(), "enum_handler".to_string()],
        );
        routing_rules.insert(
            "macro_handler".to_string(),
            vec!["include_handler".to_string()],
        );
        routing_rules.insert(
            "global_handler".to_string(),
            vec!["function_handler".to_string(), "array_handler".to_string()],
        );
        routing_rules.insert(
            "expression_handler".to_string(),
            vec!["function_handler".to_string()],
        );
        
        Self {
            handlers: HashMap::new(),
            sorted: false,
            
            // Initialize adaptive fields
            pattern_trees: HashMap::new(),
            fuzzy_scorer: FuzzyScorer::default(),
            bounds: ExplorationBounds::default(),
            exploration_cache: HashMap::new(),
            adaptive_stats: AdaptiveStats::default(),
            failure_patterns: Arc::new(RwLock::new(HashMap::new())),
            success_patterns: Arc::new(RwLock::new(HashMap::new())),
            
            // Initialize redirect fields
            visited_handlers: HashSet::new(),
            redirect_depth: 0,
            max_redirect_depth: 5, // Prevent infinite redirect loops
            pattern_cache: HashMap::new(),
            routing_rules,
        }
    }
    pub fn find_handler(&self, id: Id) -> &Arc<dyn Handler> {
        self.handlers.get(&id).unwrap()
    }
    pub fn find_by_id(&self, id: Id) -> Option<&Arc<dyn Handler>> {
        self.handlers.get(&id)
    }
    /// Register a new handler
    pub fn register<H: Handler>(&mut self, handler: H) {
        self.handlers.insert(handler.id(), Arc::new(handler));
        self.sorted = false;
    }

    /// Get all handlers sorted by priority
    pub fn get_sorted_handlers(&self) -> Vec<&Arc<dyn Handler>> {
        let mut handlers: Vec<&Arc<dyn Handler>> =
            self.handlers.iter().map(|(_, handler)| handler).collect();

        // Sort by priority (highest first)
        handlers.sort_by(|a, b| b.priority().cmp(&a.priority()));
        handlers
    }

    /// Find handlers that support a specific pattern
    pub fn find_by_pattern(&self, pattern: &str) -> Vec<&Arc<dyn Handler>> {
        self.handlers
            .values()
            .filter(|handler| handler.supported_patterns().contains(&pattern.to_string()))
            .collect()
    }

    /// Process tokens through all capable handlers with adaptive learning and comprehensive reporting


    /// Process tokens intelligently with the best available handler
    pub fn process_single(
        &mut self,
        context: &mut Context,
        token_slot: usize,
        token_range: Range<usize>,
    ) -> Result<HandlerResult> {
        let start_time = std::time::Instant::now();
        
        // Check if we have any tokens in the range
        if token_range.is_empty() {
            return Err(crate::error::C2RError::new(
                crate::error::Kind::Logic,
                crate::error::Reason::Range("Invalid token range for processing"),
                Some("Token range is empty".to_string()),
            ));
        }

        let tokens = context.tokenizer.get_tokens(token_slot, token_range.clone());

        context.registry.add_report(HandlerReport::new(
            &format!("process_single_start_{}", token_slot),
            std::sync::Arc::new(Id::get("handlizer")),
            "Handlizer".to_string(),
            "process_single".to_string(),
            format!("Starting intelligent single handler processing for {} tokens", tokens.len()),
            ReportLevel::Info,
            HandlerPhase::Process,
        ).with_tokens(tokens.len(), 0));

        // Enhanced pattern analysis with multi-criteria selection
        let mut best_handler: Option<&Arc<dyn Handler>> = None;
        let mut best_score = 0.0f64;
        let mut handler_scores = Vec::new();
        let mut fallback_handlers = Vec::new();

        // Multi-pass handler evaluation with advanced scoring
        for handler in self.get_sorted_handlers() {
            match handler.can_process(context, token_slot, token_range.clone()) {
                Ok(can_handle) if can_handle => {
                    let pattern_match_score = Handlizer::calculate_pattern_match_score(handler.as_ref(), &tokens, &token_range);
                    let complexity_score = self.calculate_token_complexity(&tokens);
                    
                    // Weighted composite score
                    let composite_score = (handler.priority() as f64 / 1000.0 * 0.4) + 
                                        (pattern_match_score * 0.4) + 
                                        (complexity_score * 0.2);
                    
                    handler_scores.push((handler, composite_score));
                    
                    // Track potential fallback handlers
                    if composite_score > 0.3 {
                        fallback_handlers.push((handler, composite_score));
                    }
                    
                    if composite_score > best_score {
                        best_score = composite_score;
                        best_handler = Some(handler);
                    }

                    context.registry.add_report(HandlerReport::new(
                        &format!("handler_assessment_{}_{}", handler.id().name(), token_slot),
                        std::sync::Arc::new(handler.id()),
                        handler.role().to_string(),
                        "can_process".to_string(),
                        format!("Handler assessed - Pattern: {:.3}, Complexity: {:.3}, Final: {:.3}", 
                               pattern_match_score, complexity_score, composite_score),
                        ReportLevel::Debug,
                        HandlerPhase::Process,
                    ).with_tokens(tokens.len(), 0));
                }
                Ok(_) => {
                    context.registry.add_report(HandlerReport::new(
                        &format!("handler_skip_{}_{}", handler.id().name(), token_slot),
                        std::sync::Arc::new(handler.id()),
                        handler.role().to_string(),
                        "can_process".to_string(),
                        "Handler cannot process these tokens".to_string(),
                        ReportLevel::Debug,
                        HandlerPhase::Process,
                    ));
                }
                Err(e) => {
                    context.registry.add_report(HandlerReport::new(
                        &format!("handler_evaluation_error_{}_{}", handler.id().name(), token_slot),
                        std::sync::Arc::new(handler.id()),
                        handler.role().to_string(),
                        "can_process".to_string(),
                        format!("Error evaluating handler: {}", e),
                        ReportLevel::Warning,
                        HandlerPhase::Process,
                    ).with_success(false));
                }
            }
        }

        // Sort fallback handlers by score for intelligent fallback
        fallback_handlers.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal));

        // Process with the best handler if found, with intelligent fallback
        if let Some(handler) = best_handler {
            context.registry.add_report(HandlerReport::new(
                &format!("best_handler_selected_{}_{}", handler.id().name(), token_slot),
                std::sync::Arc::new(handler.id()),
                handler.role().to_string(),
                "process".to_string(),
                format!("Selected as best handler with score: {:.3} ({} fallbacks available)", 
                       best_score, fallback_handlers.len()),
                ReportLevel::Info,
                HandlerPhase::Process,
            ).with_tokens(tokens.len(), 0));

            // Attempt processing with retry logic
            let mut attempts = 0;
            let max_attempts = 3;
            let mut last_error = None;

            while attempts < max_attempts {
                attempts += 1;
                
                match handler.process(context, token_slot, token_range.clone()) {
                    Ok(result) => {
                        let elapsed = start_time.elapsed();
                        
                        context.registry.add_report(HandlerReport::new(
                            &format!("process_single_success_{}_{}", handler.id().name(), token_slot),
                            std::sync::Arc::new(handler.id()),
                            handler.role().to_string(),
                            "process".to_string(),
                            format!("Single handler processing completed successfully in {:?} (attempt {})", elapsed, attempts),
                            ReportLevel::Info,
                            HandlerPhase::Process,
                        ).with_tokens(tokens.len(), result.token_count())
                         .with_success(true));
                        
                        return Ok(result);
                    }
                    Err(e) => {
                        last_error = Some(e.clone());
                        
                        if attempts < max_attempts {
                            context.registry.add_report(HandlerReport::new(
                                &format!("process_retry_{}_{}", handler.id().name(), token_slot),
                                std::sync::Arc::new(handler.id()),
                                handler.role().to_string(),
                                "process".to_string(),
                                format!("Handler processing failed on attempt {}, retrying: {}", attempts, e),
                                ReportLevel::Warning,
                                HandlerPhase::Process,
                            ).with_success(false));
                            
                            // Brief pause before retry
                            std::thread::sleep(std::time::Duration::from_millis(10));
                        }
                    }
                }
            }

            // All attempts failed, try fallback handlers
            if let Some(error) = last_error {
                // Try intelligent fallback with top-scoring alternatives
                for (fallback_handler, fallback_score) in fallback_handlers.iter().take(2) {
                    if fallback_handler.id() != handler.id() {
                        context.registry.add_report(HandlerReport::new(
                            &format!("fallback_attempt_{}_{}", fallback_handler.id().name(), token_slot),
                            std::sync::Arc::new(fallback_handler.id()),
                            fallback_handler.role().to_string(),
                            "fallback_process".to_string(),
                            format!("Attempting fallback processing with score: {:.3}", fallback_score),
                            ReportLevel::Info,
                            HandlerPhase::Process,
                        ).with_tokens(tokens.len(), 0));

                        match fallback_handler.process(context, token_slot, token_range.clone()) {
                            Ok(fallback_result) => {
                                let elapsed = start_time.elapsed();
                                
                                context.registry.add_report(HandlerReport::new(
                                    &format!("fallback_success_{}_{}", fallback_handler.id().name(), token_slot),
                                    std::sync::Arc::new(fallback_handler.id()),
                                    fallback_handler.role().to_string(),
                                    "fallback_process".to_string(),
                                    format!("Fallback processing successful in {:?}", elapsed),
                                    ReportLevel::Info,
                                    HandlerPhase::Process,
                                ).with_tokens(tokens.len(), fallback_result.token_count())
                                 .with_success(true));
                                
                                return Ok(fallback_result);
                            }
                            Err(fallback_error) => {
                                context.registry.add_report(HandlerReport::new(
                                    &format!("fallback_failed_{}_{}", fallback_handler.id().name(), token_slot),
                                    std::sync::Arc::new(fallback_handler.id()),
                                    fallback_handler.role().to_string(),
                                    "fallback_process".to_string(),
                                    format!("Fallback processing failed: {}", fallback_error),
                                    ReportLevel::Warning,
                                    HandlerPhase::Process,
                                ).with_success(false));
                            }
                        }
                    }
                }

                // Try advanced redirect system as final resort
                let failed_result = HandlerResult::NotHandled(
                    Some(tokens.clone()),
                    token_range.clone(),
                    handler.id(),
                );
                
                return context.try_redirect(token_slot, token_range, failed_result, &handler.id());
            }
        }

        // If no handler found, return not handled with comprehensive analysis
        let elapsed = start_time.elapsed();
        context.registry.add_report(HandlerReport::new(
            &format!("process_single_no_handler_{}", token_slot),
            std::sync::Arc::new(Id::get("handlizer")),
            "Handlizer".to_string(),
            "process_single".to_string(),
            format!("No suitable handler found for tokens (evaluated {} handlers), took {:?}", 
                   handler_scores.len(), elapsed),
            ReportLevel::Warning,
            HandlerPhase::Process,
        ).with_tokens(tokens.len(), 0)
         .with_success(false));

        Ok(HandlerResult::NotHandled(
            Some(tokens),
            token_range,
            Id::get("handlizer"),
        ))
    }



    fn calculate_token_complexity(&self, tokens: &[Token]) -> f64 {
        if tokens.is_empty() {
            return 0.0;
        }
        
        let mut complexity = 0.0;
        let mut nesting_level = 0;
        
        for _token in tokens {
            nesting_level += 1;
            complexity += nesting_level as f64 * 0.5;
        }
        
        complexity / tokens.len() as f64
    }

    fn suggest_alternative_handler(&mut self, tokens: &[Token], failed_handler_id: &Id) -> Option<Id> {
        let mut best_candidate = None;
        let mut best_score = 0.0;
        
        for handler in self.get_sorted_handlers() {
            if handler.id() != *failed_handler_id {
                let pattern_score = Handlizer::calculate_pattern_match_score(handler.as_ref(), tokens, &(0..tokens.len()));
                
                if pattern_score > best_score {
                    best_score = pattern_score;
                    best_candidate = Some(handler.id());
                }
            }
        }
        
        best_candidate
    }

    fn calculate_pattern_match_score(handler: &dyn Handler, tokens: &[Token], range: &Range<usize>) -> f64 {
        if tokens.is_empty() || range.is_empty() {
            return 0.0;
        }
        
        let end = range.end.min(tokens.len());
        let start = range.start.min(end);
        let token_count = end - start;
        
        if token_count == 0 {
            return 0.0;
        }
        
        // Enhanced scoring with multiple factors
        let priority_score = handler.priority() as f64 / 1000.0;
        
        // Token type relevance scoring
        let mut type_match_score = 0.0;
        let relevant_tokens = &tokens[start..end];
        
        for token in relevant_tokens {
            match token {
                Token::l("if") | Token::l("else") | Token::l("while") | Token::l("for") => {
                    type_match_score += 0.8;
                }
                Token::l("int") | Token::l("char") | Token::l("float") | Token::l("double") => {
                    type_match_score += 0.7;
                }
                Token::c('{') | Token::c('}') | Token::c('(') | Token::c(')') => {
                    type_match_score += 0.5;
                }
                Token::c(';') | Token::c(',') => {
                    type_match_score += 0.3;
                }
                _ => {
                    type_match_score += 0.1;
                }
            }
        }

        type_match_score /= token_count as f64;
        
        // Pattern complexity bonus
        let complexity_bonus = if token_count > 3 { 0.1 } else { 0.0 };
        
        // Weighted composite score
        (priority_score * 0.5) + (type_match_score * 0.4) + complexity_bonus
    }
    
    // ========================================================================
    // INTEGRATED ADAPTIVE PATTERN MATCHING METHODS
    // ========================================================================
    
    /// Perform adaptive pattern matching with learning
    pub fn adaptive_match(&mut self, pattern: &HandlerPattern, tokens: &[Token]) -> AdaptivePatternResult {
        let handler_type = pattern.id.clone();
        
        // Get or create pattern tree for this handler type
        if !self.pattern_trees.contains_key(&handler_type) {
            self.pattern_trees.insert(handler_type.clone(), Vec::new());
        }
        
        // Try matching with existing pattern variations
        if let Some(nodes) = self.pattern_trees.get(&handler_type) {
            for node in nodes {
                if let Ok(result) = self.try_pattern_match(&node.current_pattern, tokens) {
                    if result.matched {
                        return result;
                    }
                }
            }
        }
        
        // No existing pattern worked, try adaptive exploration
        self.explore_pattern_variations(pattern, tokens)
    }
    
    /// Record successful pattern matching for learning
    pub fn record_success(&mut self, handler_id: &Id, tokens_consumed: usize, confidence: f64) {
        self.adaptive_stats.total_attempts.fetch_add(1, Ordering::Relaxed);
        self.adaptive_stats.successes.fetch_add(1, Ordering::Relaxed);
        
        let record = SuccessRecord {
            handler_id: handler_id.clone(),
            token_count: tokens_consumed,
            range_size: tokens_consumed,
            confidence,
            timestamp: SystemTime::now(),
        };
        
        if let Ok(mut patterns) = self.success_patterns.write() {
            patterns.entry(handler_id.name().to_string())
                   .or_insert_with(Vec::new)
                   .push(record);
        }
    }
    
    /// Record failed pattern matching for learning
    pub fn record_failure(&mut self, handler_id: &Id, tokens_attempted: usize, reason: String) {
        self.adaptive_stats.total_attempts.fetch_add(1, Ordering::Relaxed);
        self.adaptive_stats.failures.fetch_add(1, Ordering::Relaxed);
        
        let record = FailureRecord {
            handler_id: handler_id.clone(),
            token_count: tokens_attempted,
            range_size: tokens_attempted,
            reason,
            timestamp: SystemTime::now(),
        };
        
        if let Ok(mut patterns) = self.failure_patterns.write() {
            patterns.entry(handler_id.name().to_string())
                   .or_insert_with(Vec::new)
                   .push(record);
        }
    }
    
    /// Generate analysis report of adaptive performance
    pub fn analyze_adaptive_performance(&self) -> String {
        let total_attempts = self.adaptive_stats.total_attempts.load(Ordering::Relaxed);
        let successes = self.adaptive_stats.successes.load(Ordering::Relaxed);
        let failures = self.adaptive_stats.failures.load(Ordering::Relaxed);
        
        let success_rate = if total_attempts > 0 {
            successes as f64 / total_attempts as f64
        } else {
            0.0
        };
        
        format!(
            "=== ADAPTIVE PATTERN ANALYSIS REPORT ===\n\
             Total attempts: {}\n\
             Successes: {}\n\
             Failures: {}\n\
             Success rate: {:.1}%\n\
             Pattern trees: {}\n\
             Exploration cache: {} entries",
            total_attempts, successes, failures,
            success_rate * 100.0,
            self.pattern_trees.len(),
            self.exploration_cache.len()
        )
    }
    
    // Private helper methods
    
    fn try_pattern_match(&self, pattern: &HandlerPattern, tokens: &[Token]) -> Result<AdaptivePatternResult> {
        // Try to match using the pattern - this is a simplified implementation
        let pattern_result = if tokens.len() >= pattern.min_tokens {
            let tokens_consumed = if let Some(max_tokens) = pattern.max_tokens {
                tokens.len().min(max_tokens)
            } else {
                tokens.len()
            };
            
            if tokens_consumed >= pattern.min_tokens {
                PatternResult::Match { consumed_tokens: tokens_consumed }
            } else {
                PatternResult::NoMatch { reason: "Below minimum tokens".to_string() }
            }
        } else {
            PatternResult::NoMatch { reason: "Insufficient tokens".to_string() }
        };
        
        Ok(AdaptivePatternResult::from_existing(pattern_result))
    }
    
    fn explore_pattern_variations(&mut self, pattern: &HandlerPattern, tokens: &[Token]) -> AdaptivePatternResult {
        use std::collections::VecDeque;
        
        let mut exploration_queue = VecDeque::new();
        let mut best_result = AdaptivePatternResult::no_match();
        let mut explored_count = 0;
        let handler_type = &pattern.id;

        // Start exploration from root nodes
        if let Some(root_nodes) = self.pattern_trees.get(handler_type).cloned() {
            for root_node in root_nodes {
                exploration_queue.push_back((root_node, 0)); // (node, depth)
            }
        }

        // Breadth-first exploration with probabilistic selection
        while let Some((current_node, depth)) = exploration_queue.pop_front() {
            if depth >= self.bounds.max_depth {
                continue;
            }

            explored_count += 1;

            // Generate variations of current pattern
            let variations = self.generate_pattern_variations(&current_node);

            for variation in variations {
                // Check bounds before exploring
                if variation.deviation_distance > self.bounds.max_deviation {
                    continue;
                }

                // Try this variation
                let result = match self.try_pattern_match(&variation.current_pattern, tokens) {
                    Ok(result) => result,
                    Err(_) => continue, // Skip this variation if pattern matching fails
                };
                let fuzzy_score = self.calculate_fuzzy_score(&variation, &result);

                // Update statistics
                let mut updated_variation = variation.clone();
                updated_variation.update_success(result.matched);

                // Check if this is better than our current best
                if fuzzy_score > self.calculate_fuzzy_score(&PatternNode::new_root(pattern.clone()), &best_result) {
                    best_result = result.clone();
                }

                // If this variation shows promise, add it to exploration queue
                if result.confidence >= self.bounds.min_confidence && depth < self.bounds.max_depth - 1 {
                    exploration_queue.push_back((updated_variation, depth + 1));
                }
            }

            // Limit exploration to prevent infinite branching
            if explored_count > 100 {
                break;
            }
        }

        // Update global statistics
        if best_result.matched {
            self.adaptive_stats.successful_adaptations += 1;
        } else {
            self.adaptive_stats.failed_explorations += 1;
        }

        best_result
    }
    
    /// Initialize with patterns from existing Patternizer
    pub fn initialize_from_patternizer(&mut self, patternizer: &crate::Patternizer) -> Result<()> {
        // Get exported patterns from the patternizer
        let exported_patterns = patternizer.export_patterns_for_adaptation();
        let mut total_patterns_imported = 0;

        for (handler_type, patterns) in exported_patterns {
            let mut root_nodes = Vec::new();
            
            for pattern in patterns {
                let root_node = PatternNode::new_root(pattern);
                root_nodes.push(root_node);
                total_patterns_imported += 1;
            }
            
            self.pattern_trees.insert(handler_type.clone(), root_nodes.clone());
        }

        Ok(())
    }
    
    /// Generate probabilistic variations of a pattern
    fn generate_pattern_variations(&self, node: &PatternNode) -> Vec<PatternNode> {
        let mut variations = Vec::new();
        
        // Generate variations by modifying different aspects of the pattern
        
        // 1. Confidence-based variations
        for confidence_delta in [-0.1, -0.05, 0.05, 0.1] {
            if let Some(variant_pattern) = self.create_confidence_variant(&node.current_pattern, confidence_delta) {
                let deviation = (confidence_delta.abs() / 0.1) * self.bounds.exploration_radius;
                let variation = node.create_variation(variant_pattern, deviation);
                variations.push(variation);
            }
        }

        // 2. Priority-based variations
        for priority_delta in [-1, 1] {
            if let Some(variant_pattern) = self.create_priority_variant(&node.current_pattern, priority_delta) {
                let deviation = (priority_delta.abs() as f64 / 5.0) * self.bounds.exploration_radius;
                let variation = node.create_variation(variant_pattern, deviation);
                variations.push(variation);
            }
        }

        // 3. Pattern structure variations
        let structural_variants = self.create_structural_variants(&node.current_pattern);
        for variant_pattern in structural_variants {
            let deviation = self.calculate_pattern_deviation(&node.base_pattern, &variant_pattern);
            if deviation <= self.bounds.max_deviation {
                let variation = node.create_variation(variant_pattern, deviation);
                variations.push(variation);
            }
        }

        variations
    }
    
    /// Calculate fuzzy logic score for a pattern variation
    fn calculate_fuzzy_score(&self, node: &PatternNode, result: &AdaptivePatternResult) -> f64 {
        let similarity_score = 1.0 - node.deviation_distance;
        let success_score = node.success_rate;
        let confidence_score = result.confidence;
        let deviation_penalty = node.deviation_distance * self.fuzzy_scorer.deviation_penalty;

        (similarity_score * self.fuzzy_scorer.similarity_weight) +
        (success_score * self.fuzzy_scorer.success_weight) +
        (confidence_score * self.fuzzy_scorer.confidence_weight) -
        deviation_penalty
    }
    
    /// Create a confidence-based variant of a pattern
    fn create_confidence_variant(&self, base_pattern: &HandlerPattern, _delta: f64) -> Option<HandlerPattern> {
        // Implementation would modify pattern confidence/threshold
        Some(base_pattern.clone())
    }

    /// Create a priority-based variant of a pattern  
    fn create_priority_variant(&self, base_pattern: &HandlerPattern, delta: i32) -> Option<HandlerPattern> {
        let mut variant = base_pattern.clone();
        variant.priority = (variant.priority + delta).max(0);
        Some(variant)
    }

    /// Create structural variants of a pattern
    fn create_structural_variants(&self, base_pattern: &HandlerPattern) -> Vec<HandlerPattern> {
        let mut variants = Vec::new();
        
        // For now, creating placeholder variants
        variants.push(base_pattern.clone());
        
        variants
    }

    /// Calculate deviation distance between two patterns
    fn calculate_pattern_deviation(&self, base: &HandlerPattern, variant: &HandlerPattern) -> f64 {
        let priority_diff = (base.priority as i32 - variant.priority as i32).abs() as f64 / 10.0;
        
        // Add other pattern difference calculations here
        priority_diff.min(1.0) // Cap at 1.0 (100% difference)
    }
    
    /// Update success metrics for adaptive learning
    pub fn update_success_metrics(&self, handler_id: &Id, confidence: f64, _processing_time: std::time::Duration, _token_count: usize) {
        self.adaptive_stats.total_attempts.fetch_add(1, Ordering::Relaxed);
        self.adaptive_stats.successes.fetch_add(1, Ordering::Relaxed);
    }
    
    /// Update failure metrics for adaptive learning
    pub fn update_failure_metrics(&self, handler_id: &Id, confidence: f64, _processing_time: std::time::Duration) {
        self.adaptive_stats.total_attempts.fetch_add(1, Ordering::Relaxed);
        self.adaptive_stats.failures.fetch_add(1, Ordering::Relaxed);
    }
    
    /// Get statistics about the adaptive system performance
    pub fn get_adaptive_stats(&self) -> &AdaptiveStats {
        &self.adaptive_stats
    }

    /// Reset adaptive statistics
    pub fn reset_adaptive_stats(&mut self) {
        self.adaptive_stats = AdaptiveStats::default();
    }
    
    /// Match pattern using adaptive system
    pub fn match_adaptive_pattern(&mut self, handler_type: &str, tokens: &[Token]) -> PatternResult {
        let pattern = HandlerPattern::new(handler_type.to_string(), "adaptive pattern".to_string());
        
        // Use adaptive system 
        let adaptive_result = self.adaptive_match(&pattern, tokens);

        match adaptive_result {
            AdaptivePatternResult { matched, base_result, .. } if matched => {
                base_result
            },
            _ => PatternResult::NoMatch { reason: "No adaptive match found".to_string() }
        }
    }
    
    /// Analyze patterns with Samplizer using caching
    pub fn analyze_patterns_with_samplizer_cached(&mut self, use_cache: bool) -> Result<()> {
        // Initialize pattern trees if needed
        if self.pattern_trees.is_empty() {
            // Create some default patterns for testing
            let default_patterns = vec![
                HandlerPattern::new("function".to_string(), "C function pattern".to_string()),
                HandlerPattern::new("struct".to_string(), "C struct pattern".to_string()),
                HandlerPattern::new("enum".to_string(), "C enum pattern".to_string()),
            ];
            
            for pattern in default_patterns {
                let handler_type = pattern.id.clone();
                let root_node = PatternNode::new_root(pattern);
                self.pattern_trees.insert(handler_type, vec![root_node]);
            }
        }
        
        // Update exploration statistics
        self.adaptive_stats.total_explorations += self.pattern_trees.len();
        
        Ok(())
    }
    
    // ============================================================================
    // REDIRECT SYSTEM METHODS (integrated from Redirecter)
    // ============================================================================
    
    /// Process a redirect request with pre-analyzed routing response
    pub fn process_redirect(
        &mut self,
        request: RedirectRequest,
        response: RedirectResponse,
    ) -> Result<crate::HandlerResult> {
        // Check redirect depth limit
        if self.redirect_depth >= self.max_redirect_depth {
            return Ok(crate::HandlerResult::NotHandled(
                None,
                0..0,
                request.from_handler.clone(),
            ));
        }

        // Check for circular redirects
        if self.visited_handlers.contains(&request.from_handler) {
            return Ok(crate::HandlerResult::NotHandled(
                None,
                0..0,
                request.from_handler.clone(),
            ));
        }

        // Add current handler to visited set
        self.visited_handlers.insert(request.from_handler.clone());
        self.redirect_depth += 1;

        // Convert response to HandlerResult
        if let Some(target_handler) = response.target_handler {
            let token_range = response.modified_range.unwrap_or(request.token_range);
            Ok(crate::HandlerResult::Redirected(
                None,
                token_range,
                response.routing_reason,
                request.from_handler.clone(),
                target_handler,
            ))
        } else {
            Ok(crate::HandlerResult::NotHandled(
                None,
                request.token_range,
                request.from_handler.clone(),
            ))
        }
    }
    
    /// Analyze and route a redirect request (separated for borrowing reasons)
    pub fn analyze_redirect_request(
        &self,
        context: &mut Context,
        request: &RedirectRequest,
    ) -> Result<RedirectResponse> {
        self.analyze_and_route(context, request)
    }
    
    /// Analyze patterns and tokens to determine routing target
    fn analyze_and_route(
        &self,
        context: &mut Context,
        request: &RedirectRequest,
    ) -> Result<RedirectResponse> {
        // Note: We can't cache failed patterns here since this is &self
        // Caching should be handled by the caller if needed
        let handler_name = request.from_handler.to_string();
        
        let tokens_result = if request.token_range.end > context.tokenizer.current_tokens().len()
            || request.token_range.start >= context.tokenizer.current_tokens().len()
        {
            None
        } else {
            Some(context.tokenizer.current_tokens()[request.token_range.clone()].to_vec())
        };

        let tokens = match tokens_result {
            Some(tokens) => tokens,
            None => {
                return Ok(RedirectResponse {
                    target_handler: None,
                    modified_range: None,
                    routing_reason: "Invalid token range or tokenizer not available".to_string(),
                    should_retry: false,
                    metadata: Vec::new(),
                });
            }
        };

        // Filter tokens (remove newlines)
        let filtered_tokens: Vec<Token> = tokens
            .iter()
            .filter(|token| !matches!(**token, Token::n()))
            .cloned()
            .collect();

        if filtered_tokens.is_empty() {
            return Ok(RedirectResponse {
                target_handler: None,
                modified_range: None,
                routing_reason: "No valid tokens after filtering".to_string(),
                should_retry: false,
                metadata: Vec::new(),
            });
        }
        
        // Analyze patterns and route directly
        let mut best_match = None;
        let mut best_confidence = 0.0;

        // Get potential routing targets for this handler
        let potential_targets = self
            .routing_rules
            .get(&handler_name)
            .cloned()
            .unwrap_or_default();

        for target_handler in &potential_targets {
            if self
                .visited_handlers
                .iter()
                .any(|h| h.to_string() == *target_handler)
            {
                continue; // Skip already visited handlers
            }

            // Test patterns specific to target handler
            let confidence =
                self.test_handler_patterns(&filtered_tokens, target_handler, &mut context.patternizer)?;

            if confidence > best_confidence {
                best_confidence = confidence;
                best_match = Some(target_handler.clone());
            }
        }

        // Return routing decision
        if let Some(target) = best_match {
            if best_confidence > 0.6 {
                // Confidence threshold
                Ok(RedirectResponse {
                    target_handler: Some(Id::get(&target)),
                    modified_range: None,
                    routing_reason: format!(
                        "Pattern analysis suggests {} (confidence: {:.2})",
                        target, best_confidence
                    ),
                    should_retry: true,
                    metadata: Vec::new(),
                })
            } else {
                Ok(RedirectResponse {
                    target_handler: None,
                    modified_range: None,
                    routing_reason: format!("Low confidence routing (max: {:.2})", best_confidence),
                    should_retry: false,
                    metadata: Vec::new(),
                })
            }
        } else {
            Ok(RedirectResponse {
                target_handler: None,
                modified_range: None,
                routing_reason: "No suitable routing target found".to_string(),
                should_retry: false,
                metadata: Vec::new(),
            })
        }
    }

    /// Test specific handler patterns against tokens to calculate confidence
    fn test_handler_patterns(
        &self,
        tokens: &[Token],
        handler_type: &str,
        patternizer: &mut crate::Patternizer,
    ) -> Result<f64> {
        let mut matched = 0.0;
        let mut total_tests = 0.0;

        // Test different pattern types based on handler
        match handler_type {
            "function_handler" => {
                if matches!(
                    patternizer.match_pattern("function", tokens),
                    PatternResult::Match { .. }
                ) {
                    matched += 1.0;
                }
                total_tests += 1.0;

                if matches!(
                    patternizer.match_pattern("function_declaration", tokens),
                    PatternResult::Match { .. }
                ) {
                    matched += 0.8; // Slightly lower weight
                }
                total_tests += 1.0;
            }
            "struct_handler" => {
                if matches!(
                    patternizer.match_pattern("struct_definition", tokens),
                    PatternResult::Match { .. }
                ) {
                    matched += 1.0;
                }
                total_tests += 1.0;
            }
            "enum_handler" => {
                if matches!(
                    patternizer.match_pattern("enum_definition", tokens),
                    PatternResult::Match { .. }
                ) {
                    matched += 1.0;
                }
                total_tests += 1.0;
            }
            "typedef_handler" => {
                if matches!(
                    patternizer.match_pattern("typedef_definition", tokens),
                    PatternResult::Match { .. }
                ) {
                    matched += 1.0;
                }
                total_tests += 1.0;
            }
            "array_handler" => {
                if matches!(
                    patternizer.match_pattern("array_declaration", tokens),
                    PatternResult::Match { .. }
                ) {
                    matched += 1.0;
                }
                total_tests += 1.0;
            }
            "global_handler" => {
                if matches!(
                    patternizer.match_pattern("global_variable", tokens),
                    PatternResult::Match { .. }
                ) {
                    matched += 1.0;
                }
                total_tests += 1.0;
            }
            "expression_handler" => {
                if matches!(
                    patternizer.match_pattern("expression_assignment", tokens),
                    PatternResult::Match { .. }
                ) {
                    matched += 1.0;
                }
                total_tests += 1.0;
            }
            _ => {
                // Default pattern testing
                total_tests = 1.0;
            }
        }

        if total_tests > 0.0 {
            Ok(matched / total_tests)
        } else {
            Ok(0.0)
        }
    }

    /// Reset redirect state for new processing
    pub fn reset_redirect_state(&mut self) {
        self.visited_handlers.clear();
        self.redirect_depth = 0;
        self.pattern_cache.clear();
    }
    
    /// Cache failed patterns for a handler (call before analyze_redirect_request if needed)
    pub fn cache_failed_patterns(&mut self, handler_name: String, failed_patterns: Vec<String>) {
        self.pattern_cache.insert(handler_name, failed_patterns);
    }
    
    /// Process redirect with pre-computed analysis (no borrowing conflicts)
    pub fn process_redirect_analyzed(
        &mut self,
        request: RedirectRequest,
        response: RedirectResponse,
    ) -> Result<crate::HandlerResult> {
        // Cache failed patterns
        let handler_name = request.from_handler.to_string();
        self.cache_failed_patterns(handler_name, request.failed_patterns.clone());
        
        // Process redirect with pre-analyzed response
        self.process_redirect(request, response)
    }
    
    /// Helper function for handlers to perform redirect workflow
    pub fn handle_redirect_workflow(
        context: &mut crate::Context,
        token_slot: usize,
        token_range: std::ops::Range<usize>,
        request: RedirectRequest,
    ) -> Result<crate::HandlerResult> {
        // Step 1: Extract data and analyze (immutable borrow)
        let tokens = context.tokenizer.slots()[token_slot].tokens()[token_range.clone()].to_vec();
        let mut patternizer = std::mem::take(&mut context.patternizer);
        let response = context.handlizer.analyze_redirect_with_data(&tokens, &mut patternizer, &request)?;
        
        // Step 2: Process redirect (mutable borrow)
        let result = context.handlizer.process_redirect_analyzed(request, response)?;
        
        // Step 3: Restore patternizer
        context.patternizer = patternizer;
        
        Ok(result)
    }
    
    /// Analyze redirect with pre-extracted data to avoid context borrowing issues
    pub fn analyze_redirect_with_data(
        &self,
        tokens: &[crate::Token],
        patternizer: &mut crate::Patternizer,
        request: &RedirectRequest,
    ) -> Result<RedirectResponse> {
        let handler_name = request.from_handler.to_string();

        // Filter tokens (remove newlines)
        let filtered_tokens: Vec<crate::Token> = tokens
            .iter()
            .filter(|token| !matches!(**token, crate::Token::n()))
            .cloned()
            .collect();

        if filtered_tokens.is_empty() {
            return Ok(RedirectResponse {
                target_handler: None,
                modified_range: None,
                routing_reason: "No valid tokens after filtering".to_string(),
                should_retry: false,
                metadata: Vec::new(),
            });
        }
        
        // Analyze patterns and route directly
        let mut best_match = None;
        let mut best_confidence = 0.0;

        // Get potential routing targets for this handler
        let potential_targets = self
            .routing_rules
            .get(&handler_name)
            .cloned()
            .unwrap_or_default();

        for target_handler in &potential_targets {
            if self
                .visited_handlers
                .iter()
                .any(|h| h.to_string() == *target_handler)
            {
                continue; // Skip already visited handlers
            }

            // Test patterns specific to target handler
            let confidence =
                self.test_handler_patterns(&filtered_tokens, target_handler, patternizer)?;

            if confidence > best_confidence {
                best_confidence = confidence;
                best_match = Some(target_handler.clone());
            }
        }

        // Return routing decision
        if let Some(target) = best_match {
            if best_confidence > 0.6 {
                Ok(RedirectResponse {
                    target_handler: Some(Id::get(&target)),
                    modified_range: None,
                    routing_reason: format!(
                        "Pattern analysis suggests {} (confidence: {:.2})",
                        target, best_confidence
                    ),
                    should_retry: true,
                    metadata: Vec::new(),
                })
            } else {
                Ok(RedirectResponse {
                    target_handler: None,
                    modified_range: None,
                    routing_reason: format!("Low confidence routing (max: {:.2})", best_confidence),
                    should_retry: false,
                    metadata: Vec::new(),
                })
            }
        } else {
            Ok(RedirectResponse {
                target_handler: None,
                modified_range: None,
                routing_reason: "No suitable routing target found".to_string(),
                should_retry: false,
                metadata: Vec::new(),
            })
        }
    }
}
impl Default for Handlizer {
    fn default() -> Self {
        Self::new()
    }
}
/// Legacy handler result - deprecated
/// Use the main HandlerResult enum instead
impl HandlerResult {
    pub fn timestamp(&self) -> u128 {
        match self {
            Self::Handled(_, _, id) => id.timestamp(),
            Self::Processed(_, _, _, id) => id.timestamp(),
            Self::Extracted(_, _, _, id) => id.timestamp(),
            Self::Converted(_, _, _, id) => id.timestamp(),
            Self::Completed(_, _, _, id) => id.timestamp(),
            Self::NotHandled(_, _, id) => id.timestamp(),
            Self::Redirected(_, _, _, id, _) => id.timestamp(),
        }
    }

    /// Get the number of tokens consumed by this result
    pub fn token_count(&self) -> usize {
        match self {
            Self::Handled(_, range, _) => range.len(),
            Self::Processed(_, range, _, _) => range.len(),
            Self::Extracted(_, range, _, _) => range.len(),
            Self::Converted(_, range, _, _) => range.len(),
            Self::Completed(_, range, _, _) => range.len(),
            Self::NotHandled(_, range, _) => range.len(),
            Self::Redirected(_, range, _, _, _) => range.len(),
        }
    }
}
impl Display for HandlerResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HandlerResult::Handled(tokens, range, id) => {
                let token_info = tokens.as_ref()
                    .map(|t| format!(" ({} tokens)", t.len()))
                    .unwrap_or_default();
                write!(
                    f,
                    "Handler: {} reports handling successful (range {}..{}, consumed {} tokens{}).\n",
                    id.name(),
                    range.start,
                    range.end,
                    range.len(),
                    token_info
                )
            }
            HandlerResult::Processed(tokens, range, s, id) => {
                let token_info = tokens.as_ref()
                    .map(|t| format!(" ({} tokens)", t.len()))
                    .unwrap_or_default();
                write!(
                    f,
                    "Handler: {} reports processing successful (range {}..{}, consumed {} tokens{}).\nCode:\n{}",
                    id.name(),
                    range.start,
                    range.end,
                    range.len(),
                    token_info,
                    s
                )
            }
            HandlerResult::Extracted(element, range, s, id) => {
                write!(
                    f,
                    "Handler: {} reports extraction successful (range {}..{}, consumed {} tokens).\nElement: {:?}\nCode:\n{}",
                    id.name(),
                    range.start,
                    range.end,
                    range.len(),
                    element,
                    s
                )
            }
            HandlerResult::Converted(element, range, s, id) => {
                write!(
                    f,
                    "Handler: {} reports conversion successful (range {}..{}, consumed {} tokens).\nElement: {:?}\nCode:\n{}",
                    id.name(),
                    range.start,
                    range.end,
                    range.len(),
                    element,
                    s
                )
            }
            HandlerResult::Completed(tokens, range, s, id) => {
                let token_info = tokens.as_ref()
                    .map(|t| format!(" ({} tokens)", t.len()))
                    .unwrap_or_default();
                write!(
                    f,
                    "Handler: {} reports all operations successful (range {}..{}, consumed {} tokens{}).\nCode:\n{}",
                    id.name(),
                    range.start,
                    range.end,
                    range.len(),
                    token_info,
                    s
                )
            }
            HandlerResult::NotHandled(tokens, range, id) => {
                let token_info = tokens.as_ref()
                    .map(|t| format!(" ({} tokens)", t.len()))
                    .unwrap_or_default();
                write!(
                    f,
                    "Handler: {} reports not handled (range {}..{}, consumed {} tokens{})",
                    id.name(),
                    range.start,
                    range.end,
                    range.len(),
                    token_info
                )
            }
            HandlerResult::Redirected(tokens, range, s, id1, id2) => {
                let token_info = tokens.as_ref()
                    .map(|t| format!(" ({} tokens)", t.len()))
                    .unwrap_or_default();
                write!(
                    f,
                    "Handler: {} reports redirection to Handler: {} (range {}..{}, consumed {} tokens{}).\nCode:\n{}",
                    id1.name(),
                    id2.name(),
                    range.start,
                    range.end,
                    range.len(),
                    token_info,
                    s
                )
            }
        }
    }
}
/// Result of a handler processing tokens, including the processed token range
#[derive(Debug, Clone)]
pub struct ProcessedResult {
    pub id: Id,
    /// The result of the handler's processing
    pub result: HandlerResult,
    /// The start index of the processed tokens (inclusive)
    pub start_idx: usize,
    /// The end index of the processed tokens (exclusive)
    pub end_idx: usize,
    /// Number of tokens consumed by the handler
    pub tokens_consumed: usize,
}

impl ProcessedResult {
    pub fn new(start_idx: usize, end_idx: usize, id: Id, result: HandlerResult) -> ProcessedResult {
        // Ensure consistency: tokens_consumed should match the range in HandlerResult
        let actual_consumed = result.token_count().max(end_idx.saturating_sub(start_idx));
        ProcessedResult {
            id,
            result,
            start_idx,
            end_idx: start_idx + actual_consumed,
            tokens_consumed: actual_consumed,
        }
    }

    pub fn with_end_idx(mut self, end_idx: usize) -> Self {
        self.end_idx = end_idx;
        self.tokens_consumed = self
            .result
            .token_count()
            .max(end_idx.saturating_sub(self.start_idx));
        self
    }

    pub fn with_tokens_consumed(mut self, count: usize) -> Self {
        self.tokens_consumed = count;
        self.end_idx = self.start_idx + count;
        self
    }

    /// Get the actual token range that was processed
    pub fn token_range(&self) -> Range<usize> {
        self.start_idx..self.end_idx
    }

    /// Verify consistency between ProcessedResult and HandlerResult ranges
    pub fn verify_consistency(&self) -> bool {
        let handler_count = self.result.token_count();
        let processed_count = self.tokens_consumed;
        handler_count == processed_count
    }
}

/// Collection of ProcessedResults that can be easily joined and manipulated
#[derive(Debug, Clone)]
pub struct ProcessedResults {
    pub results: Vec<ProcessedResult>,
    pub total_tokens_processed: usize,
    pub total_tokens_consumed: usize,
}

impl ProcessedResults {
    /// Create a new empty collection
    pub fn new() -> Self {
        Self {
            results: Vec::new(),
            total_tokens_processed: 0,
            total_tokens_consumed: 0,
        }
    }

    /// Create a collection from a single result
    pub fn from_single(result: ProcessedResult) -> Self {
        let tokens_consumed = result.tokens_consumed;
        Self {
            results: vec![result],
            total_tokens_processed: tokens_consumed,
            total_tokens_consumed: tokens_consumed,
        }
    }

    /// Create a collection from a vector of results
    pub fn from_vec(results: Vec<ProcessedResult>) -> Self {
        let total_consumed = results.iter().map(|r| r.tokens_consumed).sum();
        Self {
            results,
            total_tokens_processed: total_consumed,
            total_tokens_consumed: total_consumed,
        }
    }

    /// Add a ProcessedResult to the collection and update totals
    pub fn add(&mut self, result: ProcessedResult) {
        // Use the actual range from HandlerResult for consistency
        let tokens_in_range = result.result.token_count();
        self.total_tokens_consumed += tokens_in_range;
        self.total_tokens_processed += tokens_in_range;
        self.results.push(result);
    }

    /// Join another collection into this one
    pub fn join(&mut self, other: ProcessedResults) {
        self.total_tokens_consumed += other.total_tokens_consumed;
        self.total_tokens_processed += other.total_tokens_processed;
        self.results.extend(other.results);
    }

    /// Get all converted code from all results
    pub fn extract_all_code(&self) -> Vec<String> {
        self.results
            .iter()
            .filter_map(|result| match &result.result {
                HandlerResult::Processed(_, _, code, _)
                | HandlerResult::Completed(_, _, code, _) => Some(code.clone()),
                _ => None,
            })
            .filter(|code| !code.trim().is_empty())
            .collect()
    }

    /// Get total number of results
    pub fn len(&self) -> usize {
        self.results.len()
    }

    /// Check if collection is empty
    pub fn is_empty(&self) -> bool {
        self.results.is_empty()
    }

    /// Get results by handler type
    pub fn get_by_handler(&self, handler_name: &str) -> Vec<&ProcessedResult> {
        self.results
            .iter()
            .filter(|result| result.id.name() == handler_name)
            .collect()
    }

    /// Get all successful conversions (those with converted code)
    pub fn get_successful(&self) -> Vec<&ProcessedResult> {
        self.results
            .iter()
            .filter(|result| {
                matches!(
                    result.result,
                    HandlerResult::Converted(_, _, _, _) | HandlerResult::Extracted(_, _, _, _)
                )
            })
            .collect()
    }
}

impl Default for ProcessedResults {
    fn default() -> Self {
        Self::new()
    }
}
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Pending {
    /// (report,is_processed)
    Report(Box<HandlerReport>, bool),
    /// (result,timestamp,is_processed)
    Result(Box<HandlerResult>, bool),
    /// (redirect,is_processed)
    Redirect(Box<HandlerRedirect>, bool),
    /// (msg,timestamp, is_read)
    Message(String, u128, bool),
}
impl Pending {
    /// Get the priority of this pending operation (higher numbers = higher priority)
    pub fn priority(&self) -> u64 {
        match self {
            Pending::Redirect(_, _) => 1000, // Highest priority - affects control flow
            Pending::Report(_, _) => 500,    // Medium-high - logging and debugging
            Pending::Result(_, _) => 300,    // Medium - processing results
            Pending::Message(_, _, _) => 100, // Lowest - simple messages
        }
    }

    /// Get a timestamp for ordering operations of the same priority
    pub fn timestamp(&self) -> u128 {
        match self {
            Pending::Report(report, _) => report.timestamp(),
            Pending::Result(result, _) => result.timestamp(),
            Pending::Message(_, timestamp, _) => *timestamp,
            Pending::Redirect(redirect, _) => redirect.timestamp(),
        }
    }
}

/// Iterator for Pending operations that yields operations in priority order
pub struct PendingIterator {
    operations: Vec<Pending>,
    current: usize,
}

impl Iterator for PendingIterator {
    type Item = Pending;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current < self.operations.len() {
            let operation = self.operations[self.current].clone();
            self.current += 1;
            Some(operation)
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.operations.len() - self.current;
        (remaining, Some(remaining))
    }
}
