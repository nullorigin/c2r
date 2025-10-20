//! Trait-based handler system for C to Rust conversion
//!
//! This module defines the Handler trait and supporting types for processing C tokens
//! and converting them to Rust code. Each handler specializes in specific C constructs
//! like functions, arrays, structs, etc.

use crate::{
    gen_name, Context, ConvertedElement, ExtractedElement, HandlerPattern, HandlerRedirect,
    Id, Maybe, PatternResult, Phase, Report, ReportLevel, Result, Token,
};
use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use std::hash::{DefaultHasher, Hash, Hasher};
use std::ops::Range;
use std::sync::{
    atomic::{AtomicUsize, Ordering}, Arc,
    RwLock,
};
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
    pub total_explorations: AtomicUsize,
    pub successful_adaptations: AtomicUsize,
    pub failed_explorations: AtomicUsize,
    pub total_attempts: AtomicUsize,
    pub successes: AtomicUsize,
    pub failures: AtomicUsize,
    pub average_deviation: f64,
    pub best_success_rate: f64,
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
            total_explorations: AtomicUsize::new(0),
            successful_adaptations: AtomicUsize::new(0),
            failed_explorations: AtomicUsize::new(0),
            total_attempts: AtomicUsize::new(0),
            successes: AtomicUsize::new(0),
            failures: AtomicUsize::new(0),
            average_deviation: 0.0,
            best_success_rate: 0.0,
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
                reason: "No adaptive match found".to_string(),
            },
        }
    }

    /// Convert from existing PatternResult
    pub fn from_existing(result: PatternResult) -> Self {
        let (matched, confidence, tokens_consumed) = match &result {
            PatternResult::Match { consumed_tokens } => (true, 1.0, *consumed_tokens),
            PatternResult::CountOf { offsets } => (true, 0.85, offsets.len()),
            PatternResult::Sequence { range } => (true, 0.75, range.len()),
            PatternResult::Fuzzy { offsets } => {
                let fuzzy_confidence = 0.6 - (offsets.len() as f64 * 0.05).min(0.3);
                (true, fuzzy_confidence.max(0.3), offsets.len())
            }
            PatternResult::NoMatch { reason } => {
                let partial_confidence = if reason.contains("partial") { 0.1 } else { 0.0 };
                (false, partial_confidence, 0)
            }
            PatternResult::CachedPositive {
                cache_hit_count, ..
            } => {
                let cache_confidence = 0.9 + (*cache_hit_count as f64 * 0.01).min(0.09);
                (true, cache_confidence, *cache_hit_count)
            }
            PatternResult::CachedNegative {
                cache_hit_count, ..
            } => (false, 0.0, *cache_hit_count),
            PatternResult::Reject { .. } => (false, 0.0, 0),
            PatternResult::TypeMismatch { position, .. } => (false, 0.05, *position),
            PatternResult::ValueMismatch { position, .. } => (false, 0.03, *position),
            PatternResult::StructureMismatch { .. } => (false, 0.02, 0),
        };

        let mut match_data = HashMap::new();
        match &result {
            PatternResult::Fuzzy { offsets } => {
                match_data.insert("fuzzy_offsets".to_string(), offsets.len().to_string());
            }
            PatternResult::CachedPositive { pattern_id, .. }
            | PatternResult::CachedNegative { pattern_id, .. } => {
                match_data.insert("cached_pattern".to_string(), pattern_id.clone());
            }
            _ => {}
        }

        Self {
            matched,
            confidence,
            tokens_consumed,
            match_data,
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
            node_id: Id::get(&gen_name("pattern_node")),
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
            node_id: Id::get(&gen_name("pattern_variation")),
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
    Failed(String, Id),
    /// Handler successfully processed the tokens
    Handled(Option<Vec<Token>>, Range<usize>, Id),
    /// Handler processed and generated code
    Processed(Option<Vec<Token>>, Range<usize>, Id),
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
    fn detect(
        &self,
        context: &mut Context,
        token_slot: usize,
        token_range: Range<usize>,
    ) -> Result<bool>;

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
        element: ExtractedElement,
    ) -> Result<Option<ConvertedElement>> {
        Ok(None)
    }

    /// Generate documentation for processed elements
    /// Optional: Only implement if handler supports documentation
    fn document(
        &self,
        context: &mut Context,
        extracted: ExtractedElement,
        converted: ConvertedElement,
    ) -> Result<Option<String>> {
        Ok(None)
    }

    /// Generate handler-specific reports
    /// Optional: Return handler processing statistics and debug info
    fn report(&self, context: &mut Context) -> Result<Report> {
        Ok(Report::new(
            Id::get("default_handler_report"),
            Some(Id::get("default")),
            "Handler::report".to_string(),
            "Default handler report".to_string(),
            ReportLevel::Info,
            Phase::Process(None),
        ))
    }

    /// Post-process results from other handlers
    /// Optional: Used for result transformation and finalization
    fn result(
        &self,
        context: &mut Context,
        token_slot: usize,
        token_range: Range<usize>,
        result: HandlerResult,
    ) -> Result<HandlerResult> {
        Ok(result)
    }

    /// Declare which keywords this handler can process
    /// Used by KeywordRouter for efficient handler selection
    /// Return empty vec for handlers that don't have specific keywords (like expression_handler)
    fn supported_keywords(&self) -> Vec<String> {
        Vec::new() // Default: no specific keywords
    }

    /// Handle redirect requests from failed pattern matching
    /// Optional: Route tokens to more appropriate handlers
    fn route(
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
            .eq(other.handlers.iter().map(|(id, _)| id))
            && self.sorted == other.sorted
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
            vec![
                "expression_handler".to_string(),
                "global_handler".to_string(),
            ],
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
    pub fn register(&mut self, handler: Maybe<Arc<dyn Handler>>) {
        if let Maybe::Some(handler) = handler {
            self.handlers.insert(handler.id(), handler);
        }
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

        let tokens = context
            .tokenizer
            .get_tokens(token_slot, token_range.clone());

        context.registry.add_report(
            Report::new(
                Id::get(&format!("process_single_start_{}", token_slot)),
                Some(Id::get("handlizer")),
                "Handlizer::process_single".to_string(),
                format!(
                    "Starting intelligent single handler processing for {} tokens",
                    tokens.len()
                ),
                ReportLevel::Info,
                Phase::Process(None),
            )
                .with_tokens(tokens.len(), 0),
        );

        // Enhanced pattern analysis with multi-criteria selection
        let mut best_handler: Option<&Arc<dyn Handler>> = None;
        let mut best_score = 0.0f64;
        let mut handler_scores = Vec::new();
        let mut fallback_handlers = Vec::new();

        // Multi-pass handler evaluation with advanced scoring
        for handler in self.get_sorted_handlers() {
            match handler.detect(context, token_slot, token_range.clone()) {
                Ok(can_handle) if can_handle => {
                    let pattern_match_score = Handlizer::calculate_pattern_match_score(
                        handler.as_ref(),
                        &tokens,
                        &token_range,
                    );
                    let complexity_score = self.calculate_token_complexity(&tokens);

                    // Weighted composite score
                    let composite_score = (handler.priority() as f64 / 1000.0 * 0.4)
                        + (pattern_match_score * 0.4)
                        + (complexity_score * 0.2);

                    handler_scores.push((handler, composite_score));

                    // Track potential fallback handlers
                    if composite_score > 0.3 {
                        fallback_handlers.push((handler, composite_score));
                    }

                    if composite_score > best_score {
                        best_score = composite_score;
                        best_handler = Some(handler);
                    }

                    context.registry.add_report(Report::new(
                        Id::get(&format!("handler_assessment_{}_{}", handler.id().name(), token_slot)),
                        Some(handler.id()),
                        "Handlizer::process_single".to_string(),
                        format!("Handler assessed - Pattern: {:.3}, Complexity: {:.3}, Final: {:.3}",
                                pattern_match_score, complexity_score, composite_score),
                        ReportLevel::Debug,
                        Phase::Process(None),
                    ).with_tokens(tokens.len(), 0));
                }
                Ok(_) => {
                    context.registry.add_report(Report::new(
                        Id::get(&format!("handler_skip_{}_{}", handler.id().name(), token_slot)),
                        Some(handler.id()),
                        "Handlizer::process_single".to_string(),
                        "Handler cannot process these tokens".to_string(),
                        ReportLevel::Debug,
                        Phase::Process(None),
                    ));
                }
                Err(e) => {
                    context.registry.add_report(
                        Report::new(
                            Id::get(&format!(
                                "handler_evaluation_error_{}_{}",
                                handler.id().name(),
                                token_slot
                            )),
                            Some(handler.id()),
                            "Handlizer::process_single".to_string(),
                            format!("Error evaluating handler: {}", e),
                            ReportLevel::Warning,
                            Phase::Process(None),
                        )
                            .with_success(false),
                    );
                }
            }
        }

        // Sort fallback handlers by score for intelligent fallback
        fallback_handlers
            .sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal));

        // Process with the best handler if found, with intelligent fallback
        if let Some(handler) = best_handler {
            context.registry.add_report(
                Report::new(
                    Id::get(&format!(
                        "best_handler_selected_{}_{}",
                        handler.id().name(),
                        token_slot
                    )),
                    Some(handler.id()),
                    "process".to_string(),
                    format!(
                        "Selected as best handler with score: {:.3} ({} fallbacks available)",
                        best_score,
                        fallback_handlers.len()
                    ),
                    ReportLevel::Info,
                    Phase::Process(None),
                )
                    .with_tokens(tokens.len(), 0),
            );

            // Attempt processing with retry logic
            let mut attempts = 0;
            let max_attempts = 3;
            let mut last_error = None;

            while attempts < max_attempts {
                attempts += 1;

                // Use detect/extract/convert pipeline instead of process()
                match handler.detect(context, token_slot, token_range.clone()) {
                    Ok(true) => {
                        // Try to extract
                        match handler.extract(context, token_slot, token_range.clone()) {
                            Ok(Some(extracted)) => {
                                // Try to convert
                                match handler.convert(context, extracted) {
                                    Ok(Some(_converted)) => {
                                        let elapsed = start_time.elapsed();

                                        context.registry.add_report(Report::new(
                                            Id::get(&format!("process_single_success_{}_{}", handler.id().name(), token_slot)),
                                            Some(handler.id()),
                                            "detect_extract_convert".to_string(),
                                            format!("Handler pipeline completed successfully in {:?} (attempt {})", elapsed, attempts),
                                            ReportLevel::Info,
                                            Phase::Process(None),
                                        ).with_tokens(tokens.len(), tokens.len())
                                            .with_success(true));

                                        return Ok(HandlerResult::Completed(
                                            Some(tokens.clone()),
                                            token_range.clone(),
                                            String::new(),
                                            handler.id(),
                                        ));
                                    }
                                    Ok(None) => {
                                        last_error = Some(crate::C2RError::new(crate::Kind::Unexpected, crate::Reason::Value("Convert returned None"), None));
                                    }
                                    Err(e) => {
                                        last_error = Some(e.clone());
                                    }
                                }
                            }
                            Ok(None) => {
                                last_error = Some(crate::C2RError::new(crate::Kind::Unexpected, crate::Reason::Value("Extract returned None"), None));
                            }
                            Err(e) => {
                                last_error = Some(e.clone());
                            }
                        }
                    }
                    Ok(false) => {
                        last_error = Some(crate::C2RError::new(crate::Kind::Unexpected, crate::Reason::Value("Handler detect returned false"), None));
                    }
                    Err(e) => {
                        last_error = Some(e.clone());
                    }
                }

                if attempts < max_attempts {
                    if let Some(ref e) = last_error {
                        context.registry.add_report(
                            Report::new(
                                Id::get(&format!(
                                    "process_retry_{}_{}",
                                    handler.id().name(),
                                    token_slot
                                )),
                                Some(handler.id()),
                                "Handlizer::process_single".to_string(),
                                format!(
                                    "Handler processing failed on attempt {}, retrying: {}",
                                    attempts, e
                                ),
                                ReportLevel::Warning,
                                Phase::Process(None),
                            )
                                .with_success(false),
                        );

                        // Brief pause before retry
                        std::thread::sleep(std::time::Duration::from_millis(10));
                    }
                }
            }

            // All attempts failed, try fallback handlers
            if let Some(error) = last_error {
                // Try intelligent fallback with top-scoring alternatives
                for (fallback_handler, fallback_score) in fallback_handlers.iter().take(2) {
                    if fallback_handler.id() != handler.id() {
                        context.registry.add_report(
                            Report::new(
                                Id::get(&format!(
                                    "fallback_attempt_{}_{}",
                                    fallback_handler.id().name(),
                                    token_slot
                                )),
                                Some(fallback_handler.id()),
                                "fallback_detect".to_string(),
                                format!(
                                    "Attempting fallback processing with score: {:.3}",
                                    fallback_score
                                ),
                                ReportLevel::Info,
                                Phase::Process(None),
                            )
                                .with_tokens(tokens.len(), 0),
                        );

                        // Use detect/extract/convert for fallback too
                        if let Ok(true) = fallback_handler.detect(context, token_slot, token_range.clone()) {
                            if let Ok(Some(extracted)) = fallback_handler.extract(context, token_slot, token_range.clone()) {
                                if let Ok(Some(_converted)) = fallback_handler.convert(context, extracted) {
                                    let elapsed = start_time.elapsed();

                                    context.registry.add_report(
                                        Report::new(
                                            Id::get(&format!("fallback_success_{}_{}", fallback_handler.id().name(), token_slot)),
                                            Some(fallback_handler.id()),
                                            "fallback_pipeline".to_string(),
                                            format!("Fallback processing succeeded in {:?}", elapsed),
                                            ReportLevel::Info,
                                            Phase::Process(None),
                                        )
                                            .with_tokens(tokens.len(), tokens.len())
                                            .with_success(true),
                                    );

                                    return Ok(HandlerResult::Completed(
                                        Some(tokens.clone()),
                                        token_range.clone(),
                                        String::new(),
                                        fallback_handler.id(),
                                    ));
                                }
                            }
                        }

                        context.registry.add_report(
                            Report::new(
                                Id::get(&format!("fallback_failed_{}_{}", fallback_handler.id().name(), token_slot)),
                                Some(fallback_handler.id()),
                                "fallback_pipeline".to_string(),
                                format!("Fallback processing failed"),
                                ReportLevel::Warning,
                                Phase::Process(None),
                            )
                                .with_success(false),
                        );
                    }
                }

                // Return original error if all fallbacks failed
                return Err(error);
            }
        }

        // No suitable handler found
        let elapsed = start_time.elapsed();
        context.registry.add_report(
            Report::new(
                Id::get(&format!("no_handler_found_{}", token_slot)),
                Some(Id::get("handlizer")),
                "Handlizer::process_single".to_string(),
                format!("No suitable handler found for {} tokens in {:?}", tokens.len(), elapsed),
                ReportLevel::Warning,
                Phase::Process(None),
            )
                .with_tokens(tokens.len(), 0)
                .with_success(false),
        );

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
        let mut nesting_depth = 0u32;
        let mut keyword_weight = 0.0;
        let mut structural_weight = 0.0;

        for token in tokens {
            // Track nesting depth for structural complexity
            match token {
                Token::c('{') | Token::c('(') | Token::c('[') => nesting_depth += 1,
                Token::c('}') | Token::c(')') | Token::c(']') => {
                    nesting_depth = nesting_depth.saturating_sub(1)
                }
                _ => {}
            }
            // Weight tokens by semantic importance
            let token_weight = match token {
                Token::l("if")
                | Token::l("else")
                | Token::l("while")
                | Token::l("for")
                | Token::l("switch") => 2.0,
                Token::l("struct") | Token::l("class") | Token::l("enum") | Token::l("union") => {
                    1.8
                }
                Token::l("int")
                | Token::l("char")
                | Token::l("float")
                | Token::l("double")
                | Token::l("void") => 1.2,
                Token::c('{') | Token::c('}') => 1.5,
                Token::c('(') | Token::c(')') => 1.0,
                Token::c(';') => 0.8,
                _ => 0.5,
            };

            keyword_weight += token_weight;
            structural_weight += nesting_depth as f64 * 0.3;
        }

        let avg_nesting = structural_weight / tokens.len() as f64;
        let avg_keyword_weight = keyword_weight / tokens.len() as f64;

        // Composite complexity score with diminishing returns
        (avg_nesting * 0.6 + avg_keyword_weight * 0.4).min(10.0)
    }

    fn suggest_alternative_handler(
        &mut self,
        tokens: &[Token],
        failed_handler_id: &Id,
    ) -> Option<Id> {
        let mut candidates = Vec::new();
        let token_range = 0..tokens.len();

        // Collect and score all viable alternatives
        for handler in self.get_sorted_handlers() {
            if handler.id() != *failed_handler_id {
                let pattern_score =
                    Self::calculate_pattern_match_score(handler.as_ref(), tokens, &token_range);
                let historical_confidence = self.get_handler_confidence(&handler.id());

                // Composite scoring with historical bias
                let composite_score = (pattern_score * 0.7) + (historical_confidence * 0.3);

                if composite_score > 0.2 {
                    candidates.push((handler.id(), composite_score));
                }
            }
        }

        // Sort by score and return best candidate
        candidates.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal));
        candidates.first().map(|(id, _)| id.clone())
    }

    fn calculate_pattern_match_score(
        handler: &dyn Handler,
        tokens: &[Token],
        range: &Range<usize>,
    ) -> f64 {
        if tokens.is_empty() || range.is_empty() {
            return 0.0;
        }

        let end = range.end.min(tokens.len());
        let start = range.start.min(end);
        let token_slice = &tokens[start..end];

        if token_slice.is_empty() {
            return 0.0;
        }

        // Multi-factor scoring system
        let priority_factor = (handler.priority() as f64 / 1000.0).min(1.0);

        // Enhanced token relevance scoring with pattern recognition
        let relevance_score = Self::calculate_token_relevance(token_slice);

        // Structural complexity bonus for well-formed constructs
        let structure_bonus = Self::calculate_structural_bonus(token_slice);

        // Handler-specific pattern affinity
        let pattern_affinity = Self::calculate_pattern_affinity(handler, token_slice);

        // Weighted composite with normalization
        let raw_score = (priority_factor * 0.3)
            + (relevance_score * 0.4)
            + (structure_bonus * 0.2)
            + (pattern_affinity * 0.1);

        raw_score.min(1.0).max(0.0)
    }

    fn calculate_token_relevance(tokens: &[Token]) -> f64 {
        let mut relevance_sum = 0.0;

        for token in tokens {
            let weight = match token {
                Token::l("if") | Token::l("else") | Token::l("while") | Token::l("for") => 0.9,
                Token::l("struct") | Token::l("class") | Token::l("enum") => 0.85,
                Token::l("int") | Token::l("char") | Token::l("float") | Token::l("double") => 0.75,
                Token::c('{') | Token::c('}') => 0.7,
                Token::c('(') | Token::c(')') => 0.6,
                Token::c(';') | Token::c(',') => 0.4,
                _ => 0.2,
            };
            relevance_sum += weight;
        }

        (relevance_sum / tokens.len() as f64).min(1.0)
    }

    fn calculate_structural_bonus(tokens: &[Token]) -> f64 {
        let mut brace_balance = 0i32;
        let mut paren_balance = 0i32;
        let mut well_formed = true;

        for token in tokens {
            match token {
                Token::c('{') => brace_balance += 1,
                Token::c('}') => {
                    brace_balance -= 1;
                    if brace_balance < 0 {
                        well_formed = false;
                    }
                }
                Token::c('(') => paren_balance += 1,
                Token::c(')') => {
                    paren_balance -= 1;
                    if paren_balance < 0 {
                        well_formed = false;
                    }
                }
                _ => {}
            }
        }

        let balance_bonus = if brace_balance == 0 && paren_balance == 0 && well_formed {
            0.3
        } else {
            0.0
        };
        let length_bonus = (tokens.len() as f64 / 10.0).min(0.2);

        balance_bonus + length_bonus
    }

    fn calculate_pattern_affinity(handler: &dyn Handler, tokens: &[Token]) -> f64 {
        let supported_patterns = handler.supported_patterns();
        if supported_patterns.is_empty() {
            return 0.5; // Neutral affinity
        }

        // Simple heuristic: check if tokens contain keywords relevant to handler patterns
        let mut affinity = 0.0;
        for pattern in &supported_patterns {
            if tokens.iter().any(|token| {
                if let Token::l(literal) = token {
                    pattern.to_lowercase().contains(literal)
                } else {
                    false
                }
            }) {
                affinity += 0.2;
            }
        }

        (affinity / supported_patterns.len() as f64).min(1.0)
    }

    fn get_handler_confidence(&self, handler_id: &Id) -> f64 {
        let success_patterns = self.success_patterns.read().unwrap();
        let failure_patterns = self.failure_patterns.read().unwrap();

        let successes = success_patterns
            .get(&handler_id.name())
            .map(|records| records.len())
            .unwrap_or(0);
        let failures = failure_patterns
            .get(&handler_id.name())
            .map(|records| records.len())
            .unwrap_or(0);

        if successes + failures == 0 {
            return 0.5; // Neutral confidence for new handlers
        }

        let success_rate = successes as f64 / (successes + failures) as f64;
        // Apply Bayesian smoothing with prior belief of 50% success rate
        let smoothed_rate = (successes as f64 + 1.0) / (successes + failures + 2) as f64;

        (success_rate * 0.7) + (smoothed_rate * 0.3)
    }

    // ========================================================================
    // INTEGRATED ADAPTIVE PATTERN MATCHING METHODS
    // ========================================================================
    /// Perform adaptive pattern matching with learning and caching
    pub fn adaptive_match(
        &mut self,
        pattern: &HandlerPattern,
        tokens: &[Token],
    ) -> AdaptivePatternResult {
        // Generate cache key for this pattern-token combination
        let cache_key = self.generate_cache_key(&pattern.id, tokens);

        // Try basic pattern match first
        match self.try_pattern_match(pattern, tokens) {
            Ok(mut result) => {
                // If we got a match, enhance it with adaptive analysis
                if result.matched {
                    result = self.enhance_partial_match(pattern, tokens, result);

                    // Record success for learning
                    self.record_success(&pattern.id, result.tokens_consumed, result.confidence);

                    result
                } else {
                    // No basic match - try pattern variations
                    let variation_result = self.explore_pattern_variations(pattern, tokens);

                    if variation_result.matched {
                        self.record_success(
                            &pattern.id,
                            variation_result.tokens_consumed,
                            variation_result.confidence,
                        );
                        variation_result
                    } else {
                        self.record_failure(
                            &pattern.id,
                            result.tokens_consumed,
                            "No adaptive match found".to_string(),
                        );
                        AdaptivePatternResult::no_match()
                    }
                }
            }
            Err(_) => {
                self.record_failure(
                    &pattern.id,
                    tokens.len(),
                    "Pattern matching error".to_string(),
                );
                AdaptivePatternResult::no_match()
            }
        }
    }

    fn enhance_partial_match(
        &mut self,
        pattern: &HandlerPattern,
        tokens: &[Token],
        partial_result: AdaptivePatternResult,
    ) -> AdaptivePatternResult {
        let mut enhanced = partial_result.clone();

        // Apply confidence boosting based on token alignment
        let alignment_score = self.calculate_token_alignment(tokens, enhanced.tokens_consumed);
        enhanced.confidence = (enhanced.confidence + alignment_score * 0.3).min(1.0);

        // Extend token consumption if pattern suggests it
        if enhanced.tokens_consumed < tokens.len() && enhanced.confidence > 0.6 {
            let extended_tokens = (enhanced.tokens_consumed + 2).min(tokens.len());
            enhanced.tokens_consumed = extended_tokens;
            enhanced.confidence = (enhanced.confidence * 0.95).max(0.4);
        }

        enhanced
    }

    fn calculate_token_alignment(&self, tokens: &[Token], consumed: usize) -> f64 {
        if consumed == 0 || tokens.is_empty() {
            return 0.0;
        }

        let end = consumed.min(tokens.len());
        let consumed_tokens = &tokens[0..end];

        // Calculate structural alignment score
        let mut alignment = 0.0;
        let mut open_braces = 0;
        let mut open_parens = 0;

        for token in consumed_tokens {
            match token {
                Token::c('{') => {
                    open_braces += 1;
                    alignment += 0.1;
                }
                Token::c('}') => {
                    if open_braces > 0 {
                        open_braces -= 1;
                        alignment += 0.2;
                    }
                }
                Token::c('(') => {
                    open_parens += 1;
                    alignment += 0.05;
                }
                Token::c(')') => {
                    if open_parens > 0 {
                        open_parens -= 1;
                        alignment += 0.1;
                    }
                }
                Token::c(';') => alignment += 0.15,
                _ => alignment += 0.02,
            }
        }

        // Bonus for balanced structures
        if open_braces == 0 && open_parens == 0 {
            alignment += 0.3;
        }

        (alignment / consumed_tokens.len() as f64).min(1.0)
    }

    /// Record successful pattern matching with enhanced metrics
    pub fn record_success(&mut self, handler_id: &Id, tokens_consumed: usize, confidence: f64) {
        self.adaptive_stats
            .total_attempts
            .fetch_add(1, Ordering::Relaxed);
        self.adaptive_stats
            .successes
            .fetch_add(1, Ordering::Relaxed);

        let record = SuccessRecord {
            handler_id: handler_id.clone(),
            token_count: tokens_consumed,
            range_size: tokens_consumed,
            confidence: confidence.min(1.0).max(0.0),
            timestamp: SystemTime::now(),
        };

        if let Ok(mut patterns) = self.success_patterns.write() {
            let handler_records = patterns
                .entry(handler_id.name().to_string())
                .or_insert_with(Vec::new);
            handler_records.push(record);

            // Maintain reasonable history size
            if handler_records.len() > 1000 {
                handler_records.drain(0..100);
            }
        }
    }

    /// Record failed pattern matching with categorized reasons
    pub fn record_failure(&mut self, handler_id: &Id, tokens_attempted: usize, reason: String) {
        self.adaptive_stats
            .total_attempts
            .fetch_add(1, Ordering::Relaxed);
        self.adaptive_stats.failures.fetch_add(1, Ordering::Relaxed);

        let record = FailureRecord {
            handler_id: handler_id.clone(),
            token_count: tokens_attempted,
            range_size: tokens_attempted,
            reason: reason.chars().take(256).collect(), // Limit reason length
            timestamp: SystemTime::now(),
        };

        if let Ok(mut patterns) = self.failure_patterns.write() {
            let handler_records = patterns
                .entry(handler_id.name().to_string())
                .or_insert_with(Vec::new);
            handler_records.push(record);

            // Maintain reasonable history size
            if handler_records.len() > 500 {
                handler_records.drain(0..50);
            }
        }
    }

    pub fn analyze_patterns(&mut self, use_cache: bool) -> Result<()> {
        let total_attempts = self.adaptive_stats.total_attempts.load(Ordering::Relaxed);
        let successes = self.adaptive_stats.successes.load(Ordering::Relaxed);
        let failures = self.adaptive_stats.failures.load(Ordering::Relaxed);

        if total_attempts == 0 {
            return Ok(());
        }

        // Analyze pattern tree structures using existing success/failure patterns
        if let (Ok(success_patterns), Ok(failure_patterns)) =
            (self.success_patterns.read(), self.failure_patterns.read())
        {
            for (handler_name, success_records) in success_patterns.iter() {
                // Calculate handler confidence and performance metrics
                if !success_records.is_empty() {
                    let avg_confidence: f64 =
                        success_records.iter().map(|r| r.confidence).sum::<f64>()
                            / success_records.len() as f64;

                    let avg_tokens: f64 =
                        success_records.iter().map(|r| r.token_count).sum::<usize>() as f64
                            / success_records.len() as f64;

                    // Check for low-performing patterns
                    if avg_confidence < 0.3 && success_records.len() > 10 {
                        // Mark low-performing patterns for potential cleanup
                        if let Some(tree) = self.pattern_trees.get_mut(handler_name) {
                            for node in tree.iter_mut() {
                                node.deviation_distance = (node.deviation_distance + 0.1).min(1.0);
                            }
                        }
                        continue;
                    }
                }

                // Cross-reference with failure patterns for comprehensive analysis
                if let Some(failure_records) = failure_patterns.get(handler_name) {
                    let total_handler_attempts = success_records.len() + failure_records.len();
                    let handler_success_rate =
                        success_records.len() as f64 / total_handler_attempts as f64;
                    if let Some(tree) = self.pattern_trees.get_mut(handler_name) {
                        for node in tree.iter_mut() {
                            node.success_rate = handler_success_rate;
                            node.attempts = total_handler_attempts;
                            node.confidence =
                                handler_success_rate * (1.0 - node.deviation_distance);
                        }

                        // Analyze recent failure trends
                        if failure_records.len() >= 5 {
                            let recent_failures =
                                &failure_records[failure_records.len().saturating_sub(5)..];
                            let pattern_failure_rate = recent_failures.len() as f64 / 5.0;
                            if pattern_failure_rate > 0.6 {
                                for node in tree.iter_mut() {
                                    node.deviation_distance =
                                        (node.deviation_distance + 0.05).min(0.8);
                                }
                            }
                        }
                    }
                }
            }
        }
        // Cache analysis using exploration cache
        if use_cache {
            let cache_size = self.exploration_cache.len();
            let cache_effectiveness = if total_attempts > 0 {
                cache_size as f64 / total_attempts as f64
            } else {
                0.0
            };

            // Clear ineffective cache entries based on adaptive stats
            if cache_effectiveness < 0.1 && cache_size > 100 {
                // Identify stale cache entries using failure patterns
                if let Ok(failure_patterns) = self.failure_patterns.read() {
                    let mut stale_keys = Vec::new();

                    for (cache_key, _) in self.exploration_cache.iter() {
                        // Check if cache key corresponds to frequently failing patterns
                        let key_parts: Vec<&str> = cache_key.split('_').collect();
                        if let Some(handler_part) = key_parts.first() {
                            if let Some(failure_records) = failure_patterns.get(*handler_part) {
                                if failure_records.len() > 20 {
                                    let recent_failures = &failure_records
                                        [failure_records.len().saturating_sub(10)..];
                                    let recent_failure_rate = recent_failures.len() as f64 / 10.0;
                                    if recent_failure_rate > 0.7 {
                                        stale_keys.push(cache_key.clone());
                                    }
                                }
                            }
                        }
                    }

                    // Remove stale cache entries
                    for stale_key in stale_keys {
                        self.exploration_cache.remove(&stale_key);
                    }
                }
            }
        }
        Ok(())
    }
    /// Generate comprehensive analysis report with trends and insights
    pub fn analyze_adaptive_performance(&self) -> String {
        let total_attempts = self.adaptive_stats.total_attempts.load(Ordering::Relaxed);
        let successes = self.adaptive_stats.successes.load(Ordering::Relaxed);
        let failures = self.adaptive_stats.failures.load(Ordering::Relaxed);

        let success_rate = if total_attempts > 0 {
            successes as f64 / total_attempts as f64
        } else {
            0.0
        };

        // Calculate cache efficiency
        let cache_hit_rate = if self.exploration_cache.len() > 0 {
            // Simplified metric based on cache size vs total attempts
            (self.exploration_cache.len() as f64 / (total_attempts as f64 + 1.0)) * 100.0
        } else {
            0.0
        };

        // Handler performance breakdown
        let handler_breakdown = self.generate_handler_performance_summary();

        format!(
            "=== ADAPTIVE PATTERN ANALYSIS REPORT ===\n\
         Total attempts: {}\n\
         Successes: {} ({:.1}%)\n\
         Failures: {} ({:.1}%)\n\
         Overall success rate: {:.1}%\n\
         Pattern trees: {}\n\
         Exploration cache: {} entries ({:.1}% efficiency)\n\
         \n\
         === HANDLER PERFORMANCE ===\n\
         {}",
            total_attempts,
            successes,
            (successes as f64 / total_attempts as f64) * 100.0,
            failures,
            (failures as f64 / total_attempts as f64) * 100.0,
            success_rate * 100.0,
            self.pattern_trees.len(),
            self.exploration_cache.len(),
            cache_hit_rate,
            handler_breakdown
        )
    }

    fn generate_cache_key(&self, handler_type: &Id, tokens: &[Token]) -> String {
        let mut hasher = DefaultHasher::new();
        handler_type.name().hash(&mut hasher);
        tokens.len().hash(&mut hasher);

        // Hash strategic token positions for better distribution
        let token_count = tokens.len();
        let sample_positions = if token_count <= 10 {
            (0..token_count).collect::<Vec<_>>()
        } else {
            // Sample beginning, middle, and end tokens
            let mut positions = Vec::new();
            positions.extend(0..3); // First 3
            positions.push(token_count / 2); // Middle
            positions.extend((token_count.saturating_sub(3))..token_count); // Last 3
            positions
        };

        for &pos in &sample_positions {
            if let Some(token) = tokens.get(pos) {
                token.to_string().hash(&mut hasher);
            } else {
                pos.hash(&mut hasher);
            }
        }

        format!("{}_{:x}", handler_type.name(), hasher.finish())
    }

    fn generate_handler_performance_summary(&self) -> String {
        let mut summary = String::new();

        if let (Ok(success_patterns), Ok(failure_patterns)) =
            (self.success_patterns.read(), self.failure_patterns.read())
        {
            let mut handler_stats = HashMap::new();

            // Aggregate success stats with confidence tracking
            for (handler_name, records) in success_patterns.iter() {
                let entry = handler_stats
                    .entry(handler_name.clone())
                    .or_insert((0, 0, 0.0, 0.0));
                entry.0 += records.len();
                if !records.is_empty() {
                    let avg_confidence: f64 =
                        records.iter().map(|r| r.confidence).sum::<f64>() / records.len() as f64;
                    entry.2 = avg_confidence;
                }
            }

            // Aggregate failure stats with recent failure rate
            for (handler_name, records) in failure_patterns.iter() {
                let entry = handler_stats
                    .entry(handler_name.clone())
                    .or_insert((0, 0, 0.0, 0.0));
                entry.1 += records.len();

                // Calculate recent failure rate (last 10 attempts)
                if records.len() >= 10 {
                    let recent_failures = &records[records.len().saturating_sub(10)..];
                    entry.3 = recent_failures.len() as f64 / 10.0;
                }
            }

            // Sort handlers by total activity (successes + failures)
            let mut sorted_handlers: Vec<_> = handler_stats.iter().collect();
            sorted_handlers.sort_by(|a, b| (b.1.0 + b.1.1).cmp(&(a.1.0 + a.1.1)));

            for (handler_name, (successes, failures, avg_confidence, recent_failure_rate)) in
                sorted_handlers.iter().take(10)
            {
                let total = successes + failures;
                if total > 0 {
                    let success_rate = (*successes as f64 / total as f64) * 100.0;
                    summary.push_str(&format!(
                        "{}: {} attempts, {:.1}% success, conf: {:.2}, recent fail rate: {:.1}%\n",
                        handler_name,
                        total,
                        success_rate,
                        avg_confidence,
                        recent_failure_rate * 100.0
                    ));
                }
            }

            if summary.is_empty() {
                summary.push_str("No handler performance data available\n");
            }
        } else {
            summary.push_str("Unable to access performance data (lock contention)\n");
        }

        summary
    }

    // Private helper methods

    fn try_pattern_match(
        &self,
        pattern: &HandlerPattern,
        tokens: &[Token],
    ) -> Result<AdaptivePatternResult> {
        let tokens_len = tokens.len();

        if tokens_len < pattern.min_tokens {
            return Ok(AdaptivePatternResult::from_existing(
                PatternResult::NoMatch {
                    reason: "Insufficient tokens".to_string(),
                },
            ));
        }

        let tokens_consumed = pattern
            .max_tokens
            .map(|max| tokens_len.min(max))
            .unwrap_or(tokens_len);

        let pattern_result = if tokens_consumed >= pattern.min_tokens {
            PatternResult::Match {
                consumed_tokens: tokens_consumed,
            }
        } else {
            PatternResult::NoMatch {
                reason: "Below minimum tokens".to_string(),
            }
        };

        Ok(AdaptivePatternResult::from_existing(pattern_result))
    }

    fn explore_pattern_variations(
        &mut self,
        pattern: &HandlerPattern,
        tokens: &[Token],
    ) -> AdaptivePatternResult {
        use std::collections::VecDeque;

        let mut exploration_queue = VecDeque::with_capacity(64);
        let mut best_result = AdaptivePatternResult::no_match();
        let mut best_score = 0.0;
        let mut explored_count = 0;
        const MAX_EXPLORATIONS: usize = 50;

        // Initialize with existing pattern tree nodes
        if let Some(root_nodes) = self.pattern_trees.get(&pattern.id.name()).cloned() {
            for root_node in root_nodes {
                exploration_queue.push_back((root_node, 0));
            }
        }

        // Breadth-first exploration with early termination
        while let Some((current_node, depth)) = exploration_queue.pop_front() {
            if depth >= self.bounds.max_depth || explored_count >= MAX_EXPLORATIONS {
                break;
            }

            explored_count += 1;

            // Generate and evaluate variations
            let variations = self.generate_pattern_variations(&current_node);
            for variation in variations {
                if variation.deviation_distance > self.bounds.max_deviation {
                    continue;
                }

                // Evaluate pattern match
                if let Ok(result) = self.try_pattern_match(&variation.current_pattern, tokens) {
                    let fuzzy_score = self.calculate_fuzzy_score(&variation, &result);

                    // Track best result
                    if fuzzy_score > best_score {
                        best_score = fuzzy_score;
                        best_result = result.clone();
                    }

                    // Queue promising variations for deeper exploration
                    if result.confidence >= self.bounds.min_confidence
                        && depth < self.bounds.max_depth - 1
                    {
                        let mut updated_variation = variation;
                        updated_variation.update_success(result.matched);
                        exploration_queue.push_back((updated_variation, depth + 1));
                    }
                }
            }
        }

        // Update statistics
        if best_result.matched {
            self.adaptive_stats
                .successful_adaptations
                .fetch_add(1, Ordering::Relaxed);
        } else {
            self.adaptive_stats
                .failed_explorations
                .fetch_add(1, Ordering::Relaxed);
        }

        best_result
    }

    /// Initialize with patterns from existing Patternizer
    pub fn initialize_from_patternizer(&mut self, patternizer: &crate::Patternizer) -> Result<()> {
        let exported_patterns = patternizer.export_patterns_for_adaptation();
        let mut total_imported = 0;

        for (handler_type, patterns) in exported_patterns {
            let root_nodes: Vec<PatternNode> =
                patterns.into_iter().map(PatternNode::new_root).collect();

            total_imported += root_nodes.len();
            self.pattern_trees.insert(handler_type, root_nodes);
        }

        self.adaptive_stats
            .total_attempts
            .store(total_imported, Ordering::Relaxed);
        Ok(())
    }

    /// Generate optimized pattern variations using strategic mutation
    fn generate_pattern_variations(&self, node: &PatternNode) -> Vec<PatternNode> {
        let mut variations = Vec::with_capacity(8);
        let base_pattern = &node.current_pattern;

        // Confidence-based variations (strategic deltas)
        for &delta in &[-0.1, -0.05, 0.05, 0.1] {
            if let Some(variant) = self.create_confidence_variant(base_pattern, delta) {
                let deviation = (delta.abs() / 0.1) * self.bounds.exploration_radius;
                variations.push(node.create_variation(variant, deviation));
            }
        }

        // Priority-based variations
        for &delta in &[-1, 1] {
            if let Some(variant) = self.create_priority_variant(base_pattern, delta) {
                let deviation = (delta.abs() as f64 / 5.0) * self.bounds.exploration_radius;
                variations.push(node.create_variation(variant, deviation));
            }
        }

        // Structural variations with deviation filtering
        for variant in self.create_structural_variants(base_pattern) {
            let deviation = self.calculate_pattern_deviation(&node.base_pattern, &variant);
            if deviation <= self.bounds.max_deviation {
                variations.push(node.create_variation(variant, deviation));
            }
        }

        variations
    }

    /// Calculate pattern deviation using weighted metrics
    fn calculate_pattern_deviation(&self, base: &HandlerPattern, variant: &HandlerPattern) -> f64 {
        let priority_diff = (base.priority as f64 - variant.priority as f64).abs() / 10.0;
        let min_token_diff =
            (base.min_tokens as f64 - variant.min_tokens as f64).abs() / base.min_tokens as f64;

        let max_token_diff = match (base.max_tokens, variant.max_tokens) {
            (Some(base_max), Some(variant_max)) => {
                (base_max as f64 - variant_max as f64).abs() / base_max as f64
            }
            _ => 0.0,
        };

        ((priority_diff * 0.3) + (min_token_diff * 0.4) + (max_token_diff * 0.3)).min(1.0)
    }

    /// Calculate fuzzy logic score with optimized weighting
    fn calculate_fuzzy_score(&self, node: &PatternNode, result: &AdaptivePatternResult) -> f64 {
        let similarity = 1.0 - node.deviation_distance;
        let success_rate = node.success_rate;
        let confidence = result.confidence;
        let penalty = node.deviation_distance * self.fuzzy_scorer.deviation_penalty;

        (similarity * self.fuzzy_scorer.similarity_weight)
            + (success_rate * self.fuzzy_scorer.success_weight)
            + (confidence * self.fuzzy_scorer.confidence_weight)
            - penalty
    }

    /// Create confidence-adjusted pattern variant
    fn create_confidence_variant(
        &self,
        base: &HandlerPattern,
        delta: f64,
    ) -> Option<HandlerPattern> {
        let mut variant = base.clone();

        if delta > 0.0 {
            // Higher confidence: stricter matching
            variant.min_tokens = (variant.min_tokens as f64 * (1.0 + delta)).ceil() as usize;
            if let Some(max) = variant.max_tokens {
                variant.max_tokens = Some((max as f64 * (1.0 + delta * 0.5)).ceil() as usize);
            }
        } else if delta < 0.0 {
            // Lower confidence: more lenient matching
            variant.min_tokens =
                (variant.min_tokens as f64 * (1.0 + delta)).max(1.0).floor() as usize;
            if let Some(max) = variant.max_tokens {
                let new_max =
                    (max as f64 * (1.0 + delta * 0.5)).max(variant.min_tokens as f64 + 1.0);
                variant.max_tokens = Some(new_max.floor() as usize);
            }
        }

        Some(variant)
    }

    /// Create priority-adjusted pattern variant
    fn create_priority_variant(&self, base: &HandlerPattern, delta: i32) -> Option<HandlerPattern> {
        let mut variant = base.clone();
        variant.priority = (variant.priority + delta).max(0);
        Some(variant)
    }

    /// Create structural pattern variants with optimized ranges
    fn create_structural_variants(&self, base: &HandlerPattern) -> Vec<HandlerPattern> {
        let mut variants = Vec::with_capacity(2);

        // Lenient variant (fewer tokens required)
        let mut lenient = base.clone();
        lenient.min_tokens = base.min_tokens.saturating_sub(1).max(1);
        if let Some(max) = base.max_tokens {
            lenient.max_tokens = Some(max + 2);
        }
        variants.push(lenient);

        // Strict variant (more tokens required)
        let mut strict = base.clone();
        strict.min_tokens = base.min_tokens + 1;
        if let Some(max) = base.max_tokens {
            strict.max_tokens = Some(max.saturating_sub(1).max(strict.min_tokens));
        }
        variants.push(strict);

        variants
    }

    // ============================================================================
    // REDIRECT SYSTEM METHODS (integrated from Redirecter)
    // ============================================================================

    /// Process a redirect request with pre-analyzed routing response
    pub fn process_redirect(
        &mut self,
        request: RedirectRequest,
        response: RedirectResponse,
    ) -> Result<HandlerResult> {
        // Check redirect depth limit
        if self.redirect_depth >= self.max_redirect_depth {
            return Ok(HandlerResult::NotHandled(
                None,
                0..0,
                request.from_handler.clone(),
            ));
        }

        // Check for circular redirects
        if self.visited_handlers.contains(&request.from_handler) {
            return Ok(HandlerResult::NotHandled(
                None,
                0..0,
                request.from_handler.clone(),
            ));
        }

        // Add current handler to visited set
        self.visited_handlers.insert(request.from_handler.clone());
        self.redirect_depth += 1;

        // Cache failed patterns from the request
        let handler_name = request.from_handler.to_string();
        self.cache_failed_patterns(handler_name, request.failed_patterns.clone());

        // Convert response to HandlerResult
        if let Some(target_handler) = response.target_handler {
            let token_range = response.modified_range.unwrap_or(request.token_range);
            Ok(HandlerResult::Redirected(
                None,
                token_range,
                response.routing_reason,
                request.from_handler.clone(),
                target_handler,
            ))
        } else {
            Ok(HandlerResult::NotHandled(
                None,
                request.token_range,
                request.from_handler.clone(),
            ))
        }
    }

    /// Analyze patterns and tokens to determine routing target
    fn analyze_and_route(
        &self,
        tokens: &[Token],
        context: &mut Context,
        request: &RedirectRequest,
    ) -> Result<RedirectResponse> {
        let handler_name = request.from_handler.to_string();

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
            let confidence = context
                .patternizer
                .process_handler_patterns(&filtered_tokens, target_handler)?;

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
    pub fn process_analyzed_redirect(
        &mut self,
        request: RedirectRequest,
        response: RedirectResponse,
    ) -> Result<HandlerResult> {
        // Cache failed patterns
        let handler_name = request.from_handler.to_string();
        self.cache_failed_patterns(handler_name, request.failed_patterns.clone());

        // Process redirect with pre-analyzed response
        self.process_redirect(request, response)
    }

    /// Analyze redirect with pre-extracted data to avoid context borrowing issues
    pub fn analyze_redirect_request(
        &mut self,
        context: &mut Context,
        tokens: &[Token],
        request: &RedirectRequest,
    ) -> Result<RedirectResponse> {
        let handler_name = request.from_handler.to_string();

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
            let confidence = context
                .patternizer
                .process_handler_patterns(&filtered_tokens, target_handler)?;

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
            Self::Failed(_, id) => id.timestamp(),
            Self::Handled(_, _, id) => id.timestamp(),
            Self::Processed(_, _, id) => id.timestamp(),
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
            Self::Failed(_, _) => 0,
            Self::Handled(_, range, _) => range.len(),
            Self::Processed(_, range, _) => range.len(),
            Self::Extracted(_, range, _, _) => range.len(),
            Self::Converted(_, range, _, _) => range.len(),
            Self::Completed(_, range, _, _) => range.len(),
            Self::NotHandled(_, range, _) => range.len(),
            Self::Redirected(_, range, _, _, _) => range.len(),
        }
    }

    /// Get the tokens from this result
    pub fn tokens(&self) -> Option<&Vec<Token>> {
        match self {
            Self::Failed(_, _) => None,
            Self::Handled(tokens, _, _) => tokens.as_ref(),
            Self::Processed(tokens, _, _) => tokens.as_ref(),
            Self::Extracted(_, _, _, _) => None,
            Self::Converted(_, _, _, _) => None,
            Self::Completed(tokens, _, _, _) => tokens.as_ref(),
            Self::NotHandled(tokens, _, _) => tokens.as_ref(),
            Self::Redirected(tokens, _, _, _, _) => tokens.as_ref(),
        }
    }
}
impl Display for HandlerResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HandlerResult::Failed(msg, id) => {
                write!(f, "Handler {} failed. Details: {}", id.name(), msg)
            }
            HandlerResult::Handled(tokens, range, id) => {
                let token_info = tokens
                    .as_ref()
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
            HandlerResult::Processed(tokens, range, id) => {
                let token_info = tokens
                    .as_ref()
                    .map(|t| format!(" ({} tokens)", t.len()))
                    .unwrap_or_default();
                write!(
                    f,
                    "Handler: {} reports processing successful (range {}..{}, consumed {} tokens{})",
                    id.name(),
                    range.start,
                    range.end,
                    range.len(),
                    token_info
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
                let token_info = tokens
                    .as_ref()
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
                let token_info = tokens
                    .as_ref()
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
                let token_info = tokens
                    .as_ref()
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
                | HandlerResult::Completed(_, _, code, _)
                | HandlerResult::Extracted(_, _, code, _)
                | HandlerResult::Converted(_, _, code, _)
                | HandlerResult::Redirected(_, _, code, _, _) => Some(code.clone()),
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
                        | HandlerResult::Completed(_, _, _, _)
                        | HandlerResult::Converted(_, _, _, _)
                        | HandlerResult::Extracted(_, _, _, _)
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
    Report(Box<Report>, bool),
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
