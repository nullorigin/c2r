#![allow(
    unsafe_code,
    static_mut_refs,
    unused_variables,
    unused_assignments,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals
)]

use crate::{
    array_handler::ArrayHandler
    ,
    comment_handler::CommentHandler,
    enum_handler::EnumHandler,
    expression_handler::ExpressionHandler,
    function_handler::FunctionHandler,
    gen_name,
    global_handler::GlobalHandler,
    loop_handler::LoopHandler,
    macro_handler::MacroHandler,
    maybe::lock::MaybeLockWriteGuard,
    routing::KeywordRouter,
    struct_handler::StructHandler,
    time,
    typedef_handler::TypedefHandler,
    variable_handler::VariableHandler,
    ConvertedElement,
    Entry,
    ExtractedElement,
    Handler,
    HandlerResult,
    Handlizer,
    Id,
    Maybe,
    MaybeLock

    ,
    Patternizer,
    RedirectRequest,
    RedirectResponse,
    Registry,
    ReportLevel,
    Result,
    Samplizer,
    Token,
    TokenBox,
    Tokenizer,
};
use core::option::Option::{None, Some};
use std::{
    cmp::Ordering,
    collections::HashMap,
    fmt::{Debug, Display},
    hash::{Hash, Hasher},
    ops::Range,
    path::PathBuf,
    sync::Arc
    ,
};

const TOKEN_MAX: usize = 8192;

#[macro_export]
#[allow(static_mut_refs, unsafe_code)]
macro_rules! context {
    [w] => {
        *$crate::config::context()
    };
    [r] => {
        $crate::config::context()
    };
    [r,$field:ident] => {
        $crate::config::context_with(|ctx| &ctx.$field)
    };
    [$field:ident: $value:expr] => {
        $crate::config::context_with_mut(|ctx| ctx.$field = $value)
    };
}

#[allow(static_mut_refs, unsafe_code)]
pub fn context() -> MaybeLockWriteGuard<'static, Context> {
    unsafe {
        static mut CONTEXT: MaybeLock<Context> = MaybeLock::new();
        if let Maybe::Some(guard) = CONTEXT.try_write() {
            guard
        } else {
            CONTEXT.force();
            CONTEXT
                .try_write()
                .unwrap_or_else(|| panic!("Failed to initialize context"))
        }
    }
}
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Context {
    pub tokenizer: Tokenizer,
    pub tokenbox: TokenBox,
    pub patternizer: Patternizer,
    pub samplizer: Samplizer,
    pub handlizer: Handlizer, // Now includes adaptive, analysis, and redirect functionality
    pub registry: Registry,
}
impl Context {
    #[allow(static_mut_refs, unsafe_code)]
    pub fn new() -> Self {
        Context {
            tokenizer: Tokenizer::new(),
            tokenbox: TokenBox::new(Id::get("tokenbox"), TOKEN_MAX),
            patternizer: Patternizer::new(),
            samplizer: Samplizer::new(),
            handlizer: Handlizer::new(), // Handlizer now contains adaptive, analysis, and redirect functionality
            registry: Registry::new(),
        }
    }
    pub fn sync_tokenizer(&self) {
        context().tokenizer = self.tokenizer.clone();
    }

    pub fn sync_tokenbox(&self) {
        context().tokenbox = self.tokenbox.clone();
    }

    pub fn sync_patternizer(&self) {
        context().patternizer = self.patternizer.clone();
    }

    pub fn sync_samplizer(&self) {
        context().samplizer = self.samplizer.clone();
    }

    pub fn sync_analyzer(&self) {
        context().handlizer = self.handlizer.clone();
    }

    pub fn sync_registry(&self) {
        context().registry = self.registry.clone();
    }

    /// Shutdown all background threads and resources
    pub fn shutdown(&mut self) {
        // Shutdown the samplizer's thread pool
        self.samplizer.shutdown();

        // Shutdown global thread pool
        crate::thread::shutdown_global_pool();
    }

    /// Mark tokens as consumed by setting them to Token::n()
    /// Handlers should call this after successfully extracting tokens
    pub fn mark_tokens_consumed(&mut self, token_slot: usize, token_range: Range<usize>) {
        for pos in token_range {
            if let Some(_) = self.tokenizer.slots()[token_slot].get(pos) {
                self.tokenizer.slots()[token_slot].insert(pos, Token::n());
            }
        }
    }

    /// Get the current verbosity level
    pub fn get_verbosity(&self) -> u8 {
        self.registry
            .get_u64("verbosity")
            .map(|v| v as u8)
            .unwrap_or(0)
    }

    /// Set the verbosity level
    pub fn set_verbosity(&mut self, level: u8) {
        self.registry.set_value("verbosity", level as u64);
    }

    pub fn get_base_dir(&self) -> Option<PathBuf> {
        let id = Id::get("base_dir");
        self.registry
            .entries
            .get(&id)
            .and_then(|entry| match entry {
                Entry::Path(path) => Some(path.clone()),
                _ => None,
            })
    }

    /// Set the base directory
    pub fn set_base_dir(&mut self, dir: PathBuf) {
        let id = Id::get("base_dir");
        self.registry.entries.insert(id, Entry::Path(dir));
    }

    /// Set verbosity level
    pub fn with_verbosity(&mut self, level: u8) {
        self.registry.set_value("verbosity", level as u64);
    }

    /// Get system includes setting
    pub fn get_system_includes(&self) -> bool {
        self.registry.get_bool("system_includes").unwrap_or(false)
    }

    /// Set a feature flag with the given name and value
    pub fn set_feature(&mut self, name: &str, value: bool) -> Option<bool> {
        let features_id = Id::get("features");
        match self.registry.entries.get_mut(&features_id) {
            Some(Entry::StrMap(map)) => {
                let previous = map.insert(name.to_string(), Entry::Bool(value));
                previous.and_then(|entry| match entry {
                    Entry::Bool(prev_value) => Some(prev_value),
                    _ => None,
                })
            }
            _ => {
                let mut map = HashMap::new();
                map.insert(name.to_string(), Entry::Bool(value));
                self.registry
                    .entries
                    .insert(features_id, Entry::StrMap(map));
                None
            }
        }
    }

    /// Get the value of a feature flag
    pub fn get_feature(&self, name: &str) -> Option<bool> {
        let features_id = Id::get("features");
        match self.registry.entries.get(&features_id) {
            Some(Entry::StrMap(map)) => match map.get(name) {
                Some(Entry::Bool(value)) => Some(*value),
                _ => None,
            },
            _ => None,
        }
    }

    /// Get an entry by name
    pub fn get_entry(&self, id: &Id) -> Option<Entry> {
        self.registry.entries.get(id).cloned()
    }

    pub fn get_entry_count(&self) -> usize {
        self.registry.entries.len()
    }

    /// Set an entry by name
    pub fn set_entry(&mut self, id: Id, entry: Entry) {
        self.registry.insert(id, entry);
    }
    pub fn initialize(&mut self) {
        self.handlizer.handlers.clear();

        let start_time = std::time::Instant::now();

        // STEP 1: Initialize Patternizer with common patterns for all handlers
        self.patternizer.initialize_common_patterns();

        // STEP 2: Initialize Samplizer patterns (if needed)
        // The Samplizer will automatically learn patterns as handlers use it

        // STEP 3: Register handlers in priority order using batch registration
        let handlers: Vec<Arc<dyn Handler>> = vec![
            Arc::new(FunctionHandler::new()),   // Priority: 200
            Arc::new(StructHandler::new()),     // Priority: 180
            Arc::new(CommentHandler::new()),    // Priority: 170
            Arc::new(EnumHandler::new()),       // Priority: 160
            Arc::new(LoopHandler::new()),       // Priority: 150
            Arc::new(TypedefHandler::new()),    // Priority: 140
            Arc::new(VariableHandler::new()),   // Priority: 140
            Arc::new(ExpressionHandler::new()), // Priority: 120
            Arc::new(ArrayHandler::new()),      // Priority: 110
            Arc::new(MacroHandler::new()),      // Priority: 100
            Arc::new(GlobalHandler::new()),     // Priority: 60
        ];

        let handler_count = handlers.len();
        for handler in handlers {
            self.handlizer.register(Maybe::Some(handler));
        }

        // STEP 4: Initialize Handlizer's adaptive pattern system from Patternizer
        if let Err(e) = self.handlizer.initialize_from_patternizer(&self.patternizer) {
            eprintln!("Warning: Failed to initialize adaptive patterns: {}", e);
        }

        let elapsed = start_time.elapsed();

        let report = Report::new(
            Id::get(&gen_name("handler_initialization")),
            Some(Id::get("Context Initialize")),
            "Context::initialize".to_string(),
            format!(
                "Initialized {} handlers with Patternizer and Samplizer in {:?}ms",
                handler_count,
                elapsed.as_millis()
            ),
            ReportLevel::Info,
            Phase::Initialize(None),
        );

        self.registry.add_report(report);
    }
    pub fn process(
        &mut self,
        token_slot: usize,
        token_range: Range<usize>,
    ) -> Result<Vec<HandlerResult>> {
        let mut results = Vec::new();

        // Fast path for empty ranges
        if token_range.is_empty() {
            return Ok(results);
        }

        let tokens = self.tokenizer.get_tokens(token_slot, token_range.clone());
        if tokens.is_empty() {
            return Ok(results);
        }

        let token_count = tokens.len();
        let sorted_handlers: Vec<_> = self
            .handlizer
            .get_sorted_handlers()
            .into_iter()
            .cloned()
            .collect();

        // INCREMENTAL PROCESSING: Process tokens incrementally without window size limits
        let token_count = token_range.len();
        let mut results: Vec<HandlerResult> = Vec::new();

        // Cycle-based processing: 
        // Cycle 0: positions 0, 10, 20, 30, ...
        // Cycle 1: positions 1, 11, 21, 31, ...
        // ... continue until all positions covered
        const OFFSET_INCREMENT: usize = 10;

        // Build keyword router once from all handlers
        let router = KeywordRouter::from_handlers(&sorted_handlers);
        eprintln!("🧭 KeywordRouter initialized with {} keyword mappings", router.stats().total_keywords);

        // === PHASE 1: DETECTION ===
        // Find all potential patterns without extracting yet
        #[derive(Debug, Clone)]
        struct DetectedPattern {
            handler_id: Id,
            range: Range<usize>,
            window: Range<usize>,
        }
        let mut detected_patterns: Vec<DetectedPattern> = Vec::new();

        eprintln!("\n🔍 === PHASE 1: DETECTION ===");

        for cycle_offset in 0..OFFSET_INCREMENT {
            let mut current_pos = token_range.start + cycle_offset;

            if current_pos >= token_range.end {
                break; // This cycle offset exceeds range
            }

            eprintln!("\n🔄 Detection cycle {} (offset {})", cycle_offset, cycle_offset);

            // Process positions at OFFSET_INCREMENT intervals (0,10,20... or 1,11,21... etc.)
            while current_pos < token_range.end {
                let current_token = self.tokenizer.slots()[token_slot].get(current_pos);

                // Check if token is Token::n() (already processed) or missing
                if matches!(current_token, Some(Token::n())) || current_token.is_none() {
                    eprintln!("   ⏭️  Skipping Token::n() or missing token at position {}", current_pos);
                    current_pos += OFFSET_INCREMENT;
                    continue;
                }

                eprintln!("\n📍 Position {} of {}", current_pos, token_range.end);

                // Try progressively larger windows: 5, 10, 20, 50, 100, rest
                // This prevents handlers from consuming the entire file
                let remaining = token_range.end - current_pos;
                let window_sizes = vec![5, 10, 20, 50, 100, 200, remaining];

                let mut handled = false;
                for window_size in window_sizes {
                    if window_size > remaining {
                        continue;
                    }

                    let test_range = current_pos..(current_pos + window_size).min(token_range.end);

                    // SKIP ranges that contain already processed tokens (Token::n())
                    let has_consumed = test_range.clone().any(|pos| {
                        matches!(self.tokenizer.slots()[token_slot].get(pos), Some(Token::n()))
                    });
                    if has_consumed {
                        eprintln!("   ⏩ Skipping range {:?} - contains Token::n() tokens", test_range);
                        break; // No point trying larger windows if this one has consumed tokens
                    }

                    let range_len = test_range.len();
                    eprintln!("   🔬 Testing range {:?} ({} tokens)", test_range, range_len);

                    // Get tokens for routing
                    let test_tokens = self.tokenizer.slots()[token_slot].tokens()[test_range.clone()].to_vec();

                    // Use keyword routing to get prioritized handler list
                    let routed_handler_ids = router.route(&test_tokens);

                    eprintln!("   🧭 Keyword routing → {} prioritized handlers", routed_handler_ids.len());

                    // Try handlers in routed order (not all handlers!)
                    for handler_id in &routed_handler_ids {
                        // Find handler by ID
                        let handler = sorted_handlers.iter().find(|h| &h.id() == handler_id);

                        if handler.is_none() {
                            continue; // Handler not found in registry
                        }

                        let handler = handler.unwrap();
                        if let Ok(true) = handler.detect(self, token_slot, test_range.clone()) {
                            eprintln!("      ✅ Handler {} detected pattern at {:?}", handler.id().name(), test_range);

                            // Just record the detection - we'll extract/convert later
                            detected_patterns.push(DetectedPattern {
                                handler_id: handler.id(),
                                range: test_range.clone(), // Will be updated during extraction
                                window: test_range.clone(),
                            });

                            handled = true;
                            break; // One handler per window
                        }
                    }

                    if handled {
                        break; // Break from window size loop
                    }
                } // End of window size loop

                // Move to next position
                current_pos += OFFSET_INCREMENT;
            } // End of while loop
        } // End of cycle loop

        eprintln!("\n📊 Detection complete: {} patterns detected", detected_patterns.len());

        // === DEDUPLICATION ===
        // Remove overlapping patterns - prioritize by handler priority, then by position
        eprintln!("\n🔀 Deduplicating overlapping patterns...");

        // First, sort by handler priority (higher priority first), then by start position
        detected_patterns.sort_by(|a, b| {
            // Find handler priorities
            let a_handler = sorted_handlers.iter().find(|h| h.id() == a.handler_id);
            let b_handler = sorted_handlers.iter().find(|h| h.id() == b.handler_id);

            let a_priority = a_handler.map(|h| h.priority()).unwrap_or(0);
            let b_priority = b_handler.map(|h| h.priority()).unwrap_or(0);

            // Higher priority first, then earlier position
            b_priority.cmp(&a_priority)
                .then(a.window.start.cmp(&b.window.start))
        });

        let original_count = detected_patterns.len();
        let mut deduplicated: Vec<DetectedPattern> = Vec::new();
        for pattern in detected_patterns {
            // Check if this pattern overlaps with any already-kept pattern
            let overlaps = deduplicated.iter().any(|existing| {
                // Check if ranges overlap
                pattern.window.start < existing.window.end && existing.window.start < pattern.window.end
            });

            if !overlaps {
                deduplicated.push(pattern);
            } else {
                eprintln!("   ⏩ Skipping overlapping pattern at {:?}", pattern.window);
            }
        }

        eprintln!("   ✅ Deduplicated: {} → {} patterns", original_count, deduplicated.len());
        let detected_patterns = deduplicated; // Replace with deduplicated list

        // === PHASE 2: EXTRACTION ===
        eprintln!("\n🔧 === PHASE 2: EXTRACTION ===");

        #[derive(Debug, Clone)]
        struct ExtractedData {
            element: ExtractedElement,
            handler_id: Id,
        }
        let mut extracted_data: Vec<ExtractedData> = Vec::new();

        for detected in &detected_patterns {
            // Skip if any tokens in this range are already consumed
            let all_tokens = self.tokenizer.slots()[token_slot].tokens();
            let already_consumed = detected.window.clone().any(|pos| {
                pos < all_tokens.len() && matches!(all_tokens[pos], Token::n())
            });

            if already_consumed {
                eprintln!("   ⏩ Skipping {:?} - tokens already consumed", detected.window);
                continue;
            }

            let handler = sorted_handlers.iter().find(|h| h.id() == detected.handler_id);
            if let Some(handler) = handler {
                eprintln!("   🔍 Extracting with {} at {:?}", handler.id().name(), detected.window);

                match handler.extract(self, token_slot, detected.window.clone()) {
                    Ok(Some(extracted)) => {
                        eprintln!("      ✅ Extracted successfully");
                        extracted_data.push(ExtractedData {
                            element: extracted,
                            handler_id: handler.id(),
                        });
                    }
                    Ok(None) => {
                        eprintln!("      ⚠️  extract() returned None");
                    }
                    Err(e) => {
                        eprintln!("      ❌ extract() error: {}", e);
                    }
                }
            }
        }

        eprintln!("\n📊 Extraction complete: {} elements extracted", extracted_data.len());

        // === PHASE 3: CONVERSION ===
        eprintln!("\n⚙️  === PHASE 3: CONVERSION ===");

        for data in &extracted_data {
            let handler = sorted_handlers.iter().find(|h| h.id() == data.handler_id);
            if let Some(handler) = handler {
                eprintln!("   🔄 Converting with {}", handler.id().name());

                match handler.convert(self, data.element.clone()) {
                    Ok(Some(converted)) => {
                        let code = match converted {
                            ConvertedElement::Variable(v) => v.code,
                            ConvertedElement::Function(f) => f.code,
                            ConvertedElement::Struct(s) => s.code,
                            ConvertedElement::Enum(e) => e.code,
                            ConvertedElement::Global(g) => g.code,
                            _ => String::new(),
                        };
                        eprintln!("      ✅ Converted successfully");
                        results.push(HandlerResult::Completed(
                            None,
                            token_range.clone(),
                            code,
                            handler.id(),
                        ));
                    }
                    Ok(None) => {
                        eprintln!("      ⚠️  convert() returned None");
                    }
                    Err(e) => {
                        eprintln!("      ❌ convert() error: {}", e);
                    }
                }
            }
        }

        eprintln!("\n📊 Conversion complete: {} results", results.len());

        // Count consumed tokens
        let consumed_count = (token_range.start..token_range.end)
            .filter(|&pos| matches!(self.tokenizer.slots()[token_slot].get(pos), Some(Token::n())))
            .count();

        eprintln!("\n🎉 All phases complete:");
        eprintln!("   📊 Total results: {}", results.len());
        eprintln!("   🔒 Tokens marked as Token::n(): {} / {}", consumed_count, token_count);
        eprintln!("   ♻️  Tokens remaining: {}", token_count - consumed_count);

        Ok(results)
    }

    pub fn route(
        &mut self,
        token_slot: usize,
        token_range: Range<usize>,
        request: RedirectRequest,
    ) -> Result<HandlerResult> {
        let tokens = self.tokenizer.get_tokens(token_slot, token_range.clone());
        let handler_name = request.from_handler.to_string();

        // Filter tokens efficiently
        let filtered_tokens: Vec<Token> = tokens
            .into_iter()
            .filter(|token| !matches!(*token, Token::n()))
            .collect();

        let response = if filtered_tokens.is_empty() {
            RedirectResponse::default()
        } else {
            // Get routing targets early to avoid repeated lookups
            let potential_targets = self
                .handlizer
                .routing_rules
                .get(&handler_name)
                .cloned()
                .unwrap_or_default();

            // Find best match with early termination
            let best_match = potential_targets
                .iter()
                .filter(|target| {
                    !self
                        .handlizer
                        .visited_handlers
                        .iter()
                        .any(|h| h.to_string() == **target)
                })
                .filter_map(|target| {
                    self.patternizer
                        .process_handler_patterns(&filtered_tokens, target)
                        .ok()
                        .map(|confidence| (target, confidence))
                })
                .max_by(|(_, a), (_, b)| a.partial_cmp(b).unwrap_or(Ordering::Equal));

            match best_match {
                Some((target, confidence)) if confidence > 0.6 => RedirectResponse {
                    target_handler: Some(Id::get(target)),
                    modified_range: None,
                    routing_reason: format!(
                        "Pattern analysis suggests {} (confidence: {:.2})",
                        target, confidence
                    ),
                    should_retry: true,
                    metadata: Vec::new(),
                },
                Some((_, confidence)) => RedirectResponse {
                    target_handler: None,
                    modified_range: None,
                    routing_reason: format!("Low confidence routing (max: {:.2})", confidence),
                    should_retry: false,
                    metadata: Vec::new(),
                },
                None => RedirectResponse {
                    target_handler: None,
                    modified_range: None,
                    routing_reason: "No suitable routing target found".to_string(),
                    should_retry: false,
                    metadata: Vec::new(),
                },
            }
        };

        self.handlizer.process_analyzed_redirect(request, response)
    }
    /// Enhanced redirect system with intelligent routing and learning
    pub fn try_route(
        &mut self,
        token_slot: usize,
        token_range: Range<usize>,
        result: HandlerResult,
        handler_id: &Id,
    ) -> Result<HandlerResult> {
        let tokens = self.tokenizer.get_tokens(token_slot, token_range.clone());

        // Early exit for empty tokens
        if tokens.is_empty() {
            return Ok(HandlerResult::NotHandled(
                None,
                token_range,
                handler_id.clone(),
            ));
        }

        // Create redirect request with pre-allocated capacity
        let redirect_request = RedirectRequest {
            from_handler: handler_id.clone(),
            token_range: token_range.clone(),
            failed_patterns: Vec::with_capacity(1),
            suggested_handler: None,
            metadata: Vec::new(),
        };

        // Direct redirect handling without timing overhead
        match self.route(token_slot, token_range.clone(), redirect_request) {
            Ok(redirect_result) => {
                if !matches!(redirect_result, HandlerResult::NotHandled(_, _, _)) {
                    return Ok(redirect_result);
                }
            }
            Err(_) => {
                // Silent failure handling - avoid logging overhead
            }
        }

        // Return optimized NotHandled result
        Ok(HandlerResult::NotHandled(
            Some(tokens),
            token_range,
            handler_id.clone(),
        ))
    }
    /// Get a boolean value from the registry
    pub fn get_bool(&self, name: &str) -> Option<bool> {
        self.registry.get_bool(name)
    }

    /// Get a u64 value from the registry
    pub fn get_u64(&self, name: &str) -> Option<u64> {
        self.registry.get_u64(name)
    }

    /// Set a value in the registry
    pub fn set_value(&mut self, name: &str, value: Entry) {
        let id = Id::get(name);
        self.registry.insert(id, value);
    }

    /// Get include directories
    pub fn get_include_dirs(&self) -> Vec<PathBuf> {
        self.registry
            .entries
            .get(&Id::get("include_directories"))
            .and_then(|entry| match entry {
                Entry::List(paths) => Some(
                    paths
                        .iter()
                        .filter_map(|e| match e {
                            Entry::Path(p) => Some(p.clone()),
                            _ => None,
                        })
                        .collect(),
                ),
                _ => None,
            })
            .unwrap_or_else(Vec::new)
    }

    /// Set include directories
    pub fn set_include_dirs(&mut self, paths: Vec<PathBuf>) {
        self.registry.insert(
            Id::get("include_directories"),
            Entry::List(paths.into_iter().map(Entry::Path).collect()),
        );
    }

    /// Register a handler with the context
    pub fn register_handler(&mut self, handler: Box<dyn Handler>) {
        let id = handler.id();
        self.registry.insert(id, Entry::Handler(handler));
    }

    // ===== SAMPLIZER DELEGATION FUNCTIONS =====

    /// Get validation results from the global Samplizer
    pub fn get_validation_results(self) -> HashMap<Id, crate::ValidationResult> {
        self.samplizer.validation_results.clone()
    }

    /// Collect tokens from TokenBox for processing
    pub fn collect_tokens(&self, max_tokens: Option<usize>) -> Result<Vec<Token>> {
        self.tokenbox.collect_tokens(max_tokens)
    }

    /// Check TokenBox status
    pub fn tokenbox_status(&self) -> crate::TokenBoxStatus {
        self.tokenbox.status()
    }

    // ===== ADAPTIVE ANALYSIS DELEGATION FUNCTIONS =====
    // Analysis functionality now integrated into Handlizer

    /// Record successful pattern matching for learning
    pub fn record_pattern_success(
        &mut self,
        handler_id: &Id,
        tokens_consumed: usize,
        confidence: f64,
    ) {
        self.handlizer
            .record_success(handler_id, tokens_consumed, confidence);
    }

    /// Record failed pattern matching for learning  
    pub fn record_pattern_failure(
        &mut self,
        handler_id: &Id,
        tokens_attempted: usize,
        reason: String,
    ) {
        self.handlizer
            .record_failure(handler_id, tokens_attempted, reason);
    }


    /// Add a handler report to the centralized reporting system
    pub fn add_report(&mut self, report: Report) {
        self.registry.add_report(report);
    }

    /// Create and add a report
    pub fn report(
        &mut self,
        handler_name: &str,
        function_name: &str,
        level: ReportLevel,
        phase: Phase,
        message: &str,
        success: bool,
    ) {
        let report_id = Id::get(&gen_name(&format!(
            "report_{}_{}",
            handler_name, function_name
        )));
        let handler_id = Id::get(handler_name);

        let report = Report {
            report_id,
            handler_id,
            function_name: function_name.to_string(),
            message: message.to_string(),
            details: None,
            level,
            tokens_processed: 0,
            tokens_consumed: 0,
            phase,
            success,
            metadata: HashMap::new(),
        };

        self.add_report(report);
    }

    /// Create and add a report with token counts
    pub fn report_with_tokens(
        &mut self,
        handler_name: &str,
        function_name: &str,
        level: ReportLevel,
        phase: Phase,
        message: &str,
        success: bool,
        tokens_processed: usize,
        tokens_consumed: usize,
    ) {
        let report_id = Id::get(&gen_name(&format!(
            "report_{}_{}",
            handler_name, function_name
        )));
        let handler_id = Id::get(handler_name);

        let report = Report {
            report_id,
            handler_id,
            function_name: function_name.to_string(),
            message: message.to_string(),
            details: None,
            level,
            tokens_processed,
            tokens_consumed,
            phase,
            success,
            metadata: HashMap::new(),
        };

        self.registry.add_report(report);
    }

    /// Get all handler reports from the registry
    pub fn get_reports(&self) -> Vec<Report> {
        self.registry.get_reports()
    }

    /// Get reports filtered by handler name
    pub fn get_reports_by_handler(&self, handler_name: &str) -> Vec<Report> {
        self.registry.get_reports_by_handler(handler_name)
    }

    /// Get reports filtered by report level
    pub fn get_reports_by_level(&self, level: &ReportLevel) -> Vec<Report> {
        self.registry.get_reports_by_level(level)
    }

    /// Get reports filtered by handler phase
    pub fn get_reports_by_phase(&self, phase: &Phase) -> Vec<Report> {
        self.registry.get_reports_by_phase(phase)
    }

    /// Display all reports using the sophisticated Range-based table system
    pub fn display_reports(&self) {
        let reports = self.registry.get_reports();
        self.display_reports_with_table(&reports);
    }

    /// Display reports using the new Range-based table system
    fn display_reports_with_table(&self, reports: &[Report]) {
        if reports.is_empty() {
            println!("No reports to display");
            return;
        }

        // Create table using the new Range-based coordinate system
        let mut table = crate::Table::new_registry_table("Handler System Reports");

        // Add headers with better spacing
        table.add_header(vec![
            "Handler", "Status", "Function", "Phase", "Tokens", "Message",
        ]);

        // Add report data (limit to 20 for readability)
        for report in reports.iter().take(20) {
            let handler_name = truncate_string(&report.handler_name(), 19);
            let status = if report.success { "✅" } else { "❌" };
            let function_name = truncate_string(&report.function_name, 15);
            let phase = format!("{}", report.phase);
            let message = truncate_string(&report.message, 25);

            table.add_row(vec![
                Entry::Str(handler_name),
                Entry::Str(status.to_string()),
                Entry::Str(function_name),
                Entry::Str(phase),
                Entry::Val(report.tokens_processed as u64),
                Entry::Str(message),
            ]);
        }

        // Display the formatted table
        println!("{}", table.display_formatted());

        // Show additional info
        if reports.len() > 20 {
            println!("📝 Showing first 20 of {} reports", reports.len());
        }

        let success_count = reports.iter().filter(|r| r.success).count();
        let success_rate = (success_count as f64 / reports.len() as f64) * 100.0;
        println!(
            "📈 Success Rate: {:.1}% ({}/{} successful)\n",
            success_rate,
            success_count,
            reports.len()
        );
    }

    /// Simple report display that doesn't require Context access
    pub fn display_reports_simple(&self, reports: &[Report]) {
        if reports.is_empty() {
            println!("📚 No reports available");
            return;
        }

        println!("\n=== 🔧 HANDLER SYSTEM REPORTS ===");
        println!("📊 Total Reports: {}", reports.len());

        let success_count = reports.iter().filter(|r| r.success).count();
        let success_rate = (success_count as f64 / reports.len() as f64) * 100.0;
        println!("✅ Success Rate: {:.1}%\n", success_rate);

        // Group by handler for cleaner display
        let mut handler_groups: HashMap<String, Vec<&Report>> = HashMap::new();
        for report in reports {
            handler_groups
                .entry(report.handler_name().to_string())
                .or_default()
                .push(report);
        }

        for (handler_name, handler_reports) in handler_groups {
            println!("🔨 {}: {} reports", handler_name, handler_reports.len());
            for report in handler_reports.iter().take(3) {
                // Show max 3 per handler
                let status = if report.success { "✅" } else { "❌" };
                println!("   {} {}: {}", status, report.function_name, report.message);
                if report.tokens_processed > 0 {
                    println!(
                        "      📊 Tokens: {} processed, {} consumed",
                        report.tokens_processed, report.tokens_consumed
                    );
                }
            }
            if handler_reports.len() > 3 {
                println!("   ... and {} more reports", handler_reports.len() - 3);
            }
            println!();
        }

        println!("=== END REPORTS ===\n");
    }

    /// Display reports filtered by level (delegates to Registry)
    pub fn display_reports_by_level(&self, level: &ReportLevel) {
        let reports = self.get_reports_by_level(level);
        self.display_reports_simple(&reports);
    }

    /// Display reports in table format - NEW FUNCTION for better formatting
    pub fn display_reports_table(&self) {
        // Get reports in one atomic operation
        let reports = self.get_reports();

        // Process reports without holding any Context locks
        self.display_reports_table_format(&reports);
    }

    /// Create and display a formatted table from reports using Range-based coordinates
    fn display_reports_table_format(&self, reports: &[Report]) {
        if reports.is_empty() {
            println!("📋 No reports available for table display\n");
            return;
        }

        // Use the sophisticated Range-based table system
        self.display_reports_with_table(reports);
    }

    /// Clear all reports from the registry (delegates to Registry)
    pub fn clear_reports(&mut self) {
        self.registry.clear_reports();
    }

    /// Display the entire registry in a database-like format for debugging
    pub fn display_registry_database(&self) {
        println!("🗃️  Context Registry Database View:");
        let reports = self.registry.get_reports();
        self.display_reports_table_format(&reports);
    }

    /// Display registry statistics for debugging
    pub fn display_registry_stats(&self) {
        println!("📊 Context Registry Statistics:");
        let reports = self.registry.get_reports();

        if reports.is_empty() {
            println!("No reports available for statistics\n");
            return;
        }

        let mut handler_counts: HashMap<String, usize> = HashMap::new();
        let mut phase_counts: HashMap<Phase, usize> = HashMap::new();
        let mut level_counts: HashMap<ReportLevel, usize> = HashMap::new();

        for report in &reports {
            *handler_counts
                .entry(report.handler_name().to_string())
                .or_insert(0) += 1;
            *phase_counts.entry(report.phase.clone()).or_insert(0) += 1;
            *level_counts.entry(report.level.clone()).or_insert(0) += 1;
        }

        println!("By Handler:");
        for (handler, count) in handler_counts {
            println!("  - {}: {}", handler, count);
        }

        println!("By Phase:");
        for (phase, count) in phase_counts {
            println!("  - {}: {}", phase, count);
        }

        println!("By Level:");
        for (level, count) in level_counts {
            println!("  - {}: {}", level, count);
        }
        println!();
    }

    /// Display all context information including registry and reports
    pub fn display_full_context(&self) {
        println!("FULL CONTEXT DISPLAY");
        println!("===============================================================");

        // Display basic context info
        let reports = self.registry.get_reports();
        println!("Context Overview:");
        println!("  - Reports: {}", reports.len());
        println!();

        // Display registry database view
        self.display_registry_database();
        println!();

        // Display registry statistics
        self.display_registry_stats();
        println!();

        // Display reports summary
        if !reports.is_empty() {
            println!("Reports Summary:");
            let mut level_counts: HashMap<ReportLevel, i32> = HashMap::new();
            for report in reports {
                *level_counts.entry(report.level.clone()).or_insert(0) += 1;
            }
            for (level, count) in level_counts {
                println!("  - {}: {}", level, count);
            }
            println!();
        }

        println!("===============================================================");
    }
}

/// Helper function to truncate strings for table display
fn truncate_string(s: &str, max_len: usize) -> String {
    if s.len() <= max_len {
        format!("{:<width$}", s, width = max_len)
    } else {
        format!(
            "{:<width$}",
            format!("{}...", &s[..max_len - 3]),
            width = max_len
        )
    }
}

/// Comprehensive handler reporting structure for centralized debugging and statistics
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Report {
    pub report_id: Id,
    pub handler_id: Id,
    pub function_name: String,
    pub message: String,
    pub details: Option<String>,
    pub level: ReportLevel,
    pub tokens_processed: usize,
    pub tokens_consumed: usize,
    pub phase: Phase,
    pub success: bool,
    pub metadata: HashMap<String, String>,
}

impl Report {
    pub fn timestamp(&self) -> u128 {
        self.handler_id.timestamp()
    }
    pub fn report_id(&self) -> &Id {
        &self.report_id
    }
    pub fn report_name(&self) -> String {
        self.report_id.name()
    }

    pub fn handler_id(&self) -> &Id {
        &self.handler_id
    }
    pub fn handler_name(&self) -> String {
        self.handler_id.name()
    }

    pub fn function_name(&self) -> String {
        self.function_name.clone()
    }

    pub fn message(&self) -> String {
        self.message.clone()
    }

    pub fn level(&self) -> &ReportLevel {
        &self.level
    }

    pub fn tokens_processed(&self) -> usize {
        self.tokens_processed
    }

    pub fn tokens_consumed(&self) -> usize {
        self.tokens_consumed
    }

    pub fn phase(&self) -> &Phase {
        &self.phase
    }

    pub fn success(&self) -> bool {
        self.success
    }

    pub fn metadata(&self) -> &HashMap<String, String> {
        &self.metadata
    }
}

impl PartialOrd for Report {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Report {
    fn cmp(&self, other: &Self) -> Ordering {
        self.report_id
            .cmp(&other.report_id)
            .then_with(|| self.handler_id.cmp(&other.handler_id))
            .then_with(|| self.phase.cmp(&other.phase))
            .then_with(|| self.tokens_processed.cmp(&other.tokens_processed))
            .then_with(|| self.level.cmp(&other.level))
            .then_with(|| self.function_name.cmp(&other.function_name))
            .then_with(|| self.message.cmp(&other.message))
            .then_with(|| self.success.cmp(&other.success))
            .then_with(|| self.metadata.values().cmp(other.metadata.values()))
            .then_with(|| self.metadata.keys().cmp(other.metadata.keys()))
    }
}

impl Hash for Report {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.report_id.hash(state);
        self.handler_id.hash(state);
        self.function_name.hash(state);
        self.message.hash(state);
        self.level.hash(state);
        self.tokens_processed.hash(state);
        self.tokens_consumed.hash(state);
        self.phase.hash(state);
        self.success.hash(state);
        for (k, v) in &self.metadata {
            k.hash(state);
            v.hash(state);
        }
    }
}
impl Display for Report {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{}] {} | {} | {} | {}",
            self.timestamp(),
            self.level(),
            self.handler_name(),
            self.phase(),
            self.message()
        )
    }
}

/// Handler processing phases
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Phase {
    Initialize(Option<String>),
    Analyze(Option<String>),
    Process(Option<String>),
    Extract(Option<String>),
    Convert(Option<String>),
    Report(Option<String>),
    Result(Option<String>),
    Redirect(Option<String>),
}

impl std::fmt::Display for Phase {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Phase::Initialize(s) => write!(f, "INITIALIZE: {}", s.clone().unwrap_or("".to_string())),
            Phase::Analyze(s) => write!(f, "ANALYZE: {}", s.clone().unwrap_or("".to_string())),
            Phase::Process(s) => write!(f, "PROCESS: {}", s.clone().unwrap_or("".to_string())),
            Phase::Extract(s) => write!(f, "EXTRACT: {}", s.clone().unwrap_or("".to_string())),
            Phase::Convert(s) => write!(f, "CONVERT: {}", s.clone().unwrap_or("".to_string())),
            Phase::Report(s) => write!(f, "REPORT: {}", s.clone().unwrap_or("".to_string())),
            Phase::Result(s) => write!(f, "RESULT: {}", s.clone().unwrap_or("".to_string())),
            Phase::Redirect(s) => write!(f, "REDIRECT: {}", s.clone().unwrap_or("".to_string())),
        }
    }
}

/// Pending redirect structure for deferred handler processing
/// This allows redirect functions to queue up handler calls without direct function access
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HandlerRedirect {
    pub source_handler_id: Id,
    pub target_handler_id: Id,
    pub directive_type: String,
    pub tokens: Vec<Token>,
    pub original_result: String, // Serialized HandlerResult for later processing
    timestamp: u128,
}

impl HandlerRedirect {
    pub fn new(
        source_handler_id: Id,
        target_handler_id: Id,
        directive_type: String,
        tokens: Vec<Token>,
        original_result: String,
    ) -> Self {
        Self {
            source_handler_id,
            target_handler_id,
            directive_type,
            tokens,
            original_result,
            timestamp: time!(),
        }
    }
    pub fn timestamp(&self) -> u128 {
        self.timestamp.clone()
    }
    pub fn source_handler_id(&self) -> Id {
        self.source_handler_id.clone()
    }

    pub fn source_handler_name(&self) -> String {
        self.source_handler_id.name()
    }
    pub fn target_handler_name(&self) -> String {
        self.target_handler_id.name()
    }
    pub fn target_handler_id(&self) -> Id {
        self.target_handler_id.clone()
    }
    pub fn directive_type(&self) -> String {
        self.directive_type.clone()
    }
    pub fn tokens(&self) -> Vec<Token> {
        self.tokens.clone()
    }
    pub fn original_result(&self) -> String {
        self.original_result.clone()
    }
}

impl std::fmt::Display for HandlerRedirect {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Redirect({} -> {} | {} | {} tokens)",
            self.source_handler_id(),
            self.target_handler_id(),
            self.directive_type(),
            self.tokens().len().to_string()
        )
    }
}

impl Report {
    pub fn new(
        report_id: Id,
        handler_id: Option<Id>,
        function_name: String,
        message: String,
        level: ReportLevel,
        phase: Phase,
    ) -> Self {
        Report {
            report_id,
            handler_id: handler_id.unwrap_or_else(|| Id::get("unknown")),
            function_name,
            message,
            details: None,
            level,
            tokens_processed: 0,
            tokens_consumed: 0,
            phase,
            success: true,
            metadata: HashMap::new(),
        }
    }
    /// Set token statistics
    pub fn with_tokens(mut self, processed: usize, consumed: usize) -> Self {
        self.tokens_processed = processed;
        self.tokens_consumed = consumed;
        self
    }

    /// Set success status
    pub fn with_success(mut self, success: bool) -> Self {
        self.success = success;
        self
    }

    /// Add metadata key-value pair
    pub fn with_metadata(mut self, key: String, value: String) -> Self {
        self.metadata.insert(key, value);
        self
    }
}
