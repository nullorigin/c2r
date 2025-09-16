#![allow(
    unused_variables,
    unused_assignments,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals
)]

use crate::config::{HandlerPhase::Process, HandlerRedirect, HandlerReport};
use crate::convert::ConvertedElement;
use crate::document::ElementInfo;
use crate::entry::Entry;
use crate::error::C2RError;
use crate::extract::ExtractedElement;
use crate::lock::Id;
use crate::logging::ReportLevel;
use crate::{Kind, Reason, Result};
use crate::{Token, context, report};
use std::collections::HashMap;
use std::fmt::Display;
use std::hash::Hash;
use std::ops::Range;

#[derive(Debug, Clone, Eq, Hash)]
pub struct Handler {
    pub id: Id,
    pub role: String,
    pub priority: u64,
    pub process: Option<fn(Range<usize>) -> Result<bool>>,
    pub handle: Option<fn(Range<usize>) -> Result<HandlerResult>>,
    pub extract: Option<fn(Range<usize>) -> Result<Option<ExtractedElement>>>,
    pub convert: Option<fn(Range<usize>) -> Result<Option<ConvertedElement>>>,
    pub document: Option<fn(ElementInfo) -> Result<Option<String>>>,
    pub report: Option<fn() -> Result<HandlerReport>>,
    pub result: Option<fn(Range<usize>, HandlerResult) -> Result<HandlerResult>>,
    pub redirect: Option<fn(Range<usize>, HandlerResult) -> Result<HandlerResult>>,
}

impl Ord for Handler {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.priority
            .cmp(&other.priority)
            .then_with(|| self.id.cmp(&other.id))
            .then_with(|| self.role.cmp(&other.role))
    }
}

impl PartialOrd for Handler {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for Handler {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id && self.role == other.role && self.priority == other.priority
    }
}
impl Handler {
    pub fn new(id: Id, role: String, priority: u64) -> Handler {
        Handler {
            id,
            role,
            priority,
            process: None,
            handle: None,
            extract: None,
            convert: None,
            document: None,
            report: None,
            result: None,
            redirect: None,
        }
    }
    pub fn num(&self) -> u128 {
        self.id.num()
    }
    pub fn name(&self) -> &str {
        &self.id.name()
    }
    pub fn role(&self) -> &str {
        &self.role
    }
    pub fn priority(&self) -> u64 {
        self.priority
    }
    pub fn process(&self, token_range: Range<usize>) -> Result<bool> {
        self.process.unwrap()(token_range)
    }
    pub fn handle(&self, token_range: Range<usize>) -> Result<HandlerResult> {
        self.handle.unwrap()(token_range)
    }
    pub fn extract(&self, token_range: Range<usize>) -> Result<Option<ExtractedElement>> {
        self.extract.unwrap()(token_range)
    }
    pub fn convert(&self, token_range: Range<usize>) -> Result<Option<ConvertedElement>> {
        self.convert.unwrap()(token_range)
    }
    pub fn document(&self, info: ElementInfo) -> Result<Option<String>> {
        self.document.unwrap()(info)
    }
    pub fn report(&self) -> Result<HandlerReport> {
        self.report.unwrap()()
    }
    pub fn result(
        &self,
        token_range: Range<usize>,
        result: HandlerResult,
    ) -> Result<HandlerResult> {
        self.result.unwrap()(token_range, result)
    }
    pub fn redirect(
        &self,
        token_range: Range<usize>,
        result: HandlerResult,
    ) -> Result<HandlerResult> {
        self.redirect.unwrap()(token_range, result)
    }
    /// Returns a string representation of the handler including its name and role
    pub fn to_string(&self) -> String {
        format!("Handler({}, {})", self.id.name(), self.role)
    }
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HandlerMap {
    pub name: String,
    pub ids: Vec<Id>,
    pub roles: HashMap<Id, String>,
    pub priorities: HashMap<Id, u64>,
    pub current: Option<Id>,
    pub last: Option<Id>,
    pub processors: HashMap<Id, fn(Range<usize>) -> Result<bool>>,
    pub handlers: HashMap<Id, fn(Range<usize>) -> Result<HandlerResult>>,
    pub extractors: HashMap<Id, fn(Range<usize>) -> Result<Option<ExtractedElement>>>,
    pub convertors: HashMap<Id, fn(Range<usize>) -> Result<Option<ConvertedElement>>>,
    pub documenters: HashMap<Id, fn(ElementInfo) -> Result<Option<String>>>,
    pub reporters: HashMap<Id, fn() -> Result<HandlerReport>>,
    pub results: HashMap<Id, fn(Range<usize>, HandlerResult) -> Result<HandlerResult>>,
    pub redirectors: HashMap<Id, fn(Range<usize>, HandlerResult) -> Result<HandlerResult>>,
    /// Pre-sorted handlers for efficient processing
    handlers_sorted: Vec<Handler>,
}
/// Registry
impl HandlerMap {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            current: None,
            last: None,
            ids: Vec::new(),
            roles: HashMap::new(),
            priorities: HashMap::new(),
            results: HashMap::new(),
            processors: HashMap::new(),
            handlers: HashMap::new(),
            extractors: HashMap::new(),
            convertors: HashMap::new(),
            documenters: HashMap::new(),
            reporters: HashMap::new(),
            redirectors: HashMap::new(),
            handlers_sorted: Vec::new(),
        }
    }
    pub fn register(&mut self, handler: Handler) {
        let id = handler.id.clone();
        self.ids.push(id.clone());
        self.roles.insert(id.clone(), handler.role.clone());
        self.priorities.insert(id.clone(), handler.priority.clone());

        // Register callbacks only if they exist
        if let Some(process_fn) = handler.process {
            self.processors.insert(id.clone(), process_fn);
        }
        if let Some(handle_fn) = handler.handle {
            self.handlers.insert(id.clone(), handle_fn);
        }
        if let Some(extract_fn) = handler.extract {
            self.extractors.insert(id.clone(), extract_fn);
        }
        if let Some(convert_fn) = handler.convert {
            self.convertors.insert(id.clone(), convert_fn);
        }
        if let Some(document_fn) = handler.document {
            self.documenters.insert(id.clone(), document_fn);
        }
        if let Some(report_fn) = handler.report {
            self.reporters.insert(id.clone(), report_fn);
        }
        if let Some(result_fn) = handler.result {
            self.results.insert(id.clone(), result_fn);
        }
        if let Some(redirect_fn) = handler.redirect {
            self.redirectors.insert(id.clone(), redirect_fn);
        }

        // Add to sorted handlers and maintain sort order
        self.handlers_sorted.push(handler);
        self.handlers_sorted
            .sort_by(|a, b| b.priority.cmp(&a.priority).then_with(|| a.id.cmp(&b.id)));
    }

    /// Register a handler in both the registry and make it accessible via the context
    /// This is useful for handlers that need to be accessible to other handlers
    pub fn register_shared(&mut self, handler: Handler, id: &Id) {
        let handler_name = handler.id.name();
        self.register(handler.clone());
        if let Some(handler) = self.get_handler_by_name(&handler_name) {
            context!().handlers.register(handler);
            context!().set_entry(id.name(), Entry::Str(handler_name.clone()));
        }
    }

    /// Create a Handler instance from its components
    fn create_handler_from_id(&self, id: &Id) -> Option<Handler> {
        match (
            self.ids.clone(),
            self.roles.get(id),
            self.priorities.get(id),
        ) {
            (_idv, role, priority) => {
                let mut handler =
                    Handler::new(id.clone(), role.unwrap().clone(), *priority.unwrap_or(&100));
                handler.process = self.processors.get(id).cloned();
                handler.handle = self.handlers.get(id).cloned();
                handler.extract = self.extractors.get(id).cloned();
                handler.convert = self.convertors.get(id).cloned();
                handler.document = self.documenters.get(id).cloned();
                handler.report = self.reporters.get(id).cloned();
                handler.result = self.results.get(id).cloned();
                handler.redirect = self.redirectors.get(id).cloned();
                Some(handler)
            }
        }
    }

    /// Get a reference to all registered handlers
    pub fn get_handlers(&self) -> Vec<Handler> {
        let mut handlers = self
            .ids
            .iter()
            .filter_map(|id| self.create_handler_from_id(id))
            .collect::<Vec<Handler>>();

        // Sort handlers by priority (highest first)
        handlers.sort_by(|a, b| b.priority.cmp(&a.priority));
        handlers
    }
    pub fn register_all(&mut self, handlers: Vec<Handler>) {
        for handler in handlers {
            self.register(handler);
        }
    }
    pub fn register_all_shared(&mut self, handlers: Vec<Handler>, id: Id) {
        for handler in handlers.clone() {
            self.register_shared(handler.clone(), &id);
        }
    }
    /// Get a handler by name
    pub fn get_handler_by_name(&self, name: &str) -> Option<Handler> {
        self.ids
            .iter()
            .find(|id| id.name() == name)
            .and_then(|id| self.create_handler_from_id(id))
    }

    /// Set the current handler and track the last handler
    pub fn set_current_handler(&mut self, id: Id) {
        if let Some(current) = &self.current {
            self.last = Some(current.clone());
            // Handler tracking: context not available here, using println for critical info
            println!("[HANDLER] Tracking: {} -> {}", current.name(), id.name());
        }
        self.current = Some(id);
    }

    /// Get the current handler
    pub fn get_current_handler(&self) -> Option<Handler> {
        self.current
            .clone()
            .and_then(|id| self.create_handler_from_id(&id))
    }

    /// Get the last handler that was used
    pub fn get_last_handler(&self) -> Option<Handler> {
        self.last
            .clone()
            .and_then(|id| self.create_handler_from_id(&id))
    }

    /// Mark a handler as successful
    pub fn mark_handler_success(&mut self, handler_name: &str, tokens_consumed: usize) {
        let handler_id = Id::get(handler_name);
        self.set_current_handler(handler_id.clone());
        // Handler success: context not available here, using println for critical info
        println!(
            "[HANDLER] Success: {} processed {} tokens",
            handler_name, tokens_consumed
        );
    }

    /// Mark a handler as failed
    pub fn mark_handler_failure(&mut self, handler_name: &str, error: &str) {
        let handler_id = Id::get(handler_name);
        if let Some(current) = &self.current {
            self.last = Some(current.clone());
        }
        // Handler failure: context not available here, using println for critical info
        println!(
            "[HANDLER] Failure: {} failed with error: {}",
            handler_name, error
        );
    }

    /// Get debugging information about handler state
    pub fn get_handler_debug_info(&self) -> String {
        let current_name = self
            .current
            .as_ref()
            .map(|id| id.name().clone())
            .unwrap_or_else(|| "None".to_string());
        let last_name = self
            .last
            .as_ref()
            .map(|id| id.name().clone())
            .unwrap_or_else(|| "None".to_string());

        format!(
            "HandlerMap Debug Info:\n  Current: {}\n  Last: {}\n  Total Handlers: {}",
            current_name,
            last_name,
            self.ids.len()
        )
    }

    /// Find the most suitable handler for the given tokens
    /// Uses priority-based selection with fallback chains and cycle detection
    pub fn find_handler(&self, tokens: &[Token]) -> Option<Handler> {
        if tokens.is_empty() {
            report!(
                "handler_map",
                "find_handler",
                ReportLevel::Info,
                Process,
                "find_handler: empty tokens, no handler needed",
                true
            );
            return None;
        }

        // Synchronize context with current tokens before handler evaluation
        {
            let mut context = crate::context!();
            context.tokens = tokens.to_vec();

            context.push();
        }

        // Track which handlers we've already tried to avoid cycles, with their depth
        let mut tried_handlers = HashMap::with_capacity(64);
        let handler_scores = HashMap::<String, f64>::with_capacity(64);

        // Use a binary heap for efficient priority-based processing
        // (priority, confidence_score, handler_name, depth, handler)
        let mut handler_queue = std::collections::BinaryHeap::new();

        // Initial list of handlers to try - sorted by priority with confidence scoring
        for handler in self.get_handlers() {
            let confidence_score = self.calculate_handler_confidence(&handler, tokens);
            handler_queue.push((
                std::cmp::Reverse(handler.priority),
                confidence_score,
                handler.id.name().clone(),
                0,
                handler,
            ));
        }

        report!(
            "handler_map",
            "find_handler",
            ReportLevel::Info,
            Process,
            format!(
                "find_handler: trying to handle {} tokens with {} candidate handlers",
                tokens.len(),
                handler_queue.len()
            ),
            true
        );

        // Log first few tokens for debugging context
        for i in 0..std::cmp::min(3, tokens.len()) {
            report!(
                "handler_map",
                "find_handler",
                ReportLevel::Debug,
                Process,
                format!("  Token[{}]: {:?}", i, tokens[i]),
                true
            );
        }

        const MAX_DEPTH: usize = 25;
        const MAX_HANDLERS: usize = 200;
        let mut handlers_checked = 0;
        let mut best_candidate: Option<(Handler, i32)> = None;

        // Keep trying handlers until we find one or exhaust all options
        while let Some((_, confidence, handler_name, depth, handler)) = handler_queue.pop() {
            handlers_checked += 1;

            if handlers_checked > MAX_HANDLERS {
                report!(
                    "handler_map",
                    "find_handler",
                    ReportLevel::Warning,
                    Process,
                    format!(
                        "find_handler: max handlers ({}) checked, using best candidate",
                        MAX_HANDLERS
                    ),
                    true
                );
                break;
            }

            // Skip if we've already tried this handler with equal or better depth
            if let Some(&prev_depth) = tried_handlers.get(&handler_name) {
                if prev_depth <= depth {
                    continue;
                }
            }

            // Prevent excessive recursion
            if depth > MAX_DEPTH {
                report!(
                    "handler_map",
                    "find_handler",
                    ReportLevel::Warning,
                    Process,
                    format!(
                        "find_handler: max depth ({}) reached for handler '{}'",
                        MAX_DEPTH, handler_name
                    ),
                    true
                );
                continue;
            }

            // Mark this handler as tried with current depth
            tried_handlers.insert(handler_name.clone(), depth);

            report!(
                "handler_map",
                "find_handler",
                ReportLevel::Debug,
                Process,
                format!(
                    "find_handler: evaluating handler '{}' (priority: {}, confidence: {}, depth: {})",
                    handler_name, handler.priority, confidence, depth
                ),
                true
            );

            // Test handler capability with enhanced validation
            if let Some(process_fn) = handler.process {
                match process_fn(0..tokens.len()) {
                    Ok(true) => {
                        report!(
                            "handler_map",
                            "find_handler",
                            ReportLevel::Info,
                            Process,
                            format!("find_handler: handler '{}' can handle tokens", handler_name),
                            true
                        );

                        // Track successful handler selection
                        context!().handlers.set_current_handler(handler.id.clone());
                        return Some(handler);
                    }
                    Ok(false) => {
                        // Handler explicitly rejected, but track as potential candidate
                        if confidence > 0 {
                            if let Some((_, best_confidence)) = &best_candidate {
                                if confidence > *best_confidence {
                                    best_candidate = Some((handler, confidence));
                                }
                            } else {
                                best_candidate = Some((handler, confidence));
                            }
                        }
                    }
                    Err(e) => {
                        report!(
                            "handler_map",
                            "find_handler",
                            ReportLevel::Warning,
                            Process,
                            format!(
                                "find_handler: handler '{}' failed evaluation: {}",
                                handler_name, e
                            ),
                            true
                        );
                    }
                }
            } else {
                // No process function - assume it can handle based on confidence
                if confidence > 50 {
                    report!(
                        "handler_map",
                        "find_handler",
                        ReportLevel::Info,
                        Process,
                        format!(
                            "find_handler: handler '{}' selected by confidence score",
                            handler_name
                        ),
                        true
                    );
                    context!().handlers.set_current_handler(handler.id.clone());
                    return Some(handler);
                }
            }
        }

        // Fallback to best candidate if no perfect match found
        if let Some((best_handler, score)) = best_candidate {
            report!(
                "handler_map",
                "find_handler",
                ReportLevel::Info,
                Process,
                format!(
                    "find_handler: using fallback candidate '{}' with score {}",
                    best_handler.id.name(),
                    score
                ),
                true
            );
            context!()
                .handlers
                .set_current_handler(best_handler.id.clone());
            return Some(best_handler);
        }

        report!(
            "handler_map",
            "find_handler",
            ReportLevel::Info,
            Process,
            format!(
                "find_handler: no suitable handler found after checking {} handlers",
                handlers_checked
            ),
            true
        );
        None
    }

    /// Calculate confidence score for handler based on token patterns
    fn calculate_handler_confidence(&self, handler: &Handler, tokens: &[Token]) -> i32 {
        if tokens.is_empty() {
            return 0;
        }

        let mut score = 0i32;
        let handler_name = handler.id.name();

        // Pattern-based scoring
        let first_token = tokens[0].to_string();
        match handler_name.as_str() {
            name if name.contains("function") => {
                if first_token.contains("func") || first_token.contains("def") {
                    score += 80;
                }
            }
            name if name.contains("control_flow") => {
                if ["if", "for", "while", "switch"]
                    .iter()
                    .any(|&kw| first_token.contains(kw))
                {
                    score += 85;
                }
            }
            name if name.contains("array") => {
                if tokens.iter().any(|t| t.to_string().contains('[')) {
                    score += 70;
                }
            }
            name if name.contains("expression") => {
                if tokens.len() > 2
                    && tokens.iter().any(|t| {
                        let s = t.to_string();
                        ["=", "+", "-", "*", "/", "==", "!="]
                            .iter()
                            .any(|&op| s.contains(op))
                    })
                {
                    score += 60;
                }
            }
            name if name.contains("variable") => {
                if tokens.len() >= 2 && tokens.iter().any(|t| t.to_string().contains('=')) {
                    score += 75;
                }
            }
            _ => score += 30, // Base score for any handler
        }

        // Boost score based on priority
        score += (handler.priority as i32) / 10;

        score
    }

    /// Process tokens with the first handler that can handle them
    /// Enhanced with proper range handling and better error recovery
    pub fn process(&mut self, tokens: &[Token]) -> Result<ProcessedResult> {
        self.process_range(tokens, 0..tokens.len())
    }

    /// Process tokens within a specific range with the first handler that can handle them
    pub fn process_range(
        &mut self,
        tokens: &[Token],
        range: Range<usize>,
    ) -> Result<ProcessedResult> {
        let id = Id::get("process_range");
        if tokens.is_empty() || range.start >= range.end || range.end > tokens.len() {
            return Ok(ProcessedResult::new(
                0,
                0,
                id.clone(),
                HandlerResult::NotHandled(Some(tokens.to_vec()), range.clone(), id),
            ));
        }

        // Synchronize context with current tokens before processing
        {
            let mut context = crate::context!();
            context.tokens = tokens.to_vec();

            context.push();
        }

        let range_len = range.end - range.start;

        report!(
            "handler_map",
            "process",
            ReportLevel::Debug,
            Process,
            format!(
                "process: attempting to process {} tokens in range {}..{}",
                range_len, range.start, range.end
            ),
            true
        );

        // Clone handlers list to avoid borrowing issues
        let handlers = self.handlers_sorted.clone();

        // Try each handler in priority order until one succeeds
        for handler in handlers {
            let handler_id = handler.id;
            let handler_name = handler_id.name();

            report!(
                "handler_map",
                "process",
                ReportLevel::Debug,
                Process,
                format!(
                    "process: trying handler '{}' (priority: {})",
                    handler_name, handler.priority
                ),
                true
            );

            // Step 1: Check if handler has a process function and can handle these tokens
            let can_handle = if let Some(process_fn) = self.processors.get(&handler_id) {
                // Debug: Show tokens being passed to handler from the specified range
                let tokens_preview = tokens[range.clone()]
                    .iter()
                    .take(10)
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(" ");

                report!(
                    "handler_map",
                    "process",
                    ReportLevel::Debug,
                    Process,
                    format!(
                        "process: handler '{}' processing tokens: [{}{}]",
                        handler_name,
                        tokens_preview,
                        if range_len > 10 { "..." } else { "" }
                    ),
                    true
                );

                match process_fn(range.clone()) {
                    Ok(result) => {
                        report!(
                            "handler_map",
                            "process",
                            ReportLevel::Debug,
                            Process,
                            format!(
                                "process: handler '{}' validation result: {}",
                                handler_name, result
                            ),
                            true
                        );
                        result
                    }
                    Err(e) => {
                        report!(
                            "handler_map",
                            "process",
                            ReportLevel::Debug,
                            Process,
                            format!(
                                "process: handler '{}' validation failed: {}",
                                handler_name, e
                            ),
                            true
                        );
                        false
                    }
                }
            } else {
                // No process function, assume it can handle (will be determined by handler function itself)
                true
            };

            if !can_handle {
                report!(
                    "handler_map",
                    "process",
                    ReportLevel::Debug,
                    Process,
                    format!(
                        "process: handler '{}' cannot handle tokens, trying next",
                        handler_name
                    ),
                    true
                );
                continue;
            }

            // Step 2: Execute the main handler function
            report!(
                "handler_map",
                "process",
                ReportLevel::Info,
                Process,
                format!(
                    "EXEC DEBUG: Attempting to execute handler '{}' main function",
                    handler_name
                ),
                true
            );

            let mut result = if let Some(handle_fn) = self.handlers.get(&handler_id) {
                report!(
                    "handler_map",
                    "process",
                    ReportLevel::Info,
                    Process,
                    format!(
                        "EXEC DEBUG: Found main function for '{}', executing...",
                        handler_name
                    ),
                    true
                );
                match handle_fn(range.clone()) {
                    Ok(res) => {
                        report!(
                            "handler_map",
                            "process",
                            ReportLevel::Info,
                            Process,
                            format!(
                                "EXEC DEBUG: Handler '{}' executed successfully, result type: {:?}",
                                handler_name,
                                std::mem::discriminant(&res)
                            ),
                            true
                        );
                        res
                    }
                    Err(e) => {
                        report!(
                            "handler_map",
                            "process",
                            ReportLevel::Info,
                            Process,
                            format!(
                                "EXEC DEBUG: Handler '{}' execution failed: {}, trying next",
                                handler_name, e
                            ),
                            true
                        );
                        continue; // Try next handler instead of failing
                    }
                }
            } else {
                report!(
                    "handler_map",
                    "process",
                    ReportLevel::Info,
                    Process,
                    format!(
                        "EXEC DEBUG: No handler function found for '{}', skipping",
                        handler_name
                    ),
                    true
                );
                continue;
            };

            // Step 3: Check if handler actually handled the tokens
            match &result {
                HandlerResult::NotHandled(_, _, _) => {
                    report!(
                        "handler_map",
                        "process",
                        ReportLevel::Debug,
                        Process,
                        format!(
                            "process: handler '{}' returned NotHandled, trying next",
                            handler_name
                        ),
                        true
                    );
                    continue; // Try next handler
                }
                _ => {
                    // Handler successfully processed tokens, continue with post-processing
                    report!(
                        "handler_map",
                        "process",
                        ReportLevel::Info,
                        Process,
                        format!("process: handler '{}' accepted tokens", handler_name),
                        true
                    );
                }
            }

            // Step 4: Handle redirects with improved error handling
            if let HandlerResult::Redirected(
                redirect_tokens,
                redirect_range,
                target_handler_role,
                from_id,
                _to_id,
            ) = result.clone()
            {
                report!(
                    "handler_map",
                    "process",
                    ReportLevel::Info,
                    Process,
                    format!(
                        "process: handler '{}' redirected to '{}'",
                        handler_name, target_handler_role
                    ),
                    true
                );

                // Find and execute target handler
                if let Some(target_id) = self.ids.iter().find(|id| {
                    self.roles
                        .get(id)
                        .map(|r| r == &target_handler_role)
                        .unwrap_or(false)
                }) {
                    if let Some(target_handle_fn) = self.handlers.get(target_id) {
                        match target_handle_fn(redirect_range.clone()) {
                            Ok(redirected_result) => {
                                // Return immediately after redirect to avoid double processing
                                let range_consumed = redirected_result.token_count();
                                if range_consumed > 0 {
                                    report!(
                                        "handler_map",
                                        "process",
                                        ReportLevel::Info,
                                        Process,
                                        format!(
                                            "process: redirect to '{}' successfully processed {} tokens",
                                            target_handler_role, range_consumed
                                        ),
                                        true
                                    );
                                    // Return range relative to original token range (range parameter)
                                    let relative_start = redirect_range.start - range.start;
                                    let relative_end = relative_start + range_consumed;
                                    return Ok(ProcessedResult::new(
                                        range.start + relative_start,
                                        range.start + relative_end,
                                        from_id,
                                        redirected_result,
                                    ));
                                }
                            }
                            Err(e) => {
                                report!(
                                    "handler_map",
                                    "process",
                                    ReportLevel::Error,
                                    Process,
                                    format!(
                                        "process: redirect to '{}' failed: {}",
                                        target_handler_role, e
                                    ),
                                    true
                                );
                                continue; // Try next handler instead of failing
                            }
                        }
                    }
                }
            }

            // Step 5: Apply post-processing callbacks using the actual range from result
            let actual_range = range.start..(range.start + result.token_count().min(range_len));
            if let Some(result_fn) = self.results.get(&handler_id) {
                match result_fn(actual_range.clone(), result) {
                    Ok(processed_result) => result = processed_result,
                    Err(e) => {
                        report!(
                            "handler_map",
                            "process",
                            ReportLevel::Error,
                            Process,
                            format!(
                                "process: result callback failed for '{}': {}",
                                handler_name, e
                            ),
                            true
                        );
                        continue; // Try next handler instead of failing
                    }
                }
            }

            // Step 6: Apply redirect callbacks
            if let Some(redirect_fn) = self.redirectors.get(&handler_id) {
                match redirect_fn(actual_range.clone(), result) {
                    Ok(final_result) => result = final_result,
                    Err(e) => {
                        report!(
                            "handler_map",
                            "process",
                            ReportLevel::Error,
                            Process,
                            format!(
                                "process: redirect callback failed for '{}': {}",
                                handler_name, e
                            ),
                            true
                        );
                        continue; // Try next handler instead of failing
                    }
                }
            }

            // Step 7: Final validation and return results
            let range_consumed = result.token_count();
            if range_consumed > 0 {
                report!(
                    "handler_map",
                    "process",
                    ReportLevel::Info,
                    Process,
                    format!(
                        "process: handler '{}' successfully processed {} tokens",
                        handler_name, range_consumed
                    ),
                    true
                );

                // Track successful processing
                self.mark_handler_success(handler_name, range_consumed);

                return Ok(ProcessedResult::new(
                    range.start,
                    range_consumed,
                    handler_id.clone(),
                    result,
                ));
            } else {
                report!(
                    "handler_map",
                    "process",
                    ReportLevel::Debug,
                    Process,
                    format!(
                        "process: handler '{}' processed 0 tokens, trying next",
                        handler_name
                    ),
                    true
                );
                continue;
            }
        }

        // No handler could process the tokens
        report!(
            "handler_map",
            "process",
            ReportLevel::Info,
            Process,
            format!("process: no handler could process {} tokens", range_len),
            true
        );

        Ok(ProcessedResult::new(
            range.start,
            0,
            id.clone(),
            HandlerResult::NotHandled(Some(tokens[range.clone()].to_vec()), range.clone(), id),
        ))
    }

    /// Process all tokens in a file by processing them sequentially and collecting results
    /// Enhanced to properly handle token ranges instead of simple counts
    pub fn process_all(&mut self, tokens: &[Token]) -> Result<ProcessedResults> {
        let mut results = ProcessedResults::new();
        let mut current_pos = 0;
        let total_tokens = tokens.len();
        let mut consecutive_failures = 0;
        const MAX_CONSECUTIVE_FAILURES: usize = 10;
        const MIN_ADVANCE: usize = 1;
        let find_next_unprocessed_token = |tokens: &[Token], start_pos: usize| -> usize {
            // Find the next position where token is not Token::n()
            let context = crate::context!();
            for i in start_pos..tokens.len() {
                if i < context.tokens.len() && !matches!(context.tokens[i], crate::Token::n()) {
                    return i;
                }
            }
            // If all remaining tokens are processed, return end position
            tokens.len()
        };

        // Initialize global context tokens for proper Token::n() marking
        {
            let mut context = crate::context!();
            context.tokens = tokens.to_vec();

            context.push();
        }

        report!(
            "handler_map",
            "process_all",
            ReportLevel::Info,
            Process,
            format!("process_all: starting to process {} tokens", total_tokens),
            true
        );

        // Early return for empty input
        if tokens.is_empty() {
            report!(
                "handler_map",
                "process_all",
                ReportLevel::Info,
                Process,
                "process_all: no tokens to process, returning empty results".to_string(),
                true
            );
            return Ok(results);
        }

        // Process tokens sequentially until we've consumed all tokens
        while current_pos < total_tokens {
            let remaining_tokens = &tokens[current_pos..];

            report!(
                "handler_map",
                "process_all",
                ReportLevel::Debug,
                Process,
                format!(
                    "process_all: processing from position {} with {} remaining tokens (failures: {})",
                    current_pos,
                    remaining_tokens.len(),
                    consecutive_failures
                ),
                true
            );

            // Process tokens with full context but specific range
            // Handlers get full token array for context but can only process within specified range
            const MAX_WINDOW_SIZE: usize = 50; // Process up to 50 tokens at a time
            let window_size = std::cmp::min(remaining_tokens.len(), MAX_WINDOW_SIZE);
            let processing_range = current_pos..(current_pos + window_size);

            match self.process_range(tokens, processing_range.clone()) {
                Ok(processed_result) => {
                    // Get the actual range processed by the handler from HandlerResult
                    let range_consumed = processed_result.result.token_count();
                    let actual_consumed = if range_consumed > 0 {
                        range_consumed
                    } else {
                        MIN_ADVANCE
                    };

                    report!(
                        "handler_map",
                        "process_all",
                        ReportLevel::Info,
                        Process,
                        format!(
                            "process_all: processed {} tokens at position {} with handler: {}",
                            actual_consumed,
                            current_pos,
                            processed_result.id.name()
                        ),
                        true
                    );

                    // ProcessedResult already has absolute positions from process_range
                    let adjusted_result = processed_result;

                    // Add the result to our collection
                    results.add(adjusted_result);

                    // Mark consumed tokens as Token::n() in global context to prevent reprocessing
                    if actual_consumed > 0 {
                        let mut context = crate::context!();
                        for i in 0..actual_consumed {
                            if (current_pos + i) < context.tokens.len() {
                                context.tokens[current_pos + i] = crate::Token::n();
                            }
                        }
                        context.push();
                    }

                    // Advance to next unprocessed token starting from after the consumed tokens
                    current_pos =
                        find_next_unprocessed_token(tokens, current_pos + actual_consumed);
                    consecutive_failures = 0; // Reset on successful processing

                    // Check for no progress to prevent infinite loops
                    if actual_consumed == 0 {
                        consecutive_failures += 1;

                        report!(
                            "handler_map",
                            "process_all",
                            ReportLevel::Warning,
                            Process,
                            format!(
                                "process_all: no progress made at position {}, forced advance by {} (failure #{}/{})",
                                current_pos - MIN_ADVANCE,
                                MIN_ADVANCE,
                                consecutive_failures,
                                MAX_CONSECUTIVE_FAILURES
                            ),
                            true
                        );
                    }
                }
                Err(e) => {
                    consecutive_failures += 1;

                    report!(
                        "handler_map",
                        "process_all",
                        ReportLevel::Error,
                        Process,
                        format!(
                            "process_all: processing failed at position {} (failure #{}/{}): {}",
                            current_pos, consecutive_failures, MAX_CONSECUTIVE_FAILURES, e
                        ),
                        true
                    );

                    // Check if we've hit the failure limit
                    if consecutive_failures >= MAX_CONSECUTIVE_FAILURES {
                        report!(
                            "handler_map",
                            "process_all",
                            ReportLevel::Error,
                            Process,
                            format!(
                                "process_all: too many consecutive failures ({}), stopping processing at position {}/{}",
                                consecutive_failures, current_pos, total_tokens
                            ),
                            true
                        );
                        return Err(C2RError::new(
                            Kind::Other,
                            Reason::Other("Excessive processing failures"),
                            Some(format!(
                                "Too many consecutive processing failures ({}) at position {}",
                                consecutive_failures, current_pos
                            )),
                        ));
                    }

                    // Advance to next unprocessed token instead of blind MIN_ADVANCE
                    current_pos = find_next_unprocessed_token(tokens, current_pos + MIN_ADVANCE);
                }
            }

            // If no handlers processed any tokens and we still have unprocessed tokens,
            // mark the current token as Token::n() to prevent infinite loops
            if current_pos < total_tokens {
                let context = crate::context!();
                let has_unprocessed = current_pos < context.tokens.len()
                    && !matches!(context.tokens[current_pos], crate::Token::n());

                if has_unprocessed && consecutive_failures > 0 {
                    report!(
                        "handler_map",
                        "process_all",
                        ReportLevel::Warning,
                        Process,
                        format!(
                            "process_all: no handlers could process token at position {}, marking as processed to continue",
                            current_pos
                        ),
                        true
                    );

                    // Mark the unprocessable token as Token::n() using context
                    {
                        let mut context = crate::context!();
                        if current_pos < context.tokens.len() {
                            context.tokens[current_pos] = crate::Token::n();
                        }
                        context.push();
                    }

                    // Find next unprocessed token
                    current_pos = find_next_unprocessed_token(tokens, current_pos + 1);
                    consecutive_failures = 0; // Reset since we made progress
                }
            }

            // Safety check to prevent infinite loops
            if current_pos > total_tokens {
                report!(
                    "handler_map",
                    "process_all",
                    ReportLevel::Error,
                    Process,
                    format!(
                        "process_all: position overflow detected ({} > {}), capping at total",
                        current_pos, total_tokens
                    ),
                    true
                );
                break;
            }
        }

        let final_results_count = results.len();
        let tokens_processed = results.total_tokens_processed;
        let processing_efficiency = if total_tokens > 0 {
            (tokens_processed as f64 / total_tokens as f64) * 100.0
        } else {
            100.0
        };

        report!(
            "handler_map",
            "process_all",
            ReportLevel::Info,
            Process,
            format!(
                "process_all: completed processing {} results from {}/{} tokens ({:.1}% efficiency, {} failures)",
                final_results_count,
                tokens_processed,
                total_tokens,
                processing_efficiency,
                consecutive_failures
            ),
            true
        );

        Ok(results)
    }
}
impl Default for HandlerMap {
    fn default() -> Self {
        Self::new("default_handler_map")
    }
}
/// Iterator for HandlerMap that yields handlers in priority order
pub struct HandlerMapIterator {
    handlers: Vec<Handler>,
    current: usize,
}

impl Iterator for HandlerMapIterator {
    type Item = Handler;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current < self.handlers.len() {
            let handler = self.handlers[self.current].clone();
            self.current += 1;
            Some(handler)
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.handlers.len() - self.current;
        (remaining, Some(remaining))
    }
}

impl ExactSizeIterator for HandlerMapIterator {
    fn len(&self) -> usize {
        self.handlers.len() - self.current
    }
}

impl IntoIterator for HandlerMap {
    type Item = Handler;
    type IntoIter = HandlerMapIterator;

    fn into_iter(self) -> Self::IntoIter {
        // Get all handlers and sort by priority (higher priority first)
        let mut handlers = self.get_handlers();
        handlers.sort_by(|a, b| b.priority.cmp(&a.priority));

        HandlerMapIterator {
            handlers,
            current: 0,
        }
    }
}

impl<'a> IntoIterator for &'a HandlerMap {
    type Item = Handler;
    type IntoIter = HandlerMapIterator;

    fn into_iter(self) -> Self::IntoIter {
        // Get all handlers and sort by priority (higher priority first)
        let mut handlers = self.get_handlers();
        handlers.sort_by(|a, b| b.priority.cmp(&a.priority));

        HandlerMapIterator {
            handlers,
            current: 0,
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
            .filter_map(|result| extract_code_from_handler_result(&result.result))
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

/// Helper function to extract code from a HandlerResult (used by ProcessedResults)
fn extract_code_from_handler_result(result: &HandlerResult) -> Option<String> {
    match result {
        HandlerResult::Converted(element, _, _, _) => {
            // Extract code from ConvertedElement
            let code = match element {
                crate::convert::ConvertedElement::Function(f) => f.code.clone(),
                crate::convert::ConvertedElement::Struct(s) => s.code.clone(),
                crate::convert::ConvertedElement::Enum(e) => e.code.clone(),
                crate::convert::ConvertedElement::Typedef(t) => t.code.clone(),
                crate::convert::ConvertedElement::Array(a) => a.code.clone(),
                crate::convert::ConvertedElement::Include(i) => i.code.clone(),
                crate::convert::ConvertedElement::Comment(c) => c.code.clone(),
                crate::convert::ConvertedElement::Expression(e) => e.code.clone(),
                crate::convert::ConvertedElement::Global(g) => g.code.clone(),
                crate::convert::ConvertedElement::Macro(m) => m.code.clone(),
                crate::convert::ConvertedElement::ControlFlow(c) => c.code.clone(),
            };

            if !code.trim().is_empty() {
                Some(code.clone())
            } else {
                None
            }
        }
        HandlerResult::Extracted(element, _, _, _) => {
            // Extract code from ExtractedElement if it has one
            let code = match element {
                crate::extract::ExtractedElement::Function(f) => f.code.clone(),
                crate::extract::ExtractedElement::Struct(s) => s.code.clone(),
                crate::extract::ExtractedElement::Enum(e) => e.code.clone(),
                crate::extract::ExtractedElement::Typedef(t) => t.code.clone(),
                crate::extract::ExtractedElement::Array(a) => a.code.clone(),
                crate::extract::ExtractedElement::Include(i) => i.code.clone(),
                crate::extract::ExtractedElement::Comment(c) => c.code.clone(),
                crate::extract::ExtractedElement::Expression(e) => e.code.clone(),
                crate::extract::ExtractedElement::Global(g) => g.code.clone(),
                crate::extract::ExtractedElement::Macro(m) => m.code.clone(),
                crate::extract::ExtractedElement::ControlFlow(c) => c.code.clone(),
            };

            if !code.trim().is_empty() {
                Some(code.clone())
            } else {
                None
            }
        }
        HandlerResult::Completed(_, _, code, _) => {
            if !code.trim().is_empty() {
                Some(code.clone())
            } else {
                None
            }
        }
        HandlerResult::Processed(_, _, code, _) => {
            if !code.trim().is_empty() {
                Some(code.clone())
            } else {
                None
            }
        }
        HandlerResult::Handled(_, _, _)
        | HandlerResult::NotHandled(_, _, _)
        | HandlerResult::Redirected(_, _, _, _, _) => None,
    }
}
#[derive(Debug, Clone, Hash)]
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

/// Result of handling a token or sequence of tokens
#[derive(Debug, Clone, Hash)]
pub enum HandlerResult {
    Handled(Option<Vec<Token>>, Range<usize>, Id),
    Processed(Option<Vec<Token>>, Range<usize>, String, Id),
    /// Handler processed the tokens and directly replaced them with a string
    Completed(Option<Vec<Token>>, Range<usize>, String, Id),
    /// Handler could not process these tokens
    NotHandled(Option<Vec<Token>>, Range<usize>, Id),
    /// Handler wants to redirect to another handler and process the result
    Redirected(Option<Vec<Token>>, Range<usize>, String, Id, Id),
    /// Handler processed and produced extracted Rust code (alias of RustCode)
    Extracted(ExtractedElement, Range<usize>, String, Id),
    /// Handler processed and produced converted Rust code (alias of RustCode)
    Converted(ConvertedElement, Range<usize>, String, Id),
}
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
                write!(
                    f,
                    "Handler: {} reports handling successful (consumed {} tokens).\n",
                    id.name(),
                    range.len()
                )
            }
            HandlerResult::Processed(tokens, range, s, id) => write!(
                f,
                "Handler: {} reports processing successful (consumed {} tokens).\n Code:\n{}",
                id.name(),
                range.len(),
                s
            ),
            HandlerResult::Extracted(element, range, s, id) => write!(
                f,
                "Handler: {} reports extraction successful (consumed {} tokens).\n Code:\n{}",
                id.name(),
                range.len(),
                s
            ),
            HandlerResult::Converted(element, range, s, id) => write!(
                f,
                "Handler: {} reports conversion successful (consumed {} tokens).\n Code:\n{}",
                id.name(),
                range.len(),
                s
            ),
            HandlerResult::Completed(tokens, range, s, id) => write!(
                f,
                "Handler: {} reports all operations successful (consumed {} tokens).\n Code:\n{}",
                id.name(),
                range.len(),
                s
            ),
            HandlerResult::NotHandled(tokens, range, id) => {
                write!(
                    f,
                    "Handler: {} reports not handled (consumed {} tokens)",
                    id.name(),
                    range.len()
                )
            }
            HandlerResult::Redirected(tokens, range, s, id1, id2) => {
                write!(
                    f,
                    "Handler: {} reports redirection to Handler: {} (consumed {} tokens).\n Code:\n{}",
                    id1.name(),
                    id2.name(),
                    range.len(),
                    s
                )
            }
        }
    }
}
