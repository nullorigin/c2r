#![allow(
    unused_variables,
    unused_assignments,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals
)]

use crate::config::{
    HandlerPhase::Process,
    HandlerReport,
    ReportLevel::{Error, Warning},
};
use crate::error::ConversionError;
use crate::extract::ExtractedElement;
use crate::lock::Id;
use crate::ReportLevel::Info;
use crate::{report, context, Context, ConvertedElement, HandlerRedirect, Token, Entry};
use std::collections::HashMap;
use std::fmt::Display;
use std::hash::{DefaultHasher, Hash, RandomState};
use std::ops::Range;
use std::sync::{LazyLock, OnceLock};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Handler {
    pub id: Id,
    pub role: String,
    pub priority: u64,
    pub process: Option<fn(&[Token]) -> Result<bool, ConversionError>>,
    pub handle: Option<fn(&[Token]) -> Result<HandlerResult, ConversionError>>,
    pub extract:
        Option<fn(&[Token]) -> Result<Option<ExtractedElement>, ConversionError>>,
    pub convert:
        Option<fn(&[Token]) -> Result<Option<ConvertedElement>, ConversionError>>,
    pub report: Option<fn(&[Token]) -> Result<HandlerReport, ConversionError>>,
    pub result:
        Option<fn(&[Token], HandlerResult) -> Result<HandlerResult, ConversionError>>,
    pub redirect:
        Option<fn(&[Token], HandlerResult) -> Result<HandlerResult, ConversionError>>,
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
    pub fn process(
        &self,
        tokens: &[Token],
) -> Result<bool, ConversionError> {
        self.process.unwrap()(tokens)
    }
    pub fn handle(
        &self,
        tokens: &[Token],
) -> Result<HandlerResult, ConversionError> {
        self.handle.unwrap()(tokens)
    }
    pub fn extract(
        &self,
        tokens: &[Token],
) -> Result<Option<ExtractedElement>, ConversionError> {
        self.extract.unwrap()(tokens)
    }
    pub fn convert(
        &self,
        tokens: &[Token],
) -> Result<Option<ConvertedElement>, ConversionError> {
        self.convert.unwrap()(tokens)
    }
    pub fn report(
        &self,
        tokens: &[Token],
) -> Result<HandlerReport, ConversionError> {
        self.report.unwrap()(tokens)
    }
    pub fn result(
        &self,
        tokens: &[Token],
        result: HandlerResult,
) -> Result<HandlerResult, ConversionError> {
        self.result.unwrap()(tokens, result)
    }
    pub fn redirect(
        &self,
        tokens: &[Token],
        result: HandlerResult,
) -> Result<HandlerResult, ConversionError> {
        self.redirect.unwrap()(tokens, result)
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
    pub processors: HashMap<Id, fn(&[Token]) -> Result<bool, ConversionError>>,
    pub handlers: HashMap<Id, fn(&[Token]) -> Result<HandlerResult, ConversionError>>,
    pub extractors: HashMap<
        Id,
        fn(&[Token]) -> Result<Option<ExtractedElement>, ConversionError>,
    >,
    pub convertors: HashMap<
        Id,
        fn(&[Token]) -> Result<Option<ConvertedElement>, ConversionError>,
    >,
    pub reporters:
        HashMap<Id, fn(&[Token]) -> Result<HandlerReport, ConversionError>>,
    pub results: HashMap<
        Id,
        fn(&[Token], HandlerResult) -> Result<HandlerResult, ConversionError>,
    >,
    pub redirectors: HashMap<
        Id,
        fn(&[Token], HandlerResult) -> Result<HandlerResult, ConversionError>,
    >,
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
            reporters: HashMap::new(),
            redirectors: HashMap::new(),
        }
    }
    pub fn register(&mut self, handler: Handler) {
        let id = handler.id.clone();
        self.ids.push(id.clone());
        self.roles.insert(id.clone(), handler.role.clone());
        self.priorities.insert(id.clone(), handler.priority.clone());
        self.processors.insert(id.clone(), handler.process.unwrap());
        self.handlers.insert(id.clone(), handler.handle.unwrap());
        self.extractors.insert(id.clone(), handler.extract.unwrap());
        self.convertors.insert(id.clone(), handler.convert.unwrap());
        self.reporters.insert(id.clone(), handler.report.unwrap());
        self.results.insert(id.clone(), handler.result.unwrap());
        self.redirectors
            .insert(id.clone(), handler.redirect.unwrap());
    }

    /// Register a handler in both the registry and make it accessible via the context
    /// This is useful for handlers that need to be accessible to other handlers
    pub fn register_shared(&mut self, handler: Handler, context: &mut Context, id: &Id) {
        let handler_name = handler.id.name();
        self.register(handler.clone());
        if let Some(handler) = self.get_handler_by_name(&handler_name) {
            context.handlers.register(handler);
            context.set_entry(id.name(), Entry::Str(handler_name.clone()));
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
    pub fn register_all_shared(&mut self, handlers: Vec<Handler>,id: Id) {
        let mut context = context!();
        for handler in handlers.clone() {
            self.register_shared(handler, &mut context, &id);
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

    pub fn find_handler(&self, tokens: &[Token]) -> Option<Handler> {
        if tokens.is_empty() {
            report!(
                "handler_map",
                "find_handler",
                Info,
                Process,
                "find_handler: empty tokens, no handler needed",
                true
            );
            return None;
        }

        // Track which handlers we've already tried to avoid cycles, with their depth
        let mut tried_handlers = HashMap::with_capacity(32);

        // Use a binary heap for efficient priority-based processing
        // (priority, handler_name, depth, handler)
        let mut handler_queue = std::collections::BinaryHeap::new();

        // Initial list of handlers to try - sorted by priority
        for handler in self.get_handlers() {
            handler_queue.push((
                std::cmp::Reverse(handler.priority),
                handler.id.name().clone(),
                0,
                handler,
            ));
        }

        report!(
            "handler_map",
            "find_handler",
            Info,
            Process,
            format!("find_handler: trying to handle {} tokens", tokens.len()),
            true
        );
        // First few tokens for context in logs
        for i in 0..std::cmp::min(5, tokens.len()) {
            report!(
                "handler_map",
                "find_handler",
                Info,
                Process,
                format!("  Token[{}]: {:?}", i, tokens[i]),
                true
            );
        }

        const MAX_DEPTH: usize = 20; // Increased depth limit for complex fallback chains
        const MAX_HANDLERS: usize = 150; // Prevent runaway processing
        let mut handlers_checked = 0;

        // Keep trying handlers until we find one or exhaust all options
        while let Some((_, handler_name, depth, handler)) = handler_queue.pop() {
            handlers_checked += 1;

            if handlers_checked > MAX_HANDLERS {
                report!(
                    "handler_map",
                    "find_handler",
                    Error,
                    Process,
                    format!(
                        "find_handler: max handlers ({}) checked, stopping cascade",
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
                    Error,
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

            // Check if this handler can handle the tokens
            report!(
                "handler_map",
                "find_handler",
                Info,
                Process,
                format!(
                    "find_handler: trying handler '{}' (priority: {}, depth: {})",
                    handler_name, handler.priority, depth
                ),
                true
            );

            if handler.process.is_some() && handler.process(tokens).unwrap_or(false) {
                report!(
                    "handler_map",
                    "find_handler",
                    Info,
                    Process,
                    format!("find_handler: handler '{}' can handle tokens", handler_name),
                    true
                );
                // Track successful handler selection
                context!().handlers.set_current_handler(handler.id.clone());
                return Some(handler);
            }
        }

        report!(
            "handler_map",
            "find_handler",
            Info,
            Process,
            format!(
                "find_handler: no handler found after checking {} handlers",
                handlers_checked
            ),
            true
        );
        None
    }

    /// Process tokens with the first handler that can handle them
    pub fn process(
        &self,
        tokens: &[Token]
) -> Result<ProcessedResult, ConversionError> {
        let id = Id::get("process");
        if tokens.is_empty() {
            return Ok(ProcessedResult::new(
                0,
                0,
                id.clone(),
                HandlerResult::NotHandled(Some(tokens.to_vec()), 0..0, id),
            ));
        }

        // CRITICAL FIX: Process should handle ONE token chunk, not loop
        // The looping should be done by process_all(), not process()
        if let Some(handler) = self.find_handler(tokens) {
            let handler_id = handler.id.clone();
            if let Some(role) = self.roles.get(&handler_id) {
                report!(
                    "handler_map",
                    "process",
                    Info,
                    Process,
                    format!("process: trying handler '{}'", role),
                    true
                );

                // Step 1: Check if handler can process these tokens
                let can_handle = if let Some(process_fn) = self.processors.get(&handler_id) {
                    process_fn(tokens).unwrap_or_else(|_| false)
                } else {
                    true // Assume it can handle if no process function
                };

                if !can_handle {
                    report!(
                        "handler_map",
                        "process",
                        Info,
                        Process,
                        format!("process: handler '{}' rejected tokens", role),
                        true
                    );
                    return Ok(ProcessedResult::new(
                        0,
                        0,
                        id.clone(),
                        HandlerResult::NotHandled(Some(tokens.to_vec()), 0..tokens.len(), id),
                    ));
                }

                // Step 2: Execute the main handler function
                if let Some(handle_fn) = self.handlers.get(&handler_id) {
                    let mut result = handle_fn(tokens)?;

                    // Step 3: Handle redirects
                    if let HandlerResult::Redirected(
                        redirect_tokens,
                        range,
                        target_handler_role,
                        from_id,
                        to_id,
                    ) = &result
                    {
report!(
                            "handler_map",
                            "process",
                            Info,
                            Process,
                            format!(
                                "process: handler '{}' redirected to '{}'",
                                role, target_handler_role
                            ),
                            true
                        );

                        // Find the target handler ID by role and process with it
                        if let Some(target_id) = self.ids.iter().find(|id| {
                            self.roles
                                .get(id)
                                .map(|r| r == target_handler_role)
                                .unwrap_or(false)
                        }) {
                            if let Some(target_handle_fn) = self.handlers.get(target_id) {
                                let redirect_tokens_slice = redirect_tokens
                                    .as_ref()
                                    .map(|v| v.as_slice())
                                    .unwrap_or(tokens);
                                result = target_handle_fn(redirect_tokens_slice)?;
                            }
                        }
                    }

                    // Step 4: Apply result callback if present
                    if let Some(result_fn) = self.results.get(&handler_id) {
                        result = result_fn(tokens, result)?;
                    }

                    // Step 5: Apply redirect callback if present
                    if let Some(redirect_fn) = self.redirectors.get(&handler_id) {
                        result = redirect_fn(tokens, result)?;
                    }

                    // Step 6: Check if handler successfully processed tokens
                    if !matches!(result, HandlerResult::NotHandled(_, _, _)) {
                        let tokens_consumed = self.calculate_tokens_consumed(&result, tokens.len());
report!(
                            "handler_map",
                            "process",
                            Info,
                            Process,
                            format!(
                                "process: handler '{}' successfully processed {} tokens",
                                role, tokens_consumed
                            ),
                            true
                        );

                        return Ok(ProcessedResult::new(
                            0,
                            tokens_consumed,
                            handler_id.clone(),
                            result,
                        ));
                    } else {
report!(
                            "handler_map",
                            "process",
                            Info,
                            Process,
                            format!("process: handler '{}' returned NotHandled", role),
                            true
                        );
                    }
                }
            }
        }
        
        report!(
            "handler_map",
            "process",
            Info,
            Process,
            "process: no handler could process tokens",
            true
        );
        
        Ok(ProcessedResult::new(
            0,
            0,
            id.clone(),
            HandlerResult::NotHandled(Some(tokens.to_vec()), 0..tokens.len(), id),
        ))
    }

    /// Process all tokens in a file by processing them in chunks and collecting results
    pub fn process_all(
        &self,
        tokens: &[Token],
) -> Result<ProcessedResults, ConversionError> {
        let mut results = ProcessedResults::new();
        let mut current_pos = 0;

        report!(
            "handler_map",
            "process_all",
            Info,
            Process,
            format!("process_all: starting to process {} tokens", tokens.len()),
            true
        );

        // Process tokens in chunks until we've consumed all tokens
        while current_pos < tokens.len() {
            let remaining_tokens = &tokens[current_pos..];

            report!(
                "handler_map",
                "process_all",
                Info,
                Process,
                format!(
                    "process_all: processing chunk at position {} with {} remaining tokens",
                    current_pos,
                    remaining_tokens.len()
                ),
                true
            );

            // Process this chunk of tokens
            let chunk_result = self.process(remaining_tokens)?;

            // Adjust the token indices to be relative to the full file
            let adjusted_result = ProcessedResult::new(
                current_pos + chunk_result.start_idx,
                current_pos + chunk_result.end_idx,
                chunk_result.id.clone(),
                chunk_result.result,
            );

            let tokens_consumed = adjusted_result.tokens_consumed;

            report!(
                "handler_map",
                "process_all",
                Info,
                Process,
                format!(
                    "process_all: chunk processed {} tokens, handler: {}",
                    tokens_consumed,
                    adjusted_result.id.name()
                ),
                true
            );

            // Add the result to our collection
            results.add(adjusted_result);

            // Advance position by the number of tokens consumed
            if tokens_consumed == 0 {
                // Prevent infinite loop - if no tokens were consumed, advance by 1
                current_pos += 1;
                report!(
                    "handler_map",
                    "process_all",
                    Warning,
                    Process,
                    "process_all: no tokens consumed, advancing by 1 to prevent infinite loop"
                        .to_string(),
                    true
                );
            } else {
                current_pos += tokens_consumed;
            }
        }

        report!(
            "handler_map",
            "process_all",
            Info,
            Process,
            format!(
                "process_all: completed processing {} results from {} tokens",
                results.len(),
                tokens.len()
            ),
            true
        );

        Ok(results)
    }

    /// Calculate how many tokens were consumed based on the HandlerResult token range field
    pub(crate) fn calculate_tokens_consumed(
        &self,
        result: &HandlerResult,
        total_tokens: usize,
    ) -> usize {
        // Use the stored token range length from the HandlerResult, capped at total_tokens for safety
        result.token_count().min(total_tokens)
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
        ProcessedResult {
            id,
            result,
            start_idx,
            end_idx,
            tokens_consumed: end_idx - start_idx,
        }
    }

    pub fn with_end_idx(mut self, end_idx: usize) -> Self {
        self.end_idx = end_idx;
        self.tokens_consumed = end_idx - self.start_idx;
        self
    }

    pub fn with_tokens_consumed(mut self, count: usize) -> Self {
        self.tokens_consumed = count;
        self.end_idx = self.start_idx + count;
        self
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
            total_tokens_processed: total_consumed,
            total_tokens_consumed: total_consumed,
            results,
        }
    }

    /// Add a single result to the collection
    pub fn add(&mut self, result: ProcessedResult) {
        self.total_tokens_consumed += result.tokens_consumed;
        self.total_tokens_processed += result.tokens_consumed;
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
        HandlerResult::Converted(_, _, rust_code, _) => {
            if !rust_code.trim().is_empty() {
                Some(rust_code.clone())
            } else {
                None
            }
        }
        HandlerResult::Extracted(_, _, rust_code, _) => {
            if !rust_code.trim().is_empty() {
                Some(rust_code.clone())
            } else {
                None
            }
        }
        HandlerResult::Completed(_, _, rust_code, _) => Some(rust_code.clone()),
        HandlerResult::Processed(_, _, rust_code, _) => Some(rust_code.clone()),
        _ => None,
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
    /// Handler processed and produced converted Rust code (alias of RustCode)
    Extracted(ExtractedElement, Range<usize>, String, Id),
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
