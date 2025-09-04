use crate::error::ConversionError;
use crate::extract::ExtractedElement;
use crate::{debug, Context, ConvertedElement, Token};
use crate::context::HandlerReport;
use std::collections::HashMap;
use std::fmt::Display;
use std::hash::{Hash, Hasher};
use std::os::unix::raw::mode_t;
use std::pin::pin;
use std::time::{SystemTime, UNIX_EPOCH};

#[derive(Debug, Clone,PartialEq,Eq,PartialOrd,Ord)]
pub struct Handler {
    pub id: Id,
    pub role: String,
    pub priority: u64,
    pub process: Option<fn(&[Token], &mut Context) -> Result<bool,ConversionError>>,
    pub handle: Option<fn(&[Token], &mut Context) -> Result<HandlerResult, ConversionError>>,
    pub extract: Option<fn(&[Token], &mut Context) -> Result<Option<ExtractedElement>, ConversionError>>,
    pub convert: Option<fn(&[Token], &mut Context) -> Result<Option<ConvertedElement>, ConversionError>>,
    pub report: Option<fn(&[Token], &mut Context) -> Result<HandlerReport, ConversionError>>,
    pub result: Option<fn(&[Token], HandlerResult, &mut Context) -> Result<HandlerResult, ConversionError>>,
    pub redirect: Option<fn(&[Token], HandlerResult, &mut Context) -> Result<HandlerResult, ConversionError>>,
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
    pub fn num(&self) -> u64 {
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
    pub fn process(&self, tokens: &[Token], context: &mut Context) -> Result<bool,ConversionError> {
        (self.process.unwrap())(tokens, context)
    }
    pub fn handle(&self, tokens: &[Token], context: &mut Context) -> Result<HandlerResult, ConversionError> {
        (self.handle.unwrap())(tokens, context)
    }
    pub fn extract(&self, tokens: &[Token], context: &mut Context) -> Result<Option<ExtractedElement>, ConversionError> {
        (self.extract.unwrap())(tokens, context)
    }
    pub fn convert(&self, tokens: &[Token], context: &mut Context) -> Result<Option<ConvertedElement>, ConversionError> {
        (self.convert.unwrap())(tokens, context)
    }
    pub fn report(&self, tokens: &[Token], context:&mut Context) -> Result<HandlerReport, ConversionError> {
        (self.report.unwrap())(tokens, context)
    }
    pub fn result(&self, tokens: &[Token], result: HandlerResult, context: &mut Context) -> Result<HandlerResult, ConversionError> {
        (self.result.unwrap())(tokens, result, context)
    }
    pub fn redirect(&self, tokens: &[Token], result: HandlerResult, context: &mut Context) -> Result<HandlerResult, ConversionError> {
        (self.redirect.unwrap())(tokens, result, context)
    }
    /// Returns a string representation of the handler including its name and role
    pub fn to_string(&self) -> String {
        format!("Handler({}, {})", self.id.name(), self.role)
    }
}
#[derive(Debug,Clone,PartialEq,Eq)]
pub struct HandlerMap {
    pub name: String,
    pub ids: Vec<Id>,
    pub roles: HashMap<Id,String>,
    pub priorities: HashMap<Id,u64>,
    pub current: Option<Id>,
    pub last: Option<Id>,
    pub processors: HashMap<Id,Option<fn(&[Token], &mut Context) -> Result<bool,ConversionError>>>,
    pub handlers: HashMap<Id,Option<fn(&[Token], &mut Context) -> Result<HandlerResult, ConversionError>>>,
    pub extractors: HashMap<Id,Option<fn(&[Token], &mut Context) -> Result<Option<ExtractedElement>, ConversionError>>>,
    pub convertors: HashMap<Id,Option<fn(&[Token], &mut Context) -> Result<Option<ConvertedElement>, ConversionError>>>,
    pub reporters: HashMap<Id,Option<fn(&[Token], &mut Context) -> Result<HandlerReport, ConversionError>>>,
    pub results: HashMap<Id,Option<fn(&[Token], HandlerResult, &mut Context) -> Result<HandlerResult, ConversionError>>>,
    pub redirectors: HashMap<Id,Option<fn(&[Token], HandlerResult, &mut Context) -> Result<HandlerResult, ConversionError>>>,
}
/// Registry
impl HandlerMap {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            current: None,
            last:None,
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
        self.processors.insert(id.clone(), handler.process);
        self.handlers.insert(id.clone(), handler.handle);
        self.extractors.insert(id.clone(), handler.extract);
        self.convertors.insert(id.clone(), handler.convert);
        self.reporters.insert(id.clone(), handler.report);
        self.results.insert(id.clone(), handler.result);
        self.redirectors.insert(id.clone(), handler.redirect);
    }

    /// Register a handler in both the registry and make it accessible via the context
    /// This is useful for handlers that need to be accessible to other handlers
    pub fn register_shared(&mut self, handler: Handler, context: &mut Context, id: &Id) {
        let handler_name = handler.id.name();
        self.register(handler.clone());
        if let Some(handler) = self.get_handler_by_name(&handler_name) {
            context.handlers.register(handler);
            context.set_value(id.name(), handler_name);
        }
    }
    

    /// Create a Handler instance from its components
    fn create_handler_from_id(&self, id: &Id) -> Option<Handler> {
        match (self.ids.clone(), self.roles.get(id), self.priorities.get(id)) {
            (_idv, role, priority) => {
                let mut handler = Handler::new(id.clone(), role.unwrap().clone(), *priority.unwrap());
                handler.process = self.processors.get(id).cloned().flatten();
                handler.handle = self.handlers.get(id).cloned().flatten();
                handler.extract = self.extractors.get(id).cloned().flatten();
                handler.convert = self.convertors.get(id).cloned().flatten();
                handler.report = self.reporters.get(id).cloned().flatten();
                handler.result = self.results.get(id).cloned().flatten();
                handler.redirect = self.redirectors.get(id).cloned().flatten();
                Some(handler)
            }
            _ => None,
        }
    }

    /// Get a reference to all registered handlers
    pub fn get_handlers(&self) -> Vec<Handler> {
        let mut handlers = self.ids.iter()
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
    /// Get a handler by name
    pub fn get_handler_by_name(&self, name: &str) -> Option<Handler> {
        self.ids.iter().find(|id| id.name() == name).and_then(|id| self.create_handler_from_id(id))
    }

    /// Set the current handler and track the last handler
    pub fn set_current_handler(&mut self, id: Id) {
        if let Some(current) = &self.current {
            self.last = Some(current.clone());
            debug!("Handler tracking: {} -> {}", current.name(), id.name());
        }
        self.current = Some(id);
    }

    /// Get the current handler
    pub fn get_current_handler(&self) -> Option<Handler> {
        self.current.clone().and_then(|id| self.create_handler_from_id(&id))
    }

    /// Get the last handler that was used
    pub fn get_last_handler(&self) -> Option<Handler> {
        self.last.clone().and_then(|id| self.create_handler_from_id(&id))
    }

    /// Mark a handler as successful
    pub fn mark_handler_success(&mut self, handler_name: &str, tokens_consumed: usize) {
        let handler_id = get_id(handler_name);
        self.set_current_handler(handler_id.clone());
        debug!("Handler success: {} processed {} tokens", handler_name, tokens_consumed);
    }

    /// Mark a handler as failed
    pub fn mark_handler_failure(&mut self, handler_name: &str, error: &str) {
        let handler_id = get_id(handler_name);
        if let Some(current) = &self.current {
            self.last = Some(current.clone());
        }
        debug!("Handler failure: {} failed with error: {}", handler_name, error);
    }

    /// Get debugging information about handler state
    pub fn get_handler_debug_info(&self) -> String {
        let current_name = self.current.as_ref()
            .map(|id| id.name().clone())
            .unwrap_or_else(|| "None".to_string());
        let last_name = self.last.as_ref()
            .map(|id| id.name().clone())
            .unwrap_or_else(|| "None".to_string());
        
        format!("HandlerMap Debug Info:\n  Current: {}\n  Last: {}\n  Total Handlers: {}", 
                current_name, last_name, self.ids.len())
    }

    pub fn find_handler(
        &self,
        tokens: &[Token],
        context: &mut Context,
    ) -> Option<Handler> {
        if tokens.is_empty() {
            debug!("find_handler: empty tokens, no handler needed");
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
                handler
            ));
        }

        debug!("find_handler: trying to handle {} tokens", tokens.len());
        // First few tokens for context in logs
        for i in 0..std::cmp::min(5, tokens.len()) {
            debug!("  Token[{}]: {:?}", i, tokens[i]);
        }

        const MAX_DEPTH: usize = 20; // Increased depth limit for complex fallback chains
        const MAX_HANDLERS: usize = 150; // Prevent runaway processing
        let mut handlers_checked = 0;

        // Keep trying handlers until we find one or exhaust all options
        while let Some((_, handler_name, depth, handler)) = handler_queue.pop() {
            handlers_checked += 1;

            if handlers_checked > MAX_HANDLERS {
                debug!("find_handler: max handlers ({}) checked, stopping cascade", MAX_HANDLERS);
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
                debug!("find_handler: max depth ({}) reached for handler '{}'", MAX_DEPTH, handler_name);
                continue;
            }

            // Mark this handler as tried with current depth
            tried_handlers.insert(handler_name.clone(), depth);

            // Check if this handler can handle the tokens
            debug!("find_handler: trying handler '{}' (priority: {}, depth: {})",
                handler_name, handler.priority, depth);

            if handler.process.is_some() && handler.process(tokens, context).unwrap_or(false) {
                debug!("find_handler: handler '{}' can handle tokens", handler_name);
                // Track successful handler selection
                context.handlers.set_current_handler(handler.id.clone());
                return Some(handler);
            }
        }

        debug!("find_handler: no handler found after checking {} handlers", handlers_checked);
        None
    }

    /// Process tokens with the first handler that can handle them
    pub fn process(
        &self,
        tokens: &[Token],
        context: &mut Context,
    ) -> Result<ProcessedResult, ConversionError> {
        let id = get_id("process");
        if tokens.is_empty() {
            return Ok(ProcessedResult::new(0, 0, id.clone(), HandlerResult::NotHandled(Some(tokens.to_vec()), id)));
        }

        if let Some(handler) = self.find_handler(tokens, context) {
            debug!("process: trying handler '{}'", handler.name());
            let handler_id = get_id(&handler.name());

            if let Some(handle_fn) = handler.handle {
                let result = handle_fn(tokens, context)?;
                if !matches!(result.clone(), HandlerResult::NotHandled(_,_)) {
                    debug!("process: handler '{}' processed tokens", handler.name());

                    // Determine how many tokens were consumed
                    let tokens_consumed = if let Some(consumed_fn) = handler.report {
                        match consumed_fn(tokens,context) {
                            Ok(report) => report.tokens_consumed,
                            Err(_) => tokens.len()
                        }
                    } else {
                        tokens.len()
                    };

                    // Track successful processing
                    context.handlers.mark_handler_success(handler.name(), tokens_consumed);

                    return Ok(ProcessedResult::new(0, tokens_consumed, handler_id, result));
                } else {
                    // Track handler failure
                    context.handlers.mark_handler_failure(handler.name(), "Handler returned NotHandled");
                }
            }
        }

        debug!("process: no handler could process tokens");
        Ok(ProcessedResult::new(0, 0, id.clone(), HandlerResult::NotHandled(Some(tokens.to_vec()), id)))
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
    pub fn new(start_idx: usize, end_idx:usize, id: Id, result: HandlerResult) -> ProcessedResult {
        ProcessedResult {
            id: id,
            result,
            start_idx,
            end_idx: start_idx,
            tokens_consumed: 0,
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
#[derive(Debug, Clone,PartialEq,Eq,PartialOrd,Ord,Hash)]
pub struct Id(pub u64,pub String);
impl Id {
    pub const fn new(num: u64,name: String) -> Id {
        Id(num,name)
    }
    pub const fn name(&self) -> &String {
        &self.1
    }
    pub fn num(&self) -> u64 {
        self.0
    }
    pub fn gen_id(name: &str) -> Id {
        let seed = (SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_nanos() % u64::MAX as u128) as u64;
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        hasher.write_u64(seed);
        seed.hash(&mut hasher);
        name.hash(&mut hasher);
        let num = hasher.finish();
        Id(num,name.to_string())
    }
}
impl Default for Id {
    fn default() -> Self {
        get_id("default")
    }
}
#[allow(static_mut_refs)]
pub fn get_id(key: &str) -> Id {
    unsafe {
        ID_MAP.get_or_insert(HashMap::new()).entry(key.to_string()).or_insert_with(||Id::gen_id(key)).clone()
    }
}
pub static mut ID_MAP: Option<HashMap<String,Id>> = None;
/// Result of handling a token or sequence of tokens
#[derive(Debug, Clone, PartialEq)]
pub enum HandlerResult {
    Processed(Option<Vec<Token>>, String, Id),
    /// Handler processed the tokens and directly replaced them with a string
    Replaced(Option<Vec<Token>>,String,Id),
    /// Handler could not process these tokens
    NotHandled(Option<Vec<Token>>,Id),
    /// Handler wants to redirect to another handler and process the result
    Redirected(Option<Vec<Token>>,String, Id, Id),
    /// Handler processed and produced converted Rust code (alias of RustCode)
    Extracted(ExtractedElement,String,Id),
    Converted(ConvertedElement,String,Id),
}

impl Display for HandlerResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HandlerResult::Processed(tokens, s, id) => write!(f, "Handler: {} reports succesful.\n Code:\n{}", id.name(), s),
            HandlerResult::Extracted(element, s, id) => write!(f, "Handler: {} reports extraction succesful.\n Code:\n{}", id.name(), s),
            HandlerResult::Converted(element, s, id) => write!(f, "Handler: {} reports conversion succesful.\n Code:\n{}", id.name(), s),
            HandlerResult::Replaced(tokens, s, id) => write!(f, "Handler: {} reports replacment succesful.\n Code:\n{}", id.name(), s),
            HandlerResult::NotHandled(tokens, id) => write!(f, "Handler: {} reports not handled", id.name()),
            HandlerResult::Redirected(tokens, s, id1, id2) => {
                let tokens = tokens.clone().unwrap();
                write!(f, "Handler: {} reports redirection to Handler: {}.\n Code:\n{}\nTokens:{}", s, id1.name(), id2.name(), Token::tokens_to_string(tokens.as_slice(),0,tokens.len()).unwrap()) },
        }
    }
}
