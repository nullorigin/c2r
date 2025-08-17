use crate::error::ConversionError;
use crate::token_parser::{Statement, Token};
use std::any::Any;
use std::collections::HashMap;

/// Context containing state information for the parser
pub struct ParserContext {
    /// Whether declarations have already been processed
    pub declarations_done: bool,
    /// Additional context-specific data stored as string keys and values
    pub data: HashMap<String, String>,
    /// Additional context values that can store any type
    values: HashMap<String, Box<dyn Any>>,
}

impl ParserContext {
    pub fn new() -> Self {
        Self {
            declarations_done: false,
            data: HashMap::new(),
            values: HashMap::new(),
        }
    }

    pub fn set(&mut self, key: &str, value: String) {
        self.data.insert(key.to_string(), value);
    }

    pub fn get(&self, key: &str) -> Option<&String> {
        self.data.get(key)
    }

    /// Set a value of any type in the context
    pub fn set_value<T: 'static>(&mut self, key: &str, value: Box<T>) {
        self.values.insert(key.to_string(), value as Box<dyn Any>);
    }

    /// Get a reference to a value of a specific type from the context
    pub fn get_value<T: 'static>(&self, key: &str) -> Option<&T> {
        if let Some(value) = self.values.get(key) {
            value.downcast_ref::<T>()
        } else {
            None
        }
    }

    /// Get a mutable reference to a value of a specific type from the context
    pub fn get_value_mut<T: 'static>(&mut self, key: &str) -> Option<&mut T> {
        if let Some(value) = self.values.get_mut(key) {
            value.downcast_mut::<T>()
        } else {
            None
        }
    }

    /// Check if a value of a specific type exists in the context
    pub fn has_value<T: 'static>(&self, key: &str) -> bool {
        if let Some(value) = self.values.get(key) {
            value.is::<T>()
        } else {
            false
        }
    }

    /// Remove a value from the context
    pub fn remove_value(&mut self, key: &str) -> Option<Box<dyn Any>> {
        self.values.remove(key)
    }
}

/// Result of handling a token or sequence of tokens
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum HandlerResult {
    /// Handler processed the tokens and produced a statement
    Statement(Statement),
    /// Handler processed the tokens and produced a Rust code string
    RustCode(String),
    /// Handler processed the tokens and directly replaced them with a string
    Replace(String),
    /// Handler could not process these tokens
    NotHandled,
}

/// Trait defining the interface for token handlers
pub trait TokenHandler {
    /// Check if this handler can handle the given token in the current context
    fn can_handle(&self, tokens: &[Token], context: &ParserContext) -> bool;

    /// Process the tokens and produce a result
    fn handle(
        &self,
        tokens: &[Token],
        context: &mut ParserContext,
    ) -> Result<HandlerResult, ConversionError>;
}

/// Registry for token handlers
pub struct HandlerRegistry {
    handlers: Vec<Box<dyn TokenHandler>>,
}

impl HandlerRegistry {
    pub fn new() -> Self {
        HandlerRegistry {
            handlers: Vec::new(),
        }
    }

    pub fn register(&mut self, handler: Box<dyn TokenHandler>) {
        self.handlers.push(handler);
    }

    /// Get a reference to all registered handlers
    pub fn get_handlers(&self) -> &Vec<Box<dyn TokenHandler>> {
        &self.handlers
    }

    /// Find a handler that can process the given tokens
    pub fn find_handler(
        &self,
        tokens: &[Token],
        context: &ParserContext,
    ) -> Option<&Box<dyn TokenHandler>> {
        for handler in &self.handlers {
            if handler.can_handle(tokens, context) {
                return Some(handler);
            }
        }
        None
    }

    pub fn process(
        &self,
        tokens: &[Token],
        context: &mut ParserContext,
    ) -> Result<HandlerResult, ConversionError> {
        if let Some(handler) = self.find_handler(tokens, context) {
            handler.handle(tokens, context)
        } else {
            Ok(HandlerResult::NotHandled)
        }
    }
}
