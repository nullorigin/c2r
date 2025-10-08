#![allow(
    unused_variables,
    unused_assignments,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals
)]

use crate::{C2RError, Id, Reason, Result, debug, error, info, warn};
use core::cmp::Ordering;
use core::mem::transmute;
use core::ops::{Bound, Range, RangeBounds};
use core::option::Option::{self, None, Some};
use core::result::Result::Ok;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Display;
use std::hash::Hash;
use std::hash::Hasher;
use std::ops::Add;
use std::ops::BitAnd;
use std::ops::BitOr;
use std::ops::BitXor;
use std::ops::Div;
use std::ops::Mul;
use std::ops::Rem;
use std::ops::Sub;
use std::str::FromStr;
use std::str::from_utf8;
use std::sync::{Arc, RwLock, Condvar, Mutex};
use std::sync::atomic::AtomicUsize;
use std::time::Duration;



const TOKEN_MAX: usize = 8192;
const TOKENBOX_CAPACITY: usize = 10000;

/// TokenBox - A thread-safe static collection container for tokens
/// Acts as the central storage that TokenSlots feed into and threads consume from
#[derive(Debug)]
pub struct TokenBox {
    /// Main token storage with thread-safe access
    tokens: Arc<RwLock<Vec<Token>>>,
    /// Metadata storage for token indexing and lookup
    metadata: Arc<RwLock<HashMap<String, TokenMetadata>>>,
    /// Collection status tracking
    status: Arc<Mutex<TokenBoxStatus>>,
    /// Condition variable for signaling collection availability
    collection_ready: Arc<Condvar>,
    /// Unique identifier for this TokenBox
    id: Id,
    /// Maximum capacity before collection is "full"
    capacity: usize,
}

impl Clone for TokenBox {
    fn clone(&self) -> Self {
        TokenBox {
            tokens: self.tokens.clone(),
            metadata: self.metadata.clone(),
            status: self.status.clone(),
            collection_ready: self.collection_ready.clone(),
            id: self.id.clone(),
            capacity: self.capacity,
        }
    }
}
impl PartialEq for TokenBox {    
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
        && self.capacity == other.capacity
    }
}
impl Eq for TokenBox {}
impl std::hash::Hash for TokenBox {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        self.capacity.hash(state);
    }
}


#[derive(Debug, Clone)]
struct TokenMetadata {
    index: usize,
    slot_origin: Id,
    timestamp: std::time::Instant,
    token_type: String,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenBoxStatus {
    Collecting,
    Full,
    BeingConsumed,
    Empty,
}

impl TokenBox {
    /// Create a new TokenBox with specified capacity
    pub fn new(id: Id, capacity: usize) -> Self {
        TokenBox {
            tokens: Arc::new(RwLock::new(Vec::with_capacity(capacity))),
            metadata: Arc::new(RwLock::new(HashMap::new())),
            status: Arc::new(Mutex::new(TokenBoxStatus::Empty)),
            collection_ready: Arc::new(Condvar::new()),
            id,
            capacity,
        }
    }

    /// Create a default TokenBox with standard capacity
    pub fn new_default(id: Id) -> Self {
        Self::new(id, TOKENBOX_CAPACITY)
    }

    /// Deposit tokens from a TokenSlot (called by TokenSlot.flush_to_tokenbox())
    pub fn deposit_from_slot(&self, tokens: Vec<Token>, slot_id: &Id) -> Result<usize> {
        let mut status_guard = match self.status.lock() {
            Ok(guard) => guard,
            Err(poisoned) => {
                warn!("TokenBox deposit status lock poisoned, recovering...");
                poisoned.into_inner()
            }
        };

        // Don't accept deposits if being consumed
        if *status_guard == TokenBoxStatus::BeingConsumed {
            return Err(C2RError::new(
                crate::Kind::Logic,
                Reason::Other("TokenBox is being consumed, cannot deposit"),
                None
            ));
        }

        let mut token_storage = match self.tokens.write() {
            Ok(guard) => guard,
            Err(poisoned) => {
                warn!("TokenBox deposit tokens lock poisoned, recovering...");
                poisoned.into_inner()
            }
        };

        let mut metadata_storage = match self.metadata.write() {
            Ok(guard) => guard,
            Err(poisoned) => {
                warn!("TokenBox deposit metadata lock poisoned, recovering...");
                poisoned.into_inner()
            }
        };

        let starting_index = token_storage.len();
        let deposited_count = tokens.len();
        
        // Add tokens and create metadata
        for (offset, token) in tokens.into_iter().enumerate() {
            let index = starting_index + offset;
            let metadata = TokenMetadata {
                index,
                slot_origin: slot_id.clone(),
                timestamp: std::time::Instant::now(),
                token_type: token.type_name().to_string(),
            };
            
            token_storage.push(token.clone());
            metadata_storage.insert(format!("{}-{}", slot_id, index), metadata);
        }

        // Update status based on capacity
        if token_storage.len() >= self.capacity {
            *status_guard = TokenBoxStatus::Full;
            // Notify waiting consumers that collection is ready
            self.collection_ready.notify_all();
        } else if *status_guard == TokenBoxStatus::Empty {
            *status_guard = TokenBoxStatus::Collecting;
        }

        drop(status_guard);
        drop(token_storage);
        drop(metadata_storage);

        info!("TokenBox[{}] deposited {} tokens, total: {}", 
              self.id, deposited_count, self.len());

        Ok(deposited_count)
    }

    /// Collect tokens for processing (thread-safe consumer method)
    pub fn collect_tokens(&self, max_tokens: Option<usize>) -> Result<Vec<Token>> {
        let mut status_guard = match self.status.lock() {
            Ok(guard) => guard,
            Err(poisoned) => {
                warn!("TokenBox collect status lock poisoned, recovering...");
                poisoned.into_inner()
            }
        };

        // Wait for tokens to be available if empty
        while *status_guard == TokenBoxStatus::Empty {
            status_guard = match self.collection_ready.wait_timeout(status_guard, Duration::from_millis(100)) {
                Ok((guard, _)) => guard,
                Err(poisoned) => {
                    warn!("TokenBox collect wait poisoned, recovering...");
                    poisoned.into_inner().0
                }
            };
        }

        // Mark as being consumed
        *status_guard = TokenBoxStatus::BeingConsumed;
        drop(status_guard);

        let mut token_storage = match self.tokens.write() {
            Ok(guard) => guard,
            Err(poisoned) => {
                warn!("TokenBox collect tokens lock poisoned, recovering...");
                poisoned.into_inner()
            }
        };

        let collect_count = max_tokens.unwrap_or(token_storage.len()).min(token_storage.len());
        let collected_tokens: Vec<Token> = token_storage.drain(..collect_count).collect();

        // Update status
        let mut status_guard = match self.status.lock() {
            Ok(guard) => guard,
            Err(poisoned) => {
                warn!("TokenBox collect final status lock poisoned, recovering...");
                poisoned.into_inner()
            }
        };

        *status_guard = if token_storage.is_empty() {
            TokenBoxStatus::Empty
        } else {
            TokenBoxStatus::Collecting
        };

        drop(status_guard);
        drop(token_storage);

        info!("TokenBox[{}] collected {} tokens", self.id, collected_tokens.len());
        Ok(collected_tokens)
    }

    /// Get current token count
    pub fn len(&self) -> usize {
        match self.tokens.read() {
            Ok(guard) => guard.len(),
            Err(poisoned) => {
                warn!("TokenBox len lock poisoned, recovering...");
                poisoned.into_inner().len()
            }
        }
    }

    /// Check if TokenBox is empty
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Check if TokenBox is full
    pub fn is_full(&self) -> bool {
        self.len() >= self.capacity
    }

    /// Get current status
    pub fn status(&self) -> TokenBoxStatus {
        match self.status.lock() {
            Ok(guard) => guard.clone(),
            Err(poisoned) => {
                warn!("TokenBox status lock poisoned, recovering...");
                poisoned.into_inner().clone()
            }
        }
    }

    /// Get capacity
    pub fn capacity(&self) -> usize {
        self.capacity
    }

    /// Get id
    pub fn id(&self) -> &Id {
        &self.id
    }

    /// Peek at tokens without consuming them
    pub fn peek_tokens(&self, count: usize) -> Vec<Token> {
        match self.tokens.read() {
            Ok(guard) => {
                guard.iter().take(count).cloned().collect()
            }
            Err(poisoned) => {
                warn!("TokenBox peek lock poisoned, recovering...");
                poisoned.into_inner().iter().take(count).cloned().collect()
            }
        }
    }

    /// Clear all tokens (use with caution)
    pub fn clear(&self) {
        let mut token_storage = match self.tokens.write() {
            Ok(guard) => guard,
            Err(poisoned) => {
                warn!("TokenBox clear tokens lock poisoned, recovering...");
                poisoned.into_inner()
            }
        };

        let mut metadata_storage = match self.metadata.write() {
            Ok(guard) => guard,
            Err(poisoned) => {
                warn!("TokenBox clear metadata lock poisoned, recovering...");
                poisoned.into_inner()
            }
        };

        token_storage.clear();
        metadata_storage.clear();

        let mut status_guard = match self.status.lock() {
            Ok(guard) => guard,
            Err(poisoned) => {
                warn!("TokenBox clear status lock poisoned, recovering...");
                poisoned.into_inner()
            }
        };

        *status_guard = TokenBoxStatus::Empty;
    }
}
#[derive(Debug)]
pub struct TokenSlot {
    tokens: Arc<RwLock<Vec<Token>>>,
    buffer: Arc<RwLock<Vec<Token>>>,
    cursor: AtomicUsize,
    id: Id,
    token_map: Arc<RwLock<HashMap<String, usize>>>,
}
impl Clone for TokenSlot {
    fn clone(&self) -> Self {
        TokenSlot {
            tokens: self.tokens.clone(),
            buffer: self.buffer.clone(),
            cursor: AtomicUsize::new(self.cursor.load(std::sync::atomic::Ordering::SeqCst)),
            id: self.id.clone(),
            token_map: self.token_map.clone(),
        }
    }
}
impl TokenSlot {
    pub fn new(tokens: Vec<Token>, cursor: usize, id: Id) -> Self {
        let mut token_map = HashMap::new();
        for (index, token) in tokens.iter().enumerate() {
            token_map.insert(token.to_string(), index);
        }
        
        TokenSlot {
            tokens: Arc::new(RwLock::new(tokens)),
            buffer: Arc::new(RwLock::new(Vec::new())),
            cursor: AtomicUsize::new(cursor),
            id,
            token_map: Arc::new(RwLock::new(token_map)),
        }
    }

    pub fn tokens(&self) -> Vec<Token> {
        match self.tokens.read() {
            Ok(guard) => guard.clone(),
            Err(poisoned) => {
                warn!("TokenSlot tokens lock poisoned, recovering...");
                poisoned.into_inner().clone()
            }
        }
    }

    pub fn get_range<R>(&self, range: R) -> Vec<Token>
    where
        R: RangeBounds<usize>,
    {
        let tokens = match self.tokens.read() {
            Ok(guard) => guard,
            Err(poisoned) => {
                warn!("TokenSlot get_range lock poisoned, recovering...");
                poisoned.into_inner()
            }
        };
        let start = match range.start_bound() {
            Bound::Included(&n) => n,
            Bound::Excluded(&n) => n + 1,
            Bound::Unbounded => 0,
        };
        let end = match range.end_bound() {
            Bound::Included(&n) => n + 1,
            Bound::Excluded(&n) => n,
            Bound::Unbounded => tokens.len(),
        };
        
        if start <= end && start < tokens.len() {
            tokens[start..end.min(tokens.len())].to_vec()
        } else {
            Vec::new()
        }
    }

    pub fn lookup(&self, key: &str) -> Option<Token> {
        let map = match self.token_map.read() {
            Ok(guard) => guard,
            Err(poisoned) => {
                warn!("TokenSlot lookup map lock poisoned, recovering...");
                poisoned.into_inner()
            }
        };
        let tokens = match self.tokens.read() {
            Ok(guard) => guard,
            Err(poisoned) => {
                warn!("TokenSlot lookup tokens lock poisoned, recovering...");
                poisoned.into_inner()
            }
        };
        map.get(key).and_then(|&index| tokens.get(index).cloned())
    }

    pub fn seek(&self, position: usize) {
        self.cursor.store(position, std::sync::atomic::Ordering::SeqCst);
    }

    pub fn buffer_token(&self, token: Token) {
        match self.buffer.write() {
            Ok(mut guard) => guard.push(token),
            Err(poisoned) => {
                warn!("TokenSlot buffer_token lock poisoned, recovering...");
                poisoned.into_inner().push(token);
            }
        }
    }

    pub fn flush_buffer(&self) {
        {
            let mut buffer = match self.buffer.write() {
                Ok(guard) => guard,
                Err(poisoned) => {
                    warn!("TokenSlot flush_buffer buffer lock poisoned, recovering...");
                    poisoned.into_inner()
                }
            };
            let mut tokens = match self.tokens.write() {
                Ok(guard) => guard,
                Err(poisoned) => {
                    warn!("TokenSlot flush_buffer tokens lock poisoned, recovering...");
                    poisoned.into_inner()
                }
            };
            tokens.extend(buffer.drain(..));
        } // Drop both locks here
        self.rebuild_token_map();
    }

    /// Flush tokens to a TokenBox (arcade-themed producer method)
    pub fn flush_to_tokenbox(&self, tokenbox: &TokenBox) -> Result<usize> {
        let mut tokens = match self.tokens.write() {
            Ok(guard) => guard,
            Err(poisoned) => {
                warn!("TokenSlot flush_to_tokenbox tokens lock poisoned, recovering...");
                poisoned.into_inner()
            }
        };

        if tokens.is_empty() {
            return Ok(0);
        }

        // Drain all tokens from this slot
        let tokens_to_flush: Vec<Token> = tokens.drain(..).collect();
        let count = tokens_to_flush.len();

        // Clear the token map since we're emptying the slot
        match self.token_map.write() {
            Ok(mut map) => map.clear(),
            Err(poisoned) => {
                warn!("TokenSlot flush_to_tokenbox map lock poisoned, recovering...");
                poisoned.into_inner().clear();
            }
        }

        // Deposit into TokenBox
        match tokenbox.deposit_from_slot(tokens_to_flush, &self.id) {
            Ok(deposited) => {
                info!("TokenSlot[{}] flushed {} tokens to TokenBox[{}]", 
                      self.id, deposited, tokenbox.id());
                Ok(deposited)
            }
            Err(e) => {
                error!("Failed to flush TokenSlot[{}] to TokenBox[{}]: {}", 
                       self.id, tokenbox.id(), e);
                Err(e)
            }
        }
    }

    /// Flush buffer to TokenBox without storing in local tokens first
    pub fn flush_buffer_to_tokenbox(&self, tokenbox: &TokenBox) -> Result<usize> {
        let mut buffer = match self.buffer.write() {
            Ok(guard) => guard,
            Err(poisoned) => {
                warn!("TokenSlot flush_buffer_to_tokenbox lock poisoned, recovering...");
                poisoned.into_inner()
            }
        };

        if buffer.is_empty() {
            return Ok(0);
        }

        // Drain all tokens from buffer directly to TokenBox
        let buffer_tokens: Vec<Token> = buffer.drain(..).collect();
        let count = buffer_tokens.len();

        // Deposit into TokenBox
        match tokenbox.deposit_from_slot(buffer_tokens, &self.id) {
            Ok(deposited) => {
                info!("TokenSlot[{}] flushed buffer {} tokens to TokenBox[{}]", 
                      self.id, deposited, tokenbox.id());
                Ok(deposited)
            }
            Err(e) => {
                error!("Failed to flush TokenSlot[{}] buffer to TokenBox[{}]: {}", 
                       self.id, tokenbox.id(), e);
                Err(e)
            }
        }
    }

    pub fn sort(&self) {
        {
            match self.tokens.write() {
                Ok(mut guard) => guard.sort_by(|a, b| a.to_string().cmp(&b.to_string())),
                Err(poisoned) => {
                    warn!("TokenSlot sort lock poisoned, recovering...");
                    poisoned.into_inner().sort_by(|a, b| a.to_string().cmp(&b.to_string()));
                }
            }
        } // Drop lock here
        self.rebuild_token_map();
    }

    pub fn dedup(&self) {
        {
            match self.tokens.write() {
                Ok(mut guard) => guard.dedup(),
                Err(poisoned) => {
                    warn!("TokenSlot dedup lock poisoned, recovering...");
                    poisoned.into_inner().dedup();
                }
            }
        } // Drop lock here
        self.rebuild_token_map();
    }

    pub fn push(&self, token: Token) {
        let mut tokens = match self.tokens.write() {
            Ok(guard) => guard,
            Err(poisoned) => {
                warn!("TokenSlot push tokens lock poisoned, recovering...");
                poisoned.into_inner()
            }
        };
        let index = tokens.len();
        tokens.push(token.clone());
        match self.token_map.write() {
            Ok(mut map) => { map.insert(token.to_string(), index); }
            Err(poisoned) => {
                warn!("TokenSlot push map lock poisoned, recovering...");
                poisoned.into_inner().insert(token.to_string(), index);
            }
        }
    }

    pub fn append(&self, mut other: Vec<Token>) {
        {
            let mut tokens = match self.tokens.write() {
                Ok(guard) => guard,
                Err(poisoned) => {
                    warn!("TokenSlot append lock poisoned, recovering...");
                    poisoned.into_inner()
                }
            };
            tokens.append(&mut other);
        } // Drop lock here
        self.rebuild_token_map();
    }

    pub fn pop(&self) -> Option<Token> {
        let result = {
            let mut tokens = match self.tokens.write() {
                Ok(guard) => guard,
                Err(poisoned) => {
                    warn!("TokenSlot pop lock poisoned, recovering...");
                    poisoned.into_inner()
                }
            };
            tokens.pop()
        }; // Drop lock here
        if result.is_some() {
            self.rebuild_token_map();
        }
        result
    }

    pub fn len(&self) -> usize {
        match self.tokens.read() {
            Ok(guard) => guard.len(),
            Err(poisoned) => {
                warn!("TokenSlot len lock poisoned, recovering...");
                poisoned.into_inner().len()
            }
        }
    }

    pub fn is_empty(&self) -> bool {
        match self.tokens.read() {
            Ok(guard) => guard.is_empty(),
            Err(poisoned) => {
                warn!("TokenSlot is_empty lock poisoned, recovering...");
                poisoned.into_inner().is_empty()
            }
        }
    }

    pub fn clear(&self) {
        match self.tokens.write() {
            Ok(mut guard) => guard.clear(),
            Err(poisoned) => {
                warn!("TokenSlot clear tokens lock poisoned, recovering...");
                poisoned.into_inner().clear();
            }
        }
        match self.token_map.write() {
            Ok(mut map) => map.clear(),
            Err(poisoned) => {
                warn!("TokenSlot clear map lock poisoned, recovering...");
                poisoned.into_inner().clear();
            }
        }
    }

    pub fn get(&self, index: usize) -> Option<Token> {
        match self.tokens.read() {
            Ok(guard) => guard.get(index).cloned(),
            Err(poisoned) => {
                warn!("TokenSlot get lock poisoned, recovering...");
                poisoned.into_inner().get(index).cloned()
            }
        }
    }

    pub fn insert(&self, index: usize, token: Token) {
        let should_rebuild = {
            let mut tokens = match self.tokens.write() {
                Ok(guard) => guard,
                Err(poisoned) => {
                    warn!("TokenSlot insert lock poisoned, recovering...");
                    poisoned.into_inner()
                }
            };
            if index <= tokens.len() {
                tokens.insert(index, token);
                true
            } else {
                false
            }
        }; // Drop lock here
        if should_rebuild {
            self.rebuild_token_map();
        }
    }

    pub fn remove(&self, index: usize) -> Option<Token> {
        let result = {
            let mut tokens = match self.tokens.write() {
                Ok(guard) => guard,
                Err(poisoned) => {
                    warn!("TokenSlot remove lock poisoned, recovering...");
                    poisoned.into_inner()
                }
            };
            if index < tokens.len() {
                Some(tokens.remove(index))
            } else {
                None
            }
        }; // Drop lock here
        if result.is_some() {
            self.rebuild_token_map();
        }
        result
    }

    pub fn contains(&self, token: &Token) -> bool {
        match self.tokens.read() {
            Ok(guard) => guard.contains(token),
            Err(poisoned) => {
                warn!("TokenSlot contains lock poisoned, recovering...");
                poisoned.into_inner().contains(token)
            }
        }
    }

    pub fn retain<F>(&self, mut f: F) 
    where
        F: FnMut(&Token) -> bool,
    {
        {
            match self.tokens.write() {
                Ok(mut guard) => guard.retain(|token| f(token)),
                Err(poisoned) => {
                    warn!("TokenSlot retain lock poisoned, recovering...");
                    poisoned.into_inner().retain(|token| f(token));
                }
            }
        } // Drop lock here
        self.rebuild_token_map();
    }

    pub fn extend<I>(&self, iter: I)
    where
        I: IntoIterator<Item = Token>,
    {
        {
            let mut tokens = match self.tokens.write() {
                Ok(guard) => guard,
                Err(poisoned) => {
                    warn!("TokenSlot extend lock poisoned, recovering...");
                    poisoned.into_inner()
                }
            };
            tokens.extend(iter);
        } // Drop lock here
        self.rebuild_token_map();
    }

    pub fn truncate(&self, len: usize) {
        {
            match self.tokens.write() {
                Ok(mut guard) => guard.truncate(len),
                Err(poisoned) => {
                    warn!("TokenSlot truncate lock poisoned, recovering...");
                    poisoned.into_inner().truncate(len);
                }
            }
        } // Drop lock here
        self.rebuild_token_map();
    }

    fn rebuild_token_map(&self) {
        // First, create a local copy of tokens to avoid holding both locks simultaneously
        let token_strings: Vec<String> = {
            let tokens = match self.tokens.read() {
                Ok(guard) => guard,
                Err(poisoned) => {
                    warn!("TokenSlot rebuild_token_map tokens lock poisoned, recovering...");
                    poisoned.into_inner()
                }
            };
            // Create a local copy and release the lock immediately
            tokens.iter().map(|token| token.to_string()).collect()
        }; // tokens lock is dropped here
        
        // Now rebuild the map with only the write lock held
        let mut map = match self.token_map.write() {
            Ok(guard) => guard,
            Err(poisoned) => {
                warn!("TokenSlot rebuild_token_map map lock poisoned, recovering...");
                poisoned.into_inner()
            }
        };
        
        map.clear();
        for (index, token_string) in token_strings.into_iter().enumerate() {
            map.insert(token_string, index);
        }
    }
}

impl Iterator for TokenSlot {
    type Item = Token;
    
    fn next(&mut self) -> Option<Token> {
        let cursor = self.cursor.load(std::sync::atomic::Ordering::SeqCst);
        let tokens = match self.tokens.read() {
            Ok(guard) => guard,
            Err(poisoned) => {
                warn!("TokenSlot iterator lock poisoned, recovering...");
                poisoned.into_inner()
            }
        };
        
        if cursor < tokens.len() {
            let token = tokens[cursor].clone();
            self.cursor.store(cursor + 1, std::sync::atomic::Ordering::SeqCst);
            Some(token)
        } else {
            None
        }
    }
}


impl Default for TokenSlot {
    fn default() -> Self {
        TokenSlot::new(Vec::new(), 0, Id::get("token_slot"))
    }
}

impl PartialEq for TokenSlot {
    fn eq(&self, other: &Self) -> bool {
        let self_tokens = match self.tokens.read() {
            Ok(guard) => guard,
            Err(poisoned) => {
                warn!("TokenSlot eq self lock poisoned, recovering...");
                poisoned.into_inner()
            }
        };
        let other_tokens = match other.tokens.read() {
            Ok(guard) => guard,
            Err(poisoned) => {
                warn!("TokenSlot eq other lock poisoned, recovering...");
                poisoned.into_inner()
            }
        };
        
        self.id == other.id && 
        *self_tokens == *other_tokens && 
        self.cursor.load(std::sync::atomic::Ordering::SeqCst) == other.cursor.load(std::sync::atomic::Ordering::SeqCst)
    }
}

impl Eq for TokenSlot {}

impl Hash for TokenSlot {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        let tokens = match self.tokens.read() {
            Ok(guard) => guard,
            Err(poisoned) => {
                warn!("TokenSlot hash lock poisoned, recovering...");
                poisoned.into_inner()
            }
        };
        tokens.hash(state);
        self.cursor.load(std::sync::atomic::Ordering::SeqCst).hash(state);
    }
}
pub enum Token {
    a([u8; TOKEN_MAX], usize),
    b(u8),
    c(char),
    f(f64),
    i(i128),
    s(String),
    l(&'static str),
    u(u128),
    v(Vec<u8>),
    w(String),    // Whitespace token variant - stores whitespace sequences
    d(Vec<char>), // Delimiter token variant - stores delimiter character sequences
    n(),          // None/consumed token variant - placeholder for consumed tokens
}
impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::a(a, s) => write!(f, "Token::a({:?}, {})", a, s),
            Token::b(b) => write!(f, "Token::b({})", b),
            Token::c(c) => write!(f, "Token::c({})", c),
            Token::f(fl) => write!(f, "Token::f({})", fl),
            Token::i(i) => write!(f, "Token::i({})", i),
            Token::s(s) => write!(f, "Token::s({})", s),
            Token::l(l) => write!(f, "Token::l({})", l),
            Token::u(u) => write!(f, "Token::u({})", u),
            Token::v(v) => write!(f, "Token::v({:?})", v),
            Token::w(w) => write!(f, "Token::w({})", w),
            Token::d(d) => write!(f, "Token::d({:?})", d),
            Token::n() => write!(f, "Token::n()"),
        }
    }
}
#[derive(Debug, Clone, PartialEq, Eq,Hash)]
pub struct Tokenizer {
    content: Vec<u8>,
    cursor: usize,
    slots: Vec<TokenSlot>,
    active_slot: usize,
}
impl Default for Tokenizer {
    fn default() -> Self {
        Tokenizer::new()
    }
}
pub type slot = TokenSlot;
pub type tok = Token;
impl Default for Token {
    fn default() -> Self {
        Token::b(0)
    }
}

impl Token {
    /// Get the type name of this token variant
    pub fn type_name(&self) -> &'static str {
        match self {
            Token::a(_, _) => "array",
            Token::b(_) => "byte", 
            Token::c(_) => "char",
            Token::f(_) => "float",
            Token::i(_) => "integer",
            Token::s(_) => "string",
            Token::l(_) => "literal",
            Token::u(_) => "unsigned",
            Token::v(_) => "vector",
            Token::w(_) => "whitespace",
            Token::d(_) => "delimiter", 
            Token::n() => "none",
        }
    }
}

impl Token {
    /// Create a new Token from a string
    pub fn new(s: &str) -> Self {
        Token::s(s.to_string())
    }
}
impl Clone for Token {
    fn clone(&self) -> Self {
        match self {
            Token::a(a, s) => Token::a(*a, *s),
            Token::b(b) => Token::b(*b),
            Token::c(c) => Token::c(*c),
            Token::f(f) => Token::f(*f),
            Token::i(i) => Token::i(*i),
            Token::s(s) => Token::s(s.clone()),
            Token::l(s) => Token::l(*s),
            Token::u(u) => Token::u(*u),
            Token::v(v) => Token::v(v.clone()),
            Token::w(w) => Token::w(w.clone()),
            Token::d(d) => Token::d(d.clone()),
            Token::n() => Token::n(),
        }
    }
}
impl Hash for Token {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Token::a(a, s) => (a, s).hash(state),
            Token::b(b) => b.hash(state),
            Token::c(c) => c.hash(state),
            Token::f(f) => f.to_bits().hash(state),
            Token::i(i) => i.hash(state),
            Token::s(s) => s.hash(state),
            Token::l(s) => s.hash(state),
            Token::u(u) => u.hash(state),
            Token::v(v) => v.hash(state),
            Token::w(w) => w.hash(state),
            Token::d(d) => d.hash(state),
            Token::n() => 0u8.hash(state), // Hash as 0 for consistency
        }
    }
}
impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::a(a, s) => write!(
                f,
                "{}",
                from_utf8(&a[..s.clone()]).unwrap_or("Invalid UTF-8")
            ),
            Token::b(a) => write!(f, "{}", (*a as char).to_string()),
            Token::c(c) => write!(f, "{}", c.to_string()),
            Token::f(n) => write!(f, "{}", n.to_string()),
            Token::i(n) => write!(f, "{}", n.to_string()),
            Token::s(s) => write!(f, "{}", s.to_string()),
            Token::l(s) => write!(f, "{}", s.to_string()),
            Token::u(n) => write!(f, "{}", n.to_string()),
            Token::v(v) => write!(f, "{}", from_utf8(v.as_slice()).unwrap_or("Invalid UTF-8")),
            Token::w(w) => write!(f, "{}", w),
            Token::d(d) => write!(f, "{}", d.iter().collect::<String>()),
            Token::n() => write!(f, ""), // Display as empty string
        }
    }
}
impl Eq for Token {}
impl PartialOrd for Token {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Token {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            // Token::arr matches
            (Token::a(a, s1), Token::a(b, s2)) => s1.cmp(s2).then(a[..*s1].cmp(&b[..*s2])),
            (Token::a(a, s), Token::b(b)) => s.cmp(&1).then(a[0].cmp(b)),
            (Token::a(a, s1), Token::c(b)) => {
                if *s1 == 1 {
                    (a[0] as char).cmp(b)
                } else {
                    Ordering::Greater
                }
            }
            (Token::a(a, s), Token::f(b)) => unsafe {
                std::str::from_utf8_unchecked(&a[..*s]).cmp(&b.to_string())
            },
            (Token::a(a, s), Token::i(b)) => s
                .cmp(&b.to_ne_bytes().len())
                .then(a[..*s].cmp(&b.to_ne_bytes())),
            (Token::a(a, s1), Token::s(b)) => unsafe {
                std::str::from_utf8_unchecked(&a[..*s1]).cmp(b)
            },
            (Token::a(a, s1), Token::l(b)) => unsafe {
                std::str::from_utf8_unchecked(&a[..*s1]).cmp(b)
            },
            (Token::a(a, s), Token::u(b)) => s
                .cmp(&b.to_ne_bytes().len())
                .then(a[..*s].cmp(&b.to_ne_bytes())),
            (Token::a(a, s), Token::v(b)) => a[..*s].cmp(b),

            // Token::b matches
            (Token::b(a), Token::a(b, s)) => 1usize.cmp(s).then(a.cmp(&b[0])),
            (Token::b(a), Token::b(b)) => a.cmp(b),
            (Token::b(a), Token::c(b)) => (*a as char).cmp(b),
            (Token::b(a), Token::f(b)) => a.to_string().cmp(&b.to_string()),
            (Token::b(a), Token::i(b)) => (*a as i128).cmp(b),
            (Token::b(a), Token::s(b)) => 1usize.cmp(&b.len()).then(a.cmp(&b.as_bytes()[0])),
            (Token::b(a), Token::l(b)) => 1usize.cmp(&b.len()).then(a.cmp(&b.as_bytes()[0])),
            (Token::b(a), Token::u(b)) => (*a as u128).cmp(b),
            (Token::b(a), Token::v(b)) => 1usize.cmp(&b.len()).then(a.cmp(&b[0])),

            // Token::ch matches
            (Token::c(a), Token::a(b, s)) => {
                if *s == 1 {
                    a.cmp(&(b[0] as char))
                } else {
                    std::cmp::Ordering::Greater
                }
            }
            (Token::c(a), Token::b(b)) => a.cmp(&(*b as char)),
            (Token::c(a), Token::c(b)) => a.cmp(b),
            (Token::c(a), Token::f(b)) => a.to_string().cmp(&b.to_string()),
            (Token::c(a), Token::i(b)) => a.to_string().cmp(&b.to_string()),
            (Token::c(a), Token::s(b)) => a.to_string().cmp(b),
            (Token::c(a), Token::l(b)) => a.to_string().cmp(&b.to_string()),
            (Token::c(a), Token::u(b)) => a.to_string().cmp(&b.to_string()),
            (Token::c(a), Token::v(b)) => {
                if b.len() == 1 {
                    a.cmp(&(b[0] as char))
                } else {
                    std::cmp::Ordering::Less
                }
            }

            // Token::fl matches
            (Token::f(a), Token::a(b, s)) => unsafe {
                a.to_string()
                    .cmp(&String::from_utf8_unchecked(b[..*s].to_vec()))
            },
            (Token::f(a), Token::b(b)) => a.to_string().cmp(&b.to_string()),
            (Token::f(a), Token::c(b)) => a.total_cmp(&(*b as i8 as f64)),
            (Token::f(a), Token::f(b)) => a.total_cmp(b),
            (Token::f(a), Token::i(b)) => a.to_string().cmp(&b.to_string()),
            (Token::f(a), Token::s(b)) => a.to_string().cmp(b),
            (Token::f(a), Token::l(b)) => a.to_string().cmp(&b.to_string()),
            (Token::f(a), Token::u(b)) => a.to_string().cmp(&b.to_string()),
            (Token::f(a), Token::v(b)) => 8.cmp(&b.len()).then(a.to_ne_bytes()[..8].cmp(&b[..8])),

            // Token::int matches
            (Token::i(a), Token::a(b, s)) => a
                .to_ne_bytes()
                .len()
                .cmp(s)
                .then(a.to_ne_bytes().as_ref().cmp(&b[..*s])),
            (Token::i(a), Token::b(b)) => a.cmp(&(*b as i128)),
            (Token::i(a), Token::c(b)) => a.cmp(&(*b as i128)),
            (Token::i(a), Token::f(b)) => a.cmp(&(*b as i128)),
            (Token::i(a), Token::i(b)) => a.cmp(b),
            (Token::i(a), Token::s(b)) => a.to_string().cmp(&*b),
            (Token::i(a), Token::l(b)) => a.to_string().cmp(&b.to_string()),
            (Token::i(a), Token::u(b)) => (*a as u128).cmp(b),
            (Token::i(a), Token::v(b)) => unsafe {
                a.to_string().cmp(&String::from_utf8_unchecked(b.clone()))
            },
            // Token::s matches
            (Token::s(a), Token::a(b, s)) => unsafe {
                a.cmp(&String::from_utf8_unchecked(b[..*s].to_vec()))
            },
            (Token::s(a), Token::b(b)) => a.len().cmp(&1).then(a.as_bytes()[0].cmp(b)),
            (Token::s(a), Token::c(b)) => a.cmp(&b.to_string()),
            (Token::s(a), Token::f(b)) => a.cmp(&b.to_string()),
            (Token::s(a), Token::i(b)) => a.cmp(&b.to_string()),

            (Token::s(a), Token::s(b)) => a.cmp(b),
            (Token::s(a), Token::l(b)) => a.cmp(&b.to_string()),
            (Token::s(a), Token::u(b)) => a.cmp(&b.to_string()),
            (Token::s(a), Token::v(b)) => a.as_bytes().cmp(b),

            // Token::sl matches
            (Token::l(a), Token::a(b, s)) => a.as_bytes().cmp(&b[..*s]),
            (Token::l(a), Token::b(b)) => a.len().cmp(&1).then(a.as_bytes()[0].cmp(b)),
            (Token::l(a), Token::c(b)) => a.to_string().cmp(&b.to_string()),
            (Token::l(a), Token::f(b)) => a.to_string().cmp(&b.to_string()),
            (Token::l(a), Token::i(b)) => a.to_string().cmp(&b.to_string()),
            (Token::l(a), Token::s(b)) => a.to_string().cmp(b),
            (Token::l(a), Token::l(b)) => a.cmp(b),
            (Token::l(a), Token::u(b)) => a.to_string().cmp(&b.to_string()),
            (Token::l(a), Token::v(b)) => a.as_bytes().cmp(b),

            // Token::uint matches
            (Token::u(a), Token::a(b, s)) => a
                .to_ne_bytes()
                .len()
                .cmp(s)
                .then(a.to_ne_bytes().as_ref().cmp(&b[..*s])),
            (Token::u(a), Token::b(b)) => a.cmp(&(*b as u128)),
            (Token::u(a), Token::c(b)) => a.to_string().cmp(&b.to_string()),
            (Token::u(a), Token::f(b)) => a.to_string().cmp(&b.to_string()),
            (Token::u(a), Token::i(b)) => a.cmp(&(*b as u128)),
            (Token::u(a), Token::s(b)) => a.to_string().cmp(b),
            (Token::u(a), Token::l(b)) => a.to_string().cmp(&b.to_string()),
            (Token::u(a), Token::u(b)) => a.cmp(b),
            (Token::u(a), Token::v(b)) => a
                .to_string()
                .cmp(&String::from_utf8(b.clone()).unwrap_or("Invalid UTF-8".to_string())),

            // Token::vec matches
            (Token::v(a), Token::a(b, s)) => a.cmp(&b[..*s].to_vec()),
            (Token::v(a), Token::b(b)) => a.len().cmp(&1).then(a[0].cmp(b)),
            (Token::v(a), Token::c(b)) => {
                if a.len() == 1 {
                    (a[0] as char).cmp(b)
                } else {
                    std::cmp::Ordering::Greater
                }
            }
            (Token::v(a), Token::f(b)) => String::from_utf8(a.clone())
                .unwrap_or("Invalid UTF-8".to_string())
                .cmp(&b.to_string()),
            (Token::v(a), Token::i(b)) => a
                .len()
                .cmp(&b.to_ne_bytes().len())
                .then(a.cmp(&b.to_ne_bytes().to_vec())),
            (Token::v(a), Token::s(b)) => String::from_utf8(a.clone())
                .unwrap_or("Invalid UTF-8".to_string())
                .cmp(b),
            (Token::v(a), Token::l(b)) => a.cmp(&b.as_bytes().to_vec()),
            (Token::v(a), Token::u(b)) => a
                .len()
                .cmp(&b.to_ne_bytes().len())
                .then(a.cmp(&b.to_ne_bytes().to_vec())),
            (Token::v(a), Token::v(b)) => a.cmp(b),

            // Token::w matches - whitespace tokens
            (Token::w(a), Token::a(b, s)) => unsafe {
                a.cmp(&String::from_utf8_unchecked(b[..*s].to_vec()))
            },
            (Token::w(a), Token::b(b)) => a.cmp(&(*b as char).to_string()),
            (Token::w(a), Token::c(b)) => a.cmp(&b.to_string()),
            (Token::w(a), Token::f(b)) => a.cmp(&b.to_string()),
            (Token::w(a), Token::i(b)) => a.cmp(&b.to_string()),
            (Token::w(a), Token::s(b)) => a.cmp(b),
            (Token::w(a), Token::l(b)) => a.cmp(&b.to_string()),
            (Token::w(a), Token::u(b)) => a.cmp(&b.to_string()),
            (Token::w(a), Token::v(b)) => {
                a.cmp(&String::from_utf8(b.clone()).unwrap_or("Invalid UTF-8".to_string()))
            }
            (Token::w(a), Token::w(b)) => a.cmp(b),

            // Reverse matches for Token::w (all other variants compared to Token::w)
            (Token::a(a, s), Token::w(b)) => unsafe {
                String::from_utf8_unchecked(a[..*s].to_vec()).cmp(b)
            },
            (Token::b(a), Token::w(b)) => (*a as char).to_string().cmp(b),
            (Token::c(a), Token::w(b)) => a.to_string().cmp(b),
            (Token::f(a), Token::w(b)) => a.to_string().cmp(b),
            (Token::i(a), Token::w(b)) => a.to_string().cmp(b),
            (Token::s(a), Token::w(b)) => a.cmp(b),
            (Token::l(a), Token::w(b)) => a.to_string().cmp(b),
            (Token::u(a), Token::w(b)) => a.to_string().cmp(b),
            (Token::v(a), Token::w(b)) => String::from_utf8(a.clone())
                .unwrap_or("Invalid UTF-8".to_string())
                .cmp(b),

            // Token::d matches - delimiter tokens
            (Token::d(a), Token::a(b, s)) => {
                let a_str = a.iter().collect::<String>();
                unsafe { a_str.cmp(&String::from_utf8_unchecked(b[..*s].to_vec())) }
            }
            (Token::d(a), Token::b(b)) => {
                a.iter().collect::<String>().cmp(&(*b as char).to_string())
            }
            (Token::d(a), Token::c(b)) => a.iter().collect::<String>().cmp(&b.to_string()),
            (Token::d(a), Token::f(b)) => a.iter().collect::<String>().cmp(&b.to_string()),
            (Token::d(a), Token::i(b)) => a.iter().collect::<String>().cmp(&b.to_string()),
            (Token::d(a), Token::s(b)) => a.iter().collect::<String>().cmp(b),
            (Token::d(a), Token::l(b)) => a.iter().collect::<String>().cmp(&b.to_string()),
            (Token::d(a), Token::u(b)) => a.iter().collect::<String>().cmp(&b.to_string()),
            (Token::d(a), Token::v(b)) => a
                .iter()
                .collect::<String>()
                .cmp(&String::from_utf8(b.clone()).unwrap_or("Invalid UTF-8".to_string())),
            (Token::d(a), Token::w(b)) => a.iter().collect::<String>().cmp(b),
            (Token::d(a), Token::d(b)) => a.cmp(b),

            // Reverse matches for Token::d (all other variants compared to Token::d)
            (Token::a(a, s), Token::d(b)) => {
                let b_str = b.iter().collect::<String>();
                unsafe { String::from_utf8_unchecked(a[..*s].to_vec()).cmp(&b_str) }
            }
            (Token::b(a), Token::d(b)) => {
                (*a as char).to_string().cmp(&b.iter().collect::<String>())
            }
            (Token::c(a), Token::d(b)) => a.to_string().cmp(&b.iter().collect::<String>()),
            (Token::f(a), Token::d(b)) => a.to_string().cmp(&b.iter().collect::<String>()),
            (Token::i(a), Token::d(b)) => a.to_string().cmp(&b.iter().collect::<String>()),
            (Token::s(a), Token::d(b)) => a.cmp(&b.iter().collect::<String>()),
            (Token::l(a), Token::d(b)) => a.to_string().cmp(&b.iter().collect::<String>()),
            (Token::u(a), Token::d(b)) => a.to_string().cmp(&b.iter().collect::<String>()),
            (Token::v(a), Token::d(b)) => String::from_utf8(a.clone())
                .unwrap_or("Invalid UTF-8".to_string())
                .cmp(&b.iter().collect::<String>()),
            (Token::w(a), Token::d(b)) => a.cmp(&b.iter().collect::<String>()),
            // Token::n patterns - consumed tokens are always equal to each other and less than all others
            (Token::n(), Token::n()) => std::cmp::Ordering::Equal,
            (Token::n(), _) => std::cmp::Ordering::Less,
            (_, Token::n()) => std::cmp::Ordering::Greater,
        }
    }
}

impl Add for Token {
    type Output = Self;
    fn add(self, other: Self) -> Self::Output {
        self.op(other, "+").unwrap()
    }
}
impl Sub for Token {
    type Output = Self;
    fn sub(self, other: Self) -> Self::Output {
        self.op(other, "-").unwrap()
    }
}
impl Mul for Token {
    type Output = Self;
    fn mul(self, other: Self) -> Self::Output {
        self.op(other, "*").unwrap()
    }
}
impl Div for Token {
    type Output = Self;
    fn div(self, other: Self) -> Self::Output {
        self.op(other, "/").unwrap()
    }
}
impl Rem for Token {
    type Output = Self;
    fn rem(self, other: Self) -> Self::Output {
        self.op(other, "%").unwrap()
    }
}
impl BitAnd for Token {
    type Output = Self;
    fn bitand(self, other: Self) -> Self::Output {
        self.op(other, "&").unwrap()
    }
}
impl BitOr for Token {
    type Output = Self;
    fn bitor(self, other: Self) -> Self::Output {
        self.op(other, "|").unwrap()
    }
}
impl BitXor for Token {
    type Output = Self;
    fn bitxor(self, other: Self) -> Self::Output {
        self.op(other, "^").unwrap()
    }
}
impl FromStr for Token {
    type Err = C2RError;
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        Ok(Token::s(s.trim().to_string()))
    }
}
pub fn matches(v: Vec<Token>) -> bool {
    let a = v[0].clone();
    for t in v {
        if a.to_string_token() == t.to_string_token() {
            return true;
        }
    }
    false
}
#[macro_export]
macro_rules! tok {
    // String literals become Token::Str
    ($t:literal) => {
        Token::l(concat!($t))
    };
    // For token identifiers (non-literals)
    ($t:ident) => {
        Token::l($t)
    };
    ($t:tt) => {
        Token::l($t)
    };
    ($t:literal,$k:literal) => {
        Token::$k(concat!($t))
    };
    ($t:tt,$k:ident) => {
        Token::$k($t)
    };
    ($t:tt:$k:literal) => {
        Token::$k($t)
    };
}
impl Token {
    /// Check if the token is whitespace
    pub fn is_whitespace(&self) -> bool {
        match self {
            Token::w(_) => true, // Direct whitespace token variant
            Token::s(s) => s.chars().all(|c| c.is_whitespace()), // String containing only whitespace
            Token::c(c) => c.is_whitespace(),                    // Single whitespace character
            Token::b(b) => (*b as char).is_whitespace(), // Byte representing whitespace character
            _ => false,
        }
    }

    pub fn is_numeric(&self) -> bool {
        match self {
            Token::i(_) => true,
            Token::u(_) => true,
            Token::f(_) => true,
            Token::b(_) => true,
            Token::c(_) => true,
            Token::s(s) => {
                s.parse::<i128>().is_ok() || s.parse::<u128>().is_ok() || s.parse::<f64>().is_ok()
            }
            Token::l(s) => {
                s.parse::<i128>().is_ok() || s.parse::<u128>().is_ok() || s.parse::<f64>().is_ok()
            }
            _ => false,
        }
    }
    pub fn is_int(&self) -> bool {
        match self {
            Token::i(_) => true,
            Token::u(_) => true,
            Token::s(s) => s.parse::<i128>().is_ok() || s.parse::<u128>().is_ok(),
            Token::l(s) => s.parse::<i128>().is_ok() || s.parse::<u128>().is_ok(),
            _ => false,
        }
    }
    pub fn is_float(&self) -> bool {
        match self {
            Token::f(_) => true,
            Token::s(s) => s.parse::<f64>().is_ok(),
            Token::l(s) => s.parse::<f64>().is_ok(),
            _ => false,
        }
    }
    pub fn len(&self) -> usize {
        match self {
            Token::a(_, s) => s.clone(),
            Token::b(_) => 1,
            Token::c(c) => c.len_utf8(),
            Token::f(n) => n.to_string().len(),
            Token::i(n) => n.to_string().len(),
            Token::s(s) => s.len(),
            Token::l(s) => s.len(),
            Token::u(n) => n.to_string().len(),
            Token::v(v) => v.len(),
            Token::w(w) => w.len(),
            Token::d(d) => d.len(),
            Token::n() => 0, // Length is 0 for consumed tokens
        }
    }
    pub fn to_string_token(&self) -> Token {
        match self.clone() {
            Token::a(a, s) => Tokenizer::from_utf8(&a[..s]),
            Token::b(b) => Token::s((b as char).to_string()),
            Token::c(c) => Token::s(c.to_string()),
            Token::f(f) => Token::s(f.to_string()),
            Token::i(i) => Token::s(i.to_string()),
            Token::s(s) => Token::s(s.to_string()),
            Token::l(s) => Token::s(s.to_string()),
            Token::u(u) => Token::s(u.to_string()),
            Token::v(v) => Token::s(String::from_utf8(v).unwrap()),
            Token::w(w) => Token::s(w), // Convert whitespace token to string token
            Token::d(d) => Token::s(d.iter().collect::<String>()), // Convert delimiter token to string token
            Token::n() => Token::s(String::new()),                 // Convert to empty string
        }
    }
    pub fn matches(&self, others: Vec<Token>) -> Vec<Option<Token>> {
        others.into_iter()
            .map(|other| if *self == other { Some(other) } else { None })
            .collect()
    }

    pub fn match_any(&self, others: &[Token]) -> bool {
        others.iter().any(|other| *self == *other)
    }
    pub fn op(self, other: Token, op: &str) -> Option<Token> {
        match (self, other, op) {
            (Token::i(a), Token::i(b), op) => match op {
                "+" => Some(Token::i(a + b)),
                "-" => Some(Token::i(a - b)),
                "*" => Some(Token::i(a * b)),
                "/" => Some(Token::i(a / b)),
                "%" => Some(Token::i(a % b)),
                "^" => Some(Token::i(a ^ b)),
                "&" => Some(Token::i(a & b)),
                "|" => Some(Token::i(a | b)),
                "<<" => Some(Token::i(a << b)),
                ">>" => Some(Token::i(a >> b)),
                _ => Some(Token::i(0)),
            },
            (Token::u(a), Token::u(b), op) => match op {
                "+" => Some(Token::u(a.wrapping_add(b))),
                "-" => Some(Token::u(a.wrapping_sub(b))),
                "*" => Some(Token::u(a.wrapping_mul(b))),
                "/" => {
                    if b != 0 {
                        Some(Token::u(a / b))
                    } else {
                        None
                    }
                }
                "%" => {
                    if b != 0 {
                        Some(Token::u(a % b))
                    } else {
                        None
                    }
                }
                "^" => Some(Token::u(a ^ b)),
                "&" => Some(Token::u(a & b)),
                "|" => Some(Token::u(a | b)),
                "<<" => Some(Token::u(a << b)),
                ">>" => Some(Token::u(a >> b)),
                _ => Some(Token::u(0)),
            },
            (Token::f(a), Token::f(b), op) => match op {
                "+" => Some(Token::f(a + b)),
                "-" => Some(Token::f(a - b)),
                "*" => Some(Token::f(a * b)),
                "/" => Some(Token::f(a / b)),
                "%" => Some(Token::f(a % b)),
                _ => Some(Token::f(0.0)),
            },
            (Token::b(a), Token::b(b), op) => match op {
                "+" => Some(Token::b(a.wrapping_add(b))),
                "-" => Some(Token::b(a.wrapping_sub(b))),
                "*" => Some(Token::b(a.wrapping_mul(b))),
                "/" => {
                    if b != 0 {
                        Some(Token::b(a / b))
                    } else {
                        None
                    }
                }
                "%" => {
                    if b != 0 {
                        Some(Token::b(a % b))
                    } else {
                        None
                    }
                }
                "^" => Some(Token::b(a ^ b)),
                "&" => Some(Token::b(a & b)),
                "|" => Some(Token::b(a | b)),
                "<<" => Some(Token::b(a << b)),
                ">>" => Some(Token::b(a >> b)),
                _ => Some(Token::b(0)),
            },
            (Token::c(a), Token::c(b), "+") => Some(Token::s(a.to_string() + &b.to_string())),
            (Token::s(a), Token::s(b), "+") => Some(Token::s(a.to_string() + &b)),
            (Token::s(a), Token::s(b), op) => match (a.parse::<f64>(), b.parse::<f64>()) {
                (Ok(a), Ok(b)) => match op {
                    "+" => Some(Token::f(a + b)),
                    "-" => Some(Token::f(a - b)),
                    "*" => Some(Token::f(a * b)),
                    "/" => Some(Token::f(a / b)),
                    "%" => Some(Token::f(a % b)),
                    _ => Some(Token::f(0.0)),
                },
                _ => match (a.parse::<i128>(), b.parse::<i128>()) {
                    (Ok(a), Ok(b)) => match op {
                        "+" => Some(Token::i(a + b)),
                        "-" => Some(Token::i(a - b)),
                        "*" => Some(Token::i(a * b)),
                        "/" => Some(Token::i(a / b)),
                        "%" => Some(Token::i(a % b)),
                        "^" => Some(Token::i(a ^ b)),
                        "&" => Some(Token::i(a & b)),
                        "|" => Some(Token::i(a | b)),
                        "<<" => Some(Token::i(a << b)),
                        ">>" => Some(Token::i(a >> b)),
                        _ => Some(Token::i(0)),
                    },
                    _ => match (a.parse::<u128>(), b.parse::<u128>()) {
                        (Ok(a), Ok(b)) => match op {
                            "+" => Some(Token::u(a.wrapping_add(b))),
                            "-" => Some(Token::u(a.wrapping_sub(b))),
                            "*" => Some(Token::u(a.wrapping_mul(b))),
                            "/" => {
                                if b != 0 {
                                    Some(Token::u(a / b))
                                } else {
                                    None
                                }
                            }
                            "%" => {
                                if b != 0 {
                                    Some(Token::u(a % b))
                                } else {
                                    None
                                }
                            }
                            "^" => Some(Token::u(a ^ b)),
                            "&" => Some(Token::u(a & b)),
                            "|" => Some(Token::u(a | b)),
                            "<<" => Some(Token::u(a << b)),
                            ">>" => Some(Token::u(a >> b)),
                            _ => Some(Token::u(0)),
                        },
                        _ => None,
                    },
                },
            },
            (Token::v(a), Token::v(b), "+") => {
                let mut result = a.clone();
                result.extend_from_slice(&b);
                Some(Token::v(result))
            }
            (Token::l(a), Token::l(b), "+") => Some(Token::s(a.to_string() + b)),
            (Token::a(a, s1), Token::a(b, s2), "+") if s1 + s2 <= TOKEN_MAX => {
                let mut result = [0u8; TOKEN_MAX];
                result[..s1].copy_from_slice(&a[..s1]);
                result[s1..s1 + s2].copy_from_slice(&b[..s2]);
                Some(Token::a(result, s1 + s2))
            }
            _ => None,
        }
    }
    pub fn tokens_to_string(tokens: &[Token], start_idx: usize, end_idx: usize) -> Option<String> {
        if tokens.is_empty() || start_idx > end_idx {
            return None;
        }
        Some(
            tokens[start_idx..end_idx]
                .iter()
                .map(|t| t.to_string())
                .collect::<String>(),
        )
    }
    pub fn base10_zeros(&self) -> usize {
        let mut count = 0;
        if let Token::i(n) = self {
            let mut value = *n;
            while value % 10 == 0 && value != 0 {
                count += 1;
                value /= 10;
            }
        }
        count
    }
}
impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            // Token::arr matches
            (Token::a(a, s1), Token::a(b, s2)) => s1 == s2 && a[..*s1] == b[..*s2],
            (Token::a(a, s), Token::b(b)) => *s == 1 && a[0] == *b,
            (Token::a(a, s), Token::c(b)) => *s == 1 && a[0] as char == *b,
            (Token::a(a, s), Token::f(b)) => &a[..*s] == b.to_string().as_bytes(),
            (Token::a(a, s), Token::i(b)) => &a[..*s] == b.to_string().as_bytes(),
            (Token::a(a, s), Token::u(b)) => &a[..*s] == b.to_string().as_bytes(),
            (Token::a(a, s), Token::s(b)) => *s == b.len() && &a[..*s] == b.as_bytes(),
            (Token::a(a, s), Token::l(b)) => *s == b.len() && &a[..*s] == b.as_bytes(),
            (Token::a(a, s), Token::v(b)) => *s == b.len() && a[..*s] == b[..],

            // Token::b matches
            (Token::b(a), Token::a(b, s)) => *s == 1 && *a == b[0],
            (Token::b(a), Token::b(b)) => a == b,
            (Token::b(a), Token::c(b)) => *a == *b as u8,
            (Token::b(a), Token::f(b)) => a.to_string() == b.to_string(),
            (Token::b(a), Token::i(b)) => (*a as i128) == *b,
            (Token::b(a), Token::u(b)) => (*a as u128) == *b,
            (Token::b(a), Token::s(b)) => b.len() == 1 && *a == b.as_bytes()[0],
            (Token::b(a), Token::l(b)) => b.len() == 1 && *a == b.as_bytes()[0],
            (Token::b(a), Token::v(b)) => b.len() == 1 && *a == b[0],

            // Token::ch matches
            (Token::c(a), Token::a(b, s)) => *s == 1 && *a == (b[0] as char),
            (Token::c(a), Token::b(b)) => *a as u8 == *b,
            (Token::c(a), Token::c(b)) => a == b,
            (Token::c(a), Token::f(b)) => a.to_string() == b.to_string(),
            (Token::c(a), Token::i(b)) => a.to_string() == b.to_string(),
            (Token::c(a), Token::u(b)) => a.to_string() == b.to_string(),
            (Token::c(a), Token::s(b)) => a.to_string() == b.to_string(),
            (Token::c(a), Token::l(b)) => a.to_string() == b.to_string(),
            (Token::c(a), Token::v(b)) => b.len() == 1 && *a == (b[0] as char),

            // Token::fl matches
            (Token::f(a), Token::a(b, s)) => a.to_string().as_bytes() == &b[..*s],
            (Token::f(a), Token::b(b)) => a.to_string() == b.to_string(),
            (Token::f(a), Token::c(b)) => a.to_string() == b.to_string(),
            (Token::f(a), Token::f(b)) => a == b,
            (Token::f(a), Token::i(b)) => a.to_string() == b.to_string(),
            (Token::f(a), Token::u(b)) => a.to_string() == b.to_string(),
            (Token::f(a), Token::s(b)) => a.to_string() == *b,
            (Token::f(a), Token::l(b)) => a.to_string() == *b,
            (Token::f(a), Token::v(b)) => a.to_string().as_bytes() == b,

            // Token::int matches
            (Token::i(a), Token::a(b, s)) => a.to_string().as_bytes() == &b[..*s],
            (Token::i(a), Token::b(b)) => *a == (*b as i128),
            (Token::i(a), Token::c(b)) => a.to_string() == b.to_string(),
            (Token::i(a), Token::f(b)) => a.to_string() == b.to_string(),
            (Token::i(a), Token::i(b)) => a == b,
            (Token::i(a), Token::u(b)) => *a >= 0 && (*a as u128) == *b,
            (Token::i(a), Token::s(b)) => a.to_string() == *b,
            (Token::i(a), Token::l(b)) => a.to_string() == *b,
            (Token::i(a), Token::v(b)) => a.to_string().as_bytes() == b,

            // Token::uint matches
            (Token::u(a), Token::a(b, s)) => a.to_string().as_bytes() == &b[..*s],
            (Token::u(a), Token::b(b)) => *a == (*b as u128),
            (Token::u(a), Token::c(b)) => a.to_string() == b.to_string(),
            (Token::u(a), Token::f(b)) => a.to_string() == b.to_string(),
            (Token::u(a), Token::i(b)) => *b >= 0 && *a == (*b as u128),
            (Token::u(a), Token::u(b)) => a == b,
            (Token::u(a), Token::s(b)) => a.to_string() == *b,
            (Token::u(a), Token::l(b)) => a.to_string() == *b,
            (Token::u(a), Token::v(b)) => a.to_string().as_bytes() == b,

            // Token::s matches
            (Token::s(a), Token::a(b, s)) => a.as_bytes() == &b[..*s],
            (Token::s(a), Token::b(b)) => a.len() == 1 && a.as_bytes()[0] == *b,
            (Token::s(a), Token::c(b)) => *a == b.to_string(),
            (Token::s(a), Token::f(b)) => *a == b.to_string(),
            (Token::s(a), Token::i(b)) => *a == b.to_string(),
            (Token::s(a), Token::u(b)) => *a == b.to_string(),
            (Token::s(a), Token::s(b)) => a == b,
            (Token::s(a), Token::l(b)) => a == b,
            (Token::s(a), Token::v(b)) => a.as_bytes() == b,

            // Token::sl matches
            (Token::l(a), Token::a(b, s)) => a.len() == *s && a.as_bytes() == &b[..*s],
            (Token::l(a), Token::b(b)) => a.len() == 1 && a.as_bytes()[0] == *b,
            (Token::l(a), Token::c(b)) => *a == b.to_string(),
            (Token::l(a), Token::f(b)) => *a == b.to_string(),
            (Token::l(a), Token::i(b)) => *a == b.to_string(),
            (Token::l(a), Token::u(b)) => *a == b.to_string(),
            (Token::l(a), Token::s(b)) => *a == *b,
            (Token::l(a), Token::l(b)) => a == b,
            (Token::l(a), Token::v(b)) => a.as_bytes() == b,

            // Token::vec matches
            (Token::v(a), Token::a(b, s)) => a.len() == *s && a == &b[..*s],
            (Token::v(a), Token::b(b)) => a.len() == 1 && a[0] == *b,
            (Token::v(a), Token::c(b)) => a.len() == 1 && (a[0] as char) == *b,
            (Token::v(a), Token::f(b)) => a == b.to_string().as_bytes(),
            (Token::v(a), Token::i(b)) => a == b.to_string().as_bytes(),
            (Token::v(a), Token::u(b)) => a == b.to_string().as_bytes(),
            (Token::v(a), Token::s(b)) => a == b.as_bytes(),
            (Token::v(a), Token::l(b)) => a == b.as_bytes(),
            (Token::v(a), Token::v(b)) => a == b,

            // Token::w matches - whitespace tokens
            (Token::w(a), Token::a(b, s)) => a.len() == *s && a.as_bytes() == &b[..*s],
            (Token::w(a), Token::b(b)) => a.len() == 1 && a.chars().next().unwrap() as u8 == *b,
            (Token::w(a), Token::c(b)) => a.len() == 1 && a.chars().next().unwrap() == *b,
            (Token::w(a), Token::f(b)) => a == &b.to_string(),
            (Token::w(a), Token::i(b)) => a == &b.to_string(),
            (Token::w(a), Token::u(b)) => a == &b.to_string(),
            (Token::w(a), Token::s(b)) => a == b,
            (Token::w(a), Token::l(b)) => a == b,
            (Token::w(a), Token::v(b)) => a.as_bytes() == b,
            (Token::w(a), Token::w(b)) => a == b,

            // Reverse matches for Token::w (all other variants compared to Token::w)
            (Token::a(a, s), Token::w(b)) => *s == b.len() && &a[..*s] == b.as_bytes(),
            (Token::b(a), Token::w(b)) => b.len() == 1 && *a == b.chars().next().unwrap() as u8,
            (Token::c(a), Token::w(b)) => b.len() == 1 && *a == b.chars().next().unwrap(),
            (Token::f(a), Token::w(b)) => a.to_string() == *b,
            (Token::i(a), Token::w(b)) => a.to_string() == *b,
            (Token::u(a), Token::w(b)) => a.to_string() == *b,
            (Token::s(a), Token::w(b)) => a == b,
            (Token::l(a), Token::w(b)) => *a == b,
            (Token::v(a), Token::w(b)) => a == b.as_bytes(),

            // Token::d matches - delimiter tokens
            (Token::d(a), Token::a(b, s)) => {
                let a_str = a.iter().collect::<String>();
                a_str.len() == *s && a_str.as_bytes() == &b[..*s]
            }
            (Token::d(a), Token::b(b)) => a.len() == 1 && a[0] as u8 == *b,
            (Token::d(a), Token::c(b)) => a.len() == 1 && a[0] == *b,
            (Token::d(a), Token::f(b)) => a.iter().collect::<String>() == b.to_string(),
            (Token::d(a), Token::i(b)) => a.iter().collect::<String>() == b.to_string(),
            (Token::d(a), Token::u(b)) => a.iter().collect::<String>() == b.to_string(),
            (Token::d(a), Token::s(b)) => a.iter().collect::<String>() == *b,
            (Token::d(a), Token::l(b)) => a.iter().collect::<String>() == *b,
            (Token::d(a), Token::v(b)) => a.iter().collect::<String>().as_bytes() == b,
            (Token::d(a), Token::w(b)) => a.iter().collect::<String>() == *b,
            (Token::d(a), Token::d(b)) => a == b,

            // Reverse matches for Token::d (all other variants compared to Token::d)
            (Token::a(a, s), Token::d(b)) => {
                let b_str = b.iter().collect::<String>();
                *s == b_str.len() && &a[..*s] == b_str.as_bytes()
            }
            (Token::b(a), Token::d(b)) => b.len() == 1 && *a == b[0] as u8,
            (Token::c(a), Token::d(b)) => b.len() == 1 && *a == b[0],
            (Token::f(a), Token::d(b)) => a.to_string() == b.iter().collect::<String>(),
            (Token::i(a), Token::d(b)) => a.to_string() == b.iter().collect::<String>(),
            (Token::u(a), Token::d(b)) => a.to_string() == b.iter().collect::<String>(),
            (Token::s(a), Token::d(b)) => *a == b.iter().collect::<String>(),
            (Token::l(a), Token::d(b)) => *a == b.iter().collect::<String>(),
            (Token::v(a), Token::d(b)) => a == b.iter().collect::<String>().as_bytes(),
            (Token::w(a), Token::d(b)) => *a == b.iter().collect::<String>(),
            // Token::n patterns - consumed tokens are always equal to each other and not equal to others
            (Token::n(), Token::n()) => true,
            (Token::n(), _) => false,
            (_, Token::n()) => false,
        }
    }
}

impl Tokenizer {
    pub fn new() -> Self {
        Tokenizer {
            content: Vec::new(),
            cursor: 0usize,
            slots: Vec::new(),
            active_slot: 0usize,
        }
    }
    pub fn add_slot(&mut self, slot: TokenSlot) {
        self.slots.push(slot);
    }
    pub fn del_slot(&mut self, index: usize) {
        self.slots.remove(index);
    }
    pub fn slots(&self) -> &Vec<TokenSlot> {
        &self.slots
    }

    pub fn slot_count(&self) -> usize {
        self.slots.len()
    }
    pub fn current_slot(&self) -> &TokenSlot {
        &self.slots[self.active_slot]
    }
    pub fn active_slot(&self) -> usize {
        self.active_slot
    }
    pub fn current_tokens(&self) -> Vec<Token> {
        self.slots[self.active_slot].tokens()
    }
    pub fn insert_tokens(&mut self, tokens: Vec<Token>) {
        if self.active_slot >= self.slots.len() {
            self.add_slot(TokenSlot::new(tokens.clone(), 0, Id::get(format!("token_slot_{}", self.slot_count()).as_str())));
        }
        self.slots[self.active_slot].append(tokens.clone());
    }
    pub fn insert_token(&mut self, token: Token) {
        if self.active_slot >= self.slots.len() {
            self.add_slot(TokenSlot::new(Vec::new(), 0, Id::get(format!("token_slot_{}", self.slot_count()).as_str())));
        }
        self.slots[self.active_slot].push(token.clone());
    }
    pub fn get_tokens(&self, slot: usize, range: Range<usize>) -> Vec<Token> {
        self.slots[slot].get_range(range)
    }
    
    /// Flush current slot to TokenBox for multi-threaded processing
    pub fn flush_current_slot_to_tokenbox(&mut self, tokenbox: &TokenBox) -> Result<usize> {
        if self.active_slot < self.slots.len() {
            self.slots[self.active_slot].flush_to_tokenbox(tokenbox)
        } else {
            Ok(0)
        }
    }
    
    /// Flush all slots to TokenBox
    pub fn flush_all_slots_to_tokenbox(&mut self, tokenbox: &TokenBox) -> Result<usize> {
        let mut total_flushed = 0;
        for slot in &self.slots {
            match slot.flush_to_tokenbox(tokenbox) {
                Ok(count) => total_flushed += count,
                Err(e) => {
                    warn!("Failed to flush slot to TokenBox: {}", e);
                }
            }
        }
        Ok(total_flushed)
    }
    pub fn tokenize(&mut self, content: Vec<u8>) -> Result<Vec<Token>> {
        self.content = content;
        self.cursor = 0;
        
        if self.content.is_empty() {
            return Ok(Vec::new());
        }

        const TOKEN_LIMIT: usize = 100_000;
        
        let mut tokens = Vec::new();
        while self.cursor < self.content.len() {
            if let Some(token) = self.next_token() {
                if tokens.len() >= TOKEN_LIMIT {
                    return Err(C2RError::new(crate::Kind::Logic, Reason::Other("Token limit exceeded"), None));
                }
                println!("Token: {}", token);
                tokens.push(token.clone());
            } else {
                break;
            }
        }

        self.add_slot(TokenSlot::new(tokens.clone(), 0, Id::get(&format!("token_slot_{}", self.slots.len()))));
        self.active_slot = self.slots.len() - 1;
        self.insert_tokens(tokens);
        Ok(self.current_tokens().to_vec())
    }
    pub fn byte_at(&self, index: usize) -> u8 {
        self.content[index]
    }
    pub fn cursor_byte(&self) -> u8 {
        self.content[self.cursor]
    }
    pub fn chqr_at(&self, index: usize) -> char {
        self.content[index] as char
    }
    pub fn cursor_char(&self) -> char {
        self.content[self.cursor] as char
    }
    pub fn next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();

        if self.cursor >= self.content.len() {
            return None;
        }

        // Check for comments and skip them
        if self.cursor + 1 < self.content.len() {
            if self.content[self.cursor] == b'/' && self.content[self.cursor + 1] == b'/' {
                self.skip_line_comment();
                return self.next_token();
            } else if self.content[self.cursor] == b'/' && self.content[self.cursor + 1] == b'*' {
                self.skip_block_comment();
                return self.next_token();
            }
        }

        // Check for line continuation in preprocessor directives
        if self.handle_line_continuation() {
            return self.next_token();
        }

        let b = self.cursor_byte();
        let mut _is_str = false;
        let start = self.cursor;
        let mut end = self.cursor;

        if b.is_ascii_alphabetic() || b == b'_' {
            while self.cursor < self.content.len()
                && (self.cursor_byte().is_ascii_alphanumeric()
                    || self.cursor_byte() == b'_'
                    || self.cursor_byte() == b'.')
            {
                self.cursor += 1;
            }
            end = self.cursor;
            if end - start > 1 {
                _is_str = true;
            }
        } else if b.is_ascii_digit()
            || (b == b'.' && self.peek().map_or(false, |next| next.is_ascii_digit()))
        {
            // Handle numeric literals including those starting with decimal point
            let mut _has_decimal: bool = b == b'.';

            while self.cursor < self.content.len() {
                let current: u8 = self.cursor_byte();
                if current.is_ascii_digit() {
                    self.cursor += 1;
                } else if current == b'.' && !_has_decimal {
                    _has_decimal = true;
                    self.cursor += 1;
                } else if (current == b'e' || current == b'E')
                    && self.cursor + 1 < self.content.len()
                {
                    // Handle scientific notation
                    let next: u8 = self.content[self.cursor + 1];
                    if next.is_ascii_digit() || next == b'+' || next == b'-' {
                        self.cursor += 1; // Skip 'e' or 'E'
                        if next == b'+' || next == b'-' {
                            self.cursor += 1; // Skip sign if present
                        }
                        // Continue with digits after exponent
                        while self.cursor < self.content.len()
                            && self.cursor_byte().is_ascii_digit()
                        {
                            self.cursor += 1;
                        }
                    }
                    break;
                } else {
                    break;
                }
            }
            end = self.cursor;
        }

        if start != end {
            // Use transmute directly for performance - content should be valid ASCII/UTF-8 from C source
            let content = unsafe { transmute::<&[u8], &str>(&self.content[start..end]) };

            if _is_str || content.contains(' ') {
                return Some(Token::s(content.trim().to_string()));
            }

            // Handle numeric literals - create proper Token::i() or Token::f() variants using tok! macro
            if b.is_ascii_digit()
                || (b == b'.'
                    && self
                        .content
                        .get(start + 1)
                        .map_or(false, |&next| next.is_ascii_digit()))
            {
                if content.contains('.') || content.contains('e') || content.contains('E') {
                    // Parse as float
                    if let Ok(float_val) = content.parse::<f64>() {
                        return Some(tok!(float_val, f));
                    }
                } else {
                    // Parse as integer
                    if let Ok(int_val) = content.parse::<i128>() {
                        return Some(tok!(int_val, i));
                    }
                }
                // If parsing fails, fall back to string token
                return Some(Token::s(content.to_string()));
            }
        }

        match b {
            b'(' => {
                self.cursor += 1;
                Some(tok!('('))
            }
            b')' => {
                self.cursor += 1;
                Some(tok!(')'))
            }
            b'{' => {
                self.cursor += 1;
                Some(tok!('{'))
            }
            b'}' => {
                self.cursor += 1;
                Some(tok!('}'))
            }
            b'[' => {
                self.cursor += 1;
                Some(tok!('['))
            }
            b']' => {
                self.cursor += 1;
                Some(tok!(']'))
            }
            b';' => {
                self.cursor += 1;
                Some(tok!(';'))
            }
            b',' => {
                self.cursor += 1;
                Some(tok!(','))
            }
            b'+' => {
                if self.peek() == Some(b'+') {
                    self.cursor += 2;
                    Some(tok!("++"))
                } else if self.peek() == Some(b'=') {
                    self.cursor += 2;
                    Some(tok!("+="))
                } else {
                    self.cursor += 1;
                    Some(tok!('+'))
                }
            }
            b'-' => {
                if self.peek() == Some(b'-') {
                    self.cursor += 2;
                    Some(tok!("--"))
                } else if self.peek() == Some(b'=') {
                    self.cursor += 2;
                    Some(tok!("-="))
                } else if self.peek() == Some(b'>') {
                    self.cursor += 2;
                    Some(tok!("->"))
                } else {
                    self.cursor += 1;
                    Some(tok!('-'))
                }
            }
            b'/' => {
                if self.peek() == Some(b'=') {
                    self.cursor += 2;
                    Some(tok!("/="))
                } else {
                    self.cursor += 1;
                    Some(tok!('/'))
                }
            }
            b'%' => {
                if self.peek() == Some(b'=') {
                    self.cursor += 2;
                    Some(tok!("%="))
                } else {
                    self.cursor += 1;
                    Some(tok!('%'))
                }
            }
            b'*' => {
                if self.peek() == Some(b'=') {
                    self.cursor += 2;
                    Some(tok!("*="))
                } else {
                    self.cursor += 1;
                    Some(tok!('*'))
                }
            }
            b'&' => {
                if self.peek() == Some(b'&') {
                    self.cursor += 2;
                    Some(tok!("&&"))
                } else if self.peek() == Some(b'=') {
                    self.cursor += 2;
                    Some(tok!("&="))
                } else {
                    self.cursor += 1;
                    Some(tok!('&'))
                }
            }
            b'|' => {
                if self.peek() == Some(b'|') {
                    self.cursor += 2;
                    Some(tok!("||"))
                } else if self.peek() == Some(b'=') {
                    self.cursor += 2;
                    Some(tok!("|="))
                } else {
                    self.cursor += 1;
                    Some(tok!('|'))
                }
            }
            b'<' => {
                if self.peek() == Some(b'<') {
                    if self.peek_at(2) == Some(b'=') {
                        self.cursor += 3;
                        Some(tok!("<<="))
                    } else {
                        self.cursor += 2;
                        Some(tok!("<<"))
                    }
                } else if self.peek() == Some(b'=') {
                    self.cursor += 2;
                    Some(tok!("<="))
                } else {
                    self.cursor += 1;
                    Some(tok!('<'))
                }
            }
            b'>' => {
                if self.peek() == Some(b'>') {
                    if self.peek_at(2) == Some(b'=') {
                        self.cursor += 3;
                        Some(tok!(">>="))
                    } else {
                        self.cursor += 2;
                        Some(tok!(">>"))
                    }
                } else if self.peek() == Some(b'=') {
                    self.cursor += 2;
                    Some(tok!(">="))
                } else {
                    self.cursor += 1;
                    Some(tok!('>'))
                }
            }
            b'!' => {
                if self.peek() == Some(b'=') {
                    self.cursor += 2;
                    Some(tok!("!="))
                } else {
                    self.cursor += 1;
                    Some(tok!('!'))
                }
            }
            b'^' => {
                if self.peek() == Some(b'=') {
                    self.cursor += 2;
                    Some(tok!("^="))
                } else {
                    self.cursor += 1;
                    Some(tok!('^'))
                }
            }
            b'~' => {
                self.cursor += 1;
                Some(tok!('~'))
            }
            b'?' => {
                self.cursor += 1;
                Some(tok!('?'))
            }
            b':' => {
                if self.peek() == Some(b':') {
                    self.cursor += 2;
                    Some(tok!("::"))
                } else {
                    self.cursor += 1;
                    Some(tok!(':'))
                }
            }
            b'#' => {
                if self.peek() == Some(b'#') {
                    self.cursor += 2;
                    Some(tok!("##"))
                } else {
                    self.cursor += 1;
                    Some(tok!('#'))
                }
            }
            b'"' => self.read_string_literal(),
            b'\'' => self.read_char_literal(),
            b'.' => {
                if self.peek() == Some(b'.') && self.peek_at(2) == Some(b'.') {
                    self.cursor += 3;
                    Some(tok!("..."))
                } else {
                    self.cursor += 1;
                    Some(tok!('.'))
                }
            }
            b'\\' => {
                self.cursor += 1;
                Some(tok!('\\'))
            }
            b'_' => {
                self.cursor += 1;
                Some(tok!('_'))
            }
            b'=' => {
                if self.peek() == Some(b'=') {
                    self.cursor += 2;
                    Some(tok!("=="))
                } else {
                    self.cursor += 1;
                    Some(tok!('='))
                }
            }
            _ => {
                self.cursor += 1;
                Some(Token::c(b as char))
            }
        }
    }

    /// Peek at the next token without advancing the cursor
    fn peek_next_token(&mut self) -> Option<Token> {
        let current_cursor = self.cursor;
        let token = self.next_token();
        self.cursor = current_cursor;
        token
    }

    fn read_string_literal(&mut self) -> Option<Token> {
        // Read a string literal
        let mut literal = String::new();
        self.cursor += 1; // skip opening quote

        while self.cursor < self.content.len() {
            let b: u8 = self.content[self.cursor];
            if b == b'"' {
                self.cursor += 1;
                return Some(Token::s(literal));
            } else if b == b'\\' && self.cursor + 1 < self.content.len() {
                // Handle escape sequences
                self.cursor += 1;
                let escape_char = self.content[self.cursor] as char;
                literal.push('\\');
                literal.push(escape_char);
            } else {
                literal.push(b as char);
            }
            self.cursor += 1;
        }

        None // Unterminated string literal
    }

    fn read_char_literal(&mut self) -> Option<Token> {
        // Read a char literal
        self.cursor += 1; // skip opening quote

        if self.cursor >= self.content.len() {
            return None; // Unterminated char literal
        }

        let mut c: char = self.cursor_char();
        let mut _escape: bool = false;

        // Handle escape sequences
        if c == '\\' && self.cursor + 1 < self.content.len() {
            self.cursor += 1;
            c = self.cursor_char();
            _escape = true;
        }

        self.cursor += 1;

        // Check for closing quote
        if self.cursor < self.content.len() && self.content[self.cursor] == b'\'' {
            self.cursor += 1;
            return Some(Token::c(c));
        }

        None // Malformed char literal
    }
    pub fn from_byte(b: u8) -> Token {
        match b {
            0 => tok!("\0"),
            1 => tok!("\x01"),
            2 => tok!("\x02"),
            3 => tok!("\x03"),
            4 => tok!("\x04"),
            5 => tok!("\x05"),
            6 => tok!("\x06"),
            7 => tok!("\x07"),
            8 => tok!("\x08"),
            9 => tok!("\t"),
            10 => tok!("\n"),
            11 => tok!("\x0B"),
            12 => tok!("\x0C"),
            13 => tok!("\r"),
            14 => tok!("\x0E"),
            15 => tok!("\x0F"),
            16 => tok!("\x10"),
            17 => tok!("\x11"),
            18 => tok!("\x12"),
            19 => tok!("\x13"),
            20 => tok!("\x14"),
            21 => tok!("\x15"),
            22 => tok!("\x16"),
            23 => tok!("\x17"),
            24 => tok!("\x18"),
            25 => tok!("\x19"),
            26 => tok!("\x1A"),
            27 => tok!("\x1B"),
            28 => tok!("\x1C"),
            29 => tok!("\x1D"),
            30 => tok!("\x1E"),
            31 => tok!("\x1F"),
            32 => tok!(" "),
            33 => tok!("!"),
            34 => tok!("\""),
            35 => tok!("#"),
            36 => tok!("$"),
            37 => tok!("%"),
            38 => tok!("&"),
            39 => tok!("'"),
            40 => tok!("("),
            41 => tok!(")"),
            42 => tok!("*"),
            43 => tok!("+"),
            44 => tok!(","),
            45 => tok!("-"),
            46 => tok!("."),
            47 => tok!("/"),
            48 => tok!("0"),
            49 => tok!("1"),
            50 => tok!("2"),
            51 => tok!("3"),
            52 => tok!("4"),
            53 => tok!("5"),
            54 => tok!("6"),
            55 => tok!("7"),
            56 => tok!("8"),
            57 => tok!("9"),
            58 => tok!(":"),
            59 => tok!(";"),
            60 => tok!("<"),
            61 => tok!("="),
            62 => tok!(">"),
            63 => tok!("?"),
            64 => tok!("@"),
            65 => tok!("A"),
            66 => tok!("B"),
            67 => tok!("C"),
            68 => tok!("D"),
            69 => tok!("E"),
            70 => tok!("F"),
            71 => tok!("G"),
            72 => tok!("H"),
            73 => tok!("I"),
            74 => tok!("J"),
            75 => tok!("K"),
            76 => tok!("L"),
            77 => tok!("M"),
            78 => tok!("N"),
            79 => tok!("O"),
            80 => tok!("P"),
            81 => tok!("Q"),
            82 => tok!("R"),
            83 => tok!("S"),
            84 => tok!("T"),
            85 => tok!("U"),
            86 => tok!("V"),
            87 => tok!("W"),
            88 => tok!("X"),
            89 => tok!("Y"),
            90 => tok!("Z"),
            91 => tok!("["),
            92 => tok!("\\"),
            93 => tok!("]"),
            94 => tok!("^"),
            95 => tok!("_"),
            96 => tok!("`"),
            97 => tok!("a"),
            98 => tok!("b"),
            99 => tok!("c"),
            100 => tok!("d"),
            101 => tok!("e"),
            102 => tok!("f"),
            103 => tok!("g"),
            104 => tok!("h"),
            105 => tok!("i"),
            106 => tok!("j"),
            107 => tok!("k"),
            108 => tok!("l"),
            109 => tok!("m"),
            110 => tok!("n"),
            111 => tok!("o"),
            112 => tok!("p"),
            113 => tok!("q"),
            114 => tok!("r"),
            115 => tok!("s"),
            116 => tok!("t"),
            117 => tok!("u"),
            118 => tok!("v"),
            119 => tok!("w"),
            120 => tok!("x"),
            121 => tok!("y"),
            122 => tok!("z"),
            123 => tok!("{"),
            124 => tok!("|"),
            125 => tok!("}"),
            126 => tok!("~"),
            127 => tok!("\x7F"),
            _ => tok!("?"),
        }
    }
    pub fn from_utf8(b: &[u8]) -> Token {
        unsafe { Token::l(transmute::<&[u8], &str>(b)) }
    }
    fn skip_whitespace(&mut self) {
        while self.cursor < self.content.len() && self.content[self.cursor].is_ascii_whitespace() {
            self.cursor += 1;
        }
    }

    fn skip_line_comment(&mut self) {
        // Skip the opening // characters
        self.cursor += 2;

        // Skip until we reach a newline or end of content
        while self.cursor < self.content.len() && self.content[self.cursor] != b'\n' {
            // Check for line continuation (backslash followed by newline)
            if self.content[self.cursor] == b'\\'
                && self.cursor + 1 < self.content.len()
                && self.content[self.cursor + 1] == b'\n'
            {
                self.cursor += 2; // Skip backslash and newline
                continue;
            }
            self.cursor += 1;
        }

        // Skip the newline if we haven't reached the end
        if self.cursor < self.content.len() && self.content[self.cursor] == b'\n' {
            self.cursor += 1;
        }
    }

    fn skip_block_comment(&mut self) {
        // Skip the opening /* characters
        self.cursor += 2;

        // Store the starting position for error reporting
        let start_pos: usize = self.cursor - 2;
        let mut chars_consumed: usize = 0;
        let max_comment_length: usize = 5000; // Reasonable limit for a block comment

        // Skip until we find the closing */ sequence
        while self.cursor + 1 < self.content.len() {
            // Check if we found the closing sequence
            if self.content[self.cursor] == b'*' && self.content[self.cursor + 1] == b'/' {
                // Skip the closing */ characters
                self.cursor += 2;
                return; // Normal exit - found closing sequence
            }

            // Safety check to prevent consuming entire large files on unclosed comments
            chars_consumed += 1;
            if chars_consumed > max_comment_length {
                error!(
                    "WARNING: Block comment starting at position {} exceeds maximum length ({}). Assuming comment ended.",
                    start_pos, max_comment_length
                );
                error!(
                    "Comment starts with: {}",
                    String::from_utf8_lossy(
                        &self.content[start_pos..std::cmp::min(start_pos + 40, self.content.len())]
                    )
                );

                // Try to recover by finding potential code patterns
                self.recover_from_unclosed_comment(start_pos);
                return;
            }

            self.cursor += 1;
        }

        // If we reached here, we've hit the end of the content without finding a closing */
        error!(
            "WARNING: Unclosed block comment starting at position {}.",
            start_pos
        );
        error!(
            "Comment starts with: {}",
            String::from_utf8_lossy(
                &self.content[start_pos..std::cmp::min(start_pos + 40, self.content.len())]
            )
        );

        // Try to recover by searching for code patterns in the file
        self.recover_from_unclosed_comment(start_pos);
    }

    // New method to handle recovery from unclosed comments
    fn recover_from_unclosed_comment(&mut self, comment_start_pos: usize) {
        // Store a copy of the current cursor position
        let original_pos: usize = self.cursor;

        // Try to get a UTF-8 string representation of the content
        let content = match String::from_utf8_lossy(&self.content).to_string() {
            s => s,
        };

        // Common C type keywords and modifiers that might indicate function declarations
        let type_patterns = [
            // Basic types
            "void ",
            "int ",
            "char ",
            "float ",
            "double ",
            "long ",
            "short ",
            "unsigned ",
            "signed ",
            "size_t ",
            "bool ",
            // Type modifiers
            "static ",
            "extern ",
            "const ",
            "inline ",
            // Struct/enum/union declarations
            "struct ",
            "enum ",
            "union ",
            // Combinations
            "static int ",
            "static void ",
            "extern int ",
            "extern void ",
            "unsigned int ",
            "unsigned long ",
            "long int ",
            "long long ",
        ];

        // First attempt: Find function declarations after the comment start
        let mut candidates = Vec::<(usize, String)>::new();

        for pattern in &type_patterns {
            let mut search_pos: usize = comment_start_pos;

            while let Some(idx) = content[search_pos..].find(pattern) {
                let pos : usize = search_pos + idx;
                let line_end: usize = content[pos..].find('\n').unwrap_or(content.len() - pos);
                let line: &str = &content[pos..pos + line_end];

                // Check if this looks like a function declaration (has parentheses and identifier)
                if line.contains('(') && line.contains(')') {
                    // Verify there's an identifier before the opening parenthesis
                    if let Some(paren_pos) = line.find('(') {
                        let before_paren: &str = &line[pattern.len()..paren_pos].trim();
                        if !before_paren.is_empty()
                            && before_paren
                                .chars()
                                .all(|c| c.is_alphanumeric() || c == '_')
                        {
                            candidates.push((pos, line.to_string()));
                            debug!(
                                "Found potential function at position {}: {}",
                                pos,
                                line.trim()
                            );
                        }
                    }
                }

                search_pos = pos + 1;
                if search_pos >= content.len() {
                    break;
                }
            }
        }

        // If we found function declarations, resume from the earliest one
        if !candidates.is_empty() {
            candidates.sort_by_key(|(pos, _)| *pos);
            let (resume_pos, decl): (usize, String) = candidates[0].clone();

            info!(
                "Resuming tokenization at function declaration at position {}: {}",
                resume_pos,
                &decl.trim()
            );
            self.cursor = resume_pos;
            warn!(
                "/* Tokenizer recovered from unclosed comment. Resuming at function declaration */"
            );
            return;
        }

        // Second attempt: Look for closing braces followed by potential declarations
        let mut pos: usize = comment_start_pos;
        while pos + 1 < content.len() {
            if content.as_bytes()[pos] == b'}' {
                // Skip whitespace after brace
                let mut after_brace: usize = pos + 1;
                while after_brace < content.len()
                    && content.as_bytes()[after_brace].is_ascii_whitespace()
                {
                    after_brace += 1;
                }

                if after_brace < content.len() {
                    // Check if what follows looks like a declaration
                    let remaining: &str = &content[after_brace..];
                    let next_line_end: usize = remaining.find('\n').unwrap_or(remaining.len());
                    let next_line: &str = &remaining[..next_line_end];

                    // Check for type keywords and function signature pattern
                    if type_patterns
                        .iter()
                        .any(|&pattern| next_line.starts_with(pattern))
                        && next_line.contains('(')
                    {
                        debug!(
                            "Found potential function after brace at position {}: {}",
                            after_brace,
                            next_line.trim()
                        );
                        self.cursor = after_brace;
                        warn!(
                            "/* Tokenizer recovered from unclosed comment after closing brace */"
                        );
                        return;
                    }
                }
            }
            pos += 1;
        }

        // Third attempt: Look for preprocessor directives (#include, #define, etc.)
        if let Some(preprocessor_pos) = content[comment_start_pos..].find('#') {
            let pos: usize = comment_start_pos + preprocessor_pos;
            let line_start: usize = content[..pos].rfind('\n').map_or(0, |i| i + 1);

            // Check if it's at the start of a line (only whitespace before it)
            let prefix: &str = &content[line_start..pos];
            if prefix.trim().is_empty() {
                debug!("Found preprocessor directive at position {}", pos);
                self.cursor = pos;
                warn!("/* Tokenizer recovered from unclosed comment at preprocessor directive */");
                return;
            }
        }

        // If all recovery attempts failed, continue from where we left off
        debug!(
            "No recovery points found. Continuing from position {}",
            original_pos
        );
        self.cursor = original_pos;
    }

    /// Handle backslash line continuation in preprocessor directives
    fn handle_line_continuation(&mut self) -> bool {
        // Check if we're at a backslash
        if self.cursor >= self.content.len() || self.cursor_byte() != b'\\' {
            return false;
        }

        let mut lookahead: usize = self.cursor + 1;

        // Skip any whitespace except newline
        while lookahead < self.content.len()
            && self.content[lookahead].is_ascii_whitespace()
            && self.content[lookahead] != b'\n'
        {
            lookahead += 1;
        }

        // If we found a newline, this is a line continuation
        if lookahead < self.content.len() && self.content[lookahead] == b'\n' {
            // Skip the backslash and all whitespace including the newline
            self.cursor = lookahead + 1;

            // Skip any whitespace at the beginning of the next line
            self.skip_whitespace();
            return true;
        }

        false
    }

    fn peek(&self) -> Option<u8> {
        self.peek_at(1)
    }

    fn peek_at(&self, offset: usize) -> Option<u8> {
        if self.cursor + offset < self.content.len() {
            Some(self.content[self.cursor + offset])
        } else {
            None
        }
    }
}
