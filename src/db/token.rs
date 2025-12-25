//! Token System for DB Framework
//!
//! Provides tokenization and token manipulation with type-safe Token struct.
//! Integrates with the db framework for storage, queries, and transformations.
//!
//! Key components:
//! - `TokenType`: Classification of token types (keyword, identifier, etc.)
//! - `Token`: Type-safe token struct with metadata
//! - `TokenSet`: A windowed collection of tokens for manipulation
//! - `TokenRange`: Range/window for token processing
//! - `Tokenizer`: The core tokenization engine for C code

use crate::db::web::{Entry, Build};
use std::collections::HashMap;
use std::fmt::{self, Display};
use std::hash::{DefaultHasher, Hash, Hasher};
use std::ops::{Bound, Index, IndexMut, Range, RangeBounds};

const DEFAULT_CAPACITY: usize = 10000;

// ============================================================================
// Token Range
// ============================================================================

/// Token range or window for processing
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenRange {
    pub start: usize,
    pub end: usize,
    pub source_file: String,
    pub line_start: Option<usize>,
    pub line_end: Option<usize>,
}

impl TokenRange {
    pub fn new(start: usize, end: usize) -> Self {
        Self {
            start,
            end,
            source_file: String::new(),
            line_start: None,
            line_end: None,
        }
    }

    pub fn with_source(mut self, source: impl Into<String>) -> Self {
        self.source_file = source.into();
        self
    }

    pub fn with_lines(mut self, start: usize, end: usize) -> Self {
        self.line_start = Some(start);
        self.line_end = Some(end);
        self
    }

    pub fn to_range(&self) -> Range<usize> {
        self.start..self.end
    }

    pub fn len(&self) -> usize {
        self.end.saturating_sub(self.start)
    }

    pub fn is_empty(&self) -> bool {
        self.start >= self.end
    }
}

impl Build for TokenRange {
    fn to_entry(&self) -> Entry {
        let mut attrs = HashMap::new();
        attrs.insert("start".to_string(), Entry::usize(self.start));
        attrs.insert("end".to_string(), Entry::usize(self.end));
        if !self.source_file.is_empty() {
            attrs.insert("source_file".to_string(), Entry::string(&self.source_file));
        }
        if let Some(line) = self.line_start {
            attrs.insert("line_start".to_string(), Entry::usize(line));
        }
        if let Some(line) = self.line_end {
            attrs.insert("line_end".to_string(), Entry::usize(line));
        }
        Entry::node_with_attrs("token_range", &format!("{}..{}", self.start, self.end), attrs)
    }

    fn kind(&self) -> &str {
        "token_range"
    }
}

impl From<Range<usize>> for TokenRange {
    fn from(range: Range<usize>) -> Self {
        Self::new(range.start, range.end)
    }
}

// ============================================================================
// Token Type Classification
// ============================================================================

/// Token type classification
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenType {
    Keyword,
    Identifier,
    Literal,
    Number,
    Operator,
    Punctuation,
    Comment,
    Whitespace,
    Preprocessor,
    Type,
    Unknown,
    Consumed,
    Custom(String),
}

impl TokenType {
    pub fn as_str(&self) -> &str {
        match self {
            TokenType::Keyword => "keyword",
            TokenType::Identifier => "identifier",
            TokenType::Literal => "literal",
            TokenType::Number => "number",
            TokenType::Operator => "operator",
            TokenType::Punctuation => "punctuation",
            TokenType::Comment => "comment",
            TokenType::Whitespace => "whitespace",
            TokenType::Preprocessor => "preprocessor",
            TokenType::Type => "type",
            TokenType::Unknown => "unknown",
            TokenType::Consumed => "consumed",
            TokenType::Custom(s) => s.as_str(),
        }
    }

    /// Determine token type from an Entry value
    pub fn from_entry(entry: &Entry) -> Self {
        match entry {
            Entry::String(s, _) => {
                if is_c_keyword(s) {
                    TokenType::Keyword
                } else {
                    TokenType::Identifier
                }
            }
            Entry::Char(c, _) => {
                if is_c_keyword(&c.to_string()) {
                    TokenType::Keyword
                } else if c.is_ascii_punctuation() {
                    TokenType::Punctuation
                } else {
                    TokenType::Literal
                }
            }
            Entry::I8(..) | Entry::I16(..) | Entry::I32(..) | Entry::I64(..) | Entry::I128(..) |
            Entry::U8(..) | Entry::U16(..) | Entry::U32(..) | Entry::U64(..) | Entry::U128(..) |
            Entry::Isize(..) | Entry::Usize(..) | Entry::F32(..) | Entry::F64(..) => TokenType::Number,
            Entry::Unit(_) => TokenType::Consumed,
            _ => TokenType::Unknown,
        }
    }
}

impl Default for TokenType {
    fn default() -> Self {
        TokenType::Unknown
    }
}

impl Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

/// Check if a string is a C keyword
fn is_c_keyword(s: &str) -> bool {
    matches!(
        s,
        "auto" | "break" | "case" | "const" | "continue" | "default" | "do" | "else" |
        "enum" | "extern" | "for" | "goto" | "if" | "inline" | "register" | "restrict" |
        "return" | "signed" | "sizeof" | "static" | "struct" | "switch" | "typedef" |
        "union" | "unsigned" | "void" | "volatile" | "while" | "int" | "long" | "short" |
        "char" | "float" | "double" | "_Bool" | "_Complex" | "_Imaginary"
    )
}

// ============================================================================
// Token Structure
// ============================================================================

/// Token node kind constant
pub const TOKEN_KIND: &str = "Token";

/// Token value - the actual content of the token
#[derive(Debug, Clone, PartialEq)]
pub enum TokenValue {
    String(String),
    Char(char),
    Int(i128),
    Float(f64),
    None,
}

impl TokenValue {
    pub fn as_string(&self) -> Option<&str> {
        match self {
            TokenValue::String(s) => Some(s),
            _ => None,
        }
    }
    
    pub fn as_int(&self) -> Option<i128> {
        match self {
            TokenValue::Int(n) => Some(*n),
            TokenValue::String(s) => s.parse().ok(),
            _ => None,
        }
    }
    
    pub fn as_float(&self) -> Option<f64> {
        match self {
            TokenValue::Float(n) => Some(*n),
            TokenValue::Int(n) => Some(*n as f64),
            TokenValue::String(s) => s.parse().ok(),
            _ => None,
        }
    }
    
    pub fn as_char(&self) -> Option<char> {
        match self {
            TokenValue::Char(c) => Some(*c),
            TokenValue::String(s) => s.chars().next(),
            _ => None,
        }
    }
    
    pub fn to_string_repr(&self) -> String {
        match self {
            TokenValue::String(s) => s.clone(),
            TokenValue::Char(c) => c.to_string(),
            TokenValue::Int(n) => n.to_string(),
            TokenValue::Float(n) => n.to_string(),
            TokenValue::None => String::new(),
        }
    }
}

/// Type-safe Token struct
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    value: TokenValue,
    token_type: TokenType,
    is_literal: bool,
    is_string: bool,
}

impl Token {
    /// Create a new token with value and type
    pub fn new(value: impl Into<String>, token_type: TokenType) -> Self {
        Self {
            value: TokenValue::String(value.into()),
            token_type,
            is_literal: false,
            is_string: false,
        }
    }
    
    /// Create a token from a string, auto-detecting type
    pub fn from_string(value: impl Into<String>) -> Self {
        let s = value.into();
        let token_type = if is_c_keyword(&s) {
            TokenType::Keyword
        } else {
            TokenType::Identifier
        };
        Self::new(s, token_type)
    }
    
    /// Create a numeric token
    pub fn number(value: i128) -> Self {
        Self {
            value: TokenValue::Int(value),
            token_type: TokenType::Number,
            is_literal: false,
            is_string: false,
        }
    }
    
    /// Create a float token
    pub fn float(value: f64) -> Self {
        Self {
            value: TokenValue::Float(value),
            token_type: TokenType::Number,
            is_literal: false,
            is_string: false,
        }
    }
    
    /// Create a char token
    pub fn char(value: char) -> Self {
        let token_type = if value.is_ascii_punctuation() {
            TokenType::Punctuation
        } else {
            TokenType::Literal
        };
        Self {
            value: TokenValue::Char(value),
            token_type,
            is_literal: false,
            is_string: false,
        }
    }
    
    /// Create a consumed/placeholder token
    pub fn consumed() -> Self {
        Self {
            value: TokenValue::None,
            token_type: TokenType::Consumed,
            is_literal: false,
            is_string: false,
        }
    }
    
    /// Create a whitespace token
    pub fn whitespace(value: impl Into<String>) -> Self {
        Self::new(value, TokenType::Whitespace)
    }
    
    /// Create a comment token
    pub fn comment(value: impl Into<String>) -> Self {
        Self::new(value, TokenType::Comment)
    }
    
    /// Create a literal/operator token
    pub fn literal(value: &'static str) -> Self {
        let mut tok = Self::new(value, TokenType::Operator);
        tok.is_literal = true;
        tok
    }
    
    /// Create a string literal token
    pub fn string_literal(value: impl Into<String>) -> Self {
        let mut tok = Self::new(value, TokenType::Literal);
        tok.is_string = true;
        tok
    }
    
    // Getters
    
    pub fn value(&self) -> &TokenValue {
        &self.value
    }
    
    pub fn token_type(&self) -> &TokenType {
        &self.token_type
    }
    
    pub fn name(&self) -> Option<&str> {
        self.value.as_string()
    }
    
    pub fn is_keyword(&self) -> bool {
        matches!(self.token_type, TokenType::Keyword)
    }
    
    pub fn is_consumed(&self) -> bool {
        matches!(self.token_type, TokenType::Consumed)
    }
    
    pub fn is_number(&self) -> bool {
        matches!(self.token_type, TokenType::Number)
    }
    
    pub fn is_identifier(&self) -> bool {
        matches!(self.token_type, TokenType::Identifier)
    }
    
    pub fn is_operator(&self) -> bool {
        matches!(self.token_type, TokenType::Operator)
    }
    
    pub fn is_punctuation(&self) -> bool {
        matches!(self.token_type, TokenType::Punctuation)
    }
    
    pub fn is_string_like(&self) -> bool {
        matches!(self.token_type, TokenType::Identifier | TokenType::Literal | TokenType::Keyword)
    }
    
    pub fn as_int(&self) -> Option<i128> {
        self.value.as_int()
    }
    
    pub fn as_float(&self) -> Option<f64> {
        self.value.as_float()
    }
    
    pub fn as_char(&self) -> Option<char> {
        self.value.as_char()
    }
    
    /// Convert to Entry for storage
    pub fn to_entry(&self) -> Entry {
        let name = self.value.to_string_repr();
        let mut node = Entry::node(TOKEN_KIND, name);
        node.set_attr("token_type", Entry::string(self.token_type.as_str()));
        
        // Store numeric values
        match &self.value {
            TokenValue::Int(n) => { node.set_attr("value", Entry::i128(*n)); }
            TokenValue::Float(n) => { node.set_attr("value", Entry::f64(*n)); }
            TokenValue::Char(c) => { node.set_attr("char_value", Entry::char(*c)); }
            _ => {}
        }
        
        if self.is_literal {
            node.set_attr("is_literal", Entry::bool(true));
        }
        if self.is_string {
            node.set_attr("is_string", Entry::bool(true));
        }
        
        node
    }
    
    /// Create from Entry
    pub fn from_entry(entry: &Entry) -> Option<Self> {
        if entry.kind() != Some(TOKEN_KIND) {
            return None;
        }
        
        let name = entry.name().unwrap_or("").to_string();
        let type_str = entry.get_string_attr("token_type").unwrap_or("unknown");
        
        let token_type = match type_str {
            "keyword" => TokenType::Keyword,
            "identifier" => TokenType::Identifier,
            "literal" => TokenType::Literal,
            "number" => TokenType::Number,
            "operator" => TokenType::Operator,
            "punctuation" => TokenType::Punctuation,
            "comment" => TokenType::Comment,
            "whitespace" => TokenType::Whitespace,
            "preprocessor" => TokenType::Preprocessor,
            "type" => TokenType::Type,
            "consumed" => TokenType::Consumed,
            "unknown" => TokenType::Unknown,
            s => TokenType::Custom(s.to_string()),
        };
        
        // Determine value type
        let value = if let Some(Entry::I128(n, _)) = entry.attr("value") {
            TokenValue::Int(*n)
        } else if let Some(Entry::I64(n, _)) = entry.attr("value") {
            TokenValue::Int(*n as i128)
        } else if let Some(Entry::F64(n, _)) = entry.attr("value") {
            TokenValue::Float(*n)
        } else if let Some(Entry::Char(c, _)) = entry.attr("char_value") {
            TokenValue::Char(*c)
        } else if name.is_empty() {
            TokenValue::None
        } else {
            TokenValue::String(name)
        };
        
        let is_literal = entry.get_bool_attr("is_literal").unwrap_or(false);
        let is_string = entry.get_bool_attr("is_string").unwrap_or(false);
        
        Some(Self {
            value,
            token_type,
            is_literal,
            is_string,
        })
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value.to_string_repr())
    }
}

impl Build for Token {
    fn to_entry(&self) -> Entry {
        Token::to_entry(self)
    }
    
    fn kind(&self) -> &str {
        TOKEN_KIND
    }
    
    fn name(&self) -> Option<&str> {
        self.value.as_string()
    }
    
    fn category(&self) -> Option<&str> {
        Some(self.token_type.as_str())
    }
}

// ============================================================================
// Factory Functions (convenience wrappers)
// ============================================================================

/// Create a token from a string value
pub fn token(value: impl Into<String>) -> Token {
    Token::from_string(value)
}

/// Create a token with explicit type
pub fn token_typed(value: impl Into<String>, token_type: TokenType) -> Token {
    Token::new(value, token_type)
}

/// Create a numeric token
pub fn token_number(value: i128) -> Token {
    Token::number(value)
}

/// Create a float token
pub fn token_float(value: f64) -> Token {
    Token::float(value)
}

/// Create a char token
pub fn token_char(value: char) -> Token {
    Token::char(value)
}

/// Create a consumed/placeholder token
pub fn token_consumed() -> Token {
    Token::consumed()
}

/// Create a whitespace token
pub fn token_whitespace(value: impl Into<String>) -> Token {
    Token::whitespace(value)
}

/// Create a comment token
pub fn token_comment(value: impl Into<String>) -> Token {
    Token::comment(value)
}

/// Create a literal/operator token
pub fn token_literal(value: &'static str) -> Token {
    Token::literal(value)
}

// ============================================================================
// Token Set
// ============================================================================

/// A set of tokens with range and window information
#[derive(Debug, Clone, PartialEq)]
pub struct TokenSet {
    range: Range<usize>,
    window: Range<usize>,
    position: usize,
    consumed: usize,
    tokens: Vec<Token>,
    checksum: u64,
}

impl TokenSet {
    pub fn new(range: Range<usize>, window: Range<usize>, tokens: Vec<Token>) -> Self {
        let mut hasher = DefaultHasher::new();
        range.start.hash(&mut hasher);
        range.end.hash(&mut hasher);
        window.start.hash(&mut hasher);
        window.end.hash(&mut hasher);
        let checksum = hasher.finish();
        let position = range.start;

        Self {
            range,
            window,
            position,
            consumed: 0,
            tokens,
            checksum,
        }
    }

    pub fn range(&self) -> Range<usize> {
        self.range.clone()
    }

    pub fn window(&self) -> Range<usize> {
        self.window.clone()
    }

    pub fn position(&self) -> usize {
        self.position
    }

    pub fn consumed(&self) -> usize {
        self.consumed
    }

    pub fn checksum(&self) -> u64 {
        self.checksum
    }

    pub fn size(&self) -> usize {
        self.range.end - self.range.start
    }

    pub fn window_size(&self) -> usize {
        self.window.end - self.window.start
    }

    pub fn tokens(&self) -> &[Token] {
        &self.tokens
    }

    pub fn tokens_mut(&mut self) -> &mut [Token] {
        &mut self.tokens
    }

    pub fn is_empty(&self) -> bool {
        self.tokens.is_empty()
    }

    pub fn len(&self) -> usize {
        self.tokens.len()
    }

    pub fn overlaps(&self, other: &TokenSet) -> bool {
        self.range.start < other.range.end && self.range.end > other.range.start
    }

    pub fn get_overlap(&self, other: &TokenSet) -> Option<Range<usize>> {
        if self.overlaps(other) {
            let start = self.range.start.max(other.range.start);
            let end = self.range.end.min(other.range.end);
            Some(start..end)
        } else {
            None
        }
    }

    pub fn overlap_percentage(&self, other: &TokenSet) -> f64 {
        if let Some(overlap) = self.get_overlap(other) {
            let overlap_size = overlap.end - overlap.start;
            let self_size = self.size();
            if self_size > 0 {
                overlap_size as f64 / self_size as f64
            } else {
                0.0
            }
        } else {
            0.0
        }
    }

    pub fn split_at(&self, pos: usize) -> Option<(TokenSet, TokenSet)> {
        if pos <= self.range.start || pos >= self.range.end {
            return None;
        }

        let split_offset = pos - self.range.start;
        let left_tokens = self.tokens[..split_offset].to_vec();
        let right_tokens = self.tokens[split_offset..].to_vec();

        let left = TokenSet::new(self.range.start..pos, self.window.start..pos, left_tokens);
        let right = TokenSet::new(pos..self.range.end, pos..self.window.end, right_tokens);

        Some((left, right))
    }

    pub fn merge(&self, other: &TokenSet) -> Option<TokenSet> {
        if self.range.end != other.range.start {
            return None;
        }

        let mut merged_tokens = self.tokens.clone();
        merged_tokens.extend(other.tokens.clone());

        Some(TokenSet::new(
            self.range.start..other.range.end,
            self.window.start..other.window.end,
            merged_tokens,
        ))
    }

    pub fn remove_duplicates(sets: &mut Vec<TokenSet>) {
        let mut seen_checksums = std::collections::HashSet::new();
        sets.retain(|set| seen_checksums.insert(set.checksum()));
    }

    pub fn deduplicate_overlaps(sets: &mut Vec<TokenSet>) {
        let mut i = 0;
        while i < sets.len() {
            let mut j = i + 1;
            while j < sets.len() {
                if sets[i].overlaps(&sets[j]) {
                    sets.remove(j);
                } else {
                    j += 1;
                }
            }
            i += 1;
        }
    }

    /// Check if any tokens in a range are consumed
    pub fn has_consumed_in_range(&self, range: &Range<usize>) -> bool {
        for pos in range.clone() {
            let local_pos = pos.saturating_sub(self.range.start);
            if local_pos < self.tokens.len() {
                if self.tokens[local_pos].is_consumed() {
                    return true;
                }
            }
        }
        false
    }
    /// Create from Entry
    pub fn from_entry(entry: &Entry) -> Option<Self> {
        let (metadata, tokens_entry) = entry.as_pair()?;
        
        // Extract metadata
        let metadata_map = match metadata {
            Entry::HashMap(m, _) => m,
            _ => return None,
        };
        
        let range_start = metadata_map.get("range_start").and_then(|e| e.as_usize()).unwrap_or(0);
        let range_end = metadata_map.get("range_end").and_then(|e| e.as_usize()).unwrap_or(0);
        let window_start = metadata_map.get("window_start").and_then(|e| e.as_usize()).unwrap_or(0);
        let window_end = metadata_map.get("window_end").and_then(|e| e.as_usize()).unwrap_or(0);
        let position = metadata_map.get("position").and_then(|e| e.as_usize()).unwrap_or(range_start);
        let consumed = metadata_map.get("consumed").and_then(|e| e.as_usize()).unwrap_or(0);
        let checksum = metadata_map.get("checksum").and_then(|e| e.as_u64()).unwrap_or(0);
        
        // Extract tokens
        let tokens_vec = match tokens_entry {
            Entry::Vec(v, _) => v,
            _ => return None,
        };
        
        let tokens: Vec<Token> = tokens_vec.iter().filter_map(Token::from_entry).collect();
        
        Some(Self {
            range: range_start..range_end,
            window: window_start..window_end,
            position,
            consumed,
            tokens,
            checksum,
        })
    }
}

impl Build for TokenSet {
    fn to_entry(&self) -> Entry {
        // Metadata HashMap
        let mut metadata = HashMap::new();
        metadata.insert("range_start".to_string(), Entry::usize(self.range.start));
        metadata.insert("range_end".to_string(), Entry::usize(self.range.end));
        metadata.insert("window_start".to_string(), Entry::usize(self.window.start));
        metadata.insert("window_end".to_string(), Entry::usize(self.window.end));
        metadata.insert("position".to_string(), Entry::usize(self.position));
        metadata.insert("consumed".to_string(), Entry::usize(self.consumed));
        metadata.insert("checksum".to_string(), Entry::u64(self.checksum));
        
        // Tokens Vec
        let tokens_vec: Vec<Entry> = self.tokens.iter().map(|t| t.to_entry()).collect();
        
        // Pair: (metadata, tokens)
        Entry::pair(Entry::hashmap(metadata), Entry::vec(tokens_vec))
    }
    
    fn kind(&self) -> &str {
        "TokenSet"
    }
    
    fn name(&self) -> Option<&str> {
        None
    }
    
    fn category(&self) -> Option<&str> {
        Some("token")
    }
}

impl Default for TokenSet {
    fn default() -> Self {
        Self {
            range: 0..0,
            window: 0..0,
            position: 0,
            consumed: 0,
            tokens: Vec::new(),
            checksum: 0,
        }
    }
}

impl Index<usize> for TokenSet {
    type Output = Token;

    fn index(&self, index: usize) -> &Self::Output {
        &self.tokens[index]
    }
}

impl IndexMut<usize> for TokenSet {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.tokens[index]
    }
}

impl RangeBounds<usize> for TokenSet {
    fn start_bound(&self) -> Bound<&usize> {
        self.range.start_bound()
    }

    fn end_bound(&self) -> Bound<&usize> {
        self.range.end_bound()
    }
}

impl From<Vec<Token>> for TokenSet {
    fn from(tokens: Vec<Token>) -> Self {
        Self {
            range: 0..tokens.len(),
            window: 0..tokens.len(),
            position: 0,
            consumed: 0,
            tokens,
            checksum: 0,
        }
    }
}

impl AsRef<[Token]> for TokenSet {
    fn as_ref(&self) -> &[Token] {
        &self.tokens
    }
}

impl AsMut<[Token]> for TokenSet {
    fn as_mut(&mut self) -> &mut [Token] {
        &mut self.tokens
    }
}

// ============================================================================
// Tokenizer
// ============================================================================

/// C code tokenizer that produces Token objects
#[derive(Debug, Clone)]
pub struct Tokenizer {
    content: Vec<u8>,
    slots: Vec<Vec<Token>>,
    position: usize,
    capacity: usize,
}

impl Tokenizer {
    pub fn new() -> Self {
        Self::with_capacity(DEFAULT_CAPACITY)
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            content: Vec::with_capacity(capacity),
            slots: Vec::new(),
            position: 0,
            capacity,
        }
    }

    pub fn set_content(&mut self, content: Vec<u8>) {
        self.content = content;
        self.position = 0;
    }

    pub fn get_content(&self) -> &[u8] {
        &self.content
    }

    pub fn slot_count(&self) -> usize {
        self.slots.len()
    }

    pub fn get_slot(&self, slot_id: usize) -> Option<&Vec<Token>> {
        self.slots.get(slot_id)
    }

    pub fn add_slot(&mut self, tokens: Vec<Token>) -> usize {
        let slot_id = self.slots.len();
        self.slots.push(tokens);
        slot_id
    }

    pub fn slots(&self) -> &[Vec<Token>] {
        &self.slots
    }

    /// Tokenize the content and return slot ID
    pub fn tokenize(&mut self) -> usize {
        let mut tokens = Vec::new();
        self.position = 0;

        while self.position < self.content.len() {
            if let Some(token) = self.next_token() {
                tokens.push(token);
            }
        }

        self.add_slot(tokens)
    }

    /// Tokenize content into a TokenSet
    pub fn tokenize_to_set(&mut self) -> TokenSet {
        let slot_id = self.tokenize();
        let tokens = self.get_slot(slot_id).cloned().unwrap_or_default();
        let len = tokens.len();
        TokenSet::new(0..len, 0..len, tokens)
    }

    fn next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();

        if self.position >= self.content.len() {
            return None;
        }

        let b = self.content[self.position];

        if b.is_ascii_alphabetic() || b == b'_' {
            return self.read_identifier();
        }

        if b.is_ascii_digit() {
            return self.read_number();
        }

        match b {
            b'+' => self.read_plus(),
            b'-' => self.read_minus(),
            b'*' => self.read_star(),
            b'/' => self.read_slash(),
            b'=' => self.read_equals(),
            b'<' | b'>' | b'&' | b'|' | b'!' | b'"' | b'\'' | b'#' | b'.' => {
                self.handle_special_char(b)
            }
            b'(' | b')' | b'{' | b'}' | b'[' | b']' | b';' | b',' | b':' | b'?' | b'~' | b'^' | b'%' => {
                self.position += 1;
                Some(token_char(b as char))
            }
            _ => {
                self.position += 1;
                Some(token_char(b as char))
            }
        }
    }

    fn read_plus(&mut self) -> Option<Token> {
        if self.peek() == Some(b'+') {
            self.position += 2;
            Some(token_literal("++"))
        } else if self.peek() == Some(b'=') {
            self.position += 2;
            Some(token_literal("+="))
        } else {
            self.position += 1;
            Some(token_char('+'))
        }
    }

    fn read_minus(&mut self) -> Option<Token> {
        if let Some(next_byte) = self.peek() {
            if next_byte.is_ascii_digit() {
                let prev_is_digit = if self.position > 0 {
                    self.content[self.position - 1].is_ascii_digit()
                } else {
                    false
                };

                if !prev_is_digit {
                    return self.read_signed_number();
                }
            }
        }

        if self.peek() == Some(b'-') {
            self.position += 2;
            Some(token_literal("--"))
        } else if self.peek() == Some(b'=') {
            self.position += 2;
            Some(token_literal("-="))
        } else if self.peek() == Some(b'>') {
            self.position += 2;
            Some(token_literal("->"))
        } else {
            self.position += 1;
            Some(token_char('-'))
        }
    }

    fn read_star(&mut self) -> Option<Token> {
        if self.peek() == Some(b'=') {
            self.position += 2;
            Some(token_literal("*="))
        } else {
            self.position += 1;
            Some(token_char('*'))
        }
    }

    fn read_slash(&mut self) -> Option<Token> {
        if self.peek() == Some(b'/') {
            self.read_line_comment()
        } else if self.peek() == Some(b'*') {
            self.read_block_comment()
        } else if self.peek() == Some(b'=') {
            self.position += 2;
            Some(token_literal("/="))
        } else {
            self.position += 1;
            Some(token_char('/'))
        }
    }

    fn read_equals(&mut self) -> Option<Token> {
        if self.peek() == Some(b'=') {
            self.position += 2;
            Some(token_literal("=="))
        } else {
            self.position += 1;
            Some(token_char('='))
        }
    }

    fn handle_special_char(&mut self, b: u8) -> Option<Token> {
        match b {
            b'<' => {
                if self.peek() == Some(b'<') {
                    if self.peek_at(2) == Some(b'=') {
                        self.position += 3;
                        Some(token_literal("<<="))
                    } else {
                        self.position += 2;
                        Some(token_literal("<<"))
                    }
                } else if self.peek() == Some(b'=') {
                    self.position += 2;
                    Some(token_literal("<="))
                } else {
                    self.position += 1;
                    Some(token_char('<'))
                }
            }
            b'>' => {
                if self.peek() == Some(b'>') {
                    if self.peek_at(2) == Some(b'=') {
                        self.position += 3;
                        Some(token_literal(">>="))
                    } else {
                        self.position += 2;
                        Some(token_literal(">>"))
                    }
                } else if self.peek() == Some(b'=') {
                    self.position += 2;
                    Some(token_literal(">="))
                } else {
                    self.position += 1;
                    Some(token_char('>'))
                }
            }
            b'&' => {
                if self.peek() == Some(b'&') {
                    self.position += 2;
                    Some(token_literal("&&"))
                } else if self.peek() == Some(b'=') {
                    self.position += 2;
                    Some(token_literal("&="))
                } else {
                    self.position += 1;
                    Some(token_char('&'))
                }
            }
            b'|' => {
                if self.peek() == Some(b'|') {
                    self.position += 2;
                    Some(token_literal("||"))
                } else if self.peek() == Some(b'=') {
                    self.position += 2;
                    Some(token_literal("|="))
                } else {
                    self.position += 1;
                    Some(token_char('|'))
                }
            }
            b'!' => {
                if self.peek() == Some(b'=') {
                    self.position += 2;
                    Some(token_literal("!="))
                } else {
                    self.position += 1;
                    Some(token_char('!'))
                }
            }
            b'"' => self.read_string_literal(),
            b'\'' => self.read_char_literal(),
            b'#' => {
                if self.peek() == Some(b'#') {
                    self.position += 2;
                    Some(token_literal("##"))
                } else {
                    self.position += 1;
                    Some(token_char('#'))
                }
            }
            b'.' => {
                if self.peek() == Some(b'.') && self.peek_at(2) == Some(b'.') {
                    self.position += 3;
                    Some(token_literal("..."))
                } else {
                    self.position += 1;
                    Some(token_char('.'))
                }
            }
            _ => {
                self.position += 1;
                Some(token_char(b as char))
            }
        }
    }

    fn skip_whitespace(&mut self) {
        while self.position < self.content.len() {
            let b = self.content[self.position];
            if b.is_ascii_whitespace() {
                self.position += 1;
            } else {
                break;
            }
        }
    }

    fn read_identifier(&mut self) -> Option<Token> {
        let start = self.position;
        while self.position < self.content.len() {
            let b = self.content[self.position];
            if b.is_ascii_alphanumeric() || b == b'_' {
                self.position += 1;
            } else {
                break;
            }
        }
        let ident = String::from_utf8_lossy(&self.content[start..self.position]).to_string();
        Some(token(ident))
    }

    fn read_number(&mut self) -> Option<Token> {
        let start = self.position;
        let mut is_float = false;

        while self.position < self.content.len() {
            let b = self.content[self.position];
            if b.is_ascii_digit() {
                self.position += 1;
            } else if b == b'.' && !is_float {
                is_float = true;
                self.position += 1;
            } else {
                break;
            }
        }

        let num_str = String::from_utf8_lossy(&self.content[start..self.position]).to_string();

        if is_float {
            if let Ok(f) = num_str.parse::<f64>() {
                Some(token_float(f))
            } else {
                Some(token(num_str))
            }
        } else if let Ok(i) = num_str.parse::<i128>() {
            Some(token_number(i))
        } else {
            Some(token(num_str))
        }
    }

    fn read_signed_number(&mut self) -> Option<Token> {
        let start = self.position;
        self.position += 1; // Skip '-'

        let mut is_float = false;

        while self.position < self.content.len() {
            let b = self.content[self.position];
            if b.is_ascii_digit() {
                self.position += 1;
            } else if b == b'.' && !is_float {
                is_float = true;
                self.position += 1;
            } else {
                break;
            }
        }

        let num_str = String::from_utf8_lossy(&self.content[start..self.position]).to_string();

        if is_float {
            if let Ok(f) = num_str.parse::<f64>() {
                Some(token_float(f))
            } else {
                Some(token(num_str))
            }
        } else if let Ok(i) = num_str.parse::<i128>() {
            Some(token_number(i))
        } else {
            Some(token(num_str))
        }
    }

    fn read_string_literal(&mut self) -> Option<Token> {
        let mut literal = String::new();
        self.position += 1;

        while self.position < self.content.len() {
            let b = self.content[self.position];
            if b == b'"' {
                self.position += 1;
                return Some(Token::string_literal(literal));
            } else if b == b'\\' && self.position + 1 < self.content.len() {
                self.position += 1;
                let escape_char = self.content[self.position];
                match escape_char {
                    b'n' => literal.push('\n'),
                    b't' => literal.push('\t'),
                    b'r' => literal.push('\r'),
                    b'\\' => literal.push('\\'),
                    b'"' => literal.push('"'),
                    b'0' => literal.push('\0'),
                    _ => {
                        literal.push('\\');
                        literal.push(escape_char as char);
                    }
                }
                self.position += 1;
            } else {
                literal.push(b as char);
                self.position += 1;
            }
        }
        None
    }

    fn read_char_literal(&mut self) -> Option<Token> {
        self.position += 1;

        if self.position >= self.content.len() {
            return None;
        }

        let mut c = self.content[self.position] as char;

        if c == '\\' && self.position + 1 < self.content.len() {
            self.position += 1;
            c = self.content[self.position] as char;
        }

        self.position += 1;

        if self.position < self.content.len() && self.content[self.position] == b'\'' {
            self.position += 1;
            return Some(Token::char(c));
        }

        None
    }

    fn read_line_comment(&mut self) -> Option<Token> {
        let start = self.position;

        while self.position < self.content.len() && self.content[self.position] != b'\n' {
            self.position += 1;
        }

        let comment = String::from_utf8_lossy(&self.content[start..self.position]).to_string();
        Some(token_comment(comment))
    }

    fn read_block_comment(&mut self) -> Option<Token> {
        let start = self.position;
        self.position += 2;

        while self.position + 1 < self.content.len() {
            if self.content[self.position] == b'*' && self.content[self.position + 1] == b'/' {
                self.position += 2;
                break;
            }
            self.position += 1;
        }

        let comment = String::from_utf8_lossy(&self.content[start..self.position]).to_string();
        Some(token_comment(comment))
    }

    fn peek(&self) -> Option<u8> {
        self.peek_at(1)
    }

    fn peek_at(&self, offset: usize) -> Option<u8> {
        if self.position + offset < self.content.len() {
            Some(self.content[self.position + offset])
        } else {
            None
        }
    }
}

impl Default for Tokenizer {
    fn default() -> Self {
        Self::new()
    }
}

impl PartialEq for Tokenizer {
    fn eq(&self, other: &Self) -> bool {
        self.content == other.content
            && self.slots == other.slots
            && self.position == other.position
            && self.capacity == other.capacity
    }
}

impl Eq for Tokenizer {}

impl Hash for Tokenizer {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.content.hash(state);
        self.position.hash(state);
        self.capacity.hash(state);
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_token_type() {
        assert_eq!(TokenType::Keyword.as_str(), "keyword");
        assert_eq!(TokenType::from_entry(&Entry::i32(42)), TokenType::Number);
    }

    #[test]
    fn test_token_creation() {
        let t = token("main");
        assert_eq!(t.name(), Some("main"));
        assert_eq!(*t.token_type(), TokenType::Identifier);
        
        let t = token("if");
        assert_eq!(*t.token_type(), TokenType::Keyword);
    }

    #[test]
    fn test_token_number() {
        let t = token_number(42);
        assert!(t.is_number());
        assert_eq!(*t.token_type(), TokenType::Number);
        assert_eq!(t.as_int(), Some(42));
    }

    #[test]
    fn test_token_float() {
        let t = token_float(3.14);
        assert_eq!(*t.token_type(), TokenType::Number);
        assert!((t.as_float().unwrap() - 3.14).abs() < 0.001);
    }

    #[test]
    fn test_tokenizer_basic() {
        let mut tokenizer = Tokenizer::new();
        tokenizer.set_content(b"int main() { return 0; }".to_vec());
        let slot_id = tokenizer.tokenize();
        
        let tokens = tokenizer.get_slot(slot_id).unwrap();
        assert!(!tokens.is_empty());
        
        // Check first token is "int" keyword
        assert_eq!(tokens[0].name(), Some("int"));
        assert_eq!(*tokens[0].token_type(), TokenType::Keyword);
    }

    #[test]
    fn test_tokenizer_operators() {
        let mut tokenizer = Tokenizer::new();
        tokenizer.set_content(b"a += b++".to_vec());
        let slot_id = tokenizer.tokenize();
        
        let tokens = tokenizer.get_slot(slot_id).unwrap();
        assert!(tokens.iter().any(|t| t.name() == Some("+=")));
        assert!(tokens.iter().any(|t| t.name() == Some("++")));
    }

    #[test]
    fn test_tokenizer_numbers() {
        let mut tokenizer = Tokenizer::new();
        tokenizer.set_content(b"42 3.14 -5".to_vec());
        let slot_id = tokenizer.tokenize();
        
        let tokens = tokenizer.get_slot(slot_id).unwrap();
        assert_eq!(tokens.len(), 3);
        assert_eq!(tokens[0].as_int(), Some(42));
        assert!((tokens[1].as_float().unwrap() - 3.14).abs() < 0.001);
        assert_eq!(tokens[2].as_int(), Some(-5));
    }

    #[test]
    fn test_token_set() {
        let tokens = vec![token("a"), token("b"), token("c")];
        let set = TokenSet::new(0..3, 0..3, tokens);
        
        assert_eq!(set.len(), 3);
        assert_eq!(set.size(), 3);
        assert_eq!(set[0].name(), Some("a"));
    }

    #[test]
    fn test_token_set_split() {
        let tokens = vec![token("a"), token("b"), token("c"), token("d")];
        let set = TokenSet::new(0..4, 0..4, tokens);
        
        let (left, right) = set.split_at(2).unwrap();
        assert_eq!(left.len(), 2);
        assert_eq!(right.len(), 2);
        assert_eq!(left[0].name(), Some("a"));
        assert_eq!(right[0].name(), Some("c"));
    }

    #[test]
    fn test_token_set_merge() {
        let t1 = vec![token("a"), token("b")];
        let t2 = vec![token("c"), token("d")];
        let set1 = TokenSet::new(0..2, 0..2, t1);
        let set2 = TokenSet::new(2..4, 2..4, t2);
        
        let merged = set1.merge(&set2).unwrap();
        assert_eq!(merged.len(), 4);
        assert_eq!(merged.range(), 0..4);
    }

    #[test]
    fn test_c_keywords() {
        assert!(is_c_keyword("if"));
        assert!(is_c_keyword("while"));
        assert!(is_c_keyword("int"));
        assert!(!is_c_keyword("main"));
        assert!(!is_c_keyword("foo"));
    }
}
