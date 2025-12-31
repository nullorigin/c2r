//! Web - A unified data + graph structure
//!
//! Combines the rich value types of Entry with the graph connectivity of Web.
//! Each variant stores both data and optional links to other sites in an arena.
//!
//! # Design
//! - Each variant holds: (data, Links)
//! - Links contain typed, weighted edges to other nodes (by index)
//! - Supports cycles, multiple edge types, and arbitrary graph structures
//! - Can represent trees, DAGs, or general graphs

use std::collections::HashMap;
use std::fmt::{self, Debug, Display};
use std::hash::{Hash, Hasher};
use std::ops::Range;
use std::path::PathBuf;

use crate::db::NodeKind;

// ============================================================================
// Branch Operations (for conditional tree structures)
// ============================================================================

/// Logical operations for branch evaluation
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum BranchOp {
    /// Both conditions must be true (AND)
    #[default]
    And,
    /// Either condition must be true (OR)
    Or,
    /// Neither condition can be true (NOR)
    Nor,
    /// Not both conditions true (NAND)
    Nand,
    /// Exactly one condition true (XOR)
    Xor,
    /// Left true, right false (AND NOT)
    AndNot,
    /// Left false, right true (NOT AND)
    NotAnd,
}

impl BranchOp {
    /// Evaluate the logical operation
    pub fn evaluate(&self, left: bool, right: bool) -> bool {
        match self {
            Self::And => left && right,
            Self::Or => left || right,
            Self::Nor => !(left || right),
            Self::Nand => !(left && right),
            Self::Xor => left ^ right,
            Self::AndNot => left && !right,
            Self::NotAnd => !left && right,
        }
    }

    /// Get operation name as string
    pub fn name(&self) -> &'static str {
        match self {
            Self::And => "AND",
            Self::Or => "OR",
            Self::Nor => "NOR",
            Self::Nand => "NAND",
            Self::Xor => "XOR",
            Self::AndNot => "AND_NOT",
            Self::NotAnd => "NOT_AND",
        }
    }

    /// Parse from string
    pub fn from_str(s: &str) -> Option<Self> {
        match s.to_uppercase().as_str() {
            "AND" => Some(Self::And),
            "OR" => Some(Self::Or),
            "NOR" => Some(Self::Nor),
            "NAND" => Some(Self::Nand),
            "XOR" => Some(Self::Xor),
            "AND_NOT" | "ANDNOT" => Some(Self::AndNot),
            "NOT_AND" | "NOTAND" => Some(Self::NotAnd),
            _ => None,
        }
    }
}

// ============================================================================
// Link Types (from Web)
// ============================================================================

/// The kind/direction of a link between sites.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum LinkKind {
    #[default]
    Forward,
    Back,
    Branch,
    Lateral,
    Skip,
    Cross,
    Loop,
    Output,
    Failure,
    Parent,
    Child,
    Sibling,
    Angle(i16),
    Custom(u16),
}

impl LinkKind {
    pub fn is_forward(&self) -> bool {
        matches!(
            self,
            Self::Forward | Self::Branch | Self::Skip | Self::Child
        )
    }

    pub fn is_backward(&self) -> bool {
        matches!(self, Self::Back | Self::Failure | Self::Parent)
    }

    pub fn is_lateral(&self) -> bool {
        matches!(self, Self::Lateral | Self::Cross | Self::Sibling)
    }
}

impl Display for LinkKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Forward => write!(f, "→"),
            Self::Back => write!(f, "←"),
            Self::Branch => write!(f, "⑂"),
            Self::Lateral => write!(f, "↔"),
            Self::Skip => write!(f, "⇢"),
            Self::Cross => write!(f, "✕"),
            Self::Loop => write!(f, "↻"),
            Self::Output => write!(f, "◉"),
            Self::Failure => write!(f, "⤺"),
            Self::Parent => write!(f, "↑"),
            Self::Child => write!(f, "↓"),
            Self::Sibling => write!(f, "↔"),
            Self::Angle(a) => write!(f, "∠{}", a),
            Self::Custom(n) => write!(f, "#{}", n),
        }
    }
}

// ============================================================================
// Link - A weighted, typed edge
// ============================================================================

/// A link to another entry in a Site.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Link {
    pub target: usize,
    pub kind: LinkKind,
    pub weight: f32,
    pub label: u32,
}

impl Link {
    #[inline]
    pub fn new(target: usize, kind: LinkKind) -> Self {
        Self {
            target,
            kind,
            weight: 1.0,
            label: 0,
        }
    }

    #[inline]
    pub fn forward(target: usize) -> Self {
        Self::new(target, LinkKind::Forward)
    }

    #[inline]
    pub fn back(target: usize) -> Self {
        Self::new(target, LinkKind::Back)
    }

    #[inline]
    pub fn child(target: usize) -> Self {
        Self::new(target, LinkKind::Child)
    }

    #[inline]
    pub fn parent(target: usize) -> Self {
        Self::new(target, LinkKind::Parent)
    }

    #[inline]
    pub fn sibling(target: usize) -> Self {
        Self::new(target, LinkKind::Sibling)
    }

    #[inline]
    pub fn weighted(target: usize, kind: LinkKind, weight: f32) -> Self {
        Self {
            target,
            kind,
            weight,
            label: 0,
        }
    }

    #[inline]
    pub fn labeled(target: usize, kind: LinkKind, label: u32) -> Self {
        Self {
            target,
            kind,
            weight: 1.0,
            label,
        }
    }
}

impl Default for Link {
    fn default() -> Self {
        Self {
            target: 0,
            kind: LinkKind::Forward,
            weight: 1.0,
            label: 0,
        }
    }
}

impl Eq for Link {}

impl Hash for Link {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.target.hash(state);
        self.kind.hash(state);
        self.label.hash(state);
    }
}

// ============================================================================
// Links - Collection of edges from a node
// ============================================================================

/// Collection of links from an site to other sites.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct Links {
    pub edges: Vec<Link>,
    pub incoming: usize,
    pub depth: usize,
    pub terminal: bool,
    pub flags: u32,
}

impl Links {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            edges: Vec::with_capacity(capacity),
            ..Default::default()
        }
    }

    #[inline]
    pub fn add(&mut self, link: Link) {
        self.edges.push(link);
    }

    #[inline]
    pub fn add_forward(&mut self, target: usize) {
        self.edges.push(Link::forward(target));
    }

    #[inline]
    pub fn add_child(&mut self, target: usize) {
        self.edges.push(Link::child(target));
    }

    #[inline]
    pub fn add_parent(&mut self, target: usize) {
        self.edges.push(Link::parent(target));
    }

    pub fn targets(&self) -> impl Iterator<Item = usize> + '_ {
        self.edges.iter().map(|l| l.target)
    }

    pub fn targets_of_kind(&self, kind: LinkKind) -> impl Iterator<Item = usize> + '_ {
        self.edges
            .iter()
            .filter(move |l| l.kind == kind)
            .map(|l| l.target)
    }

    pub fn children(&self) -> impl Iterator<Item = usize> + '_ {
        self.targets_of_kind(LinkKind::Child)
    }

    pub fn parent(&self) -> Option<usize> {
        self.edges
            .iter()
            .find(|l| l.kind == LinkKind::Parent)
            .map(|l| l.target)
    }

    pub fn has_link_to(&self, target: usize) -> bool {
        self.edges.iter().any(|l| l.target == target)
    }

    pub fn remove_links_to(&mut self, target: usize) {
        self.edges.retain(|l| l.target != target);
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.edges.is_empty()
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.edges.len()
    }

    #[inline]
    pub fn is_leaf(&self) -> bool {
        !self.edges.iter().any(|l| l.kind == LinkKind::Child)
    }

    #[inline]
    pub fn is_root(&self) -> bool {
        !self.edges.iter().any(|l| l.kind == LinkKind::Parent)
    }
}

impl Eq for Links {}

impl Hash for Links {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.edges.hash(state);
        self.incoming.hash(state);
        self.depth.hash(state);
        self.terminal.hash(state);
        self.flags.hash(state);
    }
}

// ============================================================================
// Web - Combined data + graph structure
// ============================================================================

/// A unified site type that combines rich data values with graph connectivity.
/// Each variant stores (data, links) where links connect to other sites by index.
pub enum Entry {
    // Signed integers
    I8(i8, Links),
    I16(i16, Links),
    I32(i32, Links),
    I64(i64, Links),
    I128(i128, Links),
    Isize(isize, Links),
    // Unsigned integers
    U8(u8, Links),
    U16(u16, Links),
    U32(u32, Links),
    U64(u64, Links),
    U128(u128, Links),
    Usize(usize, Links),
    // Floats
    F32(f32, Links),
    F64(f64, Links),
    // Other primitives
    Char(char, Links),
    Bool(bool, Links),
    Unit(Links),
    // Complex types
    String(String, Links),
    Fn(fn(Entry) -> Entry, Links),
    PathBuf(PathBuf, Links),
    Range(Range<usize>, Links),
    Pair(Box<(Entry, Entry)>, Links),
    Vec(Vec<Entry>, Links),
    Box(Box<Entry>, Links),
    HashMap(HashMap<String, Entry>, Links),
    // Node/metadata types (for AST-like structures)
    Node {
        kind: String,
        name: String,
        attrs: HashMap<String, Entry>,
        links: Links,
    },
    // Conditional branch for dynamic tree structures
    Branch {
        /// Logical operation to combine left and right results
        operation: BranchOp,
        /// Left condition function
        left_fn: fn(&Entry) -> bool,
        /// Right condition function  
        right_fn: fn(&Entry) -> bool,
        /// Path to follow when condition evaluates to true
        true_path: Vec<Entry>,
        /// Path to follow when condition evaluates to false
        false_path: Vec<Entry>,
        /// Links for graph connectivity
        links: Links,
    },
}

impl Entry {
    // ========================================================================
    // Link Access
    // ========================================================================

    /// Get the links for this site.
    pub fn links(&self) -> &Links {
        match self {
            Self::I8(_, l)
            | Self::I16(_, l)
            | Self::I32(_, l)
            | Self::I64(_, l)
            | Self::I128(_, l)
            | Self::Isize(_, l)
            | Self::U8(_, l)
            | Self::U16(_, l)
            | Self::U32(_, l)
            | Self::U64(_, l)
            | Self::U128(_, l)
            | Self::Usize(_, l)
            | Self::F32(_, l)
            | Self::F64(_, l)
            | Self::Char(_, l)
            | Self::Bool(_, l)
            | Self::Unit(l)
            | Self::String(_, l)
            | Self::Fn(_, l)
            | Self::PathBuf(_, l)
            | Self::Range(_, l)
            | Self::Pair(_, l)
            | Self::Vec(_, l)
            | Self::Box(_, l)
            | Self::HashMap(_, l) => l,
            Self::Node { links, .. } | Self::Branch { links, .. } => links,
        }
    }

    /// Get mutable links for this site.
    pub fn links_mut(&mut self) -> &mut Links {
        match self {
            Self::I8(_, l)
            | Self::I16(_, l)
            | Self::I32(_, l)
            | Self::I64(_, l)
            | Self::I128(_, l)
            | Self::Isize(_, l)
            | Self::U8(_, l)
            | Self::U16(_, l)
            | Self::U32(_, l)
            | Self::U64(_, l)
            | Self::U128(_, l)
            | Self::Usize(_, l)
            | Self::F32(_, l)
            | Self::F64(_, l)
            | Self::Char(_, l)
            | Self::Bool(_, l)
            | Self::Unit(l)
            | Self::String(_, l)
            | Self::Fn(_, l)
            | Self::PathBuf(_, l)
            | Self::Range(_, l)
            | Self::Pair(_, l)
            | Self::Vec(_, l)
            | Self::Box(_, l)
            | Self::HashMap(_, l) => l,
            Self::Node { links, .. } | Self::Branch { links, .. } => links,
        }
    }

    /// Add a link to another site.
    #[inline]
    pub fn add_link(&mut self, link: Link) {
        self.links_mut().add(link);
    }

    /// Add a child link.
    #[inline]
    pub fn add_child(&mut self, target: usize) {
        self.links_mut().add_child(target);
    }

    /// Add a parent link.
    #[inline]
    pub fn add_parent(&mut self, target: usize) {
        self.links_mut().add_parent(target);
    }

    /// Get all child indices.
    pub fn children(&self) -> impl Iterator<Item = usize> + '_ {
        self.links().children()
    }

    /// Get parent index if any.
    pub fn parent(&self) -> Option<usize> {
        self.links().parent()
    }

    /// Get depth in tree/graph.
    #[inline]
    pub fn depth(&self) -> usize {
        self.links().depth
    }

    /// Set depth.
    #[inline]
    pub fn set_depth(&mut self, depth: usize) {
        self.links_mut().depth = depth;
    }

    /// Check if this is a terminal/leaf node.
    #[inline]
    pub fn is_terminal(&self) -> bool {
        self.links().terminal
    }

    /// Set terminal flag.
    #[inline]
    pub fn set_terminal(&mut self, terminal: bool) {
        self.links_mut().terminal = terminal;
    }

    // ========================================================================
    // Constructors (with empty links)
    // ========================================================================

    pub fn i8(v: i8) -> Self {
        Self::I8(v, Links::new())
    }
    pub fn i16(v: i16) -> Self {
        Self::I16(v, Links::new())
    }
    pub fn i32(v: i32) -> Self {
        Self::I32(v, Links::new())
    }
    pub fn i64(v: i64) -> Self {
        Self::I64(v, Links::new())
    }
    pub fn i128(v: i128) -> Self {
        Self::I128(v, Links::new())
    }
    pub fn isize(v: isize) -> Self {
        Self::Isize(v, Links::new())
    }
    pub fn u8(v: u8) -> Self {
        Self::U8(v, Links::new())
    }
    pub fn u16(v: u16) -> Self {
        Self::U16(v, Links::new())
    }
    pub fn u32(v: u32) -> Self {
        Self::U32(v, Links::new())
    }
    pub fn u64(v: u64) -> Self {
        Self::U64(v, Links::new())
    }
    pub fn u128(v: u128) -> Self {
        Self::U128(v, Links::new())
    }
    pub fn usize(v: usize) -> Self {
        Self::Usize(v, Links::new())
    }
    pub fn f32(v: f32) -> Self {
        Self::F32(v, Links::new())
    }
    pub fn f64(v: f64) -> Self {
        Self::F64(v, Links::new())
    }
    pub fn char(v: char) -> Self {
        Self::Char(v, Links::new())
    }
    pub fn bool(v: bool) -> Self {
        Self::Bool(v, Links::new())
    }
    pub fn unit() -> Self {
        Self::Unit(Links::new())
    }
    pub fn string(s: impl Into<String>) -> Self {
        Self::String(s.into(), Links::new())
    }
    pub fn path(p: impl Into<PathBuf>) -> Self {
        Self::PathBuf(p.into(), Links::new())
    }
    pub fn range(r: Range<usize>) -> Self {
        Self::Range(r, Links::new())
    }
    pub fn pair(a: Entry, b: Entry) -> Self {
        Self::Pair(Box::new((a, b)), Links::new())
    }
    pub fn vec(v: Vec<Entry>) -> Self {
        Self::Vec(v, Links::new())
    }
    pub fn boxed(e: Entry) -> Self {
        Self::Box(Box::new(e), Links::new())
    }
    pub fn hashmap(m: HashMap<String, Entry>) -> Self {
        Self::HashMap(m, Links::new())
    }

    /// Parse a string into an appropriate Entry type.
    /// Supports:
    /// - Array syntax: `[1, 2, 3]` → Entry::Vec with smallest fitting integer type
    /// - String arrays: `["a", "b", "c"]` → Entry::Vec<Entry::String>
    /// - HashMap tuples: `("key":"value"), ("k2":"v2")` → Entry::HashMap (colon separator)
    /// - Plain strings → Entry::String
    /// Newlines are ignored in array and hashmap parsing.
    pub fn parse_string(s: &str) -> Self {
        // Normalize: collapse all whitespace (including newlines) to single spaces
        let normalized: String = s.split_whitespace().collect::<Vec<_>>().join(" ");
        let trimmed = normalized.trim();
        
        // Check for array syntax: [...]
        if trimmed.starts_with('[') && trimmed.ends_with(']') {
            let inner = trimmed[1..trimmed.len()-1].trim();
            if inner.is_empty() {
                return Self::vec(vec![]);
            }
            
            // Try to parse as numeric array first
            if let Some(entries) = Self::parse_numeric_array(inner) {
                return Self::vec(entries);
            }
            
            // Try to parse as string array
            if let Some(entries) = Self::parse_string_array(inner) {
                return Self::vec(entries);
            }
        }
        
        // Check for key:value tuple syntax: ("key":value), ...
        // Single pair → Entry::Pair, multiple → Entry::HashMap
        if trimmed.starts_with('(') && trimmed.contains(')') {
            if let Some(pairs) = Self::parse_kv_tuples(trimmed) {
                if pairs.len() == 1 {
                    let (k, v) = pairs.into_iter().next().unwrap();
                    return Self::pair(Entry::string(k), v);
                } else {
                    return Self::hashmap(pairs.into_iter().collect());
                }
            }
        }
        
        // Default: plain string (use original, not normalized)
        Self::String(s.to_string(), Links::new())
    }

    /// Parse a comma-separated list of numbers into Entry values.
    /// Chooses the smallest integer type that fits all values.
    fn parse_numeric_array(inner: &str) -> Option<Vec<Entry>> {
        let parts: Vec<&str> = inner.split(',').map(|p| p.trim()).collect();
        let mut values: Vec<i128> = Vec::with_capacity(parts.len());
        let mut all_positive = true;
        
        for part in &parts {
            if part.is_empty() {
                return None;
            }
            match part.parse::<i128>() {
                Ok(n) => {
                    if n < 0 { all_positive = false; }
                    values.push(n);
                }
                Err(_) => return None,
            }
        }
        
        if values.is_empty() {
            return Some(vec![]);
        }
        
        Some(Self::values_to_smallest_int_entries(&values, all_positive))
    }

    /// Convert i128 values to Entry using smallest fitting integer type.
    fn values_to_smallest_int_entries(values: &[i128], all_positive: bool) -> Vec<Entry> {
        let min_val = *values.iter().min().unwrap();
        let max_val = *values.iter().max().unwrap();
        
        if all_positive {
            let max_u = max_val as u128;
            if max_u <= u8::MAX as u128 {
                values.iter().map(|&v| Entry::u8(v as u8)).collect()
            } else if max_u <= u16::MAX as u128 {
                values.iter().map(|&v| Entry::u16(v as u16)).collect()
            } else if max_u <= u32::MAX as u128 {
                values.iter().map(|&v| Entry::u32(v as u32)).collect()
            } else if max_u <= u64::MAX as u128 {
                values.iter().map(|&v| Entry::u64(v as u64)).collect()
            } else {
                values.iter().map(|&v| Entry::u128(v as u128)).collect()
            }
        } else {
            if min_val >= i8::MIN as i128 && max_val <= i8::MAX as i128 {
                values.iter().map(|&v| Entry::i8(v as i8)).collect()
            } else if min_val >= i16::MIN as i128 && max_val <= i16::MAX as i128 {
                values.iter().map(|&v| Entry::i16(v as i16)).collect()
            } else if min_val >= i32::MIN as i128 && max_val <= i32::MAX as i128 {
                values.iter().map(|&v| Entry::i32(v as i32)).collect()
            } else if min_val >= i64::MIN as i128 && max_val <= i64::MAX as i128 {
                values.iter().map(|&v| Entry::i64(v as i64)).collect()
            } else {
                values.iter().map(|&v| Entry::i128(v)).collect()
            }
        }
    }

    /// Convert a single i128 value to Entry using smallest fitting integer type.
    fn value_to_smallest_int_entry(v: i128) -> Entry {
        if v >= 0 {
            let u = v as u128;
            if u <= u8::MAX as u128 { Entry::u8(v as u8) }
            else if u <= u16::MAX as u128 { Entry::u16(v as u16) }
            else if u <= u32::MAX as u128 { Entry::u32(v as u32) }
            else if u <= u64::MAX as u128 { Entry::u64(v as u64) }
            else { Entry::u128(u) }
        } else {
            if v >= i8::MIN as i128 && v <= i8::MAX as i128 { Entry::i8(v as i8) }
            else if v >= i16::MIN as i128 && v <= i16::MAX as i128 { Entry::i16(v as i16) }
            else if v >= i32::MIN as i128 && v <= i32::MAX as i128 { Entry::i32(v as i32) }
            else if v >= i64::MIN as i128 && v <= i64::MAX as i128 { Entry::i64(v as i64) }
            else { Entry::i128(v) }
        }
    }

    /// Parse a comma-separated list of quoted strings into Entry::String values.
    fn parse_string_array(inner: &str) -> Option<Vec<Entry>> {
        let mut entries = Vec::new();
        let mut chars = inner.chars().peekable();
        
        while chars.peek().is_some() {
            // Skip whitespace
            while chars.peek().map(|c| c.is_whitespace()).unwrap_or(false) {
                chars.next();
            }
            
            // Expect opening quote
            let quote_char = match chars.next() {
                Some('"') => '"',
                Some('\'') => '\'',
                None => break,
                _ => return None,
            };
            
            // Collect string content until closing quote
            let mut content = String::new();
            loop {
                match chars.next() {
                    Some('\\') => {
                        match chars.next() {
                            Some('n') => content.push('\n'),
                            Some('t') => content.push('\t'),
                            Some('r') => content.push('\r'),
                            Some('\\') => content.push('\\'),
                            Some('"') => content.push('"'),
                            Some('\'') => content.push('\''),
                            Some(c) => { content.push('\\'); content.push(c); }
                            None => return None,
                        }
                    }
                    Some(c) if c == quote_char => break,
                    Some(c) => content.push(c),
                    None => return None,
                }
            }
            
            entries.push(Entry::string(content));
            
            // Skip whitespace after string
            while chars.peek().map(|c| c.is_whitespace()).unwrap_or(false) {
                chars.next();
            }
            
            // Expect comma or end
            match chars.peek() {
                Some(',') => { chars.next(); }
                None => break,
                _ => return None,
            }
        }
        
        if entries.is_empty() { None } else { Some(entries) }
    }

    /// Parse key-value tuples: ("key":"value"), ("k2":123), ...
    /// Uses colon as key-value separator to avoid confusion with regular tuples.
    /// Returns Vec of (String, Entry) pairs for flexible handling (Pair vs HashMap).
    fn parse_kv_tuples(s: &str) -> Option<Vec<(String, Entry)>> {
        let mut pairs = Vec::new();
        let mut chars = s.chars().peekable();
        
        loop {
            // Skip whitespace
            while chars.peek().map(|c| c.is_whitespace()).unwrap_or(false) {
                chars.next();
            }
            
            // Check for end
            if chars.peek().is_none() {
                break;
            }
            
            // Expect opening paren
            if chars.next() != Some('(') {
                return None;
            }
            
            // Skip whitespace
            while chars.peek().map(|c| c.is_whitespace()).unwrap_or(false) {
                chars.next();
            }
            
            // Parse key (string or number)
            let key = Self::parse_tuple_value(&mut chars)?;
            let key_str = match &key {
                Entry::String(s, _) => s.clone(),
                e => e.to_string_value()?,
            };
            
            // Skip whitespace
            while chars.peek().map(|c| c.is_whitespace()).unwrap_or(false) {
                chars.next();
            }
            
            // Expect colon (key:value separator)
            if chars.next() != Some(':') {
                return None;
            }
            
            // Skip whitespace
            while chars.peek().map(|c| c.is_whitespace()).unwrap_or(false) {
                chars.next();
            }
            
            // Parse value (string or number)
            let value = Self::parse_tuple_value(&mut chars)?;
            
            // Skip whitespace
            while chars.peek().map(|c| c.is_whitespace()).unwrap_or(false) {
                chars.next();
            }
            
            // Expect closing paren
            if chars.next() != Some(')') {
                return None;
            }
            
            pairs.push((key_str, value));
            
            // Skip whitespace
            while chars.peek().map(|c| c.is_whitespace()).unwrap_or(false) {
                chars.next();
            }
            
            // Expect comma or end
            match chars.peek() {
                Some(',') => { chars.next(); }
                None => break,
                _ => return None,
            }
        }
        
        if pairs.is_empty() { None } else { Some(pairs) }
    }

    /// Parse a single value in a tuple: either a quoted string or a number.
    fn parse_tuple_value(chars: &mut std::iter::Peekable<std::str::Chars>) -> Option<Entry> {
        match chars.peek()? {
            '"' | '\'' => {
                let quote_char = chars.next()?;
                let mut content = String::new();
                loop {
                    match chars.next()? {
                        '\\' => {
                            match chars.next()? {
                                'n' => content.push('\n'),
                                't' => content.push('\t'),
                                'r' => content.push('\r'),
                                '\\' => content.push('\\'),
                                '"' => content.push('"'),
                                '\'' => content.push('\''),
                                c => { content.push('\\'); content.push(c); }
                            }
                        }
                        c if c == quote_char => break,
                        c => content.push(c),
                    }
                }
                Some(Entry::string(content))
            }
            c if c.is_ascii_digit() || *c == '-' => {
                let mut num_str = String::new();
                while let Some(&c) = chars.peek() {
                    if c.is_ascii_digit() || c == '-' {
                        num_str.push(chars.next()?);
                    } else {
                        break;
                    }
                }
                let n: i128 = num_str.parse().ok()?;
                Some(Self::value_to_smallest_int_entry(n))
            }
            _ => None,
        }
    }
    pub fn set_name(mut self, n: impl Into<String>) -> bool {
        match self {
            Entry::Node { kind, name, attrs, links } => { self = Entry::Node { kind: kind.clone(),name: n.into(),attrs: attrs.clone(), links: links.clone() }; true }
            _ => false
        }
    }
    /// Create a Node site (for AST-like structures).
    pub fn node(kind: impl Into<String>, name: impl Into<String>) -> Self {
        Self::Node {
            kind: kind.into(),
            name: name.into(),
            attrs: HashMap::new(),
            links: Links::new(),
        }
    }
    /// Create a Node with attributes.
    pub fn node_with_attrs(
        kind: impl Into<String>,
        name: impl Into<String>,
        attrs: HashMap<String, Entry>,
    ) -> Self {
        Self::Node {
            kind: kind.into(),
            name: name.into(),
            attrs,
            links: Links::new(),
        }
    }

    /// Create a Branch entry for conditional tree structures.
    pub fn branch(
        operation: BranchOp,
        left_fn: fn(&Entry) -> bool,
        right_fn: fn(&Entry) -> bool,
        true_path: Vec<Entry>,
        false_path: Vec<Entry>,
    ) -> Self {
        Self::Branch {
            operation,
            left_fn,
            right_fn,
            true_path,
            false_path,
            links: Links::new(),
        }
    }

    /// Create a simple AND branch.
    pub fn branch_and(
        left_fn: fn(&Entry) -> bool,
        right_fn: fn(&Entry) -> bool,
        true_path: Vec<Entry>,
        false_path: Vec<Entry>,
    ) -> Self {
        Self::branch(BranchOp::And, left_fn, right_fn, true_path, false_path)
    }

    /// Create a simple OR branch.
    pub fn branch_or(
        left_fn: fn(&Entry) -> bool,
        right_fn: fn(&Entry) -> bool,
        true_path: Vec<Entry>,
        false_path: Vec<Entry>,
    ) -> Self {
        Self::branch(BranchOp::Or, left_fn, right_fn, true_path, false_path)
    }

    // ========================================================================
    // Node-specific methods
    // ========================================================================

    /// Get node kind (if this is a Node).
    pub fn kind(&self) -> Option<&str> {
        match self {
            Self::Node { kind, .. } => Some(kind),
            _ => None,
        }
    }

    /// Get node name (if this is a Node).
    pub fn name(&self) -> Option<&str> {
        match self {
            Self::Node { name, .. } => Some(name),
            _ => None,
        }
    }

    /// Get attribute (if this is a Node).
    pub fn attr(&self, key: &str) -> Option<&Entry> {
        match self {
            Self::Node { attrs, .. } => attrs.get(key),
            _ => None,
        }
    }

    /// Set attribute (if this is a Node).
    pub fn set_attr(&mut self, key: impl Into<String>, value: Entry) -> bool {
        match self {
            Self::Node { attrs, .. } => {
                attrs.insert(key.into(), value);
                true
            }
            _ => false,
        }
    }

    /// Set attribute from an Option - only sets if Some.
    /// Works with Option<String>, Option<&str>, Option<&String>, etc.
    pub fn set_attr_opt<S: AsRef<str>>(&mut self, key: impl Into<String>, value: Option<S>) -> bool {
        if let Some(v) = value {
            self.set_attr(key, Entry::string(v.as_ref()))
        } else {
            false
        }
    }

    /// Get all attributes (if this is a Node).
    pub fn attrs(&self) -> Option<&HashMap<String, Entry>> {
        match self {
            Self::Node { attrs, .. } => Some(attrs),
            _ => None,
        }
    }

    // ========================================================================
    // Type checking
    // ========================================================================

    pub fn is_numeric(&self) -> bool {
        matches!(
            self,
            Self::I8(..)
                | Self::I16(..)
                | Self::I32(..)
                | Self::I64(..)
                | Self::I128(..)
                | Self::Isize(..)
                | Self::U8(..)
                | Self::U16(..)
                | Self::U32(..)
                | Self::U64(..)
                | Self::U128(..)
                | Self::Usize(..)
                | Self::F32(..)
                | Self::F64(..)
        )
    }

    pub fn is_integer(&self) -> bool {
        matches!(
            self,
            Self::I8(..)
                | Self::I16(..)
                | Self::I32(..)
                | Self::I64(..)
                | Self::I128(..)
                | Self::Isize(..)
                | Self::U8(..)
                | Self::U16(..)
                | Self::U32(..)
                | Self::U64(..)
                | Self::U128(..)
                | Self::Usize(..)
        )
    }

    pub fn is_node(&self) -> bool {
        matches!(self, Self::Node { .. })
    }

    pub fn is_branch(&self) -> bool {
        matches!(self, Self::Branch { .. })
    }

    /// Evaluate branch condition and return the appropriate path.
    pub fn evaluate_branch(&self, context: &Entry) -> Option<&Vec<Entry>> {
        match self {
            Self::Branch {
                operation,
                left_fn,
                right_fn,
                true_path,
                false_path,
                ..
            } => {
                let left_result = left_fn(context);
                let right_result = right_fn(context);
                if operation.evaluate(left_result, right_result) {
                    Some(true_path)
                } else {
                    Some(false_path)
                }
            }
            _ => None,
        }
    }

    /// Get true_path from a Branch (if this is a Branch).
    pub fn true_path(&self) -> Option<&Vec<Entry>> {
        match self {
            Self::Branch { true_path, .. } => Some(true_path),
            _ => None,
        }
    }

    /// Get false_path from a Branch (if this is a Branch).
    pub fn false_path(&self) -> Option<&Vec<Entry>> {
        match self {
            Self::Branch { false_path, .. } => Some(false_path),
            _ => None,
        }
    }

    pub fn is_container(&self) -> bool {
        matches!(
            self,
            Self::Vec(..)
                | Self::HashMap(..)
                | Self::Pair(..)
                | Self::Node { .. }
                | Self::Branch { .. }
        )
    }

    pub fn type_name(&self) -> &'static str {
        match self {
            Self::I8(..) => "i8",
            Self::I16(..) => "i16",
            Self::I32(..) => "i32",
            Self::I64(..) => "i64",
            Self::I128(..) => "i128",
            Self::Isize(..) => "isize",
            Self::U8(..) => "u8",
            Self::U16(..) => "u16",
            Self::U32(..) => "u32",
            Self::U64(..) => "u64",
            Self::U128(..) => "u128",
            Self::Usize(..) => "usize",
            Self::F32(..) => "f32",
            Self::F64(..) => "f64",
            Self::Char(..) => "char",
            Self::Bool(..) => "bool",
            Self::Unit(..) => "()",
            Self::String(..) => "String",
            Self::Fn(..) => "fn",
            Self::PathBuf(..) => "PathBuf",
            Self::Range(..) => "Range",
            Self::Pair(..) => "Pair",
            Self::Vec(..) => "Vec",
            Self::Box(..) => "Box",
            Self::HashMap(..) => "HashMap",
            Self::Node { .. } => "Node",
            Self::Branch { .. } => "Branch",
        }
    }
}

// ============================================================================
// Trait Implementations
// ============================================================================

impl Debug for Entry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::I8(v, l) => f.debug_tuple("I8").field(v).field(&l.len()).finish(),
            Self::I16(v, l) => f.debug_tuple("I16").field(v).field(&l.len()).finish(),
            Self::I32(v, l) => f.debug_tuple("I32").field(v).field(&l.len()).finish(),
            Self::I64(v, l) => f.debug_tuple("I64").field(v).field(&l.len()).finish(),
            Self::I128(v, l) => f.debug_tuple("I128").field(v).field(&l.len()).finish(),
            Self::Isize(v, l) => f.debug_tuple("Isize").field(v).field(&l.len()).finish(),
            Self::U8(v, l) => f.debug_tuple("U8").field(v).field(&l.len()).finish(),
            Self::U16(v, l) => f.debug_tuple("U16").field(v).field(&l.len()).finish(),
            Self::U32(v, l) => f.debug_tuple("U32").field(v).field(&l.len()).finish(),
            Self::U64(v, l) => f.debug_tuple("U64").field(v).field(&l.len()).finish(),
            Self::U128(v, l) => f.debug_tuple("U128").field(v).field(&l.len()).finish(),
            Self::Usize(v, l) => f.debug_tuple("Usize").field(v).field(&l.len()).finish(),
            Self::F32(v, l) => f.debug_tuple("F32").field(v).field(&l.len()).finish(),
            Self::F64(v, l) => f.debug_tuple("F64").field(v).field(&l.len()).finish(),
            Self::Char(v, l) => f.debug_tuple("Char").field(v).field(&l.len()).finish(),
            Self::Bool(v, l) => f.debug_tuple("Bool").field(v).field(&l.len()).finish(),
            Self::Unit(l) => f.debug_tuple("Unit").field(&l.len()).finish(),
            Self::String(v, l) => f.debug_tuple("String").field(v).field(&l.len()).finish(),
            Self::Fn(_, l) => f
                .debug_tuple("Fn")
                .field(&"<function>")
                .field(&l.len())
                .finish(),
            Self::PathBuf(v, l) => f.debug_tuple("PathBuf").field(v).field(&l.len()).finish(),
            Self::Range(v, l) => f.debug_tuple("Range").field(v).field(&l.len()).finish(),
            Self::Pair(v, l) => f.debug_tuple("Pair").field(v).field(&l.len()).finish(),
            Self::Vec(v, l) => f
                .debug_tuple("Vec")
                .field(&v.len())
                .field(&l.len())
                .finish(),
            Self::Box(v, l) => f.debug_tuple("Box").field(v).field(&l.len()).finish(),
            Self::HashMap(v, l) => f
                .debug_tuple("HashMap")
                .field(&v.len())
                .field(&l.len())
                .finish(),
            Self::Node {
                kind,
                name,
                attrs,
                links,
            } => f
                .debug_struct("Node")
                .field("kind", kind)
                .field("name", name)
                .field("attrs", &attrs.len())
                .field("links", &links.len())
                .finish(),
            Self::Branch {
                operation,
                true_path,
                false_path,
                links,
                ..
            } => f
                .debug_struct("Branch")
                .field("operation", &operation.name())
                .field("true_path", &true_path.len())
                .field("false_path", &false_path.len())
                .field("links", &links.len())
                .finish(),
        }
    }
}

impl Display for Entry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::I8(v, _) => write!(f, "{}", v),
            Self::I16(v, _) => write!(f, "{}", v),
            Self::I32(v, _) => write!(f, "{}", v),
            Self::I64(v, _) => write!(f, "{}", v),
            Self::I128(v, _) => write!(f, "{}", v),
            Self::Isize(v, _) => write!(f, "{}", v),
            Self::U8(v, _) => write!(f, "{}", v),
            Self::U16(v, _) => write!(f, "{}", v),
            Self::U32(v, _) => write!(f, "{}", v),
            Self::U64(v, _) => write!(f, "{}", v),
            Self::U128(v, _) => write!(f, "{}", v),
            Self::Usize(v, _) => write!(f, "{}", v),
            Self::F32(v, _) => write!(f, "{}", v),
            Self::F64(v, _) => write!(f, "{}", v),
            Self::Char(v, _) => write!(f, "{}", v),
            Self::Bool(v, _) => write!(f, "{}", v),
            Self::Unit(_) => write!(f, "()"),
            Self::String(v, _) => write!(f, "{}", v),
            Self::Fn(_, _) => write!(f, "<function>"),
            Self::PathBuf(v, _) => write!(f, "{}", v.display()),
            Self::Range(v, _) => write!(f, "{}..{}", v.start, v.end),
            Self::Pair(v, _) => write!(f, "({}, {})", v.0, v.1),
            Self::Vec(v, _) => {
                write!(f, "[")?;
                for (i, item) in v.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", item)?;
                }
                write!(f, "]")
            }
            Self::Box(v, _) => write!(f, "{}", v),
            Self::HashMap(v, _) => {
                write!(f, "{{")?;
                for (i, (k, val)) in v.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", k, val)?;
                }
                write!(f, "}}")
            }
            Self::Node { kind, name, .. } => write!(f, "{}:{}", kind, name),
            Self::Branch {
                operation,
                true_path,
                false_path,
                ..
            } => write!(
                f,
                "Branch({}, true:{}, false:{})",
                operation.name(),
                true_path.len(),
                false_path.len()
            ),
        }
    }
}

impl Clone for Entry {
    fn clone(&self) -> Self {
        match self {
            Self::I8(v, l) => Self::I8(*v, l.clone()),
            Self::I16(v, l) => Self::I16(*v, l.clone()),
            Self::I32(v, l) => Self::I32(*v, l.clone()),
            Self::I64(v, l) => Self::I64(*v, l.clone()),
            Self::I128(v, l) => Self::I128(*v, l.clone()),
            Self::Isize(v, l) => Self::Isize(*v, l.clone()),
            Self::U8(v, l) => Self::U8(*v, l.clone()),
            Self::U16(v, l) => Self::U16(*v, l.clone()),
            Self::U32(v, l) => Self::U32(*v, l.clone()),
            Self::U64(v, l) => Self::U64(*v, l.clone()),
            Self::U128(v, l) => Self::U128(*v, l.clone()),
            Self::Usize(v, l) => Self::Usize(*v, l.clone()),
            Self::F32(v, l) => Self::F32(*v, l.clone()),
            Self::F64(v, l) => Self::F64(*v, l.clone()),
            Self::Char(v, l) => Self::Char(*v, l.clone()),
            Self::Bool(v, l) => Self::Bool(*v, l.clone()),
            Self::Unit(l) => Self::Unit(l.clone()),
            Self::String(v, l) => Self::String(v.clone(), l.clone()),
            Self::Fn(v, l) => Self::Fn(*v, l.clone()),
            Self::PathBuf(v, l) => Self::PathBuf(v.clone(), l.clone()),
            Self::Range(v, l) => Self::Range(v.clone(), l.clone()),
            Self::Pair(v, l) => Self::Pair(v.clone(), l.clone()),
            Self::Vec(v, l) => Self::Vec(v.clone(), l.clone()),
            Self::Box(v, l) => Self::Box(v.clone(), l.clone()),
            Self::HashMap(v, l) => Self::HashMap(v.clone(), l.clone()),
            Self::Node {
                kind,
                name,
                attrs,
                links,
            } => Self::Node {
                kind: kind.clone(),
                name: name.clone(),
                attrs: attrs.clone(),
                links: links.clone(),
            },
            Self::Branch {
                operation,
                left_fn,
                right_fn,
                true_path,
                false_path,
                links,
            } => Self::Branch {
                operation: *operation,
                left_fn: *left_fn,
                right_fn: *right_fn,
                true_path: true_path.clone(),
                false_path: false_path.clone(),
                links: links.clone(),
            },
        }
    }
}

impl PartialEq for Entry {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::I8(a, _), Self::I8(b, _)) => a == b,
            (Self::I16(a, _), Self::I16(b, _)) => a == b,
            (Self::I32(a, _), Self::I32(b, _)) => a == b,
            (Self::I64(a, _), Self::I64(b, _)) => a == b,
            (Self::I128(a, _), Self::I128(b, _)) => a == b,
            (Self::Isize(a, _), Self::Isize(b, _)) => a == b,
            (Self::U8(a, _), Self::U8(b, _)) => a == b,
            (Self::U16(a, _), Self::U16(b, _)) => a == b,
            (Self::U32(a, _), Self::U32(b, _)) => a == b,
            (Self::U64(a, _), Self::U64(b, _)) => a == b,
            (Self::U128(a, _), Self::U128(b, _)) => a == b,
            (Self::Usize(a, _), Self::Usize(b, _)) => a == b,
            (Self::F32(a, _), Self::F32(b, _)) => a == b,
            (Self::F64(a, _), Self::F64(b, _)) => a == b,
            (Self::Char(a, _), Self::Char(b, _)) => a == b,
            (Self::Bool(a, _), Self::Bool(b, _)) => a == b,
            (Self::Unit(_), Self::Unit(_)) => true,
            (Self::String(a, _), Self::String(b, _)) => a == b,
            (Self::Fn(_, _), Self::Fn(_, _)) => false,
            (Self::PathBuf(a, _), Self::PathBuf(b, _)) => a == b,
            (Self::Range(a, _), Self::Range(b, _)) => a == b,
            (Self::Pair(a, _), Self::Pair(b, _)) => a == b,
            (Self::Vec(a, _), Self::Vec(b, _)) => a == b,
            (Self::Box(a, _), Self::Box(b, _)) => a == b,
            (Self::HashMap(a, _), Self::HashMap(b, _)) => a == b,
            (
                Self::Node {
                    kind: k1, name: n1, ..
                },
                Self::Node {
                    kind: k2, name: n2, ..
                },
            ) => k1 == k2 && n1 == n2,
            _ => false,
        }
    }
}

impl Eq for Entry {}

impl Hash for Entry {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            Self::I8(v, _) => v.hash(state),
            Self::I16(v, _) => v.hash(state),
            Self::I32(v, _) => v.hash(state),
            Self::I64(v, _) => v.hash(state),
            Self::I128(v, _) => v.hash(state),
            Self::Isize(v, _) => v.hash(state),
            Self::U8(v, _) => v.hash(state),
            Self::U16(v, _) => v.hash(state),
            Self::U32(v, _) => v.hash(state),
            Self::U64(v, _) => v.hash(state),
            Self::U128(v, _) => v.hash(state),
            Self::Usize(v, _) => v.hash(state),
            Self::F32(v, _) => v.to_bits().hash(state),
            Self::F64(v, _) => v.to_bits().hash(state),
            Self::Char(v, _) => v.hash(state),
            Self::Bool(v, _) => v.hash(state),
            Self::Unit(_) => {}
            Self::String(v, _) => v.hash(state),
            Self::Fn(v, _) => v.hash(state),
            Self::PathBuf(v, _) => v.hash(state),
            Self::Range(v, _) => {
                v.start.hash(state);
                v.end.hash(state);
            }
            Self::Pair(v, _) => v.hash(state),
            Self::Vec(v, _) => v.hash(state),
            Self::Box(v, _) => v.hash(state),
            Self::HashMap(v, _) => {
                v.len().hash(state);
                for (k, val) in v.iter() {
                    k.hash(state);
                    val.hash(state);
                }
            }
            Self::Node { kind, name, .. } => {
                kind.hash(state);
                name.hash(state);
            }
            Self::Branch {
                operation,
                true_path,
                false_path,
                ..
            } => {
                operation.hash(state);
                true_path.hash(state);
                false_path.hash(state);
            }
        }
    }
}

impl Default for Entry {
    fn default() -> Self {
        Self::Unit(Links::new())
    }
}

// ============================================================================
// Site - Storage for Entry nodes
// ============================================================================

/// Storage for Entry nodes with graph connectivity.
#[derive(Debug, Clone, Default)]
pub struct Site {
    sites: Vec<Entry>,
    roots: Vec<usize>,
    name: String,
    metadata: HashMap<String, Entry>,
}

impl Site {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn named(name: impl Into<String>) -> Self {
        Self {
            sites: Vec::new(),
            roots: Vec::new(),
            name: name.into(),
            metadata: HashMap::new(),
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            sites: Vec::with_capacity(capacity),
            roots: Vec::new(),
            name: String::new(),
            metadata: HashMap::new(),
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.sites.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.sites.is_empty()
    }

    #[inline]
    pub fn name(&self) -> &str {
        &self.name
    }

    #[inline]
    pub fn roots(&self) -> &[usize] {
        &self.roots
    }

    /// Add an site and return its index.
    pub fn add(&mut self, site: Entry) -> usize {
        let idx = self.sites.len();
        self.sites.push(site);
        idx
    }

    /// Add an site as a root.
    pub fn add_root(&mut self, site: Entry) -> usize {
        let idx = self.add(site);
        self.roots.push(idx);
        idx
    }

    /// Get site by index.
    #[inline]
    pub fn get(&self, index: usize) -> Option<&Entry> {
        self.sites.get(index)
    }

    /// Get mutable site by index.
    #[inline]
    pub fn get_mut(&mut self, index: usize) -> Option<&mut Entry> {
        self.sites.get_mut(index)
    }

    /// Add a child site to a parent.
    pub fn add_child_to(&mut self, parent: usize, child: Entry) -> Option<usize> {
        if parent >= self.sites.len() {
            return None;
        }
        let child_idx = self.add(child);
        // Add child link to parent
        self.sites[parent].add_child(child_idx);
        // Add parent link to child
        self.sites[child_idx].add_parent(parent);
        // Set depth
        let parent_depth = self.sites[parent].depth();
        self.sites[child_idx].set_depth(parent_depth + 1);
        Some(child_idx)
    }

    /// Link two sites.
    pub fn link(&mut self, from: usize, to: usize, kind: LinkKind) -> bool {
        if from >= self.sites.len() || to >= self.sites.len() {
            return false;
        }
        self.sites[from].add_link(Link::new(to, kind));
        self.sites[to].links_mut().incoming += 1;
        true
    }

    /// Iterate over all sites.
    pub fn iter(&self) -> impl Iterator<Item = &Entry> {
        self.sites.iter()
    }

    /// Iterate over all sites mutably.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Entry> {
        self.sites.iter_mut()
    }

    /// Get all children of an site.
    pub fn children_of(&self, index: usize) -> Vec<usize> {
        self.sites
            .get(index)
            .map(|e| e.children().collect())
            .unwrap_or_default()
    }

    /// Traverse depth-first from roots.
    pub fn traverse_dfs(&self) -> Vec<usize> {
        let mut visited = vec![false; self.sites.len()];
        let mut result = Vec::new();

        for &root in &self.roots {
            self.dfs_helper(root, &mut visited, &mut result);
        }

        result
    }

    fn dfs_helper(&self, index: usize, visited: &mut [bool], result: &mut Vec<usize>) {
        if index >= visited.len() || visited[index] {
            return;
        }
        visited[index] = true;
        result.push(index);

        if let Some(site) = self.sites.get(index) {
            for child in site.children() {
                self.dfs_helper(child, visited, result);
            }
        }
    }
}

impl std::ops::Index<usize> for Site {
    type Output = Entry;

    fn index(&self, index: usize) -> &Self::Output {
        &self.sites[index]
    }
}

impl std::ops::IndexMut<usize> for Site {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.sites[index]
    }
}

// ============================================================================
// Node Kind Constants (replaces IFNodeKind enum for flexibility)
// ============================================================================

pub mod node_kind {
    // Top-level declarations
    pub const FUNCTION: &str = "Function";
    pub const STRUCT: &str = "Struct";
    pub const ENUM: &str = "Enum";
    pub const TYPEDEF: &str = "Typedef";
    pub const TYPE_ALIAS: &str = "TypeAlias";
    pub const GLOBAL: &str = "Global";
    pub const VARIABLE: &str = "Variable";
    pub const ARRAY: &str = "Array";
    pub const MACRO: &str = "Macro";
    pub const INCLUDE: &str = "Include";
    pub const COMMENT: &str = "Comment";
    pub const LOOP: &str = "Loop";
    pub const TYPE: &str = "Type";

    // Function components
    pub const PARAMETER: &str = "Parameter";
    pub const RETURN_TYPE: &str = "ReturnType";
    pub const FUNCTION_BODY: &str = "FunctionBody";

    // Struct components
    pub const FIELD: &str = "Field";

    // Enum components
    pub const VARIANT: &str = "Variant";
    pub const ENUM_VARIANT: &str = "EnumVariant";

    // Statements
    pub const VAR_DECL: &str = "VariableDeclaration";
    pub const ASSIGNMENT: &str = "Assignment";
    pub const EXPRESSION: &str = "Expression";
    pub const IF_STMT: &str = "IfStatement";
    pub const WHILE_LOOP: &str = "WhileLoop";
    pub const FOR_LOOP: &str = "ForLoop";
    pub const DO_WHILE: &str = "DoWhile";
    pub const SWITCH: &str = "Switch";
    pub const CASE: &str = "Case";
    pub const RETURN: &str = "Return";
    pub const BREAK: &str = "Break";
    pub const CONTINUE: &str = "Continue";
    pub const FUNC_CALL: &str = "FunctionCall";

    // Control flow
    pub const IF: &str = "If";
    pub const ELSE: &str = "Else";
    pub const WHILE: &str = "While";
    pub const FOR: &str = "For";
    pub const DO: &str = "Do";
    pub const GOTO: &str = "Goto";

    // Expressions
    pub const BINARY_OP: &str = "BinaryOp";
    pub const UNARY_OP: &str = "UnaryOp";
    pub const LITERAL: &str = "Literal";
    pub const IDENTIFIER: &str = "Identifier";
    pub const CAST: &str = "Cast";

    // Composite
    pub const BLOCK: &str = "Block";
    pub const ANNOTATION: &str = "Annotation";
    pub const SAMPLE: &str = "Sample";
    pub const UNKNOWN: &str = "Unknown";
}

// ============================================================================
// Statistics for Web
// ============================================================================

/// Statistics about an Web structure
#[derive(Debug, Clone, Default)]
pub struct SiteStats {
    pub total_sites: usize,
    pub max_depth: usize,
    pub total_links: usize,
    pub node_count: usize,
    pub type_counts: HashMap<String, usize>,
    pub kind_counts: HashMap<String, usize>,
}

impl Entry {
    /// Calculate statistics for this site and its descendants (via Vec/HashMap children).
    pub fn stats(&self) -> SiteStats {
        let mut stats = SiteStats::default();
        self.collect_stats(&mut stats, 0);
        stats
    }

    fn collect_stats(&self, stats: &mut SiteStats, depth: usize) {
        stats.total_sites += 1;
        stats.max_depth = stats.max_depth.max(depth);
        stats.total_links += self.links().len();

        // Count by type
        *stats
            .type_counts
            .entry(self.type_name().to_string())
            .or_insert(0) += 1;

        // If it's a Node, count by kind
        if let Some(kind) = self.kind() {
            stats.node_count += 1;
            *stats.kind_counts.entry(kind.to_string()).or_insert(0) += 1;
        }

        // Recurse into containers
        match self {
            Self::Vec(v, _) => {
                for item in v {
                    item.collect_stats(stats, depth + 1);
                }
            }
            Self::HashMap(m, _) => {
                for (_, v) in m {
                    v.collect_stats(stats, depth + 1);
                }
            }
            Self::Pair(p, _) => {
                p.0.collect_stats(stats, depth + 1);
                p.1.collect_stats(stats, depth + 1);
            }
            Self::Box(inner, _) => {
                inner.collect_stats(stats, depth + 1);
            }
            Self::Node { attrs, .. } => {
                for (_, v) in attrs {
                    v.collect_stats(stats, depth + 1);
                }
            }
            _ => {}
        }
    }

    /// Count total sites recursively.
    pub fn count(&self) -> usize {
        let mut count = 1;
        match self {
            Self::Vec(v, _) => count += v.iter().map(|e| e.count()).sum::<usize>(),
            Self::HashMap(m, _) => count += m.values().map(|e| e.count()).sum::<usize>(),
            Self::Pair(p, _) => count += p.0.count() + p.1.count(),
            Self::Box(inner, _) => count += inner.count(),
            Self::Node { attrs, .. } => count += attrs.values().map(|e| e.count()).sum::<usize>(),
            _ => {}
        }
        count
    }

    /// Calculate max nesting depth.
    pub fn max_depth(&self) -> usize {
        self.calc_depth(0)
    }

    fn calc_depth(&self, current: usize) -> usize {
        match self {
            Self::Vec(v, _) => v
                .iter()
                .map(|e| e.calc_depth(current + 1))
                .max()
                .unwrap_or(current),
            Self::HashMap(m, _) => m
                .values()
                .map(|e| e.calc_depth(current + 1))
                .max()
                .unwrap_or(current),
            Self::Pair(p, _) => p.0.calc_depth(current + 1).max(p.1.calc_depth(current + 1)),
            Self::Box(inner, _) => inner.calc_depth(current + 1),
            Self::Node { attrs, .. } => attrs
                .values()
                .map(|e| e.calc_depth(current + 1))
                .max()
                .unwrap_or(current),
            _ => current,
        }
    }

    /// Find all sites matching a predicate.
    pub fn find_all<F>(&self, predicate: F) -> Vec<&Entry>
    where
        F: Fn(&Entry) -> bool + Copy,
    {
        let mut results = Vec::new();
        self.find_recursive(&predicate, &mut results);
        results
    }

    fn find_recursive<'a, F>(&'a self, predicate: &F, results: &mut Vec<&'a Entry>)
    where
        F: Fn(&Entry) -> bool,
    {
        if predicate(self) {
            results.push(self);
        }
        match self {
            Self::Vec(v, _) => v.iter().for_each(|e| e.find_recursive(predicate, results)),
            Self::HashMap(m, _) => m
                .values()
                .for_each(|e| e.find_recursive(predicate, results)),
            Self::Pair(p, _) => {
                p.0.find_recursive(predicate, results);
                p.1.find_recursive(predicate, results);
            }
            Self::Box(inner, _) => inner.find_recursive(predicate, results),
            Self::Node { attrs, .. } => attrs
                .values()
                .for_each(|e| e.find_recursive(predicate, results)),
            _ => {}
        }
    }

    /// Find all nodes by kind.
    pub fn find_by_kind(&self, kind: &str) -> Vec<&Entry> {
        self.find_all(|e| e.kind() == Some(kind))
    }

    /// Find all nodes by name pattern (case-insensitive contains).
    pub fn find_by_name(&self, pattern: &str) -> Vec<&Entry> {
        let pattern_lower = pattern.to_lowercase();
        self.find_all(|e| {
            e.name()
                .map(|n| n.to_lowercase().contains(&pattern_lower))
                .unwrap_or(false)
        })
    }

    /// Find all nodes with a specific attribute.
    pub fn find_with_attr(&self, attr_key: &str) -> Vec<&Entry> {
        self.find_all(|e| e.attr(attr_key).is_some())
    }

    /// Get a string attribute from a Node.
    pub fn get_string_attr(&self, key: &str) -> Option<&str> {
        match self.attr(key)? {
            Entry::String(s, _) => Some(s.as_str()),
            _ => None,
        }
    }

    /// Get a bool attribute from a Node.
    pub fn get_bool_attr(&self, key: &str) -> Option<bool> {
        match self.attr(key)? {
            Entry::Bool(b, _) => Some(*b),
            _ => None,
        }
    }

    /// Get a numeric attribute as f64 from a Node.
    pub fn get_number_attr(&self, key: &str) -> Option<f64> {
        match self.attr(key)? {
            Entry::F64(n, _) => Some(*n),
            Entry::F32(n, _) => Some(*n as f64),
            Entry::I64(n, _) => Some(*n as f64),
            Entry::I32(n, _) => Some(*n as f64),
            Entry::U64(n, _) => Some(*n as f64),
            Entry::U32(n, _) => Some(*n as f64),
            _ => None,
        }
    }

    /// Get an array attribute from a Node.
    pub fn get_vec_attr(&self, key: &str) -> Option<&Vec<Entry>> {
        match self.attr(key)? {
            Entry::Vec(v, _) => Some(v),
            _ => None,
        }
    }

    /// Get an i64 attribute from a Node.
    pub fn get_i64_attr(&self, key: &str) -> Option<i64> {
        match self.attr(key)? {
            Entry::I64(n, _) => Some(*n),
            Entry::I32(n, _) => Some(*n as i64),
            Entry::I16(n, _) => Some(*n as i64),
            Entry::I8(n, _) => Some(*n as i64),
            Entry::Isize(n, _) => Some(*n as i64),
            _ => None,
        }
    }

    /// Get a usize attribute from a Node.
    pub fn get_usize_attr(&self, key: &str) -> Option<usize> {
        match self.attr(key)? {
            Entry::Usize(n, _) => Some(*n),
            Entry::U64(n, _) => Some(*n as usize),
            Entry::U32(n, _) => Some(*n as usize),
            Entry::U16(n, _) => Some(*n as usize),
            Entry::U8(n, _) => Some(*n as usize),
            _ => None,
        }
    }

    /// Get attribute by key (alias for attr).
    pub fn get_attr(&self, key: &str) -> Option<&Entry> {
        self.attr(key)
    }

    /// Get string value if this is a String variant.
    pub fn as_str(&self) -> Option<&str> {
        match self {
            Entry::String(s, _) => Some(s.as_str()),
            _ => None,
        }
    }

    // ========================================================================
    // as_*() - Borrow inner value (returns Option<&T>)
    // ========================================================================

    pub fn as_i8(&self) -> Option<i8> {
        match self {
            Self::I8(v, _) => Some(*v),
            _ => None,
        }
    }
    pub fn as_i16(&self) -> Option<i16> {
        match self {
            Self::I16(v, _) => Some(*v),
            _ => None,
        }
    }
    pub fn as_i32(&self) -> Option<i32> {
        match self {
            Self::I32(v, _) => Some(*v),
            _ => None,
        }
    }
    pub fn as_i64(&self) -> Option<i64> {
        match self {
            Self::I64(v, _) => Some(*v),
            _ => None,
        }
    }
    pub fn as_i128(&self) -> Option<i128> {
        match self {
            Self::I128(v, _) => Some(*v),
            _ => None,
        }
    }
    pub fn as_isize(&self) -> Option<isize> {
        match self {
            Self::Isize(v, _) => Some(*v),
            _ => None,
        }
    }
    pub fn as_u8(&self) -> Option<u8> {
        match self {
            Self::U8(v, _) => Some(*v),
            _ => None,
        }
    }
    pub fn as_u16(&self) -> Option<u16> {
        match self {
            Self::U16(v, _) => Some(*v),
            _ => None,
        }
    }
    pub fn as_u32(&self) -> Option<u32> {
        match self {
            Self::U32(v, _) => Some(*v),
            _ => None,
        }
    }
    pub fn as_u64(&self) -> Option<u64> {
        match self {
            Self::U64(v, _) => Some(*v),
            _ => None,
        }
    }
    pub fn as_u128(&self) -> Option<u128> {
        match self {
            Self::U128(v, _) => Some(*v),
            _ => None,
        }
    }
    pub fn as_usize(&self) -> Option<usize> {
        match self {
            Self::Usize(v, _) => Some(*v),
            _ => None,
        }
    }
    pub fn as_f32(&self) -> Option<f32> {
        match self {
            Self::F32(v, _) => Some(*v),
            _ => None,
        }
    }
    pub fn as_f64(&self) -> Option<f64> {
        match self {
            Self::F64(v, _) => Some(*v),
            _ => None,
        }
    }
    pub fn as_char(&self) -> Option<char> {
        match self {
            Self::Char(v, _) => Some(*v),
            _ => None,
        }
    }
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Self::Bool(v, _) => Some(*v),
            _ => None,
        }
    }
    pub fn as_path(&self) -> Option<&PathBuf> {
        match self {
            Self::PathBuf(v, _) => Some(v),
            _ => None,
        }
    }
    pub fn as_range(&self) -> Option<&Range<usize>> {
        match self {
            Self::Range(v, _) => Some(v),
            _ => None,
        }
    }
    pub fn as_pair(&self) -> Option<&(Entry, Entry)> {
        match self {
            Self::Pair(v, _) => Some(v.as_ref()),
            _ => None,
        }
    }
    pub fn as_vec(&self) -> Option<&Vec<Entry>> {
        match self {
            Self::Vec(v, _) => Some(v),
            _ => None,
        }
    }
    pub fn as_boxed(&self) -> Option<&Entry> {
        match self {
            Self::Box(v, _) => Some(v.as_ref()),
            _ => None,
        }
    }
    pub fn as_hashmap(&self) -> Option<&HashMap<String, Entry>> {
        match self {
            Self::HashMap(v, _) => Some(v),
            _ => None,
        }
    }

    /// Get mutable reference to vec contents
    pub fn as_vec_mut(&mut self) -> Option<&mut Vec<Entry>> {
        match self {
            Self::Vec(v, _) => Some(v),
            _ => None,
        }
    }

    /// Get mutable reference to hashmap contents
    pub fn as_hashmap_mut(&mut self) -> Option<&mut HashMap<String, Entry>> {
        match self {
            Self::HashMap(v, _) => Some(v),
            _ => None,
        }
    }

    // ========================================================================
    // to_*() - Convert/extract value (consumes or clones)
    // ========================================================================

    pub fn to_i64(&self) -> Option<i64> {
        match self {
            Self::I8(v, _) => Some(*v as i64),
            Self::I16(v, _) => Some(*v as i64),
            Self::I32(v, _) => Some(*v as i64),
            Self::I64(v, _) => Some(*v),
            Self::I128(v, _) => (*v).try_into().ok(),
            Self::Isize(v, _) => Some(*v as i64),
            Self::U8(v, _) => Some(*v as i64),
            Self::U16(v, _) => Some(*v as i64),
            Self::U32(v, _) => Some(*v as i64),
            Self::U64(v, _) => (*v).try_into().ok(),
            Self::U128(v, _) => (*v).try_into().ok(),
            Self::Usize(v, _) => (*v).try_into().ok(),
            _ => None,
        }
    }

    pub fn to_u64(&self) -> Option<u64> {
        match self {
            Self::I8(v, _) if *v >= 0 => Some(*v as u64),
            Self::I16(v, _) if *v >= 0 => Some(*v as u64),
            Self::I32(v, _) if *v >= 0 => Some(*v as u64),
            Self::I64(v, _) if *v >= 0 => Some(*v as u64),
            Self::I128(v, _) => (*v).try_into().ok(),
            Self::Isize(v, _) if *v >= 0 => Some(*v as u64),
            Self::U8(v, _) => Some(*v as u64),
            Self::U16(v, _) => Some(*v as u64),
            Self::U32(v, _) => Some(*v as u64),
            Self::U64(v, _) => Some(*v),
            Self::U128(v, _) => (*v).try_into().ok(),
            Self::Usize(v, _) => Some(*v as u64),
            _ => None,
        }
    }

    pub fn to_f64(&self) -> Option<f64> {
        match self {
            Self::F32(v, _) => Some(*v as f64),
            Self::F64(v, _) => Some(*v),
            Self::I8(v, _) => Some(*v as f64),
            Self::I16(v, _) => Some(*v as f64),
            Self::I32(v, _) => Some(*v as f64),
            Self::I64(v, _) => Some(*v as f64),
            Self::U8(v, _) => Some(*v as f64),
            Self::U16(v, _) => Some(*v as f64),
            Self::U32(v, _) => Some(*v as f64),
            Self::U64(v, _) => Some(*v as f64),
            _ => None,
        }
    }

    pub fn to_string_value(&self) -> Option<String> {
        match self {
            Self::String(v, _) => Some(v.clone()),
            Self::Char(v, _) => Some(v.to_string()),
            Self::PathBuf(v, _) => Some(v.to_string_lossy().into_owned()),
            Self::I8(n, _) => Some(n.to_string()),
            Self::I16(n, _) => Some(n.to_string()),
            Self::I32(n, _) => Some(n.to_string()),
            Self::I64(n, _) => Some(n.to_string()),
            Self::I128(n, _) => Some(n.to_string()),
            Self::U8(n, _) => Some(n.to_string()),
            Self::U16(n, _) => Some(n.to_string()),
            Self::U32(n, _) => Some(n.to_string()),
            Self::U64(n, _) => Some(n.to_string()),
            Self::U128(n, _) => Some(n.to_string()),
            Self::Isize(n, _) => Some(n.to_string()),
            Self::Usize(n, _) => Some(n.to_string()),
            Self::F32(n, _) => Some(n.to_string()),
            Self::F64(n, _) => Some(n.to_string()),
            Self::Bool(b, _) => Some(b.to_string()),
            _ => None,
        }
    }

    pub fn to_vec_cloned(&self) -> Option<Vec<Entry>> {
        match self {
            Self::Vec(v, _) => Some(v.clone()),
            _ => None,
        }
    }

    pub fn to_hashmap_cloned(&self) -> Option<HashMap<String, Entry>> {
        match self {
            Self::HashMap(v, _) => Some(v.clone()),
            _ => None,
        }
    }

    // ========================================================================
    // is_*() - Additional type checking predicates
    // ========================================================================

    pub fn is_signed(&self) -> bool {
        matches!(
            self,
            Self::I8(..)
                | Self::I16(..)
                | Self::I32(..)
                | Self::I64(..)
                | Self::I128(..)
                | Self::Isize(..)
        )
    }

    pub fn is_unsigned(&self) -> bool {
        matches!(
            self,
            Self::U8(..)
                | Self::U16(..)
                | Self::U32(..)
                | Self::U64(..)
                | Self::U128(..)
                | Self::Usize(..)
        )
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Self::F32(..) | Self::F64(..))
    }

    pub fn is_string(&self) -> bool {
        matches!(self, Self::String(..))
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, Self::Bool(..))
    }

    pub fn is_unit(&self) -> bool {
        matches!(self, Self::Unit(..))
    }

    pub fn is_vec(&self) -> bool {
        matches!(self, Self::Vec(..))
    }

    pub fn is_hashmap(&self) -> bool {
        matches!(self, Self::HashMap(..))
    }

    /// Check if attribute exists on a Node.
    pub fn has_attr(&self, key: &str) -> bool {
        self.attr(key).is_some()
    }

    /// Remove an attribute from a Node.
    pub fn remove_attr(&mut self, key: &str) -> Option<Entry> {
        match self {
            Self::Node { attrs, .. } => attrs.remove(key),
            _ => None,
        }
    }

    /// Get source range if this is a Range variant or has a source_range attribute.
    pub fn source_range(&self) -> Option<Range<usize>> {
        match self {
            Self::Range(r, _) => Some(r.clone()),
            Self::Node { attrs, .. } => {
                if let Some(Entry::Range(r, _)) = attrs.get("source_range") {
                    Some(r.clone())
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Set source range on a Node.
    pub fn set_source_range(&mut self, range: Range<usize>) -> bool {
        self.set_attr("source_range", Entry::range(range))
    }

    /// Set owner (parent scope name) on a Node.
    pub fn set_owner(&mut self, owner: Option<String>) -> bool {
        match owner {
            Some(o) => self.set_attr("owner", Entry::string(o)),
            None => {
                self.remove_attr("owner");
                true
            }
        }
    }

    /// Get owner (parent scope name) from a Node.
    pub fn owner(&self) -> Option<&str> {
        self.get_string_attr("owner")
    }

    /// Set declaration order on a Node.
    pub fn set_declaration_order(&mut self, order: usize) -> bool {
        self.set_attr("declaration_order", Entry::usize(order))
    }

    /// Get declaration order from a Node.
    pub fn declaration_order(&self) -> Option<usize> {
        match self.attr("declaration_order")? {
            Entry::Usize(n, _) => Some(*n),
            Entry::U64(n, _) => Some(*n as usize),
            Entry::I64(n, _) => Some(*n as usize),
            _ => None,
        }
    }
}

// ============================================================================
// Enhanced Site (IFTree replacement)
// ============================================================================

impl Site {
    /// Store metadata on the site.
    pub fn set_metadata(&mut self, key: impl Into<String>, value: Entry) {
        self.metadata.insert(key.into(), value);
    }

    /// Get metadata from the site.
    pub fn get_metadata(&self, key: &str) -> Option<&Entry> {
        self.metadata.get(key)
    }

    /// Find all nodes by kind.
    pub fn find_by_kind(&self, kind: impl Into<String>) -> Vec<usize> {
        let kind_str = kind.into();
        self.sites
            .iter()
            .enumerate()
            .filter(|(_, e)| e.kind().map_or(false, |k| k == kind_str))
            .map(|(i, _)| i)
            .collect()
    }

    /// Find all nodes by name pattern (case-insensitive contains).
    pub fn find_by_name(&self, pattern: &str) -> Vec<usize> {
        let pattern_lower = pattern.to_lowercase();
        self.sites
            .iter()
            .enumerate()
            .filter(|(_, e)| {
                e.name()
                    .map(|n| n.to_lowercase().contains(&pattern_lower))
                    .unwrap_or(false)
            })
            .map(|(i, _)| i)
            .collect()
    }

    /// Find all nodes with a specific attribute.
    pub fn find_with_attr(&self, attr_key: &str) -> Vec<usize> {
        self.sites
            .iter()
            .enumerate()
            .filter(|(_, e)| e.has_attr(attr_key))
            .map(|(i, _)| i)
            .collect()
    }

    /// Get total node count.
    pub fn total_nodes(&self) -> usize {
        self.sites.len()
    }

    /// Get maximum depth in the site.
    pub fn max_depth(&self) -> usize {
        self.sites.iter().map(|e| e.depth()).max().unwrap_or(0)
    }

    /// Get all function nodes.
    pub fn functions(&self) -> Vec<usize> {
        self.find_by_kind(NodeKind::Function)
    }

    /// Get all struct nodes.
    pub fn structs(&self) -> Vec<usize> {
        self.find_by_kind(NodeKind::Struct)
    }

    /// Get all enum nodes.
    pub fn enums(&self) -> Vec<usize> {
        self.find_by_kind(NodeKind::Enum)
    }

    /// Get all global variable nodes.
    pub fn globals(&self) -> Vec<usize> {
        self.find_by_kind(NodeKind::Global)
    }

    /// Get all typedef nodes.
    pub fn typedefs(&self) -> Vec<usize> {
        self.find_by_kind(NodeKind::Typedef)
    }

    /// Get all macro nodes.
    pub fn macros(&self) -> Vec<usize> {
        self.find_by_kind(NodeKind::Macro)
    }

    /// Calculate statistics for the site.
    pub fn stats(&self) -> SiteStats {
        let mut stats = SiteStats::default();
        stats.total_sites = self.sites.len();
        stats.max_depth = self.max_depth();

        for site in &self.sites {
            stats.total_links += site.links().len();
            *stats
                .type_counts
                .entry(site.type_name().to_string())
                .or_insert(0) += 1;
            if let Some(kind) = site.kind() {
                stats.node_count += 1;
                *stats.kind_counts.entry(kind.to_string()).or_insert(0) += 1;
            }
        }

        stats
    }

    /// Clear all sites and metadata.
    pub fn clear(&mut self) {
        self.sites.clear();
        self.roots.clear();
        self.metadata.clear();
    }

    /// Get capacity of underlying storage.
    #[inline]
    pub fn capacity(&self) -> usize {
        self.sites.capacity()
    }

    /// Reserve additional capacity.
    #[inline]
    pub fn reserve(&mut self, additional: usize) {
        self.sites.reserve(additional);
    }

    /// Traverse breadth-first from roots.
    pub fn traverse_bfs(&self) -> Vec<usize> {
        use std::collections::VecDeque;

        let mut visited = vec![false; self.sites.len()];
        let mut result = Vec::new();
        let mut queue = VecDeque::new();

        for &root in &self.roots {
            if root < visited.len() && !visited[root] {
                visited[root] = true;
                queue.push_back(root);
            }
        }

        while let Some(index) = queue.pop_front() {
            result.push(index);
            if let Some(site) = self.sites.get(index) {
                for child in site.children() {
                    if child < visited.len() && !visited[child] {
                        visited[child] = true;
                        queue.push_back(child);
                    }
                }
            }
        }

        result
    }

    /// Get path from root to node.
    pub fn path_to(&self, index: usize) -> Option<Vec<usize>> {
        if index >= self.sites.len() {
            return None;
        }

        let mut path = vec![index];
        let mut current = index;

        while let Some(parent) = self.sites.get(current).and_then(|e| e.parent()) {
            path.push(parent);
            current = parent;
        }

        path.reverse();
        Some(path)
    }

    /// Get parent of a node.
    pub fn parent_of(&self, index: usize) -> Option<usize> {
        self.sites.get(index).and_then(|e| e.parent())
    }

    /// Get siblings of a node.
    pub fn siblings_of(&self, index: usize) -> Vec<usize> {
        let Some(parent) = self.parent_of(index) else {
            return Vec::new();
        };

        self.children_of(parent)
            .into_iter()
            .filter(|&i| i != index)
            .collect()
    }

    /// Get all leaf nodes (nodes with no children).
    pub fn leaves(&self) -> Vec<usize> {
        self.sites
            .iter()
            .enumerate()
            .filter(|(_, e)| e.links().is_leaf())
            .map(|(i, _)| i)
            .collect()
    }

    /// Get nodes at a specific depth level.
    pub fn nodes_at_depth(&self, depth: usize) -> Vec<usize> {
        self.sites
            .iter()
            .enumerate()
            .filter(|(_, e)| e.depth() == depth)
            .map(|(i, _)| i)
            .collect()
    }

    /// Create a node and add it as root.
    pub fn add_node(&mut self, kind: &str, name: impl Into<String>) -> usize {
        self.add_root(Entry::node(kind, name))
    }

    /// Create a node and add it as child to parent.
    pub fn add_child_node(
        &mut self,
        parent: usize,
        kind: &str,
        name: impl Into<String>,
    ) -> Option<usize> {
        self.add_child_to(parent, Entry::node(kind, name))
    }

    /// Iterate over root indices.
    pub fn root_indices(&self) -> impl Iterator<Item = usize> + '_ {
        self.roots.iter().copied()
    }

    /// Iterate over root sites.
    pub fn root_entries(&self) -> impl Iterator<Item = &Entry> {
        self.roots.iter().filter_map(|&i| self.sites.get(i))
    }
}

// ============================================================================
// From implementations
// ============================================================================

impl From<i8> for Entry {
    fn from(v: i8) -> Self {
        Self::i8(v)
    }
}
impl From<i16> for Entry {
    fn from(v: i16) -> Self {
        Self::i16(v)
    }
}
impl From<i32> for Entry {
    fn from(v: i32) -> Self {
        Self::i32(v)
    }
}
impl From<i64> for Entry {
    fn from(v: i64) -> Self {
        Self::i64(v)
    }
}
impl From<i128> for Entry {
    fn from(v: i128) -> Self {
        Self::i128(v)
    }
}
impl From<isize> for Entry {
    fn from(v: isize) -> Self {
        Self::isize(v)
    }
}
impl From<u8> for Entry {
    fn from(v: u8) -> Self {
        Self::u8(v)
    }
}
impl From<u16> for Entry {
    fn from(v: u16) -> Self {
        Self::u16(v)
    }
}
impl From<u32> for Entry {
    fn from(v: u32) -> Self {
        Self::u32(v)
    }
}
impl From<u64> for Entry {
    fn from(v: u64) -> Self {
        Self::u64(v)
    }
}
impl From<u128> for Entry {
    fn from(v: u128) -> Self {
        Self::u128(v)
    }
}
impl From<usize> for Entry {
    fn from(v: usize) -> Self {
        Self::usize(v)
    }
}
impl From<f32> for Entry {
    fn from(v: f32) -> Self {
        Self::f32(v)
    }
}
impl From<f64> for Entry {
    fn from(v: f64) -> Self {
        Self::f64(v)
    }
}
impl From<char> for Entry {
    fn from(v: char) -> Self {
        Self::char(v)
    }
}
impl From<bool> for Entry {
    fn from(v: bool) -> Self {
        Self::bool(v)
    }
}
impl From<()> for Entry {
    fn from(_: ()) -> Self {
        Self::unit()
    }
}
impl From<String> for Entry {
    fn from(v: String) -> Self {
        Entry::parse_string(&v)
    }
}
impl From<&str> for Entry {
    fn from(v: &str) -> Self {
        Entry::parse_string(v)
    }
}
impl From<PathBuf> for Entry {
    fn from(v: PathBuf) -> Self {
        Self::PathBuf(v, Links::new())
    }
}
impl From<Range<usize>> for Entry {
    fn from(v: Range<usize>) -> Self {
        Self::range(v)
    }
}
impl From<Vec<Entry>> for Entry {
    fn from(v: Vec<Entry>) -> Self {
        Self::vec(v)
    }
}

// ============================================================================
// Build Trait - Unified building interface for all data types
// ============================================================================

/// Trait for types that can be built into Entry data for the Web database.
/// Implementors hold their own data and convert it to Entry format when added to Web.
pub trait Build {
    /// Convert this object to an Entry representation.
    fn to_entry(&self) -> Entry;

    /// Get the kind/type identifier for this object.
    fn kind(&self) -> &str;

    /// Get an optional name for this object.
    fn name(&self) -> Option<&str> {
        None
    }

    /// Get optional category for grouping.
    fn category(&self) -> Option<&str> {
        None
    }

    /// Get priority for ordering (higher = more important).
    fn priority(&self) -> i16 {
        0
    }

    /// Register this object to the system database if auto-registration is enabled.
    /// Returns true if the object was registered, false otherwise.
    fn register(&self) -> bool {
        let sys = crate::system::system();
        if sys.is_build_registration_enabled() {
            sys.db_mut().add_entry(self.to_entry());
            true
        } else {
            false
        }
    }

    /// Force registration to system database regardless of the auto-registration flag.
    fn force_register(&self) {
        crate::system::system().db_mut().add_entry(self.to_entry());
    }

    /// Check if this object matches the given query specification.
    /// The query is an Entry that specifies match criteria:
    /// - Node with kind: matches objects of that kind
    /// - Node with name: matches objects with that name
    /// - Node with attrs: matches objects where attrs match
    /// Default implementation checks kind, name, and category.
    fn matches(&self, query: &Entry) -> bool {
        // Match by kind if query has a kind
        if let Some(query_kind) = query.kind() {
            if query_kind != self.kind() {
                return false;
            }
        }

        // Match by name if query has a name
        if let Some(query_name) = query.name() {
            match self.name() {
                Some(name) if name == query_name => {}
                Some(_) => return false,
                None => return false,
            }
        }

        // Match by category if query has category attr
        if let Some(query_cat) = query.get_string_attr("category") {
            match self.category() {
                Some(cat) if cat == query_cat => {}
                Some(_) => return false,
                None => return false,
            }
        }

        true
    }
}

// ============================================================================
// Web - Unified Database for Sites and Entries
// ============================================================================

/// Unified database structure that holds all data types via the Build trait.
/// Encapsulates Sites and provides a single interface for all db operations.
#[derive(Clone, Default)]
pub struct Web {
    /// Primary site storage
    site: Site,
    /// Named categories/groups of entries
    categories: HashMap<String, Vec<usize>>,
    /// Index by kind
    by_kind: HashMap<String, Vec<usize>>,
    /// Index by name
    by_name: HashMap<String, Vec<usize>>,
    /// Metadata about the web
    metadata: HashMap<String, Entry>,
    /// Stored queries (QuerySpec entries indexed by name)
    queries: HashMap<String, Entry>,
    /// Statistics
    stats: WebStats,
}

/// Statistics for the Web database
#[derive(Clone, Default, Debug)]
pub struct WebStats {
    pub total_entries: usize,
    pub total_categories: usize,
    pub total_kinds: usize,
    pub build_count: usize,
}

impl Web {
    pub fn new() -> Self {
        Self::default()
    }

    /// Get the underlying site
    pub fn site(&self) -> &Site {
        &self.site
    }

    /// Get mutable access to the underlying site
    pub fn site_mut(&mut self) -> &mut Site {
        &mut self.site
    }

    /// Add a Build implementor to the database
    pub fn add<B: Build>(&mut self, item: &B) -> usize {
        let entry = item.to_entry();
        let kind = item.kind().to_string();
        let name = item.name().map(|s| s.to_string());
        let category = item.category().map(|s| s.to_string());

        let idx = self.site.add_root(entry);

        // Index by kind
        self.by_kind.entry(kind).or_default().push(idx);

        // Index by name if present
        if let Some(n) = name {
            self.by_name.entry(n).or_default().push(idx);
        }

        // Index by category if present
        if let Some(c) = category {
            self.categories.entry(c).or_default().push(idx);
        }

        self.stats.build_count += 1;
        self.update_stats();
        idx
    }

    /// Add an Entry directly
    pub fn add_entry(&mut self, entry: Entry) -> usize {
        let kind = entry.kind().map(|s| s.to_string());
        let name = entry.name().map(|s| s.to_string());

        let idx = self.site.add_root(entry);

        if let Some(k) = kind {
            self.by_kind.entry(k).or_default().push(idx);
        }
        if let Some(n) = name {
            self.by_name.entry(n).or_default().push(idx);
        }

        self.update_stats();
        idx
    }

    /// Add as child of existing entry
    pub fn add_child<B: Build>(&mut self, parent: usize, item: &B) -> Option<usize> {
        let entry = item.to_entry();
        self.site.add_child_to(parent, entry)
    }

    /// Add entry as child
    pub fn add_child_entry(&mut self, parent: usize, entry: Entry) -> Option<usize> {
        self.site.add_child_to(parent, entry)
    }

    /// Get entry at index
    pub fn get(&self, idx: usize) -> Option<&Entry> {
        self.site.get(idx)
    }

    /// Get mutable entry at index
    pub fn get_mut(&mut self, idx: usize) -> Option<&mut Entry> {
        self.site.get_mut(idx)
    }

    /// Get all entries of a specific kind
    pub fn by_kind(&self, kind: &str) -> Vec<&Entry> {
        self.by_kind
            .get(kind)
            .map(|indices| indices.iter().filter_map(|&i| self.get(i)).collect())
            .unwrap_or_default()
    }

    /// Get all entries with a specific name
    pub fn by_name(&self, name: &str) -> Vec<&Entry> {
        self.by_name
            .get(name)
            .map(|indices| indices.iter().filter_map(|&i| self.get(i)).collect())
            .unwrap_or_default()
    }

    /// Get all entries in a category
    pub fn by_category(&self, category: &str) -> Vec<&Entry> {
        self.categories
            .get(category)
            .map(|indices| indices.iter().filter_map(|&i| self.get(i)).collect())
            .unwrap_or_default()
    }

    /// Get all category names
    pub fn categories(&self) -> Vec<&str> {
        self.categories.keys().map(|s| s.as_str()).collect()
    }

    /// Get all kind names
    pub fn kinds(&self) -> Vec<&str> {
        self.by_kind.keys().map(|s| s.as_str()).collect()
    }

    /// Total number of entries
    pub fn len(&self) -> usize {
        self.site.len()
    }

    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.site.is_empty()
    }

    /// Set metadata
    pub fn set_metadata(&mut self, key: impl Into<String>, value: Entry) {
        self.metadata.insert(key.into(), value);
    }

    /// Get metadata
    pub fn metadata(&self, key: &str) -> Option<&Entry> {
        self.metadata.get(key)
    }

    /// Get statistics
    pub fn stats(&self) -> &WebStats {
        &self.stats
    }

    fn update_stats(&mut self) {
        self.stats.total_entries = self.site.len();
        self.stats.total_categories = self.categories.len();
        self.stats.total_kinds = self.by_kind.len();
    }

    /// Iterate over all entries
    pub fn iter(&self) -> impl Iterator<Item = &Entry> {
        self.site.sites.iter()
    }

    /// Clear all data
    pub fn clear(&mut self) {
        self.site = Site::new();
        self.categories.clear();
        self.by_kind.clear();
        self.by_name.clear();
        self.metadata.clear();
        self.queries.clear();
        self.stats = WebStats::default();
    }

    // ========================================================================
    // Query Storage - Store and retrieve named queries
    // ========================================================================

    /// Store a query specification by name for later retrieval.
    /// The query is stored as an Entry, allowing it to persist with the database.
    pub fn store_query(&mut self, name: impl Into<String>, query_entry: Entry) {
        self.queries.insert(name.into(), query_entry);
    }

    /// Retrieve a stored query by name.
    pub fn get_query(&self, name: &str) -> Option<&Entry> {
        self.queries.get(name)
    }

    /// Remove a stored query.
    pub fn remove_query(&mut self, name: &str) -> Option<Entry> {
        self.queries.remove(name)
    }

    /// Get all stored query names.
    pub fn query_names(&self) -> Vec<&str> {
        self.queries.keys().map(|s| s.as_str()).collect()
    }

    /// Check if a query exists.
    pub fn has_query(&self, name: &str) -> bool {
        self.queries.contains_key(name)
    }

    /// Execute a stored query against the database entries.
    /// Returns entries matching the stored QuerySpec.
    pub fn execute_query(&self, name: &str) -> Vec<&Entry> {
        use crate::db::query::QuerySpec;

        let query_entry = match self.queries.get(name) {
            Some(e) => e,
            None => return Vec::new(),
        };

        let spec = match QuerySpec::from_entry(query_entry) {
            Some(s) => s,
            None => return Vec::new(),
        };

        self.iter().filter(|e| spec.matches(e)).collect()
    }
    /// Initialize the web database with default structure
    pub fn init(&mut self) {
        // Clear any existing data
        if !self.is_empty() {
            self.clear();
        }

        // Set initialization metadata
        self.set_metadata("initialized", Entry::bool(true));
        self.set_metadata("version", Entry::string("1.0"));

        // Update stats
        self.update_stats();
    }

    /// Save the web database to file in specified format
    pub fn save(&self, file_path: PathBuf, format: &str) {
        use std::fs::File;
        use std::io::Write;

        let content = match format.to_lowercase().as_str() {
            "json" => self.to_json_pretty(2),
            "json-compact" | "jsonc" => self.to_json(),
            _ => self.to_json_pretty(2), // Default to pretty JSON
        };

        if let Ok(mut file) = File::create(&file_path) {
            let _ = file.write_all(content.as_bytes());
        }
    }

    /// Load database from file in specified format
    pub fn load(&mut self, file_path: PathBuf, format: &str) {
        use std::fs;

        let content = match fs::read_to_string(&file_path) {
            Ok(c) => c,
            Err(_) => return,
        };

        match format.to_lowercase().as_str() {
            "json" | "json-compact" | "jsonc" => {
                if let Ok(entry) = crate::db::json::parse(&content) {
                    // Reconstruct Web from Entry
                    self.load_from_entry(&entry);
                }
            }
            _ => {} // Unknown format, do nothing
        }
    }

    /// Load database contents from an Entry (typically parsed from JSON)
    fn load_from_entry(&mut self, entry: &Entry) {
        // Clear existing data
        self.clear();

        // If it's a HashMap, iterate and add entries
        if let Entry::HashMap(map, _) = entry {
            // Look for "entries" key which contains entries by kind
            if let Some(Entry::HashMap(entries_map, _)) = map.get("entries") {
                for (_kind, kind_entries) in entries_map {
                    if let Entry::Vec(items, _) = kind_entries {
                        for item in items {
                            self.add_entry(item.clone());
                        }
                    }
                }
            }
        }

        self.update_stats();
    }

    /// Display the web database using default format
    pub fn display(&self) {
        use crate::db::format::{Formatter, OutputFormat};
        println!("{}", self.format(&OutputFormat::Table));
    }

    /// Display the web database using specified format
    pub fn display_as(&self, format: &str) {
        use crate::db::format::{Formatter, OutputFormat};
        println!("{}", self.format(&OutputFormat::from_str(format)));
    }

    /// Convert the entire Web to a JSON string
    pub fn to_json(&self) -> String {
        use crate::db::format::{Formatter, OutputFormat};
        self.format(&OutputFormat::JsonCompact)
    }

    /// Convert the entire Web to a pretty-printed JSON string
    pub fn to_json_pretty(&self, _spaces: u16) -> String {
        use crate::db::format::{Formatter, OutputFormat};
        self.format(&OutputFormat::Json)
    }
}

impl Debug for Web {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Web({} entries, {} kinds, {} categories)",
            self.stats.total_entries, self.stats.total_kinds, self.stats.total_categories
        )
    }
}
impl Display for Web {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Web({} entries, {} kinds, {} categories)",
            self.stats.total_entries, self.stats.total_kinds, self.stats.total_categories
        )
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_entry_basic() {
        let e = Entry::i32(42);
        assert_eq!(e.type_name(), "i32");
        assert!(e.is_integer());
        assert!(e.links().is_empty());
    }

    #[test]
    fn test_entry_links() {
        let mut e = Entry::string("test");
        e.add_child(1);
        e.add_child(2);
        assert_eq!(e.links().len(), 2);
        assert_eq!(e.children().collect::<Vec<_>>(), vec![1, 2]);
    }

    #[test]
    fn test_entry_node() {
        let mut node = Entry::node("Function", "main");
        node.set_attr("return_type", Entry::string("int"));

        assert_eq!(node.kind(), Some("Function"));
        assert_eq!(node.name(), Some("main"));
        assert!(node.attr("return_type").is_some());
    }

    #[test]
    fn test_site_basic() {
        let mut site = Site::new();
        let root = site.add_root(Entry::node("Root", "root"));
        let child1 = site.add_child_to(root, Entry::string("child1")).unwrap();
        let child2 = site.add_child_to(root, Entry::string("child2")).unwrap();

        assert_eq!(site.len(), 3);
        assert_eq!(site.children_of(root), vec![child1, child2]);
        assert_eq!(site[child1].parent(), Some(root));
        assert_eq!(site[child1].depth(), 1);
    }

    #[test]
    fn test_site_traverse() {
        let mut site = Site::new();
        let root = site.add_root(Entry::i32(0));
        let c1 = site.add_child_to(root, Entry::i32(1)).unwrap();
        let _c2 = site.add_child_to(root, Entry::i32(2)).unwrap();
        let _c1_1 = site.add_child_to(c1, Entry::i32(3)).unwrap();

        let order = site.traverse_dfs();
        assert_eq!(order.len(), 4);
        assert_eq!(order[0], root);
    }

    #[test]
    fn test_range_variant() {
        let r = Entry::range(10..20);
        assert_eq!(r.type_name(), "Range");
        if let Entry::Range(range, _) = r {
            assert_eq!(range.start, 10);
            assert_eq!(range.end, 20);
        }
    }

    #[test]
    fn test_branch_op_evaluate() {
        assert!(BranchOp::And.evaluate(true, true));
        assert!(!BranchOp::And.evaluate(true, false));
        assert!(BranchOp::Or.evaluate(true, false));
        assert!(BranchOp::Or.evaluate(false, true));
        assert!(!BranchOp::Or.evaluate(false, false));
        assert!(BranchOp::Xor.evaluate(true, false));
        assert!(!BranchOp::Xor.evaluate(true, true));
        assert!(BranchOp::Nand.evaluate(false, false));
        assert!(!BranchOp::Nand.evaluate(true, true));
        assert!(BranchOp::Nor.evaluate(false, false));
        assert!(!BranchOp::Nor.evaluate(true, false));
        assert!(BranchOp::AndNot.evaluate(true, false));
        assert!(!BranchOp::AndNot.evaluate(true, true));
        assert!(BranchOp::NotAnd.evaluate(false, true));
        assert!(!BranchOp::NotAnd.evaluate(true, true));
    }

    #[test]
    fn test_branch_entry() {
        fn is_positive(e: &Entry) -> bool {
            matches!(e, Entry::I32(n, _) if *n > 0)
        }
        fn is_even(e: &Entry) -> bool {
            matches!(e, Entry::I32(n, _) if *n % 2 == 0)
        }

        let branch = Entry::branch(
            BranchOp::And,
            is_positive,
            is_even,
            vec![Entry::string("positive and even")],
            vec![Entry::string("not positive and even")],
        );

        assert!(branch.is_branch());
        assert!(branch.is_container());
        assert_eq!(branch.type_name(), "Branch");

        // Test evaluate_branch with context
        let context_pos_even = Entry::i32(4);
        let result = branch.evaluate_branch(&context_pos_even);
        assert!(result.is_some());
        assert_eq!(result.unwrap().len(), 1);

        let context_neg = Entry::i32(-2);
        let result = branch.evaluate_branch(&context_neg);
        assert!(result.is_some());
        // -2 is even but not positive, so AND fails -> false_path
        if let Some(path) = result {
            assert_eq!(path.len(), 1);
        }
    }

    #[test]
    fn test_branch_paths() {
        fn always_true(_: &Entry) -> bool {
            true
        }
        fn always_false(_: &Entry) -> bool {
            false
        }

        let branch = Entry::branch_and(
            always_true,
            always_false,
            vec![Entry::i32(1), Entry::i32(2)],
            vec![Entry::i32(3)],
        );

        assert_eq!(branch.true_path().map(|p| p.len()), Some(2));
        assert_eq!(branch.false_path().map(|p| p.len()), Some(1));
    }
}
