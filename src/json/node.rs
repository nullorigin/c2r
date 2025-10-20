use core::{fmt::Display, iter::Iterator};

use crate::json::{Key, Value};

#[derive(Clone)]
pub struct Node {
    // String-esque key abstraction
    pub key: Key,

    // Value stored.
    pub value: Value,

    // Store vector index pointing to the `Node` for which `key_hash` is smaller
    // than that of this `Node`.
    // Will default to 0 as root node can't be referenced anywhere else.
    pub left: usize,

    // Same as above but for `Node`s with hash larger than this one. If the
    // hash is the same, but keys are different, the lookup will default
    // to the right branch as well.
    pub right: usize,
}

impl std::fmt::Debug for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Debug::fmt(&(self.key.as_str(), &self.value, self.left, self.right), f)
    }
}
impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "({} {} {} {})",
            self.key.as_str(),
            self.value,
            self.left,
            self.right
        )
    }
}
impl PartialEq for Node {
    fn eq(&self, other: &Node) -> bool {
        self.key.hash == other.key.hash
            && self.key.as_bytes() == other.key.as_bytes()
            && self.value == other.value
    }
}

impl Node {
    #[inline]
    pub fn new(value: Value, hash: u64, len: usize) -> Node {
        Node {
            key: Key::new(hash, len),
            value: value,
            left: 0,
            right: 0,
        }
    }
}

pub mod iterators {
    /// Iterator over members of `Value::Array`.
    pub type Members<'a> = std::slice::Iter<'a, crate::json::Value>;

    /// Mutable iterator over members of `Value::Array`.
    pub type MembersMut<'a> = std::slice::IterMut<'a, crate::json::Value>;

    /// Iterator over key value pairs of `Value::Object`.
    pub type Entries<'a> = crate::json::Iter<'a>;

    /// Mutable iterator over key value pairs of `Value::Object`.
    pub type EntriesMut<'a> = crate::json::IterMut<'a>;
}
pub struct Iter<'a> {
    inner: std::slice::Iter<'a, Node>,
}
pub struct IterMut<'a> {
    inner: std::slice::IterMut<'a, Node>,
}
impl<'a> Iterator for Iter<'a> {
    type Item = (&'a str, &'a Value);

    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {
        self.inner
            .next()
            .map(|node| (node.key.as_str(), &node.value))
    }
}

impl<'a> DoubleEndedIterator for Iter<'a> {
    #[inline(always)]
    fn next_back(&mut self) -> Option<Self::Item> {
        self.inner
            .next_back()
            .map(|node| (node.key.as_str(), &node.value))
    }
}
impl<'a> ExactSizeIterator for Iter<'a> {
    fn len(&self) -> usize {
        self.inner.len()
    }
}
impl<'a> Iterator for IterMut<'a> {
    type Item = (&'a str, &'a mut Value);

    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {
        self.inner
            .next()
            .map(|node| (node.key.as_str(), &mut node.value))
    }
}

impl<'a> DoubleEndedIterator for IterMut<'a> {
    #[inline(always)]
    fn next_back(&mut self) -> Option<Self::Item> {
        self.inner
            .next_back()
            .map(|node| (node.key.as_str(), &mut node.value))
    }
}

impl<'a> IterMut<'a> {
    /// Create an empty iterator that always returns `None`
    pub fn empty() -> Self {
        IterMut {
            inner: [].iter_mut(),
        }
    }
}

impl<'a> Iter<'a> {
    /// Create an empty iterator that always returns `None`
    pub fn empty() -> Self {
        Iter { inner: [].iter() }
    }
}
