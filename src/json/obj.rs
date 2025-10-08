use core::option::Option::None;

use crate::json::{hash_key, DumpGenerator, Generator, Node, PrettyGenerator, Value, NULL};


/// A binary tree implementation of a string -> `JsonValue` map. You normally don't
/// have to interact with instances of `Object`, much more likely you will be
/// using the `JsonValue::Object` variant, which wraps around this struct.
#[derive(Debug)]
pub struct Object {
    store: *mut Node,
    len: usize,
    capacity: usize,
}

impl Drop for Object {
    fn drop(&mut self) {
        if !self.store.is_null() {
            let layout = std::alloc::Layout::array::<Node>(self.capacity).unwrap();
            unsafe {
                // Drop all values and keys
                for i in 0..self.len {
                    std::ptr::drop_in_place(self.store.add(i));
                }
                std::alloc::dealloc(self.store as *mut u8, layout);
            }
            self.store = std::ptr::null_mut();
            self.len = 0;
            self.capacity = 0;
        }
    }
}

impl Object {
    /// Create a new, empty instance of `Object`. Empty `Object` performs no
    /// allocation until a value is inserted into it.
    #[inline(always)]
    pub fn new() -> Self {
        Object {
            store: std::ptr::null_mut(),
            len: 0,
            capacity: 0,
        }
    }

    /// Create a new `Object` with memory preallocated for `capacity` number
    /// of entries.
    #[inline(always)]
    pub fn with_capacity(capacity: usize) -> Self {
        if capacity == 0 {
            return Self::new();
        }

        let layout = std::alloc::Layout::array::<Node>(capacity).unwrap();
        let store = unsafe { std::alloc::alloc(layout) as *mut Node };

        if store.is_null() {
            std::alloc::handle_alloc_error(layout);
        }

        Object {
            store,
            len: 0,
            capacity,
        }
    }

    #[inline]
    fn index_mut(&mut self, index: usize) -> &mut Node {
        unsafe { &mut *self.store.add(index) }
    }

    #[inline(always)]
    fn add_node(&mut self, key: &[u8], value: Value, hash: u64) -> usize {
        let index = self.len;

        if self.len >= self.capacity {
            let new_capacity = if self.capacity == 0 {
                4
            } else {
                self.capacity * 2
            };
            let new_layout = std::alloc::Layout::array::<Node>(new_capacity).unwrap();
            let new_store = unsafe {
                let ptr = if self.store.is_null() {
                    std::alloc::alloc(new_layout)
                } else {
                    let old_layout = std::alloc::Layout::array::<Node>(self.capacity).unwrap();
                    std::alloc::realloc(self.store as *mut u8, old_layout, new_layout.size())
                };

                if ptr.is_null() {
                    std::alloc::handle_alloc_error(new_layout);
                }

                ptr as *mut Node
            };

            self.store = new_store;
            self.capacity = new_capacity;
        }

        unsafe {
            let node_ptr = self.store.add(index);
            std::ptr::write(node_ptr, Node::new(value, hash, key.len()));
            (*node_ptr).key.attach(key);
        }

        self.len += 1;
        index
    }

    /// Insert a new entry, or override an existing one. Note that `key` has
    /// to be a `&str` slice and not an owned `String`. The internals of
    /// `Object` will handle the heap allocation of the key if needed for
    /// better performance.
    #[inline]
    pub fn insert(&mut self, key: &str, value: Value) {
        self.insert_index(key, value);
    }

    pub(crate) fn insert_index(&mut self, key: &str, value: Value) -> usize {
        let key_bytes = key.as_bytes();
        let hash = hash_key(key_bytes);

        if self.len == 0 {
            let index = self.add_node(key_bytes, value, hash);
            return index;
        }

        let mut current = 0;

        loop {
            let node = self.index_mut(current);

            if hash == node.key.hash && key_bytes == node.key.as_bytes() {
                node.value = value;
                return current;
            } else if hash < node.key.hash {
                if node.left != 0 {
                    current = node.left;
                    continue;
                }
                let index = self.add_node(key_bytes, value, hash);
                self.index_mut(current).left = index;
                return index;
            } else {
                if node.right != 0 {
                    current = node.right;
                    continue;
                }
                let index = self.add_node(key_bytes, value, hash);
                self.index_mut(current).right = index;
                return index;
            }
        }
    }

    #[inline]
    pub(crate) fn override_at(&mut self, index: usize, value: Value) {
        self.index_mut(index).value = value;
    }

    pub fn get(&self, key: &str) -> Option<&Value> {
        if self.len == 0 {
            return None;
        }

        let key_bytes = key.as_bytes();
        let hash = hash_key(key_bytes);
        let mut current = 0;

        loop {
            unsafe {
                let node = &*self.store.add(current);

                if hash == node.key.hash && key_bytes == node.key.as_bytes() {
                    return Some(&node.value);
                } else if hash < node.key.hash {
                    if node.left == 0 {
                        return None;
                    }
                    current = node.left;
                } else {
                    if node.right == 0 {
                        return None;
                    }
                    current = node.right;
                }
            }
        }
    }

    pub fn get_mut(&mut self, key: &str) -> Option<&mut Value> {
        if self.len == 0 {
            return None;
        }

        let key_bytes = key.as_bytes();
        let hash = hash_key(key_bytes);
        let mut current = 0;

        loop {
            unsafe {
                let node = &mut *self.store.add(current);

                if hash == node.key.hash && key_bytes == node.key.as_bytes() {
                    return Some(&mut node.value);
                } else if hash < node.key.hash {
                    if node.left == 0 {
                        return None;
                    }
                    current = node.left;
                } else {
                    if node.right == 0 {
                        return None;
                    }
                    current = node.right;
                }
            }
        }
    }

    /// Attempts to remove the value behind `key`, if successful
    /// will return the `JsonValue` stored behind the `key`.
    pub fn remove(&mut self, key: &str) -> Option<Value> {
        if self.len == 0 {
            return None;
        }

        let key_bytes = key.as_bytes();
        let hash = hash_key(key_bytes);
        let mut index = 0;

        // Try to find the node
        loop {
            unsafe {
                let node = &*self.store.add(index);

                match hash == node.key.hash && key_bytes == node.key.as_bytes() {
                    true => break,
                    false => {
                        match hash < node.key.hash {
                            true => {
                                match node.left == 0 {
                                    true => return None,
                                    false => index = node.left,
                                }
                            }
                            false => {
                                match node.right == 0 {
                                    true => return None,
                                    false => index = node.right,
                                }
                            }
                        }
                    }
                }
            }
        }

        // Removing a node would screw the tree badly, it's easier to just
        // recreate it. This is a very costly operation, but removing nodes
        // in JSON shouldn't happen very often if at all.
        let mut new_object = Object::with_capacity(self.len - 1);
        let mut removed = None;

        for i in 0..self.len {
            unsafe {
                let node = &mut *self.store.add(i);
                if i == index {
                    removed = Some(std::mem::replace(&mut node.value, Value::Null));
                } else {
                    let value = std::mem::replace(&mut node.value, Value::Null);
                    new_object.insert(node.key.as_str(), value);
                }
            }
        }

        std::mem::swap(self, &mut new_object);
        removed
    }

    #[inline(always)]
    pub fn len(&self) -> usize {
        self.len
    }

    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// Wipe the `Object` clear. The capacity will remain untouched.
    pub fn clear(&mut self) {
        self.len = 0;
    }

    #[inline(always)]
    pub fn iter(&self) -> std::slice::Iter<'_, Node> {
        unsafe { std::slice::from_raw_parts(self.store, self.len) }.iter()
    }

    #[inline(always)]
    pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, Node> {
        unsafe { std::slice::from_raw_parts_mut(self.store, self.len) }.iter_mut()
    }

    /// Prints out the value as JSON string.
    pub fn dump(&self) -> String {
        let mut generator = DumpGenerator::new();
        generator.write_object(self).expect("Can't fail");
        generator.consume()
    }

    /// Pretty prints out the value as JSON string. Takes an argument that's
    /// number of spaces to indent new blocks with.
    pub fn pretty(&self, spaces: u16) -> String {
        let mut generator = PrettyGenerator::new(spaces);
        generator.write_object(self).expect("Can't fail");
        generator.consume()
    }
}
impl std::iter::Iterator for Object {
    type Item = Node;

    fn next(&mut self) -> Option<Self::Item> {
        if self.len == 0 {
            return None;
        }
        let node = unsafe { std::ptr::read(self.store) };
        self.len -= 1;
        self.store = unsafe { self.store.add(1) };
        Some(node)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len, Some(self.len))
    }

    fn last(self) -> Option<Self::Item> {
        if self.len == 0 {
            return None;
        }
        unsafe { Some(std::ptr::read(self.store.add(self.len - 1))) }
    }

    fn count(self) -> usize {
        self.len
    }
}

impl std::iter::ExactSizeIterator for Object {
    fn len(&self) -> usize {
        self.len
    }
}

impl std::iter::DoubleEndedIterator for Object {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.len == 0 {
            return None;
        }
        self.len -= 1;
        let node = unsafe { std::ptr::read(self.store.add(self.len)) };
        Some(node)
    }
}
// Custom implementation of `Clone`, as new heap allocation means
// we have to fix key pointers everywhere!
impl Clone for Object {
    fn clone(&self) -> Self {
        if self.len == 0 {
            return Object::new();
        }
        let layout = std::alloc::Layout::array::<Node>(self.capacity).unwrap();
        let new_store = unsafe { std::alloc::alloc(layout) as *mut Node };
        if new_store.is_null() {
            std::alloc::handle_alloc_error(layout);
        }
        unsafe {
            for i in 0..self.len {
                let src = self.store.add(i);
                let dst = new_store.add(i);
                std::ptr::write(dst, (*src).clone());
                // Fix key pointer if needed
                (*dst).key.fix_ptr();
            }
        }
        Object {
            store: new_store,
            len: self.len,
            capacity: self.capacity,
        }
    }
}

impl<K: AsRef<str>, V: Into<Value>> FromIterator<(K, V)> for Object {
    fn from_iter<I: IntoIterator<Item = (K, V)>>(iter: I) -> Self {
        let iter = iter.into_iter();
        let mut object = Object::with_capacity(iter.size_hint().0);

        for (key, value) in iter {
            object.insert(key.as_ref(), value.into());
        }

        object
    }
}

// Because keys can inserted in different order, the safe way to
// compare `Object`s is to iterate over one and check if the other
// has all the same keys.
impl PartialEq for Object {
    fn eq(&self, other: &Object) -> bool {
        if self.len() != other.len() {
            return false;
        }

        for node in self.iter() {
            match other.get(node.key.as_str()) {
                Some(other_val) => {
                    if *other_val != node.value {
                        return false;
                    }
                }
                None => return false,
            }
        }

        true
    }
}



/// Implements indexing by `&str` to easily access object members:
///
/// ## Example
///
/// ```
/// # fn main() {
/// let value = object!{
///     foo: "bar"
/// };
///
/// if let JsonValue::Object(object) = value {
///   assert!(object["foo"] == "bar");
/// }
/// # }
/// ```
// TODO: doc
impl<'a> std::ops::Index<&'a str> for Object {
    type Output = Value;

    fn index(&self, index: &str) -> &Value {
        match self.get(index) {
            Some(value) => value,
            _ => &NULL,
        }
    }
}

impl std::ops::Index<String> for Object {
    type Output = Value;

    fn index(&self, index: String) -> &Value {
        self.index(index.as_str())
    }
}

impl<'a> std::ops::Index<&'a String> for Object {
    type Output = Value;

    fn index(&self, index: &String) -> &Value {
        self.index(index.as_str())
    }
}

/// Implements mutable indexing by `&str` to easily modify object members:
///
/// ## Example
///
/// ```
/// # fn main() {
/// let value = object!{};
///
/// if let JsonValue::Object(mut object) = value {
///   object["foo"] = 42.into();
///
///   assert!(object["foo"] == 42);
/// }
/// # }
/// ```
impl<'a> std::ops::IndexMut<&'a str> for Object {
    fn index_mut(&mut self, index: &str) -> &mut Value {
        if self.get(index).is_none() {
            self.insert(index, Value::Null);
        }
        self.get_mut(index).unwrap()
    }
}

impl std::ops::IndexMut<String> for Object {
    fn index_mut(&mut self, index: String) -> &mut Value {
        self.get_mut(index.as_str()).unwrap()
    }
}

impl<'a> std::ops::IndexMut<&'a String> for Object {
    fn index_mut(&mut self, index: &String) -> &mut Value {
        self.get_mut(index.as_str()).unwrap()
    }
}