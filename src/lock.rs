#![allow(unused_variables, unused_assignments)]

use crate::{handler::Pending, Entry};
use crate::{time, Futex};
use core::cell::UnsafeCell;
use core::marker::{Send, Sync};
use core::ops::FnOnce;
use core::option::Option::{self, None, Some};
use core::{clone::Clone, default::Default};
use std::borrow::ToOwned;
use std::collections::HashMap;
use std::fmt::Display;
use std::hash::{Hash, Hasher};
use std::ptr::null_mut;
use std::sync::atomic::{AtomicPtr, AtomicUsize, Ordering};
use std::sync::Arc;
use std::sync::{LazyLock, RwLockReadGuard, RwLockWriteGuard};
use std::time::SystemTime;

pub const TIME_START: LazyLock<SystemTime> = LazyLock::new(|| SystemTime::now());

// Note: time!() macro is now in macros.rs
pub fn elapsed() -> u128 {
    SystemTime::now()
        .duration_since(TIME_START.to_owned())
        .unwrap_or_else(|_| std::time::Duration::from_nanos(0))
        .as_nanos()
}
pub fn gen_name(basename: &str) -> String {
    let time = elapsed();
    let mut name = basename.to_string();
    name.push('-');
    name.push_str(&time.to_string());
    name.clone()
}

pub struct Container<T> {
    futex: Futex,
    inner: AtomicPtr<T>,
    len: AtomicUsize,
    capacity: AtomicUsize,
}

impl<T> Container<T> {
    pub fn new() -> Self {
        Container {
            futex: Futex::new(),
            inner: AtomicPtr::new(null_mut()),
            len: AtomicUsize::new(0),
            capacity: AtomicUsize::new(0),
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        let layout = std::alloc::Layout::array::<T>(capacity).unwrap();
        let ptr = unsafe { std::alloc::alloc(layout) as *mut T };
        Container {
            futex: Futex::new(),
            inner: AtomicPtr::new(ptr),
            len: AtomicUsize::new(0),
            capacity: AtomicUsize::new(capacity),
        }
    }

    pub fn push(&self, value: T) {
        self.futex.lock();
        let len = self.len.load(Ordering::Acquire);
        let capacity = self.capacity.load(Ordering::Acquire);

        if len >= capacity {
            self.grow();
        }

        let ptr = self.inner.load(Ordering::Acquire);
        unsafe {
            std::ptr::write(ptr.add(len), value);
        }
        self.len.store(len + 1, Ordering::Release);
        self.futex.unlock();
    }

    pub fn pop(&self) -> Option<T> {
        self.futex.lock();
        let len = self.len.load(Ordering::Acquire);
        if len == 0 {
            self.futex.unlock();
            return None;
        }

        let new_len = len - 1;
        let ptr = self.inner.load(Ordering::Acquire);
        let value = unsafe { std::ptr::read(ptr.add(new_len)) };
        self.len.store(new_len, Ordering::Release);
        self.futex.unlock();
        Some(value)
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        let len = self.len.load(Ordering::Acquire);
        if index >= len {
            return None;
        }

        // Memory barrier to ensure we see the latest pointer
        core::sync::atomic::fence(Ordering::Acquire);
        let ptr = self.inner.load(Ordering::Acquire);

        // Double-check length after loading pointer
        let current_len = self.len.load(Ordering::Acquire);
        if index >= current_len {
            return None;
        }

        Some(unsafe { &*ptr.add(index) })
    }

    pub fn get_mut(&self, index: usize) -> Option<&mut T> {
        self.futex.lock();
        let len = self.len.load(Ordering::Acquire);
        if index >= len {
            self.futex.unlock();
            return None;
        }
        let ptr = self.inner.load(Ordering::Acquire);
        let result = Some(unsafe { &mut *ptr.add(index) });
        self.futex.unlock();
        result
    }

    pub fn insert(&self, index: usize, value: T) {
        self.futex.lock();
        let len = self.len.load(Ordering::Acquire);
        if index > len {
            self.futex.unlock();
            panic!("index out of bounds");
        }

        let capacity = self.capacity.load(Ordering::Acquire);
        if len >= capacity {
            self.grow();
        }

        let ptr = self.inner.load(Ordering::Acquire);
        unsafe {
            std::ptr::copy(ptr.add(index), ptr.add(index + 1), len - index);
            std::ptr::write(ptr.add(index), value);
        }
        self.len.store(len + 1, Ordering::Release);
        self.futex.unlock();
    }

    pub fn remove(&self, index: usize) -> T {
        self.futex.lock();
        let len = self.len.load(Ordering::Acquire);
        if index >= len {
            self.futex.unlock();
            panic!("index out of bounds");
        }

        let ptr = self.inner.load(Ordering::Acquire);
        let value = unsafe { std::ptr::read(ptr.add(index)) };
        unsafe {
            std::ptr::copy(ptr.add(index + 1), ptr.add(index), len - index - 1);
        }
        self.len.store(len - 1, Ordering::Release);
        self.futex.unlock();
        value
    }

    pub fn len(&self) -> usize {
        self.len.load(Ordering::Acquire)
    }

    pub fn capacity(&self) -> usize {
        self.capacity.load(Ordering::Acquire)
    }

    pub fn is_empty(&self) -> bool {
        self.len.load(Ordering::Acquire) == 0
    }

    pub fn clear(&self) {
        self.futex.lock();
        let len = self.len.load(Ordering::Acquire);
        let ptr = self.inner.load(Ordering::Acquire);

        if !ptr.is_null() && len > 0 {
            unsafe {
                for i in 0..len {
                    std::ptr::drop_in_place(ptr.add(i));
                }
            }
        }
        self.len.store(0, Ordering::Release);
        self.futex.unlock();
    }

    pub fn reserve(&self, additional: usize) {
        self.futex.lock();
        let len = self.len.load(Ordering::Acquire);
        let capacity = self.capacity.load(Ordering::Acquire);

        if len + additional > capacity {
            self.grow_to(len + additional);
        }
        self.futex.unlock();
    }

    pub fn shrink_to_fit(&self) {
        self.futex.lock();
        let len = self.len.load(Ordering::Acquire);
        let capacity = self.capacity.load(Ordering::Acquire);

        if len < capacity {
            self.shrink_to(len);
        }
        self.futex.unlock();
    }

    fn grow(&self) {
        let capacity = self.capacity.load(Ordering::Acquire);
        let new_capacity = if capacity == 0 { 4 } else { capacity * 2 };
        self.grow_to(new_capacity);
    }

    fn grow_to(&self, new_capacity: usize) {
        let old_ptr = self.inner.load(Ordering::Acquire);
        let len = self.len.load(Ordering::Acquire);

        let new_layout = std::alloc::Layout::array::<T>(new_capacity).unwrap();
        let new_ptr = unsafe { std::alloc::alloc(new_layout) as *mut T };

        if !old_ptr.is_null() && len > 0 {
            unsafe {
                std::ptr::copy_nonoverlapping(old_ptr, new_ptr, len);
            }
        }

        // Update pointer and capacity with proper ordering
        self.inner.store(new_ptr, Ordering::Release);
        self.capacity.store(new_capacity, Ordering::Release);

        // Ensure all threads see the new allocation before deallocating old one
        core::sync::atomic::fence(Ordering::SeqCst);

        if !old_ptr.is_null() {
            let old_capacity = self.capacity.load(Ordering::Acquire);
            if old_capacity > 0 {
                let old_layout = std::alloc::Layout::array::<T>(old_capacity).unwrap();
                unsafe {
                    std::alloc::dealloc(old_ptr as *mut u8, old_layout);
                }
            }
        }
    }

    fn shrink_to(&self, new_capacity: usize) {
        let old_ptr = self.inner.load(Ordering::Acquire);
        let len = self.len.load(Ordering::Acquire);

        if new_capacity == 0 {
            self.inner.store(null_mut(), Ordering::Release);
            self.capacity.store(0, Ordering::Release);

            core::sync::atomic::fence(Ordering::SeqCst);

            if !old_ptr.is_null() {
                let old_capacity = self.capacity.load(Ordering::Acquire);
                if old_capacity > 0 {
                    let old_layout = std::alloc::Layout::array::<T>(old_capacity).unwrap();
                    unsafe {
                        std::alloc::dealloc(old_ptr as *mut u8, old_layout);
                    }
                }
            }
            return;
        }

        let new_layout = std::alloc::Layout::array::<T>(new_capacity).unwrap();
        let new_ptr = unsafe { std::alloc::alloc(new_layout) as *mut T };

        if !old_ptr.is_null() && len > 0 {
            unsafe {
                std::ptr::copy_nonoverlapping(old_ptr, new_ptr, len.min(new_capacity));
            }
        }

        self.inner.store(new_ptr, Ordering::Release);
        self.capacity.store(new_capacity, Ordering::Release);

        core::sync::atomic::fence(Ordering::SeqCst);

        if !old_ptr.is_null() {
            let old_capacity = self.capacity.load(Ordering::Acquire);
            if old_capacity > 0 {
                let old_layout = std::alloc::Layout::array::<T>(old_capacity).unwrap();
                unsafe {
                    std::alloc::dealloc(old_ptr as *mut u8, old_layout);
                }
            }
        }
    }
}

impl<T> Drop for Container<T> {
    fn drop(&mut self) {
        self.clear();
        let ptr = self.inner.load(Ordering::Relaxed);
        let capacity = self.capacity.load(Ordering::Relaxed);

        if !ptr.is_null() && capacity > 0 {
            let layout = std::alloc::Layout::array::<T>(capacity).unwrap();
            unsafe {
                std::alloc::dealloc(ptr as *mut u8, layout);
            }
        }
    }
}

pub type IdRef = Arc<Id>;
pub type StringRef = Arc<String>;
pub type TimeRef = Arc<u128>;

/// IdMap provides dual lookup capability:
/// 1. HashMap for name-based lookup (fast O(1) by name)
/// 2. Vec for chronological/index-based lookup (history of all IDs with timestamps)
pub struct IdMap {
    ptr: AtomicPtr<HashMap<StringRef, IdRef>>,
    history_ptr: AtomicPtr<Vec<IdRef>>,  // Time-ordered history of all IDs
    size: AtomicUsize,
    capacity: AtomicUsize,
}
impl IdMap {
    pub fn new() -> Self {
        IdMap {
            ptr: AtomicPtr::new(null_mut()),
            history_ptr: AtomicPtr::new(null_mut()),
            size: AtomicUsize::new(0),
            capacity: AtomicUsize::new(16),
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        let map = HashMap::with_capacity(capacity);
        let ptr = Box::into_raw(Box::new(map));
        let history = Vec::with_capacity(capacity);
        let history_ptr = Box::into_raw(Box::new(history));
        IdMap {
            ptr: AtomicPtr::new(ptr),
            history_ptr: AtomicPtr::new(history_ptr),
            size: AtomicUsize::new(0),
            capacity: AtomicUsize::new(capacity),
        }
    }

    fn ensure_initialized(&self) {
        if self.ptr.load(Ordering::Acquire).is_null() {
            let new_map = HashMap::with_capacity(self.capacity.load(Ordering::Relaxed));
            let new_ptr = Box::into_raw(Box::new(new_map));
            if self
                .ptr
                .compare_exchange(null_mut(), new_ptr, Ordering::Release, Ordering::Relaxed)
                .is_err()
            {
                // Another thread initialized it, clean up our allocation
                unsafe {
                    drop(Box::from_raw(new_ptr));
                }
            }
        }

        // Initialize history if needed
        if self.history_ptr.load(Ordering::Acquire).is_null() {
            let new_history = Vec::with_capacity(self.capacity.load(Ordering::Relaxed));
            let new_history_ptr = Box::into_raw(Box::new(new_history));
            if self
                .history_ptr
                .compare_exchange(null_mut(), new_history_ptr, Ordering::Release, Ordering::Relaxed)
                .is_err()
            {
                // Another thread initialized it, clean up our allocation
                unsafe {
                    drop(Box::from_raw(new_history_ptr));
                }
            }
        }
    }

    pub fn capacity(&self) -> usize {
        self.capacity.load(Ordering::Relaxed)
    }

    pub fn clear(&self) {
        let ptr = self.ptr.load(Ordering::Acquire);
        if !ptr.is_null() {
            let map = unsafe { &mut *ptr };
            map.clear();
            self.size.store(0, Ordering::Relaxed);
        }
        let history_ptr = self.history_ptr.load(Ordering::Acquire);
        if !history_ptr.is_null() {
            let history = unsafe { &mut *history_ptr };
            history.clear();
        }
    }

    pub fn contains_key(&self, key: &StringRef) -> bool {
        let ptr = self.ptr.load(Ordering::Acquire);
        if ptr.is_null() {
            return false;
        }
        let map = unsafe { &*ptr };
        map.contains_key(key)
    }

    /// Get by string content (not Arc pointer) - searches all entries
    pub fn get_by_str(&self, name: &str) -> Option<IdRef> {
        let ptr = self.ptr.load(Ordering::Acquire);
        if ptr.is_null() {
            return None;
        }
        let map = unsafe { &*ptr };
        // Search by string content, not pointer equality
        for (key, value) in map.iter() {
            if key.as_str() == name {
                return Some(value.clone());
            }
        }
        None
    }

    pub fn get(&self, key: &StringRef) -> Option<IdRef> {
        let ptr = self.ptr.load(Ordering::Acquire);
        if ptr.is_null() {
            return None;
        }
        let map = unsafe { &*ptr };
        map.get(key).cloned()
    }

    pub fn insert(&self, key: StringRef, value: IdRef) -> Option<IdRef> {
        self.ensure_initialized();
        let ptr = self.ptr.load(Ordering::Acquire);
        let map = unsafe { &mut *ptr };
        let old_value = map.insert(key, value.clone());

        // Add to history Vec for chronological lookup
        let history_ptr = self.history_ptr.load(Ordering::Acquire);
        if !history_ptr.is_null() {
            let history = unsafe { &mut *history_ptr };
            history.push(value);  // Always push to history, even if key existed
        }

        if old_value.is_none() {
            self.size.fetch_add(1, Ordering::Relaxed);
        }
        old_value
    }

    pub fn is_empty(&self) -> bool {
        self.size.load(Ordering::Relaxed) == 0
    }

    pub fn is_valid(&self) -> bool {
        let size = self.size.load(Ordering::Relaxed);
        let capacity = self.capacity.load(Ordering::Relaxed);
        size <= capacity && (!self.ptr.load(Ordering::Acquire).is_null() || size == 0)
    }

    pub fn iter(&self) -> Vec<(StringRef, IdRef)> {
        let ptr = self.ptr.load(Ordering::Acquire);
        if ptr.is_null() {
            return Vec::new();
        }
        let map = unsafe { &*ptr };
        map.iter().map(|(k, v)| (k.clone(), v.clone())).collect()
    }

    pub fn iter_sorted(&self) -> Vec<(StringRef, IdRef)> {
        let mut items = self.iter();
        items.sort_by(|a, b| a.0.cmp(&b.0));
        items
    }

    pub fn iter_sorted_by_id(&self) -> Vec<(StringRef, IdRef)> {
        let mut items = self.iter();
        items.sort_by(|a, b| a.1.cmp(&b.1));
        items
    }

    pub fn keys(&self) -> Vec<StringRef> {
        let ptr = self.ptr.load(Ordering::Acquire);
        if ptr.is_null() {
            return Vec::new();
        }
        let map = unsafe { &*ptr };
        map.keys().cloned().collect()
    }
    pub fn values(&self) -> Vec<IdRef> {
        let ptr = self.ptr.load(Ordering::Acquire);
        if ptr.is_null() {
            return Vec::new();
        }
        let map = unsafe { &*ptr };
        map.values().cloned().collect()
    }
    pub fn keys_sorted(&self) -> Vec<StringRef> {
        let mut keys: Vec<StringRef> = self.iter().into_iter().map(|(k, _)| k).collect();
        keys.sort();
        keys
    }

    pub fn values_sorted(&self) -> Vec<IdRef> {
        let mut values: Vec<IdRef> = self.iter().into_iter().map(|(_, v)| v).collect();
        values.sort();
        values
    }

    pub fn len(&self) -> usize {
        self.size.load(Ordering::Relaxed)
    }

    /// Get ID by index from chronological history (0 = oldest, sorted by timestamp)
    /// Returns None if index is out of bounds
    pub fn get_by_index(&self, index: usize) -> Option<IdRef> {
        let history_ptr = self.history_ptr.load(Ordering::Acquire);
        if history_ptr.is_null() {
            return None;
        }
        let history = unsafe { &*history_ptr };
        history.get(index).cloned()
    }

    /// Get the number of entries in the history (includes duplicates by name)
    pub fn history_len(&self) -> usize {
        let history_ptr = self.history_ptr.load(Ordering::Acquire);
        if history_ptr.is_null() {
            return 0;
        }
        let history = unsafe { &*history_ptr };
        history.len()
    }

    /// Get all IDs in chronological order
    pub fn history(&self) -> Vec<IdRef> {
        let history_ptr = self.history_ptr.load(Ordering::Acquire);
        if history_ptr.is_null() {
            return Vec::new();
        }
        let history = unsafe { &*history_ptr };
        history.clone()
    }

    /// Get IDs by name - returns all historical instances sorted by timestamp
    pub fn get_all_by_name(&self, name: &str) -> Vec<IdRef> {
        let history_ptr = self.history_ptr.load(Ordering::Acquire);
        if history_ptr.is_null() {
            return Vec::new();
        }
        let history = unsafe { &*history_ptr };
        history.iter()
            .filter(|id| id.name() == name)
            .cloned()
            .collect()
    }

    pub fn remove(&self, key: &StringRef) -> Option<IdRef> {
        let ptr = self.ptr.load(Ordering::Acquire);
        if ptr.is_null() {
            return None;
        }
        let map = unsafe { &mut *ptr };
        let removed = map.remove(key);
        if removed.is_some() {
            self.size.fetch_sub(1, Ordering::Relaxed);
        }
        removed
    }

    pub fn reserve(&self, additional: usize) {
        self.ensure_initialized();
        let ptr = self.ptr.load(Ordering::Acquire);
        let map = unsafe { &mut *ptr };
        map.reserve(additional);
        let new_capacity = self.capacity.load(Ordering::Relaxed) + additional;
        self.capacity.store(new_capacity, Ordering::Relaxed);
    }

    pub fn shrink_to_fit(&self) {
        let ptr = self.ptr.load(Ordering::Acquire);
        if !ptr.is_null() {
            let map = unsafe { &mut *ptr };
            map.shrink_to_fit();
            self.capacity.store(map.capacity(), Ordering::Relaxed);
        }
    }
}

impl Clone for IdMap {
    fn clone(&self) -> Self {
        let new_capacity = self.capacity.load(Ordering::Relaxed);
        let cloned = IdMap::with_capacity(new_capacity);

        let ptr = self.ptr.load(Ordering::Acquire);
        if !ptr.is_null() {
            let map = unsafe { &*ptr };
            let cloned_ptr = cloned.ptr.load(Ordering::Acquire);
            let cloned_map = unsafe { &mut *cloned_ptr };

            for (key, value) in map.iter() {
                cloned_map.insert(key.clone(), value.clone());
            }
            cloned.size.store(map.len(), Ordering::Relaxed);
        }

        cloned
    }
}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Key {
    pub public: u32,
    private: u128,
    name: String,
}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PadLock {
    id: Id,
    key: Key,
}
impl PadLock {
    pub fn new(key: Key) -> Self {
        PadLock {
            id: Id::encrypted(&key),
            key,
        }
    }
    pub fn unlock(&mut self, key: Key) -> Option<Id> {
        let id = &mut self.id.clone();
        let success = key.decrypt_id(id);
        if success {
            self.id = id.clone();
            Some(id.clone())
        } else {
            None
        }
    }
    pub fn lock(&mut self) -> Id {
        self.id = self.key.encrypt_id();
        self.id.clone()
    }
}
impl Default for PadLock {
    fn default() -> Self {
        PadLock {
            id: Id::default(),
            key: Key::default(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct Locker {
    id: Id,
    key: Key,
    lock: PadLock,
    queue: Vec<Pending>,
    contents: Vec<Entry>,
    timestamp: u128,
}
impl Hash for Locker {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        self.key.hash(state);
        self.lock.hash(state);
        self.queue.hash(state);
        self.contents.hash(state);
        self.timestamp.hash(state);
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Id([u8; 96]);
impl Id {
    pub fn new(name: [u8; 64], uuid: u128, timestamp: u128) -> Id {
        let mut bytes = [0u8; 96];
        bytes[0..16].copy_from_slice(&uuid.to_be_bytes());
        bytes[16..32].copy_from_slice(&timestamp.to_be_bytes());
        bytes[32..96].copy_from_slice(&name);
        Id(bytes)
    }

    pub fn name(&self) -> String {
        String::from_utf8_lossy(&self.0[32..96])
            .trim_end_matches('\0')
            .to_string()
    }

    #[allow(static_mut_refs)]
    pub fn get(name: &str) -> Id {
        // FIXED: Use get_by_str to search by string content, not Arc pointer
        if let Some(id_ref) = unsafe { MAPPED_IDS.get_by_str(name) } {
            return (*id_ref).clone();
        }
        // Create new ID if not found
        let new_id = Id::random(name.to_string());
        let name_ref = Arc::new(name.to_string());
        let id_ref = Arc::new(new_id.clone());
        unsafe { MAPPED_IDS.insert(name_ref, id_ref) };
        new_id
    }

    #[allow(static_mut_refs)]
    pub fn set(id: Id) -> Id {
        let name_ref = Arc::new(id.name());
        let mut updated_id = id.clone();
        updated_id.0[16..32].copy_from_slice(&elapsed().to_be_bytes());
        let id_ref = Arc::new(updated_id.clone());
        if let Some(prev_id_ref) = unsafe { MAPPED_IDS.insert(name_ref, id_ref) } {
            (*prev_id_ref).clone()
        } else {
            updated_id
        }
    }

    pub fn clear(&mut self) {
        self.0 = [0u8; 96];
    }

    pub fn is_empty(&self) -> bool {
        self.0.iter().all(|&b| b == 0)
    }

    pub fn is_invalid(&self) -> bool {
        let timestamp = u128::from_be_bytes([
            self.0[16], self.0[17], self.0[18], self.0[19], self.0[20], self.0[21], self.0[22],
            self.0[23], self.0[24], self.0[25], self.0[26], self.0[27], self.0[28], self.0[29],
            self.0[30], self.0[31],
        ]);
        timestamp == u128::MAX || timestamp == 0 || self.0[32..96].iter().all(|&b| b == 0)
    }

    pub fn timestamp(&self) -> u128 {
        u128::from_be_bytes([
            self.0[16], self.0[17], self.0[18], self.0[19], self.0[20], self.0[21], self.0[22],
            self.0[23], self.0[24], self.0[25], self.0[26], self.0[27], self.0[28], self.0[29],
            self.0[30], self.0[31],
        ])
    }

    pub fn uuid(&self) -> u128 {
        u128::from_be_bytes([
            self.0[0], self.0[1], self.0[2], self.0[3], self.0[4], self.0[5], self.0[6], self.0[7],
            self.0[8], self.0[9], self.0[10], self.0[11], self.0[12], self.0[13], self.0[14],
            self.0[15],
        ])
    }

    pub fn random(name: String) -> Id {
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        hasher.write_u128(time!());
        let split_name = name.split_at(name.len() / 2);
        split_name.1.hash(&mut hasher);
        split_name.0.hash(&mut hasher);
        let hi = hasher.finish();
        hasher.write_u128(time!());
        split_name.0.hash(&mut hasher);
        split_name.1.hash(&mut hasher);
        let lo = hasher.finish();
        let uuid = ((hi as u128) << 64) | (lo as u128);
        let timestamp = time!();
        if uuid == 0 || uuid == u128::MAX {
            Self::random(name)
        } else {
            let mut name_bytes = [0u8; 64];
            let name_utf8 = name.as_bytes();
            let name_len = std::cmp::min(name_utf8.len(), 64);
            name_bytes[..name_len].copy_from_slice(&name_utf8[..name_len]);
            let mut bytes = [0u8; 96];
            bytes[0..16].copy_from_slice(&uuid.to_be_bytes());
            bytes[16..32].copy_from_slice(&timestamp.to_be_bytes());
            bytes[32..96].copy_from_slice(&name_bytes);
            Id(bytes)
        }
    }

    pub fn encrypted(key: &Key) -> Id {
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        hasher.write_u128(key.private);
        let split_name = key.name.split_at(key.name.len() / 2);
        split_name.1.hash(&mut hasher);
        split_name.0.hash(&mut hasher);
        let hi = hasher.finish();
        hasher.write_u128(u128::MAX - key.private);
        split_name.0.hash(&mut hasher);
        split_name.1.hash(&mut hasher);
        let lo = hasher.finish();
        let uuid = ((hi as u128) << 64) | (lo as u128);
        let timestamp = elapsed();
        if uuid == 0 || uuid == u128::MAX {
            Self::encrypted(key)
        } else {
            let mut name_bytes = [0u8; 64];
            let name_utf8 = key.name.as_bytes();
            let name_len = std::cmp::min(name_utf8.len(), 64);
            name_bytes[..name_len].copy_from_slice(&name_utf8[..name_len]);
            let mut bytes = [0u8; 96];
            bytes[0..16].copy_from_slice(&uuid.to_be_bytes());
            bytes[16..32].copy_from_slice(&timestamp.to_be_bytes());
            bytes[32..96].copy_from_slice(&name_bytes);
            Id(bytes)
        }
    }

    pub fn decrypt(&mut self, key: &Key) -> bool {
        let expected_id = Self::encrypted(key);
        let expected_uuid = expected_id.uuid();
        let current_uuid = self.uuid();
        if expected_uuid == current_uuid {
            let mut name_bytes = [0u8; 64];
            let name_utf8 = key.name.as_bytes();
            let name_len = std::cmp::min(name_utf8.len(), 64);
            name_bytes[..name_len].copy_from_slice(&name_utf8[..name_len]);
            self.0[32..96].copy_from_slice(&name_bytes);
            true
        } else {
            false
        }
    }

    pub fn encode_uuid(&self) -> String {
        let bytes = &self.0[0..16];
        format!(
            "{:02x}{:02x}{:02x}{:02x}-{:02x}{:02x}-{:02x}{:02x}-{:02x}{:02x}-{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}",
            bytes[0],
            bytes[1],
            bytes[2],
            bytes[3],
            bytes[4],
            bytes[5],
            bytes[6],
            bytes[7],
            bytes[8],
            bytes[9],
            bytes[10],
            bytes[11],
            bytes[12],
            bytes[13],
            bytes[14],
            bytes[15]
        )
    }

    pub fn decode_uuid(uuid: &str) -> Result<Id, &'static str> {
        let hex_string = uuid.replace("-", "");
        if hex_string.len() != 32 {
            return Err("Invalid UUID length");
        }

        let mut bytes = [0u8; 96];
        for i in 0..16 {
            bytes[i] = u8::from_str_radix(&hex_string[i * 2..i * 2 + 2], 16)
                .map_err(|_| "Invalid hexadecimal character")?;
        }

        Ok(Id(bytes))
    }

    pub fn as_bytes(&self) -> &[u8; 96] {
        &self.0
    }

    pub fn from_bytes(bytes: [u8; 96]) -> Id {
        Id(bytes)
    }
}

impl Default for Id {
    fn default() -> Self {
        Id([0u8; 96])
    }
}
impl Display for Id {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}/{}/{}", self.name(), self.uuid(), self.timestamp())
    }
}
impl Key {
    pub fn new(name: &str, privkey: u128) -> Key {
        Key {
            public: 0,
            private: privkey,
            name: name.to_string(),
        }
    }

    pub fn generate(name: &str, seed: u128) -> Key {
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        hasher.write_u128(seed);
        hasher.write(name.as_bytes());
        let hi = hasher.finish();
        hasher.write_u128(u128::MAX - seed);
        hasher.write(name.as_bytes());
        let lo = hasher.finish();
        let public_key = ((hi) ^ (lo)) as u32;
        Key {
            public: public_key,
            private: seed,
            name: name.to_string(),
        }
    }

    pub fn encrypt_id(&self) -> Id {
        Id::encrypted(self)
    }

    pub fn decrypt_id(&self, id: &mut Id) -> bool {
        id.decrypt(self)
    }
}

impl Default for Key {
    fn default() -> Self {
        Key::generate("default", 0)
    }
}
impl Default for Locker {
    fn default() -> Self {
        Self {
            id: Id::default(),
            key: Key::default(),
            lock: PadLock::default(),
            queue: Vec::new(),
            contents: Vec::new(),
            timestamp: time!(),
        }
    }
}
// NOTE: PartialOrd and Ord implementations removed due to Range<usize> fields in HandlerResult
// These comparison operations are incompatible with the new Range<usize> architecture
#[allow(uninhabited_static, static_mut_refs)]
static mut MAPPED_IDS: LazyLock<IdMap> = LazyLock::new(|| IdMap::with_capacity(4096));
#[allow(unused)]
static mut ENCRYPTED_IDS: LazyLock<Vec<PadLock>> = LazyLock::new(|| Vec::with_capacity(4096));
#[allow(unused)]
static mut OBSOLETE_IDS: LazyLock<Vec<Id>> = LazyLock::new(|| Vec::with_capacity(8192));

use std::sync::RwLock;

pub struct Lock<T> {
    data: UnsafeCell<Option<T>>,
    lock: RwLock<()>,
}

unsafe impl<T: Send> Send for Lock<T> {}
unsafe impl<T: Send + Sync> Sync for Lock<T> {}

impl<T> Lock<T> {
    pub const fn new_empty() -> Self {
        Lock {
            data: UnsafeCell::new(None),
            lock: RwLock::new(()),
        }
    }

    pub fn new(value: T) -> Self {
        Lock {
            data: UnsafeCell::new(Some(value)),
            lock: RwLock::new(()),
        }
    }

    #[inline]
    pub fn is_initialized(&self) -> bool {
        let _guard = self.lock.read().unwrap();
        unsafe { (*self.data.get()).is_some() }
    }

    fn ensure_initialized(&self)
    where
        T: Default,
    {
        let _guard = self.lock.write().unwrap();
        unsafe {
            if (*self.data.get()).is_none() {
                *self.data.get() = Some(T::default());
            }
        }
    }

    pub fn get_or_init<F>(&'_ self, f: F) -> RwLockReadGuard<'_, ()>
    where
        F: FnOnce() -> T,
    {
        {
            let _guard = self.lock.write().unwrap();
            unsafe {
                if (*self.data.get()).is_none() {
                    *self.data.get() = Some(f());
                }
            }
        }
        self.lock.read().unwrap()
    }

    pub fn lock(&self) -> RwLockReadGuard<'_, ()>
    where
        T: Default,
    {
        self.ensure_initialized();
        self.lock.read().unwrap()
    }

    pub fn lock_mut(&self) -> RwLockWriteGuard<'_, ()>
    where
        T: Default,
    {
        self.ensure_initialized();
        self.lock.write().unwrap()
    }

    pub fn read(&self) -> Option<T>
    where
        T: Clone,
    {
        let _guard = self.lock.read().unwrap();
        unsafe { (*self.data.get()).clone() }
    }

    pub fn write<F, R>(&self, f: F) -> Option<R>
    where
        F: FnOnce(&mut T) -> R,
        T: Default,
    {
        let _guard = self.lock.write().unwrap();
        unsafe {
            if (*self.data.get()).is_none() {
                *self.data.get() = Some(T::default());
            }
            (*self.data.get()).as_mut().map(f)
        }
    }

    pub fn get(&self) -> Option<T>
    where
        T: Clone,
    {
        self.read()
    }

    pub fn force(&self, value: T) {
        let _guard = self.lock.write().unwrap();
        unsafe {
            *self.data.get() = Some(value);
        }
    }

    pub fn insert(&self, value: T) -> bool {
        let _guard = self.lock.write().unwrap();
        unsafe {
            if (*self.data.get()).is_none() {
                *self.data.get() = Some(value);
                true
            } else {
                false
            }
        }
    }

    pub fn remove(&self) -> Option<T> {
        let _guard = self.lock.write().unwrap();
        unsafe { (*self.data.get()).take() }
    }

    pub fn clear(&self) {
        let _guard = self.lock.write().unwrap();
        unsafe {
            *self.data.get() = None;
        }
    }

    pub fn len(&self) -> usize {
        let _guard = self.lock.read().unwrap();
        unsafe { if (*self.data.get()).is_some() { 1 } else { 0 } }
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        let _guard = self.lock.read().unwrap();
        unsafe { (*self.data.get()).is_none() }
    }

    pub fn into_inner(self) -> Option<T> {
        self.data.into_inner()
    }

    pub fn try_lock(&'_ self) -> Option<RwLockReadGuard<'_, ()>> {
        self.lock.try_read().ok()
    }

    pub fn try_lock_mut(&'_ self) -> Option<RwLockWriteGuard<'_, ()>> {
        self.lock.try_write().ok()
    }

    pub fn try_read(&self) -> Option<T>
    where
        T: Clone,
    {
        let _guard = self.lock.try_read().ok()?;
        unsafe { (*self.data.get()).clone() }
    }

    pub fn try_write<F, R>(&self, f: F) -> Option<R>
    where
        F: FnOnce(&mut T) -> R,
    {
        let _guard = self.lock.try_write().ok()?;
        unsafe { (*self.data.get()).as_mut().map(f) }
    }

    pub fn get_or_try_init<F>(&self, f: F) -> Option<T>
    where
        F: FnOnce() -> Option<T>,
        T: Clone,
    {
        let _guard = self.lock.try_write().ok()?;
        unsafe {
            if (*self.data.get()).is_none() {
                *self.data.get() = f();
            }
            (*self.data.get()).clone()
        }
    }

    #[inline]
    pub fn with<F, R>(&self, f: F) -> Option<R>
    where
        F: FnOnce(&T) -> R,
    {
        let _guard = self.lock.read().unwrap_or_else(|poisoned| {
            eprintln!("OptionLock read lock poisoned, recovering...");
            poisoned.into_inner()
        });
        unsafe { (*self.data.get()).as_ref().map(f) }
    }

    #[inline]
    pub fn with_mut<F, R>(&self, f: F) -> Option<R>
    where
        F: FnOnce(&mut T) -> R,
    {
        let _guard = self.lock.write().unwrap_or_else(|poisoned| {
            eprintln!("OptionLock write lock poisoned, recovering...");
            poisoned.into_inner()
        });
        unsafe { (*self.data.get()).as_mut().map(f) }
    }

    #[inline]
    pub fn map<F, U>(&self, f: F) -> Option<U>
    where
        F: FnOnce(&T) -> U,
    {
        self.with(f)
    }

    pub fn replace(&self, value: T) -> Option<T> {
        let _guard = self.lock.try_write().ok()?;
        unsafe { (*self.data.get()).replace(value) }
    }

    pub fn swap(&self, other: &Self) {
        let (first, second) = if std::ptr::addr_of!(*self) < std::ptr::addr_of!(*other) {
            (self, other)
        } else {
            (other, self)
        };

        let _guard1 = first.lock.try_write().unwrap();
        let _guard2 = second.lock.try_write().unwrap();

        unsafe {
            std::ptr::swap(first.data.get(), second.data.get());
        }
    }

    pub fn clone_inner(&self) -> Option<T>
    where
        T: Clone,
    {
        let _guard = self.lock.try_read().ok()?;
        unsafe { (*self.data.get()).clone() }
    }
}

impl<T: Default> Default for Lock<T> {
    fn default() -> Self {
        Self::new(T::default())
    }
}
