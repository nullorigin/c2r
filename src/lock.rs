#![allow(unused_variables, unused_assignments)]

use crate::{thread::Futex, time};
use core::cell::UnsafeCell;
use core::marker::{Send, Sync};
use core::ops::FnOnce;
use core::option::Option::{self, None, Some};
use core::{clone::Clone, default::Default};
use std::borrow::ToOwned;
use std::collections::HashMap;
use std::collections::hash_map::DefaultHasher;
use std::fmt::Display;
use std::hash::{Hash, Hasher};
use std::ptr::null_mut;
use std::sync::Arc;
use std::sync::OnceLock;
use std::sync::atomic::{AtomicPtr, AtomicUsize, Ordering};
use std::time::SystemTime;

static TIME_START: OnceLock<SystemTime> = OnceLock::new();

fn get_time_start() -> &'static SystemTime {
    TIME_START.get_or_init(|| SystemTime::now())
}

// Note: time!() macro is now in macros.rs
pub fn elapsed() -> u128 {
    SystemTime::now()
        .duration_since(*get_time_start())
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

/// IdMap provides dual lookup capability:
/// 1. HashMap for name-based lookup (fast O(1) by name)
/// 2. Vec for chronological/index-based lookup (history of all IDs with timestamps)
pub struct IdMap {
    map: std::sync::Mutex<HashMap<String, IdRef>>,
    history: std::sync::Mutex<Vec<IdRef>>,
}

impl IdMap {
    #[inline]
    pub fn new() -> Self {
        IdMap {
            map: std::sync::Mutex::new(HashMap::new()),
            history: std::sync::Mutex::new(Vec::new()),
        }
    }

    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        IdMap {
            map: std::sync::Mutex::new(HashMap::with_capacity(capacity)),
            history: std::sync::Mutex::new(Vec::with_capacity(capacity)),
        }
    }

    #[inline]
    pub fn capacity(&self) -> usize {
        self.map.lock().unwrap().capacity()
    }

    #[inline]
    pub fn clear(&self) {
        self.map.lock().unwrap().clear();
        self.history.lock().unwrap().clear();
    }

    #[inline]
    pub fn contains_key(&self, key: &str) -> bool {
        self.map.lock().unwrap().contains_key(key)
    }

    #[inline]
    pub fn get_by_str(&self, name: &str) -> Option<IdRef> {
        self.map.lock().unwrap().get(name).cloned()
    }

    #[inline]
    pub fn get(&self, key: &str) -> Option<IdRef> {
        self.map.lock().unwrap().get(key).cloned()
    }

    pub fn insert(&self, key: Arc<String>, value: IdRef) -> Option<IdRef> {
        let old = self
            .map
            .lock()
            .unwrap()
            .insert((*key).clone(), value.clone());
        self.history.lock().unwrap().push(value);
        old
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.map.lock().unwrap().is_empty()
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.map.lock().unwrap().len()
    }

    #[inline]
    pub fn is_valid(&self) -> bool {
        true
    }

    pub fn iter(&self) -> Vec<(String, IdRef)> {
        self.map
            .lock()
            .unwrap()
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect()
    }

    pub fn iter_sorted(&self) -> Vec<(String, IdRef)> {
        let mut items = self.iter();
        items.sort_by(|a, b| a.0.cmp(&b.0));
        items
    }

    pub fn iter_sorted_by_id(&self) -> Vec<(String, IdRef)> {
        let mut items = self.iter();
        items.sort_by(|a, b| a.1.cmp(&b.1));
        items
    }

    pub fn keys(&self) -> Vec<String> {
        self.map.lock().unwrap().keys().cloned().collect()
    }

    pub fn values(&self) -> Vec<IdRef> {
        self.map.lock().unwrap().values().cloned().collect()
    }

    pub fn keys_sorted(&self) -> Vec<String> {
        let mut keys = self.keys();
        keys.sort();
        keys
    }

    pub fn values_sorted(&self) -> Vec<IdRef> {
        let mut values = self.values();
        values.sort();
        values
    }

    #[inline]
    pub fn get_by_index(&self, index: usize) -> Option<IdRef> {
        self.history.lock().unwrap().get(index).cloned()
    }

    #[inline]
    pub fn history_len(&self) -> usize {
        self.history.lock().unwrap().len()
    }

    pub fn history(&self) -> Vec<IdRef> {
        self.history.lock().unwrap().clone()
    }

    pub fn get_all_by_name(&self, name: &str) -> Vec<IdRef> {
        let h = self.history.lock().unwrap();
        let mut matching: Vec<IdRef> = h.iter().filter(|id| id.name() == name).cloned().collect();
        matching.sort_by_key(|id| id.timestamp());
        matching
    }

    pub fn remove(&self, key: &str) -> Option<IdRef> {
        self.map.lock().unwrap().remove(key)
    }

    pub fn reserve(&self, additional: usize) {
        if additional == 0 {
            return;
        }
        self.map.lock().unwrap().reserve(additional);
        self.history.lock().unwrap().reserve(additional);
    }

    pub fn shrink_to_fit(&self) {
        self.map.lock().unwrap().shrink_to_fit();
        self.history.lock().unwrap().shrink_to_fit();
    }
}

impl Default for IdMap {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl Clone for IdMap {
    fn clone(&self) -> Self {
        IdMap {
            map: std::sync::Mutex::new(self.map.lock().unwrap().clone()),
            history: std::sync::Mutex::new(self.history.lock().unwrap().clone()),
        }
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
pub struct Locker<T> {
    id: Id,
    key: Key,
    lock: PadLock,
    contents: Vec<T>,
    timestamp: u128,
}
impl<T: Hash> Hash for Locker<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        self.key.hash(state);
        self.lock.hash(state);
        self.contents.hash(state);
        self.timestamp.hash(state);
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Id {
    data: Vec<u8>,
}

impl Id {
    const UUID_SIZE: usize = 16;
    const TIMESTAMP_SIZE: usize = 16;
    const UUID_OFFSET: usize = 0;
    const TIMESTAMP_OFFSET: usize = 16;
    const NAME_OFFSET: usize = 32;

    #[inline]
    pub fn new(name: &str, uuid: u128, timestamp: u128) -> Id {
        let name_bytes = name.as_bytes();
        let mut data = Vec::with_capacity(Self::NAME_OFFSET + name_bytes.len());
        data.extend_from_slice(&uuid.to_be_bytes());
        data.extend_from_slice(&timestamp.to_be_bytes());
        data.extend_from_slice(name_bytes);
        Id { data }
    }

    #[inline]
    pub fn name(&self) -> String {
        if self.data.len() <= Self::NAME_OFFSET {
            return String::new();
        }
        String::from_utf8_lossy(&self.data[Self::NAME_OFFSET..]).into_owned()
    }

    pub fn get(name: &str) -> Id {
        if let Some(id_ref) = get_mapped_ids().get_by_str(name) {
            return (*id_ref).clone();
        }

        let new_id = Self::generate(name);
        let name_ref = Arc::new(name.to_string());
        let id_ref = Arc::new(new_id.clone());
        get_mapped_ids().insert(name_ref, id_ref);
        new_id
    }

    #[inline]
    fn generate(name: &str) -> Id {
        let mut hasher = DefaultHasher::new();
        name.hash(&mut hasher);
        let uuid = hasher.finish() as u128;

        Id::new(name, uuid, elapsed())
    }

    pub fn set(mut id: Id) -> Id {
        if id.data.len() >= Self::TIMESTAMP_OFFSET + Self::TIMESTAMP_SIZE {
            let elapsed_time = elapsed();
            id.data[Self::TIMESTAMP_OFFSET..Self::TIMESTAMP_OFFSET + Self::TIMESTAMP_SIZE]
                .copy_from_slice(&elapsed_time.to_be_bytes());
        }
        let name = id.name();
        let name_ref = Arc::new(name);
        let id_ref = Arc::new(id.clone());
        get_mapped_ids().insert(name_ref, id_ref);
        id
    }

    #[inline]
    pub fn clear(&mut self) {
        self.data.fill(0);
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.data.is_empty() || self.data.iter().all(|&b| b == 0)
    }

    #[inline]
    pub fn is_invalid(&self) -> bool {
        if self.data.len() < Self::TIMESTAMP_OFFSET + Self::TIMESTAMP_SIZE {
            return true;
        }
        let timestamp = self.timestamp();
        timestamp == u128::MAX || timestamp == 0 || self.data.len() <= Self::NAME_OFFSET
    }

    #[inline]
    pub fn timestamp(&self) -> u128 {
        if self.data.len() < Self::TIMESTAMP_OFFSET + Self::TIMESTAMP_SIZE {
            return 0;
        }
        u128::from_be_bytes(
            self.data[Self::TIMESTAMP_OFFSET..Self::TIMESTAMP_OFFSET + Self::TIMESTAMP_SIZE]
                .try_into()
                .unwrap_or([0u8; 16]),
        )
    }

    #[inline]
    pub fn uuid(&self) -> u128 {
        if self.data.len() < Self::UUID_OFFSET + Self::UUID_SIZE {
            return 0;
        }
        u128::from_be_bytes(
            self.data[Self::UUID_OFFSET..Self::UUID_OFFSET + Self::UUID_SIZE]
                .try_into()
                .unwrap_or([0u8; 16]),
        )
    }

    pub fn random(name: String) -> Id {
        let current_time = elapsed();
        let uuid = Self::gen_uuid(&name, current_time);

        if uuid == 0 || uuid == u128::MAX {
            return Self::random(name);
        }

        Self::new(&name, uuid, current_time)
    }

    #[inline]
    fn gen_uuid(name: &str, timestamp: u128) -> u128 {
        let mut hasher = DefaultHasher::new();
        hasher.write_u128(timestamp);

        let split_pos = name.len() / 2;
        let (first_half, second_half) = name.split_at(split_pos);

        second_half.hash(&mut hasher);
        first_half.hash(&mut hasher);
        let hi = hasher.finish();

        hasher.write_u128(timestamp.wrapping_add(1));
        first_half.hash(&mut hasher);
        second_half.hash(&mut hasher);
        let lo = hasher.finish();

        ((hi as u128) << 64) | (lo as u128)
    }

    #[inline]
    pub fn encrypted(key: &Key) -> Id {
        let mut id = Id::random("encrypted".to_string());
        id.encrypt(key);
        id
    }

    #[inline]
    pub fn encrypt(&mut self, key: &Key) -> bool {
        if self.data.len() < Self::TIMESTAMP_OFFSET + Self::TIMESTAMP_SIZE {
            return false;
        }
        self.xor_with_key(key)
    }

    #[inline]
    pub fn decrypt(&mut self, key: &Key) -> bool {
        if self.data.len() < Self::TIMESTAMP_OFFSET + Self::TIMESTAMP_SIZE {
            return false;
        }
        self.xor_with_key(key)
    }

    #[inline]
    fn xor_with_key(&mut self, key: &Key) -> bool {
        let private_bytes = key.private.to_be_bytes();
        let xor_len = Self::UUID_SIZE.min(self.data.len());
        for i in 0..xor_len {
            self.data[i] ^= private_bytes[i];
        }
        true
    }
}

impl Default for Id {
    fn default() -> Self {
        Id::new("", 0, 0)
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
        let mut hasher = DefaultHasher::new();
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

impl<T: Default> Default for Locker<T> {
    fn default() -> Self {
        Self {
            id: Id::default(),
            key: Key::default(),
            lock: PadLock::default(),
            contents: Vec::<T>::new(),
            timestamp: time!(),
        }
    }
}
// NOTE: PartialOrd and Ord implementations removed due to Range<usize> fields in HandlerResult
// These comparison operations are incompatible with the new Range<usize> architecture
/// Global ID registry - thread-safe via IdMap's internal synchronization
static MAPPED_IDS: OnceLock<IdMap> = OnceLock::new();
/// Encrypted IDs storage
#[allow(dead_code)]
static ENCRYPTED_IDS: OnceLock<RwLock<Vec<PadLock>>> = OnceLock::new();
/// Obsolete/recycled IDs
#[allow(dead_code)]
static OBSOLETE_IDS: OnceLock<RwLock<Vec<Id>>> = OnceLock::new();

fn get_mapped_ids() -> &'static IdMap {
    MAPPED_IDS.get_or_init(|| IdMap::with_capacity(4096))
}

#[allow(dead_code)]
fn get_encrypted_ids() -> &'static RwLock<Vec<PadLock>> {
    ENCRYPTED_IDS.get_or_init(|| RwLock::new(Vec::with_capacity(4096)))
}

#[allow(dead_code)]
fn get_obsolete_ids() -> &'static RwLock<Vec<Id>> {
    OBSOLETE_IDS.get_or_init(|| RwLock::new(Vec::with_capacity(8192)))
}

use core::ops::{Deref, DerefMut};
use std::sync::RwLock;

/// RAII guard for Lock. Automatically releases the lock when dropped.
pub struct LockGuard<'a, T> {
    lock: &'a Lock<T>,
}

impl<'a, T> Deref for LockGuard<'a, T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        unsafe { &*self.lock.data.get() }
    }
}

impl<'a, T> DerefMut for LockGuard<'a, T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.lock.data.get() }
    }
}

impl<'a, T> Drop for LockGuard<'a, T> {
    #[inline]
    fn drop(&mut self) {
        self.lock.futex.unlock();
    }
}

impl<'a, T: core::fmt::Debug> core::fmt::Debug for LockGuard<'a, T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("LockGuard").field("value", &**self).finish()
    }
}

pub struct Lock<T> {
    data: UnsafeCell<T>,
    futex: Futex,
}

unsafe impl<T: Send> Send for Lock<T> {}
unsafe impl<T: Send> Sync for Lock<T> {}

impl<T> Lock<T> {
    /// Creates a new `Lock` with the given value.
    #[inline]
    pub const fn new(value: T) -> Self {
        Lock {
            data: UnsafeCell::new(value),
            futex: Futex::new(),
        }
    }

    /// Acquires the lock, blocking until it becomes available.
    /// Returns a guard that releases the lock when dropped.
    #[inline]
    pub fn lock(&self) -> LockGuard<'_, T> {
        self.futex.lock();
        LockGuard { lock: self }
    }

    /// Attempts to acquire the lock without blocking.
    /// Returns `Some(guard)` if successful, `None` otherwise.
    #[inline]
    pub fn try_lock(&self) -> Option<LockGuard<'_, T>> {
        if self.futex.try_lock() {
            Some(LockGuard { lock: self })
        } else {
            None
        }
    }

    /// Executes a closure with exclusive access to the locked data.
    #[inline]
    pub fn with_lock<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&mut T) -> R,
    {
        let mut guard = self.lock();
        f(&mut *guard)
    }

    /// Gets a clone of the locked value.
    #[inline]
    pub fn get(&self) -> T
    where
        T: Clone,
    {
        let guard = self.lock();
        (*guard).clone()
    }

    /// Acquires the lock and returns a mutable guard.
    #[inline]
    pub fn get_mut(&self) -> LockGuard<'_, T> {
        self.lock()
    }

    /// Sets the locked value.
    #[inline]
    pub fn set(&self, value: T) {
        let mut guard = self.lock();
        *guard = value;
    }

    /// Replaces the locked value and returns the old value.
    #[inline]
    pub fn replace(&self, value: T) -> T {
        let mut guard = self.lock();
        core::mem::replace(&mut *guard, value)
    }

    /// Swaps the contents of two locks, avoiding deadlock by ordering lock acquisition.
    pub fn swap(&self, other: &Self) {
        if core::ptr::eq(self, other) {
            return;
        }

        let (first, second) = if (self as *const Self as usize) < (other as *const Self as usize) {
            (self, other)
        } else {
            (other, self)
        };

        let mut first_guard = first.lock();
        let mut second_guard = second.lock();
        core::mem::swap(&mut *first_guard, &mut *second_guard);
    }

    /// Consumes the lock and returns the inner value.
    #[inline]
    pub fn into_inner(self) -> T {
        self.data.into_inner()
    }

    /// Returns a raw pointer to the underlying data.
    #[inline]
    pub fn as_ptr(&self) -> *const T {
        self.data.get()
    }

    /// Returns a mutable raw pointer to the underlying data.
    #[inline]
    pub fn as_mut_ptr(&self) -> *mut T {
        self.data.get()
    }

    /// Gets a reference to the underlying data without locking.
    /// # Safety
    /// The caller must ensure proper synchronization.
    #[inline]
    pub unsafe fn get_unchecked(&self) -> &T {
        unsafe { &*self.data.get() }
    }

    /// Gets a mutable reference to the underlying data without locking.
    /// # Safety
    /// The caller must ensure proper synchronization and exclusive access.
    #[inline]
    pub unsafe fn get_unchecked_mut(&self) -> &mut T {
        unsafe { &mut *self.data.get() }
    }

    /// Check if the lock is currently held (best-effort, may race).
    #[inline]
    pub fn is_locked(&self) -> bool {
        if self.futex.try_lock() {
            self.futex.unlock();
            false
        } else {
            true
        }
    }
}

impl<T: Default> Default for Lock<T> {
    #[inline]
    fn default() -> Self {
        Self::new(T::default())
    }
}

impl<T: Clone> Clone for Lock<T> {
    #[inline]
    fn clone(&self) -> Self {
        Self::new(self.get())
    }
}

impl<T: core::fmt::Debug> core::fmt::Debug for Lock<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self.try_lock() {
            Some(guard) => f.debug_struct("Lock").field("data", &*guard).finish(),
            None => f.debug_struct("Lock").field("data", &"<locked>").finish(),
        }
    }
}

impl<T: PartialEq + Clone> PartialEq for Lock<T> {
    fn eq(&self, other: &Self) -> bool {
        if core::ptr::eq(self, other) {
            return true;
        }

        let (first, second) = if (self as *const Self as usize) < (other as *const Self as usize) {
            (self, other)
        } else {
            (other, self)
        };

        let first_guard = first.lock();
        let second_guard = second.lock();
        *first_guard == *second_guard
    }
}

impl<T: Eq + Clone> Eq for Lock<T> {}

impl<T: Clone + PartialOrd> PartialOrd for Lock<T> {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        if core::ptr::eq(self, other) {
            return Some(core::cmp::Ordering::Equal);
        }

        let (first, second) = if (self as *const Self as usize) < (other as *const Self as usize) {
            (self, other)
        } else {
            (other, self)
        };

        let first_guard = first.lock();
        let second_guard = second.lock();
        let cmp = (*first_guard).partial_cmp(&*second_guard)?;
        Some(if core::ptr::eq(self, first) {
            cmp
        } else {
            cmp.reverse()
        })
    }
}

impl<T: Clone + Ord> Ord for Lock<T> {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        if core::ptr::eq(self, other) {
            return core::cmp::Ordering::Equal;
        }

        let (first, second) = if (self as *const Self as usize) < (other as *const Self as usize) {
            (self, other)
        } else {
            (other, self)
        };

        let first_guard = first.lock();
        let second_guard = second.lock();
        let cmp = (*first_guard).cmp(&*second_guard);
        if core::ptr::eq(self, first) {
            cmp
        } else {
            cmp.reverse()
        }
    }
}

impl<T: Hash + Clone> Hash for Lock<T> {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        let guard = self.lock();
        (*guard).hash(state);
    }
}

// =============================================================================
// ReentrantRwLock - A reentrant read-write lock using Futex
// =============================================================================

use std::collections::hash_map::DefaultHasher as ThreadHasher;
use std::sync::atomic::AtomicU64;

/// No thread owns the write lock
const RW_NO_OWNER: u64 = 0;

/// Get a unique numeric ID for the current thread
fn current_thread_id() -> u64 {
    thread_local! {
        static THREAD_ID: u64 = {
            let id = std::thread::current().id();
            let mut hasher = ThreadHasher::new();
            id.hash(&mut hasher);
            let hash = hasher.finish();
            if hash == 0 { 1 } else { hash }
        };
    }
    THREAD_ID.with(|id| *id)
}

/// A reentrant read-write lock that allows:
/// - Multiple concurrent readers OR one writer
/// - The same thread to acquire write locks multiple times (reentrant)
/// - The same thread to acquire read locks while holding a write lock
/// - Proper nesting without deadlocks
///
/// Unlike std::sync::RwLock, this lock won't deadlock when the same thread
/// tries to acquire locks in nested closures.
pub struct ReentrantRwLock<T> {
    data: UnsafeCell<T>,
    /// Futex for synchronization
    futex: Futex,
    /// Thread ID of the current write lock owner (0 = no owner)
    write_owner: AtomicU64,
    /// Reentrant write lock count for the owning thread
    write_count: AtomicUsize,
    // Number of active readers (only valid when no writer)
    reader_count: AtomicUsize,
}

unsafe impl<T: Send> Send for ReentrantRwLock<T> {}
unsafe impl<T: Send + Sync> Sync for ReentrantRwLock<T> {}

impl<T> ReentrantRwLock<T> {
    /// Creates a new ReentrantRwLock with the given value.
    #[inline]
    pub const fn new(value: T) -> Self {
        ReentrantRwLock {
            data: UnsafeCell::new(value),
            futex: Futex::new(),
            write_owner: AtomicU64::new(RW_NO_OWNER),
            write_count: AtomicUsize::new(0),
            reader_count: AtomicUsize::new(0),
        }
    }

    /// Acquires a read lock, blocking until available.
    ///
    /// If the current thread already holds the write lock, this succeeds immediately
    /// (the thread can read its own writes).
    ///
    /// Multiple threads can hold read locks simultaneously.
    #[inline]
    pub fn read(&self) -> RwReadGuard<'_, T> {
        let current = current_thread_id();

        // If we already own the write lock, we can read without additional locking
        if self.write_owner.load(Ordering::Acquire) == current {
            // Already have write access, just return a read guard
            // Don't increment reader_count since we're the writer
            return RwReadGuard {
                lock: self,
                is_writer_reading: true,
            };
        }

        loop {
            // Wait until no writer holds the lock
            self.futex.lock();

            let writer = self.write_owner.load(Ordering::Acquire);
            if writer == RW_NO_OWNER || writer == current {
                // No writer or we are the writer - safe to read
                self.reader_count.fetch_add(1, Ordering::AcqRel);
                self.futex.unlock();
                return RwReadGuard {
                    lock: self,
                    is_writer_reading: false,
                };
            }

            // A different thread is writing, wait and retry
            self.futex.unlock();
            std::thread::yield_now();
        }
    }

    /// Attempts to acquire a read lock without blocking.
    #[inline]
    pub fn try_read(&self) -> Option<RwReadGuard<'_, T>> {
        let current = current_thread_id();

        // If we already own the write lock, we can read
        if self.write_owner.load(Ordering::Acquire) == current {
            return Some(RwReadGuard {
                lock: self,
                is_writer_reading: true,
            });
        }

        if !self.futex.try_lock() {
            return None;
        }

        let writer = self.write_owner.load(Ordering::Acquire);
        if writer == RW_NO_OWNER {
            self.reader_count.fetch_add(1, Ordering::AcqRel);
            self.futex.unlock();
            Some(RwReadGuard {
                lock: self,
                is_writer_reading: false,
            })
        } else {
            self.futex.unlock();
            None
        }
    }

    /// Acquires a write lock, blocking until available.
    ///
    /// If the current thread already holds the write lock, this succeeds immediately
    /// (reentrant acquisition).
    ///
    /// Only one thread can hold the write lock at a time.
    #[inline]
    pub fn write(&self) -> RwWriteGuard<'_, T> {
        let current = current_thread_id();

        // Check for reentrant acquisition
        if self.write_owner.load(Ordering::Acquire) == current {
            // Already own the lock, just increment count
            self.write_count.fetch_add(1, Ordering::Relaxed);
            return RwWriteGuard { lock: self };
        }

        loop {
            self.futex.lock();

            let writer = self.write_owner.load(Ordering::Acquire);
            let readers = self.reader_count.load(Ordering::Acquire);

            if writer == RW_NO_OWNER && readers == 0 {
                // No writer and no readers - we can take the write lock
                self.write_owner.store(current, Ordering::Release);
                self.write_count.store(1, Ordering::Relaxed);
                self.futex.unlock();
                return RwWriteGuard { lock: self };
            }

            if writer == current {
                // We already own it (race condition check)
                self.write_count.fetch_add(1, Ordering::Relaxed);
                self.futex.unlock();
                return RwWriteGuard { lock: self };
            }

            // Someone else has the lock, wait and retry
            self.futex.unlock();
            std::thread::yield_now();
        }
    }

    /// Attempts to acquire a write lock without blocking.
    #[inline]
    pub fn try_write(&self) -> Option<RwWriteGuard<'_, T>> {
        let current = current_thread_id();

        // Check for reentrant acquisition
        if self.write_owner.load(Ordering::Acquire) == current {
            self.write_count.fetch_add(1, Ordering::Relaxed);
            return Some(RwWriteGuard { lock: self });
        }

        if !self.futex.try_lock() {
            return None;
        }

        let writer = self.write_owner.load(Ordering::Acquire);
        let readers = self.reader_count.load(Ordering::Acquire);

        if writer == RW_NO_OWNER && readers == 0 {
            self.write_owner.store(current, Ordering::Release);
            self.write_count.store(1, Ordering::Relaxed);
            self.futex.unlock();
            Some(RwWriteGuard { lock: self })
        } else {
            self.futex.unlock();
            None
        }
    }

    /// Executes a closure with read access to the data.
    #[inline]
    pub fn with_read<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&T) -> R,
    {
        let guard = self.read();
        f(&*guard)
    }

    /// Executes a closure with write access to the data.
    #[inline]
    pub fn with_write<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&mut T) -> R,
    {
        let mut guard = self.write();
        f(&mut *guard)
    }

    /// Gets a clone of the data (acquires read lock).
    #[inline]
    pub fn get(&self) -> T
    where
        T: Clone,
    {
        let guard = self.read();
        (*guard).clone()
    }

    /// Sets the data (acquires write lock).
    #[inline]
    pub fn set(&self, value: T) {
        let mut guard = self.write();
        *guard = value;
    }

    /// Consumes the lock and returns the inner value.
    #[inline]
    pub fn into_inner(self) -> T {
        self.data.into_inner()
    }

    /// Returns a raw pointer to the underlying data.
    #[inline]
    pub fn as_ptr(&self) -> *const T {
        self.data.get()
    }

    /// Returns a mutable raw pointer to the underlying data.
    #[inline]
    pub fn as_mut_ptr(&self) -> *mut T {
        self.data.get()
    }

    /// Check if the lock is currently held for writing.
    #[inline]
    pub fn is_write_locked(&self) -> bool {
        self.write_owner.load(Ordering::Relaxed) != RW_NO_OWNER
    }

    /// Check if the current thread holds the write lock.
    #[inline]
    pub fn is_write_locked_by_current_thread(&self) -> bool {
        self.write_owner.load(Ordering::Relaxed) == current_thread_id()
    }

    /// Get the number of active readers.
    #[inline]
    pub fn reader_count(&self) -> usize {
        self.reader_count.load(Ordering::Relaxed)
    }
}

/// RAII guard for read access to ReentrantRwLock.
pub struct RwReadGuard<'a, T> {
    lock: &'a ReentrantRwLock<T>,
    /// True if this read guard was created while the thread held the write lock
    is_writer_reading: bool,
}

impl<'a, T> Deref for RwReadGuard<'a, T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        unsafe { &*self.lock.data.get() }
    }
}

impl<'a, T> Drop for RwReadGuard<'a, T> {
    #[inline]
    fn drop(&mut self) {
        // Only decrement reader count if we actually incremented it
        if !self.is_writer_reading {
            self.lock.reader_count.fetch_sub(1, Ordering::AcqRel);
        }
    }
}

/// RAII guard for write access to ReentrantRwLock.
pub struct RwWriteGuard<'a, T> {
    lock: &'a ReentrantRwLock<T>,
}

impl<'a, T> Deref for RwWriteGuard<'a, T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        unsafe { &*self.lock.data.get() }
    }
}

impl<'a, T> DerefMut for RwWriteGuard<'a, T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.lock.data.get() }
    }
}

impl<'a, T> Drop for RwWriteGuard<'a, T> {
    #[inline]
    fn drop(&mut self) {
        let prev_count = self.lock.write_count.fetch_sub(1, Ordering::Relaxed);

        // Only release ownership if this was the last write lock
        if prev_count == 1 {
            self.lock.write_owner.store(RW_NO_OWNER, Ordering::Release);
        }
    }
}

impl<T: Default> Default for ReentrantRwLock<T> {
    #[inline]
    fn default() -> Self {
        Self::new(T::default())
    }
}

impl<T: Clone> Clone for ReentrantRwLock<T> {
    #[inline]
    fn clone(&self) -> Self {
        Self::new(self.get())
    }
}

impl<T: core::fmt::Debug> core::fmt::Debug for ReentrantRwLock<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self.try_read() {
            Some(guard) => f
                .debug_struct("ReentrantRwLock")
                .field("data", &*guard)
                .finish(),
            None => f
                .debug_struct("ReentrantRwLock")
                .field("data", &"<locked>")
                .finish(),
        }
    }
}

impl<T: PartialEq + Clone> PartialEq for ReentrantRwLock<T> {
    fn eq(&self, other: &Self) -> bool {
        if core::ptr::eq(self, other) {
            return true;
        }
        self.get() == other.get()
    }
}

impl<T: Eq + Clone> Eq for ReentrantRwLock<T> {}

impl<T: Hash + Clone> Hash for ReentrantRwLock<T> {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        self.get().hash(state);
    }
}

#[cfg(test)]
mod reentrant_rwlock_tests {
    use super::*;

    #[test]
    fn test_basic_read_write() {
        let lock = ReentrantRwLock::new(42);

        // Read access
        assert_eq!(*lock.read(), 42);

        // Write access
        *lock.write() = 100;
        assert_eq!(*lock.read(), 100);
    }

    #[test]
    fn test_reentrant_write() {
        let lock = ReentrantRwLock::new(0);

        // Nested write locks - this would deadlock with std::sync::RwLock
        lock.with_write(|outer| {
            *outer = 1;
            lock.with_write(|inner| {
                *inner = 2;
                lock.with_write(|deepest| {
                    *deepest = 3;
                });
            });
        });

        assert_eq!(*lock.read(), 3);
    }

    #[test]
    fn test_read_while_writing() {
        let lock = ReentrantRwLock::new(42);

        // Writer can read its own writes
        lock.with_write(|val| {
            *val = 100;
            // Reading while holding write lock
            let read_val = lock.with_read(|r| *r);
            assert_eq!(read_val, 100);
        });
    }

    #[test]
    fn test_multiple_readers() {
        let lock = ReentrantRwLock::new(42);

        let guard1 = lock.read();
        let guard2 = lock.read();
        let guard3 = lock.read();

        assert_eq!(*guard1, 42);
        assert_eq!(*guard2, 42);
        assert_eq!(*guard3, 42);
        assert_eq!(lock.reader_count(), 3);

        drop(guard1);
        assert_eq!(lock.reader_count(), 2);

        drop(guard2);
        drop(guard3);
        assert_eq!(lock.reader_count(), 0);
    }

    #[test]
    fn test_nested_closures() {
        let lock = ReentrantRwLock::new(Vec::<i32>::new());

        // Simulate nested closures that would deadlock with std::sync::RwLock
        lock.with_write(|vec| {
            vec.push(1);
            lock.with_write(|vec| {
                vec.push(2);
                lock.with_read(|vec| {
                    assert_eq!(vec.len(), 2);
                });
                vec.push(3);
            });
            vec.push(4);
        });

        assert_eq!(lock.get(), vec![1, 2, 3, 4]);
    }

    #[test]
    fn test_write_count_tracking() {
        let lock = ReentrantRwLock::new(0);

        {
            let _g1 = lock.write();
            assert!(lock.is_write_locked());
            assert!(lock.is_write_locked_by_current_thread());

            {
                let _g2 = lock.write();
                // Still locked
                assert!(lock.is_write_locked());
            }
            // Still locked (g1 still held)
            assert!(lock.is_write_locked());
        }
        // Now unlocked
        assert!(!lock.is_write_locked());
    }
}

// =============================================================================
// LazyRwLock - A lazy-initializing reentrant read-write lock
// =============================================================================

/// Initialization state constants
const LAZY_UNINIT: usize = 0;
const LAZY_INITIALIZING: usize = 1;
const LAZY_COMPLETE: usize = 2;
const LAZY_PANICKED: usize = 3;

/// A lazy-initializing reentrant read-write lock.
///
/// Combines the lazy initialization semantics of `MaybeLock`/`OnceLock` with
/// the reentrant read-write capabilities of `ReentrantRwLock`.
///
/// Features:
/// - Lazy initialization with `get_or_init`
/// - Option-like semantics (`is_some`, `is_none`, `try_get`)
/// - Reentrant write locks (same thread can acquire multiple times)
/// - Multiple concurrent readers OR one writer
/// - Thread-safe initialization (only one thread initializes)
pub struct LazyRwLock<T> {
    /// Initialization state
    init_state: AtomicUsize,
    /// The actual data storage (uses MaybeUninit semantics internally)
    data: UnsafeCell<Option<T>>,
    /// Thread ID of the current write lock owner (0 = no owner)
    write_owner: AtomicU64,
    /// Reentrant write lock count for the owning thread
    write_count: AtomicUsize,
    /// Number of active readers
    reader_count: AtomicUsize,
    /// Futex for synchronization
    futex: Futex,
}

// Safety: LazyRwLock provides its own synchronization through the internal
// Futex and atomic operations. Access to the data is always mediated through
// the lock, so we can safely implement Sync even for non-Sync types.
// This is the same reasoning std::sync::Mutex uses.
unsafe impl<T> Send for LazyRwLock<T> {}
unsafe impl<T> Sync for LazyRwLock<T> {}

impl<T> LazyRwLock<T> {
    /// Creates a new uninitialized `LazyRwLock`.
    #[inline]
    pub const fn new() -> Self {
        LazyRwLock {
            init_state: AtomicUsize::new(LAZY_UNINIT),
            data: UnsafeCell::new(None),
            write_owner: AtomicU64::new(RW_NO_OWNER),
            write_count: AtomicUsize::new(0),
            reader_count: AtomicUsize::new(0),
            futex: Futex::new(),
        }
    }

    /// Creates a new `LazyRwLock` pre-initialized with the given value.
    #[inline]
    pub fn initialized(value: T) -> Self {
        LazyRwLock {
            init_state: AtomicUsize::new(LAZY_COMPLETE),
            data: UnsafeCell::new(Some(value)),
            write_owner: AtomicU64::new(RW_NO_OWNER),
            write_count: AtomicUsize::new(0),
            reader_count: AtomicUsize::new(0),
            futex: Futex::new(),
        }
    }

    /// Alias for `initialized` - creates a pre-initialized LazyRwLock.
    /// Provided for MaybeLock API compatibility.
    #[inline]
    pub fn some(value: T) -> Self {
        Self::initialized(value)
    }

    /// Takes the value out of the lock, leaving it uninitialized.
    /// Returns `Maybe::Some(value)` if initialized, `Maybe::None` otherwise.
    pub fn take(&mut self) -> Option<T> {
        if self.init_state.load(Ordering::Acquire) == LAZY_COMPLETE {
            self.init_state.store(LAZY_UNINIT, Ordering::Release);
            self.data.get_mut().take()
        } else {
            None
        }
    }

    /// Returns `true` if the lock is initialized and contains a value.
    #[inline]
    pub fn is_some(&self) -> bool {
        self.init_state.load(Ordering::Acquire) == LAZY_COMPLETE
    }

    /// Returns `true` if the lock is not yet initialized.
    #[inline]
    pub fn is_none(&self) -> bool {
        self.init_state.load(Ordering::Acquire) != LAZY_COMPLETE
    }

    /// Returns `true` if the lock has been initialized.
    #[inline]
    pub fn is_initialized(&self) -> bool {
        self.init_state.load(Ordering::Acquire) == LAZY_COMPLETE
    }

    /// Gets a reference to the value, returning `None` if not initialized.
    #[inline]
    pub fn try_get(&self) -> Option<&T> {
        if self.init_state.load(Ordering::Acquire) == LAZY_COMPLETE {
            Some(unsafe { &*self.data.get().cast::<&T>() })
        } else {
            None
        }
    }

    /// Gets a mutable reference to the value, returning `None` if not initialized.
    #[inline]
    pub fn try_get_mut(&mut self) -> Option<&mut T> {
        if self.init_state.load(Ordering::Acquire) == LAZY_COMPLETE {
            unsafe { self.data.get_mut().as_mut() }
        } else {
            None
        }
    }

    /// Gets a reference to the value, panicking if not initialized.
    #[inline]
    pub fn get(&self) -> &T {
        if self.init_state.load(Ordering::Acquire) == LAZY_COMPLETE {
            unsafe {
                match &*self.data.get() {
                    Some(val) => val,
                    None => panic!("LazyRwLock data is None despite being initialized"),
                }
            }
        } else {
            panic!("LazyRwLock is not initialized")
        }
    }

    /// Gets a mutable reference to the value, panicking if not initialized.
    #[inline]
    pub fn get_mut(&mut self) -> &mut T {
        if self.init_state.load(Ordering::Acquire) == LAZY_COMPLETE {
            match self.data.get_mut() {
                Some(val) => val,
                None => panic!("LazyRwLock data is None despite being initialized"),
            }
        } else {
            panic!("LazyRwLock is not initialized")
        }
    }

    /// Forces initialization state to complete and returns a mutable reference.
    /// Used for manual initialization patterns. Initializes with default if not set.
    /// # Safety
    /// Caller must ensure the data is properly initialized before calling.
    #[inline]
    pub unsafe fn force_mut(&self) -> &mut T {
        // Force initialization state
        self.init_state.store(LAZY_COMPLETE, Ordering::Release);
        match &mut *self.data.get() {
            Some(val) => val,
            None => panic!("LazyRwLock data is None - must set value first"),
        }
    }

    /// Gets a mutable reference, initializing with the closure if needed.
    pub fn get_or_init_mut<F>(&self, f: F) -> &mut T
    where
        F: FnOnce() -> T,
    {
        // Fast path: already initialized
        if self.init_state.load(Ordering::Acquire) == LAZY_COMPLETE {
            return unsafe {
                match &mut *self.data.get() {
                    Some(val) => val,
                    None => unreachable!(),
                }
            };
        }

        // Slow path: need to initialize
        self.initialize(f);

        unsafe {
            match &mut *self.data.get() {
                Some(val) => val,
                None => unreachable!(),
            }
        }
    }

    /// Gets the value, initializing it with the provided closure if necessary.
    pub fn get_or_init<F>(&self, f: F) -> &T
    where
        F: FnOnce() -> T,
    {
        // Fast path: already initialized
        if self.init_state.load(Ordering::Acquire) == LAZY_COMPLETE {
            return unsafe {
                match &*self.data.get() {
                    Some(val) => val,
                    None => unreachable!(),
                }
            };
        }

        // Slow path: need to initialize
        self.initialize(f);

        unsafe {
            match &*self.data.get() {
                Some(val) => val,
                None => unreachable!(),
            }
        }
    }

    fn initialize<F>(&self, f: F)
    where
        F: FnOnce() -> T,
    {
        use std::panic;

        match self.init_state.compare_exchange(
            LAZY_UNINIT,
            LAZY_INITIALIZING,
            Ordering::AcqRel,
            Ordering::Acquire,
        ) {
            Ok(_) => {
                // We won the race, initialize
                let result = panic::catch_unwind(panic::AssertUnwindSafe(|| f()));
                match result {
                    Ok(value) => {
                        unsafe {
                            *self.data.get() = Some(value);
                        }
                        self.init_state.store(LAZY_COMPLETE, Ordering::Release);
                    }
                    Err(payload) => {
                        self.init_state.store(LAZY_PANICKED, Ordering::Release);
                        panic::resume_unwind(payload);
                    }
                }
            }
            Err(LAZY_COMPLETE) => {
                // Already initialized by another thread
            }
            Err(LAZY_PANICKED) => {
                panic!("LazyRwLock initialization previously panicked");
            }
            Err(LAZY_INITIALIZING) => {
                // Another thread is initializing, spin-wait
                while self.init_state.load(Ordering::Acquire) == LAZY_INITIALIZING {
                    std::thread::yield_now();
                }
                if self.init_state.load(Ordering::Acquire) == LAZY_PANICKED {
                    panic!("LazyRwLock initialization previously panicked");
                }
            }
            Err(_) => unreachable!(),
        }
    }

    /// Acquires a read lock, blocking until available.
    #[inline]
    pub fn read(&self) -> LazyRwReadGuard<'_, T> {
        let current = current_thread_id();

        // If we already own the write lock, we can read without additional locking
        if self.write_owner.load(Ordering::Acquire) == current {
            return LazyRwReadGuard {
                lock: self,
                is_writer_reading: true,
            };
        }

        loop {
            self.futex.lock();

            let writer = self.write_owner.load(Ordering::Acquire);
            if writer == RW_NO_OWNER || writer == current {
                self.reader_count.fetch_add(1, Ordering::AcqRel);
                self.futex.unlock();
                return LazyRwReadGuard {
                    lock: self,
                    is_writer_reading: false,
                };
            }

            self.futex.unlock();
            std::thread::yield_now();
        }
    }

    /// Acquires a write lock, blocking until available.
    #[inline]
    pub fn write(&self) -> LazyRwWriteGuard<'_, T> {
        let current = current_thread_id();

        // Check for reentrant acquisition
        if self.write_owner.load(Ordering::Acquire) == current {
            self.write_count.fetch_add(1, Ordering::Relaxed);
            return LazyRwWriteGuard { lock: self };
        }

        loop {
            self.futex.lock();

            let writer = self.write_owner.load(Ordering::Acquire);
            let readers = self.reader_count.load(Ordering::Acquire);

            if writer == RW_NO_OWNER && readers == 0 {
                self.write_owner.store(current, Ordering::Release);
                self.write_count.store(1, Ordering::Relaxed);
                self.futex.unlock();
                return LazyRwWriteGuard { lock: self };
            }

            if writer == current {
                self.write_count.fetch_add(1, Ordering::Relaxed);
                self.futex.unlock();
                return LazyRwWriteGuard { lock: self };
            }

            self.futex.unlock();
            std::thread::yield_now();
        }
    }

    /// Executes a closure with read access to the data.
    #[inline]
    pub fn with_read<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&T) -> R,
    {
        let guard = self.read();
        f(&*guard)
    }

    /// Executes a closure with write access to the data.
    #[inline]
    pub fn with_write<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&mut T) -> R,
    {
        let mut guard = self.write();
        f(&mut *guard)
    }

    /// Sets the value (initializes if needed).
    #[inline]
    pub fn set(&self, value: T) {
        if self.init_state.load(Ordering::Acquire) != LAZY_COMPLETE {
            // Initialize with the value
            self.get_or_init(|| value);
        } else {
            // Already initialized, update via write lock
            let mut guard = self.write();
            *guard = value;
        }
    }

    /// Check if the lock is currently held for writing.
    #[inline]
    pub fn is_write_locked(&self) -> bool {
        self.write_owner.load(Ordering::Relaxed) != RW_NO_OWNER
    }

    /// Check if the current thread holds the write lock.
    #[inline]
    pub fn is_write_locked_by_current_thread(&self) -> bool {
        self.write_owner.load(Ordering::Relaxed) == current_thread_id()
    }
}

/// RAII guard for read access to LazyRwLock.
pub struct LazyRwReadGuard<'a, T> {
    lock: &'a LazyRwLock<T>,
    is_writer_reading: bool,
}

impl<'a, T> Deref for LazyRwReadGuard<'a, T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        unsafe {
            match &*self.lock.data.get() {
                Some(val) => val,
                None => panic!("LazyRwLock not initialized"),
            }
        }
    }
}

impl<'a, T> Drop for LazyRwReadGuard<'a, T> {
    #[inline]
    fn drop(&mut self) {
        if !self.is_writer_reading {
            self.lock.reader_count.fetch_sub(1, Ordering::AcqRel);
        }
    }
}

/// RAII guard for write access to LazyRwLock.
pub struct LazyRwWriteGuard<'a, T> {
    lock: &'a LazyRwLock<T>,
}

impl<'a, T> Deref for LazyRwWriteGuard<'a, T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        unsafe {
            match &*self.lock.data.get() {
                Some(val) => val,
                None => panic!("LazyRwLock not initialized"),
            }
        }
    }
}

impl<'a, T> DerefMut for LazyRwWriteGuard<'a, T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe {
            match &mut *self.lock.data.get() {
                Some(val) => val,
                None => panic!("LazyRwLock not initialized"),
            }
        }
    }
}

impl<'a, T> Drop for LazyRwWriteGuard<'a, T> {
    #[inline]
    fn drop(&mut self) {
        let prev_count = self.lock.write_count.fetch_sub(1, Ordering::Relaxed);
        if prev_count == 1 {
            self.lock.write_owner.store(RW_NO_OWNER, Ordering::Release);
        }
    }
}

impl<T: Default> Default for LazyRwLock<T> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Clone> Clone for LazyRwLock<T> {
    #[inline]
    fn clone(&self) -> Self {
        if self.is_some() {
            Self::initialized(self.get().clone())
        } else {
            Self::new()
        }
    }
}

impl<T: core::fmt::Debug> core::fmt::Debug for LazyRwLock<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self.try_get() {
            Some(val) => f
                .debug_struct("LazyRwLock")
                .field("initialized", &true)
                .field("data", val)
                .finish(),
            None => f
                .debug_struct("LazyRwLock")
                .field("initialized", &false)
                .finish(),
        }
    }
}

#[cfg(test)]
mod lazy_rwlock_tests {
    use super::*;

    #[test]
    fn test_lazy_initialization() {
        let lock: LazyRwLock<i32> = LazyRwLock::new();
        assert!(lock.is_none());
        assert!(!lock.is_some());

        let val = lock.get_or_init(|| 42);
        assert_eq!(*val, 42);
        assert!(lock.is_some());
    }

    #[test]
    fn test_reentrant_write_lazy() {
        let lock = LazyRwLock::initialized(0);

        lock.with_write(|outer| {
            *outer = 1;
            lock.with_write(|inner| {
                *inner = 2;
            });
        });

        assert_eq!(*lock.get(), 2);
    }

    #[test]
    fn test_read_while_writing_lazy() {
        let lock = LazyRwLock::initialized(42);

        lock.with_write(|val| {
            *val = 100;
            let read_val = lock.with_read(|r| *r);
            assert_eq!(read_val, 100);
        });
    }

    #[test]
    fn test_set_initializes() {
        let lock: LazyRwLock<i32> = LazyRwLock::new();
        assert!(lock.is_none());

        lock.set(42);
        assert!(lock.is_some());
        assert_eq!(*lock.get(), 42);
    }
}
