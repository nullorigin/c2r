use std::{
    alloc::Layout,
    iter::{FromIterator, FusedIterator},
    ops::{Add, AddAssign, Bound, Index, IndexMut, RangeBounds, Sub, SubAssign},
    ptr::null_mut,
    sync::atomic::{AtomicBool, AtomicPtr, AtomicUsize, Ordering},
};

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

#[inline]
fn alloc_array<T>(capacity: usize) -> *mut T {
    if capacity == 0 {
        return null_mut();
    }
    let layout = Layout::array::<T>(capacity).unwrap();
    let ptr = unsafe { std::alloc::alloc(layout) } as *mut T;
    if ptr.is_null() {
        std::alloc::handle_alloc_error(layout);
    }
    ptr
}

#[inline]
fn dealloc_array<T>(ptr: *mut T, capacity: usize) {
    if !ptr.is_null() && capacity > 0 {
        let layout = Layout::array::<T>(capacity).unwrap();
        unsafe { std::alloc::dealloc(ptr as *mut u8, layout) };
    }
}

#[inline]
fn realloc_copy<T>(old_ptr: *mut T, old_cap: usize, new_cap: usize, len: usize) -> *mut T {
    let new_ptr = alloc_array::<T>(new_cap);
    if !old_ptr.is_null() && len > 0 {
        unsafe { std::ptr::copy_nonoverlapping(old_ptr, new_ptr, len) };
    }
    dealloc_array(old_ptr, old_cap);
    new_ptr
}

// ============================================================================
// MAP - Lock-free atomic map with spinlock for mutations
// ============================================================================

pub struct AtomicMap<K, V> {
    lock: AtomicBool,
    inner: AtomicPtr<(K, V)>,
    len: AtomicUsize,
    capacity: AtomicUsize,
    index: AtomicUsize,
}

impl<K, V> AtomicMap<K, V> {
    #[inline]
    pub fn new() -> Self {
        AtomicMap {
            lock: AtomicBool::new(false),
            inner: AtomicPtr::new(null_mut()),
            len: AtomicUsize::new(0),
            capacity: AtomicUsize::new(0),
            index: AtomicUsize::new(0),
        }
    }

    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        AtomicMap {
            lock: AtomicBool::new(false),
            inner: AtomicPtr::new(alloc_array(capacity)),
            len: AtomicUsize::new(0),
            capacity: AtomicUsize::new(capacity),
            index: AtomicUsize::new(0),
        }
    }

    #[inline]
    fn acquire(&self) {
        while self
            .lock
            .compare_exchange_weak(false, true, Ordering::Acquire, Ordering::Relaxed)
            .is_err()
        {
            std::hint::spin_loop();
        }
    }

    #[inline]
    fn release(&self) {
        self.lock.store(false, Ordering::Release);
    }

    #[inline]
    fn locked<R, F: FnOnce() -> R>(&self, f: F) -> R {
        self.acquire();
        let result = f();
        self.release();
        result
    }

    #[inline]
    fn ptr(&self) -> *mut (K, V) {
        self.inner.load(Ordering::Acquire)
    }

    #[inline]
    fn len_inner(&self) -> usize {
        self.len.load(Ordering::Acquire)
    }

    #[inline]
    fn cap_inner(&self) -> usize {
        self.capacity.load(Ordering::Acquire)
    }

    pub fn clear(&self) {
        self.locked(|| {
            let len = self.len_inner();
            if len > 0 {
                let ptr = self.ptr();
                unsafe {
                    for i in 0..len {
                        std::ptr::drop_in_place(ptr.add(i));
                    }
                }
                self.len.store(0, Ordering::Release);
            }
        })
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.len.load(Ordering::Relaxed)
    }

    #[inline]
    pub fn capacity(&self) -> usize {
        self.capacity.load(Ordering::Relaxed)
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn contains_key(&self, key: &K) -> bool
    where
        K: PartialEq,
    {
        self.locked(|| self.find_index(key).is_some())
    }

    pub fn get(&self, key: &K) -> Option<V>
    where
        K: PartialEq,
        V: Clone,
    {
        self.locked(|| {
            self.find_index(key)
                .map(|i| unsafe { (*self.ptr().add(i)).1.clone() })
        })
    }

    pub fn get_ref(&self, key: &K) -> Option<&V>
    where
        K: PartialEq,
    {
        self.locked(|| {
            self.find_index(key)
                .map(|i| unsafe { &(*self.ptr().add(i)).1 })
        })
    }

    pub fn get_mut(&mut self, key: &K) -> Option<&mut V>
    where
        K: PartialEq,
    {
        self.locked(|| {
            self.find_index(key)
                .map(|i| unsafe { &mut (*self.ptr().add(i)).1 })
        })
    }

    pub fn get_key_value(&self, key: &K) -> Option<(K, V)>
    where
        K: PartialEq + Clone,
        V: Clone,
    {
        self.locked(|| {
            self.find_index(key).map(|i| unsafe {
                let entry = &*self.ptr().add(i);
                (entry.0.clone(), entry.1.clone())
            })
        })
    }

    pub fn insert(&self, key: K, value: V) -> Option<V>
    where
        K: PartialEq,
    {
        self.acquire();

        if let Some(i) = self.find_index(&key) {
            let old = unsafe { std::ptr::replace(&mut (*self.ptr().add(i)).1, value) };
            self.release();
            return Some(old);
        }

        self.ensure_capacity(1);
        let len = self.len_inner();
        unsafe { std::ptr::write(self.ptr().add(len), (key, value)) };
        self.len.store(len + 1, Ordering::Release);
        self.release();
        None
    }

    pub fn get_or_insert(&self, key: K, default: V) -> V
    where
        K: PartialEq + Clone,
        V: Clone,
    {
        self.acquire();

        if let Some(i) = self.find_index(&key) {
            let val = unsafe { (*self.ptr().add(i)).1.clone() };
            self.release();
            return val;
        }

        self.ensure_capacity(1);
        let len = self.len_inner();
        let result = default.clone();
        unsafe { std::ptr::write(self.ptr().add(len), (key, default)) };
        self.len.store(len + 1, Ordering::Release);
        self.release();
        result
    }

    pub fn get_or_insert_with<F: FnOnce() -> V>(&self, key: K, f: F) -> V
    where
        K: PartialEq + Clone,
        V: Clone,
    {
        self.acquire();

        if let Some(i) = self.find_index(&key) {
            let val = unsafe { (*self.ptr().add(i)).1.clone() };
            self.release();
            return val;
        }

        let value = f();
        let result = value.clone();
        self.ensure_capacity(1);
        let len = self.len_inner();
        unsafe { std::ptr::write(self.ptr().add(len), (key, value)) };
        self.len.store(len + 1, Ordering::Release);
        self.release();
        result
    }

    pub fn remove(&self, key: &K) -> Option<V>
    where
        K: PartialEq,
    {
        self.locked(|| {
            if let Some(i) = self.find_index(key) {
                let len = self.len_inner();
                let ptr = self.ptr();
                unsafe {
                    let old = std::ptr::read(&(*ptr.add(i)).1);
                    std::ptr::copy(ptr.add(i + 1), ptr.add(i), len - i - 1);
                    self.len.store(len - 1, Ordering::Release);
                    Some(old)
                }
            } else {
                None
            }
        })
    }

    pub fn remove_entry(&self, key: &K) -> Option<(K, V)>
    where
        K: PartialEq,
    {
        self.locked(|| {
            if let Some(i) = self.find_index(key) {
                let len = self.len_inner();
                let ptr = self.ptr();
                unsafe {
                    let old = std::ptr::read(ptr.add(i));
                    std::ptr::copy(ptr.add(i + 1), ptr.add(i), len - i - 1);
                    self.len.store(len - 1, Ordering::Release);
                    Some(old)
                }
            } else {
                None
            }
        })
    }

    pub fn retain<F>(&self, mut f: F)
    where
        F: FnMut(&K, &mut V) -> bool,
    {
        self.locked(|| {
            let len = self.len_inner();
            let ptr = self.ptr();
            let mut write = 0;

            unsafe {
                for read in 0..len {
                    let entry = ptr.add(read);
                    if f(&(*entry).0, &mut (*entry).1) {
                        if write != read {
                            std::ptr::copy_nonoverlapping(entry, ptr.add(write), 1);
                        }
                        write += 1;
                    } else {
                        std::ptr::drop_in_place(entry);
                    }
                }
            }
            self.len.store(write, Ordering::Release);
        })
    }

    pub fn keys(&self) -> Vec<K>
    where
        K: Clone,
    {
        self.locked(|| {
            let len = self.len_inner();
            let ptr = self.ptr();
            (0..len)
                .map(|i| unsafe { (*ptr.add(i)).0.clone() })
                .collect()
        })
    }

    pub fn values(&self) -> Vec<V>
    where
        V: Clone,
    {
        self.locked(|| {
            let len = self.len_inner();
            let ptr = self.ptr();
            (0..len)
                .map(|i| unsafe { (*ptr.add(i)).1.clone() })
                .collect()
        })
    }

    pub fn first(&self) -> Option<(K, V)>
    where
        K: Clone,
        V: Clone,
    {
        self.locked(|| {
            if self.len_inner() > 0 {
                unsafe { Some((*self.ptr()).clone()) }
            } else {
                None
            }
        })
    }

    pub fn last(&self) -> Option<(K, V)>
    where
        K: Clone,
        V: Clone,
    {
        self.locked(|| {
            let len = self.len_inner();
            if len > 0 {
                unsafe { Some((*self.ptr().add(len - 1)).clone()) }
            } else {
                None
            }
        })
    }

    pub fn is_sorted(&self) -> bool
    where
        K: Ord,
    {
        self.locked(|| {
            let len = self.len_inner();
            if len <= 1 {
                return true;
            }
            let ptr = self.ptr();
            unsafe { (0..len - 1).all(|i| (*ptr.add(i)).0 <= (*ptr.add(i + 1)).0) }
        })
    }

    pub fn has_duplicates(&self) -> bool
    where
        K: PartialEq,
    {
        self.locked(|| {
            let len = self.len_inner();
            if len <= 1 {
                return false;
            }
            let ptr = self.ptr();
            unsafe {
                for i in 0..len - 1 {
                    for j in i + 1..len {
                        if (*ptr.add(i)).0 == (*ptr.add(j)).0 {
                            return true;
                        }
                    }
                }
            }
            false
        })
    }

    pub fn dedup(&self)
    where
        K: PartialEq,
    {
        self.locked(|| {
            let len = self.len_inner();
            if len <= 1 {
                return;
            }

            let ptr = self.ptr();
            let mut write = 1;

            unsafe {
                for read in 1..len {
                    let mut is_dup = false;
                    for w in 0..write {
                        if (*ptr.add(read)).0 == (*ptr.add(w)).0 {
                            is_dup = true;
                            break;
                        }
                    }
                    if !is_dup {
                        if write != read {
                            std::ptr::copy_nonoverlapping(ptr.add(read), ptr.add(write), 1);
                        }
                        write += 1;
                    } else {
                        std::ptr::drop_in_place(ptr.add(read));
                    }
                }
            }
            self.len.store(write, Ordering::Release);
        })
    }

    pub fn sort(&self)
    where
        K: Ord,
    {
        self.locked(|| {
            let len = self.len_inner();
            if len <= 1 {
                return;
            }

            let ptr = self.ptr();
            unsafe {
                let slice = std::slice::from_raw_parts_mut(ptr, len);
                slice.sort_unstable_by(|a, b| a.0.cmp(&b.0));
            }
        })
    }

    pub fn sort_and_dedup(&self)
    where
        K: Ord,
    {
        self.acquire();
        let len = self.len_inner();
        if len <= 1 {
            self.release();
            return;
        }

        let ptr = self.ptr();
        unsafe {
            let slice = std::slice::from_raw_parts_mut(ptr, len);
            slice.sort_unstable_by(|a, b| a.0.cmp(&b.0));

            let mut write = 1;
            for read in 1..len {
                if (*ptr.add(read)).0 != (*ptr.add(write - 1)).0 {
                    if write != read {
                        std::ptr::copy_nonoverlapping(ptr.add(read), ptr.add(write), 1);
                    }
                    write += 1;
                } else {
                    std::ptr::drop_in_place(ptr.add(read));
                }
            }
            self.len.store(write, Ordering::Release);
        }
        self.release();
    }

    pub fn entries(&self) -> Vec<(K, V)>
    where
        K: Clone,
        V: Clone,
    {
        self.locked(|| {
            let len = self.len_inner();
            let ptr = self.ptr();
            (0..len).map(|i| unsafe { (*ptr.add(i)).clone() }).collect()
        })
    }

    pub fn iter(&self) -> MapIter<'_, K, V> {
        MapIter {
            map: self,
            index: 0,
        }
    }

    fn ensure_capacity(&self, additional: usize) {
        let len = self.len_inner();
        let cap = self.cap_inner();
        let required = len + additional;

        if required <= cap {
            return;
        }

        let new_cap = (cap * 2).max(required).max(4);
        let new_ptr = realloc_copy(self.ptr(), cap, new_cap, len);
        self.inner.store(new_ptr, Ordering::Release);
        self.capacity.store(new_cap, Ordering::Release);
    }

    pub fn reserve(&self, additional: usize) {
        self.locked(|| self.ensure_capacity(additional))
    }

    pub fn shrink_to_fit(&self) {
        self.locked(|| {
            let len = self.len_inner();
            let cap = self.cap_inner();
            if len < cap {
                self.shrink_to_inner(len);
            }
        })
    }

    fn shrink_to_inner(&self, new_cap: usize) {
        let old_ptr = self.ptr();
        let len = self.len_inner();
        let old_cap = self.cap_inner();

        if new_cap >= old_cap {
            return;
        }

        let new_cap = new_cap.max(len);
        if new_cap == 0 {
            self.inner.store(null_mut(), Ordering::Release);
            self.capacity.store(0, Ordering::Release);
            dealloc_array(old_ptr, old_cap);
            return;
        }

        let new_ptr = realloc_copy(old_ptr, old_cap, new_cap, len);
        self.inner.store(new_ptr, Ordering::Release);
        self.capacity.store(new_cap, Ordering::Release);
    }

    pub fn shrink_to(&self, new_capacity: usize) {
        self.locked(|| self.shrink_to_inner(new_capacity))
    }

    #[inline]
    fn find_index(&self, key: &K) -> Option<usize>
    where
        K: PartialEq,
    {
        let len = self.len_inner();
        let ptr = self.ptr();
        if ptr.is_null() || len == 0 {
            return None;
        }

        unsafe { (0..len).find(|&i| (*ptr.add(i)).0 == *key) }
    }

    fn find_entry(&self, key: &K) -> Option<(*mut K, *mut V)>
    where
        K: PartialEq,
    {
        self.find_index(key).map(|i| {
            let ptr = self.ptr();
            unsafe {
                (
                    &mut (*ptr.add(i)).0 as *mut K,
                    &mut (*ptr.add(i)).1 as *mut V,
                )
            }
        })
    }

    /// Binary search for key in sorted map - O(log n)
    #[inline]
    fn binary_search_key(&self, key: &K) -> Result<usize, usize>
    where
        K: Ord,
    {
        let len = self.len_inner();
        let ptr = self.ptr();
        if ptr.is_null() || len == 0 {
            return Err(0);
        }

        let slice = unsafe { std::slice::from_raw_parts(ptr, len) };
        slice.binary_search_by(|entry| entry.0.cmp(key))
    }

    /// Get value using binary search (requires sorted keys) - O(log n)
    pub fn get_sorted(&self, key: &K) -> Option<V>
    where
        K: Ord,
        V: Clone,
    {
        self.locked(|| match self.binary_search_key(key) {
            Ok(i) => unsafe { Some((*self.ptr().add(i)).1.clone()) },
            Err(_) => None,
        })
    }

    /// Insert maintaining sorted order - O(n) due to shifting
    pub fn insert_sorted(&self, key: K, value: V) -> Option<V>
    where
        K: Ord,
    {
        self.acquire();

        match self.binary_search_key(&key) {
            Ok(i) => {
                let old = unsafe { std::ptr::replace(&mut (*self.ptr().add(i)).1, value) };
                self.release();
                Some(old)
            }
            Err(i) => {
                self.ensure_capacity(1);
                let len = self.len_inner();
                let ptr = self.ptr();
                unsafe {
                    if i < len {
                        std::ptr::copy(ptr.add(i), ptr.add(i + 1), len - i);
                    }
                    std::ptr::write(ptr.add(i), (key, value));
                }
                self.len.store(len + 1, Ordering::Release);
                self.release();
                None
            }
        }
    }

    /// Extend map with entries from an iterator
    pub fn extend<I: IntoIterator<Item = (K, V)>>(&self, iter: I)
    where
        K: PartialEq,
    {
        for (k, v) in iter {
            self.insert(k, v);
        }
    }

    /// Get value or insert default, returning mutable reference
    pub fn entry_or_insert(&self, key: K, default: V) -> V
    where
        K: PartialEq + Clone,
        V: Clone,
    {
        self.get_or_insert(key, default)
    }

    /// Update value in place with a function
    pub fn update<F>(&self, key: &K, f: F) -> bool
    where
        K: PartialEq,
        F: FnOnce(&mut V),
    {
        self.locked(|| {
            if let Some(i) = self.find_index(key) {
                unsafe { f(&mut (*self.ptr().add(i)).1) };
                true
            } else {
                false
            }
        })
    }

    /// Update or insert with functions
    pub fn upsert<FU, FI>(&self, key: K, update_fn: FU, insert_fn: FI)
    where
        K: PartialEq,
        FU: FnOnce(&mut V),
        FI: FnOnce() -> V,
    {
        self.acquire();

        if let Some(i) = self.find_index(&key) {
            unsafe { update_fn(&mut (*self.ptr().add(i)).1) };
            self.release();
        } else {
            let value = insert_fn();
            self.ensure_capacity(1);
            let len = self.len_inner();
            unsafe { std::ptr::write(self.ptr().add(len), (key, value)) };
            self.len.store(len + 1, Ordering::Release);
            self.release();
        }
    }

    /// Pop and return first entry
    pub fn pop_first(&self) -> Option<(K, V)>
    where
        K: Clone,
        V: Clone,
    {
        self.locked(|| {
            let len = self.len_inner();
            if len == 0 {
                return None;
            }
            let ptr = self.ptr();
            unsafe {
                let entry = std::ptr::read(ptr);
                std::ptr::copy(ptr.add(1), ptr, len - 1);
                self.len.store(len - 1, Ordering::Release);
                Some(entry)
            }
        })
    }

    /// Pop and return last entry
    pub fn pop_last(&self) -> Option<(K, V)>
    where
        K: Clone,
        V: Clone,
    {
        self.locked(|| {
            let len = self.len_inner();
            if len == 0 {
                return None;
            }
            let new_len = len - 1;
            self.len.store(new_len, Ordering::Release);
            unsafe { Some(std::ptr::read(self.ptr().add(new_len))) }
        })
    }

    /// Get entry at index
    pub fn get_index(&self, index: usize) -> Option<(K, V)>
    where
        K: Clone,
        V: Clone,
    {
        self.locked(|| {
            let len = self.len_inner();
            if index >= len {
                return None;
            }
            unsafe { Some((*self.ptr().add(index)).clone()) }
        })
    }

    /// Remove entry at index
    pub fn remove_index(&self, index: usize) -> Option<(K, V)> {
        self.locked(|| {
            let len = self.len_inner();
            if index >= len {
                return None;
            }
            let ptr = self.ptr();
            unsafe {
                let entry = std::ptr::read(ptr.add(index));
                std::ptr::copy(ptr.add(index + 1), ptr.add(index), len - index - 1);
                self.len.store(len - 1, Ordering::Release);
                Some(entry)
            }
        })
    }

    /// Swap remove - O(1) removal by swapping with last element
    pub fn swap_remove(&self, key: &K) -> Option<V>
    where
        K: PartialEq,
    {
        self.locked(|| {
            if let Some(i) = self.find_index(key) {
                let len = self.len_inner();
                let ptr = self.ptr();
                unsafe {
                    let old_val = std::ptr::read(&(*ptr.add(i)).1);
                    if i < len - 1 {
                        std::ptr::copy_nonoverlapping(ptr.add(len - 1), ptr.add(i), 1);
                    }
                    self.len.store(len - 1, Ordering::Release);
                    Some(old_val)
                }
            } else {
                None
            }
        })
    }

    /// Filter map in place, keeping only entries where predicate returns true
    pub fn filter<F>(&self, mut predicate: F)
    where
        F: FnMut(&K, &V) -> bool,
    {
        self.retain(|k, v| predicate(k, v));
    }

    /// Map values in place
    pub fn map_values<F>(&self, mut f: F)
    where
        F: FnMut(&mut V),
    {
        self.locked(|| {
            let len = self.len_inner();
            let ptr = self.ptr();
            unsafe {
                for i in 0..len {
                    f(&mut (*ptr.add(i)).1);
                }
            }
        })
    }

    /// Find first entry matching predicate
    pub fn find<F>(&self, mut predicate: F) -> Option<(K, V)>
    where
        K: Clone,
        V: Clone,
        F: FnMut(&K, &V) -> bool,
    {
        self.locked(|| {
            let len = self.len_inner();
            let ptr = self.ptr();
            unsafe {
                for i in 0..len {
                    let entry = &*ptr.add(i);
                    if predicate(&entry.0, &entry.1) {
                        return Some(entry.clone());
                    }
                }
            }
            None
        })
    }

    /// Count entries matching predicate
    pub fn count<F>(&self, mut predicate: F) -> usize
    where
        F: FnMut(&K, &V) -> bool,
    {
        self.locked(|| {
            let len = self.len_inner();
            let ptr = self.ptr();
            let mut count = 0;
            unsafe {
                for i in 0..len {
                    let entry = &*ptr.add(i);
                    if predicate(&entry.0, &entry.1) {
                        count += 1;
                    }
                }
            }
            count
        })
    }

    /// Check if any entry matches predicate
    pub fn any<F>(&self, mut predicate: F) -> bool
    where
        F: FnMut(&K, &V) -> bool,
    {
        self.locked(|| {
            let len = self.len_inner();
            let ptr = self.ptr();
            unsafe {
                for i in 0..len {
                    let entry = &*ptr.add(i);
                    if predicate(&entry.0, &entry.1) {
                        return true;
                    }
                }
            }
            false
        })
    }

    /// Check if all entries match predicate
    pub fn all<F>(&self, mut predicate: F) -> bool
    where
        F: FnMut(&K, &V) -> bool,
    {
        self.locked(|| {
            let len = self.len_inner();
            let ptr = self.ptr();
            unsafe {
                for i in 0..len {
                    let entry = &*ptr.add(i);
                    if !predicate(&entry.0, &entry.1) {
                        return false;
                    }
                }
            }
            true
        })
    }

    /// Drain all entries, returning them as a Vec
    pub fn drain(&self) -> Vec<(K, V)> {
        self.acquire();
        let len = self.len_inner();
        if len == 0 {
            self.release();
            return Vec::new();
        }

        let ptr = self.ptr();
        let mut result = Vec::with_capacity(len);
        unsafe {
            for i in 0..len {
                result.push(std::ptr::read(ptr.add(i)));
            }
        }
        self.len.store(0, Ordering::Release);
        self.release();
        result
    }

    /// Merge another map into this one
    pub fn merge(&self, other: &Self)
    where
        K: PartialEq + Clone,
        V: Clone,
    {
        other.acquire();
        let other_len = other.len_inner();
        let other_ptr = other.ptr();

        for i in 0..other_len {
            let (k, v) = unsafe { (*other_ptr.add(i)).clone() };
            self.insert(k, v);
        }

        other.release();
    }

    /// Reset iterator index for consuming iteration
    pub fn reset_iter(&self) {
        self.index.store(0, Ordering::Release);
    }
}

// ============================================================================
// MAP ITERATOR
// ============================================================================

pub struct MapIter<'a, K, V> {
    map: &'a AtomicMap<K, V>,
    index: usize,
}

impl<'a, K: Clone, V: Clone> Iterator for MapIter<'a, K, V> {
    type Item = (K, V);

    fn next(&mut self) -> Option<Self::Item> {
        self.map.acquire();
        let len = self.map.len_inner();
        let result = if self.index < len {
            let ptr = self.map.ptr();
            let item = unsafe { (*ptr.add(self.index)).clone() };
            self.index += 1;
            Some(item)
        } else {
            None
        };
        self.map.release();
        result
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.map.len().saturating_sub(self.index);
        (remaining, Some(remaining))
    }
}

impl<'a, K: Clone, V: Clone> ExactSizeIterator for MapIter<'a, K, V> {}
impl<'a, K: Clone, V: Clone> FusedIterator for MapIter<'a, K, V> {}

impl<K, V> AsMut<AtomicMap<K, V>> for AtomicMap<K, V> {
    fn as_mut(&mut self) -> &mut AtomicMap<K, V> {
        self
    }
}
impl<K, V> AsRef<AtomicMap<K, V>> for AtomicMap<K, V> {
    fn as_ref(&self) -> &AtomicMap<K, V> {
        self
    }
}
impl<K, V> RangeBounds<K> for AtomicMap<K, V>
where
    K: Clone + Ord,
    V: Clone,
{
    fn start_bound(&self) -> Bound<&K> {
        if self.is_empty() {
            return Bound::Unbounded;
        }
        unsafe {
            self.sort();
            Bound::Included(&(*self.inner.load(Ordering::Acquire).wrapping_add(0)).0)
        }
    }
    fn end_bound(&self) -> Bound<&K>
    where
        K: Ord + Clone,
        V: Clone,
    {
        if self.is_empty() {
            return Bound::Unbounded;
        }
        unsafe {
            self.sort();
            Bound::Included(
                &(*self
                    .inner
                    .load(Ordering::Acquire)
                    .wrapping_add(self.len.load(Ordering::Acquire) - 1))
                .0,
            )
        }
    }
}
impl<K, V> Default for AtomicMap<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K, V> FromIterator<(K, V)> for AtomicMap<K, V>
where
    K: PartialEq,
{
    fn from_iter<I: IntoIterator<Item = (K, V)>>(iter: I) -> Self {
        let map = Self::new();
        for (k, v) in iter {
            map.insert(k, v);
        }
        map
    }
}

impl<K, V> Clone for AtomicMap<K, V>
where
    K: Clone,
    V: Clone,
{
    fn clone(&self) -> Self {
        self.acquire();
        let len = self.len.load(Ordering::Acquire);

        if len == 0 {
            self.release();
            return Self::new();
        }

        let old_ptr = self.inner.load(Ordering::Acquire);
        let map = Self::with_capacity(len);
        let new_ptr = map.inner.load(Ordering::Acquire);

        unsafe {
            for i in 0..len {
                let entry = &*old_ptr.add(i);
                std::ptr::write(new_ptr.add(i), (entry.0.clone(), entry.1.clone()));
            }
        }

        map.len.store(len, Ordering::Release);
        self.release();
        map
    }
}

impl<K, V> PartialEq for AtomicMap<K, V>
where
    K: PartialEq,
    V: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        let self_len = self.len.load(Ordering::Acquire);
        let other_len = other.len.load(Ordering::Acquire);

        if self_len != other_len {
            return false;
        }

        if self_len == 0 {
            return true;
        }

        self.acquire();
        other.acquire();

        let self_ptr = self.inner.load(Ordering::Acquire);
        let other_ptr = other.inner.load(Ordering::Acquire);

        let result = unsafe {
            (0..self_len).all(|i| {
                let self_entry = &*self_ptr.add(i);
                let other_entry = &*other_ptr.add(i);
                self_entry == other_entry
            })
        };

        other.release();
        self.release();
        result
    }
}

impl<K, V> Eq for AtomicMap<K, V>
where
    K: Eq,
    V: Eq,
{
}

impl<K, V> Drop for AtomicMap<K, V> {
    fn drop(&mut self) {
        // Atomically swap pointer to null to prevent double-free
        let ptr = self.inner.swap(null_mut(), Ordering::AcqRel);
        let capacity = self.capacity.swap(0, Ordering::AcqRel);
        let len = self.len.swap(0, Ordering::AcqRel);

        if !ptr.is_null() && capacity > 0 {
            if len > 0 {
                unsafe {
                    for i in 0..len {
                        std::ptr::drop_in_place(ptr.add(i));
                    }
                }
            }
            let layout = Layout::array::<(K, V)>(capacity).unwrap();
            unsafe {
                std::alloc::dealloc(ptr as *mut u8, layout);
            }
        }
    }
}

impl<K, V> Index<K> for AtomicMap<K, V>
where
    K: PartialEq,
{
    type Output = V;

    fn index(&self, key: K) -> &Self::Output {
        self.acquire();
        let result = if let Some((_, v_ptr)) = self.find_entry(&key) {
            unsafe { &*v_ptr }
        } else {
            self.release();
            panic!("Key not found in map")
        };
        self.release();
        result
    }
}

impl<K, V> IndexMut<K> for AtomicMap<K, V>
where
    K: PartialEq,
{
    fn index_mut(&mut self, key: K) -> &mut Self::Output {
        self.acquire();
        let result = if let Some((_, v_ptr)) = self.find_entry(&key) {
            unsafe { &mut *v_ptr }
        } else {
            self.release();
            panic!("Key not found in map")
        };
        self.release();
        result
    }
}

impl<K, V> Iterator for AtomicMap<K, V>
where
    K: Clone,
    V: Clone,
{
    type Item = (K, V);

    fn next(&mut self) -> Option<Self::Item> {
        let current_index = self.index.fetch_add(1, Ordering::AcqRel);
        let len = self.len.load(Ordering::Acquire);

        if current_index < len {
            let ptr = self.inner.load(Ordering::Acquire);
            unsafe { Some((*ptr.add(current_index)).clone()) }
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.len.load(Ordering::Acquire);
        let index = self.index.load(Ordering::Acquire);
        let remaining = len.saturating_sub(index);
        (remaining, Some(remaining))
    }
}

impl<K, V> DoubleEndedIterator for AtomicMap<K, V>
where
    K: Clone,
    V: Clone,
{
    fn next_back(&mut self) -> Option<Self::Item> {
        let len = self.len.load(Ordering::Acquire);
        let current_index = self.index.load(Ordering::Acquire);

        if current_index < len {
            let new_len = len - 1;
            if self
                .len
                .compare_exchange(len, new_len, Ordering::AcqRel, Ordering::Relaxed)
                .is_ok()
            {
                let ptr = self.inner.load(Ordering::Acquire);
                unsafe { Some((*ptr.add(new_len)).clone()) }
            } else {
                None
            }
        } else {
            None
        }
    }
}

impl<K, V> ExactSizeIterator for AtomicMap<K, V>
where
    K: Clone,
    V: Clone,
{
    fn len(&self) -> usize {
        let len = self.len.load(Ordering::Acquire);
        let index = self.index.load(Ordering::Acquire);
        len.saturating_sub(index)
    }
}

impl<K, V> FusedIterator for AtomicMap<K, V>
where
    K: Clone,
    V: Clone,
{
}

// ============================================================================
// SET - Thread-safe auto-deduplicating collection with spinlock
// ============================================================================

#[derive(Debug)]
pub struct AtomicSet<T: Eq + Ord> {
    lock: AtomicBool,
    inner: AtomicPtr<T>,
    len: AtomicUsize,
    capacity: AtomicUsize,
    sorted: AtomicBool,
}

impl<T: Ord + Eq> AtomicSet<T> {
    #[inline]
    pub fn new() -> Self {
        Self {
            lock: AtomicBool::new(false),
            inner: AtomicPtr::new(null_mut()),
            len: AtomicUsize::new(0),
            capacity: AtomicUsize::new(0),
            sorted: AtomicBool::new(true),
        }
    }

    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            lock: AtomicBool::new(false),
            inner: AtomicPtr::new(alloc_array(capacity)),
            len: AtomicUsize::new(0),
            capacity: AtomicUsize::new(capacity),
            sorted: AtomicBool::new(true),
        }
    }

    #[inline]
    fn acquire(&self) {
        while self
            .lock
            .compare_exchange_weak(false, true, Ordering::Acquire, Ordering::Relaxed)
            .is_err()
        {
            std::hint::spin_loop();
        }
    }

    #[inline]
    fn release(&self) {
        self.lock.store(false, Ordering::Release);
    }

    #[inline]
    fn locked<R, F: FnOnce() -> R>(&self, f: F) -> R {
        self.acquire();
        let result = f();
        self.release();
        result
    }

    #[inline]
    fn ptr(&self) -> *mut T {
        self.inner.load(Ordering::Acquire)
    }

    #[inline]
    fn len_inner(&self) -> usize {
        self.len.load(Ordering::Acquire)
    }

    #[inline]
    fn cap_inner(&self) -> usize {
        self.capacity.load(Ordering::Acquire)
    }

    fn ensure_capacity(&self, additional: usize) {
        let len = self.len_inner();
        let cap = self.cap_inner();
        let required = len + additional;

        if required <= cap {
            return;
        }

        let new_cap = (cap * 2).max(required).max(4);
        let new_ptr = realloc_copy(self.ptr(), cap, new_cap, len);
        self.inner.store(new_ptr, Ordering::Release);
        self.capacity.store(new_cap, Ordering::Release);
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.len.load(Ordering::Relaxed)
    }

    #[inline]
    pub fn capacity(&self) -> usize {
        self.capacity.load(Ordering::Relaxed)
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    #[inline]
    pub fn is_sorted(&self) -> bool {
        self.sorted.load(Ordering::Relaxed)
    }

    /// Binary search for value - O(log n) for sorted, O(n) for unsorted
    pub fn contains(&self, value: &T) -> bool {
        self.locked(|| {
            let len = self.len_inner();
            if len == 0 {
                return false;
            }

            let ptr = self.ptr();
            if self.sorted.load(Ordering::Relaxed) {
                let slice = unsafe { std::slice::from_raw_parts(ptr, len) };
                slice.binary_search(value).is_ok()
            } else {
                unsafe { (0..len).any(|i| &*ptr.add(i) == value) }
            }
        })
    }

    /// Find index using binary search - requires sorted set
    #[inline]
    fn binary_search(&self, value: &T) -> Result<usize, usize> {
        let len = self.len_inner();
        if len == 0 {
            return Err(0);
        }

        let ptr = self.ptr();
        let slice = unsafe { std::slice::from_raw_parts(ptr, len) };
        slice.binary_search(value)
    }

    /// Find index for insertion - returns position where value should be inserted
    fn binary_search_insert_pos(&self, value: &T) -> usize {
        match self.binary_search(value) {
            Ok(i) | Err(i) => i,
        }
    }

    /// Insert maintaining sorted order - O(n) due to shifting
    pub fn insert(&self, v: T) -> bool {
        self.acquire();

        // Check for duplicates
        let len = self.len_inner();
        let ptr = self.ptr();

        if len > 0 {
            if self.sorted.load(Ordering::Relaxed) {
                let slice = unsafe { std::slice::from_raw_parts(ptr, len) };
                if slice.binary_search(&v).is_ok() {
                    self.release();
                    return false;
                }
            } else {
                for i in 0..len {
                    if unsafe { &*ptr.add(i) == &v } {
                        self.release();
                        return false;
                    }
                }
            }
        }

        self.ensure_capacity(1);
        let ptr = self.ptr(); // Re-fetch after potential realloc

        if self.sorted.load(Ordering::Relaxed) {
            let pos = self.binary_search_insert_pos(&v);
            unsafe {
                if pos < len {
                    std::ptr::copy(ptr.add(pos), ptr.add(pos + 1), len - pos);
                }
                std::ptr::write(ptr.add(pos), v);
            }
        } else {
            unsafe { std::ptr::write(ptr.add(len), v) };
        }

        self.len.store(len + 1, Ordering::Release);
        self.release();
        true
    }

    /// Insert without maintaining sorted order - O(1) amortized
    pub fn push(&self, v: T) -> bool {
        self.acquire();

        // Check for duplicates
        let len = self.len_inner();
        let ptr = self.ptr();

        if len > 0 {
            if self.sorted.load(Ordering::Relaxed) {
                let slice = unsafe { std::slice::from_raw_parts(ptr, len) };
                if slice.binary_search(&v).is_ok() {
                    self.release();
                    return false;
                }
            } else {
                for i in 0..len {
                    if unsafe { &*ptr.add(i) == &v } {
                        self.release();
                        return false;
                    }
                }
            }
        }

        self.ensure_capacity(1);
        let ptr = self.ptr();
        unsafe { std::ptr::write(ptr.add(len), v) };
        self.len.store(len + 1, Ordering::Release);
        self.sorted.store(false, Ordering::Release);
        self.release();
        true
    }

    /// Remove and return last element
    pub fn pop(&self) -> Option<T> {
        self.locked(|| {
            let len = self.len_inner();
            if len == 0 {
                return None;
            }

            let new_len = len - 1;
            self.len.store(new_len, Ordering::Release);
            unsafe { Some(std::ptr::read(self.ptr().add(new_len))) }
        })
    }

    /// Remove element at index - O(n) due to shifting
    pub fn remove_at(&self, i: usize) -> Option<T> {
        self.locked(|| {
            let len = self.len_inner();
            if i >= len {
                return None;
            }

            let ptr = self.ptr();
            let new_len = len - 1;
            unsafe {
                let ret = std::ptr::read(ptr.add(i));
                if i < new_len {
                    std::ptr::copy(ptr.add(i + 1), ptr.add(i), new_len - i);
                }
                self.len.store(new_len, Ordering::Release);
                Some(ret)
            }
        })
    }

    /// Remove value from set - O(log n) for sorted, O(n) for unsorted
    pub fn remove(&self, value: &T) -> bool {
        self.acquire();

        let len = self.len_inner();
        if len == 0 {
            self.release();
            return false;
        }

        let ptr = self.ptr();
        let idx = if self.sorted.load(Ordering::Relaxed) {
            let slice = unsafe { std::slice::from_raw_parts(ptr, len) };
            match slice.binary_search(value) {
                Ok(i) => Some(i),
                Err(_) => None,
            }
        } else {
            (0..len).find(|&i| unsafe { &*ptr.add(i) == value })
        };

        if let Some(i) = idx {
            let new_len = len - 1;
            unsafe {
                std::ptr::drop_in_place(ptr.add(i));
                if i < new_len {
                    std::ptr::copy(ptr.add(i + 1), ptr.add(i), new_len - i);
                }
            }
            self.len.store(new_len, Ordering::Release);
            self.release();
            true
        } else {
            self.release();
            false
        }
    }

    /// Swap remove - O(1) removal by swapping with last element (breaks sorted order)
    pub fn swap_remove(&self, value: &T) -> bool {
        self.acquire();

        let len = self.len_inner();
        if len == 0 {
            self.release();
            return false;
        }

        let ptr = self.ptr();
        let idx = if self.sorted.load(Ordering::Relaxed) {
            let slice = unsafe { std::slice::from_raw_parts(ptr, len) };
            match slice.binary_search(value) {
                Ok(i) => Some(i),
                Err(_) => None,
            }
        } else {
            (0..len).find(|&i| unsafe { &*ptr.add(i) == value })
        };

        if let Some(i) = idx {
            let new_len = len - 1;
            unsafe {
                std::ptr::drop_in_place(ptr.add(i));
                if i < new_len {
                    std::ptr::copy_nonoverlapping(ptr.add(new_len), ptr.add(i), 1);
                }
            }
            self.len.store(new_len, Ordering::Release);
            if len > 1 {
                self.sorted.store(false, Ordering::Release);
            }
            self.release();
            true
        } else {
            self.release();
            false
        }
    }

    /// Clear all elements
    pub fn clear(&self) {
        self.locked(|| {
            let len = self.len_inner();
            if len > 0 {
                let ptr = self.ptr();
                unsafe {
                    for i in 0..len {
                        std::ptr::drop_in_place(ptr.add(i));
                    }
                }
                self.len.store(0, Ordering::Release);
                self.sorted.store(true, Ordering::Release);
            }
        })
    }

    /// Sort the set - O(n log n)
    pub fn sort(&self) {
        self.locked(|| {
            let len = self.len_inner();
            if len <= 1 || self.sorted.load(Ordering::Relaxed) {
                self.sorted.store(true, Ordering::Release);
                return;
            }

            let ptr = self.ptr();
            unsafe {
                let slice = std::slice::from_raw_parts_mut(ptr, len);
                slice.sort_unstable();
            }
            self.sorted.store(true, Ordering::Release);
        })
    }

    /// Remove duplicates - sorts first if needed
    pub fn dedup(&self) {
        self.locked(|| {
            let len = self.len_inner();
            if len <= 1 {
                return;
            }

            // Sort first if needed
            if !self.sorted.load(Ordering::Relaxed) {
                let ptr = self.ptr();
                unsafe {
                    let slice = std::slice::from_raw_parts_mut(ptr, len);
                    slice.sort_unstable();
                }
                self.sorted.store(true, Ordering::Release);
            }

            let ptr = self.ptr();
            let mut write = 1;
            unsafe {
                for read in 1..len {
                    if *ptr.add(read) != *ptr.add(write - 1) {
                        if write != read {
                            std::ptr::copy_nonoverlapping(ptr.add(read), ptr.add(write), 1);
                        }
                        write += 1;
                    } else {
                        std::ptr::drop_in_place(ptr.add(read));
                    }
                }
            }
            self.len.store(write, Ordering::Release);
        })
    }

    /// Get first element
    pub fn first(&self) -> Option<T>
    where
        T: Clone,
    {
        self.locked(|| {
            if self.len_inner() > 0 {
                unsafe { Some((*self.ptr()).clone()) }
            } else {
                None
            }
        })
    }

    /// Get last element
    pub fn last(&self) -> Option<T>
    where
        T: Clone,
    {
        self.locked(|| {
            let len = self.len_inner();
            if len > 0 {
                unsafe { Some((*self.ptr().add(len - 1)).clone()) }
            } else {
                None
            }
        })
    }

    #[inline]
    pub fn get(&self, i: usize) -> Option<&T> {
        if i < self.len_inner() {
            unsafe { self.ptr().add(i).as_ref() }
        } else {
            None
        }
    }

    #[inline]
    pub fn get_mut(&mut self, i: usize) -> Option<&mut T> {
        if i < self.len_inner() {
            unsafe { self.ptr().add(i).as_mut() }
        } else {
            None
        }
    }

    /// Remove value (mutable variant) - for internal use
    fn remove_value_mut(&mut self, value: &T) -> bool {
        let len = self.len_inner();
        let ptr = self.ptr();

        for i in 0..len {
            if unsafe { &*ptr.add(i) } == value {
                self.swap_remove_at_mut(i);
                return true;
            }
        }
        false
    }

    /// Swap remove at index (mutable variant) - O(1)
    #[inline]
    fn swap_remove_at_mut(&mut self, i: usize) -> T {
        let len = self.len_inner();
        debug_assert!(i < len);

        let ptr = self.ptr();
        let ret = unsafe { std::ptr::read(ptr.add(i)) };
        let new_len = len - 1;

        if i != new_len {
            unsafe { std::ptr::copy(ptr.add(new_len), ptr.add(i), 1) };
            self.sorted.store(false, Ordering::Release);
        }
        self.len.store(new_len, Ordering::Release);
        ret
    }

    pub fn retain<F: FnMut(&T) -> bool>(&mut self, mut f: F) {
        let len = self.len_inner();
        let ptr = self.ptr();
        let mut write = 0;

        for read in 0..len {
            let item = unsafe { &*ptr.add(read) };
            if f(item) {
                if read != write {
                    unsafe { std::ptr::copy_nonoverlapping(ptr.add(read), ptr.add(write), 1) };
                }
                write += 1;
            } else {
                unsafe { std::ptr::drop_in_place(ptr.add(read)) };
            }
        }
        self.len.store(write, Ordering::Release);
    }

    pub fn extend(&mut self, other: &AtomicSet<T>)
    where
        T: Clone,
    {
        let other_len = other.len_inner();
        if other_len == 0 {
            return;
        }

        self.ensure_capacity(other_len);
        let other_ptr = other.ptr();

        for i in 0..other_len {
            let item = unsafe { (*other_ptr.add(i)).clone() };
            if !self.contains(&item) {
                let len = self.len_inner();
                unsafe { std::ptr::write(self.ptr().add(len), item) };
                self.len.store(len + 1, Ordering::Release);
            }
        }
        self.sorted.store(false, Ordering::Release);
    }

    pub fn extend_from_slice(&mut self, slice: &[T])
    where
        T: Clone,
    {
        if slice.is_empty() {
            return;
        }

        self.ensure_capacity(slice.len());
        for item in slice {
            if !self.contains(item) {
                let len = self.len_inner();
                unsafe { std::ptr::write(self.ptr().add(len), item.clone()) };
                self.len.store(len + 1, Ordering::Release);
            }
        }
        self.sorted.store(false, Ordering::Release);
    }

    pub fn reserve(&mut self, additional: usize) {
        self.ensure_capacity(additional);
    }

    pub fn shrink_to_fit(&mut self) {
        let len = self.len_inner();
        let cap = self.cap_inner();
        if len < cap && len > 0 {
            let new_ptr = realloc_copy(self.ptr(), cap, len, len);
            self.inner.store(new_ptr, Ordering::Release);
            self.capacity.store(len, Ordering::Release);
        } else if len == 0 {
            dealloc_array(self.ptr(), cap);
            self.inner.store(null_mut(), Ordering::Release);
            self.capacity.store(0, Ordering::Release);
        }
    }

    #[inline]
    pub fn as_slice(&self) -> &[T] {
        let len = self.len_inner();
        if len == 0 {
            return &[];
        }
        unsafe { std::slice::from_raw_parts(self.ptr(), len) }
    }

    #[inline]
    pub fn as_mut_slice(&mut self) -> &mut [T] {
        let len = self.len_inner();
        if len == 0 {
            return &mut [];
        }
        unsafe { std::slice::from_raw_parts_mut(self.ptr(), len) }
    }

    pub fn to_vec(&self) -> Vec<T>
    where
        T: Clone,
    {
        let len = self.len_inner();
        if len == 0 {
            return Vec::new();
        }

        let ptr = self.ptr();
        (0..len).map(|i| unsafe { (*ptr.add(i)).clone() }).collect()
    }

    pub fn from_vec(vec: Vec<T>) -> Self {
        if vec.is_empty() {
            return Self::new();
        }

        let set = Self::with_capacity(vec.len());
        for item in vec {
            if !set.contains(&item) {
                let len = set.len_inner();
                unsafe { std::ptr::write(set.ptr().add(len), item) };
                set.len.store(len + 1, Ordering::Release);
            }
        }
        set.sorted.store(false, Ordering::Release);
        set
    }

    #[inline]
    pub fn iter(&self) -> std::slice::Iter<'_, T> {
        self.as_slice().iter()
    }

    #[inline]
    pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, T> {
        self.as_mut_slice().iter_mut()
    }

    pub fn union(&self, other: &AtomicSet<T>) -> AtomicSet<T>
    where
        T: Clone,
    {
        let mut result = self.clone();
        result.extend(other);
        result
    }

    pub fn intersection(&self, other: &AtomicSet<T>) -> AtomicSet<T>
    where
        T: Clone,
    {
        let mut result: AtomicSet<T> = AtomicSet::new();
        for item in self.iter() {
            if other.contains(item) {
                result.ensure_capacity(1);
                let len = result.len_inner();
                unsafe { std::ptr::write(result.ptr().add(len), item.clone()) };
                result.len.store(len + 1, Ordering::Release);
            }
        }
        result
    }

    pub fn difference(&self, other: &AtomicSet<T>) -> AtomicSet<T>
    where
        T: Clone,
    {
        let mut result: AtomicSet<T> = AtomicSet::new();
        for item in self.iter() {
            if !other.contains(item) {
                result.ensure_capacity(1);
                let len = result.len_inner();
                unsafe { std::ptr::write(result.ptr().add(len), item.clone()) };
                result.len.store(len + 1, Ordering::Release);
            }
        }
        result
    }

    pub fn is_subset(&self, other: &AtomicSet<T>) -> bool {
        self.iter().all(|item| other.contains(item))
    }

    pub fn is_superset(&self, other: &AtomicSet<T>) -> bool {
        other.is_subset(self)
    }

    pub fn is_disjoint(&self, other: &AtomicSet<T>) -> bool {
        self.iter().all(|item| !other.contains(item))
    }

    #[inline]
    pub fn swap(&mut self, i: usize, j: usize) {
        let len = self.len_inner();
        if i < len && j < len && i != j {
            let ptr = self.ptr();
            unsafe { std::ptr::swap(ptr.add(i), ptr.add(j)) };
            self.sorted.store(false, Ordering::Release);
        }
    }

    pub fn reverse(&mut self) {
        let len = self.len_inner();
        if len <= 1 {
            return;
        }

        let ptr = self.ptr();
        let (mut i, mut j) = (0, len - 1);
        while i < j {
            unsafe { std::ptr::swap(ptr.add(i), ptr.add(j)) };
            i += 1;
            j -= 1;
        }
        self.sorted.store(false, Ordering::Release);
    }

    pub fn position<F: FnMut(&T) -> bool>(&self, mut f: F) -> Option<usize> {
        let len = self.len_inner();
        let ptr = self.ptr();
        (0..len).find(|&i| f(unsafe { &*ptr.add(i) }))
    }

    pub fn find<F: FnMut(&T) -> bool>(&self, mut f: F) -> Option<&T> {
        let len = self.len_inner();
        let ptr = self.ptr();
        for i in 0..len {
            let item = unsafe { &*ptr.add(i) };
            if f(item) {
                return Some(item);
            }
        }
        None
    }

    pub fn map<U, F: FnMut(&T) -> U>(&self, mut f: F) -> AtomicSet<U>
    where
        U: Eq + Ord,
    {
        let mut result: AtomicSet<U> = AtomicSet::with_capacity(self.len_inner());
        for item in self.iter() {
            let mapped = f(item);
            if !result.contains(&mapped) {
                result.ensure_capacity(1);
                let len = result.len_inner();
                unsafe { std::ptr::write(result.ptr().add(len), mapped) };
                result.len.store(len + 1, Ordering::Release);
            }
        }
        result
    }

    pub fn filter<F: FnMut(&T) -> bool>(&self, mut f: F) -> AtomicSet<T>
    where
        T: Clone,
    {
        let mut result: AtomicSet<T> = AtomicSet::new();
        for item in self.iter() {
            if f(item) && !result.contains(item) {
                let len = result.len_inner();
                result.ensure_capacity(1);
                unsafe { std::ptr::write(result.ptr().add(len), item.clone()) };
                result.len.store(len + 1, Ordering::Release);
            }
        }
        result
    }

    pub fn truncate(&mut self, new_len: usize) {
        let len = self.len_inner();
        if new_len >= len {
            return;
        }

        let ptr = self.ptr();
        for i in new_len..len {
            unsafe { std::ptr::drop_in_place(ptr.add(i)) };
        }
        self.len.store(new_len, Ordering::Release);
    }

    pub fn split_off(&mut self, at: usize) -> Self
    where
        T: Clone,
    {
        let len = self.len_inner();
        if at >= len {
            return Self::new();
        }

        let right_len = len - at;
        let right = Self::with_capacity(right_len);
        let ptr = self.ptr();
        let right_ptr = right.ptr();

        unsafe {
            for i in 0..right_len {
                std::ptr::write(right_ptr.add(i), (*ptr.add(at + i)).clone());
            }
        }

        right.len.store(right_len, Ordering::Release);
        right
            .sorted
            .store(self.sorted.load(Ordering::Relaxed), Ordering::Release);
        self.truncate(at);
        right
    }

    pub fn subset(&self, start: usize, end: usize) -> Self
    where
        T: Clone,
    {
        let len = self.len_inner();
        if start >= len || end <= start || end > len {
            return Self::new();
        }

        let subset_len = end - start;
        let result = Self::with_capacity(subset_len);
        let src = self.ptr();
        let dst = result.ptr();

        unsafe {
            for i in 0..subset_len {
                std::ptr::write(dst.add(i), (*src.add(start + i)).clone());
            }
        }

        result.len.store(subset_len, Ordering::Release);
        result
            .sorted
            .store(self.sorted.load(Ordering::Relaxed), Ordering::Release);
        result
    }

    // Legacy compatibility methods
    #[inline]
    pub fn index(&self, i: usize) -> Option<&T> {
        self.get(i)
    }

    #[inline]
    pub fn index_mut(&mut self, i: usize) -> Option<&mut T> {
        self.get_mut(i)
    }

    #[inline]
    pub fn extend_as_slice(&mut self, slice: &[T])
    where
        T: Clone,
    {
        self.extend_from_slice(slice)
    }

    /// Remove first element matching predicate (mutable variant)
    pub fn remove_if<F: FnMut(&T) -> bool>(&mut self, mut f: F) -> Option<T> {
        let len = self.len_inner();
        let ptr = self.ptr();
        for i in 0..len {
            if f(unsafe { &*ptr.add(i) }) {
                return Some(self.swap_remove_at_mut(i));
            }
        }
        None
    }

    /// Replace element at index
    pub fn replace(&mut self, i: usize, v: T) -> T {
        let ptr = self.ptr();
        unsafe { std::ptr::replace(ptr.add(i), v) }
    }

    /// Insert at index (mutable variant)
    pub fn insert_at(&mut self, i: usize, v: T) {
        if self.contains(&v) {
            return;
        }

        let len = self.len_inner();
        if i > len {
            return;
        }

        self.ensure_capacity(1);
        let ptr = self.ptr();

        unsafe {
            if i < len {
                std::ptr::copy(ptr.add(i), ptr.add(i + 1), len - i);
            }
            std::ptr::write(ptr.add(i), v);
        }

        self.len.store(len + 1, Ordering::Release);
        self.sorted.store(false, Ordering::Release);
    }

    pub fn find_range<F: Fn(&T) -> bool>(&self, f: F) -> Option<(usize, usize)> {
        let len = self.len_inner();
        if len == 0 {
            return None;
        }

        let ptr = self.ptr();
        let start = (0..len).find(|&i| f(unsafe { &*ptr.add(i) }))?;
        let end = (start + 1..len)
            .find(|&i| !f(unsafe { &*ptr.add(i) }))
            .unwrap_or(len);

        Some((start, end))
    }

    pub fn subdivide(&self, count: usize) -> AtomicSet<Self>
    where
        T: Clone,
    {
        if count == 0 {
            return AtomicSet::new();
        }

        let len = self.len_inner();
        if len == 0 {
            return AtomicSet::new();
        }

        let subsets: AtomicSet<Self> = AtomicSet::with_capacity(count);
        let base_size = len / count;
        let remainder = len % count;
        let mut start = 0;

        for i in 0..count {
            let size = base_size + if i < remainder { 1 } else { 0 };
            let end = start + size;
            let subset = self.subset(start, end);
            let sub_len = subsets.len_inner();
            unsafe { std::ptr::write(subsets.ptr().add(sub_len), subset) };
            subsets.len.store(sub_len + 1, Ordering::Release);
            start = end;
        }

        subsets
    }

    pub fn range_partition(&self, ranges: AtomicSet<(usize, usize)>) -> AtomicSet<Self>
    where
        T: Clone,
    {
        let subsets: AtomicSet<Self> = AtomicSet::with_capacity(ranges.len());
        for (start, end) in ranges.iter() {
            let subset = self.subset(*start, *end);
            let len = subsets.len_inner();
            unsafe { std::ptr::write(subsets.ptr().add(len), subset) };
            subsets.len.store(len + 1, Ordering::Release);
        }
        subsets
    }

    pub fn rotate_left(&mut self, mid: usize) {
        let len = self.len_inner();
        if mid == 0 || mid >= len {
            return;
        }

        let ptr = self.ptr();
        unsafe {
            let slice = std::slice::from_raw_parts_mut(ptr, len);
            slice.rotate_left(mid);
        }
        self.sorted.store(false, Ordering::Release);
    }

    pub fn rotate_right(&mut self, k: usize) {
        let len = self.len_inner();
        if k == 0 || len == 0 {
            return;
        }

        let k = k % len;
        if k > 0 {
            self.rotate_left(len - k);
        }
    }

    pub fn dealloc(&mut self) {
        let len = self.len_inner();
        let ptr = self.ptr();
        let cap = self.cap_inner();

        if len > 0 {
            unsafe {
                for i in 0..len {
                    std::ptr::drop_in_place(ptr.add(i));
                }
            }
        }

        dealloc_array(ptr, cap);
        self.inner.store(null_mut(), Ordering::Release);
        self.len.store(0, Ordering::Release);
        self.capacity.store(0, Ordering::Release);
        self.sorted.store(true, Ordering::Release);
    }

    pub fn copy_from(&mut self, other: &AtomicSet<T>)
    where
        T: Clone,
    {
        self.clear();
        self.extend(other);
        self.sorted
            .store(other.sorted.load(Ordering::Relaxed), Ordering::Release);
    }

    pub fn copy_from_slice(&mut self, slice: &[T])
    where
        T: Clone,
    {
        self.clear();
        self.extend_from_slice(slice);
    }

    pub fn clone_from(&mut self, other: &AtomicSet<T>)
    where
        T: Clone,
    {
        self.copy_from(other);
    }

    pub fn clone_from_slice(slice: &[T]) -> Self
    where
        T: Clone,
    {
        let mut set = Self::with_capacity(slice.len());
        set.extend_from_slice(slice);
        set
    }

    pub fn from_raw_parts(ptr: *mut T, len: usize) -> Self {
        Self {
            lock: AtomicBool::new(false),
            inner: AtomicPtr::new(ptr),
            len: AtomicUsize::new(len),
            capacity: AtomicUsize::new(len),
            sorted: AtomicBool::new(false),
        }
    }

    pub fn alloc(size: usize, capacity: usize, _zeroed: bool) -> Self {
        let cap = capacity.max(size);
        let mut set = Self::with_capacity(cap);
        set.len.store(size, Ordering::Release);
        set
    }

    pub fn resize(&mut self, new_size: usize) {
        let len = self.len_inner();
        if new_size < len {
            self.truncate(new_size);
        } else if new_size > len {
            self.ensure_capacity(new_size - len);
        }
    }

    pub fn split_at(&self, i: usize) -> (&[T], &[T]) {
        let slice = self.as_slice();
        if i > slice.len() {
            (slice, &[])
        } else {
            slice.split_at(i)
        }
    }

    pub fn split_at_mut(&mut self, i: usize) -> (&mut [T], &mut [T]) {
        let slice = self.as_mut_slice();
        if i > slice.len() {
            let len = slice.len();
            (slice, &mut [][..len.min(0)])
        } else {
            slice.split_at_mut(i)
        }
    }

    pub fn find_range_mut<F: Fn(&T) -> bool>(&mut self, f: F) -> Option<(usize, usize)> {
        self.find_range(f)
    }
}

// ============================================================================
// SET TRAIT IMPLEMENTATIONS
// ============================================================================

impl<T: Eq + Ord> Drop for AtomicSet<T> {
    fn drop(&mut self) {
        // Atomically swap pointer to null to prevent double-free
        let ptr = self.inner.swap(null_mut(), Ordering::AcqRel);
        let cap = self.capacity.swap(0, Ordering::AcqRel);
        let len = self.len.swap(0, Ordering::AcqRel);

        if !ptr.is_null() && cap > 0 {
            if len > 0 {
                unsafe {
                    for i in 0..len {
                        std::ptr::drop_in_place(ptr.add(i));
                    }
                }
            }
            dealloc_array(ptr, cap);
        }
    }
}

impl<T: Ord> Default for AtomicSet<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Eq + Ord + Clone> Clone for AtomicSet<T> {
    fn clone(&self) -> Self {
        let len = self.len_inner();
        if len == 0 {
            return Self::new();
        }

        let ptr = self.ptr();
        let result = Self::with_capacity(len);
        let result_ptr = result.ptr();

        unsafe {
            for i in 0..len {
                std::ptr::write(result_ptr.add(i), (*ptr.add(i)).clone());
            }
        }
        result.len.store(len, Ordering::Release);
        result
            .sorted
            .store(self.sorted.load(Ordering::Relaxed), Ordering::Release);
        result
    }
}

impl<T: Ord> AsMut<AtomicSet<T>> for AtomicSet<T> {
    fn as_mut(&mut self) -> &mut AtomicSet<T> {
        self
    }
}

impl<T: Ord> AsRef<AtomicSet<T>> for AtomicSet<T> {
    fn as_ref(&self) -> &AtomicSet<T> {
        self
    }
}

impl<T: Eq + Ord> Eq for AtomicSet<T> {}

impl<T: Ord> Ord for AtomicSet<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.as_slice().cmp(other.as_slice())
    }
}

impl<T: Eq + Ord + Clone> AddAssign for AtomicSet<T> {
    fn add_assign(&mut self, other: Self) {
        self.extend(&other);
    }
}

impl<T: Eq + Ord + Clone> Add for AtomicSet<T> {
    type Output = AtomicSet<T>;
    fn add(mut self, other: Self) -> Self::Output {
        self.extend(&other);
        self
    }
}

impl<T: Eq + Ord> SubAssign for AtomicSet<T> {
    fn sub_assign(&mut self, other: Self) {
        self.retain(|x| !other.contains(x));
    }
}

impl<T: Eq + Ord> Sub for AtomicSet<T> {
    type Output = AtomicSet<T>;
    fn sub(mut self, other: Self) -> Self::Output {
        self.retain(|x| !other.contains(x));
        self
    }
}

impl<T: Eq + Ord> Index<usize> for AtomicSet<T> {
    type Output = T;
    fn index(&self, i: usize) -> &Self::Output {
        self.get(i).expect("index out of bounds")
    }
}

impl<T: Eq + Ord> IndexMut<usize> for AtomicSet<T> {
    fn index_mut(&mut self, i: usize) -> &mut Self::Output {
        self.get_mut(i).expect("index out of bounds")
    }
}

impl<T: Eq + Ord> RangeBounds<T> for AtomicSet<T> {
    fn start_bound(&self) -> Bound<&T> {
        let len = self.len_inner();
        if len == 0 {
            return Bound::Unbounded;
        }
        unsafe { Bound::Included(&*self.ptr()) }
    }

    fn end_bound(&self) -> Bound<&T> {
        let len = self.len_inner();
        if len == 0 {
            return Bound::Unbounded;
        }
        unsafe { Bound::Included(&*self.ptr().add(len - 1)) }
    }
}

impl<T: Eq + Ord> From<Vec<T>> for AtomicSet<T> {
    fn from(vec: Vec<T>) -> Self {
        Self::from_vec(vec)
    }
}

impl<T: Ord> FromIterator<T> for AtomicSet<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut set = AtomicSet::new();
        for item in iter {
            set.push(item);
        }
        set
    }
}

impl<T: Eq + Ord> PartialOrd for AtomicSet<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: Eq + Ord> PartialEq for AtomicSet<T> {
    fn eq(&self, other: &Self) -> bool {
        self.as_slice() == other.as_slice()
    }
}
