use crate::maybe::futex::Futex;
use std::{
    alloc::Layout,
    iter::FusedIterator,
    ops::{Add, AddAssign, Index, IndexMut, Sub, SubAssign},
    ptr::null_mut,
    sync::atomic::{AtomicPtr, AtomicUsize, Ordering},
};

pub struct Map<K, V> {
    futex: Futex,
    inner: AtomicPtr<(K, V)>,
    len: AtomicUsize,
    capacity: AtomicUsize,
    index: AtomicUsize,
}
pub struct MapEntry<'a, K, V> {
    map: &'a Map<K, V>,
    key: K,
}
pub struct MapIter<'a, K, V> {
    map: &'a Map<K, V>,
    index: usize,
}
impl<K, V> Map<K, V> {
    pub fn new() -> Self {
        Map {
            futex: Futex::new(),
            inner: AtomicPtr::new(null_mut()),
            len: AtomicUsize::new(0),
            capacity: AtomicUsize::new(0),
            index: AtomicUsize::new(0),
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        if capacity == 0 {
            return Self::new();
        }

        let layout = Layout::array::<(K, V)>(capacity).unwrap();
        let ptr = unsafe { std::alloc::alloc(layout) } as *mut (K, V);

        Map {
            futex: Futex::new(),
            inner: AtomicPtr::new(ptr),
            len: AtomicUsize::new(0),
            capacity: AtomicUsize::new(capacity),
            index: AtomicUsize::new(0),
        }
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

    pub fn len(&self) -> usize {
        self.len.load(Ordering::Acquire)
    }

    pub fn capacity(&self) -> usize {
        self.capacity.load(Ordering::Acquire)
    }

    pub fn is_empty(&self) -> bool {
        self.len.load(Ordering::Acquire) == 0
    }

    pub fn contains_key(&self, key: &K) -> bool
    where
        K: PartialEq,
    {
        self.futex.lock();
        let result = self.find_entry(key).is_some();
        self.futex.unlock();
        result
    }

    pub fn get(&self, key: &K) -> Option<V>
    where
        K: PartialEq,
        V: Clone,
    {
        self.futex.lock();
        if let Some((_, v_ptr)) = self.find_entry(key) {
            let result = unsafe { (*v_ptr).clone() };
            self.futex.unlock();
            Some(result)
        } else {
            self.futex.unlock();
            None
        }
    }

    pub fn get_mut(&mut self, key: &K) -> Option<&mut V>
    where
        K: PartialEq,
    {
        self.futex.lock();
        if let Some((_, v_ptr)) = self.find_entry(key) {
            let result = unsafe { &mut *v_ptr };
            self.futex.unlock();
            Some(result)
        } else {
            self.futex.unlock();
            None
        }
    }

    pub fn get_key_value(&self, key: &K) -> Option<(K, V)>
    where
        K: PartialEq + Clone,
        V: Clone,
    {
        self.futex.lock();
        if let Some((k_ptr, v_ptr)) = self.find_entry(key) {
            let result = unsafe { ((*k_ptr).clone(), (*v_ptr).clone()) };
            self.futex.unlock();
            Some(result)
        } else {
            self.futex.unlock();
            None
        }
    }

    pub fn insert(&self, key: K, value: V) -> Option<V>
    where
        K: PartialEq,
    {
        self.futex.lock();

        // Check if key already exists
        if let Some((_, v_ptr)) = self.find_entry(&key) {
            let old_value = unsafe { std::ptr::read(v_ptr) };
            unsafe { std::ptr::write(v_ptr, value) };
            self.futex.unlock();
            return Some(old_value);
        }

        let len = self.len.load(Ordering::Acquire);
        let capacity = self.capacity.load(Ordering::Acquire);

        // Need to grow if at capacity
        if len >= capacity {
            self.reserve_x2();
        }

        // Reload pointer after potential reallocation
        let ptr = self.inner.load(Ordering::Acquire);
        unsafe {
            std::ptr::write(ptr.add(len), (key, value));
        }
        self.len.store(len + 1, Ordering::Release);
        self.futex.unlock();
        None
    }

    pub fn remove(&self, key: &K) -> Option<V>
    where
        K: PartialEq,
    {
        self.futex.lock();
        let len = self.len.load(Ordering::Acquire);
        let ptr = self.inner.load(Ordering::Acquire);

        if !ptr.is_null() && len > 0 {
            unsafe {
                for i in 0..len {
                    let entry = ptr.add(i);
                    if (*entry).0 == *key {
                        let old_value = std::ptr::read(&(*entry).1);
                        // Move remaining elements
                        std::ptr::copy(ptr.add(i + 1), ptr.add(i), len - i - 1);
                        self.len.store(len - 1, Ordering::Release);
                        self.futex.unlock();
                        return Some(old_value);
                    }
                }
            }
        }
        self.futex.unlock();
        None
    }

    pub fn remove_entry(&self, key: &K) -> Option<(K, V)>
    where
        K: PartialEq,
    {
        self.futex.lock();
        let len = self.len.load(Ordering::Acquire);
        let ptr = self.inner.load(Ordering::Acquire);

        if !ptr.is_null() && len > 0 {
            unsafe {
                for i in 0..len {
                    let entry = ptr.add(i);
                    if (*entry).0 == *key {
                        let old_entry = std::ptr::read(entry);
                        // Move remaining elements
                        std::ptr::copy(ptr.add(i + 1), ptr.add(i), len - i - 1);
                        self.len.store(len - 1, Ordering::Release);
                        self.futex.unlock();
                        return Some(old_entry);
                    }
                }
            }
        }
        self.futex.unlock();
        None
    }

    pub fn retain<F>(&self, mut f: F)
    where
        F: FnMut(&K, &mut V) -> bool,
        K: PartialEq,
    {
        self.futex.lock();
        let len = self.len.load(Ordering::Acquire);
        let ptr = self.inner.load(Ordering::Acquire);

        if !ptr.is_null() && len > 0 {
            let mut write_idx = 0;
            unsafe {
                for read_idx in 0..len {
                    let entry = ptr.add(read_idx);
                    let keep = f(&(*entry).0, &mut (*entry).1);

                    if keep {
                        if write_idx != read_idx {
                            std::ptr::copy_nonoverlapping(entry, ptr.add(write_idx), 1);
                        }
                        write_idx += 1;
                    } else {
                        std::ptr::drop_in_place(entry);
                    }
                }
            }
            self.len.store(write_idx, Ordering::Release);
        }
        self.futex.unlock();
    }

    pub fn keys(&self) -> Vec<K>
    where
        K: Clone,
    {
        self.futex.lock();
        let mut keys = Vec::new();
        let len = self.len.load(Ordering::Acquire);
        let ptr = self.inner.load(Ordering::Acquire);

        if !ptr.is_null() && len > 0 {
            keys.reserve(len);
            unsafe {
                for i in 0..len {
                    keys.push((*ptr.add(i)).0.clone());
                }
            }
        }
        self.futex.unlock();
        keys
    }

    pub fn values(&self) -> Vec<V>
    where
        V: Clone,
    {
        self.futex.lock();
        let mut values = Vec::new();
        let len = self.len.load(Ordering::Acquire);
        let ptr = self.inner.load(Ordering::Acquire);

        if !ptr.is_null() && len > 0 {
            values.reserve(len);
            unsafe {
                for i in 0..len {
                    values.push((*ptr.add(i)).1.clone());
                }
            }
        }
        self.futex.unlock();
        values
    }

    pub fn iter(&self) -> impl Iterator<Item=(&K, &V)> + '_ {
        (0..self.len()).filter_map(move |i| {
            self.futex.lock();
            let ptr = self.inner.load(Ordering::Acquire);
            let len = self.len.load(Ordering::Acquire);

            if !ptr.is_null() && i < len {
                let entry = unsafe { &*ptr.add(i) };
                let result = Some((&entry.0, &entry.1));
                self.futex.unlock();
                result
            } else {
                self.futex.unlock();
                None
            }
        })
    }

    pub fn entry(&'_ self, key: K) -> MapEntry<'_, K, V>
    where
        K: PartialEq + Clone,
    {
        MapEntry::<K, V> { map: self, key }
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

    fn find_entry(&self, key: &K) -> Option<(*mut K, *mut V)>
    where
        K: PartialEq,
    {
        let len = self.len.load(Ordering::Acquire);
        let ptr = self.inner.load(Ordering::Acquire);

        if !ptr.is_null() && len > 0 {
            unsafe {
                for i in 0..len {
                    let entry = ptr.add(i);
                    if (*entry).0 == *key {
                        return Some((&mut (*entry).0 as *mut K, &mut (*entry).1 as *mut V));
                    }
                }
            }
        }
        None
    }

    pub fn shrink_to(&self, new_capacity: usize) {
        self.futex.lock();
        let old_ptr = self.inner.load(Ordering::Acquire);
        let len = self.len.load(Ordering::Acquire);
        let old_capacity = self.capacity.load(Ordering::Acquire);

        // Don't shrink if new capacity is larger than current
        if new_capacity >= old_capacity {
            self.futex.unlock();
            return;
        }

        // Ensure we don't shrink below current length
        let new_capacity = new_capacity.max(len);

        if new_capacity == 0 {
            self.inner.store(null_mut(), Ordering::Release);
            self.capacity.store(0, Ordering::Release);
            self.futex.unlock();

            if !old_ptr.is_null() && old_capacity > 0 {
                let old_layout = Layout::array::<(K, V)>(old_capacity).unwrap();
                unsafe { std::alloc::dealloc(old_ptr as *mut u8, old_layout) };
            }
            return;
        }

        let new_layout = Layout::array::<(K, V)>(new_capacity).unwrap();
        let new_ptr = unsafe { std::alloc::alloc(new_layout) } as *mut (K, V);

        if new_ptr.is_null() {
            self.futex.unlock();
            std::alloc::handle_alloc_error(new_layout);
        }

        if !old_ptr.is_null() && len > 0 {
            unsafe {
                std::ptr::copy_nonoverlapping(old_ptr, new_ptr, len);
            }
        }

        self.inner.store(new_ptr, Ordering::Release);
        self.capacity.store(new_capacity, Ordering::Release);
        self.futex.unlock();

        if !old_ptr.is_null() && old_capacity > 0 {
            let old_layout = Layout::array::<(K, V)>(old_capacity).unwrap();
            unsafe { std::alloc::dealloc(old_ptr as *mut u8, old_layout) };
        }
    }

    fn reserve_x2(&self) {
        let capacity = self.capacity.load(Ordering::Acquire);
        let new_capacity = if capacity == 0 { 4 } else { capacity * 2 };
        self.reserve(new_capacity);
    }

    fn reserve(&self, new_capacity: usize) {
        let old_ptr = self.inner.load(Ordering::Acquire);
        let len = self.len.load(Ordering::Acquire);
        let old_capacity = self.capacity.load(Ordering::Acquire);

        // Don't grow if new capacity is not larger than current
        if new_capacity <= old_capacity {
            return;
        }

        let new_layout = Layout::array::<(K, V)>(new_capacity).unwrap();
        let new_ptr = unsafe { std::alloc::alloc(new_layout) } as *mut (K, V);

        if new_ptr.is_null() {
            std::alloc::handle_alloc_error(new_layout);
        }

        if !old_ptr.is_null() && len > 0 {
            unsafe {
                std::ptr::copy_nonoverlapping(old_ptr, new_ptr, len);
            }
        }

        self.inner.store(new_ptr, Ordering::Release);
        self.capacity.store(new_capacity, Ordering::Release);

        if !old_ptr.is_null() && old_capacity > 0 {
            let old_layout = Layout::array::<(K, V)>(old_capacity).unwrap();
            unsafe { std::alloc::dealloc(old_ptr as *mut u8, old_layout) };
        }
    }
}

impl<'a, K, V> MapEntry<'a, K, V>
where
    K: PartialEq,
{
    pub fn or_insert(self, default: V) -> &'a mut V {
        self.or_insert_with(|| default)
    }

    pub fn or_insert_with<F>(self, f: F) -> &'a mut V
    where
        F: FnOnce() -> V,
    {
        self.map.futex.lock();
        if let Some((_, v_ptr)) = self.map.find_entry(&self.key) {
            let result = unsafe { &mut *v_ptr };
            self.map.futex.unlock();
            result
        } else {
            let value = f();
            let len = self.map.len.load(Ordering::Acquire);
            let capacity = self.map.capacity.load(Ordering::Acquire);

            if len >= capacity {
                self.map.reserve_x2();
            }

            let ptr = self.map.inner.load(Ordering::Acquire);
            unsafe {
                std::ptr::write(ptr.add(len), (self.key, value));
                let result = &mut (*ptr.add(len)).1;
                self.map.len.store(len + 1, Ordering::Release);
                self.map.futex.unlock();
                result
            }
        }
    }

    pub fn key(&self) -> &K {
        &self.key
    }
}

impl<K, V> Default for Map<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K, V> Clone for Map<K, V>
where
    K: Clone,
    V: Clone,
{
    fn clone(&self) -> Self {
        self.futex.lock();
        let len = self.len.load(Ordering::Acquire);
        let capacity = self.capacity.load(Ordering::Acquire);
        let map = Self::with_capacity(capacity);

        let old_ptr = self.inner.load(Ordering::Acquire);
        let new_ptr = map.inner.load(Ordering::Acquire);

        if !old_ptr.is_null() && len > 0 {
            unsafe {
                for i in 0..len {
                    let entry = &*old_ptr.add(i);
                    std::ptr::write(new_ptr.add(i), (entry.0.clone(), entry.1.clone()));
                }
            }
        }

        map.len.store(len, Ordering::Release);
        self.futex.unlock();
        map
    }
}

impl<K, V> PartialEq for Map<K, V>
where
    K: PartialEq,
    V: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        if self.len() != other.len() {
            return false;
        }

        self.futex.lock();
        other.futex.lock();

        let len = self.len.load(Ordering::Acquire);
        let self_ptr = self.inner.load(Ordering::Acquire);
        let other_ptr = other.inner.load(Ordering::Acquire);

        let result = if self_ptr.is_null() || other_ptr.is_null() {
            len == 0
        } else {
            unsafe {
                (0..len).all(|i| {
                    let self_entry = &*self_ptr.add(i);
                    let other_entry = &*other_ptr.add(i);
                    self_entry.0 == other_entry.0 && self_entry.1 == other_entry.1
                })
            }
        };

        other.futex.unlock();
        self.futex.unlock();
        result
    }
}

impl<K, V> Eq for Map<K, V>
where
    K: Eq,
    V: Eq,
{}

impl<K, V> Drop for Map<K, V> {
    fn drop(&mut self) {
        self.clear();
        let ptr = self.inner.load(Ordering::Relaxed);
        let capacity = self.capacity.load(Ordering::Relaxed);

        if !ptr.is_null() && capacity > 0 {
            let layout = Layout::array::<(K, V)>(capacity).unwrap();
            unsafe {
                std::alloc::dealloc(ptr as *mut u8, layout);
            }
        }
    }
}

impl<K, V> Index<K> for Map<K, V>
where
    K: PartialEq,
{
    type Output = V;

    fn index(&self, key: K) -> &Self::Output {
        self.futex.lock();
        if let Some((_, v_ptr)) = self.find_entry(&key) {
            let result = unsafe { &*v_ptr };
            self.futex.unlock();
            result
        } else {
            self.futex.unlock();
            panic!("Key not found in map");
        }
    }
}

impl<K, V> IndexMut<K> for Map<K, V>
where
    K: PartialEq,
{
    fn index_mut(&mut self, key: K) -> &mut Self::Output {
        self.futex.lock();
        if let Some((_, v_ptr)) = self.find_entry(&key) {
            let result = unsafe { &mut *v_ptr };
            self.futex.unlock();
            result
        } else {
            self.futex.unlock();
            panic!("Key not found in map");
        }
    }
}
impl<K, V> Iterator for Map<K, V>
where
    K: Clone,
    V: Clone,
{
    type Item = (K, V);

    fn next(&mut self) -> Option<Self::Item> {
        self.futex.lock();
        let len = self.len.load(Ordering::Acquire);
        let current_index = self.index.fetch_add(1, Ordering::Acquire);

        if current_index < len {
            let ptr = self.inner.load(Ordering::Acquire);
            if !ptr.is_null() {
                let entry = unsafe { &*ptr.add(current_index) };
                let result = Some((entry.0.clone(), entry.1.clone()));
                self.futex.unlock();
                result
            } else {
                self.futex.unlock();
                None
            }
        } else {
            self.futex.unlock();
            None
        }
    }
}
impl<K, V> DoubleEndedIterator for Map<K, V>
where
    K: Clone,
    V: Clone,
{
    fn next_back(&mut self) -> Option<Self::Item> {
        self.futex.lock();
        let len = self.len.load(Ordering::Acquire);
        let current_index = self.index.load(Ordering::Acquire);

        if current_index > 0 {
            let new_index = current_index - 1;
            self.index.store(new_index, Ordering::Release);
            let ptr = self.inner.load(Ordering::Acquire);

            if !ptr.is_null() && new_index < len {
                let entry = unsafe { &*ptr.add(new_index) };
                let result = Some((entry.0.clone(), entry.1.clone()));
                self.futex.unlock();
                result
            } else {
                self.futex.unlock();
                None
            }
        } else {
            self.futex.unlock();
            None
        }
    }
}
impl<K, V> ExactSizeIterator for Map<K, V>
where
    K: Clone,
    V: Clone,
{
    fn len(&self) -> usize {
        self.len.load(Ordering::Acquire)
    }
}
impl<K, V> FusedIterator for Map<K, V>
where
    K: Clone,
    V: Clone,
{}

#[derive(Debug)]
pub struct Set<T: Eq + Ord> {
    futex: Futex,
    inner: AtomicPtr<T>,
    len: AtomicUsize,
    capacity: AtomicUsize,
}

impl<T: Ord + Eq> Set<T> {
    #[inline]
    pub fn new() -> Self {
        Self {
            futex: Futex::new(),
            inner: AtomicPtr::new(null_mut()),
            len: AtomicUsize::new(0),
            capacity: AtomicUsize::new(0),
        }
    }

    #[inline]
    pub fn alloc(size: usize, capacity: usize, zeroed: bool) -> Self {
        let capacity = if capacity > size { capacity } else { size };
        if capacity == 0 {
            return Self::new();
        }
        let layout = Layout::array::<T>(capacity).unwrap();
        let pointer = if zeroed {
            unsafe { std::alloc::alloc_zeroed(layout) }
        } else {
            unsafe { std::alloc::alloc(layout) }
        };
        if pointer.is_null() {
            std::alloc::handle_alloc_error(layout);
        }
        Self {
            futex: Futex::new(),
            inner: AtomicPtr::new(pointer as *mut T),
            len: AtomicUsize::new(size),
            capacity: AtomicUsize::new(capacity),
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self::alloc(0, capacity, false)
    }

    pub fn resize(&mut self, size: usize) {
        let current_capacity = self.capacity.load(Ordering::SeqCst);

        if size > current_capacity {
            let new_capacity = std::cmp::max(current_capacity * 2, size);
            let old_layout = Layout::array::<T>(current_capacity).unwrap();
            let new_layout = Layout::array::<T>(new_capacity).unwrap();

            let old_ptr = self.inner.load(Ordering::SeqCst);
            let new_ptr = if old_ptr.is_null() {
                unsafe { std::alloc::alloc(new_layout) }
            } else {
                unsafe { std::alloc::realloc(old_ptr as *mut u8, old_layout, new_layout.size()) }
            };

            if new_ptr.is_null() {
                std::alloc::handle_alloc_error(new_layout);
            }

            self.inner.store(new_ptr as *mut T, Ordering::SeqCst);
            self.capacity.store(new_capacity, Ordering::SeqCst);
        } else if size == 0 {
            self.dealloc();
            return;
        }
        self.len.store(size, Ordering::SeqCst);
    }

    #[inline]
    pub fn dealloc(&mut self) {
        let ptr = self.inner.load(Ordering::SeqCst);
        let capacity = self.capacity.load(Ordering::SeqCst);

        if !ptr.is_null() && capacity > 0 {
            let layout = Layout::array::<T>(capacity).unwrap();
            unsafe {
                std::alloc::dealloc(ptr as *mut u8, layout);
            }
        }

        self.inner.store(null_mut(), Ordering::SeqCst);
        self.len.store(0, Ordering::SeqCst);
        self.capacity.store(0, Ordering::SeqCst);
    }

    #[inline]
    pub fn default() -> Self {
        Self::new()
    }

    #[inline]
    pub fn copy_from(&mut self, other: &Set<T>)
    where
        T: Clone,
    {
        let other_len = other.len.load(Ordering::SeqCst);
        let other_ptr = other.inner.load(Ordering::SeqCst);

        if other_ptr.is_null() || other_len == 0 {
            self.dealloc();
            return;
        }

        if self.capacity.load(Ordering::SeqCst) < other_len
            || self.inner.load(Ordering::SeqCst).is_null()
        {
            self.dealloc();
            let layout = Layout::array::<T>(other_len).unwrap();
            let new_ptr = unsafe { std::alloc::alloc(layout) } as *mut T;
            if new_ptr.is_null() {
                std::alloc::handle_alloc_error(layout);
            }
            self.inner.store(new_ptr, Ordering::SeqCst);
            self.capacity.store(other_len, Ordering::SeqCst);
        }

        unsafe {
            for i in 0..other_len {
                std::ptr::write(
                    self.inner.load(Ordering::SeqCst).add(i),
                    (*other_ptr.add(i)).clone(),
                );
            }
        }
        self.len.store(other_len, Ordering::SeqCst);
    }

    pub fn copy_from_slice(&mut self, slice: &[T])
    where
        T: Clone,
    {
        if slice.is_empty() {
            self.dealloc();
            return;
        }

        if self.capacity.load(Ordering::SeqCst) < slice.len() {
            let layout = Layout::array::<T>(slice.len()).unwrap();
            let new_ptr = unsafe { std::alloc::alloc(layout) } as *mut T;
            if new_ptr.is_null() {
                std::alloc::handle_alloc_error(layout);
            }

            let old_ptr = self.inner.load(Ordering::SeqCst);
            let old_capacity = self.capacity.load(Ordering::SeqCst);
            if !old_ptr.is_null() && old_capacity > 0 {
                let old_layout = Layout::array::<T>(old_capacity).unwrap();
                unsafe {
                    std::alloc::dealloc(old_ptr as *mut u8, old_layout);
                }
            }

            self.inner.store(new_ptr, Ordering::SeqCst);
            self.capacity.store(slice.len(), Ordering::SeqCst);
        }

        unsafe {
            for (i, item) in slice.iter().enumerate() {
                std::ptr::write(self.inner.load(Ordering::SeqCst).add(i), item.clone());
            }
        }
        self.len.store(slice.len(), Ordering::SeqCst);
    }

    #[inline]
    pub fn as_ptr(&self) -> *const T {
        self.inner.load(Ordering::SeqCst)
    }

    #[inline]
    pub fn as_mut_ptr(&mut self) -> *mut T {
        self.inner.load(Ordering::SeqCst)
    }

    #[inline]
    pub fn size(&self) -> usize {
        self.len.load(Ordering::SeqCst)
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.len.load(Ordering::SeqCst)
    }

    #[inline]
    pub fn capacity(&self) -> usize {
        self.capacity.load(Ordering::SeqCst)
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len.load(Ordering::SeqCst) == 0
    }

    pub fn clone_from(&mut self, other: &Set<T>)
    where
        T: Clone,
    {
        let other_len = other.len.load(Ordering::SeqCst);
        let other_inner = other.inner.load(Ordering::SeqCst);

        if other_inner.is_null() || other_len == 0 {
            self.dealloc();
            return;
        }

        if self.capacity.load(Ordering::SeqCst) < other_len
            || self.inner.load(Ordering::SeqCst).is_null()
        {
            self.dealloc();
            let layout = Layout::array::<T>(other_len).unwrap();
            let new_ptr = unsafe { std::alloc::alloc(layout) } as *mut T;
            if new_ptr.is_null() {
                std::alloc::handle_alloc_error(layout);
            }
            self.inner.store(new_ptr, Ordering::SeqCst);
            self.capacity.store(other_len, Ordering::SeqCst);
        }

        unsafe {
            for i in 0..other_len {
                std::ptr::write(
                    self.inner.load(Ordering::SeqCst).add(i),
                    (*other_inner.add(i)).clone(),
                );
            }
        }
        self.len.store(other_len, Ordering::SeqCst);
    }

    #[inline]
    pub fn from_raw_parts(pointer: *mut T, size: usize) -> Self {
        Self {
            futex: Futex::new(),
            inner: AtomicPtr::new(pointer),
            len: AtomicUsize::new(size),
            capacity: AtomicUsize::new(size),
        }
    }

    pub fn clone_from_slice(slice: &[T]) -> Self
    where
        T: Clone,
    {
        if slice.is_empty() {
            return Self::new();
        }

        let layout = Layout::array::<T>(slice.len()).unwrap();
        let ptr = unsafe { std::alloc::alloc(layout) } as *mut T;
        if ptr.is_null() {
            std::alloc::handle_alloc_error(layout);
        }

        unsafe {
            for (i, item) in slice.iter().enumerate() {
                std::ptr::write(ptr.add(i), item.clone());
            }
        }

        Self {
            futex: Futex::new(),
            inner: AtomicPtr::new(ptr),
            len: AtomicUsize::new(slice.len()),
            capacity: AtomicUsize::new(slice.len()),
        }
    }

    #[inline]
    pub fn as_slice(&self) -> &[T] {
        let len = self.len.load(Ordering::Acquire);
        let ptr = self.inner.load(Ordering::Acquire);

        if len == 0 || ptr.is_null() {
            &[]
        } else {
            unsafe { std::slice::from_raw_parts(ptr, len) }
        }
    }

    #[inline]
    pub fn as_mut_slice(&mut self) -> &mut [T] {
        let len = self.len.load(Ordering::SeqCst);
        let capacity = self.capacity.load(Ordering::SeqCst);
        let ptr = self.inner.load(Ordering::SeqCst);

        if len == 0 || capacity == 0 || ptr.is_null() {
            &mut []
        } else {
            unsafe { std::slice::from_raw_parts_mut(ptr, len) }
        }
    }

    #[inline]
    pub fn split_at(&self, i: usize) -> (&[T], &[T]) {
        let len = self.len.load(Ordering::SeqCst);
        if i > len {
            panic!("split index out of bounds");
        }

        let ptr = self.inner.load(Ordering::SeqCst);
        if len == 0 || ptr.is_null() {
            (&[], &[])
        } else {
            unsafe {
                let left: &[T] = std::slice::from_raw_parts(ptr, i);
                let right: &[T] = std::slice::from_raw_parts(ptr.add(i), len - i);
                (left, right)
            }
        }
    }

    #[inline]
    pub fn split_at_mut(&mut self, i: usize) -> (&mut [T], &mut [T]) {
        let len = self.len.load(Ordering::SeqCst);
        if i > len {
            panic!("split index out of bounds");
        }

        let ptr = self.inner.load(Ordering::SeqCst);
        unsafe {
            (
                std::slice::from_raw_parts_mut(ptr, i),
                std::slice::from_raw_parts_mut(ptr.add(i), len - i),
            )
        }
    }
    #[inline]
    pub fn split_off(&mut self, i: usize) -> Self {
        assert!(
            i <= self.len.load(Ordering::SeqCst),
            "Split index out of bounds"
        );

        let current_len = self.len.load(Ordering::SeqCst);
        let right_size = current_len - i;

        if right_size == 0 {
            return Self::new();
        }

        let mut right = Self::with_capacity(right_size);
        let current_ptr = self.inner.load(Ordering::SeqCst);

        unsafe {
            std::ptr::copy_nonoverlapping(
                current_ptr.add(i),
                right.inner.load(Ordering::SeqCst),
                right_size,
            );
        }

        right.len.store(right_size, Ordering::SeqCst);
        self.len.store(i, Ordering::SeqCst);

        right
    }

    #[inline]
    pub fn push(&mut self, v: T) {
        if self.contains(&v) {
            return;
        }

        let current_len = self.len.load(Ordering::SeqCst);
        let current_capacity = self.capacity.load(Ordering::SeqCst);

        if current_len == current_capacity {
            let new_capacity = if current_capacity == 0 {
                1
            } else {
                current_capacity * 2
            };
            self.resize(new_capacity);
        }

        unsafe {
            std::ptr::write(self.inner.load(Ordering::SeqCst).add(current_len), v);
        }
        self.len.store(current_len + 1, Ordering::SeqCst);
    }

    #[inline]
    pub fn pop(&mut self) -> Option<T> {
        let current_len = self.len.load(Ordering::SeqCst);
        if current_len == 0 {
            return None;
        }

        let new_len = current_len - 1;
        self.len.store(new_len, Ordering::SeqCst);
        unsafe {
            Some(std::ptr::read(
                self.inner.load(Ordering::SeqCst).add(new_len),
            ))
        }
    }

    #[inline]
    pub fn pop_at(&mut self, i: usize) -> Option<T> {
        let current_len = self.len.load(Ordering::SeqCst);
        if i >= current_len {
            return None;
        }

        let new_len = current_len - 1;
        let current_ptr = self.inner.load(Ordering::SeqCst);

        unsafe {
            let ret = std::ptr::read(current_ptr.add(i));
            if i < new_len {
                std::ptr::copy(current_ptr.add(i + 1), current_ptr.add(i), new_len - i);
            }
            self.len.store(new_len, Ordering::SeqCst);
            Some(ret)
        }
    }

    #[inline]
    pub fn clear(&mut self) {
        let current_len = self.len.load(Ordering::SeqCst);
        if current_len > 0 {
            unsafe {
                let ptr = self.inner.load(Ordering::SeqCst);
                for i in 0..current_len {
                    std::ptr::drop_in_place(ptr.add(i));
                }
            }
            self.len.store(0, Ordering::SeqCst);
        }
    }

    #[inline]
    pub fn index(&self, i: usize) -> Option<&T> {
        let current_len = self.len.load(Ordering::SeqCst);
        if i < current_len {
            unsafe { self.inner.load(Ordering::SeqCst).add(i).as_ref() }
        } else {
            None
        }
    }

    #[inline]
    pub fn index_mut(&mut self, i: usize) -> Option<&mut T> {
        let current_len = self.len.load(Ordering::SeqCst);
        if i < current_len {
            unsafe { self.inner.load(Ordering::SeqCst).add(i).as_mut() }
        } else {
            None
        }
    }

    #[inline]
    pub fn from_vec(vec: Vec<T>) -> Self {
        if vec.is_empty() {
            return Self::new();
        }

        let mut set = Self::with_capacity(vec.len());
        for item in vec {
            if !set.contains(&item) {
                let current_len = set.len.load(Ordering::SeqCst);
                unsafe {
                    std::ptr::write(set.inner.load(Ordering::SeqCst).add(current_len), item);
                }
                set.len.store(current_len + 1, Ordering::SeqCst);
            }
        }
        set
    }

    pub fn to_vec(&self) -> Vec<T>
    where
        T: Clone,
    {
        let current_len = self.len.load(Ordering::SeqCst);
        if current_len == 0 {
            return Vec::new();
        }

        let mut vec = Vec::with_capacity(current_len);
        let ptr = self.inner.load(Ordering::SeqCst);

        unsafe {
            for i in 0..current_len {
                vec.push((*ptr.add(i)).clone());
            }
        }
        vec
    }

    #[inline]
    pub fn extend(&mut self, set: &Set<T>) {
        if set.is_empty() {
            return;
        }

        let set_len = set.len.load(Ordering::SeqCst);
        let set_ptr = set.inner.load(Ordering::SeqCst);

        for i in 0..set_len {
            let item = unsafe { &*set_ptr.add(i) };
            if !self.contains(item) {
                self.push(unsafe { std::ptr::read(set_ptr.add(i)) });
            }
        }
    }
    #[inline]
    pub fn extend_as_slice(&mut self, set: &[T]) {
        if set.is_empty() {
            return;
        }

        // Only add items that aren't already in the set
        let mut i = 0;
        let pointer = set.as_ptr();
        while i < set.len() {
            let item = unsafe { pointer.add(i).read() };
            if !self.contains(&item) {
                self.push(item);
            }
            i += 1;
        }
    }

    #[inline]
    pub fn reserve(&mut self, additional: usize) {
        if additional == 0 {
            return;
        }

        let current_capacity = self.capacity.load(Ordering::SeqCst);
        let new_capacity = current_capacity + additional;

        if current_capacity == 0 {
            let layout = Layout::array::<T>(new_capacity).unwrap();
            let new_ptr = unsafe { std::alloc::alloc(layout) } as *mut T;
            if new_ptr.is_null() {
                std::alloc::handle_alloc_error(layout);
            }
            self.inner.store(new_ptr, Ordering::SeqCst);
            self.capacity.store(new_capacity, Ordering::SeqCst);
            return;
        }

        let old_layout = Layout::array::<T>(current_capacity).unwrap();
        let new_layout = Layout::array::<T>(new_capacity).unwrap();

        let old_ptr = self.inner.load(Ordering::SeqCst);
        let new_ptr =
            unsafe { std::alloc::realloc(old_ptr as *mut u8, old_layout, new_layout.size()) }
                as *mut T;

        if new_ptr.is_null() {
            std::alloc::handle_alloc_error(new_layout);
        }

        self.inner.store(new_ptr, Ordering::SeqCst);
        self.capacity.store(new_capacity, Ordering::SeqCst);
    }

    #[inline]
    pub fn sort(&mut self)
    where
        T: Ord,
    {
        let current_len = self.len.load(Ordering::SeqCst);
        if current_len <= 1 {
            return;
        }

        if current_len <= 16 {
            self.insertion_sort();
        } else {
            self.quicksort(0, current_len - 1);
        }
    }

    #[inline]
    fn insertion_sort(&mut self)
    where
        T: Ord,
    {
        let current_len = self.len.load(Ordering::SeqCst);
        let ptr = self.inner.load(Ordering::SeqCst);

        for i in 1..current_len {
            let key = unsafe { std::ptr::read(ptr.add(i)) };
            let mut j = i;

            while j > 0 {
                let prev = unsafe { &*ptr.add(j - 1) };
                if prev <= &key {
                    break;
                }
                unsafe { std::ptr::copy(ptr.add(j - 1), ptr.add(j), 1) };
                j -= 1;
            }
            unsafe { std::ptr::write(ptr.add(j), key) };
        }
    }

    #[inline]
    fn quicksort(&mut self, low: usize, high: usize)
    where
        T: Ord,
    {
        if low < high {
            let pivot = self.partition(low, high);
            if pivot > 0 {
                self.quicksort(low, pivot - 1);
            }
            self.quicksort(pivot + 1, high);
        }
    }

    #[inline]
    fn partition(&mut self, low: usize, high: usize) -> usize
    where
        T: Ord,
    {
        let ptr = self.inner.load(Ordering::SeqCst);
        let pivot = unsafe { &*ptr.add(high) };
        let mut i = low;

        for j in low..high {
            if unsafe { &*ptr.add(j) } <= pivot {
                unsafe { std::ptr::swap(ptr.add(i), ptr.add(j)) };
                i += 1;
            }
        }
        unsafe { std::ptr::swap(ptr.add(i), ptr.add(high)) };
        i
    }
    #[inline]
    pub fn swap(&mut self, i: usize, j: usize) {
        let current_len = self.len.load(Ordering::SeqCst);
        if i < current_len && j < current_len && i != j {
            unsafe {
                std::ptr::swap(
                    self.inner.load(Ordering::SeqCst).add(i),
                    self.inner.load(Ordering::SeqCst).add(j),
                );
            }
        }
    }

    #[inline]
    pub fn swap_remove(&mut self, i: usize) -> T {
        let current_len = self.len.load(Ordering::SeqCst);
        debug_assert!(i < current_len);

        let ptr = self.inner.load(Ordering::SeqCst);
        let ret = unsafe { std::ptr::read(ptr.add(i)) };
        let new_len = current_len - 1;
        self.len.store(new_len, Ordering::SeqCst);
        if i != new_len {
            unsafe {
                std::ptr::copy(ptr.add(new_len), ptr.add(i), 1);
            }
        }
        ret
    }

    #[inline]
    pub fn retain<F: FnMut(&T) -> bool>(&mut self, mut f: F) {
        let mut i = 0;
        while i < self.len.load(Ordering::SeqCst) {
            if !f(unsafe { &*self.inner.load(Ordering::SeqCst).add(i) }) {
                self.swap_remove(i);
            } else {
                i += 1;
            }
        }
    }

    #[inline]
    pub fn remove<F: FnMut(&T) -> bool>(&mut self, mut f: F) -> Option<T> {
        let current_len = self.len.load(Ordering::SeqCst);
        let ptr = self.inner.load(Ordering::SeqCst);
        for i in 0..current_len {
            if f(unsafe { &*ptr.add(i) }) {
                return Some(self.swap_remove(i));
            }
        }
        None
    }

    pub fn replace(&mut self, i: usize, v: T) -> T {
        unsafe { std::ptr::replace(self.inner.load(Ordering::SeqCst).add(i), v) }
    }

    #[inline]
    pub fn truncate(&mut self, len: usize) {
        let current_len = self.len.load(Ordering::SeqCst);
        if len < current_len {
            unsafe {
                let tail = current_len - len;
                let ptr = self.inner.load(Ordering::SeqCst).add(len);
                for i in 0..tail {
                    std::ptr::drop_in_place(ptr.add(i));
                }
                self.len.store(len, Ordering::SeqCst);
            }
        }
    }

    #[inline]
    pub fn insert(&mut self, i: usize, v: T) {
        let current_len = self.len.load(Ordering::SeqCst);
        if i <= current_len && !self.contains(&v) {
            self.reserve(1);
            let ptr = self.inner.load(Ordering::SeqCst);
            unsafe {
                if i < current_len {
                    std::ptr::copy(ptr.add(i), ptr.add(i + 1), current_len - i);
                }
                std::ptr::write(ptr.add(i), v);
                self.len.store(current_len + 1, Ordering::SeqCst);
            }
        }
    }

    #[inline]
    pub fn iter(&self) -> std::slice::Iter<'_, T> {
        let current_len = self.len.load(Ordering::SeqCst);
        let ptr = self.inner.load(Ordering::SeqCst);
        unsafe { std::slice::from_raw_parts(ptr, current_len) }.iter()
    }

    #[inline]
    pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, T> {
        let current_len = self.len.load(Ordering::SeqCst);
        let ptr = self.inner.load(Ordering::SeqCst);
        unsafe { std::slice::from_raw_parts_mut(ptr, current_len) }.iter_mut()
    }

    #[inline]
    pub fn position<F: FnMut(&T) -> bool>(&self, mut f: F) -> Option<usize> {
        let current_len = self.len.load(Ordering::SeqCst);
        let ptr = self.inner.load(Ordering::SeqCst);
        for i in 0..current_len {
            if f(unsafe { &*ptr.add(i) }) {
                return Some(i);
            }
        }
        None
    }

    #[inline]
    pub fn contains(&self, value: &T) -> bool {
        self.iter().any(|v| v == value)
    }

    #[inline]
    pub fn map<U, F: FnMut(&T) -> U>(&self, mut f: F) -> Set<U>
    where
        U: Eq + Ord,
    {
        let current_len = self.len.load(Ordering::SeqCst);
        let mut result = Set::with_capacity(current_len);
        for item in self.iter() {
            let mapped = f(item);
            if !result.contains(&mapped) {
                result.push(mapped);
            }
        }
        result
    }

    #[inline]
    pub fn find<F: FnMut(&T) -> bool>(&self, mut f: F) -> Option<&T> {
        let current_len = self.len.load(Ordering::SeqCst);
        let ptr = self.inner.load(Ordering::SeqCst);
        for i in 0..current_len {
            unsafe {
                let item = &*ptr.add(i);
                if f(item) {
                    return Some(item);
                }
            }
        }
        None
    }

    #[inline]
    pub fn find_range<F: Fn(&T) -> bool>(&self, f: F) -> Option<(usize, usize)> {
        let current_len = self.len.load(Ordering::SeqCst);
        if current_len == 0 {
            return None;
        }

        let ptr = self.inner.load(Ordering::SeqCst);
        let start = (0..current_len).find(|&i| f(unsafe { &*ptr.add(i) }))?;
        let end = (start + 1..current_len)
            .find(|&i| !f(unsafe { &*ptr.add(i) }))
            .unwrap_or(current_len);

        Some((start, end))
    }

    #[inline]
    pub fn find_range_mut<F: FnMut(&T) -> bool>(&mut self, mut f: F) -> Option<(usize, usize)> {
        let current_len = self.len.load(Ordering::SeqCst);
        if current_len == 0 {
            return None;
        }

        let ptr = self.inner.load(Ordering::SeqCst);
        let start = (0..current_len).find(|&i| f(unsafe { &*ptr.add(i) }))?;
        let end = (start + 1..current_len)
            .find(|&i| !f(unsafe { &*ptr.add(i) }))
            .unwrap_or(current_len);

        Some((start, end))
    }

    #[inline]
    pub fn range_partition(&self, ranges: Set<(usize, usize)>) -> Set<Self>
    where
        T: Clone,
    {
        let mut subsets: Set<Self> = Set::new();
        for (start, end) in ranges.iter() {
            subsets.push(self.subset(*start, *end));
        }
        subsets
    }

    #[inline]
    pub fn subdivide(&self, count: usize) -> Set<Self>
    where
        T: Clone,
    {
        if count == 0 || self.is_empty() {
            return Set::new();
        }

        let mut subsets = Set::with_capacity(count);
        let current_len = self.len.load(Ordering::SeqCst);
        let base_size = current_len / count;
        let remainder = current_len % count;

        let mut start = 0;
        for i in 0..count {
            let subset_size = base_size + if i < remainder { 1 } else { 0 };
            let end = start + subset_size;
            subsets.push(self.subset(start, end));
            start = end;
        }

        subsets
    }

    #[inline]
    pub fn subset(&self, start: usize, end: usize) -> Self
    where
        T: Clone,
    {
        let current_len = self.len.load(Ordering::SeqCst);
        if start >= current_len || end <= start || end > current_len {
            Self::new()
        } else {
            let len = end - start;
            let mut data = Self::with_capacity(len);
            let ptr = self.inner.load(Ordering::SeqCst);
            unsafe {
                for i in 0..len {
                    std::ptr::write(
                        data.inner.load(Ordering::SeqCst).add(i),
                        (*ptr.add(start + i)).clone(),
                    );
                }
            }
            data.len.store(len, Ordering::SeqCst);
            data
        }
    }

    #[inline]
    pub fn reverse(&mut self) {
        let current_len = self.len.load(Ordering::SeqCst);
        if current_len <= 1 {
            return;
        }
        let mut i = 0;
        let mut j = current_len - 1;
        let ptr = self.inner.load(Ordering::SeqCst);
        while i < j {
            unsafe {
                std::ptr::swap(ptr.add(i), ptr.add(j));
            }
            i += 1;
            j -= 1;
        }
    }

    #[inline]
    pub fn rotate_left(&mut self, mid: usize) {
        let current_len = self.len.load(Ordering::SeqCst);
        if mid > 0 && mid < current_len {
            let ptr = self.inner.load(Ordering::SeqCst);
            unsafe {
                let slice = std::slice::from_raw_parts_mut(ptr, current_len);
                slice.rotate_left(mid);
            }
        }
    }

    #[inline]
    pub fn rotate_right(&mut self, k: usize) {
        let current_len = self.len.load(Ordering::SeqCst);
        if k > 0 && current_len > 0 {
            let k = k % current_len;
            if k > 0 {
                self.rotate_left(current_len - k);
            }
        }
    }
}

impl<T: Eq + Ord> Drop for Set<T> {
    fn drop(&mut self) {
        let ptr = self.inner.load(Ordering::SeqCst);
        let capacity = self.capacity.load(Ordering::SeqCst);
        let current_len = self.len.load(Ordering::SeqCst);

        if !ptr.is_null() && capacity > 0 {
            unsafe {
                for i in 0..current_len {
                    std::ptr::drop_in_place(ptr.add(i));
                }
                std::alloc::dealloc(ptr as *mut u8, Layout::array::<T>(capacity).unwrap());
            }
        }
    }
}

impl<T: Ord> Default for Set<T> {
    fn default() -> Self {
        Self {
            inner: AtomicPtr::new(null_mut()),
            len: AtomicUsize::new(0),
            capacity: AtomicUsize::new(0),
            futex: Futex::new(),
        }
    }
}

impl<T: Eq + Ord + Clone> Clone for Set<T> {
    fn clone(&self) -> Self {
        let current_len = self.len.load(Ordering::SeqCst);
        let mut result = Self::with_capacity(current_len);
        for item in self.iter() {
            result.push(item.clone());
        }
        result
    }
}

impl<T: Ord> AsMut<Set<T>> for Set<T> {
    fn as_mut(&mut self) -> &mut Set<T> {
        self
    }
}

impl<T: Ord> AsRef<Set<T>> for Set<T> {
    fn as_ref(&self) -> &Set<T> {
        self
    }
}

impl<T: Eq + Ord> Eq for Set<T> {}

impl<T: Ord> Ord for Set<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self
            .len
            .load(Ordering::SeqCst)
            .cmp(&other.len.load(Ordering::SeqCst))
        {
            std::cmp::Ordering::Equal => {
                let self_ptr = self.inner.load(Ordering::SeqCst);
                let other_ptr = other.inner.load(Ordering::SeqCst);
                for i in 0..self.len.load(Ordering::SeqCst) {
                    match unsafe { (*self_ptr.add(i)).cmp(&*other_ptr.add(i)) } {
                        std::cmp::Ordering::Equal => continue,
                        ord => return ord,
                    }
                }
                std::cmp::Ordering::Equal
            }
            ord => ord,
        }
    }
}
impl<T: Eq + Ord> AddAssign for Set<T> {
    fn add_assign(&mut self, other: Self) {
        self.extend(&other);
    }
}

impl<T: Eq + Ord> Add for Set<T> {
    type Output = Set<T>;
    fn add(self, other: Self) -> Self::Output {
        let mut set = self;
        set.extend(&other);
        set
    }
}

impl<T: Eq + Ord> SubAssign for Set<T> {
    fn sub_assign(&mut self, other: Self) {
        self.retain(|x| !other.contains(x));
    }
}

impl<T: Eq + Ord> Sub for Set<T> {
    type Output = Set<T>;
    fn sub(self, other: Self) -> Self::Output {
        let mut set = self;
        set.retain(|x| !other.contains(x));
        set
    }
}

impl<T: Eq + Ord> Index<usize> for Set<T> {
    type Output = T;
    fn index(&self, i: usize) -> &Self::Output {
        if i >= self.len.load(Ordering::SeqCst) {
            panic!(
                "index out of bounds: the len is {} but the index is {}",
                self.len.load(Ordering::SeqCst),
                i
            );
        }
        unsafe { &*self.inner.load(Ordering::SeqCst).add(i) }
    }
}

impl<T: Eq + Ord> IndexMut<usize> for Set<T> {
    fn index_mut(&mut self, i: usize) -> &mut Self::Output {
        if i >= self.len.load(Ordering::SeqCst) {
            panic!(
                "index out of bounds: the len is {} but the index is {}",
                self.len.load(Ordering::SeqCst),
                i
            );
        }
        unsafe { &mut *self.inner.load(Ordering::SeqCst).add(i) }
    }
}
impl<T: Eq + Ord> std::ops::RangeBounds<T> for Set<T> {
    fn start_bound(&self) -> std::ops::Bound<&T> {
        if self.len.load(Ordering::SeqCst) == 0 {
            std::ops::Bound::Unbounded
        } else {
            std::ops::Bound::Included(&self[0])
        }
    }

    fn end_bound(&self) -> std::ops::Bound<&T> {
        if self.len.load(Ordering::SeqCst) == 0 {
            std::ops::Bound::Unbounded
        } else {
            std::ops::Bound::Included(&self[self.len.load(Ordering::SeqCst) - 1])
        }
    }
}

impl<T: Eq + Ord> From<Vec<T>> for Set<T> {
    fn from(vec: Vec<T>) -> Self {
        if vec.is_empty() {
            Set::new()
        } else {
            let mut set = Set::with_capacity(vec.len());
            for item in vec {
                if !set.contains(&item) {
                    set.push(item);
                }
            }
            set
        }
    }
}

impl<T: Ord> FromIterator<T> for Set<T> {
    fn from_iter<I: IntoIterator<Item=T>>(iter: I) -> Self {
        let mut set = Set::new();
        for item in iter {
            set.push(item);
        }
        set
    }
}

impl<T: Eq + Ord> PartialOrd for Set<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: Eq + Ord> PartialEq for Set<T> {
    fn eq(&self, other: &Self) -> bool {
        self.len.load(Ordering::SeqCst) == other.len.load(Ordering::SeqCst)
            && self.iter().zip(other.iter()).all(|(a, b)| a == b)
    }
}
