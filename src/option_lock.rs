use core::clone::Clone;
use core::ops::FnOnce;
use core::option::Option::{self, None, Some};
use core::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::hash::Hasher;
use std::collections::HashMap;
use std::mem::MaybeUninit;
use std::ops::{Deref, DerefMut};
use std::sync::Arc;
use std::sync::{Mutex, OnceLock, Weak};
use std::thread::ThreadId;

use std::sync::atomic::AtomicPtr;
use std::thread;

#[derive(Debug)]
struct WaitNode {
    next: AtomicPtr<WaitNode>,
    thread: thread::Thread,
    woken: AtomicBool,
}

static WAIT_QUEUE: AtomicPtr<WaitNode> = AtomicPtr::new(std::ptr::null_mut());

// Shared counter registry so that all guards for the same pointer coordinate
// through a single Arc<AtomicUsize>. Use address-as-usize to keep it Send/Sync.
type CounterMap = HashMap<usize, Weak<AtomicUsize>>;
static COUNTERS: OnceLock<Mutex<CounterMap>> = OnceLock::new();

// Reentrant lock tracking: maps allocation address to (owner_thread_id, recursion_count)
type ReentrantMap = HashMap<usize, (ThreadId, usize)>;
static REENTRANT_LOCKS: OnceLock<Mutex<ReentrantMap>> = OnceLock::new();

#[inline]
fn reentrant_locks() -> &'static Mutex<ReentrantMap> {
    REENTRANT_LOCKS.get_or_init(|| Mutex::new(HashMap::new()))
}

#[inline]
fn counters() -> &'static Mutex<CounterMap> {
    COUNTERS.get_or_init(|| Mutex::new(HashMap::new()))
}

#[inline]
fn get_shared_counter_for<T>(raw_ptr: *const T) -> Arc<AtomicUsize> {
    let key = raw_ptr as usize;
    let mut map = counters().lock().unwrap();
    
    // Clean up expired weak references while we have the lock
    if let Some(weak) = map.get(&key) {
        if let Some(strong) = weak.upgrade() {
            return strong;
        } else {
            map.remove(&key);
        }
    }
    
    let counter = Arc::new(AtomicUsize::new(0));
    map.insert(key, Arc::downgrade(&counter));
    counter
}

#[inline]
fn remove_shared_counter_for<T>(raw_ptr: *const T) {
    let key = raw_ptr as usize;
    if let Some(m) = COUNTERS.get() {
        let mut map = m.lock().unwrap();
        map.remove(&key); 
    }
    // Also clean up reentrant lock info
    if let Some(r) = REENTRANT_LOCKS.get() {
        let mut reentrant_map = r.lock().unwrap();
        reentrant_map.remove(&key);
    }
}

impl WaitNode {
    fn new() -> Self {
        WaitNode {
            next: AtomicPtr::new(std::ptr::null_mut()),
            thread: thread::current(),
            woken: AtomicBool::new(false),
        }
    }

    fn park_current_thread() {
        let node = Box::into_raw(Box::new(WaitNode::new()));

        // Add to wait queue atomically
        let mut attempts = 0;
        loop {
            let head = WAIT_QUEUE.load(Ordering::Acquire);
            unsafe { (*node).next.store(head, Ordering::Relaxed) };
            
            match WAIT_QUEUE.compare_exchange_weak(head, node, Ordering::Release, Ordering::Relaxed) {
                Ok(_) => break,
                Err(_) => {
                    attempts += 1;
                    if attempts > 100 {
                        std::hint::spin_loop();
                    }
                }
            }
        }

        // Park until woken with exponential backoff
        let mut backoff = 1u32;
        while !unsafe { (*node).woken.load(Ordering::Acquire) } {
            if backoff <= 256 {
                for _ in 0..backoff {
                    std::hint::spin_loop();
                }
                backoff = backoff.saturating_mul(2);
            } else {
                thread::park();
                backoff = 1; // Reset backoff after parking
            }
        }

        // Cleanup
        unsafe { drop(Box::from_raw(node)) };
    }

    fn wake_waiters() {
        let mut current = WAIT_QUEUE.swap(std::ptr::null_mut(), Ordering::AcqRel);
        while !current.is_null() {
            unsafe {
                let node = &*current;
                let next = node.next.load(Ordering::Acquire);
                node.woken.store(true, Ordering::Release);
                node.thread.unpark();
                current = next;
            }
        }
    }

    fn wait_for_value() {
        let node_ptr = Box::into_raw(Box::new(WaitNode::new()));

        // Atomically add to wait queue
        let mut attempts = 0;
        loop {
            let head = WAIT_QUEUE.load(Ordering::Acquire);
            unsafe { (*node_ptr).next.store(head, Ordering::Relaxed) };
            
            match WAIT_QUEUE.compare_exchange_weak(head, node_ptr, Ordering::Release, Ordering::Relaxed) {
                Ok(_) => break,
                Err(_) => {
                    attempts += 1;
                    if attempts > 100 {
                        std::hint::spin_loop();
                    }
                }
            }
        }

        // Wait with exponential backoff
        let mut backoff = 1u32;
        while !unsafe { (*node_ptr).woken.load(Ordering::Acquire) } {
            if backoff <= 256 {
                for _ in 0..backoff {
                    std::hint::spin_loop();
                }
                backoff = backoff.saturating_mul(2);
            } else {
                thread::park();
                backoff = 1;
            }
        }

        unsafe { drop(Box::from_raw(node_ptr)) };
    }
}

#[derive(Debug)]
pub enum OptionLock<T> {
    None,
    Maybe(MaybeUninit<T>),
    Some(AtomicPtr<T>),
}

impl<T> OptionLock<T> {
    /// Creates a new uninitialized OptionLock in the Some state.
    pub const fn new() -> Self {
        OptionLock::Some(AtomicPtr::new(std::ptr::null_mut()))
    }

    /// Creates a new OptionLock in the None state.
    pub const fn none() -> Self {
        OptionLock::None
    }

    /// Creates a new OptionLock with the given value in the Maybe state.
    pub const fn maybe(value: T) -> OptionLock<T> {
        OptionLock::Maybe(MaybeUninit::new(value))
    }

    /// Creates a new OptionLock with the given value in the Some state.
    pub fn some(value: T) -> OptionLock<T> {
        OptionLock::Some(AtomicPtr::new(Box::into_raw(Box::new(value))))
    }

    /// Promotes a Maybe variant to Some by allocating the value on the heap.
    /// Returns the new OptionLock, leaving the original in None state.
    pub fn promote(&mut self) -> Self {
        match std::mem::take(self) {
            OptionLock::Maybe(maybe) => {
                let value = unsafe { maybe.assume_init() };
                OptionLock::Some(AtomicPtr::new(Box::into_raw(Box::new(value))))
            }
            other => other,
        }
    }

    /// Acquires a write lock, blocking until available.
    pub fn write(&self) -> WriteGuard<T> {
        let mut backoff = 1u32;
        loop {
            if let Some(guard) = self.try_lock_write() {
                return guard;
            }
            
            // Exponential backoff before parking
            if backoff <= 64 {
                for _ in 0..backoff {
                    std::hint::spin_loop();
                }
                backoff = backoff.saturating_mul(2);
            } else {
                WaitNode::park_current_thread();
                backoff = 1; // Reset after parking
            }
        }
    }
    /// Acquires a read lock, blocking until available.
    pub fn read(&self) -> ReadGuard<T> {
        let mut backoff = 1u32;
        loop {
            if let Some(guard) = self.try_lock_read() {
                return guard;
            }
            
            // Exponential backoff before parking
            if backoff <= 64 {
                for _ in 0..backoff {
                    std::hint::spin_loop();
                }
                backoff = backoff.saturating_mul(2);
            } else {
                WaitNode::park_current_thread();
                backoff = 1; // Reset after parking
            }
        }
    }

    /// Attempts to get a reference to the value without locking.
    /// Returns None if the value is not available or locked.
    pub fn try_lock(&self) -> Option<&T> {
        match self {
            OptionLock::Some(ptr) => {
                let raw_ptr = ptr.load(Ordering::Acquire);
                if !raw_ptr.is_null() {
                    // Check if not write-locked
                    let counter = get_shared_counter_for(raw_ptr);
                    let current = counter.load(Ordering::Acquire);
                    if current & 0x80000000 == 0 {
                        Some(unsafe { &*raw_ptr })
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            OptionLock::Maybe(maybe) => unsafe { maybe.as_ptr().as_ref() },
            OptionLock::None => None,
        }
    }

    /// Attempts to acquire a read lock without blocking.
    pub fn try_lock_read(&self) -> Option<ReadGuard<T>> {
        match self {
            OptionLock::Some(ptr) => {
                let raw_ptr = ptr.load(Ordering::Acquire);
                if !raw_ptr.is_null() {
                    let counter = get_shared_counter_for(raw_ptr);
                    let mut attempts = 0;
                    loop {
                        let current = counter.load(Ordering::Acquire);
                        // Can't read if writer has lock (high bit set)
                        if current & 0x80000000 != 0 {
                            return None;
                        }
                        // Try to increment reader count
                        if counter.compare_exchange_weak(current, current + 1, Ordering::AcqRel, Ordering::Relaxed).is_ok() {
                            return Some(ReadGuard::new_with_counter(raw_ptr, counter));
                        }
                        // Avoid infinite retry loops
                        attempts += 1;
                        if attempts > 16 {
                            return None;
                        }
                        std::hint::spin_loop();
                    }
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Attempts to acquire a write lock without blocking.
    /// Supports reentrant locking for the same thread.
    pub fn try_lock_write(&self) -> Option<WriteGuard<T>> {
        match self {
            OptionLock::Some(ptr) => {
                let raw_ptr = ptr.load(Ordering::Acquire);
                if !raw_ptr.is_null() {
                    let key = raw_ptr as usize;
                    let current_thread = std::thread::current().id();
                    
                    // Check if current thread already owns this lock (reentrant case)
                    {
                        let reentrant_map = reentrant_locks().lock().ok()?;
                        if let Some((owner_thread, _recursion_count)) = reentrant_map.get(&key) {
                            if *owner_thread == current_thread {
                                // Reentrant lock - increment recursion count
                                drop(reentrant_map);
                                let mut reentrant_map = reentrant_locks().lock().ok()?;
                                if let Some((_, count)) = reentrant_map.get_mut(&key) {
                                    *count += 1;
                                }
                                let counter = get_shared_counter_for(raw_ptr);
                                return Some(WriteGuard { inner: raw_ptr as *mut T, counter });
                            }
                        }
                    }
                    
                    // Try to acquire new write lock
                    let counter = get_shared_counter_for(raw_ptr);
                    let current = counter.load(Ordering::Acquire);
                    // Can only acquire write lock if no readers and no writer (counter == 0)
                    if current == 0 && counter.compare_exchange(0, 0x80000000, Ordering::AcqRel, Ordering::Relaxed).is_ok() {
                        // Record this thread as owner
                        if let Ok(mut reentrant_map) = reentrant_locks().lock() {
                            reentrant_map.insert(key, (current_thread, 1));
                        }
                        Some(WriteGuard { inner: raw_ptr as *mut T, counter })
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Executes a closure with a reference to the value.
    /// Panics if the value is None.
    pub fn with<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&T) -> R,
    {
        match self {
            OptionLock::Some(_) => {
                let guard = self.read();
                f(&*guard)
            }
            OptionLock::Maybe(maybe) => {
                let value = unsafe { maybe.as_ptr().as_ref().unwrap_unchecked() };
                f(value)
            }
            OptionLock::None => panic!("Cannot access a None value"),
        }
    }

    /// Like `with`, but catches panics in `f` and returns `None` on failure.
    /// Does not alter the underlying state and logs a recovery message.
    pub fn with_catch<F, R>(&self, f: F) -> Option<R>
    where
        F: std::panic::UnwindSafe + FnOnce(&T) -> R,
    {
        use std::panic::{catch_unwind, AssertUnwindSafe};
        match self {
            OptionLock::Some(_) => {
                let guard = self.read();
                match catch_unwind(AssertUnwindSafe(|| f(&*guard))) {
                    Ok(v) => Some(v),
                    Err(_) => {
                        eprintln!("OptionLock.with_catch: closure panicked; recovered");
                        None
                    }
                }
            }
            OptionLock::Maybe(maybe) => {
                let val = unsafe { maybe.as_ptr().as_ref().unwrap_unchecked() };
                match catch_unwind(AssertUnwindSafe(|| f(val))) {
                    Ok(v) => Some(v),
                    Err(_) => {
                        eprintln!("OptionLock.with_catch(Maybe): closure panicked; recovered");
                        None
                    }
                }
            }
            OptionLock::None => None,
        }
    }

    /// Executes a closure with a mutable reference to the value.
    /// Panics if the value is None.
    pub fn with_mut<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut T) -> R,
    {
        match self {
            OptionLock::Some(_) => {
                let mut guard = self.write();
                f(&mut *guard)
            }
            OptionLock::Maybe(maybe) => {
                let val = unsafe { maybe.as_mut_ptr().as_mut().unwrap_unchecked() };
                f(val)
            }
            OptionLock::None => panic!("Cannot mutate a None value"),
        }
    }

    /// Like `with_mut`, but catches panics in `f` and returns `None` on failure.
    /// Ensures guards are dropped and logs a recovery message.
    pub fn with_mut_catch<F, R>(&mut self, f: F) -> Option<R>
    where
        F: std::panic::UnwindSafe + FnOnce(&mut T) -> R,
    {
        use std::panic::{catch_unwind, AssertUnwindSafe};
        match self {
            OptionLock::Some(_) => {
                let mut guard = self.write();
                match catch_unwind(AssertUnwindSafe(|| f(&mut *guard))) {
                    Ok(v) => Some(v),
                    Err(_) => {
                        eprintln!("OptionLock.with_mut_catch: closure panicked; recovered");
                        None
                    }
                }
            }
            OptionLock::Maybe(maybe) => {
                let val = unsafe { maybe.as_mut_ptr().as_mut().unwrap_unchecked() };
                match catch_unwind(AssertUnwindSafe(|| f(val))) {
                    Ok(v) => Some(v),
                    Err(_) => {
                        eprintln!("OptionLock.with_mut_catch(Maybe): closure panicked; recovered");
                        None
                    }
                }
            }
            OptionLock::None => None,
        }
    }

    pub fn try_with<F, R>(&self, f: F) -> Option<R>
    where
        F: FnOnce(&T) -> R,
    {
        match self {
            OptionLock::Some(_) => self.try_lock_read().map(|guard| f(&*guard)),
            OptionLock::Maybe(maybe) => unsafe { maybe.as_ptr().as_ref().map(f) },
            OptionLock::None => None,
        }
    }
    /// Resilient variant of `try_with` that catches panics and returns `None` on failure.
    pub fn try_with_catch<F, R>(&self, f: F) -> Option<R>
    where
        F: std::panic::UnwindSafe + FnOnce(&T) -> R,
    {
        use std::panic::{catch_unwind, AssertUnwindSafe};
        match self {
            OptionLock::Some(_) => {
                self.try_lock_read().and_then(|guard| {
                    match catch_unwind(AssertUnwindSafe(|| f(&*guard))) {
                        Ok(result) => Some(result),
                        Err(_) => {
                            eprintln!("OptionLock.try_with_catch: closure panicked; recovered");
                            None
                        }
                    }
                })
            }
            OptionLock::Maybe(maybe) => unsafe {
                maybe.as_ptr().as_ref().and_then(|v| {
                    match catch_unwind(AssertUnwindSafe(|| f(v))) {
                        Ok(result) => Some(result),
                        Err(_) => {
                            eprintln!("OptionLock.try_with_catch(Maybe): closure panicked; recovered");
                            None
                        }
                    }
                })
            },
            OptionLock::None => None,
        }
    }

    pub fn try_with_mut<F, R>(&mut self, f: F) -> Option<R>
    where
        F: FnOnce(&mut T) -> R,
    {
        match self {
            OptionLock::Some(_) => self.try_lock_write().map(|mut guard| f(&mut *guard)),
            OptionLock::Maybe(maybe) => unsafe { maybe.as_mut_ptr().as_mut().map(f) },
            OptionLock::None => None,
        }
    }

    /// Resilient variant of `try_with_mut` that catches panics and returns `None` on failure.
    pub fn try_with_mut_catch<F, R>(&mut self, f: F) -> Option<R>
    where
        F: std::panic::UnwindSafe + FnOnce(&mut T) -> R,
    {
        use std::panic::{catch_unwind, AssertUnwindSafe};
        match self {
            OptionLock::Some(_) => {
                self.try_lock_write().and_then(|mut guard| {
                    match catch_unwind(AssertUnwindSafe(|| f(&mut *guard))) {
                        Ok(result) => Some(result),
                        Err(_) => {
                            eprintln!("OptionLock.try_with_mut_catch: closure panicked; recovered");
                            None
                        }
                    }
                })
            }
            OptionLock::Maybe(maybe) => unsafe {
                maybe.as_mut_ptr().as_mut().and_then(|v| {
                    match catch_unwind(AssertUnwindSafe(|| f(v))) {
                        Ok(result) => Some(result),
                        Err(_) => {
                            eprintln!("OptionLock.try_with_mut_catch(Maybe): closure panicked; recovered");
                            None
                        }
                    }
                })
            },
            OptionLock::None => None,
        }
    }

    /// Map with panic recovery. Returns `OptionLock::None` if the closure panics or the value is absent.
    pub fn map_catch<U, F>(&self, f: F) -> OptionLock<U>
    where
        F: std::panic::UnwindSafe + FnOnce(&T) -> U,
    {
        use std::panic::{catch_unwind, AssertUnwindSafe};
        match self {
            OptionLock::Some(_) => {
                self.try_lock_read().map_or(OptionLock::None, |guard| {
                    match catch_unwind(AssertUnwindSafe(|| f(&*guard))) {
                        Ok(result) => OptionLock::maybe(result),
                        Err(_) => {
                            eprintln!("OptionLock.map_catch: closure panicked; recovered");
                            OptionLock::None
                        }
                    }
                })
            }
            OptionLock::Maybe(maybe) => {
                let val = unsafe { maybe.as_ptr().as_ref().unwrap_unchecked() };
                match catch_unwind(AssertUnwindSafe(|| f(val))) {
                    Ok(result) => OptionLock::maybe(result),
                    Err(_) => {
                        eprintln!("OptionLock.map_catch(Maybe): closure panicked; recovered");
                        OptionLock::None
                    }
                }
            }
            OptionLock::None => OptionLock::None,
        }
    }

    /// and_then with panic recovery.
    pub fn and_then_catch<U, F>(&self, f: F) -> OptionLock<U>
    where
        F: std::panic::UnwindSafe + FnOnce(&T) -> OptionLock<U>,
    {
        use std::panic::{catch_unwind, AssertUnwindSafe};
        match self {
            OptionLock::Some(_) => {
                self.try_lock_read().map_or(OptionLock::None, |guard| {
                    match catch_unwind(AssertUnwindSafe(|| f(&*guard))) {
                        Ok(result) => result,
                        Err(_) => {
                            eprintln!("OptionLock.and_then_catch: closure panicked; recovered");
                            OptionLock::None
                        }
                    }
                })
            }
            OptionLock::Maybe(maybe) => {
                let val = unsafe { maybe.as_ptr().as_ref().unwrap_unchecked() };
                match catch_unwind(AssertUnwindSafe(|| f(val))) {
                    Ok(result) => result,
                    Err(_) => {
                        eprintln!("OptionLock.and_then_catch(Maybe): closure panicked; recovered");
                        OptionLock::None
                    }
                }
            }
            OptionLock::None => OptionLock::None,
        }
    }

    /// Executes a closure with a reference to the value if present, otherwise returns the default.
    pub fn try_with_or<F, R>(&self, f: F, default: R) -> R
    where
        F: FnOnce(&T) -> R,
    {
        self.try_with(f).unwrap_or(default)
    }

    /// Executes a closure with a reference to the value if present, otherwise calls the default closure.
    pub fn try_with_or_else<F, G, R>(&self, f: F, g: G) -> R
    where
        F: FnOnce(&T) -> R,
        G: FnOnce() -> R,
    {
        self.try_with(f).unwrap_or_else(g)
    }

    /// Force execution of a closure with the value, panicking if None.
    #[inline]
    pub fn force<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&T) -> R,
    {
        match self {
            OptionLock::Some(_) => {
                let guard = self.read();
                f(&*guard)
            }
            OptionLock::Maybe(maybe) => {
                let value = unsafe { maybe.as_ptr().as_ref().unwrap_unchecked() };
                f(value)
            }
            OptionLock::None => panic!("Cannot force a None value"),
        }
    }

    /// Force execution of a closure with a mutable reference to the value, panicking if None.
    #[inline]
    pub fn force_mut<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut T) -> R,
    {
        match self {
            OptionLock::Some(_) => {
                let mut guard = self.write();
                f(&mut *guard)
            }
            OptionLock::Maybe(maybe) => {
                let value = unsafe { maybe.as_mut_ptr().as_mut().unwrap_unchecked() };
                f(value)
            }
            OptionLock::None => panic!("Cannot force a None value"),
        }
    }

    /// Filters the value based on a predicate, returning None if the predicate fails.
    pub fn filter<P>(&self, predicate: P) -> OptionLock<T>
    where
        P: FnOnce(&T) -> bool,
        T: Clone,
    {
        match self {
            OptionLock::Some(_) => {
                if let Some(guard) = self.try_lock_read() {
                    if predicate(&*guard) {
                        OptionLock::maybe((*guard).clone())
                    } else {
                        OptionLock::None
                    }
                } else {
                    OptionLock::None
                }
            }
            OptionLock::Maybe(maybe) => {
                let value = unsafe { maybe.as_ptr().as_ref().unwrap_unchecked() };
                if predicate(value) {
                    OptionLock::maybe(value.clone())
                } else {
                    OptionLock::None
                }
            }
            OptionLock::None => OptionLock::None,
        }
    }

    /// Inserts a value, returning the previous value.
    pub fn insert(&mut self, value: T) -> OptionLock<T> {
        std::mem::replace(self, OptionLock::maybe(value))
    }

    /// Replaces the value, returning the previous value.
    pub fn replace(&mut self, value: T) -> OptionLock<T> {
        std::mem::replace(self, OptionLock::maybe(value))
    }
    
    /// Takes the value out of the OptionLock, leaving None in its place.
    pub fn take(&mut self) -> OptionLock<T> {
        match std::mem::replace(self, OptionLock::None) {
            OptionLock::Some(ptr) => {
                let raw_ptr = ptr.load(Ordering::Acquire);
                if !raw_ptr.is_null() {
                    let value = unsafe { raw_ptr.read() };
                    WaitNode::wake_waiters();
                    OptionLock::maybe(value)
                } else {
                    OptionLock::None
                }
            }
            other => other,
        }
    }

    /// Chains operations on the contained value.
    pub fn and_then<U, F>(&self, f: F) -> OptionLock<U>
    where
        F: FnOnce(&T) -> OptionLock<U>,
    {
        match self {
            OptionLock::Some(_) => {
                if let Some(guard) = self.try_lock_read() {
                    f(&*guard)
                } else {
                    OptionLock::None
                }
            }
            OptionLock::Maybe(maybe) => {
                let value = unsafe { maybe.as_ptr().as_ref().unwrap_unchecked() };
                f(value)
            }
            OptionLock::None => OptionLock::None,
        }
    }
    
    /// Returns None if self is None, otherwise returns optb.
    pub fn and<U>(&self, optb: &OptionLock<U>) -> OptionLock<U>
    where
        U: Clone,
    {
        if self.is_none() {
            OptionLock::None
        } else {
            match optb {
                OptionLock::Some(_) => {
                    if let Some(guard) = optb.try_lock_read() {
                        OptionLock::maybe((*guard).clone())
                    } else {
                        OptionLock::None
                    }
                }
                OptionLock::Maybe(maybe) => {
                    let value = unsafe { maybe.as_ptr().as_ref().unwrap_unchecked() };
                    OptionLock::maybe(value.clone())
                }
                OptionLock::None => OptionLock::None,
            }
        }
    }

    /// Returns self if it contains a value, otherwise calls f.
    pub fn or_else<F>(&self, f: F) -> OptionLock<T>
    where
        F: FnOnce() -> OptionLock<T>,
        T: Clone,
    {
        match self {
            OptionLock::Some(_) => {
                if let Some(guard) = self.try_lock_read() {
                    OptionLock::maybe((*guard).clone())
                } else {
                    f()
                }
            }
            OptionLock::Maybe(maybe) => {
                let value = unsafe { maybe.as_ptr().as_ref().unwrap_unchecked() };
                OptionLock::maybe(value.clone())
            }
            OptionLock::None => f(),
        }
    }

    /// Returns self if it contains a value, otherwise returns other.
    pub fn or(&self, other: &OptionLock<T>) -> OptionLock<T>
    where
        T: Clone,
    {
        match self {
            OptionLock::Some(_) => {
                if let Some(guard) = self.try_lock_read() {
                    OptionLock::maybe((*guard).clone())
                } else {
                    match other {
                        OptionLock::Some(_) => {
                            if let Some(guard) = other.try_lock_read() {
                                OptionLock::maybe((*guard).clone())
                            } else {
                                OptionLock::None
                            }
                        }
                        OptionLock::Maybe(maybe) => {
                            let value = unsafe { maybe.as_ptr().as_ref().unwrap_unchecked() };
                            OptionLock::maybe(value.clone())
                        }
                        OptionLock::None => OptionLock::None,
                    }
                }
            }
            OptionLock::Maybe(maybe) => {
                let value = unsafe { maybe.as_ptr().as_ref().unwrap_unchecked() };
                OptionLock::maybe(value.clone())
            }
            OptionLock::None => match other {
                OptionLock::Some(_) => {
                    if let Some(guard) = other.try_lock_read() {
                        OptionLock::maybe((*guard).clone())
                    } else {
                        OptionLock::None
                    }
                }
                OptionLock::Maybe(maybe) => {
                    let value = unsafe { maybe.as_ptr().as_ref().unwrap_unchecked() };
                    OptionLock::maybe(value.clone())
                }
                OptionLock::None => OptionLock::None,
            },
        }
    }
    
    /// Returns Some if exactly one of self or other is Some, otherwise returns None.
    pub fn xor(&self, other: &OptionLock<T>) -> OptionLock<T>
    where
        T: Clone,
    {
        let self_some = self.is_some();
        let other_some = other.is_some();
        
        match (self_some, other_some) {
            (true, false) => match self {
                OptionLock::Some(_) => {
                    if let Some(guard) = self.try_lock_read() {
                        OptionLock::maybe((*guard).clone())
                    } else {
                        OptionLock::None
                    }
                }
                OptionLock::Maybe(maybe) => {
                    let value = unsafe { maybe.as_ptr().as_ref().unwrap_unchecked() };
                    OptionLock::maybe(value.clone())
                }
                OptionLock::None => OptionLock::None,
            },
            (false, true) => match other {
                OptionLock::Some(_) => {
                    if let Some(guard) = other.try_lock_read() {
                        OptionLock::maybe((*guard).clone())
                    } else {
                        OptionLock::None
                    }
                }
                OptionLock::Maybe(maybe) => {
                    let value = unsafe { maybe.as_ptr().as_ref().unwrap_unchecked() };
                    OptionLock::maybe(value.clone())
                }
                OptionLock::None => OptionLock::None,
            },
            _ => OptionLock::None,
        }
    }

    /// Zips self with another OptionLock, returning a tuple of both values if both are Some.
    pub fn zip<U>(&self, other: &OptionLock<U>) -> OptionLock<(T, U)>
    where
        T: Clone,
        U: Clone,
    {
        self.zip_with(other, |a, b| (a.clone(), b.clone()))
    }

    /// Zips self with another OptionLock using a combining function.
    pub fn zip_with<U, F, R>(&self, other: &OptionLock<U>, f: F) -> OptionLock<R>
    where
        F: FnOnce(&T, &U) -> R,
    {
        match (self, other) {
            (OptionLock::None, _) | (_, OptionLock::None) => OptionLock::None,
            (OptionLock::Maybe(self_maybe), OptionLock::Maybe(other_maybe)) => {
                let self_val = unsafe { self_maybe.as_ptr().as_ref().unwrap_unchecked() };
                let other_val = unsafe { other_maybe.as_ptr().as_ref().unwrap_unchecked() };
                OptionLock::maybe(f(self_val, other_val))
            }
            (OptionLock::Maybe(self_maybe), OptionLock::Some(_)) => {
                let self_val = unsafe { self_maybe.as_ptr().as_ref().unwrap_unchecked() };
                if let Some(other_guard) = other.try_lock_read() {
                    OptionLock::maybe(f(self_val, &*other_guard))
                } else {
                    OptionLock::None
                }
            }
            (OptionLock::Some(_), OptionLock::Maybe(other_maybe)) => {
                let other_val = unsafe { other_maybe.as_ptr().as_ref().unwrap_unchecked() };
                if let Some(self_guard) = self.try_lock_read() {
                    OptionLock::maybe(f(&*self_guard, other_val))
                } else {
                    OptionLock::None
                }
            }
            (OptionLock::Some(_), OptionLock::Some(_)) => {
                match (self.try_lock_read(), other.try_lock_read()) {
                    (Some(self_guard), Some(other_guard)) => {
                        OptionLock::maybe(f(&*self_guard, &*other_guard))
                    }
                    _ => OptionLock::None,
                }
            }
        }
    }

    /// Unzips an OptionLock containing a tuple into two OptionLocks.
    pub fn unzip<A, B>(self) -> (OptionLock<A>, OptionLock<B>)
    where
        T: Into<(A, B)>,
    {
        match self {
            OptionLock::Maybe(maybe) => {
                let val = unsafe { maybe.assume_init() };
                let (a, b) = val.into();
                (OptionLock::maybe(a), OptionLock::maybe(b))
            }
            OptionLock::Some(ptr) => {
                let raw_ptr = ptr.load(Ordering::Acquire);
                if !raw_ptr.is_null() {
                    let val = unsafe { raw_ptr.read() };
                    let (a, b) = val.into();
                    WaitNode::wake_waiters();
                    (OptionLock::maybe(a), OptionLock::maybe(b))
                } else {
                    (OptionLock::None, OptionLock::None)
                }
            }
            OptionLock::None => (OptionLock::None, OptionLock::None),
        }
    }

    /// Transposes an OptionLock of a Result into a Result of an OptionLock.
    pub fn transpose<U, E>(self) -> Result<OptionLock<U>, E>
    where
        T: Into<Result<U, E>>,
    {
        match self {
            OptionLock::None => Ok(OptionLock::None),
            OptionLock::Maybe(maybe) => {
                unsafe { maybe.assume_init() }.into().map(OptionLock::maybe)
            }
            OptionLock::Some(ptr) => {
                let raw_ptr = ptr.load(Ordering::Acquire);
                if raw_ptr.is_null() {
                    Ok(OptionLock::None)
                } else {
                    let result = unsafe { raw_ptr.read() }.into().map(OptionLock::maybe);
                    WaitNode::wake_waiters();
                    result
                }
            }
        }
    }

    pub fn flatten<U>(self) -> OptionLock<U>
    where
        T: Into<OptionLock<U>>,
    {
        match self {
            OptionLock::Maybe(maybe) => unsafe { maybe.assume_init() }.into(),
            OptionLock::Some(ptr) => {
                let raw_ptr = ptr.load(Ordering::Acquire);
                if raw_ptr.is_null() {
                    OptionLock::None
                } else {
                    let result = unsafe { raw_ptr.read() }.into();
                    WaitNode::wake_waiters();
                    result
                }
            }
            OptionLock::None => OptionLock::None,
        }
    }

    #[inline]
    pub fn get(&self) -> Option<&T> {
        self.try_lock()
    }

    #[inline]
    pub fn get_mut(&mut self) -> Option<&mut T> {
        match self {
            OptionLock::Some(ptr) => {
                let raw_ptr = ptr.load(Ordering::Acquire);
                (!raw_ptr.is_null()).then(|| unsafe { &mut *raw_ptr })
            }
            OptionLock::Maybe(maybe) => unsafe { maybe.as_mut_ptr().as_mut() },
            OptionLock::None => None,
        }
    }

    pub fn get_or_insert(&mut self, value: T) -> &mut T {
        if matches!(self, OptionLock::None) {
            *self = OptionLock::maybe(value);
        }
        match self {
            OptionLock::Maybe(maybe) => unsafe { maybe.as_mut_ptr().as_mut().unwrap_unchecked() },
            OptionLock::Some(_) => {
                let mut guard = self.write();
                unsafe { &mut *(&mut *guard as *mut T) }
            }
            OptionLock::None => unreachable!(),
        }
    }

    pub fn get_or_insert_with<F>(&mut self, f: F) -> &mut T
    where
        F: FnOnce() -> T,
    {
        if matches!(self, OptionLock::None) {
            *self = OptionLock::maybe(f());
        }
        match self {
            OptionLock::Maybe(maybe) => unsafe { maybe.as_mut_ptr().as_mut().unwrap_unchecked() },
            OptionLock::Some(_) => {
                let mut guard = self.write();
                unsafe { &mut *(&mut *guard as *mut T) }
            }
            OptionLock::None => unreachable!(),
        }
    }

    pub fn get_or_insert_default(&mut self) -> &mut T
    where
        T: Default,
    {
        self.get_or_insert_with(T::default)
    }

    fn wait_for_value(&self) {
        WaitNode::wait_for_value();
    }

    /// True if variant is `None`.
    #[inline]
    pub fn is_none(&self) -> bool {
        matches!(self, OptionLock::None)
    }
    
    #[inline]
    pub fn is_none_and<F>(&self, f: F) -> bool
    where
        F: FnOnce() -> bool,
    {
        matches!(self, OptionLock::None) && f()
    }
    
    #[inline]
    pub fn is_none_and_then<F, U>(&self, f: F) -> OptionLock<U>
    where
        F: FnOnce() -> OptionLock<U>,
    {
        if matches!(self, OptionLock::None) {
            f()
        } else {
            OptionLock::None
        }
    }

    #[inline]
    pub fn is_none_or<F>(&self, f: F) -> bool
    where
        F: FnOnce() -> bool,
    {
        matches!(self, OptionLock::None) || f()
    }
    
    #[inline]
    pub fn is_none_or_else<F>(&self, f: F) -> bool
    where
        F: FnOnce() -> bool,
    {
        matches!(self, OptionLock::None) || f()
    }

    /// True if variant is the `Maybe` state.
    #[inline]
    pub fn is_maybe(&self) -> bool {
        matches!(self, OptionLock::Maybe(_))
    }
    
    #[inline]
    pub fn is_maybe_and<F>(&self, f: F) -> bool
    where
        F: FnOnce(&T) -> bool,
    {
        match self {
            OptionLock::Maybe(maybe) => {
                let val = unsafe { maybe.as_ptr().as_ref().unwrap_unchecked() };
                f(val)
            }
            _ => false,
        }
    }
    
    #[inline]
    pub fn is_maybe_and_then<F, U>(&self, f: F) -> OptionLock<U>
    where
        F: FnOnce(&T) -> OptionLock<U>,
    {
        match self {
            OptionLock::Maybe(maybe) => {
                let val = unsafe { maybe.as_ptr().as_ref().unwrap_unchecked() };
                f(val)
            }
            _ => OptionLock::None,
        }
    }
    
    #[inline]
    pub fn is_maybe_or<F>(&self, f: F) -> bool
    where
        F: FnOnce() -> bool,
    {
        matches!(self, OptionLock::Maybe(_)) || f()
    }
    
    #[inline]
    pub fn is_maybe_or_else<F>(&self, f: F) -> bool
    where
        F: FnOnce() -> bool,
    {
        matches!(self, OptionLock::Maybe(_)) || f()
    }

    #[inline]
    pub fn is_some(&self) -> bool {
        match self {
            OptionLock::Some(ptr) => !ptr.load(Ordering::Acquire).is_null(),
            OptionLock::Maybe(_) => true,
            OptionLock::None => false,
        }
    }

    #[inline]
    pub fn is_some_and<F>(&self, f: F) -> bool
    where
        F: FnOnce(&T) -> bool,
    {
        self.try_with(f).unwrap_or(false)
    }
    
    #[inline]
    pub fn is_some_and_then<F, U>(&self, f: F) -> OptionLock<U>
    where
        F: FnOnce(&T) -> OptionLock<U>,
    {
        self.try_with(f).unwrap_or(OptionLock::None)
    }
    
    #[inline]
    pub fn is_some_or<F>(&self, f: F) -> bool
    where
        F: FnOnce() -> bool,
    {
        self.is_some() || f()
    }
    
    #[inline]
    pub fn is_some_or_else<F>(&self, f: F) -> bool
    where
        F: FnOnce() -> bool,
    {
        self.is_some() || f()
    }

    /// Unwraps the value, consuming the OptionLock and returning the contained value.
    /// Panics if the value is None.
    pub fn unwrap(self) -> T {
        match self {
            OptionLock::Some(ptr) => {
                let raw_ptr = ptr.load(Ordering::Acquire);
                if raw_ptr.is_null() {
                    panic!("Cannot unwrap a None value");
                }
                unsafe { raw_ptr.read() }
            }
            OptionLock::Maybe(maybe) => unsafe { maybe.assume_init() },
            OptionLock::None => panic!("Cannot unwrap a None value"),
        }
    }

    /// Unwraps the value or returns the provided default.
    #[inline]
    pub fn unwrap_or(self, default: T) -> T {
        match self {
            OptionLock::Some(ptr) => {
                let raw_ptr = ptr.load(Ordering::Acquire);
                if raw_ptr.is_null() {
                    default
                } else {
                    unsafe { raw_ptr.read() }
                }
            }
            OptionLock::Maybe(maybe) => unsafe { maybe.assume_init() },
            OptionLock::None => default,
        }
    }

    /// Unwraps the value or returns the default value of type T.
    #[inline]
    pub fn unwrap_or_default(self) -> T
    where
        T: Default,
    {
        match self {
            OptionLock::Some(ptr) => {
                let raw_ptr = ptr.load(Ordering::Acquire);
                if raw_ptr.is_null() {
                    T::default()
                } else {
                    unsafe { raw_ptr.read() }
                }
            }
            OptionLock::Maybe(maybe) => unsafe { maybe.assume_init() },
            OptionLock::None => T::default(),
        }
    }

    /// Unwraps the value or computes it from a closure.
    #[inline]
    pub fn unwrap_or_else<F>(self, f: F) -> T
    where
        F: FnOnce() -> T,
    {
        match self {
            OptionLock::Some(ptr) => {
                let raw_ptr = ptr.load(Ordering::Acquire);
                if raw_ptr.is_null() {
                    f()
                } else {
                    unsafe { raw_ptr.read() }
                }
            }
            OptionLock::Maybe(maybe) => unsafe { maybe.assume_init() },
            OptionLock::None => f(),
        }
    }
    
    /// Unwraps the value, panicking with a custom message if None.
    #[inline]
    pub fn expect(self, msg: &str) -> T {
        match self {
            OptionLock::Some(ptr) => {
                let raw_ptr = ptr.load(Ordering::Acquire);
                if raw_ptr.is_null() {
                    panic!("{}", msg);
                }
                unsafe { raw_ptr.read() }
            }
            OptionLock::Maybe(maybe) => unsafe { maybe.assume_init() },
            OptionLock::None => panic!("{}", msg),
        }
    }
}

impl<T> Clone for OptionLock<T>
where
    T: Clone,
{
    fn clone(&self) -> Self {
        match self {
            OptionLock::Some(_) => {
                if let Some(guard) = self.try_lock_read() {
                    OptionLock::maybe((*guard).clone())
                } else {
                    OptionLock::None
                }
            }
            OptionLock::Maybe(maybe) => {
                let value = unsafe { maybe.as_ptr().as_ref().unwrap() };
                OptionLock::maybe(value.clone())
            }
            OptionLock::None => OptionLock::None,
        }
    }
}

impl<T> Default for OptionLock<T> {
    fn default() -> Self {
        OptionLock::None
    }
}

impl<T: PartialEq> PartialEq for OptionLock<T> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (OptionLock::Some(_), OptionLock::Some(_)) => {
                if let (Some(guard1), Some(guard2)) = (self.try_lock_read(), other.try_lock_read()) {
                    *guard1 == *guard2
                } else {
                    false
                }
            }
            (OptionLock::Maybe(maybe1), OptionLock::Maybe(maybe2)) => {
                let value1 = unsafe { maybe1.as_ptr().as_ref().unwrap() };
                let value2 = unsafe { maybe2.as_ptr().as_ref().unwrap() };
                value1 == value2
            }
            (OptionLock::None, OptionLock::None) => true,
            _ => false,
        }
    }
}
impl<T: PartialEq> Eq for OptionLock<T> {}

impl<T> From<T> for OptionLock<T> {
    fn from(value: T) -> Self {
        OptionLock::some(value)
    }
}

impl<T> From<Option<T>> for OptionLock<T> {
    fn from(value: Option<T>) -> Self {
        match value {
            Some(value) => OptionLock::some(value),
            None => OptionLock::None,
        }
    }
}

impl<T> From<&T> for OptionLock<T>
where
    T: Clone,
{
    fn from(value: &T) -> Self {
        OptionLock::some(value.clone())
    }
}

impl<T> From<&mut T> for OptionLock<T>
where
    T: Clone,
{
    fn from(value: &mut T) -> Self {
        OptionLock::some(value.clone())
    }
}

impl<T> From<&Option<T>> for OptionLock<T>
where
    T: Clone,
{
    fn from(value: &Option<T>) -> Self {
        match value {
            Some(value) => OptionLock::some(value.clone()),
            None => OptionLock::None,
        }
    }
}

impl<T> From<&mut Option<T>> for OptionLock<T>
where
    T: Clone,
{
    fn from(value: &mut Option<T>) -> Self {
        match value {
            Some(value) => OptionLock::some(value.clone()),
            None => OptionLock::None,
        }
    }
}

impl<T> Into<Option<T>> for OptionLock<T> {
    fn into(self) -> Option<T> {
        match self {
            OptionLock::Some(ptr) => {
                let raw_ptr = ptr.load(Ordering::Acquire);
                if !raw_ptr.is_null() {
                    Some(unsafe { raw_ptr.read() })
                } else {
                    None
                }
            }
            OptionLock::Maybe(maybe) => Some(unsafe { maybe.assume_init() }),
            OptionLock::None => None,
        }
    }
}

impl<T> std::hash::Hash for OptionLock<T>
where
    T: std::hash::Hash,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            OptionLock::Some(_) => {
                if let Some(guard) = self.try_lock_read() {
                    (*guard).hash(state);
                }
            }
            OptionLock::Maybe(maybe) => {
                let value = unsafe { maybe.as_ptr().as_ref().unwrap() };
                value.hash(state);
            }
            OptionLock::None => 0usize.hash(state),
        }
    }
}
impl<T> AsRef<T> for OptionLock<T> {
    fn as_ref(&self) -> &T {
        match self {
            OptionLock::Some(_) => {
                let guard = self.read();
                unsafe { &*(guard.as_ref() as *const T) }
            }
            OptionLock::Maybe(maybe) => unsafe { maybe.as_ptr().as_ref().unwrap_unchecked() },
            OptionLock::None => panic!("Cannot get reference to None value"),
        }
    }
}
impl<T> AsMut<T> for OptionLock<T> {
    fn as_mut(&mut self) -> &mut T {
        match self {
            OptionLock::Some(_) => {
                let mut guard = self.write();
                unsafe { &mut *(&mut *guard as *mut T) }
            }
            OptionLock::Maybe(maybe) => unsafe { maybe.as_mut_ptr().as_mut().unwrap_unchecked() },
            OptionLock::None => panic!("Cannot get mutable reference to None value"),
        }
    }
}

impl<T> From<OptionLock<T>> for MaybeUninit<T> {
    fn from(value: OptionLock<T>) -> Self {
        match value {
            OptionLock::Some(ptr) => {
                let raw_ptr = ptr.load(Ordering::Acquire);
                if !raw_ptr.is_null() {
                    MaybeUninit::new(unsafe { raw_ptr.read() })
                } else {
                    MaybeUninit::uninit()
                }
            }
            OptionLock::Maybe(maybe) => MaybeUninit::new(unsafe { maybe.assume_init() }),
            OptionLock::None => MaybeUninit::uninit(),
        }
    }
}

impl<T> From<MaybeUninit<T>> for OptionLock<T> {
    fn from(value: MaybeUninit<T>) -> Self {
        OptionLock::Maybe(value)
    }
}

impl<T> From<Box<T>> for OptionLock<T> {
    fn from(value: Box<T>) -> Self {
        OptionLock::Some(AtomicPtr::new(Box::into_raw(value)))
    }
}

impl<T> TryFrom<OptionLock<T>> for Arc<T> {
    type Error = OptionLock<T>;
    
    fn try_from(value: OptionLock<T>) -> Result<Arc<T>, Self::Error> {
        match value {
            OptionLock::Some(ptr) => {
                let raw_ptr = ptr.load(Ordering::Acquire);
                if !raw_ptr.is_null() {
                    let val = unsafe { raw_ptr.read() };
                    Ok(Arc::new(val))
                } else {
                    Err(OptionLock::None)
                }
            }
            OptionLock::Maybe(maybe) => {
                let val = unsafe { maybe.assume_init() };
                Ok(Arc::new(val))
            }
            OptionLock::None => Err(OptionLock::None),
        }
    }
}
impl<T> TryFrom<OptionLock<T>> for *mut T {
    type Error = OptionLock<T>;
    
    fn try_from(value: OptionLock<T>) -> Result<*mut T, Self::Error> {
        match value {
            OptionLock::Some(ptr) => {
                let raw_ptr = unsafe { *ptr.as_ptr() };
                Ok(raw_ptr)
            }
            OptionLock::Maybe(maybe) => Ok(maybe.as_ptr() as *mut T),
            OptionLock::None => Err(OptionLock::None),
        }
    }
}
    
unsafe impl<T: Send> Send for OptionLock<T> {}
unsafe impl<T: Send + Sync> Sync for OptionLock<T> {}

#[derive(Debug)]
#[repr(transparent)]
pub struct Lock<T: ?Sized> {
    value: T,
}

impl<T> Lock<T> {
    #[inline(always)]
    pub const fn new(value: T) -> Lock<T> {
        Lock { value }
    }

    #[inline(always)]
    pub fn into_inner(self) -> T {
        self.value
    }

    #[inline]
    pub const unsafe fn replace(&self, value: T) -> T {
        unsafe { core::ptr::replace(self.get(), value) }
    }
}
impl<T: ?Sized> Lock<T> {
    #[inline(always)]
    pub const fn from_mut(value: &mut T) -> &mut Lock<T> {
        unsafe { &mut *(value as *mut T as *mut Lock<T>) }
    }

    #[inline(always)]
    pub const fn get(&self) -> *mut T {
        self as *const Lock<T> as *const T as *mut T
    }

    #[inline(always)]
    pub const fn get_mut(&mut self) -> &mut T {
        &mut self.value
    }

    #[inline(always)]
    pub const fn raw_get(this: *const Self) -> *mut T {
        this as *const T as *mut T
    }

    #[inline]
    pub const unsafe fn as_ref_unchecked<'a>(&self) -> &'a T {
        unsafe { &*self.get() }
    }

    #[inline]
    pub const unsafe fn as_mut_unchecked<'a>(&self) -> &'a mut T {
        unsafe { &mut *self.get() }
    }
}

impl<T: Default> Default for Lock<T> {
    fn default() -> Lock<T> {
        Lock::new(Default::default())
    }
}

impl<T> From<T> for Lock<T> {
    fn from(t: T) -> Lock<T> {
        Lock::new(t)
    }
}

impl<T: Clone> Clone for Lock<T> {
    fn clone(&self) -> Self {
        Lock::new(self.value.clone())
    }
}

impl<T: PartialEq> PartialEq for Lock<T> {
    fn eq(&self, other: &Self) -> bool {
        self.value.eq(&other.value)
    }
}

impl<T: Eq> Eq for Lock<T> {}

impl<T: PartialOrd> PartialOrd for Lock<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.value.partial_cmp(&other.value)
    }
}

impl<T: Ord> Ord for Lock<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.value.cmp(&other.value)
    }
}

impl<T: std::hash::Hash> std::hash::Hash for Lock<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.value.hash(state)
    }
}
impl<T> AsRef<T> for Lock<T> {
    fn as_ref(&self) -> &T {
        &self.value
    }
}
impl<T> AsMut<T> for Lock<T> {
    fn as_mut(&mut self) -> &mut T {
        &mut self.value
    }
}
unsafe impl<T: Send> Send for Lock<T> {}
unsafe impl<T: Send + Sync> Sync for Lock<T> {}

#[derive(Debug)]
#[repr(transparent)]
pub struct SyncLock<T: ?Sized> {
    value: Lock<T>,
}

impl<T> SyncLock<T> {
    #[inline]
    pub const fn new(value: T) -> Self {
        Self {
            value: Lock { value },
        }
    }
    
    #[inline]
    pub fn into_inner(self) -> T {
        self.value.into_inner()
    }
}

impl<T: ?Sized> SyncLock<T> {
    #[inline]
    pub const fn get(&self) -> *mut T {
        self.value.get()
    }

    #[inline]
    pub const fn get_mut(&mut self) -> &mut T {
        self.value.get_mut()
    }

    #[inline]
    pub const fn raw_get(this: *const Self) -> *mut T {
        this as *const T as *mut T
    }
}

impl<T: Default> Default for SyncLock<T> {
    fn default() -> SyncLock<T> {
        SyncLock::new(Default::default())
    }
}

impl<T> From<T> for SyncLock<T> {
    fn from(t: T) -> SyncLock<T> {
        SyncLock::new(t)
    }
}

impl<T: Clone> Clone for SyncLock<T> {
    fn clone(&self) -> Self {
        SyncLock::new(self.value.value.clone())
    }
}

impl<T: PartialEq> PartialEq for SyncLock<T> {
    fn eq(&self, other: &Self) -> bool {
        self.value.value.eq(&other.value.value)
    }
}

impl<T: Eq> Eq for SyncLock<T> {}

impl<T: PartialOrd> PartialOrd for SyncLock<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.value.value.partial_cmp(&other.value.value)
    }
}

impl<T: Ord> Ord for SyncLock<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.value.value.cmp(&other.value.value)
    }
}

impl<T: std::hash::Hash> std::hash::Hash for SyncLock<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.value.value.hash(state)
    }
}
impl<T> AsRef<T> for SyncLock<T> {
    fn as_ref(&self) -> &T {
        &self.value.value
    }
}
impl<T> AsMut<T> for SyncLock<T> {
    fn as_mut(&mut self) -> &mut T {
        &mut self.value.value
    }
}

unsafe impl<T: Send> Send for SyncLock<T> {}
unsafe impl<T: Send + Sync> Sync for SyncLock<T> {}

pub struct WriteGuard<T> {
    inner: *mut T,
    counter: Arc<AtomicUsize>,
}

impl<T> WriteGuard<T> {
    #[inline]
    pub fn new_const(ptr: *const T, counter: Arc<AtomicUsize>) -> Self {
        // Counter already set to 0x80000000 by try_lock_write
        WriteGuard { inner: ptr as *mut T, counter }
    }
    
    pub fn wait_for_readers(&self) {
        let counter = &self.counter;
        let mut backoff = 1u32;
        loop {
            let current = counter.load(Ordering::Acquire);
            if current & 0x7FFFFFFF == 0 {
                break;
            }
            if backoff <= 64 {
                for _ in 0..backoff {
                    std::hint::spin_loop();
                }
                backoff = backoff.saturating_mul(2);
            } else {
                std::thread::yield_now();
            }
        }
    }
    
    pub fn try_wait_for_readers(&self, timeout: std::time::Duration) -> bool {
        let counter = &self.counter;
        let start = std::time::Instant::now();
        let mut backoff = 1u32;
        
        loop {
            let current = counter.load(Ordering::Acquire);
            if current & 0x7FFFFFFF == 0 {
                return true;
            }
            if start.elapsed() >= timeout {
                return false;
            }
            if backoff <= 64 {
                for _ in 0..backoff {
                    std::hint::spin_loop();
                }
                backoff = backoff.saturating_mul(2);
            } else {
                std::thread::yield_now();
            }
        }
    }
    
    #[inline]
    pub fn reader_count(&self) -> usize {
        self.counter.load(Ordering::Acquire) & 0x7FFFFFFF
    }
    
    pub fn downgrade(self) -> ReadGuard<T> {
        let ptr = self.inner;
        let counter = self.counter.clone();
        std::mem::forget(self);
        counter.store(1, Ordering::Release);
        ReadGuard::new_with_counter(ptr, counter)
    }
    
    #[inline]
    fn ptr(&self) -> *const T {
        self.inner
    }
}

impl<T> Deref for WriteGuard<T> {
    type Target = T;
    
    #[inline]
    fn deref(&self) -> &Self::Target {
        unsafe { &*self.inner }
    }
}

impl<T> DerefMut for WriteGuard<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.inner }
    }
}

impl<T> Drop for WriteGuard<T> {
    fn drop(&mut self) {
        // Recovery-aware: handle reentrant unlocking properly
        if std::thread::panicking() {
            eprintln!("WriteGuard::drop: recovering from panic; releasing writer lock");
        }
        
        let key = self.inner as usize;
        let current_thread = thread::current().id();
        
        let should_release = {
            let mut reentrant_map = reentrant_locks().lock().unwrap();
            if let Some((owner_thread, recursion_count)) = reentrant_map.get_mut(&key) {
                if *owner_thread == current_thread {
                    *recursion_count -= 1;
                    if *recursion_count == 0 {
                        reentrant_map.remove(&key);
                        true // Release the actual lock
                    } else {
                        false // Still nested, don't release
                    }
                } else {
                    true // Shouldn't happen, but release anyway
                }
            } else {
                true // No reentrant info, release
            }
        };
        
        if should_release {
            self.counter.store(0, Ordering::Release);
            WaitNode::wake_waiters();
            // Yield to allow waiting threads to proceed
            std::thread::yield_now();
        }
    }
}

impl<T: PartialEq> PartialEq for WriteGuard<T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        **self == **other
    }
}

impl<T: Eq> Eq for WriteGuard<T> {}

impl<T: std::hash::Hash> std::hash::Hash for WriteGuard<T> {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        (**self).hash(state);
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for WriteGuard<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("WriteGuard")
            .field("value", &**self)
            .field("reader_count", &self.reader_count())
            .finish()
    }
}

impl<T> AsRef<T> for WriteGuard<T> {
    #[inline]
    fn as_ref(&self) -> &T {
        self
    }
}

impl<T> AsMut<T> for WriteGuard<T> {
    #[inline]
    fn as_mut(&mut self) -> &mut T {
        self
    }
}

unsafe impl<T: Send> Send for WriteGuard<T> {}
unsafe impl<T: Sync> Sync for WriteGuard<T> {}

pub struct ReadGuard<T> {
    ptr: *const T,
    counter: Option<Arc<AtomicUsize>>,
}

impl<T> ReadGuard<T> {
    #[inline]
    pub const fn new(ptr: *const T) -> Self {
        ReadGuard { ptr, counter: None }
    }
    
    #[inline]
    pub fn new_with_counter(ptr: *const T, counter: Arc<AtomicUsize>) -> Self {
        // Counter already incremented by try_lock_read, don't double-increment
        ReadGuard { ptr, counter: Some(counter) }
    }
    
    #[inline]
    pub const fn ptr(&self) -> *const T {
        self.ptr
    }
    
    pub fn upgrade(self) -> Result<WriteGuard<T>, Self> {
        if let Some(counter) = &self.counter {
            let current = counter.load(Ordering::Acquire);
            // Can only upgrade if we're the only reader (count == 1)
            if current == 1 && counter.compare_exchange(1, 0x80000000, Ordering::AcqRel, Ordering::Relaxed).is_ok() {
                let ptr = self.ptr as *mut T;
                let counter = counter.clone();
                std::mem::forget(self);
                return Ok(WriteGuard { inner: ptr, counter });
            }
        }
        Err(self)
    }
    
    #[inline]
    pub fn reader_count(&self) -> Option<usize> {
        self.counter.as_ref().map(|c| c.load(Ordering::Acquire) & 0x7FFFFFFF)
    }
}

impl<T> Deref for ReadGuard<T> {
    type Target = T;
    
    #[inline]
    fn deref(&self) -> &Self::Target {
        unsafe { &*self.ptr }
    }
}

impl<T> Drop for ReadGuard<T> {
    fn drop(&mut self) {
        if let Some(counter) = &self.counter {
            // Recovery-aware: decrement reader count and wake waiters if needed
            let old = counter.fetch_sub(1, Ordering::AcqRel);
            let was_last_reader = (old & 0x7FFFFFFF) == 1;
            let writer_waiting = (old & 0x80000000) != 0;
            
            if std::thread::panicking() {
                eprintln!("ReadGuard::drop: recovering from panic; readers left before drop: {}", (old & 0x7FFFFFFF));
            }
            
            if was_last_reader && writer_waiting {
                WaitNode::wake_waiters();
                std::thread::yield_now();
            }
        }
    }
}

impl<T> Clone for ReadGuard<T> {
    #[inline]
    fn clone(&self) -> Self {
        if let Some(counter) = &self.counter {
            counter.fetch_add(1, Ordering::AcqRel);
        }
        ReadGuard {
            ptr: self.ptr,
            counter: self.counter.clone(),
        }
    }
}

impl<T: PartialEq> PartialEq for ReadGuard<T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        **self == **other
    }
}

impl<T: Eq> Eq for ReadGuard<T> {}

impl<T: std::hash::Hash> std::hash::Hash for ReadGuard<T> {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        (**self).hash(state);
    }
}

impl<T> AsRef<T> for ReadGuard<T> {
    #[inline]
    fn as_ref(&self) -> &T {
        self
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for ReadGuard<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ReadGuard")
            .field("value", &**self)
            .field("reader_count", &self.reader_count())
            .finish()
    }
}

unsafe impl<T: Send> Send for ReadGuard<T> {}
unsafe impl<T: Sync> Sync for ReadGuard<T> {}
