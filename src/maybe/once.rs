use std::panic::{RefUnwindSafe, UnwindSafe};
use std::ptr::null;
use std::sync::atomic::AtomicU32;

use crate::maybe::Maybe;
use std::sync::atomic::Ordering::{Acquire, Relaxed};

unsafe impl<T: Send> Send for MaybeOnce<T> {}
unsafe impl<T: Sync + Send> Sync for MaybeOnce<T> {}
impl<T> UnwindSafe for MaybeOnce<T> {}
impl<T> RefUnwindSafe for MaybeOnce<T> {}

use crate::maybe::futex::Futex;
const INCOMPLETE: u32 = 0;
const RUNNING: u32 = 1;
const COMPLETE: u32 = 2;
const PANICKED: u32 = 3;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum State {
    Incomplete,
    Running,
    Complete,
    Panicked,
}

#[derive(Debug)]
pub struct MaybeOnce<T> {
    state: Futex,
    data: Maybe<T>,
}

impl<T> MaybeOnce<T> {
    pub const fn new() -> Self {
        Self {
            state: Futex::new(),
            data: Maybe::None,
        }
    }

    pub fn call_once<F>(&self, f: F) -> &T
    where
        F: FnOnce() -> T,
    {
        if self.state.get() == COMPLETE {
            return self.data.get();
        }

        // Use compare_exchange to atomically transition from INCOMPLETE to RUNNING
        match self
            .state
            .inner
            .compare_exchange_weak(INCOMPLETE, RUNNING, Acquire, Relaxed)
        {
            Ok(_) => {
                let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(f));
                match result {
                    Ok(value) => {
                        unsafe {
                            let ptr = &self.data as *const Maybe<T> as *mut Maybe<T>;
                            ptr.write(Maybe::Some(value));
                        }
                        self.state.set(COMPLETE);
                        self.state.futex_wake_all();
                        self.data.get()
                    }
                    Err(err) => {
                        self.state.set(PANICKED);
                        self.state.futex_wake_all();
                        std::panic::resume_unwind(err);
                    }
                }
            }
            Err(COMPLETE) => self.data.get(),
            Err(PANICKED) => panic!("Once instance has previously panicked"),
            Err(RUNNING) => {
                // Wait for the running thread to complete using futex
                while self.state.get() == RUNNING {
                    self.state
                        .futex_wait(RUNNING, Some(core::time::Duration::from_millis(100)));
                }
                match self.state.get() {
                    COMPLETE => self.data.get(),
                    PANICKED => panic!("Once instance has previously panicked"),
                    _ => unreachable!(),
                }
            }
            Err(_) => unreachable!(),
        }
    }

    pub fn call_once_force<F>(&self, f: F) -> &T
    where
        F: FnOnce() -> T,
    {
        if self.state.get() == COMPLETE {
            return self.data.get();
        }

        // Force execution even if previously panicked
        let current_state = self.state.get();
        if current_state == PANICKED {
            self.state.set(INCOMPLETE);
        }

        self.call_once(f)
    }

    pub fn try_call_once<F, E>(&self, f: F) -> Result<&T, E>
    where
        F: FnOnce() -> Result<T, E>,
    {
        if self.state.get() == COMPLETE {
            return Ok(self.data.get());
        }

        match self
            .state
            .inner
            .compare_exchange_weak(INCOMPLETE, RUNNING, Acquire, Relaxed)
        {
            Ok(_) => {
                let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(f));
                match result {
                    Ok(Ok(value)) => {
                        unsafe {
                            let ptr = &self.data as *const Maybe<T> as *mut Maybe<T>;
                            ptr.write(Maybe::Some(value));
                        }
                        self.state.set(COMPLETE);
                        self.state.futex_wake_all();
                        Ok(self.data.get())
                    }
                    Ok(Err(err)) => {
                        self.state.set(INCOMPLETE);
                        self.state.futex_wake_all();
                        Err(err)
                    }
                    Err(panic_err) => {
                        self.state.set(PANICKED);
                        self.state.futex_wake_all();
                        std::panic::resume_unwind(panic_err);
                    }
                }
            }
            Err(COMPLETE) => Ok(self.data.get()),
            Err(PANICKED) => panic!("Once instance has previously panicked"),
            Err(RUNNING) => {
                while self.state.get() == RUNNING {
                    self.state
                        .futex_wait(RUNNING, Some(core::time::Duration::from_millis(100)));
                }
                match self.state.get() {
                    COMPLETE => Ok(self.data.get()),
                    PANICKED => panic!("Once instance has previously panicked"),
                    INCOMPLETE => panic!("Initialization failed"),
                    _ => unreachable!(),
                }
            }
            Err(_) => unreachable!(),
        }
    }

    pub fn try_get(&self) -> Maybe<&T> {
        if self.state.get() == COMPLETE {
            Maybe::Some(self.data.as_ref())
        } else {
            Maybe::None
        }
    }

    pub fn get(&self) -> Option<&T> {
        if self.state.get() == COMPLETE {
            Some(self.data.get())
        } else {
            None
        }
    }

    pub fn is_completed(&self) -> bool {
        self.state.get() == COMPLETE
    }

    pub fn is_panicked(&self) -> bool {
        self.state.get() == PANICKED
    }

    pub fn is_running(&self) -> bool {
        self.state.get() == RUNNING
    }

    pub fn state(&self) -> State {
        match self.state.get() {
            INCOMPLETE => State::Incomplete,
            RUNNING => State::Running,
            COMPLETE => State::Complete,
            PANICKED => State::Panicked,
            _ => unreachable!("invalid MaybeOnce state"),
        }
    }

    pub fn wait(&self) {
        while !self.is_completed() && !self.is_panicked() {
            self.state.futex_wait(
                self.state.get(),
                Some(core::time::Duration::from_millis(100)),
            );
        }
    }

    pub fn wait_timeout(&self, timeout: core::time::Duration) -> bool {
        let start = std::time::Instant::now();
        while !self.is_completed() && !self.is_panicked() {
            if start.elapsed() >= timeout {
                return false;
            }
            let remaining = timeout.saturating_sub(start.elapsed());
            self.state.futex_wait(
                self.state.get(),
                Some(remaining.min(core::time::Duration::from_millis(100))),
            );
        }
        true
    }

    pub fn reset(&mut self) {
        if self.state.get() != INCOMPLETE {
            self.state.set(INCOMPLETE);
            self.data = Maybe::None;
        }
    }
}

impl<T: Clone> Clone for MaybeOnce<T> {
    fn clone(&self) -> Self {
        match self.try_get() {
            Maybe::Some(value) => {
                let mut new_once = MaybeOnce::new();
                new_once.data = Maybe::Some(value.clone());
                new_once.state.set(COMPLETE);
                new_once
            }
            Maybe::None => MaybeOnce::new(),
        }
    }
}

impl<T: PartialEq> PartialEq for MaybeOnce<T> {
    fn eq(&self, other: &Self) -> bool {
        match (self.try_get(), other.try_get()) {
            (Maybe::Some(a), Maybe::Some(b)) => a == b,
            (Maybe::None, Maybe::None) => self.state() == other.state(),
            _ => false,
        }
    }
}

impl<T: Eq> Eq for MaybeOnce<T> {}

impl<T: std::hash::Hash> std::hash::Hash for MaybeOnce<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self.try_get() {
            Maybe::Some(value) => {
                true.hash(state);
                value.hash(state);
            }
            Maybe::None => {
                false.hash(state);
                self.state().hash(state);
            }
        }
    }
}

impl<T> Drop for MaybeOnce<T> {
    fn drop(&mut self) {
        // Wake any waiting threads before dropping
        if self.state.get() == RUNNING {
            self.state.set(PANICKED);
            self.state.futex_wake_all();
        }
        // Let Maybe<T> handle its own drop
    }
}
