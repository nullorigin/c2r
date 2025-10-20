use std::sync::atomic::{AtomicU32, Ordering};

use crate::maybe::Maybe;

pub const FUTEX_WAIT: i32 = 0;
pub const FUTEX_WAKE: i32 = 1;
pub const FUTEX_WAIT_BITSET: i32 = 9;
pub const FUTEX_PRIVATE_FLAG: i32 = 128;
pub const SYS_futex: i32 = 202;
pub const CLOCK_MONOTONIC: i32 = 1;
pub const ETIMEDOUT: i32 = 110;
pub const EINTR: i32 = 4;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(C)]
pub struct Timespec {
    pub tv_sec: i64,
    pub tv_nsec: i64,
}
#[derive(Debug)]
pub struct Futex {
    pub inner: AtomicU32,
}

impl Futex {
    pub const fn new() -> Self {
        Self {
            inner: AtomicU32::new(0),
        }
    }
    pub fn store(&self, value: u32, ordering: Ordering) {
        self.inner.store(value, ordering);
    }
    pub fn load(&self, ordering: Ordering) -> u32 {
        self.inner.load(ordering)
    }
    pub fn lock(&self) {
        // Try to acquire the lock with compare_exchange
        if self
            .inner
            .compare_exchange_weak(0, 1, Ordering::Acquire, Ordering::Relaxed)
            .is_ok()
        {
            return;
        }

        // If that fails, fall back to the slow path
        self.lock_slow();
    }

    fn lock_slow(&self) {
        loop {
            // Wait for the lock to be released
            while self.inner.load(Ordering::Relaxed) != 0 {
                self.futex_wait(1, None);
            }

            // Try to acquire the lock again
            if self
                .inner
                .compare_exchange_weak(0, 1, Ordering::Acquire, Ordering::Relaxed)
                .is_ok()
            {
                return;
            }
        }
    }

    pub fn unlock(&self) {
        self.inner.store(0, Ordering::Release);
        self.futex_wake();
    }

    pub fn try_lock(&self) -> bool {
        self.inner
            .compare_exchange_weak(0, 1, Ordering::Acquire, Ordering::Relaxed)
            .is_ok()
    }

    pub fn futex_wait(&self, expected: u32, timeout: Option<std::time::Duration>) -> bool {
        let mut timespec = None;
        // Calculate the timeout as an absolute timespec.
        if let Some(duration) = timeout {
            timespec = Some(
                Timespec::now(CLOCK_MONOTONIC)
                    .checked_add_duration(&duration)
                    .expect("overflow"),
            );
        }
        loop {
            // No need to wait if the value already changed.
            if self.inner.load(Ordering::Relaxed) != expected {
                return true;
            }

            let timespec_ptr = match &timespec {
                Some(ts) => ts as *const Timespec,
                None => core::ptr::null(),
            };

            let r = unsafe {
                syscall(
                    SYS_futex,
                    self.inner.as_ptr(),
                    FUTEX_WAIT_BITSET | FUTEX_PRIVATE_FLAG,
                    expected,
                    timespec_ptr,
                    core::ptr::null::<u32>(),
                    !0u32,
                )
            };

            if r < 0 {
                let errno = unsafe { *__errno_location() };
                match errno {
                    ETIMEDOUT => return false,
                    EINTR => continue,
                    _ => return true,
                }
            } else {
                return true;
            }
        }
    }

    pub fn futex_wake(&self) -> bool {
        let ptr = self.inner.as_ptr();
        let op = FUTEX_WAKE | FUTEX_PRIVATE_FLAG;
        unsafe { syscall(SYS_futex, ptr, op, 1) > 0 }
    }

    pub fn futex_wake_all(&self) {
        let ptr = self.inner.as_ptr();
        let op = FUTEX_WAKE | FUTEX_PRIVATE_FLAG;
        unsafe {
            syscall(SYS_futex, ptr, op, i32::MAX);
        }
    }

    pub fn get(&self) -> u32 {
        self.inner.load(Ordering::Relaxed)
    }

    pub fn set(&self, value: u32) {
        self.inner.store(value, Ordering::Relaxed);
    }
}
impl Clone for Futex {
    fn clone(&self) -> Self {
        Self {
            inner: AtomicU32::new(self.inner.load(Ordering::Relaxed)),
        }
    }
}
impl PartialEq for Futex {
    fn eq(&self, other: &Self) -> bool {
        self.inner.load(Ordering::Relaxed) == other.inner.load(Ordering::Relaxed)
    }
}
impl Eq for Futex {}
impl PartialOrd for Futex {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for Futex {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        self.inner
            .load(Ordering::Relaxed)
            .cmp(&other.inner.load(Ordering::Relaxed))
    }
}
impl Default for Futex {
    fn default() -> Self {
        Self {
            inner: AtomicU32::new(0),
        }
    }
}
impl Drop for Futex {
    fn drop(&mut self) {
        self.inner.store(0, Ordering::Relaxed);
    }
}
impl Timespec {
    pub fn now(clock_id: i32) -> Self {
        let mut ts = Timespec {
            tv_sec: 0,
            tv_nsec: 0,
        };
        unsafe {
            syscall(228, clock_id, &mut ts); // SYS_clock_gettime
        }
        ts
    }

    pub fn checked_add_duration(&self, duration: &core::time::Duration) -> Option<Timespec> {
        let secs = duration.as_secs() as i64;
        let nanos = duration.subsec_nanos() as i64;

        let new_sec = self.tv_sec.checked_add(secs).unwrap_or_default();
        let new_nsec = self.tv_nsec + nanos;

        if new_nsec >= 1_000_000_000 {
            Some(Timespec {
                tv_sec: new_sec.checked_add(1).unwrap_or_default(),
                tv_nsec: new_nsec - 1_000_000_000,
            })
        } else {
            Some(Timespec {
                tv_sec: new_sec,
                tv_nsec: new_nsec,
            })
        }
    }

    pub fn to_timespec(&self) -> Maybe<Timespec> {
        Maybe::Some(self.clone())
    }
}

unsafe extern "C" {
    pub fn syscall(num: i32, ...) -> i32;
}

unsafe extern "C" {
    fn __errno_location() -> *mut i32;
}
