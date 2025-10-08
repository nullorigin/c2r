use std::sync::atomic::AtomicU32;

use crate::maybe::Maybe;

pub const FUTEX_WAIT: i32 = 0;
pub const FUTEX_WAKE: i32 = 1;
pub const FUTEX_WAIT_BITSET: i32 = 9;
pub const FUTEX_PRIVATE_FLAG: i32 = 128;
pub const SYS_futex: i32 = 202;
pub const CLOCK_MONOTONIC: i32 = 1;
pub const ETIMEDOUT: i32 = 110;
pub const EINTR: i32 = 4;

#[derive(Debug, Clone,Copy,PartialEq,Eq,PartialOrd,Ord,Hash)]
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
    pub fn futex_wait(&self, expected: u32, timeout: Option<core::time::Duration>) -> bool {
        use core::sync::atomic::Ordering::Relaxed;
        let mut timespec = None;
        // Calculate the timeout as an absolute timespec.
        if let Some(duration) = timeout {
            timespec = Some(Timespec::now(CLOCK_MONOTONIC).checked_add_duration(&duration).expect("overflow"));
        }
        loop {
            // No need to wait if the value already changed.
            if self.inner.load(Relaxed) != expected {
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
        self.inner.load(core::sync::atomic::Ordering::Relaxed)
    }

    pub fn set(&self, value: u32) {
        self.inner.store(value, core::sync::atomic::Ordering::Relaxed);
    }
}
impl Clone for Futex {
    fn clone(&self) -> Self {
        Self {
            inner: core::sync::atomic::AtomicU32::new(self.inner.load(core::sync::atomic::Ordering::Relaxed)),
        }
    }
}
impl Drop for Futex {
    fn drop(&mut self) {
        self.inner.store(0, core::sync::atomic::Ordering::Relaxed);
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
    pub unsafe fn syscall(num: i32, ...) -> i32;
}

unsafe extern "C" {
    unsafe fn __errno_location() -> *mut i32;
}

