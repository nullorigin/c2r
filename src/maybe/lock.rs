use crate::{maybe::Maybe, Futex};

pub struct MaybeLock<T> {
    futex: Futex,
    data: Maybe<T>,
}
const INCOMPLETE: u32 = 0;
const RUNNING: u32 = 1;
const COMPLETE: u32 = 2;
const PANICKED: u32 = 3;

impl<T> MaybeLock<T> {
    #[inline(always)]
    pub const fn new() -> Self {
        Self {
            futex: Futex::new(),
            data: Maybe::None,
        }
    }

    #[inline]
    pub fn is_some(&self) -> bool {
        self.futex.get() == COMPLETE && self.data.is_some()
    }

    #[inline]
    pub fn is_none(&self) -> bool {
        self.futex.get() != COMPLETE || self.data.is_none()
    }

    #[inline]
    pub fn is_uninit(&self) -> bool {
        self.futex.get() != COMPLETE
    }

    #[inline]
    pub fn some(value: T) -> Self {
        let lock = Self {
            futex: Futex::new(),
            data: Maybe::Some(value),
        };
        lock.futex.set(COMPLETE);
        lock
    }

    #[inline]
    pub fn none() -> Self {
        Self::new()
    }

    #[inline]
    pub fn preinit(value: T) -> Self {
        Self::some(value)
    }

    #[inline]
    pub fn is_initialized(&self) -> bool {
        self.futex.get() == COMPLETE
    }

    #[inline]
    pub fn try_get(&self) -> Maybe<&T> {
        if self.futex.get() == COMPLETE {
            Maybe::Some(self.data.assume_init_ref())
        } else {
            Maybe::None
        }
    }

    #[inline]
    pub fn try_get_mut(&mut self) -> Maybe<&mut T> {
        if self.futex.get() == COMPLETE {
            Maybe::Some(self.data.assume_init_mut())
        } else {
            Maybe::None
        }
    }

    #[inline]
    pub fn get_or_init<F>(&self, f: F) -> &T
    where
        F: FnOnce() -> T,
    {
        // Prevent double initialization
        if self.futex.get() == COMPLETE {
            return self.data.get();
        }

        self.call_once(|| {
            let ptr = self as *const Self as *mut Self;
            unsafe {
                (*ptr).data = Maybe::Some(f());
            }
        });
        self.data.get()
    }

    #[inline]
    pub fn get_or_try_init<F, E>(&self, f: F) -> Result<&T, E>
    where
        F: FnOnce() -> Result<T, E>,
    {
        use core::cell::Cell;
        thread_local! {
            static ERROR: Cell<Option<*const ()>> = Cell::new(None);
        }

        let mut error: Option<E> = None;
        let error_ref = &mut error as *mut Option<E> as *const ();

        ERROR.with(|e| e.set(Some(error_ref)));

        self.call_once(|| match f() {
            Ok(value) => {
                let ptr = self as *const Self as *mut Self;
                unsafe {
                    (*ptr).data = Maybe::Some(value);
                }
            }
            Err(err) => {
                ERROR.with(|e| {
                    if let Some(ptr) = e.get() {
                        unsafe {
                            let error_ptr = ptr as *mut Option<E>;
                            *error_ptr = Some(err);
                        }
                    }
                });
            }
        });

        match error {
            Some(e) => Err(e),
            None => Ok(self.data.get()),
        }
    }

    #[inline]
    pub fn force(&self) -> &T {
        self.get_or_init(|| panic!("MaybeLock is not initialized"))
    }

    #[inline]
    pub fn force_mut(&mut self) -> &mut T {
        self.call_once_mut(|| {});
        self.data.get_mut()
    }

    fn call_once<F>(&self, f: F)
    where
        F: FnOnce(),
    {
        if self.futex.get() == COMPLETE {
            return;
        }

        use core::sync::atomic::{
            AtomicU32,
            Ordering::{Acquire, Relaxed},
        };

        let futex_ptr = &self.futex.inner as *const AtomicU32 as *mut AtomicU32;
        let futex_atomic = unsafe { &*futex_ptr };

        match futex_atomic.compare_exchange_weak(INCOMPLETE, RUNNING, Acquire, Relaxed) {
            Ok(_) => {
                f();
                self.futex.set(COMPLETE);
                self.futex.futex_wake_all();
            }
            Err(COMPLETE) => return,
            Err(PANICKED) => panic!("MaybeLock instance has previously panicked"),
            Err(RUNNING) => {
                while self.futex.get() == RUNNING {
                    self.futex
                        .futex_wait(RUNNING, Some(core::time::Duration::from_millis(100)));
                }
                match self.futex.get() {
                    COMPLETE => return,
                    PANICKED => panic!("MaybeLock instance has previously panicked"),
                    _ => unreachable!(),
                }
            }
            Err(_) => unreachable!(),
        }
    }

    fn call_once_mut<F>(&mut self, f: F)
    where
        F: FnOnce(),
    {
        if self.futex.get() == COMPLETE {
            return;
        }

        self.futex.set(RUNNING);
        f();
        self.futex.set(COMPLETE);
        self.futex.futex_wake_all();
    }

    #[inline]
    pub fn read(&self) -> MaybeLockReadGuard<'_, T> {
        // Ensure initialization with poison recovery
        self.force();
        MaybeLockReadGuard { lock: self }
    }

    #[inline]
    pub fn try_read(&self) -> Maybe<MaybeLockReadGuard<'_, T>> {
        if self.is_initialized() {
            Maybe::Some(MaybeLockReadGuard { lock: self })
        } else {
            Maybe::None
        }
    }

    #[inline]
    pub fn write(&'_ mut self) -> MaybeLockWriteGuard<'_, T> {
        self.force_mut();
        MaybeLockWriteGuard { lock: self }
    }

    #[inline]
    pub fn try_write(&'_ mut self) -> Maybe<MaybeLockWriteGuard<'_, T>> {
        if self.is_initialized() {
            Maybe::Some(MaybeLockWriteGuard { lock: self })
        } else {
            Maybe::None
        }
    }

    #[inline]
    pub fn take(&mut self) -> Maybe<T> {
        if self.futex.get() == COMPLETE {
            let value = core::mem::replace(&mut self.data, Maybe::None);
            self.futex.set(INCOMPLETE);
            value
        } else {
            Maybe::None
        }
    }

    #[inline]
    pub fn replace(&mut self, value: T) -> T {
        self.call_once_mut(|| {});
        core::mem::replace(self.data.get_mut(), value)
    }

    #[inline]
    pub fn swap(&mut self, other: &mut Self) {
        if self.futex.get() == COMPLETE && other.futex.get() == COMPLETE {
            core::mem::swap(self.data.get_mut(), other.data.get_mut());
        } else {
            core::mem::swap(self, other);
        }
    }
}

impl<T> Drop for MaybeLock<T> {
    #[inline]
    fn drop(&mut self) {
        if self.futex.get() == COMPLETE {
            // Take ownership and let Rust handle the drop naturally
            let _ = core::mem::replace(&mut self.data, Maybe::None);
            self.futex.set(INCOMPLETE); // Mark as no longer initialized
        }
        // The replaced data will be dropped by Rust automatically
    }
}

pub struct MaybeLockReadGuard<'a, T> {
    lock: &'a MaybeLock<T>,
}

impl<'a, T> core::ops::Deref for MaybeLockReadGuard<'a, T> {
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &T {
        self.lock.data.get()
    }
}

impl<'a, T> AsRef<T> for MaybeLockReadGuard<'a, T> {
    #[inline(always)]
    fn as_ref(&self) -> &T {
        self.lock.data.get()
    }
}

pub struct MaybeLockWriteGuard<'a, T> {
    lock: &'a mut MaybeLock<T>,
}

impl<'a, T> core::ops::Deref for MaybeLockWriteGuard<'a, T> {
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &T {
        self.lock.data.get()
    }
}

impl<'a, T> core::ops::DerefMut for MaybeLockWriteGuard<'a, T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut T {
        self.lock.data.get_mut()
    }
}

impl<T> AsMut<T> for MaybeLockWriteGuard<'_, T> {
    #[inline(always)]
    fn as_mut(&mut self) -> &mut T {
        self.lock.data.get_mut()
    }
}

impl<T> AsRef<T> for MaybeLockWriteGuard<'_, T> {
    #[inline(always)]
    fn as_ref(&self) -> &T {
        self.lock.data.get()
    }
}

impl<T> core::ops::Deref for MaybeLock<T> {
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &T {
        self.force()
    }
}

impl<T> core::ops::DerefMut for MaybeLock<T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut T {
        self.force_mut()
    }
}

unsafe impl<T: Send> Send for MaybeLock<T> {}
unsafe impl<T: Sync> Sync for MaybeLock<T> {}

impl<T: Default> Default for MaybeLock<T> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl<T> From<T> for MaybeLock<T> {
    #[inline]
    fn from(value: T) -> Self {
        Self::preinit(value)
    }
}

impl<T: core::fmt::Debug> core::fmt::Debug for MaybeLock<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self.try_get() {
            Maybe::Some(value) => f
                .debug_struct("MaybeLock")
                .field("initialized", &true)
                .field("value", value)
                .finish(),
            Maybe::None => f
                .debug_struct("MaybeLock")
                .field("initialized", &false)
                .finish_non_exhaustive(),
        }
    }
}

impl<T: Clone> Clone for MaybeLock<T> {
    #[inline]
    fn clone(&self) -> Self {
        match self.try_get() {
            Maybe::Some(value) => Self::preinit(value.clone()),
            Maybe::None => Self::new(),
        }
    }
}

impl<T: PartialEq> PartialEq for MaybeLock<T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        match (self.try_get(), other.try_get()) {
            (Maybe::Some(a), Maybe::Some(b)) => a == b,
            (Maybe::None, Maybe::None) => true,
            _ => false,
        }
    }
}

impl<T: PartialEq> Eq for MaybeLock<T> {}

impl<T: PartialOrd> PartialOrd for MaybeLock<T> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        match (self.try_get(), other.try_get()) {
            (Maybe::Some(a), Maybe::Some(b)) => a.partial_cmp(b),
            (Maybe::None, Maybe::None) => Some(core::cmp::Ordering::Equal),
            (Maybe::None, Maybe::Some(_)) => Some(core::cmp::Ordering::Less),
            (Maybe::Some(_), Maybe::None) => Some(core::cmp::Ordering::Greater),
        }
    }
}

impl<T: Ord> Ord for MaybeLock<T> {
    #[inline]
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        match (self.try_get(), other.try_get()) {
            (Maybe::Some(a), Maybe::Some(b)) => a.cmp(b),
            (Maybe::None, Maybe::None) => core::cmp::Ordering::Equal,
            (Maybe::None, Maybe::Some(_)) => core::cmp::Ordering::Less,
            (Maybe::Some(_), Maybe::None) => core::cmp::Ordering::Greater,
        }
    }
}

impl<T: core::hash::Hash> core::hash::Hash for MaybeLock<T> {
    #[inline]
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        match self.try_get() {
            Maybe::Some(value) => {
                true.hash(state);
                value.hash(state);
            }
            Maybe::None => {
                false.hash(state);
            }
        }
    }
}

impl<T> AsMut<T> for MaybeLock<T> {
    #[inline]
    fn as_mut(&mut self) -> &mut T {
        self.force_mut()
    }
}

impl<T> AsRef<T> for MaybeLock<T> {
    #[inline]
    fn as_ref(&self) -> &T {
        self.force()
    }
}
