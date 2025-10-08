use core::any::type_name;
use core::ops::{Deref, DerefMut};
use core::{fmt};
use std::hash::Hasher;
use std::sync::{Once};

pub mod futex;
pub use futex::Futex;
pub mod once;
pub use once::MaybeOnce;
pub mod lock;
pub use lock::MaybeLock;

pub enum Maybe<T> {
    None,
    Some(T),
}
impl<T> Maybe<T> {
    #[inline(always)]
    pub const fn new(val: T) -> Maybe<T> {
        Maybe::Some(val)
    }

    #[inline(always)]
    pub const fn uninit() -> Maybe<T> {
        Maybe::None
    }

    #[inline]
    pub const fn zeroed() -> Maybe<T>
    where
        T: Copy,
    {
        let mut u = Maybe::<T>::uninit();
        unsafe { 
            core::ptr::write_bytes(u.as_mut_ptr(), 0, 1);
        }
        u
    }

    #[inline(always)]
    pub fn write(&mut self, val: T) -> &mut T {
        *self = Maybe::Some(val);
        match self {
            Maybe::Some(v) => v,
            Maybe::None => unreachable!(),
        }
    }

    #[inline]
    pub fn read(&self) -> T
    where
        T: Copy,
    {
        match self {
            Maybe::Some(val) => *val,
            Maybe::None => panic!("Called `read` on a `Maybe::None` value"),
        }
    }

    #[inline(always)]
    pub const fn as_ptr(&self) -> *const T {
        self as *const Self as *const T
    }

    #[inline(always)]
    pub const fn as_mut_ptr(&mut self) -> *mut T {
        self as *mut Self as *mut T
    }

    #[inline]
    pub fn as_bytes(&self) -> &[u8] {
        match self {
            Maybe::Some(val) => unsafe { 
                core::slice::from_raw_parts(
                    val as *const T as *const u8,
                    core::mem::size_of::<T>()
                )
            },
            Maybe::None => &[],
        }
    }

    #[inline]
    pub fn as_bytes_mut(&mut self) -> &mut [u8] {
        match self {
            Maybe::Some(val) => unsafe {
                core::slice::from_raw_parts_mut(
                    val as *mut T as *mut u8,
                    core::mem::size_of::<T>()
                )
            },
            Maybe::None => &mut [],
        }
    }

    #[inline(always)]
    pub fn assume_init(self) -> T {
        match self {
            Maybe::Some(val) => val,
            Maybe::None => panic!("Called `assume_init` on a `Maybe::None` value"),
        }
    }

    #[inline]
    pub fn assume_init_read(&self) -> T 
    where 
        T: Copy,
    {
        match self {
            Maybe::Some(val) => *val,
            Maybe::None => panic!("Called `assume_init_read` on a `Maybe::None` value"),
        }
    }

    pub fn assume_init_drop(&mut self) {
        if let Maybe::Some(_) = self {
            unsafe { 
                core::ptr::drop_in_place(self.as_mut_ptr());
            }
            *self = Maybe::None;
        }
    }

    #[inline(always)]
    pub const fn assume_init_ref(&self) -> &T {
        match self {
            Maybe::Some(val) => val,
            Maybe::None => panic!("Called `assume_init_ref` on a `Maybe::None` value"),
        }
    }

    #[inline(always)]
    pub const fn assume_init_mut(&mut self) -> &mut T {
        match self {
            Maybe::Some(val) => val,
            Maybe::None => panic!("Called `assume_init_mut` on a `Maybe::None` value"),
        }
    }

    #[inline(always)]
    pub const fn is_some(&self) -> bool {
        matches!(self, Maybe::Some(_))
    }

    #[inline(always)]
    pub const fn is_none(&self) -> bool {
        matches!(self, Maybe::None)
    }

    #[inline]
    pub fn expect(self, msg: &str) -> T {
        match self {
            Maybe::Some(val) => val,
            Maybe::None => panic!("{}", msg),
        }
    }

    pub fn expect_none(self, msg: &str) {
        match self {
            Maybe::Some(_) => panic!("{}", msg),
            Maybe::None => (),
        }
    }

    #[inline(always)]
    pub fn unwrap(self) -> T {
        self.expect("Called `unwrap` on a `Maybe::None` value")
    }

    pub fn unwrap_or(self, default: T) -> T {
        match self {
            Maybe::Some(val) => val,
            Maybe::None => default,
        }
    }

    pub fn unwrap_or_else<F>(self, f: F) -> T 
    where
        F: FnOnce() -> T,
    {
        match self {
            Maybe::Some(val) => val,
            Maybe::None => f(),
        }
    }

    pub fn map<U, F>(self, f: F) -> Maybe<U>
    where
        F: FnOnce(T) -> U,
    {
        match self {
            Maybe::Some(val) => Maybe::Some(f(val)),
            Maybe::None => Maybe::None,
        }
    }

    pub fn map_or<U, F>(self, default: U, f: F) -> U
    where
        F: FnOnce(T) -> U,
    {
        match self {
            Maybe::Some(val) => f(val),
            Maybe::None => default,
        }
    }

    pub fn map_or_else<U, D, F>(self, default: D, f: F) -> U
    where
        D: FnOnce() -> U,
        F: FnOnce(T) -> U,
    {
        match self {
            Maybe::Some(val) => f(val),
            Maybe::None => default(),
        }
    }

    pub fn and_then<F>(self, f: F) -> Maybe<T>
    where
        F: FnOnce(T) -> Maybe<T>,
    {
        match self {
            Maybe::Some(val) => f(val),
            Maybe::None => Maybe::None,
        }
    }

    pub fn filter<P>(self, predicate: P) -> Maybe<T>
    where
        P: FnOnce(&T) -> bool,
    {
        match self {
            Maybe::Some(val) if predicate(&val) => Maybe::Some(val),
            _ => Maybe::None,
        }
    }

    pub fn or_else<F>(self, f: F) -> Maybe<T>
    where
        F: FnOnce() -> Maybe<T>,
    {
        match self {
            Maybe::Some(val) => Maybe::Some(val),
            Maybe::None => f(),
        }
    }

    #[inline(always)]
    pub const fn get(&self) -> &T {
        match self {
            Maybe::Some(val) => val,
            Maybe::None => panic!("Called `get` on a `Maybe::None` value"),
        }
    }

    #[inline(always)]
    pub const fn get_mut(&mut self) -> &mut T {
        match self {
            Maybe::Some(val) => val,
            Maybe::None => panic!("Called `get_mut` on a `Maybe::None` value"),
        }
    }

    #[inline]
    pub fn take(&mut self) -> T {
        match core::mem::replace(self, Maybe::None) {
            Maybe::Some(val) => val,
            Maybe::None => panic!("Called `take` on a `Maybe::None` value"),
        }
    }

    #[inline]
    pub fn replace(&mut self, val: T) -> Maybe<T> {
        core::mem::replace(self, Maybe::Some(val))
    }

    #[inline(always)]
    pub const fn from_mut(val: &mut T) -> &mut Maybe<T> {
        unsafe { &mut *(val as *mut T as *mut Maybe<T>) }
    }

    #[inline(always)]
    pub const fn raw_get(this: *const Self) -> *mut T {
        this as *mut T
    }

    pub fn drop_in_place(val: &mut Maybe<T>) {
        if let Maybe::Some(_) = val {
            unsafe { 
                core::ptr::drop_in_place(val.as_mut_ptr());
            }
            *val = Maybe::None;
        }
    }
}

// Only implement Copy when T is Copy to prevent double-drop issues
impl<T: Copy> Copy for Maybe<T> {}

unsafe impl<T: Send> Send for Maybe<T> {}
unsafe impl<T: Sync> Sync for Maybe<T> {}

impl<T> Deref for Maybe<T> {
    type Target = T;
    #[inline(always)]
    fn deref(&self) -> &T {
        match self {
            Maybe::None => panic!("Deref on Maybe::None"),
            Maybe::Some(val) => &val,
        }
    }
}
impl<T> DerefMut for Maybe<T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut T {
        match self {
            Maybe::None => panic!("DerefMut on Maybe::None"),
            Maybe::Some(val) => val,
        }
    }
}
impl<T> AsRef<T> for Maybe<T> {
    #[inline]
    fn as_ref(&self) -> &T {
        match self {
            Maybe::None => panic!("as_ref on Maybe::None"),
            Maybe::Some(val) => val,
        }
    }
}
impl<T> AsMut<T> for Maybe<T> {
    #[inline]
    fn as_mut(&mut self) -> &mut T {
        match self {
            Maybe::None => panic!("as_mut on Maybe::None"),
            Maybe::Some(val) => val,
        }
    }
}
impl<T: Clone> Clone for Maybe<T> {
    #[inline]
    fn clone(&self) -> Self {
        match self {
            Maybe::None => Maybe::None,
            Maybe::Some(val) => Maybe::Some(val.clone()),
        }
    }
}
impl<T:PartialEq> PartialEq for Maybe<T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Maybe::Some(a), Maybe::Some(b)) => a == b,
            (Maybe::None, Maybe::None) => true,
            _ => false,
        }
    }
}
impl<T:Eq> Eq for Maybe<T> {}
impl<T:PartialOrd> PartialOrd for Maybe<T> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        match (self, other) {
            (Maybe::Some(a), Maybe::Some(b)) => a.partial_cmp(b),
            (Maybe::None, Maybe::None) => Some(core::cmp::Ordering::Equal),
            _ => None,
        }
    }
}
impl<T:Ord> Ord for Maybe<T> {
    #[inline]
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        match (self, other) {
            (Maybe::Some(a), Maybe::Some(b)) => a.cmp(b),
            (Maybe::None, Maybe::None) => core::cmp::Ordering::Equal,
            (Maybe::None, Maybe::Some(_)) => core::cmp::Ordering::Less,
            (Maybe::Some(_), Maybe::None) => core::cmp::Ordering::Greater,
        }
    }
}
impl<T:core::hash::Hash> core::hash::Hash for Maybe<T> {
    #[inline]
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        match self {
            Maybe::Some(val) => val.hash(state),
            Maybe::None => state.write_u8(0),
        }
    }
}
impl<T: core::fmt::Debug> core::fmt::Debug for Maybe<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Maybe::Some(val) => f.debug_tuple("Some").field(val).finish(),
            Maybe::None => f.write_str("None"),
        }
    }
}
impl<T, const N: usize> Maybe<[T; N]> {
    #[inline]
    pub const fn transpose(&self) -> &[Maybe<T>; N] {
        unsafe { core::mem::transmute(self) }
    }
    #[inline]
    pub const fn transpose_mut(&mut self) -> &mut [Maybe<T>; N] {
        unsafe { core::mem::transmute(self) }
    }
    #[inline]
    pub fn to_array(&self) -> [T; N] where T: Default + Copy {
        match self {
            Maybe::Some(arr) => *arr,
            Maybe::None => [T::default(); N]    
        }
    }
}
impl<T> From<T> for Maybe<T> {
    #[inline]
    fn from(value: T) -> Self {
        Maybe::Some(value)
    }
}
impl<T, const N: usize> FromIterator<T> for Maybe<[T;N]> {
    #[inline]
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let arr = iter.into_iter().collect::<Maybe<[T; N]>>();  
        arr
    }
}
impl<T, const N: usize> From<Vec<T>> for Maybe<[T; N]> {
    fn from(value: Vec<T>) -> Self {
        let arr = value.into_iter().collect::<Maybe<[T; N]>>();
        arr
    }
}
struct Guard<'a, T> {
    slice: &'a mut [Maybe<T>],
    initialized: usize,
}

impl<T> Drop for Guard<'_, T> {
    fn drop(&mut self) {
        // Safely drop each initialized element individually
        for i in 0..self.initialized {
            self.slice[i].assume_init_drop();
        }
    }
}
trait SpecFill<T> {
    fn spec_fill(&mut self, value: T);
}

impl<T: Clone> SpecFill<T> for [Maybe<T>] {
    fn spec_fill(&mut self, value: T) {
        let mut guard = Guard {
            slice: self,
            initialized: 0,
        };

        if let Some((last, elems)) = guard.slice.split_last_mut() {
            for el in elems {
                el.write(value.clone());
                guard.initialized += 1;
            }

            last.write(value);
            guard.initialized += 1;
        }
        core::mem::forget(guard);
    }
}
impl<T> Default for Maybe<T> {
    #[inline]
    fn default() -> Self {
        Maybe::None
    }
}

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct DropLock<T: ?Sized> {
    value: T,
}

impl<T> DropLock<T> {
    #[inline(always)]
    pub const fn new(value: T) -> DropLock<T> {
        DropLock { value }
    }

    #[inline(always)]
    pub const fn into_inner(slot: &DropLock<T>) -> T
    where
        T: Copy,
    {
        slot.value
    }

    #[inline]
    pub unsafe fn take(slot: &mut DropLock<T>) -> T {
        unsafe { core::ptr::read(&slot.value) }
    }

    #[inline]
    pub const unsafe fn replace(&self, value: T) -> T {
        unsafe { core::ptr::replace(self.get(), value) }
    }
}
impl<T: ?Sized> DropLock<T> {
    #[inline(always)]
    pub const fn from_mut(value: &mut T) -> &mut DropLock<T> {
        unsafe { &mut *(value as *mut T as *mut DropLock<T>) }
    }

    #[inline(always)]
    pub const fn get(&self) -> *mut T {
        self as *const DropLock<T> as *const T as *mut T
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
    #[inline]
    pub unsafe fn drop(slot: &mut DropLock<T>) {
        // Only drop if the value is still valid
        // This prevents double-drops in concurrent scenarios
        unsafe { core::ptr::drop_in_place(&mut slot.value) }
    }
}

impl<T: ?Sized> Deref for DropLock<T> {
    type Target = T;
    #[inline(always)]
    fn deref(&self) -> &T {
        &self.value
    }
}

impl<T: ?Sized> DerefMut for DropLock<T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut T {
        &mut self.value
    }
}