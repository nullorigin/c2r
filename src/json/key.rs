use core::marker::{Send, Sync};

use crate::json::Value;

const KEY_BUF_LEN: usize = 32;
const NULL: Value = Value::Null;

// FNV-1a implementation
//
// While the `Object` is implemented as a binary tree, not a hash table, the
// order in which the tree is balanced makes absolutely no difference as long
// as there is a deterministic left / right ordering with good spread.
// Comparing a hashed `u64` is faster than comparing `&str` or even `&[u8]`,
// for larger objects this yields non-trivial performance benefits.
//
// Additionally this "randomizes" the keys a bit. Should the keys in an object
// be inserted in alphabetical order (an example of such a use case would be
// using an object as a store for entries by ids, where ids are sorted), this
// will prevent the tree from being constructed in a way where the same branch
// of each node is always used, effectively producing linear lookup times. Bad!
//
// Example:
//
// ```
// println!("{}", hash_key(b"10000056"));
// println!("{}", hash_key(b"10000057"));
// println!("{}", hash_key(b"10000058"));
// println!("{}", hash_key(b"10000059"));
// ```
//
// Produces:
//
// ```
// 15043794053238616431  <-- 2nd
// 15043792953726988220  <-- 1st
// 15043800650308385697  <-- 4th
// 15043799550796757486  <-- 3rd
// ```
#[inline]
pub fn hash_key(key: &[u8]) -> u64 {
    let mut hash: u64 = 0xcbf29ce484222325;
    for byte in key {
        hash ^= *byte as u64;
        hash = hash.wrapping_mul(0x100000001b3);
    }
    hash
}

pub struct Key {
    // Internal buffer to store keys that fit within `KEY_BUF_LEN`,
    // otherwise this field will contain garbage.
    pub buf: [u8; KEY_BUF_LEN],

    // Length of the key in bytes.
    pub len: usize,

    // Cached raw pointer to the key, so that we can cheaply construct
    // a `&str` slice from the `Node` without checking if the key is
    // allocated separately on the heap, or in the `key_buf`.
    pub ptr: *mut u8,

    // A hash of the key, explanation below.
    pub hash: u64,
}

impl Key {
    #[inline]
    pub fn new(hash: u64, len: usize) -> Self {
        Key {
            buf: [0; KEY_BUF_LEN],
            len: len,
            ptr: std::ptr::null_mut(),
            hash: hash,
        }
    }

    #[inline]
    pub fn as_bytes(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.ptr, self.len) }
    }

    #[inline]
    pub fn as_str(&self) -> &str {
        unsafe { std::str::from_utf8_unchecked(self.as_bytes()) }
    }

    // The `buf` on the `Key` can only be filled after the struct
    // is already on the `Vec`'s heap (along with the `Node`).
    // For that reason it's not set in `Key::new` but only after
    // the `Node` is created and allocated.
    #[inline]
    pub fn attach(&mut self, key: &[u8]) {
        if self.len <= KEY_BUF_LEN {
            unsafe {
                std::ptr::copy_nonoverlapping(key.as_ptr(), self.buf.as_mut_ptr(), self.len);
            }
            self.ptr = self.buf.as_mut_ptr();
        } else {
            let mut heap = key.to_vec();
            self.ptr = heap.as_mut_ptr();
            std::mem::forget(heap);
        }
    }

    // Since we store `Node`s on a vector, it will suffer from reallocation.
    // Whenever that happens, `key.ptr` for short keys will turn into dangling
    // pointers and will need to be re-cached.
    #[inline]
    pub fn fix_ptr(&mut self) {
        if self.len <= KEY_BUF_LEN {
            self.ptr = self.buf.as_mut_ptr();
        }
    }
}

// Implement `Sync` and `Send` for `Key` despite the use of raw pointers. The struct
// itself should be memory safe.
unsafe impl Sync for Key {}
unsafe impl Send for Key {}

// Because long keys _can_ be stored separately from the `Key` on heap,
// it's essential to clean up the heap allocation when the `Key` is dropped.
impl Drop for Key {
    fn drop(&mut self) {
        unsafe {
            if self.len > KEY_BUF_LEN {
                // Construct a `Vec` out of the `key_ptr`. Since the key is
                // always allocated from a slice, the capacity is equal to length.
                let heap = Vec::from_raw_parts(self.ptr, self.len, self.len);

                // Now that we have an owned `Vec<u8>`, drop it.
                std::mem::drop(heap);
            }
        }
    }
}

// Just like with `Drop`, `Clone` needs a custom implementation that accounts
// for the fact that key _can_ be separately heap allocated.
impl Clone for Key {
    fn clone(&self) -> Self {
        if self.len > KEY_BUF_LEN {
            let mut heap = self.as_bytes().to_vec();
            let ptr = heap.as_mut_ptr();
            std::mem::forget(heap);

            Key {
                buf: [0; KEY_BUF_LEN],
                len: self.len,
                ptr: ptr,
                hash: self.hash,
            }
        } else {
            Key {
                buf: self.buf,
                len: self.len,
                ptr: std::ptr::null_mut(), // requires a `fix_ptr` call after `Node` is on the heap
                hash: self.hash,
            }
        }
    }
}