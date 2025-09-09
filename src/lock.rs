#![allow(
    unused_variables,
    unused_assignments,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals
)]

use crate::{Entry, Pending};
use std::borrow::ToOwned;
use std::clone::Clone;
use std::collections::HashMap;
use std::fmt::Display;
use std::hash::{Hash, Hasher};
use std::sync::LazyLock;
use std::time::{SystemTime, UNIX_EPOCH};

pub const TIME_START: LazyLock<SystemTime> = LazyLock::new(|| SystemTime::now());
pub fn time() -> u128 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_nanos()
}
pub fn elapsed() -> u128 {
    SystemTime::now()
        .duration_since(TIME_START.to_owned())
        .unwrap()
        .as_nanos()
}
pub fn gen_name(basename: &str) -> String {
    let time = elapsed();
    let mut name = basename.to_string();
    name.push('-');
    name.push_str(&time.to_string());
    name.clone()
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Id {
    num: u128,
    pub name: String,
    priority: u64,
    timestamp: u128,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Key {
    pub public: u32,
    private: u128,
    name: String,
}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Lock {
    id: Id,
    key: Key,
}
impl Lock {
    pub fn new(key: Key) -> Self {
        Lock {
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
impl Default for Lock {
    fn default() -> Self {
        Lock {
            id: Id::default(),
            key: Key::default(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct Locker {
    id: Id,
    key: Key,
    lock: Lock,
    queue: Vec<Pending>,
    contents: Vec<Entry>,
    timestamp: u128,
}
impl Hash for Locker {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        self.key.hash(state);
        self.lock.hash(state);
        self.queue.hash(state);
        self.contents.hash(state);
        self.timestamp.hash(state);
    }
}
impl Id {
    pub const fn new(num: u128, name: String) -> Id {
        Id {
            num,
            name,
            priority: 100,
            timestamp: 0,
        }
    }
    pub const fn name(&self) -> &String {
        &self.name
    }
    pub fn num(&self) -> u128 {
        self.num
    }
    #[allow(static_mut_refs)]
    pub fn get(name: &str) -> Id {
        if let Some(id) = unsafe { MAPPED_IDS.get(name) } {
            return id.clone();
        }
        // Generate new random ID and cache it for future lookups
        let new_id = Id::random(name.to_string());
        unsafe { MAPPED_IDS.insert(name.to_string(), new_id.clone()) };
        new_id
    }
    #[allow(static_mut_refs)]
    pub fn set(id: Id) -> Id {
        if let Some(prev_id) = unsafe { MAPPED_IDS.insert(id.name.clone(), id.clone()) } {
            prev_id.clone()
        } else {
            id.clone()
        }
    }
    pub fn increment(&mut self) -> u128 {
        if self.num < u128::MAX - 1 {
            self.num += 1;
        }
        self.update_timestamp();
        self.num.clone()
    }
    pub fn decrement(&mut self) -> u128 {
        if self.num > 1 {
            self.num -= 1;
        }
        self.update_timestamp();
        self.num.clone()
    }
    pub fn priority(&self) -> u64 {
        self.priority.clone()
    }
    pub fn set_priority(&mut self, priority: u64) {
        self.priority = priority.clamp(0, 200);
    }
    pub fn clear(&mut self) {
        self.num = 0;
        self.name.clear();
        self.priority = 100;
        self.timestamp = 0;
    }
    pub fn is_empty(&self) -> bool {
        self.num == 0 && self.name.is_empty()
    }
    pub fn is_invalid(&self) -> bool {
        self.num == u128::MAX || self.num == 0 || self.name.is_empty()
    }
    pub fn timestamp(&self) -> u128 {
        self.timestamp.clone()
    }
    pub fn update_timestamp(&mut self) -> u128 {
        self.timestamp = time();
        self.timestamp.clone()
    }
    pub fn random(name: String) -> Id {
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        hasher.write_u128(time());
        let split_name = name.split_at(name.len() / 2);
        split_name.1.hash(&mut hasher);
        split_name.0.hash(&mut hasher);
        let hi = hasher.finish();
        hasher.write_u128(time());
        split_name.0.hash(&mut hasher);
        split_name.1.hash(&mut hasher);
        let lo = hasher.finish();
        let num = (hi as u128) << 64 | (lo as u128);
        if num == 0 || num == u128::MAX {
            Self::random(name).clone()
        } else {
            Id {
                num,
                name: name,
                priority: 100,
                timestamp: time(),
            }
        }
    }
    pub fn encrypted(key: &Key) -> Id {
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        hasher.write_u128(key.private);
        let split_name = key.name.split_at(key.name.len() / 2);
        split_name.1.hash(&mut hasher);
        split_name.0.hash(&mut hasher);
        let hi = hasher.finish();
        hasher.write_u128(u128::MAX - key.private);
        split_name.0.hash(&mut hasher);
        split_name.1.hash(&mut hasher);
        let lo = hasher.finish();
        let num = (hi as u128) << 64 | (lo as u128);
        if num == 0 || num == u128::MAX {
            Self::encrypted(key)
        } else {
            Id {
                num,
                name: key.name.clone(),
                priority: 100,
                timestamp: time(),
            }
        }
    }
    pub fn decrypt(&mut self, key: &Key) -> bool {
        let expected_id = Self::encrypted(key);
        if expected_id.num == self.num {
            self.name = key.name.clone();
            self.update_timestamp();
            true
        } else {
            false
        }
    }
    pub fn gen_name(basename: &str) -> String {
        let time = time();
        let mut name = basename.to_string();
        name.push('-');
        name.push_str(&time.to_string());
        name.clone()
    }
}
impl Default for Id {
    fn default() -> Self {
        Id::get("default")
    }
}
impl Display for Id {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(fmt, "{}", self.name())
    }
}

impl std::fmt::Debug for Id {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Clean debug output without escaped quotes
        write!(f, "Id({})", self.name)
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
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        hasher.write_u128(seed);
        hasher.write(name.as_bytes());
        let hi = hasher.finish();
        hasher.write_u128(u128::MAX - seed);
        hasher.write(name.as_bytes());
        let lo = hasher.finish();
        let public_key = ((hi as u64) ^ (lo as u64)) as u32;
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
impl Default for Locker {
    fn default() -> Self {
        Self {
            id: Id::default(),
            key: Key::default(),
            lock: Lock::default(),
            queue: Vec::new(),
            contents: Vec::new(),
            timestamp: time(),
        }
    }
}
// NOTE: PartialOrd and Ord implementations removed due to Range<usize> fields in HandlerResult
// These comparison operations are incompatible with the new Range<usize> architecture
#[allow(uninhabited_static, static_mut_refs)]
static mut MAPPED_IDS: LazyLock<HashMap<String, Id>> =
    LazyLock::new(|| HashMap::with_capacity(4096));
static mut ENCRYPTED_IDS: LazyLock<Vec<Lock>> = LazyLock::new(|| Vec::with_capacity(4096));
static mut OBSOLETE_IDS: LazyLock<Vec<Id>> = LazyLock::new(|| Vec::with_capacity(8192));
