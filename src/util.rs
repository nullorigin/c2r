use core::{cmp::{Eq, Ord, PartialOrd}, iter::DoubleEndedIterator, marker::{Send, Sync}, option::Option::None};
use std::{
    alloc::Layout, cmp::Ordering, fmt::{Debug, Display, Formatter}, fs::File, io::{BufRead, Error as IoError, ErrorKind as IoErrorKind, Read, Seek, SeekFrom, Write}, mem, ops::{self, Add, AddAssign, Bound, Deref, DerefMut, Index, IndexMut, RangeBounds, Sub, SubAssign}, path::PathBuf, ptr::{self}, slice, str::FromStr
};
use crate::{
    error::{C2RError, Kind, Reason},
    Result,
};

// Alias for backwards compatibility
type Error = C2RError;
#[derive(Clone, Eq, PartialEq, Default)]
pub struct Paths {
    paths: Vec<PathBuf>,
    max_depth: usize,
    min_depth: usize,
}

#[derive(Debug)]
pub struct Set<T: Eq + Ord> {
    pointer: *mut T,
    size: usize,
    capacity: usize,
}

impl<T: Ord + Eq> Set<T> {
    #[inline]
    pub fn new() -> Self {
        Self {
            pointer: ptr::null_mut(),
            size: 0,
            capacity: 0,
        }
    }
    #[inline]
    pub fn alloc(size: usize, capacity: usize, zeroed: bool) -> Self {
        let capacity = if capacity > size { capacity } else { size };
        if capacity == 0 {
            return Self { pointer: ptr::null_mut(), size: 0, capacity: 0, };
        }
        let layout = Layout::array::<T>(capacity).unwrap();
        let pointer = match zeroed { true => unsafe { std::alloc::alloc_zeroed(layout) }, false => unsafe { std::alloc::alloc(layout) } };
        Self {
            pointer: pointer as *mut T,
            size,
            capacity,
        }
    }
    pub fn with_capacity(capacity: usize) -> Self {
        Self::alloc(capacity, capacity, false)
    }
    pub fn resize(&mut self, size: usize) {
        if size > self.capacity {
            let capacity = std::cmp::max(self.capacity * 2, size);
            let layout = Layout::array::<T>(capacity).unwrap();
            let pointer = unsafe { std::alloc::realloc(self.pointer as *mut u8, layout, capacity * size_of::<T>()) };
            match pointer.is_null() {
                true => panic!("Failed allocation of: {} elements each with size: {}", capacity, size_of::<T>()),
                false => {
                    self.pointer = pointer as *mut T;
                }
            }
            self.capacity = capacity;
        } else if size == 0 {
            self.dealloc();
            return;
        }
        self.size = size;
    }

    #[inline]
    pub fn dealloc(&mut self) {
        if !self.pointer.is_null() {
            let layout = Layout::array::<T>(self.capacity).unwrap();
            unsafe {
                std::alloc::dealloc(self.pointer as *mut u8, layout);
            }
            self.pointer = ptr::null_mut();
            self.size = 0;
            self.capacity = 0;
        }
    }

    #[inline]
    pub fn default() -> Self {
        Self {
            pointer: ptr::null_mut(),
            size: 0,
            capacity: 0,
        }
    }
    #[inline]
    pub fn copy_from(&mut self, other: Set<T>) {
        if other.pointer.is_null() {
            self.dealloc();
            self.pointer = other.pointer;
            return;
        }
        if self.capacity < other.size || self.pointer.is_null() {
            self.pointer = unsafe { std::alloc::realloc(self.pointer as *mut u8, Layout::array::<T>(other.size).unwrap(), other.size * size_of::<T>()) } as *mut T;
            self.capacity = other.size;
        } 
        unsafe { ptr::copy_nonoverlapping(other.pointer, self.pointer, other.size) };
        self.size = other.size;
    }
    pub fn copy_from_slice(&mut self, slice: &[T]) {
        if slice.is_empty() {
            self.dealloc();
            return;
        }
        if self.capacity < slice.len() {
            self.pointer = unsafe { std::alloc::realloc(self.pointer as *mut u8, Layout::array::<T>(slice.len()).unwrap(), slice.len() * size_of::<T>()) } as *mut T;
            self.capacity = slice.len();
        }
        unsafe { ptr::copy_nonoverlapping(slice.as_ptr(), self.pointer, slice.len()) };
        self.size = slice.len();
    }
    #[inline]
    pub fn as_ptr(&self) -> *const T {
        self.pointer as *const T
    }
    #[inline]
    pub fn as_mut_ptr(&mut self) -> *mut T {
        self.pointer
    }
    #[inline]
    pub fn deref(&self) -> Self {
        self.clone()
    }
    #[inline]
    pub fn size(&self) -> usize {
        self.size
    }
    
    #[inline]
    pub fn len(&self) -> usize {
        self.size
    }

    #[inline]
    pub fn capacity(&self) -> usize {
        self.capacity
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.size == 0
    }

    pub fn clone_from(&mut self, other: &Set<T>) {
        if self.capacity < other.size {
            self.dealloc();
            self.pointer = unsafe { std::alloc::alloc(Layout::array::<T>(other.size).unwrap()) } as *mut T;
            self.capacity = other.size;
        }
        unsafe { ptr::copy_nonoverlapping(other.pointer, self.pointer, other.size) };
        self.size = other.size;
    }
    #[inline]
    pub fn from_raw_parts(pointer: *mut T, size: usize) -> Self {
        Self {
            pointer,
            size,
            capacity: size,
        }
    }
    pub fn clone_from_slice(slice: &[T]) -> Self {
        let mut set = Set::from_raw_parts(ptr::null_mut(), slice.len());
        set.pointer = unsafe { std::alloc::alloc(Layout::array::<T>(slice.len()).unwrap()) } as *mut T;
        set.capacity = slice.len();
        unsafe { ptr::copy_nonoverlapping(slice.as_ptr(), set.pointer, slice.len()) };
        set
    }
    #[inline]
    pub fn as_slice(&self) -> &[T] {
        if self.size == 0 || self.capacity == 0 || self.pointer.is_null() {
            &[]
        } else {
            unsafe { slice::from_raw_parts(self.pointer, self.size) }
        }
    }

    #[inline]
    pub fn as_mut_slice(&mut self) -> &mut [T] {
        if self.size == 0 || self.capacity == 0 || self.pointer.is_null() {
            &mut []
        } else {
            unsafe { slice::from_raw_parts_mut(self.pointer, self.size) }
        }
    }
    #[inline]
    pub fn split_at(&self, i: usize) -> (Set<T>, Set<T>) {
        (Set {
            pointer: self.pointer,
            size: i,
            capacity: i,
        },
        Set {
            pointer: unsafe { self.pointer.add(i) },
            size: self.size - i,
            capacity: self.capacity - i,
        })
    }
    #[inline]
    pub fn split_at_mut(&mut self, i: usize) -> (&mut Set<T>, &mut Set<T>) {
        ( unsafe { &mut * (self as *const Self as *mut Set<T>).add(0) },
          unsafe { &mut * (self as *const Self as *mut Set<T>).add(i) } )
    }
    #[inline]
    pub fn split_off(&mut self, i: usize) ->Self {
        let right_size = self.size - i;
        self.size = i;
        let right = Self {
            pointer: unsafe { self.pointer.add(i) },
            size: right_size,
            capacity: right_size,
        };
        right
    }
    
    #[inline]
    pub fn push(&mut self, v: T) {
        if self.contains(&v) {
            return;
        }

        if self.size == self.capacity {
            let new_capacity = if self.capacity == 0 {
                1
            } else {
                self.capacity * 2
            };
            self.reserve(new_capacity - self.capacity);
        }

        unsafe {
            *self.pointer.add(self.size) = v;
        }
        self.size += 1;
    }

    #[inline]
    pub fn pop(&mut self) -> Option<T> {
        self.size -= 1;
        unsafe { Some(ptr::read(self.pointer.add(self.size))) }
    }

    #[inline]
    pub fn pop_at(&mut self, i: usize) -> Option<T> {
        if i >= self.size {
            None
        } else {
            self.size -= 1;
            unsafe {
                let ret = ptr::read(self.pointer.add(i));
                if i < self.size {
                    ptr::copy(
                        self.pointer.add(i + 1),
                        self.pointer.add(i),
                        self.size - i,
                    );
                }
                Some(ret)
            }
        }
    }

    #[inline]
    pub fn clear(&mut self) {
        self.size = 0;
    }

    #[inline]
    pub fn index(&self, i: usize) -> Option<&T> {
        unsafe { self.pointer.add(i).as_ref() }
    }

    #[inline]
    pub fn index_mut(&mut self, i: usize) -> Option<&mut T> {
        unsafe { self.pointer.add(i).as_mut() }
    }


    #[inline]
    pub fn from_vec(vec: &Vec<T>) -> Self {
        if vec.is_empty() {
            return Set::new();
        }
        Set::from_raw_parts(vec.as_ptr() as *mut T, vec.len())
    }
    pub fn to_vec(&self) -> Vec<T> {
        if self.size == 0 || self.capacity == 0 || self.pointer.is_null() {
            return Vec::new();
        }
         unsafe { Vec::from_raw_parts(self.pointer, self.size, self.capacity) }
    }
    #[inline]
    pub fn extend(&mut self, set: &Set<T>) {
        if set.is_empty() {
            return;
        }

        self.reserve(set.size);

        let mut i = 0;
        while i < set.size {
            let item = unsafe { set.pointer.add(i).read() };
            if !self.contains(&item) {
                self.push(item);
            }
            i += 1;
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
        if self.capacity == 0 || self.pointer.is_null() {
            self.pointer = unsafe {
                std::alloc::alloc(Layout::array::<T>(additional).unwrap())
            } as *mut T;
            self.capacity = additional;
            return;
        }
        let capacity = self.capacity + additional;
        self.pointer = unsafe { 
            std::alloc::realloc(self.pointer as *mut u8, Layout::array::<T>(self.capacity).unwrap(), capacity * mem::size_of::<T>(),) as *mut T
        };
        self.capacity = capacity;
    }
    #[inline]
    pub fn sort(&mut self) {
        if self.size <= 1 {
            return;
        }
        
        // Use insertion sort for smaller lists
        if self.size <= 16 {
            self.insertion_sort();
        } else {
            // Use quicksort for larger lists
            self.quicksort(0, self.size - 1);
        }
        
        // Use insertion sort to sort the last 16 elements
        self.insertion_sort();
    }
    
    #[inline]
    pub fn insertion_sort(&mut self) {
        for i in 1..self.size {
            let mut j = i;
            let key = unsafe { self.pointer.add(i).read() };
            while j > 0 && unsafe { self.pointer.add(j - 1).read() < key } {
                unsafe { self.pointer.add(j).write(self.pointer.add(j - 1).read()) };
                j -= 1;
            }
            unsafe { self.pointer.add(j).write(key) };
        }
    }
    
    #[inline]
    pub fn quicksort(&mut self, low: usize, high: usize) {
        if low < high {
            let pivot = self.partition(low, high);
            let left_size = pivot - low;
            let right_size = high - pivot;
            if left_size < right_size {
                self.quicksort(low, pivot - 1);
                self.quicksort(pivot + 1, high);
            } else {
                self.quicksort(pivot + 1, high);
                self.quicksort(low, pivot - 1);
            }
        }
    }
    
    #[inline]
    pub fn partition(&mut self, low: usize, high: usize) -> usize {
        let pivot = unsafe { self.pointer.add(high) };
        let mut i = low;
        for j in low..high {
            if unsafe { (*self.pointer.add(j)) <= *pivot } {
                unsafe { ptr::swap(self.pointer.add(i), self.pointer.add(j)) };
                i += 1;
            }
        }
        unsafe { ptr::swap(self.pointer.add(i), self.pointer.add(high)) };
        i
    }
    #[inline]
    pub fn sort_by<F: FnMut(&T, &T) -> Ordering>(&mut self, f: F) {
        self.as_mut_slice().sort_by(f);
    }
    #[inline]
    pub fn sort_by_key<F: FnMut(&T) -> K, K: Ord>(&mut self, f: F) {
        self.as_mut_slice().sort_by_key(f);
    }
    #[inline]
    pub fn dedup(&mut self) {
        if self.size <= 1 {
            return;
        }
        
        let mut write_idx = 1;
        for read_idx in 1..self.size {
            let mut is_unique = true;
            
            // Check if current element already exists in the processed portion
            for i in 0..write_idx {
                if unsafe { *self.pointer.add(read_idx) == *self.pointer.add(i) } {
                    is_unique = false;
                    break;
                }
            }
            
            // If unique, keep it
            if is_unique {
                unsafe {
                    if write_idx != read_idx {
                        ptr::copy_nonoverlapping(
                            self.pointer.add(read_idx),
                            self.pointer.add(write_idx),
                            1
                        );
                    }
                }
                write_idx += 1;
            }
        }
        
        self.size = write_idx;
    }
    #[inline]
    pub fn shrink_to_fit(&mut self) {
        if self.size < self.capacity / 2 {
            self.reserve(0);
        }
    }
    #[inline]
    pub fn size_of(&self) -> usize {
        size_of::<Self>()
    }
    #[inline]
    pub fn align_of(&self) -> usize {
        align_of::<Self>()
    }
    #[inline]
    pub fn drop(&mut self) {
        if self.size > 0 {
            unsafe {
                ptr::drop_in_place(self.pointer);
                self.size = 0;
            }
        }
    }
    #[inline]
    pub fn swap(&mut self, i: usize, j: usize) {
        if i < self.size && j < self.size && i != j {
            unsafe {
                ptr::swap(self.pointer.add(i), self.pointer.add(j));
            }
        }
    }
    #[inline]
    pub fn swap_remove(&mut self, i: usize) -> T {
        debug_assert!(i < self.size);
        
        let ret = unsafe { ptr::replace(self.pointer.add(i), ptr::read(self.pointer.add(self.size - 1))) };
        self.size -= 1;
        ret
    }

    #[inline]
    pub fn retain<F: FnMut(&T) -> bool>(&mut self, mut f: F) {
        let mut i = 0;
        while i < self.size {
            if !f(unsafe { &*self.pointer.add(i) }) {
                let last = self.swap_remove(self.size - 1);
                if i != self.size - 1 {
                    unsafe { ptr::write(self.pointer.add(i), last) };
                }
            } else {
                i += 1;
            }
        }
    }
    #[inline]
    pub fn remove<F: FnMut(&T) -> bool>(&mut self, mut f: F) -> Option<T> {
        let mut i = 0;
        while i < self.size {
            if f(unsafe { &*self.pointer.add(i) }) {
                return Some(self.swap_remove(i));
            } else {
                i += 1;
            }
        }
        None
    }
    pub fn replace(&mut self, i: usize, v: T) -> T {
        unsafe { ptr::replace(self.pointer.add(i), v) }
    }
    #[inline]
    pub fn truncate(&mut self, len: usize) {
        if len < self.size {
            unsafe {
                let tail = self.size - len;
                let ptr = self.pointer.add(len);
                let mut i = 0;
                while i < tail {
                    ptr::drop_in_place(ptr.add(i));
                    i += 1;
                }
                self.size = len;
            }
        }
    }

    #[inline]
    pub fn insert(&mut self, i: usize, v: T) {
        if i <= self.size && !self.contains(&v) {
            self.reserve(1);
            unsafe {
                if i < self.size {
                    ptr::copy(self.pointer.add(i), self.pointer.add(i + 1), self.size - i);
                }
                ptr::write(self.pointer.add(i), v);
                self.size += 1;
            }
        }
    }

    #[inline]
    pub fn iter(&self) -> slice::Iter<'_, T> {
        unsafe { slice::from_raw_parts(self.pointer, self.size) }.iter()
    }
    
    #[inline]
    pub fn iter_mut(&mut self) -> slice::IterMut<'_, T> {
        unsafe { slice::from_raw_parts_mut(self.pointer, self.size) }.iter_mut()
    }

    #[inline]
    pub fn position<F: FnMut(&T) -> bool>(&self, mut f: F) -> Option<usize> {
        for i in 0..self.size {
            if f(unsafe { &*self.pointer.add(i) }) {
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
    pub fn map<F: FnMut(&T) -> T>(&self, mut f: F) -> Self {
        let mut result = self.clone();
        unsafe {
            self.iter().for_each(|v| {
                *result.pointer.add(result.size) = f(v);
                result.size += 1;
            });
        }
        result
    }
    #[inline]
    pub fn find<F: FnMut(&T) -> bool>(&self, mut f: F) -> Option<&T> {
        for i in 0..self.size {
            unsafe {
                let item = &*self.pointer.add(i);
                if f(item) {
                    return Some(item);
                }
            }
        }
        None
    }
    #[inline]
    pub fn find_range<F: Fn(&T) -> bool>(&self, f: F) -> Option<(usize, usize)> {
        if self.size == 0 {
            return None;
        }

        let start = (0..self.size).find(|&i| f(unsafe { &*self.pointer.add(i) }))?;
        let end = (start + 1..self.size)
            .find(|&i| !f(unsafe { &*self.pointer.add(i) }))
            .unwrap_or(self.size);

        Some((start, end))
    }
    #[inline]
    pub fn find_range_mut<F: FnMut(&T) -> bool>(&mut self, mut f: F) -> Option<(usize, usize)> {
        if self.size == 0 {
            return None;
        }

        let start = (0..self.size).find(|&i| f(unsafe { &*self.pointer.add(i) }))?;
        let end = (start + 1..self.size)
            .find(|&i| !f(unsafe { &*self.pointer.add(i) }))
            .unwrap_or(self.size);

        Some((start, end))
    }
    #[inline]
    pub fn range_partition(&self, ranges: Set<(usize, usize)>) -> Set<Self> {
        let mut subsets: Set<Self> = Set::new();
        for (start, end) in ranges.iter() {
            subsets.push(self.subset(*start, *end));
        }
        subsets
    }
    #[inline]
    pub fn subdivide(&self, count: usize) -> Set<Self> {
        if count == 0 || self.is_empty() {
            return Set::new();
        }

        let mut subsets = Set::with_capacity(count);
        let base_size = self.size / count;
        let remainder = self.size % count;

        let mut start = 0;
        for i in 0..count {
            let subset_size = base_size
                + match i == count - 1 {
                    true => remainder,
                    false => 0,
                };
            let end = start + subset_size;
            subsets.push(self.subset(start, end));
            start = end;
        }

        subsets
    }
    #[inline]
    pub fn subset(&self, start: usize, end: usize) -> Self {
        match start >= self.size || end <= start || end > self.size {
            true => Self::new(),
            false => {
                let len = end - start;
                let mut data = Self::with_capacity(len);
                unsafe {
                    ptr::copy_nonoverlapping(self.pointer.add(start), data.pointer, len);
                    data.size = len;
                }
                data
            }
        }
    }
    pub fn reverse(&self) -> Self {
        let pointer = self.pointer;
        let mut i = 0;
        let mut j = self.size - 1;
        while i < j {
            unsafe { pointer.add(i).swap(pointer.add(j)); };
            i += 1;
            j -= 1;
        }
        Self { pointer, size: self.size, capacity: self.capacity, }
    }
    #[inline]
    pub fn rotate(&self, start: usize, end: usize) -> Self {
        if start <= self.size && end > start && end <= self.size {
            let pointer = unsafe { self.pointer.add(start) };
            let size = end - start;
            let capacity = self.capacity - start;
            Self {
                pointer,
                size,
                capacity,
            }
        } else if start <= self.size && start > end && end <= self.size {
            let pointer = unsafe { self.pointer.add(end) };
            let mut i = 0;
            let mut j = start - end;
            while i < j {
                unsafe { pointer.add(i).swap(pointer.add(j)); };
                i += 1;
                j -= 1;
            }
            Self {
                pointer,
                size: self.size - end,
                capacity: self.capacity - end,
            }
        } else {
            Self::new()
        }
    }
}

impl<T: Eq + Ord> Drop for Set<T> {
    fn drop(&mut self) {
        if self.capacity > 0 {
            self.dealloc();
        } else {
            self.pointer = ptr::null_mut();
        }
    }
}
impl<T: Ord> Default for Set<T> {
    fn default() -> Self {
        Self {
            pointer: ptr::null_mut(),
            size: 0,
            capacity: 0,
        }
    }
}
impl<T: Eq + Ord> Clone for Set<T> {
    fn clone(&self) -> Self {
        let result = Self::alloc(self.size, self.capacity, true);
        unsafe {
            result.pointer.copy_from_nonoverlapping(self.pointer, self.size);
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
impl<T:Eq + Ord> AddAssign for Set<T> {
    fn add_assign(&mut self, other: Self) {
        self.extend(&other);
    }
}
impl<T:Eq + Ord> Add for Set<T> {
    type Output = Set<T>;
    fn add(self, other: Self) -> Self::Output {
        let mut set = self;
        set.extend(&other);
        set
    }
}
impl<T:Eq + Ord> SubAssign for Set<T> {
    fn sub_assign(&mut self, other: Self) {
        self.retain(|x| !other.contains(x));
    }
}
impl<T:Eq + Ord> Sub for Set<T> {
    type Output = Set<T>;
    fn sub(self, other: Self) -> Self::Output {
        let mut set = self;
        set.retain(|x| !other.contains(x));
        set
    }
}
impl<T:Eq + Ord> Index<usize> for Set<T> {
    type Output = T;
    fn index(&self, i: usize) -> &Self::Output {
        match i < self.size {
            true => unsafe { &*self.pointer.add(i) },
            false => panic!("index out of bounds"),
        }
    }
}
impl<T:Eq + Ord> IndexMut<usize> for Set<T> {
    fn index_mut(&mut self, i: usize) -> &mut Self::Output {
        match i < self.size {
            true => unsafe { &mut *self.pointer.add(i) },
            false => panic!("index out of bounds"),
        }
    }
}
impl<T:Eq + Ord> RangeBounds<T> for Set<T> {
    fn start_bound(&self) -> Bound<&T> {
        match self.size == 0 {
            true => Bound::Unbounded,
            false => Bound::Included(&self[0]),
        }
    }
    fn end_bound(&self) -> Bound<&T> {
        match self.size == 0 {
            true => Bound::Unbounded,
            false => Bound::Included(&self[self.size - 1]),
        }
    }
}
impl<T:Eq + Ord> From<Vec<T>> for Set<T> {
    fn from(vec: Vec<T>) -> Self {
        match vec.is_empty() {
            true => Set::new(),
            false => {
                let size = vec.len();
                let result = Self::alloc(size, size,false);
                unsafe {
                    result.pointer.copy_from_nonoverlapping(vec.as_ptr(), size);
                }
                result
            }
        }
    }
}
impl<T: Ord> FromIterator<T> for Set<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut set = Set::new();
        for item in iter {
            set.push(item);
        }
        set
    }
}
impl<T: Clone + Ord> Iterator for Set<T> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        match self.size == 0 {
            true => None,
            false => {
                let item = unsafe { (*self.pointer).clone() };
                self.pop_at(0);
                Some(item)
            }
        }
    }
}
impl<T: Clone + Ord> DoubleEndedIterator for Set<T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        match self.size == 0 {
            true => None,
            false => {
                let index = self.size - 1;
                let item = unsafe { (*self.pointer.add(index)).clone() };
                self.pop_at(index);
                Some(item)
            }
        }
    }
}
impl<T: Clone + Ord> Extend<T> for Set<T> {
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        for item in iter {
            if !self.contains(&item) {
                self.push(item);
            }
        }
    }
}
impl<T:                             Eq + Ord> PartialOrd for Set<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl<T: Eq + Ord> Ord for Set<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        // Compare lengths first for quick inequality check
        match self.size.cmp(&other.size) {
            Ordering::Equal => {}
            ord => return ord,
        }

        // If equal lengths, compare elements
        for i in 0..self.size {
            match unsafe { (*self.pointer.add(i)).cmp(&*other.pointer.add(i)) } {
                Ordering::Equal => continue,
                ord => return ord,
            }
        }

        Ordering::Equal
    }
}
impl<T: Eq + Ord + AsRef<[u8]>> BufRead for Set<T> {
    fn fill_buf(&mut self) -> std::result::Result<&[u8], IoError> {
        match self.is_empty() {
            true => Ok(&[]),
            false => Ok(self[0].as_ref()),
        }
    }

    fn consume(&mut self, amt: usize) {
        self.size = self.size.saturating_sub(amt);
        unsafe {
            self.pointer = self.pointer.add(amt.min(self.size));
        }
    }

    fn read_until(&mut self, byte: u8, buf: &mut Vec<u8>) -> std::result::Result<usize, IoError> {
        let initial_len = buf.len();

        for item in self.iter() {
            let data = item.as_ref();
            if let Some(pos) = data.iter().position(|&b| b == byte) {
                buf.extend_from_slice(&data[..=pos]);
                return Ok(buf.len() - initial_len);
            }
            buf.extend_from_slice(data);
        }

        Ok(buf.len() - initial_len)
    }

    fn read_line(&mut self, buf: &mut String) -> std::result::Result<usize, IoError> {
        let _initial_len = buf.len();
        let mut bytes = Vec::new();
        let n = self.read_until(b'\n', &mut bytes)?;

        buf.push_str(
            std::str::from_utf8(&bytes)
                .map_err(|_| IoError::new(IoErrorKind::InvalidData, "Invalid UTF-8"))?,
        );

        Ok(n)
    }
}

impl<T: Clone + Ord + AsRef<[u8]> + From<Vec<u8>>> Write for Set<T> {
    fn write(&mut self, buf: &[u8]) -> std::result::Result<usize, IoError> {
        if !buf.is_empty() {
            self.push(T::from(buf.to_vec()));
        }
        Ok(buf.len())
    }
    fn flush(&mut self) -> std::result::Result<(), IoError> {
        Ok(())
    }
}

impl<T: Ord + Eq + AsRef<[u8]>> Read for Set<T> {
    fn read(&mut self, buf: &mut [u8]) -> std::result::Result<usize, IoError> {
        if buf.is_empty() || self.is_empty() {
            return Ok(0);
        }

        let mut total = 0;
        for item in self.iter() {
            let data = item.as_ref();
            let n = std::cmp::min(data.len(), buf.len() - total);
            if n > 0 {
                buf[total..total + n].copy_from_slice(&data[..n]);
                total += n;
                if total == buf.len() {
                    break;
                }
            }
        }
        Ok(total)
    }
}

impl<T: Ord> Seek for Set<T> {
    fn seek(&mut self, pos: SeekFrom) -> std::result::Result<u64, std::io::Error> {
        match pos {
            SeekFrom::Start(n) => match n <= self.size as u64 {
                true => Ok(n),
                false => Err(std::io::Error::new(
                    std::io::ErrorKind::InvalidInput,
                    "Invalid seek position",
                )),
            },
            SeekFrom::End(n) => {
                let new_pos = self.size as i64 + n;
                match new_pos >= 0 && new_pos <= self.size as i64 {
                    true => Ok(new_pos as u64),
                    false => Err(std::io::Error::new(
                        std::io::ErrorKind::InvalidInput,
                        "Invalid seek position",
                    )),
                }
            }
            SeekFrom::Current(_) => Err(std::io::Error::new(
                std::io::ErrorKind::Unsupported,
                "SeekFrom::Current not supported",
            )),
        }
    }
}
impl<T: Eq + Ord> PartialEq for Set<T> {
    fn eq(&self, other: &Self) -> bool {
        self.size == other.size && self.iter().zip(other.iter()).all(|(a, b)| a == b)
    }
}



#[derive(Debug)]
pub struct List<K: Eq, V> {
    keys: *mut K,
    values: *mut V,
    cursor: usize,
    size: usize,
    capacity: usize,
}
// Only implement Clone when both K and V are cloneable
impl<K: Eq, V> Default for List<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K: Eq + Clone, V: Clone> Clone for List<K, V> {
    fn clone(&self) -> Self {
        let mut list = List::with_capacity(self.capacity);
        list.cursor = self.cursor;
        
        // Properly clone the actual data, not just the pointers
        for i in 0..self.size {
            unsafe {
                let key = (*self.keys.add(i)).clone();
                let value = (*self.values.add(i)).clone();
                list.push(key, value);
            }
        }
        
        list
    }
}

impl<K: Eq, V> Drop for List<K, V> {
    fn drop(&mut self) {
        self.dealloc();
    }
}
impl<K: Eq, V> List<K, V> {
    pub const fn new() -> Self {
        Self {
            keys: ptr::null_mut(),
            values: ptr::null_mut(),
            cursor: 0,
            size: 0,
            capacity: 0,
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        if capacity == 0 {
            return Self::new();
        }
        
        let keys_layout = Layout::array::<K>(capacity).unwrap();
        let values_layout = Layout::array::<V>(capacity).unwrap();
        
        let keys = unsafe { std::alloc::alloc(keys_layout) as *mut K };
        let values = unsafe { std::alloc::alloc(values_layout) as *mut V };
        
        // Check if allocation succeeded
        if keys.is_null() || values.is_null() {
            panic!("Failed to allocate memory for List with capacity {}", capacity);
        }
        
        Self {
            keys,
            values,
            cursor: 0,
            size: 0,
            capacity,
        }
    }

    #[inline]
    fn resize(&mut self, new_size: usize) {
        if new_size <= self.capacity {
            return;
        }
        
        // Ensure minimum capacity and proper growth
        let new_capacity = new_size.max(self.capacity * 2).max(4);
        
        if self.keys.is_null() {
            *self = Self::with_capacity(new_capacity);
            return;
        }
        
        let old_keys_layout = Layout::array::<K>(self.capacity).unwrap();
        let old_values_layout = Layout::array::<V>(self.capacity).unwrap();
        
        let new_keys_layout = Layout::array::<K>(new_capacity).unwrap();
        let new_values_layout = Layout::array::<V>(new_capacity).unwrap();
        
        let new_keys = unsafe { 
            std::alloc::realloc(self.keys as *mut u8, old_keys_layout, new_keys_layout.size()) as *mut K
        };
        let new_values = unsafe { 
            std::alloc::realloc(self.values as *mut u8, old_values_layout, new_values_layout.size()) as *mut V
        };
        
        // Check if allocation succeeded
        if new_keys.is_null() || new_values.is_null() {
            panic!("Failed to allocate memory for List resize");
        }
        
        self.keys = new_keys;
        self.values = new_values;
        self.capacity = new_capacity;
    }

    #[inline]
    pub fn get(&self, key: &K) -> Option<&V> {
        self.find_key_index(key).map(|index| unsafe { &*self.values.add(index) })
    }

    #[inline]
    pub fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        self.find_key_index(key).map(|index| unsafe { &mut *self.values.add(index) })
    }

    #[inline]
    pub fn get_at(&self, index: usize) -> Option<(&K, &V)> {
        (index < self.size).then(|| unsafe { 
            (&*self.keys.add(index), &*self.values.add(index)) 
        })
    }

    #[inline]
    pub fn get_at_mut(&mut self, index: usize) -> Option<(&K, &mut V)> {
        (index < self.size).then(|| unsafe { 
            (&*self.keys.add(index), &mut *self.values.add(index)) 
        })
    }

    #[inline]
    pub fn get_key_at(&self, index: usize) -> Option<&K> {
        (index < self.size).then(|| unsafe { &*self.keys.add(index) })
    }

    #[inline]
    pub fn get_value_at(&self, index: usize) -> Option<&V> {
        (index < self.size).then(|| unsafe { &*self.values.add(index) })
    }

    #[inline]
    pub fn get_value_at_mut(&mut self, index: usize) -> Option<&mut V> {
        (index < self.size).then(|| unsafe { &mut *self.values.add(index) })
    }

    #[inline]
    fn find_key_index(&self, key: &K) -> Option<usize> {
        (0..self.size).find(|&i| unsafe { &*self.keys.add(i) == key })
    }

    #[inline]
    pub fn contains_key(&self, key: &K) -> bool {
        self.find_key_index(key).is_some()
    }
    pub fn keys(&self) -> impl Iterator<Item = &K> {
        (0..self.size).map(|i| unsafe { &*self.keys.add(i) })
    }
    pub fn values(&self) -> impl Iterator<Item = &V> {
        (0..self.size).map(|i| unsafe { &*self.values.add(i) })
    }
    pub fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        (0..self.size).map(|i| unsafe { (&*self.keys.add(i), &*self.values.add(i)) })
    }  
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (&K, &mut V)> {
        (0..self.size).map(|i| unsafe { (&*self.keys.add(i), &mut *self.values.add(i)) })
    }
    #[inline]
    pub fn cursor(&self) -> usize {
        self.cursor
    }
    
    #[inline]
    pub fn capacity(&self) -> usize {
        self.capacity
    }
    
    #[inline]
    pub fn size(&self) -> usize {
        self.size
    }
    
    #[inline]
    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        if let Some(index) = self.find_key_index(&key) {
            let old_value = unsafe { ptr::replace(self.values.add(index), value) };
            unsafe {
                ptr::write(self.keys.add(index), key);
            }
            Some(old_value)
        } else {
            self.resize(self.size + 1);
            
            // Safety check: ensure pointers are valid after resize
            if self.keys.is_null() || self.values.is_null() {
                panic!("Invalid pointers after List resize - memory allocation failed");
            }
            
            unsafe {
                ptr::write(self.keys.add(self.size), key);
                ptr::write(self.values.add(self.size), value);
            }
            self.size += 1;
            None
        }
    }

    pub fn remove(&mut self, key: &K) -> Option<V> {
        let index = self.find_key_index(key)?;
        
        let value = unsafe { ptr::read(self.values.add(index)) };
        unsafe { ptr::drop_in_place(self.keys.add(index)) };
        
        let move_count = self.size - index - 1;
        if move_count > 0 {
            unsafe {
                ptr::copy(self.keys.add(index + 1), self.keys.add(index), move_count);
                ptr::copy(self.values.add(index + 1), self.values.add(index), move_count);
            }
        }
        
        self.size -= 1;
        
        if self.cursor > index {
            self.cursor -= 1;
        } else if self.cursor == index && self.cursor >= self.size && self.size > 0 {
            self.cursor = self.size - 1;
        }
        
        Some(value)
    }

    pub fn remove_at(&mut self, index: usize) -> Option<(K, V)> {
        if index >= self.size {
            return None;
        }
        
        let key = unsafe { ptr::read(self.keys.add(index)) };
        let value = unsafe { ptr::read(self.values.add(index)) };
        
        let move_count = self.size - index - 1;
        if move_count > 0 {
            unsafe {
                ptr::copy(self.keys.add(index + 1), self.keys.add(index), move_count);
                ptr::copy(self.values.add(index + 1), self.values.add(index), move_count);
            }
        }
        
        self.size -= 1;
        
        if self.cursor > index {
            self.cursor -= 1;
        } else if self.cursor == index && self.cursor >= self.size && self.size > 0 {
            self.cursor = self.size - 1;
        }
        
        Some((key, value))
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.size
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.size == 0
    }

    pub fn clear(&mut self) {
        for i in 0..self.size {
            unsafe {
                ptr::drop_in_place(self.keys.add(i));
                ptr::drop_in_place(self.values.add(i));
            }
        }
        self.size = 0;
        self.cursor = 0;
    }
    pub fn as_slice(&self) -> &[V] {
        unsafe { std::slice::from_raw_parts(self.values, self.size) }
    }
    pub fn as_mut_slice(&mut self) -> &mut [V] {
        unsafe { std::slice::from_raw_parts_mut(self.values, self.size) }
    }

    pub fn as_key_slice(&self) -> &[K] {
        unsafe { std::slice::from_raw_parts(self.keys, self.size) }
    }
    pub fn as_mut_key_slice(&mut self) -> &mut [K] {
        unsafe { std::slice::from_raw_parts_mut(self.keys, self.size) }
    }
    pub fn to_vec(&self) -> Vec<(K, V)> {
        (0..self.size)
            .map(|i| unsafe { (self.keys.add(i).read(), self.values.add(i).read()) })
            .collect()
    }

    pub fn to_value_vec(&self) -> Vec<V> {
        (0..self.size)
            .map(|i| unsafe { self.values.add(i).read() })
            .collect()
    }

    pub fn to_key_vec(&self) -> Vec<K> {
        (0..self.size)
            .map(|i| unsafe { self.keys.add(i).read() })
            .collect()
    }
    #[inline]
    pub fn seek_to_key(&mut self, key: &K) -> bool {
        if let Some(index) = self.find_key_index(key) {
            self.cursor = index;
            true
        } else {
            false
        }
    }

    #[inline]
    pub fn seek_to_index(&mut self, index: usize) {
        if index < self.size {
            self.cursor = index;
        }
    }
    pub fn sort_by_key(self, compare: fn(&K, &K) -> Ordering) -> List<K, V> {
        for i in 0..self.size {
            for j in i+1..self.size {
                if compare(unsafe { &*self.keys.add(i) }, unsafe { &*self.keys.add(j) }) > Ordering::Equal {
                    unsafe { self.keys.add(i).swap(self.keys.add(j)) };
                    unsafe { self.values.add(i).swap(self.values.add(j)) };
                }
            }
        }
        self
    }
    pub fn sort_by_value(self, compare: fn(&V, &V) -> Ordering) -> List<K, V> {
        for i in 0..self.size {
            for j in i+1..self.size {
                if compare(unsafe { &*self.values.add(i) }, unsafe { &*self.values.add(j) }) > Ordering::Equal {
                    unsafe { self.values.add(i).swap(self.values.add(j)) };
                    unsafe { self.keys.add(i).swap(self.keys.add(j)) };
                }
            }
        }
        self
    }
    #[inline]
    pub fn current(&self) -> Option<(&K, &V)> {
        (self.cursor < self.size).then(|| unsafe { 
            (&*self.keys.add(self.cursor), &*self.values.add(self.cursor))
        })
    }

    #[inline]
    pub fn current_mut(&mut self) -> Option<(&K, &mut V)> {
        (self.cursor < self.size).then(|| unsafe {
            (&*self.keys.add(self.cursor), &mut *self.values.add(self.cursor))
        })
    }

    // Additional methods adapted from Set implementation
    
    /// Properly deallocate the List memory
    #[inline]
    pub fn dealloc(&mut self) {
        // Prevent double-free by checking if already deallocated
        if self.keys.is_null() || self.values.is_null() {
            return;
        }
        
        // Drop all elements first
        if self.size > 0 {
            for i in 0..self.size {
                unsafe {
                    // Ensure we only drop valid elements
                    if i < self.capacity {
                        self.keys.add(i).drop_in_place();
                        self.values.add(i).drop_in_place();
                    }
                }
            }
        }
        
        // Deallocate memory
        if self.capacity > 0 {
            if let Ok(keys_layout) = Layout::array::<K>(self.capacity) {
                if let Ok(values_layout) = Layout::array::<V>(self.capacity) {
                    unsafe {
                        std::alloc::dealloc(self.keys as *mut u8, keys_layout);
                        std::alloc::dealloc(self.values as *mut u8, values_layout);
                    }
                }
            }
        }
        
        // Reset all fields to prevent double-free
        self.keys = ptr::null_mut();
        self.values = ptr::null_mut();
        self.size = 0;
        self.capacity = 0;
        self.cursor = 0;
    }
    
    /// Copy data from another List
    pub fn copy_from(&mut self, other: &List<K, V>) 
    where
        K: Clone,
        V: Clone,
    {
        if other.is_empty() {
            self.clear();
            return;
        }
        
        // Ensure sufficient capacity
        if self.capacity < other.size {
            self.resize(other.size);
        }
        
        // Copy elements
        for i in 0..other.size {
            unsafe {
                let key = (*other.keys.add(i)).clone();
                let value = (*other.values.add(i)).clone();
                ptr::write(self.keys.add(i), key);
                ptr::write(self.values.add(i), value);
            }
        }
        
        self.size = other.size;
        self.cursor = other.cursor.min(self.size.saturating_sub(1));
    }

    /// Copy data from slices
    pub fn copy_from_slices(&mut self, keys: &[K], values: &[V])
    where
        K: Clone,
        V: Clone,
    {
        assert_eq!(keys.len(), values.len(), "Keys and values must have same length");
        
        if keys.is_empty() {
            self.clear();
            return;
        }
        
        // Ensure sufficient capacity
        if self.capacity < keys.len() {
            self.resize(keys.len());
        }
        
        // Copy elements
        for i in 0..keys.len() {
            unsafe {
                ptr::write(self.keys.add(i), keys[i].clone());
                ptr::write(self.values.add(i), values[i].clone());
            }
        }
        
        self.size = keys.len();
        self.cursor = 0;
    }

    /// Get raw pointer to keys
    #[inline]
    pub fn as_key_ptr(&self) -> *const K {
        self.keys as *const K
    }

    /// Get raw pointer to values  
    #[inline]
    pub fn as_value_ptr(&self) -> *const V {
        self.values as *const V
    }

    /// Get mutable raw pointer to keys
    #[inline]
    pub fn as_mut_key_ptr(&mut self) -> *mut K {
        self.keys
    }

    /// Get mutable raw pointer to values
    #[inline]  
    pub fn as_mut_value_ptr(&mut self) -> *mut V {
        self.values
    }

    /// Reserve additional capacity
    pub fn reserve(&mut self, additional: usize) {
        if additional == 0 {
            return;
        }
        
        let new_capacity = self.capacity + additional;
        self.resize(new_capacity);
    }

    /// Split the List at the given index, returning the right part
    pub fn split_off(&mut self, at: usize) -> Self
    where
        K: Clone,
        V: Clone,
    {
        assert!(at <= self.size, "Split index {} out of bounds for List of size {}", at, self.size);
        
        if at == self.size {
            return Self::new();
        }
        
        let right_size = self.size - at;
        let mut right = Self::with_capacity(right_size);
        
        // Copy elements to new List
        for i in 0..right_size {
            unsafe {
                let key = (*self.keys.add(at + i)).clone();
                let value = (*self.values.add(at + i)).clone();
                ptr::write(right.keys.add(i), key);
                ptr::write(right.values.add(i), value);
            }
        }
        
        right.size = right_size;
        
        // Truncate this List
        self.size = at;
        if self.cursor >= at {
            self.cursor = at.saturating_sub(1);
        }
        
        right
    }

    /// Push a key-value pair to the end of the List
    pub fn push(&mut self, key: K, value: V) {
        if self.size >= self.capacity {
            let new_capacity = if self.capacity == 0 { 4 } else { self.capacity * 2 };
            self.resize(new_capacity);
        }
        
        unsafe {
            ptr::write(self.keys.add(self.size), key);
            ptr::write(self.values.add(self.size), value);
        }
        self.size += 1;
    }

    /// Pop the last key-value pair from the List
    pub fn pop(&mut self) -> Option<(K, V)> {
        if self.size == 0 {
            return None;
        }
        
        self.size -= 1;
        let key = unsafe { ptr::read(self.keys.add(self.size)) };
        let value = unsafe { ptr::read(self.values.add(self.size)) };
        
        if self.cursor >= self.size && self.size > 0 {
            self.cursor = self.size - 1;
        } else if self.size == 0 {
            self.cursor = 0;
        }
        
        Some((key, value))
    }

    /// Pop element at specific index  
    pub fn pop_at(&mut self, index: usize) -> Option<(K, V)> {
        if index >= self.size {
            return None;
        }
        
        let key = unsafe { ptr::read(self.keys.add(index)) };
        let value = unsafe { ptr::read(self.values.add(index)) };
        
        // Shift elements down
        let move_count = self.size - index - 1;
        if move_count > 0 {
            unsafe {
                ptr::copy(self.keys.add(index + 1), self.keys.add(index), move_count);
                ptr::copy(self.values.add(index + 1), self.values.add(index), move_count);
            }
        }
        
        self.size -= 1;
        
        // Update cursor
        if self.cursor > index {
            self.cursor -= 1;
        } else if self.cursor == index && self.cursor >= self.size && self.size > 0 {
            self.cursor = self.size - 1;
        }
        
        Some((key, value))
    }

    /// Extend with another List
    pub fn extend_from_list(&mut self, other: &List<K, V>)
    where
        K: Clone,
        V: Clone,
    {
        if other.is_empty() {
            return;
        }
        
        self.reserve(other.size);
        
        for i in 0..other.size {
            unsafe {
                let key = (*other.keys.add(i)).clone();
                let value = (*other.values.add(i)).clone();
                self.push(key, value);
            }
        }
    }

    /// Create a List from raw parts (unsafe)
    #[inline]
    pub unsafe fn from_raw_parts(keys: *mut K, values: *mut V, size: usize, capacity: usize) -> Self {
        Self {
            keys,
            values,
            cursor: 0,
            size,
            capacity,
        }
    }

    /// Consume the List and return raw parts
    #[inline] 
    pub fn into_raw_parts(self) -> (*mut K, *mut V, usize, usize) {
        let keys = self.keys;
        let values = self.values;
        let size = self.size;
        let capacity = self.capacity;
        std::mem::forget(self); // Prevent Drop from running
        (keys, values, size, capacity)
    }

    /// Shrink capacity to fit current size
    pub fn shrink_to_fit(&mut self) {
        if self.size < self.capacity {
            if self.size == 0 {
                self.dealloc();
            } else {
                self.resize(self.size);
            }
        }
    }

    /// Create a List from a Vec of tuples
    pub fn from_vec(vec: Vec<(K, V)>) -> Self {
        if vec.is_empty() {
            return Self::new();
        }
        
        let mut list = Self::with_capacity(vec.len());
        for (key, value) in vec {
            list.push(key, value);
        }
        list
    }

    /// Convert List to Vec of tuples
    pub fn to_tuple_vec(&self) -> Vec<(K, V)>
    where
        K: Clone,
        V: Clone,
    {
        (0..self.size)
            .map(|i| unsafe {
                ((*self.keys.add(i)).clone(), (*self.values.add(i)).clone())
            })
            .collect()
    }

    /// Retain elements matching predicate
    pub fn retain<F>(&mut self, mut f: F)
    where
        F: FnMut(&K, &V) -> bool,
    {
        let mut write_idx = 0;
        
        for read_idx in 0..self.size {
            unsafe {
                let key_ref = &*self.keys.add(read_idx);
                let value_ref = &*self.values.add(read_idx);
                
                if f(key_ref, value_ref) {
                    if write_idx != read_idx {
                        let key = ptr::read(self.keys.add(read_idx));
                        let value = ptr::read(self.values.add(read_idx));
                        ptr::write(self.keys.add(write_idx), key);
                        ptr::write(self.values.add(write_idx), value);
                    }
                    write_idx += 1;
                } else {
                    // Drop elements that don't match
                    ptr::drop_in_place(self.keys.add(read_idx));
                    ptr::drop_in_place(self.values.add(read_idx));
                }
            }
        }
        
        self.size = write_idx;
        if self.cursor >= self.size && self.size > 0 {
            self.cursor = self.size - 1;
        } else if self.size == 0 {
            self.cursor = 0;
        }
    }

    /// Find first element matching predicate
    pub fn find<F>(&self, mut f: F) -> Option<(&K, &V)>
    where
        F: FnMut(&K, &V) -> bool,
    {
        for i in 0..self.size {
            unsafe {
                let key = &*self.keys.add(i);
                let value = &*self.values.add(i);
                if f(key, value) {
                    return Some((key, value));
                }
            }
        }
        None
    }

    /// Find first element matching predicate (mutable)
    pub fn find_mut<F>(&mut self, mut f: F) -> Option<(&K, &mut V)>
    where
        F: FnMut(&K, &V) -> bool,
    {
        for i in 0..self.size {
            unsafe {
                let key = &*self.keys.add(i);
                let value = &*self.values.add(i);
                if f(key, value) {
                    return Some((key, &mut *self.values.add(i)));
                }
            }
        }
        None
    }

    /// Check if any element matches predicate
    pub fn any<F>(&self, f: F) -> bool
    where
        F: FnMut(&K, &V) -> bool,
    {
        self.find(f).is_some()
    }

    /// Check if all elements match predicate
    pub fn all<F>(&self, mut f: F) -> bool
    where
        F: FnMut(&K, &V) -> bool,
    {
        for i in 0..self.size {
            unsafe {
                let key = &*self.keys.add(i);
                let value = &*self.values.add(i);
                if !f(key, value) {
                    return false;
                }
            }
        }
        true
    }

    /// Map List to a new List with different value type
    pub fn map<U, F>(self, mut f: F) -> List<K, U>
    where
        F: FnMut(K, V) -> (K, U),
        K: Eq,
    {
        let mut new_list = List::with_capacity(self.size);
        
        for i in 0..self.size {
            unsafe {
                let key = ptr::read(self.keys.add(i));
                let value = ptr::read(self.values.add(i));
                let (new_key, new_value) = f(key, value);
                new_list.push(new_key, new_value);
            }
        }
        
        // Prevent drop of original list since we consumed it
        std::mem::forget(self);
        new_list
    }
}

impl<K: Eq, V: Eq> Index<usize> for List<K, V> {
    type Output = V;
    
    #[inline]
    fn index(&self, index: usize) -> &Self::Output {
        if index >= self.size {
            panic!("index out of bounds: the len is {} but the index is {}", self.size, index);
        }
        unsafe { &*self.values.add(index) }
    }
}
impl<K: Eq, V: Eq> IndexMut<usize> for List<K, V> {
    #[inline]
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        if index >= self.size {
            panic!("index out of bounds: the len is {} but the index is {}", self.size, index);
        }
        unsafe { &mut *self.values.add(index) }
    }
}

impl<K: Eq, V>  Iterator for List<K, V> {
    type Item = V;
    
    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.cursor >= self.size {
            None
        } else {
            let item = unsafe { self.values.add(self.cursor).read() };
            self.cursor += 1;
            Some(item)
        }
    }
}

impl<K: Eq, V>  DoubleEndedIterator for List<K, V> {
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.cursor >= self.size {
            None
        } else {
            let item = unsafe { self.values.add(self.cursor).read() };
            self.cursor += 1;
            Some(item)
        }
    }
}

impl<K: Eq, V> ExactSizeIterator for List<K, V> {
    #[inline]
    fn len(&self) -> usize {
        self.size
    }
}

impl<K: Eq, V> AsRef<List<K, V>> for List<K, V> {
    fn as_ref(&self) -> &List<K, V> {
        self
    }
}

impl<K: Eq, V> AsMut<List<K, V>> for List<K, V> {
    fn as_mut(&mut self) -> &mut List<K, V> {
        self
    }
}

impl<K: Eq, V> Extend<(K, V)> for List<K, V> {
    fn extend<T: IntoIterator<Item = (K, V)>>(&mut self, iter: T) {
        for (key, value) in iter {
            self.insert(key, value);
        }
    }
}

impl<K: Eq + Clone, V: Clone> From<&[(K, V)]> for List<K, V> {
    fn from(slice: &[(K, V)]) -> Self {
        let mut list = List::new();
        for (key, value) in slice {
            list.insert(key.clone(), value.clone());
        }
        list
    }
}
impl<K: Eq + Clone, V: Clone> From<Vec<(&K, &V)>> for List<K, V> {
    fn from(vec: Vec<(&K, &V)>) -> Self {
        let mut list = List::new();
        for (key, value) in vec {
            list.insert(key.clone(), value.clone());
        }
        list
    }
}
unsafe impl<K: Eq + Send + Sync, V: Send + Sync> Send for List<K, V> {}
unsafe impl<K: Eq + Send + Sync, V: Send + Sync> Sync for List<K, V> {}


impl<T: Eq + Ord> Eq for Set<T> {}
impl Paths {
    pub fn new() -> Self {
        Self {
            paths: Vec::new(),
            max_depth: 10,
            min_depth: 1,
        }
    }

    pub fn get_at(&self, index: usize) -> Result<&PathBuf> {
        self.paths
            .get(index)
            .ok_or(C2RError::new(Kind::Io, Reason::Not("found"), Some("Path not found".to_string())))
    }

    pub fn set_at(&mut self, path: PathBuf, index: usize) -> Result<()> {
        if !path.exists() {
            return Err(C2RError::new(Kind::Io, Reason::Not("found"), Some("Path not found".to_string())));
        }
        let count = path.components().count();
        if count < self.min_depth || count > self.max_depth {
            return Err(C2RError::new(Kind::Io, Reason::Depth("invalid"), Some("Path depth is out of range".to_string())));
        }
        if index >= self.paths.len() {
            self.paths.resize_with(index + 1, PathBuf::new);
        }
        self.paths[index] = path;
        Ok(())
    }

    pub fn set(&mut self, paths: Vec<PathBuf>) -> Result<()> {
        self.paths.clear();
        for path in paths {
            if !path.exists() {
                return Err(C2RError::new(Kind::Io, Reason::Not("found"), Some("Path not found".to_string())));
            }
            let count = path.components().count();
            if count < self.min_depth || count > self.max_depth {
                return Err(C2RError::new(Kind::Io, Reason::Depth("invalid"), Some("Path depth is out of range".to_string())));
            }
            self.paths.push(path);
        }
        Ok(())
    }

    pub fn max_depth(&self) -> usize {
        self.max_depth
    }
    pub fn set_max_depth(&mut self, depth: usize) {
        self.max_depth = depth;
    }
    pub fn min_depth(&self) -> usize {
        self.min_depth
    }
    pub fn set_min_depth(&mut self, depth: usize) {
        self.min_depth = depth;
    }

    pub fn paths(&self) -> &[PathBuf] {
        &self.paths
    }

    pub fn set_paths(&mut self, paths: Vec<PathBuf>) -> Result<()> {
        let filtered: Vec<_> = paths
            .into_iter()
            .filter(|p| {
                let c = p.components().count();
                c >= self.min_depth && c <= self.max_depth
            })
            .collect();

        if filtered.is_empty() {
            return Err(C2RError::new(Kind::Io, Reason::Invalid("paths"), Some("No valid paths found".to_string())));
        }
        self.paths = filtered;
        self.paths.sort_unstable();
        self.paths.dedup();
        Ok(())
    }

    pub fn read(&self, index: usize) -> Result<String> {
        let path = self.get_at(index)?;
        if !path.exists() || !path.is_file() {
            return Err(C2RError::new(Kind::Io, Reason::Not("found"), Some("Path is not a file".to_string())));
        }
        let mut buf = String::new();
        File::open(path)?.read_to_string(&mut buf)?;
        Ok(buf)
    }

    pub fn write(&self, index: usize, contents: &str) -> Result<()> {
        let path = self.get_at(index)?;
        if !path.exists() || !path.is_file() {
            return Err(C2RError::new(Kind::Io, Reason::Invalid("path"), Some("Path is not a file".to_string())));
        }
        File::create(path)?.write_all(contents.as_bytes())?;
        Ok(())
    }

    pub fn read_all(&self) -> Result<Vec<String>> {
        self.paths
            .iter()
            .map(|path| self.read(self.paths.iter().position(|x| x == path).unwrap()))
            .collect()
    }

    pub fn write_all(&self, contents: &[String]) -> Result<()> {
        if contents.len() != self.paths.len() {
            return Err(C2RError::new(Kind::Io, Reason::Length("invalid"), Some("Contents length does not match paths length".to_string())));
        }
        for (i, content) in contents.iter().enumerate() {
            self.write(i, content)?;
        }
        Ok(())
    }

    pub fn exists_count(&self) -> usize {
        self.paths.iter().filter(|p| p.exists()).count()
    }
    pub fn files_count(&self) -> usize {
        self.paths.iter().filter(|p| p.is_file()).count()
    }
    pub fn dirs_count(&self) -> usize {
        self.paths.iter().filter(|p| p.is_dir()).count()
    }
    pub fn symlinks_count(&self) -> usize {
        self.paths.iter().filter(|p| p.is_symlink()).count()
    }

    pub fn exists(&self) -> bool {
        self.paths.iter().all(|p| p.exists())
    }
    pub fn is_all_files(&self) -> bool {
        !self.paths.is_empty() && self.paths.iter().all(|p| p.is_file())
    }
    pub fn is_all_dirs(&self) -> bool {
        !self.paths.is_empty() && self.paths.iter().all(|p| p.is_dir())
    }
    pub fn is_all_symlinks(&self) -> bool {
        !self.paths.is_empty() && self.paths.iter().all(|p| p.is_symlink())
    }
    pub fn is_all_files_or_dirs(&self) -> bool {
        !self.paths.is_empty() && self.paths.iter().all(|p| p.is_file() || p.is_dir())
    }
    pub fn is_all_files_or_symlinks(&self) -> bool {
        !self.paths.is_empty() && self.paths.iter().all(|p| p.is_file() || p.is_symlink())
    }
    pub fn is_all_dirs_or_symlinks(&self) -> bool {
        !self.paths.is_empty() && self.paths.iter().all(|p| p.is_dir() || p.is_symlink())
    }
}

impl Index<usize> for Paths {
    type Output = PathBuf;
    fn index(&self, index: usize) -> &Self::Output {
        &self.paths[index]
    }
}
impl IndexMut<usize> for Paths {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.paths[index]
    }
}
impl<'a> Index<usize> for &'a Paths {
    type Output = PathBuf;
    fn index(&self, index: usize) -> &Self::Output {
        &self.paths[index]
    }
}
impl Deref for Paths {
    type Target = [PathBuf];
    fn deref(&self) -> &Self::Target {
        self.paths.as_slice()
    }
}
impl DerefMut for Paths {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.paths.as_mut_slice()
    }
}
impl From<Vec<PathBuf>> for Paths {
    fn from(paths: Vec<PathBuf>) -> Self {
        let mut pv = Self::new();
        pv.paths = paths;
        pv
    }
}
impl FromStr for Paths {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        let paths: Vec<PathBuf> = s
            .lines()
            .filter(|l| !l.trim().is_empty())
            .map(PathBuf::from)
            .collect();
        match !paths.is_empty() {
            true => Ok(Self::from(paths)),
            false => Err(C2RError::new(Kind::Io, Reason::Invalid("input"), Some("No valid paths".to_string()))),
        }
    }
}

impl Debug for Paths {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_struct("PathVec")
            .field("paths", &self.paths)
            .field("max_depth", &self.max_depth)
            .field("min_depth", &self.min_depth)
            .finish()
    }
}

impl Display for Paths {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        for (i, path) in self.paths.iter().enumerate() {
            if i > 0 {
                write!(f, "\n")?;
            }
            write!(f, "{}", path.display())?;
        }
        Ok(())
    }
}

const SIGNIFICAND_SIZE: isize = 64;
const DP_SIGNIFICAND_SIZE: isize = 52;
const DP_EXPONENT_BIAS: isize = 0x3FF + DP_SIGNIFICAND_SIZE;
const DP_MIN_EXPONENT: isize = -DP_EXPONENT_BIAS;
const DP_EXPONENT_MASK: u64 = 0x7FF0000000000000;
const DP_SIGNIFICAND_MASK: u64 = 0x000FFFFFFFFFFFFF;
const DP_HIDDEN_BIT: u64 = 0x0010000000000000;

#[derive(Copy, Clone, Debug)]
pub struct FramePointer {
    pub f: u64,
    pub e: isize,
}

impl FramePointer {
    pub fn new(f: u64, e: isize) -> Self {
        FramePointer { f, e }
    }
    pub fn from_f64(d: f64) -> Self {
        let u: u64 = f64::to_bits(d);

        let biased_e = ((u & DP_EXPONENT_MASK) >> DP_SIGNIFICAND_SIZE) as isize;
        let significand = u & DP_SIGNIFICAND_MASK;
        match biased_e != 0 {
            true => FramePointer {
                f: significand + DP_HIDDEN_BIT,
                e: biased_e - DP_EXPONENT_BIAS,
            },
            false => FramePointer {
                f: significand,
                e: DP_MIN_EXPONENT + 1,
            },
        }
    }

    pub fn normalize(self) -> FramePointer {
        let mut res = self;
        while (res.f & (1u64 << 63)) == 0 {
            res.f <<= 1;
            res.e -= 1;
        }
        res
    }
    pub fn normalize_boundary(self) -> FramePointer {
        let mut res = self;
        while (res.f & DP_HIDDEN_BIT << 1) == 0 {
            res.f <<= 1;
            res.e -= 1;
        }
        res.f <<= SIGNIFICAND_SIZE - DP_SIGNIFICAND_SIZE - 2;
        res.e -= SIGNIFICAND_SIZE - DP_SIGNIFICAND_SIZE - 2;
        res
    }

    pub fn normalized_boundaries(self) -> (FramePointer, FramePointer) {
        let pl = FramePointer::new((self.f << 1) + 1, self.e - 1).normalize_boundary();
        let mut mi = match self.f == DP_HIDDEN_BIT {
            true => FramePointer::new((self.f << 2) - 1, self.e - 2),
            false => FramePointer::new((self.f << 1) - 1, self.e - 1),
        };
        mi.f <<= mi.e - pl.e;
        mi.e = pl.e;
        (mi, pl)
    }
}

impl Sub for FramePointer {
    type Output = FramePointer;
    fn sub(self, rhs: FramePointer) -> FramePointer {
        FramePointer {
            f: self.f - rhs.f,
            e: self.e,
        }
    }
}

impl ops::Mul for FramePointer {
    type Output = FramePointer;
    fn mul(self, rhs: FramePointer) -> FramePointer {
        let m32 = 0xFFFFFFFFu64;
        let a = self.f >> 32;
        let b = self.f & m32;
        let c = rhs.f >> 32;
        let d = rhs.f & m32;
        let ac = a * c;
        let bc = b * c;
        let ad = a * d;
        let bd = b * d;
        let mut tmp = (bd >> 32) + (ad & m32) + (bc & m32);
        tmp += 1u64 << 31; // multiply round
        FramePointer {
            f: ac + (ad >> 32) + (bc >> 32) + (tmp >> 32),
            e: self.e + rhs.e + 64,
        }
    }
}

fn get_cached_power_by_index(index: usize) -> FramePointer {
    // 10^-348, 10^-340, ..., 10^340
    static CACHED_POWERS_F: [u64; 87] = [
        0xfa8fd5a0081c0288,
        0xbaaee17fa23ebf76,
        0x8b16fb203055ac76,
        0xcf42894a5dce35ea,
        0x9a6bb0aa55653b2d,
        0xe61acf033d1a45df,
        0xab70fe17c79ac6ca,
        0xff77b1fcbebcdc4f,
        0xbe5691ef416bd60c,
        0x8dd01fad907ffc3c,
        0xd3515c2831559a83,
        0x9d71ac8fada6c9b5,
        0xea9c227723ee8bcb,
        0xaecc49914078536d,
        0x823c12795db6ce57,
        0xc21094364dfb5637,
        0x9096ea6f3848984f,
        0xd77485cb25823ac7,
        0xa086cfcd97bf97f4,
        0xef340a98172aace5,
        0xb23867fb2a35b28e,
        0x84c8d4dfd2c63f3b,
        0xc5dd44271ad3cdba,
        0x936b9fcebb25c996,
        0xdbac6c247d62a584,
        0xa3ab66580d5fdaf6,
        0xf3e2f893dec3f126,
        0xb5b5ada8aaff80b8,
        0x87625f056c7c4a8b,
        0xc9bcff6034c13053,
        0x964e858c91ba2655,
        0xdff9772470297ebd,
        0xa6dfbd9fb8e5b88f,
        0xf8a95fcf88747d94,
        0xb94470938fa89bcf,
        0x8a08f0f8bf0f156b,
        0xcdb02555653131b6,
        0x993fe2c6d07b7fac,
        0xe45c10c42a2b3b06,
        0xaa242499697392d3,
        0xfd87b5f28300ca0e,
        0xbce5086492111aeb,
        0x8cbccc096f5088cc,
        0xd1b71758e219652c,
        0x9c40000000000000,
        0xe8d4a51000000000,
        0xad78ebc5ac620000,
        0x813f3978f8940984,
        0xc097ce7bc90715b3,
        0x8f7e32ce7bea5c70,
        0xd5d238a4abe98068,
        0x9f4f2726179a2245,
        0xed63a231d4c4fb27,
        0xb0de65388cc8ada8,
        0x83c7088e1aab65db,
        0xc45d1df942711d9a,
        0x924d692ca61be758,
        0xda01ee641a708dea,
        0xa26da3999aef774a,
        0xf209787bb47d6b85,
        0xb454e4a179dd1877,
        0x865b86925b9bc5c2,
        0xc83553c5c8965d3d,
        0x952ab45cfa97a0b3,
        0xde469fbd99a05fe3,
        0xa59bc234db398c25,
        0xf6c69a72a3989f5c,
        0xb7dcbf5354e9bece,
        0x88fcf317f22241e2,
        0xcc20ce9bd35c78a5,
        0x98165af37b2153df,
        0xe2a0b5dc971f303a,
        0xa8d9d1535ce3b396,
        0xfb9b7cd9a4a7443c,
        0xbb764c4ca7a44410,
        0x8bab8eefb6409c1a,
        0xd01fef10a657842c,
        0x9b10a4e5e9913129,
        0xe7109bfba19c0c9d,
        0xac2820d9623bf429,
        0x80444b5e7aa7cf85,
        0xbf21e44003acdd2d,
        0x8e679c2f5e44ff8f,
        0xd433179d9c8cb841,
        0x9e19db92b4e31ba9,
        0xeb96bf6ebadf77d9,
        0xaf87023b9bf0ee6b,
    ];
    static CACHED_POWERS_E: [i16; 87] = [
        -1220, -1193, -1166, -1140, -1113, -1087, -1060, -1034, -1007, -980, -954, -927, -901,
        -874, -847, -821, -794, -768, -741, -715, -688, -661, -635, -608, -582, -555, -529, -502,
        -475, -449, -422, -396, -369, -343, -316, -289, -263, -236, -210, -183, -157, -130, -103,
        -77, -50, -24, 3, 30, 56, 83, 109, 136, 162, 189, 216, 242, 269, 295, 322, 348, 375, 402,
        428, 455, 481, 508, 534, 561, 588, 614, 641, 667, 694, 720, 747, 774, 800, 827, 853, 880,
        907, 933, 960, 986, 1013, 1039, 1066,
    ];
    FramePointer::new(CACHED_POWERS_F[index], CACHED_POWERS_E[index] as isize)
}

#[inline]
pub fn get_cached_power(e: isize) -> (FramePointer, isize) {
    let dk = (-61 - e) as f64 * 0.30102999566398114f64 + 347f64; // dk must be positive, so can do ceiling in positive
    let mut k = dk as isize;
    if dk - k as f64 > 0.0 {
        k += 1;
    }

    let index = ((k >> 3) + 1) as usize;
    let k = -(-348 + (index << 3) as isize); // decimal exponent no need lookup table

    (get_cached_power_by_index(index), k)
}
#[inline]
unsafe fn grisu_round(buffer: &mut u64, delta: u64, mut rest: u64, ten_kappa: u64, wp_w: u64) {
    while rest < wp_w
        && delta - rest >= ten_kappa
        && (rest + ten_kappa < wp_w || // closer
            wp_w - rest > rest + ten_kappa - wp_w)
    {
        *buffer -= 1;
        rest += ten_kappa;
    }
}

#[inline]
fn count_decimal_digit32(n: u32) -> i16 {
    match n {
        0..=9 => 1,
        10..=99 => 2,
        100..=999 => 3,
        1000..=9999 => 4,
        10000..=99999 => 5,
        100000..=999999 => 6,
        1000000..=9999999 => 7,
        10000000..=99999999 => 8,
        100000000..=999999999 => 9,
        _ => 10,
    }
}

#[inline]
unsafe fn digit_gen(w: FramePointer, mp: FramePointer, mut delta: u64, mut k: i16) -> (u64, i16) {
    static POW10: [u32; 10] = [
        1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000,
    ];
    let one = FramePointer::new(1u64 << -mp.e, mp.e);
    let wp_w = mp - w;
    let mut p1 = (mp.f >> -one.e) as u32;
    let mut p2 = mp.f & (one.f - 1);
    let mut kappa = count_decimal_digit32(p1); // kappa in [0, 9]

    let mut buffer = p1 as u64;

    while kappa > 0 {
        match kappa {
            9 => {
                p1 %= 100000000;
            }
            8 => {
                p1 %= 10000000;
            }
            7 => {
                p1 %= 1000000;
            }
            6 => {
                p1 %= 100000;
            }
            5 => {
                p1 %= 10000;
            }
            4 => {
                p1 %= 1000;
            }
            3 => {
                p1 %= 100;
            }
            2 => {
                p1 %= 10;
            }
            1 => {
                p1 = 0;
            }
            _ => {}
        }
        kappa = kappa.wrapping_sub(1);
        let tmp = ((p1 as u64) << -one.e) + p2;
        if tmp <= delta {
            k += kappa;
            let pow10 = POW10[kappa as usize] as u64;
            buffer /= pow10;

            unsafe { grisu_round(&mut buffer, delta, tmp, pow10 << -one.e, wp_w.f) };
            return (buffer, k);
        }
    }

    loop {
        p2 *= 10;
        delta *= 10;
        let d = (p2 >> -one.e) as u8;
        if d != 0 || buffer != 0 {
            buffer = buffer * 10 + d as u64;
        }
        p2 &= one.f - 1;
        kappa = kappa.wrapping_sub(1);
        if p2 < delta {
            k += kappa;
            let index = -(kappa as isize);

            unsafe {
                grisu_round(
                    &mut buffer,
                    delta,
                    p2,
                    one.f,
                    wp_w.f
                        * match index < 9 {
                            true => POW10[-(kappa as isize) as usize] as u64,
                            false => 0,
                        },
                )
            };
            return (buffer, k);
        }
    }
}

#[inline]
pub fn convert(float: f64) -> (u64, i16) {
    if float == 0.0 {
        return (0, 0);
    }
    unsafe {
        let v = FramePointer::from_f64(float);
        let (w_m, w_p) = v.normalized_boundaries();
        let (c_mk, k) = get_cached_power(w_p.e);
        let w = v.normalize() * c_mk;
        let mut wp = w_p * c_mk;
        let mut wm = w_m * c_mk;
        wm.f += 1;
        wp.f -= 1;

        digit_gen(w, wp, wp.f - wm.f, k as i16)
    }
}
const DEC_DIGITS_LUT: &'static [u8] = b"0001020304050607080910111213141516171819      2021222324252627282930313233343536373839      4041424344454647484950515253545556575859      6061626364656667686970717273747576777879      8081828384858687888990919293949596979899";

const ZEROFILL: &'static [u8] = &[b'0'; 20];

#[inline(always)]
unsafe fn write_num(n: &mut u64, curr: &mut isize, buf_ptr: *mut u8, lut_ptr: *const u8) {
    // eagerly decode 4 digits at a time
    while *n >= 10000 {
        let rem = (*n % 10000) as isize;
        *n /= 10000;

        let d1 = (rem / 100) << 1;
        let d2 = (rem % 100) << 1;
        *curr -= 4;
        unsafe { ptr::copy_nonoverlapping(lut_ptr.offset(d1), buf_ptr.offset(*curr), 2) };
        unsafe { ptr::copy_nonoverlapping(lut_ptr.offset(d2), buf_ptr.offset(*curr + 2), 2) };
    }

    // decode 2 more digits
    if *n >= 100 {
        let d1 = ((*n % 100) << 1) as isize;
        *n /= 100;
        *curr -= 2;
        unsafe { ptr::copy_nonoverlapping(lut_ptr.offset(d1), buf_ptr.offset(*curr), 2) };
    }

    // decode last 1 or 2 digits
    match *n < 10 {
        true => {
            *curr -= 1;
            unsafe { *buf_ptr.offset(*curr) = (*n as u8) + b'0' };
        }
        false => {
            let d1 = (*n << 1) as isize;
            *curr -= 2;
            unsafe { ptr::copy_nonoverlapping(lut_ptr.offset(d1), buf_ptr.offset(*curr), 2) };
        }
    }
}

pub fn write<W: Write>(
    wr: &mut W,
    positive: bool,
    mut n: u64,
    exponent: i16,
) -> Result<()> {
    if !positive {
        wr.write_all(b"-")?;
    }

    if n == 0 {
        wr.write_all(b"0")?;
    }

    const BUF_LEN: usize = 30;
    let mut buf = mem::MaybeUninit::<[u8; BUF_LEN]>::uninit();
    let mut curr = BUF_LEN as isize;
    let buf_ptr = buf.as_mut_ptr() as *mut u8;
    let lut_ptr = DEC_DIGITS_LUT.as_ptr();

    if exponent == 0 {
        unsafe { write_num(&mut n, &mut curr, buf_ptr, lut_ptr) };

        return unsafe {
            wr.write_all(slice::from_raw_parts(
                buf_ptr.offset(curr),
                BUF_LEN - curr as usize,
            )).map_err(|e| C2RError::new(Kind::Io, Reason::Write("failed"), Some(e.to_string())))
        };
    } else if exponent < 0 {
        let mut e = safe_abs(exponent);

        // Decimal number with a fraction that's fully printable
        if e < 18 {
            // eagerly decode 4 digits at a time
            for _ in 0..e >> 2 {
                let rem = (n % 10000) as isize;
                n /= 10000;

                let d1 = (rem / 100) << 1;
                let d2 = (rem % 100) << 1;
                curr -= 4;
                unsafe { ptr::copy_nonoverlapping(lut_ptr.offset(d1), buf_ptr.offset(curr), 2) };
                unsafe {
                    ptr::copy_nonoverlapping(lut_ptr.offset(d2), buf_ptr.offset(curr + 2), 2)
                };
            }

            e &= 3;

            // write the remaining 3, 2 or 1 digits
            if e & 2 == 2 {
                let d1 = ((n % 100) << 1) as isize;
                n /= 100;
                curr -= 2;
                unsafe { ptr::copy_nonoverlapping(lut_ptr.offset(d1), buf_ptr.offset(curr), 2) };
            }

            if e & 1 == 1 {
                curr -= 1;
                unsafe { *buf_ptr.offset(curr) = ((n % 10) as u8) + b'0' };
                n /= 10;
            }

            curr -= 1;
            unsafe { *buf_ptr.offset(curr) = b'.' };

            unsafe { write_num(&mut n, &mut curr, buf_ptr, lut_ptr) };

            return unsafe {
                wr.write_all(slice::from_raw_parts(
                    buf_ptr.offset(curr),
                    BUF_LEN - curr as usize,
                )).map_err(|e| C2RError::new(Kind::Io, Reason::Write("failed"), Some(e.to_string())))
            };
        }

        // Not easily printable, write down fraction, then full number, then exponent

        // Since we move the decimal point right after the first digit, we have to adjust the
        // exponent part. If the number is long enough, this may result in the exponent switching
        // sign from negative to positive - we have to handle this case separately.
        let mut exponent_positive = false;
        match n < 10 {
            true => {
                curr -= 1;
                unsafe { *buf_ptr.offset(curr) = ((n % 10) as u8) + b'0' };
            }
            false => {
                // eagerly decode 4 digits at a time
                while n >= 100000 {
                    let rem = (n % 10000) as isize;
                    n /= 10000;

                    let d1 = (rem / 100) << 1;
                    let d2 = (rem % 100) << 1;
                    curr -= 4;
                    unsafe {
                        ptr::copy_nonoverlapping(lut_ptr.offset(d1), buf_ptr.offset(curr), 2)
                    };
                    unsafe {
                        ptr::copy_nonoverlapping(lut_ptr.offset(d2), buf_ptr.offset(curr + 2), 2)
                    };
                }

                // decode 2 more digits
                if n >= 1000 {
                    let d1 = ((n % 100) << 1) as isize;
                    n /= 100;
                    curr -= 2;
                    unsafe {
                        ptr::copy_nonoverlapping(lut_ptr.offset(d1), buf_ptr.offset(curr), 2)
                    };
                }

                // decode last 1 or 2 digits
                match n < 100 {
                    true => {
                        curr -= 1;
                        unsafe { *buf_ptr.offset(curr) = ((n % 10) as u8) + b'0' };
                        n /= 10;
                    }
                    false => {
                        let d1 = ((n % 100) << 1) as isize;
                        n /= 100;
                        curr -= 2;
                        unsafe {
                            ptr::copy_nonoverlapping(lut_ptr.offset(d1), buf_ptr.offset(curr), 2)
                        };
                    }
                }

                let printed_so_far = BUF_LEN as u16 - curr as u16;

                match printed_so_far <= e {
                    true => {
                        e -= printed_so_far;
                    }
                    false => {
                        // Same as e = |e - printed_so_far|.
                        e = printed_so_far - e;
                        exponent_positive = true;
                    }
                }

                curr -= 1;
                unsafe { *buf_ptr.offset(curr) = b'.' };

                unsafe { write_num(&mut n, &mut curr, buf_ptr, lut_ptr) };
            }
        }

        // Write out the number with a fraction
        unsafe {
            wr.write_all(slice::from_raw_parts(
                buf_ptr.offset(curr),
                BUF_LEN - curr as usize,
            ))
        }?;

        // Omit the 'e' notation for e == 0
        if e == 0 {
            return Ok(());
        }
        // Write the remaining `e` notation, with proper sign
        match exponent_positive {
            true => wr.write_all(b"e+")?,
            false => wr.write_all(b"e-")?,
        }
        return write(wr, true, e as u64, 0);
    }

    // Exponent greater than 0
    unsafe { write_num(&mut n, &mut curr, buf_ptr, lut_ptr) };
    let printed = BUF_LEN - curr as usize;

    // No need for `e` notation, just print out zeroes
    if (printed + exponent as usize) <= 20 {
        unsafe {
            wr.write_all(slice::from_raw_parts(
                buf_ptr.offset(curr),
                BUF_LEN - curr as usize,
            ))
        }?;

        return wr.write_all(&ZEROFILL[..exponent as usize]).map_err(|e| C2RError::new(Kind::Io, Reason::Write("failed"), Some(e.to_string())));
    }

    let mut e = exponent as u64;

    // More than one digit, turn into a fraction
    if printed != 1 {
        unsafe { *buf_ptr.offset(curr - 1) = *buf_ptr.offset(curr) };
        unsafe { *buf_ptr.offset(curr) = b'.' };
        curr -= 1;
        e += (printed as u64) - 1;
    }

    unsafe {
        wr.write_all(slice::from_raw_parts(
            buf_ptr.offset(curr),
            BUF_LEN - curr as usize,
        ))
    }?;
    wr.write_all(b"e")?;
    write(wr, true, e, 0)
}

fn safe_abs(x: i16) -> u16 {
    match x.checked_abs() {
        Some(y) => y as u16,
        None => i16::MAX as u16 + 1u16,
    }
}