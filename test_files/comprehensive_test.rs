use std::ffi::CStr;
use std::os::raw::c_char;

// Global variables
static mut GLOBAL_COUNTER: i32 = 0;
static STATIC_GLOBAL: i32 = 42;
extern "C" {
    static EXTERNAL_VAR: i32;
}

// Function declarations (would be in a header in C)
// In Rust these are implemented directly

// Function with various parameter types
pub fn complex_function(x: i32, y: f32, str_ptr: *const c_char, arr: *const i32, len: usize) -> i32 {
    if x > 0 && y > 0.0 {
        unsafe {
            if !str_ptr.is_null() {
                let c_str = CStr::from_ptr(str_ptr);
                if let Ok(str_slice) = c_str.to_str() {
                    println!("Processing {} with {} elements", str_slice, len);
                }
            }
        }
        return x + y as i32;
    }
    -1
}

// Static function (private in Rust)
fn private_helper(value: i32) -> i32 {
    value * 2
}

// Inline function (Rust inlines automatically, but we can suggest it)
#[inline]
pub fn fast_multiply(a: i32, b: i32) -> i32 {
    a * b
}

// Simple struct definition
#[derive(Debug, Clone, Copy)]
pub struct Point {
    pub x: f64,
    pub y: f64,
}

// Complex struct with nested types
#[derive(Debug, Clone)]
pub struct Rectangle {
    pub top_left: Point,
    pub bottom_right: Point,
    pub color: i32,
    pub label: [u8; 32],
}

// Struct with function pointers
pub struct Calculator {
    pub add: fn(i32, i32) -> i32,
    pub subtract: fn(i32, i32) -> i32,
    pub multiply: fn(f64, f64) -> f64,
}

// Union definition (using union type)
#[repr(C)]
pub union Data {
    pub i: i32,
    pub f: f32,
    pub str: [u8; 16],
}

// Simple enum
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Color {
    Red,
    Green,
    Blue,
}

// Enum with explicit values
#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(i32)]
pub enum Status {
    Error = -1,
    Success = 0,
    Pending = 1,
    Complete = 100,
}

// Type aliases (Rust equivalent of typedef)
pub type Point2D = Point;
pub type BinaryOperation = fn(i32, i32) -> i32;
pub type ColorType = Color;

// Array declarations and initializations
pub static NUMBERS: [i32; 10] = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
pub static FRUITS: [&str; 3] = ["apple", "banana", "orange"];
pub static POINTS: [Point; 3] = [
    Point { x: 0.0, y: 0.0 },
    Point { x: 1.0, y: 1.0 },
    Point { x: 2.0, y: 2.0 },
];

// Function implementations
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}

pub fn print_message(msg: *const c_char) {
    unsafe {
        if !msg.is_null() {
            let c_str = CStr::from_ptr(msg);
            if let Ok(str_slice) = c_str.to_str() {
                println!("Message: {}", str_slice);
            }
        }
    }
}

pub fn calculate_average(values: *const i32, count: i32) -> f64 {
    if values.is_null() || count <= 0 {
        return 0.0;
    }

    let mut sum = 0;
    unsafe {
        for i in 0..count {
            sum += *values.offset(i as isize);
        }
    }

    sum as f64 / count as f64
}

// Function with control flow
pub fn process_data(data: *mut i32, size: i32) -> i32 {
    let mut processed = 0;

    unsafe {
        for i in 0..size {
            let value_ptr = data.offset(i as isize);
            match *value_ptr % 3 {
                0 => {
                    *value_ptr *= 2;
                    processed += 1;
                }
                1 => {
                    *value_ptr += 10;
                    processed += 1;
                }
                2 => {
                    *value_ptr = 0;
                }
                _ => {}
            }
        }
    }

    processed
}

// Function with while and do-while loops
pub fn count_operations(limit: i32) {
    let mut counter = 0;

    // While loop
    unsafe {
        while counter < limit {
            GLOBAL_COUNTER += 1;
            counter += 1;
        }

        // Do-while loop (Rust doesn't have do-while, so we use loop with break)
        loop {
            GLOBAL_COUNTER -= 1;
            if !(GLOBAL_COUNTER > 0 && counter > 0) {
                break;
            }
        }
    }
}

// Function with complex conditionals
pub fn validate_input(input: *const c_char, min_len: i32, max_len: i32) -> i32 {
    if input.is_null() {
        return -1;
    }

    unsafe {
        let c_str = CStr::from_ptr(input);
        let str_slice = match c_str.to_str() {
            Ok(s) => s,
            Err(_) => return -1,
        };

        let len = str_slice.len() as i32;

        if len < min_len || len > max_len {
            return 0;
        }

        // Check for valid characters
        for ch in str_slice.chars() {
            if !ch.is_alphanumeric() {
                return 0;
            }
        }

        1
    }
}

// Main function
fn main() {
    println!("Comprehensive Rust Test Program");

    // Test basic operations
    let result = add(5, 3);
    println!("5 + 3 = {}", result);

    // Test struct usage
    let p1 = Point { x: 10.5, y: 20.3 };
    let rect = Rectangle {
        top_left: Point { x: 0.0, y: 0.0 },
        bottom_right: Point { x: 100.0, y: 50.0 },
        color: 255,
        label: [0; 32], // Initialize with zeros
    };

    // Test array operations
    let test_data = [1, 2, 3, 4, 5];
    let avg = calculate_average(test_data.as_ptr(), 5);
    println!("Average: {:.2}", avg);

    // Test enum usage
    let current_color = Color::Red;
    let app_status = Status::Success;

    println!("Program completed successfully");
}
