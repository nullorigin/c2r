// Simple enum test file equivalent in Rust

// Basic enum
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
}

// Enum with mixed values
#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(i32)]
pub enum Priority {
    Low = 1,
    Medium = 2,
    High = 10,
    Critical = 11,
}

// Function using enums
pub fn print_color(c: Color) {
    match c {
        Color::Red => println!("Red"),
        Color::Green => println!("Green"),
        Color::Blue => println!("Blue"),
    }
}

fn main() {
    let favorite = Color::Red;
    let app_status = Status::Success;

    print_color(favorite);
    println!("Status: {}", app_status as i32);
}
