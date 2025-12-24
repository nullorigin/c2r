// Basic struct definition
#[derive(Debug, Clone, Copy)]
pub struct Point {
    pub x: i32,
    pub y: i32,
}

// Struct with different data types
#[derive(Debug, Clone)]
pub struct Person {
    pub name: [u8; 50],
    pub age: i32,
    pub height: f32,
    pub salary: f64,
}

// Nested struct
#[derive(Debug, Clone)]
pub struct Address {
    pub street: [u8; 100],
    pub city: [u8; 50],
    pub zip_code: i32,
}

#[derive(Debug, Clone)]
pub struct Employee {
    pub info: Person,
    pub address: Address,
    pub employee_id: i32,
}

// Function that uses structs
pub fn create_point(x: i32, y: i32) -> Point {
    Point { x, y }
}

pub fn print_point(p: Point) {
    println!("Point: ({}, {})", p.x, p.y);
}

fn main() {
    let origin = Point { x: 0, y: 0 };
    let p1 = create_point(10, 20);

    print_point(origin);
    print_point(p1);
}
