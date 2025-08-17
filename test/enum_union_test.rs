/**
 * Rust version of enum_union_test.c
 * 
 * This is the expected output after conversion from C to Rust
 * using our EnumHandler and UnionHandler
 */

// Required imports for C compatibility
use std::mem::MaybeUninit;
use std::os::raw::c_void;

// Simple enum example
#[repr(C)]
enum Color {
    RED = 0,
    GREEN = 1,
    BLUE = 2,
}

// Enum with explicit values
#[repr(C)]
enum Status {
    OK = 0,
    ERROR = 1,
    PENDING = 2,
    CANCELLED = 3,
}

// Enum with non-sequential values
#[repr(C)]
enum Flags {
    FLAG_NONE = 0x00,
    FLAG_READ = 0x01,
    FLAG_WRITE = 0x02,
    FLAG_EXECUTE = 0x04,
}

// Enum with mixed assignment
#[repr(C)]
enum Direction {
    UP = 0,
    DOWN = 1,
    LEFT = 10,
    RIGHT = 11,
}

// Enum with explicit integer type
#[repr(u16)]
enum PowerLevel {
    LOW = 1,
    MEDIUM = 500,
    HIGH = 1000,
    MAX = 65535,
}

// Simple union example
#[repr(C)]
union Data {
    i: i32,
    f: f32,
    str: [i8; 20],
}

impl Data {
    pub fn get_i(&self) -> i32 {
        unsafe { self.i }
    }
    
    pub fn set_i(&mut self, val: i32) {
        unsafe { self.i = val; }
    }
    
    pub fn get_f(&self) -> f32 {
        unsafe { self.f }
    }
    
    pub fn set_f(&mut self, val: f32) {
        unsafe { self.f = val; }
    }
    
    pub fn get_str(&self) -> [i8; 20] {
        unsafe { self.str }
    }
    
    pub fn set_str(&mut self, val: [i8; 20]) {
        unsafe { self.str = val; }
    }
}

// Union with nested structs
#[repr(C)]
struct Point {
    x: i32,
    y: i32,
}

#[repr(C)]
struct Record {
    name: [i8; 10],
    id: i32,
}

#[repr(C)]
union ComplexData {
    simple: i32,
    point: Point,
    record: Record,
}

impl ComplexData {
    pub fn get_simple(&self) -> i32 {
        unsafe { self.simple }
    }
    
    pub fn set_simple(&mut self, val: i32) {
        unsafe { self.simple = val; }
    }
    
    pub fn get_point(&self) -> &Point {
        unsafe { &self.point }
    }
    
    pub fn set_point(&mut self, val: Point) {
        unsafe { self.point = val; }
    }
    
    pub fn get_record(&self) -> &Record {
        unsafe { &self.record }
    }
    
    pub fn set_record(&mut self, val: Record) {
        unsafe { self.record = val; }
    }
}

// Packed union with memory layout optimization
#[repr(C)]
#[repr(packed)]
struct Bytes {
    byte1: u8,
    byte2: u8,
    byte3: u8,
    byte4: u8,
}

#[repr(C)]
#[repr(packed(1))]
union PackedData {
    value: u32,
    bytes: Bytes,
}

impl PackedData {
    pub fn get_value(&self) -> u32 {
        unsafe { self.value }
    }
    
    pub fn set_value(&mut self, val: u32) {
        unsafe { self.value = val; }
    }
    
    pub fn get_bytes(&self) -> &Bytes {
        unsafe { &self.bytes }
    }
    
    pub fn set_bytes(&mut self, val: Bytes) {
        unsafe { self.bytes = val; }
    }
}

// Using the enum and union in functions
fn print_color(color: Color) {
    match color {
        Color::RED => println!("Red"),
        Color::GREEN => println!("Green"),
        Color::BLUE => println!("Blue"),
    }
}

fn use_union(data: &mut Data) {
    // Access union members safely
    data.set_i(10);
    println!("Integer: {}", data.get_i());
    
    data.set_f(3.14);
    println!("Float: {}", data.get_f());
    
    // String handling in Rust is more complex than in C
    // This is a simplified example
    let hello = b"Hello\0";
    let mut str_bytes = [0i8; 20];
    for (i, &b) in hello.iter().enumerate() {
        str_bytes[i] = b as i8;
    }
    data.set_str(str_bytes);
    println!("String: {:?}", data.get_str());
}

fn main() {
    // Using enums
    let my_color = Color::BLUE;
    print_color(my_color);
    
    let permissions = Flags::FLAG_READ as i32 | Flags::FLAG_WRITE as i32;
    if permissions & Flags::FLAG_READ as i32 != 0 {
        println!("Read permission is granted");
    }
    
    // Using unions
    let mut data = unsafe { MaybeUninit::<Data>::zeroed().assume_init() };
    use_union(&mut data);
    
    let mut packed = unsafe { MaybeUninit::<PackedData>::zeroed().assume_init() };
    packed.set_value(0x12345678);
    let bytes = packed.get_bytes();
    println!("Bytes: {:02x} {:02x} {:02x} {:02x}", 
             bytes.byte1, bytes.byte2, bytes.byte3, bytes.byte4);
}
