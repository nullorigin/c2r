use std::os::raw::*;
use std::ptr;
use std::mem;

// Test function with while(true) loop converted to Rust 'loop'
fn infinite_loop_test() {
    let mut i = 0;
    loop {
        println!("This is an infinite loop, iteration: {}", i);
        i += 1;
        if i > 10 { break; }
    }
}

// Test function with a C-style for loop converted to range-based loop
fn standard_for_loop() {
    for i in 0..5 {
        println!("Standard for loop: {}", i);
    }
}

// Test function with a C-style for loop with external counter
fn external_counter_for_loop() {
    let mut j: i32;
    for j in (0..11).rev() {
        println!("Countdown: {}", j);
    }
}

// Test function with a do-while loop
fn do_while_test() {
    let mut k = 0;
    loop {
        println!("Do-while loop: {}", k);
        k += 1;
        if !(k < 5) { break; }
    }
}

// Complex for-loop that can't easily be translated to a range-based loop
fn complex_for_loop() {
    let str = "12345";
    let mut sum = 0;
    
    let mut i = 0;
    while i < str.len() {
        sum += str.as_bytes()[i] as i32 - '0' as i32;
        i += 1;
    }
    
    println!("Sum of digits: {}", sum);
}

fn main() {
    infinite_loop_test();
    standard_for_loop();
    external_counter_for_loop();
    do_while_test();
    complex_for_loop();
    
    println!("All loop tests completed!");
}
