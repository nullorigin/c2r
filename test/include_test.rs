// System includes converted to appropriate Rust imports
// These would typically be handled by the dependency management system
// and are generally commented out in our conversion
// use std::io;
// use std::process;

mod myheader;
mod utils {
    pub mod helper;
}

fn main() {
    println!("Testing include handling");
    utils::helper::helper_function();
}
