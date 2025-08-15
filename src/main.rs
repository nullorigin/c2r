// C to Rust converter modules
pub mod error;
pub mod flag;
pub mod function;
pub mod struct_def;
pub mod variable;
pub mod enum_def;
pub mod typedef;
pub mod impl_block;
pub mod parser;
pub mod union;
pub mod constant;
pub mod macro_def;
pub mod common;
pub mod comment;
pub mod pointer;
pub mod types;
pub mod token_parser;

// Re-export primary types
pub use crate::function::*;
pub use crate::struct_def::*;
pub use crate::variable::*;
pub use crate::enum_def::*;
pub use crate::typedef::*;
pub use crate::union::*;
pub use crate::impl_block::*;
pub use crate::parser::*;
pub use crate::error::*;
pub use crate::flag::*;
pub use crate::constant::*;
pub use crate::macro_def::*;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        println!("Usage: c2r <input.c> [output.rs]");
        return;
    }

    let input_path = &args[1];
    let output_path = if args.len() > 2 { &args[2] } else { "output.rs" };

    let mut parser = crate::parser::Parser::new();
    if let Err(e) = parser.read(input_path) {
        eprintln!("Error reading file: {}", e);
        return;
    }

    if let Err(e) = parser.write(output_path) {
        eprintln!("Error writing file: {}", e);
    }
}
