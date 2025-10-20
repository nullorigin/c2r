pub mod generator;
pub mod key;
pub mod macros;
pub mod node;
pub mod num;
pub mod obj;
pub mod parsing;
pub mod short;
pub mod value;
pub use crate::implement;
pub use crate::implement_eq;

pub use crate::json::generator::*;
pub use crate::json::key::*;
pub use crate::json::node::*;
pub use crate::json::num::*;
pub use crate::json::obj::*;
pub use crate::json::parsing::*;
pub use crate::json::short::*;
pub use crate::json::value::*;

implement!(String, String);
implement!(Number, isize as num);
implement!(Number, usize as num);
implement!(Number, i8 as num);
implement!(Number, i16 as num);
implement!(Number, i32 as num);
implement!(Number, i64 as num);
implement!(Number, u8 as num);
implement!(Number, u16 as num);
implement!(Number, u32 as num);
implement!(Number, u64 as num);
implement!(Number, f32 as num);
implement!(Number, f64 as num);
implement!(Number, Number);
implement!(Object, Object);
implement!(Boolean, bool);

// From: https://github.com/dtolnay/fastwrite/blob/master/src/lib.rs#L68
//
// LLVM is not able to lower `Vec::extend_from_slice` into a memcpy, so this
// helps eke out that last bit of performance.
#[inline]
fn extend_from_slice(dst: &mut Vec<u8>, src: &[u8]) {
    let dst_len = dst.len();
    let src_len = src.len();

    dst.reserve(src_len);

    unsafe {
        // We would have failed if `reserve` overflowed
        dst.set_len(dst_len + src_len);

        std::ptr::copy_nonoverlapping(
            src.as_ptr(),
            dst.as_mut_ptr().offset(dst_len as isize),
            src_len,
        );
    }
}

pub const KEY_BUF_LEN: usize = 32;
