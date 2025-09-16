pub use crate::error::Error;
pub use crate::error::Kind;
pub use crate::error::Reason;
pub use crate::error::Result;
use std::{io::Write, ops::Deref};

/// Result type used by this crate

pub mod iterators {
    /// Iterator over members of `Value::Array`.
    pub type Members<'a> = std::slice::Iter<'a, crate::json::Value>;

    /// Mutable iterator over members of `Value::Array`.
    pub type MembersMut<'a> = std::slice::IterMut<'a, crate::json::Value>;

    /// Iterator over key value pairs of `Value::Object`.
    pub type Entries<'a> = crate::json::Iter<'a>;

    /// Mutable iterator over key value pairs of `Value::Object`.
    pub type EntriesMut<'a> = crate::json::IterMut<'a>;
}
pub type Array = Vec<Value>;

/// Convenience for `Value::from(value)`
pub fn from<T>(value: T) -> Value
where
    T: Into<Value>,
{
    value.into()
}

/// Pretty prints out the value as JSON string.
pub fn stringify<T>(root: T) -> String
where
    T: Into<Value>,
{
    let root: Value = root.into();
    root.dump()
}

/// Pretty prints out the value as JSON string. Second argument is a
/// number of spaces to indent new blocks with.
pub fn stringify_pretty<T>(root: T, spaces: u16) -> String
where
    T: Into<Value>,
{
    let root: Value = root.into();
    root.pretty(spaces)
}

/// Helper macro for creating instances of `Value::Array`.
///
/// ```
/// # #[macro_use] extern crate json;
/// # fn main() {
/// let data = array!["foo", 42, false];
///
/// assert_eq!(data[0], "foo");
/// assert_eq!(data[1], 42);
/// assert_eq!(data[2], false);
///
/// assert_eq!(data.dump(), r#"["foo",42,false]"#);
/// # }
/// ```
#[macro_export]
macro_rules! array {
    [] => (Value::new_array());

    // Handles for token tree items
    [@ITEM($( $i:expr, )*) $item:tt, $( $cont:tt )+] => {
        array!(
            @ITEM($( $i, )* $crate::value!($item), )
            $( $cont )*
        )
    };
    (@ITEM($( $i:expr, )*) $item:tt,) => ({
        array!(@END $( $i, )* $crate::value!($item), )
    });
    (@ITEM($( $i:expr, )*) $item:tt) => ({
        array!(@END $( $i, )* $crate::value!($item), )
    });

    // Handles for expression items
    [@ITEM($( $i:expr, )*) $item:expr, $( $cont:tt )+] => {
        array!(
            @ITEM($( $i, )* $crate::value!($item), )
            $( $cont )*
        )
    };
    (@ITEM($( $i:expr, )*) $item:expr,) => ({
        array!(@END $( $i, )* $crate::value!($item), )
    });
    (@ITEM($( $i:expr, )*) $item:expr) => ({
        array!(@END $( $i, )* $crate::value!($item), )
    });

    // Construct the actual array
    (@END $( $i:expr, )*) => ({
        let size = 0 $( + {let _ = &$i; 1} )*;
        let mut array = Vec::with_capacity(size);

        $(
            array.push($i.into());
        )*

        Value::Array(array)
    });

    // Entry point to the macro
    ($( $cont:tt )+) => {
        array!(@ITEM() $($cont)*)
    };
}

#[macro_export]
/// Helper crate for converting types into `Value`. It's used
/// internally by the `object!` and `array!` macros.
macro_rules! value {
    ( null ) => { Null };
    ( [$( $token:tt )*] ) => {
        array![ $( $token )* ]
    };
    ( {$( $token:tt )*} ) => {
        object!{ $( $token )* }
    };
    { $value:expr } => { $value };
}

/// Helper macro for creating instances of `Value::Object`.
///
/// ```
/// # #[macro_use] extern crate json;
/// # fn main() {
/// let data = object!{
///     foo: 42,
///     bar: false,
/// };
///
/// assert_eq!(data["foo"], 42);
/// assert_eq!(data["bar"], false);
///
/// assert_eq!(data.dump(), r#"{"foo":42,"bar":false}"#);
/// # }
/// ```
#[macro_export]
macro_rules! object {
    // Empty object.
    {} => (Value::new_object());

    // Handles for different types of keys
    (@ENTRY($( $k:expr => $v:expr, )*) $key:ident: $( $cont:tt )*) => {
        object!(@ENTRY($( $k => $v, )*) stringify!($key) => $($cont)*)
    };
    (@ENTRY($( $k:expr => $v:expr, )*) $key:literal: $( $cont:tt )*) => {
        object!(@ENTRY($( $k => $v, )*) $key => $($cont)*)
    };
    (@ENTRY($( $k:expr => $v:expr, )*) [$key:expr]: $( $cont:tt )*) => {
        object!(@ENTRY($( $k => $v, )*) $key => $($cont)*)
    };

    // Handles for token tree values
    (@ENTRY($( $k:expr => $v:expr, )*) $key:expr => $value:tt, $( $cont:tt )+) => {
        $crate::object!(
            @ENTRY($( $k => $v, )* $key => $crate::value!($value), )
            $( $cont )*
        )
    };
    (@ENTRY($( $k:expr => $v:expr, )*) $key:expr => $value:tt,) => ({
        $crate::object!(@END $( $k => $v, )* $key => $crate::value!($value), )
    });
    (@ENTRY($( $k:expr => $v:expr, )*) $key:expr => $value:tt) => ({
        object!(@END $( $k => $v, )* $key => $crate::value!($value), )
    });

    // Handles for expression values
    (@ENTRY($( $k:expr => $v:expr, )*) $key:expr => $value:expr, $( $cont:tt )+) => {
        object!(
            @ENTRY($( $k => $v, )* $key => $crate::value!($value), )
            $( $cont )*
        )
    };
    (@ENTRY($( $k:expr => $v:expr, )*) $key:expr => $value:expr,) => ({
        object!(@END $( $k => $v, )* $key => $crate::value!($value), )
    });

    (@ENTRY($( $k:expr => $v:expr, )*) $key:expr => $value:expr) => ({
        object!(@END $( $k => $v, )* $key => $crate::value!($value), )
    });

    // Construct the actual object
    (@END $( $k:expr => $v:expr, )*) => ({
        let size = 0 $( + {let _ = &$k; 1} )*;
        let mut object = Object::with_capacity(size);

        $(
            object.insert($k, $v.into());
        )*

        Value::Object(object)
    });

    // Entry point to the macro
    ($key:tt: $( $cont:tt )+) => {
        object!(@ENTRY() $key: $($cont)*)
    };

    // Legacy macro
    ($( $k:expr => $v:expr, )*) => {
        object!(@END $( $k => $crate::value!($v), )*)
    };
    ($( $k:expr => $v:expr ),*) => {
        object!(@END $( $k => $crate::value!($v), )*)
    };
}
const QU: u8 = b'"';
const BS: u8 = b'\\';
const BB: u8 = b'b';
const TT: u8 = b't';
const NN: u8 = b'n';
const FF: u8 = b'f';
const RR: u8 = b'r';
const UU: u8 = b'u';
const OO: u8 = 0;

// Look up table for characters that need escaping in a product string
static ESCAPED: [u8; 256] = [
    // 0   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F
    UU, UU, UU, UU, UU, UU, UU, UU, BB, TT, NN, UU, FF, RR, UU, UU, // 0
    UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, // 1
    OO, OO, QU, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, // 2
    OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, // 3
    OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, // 4
    OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, BS, OO, OO, OO, // 5
    OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, // 6
    OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, // 7
    OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, // 8
    OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, // 9
    OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, // A
    OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, // B
    OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, // C
    OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, // D
    OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, // E
    OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, OO, // F
];

/// Default trait for serializing JSONValue into string.
pub trait Generator {
    type W: std::io::Write;

    fn get_writer(&mut self) -> &mut Self::W;

    #[inline(always)]
    fn write(&mut self, slice: &[u8]) -> Result<()> {
        self.get_writer().write_all(slice).map_err(|_| Error::new(Kind::Io, Reason::Write("failed"), "Failed to write to writer".to_string()))
    }

    #[inline(always)]
    fn write_char(&mut self, ch: u8) -> Result<()> {
        self.get_writer().write_all(&[ch]).map_err(|_| Error::new(Kind::Io, Reason::Write("failed"), "Failed to write to writer".to_string()))
    }

    fn write_min(&mut self, slice: &[u8], min: u8) -> Result<()>;

    #[inline(always)]
    fn new_line(&mut self) -> Result<()> {
        Ok(())
    }

    #[inline(always)]
    fn indent(&mut self) {}

    #[inline(always)]
    fn dedent(&mut self) {}

    #[inline(never)]
    fn write_string_complex(&mut self, string: &str, mut start: usize) -> Result<()> {
        let bytes = string.as_bytes();
        self.write(&bytes[..start])?;

        for (index, &ch) in bytes.iter().enumerate().skip(start) {
            let escape = ESCAPED[ch as usize];
            if escape > 0 {
                self.write(&bytes[start..index])?;
                self.write_char(b'\\')?;
                self.write_char(escape)?;
                start = index + 1;

                if escape == b'u' {
                    write!(self.get_writer(), "{:04x}", ch)?;
                }
            }
        }

        self.write(&bytes[start..])?;
        self.write_char(b'"')?;
        Ok(())
    }

    #[inline(always)]
    fn write_string(&mut self, string: &str) -> Result<()> {
        self.write_char(b'"')?;

        let bytes = string.as_bytes();
        for (index, &ch) in bytes.iter().enumerate() {
            if ESCAPED[ch as usize] > 0 {
                return self.write_string_complex(string, index);
            }
        }

        self.write(bytes)?;
        self.write_char(b'"')?;
        Ok(())
    }

    #[inline(always)]
    fn write_number(&mut self, num: &Number) -> Result<()> {
        if num.is_nan() {
            return self.write(b"null");
        }
        let (positive, mantissa, exponent) = num.as_parts();
        crate::util::write(self.get_writer(), positive, mantissa, exponent)
    }

    #[inline(always)]
    fn write_object(&mut self, object: &Object) -> Result<()> {
        self.write_char(b'{')?;

        if object.is_empty() {
            return self.write_char(b'}');
        }

        self.indent();
        let mut first = true;

        for node in object.iter() {
            if first {
                first = false;
            } else {
                self.write_char(b',')?;
            }
            self.new_line()?;
            self.write_string(node.key.as_str())?;
            self.write_min(b": ", b':')?;
            self.write_json(&node.value)?;
        }

        self.dedent();
        self.new_line()?;
        self.write_char(b'}')?;
        Ok(())
    }

    fn write_json(&mut self, json: &Value) -> Result<()> {
        match json {
            Value::Null => self.write(b"null"),
            Value::Short(short) => self.write_string(short.as_str()),
            Value::String(string) => self.write_string(string),
            Value::Number(number) => self.write_number(number),
            Value::Boolean(true) => self.write(b"true"),
            Value::Boolean(false) => self.write(b"false"),
            Value::Array(array) => {
                self.write_char(b'[')?;

                if array.is_empty() {
                    return self.write_char(b']');
                }

                self.indent();
                let mut first = true;

                for item in array.iter() {
                    if first {
                        first = false;
                    } else {
                        self.write_char(b',')?;
                    }
                    self.new_line()?;
                    self.write_json(&item)?;
                }

                self.dedent();
                self.new_line()?;
                self.write_char(b']')
            }
            Value::Object(object) => self.write_object(object),
        }
    }
}

/// In-Memory Generator, this uses a Vec to store the JSON result.
pub struct DumpGenerator {
    code: Vec<u8>,
}

impl DumpGenerator {
    pub fn new() -> Self {
        DumpGenerator {
            code: Vec::with_capacity(1024),
        }
    }

    pub fn consume(self) -> String {
        unsafe { String::from_utf8_unchecked(self.code) }
    }
}

impl Generator for DumpGenerator {
    type W = Vec<u8>;

    #[inline(always)]
    fn get_writer(&mut self) -> &mut Vec<u8> {
        &mut self.code
    }

    #[inline(always)]
    fn write(&mut self, slice: &[u8]) -> Result<()> {
        extend_from_slice(&mut self.code, slice);
        Ok(())
    }

    #[inline(always)]
    fn write_char(&mut self, ch: u8) -> Result<()> {
        self.code.push(ch);
        Ok(())
    }

    #[inline(always)]
    fn write_min(&mut self, _: &[u8], min: u8) -> Result<()> {
        self.code.push(min);
        Ok(())
    }
}

/// Pretty In-Memory Generator, this uses a Vec to store the JSON result and add indent.
pub struct PrettyGenerator {
    code: Vec<u8>,
    dent: u16,
    spaces_per_indent: u16,
}

impl PrettyGenerator {
    pub fn new(spaces: u16) -> Self {
        PrettyGenerator {
            code: Vec::with_capacity(1024),
            dent: 0,
            spaces_per_indent: spaces,
        }
    }

    pub fn consume(self) -> String {
        unsafe { String::from_utf8_unchecked(self.code) }
    }
}

impl Generator for PrettyGenerator {
    type W = Vec<u8>;

    #[inline(always)]
    fn get_writer(&mut self) -> &mut Vec<u8> {
        &mut self.code
    }

    #[inline(always)]
    fn write(&mut self, slice: &[u8]) -> Result<()> {
        extend_from_slice(&mut self.code, slice);
        Ok(())
    }

    #[inline(always)]
    fn write_char(&mut self, ch: u8) -> Result<()> {
        self.code.push(ch);
        Ok(())
    }

    #[inline(always)]
    fn write_min(&mut self, slice: &[u8], _: u8) -> Result<()> {
        extend_from_slice(&mut self.code, slice);
        Ok(())
    }

    fn new_line(&mut self) -> Result<()> {
        self.code.push(b'\n');
        let spaces = self.dent * self.spaces_per_indent;

        // Optimize space writing by reserving capacity once
        let old_len = self.code.len();
        let new_len = old_len + spaces as usize;

        self.code.reserve(spaces as usize);
        unsafe {
            self.code.set_len(new_len);
            let ptr = self.code.as_mut_ptr().add(old_len);
            ptr.write_bytes(b' ', spaces as usize);
        }

        Ok(())
    }

    #[inline(always)]
    fn indent(&mut self) {
        self.dent += 1;
    }

    #[inline(always)]
    fn dedent(&mut self) {
        self.dent -= 1;
    }
}

/// Writer Generator, this uses a custom writer to store the JSON result.
pub struct WriterGenerator<'a, W: 'a + std::io::Write> {
    writer: &'a mut W,
}

impl<'a, W> WriterGenerator<'a, W>
where
    W: 'a + std::io::Write,
{
    pub fn new(writer: &'a mut W) -> Self {
        WriterGenerator { writer }
    }
}

impl<'a, W> Generator for WriterGenerator<'a, W>
where
    W: std::io::Write,
{
    type W = W;

    #[inline(always)]
    fn get_writer(&mut self) -> &mut W {
        &mut self.writer
    }

    #[inline(always)]
    fn write_min(&mut self, _: &[u8], min: u8) -> Result<()> {
        self.writer.write_all(&[min]).map_err(|e| Error::new(Kind::Io, Reason::Write("failed"), e.to_string()))
    }
}

/// Pretty Writer Generator, this uses a custom writer to store the JSON result and add indent.
pub struct PrettyWriterGenerator<'a, W: 'a + std::io::Write> {
    writer: &'a mut W,
    dent: u16,
    spaces_per_indent: u16,
}

impl<'a, W> PrettyWriterGenerator<'a, W>
where
    W: 'a + std::io::Write,
{
    pub fn new(writer: &'a mut W, spaces: u16) -> Self {
        PrettyWriterGenerator {
            writer,
            dent: 0,
            spaces_per_indent: spaces,
        }
    }
}

impl<'a, W> Generator for PrettyWriterGenerator<'a, W>
where
    W: std::io::Write,
{
    type W = W;

    #[inline(always)]
    fn get_writer(&mut self) -> &mut W {
        &mut self.writer
    }

    #[inline(always)]
    fn write_min(&mut self, slice: &[u8], _: u8) -> Result<()> {
        self.writer.write_all(slice).map_err(|e| Error::new(Kind::Io, Reason::Write("failed"), e.to_string()))
    }

    fn new_line(&mut self) -> Result<()> {
        self.write_char(b'\n')?;

        // Use a static buffer for spaces to avoid allocation
        const SPACE_BUFFER_SIZE: usize = 128;
        static SPACES: [u8; SPACE_BUFFER_SIZE] = [b' '; SPACE_BUFFER_SIZE];

        let spaces_needed = (self.dent * self.spaces_per_indent) as usize;
        let mut remaining = spaces_needed;

        while remaining > 0 {
            let to_write = remaining.min(SPACE_BUFFER_SIZE);
            self.writer.write_all(&SPACES[..to_write])?;
            remaining -= to_write;
        }

        Ok(())
    }

    #[inline(always)]
    fn indent(&mut self) {
        self.dent += 1;
    }

    #[inline(always)]
    fn dedent(&mut self) {
        self.dent -= 1;
    }
}

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

// This is not actual max precision, but a threshold at which number parsing
// kicks into checked math.
const MAX_PRECISION: u64 = 576460752303423500;

// How many nested Objects/Arrays are allowed to be parsed
const DEPTH_LIMIT: usize = 512;

// The `Parser` struct keeps track of indexing over our buffer. All niceness
// has been abandoned in favor of raw pointer magic. Does that make you feel
// dirty? _Good._
struct Parser<'a> {
    // Helper buffer for parsing strings that can't be just memcopied from
    // the original source (escaped characters)
    buffer: Vec<u8>,

    // String slice to parse
    source: &'a str,

    // Byte pointer to the slice above
    byte_ptr: *const u8,

    // Current index
    index: usize,

    // Length of the source
    length: usize,
}

// Read a byte from the source.
// Will return an error if there are no more bytes.
macro_rules! expect_byte {
    ($parser:ident) => {{
        if $parser.is_eof() {
            return Err(Error::new(
                Kind::Json,
                Reason::Unexpected("end of JSON"),
                "reached end of JSON source unexpectedly".to_string(),
            ));
        }

        let ch = $parser.read_byte();
        $parser.bump();
        ch
    }};
}

// Expect a sequence of specific bytes in specific order, error otherwise.
// This is useful for reading the 3 JSON identifiers:
//
// - "t" has to be followed by "rue"
// - "f" has to be followed by "alse"
// - "n" has to be followed by "ull"
//
// Anything else is an error.
macro_rules! expect_sequence {
    ($parser:ident, $( $ch:pat ),*) => {
        $(
            match expect_byte!($parser) {
                $ch => {},
                _   => return $parser.unexpected_character(),
            }
        )*
    }
}

// A drop in macro for when we expect to read a byte, but we don't care
// about any whitespace characters that might occur before it.
macro_rules! expect_byte_ignore_whitespace {
    ($parser:ident) => {{
        let mut ch = expect_byte!($parser);

        // Don't go straight for the loop, assume we are in the clear first.
        match ch {
            // whitespace
            9..=13 | 32 => loop {
                match expect_byte!($parser) {
                    9..=13 | 32 => {}
                    next => {
                        ch = next;
                        break;
                    }
                }
            },
            _ => {}
        }

        ch
    }};
}

// Expect to find EOF or just whitespaces leading to EOF after a JSON value
macro_rules! expect_eof {
    ($parser:ident) => {{
        while !$parser.is_eof() {
            match $parser.read_byte() {
                9..=13 | 32 => $parser.bump(),
                _ => {
                    $parser.bump();
                    return $parser.unexpected_character();
                }
            }
        }
    }};
}

// Expect a particular byte to be next. Also available with a variant
// creates a `match` expression just to ease some pain.
macro_rules! expect {
    ($parser:ident, $byte:expr) => ({
        let ch = expect_byte_ignore_whitespace!($parser);

        if ch != $byte {
            return $parser.unexpected_character()
        }
    });

    {$parser:ident $(, $byte:pat => $then:expr )*} => ({
        let ch = expect_byte_ignore_whitespace!($parser);

        match ch {
            $(
                $byte => $then,
            )*
            _ => return $parser.unexpected_character()
        }

    })
}

// Look up table that marks which characters are allowed in their raw
// form in a string.
const X: bool = false;
const O: bool = true;

static ALLOWED: [bool; 256] = [
 // 0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F
    X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, // 0
    X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, // 1
    O, O, X, O, O, O, O, O, O, O, O, O, O, O, O, O, // 2
    O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, // 3
    O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, // 4
    O, O, O, O, O, O, O, O, O, O, O, O, X, O, O, O, // 5
    O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, // 6
    O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, // 7
    O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, // 8
    O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, // 9
    O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, // A
    O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, // B
    O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, // C
    O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, // D
    O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, // E
    O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, // F
];

// Expect a string. This is called after encountering, and consuming, a
// double quote character. This macro has a happy path variant where it
// does almost nothing as long as all characters are allowed (as described
// in the look up table above). If it encounters a closing quote without
// any escapes, it will use a slice straight from the source, avoiding
// unnecessary buffering.
macro_rules! expect_string {
    ($parser:ident) => {{
        let result: &str;
        let start = $parser.index;

        loop {
            let ch = expect_byte!($parser);
            if ALLOWED[ch as usize] {
                continue;
            }
            if ch == b'"' {
                unsafe {
                    let ptr = $parser.byte_ptr.offset(start as isize);
                    let len = $parser.index - 1 - start;
                    result = str::from_utf8_unchecked(std::slice::from_raw_parts(ptr, len));
                }
                break;
            }
            if ch == b'\\' {
                result = $parser.read_complex_string(start)?;
                break;
            }

            return $parser.unexpected_character();
        }

        result
    }};
}

// Invoked after parsing an integer, this will account for fractions and/or
// `e` notation.
fn allow_number_extensions(
    parser: &mut Parser,
    mut num: u64,
    mut e: i16,
    ch: u8,
) -> Result<Number> {
    match ch {
        b'.' => {
            parser.bump();
            expect_fraction(parser, &mut num, &mut e)
        }
        b'e' | b'E' => {
            parser.bump();
            parser.expect_exponent(num, e)
        }
        _ => Ok(num.into()),
    }
}

fn allow_number_extensions_zero(parser: &mut Parser) -> Result<Number> {
    if parser.is_eof() {
        Ok(0.into())
    } else {
        let num = 0;
        let e = 0;
        let ch = parser.read_byte();
        allow_number_extensions(parser, num, e, ch)
    }
}

fn expect_fraction(parser: &mut Parser, num: &mut u64, e: &mut i16) -> Result<Number> {
    let ch = expect_byte!(parser);
    match ch {
        b'0'..=b'9' => {
            if *num < MAX_PRECISION {
                *num = *num * 10 + (ch - b'0') as u64;
                *e -= 1;
            } else {
                match num
                    .checked_mul(10)
                    .and_then(|n| n.checked_add((ch - b'0') as u64))
                {
                    Some(result) => {
                        *num = result;
                        *e -= 1;
                    }
                    None => {}
                }
            }
            // Continue parsing more digits after the decimal point
            loop {
                let ch = parser.read_byte();
                match ch {
                    b'0'..=b'9' => {
                        if *num < MAX_PRECISION {
                            *num = *num * 10 + (ch - b'0') as u64;
                            *e -= 1;
                        } else {
                            match num
                                .checked_mul(10)
                                .and_then(|n| n.checked_add((ch - b'0') as u64))
                            {
                                Some(result) => {
                                    *num = result;
                                    *e -= 1;
                                }
                                None => {}
                            }
                        }
                    }
                    b'e' | b'E' => {
                        parser.bump();
                        return parser.expect_exponent(*num, *e);
                    }
                    _ => {
                        return Ok(unsafe { Number::from_parts_unchecked(true, *num, *e) });
                    }
                }
            }
        }
        b'e' | b'E' => {
            parser.bump();
            return parser.expect_exponent(*num, *e);
        }
        _ => {
            return Ok(unsafe { Number::from_parts_unchecked(true, *num, *e) });
        }
    }
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Parser {
            buffer: Vec::with_capacity(30),
            source: source,
            byte_ptr: source.as_ptr(),
            index: 0,
            length: source.len(),
        }
    }

    // Check if we are at the end of the source.
    #[inline(always)]
    fn is_eof(&mut self) -> bool {
        self.index == self.length
    }

    // Read a byte from the source. Note that this does not increment
    // the index. In few cases (all of them related to number parsing)
    // we want to peek at the byte before doing anything. This will,
    // very very rarely, lead to a situation where the same byte is read
    // twice, but since this operation is using a raw pointer, the cost
    // is virtually irrelevant.
    #[inline(always)]
    fn read_byte(&mut self) -> u8 {
        debug_assert!(self.index < self.length, "Reading out of bounds");

        unsafe { *self.byte_ptr.offset(self.index as isize) }
    }

    // Manually increment the index. Calling `read_byte` and then `bump`
    // is equivalent to consuming a byte on an iterator.
    #[inline(always)]
    fn bump(&mut self) {
        self.index = self.index.wrapping_add(1);
    }

    // So we got an unexpected character, now what? Well, figure out where
    // it is, and throw an error!
    fn unexpected_character<T: Sized>(&mut self) -> Result<T> {
        let at = self.index - 1;

        let ch = self.source[at..]
            .chars()
            .next()
            .expect("Must have a character");

        let (_lineno, col) = self.source[..at]
            .lines()
            .enumerate()
            .last()
            .unwrap_or((0, ""));

        let _colno = col.chars().count();

        Err(Error::new(
            Kind::Json,
            Reason::Unexpected("character"),
            format!("Unexpected character: {}", ch)
        ))
    }

    // Boring
    fn read_hexdec_digit(&mut self) -> Result<u16> {
        let ch = expect_byte!(self);
        Ok(match ch {
            b'0'..=b'9' => ch - b'0',
            b'a'..=b'f' => ch + 10 - b'a',
            b'A'..=b'F' => ch + 10 - b'A',
            _ => return self.unexpected_character(),
        } as u16)
    }

    // Boring
    fn read_hexdec_codepoint(&mut self) -> Result<u16> {
        Ok(self.read_hexdec_digit()? << 12
            | self.read_hexdec_digit()? << 8
            | self.read_hexdec_digit()? << 4
            | self.read_hexdec_digit()?)
    }

    // Oh look, some action. This method reads an escaped unicode
    // sequence such as `\uDEAD` from the string. Except `DEAD` is
    // not a valid codepoint, so it also needs to handle errors...
    fn read_codepoint(&mut self) -> Result<()> {
        let mut buf = [0; 4];
        let codepoint = self.read_hexdec_codepoint()?;

        let unicode = match char::try_from(codepoint as u32) {
            Ok(code) => code,
            // Handle surrogate pairs
            Err(_) => {
                expect_sequence!(self, b'\\', b'u');

                match std::char::decode_utf16(
                    [codepoint, self.read_hexdec_codepoint()?].iter().copied(),
                )
                .next()
                {
                    Some(Ok(code)) => code,
                    _ => {
                        return Err(Error::new(
                            Kind::Json,
                            Reason::Failed("utf8 parsing"),
                            "Failed to parse UTF-8 codepoint".to_string()
                        ));
                    }
                }
            }
        };

        self.buffer
            .extend_from_slice(unicode.encode_utf8(&mut buf).as_bytes());

        Ok(())
    }

    // What's so complex about strings you may ask? Not that much really.
    // This method is called if the `expect_string!` macro encounters an
    // escape. The added complexity is that it will have to use an internal
    // buffer to read all the escaped characters into, before finally
    // producing a usable slice. What it means it that parsing "foo\bar"
    // is whole lot slower than parsing "foobar", as the former suffers from
    // having to be read from source to a buffer and then from a buffer to
    // our target string. Nothing to be done about this, really.
    fn read_complex_string<'b>(&mut self, start: usize) -> Result<&'b str> {
        // Since string slices are returned by this function that are created via pointers into `self.buffer`
        // we shouldn't be clearing or modifying the buffer in consecutive calls to this function. Instead
        // we continuously append bytes to `self.buffer` and keep track of the starting offset of the buffer on each
        // call to this function. Later when creating string slices that point to the contents of this buffer
        // we use this starting offset to make sure that newly created slices point only to the bytes that were
        // appended in the most recent call to this function.
        //
        // Failing to do this can result in the StackBlock `key` values being modified in place later.
        let len = self.buffer.len();
        //self.buffer.clear();
        let mut ch = b'\\';

        // TODO: Use fastwrite here as well
        self.buffer
            .extend_from_slice(&self.source.as_bytes()[start..self.index - 1]);

        loop {
            if ALLOWED[ch as usize] {
                self.buffer.push(ch);
                ch = expect_byte!(self);
                continue;
            }
            match ch {
                b'"' => break,
                b'\\' => {
                    let escaped = expect_byte!(self);
                    let escaped = match escaped {
                        b'u' => {
                            self.read_codepoint()?;
                            ch = expect_byte!(self);
                            continue;
                        }
                        b'"' | b'\\' | b'/' => escaped,
                        b'b' => 0x8,
                        b'f' => 0xC,
                        b't' => b'\t',
                        b'r' => b'\r',
                        b'n' => b'\n',
                        _ => return self.unexpected_character(),
                    };
                    self.buffer.push(escaped);
                }
                _ => return self.unexpected_character(),
            }
            ch = expect_byte!(self);
        }

        // Since the original source is already valid UTF-8, and `\`
        // cannot occur in front of a codepoint > 127, this is safe.
        Ok(unsafe {
            str::from_utf8_unchecked(
                // Because the buffer is stored on the parser, returning it
                // as a slice here freaks out the borrow checker. The compiler
                // can't know that the buffer isn't used till the result
                // of this function is long used and irrelevant. To avoid
                // issues here, we construct a new slice from raw parts, which
                // then has lifetime bound to the outer function scope instead
                // of the parser itself.
                std::slice::from_raw_parts(self.buffer[len..].as_ptr(), self.buffer.len() - len),
            )
        })
    }

    // Big numbers! If the `expect_number!` reaches a point where the decimal
    // mantissa could have overflown the size of u64, it will switch to this
    // control path instead. This method will pick up where the macro started,
    // but instead of continuing to read into the mantissa, it will increment
    // the exponent. Note that no digits are actually read here, as we already
    // exceeded the precision range of f64 anyway.
    fn read_big_number(&mut self, mut num: u64) -> Result<Number> {
        let mut e = 0i16;
        loop {
            if self.is_eof() {
                return Ok(unsafe { Number::from_parts_unchecked(true, num, e) });
            }
            let ch = self.read_byte();
            match ch {
                b'0'..=b'9' => {
                    self.bump();
                    match num
                        .checked_mul(10)
                        .and_then(|num| num.checked_add((ch - b'0') as u64))
                    {
                        Some(result) => num = result,
                        None => {
                            e = e.checked_add(1).ok_or_else(|| {
                                Error::new(
                                    Kind::Json,
                                    Reason::Depth("limit exceeded"),
                                    e.to_string()
                                )
                            })?
                        }
                    }
                }
                b'.' => {
                    self.bump();
                    return Ok(expect_fraction(self, &mut num, &mut e)?);
                }
                b'e' | b'E' => {
                    self.bump();
                    return self.expect_exponent(num, e);
                }
                _ => break,
            }
        }

        Ok(unsafe { Number::from_parts_unchecked(true, num, e) })
    }

    // Called in the rare case that a number with `e` notation has been
    // encountered. This is pretty straight forward, I guess.
    fn expect_exponent(&mut self, num: u64, big_e: i16) -> Result<Number> {
        let mut ch = expect_byte!(self);
        let sign = match ch {
            b'-' => {
                ch = expect_byte!(self);
                -1
            }
            b'+' => {
                ch = expect_byte!(self);
                1
            }
            _ => 1,
        };

        let mut e = match ch {
            b'0'..=b'9' => (ch - b'0') as i16,
            _ => return self.unexpected_character(),
        };

        loop {
            if self.is_eof() {
                break;
            }
            let ch = self.read_byte();
            match ch {
                b'0'..=b'9' => {
                    self.bump();
                    e = e.saturating_mul(10).saturating_add((ch - b'0') as i16);
                }
                _ => break,
            }
        }

        Ok(unsafe { Number::from_parts_unchecked(true, num, big_e.saturating_add(e * sign)) })
    }

    // Parse away!
    fn parse(&mut self) -> std::result::Result<Value, Error> {
        let mut stack = Vec::with_capacity(3);
        let mut ch = expect_byte_ignore_whitespace!(self);

        'parsing: loop {
            let mut value = match ch {
                b'[' => {
                    ch = expect_byte_ignore_whitespace!(self);

                    if ch != b']' {
                        if stack.len() == DEPTH_LIMIT {
                            return Err(Error::new(
                                Kind::Json,
                                Reason::Depth("limit exceeded"),
                                "Exceeded depth limit".to_string()
                            ));
                        }

                        stack.push(StackBlock(Value::Array(Vec::with_capacity(2)), 0));
                        continue 'parsing;
                    }

                    Value::Array(Vec::new())
                }
                b'{' => {
                    ch = expect_byte_ignore_whitespace!(self);

                    if ch != b'}' {
                        if stack.len() == DEPTH_LIMIT {
                            return Err(Error::new(
                                Kind::Json,
                                Reason::Depth("limit exceeded"),
                                "Exceeded depth limit".to_string()
                            ));
                        }

                        let mut object = Object::with_capacity(3);

                        if ch != b'"' {
                            return self.unexpected_character();
                        }

                        let index = object.insert_index(expect_string!(self), Value::Null);
                        expect!(self, b':');

                        stack.push(StackBlock(Value::Object(object), index));

                        ch = expect_byte_ignore_whitespace!(self);

                        continue 'parsing;
                    }

                    Value::Object(Object::new())
                }
                b'"' => expect_string!(self).into(),
                b'0' => Value::Number(allow_number_extensions_zero(self)?),
                b'1'..=b'9' => {
                    let mut num = (ch - b'0') as u64;
                    let result: Number;
                    loop {
                        if num >= MAX_PRECISION {
                            result = self.read_big_number(num)?;
                            break;
                        }
                        if self.is_eof() {
                            result = num.into();
                            break;
                        }
                        let ch = self.read_byte();
                        match ch {
                            b'0'..=b'9' => {
                                self.bump();
                                num = num * 10 + (ch - b'0') as u64;
                            }
                            _ => {
                                let e = 0;
                                result = allow_number_extensions(self, num, e, ch)?;
                                break;
                            }
                        }
                    }
                    Value::Number(result)
                }
                b'-' => {
                    let ch = expect_byte!(self);
                    let n = match ch {
                        b'0' => allow_number_extensions_zero(self)?,
                        b'1'..=b'9' => {
                            let mut num = (ch - b'0') as u64;
                            let result: Number;
                            loop {
                                if num >= MAX_PRECISION {
                                    result = self.read_big_number(num)?;
                                    break;
                                }
                                if self.is_eof() {
                                    result = num.into();
                                    break;
                                }
                                let ch = self.read_byte();
                                match ch {
                                    b'0'..=b'9' => {
                                        self.bump();
                                        num = num * 10 + (ch - b'0') as u64;
                                    }
                                    _ => {
                                        let e = 0;
                                        result = allow_number_extensions(self, num, e, ch)?;
                                        break;
                                    }
                                }
                            }
                            result
                        }
                        _ => return self.unexpected_character(),
                    };
                    Value::Number(-n)
                }
                b't' => {
                    expect_sequence!(self, b'r', b'u', b'e');
                    Value::Boolean(true)
                }
                b'f' => {
                    expect_sequence!(self, b'a', b'l', b's', b'e');
                    Value::Boolean(false)
                }
                b'n' => {
                    expect_sequence!(self, b'u', b'l', b'l');
                    Value::Null
                }
                _ => return self.unexpected_character(),
            };

            'popping: loop {
                match stack.last_mut() {
                    None => {
                        expect_eof!(self);

                        return Ok(value);
                    }

                    Some(&mut StackBlock(Value::Array(ref mut array), _)) => {
                        array.push(value);

                        ch = expect_byte_ignore_whitespace!(self);

                        match ch {
                            b',' => {
                                ch = expect_byte_ignore_whitespace!(self);

                                continue 'parsing;
                            }
                            b']' => {}
                            _ => return self.unexpected_character(),
                        }
                    }

                    Some(&mut StackBlock(Value::Object(ref mut object), ref mut index)) => {
                        object.override_at(*index, value);

                        ch = expect_byte_ignore_whitespace!(self);

                        match ch {
                            b',' => {
                                expect!(self, b'"');
                                *index = object.insert_index(expect_string!(self), Value::Null);
                                expect!(self, b':');

                                ch = expect_byte_ignore_whitespace!(self);

                                continue 'parsing;
                            }
                            b'}' => {}
                            _ => return self.unexpected_character(),
                        }
                    }

                    _ => {
    return Err(Error::new(
        Kind::Json,
        Reason::Internal("invariant"),
        "Parsing failed".to_string()
    ));
}
                }

                value = match stack.pop() {
                    Some(StackBlock(value, _)) => value,
                    None => break 'popping,
                }
            }
        }
    }
}

struct StackBlock(Value, usize);

// All that hard work, and in the end it's just a single function in the API.
#[inline]
pub fn parse(source: &str) -> std::result::Result<Value, Error> {
    Parser::new(source).parse()
}

const KEY_BUF_LEN: usize = 32;
const NULL: Value = Value::Null;

// FNV-1a implementation
//
// While the `Object` is implemented as a binary tree, not a hash table, the
// order in which the tree is balanced makes absolutely no difference as long
// as there is a deterministic left / right ordering with good spread.
// Comparing a hashed `u64` is faster than comparing `&str` or even `&[u8]`,
// for larger objects this yields non-trivial performance benefits.
//
// Additionally this "randomizes" the keys a bit. Should the keys in an object
// be inserted in alphabetical order (an example of such a use case would be
// using an object as a store for entries by ids, where ids are sorted), this
// will prevent the tree from being constructed in a way where the same branch
// of each node is always used, effectively producing linear lookup times. Bad!
//
// Example:
//
// ```
// println!("{}", hash_key(b"10000056"));
// println!("{}", hash_key(b"10000057"));
// println!("{}", hash_key(b"10000058"));
// println!("{}", hash_key(b"10000059"));
// ```
//
// Produces:
//
// ```
// 15043794053238616431  <-- 2nd
// 15043792953726988220  <-- 1st
// 15043800650308385697  <-- 4th
// 15043799550796757486  <-- 3rd
// ```
#[inline]
fn hash_key(key: &[u8]) -> u64 {
    let mut hash: u64 = 0xcbf29ce484222325;
    for byte in key {
        hash ^= *byte as u64;
        hash = hash.wrapping_mul(0x100000001b3);
    }
    hash
}

pub struct Key {
    // Internal buffer to store keys that fit within `KEY_BUF_LEN`,
    // otherwise this field will contain garbage.
    pub buf: [u8; KEY_BUF_LEN],

    // Length of the key in bytes.
    pub len: usize,

    // Cached raw pointer to the key, so that we can cheaply construct
    // a `&str` slice from the `Node` without checking if the key is
    // allocated separately on the heap, or in the `key_buf`.
    pub ptr: *mut u8,

    // A hash of the key, explanation below.
    pub hash: u64,
}

impl Key {
    #[inline]
    fn new(hash: u64, len: usize) -> Self {
        Key {
            buf: [0; KEY_BUF_LEN],
            len: len,
            ptr: std::ptr::null_mut(),
            hash: hash,
        }
    }

    #[inline]
    fn as_bytes(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.ptr, self.len) }
    }

    #[inline]
    fn as_str(&self) -> &str {
        unsafe { std::str::from_utf8_unchecked(self.as_bytes()) }
    }

    // The `buf` on the `Key` can only be filled after the struct
    // is already on the `Vec`'s heap (along with the `Node`).
    // For that reason it's not set in `Key::new` but only after
    // the `Node` is created and allocated.
    #[inline]
    fn attach(&mut self, key: &[u8]) {
        if self.len <= KEY_BUF_LEN {
            unsafe {
                std::ptr::copy_nonoverlapping(key.as_ptr(), self.buf.as_mut_ptr(), self.len);
            }
            self.ptr = self.buf.as_mut_ptr();
        } else {
            let mut heap = key.to_vec();
            self.ptr = heap.as_mut_ptr();
            std::mem::forget(heap);
        }
    }

    // Since we store `Node`s on a vector, it will suffer from reallocation.
    // Whenever that happens, `key.ptr` for short keys will turn into dangling
    // pointers and will need to be re-cached.
    #[inline]
    fn fix_ptr(&mut self) {
        if self.len <= KEY_BUF_LEN {
            self.ptr = self.buf.as_mut_ptr();
        }
    }
}

// Implement `Sync` and `Send` for `Key` despite the use of raw pointers. The struct
// itself should be memory safe.
unsafe impl Sync for Key {}
unsafe impl Send for Key {}

// Because long keys _can_ be stored separately from the `Key` on heap,
// it's essential to clean up the heap allocation when the `Key` is dropped.
impl Drop for Key {
    fn drop(&mut self) {
        unsafe {
            if self.len > KEY_BUF_LEN {
                // Construct a `Vec` out of the `key_ptr`. Since the key is
                // always allocated from a slice, the capacity is equal to length.
                let heap = Vec::from_raw_parts(self.ptr, self.len, self.len);

                // Now that we have an owned `Vec<u8>`, drop it.
                std::mem::drop(heap);
            }
        }
    }
}

// Just like with `Drop`, `Clone` needs a custom implementation that accounts
// for the fact that key _can_ be separately heap allocated.
impl Clone for Key {
    fn clone(&self) -> Self {
        if self.len > KEY_BUF_LEN {
            let mut heap = self.as_bytes().to_vec();
            let ptr = heap.as_mut_ptr();
            std::mem::forget(heap);

            Key {
                buf: [0; KEY_BUF_LEN],
                len: self.len,
                ptr: ptr,
                hash: self.hash,
            }
        } else {
            Key {
                buf: self.buf,
                len: self.len,
                ptr: std::ptr::null_mut(), // requires a `fix_ptr` call after `Node` is on the heap
                hash: self.hash,
            }
        }
    }
}

#[derive(Clone)]
pub struct Node {
    // String-esque key abstraction
    pub key: Key,

    // Value stored.
    pub value: Value,

    // Store vector index pointing to the `Node` for which `key_hash` is smaller
    // than that of this `Node`.
    // Will default to 0 as root node can't be referenced anywhere else.
    pub left: usize,

    // Same as above but for `Node`s with hash larger than this one. If the
    // hash is the same, but keys are different, the lookup will default
    // to the right branch as well.
    pub right: usize,
}

impl std::fmt::Debug for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Debug::fmt(&(self.key.as_str(), &self.value, self.left, self.right), f)
    }
}

impl PartialEq for Node {
    fn eq(&self, other: &Node) -> bool {
        self.key.hash == other.key.hash
            && self.key.as_bytes() == other.key.as_bytes()
            && self.value == other.value
    }
}

impl Node {
    #[inline]
    fn new(value: Value, hash: u64, len: usize) -> Node {
        Node {
            key: Key::new(hash, len),
            value: value,
            left: 0,
            right: 0,
        }
    }
}

/// A binary tree implementation of a string -> `JsonValue` map. You normally don't
/// have to interact with instances of `Object`, much more likely you will be
/// using the `JsonValue::Object` variant, which wraps around this struct.
#[derive(Debug)]
pub struct Object {
    store: *mut Node,
    len: usize,
    capacity: usize,
}

impl Drop for Object {
    fn drop(&mut self) {
        if !self.store.is_null() {
            let layout = std::alloc::Layout::array::<Node>(self.capacity).unwrap();
            unsafe {
                // Drop all values and keys
                for i in 0..self.len {
                    std::ptr::drop_in_place(self.store.add(i));
                }
                std::alloc::dealloc(self.store as *mut u8, layout);
            }
            self.store = std::ptr::null_mut();
            self.len = 0;
            self.capacity = 0;
        }
    }
}

impl Object {
    /// Create a new, empty instance of `Object`. Empty `Object` performs no
    /// allocation until a value is inserted into it.
    #[inline(always)]
    pub fn new() -> Self {
        Object {
            store: std::ptr::null_mut(),
            len: 0,
            capacity: 0,
        }
    }

    /// Create a new `Object` with memory preallocated for `capacity` number
    /// of entries.
    #[inline(always)]
    pub fn with_capacity(capacity: usize) -> Self {
        if capacity == 0 {
            return Self::new();
        }

        let layout = std::alloc::Layout::array::<Node>(capacity).unwrap();
        let store = unsafe { std::alloc::alloc(layout) as *mut Node };

        if store.is_null() {
            std::alloc::handle_alloc_error(layout);
        }

        Object {
            store,
            len: 0,
            capacity,
        }
    }

    #[inline]
    fn index_mut(&mut self, index: usize) -> &mut Node {
        unsafe { &mut *self.store.add(index) }
    }

    #[inline(always)]
    fn add_node(&mut self, key: &[u8], value: Value, hash: u64) -> usize {
        let index = self.len;

        if self.len >= self.capacity {
            let new_capacity = if self.capacity == 0 {
                4
            } else {
                self.capacity * 2
            };
            let new_layout = std::alloc::Layout::array::<Node>(new_capacity).unwrap();
            let new_store = unsafe {
                let ptr = if self.store.is_null() {
                    std::alloc::alloc(new_layout)
                } else {
                    let old_layout = std::alloc::Layout::array::<Node>(self.capacity).unwrap();
                    std::alloc::realloc(self.store as *mut u8, old_layout, new_layout.size())
                };

                if ptr.is_null() {
                    std::alloc::handle_alloc_error(new_layout);
                }

                ptr as *mut Node
            };

            self.store = new_store;
            self.capacity = new_capacity;
        }

        unsafe {
            let node_ptr = self.store.add(index);
            std::ptr::write(node_ptr, Node::new(value, hash, key.len()));
            (*node_ptr).key.attach(key);
        }

        self.len += 1;
        index
    }

    /// Insert a new entry, or override an existing one. Note that `key` has
    /// to be a `&str` slice and not an owned `String`. The internals of
    /// `Object` will handle the heap allocation of the key if needed for
    /// better performance.
    #[inline]
    pub fn insert(&mut self, key: &str, value: Value) {
        self.insert_index(key, value);
    }

    pub(crate) fn insert_index(&mut self, key: &str, value: Value) -> usize {
        let key_bytes = key.as_bytes();
        let hash = hash_key(key_bytes);

        if self.len == 0 {
            let index = self.add_node(key_bytes, value, hash);
            return index;
        }

        let mut current = 0;

        loop {
            let node = self.index_mut(current);

            if hash == node.key.hash && key_bytes == node.key.as_bytes() {
                node.value = value;
                return current;
            } else if hash < node.key.hash {
                if node.left != 0 {
                    current = node.left;
                    continue;
                }
                let index = self.add_node(key_bytes, value, hash);
                self.index_mut(current).left = index;
                return index;
            } else {
                if node.right != 0 {
                    current = node.right;
                    continue;
                }
                let index = self.add_node(key_bytes, value, hash);
                self.index_mut(current).right = index;
                return index;
            }
        }
    }

    #[inline]
    pub(crate) fn override_at(&mut self, index: usize, value: Value) {
        self.index_mut(index).value = value;
    }

    pub fn get(&self, key: &str) -> Option<&Value> {
        if self.len == 0 {
            return None;
        }

        let key_bytes = key.as_bytes();
        let hash = hash_key(key_bytes);
        let mut current = 0;

        loop {
            unsafe {
                let node = &*self.store.add(current);

                if hash == node.key.hash && key_bytes == node.key.as_bytes() {
                    return Some(&node.value);
                } else if hash < node.key.hash {
                    if node.left == 0 {
                        return None;
                    }
                    current = node.left;
                } else {
                    if node.right == 0 {
                        return None;
                    }
                    current = node.right;
                }
            }
        }
    }

    pub fn get_mut(&mut self, key: &str) -> Option<&mut Value> {
        if self.len == 0 {
            return None;
        }

        let key_bytes = key.as_bytes();
        let hash = hash_key(key_bytes);
        let mut current = 0;

        loop {
            unsafe {
                let node = &mut *self.store.add(current);

                if hash == node.key.hash && key_bytes == node.key.as_bytes() {
                    return Some(&mut node.value);
                } else if hash < node.key.hash {
                    if node.left == 0 {
                        return None;
                    }
                    current = node.left;
                } else {
                    if node.right == 0 {
                        return None;
                    }
                    current = node.right;
                }
            }
        }
    }

    /// Attempts to remove the value behind `key`, if successful
    /// will return the `JsonValue` stored behind the `key`.
    pub fn remove(&mut self, key: &str) -> Option<Value> {
        if self.len == 0 {
            return None;
        }

        let key_bytes = key.as_bytes();
        let hash = hash_key(key_bytes);
        let mut index = 0;

        // Try to find the node
        loop {
            unsafe {
                let node = &*self.store.add(index);

                match hash == node.key.hash && key_bytes == node.key.as_bytes() {
                    true => break,
                    false => {
                        match hash < node.key.hash {
                            true => {
                                match node.left == 0 {
                                    true => return None,
                                    false => index = node.left,
                                }
                            }
                            false => {
                                match node.right == 0 {
                                    true => return None,
                                    false => index = node.right,
                                }
                            }
                        }
                    }
                }
            }
        }

        // Removing a node would screw the tree badly, it's easier to just
        // recreate it. This is a very costly operation, but removing nodes
        // in JSON shouldn't happen very often if at all.
        let mut new_object = Object::with_capacity(self.len - 1);
        let mut removed = None;

        for i in 0..self.len {
            unsafe {
                let node = &mut *self.store.add(i);
                if i == index {
                    removed = Some(std::mem::replace(&mut node.value, Value::Null));
                } else {
                    let value = std::mem::replace(&mut node.value, Value::Null);
                    new_object.insert(node.key.as_str(), value);
                }
            }
        }

        std::mem::swap(self, &mut new_object);
        removed
    }

    #[inline(always)]
    pub fn len(&self) -> usize {
        self.len
    }

    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// Wipe the `Object` clear. The capacity will remain untouched.
    pub fn clear(&mut self) {
        self.len = 0;
    }

    #[inline(always)]
    pub fn iter(&self) -> std::slice::Iter<'_, Node> {
        unsafe { std::slice::from_raw_parts(self.store, self.len) }.iter()
    }

    #[inline(always)]
    pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, Node> {
        unsafe { std::slice::from_raw_parts_mut(self.store, self.len) }.iter_mut()
    }

    /// Prints out the value as JSON string.
    pub fn dump(&self) -> String {
        let mut generator = DumpGenerator::new();
        generator.write_object(self).expect("Can't fail");
        generator.consume()
    }

    /// Pretty prints out the value as JSON string. Takes an argument that's
    /// number of spaces to indent new blocks with.
    pub fn pretty(&self, spaces: u16) -> String {
        let mut generator = PrettyGenerator::new(spaces);
        generator.write_object(self).expect("Can't fail");
        generator.consume()
    }
}
impl std::iter::Iterator for Object {
    type Item = Node;

    fn next(&mut self) -> Option<Self::Item> {
        if self.len == 0 {
            return None;
        }
        let node = unsafe { std::ptr::read(self.store) };
        self.len -= 1;
        self.store = unsafe { self.store.add(1) };
        Some(node)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len, Some(self.len))
    }

    fn last(self) -> Option<Self::Item> {
        if self.len == 0 {
            return None;
        }
        unsafe { Some(std::ptr::read(self.store.add(self.len - 1))) }
    }

    fn count(self) -> usize {
        self.len
    }
}

impl std::iter::ExactSizeIterator for Object {
    fn len(&self) -> usize {
        self.len
    }
}

impl std::iter::DoubleEndedIterator for Object {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.len == 0 {
            return None;
        }
        self.len -= 1;
        let node = unsafe { std::ptr::read(self.store.add(self.len)) };
        Some(node)
    }
}
// Custom implementation of `Clone`, as new heap allocation means
// we have to fix key pointers everywhere!
impl Clone for Object {
    fn clone(&self) -> Self {
        if self.len == 0 {
            return Object::new();
        }
        let layout = std::alloc::Layout::array::<Node>(self.capacity).unwrap();
        let new_store = unsafe { std::alloc::alloc(layout) as *mut Node };
        if new_store.is_null() {
            std::alloc::handle_alloc_error(layout);
        }
        unsafe {
            for i in 0..self.len {
                let src = self.store.add(i);
                let dst = new_store.add(i);
                std::ptr::write(dst, (*src).clone());
                // Fix key pointer if needed
                (*dst).key.fix_ptr();
            }
        }
        Object {
            store: new_store,
            len: self.len,
            capacity: self.capacity,
        }
    }
}

impl<K: AsRef<str>, V: Into<Value>> FromIterator<(K, V)> for Object {
    fn from_iter<I: IntoIterator<Item = (K, V)>>(iter: I) -> Self {
        let iter = iter.into_iter();
        let mut object = Object::with_capacity(iter.size_hint().0);

        for (key, value) in iter {
            object.insert(key.as_ref(), value.into());
        }

        object
    }
}

// Because keys can inserted in different order, the safe way to
// compare `Object`s is to iterate over one and check if the other
// has all the same keys.
impl PartialEq for Object {
    fn eq(&self, other: &Object) -> bool {
        if self.len() != other.len() {
            return false;
        }

        for node in self.iter() {
            match other.get(node.key.as_str()) {
                Some(other_val) => {
                    if *other_val != node.value {
                        return false;
                    }
                }
                None => return false,
            }
        }

        true
    }
}

pub struct Iter<'a> {
    inner: std::slice::Iter<'a, Node>,
}

impl<'a> Iter<'a> {
    /// Create an empty iterator that always returns `None`
    pub fn empty() -> Self {
        Iter { inner: [].iter() }
    }
}

impl<'a> Iterator for Iter<'a> {
    type Item = (&'a str, &'a Value);

    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {
        self.inner
            .next()
            .map(|node| (node.key.as_str(), &node.value))
    }
}

impl<'a> DoubleEndedIterator for Iter<'a> {
    #[inline(always)]
    fn next_back(&mut self) -> Option<Self::Item> {
        self.inner
            .next_back()
            .map(|node| (node.key.as_str(), &node.value))
    }
}

impl<'a> ExactSizeIterator for Iter<'a> {
    fn len(&self) -> usize {
        self.inner.len()
    }
}

pub struct IterMut<'a> {
    inner: std::slice::IterMut<'a, Node>,
}

impl<'a> IterMut<'a> {
    /// Create an empty iterator that always returns `None`
    pub fn empty() -> Self {
        IterMut {
            inner: [].iter_mut(),
        }
    }
}

impl<'a> Iterator for IterMut<'a> {
    type Item = (&'a str, &'a mut Value);

    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {
        self.inner
            .next()
            .map(|node| (node.key.as_str(), &mut node.value))
    }
}

impl<'a> DoubleEndedIterator for IterMut<'a> {
    #[inline(always)]
    fn next_back(&mut self) -> Option<Self::Item> {
        self.inner
            .next_back()
            .map(|node| (node.key.as_str(), &mut node.value))
    }
}

impl<'a> ExactSizeIterator for IterMut<'a> {
    fn len(&self) -> usize {
        self.inner.len()
    }
}

/// Implements indexing by `&str` to easily access object members:
///
/// ## Example
///
/// ```
/// # fn main() {
/// let value = object!{
///     foo: "bar"
/// };
///
/// if let JsonValue::Object(object) = value {
///   assert!(object["foo"] == "bar");
/// }
/// # }
/// ```
// TODO: doc
impl<'a> std::ops::Index<&'a str> for Object {
    type Output = Value;

    fn index(&self, index: &str) -> &Value {
        match self.get(index) {
            Some(value) => value,
            _ => &NULL,
        }
    }
}

impl std::ops::Index<String> for Object {
    type Output = Value;

    fn index(&self, index: String) -> &Value {
        self.index(index.deref())
    }
}

impl<'a> std::ops::Index<&'a String> for Object {
    type Output = Value;

    fn index(&self, index: &String) -> &Value {
        self.index(index.deref())
    }
}

/// Implements mutable indexing by `&str` to easily modify object members:
///
/// ## Example
///
/// ```
/// # fn main() {
/// let value = object!{};
///
/// if let JsonValue::Object(mut object) = value {
///   object["foo"] = 42.into();
///
///   assert!(object["foo"] == 42);
/// }
/// # }
/// ```
impl<'a> std::ops::IndexMut<&'a str> for Object {
    fn index_mut(&mut self, index: &str) -> &mut Value {
        if self.get(index).is_none() {
            self.insert(index, Value::Null);
        }
        self.get_mut(index).unwrap()
    }
}

impl std::ops::IndexMut<String> for Object {
    fn index_mut(&mut self, index: String) -> &mut Value {
        self.get_mut(index.deref()).unwrap()
    }
}

impl<'a> std::ops::IndexMut<&'a String> for Object {
    fn index_mut(&mut self, index: &String) -> &mut Value {
        self.get_mut(index.deref()).unwrap()
    }
}

/// NaN value represented in `Number` type. NaN is equal to itself.
pub const NAN: Number = Number {
    category: NAN_MASK,
    mantissa: 0,
    exponent: 0,
};

const NEGATIVE: u8 = 0;
const POSITIVE: u8 = 1;
const NAN_MASK: u8 = !1;

/// Number representation used inside `Value`. You can easily convert
/// the `Number` type into native Rust number types and back, or use the
/// equality operator with another number type.
///
/// ```
/// let foo: Number = 3.14.into();
/// let bar: f64 = foo.into();
///
/// assert_eq!(foo, 3.14);
/// assert_eq!(bar, 3.14);
/// ```
///
/// More often than not you will deal with `Value::Number` variant that
/// wraps around this type, instead of using the methods here directly.
#[derive(Copy, Clone, Debug)]
pub struct Number {
    // A byte describing the sign and NaN-ness of the number.
    //
    // category == 0 (NEGATIVE constant)         -> negative sign
    // category == 1 (POSITIVE constant)         -> positive sign
    // category >  1 (matches NAN_MASK constant) -> NaN
    category: u8,

    // Decimal exponent, analog to `e` notation in string form.
    exponent: i16,

    // Integer base before sing and exponent applied.
    mantissa: u64,
}

impl Number {
    /// Construct a new `Number` from parts. This can't create a NaN value.
    ///
    /// ```
    /// let pi = unsafe { Number::from_parts_unchecked(true, 3141592653589793, -15) };
    ///
    /// assert_eq!(pi, 3.141592653589793);
    /// ```
    ///
    /// While this method is marked unsafe, it doesn't actually perform any unsafe operations.
    /// THe goal of the 'unsafe' is to deter from using this method in favor of its safe equivalent
    /// `from_parts`, at least in context when the associated performance cost is negligible.
    #[inline]
    pub unsafe fn from_parts_unchecked(positive: bool, mantissa: u64, exponent: i16) -> Self {
        Number {
            category: positive as u8,
            exponent: exponent,
            mantissa: mantissa,
        }
    }

    /// Construct a new `Number` from parts, stripping unnecessary trailing zeroes.
    /// This can't create a NaN value.
    ///
    /// ```
    /// let one = Number::from_parts(true, 1000, -3);
    /// let (positive, mantissa, exponent) = one.as_parts();
    ///
    /// assert_eq!(true, positive);
    /// assert_eq!(1, mantissa);
    /// assert_eq!(0, exponent);
    /// ```
    #[inline]
    pub fn from_parts(positive: bool, mut mantissa: u64, mut exponent: i16) -> Self {
        while exponent < 0 && mantissa % 10 == 0 {
            exponent += 1;
            mantissa /= 10;
        }
        unsafe { Number::from_parts_unchecked(positive, mantissa, exponent) }
    }

    /// Reverse to `from_parts` - obtain parts from an existing `Number`.
    ///
    /// ```
    /// let pi = Number::from(3.141592653589793);
    /// let (positive, mantissa, exponent) = pi.as_parts();
    ///
    /// assert_eq!(positive, true);
    /// assert_eq!(mantissa, 3141592653589793);
    /// assert_eq!(exponent, -15);
    /// ```
    #[inline]
    pub fn as_parts(&self) -> (bool, u64, i16) {
        (self.category == POSITIVE, self.mantissa, self.exponent)
    }

    #[inline]
    pub fn is_sign_positive(&self) -> bool {
        self.category == POSITIVE
    }

    #[inline]
    pub fn is_zero(&self) -> bool {
        self.mantissa == 0 && !self.is_nan()
    }

    #[inline]
    pub fn is_nan(&self) -> bool {
        self.category & NAN_MASK != 0
    }

    /// Test if the number is NaN or has a zero value.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.mantissa == 0 || self.is_nan()
    }

    /// Obtain an integer at a fixed decimal point. This is useful for
    /// converting monetary values and doing arithmetic on them without
    /// rounding errors introduced by floating point operations.
    ///
    /// Will return `None` if `Number` is negative or a NaN.
    ///
    /// ```
    /// let price_a = Number::from(5.99);
    /// let price_b = Number::from(7);
    /// let price_c = Number::from(10.2);
    ///
    /// assert_eq!(price_a.as_fixed_point_u64(2), Some(599));
    /// assert_eq!(price_b.as_fixed_point_u64(2), Some(700));
    /// assert_eq!(price_c.as_fixed_point_u64(2), Some(1020));
    /// ```
    pub fn as_fixed_point_u64(&self, point: u16) -> Option<u64> {
        if self.category != POSITIVE {
            return None;
        }

        let e_diff = point as i16 + self.exponent;

        Some(if e_diff == 0 {
            self.mantissa
        } else if e_diff < 0 {
            self.mantissa.wrapping_div(decimal_power(-e_diff as u16))
        } else {
            self.mantissa.wrapping_mul(decimal_power(e_diff as u16))
        })
    }

    /// Analog to `as_fixed_point_u64`, except returning a signed
    /// `i64`, properly handling negative numbers.
    ///
    /// ```
    /// let balance_a = Number::from(-1.49);
    /// let balance_b = Number::from(42);
    ///
    /// assert_eq!(balance_a.as_fixed_point_i64(2), Some(-149));
    /// assert_eq!(balance_b.as_fixed_point_i64(2), Some(4200));
    /// ```
    pub fn as_fixed_point_i64(&self, point: u16) -> Option<i64> {
        if self.is_nan() {
            return None;
        }

        let num = if self.is_sign_positive() {
            self.mantissa as i64
        } else {
            -(self.mantissa as i64)
        };

        let e_diff = point as i16 + self.exponent;

        Some(if e_diff == 0 {
            num
        } else if e_diff < 0 {
            num.wrapping_div(decimal_power(-e_diff as u16) as i64)
        } else {
            num.wrapping_mul(decimal_power(e_diff as u16) as i64)
        })
    }
}

impl PartialEq for Number {
    #[inline]
    fn eq(&self, other: &Number) -> bool {
        if self.is_zero() && other.is_zero() || self.is_nan() && other.is_nan() {
            return true;
        }

        if self.category != other.category {
            return false;
        }

        let e_diff = self.exponent - other.exponent;

        if e_diff == 0 {
            return self.mantissa == other.mantissa;
        } else if e_diff > 0 {
            let power = decimal_power(e_diff as u16);

            self.mantissa.wrapping_mul(power) == other.mantissa
        } else {
            let power = decimal_power(-e_diff as u16);

            self.mantissa == other.mantissa.wrapping_mul(power)
        }
    }
}

impl std::fmt::Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        unsafe {
            if self.is_nan() {
                return f.write_str("nan");
            }
            let (positive, mantissa, exponent) = self.as_parts();
            let mut buf = Vec::new();
            crate::util::write(&mut buf, positive, mantissa, exponent).unwrap();
            f.write_str(&String::from_utf8_unchecked(buf))
        }
    }
}

fn exponentiate_f64(n: f64, e: i16) -> f64 {
    static CACHE_POWERS: [f64; 23] = [
        1.0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10, 1e11, 1e12, 1e13, 1e14, 1e15, 1e16,
        1e17, 1e18, 1e19, 1e20, 1e21, 1e22,
    ];

    if e >= 0 {
        let index = e as usize;

        n * if index < 23 {
            CACHE_POWERS[index]
        } else {
            10f64.powf(index as f64)
        }
    } else {
        let index = -e as usize;

        n / if index < 23 {
            CACHE_POWERS[index]
        } else {
            10f64.powf(index as f64)
        }
    }
}

fn exponentiate_f32(n: f32, e: i16) -> f32 {
    static CACHE_POWERS: [f32; 23] = [
        1.0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10, 1e11, 1e12, 1e13, 1e14, 1e15, 1e16,
        1e17, 1e18, 1e19, 1e20, 1e21, 1e22,
    ];

    if e >= 0 {
        let index = e as usize;

        n * if index < 23 {
            CACHE_POWERS[index]
        } else {
            10f32.powf(index as f32)
        }
    } else {
        let index = -e as usize;

        n / if index < 23 {
            CACHE_POWERS[index]
        } else {
            10f32.powf(index as f32)
        }
    }
}

impl From<Number> for f64 {
    fn from(num: Number) -> f64 {
        if num.is_nan() {
            return f64::NAN;
        }

        let mut n = num.mantissa as f64;
        let mut e = num.exponent;

        if e < -308 {
            n = exponentiate_f64(n, e + 308);
            e = -308;
        }

        let f = exponentiate_f64(n, e);
        if num.is_sign_positive() { f } else { -f }
    }
}

impl From<Number> for f32 {
    fn from(num: Number) -> f32 {
        if num.is_nan() {
            return f32::NAN;
        }

        let mut n = num.mantissa as f32;
        let mut e = num.exponent;

        if e < -127 {
            n = exponentiate_f32(n, e + 127);
            e = -127;
        }

        let f = exponentiate_f32(n, e);
        if num.is_sign_positive() { f } else { -f }
    }
}

impl From<f64> for Number {
    fn from(float: f64) -> Number {
        match float.classify() {
            std::num::FpCategory::Infinite | std::num::FpCategory::Nan => return NAN,
            _ => {}
        }

        if !float.is_sign_positive() {
            let (mantissa, exponent) = crate::util::convert(-float);

            Number::from_parts(false, mantissa, exponent)
        } else {
            let (mantissa, exponent) = crate::util::convert(float);

            Number::from_parts(true, mantissa, exponent)
        }
    }
}

impl From<f32> for Number {
    fn from(float: f32) -> Number {
        match float.classify() {
            std::num::FpCategory::Infinite | std::num::FpCategory::Nan => return NAN,
            _ => {}
        }

        if !float.is_sign_positive() {
            let (mantissa, exponent) = crate::util::convert(-float as f64);

            Number::from_parts(false, mantissa, exponent)
        } else {
            let (mantissa, exponent) = crate::util::convert(float as f64);

            Number::from_parts(true, mantissa, exponent)
        }
    }
}

impl PartialEq<f64> for Number {
    fn eq(&self, other: &f64) -> bool {
        f64::from(*self) == *other
    }
}

impl PartialEq<f32> for Number {
    fn eq(&self, other: &f32) -> bool {
        f32::from(*self) == *other
    }
}

impl PartialEq<Number> for f64 {
    fn eq(&self, other: &Number) -> bool {
        f64::from(*other) == *self
    }
}

impl PartialEq<Number> for f32 {
    fn eq(&self, other: &Number) -> bool {
        f32::from(*other) == *self
    }
}

/// Error type generated when trying to convert a `Number` into an
/// integer of inadequate size.
#[derive(Clone, Copy)]
pub struct NumberOutOfScope;

impl From<std::convert::Infallible> for NumberOutOfScope {
    fn from(_: std::convert::Infallible) -> NumberOutOfScope {
        NumberOutOfScope
    }
}

impl From<std::num::TryFromIntError> for NumberOutOfScope {
    fn from(_: std::num::TryFromIntError) -> NumberOutOfScope {
        NumberOutOfScope
    }
}

macro_rules! impl_unsigned {
    ($( $t:ty ),*) => ($(
        impl From<$t> for Number {
            #[inline]
            fn from(num: $t) -> Number {
                Number {
                    category: POSITIVE,
                    exponent: 0,
                    mantissa: num as u64,
                }
            }
        }

        impl TryFrom<Number> for $t {
            type Error = NumberOutOfScope;

            fn try_from(num: Number) -> std::result::Result<Self, Self::Error> {
                let (positive, mantissa, exponent) = num.as_parts();

                if !positive || exponent != 0 {
                    return Err(NumberOutOfScope);
                }

                std::convert::TryFrom::try_from(mantissa).map_err(Into::into)
            }
        }

        impl_integer!($t);
    )*)
}

macro_rules! impl_signed {
    ($( $t:ty ),*) => ($(
        impl From<$t> for Number {
            fn from(num: $t) -> Number {
                if num < 0 {
                    Number {
                        category: NEGATIVE,
                        exponent: 0,
                        mantissa: -num as u64,
                    }
                } else {
                    Number {
                        category: POSITIVE,
                        exponent: 0,
                        mantissa: num as u64,
                    }
                }
            }
        }

        impl TryFrom<Number> for $t {
            type Error = NumberOutOfScope;

            fn try_from(num: Number) -> std::result::Result<Self, Self::Error> {
                let (positive, mantissa, exponent) = num.as_parts();

                if exponent != 0 {
                    return Err(NumberOutOfScope);
                }

                let mantissa = if positive {
                    mantissa as i64
                } else {
                    -(mantissa as i64)
                };

                TryFrom::try_from(mantissa).map_err(Into::into)
            }
        }

        impl_integer!($t);
    )*)
}

macro_rules! impl_integer {
    ($t:ty) => {
        impl PartialEq<$t> for Number {
            fn eq(&self, other: &$t) -> bool {
                *self == Number::from(*other)
            }
        }

        impl PartialEq<Number> for $t {
            fn eq(&self, other: &Number) -> bool {
                Number::from(*self) == *other
            }
        }
    };
}

impl_signed!(isize, i8, i16, i32, i64);
impl_unsigned!(usize, u8, u16, u32, u64);

impl std::ops::Neg for Number {
    type Output = Number;

    #[inline]
    fn neg(self) -> Number {
        Number {
            category: self.category ^ POSITIVE,
            exponent: self.exponent,
            mantissa: self.mantissa,
        }
    }
}

// Commented out for now - not doing math ops for 0.10.0
// -----------------------------------------------------
//
// impl std::ops::Mul for Number {
//     type Output = Number;

//     #[inline]
//     fn mul(self, other: Number) -> Number {
//         // If either is a NaN, return a NaN
//         if (self.category | other.category) & NAN_MASK != 0 {
//             NAN
//         } else {
//             Number {
//                 // If both signs are the same, xoring will produce 0.
//                 // If they are different, xoring will produce 1.
//                 // Xor again with 1 to get a proper proper sign!
//                 // Xor all the things!                              ^ _ ^

//                 category: self.category ^ other.category ^ POSITIVE,
//                 exponent: self.exponent + other.exponent,
//                 mantissa: self.mantissa * other.mantissa,
//             }
//         }
//     }
// }

// impl std::ops::MulAssign for Number {
//     #[inline]
//     fn mul_assign(&mut self, other: Number) {
//         *self = *self * other;
//     }
// }

#[inline]
fn decimal_power(mut e: u16) -> u64 {
    static CACHED: [u64; 20] = [
        1,
        10,
        100,
        1000,
        10000,
        100000,
        1000000,
        10000000,
        100000000,
        1000000000,
        10000000000,
        100000000000,
        1000000000000,
        10000000000000,
        100000000000000,
        1000000000000000,
        10000000000000000,
        100000000000000000,
        1000000000000000000,
        10000000000000000000,
    ];

    if e < 20 {
        CACHED[e as usize]
    } else {
        let mut pow = 1u64;
        while e >= 20 {
            pow = pow.saturating_mul(CACHED[(e % 20) as usize]);
            e /= 20;
        }

        pow
    }
}
pub const MAX_LEN: usize = 30;

#[derive(Clone, Copy)]
pub struct Short {
    len: u8,
    value: [u8; MAX_LEN],
}

/// A `Short` is a small string, up to `MAX_LEN` bytes, that can be managed without
/// the expensive heap allocation performed for the regular `String` type.
impl Short {
    /// Creates a `Short` from a `&str` slice. This method can cause buffer
    /// overflow if the length of the slice is larger than `MAX_LEN`, which is why
    /// it is marked as `unsafe`.
    ///
    ///
    /// Typically you should avoid creating your own `Short`s, instead create a
    /// `Value` (either using `"foo".into()` or `Value::from("foo")`) out
    /// of a slice. This will automatically decide on `String` or `Short` for you.
    #[inline(always)]
    pub unsafe fn from_slice(slice: &str) -> Self {
        let mut short = Short {
            value: [0; MAX_LEN],
            len: slice.len() as u8,
        };

        unsafe {
            std::ptr::copy_nonoverlapping(slice.as_ptr(), short.value.as_mut_ptr(), slice.len())
        };

        short
    }

    /// Cheaply obtain a `&str` slice out of the `Short`.
    #[inline]
    pub fn as_str(&self) -> &str {
        unsafe {
            std::str::from_utf8_unchecked(std::slice::from_raw_parts(
                self.value.as_ptr(),
                self.len as usize,
            ))
        }
    }
}

impl PartialEq for Short {
    #[inline]
    fn eq(&self, other: &Short) -> bool {
        self.as_str() == other.as_str()
    }
}

impl std::fmt::Debug for Short {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Debug::fmt(self.as_str(), f)
    }
}

impl std::fmt::Display for Short {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_str(), f)
    }
}

/// Implements `Deref` for `Short` means that, just like `String`, you can
/// pass `&Short` to functions that expect `&str` and have the conversion happen
/// automagically. On top of that, all methods present on `&str` can be called on
/// an instance of `Short`.
impl std::ops::Deref for Short {
    type Target = str;

    #[inline(always)]
    fn deref(&self) -> &str {
        self.as_str()
    }
}

impl From<Short> for String {
    fn from(short: Short) -> String {
        String::from(short.as_str())
    }
}

impl PartialEq<str> for Short {
    fn eq(&self, other: &str) -> bool {
        self.as_str().eq(other)
    }
}

impl PartialEq<Short> for str {
    fn eq(&self, other: &Short) -> bool {
        other.as_str().eq(self)
    }
}

impl PartialEq<String> for Short {
    fn eq(&self, other: &String) -> bool {
        self.as_str().eq(other)
    }
}

impl PartialEq<Short> for String {
    fn eq(&self, other: &Short) -> bool {
        other.as_str().eq(self)
    }
}

// This is a private module that contains `PartialEq` and `From` trait
// implementations for `Value`.

macro_rules! implement_eq {
    ($to:ident, $from:ty) => {
        impl PartialEq<$from> for Value {
            fn eq(&self, other: &$from) -> bool {
                match *self {
                    Value::$to(ref value) => value == other,
                    _ => false,
                }
            }
        }

        impl<'a> PartialEq<$from> for &'a Value {
            fn eq(&self, other: &$from) -> bool {
                match **self {
                    Value::$to(ref value) => value == other,
                    _ => false,
                }
            }
        }

        impl PartialEq<Value> for $from {
            fn eq(&self, other: &Value) -> bool {
                match *other {
                    Value::$to(ref value) => value == self,
                    _ => false,
                }
            }
        }
    };
}

macro_rules! implement {
    ($to:ident, $from:ty as num) => {
        impl From<$from> for Value {
            fn from(val: $from) -> Value {
                Value::$to(val.into())
            }
        }

        implement_eq!($to, $from);
    };
    ($to:ident, $from:ty) => {
        impl From<$from> for Value {
            fn from(val: $from) -> Value {
                Value::$to(val)
            }
        }

        implement_eq!($to, $from);
    };
}

impl<'a> From<&'a str> for Value {
    fn from(val: &'a str) -> Value {
        if val.len() <= MAX_LEN {
            Value::Short(unsafe { Short::from_slice(val) })
        } else {
            Value::String(val.into())
        }
    }
}

impl<T: Into<Value>> From<Option<T>> for Value {
    fn from(val: Option<T>) -> Value {
        match val {
            Some(val) => val.into(),
            None => Value::Null,
        }
    }
}

impl<T: Into<Value>> From<Vec<T>> for Value {
    fn from(val: Vec<T>) -> Value {
        Value::Array(val.into_iter().map(Into::into).collect())
    }
}

impl<'a, T: Into<Value> + Clone> From<&'a [T]> for Value {
    fn from(val: &'a [T]) -> Value {
        Value::Array(val.iter().cloned().map(Into::into).collect())
    }
}

impl<K: AsRef<str>, V: Into<Value>> From<std::collections::HashMap<K, V>> for Value {
    fn from(val: std::collections::HashMap<K, V>) -> Value {
        Value::Object(val.into_iter().collect())
    }
}

impl<K: AsRef<str>, V: Into<Value>> From<std::collections::BTreeMap<K, V>> for Value {
    fn from(val: std::collections::BTreeMap<K, V>) -> Value {
        Value::Object(val.into_iter().collect())
    }
}

impl<'a> PartialEq<&'a str> for Value {
    fn eq(&self, other: &&str) -> bool {
        match *self {
            Value::Short(ref value) => value == *other,
            Value::String(ref value) => value == *other,
            _ => false,
        }
    }
}

impl<'a> PartialEq<Value> for &'a str {
    fn eq(&self, other: &Value) -> bool {
        match *other {
            Value::Short(ref value) => value == *self,
            Value::String(ref value) => value == *self,
            _ => false,
        }
    }
}

impl PartialEq<str> for Value {
    fn eq(&self, other: &str) -> bool {
        match *self {
            Value::Short(ref value) => value == other,
            Value::String(ref value) => value == other,
            _ => false,
        }
    }
}

impl<'a> PartialEq<Value> for str {
    fn eq(&self, other: &Value) -> bool {
        match *other {
            Value::Short(ref value) => value == self,
            Value::String(ref value) => value == self,
            _ => false,
        }
    }
}

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

// These are convenience macros for converting `f64` to the `$unsigned` type.
// The macros check that the numbers are representable the target type.
macro_rules! number_to_unsigned {
    ($unsigned:ident, $value:expr, $high:ty) => {
        if $value > $unsigned::MAX as $high {
            None
        } else {
            Some($value as $unsigned)
        }
    };
}

macro_rules! number_to_signed {
    ($signed:ident, $value:expr, $high:ty) => {
        if $value < $signed::MIN as $high || $value > $signed::MAX as $high {
            None
        } else {
            Some($value as $signed)
        }
    };
}

#[derive(Debug, Clone)]
pub enum Value {
    Null,
    Short(Short),
    String(String),
    Number(Number),
    Boolean(bool),
    Object(Object),
    Array(Vec<Value>),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        use self::Value::*;
        match (self, other) {
            (&Null, &Null) => true,
            (&Short(ref a), &Short(ref b)) => a == b,
            (&String(ref a), &String(ref b)) => a == b,
            (&Short(ref a), &String(ref b)) | (&String(ref b), &Short(ref a)) => {
                a.as_str() == b.as_str()
            }
            (&Number(ref a), &Number(ref b)) => a == b,
            (&Boolean(ref a), &Boolean(ref b)) => a == b,
            (&Object(ref a), &Object(ref b)) => a == b,
            (&Array(ref a), &Array(ref b)) => a == b,
            _ => false,
        }
    }
}

impl Eq for Value {}

/// Implements formatting
///
/// ```
/// let data = json::parse(r#"{"url":"https://github.com/"}"#).unwrap();
/// println!("{}", data);
/// println!("{:#}", data);
/// ```
impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if f.alternate() {
            f.write_str(&self.pretty(4))
        } else {
            match *self {
                Value::Short(ref value) => value.fmt(f),
                Value::String(ref value) => value.fmt(f),
                Value::Number(ref value) => value.fmt(f),
                Value::Boolean(ref value) => value.fmt(f),
                Value::Null => f.write_str("null"),
                _ => f.write_str(&self.dump()),
            }
        }
    }
}

impl Value {
    /// Create an empty `Value::Object` instance.
    /// When creating an object with data, consider using the `object!` macro.
    pub fn new_object() -> Value {
        Value::Object(Object::new())
    }

    /// Create an empty `Value::Array` instance.
    /// When creating array with data, consider using the `array!` macro.
    pub fn new_array() -> Value {
        Value::Array(Vec::new())
    }

    /// Prints out the value as JSON string.
    pub fn dump(&self) -> String {
        let mut generator = DumpGenerator::new();
        generator.write_json(self).expect("Can't fail");
        generator.consume()
    }

    /// Pretty prints out the value as JSON string. Takes an argument that's
    /// number of spaces to indent new blocks with.
    pub fn pretty(&self, spaces: u16) -> String {
        let mut generator = PrettyGenerator::new(spaces);
        generator.write_json(self).expect("Can't fail");
        generator.consume()
    }

    /// Writes the JSON as byte stream into an implementor of `std::io::Write`.
    pub fn write<W: std::io::Write>(&self, writer: &mut W) -> Result<()> {
        let mut generator = WriterGenerator::new(writer);
        generator.write_json(self)
    }

    /// Writes the JSON as byte stream into an implementor of `std::io::Write`.
    pub fn write_pretty<W: std::io::Write>(
        &self,
        writer: &mut W,
        spaces: u16,
    ) -> Result<()> {
        let mut generator = PrettyWriterGenerator::new(writer, spaces);
        generator.write_json(self)
    }

    pub fn is_string(&self) -> bool {
        match *self {
            Value::Short(_) => true,
            Value::String(_) => true,
            _ => false,
        }
    }

    pub fn is_number(&self) -> bool {
        match *self {
            Value::Number(_) => true,
            _ => false,
        }
    }

    pub fn is_boolean(&self) -> bool {
        match *self {
            Value::Boolean(_) => true,
            _ => false,
        }
    }

    pub fn is_null(&self) -> bool {
        match *self {
            Value::Null => true,
            _ => false,
        }
    }

    pub fn is_object(&self) -> bool {
        match *self {
            Value::Object(_) => true,
            _ => false,
        }
    }

    pub fn is_array(&self) -> bool {
        match *self {
            Value::Array(_) => true,
            _ => false,
        }
    }

    /// Checks whether the value is empty. Returns true for:
    ///
    /// - empty string (`""`)
    /// - number `0`
    /// - boolean `false`
    /// - null
    /// - empty array (`array![]`)
    /// - empty object (`object!{}`)
    pub fn is_empty(&self) -> bool {
        match *self {
            Value::Null => true,
            Value::Short(ref value) => value.is_empty(),
            Value::String(ref value) => value.is_empty(),
            Value::Number(ref value) => value.is_empty(),
            Value::Boolean(ref value) => !value,
            Value::Array(ref value) => value.is_empty(),
            Value::Object(ref value) => value.is_empty(),
        }
    }

    pub fn as_str(&self) -> Option<&str> {
        match *self {
            Value::Short(ref value) => Some(value),
            Value::String(ref value) => Some(value),
            _ => None,
        }
    }

    pub fn as_number(&self) -> Option<Number> {
        match *self {
            Value::Number(value) => Some(value),
            _ => None,
        }
    }

    pub fn as_f64(&self) -> Option<f64> {
        self.as_number().map(|value| value.into())
    }

    pub fn as_f32(&self) -> Option<f32> {
        self.as_number().map(|value| value.into())
    }

    pub fn as_u64(&self) -> Option<u64> {
        self.as_number().and_then(|value| value.try_into().ok())
    }

    pub fn as_u32(&self) -> Option<u32> {
        self.as_u64()
            .and_then(|value| number_to_unsigned!(u32, value, u64))
    }

    pub fn as_u16(&self) -> Option<u16> {
        self.as_u64()
            .and_then(|value| number_to_unsigned!(u16, value, u64))
    }

    pub fn as_u8(&self) -> Option<u8> {
        self.as_u64()
            .and_then(|value| number_to_unsigned!(u8, value, u64))
    }

    pub fn as_usize(&self) -> Option<usize> {
        self.as_u64()
            .and_then(|value| number_to_unsigned!(usize, value, u64))
    }

    pub fn as_i64(&self) -> Option<i64> {
        self.as_number().and_then(|value| value.try_into().ok())
    }

    pub fn as_i32(&self) -> Option<i32> {
        self.as_i64()
            .and_then(|value| number_to_signed!(i32, value, i64))
    }

    pub fn as_i16(&self) -> Option<i16> {
        self.as_i64()
            .and_then(|value| number_to_signed!(i16, value, i64))
    }

    pub fn as_i8(&self) -> Option<i8> {
        self.as_i64()
            .and_then(|value| number_to_signed!(i8, value, i64))
    }

    pub fn as_isize(&self) -> Option<isize> {
        self.as_i64()
            .and_then(|value| number_to_signed!(isize, value, i64))
    }

    pub fn as_bool(&self) -> Option<bool> {
        match *self {
            Value::Boolean(ref value) => Some(*value),
            _ => None,
        }
    }

    /// Obtain an integer at a fixed decimal point. This is useful for
    /// converting monetary values and doing arithmetic on them without
    /// rounding errors introduced by floating point operations.
    ///
    /// Will return `None` if `Number` called on a value that's not a number,
    /// or if the number is negative or a NaN.
    ///
    /// ```
    /// let price_a = Value::from(5.99);
    /// let price_b = Value::from(7);
    /// let price_c = Value::from(10.2);
    ///
    /// assert_eq!(price_a.as_fixed_point_u64(2), Some(599));
    /// assert_eq!(price_b.as_fixed_point_u64(2), Some(700));
    /// assert_eq!(price_c.as_fixed_point_u64(2), Some(1020));
    /// ```
    pub fn as_fixed_point_u64(&self, point: u16) -> Option<u64> {
        match *self {
            Value::Number(ref value) => value.as_fixed_point_u64(point),
            _ => None,
        }
    }

    /// Analog to `as_fixed_point_u64`, except returning a signed
    /// `i64`, properly handling negative numbers.
    ///
    /// ```
    /// let balance_a = Value::from(-1.49);
    /// let balance_b = Value::from(42);
    ///
    /// assert_eq!(balance_a.as_fixed_point_i64(2), Some(-149));
    /// assert_eq!(balance_b.as_fixed_point_i64(2), Some(4200));
    /// ```
    pub fn as_fixed_point_i64(&self, point: u16) -> Option<i64> {
        match *self {
            Value::Number(ref value) => value.as_fixed_point_i64(point),
            _ => None,
        }
    }

    /// Take over the ownership of the value, leaving `Null` in it's place.
    ///
    /// ## Example
    ///
    /// ```
    /// # fn main() {
    /// let mut data = array!["Foo", 42];
    ///
    /// let first = data[0].take();
    /// let second = data[1].take();
    ///
    /// assert!(first == "Foo");
    /// assert!(second == 42);
    ///
    /// assert!(data[0].is_null());
    /// assert!(data[1].is_null());
    /// # }
    /// ```
    pub fn take(&mut self) -> Value {
        std::mem::replace(self, Value::Null)
    }

    /// Checks that self is a string, returns an owned Rust `String`, leaving
    /// `Null` in it's place.
    ///
    /// - If the contained string is already a heap allocated `String`, then
    /// the ownership is moved without any heap allocation.
    ///
    /// - If the contained string is a `Short`, this will perform a heap
    /// allocation to convert the types for you.
    ///
    /// ## Example
    ///
    /// ```
    /// # fn main() {
    /// let mut data = array!["Hello", "World"];
    ///
    /// let owned = data[0].take_string().expect("Should be a string");
    ///
    /// assert_eq!(owned, "Hello");
    /// assert!(data[0].is_null());
    /// # }
    /// ```
    pub fn take_string(&mut self) -> Option<String> {
        let mut placeholder = Value::Null;

        std::mem::swap(self, &mut placeholder);

        match placeholder {
            Value::Short(short) => return Some(short.into()),
            Value::String(string) => return Some(string),

            // Not a string? Swap the original value back in place!
            _ => std::mem::swap(self, &mut placeholder),
        }

        None
    }

    /// Works on `Value::Array` - pushes a new value to the array.
    pub fn push<T>(&mut self, value: T) -> Result<()>
    where
        T: Into<Value>,
    {
        match *self {
            Value::Array(ref mut vec) => {
                vec.push(value.into());
                Ok(())
            }
            _ => Err(Error::new(
                Kind::Json,
                Reason::Not("an array"),
                "Not an array".to_string(),
            )),
        }
    }

    /// Works on `Value::Array` - remove and return last element from
    /// an array. On failure returns a null.
    pub fn pop(&mut self) -> Value {
        match *self {
            Value::Array(ref mut vec) => vec.pop().unwrap_or(Value::Null),
            _ => Value::Null,
        }
    }

    /// Works on `Value::Array` - checks if the array contains a value
    pub fn contains<T>(&self, item: T) -> bool
    where
        T: PartialEq<Value>,
    {
        match *self {
            Value::Array(ref vec) => vec.iter().any(|member| item == *member),
            _ => false,
        }
    }

    /// Works on `Value::Object` - checks if the object has a key
    pub fn has_key(&self, key: &str) -> bool {
        match *self {
            Value::Object(ref object) => object.get(key).is_some(),
            _ => false,
        }
    }

    /// Returns length of array or object (number of keys), defaults to `0` for
    /// other types.
    pub fn len(&self) -> usize {
        match *self {
            Value::Array(ref vec) => vec.len(),
            Value::Object(ref object) => object.len(),
            _ => 0,
        }
    }

    /// Works on `Value::Array` - returns an iterator over members.
    /// Will return an empty iterator if called on non-array types.
    /// ## Example
    /// ```
    /// let animals = array!["Cat", "Dog", "Snail"];
    /// let mut animals_with_letter_a = Vec::new();
    /// for animal in animals.members() {
    ///     if animal.as_str().unwrap().contains('a') {
    ///         animals_with_letter_a.push(animal);
    ///     }
    /// }
    /// assert_eq!(animals_with_letter_a, vec!["Cat", "Snail"]);
    /// ```
    pub fn members(&self) -> iterators::Members {
        match *self {
            Value::Array(ref vec) => vec.iter(),
            _ => [].iter(),
        }
    }

    /// Works on `Value::Array` - returns a mutable iterator over members.
    /// Will return an empty iterator if called on non-array types.
    pub fn members_mut(&mut self) -> iterators::MembersMut {
        match *self {
            Value::Array(ref mut vec) => vec.iter_mut(),
            _ => [].iter_mut(),
        }
    }

    /// Works on `Value::Object` - returns an iterator over key value pairs.
    /// Will return an empty iterator if called on non-object types.
    /// ## Example
    /// ```
    /// let heights = json::parse(r#"
    /// {
    ///     "Alice": 1.42,
    ///     "Bob": 1.6,
    ///     "Carlos": 2.1
    /// }
    /// "#).unwrap();
    /// let mut total_height = 0.0;
    /// let mut names_with_o: Vec<&str> = Vec::new();
    /// for (name, height) in heights.entries() {
    ///     total_height += height.as_f64().unwrap();
    ///     if name.contains('o') {
    ///         names_with_o.push(name);
    ///     }
    /// }
    /// assert_eq!(total_height, 5.12);
    /// assert_eq!(names_with_o, vec!["Bob", "Carlos"]);
    /// ```
    pub fn entries(&self) -> std::slice::Iter<'_, Node> {
        match *self {
            Value::Object(ref object) => object.iter(),
            _ => [].iter(),
        }
    }

    /// Works on `Value::Object` - returns a mutable iterator over
    /// key value pairs.
    /// Will return an empty iterator if called on non-object types.
    pub fn entries_mut(&mut self) -> std::slice::IterMut<'_, Node> {
        match *self {
            Value::Object(ref mut object) => object.iter_mut(),
            _ => [].iter_mut(),
        }
    }

    /// Works on `Value::Object` - inserts a new entry, or override an existing
    /// one into the object. Note that `key` has to be a `&str` slice and not an owned
    /// `String`. The internals of `Object` will handle the heap allocation of the key
    /// if needed for better performance.
    pub fn insert<T>(&mut self, key: &str, value: T) -> Result<()>
    where
        T: Into<Value>,
    {
        match *self {
            Value::Object(ref mut object) => {
                object.insert(key, value.into());
                Ok(())
            }
            _ => Err(Error::new(
                Kind::Json,
                Reason::Not("an object"),
                "Not an object, cannot insert".to_string(),
            )),
        }
    }

    /// Works on `Value::Object` - remove a key and return the value it held.
    /// If the key was not present, the method is called on anything but an
    /// object, it will return a null.
    pub fn remove(&mut self, key: &str) -> Value {
        match *self {
            Value::Object(ref mut object) => object.remove(key).unwrap_or(Value::Null),
            _ => Value::Null,
        }
    }

    /// Works on `Value::Array` - remove an entry and return the value it held.
    /// If the method is called on anything but an object or if the index is out of bounds, it
    /// will return `JsonValue::Null`.
    pub fn array_remove(&mut self, index: usize) -> Value {
        match *self {
            Value::Array(ref mut vec) => {
                if index < vec.len() {
                    vec.remove(index)
                } else {
                    Value::Null
                }
            }
            _ => Value::Null,
        }
    }

    /// When called on an array or an object, will wipe them clean. When called
    /// on a string will clear the string. Numbers and booleans become null.
    pub fn clear(&mut self) {
        match *self {
            Value::String(ref mut string) => string.clear(),
            Value::Object(ref mut object) => object.clear(),
            Value::Array(ref mut vec) => vec.clear(),
            _ => *self = Value::Null,
        }
    }
}

/// Implements indexing by `usize` to easily access array members:
///
/// ## Example
///
/// ```
/// let mut array = Value::new_array();
///
/// array.push("foo");
///
/// assert!(array[0] == "foo");
/// ```
impl std::ops::Index<usize> for Value {
    type Output = Value;

    fn index(&self, index: usize) -> &Value {
        match *self {
            Value::Array(ref vec) => vec.get(index).unwrap_or(&NULL),
            _ => &NULL,
        }
    }
}

/// Implements mutable indexing by `usize` to easily modify array members:
///
/// ## Example
///
/// ```
/// # fn main() {
/// let mut array = array!["foo", 3.14];
///
/// array[1] = "bar".into();
///
/// assert!(array[1] == "bar");
/// # }
/// ```
impl std::ops::IndexMut<usize> for Value {
    fn index_mut(&mut self, index: usize) -> &mut Value {
        match *self {
            Value::Array(ref mut vec) => {
                let in_bounds = index < vec.len();

                if in_bounds {
                    &mut vec[index]
                } else {
                    vec.push(Value::Null);
                    vec.last_mut().unwrap()
                }
            }
            _ => {
                *self = Value::new_array();
                self.push(Value::Null).unwrap();
                self.index_mut(index)
            }
        }
    }
}

/// Implements indexing by `&str` to easily access object members:
///
/// ## Example
///
/// ```
/// # fn main() {
/// let object = object!{
///     foo: "bar"
/// };
///
/// assert!(object["foo"] == "bar");
/// # }
/// ```
impl<'a> std::ops::Index<&'a str> for Value {
    type Output = Value;

    fn index(&self, index: &str) -> &Value {
        match *self {
            Value::Object(ref object) => &object[index],
            _ => &NULL,
        }
    }
}

impl std::ops::Index<String> for Value {
    type Output = Value;

    fn index(&self, index: String) -> &Value {
        self.index(index.deref())
    }
}

impl<'a> std::ops::Index<&'a String> for Value {
    type Output = Value;

    fn index(&self, index: &String) -> &Value {
        self.index(index.deref())
    }
}

/// Implements mutable indexing by `&str` to easily modify object members:
///
/// ## Example
///
/// ```
/// # fn main() {
/// let mut object = object!{};
///
/// object["foo"] = 42.into();
///
/// assert!(object["foo"] == 42);
/// # }
/// ```
impl<'a> std::ops::IndexMut<&'a str> for Value {
    fn index_mut(&mut self, index: &str) -> &mut Value {
        match *self {
            Value::Object(ref mut object) => &mut object[index],
            _ => {
                *self = Value::new_object();
                self.index_mut(index)
            }
        }
    }
}

impl std::ops::IndexMut<String> for Value {
    fn index_mut(&mut self, index: String) -> &mut Value {
        self.index_mut(index.deref())
    }
}

impl<'a> std::ops::IndexMut<&'a String> for Value {
    fn index_mut(&mut self, index: &String) -> &mut Value {
        self.index_mut(index.deref())
    }
}
