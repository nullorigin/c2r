use std::io::Write;

use crate::{json::{Number, Object, Value,ESCAPED}, C2RError, Kind, Reason, Result};


/// Default trait for serializing JSONValue into string.
pub trait Generator {
    type W: std::io::Write;

    fn get_writer(&mut self) -> &mut Self::W;

    #[inline(always)]
    fn write(&mut self, slice: &[u8]) -> Result<()> {
        self.get_writer().write_all(slice).map_err(|_| C2RError::new(Kind::Io, Reason::Write("failed"), Some("Failed to write to writer".to_string())))
    }

    #[inline(always)]
    fn write_char(&mut self, ch: u8) -> Result<()> {
        self.get_writer().write_all(&[ch]).map_err(|_| C2RError::new(Kind::Io, Reason::Write("failed"), Some("Failed to write to writer".to_string())))
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
        self.writer.write_all(&[min]).map_err(|e| C2RError::new(Kind::Io, Reason::Write("failed"), Some(e.to_string())))
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
        self.writer.write_all(slice).map_err(|e| C2RError::new(Kind::Io, Reason::Write("failed"), Some(e.to_string())))
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