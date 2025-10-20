use core::{debug_assert, option::Option::None};

use crate::{
    expect, expect_byte, expect_byte_ignore_whitespace, expect_eof, expect_sequence, expect_string, json::{
        allow_number_extensions, allow_number_extensions_zero, expect_fraction, Number, Object,
        Value, MAX_PRECISION,
    }, C2RError,
    Kind, Reason,
    Result,
};

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
pub static ESCAPED: [u8; 256] = [
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
// How many nested Objects/Arrays are allowed to be parsed
pub const DEPTH_LIMIT: usize = 512;

// Look up table that marks which characters are allowed in their raw
// form in a string.
const X: bool = false;
const O: bool = true;

pub static ALLOWED: [bool; 256] = [
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

pub struct Parser<'a> {
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
impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Parser {
            buffer: Vec::with_capacity(30),
            source,
            byte_ptr: source.as_ptr(),
            index: 0,
            length: source.len(),
        }
    }

    // Check if we are at the end of the source.
    #[inline(always)]
    pub fn is_eof(&mut self) -> bool {
        self.index == self.length
    }

    // Read a byte from the source. Note that this does not increment
    // the index. In few cases (all of them related to number parsing)
    // we want to peek at the byte before doing anything. This will,
    // very very rarely, lead to a situation where the same byte is read
    // twice, but since this operation is using a raw pointer, the cost
    // is virtually irrelevant.
    #[inline(always)]
    pub fn read_byte(&mut self) -> u8 {
        debug_assert!(self.index < self.length, "Reading out of bounds");

        unsafe { *self.byte_ptr.offset(self.index as isize) }
    }

    // Manually increment the index. Calling `read_byte` and then `bump`
    // is equivalent to consuming a byte on an iterator.
    #[inline(always)]
    pub fn bump(&mut self) {
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

        Err(C2RError::new(
            Kind::Json,
            Reason::Unexpected("character"),
            Some(format!("Unexpected character: {}", ch)),
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
                        return Err(C2RError::new(
                            Kind::Json,
                            Reason::Failed("utf8 parsing"),
                            Some("Failed to parse UTF-8 codepoint".to_string()),
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
                                C2RError::new(
                                    Kind::Json,
                                    Reason::Exceeded("depth limit"),
                                    Some(e.to_string()),
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
    pub fn expect_exponent(&mut self, num: u64, big_e: i16) -> Result<Number> {
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
    fn parse(&mut self) -> Result<Value> {
        let mut stack = Vec::with_capacity(3);
        let mut ch = expect_byte_ignore_whitespace!(self);

        'parsing: loop {
            let mut value = match ch {
                b'[' => {
                    ch = expect_byte_ignore_whitespace!(self);

                    if ch != b']' {
                        if stack.len() == DEPTH_LIMIT {
                            return Err(C2RError::new(
                                Kind::Json,
                                Reason::Exceeded("depth limit"),
                                Some("Exceeded depth limit".to_string()),
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
                            return Err(C2RError::new(
                                Kind::Json,
                                Reason::Exceeded("depth limit"),
                                Some("Exceeded depth limit".to_string()),
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
                b'"' => Value::String(expect_string!(self).to_string()),
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
                        return Err(C2RError::new(
                            Kind::Json,
                            Reason::Internal("invariant"),
                            Some("Parsing failed".to_string()),
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
pub fn parse(source: &str) -> Result<Value> {
    Parser::new(source).parse()
}
