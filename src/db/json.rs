//! JSON Parser/Exporter using Web
//!
//! A JSON implementation that uses Web instead of Value for storage.
//! Provides parsing, serialization, and pretty-printing capabilities.

use crate::db::web::Entry;
use crate::{Error, Kind, Reason, Result};
use std::collections::HashMap;
use std::io::Write;

// ============================================================================
// Constants and Tables
// ============================================================================

const QU: u8 = b'"';
const BS: u8 = b'\\';
const BB: u8 = b'b';
const TT: u8 = b't';
const NN: u8 = b'n';
const FF: u8 = b'f';
const RR: u8 = b'r';
const UU: u8 = b'u';
const OO: u8 = 0;

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

pub const DEPTH_LIMIT: usize = 512;

// ============================================================================
// Parser
// ============================================================================

pub struct JsonParser<'a> {
    buffer: Vec<u8>,
    source: &'a str,
    byte_ptr: *const u8,
    index: usize,
    length: usize,
}

impl<'a> JsonParser<'a> {
    pub fn new(source: &'a str) -> Self {
        JsonParser {
            buffer: Vec::with_capacity(64),
            source,
            byte_ptr: source.as_ptr(),
            index: 0,
            length: source.len(),
        }
    }

    #[inline(always)]
    fn is_eof(&self) -> bool {
        self.index >= self.length
    }

    #[inline(always)]
    fn read_byte(&self) -> u8 {
        debug_assert!(self.index < self.length, "Reading out of bounds");
        unsafe { *self.byte_ptr.add(self.index) }
    }

    #[inline(always)]
    fn bump(&mut self) {
        self.index += 1;
    }

    #[inline(always)]
    fn skip_whitespace(&mut self) {
        while !self.is_eof() {
            match self.read_byte() {
                b' ' | b'\t' | b'\n' | b'\r' => self.bump(),
                _ => break,
            }
        }
    }

    fn unexpected_character<T>(&self) -> Result<T> {
        let ch = if self.index > 0 && self.index <= self.length {
            self.source[self.index - 1..].chars().next().unwrap_or('?')
        } else {
            '?'
        };
        Err(Error::new(
            Kind::Json,
            Reason::Unexpected("character"),
            Some(format!(
                "Unexpected character: '{}' at position {}",
                ch, self.index
            )),
        ))
    }

    fn expect_byte(&mut self) -> Result<u8> {
        if self.is_eof() {
            return Err(Error::new(
                Kind::Json,
                Reason::Unexpected("EOF"),
                Some("Unexpected end of input".to_string()),
            ));
        }
        let b = self.read_byte();
        self.bump();
        Ok(b)
    }
    #[allow(unused)]
    fn expect_byte_skip_ws(&mut self) -> Result<u8> {
        self.skip_whitespace();
        self.expect_byte()
    }

    fn read_hexdec_digit(&mut self) -> Result<u16> {
        let ch = self.expect_byte()?;
        Ok(match ch {
            b'0'..=b'9' => (ch - b'0') as u16,
            b'a'..=b'f' => (ch - b'a' + 10) as u16,
            b'A'..=b'F' => (ch - b'A' + 10) as u16,
            _ => return self.unexpected_character(),
        })
    }

    fn read_hexdec_codepoint(&mut self) -> Result<u16> {
        Ok(self.read_hexdec_digit()? << 12
            | self.read_hexdec_digit()? << 8
            | self.read_hexdec_digit()? << 4
            | self.read_hexdec_digit()?)
    }

    fn read_codepoint(&mut self) -> Result<()> {
        let mut buf = [0u8; 4];
        let codepoint = self.read_hexdec_codepoint()?;

        let unicode = match char::try_from(codepoint as u32) {
            Ok(code) => code,
            Err(_) => {
                // Handle surrogate pairs
                if self.expect_byte()? != b'\\' || self.expect_byte()? != b'u' {
                    return Err(Error::new(
                        Kind::Json,
                        Reason::Failed("utf8 parsing"),
                        Some("Invalid surrogate pair".to_string()),
                    ));
                }
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

    fn read_string(&mut self) -> Result<String> {
        let start = self.index;

        loop {
            if self.is_eof() {
                return Err(Error::new(
                    Kind::Json,
                    Reason::Unexpected("EOF"),
                    Some("Unterminated string".to_string()),
                ));
            }

            let ch = self.read_byte();
            self.bump();

            match ch {
                b'"' => {
                    // Simple string without escapes
                    return Ok(self.source[start..self.index - 1].to_string());
                }
                b'\\' => {
                    // Complex string with escapes
                    return self.read_complex_string(start);
                }
                _ if !ALLOWED[ch as usize] => {
                    return self.unexpected_character();
                }
                _ => continue,
            }
        }
    }

    fn read_complex_string(&mut self, start: usize) -> Result<String> {
        self.buffer.clear();
        self.buffer
            .extend_from_slice(&self.source.as_bytes()[start..self.index - 1]);

        loop {
            let escaped = self.expect_byte()?;
            let escaped = match escaped {
                b'u' => {
                    self.read_codepoint()?;
                    // Continue reading
                    loop {
                        if self.is_eof() {
                            return Err(Error::new(
                                Kind::Json,
                                Reason::Unexpected("EOF"),
                                Some("Unterminated string".to_string()),
                            ));
                        }
                        let ch = self.read_byte();
                        self.bump();
                        match ch {
                            b'"' => return Ok(String::from_utf8_lossy(&self.buffer).to_string()),
                            b'\\' => break, // Handle next escape
                            _ if ALLOWED[ch as usize] => self.buffer.push(ch),
                            _ => return self.unexpected_character(),
                        }
                    }
                    continue;
                }
                b'"' | b'\\' | b'/' => escaped,
                b'b' => 0x08,
                b'f' => 0x0C,
                b't' => b'\t',
                b'r' => b'\r',
                b'n' => b'\n',
                _ => return self.unexpected_character(),
            };
            self.buffer.push(escaped);

            // Continue reading until next escape or end
            loop {
                if self.is_eof() {
                    return Err(Error::new(
                        Kind::Json,
                        Reason::Unexpected("EOF"),
                        Some("Unterminated string".to_string()),
                    ));
                }
                let ch = self.read_byte();
                self.bump();
                match ch {
                    b'"' => return Ok(String::from_utf8_lossy(&self.buffer).to_string()),
                    b'\\' => break, // Handle next escape
                    _ if ALLOWED[ch as usize] => self.buffer.push(ch),
                    _ => return self.unexpected_character(),
                }
            }
        }
    }

    fn read_number(&mut self, first_char: u8, negative: bool) -> Result<Entry> {
        let mut integer: i64 = (first_char - b'0') as i64;
        let mut is_float = false;
        let mut decimal_places = 0u32;
        let mut fraction: f64 = 0.0;

        // Read integer part
        while !self.is_eof() {
            let ch = self.read_byte();
            match ch {
                b'0'..=b'9' => {
                    self.bump();
                    integer = integer
                        .saturating_mul(10)
                        .saturating_add((ch - b'0') as i64);
                }
                b'.' => {
                    self.bump();
                    is_float = true;
                    break;
                }
                b'e' | b'E' => {
                    self.bump();
                    return self.read_exponent(integer as f64, negative);
                }
                _ => break,
            }
        }

        // Read fractional part if present
        if is_float {
            while !self.is_eof() {
                let ch = self.read_byte();
                match ch {
                    b'0'..=b'9' => {
                        self.bump();
                        decimal_places += 1;
                        fraction = fraction * 10.0 + (ch - b'0') as f64;
                    }
                    b'e' | b'E' => {
                        self.bump();
                        let value = integer as f64 + fraction / 10f64.powi(decimal_places as i32);
                        return self.read_exponent(value, negative);
                    }
                    _ => break,
                }
            }

            let mut value = integer as f64 + fraction / 10f64.powi(decimal_places as i32);
            if negative {
                value = -value;
            }
            return Ok(Entry::f64(value));
        }

        // Return integer
        if negative {
            integer = -integer;
        }

        // Choose appropriate integer size
        if integer >= i32::MIN as i64 && integer <= i32::MAX as i64 {
            Ok(Entry::i32(integer as i32))
        } else {
            Ok(Entry::i64(integer))
        }
    }

    fn read_exponent(&mut self, base: f64, negative: bool) -> Result<Entry> {
        let mut exp_negative = false;
        let mut exponent: i32 = 0;

        if !self.is_eof() {
            match self.read_byte() {
                b'-' => {
                    self.bump();
                    exp_negative = true;
                }
                b'+' => {
                    self.bump();
                }
                _ => {}
            }
        }

        while !self.is_eof() {
            let ch = self.read_byte();
            match ch {
                b'0'..=b'9' => {
                    self.bump();
                    exponent = exponent
                        .saturating_mul(10)
                        .saturating_add((ch - b'0') as i32);
                }
                _ => break,
            }
        }

        if exp_negative {
            exponent = -exponent;
        }

        let mut value = base * 10f64.powi(exponent);
        if negative {
            value = -value;
        }
        Ok(Entry::f64(value))
    }

    fn expect_sequence(&mut self, expected: &[u8]) -> Result<()> {
        for &b in expected {
            if self.expect_byte()? != b {
                return self.unexpected_character();
            }
        }
        Ok(())
    }

    pub fn parse(&mut self) -> Result<Entry> {
        self.parse_value()
    }

    fn parse_value(&mut self) -> Result<Entry> {
        self.skip_whitespace();

        if self.is_eof() {
            return Err(Error::new(
                Kind::Json,
                Reason::Unexpected("EOF"),
                Some("Unexpected end of input".to_string()),
            ));
        }

        let ch = self.read_byte();
        self.bump();

        match ch {
            b'"' => Ok(Entry::string(self.read_string()?)),
            b'{' => self.parse_object(),
            b'[' => self.parse_array(),
            b't' => {
                self.expect_sequence(b"rue")?;
                Ok(Entry::bool(true))
            }
            b'f' => {
                self.expect_sequence(b"alse")?;
                Ok(Entry::bool(false))
            }
            b'n' => {
                self.expect_sequence(b"ull")?;
                Ok(Entry::unit())
            }
            b'-' => {
                if self.is_eof() {
                    return self.unexpected_character();
                }
                let ch = self.read_byte();
                self.bump();
                match ch {
                    b'0'..=b'9' => self.read_number(ch, true),
                    _ => self.unexpected_character(),
                }
            }
            b'0'..=b'9' => self.read_number(ch, false),
            _ => self.unexpected_character(),
        }
    }

    fn parse_object(&mut self) -> Result<Entry> {
        let mut map = HashMap::new();

        self.skip_whitespace();

        if !self.is_eof() && self.read_byte() == b'}' {
            self.bump();
            return Ok(Entry::hashmap(map));
        }

        loop {
            self.skip_whitespace();

            // Expect key
            if self.expect_byte()? != b'"' {
                return self.unexpected_character();
            }
            let key = self.read_string()?;

            self.skip_whitespace();

            // Expect colon
            if self.expect_byte()? != b':' {
                return self.unexpected_character();
            }

            // Parse value
            let value = self.parse_value()?;
            map.insert(key, value);

            self.skip_whitespace();

            if self.is_eof() {
                return Err(Error::new(
                    Kind::Json,
                    Reason::Unexpected("EOF"),
                    Some("Unterminated object".to_string()),
                ));
            }

            match self.read_byte() {
                b',' => {
                    self.bump();
                    continue;
                }
                b'}' => {
                    self.bump();
                    return Ok(Entry::hashmap(map));
                }
                _ => return self.unexpected_character(),
            }
        }
    }

    fn parse_array(&mut self) -> Result<Entry> {
        let mut arr = Vec::new();

        self.skip_whitespace();

        if !self.is_eof() && self.read_byte() == b']' {
            self.bump();
            return Ok(Entry::vec(arr));
        }

        loop {
            let value = self.parse_value()?;
            arr.push(value);

            self.skip_whitespace();

            if self.is_eof() {
                return Err(Error::new(
                    Kind::Json,
                    Reason::Unexpected("EOF"),
                    Some("Unterminated array".to_string()),
                ));
            }

            match self.read_byte() {
                b',' => {
                    self.bump();
                    continue;
                }
                b']' => {
                    self.bump();
                    return Ok(Entry::vec(arr));
                }
                _ => return self.unexpected_character(),
            }
        }
    }
}

/// Parse a JSON string into an Web.
pub fn parse(source: &str) -> Result<Entry> {
    JsonParser::new(source).parse()
}

// ============================================================================
// Generator / Exporter
// ============================================================================

pub trait JsonGenerator {
    fn write(&mut self, slice: &[u8]) -> Result<()>;
    fn write_char(&mut self, ch: u8) -> Result<()>;
    fn new_line(&mut self) -> Result<()> {
        Ok(())
    }
    fn indent(&mut self) {}
    fn dedent(&mut self) {}
    fn write_min(&mut self, full: &[u8], min: u8) -> Result<()>;

    fn write_string(&mut self, s: &str) -> Result<()> {
        self.write_char(b'"')?;

        let bytes = s.as_bytes();
        let mut start = 0;

        for (i, &ch) in bytes.iter().enumerate() {
            let escape = ESCAPED[ch as usize];
            if escape > 0 {
                self.write(&bytes[start..i])?;
                self.write_char(b'\\')?;
                self.write_char(escape)?;
                start = i + 1;

                if escape == b'u' {
                    self.write(format!("{:04x}", ch).as_bytes())?;
                }
            }
        }

        self.write(&bytes[start..])?;
        self.write_char(b'"')
    }

    fn write_entry(&mut self, entry: &Entry) -> Result<()> {
        match entry {
            Entry::Unit(_) => self.write(b"null"),
            Entry::Bool(b, _) => self.write(if *b { b"true" } else { b"false" }),
            Entry::I8(n, _) => self.write(n.to_string().as_bytes()),
            Entry::I16(n, _) => self.write(n.to_string().as_bytes()),
            Entry::I32(n, _) => self.write(n.to_string().as_bytes()),
            Entry::I64(n, _) => self.write(n.to_string().as_bytes()),
            Entry::I128(n, _) => self.write(n.to_string().as_bytes()),
            Entry::Isize(n, _) => self.write(n.to_string().as_bytes()),
            Entry::U8(n, _) => self.write(n.to_string().as_bytes()),
            Entry::U16(n, _) => self.write(n.to_string().as_bytes()),
            Entry::U32(n, _) => self.write(n.to_string().as_bytes()),
            Entry::U64(n, _) => self.write(n.to_string().as_bytes()),
            Entry::U128(n, _) => self.write(n.to_string().as_bytes()),
            Entry::Usize(n, _) => self.write(n.to_string().as_bytes()),
            Entry::F32(n, _) => {
                if n.is_nan() || n.is_infinite() {
                    self.write(b"null")
                } else {
                    self.write(n.to_string().as_bytes())
                }
            }
            Entry::F64(n, _) => {
                if n.is_nan() || n.is_infinite() {
                    self.write(b"null")
                } else {
                    self.write(n.to_string().as_bytes())
                }
            }
            Entry::Char(c, _) => self.write_string(&c.to_string()),
            Entry::String(s, _) => self.write_string(s),
            Entry::PathBuf(p, _) => self.write_string(&p.display().to_string()),
            Entry::Range(r, _) => {
                self.write_char(b'{')?;
                self.write_string("start")?;
                self.write_min(b": ", b':')?;
                self.write(r.start.to_string().as_bytes())?;
                self.write_char(b',')?;
                self.write_string("end")?;
                self.write_min(b": ", b':')?;
                self.write(r.end.to_string().as_bytes())?;
                self.write_char(b'}')
            }
            Entry::Pair(p, _) => {
                self.write_char(b'[')?;
                self.write_entry(&p.0)?;
                self.write_char(b',')?;
                self.write_entry(&p.1)?;
                self.write_char(b']')
            }
            Entry::Vec(arr, _) => {
                self.write_char(b'[')?;
                if arr.is_empty() {
                    return self.write_char(b']');
                }
                self.indent();
                for (i, item) in arr.iter().enumerate() {
                    if i > 0 {
                        self.write_char(b',')?;
                    }
                    self.new_line()?;
                    self.write_entry(item)?;
                }
                self.dedent();
                self.new_line()?;
                self.write_char(b']')
            }
            Entry::Box(inner, _) => self.write_entry(inner),
            Entry::HashMap(map, _) => {
                self.write_char(b'{')?;
                if map.is_empty() {
                    return self.write_char(b'}');
                }
                self.indent();
                let mut first = true;
                for (key, value) in map.iter() {
                    if !first {
                        self.write_char(b',')?;
                    }
                    first = false;
                    self.new_line()?;
                    self.write_string(key)?;
                    self.write_min(b": ", b':')?;
                    self.write_entry(value)?;
                }
                self.dedent();
                self.new_line()?;
                self.write_char(b'}')
            }
            Entry::Node {
                kind, name, attrs, ..
            } => {
                self.write_char(b'{')?;
                self.indent();

                self.new_line()?;
                self.write_string("kind")?;
                self.write_min(b": ", b':')?;
                self.write_string(kind)?;

                self.write_char(b',')?;
                self.new_line()?;
                self.write_string("name")?;
                self.write_min(b": ", b':')?;
                self.write_string(name)?;

                if !attrs.is_empty() {
                    self.write_char(b',')?;
                    self.new_line()?;
                    self.write_string("attrs")?;
                    self.write_min(b": ", b':')?;
                    self.write_char(b'{')?;
                    self.indent();
                    let mut first = true;
                    for (key, value) in attrs.iter() {
                        if !first {
                            self.write_char(b',')?;
                        }
                        first = false;
                        self.new_line()?;
                        self.write_string(key)?;
                        self.write_min(b": ", b':')?;
                        self.write_entry(value)?;
                    }
                    self.dedent();
                    self.new_line()?;
                    self.write_char(b'}')?;
                }

                self.dedent();
                self.new_line()?;
                self.write_char(b'}')
            }
            Entry::Fn(_, _) => self.write_string("<function>"),
            Entry::Branch {
                operation,
                true_path,
                false_path,
                ..
            } => {
                self.write_char(b'{')?;
                self.indent();

                self.new_line()?;
                self.write_string("type")?;
                self.write_min(b": ", b':')?;
                self.write_string("branch")?;

                self.write_char(b',')?;
                self.new_line()?;
                self.write_string("operation")?;
                self.write_min(b": ", b':')?;
                self.write_string(operation.name())?;

                self.write_char(b',')?;
                self.new_line()?;
                self.write_string("true_path")?;
                self.write_min(b": ", b':')?;
                self.write_char(b'[')?;
                if !true_path.is_empty() {
                    self.indent();
                    for (i, item) in true_path.iter().enumerate() {
                        if i > 0 {
                            self.write_char(b',')?;
                        }
                        self.new_line()?;
                        self.write_entry(item)?;
                    }
                    self.dedent();
                    self.new_line()?;
                }
                self.write_char(b']')?;

                self.write_char(b',')?;
                self.new_line()?;
                self.write_string("false_path")?;
                self.write_min(b": ", b':')?;
                self.write_char(b'[')?;
                if !false_path.is_empty() {
                    self.indent();
                    for (i, item) in false_path.iter().enumerate() {
                        if i > 0 {
                            self.write_char(b',')?;
                        }
                        self.new_line()?;
                        self.write_entry(item)?;
                    }
                    self.dedent();
                    self.new_line()?;
                }
                self.write_char(b']')?;

                self.dedent();
                self.new_line()?;
                self.write_char(b'}')
            }
        }
    }
}

/// Compact JSON generator (no whitespace).
pub struct DumpGenerator {
    output: Vec<u8>,
}

impl DumpGenerator {
    pub fn new() -> Self {
        Self {
            output: Vec::with_capacity(256),
        }
    }

    pub fn consume(self) -> String {
        unsafe { String::from_utf8_unchecked(self.output) }
    }
}

impl Default for DumpGenerator {
    fn default() -> Self {
        Self::new()
    }
}

impl JsonGenerator for DumpGenerator {
    fn write(&mut self, slice: &[u8]) -> Result<()> {
        self.output.extend_from_slice(slice);
        Ok(())
    }

    fn write_char(&mut self, ch: u8) -> Result<()> {
        self.output.push(ch);
        Ok(())
    }

    fn write_min(&mut self, _full: &[u8], min: u8) -> Result<()> {
        self.output.push(min);
        Ok(())
    }
}

/// Pretty JSON generator (with indentation).
pub struct PrettyGenerator {
    output: Vec<u8>,
    indent_level: u16,
    spaces_per_indent: u16,
}

impl PrettyGenerator {
    pub fn new(spaces: u16) -> Self {
        Self {
            output: Vec::with_capacity(256),
            indent_level: 0,
            spaces_per_indent: spaces,
        }
    }

    pub fn consume(self) -> String {
        unsafe { String::from_utf8_unchecked(self.output) }
    }
}

impl JsonGenerator for PrettyGenerator {
    fn write(&mut self, slice: &[u8]) -> Result<()> {
        self.output.extend_from_slice(slice);
        Ok(())
    }

    fn write_char(&mut self, ch: u8) -> Result<()> {
        self.output.push(ch);
        Ok(())
    }

    fn new_line(&mut self) -> Result<()> {
        self.output.push(b'\n');
        for _ in 0..(self.indent_level * self.spaces_per_indent) {
            self.output.push(b' ');
        }
        Ok(())
    }

    fn indent(&mut self) {
        self.indent_level += 1;
    }

    fn dedent(&mut self) {
        self.indent_level = self.indent_level.saturating_sub(1);
    }

    fn write_min(&mut self, full: &[u8], _min: u8) -> Result<()> {
        self.output.extend_from_slice(full);
        Ok(())
    }
}

/// Writer-based JSON generator.
pub struct WriterGenerator<W: Write> {
    writer: W,
}

impl<W: Write> WriterGenerator<W> {
    pub fn new(writer: W) -> Self {
        Self { writer }
    }

    pub fn into_inner(self) -> W {
        self.writer
    }
}

impl<W: Write> JsonGenerator for WriterGenerator<W> {
    fn write(&mut self, slice: &[u8]) -> Result<()> {
        self.writer
            .write_all(slice)
            .map_err(|e| Error::new(Kind::Io, Reason::Write("failed"), Some(e.to_string())))
    }

    fn write_char(&mut self, ch: u8) -> Result<()> {
        self.writer
            .write_all(&[ch])
            .map_err(|e| Error::new(Kind::Io, Reason::Write("failed"), Some(e.to_string())))
    }

    fn write_min(&mut self, _full: &[u8], min: u8) -> Result<()> {
        self.write_char(min)
    }
}

// ============================================================================
// Web JSON methods
// ============================================================================

impl Entry {
    /// Serialize to compact JSON string.
    pub fn to_json(&self) -> String {
        let mut generator = DumpGenerator::new();
        generator.write_entry(self).expect("JSON generation failed");
        generator.consume()
    }

    /// Serialize to pretty JSON string with indentation.
    pub fn to_json_pretty(&self, spaces: u16) -> String {
        let mut generator = PrettyGenerator::new(spaces);
        generator.write_entry(self).expect("JSON generation failed");
        generator.consume()
    }

    /// Write JSON to a writer.
    pub fn write_json<W: Write>(&self, writer: W) -> Result<()> {
        let mut generator = WriterGenerator::new(writer);
        generator.write_entry(self)
    }

    /// Parse JSON string into Web.
    pub fn from_json(source: &str) -> Result<Self> {
        parse(source)
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_null() {
        let e = parse("null").unwrap();
        assert!(matches!(e, Entry::Unit(_)));
    }

    #[test]
    fn test_parse_bool() {
        let t = parse("true").unwrap();
        let f = parse("false").unwrap();
        assert!(matches!(t, Entry::Bool(true, _)));
        assert!(matches!(f, Entry::Bool(false, _)));
    }

    #[test]
    fn test_parse_numbers() {
        let i = parse("42").unwrap();
        let n = parse("-123").unwrap();
        let f = parse("3.14").unwrap();
        let e = parse("1e10").unwrap();

        assert!(matches!(i, Entry::I32(42, _)));
        assert!(matches!(n, Entry::I32(-123, _)));
        if let Entry::F64(v, _) = f {
            assert!((v - 3.14).abs() < 0.001);
        } else {
            panic!("Expected f64");
        }
        if let Entry::F64(v, _) = e {
            assert!((v - 1e10).abs() < 1.0);
        } else {
            panic!("Expected f64");
        }
    }

    #[test]
    fn test_parse_string() {
        let s = parse(r#""hello world""#).unwrap();
        assert!(matches!(s, Entry::String(ref v, _) if v == "hello world"));

        let escaped = parse(r#""hello\nworld""#).unwrap();
        assert!(matches!(escaped, Entry::String(ref v, _) if v == "hello\nworld"));
    }

    #[test]
    fn test_parse_array() {
        let arr = parse("[1, 2, 3]").unwrap();
        if let Entry::Vec(v, _) = arr {
            assert_eq!(v.len(), 3);
        } else {
            panic!("Expected Vec");
        }

        let empty = parse("[]").unwrap();
        if let Entry::Vec(v, _) = empty {
            assert!(v.is_empty());
        } else {
            panic!("Expected Vec");
        }
    }

    #[test]
    fn test_parse_object() {
        let obj = parse(r#"{"name": "test", "value": 42}"#).unwrap();
        if let Entry::HashMap(m, _) = obj {
            assert_eq!(m.len(), 2);
            assert!(m.contains_key("name"));
            assert!(m.contains_key("value"));
        } else {
            panic!("Expected HashMap");
        }
    }

    #[test]
    fn test_parse_nested() {
        let nested = parse(r#"{"arr": [1, {"inner": true}], "obj": {"a": "b"}}"#).unwrap();
        assert!(matches!(nested, Entry::HashMap(_, _)));
    }

    #[test]
    fn test_to_json() {
        let e = Entry::i32(42);
        assert_eq!(e.to_json(), "42");

        let s = Entry::string("hello");
        assert_eq!(s.to_json(), "\"hello\"");

        let b = Entry::bool(true);
        assert_eq!(b.to_json(), "true");

        let n = Entry::unit();
        assert_eq!(n.to_json(), "null");
    }

    #[test]
    fn test_roundtrip() {
        let json = r#"{"name":"test","values":[1,2,3],"nested":{"flag":true}}"#;
        let parsed = parse(json).unwrap();
        let output = parsed.to_json();
        let reparsed = parse(&output).unwrap();

        // Both should produce same JSON (order may differ for objects)
        assert!(matches!(reparsed, Entry::HashMap(_, _)));
    }

    #[test]
    fn test_pretty_print() {
        let e = Entry::hashmap({
            let mut m = HashMap::new();
            m.insert("a".to_string(), Entry::i32(1));
            m
        });
        let pretty = e.to_json_pretty(2);
        assert!(pretty.contains('\n'));
        assert!(pretty.contains("  "));
    }

    #[test]
    fn test_node_to_json() {
        let mut node = Entry::node("Function", "main");
        node.set_attr("return_type", Entry::string("int"));

        let json = node.to_json();
        assert!(json.contains("\"kind\""));
        assert!(json.contains("\"name\""));
        assert!(json.contains("Function"));
        assert!(json.contains("main"));
    }
}
