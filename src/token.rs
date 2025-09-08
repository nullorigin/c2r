#![allow(
    unused_variables,
    unused_assignments,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals
)]

use crate::error::ConversionError;
use crate::handler::{HandlerResult, ProcessedResult};
use crate::{context, debug, error, info, warn};
use std::collections::HashMap;
use std::fmt;
use std::fmt::Display;
use std::hash::Hash;
use std::hash::Hasher;
use std::mem::transmute;
use std::ops::Add;
use std::ops::BitAnd;
use std::ops::BitOr;
use std::ops::BitXor;
use std::ops::Div;
use std::ops::Mul;
use std::ops::Rem;
use std::ops::Sub;
use std::str::from_utf8;
use std::str::FromStr;
use std::time::Instant;

const TOKEN_MAX: usize = 4096;
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TokenSlot(Vec<Token>, u64);
#[derive(Debug)]
pub enum Token {
    a([u8; TOKEN_MAX], usize),
    b(u8),
    c(char),
    f(f64),
    i(i128),
    s(String),
    l(&'static str),
    u(u128),
    v(Vec<u8>),
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Tokenizer {
    name: String,
    content: Vec<u8>,
    cursor: usize,
}
impl Default for Tokenizer {
    fn default() -> Self {
        Tokenizer::new("default_tokenizer")
    }
}
pub type slot = TokenSlot;
pub type tok = Token;
impl Default for Token {
    fn default() -> Self {
        Token::b(0)
    }
}
impl Clone for Token {
    fn clone(&self) -> Self {
        match self {
            Token::a(a, s) => Token::a(*a, *s),
            Token::b(b) => Token::b(*b),
            Token::c(c) => Token::c(*c),
            Token::f(f) => Token::f(*f),
            Token::i(i) => Token::i(*i),
            Token::s(s) => Token::s(s.clone()),
            Token::l(s) => Token::l(*s),
            Token::u(u) => Token::u(*u),
            Token::v(v) => Token::v(v.clone()),
        }
    }
}
impl Hash for Token {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Token::a(a, s) => (a, s).hash(state),
            Token::b(b) => b.hash(state),
            Token::c(c) => c.hash(state),
            Token::f(f) => f.to_bits().hash(state),
            Token::i(i) => i.hash(state),
            Token::s(s) => s.hash(state),
            Token::l(s) => s.hash(state),
            Token::u(u) => u.hash(state),
            Token::v(v) => v.hash(state),
        }
    }
}
impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::a(a, s) => write!(
                f,
                "{}",
                from_utf8(&a[..s.clone()]).unwrap_or("Invalid UTF-8")
            ),
            Token::b(a) => write!(f, "{}", *a as char),
            Token::c(c) => write!(f, "{}", c.to_string()),
            Token::f(n) => write!(f, "{}", n.to_string()),
            Token::i(n) => write!(f, "{}", n.to_string()),
            Token::s(s) => write!(f, "{}", s.to_string()),
            Token::l(s) => write!(f, "{}", s.to_string()),
            Token::u(n) => write!(f, "{}", n.to_string()),
            Token::v(v) => write!(f, "{}", Tokenizer::from_utf8(v.as_slice())),
        }
    }
}
impl Eq for Token {}

impl PartialOrd for Token {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Token {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            // Token::arr matches
            (Token::a(a, s1), Token::a(b, s2)) => s1.cmp(s2).then(a[..*s1].cmp(&b[..*s2])),
            (Token::a(a, s), Token::b(b)) => s.cmp(&1).then(a[0].cmp(b)),
            (Token::a(a, s1), Token::c(b)) => {
                if *s1 == 1 {
                    (a[0] as char).cmp(b)
                } else {
                    std::cmp::Ordering::Greater
                }
            }
            (Token::a(a, s), Token::f(b)) => unsafe {
                std::str::from_utf8_unchecked(&a[..*s]).cmp(&b.to_string())
            },
            (Token::a(a, s), Token::i(b)) => s
                .cmp(&b.to_ne_bytes().len())
                .then(a[..*s].cmp(&b.to_ne_bytes())),
            (Token::a(a, s1), Token::s(b)) => unsafe {
                std::str::from_utf8_unchecked(&a[..*s1]).cmp(b)
            },
            (Token::a(a, s1), Token::l(b)) => unsafe {
                std::str::from_utf8_unchecked(&a[..*s1]).cmp(b)
            },
            (Token::a(a, s), Token::u(b)) => s
                .cmp(&b.to_ne_bytes().len())
                .then(a[..*s].cmp(&b.to_ne_bytes())),
            (Token::a(a, s), Token::v(b)) => a[..*s].cmp(b),

            // Token::b matches
            (Token::b(a), Token::a(b, s)) => 1usize.cmp(s).then(a.cmp(&b[0])),
            (Token::b(a), Token::b(b)) => a.cmp(b),
            (Token::b(a), Token::c(b)) => (*a as char).cmp(b),
            (Token::b(a), Token::f(b)) => a.to_string().cmp(&b.to_string()),
            (Token::b(a), Token::i(b)) => (*a as i128).cmp(b),
            (Token::b(a), Token::s(b)) => 1usize.cmp(&b.len()).then(a.cmp(&b.as_bytes()[0])),
            (Token::b(a), Token::l(b)) => 1usize.cmp(&b.len()).then(a.cmp(&b.as_bytes()[0])),
            (Token::b(a), Token::u(b)) => (*a as u128).cmp(b),
            (Token::b(a), Token::v(b)) => 1usize.cmp(&b.len()).then(a.cmp(&b[0])),

            // Token::ch matches
            (Token::c(a), Token::a(b, s)) => {
                if *s == 1 {
                    a.cmp(&(b[0] as char))
                } else {
                    std::cmp::Ordering::Greater
                }
            }
            (Token::c(a), Token::b(b)) => a.cmp(&(*b as char)),
            (Token::c(a), Token::c(b)) => a.cmp(b),
            (Token::c(a), Token::f(b)) => a.to_string().cmp(&b.to_string()),
            (Token::c(a), Token::i(b)) => a.to_string().cmp(&b.to_string()),
            (Token::c(a), Token::s(b)) => a.to_string().cmp(b),
            (Token::c(a), Token::l(b)) => a.to_string().cmp(&b.to_string()),
            (Token::c(a), Token::u(b)) => a.to_string().cmp(&b.to_string()),
            (Token::c(a), Token::v(b)) => {
                if b.len() == 1 {
                    a.cmp(&(b[0] as char))
                } else {
                    std::cmp::Ordering::Less
                }
            }

            // Token::fl matches
            (Token::f(a), Token::a(b, s)) => unsafe {
                a.to_string()
                    .cmp(&String::from_utf8_unchecked(b[..*s].to_vec()))
            },
            (Token::f(a), Token::b(b)) => a.to_string().cmp(&b.to_string()),
            (Token::f(a), Token::c(b)) => a.total_cmp(&(*b as i8 as f64)),
            (Token::f(a), Token::f(b)) => a.total_cmp(b),
            (Token::f(a), Token::i(b)) => a.to_string().cmp(&b.to_string()),
            (Token::f(a), Token::s(b)) => a.to_string().cmp(b),
            (Token::f(a), Token::l(b)) => a.to_string().cmp(&b.to_string()),
            (Token::f(a), Token::u(b)) => a.to_string().cmp(&b.to_string()),
            (Token::f(a), Token::v(b)) => 8.cmp(&b.len()).then(a.to_ne_bytes()[..8].cmp(&b[..8])),

            // Token::int matches
            (Token::i(a), Token::a(b, s)) => a
                .to_ne_bytes()
                .len()
                .cmp(s)
                .then(a.to_ne_bytes().as_ref().cmp(&b[..*s])),
            (Token::i(a), Token::b(b)) => a.cmp(&(*b as i128)),
            (Token::i(a), Token::c(b)) => a.cmp(&(*b as i128)),
            (Token::i(a), Token::f(b)) => a.cmp(&(*b as i128)),
            (Token::i(a), Token::i(b)) => a.cmp(b),
            (Token::i(a), Token::s(b)) => a.to_string().cmp(&*b),
            (Token::i(a), Token::l(b)) => a.to_string().cmp(&b.to_string()),
            (Token::i(a), Token::u(b)) => (*a as u128).cmp(b),
            (Token::i(a), Token::v(b)) => unsafe {
                a.to_string().cmp(&String::from_utf8_unchecked(b.clone()))
            },
            // Token::s matches
            (Token::s(a), Token::a(b, s)) => unsafe {
                a.cmp(&String::from_utf8_unchecked(b[..*s].to_vec()))
            },
            (Token::s(a), Token::b(b)) => a.len().cmp(&1).then(a.as_bytes()[0].cmp(b)),
            (Token::s(a), Token::c(b)) => a.cmp(&b.to_string()),
            (Token::s(a), Token::f(b)) => a.cmp(&b.to_string()),
            (Token::s(a), Token::i(b)) => a.cmp(&b.to_string()),

            (Token::s(a), Token::s(b)) => a.cmp(b),
            (Token::s(a), Token::l(b)) => a.cmp(&b.to_string()),
            (Token::s(a), Token::u(b)) => a.cmp(&b.to_string()),
            (Token::s(a), Token::v(b)) => a.as_bytes().cmp(b),

            // Token::sl matches
            (Token::l(a), Token::a(b, s)) => a.as_bytes().cmp(&b[..*s]),
            (Token::l(a), Token::b(b)) => a.len().cmp(&1).then(a.as_bytes()[0].cmp(b)),
            (Token::l(a), Token::c(b)) => a.to_string().cmp(&b.to_string()),
            (Token::l(a), Token::f(b)) => a.to_string().cmp(&b.to_string()),
            (Token::l(a), Token::i(b)) => a.to_string().cmp(&b.to_string()),
            (Token::l(a), Token::s(b)) => a.to_string().cmp(b),
            (Token::l(a), Token::l(b)) => a.cmp(b),
            (Token::l(a), Token::u(b)) => a.to_string().cmp(&b.to_string()),
            (Token::l(a), Token::v(b)) => a.as_bytes().cmp(b),

            // Token::uint matches
            (Token::u(a), Token::a(b, s)) => a
                .to_ne_bytes()
                .len()
                .cmp(s)
                .then(a.to_ne_bytes().as_ref().cmp(&b[..*s])),
            (Token::u(a), Token::b(b)) => a.cmp(&(*b as u128)),
            (Token::u(a), Token::c(b)) => a.to_string().cmp(&b.to_string()),
            (Token::u(a), Token::f(b)) => a.to_string().cmp(&b.to_string()),
            (Token::u(a), Token::i(b)) => a.cmp(&(*b as u128)),
            (Token::u(a), Token::s(b)) => a.to_string().cmp(b),
            (Token::u(a), Token::l(b)) => a.to_string().cmp(&b.to_string()),
            (Token::u(a), Token::u(b)) => a.cmp(b),
            (Token::u(a), Token::v(b)) => a
                .to_string()
                .cmp(&String::from_utf8(b.clone()).unwrap_or("Invalid UTF-8".to_string())),

            // Token::vec matches
            (Token::v(a), Token::a(b, s)) => a.cmp(&b[..*s].to_vec()),
            (Token::v(a), Token::b(b)) => a.len().cmp(&1).then(a[0].cmp(b)),
            (Token::v(a), Token::c(b)) => {
                if a.len() == 1 {
                    (a[0] as char).cmp(b)
                } else {
                    std::cmp::Ordering::Greater
                }
            }
            (Token::v(a), Token::f(b)) => String::from_utf8(a.clone())
                .unwrap_or("Invalid UTF-8".to_string())
                .cmp(&b.to_string()),
            (Token::v(a), Token::i(b)) => a
                .len()
                .cmp(&b.to_ne_bytes().len())
                .then(a.cmp(&b.to_ne_bytes().to_vec())),
            (Token::v(a), Token::s(b)) => String::from_utf8(a.clone())
                .unwrap_or("Invalid UTF-8".to_string())
                .cmp(b),
            (Token::v(a), Token::l(b)) => a.cmp(&b.as_bytes().to_vec()),
            (Token::v(a), Token::u(b)) => a
                .len()
                .cmp(&b.to_ne_bytes().len())
                .then(a.cmp(&b.to_ne_bytes().to_vec())),
            (Token::v(a), Token::v(b)) => a.cmp(b),
        }
    }
}

impl Add for Token {
    type Output = Self;
    fn add(self, other: Self) -> Self::Output {
        self.op(other, "+").unwrap()
    }
}
impl Sub for Token {
    type Output = Self;
    fn sub(self, other: Self) -> Self::Output {
        self.op(other, "-").unwrap()
    }
}
impl Mul for Token {
    type Output = Self;
    fn mul(self, other: Self) -> Self::Output {
        self.op(other, "*").unwrap()
    }
}
impl Div for Token {
    type Output = Self;
    fn div(self, other: Self) -> Self::Output {
        self.op(other, "/").unwrap()
    }
}
impl Rem for Token {
    type Output = Self;
    fn rem(self, other: Self) -> Self::Output {
        self.op(other, "%").unwrap()
    }
}
impl BitAnd for Token {
    type Output = Self;
    fn bitand(self, other: Self) -> Self::Output {
        self.op(other, "&").unwrap()
    }
}
impl BitOr for Token {
    type Output = Self;
    fn bitor(self, other: Self) -> Self::Output {
        self.op(other, "|").unwrap()
    }
}
impl BitXor for Token {
    type Output = Self;
    fn bitxor(self, other: Self) -> Self::Output {
        self.op(other, "^").unwrap()
    }
}
impl FromStr for Token {
    type Err = ConversionError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Token::s(s.trim().to_string()))
    }
}
pub fn matches(v: Vec<Token>) -> bool {
    let a = v[0].clone();
    for t in v {
        if a.stok() == t.stok() {
            return true;
        }
    }
    false
}
#[macro_export]
macro_rules! tok {
    // String literals become Token::Str
    ($t:literal) => {
        Token::l(concat!($t))
    };
    // For token identifiers (non-literals)
    ($t:ident) => {
        Token::l($t)
    };
    ($t:tt) => {
        Token::l($t)
    };
    ($t:literal,$k:literal) => {
        Token::$k(concat!($t))
    };
    ($t:tt,$k:ident) => {
        Token::$k($t)
    };
    ($t:tt:$k:literal) => {
        Token::$k($t)
    };
}
impl Token {
    /// Check if the token is whitespace
    pub fn is_whitespace(&self) -> bool {
        matches(vec![
            self.clone(),
            tok!(' '),
            tok!('\t'),
            tok!('\n'),
            tok!('\r'),
            tok!('\0'),
        ])
    }

    pub fn is_numeric(&self) -> bool {
        match self {
            Token::i(_) => true,
            Token::u(_) => true,
            Token::f(_) => true,
            Token::b(_) => true,
            Token::c(_) => true,
            Token::s(s) => {
                s.parse::<i128>().is_ok() || s.parse::<u128>().is_ok() || s.parse::<f64>().is_ok()
            }
            Token::l(s) => {
                s.parse::<i128>().is_ok() || s.parse::<u128>().is_ok() || s.parse::<f64>().is_ok()
            }
            _ => false,
        }
    }
    pub fn is_int(&self) -> bool {
        match self {
            Token::i(_) => true,
            Token::u(_) => true,
            Token::s(s) => s.parse::<i128>().is_ok() || s.parse::<u128>().is_ok(),
            Token::l(s) => s.parse::<i128>().is_ok() || s.parse::<u128>().is_ok(),
            _ => false,
        }
    }
    pub fn is_float(&self) -> bool {
        match self {
            Token::f(_) => true,
            Token::s(s) => s.parse::<f64>().is_ok(),
            Token::l(s) => s.parse::<f64>().is_ok(),
            _ => false,
        }
    }
    pub fn len(&self) -> usize {
        match self {
            Token::a(_, s) => s.clone(),
            Token::b(_) => 1,
            Token::c(c) => c.len_utf8(),
            Token::f(n) => n.to_string().len(),
            Token::i(n) => n.to_string().len(),
            Token::s(s) => s.len(),
            Token::l(s) => s.len(),
            Token::u(n) => n.to_string().len(),
            Token::v(v) => v.len(),
        }
    }
    pub fn stok(&self) -> Token {
        match self.clone() {
            Token::a(a, s) => Tokenizer::from_utf8(&a[..s]),
            Token::b(b) => Token::s((b as char).to_string()),
            Token::c(c) => Token::s(c.to_string()),
            Token::f(f) => Token::s(f.to_string()),
            Token::i(i) => Token::s(i.to_string()),
            Token::s(s) => Token::s(s.to_string()),
            Token::l(s) => Token::s(s.to_string()),
            Token::u(u) => Token::s(u.to_string()),
            Token::v(v) => Token::s(String::from_utf8(v).unwrap()),
        }
    }
    pub fn matches(&self, others: Vec<Token>) -> Vec<Option<Token>> {
        if others.is_empty() {
            return Vec::<Option<Token>>::new();
        }
        let mut res = Vec::<Option<Token>>::new();
        for other in others {
            if self.eq(&other) {
                res.push(Some(other));
            } else {
                res.push(None);
            }
        }
        res
    }
    pub fn match_any(&self, others: &[Token]) -> bool {
        if others.is_empty() {
            return false;
        }
        for other in others {
            if self.eq(other) {
                return true;
            }
        }
        false
    }
    pub fn op(self, other: Token, op: &str) -> Option<Token> {
        match (self, other, op) {
            (Token::i(a), Token::i(b), op) => match op {
                "+" => Some(Token::i(a + b)),
                "-" => Some(Token::i(a - b)),
                "*" => Some(Token::i(a * b)),
                "/" => Some(Token::i(a / b)),
                "%" => Some(Token::i(a % b)),
                "^" => Some(Token::i(a ^ b)),
                "&" => Some(Token::i(a & b)),
                "|" => Some(Token::i(a | b)),
                "<<" => Some(Token::i(a << b)),
                ">>" => Some(Token::i(a >> b)),
                _ => Some(Token::i(0)),
            },
            (Token::u(a), Token::u(b), op) => match op {
                "+" => Some(Token::u(a.wrapping_add(b))),
                "-" => Some(Token::u(a.wrapping_sub(b))),
                "*" => Some(Token::u(a.wrapping_mul(b))),
                "/" => {
                    if b != 0 {
                        Some(Token::u(a / b))
                    } else {
                        None
                    }
                }
                "%" => {
                    if b != 0 {
                        Some(Token::u(a % b))
                    } else {
                        None
                    }
                }
                "^" => Some(Token::u(a ^ b)),
                "&" => Some(Token::u(a & b)),
                "|" => Some(Token::u(a | b)),
                "<<" => Some(Token::u(a << b)),
                ">>" => Some(Token::u(a >> b)),
                _ => Some(Token::u(0)),
            },
            (Token::f(a), Token::f(b), op) => match op {
                "+" => Some(Token::f(a + b)),
                "-" => Some(Token::f(a - b)),
                "*" => Some(Token::f(a * b)),
                "/" => Some(Token::f(a / b)),
                "%" => Some(Token::f(a % b)),
                _ => Some(Token::f(0.0)),
            },
            (Token::b(a), Token::b(b), op) => match op {
                "+" => Some(Token::b(a.wrapping_add(b))),
                "-" => Some(Token::b(a.wrapping_sub(b))),
                "*" => Some(Token::b(a.wrapping_mul(b))),
                "/" => {
                    if b != 0 {
                        Some(Token::b(a / b))
                    } else {
                        None
                    }
                }
                "%" => {
                    if b != 0 {
                        Some(Token::b(a % b))
                    } else {
                        None
                    }
                }
                "^" => Some(Token::b(a ^ b)),
                "&" => Some(Token::b(a & b)),
                "|" => Some(Token::b(a | b)),
                "<<" => Some(Token::b(a << b)),
                ">>" => Some(Token::b(a >> b)),
                _ => Some(Token::b(0)),
            },
            (Token::c(a), Token::c(b), "+") => Some(Token::s(a.to_string() + &b.to_string())),
            (Token::s(a), Token::s(b), "+") => Some(Token::s(a.to_string() + &b)),
            (Token::s(a), Token::s(b), op) => match (a.parse::<f64>(), b.parse::<f64>()) {
                (Ok(a), Ok(b)) => match op {
                    "+" => Some(Token::f(a + b)),
                    "-" => Some(Token::f(a - b)),
                    "*" => Some(Token::f(a * b)),
                    "/" => Some(Token::f(a / b)),
                    "%" => Some(Token::f(a % b)),
                    _ => Some(Token::f(0.0)),
                },
                _ => match (a.parse::<i128>(), b.parse::<i128>()) {
                    (Ok(a), Ok(b)) => match op {
                        "+" => Some(Token::i(a + b)),
                        "-" => Some(Token::i(a - b)),
                        "*" => Some(Token::i(a * b)),
                        "/" => Some(Token::i(a / b)),
                        "%" => Some(Token::i(a % b)),
                        "^" => Some(Token::i(a ^ b)),
                        "&" => Some(Token::i(a & b)),
                        "|" => Some(Token::i(a | b)),
                        "<<" => Some(Token::i(a << b)),
                        ">>" => Some(Token::i(a >> b)),
                        _ => Some(Token::i(0)),
                    },
                    _ => match (a.parse::<u128>(), b.parse::<u128>()) {
                        (Ok(a), Ok(b)) => match op {
                            "+" => Some(Token::u(a.wrapping_add(b))),
                            "-" => Some(Token::u(a.wrapping_sub(b))),
                            "*" => Some(Token::u(a.wrapping_mul(b))),
                            "/" => {
                                if b != 0 {
                                    Some(Token::u(a / b))
                                } else {
                                    None
                                }
                            }
                            "%" => {
                                if b != 0 {
                                    Some(Token::u(a % b))
                                } else {
                                    None
                                }
                            }
                            "^" => Some(Token::u(a ^ b)),
                            "&" => Some(Token::u(a & b)),
                            "|" => Some(Token::u(a | b)),
                            "<<" => Some(Token::u(a << b)),
                            ">>" => Some(Token::u(a >> b)),
                            _ => Some(Token::u(0)),
                        },
                        _ => None,
                    },
                },
            },
            (Token::v(a), Token::v(b), "+") => {
                let mut result = a.clone();
                result.extend_from_slice(&b);
                Some(Token::v(result))
            }
            (Token::l(a), Token::l(b), "+") => Some(Token::s(a.to_string() + b)),
            (Token::a(a, s1), Token::a(b, s2), "+") if s1 + s2 <= TOKEN_MAX => {
                let mut result = [0u8; TOKEN_MAX];
                result[..s1].copy_from_slice(&a[..s1]);
                result[s1..s1 + s2].copy_from_slice(&b[..s2]);
                Some(Token::a(result, s1 + s2))
            }
            _ => None,
        }
    }
    pub fn tokens_to_string(tokens: &[Token], start_idx: usize, end_idx: usize) -> Option<String> {
        if tokens.is_empty() || start_idx > end_idx {
            return None;
        }
        Some(
            tokens[start_idx..end_idx]
                .iter()
                .map(|t| t.to_string())
                .collect::<String>(),
        )
    }
    pub fn base10_zeros(&self) -> usize {
        let mut count = 0;
        if let Token::i(n) = self {
            let mut value = *n;
            while value % 10 == 0 && value != 0 {
                count += 1;
                value /= 10;
            }
        }
        count
    }
}
impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            // Token::arr matches
            (Token::a(a, s1), Token::a(b, s2)) => s1 == s2 && a[..*s1] == b[..*s2],
            (Token::a(a, s), Token::b(b)) => *s == 1 && a[0] == *b,
            (Token::a(a, s), Token::c(b)) => *s == 1 && a[0] as char == *b,
            (Token::a(a, s), Token::f(b)) => &a[..*s] == b.to_string().as_bytes(),
            (Token::a(a, s), Token::i(b)) => &a[..*s] == b.to_string().as_bytes(),
            (Token::a(a, s), Token::u(b)) => &a[..*s] == b.to_string().as_bytes(),
            (Token::a(a, s), Token::s(b)) => *s == b.len() && &a[..*s] == b.as_bytes(),
            (Token::a(a, s), Token::l(b)) => *s == b.len() && &a[..*s] == b.as_bytes(),
            (Token::a(a, s), Token::v(b)) => *s == b.len() && a[..*s] == b[..],

            // Token::b matches
            (Token::b(a), Token::a(b, s)) => *s == 1 && *a == b[0],
            (Token::b(a), Token::b(b)) => a == b,
            (Token::b(a), Token::c(b)) => *a == *b as u8,
            (Token::b(a), Token::f(b)) => a.to_string() == b.to_string(),
            (Token::b(a), Token::i(b)) => (*a as i128) == *b,
            (Token::b(a), Token::u(b)) => (*a as u128) == *b,
            (Token::b(a), Token::s(b)) => b.len() == 1 && *a == b.as_bytes()[0],
            (Token::b(a), Token::l(b)) => b.len() == 1 && *a == b.as_bytes()[0],
            (Token::b(a), Token::v(b)) => b.len() == 1 && *a == b[0],

            // Token::ch matches
            (Token::c(a), Token::a(b, s)) => *s == 1 && *a == (b[0] as char),
            (Token::c(a), Token::b(b)) => *a as u8 == *b,
            (Token::c(a), Token::c(b)) => a == b,
            (Token::c(a), Token::f(b)) => a.to_string() == b.to_string(),
            (Token::c(a), Token::i(b)) => a.to_string() == b.to_string(),
            (Token::c(a), Token::u(b)) => a.to_string() == b.to_string(),
            (Token::c(a), Token::s(b)) => a.to_string() == b.to_string(),
            (Token::c(a), Token::l(b)) => a.to_string() == b.to_string(),
            (Token::c(a), Token::v(b)) => b.len() == 1 && *a == (b[0] as char),

            // Token::fl matches
            (Token::f(a), Token::a(b, s)) => a.to_string().as_bytes() == &b[..*s],
            (Token::f(a), Token::b(b)) => a.to_string() == b.to_string(),
            (Token::f(a), Token::c(b)) => a.to_string() == b.to_string(),
            (Token::f(a), Token::f(b)) => a == b,
            (Token::f(a), Token::i(b)) => a.to_string() == b.to_string(),
            (Token::f(a), Token::u(b)) => a.to_string() == b.to_string(),
            (Token::f(a), Token::s(b)) => a.to_string() == *b,
            (Token::f(a), Token::l(b)) => a.to_string() == *b,
            (Token::f(a), Token::v(b)) => a.to_string().as_bytes() == b,

            // Token::int matches
            (Token::i(a), Token::a(b, s)) => a.to_string().as_bytes() == &b[..*s],
            (Token::i(a), Token::b(b)) => *a == (*b as i128),
            (Token::i(a), Token::c(b)) => a.to_string() == b.to_string(),
            (Token::i(a), Token::f(b)) => a.to_string() == b.to_string(),
            (Token::i(a), Token::i(b)) => a == b,
            (Token::i(a), Token::u(b)) => *a >= 0 && (*a as u128) == *b,
            (Token::i(a), Token::s(b)) => a.to_string() == *b,
            (Token::i(a), Token::l(b)) => a.to_string() == *b,
            (Token::i(a), Token::v(b)) => a.to_string().as_bytes() == b,

            // Token::uint matches
            (Token::u(a), Token::a(b, s)) => a.to_string().as_bytes() == &b[..*s],
            (Token::u(a), Token::b(b)) => *a == (*b as u128),
            (Token::u(a), Token::c(b)) => a.to_string() == b.to_string(),
            (Token::u(a), Token::f(b)) => a.to_string() == b.to_string(),
            (Token::u(a), Token::i(b)) => *b >= 0 && *a == (*b as u128),
            (Token::u(a), Token::u(b)) => a == b,
            (Token::u(a), Token::s(b)) => a.to_string() == *b,
            (Token::u(a), Token::l(b)) => a.to_string() == *b,
            (Token::u(a), Token::v(b)) => a.to_string().as_bytes() == b,

            // Token::s matches
            (Token::s(a), Token::a(b, s)) => a.as_bytes() == &b[..*s],
            (Token::s(a), Token::b(b)) => a.len() == 1 && a.as_bytes()[0] == *b,
            (Token::s(a), Token::c(b)) => *a == b.to_string(),
            (Token::s(a), Token::f(b)) => *a == b.to_string(),
            (Token::s(a), Token::i(b)) => *a == b.to_string(),
            (Token::s(a), Token::u(b)) => *a == b.to_string(),
            (Token::s(a), Token::s(b)) => a == b,
            (Token::s(a), Token::l(b)) => a == b,
            (Token::s(a), Token::v(b)) => a.as_bytes() == b,

            // Token::sl matches
            (Token::l(a), Token::a(b, s)) => a.len() == *s && a.as_bytes() == &b[..*s],
            (Token::l(a), Token::b(b)) => a.len() == 1 && a.as_bytes()[0] == *b,
            (Token::l(a), Token::c(b)) => *a == b.to_string(),
            (Token::l(a), Token::f(b)) => *a == b.to_string(),
            (Token::l(a), Token::i(b)) => *a == b.to_string(),
            (Token::l(a), Token::u(b)) => *a == b.to_string(),
            (Token::l(a), Token::s(b)) => *a == *b,
            (Token::l(a), Token::l(b)) => a == b,
            (Token::l(a), Token::v(b)) => a.as_bytes() == b,

            // Token::vec matches
            (Token::v(a), Token::a(b, s)) => a.len() == *s && a == &b[..*s],
            (Token::v(a), Token::b(b)) => a.len() == 1 && a[0] == *b,
            (Token::v(a), Token::c(b)) => a.len() == 1 && (a[0] as char) == *b,
            (Token::v(a), Token::f(b)) => a == b.to_string().as_bytes(),
            (Token::v(a), Token::i(b)) => a == b.to_string().as_bytes(),
            (Token::v(a), Token::u(b)) => a == b.to_string().as_bytes(),
            (Token::v(a), Token::s(b)) => a == b.as_bytes(),
            (Token::v(a), Token::l(b)) => a == b.as_bytes(),
            (Token::v(a), Token::v(b)) => a == b,
        }
    }
}

impl Tokenizer {
    pub fn new(name: &str) -> Self {
        Tokenizer {
            name: name.to_string(),
            content: Vec::new(),
            cursor: 0,
        }
    }

    pub fn tokenize(
        mut self,
        content: Vec<u8>,
    ) -> Result<Vec<Token>, ConversionError> {
        let mut tokens = Vec::new();
        self.content = content.clone();
        // Debug: Print total content length
        debug!(
            "Tokenizing content with total length: {}",
            self.content.len()
        );
        if !self.content.is_empty() {
            let preview = &self.content[..std::cmp::min(100, self.content.len())];
            debug!(
                "First 100 chars: {:?}",
                from_utf8(preview).unwrap_or("Invalid UTF-8")
            );
        }

        let mut token_count = 0;
        while let Some(token) = self.next_token() {
            tokens.push(token.clone());
            token_count += 1;

            // Print more frequent token info for debugging
            if token_count % 10 == 0 || token_count == 1 || token_count == tokens.len() {
                // Print special tokens that might indicate issues
                match &token {
                    tok!('{') => debug!(
                        "Found OpenBrace at token #{} (cursor {})",
                        token_count, self.cursor
                    ),
                    tok!('}') => debug!(
                        "Found CloseBrace at token #{} (cursor {})",
                        token_count, self.cursor
                    ),
                    tok!('=') => {
                        if matches!(self.peek_next_token(), Some(tok!('{'))) {
                            debug!(
                                "Found Equals followed by OpenBrace at token #{} (cursor {})",
                                token_count, self.cursor
                            );
                        }
                    }
                    _ => {
                        debug!(
                            "Found token {} at token #{} (cursor {})",
                            token.stok(),
                            token_count,
                            self.cursor
                        );
                    }
                }

                // Safety check to prevent infinite loops during debugging
                if token_count > TOKEN_MAX {
                    warn!("Token limit reached ({}), stopping tokenization", TOKEN_MAX);
                    break;
                }
            }
        }

        debug!(
            "Finished tokenizing. Generated {} tokens. Cursor position: {}/{}",
            tokens.len(),
            self.cursor,
            self.content.len()
        );

        // Check if we've processed the entire content
        if self.cursor < self.content.len() {
            warn!(
                "Not all content was processed! Remaining characters: {}",
                self.content.len() - self.cursor
            );

            let content = self.content.as_slice();
            let (processed, remaining) = content.split_at(self.cursor);
            let processed = from_utf8(processed).unwrap();
            let remaining = from_utf8(remaining).unwrap();
            debug!("Processed content:\n{}", processed);
            warn!("Remaining content:\n{}", remaining);
        }
        for mut token in tokens.iter() {
            token = &token.stok();
        }
        Ok(tokens)
    }
    pub fn byte_at(&self, index: usize) -> u8 {
        self.content[index]
    }
    pub fn cursor_byte(&self) -> u8 {
        self.content[self.cursor]
    }
    pub fn chqr_at(&self, index: usize) -> char {
        self.content[index] as char
    }
    pub fn cursor_char(&self) -> char {
        self.content[self.cursor] as char
    }
    pub fn next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();

        if self.cursor >= self.content.len() {
            return None;
        }

        // Check for comments and skip them
        if self.cursor + 1 < self.content.len() {
            if self.content[self.cursor] == b'/' && self.content[self.cursor + 1] == b'/' {
                self.skip_line_comment();
                return self.next_token();
            } else if self.content[self.cursor] == b'/' && self.content[self.cursor + 1] == b'*' {
                self.skip_block_comment();
                return self.next_token();
            }
        }

        // Check for line continuation in preprocessor directives
        if self.handle_line_continuation() {
            return self.next_token();
        }

        let b = self.cursor_byte();
        let mut _is_str = false;
        let start = self.cursor;
        let mut end = self.cursor;

        if b.is_ascii_alphabetic() || b == b'_' {
            while self.cursor < self.content.len()
                && (self.cursor_byte().is_ascii_alphanumeric()
                || self.cursor_byte() == b'_'
                || self.cursor_byte() == b'.')
            {
                self.cursor += 1;
            }
            end = self.cursor;
            if end - start > 1 {
                _is_str = true;
            }
        } else if b.is_ascii_digit()
            || (b == b'.' && self.peek().map_or(false, |next| next.is_ascii_digit()))
        {
            // Handle numeric literals including those starting with decimal point
            let mut _has_decimal = b == b'.';

            while self.cursor < self.content.len() {
                let current = self.cursor_byte();
                if current.is_ascii_digit() {
                    self.cursor += 1;
                } else if current == b'.' && !_has_decimal {
                    _has_decimal = true;
                    self.cursor += 1;
                } else if (current == b'e' || current == b'E')
                    && self.cursor + 1 < self.content.len()
                {
                    // Handle scientific notation
                    let next = self.content[self.cursor + 1];
                    if next.is_ascii_digit() || next == b'+' || next == b'-' {
                        self.cursor += 1; // Skip 'e' or 'E'
                        if next == b'+' || next == b'-' {
                            self.cursor += 1; // Skip sign if present
                        }
                        // Continue with digits after exponent
                        while self.cursor < self.content.len()
                            && self.cursor_byte().is_ascii_digit()
                        {
                            self.cursor += 1;
                        }
                    }
                    break;
                } else {
                    break;
                }
            }
            end = self.cursor;
        }

        if start != end {
            // Use transmute directly for performance - content should be valid ASCII/UTF-8 from C source
            let content = unsafe { transmute::<&[u8], &str>(&self.content[start..end]) };

            if _is_str || content.contains(' ') {
                return Some(Token::s(content.trim().to_string()));
            }

            // Handle numeric literals - create proper Token::i() or Token::f() variants using tok! macro
            if b.is_ascii_digit()
                || (b == b'.'
                && self
                .content
                .get(start + 1)
                .map_or(false, |&next| next.is_ascii_digit()))
            {
                if content.contains('.') || content.contains('e') || content.contains('E') {
                    // Parse as float
                    if let Ok(float_val) = content.parse::<f64>() {
                        return Some(tok!(float_val, f));
                    }
                } else {
                    // Parse as integer
                    if let Ok(int_val) = content.parse::<i128>() {
                        return Some(tok!(int_val, i));
                    }
                }
                // If parsing fails, fall back to string token
                return Some(Token::s(content.to_string()));
            }
        }

        match b {
            b'(' => {
                self.cursor += 1;
                Some(tok!('('))
            }
            b')' => {
                self.cursor += 1;
                Some(tok!(')'))
            }
            b'{' => {
                self.cursor += 1;
                Some(tok!('{'))
            }
            b'}' => {
                self.cursor += 1;
                Some(tok!('}'))
            }
            b'[' => {
                self.cursor += 1;
                Some(tok!('['))
            }
            b']' => {
                self.cursor += 1;
                Some(tok!(']'))
            }
            b';' => {
                self.cursor += 1;
                Some(tok!(';'))
            }
            b',' => {
                self.cursor += 1;
                Some(tok!(','))
            }
            b'+' => {
                if self.peek() == Some(b'+') {
                    self.cursor += 2;
                    Some(tok!("++"))
                } else if self.peek() == Some(b'=') {
                    self.cursor += 2;
                    Some(tok!("+="))
                } else {
                    self.cursor += 1;
                    Some(tok!('+'))
                }
            }
            b'-' => {
                if self.peek() == Some(b'-') {
                    self.cursor += 2;
                    Some(tok!("--"))
                } else if self.peek() == Some(b'=') {
                    self.cursor += 2;
                    Some(tok!("-="))
                } else if self.peek() == Some(b'>') {
                    self.cursor += 2;
                    Some(tok!("->"))
                } else {
                    self.cursor += 1;
                    Some(tok!('-'))
                }
            }
            b'/' => {
                if self.peek() == Some(b'=') {
                    self.cursor += 2;
                    Some(tok!("/="))
                } else {
                    self.cursor += 1;
                    Some(tok!('/'))
                }
            }
            b'%' => {
                if self.peek() == Some(b'=') {
                    self.cursor += 2;
                    Some(tok!("%="))
                } else {
                    self.cursor += 1;
                    Some(tok!('%'))
                }
            }
            b'*' => {
                if self.peek() == Some(b'=') {
                    self.cursor += 2;
                    Some(tok!("*="))
                } else {
                    self.cursor += 1;
                    Some(tok!('*'))
                }
            }
            b'&' => {
                if self.peek() == Some(b'&') {
                    self.cursor += 2;
                    Some(tok!("&&"))
                } else if self.peek() == Some(b'=') {
                    self.cursor += 2;
                    Some(tok!("&="))
                } else {
                    self.cursor += 1;
                    Some(tok!('&'))
                }
            }
            b'|' => {
                if self.peek() == Some(b'|') {
                    self.cursor += 2;
                    Some(tok!("||"))
                } else if self.peek() == Some(b'=') {
                    self.cursor += 2;
                    Some(tok!("|="))
                } else {
                    self.cursor += 1;
                    Some(tok!('|'))
                }
            }
            b'<' => {
                if self.peek() == Some(b'<') {
                    if self.peek_at(2) == Some(b'=') {
                        self.cursor += 3;
                        Some(tok!("<<="))
                    } else {
                        self.cursor += 2;
                        Some(tok!("<<"))
                    }
                } else if self.peek() == Some(b'=') {
                    self.cursor += 2;
                    Some(tok!("<="))
                } else {
                    self.cursor += 1;
                    Some(tok!('<'))
                }
            }
            b'>' => {
                if self.peek() == Some(b'>') {
                    if self.peek_at(2) == Some(b'=') {
                        self.cursor += 3;
                        Some(tok!(">>="))
                    } else {
                        self.cursor += 2;
                        Some(tok!(">>"))
                    }
                } else if self.peek() == Some(b'=') {
                    self.cursor += 2;
                    Some(tok!(">="))
                } else {
                    self.cursor += 1;
                    Some(tok!('>'))
                }
            }
            b'!' => {
                if self.peek() == Some(b'=') {
                    self.cursor += 2;
                    Some(tok!("!="))
                } else {
                    self.cursor += 1;
                    Some(tok!('!'))
                }
            }
            b'^' => {
                if self.peek() == Some(b'=') {
                    self.cursor += 2;
                    Some(tok!("^="))
                } else {
                    self.cursor += 1;
                    Some(tok!('^'))
                }
            }
            b'~' => {
                self.cursor += 1;
                Some(tok!('~'))
            }
            b'?' => {
                self.cursor += 1;
                Some(tok!('?'))
            }
            b':' => {
                if self.peek() == Some(b':') {
                    self.cursor += 2;
                    Some(tok!("::"))
                } else {
                    self.cursor += 1;
                    Some(tok!(':'))
                }
            }
            b'#' => {
                if self.peek() == Some(b'#') {
                    self.cursor += 2;
                    Some(tok!("##"))
                } else {
                    self.cursor += 1;
                    Some(tok!('#'))
                }
            }
            b'"' => self.read_string_literal(),
            b'\'' => self.read_char_literal(),
            b'.' => {
                if self.peek() == Some(b'.') && self.peek_at(2) == Some(b'.') {
                    self.cursor += 3;
                    Some(tok!("..."))
                } else {
                    self.cursor += 1;
                    Some(tok!('.'))
                }
            }
            b'\\' => {
                self.cursor += 1;
                Some(tok!('\\'))
            }
            b'_' => {
                self.cursor += 1;
                Some(tok!('_'))
            }
            b'=' => {
                if self.peek() == Some(b'=') {
                    self.cursor += 2;
                    Some(tok!("=="))
                } else {
                    self.cursor += 1;
                    Some(tok!('='))
                }
            }
            _ => {
                self.cursor += 1;
                Some(Token::c(b as char))
            }
        }
    }

    /// Peek at the next token without advancing the cursor
    fn peek_next_token(&mut self) -> Option<Token> {
        let current_cursor = self.cursor;
        let token = self.next_token();
        self.cursor = current_cursor;
        token
    }

    fn read_string_literal(&mut self) -> Option<Token> {
        // Read a string literal
        let mut literal = String::new();
        self.cursor += 1; // skip opening quote

        while self.cursor < self.content.len() {
            let b = self.content[self.cursor];
            if b == b'"' {
                self.cursor += 1;
                return Some(Token::s(literal));
            } else if b == b'\\' && self.cursor + 1 < self.content.len() {
                // Handle escape sequences
                self.cursor += 1;
                let escape_char = self.content[self.cursor] as char;
                literal.push('\\');
                literal.push(escape_char);
            } else {
                literal.push(b as char);
            }
            self.cursor += 1;
        }

        None // Unterminated string literal
    }

    fn read_char_literal(&mut self) -> Option<Token> {
        // Read a char literal
        self.cursor += 1; // skip opening quote

        if self.cursor >= self.content.len() {
            return None; // Unterminated char literal
        }

        let mut c = self.cursor_char();
        let mut _escape = false;

        // Handle escape sequences
        if c == '\\' && self.cursor + 1 < self.content.len() {
            self.cursor += 1;
            c = self.cursor_char();
            _escape = true;
        }

        self.cursor += 1;

        // Check for closing quote
        if self.cursor < self.content.len() && self.content[self.cursor] == b'\'' {
            self.cursor += 1;
            return Some(Token::c(c));
        }

        None // Malformed char literal
    }
    pub fn from_byte(b: u8) -> Token {
        match b {
            0 => tok!("\0"),
            1 => tok!("\x01"),
            2 => tok!("\x02"),
            3 => tok!("\x03"),
            4 => tok!("\x04"),
            5 => tok!("\x05"),
            6 => tok!("\x06"),
            7 => tok!("\x07"),
            8 => tok!("\x08"),
            9 => tok!("\t"),
            10 => tok!("\n"),
            11 => tok!("\x0B"),
            12 => tok!("\x0C"),
            13 => tok!("\r"),
            14 => tok!("\x0E"),
            15 => tok!("\x0F"),
            16 => tok!("\x10"),
            17 => tok!("\x11"),
            18 => tok!("\x12"),
            19 => tok!("\x13"),
            20 => tok!("\x14"),
            21 => tok!("\x15"),
            22 => tok!("\x16"),
            23 => tok!("\x17"),
            24 => tok!("\x18"),
            25 => tok!("\x19"),
            26 => tok!("\x1A"),
            27 => tok!("\x1B"),
            28 => tok!("\x1C"),
            29 => tok!("\x1D"),
            30 => tok!("\x1E"),
            31 => tok!("\x1F"),
            32 => tok!(" "),
            33 => tok!("!"),
            34 => tok!("\""),
            35 => tok!("#"),
            36 => tok!("$"),
            37 => tok!("%"),
            38 => tok!("&"),
            39 => tok!("'"),
            40 => tok!("("),
            41 => tok!(")"),
            42 => tok!("*"),
            43 => tok!("+"),
            44 => tok!(","),
            45 => tok!("-"),
            46 => tok!("."),
            47 => tok!("/"),
            48 => tok!("0"),
            49 => tok!("1"),
            50 => tok!("2"),
            51 => tok!("3"),
            52 => tok!("4"),
            53 => tok!("5"),
            54 => tok!("6"),
            55 => tok!("7"),
            56 => tok!("8"),
            57 => tok!("9"),
            58 => tok!(":"),
            59 => tok!(";"),
            60 => tok!("<"),
            61 => tok!("="),
            62 => tok!(">"),
            63 => tok!("?"),
            64 => tok!("@"),
            65 => tok!("A"),
            66 => tok!("B"),
            67 => tok!("C"),
            68 => tok!("D"),
            69 => tok!("E"),
            70 => tok!("F"),
            71 => tok!("G"),
            72 => tok!("H"),
            73 => tok!("I"),
            74 => tok!("J"),
            75 => tok!("K"),
            76 => tok!("L"),
            77 => tok!("M"),
            78 => tok!("N"),
            79 => tok!("O"),
            80 => tok!("P"),
            81 => tok!("Q"),
            82 => tok!("R"),
            83 => tok!("S"),
            84 => tok!("T"),
            85 => tok!("U"),
            86 => tok!("V"),
            87 => tok!("W"),
            88 => tok!("X"),
            89 => tok!("Y"),
            90 => tok!("Z"),
            91 => tok!("["),
            92 => tok!("\\"),
            93 => tok!("]"),
            94 => tok!("^"),
            95 => tok!("_"),
            96 => tok!("`"),
            97 => tok!("a"),
            98 => tok!("b"),
            99 => tok!("c"),
            100 => tok!("d"),
            101 => tok!("e"),
            102 => tok!("f"),
            103 => tok!("g"),
            104 => tok!("h"),
            105 => tok!("i"),
            106 => tok!("j"),
            107 => tok!("k"),
            108 => tok!("l"),
            109 => tok!("m"),
            110 => tok!("n"),
            111 => tok!("o"),
            112 => tok!("p"),
            113 => tok!("q"),
            114 => tok!("r"),
            115 => tok!("s"),
            116 => tok!("t"),
            117 => tok!("u"),
            118 => tok!("v"),
            119 => tok!("w"),
            120 => tok!("x"),
            121 => tok!("y"),
            122 => tok!("z"),
            123 => tok!("{"),
            124 => tok!("|"),
            125 => tok!("}"),
            126 => tok!("~"),
            127 => tok!("\x7F"),
            _ => tok!("?"),
        }
    }
    pub fn from_utf8(b: &[u8]) -> Token {
        unsafe { Token::l(transmute::<&[u8], &str>(b)) }
    }
    fn skip_whitespace(&mut self) {
        while self.cursor < self.content.len() && self.content[self.cursor].is_ascii_whitespace() {
            self.cursor += 1;
        }
    }

    fn skip_line_comment(&mut self) {
        // Skip the opening // characters
        self.cursor += 2;

        // Skip until we reach a newline or end of content
        while self.cursor < self.content.len() && self.content[self.cursor] != b'\n' {
            // Check for line continuation (backslash followed by newline)
            if self.content[self.cursor] == b'\\'
                && self.cursor + 1 < self.content.len()
                && self.content[self.cursor + 1] == b'\n'
            {
                self.cursor += 2; // Skip backslash and newline
                continue;
            }
            self.cursor += 1;
        }

        // Skip the newline if we haven't reached the end
        if self.cursor < self.content.len() && self.content[self.cursor] == b'\n' {
            self.cursor += 1;
        }
    }

    fn skip_block_comment(&mut self) {
        // Skip the opening /* characters
        self.cursor += 2;

        // Store the starting position for error reporting
        let start_pos = self.cursor - 2;
        let mut chars_consumed = 0;
        let max_comment_length = 5000; // Reasonable limit for a block comment

        // Skip until we find the closing */ sequence
        while self.cursor + 1 < self.content.len() {
            // Check if we found the closing sequence
            if self.content[self.cursor] == b'*' && self.content[self.cursor + 1] == b'/' {
                // Skip the closing */ characters
                self.cursor += 2;
                return; // Normal exit - found closing sequence
            }

            // Safety check to prevent consuming entire large files on unclosed comments
            chars_consumed += 1;
            if chars_consumed > max_comment_length {
                error!(
                    "WARNING: Block comment starting at position {} exceeds maximum length ({}). Assuming comment ended.",
                    start_pos, max_comment_length
                );
                error!(
                    "Comment starts with: {}",
                    String::from_utf8_lossy(
                        &self.content[start_pos..std::cmp::min(start_pos + 40, self.content.len())]
                    )
                );

                // Try to recover by finding potential code patterns
                self.recover_from_unclosed_comment(start_pos);
                return;
            }

            self.cursor += 1;
        }

        // If we reached here, we've hit the end of the content without finding a closing */
        error!(
            "WARNING: Unclosed block comment starting at position {}.",
            start_pos
        );
        error!(
            "Comment starts with: {}",
            String::from_utf8_lossy(
                &self.content[start_pos..std::cmp::min(start_pos + 40, self.content.len())]
            )
        );

        // Try to recover by searching for code patterns in the file
        self.recover_from_unclosed_comment(start_pos);
    }

    // New method to handle recovery from unclosed comments
    fn recover_from_unclosed_comment(&mut self, comment_start_pos: usize) {
        // Store a copy of the current cursor position
        let original_pos = self.cursor;

        // Try to get a UTF-8 string representation of the content
        let content = match String::from_utf8_lossy(&self.content).to_string() {
            s => s,
        };

        // Common C type keywords and modifiers that might indicate function declarations
        let type_patterns = [
            // Basic types
            "void ",
            "int ",
            "char ",
            "float ",
            "double ",
            "long ",
            "short ",
            "unsigned ",
            "signed ",
            "size_t ",
            "bool ",
            // Type modifiers
            "static ",
            "extern ",
            "const ",
            "inline ",
            // Struct/enum/union declarations
            "struct ",
            "enum ",
            "union ",
            // Combinations
            "static int ",
            "static void ",
            "extern int ",
            "extern void ",
            "unsigned int ",
            "unsigned long ",
            "long int ",
            "long long ",
        ];

        // First attempt: Find function declarations after the comment start
        let mut candidates = Vec::new();

        for pattern in &type_patterns {
            let mut search_pos = comment_start_pos;

            while let Some(idx) = content[search_pos..].find(pattern) {
                let pos = search_pos + idx;
                let line_end = content[pos..].find('\n').unwrap_or(content.len() - pos);
                let line = &content[pos..pos + line_end];

                // Check if this looks like a function declaration (has parentheses and identifier)
                if line.contains('(') && line.contains(')') {
                    // Verify there's an identifier before the opening parenthesis
                    if let Some(paren_pos) = line.find('(') {
                        let before_paren = &line[pattern.len()..paren_pos].trim();
                        if !before_paren.is_empty()
                            && before_paren
                            .chars()
                            .all(|c| c.is_alphanumeric() || c == '_')
                        {
                            candidates.push((pos, line.to_string()));
                            debug!(
                                "Found potential function at position {}: {}",
                                pos,
                                line.trim()
                            );
                        }
                    }
                }

                search_pos = pos + 1;
                if search_pos >= content.len() {
                    break;
                }
            }
        }

        // If we found function declarations, resume from the earliest one
        if !candidates.is_empty() {
            candidates.sort_by_key(|(pos, _)| *pos);
            let (resume_pos, decl) = &candidates[0];

            info!(
                "Resuming tokenization at function declaration at position {}: {}",
                resume_pos,
                decl.trim()
            );
            self.cursor = *resume_pos;
            warn!(
                "/* Tokenizer recovered from unclosed comment. Resuming at function declaration */"
            );
            return;
        }

        // Second attempt: Look for closing braces followed by potential declarations
        let mut pos = comment_start_pos;
        while pos + 1 < content.len() {
            if content.as_bytes()[pos] == b'}' {
                // Skip whitespace after brace
                let mut after_brace = pos + 1;
                while after_brace < content.len()
                    && content.as_bytes()[after_brace].is_ascii_whitespace()
                {
                    after_brace += 1;
                }

                if after_brace < content.len() {
                    // Check if what follows looks like a declaration
                    let remaining = &content[after_brace..];
                    let next_line_end = remaining.find('\n').unwrap_or(remaining.len());
                    let next_line = &remaining[..next_line_end];

                    // Check for type keywords and function signature pattern
                    if type_patterns
                        .iter()
                        .any(|&pattern| next_line.starts_with(pattern))
                        && next_line.contains('(')
                    {
                        debug!(
                            "Found potential function after brace at position {}: {}",
                            after_brace,
                            next_line.trim()
                        );
                        self.cursor = after_brace;
                        warn!(
                            "/* Tokenizer recovered from unclosed comment after closing brace */"
                        );
                        return;
                    }
                }
            }
            pos += 1;
        }

        // Third attempt: Look for preprocessor directives (#include, #define, etc.)
        if let Some(preprocessor_pos) = content[comment_start_pos..].find('#') {
            let pos = comment_start_pos + preprocessor_pos;
            let line_start = content[..pos].rfind('\n').map_or(0, |i| i + 1);

            // Check if it's at the start of a line (only whitespace before it)
            let prefix = &content[line_start..pos];
            if prefix.trim().is_empty() {
                debug!("Found preprocessor directive at position {}", pos);
                self.cursor = pos;
                warn!("/* Tokenizer recovered from unclosed comment at preprocessor directive */");
                return;
            }
        }

        // If all recovery attempts failed, continue from where we left off
        debug!(
            "No recovery points found. Continuing from position {}",
            original_pos
        );
        self.cursor = original_pos;
    }

    /// Handle backslash line continuation in preprocessor directives
    fn handle_line_continuation(&mut self) -> bool {
        // Check if we're at a backslash
        if self.cursor >= self.content.len() || self.cursor_byte() != b'\\' {
            return false;
        }

        let mut lookahead = self.cursor + 1;

        // Skip any whitespace except newline
        while lookahead < self.content.len()
            && self.content[lookahead].is_ascii_whitespace()
            && self.content[lookahead] != b'\n'
        {
            lookahead += 1;
        }

        // If we found a newline, this is a line continuation
        if lookahead < self.content.len() && self.content[lookahead] == b'\n' {
            // Skip the backslash and all whitespace including the newline
            self.cursor = lookahead + 1;

            // Skip any whitespace at the beginning of the next line
            self.skip_whitespace();
            return true;
        }

        false
    }

    fn peek(&self) -> Option<u8> {
        self.peek_at(1)
    }

    fn peek_at(&self, offset: usize) -> Option<u8> {
        if self.cursor + offset < self.content.len() {
            Some(self.content[self.cursor + offset])
        } else {
            None
        }
    }

    /// Process tokens incrementally with registered handlers
    /// Returns a vector of ProcessedResults that can be used to generate Rust code
    pub fn process_tokens(
        &mut self,
        tokens: &[Token],
    ) -> Result<ProcessedResult, ConversionError> {
        let start_time = Instant::now();
        // Track statistics if debug mode is enabled
        let total_tokens = tokens.len();
        let mut processed_tokens = 0;
        let mut handler_stats: HashMap<String, usize> = HashMap::new();
        let mut error_count = 0;
        let mut unhandled_count = 0;
        let handlers = context!().handlers.clone();
        // Initialize handler statistics
        for handler in context!().handlers.clone() {
            handler_stats.insert(handler.name().to_string(), 0);
        }
        let processed = &mut handlers.process(tokens)?;
        // Process tokens incrementally
        // Build the resulting Rust code from the processed results
        let mut result = String::with_capacity(tokens.len() * 2); // Pre-allocate memory
        while processed_tokens <= total_tokens {
            let token_count = processed.tokens_consumed;
            processed_tokens += token_count;

            // Update handler statistics
            let handler_name = processed.id.name();
            if let Some(count) = handler_stats.get_mut(handler_name) {
                *count += token_count;
            }

            // Add debug comment showing which handler processed these tokens
            if context!().get_verbosity() >= 2 {
                match &processed.result {
                    HandlerResult::Converted(_, _, _, id)
                    | HandlerResult::Completed(_, _, _, id) => {
                        result.push_str(&format!("// Processed by {}\n", id.name()));
                    }
                    _ => {}
                }
            }

            // Process the result based on its type
            match &processed.result {
                HandlerResult::Processed(_, _, code, _) => {
                    result.push_str(code);
                    if !code.ends_with('\n') {
                        result.push('\n');
                    }
                    if !code.ends_with("\n\n") {
                        result.push('\n');
                    }
                }
                HandlerResult::Converted(_, _, code, _) => {
                    result.push_str(code);
                    if !code.ends_with('\n') {
                        result.push('\n');
                    }
                    if !code.ends_with("\n\n") {
                        result.push('\n');
                    }
                }
                HandlerResult::Completed(_, _, code, _) => {
                    result.push_str(code);
                    if !code.ends_with('\n') {
                        result.push('\n');
                    }
                }
                HandlerResult::NotHandled(Some(tokens), _, _) => {
                    unhandled_count += 1;
                    // For unhandled tokens, generate a comment with the original _code
                    let unhandled_tokens = &tokens
                        [processed.start_idx..processed.start_idx + processed.tokens_consumed];
                    let unhandled_token_str = unhandled_tokens
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<String>>()
                        .join(" ");

                    // Only warn about non-trivial unhandled tokens (not just whitespace or comments)
                    if !unhandled_token_str.trim().is_empty()
                        && !unhandled_tokens
                        .iter()
                        .all(|t| t.to_string().trim().is_empty())
                    {
                        warn!("Unhandled tokens: {}", unhandled_token_str);

                        // Add a detailed comment in the generated _code
                        result.push_str(&format!("// UNHANDLED C CODE: {}\n", unhandled_token_str));

                        // Format multi-line unhandled _code appropriately
                        let lines: Vec<&str> = unhandled_token_str.lines().collect();
                        if lines.len() > 1 {
                            result.push_str("/*\n");
                            for line in lines {
                                result.push_str(&format!(" * {}\n", line));
                            }
                            result.push_str(" */\n\n");
                        } else if !unhandled_token_str.is_empty() {
                            result.push_str(&format!("// {}\n\n", unhandled_token_str));
                        }
                    }
                }
                HandlerResult::Redirected(_tokens, _, _code, id1, id2) => {
                    error_count += 1;
                    let error_msg = format!(
                        "Unexpected Redirect result at token index {} (to handler: {}, from handler: {})",
                        processed.start_idx,
                        id1.name(),
                        id2.name()
                    );
                    warn!("{}", error_msg);
                    result.push_str(&format!("/* ERROR: {} */\n\n", error_msg));
                }
                HandlerResult::Handled(Some(tokens), _, _id) => {
                    let handled_str = tokens
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<String>>()
                        .join(" ");
                    result.push_str(&handled_str);
                    if !handled_str.ends_with('\n') {
                        result.push('\n');
                    }
                    if !handled_str.ends_with("\n\n") {
                        result.push('\n');
                    }
                }
                HandlerResult::Extracted(element, _, code, _) => {
                    result.push_str(&format!("// Extracted: {:?}\n", element));
                    result.push_str(code);
                    if !code.ends_with('\n') {
                        result.push('\n');
                    }
                    if !code.ends_with("\n\n") {
                        result.push('\n');
                    }
                }
                HandlerResult::NotHandled(None, _, _) => {
                    // No tokens to process, continue
                }
                HandlerResult::Handled(None, _, _) => {
                    // No tokens returned, but processing was successful
                }
            }
        }
        let elapsed = start_time.elapsed();

        // Add debug information if enabled
        if context!().get_verbosity() >= 1 {
            let processed_pct = if total_tokens > 0 {
                (processed_tokens as f64 / total_tokens as f64) * 100.0
            } else {
                0.0
            };

            let mut footer = "\n// Conversion statistics:\n".to_string();
            footer.push_str(&format!("// Total tokens: {}\n", total_tokens));
            footer.push_str(&format!(
                "// Processed tokens: {} ({:.1}%)\n",
                processed_tokens, processed_pct
            ));
            footer.push_str(&format!("// Unhandled sections: {}\n", unhandled_count));
            footer.push_str(&format!("// Errors encountered: {}\n", error_count));
            footer.push_str(&format!("// Conversion time: {:.2?}\n", elapsed));

            if !handler_stats.is_empty() && context!().get_verbosity() >= 2 {
                footer.push_str("// Handler statistics:\n");
                let mut sorted_stats: Vec<_> = handler_stats.iter().collect();
                sorted_stats.sort_by(|a, b| b.1.cmp(a.1)); // Sort by count, descending

                for (handler, count) in sorted_stats {
                    if *count > 0 {
                        let handler_pct = (*count as f64 / total_tokens as f64) * 100.0;
                        footer.push_str(&format!(
                            "//   {}: {} tokens ({:.1}%)\n",
                            handler, count, handler_pct
                        ));
                    }
                }
            }

            result.push_str(&footer);
        }

        // Always add a trailing newline for clean output
        if !result.ends_with('\n') {
            result.push('\n');
        }
        println!("\n{}", result);
        Ok(processed.clone())
    }
}
