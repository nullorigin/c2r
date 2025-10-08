#[macro_export]
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

#[macro_export]
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
#[macro_export]
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
    [] => (crate::json::Value::new_array());

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

        crate::json::Value::Array(array)
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
    {} => (crate::json::Value::new_object());

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
        let mut object = $crate::json::Object::with_capacity(size);

        $(
            object.insert($k, $v);
        )*

        $crate::json::Value::Object(object)
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
// Read a byte from the source.
// Will return an error if there are no more bytes.
#[macro_export]
macro_rules! expect_byte {
    ($parser:ident) => {{
        if $parser.is_eof() {
            return Err(C2RError::new(
                Kind::Json,
                Reason::Unexpected("end of JSON"),
                Some("reached end of JSON source unexpectedly".to_string()),
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
#[macro_export]
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
#[macro_export]
macro_rules! expect_byte_ignore_whitespace {
    ($parser:ident) => {{
        let mut ch = $crate::json::parsing::expect_byte!($parser);

        // Don't go straight for the loop, assume we are in the clear first.
        match ch {
            // whitespace
            9..=13 | 32 => loop {
                match $crate::json::parsing::expect_byte!($parser) {
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
#[macro_export]
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
#[macro_export]
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
// Expect a string. This is called after encountering, and consuming, a
// double quote character. This macro has a happy path variant where it
// does almost nothing as long as all characters are allowed (as described
// in the look up table above). If it encounters a closing quote without
// any escapes, it will use a slice straight from the source, avoiding
// unnecessary buffering.
#[macro_export]
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
#[macro_export]
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
#[macro_export]
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
// These are convenience macros for converting `f64` to the `$unsigned` type.
// The macros check that the numbers are representable the target type.
#[macro_export]
macro_rules! number_to_unsigned {
    ($unsigned:ident, $value:expr, $high:ty) => {
        if $value > $unsigned::MAX as $high {
            None
        } else {
            Some($value as $unsigned)
        }
    };
}
#[macro_export]
macro_rules! number_to_signed {
    ($signed:ident, $value:expr, $high:ty) => {
        if $value < $signed::MIN as $high || $value > $signed::MAX as $high {
            None
        } else {
            Some($value as $signed)
        }
    };
}