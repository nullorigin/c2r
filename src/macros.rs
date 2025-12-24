#![allow(unused_variables, unused_assignments)]

/// Get current timestamp in nanoseconds since UNIX_EPOCH
#[macro_export]
macro_rules! time {
    () => {{
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    }};
}

/// Get the current function name at compile time
/// This is a simple implementation - for more robust solutions, consider the `stdext` crate
#[macro_export]
macro_rules! function_name {
    () => {{
        fn f() {}
        fn type_name_of<T>(_: T) -> &'static str {
            std::any::type_name::<T>()
        }
        let name = type_name_of(f);
        // Extract just the function name from the full path
        // e.g., "module::submodule::function::f" -> "function"
        name.strip_suffix("::f")
            .unwrap_or(name)
            .rsplit("::")
            .next()
            .unwrap_or(name)
    }};
}

/// Optimized macro for arbitrary-precision numeric types
///
/// Usage:
///   impl_number_type!(U256, 32, unsigned);  // 256-bit unsigned
///   impl_number_type!(I512, 64, signed);    // 512-bit signed
///   impl_number_type!(F256, 32, float);     // 256-bit float
///
/// Features: Full arithmetic, bitwise ops, memory pool access, conversions
#[macro_export]
macro_rules! impl_number_type {
    // Main entry points
    ($name:ident, $byte_count:expr, signed) => {
        impl_number_type!(@base $name, $byte_count);
        impl_number_type!(@signed_extras $name, $byte_count);
    };

    ($name:ident, $byte_count:expr, unsigned) => {
        impl_number_type!(@base $name, $byte_count);
        impl_number_type!(@unsigned_extras $name, $byte_count);
    };

    ($name:ident, $byte_count:expr, float) => {
        impl_number_type!(@float_impl $name, $byte_count);
    };

    // Legacy aliases for backward compatibility
    ($name:ident, $byte_count:expr, signed_int) => {
        impl_number_type!($name, $byte_count, signed);
    };
    ($name:ident, $byte_count:expr, unsigned_int) => {
        impl_number_type!($name, $byte_count, unsigned);
    };
        // Base structure and common methods (shared by signed/unsigned)
    (@base $name:ident, $byte_count:expr) => {
        #[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
        pub struct $name([u8; $byte_count]);

        impl $name {
            pub const BYTE_COUNT: usize = $byte_count;
            pub const BITS: usize = $byte_count * 8;

            #[inline(always)]
            pub const fn new() -> Self { Self([0; $byte_count]) }

            #[inline]
            pub fn from_bytes(bytes: &[u8]) -> Self {
                let mut data = [0u8; $byte_count];
                let len = core::cmp::min(bytes.len(), $byte_count);
                data[..len].copy_from_slice(&bytes[..len]);
                Self(data)
            }

            #[inline]
            pub const fn as_bytes(&self) -> &[u8; $byte_count] { &self.0 }

            #[inline]
            pub fn as_mut_bytes(&mut self) -> &mut [u8; $byte_count] { &mut self.0 }

            #[inline]
            pub fn is_zero(&self) -> bool {
                self.0.iter().all(|&b| b == 0)
            }
                        // Arithmetic operations (wrapping)
            #[inline]
            pub fn wrapping_add(self, other: Self) -> Self {
                let mut result = [0u8; $byte_count];
                let mut carry = 0u16;
                for i in 0..$byte_count {
                    let sum = self.0[i] as u16 + other.0[i] as u16 + carry;
                    result[i] = sum as u8;
                    carry = sum >> 8;
                }
                Self(result)
            }

            #[inline]
            pub fn wrapping_sub(self, other: Self) -> Self {
                let mut result = [0u8; $byte_count];
                let mut borrow = 0i16;
                for i in 0..$byte_count {
                    let diff = self.0[i] as i16 - other.0[i] as i16 - borrow;
                    result[i] = ((diff + 256) & 0xFF) as u8;
                    borrow = if diff < 0 { 1 } else { 0 };
                }
                Self(result)
            }

            pub fn wrapping_mul(self, other: Self) -> Self {
                let mut result = [0u8; $byte_count];
                for i in 0..$byte_count {
                    let mut carry = 0u16;
                    for j in 0..$byte_count {
                        if i + j < $byte_count {
                            let prod = self.0[i] as u16 * other.0[j] as u16
                                     + result[i + j] as u16 + carry;
                            result[i + j] = prod as u8;
                            carry = prod >> 8;
                        }
                    }
                }
                Self(result)
            }
            pub fn wrapping_div(self, other: Self) -> Self {
                if other.is_zero() { panic!("Division by zero"); }
                let mut quotient = [0u8; $byte_count];
                let mut remainder = [0u8; $byte_count];

                for bit_pos in (0..$byte_count * 8).rev() {
                    let byte_idx = bit_pos / 8;
                    let bit_idx = bit_pos % 8;

                    // Shift remainder left
                    let mut carry = 0u8;
                    for i in 0..$byte_count {
                        let temp = (remainder[i] << 1) | carry;
                        carry = remainder[i] >> 7;
                        remainder[i] = temp;
                    }
                    if (self.0[byte_idx] >> bit_idx) & 1 == 1 {
                        remainder[0] |= 1;
                    }

                    // Check if can subtract
                    let mut can_sub = false;
                    for i in (0..$byte_count).rev() {
                        if remainder[i] > other.0[i] { can_sub = true; break; }
                        else if remainder[i] < other.0[i] { break; }
                    }
                    if !can_sub {
                        can_sub = remainder.iter().zip(other.0.iter()).all(|(r, d)| r >= d);
                    }

                    if can_sub {
                        let mut borrow = 0i16;
                        for i in 0..$byte_count {
                            let diff = remainder[i] as i16 - other.0[i] as i16 - borrow;
                            remainder[i] = ((diff + 256) & 0xFF) as u8;
                            borrow = if diff < 0 { 1 } else { 0 };
                        }
                        quotient[byte_idx] |= 1 << bit_idx;
                    }
                }
                Self(quotient)
            }

            #[inline]
            pub fn wrapping_rem(self, other: Self) -> Self {
                if other.is_zero() { panic!("Division by zero"); }
                let quotient = self.wrapping_div(other);
                self.wrapping_sub(quotient.wrapping_mul(other))
            }
                        // Bitwise operations
            #[inline]
            pub fn bitand(self, other: Self) -> Self {
                let mut result = [0u8; $byte_count];
                for i in 0..$byte_count { result[i] = self.0[i] & other.0[i]; }
                Self(result)
            }

            #[inline]
            pub fn bitor(self, other: Self) -> Self {
                let mut result = [0u8; $byte_count];
                for i in 0..$byte_count { result[i] = self.0[i] | other.0[i]; }
                Self(result)
            }

            #[inline]
            pub fn bitxor(self, other: Self) -> Self {
                let mut result = [0u8; $byte_count];
                for i in 0..$byte_count { result[i] = self.0[i] ^ other.0[i]; }
                Self(result)
            }

            #[inline]
            pub fn bitnot(self) -> Self {
                let mut result = [0u8; $byte_count];
                for i in 0..$byte_count { result[i] = !self.0[i]; }
                Self(result)
            }

            #[inline]
            pub fn shl(self, shift: usize) -> Self {
                if shift >= $byte_count * 8 { return Self::new(); }
                let mut result = [0u8; $byte_count];
                let byte_shift = shift / 8;
                let bit_shift = shift % 8;
                for i in 0..$byte_count {
                    if i >= byte_shift {
                        let src = i - byte_shift;
                        result[i] = self.0[src] << bit_shift;
                        if bit_shift > 0 && src > 0 {
                            result[i] |= self.0[src - 1] >> (8 - bit_shift);
                        }
                    }
                }
                Self(result)
            }

            #[inline]
            pub fn shr(self, shift: usize) -> Self {
                if shift >= $byte_count * 8 { return Self::new(); }
                let mut result = [0u8; $byte_count];
                let byte_shift = shift / 8;
                let bit_shift = shift % 8;
                for i in 0..$byte_count {
                    let src = i + byte_shift;
                    if src < $byte_count {
                        result[i] = self.0[src] >> bit_shift;
                        if bit_shift > 0 && src + 1 < $byte_count {
                            result[i] |= self.0[src + 1] << (8 - bit_shift);
                        }
                    }
                }
                Self(result)
            }
                        // Bit counting helpers
            #[inline]
            pub fn leading_zeros(&self) -> u32 {
                let mut count = 0u32;
                for &byte in self.0.iter().rev() {
                    if byte == 0 { count += 8; }
                    else { count += byte.leading_zeros(); break; }
                }
                count
            }

            #[inline]
            pub fn trailing_zeros(&self) -> u32 {
                let mut count = 0u32;
                for &byte in self.0.iter() {
                    if byte == 0 { count += 8; }
                    else { count += byte.trailing_zeros(); break; }
                }
                count
            }

            #[inline]
            pub fn count_ones(&self) -> u32 {
                self.0.iter().map(|&b| b.count_ones()).sum()
            }
        }
                // Standard trait implementations
        impl Default for $name {
            #[inline]
            fn default() -> Self { Self::new() }
        }

        impl core::ops::Add for $name {
            type Output = Self;
            #[inline]
            fn add(self, rhs: Self) -> Self { self.wrapping_add(rhs) }
        }

        impl core::ops::Sub for $name {
            type Output = Self;
            #[inline]
            fn sub(self, rhs: Self) -> Self { self.wrapping_sub(rhs) }
        }

        impl core::ops::Mul for $name {
            type Output = Self;
            #[inline]
            fn mul(self, rhs: Self) -> Self { self.wrapping_mul(rhs) }
        }

        impl core::ops::Div for $name {
            type Output = Self;
            #[inline]
            fn div(self, rhs: Self) -> Self { self.wrapping_div(rhs) }
        }

        impl core::ops::Rem for $name {
            type Output = Self;
            #[inline]
            fn rem(self, rhs: Self) -> Self { self.wrapping_rem(rhs) }
        }

        impl core::ops::BitAnd for $name {
            type Output = Self;
            #[inline]
            fn bitand(self, rhs: Self) -> Self { self.bitand(rhs) }
        }

        impl core::ops::BitOr for $name {
            type Output = Self;
            #[inline]
            fn bitor(self, rhs: Self) -> Self { self.bitor(rhs) }
        }

        impl core::ops::BitXor for $name {
            type Output = Self;
            #[inline]
            fn bitxor(self, rhs: Self) -> Self { self.bitxor(rhs) }
        }

        impl core::ops::Not for $name {
            type Output = Self;
            #[inline]
            fn not(self) -> Self { self.bitnot() }
        }

        impl core::ops::Shl<usize> for $name {
            type Output = Self;
            #[inline]
            fn shl(self, rhs: usize) -> Self { self.shl(rhs) }
        }

        impl core::ops::Shr<usize> for $name {
            type Output = Self;
            #[inline]
            fn shr(self, rhs: usize) -> Self { self.shr(rhs) }
        }

        impl core::ops::Index<usize> for $name {
            type Output = u8;
            #[inline]
            fn index(&self, idx: usize) -> &u8 { &self.0[idx] }
        }

        impl core::ops::IndexMut<usize> for $name {
            #[inline]
            fn index_mut(&mut self, idx: usize) -> &mut u8 { &mut self.0[idx] }
        }
    };
    // Signed-specific operations
    (@signed_extras $name:ident, $byte_count:expr) => {
        impl $name {
            pub const IS_SIGNED: bool = true;

            #[inline]
            pub fn is_negative(&self) -> bool {
                $byte_count > 0 && (self.0[$byte_count - 1] & 0x80) != 0
            }

            #[inline]
            pub fn abs(self) -> Self {
                if self.is_negative() {
                    self.bitnot().wrapping_add(Self::from_bytes(&[1]))
                } else {
                    self
                }
            }

            pub fn from_i128(val: i128) -> Self {
                let mut bytes = [0u8; $byte_count];
                let val_bytes = val.to_le_bytes();
                let fill = if val < 0 { 0xFF } else { 0 };
                for i in 0..$byte_count {
                    bytes[i] = if i < 16 { val_bytes[i] } else { fill };
                }
                Self(bytes)
            }

            pub fn to_i128(&self) -> i128 {
                let mut bytes = [0u8; 16];
                let extend = if self.is_negative() { 0xFF } else { 0 };
                for i in 0..16 {
                    bytes[i] = if i < $byte_count { self.0[i] } else { extend };
                }
                i128::from_le_bytes(bytes)
            }
        }

        impl core::ops::Neg for $name {
            type Output = Self;
            #[inline]
            fn neg(self) -> Self {
                self.bitnot().wrapping_add(Self::from_bytes(&[1]))
            }
        }
    };

    // Unsigned-specific operations
    (@unsigned_extras $name:ident, $byte_count:expr) => {
        impl $name {
            pub const IS_SIGNED: bool = false;

            pub fn from_u128(val: u128) -> Self {
                let mut bytes = [0u8; $byte_count];
                let val_bytes = val.to_le_bytes();
                for i in 0..core::cmp::min($byte_count, 16) {
                    bytes[i] = val_bytes[i];
                }
                Self(bytes)
            }

            pub fn to_u128(&self) -> u128 {
                let mut bytes = [0u8; 16];
                for i in 0..core::cmp::min(16, $byte_count) {
                    bytes[i] = self.0[i];
                }
                u128::from_le_bytes(bytes)
            }
        }
    };
        // Float implementation (simplified - stores bytes, basic ops)
    (@float_impl $name:ident, $byte_count:expr) => {
        #[derive(Copy, Clone, PartialEq, PartialOrd, Debug)]
        pub struct $name([u8; $byte_count]);

        impl $name {
            pub const BYTE_COUNT: usize = $byte_count;
            pub const BITS: usize = $byte_count * 8;

            #[inline(always)]
            pub const fn new() -> Self { Self([0; $byte_count]) }

            #[inline]
            pub fn from_bytes(bytes: &[u8]) -> Self {
                let mut data = [0u8; $byte_count];
                let len = core::cmp::min(bytes.len(), $byte_count);
                data[..len].copy_from_slice(&bytes[..len]);
                Self(data)
            }

            pub fn from_f64(val: f64) -> Self {
                let mut bytes = [0u8; $byte_count];
                let val_bytes = val.to_le_bytes();
                for i in 0..core::cmp::min($byte_count, 8) {
                    bytes[i] = val_bytes[i];
                }
                Self(bytes)
            }

            pub fn to_f64(&self) -> f64 {
                let mut bytes = [0u8; 8];
                for i in 0..core::cmp::min(8, $byte_count) {
                    bytes[i] = self.0[i];
                }
                f64::from_le_bytes(bytes)
            }

            #[inline]
            pub const fn as_bytes(&self) -> &[u8; $byte_count] { &self.0 }

            #[inline]
            pub fn as_mut_bytes(&mut self) -> &mut [u8; $byte_count] { &mut self.0 }

            #[inline]
            pub fn is_zero(&self) -> bool {
                self.0.iter().all(|&b| b == 0)
            }

            #[inline]
            pub fn bitxor(self, other: Self) -> Self {
                let mut result = [0u8; $byte_count];
                for i in 0..$byte_count { result[i] = self.0[i] ^ other.0[i]; }
                Self(result)
            }

            #[inline]
            pub fn abs(self) -> Self {
                let mut result = self;
                if $byte_count >= 8 {
                    result.0[7] &= 0x7F;
                }
                result
            }

            pub fn is_nan(&self) -> bool {
                if $byte_count < 8 { return false; }
                let exp = ((self.0[7] & 0x7F) as u16) << 4 | ((self.0[6] & 0xF0) >> 4) as u16;
                let mantissa = (0..6).fold(0u64, |acc, i| acc | ((self.0[i] as u64) << (i * 8)));
                exp == 0x7FF && mantissa != 0
            }

            pub fn is_infinite(&self) -> bool {
                if $byte_count < 8 { return false; }
                let exp = ((self.0[7] & 0x7F) as u16) << 4 | ((self.0[6] & 0xF0) >> 4) as u16;
                let mantissa = (0..6).fold(0u64, |acc, i| acc | ((self.0[i] as u64) << (i * 8)));
                exp == 0x7FF && mantissa == 0
            }
        }

        impl Default for $name {
            #[inline]
            fn default() -> Self { Self::new() }
        }

        impl core::ops::Index<usize> for $name {
            type Output = u8;
            #[inline]
            fn index(&self, idx: usize) -> &u8 { &self.0[idx] }
        }

        impl core::ops::IndexMut<usize> for $name {
            #[inline]
            fn index_mut(&mut self, idx: usize) -> &mut u8 { &mut self.0[idx] }
        }
    }
}