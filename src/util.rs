use crate::{
    Result,
    error::{Error, Kind, Reason},
};
use std::{
    collections::VecDeque,
    fmt::{Debug, Display, Formatter},
    fs::File,
    io::{Read, Write},
    iter::FusedIterator,
    mem,
    ops::{Bound, Deref, DerefMut, Index, IndexMut, Mul, RangeBounds, Sub},
    path::PathBuf,
    ptr, slice,
    str::FromStr,
};

// ============================================================================
// Paths - Collection of filesystem paths with depth constraints
// ============================================================================

#[derive(Clone, Eq, PartialEq, Default)]
pub struct Paths {
    paths: Vec<PathBuf>,
    max_depth: usize,
    min_depth: usize,
}
impl Paths {
    pub fn new() -> Self {
        Self {
            paths: Vec::new(),
            max_depth: 10,
            min_depth: 1,
        }
    }

    pub fn get_at(&self, index: usize) -> Result<&PathBuf> {
        self.paths.get(index).ok_or(Error::new(
            Kind::Io,
            Reason::Not("found"),
            Some("Path not found".to_string()),
        ))
    }

    pub fn set_at(&mut self, path: PathBuf, index: usize) -> Result<()> {
        if !path.exists() {
            return Err(Error::new(
                Kind::Io,
                Reason::Not("found"),
                Some("Path not found".to_string()),
            ));
        }
        let count = path.components().count();
        if count < self.min_depth || count > self.max_depth {
            return Err(Error::new(
                Kind::Io,
                Reason::Invalid("depth"),
                Some("Path depth is out of range".to_string()),
            ));
        }
        if index >= self.paths.len() {
            self.paths.resize_with(index + 1, PathBuf::new);
        }
        self.paths[index] = path;
        Ok(())
    }

    pub fn set(&mut self, paths: Vec<PathBuf>) -> Result<()> {
        self.paths.clear();
        for path in paths {
            if !path.exists() {
                return Err(Error::new(
                    Kind::Io,
                    Reason::Not("found"),
                    Some("Path not found".to_string()),
                ));
            }
            let count = path.components().count();
            if count < self.min_depth || count > self.max_depth {
                return Err(Error::new(
                    Kind::Io,
                    Reason::Invalid("depth"),
                    Some("Path depth is out of range".to_string()),
                ));
            }
            self.paths.push(path);
        }
        Ok(())
    }

    pub fn max_depth(&self) -> usize {
        self.max_depth
    }
    pub fn set_max_depth(&mut self, depth: usize) {
        self.max_depth = depth;
    }
    pub fn min_depth(&self) -> usize {
        self.min_depth
    }
    pub fn set_min_depth(&mut self, depth: usize) {
        self.min_depth = depth;
    }

    pub fn paths(&self) -> &[PathBuf] {
        &self.paths
    }

    pub fn set_paths(&mut self, paths: Vec<PathBuf>) -> Result<()> {
        let filtered: Vec<_> = paths
            .into_iter()
            .filter(|p| {
                let c = p.components().count();
                c >= self.min_depth && c <= self.max_depth
            })
            .collect();

        if filtered.is_empty() {
            return Err(Error::new(
                Kind::Io,
                Reason::Invalid("paths"),
                Some("No valid paths found".to_string()),
            ));
        }
        self.paths = filtered;
        self.paths.sort_unstable();
        self.paths.dedup();
        Ok(())
    }

    pub fn read(&self, index: usize) -> Result<String> {
        let path = self.get_at(index)?;
        if !path.exists() || !path.is_file() {
            return Err(Error::new(
                Kind::Io,
                Reason::Not("found"),
                Some("Path is not a file".to_string()),
            ));
        }
        let mut buf = String::new();
        File::open(path)?.read_to_string(&mut buf)?;
        Ok(buf)
    }

    pub fn write(&self, index: usize, contents: &str) -> Result<()> {
        let path = self.get_at(index)?;
        if !path.exists() || !path.is_file() {
            return Err(Error::new(
                Kind::Io,
                Reason::Invalid("path"),
                Some("Path is not a file".to_string()),
            ));
        }
        File::create(path)?.write_all(contents.as_bytes())?;
        Ok(())
    }

    pub fn read_all(&self) -> Result<Vec<String>> {
        self.paths
            .iter()
            .map(|path| self.read(self.paths.iter().position(|x| x == path).unwrap()))
            .collect()
    }

    pub fn write_all(&self, contents: &[String]) -> Result<()> {
        if contents.len() != self.paths.len() {
            return Err(Error::new(
                Kind::Io,
                Reason::Length("invalid"),
                Some("Contents length does not match paths length".to_string()),
            ));
        }
        for (i, content) in contents.iter().enumerate() {
            self.write(i, content)?;
        }
        Ok(())
    }

    pub fn exists_count(&self) -> usize {
        self.paths.iter().filter(|p| p.exists()).count()
    }
    pub fn files_count(&self) -> usize {
        self.paths.iter().filter(|p| p.is_file()).count()
    }
    pub fn dirs_count(&self) -> usize {
        self.paths.iter().filter(|p| p.is_dir()).count()
    }
    pub fn symlinks_count(&self) -> usize {
        self.paths.iter().filter(|p| p.is_symlink()).count()
    }

    pub fn exists(&self) -> bool {
        self.paths.iter().all(|p| p.exists())
    }
    pub fn is_all_files(&self) -> bool {
        !self.paths.is_empty() && self.paths.iter().all(|p| p.is_file())
    }
    pub fn is_all_dirs(&self) -> bool {
        !self.paths.is_empty() && self.paths.iter().all(|p| p.is_dir())
    }
    pub fn is_all_symlinks(&self) -> bool {
        !self.paths.is_empty() && self.paths.iter().all(|p| p.is_symlink())
    }
    pub fn is_all_files_or_dirs(&self) -> bool {
        !self.paths.is_empty() && self.paths.iter().all(|p| p.is_file() || p.is_dir())
    }
    pub fn is_all_files_or_symlinks(&self) -> bool {
        !self.paths.is_empty() && self.paths.iter().all(|p| p.is_file() || p.is_symlink())
    }
    pub fn is_all_dirs_or_symlinks(&self) -> bool {
        !self.paths.is_empty() && self.paths.iter().all(|p| p.is_dir() || p.is_symlink())
    }
}

impl Index<usize> for Paths {
    type Output = PathBuf;
    fn index(&self, index: usize) -> &Self::Output {
        &self.paths[index]
    }
}
impl IndexMut<usize> for Paths {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.paths[index]
    }
}
impl<'a> Index<usize> for &'a Paths {
    type Output = PathBuf;
    fn index(&self, index: usize) -> &Self::Output {
        &self.paths[index]
    }
}
impl Deref for Paths {
    type Target = [PathBuf];
    fn deref(&self) -> &Self::Target {
        self.paths.as_slice()
    }
}
impl DerefMut for Paths {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.paths.as_mut_slice()
    }
}
impl From<Vec<PathBuf>> for Paths {
    fn from(paths: Vec<PathBuf>) -> Self {
        let mut pv = Self::new();
        pv.paths = paths;
        pv
    }
}
impl FromStr for Paths {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        let paths: Vec<PathBuf> = s
            .lines()
            .filter(|l| !l.trim().is_empty())
            .map(PathBuf::from)
            .collect();
        match !paths.is_empty() {
            true => Ok(Self::from(paths)),
            false => Err(Error::new(
                Kind::Io,
                Reason::Invalid("input"),
                Some("No valid paths".to_string()),
            )),
        }
    }
}

impl Debug for Paths {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_struct("PathVec")
            .field("paths", &self.paths)
            .field("max_depth", &self.max_depth)
            .field("min_depth", &self.min_depth)
            .finish()
    }
}

impl Display for Paths {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        for (i, path) in self.paths.iter().enumerate() {
            if i > 0 {
                write!(f, "\n")?;
            }
            write!(f, "{}", path.display())?;
        }
        Ok(())
    }
}

const SIGNIFICAND_SIZE: isize = 64;
const DP_SIGNIFICAND_SIZE: isize = 52;
const DP_EXPONENT_BIAS: isize = 0x3FF + DP_SIGNIFICAND_SIZE;
const DP_MIN_EXPONENT: isize = -DP_EXPONENT_BIAS;
const DP_EXPONENT_MASK: u64 = 0x7FF0000000000000;
const DP_SIGNIFICAND_MASK: u64 = 0x000FFFFFFFFFFFFF;
const DP_HIDDEN_BIT: u64 = 0x0010000000000000;

#[derive(Copy, Clone, Debug)]
pub struct FramePointer {
    pub f: u64,
    pub e: isize,
}

impl FramePointer {
    pub fn new(f: u64, e: isize) -> Self {
        FramePointer { f, e }
    }
    pub fn from_f64(d: f64) -> Self {
        let u: u64 = f64::to_bits(d);

        let biased_e = ((u & DP_EXPONENT_MASK) >> DP_SIGNIFICAND_SIZE) as isize;
        let significand = u & DP_SIGNIFICAND_MASK;
        match biased_e != 0 {
            true => FramePointer {
                f: significand + DP_HIDDEN_BIT,
                e: biased_e - DP_EXPONENT_BIAS,
            },
            false => FramePointer {
                f: significand,
                e: DP_MIN_EXPONENT + 1,
            },
        }
    }

    pub fn normalize(self) -> FramePointer {
        let mut res = self;
        while (res.f & (1u64 << 63)) == 0 {
            res.f <<= 1;
            res.e -= 1;
        }
        res
    }
    pub fn normalize_boundary(self) -> FramePointer {
        let mut res = self;
        while (res.f & DP_HIDDEN_BIT << 1) == 0 {
            res.f <<= 1;
            res.e -= 1;
        }
        res.f <<= SIGNIFICAND_SIZE - DP_SIGNIFICAND_SIZE - 2;
        res.e -= SIGNIFICAND_SIZE - DP_SIGNIFICAND_SIZE - 2;
        res
    }

    pub fn normalized_boundaries(self) -> (FramePointer, FramePointer) {
        let pl = FramePointer::new((self.f << 1) + 1, self.e - 1).normalize_boundary();
        let mut mi = match self.f == DP_HIDDEN_BIT {
            true => FramePointer::new((self.f << 2) - 1, self.e - 2),
            false => FramePointer::new((self.f << 1) - 1, self.e - 1),
        };
        mi.f <<= mi.e - pl.e;
        mi.e = pl.e;
        (mi, pl)
    }
}

impl Sub for FramePointer {
    type Output = FramePointer;
    fn sub(self, rhs: FramePointer) -> FramePointer {
        FramePointer {
            f: self.f - rhs.f,
            e: self.e,
        }
    }
}

impl Mul for FramePointer {
    type Output = FramePointer;
    fn mul(self, rhs: FramePointer) -> FramePointer {
        let m32 = 0xFFFFFFFFu64;
        let a = self.f >> 32;
        let b = self.f & m32;
        let c = rhs.f >> 32;
        let d = rhs.f & m32;
        let ac = a * c;
        let bc = b * c;
        let ad = a * d;
        let bd = b * d;
        let mut tmp = (bd >> 32) + (ad & m32) + (bc & m32);
        tmp += 1u64 << 31; // multiply round
        FramePointer {
            f: ac + (ad >> 32) + (bc >> 32) + (tmp >> 32),
            e: self.e + rhs.e + 64,
        }
    }
}

fn get_cached_power_by_index(index: usize) -> FramePointer {
    // 10^-348, 10^-340, ..., 10^340
    static CACHED_POWERS_F: [u64; 87] = [
        0xfa8fd5a0081c0288,
        0xbaaee17fa23ebf76,
        0x8b16fb203055ac76,
        0xcf42894a5dce35ea,
        0x9a6bb0aa55653b2d,
        0xe61acf033d1a45df,
        0xab70fe17c79ac6ca,
        0xff77b1fcbebcdc4f,
        0xbe5691ef416bd60c,
        0x8dd01fad907ffc3c,
        0xd3515c2831559a83,
        0x9d71ac8fada6c9b5,
        0xea9c227723ee8bcb,
        0xaecc49914078536d,
        0x823c12795db6ce57,
        0xc21094364dfb5637,
        0x9096ea6f3848984f,
        0xd77485cb25823ac7,
        0xa086cfcd97bf97f4,
        0xef340a98172aace5,
        0xb23867fb2a35b28e,
        0x84c8d4dfd2c63f3b,
        0xc5dd44271ad3cdba,
        0x936b9fcebb25c996,
        0xdbac6c247d62a584,
        0xa3ab66580d5fdaf6,
        0xf3e2f893dec3f126,
        0xb5b5ada8aaff80b8,
        0x87625f056c7c4a8b,
        0xc9bcff6034c13053,
        0x964e858c91ba2655,
        0xdff9772470297ebd,
        0xa6dfbd9fb8e5b88f,
        0xf8a95fcf88747d94,
        0xb94470938fa89bcf,
        0x8a08f0f8bf0f156b,
        0xcdb02555653131b6,
        0x993fe2c6d07b7fac,
        0xe45c10c42a2b3b06,
        0xaa242499697392d3,
        0xfd87b5f28300ca0e,
        0xbce5086492111aeb,
        0x8cbccc096f5088cc,
        0xd1b71758e219652c,
        0x9c40000000000000,
        0xe8d4a51000000000,
        0xad78ebc5ac620000,
        0x813f3978f8940984,
        0xc097ce7bc90715b3,
        0x8f7e32ce7bea5c70,
        0xd5d238a4abe98068,
        0x9f4f2726179a2245,
        0xed63a231d4c4fb27,
        0xb0de65388cc8ada8,
        0x83c7088e1aab65db,
        0xc45d1df942711d9a,
        0x924d692ca61be758,
        0xda01ee641a708dea,
        0xa26da3999aef774a,
        0xf209787bb47d6b85,
        0xb454e4a179dd1877,
        0x865b86925b9bc5c2,
        0xc83553c5c8965d3d,
        0x952ab45cfa97a0b3,
        0xde469fbd99a05fe3,
        0xa59bc234db398c25,
        0xf6c69a72a3989f5c,
        0xb7dcbf5354e9bece,
        0x88fcf317f22241e2,
        0xcc20ce9bd35c78a5,
        0x98165af37b2153df,
        0xe2a0b5dc971f303a,
        0xa8d9d1535ce3b396,
        0xfb9b7cd9a4a7443c,
        0xbb764c4ca7a44410,
        0x8bab8eefb6409c1a,
        0xd01fef10a657842c,
        0x9b10a4e5e9913129,
        0xe7109bfba19c0c9d,
        0xac2820d9623bf429,
        0x80444b5e7aa7cf85,
        0xbf21e44003acdd2d,
        0x8e679c2f5e44ff8f,
        0xd433179d9c8cb841,
        0x9e19db92b4e31ba9,
        0xeb96bf6ebadf77d9,
        0xaf87023b9bf0ee6b,
    ];
    static CACHED_POWERS_E: [i16; 87] = [
        -1220, -1193, -1166, -1140, -1113, -1087, -1060, -1034, -1007, -980, -954, -927, -901,
        -874, -847, -821, -794, -768, -741, -715, -688, -661, -635, -608, -582, -555, -529, -502,
        -475, -449, -422, -396, -369, -343, -316, -289, -263, -236, -210, -183, -157, -130, -103,
        -77, -50, -24, 3, 30, 56, 83, 109, 136, 162, 189, 216, 242, 269, 295, 322, 348, 375, 402,
        428, 455, 481, 508, 534, 561, 588, 614, 641, 667, 694, 720, 747, 774, 800, 827, 853, 880,
        907, 933, 960, 986, 1013, 1039, 1066,
    ];
    FramePointer::new(CACHED_POWERS_F[index], CACHED_POWERS_E[index] as isize)
}

#[inline]
pub fn get_cached_power(e: isize) -> (FramePointer, isize) {
    let dk = (-61 - e) as f64 * 0.30102999566398114f64 + 347f64; // dk must be positive, so can do ceiling in positive
    let mut k = dk as isize;
    if dk - k as f64 > 0.0 {
        k += 1;
    }

    let index = ((k >> 3) + 1) as usize;
    let k = -(-348 + (index << 3) as isize); // decimal exponent no need lookup table

    (get_cached_power_by_index(index), k)
}
#[inline]
unsafe fn grisu_round(buffer: &mut u64, delta: u64, mut rest: u64, ten_kappa: u64, wp_w: u64) {
    while rest < wp_w
        && delta - rest >= ten_kappa
        && (rest + ten_kappa < wp_w || // closer
        wp_w - rest > rest + ten_kappa - wp_w)
    {
        *buffer -= 1;
        rest += ten_kappa;
    }
}

#[inline]
fn count_decimal_digit32(n: u32) -> i16 {
    match n {
        0..=9 => 1,
        10..=99 => 2,
        100..=999 => 3,
        1000..=9999 => 4,
        10000..=99999 => 5,
        100000..=999999 => 6,
        1000000..=9999999 => 7,
        10000000..=99999999 => 8,
        100000000..=999999999 => 9,
        _ => 10,
    }
}

#[inline]
unsafe fn digit_gen(w: FramePointer, mp: FramePointer, mut delta: u64, mut k: i16) -> (u64, i16) {
    static POW10: [u32; 10] = [
        1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000,
    ];
    let one = FramePointer::new(1u64 << -mp.e, mp.e);
    let wp_w = mp - w;
    let mut p1 = (mp.f >> -one.e) as u32;
    let mut p2 = mp.f & (one.f - 1);
    let mut kappa = count_decimal_digit32(p1); // kappa in [0, 9]

    let mut buffer = p1 as u64;

    while kappa > 0 {
        match kappa {
            9 => {
                p1 %= 100000000;
            }
            8 => {
                p1 %= 10000000;
            }
            7 => {
                p1 %= 1000000;
            }
            6 => {
                p1 %= 100000;
            }
            5 => {
                p1 %= 10000;
            }
            4 => {
                p1 %= 1000;
            }
            3 => {
                p1 %= 100;
            }
            2 => {
                p1 %= 10;
            }
            1 => {
                p1 = 0;
            }
            _ => {}
        }
        kappa = kappa.wrapping_sub(1);
        let tmp = ((p1 as u64) << -one.e) + p2;
        if tmp <= delta {
            k += kappa;
            let pow10 = POW10[kappa as usize] as u64;
            buffer /= pow10;

            unsafe { grisu_round(&mut buffer, delta, tmp, pow10 << -one.e, wp_w.f) };
            return (buffer, k);
        }
    }

    loop {
        p2 *= 10;
        delta *= 10;
        let d = (p2 >> -one.e) as u8;
        if d != 0 || buffer != 0 {
            buffer = buffer * 10 + d as u64;
        }
        p2 &= one.f - 1;
        kappa = kappa.wrapping_sub(1);
        if p2 < delta {
            k += kappa;
            let index = -(kappa as isize);

            unsafe {
                grisu_round(
                    &mut buffer,
                    delta,
                    p2,
                    one.f,
                    wp_w.f
                        * match index < 9 {
                            true => POW10[-(kappa as isize) as usize] as u64,
                            false => 0,
                        },
                )
            };
            return (buffer, k);
        }
    }
}

#[inline]
pub fn convert(float: f64) -> (u64, i16) {
    if float == 0.0 {
        return (0, 0);
    }
    unsafe {
        let v = FramePointer::from_f64(float);
        let (w_m, w_p) = v.normalized_boundaries();
        let (c_mk, k) = get_cached_power(w_p.e);
        let w = v.normalize() * c_mk;
        let mut wp = w_p * c_mk;
        let mut wm = w_m * c_mk;
        wm.f += 1;
        wp.f -= 1;

        digit_gen(w, wp, wp.f - wm.f, k as i16)
    }
}
const DEC_DIGITS_LUT: &'static [u8] = b"0001020304050607080910111213141516171819      2021222324252627282930313233343536373839      4041424344454647484950515253545556575859      6061626364656667686970717273747576777879      8081828384858687888990919293949596979899";

const ZEROFILL: &'static [u8] = &[b'0'; 20];

#[inline(always)]
unsafe fn write_num(n: &mut u64, curr: &mut isize, buf_ptr: *mut u8, lut_ptr: *const u8) {
    // eagerly decode 4 digits at a time
    while *n >= 10000 {
        let rem = (*n % 10000) as isize;
        *n /= 10000;

        let d1 = (rem / 100) << 1;
        let d2 = (rem % 100) << 1;
        *curr -= 4;
        unsafe { ptr::copy_nonoverlapping(lut_ptr.offset(d1), buf_ptr.offset(*curr), 2) };
        unsafe { ptr::copy_nonoverlapping(lut_ptr.offset(d2), buf_ptr.offset(*curr + 2), 2) };
    }

    // decode 2 more digits
    if *n >= 100 {
        let d1 = ((*n % 100) << 1) as isize;
        *n /= 100;
        *curr -= 2;
        unsafe { ptr::copy_nonoverlapping(lut_ptr.offset(d1), buf_ptr.offset(*curr), 2) };
    }

    // decode last 1 or 2 digits
    match *n < 10 {
        true => {
            *curr -= 1;
            unsafe { *buf_ptr.offset(*curr) = (*n as u8) + b'0' };
        }
        false => {
            let d1 = (*n << 1) as isize;
            *curr -= 2;
            unsafe { ptr::copy_nonoverlapping(lut_ptr.offset(d1), buf_ptr.offset(*curr), 2) };
        }
    }
}

pub fn write<W: Write>(wr: &mut W, positive: bool, mut n: u64, exponent: i16) -> Result<()> {
    if !positive {
        wr.write_all(b"-")?;
    }

    if n == 0 {
        wr.write_all(b"0")?;
    }

    const BUF_LEN: usize = 30;
    let mut buf = mem::MaybeUninit::<[u8; BUF_LEN]>::uninit();
    let mut curr = BUF_LEN as isize;
    let buf_ptr = buf.as_mut_ptr() as *mut u8;
    let lut_ptr = DEC_DIGITS_LUT.as_ptr();

    if exponent == 0 {
        unsafe { write_num(&mut n, &mut curr, buf_ptr, lut_ptr) };

        return unsafe {
            wr.write_all(slice::from_raw_parts(
                buf_ptr.offset(curr),
                BUF_LEN - curr as usize,
            ))
            .map_err(|e| Error::new(Kind::Io, Reason::Write("failed"), Some(e.to_string())))
        };
    } else if exponent < 0 {
        let mut e = safe_abs(exponent);

        // Decimal number with a fraction that's fully printable
        if e < 18 {
            // eagerly decode 4 digits at a time
            for _ in 0..e >> 2 {
                let rem = (n % 10000) as isize;
                n /= 10000;

                let d1 = (rem / 100) << 1;
                let d2 = (rem % 100) << 1;
                curr -= 4;
                unsafe { ptr::copy_nonoverlapping(lut_ptr.offset(d1), buf_ptr.offset(curr), 2) };
                unsafe {
                    ptr::copy_nonoverlapping(lut_ptr.offset(d2), buf_ptr.offset(curr + 2), 2)
                };
            }

            e &= 3;

            // write the remaining 3, 2 or 1 digits
            if e & 2 == 2 {
                let d1 = ((n % 100) << 1) as isize;
                n /= 100;
                curr -= 2;
                unsafe { ptr::copy_nonoverlapping(lut_ptr.offset(d1), buf_ptr.offset(curr), 2) };
            }

            if e & 1 == 1 {
                curr -= 1;
                unsafe { *buf_ptr.offset(curr) = ((n % 10) as u8) + b'0' };
                n /= 10;
            }

            curr -= 1;
            unsafe { *buf_ptr.offset(curr) = b'.' };

            unsafe { write_num(&mut n, &mut curr, buf_ptr, lut_ptr) };

            return unsafe {
                wr.write_all(slice::from_raw_parts(
                    buf_ptr.offset(curr),
                    BUF_LEN - curr as usize,
                ))
                .map_err(|e| Error::new(Kind::Io, Reason::Write("failed"), Some(e.to_string())))
            };
        }

        // Not easily printable, write down fraction, then full number, then exponent

        // Since we move the decimal point right after the first digit, we have to adjust the
        // exponent part. If the number is long enough, this may result in the exponent switching
        // sign from negative to positive - we have to handle this case separately.
        let mut exponent_positive = false;
        match n < 10 {
            true => {
                curr -= 1;
                unsafe { *buf_ptr.offset(curr) = ((n % 10) as u8) + b'0' };
            }
            false => {
                // eagerly decode 4 digits at a time
                while n >= 100000 {
                    let rem = (n % 10000) as isize;
                    n /= 10000;

                    let d1 = (rem / 100) << 1;
                    let d2 = (rem % 100) << 1;
                    curr -= 4;
                    unsafe {
                        ptr::copy_nonoverlapping(lut_ptr.offset(d1), buf_ptr.offset(curr), 2)
                    };
                    unsafe {
                        ptr::copy_nonoverlapping(lut_ptr.offset(d2), buf_ptr.offset(curr + 2), 2)
                    };
                }

                // decode 2 more digits
                if n >= 1000 {
                    let d1 = ((n % 100) << 1) as isize;
                    n /= 100;
                    curr -= 2;
                    unsafe {
                        ptr::copy_nonoverlapping(lut_ptr.offset(d1), buf_ptr.offset(curr), 2)
                    };
                }

                // decode last 1 or 2 digits
                match n < 100 {
                    true => {
                        curr -= 1;
                        unsafe { *buf_ptr.offset(curr) = ((n % 10) as u8) + b'0' };
                        n /= 10;
                    }
                    false => {
                        let d1 = ((n % 100) << 1) as isize;
                        n /= 100;
                        curr -= 2;
                        unsafe {
                            ptr::copy_nonoverlapping(lut_ptr.offset(d1), buf_ptr.offset(curr), 2)
                        };
                    }
                }

                let printed_so_far = BUF_LEN as u16 - curr as u16;

                match printed_so_far <= e {
                    true => {
                        e -= printed_so_far;
                    }
                    false => {
                        // Same as e = |e - printed_so_far|.
                        e = printed_so_far - e;
                        exponent_positive = true;
                    }
                }

                curr -= 1;
                unsafe { *buf_ptr.offset(curr) = b'.' };

                unsafe { write_num(&mut n, &mut curr, buf_ptr, lut_ptr) };
            }
        }

        // Write out the number with a fraction
        unsafe {
            wr.write_all(slice::from_raw_parts(
                buf_ptr.offset(curr),
                BUF_LEN - curr as usize,
            ))
        }?;

        // Omit the 'e' notation for e == 0
        if e == 0 {
            return Ok(());
        }
        // Write the remaining `e` notation, with proper sign
        match exponent_positive {
            true => wr.write_all(b"e+")?,
            false => wr.write_all(b"e-")?,
        }
        return write(wr, true, e as u64, 0);
    }

    // Exponent greater than 0
    unsafe { write_num(&mut n, &mut curr, buf_ptr, lut_ptr) };
    let printed = BUF_LEN - curr as usize;

    // No need for `e` notation, just print out zeroes
    if (printed + exponent as usize) <= 20 {
        unsafe {
            wr.write_all(slice::from_raw_parts(
                buf_ptr.offset(curr),
                BUF_LEN - curr as usize,
            ))
        }?;

        return wr
            .write_all(&ZEROFILL[..exponent as usize])
            .map_err(|e| Error::new(Kind::Io, Reason::Write("failed"), Some(e.to_string())));
    }

    let mut e = exponent as u64;

    // More than one digit, turn into a fraction
    if printed != 1 {
        unsafe { *buf_ptr.offset(curr - 1) = *buf_ptr.offset(curr) };
        unsafe { *buf_ptr.offset(curr) = b'.' };
        curr -= 1;
        e += (printed as u64) - 1;
    }

    unsafe {
        wr.write_all(slice::from_raw_parts(
            buf_ptr.offset(curr),
            BUF_LEN - curr as usize,
        ))
    }?;
    wr.write_all(b"e")?;
    write(wr, true, e, 0)
}

fn safe_abs(x: i16) -> u16 {
    match x.checked_abs() {
        Some(y) => y as u16,
        None => i16::MAX as u16 + 1u16,
    }
}

fn truncate_string(s: &str, max_len: usize) -> String {
    if s.len() <= max_len {
        format!("{:<width$}", s, width = max_len)
    } else {
        format!(
            "{:<width$}",
            format!("{}...", &s[..max_len - 3]),
            width = max_len
        )
    }
}
