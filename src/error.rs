use core::option::Option::Some;
use std::cmp::Eq;
use std::cmp::Ord;
use std::cmp::PartialEq;
use std::cmp::PartialOrd;
use std::fmt::Debug;
use std::hash::Hash;
use std::{cmp::Ordering, fmt::Display};
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct C2RError(Kind, Reason, Option<String>);

pub type Result<T> = std::result::Result<T, C2RError>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Kind {
    Io = 1,
    Int = 2,
    Float = 3,
    Format = 4,
    Network = 5,
    Json = 6,
    Logic = 7,
    Other = 9,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Reason {
    Abort(&'static str),
    Aborted(&'static str),
    Access(&'static str),
    Accessible(&'static str),
    Activate(&'static str),
    Activated(&'static str),
    Active(&'static str),
    Add(&'static str),
    Added(&'static str),
    Addition(&'static str),
    Addressable(&'static str),
    Address(&'static str),
    Already(&'static str),
    Allow(&'static str),
    Allowed(&'static str),
    Archive(&'static str),
    Archived(&'static str),
    Array(&'static str),
    Argument(&'static str),
    Arguments(&'static str),
    Bad(&'static str),
    Bind(&'static str),
    Block(&'static str),
    Blocked(&'static str),
    Bound(&'static str),
    Broken(&'static str),
    Busy(&'static str),
    Cache(&'static str),
    Cached(&'static str),
    Can(&'static str),
    Cannot(&'static str),
    Change(&'static str),
    Changed(&'static str),
    Character(&'static str),
    Clock(&'static str),
    Close(&'static str),
    Closed(&'static str),
    Conflict(&'static str),
    Connection(&'static str),
    Count(&'static str),
    Crosses(&'static str),
    Crossed(&'static str),
    Dead(&'static str),
    Deadlock(&'static str),
    Deny(&'static str),
    Denied(&'static str),
    Dependency(&'static str),
    Dependent(&'static str),
    Depth(&'static str),
    Digit(&'static str),
    Direct(&'static str),
    Directory(&'static str),
    Disallow(&'static str),
    Disallowed(&'static str),
    Disconnect(&'static str),
    Disconnected(&'static str),
    Divide(&'static str),
    Divided(&'static str),
    Division(&'static str),
    Duplicate(&'static str),
    Empty(&'static str),
    End(&'static str),
    Ended(&'static str),
    Eof(&'static str),
    Error(&'static str),
    Executable(&'static str),
    Exceeded(&'static str),
    Expected(&'static str),
    Exists(&'static str),
    Exit(&'static str),
    Exited(&'static str),
    External(&'static str),
    Failed(&'static str),
    File(&'static str),
    FileDescriptor(&'static str),
    Filesystem(&'static str),
    General(&'static str),
    Get(&'static str),
    Good(&'static str),
    Group(&'static str),
    Grow(&'static str),
    Hardware(&'static str),
    Hash(&'static str),
    Hashable(&'static str),
    Hashed(&'static str),
    Host(&'static str),
    Hostname(&'static str),
    Id(&'static str),
    Illegal(&'static str),
    Impossible(&'static str),
    Improbable(&'static str),
    Incompatible(&'static str),
    Incomplete(&'static str),
    Inconsistent(&'static str),
    Index(&'static str),
    Indexed(&'static str),
    Infinity(&'static str),
    Infinite(&'static str),
    Infintesimal(&'static str),
    Information(&'static str),
    InProgress(&'static str),
    Input(&'static str),
    Insert(&'static str),
    Inserted(&'static str),
    Install(&'static str),
    Installed(&'static str),
    Instance(&'static str),
    Instant(&'static str),
    Instantiated(&'static str),
    Internal(&'static str),
    Interrupt(&'static str),
    Interrupted(&'static str),
    Invalid(&'static str),
    Is(&'static str),
    IsNot(&'static str),
    Json(&'static str),
    Key(&'static str),
    Kill(&'static str),
    Killed(&'static str),
    Last(&'static str),
    Leak(&'static str),
    Leaked(&'static str),
    Left(&'static str),
    Length(&'static str),
    Limit(&'static str),
    Link(&'static str),
    Linked(&'static str),
    Linkability(&'static str),
    Lock(&'static str),
    Locked(&'static str),
    Log(&'static str),
    Logged(&'static str),
    Loop(&'static str),
    Mail(&'static str),
    Memory(&'static str),
    Message(&'static str),
    Missing(&'static str),
    Module(&'static str),
    More(&'static str),
    Multiple(&'static str),
    Multiply(&'static str),
    Multiplied(&'static str),
    Multiplication(&'static str),
    Mutex(&'static str),
    Name(&'static str),
    Named(&'static str),
    Negate(&'static str),
    Negativity(&'static str),
    Negative(&'static str),
    Negated(&'static str),
    Network(&'static str),
    NoneOf(&'static str),
    None(&'static str),
    Nonexistent(&'static str),
    Not(&'static str),
    Only(&'static str),
    Open(&'static str),
    Opened(&'static str),
    Other(&'static str),
    OutOf(&'static str),
    Output(&'static str),
    Outside(&'static str),
    Overflow(&'static str),
    Overflowed(&'static str),
    Overflowing(&'static str),
    Parse(&'static str),
    Parseable(&'static str),
    Parsed(&'static str),
    Parser(&'static str),
    Path(&'static str),
    Pause(&'static str),
    Paused(&'static str),
    Permission(&'static str),
    Port(&'static str),
    Positive(&'static str),
    Positivity(&'static str),
    Pop(&'static str),
    Possible(&'static str),
    Process(&'static str),
    Protocol(&'static str),
    Queue(&'static str),
    Question(&'static str),
    Questionable(&'static str),
    Quota(&'static str),
    Push(&'static str),
    Range(&'static str),
    Ranged(&'static str),
    Read(&'static str),
    Readable(&'static str),
    Readability(&'static str),
    Recover(&'static str),
    Recovered(&'static str),
    Recursion(&'static str),
    Recursive(&'static str),
    Redirect(&'static str),
    Redirection(&'static str),
    Redundant(&'static str),
    Reference(&'static str),
    Reliability(&'static str),
    Release(&'static str),
    Released(&'static str),
    Resource(&'static str),
    Restart(&'static str),
    Restartable(&'static str),
    Restarted(&'static str),
    Result(&'static str),
    Resume(&'static str),
    Resumed(&'static str),
    Return(&'static str),
    Reverse(&'static str),
    Right(&'static str),
    Root(&'static str),
    Same(&'static str),
    Seek(&'static str),
    Seekable(&'static str),
    Select(&'static str),
    Selected(&'static str),
    Server(&'static str),
    Set(&'static str),
    Sizeable(&'static str),
    Shared(&'static str),
    Shrink(&'static str),
    Signal(&'static str),
    Size(&'static str),
    Skip(&'static str),
    Skippable(&'static str),
    Skipped(&'static str),
    Slice(&'static str),
    Socket(&'static str),
    Stale(&'static str),
    Start(&'static str),
    Started(&'static str),
    State(&'static str),
    Status(&'static str),
    Stop(&'static str),
    Stopped(&'static str),
    Storage(&'static str),
    String(&'static str),
    Subtract(&'static str),
    Subtracted(&'static str),
    Subtraction(&'static str),
    Thread(&'static str),
    Threaded(&'static str),
    Time(&'static str),
    Timeout(&'static str),
    Too(&'static str),
    Type(&'static str),
    Unaddressable(&'static str),
    Unavailable(&'static str),
    Unexpected(&'static str),
    Unknown(&'static str),
    Unlinkable(&'static str),
    Unlinked(&'static str),
    Unlocked(&'static str),
    Unreachable(&'static str),
    Unread(&'static str),
    Unreadable(&'static str),
    Unseekable(&'static str),
    Unset(&'static str),
    Unsized(&'static str),
    Unstable(&'static str),
    Unsupported(&'static str),
    Unwritable(&'static str),
    Unwritten(&'static str),
    Up(&'static str),
    Update(&'static str),
    Updated(&'static str),
    User(&'static str),
    Value(&'static str),
    Version(&'static str),
    Wait(&'static str),
    Waitable(&'static str),
    Waiting(&'static str),
    Would(&'static str),
    Write(&'static str),
    Writability(&'static str),
    Writable(&'static str),
    Wrong(&'static str),
    Zero(&'static str),
    Zeroed(&'static str),
    Zombie(&'static str),
}
impl C2RError {
    pub fn new(kind: Kind, reason: Reason, message: Option<String>) -> Self {
        Self(kind, reason, message)
    }
    pub fn to_string(&self) -> String {
        if self.2.is_none() {
            format!(
                "{} Error, Reason: {}; time:{:?}",
                self.0.to_string(),
                self.1.to_string(),
                std::time::Instant::now()
            )
        } else {
            format!(
                "{} Error. Reason: {}, Message: {}; Time {:?}",
                self.0.to_string(),
                self.1.to_string(),
                self.2.as_ref().unwrap(),
                std::time::Instant::now()
            )
        }
    }
    pub fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
    pub fn category(&self) -> Kind {
        self.0.clone()
    }
    pub fn kind(&self) -> Reason {
        self.1.clone()
    }
    pub fn description(&self) -> String {
        self.to_string()
    }
}
impl Default for C2RError {
    fn default() -> Self {
        Self(Kind::Other, Reason::Other("other error"), None)
    }
}
impl Display for C2RError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}
impl std::error::Error for C2RError {}
impl Ord for C2RError {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.cmp(&other.0).then_with(|| self.1.cmp(&other.1))
    }
}
impl PartialOrd for C2RError {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl From<std::fmt::Error> for C2RError {
    fn from(e: std::fmt::Error) -> Self {
        C2RError(Kind::Format, Reason::Invalid("format"), Some(e.to_string()))
    }
}
impl From<C2RError> for std::fmt::Error {
    fn from(_e: C2RError) -> Self {
        std::fmt::Error {}
    }
}
impl From<std::num::ParseIntError> for C2RError {
    fn from(e: std::num::ParseIntError) -> C2RError {
        match e.kind() {
            std::num::IntErrorKind::Empty => {
                C2RError(Kind::Int, Reason::Empty(""), Some(e.to_string()))
            }
            std::num::IntErrorKind::InvalidDigit => {
                C2RError(Kind::Int, Reason::Digit("is invalid"), Some(e.to_string()))
            }
            std::num::IntErrorKind::PosOverflow => C2RError(
                Kind::Int,
                Reason::Overflow("of positive type"),
                Some(e.to_string()),
            ),
            std::num::IntErrorKind::NegOverflow => C2RError(
                Kind::Int,
                Reason::Overflow("of negative type"),
                Some(e.to_string()),
            ),
            std::num::IntErrorKind::Zero => {
                C2RError(Kind::Int, Reason::Size("is zero"), Some(e.to_string()))
            }
            _ => C2RError(
                Kind::Int,
                Reason::Other("integer error"),
                Some(e.to_string()),
            ),
        }
    }
}
impl From<C2RError> for std::num::ParseIntError {
    fn from(e: C2RError) -> std::num::ParseIntError {
        match e {
            C2RError(Kind::Int, _, _) => std::num::ParseIntError::try_from(e).unwrap(),
            _ => panic!("unhandled error: {}", e.to_string()),
        }
    }
}
impl From<std::num::ParseFloatError> for C2RError {
    fn from(e: std::num::ParseFloatError) -> Self {
        match e.to_string().as_str() {
            "cannot parse float from empty string" => {
                C2RError(Kind::Float, Reason::Empty("string"), Some(e.to_string()))
            }
            "invalid float literal" => {
                C2RError(Kind::Float, Reason::Invalid("float"), Some(e.to_string()))
            }
            _ => C2RError(
                Kind::Float,
                Reason::Other("float parsing error"),
                Some(e.to_string()),
            ),
        }
    }
}
impl From<C2RError> for std::num::ParseFloatError {
    fn from(e: C2RError) -> Self {
        match e {
            C2RError(Kind::Float, _, _) => std::num::ParseFloatError::try_from(e).unwrap(),
            _ => panic!("unhandled error: {}", e.to_string()),
        }
    }
}
impl From<std::io::Error> for C2RError {
    fn from(e: std::io::Error) -> Self {
        match e.kind() {
            std::io::ErrorKind::AddrInUse => {
                C2RError(Kind::Io, Reason::Address("in use"), Some(e.to_string()))
            }
            std::io::ErrorKind::AddrNotAvailable => C2RError(
                Kind::Io,
                Reason::Address("not available"),
                Some(e.to_string()),
            ),
            std::io::ErrorKind::AlreadyExists => {
                C2RError(Kind::Io, Reason::Exists("already"), Some(e.to_string()))
            }
            std::io::ErrorKind::ArgumentListTooLong => {
                C2RError(Kind::Io, Reason::Length("too long"), Some(e.to_string()))
            }
            std::io::ErrorKind::BrokenPipe => {
                C2RError(Kind::Io, Reason::Broken("pipe"), Some(e.to_string()))
            }
            std::io::ErrorKind::ConnectionAborted => {
                C2RError(Kind::Io, Reason::Connection("aborted"), Some(e.to_string()))
            }
            std::io::ErrorKind::ConnectionRefused => {
                C2RError(Kind::Io, Reason::Connection("refused"), Some(e.to_string()))
            }
            std::io::ErrorKind::ConnectionReset => {
                C2RError(Kind::Io, Reason::Connection("reset"), Some(e.to_string()))
            }
            std::io::ErrorKind::CrossesDevices => {
                C2RError(Kind::Io, Reason::Crosses("devices"), Some(e.to_string()))
            }
            std::io::ErrorKind::Deadlock => C2RError(
                Kind::Io,
                Reason::Deadlock("from unknown origin"),
                Some(e.to_string()),
            ),
            std::io::ErrorKind::DirectoryNotEmpty => C2RError(
                Kind::Io,
                Reason::Not("an empty directory"),
                Some(e.to_string()),
            ),
            std::io::ErrorKind::ExecutableFileBusy => C2RError(
                Kind::Io,
                Reason::Executable("file busy"),
                Some(e.to_string()),
            ),
            std::io::ErrorKind::FileTooLarge => {
                C2RError(Kind::Io, Reason::File("too large"), Some(e.to_string()))
            }
            std::io::ErrorKind::HostUnreachable => {
                C2RError(Kind::Io, Reason::Host("unreachable"), Some(e.to_string()))
            }
            std::io::ErrorKind::Interrupted => C2RError(
                Kind::Io,
                Reason::Interrupted("operation"),
                Some(e.to_string()),
            ),
            std::io::ErrorKind::InvalidData => {
                C2RError(Kind::Io, Reason::Invalid("data"), Some(e.to_string()))
            }
            std::io::ErrorKind::InvalidFilename => {
                C2RError(Kind::Io, Reason::Invalid("filename"), Some(e.to_string()))
            }
            std::io::ErrorKind::InvalidInput => {
                C2RError(Kind::Io, Reason::Invalid("input"), Some(e.to_string()))
            }
            std::io::ErrorKind::IsADirectory => {
                C2RError(Kind::Io, Reason::Is("a directory"), Some(e.to_string()))
            }
            std::io::ErrorKind::NetworkDown => {
                C2RError(Kind::Io, Reason::Network("down"), Some(e.to_string()))
            }
            std::io::ErrorKind::NetworkUnreachable => C2RError(
                Kind::Io,
                Reason::Network("unreachable"),
                Some(e.to_string()),
            ),
            std::io::ErrorKind::NotADirectory => {
                C2RError(Kind::Io, Reason::Not("a directory"), Some(e.to_string()))
            }
            std::io::ErrorKind::NotConnected => {
                C2RError(Kind::Io, Reason::Not("connected"), Some(e.to_string()))
            }
            std::io::ErrorKind::NotFound => {
                C2RError(Kind::Io, Reason::Not("found"), Some(e.to_string()))
            }
            std::io::ErrorKind::NotSeekable => {
                C2RError(Kind::Io, Reason::Not("seekable"), Some(e.to_string()))
            }
            std::io::ErrorKind::OutOfMemory => C2RError(
                Kind::Io,
                Reason::Memory("limit exceeded"),
                Some(e.to_string()),
            ),
            std::io::ErrorKind::PermissionDenied => {
                C2RError(Kind::Io, Reason::Permission("denied"), Some(e.to_string()))
            }
            std::io::ErrorKind::QuotaExceeded => {
                C2RError(Kind::Io, Reason::Quota("exceeded"), Some(e.to_string()))
            }
            std::io::ErrorKind::ReadOnlyFilesystem => C2RError(
                Kind::Io,
                Reason::Read("only filesystem"),
                Some(e.to_string()),
            ),
            std::io::ErrorKind::ResourceBusy => {
                C2RError(Kind::Io, Reason::Resource("busy"), Some(e.to_string()))
            }
            std::io::ErrorKind::StaleNetworkFileHandle => C2RError(
                Kind::Io,
                Reason::Stale("network file handle"),
                Some(e.to_string()),
            ),
            std::io::ErrorKind::StorageFull => {
                C2RError(Kind::Io, Reason::Storage("full"), Some(e.to_string()))
            }
            std::io::ErrorKind::TimedOut => C2RError(
                Kind::Io,
                Reason::Time("limit exceeded"),
                Some(e.to_string()),
            ),
            std::io::ErrorKind::TooManyLinks => {
                C2RError(Kind::Io, Reason::Too("many links"), Some(e.to_string()))
            }
            std::io::ErrorKind::UnexpectedEof => {
                C2RError(Kind::Io, Reason::Eof("unexpected"), Some(e.to_string()))
            }
            std::io::ErrorKind::Unsupported => C2RError(
                Kind::Io,
                Reason::Unsupported("operation"),
                Some(e.to_string()),
            ),
            std::io::ErrorKind::WouldBlock => C2RError(
                Kind::Io,
                Reason::Would("block input/output"),
                Some(e.to_string()),
            ),
            std::io::ErrorKind::WriteZero => C2RError(
                Kind::Io,
                Reason::Write("with zero bytes written"),
                Some(e.to_string()),
            ),
            std::io::ErrorKind::Other => C2RError(
                Kind::Io,
                Reason::Other("unspecified error"),
                Some(e.to_string()),
            ),
            _ => C2RError(Kind::Io, Reason::Invalid("error"), Some(e.to_string())),
        }
    }
}
impl From<C2RError> for std::io::Error {
    fn from(e: C2RError) -> Self {
        match e {
            C2RError(Kind::Io, Reason::Address("in use"), _) => {
                std::io::Error::new(std::io::ErrorKind::AddrInUse, e)
            }
            C2RError(Kind::Io, Reason::Address("not available"), _) => {
                std::io::Error::new(std::io::ErrorKind::AddrNotAvailable, e)
            }
            C2RError(Kind::Io, Reason::Already("exists"), _) => {
                std::io::Error::new(std::io::ErrorKind::AlreadyExists, e)
            }
            C2RError(Kind::Io, Reason::Argument("list too long"), _) => {
                std::io::Error::new(std::io::ErrorKind::ArgumentListTooLong, e)
            }
            C2RError(Kind::Io, Reason::Broken("pipe"), _) => {
                std::io::Error::new(std::io::ErrorKind::BrokenPipe, e)
            }
            C2RError(Kind::Io, Reason::Connection("aborted"), _) => {
                std::io::Error::new(std::io::ErrorKind::ConnectionAborted, e)
            }
            C2RError(Kind::Io, Reason::Connection("refused"), _) => {
                std::io::Error::new(std::io::ErrorKind::ConnectionRefused, e)
            }
            C2RError(Kind::Io, Reason::Connection("reset"), _) => {
                std::io::Error::new(std::io::ErrorKind::ConnectionReset, e)
            }
            C2RError(Kind::Io, Reason::Crosses("devices"), _) => {
                std::io::Error::new(std::io::ErrorKind::CrossesDevices, e)
            }
            C2RError(Kind::Io, Reason::Deadlock(_), _) => {
                std::io::Error::new(std::io::ErrorKind::Deadlock, e)
            }
            C2RError(Kind::Io, Reason::Directory("not empty"), _) => {
                std::io::Error::new(std::io::ErrorKind::DirectoryNotEmpty, e)
            }
            C2RError(Kind::Io, Reason::Executable("file busy"), _) => {
                std::io::Error::new(std::io::ErrorKind::ExecutableFileBusy, e)
            }
            C2RError(Kind::Io, Reason::File("too large"), _) => {
                std::io::Error::new(std::io::ErrorKind::FileTooLarge, e)
            }
            C2RError(Kind::Io, Reason::Host("unreachable"), _) => {
                std::io::Error::new(std::io::ErrorKind::HostUnreachable, e)
            }
            C2RError(Kind::Io, Reason::Interrupted(_), _) => {
                std::io::Error::new(std::io::ErrorKind::Interrupted, e)
            }
            C2RError(Kind::Io, Reason::Invalid("data"), _) => {
                std::io::Error::new(std::io::ErrorKind::InvalidData, e)
            }
            C2RError(Kind::Io, Reason::Invalid("filename"), _) => {
                std::io::Error::new(std::io::ErrorKind::InvalidFilename, e)
            }
            C2RError(Kind::Io, Reason::Invalid("input"), _) => {
                std::io::Error::new(std::io::ErrorKind::InvalidInput, e)
            }
            C2RError(Kind::Io, Reason::Is("a directory"), _) => {
                std::io::Error::new(std::io::ErrorKind::IsADirectory, e)
            }
            C2RError(Kind::Io, Reason::Network("down"), _) => {
                std::io::Error::new(std::io::ErrorKind::NetworkDown, e)
            }
            C2RError(Kind::Io, Reason::Network("unreachable"), _) => {
                std::io::Error::new(std::io::ErrorKind::NetworkUnreachable, e)
            }
            C2RError(Kind::Io, Reason::Not("a directory"), _) => {
                std::io::Error::new(std::io::ErrorKind::NotADirectory, e)
            }
            C2RError(Kind::Io, Reason::Not("connected"), _) => {
                std::io::Error::new(std::io::ErrorKind::NotConnected, e)
            }
            C2RError(Kind::Io, Reason::Not("found"), _) => {
                std::io::Error::new(std::io::ErrorKind::NotFound, e)
            }
            C2RError(Kind::Io, Reason::Not("seekable"), _) => {
                std::io::Error::new(std::io::ErrorKind::NotSeekable, e)
            }
            C2RError(Kind::Io, Reason::OutOf("memory"), _) => {
                std::io::Error::new(std::io::ErrorKind::OutOfMemory, e)
            }
            C2RError(Kind::Io, Reason::Permission("denied"), _) => {
                std::io::Error::new(std::io::ErrorKind::PermissionDenied, e)
            }
            C2RError(Kind::Io, Reason::Quota("exceeded"), _) => {
                std::io::Error::new(std::io::ErrorKind::QuotaExceeded, e)
            }
            C2RError(Kind::Io, Reason::Filesystem("read-only"), _) => {
                std::io::Error::new(std::io::ErrorKind::ReadOnlyFilesystem, e)
            }
            C2RError(Kind::Io, Reason::Resource("busy"), _) => {
                std::io::Error::new(std::io::ErrorKind::ResourceBusy, e)
            }
            C2RError(Kind::Io, Reason::Stale("network file handle"), _) => {
                std::io::Error::new(std::io::ErrorKind::StaleNetworkFileHandle, e)
            }
            C2RError(Kind::Io, Reason::Storage("full"), _) => {
                std::io::Error::new(std::io::ErrorKind::StorageFull, e)
            }
            C2RError(Kind::Io, Reason::Time("ran out"), _) => {
                std::io::Error::new(std::io::ErrorKind::TimedOut, e)
            }
            C2RError(Kind::Io, Reason::Too("many links"), _) => {
                std::io::Error::new(std::io::ErrorKind::TooManyLinks, e)
            }
            C2RError(Kind::Io, Reason::Unexpected("eof"), _) => {
                std::io::Error::new(std::io::ErrorKind::UnexpectedEof, e)
            }
            C2RError(Kind::Io, Reason::Unsupported(_), _) => {
                std::io::Error::new(std::io::ErrorKind::Unsupported, e)
            }
            C2RError(Kind::Io, Reason::Would("block"), _) => {
                std::io::Error::new(std::io::ErrorKind::WouldBlock, e)
            }
            C2RError(Kind::Io, Reason::Write("zero"), _) => {
                std::io::Error::new(std::io::ErrorKind::WriteZero, e)
            }
            C2RError(Kind::Io, Reason::Other(e), _) => {
                std::io::Error::new(std::io::ErrorKind::Other, e)
            }
            _ => std::io::Error::new(std::io::ErrorKind::Other, e),
        }
    }
}
impl From<std::num::TryFromIntError> for C2RError {
    fn from(error: std::num::TryFromIntError) -> Self {
        C2RError::new(
            Kind::Int,
            Reason::Failed("try from int error"),
            Some(error.to_string()),
        )
    }
}
impl From<Box<dyn std::error::Error + Send + Sync>> for C2RError {
    fn from(e: Box<dyn std::error::Error + Send + Sync>) -> Self {
        C2RError::new(Kind::Other, Reason::Other("error"), Some(e.to_string()))
    }
}
impl Kind {
    pub fn as_str(&self) -> &'static str {
        match self {
            Kind::Io => "io",
            Kind::Int => "int",
            Kind::Float => "float",
            Kind::Format => "format",
            Kind::Network => "network",
            Kind::Json => "json",
            Kind::Logic => "logic",
            Kind::Other => "other",
        }
    }
}
impl Display for Kind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}
impl Ord for Kind {
    fn cmp(&self, other: &Self) -> Ordering {
        (*self as u8).cmp(&(*other as u8))
    }
}
impl PartialOrd for Kind {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Reason {
    pub fn as_str(&self) -> String {
        match self {
            Reason::Abort(s) => ["Abort ", s].join(""),
            Reason::Aborted(s) => ["Aborted ", s].join(""),
            Reason::Access(s) => ["Access ", s].join(""),
            Reason::Accessible(s) => ["Accessible ", s].join(""),
            Reason::Activate(s) => ["Activate ", s].join(""),
            Reason::Activated(s) => ["Activated ", s].join(""),
            Reason::Active(s) => ["Active ", s].join(""),
            Reason::Add(s) => ["Add ", s].join(""),
            Reason::Added(s) => ["Added ", s].join(""),
            Reason::Addition(s) => ["Addition ", s].join(""),
            Reason::Addressable(s) => ["Addressable ", s].join(""),
            Reason::Address(s) => ["Address ", s].join(""),
            Reason::Already(s) => ["Already ", s].join(""),
            Reason::Allow(s) => ["Allow ", s].join(""),
            Reason::Allowed(s) => ["Allowed ", s].join(""),
            Reason::Archive(s) => ["Archive ", s].join(""),
            Reason::Archived(s) => ["Archived ", s].join(""),
            Reason::Array(s) => ["Array ", s].join(""),
            Reason::Argument(s) => ["Argument ", s].join(""),
            Reason::Arguments(s) => ["Arguments ", s].join(""),
            Reason::Bad(s) => ["Bad ", s].join(""),
            Reason::Bind(s) => ["Bind ", s].join(""),
            Reason::Block(s) => ["Block ", s].join(""),
            Reason::Blocked(s) => ["Blocked ", s].join(""),
            Reason::Bound(s) => ["Bound ", s].join(""),
            Reason::Broken(s) => ["Broken ", s].join(""),
            Reason::Busy(s) => ["Busy ", s].join(""),
            Reason::Cache(s) => ["Cache ", s].join(""),
            Reason::Cached(s) => ["Cached ", s].join(""),
            Reason::Can(s) => ["Can ", s].join(""),
            Reason::Cannot(s) => ["Cannot ", s].join(""),
            Reason::Change(s) => ["Change ", s].join(""),
            Reason::Changed(s) => ["Changed ", s].join(""),
            Reason::Character(s) => ["Character ", s].join(""),
            Reason::Clock(s) => ["Clock ", s].join(""),
            Reason::Close(s) => ["Close ", s].join(""),
            Reason::Closed(s) => ["Closed ", s].join(""),
            Reason::Conflict(s) => ["Conflict ", s].join(""),
            Reason::Connection(s) => ["Connection ", s].join(""),
            Reason::Count(s) => ["Count ", s].join(""),
            Reason::Crosses(s) => ["Crosses ", s].join(""),
            Reason::Crossed(s) => ["Crossed ", s].join(""),
            Reason::Dead(s) => ["Dead ", s].join(""),
            Reason::Deadlock(s) => ["Deadlock ", s].join(""),
            Reason::Deny(s) => ["Deny ", s].join(""),
            Reason::Denied(s) => ["Denied ", s].join(""),
            Reason::Dependency(s) => ["Dependency ", s].join(""),
            Reason::Dependent(s) => ["Dependent ", s].join(""),
            Reason::Depth(s) => ["Depth ", s].join(""),
            Reason::Digit(s) => ["Digit ", s].join(""),
            Reason::Direct(s) => ["Direct ", s].join(""),
            Reason::Directory(s) => ["Directory ", s].join(""),
            Reason::Disallow(s) => ["Disallow ", s].join(""),
            Reason::Disallowed(s) => ["Disallowed ", s].join(""),
            Reason::Disconnect(s) => ["Disconnect ", s].join(""),
            Reason::Disconnected(s) => ["Disconnected ", s].join(""),
            Reason::Divide(s) => ["Divide ", s].join(""),
            Reason::Divided(s) => ["Divided ", s].join(""),
            Reason::Division(s) => ["Division ", s].join(""),
            Reason::Duplicate(s) => ["Duplicate ", s].join(""),
            Reason::Empty(s) => ["Empty ", s].join(""),
            Reason::End(s) => ["End ", s].join(""),
            Reason::Ended(s) => ["Ended ", s].join(""),
            Reason::Eof(s) => ["Eof ", s].join(""),
            Reason::Error(s) => ["Error ", s].join(""),
            Reason::Executable(s) => ["Executable ", s].join(""),
            Reason::Exceeded(s) => ["Exceeded ", s].join(""),
            Reason::Expected(s) => ["Expected ", s].join(""),
            Reason::Exists(s) => ["Exists ", s].join(""),
            Reason::Exit(s) => ["Exit ", s].join(""),
            Reason::Exited(s) => ["Exited ", s].join(""),
            Reason::External(s) => ["External ", s].join(""),
            Reason::Failed(s) => ["Failed ", s].join(""),
            Reason::File(s) => ["File ", s].join(""),
            Reason::FileDescriptor(s) => ["FileDescriptor ", s].join(""),
            Reason::Filesystem(s) => ["Filesystem ", s].join(""),
            Reason::General(s) => ["General ", s].join(""),
            Reason::Get(s) => ["Get ", s].join(""),
            Reason::Good(s) => ["Good ", s].join(""),
            Reason::Group(s) => ["Group ", s].join(""),
            Reason::Grow(s) => ["Grow ", s].join(""),
            Reason::Hardware(s) => ["Hardware ", s].join(""),
            Reason::Hash(s) => ["Hash ", s].join(""),
            Reason::Hashable(s) => ["Hashable ", s].join(""),
            Reason::Hashed(s) => ["Hashed ", s].join(""),
            Reason::Host(s) => ["Host ", s].join(""),
            Reason::Hostname(s) => ["Hostname ", s].join(""),
            Reason::Id(s) => ["Id ", s].join(""),
            Reason::Illegal(s) => ["Illegal ", s].join(""),
            Reason::Impossible(s) => ["Impossible ", s].join(""),
            Reason::Improbable(s) => ["Improbable ", s].join(""),
            Reason::Incompatible(s) => ["Incompatible ", s].join(""),
            Reason::Incomplete(s) => ["Incomplete ", s].join(""),
            Reason::Inconsistent(s) => ["Inconsistent ", s].join(""),
            Reason::Index(s) => ["Index ", s].join(""),
            Reason::Indexed(s) => ["Indexed ", s].join(""),
            Reason::Infinity(s) => ["Infinity ", s].join(""),
            Reason::Infinite(s) => ["Infinite ", s].join(""),
            Reason::Infintesimal(s) => ["Infintesimal ", s].join(""),
            Reason::Information(s) => ["Information ", s].join(""),
            Reason::InProgress(s) => ["InProgress ", s].join(""),
            Reason::Input(s) => ["Input ", s].join(""),
            Reason::Insert(s) => ["Insert ", s].join(""),
            Reason::Inserted(s) => ["Inserted ", s].join(""),
            Reason::Install(s) => ["Install ", s].join(""),
            Reason::Installed(s) => ["Installed ", s].join(""),
            Reason::Instance(s) => ["Instance ", s].join(""),
            Reason::Instant(s) => ["Instant ", s].join(""),
            Reason::Instantiated(s) => ["Instantiated ", s].join(""),
            Reason::Internal(s) => ["Internal ", s].join(""),
            Reason::Interrupt(s) => ["Interrupt ", s].join(""),
            Reason::Interrupted(s) => ["Interrupted ", s].join(""),
            Reason::Invalid(s) => ["Invalid ", s].join(""),
            Reason::Is(s) => ["Is ", s].join(""),
            Reason::IsNot(s) => ["IsNot ", s].join(""),
            Reason::Json(s) => ["Json ", s].join(""),
            Reason::Key(s) => ["Key ", s].join(""),
            Reason::Kill(s) => ["Kill ", s].join(""),
            Reason::Killed(s) => ["Killed ", s].join(""),
            Reason::Last(s) => ["Last ", s].join(""),
            Reason::Leak(s) => ["Leak ", s].join(""),
            Reason::Leaked(s) => ["Leaked ", s].join(""),
            Reason::Left(s) => ["Left ", s].join(""),
            Reason::Length(s) => ["Length ", s].join(""),
            Reason::Limit(s) => ["Limit ", s].join(""),
            Reason::Link(s) => ["Link ", s].join(""),
            Reason::Linked(s) => ["Linked ", s].join(""),
            Reason::Linkability(s) => ["Linkability ", s].join(""),
            Reason::Lock(s) => ["Lock ", s].join(""),
            Reason::Locked(s) => ["Locked ", s].join(""),
            Reason::Log(s) => ["Log ", s].join(""),
            Reason::Logged(s) => ["Logged ", s].join(""),
            Reason::Loop(s) => ["Loop ", s].join(""),
            Reason::Mail(s) => ["Mail ", s].join(""),
            Reason::Memory(s) => ["Memory ", s].join(""),
            Reason::Message(s) => ["Message ", s].join(""),
            Reason::Missing(s) => ["Missing ", s].join(""),
            Reason::Module(s) => ["Module ", s].join(""),
            Reason::More(s) => ["More ", s].join(""),
            Reason::Multiple(s) => ["Multiple ", s].join(""),
            Reason::Multiply(s) => ["Multiply ", s].join(""),
            Reason::Multiplied(s) => ["Multiplied ", s].join(""),
            Reason::Multiplication(s) => ["Multiplication ", s].join(""),
            Reason::Mutex(s) => ["Mutex ", s].join(""),
            Reason::Name(s) => ["Name ", s].join(""),
            Reason::Named(s) => ["Named ", s].join(""),
            Reason::Negate(s) => ["Negate ", s].join(""),
            Reason::Negativity(s) => ["Negativity ", s].join(""),
            Reason::Negative(s) => ["Negative ", s].join(""),
            Reason::Negated(s) => ["Negated ", s].join(""),
            Reason::Network(s) => ["Network ", s].join(""),
            Reason::NoneOf(s) => ["NoneOf ", s].join(""),
            Reason::None(s) => ["None ", s].join(""),
            Reason::Nonexistent(s) => ["Nonexistent ", s].join(""),
            Reason::Not(s) => ["Not ", s].join(""),
            Reason::Only(s) => ["Only ", s].join(""),
            Reason::Open(s) => ["Open ", s].join(""),
            Reason::Opened(s) => ["Opened ", s].join(""),
            Reason::Other(s) => ["Other ", s].join(""),
            Reason::OutOf(s) => ["OutOf ", s].join(""),
            Reason::Output(s) => ["Output ", s].join(""),
            Reason::Outside(s) => ["Outside ", s].join(""),
            Reason::Overflow(s) => ["Overflow ", s].join(""),
            Reason::Overflowed(s) => ["Overflowed ", s].join(""),
            Reason::Overflowing(s) => ["Overflowing ", s].join(""),
            Reason::Parse(s) => ["Parse ", s].join(""),
            Reason::Parseable(s) => ["Parseable ", s].join(""),
            Reason::Parsed(s) => ["Parsed ", s].join(""),
            Reason::Parser(s) => ["Parser ", s].join(""),
            Reason::Path(s) => ["Path ", s].join(""),
            Reason::Pause(s) => ["Pause ", s].join(""),
            Reason::Paused(s) => ["Paused ", s].join(""),
            Reason::Permission(s) => ["Permission ", s].join(""),
            Reason::Port(s) => ["Port ", s].join(""),
            Reason::Positive(s) => ["Positive ", s].join(""),
            Reason::Positivity(s) => ["Positivity ", s].join(""),
            Reason::Pop(s) => ["Pop ", s].join(""),
            Reason::Possible(s) => ["Possible ", s].join(""),
            Reason::Process(s) => ["Process ", s].join(""),
            Reason::Protocol(s) => ["Protocol ", s].join(""),
            Reason::Queue(s) => ["Queue ", s].join(""),
            Reason::Question(s) => ["Question ", s].join(""),
            Reason::Questionable(s) => ["Questionable ", s].join(""),
            Reason::Quota(s) => ["Quota ", s].join(""),
            Reason::Push(s) => ["Push ", s].join(""),
            Reason::Range(s) => ["Range ", s].join(""),
            Reason::Ranged(s) => ["Ranged ", s].join(""),
            Reason::Read(s) => ["Read ", s].join(""),
            Reason::Readable(s) => ["Readable ", s].join(""),
            Reason::Readability(s) => ["Readability ", s].join(""),
            Reason::Recover(s) => ["Recover ", s].join(""),
            Reason::Recovered(s) => ["Recovered ", s].join(""),
            Reason::Recursion(s) => ["Recursion ", s].join(""),
            Reason::Recursive(s) => ["Recursive ", s].join(""),
            Reason::Redirect(s) => ["Redirect ", s].join(""),
            Reason::Redirection(s) => ["Redirection ", s].join(""),
            Reason::Redundant(s) => ["Redundant ", s].join(""),
            Reason::Reference(s) => ["Reference ", s].join(""),
            Reason::Reliability(s) => ["Reliability ", s].join(""),
            Reason::Release(s) => ["Release ", s].join(""),
            Reason::Released(s) => ["Released ", s].join(""),
            Reason::Resource(s) => ["Resource ", s].join(""),
            Reason::Restart(s) => ["Restart ", s].join(""),
            Reason::Restartable(s) => ["Restartable ", s].join(""),
            Reason::Restarted(s) => ["Restarted ", s].join(""),
            Reason::Result(s) => ["Result ", s].join(""),
            Reason::Resume(s) => ["Resume ", s].join(""),
            Reason::Resumed(s) => ["Resumed ", s].join(""),
            Reason::Return(s) => ["Return ", s].join(""),
            Reason::Reverse(s) => ["Reverse ", s].join(""),
            Reason::Right(s) => ["Right ", s].join(""),
            Reason::Root(s) => ["Root ", s].join(""),
            Reason::Same(s) => ["Same ", s].join(""),
            Reason::Seek(s) => ["Seek ", s].join(""),
            Reason::Seekable(s) => ["Seekable ", s].join(""),
            Reason::Select(s) => ["Select ", s].join(""),
            Reason::Selected(s) => ["Selected ", s].join(""),
            Reason::Server(s) => ["Server ", s].join(""),
            Reason::Set(s) => ["Set ", s].join(""),
            Reason::Sizeable(s) => ["Sizeable ", s].join(""),
            Reason::Shared(s) => ["Shared ", s].join(""),
            Reason::Shrink(s) => ["Shrink ", s].join(""),
            Reason::Signal(s) => ["Signal ", s].join(""),
            Reason::Size(s) => ["Size ", s].join(""),
            Reason::Skip(s) => ["Skip ", s].join(""),
            Reason::Skippable(s) => ["Skippable ", s].join(""),
            Reason::Skipped(s) => ["Skipped ", s].join(""),
            Reason::Slice(s) => ["Slice ", s].join(""),
            Reason::Socket(s) => ["Socket ", s].join(""),
            Reason::Stale(s) => ["Stale ", s].join(""),
            Reason::Start(s) => ["Start ", s].join(""),
            Reason::Started(s) => ["Started ", s].join(""),
            Reason::State(s) => ["State ", s].join(""),
            Reason::Status(s) => ["Status ", s].join(""),
            Reason::Stop(s) => ["Stop ", s].join(""),
            Reason::Stopped(s) => ["Stopped ", s].join(""),
            Reason::Storage(s) => ["Storage ", s].join(""),
            Reason::String(s) => ["String ", s].join(""),
            Reason::Subtract(s) => ["Subtract ", s].join(""),
            Reason::Subtracted(s) => ["Subtracted ", s].join(""),
            Reason::Subtraction(s) => ["Subtraction ", s].join(""),
            Reason::Thread(s) => ["Thread ", s].join(""),
            Reason::Threaded(s) => ["Threaded ", s].join(""),
            Reason::Time(s) => ["Time ", s].join(""),
            Reason::Timeout(s) => ["Timeout ", s].join(""),
            Reason::Too(s) => ["Too ", s].join(""),
            Reason::Type(s) => ["Type ", s].join(""),
            Reason::Unaddressable(s) => ["Unaddressable ", s].join(""),
            Reason::Unavailable(s) => ["Unavailable ", s].join(""),
            Reason::Unexpected(s) => ["Unexpected ", s].join(""),
            Reason::Unknown(s) => ["Unknown ", s].join(""),
            Reason::Unlinkable(s) => ["Unlinkable ", s].join(""),
            Reason::Unlinked(s) => ["Unlinked ", s].join(""),
            Reason::Unlocked(s) => ["Unlocked ", s].join(""),
            Reason::Unreachable(s) => ["Unreachable ", s].join(""),
            Reason::Unread(s) => ["Unread ", s].join(""),
            Reason::Unreadable(s) => ["Unreadable ", s].join(""),
            Reason::Unseekable(s) => ["Unseekable ", s].join(""),
            Reason::Unset(s) => ["Unset ", s].join(""),
            Reason::Unsized(s) => ["Unsized ", s].join(""),
            Reason::Unstable(s) => ["Unstable ", s].join(""),
            Reason::Unsupported(s) => ["Unsupported ", s].join(""),
            Reason::Unwritable(s) => ["Unwritable ", s].join(""),
            Reason::Unwritten(s) => ["Unwritten ", s].join(""),
            Reason::Up(s) => ["Up ", s].join(""),
            Reason::Update(s) => ["Update ", s].join(""),
            Reason::Updated(s) => ["Updated ", s].join(""),
            Reason::User(s) => ["User ", s].join(""),
            Reason::Value(s) => ["Value ", s].join(""),
            Reason::Version(s) => ["Version ", s].join(""),
            Reason::Wait(s) => ["Wait ", s].join(""),
            Reason::Waitable(s) => ["Waitable ", s].join(""),
            Reason::Waiting(s) => ["Waiting ", s].join(""),
            Reason::Would(s) => ["Would ", s].join(""),
            Reason::Write(s) => ["Write ", s].join(""),
            Reason::Writability(s) => ["Writability ", s].join(""),
            Reason::Writable(s) => ["Writable ", s].join(""),
            Reason::Wrong(s) => ["Wrong ", s].join(""),
            Reason::Zero(s) => ["Zero ", s].join(""),
            Reason::Zeroed(s) => ["Zeroed ", s].join(""),
            Reason::Zombie(s) => ["Zombie ", s].join(""),
        }
    }
}
impl Display for Reason {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}
impl Ord for Reason {
    fn cmp(&self, other: &Self) -> Ordering {
        self.to_string().cmp(&other.to_string())
    }
}
impl PartialOrd for Reason {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl From<std::io::ErrorKind> for Reason {
    fn from(kind: std::io::ErrorKind) -> Self {
        match kind {
            std::io::ErrorKind::AddrInUse => Reason::Address("in use"),
            std::io::ErrorKind::AddrNotAvailable => Reason::Address("not available"),
            std::io::ErrorKind::AlreadyExists => Reason::Already("exists"),
            std::io::ErrorKind::ArgumentListTooLong => Reason::Argument("list too long"),
            std::io::ErrorKind::BrokenPipe => Reason::Broken("pipe"),
            std::io::ErrorKind::ConnectionAborted => Reason::Connection("aborted"),
            std::io::ErrorKind::ConnectionRefused => Reason::Connection("refused"),
            std::io::ErrorKind::ConnectionReset => Reason::Connection("reset"),
            std::io::ErrorKind::CrossesDevices => Reason::Crosses("devices"),
            std::io::ErrorKind::Deadlock => Reason::Deadlock(""),
            std::io::ErrorKind::DirectoryNotEmpty => Reason::Directory("not empty"),
            std::io::ErrorKind::ExecutableFileBusy => Reason::Executable("file busy"),
            std::io::ErrorKind::FileTooLarge => Reason::File("too large"),
            std::io::ErrorKind::HostUnreachable => Reason::Host("unreachable"),
            std::io::ErrorKind::Interrupted => Reason::Interrupted(""),
            std::io::ErrorKind::InvalidData => Reason::Invalid("data"),
            std::io::ErrorKind::InvalidFilename => Reason::Invalid("filename"),
            std::io::ErrorKind::InvalidInput => Reason::Invalid("input"),
            std::io::ErrorKind::IsADirectory => Reason::Is("a directory"),
            std::io::ErrorKind::NetworkDown => Reason::Network("down"),
            std::io::ErrorKind::NetworkUnreachable => Reason::Network("unreachable"),
            std::io::ErrorKind::NotADirectory => Reason::Not("a directory"),
            std::io::ErrorKind::NotConnected => Reason::Not("connected"),
            std::io::ErrorKind::NotFound => Reason::Not("found"),
            std::io::ErrorKind::NotSeekable => Reason::Not("seekable"),
            std::io::ErrorKind::OutOfMemory => Reason::OutOf("memory"),
            std::io::ErrorKind::PermissionDenied => Reason::Permission("denied"),
            std::io::ErrorKind::QuotaExceeded => Reason::Quota("exceeded"),
            std::io::ErrorKind::ReadOnlyFilesystem => Reason::Filesystem("read-only"),
            std::io::ErrorKind::ResourceBusy => Reason::Resource("busy"),
            std::io::ErrorKind::StaleNetworkFileHandle => Reason::Stale("network file handle"),
            std::io::ErrorKind::StorageFull => Reason::Storage("full"),
            std::io::ErrorKind::TimedOut => Reason::Time("ran out"),
            std::io::ErrorKind::TooManyLinks => Reason::Too("many links"),
            std::io::ErrorKind::UnexpectedEof => Reason::Eof("unexpected"),
            std::io::ErrorKind::Unsupported => Reason::Unsupported(""),
            std::io::ErrorKind::WouldBlock => Reason::Would("block"),
            std::io::ErrorKind::WriteZero => Reason::Write("zero"),
            _ => Reason::Other("unknown"),
        }
    }
}
impl From<Reason> for std::io::ErrorKind {
    fn from(kind: Reason) -> Self {
        match kind {
            Reason::Address("in use") => std::io::ErrorKind::AddrInUse,
            Reason::Address("not available") => std::io::ErrorKind::AddrNotAvailable,
            Reason::Exists("already") => std::io::ErrorKind::AlreadyExists,
            Reason::Argument("list too long") => std::io::ErrorKind::ArgumentListTooLong,
            Reason::Broken("pipe") => std::io::ErrorKind::BrokenPipe,
            Reason::Connection("aborted") => std::io::ErrorKind::ConnectionAborted,
            Reason::Connection("refused") => std::io::ErrorKind::ConnectionRefused,
            Reason::Connection("reset") => std::io::ErrorKind::ConnectionReset,
            Reason::Crosses("devices") => std::io::ErrorKind::CrossesDevices,
            Reason::Deadlock(_) => std::io::ErrorKind::Deadlock,
            Reason::Directory("not empty") => std::io::ErrorKind::DirectoryNotEmpty,
            Reason::Executable("file busy") => std::io::ErrorKind::ExecutableFileBusy,
            Reason::File("too large") => std::io::ErrorKind::FileTooLarge,
            Reason::Host("unreachable") => std::io::ErrorKind::HostUnreachable,
            Reason::Interrupted(_) => std::io::ErrorKind::Interrupted,
            Reason::Invalid("data") => std::io::ErrorKind::InvalidData,
            Reason::Invalid("filename") => std::io::ErrorKind::InvalidFilename,
            Reason::Invalid("input") => std::io::ErrorKind::InvalidInput,
            Reason::Is("a directory") => std::io::ErrorKind::IsADirectory,
            Reason::Network("down") => std::io::ErrorKind::NetworkDown,
            Reason::Network("unreachable") => std::io::ErrorKind::NetworkUnreachable,
            Reason::Not("a directory") => std::io::ErrorKind::NotADirectory,
            Reason::Not("connected") => std::io::ErrorKind::NotConnected,
            Reason::Not("found") => std::io::ErrorKind::NotFound,
            Reason::Not("seekable") => std::io::ErrorKind::NotSeekable,
            Reason::OutOf("memory") => std::io::ErrorKind::OutOfMemory,
            Reason::Permission("denied") => std::io::ErrorKind::PermissionDenied,
            Reason::Quota("exceeded") => std::io::ErrorKind::QuotaExceeded,
            Reason::Filesystem("read-only") => std::io::ErrorKind::ReadOnlyFilesystem,
            Reason::Resource("busy") => std::io::ErrorKind::ResourceBusy,
            Reason::Stale("network file handle") => std::io::ErrorKind::StaleNetworkFileHandle,
            Reason::Storage("full") => std::io::ErrorKind::StorageFull,
            Reason::Time("ran out") => std::io::ErrorKind::TimedOut,
            Reason::Too("many links") => std::io::ErrorKind::TooManyLinks,
            Reason::Unexpected("eof") => std::io::ErrorKind::UnexpectedEof,
            Reason::Unsupported(_) => std::io::ErrorKind::Unsupported,
            Reason::Would("block") => std::io::ErrorKind::WouldBlock,
            Reason::Write("zero") => std::io::ErrorKind::WriteZero,
            _ => std::io::ErrorKind::Other,
        }
    }
}
impl From<std::num::IntErrorKind> for Reason {
    fn from(kind: std::num::IntErrorKind) -> Self {
        match kind {
            std::num::IntErrorKind::Empty => Reason::Empty("value"),
            std::num::IntErrorKind::PosOverflow => Reason::Overflow("of positive type"),
            std::num::IntErrorKind::NegOverflow => Reason::Overflow("of negative type"),
            std::num::IntErrorKind::InvalidDigit => Reason::Digit("invalid"),
            std::num::IntErrorKind::Zero => Reason::Zero("as divisor"),
            _ => Reason::Other("unknown integer error"),
        }
    }
}
impl From<Reason> for std::num::IntErrorKind {
    fn from(kind: Reason) -> Self {
        match kind {
            Reason::Empty("value") => std::num::IntErrorKind::Empty,
            Reason::Overflow("of positive type") => std::num::IntErrorKind::PosOverflow,
            Reason::Overflow("of negative type") => std::num::IntErrorKind::NegOverflow,
            Reason::Digit("is invalid") => std::num::IntErrorKind::InvalidDigit,
            Reason::Zero("as divisor") => std::num::IntErrorKind::Zero,
            _ => panic!("unhandled error: {}", kind.to_string()),
        }
    }
}
pub fn err<T>(kind: Kind, reason: Reason, message: &str) -> Result<T> {
    if message.is_empty() {
        return Err(C2RError::new(
            kind,
            reason,
            Some("unknown error".to_string()),
        ));
    }
    Err(C2RError::new(kind, reason, Some(message.to_string())))
}
