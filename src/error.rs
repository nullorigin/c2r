use core::option::Option::Some;
use std::cmp::Eq;
use std::cmp::Ord;
use std::cmp::PartialEq;
use std::cmp::PartialOrd;
use std::fmt::Debug;
use std::hash::Hash;
use std::{cmp::Ordering, fmt::Display};
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Error(Kind, Reason, Option<String>);

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Kind {
    // IO operations
    IoRead,
    IoWrite,
    IoAccess,
    IoNotFound,
    IoPermission,

    // Network operations
    NetworkConnection,
    NetworkTimeout,
    NetworkHost,

    // Parsing operations
    ParseInt,
    ParseFloat,
    ParseJson,

    // Format operations
    FormatInvalid,

    // Logic operations
    LogicAssertion,
    LogicState,

    // Memory operations
    Memory,
    MemoryLeak,
    MemoryAccess,

    // Threading operations
    Thread,
    ThreadLock,
    ThreadMutex,

    // File operations
    File,
    FileOpen,
    FileClose,

    // System operations
    System,
    SystemCall,
    SystemResource,

    // Configuration operations
    Config,
    ConfigMissing,
    ConfigInvalid,

    // Authentication operations
    Auth,
    AuthPermission,
    AuthAccess,

    // Database operations
    Database,
    DatabaseConnection,
    DatabaseQuery,

    // Cache operations
    Cache,
    CacheExpired,
    CacheMiss,

    // Operation states
    Abort,
    Access,
    Activate,
    Add,
    Address,
    Allow,
    Archive,
    Array,
    Argument,
    Bind,
    Block,
    Bound,
    Broken,
    Busy,
    Change,
    Character,
    Clock,
    Close,
    Conflict,
    Connection,
    Count,
    Cross,
    Dead,
    Deadlock,
    Delete,
    Deny,
    Device,
    Directory,
    Disable,
    Disconnect,
    Disk,
    Drain,
    Drop,
    Empty,
    Enable,
    End,
    Error,
    Event,
    Executable,
    Exists,
    Exit,
    Expire,
    Fail,
    Filesystem,
    Filter,
    Find,
    Finish,
    Flag,
    Flush,
    Format,
    Full,
    Function,
    Generate,
    Group,
    Handle,
    Hash,
    Header,
    Host,
    Index,
    Initialize,
    Input,
    Insert,
    Install,
    Interrupt,
    Invalid,
    Item,
    Json,
    Key,
    Kill,
    Last,
    Leak,
    Left,
    Length,
    Limit,
    Link,
    Lock,
    Log,
    Loop,
    Mail,
    Message,
    Missing,
    Module,
    Multiple,
    Multiply,
    Mutex,
    Name,
    Negate,
    Network,
    New,
    Next,
    Node,
    Null,
    Number,
    Object,
    Open,
    Operation,
    Option,
    Order,
    OutOf,
    Overflow,
    Permission,
    Process,
    Protocol,
    Queue,
    Quota,
    Read,
    Receive,
    Record,
    Reference,
    Register,
    Remove,
    Request,
    Resource,
    Response,
    Right,
    Route,
    Search,
    Security,
    Send,
    Service,
    Session,
    Set,
    Share,
    Signal,
    Size,
    Socket,
    Stale,
    Start,
    State,
    Storage,
    Stream,
    String,
    Sync,
    Table,
    Task,
    Terminate,
    Test,
    Time,
    Timeout,
    Token,
    Transaction,
    Transfer,
    Transform,
    Type,
    Unavailable,
    Undefined,
    Unexpected,
    Unicode,
    Uninitialized,
    Unknown,
    Unlock,
    Unregister,
    Update,
    Upload,
    User,
    Validation,
    Value,
    Version,
    Wait,
    Warning,
    Write,

    // General categories
    Io,
    Int,
    Float,
    Logic,
    Other,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Reason {
    Abort(&'static str),
    Aborted(&'static str),
    Access(&'static str),
    Activate(&'static str),
    Add(&'static str),
    Address(&'static str),
    Already(&'static str),
    Allow(&'static str),
    Archive(&'static str),
    Argument(&'static str),
    Bad(&'static str),
    Bind(&'static str),
    Block(&'static str),
    Bound(&'static str),
    Broken(&'static str),
    Busy(&'static str),
    Cache(&'static str),
    Cannot(&'static str),
    Change(&'static str),
    Character(&'static str),
    Close(&'static str),
    Conflict(&'static str),
    Connection(&'static str),
    Count(&'static str),
    Crosses(&'static str),
    Dead(&'static str),
    Deadlock(&'static str),
    Deny(&'static str),
    Directory(&'static str),
    Disconnect(&'static str),
    Divide(&'static str),
    Duplicate(&'static str),
    Empty(&'static str),
    End(&'static str),
    Eof(&'static str),
    Executable(&'static str),
    Exceeded(&'static str),
    Expected(&'static str),
    Exists(&'static str),
    Exit(&'static str),
    External(&'static str),
    Failed(&'static str),
    File(&'static str),
    Filesystem(&'static str),
    Hash(&'static str),
    Host(&'static str),
    Illegal(&'static str),
    Incomplete(&'static str),
    Index(&'static str),
    Infinite(&'static str),
    Input(&'static str),
    Install(&'static str),
    Internal(&'static str),
    Interrupted(&'static str),
    Invalid(&'static str),
    Is(&'static str),
    Kill(&'static str),
    Leak(&'static str),
    Length(&'static str),
    Limit(&'static str),
    Link(&'static str),
    Lock(&'static str),
    Memory(&'static str),
    Missing(&'static str),
    Multiple(&'static str),
    Mutex(&'static str),
    Name(&'static str),
    Network(&'static str),
    Not(&'static str),
    Null(&'static str),
    Open(&'static str),
    Operation(&'static str),
    OutOf(&'static str),
    Overflow(&'static str),
    Parse(&'static str),
    Path(&'static str),
    Permission(&'static str),
    Pipe(&'static str),
    Pointer(&'static str),
    Port(&'static str),
    Process(&'static str),
    Protocol(&'static str),
    Queue(&'static str),
    Quota(&'static str),
    Range(&'static str),
    Read(&'static str),
    Reference(&'static str),
    Resource(&'static str),
    Security(&'static str),
    Size(&'static str),
    Socket(&'static str),
    Stale(&'static str),
    Storage(&'static str),
    Stream(&'static str),
    System(&'static str),
    Thread(&'static str),
    Timeout(&'static str),
    Type(&'static str),
    Undefined(&'static str),
    Unicode(&'static str),
    Unknown(&'static str),
    Unlock(&'static str),
    Unreachable(&'static str),
    Unsupported(&'static str),
    User(&'static str),
    Value(&'static str),
    Write(&'static str),
    Unexpected(&'static str),
}

impl Error {
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
impl Default for Error {
    fn default() -> Self {
        Self(Kind::Other, Reason::Unknown("unknown error"), None)
    }
}
impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}
impl std::error::Error for Error {}
impl Ord for Error {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.cmp(&other.0).then_with(|| self.1.cmp(&other.1))
    }
}
impl PartialOrd for Error {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl From<std::fmt::Error> for Error {
    fn from(e: std::fmt::Error) -> Self {
        Error::new(Kind::Format, Reason::Invalid("format"), Some(e.to_string()))
    }
}

impl From<Error> for std::fmt::Error {
    fn from(_e: Error) -> Self {
        std::fmt::Error
    }
}

impl From<std::num::ParseIntError> for Error {
    fn from(e: std::num::ParseIntError) -> Self {
        match e.kind() {
            std::num::IntErrorKind::Empty => {
                Error::new(Kind::ParseInt, Reason::Empty("string"), Some(e.to_string()))
            }
            std::num::IntErrorKind::InvalidDigit => Error::new(
                Kind::ParseInt,
                Reason::Invalid("digit"),
                Some(e.to_string()),
            ),
            std::num::IntErrorKind::PosOverflow => Error::new(
                Kind::ParseInt,
                Reason::Overflow("positive"),
                Some(e.to_string()),
            ),
            std::num::IntErrorKind::NegOverflow => Error::new(
                Kind::ParseInt,
                Reason::Overflow("negative"),
                Some(e.to_string()),
            ),
            std::num::IntErrorKind::Zero => Error::new(
                Kind::ParseInt,
                Reason::Divide("by zero"),
                Some(e.to_string()),
            ),
            _ => Error::new(
                Kind::ParseInt,
                Reason::Unknown("integer parsing"),
                Some(e.to_string()),
            ),
        }
    }
}

impl From<std::num::ParseFloatError> for Error {
    fn from(e: std::num::ParseFloatError) -> Self {
        match e.to_string().as_str() {
            "cannot parse float from empty string" => Error(
                Kind::ParseFloat,
                Reason::Empty("string"),
                Some(e.to_string()),
            ),
            "invalid float literal" => Error(
                Kind::ParseFloat,
                Reason::Invalid("float"),
                Some(e.to_string()),
            ),
            _ => Error(
                Kind::ParseFloat,
                Reason::Parse("float parsing error"),
                Some(e.to_string()),
            ),
        }
    }
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        match e.kind() {
            std::io::ErrorKind::AddrInUse => Error(
                Kind::IoAccess,
                Reason::Address("in use"),
                Some(e.to_string()),
            ),
            std::io::ErrorKind::AddrNotAvailable => Error(
                Kind::IoAccess,
                Reason::Address("not available"),
                Some(e.to_string()),
            ),
            std::io::ErrorKind::AlreadyExists => Error(
                Kind::IoAccess,
                Reason::Exists("already"),
                Some(e.to_string()),
            ),
            std::io::ErrorKind::ArgumentListTooLong => Error(
                Kind::Io,
                Reason::Argument("list too long"),
                Some(e.to_string()),
            ),
            std::io::ErrorKind::BrokenPipe => {
                Error(Kind::Io, Reason::Broken("pipe"), Some(e.to_string()))
            }
            std::io::ErrorKind::ConnectionAborted => Error(
                Kind::NetworkConnection,
                Reason::Connection("aborted"),
                Some(e.to_string()),
            ),
            std::io::ErrorKind::ConnectionRefused => Error(
                Kind::NetworkConnection,
                Reason::Connection("refused"),
                Some(e.to_string()),
            ),
            std::io::ErrorKind::ConnectionReset => Error(
                Kind::NetworkConnection,
                Reason::Connection("reset"),
                Some(e.to_string()),
            ),
            std::io::ErrorKind::CrossesDevices => {
                Error(Kind::Io, Reason::Crosses("devices"), Some(e.to_string()))
            }
            std::io::ErrorKind::Deadlock => {
                Error(Kind::Io, Reason::Deadlock("detected"), Some(e.to_string()))
            }
            std::io::ErrorKind::DirectoryNotEmpty => Error(
                Kind::Io,
                Reason::Directory("not empty"),
                Some(e.to_string()),
            ),
            std::io::ErrorKind::ExecutableFileBusy => Error(
                Kind::Io,
                Reason::Executable("file busy"),
                Some(e.to_string()),
            ),
            std::io::ErrorKind::FileTooLarge => {
                Error(Kind::Io, Reason::File("too large"), Some(e.to_string()))
            }
            std::io::ErrorKind::HostUnreachable => Error(
                Kind::NetworkHost,
                Reason::Host("unreachable"),
                Some(e.to_string()),
            ),
            std::io::ErrorKind::Interrupted => Error(
                Kind::Io,
                Reason::Interrupted("operation"),
                Some(e.to_string()),
            ),
            std::io::ErrorKind::InvalidData => {
                Error(Kind::Io, Reason::Invalid("data"), Some(e.to_string()))
            }
            std::io::ErrorKind::InvalidFilename => {
                Error(Kind::Io, Reason::Invalid("filename"), Some(e.to_string()))
            }
            std::io::ErrorKind::InvalidInput => {
                Error(Kind::Io, Reason::Invalid("input"), Some(e.to_string()))
            }
            std::io::ErrorKind::IsADirectory => {
                Error(Kind::Io, Reason::Is("a directory"), Some(e.to_string()))
            }
            std::io::ErrorKind::NetworkDown => {
                Error(Kind::Network, Reason::Network("down"), Some(e.to_string()))
            }
            std::io::ErrorKind::NetworkUnreachable => Error(
                Kind::Network,
                Reason::Network("unreachable"),
                Some(e.to_string()),
            ),
            std::io::ErrorKind::NotADirectory => {
                Error(Kind::Io, Reason::Not("a directory"), Some(e.to_string()))
            }
            std::io::ErrorKind::NotConnected => Error(
                Kind::NetworkConnection,
                Reason::Not("connected"),
                Some(e.to_string()),
            ),
            std::io::ErrorKind::NotFound => {
                Error(Kind::IoNotFound, Reason::Not("found"), Some(e.to_string()))
            }
            std::io::ErrorKind::NotSeekable => {
                Error(Kind::Io, Reason::Not("seekable"), Some(e.to_string()))
            }
            std::io::ErrorKind::OutOfMemory => {
                Error(Kind::Io, Reason::OutOf("memory"), Some(e.to_string()))
            }
            std::io::ErrorKind::PermissionDenied => Error(
                Kind::IoPermission,
                Reason::Permission("denied"),
                Some(e.to_string()),
            ),
            std::io::ErrorKind::QuotaExceeded => {
                Error(Kind::Io, Reason::Quota("exceeded"), Some(e.to_string()))
            }
            std::io::ErrorKind::ReadOnlyFilesystem => Error(
                Kind::Io,
                Reason::Filesystem("read-only"),
                Some(e.to_string()),
            ),
            std::io::ErrorKind::ResourceBusy => {
                Error(Kind::Io, Reason::Busy("resource"), Some(e.to_string()))
            }
            std::io::ErrorKind::StaleNetworkFileHandle => Error(
                Kind::Network,
                Reason::Stale("network file handle"),
                Some(e.to_string()),
            ),
            std::io::ErrorKind::StorageFull => {
                Error(Kind::Io, Reason::Storage("full"), Some(e.to_string()))
            }
            std::io::ErrorKind::TimedOut => Error(
                Kind::NetworkTimeout,
                Reason::Timeout("operation"),
                Some(e.to_string()),
            ),
            std::io::ErrorKind::TooManyLinks => {
                Error(Kind::Io, Reason::Multiple("links"), Some(e.to_string()))
            }
            std::io::ErrorKind::UnexpectedEof => {
                Error(Kind::IoRead, Reason::Unexpected("eof"), Some(e.to_string()))
            }
            std::io::ErrorKind::Unsupported => Error(
                Kind::Io,
                Reason::Unsupported("operation"),
                Some(e.to_string()),
            ),
            std::io::ErrorKind::WouldBlock => {
                Error(Kind::Io, Reason::Block("would block"), Some(e.to_string()))
            }
            std::io::ErrorKind::WriteZero => {
                Error(Kind::IoWrite, Reason::Write("zero"), Some(e.to_string()))
            }
            std::io::ErrorKind::Other => {
                Error(Kind::Io, Reason::Unknown("other"), Some(e.to_string()))
            }
            _ => Error(Kind::Io, Reason::Unknown("unknown"), Some(e.to_string())),
        }
    }
}
impl From<Error> for std::io::Error {
    fn from(e: Error) -> Self {
        match e {
            Error(Kind::Io, Reason::Address("address is in use"), _) => {
                std::io::Error::new(std::io::ErrorKind::AddrInUse, e)
            }
            Error(Kind::Io, Reason::Address("address is not available"), _) => {
                std::io::Error::new(std::io::ErrorKind::AddrNotAvailable, e)
            }
            Error(Kind::Io, Reason::Already("unspecified io exists already"), _) => {
                std::io::Error::new(std::io::ErrorKind::AlreadyExists, e)
            }
            Error(Kind::Io, Reason::Argument("list is too long"), _) => {
                std::io::Error::new(std::io::ErrorKind::ArgumentListTooLong, e)
            }
            Error(Kind::Io, Reason::Broken("pipe is broken"), _) => {
                std::io::Error::new(std::io::ErrorKind::BrokenPipe, e)
            }
            Error(Kind::Io, Reason::Connection("connection has been aborted"), _) => {
                std::io::Error::new(std::io::ErrorKind::ConnectionAborted, e)
            }
            Error(Kind::Io, Reason::Connection("connection has been refused"), _) => {
                std::io::Error::new(std::io::ErrorKind::ConnectionRefused, e)
            }
            Error(Kind::Io, Reason::Connection("connection has been reset"), _) => {
                std::io::Error::new(std::io::ErrorKind::ConnectionReset, e)
            }
            Error(Kind::Io, Reason::Crosses("io operations cross devices"), _) => {
                std::io::Error::new(std::io::ErrorKind::CrossesDevices, e)
            }
            Error(Kind::Io, Reason::Deadlock("io operations are deadlocked"), _) => {
                std::io::Error::new(std::io::ErrorKind::Deadlock, e)
            }
            Error(Kind::Io, Reason::Directory("directory is not empty"), _) => {
                std::io::Error::new(std::io::ErrorKind::DirectoryNotEmpty, e)
            }
            Error(Kind::Io, Reason::Executable("file busy"), _) => {
                std::io::Error::new(std::io::ErrorKind::ExecutableFileBusy, e)
            }
            Error(Kind::Io, Reason::File("too large"), _) => {
                std::io::Error::new(std::io::ErrorKind::FileTooLarge, e)
            }
            Error(Kind::Io, Reason::Host("unreachable"), _) => {
                std::io::Error::new(std::io::ErrorKind::HostUnreachable, e)
            }
            Error(Kind::Io, Reason::Interrupted("io operation was interrupted"), _) => {
                std::io::Error::new(std::io::ErrorKind::Interrupted, e)
            }
            Error(Kind::Io, Reason::Invalid("invalid io data"), _) => {
                std::io::Error::new(std::io::ErrorKind::InvalidData, e)
            }
            Error(Kind::Io, Reason::Invalid("invalid filename"), _) => {
                std::io::Error::new(std::io::ErrorKind::InvalidFilename, e)
            }
            Error(Kind::Io, Reason::Invalid("invalid input"), _) => {
                std::io::Error::new(std::io::ErrorKind::InvalidInput, e)
            }
            Error(Kind::Io, Reason::Is("is a directory"), _) => {
                std::io::Error::new(std::io::ErrorKind::IsADirectory, e)
            }
            Error(Kind::Io, Reason::Network("network is down"), _) => {
                std::io::Error::new(std::io::ErrorKind::NetworkDown, e)
            }
            Error(Kind::Io, Reason::Network("unreachable"), _) => {
                std::io::Error::new(std::io::ErrorKind::NetworkUnreachable, e)
            }
            Error(Kind::Io, Reason::Not("not a directory"), _) => {
                std::io::Error::new(std::io::ErrorKind::NotADirectory, e)
            }
            Error(Kind::Io, Reason::Not("not connected"), _) => {
                std::io::Error::new(std::io::ErrorKind::NotConnected, e)
            }
            Error(Kind::Io, Reason::Not("unspecified io entity not found"), _) => {
                std::io::Error::new(std::io::ErrorKind::NotFound, e)
            }
            Error(Kind::Io, Reason::Not("not seekable"), _) => {
                std::io::Error::new(std::io::ErrorKind::NotSeekable, e)
            }
            Error(Kind::Io, Reason::OutOf("out of memory"), _) => {
                std::io::Error::new(std::io::ErrorKind::OutOfMemory, e)
            }
            Error(Kind::Io, Reason::Permission("permission denied"), _) => {
                std::io::Error::new(std::io::ErrorKind::PermissionDenied, e)
            }
            Error(Kind::Io, Reason::Quota("quota exceeded"), _) => {
                std::io::Error::new(std::io::ErrorKind::QuotaExceeded, e)
            }
            Error(Kind::Io, Reason::Filesystem("filesystem is read-only"), _) => {
                std::io::Error::new(std::io::ErrorKind::ReadOnlyFilesystem, e)
            }
            Error(Kind::Io, Reason::Resource("resource is busy"), _) => {
                std::io::Error::new(std::io::ErrorKind::ResourceBusy, e)
            }
            Error(Kind::Io, Reason::Stale("network file handle stale"), _) => {
                std::io::Error::new(std::io::ErrorKind::StaleNetworkFileHandle, e)
            }
            Error(Kind::Io, Reason::Storage("storage full"), _) => {
                std::io::Error::new(std::io::ErrorKind::StorageFull, e)
            }
            Error(Kind::Io, Reason::Timeout("timed out"), _) => {
                std::io::Error::new(std::io::ErrorKind::TimedOut, e)
            }
            Error(Kind::Io, Reason::Exceeded("max number of links exceeded"), _) => {
                std::io::Error::new(std::io::ErrorKind::TooManyLinks, e)
            }
            Error(Kind::Io, Reason::Unexpected("unexpected end of file"), _) => {
                std::io::Error::new(std::io::ErrorKind::UnexpectedEof, e)
            }
            Error(Kind::Io, Reason::Unsupported("unsupported io operation"), _) => {
                std::io::Error::new(std::io::ErrorKind::Unsupported, e)
            }
            Error(Kind::Io, Reason::Conflict("io operation would block"), _) => {
                std::io::Error::new(std::io::ErrorKind::WouldBlock, e)
            }
            Error(Kind::Io, Reason::Write("would write zero bytes"), _) => {
                std::io::Error::new(std::io::ErrorKind::WriteZero, e)
            }
            Error(Kind::Io, Reason::Unknown("unknown io error"), _) => {
                std::io::Error::new(std::io::ErrorKind::Other, e)
            }
            _ => std::io::Error::new(std::io::ErrorKind::Other, e),
        }
    }
}
impl From<std::num::TryFromIntError> for Error {
    fn from(error: std::num::TryFromIntError) -> Self {
        Error::new(
            Kind::Int,
            Reason::Failed("try from int error"),
            Some(error.to_string()),
        )
    }
}
impl From<Box<dyn std::error::Error + Send + Sync>> for Error {
    fn from(e: Box<dyn std::error::Error + Send + Sync>) -> Self {
        Error::new(Kind::Other, Reason::Unknown("error"), Some(e.to_string()))
    }
}
impl Kind {
    pub fn as_str(&self) -> &'static str {
        match self {
            Kind::IoRead => "io read",
            Kind::IoWrite => "io write",
            Kind::IoAccess => "io access",
            Kind::IoNotFound => "io not found",
            Kind::IoPermission => "io permission",
            Kind::NetworkConnection => "network connection",
            Kind::NetworkTimeout => "network timeout",
            Kind::NetworkHost => "network host",
            Kind::ParseInt => "parse int",
            Kind::ParseFloat => "parse float",
            Kind::ParseJson => "parse json",
            Kind::FormatInvalid => "format invalid",
            Kind::LogicAssertion => "logic assertion",
            Kind::LogicState => "logic state",
            Kind::Io => "io",
            Kind::Int => "int",
            Kind::Float => "float",
            Kind::Format => "format",
            Kind::Network => "network",
            Kind::Json => "json",
            Kind::Logic => "logic",
            Kind::Other => "other",
            Kind::Memory => "memory",
            Kind::MemoryLeak => "memory leak",
            Kind::MemoryAccess => "memory access",
            Kind::Thread => "thread",
            Kind::ThreadLock => "thread lock",
            Kind::ThreadMutex => "thread mutex",
            Kind::File => "file",
            Kind::FileOpen => "file open",
            Kind::FileClose => "file close",
            Kind::System => "system",
            Kind::SystemCall => "system call",
            Kind::SystemResource => "system resource",
            Kind::Config => "config",
            Kind::ConfigMissing => "config missing",
            Kind::ConfigInvalid => "config invalid",
            Kind::Auth => "auth",
            Kind::AuthPermission => "auth permission",
            Kind::AuthAccess => "auth access",
            Kind::Database => "database",
            Kind::DatabaseConnection => "database connection",
            Kind::DatabaseQuery => "database query",
            Kind::Cache => "cache",
            Kind::CacheExpired => "cache expired",
            Kind::CacheMiss => "cache miss",
            Kind::Abort => "abort",
            Kind::Access => "access",
            Kind::Activate => "activate",
            Kind::Add => "add",
            Kind::Address => "address",
            Kind::Allow => "allow",
            Kind::Archive => "archive",
            Kind::Array => "array",
            Kind::Argument => "argument",
            Kind::Bind => "bind",
            Kind::Block => "block",
            Kind::Bound => "bound",
            Kind::Broken => "broken",
            Kind::Busy => "busy",
            Kind::Change => "change",
            Kind::Character => "character",
            Kind::Clock => "clock",
            Kind::Close => "close",
            Kind::Conflict => "conflict",
            Kind::Connection => "connection",
            Kind::Count => "count",
            Kind::Cross => "cross",
            Kind::Dead => "dead",
            Kind::Deadlock => "deadlock",
            Kind::Delete => "delete",
            Kind::Deny => "deny",
            Kind::Device => "device",
            Kind::Directory => "directory",
            Kind::Disable => "disable",
            Kind::Disconnect => "disconnect",
            Kind::Disk => "disk",
            Kind::Drain => "drain",
            Kind::Drop => "drop",
            Kind::Empty => "empty",
            Kind::Enable => "enable",
            Kind::End => "end",
            Kind::Error => "error",
            Kind::Event => "event",
            Kind::Executable => "executable",
            Kind::Exists => "exists",
            Kind::Exit => "exit",
            Kind::Expire => "expire",
            Kind::Fail => "fail",
            Kind::Filesystem => "filesystem",
            Kind::Filter => "filter",
            Kind::Find => "find",
            Kind::Finish => "finish",
            Kind::Flag => "flag",
            Kind::Flush => "flush",
            Kind::Full => "full",
            Kind::Function => "function",
            Kind::Generate => "generate",
            Kind::Group => "group",
            Kind::Handle => "handle",
            Kind::Hash => "hash",
            Kind::Header => "header",
            Kind::Host => "host",
            Kind::Index => "index",
            Kind::Initialize => "initialize",
            Kind::Input => "input",
            Kind::Insert => "insert",
            Kind::Install => "install",
            Kind::Interrupt => "interrupt",
            Kind::Invalid => "invalid",
            Kind::Item => "item",
            Kind::Key => "key",
            Kind::Kill => "kill",
            Kind::Last => "last",
            Kind::Leak => "leak",
            Kind::Left => "left",
            Kind::Length => "length",
            Kind::Limit => "limit",
            Kind::Link => "link",
            Kind::Lock => "lock",
            Kind::Log => "log",
            Kind::Loop => "loop",
            Kind::Mail => "mail",
            Kind::Message => "message",
            Kind::Missing => "missing",
            Kind::Module => "module",
            Kind::Multiple => "multiple",
            Kind::Multiply => "multiply",
            Kind::Mutex => "mutex",
            Kind::Name => "name",
            Kind::Negate => "negate",
            Kind::New => "new",
            Kind::Next => "next",
            Kind::Node => "node",
            Kind::Null => "null",
            Kind::Number => "number",
            Kind::Object => "object",
            Kind::Open => "open",
            Kind::Operation => "operation",
            Kind::Option => "option",
            Kind::Order => "order",
            Kind::OutOf => "out of",
            Kind::Overflow => "overflow",
            Kind::Permission => "permission",
            Kind::Process => "process",
            Kind::Protocol => "protocol",
            Kind::Queue => "queue",
            Kind::Quota => "quota",
            Kind::Read => "read",
            Kind::Receive => "receive",
            Kind::Record => "record",
            Kind::Reference => "reference",
            Kind::Register => "register",
            Kind::Remove => "remove",
            Kind::Request => "request",
            Kind::Resource => "resource",
            Kind::Response => "response",
            Kind::Right => "right",
            Kind::Route => "route",
            Kind::Search => "search",
            Kind::Security => "security",
            Kind::Send => "send",
            Kind::Service => "service",
            Kind::Session => "session",
            Kind::Set => "set",
            Kind::Share => "share",
            Kind::Signal => "signal",
            Kind::Size => "size",
            Kind::Socket => "socket",
            Kind::Stale => "stale",
            Kind::Start => "start",
            Kind::State => "state",
            Kind::Storage => "storage",
            Kind::Stream => "stream",
            Kind::String => "string",
            Kind::Sync => "sync",
            Kind::Table => "table",
            Kind::Task => "task",
            Kind::Terminate => "terminate",
            Kind::Test => "test",
            Kind::Time => "time",
            Kind::Timeout => "timeout",
            Kind::Token => "token",
            Kind::Transaction => "transaction",
            Kind::Transfer => "transfer",
            Kind::Transform => "transform",
            Kind::Type => "type",
            Kind::Unavailable => "unavailable",
            Kind::Undefined => "undefined",
            Kind::Unexpected => "unexpected",
            Kind::Unicode => "unicode",
            Kind::Uninitialized => "uninitialized",
            Kind::Unknown => "unknown",
            Kind::Unlock => "unlock",
            Kind::Unregister => "unregister",
            Kind::Update => "update",
            Kind::Upload => "upload",
            Kind::User => "user",
            Kind::Validation => "validation",
            Kind::Value => "value",
            Kind::Version => "version",
            Kind::Wait => "wait",
            Kind::Warning => "warning",
            Kind::Write => "write",
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
            Reason::Abort(s) => format!("Abort {}", s),
            Reason::Aborted(s) => format!("Aborted {}", s),
            Reason::Access(s) => format!("Access {}", s),
            Reason::Activate(s) => format!("Activate {}", s),
            Reason::Add(s) => format!("Add {}", s),
            Reason::Address(s) => format!("Address {}", s),
            Reason::Already(s) => format!("Already {}", s),
            Reason::Allow(s) => format!("Allow {}", s),
            Reason::Archive(s) => format!("Archive {}", s),
            Reason::Argument(s) => format!("Argument {}", s),
            Reason::Bad(s) => format!("Bad {}", s),
            Reason::Bind(s) => format!("Bind {}", s),
            Reason::Block(s) => format!("Block {}", s),
            Reason::Bound(s) => format!("Bound {}", s),
            Reason::Broken(s) => format!("Broken {}", s),
            Reason::Busy(s) => format!("Busy {}", s),
            Reason::Cache(s) => format!("Cache {}", s),
            Reason::Cannot(s) => format!("Cannot {}", s),
            Reason::Change(s) => format!("Change {}", s),
            Reason::Character(s) => format!("Character {}", s),
            Reason::Close(s) => format!("Close {}", s),
            Reason::Conflict(s) => format!("Conflict {}", s),
            Reason::Connection(s) => format!("Connection {}", s),
            Reason::Count(s) => format!("Count {}", s),
            Reason::Crosses(s) => format!("Crosses {}", s),
            Reason::Dead(s) => format!("Dead {}", s),
            Reason::Deadlock(s) => format!("Deadlock {}", s),
            Reason::Deny(s) => format!("Deny {}", s),
            Reason::Directory(s) => format!("Directory {}", s),
            Reason::Disconnect(s) => format!("Disconnect {}", s),
            Reason::Divide(s) => format!("Divide {}", s),
            Reason::Duplicate(s) => format!("Duplicate {}", s),
            Reason::Empty(s) => format!("Empty {}", s),
            Reason::End(s) => format!("End {}", s),
            Reason::Eof(s) => format!("Eof {}", s),
            Reason::Executable(s) => format!("Executable {}", s),
            Reason::Exceeded(s) => format!("Exceeded {}", s),
            Reason::Expected(s) => format!("Expected {}", s),
            Reason::Exists(s) => format!("Exists {}", s),
            Reason::Exit(s) => format!("Exit {}", s),
            Reason::External(s) => format!("External {}", s),
            Reason::Failed(s) => format!("Failed {}", s),
            Reason::File(s) => format!("File {}", s),
            Reason::Filesystem(s) => format!("Filesystem {}", s),
            Reason::Hash(s) => format!("Hash {}", s),
            Reason::Host(s) => format!("Host {}", s),
            Reason::Illegal(s) => format!("Illegal {}", s),
            Reason::Incomplete(s) => format!("Incomplete {}", s),
            Reason::Index(s) => format!("Index {}", s),
            Reason::Infinite(s) => format!("Infinite {}", s),
            Reason::Input(s) => format!("Input {}", s),
            Reason::Install(s) => format!("Install {}", s),
            Reason::Internal(s) => format!("Internal {}", s),
            Reason::Interrupted(s) => format!("Interrupted {}", s),
            Reason::Invalid(s) => format!("Invalid {}", s),
            Reason::Is(s) => format!("Is {}", s),
            Reason::Kill(s) => format!("Kill {}", s),
            Reason::Leak(s) => format!("Leak {}", s),
            Reason::Length(s) => format!("Length {}", s),
            Reason::Limit(s) => format!("Limit {}", s),
            Reason::Link(s) => format!("Link {}", s),
            Reason::Lock(s) => format!("Lock {}", s),
            Reason::Memory(s) => format!("Memory {}", s),
            Reason::Missing(s) => format!("Missing {}", s),
            Reason::Multiple(s) => format!("Multiple {}", s),
            Reason::Mutex(s) => format!("Mutex {}", s),
            Reason::Name(s) => format!("Name {}", s),
            Reason::Network(s) => format!("Network {}", s),
            Reason::Not(s) => format!("Not {}", s),
            Reason::Null(s) => format!("Null {}", s),
            Reason::Open(s) => format!("Open {}", s),
            Reason::Operation(s) => format!("Operation {}", s),
            Reason::OutOf(s) => format!("OutOf {}", s),
            Reason::Overflow(s) => format!("Overflow {}", s),
            Reason::Parse(s) => format!("Parse {}", s),
            Reason::Path(s) => format!("Path {}", s),
            Reason::Permission(s) => format!("Permission {}", s),
            Reason::Pipe(s) => format!("Pipe {}", s),
            Reason::Pointer(s) => format!("Pointer {}", s),
            Reason::Port(s) => format!("Port {}", s),
            Reason::Process(s) => format!("Process {}", s),
            Reason::Protocol(s) => format!("Protocol {}", s),
            Reason::Queue(s) => format!("Queue {}", s),
            Reason::Quota(s) => format!("Quota {}", s),
            Reason::Range(s) => format!("Range {}", s),
            Reason::Read(s) => format!("Read {}", s),
            Reason::Reference(s) => format!("Reference {}", s),
            Reason::Resource(s) => format!("Resource {}", s),
            Reason::Security(s) => format!("Security {}", s),
            Reason::Size(s) => format!("Size {}", s),
            Reason::Socket(s) => format!("Socket {}", s),
            Reason::Stale(s) => format!("Stale {}", s),
            Reason::Storage(s) => format!("Storage {}", s),
            Reason::Stream(s) => format!("Stream {}", s),
            Reason::System(s) => format!("System {}", s),
            Reason::Thread(s) => format!("Thread {}", s),
            Reason::Timeout(s) => format!("Timeout {}", s),
            Reason::Type(s) => format!("Type {}", s),
            Reason::Undefined(s) => format!("Undefined {}", s),
            Reason::Unexpected(s) => format!("Unexpected {}", s),
            Reason::Unicode(s) => format!("Unicode {}", s),
            Reason::Unknown(s) => format!("Unknown {}", s),
            Reason::Unlock(s) => format!("Unlock {}", s),
            Reason::Unreachable(s) => format!("Unreachable {}", s),
            Reason::Unsupported(s) => format!("Unsupported {}", s),
            Reason::User(s) => format!("User {}", s),
            Reason::Value(s) => format!("Value {}", s),
            Reason::Write(s) => format!("Write {}", s),
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
            std::io::ErrorKind::AlreadyExists => Reason::Exists("already"),
            std::io::ErrorKind::ArgumentListTooLong => Reason::Argument("list too long"),
            std::io::ErrorKind::BrokenPipe => Reason::Broken("pipe"),
            std::io::ErrorKind::ConnectionAborted => Reason::Connection("aborted"),
            std::io::ErrorKind::ConnectionRefused => Reason::Connection("refused"),
            std::io::ErrorKind::ConnectionReset => Reason::Connection("reset"),
            std::io::ErrorKind::CrossesDevices => Reason::Crosses("devices"),
            std::io::ErrorKind::Deadlock => Reason::Deadlock("detected"),
            std::io::ErrorKind::DirectoryNotEmpty => Reason::Directory("not empty"),
            std::io::ErrorKind::ExecutableFileBusy => Reason::Executable("file busy"),
            std::io::ErrorKind::FileTooLarge => Reason::File("too large"),
            std::io::ErrorKind::HostUnreachable => Reason::Host("unreachable"),
            std::io::ErrorKind::Interrupted => Reason::Interrupted("operation"),
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
            std::io::ErrorKind::TimedOut => Reason::Timeout("occurred"),
            std::io::ErrorKind::TooManyLinks => Reason::Exceeded("max links"),
            std::io::ErrorKind::UnexpectedEof => Reason::End("unexpected eof"),
            std::io::ErrorKind::Unsupported => Reason::Unsupported("operation"),
            std::io::ErrorKind::WouldBlock => Reason::Conflict("would block"),
            std::io::ErrorKind::WriteZero => Reason::Write("zero bytes"),
            _ => Reason::Unknown("io error kind"),
        }
    }
}
impl From<Reason> for std::io::ErrorKind {
    fn from(reason: Reason) -> Self {
        match reason {
            Reason::Address(msg) if msg == "in use" => std::io::ErrorKind::AddrInUse,
            Reason::Address(msg) if msg == "not available" => std::io::ErrorKind::AddrNotAvailable,
            Reason::Exists(msg) if msg == "already" => std::io::ErrorKind::AlreadyExists,
            Reason::Argument(msg) if msg == "list too long" => {
                std::io::ErrorKind::ArgumentListTooLong
            }
            Reason::Broken(msg) if msg == "pipe" => std::io::ErrorKind::BrokenPipe,
            Reason::Connection(msg) if msg == "aborted" => std::io::ErrorKind::ConnectionAborted,
            Reason::Connection(msg) if msg == "refused" => std::io::ErrorKind::ConnectionRefused,
            Reason::Connection(msg) if msg == "reset" => std::io::ErrorKind::ConnectionReset,
            Reason::Crosses(msg) if msg == "devices" => std::io::ErrorKind::CrossesDevices,
            Reason::Deadlock(msg) if msg == "detected" => std::io::ErrorKind::Deadlock,
            Reason::Directory(msg) if msg == "not empty" => std::io::ErrorKind::DirectoryNotEmpty,
            Reason::Executable(msg) if msg == "file busy" => std::io::ErrorKind::ExecutableFileBusy,
            Reason::File(msg) if msg == "too large" => std::io::ErrorKind::FileTooLarge,
            Reason::Host(msg) if msg == "unreachable" => std::io::ErrorKind::HostUnreachable,
            Reason::Interrupted(msg) if msg == "operation" => std::io::ErrorKind::Interrupted,
            Reason::Invalid(msg) if msg == "data" => std::io::ErrorKind::InvalidData,
            Reason::Invalid(msg) if msg == "filename" => std::io::ErrorKind::InvalidFilename,
            Reason::Invalid(msg) if msg == "input" => std::io::ErrorKind::InvalidInput,
            Reason::Is(msg) if msg == "a directory" => std::io::ErrorKind::IsADirectory,
            Reason::Network(msg) if msg == "down" => std::io::ErrorKind::NetworkDown,
            Reason::Network(msg) if msg == "unreachable" => std::io::ErrorKind::NetworkUnreachable,
            Reason::Not(msg) if msg == "a directory" => std::io::ErrorKind::NotADirectory,
            Reason::Not(msg) if msg == "connected" => std::io::ErrorKind::NotConnected,
            Reason::Not(msg) if msg == "found" => std::io::ErrorKind::NotFound,
            Reason::Not(msg) if msg == "seekable" => std::io::ErrorKind::NotSeekable,
            Reason::OutOf(msg) if msg == "memory" => std::io::ErrorKind::OutOfMemory,
            Reason::Permission(msg) if msg == "denied" => std::io::ErrorKind::PermissionDenied,
            Reason::Exceeded(msg) if msg == "quota" => std::io::ErrorKind::QuotaExceeded,
            Reason::Filesystem(msg) if msg == "read-only" => std::io::ErrorKind::ReadOnlyFilesystem,
            Reason::Resource(msg) if msg == "busy" => std::io::ErrorKind::ResourceBusy,
            Reason::Stale(msg) if msg == "network file handle" => {
                std::io::ErrorKind::StaleNetworkFileHandle
            }
            Reason::Storage(msg) if msg == "full" => std::io::ErrorKind::StorageFull,
            Reason::Timeout(msg) if msg == "occurred" => std::io::ErrorKind::TimedOut,
            Reason::Exceeded(msg) if msg == "max links" => std::io::ErrorKind::TooManyLinks,
            Reason::End(msg) if msg == "unexpected eof" => std::io::ErrorKind::UnexpectedEof,
            Reason::Unsupported(msg) if msg == "operation" => std::io::ErrorKind::Unsupported,
            Reason::Conflict(msg) if msg == "would block" => std::io::ErrorKind::WouldBlock,
            Reason::Write(msg) if msg == "zero bytes" => std::io::ErrorKind::WriteZero,
            _ => std::io::ErrorKind::Other,
        }
    }
}
impl From<std::num::IntErrorKind> for Reason {
    fn from(kind: std::num::IntErrorKind) -> Self {
        match kind {
            std::num::IntErrorKind::Empty => Reason::Empty("value"),
            std::num::IntErrorKind::PosOverflow => Reason::Overflow("positive"),
            std::num::IntErrorKind::NegOverflow => Reason::Overflow("negative"),
            std::num::IntErrorKind::InvalidDigit => Reason::Invalid("digit"),
            std::num::IntErrorKind::Zero => Reason::Divide("by zero"),
            _ => Reason::Unknown("integer parsing"),
        }
    }
}

impl From<Reason> for std::num::IntErrorKind {
    fn from(reason: Reason) -> Self {
        match reason {
            Reason::Empty(msg) if msg == "value" => std::num::IntErrorKind::Empty,
            Reason::Overflow(msg) if msg == "positive" => std::num::IntErrorKind::PosOverflow,
            Reason::Overflow(msg) if msg == "negative" => std::num::IntErrorKind::NegOverflow,
            Reason::Invalid(msg) if msg == "digit" => std::num::IntErrorKind::InvalidDigit,
            Reason::Divide(msg) if msg == "by zero" => std::num::IntErrorKind::Zero,
            _ => std::num::IntErrorKind::InvalidDigit,
        }
    }
}
pub fn err<T>(kind: Kind, reason: Reason, message: &str) -> Result<T> {
    if message.is_empty() {
        return Err(Error::new(kind, reason, Some("unknown error".to_string())));
    }
    Err(Error::new(kind, reason, Some(message.to_string())))
}

#[macro_export]
macro_rules! err {
    ($kind:tt, $reason:tt) => {
        Err(Error::new(
            $crate::error::Kind::$kind,
            $crate::error::Reason::$reason,
            None,
        ))
    };
    ($kind:tt, $reason:tt, $message:expr) => {
        Err(Error::new(
            $crate::error::Kind::$kind,
            $crate::error::Reason::$reason,
            Some($message.into()),
        ))
    };
}

// ============================================================================
// Database Integration
// ============================================================================

use crate::db::web::{Build, Entry};
use std::collections::HashMap as StdHashMap;

impl Build for Error {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("Error", self.0.as_str());
        entry.set_attr("kind", Entry::string(self.0.as_str()));
        entry.set_attr("reason", Entry::string(self.1.as_str()));
        if let Some(ref msg) = self.2 {
            entry.set_attr("message", Entry::string(msg));
        }
        entry.set_attr(
            "timestamp",
            Entry::string(format!("{:?}", std::time::Instant::now())),
        );
        entry
    }

    fn kind(&self) -> &str {
        "error"
    }

    fn name(&self) -> Option<&str> {
        Some(self.0.as_str())
    }

    fn category(&self) -> Option<&str> {
        // Group errors by their kind category
        Some(match self.0 {
            Kind::IoRead
            | Kind::IoWrite
            | Kind::IoAccess
            | Kind::IoNotFound
            | Kind::IoPermission
            | Kind::Io => "io",
            Kind::NetworkConnection | Kind::NetworkTimeout | Kind::NetworkHost | Kind::Network => {
                "network"
            }
            Kind::ParseInt | Kind::ParseFloat | Kind::ParseJson => "parse",
            Kind::Memory | Kind::MemoryLeak | Kind::MemoryAccess => "memory",
            Kind::Thread | Kind::ThreadLock | Kind::ThreadMutex => "thread",
            Kind::File | Kind::FileOpen | Kind::FileClose => "file",
            Kind::Database | Kind::DatabaseConnection | Kind::DatabaseQuery => "database",
            Kind::Config | Kind::ConfigMissing | Kind::ConfigInvalid => "config",
            Kind::Auth | Kind::AuthPermission | Kind::AuthAccess => "auth",
            Kind::Cache | Kind::CacheExpired | Kind::CacheMiss => "cache",
            _ => "general",
        })
    }

    fn priority(&self) -> i16 {
        // Higher priority for more severe error types
        match self.0 {
            Kind::Memory | Kind::MemoryLeak | Kind::MemoryAccess => 100,
            Kind::Thread | Kind::ThreadLock | Kind::ThreadMutex | Kind::Deadlock => 90,
            Kind::IoRead | Kind::IoWrite | Kind::IoNotFound => 80,
            Kind::NetworkConnection | Kind::NetworkTimeout => 70,
            Kind::Database | Kind::DatabaseConnection | Kind::DatabaseQuery => 60,
            Kind::ParseInt | Kind::ParseFloat | Kind::ParseJson => 50,
            Kind::Config | Kind::ConfigMissing | Kind::ConfigInvalid => 40,
            _ => 0,
        }
    }
}

/// Convert a C2RError to an Entry for storage in Web database
pub fn error_to_entry(error: &Error) -> Entry {
    error.to_entry()
}
