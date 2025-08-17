use std::fmt;
use std::error::Error;

/// Custom error types for the C to Rust conversion process
#[derive(Debug)]
pub enum ConversionError {
    /// Error parsing a C construct
    ParseError {
        /// The source code being parsed
        source: String,
        /// Line number where the error occurred
        line: usize,
        /// Position in the line
        position: usize,
        /// Description of what went wrong
        message: String,
    },
    
    /// Error converting a C type to Rust
    TypeConversionError {
        /// The C type that couldn't be converted
        c_type: String,
        /// Description of what went wrong
        message: String,
    },
    
    /// Error converting a C expression to Rust
    ExpressionConversionError {
        /// The C expression that couldn't be converted
        c_expr: String,
        /// Description of what went wrong
        message: String,
    },
    
    /// I/O error during file operations
    Io(std::io::Error),
    
    /// Unsupported C construct
    UnsupportedConstruct {
        /// The construct type (e.g., "inline assembly", "variable arguments")
        construct: String,
        /// Description of why it's not supported
        message: String,
    },
    
    /// General error
    General(String),
}

impl ConversionError {
    /// Create a new parse error
    pub fn parse_error(source: &str, line: usize, position: usize, message: &str) -> Self {
        ConversionError::ParseError {
            source: source.to_string(),
            line,
            position,
            message: message.to_string(),
        }
    }
    
    /// Create a new type conversion error
    pub fn type_error(c_type: &str, message: &str) -> Self {
        ConversionError::TypeConversionError {
            c_type: c_type.to_string(),
            message: message.to_string(),
        }
    }
    
    /// Create a new expression conversion error
    pub fn expr_error(c_expr: &str, message: &str) -> Self {
        ConversionError::ExpressionConversionError {
            c_expr: c_expr.to_string(),
            message: message.to_string(),
        }
    }
    
    /// Create a new I/O error
    pub fn io_error(err: std::io::Error) -> Self {
        ConversionError::Io(err)
    }
    
    /// Create a new I/O error from a message string
    pub fn io_error_msg(message: &str) -> Self {
        ConversionError::Io(std::io::Error::new(std::io::ErrorKind::Other, message))
    }
    
    /// Create a new unsupported construct error
    pub fn unsupported_construct(construct: &str, message: &str) -> Self {
        ConversionError::UnsupportedConstruct {
            construct: construct.to_string(),
            message: message.to_string(),
        }
    }
    
    /// Create a new generic error
    pub fn general(message: &str) -> Self {
        ConversionError::General(message.to_string())
    }
}

impl Clone for ConversionError {
    fn clone(&self) -> Self {
        match self {
            ConversionError::ParseError { source, line, position, message } => {
                ConversionError::ParseError { source: source.clone(), line: *line, position: *position, message: message.clone() }
            }

            ConversionError::TypeConversionError { c_type, message } => {
                ConversionError::TypeConversionError { c_type: c_type.clone(), message: message.clone() }
            }

            ConversionError::ExpressionConversionError { c_expr, message } => {
                ConversionError::ExpressionConversionError { c_expr: c_expr.clone(), message: message.clone() }
            }

            ConversionError::Io(e) => {
                ConversionError::Io(std::io::Error::new(e.kind(), format!("{}", e)))
            }

            ConversionError::UnsupportedConstruct { construct, message } => {
                ConversionError::UnsupportedConstruct { construct: construct.clone(), message: message.clone() }
            }

            ConversionError::General(msg) => {
                ConversionError::General(msg.clone())
            }
        }
    }
}

// Implement Display for ConversionError
impl fmt::Display for ConversionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ConversionError::ParseError { source, line, position, message } => {
                write!(f, "Parse error at line {}, position {}: {}\nSource: {}", 
                    line, position, message, source)
            }
            
            ConversionError::TypeConversionError { c_type, message } => {
                write!(f, "Type conversion error for '{}': {}", c_type, message)
            }
            
            ConversionError::ExpressionConversionError { c_expr, message } => {
                write!(f, "Expression conversion error for '{}': {}", c_expr, message)
            }
            
            ConversionError::Io(err) => {
                write!(f, "I/O error: {}", err)
            }
            
            ConversionError::UnsupportedConstruct { construct, message } => {
                write!(f, "Unsupported C construct '{}': {}", construct, message)
            }
            
            ConversionError::General(msg) => {
                write!(f, "Error: {}", msg)
            }
        }
    }
}

// Implement Error trait for ConversionError
impl Error for ConversionError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            ConversionError::Io(err) => Some(err),
            _ => None,
        }
    }
}

// Implement From for std::io::Error
impl From<std::io::Error> for ConversionError {
    fn from(e: std::io::Error) -> Self {
        ConversionError::Io(e)
    }
}

/// Result type for conversion operations
pub type ConversionResult<T> = Result<T, ConversionError>;
