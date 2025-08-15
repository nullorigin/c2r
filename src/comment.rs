use crate::error::ConversionResult;

/// Represents a C comment and its Rust equivalent
#[derive(Debug, Clone)]
pub struct Comment {
    /// The original comment text
    pub content: String,
    /// Whether this is a block comment (/* */) or line comment (//)
    pub is_block: bool,
    /// The length of the original comment
    pub len: usize,
}

impl Comment {
    /// Creates a new Comment instance
    pub fn new() -> Self {
        Self {
            content: String::new(),
            is_block: false,
            len: 0,
        }
    }

    /// Creates a Comment from a C comment
    pub fn from(content: &str, is_block: bool) -> Self {
        let cleaned = if is_block {
            // Strip /* and */
            content.trim_start_matches("/*").trim_end_matches("*/").trim().to_string()
        } else {
            // Strip //
            content.trim_start_matches("//").trim().to_string()
        };

        Self {
            content: cleaned,
            is_block,
            len: content.len(),
        }
    }

    /// Parses a C comment
    pub fn parse(&mut self, s: &str) -> ConversionResult<()> {
        if s.starts_with("/*") && s.ends_with("*/") {
            self.is_block = true;
            self.content = s.trim_start_matches("/*").trim_end_matches("*/").trim().to_string();
        } else if s.starts_with("//") {
            self.is_block = false;
            self.content = s.trim_start_matches("//").trim().to_string();
        }
        self.len = s.len();
        Ok(())
    }

    /// Converts to Rust comment
    pub fn output(&self) -> String {
        if self.is_block {
            format!("/* {} */", self.content)
        } else {
            format!("// {}", self.content)
        }
    }
    
    /// Returns the length of the original comment
    pub fn len(&self) -> usize {
        self.len
    }

    /// Check if the comment is empty
    pub fn is_empty(&self) -> bool {
        self.content.is_empty()
    }
        
    /// Find the end of a comment
    pub fn find_end(s: &str, start: usize) -> usize {
        if start >= s.len() {
            return start;
        }

        if &s[start..start+2] == "//" {
            // Single line comment - find the end of the line
            match s[start..].find('\n') {
                Some(pos) => start + pos + 1,
                None => s.len()
            }
        } else if &s[start..start+2] == "/*" {
            // Multi-line comment - find the closing */
            match s[start..].find("*/") {
                Some(pos) => start + pos + 2,
                None => s.len()
            }
        } else {
            // Not a comment
            start
        }
    }
}
