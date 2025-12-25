//! String Format Handler
//!
//! Converts C printf/sprintf format strings to Rust format strings.
//! Handles format specifiers like %d, %s, %f, etc.

use crate::db::web::{Entry, Build};
use crate::db::token::Token;
use crate::db::pattern::{Pattern, PatternRule};
use crate::handlers::process::{ProcessStage, Processor};

// ============================================================================
// Format Specifier Conversion
// ============================================================================

/// C format specifier information
#[derive(Debug, Clone)]
pub struct FormatSpecifier {
    /// Original specifier (e.g., "%d", "%-5.2f")
    pub original: String,
    /// Width if specified
    pub width: Option<usize>,
    /// Precision if specified
    pub precision: Option<usize>,
    /// Left-justify flag
    pub left_justify: bool,
    /// Zero-pad flag
    pub zero_pad: bool,
    /// The conversion character (d, s, f, etc.)
    pub conversion: char,
}

impl FormatSpecifier {
    /// Convert C format specifier to Rust format specifier
    pub fn to_rust(&self) -> String {
        let mut result = String::from("{");
        
        // Add formatting options
        if self.left_justify || self.zero_pad || self.width.is_some() || self.precision.is_some() {
            result.push(':');
            
            if self.left_justify {
                result.push('<');
            }
            
            if self.zero_pad {
                result.push('0');
            }
            
            if let Some(w) = self.width {
                result.push_str(&w.to_string());
            }
            
            if let Some(p) = self.precision {
                result.push('.');
                result.push_str(&p.to_string());
            }
            
            // Add type hint for floats
            match self.conversion {
                'e' | 'E' => result.push('e'),
                'x' => result.push('x'),
                'X' => result.push('X'),
                'o' => result.push('o'),
                'b' => result.push('b'),
                _ => {}
            }
        }
        
        result.push('}');
        result
    }
}

/// Parse a C format specifier from a string starting at position
fn parse_format_specifier(s: &str, start: usize) -> Option<(FormatSpecifier, usize)> {
    let chars: Vec<char> = s[start..].chars().collect();
    if chars.is_empty() || chars[0] != '%' {
        return None;
    }
    
    // Check for %% escape
    if chars.len() > 1 && chars[1] == '%' {
        return Some((FormatSpecifier {
            original: "%%".to_string(),
            width: None,
            precision: None,
            left_justify: false,
            zero_pad: false,
            conversion: '%',
        }, 2));
    }
    
    let mut idx = 1;
    let mut left_justify = false;
    let mut zero_pad = false;
    let mut width: Option<usize> = None;
    let mut precision: Option<usize> = None;
    
    // Parse flags
    while idx < chars.len() {
        match chars[idx] {
            '-' => { left_justify = true; idx += 1; }
            '+' => { idx += 1; } // Ignore + flag
            ' ' => { idx += 1; } // Ignore space flag
            '#' => { idx += 1; } // Ignore alternate form
            '0' => { zero_pad = true; idx += 1; }
            _ => break,
        }
    }
    
    // Parse width
    let width_start = idx;
    while idx < chars.len() && chars[idx].is_ascii_digit() {
        idx += 1;
    }
    if idx > width_start {
        width = s[start + width_start..start + idx].parse().ok();
    }
    
    // Parse precision
    if idx < chars.len() && chars[idx] == '.' {
        idx += 1;
        let prec_start = idx;
        while idx < chars.len() && chars[idx].is_ascii_digit() {
            idx += 1;
        }
        if idx > prec_start {
            precision = s[start + prec_start..start + idx].parse().ok();
        } else {
            precision = Some(0);
        }
    }
    
    // Skip length modifiers (h, l, ll, L, z, j, t)
    while idx < chars.len() {
        match chars[idx] {
            'h' | 'l' | 'L' | 'z' | 'j' | 't' => idx += 1,
            _ => break,
        }
    }
    
    // Parse conversion specifier
    if idx >= chars.len() {
        return None;
    }
    
    let conversion = chars[idx];
    if !matches!(conversion, 'd' | 'i' | 'u' | 'o' | 'x' | 'X' | 'f' | 'F' | 
                            'e' | 'E' | 'g' | 'G' | 'c' | 's' | 'p' | 'n') {
        return None;
    }
    
    idx += 1;
    
    Some((FormatSpecifier {
        original: s[start..start + idx].to_string(),
        width,
        precision,
        left_justify,
        zero_pad,
        conversion,
    }, idx))
}

// ============================================================================
// String Format Handler
// ============================================================================

/// Extracted format string information
#[derive(Debug, Clone, Default)]
pub struct StringFormatData {
    /// The function name (printf, sprintf, fprintf, etc.)
    pub function_name: String,
    /// The format string (with quotes removed)
    pub format_string: String,
    /// The arguments after the format string
    pub arguments: Vec<String>,
    /// Parsed format specifiers
    pub specifiers: Vec<FormatSpecifier>,
}

/// Handler for C format string functions
#[derive(Debug)]
pub struct StringFormatHandler {
    stage: ProcessStage,
    confidence: f64,
    error: Option<String>,
    output: Option<String>,
    data: StringFormatData,
}

impl StringFormatHandler {
    pub fn new() -> Self {
        Self {
            stage: ProcessStage::Pending,
            confidence: 0.0,
            error: None,
            output: None,
            data: StringFormatData::default(),
        }
    }
    
    /// Convert C format string to Rust format string
    fn convert_format_string(&self, format: &str) -> String {
        let mut result = String::new();
        let mut chars = format.chars().peekable();
        let mut pos = 0;
        
        while pos < format.len() {
            if format[pos..].starts_with('%') {
                if let Some((spec, len)) = parse_format_specifier(format, pos) {
                    if spec.conversion == '%' {
                        result.push('%');
                    } else {
                        result.push_str(&spec.to_rust());
                    }
                    pos += len;
                    // Advance chars iterator
                    for _ in 0..len {
                        chars.next();
                    }
                    continue;
                }
            }
            
            if let Some(c) = chars.next() {
                // Convert C escape sequences
                if c == '\\' {
                    if let Some(&next) = chars.peek() {
                        match next {
                            'n' => { result.push('\n'); chars.next(); pos += 1; }
                            't' => { result.push('\t'); chars.next(); pos += 1; }
                            'r' => { result.push('\r'); chars.next(); pos += 1; }
                            '\\' => { result.push('\\'); chars.next(); pos += 1; }
                            '"' => { result.push('"'); chars.next(); pos += 1; }
                            _ => result.push(c),
                        }
                    } else {
                        result.push(c);
                    }
                } else {
                    result.push(c);
                }
            }
            pos += 1;
        }
        
        result
    }
    
    /// Get the Rust macro name for the C function
    fn get_rust_macro(&self) -> &str {
        match self.data.function_name.as_str() {
            "printf" => "print!",
            "puts" | "println" => "println!",
            "fprintf" => "write!",
            "sprintf" | "snprintf" => "format!",
            "eprintf" | "perror" => "eprint!",
            _ => "print!",
        }
    }
    
    /// Check if this is a println-style call (ends with newline)
    fn is_println_style(&self) -> bool {
        self.data.format_string.ends_with("\\n") || 
        self.data.format_string.ends_with('\n')
    }
}

impl Default for StringFormatHandler {
    fn default() -> Self {
        Self::new()
    }
}

impl Processor for StringFormatHandler {
    fn name(&self) -> &str { "StringFormatHandler" }
    
    fn supported_patterns(&self) -> &[&str] {
        &["printf_call", "format_call", "string_format"]
    }
    
    fn patterns(&self) -> Vec<Pattern> {
        vec![
            // printf("format", args...)
            Pattern::definition(700, "printf_call", vec![
                PatternRule::one_of(vec![
                    "printf".to_string(), 
                    "fprintf".to_string(),
                    "sprintf".to_string(),
                    "snprintf".to_string(),
                ]),
                PatternRule::exact("("),
                PatternRule::any(),
                PatternRule::exact(")"),
            ])
            .with_category("expression")
            .with_priority(180)
            .with_min_tokens(4)
            .with_description("Printf-style format call"),
        ]
    }
    
    fn validate(&mut self, tokens: &[Token]) -> bool {
        if tokens.len() < 4 {
            return false;
        }
        
        let first = tokens[0].to_string();
        if !matches!(first.as_str(), "printf" | "fprintf" | "sprintf" | "snprintf" | "puts") {
            return false;
        }
        
        // Must have ( and )
        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        let has_parens = token_strs.contains(&"(".to_string()) && 
                        token_strs.contains(&")".to_string());
        
        if !has_parens {
            return false;
        }
        
        self.confidence = 0.9;
        true
    }
    
    fn extract(&mut self, tokens: &[Token]) -> bool {
        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        
        // Get function name
        self.data.function_name = token_strs[0].clone();
        
        // Find parentheses
        let open_paren = token_strs.iter().position(|t| t == "(");
        let close_paren = token_strs.iter().rposition(|t| t == ")");
        
        if let (Some(op), Some(cp)) = (open_paren, close_paren) {
            if cp <= op + 1 {
                return false;
            }
            
            // Get arguments between parentheses
            let args = &token_strs[op + 1..cp];
            
            // First argument should be the format string
            if args.is_empty() {
                return false;
            }
            
            // Handle fprintf (first arg is file handle)
            let format_start = if self.data.function_name == "fprintf" || 
                                 self.data.function_name == "snprintf" {
                // Skip first argument (file handle or buffer)
                args.iter().position(|t| t == ",").map(|p| p + 1).unwrap_or(0)
            } else {
                0
            };
            
            if format_start >= args.len() {
                return false;
            }
            
            // Extract format string (remove quotes)
            let mut format = args[format_start].clone();
            if format.starts_with('"') && format.ends_with('"') {
                format = format[1..format.len()-1].to_string();
            } else if format.starts_with('"') {
                // Multi-token string - collect until closing quote
                format = format[1..].to_string();
                for i in (format_start + 1)..args.len() {
                    if args[i].ends_with('"') {
                        format.push_str(&args[i][..args[i].len()-1]);
                        break;
                    } else if args[i] != "," {
                        format.push_str(&args[i]);
                    }
                }
            }
            
            self.data.format_string = format;
            
            // Extract remaining arguments
            let mut in_format = true;
            let mut current_arg = String::new();
            let mut paren_depth = 0;
            
            for arg in args.iter().skip(format_start) {
                if in_format {
                    if arg.ends_with('"') || (arg.starts_with('"') && arg.ends_with('"')) {
                        in_format = false;
                    }
                    continue;
                }
                
                if arg == "," && paren_depth == 0 {
                    if !current_arg.is_empty() {
                        self.data.arguments.push(current_arg.trim().to_string());
                        current_arg = String::new();
                    }
                } else {
                    if arg == "(" { paren_depth += 1; }
                    if arg == ")" { paren_depth -= 1; }
                    if !current_arg.is_empty() {
                        current_arg.push(' ');
                    }
                    current_arg.push_str(arg);
                }
            }
            
            if !current_arg.is_empty() {
                self.data.arguments.push(current_arg.trim().to_string());
            }
        }
        
        self.confidence = 0.85;
        true
    }
    
    fn convert(&mut self) -> Option<String> {
        // Convert format string
        let rust_format = self.convert_format_string(&self.data.format_string);
        
        // Determine macro to use
        let is_println = self.is_println_style();
        let macro_name = if is_println && self.data.function_name == "printf" {
            "println!"
        } else {
            self.get_rust_macro()
        };
        
        // Remove trailing newline from format if using println
        let format_str = if is_println && rust_format.ends_with('\n') {
            rust_format[..rust_format.len()-1].to_string()
        } else {
            rust_format
        };
        
        // Build the call
        let args = if self.data.arguments.is_empty() {
            String::new()
        } else {
            format!(", {}", self.data.arguments.join(", "))
        };
        
        let result = format!("{}(\"{}\"{})", macro_name, format_str, args);
        
        self.confidence = 0.9;
        Some(result)
    }
    
    fn current_stage(&self) -> ProcessStage { self.stage }
    fn set_stage(&mut self, stage: ProcessStage) { self.stage = stage; }
    fn output(&self) -> Option<String> { self.output.clone() }
    fn set_output(&mut self, output: String) { self.output = Some(output); }
    fn error(&self) -> Option<String> { self.error.clone() }
    fn set_error(&mut self, error: String) { self.error = Some(error); }
    fn confidence(&self) -> f64 { self.confidence }
    fn set_confidence(&mut self, confidence: f64) { self.confidence = confidence; }
}

impl Build for StringFormatHandler {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("StringFormatHandler", "string_format_handler");
        entry.set_attr("stage", Entry::string(&format!("{:?}", self.stage)));
        entry.set_attr("confidence", Entry::f64(self.confidence));
        if let Some(ref output) = self.output {
            entry.set_attr("output", Entry::string(output));
        }
        entry
    }
    
    fn kind(&self) -> &str { "Handler" }
    fn name(&self) -> Option<&str> { Some("StringFormatHandler") }
    fn category(&self) -> Option<&str> { Some("expression") }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_format_specifier_parsing() {
        let (spec, len) = parse_format_specifier("%d", 0).unwrap();
        assert_eq!(spec.conversion, 'd');
        assert_eq!(len, 2);
        
        let (spec, len) = parse_format_specifier("%-5.2f", 0).unwrap();
        assert_eq!(spec.conversion, 'f');
        assert!(spec.left_justify);
        assert_eq!(spec.width, Some(5));
        assert_eq!(spec.precision, Some(2));
        assert_eq!(len, 6);
    }
    
    #[test]
    fn test_format_conversion() {
        let handler = StringFormatHandler::new();
        
        // Simple %d
        let result = handler.convert_format_string("Value: %d");
        assert_eq!(result, "Value: {}");
        
        // Multiple specifiers
        let result = handler.convert_format_string("%s = %d");
        assert_eq!(result, "{} = {}");
    }
}
