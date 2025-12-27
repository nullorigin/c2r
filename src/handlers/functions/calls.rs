//! Function Call Handler
//!
//! Converts C function calls to Rust function calls.

use crate::db::convert::IdentifierConverter;
use crate::db::pattern::{Pattern, PatternRule};
use crate::db::token::Token;
use crate::db::web::{Build, Entry};
use crate::handlers::process::{ProcessStage, Processor};
use crate::system;

// ============================================================================
// Function Call Handler Implementation
// ============================================================================

/// Extracted function call information
#[derive(Debug, Clone, Default)]
pub struct CallData {
    /// Function name being called
    pub name: String,
    /// Arguments as token strings
    pub arguments: Vec<Vec<String>>,
    /// Whether this is a method call (obj.method())
    pub is_method_call: bool,
    /// Object for method calls
    pub object: Option<String>,
}

/// Handler for C function calls
#[derive(Debug)]
pub struct CallHandler {
    stage: ProcessStage,
    confidence: f64,
    error: Option<String>,
    output: Option<String>,
    data: CallData,
}

impl CallHandler {
    pub fn new() -> Self {
        Self {
            stage: ProcessStage::Pending,
            confidence: 0.0,
            error: None,
            output: None,
            data: CallData::default(),
        }
    }

    /// Convert function call from tokens (called from DefinitionHandler via routing)
    pub fn convert_from_tokens(tokens: &[Token]) -> String {
        let mut handler = Self::new();
        if handler.validate(tokens) && handler.extract(tokens) {
            handler.convert().unwrap_or_else(|| {
                let strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
                format!("// call: {}", strs.join(" "))
            })
        } else {
            let strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
            format!("// call: {}", strs.join(" "))
        }
    }
}

impl Default for CallHandler {
    fn default() -> Self {
        Self::new()
    }
}

impl Processor for CallHandler {
    fn name(&self) -> &str {
        "CallHandler"
    }

    fn supported_patterns(&self) -> &[&str] {
        &["validate_function_call", "extract_function_call"]
    }

    fn patterns(&self) -> Vec<(Pattern, Pattern)> {
        vec![(
            Pattern::definition(
                500,
                "validate_function_call",
                vec![
                    PatternRule::identifier(),
                    PatternRule::exact("("),
                    PatternRule::any(),
                    PatternRule::exact(")"),
                ],
            )
            .with_category("call")
            .with_priority(60)
            .with_min_tokens(3)
            .with_description("Function call"),
            Pattern::definition(
                500,
                "extract_function_call",
                vec![
                    PatternRule::identifier().with_extract(|rule, ctx| {
                        let token = ctx.current_token.clone();
                        if ctx.did_match {
                            ctx.set_value("function_name", &token);
                        }
                        rule.clone()
                    }),
                    PatternRule::exact("(").with_extract(|rule, ctx| {
                        ctx.set_flag("in_args", true);
                        rule.clone()
                    }),
                    PatternRule::any().with_extract(|rule, ctx| {
                        let token = ctx.current_token.clone();
                        if ctx.did_match && ctx.flag("in_args") {
                            ctx.push_list("args", &token);
                        }
                        rule.clone()
                    }),
                    PatternRule::exact(")").with_extract(|rule, ctx| {
                        ctx.set_flag("in_args", false);
                        rule.clone()
                    }),
                ],
            )
            .with_category("call")
            .with_priority(60)
            .with_min_tokens(3)
            .with_description("Function call"),
        )]
    }

    fn validate(&mut self, tokens: &[Token]) -> bool {
        if tokens.len() < 3 {
            return false;
        }

        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        // Must have identifier followed by (
        let paren_pos = token_strs.iter().position(|t| t == "(");
        if paren_pos.is_none() || paren_pos == Some(0) {
            return false;
        }

        // Must have matching )
        let close_paren = token_strs.iter().rposition(|t| t == ")");
        if close_paren.is_none() {
            return false;
        }

        // Check it's not a function definition (no { after ))
        let close_pos = close_paren.unwrap();
        if close_pos + 1 < token_strs.len() {
            let next = &token_strs[close_pos + 1];
            if next == "{" {
                return false; // This is a function definition
            }
        }

        self.confidence = 0.7;
        true
    }

    fn extract(&mut self, tokens: &[Token]) -> bool {
        let token_strs: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        // Try extraction patterns first
        for (_, extract_pattern) in self.patterns() {
            if let Some(ctx) = extract_pattern.run_extraction(&token_strs) {
                // Get function name from pattern extraction
                if let Some(func_name) = ctx.value("function_name") {
                    self.data.name = func_name.to_string();
                }

                // Get arguments from pattern extraction
                if let Some(args) = ctx.list("args") {
                    self.parse_arguments(args);
                }

                if !self.data.name.is_empty() {
                    self.confidence = 0.8;
                    return true;
                }
            }
        }

        // Fallback to manual extraction
        let paren_pos = match token_strs.iter().position(|t| t == "(") {
            Some(p) => p,
            None => return false,
        };

        if paren_pos >= 2 {
            let sep = &token_strs[paren_pos - 2];
            if sep == "." || sep == "->" {
                self.data.is_method_call = true;
                self.data.object = Some(token_strs[..paren_pos - 2].join(" "));
                self.data.name = token_strs[paren_pos - 1].clone();
            } else {
                self.data.name = token_strs[..paren_pos].join("");
            }
        } else {
            self.data.name = token_strs[..paren_pos].join("");
        }

        let close_paren = match token_strs.iter().rposition(|t| t == ")") {
            Some(p) => p,
            None => return false,
        };

        if close_paren > paren_pos + 1 {
            let arg_tokens = &token_strs[paren_pos + 1..close_paren];
            self.parse_arguments(arg_tokens);
        }

        self.confidence = 0.8;
        true
    }

    fn convert(&mut self) -> Option<String> {
        // Clone arguments to avoid borrow issues
        let arguments = self.data.arguments.clone();

        // Convert arguments using IdentifierConverter
        let args: Vec<String> = arguments
            .iter()
            .map(|arg| self.convert_argument(arg))
            .collect();
        let args_str = args.join(", ");

        // Build the call
        let rust_code = if self.data.is_method_call {
            if let Some(ref obj) = self.data.object {
                // Convert -> to .
                format!("{}.{}({})", obj, self.data.name, args_str)
            } else {
                format!("{}({})", self.data.name, args_str)
            }
        } else {
            // Use IdentifierConverter for function name mapping
            let func_name = system().lookup_identifier(&self.data.name)
                .unwrap_or_else(|| self.data.name.clone());
            format!("{}({})", func_name, args_str)
        };

        self.confidence = 0.85;
        Some(rust_code)
    }

    fn current_stage(&self) -> ProcessStage {
        self.stage
    }
    fn set_stage(&mut self, stage: ProcessStage) {
        self.stage = stage;
    }
    fn output(&self) -> Option<String> {
        self.output.clone()
    }
    fn set_output(&mut self, output: String) {
        self.output = Some(output);
    }
    fn error(&self) -> Option<String> {
        self.error.clone()
    }
    fn set_error(&mut self, error: String) {
        self.error = Some(error);
    }
    fn confidence(&self) -> f64 {
        self.confidence
    }
    fn set_confidence(&mut self, confidence: f64) {
        self.confidence = confidence;
    }
}

impl CallHandler {
    fn parse_arguments(&mut self, tokens: &[String]) {
        let mut current: Vec<String> = Vec::new();
        let mut paren_depth = 0;

        for token in tokens {
            match token.as_str() {
                "(" => {
                    current.push(token.clone());
                    paren_depth += 1;
                }
                ")" => {
                    current.push(token.clone());
                    paren_depth -= 1;
                }
                "," if paren_depth == 0 => {
                    if !current.is_empty() {
                        self.data.arguments.push(std::mem::take(&mut current));
                    }
                }
                _ => {
                    current.push(token.clone());
                }
            }
        }

        if !current.is_empty() {
            self.data.arguments.push(current);
        }
    }

    fn convert_argument(&mut self, tokens: &[String]) -> String {
        // Convert each token using IdentifierConverter for known identifiers
        let converted: Vec<String> = tokens
            .iter()
            .map(|t| system().lookup_identifier(t).unwrap_or_else(|| t.clone()))
            .collect();

        converted.join(" ").replace("& ", "&").replace("* ", "*")
    }
}

impl Build for CallHandler {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("Handler", "CallHandler");
        entry.set_attr("stage", Entry::string(self.stage.as_str()));
        entry.set_attr("confidence", Entry::f64(self.confidence));
        if let Some(ref output) = self.output {
            entry.set_attr("output", Entry::string(output));
        }
        entry.set_attr("function_name", Entry::string(&self.data.name));
        entry
    }

    fn kind(&self) -> &str {
        "Handler"
    }
    fn name(&self) -> Option<&str> {
        Some("CallHandler")
    }
    fn category(&self) -> Option<&str> {
        Some("call")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_tokens(strs: &[&str]) -> Vec<Token> {
        strs.iter().map(|s| Token::from_string(*s)).collect()
    }

    #[test]
    fn test_simple_call() {
        let tokens = make_tokens(&["foo", "(", ")"]);
        let mut handler = CallHandler::new();

        assert!(handler.validate(&tokens));
        assert!(handler.extract(&tokens));
        assert_eq!(handler.data.name, "foo");

        let output = handler.convert();
        assert!(output.is_some());
        assert_eq!(output.unwrap(), "foo()");
    }

    #[test]
    fn test_call_with_args() {
        let tokens = make_tokens(&["bar", "(", "x", ",", "y", ")"]);
        let mut handler = CallHandler::new();

        assert!(handler.validate(&tokens));
        assert!(handler.extract(&tokens));
        assert_eq!(handler.data.arguments.len(), 2);

        let output = handler.convert();
        assert!(output.is_some());
        assert_eq!(output.unwrap(), "bar(x, y)");
    }

    #[test]
    fn test_printf_conversion() {
        let tokens = make_tokens(&["printf", "(", "\"hello\"", ")"]);
        let mut handler = CallHandler::new();

        assert!(handler.validate(&tokens));
        assert!(handler.extract(&tokens));

        let output = handler.convert();
        assert!(output.is_some());
        assert!(output.unwrap().starts_with("print!"));
    }

    #[test]
    fn test_method_call() {
        let tokens = make_tokens(&["obj", ".", "method", "(", "arg", ")"]);
        let mut handler = CallHandler::new();

        assert!(handler.validate(&tokens));
        assert!(handler.extract(&tokens));
        assert!(handler.data.is_method_call);
        assert_eq!(handler.data.name, "method");
    }
}
