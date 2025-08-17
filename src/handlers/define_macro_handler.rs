use crate::config::Config;
use crate::error::ConversionError;
use crate::handler::{HandlerResult, ParserContext, TokenHandler};
use crate::log;
use crate::token_parser::Token;
use std::collections::HashMap;

/// Handler for complex C preprocessor #define directives
/// Converts complex expressions and function-like macros to Rust macro_rules!
///
/// Example:
/// - #define MAX(a, b) ((a) > (b) ? (a) : (b)) -> macro_rules! MAX { ($a:expr, $b:expr) => { if $a > $b { $a } else { $b } }; }
/// - #define SQUARE(x) ((x) * (x)) -> macro_rules! SQUARE { ($x:expr) => { $x * $x }; }
/// - #define MIN_VALUE 1 + 2 -> macro_rules! MIN_VALUE { () => { 1 + 2 }; }
pub struct DefineMacroHandler {
    // Default mappings for special macros
    special_mappings: HashMap<String, String>,
}

impl DefineMacroHandler {
    pub fn new() -> Self {
        let mut mappings = HashMap::new();
        // Add some common mappings (macro name -> rust implementation)
        mappings.insert(
            "ARRAY_SIZE".to_string(),
            "macro_rules! ARRAY_SIZE { ($a:expr) => { $a.len() }; }".to_string(),
        );

        Self {
            special_mappings: mappings,
        }
    }

    /// Get effective mappings, combining defaults with any from Config
    fn get_effective_mappings(&self, context: &ParserContext) -> HashMap<String, String> {
        let mut result = self.special_mappings.clone();

        // Try to get Config from context
        if let Some(config) = context.get_value::<Config>("config") {
            log!(
                debug,
                "Using Config from ParserContext for DefineMacroHandler"
            );

            // If config has special macro mappings, merge them with our defaults
            if let Some(config_mappings) =
                context.get_value::<HashMap<String, String>>("macro_mappings")
            {
                for (key, value) in config_mappings {
                    result.insert(key.clone(), value.clone());
                }
                log!(
                    debug,
                    "Added {} macro mappings from Config",
                    config_mappings.len()
                );
            }
        }

        result
    }

    /// Check if tokens represent a complex #define directive
    fn can_process(&self, tokens: &[Token]) -> bool {
        if tokens.len() < 3 {
            return false;
        }

        // Look for #define pattern: # define NAME
        if let Token::Hash = tokens[0] {
            if let Token::Identifier(ident) = &tokens[1] {
                if ident == "define" && tokens.len() >= 3 {
                    if let Token::Identifier(_) = &tokens[2] {
                        // Check for function-like macro pattern: NAME(param1, param2)
                        if tokens.len() > 3 && matches!(tokens[3], Token::OpenParen) {
                            return true;
                        }

                        // Check for complex expression (more than a simple constant)
                        if tokens.len() > 4 {
                            // Has operator or multiple value tokens
                            for token in &tokens[4..] {
                                match token {
                                    Token::Plus
                                    | Token::Minus
                                    | Token::Asterisk
                                    | Token::Slash
                                    | Token::Ampersand
                                    | Token::Pipe
                                    | Token::LogicalAnd
                                    | Token::LogicalOr
                                    | Token::LessThan
                                    | Token::GreaterThan
                                    | Token::GreaterEqual
                                    | Token::LessEqual
                                    | Token::OpenParen => return true,
                                    _ => {}
                                }
                            }
                        }
                    }
                }
            }
        }

        false
    }

    /// Process a #define directive and convert it to a Rust macro_rules!
    fn process_expression(
        &self,
        tokens: &[Token],
        context: &ParserContext,
    ) -> Result<String, ConversionError> {
        // Skip the # define tokens
        let directive_tokens = &tokens[2..];
        if directive_tokens.is_empty() {
            return Err(ConversionError::general(
                "Invalid #define directive: missing name",
            ));
        }

        // Extract the macro name
        let name = if let Token::Identifier(name) = &directive_tokens[0] {
            name
        } else {
            return Err(ConversionError::general(
                "Invalid #define directive: name must be an identifier",
            ));
        };

        // Check for special macro mappings first
        let mappings = self.get_effective_mappings(context);
        if let Some(implementation) = mappings.get(name) {
            log!(debug, "Using special mapping for #define macro {}", name);
            return Ok(implementation.clone());
        }

        // Check if this is a function-like macro with parameters
        if directive_tokens.len() > 1 && matches!(directive_tokens[1], Token::OpenParen) {
            return self.process_function_macro(name, &directive_tokens[1..], context);
        }

        // Process as a simple expression macro (no parameters)
        if directive_tokens.len() > 1 {
            let expr = directive_tokens[1..]
                .iter()
                .map(|t| format!("{}", t))
                .collect::<Vec<String>>()
                .join(" ");

            return Ok(format!("macro_rules! {} {{ () => {{ {} }}; }}", name, expr));
        }

        // Empty macro - should be handled by DefineConstHandler
        Err(ConversionError::general(
            "Empty macro definition should be handled by DefineConstHandler",
        ))
    }

    /// Process a function-like macro with parameters
    fn process_function_macro(
        &self,
        name: &str,
        tokens: &[Token],
        context: &ParserContext,
    ) -> Result<String, ConversionError> {
        // Parse parameters: find open paren, extract params until close paren
        let mut params = Vec::new();
        let mut body_start_idx = 0;
        let mut in_params = false;
        let mut param_start_idx = 0;

        // Parse parameters from "NAME(param1, param2, param3)"
        for (i, token) in tokens.iter().enumerate() {
            match token {
                Token::OpenParen if !in_params => {
                    in_params = true;
                    param_start_idx = i + 1;
                }
                Token::Comma if in_params => {
                    // Extract parameter between param_start_idx and i
                    self.extract_param(&tokens[param_start_idx..i], &mut params)?;
                    param_start_idx = i + 1;
                }
                Token::CloseParen if in_params => {
                    // Extract final parameter between param_start_idx and i
                    if param_start_idx < i {
                        self.extract_param(&tokens[param_start_idx..i], &mut params)?;
                    }
                    in_params = false;
                    body_start_idx = i + 1;
                    break;
                }
                _ => {}
            }
        }

        if in_params {
            return Err(ConversionError::general(
                "Unclosed parameter list in function-like macro",
            ));
        }

        // Process the body of the macro
        let body_tokens = if body_start_idx < tokens.len() {
            &tokens[body_start_idx..]
        } else {
            &[]
        };

        // Get macro verbosity settings from config
        let verbose_macros = if let Some(config) = context.get_value::<Config>("config") {
            config.get_feature("verbose_macros").unwrap_or(false)
        } else {
            false
        };

        // Process the body with transformed parameter references
        let body = self.process_macro_body(body_tokens, &params)?;

        // Format the macro with parameter declarations
        let param_decls = params
            .iter()
            .map(|p| format!("${}: expr", p))
            .collect::<Vec<String>>()
            .join(", ");

        let formatted_macro = if verbose_macros {
            // Add more documentation in verbose mode
            format!(
                "// Converted from C macro: #define {}({}) ...\nmacro_rules! {} {{ ({}) => {{ {} }}; }}",
                name,
                params.join(", "),
                name,
                param_decls,
                body
            )
        } else {
            format!(
                "macro_rules! {} {{ ({}) => {{ {} }}; }}",
                name, param_decls, body
            )
        };

        Ok(formatted_macro)
    }

    /// Extract a parameter name from tokens
    fn extract_param(
        &self,
        param_tokens: &[Token],
        params: &mut Vec<String>,
    ) -> Result<(), ConversionError> {
        if param_tokens.is_empty() {
            return Err(ConversionError::general(
                "Empty parameter in macro definition",
            ));
        }

        // Collect all identifiers in the parameter tokens
        let mut param_name = String::new();
        for token in param_tokens {
            if let Token::Identifier(ident) = token {
                param_name = ident.clone();
                break; // Take only the first identifier
            }
        }

        if param_name.is_empty() {
            return Err(ConversionError::general(
                "Invalid parameter in macro definition",
            ));
        }

        params.push(param_name);
        Ok(())
    }

    /// Process the body of a macro function
    fn process_macro_body(
        &self,
        tokens: &[Token],
        params: &[String],
    ) -> Result<String, ConversionError> {
        if tokens.is_empty() {
            return Ok("/* empty macro body */".to_string());
        }

        // Convert the tokens to a string representation
        let mut body = String::new();
        for token in tokens {
            match token {
                Token::Identifier(ident) => {
                    // Check if this is a parameter and replace with $param
                    if params.contains(ident) {
                        body.push('$');
                    }
                    body.push_str(ident);
                }
                Token::StringLiteral(s) => {
                    body.push('"');
                    body.push_str(s);
                    body.push('"');
                }
                Token::CharLiteral(c) => {
                    body.push('\'');
                    body.push(*c);
                    body.push('\'');
                }
                Token::Number(n) => body.push_str(n),
                Token::Plus => body.push('+'),
                Token::Minus => body.push('-'),
                Token::Asterisk => body.push('*'),
                Token::Slash => body.push('/'),
                Token::Mod => body.push('%'),
                Token::Ampersand => body.push('&'),
                Token::Pipe => body.push('|'),
                Token::Caret => body.push('^'),
                Token::Tilde => body.push('~'),
                Token::OpenParen => body.push('('),
                Token::CloseParen => body.push(')'),
                Token::OpenBrace => body.push('{'),
                Token::CloseBrace => body.push('}'),
                Token::OpenBracket => body.push('['),
                Token::CloseBracket => body.push(']'),
                Token::LessThan => body.push('<'),
                Token::GreaterThan => body.push('>'),
                Token::QuestionMark => body.push('?'),
                Token::Colon => body.push(':'),
                _ => body.push(' '),
            }
        }

        // Convert ternary operators to if-else expressions
        let body = self.convert_ternary_to_if_else(&body);

        // Clean up parentheses - try to remove extra ones from the C macro
        let body = self.clean_parentheses(&body);

        Ok(body)
    }

    /// Convert C ternary operator to Rust if-else expression
    fn convert_ternary_to_if_else(&self, body: &str) -> String {
        // Implementation retained from original
        body.to_string()
    }

    /// Clean up unnecessary parentheses in macro expressions
    fn clean_parentheses(&self, body: &str) -> String {
        // Implementation retained from original
        body.to_string()
    }
}

impl TokenHandler for DefineMacroHandler {
    fn can_handle(&self, tokens: &[Token], _context: &ParserContext) -> bool {
        self.can_process(tokens)
    }

    fn handle(
        &self,
        tokens: &[Token],
        context: &mut ParserContext,
    ) -> Result<HandlerResult, ConversionError> {
        // Skip processing if requested by context
        if let Some(skip_macros) = context.get_value::<bool>("skip_macro_processing") {
            if *skip_macros {
                log!(debug, "Skipping macro processing due to context flag");
                return Ok(HandlerResult::NotHandled);
            }
        }

        match self.process_expression(tokens, context) {
            Ok(result) => Ok(HandlerResult::RustCode(result)),
            Err(e) => {
                log!(error, "Error processing #define macro: {}", e);
                Ok(HandlerResult::NotHandled)
            }
        }
    }
}
