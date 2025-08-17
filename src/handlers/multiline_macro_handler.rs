#![allow(non_snake_case)]

use crate::config::Config;
use crate::error::ConversionError;
use crate::handler::{HandlerResult, ParserContext, TokenHandler};
use crate::log;
use crate::token_parser::Token;
use std::collections::HashMap;

/// Handler for multi-line C preprocessor #define directives
/// Converts multi-line macros (using backslash line continuations) to Rust macro_rules!
///
/// Example:
/// ```c
/// #define MULTI_LINE_MACRO(x, y) \
///     do { \
///         if (x > y) { \
///             printf("x is greater than y\n"); \
///         } else { \
///             printf("y is greater than or equal to x\n"); \
///         } \
///     } while(0)
/// ```
///
/// Converts to:
/// ```rust
/// macro_rules! MULTI_LINE_MACRO {
///     ($x:expr, $y:expr) => {
///         {
///             if $x > $y {
///                 println!("x is greater than y");
///             } else {
///                 println!("y is greater than or equal to x");
///             }
///         }
///     };
/// }
/// ```
pub struct MultilineMacroHandler {
    // Default mappings for special multi-line macros
    special_mappings: HashMap<String, String>,
}

impl MultilineMacroHandler {
    pub fn new() -> Self {
        let mut mappings = HashMap::new();
        // Add some common mappings of macro names to full Rust implementations
        // For multi-line macros that are commonly used in C code
        mappings.insert(
            "ASSERT".to_string(),
            "macro_rules! ASSERT { ($condition:expr) => { debug_assert!($condition); }; }"
                .to_string(),
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
                "Using Config from ParserContext for MultilineMacroHandler"
            );

            // If config has special multiline macro mappings, merge them with our defaults
            if let Some(config_mappings) =
                context.get_value::<HashMap<String, String>>("multiline_macro_mappings")
            {
                for (key, value) in config_mappings {
                    result.insert(key.clone(), value.clone());
                }
                log!(
                    debug,
                    "Added {} multiline macro mappings from Config",
                    config_mappings.len()
                );
            }
        }

        result
    }

    /// Check if tokens represent a multi-line #define directive
    fn can_process(&self, tokens: &[Token]) -> bool {
        if tokens.len() < 3 {
            return false;
        }

        // Look for #define pattern: # define NAME
        if let Token::Hash = tokens[0] {
            if let Token::Identifier(ident) = &tokens[1] {
                if ident == "define" && tokens.len() >= 3 {
                    // Check for backslash token which indicates a multi-line macro
                    for token in tokens {
                        if let Token::Backslash = token {
                            return true;
                        }
                    }
                }
            }
        }

        false
    }

    /// Process a multi-line #define directive and convert it to a Rust macro_rules!
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
            log!(debug, "Using special mapping for multiline macro {}", name);
            return Ok(implementation.clone());
        }

        // Check if this is a function-like macro with parameters
        if directive_tokens.len() > 1 && matches!(directive_tokens[1], Token::OpenParen) {
            return self.process_function_macro(name, &directive_tokens[1..], context);
        }

        // Process as a multi-line expression macro (no parameters)
        let lines = self.split_into_lines(directive_tokens);
        let body = self.process_macro_body(&lines, context)?;

        // Get macro formatting configuration
        let verbose_macros = if let Some(config) = context.get_value::<Config>("config") {
            config.get_feature("verbose_macros").unwrap_or(false)
        } else {
            false
        };

        let formatted_macro = if verbose_macros {
            format!(
                "// Converted from multi-line C macro: #define {} ...\nmacro_rules! {} {{ () => {{ {} }}; }}",
                name, name, body
            )
        } else {
            format!("macro_rules! {} {{ () => {{ {} }}; }}", name, body)
        };

        Ok(formatted_macro)
    }

    /// Split tokens into lines based on backslash tokens
    fn split_into_lines(&self, tokens: &[Token]) -> Vec<Vec<Token>> {
        let mut lines = Vec::new();
        let mut current_line = Vec::new();

        // Skip the macro name
        let mut skip_first = true;

        for token in tokens {
            if skip_first {
                skip_first = false;
                current_line.push(token.clone());
                continue;
            }

            match token {
                Token::Backslash => {
                    lines.push(current_line);
                    current_line = Vec::new();
                }
                _ => current_line.push(token.clone()),
            }
        }

        if !current_line.is_empty() {
            lines.push(current_line);
        }

        lines
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

        // Extract the body tokens
        let mut body_tokens = Vec::new();
        if body_start_idx < tokens.len() {
            let mut line_tokens = Vec::new();
            for token in &tokens[body_start_idx..] {
                match token {
                    Token::Backslash => {
                        // Add the current line to body_tokens
                        body_tokens.extend_from_slice(&line_tokens);
                        line_tokens.clear();
                    }
                    _ => line_tokens.push(token.clone()),
                }
            }

            // Add any remaining tokens
            body_tokens.extend_from_slice(&line_tokens);
        }

        // Replace parameter references in the body with $param
        let body_tokens = self.replace_param_refs(&body_tokens, &params);

        // Convert body tokens to lines
        let lines = self.split_into_lines(&body_tokens);

        // Process the body of the macro
        let body = self.process_macro_body(&lines, context)?;

        // Format the macro with parameter declarations
        let param_decls = params
            .iter()
            .map(|p| format!("${}: expr", p))
            .collect::<Vec<String>>()
            .join(", ");

        // Get macro formatting configuration
        let verbose_macros = if let Some(config) = context.get_value::<Config>("config") {
            config.get_feature("verbose_macros").unwrap_or(false)
        } else {
            false
        };

        let formatted_macro = if verbose_macros {
            format!(
                "// Converted from multi-line C macro: #define {}({}) ...\nmacro_rules! {} {{ ({}) => {{ {} }}; }}",
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

        for token in param_tokens {
            if let Token::Identifier(ident) = token {
                params.push(ident.clone());
                return Ok(());
            }
        }

        Err(ConversionError::general(
            "Invalid parameter in macro definition",
        ))
    }

    /// Replace parameter references in the body with Rust-style $param
    fn replace_param_refs(&self, tokens: &[Token], params: &[String]) -> Vec<Token> {
        let mut result = Vec::new();

        for token in tokens {
            if let Token::Identifier(ident) = token {
                if params.contains(ident) {
                    // This is a parameter reference, transform it to $param
                    result.push(Token::Identifier(format!("${}", ident)));
                } else {
                    result.push(token.clone());
                }
            } else {
                result.push(token.clone());
            }
        }

        result
    }

    /// Process the body of a multi-line macro
    fn process_macro_body(
        &self,
        lines: &[Vec<Token>],
        context: &ParserContext,
    ) -> Result<String, ConversionError> {
        let mut body = String::new();

        for line in lines {
            for token in line {
                body.push_str(&self.token_to_rust_string(token));
                body.push(' ');
            }
            body.push('\n');
        }

        // Apply transformations based on configuration
        if let Some(config) = context.get_value::<Config>("config") {
            // Check if we should convert do-while(0) to simple blocks
            if config.get_feature("convert_do_while_zero").unwrap_or(true) {
                body = self.convert_do_while_zero(&body);
            }

            // Check if we should convert printf to println
            if config
                .get_feature("convert_printf_to_println")
                .unwrap_or(true)
            {
                body = self.convert_printf_to_println(&body);
            }

            // Check if we should convert ternary operators
            if config
                .get_feature("convert_ternary_operators")
                .unwrap_or(true)
            {
                body = self.convert_ternary_to_if_else(&body);
            }

            // Check if we should clean up parentheses
            if config.get_feature("clean_parentheses").unwrap_or(true) {
                body = self.clean_parentheses(&body);
            }
        } else {
            // Apply default transformations
            body = self.convert_do_while_zero(&body);
            body = self.convert_printf_to_println(&body);
            body = self.convert_ternary_to_if_else(&body);
            body = self.clean_parentheses(&body);
        }

        Ok(body)
    }

    /// Convert C's do { ... } while(0) to Rust's { ... }
    fn convert_do_while_zero(&self, body: &str) -> String {
        // Simple regex-like implementation to replace do { ... } while(0)
        let mut result = body.to_string();

        // Look for "do {" and corresponding "} while ( 0 )" patterns
        if result.contains("do {") && result.contains("} while ( 0 )") {
            result = result.replace("do {", "{");
            result = result.replace("} while ( 0 )", "}");
        }

        result
    }

    /// Convert C printf statements to Rust println!
    fn convert_printf_to_println(&self, body: &str) -> String {
        let mut result = body.to_string();

        // Simple pattern replacement for printf
        // More complex conversion would need a proper parser
        if result.contains("printf (") {
            result = result.replace("printf (", "println! (");

            // Remove trailing newlines from format strings
            // This is a very basic implementation
            result = result.replace("\\n\")", "\")");
        }

        result
    }

    /// Convert C ternary operator to Rust if-else expression
    fn convert_ternary_to_if_else(&self, body: &str) -> String {
        // Find patterns like "condition ? true_expr : false_expr"
        let mut result = body.to_string();

        // Very basic implementation - would need proper parsing for robust conversion
        let mut question_idx = result.find('?');
        while let Some(q_idx) = question_idx {
            let before_question = &result[..q_idx];

            // Find matching condition start
            let mut paren_count = 0;
            let mut condition_start = 0;
            for (i, c) in before_question.chars().rev().enumerate() {
                if c == ')' {
                    paren_count += 1;
                } else if c == '(' {
                    paren_count -= 1;
                    if paren_count == 0 {
                        condition_start = q_idx - i;
                        break;
                    }
                }
            }

            // Find colon that matches this question mark
            let mut colon_idx = None;
            let mut paren_count = 0;
            for (i, c) in result[q_idx..].chars().enumerate() {
                if c == '(' {
                    paren_count += 1;
                } else if c == ')' {
                    paren_count -= 1;
                } else if c == ':' && paren_count == 0 {
                    colon_idx = Some(q_idx + i);
                    break;
                }
            }

            if let Some(c_idx) = colon_idx {
                let condition = result[condition_start..q_idx].trim();
                let true_expr = result[q_idx + 1..c_idx].trim();

                // Find end of false expression
                let mut false_end = c_idx + 1;
                for (i, c) in result[c_idx..].chars().enumerate() {
                    if c == ';' || c == ',' || c == ')' {
                        false_end = c_idx + i;
                        break;
                    }
                }

                let false_expr = result[c_idx + 1..false_end].trim();

                // Replace with if-else
                let replacement = format!(
                    "if {} {{ {} }} else {{ {} }}",
                    condition, true_expr, false_expr
                );
                result.replace_range(condition_start..false_end, &replacement);
            }

            // Find next question mark
            question_idx = result.find('?');
        }

        result
    }

    /// Clean up unnecessary parentheses in expressions
    fn clean_parentheses(&self, body: &str) -> String {
        // This is a simplistic approach - proper parsing would be better
        let mut result = body.to_string();

        // Remove double parentheses: ((expr)) -> (expr)
        let mut double_paren_found = true;
        while double_paren_found {
            double_paren_found = false;
            if result.contains("((") && result.contains("))") {
                result = result.replace("((", "(");
                result = result.replace("))", ")");
                double_paren_found = true;
            }
        }

        result
    }

    /// Convert a token to a Rust string representation
    fn token_to_rust_string(&self, token: &Token) -> String {
        match token {
            Token::Identifier(ident) => {
                // Handle C-specific identifiers
                match ident.as_str() {
                    "NULL" => "std::ptr::null()".to_string(),
                    _ => ident.clone(),
                }
            }
            Token::StringLiteral(s) => {
                format!("\"{}\"", s)
            }
            Token::CharLiteral(c) => {
                format!("'{}'", c)
            }
            Token::Number(n) => n.clone(),
            Token::Plus => "+".to_string(),
            Token::Minus => "-".to_string(),
            Token::Asterisk => "*".to_string(),
            Token::Slash => "/".to_string(),
            Token::Mod => "%".to_string(),
            Token::Ampersand => "&".to_string(),
            Token::Pipe => "|".to_string(),
            Token::Caret => "^".to_string(),
            Token::Tilde => "~".to_string(),
            Token::OpenParen => "(".to_string(),
            Token::CloseParen => ")".to_string(),
            Token::OpenBrace => "{".to_string(),
            Token::CloseBrace => "}".to_string(),
            Token::OpenBracket => "[".to_string(),
            Token::CloseBracket => "]".to_string(),
            Token::LessThan => "<".to_string(),
            Token::GreaterThan => ">".to_string(),
            Token::LessEqual => "<=".to_string(),
            Token::GreaterEqual => ">=".to_string(),
            Token::DoubleEqual => "==".to_string(),
            Token::NotEquals => "!=".to_string(),
            Token::Semicolon => ";".to_string(),
            Token::Colon => ":".to_string(),
            Token::QuestionMark => "?".to_string(),
            _ => " ".to_string(),
        }
    }
}

impl TokenHandler for MultilineMacroHandler {
    fn can_handle(&self, tokens: &[Token], _context: &ParserContext) -> bool {
        self.can_process(tokens)
    }

    fn handle(
        &self,
        tokens: &[Token],
        context: &mut ParserContext,
    ) -> Result<HandlerResult, ConversionError> {
        // Skip processing if requested by context
        if let Some(skip_macros) = context.get_value::<bool>("skip_multiline_macro_processing") {
            if *skip_macros {
                log!(
                    debug,
                    "Skipping multiline macro processing due to context flag"
                );
                return Ok(HandlerResult::NotHandled);
            }
        }

        match self.process_expression(tokens, context) {
            Ok(result) => Ok(HandlerResult::RustCode(result)),
            Err(e) => {
                log!(error, "Error processing multiline macro: {}", e);
                Ok(HandlerResult::NotHandled)
            }
        }
    }
}
