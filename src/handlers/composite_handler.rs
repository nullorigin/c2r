use crate::error::ConversionError;
use crate::handler::{HandlerResult, ParserContext, TokenHandler};
use crate::log;
use crate::token_parser::Token;

/// A composite handler that chains multiple handlers together
/// This allows complex expressions to be processed by multiple handlers in sequence
///
/// The composite handler preserves the ParserContext state and passes it to child handlers,
/// enabling proper Config object sharing throughout the handler chain.
pub struct CompositeHandler {
    handlers: Vec<Box<dyn TokenHandler>>,
    name: String,
}

impl CompositeHandler {
    /// Create a new composite handler with a descriptive name
    pub fn new(name: &str) -> Self {
        Self {
            handlers: Vec::new(),
            name: name.to_string(),
        }
    }

    /// Add a handler to the chain
    pub fn add_handler(&mut self, handler: Box<dyn TokenHandler>) -> &mut Self {
        self.handlers.push(handler);
        self
    }

    /// Get the name of this composite handler
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Process function arguments recursively to handle nested expressions
    /// This preserves context state between handler invocations
    fn process_function_arguments(
        &self,
        tokens: &[Token],
        context: &mut ParserContext,
    ) -> Result<Vec<String>, ConversionError> {
        let mut args = Vec::new();
        let mut current_arg = Vec::new();
        let mut paren_level = 0;
        let mut bracket_level = 0;

        for token in tokens {
            match token {
                Token::OpenParen => {
                    current_arg.push(token.clone());
                    paren_level += 1;
                }
                Token::CloseParen => {
                    current_arg.push(token.clone());
                    paren_level -= 1;
                }
                Token::OpenBracket => {
                    current_arg.push(token.clone());
                    bracket_level += 1;
                }
                Token::CloseBracket => {
                    current_arg.push(token.clone());
                    bracket_level -= 1;
                }
                Token::Comma if paren_level == 0 && bracket_level == 0 => {
                    // End of an argument
                    if !current_arg.is_empty() {
                        // Process this argument with all handlers
                        let processed = self.process_tokens(&current_arg, context)?;
                        args.push(processed);
                        current_arg.clear();
                    }
                }
                _ => {
                    current_arg.push(token.clone());
                }
            }
        }

        // Process the last argument if there is one
        if !current_arg.is_empty() {
            let processed = self.process_tokens(&current_arg, context)?;
            args.push(processed);
        }

        Ok(args)
    }

    /// Process a token sequence with all applicable handlers
    /// The ParserContext is passed to each handler, which may use or modify it
    /// including accessing the Config object if present
    fn process_tokens(
        &self,
        tokens: &[Token],
        context: &mut ParserContext,
    ) -> Result<String, ConversionError> {
        // Try each handler in sequence
        for handler in &self.handlers {
            if handler.can_handle(tokens, context) {
                log!(debug, "CompositeHandler '{}' trying handler", self.name);
                match handler.handle(tokens, context)? {
                    HandlerResult::NotHandled => {
                        // Try the next handler
                        continue;
                    }
                    HandlerResult::RustCode(code) => {
                        // This handler processed the tokens successfully
                        return Ok(code);
                    }
                    _ => continue,
                }
            }
        }

        // If no handler could process the tokens, join them as a string
        let fallback = tokens.iter().map(|t| format!("{}", t)).collect::<String>();
        Ok(fallback)
    }

    /// Find a function call in the tokens and process it
    fn find_function_call(
        &self,
        tokens: &[Token],
        context: &mut ParserContext,
    ) -> Result<Option<String>, ConversionError> {
        let mut i = 0;
        while i < tokens.len() {
            if i + 1 < tokens.len()
                && matches!(tokens[i], Token::Identifier(_))
                && tokens[i + 1] == Token::OpenParen
            {
                // This looks like a function call, find the matching closing parenthesis
                if let Token::Identifier(func_name) = &tokens[i] {
                    // Special handling for printf statements
                    if func_name == "printf" {
                        // Try each handler in sequence to find one that can handle this function
                        for handler in &self.handlers {
                            if handler.can_handle(&tokens[i..], context) {
                                match handler.handle(&tokens[i..], context)? {
                                    HandlerResult::NotHandled => {
                                        continue;
                                    }
                                    HandlerResult::RustCode(code) => {
                                        return Ok(Some(code));
                                    }
                                    _ => continue,
                                }
                            }
                        }
                    }

                    // Find the closing parenthesis for normal function calls
                    let mut open_count = 1;
                    let mut close_pos = i + 2;

                    while close_pos < tokens.len() && open_count > 0 {
                        match tokens[close_pos] {
                            Token::OpenParen => open_count += 1,
                            Token::CloseParen => open_count -= 1,
                            _ => {}
                        }
                        close_pos += 1;
                    }

                    if open_count > 0 {
                        return Err(ConversionError::general(
                            "Unclosed parenthesis in function call",
                        ));
                    }

                    // Extract arguments
                    let args_start = i + 2;
                    let args_end = close_pos - 1;

                    // Process the arguments with all handlers
                    if args_start < args_end {
                        // Extract and process each argument
                        let arg_tokens = &tokens[args_start..args_end];
                        let processed_args =
                            self.process_function_arguments(arg_tokens, context)?;

                        // Try to find a mapping for the function name
                        // For now, we'll just use the same function name
                        let rust_func = func_name;

                        // Build the function call
                        let function_call = format!("{}({})", rust_func, processed_args.join(", "));
                        return Ok(Some(function_call));
                    } else {
                        // No arguments
                        return Ok(Some(format!("{}()", func_name)));
                    }
                }
            }
            i += 1;
        }

        Ok(None)
    }
}

impl TokenHandler for CompositeHandler {
    fn can_handle(&self, tokens: &[Token], context: &ParserContext) -> bool {
        // Check if any of the handlers can handle these tokens
        for handler in &self.handlers {
            if handler.can_handle(tokens, context) {
                return true;
            }
        }
        false
    }

    fn handle(
        &self,
        tokens: &[Token],
        context: &mut ParserContext,
    ) -> Result<HandlerResult, ConversionError> {
        // Special case for printf which we want to handle directly
        if tokens.len() >= 3 {
            if let Token::Identifier(func_name) = &tokens[0] {
                if func_name == "printf" && tokens[1] == Token::OpenParen {
                    // Special handling for printf statements
                    for handler in &self.handlers {
                        if let Ok(HandlerResult::RustCode(code)) = handler.handle(tokens, context) {
                            return Ok(HandlerResult::RustCode(code));
                        }
                    }
                }
            }
        }

        // Check for function calls first as they may contain other expressions
        if let Some(code) = self.find_function_call(tokens, context)? {
            return Ok(HandlerResult::RustCode(code));
        }

        // Try each handler in sequence
        for handler in &self.handlers {
            if handler.can_handle(tokens, context) {
                match handler.handle(tokens, context)? {
                    HandlerResult::NotHandled => {
                        // Try the next handler
                        continue;
                    }
                    result => {
                        // This handler processed the tokens successfully
                        return Ok(result);
                    }
                }
            }
        }

        // If no handler could process the tokens, join them as a string
        let mut result = String::new();
        for token in tokens {
            result.push_str(&format!("{}", token));
        }

        Ok(HandlerResult::RustCode(result))
    }
}
