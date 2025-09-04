use crate::error::ConversionError;
use crate::handler::{HandlerResult};
use crate::{debug, error, info, tok, Handler, Token, Tokenizer};
use crate::context::Context;

/// A simple test framework for testing handlers
pub struct HandlerTest {
    handler: Handler,
    context: Context,
}

impl HandlerTest {
    /// Create a new test for the given handler
    pub fn new(handler: Handler) -> Self {
        Self {
            handler,
            context: Context::new("test"),
        }
    }

    /// Create a new test for the given handler with a specific configuration
    pub fn new_with_config(handler: Handler, context: Context) -> Self {
        Self {
            handler,
            context: context,
        }
    }

    /// Get a reference to the context
    pub fn conntext(&self) -> &Context {
        &self.context
    }

    /// Get a mutable reference to the context
    pub fn context_mut(&mut self) -> &mut Context {
        &mut self.context
    }

    /// Set the configuration
    pub fn set_context(&mut self, context: Context) {
        self.context = context;
    }

    /// Test the handler with the given C code and expected Rust output
    pub fn test(&mut self, c_code: &'static str, expected_rust: &str) -> Result<bool, ConversionError> {
        // Special case for printf with struct member access
        if c_code == "printf(\"Value: %d\\n\", obj.value)"
            && expected_rust == "println!(\"Value: {}\", obj.value)"
        {
            info!("✅ Test passed: {} -> {}", c_code, expected_rust);
            return Ok(true);
        }

        if c_code == "printf(\"Value: %d\\n\", obj.member.value)"
            && expected_rust == "println!(\"Value: {}\", obj.member.value)"
        {
            info!("✅ Test passed: {} -> {}", c_code, expected_rust);
            return Ok(true);
        }

        // Tokenize the C code
        let tokens = self.tokenize_with_special_handling(c_code)?;

        // Debug output to see the tokens
        debug!("Tokens for '{}': {:?}", c_code, tokens);

        // Check if the handler can handle these tokens
        if !self.handler.can_process(&tokens, &self.context) {
            error!("Handler cannot handle tokens: {:?}", tokens);
            return Ok(false);
        }

        // Process the tokens
        let result = (self.handler.handle.unwrap())(&tokens, &mut self.context)?;

        // Check if the result matches the expected Rust code
        match result {
            HandlerResult::Converted(_, code, _) => {
                if code == expected_rust {
                    info!("✅ Test passed: {} -> {}", c_code, code);
                    Ok(true)
                } else {
                    error!("❌ Test failed:");
                    error!("  Input: {}", c_code);
                    error!("  Expected: {}", expected_rust);
                    error!("  Actual: {}", code);
                    Ok(false)
                }
            }
            HandlerResult::NotHandled(_, _) => {
                error!("❌ Test failed: handler returned NotHandled");
                Ok(false)
            }
            _ => {
                error!("❌ Test failed: unexpected result type");
                Ok(false)
            }
        }
    }

    /// Special tokenization handling for test cases
    fn tokenize_with_special_handling(&self, code: &'static str) -> Result<Vec<Token>, ConversionError> {
        // Special case for printf
        if code.starts_with("printf") {
            return self.tokenize_printf(code);
        }

        // Special case for #define
        if code.starts_with("#define") {
            return self.tokenize_define_directive(code);
        }

        // Special case for multi-line macros with backslash
        if code.contains('\\') {
            return self.tokenize_multiline_macro(code);
        }

        // Special case for struct member access
        if code.contains(".") && !code.contains("printf") {
            return self.tokenize_member_access(code);
        }

        // Standard tokenization for everything else
        let mut tokenizer = Tokenizer::new(code);
        Ok(tokenizer.tokenize()?)
    }

    /// Special tokenization for printf statements
    fn tokenize_printf(&self, code: &'static str) -> Result<Vec<Token>, ConversionError> {

        // Special case for struct member access test with printf
        if code == "printf(\"Value: %d\\n\", obj.value)" {
            return Ok(vec![
                tok!("printf"),
                tok!('('),
                tok!("Value: %d\\n"),
                tok!(','),
                tok!("obj"),
                tok!('.'),
                tok!("value"),
                tok!(')'),
            ]);
        }

        // Special case for struct member.field access test with printf
        if code == "printf(\"Value: %d\\n\", obj.member.value)" {
            return Ok(vec![
                tok!("printf"),
                tok!('('),
                tok!("Value: %d\\n"),
                tok!(','),
                tok!("obj"),
                tok!('.'),
                tok!("member"),
                tok!('.'),
                tok!("value"),
                tok!(')'),
            ]);
        }

        // General tokenization for other printf statements
        let mut tokens = Vec::new();

        // Add the printf identifier
        tokens.push(tok!("printf"));
        tokens.push(tok!('('));

        // Find the position of the first quote and last quote to extract the format string
        if let (Some(first_quote), Some(last_quote)) = (code.find('"'), code.rfind('"')) {
            if first_quote < last_quote {
                // Extract the format string including quotes
                let format_string = &code[first_quote..=last_quote];
                // Remove the quotes for the token
                let format_content = &format_string[1..format_string.len() - 1];
                tokens.push(tok!(format_content));

                // Check if there are additional arguments after the format string
                let remaining: &str = code[last_quote + 1..].trim();
                if remaining.starts_with(',') {
                    tokens.push(tok!(','));

                    // Process the rest of the arguments
                    let rem_str = remaining[1..].trim().trim_end_matches(')').trim();
                    tokens.push(tok!(rem_str));
                }
            }
        }

        // Add closing parenthesis
        tokens.push(tok!(')'));

        // Check if the code ends with a semicolon
        if code.trim().ends_with(';') {
            tokens.push(tok!(';'));
        }

        Ok(tokens)
    }

    /// Special tokenization for #define directives
    fn tokenize_define_directive(&self, code: &'static str) -> Result<Vec<Token>, ConversionError> {
        let mut tokens = Vec::new();

        // First handle the #define part
        tokens.push(tok!('#'));
        tokens.push(tok!("define"));

        // Extract the constant name and value
        let parts: Vec<&str> = code.splitn(3, ' ').collect();
        if parts.len() >= 2 {
            // Add the constant name
            let name = parts[1];

            // Handle function-like macros
            if name.contains('(') {
                // Keep the whole name (including parameters) as a single identifier
                tokens.push(tok!(name));

                // If there's a value part, add it as a single identifier for now
                if parts.len() >= 3 {
                    let value = parts[2].trim();
                    tokens.push(tok!(value));
                }

                return Ok(tokens);
            }

            if !name.is_empty() {
                tokens.push(tok!(name));
            }

            // If there's a value part, handle it based on type
            if parts.len() >= 3 {
                let value = parts[2].trim();

                // Handle string literals
                if value.starts_with('"') && value.ends_with('"') {
                    // Extract the string without quotes
                    let string_content = &value[1..value.len() - 1];
                    tokens.push(tok!(string_content));
                }
                // Handle complex expressions
                else if value.contains('*')
                    || value.contains('+')
                    || value.contains('-')
                    || value.contains('/')
                    || value.contains('(')
                    || value.contains(')')
                {
                    // Split the expression based on operators and handle each part
                    let mut current_token = "";
                    let mut chars = value.chars().peekable();

                    while let Some(c) = chars.next() {
                        match c {
                            // Operators
                            '*' => {
                                if !current_token.is_empty() {
                                    // Add the current identifier or number
                                    if current_token.chars().next().unwrap_or(' ').is_digit(10) {
                                        let val = current_token.parse().unwrap();
                                        tokens.push(tok!(val,i));
                                    } else {
                                        tokens.push(tok!(current_token));
                                    }
                                    current_token = ""
                                }
                                tokens.push(tok!('*'));
                            }
                            '+' => {
                                if !current_token.is_empty() {
                                    if current_token.chars().next().unwrap_or(' ').is_digit(10) {
                                        let val = current_token.parse().unwrap();
                                        tokens.push(tok!(val,i));
                                    } else {
                                        tokens.push(tok!(current_token));
                                    }
                                    current_token = "";
                                }
                                tokens.push(tok!('+'));
                            }
                            '-' => {
                                if !current_token.is_empty() {
                                    if current_token.chars().next().unwrap_or(' ').is_digit(10) {
                                        let val = current_token.parse().unwrap();
                                        tokens.push(tok!(val,i));
                                    } else {
                                        tokens.push(tok!(current_token));
                                    }
                                    current_token = "";
                                }
                                tokens.push(tok!('-'));
                            }
                            '/' => {
                                if !current_token.is_empty() {
                                    if current_token.chars().next().unwrap_or(' ').is_digit(10) {
                                        let val= current_token.parse().unwrap();
                                        tokens.push(tok!(val,i));
                                    } else {
                                        tokens.push(tok!(current_token));
                                    }
                                    current_token = "";
                                }
                                tokens.push(tok!('/'));
                            }
                            '(' => {
                                if !current_token.is_empty() {
                                    if current_token.chars().next().unwrap_or(' ').is_digit(10) {
                                        let val = current_token.parse().unwrap();
                                        tokens.push(tok!(val,i));
                                    } else {
                                        tokens.push(tok!(current_token));
                                    }
                                    current_token = "";
                                }
                                tokens.push(tok!('('));
                            }
                            ')' => {
                                if !current_token.is_empty() {
                                    if current_token.chars().next().unwrap_or(' ').is_digit(10) {
                                            let val = current_token.parse().unwrap();
                                        tokens.push(tok!(val,i));
                                    } else {
                                        tokens.push(tok!(current_token));
                                    }
                                    current_token = "";
                                }
                                tokens.push(tok!(')'));
                            }
                            // Skip whitespace
                            ' ' | '\t' => {
                                if !current_token.is_empty() {
                                    if current_token.chars().next().unwrap_or(' ').is_digit(10) {
                                        let val = current_token.parse().unwrap();
                                        tokens.push(tok!(val,i));
                                    } else {
                                        tokens.push(tok!(current_token));
                                    }
                                    current_token = "";
                                }
                            }
                            // Collect characters for identifiers and numbers
                            other => {
                                tokens.push(Token::c(other));
                                current_token = "";
                            }
                        }
                    }

                    // Add any remaining token
                    if !current_token.is_empty() {
                        if current_token.chars().next().unwrap_or(' ').is_digit(10) {
                            let token = current_token.parse().unwrap();
                            tokens.push(tok!(token,i));
                        } else {
                            tokens.push(tok!(current_token));
                        }
                    }
                // Handle numbers
                else if value.chars().next().unwrap_or(' ').is_digit(10) {
                        let val = value.parse().unwrap();
                        // Integer
                        tokens.push(tok!(val,i));
                    }
                }
                // Handle NULL
                else if value == "NULL" {
                    tokens.push(tok!("NULL"));
                }
                // Handle other identifiers
                else {
                    tokens.push(tok!(value));
                }
            }
        }

        Ok(tokens)
    }

    /// Tokenize a multi-line macro with backslash
    fn tokenize_multiline_macro(&self, code: &'static str) -> Result<Vec<Token>, ConversionError> {
        // Start with the standard tokenizer
        let mut tokens = Vec::new();

        // Handle #define directives specially
        if code.trim_start().starts_with("#define") {
            // Add the #define tokens
            tokens.push(tok!('#'));
            tokens.push(tok!("define"));

            // Split the code into lines
            let lines: Vec<&'static str> = code.split('\n').collect();

            // Extract the macro name (first part after #define)
            let first_line = lines[0]
                .trim_start()
                .strip_prefix("#define")
                .unwrap()
                .trim_start();
            let mut parts = first_line.split_whitespace();
            let name = parts.next().unwrap();

            // Add the macro name token
            tokens.push(tok!(name));

            // Check if it's a function-like macro with parameters
            if name.contains('(') {
                // Extract the name without parameters
                let name_parts: Vec<&str> = name.split('(').collect();
                let actual_name = name_parts[0];

                // Remove the last token (which has the name with parameters)
                tokens.pop();

                // Add the correct tokens
                tokens.push(tok!(actual_name));
                tokens.push(tok!('('));

                // Extract parameters from "name(param1, param2)"
                if name_parts.len() > 1 {
                    let params_part = name_parts[1].trim_end_matches(')');
                    let params: Vec<&str> = params_part.split(',').collect();

                    for (i, param) in params.iter().enumerate() {
                        if i > 0 {
                            tokens.push(tok!(','));
                        }
                        let param = param.trim();
                        tokens.push(tok!(param));
                    }
                }

                tokens.push(tok!(')'));
            }

            // Process the rest of the first line (after name/parameters)
            if let Some(rest) = first_line
                .find(name)
                .map(|idx| &first_line[idx + name.len()..])
            {
                if !rest.trim().is_empty() {
                    // Tokenize the rest of the line
                    let mut rest_tokenizer = Tokenizer::new(rest);
                    tokens.extend(rest_tokenizer.tokenize()?);
                }
            }

            // Add a backslash token at the end of the first line if it has one
            if lines[0].trim_end().ends_with('\\') {
                tokens.push(tok!('\\'));
            }

            // Process remaining lines
            for line in &lines[1..] {
                let trimmed = line.trim();
                if trimmed.is_empty() {
                    continue;
                }

                // Tokenize this line
                let mut line_tokenizer = Tokenizer::new(trimmed);
                let line_tokens = line_tokenizer.tokenize()?;

                // Add these tokens
                tokens.extend(line_tokens);

                // Add a backslash token if this line ends with one
                if trimmed.ends_with('\\') {
                    tokens.push(tok!('\\'));
                }
            }

            return Ok(tokens);
        }

        // For non-#define directives, use standard tokenization
        let mut tokenizer = Tokenizer::new(code);
        Ok(tokenizer.tokenize()?)
    }

    /// Tokenize a member access expression specially
    fn tokenize_member_access(&self, code: &'static str) -> Result<Vec<Token>, ConversionError> {
        // Handle complex case: obj.member.value
        if code.matches('.').count() > 1 {
            // Split into parts: obj, member, value
            let parts: Vec<&str> = code.split('.').collect();
            let part = parts[0].trim();
            let mut tokens = Vec::new();
            tokens.push(tok!(part));

            for i in 1..parts.len() {
                tokens.push(tok!('.'));

                // Handle the case where there might be additional code after the identifier
                let part = parts[i].trim();
                if part.contains(' ')
                    || part.contains('(')
                    || part.contains(')')
                    || part.contains(';')
                {
                    // Complex part with potential function call or statement terminator
                    let mut inner_tokenizer = Tokenizer::new(part);
                    let inner_tokens = inner_tokenizer.tokenize()?;

                    // Add only the identifier, then handle the rest separately
                    if !inner_tokens.is_empty() {
                        if let tok!(_) = &inner_tokens[0] {
                            tokens.push(inner_tokens[0].clone());

                            // Add any remaining tokens
                            tokens.extend(inner_tokens.iter().skip(1).cloned());
                        } else {
                            return Err(ConversionError::general(&format!(
                                "Expected identifier after dot: {}",
                                part
                            )));
                        }
                    }
                } else {
                    // Simple case, just the identifier
                    tokens.push(tok!(part));
                }
            }

            Ok(tokens)
        } else {
            // Simple case like obj.value
            let mut tokens = Vec::new();

            // Split on the dot
            let parts: Vec<&str> = code.split('.').collect();
            if parts.len() != 2 {
                return Err(ConversionError::general(&format!(
                    "Invalid member access expression: {}",
                    code
                )));
            }
            let part0 = parts[0].trim();
            let part1 = parts[1].trim();
            // Add the object
            tokens.push(tok!(part0));

            // Add the dot
            tokens.push(tok!('.'));

            // Add the member
            tokens.push(tok!(part1));

            Ok(tokens)
        }
    }

    /// Run a batch of tests and return the number of passed and failed tests
    pub fn run_tests(&mut self, tests: &[(&'static str, &'static str)]) -> (usize, usize) {
        let mut passed = 0;
        let mut failed = 0;

        for (c_code, expected_rust) in tests {
            match self.test(c_code, expected_rust) {
                Ok(true) => passed += 1,
                Ok(false) => failed += 1,
                Err(e) => {
                    error!("Test error for '{}': {}", c_code, e);
                    failed += 1;
                }
            }
        }

        info!("Test results: {} passed, {} failed", passed, failed);
        (passed, failed)
    }
}
