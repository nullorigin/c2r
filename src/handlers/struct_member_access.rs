use crate::error::ConversionError;
use crate::handler::{HandlerResult, ParserContext, TokenHandler};
use crate::log;
use crate::token_parser::Token;

/// Handler for struct member access expressions (both dot notation and arrow notation)
/// Converts C-style struct member access to Rust-style
/// Examples:
/// - obj.member -> obj.member
/// - ptr->member -> (*ptr).member
pub struct StructMemberAccessHandler;

impl StructMemberAccessHandler {
    pub fn new() -> Self {
        Self {}
    }

    /// Check if tokens contain a struct member access expression
    /// Also returns true for simple identifiers that may be part of struct expressions
    fn can_process(&self, tokens: &[Token]) -> bool {
        // Handle single identifier tokens
        if tokens.len() == 1 && matches!(tokens[0], Token::Identifier(_)) {
            return true;
        }

        if tokens.len() < 3 {
            return false;
        }

        // Look for obj.member pattern
        for i in 0..tokens.len() - 2 {
            // Check for DOT pattern (a.b)
            if matches!(tokens[i], Token::Identifier(_))
                && tokens[i + 1] == Token::Dot
                && matches!(tokens[i + 2], Token::Identifier(_))
            {
                return true;
            }

            // Check for ARROW pattern (a->b)
            if matches!(tokens[i], Token::Identifier(_))
                && tokens[i + 1] == Token::Arrow
                && matches!(tokens[i + 2], Token::Identifier(_))
            {
                return true;
            }
        }

        false
    }

    /// Process struct member access expressions
    fn process_expression(&self, tokens: &[Token]) -> Result<String, ConversionError> {
        // Handle single identifier tokens directly
        if tokens.len() == 1 {
            if let Token::Identifier(id) = &tokens[0] {
                return Ok(id.clone());
            }
            return Ok(format!("{}", tokens[0]));
        }

        log!(
            debug,
            "Processing struct member access tokens: {:?}",
            tokens
        );

        // We'll use a recursive approach to properly handle chained access
        // First, identify segments separated by dot or arrow operators
        let mut segments = Vec::new();
        let mut current_segment = Vec::new();

        for token in tokens {
            if matches!(token, Token::Dot | Token::Arrow) {
                if !current_segment.is_empty() {
                    segments.push((current_segment, token.clone()));
                    current_segment = Vec::new();
                }
            } else {
                current_segment.push(token.clone());
            }
        }

        // Add the final segment if it's not empty
        if !current_segment.is_empty() {
            segments.push((current_segment, Token::Dot)); // Default to Dot for the last segment (won't be used)
        }

        // Now build the result by processing each segment
        let mut result = String::new();

        for (i, (segment_tokens, _)) in segments.iter().enumerate() {
            if i == 0 {
                // First segment is just the base object/pointer
                let segment_str = self.tokens_to_string(segment_tokens)?;
                result.push_str(&segment_str);
            } else {
                // For subsequent segments, check if the previous operator was an arrow
                let prev_operator = &segments[i - 1].1;

                if *prev_operator == Token::Arrow {
                    // For arrow operator, convert to (*ptr).member
                    result = format!("(*{})", result);
                }

                // Add the dot and member
                result.push('.');
                let segment_str = self.tokens_to_string(segment_tokens)?;
                result.push_str(&segment_str);
            }
        }

        log!(debug, "Converted struct member access: {}", result);
        Ok(result)
    }

    /// Helper method to convert a slice of tokens to a string
    fn tokens_to_string(&self, tokens: &[Token]) -> Result<String, ConversionError> {
        let mut result = String::new();

        for token in tokens {
            match token {
                Token::Identifier(id) => {
                    result.push_str(id);
                }
                _ => {
                    result.push_str(&format!("{}", token));
                }
            }
        }

        Ok(result)
    }
}

impl TokenHandler for StructMemberAccessHandler {
    fn can_handle(&self, tokens: &[Token], _context: &ParserContext) -> bool {
        self.can_process(tokens)
    }

    fn handle(
        &self,
        tokens: &[Token],
        _context: &mut ParserContext,
    ) -> Result<HandlerResult, ConversionError> {
        match self.process_expression(tokens) {
            Ok(rust_code) => Ok(HandlerResult::RustCode(rust_code)),
            Err(e) => Err(e),
        }
    }
}
