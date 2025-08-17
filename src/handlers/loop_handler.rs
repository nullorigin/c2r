use crate::error::ConversionError;
use crate::handler::{HandlerResult, ParserContext, TokenHandler};
use crate::token_parser::{Statement, Token};

/// Handler for converting C loop structures to idiomatic Rust loops
///
/// This handler supports:
/// 1. "while (true)" or "while (1)" to Rust's "loop" construct
/// 2. C-style for loops to Rust's range-based for loops when possible
/// 3. Complex for loops to while loops in Rust
/// 4. Standard while loops
pub struct LoopHandler;

impl LoopHandler {
    pub fn new() -> Self {
        LoopHandler
    }

    /// Determines if this is a "while true" or "while (1)" loop that should be converted
    /// to a Rust "loop {}" construct
    fn is_infinite_loop(&self, condition: &[Token]) -> bool {
        // Check for "while (true)" pattern
        if condition.len() == 1 {
            match &condition[0] {
                Token::Identifier(id) if id == "true" => return true,
                Token::Number(lit) if lit == "1" => return true,
                _ => {}
            }
        }

        // Check for "while (1)" with parentheses
        if condition.len() == 3
            && condition[0] == Token::OpenParen
            && condition[2] == Token::CloseParen
        {
            if let Token::Number(lit) = &condition[1] {
                if lit == "1" {
                    return true;
                }
            }
        }

        false
    }

    /// Converts a C-style for loop into an appropriate Rust construct
    /// Handles cases like: for(i=0; i<n; i++) → for i in 0..n
    fn convert_for_loop(
        &self,
        init: &[Token],
        condition: &[Token],
        increment: &[Token],
        body: &[Statement],
    ) -> Result<HandlerResult, ConversionError> {
        // Check if this is a standard C for-loop pattern that can be converted to a range-based for loop
        if let Some(rust_for_loop) =
            self.convert_to_range_based_loop(init, condition, increment, body)
        {
            return Ok(HandlerResult::RustCode(rust_for_loop));
        }

        // Otherwise, convert to a while loop with initialization and increment parts
        let init_str = if !init.is_empty() {
            let tokens_str = init
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<String>>()
                .join("");
            format!("{};\n    ", tokens_str)
        } else {
            String::new()
        };

        let condition_str = if !condition.is_empty() {
            condition
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<String>>()
                .join("")
        } else {
            "true".to_string()
        };

        let body_str = if !body.is_empty() {
            // Convert block statements
            let mut block_str = String::from("{\n        ");
            for stmt in body {
                // Add statement representation with proper indentation
                block_str.push_str(&self.format_statement(stmt));
                block_str.push_str(";\n        ");
            }

            // Add increment at the end of the loop body
            if !increment.is_empty() {
                let inc_str = increment
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<String>>()
                    .join("");
                block_str.push_str(&inc_str);
                block_str.push_str(";\n    ");
            }

            block_str.push_str("}");
            block_str
        } else {
            String::from("{}")
        };

        // Combine into a while loop with initialization
        let result = format!(
            "{}{}{}",
            init_str,
            format!("while {} ", condition_str),
            body_str
        );

        Ok(HandlerResult::RustCode(result))
    }

    /// Attempts to convert a C-style for loop to a Rust range-based for loop
    /// Returns None if the loop doesn't match a standard pattern that can be converted
    fn convert_to_range_based_loop(
        &self,
        init: &[Token],
        condition: &[Token],
        increment: &[Token],
        body: &[Statement],
    ) -> Option<String> {
        // Try to extract init variable and start value (e.g., i = 0)
        let (var_name, start_value) = self.extract_init_var_and_value(init)?;

        // Try to extract end condition (e.g., i < n or i <= n)
        let (end_value, inclusive) = self.extract_end_condition(condition, &var_name)?;

        // Verify increment is simple (e.g., i++ or ++i or i += 1)
        if !self.is_simple_increment(increment, &var_name) {
            return None;
        }

        // Generate the range-based for loop
        let range_operator = if inclusive { "..=" } else { ".." };

        // Convert body
        let body_str = if !body.is_empty() {
            let mut block_str = String::from("{\n        ");
            for stmt in body {
                block_str.push_str(&self.format_statement(stmt));
                block_str.push_str(";\n        ");
            }
            block_str.push_str("}");
            block_str
        } else {
            String::from("{}")
        };

        Some(format!(
            "for {} in {}{}{} {}",
            var_name, start_value, range_operator, end_value, body_str
        ))
    }

    /// Extract the variable name and start value from loop initialization
    /// e.g., "int i = 0" or "i = 0" -> ("i", "0")
    fn extract_init_var_and_value(&self, init: &[Token]) -> Option<(String, String)> {
        // Skip the type declaration if present (int i = 0)
        let mut idx = 0;
        while idx < init.len() && init[idx] != Token::Equal {
            if let Token::Identifier(var_name) = &init[idx] {
                // This could be our variable name
                if idx + 2 < init.len() && init[idx + 1] == Token::Equal {
                    // Found pattern "i = value"
                    if let Token::Number(start_value) = &init[idx + 2] {
                        return Some((var_name.clone(), start_value.clone()));
                    }
                }
            }
            idx += 1;
        }

        None
    }

    /// Extract the end condition and whether it's inclusive from the condition expression
    /// e.g., "i < 10" -> ("10", false) or "i <= 10" -> ("10", true)
    fn extract_end_condition(&self, condition: &[Token], var_name: &str) -> Option<(String, bool)> {
        // Look for patterns like "i < n" or "i <= n"
        let mut idx = 0;
        while idx < condition.len() {
            if let Token::Identifier(id) = &condition[idx] {
                if id == var_name && idx + 2 < condition.len() {
                    // Check for < or <= operator
                    match condition[idx + 1] {
                        Token::LessThan => {
                            // i < end_value
                            match &condition[idx + 2] {
                                Token::Number(end_val) => {
                                    return Some((end_val.clone(), false));
                                }
                                Token::Identifier(end_id) => {
                                    return Some((end_id.clone(), false));
                                }
                                _ => {}
                            }
                        }
                        Token::LessThanOrEqual => {
                            // i <= end_value
                            match &condition[idx + 2] {
                                Token::Number(end_val) => {
                                    return Some((end_val.clone(), true));
                                }
                                Token::Identifier(end_id) => {
                                    return Some((end_id.clone(), true));
                                }
                                _ => {}
                            }
                        }
                        _ => {}
                    }
                }
            }
            idx += 1;
        }

        None
    }

    /// Check if the increment is a simple increment of the loop variable
    /// e.g., i++, ++i, or i += 1
    fn is_simple_increment(&self, increment: &[Token], var_name: &str) -> bool {
        if increment.is_empty() {
            return false;
        }

        // Check for i++
        if increment.len() == 2 {
            if let Token::Identifier(id) = &increment[0] {
                if id == var_name && increment[1] == Token::Increment {
                    return true;
                }
            }
        }

        // Check for ++i
        if increment.len() == 2 {
            if let Token::Identifier(id) = &increment[1] {
                if id == var_name && increment[0] == Token::Increment {
                    return true;
                }
            }
        }

        // Check for i += 1
        if increment.len() == 3 {
            if let Token::Identifier(id) = &increment[0] {
                if id == var_name && increment[1] == Token::PlusEquals {
                    if let Token::Number(val) = &increment[2] {
                        if val == "1" {
                            return true;
                        }
                    }
                }
            }
        }

        false
    }

    /// Converts a while loop to appropriate Rust construct
    fn convert_while_loop(
        &self,
        condition: &[Token],
        body: &[Statement],
    ) -> Result<HandlerResult, ConversionError> {
        // Check if this is an infinite loop (while true or while 1)
        if self.is_infinite_loop(condition) {
            // Convert to Rust's loop construct
            let body_str = if !body.is_empty() {
                let mut block_str = String::from("{\n        ");
                for stmt in body {
                    block_str.push_str(&self.format_statement(stmt));
                    block_str.push_str(";\n        ");
                }
                block_str.push_str("}");
                block_str
            } else {
                String::from("{}")
            };

            return Ok(HandlerResult::RustCode(format!("loop {}", body_str)));
        }

        // Standard while loop conversion
        let condition_str = condition
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<String>>()
            .join("");

        let body_str = if !body.is_empty() {
            let mut block_str = String::from("{\n        ");
            for stmt in body {
                block_str.push_str(&self.format_statement(stmt));
                block_str.push_str(";\n        ");
            }
            block_str.push_str("}");
            block_str
        } else {
            String::from("{}")
        };

        Ok(HandlerResult::RustCode(format!(
            "while {} {}",
            condition_str, body_str
        )))
    }

    /// Converts a do-while loop (implemented as a loop with a condition check at the end)
    /// to Rust's loop with a conditional break at the end
    fn convert_do_while_loop(
        &self,
        body_tokens: &[Token],
        condition: &[Token],
    ) -> Result<HandlerResult, ConversionError> {
        let condition_str = condition
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<String>>()
            .join("");

        let body_str = body_tokens
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<String>>()
            .join("");

        let result = format!(
            "loop {{\n        {};\n        if !({}) {{ break; }}\n    }}",
            body_str, condition_str
        );

        Ok(HandlerResult::RustCode(result))
    }

    /// Format a statement for output in the generated Rust code
    fn format_statement(&self, stmt: &Statement) -> String {
        match stmt {
            Statement::Expression(tokens) => tokens
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<String>>()
                .join(""),
            Statement::If(condition, then_block, else_block) => {
                let cond_str = condition
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<String>>()
                    .join("");
                let then_str = if !then_block.is_empty() {
                    let mut block = String::from("{ ");
                    for s in then_block {
                        block.push_str(&self.format_statement(s));
                        block.push_str("; ");
                    }
                    block.push_str("}");
                    block
                } else {
                    "{}".to_string()
                };

                if !else_block.is_empty() {
                    let mut else_str = String::from(" else { ");
                    for s in else_block {
                        else_str.push_str(&self.format_statement(s));
                        else_str.push_str("; ");
                    }
                    else_str.push_str("}");
                    format!("if {} {} {}", cond_str, then_str, else_str)
                } else {
                    format!("if {} {}", cond_str, then_str)
                }
            }
            Statement::For(_init, _cond, _inc, _body) => {
                // Already handled by dedicated methods
                format!("/* for loop handled separately */")
            }
            Statement::While(_cond, _body) => {
                // Already handled by dedicated methods
                format!("/* while loop handled separately */")
            }
            Statement::Return(tokens) => {
                let ret_str = tokens
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<String>>()
                    .join("");
                format!("return {}", ret_str)
            }
            Statement::Declaration(tokens) => tokens
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<String>>()
                .join(""),
            Statement::Block(statements) => {
                let mut block = String::from("{ ");
                for s in statements {
                    block.push_str(&self.format_statement(s));
                    block.push_str("; ");
                }
                block.push_str("}");
                block
            }
            Statement::Define(_, _) => {
                // Macros handled by another handler
                format!("/* define handled separately */")
            }
            Statement::Empty => "".to_string(),
        }
    }

    /// Check if the given tokens represent a loop pattern we can handle
    fn can_process(&self, tokens: &[Token]) -> bool {
        // Look for for/while loop patterns
        for i in 0..tokens.len() {
            if i + 2 < tokens.len() {
                // Check for "for" keyword
                if let Token::Identifier(id) = &tokens[i] {
                    if id == "for" && tokens[i + 1] == Token::OpenParen {
                        return true;
                    }
                }

                // Check for "while" keyword
                if let Token::Identifier(id) = &tokens[i] {
                    if id == "while" && tokens[i + 1] == Token::OpenParen {
                        return true;
                    }
                }

                // Check for "do" keyword with "while" later
                if let Token::Identifier(id) = &tokens[i] {
                    if id == "do" {
                        // Look for while later
                        for j in (i + 1)..tokens.len() {
                            if let Token::Identifier(id2) = &tokens[j] {
                                if id2 == "while" {
                                    return true;
                                }
                            }
                        }
                    }
                }
            }
        }

        false
    }
}

impl TokenHandler for LoopHandler {
    fn can_handle(&self, tokens: &[Token], _context: &ParserContext) -> bool {
        self.can_process(tokens)
    }

    fn handle(
        &self,
        tokens: &[Token],
        _context: &mut ParserContext,
    ) -> Result<HandlerResult, ConversionError> {
        // This implementation needs more work to properly parse loop structures from tokens
        // Currently we have unit tests for the functionality, but we need to implement
        // proper parsing of for/while loops directly from token streams

        // A basic implementation to handle while loops
        if tokens.len() >= 3 {
            for i in 0..tokens.len() - 2 {
                if let Token::Identifier(id) = &tokens[i] {
                    if id == "while" && tokens[i + 1] == Token::OpenParen {
                        // Find closing paren and extract condition
                        let mut paren_level = 1;
                        let mut condition_end = i + 2;

                        while condition_end < tokens.len() && paren_level > 0 {
                            if tokens[condition_end] == Token::OpenParen {
                                paren_level += 1;
                            } else if tokens[condition_end] == Token::CloseParen {
                                paren_level -= 1;
                            }

                            if paren_level > 0 {
                                condition_end += 1;
                            }
                        }

                        if condition_end < tokens.len() {
                            let condition = &tokens[i + 2..condition_end];

                            // Check if this is an infinite loop
                            if condition.len() == 1 {
                                if let Token::Number(val) = &condition[0] {
                                    if val == "1" {
                                        return Ok(HandlerResult::RustCode("loop {".to_string()));
                                    }
                                } else if let Token::Identifier(id) = &condition[0] {
                                    if id == "true" {
                                        return Ok(HandlerResult::RustCode("loop {".to_string()));
                                    }
                                }
                            }

                            // Regular while loop
                            let condition_str = condition
                                .iter()
                                .map(|t| t.to_string())
                                .collect::<Vec<String>>()
                                .join("");

                            return Ok(HandlerResult::RustCode(format!(
                                "while {} {{",
                                condition_str
                            )));
                        }
                    }
                }
            }
        }

        // A basic implementation to handle for loops
        // This is just a placeholder, actual implementation would need to properly
        // parse the initialization, condition, and increment parts
        if tokens.len() >= 3 {
            for i in 0..tokens.len() - 2 {
                if let Token::Identifier(id) = &tokens[i] {
                    if id == "for" && tokens[i + 1] == Token::OpenParen {
                        // Very basic detection of standard for loop patterns
                        // In a real implementation, we'd parse the init, condition, and increment

                        // For now, just return a placeholder
                        return Ok(HandlerResult::RustCode("for i in 0..n {".to_string()));
                    }
                }
            }
        }

        Ok(HandlerResult::NotHandled)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_infinite_loop_conversion() {
        // Test with a simple token sequence for a while(1) loop
        let tokens = vec![
            Token::Identifier("while".to_string()),
            Token::OpenParen,
            Token::Number("1".to_string()),
            Token::CloseParen,
        ];

        let handler = LoopHandler::new();
        let mut context = ParserContext::new();
        let result = handler.handle(&tokens, &mut context).unwrap();

        match result {
            HandlerResult::RustCode(rust_code) => {
                assert_eq!(rust_code, "loop {");
            }
            _ => panic!("Expected RustCode result for infinite loop"),
        }
    }

    #[test]
    fn test_is_infinite_loop() {
        let handler = LoopHandler::new();

        // Test with simple "1"
        let condition1 = vec![Token::Number("1".to_string())];
        assert!(handler.is_infinite_loop(&condition1));

        // Test with "true"
        let condition2 = vec![Token::Identifier("true".to_string())];
        assert!(handler.is_infinite_loop(&condition2));

        // Test with "(1)"
        let condition3 = vec![
            Token::OpenParen,
            Token::Number("1".to_string()),
            Token::CloseParen,
        ];
        assert!(handler.is_infinite_loop(&condition3));

        // Test with "0" (not infinite)
        let condition4 = vec![Token::Number("0".to_string())];
        assert!(!handler.is_infinite_loop(&condition4));
    }
}
