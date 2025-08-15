//! A token-based parser for C function bodies.

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // Keywords
    If,
    Else,
    For,
    While,
    Return,
    Sizeof,

    // Identifiers and Literals
    Identifier(String),
    StringLiteral(String),
    Number(String),

    // Symbols and Operators
    OpenParen,    // (
    CloseParen,   // )
    OpenBrace,    // {
    CloseBrace,   // }
    OpenBracket,  // [
    CloseBracket, // ]
    Semicolon,    // ;
    Comma,        // ,
    Asterisk,     // *
    Equals,       // =
    EqualsEquals, // ==
    Plus,         // +
    Minus,        // -
    Slash,        // /
    Arrow,        // ->
    Dot,          // .
    Ampersand,    // &
    Pipe,         // |
    LogicalAnd,   // &&
    LogicalOr,    // ||
    LessThan,     // <
    GreaterThan,  // >
}

pub struct Tokenizer {
    content: Vec<char>,
    cursor: usize,
}

impl Tokenizer {
    pub fn new(content: &str) -> Self {
        Tokenizer {
            content: content.chars().collect::<Vec<char>>(),
            cursor: 0,
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        while let Some(token) = self.next_token() {
            tokens.push(token);
        }
        tokens
    }

    fn next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();
        if self.cursor >= self.content.len() {
            return None;
        }

        let c = self.content[self.cursor];

        if c.is_alphabetic() || c == '_' {
            let start = self.cursor;
            while self.cursor < self.content.len() && (self.content[self.cursor].is_alphanumeric() || self.content[self.cursor] == '_') {
                self.cursor += 1;
            }
            let identifier: String = self.content[start..self.cursor].iter().collect();
            return Some(match identifier.as_str() {
                "if" => Token::If,
                "else" => Token::Else,
                "for" => Token::For,
                "while" => Token::While,
                "return" => Token::Return,
                "sizeof" => Token::Sizeof,
                _ => Token::Identifier(identifier),
            });
        }

        if c.is_ascii_digit() {
            let start = self.cursor;
            while self.cursor < self.content.len() && (self.content[self.cursor].is_ascii_digit() || self.content[self.cursor] == '.') {
                self.cursor += 1;
            }
            let num_str: String = self.content[start..self.cursor].iter().collect();
            return Some(Token::Number(num_str));
        }

        match c {
            '(' => { self.cursor += 1; Some(Token::OpenParen) }
            ')' => { self.cursor += 1; Some(Token::CloseParen) }
            '{' => { self.cursor += 1; Some(Token::OpenBrace) }
            '}' => { self.cursor += 1; Some(Token::CloseBrace) }
            '[' => { self.cursor += 1; Some(Token::OpenBracket) }
            ']' => { self.cursor += 1; Some(Token::CloseBracket) }
            ';' => { self.cursor += 1; Some(Token::Semicolon) }
            ',' => { self.cursor += 1; Some(Token::Comma) }
            '*' => { self.cursor += 1; Some(Token::Asterisk) }
            '=' => {
                if self.peek() == Some('=') {
                    self.cursor += 2;
                    Some(Token::EqualsEquals)
                } else {
                    self.cursor += 1;
                    Some(Token::Equals)
                }
            }
            '+' => { self.cursor += 1; Some(Token::Plus) }
            '/' => { self.cursor += 1; Some(Token::Slash) }
            '.' => { self.cursor += 1; Some(Token::Dot) }
            '&' => {
                if self.peek() == Some('&') {
                    self.cursor += 2;
                    Some(Token::LogicalAnd)
                } else {
                    self.cursor += 1;
                    Some(Token::Ampersand)
                }
            }
            '|' => {
                if self.peek() == Some('|') {
                    self.cursor += 2;
                    Some(Token::LogicalOr)
                } else {
                    self.cursor += 1;
                    Some(Token::Pipe)
                }
            }
            '<' => { self.cursor += 1; Some(Token::LessThan) }
            '>' => { self.cursor += 1; Some(Token::GreaterThan) }
            '-' => {
                if self.peek() == Some('>') {
                    self.cursor += 2;
                    Some(Token::Arrow)
                } else {
                    self.cursor += 1;
                    Some(Token::Minus)
                }
            }
            '"' => self.read_string_literal(),
            _ => {
                self.cursor += 1;
                self.next_token()
            }
        }
    }

    fn read_string_literal(&mut self) -> Option<Token> {
        self.cursor += 1; // Skip opening quote
        let mut literal = String::new();
        let mut escaped = false;

        while self.cursor < self.content.len() {
            let c = self.content[self.cursor];

            if escaped {
                literal.push('\\');
                literal.push(c);
                escaped = false;
                self.cursor += 1;
            } else if c == '\\' {
                escaped = true;
                self.cursor += 1;
            } else if c == '"' {
                self.cursor += 1; // Skip closing quote
                return Some(Token::StringLiteral(literal));
            } else {
                literal.push(c);
                self.cursor += 1;
            }
        }
        // Unterminated string literal
        Some(Token::StringLiteral(literal))
    }

    fn skip_whitespace(&mut self) {
        while self.cursor < self.content.len() && self.content[self.cursor].is_whitespace() {
            self.cursor += 1;
        }
    }

    fn peek(&self) -> Option<char> {
        self.content.get(self.cursor + 1).copied()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Expression(Vec<Token>),
    If(Vec<Token>, Vec<Statement>, Vec<Statement>),
    For(Vec<Token>, Vec<Token>, Vec<Token>, Vec<Statement>),
    While(Vec<Token>, Vec<Statement>),
    Return(Vec<Token>),
    Declaration(Vec<Token>),
    Block(Vec<Statement>),
    Empty,
}

pub struct StatementParser {
    tokens: Vec<Token>,
    position: usize,
}

impl StatementParser {
    pub fn new(tokens: Vec<Token>) -> Self {
        StatementParser { tokens, position: 0 }
    }

    pub fn parse(&mut self) -> Vec<Statement> {
        let mut statements = Vec::new();
        while self.position < self.tokens.len() {
            if let Some(stmt) = self.parse_statement() {
                statements.push(stmt);
            }
        }
        statements
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        if self.position >= self.tokens.len() {
            return None;
        }

        let token = &self.tokens[self.position];

        match token {
            Token::If => self.parse_if_statement(),
            Token::For => self.parse_for_statement(),
            Token::While => self.parse_while_statement(),
            Token::Return => self.parse_return_statement(),
            Token::OpenBrace => Some(self.parse_block()),
            Token::Identifier(_) => {
                // Look ahead to see if it's a declaration or an expression
                if self.is_declaration() {
                    self.parse_declaration()
                } else {
                    self.parse_expression_statement()
                }
            }
            Token::Semicolon => {
                self.position += 1;
                Some(Statement::Empty)
            }
            _ => self.parse_expression_statement(),
        }
    }

    fn is_declaration(&self) -> bool {
        let mut temp_pos = self.position;
        if temp_pos >= self.tokens.len() {
            return false;
        }

        // 1. Check for type keywords
        let mut type_found = false;
        while temp_pos < self.tokens.len() {
            if let Token::Identifier(id) = &self.tokens[temp_pos] {
                if is_type_keyword(id) {
                    type_found = true;
                    temp_pos += 1;
                    continue;
                }
            }
            break;
        }

        if !type_found {
            return false;
        }

        // 2. Skip over any pointer asterisks
        while temp_pos < self.tokens.len() && self.tokens[temp_pos] == Token::Asterisk {
            temp_pos += 1;
        }

        // 3. The next token must be an identifier (the variable name).
        if temp_pos >= self.tokens.len() {
            return false;
        }

        if let Token::Identifier(_) = &self.tokens[temp_pos] {
            temp_pos += 1; // Consume the identifier
        } else {
            return false; // Not a declaration if there's no variable name
        }

        // 4. After the name, we expect a semicolon, equals, or open bracket.
        if temp_pos >= self.tokens.len() {
            return true; // End of tokens, could be a valid declaration fragment
        }

        matches!(
            self.tokens[temp_pos],
            Token::Semicolon | Token::Equals | Token::EqualsEquals | Token::OpenBracket
        )
    }

    fn parse_tokens_until(&mut self, end_token: Token) -> Vec<Token> {
        let start_pos = self.position;
        while self.position < self.tokens.len() && self.tokens[self.position] != end_token {
            self.position += 1;
        }
        let end_pos = self.position;
        let statement_tokens = self.tokens[start_pos..end_pos].to_vec();
        if self.position < self.tokens.len() {
            self.position += 1; // consume end token
        }
        statement_tokens
    }

    fn parse_declaration(&mut self) -> Option<Statement> {
        Some(Statement::Declaration(self.parse_tokens_until(Token::Semicolon)))
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        Some(Statement::Expression(self.parse_tokens_until(Token::Semicolon)))
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        self.position += 1; // consume 'return'
        Some(Statement::Return(self.parse_tokens_until(Token::Semicolon)))
    }

    fn parse_if_statement(&mut self) -> Option<Statement> {
        self.position += 1; // consume 'if'

        if self.tokens.get(self.position) != Some(&Token::OpenParen) {
            return Some(Statement::Expression(vec![Token::Identifier("// malformed if: missing '('".to_string())]));
        }
        self.position += 1; // consume '('

        let condition_end = self.find_matching_paren(self.position);
        if condition_end.is_none() {
            return Some(Statement::Expression(vec![Token::Identifier("// malformed if: missing ')'".to_string())]));
        }
        let condition_end = condition_end.unwrap();

        let condition = self.tokens[self.position..condition_end].to_vec();
        self.position = condition_end + 1;

        let if_body = self.parse_statement();

        let mut else_body = None;
        if self.tokens.get(self.position) == Some(&Token::Else) {
            self.position += 1; // consume 'else'
            else_body = self.parse_statement();
        }

        Some(Statement::If(condition, vec![if_body.unwrap_or(Statement::Empty)], if let Some(body) = else_body { vec![body] } else { vec![] }))
    }

    fn find_matching_paren(&self, start_pos: usize) -> Option<usize> {
        let mut balance = 1;
        for i in start_pos..self.tokens.len() {
            match self.tokens[i] {
                Token::OpenParen => balance += 1,
                Token::CloseParen => {
                    balance -= 1;
                    if balance == 0 {
                        return Some(i);
                    }
                }
                _ => {}
            }
        }
        None
    }

    fn parse_for_statement(&mut self) -> Option<Statement> {
        // Placeholder implementation
        self.position += 1; // consume 'for'
        Some(Statement::Expression(vec![Token::Identifier("// for loop not fully parsed".to_string())]))
    }

    fn parse_while_statement(&mut self) -> Option<Statement> {
        // Placeholder implementation
        self.position += 1; // consume 'while'
        Some(Statement::Expression(vec![Token::Identifier("// while loop not fully parsed".to_string())]))
    }

    fn parse_block(&mut self) -> Statement {
        let mut statements = Vec::new();
        self.position += 1; // Consume '{'

        while self.position < self.tokens.len() && self.tokens[self.position] != Token::CloseBrace {
            if let Some(stmt) = self.parse_statement() {
                statements.push(stmt);
            }
        }

        if self.position < self.tokens.len() && self.tokens[self.position] == Token::CloseBrace {
            self.position += 1; // Consume '}'
        }
        Statement::Block(statements)
    }
}

fn is_type_keyword(s: &str) -> bool {
    matches!(
        s,
        "const"
            | "unsigned"
            | "signed"
            | "char"
            | "short"
            | "int"
            | "long"
            | "float"
            | "double"
            | "void"
            | "struct"
            | "union"
            | "enum"
            | "FILE"
            // Add other C type keywords as needed
    )
}
