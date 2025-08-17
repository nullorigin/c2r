//! A token-based parser for C function bodies.

use crate::error::ConversionError;
use std::fmt;
use std::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Not};
use std::str::FromStr;

#[derive(Debug, PartialEq, Eq, Clone, Hash, PartialOrd, Ord)]
pub enum Token {
    // Keywords
    If,
    Else,
    For,
    While,
    Do,
    Return,
    Sizeof,
    Break,
    Continue,
    Define,
    // Identifiers and Literals
    Identifier(String),
    StringLiteral(String),
    Number(String),

    // Symbols and Operators
    OpenParen,      // (
    CloseParen,     // )
    OpenBrace,      // {
    CloseBrace,     // }
    OpenBracket,    // [
    CloseBracket,   // ]
    Semicolon,      // ;
    Comma,          // ,
    Dot,            // .
    Hash,           // #
    Plus,           // +
    Minus,          // -
    Asterisk,       // *
    Slash,          // /
    Percent,        // %
    Equals,         // =
    Colon,          // :
    QuestionMark,   // ?
    Ampersand,      // &
    Pipe,           // |
    Caret,          // ^
    Tilde,          // ~
    Exclamation,    // !
    Arrow,          // ->
    DoublePlus,     // ++
    DoubleMinus,    // --
    DoubleEqual,    // ==
    NotEqual,       // !=
    NotEquals,      // !=
    LessEqual,      // <=
    GreaterEqual,   // >=
    LessThan,       // <
    GreaterThan,    // >
    LogicalAnd,     // &&
    LogicalOr,      // ||
    ShiftLeft,      // <<
    ShiftRight,     // >>
    PlusEquals,     // +=
    MinusEquals,    // -=
    StarEquals,     // *=
    SlashEquals,    // /=
    AmpersandEqual, // &=
    PipeEqual,      // |=
    CaretEqual,     // ^=
    Backslash,      // \
    DoubleHash,     // ##
    ShlEquals,      // <<=
    ShrEquals,      // >>=
    Underscore,     // _
    Keyword(Keyword),
    Ellipsis,           // ...
    AngleBracketLeft,   // <
    AngleBracketRight,  // >
    Increment,          // ++
    Decrement,          // --
    Equal,              // =
    LessThanOrEqual,    // <=
    GreaterThanOrEqual, // >=
    BitwiseNot,
    BitwiseXor,
    BitwiseOr,
    BitwiseAnd,
    Mod,
    Divide,
    Multiply,
    CharLiteral(char),
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Keyword::Auto => "auto",
                Keyword::Break => "break",
                Keyword::Case => "case",
                Keyword::Char => "char",
                Keyword::Const => "const",
                Keyword::Continue => "continue",
                Keyword::Default => "default",
                Keyword::Do => "do",
                Keyword::Double => "double",
                Keyword::Else => "else",
                Keyword::Enum => "enum",
                Keyword::Extern => "extern",
                Keyword::Float => "float",
                Keyword::For => "for",
                Keyword::Goto => "goto",
                Keyword::If => "if",
                Keyword::Int => "int",
                Keyword::Long => "long",
                Keyword::Register => "register",
                Keyword::Return => "return",
                Keyword::Short => "short",
                Keyword::Signed => "signed",
                Keyword::Sizeof => "sizeof",
                Keyword::Static => "static",
                Keyword::Struct => "struct",
                Keyword::Switch => "switch",
                Keyword::Typedef => "typedef",
                Keyword::Union => "union",
                Keyword::Unsigned => "unsigned",
                Keyword::Void => "void",
                Keyword::Volatile => "volatile",
                Keyword::While => "while",
                _ => "", // Handle other keywords if necessary
            }
        )
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::For => write!(f, "for"),
            Token::While => write!(f, "while"),
            Token::Do => write!(f, "do"),
            Token::Return => write!(f, "return"),
            Token::Sizeof => write!(f, "sizeof"),
            Token::Break => write!(f, "break"),
            Token::Continue => write!(f, "continue"),
            Token::Define => write!(f, "#define"),
            Token::Keyword(k) => write!(f, "{}", k),
            Token::Identifier(s) => write!(f, "{}", s),
            Token::StringLiteral(s) => write!(f, "\"{}\"", s),
            Token::Number(n) => write!(f, "{}", n),
            Token::OpenParen => write!(f, "("),
            Token::CloseParen => write!(f, ")"),
            Token::OpenBrace => write!(f, "{{"),
            Token::CloseBrace => write!(f, "}}"),
            Token::OpenBracket => write!(f, "["),
            Token::CloseBracket => write!(f, "]"),
            Token::Semicolon => write!(f, ";"),
            Token::Comma => write!(f, ","),
            Token::Asterisk => write!(f, "*"),
            Token::DoubleEqual => write!(f, "=="),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Slash => write!(f, "/"),
            Token::Arrow => write!(f, "->"),
            Token::Dot => write!(f, "."),
            Token::Ampersand => write!(f, "&"),
            Token::Pipe => write!(f, "|"),
            Token::LogicalAnd => write!(f, "&&"),
            Token::LogicalOr => write!(f, "||"),
            Token::LessThan => write!(f, "<"),
            Token::GreaterThan => write!(f, ">"),
            Token::Equal => write!(f, "="),
            Token::Exclamation => write!(f, "!"),
            Token::Caret => write!(f, "^"),
            Token::Tilde => write!(f, "~"),
            Token::PlusEquals => write!(f, "+="),
            Token::MinusEquals => write!(f, "-="),
            Token::StarEquals => write!(f, "*="),
            Token::SlashEquals => write!(f, "/="),
            Token::AmpersandEqual => write!(f, "&="),
            Token::PipeEqual => write!(f, "|="),
            Token::CaretEqual => write!(f, "^="),
            Token::NotEqual => write!(f, "!="),
            Token::LessEqual => write!(f, "<="),
            Token::GreaterEqual => write!(f, ">="),
            Token::Increment => write!(f, "++"),
            Token::Decrement => write!(f, "--"),
            Token::Colon => write!(f, ":"),
            Token::Hash => write!(f, "#"),
            Token::Percent => write!(f, "%"),
            Token::Ellipsis => write!(f, "..."),
            Token::DoublePlus => write!(f, "++"),
            Token::DoubleMinus => write!(f, "--"),
            Token::QuestionMark => write!(f, "?"),
            Token::Backslash => write!(f, "\\"),
            Token::DoubleHash => write!(f, "##"),
            Token::Underscore => write!(f, "_"),
            Token::AngleBracketLeft => write!(f, "<"),
            Token::AngleBracketRight => write!(f, ">"),
            _ => write!(f, "{:?}", self),
        }
    }
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
            while self.cursor < self.content.len()
                && (self.content[self.cursor].is_alphanumeric() || self.content[self.cursor] == '_')
            {
                self.cursor += 1;
            }
            let identifier: String = self.content[start..self.cursor].iter().collect();
            return Some(match identifier.as_str() {
                "if" => Token::If,
                "else" => Token::Else,
                "for" => Token::For,
                "while" => Token::While,
                "do" => Token::Do,
                "return" => Token::Return,
                "sizeof" => Token::Sizeof,
                "break" => Token::Break,
                "continue" => Token::Continue,
                "#define" => Token::Define,
                _ => Token::Identifier(identifier),
            });
        }

        if c.is_ascii_digit() {
            let start = self.cursor;
            while self.cursor < self.content.len()
                && (self.content[self.cursor].is_ascii_digit() || self.content[self.cursor] == '.')
            {
                self.cursor += 1;
            }
            let num_str: String = self.content[start..self.cursor].iter().collect();
            return Some(Token::Number(num_str));
        }

        match c {
            '(' => {
                self.cursor += 1;
                Some(Token::OpenParen)
            }
            ')' => {
                self.cursor += 1;
                Some(Token::CloseParen)
            }
            '{' => {
                self.cursor += 1;
                Some(Token::OpenBrace)
            }
            '}' => {
                self.cursor += 1;
                Some(Token::CloseBrace)
            }
            '[' => {
                self.cursor += 1;
                Some(Token::OpenBracket)
            }
            ']' => {
                self.cursor += 1;
                Some(Token::CloseBracket)
            }
            ';' => {
                self.cursor += 1;
                Some(Token::Semicolon)
            }
            ',' => {
                self.cursor += 1;
                Some(Token::Comma)
            }
            '+' => {
                if self.peek() == Some('=') {
                    self.cursor += 2;
                    Some(Token::PlusEquals)
                } else if self.peek() == Some('+') {
                    self.cursor += 2;
                    Some(Token::Increment)
                } else {
                    self.cursor += 1;
                    Some(Token::Plus)
                }
            }
            '-' => {
                if self.peek() == Some('=') {
                    self.cursor += 2;
                    Some(Token::MinusEquals)
                } else if self.peek() == Some('-') {
                    self.cursor += 2;
                    Some(Token::Decrement)
                } else if self.peek() == Some('>') {
                    self.cursor += 2;
                    Some(Token::Arrow)
                } else {
                    self.cursor += 1;
                    Some(Token::Minus)
                }
            }
            '/' => {
                if self.peek() == Some('=') {
                    self.cursor += 2;
                    Some(Token::SlashEquals)
                } else {
                    self.cursor += 1;
                    Some(Token::Slash)
                }
            }
            '%' => {
                self.cursor += 1;
                Some(Token::Percent)
            }
            '*' => {
                if self.peek() == Some('=') {
                    self.cursor += 2;
                    Some(Token::StarEquals)
                } else {
                    self.cursor += 1;
                    Some(Token::Asterisk)
                }
            }
            '&' => {
                if self.peek() == Some('&') {
                    self.cursor += 2;
                    Some(Token::LogicalAnd)
                } else if self.peek() == Some('=') {
                    self.cursor += 2;
                    Some(Token::AmpersandEqual)
                } else {
                    self.cursor += 1;
                    Some(Token::Ampersand)
                }
            }
            '|' => {
                if self.peek() == Some('|') {
                    self.cursor += 2;
                    Some(Token::LogicalOr)
                } else if self.peek() == Some('=') {
                    self.cursor += 2;
                    Some(Token::PipeEqual)
                } else {
                    self.cursor += 1;
                    Some(Token::Pipe)
                }
            }
            '<' => {
                if self.peek() == Some('<') {
                    if self.peek_at(2) == Some('=') {
                        self.cursor += 3;
                        Some(Token::ShlEquals)
                    } else {
                        self.cursor += 2;
                        Some(Token::ShiftLeft)
                    }
                } else if self.peek() == Some('=') {
                    self.cursor += 2;
                    Some(Token::LessThanOrEqual)
                } else {
                    self.cursor += 1;
                    Some(Token::LessThan)
                }
            }
            '>' => {
                if self.peek() == Some('>') {
                    if self.peek_at(2) == Some('=') {
                        self.cursor += 3;
                        Some(Token::ShrEquals)
                    } else {
                        self.cursor += 2;
                        Some(Token::ShiftRight)
                    }
                } else if self.peek() == Some('=') {
                    self.cursor += 2;
                    Some(Token::GreaterThanOrEqual)
                } else {
                    self.cursor += 1;
                    Some(Token::GreaterThan)
                }
            }
            '!' => {
                if self.peek() == Some('=') {
                    self.cursor += 2;
                    Some(Token::NotEquals)
                } else {
                    self.cursor += 1;
                    Some(Token::Exclamation)
                }
            }
            '^' => {
                if self.peek() == Some('=') {
                    self.cursor += 2;
                    Some(Token::CaretEqual)
                } else {
                    self.cursor += 1;
                    Some(Token::Caret)
                }
            }
            '~' => {
                self.cursor += 1;
                Some(Token::Tilde)
            }
            '?' => {
                self.cursor += 1;
                Some(Token::QuestionMark)
            }
            ':' => {
                self.cursor += 1;
                Some(Token::Colon)
            }
            '#' => {
                if self.peek() == Some('#') {
                    self.cursor += 2;
                    Some(Token::DoubleHash)
                } else {
                    self.cursor += 1;
                    Some(Token::Hash)
                }
            }
            '"' => self.read_string_literal(),
            '.' => {
                if self.peek() == Some('.') && self.peek_at(2) == Some('.') {
                    self.cursor += 3;
                    Some(Token::Ellipsis)
                } else {
                    self.cursor += 1;
                    Some(Token::Dot)
                }
            }
            '\\' => {
                self.cursor += 1;
                Some(Token::Backslash)
            }
            '_' => {
                self.cursor += 1;
                Some(Token::Underscore)
            }
            _ => None,
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
        if self.cursor < self.content.len() - 1 {
            Some(self.content[self.cursor + 1])
        } else {
            None
        }
    }

    fn peek_at(&self, offset: usize) -> Option<char> {
        if self.cursor + offset < self.content.len() {
            Some(self.content[self.cursor + offset])
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Statement {
    Expression(Vec<Token>),
    If(Vec<Token>, Vec<Statement>, Vec<Statement>),
    For(Vec<Token>, Vec<Token>, Vec<Token>, Vec<Statement>),
    While(Vec<Token>, Vec<Statement>),
    Return(Vec<Token>),
    Declaration(Vec<Token>),
    Block(Vec<Statement>),
    Define(Vec<Token>, Vec<Statement>),
    Empty,
}

pub struct StatementParser {
    tokens: Vec<Token>,
    position: usize,
}

impl StatementParser {
    pub fn new(tokens: Vec<Token>) -> Self {
        StatementParser {
            tokens,
            position: 0,
        }
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

    pub fn parse_statements(&mut self) -> Vec<Statement> {
        // This is a convenience wrapper around parse method
        self.parse()
    }

    pub(crate) fn parse_statement(&mut self) -> Option<Statement> {
        if self.position >= self.tokens.len() {
            return None;
        }

        let token = &self.tokens[self.position];
        println!(
            "Debug: Starting statement parse at position {} with token: {:?}",
            self.position, token
        );

        match token {
            Token::If => self.parse_if_statement(),
            Token::For => self.parse_for_statement(),
            Token::While => self.parse_while_statement(),
            Token::Return => self.parse_return_statement(),
            Token::OpenBrace => Some(self.parse_block()),
            Token::Identifier(id) => {
                // Enhanced identifier handling for declarations
                if id == "FILE"
                    || id == "int"
                    || id == "char"
                    || id == "float"
                    || id == "double"
                    || id == "long"
                    || id == "short"
                    || id == "struct"
                    || id == "enum"
                    || id == "union"
                    || id == "void"
                    || id == "unsigned"
                    || id == "signed"
                    || id == "const"
                    || id == "static"
                {
                    println!("Debug: Found likely type identifier: {}", id);
                    // Look for pointer declarations (TYPE * VAR)
                    let mut lookahead = self.position + 1;
                    let mut is_pointer_decl = false;

                    // Check for asterisk after type
                    if lookahead < self.tokens.len() && self.tokens[lookahead] == Token::Asterisk {
                        is_pointer_decl = true;
                        // Check for variable name after asterisk
                        lookahead += 1;
                        if lookahead < self.tokens.len()
                            && matches!(self.tokens[lookahead], Token::Identifier(_))
                        {
                            println!("Debug: Found pointer declaration pattern: TYPE * VAR");
                            return self.parse_declaration();
                        }
                    }

                    // Regular declaration check
                    if !is_pointer_decl && self.is_declaration() {
                        println!("Debug: is_declaration returned true, parsing declaration");
                        return self.parse_declaration();
                    }

                    // Fall back to expression
                    println!("Debug: Falling back to expression for identifier {}", id);
                    return self.parse_expression_statement();
                }

                // Otherwise normal identifier handling
                // Look ahead to see if it's a declaration or an expression
                if self.is_declaration() {
                    println!("Debug: Regular is_declaration returned true");
                    self.parse_declaration()
                } else {
                    println!("Debug: Regular is_declaration returned false, parsing expression");
                    self.parse_expression_statement()
                }
            }
            Token::Semicolon => {
                self.position += 1;
                Some(Statement::Empty)
            }
            Token::Define => self.parse_define_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn is_declaration(&self) -> bool {
        let mut temp_pos = self.position;
        if temp_pos >= self.tokens.len() {
            return false;
        }

        // Check if declaration starts with struct, union or enum keyword
        if temp_pos < self.tokens.len() {
            if let Token::Identifier(id) = &self.tokens[temp_pos] {
                // Check for struct/union/enum identifiers (not keywords)
                if id == "struct" || id == "union" || id == "enum" {
                    // Skip the struct/union/enum identifier
                    temp_pos += 1;

                    // Skip the struct name identifier if present
                    if temp_pos < self.tokens.len() {
                        if let Token::Identifier(_) = &self.tokens[temp_pos] {
                            temp_pos += 1;

                            // Check for type specifier with colon (enum Color : uint8_t)
                            if temp_pos < self.tokens.len() && self.tokens[temp_pos] == Token::Colon
                            {
                                temp_pos += 1; // Skip the colon

                                // Skip the type specifier
                                while temp_pos < self.tokens.len() {
                                    if let Token::Identifier(_) = &self.tokens[temp_pos] {
                                        temp_pos += 1;
                                    } else {
                                        break;
                                    }
                                }
                            }
                        }
                    }

                    // Check for an opening brace (indicating a declaration with body)
                    if temp_pos < self.tokens.len() && self.tokens[temp_pos] == Token::OpenBrace {
                        return true; // Found a struct/union/enum declaration with a body
                    }

                    // The next token should be an identifier (variable name)
                    if temp_pos < self.tokens.len() {
                        if let Token::Identifier(_) = &self.tokens[temp_pos] {
                            return true; // Found a struct/union/enum declaration
                        }
                    }
                }
            } else if let Token::Keyword(kw) = &self.tokens[temp_pos] {
                if matches!(kw, Keyword::Struct | Keyword::Union | Keyword::Enum) {
                    // Skip the struct/union/enum keyword
                    temp_pos += 1;

                    // Skip the struct name identifier if present
                    if temp_pos < self.tokens.len() {
                        if let Token::Identifier(_) = &self.tokens[temp_pos] {
                            temp_pos += 1;

                            // Check for type specifier with colon (enum Color : uint8_t)
                            if temp_pos < self.tokens.len() && self.tokens[temp_pos] == Token::Colon
                            {
                                temp_pos += 1; // Skip the colon

                                // Skip the type specifier
                                while temp_pos < self.tokens.len() {
                                    if let Token::Identifier(_) = &self.tokens[temp_pos] {
                                        temp_pos += 1;
                                    } else {
                                        break;
                                    }
                                }
                            }
                        }
                    }

                    // Check for an opening brace (indicating a declaration with body)
                    if temp_pos < self.tokens.len() && self.tokens[temp_pos] == Token::OpenBrace {
                        return true; // Found a struct/union/enum declaration with a body
                    }

                    // The next token should be an identifier (variable name)
                    if temp_pos < self.tokens.len() {
                        if let Token::Identifier(_) = &self.tokens[temp_pos] {
                            return true; // Found a struct/union/enum declaration
                        }
                    }
                }
            }
        }

        // Handle standard type declarations including pointer declarations
        if temp_pos < self.tokens.len() {
            // A declaration must start with a type keyword or known type.
            if let Token::Identifier(id) = &self.tokens[temp_pos] {
                // Check if it's a known type
                if !is_type_keyword(id) && !is_known_type(id) {
                    return false;
                }
            } else {
                return false; // Does not start with an identifier, so can't be a type.
            }

            // Skip the type part.
            temp_pos += 1; // Move past the type

            // Skip pointer asterisks
            let mut has_asterisk = false;
            while temp_pos < self.tokens.len() && self.tokens[temp_pos] == Token::Asterisk {
                has_asterisk = true;
                temp_pos += 1;
            }

            // The next token must be an identifier (the variable name).
            if temp_pos < self.tokens.len() {
                if let Token::Identifier(_) = &self.tokens[temp_pos] {
                    return true; // Found a type followed by an identifier, this is a declaration.
                }
            }

            // Handle the case where there's no space between asterisk and identifier
            // Example: int *x; (where * and x are parsed as separate tokens)
            if has_asterisk && temp_pos > 0 && temp_pos - 1 < self.tokens.len() {
                if let Token::Identifier(_) = &self.tokens[temp_pos - 1] {
                    return true;
                }
            }
        }

        false
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
        Some(Statement::Declaration(
            self.parse_tokens_until(Token::Semicolon),
        ))
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        Some(Statement::Expression(
            self.parse_tokens_until(Token::Semicolon),
        ))
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        self.position += 1; // consume 'return'
        Some(Statement::Return(self.parse_tokens_until(Token::Semicolon)))
    }

    fn parse_if_statement(&mut self) -> Option<Statement> {
        self.position += 1; // consume 'if'

        if self.tokens.get(self.position) != Some(&Token::OpenParen) {
            return Some(Statement::Expression(vec![Token::Identifier(
                "// malformed if: missing '('".to_string(),
            )]));
        }
        self.position += 1; // consume '('

        let condition_end = self.find_matching_paren(self.position);
        if condition_end.is_none() {
            return Some(Statement::Expression(vec![Token::Identifier(
                "// malformed if: missing ')'".to_string(),
            )]));
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

        Some(Statement::If(
            condition,
            vec![if_body.unwrap_or(Statement::Empty)],
            if let Some(body) = else_body {
                vec![body]
            } else {
                vec![]
            },
        ))
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
        self.position += 1; // consume 'for'

        if self.position >= self.tokens.len() || self.tokens[self.position] != Token::OpenParen {
            return Some(Statement::Expression(vec![Token::Identifier(
                "// malformed for: missing '('".to_string(),
            )]));
        }
        self.position += 1; // consume '('

        // Parse initialization part
        let init_tokens = self.parse_tokens_until(Token::Semicolon);

        // Parse condition part
        let condition_tokens = self.parse_tokens_until(Token::Semicolon);

        // Parse increment part
        let _params = self.parse_tokens_until(Token::CloseParen);

        // Parse body
        let body = self.parse_statement();

        Some(Statement::For(
            init_tokens,
            condition_tokens,
            vec![],
            vec![body.unwrap_or(Statement::Empty)],
        ))
    }

    fn parse_while_statement(&mut self) -> Option<Statement> {
        self.position += 1; // consume 'while'

        if self.position >= self.tokens.len() || self.tokens[self.position] != Token::OpenParen {
            return Some(Statement::Expression(vec![Token::Identifier(
                "// malformed while: missing '('".to_string(),
            )]));
        }
        self.position += 1; // consume '('

        let condition_end = self.find_matching_paren(self.position);
        if condition_end.is_none() {
            return Some(Statement::Expression(vec![Token::Identifier(
                "// malformed while: missing ')'".to_string(),
            )]));
        }
        let condition_end = condition_end.unwrap();

        let condition = self.tokens[self.position..condition_end].to_vec();
        self.position = condition_end + 1;

        // Parse body
        let body = self.parse_statement();

        Some(Statement::While(
            condition,
            vec![body.unwrap_or(Statement::Empty)],
        ))
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
    fn parse_define_statement(&mut self) -> Option<Statement> {
        let start_pos = self.position;
        self.position += 1; // Consume identifier (function name)

        if self.position < self.tokens.len() && self.tokens[self.position] == Token::OpenParen {
            // This is likely a function definition
            let name_tokens = vec![self.tokens[start_pos].clone()];
            let _params = self.parse_tokens_until(Token::CloseParen);

            if self.position < self.tokens.len() && self.tokens[self.position] == Token::OpenBrace {
                let body = match self.parse_block() {
                    Statement::Block(statements) => statements,
                    _ => Vec::new(),
                };
                return Some(Statement::Define(name_tokens, body));
            }
        }

        None
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
            | "FILE"
            | "typedef"
            | "static"
            | "extern"
            | "inline"
            | "register"
            | "volatile"
            | "break"
            | "case"
            | "continue"
            | "default"
            | "do"
            | "else"
            | "for"
            | "goto"
            | "if"
            | "return"
            | "sizeof"
            | "switch"
            | "while"
            | "asm" // Add other C type keywords as needed
    )
}

fn is_known_type(id: &str) -> bool {
    matches!(
        id,
        "FILE"
            | "DIR"
            | "va_list"
            | "size_t"
            | "ssize_t"
            | "time_t"
            | "clock_t"
            | "wchar_t"
            | "off_t"
            | "pthread_t"
            | "socklen_t"
            | "intptr_t"
            | "uintptr_t"
    )
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(u128)]
pub enum Keyword {
    None = 0,
    Auto = 1 << 0,
    Break = 1 << 1,
    Case = 1 << 2,
    Char = 1 << 3,
    Const = 1 << 4,
    Continue = 1 << 5,
    Default = 1 << 6,
    Do = 1 << 7,
    Double = 1 << 8,
    Else = 1 << 9,
    Enum = 1 << 10,
    Extern = 1 << 11,
    Float = 1 << 12,
    For = 1 << 13,
    Goto = 1 << 14,
    If = 1 << 15,
    Int = 1 << 16,
    Long = 1 << 17,
    Register = 1 << 18,
    Return = 1 << 19,
    Short = 1 << 20,
    Signed = 1 << 21,
    Sizeof = 1 << 22,
    Static = 1 << 23,
    Struct = 1 << 24,
    Switch = 1 << 25,
    Typedef = 1 << 26,
    Union = 1 << 27,
    Unsigned = 1 << 28,
    Void = 1 << 29,
    Volatile = 1 << 30,
    While = 1 << 31,
    Asm = 1 << 32,
    Catch = 1 << 33,
    Class = 1 << 34,
    ConstCast = 1 << 35,
    Delete = 1 << 36,
    DynamicCast = 1 << 37,
    Explicit = 1 << 38,
    Export = 1 << 39,
    Friend = 1 << 40,
    Inline = 1 << 41,
    Mutable = 1 << 42,
    New = 1 << 43,
    Operator = 1 << 44,
    Private = 1 << 45,
    Protected = 1 << 46,
    Public = 1 << 47,
    ReinterpretCast = 1 << 48,
    StaticCast = 1 << 49,
    Template = 1 << 50,
    This = 1 << 51,
    Throw = 1 << 52,
    Typename = 1 << 53,
    Using = 1 << 54,
    WcharT = 1 << 55,
    Bool = 1 << 56,
    Complex = 1 << 57,
    Imaginary = 1 << 58,
    Restrict = 1 << 59,
    Alignas = 1 << 60,
    Alignof = 1 << 61,
    Atomic = 1 << 62,
    Try = 1 << 63,
    Generic = 1 << 64,
    Noreturn = 1 << 65,
    StaticAssert = 1 << 66,
    ThreadLocal = 1 << 67,
    Virtual = 1 << 68,
    Typeof = 1 << 69,
    Assert = 1 << 70,
    MacroRules = 1 << 71,
    MacroExport = 1 << 72,
    MacroUse = 1 << 73,
    Boolean = 1 << 74,
    Or = 1 << 75,
    And = 1 << 76,
    Not = 1 << 77,
    IfLet = 1 << 78,
    ElseIf = 1 << 79,
    IfDef = 1 << 80,
    EndIf = 1 << 81,
}

impl Keyword {
    pub fn reset(&mut self) {
        *self = Keyword::None;
    }

    pub fn set(&mut self, keyword: Keyword) {
        *self |= keyword;
    }
    pub fn unset(&mut self, keyword: Keyword) {
        *self &= !keyword;
    }
    pub fn has(&self, keyword: Keyword) -> bool {
        *self & keyword == keyword
    }
    pub fn is(&self, keyword: Keyword) -> bool {
        *self == keyword
    }
    pub fn is_not(&self, keyword: Keyword) -> bool {
        *self != keyword
    }
    pub fn toggle(&mut self, flag: Keyword) {
        *self ^= flag;
    }
}

impl From<u128> for Keyword {
    fn from(value: u128) -> Self {
        unsafe { std::mem::transmute(value) }
    }
}

impl From<Keyword> for u128 {
    fn from(value: Keyword) -> Self {
        value as u128
    }
}

impl Not for Keyword {
    type Output = Self;

    fn not(self) -> Self {
        Self::from(!(self as u128))
    }
}

impl BitOr for Keyword {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self {
        Self::from(self as u128 | rhs as u128)
    }
}

impl BitOrAssign for Keyword {
    fn bitor_assign(&mut self, rhs: Self) {
        *self = *self | rhs;
    }
}

impl BitAnd for Keyword {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self {
        Self::from(self as u128 & rhs as u128)
    }
}

impl BitAndAssign for Keyword {
    fn bitand_assign(&mut self, rhs: Self) {
        *self = *self & rhs;
    }
}

impl BitXor for Keyword {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self {
        Self::from(self as u128 ^ rhs as u128)
    }
}

impl BitXorAssign for Keyword {
    fn bitxor_assign(&mut self, rhs: Self) {
        *self = *self ^ rhs;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(u32)]
pub enum Attribute {
    None = 0,
    Const = 1 << 0,
    Volatile = 1 << 1,
    Static = 1 << 2,
    Extern = 1 << 3,
    Inline = 1 << 4,
    Restrict = 1 << 5,
    Noreturn = 1 << 6,
    Atomic = 1 << 7,
    ThreadLocal = 1 << 8,
    Alignas = 1 << 9,
    Nonnull = 1 << 10,
    NullUnspecified = 1 << 11,
    Nullable = 1 << 12,
    Cdecl = 1 << 13,
    Stdcall = 1 << 14,
    Fastcall = 1 << 15,
    Vectorcall = 1 << 16,
    Thiscall = 1 << 17,
    Pascal = 1 << 18,
    Attribute = 1 << 19,
    Declspec = 1 << 20,
    Asm = 1 << 21,
    Extension = 1 << 22,
    IfDef = 1 << 23,
    EndIf = 1 << 24,
}

impl FromStr for Attribute {
    type Err = ConversionError;
    #[allow(unreachable_patterns)]
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut attribute = Attribute::None;
        for keyword in s.split_whitespace() {
            match keyword {
                "const" => attribute.set(Attribute::Const),
                "volatile" => attribute.set(Attribute::Volatile),
                "static" => attribute.set(Attribute::Static),
                "extern" => attribute.set(Attribute::Extern),
                "inline" => attribute.set(Attribute::Inline),
                "restrict" => attribute.set(Attribute::Restrict),
                "noreturn" => attribute.set(Attribute::Noreturn),
                "atomic" => attribute.set(Attribute::Atomic),
                "thread_local" => attribute.set(Attribute::ThreadLocal),
                "alignas" => attribute.set(Attribute::Alignas),
                "nonnull" => attribute.set(Attribute::Nonnull),
                "null_unspecified" => attribute.set(Attribute::NullUnspecified),
                "nullable" => attribute.set(Attribute::Nullable),
                "cdecl" => attribute.set(Attribute::Cdecl),
                "stdcall" => attribute.set(Attribute::Stdcall),
                "fastcall" => attribute.set(Attribute::Fastcall),
                "vectorcall" => attribute.set(Attribute::Vectorcall),
                "thiscall" => attribute.set(Attribute::Thiscall),
                "pascal" => attribute.set(Attribute::Pascal),
                "attribute" => attribute.set(Attribute::Attribute),
                "declspec" => attribute.set(Attribute::Declspec),
                "asm" => attribute.set(Attribute::Asm),
                "extension" => attribute.set(Attribute::Extension),
                "#ifdef" => attribute.set(Attribute::IfDef),
                "#endif" => attribute.set(Attribute::EndIf),
                _ => {
                    return Err(ConversionError::General(format!(
                        "Unknown attribute: {}",
                        keyword.to_string()
                    )));
                }
            }
        }
        Ok(attribute)
    }
}

impl Attribute {
    pub fn none() -> Attribute {
        Attribute::None
    }
    pub fn set(&mut self, attribute: Attribute) {
        *self |= attribute;
    }
    pub fn unset(&mut self, attribute: Attribute) {
        *self &= !attribute;
    }
    pub fn reset(&mut self) {
        *self = Attribute::None;
    }
    pub fn has(&self, attribute: Attribute) -> bool {
        *self & attribute == attribute
    }
    pub fn is(&self, attribute: Attribute) -> bool {
        *self == attribute
    }
    pub fn is_not(&self, attribute: Attribute) -> bool {
        *self != attribute
    }
    pub fn toggle(&mut self, flag: Attribute) {
        *self ^= flag;
    }
}

impl From<Attribute> for u32 {
    fn from(value: Attribute) -> Self {
        value as u32
    }
}

impl From<u32> for Attribute {
    fn from(value: u32) -> Attribute {
        unsafe { std::mem::transmute(value) }
    }
}

impl BitOr for Attribute {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self {
        Self::from(self as u32 | rhs as u32)
    }
}

impl BitOrAssign for Attribute {
    fn bitor_assign(&mut self, rhs: Self) {
        *self = *self | rhs;
    }
}

impl BitAnd for Attribute {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self {
        Self::from(self as u32 & rhs as u32)
    }
}

impl BitAndAssign for Attribute {
    fn bitand_assign(&mut self, rhs: Self) {
        *self = *self & rhs;
    }
}

impl BitXor for Attribute {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self {
        Self::from(self as u32 ^ rhs as u32)
    }
}

impl BitXorAssign for Attribute {
    fn bitxor_assign(&mut self, rhs: Self) {
        *self = *self ^ rhs;
    }
}

impl Not for Attribute {
    type Output = Self;

    fn not(self) -> Self {
        Self::from(!(self as u32))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Type {
    pub name: String,
    pub flags: Attribute,
}

impl Type {
    pub fn is_known_type(s: &str) -> bool {
        matches!(
            s,
            "char"
                | "short"
                | "int"
                | "long"
                | "float"
                | "double"
                | "void"
                | "FILE"
                | "size_t"
                | "ssize_t"
                | "int8_t"
                | "int16_t"
                | "int32_t"
                | "int64_t"
                | "uint8_t"
                | "uint16_t"
                | "uint32_t"
                | "uint64_t"
                | "unsigned"
                | "signed"
                | "const"
        )
    }

    /// Converts a C type to its Rust equivalent
    pub fn convert(c_type: &str) -> String {
        let mut c_type_str = c_type.trim().to_string();
        // Special cases for multi-word types
        if c_type.contains("unsigned") {
            c_type_str = c_type.replace("unsigned", "u");
        }
        if c_type.contains("long long") {
            c_type_str = c_type.replace("long long", "i64");
        } else if c_type.contains("long") {
            c_type_str = c_type.replace("long", "i64");
        }

        let parts: Vec<&str> = c_type_str.split_whitespace().collect();
        let c_type_str = parts.join(" ");

        let rust_type = match c_type_str.as_str() {
            "char" => "i8".to_string(),
            "short" => "i16".to_string(),
            "int" => "i32".to_string(),
            "long" => "i64".to_string(),
            "long long" => "i64".to_string(),
            "unsigned char" => "u8".to_string(),
            "unsigned short" => "u16".to_string(),
            "unsigned int" => "u32".to_string(),
            "unsigned long" => "u64".to_string(),
            "unsigned long long" => "u64".to_string(),
            "float" => "f32".to_string(),
            "double" => "f64".to_string(),
            "void" => "()".to_string(),
            "FILE" => "FILE".to_string(),
            "size_t" => "usize".to_string(),
            "ssize_t" => "isize".to_string(),
            _ => c_type_str,
        };

        Self::escape(&rust_type)
    }

    /// Escapes a Rust type name if it is a keyword
    pub fn escape(name: &str) -> String {
        match name {
            "as" | "break" | "const" | "continue" | "crate" | "else" | "enum" | "extern"
            | "false" | "fn" | "for" | "if" | "impl" | "in" | "let" | "loop" | "match" | "mod"
            | "move" | "mut" | "pub" | "ref" | "return" | "self" | "Self" | "static" | "struct"
            | "super" | "trait" | "true" | "type" | "unsafe" | "use" | "where" | "while"
            | "async" | "await" | "dyn" | "abstract" | "become" | "box" | "do" | "final"
            | "macro" | "override" | "priv" | "typeof" | "unsized" | "virtual" | "yield"
            | "try" => format!("r#{}", name),
            _ => name.to_string(),
        }
    }

    pub fn sanitize_name(name: &str) -> String {
        Self::escape(name)
    }
}

impl Default for Type {
    fn default() -> Self {
        Type {
            name: String::new(),
            flags: Attribute::None,
        }
    }
}
