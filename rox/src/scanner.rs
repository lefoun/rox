use std::collections::HashMap;
use std::str::CharIndices;

pub struct Scanner {
    source: String,
    start: usize,
    current: usize,
    line: usize,
    tokens: Vec<Token>,
    had_error: bool,
    keywords: HashMap<String, TokenType>,
}

impl Scanner {
    pub fn new(source: String) -> Self {
        use TokenType::*;
        let keywords: HashMap<String, TokenType> = [
            ("and", And),
            ("class", Class),
            ("else", Else),
            ("false", False),
            ("fun", Fun),
            ("for", For),
            ("if", If),
            ("null", Null),
            ("or", Or),
            ("print", Print),
            ("return", Return),
            ("super", Super),
            ("this", This),
            ("true", True),
            ("var", Var),
            ("while", While),
        ]
        .into_iter()
        .map(|(s, t)| (s.to_owned(), t))
        .collect();
        Self {
            source,
            start: 0,
            current: 0,
            line: 0,
            tokens: Vec::new(),
            had_error: false,
            keywords,
        }
    }

    pub fn scan_tokens(&mut self) -> Vec<Token> {
        // need to clone self because behind a mutable borrow
        let source = self.source.to_owned();
        let mut source_iter = source.char_indices().peekable();

        while let Some((i, c)) = source_iter.next() {
            self.start = i;
            self.current = i;
            self.scan_token(c, i, &mut source_iter);
        }

        self.tokens
            .push(Token::new(TokenType::Eof, self.line, String::new()));
        self.tokens.clone()
    }

    fn scan_token(&mut self, c: char, pos: usize, source: &mut std::iter::Peekable<CharIndices>) {
        use TokenType::*;
        match c {
            '+' => self.add_token(Plus, c),
            '*' => self.add_token(Star, c),
            '-' => self.add_token(Minus, c),
            ',' => self.add_token(Comma, c),
            '.' => self.add_token(Dot, c),
            ';' => self.add_token(SemiColon, c),
            ':' => self.add_token(Colon, c),
            '(' => self.add_token(LeftParen, c),
            '[' => self.add_token(LeftBracket, c),
            '{' => self.add_token(LeftBrace, c),
            ')' => self.add_token(RightParen, c),
            ']' => self.add_token(RightBracket, c),
            '}' => self.add_token(RightBrace, c),

            '!' => match source.next_if_eq(&(pos + 1, '=')) {
                Some((_, chr)) => self.add_token(BangEqual, chr),
                None => self.add_token(Bang, c),
            },
            '>' => match source.next_if_eq(&(pos + 1, '=')) {
                Some((_, chr)) => self.add_token(GreaterEqual, chr),
                None => self.add_token(Greater, c),
            },
            '=' => match source.next_if_eq(&(pos + 1, '=')) {
                Some((_, chr)) => self.add_token(DoubleEqual, chr),
                None => self.add_token(Equal, c),
            },
            '<' => match source.next_if_eq(&(pos + 1, '=')) {
                Some((_, chr)) => self.add_token(LessEqual, chr),
                None => self.add_token(Less, c),
            },
            '/' => match source.next_if_eq(&(pos + 1, '/')) {
                Some(_) => {
                    source
                        .take_while(|(_, char)| *char != '\n')
                        .for_each(|(_, char)| {
                            if char == '\n' {
                                self.line += 1;
                            }
                        });
                }
                None => self.add_token(Slash, c),
            },
            '"' => self.add_string_token(source),
            '0'..='9' => self.add_number_token(c, source),
            'a'..='z' | 'A'..='Z' | '_' => self.add_literal_token(c, source),
            '\n' => {
                self.line += 1;
                self.current += 1;
            }
            ' ' | '\r' | '\t' => self.current += 1,
            other => self.error(other),
        }
    }

    fn add_token(&mut self, tkn_type: TokenType, lexem: char) {
        self.tokens
            .push(Token::new(tkn_type, self.line, lexem.to_string()));
    }

    fn add_literal_token(&mut self, chr: char, source: &mut impl Iterator<Item = (usize, char)>) {
        let mut literal: String = String::from(chr);
        literal.extend(
            source
                .take_while(|(_, chr)| chr.is_ascii_alphabetic() || *chr == '_')
                .map(|(_, chr)| chr),
        );

        self.current += literal.len();

        let lexem = self.source.as_str()[self.start..self.current].to_owned();
        match self.keywords.get(&literal) {
            Some(val) => self.tokens.push(Token::new(val.clone(), self.line, lexem)),
            None => self
                .tokens
                .push(Token::new(TokenType::Identifier, self.line, lexem)),
        }
    }

    fn add_number_token(&mut self, chr: char, source: &mut impl Iterator<Item = (usize, char)>) {
        let mut num: String = String::from(chr);
        num.extend(
            source
                .take_while(|(_, c)| c.is_ascii_alphanumeric() || *c == '.')
                .map(|(_, c)| c),
        );

        match num.parse::<f64>() {
            Ok(number) => {
                self.current += num.len();
                let lexem = self.source.as_str()[self.start..self.current].to_owned();
                self.tokens
                    .push(Token::new(TokenType::Number(number), self.line, lexem));
            }
            Err(_) => self.error(chr),
        };
    }

    fn add_string_token(&mut self, source: &mut impl Iterator<Item = (usize, char)>) {
        let mut last_char = '"';
        let mut s = String::new();
        source
            .take_while(|(_, chr)| *chr != '"')
            .for_each(|(_, c)| {
                if c == '\n' {
                    self.line += 1;
                }
                last_char = c;
                s.push(c);
            });
        if last_char == '"' {
            // it means we reached EOF before closing the string
            self.error('"');
        } else {
            self.current += s.len() + 1; // + 1 for the other ' " '
            self.tokens
                .push(Token::new(TokenType::RoxString(s.clone()), self.line, s))
        }
    }

    fn error(&mut self, c: char) {
        self.had_error = true;
        eprintln!("Unrecognized character {c} at line {}", self.line);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    // single character tokens
    Bang,
    Carrot,
    Colon,
    Comma,
    Dot,
    Equal,
    Greater,
    LeftBrace,
    LeftBracket,
    LeftParen,
    Less,
    Minus,
    Plus,
    RightBrace,
    RightBracket,
    RightParen,
    Star,
    SemiColon,
    Slash,

    // two or more characters tokens
    BangEqual,
    DoubleEqual,
    GreaterEqual,
    LessEqual,

    // literals
    Identifier,
    Number(f64),
    RoxString(String),

    // keywords
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Null,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Invalid,
    Eof,
}

#[derive(Debug, Clone)]
pub struct Token {
    token_type: TokenType,
    line: usize,
    lexem: String,
}

impl Token {
    pub fn new(token_type: TokenType, line: usize, lexem: String) -> Self {
        Self {
            token_type,
            line,
            lexem,
        }
    }

    pub fn lexem(&self) -> &str {
        self.lexem.as_str()
    }

    pub fn token_type(&self) -> TokenType {
        self.token_type.clone()
    }
}

impl ToString for Token {
    fn to_string(&self) -> String {
        format!("{:?} {}", self.token_type, self.lexem)
    }

    pub fn line(&self) -> usize {
        self.line
    }
}

#[cfg(test)]
mod tests {}
