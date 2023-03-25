use std::str::CharIndices;

struct Scanner {
    source: String,
    start: usize,
    current: usize,
    line: usize,
    tokens: Vec<Token>,
}

impl Scanner {
    pub fn new(source: String) -> Self {
        Self {
            source,
            start: 0,
            current: 0,
            line: 0,
            tokens: Vec::new(),
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    pub fn scan_tokens(&mut self) -> Vec<Token> {
        let source = self.source.to_owned();
        let mut source = source.char_indices();
        self.start = self.current;

        while let Some((i, c)) = source.next() {
            self.scan_token(c, &mut source);
        }

        self.tokens.push(Token::new(
            TokenType::Eof,
            self.line,
            String::new(),
            Literal {},
        ));
        self.tokens.clone()
    }

    fn scan_token(&mut self, c: char, source: &mut CharIndices) {
        use TokenType::*;
        match c {
            '0'..='9' => (),
            'A'..='Z' | 'a'..='z' | '_' => (),
            '+' => self.add_token(Plus),
            '*' => self.add_token(Star),
            '-' => self.add_token(Minus),
            ',' => self.add_token(Comma),
            '.' => self.add_token(Dot),
            ';' => self.add_token(SemiColon),
            ':' => self.add_token(Colon),
            '/' => self.add_token(Slash),
            '(' => self.add_token(LeftParen),
            '[' => self.add_token(LeftBracket),
            '{' => self.add_token(LeftBrace),
            ')' => self.add_token(RightParen),
            ']' => self.add_token(RightBracket),
            '}' => self.add_token(RightBrace),
            '>' => (),
            '=' => (),
            '<' => (),
            ' ' => (),
            '\n' => (),
            _ => (),
        }
    }

    fn add_token(&mut self, tkn_type: TokenType) {
        self.tokens
            .push(Token::new(tkn_type, self.line, String::new(), Literal {}));
    }
    fn add_literal_token(&mut self, tkn_type: TokenType) {
        let lexem = self.source.as_str()[self.start..self.current].to_owned();
        self.tokens
            .push(Token::new(tkn_type, self.line, lexem, Literal {}));
    }
}

#[derive(Debug, Clone, Copy)]
enum TokenType {
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
    Number,
    String,

    // keywords
    Add,
    Class,
    Else,
    False,
    Fun,
    If,
    Nil,
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

#[derive(Debug, Clone, Copy)]
struct Literal {}
#[derive(Debug, Clone)]
struct Token {
    tkn_type: TokenType,
    line: usize,
    lexem: String,
    literal: Literal,
}

impl Token {
    fn new(tkn_type: TokenType, line: usize, lexem: String, literal: Literal) -> Self {
        Self {
            tkn_type,
            line,
            lexem,
            literal,
        }
    }

    pub fn to_string(&self) -> String {
        format!("{:?} {} {:?}", self.tkn_type, self.lexem, self.literal)
    }
}

#[cfg(test)]
mod tests {}
