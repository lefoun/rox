#[derive(Debug)]
enum TokenType {
    // single character tokens
    Bang,
    Carrot,
    Colon,
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
}

#[cfg(test)]
mod tests {
}
