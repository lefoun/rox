#[derive(thiserror::Error, Debug)]
pub enum ScanError {
    #[error("Unexpected syntax error in line: {line} {message}")]
    Syntax { message: String, line: usize },
    #[error("Bad arguments")]
    BadArguments,
    #[error("Io error: {source}")]
    IoError {
        #[from]
        source: std::io::Error,
    },
}

#[derive(thiserror::Error, Debug)]
pub enum ParseError {
    #[error("Parse error at line: {line} Unexpected Token {token}")]
    UnexpectedToken { token: String, line: usize },

    #[error("Parse error: Unexpected end of file")]
    UnexpectedEOF,

    #[error("Parse error at line: {line} Expected token {token}")]
    ExpectedToken { token: String, line: usize },
}
