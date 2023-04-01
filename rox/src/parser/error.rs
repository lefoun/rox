#[derive(thiserror::Error, Debug)]
pub enum ParseError {
    #[error("Parse error: Unexpected token {token}")]
    UnexpectedToken { token: String },

    #[error("Parse error: Unexpected end of file")]
    UnexpectedEOF,

    #[error("Parse error: Expected token {token}")]
    ExpectedToken { token: String },

    #[error("Parse error {msg}")]
    BadOperand { msg: String },

    #[error("Parse error: Cant have more than 255 arguments")]
    MaxNbArgumentsReached,
}