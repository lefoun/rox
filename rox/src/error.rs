#[derive(thiserror::Error, Debug)]
pub enum RoxError {
    #[error("ScanError")]
    Scan,
    #[error("ParseError")]
    Parser,
    #[error("RuntimeError")]
    RuntimeError,
}
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

    #[error("Parse error at line: {line}")]
    BadOperand { msg: String, line: usize },
}

#[derive(thiserror::Error, Debug)]
pub enum RuntimeError {
    #[error("Inavlid Operand {operand} for {expr_type} expression at line: {line}")]
    InvalidOperand {
        operand: String,
        line: usize,
        expr_type: String,
    },

    #[error("Inavlid Operands lhs: {lhs} rhs: {rhs} for {expr_type} expression at line: {line}")]
    InvalidOperands {
        lhs: String,
        rhs: String,
        expr_type: String,
        line: usize,
    },

    #[error("Invalid expression: Expected literal got {token} at line {line}")]
    ExpectedLiteral { token: String, line: usize },

    #[error("Unsupported binary operator: {op} at line {line}")]
    UnsupportedBinaryOperator { op: String, line: usize },

    #[error("Unexpected Unary operator: {op} at line {line}")]
    UnexpectedUnaryOperator { op: String, line: usize },
}
