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
    #[error("Parse error: Unexpected token {token}")]
    UnexpectedToken { token: String },

    #[error("Parse error: Unexpected end of file")]
    UnexpectedEOF,

    #[error("Parse error: Expected token {token}")]
    ExpectedToken { token: String },

    #[error("Parse error {msg}")]
    BadOperand { msg: String },

    #[error("Parse error: Expected expression")]
    ExpectedExpression
}

#[derive(thiserror::Error, Debug)]
pub enum RuntimeError {
    #[error("Inavlid Operand {operand} for {expr_type} expression, {msg}")]
    InvalidOperand { operand: String, expr_type: String, msg: String },

    #[error("Inavlid Operands '{lhs}' and '{rhs}' for {expr_type} expression. {msg}")]
    InvalidOperands {
        lhs: String,
        rhs: String,
        expr_type: String,
        msg: String,
    },

    #[error("Invalid expression: Expected literal got {token}")]
    ExpectedLiteral { token: String },

    #[error("Unsupported binary operator: {op}")]
    UnsupportedBinaryOperator { op: String },

    #[error("Unexpected Unary operator: {op}")]
    UnexpectedUnaryOperator { op: String },

    #[error("Invalid identifier: {ident}")]
    InvalidIdentifier { ident: String },

    #[error("Undefined identifier: {ident}")]
    UndefinedIdentifier { ident: String },

    #[error("Undefined variable: {ident}")]
    UndefinedVariable { ident: String },

    #[error("Unintitialized variable : {ident}")]
    UninitializedVariable { ident: String },

    #[error("Expected boolean value as condition for conditional statement")]
    ExpectedBooleanCondition,
}
