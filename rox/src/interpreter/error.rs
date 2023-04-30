#[derive(thiserror::Error, Debug)]
pub enum RuntimeError {
    #[error("Inavlid Operand {operand} for {expr_type} expression, {msg}")]
    InvalidOperand {
        operand: String,
        expr_type: String,
        msg: String,
    },

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

    #[error("Undefined variable: {ident}")]
    UndefinedVariable { ident: String },

    #[error("Unintitialized variable : {ident}")]
    UninitializedVariable { ident: String },

    #[error("Expected boolean value as condition for conditional statement")]
    ExpectedBooleanCondition,

    #[error("Missing {} required positional argument{}: {}.", args.len(), if args.len() > 1 { "s" } else { "" }, args.join(", "))]
    MissingPositionalArguments { args: Vec<String> },

    #[error("Too many arguments for function call {}", fun)]
    TooManyArguments { fun: String },
}
