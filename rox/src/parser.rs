use crate::scanner::Token;
trait Expression {}
struct BinaryExpr {
    left: Box<dyn Expression>,
    right: Box<dyn Expression>,
    operand: Token,
}

impl BinaryExpr {
    pub fn new(left: Box<dyn Expression>, right: Box<dyn Expression>, operand: Token) -> Self {
        Self {
            left,
            right,
            operand,
        }
    }
}
