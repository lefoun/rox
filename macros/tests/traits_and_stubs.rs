pub trait Expr {
    fn left(&self) -> Option<&dyn Expr> {
        None
    }
    fn right(&self) -> Option<&dyn Expr> {
        None
    }
    fn operator(&self) -> Option<&Token> {
        None
    }
}

#[derive(Clone, Copy)]
pub struct Token;