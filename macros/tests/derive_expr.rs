use macros::Expr;
trait Expr {
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

#[derive(Expr)]
struct BinaryStub {
    left: Box<dyn Expr>,
    right: Box<dyn Expr>,
    operator: Token,
}

#[derive(Clone, Copy)]
struct Stub;
impl Expr for Stub {}

#[derive(Clone, Copy)]
struct Token;

fn main() {
    let stub = Stub;
    let token = Token;
    let b = BinaryStub {
        left: Box::new(stub),
        right: Box::new(stub),
        operator: token,
    };

    assert!(b.left().is_some());
    assert!(b.right().is_some());
    assert!(b.operator().is_some());
}
