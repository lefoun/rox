mod traits_and_stubs;
use macros::{Impl};

use traits_and_stubs::{Expr, Token};

#[derive(Impl)]
struct BinaryStub {
    _left: Box<dyn Expr>,
    _right: Box<dyn Expr>,
    _operator: Token,
}

#[derive(Clone, Copy)]
struct Stub;
impl Expr for Stub {}

fn main() {
    let stub = Stub;
    let token = Token;
    let _b = BinaryStub::new(Box::new(stub), Box::new(stub), token);
}
