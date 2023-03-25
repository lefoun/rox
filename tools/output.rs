#[derive(Expr)]
struct Binary {
    left: Box<dyn Expr>,
    operator: Token,
    right: Box<dyn Expr>,
}
#[derive(Expr)]
struct Grouping {
    expression: Box<dyn Expr>,
}
#[derive(Expr)]
struct Literal {
    value: Token,
}
#[derive(Expr)]
struct Unary {
    operator: Token,
    right: Box<dyn Expr>,
}
