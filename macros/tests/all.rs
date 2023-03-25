#[test]
fn main() {
    let t = trybuild::TestCases::new();
    t.pass("tests/derive_expr.rs");
    t.pass("tests/derive_impl.rs");
}
