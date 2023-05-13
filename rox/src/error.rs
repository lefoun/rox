#[derive(thiserror::Error, Debug)]
pub enum RoxError {
    #[error("ScanError")]
    Scan,
    #[error("ParseError")]
    Parser,
    #[error("RuntimeError")]
    RuntimeError,
    #[error("ResolveError")]
    Resolve,
}
