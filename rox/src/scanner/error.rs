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