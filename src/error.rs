use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Unexpected syntax error in line: {line} {message}")]
    Syntax { message: String, line: u32 },
    #[error("Bad arguments")]
    BadArguments,
    #[error("Io error: {source}")]
    IoError {
        #[from]
        source: std::io::Error,
    },
}
