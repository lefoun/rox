use rox;
use std::env;
fn main() -> Result<(), rox::error::Error> {
    match env::args().len() {
        1 => Ok(rox::run_prompt()),
        2 => rox::run_file(env::args().into_iter().nth(2).unwrap()),
        _ => {
            println!("Usage: lox [script]");
            return Err(rox::error::Error::BadArguments);
        }
    }
}
