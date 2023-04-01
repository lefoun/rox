use rox::{self, error::RoxError};
use std::env;
fn main() {
    match env::args().len() {
        1 => rox::run_prompt(),
        2 => match rox::run_file(env::args().nth(1).unwrap()) {
            Ok(()) => (),
            Err(_) => std::process::exit(76),
        },
        _ => println!("Usage: lox [script]"),
    }
}
