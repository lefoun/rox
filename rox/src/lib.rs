pub mod error;
pub mod parser;
pub mod scanner;

use crate::error::Error;
use std::fs;
use std::io::{self, BufRead, Write};

fn run(content: String) -> Result<(), Error> {
    let mut scanner = scanner::Scanner::new(content);
    let tokens = scanner.scan_tokens();
    for token in tokens {
        println!("{:?}", token);
    }
    Ok(())
}

pub fn run_file(path: String) -> Result<(), Error> {
    let contents = fs::read_to_string(path)?;
    run(contents)
}

pub fn run_prompt() {
    let reader = io::stdin();

    print!("Rox interactive mode.\n>>> ");
    io::stdout().flush().unwrap();

    for line in reader.lock().lines() {
        if let Err(e) = line {
            eprintln!("{e}");
        } else {
            let res = run(line.unwrap());
            if let Err(e) = res {
                eprintln!("Error of type {e} when runing lox scanner");
            }
        }
        print!(">>> ");
        io::stdout().flush().unwrap();
    }
}

#[cfg(test)]
mod tests {}

