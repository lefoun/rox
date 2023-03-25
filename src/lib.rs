pub mod error;
pub mod scanner;

use crate::error::Error;
use std::fs;
use std::io::{self, BufRead, Write};

fn run(content: String) -> Result<(), Error> {
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
        print!(">>> ");
        io::stdout().flush().unwrap();
        if let Err(e) = line {
            eprintln!("{e}");
        } else {
            let res = run(line.unwrap());
            if let Err(e) = res {
                eprintln!("Error of type {e} when runing lox scanner");
            }
        }
    }
}

#[cfg(test)]
mod tests {}

