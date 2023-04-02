pub mod error;
pub mod exprs;
mod interepreter;
pub mod parser;
pub mod scanner;
pub mod stmts;

use error::RoxError;
use interepreter::Interpreter;
use std::fs;
use std::io::{self, BufRead, Write};

fn run(content: String) -> Result<(), RoxError> {
    let mut scanner = scanner::Scanner::new(content);
    let tokens = scanner.scan_tokens();
    let mut parser = parser::Parser::new(tokens.into_iter());
    let interepreter = Interpreter;
    let statements = parser.parse();
    match interepreter.interpret(statements) {
        Ok(()) => Ok(()),
        Err(e) => {
            eprintln!("{e}");
            return Err(RoxError::RuntimeError);
        }
    }
}

pub fn run_file(path: String) -> Result<(), RoxError> {
    let contents = fs::read_to_string(path).unwrap();
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
