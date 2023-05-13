#![feature(is_some_and)] // used in resolver

mod error;
mod interpreter;
mod parser;
mod scanner;

use error::RoxError;
use interpreter::interpreter::Interpreter;
use interpreter::resolver::Resolver;
use parser::parser::Parser;
use scanner::scanner::Scanner;
use std::fs;
use std::io::{self, BufRead, Write};

fn run(content: String, interepreter: &mut Interpreter, repl_mode: bool) -> Result<(), RoxError> {
    let mut scanner = Scanner::new(content);
    if scanner.had_error() {
        return Err(RoxError::Scan);
    }
    let tokens = scanner.scan_tokens();
    let mut parser = Parser::new(tokens.into_iter(), repl_mode);
    let statements = parser.parse();
    // neeed to handle parse errors here
    let mut resolver = Resolver::new(interepreter);
    if let Err(e) = resolver.resolve(statements.clone()) {
        eprintln!("{}: {e}", RoxError::Resolve);
        return Err(RoxError::Resolve);
    }
    if let Err(e) = interepreter.interpret(statements) {
        eprintln!("{}: {e}", RoxError::RuntimeError);
        return Err(RoxError::RuntimeError);
    }
    Ok(())
}

pub fn run_file(path: String) -> Result<(), RoxError> {
    let mut interpreter = Interpreter::new();
    let contents = fs::read_to_string(path).unwrap();
    run(contents, &mut interpreter, false)
}

pub fn run_prompt() {
    let reader = io::stdin();

    print!("Rox interactive mode.\n>>> ");
    io::stdout().flush().unwrap();

    let mut interepreter = Interpreter::new();

    for line in reader.lock().lines() {
        if let Err(e) = line {
            eprintln!("{e}");
        } else {
            let _ = run(line.unwrap(), &mut interepreter, true);
        }
        print!(">>> ");
        io::stdout().flush().unwrap();
    }
}

#[cfg(test)]
mod tests {}
