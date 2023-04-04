mod environment;
mod error;
mod exprs;
mod interepreter;
mod parser;
mod scanner;
mod stmts;

use error::RoxError;
use interepreter::Interpreter;
use std::fs;
use std::io::{self, BufRead, Write};

fn run(content: String, interepreter: &mut Interpreter, repl_mode: bool) -> Result<(), RoxError> {
    let mut scanner = scanner::Scanner::new(content);
    let tokens = scanner.scan_tokens();
    let mut parser = parser::Parser::new(tokens.into_iter(), repl_mode);
    let statements = parser.parse();
    match interepreter.interpret_stmt(statements) {
        Ok(()) => Ok(()),
        Err(e) => {
            eprintln!("{}: {e}", RoxError::RuntimeError);
            return Err(RoxError::RuntimeError);
        }
    }
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
