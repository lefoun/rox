pub mod error;
pub mod parser;
pub mod scanner;

use error::ScanError;
use parser::{AstPrinter, Visitor};
use std::fs;
use std::io::{self, BufRead, Write};

fn run(content: String) -> Result<(), ScanError> {
    let mut scanner = scanner::Scanner::new(content);
    let tokens = scanner.scan_tokens();
    let mut parser = parser::Parser::new(tokens.into_iter());
    match parser.parse() {
        Some(expr) => {
            println!("Found Expression");
            let ast_printer = AstPrinter;
            println!("{}", ast_printer.visit_expr(&expr));
        }
        None => println!("Found No Expression"),
    }
    Ok(())
}

pub fn run_file(path: String) -> Result<(), ScanError> {
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
