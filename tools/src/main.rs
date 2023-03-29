use std::env;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;
use std::process;

fn define_ast(output_file: &mut File, types: &[&str]) -> Result<(), std::io::Error> {
    write!(
        output_file,
        "// Automatically generated Structs, Traits and their impl.\n"
    )?;
    // should change this to only include relevant names
    write!(output_file, "use crate::scanner::*;\n")?;
    for type_ in types {
        let mut line = type_.split(':');
        let struct_name = line.next().unwrap().split_whitespace().next().unwrap();
        write!(output_file, "#[derive(Expr, Impl)]\n")?;
        write!(output_file, "struct {struct_name} {{\n")?;
        let members = line.next().unwrap().split(',');
        for member in members {
            let mut member = member.split_whitespace();
            let type_name = match member.next().unwrap() {
                "Expr" => "Box<dyn Expr>",
                other => other,
            };
            let field_name = member.next().unwrap();

            write!(output_file, "    {field_name}: {type_name},\n")?;
        }
        write!(output_file, "}}\n")?;
    }
    Ok(())
}

fn main() -> Result<(), std::io::Error> {
    if env::args().len() != 2 {
        eprintln!("Usage generate_ast <output_file>");
        process::exit(64);
    }
    let output_file = PathBuf::from(env::args().nth(1).unwrap());
    let mut output_file = File::create(output_file)?;

    let types = vec![
        "Binary   : Expr left, Token operator, Expr right",
        "Grouping : Expr expression",
        "Literal  : Token value",
        "Unary    : Token operator, Expr right",
    ];
    define_ast(&mut output_file, &types)?;
    Ok(())
}
