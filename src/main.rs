use std::io::{self, stdin, stdout, Write};

pub mod ast;
pub mod error;
pub mod evaluate;
pub mod parse;
pub mod token;

fn main() -> Result<(), io::Error> {
    let mut input = String::new();
    loop {
        print!(">>> ");
        stdout().flush()?;
        stdin().read_line(&mut input)?;

        if input.trim() == "exit".to_string() {
            break;
        }

        let tokens = token::tokenize(&input);
        let tree = parse::parse(tokens);
        let value = evaluate::evaluate(tree.unwrap()).unwrap();
        println!("{:?}", value);
        input = "".to_string();
    }
    Ok(())
}
