use std::io::{self, stdin, stdout, Write};

pub mod ast;
pub mod error;
pub mod evaluate;
pub mod parse;
pub mod token;

fn main() -> Result<(), io::Error> {
    let mut input = String::new();
    let mut state: evaluate::State = Default::default();
    loop {
        print!(">>> ");
        stdout().flush()?;
        stdin().read_line(&mut input)?;

        if input.trim() == "exit".to_string() {
            break;
        }

        let tokens = token::tokenize(&input.trim());
        input = "".to_string();
        let tree = match parse::parse(tokens) {
            Ok(v) => v,
            Err(e) => {
                eprintln!("error: {}", e);
                continue;
            }
        };
        let (_, res) = evaluate::execute!(tree, state);
        match res[0] {
            Ok(ref v) => println!("{}", v.to_string()),
            Err(ref e) => eprintln!("{}", e),
        }
    }
    Ok(())
}
