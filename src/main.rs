use std::{fs, io};

use clap::{Parser, Subcommand};
use evaluate::{interpret, repl};

pub mod ast;
pub mod error;
pub mod evaluate;
pub mod parse;
pub mod token;

fn main() -> io::Result<()> {
    let cli = Cli::parse();
    match cli.command {
        Commands::Repl => repl()?,
        Commands::Exec { file } => {
            let doc = match fs::read_to_string(file) {
                Ok(s) => s,
                Err(e) => {
                    eprintln!("{}", e);
                    return Ok(());
                }
            };
            match interpret(&doc) {
                Ok(v) => println!("{}", v.to_string()),
                Err(e) => eprintln!("{}", e),
            }
        }
    }
    Ok(())
}

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Clone)]
enum Commands {
    #[command(about = "Run the REPL")]
    Repl,
    #[command(about = "Execute the file")]
    Exec { file: String },
}
