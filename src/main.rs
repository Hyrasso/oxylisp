mod interpreter;
use interpreter::*;
mod parser;
use parser::*;
mod forms;
use forms::*;
mod procedures;

use std::env;
use std::io::{BufRead, self, Write};
use std::fs;

fn main() -> io::Result<()> {
    let args: Vec<_> = env::args().collect();

    if args.len() == 1 {
        repl();
    } else {
        let file = &args[1];
        let prog = fs::read_to_string(file)?;
        let mut interpreter = interpreter::Interpreter::new();
        println!("{:?}", interpreter.run_file(&prog));
    }
    Ok(())
}

fn print_repl_input() {
    print!("|isitlispyet> ");
    io::stdout().flush().unwrap();
}

// todo remove unwraps
fn repl() {
    let stdin = io::stdin();
    let mut interperter = interpreter::Interpreter::new();
    print_repl_input();
    for line in stdin.lock().lines() {
        let res = interperter.run(&line.unwrap());
        println!("{:?}", res);
        print_repl_input();
    }
}