mod interpreter;
use interpreter::*;
mod parser;
use parser::*;

use std::io::{BufRead, self, Write};

fn main() {
    if true {
        repl();
    } else {
        let mut env = interpreter::Interpreter::new();
        println!("{:?}", env.run("(define blop (if 1 (1) (0)))"));
        print!("{:?}", env);
    }
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