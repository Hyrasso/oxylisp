use oxylisp::interpreter;

use std::env;
use std::fs;
use std::io::{self, BufRead, Write};

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
        match res {
            Ok(res) => println!("{}", res),
            Err(e) => println!("{:?}", e)
        }
        print_repl_input();
    }
}
