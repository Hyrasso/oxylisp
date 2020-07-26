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
    // todo: do not break on lines but expression
    // rewrite parsing in a stream way?
    // maybe by keeping track of the current char at parse level we can show around where the errors come from
    // parser reads from stream, char by char -> iterator that returns exp to be evaluated
    // with some info about where the expression came from (start and stop chars)
    for line in stdin.lock().lines() {
        let res = interperter.run(&line.unwrap());
        match res {
            Ok(res) => println!("{}", res),
            Err(e) => println!("Error: {}", e)
        }
        print_repl_input();
    }
}
