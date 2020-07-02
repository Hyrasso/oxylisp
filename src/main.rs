mod parser;
use parser::*;
// mod interpreter;
// use interpreter::*;

fn main() {
    let mut prog: Vec<String> = tokenize("(print 1 (+ 1 2))");
    println!("{:?}", exp_from_str(&mut prog));
}
