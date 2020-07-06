mod parser;
use parser::*;
mod interpreter;
use interpreter::*;

fn main() {
    let mut prog: Vec<String> = tokenize("(begin (define r 10) (* pi (* r r)))");
    println!("{:?}", exp_from_str(&mut prog));
}
