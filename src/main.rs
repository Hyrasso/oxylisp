mod parser;
use parser::*;
mod interpreter;
use interpreter::*;

fn main() {
    let mut prog: Vec<Token> = tokenize("(if 1 (1) (0))");
    let res = exp_from_tokens(&mut prog).unwrap();
    println!("{:?}", res);
    println!("{:?}", eval(res, &mut Env::new()));
}
