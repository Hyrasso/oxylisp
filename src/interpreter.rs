use crate::parser::*;
use std::collections::HashMap;

type Env =  HashMap<String, Box<Fn(Exp) -> Exp>>;

pub fn eval(expression: Exp, environment: Env) -> Exp {
    match expression {
        Exp::List(expression) => {
            match expression[0] {
                Exp::Symbol(symbol) if &symbol == "if" => {
                    let exp = if expression[1] {
                        expression[2]
                    } else {
                        expression[3]
                    };
                    eval(exp, environment)
                },
                Exp::Symbol(symbol) if &symbol == "define" => {
                    let symbol = expression[1];
                    let value = expression[2];
                    environment[&symbol] = eval(value, environment);
                    *environment[&symbol]
                },
                Token::Symbol(symbol) => {
                    let args = expression[1..].map(|exp| eval(exp, environment)).collect();
                    environment[&symbol](args)
                },
                number => number
            }
        },
        Exp::Symbol(symbol) => environment[&symbol],
        number => number
    }
}