use crate::parser::*;
use std::collections::HashMap;

// fn insert_from_env(mut env: Env, update: Env) {
//     for key in update {
//         if key not in env {
//             env.insert(key, update[key])
//         }
//     }
// }

// TODO: environment
type Env =  HashMap<String, Exp>;

// TODO: global builtins env/or tree env structure instead of hasmap/struct wiht parent env to lookup in case
// static global_env
// fn lookeup(local env) -> Exp

pub fn eval(expression: Exp, environment: Env) -> Exp {
    match expression {
        Exp::List(expression) => {
            match expression[0] {
                Exp::Lambda(body, arguments) => {
                    let new_env = arguments.zip(expression[1..]).collect();
                    update(
                        new_env,
                        environment
                    );
                    eval(body, new_env)
                },
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
                    environment[&symbol]
                },
                Exp::Symbol(symbol) if &symbol == "lambda" => {
                    let arguments = expression[1];
                    let body = expression[2];
                    Exp::Lambda(arguments, body)
                },
                // Token::Symbol(symbol) => {
                //     let args = expression[1..].map(|exp| eval(exp, environment)).collect();
                //     environment[&symbol].call(args)
                // },
                number => number
            }
        },
        Exp::Symbol(symbol) => environment[&symbol],
        number => number
    }
}