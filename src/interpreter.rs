use crate::parser::*;
use std::collections::HashMap;
// use Box;


#[derive(Debug, Clone)]
pub enum Exp {
    Int(i64),
    Float(f64),
    Symbol(String),
    List(Vec<Exp>),
    // probably split between tokens and interpreter types
    // Lambda(Box<Exp>, Vec<String>)
}


pub fn exp_from_tokens(mut tokens: &mut Vec<Token>) -> Result<Exp, ()> {
    if tokens.is_empty() {
        return Err(());
    }

    let token = tokens.remove(0);
    match token {
        Token::LParen => {
            let mut args = vec![];
            while tokens[0] != Token::RParen {
                args.push(exp_from_tokens(&mut tokens)?);
            }
            // remove ')'
            tokens.remove(0);
            Ok(Exp::List(args))
        },
        Token::Integer(value) => Ok(Exp::Int(value)),
        Token::Float(value) => Ok(Exp::Float(value)),
        Token::Symbol(value) => Ok(Exp::Symbol(value)),
        Token::RParen => Err(())
    }
}


mod test {
    use super::*;

    #[test]
    fn exp() {
        let mut prog = vec![];
        assert!(exp_from_tokens(&mut prog).is_err());

        
        let mut prog: Vec<Token> = tokenize("(print 1 (+ 1 2))");
        assert!(exp_from_tokens(&mut prog).is_ok());
    }
}

// fn insert_from_env(mut env: Env, update: Env) {
//     for key in update {
//         if key not in env {
//             env.insert(key, update[key])
//         }
//     }
// }


// TODO: environment
pub type Env = HashMap<String, Exp>;

// TODO: global builtins env/or tree env structure instead of hasmap/struct wiht parent env to lookup in case
// static global_env
// fn lookeup(local env) -> Exp

fn is_true(exp: Exp) -> bool {
    match exp {
        Exp::Float(value) => value != 0.0,
        Exp::Int(value) => value != 0,
        _ => false
    }
}

pub fn eval(expression: Exp, environment: &mut Env) -> Exp {
    match expression.clone() {
        Exp::List(expressions) => {
            match expressions[0].clone() {
                // Exp::Lambda(body, arguments) => {
                //     let new_env = arguments.zip(expression[1..]).collect();
                //     new_env.extend(environment);
                //     eval(body, new_env)
                // },
                Exp::Symbol(symbol) => {
                    match symbol.as_str() {
                        "if" => {
                            let exp = if is_true(expressions[1].clone()) {
                                &expressions[2]
                            } else {
                                &expressions[3]
                            };
                            eval(exp.clone(), environment)
                        },
                        "define" => {
                            if let Exp::Symbol(name) = &expressions[1] {
                                let value = &expressions[2];
                                let value = eval(value.clone(), environment);
                                environment.insert(name.clone(), value.clone());
                                environment.get(name).unwrap().clone()
                            } else {
                                panic!();
                            }
                        }
                        // Exp::Symbol(symbol) if &symbol == "lambda" => {
                        //     let arguments = expression[1];
                        //     let body = expression[2];
                        //     Exp::Lambda(arguments, body)
                        // },
                        // symbol => {
                        //     let args = expression[1..].map(|exp| eval(exp, environment)).collect();
                        //     environment[&symbol].call(args)
                        // }
                        _ => expression
                    }
                },
                number => number
            }
        },
        Exp::Symbol(symbol) => environment.get(&symbol).unwrap().clone(),
        number => number
    }
}
