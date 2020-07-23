use super::interpreter::{Exp, Env, Error, eval, Lambda};
use std::rc::Rc;

pub fn define(expression: Vec<Exp>, environment: Rc<Env>) -> Result<Exp, Error> {
    match &expression[0] {
        Exp::Symbol(name) => {
            let value = &expression[1];
            let value = eval(value.clone(), Rc::clone(&environment))?;
            environment.insert(name.clone(), value.clone());
            environment.get(name)
        },
        _ => Err(Error::SyntaxError)
    }
}

pub fn set(expression: Vec<Exp>, environment: Rc<Env>) -> Result<Exp, Error> {
    if let Exp::Symbol(name) = &expression[0] {
        environment.set(name, eval(expression[1].clone(), Rc::clone(&environment))?)
    } else {
        Err(Error::SyntaxError)
    }
}

pub fn quote(expression: Vec<Exp>, _environment: Rc<Env>) -> Result<Exp, Error> {
    Ok(expression[0].clone())
}

fn is_true(exp: Exp) -> bool {
    match exp {
        Exp::Bool(value) => value,
        _ => true
    }
}

pub fn if_form(expression: Vec<Exp>, environment: Rc<Env>) -> Result<Exp, Error> {
    let exp = if is_true(eval(expression[0].clone(), Rc::clone(&environment))?) {
        &expression[1]
    } else {
        &expression[2]
    };
    eval(exp.clone(), environment)
}

pub fn lambda(expression: Vec<Exp>, environment: Rc<Env>) -> Result<Exp, Error> {
    let mut arguments = vec![];
    // (lambda (. a) a) is a valid syntax for now
    // equivalent to (lambda a a)
    let (args, body) = expression.split_first().unwrap();
    match args.clone() {
        Exp::List(args) => {
            for arg in args {
                if let Exp::Symbol(arg_symbol) = arg {
                    arguments.push(arg_symbol);
                } else {
                    // sould be only symbols
                    return Err(Error::SyntaxError);
                }
            }
        },
        Exp::Symbol(vararg_name) => {
            arguments.push(".".to_string());
            arguments.push(vararg_name);
        },
        _ => return Err(Error::SyntaxError)

    };
    let mut body_expand = vec![Exp::Symbol("begin".to_string())];
    body_expand.extend_from_slice(body);
    let lambda = Lambda::new(
        Box::new(Exp::List(body_expand)),
        arguments,
         Rc::clone(&environment)
    );
    Ok(Exp::Lambda(lambda))
}