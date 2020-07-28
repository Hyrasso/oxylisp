use super::interpreter::{Exp, Env, Error, Interpreter};

use std::{fs, rc::Rc};

// TODO: Some macro like #[proc(name, ?lib)] to generate a list of all the proc exported for a lib
pub fn equal(expression: Vec<Exp>, _environment: Rc<Env>) -> Result<Exp, Error> {
    Ok(Exp::Bool(expression[0] == expression[1]))
}

pub fn add(expression: Vec<Exp>, _environment: Rc<Env>) -> Result<Exp, Error> {
    match expression[..] {
        [Exp::Int(a), Exp::Int(b)] => Ok(Exp::Int(a + b)),
        [Exp::Float(a), Exp::Float(b)] => Ok(Exp::Float(a + b)),
        _ => Err(Error::SyntaxError("+ expects 2 arguments".to_string(), None))
    }
}

pub fn mul(expression: Vec<Exp>, _environment: Rc<Env>) -> Result<Exp, Error> {
    match expression[..] {
        [Exp::Int(a), Exp::Int(b)] => Ok(Exp::Int(a * b)),
        [Exp::Float(a), Exp::Float(b)] => Ok(Exp::Float(a * b)),
        _ => Err(Error::SyntaxError("* expects 2 arguments".to_string(), None))
    }   
}

// rewrite with the port thing and stdout
pub fn write(expression: Vec<Exp>, _environment: Rc<Env>) -> Result<Exp, Error> {
    if expression.len() == 0 {
        return Err(Error::SyntaxError("Not enough args for write".to_string(), None));
    }
    print!("{}", expression[0]);
    Ok(Exp::Bool(true))
}

pub fn dbg(expression: Vec<Exp>, _environment: Rc<Env>) -> Result<Exp, Error> {
    println!("{:?}", expression);
    Ok(Exp::Bool(true))
}

pub fn gt(expression: Vec<Exp>, _environment: Rc<Env>) -> Result<Exp, Error> {
    match expression[..] {
        [Exp::Int(a), Exp::Int(b)] => Ok(Exp::Bool(a > b)),
        [Exp::Float(a), Exp::Float(b)] => Ok(Exp::Bool(a > b)),
        _ => Err(Error::SyntaxError("> expects 2 arguments".to_string(), None))
    }   
} 

// fn car

// fn cdr

pub fn load(expression: Vec<Exp>, environment: Rc<Env>) -> Result<Exp, Error> {
    if let Exp::Symbol(filename) = &expression[0] {
        let mut interpreter = Interpreter {environment: Rc::clone(&environment)};
        let code = fs::read_to_string(filename).map_err(|e| {
            Error::RuntimeError(e.to_string() + &format!(" path: {}", filename))
        })?;
        interpreter.run_file(&code)
    } else {
        Err(Error::SyntaxError("Expected a file name".to_string(), Some(expression[0].clone())))
    }
}