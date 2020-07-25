use super::interpreter::{Exp, Env, Error};

use std::rc::Rc;

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
    if expression.len() < 1 {
        print!("{:?}", expression[0]);
        Ok(Exp::List(vec![]))
    } else {
        print!("{:?}", expression);
        Ok(Exp::List(vec![]))
    }
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