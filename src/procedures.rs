use super::interpreter::{Exp, Env, Error};

use std::rc::Rc;

pub fn equal(expression: Vec<Exp>, _environment: Rc<Env>) -> Result<Exp, Error> {
    Ok(Exp::Bool(expression[0] == expression[1]))
}

pub fn add(expression: Vec<Exp>, _environment: Rc<Env>) -> Result<Exp, Error> {
    match expression[..] {
        [Exp::Int(a), Exp::Int(b)] => Ok(Exp::Int(a + b)),
        [Exp::Float(a), Exp::Float(b)] => Ok(Exp::Float(a + b)),
        _ => Err(Error::SyntaxError)
    }
    
}

// rewrite with the port thing and stdout
pub fn write(expression: Vec<Exp>, _environment: Rc<Env>) -> Result<Exp, Error> {
    if expression.len() < 1 {
        println!("{:?}", expression[0]);
        Ok(expression[0].clone())
    } else {
        println!("{:?}", expression);
        Ok(Exp::List(expression))
    }
}

// fn gt

// fn sub   

// fn car

// fn cdr