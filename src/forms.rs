use super::interpreter::{eval, Env, Error, Exp};
use super::types::{Lambda, Syntax};
use std::rc::Rc;

pub fn define(expression: Vec<Exp>, environment: Rc<Env>) -> Result<Exp, Error> {
    match &expression[0] {
        Exp::Symbol(name) => {
            let value = &expression[1];
            let value = eval(value.clone(), Rc::clone(&environment))?;
            environment.insert(name.clone(), value.clone());
            environment.get(name)
        }
        Exp::List(lambda) => {
            // (define (name arg . args) body1 body2)
            let mut symbols = vec![];
            for exp in lambda {
                match exp {
                    Exp::Symbol(s) => symbols.push(s.to_string()),
                    _ => return Err(Error::SyntaxError("lambda args of define needs to be a symbol".to_string(), Some(exp.clone())))
                }
            }
            let lambda_name = &symbols[0];
            let lambda = Exp::Lambda(Lambda::new(expression[1..].to_vec(), symbols[1..].to_vec(), Rc::clone(&environment)));
            environment.insert(lambda_name.to_string(), lambda);
            environment.get(lambda_name)
        },
        _ => Err(Error::SyntaxError("First arg of define needs to be a symbol".to_string(), Some(expression[0].clone()))),
    }
}

pub fn set(expression: Vec<Exp>, environment: Rc<Env>) -> Result<Exp, Error> {
    match &expression[0] {
        Exp::Symbol(name) => environment.set(name, eval(expression[1].clone(), Rc::clone(&environment))?),
        _ => Err(Error::SyntaxError("First arg of set! needs to be a symbol".to_string(), Some(expression[0].clone()))),
    }
}

pub fn quote(expression: Vec<Exp>, _environment: Rc<Env>) -> Result<Exp, Error> {
    Ok(expression[0].clone())
}

fn is_true(exp: Exp) -> bool {
    match exp {
        Exp::Bool(value) => value,
        _ => true,
    }
}

pub fn if_form(expression: Vec<Exp>, environment: Rc<Env>) -> Result<Exp, Error> {
    let exp = if is_true(eval(expression[0].clone(), Rc::clone(&environment))?) {
        &expression[1]
    } else if let Some(exp) = expression.get(2) {
        exp
    } else {
        &Exp::Bool(false)
    };
    Ok(exp.clone())
}

pub fn lambda(expression: Vec<Exp>, environment: Rc<Env>) -> Result<Exp, Error> {
    let mut arguments = vec![];
    // (lambda (. a) a) is a valid syntax for now
    // equivalent to (lambda a a)
    let (args, body) = expression.split_first().ok_or(Error::SyntaxError("Expecting arguments in lambda definition".to_string(), None))?;
    match args.clone() {
        Exp::List(args) => {
            for arg in args {
                if let Exp::Symbol(arg_symbol) = arg {
                    arguments.push(arg_symbol);
                } else {
                    // sould be only symbols
                    return Err(Error::SyntaxError("Lambda arguments can only be symbols".to_string(), Some(arg.clone())));
                }
            }
        }
        Exp::Symbol(vararg_name) => {
            arguments.push(".".to_string());
            arguments.push(vararg_name);
        }
        _ => return Err(Error::SyntaxError("Lambda argument needs to be a list of symbol or a symbol".to_string(), Some(args.clone()))),
    };
    let lambda = Lambda::new(body.to_vec(), arguments, Rc::clone(&environment));
    Ok(Exp::Lambda(lambda))
}

pub fn define_syntax(expression: Vec<Exp>, environment: Rc<Env>) -> Result<Exp, Error> {
    if let [Exp::Symbol(key), syntax] = &expression[..2] {
        if let Exp::Syntax(transform) = eval(syntax.clone(), Rc::clone(&environment))? {
            environment.insert(key.to_string(), Exp::Syntax(transform));
        } else {
            return Err(Error::SyntaxError("Can only assign syntax transformer".to_string(), None));
        }
        // replace with nil or '()
        Ok(Exp::Bool(true))
    } else {
        Err(Error::SyntaxError("Syntax error in define-syntax".to_string(), None))
    }
}

pub fn syntax_rules(mut expression: Vec<Exp>, environment: Rc<Env>) -> Result<Exp, Error> {
    let mut ellipsis = "...".to_string();
    if let Exp::Symbol(ellipsi_symbol) = &expression[0] {
        ellipsis = ellipsi_symbol.to_string();
        expression = expression[1..].to_vec();
    }
    let (reserved_keywords, transformers) = expression.split_first().ok_or(Error::SyntaxError("Empty syntax-rules declaration".to_string(), None))?;
    // fill keywords vec
    let mut keywords = vec![];
    if let Exp::List(reserved_keywords) = reserved_keywords {
        for keyword in reserved_keywords {
            match keyword {
                Exp::Symbol(keyword) => keywords.push(keyword.clone()),
                _ => return Err(Error::SyntaxError("Syntax keywords need to be symbols".to_string(), Some(keyword.clone()))),
            }
        }
    } else {
        return Err(Error::SyntaxError("Expected list of keywords or empty list".to_string(), Some(reserved_keywords.clone())));
    };
    let mut transforms = vec![];
    for transformer in transformers {
        if let Exp::List(transform) = transformer {
            // refactor slice destructuring
            if transform.len() != 2 {
                return Err(Error::SyntaxError("Syntax rule must be a list (patter template)".to_string(), Some(transformer.clone())));
            }
            let pattern = transform[0].clone();
            let template = transform[1].clone();
            transforms.push((pattern, make_hygenic(template, Rc::clone(&environment), &keywords)));
        } else {
            // not a list form
            return Err(Error::SyntaxError("Syntax rule must be a list (patter template)".to_string(), Some(transformer.clone())));
        }
    }
    let syntax = Syntax::new(transforms, keywords, ellipsis);
    Ok(Exp::Syntax(syntax))
}

// this function replaces symbols with evaluation in the current env
// practically replace all defined symbols by ((lambda () symbol))
fn make_hygenic(template: Exp, environment: Rc<Env>, keywords: &Vec<String>) -> Exp {
    match template {
        Exp::List(expressions) => Exp::List(
            expressions
                .iter()
                .map(|e| make_hygenic(e.clone(), Rc::clone(&environment), &keywords))
                .collect(),
            ),
            Exp::Symbol(symbol) => {
            // creates closure to resolve the symbol to the correct value once expanded in different env
            if !keywords.contains(&symbol) && environment.get(&symbol).is_ok() {
                Exp::List(vec![
                    Exp::Lambda(Lambda::new(
                        vec![Exp::Symbol(symbol)],
                        vec![],
                        Rc::clone(&environment),
                    ))
                ])
            } else {
                Exp::Symbol(symbol)
            }
        }
        constant => constant,
    }
}
