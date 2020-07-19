use super::parser::*;
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use std::fmt::{self, Formatter, Display};

#[derive(Debug, Clone, PartialEq)]
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

        
        let mut prog: Vec<Token> = tokenize("(print 1 (+ 1 2)) ()");
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
// TODO: first global env for everyone, overwrite everything no recursive lookup
pub type EnvMap = HashMap<String, Exp>;

#[derive(Debug)]
pub struct Env {
    parent: Option<Rc<Env>>,
    local: RefCell<EnvMap>
}

type EnvRcRefCell = Rc<RefCell<Env>>;

impl Env {
    fn new() -> Self {
        Env {
            parent: None,
            local: RefCell::new(EnvMap::new())
        }
    }

    fn new_with_parent(parent: &Rc<Env>) -> Self {
        Env {
            parent: Some(Rc::clone(parent)),
            local: RefCell::new(EnvMap::new())
        }
    }

    fn insert(&self, key: String, value: Exp) -> Option<Exp> {
        self.local.borrow_mut().insert(key, value)
    }

    /// Returns cloned value of the env object,  
    /// TODO: returns an error if key doens not exists
    ///       with somting like Result<Exp, EnvKeyError>
    fn get(&self, key: &str) -> Result<Exp, Error> {
        let local = self.local.borrow();
        if let Some(value) = local.get(key) {
            Ok(value.clone())
        } else {
            if let Some(parent) = &self.parent {
                parent.get(key)
            } else {
                Err(Error::UndefinedSymbol(key.to_string()))
            }
        }
    }
}
mod test_env {
    use super::*;

    #[test]
    fn test_new() {
        let env = Rc::new(Env::new());

        let _child_env = Env::new_with_parent(&env);
    }

    
    #[test]
    fn test_insert() {
        let env = Rc::new(Env::new());
        let child_env = Env::new_with_parent(&env);

        env.insert("key".to_owned(), Exp::Int(1));
        child_env.insert("key".to_owned(), Exp::Int(1));

    }

    #[test]
    fn test_get() {
        let env = Rc::new(Env::new());
        let child_env = Env::new_with_parent(&env);

        env.insert("key".to_owned(), Exp::Int(0));
        env.insert("parent_key".to_owned(), Exp::Int(0));

        child_env.insert("key".to_owned(), Exp::Int(1));
        child_env.insert("children_key".to_owned(), Exp::Int(0));

        
        assert_eq!(child_env.get("key").unwrap(), Exp::Int(1));
        assert_eq!(env.get("key").unwrap(), Exp::Int(0));
        
        assert_eq!(child_env.get("parent_key").unwrap(), Exp::Int(0));
        assert_eq!(env.get("parent_key").unwrap(), Exp::Int(0));
        
        assert!(env.get("children_key").is_err());
    }
}

// TODO: global builtins env/or tree env structure instead of hasmap/struct wiht parent env to lookup in case
// static global_env
// fn lookeup(local env) -> Exp

fn is_true(exp: Exp) -> bool {
    match exp {
        Exp::Float(value) => value != 0.0,
        Exp::Int(value) => value != 0,
        Exp::Symbol(_) | Exp::List(_) => false
    }
}

// change to result to handle runtime errors
fn eval(expression: Exp, environment: &Env) -> Result<Exp, Error> {
    match expression.clone() {
        Exp::List(expressions) => {
            match expressions[0].clone() {
                // Exp::Lambda(body, arguments) => {
                //     // lambda env is parent env, create local env for body execution, with outer: lambda env
                //     // local scope can be saved, probably needs some reference counting
                //     let new_env = arguments.zip(expression[1..]).collect();
                //     new_env.extend(environment);
                //     eval(body, new_env)
                // },
                Exp::Symbol(symbol) => {
                    match symbol.as_str() {
                        "if" => {
                            let exp = if is_true(eval(expressions[1].clone(), environment)?) {
                                &expressions[2]
                            } else {
                                &expressions[3]
                            };
                            eval(exp.clone(), environment)
                        },
                        "define" => {
                            if let Exp::Symbol(name) = &expressions[1] {
                                let value = &expressions[2];
                                let value = eval(value.clone(), environment)?;
                                environment.insert(name.clone(), value.clone());
                                environment.get(name)
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
                        _ => Ok(expression)
                    }
                },
                // sould be an error, returns only first number of list
                // Unexcpected token
                Exp::Float(_) | Exp::Int(_) => Err(Error::SyntaxError),
                Exp::List(_expr) => Err(Error::NotImplemented)
            }
        },
        Exp::Symbol(symbol) => environment.get(&symbol),
        number => Ok(number)
    }
}

#[derive(Debug)]
pub enum Error {
    UndefinedSymbol(String),
    SyntaxError,
    NotImplemented
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Error::UndefinedSymbol(symbol) => write!(f, "Symbol not defined: {}", symbol),
            Error::SyntaxError => write!(f, "Syntax Error somwhere"),
            Error::NotImplemented => write!(f, "No implementation for this case (who knows what this refers to)")
        }
    }
}

#[derive(Debug)]
pub struct Interpreter {
    environment: Rc<Env>
}

// // todo: populate global env with functions
// pub fn make_std_env() {
//
// }

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            environment: Rc::new(Env::new())
        }
    }

    pub fn eval(&mut self, expression: Exp) -> Result<Exp, Error> {
        // "get_mut panic if more than 2 references to it"
        // see rust implementation in readme (it uses rc for env) or switch to refcell
        eval(expression, &self.environment)
    }

    pub fn run(&mut self, code: &str) -> Result<Exp, Error> {
        let mut tokens = tokenize(code);
        let expression = exp_from_tokens(&mut tokens).unwrap();
        self.eval(expression)
    }
}

mod test_interpreter {
    use super::*;

    #[test]
    fn test_define() {
        let mut interpreter = Interpreter::new();
        let res = interpreter.run("(define a 2)").unwrap();
        assert_eq!(res, Exp::Int(2));

        let env = interpreter.environment;
        let res = env.get("a").unwrap();
        assert_eq!(res, Exp::Int(2));
    }

    #[test]
    fn test_if() {
        let mut interpreter = Interpreter::new();
        let code = "(if 1 1 0)";
        let res = interpreter.run(code).unwrap();
        assert_eq!(res, Exp::Int(1));

        let code = "(if 0 1 0)";
        let res = interpreter.run(code).unwrap();
        assert_eq!(res, Exp::Int(0));

    }
}
