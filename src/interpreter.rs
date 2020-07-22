use super::parser::*;
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use std::fmt::{self, Formatter, Display, Debug};

// at some point will probably need to replace some vec with linked list, or custom pair
// use std::collections::LinkedList;
// enum pair {some((Rc?Exp, pair))}

#[derive(Clone)]
pub struct Lambda {
    body: Box<Exp>,
    arguments: Vec<String>,
    environment: Rc<Env>
}

impl PartialEq for Lambda {
    fn eq(&self, _other: &Self) -> bool {
        // TODO: in some cases maybe they can be the same?
        false
    }
}

// printing is self refecrencial if lambda is in the parent env
// ex (define f (lambda (x) x)), diplaying f display parent env in which f is defined ...
impl Debug for Lambda {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Lambda {{ body: {:?}, arguments: {:?}, environment: Env }}", self.body, self.arguments)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Exp {
    Int(i64),
    Float(f64),
    Bool(bool),
    Symbol(String),
    List(Vec<Exp>),
    Lambda(Lambda)
    // Builtin(BuiltinProc)
}


pub fn exp_from_tokens(mut tokens: &mut Vec<Token>) -> Result<Exp, Error> {
    if tokens.is_empty() {
        return Err(Error::SyntaxError);
    }

    let token = tokens.remove(0);
    match token {
        Token::LParen => {
            let mut args = vec![];
            while tokens.get(0).ok_or(Error::SyntaxError)? != &Token::RParen {
                args.push(exp_from_tokens(&mut tokens)?);
            }
            // remove ')'
            tokens.remove(0);
            Ok(Exp::List(args))
        },
        Token::Integer(value) => Ok(Exp::Int(value)),
        Token::Float(value) => Ok(Exp::Float(value)),
        Token::Symbol(value) => Ok(Exp::Symbol(value)),
        Token::RParen => Err(Error::SyntaxError)
    }
}


mod test {
    #[allow(unused_imports)]
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


pub type EnvMap = HashMap<String, Exp>;

#[derive(Debug)]
pub struct Env {
    parent: Option<Rc<Env>>,
    local: RefCell<EnvMap>
}

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

    /// Returns cloned value of the env object
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

    // TODO: probably some refactorisation could be done
    fn set(&self, key: &str, value: Exp) -> Result<Exp, Error> {
        // let local = self.local.borrow();
        let mut local = self.local.borrow_mut();
        if local.get(key).is_some() {
            Ok(local.insert(key.to_string(), value).unwrap())
        } else {
            if let Some(parent) = &self.parent {
                parent.set(key, value)
            } else {
                Err(Error::UndefinedSymbol(key.to_string()))
            }
        }
    }
}
mod test_env {
    #[allow(unused_imports)]
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

// TODO: boolean type, needs to type (quote #f) for now
fn is_true(exp: Exp) -> bool {
    match exp {
        Exp::Symbol(symbol) => !(&symbol == "#f"),
        _ => true
    }
}

fn eval(expression: Exp, environment: Rc<Env>) -> Result<Exp, Error> {
    let mut expression = expression;
    let mut environment = environment;
    loop {
    return match expression {
        Exp::List(expressions) => {
            // TODO: add some syntax check 
            // ex check that the number of exp in the list is coherent for each keyword
            // lookup rust slice pattern, something like [a, b, c] = rest
            let (first, rest) = expressions.split_first().unwrap();
            match first {
                Exp::Lambda(lambda) => {
                    let new_env = Rc::new(Env::new_with_parent(&lambda.environment));
                    // add argument -> eval exp1, exp2.. to local env 
                    let mut args_iter = lambda.arguments.split(|symbol| symbol == ".");
                    let args = args_iter.next().unwrap();
                    for (key, value) in args.iter().zip(rest.into_iter()) {
                        new_env.insert(key.to_string(), eval(value.clone(), Rc::clone(&environment))?);
                    }
                    if let Some(varargs) = args_iter.next() {
                        let vararg_name = varargs.first().ok_or(Error::SyntaxError)?;
                        // evaluate all the exp before passing them
                        let mut values = vec![];
                        for arg in rest[args.len()..].iter() {
                            values.push(eval(arg.clone(), Rc::clone(&environment))?);
                        }
                        new_env.insert(vararg_name.clone(), Exp::List(values));
                        if args_iter.next().is_some() {
                            return Err(Error::SyntaxError);
                        }
                    }
                    // new env is dropped if no reference to it is made
                    // implement tail recursion here instead
                    // something like
                    expression = *lambda.body.clone();
                    environment = new_env;
                    continue;
                    // eval(*lambda.body.clone(), new_env)
                },
                // Exp::Symbol(symbol @ "if") => {
                Exp::Symbol(symbol) => {
                    match symbol.as_str() {
                        "if" => {
                            let exp = if is_true(eval(rest[0].clone(), Rc::clone(&environment))?) {
                                &rest[1]
                            } else {
                                &rest[2]
                            };
                            eval(exp.clone(), environment)
                        },
                        "define" => {
                            if let Exp::Symbol(name) = &rest[0] {
                                let value = &rest[1];
                                let value = eval(value.clone(), Rc::clone(&environment))?;
                                environment.insert(name.clone(), value.clone());
                                environment.get(name)
                            } else {
                                Err(Error::SyntaxError)
                            }
                        },
                        "set!" => {
                            if let Exp::Symbol(name) = &rest[0] {
                                environment.set(name, eval(rest[1].clone(), Rc::clone(&environment))?)
                            } else {
                                Err(Error::SyntaxError)
                            }
                        },
                        "quote" => {
                            Ok(rest[0].clone())
                        },
                        "begin" => {
                            if let Some((last, others)) = rest.split_last() {
                                for exp in others {
                                    eval(exp.clone(), Rc::clone(&environment))?;
                                }
                                expression = last.clone();
                                continue;
                            } else {
                                Err(Error::SyntaxError)
                            }
                        },
                        "lambda" => {
                            let mut arguments = vec![];
                            // (lambda (. a) a) is a valid syntax for now
                            // equivalent to (lambda a a)
                            let (args, body) = rest.split_first().unwrap();
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
                            let lambda = Lambda {
                                body: Box::new(Exp::List(body_expand)),
                                arguments,
                                environment: Rc::clone(&environment)
                            };
                            Ok(Exp::Lambda(lambda))
                        }
                        // Exp::Symbol(symbol) if &symbol == "lambda" => {
                        //     let arguments = expression[1];
                        //     let body = expression[2];
                        //     Exp::Lambda(arguments, body)
                            // lambda env is parent env, create local env for body execution, with outer: lambda env
                            // local scope can be saved, probably needs some reference counting
                            // let new_env = arguments.zip(expression[1..]).collect();
                            // new_env.extend(environment);
                            // eval(body, new_env)
                        // },
                        symbol => {
                            let mut exp_list = vec![environment.get(symbol)?];
                            exp_list.extend_from_slice(rest);
                            expression = Exp::List(exp_list);
                            continue;
                            // eval(Exp::List(exp_list), environment)
                        }
                        // _ => Err(Error::SyntaxError)
                    }
                },
                // Unexcpected token
                Exp::Float(_) | Exp::Int(_) | Exp::Bool(_) => Err(Error::SyntaxError),
                Exp::List(expr) => {
                    // eval expression at the start of the list then eval the whole expression
                    let mut expr_list = vec![eval(Exp::List(expr.to_vec()), Rc::clone(&environment))?];
                    expr_list.extend_from_slice(rest);
                    expression = Exp::List(expr_list);
                    continue;
                    // eval(Exp::List(expr_list), environment)
                }
            }
        },
        Exp::Symbol(symbol) => environment.get(&symbol),
        number => Ok(number)
    }
    } // end of loop
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


impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            environment: Rc::new(Env::new())
        }
    }

    pub fn eval(&mut self, expression: Exp) -> Result<Exp, Error> {
        // "get_mut panic if more than 2 references to it"
        // see rust implementation in readme (it uses rc for env) or switch to refcell
        eval(expression, Rc::clone(&self.environment))
    }

    pub fn run(&mut self, code: &str) -> Result<Exp, Error> {
        let mut tokens = tokenize(code);
        let expression = exp_from_tokens(&mut tokens)?;
        self.eval(expression)
    }

    pub fn run_file(&mut self, code: &str) -> Result<Exp, Error> {
        let mut tokens = tokenize(code);
        let mut res = self.eval(exp_from_tokens(&mut tokens)?)?;
        while tokens.len() > 0 {
            res = self.eval(exp_from_tokens(&mut tokens)?)?;
        }
        Ok(res)
    }
}

mod test_interpreter {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn test_run() {
        let mut interpreter = Interpreter::new();
        assert_eq!(interpreter.run("1").unwrap(), Exp::Int(1));
        assert_eq!(interpreter.run("(define a 1)").unwrap(), Exp::Int(1));
        assert_eq!(interpreter.run_file("(define a 1)\n(set! a 2)\n(set! a 3)").unwrap(), Exp::Int(2));
    }

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
    fn test_set() {
        let mut interpreter = Interpreter::new();
        let _res = interpreter.run("(define a 2)").unwrap();
        let res = interpreter.run("(set! a 1)").unwrap();
        assert_eq!(res, Exp::Int(2));
        
        let res = interpreter.run("a").unwrap();
        assert_eq!(res, Exp::Int(1));
    }

    #[test]
    fn test_if() {
        let mut interpreter = Interpreter::new();
        let code = "(if 1 1 0)";
        let res = interpreter.run(code).unwrap();
        assert_eq!(res, Exp::Int(1));

        let code = "(if (quote #f) 1 0)";
        let res = interpreter.run(code).unwrap();
        assert_eq!(res, Exp::Int(0));

    }

    #[test]
    fn test_lambda() {
        let mut interpreter = Interpreter::new();
        let code = "((lambda (x) x) 10)";
        let res = interpreter.run(code).unwrap();
        assert_eq!(res, Exp::Int(10));
        
        let code = "((lambda x x) 10)";
        let res = interpreter.run(code).unwrap();
        assert_eq!(res, Exp::List(vec![Exp::Int(10)]));

        let code = "((lambda (a . x) x) 5 (quote a) 11)";
        let res = interpreter.run(code).unwrap();
        assert_eq!(res, Exp::List(vec![Exp::Symbol("a".to_string()), Exp::Int(11)]));
    }

    #[test]
    fn test_quote() {
        let mut interpreter = Interpreter::new();
        let code = "(quote (define a 1))";
        let res = interpreter.run(code).unwrap();
        assert_eq!(res, Exp::List(vec![Exp::Symbol("define".to_string()), Exp::Symbol("a".to_string()), Exp::Int(1)]));
    }

    #[test]
    fn test_begin() {
        let mut interpreter = Interpreter::new();
        let code = "(begin 0 (define a 1) (quote (1 2)) 1)";
        let res = interpreter.run(code).unwrap();
        assert_eq!(res, Exp::Int(1));
    }
}
