use super::forms::*;
use super::parser::*;
use super::procedures::*;
use super::types::{Lambda, Syntax, Transform};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{self, Debug, Display, Formatter};
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum Exp {
    Int(i64),
    Float(f64),
    Bool(bool),
    Symbol(String),
    List(Vec<Exp>),
    Lambda(Lambda),
    Procedure(Transform),
    SyntaxForm(Transform),
    Syntax(Syntax),
}

impl From<&str> for Exp {
    fn from(s: &str) -> Self {
        Exp::Symbol(s.to_string())
    }
}

// TODO: tests for display
impl Display for Exp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Exp::Int(value) => write!(f, "{}", value),
            Exp::Float(value) => write!(f, "{}", value),
            Exp::Bool(value) => {
                if *value {
                    write!(f, "#t")
                } else {
                    write!(f, "#f")
                }
            }
            Exp::Symbol(value) => write!(f, "{}", value),
            Exp::List(value) => write!(
                f,
                "({})",
                value
                    .iter()
                    .map(Exp::to_string)
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            Exp::Lambda(value) => write!(f, "{}", value),
            Exp::Procedure(value) => write!(f, "#<Proc {:?}>", value),
            Exp::SyntaxForm(value) => write!(f, "#<Proc {:?}>", value),
            Exp::Syntax(value) => write!(f, "{}", value),
        }
    }
}

impl Exp {
    pub fn visit<T>(&self, mut visitor: T)
    where
        T: FnMut(&Exp),
    {
        match &self {
            Exp::List(expressions) => expressions.iter().for_each(|e| visitor(e)),
            exp => visitor(exp),
        }
    }

    // refactor: not used atm
    pub fn visit_mut<T>(&mut self, mut visitor: T)
    where
        T: FnMut(&Exp) -> Exp,
    {
        match &self {
            Exp::List(expressions) => *self = Exp::List(expressions.iter().map(visitor).collect()),
            exp => *self = visitor(exp)
        }
    }

    pub fn get_symbol(&self) -> Option<&String> {
        match &self {
            Exp::Symbol(symbol) => Some(symbol),
            _ => None,
        }
    }

    // refactor, not used, decide whats better between this and "a".into()
    pub fn symbol(symbol: &str) -> Exp {
        Exp::Symbol(symbol.to_string())
    }
}

#[cfg(test)]
mod test {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn test_exp() {
        let mut prog = vec![];
        assert!(exp_from_tokens(&mut prog).is_err());

        let mut prog: Vec<Token> = tokenize("(print 1 (+ 1 2)) ()");
        assert!(exp_from_tokens(&mut prog).is_ok());

        let mut prog: Vec<Token> = tokenize("'(1 '2)");
        assert_eq!(
            exp_from_tokens(&mut prog).unwrap(),
            Exp::List(vec![
                Exp::Symbol("quote".to_string()),
                Exp::List(vec![
                    Exp::Int(1),
                    Exp::List(vec![Exp::Symbol("quote".to_string()), Exp::Int(2)])
                ])
            ])
        );

        let mut prog: Vec<Token> = tokenize("'(test-begin '(R7RS))");
        assert_eq!(
            exp_from_tokens(&mut prog).unwrap(),
            Exp::List(vec![
                Exp::Symbol("quote".to_string()),
                Exp::List(vec![
                    Exp::Symbol("test-begin".to_string()),
                    Exp::List(vec![
                        Exp::Symbol("quote".to_string()),
                        Exp::List(vec![Exp::Symbol("R7RS".to_string())])
                    ])
                ])
            ])
        );

        let mut prog: Vec<Token> = tokenize("`,1");
        assert_eq!(
            exp_from_tokens(&mut prog).unwrap(),
            Exp::List(vec![
                Exp::Symbol("quasiquote".to_string()),
                Exp::List(vec![Exp::Symbol("unquote".to_string()), Exp::Int(1)])
            ])
        );

        let mut prog: Vec<Token> = tokenize("`(1 ,@(2))");
        assert_eq!(
            exp_from_tokens(&mut prog).unwrap(),
            Exp::List(vec![
                Exp::Symbol("quasiquote".to_string()),
                Exp::List(vec![
                    Exp::Int(1),
                    Exp::List(vec![
                        Exp::Symbol("unquote-splicing".to_string()),
                        Exp::List(vec![Exp::Int(2)])
                    ])
                ])
            ])
        );
    }

    #[test]
    fn test_visit() {
        let e = Exp::Int(0);
        e.visit(|exp| assert_eq!(exp, &Exp::Int(0)));
        
        let e = Exp::List(vec!["a".into()]);
        e.visit(|exp| assert_eq!(exp, &Exp::Symbol("a".to_string())));

        
        let mut e = Exp::Int(0);
        e.visit_mut(|_exp| Exp::Int(1));
        assert_eq!(e, Exp::Int(1));


        let mut e = Exp::List(vec!["a".into()]);
        e.visit_mut(|exp| Exp::Symbol(exp.get_symbol().unwrap().clone() + "b"));
        assert_eq!(e, Exp::List(vec!["ab".into()]));
    }
}

pub type EnvMap = HashMap<String, Exp>;

#[derive(Debug)]
pub struct Env {
    parent: Option<Rc<Env>>,
    local: RefCell<EnvMap>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            parent: None,
            local: RefCell::new(EnvMap::new()),
        }
    }

    pub fn new_with_parent(parent: &Rc<Env>) -> Self {
        Env {
            parent: Some(Rc::clone(parent)),
            local: RefCell::new(EnvMap::new()),
        }
    }

    pub fn insert(&self, key: String, value: Exp) -> Option<Exp> {
        self.local.borrow_mut().insert(key, value)
    }

    /// Returns cloned value of the env object
    pub fn get(&self, key: &str) -> Result<Exp, Error> {
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

    pub fn set(&self, key: &str, value: Exp) -> Result<Exp, Error> {
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

#[cfg(test)]
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

pub fn eval(expression: Exp, environment: Rc<Env>) -> Result<Exp, Error> {
    let mut expression = expression;
    let mut environment = environment;
    loop {
        return match expression {
            Exp::List(mut expressions) => {
                // TODO: add some syntax check
                // ex check that the number of exp in the list is coherent for each keyword
                // lookup rust slice pattern, something like [a, b, c] = rest
                // empty list is an error (not equivalent to nil and constant)
                if expressions.len() == 0 {
                    return Err(Error::SyntaxError(
                        "The empty list '() cannot be evaluated".to_string(),
                        None,
                    ));
                }
                let (first, rest) = expressions.split_first_mut().unwrap();
                let mut rest = rest.to_vec();
                match first {
                    // all symbol are resolved during syntax definition
                    // (new-define a b) (define a b) -> (Exp::proc(define) Symbol(a) symbol(b))
                    Exp::SyntaxForm(transform) => {
                        return transform(rest, Rc::clone(&environment));
                    },
                    Exp::Syntax(syntax) => {
                        // replace happens in get template
                        expression = syntax.get_template(Exp::List(rest))?;
                        continue;
                    }
                    Exp::Procedure(proc) => {
                        let mut args = vec![];
                        for value in rest.iter() {
                            args.push(eval(value.clone(), Rc::clone(&environment))?);
                        }
                        expression = proc(args, Rc::clone(&environment))?;
                        continue;
                    }
                    Exp::Lambda(lambda) => {
                        let new_env = Rc::new(Env::new_with_parent(&lambda.environment));
                        // add argument -> eval exp1, exp2.. to local env
                        let mut args_iter = lambda.arguments.split(|symbol| symbol == ".");
                        let args = args_iter.next().unwrap();
                        let (arg_values, vararg_values) = rest.split_at(args.len());
                        for (key, value) in args.iter().zip(arg_values.into_iter()) {
                            new_env.insert(
                                key.to_string(),
                                eval(value.clone(), Rc::clone(&environment))?,
                            );
                        }
                        if let Some(varargs) = args_iter.next() {
                            let vararg_name = varargs.first().ok_or(Error::SyntaxError(
                                "Need an identifier after . in args".to_string(),
                                None,
                            ))?;
                            // evaluate all the exp before passing them
                            let mut values = vec![];
                            for arg in vararg_values.iter() {
                                values.push(eval(arg.clone(), Rc::clone(&environment))?);
                            }
                            new_env.insert(vararg_name.clone(), Exp::List(values));
                        }
                        if args_iter.next().is_some() {
                            return Err(Error::SyntaxError(
                                "Only one identifier allowed after . in args".to_string(),
                                None,
                            ));
                        }
                        // new env is dropped if no reference to it is made
                        if let Some((last, others)) = lambda.body.split_last() {
                            for exp in others {
                                eval(exp.clone(), Rc::clone(&new_env))?;
                            }
                            expression = last.clone();
                            environment = new_env;
                            continue;
                        } else {
                            return Err(Error::SyntaxError("No body in lambda".to_string(), None));
                        }
                    }
                    // Exp::Symbol(symbol @ if_) => {
                    //     Err(Error::NotImplemented)
                    // },
                    Exp::Symbol(symbol) => {
                        // all these should be defined as compiled syntax (and set at interpreter instanciation?)
                        match symbol.as_str() {
                            // todo: begin can be implemented in term of lambda expansion
                            "begin" => {
                                if let Some((last, others)) = rest.split_last() {
                                    for exp in others {
                                        eval(exp.clone(), Rc::clone(&environment))?;
                                    }
                                    expression = last.clone();
                                    continue;
                                } else {
                                    Err(Error::SyntaxError("Begin needs a body".to_string(), None))
                                }
                            }
                            symbol => {
                                let mut exp_list = vec![environment.get(symbol)?];
                                // exp_list.extend_from_slice(rest);
                                exp_list.append(&mut rest);
                                expression = Exp::List(exp_list);
                                continue;
                                // eval(Exp::List(exp_list), environment)
                            } // _ => Err(Error::SyntaxError)
                        }
                    }
                    // Unexcpected token
                    Exp::Float(_) | Exp::Int(_) | Exp::Bool(_) => Err(Error::SyntaxError(
                        "Cannot be evaluated as list first element".to_string(),
                        Some(first.clone()),
                    )),
                    Exp::List(expr) => {
                        // eval expression at the start of the list then eval the whole expression
                        let mut expr_list =
                            vec![eval(Exp::List(expr.to_vec()), Rc::clone(&environment))?];
                        expr_list.append(&mut rest);
                        // extend_from_slice(rest);
                        expression = Exp::List(expr_list);
                        continue;
                        // eval(Exp::List(expr_list), environment)
                    }
                }
            }
            Exp::Symbol(symbol) => environment.get(&symbol),
            constant => Ok(constant), // Exp::Syntax => Err::Syntax, syntactic keyword may not be used as an expression
        };
    } // end of loop
}

#[derive(Debug)]
pub enum Error {
    UndefinedSymbol(String),
    SyntaxError(String, Option<Exp>),
    NotImplemented,
    SyntaxExpansion,
    RuntimeError(String)
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Error::UndefinedSymbol(symbol) => write!(f, "Symbol not defined: {}", symbol),
            Error::SyntaxError(text, Some(exp)) => {
                write!(f, "Syntax error: {} for exp: {}", text, exp)
            }
            Error::SyntaxError(text, None) => write!(f, "Syntax error: {}", text),
            Error::NotImplemented => write!(
                f,
                "No implementation for this case (who knows what 'this' refers to)"
            ),
            Error::SyntaxExpansion => write!(f, "Not enough arguments for syntax expansion"),
            Error::RuntimeError(text) => write!(f, "Runtime error: {}", text)
        }
    }
}

#[derive(Debug)]
pub struct Interpreter {
    pub environment: Rc<Env>,
}

impl Interpreter {
    pub fn new() -> Self {
        // TODO: imports instead of this
        let env = Env::new();
        env.insert("if".to_string(), Exp::SyntaxForm(if_form));
        env.insert("define".to_string(), Exp::SyntaxForm(define));
        env.insert("lambda".to_string(), Exp::SyntaxForm(lambda));
        env.insert("set!".to_string(), Exp::SyntaxForm(set));
        env.insert("quote".to_string(), Exp::SyntaxForm(quote));
        env.insert("define-syntax".to_string(), Exp::SyntaxForm(define_syntax));
        env.insert("syntax-rules".to_string(), Exp::SyntaxForm(syntax_rules));

        env.insert("car".to_string(), Exp::SyntaxForm(car));
        env.insert("cdr".to_string(), Exp::SyntaxForm(cdr));

        env.insert("equal?".to_string(), Exp::Procedure(equal));
        env.insert("+".to_string(), Exp::Procedure(add));
        env.insert("*".to_string(), Exp::Procedure(mul));
        env.insert(">".to_string(), Exp::Procedure(gt));
        env.insert("write".to_string(), Exp::Procedure(write));
        env.insert("dbg".to_string(), Exp::Procedure(dbg));
        env.insert("load".to_string(), Exp::Procedure(load));

        Interpreter {
            environment: Rc::new(env),
        }
    }

    pub fn eval(&mut self, expression: Exp) -> Result<Exp, Error> {
        // println!("{:?}", expression);
        eval(expression, Rc::clone(&self.environment))
    }

    pub fn run(&mut self, code: &str) -> Result<Exp, Error> {
        let expression = parse(code)?;
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

#[cfg(test)]
mod test_interpreter {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn test_run() {
        let mut interpreter = Interpreter::new();
        assert_eq!(interpreter.run("1").unwrap(), Exp::Int(1));
        assert_eq!(interpreter.run("(define a 1)").unwrap(), Exp::Int(1));
        assert_eq!(
            interpreter
                .run_file("(define a 1)\n(set! a 2)\n(set! a 3)")
                .unwrap(),
            Exp::Int(2)
        );
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

        let code = "(if #f 1 0)";
        let res = interpreter.run(code).unwrap();
        assert_eq!(res, Exp::Int(0));

        let code = "(if #f 1)";
        let res = interpreter.run(code);
        assert!(res.is_ok());
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
        assert_eq!(
            res,
            Exp::List(vec![Exp::Symbol("a".to_string()), Exp::Int(11)])
        );
    }

    #[test]
    fn test_quote() {
        let mut interpreter = Interpreter::new();
        let code = "(quote (define a 1))";
        let res = interpreter.run(code).unwrap();
        assert_eq!(
            res,
            Exp::List(vec![
                Exp::Symbol("define".to_string()),
                Exp::Symbol("a".to_string()),
                Exp::Int(1)
            ])
        );
    }

    #[test]
    fn test_begin() {
        let mut interpreter = Interpreter::new();
        let code = "(begin 0 (define a 1) (quote (1 2)) 1)";
        let res = interpreter.run(code).unwrap();
        assert_eq!(res, Exp::Int(1));
    }
}
