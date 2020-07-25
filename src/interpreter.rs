use super::forms::*;
use super::parser::*;
use super::procedures::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::From;
use std::fmt::{self, Debug, Display, Formatter};
use std::rc::Rc;

// at some point will probably need to replace some vec with linked list, or custom pair
// use std::collections::LinkedList;
// enum pair {some((Rc?Exp, pair))}

#[derive(Clone)]
pub struct Lambda {
    body: Vec<Exp>,
    arguments: Vec<String>,
    environment: Rc<Env>,
}

impl Lambda {
    pub fn new(body: Vec<Exp>, arguments: Vec<String>, environment: Rc<Env>) -> Self {
        Lambda {
            body,
            arguments,
            environment,
        }
    }
}

impl PartialEq for Lambda {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

// printing is self refecrencial if lambda is in the parent env
// ex (define f (lambda (x) x)), diplaying f display parent env in which f is defined ...
impl Debug for Lambda {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "Lambda {{ body: {:?}, arguments: {:?}, environment: Env }}",
            self.body, self.arguments
        )
    }
}

type Transform = fn(Vec<Exp>, Rc<Env>) -> Result<Exp, Error>;

#[derive(Debug, Clone)]
pub struct SyntaxForm {
    transform: Transform,
    env: Rc<Env>,
}

// TODO: unit tests for matches, expand...
#[derive(Debug, Clone)]
pub struct Syntax {
    // Vec pattern template
    transforms: Vec<(Exp, Exp)>,
    keywords: Vec<String>,
    ellipsis: String,
}

impl PartialEq for Syntax {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

impl Syntax {
    pub fn new(transforms: Vec<(Exp, Exp)>, keywords: Vec<String>, ellipsis: String) -> Self {
        Self {
            transforms,
            keywords,
            ellipsis,
        }
    }

    pub fn get_template(&self, expression: Exp) -> Result<Exp, Error> {
        for (pattern, template) in self.transforms.iter() {
            let mut env: HashMap<String, Vec<Exp>> = HashMap::new();
            if let Exp::List(pattern) = pattern {
                if self.matches(
                    // skip first
                    Exp::List(pattern[1..].to_vec()),
                    expression.clone(),
                    &mut env,
                )? {
                    return self.expand(template.clone(), &mut env, 0, true);
                }
            } else {
                return Err(Error::SyntaxError(
                    "Pattern needs to be a list".to_string(),
                    Some(pattern.clone()),
                ));
            }
        }
        // no match found
        Err(Error::SyntaxError(
            "No match found".to_string(),
            Some(expression.clone()),
        ))
    }

    fn matches(
        &self,
        pattern: Exp,
        expression: Exp,
        mut env: &mut HashMap<String, Vec<Exp>>,
    ) -> Result<bool, Error> {
        match &pattern {
            Exp::Symbol(symbol) => {
                if symbol == "_" {
                    Ok(true)
                // match keyword
                } else if self.keywords.contains(symbol) {
                    if let Exp::Symbol(exp) = &expression {
                        Ok(exp == symbol)
                    } else {
                        Ok(false)
                    }
                } else {
                    // push to vec
                    if let Some(stack) = env.get_mut(symbol) {
                        stack.push(expression);
                    } else {
                        env.insert(symbol.to_string(), vec![expression]);
                    }
                    Ok(true)
                }
            }
            Exp::List(patterns) => {
                if let Exp::List(expressions) = expression {
                    // () match ()
                    if patterns.len() == 0 {
                        return Ok(expressions.len() == 0);
                    // (p) match (e), calls p match e, see above
                    } else if patterns.len() == 1 {
                        if expressions.len() != 1 {
                            return Ok(false);
                        }
                        return self.matches(patterns[0].clone(), expressions[0].clone(), &mut env);
                    } else if let Some(index) = patterns[1..]
                        .iter()
                        .position(|e| e == &Exp::Symbol(self.ellipsis.clone()))
                    {
                        let ellipsis_index = index + 1;
                        // (body ...) also match ()
                        if expressions.len() == 0
                            && patterns.len() == 2
                            && patterns[1] == Exp::Symbol(self.ellipsis.clone())
                        {
                            return Ok(true);
                        }
                        // (body1 body2 ...) (e1 e2)
                        if expressions.len() < patterns.len() {
                            return Ok(false);
                        }
                        // zip (p1 p2 ... p-2 p-1) (e1 e2 e3 e4 e5 e6) -> (e1 p1) (e2 p2)
                        for (exp, pattern) in
                            expressions[..ellipsis_index].iter().zip(patterns.iter())
                        {
                            if !self.matches(pattern.clone(), exp.clone(), &mut env)? {
                                return Ok(false);
                            }
                        }
                        let tail_count = patterns.len() - (ellipsis_index + 1);
                        let ellipsed_count = expressions.len() - ellipsis_index - tail_count;
                        if ellipsed_count > 0 {
                            // (p1 p2 ... p-2 p-1) (e1 e2 e3 e4 e5 e6) -> (e3 p2) (e4 p2)
                            for exp in
                                expressions[ellipsis_index..ellipsis_index + ellipsed_count].iter()
                            {
                                if !self.matches(
                                    patterns[ellipsis_index - 1].clone(),
                                    exp.clone(),
                                    &mut env,
                                )? {
                                    return Ok(false);
                                }
                            }
                        }
                        if tail_count > 0 {
                            // (p1 p2 ... p-2 p-1) (e1 e2 e3 e4 e5 e6) -> (e5 p-2) (e6 p-1)
                            for (exp, pattern) in expressions[expressions.len() - tail_count..]
                                .iter()
                                .zip(patterns[patterns.len() - tail_count..].iter())
                            {
                                if !self.matches(pattern.clone(), exp.clone(), &mut env)? {
                                    return Ok(false);
                                }
                            }
                        }
                    } else if patterns.len() == expressions.len() {
                        // no ellipsis YAY!
                        for (pattern, expression) in patterns.iter().zip(expressions.iter()) {
                            if !self.matches(pattern.clone(), expression.clone(), &mut env)? {
                                return Ok(false);
                            };
                        }
                    } else {
                        return Ok(false);
                        // println!("Matching {:?} with {:?}", pattern, expressions);
                        // return Err(Error::SyntaxError(
                        //     "Pattern must match in size with expression or contain ...".to_string(),
                        //     Some(pattern.clone()),
                        // ));
                    }
                    // everything matched
                    Ok(true)
                } else {
                    Ok(false)
                }
            }
            constant => Ok(constant == &expression),
        }
    }

    fn expand(
        &self,
        template: Exp,
        env: &HashMap<String, Vec<Exp>>,
        ellipsis_index: usize,
        expand_ellipsis: bool,
    ) -> Result<Exp, Error> {
        match template {
            Exp::Symbol(symbol) => {
                if let Some(exp) = env.get(&symbol) {
                    // is never an empty vec
                    exp.get(ellipsis_index)
                        .map(Exp::clone)
                        .ok_or(Error::SyntaxExpansion)
                } else {
                    Ok(Exp::Symbol(symbol))
                }
            }
            Exp::List(expressions) => {
                if expressions.len() == 0 {
                    return Ok(Exp::List(vec![]));
                }
                // (body ...) matched ()
                if expressions.len() == 2 && expressions[1] == Exp::Symbol(self.ellipsis.clone()) {
                    return Ok(Exp::List(vec![]));
                }

                // (<ellipsis> pattern) is expanded to: pattern , treating <ellipsis> inside as literal
                if expressions[0] == Exp::Symbol(self.ellipsis.clone()) {
                    return self.expand(expressions[1].clone(), &env, 0, false);
                }
                let mut expanded_expression = vec![];
                for (i, expression) in expressions.iter().enumerate() {
                    if expression == &mut Exp::Symbol(self.ellipsis.clone())
                        && i > 0
                        && expand_ellipsis
                    {
                        for ellipsis_index in 0.. {
                            match self.expand(
                                expressions[i - 1].clone(),
                                &env,
                                ellipsis_index,
                                expand_ellipsis,
                            ) {
                                Err(Error::SyntaxExpansion) => break, // no more exp to expand
                                Ok(exp) => expanded_expression.push(exp), // add matching exp
                                Err(error) => return Err(error),      // stop expansion
                            }
                        }
                    } else {
                        expanded_expression.push(self.expand(
                            expression.clone(),
                            &env,
                            0,
                            expand_ellipsis,
                        )?);
                    }
                }
                Ok(Exp::List(expanded_expression))
            }
            identity => Ok(identity),
        }
    }
}

mod test_syntax {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn test_matches() {
        let syntax = Syntax::new(
            Vec::new(),
            vec!["keyword".to_string()],
            "<ellipsis>".to_string(),
        );
        // vec![
        //         (Exp::List(vec![Exp::Int(0)]), Exp::Int(0)),
        //         (
        //             Exp::List(vec![Exp::Symbol("ident".to_string())]),
        //             Exp::Int(2),
        //         ),
        //         (
        //             Exp::List(vec![Exp::Symbol("keyword".to_string())]),
        //             Exp::Int(3),
        //         ),
        //         (
        //             Exp::List(vec![Exp::List(vec![
        //                 Exp::Symbol("_".to_string()),
        //                 Exp::Symbol("ident".to_string()),
        //                 Exp::Symbol("keyword".to_string()),
        //             ])]),
        //             Exp::Int(4),
        //         ),
        //         (
        //             Exp::List(vec![Exp::List(vec![
        //                 Exp::Symbol("_".to_string()),
        //                 Exp::Symbol("ident".to_string()),
        //                 Exp::Symbol("<ellipsis>".to_string()),
        //                 Exp::Symbol("keyword".to_string()),
        //             ])]),
        //             Exp::Int(5),
        //         ),
        //         (Exp::List(vec![Exp::Symbol("_".to_string())]), Exp::Int(1)),

        // constant
        match syntax.matches(Exp::Int(0), Exp::Int(0), &mut HashMap::new()) {
            Ok(true) => (),
            Ok(false) => panic!("Should match"),
            Err(e) => panic!("{:?}", e),
        }
        match syntax.matches(Exp::Int(0), Exp::Int(1), &mut HashMap::new()) {
            Ok(true) => panic!("Should not match"),
            Ok(false) => (),
            Err(e) => panic!("{:?}", e),
        }

        // _
        match syntax.matches(
            Exp::Symbol("_".to_string()),
            Exp::Int(0),
            &mut HashMap::new(),
        ) {
            Ok(true) => (),
            Ok(false) => panic!("Should match"),
            Err(e) => panic!("{:?}", e),
        }
        match syntax.matches(
            Exp::Symbol("_".to_string()),
            Exp::List(vec![Exp::Symbol("test".to_string())]),
            &mut HashMap::new(),
        ) {
            Ok(true) => (),
            Ok(false) => panic!("Should match"),
            Err(e) => panic!("{:?}", e),
        }

        // keyword
        match syntax.matches(
            Exp::Symbol("keyword".to_string()),
            Exp::Symbol("keyword".to_string()),
            &mut HashMap::new(),
        ) {
            Ok(true) => (),
            Ok(false) => panic!("Should match"),
            Err(e) => panic!("{:?}", e),
        }
        match syntax.matches(
            Exp::Symbol("keyword".to_string()),
            Exp::Symbol("not-keyword".to_string()),
            &mut HashMap::new(),
        ) {
            Ok(false) => (),
            Ok(true) => panic!("Should not match"),
            Err(e) => panic!("{:?}", e),
        }
        match syntax.matches(
            Exp::Symbol("keyword".to_string()),
            Exp::Int(0),
            &mut HashMap::new(),
        ) {
            Ok(false) => (),
            Ok(true) => panic!("Should not match"),
            Err(e) => panic!("{:?}", e),
        }

        // identifier
        match syntax.matches(
            Exp::Symbol("ident".to_string()),
            Exp::Int(0),
            &mut HashMap::new(),
        ) {
            Ok(true) => (),
            Ok(false) => panic!("Should match"),
            Err(e) => panic!("{:?}", e),
        }
        let mut env = HashMap::new();
        match syntax.matches(
            Exp::Symbol("ident".to_string()),
            Exp::List(vec![Exp::Symbol("test".to_string())]),
            &mut env,
        ) {
            Ok(true) => (),
            Ok(false) => panic!("Should match"),
            Err(e) => panic!("{:?}", e),
        }
        assert_eq!(
            env["ident"][0],
            Exp::List(vec![Exp::Symbol("test".to_string())])
        );

        // List
        let mut env = HashMap::new();
        match syntax.matches(
            Exp::List(vec![
                Exp::Symbol("_".to_string()),
                Exp::Symbol("ident".to_string()),
                Exp::Symbol("keyword".to_string()),
            ]),
            Exp::List(vec![
                Exp::Int(0),
                Exp::Int(1),
                Exp::Symbol("keyword".to_string()),
            ]),
            &mut env,
        ) {
            Ok(true) => (),
            Ok(false) => panic!("Should match"),
            Err(e) => panic!("{:?}", e),
        }
        assert_eq!(env["ident"][0], Exp::Int(1));
        match syntax.matches(
            Exp::List(vec![
                Exp::Symbol("_".to_string()),
                Exp::Symbol("ident".to_string()),
                Exp::Symbol("keyword".to_string()),
            ]),
            Exp::List(vec![Exp::Symbol("test".to_string())]),
            &mut env,
        ) {
            Ok(false) => (),
            Ok(true) => panic!("Should not match"),
            Err(e) => panic!("{:?}", e),
        }

        // ellipsis
        // TODO: (a ...), (a ... b), (a (b c) ...)
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum Exp {
    Int(i64),
    Float(f64),
    Bool(bool),
    Symbol(String),
    List(Vec<Exp>),
    Lambda(Lambda),
    Procedure(fn(Vec<Exp>, Rc<Env>) -> Result<Exp, Error>),
    SyntaxForm(fn(Vec<Exp>, Rc<Env>) -> Result<Exp, Error>),
    Syntax(Syntax),
}

impl From<Lambda> for Exp {
    fn from(lambda: Lambda) -> Exp {
        Exp::Lambda(lambda)
    }
}

pub fn exp_from_tokens(mut tokens: &mut Vec<Token>) -> Result<Exp, Error> {
    if tokens.is_empty() {
        return Err(Error::SyntaxError("No tokens to process".to_string(), None));
    }

    let token = tokens.remove(0);
    match token {
        Token::LParen => {
            let mut args = vec![];
            while tokens.get(0).ok_or(Error::SyntaxError(
                "Missing closing paren".to_string(),
                None,
            ))? != &Token::RParen
            {
                args.push(exp_from_tokens(&mut tokens)?);
            }
            // remove ')'
            tokens.remove(0);
            Ok(Exp::List(args))
        }
        Token::Integer(value) => Ok(Exp::Int(value)),
        Token::Float(value) => Ok(Exp::Float(value)),
        Token::Symbol(value) => {
            if &value == "#f" {
                Ok(Exp::Bool(false))
            } else if &value == "#t" {
                Ok(Exp::Bool(true))
            } else {
                Ok(Exp::Symbol(value))
            }
        }
        Token::RParen => Err(Error::SyntaxError(
            "Unexpected closing paren".to_string(),
            None,
        )),
        // Lots of refactor todo around here
        quoting @ Token::Quote
        | quoting @ Token::Unquote
        | quoting @ Token::UnquoteSplicing
        | quoting @ Token::Quasiquote => {
            let quoted = exp_from_tokens(&mut tokens)?;
            let quote_symbol = match quoting {
                Token::Quote => Exp::Symbol("quote".to_string()),
                Token::Quasiquote => Exp::Symbol("quasiquote".to_string()),
                Token::Unquote => Exp::Symbol("unquote".to_string()),
                Token::UnquoteSplicing => Exp::Symbol("unquote-splicing".to_string()),
                _ => unreachable!(),
            };
            Ok(Exp::List(vec![quote_symbol, quoted]))
        }
    }
}

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
                // empty list is equivalent to nil and constant?
                if expressions.len() == 0 {
                    return Ok(Exp::List(vec![]));
                }
                let (first, rest) = expressions.split_first_mut().unwrap();
                let mut rest = rest.to_vec();
                match first {
                    // Exp::Syntax(Syntax) = {
                    //     unimplemented!();
                    // },
                    // all symbol are resolved during syntax definition
                    // (new-define a b) (define a b) -> (Exp::proc(define) Symbol(a) symbol(b))
                    Exp::SyntaxForm(transform) => transform(rest, Rc::clone(&environment)),
                    Exp::Syntax(syntax) => {
                        // replace happens in get template
                        expression = syntax.get_template(Exp::List(rest))?;
                        continue;
                    }
                    Exp::Procedure(proc) => {
                        let mut args = vec![];
                        // drain?
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
                        // implement tail recursion here instead
                        // something like
                        // lambda body is like begin, eval all return last with tail recurstion
                        // then replace begin by syntax
                        // change lambda body to vec of exp
                        // tail recurse on last, this way works the same when only one
                        // see begin for implementation
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
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Error::UndefinedSymbol(symbol) => write!(f, "Symbol not defined: {}", symbol),
            Error::SyntaxError(text, Some(exp)) => {
                write!(f, "Syntax error: {} for exp: {:?}", text, exp)
            }
            Error::SyntaxError(text, None) => write!(f, "Syntax error: {}", text),
            Error::NotImplemented => write!(
                f,
                "No implementation for this case (who knows what this refers to)"
            ),
            Error::SyntaxExpansion => write!(f, "Not enough arguments for syntax expansion"),
        }
    }
}

#[derive(Debug)]
pub struct Interpreter {
    environment: Rc<Env>,
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

        env.insert("equal?".to_string(), Exp::Procedure(equal));
        env.insert("+".to_string(), Exp::Procedure(add));
        env.insert("*".to_string(), Exp::Procedure(mul));
        env.insert(">".to_string(), Exp::Procedure(gt));
        env.insert("write".to_string(), Exp::Procedure(write));

        Interpreter {
            environment: Rc::new(env),
        }
    }

    pub fn eval(&mut self, expression: Exp) -> Result<Exp, Error> {
        // println!("{:?}", expression);
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
