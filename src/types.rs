use crate::interpreter::{Env, Exp, Error};
use std::{fmt::{Formatter, Debug, self}, rc::Rc, collections::HashMap};
use fmt::Display;

// at some point will probably need to replace some vec with linked list, or custom pair
// use std::collections::LinkedList;
// enum pair {some((Rc?Exp, pair))}

#[derive(Clone)]
pub struct Lambda {
    pub body: Vec<Exp>,
    pub arguments: Vec<String>,
    pub environment: Rc<Env>,
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

impl Display for Lambda {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "#<Procedure ({})>", // TODO: find a way to differentiate between procs
            self.arguments.join(" ")
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

impl Display for Syntax {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "#<Syntactic keyword: THIS SHOULD NOT BE HAPPENING>")
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
