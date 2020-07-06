use From;

pub fn tokenize(text: &str) -> Vec<String> {
    text.replace("(", " ( ")
        .replace(")", " ) ")
        .split_whitespace()
        .map(str::to_owned)
        .collect()    
}
    
#[derive(Debug)]
pub enum Exp {
    Int(i64),
    Float(f64),
    Symbol(String),
    List(Vec<Exp>),
    // probably split between tokens and interpreter types
    Lambda(Exp, Vec<Exp::Symbol>)
}

impl From<&str> for Exp {
    fn from(token: &str) -> Self {
        assert!(token.find(" ").is_none());
        if let Ok(value) = token.parse::<i64>() {
            Exp::Int(value)
        } else if let Ok(value) = token.parse::<f64>() {
            Exp::Float(value)
        } else {
            Exp::Symbol(token.to_owned())
        }
    }
}


// TODO: replace vec for generic iterator
pub fn exp_from_str(mut tokens: &mut Vec<String>) -> Result<Exp, ()> {
    if tokens.is_empty() {
        return Err(());
    }

    let token: &str = &tokens.remove(0);
    match token {
        "(" => {
            let mut args = vec![];
            while tokens[0] != ")" {
                args.push(exp_from_str(&mut tokens)?);
            }
            // remove ')'
            tokens.remove(0);
            Ok(Exp::List(args))
        },
        ")" => Err(()),
        token => Ok(Exp::from(token))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn tokenizer() {
        let data = "(print 1 (+ 1 2))";
        let tokens = tokenize(data);
        assert_eq!(tokens, vec!["(", "print", "1", "(", "+", "1", "2", ")", ")"]);
    }

    #[test]
    fn str_to_exp() {
        match Exp::from("12") {
            Exp::Int(value) => assert_eq!(value, 12),
            token => panic!("Should not be something else than int {:?}", token)
        }

        match Exp::from("12.5") {
            Exp::Float(value) => assert_eq!(value, 12.5),
            token => panic!("Should not be something else than float {:?}", token)
        }

        match Exp::from("symbol😀") {
            Exp::Symbol(value) => assert_eq!(value, "symbol😀"),
            token => panic!("Should not be something else than symbol {:?}", token)
        }
    }

    #[test]
    fn exp() {
        let mut prog = vec![];
        assert!(exp_from_str(&mut prog).is_err());

        
        let mut prog: Vec<String> = tokenize("(print 1 (+ 1 2))");
        assert!(exp_from_str(&mut prog).is_ok());
    }
}
