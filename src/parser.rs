use From;
use super::interpreter::{Exp, Error};

// TODO: multiline comment #| or expr comment #;
fn remove_comments(text: &str) -> String {
    let mut text = text.to_string();
    while let Some(start_index) = text.find(";") {
        if let Some(offset) = text[start_index..].find("\n") {
            // println!(text[start_index..end_index]);
            text.replace_range(start_index..(start_index + offset), "");
        } else {
            text = text[..start_index].to_string();
        }
    }
    text
}

// TODO: iterator from buffer with () counting to break between exp?
// reading from buffer can also simplify comments handeling
pub fn tokenize(text: &str) -> Vec<Token> {
    let text = remove_comments(text);
    text.replace("(", " ( ")
        .replace("# (", "#(")
        .replace(")", " ) ")
        .replace("'", " ' ")
        .replace("`", " ` ")
        .replace(",@", " ,@ ")
        .replace(",", " , ")
        .replace(", @", ",@")
        .split_whitespace()
        .map(Token::from)
        .collect()
}

#[derive(Debug, PartialEq)]
pub enum Token {
    Integer(i64),
    Float(f64),
    Symbol(String),
    LParen,
    VectorStart,
    RParen,
    Quote,
    Quasiquote,
    Unquote,
    UnquoteSplicing,
}

impl From<&str> for Token {
    fn from(token: &str) -> Self {
        assert!(token.find(" ").is_none());
        if let Ok(value) = token.parse::<i64>() {
            Token::Integer(value)
        } else if let Ok(value) = token.parse::<f64>() {
            Token::Float(value)
        } else {
            match token {
                "(" => Token::LParen,
                "#(" => Token::VectorStart,
                ")" => Token::RParen,
                "'" => Token::Quote,
                "`" => Token::Quasiquote,
                "," => Token::Unquote,
                ",@" => Token::UnquoteSplicing,
                token => Token::Symbol(token.to_owned()),
            }
        }
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
        Token::VectorStart => {
            // refactor parse to produce vectorstart and lparen token
            tokens.insert(0, Token::LParen);
            let vector = exp_from_tokens(&mut tokens)?;
            match vector {
                Exp::List(_) => Ok(Exp::List(vec!["quote".into(), vector])),
                _ => Err(Error::SyntaxError("# vector declaration must be followed by a list".to_string(), Some(vector)))
            }
        },
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

pub fn parse(text: &str) -> Result<Exp, Error> {
    exp_from_tokens(&mut tokenize(text))
}

#[cfg(test)]
mod test {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn test_remove_comments() {
        let text = ";une ligne commentée\n;une ligne commentée\nbla 😀;un commentaéidddre\n rien ici\nthe ;end";
        let result = remove_comments(text);
        assert_eq!(result, "\n\nbla 😀\n rien ici\nthe ".to_string());
    }

    #[test]
    fn tokenizer() {
        let data = "(print 1 (+ 1.1 3e5))";
        let tokens = tokenize(data);
        assert_eq!(
            tokens,
            vec![
                Token::LParen,
                Token::Symbol("print".to_owned()),
                Token::Integer(1),
                Token::LParen,
                Token::Symbol("+".to_owned()),
                Token::Float(1.1),
                Token::Float(3e5),
                Token::RParen,
                Token::RParen
            ]
        );

        let data = "('blop)";
        let tokens = tokenize(data);
        assert_eq!(
            tokens,
            vec![
                Token::LParen,
                Token::Quote,
                Token::Symbol("blop".to_owned()),
                Token::RParen
            ]
        );

        let data = "'`,@(blop)";
        let tokens = tokenize(data);
        assert_eq!(
            tokens,
            vec![
                Token::Quote,
                Token::Quasiquote,
                Token::UnquoteSplicing,
                Token::LParen,
                Token::Symbol("blop".to_owned()),
                Token::RParen
            ]
        );

        let data = "`(a ,b ,@(a b))";
        let tokens = tokenize(data);
        assert_eq!(
            tokens,
            vec![
                Token::Quasiquote,
                Token::LParen,
                Token::Symbol("a".to_owned()),
                Token::Unquote,
                Token::Symbol("b".to_owned()),
                Token::UnquoteSplicing,
                Token::LParen,
                Token::Symbol("a".to_owned()),
                Token::Symbol("b".to_owned()),
                Token::RParen,
                Token::RParen
            ]
        );
    }
}
