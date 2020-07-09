use From;

pub fn tokenize(text: &str) -> Vec<Token> {
    text.replace("(", " ( ")
        .replace(")", " ) ")
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
    RParen
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
                ")" => Token::RParen,
                token => Token::Symbol(token.to_owned())
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn tokenizer() {
        let data = "(print 1 (+ 1.1 3e5))";
        let tokens = tokenize(data);
        assert_eq!(tokens, vec![
            Token::LParen,
            Token::Symbol("print".to_owned()), 
            Token::Integer(1),
            Token::LParen,
            Token::Symbol("+".to_owned()),
            Token::Float(1.1),
            Token::Float(3e5),
            Token::RParen,
            Token::RParen
        ]);
    }

}
