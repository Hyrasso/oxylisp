use From;

// TODO: multiline comment #| or expr comment #;
fn remove_comments(text: &str) -> String {
    let mut text = text.to_string();
    while let Some(start_index) = text.find(";") {
        if let Some(offset) = text[start_index..].find("\n") {
            for _ in 0..offset {
                text.remove(start_index);
            }
        } else {
            text = text[start_index..].to_string();
        }
    }
    text
}

pub fn tokenize(text: &str) -> Vec<Token> {
    let text = remove_comments(text);
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

mod test {
    #[allow(unused_imports)]
    use super::*;

    fn test_remove_comments() {
        assert_eq!(
            remove_comments("bla;un commentaire\n rien ici\n;une ligne commentée\nthe ;end"),
            "bla\n rien ici\n\nthe ".to_string()
        );
    }

    #[test]
    fn tokenizer() {
        let data = "(print 1 (+ 1.1 3e5))\n (blop) ";
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
            Token::RParen,
            Token::LParen,
            Token::Symbol("blop".to_owned()),
            Token::RParen
        ]);
    }

}
