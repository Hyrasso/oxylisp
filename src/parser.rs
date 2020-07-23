use From;

// TODO: multiline comment #| or expr comment #;
fn remove_comments(text: &str) -> String {
    let mut text = text.to_string();
    while let Some(start_index) = text.find(";") {
        if let Some(offset) = text[start_index..].find("\n") {
            // println!(text[start_index..end_index]);
            text.replace_range(start_index..(start_index+offset), "");
        } else {
            text = text[..start_index].to_string();
        }
    }
    text
}

pub fn tokenize(text: &str) -> Vec<Token> {
    let text = remove_comments(text);
    text.replace("(", " ( ")
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
    RParen,
    Quote,
    Quasiquote,
    Unquote,
    UnquoteSplicing
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
                "'" => Token::Quote,
                "`" => Token::Quasiquote,
                "," => Token::Unquote,
                ",@" => Token::UnquoteSplicing,
                token => Token::Symbol(token.to_owned())
            }
        }
    }
}

mod test {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn test_remove_comments() {
        let text = ";une ligne commentée\n;une ligne commentée\nbla 😀;un commentaéidddre\n rien ici\nthe ;end";
        let result = remove_comments(text);
        assert_eq!(
            result,
            "\n\nbla 😀\n rien ici\nthe ".to_string()
        );
    }

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

        let data = "('blop)";
        let tokens = tokenize(data);
        assert_eq!(tokens, vec![
            Token::LParen,
            Token::Quote,
            Token::Symbol("blop".to_owned()),
            Token::RParen
        ]);

        let data = "`(a ,b ,@(a b))";;
        let tokens = tokenize(data);
        assert_eq!(tokens, vec![
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
        ]);
    }
}
