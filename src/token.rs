use crate::error::Coords;
use std::vec;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum TokenType {
    // Value
    Number,     // e.g. 420
    Identifier, // e.g. hello_world

    // Keywords
    True,  // true
    False, // false

    // Arithmetic operators
    Div, // /
    Sub, // -
    Mod, // %
    Add, // +
    Mul, // *
    Pow, // **

    // Comparison operators
    Eq,  // ==
    Neq, // !=
    Gt,  // >
    Gte, // >=
    Lt,  // <
    Lte, // <=

    // Boolean operators
    And, // and
    Or,  // or
    Not, // not

    // Delimiters
    LeftParen,    // (
    RightParen,   // )
    LeftBracket,  // [
    RightBracket, // ]
    LeftBrace,    // {
    RightBrace,   // }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct Token {
    pub r#type: TokenType,
    pub value: String,
    pub coordinates: Coords,
}

impl Token {
    fn new(r#type: TokenType, value: String, coordinates: Coords) -> Self {
        Self {
            r#type,
            value,
            coordinates,
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
struct Tokenizer<'a> {
    s: &'a str,
    index: usize,
    line: u16,
    column: u16,
}

impl<'a> Tokenizer<'a> {
    fn new(s: &'a str) -> Self {
        Self {
            s,
            index: 0,
            line: 1,
            column: 1,
        }
    }

    /// Consumes the current character and returns the next.
    fn next(&mut self) -> Option<char> {
        let top = self.peek()?;
        self.index += 1;
        if top == '\n' {
            self.line += 1;
            self.column = 0;
        } else {
            self.column += 1;
        }
        Some(top)
    }

    /// Current character (hasn't been consumed yet).
    fn peek(&mut self) -> Option<char> {
        self.s.chars().nth(self.index)
    }

    fn matches(&self, s: String) -> bool {
        self.s
            .chars()
            .skip(self.index)
            .collect::<String>()
            .starts_with(&s)
    }
}

macro_rules! rest {
    ($tokenizer:ident, $acc:ident, $cond:ident $(::$cond_path:ident)*) => {
        while let Some(ch) = $tokenizer.peek() {
            if !$cond$(::$cond_path)*(ch) {
                break;
            }
            $acc.push(ch);
            $tokenizer.next();
        }
    };
}

pub fn tokenize(s: &str) -> Vec<Token> {
    let mut acc = vec![];
    let mut tokenizer = Tokenizer::new(s);

    while let Some(ch) = tokenizer.next() {
        match ch {
            // Whitespace
            x if x.is_whitespace() => (),

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // VALUES //////////////////////////////////////////////////////////////////////////////////////////////////
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            // Numbers
            x if x.is_numeric() => {
                let (line, column) = (tokenizer.line, tokenizer.column - 1);
                let mut num_acc = x.to_string();
                rest!(tokenizer, num_acc, char::is_numeric);
                if tokenizer.peek() == Some('.') {
                    num_acc.push('.');
                    tokenizer.next();
                    rest!(tokenizer, num_acc, char::is_numeric);
                }
                if tokenizer.peek() == Some('e') || tokenizer.peek() == Some('E') {
                    num_acc.push('e');
                    tokenizer.next();
                    if tokenizer.peek() == Some('-') {
                        num_acc.push('-');
                        tokenizer.next();
                    }
                    rest!(tokenizer, num_acc, char::is_numeric);
                }
                acc.push(Token::new(
                    TokenType::Number,
                    num_acc,
                    Coords::new(line, column),
                ));
            }

            // Words (including keywords)
            x if x.is_alphabetic() => {
                let (line, column) = (tokenizer.line, tokenizer.column - 1);
                let mut word_acc = x.to_string();
                rest!(tokenizer, word_acc, char::is_alphanumeric);

                acc.push(Token::new(
                    match word_acc.as_str() {
                        "true" => TokenType::True,
                        "false" => TokenType::False,
                        "and" => TokenType::And,
                        "or" => TokenType::Or,
                        "not" => TokenType::Not,
                        _ => TokenType::Identifier,
                    },
                    word_acc,
                    Coords::new(line, column),
                ));
            }

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // OPERATORS ///////////////////////////////////////////////////////////////////////////////////////////////
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            // ARITHMETIC OPERATORS ////////////////////////////////////////////////////////////////////////////////////
            // ** operator
            '*' if tokenizer.peek() == Some('*') => {
                acc.push(Token::new(
                    TokenType::Pow,
                    "**".to_string(),
                    Coords::new(tokenizer.line, tokenizer.column - 1),
                ));
                tokenizer.next();
            }

            // * operator
            '*' => acc.push(Token::new(
                TokenType::Mul,
                "*".to_string(),
                Coords::new(tokenizer.line, tokenizer.column - 1),
            )),

            // / operator
            '/' => acc.push(Token::new(
                TokenType::Div,
                "/".to_string(),
                Coords::new(tokenizer.line, tokenizer.column - 1),
            )),

            // + operator
            '+' => acc.push(Token::new(
                TokenType::Add,
                "+".to_string(),
                Coords::new(tokenizer.line, tokenizer.column - 1),
            )),

            // - operator
            '-' => acc.push(Token::new(
                TokenType::Sub,
                "-".to_string(),
                Coords::new(tokenizer.line, tokenizer.column - 1),
            )),

            // % operator
            '%' => acc.push(Token::new(
                TokenType::Mod,
                "%".to_string(),
                Coords::new(tokenizer.line, tokenizer.column - 1),
            )),

            // COMPARISON OPERATORS ////////////////////////////////////////////////////////////////////////////////////
            // == operator
            '=' if tokenizer.peek() == Some('=') => {
                acc.push(Token::new(
                    TokenType::Eq,
                    "==".to_string(),
                    Coords::new(tokenizer.line, tokenizer.column - 1),
                ));
                tokenizer.next();
            }
            // != operator
            '!' if tokenizer.peek() == Some('=') => {
                acc.push(Token::new(
                    TokenType::Neq,
                    "!=".to_string(),
                    Coords::new(tokenizer.line, tokenizer.column - 1),
                ));
                tokenizer.next();
            }
            // >= operator
            '>' if tokenizer.peek() == Some('=') => {
                acc.push(Token::new(
                    TokenType::Gte,
                    ">=".to_string(),
                    Coords::new(tokenizer.line, tokenizer.column - 1),
                ));
                tokenizer.next();
            }
            // > operator
            '>' => acc.push(Token::new(
                TokenType::Gt,
                ">".to_string(),
                Coords::new(tokenizer.line, tokenizer.column - 1),
            )),

            // <= operator
            '<' if tokenizer.peek() == Some('=') => {
                acc.push(Token::new(
                    TokenType::Lte,
                    "<=".to_string(),
                    Coords::new(tokenizer.line, tokenizer.column - 1),
                ));
                tokenizer.next();
            }
            // < operator
            '<' => acc.push(Token::new(
                TokenType::Lt,
                "<".to_string(),
                Coords::new(tokenizer.line, tokenizer.column - 1),
            )),

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // DELIMITERS //////////////////////////////////////////////////////////////////////////////////////////////
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            // Parentheses `(` and `)`
            '(' => acc.push(Token::new(
                TokenType::LeftParen,
                "(".to_string(),
                Coords::new(tokenizer.line, tokenizer.column - 1),
            )),
            ')' => acc.push(Token::new(
                TokenType::RightParen,
                ")".to_string(),
                Coords::new(tokenizer.line, tokenizer.column - 1),
            )),
            _ => unimplemented!(),
        }
    }

    acc
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenize_numbers() {
        // Natural
        let input = " 123";
        let expected = vec![Token::new(
            TokenType::Number,
            "123".to_string(),
            Coords::new(1, 2),
        )];
        assert_eq!(tokenize(input), expected);

        // Real
        let input = "69.420";
        let expected = vec![Token::new(
            TokenType::Number,
            "69.420".to_string(),
            Coords::new(1, 1),
        )];
        assert_eq!(tokenize(input), expected);

        // Scientific notation
        let input = "69.42e5";
        let expected = vec![Token::new(
            TokenType::Number,
            "69.42e5".to_string(),
            Coords::new(1, 1),
        )];
        assert_eq!(tokenize(input), expected);
    }

    #[test]
    fn tokenize_operators() {
        let input = "* + / - ** > >= < <=";
        let expected = vec![
            Token::new(TokenType::Mul, "*".to_string(), Coords::new(1, 1)),
            Token::new(TokenType::Add, "+".to_string(), Coords::new(1, 3)),
            Token::new(TokenType::Div, "/".to_string(), Coords::new(1, 5)),
            Token::new(TokenType::Sub, "-".to_string(), Coords::new(1, 7)),
            Token::new(TokenType::Pow, "**".to_string(), Coords::new(1, 9)),
            Token::new(TokenType::Gt, ">".to_string(), Coords::new(1, 12)),
            Token::new(TokenType::Gte, ">=".to_string(), Coords::new(1, 14)),
            Token::new(TokenType::Lt, "<".to_string(), Coords::new(1, 17)),
            Token::new(TokenType::Lte, "<=".to_string(), Coords::new(1, 19)),
        ];
        assert_eq!(tokenize(input), expected);
    }

    #[test]
    fn tokenize_delimiters() {
        let input = " ( )";
        let expected = vec![
            Token::new(TokenType::LeftParen, "(".to_string(), Coords::new(1, 2)),
            Token::new(TokenType::RightParen, ")".to_string(), Coords::new(1, 4)),
        ];
        assert_eq!(tokenize(input), expected);
    }

    #[test]
    fn tokenize_keywords() {
        let input = "true false and or not";
        let expected = vec![
            Token::new(TokenType::True, "true".to_string(), Coords::new(1, 1)),
            Token::new(TokenType::False, "false".to_string(), Coords::new(1, 6)),
            Token::new(TokenType::And, "and".to_string(), Coords::new(1, 12)),
            Token::new(TokenType::Or, "or".to_string(), Coords::new(1, 16)),
            Token::new(TokenType::Not, "not".to_string(), Coords::new(1, 19)),
        ];
        assert_eq!(tokenize(input), expected);
    }

    #[test]
    fn tokenize_not_keywords() {
        let input = "truefalseandornot";
        let expected = vec![Token::new(
            TokenType::Identifier,
            "truefalseandornot".to_string(),
            Coords::new(1, 1),
        )];
        assert_eq!(tokenize(input), expected);
    }
}