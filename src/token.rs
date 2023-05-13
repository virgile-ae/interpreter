use crate::error::Coords;
use std::vec;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum TokenKind {
    // Keywords
    Let,
    If,
    Else,
    // Value
    Number, // e.g. 420
    Ident,  // e.g. hello_world
    True,   // true
    False,  // false

    // Assignment operator
    Eq,

    // Arithmetic operators
    Div, // /
    Sub, // -
    Mod, // %
    Add, // +
    Mul, // *
    Pow, // **

    // Comparison operators
    EqEq, // ==
    Neq,  // !=
    Gt,   // >
    Gte,  // >=
    Lt,   // <
    Lte,  // <=

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
    Semicolon,    // ;
    Comma,        // ,
    Bar,          // |
    NewLine,
}

impl ToString for TokenKind {
    fn to_string(&self) -> String {
        String::from(match self {
            TokenKind::Let => "let",
            TokenKind::If => "if",
            TokenKind::Else => "else",
            TokenKind::Number => "number",
            TokenKind::Ident => "identifier",
            TokenKind::True => "true",
            TokenKind::False => "false",
            TokenKind::Eq => "=",
            TokenKind::Div => "/",
            TokenKind::Sub => "-",
            TokenKind::Mod => "%",
            TokenKind::Add => "+",
            TokenKind::Mul => "*",
            TokenKind::Pow => "**",
            TokenKind::EqEq => "==",
            TokenKind::Neq => "!=",
            TokenKind::Gt => ">",
            TokenKind::Gte => ">=",
            TokenKind::Lt => "<",
            TokenKind::Lte => "<=",
            TokenKind::And => "and",
            TokenKind::Or => "or",
            TokenKind::Not => "not",
            TokenKind::LeftParen => "(",
            TokenKind::RightParen => ")",
            TokenKind::LeftBracket => "[",
            TokenKind::RightBracket => "]",
            TokenKind::LeftBrace => "{",
            TokenKind::RightBrace => "}",
            TokenKind::NewLine => "new line",
            TokenKind::Semicolon => ";",
            TokenKind::Comma => ",",
            TokenKind::Bar => "|",
        })
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub value: String,
    pub coords: Coords,
}

impl Token {
    pub fn new(kind: TokenKind, value: String, coords: Coords) -> Self {
        Self {
            kind,
            value,
            coords,
        }
    }

    pub fn end_coords(&self) -> Coords {
        Coords::new(
            self.coords.line,
            self.coords.column + self.value.len() as u16 - 1,
        )
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
            column: 0,
        }
    }

    /// Consumes the current character and returns the next.
    fn bump(&mut self) -> Option<char> {
        let top = self.peek()?;
        self.index += 1;
        self.column += 1;
        Some(top)
    }

    /// Look at the current character without consuming it.
    fn peek(&mut self) -> Option<char> {
        self.s.chars().nth(self.index)
    }

    fn rest(&mut self, acc: &mut String, cond: fn(char) -> bool) {
        while let Some(ch) = self.peek() {
            if !cond(ch) {
                break;
            }
            acc.push(ch);
            self.bump();
        }
    }
}

const VALID_IDENTIFIER: fn(char) -> bool = |ch| ch.is_alphanumeric() || ch == '_';

pub fn tokenize(s: &str) -> Vec<Token> {
    let mut acc = vec![];
    let mut tokenizer = Tokenizer::new(s);

    while let Some(ch) = tokenizer.bump() {
        match ch {
            // Whitespace

            // Ignore newlines for the moment
            // '\n' => {
            //     acc.push(Token::new(
            //         TokenKind::NewLine,
            //         "\n".to_string(),
            //         Coords::new(tokenizer.line, tokenizer.column),
            //     ));
            //     tokenizer.column = 0;
            //     tokenizer.line += 1;
            // }
            x if x.is_whitespace() => (),

            ////////////////////////////////////////////////////////////////////////////////////////
            // VALUES //////////////////////////////////////////////////////////////////////////////
            ////////////////////////////////////////////////////////////////////////////////////////

            // Numbers
            x if x.is_numeric() => {
                let (line, column) = (tokenizer.line, tokenizer.column);
                let mut num_acc = x.to_string();
                tokenizer.rest(&mut num_acc, char::is_numeric);
                if tokenizer.peek() == Some('.') {
                    num_acc.push('.');
                    tokenizer.bump();
                    tokenizer.rest(&mut num_acc, char::is_numeric);
                }
                if tokenizer.peek() == Some('e') || tokenizer.peek() == Some('E') {
                    tokenizer.bump();
                    num_acc.push('e');
                    if tokenizer.peek() == Some('-') {
                        num_acc.push('-');
                        tokenizer.bump();
                    } else if tokenizer.peek() == Some('+') {
                        tokenizer.bump();
                    }
                    tokenizer.rest(&mut num_acc, char::is_numeric);
                }
                acc.push(Token::new(
                    TokenKind::Number,
                    num_acc,
                    Coords::new(line, column),
                ));
            }

            // Words (including keywords)
            x if x.is_alphabetic() => {
                let (line, column) = (tokenizer.line, tokenizer.column);
                let mut word_acc = x.to_string();
                tokenizer.rest(&mut word_acc, VALID_IDENTIFIER);

                acc.push(Token::new(
                    match word_acc.as_str() {
                        "true" => TokenKind::True,
                        "false" => TokenKind::False,
                        "and" => TokenKind::And,
                        "or" => TokenKind::Or,
                        "not" => TokenKind::Not,
                        "let" => TokenKind::Let,
                        "if" => TokenKind::If,
                        "else" => TokenKind::Else,
                        _ => TokenKind::Ident,
                    },
                    word_acc,
                    Coords::new(line, column),
                ));
            }

            ////////////////////////////////////////////////////////////////////////////////////////
            // OPERATORS ///////////////////////////////////////////////////////////////////////////
            ////////////////////////////////////////////////////////////////////////////////////////

            // ARITHMETIC OPERATORS ////////////////////////////////////////////////////////////////
            // ** operator
            '*' if tokenizer.peek() == Some('*') => {
                acc.push(Token::new(
                    TokenKind::Pow,
                    "**".to_string(),
                    Coords::new(tokenizer.line, tokenizer.column),
                ));
                tokenizer.bump();
            }
            // * operator
            '*' => acc.push(Token::new(
                TokenKind::Mul,
                "*".to_string(),
                Coords::new(tokenizer.line, tokenizer.column),
            )),
            // / operator
            '/' => acc.push(Token::new(
                TokenKind::Div,
                "/".to_string(),
                Coords::new(tokenizer.line, tokenizer.column),
            )),
            // + operator
            '+' => acc.push(Token::new(
                TokenKind::Add,
                "+".to_string(),
                Coords::new(tokenizer.line, tokenizer.column),
            )),
            // - operator
            '-' => acc.push(Token::new(
                TokenKind::Sub,
                "-".to_string(),
                Coords::new(tokenizer.line, tokenizer.column),
            )),
            // % operator
            '%' => acc.push(Token::new(
                TokenKind::Mod,
                "%".to_string(),
                Coords::new(tokenizer.line, tokenizer.column),
            )),

            // COMPARISON OPERATORS ////////////////////////////////////////////////////////////////
            // == operator
            '=' if tokenizer.peek() == Some('=') => {
                acc.push(Token::new(
                    TokenKind::EqEq,
                    "==".to_string(),
                    Coords::new(tokenizer.line, tokenizer.column),
                ));
                tokenizer.bump();
            }
            // != operator
            '!' if tokenizer.peek() == Some('=') => {
                acc.push(Token::new(
                    TokenKind::Neq,
                    "!=".to_string(),
                    Coords::new(tokenizer.line, tokenizer.column),
                ));
                tokenizer.bump();
            }
            // >= operator
            '>' if tokenizer.peek() == Some('=') => {
                acc.push(Token::new(
                    TokenKind::Gte,
                    ">=".to_string(),
                    Coords::new(tokenizer.line, tokenizer.column),
                ));
                tokenizer.bump();
            }
            // > operator
            '>' => acc.push(Token::new(
                TokenKind::Gt,
                ">".to_string(),
                Coords::new(tokenizer.line, tokenizer.column),
            )),
            // <= operator
            '<' if tokenizer.peek() == Some('=') => {
                acc.push(Token::new(
                    TokenKind::Lte,
                    "<=".to_string(),
                    Coords::new(tokenizer.line, tokenizer.column),
                ));
                tokenizer.bump();
            }
            // < operator
            '<' => acc.push(Token::new(
                TokenKind::Lt,
                "<".to_string(),
                Coords::new(tokenizer.line, tokenizer.column),
            )),

            // ASSIGNMENT OPERATOR /////////////////////////////////////////////////////////////////
            // = operator
            '=' => acc.push(Token::new(
                TokenKind::Eq,
                "=".to_string(),
                Coords::new(tokenizer.line, tokenizer.column),
            )),

            ////////////////////////////////////////////////////////////////////////////////////////
            // DELIMITERS //////////////////////////////////////////////////////////////////////////
            ////////////////////////////////////////////////////////////////////////////////////////
            ';' => acc.push(Token::new(
                TokenKind::Semicolon,
                ";".to_string(),
                Coords::new(tokenizer.line, tokenizer.column),
            )),

            ',' => acc.push(Token::new(
                TokenKind::Comma,
                ",".to_string(),
                Coords::new(tokenizer.line, tokenizer.column),
            )),

            // Parentheses `(` and `)`
            '(' => acc.push(Token::new(
                TokenKind::LeftParen,
                "(".to_string(),
                Coords::new(tokenizer.line, tokenizer.column),
            )),
            ')' => acc.push(Token::new(
                TokenKind::RightParen,
                ")".to_string(),
                Coords::new(tokenizer.line, tokenizer.column),
            )),
            // Braces `{` and `}`
            '{' => acc.push(Token::new(
                TokenKind::LeftBrace,
                "{".to_string(),
                Coords::new(tokenizer.line, tokenizer.column),
            )),
            '}' => acc.push(Token::new(
                TokenKind::RightBrace,
                "}".to_string(),
                Coords::new(tokenizer.line, tokenizer.column),
            )),
            '|' => acc.push(Token::new(
                TokenKind::Bar,
                "|".to_string(),
                Coords::new(tokenizer.line, tokenizer.column),
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
            TokenKind::Number,
            "123".to_string(),
            Coords::new(1, 2),
        )];
        assert_eq!(tokenize(input), expected);

        // Real
        let input = "69.420";
        let expected = vec![Token::new(
            TokenKind::Number,
            "69.420".to_string(),
            Coords::new(1, 1),
        )];
        assert_eq!(tokenize(input), expected);

        // Scientific notation
        let input = "69.42e5";
        let expected = vec![Token::new(
            TokenKind::Number,
            "69.42e5".to_string(),
            Coords::new(1, 1),
        )];
        assert_eq!(tokenize(input), expected);
    }

    #[test]
    fn tokenize_operators() {
        let input = "* + / - ** > >= < <= == = !=";
        let expected = vec![
            Token::new(TokenKind::Mul, "*".to_string(), Coords::new(1, 1)),
            Token::new(TokenKind::Add, "+".to_string(), Coords::new(1, 3)),
            Token::new(TokenKind::Div, "/".to_string(), Coords::new(1, 5)),
            Token::new(TokenKind::Sub, "-".to_string(), Coords::new(1, 7)),
            Token::new(TokenKind::Pow, "**".to_string(), Coords::new(1, 9)),
            Token::new(TokenKind::Gt, ">".to_string(), Coords::new(1, 12)),
            Token::new(TokenKind::Gte, ">=".to_string(), Coords::new(1, 14)),
            Token::new(TokenKind::Lt, "<".to_string(), Coords::new(1, 17)),
            Token::new(TokenKind::Lte, "<=".to_string(), Coords::new(1, 19)),
            Token::new(TokenKind::EqEq, "==".to_string(), Coords::new(1, 22)),
            Token::new(TokenKind::Eq, "=".to_string(), Coords::new(1, 25)),
            Token::new(TokenKind::Neq, "!=".to_string(), Coords::new(1, 27)),
        ];
        assert_eq!(tokenize(input), expected);
    }

    #[test]
    fn tokenize_delimiters() {
        let input = " ( ) { };,";
        let expected = vec![
            Token::new(TokenKind::LeftParen, "(".to_string(), Coords::new(1, 2)),
            Token::new(TokenKind::RightParen, ")".to_string(), Coords::new(1, 4)),
            Token::new(TokenKind::LeftBrace, "{".to_string(), Coords::new(1, 6)),
            Token::new(TokenKind::RightBrace, "}".to_string(), Coords::new(1, 8)),
            Token::new(TokenKind::Semicolon, ";".to_string(), Coords::new(1, 9)),
            Token::new(TokenKind::Comma, ",".to_string(), Coords::new(1, 10)),
        ];
        assert_eq!(tokenize(input), expected);
    }

    #[test]
    fn tokenize_keywords() {
        let input = "true false and or not";
        let expected = vec![
            Token::new(TokenKind::True, "true".to_string(), Coords::new(1, 1)),
            Token::new(TokenKind::False, "false".to_string(), Coords::new(1, 6)),
            Token::new(TokenKind::And, "and".to_string(), Coords::new(1, 12)),
            Token::new(TokenKind::Or, "or".to_string(), Coords::new(1, 16)),
            Token::new(TokenKind::Not, "not".to_string(), Coords::new(1, 19)),
        ];
        assert_eq!(tokenize(input), expected);
    }

    #[test]
    fn tokenize_not_keywords() {
        let input = "truefalseandornot";
        let expected = vec![Token::new(
            TokenKind::Ident,
            "truefalseandornot".to_string(),
            Coords::new(1, 1),
        )];
        assert_eq!(tokenize(input), expected);
    }

    #[test]
    fn tokenize_newline() {
        let input = "\n";
        let expected = vec![Token::new(
            TokenKind::NewLine,
            "\n".to_string(),
            Coords::new(1, 1),
        )];
        assert_eq!(tokenize(input), expected);
    }

    #[test]
    fn tokenize_let_decl() {
        let input = "let a=5";
        let expected = vec![
            Token::new(TokenKind::Let, "let".to_string(), Coords::new(1, 1)),
            Token::new(TokenKind::Ident, "a".to_string(), Coords::new(1, 5)),
            Token::new(TokenKind::Eq, "=".to_string(), Coords::new(1, 6)),
            Token::new(TokenKind::Number, "5".to_string(), Coords::new(1, 7)),
        ];
        assert_eq!(tokenize(input), expected);
    }
}
