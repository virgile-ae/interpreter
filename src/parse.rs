use crate::{
    ast::{Decl, Expr},
    err_msg,
    error::{Coords, Position},
    token::{Token, TokenType},
};

#[derive(Debug)]
pub(crate) struct Parser {
    tokens: Vec<Token>,
    index: usize,
}

impl Parser {
    pub const fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, index: 0 }
    }

    fn first(&self) -> Result<Token, String> {
        if self.index < self.tokens.len() {
            Ok(self.tokens[self.index].clone())
        } else {
            Err(String::from(EOF_ERROR))
        }
    }

    fn previous(&self) -> Option<&Token> {
        self.tokens.iter().nth(self.index - 1)
    }

    fn consume_first(&mut self) {
        self.index += 1;
    }

    fn consume_token_of_type(&mut self, tok_type: TokenType) -> Result<Token, String> {
        match self.first() {
            Ok(Token {
                r#type,
                value,
                coords,
            }) if r#type == tok_type => {
                self.consume_first();
                Ok(Token::new(r#type, value, coords))
            }
            Ok(Token { value, coords, .. }) => {
                let tok_str = tok_type.to_string();
                err_msg!(coords, "expected `{}`, but got `{}`", tok_str, value)
            }
            Err(e) if e == String::from(EOF_ERROR) => {
                let coords = match self.previous() {
                    Some(Token { coords, .. }) => *coords,
                    None => Coords::new(1, 1),
                };
                let tok_str = tok_type.to_string();
                err_msg!(coords, "expected `{}`, but reached end of file", tok_str)
            }
            Err(_) => unreachable!(),
        }
    }
}

const EOF_ERROR: &'static str = "unexpected end of file";

pub fn parse(tokens: Vec<Token>) -> Result<Vec<Decl>, String> {
    let mut parser = Parser::new(tokens);
    declarations(&mut parser)
}

pub(crate) fn declarations(parser: &mut Parser) -> Result<Vec<Decl>, String> {
    let mut acc = vec![];
    loop {
        match declaration(parser) {
            Ok(x) => acc.push(x),
            Err(e) if e == EOF_ERROR => return Ok(acc),
            Err(e) => return Err(e),
        }
        match parser.consume_token_of_type(TokenType::NewLine) {
            Ok(_) => (),
            Err(_) => return Ok(acc),
        }
    }
}

pub(crate) fn declaration(parser: &mut Parser) -> Result<Decl, String> {
    assignment_declaration(parser).or_else(|_| expression_declaration(parser))
}

pub(crate) fn expression(parser: &mut Parser) -> Result<Expr, String> {
    binary_expression(parser)
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// DECLARATIONS ////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

pub(crate) fn assignment_declaration(parser: &mut Parser) -> Result<Decl, String> {
    // let `id` = `expr`
    // ^^^
    let r#let = parser.consume_token_of_type(TokenType::Let)?;

    // let `id` = `expr`
    //     ^^^^
    let identifier = parser.consume_token_of_type(TokenType::Ident)?;

    // let `id` = `expr`
    //          ^
    parser.consume_token_of_type(TokenType::Eq)?;

    // let `id` = `expr`
    //            ^^^^^^
    let value = expression(parser)?;

    let end = value.position().end;
    Ok(Decl::Assignment {
        id: identifier.value,
        body: Box::new(value),
        position: Position::new(r#let.coords, end),
    })
}

pub(crate) fn expression_declaration(parser: &mut Parser) -> Result<Decl, String> {
    Ok(Decl::Expression(expression(parser)?))
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// BINARY EXPRESSIONS //////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

pub(crate) fn binary_expression(parser: &mut Parser) -> Result<Expr, String> {
    or_binary_expression(parser)
}

macro_rules! binary_expression_parser {
    ($func_name:ident, $below:ident, $tok_type:path, $op_type:ident$(::$rest:ident)* $(,)?) => {
        fn $func_name(parser: &mut Parser) -> Result<Expr, String> {
            let left = $below(parser)?;

            match parser.first() {
                Ok(Token {
                    r#type: $tok_type, ..
                }) => parser.consume_first(),
                Ok(_) => return Ok(left),
                Err(e) if e == String::from(EOF_ERROR) => return Ok(left),
                Err(_) => unreachable!(),
            }

            let right = $func_name(parser)?;
            let left_pos = left.position();
            let right_pos = right.position();
            Ok(Expr::$op_type {
                left: Box::new(left),
                right: Box::new(right),
                position: Position::new(left_pos.start, right_pos.end),
            })
        }
    };
}

// BOOLEAN EXPRESSIONS /////////////////////////////////////////////////////////////////////////////
binary_expression_parser!(
    or_binary_expression,
    and_binary_expression,
    TokenType::Or,
    Or,
);

binary_expression_parser!(
    and_binary_expression,
    comparative_expression,
    TokenType::And,
    And
);

// COMPARATIVE EXPRESSIONS /////////////////////////////////////////////////////////////////////////
macro_rules! consume_right_and_create_node {
    (
        $parser:ident, $left:ident,
        $op_type:ident$(::$op_type_rest:ident)* $(,)?
    ) => {{
        $parser.consume_first();
        let right = modulo_binary_expression($parser)?;
        let (l_pos, r_pos) = ($left.position().start, right.position().end);
        Ok(Expr::$op_type {
            left: Box::new($left),
            right: Box::new(right),
            position: Position::new(l_pos, r_pos),
        })
    }};
}
fn comparative_expression(parser: &mut Parser) -> Result<Expr, String> {
    let left = modulo_binary_expression(parser)?;
    match parser.first() {
        Ok(Token {
            r#type: TokenType::EqEq,
            ..
        }) => consume_right_and_create_node!(parser, left, EqEq),
        Ok(Token {
            r#type: TokenType::Neq,
            ..
        }) => consume_right_and_create_node!(parser, left, Neq),
        Ok(Token {
            r#type: TokenType::Gt,
            ..
        }) => consume_right_and_create_node!(parser, left, Gt),
        Ok(Token {
            r#type: TokenType::Gte,
            ..
        }) => consume_right_and_create_node!(parser, left, Gte),
        Ok(Token {
            r#type: TokenType::Lt,
            ..
        }) => consume_right_and_create_node!(parser, left, Lt),
        Ok(Token {
            r#type: TokenType::Lte,
            ..
        }) => consume_right_and_create_node!(parser, left, Lte),
        Ok(_) => Ok(left),
        Err(e) if e == String::from(EOF_ERROR) => Ok(left),
        Err(_) => unreachable!(),
    }
}

// ARITHMETIC EXPRESSIONS //////////////////////////////////////////////////////////////////////////
binary_expression_parser!(
    modulo_binary_expression,
    minus_binary_expression,
    TokenType::Mod,
    Mod,
);

binary_expression_parser!(
    minus_binary_expression,
    add_binary_expression,
    TokenType::Sub,
    Sub,
);

binary_expression_parser!(
    add_binary_expression,
    star_binary_expression,
    TokenType::Add,
    Add,
);

binary_expression_parser!(
    star_binary_expression,
    divide_binary_expression,
    TokenType::Mul,
    Mul,
);

binary_expression_parser!(
    divide_binary_expression,
    exponent_binary_expression,
    TokenType::Div,
    Div,
);

binary_expression_parser!(
    exponent_binary_expression,
    not_unary_expression,
    TokenType::Pow,
    Pow,
);

////////////////////////////////////////////////////////////////////////////////////////////////////
// UNARY EXPRESSIONS ///////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

macro_rules! unary_expression_parser {
    ($func_name:ident, $below:ident, $tok_type:path, $op_type:ident$(::$rest:ident)* $(,)?) => {
        fn $func_name(parser: &mut Parser) -> Result<Expr, String> {
            let left_pos;
            match parser.first() {
                Ok(Token {
                    r#type: $tok_type,
                    coords,
                    ..
                }) => {
                    left_pos = coords;
                    parser.consume_first();
                }
                Ok(_) => return $below(parser),
                Err(e) if e == String::from(EOF_ERROR) => return Err(e),
                Err(_) => unreachable!(),
            }

            let right = $func_name(parser)?;
            let right_pos = right.position();
            Ok(Expr::$op_type {
                operand: Box::new(right),
                position: Position::new(left_pos, right_pos.end),
            })
        }
    };
}

unary_expression_parser!(
    not_unary_expression,
    minus_unary_expression,
    TokenType::Not,
    Not,
);

unary_expression_parser!(
    minus_unary_expression,
    value_expression,
    TokenType::Sub,
    UnarySub,
);

////////////////////////////////////////////////////////////////////////////////////////////////////
// VALUE EXPRESSIONS ///////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

pub(crate) fn value_expression(parser: &mut Parser) -> Result<Expr, String> {
    match parser.first()? {
        Token {
            r#type: TokenType::LeftParen,
            ..
        } => parenthesized_expression(parser),
        Token {
            r#type: TokenType::Number | TokenType::False | TokenType::True,
            ..
        } => literal_expression(parser),
        Token {
            r#type: TokenType::Ident,
            ..
        } => ident_expression(parser),
        _ => unimplemented!(),
    }
}

pub(crate) fn parenthesized_expression(parser: &mut Parser) -> Result<Expr, String> {
    let Token { coords, .. } = parser.consume_token_of_type(TokenType::LeftParen)?;
    // Check that expr has a body
    match parser.first() {
        Ok(Token {
            r#type: TokenType::RightParen,
            ..
        }) => return err_msg!(coords, "parenthesized expression has no body"),
        Err(_) => {
            return err_msg!(
                coords,
                "reached end of file without closing the parenthesized expression"
            )
        }
        _ => (),
    }
    let body = match expression(parser) {
        Ok(val) => val,
        Err(_) => return err_msg!(coords, "parethesized expression has incorrect contents"),
    };
    parser.consume_token_of_type(TokenType::RightParen)?;
    Ok(body)
}

pub(crate) fn literal_expression(parser: &mut Parser) -> Result<Expr, String> {
    match parser.first()? {
        Token {
            r#type: TokenType::Number,
            value,
            coords: Coords { line, column },
        } => {
            parser.consume_first();
            Ok(Expr::Number {
                val: value.parse::<f64>().unwrap(),
                position: Position::from_tuples(
                    (line, column),
                    (line, column + (value.len() as u16 - 1)),
                ),
            })
        }
        Token {
            r#type: TokenType::True | TokenType::False,
            value,
            coords: Coords { line, column },
        } => {
            parser.consume_first();
            Ok(Expr::Boolean {
                val: value == "true",
                position: Position::from_tuples(
                    (line, column),
                    (line, column + (value.len() as u16 - 1)),
                ),
            })
        }
        Token {
            r#type,
            value,
            coords,
        } => {
            let tok_str = r#type.to_string();
            err_msg!(
                coords,
                "expected literal, got `{}` of type `{}`",
                value,
                tok_str
            )
        }
    }
}

pub(crate) fn ident_expression(parser: &mut Parser) -> Result<Expr, String> {
    let token = parser.consume_token_of_type(TokenType::Ident)?;
    let end_coords = token.end_coords();
    Ok(Expr::Ident {
        val: token.value,
        position: Position::new(token.coords, end_coords),
    })
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// TESTS ///////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    use crate::{parse::*, token};
    #[test]
    fn star_expression() {
        let expected = Ok(Expr::Mul {
            left: Box::new(Expr::Number {
                val: 12.,
                position: Position::from_tuples((1, 1), (1, 2)),
            }),
            right: Box::new(Expr::Number {
                val: 42.,
                position: Position::from_tuples((1, 6), (1, 7)),
            }),
            position: Position::from_tuples((1, 1), (1, 7)),
        });
        let mut input = Parser::new(token::tokenize("12 * 42"));
        assert_eq!(binary_expression(&mut input), expected);
    }

    #[test]
    fn divide_expression() {
        let expected = Ok(Expr::Div {
            left: Box::new(Expr::Number {
                val: 12.,
                position: Position::from_tuples((1, 1), (1, 2)),
            }),
            right: Box::new(Expr::Number {
                val: 42.,
                position: Position::from_tuples((1, 6), (1, 7)),
            }),
            position: Position::from_tuples((1, 1), (1, 7)),
        });
        let mut input = Parser::new(token::tokenize("12 / 42"));
        assert_eq!(binary_expression(&mut input), expected);
    }

    #[test]
    fn minus_expression() {
        let expected = Ok(Expr::Mul {
            left: Box::new(Expr::Number {
                val: 12.,
                position: Position::from_tuples((1, 1), (1, 2)),
            }),
            right: Box::new(Expr::Number {
                val: 42.,
                position: Position::from_tuples((1, 1), (1, 2)),
            }),
            position: Position::from_tuples((1, 1), (1, 12)),
        });
        let mut input = Parser::new(token::tokenize("12 - 42"));
        assert_eq!(binary_expression(&mut input), expected);
    }

    #[test]
    fn multiple_star_expression() {
        let expected = Ok(Expr::Mul {
            left: Box::new(Expr::Number {
                val: 12.,
                position: Position::from_tuples((1, 1), (1, 2)),
            }),
            right: Box::new(Expr::Mul {
                left: Box::new(Expr::Number {
                    val: 42.,
                    position: Position::from_tuples((1, 6), (1, 7)),
                }),
                right: Box::new(Expr::Number {
                    val: 32.,
                    position: Position::from_tuples((1, 11), (1, 12)),
                }),
                position: Position::from_tuples((1, 6), (1, 12)),
            }),
            position: Position::from_tuples((1, 1), (1, 12)),
        });
        let mut input = Parser::new(token::tokenize("12 * 42 * 32"));
        assert_eq!(binary_expression(&mut input), expected);
    }

    #[test]
    fn order_binary_expression() {
        let expected = Ok(Expr::Sub {
            left: Box::new(Expr::Add {
                left: Box::new(Expr::Number {
                    val: 2.,
                    position: Position::from_tuples((1, 1), (1, 1)),
                }),
                right: Box::new(Expr::Number {
                    val: 3.,
                    position: Position::from_tuples((1, 5), (1, 5)),
                }),
                position: Position::from_tuples((1, 1), (1, 5)),
            }),
            right: Box::new(Expr::Mul {
                left: Box::new(Expr::Div {
                    left: Box::new(Expr::Number {
                        val: 4.,
                        position: Position::from_tuples((1, 9), (1, 9)),
                    }),
                    right: Box::new(Expr::Number {
                        val: 5.,
                        position: Position::from_tuples((1, 13), (1, 13)),
                    }),
                    position: Position::from_tuples((1, 9), (1, 13)),
                }),
                right: Box::new(Expr::Number {
                    val: 6.,
                    position: Position::from_tuples((1, 17), (1, 17)),
                }),
                position: Position::from_tuples((1, 13), (1, 17)),
            }),
            position: Position::from_tuples((1, 1), (1, 17)),
        });
        let mut input = Parser::new(token::tokenize("2 + 3 - 4 / 5 * 6")); // should be (2 + 3) - ((4 / 5) * 6)
        let result = binary_expression(&mut input);
        assert_eq!(result, expected);
    }

    #[test]
    fn parenthesized_expression() {
        let input = "(2 + 3) * 3";
        let expected = Ok(Expr::Mul {
            left: Box::new(Expr::Add {
                left: Box::new(Expr::Number {
                    val: 2.,
                    position: Position::from_tuples((1, 2), (1, 2)),
                }),
                right: Box::new(Expr::Number {
                    val: 3.,
                    position: Position::from_tuples((1, 6), (1, 6)),
                }),
                position: Position::from_tuples((1, 1), (1, 7)),
            }),
            right: Box::new(Expr::Number {
                val: 3.,
                position: Position::from_tuples((1, 11), (1, 11)),
            }),
            position: Position::from_tuples((1, 1), (1, 11)),
        });
        let mut input = Parser::new(token::tokenize(input));
        assert_eq!(expression(&mut input), expected);
    }

    #[test]
    fn unary_minus() {
        let input = "- 32 - - 23";
        let expected = Ok(Expr::Sub {
            left: Box::new(Expr::UnarySub {
                operand: Box::new(Expr::Number {
                    val: 32.,
                    position: Position::from_tuples((1, 3), (1, 4)),
                }),
                position: Position::from_tuples((1, 1), (1, 4)),
            }),
            right: Box::new(Expr::UnarySub {
                operand: Box::new(Expr::Number {
                    val: 23.,
                    position: Position::from_tuples((1, 10), (1, 11)),
                }),
                position: Position::from_tuples((1, 8), (1, 11)),
            }),
            position: Position::from_tuples((1, 1), (1, 11)),
        });

        let mut input = Parser::new(token::tokenize(input));
        assert_eq!(expression(&mut input), expected);
    }
}
