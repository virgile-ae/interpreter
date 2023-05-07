use crate::{
    ast::*,
    error::{Coords, Position},
    token::{self, Token, TokenType},
};

#[derive(Debug)]
pub(crate) struct Parser {
    tokens: Vec<token::Token>,
    index: usize,
}

impl Parser {
    pub const fn new(tokens: Vec<token::Token>) -> Self {
        Self { tokens, index: 0 }
    }

    fn remaining(&self) -> usize {
        self.tokens.len() - self.index
    }

    fn first(&self) -> Result<token::Token, String> {
        if self.index < self.tokens.len() {
            Ok(self.tokens[self.index].clone())
        } else {
            Err(String::from(EOF_ERROR))
        }
    }

    fn consume_first(&mut self) {
        self.index += 1;
    }

    fn reached_end(&self) -> bool {
        self.index >= self.tokens.len()
    }
}

const EOF_ERROR: &'static str = "unexpected end of file";

pub fn parse(tokens: Vec<token::Token>) -> Result<Node, String> {
    let mut parser = Parser::new(tokens);
    expression(&mut parser)
}

pub(crate) fn expression(parser: &mut Parser) -> Result<Node, String> {
    binary_expression(parser)
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// BINARY EXPRESSIONS //////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

pub(crate) fn binary_expression(parser: &mut Parser) -> Result<Node, String> {
    or_binary_expression(parser)
}

macro_rules! binary_expression_parser {
    ($func_name:ident, $below:ident, $tok_type:path, $op_type:ident$(::$rest:ident)* $(,)?) => {
        fn $func_name(parser: &mut Parser) -> Result<Node, String> {
            let left = $below(parser)?;

            match parser.first() {
                Ok(token::Token {
                    r#type: $tok_type,
                    ..
                }) => parser.consume_first(),
                Ok(_) => return Ok(left),
                Err(e) if e == String::from(EOF_ERROR) => return Ok(left),
                Err(_) => unreachable!(),
            }

            let right = $func_name(parser)?;
            let left_pos = left.position;
            let right_pos = right.position;
            Ok(Node::new(
                NodeType::Operator($op_type$(::$rest)* {
                    left: Box::new(left),
                    right: Box::new(right),
                }),
                Position::new(left_pos.start, right_pos.end),
            ))
        }
    };
}

binary_expression_parser!(
    or_binary_expression,
    and_binary_expression,
    token::TokenType::Or,
    Operator::Or,
);

binary_expression_parser!(
    and_binary_expression,
    comparative_expression,
    token::TokenType::And,
    Operator::And
);

fn comparative_expression(parser: &mut Parser) -> Result<Node, String> {
    let left = modulo_binary_expression(parser)?;
    match parser.first() {
        Ok(token::Token {
            r#type: token::TokenType::Eq,
            ..
        }) => {
            parser.consume_first();
            let right = modulo_binary_expression(parser)?;
            let (l_pos, r_pos) = (left.position.start, right.position.end);
            Ok(Node::new(
                NodeType::Operator(Operator::Eq {
                    left: Box::new(left),
                    right: Box::new(right),
                }),
                Position::new(l_pos, r_pos),
            ))
        }
        Ok(token::Token {
            r#type: token::TokenType::Neq,
            ..
        }) => {
            parser.consume_first();
            let right = modulo_binary_expression(parser)?;
            let (l_pos, r_pos) = (left.position.start, right.position.end);
            Ok(Node::new(
                NodeType::Operator(Operator::Neq {
                    left: Box::new(left),
                    right: Box::new(right),
                }),
                Position::new(l_pos, r_pos),
            ))
        }
        Ok(token::Token {
            r#type: token::TokenType::Gt,
            ..
        }) => {
            parser.consume_first();
            let right = modulo_binary_expression(parser)?;
            let (l_pos, r_pos) = (left.position.start, right.position.end);
            Ok(Node::new(
                NodeType::Operator(Operator::Gt {
                    left: Box::new(left),
                    right: Box::new(right),
                }),
                Position::new(l_pos, r_pos),
            ))
        }
        Ok(token::Token {
            r#type: token::TokenType::Gte,
            ..
        }) => {
            parser.consume_first();
            let right = modulo_binary_expression(parser)?;
            let (l_pos, r_pos) = (left.position.start, right.position.end);
            Ok(Node::new(
                NodeType::Operator(Operator::Gte {
                    left: Box::new(left),
                    right: Box::new(right),
                }),
                Position::new(l_pos, r_pos),
            ))
        }
        Ok(token::Token {
            r#type: token::TokenType::Lt,
            ..
        }) => {
            parser.consume_first();
            let right = modulo_binary_expression(parser)?;
            let (l_pos, r_pos) = (left.position.start, right.position.end);
            Ok(Node::new(
                NodeType::Operator(Operator::Lt {
                    left: Box::new(left),
                    right: Box::new(right),
                }),
                Position::new(l_pos, r_pos),
            ))
        }
        Ok(token::Token {
            r#type: token::TokenType::Lte,
            ..
        }) => {
            parser.consume_first();
            let right = modulo_binary_expression(parser)?;
            let (l_pos, r_pos) = (left.position.start, right.position.end);
            Ok(Node::new(
                NodeType::Operator(Operator::Lte {
                    left: Box::new(left),
                    right: Box::new(right),
                }),
                Position::new(l_pos, r_pos),
            ))
        }
        Ok(_) => Ok(left),
        Err(e) if e == String::from(EOF_ERROR) => Ok(left),
        Err(_) => unreachable!(),
    }
}

binary_expression_parser!(
    modulo_binary_expression,
    minus_binary_expression,
    token::TokenType::Mod,
    Operator::Mod,
);

binary_expression_parser!(
    minus_binary_expression,
    add_binary_expression,
    token::TokenType::Sub,
    Operator::Sub,
);

binary_expression_parser!(
    add_binary_expression,
    star_binary_expression,
    token::TokenType::Add,
    Operator::Add,
);

binary_expression_parser!(
    star_binary_expression,
    divide_binary_expression,
    token::TokenType::Mul,
    Operator::Mul,
);

binary_expression_parser!(
    divide_binary_expression,
    exponent_binary_expression,
    token::TokenType::Div,
    Operator::Div,
);

binary_expression_parser!(
    exponent_binary_expression,
    not_unary_expression,
    token::TokenType::Pow,
    Operator::Pow,
);

////////////////////////////////////////////////////////////////////////////////////////////////////
// UNARY EXPRESSIONS ///////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

macro_rules! unary_expression_parser {
    ($func_name:ident, $below:ident, $tok_type:path, $op_type:ident$(::$rest:ident)* $(,)?) => {
        fn $func_name(parser: &mut Parser) -> Result<Node, String> {
            let left_pos;
            match parser.first() {
                Ok(token::Token {
                    r#type: $tok_type,
                    coordinates,
                    ..
                }) => {
                    left_pos = coordinates;
                    parser.consume_first();
                }
                Ok(_) => return $below(parser),
                Err(e) if e == String::from(EOF_ERROR) => return Err(e),
                Err(_) => unreachable!(),
            }

            let right = $func_name(parser)?;
            let right_pos = right.position;
            Ok(Node::new(
                NodeType::Operator($op_type$(::$rest)*(Box::new(right))),
                Position::new(left_pos, right_pos.end),
            ))
        }
    };
}

unary_expression_parser!(
    not_unary_expression,
    minus_unary_expression,
    token::TokenType::Not,
    Operator::Not,
);

unary_expression_parser!(
    minus_unary_expression,
    value_expression,
    token::TokenType::Sub,
    Operator::UnarySub,
);

////////////////////////////////////////////////////////////////////////////////////////////////////
// VALUE EXPRESSIONS ///////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

pub(crate) fn value_expression(parser: &mut Parser) -> Result<Node, String> {
    match parser.first()? {
        Token {
            r#type: TokenType::LeftParen,
            ..
        } => parenthesized_expression(parser),
        Token {
            r#type: TokenType::Number | TokenType::False | TokenType::True,
            ..
        } => literal_expression(parser),
        _ => unreachable!(),
    }
}

pub(crate) fn parenthesized_expression(parser: &mut Parser) -> Result<Node, String> {
    match parser.first() {
        Ok(token::Token {
            r#type: token::TokenType::LeftParen,
            ..
        }) => parser.consume_first(),
        Ok(val) => return Err(format!("expected left parenthesis `(`, got {:?}", val)),
        Err(e) if e == String::from(EOF_ERROR) => return Err(String::from(EOF_ERROR)),
        Err(_) => unreachable!(),
    }
    let body = match expression(parser) {
        Ok(val) => val,
        Err(_) => return Err(format!("parethesized expression has no body")),
    };
    match parser.first() {
        Ok(token::Token {
            r#type: token::TokenType::RightParen,
            ..
        }) => parser.consume_first(),
        Ok(val) => {
            return Err(format!(
                "unclosed parenthesized expression, expected right parenthesis `)`, got {:?}",
                val
            ))
        }
        Err(e) if e == String::from(EOF_ERROR) => return Err(format!("unclosed parenthesized expression, expected right parenthesis `)`, but reached end of file")),
        Err(_) => unreachable!(),
    }
    Ok(body)
}

pub(crate) fn literal_expression(parser: &mut Parser) -> Result<Node, String> {
    match parser.first()? {
        token::Token {
            r#type: token::TokenType::Number,
            value,
            coordinates: Coords { line, column },
        } => {
            parser.consume_first();
            Ok(Node::new(
                NodeType::Literal(Literal::Number(value.parse::<f64>().unwrap())),
                Position::from_tuples((line, column), (line, column + (value.len() as u16 - 1))),
            ))
        }
        token::Token {
            r#type: token::TokenType::True | token::TokenType::False,
            value,
            coordinates: Coords { line, column },
        } => {
            parser.consume_first();
            Ok(Node::new(
                NodeType::Literal(Literal::Boolean(value == "true")),
                Position::from_tuples((line, column), (line, column + (value.len() as u16 - 1))),
            ))
        }
        token::Token {
            r#type: _type,
            value,
            coordinates,
        } => Err(format!(
            "{:?}:{:?} expected literal, got `{}` of type `{:?}`",
            coordinates.line, coordinates.column, value, _type
        )),
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// TESTS ///////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    use crate::{parse::*, token};
    #[test]
    fn star_expression() {
        let expected: Result<Node, String> = Ok(Node::new(
            NodeType::Operator(Operator::Mul {
                left: Box::new(Node::new(
                    NodeType::Literal(Literal::Number(12.)),
                    Position::from_tuples((1, 1), (1, 2)),
                )),
                right: Box::new(Node::new(
                    NodeType::Literal(Literal::Number(42.)),
                    Position::from_tuples((1, 6), (1, 7)),
                )),
            }),
            Position::from_tuples((1, 1), (1, 7)),
        ));
        let mut input = Parser::new(token::tokenize("12 * 42"));
        assert_eq!(binary_expression(&mut input), expected);
    }

    #[test]
    fn divide_expression() {
        let expected: Result<Node, String> = Ok(Node::new(
            NodeType::Operator(Operator::Div {
                left: Box::new(Node::new(
                    NodeType::Literal(Literal::Number(12.)),
                    Position::from_tuples((1, 1), (1, 2)),
                )),
                right: Box::new(Node::new(
                    NodeType::Literal(Literal::Number(42.)),
                    Position::from_tuples((1, 6), (1, 7)),
                )),
            }),
            Position::from_tuples((1, 1), (1, 7)),
        ));
        let mut input = Parser::new(token::tokenize("12 / 42"));
        assert_eq!(binary_expression(&mut input), expected);
    }

    #[test]
    fn multiple_star_expression() {
        let expected: Result<Node, String> = Ok(Node::new(
            NodeType::Operator(Operator::Mul {
                left: Box::new(Node::new(
                    NodeType::Literal(Literal::Number(12.)),
                    Position::from_tuples((1, 1), (1, 2)),
                )),
                right: Box::new(Node::new(
                    NodeType::Operator(Operator::Mul {
                        left: Box::new(Node::new(
                            NodeType::Literal(Literal::Number(42.)),
                            Position::from_tuples((1, 6), (1, 7)),
                        )),
                        right: Box::new(Node::new(
                            NodeType::Literal(Literal::Number(32.)),
                            Position::from_tuples((1, 11), (1, 12)),
                        )),
                    }),
                    Position::from_tuples((1, 6), (1, 12)),
                )),
            }),
            Position::from_tuples((1, 1), (1, 12)),
        ));
        let mut input = Parser::new(token::tokenize("12 * 42 * 32"));
        assert_eq!(binary_expression(&mut input), expected);
    }

    #[test]
    fn minus_expression() {
        let expected: Result<Node, String> = Ok(Node::new(
            NodeType::Operator(Operator::Sub {
                left: Box::new(Node::new(
                    NodeType::Literal(Literal::Number(12.)),
                    Position::from_tuples((1, 1), (1, 2)),
                )),
                right: Box::new(Node::new(
                    NodeType::Literal(Literal::Number(42.)),
                    Position::from_tuples((1, 6), (1, 7)),
                )),
            }),
            Position::from_tuples((1, 1), (1, 7)),
        ));
        let mut input = Parser::new(token::tokenize("12 - 42"));
        assert_eq!(binary_expression(&mut input), expected);
    }

    #[test]
    fn order_binary_expression() {
        let expected: Result<Node, String> = Ok(Node::new(
            NodeType::Operator(Operator::Sub {
                left: Node::new_boxed(
                    NodeType::Operator(Operator::Add {
                        left: Node::new_boxed(
                            NodeType::Literal(Literal::Number(2.)),
                            Position::from_tuples((1, 1), (1, 1)),
                        ),
                        right: Node::new_boxed(
                            NodeType::Literal(Literal::Number(3.)),
                            Position::from_tuples((1, 5), (1, 5)),
                        ),
                    }),
                    Position::from_tuples((1, 1), (1, 5)),
                ),
                right: Node::new_boxed(
                    NodeType::Operator(Operator::Mul {
                        left: Node::new_boxed(
                            NodeType::Operator(Operator::Div {
                                left: Node::new_boxed(
                                    NodeType::Literal(Literal::Number(4.)),
                                    Position::from_tuples((1, 9), (1, 9)),
                                ),
                                right: Node::new_boxed(
                                    NodeType::Literal(Literal::Number(5.)),
                                    Position::from_tuples((1, 13), (1, 13)),
                                ),
                            }),
                            Position::from_tuples((1, 9), (1, 13)),
                        ),
                        right: Node::new_boxed(
                            NodeType::Literal(Literal::Number(6.)),
                            Position::from_tuples((1, 17), (1, 17)),
                        ),
                    }),
                    Position::from_tuples((1, 9), (1, 17)),
                ),
            }),
            Position::from_tuples((1, 1), (1, 17)),
        ));
        let mut input = Parser::new(token::tokenize("2 + 3 - 4 / 5 * 6")); // should be (2 + 3) - ((4 / 5) * 6)
        let result = binary_expression(&mut input);
        assert_eq!(result, expected);
    }

    #[test]
    fn parenthesized_expression() {
        let input = "(2 + 3) * 3";
        let expected = Ok(Node::new(
            NodeType::Operator(Operator::Mul {
                left: Node::new_boxed(
                    NodeType::Operator(Operator::Add {
                        left: Node::new_boxed(
                            NodeType::Literal(Literal::Number(2.)),
                            Position::from_tuples((1, 2), (1, 2)),
                        ),
                        right: Node::new_boxed(
                            NodeType::Literal(Literal::Number(3.)),
                            Position::from_tuples((1, 6), (1, 6)),
                        ),
                    }),
                    Position::from_tuples((1, 2), (1, 6)),
                ),
                right: Node::new_boxed(
                    NodeType::Literal(Literal::Number(3.)),
                    Position::from_tuples((1, 11), (1, 11)),
                ),
            }),
            Position::from_tuples((1, 2), (1, 11)),
        ));
        assert_eq!(parse(token::tokenize(input)), expected);
    }

    #[test]
    fn unary_minus() {
        let input = "- 32 - - 23";
        let expected = Ok(Node::new(
            NodeType::Operator(Operator::Sub {
                left: Node::new_boxed(
                    NodeType::Operator(Operator::UnarySub(Node::new_boxed(
                        NodeType::Literal(Literal::Number(32.)),
                        Position::from_tuples((1, 3), (1, 4)),
                    ))),
                    Position::from_tuples((1, 1), (1, 4)),
                ),
                right: Node::new_boxed(
                    NodeType::Operator(Operator::UnarySub(Node::new_boxed(
                        NodeType::Literal(Literal::Number(23.)),
                        Position::from_tuples((1, 10), (1, 11)),
                    ))),
                    Position::from_tuples((1, 8), (1, 11)),
                ),
            }),
            Position::from_tuples((1, 1), (1, 11)),
        ));

        assert_eq!(parse(token::tokenize(input)), expected);
    }
}
