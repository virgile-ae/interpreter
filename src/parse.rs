use crate::{
    ast::Expr,
    err_msg,
    error::{Coords, Span},
    token::{
        Token,
        TokenKind::{self, *},
    },
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

    fn bump(&mut self) {
        self.index += 1;
    }

    fn is_token_of_kind(&mut self, kind: TokenKind) -> bool {
        self.first().map(|tok| tok.kind == kind).unwrap_or(false)
    }

    fn consume_token_of_kind(&mut self, kind: TokenKind) -> Result<Token, String> {
        match self.first() {
            Ok(Token {
                kind,
                value,
                coords,
            }) if kind == kind => {
                self.bump();
                Ok(Token::new(kind, value, coords))
            }
            Ok(Token { value, coords, .. }) => {
                let tok_str = kind.to_string();
                err_msg!(coords, "expected `{}`, but got `{}`", tok_str, value)
            }
            Err(e) if e == String::from(EOF_ERROR) => {
                let coords = match self.previous() {
                    Some(Token { coords, .. }) => *coords,
                    None => Coords::new(1, 1),
                };
                let tok_str = kind.to_string();
                err_msg!(coords, "expected `{}`, but reached end of file", tok_str)
            }
            Err(_) => unreachable!(),
        }
    }

    fn try_consume(&mut self, expected: TokenKind) -> Option<Token> {
        match self.first() {
            Ok(Token {
                kind,
                value,
                coords,
            }) if kind == expected => Some(Token {
                kind,
                value,
                coords,
            }),
            _ => None,
        }
    }
}

const EOF_ERROR: &'static str = "unexpected end of file";

pub fn parse(tokens: Vec<Token>) -> Result<Vec<Expr>, String> {
    let mut parser = Parser::new(tokens);
    exprs(&mut parser)
}

fn many_delimited_by(
    parser: &mut Parser,
    expr: fn(&mut Parser) -> Result<Expr, String>,
    delimiter: TokenKind,
) -> Result<Vec<Expr>, String> {
    let mut acc = vec![];
    loop {
        match expr(parser) {
            Ok(val) => {
                acc.push(val);
                if parser.is_token_of_kind(delimiter) {
                    parser.consume_token_of_kind(delimiter)?;
                } else {
                    return Ok(acc);
                }
            }
            Err(e) if e == EOF_ERROR => return Ok(acc),
            Err(e) => return Err(e),
        }
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// EXPRESSIONS /////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

fn exprs(parser: &mut Parser) -> Result<Vec<Expr>, String> {
    many_delimited_by(parser, expr, Semicolon)
}

fn expr(parser: &mut Parser) -> Result<Expr, String> {
    match parser.first()?.kind {
        If => if_else_expr(parser),
        LeftBrace => block_expr(parser),
        Let => assignment_expr(parser),
        Bar => func_expr(parser),
        _ => binary_expr(parser),
    }
}

// ASSIGNMENT DECLERATION //////////////////////////////////////////////////////////////////////////
fn assignment_expr(parser: &mut Parser) -> Result<Expr, String> {
    // let `id` = `expr`
    // ^^^
    let r#let = parser.consume_token_of_kind(Let)?.coords;

    // let `id` = `expr`
    //     ^^^^
    let identifier = parser.consume_token_of_kind(Ident)?;

    // let `id` = `expr`
    //          ^
    parser.consume_token_of_kind(Eq)?;

    // let `id` = `expr`
    //            ^^^^^^
    let value = expr(parser)?;

    let end = value.position().end;
    Ok(Expr::Assignment {
        id: identifier.value,
        body: Box::new(value),
        span: Span::new(r#let, end),
    })
}

// IF EXPRESSION ///////////////////////////////////////////////////////////////////////////////////
fn if_else_expr(parser: &mut Parser) -> Result<Expr, String> {
    // if `expr` { `block` } else { `block` }
    // ^^
    let r#if = parser.consume_token_of_kind(If)?;

    // if `expr` { `block` } else { `block` }
    //    ^^^^^^
    let cond = Box::new(expr(parser)?);

    // if `expr` { `block` } else { `block` }
    //           ^^^^^^^^^^^
    let on_true = Box::new(block_expr(parser)?);

    // if `expr` { `block` } else { `block` }
    //                       ^^^^
    parser.consume_token_of_kind(Else)?;

    // if `expr` { `block` } else { `block` }
    //                            ^^^^^^^^^^^
    let on_false = Box::new(block_expr(parser)?);

    let right_pos = on_false.position().end;
    Ok(Expr::IfElse {
        cond,
        on_true,
        on_false,
        span: Span::new(r#if.coords, right_pos),
    })
}

// BLOCK EXPRESSION ////////////////////////////////////////////////////////////////////////////////
fn block_expr(parser: &mut Parser) -> Result<Expr, String> {
    // { `exprs` }
    // ^
    let left_brace = parser.consume_token_of_kind(LeftBrace)?;

    // { `exprs` }
    //   ^^^^^^^
    let expressions = exprs(parser)?;

    // { `exprs` }
    //           ^
    let right_brace = parser.consume_token_of_kind(RightBrace)?;

    Ok(Expr::Block {
        exprs: expressions,
        span: Span::new(left_brace.coords, right_brace.coords),
    })
}

// BINARY EXPRESSIONS //////////////////////////////////////////////////////////////////////////////
fn binary_expr(parser: &mut Parser) -> Result<Expr, String> {
    or_expr(parser)
}

macro_rules! binary_expression_parser {
    ($func_name:ident, $below:ident, $tok_type:path, $op_type:ident$(::$rest:ident)* $(,)?) => {
        fn $func_name(parser: &mut Parser) -> Result<Expr, String> {
            let left = $below(parser)?;

            match parser.first() {
                Ok(Token {
                    kind: $tok_type, ..
                }) => parser.bump(),
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
                span: Span::new(left_pos.start, right_pos.end),
            })
        }
    };
}

// BOOLEAN EXPRESSIONS
binary_expression_parser!(or_expr, and_expr, Or, Or,);

binary_expression_parser!(and_expr, comparative_expression, And, And);

// COMPARATIVE EXPRESSIONS
macro_rules! consume_right_and_create_node {
    (
        $parser:ident, $left:ident,
        $op_type:ident$(::$op_type_rest:ident)* $(,)?
    ) => {{
        $parser.bump();
        let right = mod_expr($parser)?;
        let (l_pos, r_pos) = ($left.position().start, right.position().end);
        Ok(Expr::$op_type {
            left: Box::new($left),
            right: Box::new(right),
            span: Span::new(l_pos, r_pos),
        })
    }};
}

fn comparative_expression(parser: &mut Parser) -> Result<Expr, String> {
    let left = mod_expr(parser)?;
    match parser.first() {
        Ok(Token { kind: EqEq, .. }) => consume_right_and_create_node!(parser, left, EqEq),
        Ok(Token { kind: Neq, .. }) => consume_right_and_create_node!(parser, left, Neq),
        Ok(Token { kind: Gt, .. }) => consume_right_and_create_node!(parser, left, Gt),
        Ok(Token { kind: Gte, .. }) => consume_right_and_create_node!(parser, left, Gte),
        Ok(Token { kind: Lt, .. }) => consume_right_and_create_node!(parser, left, Lt),
        Ok(Token { kind: Lte, .. }) => consume_right_and_create_node!(parser, left, Lte),
        Ok(_) => Ok(left),
        Err(e) if e == String::from(EOF_ERROR) => Ok(left),
        Err(_) => unreachable!(),
    }
}

// ARITHMETIC EXPRESSIONS
binary_expression_parser!(mod_expr, sub_expr, Mod, Mod,);

binary_expression_parser!(sub_expr, add_expr, Sub, Sub,);

binary_expression_parser!(add_expr, mul_expr, Add, Add,);

binary_expression_parser!(mul_expr, div_expr, Mul, Mul,);

binary_expression_parser!(div_expr, pow_expr, Div, Div,);

binary_expression_parser!(pow_expr, not_expr, Pow, Pow,);

// UNARY EXPRESSIONS ///////////////////////////////////////////////////////////////////////////////

macro_rules! unary_expression_parser {
    ($func_name:ident, $below:ident, $tok_type:path, $op_type:ident$(::$rest:ident)* $(,)?) => {
        fn $func_name(parser: &mut Parser) -> Result<Expr, String> {
            let left_pos;
            match parser.first() {
                Ok(Token {
                    kind: $tok_type,
                    coords,
                    ..
                }) => {
                    left_pos = coords;
                    parser.bump();
                }
                Ok(_) => return $below(parser),
                Err(e) if e == String::from(EOF_ERROR) => return Err(e),
                Err(_) => unreachable!(),
            }

            let right = $func_name(parser)?;
            let right_pos = right.position();
            Ok(Expr::$op_type {
                operand: Box::new(right),
                span: Span::new(left_pos, right_pos.end),
            })
        }
    };
}

unary_expression_parser!(not_expr, unary_sub_expr, Not, Not);

unary_expression_parser!(unary_sub_expr, value_expr, Sub, UnSub);

// VALUE EXPRESSIONS ///////////////////////////////////////////////////////////////////////////////

fn value_expr(parser: &mut Parser) -> Result<Expr, String> {
    match parser.first()? {
        Token {
            kind: Number | False | True,
            ..
        } => literal_expr(parser),
        Token {
            kind: LeftParen | Ident,
            ..
        } => ident_or_parenthesized_expr_or_call(parser),
        Token { kind, coords, .. } => {
            let str_type = kind.to_string();
            err_msg!(coords, "expected value (e.g. number, boolean), parenthesized expression, or identifier, but got `{}`", str_type)
        }
    }
}

fn ident_or_parenthesized_expr_or_call(parser: &mut Parser) -> Result<Expr, String> {
    let start = match parser.first() {
        Ok(Token {
            kind: LeftParen, ..
        }) => parenthesized_expr(parser),
        Ok(Token { kind: Ident, .. }) => ident_expr(parser),
        _ => unreachable!(),
    }?;

    // If it has a trailing `(`, then it is a function call
    if !parser.is_token_of_kind(LeftParen) {
        Ok(start)
    } else {
        parser.consume_token_of_kind(LeftParen)?;
        let args = many_delimited_by(parser, expr, Comma)?;
        let end = parser.consume_token_of_kind(RightParen)?.coords;
        let start_pos = start.position().start;
        Ok(Expr::Call {
            func: Box::new(start),
            args,
            span: Span::new(start_pos, end),
        })
    }
}

fn func_expr(parser: &mut Parser) -> Result<Expr, String> {
    // | `args` | `body`
    let start = parser.consume_token_of_kind(Bar)?.coords;
    let args = many_delimited_by(parser, ident_expr, Comma)?
        .iter()
        .map(|x| {
            if let Expr::Ident { val, .. } = x {
                val.clone()
            } else {
                unreachable!()
            }
        })
        .collect::<Vec<_>>();
    parser.consume_token_of_kind(Bar)?;
    let body = expr(parser)?;
    let end = body.position().end;
    Ok(Expr::Function {
        args,
        body: Box::new(body),
        span: Span::new(start, end),
    })
}

fn ident_expr(parser: &mut Parser) -> Result<Expr, String> {
    let token = parser.consume_token_of_kind(Ident)?;
    let end_coords = token.end_coords();
    Ok(Expr::Ident {
        val: token.value,
        span: Span::new(token.coords, end_coords),
    })
}

fn parenthesized_expr(parser: &mut Parser) -> Result<Expr, String> {
    let Token { coords, .. } = parser.consume_token_of_kind(LeftParen)?;
    // Check that expr has a body
    match parser.first() {
        Ok(Token {
            kind: RightParen, ..
        }) => return err_msg!(coords, "parenthesized expression has no body"),
        Err(_) => {
            return err_msg!(
                coords,
                "reached end of file without closing the parenthesized expression"
            )
        }
        _ => (),
    }
    let body = match expr(parser) {
        Ok(val) => val,
        Err(e) => {
            return err_msg!(
                coords,
                "parethesized expression has incorrect contents: {}",
                e
            )
        }
    };
    parser.consume_token_of_kind(RightParen)?;
    Ok(body)
}

fn literal_expr(parser: &mut Parser) -> Result<Expr, String> {
    match parser.first()? {
        Token {
            kind: Number,
            value,
            coords: Coords { line, column },
        } => {
            parser.bump();
            Ok(Expr::Number {
                val: value.parse::<f64>().unwrap(),
                span: Span::from_tuples((line, column), (line, column + (value.len() as u16 - 1))),
            })
        }
        Token {
            kind: True | False,
            value,
            coords: Coords { line, column },
        } => {
            parser.bump();
            Ok(Expr::Boolean {
                val: value == "true",
                span: Span::from_tuples((line, column), (line, column + (value.len() as u16 - 1))),
            })
        }
        Token {
            kind: r#type,
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
                span: Span::from_tuples((1, 1), (1, 2)),
            }),
            right: Box::new(Expr::Number {
                val: 42.,
                span: Span::from_tuples((1, 6), (1, 7)),
            }),
            span: Span::from_tuples((1, 1), (1, 7)),
        });
        let mut input = Parser::new(token::tokenize("12 * 42"));
        assert_eq!(binary_expr(&mut input), expected);
    }

    #[test]
    fn divide_expression() {
        let expected = Ok(Expr::Div {
            left: Box::new(Expr::Number {
                val: 12.,
                span: Span::from_tuples((1, 1), (1, 2)),
            }),
            right: Box::new(Expr::Number {
                val: 42.,
                span: Span::from_tuples((1, 6), (1, 7)),
            }),
            span: Span::from_tuples((1, 1), (1, 7)),
        });
        let mut input = Parser::new(token::tokenize("12 / 42"));
        assert_eq!(binary_expr(&mut input), expected);
    }

    #[test]
    fn minus_expression() {
        let expected = Ok(Expr::Sub {
            left: Box::new(Expr::Number {
                val: 12.,
                span: Span::from_tuples((1, 1), (1, 2)),
            }),
            right: Box::new(Expr::Number {
                val: 42.,
                span: Span::from_tuples((1, 6), (1, 7)),
            }),
            span: Span::from_tuples((1, 1), (1, 7)),
        });
        let mut input = Parser::new(token::tokenize("12 - 42"));
        assert_eq!(binary_expr(&mut input), expected);
    }

    #[test]
    fn multiple_star_expression() {
        let expected = Ok(Expr::Mul {
            left: Box::new(Expr::Number {
                val: 12.,
                span: Span::from_tuples((1, 1), (1, 2)),
            }),
            right: Box::new(Expr::Mul {
                left: Box::new(Expr::Number {
                    val: 42.,
                    span: Span::from_tuples((1, 6), (1, 7)),
                }),
                right: Box::new(Expr::Number {
                    val: 32.,
                    span: Span::from_tuples((1, 11), (1, 12)),
                }),
                span: Span::from_tuples((1, 6), (1, 12)),
            }),
            span: Span::from_tuples((1, 1), (1, 12)),
        });
        let mut input = Parser::new(token::tokenize("12 * 42 * 32"));
        assert_eq!(binary_expr(&mut input), expected);
    }

    #[test]
    fn order_binary_expression() {
        let expected = Ok(Expr::Sub {
            left: Box::new(Expr::Add {
                left: Box::new(Expr::Number {
                    val: 2.,
                    span: Span::from_tuples((1, 1), (1, 1)),
                }),
                right: Box::new(Expr::Number {
                    val: 3.,
                    span: Span::from_tuples((1, 5), (1, 5)),
                }),
                span: Span::from_tuples((1, 1), (1, 5)),
            }),
            right: Box::new(Expr::Mul {
                left: Box::new(Expr::Div {
                    left: Box::new(Expr::Number {
                        val: 4.,
                        span: Span::from_tuples((1, 9), (1, 9)),
                    }),
                    right: Box::new(Expr::Number {
                        val: 5.,
                        span: Span::from_tuples((1, 13), (1, 13)),
                    }),
                    span: Span::from_tuples((1, 9), (1, 13)),
                }),
                right: Box::new(Expr::Number {
                    val: 6.,
                    span: Span::from_tuples((1, 17), (1, 17)),
                }),
                span: Span::from_tuples((1, 9), (1, 17)),
            }),
            span: Span::from_tuples((1, 1), (1, 17)),
        });
        let mut input = Parser::new(token::tokenize("2 + 3 - 4 / 5 * 6")); // should be (2 + 3) - ((4 / 5) * 6)
        let result = binary_expr(&mut input);
        assert_eq!(result, expected);
    }

    #[test]
    fn parenthesized_expression() {
        let input = "(2 + 3) * 3";
        let expected = Ok(Expr::Mul {
            left: Box::new(Expr::Add {
                left: Box::new(Expr::Number {
                    val: 2.,
                    span: Span::from_tuples((1, 2), (1, 2)),
                }),
                right: Box::new(Expr::Number {
                    val: 3.,
                    span: Span::from_tuples((1, 6), (1, 6)),
                }),
                span: Span::from_tuples((1, 2), (1, 6)),
            }),
            right: Box::new(Expr::Number {
                val: 3.,
                span: Span::from_tuples((1, 11), (1, 11)),
            }),
            span: Span::from_tuples((1, 2), (1, 11)),
        });
        let mut input = Parser::new(token::tokenize(input));
        assert_eq!(expr(&mut input), expected);
    }

    #[test]
    fn unary_minus() {
        let input = "- 32 - - 23";
        let expected = Ok(Expr::Sub {
            left: Box::new(Expr::UnSub {
                operand: Box::new(Expr::Number {
                    val: 32.,
                    span: Span::from_tuples((1, 3), (1, 4)),
                }),
                span: Span::from_tuples((1, 1), (1, 4)),
            }),
            right: Box::new(Expr::UnSub {
                operand: Box::new(Expr::Number {
                    val: 23.,
                    span: Span::from_tuples((1, 10), (1, 11)),
                }),
                span: Span::from_tuples((1, 8), (1, 11)),
            }),
            span: Span::from_tuples((1, 1), (1, 11)),
        });

        let mut input = Parser::new(token::tokenize(input));
        assert_eq!(expr(&mut input), expected);
    }

    #[test]
    fn let_binding() {
        let input = "let a = 5;";
        let expected = Ok(Expr::Assignment {
            id: String::from("a"),
            body: Box::new(Expr::Number {
                val: 5.,
                span: Span::from_tuples((1, 9), (1, 9)),
            }),
            span: Span::from_tuples((1, 1), (1, 9)),
        });
        let mut input = Parser::new(token::tokenize(input));
        assert_eq!(expr(&mut input), expected);
    }
}
