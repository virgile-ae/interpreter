use std::collections::HashMap;

use crate::ast::{Literal, Node, NodeType, Operator};

#[derive(Debug, PartialEq)]
pub enum Types {
    Bool(bool),
    Null,
    Num(f64),
    Record(HashMap<String, Box<Types>>),
    Str(String),
}

pub fn evaluate(node: Node) -> Result<Types, String> {
    expression(node)
}

fn expression(node: Node) -> Result<Types, String> {
    match *node.value {
        NodeType::Operator(ref op) => match op {
            Operator::Add { .. } => add_expr(node),
            Operator::And { .. } => and_expr(node),
            Operator::Div { .. } => div_expr(node),
            Operator::Mod { .. } => mod_expr(node),
            Operator::Mul { .. } => mul_expr(node),
            Operator::Not(_) => not_expr(node),
            Operator::Or { .. } => or_expr(node),
            Operator::Pow { .. } => pow_expr(node),
            Operator::Sub { .. } => sub_expr(node),
            Operator::UnarySub(_) => unary_sub_expr(node),
            Operator::Eq { .. } => eq_expr(node),
            Operator::Neq { .. } => neq_expr(node),
            Operator::Gt { .. } => gt_expr(node),
            Operator::Gte { .. } => gte_expr(node),
            Operator::Lt { .. } => lt_expr(node),
            Operator::Lte { .. } => lte_expr(node),
        },
        NodeType::Literal(ref _literal) => literal_expr(node),
        _ => unreachable!(),
    }
}
////////////////////////////////////////////////////////////////////////////////////////////////////
/// UNARY EXPRESSIONS
macro_rules! create_unary_expr {
    (
        $func_name:ident,
        $op:ident$(::$op_rest:ident)*,
        $err_str:literal,
        $type:ident$(::$type_rest:ident)*,
        $func:ident$(::$func_rest:ident)* $(,)?
    ) => {
        fn $func_name(node: Node) -> Result<Types, String> {
            if let NodeType::Operator($op$(::$op_rest)* (body)) = *node.value {
                let body = expression(*body)?;
                match body {
                    $type$(::$type_rest)*(body) => Ok($type$(::$type_rest)*($func$(::$func_rest)*(body))),
                    body => Err(format!(
                        "unsupported operand for {} operator: `{:?}`",
                        $err_str, body
                    )),
                }
            } else {
                unreachable!()
            }
        }
    };
}

const NOT: fn(bool) -> bool = |x| !x;
create_unary_expr!(not_expr, Operator::Not, "`not`", Types::Bool, NOT);

const UNARY_SUB: fn(f64) -> f64 = |x| -x;
create_unary_expr!(
    unary_sub_expr,
    Operator::UnarySub,
    "`-`",
    Types::Num,
    UNARY_SUB
);

////////////////////////////////////////////////////////////////////////////////////////////////////
/// BINARY EXPRESSIONS

macro_rules! create_binary_expr {
    (
        $func_name:ident,
        $op:ident$(::$op_rest:ident)*,
        $err_str:literal,
        $param_type:ident$(::$param_type_rest:ident)*,
        $return_type:ident$(::$return_rest:ident)*,
        $func:ident$(::$func_rest:ident)* $(,)?
    ) => {
        fn $func_name(node: Node) -> Result<Types, String> {
            if let NodeType::Operator($op$(::$op_rest)* { left, right }) = *node.value {
                let left = expression(*left)?;
                let right = expression(*right)?;
                match (left, right) {
                    ($param_type$(::$param_type_rest)*(left), $param_type$(::$param_type_rest)*(right)) =>
                        Ok($return_type$(::$return_rest)*($func$(::$func_rest)*(left,right))),

                    (left, $param_type$(::$param_type_rest)*(_)) => Err(format!(
                        "unsupported lhs operand for {} operator: `{:?}`",
                        $err_str, left
                    )),
                    ($param_type$(::$param_type_rest)*(_), right) => Err(format!(
                        "unsupported rhs operand for {} operator: `{:?}`",
                        $err_str, right
                    )),
                    (left, right) => Err(format!(
                        "unsupported operands for {} operator: `{:?}` and `{:?}`",
                        $err_str, left, right
                    )),
                }
            } else {
                unreachable!()
            }
        }
    };
    (
        $func_name:ident,
        $op:ident$(::$op_rest:ident)*,
        $err_str:literal,
        $param_type:ident$(::$param_type_rest:ident)*,
        $func:ident$(::$func_rest:ident)* $(,)?
    ) => {
        create_binary_expr!(
            $func_name,
            $op$(::$op_rest)*,
            $err_str,
            $param_type$(::$param_type_rest)*,
            $param_type$(::$param_type_rest)*,
            $func$(::$func_rest)*
        );
    };
}

const OR: fn(bool, bool) -> bool = |l, r| l || r;
create_binary_expr!(or_expr, Operator::Or, "`or`", Types::Bool, OR);

const AND: fn(bool, bool) -> bool = |l, r| l && r;
create_binary_expr!(and_expr, Operator::And, "`and`", Types::Bool, AND);

const EQ: fn(f64, f64) -> bool = |l, r| l == r;
create_binary_expr!(eq_expr, Operator::Eq, "`==`", Types::Num, Types::Bool, EQ);

const NEQ: fn(f64, f64) -> bool = |l, r| l != r;
create_binary_expr!(
    neq_expr,
    Operator::Neq,
    "`!=`",
    Types::Num,
    Types::Bool,
    NEQ
);

const GT: fn(f64, f64) -> bool = |l, r| l > r;
create_binary_expr!(gt_expr, Operator::Gt, "`>`", Types::Num, Types::Bool, GT);

const GTE: fn(f64, f64) -> bool = |l, r| l >= r;
create_binary_expr!(
    gte_expr,
    Operator::Gte,
    "`>=`",
    Types::Num,
    Types::Bool,
    GTE
);

const LT: fn(f64, f64) -> bool = |l, r| l < r;
create_binary_expr!(lt_expr, Operator::Lt, "`<`", Types::Num, Types::Bool, LT);

const LTE: fn(f64, f64) -> bool = |l, r| l <= r;
create_binary_expr!(
    lte_expr,
    Operator::Lte,
    "`<=`",
    Types::Num,
    Types::Bool,
    LTE
);

const MOD: fn(f64, f64) -> f64 = |l, r| l.rem_euclid(r);
create_binary_expr!(mod_expr, Operator::Mod, "`%`", Types::Num, MOD);

const SUB: fn(f64, f64) -> f64 = |l, r| l - r;
create_binary_expr!(sub_expr, Operator::Sub, "`-`", Types::Num, SUB);

const ADD: fn(f64, f64) -> f64 = |l, r| l + r;
create_binary_expr!(add_expr, Operator::Add, "`+`", Types::Num, ADD);

const MUL: fn(f64, f64) -> f64 = |l, r| l * r;
create_binary_expr!(mul_expr, Operator::Mul, "`*`", Types::Num, MUL,);

const DIV: fn(f64, f64) -> f64 = |l, r| l / r;
create_binary_expr!(div_expr, Operator::Div, "`/`", Types::Num, DIV);

const POW: fn(f64, f64) -> f64 = |l, r| l.powf(r);
create_binary_expr!(pow_expr, Operator::Pow, "`**`", Types::Num, POW);

////////////////////////////////////////////////////////////////////////////////////////////////////
/// LITERAL EXPRESSIONS
fn literal_expr(node: Node) -> Result<Types, String> {
    if let NodeType::Literal(val) = *node.value {
        match val {
            Literal::Number(n) => Ok(Types::Num(n)),
            Literal::Boolean(b) => Ok(Types::Bool(b)),
        }
    } else {
        panic!("literal_expr should receive Literal, but got {:?}", node);
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
/// TESTS
#[cfg(test)]
mod tests {
    use crate::{evaluate::*, parse, token::*};

    #[test]
    fn num_literal() {
        // Integer
        let expected = 5.;
        let input = String::from("5");
        assert_eq!(
            expression(parse::parse(tokenize(&input)).unwrap()),
            Ok(Types::Num(expected))
        );

        // Real
        let expected = 5.4;
        let input = String::from("5.4");
        assert_eq!(
            expression(parse::parse(tokenize(&input)).unwrap()),
            Ok(Types::Num(expected))
        );
        // Scientific notation
        let expected = 5.4e-5;
        let input = String::from("5.4e-5");
        assert_eq!(
            expression(parse::parse(tokenize(&input)).unwrap()),
            Ok(Types::Num(expected))
        );
    }

    #[test]
    fn bool_literal() {
        let expected = true;
        let input = String::from("true");
        assert_eq!(
            expression(parse::parse(tokenize(&input)).unwrap()),
            Ok(Types::Bool(expected))
        );
    }

    #[test]
    fn star_expression() {
        let expected = 60.;
        let input = String::from("5 * 12");
        assert_eq!(
            expression(parse::parse(tokenize(&input)).unwrap()),
            Ok(Types::Num(expected))
        );
    }

    #[test]
    fn divide_expression() {
        let expected = 2.;
        let input = String::from("12 / 6");
        assert_eq!(
            expression(parse::parse(tokenize(&input)).unwrap()),
            Ok(Types::Num(expected))
        );
    }

    #[test]
    fn add_expression() {
        let expected = 25.;
        let input = String::from("5 + 20");
        assert_eq!(
            expression(parse::parse(tokenize(&input)).unwrap()),
            Ok(Types::Num(expected))
        );
    }

    #[test]
    fn minus_expression() {
        let expected = -15.;
        let input = String::from("5 - 20");
        assert_eq!(
            expression(parse::parse(tokenize(&input)).unwrap()),
            Ok(Types::Num(expected))
        );
    }

    #[test]
    fn add_and_star_expression() {
        let expected = 13.;
        let input = String::from("3*3 + 2*2");
        assert_eq!(
            expression(parse::parse(tokenize(&input)).unwrap()),
            Ok(Types::Num(expected))
        );
    }

    #[test]
    fn order_binary_expression() {
        let expected = 5.;
        let input = String::from("4/2 * 3 + 5 - 6");
        assert_eq!(
            expression(parse::parse(tokenize(&input)).unwrap()),
            Ok(Types::Num(expected))
        );
    }

    #[test]
    fn reverse_order_binary_expression() {
        let expected = -5.;
        let input = String::from("6 - 5 + 3 * 4 / 2");
        assert_eq!(
            expression(parse::parse(tokenize(&input)).unwrap()),
            Ok(Types::Num(expected))
        );
    }

    #[test]
    fn parenthesized_expression() {
        // With brackets
        let expected = 15.;
        let input = String::from("(2 + 3) * 3");
        assert_eq!(
            expression(parse::parse(tokenize(&input)).unwrap()),
            Ok(Types::Num(expected))
        );

        // Without brackets
        let expected = 11.;
        let input = String::from("2 + 3 * 3");
        assert_eq!(
            expression(parse::parse(tokenize(&input)).unwrap()),
            Ok(Types::Num(expected))
        );
    }

    #[test]
    fn comparative_expression() {
        let expected = true;
        let input = String::from("2 >= 2 and 4.2 == 4.2");
        let tokens = tokenize(&input);
        let as_tree = parse::parse(tokens).unwrap();
        assert_eq!(expression(as_tree), Ok(Types::Bool(expected)));
    }
}
