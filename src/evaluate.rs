use std::collections::HashMap;

use crate::{
    ast::{Decl, Expr},
    err_msg,
};

// Node instead of value makes the language lazy
pub(crate) type State = HashMap<String, Expr>;

#[derive(Debug, PartialEq)]
pub enum Value {
    Bool(bool),
    Num(f64),
    // Record(HashMap<String, Value>),
    // Str(String),
    Variable(String),
    Unit,
}

impl ToString for Value {
    fn to_string(&self) -> String {
        match self {
            Value::Bool(b) => b.to_string(),
            Value::Num(f) => f.to_string(),
            Value::Variable(v) => v.clone(),
            Value::Unit => "()".to_string(),
        }
    }
}

#[macro_export]
macro_rules! execute {
    ($declarations:ident, $state:ident) => {{
        use crate::evaluate::execute_inner;
        execute_inner($declarations, &mut $state)
    }};
    ($declarations:ident) => {{
        use crate::evaluate::execute_inner;
        let mut state = Default::default();
        execute_inner($declarations, &mut state)
    }};
}
pub(crate) use execute;

pub(crate) fn execute_inner(
    declarations: Vec<Decl>,
    state: &mut State,
) -> (State, Vec<Result<Value, String>>) {
    let values = declarations
        .iter()
        .map(|x| declaration(state, x.clone()))
        .collect();
    (state.clone(), values)
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// DECLARATIONS ////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////
pub(crate) fn declaration(state: &mut State, node: Decl) -> Result<Value, String> {
    // match node.value {
    //     NodeType::Declaration(_) => assignment_declaration(state, node),
    //     _ => expression_declaration(state, node),
    // }
    match node {
        Decl::Assignment { .. } => assignment_declaration(state, node),
        Decl::Expression(ex) => expression(state, ex),
    }
}

// ASSIGNMENT DECLARATION //////////////////////////////////////////////////////////////////////////
fn assignment_declaration(state: &mut State, node: Decl) -> Result<Value, String> {
    // match *node.value {
    //     NodeType::Declaration(Declaration::Assignment { id, body }) => {
    //         if state.contains_key(&id) {
    //             err_msg!(
    //                 node.position.start,
    //                 "variable `{}` cannot be redefined or defined as itself",
    //                 id
    //             )
    //         } else {
    //             state.insert(id, *body);
    //             Ok(Value::Unit)
    //         }
    //     }
    //     _ => unreachable!(),
    // }
    match node {
        Decl::Assignment { id, body, position } => {
            if state.contains_key(&id) {
                err_msg!(
                    position.start,
                    "variable `{}` cannot be redefined or defined as itself",
                    id
                )
            } else {
                state.insert(id, *body);
                Ok(Value::Unit)
            }
        }
        _ => unreachable!(),
    }
}

// ASSIGNMENT DECLARATION //////////////////////////////////////////////////////////////////////////
fn expression_declaration(state: &State, node: Decl) -> Result<Value, String> {
    match node {
        Decl::Expression(e) => expression(state, e),
        _ => unreachable!(),
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// EXPRESSIONS /////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

fn expression(state: &State, node: Expr) -> Result<Value, String> {
    match node {
        Expr::Ident { .. } => ident_expr(state, node),
        Expr::Number { .. } => literal_expr(node),
        Expr::String { .. } => todo!(),
        Expr::Boolean { .. } => literal_expr(node),
        Expr::Call { .. } => todo!(),
        Expr::Not { .. } => not_expr(state, node),
        Expr::UnarySub { .. } => unary_sub_expr(state, node),
        Expr::Or { .. } => or_expr(state, node),
        Expr::And { .. } => and_expr(state, node),
        Expr::EqEq { .. } => eqeq_expr(state, node),
        Expr::Neq { .. } => neq_expr(state, node),
        Expr::Gt { .. } => gt_expr(state, node),
        Expr::Gte { .. } => gte_expr(state, node),
        Expr::Lt { .. } => lt_expr(state, node),
        Expr::Lte { .. } => lte_expr(state, node),
        Expr::Mod { .. } => mod_expr(state, node),
        Expr::Sub { .. } => sub_expr(state, node),
        Expr::Add { .. } => add_expr(state, node),
        Expr::Div { .. } => div_expr(state, node),
        Expr::Mul { .. } => mul_expr(state, node),
        Expr::Pow { .. } => pow_expr(state, node),
        Expr::IfElse { .. } => todo!(),
        Expr::Block { .. } => todo!(),
    }
}
////////////////////////////////////////////////////////////////////////////////////////////////////
// UNARY EXPRESSIONS ///////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////
macro_rules! create_unary_expr {
    (
        $func_name:ident,
        $op:ident$(::$op_rest:ident)*,
        $err_str:literal,
        $type:ident$(::$type_rest:ident)*,
        $func:ident$(::$func_rest:ident)* $(,)?
    ) => {
        fn $func_name(state: &State, node: Expr) -> Result<Value, String> {
            if let Expr::$op {
                operand,
                ..
            } = node {
                let body = expression(state, *operand)?;
                match body {
                    $type$(::$type_rest)*(body) => Ok($type$(::$type_rest)*($func$(::$func_rest)*(body))),
                    body => Err(format!(
                        "unsupported operand for `{}` operator: `{:?}`",
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
create_unary_expr!(not_expr, Not, "not", Value::Bool, NOT);

const UNARY_SUB: fn(f64) -> f64 = |x| -x;
create_unary_expr!(unary_sub_expr, UnarySub, "-", Value::Num, UNARY_SUB);

////////////////////////////////////////////////////////////////////////////////////////////////////
// BINARY EXPRESSIONS //////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

macro_rules! create_binary_expr {
    (
        $func_name:ident,
        $op:ident$(::$op_rest:ident)*,
        $err_str:literal,
        $param_type:ident$(::$param_type_rest:ident)*,
        $return_type:ident$(::$return_rest:ident)*,
        $func:ident$(::$func_rest:ident)* $(,)?
    ) => {
        fn $func_name(state: &State, node: Expr) -> Result<Value, String> {
            if let Expr::$op { left, right, .. } = node {
                let left = expression(state, *left)?;
                let right = expression(state, *right)?;
                match (left, right) {
                    ($param_type$(::$param_type_rest)*(left), $param_type$(::$param_type_rest)*(right)) =>
                        Ok($return_type$(::$return_rest)*($func$(::$func_rest)*(left,right))),

                    (left, $param_type$(::$param_type_rest)*(_)) => Err(format!(
                        "unsupported lhs operand for `{}` operator: `{}`",
                        $err_str, left.to_string()
                    )),
                    ($param_type$(::$param_type_rest)*(_), right) => Err(format!(
                        "unsupported rhs operand for `{}` operator: `{}`",
                        $err_str, right.to_string()
                    )),
                    (left, right) => Err(format!(
                        "unsupported operands for `{}` operator: `{}` and `{}`",
                        $err_str, left.to_string(), right.to_string()
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
create_binary_expr!(or_expr, Or, "or", Value::Bool, OR);

const AND: fn(bool, bool) -> bool = |l, r| l && r;
create_binary_expr!(and_expr, And, "and", Value::Bool, AND);

const EQ: fn(f64, f64) -> bool = |l, r| l == r;
create_binary_expr!(eqeq_expr, EqEq, "==", Value::Num, Value::Bool, EQ);

const NEQ: fn(f64, f64) -> bool = |l, r| l != r;
create_binary_expr!(neq_expr, Neq, "!=", Value::Num, Value::Bool, NEQ);

const GT: fn(f64, f64) -> bool = |l, r| l > r;
create_binary_expr!(gt_expr, Gt, ">", Value::Num, Value::Bool, GT);

const GTE: fn(f64, f64) -> bool = |l, r| l >= r;
create_binary_expr!(gte_expr, Gte, ">=", Value::Num, Value::Bool, GTE);

const LT: fn(f64, f64) -> bool = |l, r| l < r;
create_binary_expr!(lt_expr, Lt, "<", Value::Num, Value::Bool, LT);

const LTE: fn(f64, f64) -> bool = |l, r| l <= r;
create_binary_expr!(lte_expr, Lte, "<=", Value::Num, Value::Bool, LTE);

const MOD: fn(f64, f64) -> f64 = |l, r| l.rem_euclid(r);
create_binary_expr!(mod_expr, Mod, "%", Value::Num, MOD);

const SUB: fn(f64, f64) -> f64 = |l, r| l - r;
create_binary_expr!(sub_expr, Sub, "-", Value::Num, SUB);

const ADD: fn(f64, f64) -> f64 = |l, r| l + r;
create_binary_expr!(add_expr, Add, "+", Value::Num, ADD);

const MUL: fn(f64, f64) -> f64 = |l, r| l * r;
create_binary_expr!(mul_expr, Mul, "*", Value::Num, MUL,);

const DIV: fn(f64, f64) -> f64 = |l, r| l / r;
create_binary_expr!(div_expr, Div, "/", Value::Num, DIV);

const POW: fn(f64, f64) -> f64 = |l, r| l.powf(r);
create_binary_expr!(pow_expr, Pow, "**", Value::Num, POW);

////////////////////////////////////////////////////////////////////////////////////////////////////
// LITERAL EXPRESSIONS /////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////
fn literal_expr(node: Expr) -> Result<Value, String> {
    match node {
        Expr::Number { val, .. } => Ok(Value::Num(val)),
        Expr::String { .. } => todo!(),
        Expr::Boolean { val, .. } => Ok(Value::Bool(val)),
        _ => panic!("literal_expr should receive Literal, but got {:?}", node),
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// LITERAL EXPRESSIONS /////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////
fn ident_expr(state: &State, node: Expr) -> Result<Value, String> {
    if let Expr::Ident { val, .. } = node {
        expression(state, state[&val].clone())
    } else {
        panic!("ident_expr should receive Ident, but got {:?}", node);
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// TESTS ///////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////
/* #[cfg(test)]
 mod tests {
    use crate::{evaluate::*, parse, token::*};

    #[test]
    fn num_literal() {
        // Integer
        let expected = 5.;
        let input = String::from("5");
        assert_eq!(
            expression(parse::parse(tokenize(&input)).unwrap()[0]),
            Ok(Value::Num(expected))
        );

        // Real
        let expected = 5.4;
        let input = String::from("5.4");
        assert_eq!(
            expression(parse::parse(tokenize(&input)).unwrap()[0]),
            Ok(Value::Num(expected))
        );
        // Scientific notation
        let expected = 5.4e-5;
        let input = String::from("5.4e-5");
        assert_eq!(
            expression(parse::parse(tokenize(&input)).unwrap()[0]),
            Ok(Value::Num(expected))
        );
    }

    #[test]
    fn bool_literal() {
        let expected = true;
        let input = String::from("true");
        assert_eq!(
            expression(parse::parse(tokenize(&input)).unwrap()[0]),
            Ok(Value::Bool(expected))
        );
    }

    #[test]
    fn star_expression() {
        let expected = 60.;
        let input = String::from("5 * 12");
        assert_eq!(
            expression(parse::parse(tokenize(&input)).unwrap()[0]),
            Ok(Value::Num(expected))
        );
    }

    #[test]
    fn divide_expression() {
        let expected = 2.;
        let input = String::from("12 / 6");
        assert_eq!(
            expression(parse::parse(tokenize(&input)).unwrap()[0]),
            Ok(Value::Num(expected))
        );
    }

    #[test]
    fn add_expression() {
        let expected = 25.;
        let input = String::from("5 + 20");
        assert_eq!(
            expression(parse::parse(tokenize(&input)).unwrap()[0]),
            Ok(Value::Num(expected))
        );
    }

    #[test]
    fn minus_expression() {
        let expected = -15.;
        let input = String::from("5 - 20");
        assert_eq!(
            expression(parse::parse(tokenize(&input)).unwrap()[0]),
            Ok(Value::Num(expected))
        );
    }

    #[test]
    fn add_and_star_expression() {
        let expected = 13.;
        let input = String::from("3*3 + 2*2");
        assert_eq!(
            expression(parse::parse(tokenize(&input)).unwrap()[0]),
            Ok(Value::Num(expected))
        );
    }

    #[test]
    fn order_binary_expression() {
        let expected = 5.;
        let input = String::from("4/2 * 3 + 5 - 6");
        assert_eq!(
            expression(parse::parse(tokenize(&input)).unwrap()[0]),
            Ok(Value::Num(expected))
        );
    }

    #[test]
    fn reverse_order_binary_expression() {
        let expected = -5.;
        let input = String::from("6 - 5 + 3 * 4 / 2");
        assert_eq!(
            expression(parse::parse(tokenize(&input)).unwrap()[0]),
            Ok(Value::Num(expected))
        );
    }

    #[test]
    fn parenthesized_expression() {
        // With brackets
        let expected = 15.;
        let input = String::from("(2 + 3) * 3");
        assert_eq!(
            expression(parse::parse(tokenize(&input)).unwrap()[0]),
            Ok(Value::Num(expected))
        );

        // Without brackets
        let expected = 11.;
        let input = String::from("2 + 3 * 3");
        assert_eq!(
            expression(parse::parse(tokenize(&input)).unwrap()[0]),
            Ok(Value::Num(expected))
        );
    }

    #[test]
    fn comparative_expression() {
        let expected = true;
        let input = String::from("2 >= 2 and 4.2 == 4.2");
        let tokens = tokenize(&input);
        let as_tree = parse::parse(tokens).unwrap()[0];
        assert_eq!(expression(as_tree), Ok(Value::Bool(expected)));
    }
} */
