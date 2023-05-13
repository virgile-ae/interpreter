use std::{
    collections::{HashMap, HashSet},
    io,
};

use crate::{ast::Expr, parse::parse, token::tokenize};
use std::io::{stdin, stdout, Write};

#[derive(Debug, Default, Clone, PartialEq)]
pub struct State(HashMap<String, Value>);

impl State {
    pub fn define(&mut self, name: &String, value: Value) {
        self.0.insert(name.to_string(), value);
    }

    pub fn get(&self, name: &String) -> Result<Value, String> {
        self.0
            .get(name)
            .ok_or(format!("undefined variable `{}`", name))
            .cloned()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Bool(bool),
    Num(f64),
    // Record(HashMap<String, Value>),
    // Str(String),
    Function { args: Vec<String>, body: Box<Expr> },
    Unit,
}

impl ToString for Value {
    fn to_string(&self) -> String {
        match self {
            Value::Bool(b) => b.to_string(),
            Value::Num(f) => f.to_string(),
            Value::Unit => String::from(""),
            Value::Function { args, body } => {
                format!("|{}| {{ {} }}", args.join(", "), body.to_string())
            }
        }
    }
}

pub fn interpret(s: &str) -> Result<Value, String> {
    let tokens = tokenize(s);
    let ast = parse(tokens)?;
    let mut state = Default::default();
    let (_, res) = execute!(ast, state);
    res
}

pub fn repl() -> io::Result<()> {
    let mut input = String::new();
    let mut state: State = Default::default();
    loop {
        print!(">>> ");
        stdout().flush()?;
        stdin().read_line(&mut input)?;

        if input.trim() == "exit".to_string() {
            break;
        }

        let tokens = tokenize(&input.trim());
        input = "".to_string();
        let tree = match parse(tokens) {
            Ok(v) => v,
            Err(e) => {
                eprintln!("error: {}", e);
                continue;
            }
        };
        let (_, res) = execute!(tree, state);
        match res {
            Ok(ref v) => println!("{}", v.to_string()),
            Err(ref e) => eprintln!("{}", e),
        }
    }
    Ok(())
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
    expressions: Vec<Expr>,
    state: &mut State,
) -> (State, Result<Value, String>) {
    let values = expressions
        .iter()
        .map(|x| eval_expr(state, x.clone()))
        .last()
        .unwrap_or(Ok(Value::Unit));
    (state.clone(), values)
}

// ASSIGNMENT DECLARATION //////////////////////////////////////////////////////////////////////////
fn assignment_expr(state: &mut State, node: Expr) -> Result<Value, String> {
    match node {
        Expr::Assignment { id, body, .. } => {
            let res = eval_expr(state, *body)?;
            state.define(&id, res);
            Ok(Value::Unit)
        }
        _ => unreachable!(),
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// EXPRESSIONS /////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

fn eval_expr(state: &mut State, node: Expr) -> Result<Value, String> {
    match node {
        Expr::Ident { .. } => ident_expr(state, node),
        Expr::Number { .. } => literal_expr(node),
        Expr::String { .. } => todo!(),
        Expr::Boolean { .. } => literal_expr(node),
        Expr::Call { .. } => call_expr(state, node),
        Expr::Not { .. } => not_expr(state, node),
        Expr::UnSub { .. } => unary_sub_expr(state, node),
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
        Expr::IfElse { .. } => if_else_expr(state, node),
        Expr::Block { .. } => block_expr(state, node),
        Expr::Assignment { .. } => assignment_expr(state, node),
        Expr::Function { .. } => func_expr(state, node),
    }
}

// FUNCTION EXPRESSION /////////////////////////////////////////////////////////////////////////////
fn func_expr(_state: &mut State, node: Expr) -> Result<Value, String> {
    if let Expr::Function { args, body, .. } = node {
        Ok(Value::Function { args, body })
    } else {
        unreachable!()
    }
}

// FIXME: problem arises with a higher order function when it's params have the same name as the
// params of the function it is being called with
fn call_expr(state: &mut State, node: Expr) -> Result<Value, String> {
    if let Expr::Call { func, args, .. } = node {
        // Extract function args and body
        let func_args;
        let func_body;
        match eval_expr(state, *func)? {
            Value::Function { args, body } => {
                func_args = args;
                func_body = body;
            }
            Value::Bool(b) => {
                return Err(format!("`{b}` is not a function and so can't be called"))
            }
            Value::Num(b) => return Err(format!("`{b}` is not a function and so can't be called")),
            Value::Unit => todo!(),
        };
        // Ensure that the correct number of arguments were provided
        if func_args.len() != args.len() {
            return Err(format!(
                "function takes {} arguments, but only {} were provided",
                func_args.len(),
                args.len()
            ));
        }
        // Check that the identifiers are all different in func_args
        {
            let mut acc = HashSet::new();
            for i in func_args.iter() {
                if acc.contains(&i) {
                    return Err(format!(
                        "function can't have 2 arguments with the same identifier (`{}`)",
                        i
                    ));
                } else {
                    acc.insert(i);
                }
            }
        }
        // Setting up inner state
        let mut inner_state = state.clone();

        for (key, val) in func_args.iter().zip(args.iter()) {
            // inner_state.define(key, EvalState::Unevaluated(val.clone()));
            inner_state.define(key, eval_expr(state, val.clone())?);
        }

        // Evaluate the function body with the new inner state
        eval_expr(&mut inner_state, *func_body)
    } else {
        unreachable!()
    }
}

// IF ELSE EXPRESSION //////////////////////////////////////////////////////////////////////////////
fn if_else_expr(state: &mut State, node: Expr) -> Result<Value, String> {
    if let Expr::IfElse {
        cond,
        on_true,
        on_false,
        ..
    } = node
    {
        match eval_expr(state, *cond)? {
            Value::Bool(true) => block_expr(state, *on_true),
            Value::Bool(false) => block_expr(state, *on_false),
            _ => unreachable!(),
        }
    } else {
        unreachable!()
    }
}

// BLOCK EXPRESSION ////////////////////////////////////////////////////////////////////////////////
fn block_expr(state: &mut State, node: Expr) -> Result<Value, String> {
    if let Expr::Block { exprs, .. } = node {
        // Setting up inner state
        let mut inner_state = state.clone();

        exprs
            .iter()
            .map(|x| eval_expr(&mut inner_state, x.clone()))
            .last()
            .unwrap_or(Ok(Value::Unit))
    } else {
        unreachable!()
    }
}

// UNARY EXPRESSIONS ///////////////////////////////////////////////////////////////////////////////
macro_rules! create_unary_expr {
    (
        $func_name:ident,
        $op:ident$(::$op_rest:ident)*,
        $err_str:literal,
        $type:ident$(::$type_rest:ident)*,
        $func:ident$(::$func_rest:ident)* $(,)?
    ) => {
        fn $func_name(state: &mut State, node: Expr) -> Result<Value, String> {
            if let Expr::$op {
                operand,
                ..
            } = node {
                let body = eval_expr(state, *operand)?;
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
create_unary_expr!(unary_sub_expr, UnSub, "-", Value::Num, UNARY_SUB);

////////////////////////////////////////////////////////////////////////////////////////////////////
// BINARY EXPRESSIONS //////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

macro_rules! create_binary_expr {
    (
        $func_name:ident,
        $op:ident,
        $err_str:literal,
        $param_type:ident$(::$param_type_rest:ident)*,
        $return_type:ident$(::$return_rest:ident)*,
        $func:ident $(,)?
    ) => {
        fn $func_name(state: &mut State, node: Expr) -> Result<Value, String> {
            if let Expr::$op { left, right, .. } = node {
                let left = eval_expr(state, *left)?;
                let right = eval_expr(state, *right)?;
                match (left, right) {
                    ($param_type$(::$param_type_rest)*(left), $param_type$(::$param_type_rest)*(right)) =>
                        Ok($return_type$(::$return_rest)*($func(left,right))),

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
create_binary_expr!(mul_expr, Mul, "*", Value::Num, MUL);

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
        _ => unreachable!(),
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// IDENTIFIER EXPRESSION ///////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////
fn ident_expr(state: &mut State, node: Expr) -> Result<Value, String> {
    if let Expr::Ident { val, .. } = node {
        // state.eval_get(&val)
        state.get(&val)
    } else {
        unreachable!()
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// TESTS ///////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    use crate::{evaluate::*, parse};

    fn should_evaluate_to(input: &str, expected: Value) {
        let tokens = tokenize(input);
        let ast = parse::parse(tokens).unwrap();
        let (_, result) = execute!(ast);
        assert_eq!(result, Ok(expected));
    }

    #[test]
    fn num_literal() {
        // Integer
        should_evaluate_to("5", Value::Num(5.));

        // Real
        should_evaluate_to("5.4", Value::Num(5.4));

        // Scientific notation
        should_evaluate_to("5.4e-5", Value::Num(5.4e-5));
    }

    #[test]
    fn bool_literal() {
        should_evaluate_to("true", Value::Bool(true));
        should_evaluate_to("false", Value::Bool(false));
    }

    #[test]
    fn binary_expression() {
        // Numbers
        should_evaluate_to("5 * 12", Value::Num(60.));
        should_evaluate_to("12 / 12", Value::Num(1.));
        should_evaluate_to("5 + 12", Value::Num(17.));
        should_evaluate_to("5 - 12", Value::Num(-7.));
        should_evaluate_to("12 % 5", Value::Num(2.));
        should_evaluate_to("2 ** 5", Value::Num(32.));

        // Boolean
        should_evaluate_to("true and false", Value::Bool(false));
        should_evaluate_to("false or true", Value::Bool(true));
    }
    #[test]
    fn unary_expression() {
        should_evaluate_to("-5", Value::Num(-5.));
        should_evaluate_to("not false", Value::Bool(true));
    }

    #[test]
    fn order_binary_expression() {
        should_evaluate_to("4/2 * 3 + 5 - 6", Value::Num(5.));
        should_evaluate_to("6 - 5 + 3 * 4 / 2", Value::Num(-5.));
    }

    #[test]
    fn parenthesized_expression() {
        // With brackets
        should_evaluate_to("(2 + 3) * 3", Value::Num(15.));
        should_evaluate_to("2 + 3 * 3", Value::Num(11.));
    }

    #[test]
    fn comparative_expression() {
        should_evaluate_to("2 >= 2 and 4.2 == 4.2", Value::Bool(true));
    }
}
