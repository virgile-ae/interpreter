use crate::error::Span;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Expr {
    // Values //////////////////////////////////////////////////////////////////////////////////////
    Ident {
        val: String,
        span: Span,
    },
    Number {
        val: f64,
        span: Span,
    },
    String {
        val: String,
        span: Span,
    },
    Boolean {
        val: bool,
        span: Span,
    },
    Call {
        func: Box<Expr>,
        args: Vec<Expr>,
        span: Span,
    },
    Function {
        args: Vec<String>,
        body: Box<Expr>,
        span: Span,
    },
    // Operators ///////////////////////////////////////////////////////////////////////////////////
    // Unary operators
    Not {
        operand: Box<Expr>,
        span: Span,
    },
    UnSub {
        operand: Box<Expr>,
        span: Span,
    },
    // Binary operators
    Or {
        left: Box<Expr>,
        right: Box<Expr>,
        span: Span,
    },
    And {
        left: Box<Expr>,
        right: Box<Expr>,
        span: Span,
    },
    EqEq {
        left: Box<Expr>,
        right: Box<Expr>,
        span: Span,
    },
    Neq {
        left: Box<Expr>,
        right: Box<Expr>,
        span: Span,
    },
    Gt {
        left: Box<Expr>,
        right: Box<Expr>,
        span: Span,
    },
    Gte {
        left: Box<Expr>,
        right: Box<Expr>,
        span: Span,
    },
    Lt {
        left: Box<Expr>,
        right: Box<Expr>,
        span: Span,
    },
    Lte {
        left: Box<Expr>,
        right: Box<Expr>,
        span: Span,
    },
    Mod {
        left: Box<Expr>,
        right: Box<Expr>,
        span: Span,
    },
    Sub {
        left: Box<Expr>,
        right: Box<Expr>,
        span: Span,
    },
    Add {
        left: Box<Expr>,
        right: Box<Expr>,
        span: Span,
    },
    Div {
        left: Box<Expr>,
        right: Box<Expr>,
        span: Span,
    },
    Mul {
        left: Box<Expr>,
        right: Box<Expr>,
        span: Span,
    },
    Pow {
        left: Box<Expr>,
        right: Box<Expr>,
        span: Span,
    },
    // Control flow ////////////////////////////////////////////////////////////////////////////////
    IfElse {
        cond: Box<Expr>,
        on_true: Box<Expr>,
        on_false: Box<Expr>,
        span: Span,
    },
    // Other ///////////////////////////////////////////////////////////////////////////////////////
    Block {
        exprs: Vec<Expr>,
        span: Span,
    },
    Assignment {
        id: String,
        body: Box<Expr>,
        span: Span,
    },
}

impl Expr {
    pub fn position(&self) -> Span {
        match self {
            Expr::Ident { span, .. } => *span,
            Expr::Number { span, .. } => *span,
            Expr::String { span, .. } => *span,
            Expr::Boolean { span, .. } => *span,
            Expr::Call { span, .. } => *span,
            Expr::Not { span, .. } => *span,
            Expr::UnSub { span, .. } => *span,
            Expr::Or { span, .. } => *span,
            Expr::And { span, .. } => *span,
            Expr::EqEq { span, .. } => *span,
            Expr::Neq { span, .. } => *span,
            Expr::Gt { span, .. } => *span,
            Expr::Gte { span, .. } => *span,
            Expr::Lt { span, .. } => *span,
            Expr::Lte { span, .. } => *span,
            Expr::Mod { span, .. } => *span,
            Expr::Sub { span, .. } => *span,
            Expr::Add { span, .. } => *span,
            Expr::Div { span, .. } => *span,
            Expr::Mul { span, .. } => *span,
            Expr::Pow { span, .. } => *span,
            Expr::IfElse { span, .. } => *span,
            Expr::Block { span, .. } => *span,
            Expr::Assignment { span, .. } => *span,
            Expr::Function { span, .. } => *span,
        }
    }
}

impl ToString for Expr {
    fn to_string(&self) -> String {
        match self {
            Expr::Ident { val, .. } => val.clone(),
            Expr::Number { val, .. } => val.to_string(),
            Expr::String { val, .. } => val.clone(),
            Expr::Boolean { val, .. } => val.to_string(),
            Expr::Call { func, args, .. } => format!(
                "{}({})",
                func.to_string(),
                args.iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Expr::Not { operand, .. } => format!("not {}", operand.to_string()),
            Expr::UnSub { operand, .. } => format!("-{}", operand.to_string()),
            Expr::Or { left, right, .. } => {
                format!("{} or {}", left.to_string(), right.to_string())
            }
            Expr::And { left, right, .. } => {
                format!("{} and {}", left.to_string(), right.to_string())
            }
            Expr::EqEq { left, right, .. } => {
                format!("{} == {}", left.to_string(), right.to_string())
            }
            Expr::Neq { left, right, .. } => {
                format!("{} != {}", left.to_string(), right.to_string())
            }
            Expr::Gt { left, right, .. } => {
                format!("{} > {}", left.to_string(), right.to_string())
            }
            Expr::Gte { left, right, .. } => {
                format!("{} >= {}", left.to_string(), right.to_string())
            }
            Expr::Lt { left, right, .. } => {
                format!("{} < {}", left.to_string(), right.to_string())
            }
            Expr::Lte { left, right, .. } => {
                format!("{} <= {}", left.to_string(), right.to_string())
            }
            Expr::Mod { left, right, .. } => {
                format!("{} % {}", left.to_string(), right.to_string())
            }
            Expr::Sub { left, right, .. } => {
                format!("{} - {}", left.to_string(), right.to_string())
            }
            Expr::Add { left, right, .. } => {
                format!("{} + {}", left.to_string(), right.to_string())
            }
            Expr::Div { left, right, .. } => {
                format!("{} / {}", left.to_string(), right.to_string())
            }
            Expr::Mul { left, right, .. } => {
                format!("{} * {}", left.to_string(), right.to_string())
            }
            Expr::Pow { left, right, .. } => {
                format!("{} ** {}", left.to_string(), right.to_string())
            }
            Expr::IfElse {
                cond,
                on_true,
                on_false,
                ..
            } => format!(
                "if {} {} else {}",
                cond.to_string(),
                on_true.to_string(),
                on_false.to_string()
            ),
            Expr::Block { exprs, .. } => format!(
                "{{ {} }}",
                exprs
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(";\n")
            ),
            Expr::Assignment { id, body, .. } => format!("let {} = {}", id, body.to_string()),
            Expr::Function { args, body, .. } => {
                format!("|{}| {}", args.join(", "), body.to_string())
            }
        }
    }
}
