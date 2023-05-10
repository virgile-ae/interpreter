use crate::error::Position;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Expr {
    // Values //////////////////////////////////////////////////////////////////////////////////////
    Ident {
        val: String,
        position: Position,
    },
    Number {
        val: f64,
        position: Position,
    },
    String {
        val: String,
        position: Position,
    },
    Boolean {
        val: bool,
        position: Position,
    },
    Call {
        func: Box<Expr>,
        position: Position,
    },
    // Operators ///////////////////////////////////////////////////////////////////////////////////
    // Unary operators
    Not {
        operand: Box<Expr>,
        position: Position,
    },
    UnarySub {
        operand: Box<Expr>,
        position: Position,
    },
    // Binary operators
    Or {
        left: Box<Expr>,
        right: Box<Expr>,
        position: Position,
    },
    And {
        left: Box<Expr>,
        right: Box<Expr>,
        position: Position,
    },
    EqEq {
        left: Box<Expr>,
        right: Box<Expr>,
        position: Position,
    },
    Neq {
        left: Box<Expr>,
        right: Box<Expr>,
        position: Position,
    },
    Gt {
        left: Box<Expr>,
        right: Box<Expr>,
        position: Position,
    },
    Gte {
        left: Box<Expr>,
        right: Box<Expr>,
        position: Position,
    },
    Lt {
        left: Box<Expr>,
        right: Box<Expr>,
        position: Position,
    },
    Lte {
        left: Box<Expr>,
        right: Box<Expr>,
        position: Position,
    },
    Mod {
        left: Box<Expr>,
        right: Box<Expr>,
        position: Position,
    },
    Sub {
        left: Box<Expr>,
        right: Box<Expr>,
        position: Position,
    },
    Add {
        left: Box<Expr>,
        right: Box<Expr>,
        position: Position,
    },
    Div {
        left: Box<Expr>,
        right: Box<Expr>,
        position: Position,
    },
    Mul {
        left: Box<Expr>,
        right: Box<Expr>,
        position: Position,
    },
    Pow {
        left: Box<Expr>,
        right: Box<Expr>,
        position: Position,
    },
    // Control flow ////////////////////////////////////////////////////////////////////////////////
    IfElse {
        cond: Box<Expr>,
        on_true: Box<Expr>,
        on_false: Option<Box<Expr>>,
        position: Position,
    },
    // Other ///////////////////////////////////////////////////////////////////////////////////////
    Block {
        decls: Vec<Decl>,
        expr: Box<Expr>,
        position: Position,
    },
}

impl Expr {
    pub fn position(&self) -> Position {
        match self {
            Expr::Ident { position, .. } => *position,
            Expr::Number { position, .. } => *position,
            Expr::String { position, .. } => *position,
            Expr::Boolean { position, .. } => *position,
            Expr::Call { position, .. } => *position,
            Expr::Not { position, .. } => *position,
            Expr::UnarySub { position, .. } => *position,
            Expr::Or { position, .. } => *position,
            Expr::And { position, .. } => *position,
            Expr::EqEq { position, .. } => *position,
            Expr::Neq { position, .. } => *position,
            Expr::Gt { position, .. } => *position,
            Expr::Gte { position, .. } => *position,
            Expr::Lt { position, .. } => *position,
            Expr::Lte { position, .. } => *position,
            Expr::Mod { position, .. } => *position,
            Expr::Sub { position, .. } => *position,
            Expr::Add { position, .. } => *position,
            Expr::Div { position, .. } => *position,
            Expr::Mul { position, .. } => *position,
            Expr::Pow { position, .. } => *position,
            Expr::IfElse { position, .. } => *position,
            Expr::Block { position, .. } => *position,
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Decl {
    Assignment {
        id: String,
        body: Box<Expr>,
        position: Position,
    },
    Expression(Expr),
}
