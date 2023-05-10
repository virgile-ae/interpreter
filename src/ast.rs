use crate::error;

#[derive(Debug, Clone, PartialEq)]
pub struct Node {
    pub value: Box<NodeType>,
    pub position: error::Position,
}

impl Node {
    pub fn new(value: NodeType, position: error::Position) -> Self {
        Self {
            value: Box::new(value),
            position,
        }
    }

    pub fn new_boxed(value: NodeType, position: error::Position) -> Box<Self> {
        Box::new(Self::new(value, position))
    }
}

impl ToString for Node {
    fn to_string(&self) -> String {
        (*self.value).to_string()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum NodeType {
    Literal(Literal), // string | raw string | integer | float | boolean
    Ident(String),    // (e.g. std::Vec) (includes normal variables)
    // Block,  // (e.g. let five = { fn_call(); 5 };)
    Operator(Operator),
    Declaration(Declaration),
    // Grouped, // `(` __expression__ ``)`
    // Array, // []
    // Array indexing,
    // Tuple,
    // Tuple indexing,
    // Struct, // (object literal)
    // Function call,
    // Method call,
    // Field access,
    // Closure, // function literal
    // Loop, // infinite loop,
    // Range,
    // If,  // if let
    // Match,
    // Return,
    // Await,
    // Underscore,
    // Macro,
}

impl ToString for NodeType {
    fn to_string(&self) -> String {
        match self {
            NodeType::Declaration(d) => (*d).to_string(),
            NodeType::Literal(l) => (*l).to_string(),
            NodeType::Ident(i) => (*i).to_string(),
            NodeType::Operator(o) => (*o).to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    Assignment { id: String, body: Box<Node> },
}

impl ToString for Declaration {
    fn to_string(&self) -> String {
        match self {
            Declaration::Assignment { id, body } => format!("{} = {}", id, body.to_string()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    // String(String),
    Number(f64),
    Boolean(bool),
}

impl ToString for Literal {
    fn to_string(&self) -> String {
        match self {
            Literal::Number(n) => n.to_string(),
            Literal::Boolean(b) => b.to_string(),
        }
    }
}

// Operators:
// negation (-, not),
// arithmetic (+, -, *, /, %),
// boolean (and, or)
// comparison (==, !=, >, <, >=, <=),
// compound assignment (+=, -=, *=, /=, %=)
// error propogation (?),
#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
    // In parsing hierarchy
    Or { left: Box<Node>, right: Box<Node> },
    And { left: Box<Node>, right: Box<Node> },
    EqEq { left: Box<Node>, right: Box<Node> },
    Neq { left: Box<Node>, right: Box<Node> },
    Gt { left: Box<Node>, right: Box<Node> },
    Gte { left: Box<Node>, right: Box<Node> },
    Lt { left: Box<Node>, right: Box<Node> },
    Lte { left: Box<Node>, right: Box<Node> },
    Mod { left: Box<Node>, right: Box<Node> },
    Sub { left: Box<Node>, right: Box<Node> },
    Add { left: Box<Node>, right: Box<Node> },
    Div { left: Box<Node>, right: Box<Node> },
    Mul { left: Box<Node>, right: Box<Node> },
    Pow { left: Box<Node>, right: Box<Node> },
    Not(Box<Node>),
    UnarySub(Box<Node>),
}

impl ToString for Operator {
    fn to_string(&self) -> String {
        match self {
            Operator::Or { left, right } => {
                format!("({} or {})", (*left).to_string(), (*right).to_string())
            }
            Operator::And { left, right } => {
                format!("({} and {})", (*left).to_string(), (*right).to_string())
            }
            Operator::EqEq { left, right } => {
                format!("({} == {})", (*left).to_string(), (*right).to_string())
            }
            Operator::Neq { left, right } => {
                format!("({} != {})", (*left).to_string(), (*right).to_string())
            }
            Operator::Gt { left, right } => {
                format!("({} > {})", (*left).to_string(), (*right).to_string())
            }
            Operator::Gte { left, right } => {
                format!("({} >= {})", (*left).to_string(), (*right).to_string())
            }
            Operator::Lt { left, right } => {
                format!("({} < {})", (*left).to_string(), (*right).to_string())
            }
            Operator::Lte { left, right } => {
                format!("({} <= {})", (*left).to_string(), (*right).to_string())
            }
            Operator::Mod { left, right } => {
                format!("({} % {})", (*left).to_string(), (*right).to_string())
            }
            Operator::Sub { left, right } => {
                format!("({} - {})", (*left).to_string(), (*right).to_string())
            }
            Operator::Add { left, right } => {
                format!("({} + {})", (*left).to_string(), (*right).to_string())
            }
            Operator::Div { left, right } => {
                format!("({} / {})", (*left).to_string(), (*right).to_string())
            }
            Operator::Mul { left, right } => {
                format!("({} * {})", (*left).to_string(), (*right).to_string())
            }
            Operator::Pow { left, right } => {
                format!("({} ** {})", (*left).to_string(), (*right).to_string())
            }
            Operator::Not(val) => format!("(not {})", val.to_string()),
            Operator::UnarySub(val) => format!("(- {})", val.to_string()),
        }
    }
}
