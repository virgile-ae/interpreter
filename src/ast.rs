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

#[derive(Debug, PartialEq, Clone)]
pub enum NodeType {
    Literal(Literal), // string | raw string | integer | float | boolean
    Path,             // (e.g. std::Vec) (includes normal variables)
    // Block,  // (e.g. let five = { fn_call(); 5 };)
    Operator(Operator),
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

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    // String(String),
    Number(f64),
    Boolean(bool),
}

// Operators:
// negation (-, not),
// arithmetic (+, -, *, /, %),
// boolean (and, or)
// comparison (==, !=, >, <, >=, <=),
// assignment (=)
// compound assignment (+=, -=, *=, /=, %=)
// error propogation (?),
#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
    // In parsing hierarchy
    Or { left: Box<Node>, right: Box<Node> },
    And { left: Box<Node>, right: Box<Node> },
    Eq { left: Box<Node>, right: Box<Node> },
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
