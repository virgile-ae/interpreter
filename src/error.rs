#[derive(Default, Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct Coords {
    pub line: u16,
    pub column: u16,
}

impl Coords {
    pub fn new(line: u16, column: u16) -> Self {
        Self { line, column }
    }
}

#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct Position {
    pub start: Coords,
    pub end: Coords,
}

impl Position {
    pub fn new(start: Coords, end: Coords) -> Self {
        Self { start, end }
    }

    pub fn from_tuples(start: (u16, u16), end: (u16, u16)) -> Self {
        Self {
            start: Coords::new(start.0, start.1),
            end: Coords::new(end.0, end.1),
        }
    }
}
