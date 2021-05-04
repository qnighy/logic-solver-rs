#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Pos {
    pub bytepos: usize,
    pub line: u32,
    pub column: u32,
}
