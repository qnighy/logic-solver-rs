use super::pos::Pos;
use super::tokenizer::Token;

#[derive(Debug)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub start: Pos,
    pub end: Pos,
}

impl ParseError {
    pub fn new(kind: ParseErrorKind, start: Pos, end: Pos) -> Self {
        Self { kind, start, end }
    }

    pub fn unexpected_token(token: &Token) -> Self {
        Self::new(
            ParseErrorKind::UnexpectedToken(token.clone()),
            token.start,
            token.end,
        )
    }
}

#[derive(Debug)]
pub enum ParseErrorKind {
    UnexpectedToken(Token),
    Other(String),
}
