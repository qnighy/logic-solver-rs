use std::fmt;
use thiserror::Error;

use crate::parsing::tokenizer::TokenKind;

use super::pos::Pos;
use super::tokenizer::Token;

#[derive(Debug, Error)]
#[error("parse error at line {}, column {}: {}", .start.line + 1, .start.column + 1, .kind)]
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

impl fmt::Display for ParseErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ParseErrorKind::*;

        match self {
            UnexpectedToken(token) => {
                if let TokenKind::Unknown(ref s) = token.kind {
                    write!(f, "invalid characters: {:?}", s)
                } else {
                    write!(f, "unexpected token: {:?}", token.kind)
                }
            }
            Other(s) => f.write_str(s),
        }
    }
}
