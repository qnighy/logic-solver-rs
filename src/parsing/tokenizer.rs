use super::error::ParseError;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Pos {
    bytepos: usize,
    line: u32,
    column: u32,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Token {
    pub kind: TokenKind,
    pub start: Pos,
    pub end: Pos,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenKind {
    Ident(String),
    LParen,
    RParen,
    Arrow,
    Conj,
    Disj,
    Top,
    Bottom,
}

#[derive(Debug)]
struct Tokenizer<'a> {
    source: &'a str,
    pos: Pos,
}

impl<'a> Tokenizer<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            source,
            pos: Pos::default(),
        }
    }

    fn parse_ident(&mut self) -> Result<String, ParseError> {
        let start = self.pos;
        let ch = self.peek().ok_or_else(|| ParseError)?;
        if !ch.is_ascii_alphabetic() {
            return Err(ParseError);
        }
        self.bump();
        while self
            .peek()
            .map(|b| b.is_ascii_alphanumeric())
            .unwrap_or(false)
        {
            self.bump();
        }
        let end = self.pos;
        Ok(self.source[start.bytepos..end.bytepos].to_owned())
    }

    fn parse_token(&mut self) -> Result<Option<Token>, ParseError> {
        self.skip_spaces()?;
        let start = self.pos;
        let kind = self.parse_token_kind()?;
        let end = self.pos;
        if let Some(kind) = kind {
            Ok(Some(Token { kind, start, end }))
        } else {
            Ok(None)
        }
    }

    fn parse_token_kind(&mut self) -> Result<Option<TokenKind>, ParseError> {
        let next = if let Some(next) = self.peek() {
            next
        } else {
            return Ok(None);
        };
        if next.is_ascii_alphabetic() {
            let ident = self.parse_ident()?;
            Ok(Some(TokenKind::Ident(ident)))
        } else if next == '→' {
            self.bump();
            Ok(Some(TokenKind::Arrow))
        } else if next == '∧' {
            self.bump();
            Ok(Some(TokenKind::Conj))
        } else if next == '∨' {
            self.bump();
            Ok(Some(TokenKind::Disj))
        } else if next == '⊤' {
            self.bump();
            Ok(Some(TokenKind::Top))
        } else if next == '⊥' /* up tack */ || next == '⟂'
        /* perpendicular */
        {
            self.bump();
            Ok(Some(TokenKind::Bottom))
        } else if next == '(' {
            self.bump();
            Ok(Some(TokenKind::LParen))
        } else if next == ')' {
            self.bump();
            Ok(Some(TokenKind::RParen))
        } else {
            Err(ParseError)
        }
    }

    fn skip_spaces(&mut self) -> Result<(), ParseError> {
        while self
            .peek()
            .map(|b| b.is_ascii_whitespace())
            .unwrap_or(false)
        {
            self.bump();
        }
        Ok(())
    }

    fn peek(&self) -> Option<char> {
        self.source[self.pos.bytepos..].chars().next()
    }

    fn bump(&mut self) -> Option<char> {
        let mut iter = self.source[self.pos.bytepos..].chars();
        let ret = iter.next();
        self.pos.bytepos = self.source.len() - iter.as_str().len();
        if ret == Some('\n') {
            self.pos.line += 1;
            self.pos.column = 0;
        } else if ret.is_some() {
            self.pos.column += 1;
        }
        ret
    }
}

pub fn tokenize(s: &str) -> Result<Vec<Token>, ParseError> {
    let mut tokenizer = Tokenizer::new(s);
    let mut tokens = Vec::new();
    while let Some(token) = tokenizer.parse_token()? {
        tokens.push(token);
    }
    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;
    use big_s::S;

    #[test]
    fn test_atom1() {
        use TokenKind::*;

        let prop = tokenize("A").unwrap();
        assert_eq!(
            prop,
            vec![Token {
                kind: Ident(S("A")),
                start: pos(0, 0, 0),
                end: pos(1, 0, 1),
            }]
        );
    }

    #[test]
    fn test_atom2() {
        use TokenKind::*;

        let prop = tokenize("foo23").unwrap();
        assert_eq!(
            prop,
            vec![Token {
                kind: Ident(S("foo23")),
                start: pos(0, 0, 0),
                end: pos(5, 0, 5),
            }]
        );
    }

    #[test]
    fn test_atom3() {
        tokenize("foo23+").unwrap_err();
    }

    #[test]
    fn test_atom4() {
        use TokenKind::*;

        let prop = tokenize("\nfoo23 ").unwrap();
        assert_eq!(
            prop,
            vec![Token {
                kind: Ident(S("foo23")),
                start: pos(1, 1, 0),
                end: pos(6, 1, 5)
            }]
        );
    }

    #[test]
    fn test_arrow() {
        use TokenKind::*;

        let prop = tokenize("→").unwrap();
        assert_eq!(
            prop,
            vec![Token {
                kind: Arrow,
                start: pos(0, 0, 0),
                end: pos(3, 0, 1)
            }]
        );
    }

    #[test]
    fn test_conj1() {
        use TokenKind::*;

        let prop = tokenize("∧").unwrap();
        assert_eq!(
            prop,
            vec![Token {
                kind: Conj,
                start: pos(0, 0, 0),
                end: pos(3, 0, 1)
            }]
        );
    }

    #[test]
    fn test_conj2() {
        use TokenKind::*;

        let prop = tokenize("A ∧ B").unwrap();
        assert_eq!(
            prop,
            vec![
                Token {
                    kind: Ident(S("A")),
                    start: pos(0, 0, 0),
                    end: pos(1, 0, 1)
                },
                Token {
                    kind: Conj,
                    start: pos(2, 0, 2),
                    end: pos(5, 0, 3)
                },
                Token {
                    kind: Ident(S("B")),
                    start: pos(6, 0, 4),
                    end: pos(7, 0, 5)
                },
            ]
        );
    }

    #[test]
    fn test_paren1() {
        use TokenKind::*;

        let prop = tokenize("(A) ∧ B").unwrap();
        assert_eq!(
            prop,
            vec![
                Token {
                    kind: LParen,
                    start: pos(0, 0, 0),
                    end: pos(1, 0, 1)
                },
                Token {
                    kind: Ident(S("A")),
                    start: pos(1, 0, 1),
                    end: pos(2, 0, 2)
                },
                Token {
                    kind: RParen,
                    start: pos(2, 0, 2),
                    end: pos(3, 0, 3)
                },
                Token {
                    kind: Conj,
                    start: pos(4, 0, 4),
                    end: pos(7, 0, 5)
                },
                Token {
                    kind: Ident(S("B")),
                    start: pos(8, 0, 6),
                    end: pos(9, 0, 7)
                },
            ]
        );
    }

    #[test]
    fn test_top1() {
        use TokenKind::*;

        let prop = tokenize("⊤").unwrap();
        assert_eq!(
            prop,
            vec![Token {
                kind: Top,
                start: pos(0, 0, 0),
                end: pos(3, 0, 1)
            }]
        );
    }

    #[test]
    fn test_bottom1() {
        use TokenKind::*;

        let prop = tokenize("⊥").unwrap();
        assert_eq!(
            prop,
            vec![Token {
                kind: Bottom,
                start: pos(0, 0, 0),
                end: pos(3, 0, 1)
            }]
        );
    }

    #[test]
    fn test_bottom2() {
        use TokenKind::*;

        let prop = tokenize("⟂").unwrap();
        assert_eq!(
            prop,
            vec![Token {
                kind: Bottom,
                start: pos(0, 0, 0),
                end: pos(3, 0, 1)
            }]
        );
    }

    fn pos(bytepos: usize, line: u32, column: u32) -> Pos {
        Pos {
            bytepos,
            line,
            column,
        }
    }
}
