use super::pos::Pos;

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
    Not,
    Iff,
    Unknown(String),
    Eof,
}

const SIMPLE_SYMBOLS: &[&str] = &[
    "→", "∧", "∨", "⊤", "⊥", /* up tack */
    "⟂", /* perpendicular */
    "¬", "⇔", "(", ")", "->", "/\\", "\\/", "~", "<=>",
];

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

    fn parse_ident(&mut self) -> String {
        let start = self.pos;
        let ch = self.peek().unwrap();
        assert!(ch.is_ascii_alphabetic());
        self.bump();
        while self
            .peek()
            .map(|b| b.is_ascii_alphanumeric())
            .unwrap_or(false)
        {
            self.bump();
        }
        let end = self.pos;
        self.source[start.bytepos..end.bytepos].to_owned()
    }

    fn parse_token(&mut self) -> Option<Token> {
        self.skip_spaces();
        let start = self.pos;
        let kind = self.parse_token_kind()?;
        let end = self.pos;
        Some(Token { kind, start, end })
    }

    fn parse_token_kind(&mut self) -> Option<TokenKind> {
        if let Some(sym) = SIMPLE_SYMBOLS.iter().copied().find(|sym| self.is_next(sym)) {
            let kind = match sym {
                "→" | "->" => TokenKind::Arrow,
                "∧" | "/\\" => TokenKind::Conj,
                "∨" | "\\/" => TokenKind::Disj,
                "⊤" => TokenKind::Top,
                "⊥" /* up tack */ |
                "⟂" /* perpendicular */ => TokenKind::Bottom,
                "¬" | "~" => TokenKind::Not,
                "⇔" | "<=>" => TokenKind::Iff,
                "(" => TokenKind::LParen,
                ")" => TokenKind::RParen,
                _ => unreachable!(),
            };
            self.bump_str(sym);
            return Some(kind);
        }
        let next = self.peek()?;
        if next.is_ascii_alphabetic() {
            let ident = self.parse_ident();
            Some(TokenKind::Ident(ident))
        } else if next == '\\' {
            let start = self.pos;
            self.bump().unwrap();
            if let Some(kind) = self.parse_after_backslash() {
                Some(kind)
            } else {
                let end = self.pos;
                let s = self.source[start.bytepos..end.bytepos].to_owned();
                Some(TokenKind::Unknown(s))
            }
        } else {
            let start = self.pos;
            self.bump().unwrap();
            while let Some(ch) = self.peek() {
                if ch.is_ascii_alphabetic() || ch.is_ascii_whitespace() || "→∧∨⊤⊥⟂()".contains(ch)
                {
                    break;
                }
                self.bump();
            }
            let end = self.pos;
            let s = self.source[start.bytepos..end.bytepos].to_owned();
            Some(TokenKind::Unknown(s))
        }
    }

    fn parse_after_backslash(&mut self) -> Option<TokenKind> {
        let next = self.peek()?;
        if next.is_ascii_alphabetic() {
            let ident = self.parse_ident();
            let kind = match ident.as_str() {
                "top" => TokenKind::Top,
                "bot" => TokenKind::Bottom,
                _ => return None,
            };
            return Some(kind);
        }
        None
    }

    fn skip_spaces(&mut self) {
        while self
            .peek()
            .map(|b| b.is_ascii_whitespace())
            .unwrap_or(false)
        {
            self.bump();
        }
    }

    fn peek(&self) -> Option<char> {
        self.source[self.pos.bytepos..].chars().next()
    }

    fn is_next(&self, s: &str) -> bool {
        self.source[self.pos.bytepos..].starts_with(s)
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

    fn bump_str(&mut self, s: &str) {
        debug_assert!(self.is_next(s));
        self.pos.bytepos += s.len();
        for c in s.chars() {
            if c == '\n' {
                self.pos.line += 1;
                self.pos.column = 0;
            } else {
                self.pos.column += 1;
            }
        }
    }
}

pub fn tokenize(s: &str) -> Vec<Token> {
    let mut tokenizer = Tokenizer::new(s);
    let mut tokens = Vec::new();
    while let Some(token) = tokenizer.parse_token() {
        tokens.push(token);
    }
    tokens.push(Token {
        kind: TokenKind::Eof,
        start: tokenizer.pos,
        end: tokenizer.pos,
    });
    tokens
}

#[cfg(test)]
mod tests {
    use super::*;
    use big_s::S;

    #[test]
    fn test_atom1() {
        use TokenKind::*;

        let prop = tokenize("A");
        assert_eq!(
            prop,
            vec![
                Token {
                    kind: Ident(S("A")),
                    start: pos(0, 0, 0),
                    end: pos(1, 0, 1),
                },
                eof(1, 0, 1),
            ]
        );
    }

    #[test]
    fn test_atom2() {
        use TokenKind::*;

        let prop = tokenize("foo23");
        assert_eq!(
            prop,
            vec![
                Token {
                    kind: Ident(S("foo23")),
                    start: pos(0, 0, 0),
                    end: pos(5, 0, 5),
                },
                eof(5, 0, 5),
            ]
        );
    }

    #[test]
    fn test_atom3() {
        use TokenKind::*;

        let tokens = tokenize("foo23+");
        assert_eq!(
            tokens,
            vec![
                Token {
                    kind: Ident(S("foo23")),
                    start: pos(0, 0, 0),
                    end: pos(5, 0, 5),
                },
                Token {
                    kind: Unknown(S("+")),
                    start: pos(5, 0, 5),
                    end: pos(6, 0, 6),
                },
                eof(6, 0, 6),
            ]
        );
    }

    #[test]
    fn test_atom4() {
        use TokenKind::*;

        let prop = tokenize("\nfoo23 ");
        assert_eq!(
            prop,
            vec![
                Token {
                    kind: Ident(S("foo23")),
                    start: pos(1, 1, 0),
                    end: pos(6, 1, 5)
                },
                eof(7, 1, 6),
            ]
        );
    }

    #[test]
    fn test_arrow() {
        use TokenKind::*;

        let prop = tokenize("→");
        assert_eq!(
            prop,
            vec![
                Token {
                    kind: Arrow,
                    start: pos(0, 0, 0),
                    end: pos(3, 0, 1)
                },
                eof(3, 0, 1),
            ]
        );
    }

    #[test]
    fn test_conj1() {
        use TokenKind::*;

        let prop = tokenize("∧");
        assert_eq!(
            prop,
            vec![
                Token {
                    kind: Conj,
                    start: pos(0, 0, 0),
                    end: pos(3, 0, 1)
                },
                eof(3, 0, 1),
            ]
        );
    }

    #[test]
    fn test_conj2() {
        use TokenKind::*;

        let prop = tokenize("A ∧ B");
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
                eof(7, 0, 5),
            ]
        );
    }

    #[test]
    fn test_paren1() {
        use TokenKind::*;

        let prop = tokenize("(A) ∧ B");
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
                eof(9, 0, 7),
            ]
        );
    }

    #[test]
    fn test_top1() {
        use TokenKind::*;

        let prop = tokenize("⊤");
        assert_eq!(
            prop,
            vec![
                Token {
                    kind: Top,
                    start: pos(0, 0, 0),
                    end: pos(3, 0, 1)
                },
                eof(3, 0, 1),
            ]
        );
    }

    #[test]
    fn test_bottom1() {
        use TokenKind::*;

        let prop = tokenize("⊥");
        assert_eq!(
            prop,
            vec![
                Token {
                    kind: Bottom,
                    start: pos(0, 0, 0),
                    end: pos(3, 0, 1)
                },
                eof(3, 0, 1),
            ]
        );
    }

    #[test]
    fn test_bottom2() {
        use TokenKind::*;

        let prop = tokenize("⟂");
        assert_eq!(
            prop,
            vec![
                Token {
                    kind: Bottom,
                    start: pos(0, 0, 0),
                    end: pos(3, 0, 1)
                },
                eof(3, 0, 1),
            ]
        );
    }

    #[test]
    fn test_not1() {
        use TokenKind::*;

        let prop = tokenize("¬");
        assert_eq!(
            prop,
            vec![
                Token {
                    kind: Not,
                    start: pos(0, 0, 0),
                    end: pos(2, 0, 1)
                },
                eof(2, 0, 1),
            ]
        );
    }

    #[test]
    fn test_iff1() {
        use TokenKind::*;

        let prop = tokenize("⇔");
        assert_eq!(
            prop,
            vec![
                Token {
                    kind: Iff,
                    start: pos(0, 0, 0),
                    end: pos(3, 0, 1)
                },
                eof(3, 0, 1),
            ]
        );
    }

    fn eof(bytepos: usize, line: u32, column: u32) -> Token {
        Token {
            kind: TokenKind::Eof,
            start: pos(bytepos, line, column),
            end: pos(bytepos, line, column),
        }
    }

    fn pos(bytepos: usize, line: u32, column: u32) -> Pos {
        Pos {
            bytepos,
            line,
            column,
        }
    }

    #[test]
    fn test_ascii_tokens1() {
        use TokenKind::*;

        let prop = tokenize("\\bot-><=>\\//\\~\\top");
        assert_eq!(
            prop,
            vec![
                Token {
                    kind: Bottom,
                    start: pos(0, 0, 0),
                    end: pos(4, 0, 4),
                },
                Token {
                    kind: Arrow,
                    start: pos(4, 0, 4),
                    end: pos(6, 0, 6),
                },
                Token {
                    kind: Iff,
                    start: pos(6, 0, 6),
                    end: pos(9, 0, 9),
                },
                Token {
                    kind: Disj,
                    start: pos(9, 0, 9),
                    end: pos(11, 0, 11),
                },
                Token {
                    kind: Conj,
                    start: pos(11, 0, 11),
                    end: pos(13, 0, 13),
                },
                Token {
                    kind: Not,
                    start: pos(13, 0, 13),
                    end: pos(14, 0, 14),
                },
                Token {
                    kind: Top,
                    start: pos(14, 0, 14),
                    end: pos(18, 0, 18),
                },
                eof(18, 0, 18),
            ]
        );
    }

    #[test]
    fn test_ascii_tokens2() {
        use TokenKind::*;

        let prop = tokenize("\\botw");
        assert_eq!(
            prop,
            vec![
                Token {
                    kind: Unknown(S("\\botw")),
                    start: pos(0, 0, 0),
                    end: pos(5, 0, 5),
                },
                eof(5, 0, 5),
            ]
        );
    }
}
