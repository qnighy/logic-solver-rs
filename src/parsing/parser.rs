use super::ast::Prop;
use super::error::ParseError;
use super::tokenizer::{tokenize, Token, TokenKind};

#[derive(Debug)]
struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    fn parse_primary_prop(&mut self) -> Result<Prop, ParseError> {
        match self.tokens[self.pos].kind {
            TokenKind::Ident(ref ident) => {
                self.pos += 1;
                Ok(Prop::Atom(ident.clone()))
            }
            TokenKind::LParen => {
                self.pos += 1;
                let prop = self.parse_prop()?;
                if let TokenKind::RParen = self.tokens[self.pos].kind {
                    self.pos += 1;
                    Ok(prop)
                } else {
                    Err(ParseError::unexpected_token(&self.tokens[self.pos]))
                }
            }
            TokenKind::Top => {
                self.pos += 1;
                Ok(Prop::Conj(vec![]))
            }
            TokenKind::Bottom => {
                self.pos += 1;
                Ok(Prop::Disj(vec![]))
            }
            _ => Err(ParseError::unexpected_token(&self.tokens[self.pos])),
        }
    }

    fn parse_neg_prop(&mut self) -> Result<Prop, ParseError> {
        match self.tokens[self.pos].kind {
            TokenKind::Not => {
                self.pos += 1;
                let sub = self.parse_neg_prop()?;
                Ok(Prop::Neg(Box::new(sub)))
            }
            _ => self.parse_primary_prop(),
        }
    }

    fn parse_conj_prop(&mut self) -> Result<Prop, ParseError> {
        let base_prop = self.parse_neg_prop()?;
        match self.tokens[self.pos].kind {
            TokenKind::Conj => {
                let mut props = vec![base_prop];
                while let TokenKind::Conj = self.tokens[self.pos].kind {
                    self.pos += 1;
                    props.push(self.parse_neg_prop()?);
                }
                Ok(Prop::Conj(props))
            }
            _ => Ok(base_prop),
        }
    }

    fn parse_disj_prop(&mut self) -> Result<Prop, ParseError> {
        let base_prop = self.parse_conj_prop()?;
        match self.tokens[self.pos].kind {
            TokenKind::Disj => {
                let mut props = vec![base_prop];
                while let TokenKind::Disj = self.tokens[self.pos].kind {
                    self.pos += 1;
                    props.push(self.parse_conj_prop()?);
                }
                Ok(Prop::Disj(props))
            }
            _ => Ok(base_prop),
        }
    }

    fn parse_equiv_prop(&mut self) -> Result<Prop, ParseError> {
        let base_prop = self.parse_disj_prop()?;
        match self.tokens[self.pos].kind {
            TokenKind::Iff => {
                self.pos += 1;
                let rhs = self.parse_disj_prop()?;
                Ok(Prop::Equiv(Box::new(base_prop), Box::new(rhs)))
            }
            _ => Ok(base_prop),
        }
    }

    fn parse_prop(&mut self) -> Result<Prop, ParseError> {
        let base_prop = self.parse_equiv_prop()?;
        match self.tokens[self.pos].kind {
            TokenKind::Arrow => {
                self.pos += 1;
                let rhs = self.parse_prop()?;
                Ok(Prop::Impl(Box::new(base_prop), Box::new(rhs)))
            }
            _ => Ok(base_prop),
        }
    }

    fn parse_eof(&mut self) -> Result<(), ParseError> {
        if let TokenKind::Eof = self.tokens[self.pos].kind {
            Ok(())
        } else {
            Err(ParseError::unexpected_token(&self.tokens[self.pos]))
        }
    }
}

pub fn parse_prop(s: &str) -> Result<Prop, ParseError> {
    let tokens = tokenize(s);
    let mut parser = Parser::new(tokens);
    let prop = parser.parse_prop()?;
    parser.parse_eof()?;
    Ok(prop)
}

#[cfg(test)]
mod tests {
    use super::*;
    use big_s::S;

    #[test]
    fn test_atom1() {
        use Prop::*;

        let prop = parse_prop("A").unwrap();
        assert_eq!(prop, Atom(S("A")));
    }

    #[test]
    fn test_atom2() {
        use Prop::*;

        let prop = parse_prop("foo23").unwrap();
        assert_eq!(prop, Atom(S("foo23")));
    }

    #[test]
    fn test_atom3() {
        let err = parse_prop("foo23+").unwrap_err();
        assert_eq!(
            err.to_string(),
            "parse error at line 1, column 6: invalid characters: \"+\""
        );
    }

    #[test]
    fn test_atom4() {
        use Prop::*;

        let prop = parse_prop("\nfoo23 ").unwrap();
        assert_eq!(prop, Atom(S("foo23")));
    }

    #[test]
    fn test_conj1() {
        use Prop::*;

        let prop = parse_prop("A ∧ B").unwrap();
        assert_eq!(prop, Conj(vec![Atom(S("A")), Atom(S("B"))]));
    }

    #[test]
    fn test_conj2() {
        use Prop::*;

        let prop = parse_prop("A ∧ B ∧ C").unwrap();
        assert_eq!(prop, Conj(vec![Atom(S("A")), Atom(S("B")), Atom(S("C"))]));
    }

    #[test]
    fn test_conj3() {
        let err = parse_prop("A ∧ B ∧").unwrap_err();
        assert_eq!(
            err.to_string(),
            "parse error at line 1, column 8: unexpected token: Eof"
        );
    }

    #[test]
    fn test_conj4() {
        let err = parse_prop("A ∧ ∧ B").unwrap_err();
        assert_eq!(
            err.to_string(),
            "parse error at line 1, column 5: unexpected token: Conj"
        );
    }

    #[test]
    fn test_disj1() {
        use Prop::*;

        let prop = parse_prop("A ∨ B").unwrap();
        assert_eq!(prop, Disj(vec![Atom(S("A")), Atom(S("B"))]));
    }

    #[test]
    fn test_disj2() {
        use Prop::*;

        let prop = parse_prop("A ∨ B ∨ C").unwrap();
        assert_eq!(prop, Disj(vec![Atom(S("A")), Atom(S("B")), Atom(S("C"))]));
    }

    #[test]
    fn test_disj3() {
        let err = parse_prop("A ∨ B ∨").unwrap_err();
        assert_eq!(
            err.to_string(),
            "parse error at line 1, column 8: unexpected token: Eof"
        );
    }

    #[test]
    fn test_disj4() {
        let err = parse_prop("A ∨ ∨ B").unwrap_err();
        assert_eq!(
            err.to_string(),
            "parse error at line 1, column 5: unexpected token: Disj"
        );
    }

    #[test]
    fn test_conj_disj1() {
        use Prop::*;

        let prop = parse_prop("A ∧ B ∨ C").unwrap();
        assert_eq!(
            prop,
            Disj(vec![Conj(vec![Atom(S("A")), Atom(S("B"))]), Atom(S("C"))])
        );
    }

    #[test]
    fn test_conj_disj2() {
        use Prop::*;

        let prop = parse_prop("A ∨ B ∧ C").unwrap();
        assert_eq!(
            prop,
            Disj(vec![Atom(S("A")), Conj(vec![Atom(S("B")), Atom(S("C"))])])
        );
    }

    #[test]
    fn test_top1() {
        use Prop::*;

        let prop = parse_prop("⊤").unwrap();
        assert_eq!(prop, Conj(vec![]));
    }

    #[test]
    fn test_top2() {
        use Prop::*;

        let prop = parse_prop("A ∧ ⊤").unwrap();
        assert_eq!(prop, Conj(vec![Atom(S("A")), Conj(vec![])]));
    }

    #[test]
    fn test_bottom1() {
        use Prop::*;

        let prop = parse_prop("⊥").unwrap();
        assert_eq!(prop, Disj(vec![]));
    }

    #[test]
    fn test_bottom2() {
        use Prop::*;

        let prop = parse_prop("A ∧ ⊥").unwrap();
        assert_eq!(prop, Conj(vec![Atom(S("A")), Disj(vec![])]));
    }

    #[test]
    fn test_impl1() {
        use Prop::*;

        let prop = parse_prop("A → B").unwrap();
        assert_eq!(prop, impl_(Atom(S("A")), Atom(S("B"))));
    }

    #[test]
    fn test_impl2() {
        use Prop::*;

        let prop = parse_prop("A → B → C").unwrap();
        assert_eq!(prop, impl_(Atom(S("A")), impl_(Atom(S("B")), Atom(S("C")))));
    }

    #[test]
    fn test_impl3() {
        use Prop::*;

        let prop = parse_prop("(A → B) → C").unwrap();
        assert_eq!(prop, impl_(impl_(Atom(S("A")), Atom(S("B"))), Atom(S("C"))));
    }

    #[test]
    fn test_impl4() {
        use Prop::*;

        let prop = parse_prop("A ∨ B → C ∨ D").unwrap();
        assert_eq!(
            prop,
            impl_(
                Disj(vec![Atom(S("A")), Atom(S("B"))]),
                Disj(vec![Atom(S("C")), Atom(S("D"))])
            )
        );
    }

    #[test]
    fn test_paren1() {
        use Prop::*;

        let prop = parse_prop("(A)").unwrap();
        assert_eq!(prop, Atom(S("A")));
    }

    #[test]
    fn test_paren2() {
        use Prop::*;

        let prop = parse_prop("(A ∧ B)").unwrap();
        assert_eq!(prop, Conj(vec![Atom(S("A")), Atom(S("B"))]));
    }

    #[test]
    fn test_paren3() {
        use Prop::*;

        let prop = parse_prop("(A ∧ B) ∧ C").unwrap();
        assert_eq!(
            prop,
            Conj(vec![Conj(vec![Atom(S("A")), Atom(S("B"))]), Atom(S("C"))])
        );
    }

    #[test]
    fn test_paren4() {
        use Prop::*;

        let prop = parse_prop("A ∧ (B ∧ C)").unwrap();
        assert_eq!(
            prop,
            Conj(vec![Atom(S("A")), Conj(vec![Atom(S("B")), Atom(S("C"))])])
        );
    }

    #[test]
    fn test_paren5() {
        let err = parse_prop("(B").unwrap_err();
        assert_eq!(
            err.to_string(),
            "parse error at line 1, column 3: unexpected token: Eof"
        );
    }

    #[test]
    fn test_paren6() {
        let err = parse_prop("B(").unwrap_err();
        assert_eq!(
            err.to_string(),
            "parse error at line 1, column 2: unexpected token: LParen"
        );
    }

    #[test]
    fn test_paren7() {
        let err = parse_prop(")A").unwrap_err();
        assert_eq!(
            err.to_string(),
            "parse error at line 1, column 1: unexpected token: RParen"
        );
    }

    #[test]
    fn test_paren8() {
        let err = parse_prop("A)").unwrap_err();
        assert_eq!(
            err.to_string(),
            "parse error at line 1, column 2: unexpected token: RParen"
        );
    }

    #[test]
    fn test_paren9() {
        use Prop::*;

        let prop = parse_prop("A ∧ (B ∨ C)").unwrap();
        assert_eq!(
            prop,
            Conj(vec![Atom(S("A")), Disj(vec![Atom(S("B")), Atom(S("C"))])])
        );
    }

    #[test]
    fn test_neg1() {
        use Prop::*;

        let prop = parse_prop("¬A").unwrap();
        assert_eq!(prop, Neg(Box::new(Atom(S("A")))));
    }

    #[test]
    fn test_neg2() {
        use Prop::*;

        let prop = parse_prop("¬A ∧ ¬B").unwrap();
        assert_eq!(
            prop,
            Conj(vec![
                Neg(Box::new(Atom(S("A")))),
                Neg(Box::new(Atom(S("B"))))
            ])
        );
    }

    #[test]
    fn test_neg3() {
        use Prop::*;

        let prop = parse_prop("¬¬(A∨¬A)").unwrap();
        assert_eq!(
            prop,
            Neg(Box::new(Neg(Box::new(Disj(vec![
                Atom(S("A")),
                Neg(Box::new(Atom(S("A"))))
            ])))))
        );
    }

    #[test]
    fn test_iff1() {
        use Prop::*;

        let prop = parse_prop("A⇔B").unwrap();
        assert_eq!(prop, Equiv(Box::new(Atom(S("A"))), Box::new(Atom(S("B")))));
    }

    #[test]
    fn test_iff2() {
        use Prop::*;

        let prop = parse_prop("A⇔B→C").unwrap();
        assert_eq!(
            prop,
            Impl(
                Box::new(Equiv(Box::new(Atom(S("A"))), Box::new(Atom(S("B"))))),
                Box::new(Atom(S("C")))
            )
        );
    }

    #[test]
    fn test_iff3() {
        use Prop::*;

        let prop = parse_prop("A→B⇔C").unwrap();
        assert_eq!(
            prop,
            Impl(
                Box::new(Atom(S("A"))),
                Box::new(Equiv(Box::new(Atom(S("B"))), Box::new(Atom(S("C")))))
            )
        );
    }

    #[test]
    fn test_iff4() {
        use Prop::*;

        let prop = parse_prop("A∨B⇔B∨A").unwrap();
        assert_eq!(
            prop,
            Equiv(
                Box::new(Disj(vec![Atom(S("A")), Atom(S("B"))])),
                Box::new(Disj(vec![Atom(S("B")), Atom(S("A"))]))
            )
        );
    }

    #[test]
    fn test_iff5() {
        let err = parse_prop("A⇔B⇔C").unwrap_err();
        assert_eq!(
            err.to_string(),
            "parse error at line 1, column 4: unexpected token: Iff"
        );
    }

    fn impl_(lhs: Prop, rhs: Prop) -> Prop {
        Prop::Impl(Box::new(lhs), Box::new(rhs))
    }
}
