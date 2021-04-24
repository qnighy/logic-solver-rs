use crate::ast::Prop;
use crate::tokenizer::{tokenize, ParseError, Token};

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
        match self.tokens.get(self.pos) {
            Some(&Token::Ident(ref ident)) => {
                self.pos += 1;
                Ok(Prop::Atom(ident.clone()))
            }
            Some(&Token::LParen) => {
                self.pos += 1;
                let prop = self.parse_prop()?;
                if let Some(&Token::RParen) = self.tokens.get(self.pos) {
                    self.pos += 1;
                    Ok(prop)
                } else {
                    Err(ParseError)
                }
            }
            Some(&Token::Top) => {
                self.pos += 1;
                Ok(Prop::Conj(vec![]))
            }
            Some(&Token::Bottom) => {
                self.pos += 1;
                Ok(Prop::Disj(vec![]))
            }
            _ => Err(ParseError),
        }
    }

    fn parse_conj_prop(&mut self) -> Result<Prop, ParseError> {
        let base_prop = self.parse_primary_prop()?;
        match self.tokens.get(self.pos) {
            Some(&Token::Conj) => {
                let mut props = vec![base_prop];
                while let Some(&Token::Conj) = self.tokens.get(self.pos) {
                    self.pos += 1;
                    props.push(self.parse_primary_prop()?);
                }
                Ok(Prop::Conj(props))
            }
            _ => Ok(base_prop),
        }
    }

    fn parse_prop(&mut self) -> Result<Prop, ParseError> {
        let base_prop = self.parse_conj_prop()?;
        match self.tokens.get(self.pos) {
            Some(&Token::Disj) => {
                let mut props = vec![base_prop];
                while let Some(&Token::Disj) = self.tokens.get(self.pos) {
                    self.pos += 1;
                    props.push(self.parse_conj_prop()?);
                }
                Ok(Prop::Disj(props))
            }
            _ => Ok(base_prop),
        }
    }

    fn parse_eof(&mut self) -> Result<(), ParseError> {
        if self.pos < self.tokens.len() {
            return Err(ParseError);
        }
        Ok(())
    }
}

pub fn parse_prop(s: &str) -> Result<Prop, ParseError> {
    let tokens = tokenize(s)?;
    let mut parser = Parser::new(tokens);
    let prop = parser.parse_prop()?;
    parser.parse_eof()?;
    Ok(prop)
}

#[cfg(test)]
mod tests {
    use super::*;

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
        parse_prop("foo23+").unwrap_err();
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
        parse_prop("A ∧ B ∧").unwrap_err();
    }

    #[test]
    fn test_conj4() {
        parse_prop("A ∧ ∧ B").unwrap_err();
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
        parse_prop("A ∨ B ∨").unwrap_err();
    }

    #[test]
    fn test_disj4() {
        parse_prop("A ∨ ∨ B").unwrap_err();
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
        parse_prop("(B").unwrap_err();
    }

    #[test]
    fn test_paren6() {
        parse_prop("B(").unwrap_err();
    }

    #[test]
    fn test_paren7() {
        parse_prop(")A").unwrap_err();
    }

    #[test]
    fn test_paren8() {
        parse_prop("A)").unwrap_err();
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

    #[allow(non_snake_case)]
    fn S(s: &str) -> String {
        s.to_owned()
    }
}
