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
            Some(&Token::Conj) => Err(ParseError),
            None => Err(ParseError),
        }
    }

    fn parse_prop(&mut self) -> Result<Prop, ParseError> {
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
        assert_eq!(prop, Atom(S("A")))
    }

    #[test]
    fn test_atom2() {
        use Prop::*;

        let prop = parse_prop("foo23").unwrap();
        assert_eq!(prop, Atom(S("foo23")))
    }

    #[test]
    fn test_atom3() {
        parse_prop("foo23+").unwrap_err();
    }

    #[test]
    fn test_atom4() {
        use Prop::*;

        let prop = parse_prop("\nfoo23 ").unwrap();
        assert_eq!(prop, Atom(S("foo23")))
    }

    #[test]
    fn test_conj1() {
        use Prop::*;

        let prop = parse_prop("A ∧ B").unwrap();
        assert_eq!(prop, Conj(vec![Atom(S("A")), Atom(S("B"))]))
    }

    #[test]
    fn test_conj2() {
        use Prop::*;

        let prop = parse_prop("A ∧ B ∧ C").unwrap();
        assert_eq!(prop, Conj(vec![Atom(S("A")), Atom(S("B")), Atom(S("C"))]))
    }

    #[test]
    fn test_conj3() {
        parse_prop("A ∧ B ∧").unwrap_err();
    }

    #[test]
    fn test_conj4() {
        parse_prop("A ∧ ∧ B").unwrap_err();
    }

    #[allow(non_snake_case)]
    fn S(s: &str) -> String {
        s.to_owned()
    }
}
