use crate::ast::Prop;

#[derive(Debug)]
pub struct ParseError;

#[derive(Debug)]
struct Parser<'a> {
    source: &'a str,
    pos: usize,
}

impl<'a> Parser<'a> {
    fn new(source: &'a str) -> Self {
        Self { source, pos: 0 }
    }

    fn parse_ident(&mut self) -> Result<String, ParseError> {
        self.skip_spaces()?;
        let start = self.pos;
        let b = self.next_byte().ok_or_else(|| ParseError)?;
        if !b.is_ascii_alphabetic() {
            return Err(ParseError);
        }
        self.pos += 1;
        while self
            .next_byte()
            .map(|b| b.is_ascii_alphanumeric())
            .unwrap_or(false)
        {
            self.pos += 1;
        }
        let end = self.pos;
        Ok(self.source[start..end].to_owned())
    }

    fn parse_prop(&mut self) -> Result<Prop, ParseError> {
        let ident = self.parse_ident()?;
        Ok(Prop::Atom(ident))
    }

    fn skip_spaces(&mut self) -> Result<(), ParseError> {
        while self
            .next_byte()
            .map(|b| b.is_ascii_whitespace())
            .unwrap_or(false)
        {
            self.pos += 1;
        }
        Ok(())
    }

    fn parse_eof(&mut self) -> Result<(), ParseError> {
        self.skip_spaces()?;
        if self.pos < self.source.len() {
            return Err(ParseError);
        }
        Ok(())
    }

    fn next_byte(&self) -> Option<u8> {
        self.source.as_bytes().get(self.pos).copied()
    }
}

pub fn parse_prop(s: &str) -> Result<Prop, ParseError> {
    let mut parser = Parser::new(s);
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

    #[allow(non_snake_case)]
    fn S(s: &str) -> String {
        s.to_owned()
    }
}
