#[derive(Debug)]
pub struct ParseError;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    Ident(String),
}

#[derive(Debug)]
struct Tokenizer<'a> {
    source: &'a str,
    pos: usize,
}

impl<'a> Tokenizer<'a> {
    fn new(source: &'a str) -> Self {
        Self { source, pos: 0 }
    }

    fn parse_ident(&mut self) -> Result<String, ParseError> {
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

    fn parse_token(&mut self) -> Result<Option<Token>, ParseError> {
        self.skip_spaces()?;
        if self.pos == self.source.len() {
            return Ok(None);
        }
        let ident = self.parse_ident()?;
        Ok(Some(Token::Ident(ident)))
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

    fn next_byte(&self) -> Option<u8> {
        self.source.as_bytes().get(self.pos).copied()
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

    #[test]
    fn test_atom1() {
        use Token::*;

        let prop = tokenize("A").unwrap();
        assert_eq!(prop, vec![Ident(S("A"))]);
    }

    #[test]
    fn test_atom2() {
        use Token::*;

        let prop = tokenize("foo23").unwrap();
        assert_eq!(prop, vec![Ident(S("foo23"))]);
    }

    #[test]
    fn test_atom3() {
        tokenize("foo23+").unwrap_err();
    }

    #[test]
    fn test_atom4() {
        use Token::*;

        let prop = tokenize("\nfoo23 ").unwrap();
        assert_eq!(prop, vec![Ident(S("foo23"))])
    }

    #[allow(non_snake_case)]
    fn S(s: &str) -> String {
        s.to_owned()
    }
}
