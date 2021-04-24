#[derive(Debug)]
pub struct ParseError;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    Ident(String),
    LParen,
    RParen,
    Conj,
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
        Ok(self.source[start..end].to_owned())
    }

    fn parse_token(&mut self) -> Result<Option<Token>, ParseError> {
        self.skip_spaces()?;
        let next = if let Some(next) = self.peek() {
            next
        } else {
            return Ok(None);
        };
        if next.is_ascii_alphabetic() {
            let ident = self.parse_ident()?;
            Ok(Some(Token::Ident(ident)))
        } else if next == '∧' {
            self.bump();
            Ok(Some(Token::Conj))
        } else if next == '(' {
            self.bump();
            Ok(Some(Token::LParen))
        } else if next == ')' {
            self.bump();
            Ok(Some(Token::RParen))
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
        self.source[self.pos..].chars().next()
    }

    fn bump(&mut self) -> Option<char> {
        let mut iter = self.source[self.pos..].chars();
        let ret = iter.next();
        self.pos = self.source.len() - iter.as_str().len();
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
        assert_eq!(prop, vec![Ident(S("foo23"))]);
    }

    #[test]
    fn test_conj1() {
        use Token::*;

        let prop = tokenize("∧").unwrap();
        assert_eq!(prop, vec![Conj]);
    }

    #[test]
    fn test_conj2() {
        use Token::*;

        let prop = tokenize("A ∧ B").unwrap();
        assert_eq!(prop, vec![Ident(S("A")), Conj, Ident(S("B"))]);
    }

    #[test]
    fn test_paren1() {
        use Token::*;

        let prop = tokenize("(A) ∧ B").unwrap();
        assert_eq!(
            prop,
            vec![LParen, Ident(S("A")), RParen, Conj, Ident(S("B"))]
        );
    }

    #[allow(non_snake_case)]
    fn S(s: &str) -> String {
        s.to_owned()
    }
}
