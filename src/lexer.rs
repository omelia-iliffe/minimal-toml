use logos::Logos;

pub type Lexer<'a> = logos::Lexer<'a, Token>;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BracketType {
    Open,
    Close,
}

#[derive(Logos, Copy, Clone, Debug, PartialEq, Eq)]
pub enum Token {
    #[regex("[A-Za-z0-9_-]+")]
    BareString,

    #[regex("#[^\n|\r\n]*")]
    Comment,

    #[token(",")]
    Comma,

    #[token("=")]
    Equals,

    #[error]
    //Skip whitespace
    #[regex("[ |\t]+", logos::skip, priority = 2)]
    Error,

    #[regex("'[^']*'")]
    LiteralString,

    #[token("[", |_| BracketType::Open)]
    #[token("]", |_| BracketType::Close)]
    SquareBracket(BracketType),

    #[token("{", |_| BracketType::Open)]
    #[token("}", |_| BracketType::Close)]
    CurlyBracket(BracketType),

    #[regex("'''[^']*'''", priority = 3)]
    LiteralMutlilineString,

    #[regex("\"\"\"[^\"]*\"\"\"", priority = 3)]
    MutlilineString,

    #[regex("[\n|\r\n]")]
    EOL,

    #[regex("[0-9\\.]+", priority = 5)]
    NumberLit,

    #[token("true", |_| true)]
    #[token("false", |_| false)]
    BoolLit(bool),

    #[token(".")]
    Period,

    #[regex("\"[^\"]*\"", priority = 2)]
    QuotedString,
}

pub fn lex<'a>(input: &'a str) -> logos::Lexer<'a, Token> {
    Token::lexer(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_lex(toml: &'static str, expected_tokens: Vec<(Token, &'static str)>) {
        let mut lex = Token::lexer(toml);

        for (expected_token, expected_str) in expected_tokens {
            let token = lex.next();
            println!("cmp: {} to {}", lex.slice().trim(), expected_str.trim());
            assert_eq!(token, Some(expected_token));
            assert_eq!(expected_str, lex.slice());
        }
        assert_eq!(lex.next(), None);
    }
    #[test]
    fn lex1() {
        test_lex(
            r#"#bare_str
    # This is a comment

    #^ Empty line above

            "#,
            vec![
                (Token::Comment, "#bare_str"),
                (Token::EOL, "\n"),
                (Token::Comment, "# This is a comment"),
                (Token::EOL, "\n"),
                (Token::EOL, "\n"),
                (Token::Comment, "#^ Empty line above"),
                (Token::EOL, "\n"),
                (Token::EOL, "\n"),
            ],
        );
    }

    #[test]
    fn lex2() {
        test_lex(
            r#"
    server = "test"
    "my fav number" = 7
    "127.0.0.1" = 15
            "#,
            vec![
                (Token::EOL, "\n"),
                (Token::BareString, "server"),
                (Token::Equals, "="),
                (Token::QuotedString, "\"test\""),
                (Token::EOL, "\n"),
                (Token::QuotedString, "\"my fav number\""),
                (Token::Equals, "="),
                (Token::NumberLit, "7"),
                (Token::EOL, "\n"),
                (Token::QuotedString, "\"127.0.0.1\""),
                (Token::Equals, "="),
                (Token::NumberLit, "15"),
                (Token::EOL, "\n"),
            ],
        );
    }

    #[test]
    fn lex3() {
        test_lex(
            "test = \"test\"\na\n#ABC\na",
            vec![
                (Token::BareString, "test"),
                (Token::Equals, "="),
                (Token::QuotedString, "\"test\""),
                (Token::EOL, "\n"),
                (Token::BareString, "a"),
                (Token::EOL, "\n"),
                (Token::Comment, "#ABC"),
                (Token::EOL, "\n"),
                (Token::BareString, "a"),
            ],
        );
    }

    #[test]
    fn lex4() {
        test_lex(
            r#"
             """multi line test
  please parse these newlines
   """ = '''
me too!
            '''
            "#,
            vec![
                (Token::EOL, "\n"),
                (
                    Token::MutlilineString,
                    "\"\"\"multi line test\n  please parse these newlines\n   \"\"\"",
                ),
                (Token::Equals, "="),
                (
                    Token::LiteralMutlilineString,
                    "'''\nme too!\n            '''",
                ),
                (Token::EOL, "\n"),
            ],
        );
    }

    #[test]
    fn lex5() {
        test_lex(
            r#"
             
numbers = [ 0.1, 0.2, 0.5, 1, 2, 5 ]
contributors = [
  "Foo Bar <foo@example.com>",
  { name = "Baz Qux", email = "bazqux@example.com", url = "https://example.com/bazqux" }
]
            "#,
            vec![
                (Token::EOL, "\n"),
                (Token::EOL, "\n"),
                (Token::BareString, "numbers"),
                (Token::Equals, "="),
                (Token::SquareBracket(BracketType::Open), "["),
                (Token::NumberLit, "0.1"),
                (Token::Comma, ","),
                (Token::NumberLit, "0.2"),
                (Token::Comma, ","),
                (Token::NumberLit, "0.5"),
                (Token::Comma, ","),
                (Token::NumberLit, "1"),
                (Token::Comma, ","),
                (Token::NumberLit, "2"),
                (Token::Comma, ","),
                (Token::NumberLit, "5"),
                (Token::SquareBracket(BracketType::Close), "]"),
                (Token::EOL, "\n"),
                (Token::BareString, "contributors"),
                (Token::Equals, "="),
                (Token::SquareBracket(BracketType::Open), "["),
                (Token::EOL, "\n"),
                (Token::QuotedString, "\"Foo Bar <foo@example.com>\""),
                (Token::Comma, ","),
                (Token::EOL, "\n"),
                (Token::CurlyBracket(BracketType::Open), "{"),
                (Token::BareString, "name"),
                (Token::Equals, "="),
                (Token::QuotedString, "\"Baz Qux\""),
                (Token::Comma, ","),
                (Token::BareString, "email"),
                (Token::Equals, "="),
                (Token::QuotedString, "\"bazqux@example.com\""),
                (Token::Comma, ","),
                (Token::BareString, "url"),
                (Token::Equals, "="),
                (Token::QuotedString, "\"https://example.com/bazqux\""),
                (Token::CurlyBracket(BracketType::Close), "}"),
                (Token::EOL, "\n"),
                (Token::SquareBracket(BracketType::Close), "]"),
                (Token::EOL, "\n"),
            ],
        );
    }
}

/// Taken from core::iter::Peekable but with the ability to get the inner iterator
#[derive(Clone, Debug)]
#[must_use = "iterators are lazy and do nothing unless consumed"]
pub struct Peekable<I: Iterator> {
    iter: I,
    /// Remember a peeked value, even if it was None.
    peeked: Option<Option<I::Item>>,
}

impl<I: Iterator> Peekable<I> {
    pub fn new(iter: I) -> Peekable<I> {
        Peekable { iter, peeked: None }
    }
}

// Peekable must remember if a None has been seen in the `.peek()` method.
// It ensures that `.peek(); .peek();` or `.peek(); .next();` only advances the
// underlying iterator at most once. This does not by itself make the iterator
// fused.
impl<I: Iterator> Iterator for Peekable<I> {
    type Item = I::Item;

    fn next(&mut self) -> Option<I::Item> {
        match self.peeked.take() {
            Some(v) => v,
            None => self.iter.next(),
        }
    }

    fn count(mut self) -> usize {
        match self.peeked.take() {
            Some(None) => 0,
            Some(Some(_)) => 1 + self.iter.count(),
            None => self.iter.count(),
        }
    }

    fn nth(&mut self, n: usize) -> Option<I::Item> {
        match self.peeked.take() {
            Some(None) => None,
            Some(v @ Some(_)) if n == 0 => v,
            Some(Some(_)) => self.iter.nth(n - 1),
            None => self.iter.nth(n),
        }
    }

    fn last(mut self) -> Option<I::Item> {
        let peek_opt = match self.peeked.take() {
            Some(None) => return None,
            Some(v) => v,
            None => None,
        };
        self.iter.last().or(peek_opt)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let peek_len = match self.peeked {
            Some(None) => return (0, Some(0)),
            Some(Some(_)) => 1,
            None => 0,
        };
        let (lo, hi) = self.iter.size_hint();
        let lo = lo.saturating_add(peek_len);
        let hi = match hi {
            Some(x) => x.checked_add(peek_len),
            None => None,
        };
        (lo, hi)
    }
}

impl<I: Iterator> Peekable<I> {
    pub fn inner<'a>(&'a self) -> &'a I {
        &self.iter
    }

    pub fn inner_mut<'a>(&'a mut self) -> &'a mut I {
        &mut self.iter
    }
}

impl<I: Iterator> Peekable<I> {
    pub fn peek<'a>(&'a mut self) -> Option<&'a I::Item> {
        let iter = &mut self.iter;
        self.peeked.get_or_insert_with(|| iter.next()).as_ref()
    }
}
