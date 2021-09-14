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

    #[token("[[", |_| BracketType::Open)]
    #[token("]]", |_| BracketType::Close)]
    DoubleSquareBracket(BracketType),

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
    Eol,

    #[regex("[\\-0-9\\.]+", priority = 5)]
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

#[derive(Debug, Clone)]
enum PeekedState<I> {
    None,
    Single(Option<I>),
    Double(I, Option<I>),
}

impl<I> PeekedState<I> {
    /// Takes a peeked value from inside this state returning it if there was one
    fn take(&mut self) -> Option<I> {
        let (i, new_self): (Option<I>, PeekedState<I>) = match self.take_state() {
            PeekedState::None => (None, PeekedState::None),
            PeekedState::Single(i) => (i, PeekedState::None),
            PeekedState::Double(i1, i2) => (Some(i1), PeekedState::Single(i2)),
        };
        *self = new_self;
        i
    }

    fn take_state(&mut self) -> PeekedState<I> {
        core::mem::take(self)
    }
}

impl<I> Default for PeekedState<I> {
    fn default() -> Self {
        Self::None
    }
}

/// Taken from core::iter::Peekable but with the ability to get the inner iterator
#[derive(Clone, Debug)]
#[must_use = "iterators are lazy and do nothing unless consumed"]
pub struct Peekable<I: Iterator> {
    iter: I,
    /// Remember a peeked value, even if it was None.
    state: PeekedState<I::Item>,
}

impl<I: Iterator> Peekable<I> {
    pub fn new(iter: I) -> Peekable<I> {
        Peekable {
            iter,
            state: PeekedState::None,
        }
    }
}

// Peekable must remember if a None has been seen in the `.peek()` method.
// It ensures that `.peek(); .peek();` or `.peek(); .next();` only advances the
// underlying iterator at most once. This does not by itself make the iterator
// fused.
impl<I: Iterator> Iterator for Peekable<I> {
    type Item = I::Item;

    fn next(&mut self) -> Option<I::Item> {
        match self.state.take() {
            Some(v) => Some(v),
            None => self.iter.next(),
        }
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
        match self.state {
            PeekedState::None => {
                let i = self.iter.next();
                self.state = PeekedState::Single(i);
                match &self.state {
                    PeekedState::Single(ref i) => i.as_ref(),
                    _ => unreachable!(),
                }
            }
            PeekedState::Single(ref i) => i.as_ref(),
            PeekedState::Double(ref i, _) => Some(i),
        }
    }

    /// Peeks the second un-consumed value.
    /// [`peek`] must return Some(_) before calling this method, otherwise it will panic
    pub fn double_peek<'a>(&'a mut self) -> Option<&'a I::Item> {
        let state: PeekedState<I::Item> = match self.state.take_state() {
            PeekedState::None => {
                let i1 = self
                    .iter
                    .next()
                    .expect("Caller must ensure that peek returns Some first!");
                let i2 = self.iter.next();
                PeekedState::Double(i1, i2)
            }
            PeekedState::Single(i1) => {
                let i1 = i1.expect("Caller must ensure that peek returns Some first!");

                let i2 = self.iter.next();

                PeekedState::Double(i1, i2)
            }
            PeekedState::Double(i1, i2) => PeekedState::Double(i1, i2),
        };

        self.state = state;
        match &self.state {
            PeekedState::Double(_, i2) => i2.as_ref(),
            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    fn test_lex(toml: &'static str, expected_tokens: Vec<(Token, &'static str)>) {
        let mut lex = Token::lexer(toml);

        for (expected_token, expected_str) in expected_tokens {
            let token = lex.next();
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
                (Token::Eol, "\n"),
                (Token::Comment, "# This is a comment"),
                (Token::Eol, "\n"),
                (Token::Eol, "\n"),
                (Token::Comment, "#^ Empty line above"),
                (Token::Eol, "\n"),
                (Token::Eol, "\n"),
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
                (Token::Eol, "\n"),
                (Token::BareString, "server"),
                (Token::Equals, "="),
                (Token::QuotedString, "\"test\""),
                (Token::Eol, "\n"),
                (Token::QuotedString, "\"my fav number\""),
                (Token::Equals, "="),
                (Token::NumberLit, "7"),
                (Token::Eol, "\n"),
                (Token::QuotedString, "\"127.0.0.1\""),
                (Token::Equals, "="),
                (Token::NumberLit, "15"),
                (Token::Eol, "\n"),
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
                (Token::Eol, "\n"),
                (Token::BareString, "a"),
                (Token::Eol, "\n"),
                (Token::Comment, "#ABC"),
                (Token::Eol, "\n"),
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
                (Token::Eol, "\n"),
                (
                    Token::MutlilineString,
                    "\"\"\"multi line test\n  please parse these newlines\n   \"\"\"",
                ),
                (Token::Equals, "="),
                (
                    Token::LiteralMutlilineString,
                    "'''\nme too!\n            '''",
                ),
                (Token::Eol, "\n"),
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
                (Token::Eol, "\n"),
                (Token::Eol, "\n"),
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
                (Token::Eol, "\n"),
                (Token::BareString, "contributors"),
                (Token::Equals, "="),
                (Token::SquareBracket(BracketType::Open), "["),
                (Token::Eol, "\n"),
                (Token::QuotedString, "\"Foo Bar <foo@example.com>\""),
                (Token::Comma, ","),
                (Token::Eol, "\n"),
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
                (Token::Eol, "\n"),
                (Token::SquareBracket(BracketType::Close), "]"),
                (Token::Eol, "\n"),
            ],
        );
    }

    #[test]
    fn lex6() {
        test_lex(
            r#"
[[]]
a = 3564
b = false"#,
            vec![
                (Token::Eol, "\n"),
                (Token::DoubleSquareBracket(BracketType::Open), "[["),
                (Token::DoubleSquareBracket(BracketType::Close), "]]"),
                (Token::Eol, "\n"),
                (Token::BareString, "a"),
                (Token::Equals, "="),
                (Token::NumberLit, "3564"),
                (Token::Eol, "\n"),
                (Token::BareString, "b"),
                (Token::Equals, "="),
                (Token::BoolLit(false), "false"),
            ],
        );
    }
    #[test]
    fn test_single() {
        let mut p: Peekable<_> = Peekable::new(0..5);
        assert_eq!(p.peek(), Some(&0));
        assert_eq!(p.peek(), Some(&0));
        assert_eq!(p.peek(), Some(&0));
        assert_eq!(p.next(), Some(0));
        assert_eq!(p.next(), Some(1));

        assert_eq!(p.peek(), Some(&2));
        assert_eq!(p.peek(), Some(&2));
        assert_eq!(p.next(), Some(2));
    }
}
