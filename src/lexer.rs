use logos::Logos;

use crate::{Error, ErrorKind, Expected};
use core::ops::Range;

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

pub fn lex(input: &str) -> logos::Lexer<'_, Token> {
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

pub struct LexerIterator<'a> {
    lexer: Lexer<'a>,
}

impl<'a> LexerIterator<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self { lexer }
    }

    pub fn inner(&self) -> &Lexer<'a> {
        &self.lexer
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TokenItem {
    pub token: Token,
    pub range: Range<usize>,
}

impl<'a> TokenItem {
    pub fn new(t: Token, r: Range<usize>) -> Self {
        Self { token: t, range: r }
    }

    pub fn expect(self, expected: Token) -> crate::error::Result<()> {
        if self.token == expected {
            Ok(())
        } else {
            let expected = match expected {
                Token::Comment => Expected::Token(Token::Comment),
                Token::BareString => Expected::Token(Token::BareString),
                Token::Comma => Expected::Token(Token::Comma),
                Token::Equals => Expected::Token(Token::Equals),
                Token::Error => Expected::Token(Token::Error),
                Token::LiteralString => Expected::Token(Token::LiteralString),
                Token::DoubleSquareBracket(t) => Expected::Token(Token::DoubleSquareBracket(t)),
                Token::SquareBracket(t) => Expected::Token(Token::SquareBracket(t)),
                Token::CurlyBracket(t) => Expected::Token(Token::CurlyBracket(t)),
                Token::LiteralMutlilineString => Expected::Token(Token::LiteralMutlilineString),
                Token::MutlilineString => Expected::Token(Token::MutlilineString),
                Token::Eol => Expected::Token(Token::Eol),
                Token::NumberLit => Expected::Token(Token::NumberLit),
                Token::BoolLit(_) => Expected::Bool,
                Token::Period => Expected::Token(Token::Period),
                Token::QuotedString => Expected::Token(Token::QuotedString),
            };

            Err(Error::new(
                self.range,
                ErrorKind::UnexpectedToken(self.token, expected),
            ))
        }
    }
}

macro_rules! my_match {
   ($obj:expr, $($matcher:pat $(if $pred:expr)* => $result:expr),*) => {
       match $obj {
           $($matcher $(if $pred)* => $result),*
       }
   }
}

fn a() {
    let x = 7;
    let s = my_match! {
        x,
        10 => "Ten",
        n if x < 5 => "Less than 5",
        _ => "something else"
    };
    println!("s = {:?}", s); // "Something else"
}

#[macro_export]
macro_rules! expect_next {
    ($self:ident, $expected:expr, $($matcher:pat $(if $pred:expr)* => $result:expr),*) => {
        {
            let item = $self.next()?;
            match item.token {
                 $($matcher $(if $pred)* => $result),*
                ,
                _ =>
                    return $crate::error::Result::Err(
                        $crate::error::Error::new(
                            item.range,
                            $crate::error::ErrorKind::UnexpectedToken(item.token, $expected)
                        )
                    ),
            }
        }
    };
}

impl<'a> Iterator for LexerIterator<'a> {
    type Item = TokenItem;

    fn next(&mut self) -> Option<Self::Item> {
        self.lexer
            .next()
            .map(|t| TokenItem::new(t, self.lexer.span()))
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
}
