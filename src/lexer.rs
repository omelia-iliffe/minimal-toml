use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
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

    #[token("[", |_| '[')]
    #[token("]", |_| ']')]
    SquareBracket(char),

    #[token("{", |_| '{')]
    #[token("}", |_| '}')]
    CurlyBracket(char),

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

pub fn lex(input: &str) -> logos::Lexer<Token> {
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
                (Token::SquareBracket('['), "["),
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
                (Token::SquareBracket(']'), "]"),
                (Token::EOL, "\n"),
                (Token::BareString, "contributors"),
                (Token::Equals, "="),
                (Token::SquareBracket('['), "["),
                (Token::EOL, "\n"),
                (Token::QuotedString, "\"Foo Bar <foo@example.com>\""),
                (Token::Comma, ","),
                (Token::EOL, "\n"),
                (Token::CurlyBracket('{'), "{"),
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
                (Token::CurlyBracket('}'), "}"),
                (Token::EOL, "\n"),
                (Token::SquareBracket(']'), "]"),
                (Token::EOL, "\n"),
            ],
        );
    }
}
