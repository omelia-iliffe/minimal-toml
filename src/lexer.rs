use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
enum Token {
    #[regex("[A-Za-z0-9_-]+")]
    BareString,

    #[regex("#[^\n|\r\n]*")]
    Comment,

    #[token("=")]
    Equals,

    #[error]
    //Skip whitespace
    #[regex("[ |\t]+", logos::skip, priority = 2)]
    Error,

    #[regex("'[^']*'")]
    LiteralString,

    #[token("[")]
    OpenBracket,

    #[token("]")]
    CloseBracket,

    #[regex("'''[^']*'''", priority = 3)]
    LiteralMutlilineString,

    #[regex("\"\"\"[^\"]*\"\"\"", priority = 3)]
    MutlilineString,

    #[regex("[\n|\r\n]")]
    EOL,

    #[regex("[0-9]+", priority = 2)]
    NumberLit,

    #[token("true")]
    TrueLit,

    #[token("false")]
    FalseLit,

    #[token(".")]
    Period,

    #[regex("\"[^\"]*\"", priority = 2)]
    QuotedString,
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
                (Token::MutlilineString, "\"\"\"multi line test\n  please parse these newlines\n   \"\"\""),
                (Token::Equals, "="),
                (Token::LiteralMutlilineString, "'''\nme too!\n            '''"),
                (Token::EOL, "\n"),
            ],
        );
    }
}
