use core::str::FromStr;
use serde::de::{
    Deserialize, DeserializeSeed, EnumAccess, MapAccess, SeqAccess, VariantAccess, Visitor,
};
use serde::Deserializer as SerdeDeserializer;

use crate::error::{Error, ErrorKind, Expected};
use crate::lexer::{self, BracketType::*, Lexer, Peekable, Token};

pub struct Deserializer<'de> {
    // This string starts with the input data and characters are truncated off
    // the beginning as data is parsed.
    pub(crate) input: &'de str,
    tokens: Peekable<Lexer<'de>>,
    depth: usize,
}

impl<'de> Deserializer<'de> {
    pub fn from_str(input: &'de str) -> Self {
        Deserializer {
            input,
            tokens: Peekable::new(lexer::lex(input)),
            depth: 0,
        }
    }
}

pub type Result<T> = core::result::Result<T, Error>;

#[cfg(test)]
use println as p;

#[cfg(not(test))]
macro_rules! p {
    () => {};
    ($($arg:tt)*) => {};
}

macro_rules! dbg {
    () => {
        p!("[{}:{}]", file!(), line!())
    };
    ($val:expr $(,)?) => {
        // Use of `match` here is intentional because it affects the lifetimes
        // of temporaries - https://stackoverflow.com/a/48732525/1063961
        match $val {
            tmp => {
                p!("[{}:{}] {} = {:#?}",
                    file!(), line!(), stringify!($val), &tmp);
                tmp
            }
        }
    };
    ($($val:expr),+ $(,)?) => {
        ($(dbg!($val)),+,)
    };
}

pub fn from_str<'a, T>(s: &'a str) -> Result<T>
where
    T: Deserialize<'a>,
{
    let mut de = Deserializer::from_str(s);
    let t = T::deserialize(&mut de)?;
    Ok(t)
}

macro_rules! expect_token {
    ($self:ident, $pattern:pat_param, $expected:expr) => {
        match $self.next()? {
            $pattern => {}
            _ => return $self.error(ErrorKind::UnexpectedToken($expected)),
        }
    };
}

macro_rules! expect_eol_or_eof {
    ($self:ident) => {
        match $self.next() {
            Ok(token) => match token {
                Token::Eol => {}
                _ => return $self.error(ErrorKind::UnexpectedToken(Expected::EolOrEof)),
            },
            Err(_eof) => {}
        }
    };
}

impl<'de> Deserializer<'de> {
    /*
    fn deserialize_r_value<V>(&mut self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let token = self.peek()?;
        p!("r value token: {:?}", token);
        match token {
            Token::QuotedString => self.deserialize_str(visitor),
            Token::LiteralString => self.deserialize_str(visitor),
            Token::MutlilineString => self.deserialize_str(visitor),
            Token::LiteralMutlilineString => self.deserialize_str(visitor),
            Token::NumberLit => self.deserialize_u64(visitor),
            _ => self.error(ErrorKind::UnexpectedToken(Expected::RValue)),
        }
    }
    */

    fn error<T>(&self, kind: ErrorKind) -> Result<T> {
        return Err(Error::new(&self.tokens.inner(), kind));
    }

    fn peek<'a>(&'a mut self) -> Result<Token> {
        match self.tokens.peek() {
            Some(t) => Ok(t.clone()),
            None => self.error(ErrorKind::MissingToken),
        }
    }

    fn next(&mut self) -> Result<Token> {
        match self.tokens.next() {
            Some(t) => Ok(t),
            None => self.error(ErrorKind::MissingToken),
        }
    }

    /// Consumes any whitespace tokens in the token buffer. Returns Err(...) When out of tokens, or
    /// Ok(()) to indicate that the next token is valid and non-whitespace
    fn consume_whitespace_and_comments(&mut self) -> Result<()> {
        loop {
            let _ = match self.peek()? {
                //Tabs and spaces are consumed implicitly by the lexer so we only need to eat Eol here
                Token::Eol | Token::Comment => self.next().unwrap(),
                _ => return Ok(()),
            };
        }
    }

    fn parse_string(&mut self) -> Result<&'de str> {
        self.peek()?;
        let base = self.tokens.inner().slice();
        let base_span = self.tokens.inner().span();
        let span = match self.next()? {
            // Return a slice of the actual string data
            Token::BareString => base,
            Token::QuotedString => &base[1..base_span.len() - 1],
            Token::MutlilineString => &base[3..base_span.len() - 3],
            _ => return self.error(ErrorKind::UnexpectedToken(Expected::String)),
        };
        //TODO: Check if there are any escape sequences because we can't support them
        p!("De str: {}", span);
        Ok(span)
    }

    fn parse_bool(&mut self) -> Result<bool> {
        //expect_token!(self, Token::BoolLit(_), Expected::Bool);
        match self.next()? {
            Token::BoolLit(value) => {
                p!("Got bool {}", value);
                Ok(value)
            }
            _ => self.error(ErrorKind::UnexpectedToken(Expected::Bool)),
        }
    }
}

macro_rules! deserialize_int_from_str {
    ($self:ident, $visitor:ident, $typ:ident, $visit_fn:ident) => {{
        $self.next()?;
        let s = $self.tokens.inner().slice();
        let v = $typ::from_str(s)
            .map_err(|e| Error::new($self.tokens.inner(), ErrorKind::InvalidInteger(e)))?;

        $visitor.$visit_fn(v)
    }};
}

macro_rules! deserialize_float_from_str {
    ($self:ident, $visitor:ident, $typ:ident, $visit_fn:ident) => {{
        $self.next()?;
        let s = $self.tokens.inner().slice();
        let v = $typ::from_str(s)
            .map_err(|e| Error::new($self.tokens.inner(), ErrorKind::InvalidFloat(e)))?;

        $visitor.$visit_fn(v)
    }};
}

impl<'de, 'a> SerdeDeserializer<'de> for &'a mut Deserializer<'de> {
    type Error = Error;

    // Entry point of parsing. Looks at token and decides what to do
    fn deserialize_any<V>(mut self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        panic!("Test");
        self.consume_whitespace_and_comments()?;
        let token = self.peek()?;
        p!("Got any token: {:?}", token);
        let value = match token {
            /*
            Token::BareString | Token::QuotedString => {
                //Key-value pairs
                p!("Visiting map based on {}", self.tokens.inner().slice());
                visitor.visit_map(KeyValuePairs::new(&mut self))?
            }
            */
            Token::CurlyBracket(Open) => {
                //Literal one line map
                self.depth += 1;
                let value = visitor.visit_map(CommaSeparated::new(&mut self))?;
                // Parse the closing bracket of the sequence.
                expect_token!(
                    self,
                    Token::CurlyBracket(Close),
                    Expected::Token(Token::CurlyBracket(Close))
                );
                self.depth -= 1;

                value
            }
            /*
            Token::SquareBracket(Open) => {
                //Field is a substruct Eg:
                //[wg]
                //field1 = 7

                let value = visitor.visit_map(SubStructSeparated::new(&mut self))?;
                // Parse the closing bracket of the sequence.
                expect_token!(
                    self,
                    Token::SquareBracket(Close),
                    Expected::Token(Token::SquareBracket(Close))
                );

                value
            }
            */
            _ => return Err(Error::unexpected(self.tokens.inner(), Expected::MapStart)),
        };
        Ok(value)
    }

    /*
        let token = self.peek()?;
        p!("Any got token {:?}", token);
        match token {
            Token::EOL | Token::Comment => {
                self.next().unwrap();
                self.deserialize_any(visitor)
            }
            Token::SquareBracket(Open) => self.deserialize_map(visitor),
            Token::BareString => self.deserialize_assignment(visitor),
            Token::QuotedString => self.deserialize_assignment(visitor),
            Token::Error => self.error(ErrorKind::UnknownToken),
            _ => self.error(ErrorKind::UnexpectedToken(Expected::LineStart)),
        }
    }*/

    // Uses the `parse_bool` parsing function defined above to read the JSON
    // identifier `true` or `false` from the input.
    //
    // Parsing refers to looking at the input and deciding that it contains the
    // JSON value `true` or `false`.
    //
    // Deserialization refers to mapping that JSON value into Serde's data
    // model by invoking one of the `Visitor` methods. In the case of JSON and
    // bool that mapping is straightforward so the distinction may seem silly,
    // but in other cases Deserializers sometimes perform non-obvious mappings.
    // For example the TOML format has a Datetime type and Serde's data model
    // does not. In the `toml` crate, a Datetime in the input is deserialized by
    // mapping it to a Serde data model "struct" type with a special name and a
    // single field containing the Datetime represented as a string.
    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        dbg!();
        visitor.visit_bool(self.parse_bool()?)
    }

    // The `parse_signed` function is generic over the integer type `T` so here
    // it is invoked with `T=i8`. The next 8 methods are similar.
    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        dbg!();
        deserialize_int_from_str!(self, visitor, i8, visit_i8)
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        dbg!();
        deserialize_int_from_str!(self, visitor, i16, visit_i16)
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        dbg!();
        deserialize_int_from_str!(self, visitor, i32, visit_i32)
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        dbg!();
        deserialize_int_from_str!(self, visitor, i64, visit_i64)
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        deserialize_int_from_str!(self, visitor, u8, visit_u8)
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        dbg!();
        deserialize_int_from_str!(self, visitor, u16, visit_u16)
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        dbg!();
        deserialize_int_from_str!(self, visitor, u32, visit_u32)
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        dbg!();
        deserialize_int_from_str!(self, visitor, u64, visit_u64)
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        dbg!();
        deserialize_float_from_str!(self, visitor, f32, visit_f32)
    }

    // Float parsing is stupidly hard.
    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        dbg!();
        deserialize_float_from_str!(self, visitor, f64, visit_f64)
    }

    // The `Serializer` implementation on the previous page serialized chars as
    // single-character strings so handle that representation here.
    fn deserialize_char<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        dbg!();
        // Parse a string, check that it is one character, call `visit_char`.
        unimplemented!()
    }

    // Refer to the "Understanding deserializer lifetimes" page for information
    // about the three deserialization flavors of strings in Serde.
    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        dbg!();
        visitor.visit_borrowed_str(self.parse_string()?)
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        dbg!();
        self.deserialize_str(visitor)
    }

    fn deserialize_bytes<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        dbg!();
        unimplemented!()
    }

    fn deserialize_byte_buf<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        dbg!();
        unimplemented!()
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        dbg!();
        visitor.visit_some(self)
    }

    // In Serde, unit means an anonymous value containing no data.
    fn deserialize_unit<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        dbg!();
        unimplemented!()
    }

    // Unit struct means a named value containing no data.
    fn deserialize_unit_struct<V>(self, _name: &'static str, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        dbg!();
        self.deserialize_unit(visitor)
    }

    // As is done here, serializers are encouraged to treat newtype structs as
    // insignificant wrappers around the data they contain. That means not
    // parsing anything other than the contained value.
    fn deserialize_newtype_struct<V>(self, _name: &'static str, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        dbg!();
        visitor.visit_newtype_struct(self)
    }

    // Deserialization of compound types like sequences and maps happens by
    // passing the visitor an "Access" object that gives it the ability to
    // iterate through the data contained in the sequence.
    fn deserialize_seq<V>(mut self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        dbg!();
        expect_token!(
            self,
            Token::SquareBracket(Open),
            Expected::Token(Token::SquareBracket(Open))
        );
        // Give the visitor access to each element of the sequence.
        self.depth += 1;
        let value = visitor.visit_seq(CommaSeparated::new(&mut self))?;
        self.depth -= 1;
        // Parse the closing bracket of the sequence.
        expect_token!(
            self,
            Token::SquareBracket(Close),
            Expected::Token(Token::SquareBracket(Close))
        );
        Ok(value)
    }

    // Tuples look just like sequences in JSON. Some formats may be able to
    // represent tuples more efficiently.
    //
    // As indicated by the length parameter, the `Deserialize` implementation
    // for a tuple in the Serde data model is required to know the length of the
    // tuple before even looking at the input data.
    fn deserialize_tuple<V>(self, _len: usize, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        dbg!();
        self.deserialize_seq(visitor)
    }

    // Tuple structs look just like sequences in JSON.
    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        dbg!();
        self.deserialize_seq(visitor)
    }

    // Much like `deserialize_seq` but calls the visitors `visit_map` method
    // with a `MapAccess` implementation, rather than the visitor's `visit_seq`
    // method with a `SeqAccess` implementation.
    fn deserialize_map<V>(mut self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        p!("deserialize_map");
        self.depth += 1;
        let r = visitor.visit_map(KeyValuePairs::new(&mut self));
        self.depth -= 1;
        r
    }

    // Structs look just like maps in JSON.
    //
    // Notice the `fields` parameter - a "struct" in the Serde data model means
    // that the `Deserialize` implementation is required to know what the fields
    // are before even looking at the input data. Any key-value pairing in which
    // the fields cannot be known ahead of time is probably a map.
    fn deserialize_struct<V>(
        mut self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        p!("De struct {}: {:?}", _name, _fields);
        self.depth += 1;
        let r = visitor.visit_map(KeyValuePairs::new(&mut self));
        self.depth -= 1;
        r
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        _visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        dbg!();
        unimplemented!()
        /*
        if self.peek_char()? == '"' {
            // Visit a unit variant.
            visitor.visit_enum(self.parse_string()?.into_deserializer())
        } else if self.next_char()? == '{' {
            // Visit a newtype variant, tuple variant, or struct variant.
            let value = visitor.visit_enum(Enum::new(self))?;
            // Parse the matching close brace.
            if self.next_char()? == '}' {
                Ok(value)
            } else {
                Err(Error::ExpectedMapEnd)
            }
        } else {
            Err(Error::ExpectedEnum)
        }
        */
    }

    // An identifier in Serde is the type that identifies a field of a struct or
    // the variant of an enum. In JSON, struct fields and enum variants are
    // represented as strings. In other formats they may be represented as
    // numeric indices.
    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        dbg!();
        self.deserialize_str(visitor)
    }

    // Like `deserialize_any` but indicates to the `Deserializer` that it makes
    // no difference which `Visitor` method is called because the data is
    // ignored.
    //
    // Some deserializers are able to implement this more efficiently than
    // `deserialize_any`, for example by rapidly skipping over matched
    // delimiters without paying close attention to the data in between.
    //
    // Some formats are not able to implement this at all. Formats that can
    // implement `deserialize_any` and `deserialize_ignored_any` are known as
    // self-describing.
    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.consume_whitespace_and_comments()?;
        let token = self.next()?;

        match token {
            Token::BoolLit(_)
            | Token::NumberLit
            | Token::QuotedString
            | Token::LiteralString
            | Token::MutlilineString
            | Token::LiteralMutlilineString => {}
            _ => unimplemented!("deserialize_ignored_any is not implemented for {:?}", token),
        }

        visitor.visit_none()
    }
}

// In order to handle commas correctly when deserializing a JSON array or map,
// we need to track whether we are on the first element or past the first
// element.
struct CommaSeparated<'a, 'de: 'a> {
    de: &'a mut Deserializer<'de>,
    first: bool,
}

impl<'a, 'de> CommaSeparated<'a, 'de> {
    fn new(de: &'a mut Deserializer<'de>) -> Self {
        Self { de, first: true }
    }
}

// `SeqAccess` is provided to the `Visitor` to give it the ability to iterate
// through elements of the sequence.
impl<'de, 'a> SeqAccess<'de> for CommaSeparated<'a, 'de> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>>
    where
        T: DeserializeSeed<'de>,
    {
        dbg!();
        // Check if there are no more elements.
        match self.de.peek()? {
            Token::SquareBracket(Close) => return Ok(None),
            _ => {}
        }

        // Comma is required before every element except the first.
        if !self.first {
            let de_self = &mut self.de;
            expect_token!(de_self, Token::Comma, Expected::Token(Token::Comma));
        }
        self.first = false;
        // Deserialize an array element.
        seed.deserialize(&mut *self.de).map(Some)
    }
}

// `MapAccess` is provided to the `Visitor` to give it the ability to iterate
// through entries of the map.
impl<'de, 'a> MapAccess<'de> for CommaSeparated<'a, 'de> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>>
    where
        K: DeserializeSeed<'de>,
    {
        dbg!();
        // Check if there are no more elements.
        match self.de.peek()? {
            Token::CurlyBracket(Close) => return Ok(None),
            _ => {}
        }

        // Comma is required before every entry except the first.
        if !self.first {
            let de_self = &mut self.de;
            expect_token!(de_self, Token::Comma, Expected::Token(Token::Comma));
        }
        self.first = false;
        // Deserialize a map key.
        seed.deserialize(&mut *self.de).map(Some)
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value>
    where
        V: DeserializeSeed<'de>,
    {
        dbg!();
        let de_self = &mut self.de;
        expect_token!(de_self, Token::Equals, Expected::Token(Token::Equals));
        // Deserialize a map value.
        seed.deserialize(&mut *self.de)
    }
}

struct KeyValuePairs<'a, 'de: 'a> {
    de: &'a mut Deserializer<'de>,
    first: bool,
}

impl<'a, 'de> KeyValuePairs<'a, 'de> {
    fn new(de: &'a mut Deserializer<'de>) -> Self {
        Self { de, first: true }
    }
}

impl<'de, 'a> MapAccess<'de> for KeyValuePairs<'a, 'de> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>>
    where
        K: DeserializeSeed<'de>,
    {
        //Here we are getting the name of a particular key. This can either be a "key_name = 'value'" pair line
        //Or a table, where the value is a sub-struct with more key-value pairs
        dbg!();
        let de_self = &mut self.de;
        if de_self.consume_whitespace_and_comments().is_err() {
            //consume_whitespace_and_comments returns error on end of stream
            p!("Got end of stream while reading next entry");
            return Ok(None);
        }
        let token = de_self.peek().unwrap();
        p!("Key: {:?} - depth: {}", token, de_self.depth);
        let result = match token {
            //Simple key value pair - parse name
            Token::BareString => seed.deserialize(&mut *self.de).map(Some),
            Token::SquareBracket(Open) => {
                p!("Got square bracket open. First: {}", self.first);
                if !self.first && de_self.depth > 1 {
                    //Start of new table that is not part of our entries
                    p!("Got new table - breaking");
                    return Ok(None);
                }
                //Table in format:
                //[...]
                //<struct data>

                //Consume [
                de_self.next().unwrap();
                expect_token!(
                    de_self,
                    Token::BareString,
                    Expected::Token(Token::BareString)
                );
                //Read string within [...]
                let result = seed
                    .deserialize(SingleStrDeserializer {
                        s: de_self.tokens.inner().slice(),
                    })
                    .map(Some);

                expect_token!(
                    de_self,
                    Token::SquareBracket(Close),
                    Expected::Token(Token::SquareBracket(Close))
                );
                de_self.consume_whitespace_and_comments()?;

                result
            }
            _ => unimplemented!(),
        };
        self.first = false;
        result
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value>
    where
        V: DeserializeSeed<'de>,
    {
        dbg!();
        let de_self = &mut *self.de;
        let initial_token = de_self.peek()?;
        p!("Initial token: {:?}", initial_token);

        let allow_consume_whitespace = match initial_token {
            Token::Equals => {
                //Consume equals
                let _ = de_self.next().unwrap();
                false
            }
            Token::BareString => true,
            _ => return Err(Error::unexpected(de_self.tokens.inner(), Expected::Value)),
        };
        let result = seed.deserialize(&mut *self.de);
        if self.de.depth <= 1 && allow_consume_whitespace {
            p!("Consuming whitespace after other value");
            //Failure indicates end of stream so dont fail here because we will retry in next_key_seed,
            //see the end of stream there, then end the map
            let _ = self.de.consume_whitespace_and_comments();
        } else {
            //Normal key-value pair - Consume end of line
            let de_self = &mut *self.de;
            expect_eol_or_eof!(de_self);
        }

        result
    }
}

struct SingleStrDeserializer<'a> {
    s: &'a str,
}

impl<'de, 'a> SerdeDeserializer<'de> for SingleStrDeserializer<'a> {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_str(self.s)
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct map struct enum identifier ignored_any
    }
}

/*
struct SubStructSeparated<'a, 'de: 'a> {
    de: &'a mut Deserializer<'de>,
}

impl<'a, 'de> SubStructSeparated<'a, 'de> {
    fn new(de: &'a mut Deserializer<'de>) -> Self {
        Self { de }
    }
}

impl<'de, 'a> MapAccess<'de> for SubStructSeparated<'a, 'de> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>>
    where
        K: DeserializeSeed<'de>,
    {
        dbg!();
        let de_self = &mut self.de;
        let token = match de_self.peek() {
            Ok(t) => t,
            Err(_eol) => {
                p!("Got end of stream");
                return Ok(None);
            }
        };
        p!("Key: {:?}", token);
        let result = match token {
            Token::BareString => seed.deserialize(&mut *self.de).map(Some),
            _ => unimplemented!(),
        };
        // Deserialize a map key.
        p!("Got key result");
        result
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value>
    where
        V: DeserializeSeed<'de>,
    {
        dbg!();
        {
            let de_self = &mut *self.de;
            expect_token!(de_self, Token::Equals, Expected::Token(Token::Equals));
        }
        // Deserialize a map value.
        let result = seed.deserialize(&mut *self.de);

        //Consume end of line
        let de_self = &mut *self.de;
        expect_eol_or_eof!(de_self);
        result
    }
}
*/

struct Enum<'a, 'de: 'a> {
    de: &'a mut Deserializer<'de>,
}

impl<'a, 'de> Enum<'a, 'de> {
    fn new(de: &'a mut Deserializer<'de>) -> Self {
        Self { de }
    }
}

// `EnumAccess` is provided to the `Visitor` to give it the ability to determine
// which variant of the enum is supposed to be deserialized.
//
// Note that all enum deserialization methods in Serde refer exclusively to the
// "externally tagged" enum representation.
impl<'de, 'a> EnumAccess<'de> for Enum<'a, 'de> {
    type Error = Error;
    type Variant = Self;

    fn variant_seed<V>(mut self, seed: V) -> Result<(V::Value, Self::Variant)>
    where
        V: DeserializeSeed<'de>,
    {
        dbg!();
        let val = seed.deserialize(&mut *self.de)?;

        let de_self = &mut self.de;
        expect_token!(de_self, Token::Equals, Expected::Token(Token::Equals));
        Ok((val, self))
    }
}

// `VariantAccess` is provided to the `Visitor` to give it the ability to see
// the content of the single variant that it decided to deserialize.
impl<'de, 'a> VariantAccess<'de> for Enum<'a, 'de> {
    type Error = Error;

    // If the `Visitor` expected this variant to be a unit variant, the input
    // should have been the plain string case handled in `deserialize_enum`.
    fn unit_variant(self) -> Result<()> {
        unimplemented!()
    }

    // Newtype variants are represented in JSON as `{ NAME: VALUE }` so
    // deserialize the value here.
    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value>
    where
        T: DeserializeSeed<'de>,
    {
        seed.deserialize(self.de)
    }

    // Tuple variants are represented in JSON as `{ NAME: [DATA...] }` so
    // deserialize the sequence of data here.
    fn tuple_variant<V>(self, _len: usize, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        SerdeDeserializer::deserialize_seq(self.de, visitor)
    }

    // Struct variants are represented in JSON as `{ NAME: { K: V, ... } }` so
    // deserialize the inner map here.
    fn struct_variant<V>(self, _fields: &'static [&'static str], visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        SerdeDeserializer::deserialize_map(self.de, visitor)
    }
}

////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    use super::*;
    use serde::Deserialize;

    fn print_token_error(input: &'static str, err: Error) -> ! {
        let draw_range = 16;
        let start = std::cmp::max(0isize, err.span.start as isize - draw_range) as usize;
        let end = std::cmp::min(input.len(), err.span.end + draw_range as usize);
        println!();
        println!("Error in token");

        let dots = if start != 0 {
            print!("...");
            3
        } else {
            0
        };
        let replaced = input[start..end].replace('\n', "|");
        print!("{}", replaced);
        if end != input.len() {
            print!("...");
        }
        println!();

        for _ in 0..(err.span.start - start + dots) {
            print!(" ");
        }
        for _ in err.span.clone().into_iter() {
            print!("^");
        }
        if err.span.start == input.len() {
            print!("| Missing token");
        }
        println!();
        panic!("Test failed: {}", err)
    }

    fn expect_output<'de, T>(input: &'static str, expected: T)
    where
        T: std::fmt::Debug + PartialEq + Deserialize<'de>,
    {
        let v = match from_str(input) {
            Err(err) => print_token_error(input, err),
            Ok(v) => v,
        };
        assert_eq!(expected, v);
    }

    #[test]
    fn test_struct() {
        #[derive(Deserialize, PartialEq, Debug)]
        struct Test {
            int: u32,
            f: f32,
            ok: bool,
        }

        expect_output(
            "int = 1\n f = 1.0\n ok = true",
            Test {
                int: 1,
                f: 1.0,
                ok: true,
            },
        );
    }

    #[test]
    fn test_mutli_struct() {
        #[derive(Deserialize, PartialEq, Debug)]
        pub struct WgFlags<'a> {
            //pub address: IpNet,
            pub interface_name: &'a str,
            pub persistent_keep_alive: u16,
            pub dns: &'a str,
            pub f: f32,
            //pub handshake_check_interval: Duration,
        }

        #[derive(Deserialize, PartialEq, Debug)]
        pub struct ServerFlags<'a> {
            pub gateway_interface_name_override: Option<&'a str>,
            pub endpoint: Option<&'a str>,
            pub wg_listen_port: u16,
            pub disconnect_listen_port: u16,
            pub http_listen_port: u16,
            pub getip_listen_port: u16,
            pub disable_verify_server_pem: Option<bool>,
            pub disable_client_cert: Option<bool>,
            pub enforce_device_limits: bool,
            pub name: &'a str,
            pub flag: &'a str,
            pub is_staff_server: bool,
            pub flight: u8,
            pub torrenting_allowed: bool,
            pub persist_peers: bool,
        }

        #[derive(Deserialize, PartialEq, Debug)]
        pub struct GCFlags {
            pub initial_allocate_count: u32,
            pub allocate_step: u32,
            pub under_pressure_gc_count: u32,
            pub max_configs: u32,
        }

        #[derive(Deserialize, PartialEq, Debug)]
        pub struct Flags<'a> {
            //ff: f64,
            #[serde(borrow)]
            pub wg: WgFlags<'a>,
            #[serde(borrow)]
            pub server: ServerFlags<'a>,
            pub gc: GCFlags,
        }

        let input = r#"
#ff = 64.0009765625
    [wg]
	#address = "10.10.0.0/16"
	interface_name = "es0"
	persistent_keep_alive = 21
	dns = "1.1.1.1"
        f = -75.8125
	
	#[wg.handshake_check_interval]
	#secs = 1
	#nanos = 0
	
	[server]
	wg_listen_port = 51820
	disconnect_listen_port = 6976
	http_listen_port = 25566
	getip_listen_port = 6977
	enforce_device_limits = true
	use_client_cert = true
	verify_server_pem = true
	name = "Frankfurt"
        flag = "DE"
	is_staff_server = false
	flight = 0
	torrenting_allowed = false
	persist_peers = true
	
	[gc]
	initial_allocate_count = 100
	allocate_step = 25
	under_pressure_gc_count = 10
	max_configs = 1000
    "#;
        let expected = Flags {
            //ff: 64.0009765625,
            wg: WgFlags {
                interface_name: "es0",
                persistent_keep_alive: 21,
                dns: "1.1.1.1",
                f: -75.8125,
            },
            server: ServerFlags {
                gateway_interface_name_override: None,
                endpoint: None,
                wg_listen_port: 51820,
                disconnect_listen_port: 6976,
                http_listen_port: 25566,
                getip_listen_port: 6977,
                disable_verify_server_pem: None,
                disable_client_cert: None,
                enforce_device_limits: true,
                name: "Frankfurt",
                flag: "DE",
                is_staff_server: false,
                flight: 0,
                torrenting_allowed: false,
                persist_peers: true,
            },
            gc: GCFlags {
                initial_allocate_count: 100,
                allocate_step: 25,
                under_pressure_gc_count: 10,
                max_configs: 1000,
            },
        };

        let v: Flags = match from_str(input) {
            Err(err) => print_token_error(input, err),
            Ok(v) => v,
        };
        assert_eq!(expected, v);
    }

    //#[test]
    fn test_enum() {
        #[derive(Deserialize, PartialEq, Debug)]
        enum E {
            Unit,
            Newtype(u32),
            Tuple(u32, u32),
            Struct { a: u32 },
        }

        let j = r#""Unit""#;
        let expected = E::Unit;
        assert_eq!(expected, from_str(j).unwrap());

        let j = r#"{"Newtype":1}"#;
        let expected = E::Newtype(1);
        assert_eq!(expected, from_str(j).unwrap());

        let j = r#"{"Tuple":[1,2]}"#;
        let expected = E::Tuple(1, 2);
        assert_eq!(expected, from_str(j).unwrap());

        let j = r#"{"Struct":{"a":1}}"#;
        let expected = E::Struct { a: 1 };
        assert_eq!(expected, from_str(j).unwrap());
    }
}
