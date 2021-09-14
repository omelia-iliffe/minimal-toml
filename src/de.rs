use core::convert::{AsMut, AsRef};
use core::str::FromStr;
use serde::de::{
    Deserialize, DeserializeSeed, EnumAccess, IntoDeserializer, MapAccess, SeqAccess,
    VariantAccess, Visitor,
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
    in_array_table: Option<&'de str>,
}

impl<'de> Deserializer<'de> {
    pub fn from_str(input: &'de str) -> Self {
        Deserializer {
            input,
            tokens: Peekable::new(lexer::lex(input)),
            depth: 0,
            in_array_table: None,
        }
    }
}

pub type Result<T> = core::result::Result<T, Error>;

#[cfg(all(debug_assertions, feature = "std"))]
use println as p;

#[cfg(not(debug_assertions))]
macro_rules! p {
    () => {};
    ($($arg:tt)*) => {};
}

#[cfg(not(feature = "std"))]
use libc_print::libc_println as p;

#[cfg(not(feature = "std"))]
use libc_print::libc_print as print;

#[cfg(debug_assertions)]
macro_rules! p2 {
    ($self:ident, $($arg:tt)*) => ({
        for _ in 0..($self.depth.clone()) {
            print!("  ");
        }
        p!($($arg)*);
    })
}

#[cfg(not(debug_assertions))]
macro_rules! p2 {
    ($self:ident, $($arg:tt)*) => {};
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
            t => return $self.error(ErrorKind::UnexpectedToken(t, $expected)),
        }
    };
}

macro_rules! expect_eol_or_eof {
    ($self:ident) => {
        match $self.next() {
            Ok(token) => match token {
                Token::Eol => {}
                t => return $self.error(ErrorKind::UnexpectedToken(t, Expected::EolOrEof)),
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
        return Err(Error::new(self.tokens.inner(), kind));
    }

    fn peek(&mut self) -> Result<Token> {
        match self.tokens.peek() {
            Some(t) => match t {
                Token::Error => self.error(ErrorKind::FailedToLex),
                t => Ok(*t),
            },
            None => self.error(ErrorKind::MissingToken),
        }
    }

    fn double_peek(&mut self) -> Result<Token> {
        match self.tokens.double_peek() {
            Some(t) => match t {
                Token::Error => self.error(ErrorKind::FailedToLex),
                t => Ok(*t),
            },
            None => self.error(ErrorKind::MissingToken),
        }
    }

    fn next(&mut self) -> Result<Token> {
        match self.tokens.next() {
            Some(t) => match t {
                Token::Error => self.error(ErrorKind::FailedToLex),
                t => Ok(t),
            },
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
            t => return self.error(ErrorKind::UnexpectedToken(t, Expected::String)),
        };
        //TODO: Check if there are any escape sequences because we can't support them
        Ok(span)
    }

    fn parse_bool(&mut self) -> Result<bool> {
        //expect_token!(self, Token::BoolLit(_), Expected::Bool);
        match self.next()? {
            Token::BoolLit(value) => {
                p2!(self, "Got bool {}", value);
                Ok(value)
            }
            t => self.error(ErrorKind::UnexpectedToken(t, Expected::Bool)),
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

impl<'de, 'a> AsRef<logos::Lexer<'de, Token>> for &'a Deserializer<'de> {
    fn as_ref(&self) -> &logos::Lexer<'de, Token> {
        self.tokens.inner()
    }
}

impl<'de> AsMut<logos::Lexer<'de, Token>> for &mut Deserializer<'de> {
    fn as_mut(&mut self) -> &mut logos::Lexer<'de, Token> {
        self.tokens.inner_mut()
    }
}

impl<'de, 'a> SerdeDeserializer<'de> for &'a mut Deserializer<'de> {
    type Error = Error;

    // Entry point of parsing. Looks at token and decides what to do
    fn deserialize_any<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        p2!(self, "~deserialize_bool");
        visitor.visit_bool(self.parse_bool()?)
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        p2!(self, "~deserialize_i8");
        deserialize_int_from_str!(self, visitor, i8, visit_i8)
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        p2!(self, "~deserialize_i16");
        deserialize_int_from_str!(self, visitor, i16, visit_i16)
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        p2!(self, "~deserialize_i32");
        deserialize_int_from_str!(self, visitor, i32, visit_i32)
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        p2!(self, "~deserialize_i64");
        deserialize_int_from_str!(self, visitor, i64, visit_i64)
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        p2!(self, "~deserialize_u8");
        deserialize_int_from_str!(self, visitor, u8, visit_u8)
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        p2!(self, "~deserialize_u16");
        deserialize_int_from_str!(self, visitor, u16, visit_u16)
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        p2!(self, "~deserialize_u32");
        deserialize_int_from_str!(self, visitor, u32, visit_u32)
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        p2!(self, "~deserialize_u64");
        deserialize_int_from_str!(self, visitor, u64, visit_u64)
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        p2!(self, "~deserialize_f32");
        deserialize_float_from_str!(self, visitor, f32, visit_f32)
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        p2!(self, "~deserialize_f64");
        deserialize_float_from_str!(self, visitor, f64, visit_f64)
    }

    fn deserialize_char<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        p2!(self, "~deserialize_char");
        // Parse a string, check that it is one character, call `visit_char`.
        unimplemented!()
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        p2!(self, "~deserialize_str");
        visitor.visit_borrowed_str(self.parse_string()?)
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_bytes<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        p2!(self, "~deserialize_bytes");
        unimplemented!()
    }

    fn deserialize_byte_buf<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        p2!(self, "~deserialize_byte_buf");
        unimplemented!()
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        p2!(self, "~deserialize_option");
        visitor.visit_some(self)
    }

    fn deserialize_unit<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        p2!(self, "~deserialize_unit");
        unimplemented!()
    }

    fn deserialize_unit_struct<V>(self, _name: &'static str, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        p2!(self, "~deserialize_unit_struct");
        self.deserialize_unit(visitor)
    }

    // As is done here, serializers are encouraged to treat newtype structs as
    // insignificant wrappers around the data they contain. That means not
    // parsing anything other than the contained value.
    fn deserialize_newtype_struct<V>(self, _name: &'static str, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        p2!(self, "~deserialize_newtype_struct");
        visitor.visit_newtype_struct(self)
    }

    fn deserialize_seq<V>(mut self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        p2!(self, "~deserialize_seq");
        let value = match self.tokens.peek().copied() {
            Some(Token::SquareBracket(Open)) => {
                expect_token!(
                    self,
                    Token::SquareBracket(Open),
                    Expected::Token(Token::SquareBracket(Open))
                );
                // Give the visitor access to each element of the sequence.
                self.depth += 1;
                let v = visitor.visit_seq(CommaSeparated::new(&mut self))?;
                self.depth -= 1;
                // Parse the closing bracket of the sequence.
                expect_token!(
                    self,
                    Token::SquareBracket(Close),
                    Expected::Token(Token::SquareBracket(Close))
                );
                v
            }
            Some(Token::DoubleSquareBracket(Open)) => {
                self.depth += 1;
                let v = visitor.visit_seq(ArrayOfTables::new(&mut self))?;
                self.depth -= 1;

                v
            }
            Some(token) => return Err(Error::unexpected(self.as_mut(), token, Expected::SeqStart)),
            None => return Err(Error::new(self.as_mut(), ErrorKind::MissingToken)),
        };

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
        p2!(self, "~deserialize_tuple");
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
        p2!(self, "~deserialize_tuple_struct");
        self.deserialize_seq(visitor)
    }

    // Much like `deserialize_seq` but calls the visitors `visit_map` method
    // with a `MapAccess` implementation, rather than the visitor's `visit_seq`
    // method with a `SeqAccess` implementation.
    fn deserialize_map<V>(mut self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        p2!(self, "~deserialize_map");
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
        name: &'static str,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        p2!(self, "~deserialize_struct {} {:?}", name, fields);
        self.depth += 1;
        let r = visitor.visit_map(KeyValuePairs::new_expecting_struct(
            &mut self,
            StructInfo { name, fields },
        ));
        self.depth -= 1;
        r
    }

    fn deserialize_enum<V>(
        mut self,
        _enum_name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        p2!(self, "~deserialize_enum {} {:?}", _enum_name, _variants);
        //An enum is in the format <Enum name> = "Normal_Variant"
        //Or <Enum Name> = {
        let initial_token = self.peek()?;
        match initial_token {
            Token::BareString | Token::QuotedString => {
                visitor.visit_enum(self.parse_string()?.into_deserializer())
            }
            t => Err(Error::unexpected(self.as_mut(), t, Expected::Enum)),
        }
    }

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        p2!(self, "~deserialize_identifier");
        self.deserialize_str(visitor)
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        p2!(self, "~deserialize_ignored_any");
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
        if let Token::SquareBracket(Close) = self.de.peek()? {
            return Ok(None);
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
        if let Token::CurlyBracket(Close) = self.de.peek()? {
            return Ok(None);
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
        self.de.depth += 1;
        let r = seed.deserialize(&mut *self.de);
        self.de.depth -= 1;
        r
    }
}

struct StructInfo {
    name: &'static str,
    fields: &'static [&'static str],
}

struct KeyValuePairs<'a, 'de: 'a> {
    de: &'a mut Deserializer<'de>,
    first: bool,
    struct_info: Option<StructInfo>,
}

impl<'a, 'de> KeyValuePairs<'a, 'de> {
    fn new(de: &'a mut Deserializer<'de>) -> Self {
        Self {
            de,
            first: true,
            struct_info: None,
        }
    }

    fn new_expecting_struct(de: &'a mut Deserializer<'de>, info: StructInfo) -> Self {
        Self {
            de,
            first: true,
            struct_info: Some(info),
        }
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
        let de_self = &mut self.de;
        p2!(de_self, "~next_key_seed (KeyValuePairs)");
        if de_self.consume_whitespace_and_comments().is_err() {
            //consume_whitespace_and_comments returns error on end of stream
            p2!(de_self, "Got end of stream while reading next entry");
            return Ok(None);
        }
        let token = de_self.peek().unwrap();
        p2!(de_self, "Key: {:?} - depth: {}", token, de_self.depth);
        let result = match token {
            //Simple key value pair - parse name
            Token::BareString => seed.deserialize(&mut *self.de).map(Some),
            Token::SquareBracket(Open) => {
                p2!(de_self, "Got square bracket open. First: {}", self.first);
                if !self.first && de_self.depth > 1 {
                    //Start of new table that is not part of our entries
                    p2!(de_self, "Got new table - breaking");
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
            Token::DoubleSquareBracket(Open) => {
                p2!(de_self, "Got double square bracket open");
                //Table in format:
                //[[...]]
                //<element data>

                let second = de_self.double_peek()?;
                match second {
                    Token::BareString => {}
                    token => {
                        return Err(Error::unexpected(
                            de_self.as_mut(),
                            token,
                            Expected::Token(Token::BareString),
                        ))
                    }
                }
                let name = de_self.tokens.inner().slice();
                if let Some(outer_name) = de_self.in_array_table {
                    if de_self.peek()? == Token::Period {
                        if name != outer_name {
                            panic!(
                                "current table path start {} doesn't equal expected table name {}",
                                name, outer_name
                            );
                        }
                    } else if name == outer_name {
                        p2!(de_self, "Found next table in array");
                        return Ok(None);
                    } else {
                        //We found a table with a different name than ours. Because
                        //`de_self.in_array_table` is `Some`, we are too deep to parse this because
                        //the next table that were seeing is at the root level
                        p2!(de_self, "Not root level array of tables. Breaking");
                        return Ok(None);
                    }
                } else {
                    de_self.in_array_table = Some(name);
                }
                //Subtable element with path

                p2!(de_self, "In array of tables: {}", name);
                //Read string within [[...]]
                let result = seed
                    .deserialize(SingleStrDeserializer { s: name })
                    .map(Some);

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
        let mut de_self = &mut *self.de;
        let initial_token = de_self.peek()?;
        p2!(de_self, "~next_value_seed (KeyValuePairs)");

        let allow_consume_whitespace = match initial_token {
            Token::Equals => {
                //Consume equals
                let _ = de_self.next().unwrap();
                false
            }
            Token::DoubleSquareBracket(Open) => false,
            Token::BareString => true,
            t => return Err(Error::unexpected(de_self.as_mut(), t, Expected::Value)),
        };

        self.de.depth += 1;
        let result = seed.deserialize(&mut *self.de);
        self.de.depth -= 1;

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

struct ArrayOfTables<'a, 'de: 'a> {
    de: &'a mut Deserializer<'de>,
    table_name: Option<&'a str>,
}

impl<'a, 'de> ArrayOfTables<'a, 'de> {
    fn new(de: &'a mut Deserializer<'de>) -> Self {
        Self {
            de,
            table_name: None,
        }
    }
}

// `SeqAccess` is provided to the `Visitor` to give it the ability to iterate
// through elements of the sequence.
impl<'de, 'a> SeqAccess<'de> for ArrayOfTables<'a, 'de> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>>
    where
        T: DeserializeSeed<'de>,
    {
        let de_self = &mut self.de;
        p2!(de_self, "~next_value_seed (ArrayOfTables)");
        match de_self.tokens.peek().copied() {
            None => return Ok(None),
            Some(Token::DoubleSquareBracket(Open)) => {}
            Some(token) => {
                return Err(Error::unexpected(
                    de_self.as_mut(),
                    token,
                    Expected::Token(Token::DoubleSquareBracket(Open)),
                ))
            }
        }
        match de_self.tokens.double_peek() {
            Some(Token::BareString) => {}
            _ => {
                return Err(Error::unexpected(
                    de_self.as_mut(),
                    Token::BareString,
                    Expected::Token(Token::BareString),
                ))
            }
        }
        let name = de_self.tokens.inner().slice();
        p2!(de_self, "Got inner table name {}", name);
        match self.table_name {
            Some(table_name) => {
                if table_name != name {
                    //We used peek above so that we can stop reading this sequence and leave the
                    //start of the new table un-consumed.
                    //This keeps the code generic for the start of any table
                    p2!(de_self, "Got different table of arrays");
                    unimplemented!();
                }
            }
            None => self.table_name = Some(name),
        }

        //Consume [[ and <name>
        let _ = de_self.next();
        let _ = de_self.next();

        expect_token!(
            de_self,
            Token::DoubleSquareBracket(Close),
            Expected::Token(Token::DoubleSquareBracket(Close))
        );

        // Deserialize an array element (probably a struct)
        self.de.depth += 1;
        let r = seed.deserialize(&mut *self.de).map(Some);
        self.de.depth -= 1;
        r
    }
}

struct Enum<'a, 'de: 'a> {
    de: &'a mut Deserializer<'de>,
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
        p!("In variant seed");
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
