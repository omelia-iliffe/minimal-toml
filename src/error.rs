use serde::de;

use core::fmt::Display;
use core::fmt::{self, Write};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ErrorKind {
    UnknownToken,
    UnexpectedToken(Expected),
    InvalidInteger(core::num::ParseIntError),
    InvalidFloat(core::num::ParseFloatError),
    TableAlreadyDefined,
    TrailingCharacters,
    MissingToken,
    Custom([u8; 64], usize),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expected {
    Token(crate::lexer::Token),
    LineStart,
    RValue,
    Bool,
    String,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Error {
    span: core::ops::Range<usize>,
    kind: ErrorKind,
}

impl Error {
    pub fn new(lexer: &logos::Lexer<crate::lexer::Token>, kind: ErrorKind) -> Self {
        Self {
            span: lexer.span(),
            kind,
        }
    }
}

#[cfg(test)]
impl std::error::Error for Error {}

#[cfg(not(test))]
impl serde::ser::StdError for Error {}

impl de::Error for Error {
    fn custom<T: Display>(msg: T) -> Self {
        let mut buf = [0u8; 64];
        let offset = {
            let mut wrapper = Wrapper::new(&mut buf);
            write!(&mut wrapper, "{}", msg);
            wrapper.offset
        };

        Error {
            span: 0..0,
            kind: ErrorKind::Custom(buf, offset),
        }
    }
}

impl core::fmt::Display for Error {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.write_fmt(format_args!("{:?}", self))
    }
}

impl Error {
    pub fn end<'de>(de: &crate::de::Deserializer<'de>, kind: ErrorKind) -> Error {
        Error {
            span: de.input.len()..de.input.len(),
            kind,
        }
    }
}

//Wrapper type so that we can format a T: Display into a fixed size buffer
struct Wrapper<'a> {
    buf: &'a mut [u8],
    offset: usize,
}

impl<'a> Wrapper<'a> {
    fn new(buf: &'a mut [u8]) -> Self {
        Wrapper { buf, offset: 0 }
    }
}

impl<'a> fmt::Write for Wrapper<'a> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        let bytes = s.as_bytes();

        let remainder = &mut self.buf[self.offset..];
        if remainder.len() < bytes.len() {
            // return error instead of panicking if out of space
            return Err(core::fmt::Error);
        }
        let remainder = &mut remainder[..bytes.len()];
        remainder.copy_from_slice(bytes);

        self.offset += bytes.len();

        Ok(())
    }
}
