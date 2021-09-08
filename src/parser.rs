
pub enum ErrorKind {
    UnknownToken,
    TableAlreadyDefined,
}

pub struct Error<'a> {
    span: &'a str,
    kind: ErrorKind,
}

pub fn parse() {}


