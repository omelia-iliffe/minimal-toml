mod bool_test;
mod enum_test;
mod struct_test;
mod array_test;

use minimal_toml::Error;
use serde::{Deserialize, Serialize};

pub fn print_token_error(input: &str, err: Error) -> ! {
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

pub fn expect_output<'de, T>(input: &'static str, expected: T)
where
    T: std::fmt::Debug + PartialEq + Deserialize<'de>,
{
    let v = match minimal_toml::from_str(input) {
        Err(err) => print_token_error(input, err),
        Ok(v) => v,
    };
    assert_eq!(expected, v);
}

pub fn expect_with_toml_rs<'de, T>(value: &T)
where
    T: std::fmt::Debug + PartialEq + Deserialize<'de> + Serialize,
{
    let s = toml::to_string(value).expect("toml-rs failed to serialize value");
    println!("==========\n\n\n{}\n\n\n==========", s);
    let s_in = s.clone();
    let s_in: &'de str = unsafe { std::mem::transmute(s_in.as_str()) };
    {
        let my_t: T = match minimal_toml::from_str(s_in) {
            Err(err) => print_token_error(s.as_str(), err),
            Ok(v) => v,
        };
        assert_eq!(value, &my_t);
    }
}
