//! A no_std toml deserializer for embedded systems
//! Full toml support for deserializing into structs that implement [`serde::Deserialize`].
//!
//! Requires no memory allocations and is likely much faster than toml-rs.
//! Supports deserialization only
//!

//We are no std. But we still want the standard library for testing since we only run tests on
//normal (usually tier 1) hardware
#![cfg_attr(not(test), no_std)]

mod de;
mod lexer;
mod error;

pub use de::from_str;
