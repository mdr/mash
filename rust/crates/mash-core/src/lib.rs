//! Mash values, evaluator, type inference and platform-independent builtins.
//!
//! This crate must build for `wasm32-unknown-unknown`: it never touches the OS directly. Everything
//! platform-specific goes through [`platform::Platform`].

pub mod ast;
pub mod compile;
pub mod function;
pub mod platform;
pub mod value;
