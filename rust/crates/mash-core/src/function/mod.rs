//! Functions and methods: parameter models, and the traits builtins implement.

mod params;

pub use params::{DefaultValue, Param, Params};

/// A function implemented in Rust. Builtins are `'static` (usually unit structs), so values can refer to
/// them without allocating.
pub trait BuiltinFunction: Sync + 'static {
    /// e.g. `map`
    fn name(&self) -> &'static str;

    /// e.g. `collections`
    fn namespace(&self) -> &'static str;

    /// e.g. `collections.map`
    fn full_name(&self) -> String {
        format!("{}.{}", self.namespace(), self.name())
    }
}

/// A method implemented in Rust, belonging to a builtin class.
pub trait BuiltinMethod: Sync + 'static {
    fn name(&self) -> &'static str;
}
