//! Functions and methods: parameter models, argument binding, typed argument extraction, and the traits
//! builtins implement.

mod args;
mod bind;
mod params;

pub use args::{Args, Callable, FromValue, Sequence};
pub use bind::{Argument, Bound, Suspended, bind};
pub use params::{DefaultValue, Param, Params};

use crate::eval::{Ctx, R};
use crate::value::Value;

/// A function implemented in Rust. Builtins are `'static` (usually unit structs), so values can refer to
/// them without allocating.
pub trait BuiltinFunction: Sync + 'static {
    /// e.g. `map`
    fn name(&self) -> &'static str;

    /// e.g. `collections`
    fn namespace(&self) -> &'static str;

    /// Other fully-qualified names this function is known by (e.g. `os.ls`)
    fn aliases(&self) -> &'static [&'static str] {
        &[]
    }

    fn params(&self) -> &Params;

    fn call(&self, ctx: &mut Ctx, args: &Args) -> R<Value>;

    /// One-line description
    fn summary(&self) -> &'static str;

    /// Further documentation (may contain `<mash>` markup)
    fn description(&self) -> Option<&'static str> {
        None
    }

    /// e.g. `collections.map`
    fn full_name(&self) -> String {
        if self.namespace().is_empty() {
            self.name().to_string()
        } else {
            format!("{}.{}", self.namespace(), self.name())
        }
    }
}

/// A method implemented in Rust, belonging to a builtin class.
pub trait BuiltinMethod: Sync + 'static {
    fn name(&self) -> &'static str;

    fn aliases(&self) -> &'static [&'static str] {
        &[]
    }

    fn params(&self) -> &Params;

    fn call(&self, ctx: &mut Ctx, target: &Value, args: &Args) -> R<Value>;

    fn summary(&self) -> &'static str;

    fn description(&self) -> Option<&'static str> {
        None
    }

    /// Only accessible via `this`
    fn private(&self) -> bool {
        false
    }

    /// Not found by an unqualified name inside a method (Scala `isShy`), nor imported
    fn shy(&self) -> bool {
        false
    }
}
