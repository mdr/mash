//! The `core` namespace: the classes of basic values, and general-purpose functions.

mod any;
mod functions;
pub mod help;

use crate::function::{BuiltinFunction, BuiltinMethod};
use crate::value::BuiltinClass;

pub use functions::*;

/// Functions in `core` that are also in the global scope
pub(crate) const FUNCTIONS: &[&dyn BuiltinFunction] =
    &[&ErrorFunction, &IdentityFunction, &IsNullFunction, &NotFunction];

pub static ANY_CLASS: BuiltinClass = BuiltinClass {
    summary: "The root class of all Mash values",
    methods: any::METHODS,
    ..BuiltinClass::new("core", "Any", None)
};

const fn core_class(
    name: &'static str,
    summary: &'static str,
    methods: &'static [&'static dyn BuiltinMethod],
) -> BuiltinClass {
    BuiltinClass {
        summary,
        methods,
        ..BuiltinClass::new("core", name, Some(&ANY_CLASS))
    }
}

pub static OBJECT_CLASS: BuiltinClass = core_class("Object", "The class of all objects", &[]);
pub static NULL_CLASS: BuiltinClass = core_class("Null", "The type of null", &[]);
pub static UNIT_CLASS: BuiltinClass = core_class("Unit", "The type of the unit value", &[]);
pub static BOOLEAN_CLASS: BuiltinClass = core_class("Boolean", "Boolean values true and false", &[]);
pub static NUMBER_CLASS: BuiltinClass = core_class("Number", "A number", &[]);
pub static STRING_CLASS: BuiltinClass = core_class("String", "A string", &[]);
pub static CHARACTER_CLASS: BuiltinClass = core_class("Character", "Tag class for a character", &[]);
pub static FUNCTION_CLASS: BuiltinClass = core_class("Function", "A function", &[]);
pub static BOUND_METHOD_CLASS: BuiltinClass = core_class("BoundMethod", "A method bound to a target", &[]);
pub static CLASS_CLASS: BuiltinClass = core_class("Class", "A class", &[]);
