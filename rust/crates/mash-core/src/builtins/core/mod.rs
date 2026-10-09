//! The `core` namespace: the classes of basic values, and general-purpose functions.

mod any;
mod classes;
mod functions;
pub mod help;
mod number;
mod object;
mod regex;
mod string;
mod thread;

use crate::infer::Type;
use crate::function::{BuiltinFunction, BuiltinMethod};
use crate::value::{BuiltinClass, Field};

pub use functions::*;
pub use help::HelpFunction;
pub use number::BYTES_CLASS;
pub use object::FIELD_AND_VALUE_CLASS;
pub(crate) use regex::{compile as compile_regex, runtime_error as regex_runtime_error};
pub(crate) use string::SplitFunction;
pub use thread::*;

/// Functions in `core` that are also in the global scope
pub(crate) const FUNCTIONS: &[&dyn BuiltinFunction] = &[
    &ErrorFunction,
    &IdentityFunction,
    &InBackgroundFunction,
    &IsNullFunction,
    &HelpFunction,
    &NoArgFunction,
    &NotFunction,
    &ParallelMapFunction,
    &ParseNumberFunction,
    &SafeFunction,
    &SplitFunction,
    &TapFunction,
    &TimeTakenFunction,
    &TryFunction,
    &WhileFunction,
];

/// Classes in `core` that aren't in the global scope, but are reachable through namespaces
pub(crate) const OTHER_CLASSES: &[&BuiltinClass] = &[
    &BYTES_CLASS,
    &NO_ARG_CLASS,
    &FIELD_AND_VALUE_CLASS,
    &TIMED_RESULT_CLASS,
    &help::METHOD_HELP_CLASS,
    &help::FIELD_HELP_CLASS,
    &help::PARAMETER_HELP_CLASS,
    &REGEX_CLASS,
    &MATCH_CLASS,
];

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

pub static OBJECT_CLASS: BuiltinClass = BuiltinClass {
    static_methods: object::OBJECT_STATIC_METHODS,
    ..core_class("Object", "The class of all objects", object::OBJECT_METHODS)
};
pub static NULL_CLASS: BuiltinClass = core_class("Null", "The type of null", &[]);
pub static UNIT_CLASS: BuiltinClass = core_class("Unit", "The type of the unit value", &[]);
pub static BOOLEAN_CLASS: BuiltinClass =
    core_class("Boolean", "Boolean values true and false", classes::BOOLEAN_METHODS);
pub static NUMBER_CLASS: BuiltinClass = core_class("Number", "A number", number::METHODS);
pub static STRING_CLASS: BuiltinClass = core_class("String", "A string", string::STRING_METHODS);
pub static CHARACTER_CLASS: BuiltinClass =
    core_class("Character", "Tag class for a character", classes::CHARACTER_METHODS);
pub static FUNCTION_CLASS: BuiltinClass = core_class("Function", "A function", classes::FUNCTION_METHODS);
pub static BOUND_METHOD_CLASS: BuiltinClass = core_class(
    "BoundMethod",
    "A method bound to a target",
    classes::BOUND_METHOD_METHODS,
);
pub static CLASS_CLASS: BuiltinClass = core_class("Class", "A class", classes::CLASS_METHODS);

pub static REGEX_CLASS: BuiltinClass = core_class("Regex", "A regular expression", regex::REGEX_METHODS);

pub static MATCH_CLASS: BuiltinClass = BuiltinClass {
    summary: "A match from a regular expression search",
    fields: &[
        Field {
            name: "matched",
            summary: "The matched region of text",
            ty: || Type::STRING,
        },
        Field {
            name: "groups",
            summary: "Type (file, dir, link)",
            ty: || Type::seq(Type::STRING),
        },
    ],
    ..BuiltinClass::new("core", "Match", Some(&OBJECT_CLASS))
};

const NO_ARG_SUMMARY: &str =
    "Value that represents no argument being provided, for use as sentinel values in default arguments";

pub static NO_ARG_CLASS: BuiltinClass = BuiltinClass {
    summary: NO_ARG_SUMMARY,
    ..BuiltinClass::new("core", "NoArg", Some(&OBJECT_CLASS))
};

pub static TIMED_RESULT_CLASS: BuiltinClass = BuiltinClass {
    summary: "The result of a timed execution",
    fields: &[
        Field {
            name: "result",
            summary: "Result of the piece of code executed",
            ty: || Type::ANY,
        },
        Field {
            name: "started",
            summary: "Time execution started",
            ty: || Type::Instance(&crate::builtins::time::DATE_TIME_CLASS),
        },
        Field {
            name: "duration",
            summary: "Duration of execution",
            ty: || Type::tagged_number(&crate::builtins::time::MILLISECONDS_CLASS),
        },
    ],
    methods: &[&FinishedMethod],
    ..BuiltinClass::new("core", "TimedResult", Some(&OBJECT_CLASS))
};
