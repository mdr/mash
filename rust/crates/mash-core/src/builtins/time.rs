//! The `time` namespace.

use crate::value::BuiltinClass;

use super::core::ANY_CLASS;

pub static DATE_CLASS: BuiltinClass = BuiltinClass {
    summary: "A date",
    ..BuiltinClass::new("time", "Date", Some(&ANY_CLASS))
};

pub static DATE_TIME_CLASS: BuiltinClass = BuiltinClass {
    summary: "An instant in time",
    ..BuiltinClass::new("time", "DateTime", Some(&ANY_CLASS))
};
