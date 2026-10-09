//! The `os` namespace. Most of it is native-only and lives in `mash-os`; the `Path` tag class is here because
//! double-quoted strings are paths.

use crate::value::BuiltinClass;

use super::core::ANY_CLASS;

pub static PATH_CLASS: BuiltinClass = BuiltinClass {
    summary: "Tag class for a filesystem path",
    ..BuiltinClass::new("os", "Path", Some(&ANY_CLASS))
};
