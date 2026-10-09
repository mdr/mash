//! The `view` namespace: directives on how the REPL should display a result (Scala `ns.view`).
//!
//! Each function wraps its data in a `view.View` object; the REPL's viewer (`mash_repl::view`) unwraps it and
//! follows the directives. As in Scala, `view.print` doesn't print anything itself: it asks the viewer to
//! print the data even when it would otherwise open the object browser.

use mash_macros::builtin;

use super::core::OBJECT_CLASS;
use crate::eval::R;
use crate::function::BuiltinFunction;
use crate::infer::Type;
use crate::value::{BuiltinClass, ClassRef, Field, Object, Value};

pub(crate) const FUNCTIONS: &[&dyn BuiltinFunction] = &[&BrowseFunction, &PrintFunction, &RawFunction, &TreeFunction];

pub static VIEW_CLASS: BuiltinClass = BuiltinClass {
    summary: "Instructions on how to display data in Mash's output system",
    fields: &[
        Field {
            name: DATA,
            summary: "Data to display",
            ty: || Type::ANY,
        },
        Field {
            name: DISABLE_CUSTOM_VIEWS,
            summary: "If true, disable custom views for this data",
            ty: || Type::BOOLEAN,
        },
        Field {
            name: USE_BROWSER,
            summary: "If true, always use the object browser where possible",
            ty: || Type::BOOLEAN,
        },
        Field {
            name: USE_TREE,
            summary: "If true, always use the tree object browser where possible",
            ty: || Type::BOOLEAN,
        },
    ],
    static_methods: &[&ViewNewFunction],
    ..BuiltinClass::new("view", "View", Some(&OBJECT_CLASS))
};

const DATA: &str = "data";
const DISABLE_CUSTOM_VIEWS: &str = "disableCustomViews";
const USE_BROWSER: &str = "useBrowser";
const USE_TREE: &str = "useTree";
/// Not one of the class's declared fields in Scala Mash (so not a constructor parameter), but always set by
/// the `view` functions
const PRINT: &str = "print";

/// The directives in a `view.View` object.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub struct ViewDirectives {
    /// Show functions, classes and help objects as plain values rather than as help
    pub disable_custom_views: bool,
    /// Always use the object browser
    pub use_browser: bool,
    /// Show the data as a tree
    pub use_tree: bool,
    /// Print, even if the output is too large for the screen
    pub print: bool,
}

impl ViewDirectives {
    fn or(self, other: ViewDirectives) -> ViewDirectives {
        ViewDirectives {
            disable_custom_views: self.disable_custom_views || other.disable_custom_views,
            use_browser: self.use_browser || other.use_browser,
            use_tree: self.use_tree || other.use_tree,
            print: self.print || other.print,
        }
    }
}

/// If `value` is a `view.View` object, its data and directives
pub fn as_view(value: &Value) -> Option<(Value, ViewDirectives)> {
    let Value::Object(object) = value else {
        return None;
    };
    if !object.class().is_some_and(|c| c.is_builtin(&VIEW_CLASS)) {
        return None;
    }
    let flag = |name: &str| matches!(object.get_str(name), Some(Value::Bool(true)));
    let directives = ViewDirectives {
        disable_custom_views: flag(DISABLE_CUSTOM_VIEWS),
        use_browser: flag(USE_BROWSER),
        use_tree: flag(USE_TREE),
        print: flag(PRINT),
    };
    Some((object.get_str(DATA).unwrap_or(Value::Null), directives))
}

/// The data inside (possibly nested) `view.View` objects, or the value itself (Scala `ViewClass.unpackView`):
/// what `it` is set to after a command
pub fn unpack_view(value: &Value) -> Value {
    match as_view(value) {
        Some((data, _)) => unpack_view(&data),
        None => value.clone(),
    }
}

/// A `view.View` of `data` with these directives; a view of a view combines their directives
pub fn view_object(data: Value, directives: ViewDirectives) -> Object {
    let (data, directives) = match as_view(&data) {
        Some((inner, existing)) => (inner, directives.or(existing)),
        None => (data, directives),
    };
    let object = Object::with_class(Some(ClassRef::Builtin(&VIEW_CLASS)));
    object.set_str(DATA, data);
    object.set_str(DISABLE_CUSTOM_VIEWS, Value::Bool(directives.disable_custom_views));
    object.set_str(USE_BROWSER, Value::Bool(directives.use_browser));
    object.set_str(USE_TREE, Value::Bool(directives.use_tree));
    object.set_str(PRINT, Value::Bool(directives.print));
    object
}

/// View data in the object browser
#[builtin(namespace = "view", returns = Type::Instance(&VIEW_CLASS))]
pub fn browse(#[param("Data to view")] data: Value) -> R<Object> {
    Ok(view_object(
        data,
        ViewDirectives {
            use_browser: true,
            ..ViewDirectives::default()
        },
    ))
}

/// Print the data
#[builtin(namespace = "view", returns = Type::Instance(&VIEW_CLASS))]
pub fn print(#[param("Data to print")] data: Value) -> R<Object> {
    Ok(view_object(
        data,
        ViewDirectives {
            print: true,
            ..ViewDirectives::default()
        },
    ))
}

/// View an object without custom formatting
#[builtin(namespace = "view", returns = Type::Instance(&VIEW_CLASS))]
pub fn raw(#[param("Data to view")] data: Value) -> R<Object> {
    Ok(view_object(
        data,
        ViewDirectives {
            disable_custom_views: true,
            ..ViewDirectives::default()
        },
    ))
}

/// View data in the object tree browser, where possible
#[builtin(namespace = "view", returns = Type::Instance(&VIEW_CLASS))]
pub fn tree(#[param("Data to view")] data: Value) -> R<Object> {
    Ok(view_object(
        data,
        ViewDirectives {
            use_tree: true,
            ..ViewDirectives::default()
        },
    ))
}

/// Construct a new View object
#[builtin(namespace = "view.View", name = "new", returns = Type::Instance(&VIEW_CLASS))]
pub fn view_new(
    #[param("Data to display")] data: Value,
    #[param("If true, disable custom views for this data")] disable_custom_views: Value,
    #[param("If true, always use the object browser where possible")] use_browser: Value,
    #[param("If true, always use the tree object browser where possible")] use_tree: Value,
) -> R<Object> {
    let object = Object::with_class(Some(ClassRef::Builtin(&VIEW_CLASS)));
    object.set_str(DATA, data);
    object.set_str(DISABLE_CUSTOM_VIEWS, disable_custom_views);
    object.set_str(USE_BROWSER, use_browser);
    object.set_str(USE_TREE, use_tree);
    Ok(object)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn views_of_views_combine_their_directives() {
        let tree = ViewDirectives {
            use_tree: true,
            ..ViewDirectives::default()
        };
        let raw = ViewDirectives {
            disable_custom_views: true,
            ..ViewDirectives::default()
        };
        let inner = Value::Object(view_object(Value::num(1.0), tree));
        let outer = Value::Object(view_object(inner.clone(), raw));
        let (data, directives) = as_view(&outer).unwrap();
        assert!(data.mash_equals(&Value::num(1.0)));
        assert!(directives.use_tree && directives.disable_custom_views && !directives.print);
        assert!(unpack_view(&inner).mash_equals(&Value::num(1.0)));
        assert!(as_view(&Value::num(1.0)).is_none());
    }
}
