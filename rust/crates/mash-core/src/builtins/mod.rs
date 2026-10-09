//! Builtin functions and classes, and the standard global environment (Scala `MashRoot`,
//! `StandardEnvironment`, `NamespaceCreator`).

pub mod collections;
pub mod core;
pub mod encoding;
pub mod json;
pub mod maths;
pub mod meta;
pub mod net;
pub mod os;
pub mod random;
pub mod time;
pub mod view;
pub mod xml;

use std::collections::BTreeMap;

use crate::function::BuiltinFunction;
use crate::platform::Platform;
use crate::value::{Builtin, BuiltinClass, ClassRef, Object, Str, Value};

/// Functions available unqualified in the global scope
pub fn standard_functions() -> Vec<&'static dyn BuiltinFunction> {
    let mut functions = core::FUNCTIONS.to_vec();
    functions.extend_from_slice(collections::FUNCTIONS);
    functions.extend_from_slice(collections::SEQUENCE_FUNCTIONS);
    functions.extend_from_slice(collections::ORDERING_FUNCTIONS);
    functions.extend_from_slice(time::FUNCTIONS);
    functions
}

/// Every builtin function, reachable by its full name under the namespace objects (e.g. `core.identity`)
pub fn all_functions() -> Vec<&'static dyn BuiltinFunction> {
    let mut functions = standard_functions();
    for namespace in [
        encoding::FUNCTIONS,
        json::FUNCTIONS,
        maths::FUNCTIONS,
        meta::FUNCTIONS,
        net::FUNCTIONS,
        random::FUNCTIONS,
        view::FUNCTIONS,
        xml::FUNCTIONS,
    ] {
        functions.extend_from_slice(namespace);
    }
    functions
}

/// Classes available unqualified in the global scope
pub fn standard_classes() -> Vec<&'static BuiltinClass> {
    vec![
        &collections::LIST_CLASS,
        &core::BOOLEAN_CLASS,
        &core::BOUND_METHOD_CLASS,
        &core::CLASS_CLASS,
        &core::FUNCTION_CLASS,
        &core::NULL_CLASS,
        &core::NUMBER_CLASS,
        &core::OBJECT_CLASS,
        &core::ANY_CLASS,
        &core::STRING_CLASS,
        &core::CHARACTER_CLASS,
        &core::UNIT_CLASS,
    ]
}

/// Every builtin class, reachable by its full name under the namespace objects
pub fn all_classes() -> Vec<&'static BuiltinClass> {
    let mut classes = standard_classes();
    classes.extend([
        &collections::GROUP_CLASS,
        &maths::STATS_CLASS,
        &net::HOST_CLASS,
        &net::URL_CLASS,
        &os::PATH_CLASS,
        &view::VIEW_CLASS,
    ]);
    classes.extend_from_slice(time::CLASSES);
    classes.extend_from_slice(core::OTHER_CLASSES);
    classes
}

/// Short names for standard functions (e.g. `count` for `length`)
fn aliases() -> Vec<(&'static str, &'static dyn BuiltinFunction)> {
    vec![("count", &collections::LengthFunction), ("man", &core::HelpFunction)]
}

/// The global variables of a new interpreter (Scala `StandardEnvironment.createGlobalVariables`): standard
/// functions and classes by name, aliases, the root namespaces (`core`, `collections`, …), `env`, `config`,
/// `it`, `ns` (all namespaces) and `global` (this object).
pub fn standard_globals(platform: &dyn Platform) -> Object {
    let globals = Object::new();
    for f in standard_functions() {
        globals.set_str(f.name(), Value::Builtin(Builtin(f)));
    }
    for class in standard_classes() {
        globals.set_str(class.name, Value::Class(ClassRef::Builtin(class)));
    }
    for (alias, f) in aliases() {
        globals.set_str(alias, Value::Builtin(Builtin(f)));
    }
    let ns = namespaces();
    for (name, value) in ns.entries() {
        globals.set(name, value);
    }
    let env = Object::new();
    let mut vars = platform.env_vars();
    vars.sort();
    for (name, value) in vars {
        env.set_str(&name, Value::Str(Str::new(value)));
    }
    globals.set_str("env", Value::Object(env));
    globals.set_str("config", Value::Object(default_config()));
    globals.set_str("it", Value::Null);
    globals.set_str("ns", Value::Object(ns));
    globals.set_str("global", Value::Object(globals.clone()));
    globals
}

/// Nested objects holding every builtin by its full name: `ns.collections.map` (Scala `NamespaceCreator`)
fn namespaces() -> Object {
    #[derive(Default)]
    struct Tree {
        leaf: Option<Value>,
        children: BTreeMap<String, Tree>,
    }
    fn insert(tree: &mut Tree, segments: &[&str], value: Value) {
        match segments {
            [] => tree.leaf = Some(value),
            [first, rest @ ..] => insert(tree.children.entry(first.to_string()).or_default(), rest, value),
        }
    }
    fn to_object(tree: &Tree) -> Object {
        let object = Object::new();
        for (name, child) in &tree.children {
            let value = match &child.leaf {
                Some(value) if child.children.is_empty() => value.clone(),
                _ => Value::Object(to_object(child)),
            };
            object.set_str(name, value);
        }
        object
    }
    let mut root = Tree::default();
    for f in all_functions() {
        let value = Value::Builtin(Builtin(f));
        let full_name = f.full_name();
        insert(&mut root, &full_name.split('.').collect::<Vec<_>>(), value.clone());
        for alias in f.aliases() {
            insert(&mut root, &alias.split('.').collect::<Vec<_>>(), value.clone());
        }
    }
    for class in all_classes() {
        let full_name = class.full_name();
        insert(
            &mut root,
            &full_name.split('.').collect::<Vec<_>>(),
            Value::Class(ClassRef::Builtin(class)),
        );
    }
    to_object(&root)
}

/// Configuration options and their defaults (Scala `Config.defaultConfig`)
fn default_config() -> Object {
    let section = |entries: &[(&str, Value)]| {
        let object = Object::new();
        for (name, value) in entries {
            object.set_str(name, value.clone());
        }
        Value::Object(object)
    };
    let config = Object::new();
    config.set_str("cli", section(&[("showStartupTips", Value::Bool(true))]));
    config.set_str("language", section(&[("bareWords", Value::Bool(false))]));
    config.set_str(
        "view",
        section(&[
            ("browseLargeOutput", Value::Bool(true)),
            ("discoBorders", Value::Bool(false)),
            ("fuzzyTime", Value::Bool(true)),
        ]),
    );
    config
}
