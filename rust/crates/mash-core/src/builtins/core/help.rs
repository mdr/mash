//! Help for functions, methods, classes and fields (`foo?`, `help foo`): Scala `HelpCreator` and the help
//! classes.

use mash_macros::builtin;

use crate::eval::{Ctx, MemberLookup, R, primary_class};
use crate::value::{BoundMethod, BuiltinClass, ClassRef, Field, MethodRef, Object, Value};

use super::OBJECT_CLASS;

pub static METHOD_HELP_CLASS: BuiltinClass = BuiltinClass {
    summary: "Help documentation for a method",
    fields: &[
        Field {
            name: "name",
            summary: "Method name",
        },
        Field {
            name: "owningClass",
            summary: "The class the method belongs to",
        },
    ],
    ..BuiltinClass::new("core.help", "MethodHelp", Some(&OBJECT_CLASS))
};

pub static FIELD_HELP_CLASS: BuiltinClass = BuiltinClass {
    summary: "Help documentation for a field",
    fields: &[
        Field {
            name: "name",
            summary: "Field name",
        },
        Field {
            name: "owningClass",
            summary: "Class this field belongs to",
        },
    ],
    ..BuiltinClass::new("core.help", "FieldHelp", Some(&OBJECT_CLASS))
};

pub static PARAMETER_HELP_CLASS: BuiltinClass = BuiltinClass {
    summary: "Help documentation for parameters",
    fields: &[
        Field {
            name: "name",
            summary: "Parameter name",
        },
        Field {
            name: "summary",
            summary: "Summary of what the parameter does",
        },
        Field {
            name: "description",
            summary: "Description of the parameter",
        },
        Field {
            name: "shortFlag",
            summary: "Alternative single-character flag form, or null if none",
        },
        Field {
            name: "isFlagParameter",
            summary: "If true, this parameter can only be given as a flag",
        },
        Field {
            name: "isOptional",
            summary: "If true, this parameter is optional",
        },
        Field {
            name: "isLazy",
            summary: "If true, this parameter is evaluated lazily",
        },
        Field {
            name: "isNamedArg",
            summary: "If true, this parameter is a @namedArg parameter",
        },
        Field {
            name: "isVariadic",
            summary: "If true, this parameter can take an arbitrary number of positional arguments",
        },
    ],
    ..BuiltinClass::new("core.help", "ParameterHelp", Some(&OBJECT_CLASS))
};

/// Find help for the given function, method, field or class
#[builtin(namespace = "core.help")]
pub fn help(#[param("The item to find help for", safe)] item: Value) -> R<Value> {
    Ok(help_value(&item))
}

/// The help for a value (Scala `HelpCreator.getHelp`): a function is its own help; a bound method gets a
/// `MethodHelp`; a class is its own help; anything else gets its class.
pub(crate) fn help_value(value: &Value) -> Value {
    match value {
        Value::Function(_) | Value::Builtin(_) | Value::Class(_) => value.clone(),
        Value::BoundMethod(m) => method_help(m),
        _ => Value::Class(primary_class(value)),
    }
}

/// The help object for a value (`expr?`)
pub(crate) fn help_for(_ctx: &mut Ctx, value: &Value) -> R<Value> {
    Ok(help_value(value))
}

/// Help for `target.name`, if it's a field of the target's class or a method (Scala
/// `HelpEvaluator.getHelpForMember`)
pub(crate) fn help_for_member(ctx: &Ctx, target: &Value, name: &str) -> Option<Value> {
    if let Value::Object(object) = target
        && let Some(class) = object.class()
        && has_field(&class, name)
    {
        return Some(field_help(name, class));
    }
    match ctx.lookup_member(target, name, MemberLookup::DEFAULT)? {
        Value::BoundMethod(m) => Some(method_help(&m)),
        _ => None,
    }
}

pub(crate) fn has_field(class: &ClassRef, name: &str) -> bool {
    super::classes::class_field_names(class).iter().any(|f| f == name)
}

pub(crate) fn method_help(method: &BoundMethod) -> Value {
    let class = match method.method() {
        MethodRef::Builtin { class, .. } => ClassRef::Builtin(class),
        MethodRef::User { class, .. } => ClassRef::User(class.clone()),
    };
    method_help_object(method.name(), class)
}

pub(crate) fn method_help_object(name: &str, class: ClassRef) -> Value {
    help_object(&METHOD_HELP_CLASS, name, class)
}

pub(crate) fn field_help(name: &str, class: ClassRef) -> Value {
    help_object(&FIELD_HELP_CLASS, name, class)
}

fn help_object(help_class: &'static BuiltinClass, name: &str, class: ClassRef) -> Value {
    let object = Object::with_class(Some(ClassRef::Builtin(help_class)));
    object.set_str("name", Value::str(name));
    object.set_str("owningClass", Value::Class(class));
    Value::Object(object)
}
