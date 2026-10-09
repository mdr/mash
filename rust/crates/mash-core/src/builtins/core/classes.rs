//! Methods of the core classes `Function`, `BoundMethod`, `Class`, `Boolean` and `Character`.

use mash_macros::builtin_method;

use crate::eval::{Ctx, MashError, R, is_subclass_of, parent_of};
use crate::function::{Argument, BuiltinMethod, Sequence, Suspended};
use crate::value::{ClassRef, FunctionKind, MethodRef, Object, Str, Value};

use super::OBJECT_CLASS;
use super::help::{field_help, has_field, method_help};

pub(super) const FUNCTION_METHODS: &[&dyn BuiltinMethod] = &[
    &FunctionDescriptionMethod,
    &FunctionInvokeMethod,
    &FunctionSummaryMethod,
];
pub(super) const BOUND_METHOD_METHODS: &[&dyn BuiltinMethod] = &[
    &BoundMethodDescriptionMethod,
    &BoundMethodHelpMethod,
    &BoundMethodInvokeMethod,
    &BoundMethodSummaryMethod,
    &BoundMethodTargetMethod,
];
pub(super) const CLASS_METHODS: &[&dyn BuiltinMethod] = &[
    &BlessMethod,
    &FullNameMethod,
    &HelpForFieldMethod,
    &HelpForMethodMethod,
    &NameMethod,
    &ParentMethod,
];
pub(super) const BOOLEAN_METHODS: &[&dyn BuiltinMethod] = &[&BooleanNotMethod];
pub(super) const CHARACTER_METHODS: &[&dyn BuiltinMethod] = &[&IsDigitMethod];

fn maybe_str(s: Option<String>) -> Value {
    s.map_or(Value::Null, Value::from)
}

fn non_empty(s: &str) -> Option<String> {
    (!s.is_empty()).then(|| s.to_string())
}

/// The summary of a function value, if it has one
pub(crate) fn function_summary(f: &Value) -> Option<String> {
    match f {
        Value::Builtin(b) => non_empty(b.0.summary()),
        Value::Function(function) => match function.kind() {
            FunctionKind::Closure { def, .. } if def.name.is_none() => Some("anonymous function".into()),
            FunctionKind::Closure { def, .. } => def.doc.as_ref().map(|d| d.summary.clone()),
            FunctionKind::Constructor(class) => Some(format!("Construct a new {} object", class.name())),
            FunctionKind::Suspended(_) => Some("Lazily computed argument".into()),
            FunctionKind::SystemCommand(_) => None,
        },
        _ => None,
    }
}

/// The description of a function value, if it has one
pub(crate) fn function_description(f: &Value) -> Option<String> {
    match f {
        Value::Builtin(b) => b.0.description().map(str::to_string),
        Value::Function(function) => match function.kind() {
            FunctionKind::Closure { def, .. } => def.doc.as_ref().and_then(|d| d.description.clone()),
            _ => None,
        },
        _ => None,
    }
}

fn method_summary(method: &MethodRef) -> Option<String> {
    match method {
        MethodRef::Builtin { method, .. } => non_empty(method.summary()),
        MethodRef::User { class, index } => class.methods()[*index].def.doc.as_ref().map(|d| d.summary.clone()),
    }
}

fn method_description(method: &MethodRef) -> Option<String> {
    match method {
        MethodRef::Builtin { method, .. } => method.description().map(str::to_string),
        MethodRef::User { class, index } => class.methods()[*index]
            .def
            .doc
            .as_ref()
            .and_then(|d| d.description.clone()),
    }
}

/// Positional and named arguments for `invoke`
fn invocation_arguments(args: Sequence, named_args: Object) -> R<Vec<Argument>> {
    let mut arguments: Vec<Argument> = args.items().into_iter().map(Argument::value).collect();
    for (field, value) in named_args.entries() {
        let Value::Str(name) = &field else {
            return Err(MashError::new(format!(
                "Named arguments must be Strings, but was {}",
                field.type_name()
            )));
        };
        arguments.push(Argument::LongFlag {
            name: name.as_str().into(),
            value: Some(Suspended::Value(value)),
            location: None,
        });
    }
    Ok(arguments)
}

// ------------------------------------------------------------------------------------------------ Function

/// Get the summary of this method, if any, else null
#[builtin_method(name = "summary", returns = crate::infer::Type::STRING)]
fn function_summary_(target: &Value) -> R<Value> {
    Ok(maybe_str(function_summary(target)))
}

/// Get the description of this method, if any, else null
#[builtin_method(name = "description", returns = crate::infer::Type::STRING)]
fn function_description_(target: &Value) -> R<Value> {
    Ok(maybe_str(function_description(target)))
}

/// Invoke this function with the given arguments
#[builtin_method(name = "invoke")]
fn function_invoke(
    ctx: &mut Ctx,
    target: &Value,
    #[param("Positional arguments for this function", default = Vec::<Value>::new())] args: Sequence,
    #[param("Named arguments for this function", default = Object::new())] named_args: Object,
) -> R<Value> {
    let arguments = invocation_arguments(args, named_args)?;
    ctx.call(target, &arguments, None, None)
}

// ------------------------------------------------------------------------------------------------ BoundMethod

fn bound_method(target: &Value) -> R<&crate::value::BoundMethod> {
    match target {
        Value::BoundMethod(m) => Ok(m),
        other => Err(MashError::new(format!(
            "Expected a bound method, but was a {}",
            other.type_name()
        ))),
    }
}

/// Get the summary of this method, if any, else null
#[builtin_method(name = "summary", returns = crate::infer::Type::STRING)]
fn bound_method_summary(target: &Value) -> R<Value> {
    Ok(maybe_str(method_summary(bound_method(target)?.method())))
}

/// Get the description of this method, if any, else null
#[builtin_method(name = "description", returns = crate::infer::Type::STRING)]
fn bound_method_description(target: &Value) -> R<Value> {
    Ok(maybe_str(method_description(bound_method(target)?.method())))
}

/// Invoke this method with the given arguments
#[builtin_method(name = "invoke")]
fn bound_method_invoke(
    ctx: &mut Ctx,
    target: &Value,
    #[param("Positional arguments for this function", default = Vec::<Value>::new())] args: Sequence,
    #[param("Named arguments for this function", default = Object::new())] named_args: Object,
) -> R<Value> {
    let arguments = invocation_arguments(args, named_args)?;
    ctx.call(target, &arguments, None, None)
}

/// Help documentation for this method
#[builtin_method(name = "help", returns = crate::infer::Type::Instance(&super::help::METHOD_HELP_CLASS))]
fn bound_method_help(target: &Value) -> R<Value> {
    Ok(method_help(bound_method(target)?))
}

/// Target of this bound method
///
/// Examples:
/// <mash>
///   [1, 2, 3].sortBy.target # [1, 2, 3]
/// </mash>
#[builtin_method(name = "target", infer = super::infer_rules::bound_method_target)]
fn bound_method_target(target: &Value) -> R<Value> {
    Ok(bound_method(target)?.target().clone())
}

// ------------------------------------------------------------------------------------------------ Class

/// Give the given object this class
#[builtin_method(infer = super::infer_rules::class_bless)]
fn bless(target: ClassRef, #[param("Object to give this class")] object: Value) -> R<Object> {
    let Value::Object(object) = object else {
        return Err(MashError::new(format!(
            "Invalid argument 'object'. Must be an object, but was a {}",
            object.type_name()
        )));
    };
    if !is_subclass_of(&target, &ClassRef::Builtin(&OBJECT_CLASS)) {
        return Err(MashError::new("Only object classes can be used to bless values"));
    }
    let missing: Vec<String> = class_field_names(&target)
        .into_iter()
        .filter(|name| !object.contains_str(name))
        .map(|name| format!("'{name}'"))
        .collect();
    if !missing.is_empty() {
        return Err(MashError::new(format!(
            "Missing fields for class '{}': {}",
            target.full_name(),
            missing.join(", ")
        )));
    }
    Ok(object.copy_with_class(Some(target)))
}

/// The names of the fields of instances of a class
pub(crate) fn class_field_names(class: &ClassRef) -> Vec<String> {
    match class {
        ClassRef::Builtin(c) => c.fields.iter().map(|f| f.name.to_string()).collect(),
        ClassRef::User(c) => c.params().params.iter().map(|p| p.name().to_string()).collect(),
    }
}

/// The fully-qualified name of this class
#[builtin_method(returns = crate::infer::Type::STRING)]
fn full_name(target: ClassRef) -> R<String> {
    Ok(target.full_name())
}

/// The name of this class
#[builtin_method(returns = crate::infer::Type::STRING)]
fn name(target: ClassRef) -> R<String> {
    Ok(target.name())
}

/// The parent of this class, if any, else null
#[builtin_method(returns = crate::infer::Type::CLASS)]
fn parent(target: ClassRef) -> R<Value> {
    Ok(parent_of(&target).map_or(Value::Null, Value::Class))
}

/// Help documentation for a method
#[builtin_method(returns = crate::infer::Type::Instance(&super::help::METHOD_HELP_CLASS))]
fn help_for_method(target: ClassRef, #[param("Name of a method of this class")] name: Str) -> R<Value> {
    let exists = match &target {
        ClassRef::Builtin(c) => c.method(name.as_str()).is_some(),
        ClassRef::User(c) => c.find_method(name.as_str()).is_some(),
    };
    if !exists {
        return Err(MashError::new(format!(
            "Invalid argument 'name'. No method called '{}'",
            name.as_str()
        )));
    }
    Ok(super::help::method_help_object(name.as_str(), target))
}

/// Help documentation for a field
#[builtin_method(returns = crate::infer::Type::Instance(&super::help::FIELD_HELP_CLASS))]
fn help_for_field(target: ClassRef, #[param("Name of a field of this class")] name: Str) -> R<Value> {
    if !has_field(&target, name.as_str()) {
        return Err(MashError::new(format!(
            "Invalid argument 'name'. No field called '{}'",
            name.as_str()
        )));
    }
    Ok(field_help(name.as_str(), target))
}

// ------------------------------------------------------------------------------------------------ Boolean

/// Negate this boolean
///
/// Examples:
/// <mash>
///   true.negate  # false
///   false.negate # true
/// </mash>
#[builtin_method(name = "not", returns = crate::infer::Type::BOOLEAN)]
fn boolean_not(target: bool) -> R<bool> {
    Ok(!target)
}

// ------------------------------------------------------------------------------------------------ Character

/// Return true if this character is a digit
#[builtin_method(returns = crate::infer::Type::BOOLEAN)]
fn is_digit(target: Str) -> R<bool> {
    Ok(target.as_str().chars().all(char::is_numeric))
}
