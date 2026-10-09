//! Type inference and completion rules for core builtins that are more than a fixed result type (ports of
//! the Scala `typeInferenceStrategy` and `getCompletionSpecs` implementations).

use std::rc::Rc;

use indexmap::IndexMap;

use super::help::METHOD_HELP_CLASS;
use super::{OBJECT_CLASS, TIMED_RESULT_CLASS};
use crate::complete::CompletionSpec;
use crate::eval::is_subclass_of;
use crate::function::Params;
use crate::infer::{Inferencer, Type, TypedArgs, field_type};
use crate::value::{ClassRef, Value};

type Fields = IndexMap<Rc<str>, Type>;

/// The type of the argument for parameter `name`
fn arg_type(params: &Params, args: &TypedArgs, name: &str) -> Option<Type> {
    args.bind(params).ty(params, name)
}

/// The name of the parameter that the argument at `arg_pos` is bound to
fn param_at<'p>(params: &'p Params, args: &TypedArgs, arg_pos: usize) -> Option<&'p str> {
    let index = (*args.bind(params).arg_params.get(arg_pos)?)?;
    params.params[index].name.as_deref()
}

// ------------------------------------------------------------------------------------------------ functions

/// `identity`: the type of its argument
pub(crate) fn identity(_: &mut Inferencer<'_>, params: &Params, args: &TypedArgs) -> Option<Type> {
    arg_type(params, args, "item")
}

/// `try`: the type of the body
pub(crate) fn try_(_: &mut Inferencer<'_>, params: &Params, args: &TypedArgs) -> Option<Type> {
    arg_type(params, args, "body")
}

/// `timeTaken`: a TimedResult of the block's type
pub(crate) fn time_taken(_: &mut Inferencer<'_>, params: &Params, args: &TypedArgs) -> Option<Type> {
    let result = arg_type(params, args, "block").unwrap_or(Type::ANY);
    Some(Type::generic(&TIMED_RESULT_CLASS, [result]))
}

/// `core.split`: a list of strings like the one given
pub(crate) fn split(_: &mut Inferencer<'_>, params: &Params, args: &TypedArgs) -> Option<Type> {
    Some(Type::seq(arg_type(params, args, "string").unwrap_or(Type::STRING)))
}

/// `help`: function help for functions, method help for methods, the class for classes
pub(crate) fn help(_: &mut Inferencer<'_>, params: &Params, args: &TypedArgs) -> Option<Type> {
    match arg_type(params, args, "item")? {
        Type::BuiltinFunction(_) | Type::UserFunction(_) => Some(Type::FUNCTION),
        Type::BoundBuiltinMethod(..) | Type::BoundUserMethod(..) => Some(Type::Instance(&METHOD_HELP_CLASS)),
        Type::UserClass(_) => Some(Type::CLASS),
        t if t == Type::CLASS => Some(Type::CLASS),
        _ => None,
    }
}

/// `parallelMap`: as `map` (Scala `MapTypeInferenceStrategy`)
pub(crate) fn parallel_map(inferencer: &mut Inferencer<'_>, params: &Params, args: &TypedArgs) -> Option<Type> {
    let bound = args.bind(params);
    let function = bound.info(params, "f").cloned();
    let sequence = match bound.ty(params, "sequence") {
        Some(Type::Seq(element)) => Type::Seq(element),
        Some(s) if s.is_string() => s,
        _ => Type::seq(Type::ANY),
    };
    let new_element = function.and_then(|function| {
        let function_type = function.ty.as_ref()?;
        let element = match &sequence {
            Type::Seq(element) => (**element).clone(),
            _ => Type::CHARACTER,
        };
        inferencer.apply_function(function_type, &element, function.value.as_ref())
    });
    Some(if sequence.is_string() {
        match new_element {
            None => sequence,
            Some(t) if t.is_string() => sequence,
            Some(t) => Type::seq(t),
        }
    } else {
        Type::seq(new_element.unwrap_or(Type::ANY))
    })
}

/// `parallelMap`: the function argument can be the name of a member of the elements
pub(crate) fn parallel_map_completion(params: &Params, arg_pos: usize, args: &TypedArgs) -> Vec<CompletionSpec> {
    if param_at(params, args, arg_pos) != Some("f") {
        return vec![];
    }
    match arg_type(params, args, "sequence") {
        Some(Type::Seq(element)) => vec![CompletionSpec::Members((*element).clone())],
        _ => vec![],
    }
}

// ------------------------------------------------------------------------------------------------ methods

/// The target's type (e.g. `toInt` and `negate` keep a number's tag)
pub(crate) fn same_as_target(_: &mut Inferencer<'_>, target: Option<&Type>, _: &Params, _: &TypedArgs) -> Option<Type> {
    target.cloned()
}

/// String methods returning a string like their target (Scala `SameStringMethodTypeInferenceStrategy`)
pub(crate) fn same_string(_: &mut Inferencer<'_>, target: Option<&Type>, _: &Params, _: &TypedArgs) -> Option<Type> {
    Some(target.cloned().unwrap_or(Type::STRING))
}

/// `String.split` and `.lines`: a list of strings like the target
pub(crate) fn string_split(_: &mut Inferencer<'_>, target: Option<&Type>, _: &Params, _: &TypedArgs) -> Option<Type> {
    Some(Type::seq(target.cloned().unwrap_or(Type::STRING)))
}

/// `String.grep`: a line, or a list of them
pub(crate) fn string_grep(
    _: &mut Inferencer<'_>,
    target: Option<&Type>,
    params: &Params,
    args: &TypedArgs,
) -> Option<Type> {
    let first = args.bind(params).bool_value(params, "first").is_some();
    target.map(|t| if first { t.clone() } else { Type::seq(t.clone()) })
}

/// `Number.times`: a list of the block's type
pub(crate) fn times(_: &mut Inferencer<'_>, _: Option<&Type>, params: &Params, args: &TypedArgs) -> Option<Type> {
    arg_type(params, args, "block").map(Type::seq)
}

/// `BoundMethod.target`
pub(crate) fn bound_method_target(
    _: &mut Inferencer<'_>,
    target: Option<&Type>,
    _: &Params,
    _: &TypedArgs,
) -> Option<Type> {
    match target? {
        Type::BoundBuiltinMethod(target, _) => Some((**target).clone()),
        _ => None,
    }
}

/// `Class.bless`: an instance of the class, else the object's type
pub(crate) fn class_bless(
    _: &mut Inferencer<'_>,
    target: Option<&Type>,
    params: &Params,
    args: &TypedArgs,
) -> Option<Type> {
    match target {
        Some(Type::UserClass(class)) => Some(Type::UserClassInstance(class.clone())),
        _ => arg_type(params, args, "object"),
    }
}

/// `Object.bless`: an instance of the class given
pub(crate) fn object_bless(
    _: &mut Inferencer<'_>,
    _: Option<&Type>,
    params: &Params,
    args: &TypedArgs,
) -> Option<Type> {
    let bound = args.bind(params);
    let info = bound.info(params, "class")?;
    match (&info.ty, &info.value) {
        (Some(Type::UserClass(class)), _) => Some(Type::UserClassInstance(class.clone())),
        (_, Some(Value::Class(ClassRef::Builtin(class)))) => Some(Type::Instance(class)),
        _ => None,
    }
}

/// `Object.unbless`: an object with the class's fields
pub(crate) fn unbless(_: &mut Inferencer<'_>, target: Option<&Type>, _: &Params, _: &TypedArgs) -> Option<Type> {
    match target? {
        Type::Instance(class) => Some(Type::object(class.fields.iter().map(|f| (Rc::from(f.name), (f.ty)())))),
        Type::UserClassInstance(class) => Some(Type::object(
            class
                .params
                .params
                .iter()
                .filter_map(|p| Some((Rc::from(p.name.as_deref()?), Type::ANY))),
        )),
        _ => None,
    }
}

/// `Object.get`: the field's type, else the default's
pub(crate) fn get(_: &mut Inferencer<'_>, target: Option<&Type>, params: &Params, args: &TypedArgs) -> Option<Type> {
    let bound = args.bind(params);
    let field = bound.string_value(params, "name").and_then(|name| match target? {
        Type::Object(fields) => fields.get(name.as_str()).cloned(),
        Type::Instance(class) => field_type(class, &name),
        _ => None,
    });
    field.or_else(|| bound.ty(params, "default"))
}

/// Names of the fields of an object-like type (Scala `GetMethod.getFields`)
fn field_names(ty: &Type) -> Vec<String> {
    match ty {
        Type::Object(fields) => fields.keys().map(|k| k.to_string()).collect(),
        Type::Instance(class) => class.fields.iter().map(|f| f.name.to_string()).collect(),
        _ => vec![],
    }
}

/// `Object.get`: the name argument can be one of the target's fields
pub(crate) fn get_completion(
    params: &Params,
    arg_pos: usize,
    target: Option<&Type>,
    args: &TypedArgs,
) -> Vec<CompletionSpec> {
    match (param_at(params, args, arg_pos), target) {
        (Some("name"), Some(target)) => vec![CompletionSpec::Items(field_names(target))],
        _ => vec![],
    }
}

/// `Object.hoist`: the field argument can be one of the target's fields
pub(crate) fn hoist_completion(
    params: &Params,
    arg_pos: usize,
    target: Option<&Type>,
    args: &TypedArgs,
) -> Vec<CompletionSpec> {
    match (param_at(params, args, arg_pos), target) {
        (Some("fieldName"), Some(target)) => vec![CompletionSpec::Items(field_names(target))],
        _ => vec![],
    }
}

/// The fields of an object type or object class (Scala `HoistMethodTypeInferenceStrategy.getFields`)
fn object_fields(ty: &Type) -> Option<Fields> {
    match ty {
        Type::Instance(class) if is_subclass_of(&ClassRef::Builtin(class), &ClassRef::Builtin(&OBJECT_CLASS)) => {
            Some(class.fields.iter().map(|f| (Rc::from(f.name), (f.ty)())).collect())
        }
        Type::Object(fields) => Some((**fields).clone()),
        _ => None,
    }
}

/// `Object.hoist`: the target's fields with the hoisted field replaced by its subfields
pub(crate) fn hoist(_: &mut Inferencer<'_>, target: Option<&Type>, params: &Params, args: &TypedArgs) -> Option<Type> {
    let field_name = args.bind(params).string_value(params, "fieldName")?;
    let fields = object_fields(target?)?;
    let field_type = fields.get(field_name.as_str())?;
    let (new_fields, is_list) = match field_type {
        Type::Seq(element) => (object_fields(element)?, true),
        other => (object_fields(other)?, false),
    };
    let mut result: Fields = fields.clone();
    result.shift_remove(field_name.as_str());
    result.extend(new_fields);
    let object = Type::Object(Rc::new(result));
    Some(if is_list { Type::seq(object) } else { object })
}

/// `Object.withField`: the target's type, plus the field for a plain object
pub(crate) fn with_field(
    _: &mut Inferencer<'_>,
    target: Option<&Type>,
    params: &Params,
    args: &TypedArgs,
) -> Option<Type> {
    match target? {
        t @ Type::Instance(class) if is_subclass_of(&ClassRef::Builtin(class), &ClassRef::Builtin(&OBJECT_CLASS)) => {
            Some(t.clone())
        }
        t @ Type::UserClassInstance(_) => Some(t.clone()),
        Type::Object(fields) => {
            let bound = args.bind(params);
            let name = bound.string_value(params, "name")?;
            let value = bound.ty(params, "value").unwrap_or(Type::ANY);
            let mut fields = (**fields).clone();
            fields.insert(name.into(), value);
            Some(Type::Object(Rc::new(fields)))
        }
        _ => None,
    }
}
