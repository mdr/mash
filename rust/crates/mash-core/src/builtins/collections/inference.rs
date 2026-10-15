//! Type inference and argument completion for the collections builtins (ports of the Scala
//! `…TypeInferenceStrategy` objects and `getCompletionSpecs` methods in `ns/collections`).

use std::rc::Rc;

use crate::builtins::collections::{GROUP_CLASS, MapFunction};
use crate::builtins::core::OBJECT_CLASS;
use crate::complete::CompletionSpec;
use crate::eval::is_subclass_of;
use crate::function::{BuiltinFunction, Params};
use crate::infer::{Inferencer, Type, TypedArg, TypedArgs, ValueInfo, infer_add, object_fields};
use crate::value::{ClassRef, Value};

// ------------------------------------------------------------------------------------------------ helpers

/// The element type of a sequence type: a list's element type, or a string's own type (Scala's
/// `Seq(e) ⇒ e; AnyString ⇒ sequenceType`)
fn element_or_string(sequence: &Type) -> Option<Type> {
    match sequence {
        Type::Seq(element) => Some((**element).clone()),
        t if t.is_string() => Some(t.clone()),
        _ => None,
    }
}

/// The type of the elements of `sequence` after applying `function` to each (Scala
/// `MapTypeInferenceStrategy.inferMappedType`)
pub(crate) fn mapped_type(
    inferencer: &mut Inferencer<'_>,
    function: Option<&ValueInfo>,
    sequence: Option<&Type>,
) -> Option<Type> {
    let function = function?;
    let function_type = function.ty.as_ref()?;
    let element = match sequence {
        Some(Type::Seq(element)) => (**element).clone(),
        Some(t) if t.is_string() => Type::CHARACTER,
        _ => Type::ANY,
    };
    inferencer.apply_function(function_type, &element, function.value.as_ref())
}

/// What's known about the argument for parameter `name`
fn info<'a>(bound: &'a crate::infer::BoundTypes, params: &Params, name: &str) -> Option<&'a ValueInfo> {
    bound.info(params, name)
}

/// Field names of a type (Scala `GetMethod.getFields`)
pub(crate) fn field_names(ty: &Type) -> Vec<String> {
    match ty {
        Type::Object(fields) => fields.keys().map(|k| k.to_string()).collect(),
        Type::Instance(class) => class.fields.iter().map(|f| f.name.to_string()).collect(),
        _ => vec![],
    }
}

fn is_object_class(ty: &Type) -> bool {
    matches!(ty, Type::Instance(class)
        if is_subclass_of(&ClassRef::Builtin(class), &ClassRef::Builtin(&OBJECT_CLASS)))
}

// ------------------------------------------------------------------------------------------------ strategies

/// `map`: a list of the function's result type; for a string, a string if the function returns strings
pub(crate) fn infer_map(inferencer: &mut Inferencer<'_>, params: &Params, args: &TypedArgs) -> Option<Type> {
    let bound = args.bind(params);
    let sequence = match bound.ty(params, "sequence") {
        Some(Type::Seq(element)) => Type::Seq(element),
        Some(s) if s.is_string() => s,
        _ => Type::seq(Type::ANY),
    };
    let new_element = mapped_type(inferencer, info(&bound, params, "f"), Some(&sequence));
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

pub(crate) fn infer_flat_map(inferencer: &mut Inferencer<'_>, params: &Params, args: &TypedArgs) -> Option<Type> {
    let bound = args.bind(params);
    let sequence = bound.ty(params, "sequence");
    let mapped = mapped_type(inferencer, info(&bound, params, "f"), sequence.as_ref());
    let element = match mapped {
        Some(Type::Seq(element)) => (*element).clone(),
        _ => Type::ANY,
    };
    Some(Type::seq(element))
}

pub(crate) fn infer_flatten(_: &mut Inferencer<'_>, params: &Params, args: &TypedArgs) -> Option<Type> {
    match args.bind(params).ty(params, "sequence")? {
        Type::Seq(inner) => match &*inner {
            Type::Seq(_) => Some((*inner).clone()),
            _ => None,
        },
        _ => None,
    }
}

/// A predicate is applied (so its subexpressions are typed); the result is the given type (Scala
/// `AllTypeInferenceStrategy`, `CountMatchesTypeInferenceStrategy`, `EachTypeInferenceStrategy`)
fn applying(
    inferencer: &mut Inferencer<'_>,
    params: &Params,
    args: &TypedArgs,
    function_param: &str,
    result: Type,
) -> Option<Type> {
    let bound = args.bind(params);
    let sequence = bound.ty(params, "sequence");
    mapped_type(inferencer, info(&bound, params, function_param), sequence.as_ref());
    Some(result)
}

pub(crate) fn infer_all(inferencer: &mut Inferencer<'_>, params: &Params, args: &TypedArgs) -> Option<Type> {
    applying(inferencer, params, args, "predicate", Type::BOOLEAN)
}

pub(crate) fn infer_count_matches(inferencer: &mut Inferencer<'_>, params: &Params, args: &TypedArgs) -> Option<Type> {
    applying(inferencer, params, args, "predicate", Type::NUMBER)
}

pub(crate) fn infer_each(inferencer: &mut Inferencer<'_>, params: &Params, args: &TypedArgs) -> Option<Type> {
    applying(inferencer, params, args, "action", Type::UNIT)
}

/// The function is applied; the result is an element of the sequence (Scala `FindTypeInferenceStrategy`,
/// also used by `maxBy`/`minBy`)
fn element_after_applying(
    inferencer: &mut Inferencer<'_>,
    params: &Params,
    args: &TypedArgs,
    function_param: &str,
) -> Option<Type> {
    let bound = args.bind(params);
    let sequence = bound.ty(params, "sequence");
    mapped_type(inferencer, info(&bound, params, function_param), sequence.as_ref());
    element_or_string(&sequence?)
}

pub(crate) fn infer_find(inferencer: &mut Inferencer<'_>, params: &Params, args: &TypedArgs) -> Option<Type> {
    element_after_applying(inferencer, params, args, "predicate")
}

pub(crate) fn infer_max_by(inferencer: &mut Inferencer<'_>, params: &Params, args: &TypedArgs) -> Option<Type> {
    element_after_applying(inferencer, params, args, "attribute")
}

/// The predicate is applied; the result is the sequence's type (a list or string), else a list (Scala
/// `WhereTypeInferenceStrategy`, used by `where`, `whereNot`, `skipWhile`, `skipUntil`, `takeWhile`, `sortBy`)
fn filtered(inferencer: &mut Inferencer<'_>, params: &Params, args: &TypedArgs, function_param: &str) -> Option<Type> {
    let bound = args.bind(params);
    let sequence = bound.ty(params, "sequence");
    mapped_type(inferencer, info(&bound, params, function_param), sequence.as_ref());
    match sequence {
        Some(t @ Type::Seq(_)) => Some(t),
        Some(t) if t.is_string() => Some(t),
        _ => Some(Type::seq(Type::ANY)),
    }
}

/// `whereNotNull`: the same kind of list
pub(crate) fn infer_where_not_null(_: &mut Inferencer<'_>, params: &Params, args: &TypedArgs) -> Option<Type> {
    match args.bind(params).ty(params, "sequence")? {
        t @ Type::Seq(_) => Some(t),
        _ => None,
    }
}

/// `partition`: a list of two of what `where` gives
pub(crate) fn infer_partition(inferencer: &mut Inferencer<'_>, params: &Params, args: &TypedArgs) -> Option<Type> {
    filtered(inferencer, params, args, "predicate").map(Type::seq)
}

pub(crate) fn infer_where(inferencer: &mut Inferencer<'_>, params: &Params, args: &TypedArgs) -> Option<Type> {
    filtered(inferencer, params, args, "predicate")
}

/// `sortBy`'s attributes are variadic; as in Scala, the first argument is treated as the function
pub(crate) fn infer_sort_by(inferencer: &mut Inferencer<'_>, params: &Params, args: &TypedArgs) -> Option<Type> {
    let bound = args.bind(params);
    let sequence = bound.ty(params, "sequence");
    let first_attribute = args.args.iter().find_map(|a| match a {
        TypedArg::Positional(info) => Some(info.clone()),
        _ => None,
    });
    mapped_type(inferencer, first_attribute.as_ref(), sequence.as_ref());
    match sequence {
        Some(t @ Type::Seq(_)) => Some(t),
        Some(t) if t.is_string() => Some(t),
        _ => Some(Type::seq(Type::ANY)),
    }
}

/// The same type as the sequence (Scala `SeqToSeqTypeInferenceStrategy`)
pub(crate) fn infer_same_sequence(_: &mut Inferencer<'_>, params: &Params, args: &TypedArgs) -> Option<Type> {
    args.bind(params).ty(params, "sequence")
}

/// A list of chunks/windows of the sequence (Scala `SlidingTypeInferenceStrategy`)
pub(crate) fn infer_windows(_: &mut Inferencer<'_>, params: &Params, args: &TypedArgs) -> Option<Type> {
    args.bind(params).ty(params, "sequence").map(Type::seq)
}

/// With a count, the same type as the sequence (objects unblessed); without, an element (Scala
/// `FirstLastTypeInferenceStrategy`)
pub(crate) fn infer_first_last(_: &mut Inferencer<'_>, params: &Params, args: &TypedArgs) -> Option<Type> {
    let bound = args.bind(params);
    let sequence = bound.ty(params, "sequence");
    if bound.contains(params, "n") {
        sequence.map(|t| match &t {
            Type::Instance(_) if is_object_class(&t) => unblessed(&t),
            Type::UserClassInstance(_) => unblessed(&t),
            _ => t,
        })
    } else {
        element_or_string(&sequence?)
    }
}

fn unblessed(ty: &Type) -> Type {
    Type::object(object_fields(ty).unwrap_or_default())
}

/// The largest/smallest item: an element of the items (Scala `MaxTypeInferenceStrategy`)
pub(crate) fn infer_max(_: &mut Inferencer<'_>, params: &Params, args: &TypedArgs) -> Option<Type> {
    match args.bind(params).ty(params, "items")? {
        Type::Seq(element) => Some((*element).clone()),
        _ => None,
    }
}

pub(crate) fn infer_sum(_: &mut Inferencer<'_>, params: &Params, args: &TypedArgs) -> Option<Type> {
    let element = match args.bind(params).ty(params, "sequence") {
        Some(Type::Seq(element)) => Some((*element).clone()),
        _ => None,
    };
    infer_add(element.clone(), element)
}

pub(crate) fn infer_sum_by(inferencer: &mut Inferencer<'_>, params: &Params, args: &TypedArgs) -> Option<Type> {
    let bound = args.bind(params);
    let sequence = bound.ty(params, "sequence");
    let mapped = mapped_type(inferencer, info(&bound, params, "attribute"), sequence.as_ref());
    infer_add(mapped.clone(), mapped)
}

pub(crate) fn infer_reduce(inferencer: &mut Inferencer<'_>, params: &Params, args: &TypedArgs) -> Option<Type> {
    let bound = args.bind(params);
    let element = match bound.ty(params, "sequence") {
        Some(Type::Seq(element)) => Some((*element).clone()),
        _ => None,
    };
    let accumulation = bound.ty(params, "initial").or_else(|| element.clone());
    let output = match (bound.ty(params, "accumulator"), &accumulation, &element) {
        (Some(function), Some(item), Some(element)) => inferencer.apply_function2(&function, item, element),
        _ => None,
    };
    output.or(accumulation)
}

pub(crate) fn infer_grep(_: &mut Inferencer<'_>, params: &Params, args: &TypedArgs) -> Option<Type> {
    let bound = args.bind(params);
    // Scala checks whether `--first` has a statically known value
    let first = bound.bool_value(params, "first").is_some();
    let input = bound.ty(params, "input");
    let is_object = input
        .as_ref()
        .is_some_and(|t| is_object_class(t) || matches!(t, Type::Object(_) | Type::UserClassInstance(_)));
    if is_object {
        return Some(Type::OBJECT);
    }
    let item = match &input {
        Some(Type::Seq(element)) => (**element).clone(),
        Some(t) if t.is_string() => t.clone(),
        _ => Type::ANY,
    };
    Some(if first { item } else { Type::seq(item) })
}

pub(crate) fn infer_group_by(inferencer: &mut Inferencer<'_>, params: &Params, args: &TypedArgs) -> Option<Type> {
    let bound = args.bind(params);
    let sequence = bound.ty(params, "sequence");
    let groups = bound.value(params, "groups").is_some_and(|v| v.is_truthy());
    let key = mapped_type(inferencer, info(&bound, params, "discriminator"), sequence.as_ref());
    if groups {
        let values = element_or_string(&sequence?)?;
        Some(Type::seq(Type::generic(&GROUP_CLASS, [key?, values])))
    } else {
        Some(Type::OBJECT)
    }
}

/// An object of counts, or with `--groups`, a list of `{ key, count }` rows (the key typed by the discriminator)
pub(crate) fn infer_count_by(inferencer: &mut Inferencer<'_>, params: &Params, args: &TypedArgs) -> Option<Type> {
    let bound = args.bind(params);
    let sequence = bound.ty(params, "sequence");
    let groups = bound.value(params, "groups").is_some_and(|v| v.is_truthy());
    let key = mapped_type(inferencer, info(&bound, params, "discriminator"), sequence.as_ref());
    if groups {
        Some(Type::seq(Type::object([
            (Rc::from("key"), key.unwrap_or(Type::ANY)),
            (Rc::from("count"), Type::NUMBER),
        ])))
    } else {
        Some(Type::OBJECT)
    }
}

/// `counts`: an object of counts, or with `--groups`, a list of `{ key, count }` rows (the key typed as an element)
pub(crate) fn infer_counts(_: &mut Inferencer<'_>, params: &Params, args: &TypedArgs) -> Option<Type> {
    let bound = args.bind(params);
    if bound.value(params, "groups").is_some_and(|v| v.is_truthy()) {
        let key = match bound.ty(params, "sequence") {
            Some(Type::Seq(element)) => (*element).clone(),
            Some(t) if t.is_string() => Type::CHARACTER,
            _ => Type::ANY,
        };
        Some(Type::seq(Type::object([
            (Rc::from("key"), key),
            (Rc::from("count"), Type::NUMBER),
        ])))
    } else {
        Some(Type::OBJECT)
    }
}

pub(crate) fn infer_transpose(_: &mut Inferencer<'_>, params: &Params, args: &TypedArgs) -> Option<Type> {
    match args.bind(params).ty(params, "sequence")? {
        Type::Seq(element) => Some(match &*element {
            Type::Seq(x) => Type::seq(Type::seq((**x).clone())),
            Type::Object(fields) => Type::object(fields.iter().map(|(k, v)| (k.clone(), Type::seq(v.clone())))),
            Type::Instance(class) => Type::object(class.fields.iter().map(|f| (Rc::from(f.name), Type::seq((f.ty)())))),
            _ => Type::seq(Type::seq(Type::ANY)),
        }),
        _ => None,
    }
}

pub(crate) fn infer_zip(_: &mut Inferencer<'_>, params: &Params, args: &TypedArgs) -> Option<Type> {
    let bound = args.bind(params);
    let element = |name: &str| match bound.ty(params, name) {
        Some(Type::Seq(element)) => Some((*element).clone()),
        _ => None,
    };
    let pair = element("sequence1")
        .or_else(|| element("sequence2"))
        .unwrap_or(Type::ANY);
    Some(Type::seq(Type::seq(pair)))
}

/// `select`: an object (or list of objects) with the selected fields (Scala `SelectTypeInferenceStrategy`)
pub(crate) fn infer_select(inferencer: &mut Inferencer<'_>, _: &Params, args: &TypedArgs) -> Option<Type> {
    let (last, selectors) = args.args.split_last()?;
    let TypedArg::Positional(ValueInfo { ty: Some(input), .. }) = last else {
        return None;
    };
    let target = match input {
        Type::Seq(element) => (**element).clone(),
        other => other.clone(),
    };
    let special = |flag: &str| flag == "add" || flag == "target";
    let mut fields: Vec<(Rc<str>, Type)> = vec![];
    for selector in selectors {
        match selector {
            TypedArg::Positional(ValueInfo {
                value,
                ty: Some(function),
            }) => {
                let ty = inferencer.apply_function(function, &target, value.as_ref());
                if let Some(Value::Str(name)) = value {
                    fields.push((name.as_str().into(), ty.unwrap_or(Type::ANY)));
                }
            }
            TypedArg::LongFlag(
                flag,
                Some(ValueInfo {
                    value,
                    ty: Some(function),
                }),
            ) if !special(flag) => {
                let ty = inferencer.apply_function(function, &target, value.as_ref());
                fields.push((flag.clone(), ty.unwrap_or(Type::ANY)));
            }
            TypedArg::LongFlag(flag, None) if !special(flag) => {
                let ty = inferencer.apply_function(&Type::STRING, &target, Some(&Value::str(flag)));
                fields.push((flag.clone(), ty.unwrap_or(Type::ANY)));
            }
            _ => {}
        }
    }
    let add = args.args.iter().any(|a| match a {
        TypedArg::LongFlag(flag, None) => &**flag == "add",
        TypedArg::ShortFlags(flags) => flags.contains(&'a'),
        _ => false,
    });
    let new_type = |element: &Type| {
        let mut all: Vec<(Rc<str>, Type)> = if add {
            match element {
                Type::Object(existing) => existing.iter().map(|(k, v)| (k.clone(), v.clone())).collect(),
                Type::Instance(class) => class.fields.iter().map(|f| (Rc::from(f.name), (f.ty)())).collect(),
                _ => vec![],
            }
        } else {
            vec![]
        };
        for (name, ty) in &fields {
            match all.iter_mut().find(|(k, _)| k == name) {
                Some(existing) => existing.1 = ty.clone(),
                None => all.push((name.clone(), ty.clone())),
            }
        }
        Type::object(all)
    };
    Some(match input {
        Type::Seq(element) => Type::seq(new_type(element)),
        other => new_type(other),
    })
}

/// `select` as a List method: the target is the input
pub(crate) fn infer_select_method(
    inferencer: &mut Inferencer<'_>,
    target: Option<&Type>,
    params: &Params,
    args: &TypedArgs,
) -> Option<Type> {
    infer_select(inferencer, params, &with_target(target, args))
}

pub(crate) fn infer_append(
    _: &mut Inferencer<'_>,
    target: Option<&Type>,
    params: &Params,
    args: &TypedArgs,
) -> Option<Type> {
    target
        .cloned()
        .or_else(|| args.bind(params).ty(params, "item").map(Type::seq))
}

pub(crate) fn infer_intersect(
    _: &mut Inferencer<'_>,
    target: Option<&Type>,
    params: &Params,
    args: &TypedArgs,
) -> Option<Type> {
    target.cloned().or_else(|| args.bind(params).ty(params, "sequence"))
}

pub(crate) fn infer_list_new(_: &mut Inferencer<'_>, params: &Params, args: &TypedArgs) -> Option<Type> {
    args.bind(params)
        .ty(params, "elements")
        .or_else(|| Some(Type::seq(Type::ANY)))
}

fn with_target(target: Option<&Type>, args: &TypedArgs) -> TypedArgs {
    let mut args = args.clone();
    args.args.push(TypedArg::Positional(ValueInfo {
        value: None,
        ty: target.cloned(),
    }));
    args
}

// ------------------------------------------------------------------------------------------------ completion

/// Completing the function argument of `map` (and, as in Scala, of the other functions that take a function
/// applied to each element): members of the element type
pub(crate) fn complete_map_function(_: &Params, arg_pos: usize, args: &TypedArgs) -> Vec<CompletionSpec> {
    let params = MapFunction.params();
    let bound = args.bind(params);
    match (bound.param_at(params, arg_pos), bound.ty(params, "sequence")) {
        (Some("f"), Some(Type::Seq(element))) => vec![CompletionSpec::Members((*element).clone())],
        _ => vec![],
    }
}

/// `flatMap`'s function argument: members of the element type
pub(crate) fn complete_function_argument(params: &Params, arg_pos: usize, args: &TypedArgs) -> Vec<CompletionSpec> {
    let bound = args.bind(params);
    match (bound.param_at(params, arg_pos), bound.ty(params, "sequence")) {
        (Some("f"), Some(Type::Seq(element))) => vec![CompletionSpec::Members((*element).clone())],
        _ => vec![],
    }
}

/// `deselect`'s field names: the fields of the target (or its elements)
pub(crate) fn complete_deselect(params: &Params, arg_pos: usize, args: &TypedArgs) -> Vec<CompletionSpec> {
    let bound = args.bind(params);
    match (bound.param_at(params, arg_pos), bound.ty(params, "target")) {
        (Some("fields"), Some(target)) => {
            let element = match target {
                Type::Seq(element) => (*element).clone(),
                other => other,
            };
            vec![CompletionSpec::Items(field_names(&element))]
        }
        _ => vec![],
    }
}

/// `select`'s arguments (all but the last, which is the input): members of the input (or its elements)
pub(crate) fn complete_select(_: &Params, arg_pos: usize, args: &TypedArgs) -> Vec<CompletionSpec> {
    if arg_pos + 1 >= args.args.len() {
        return vec![];
    }
    match args.args.last() {
        Some(TypedArg::Positional(ValueInfo {
            ty: Some(Type::Seq(element)),
            ..
        })) => {
            vec![CompletionSpec::Members((**element).clone())]
        }
        Some(TypedArg::Positional(ValueInfo { ty: Some(target), .. })) => vec![CompletionSpec::Members(target.clone())],
        _ => vec![],
    }
}

pub(crate) fn complete_select_method(
    params: &Params,
    arg_pos: usize,
    target: Option<&Type>,
    args: &TypedArgs,
) -> Vec<CompletionSpec> {
    complete_select(params, arg_pos, &with_target(target, args))
}
