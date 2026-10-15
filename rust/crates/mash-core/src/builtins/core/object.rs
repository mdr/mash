//! Methods of objects (Scala `ObjectClass` and `objectClass/*`), the `Object.merge` and `Object.fromPairs`
//! static methods, and the `FieldAndValue` class.

use crate::infer::Type;
use mash_macros::{builtin, builtin_method};

use super::classes::class_field_names;
use crate::builtins::collections::{
    AllButLastFunction, AllFunction, AnyFunction, FindFunction, FirstFunction, FlatMapFunction, GrepOptions,
    IsEmptyFunction, LastFunction, LengthFunction, Methodised, NonEmptyFunction, RenameFunction, ReverseFunction,
    SkipFunction, SortFunction, TransposeFunction, attribute_functions, descending_param, filter_object, grep_object,
    map_object, merge_objects, natural_order_param, sort_object_by,
};
use crate::eval::{Ctx, MashError, R, is_subclass_of};
use crate::function::{Args, BuiltinFunction, BuiltinMethod, Callable};
use crate::value::{BuiltinClass, ClassRef, Field, List, Object, Str, Value};

use super::OBJECT_CLASS;

pub(crate) const OBJECT_METHODS: &[&dyn BuiltinMethod] = &[
    &BlessMethod,
    &FieldsMethod,
    &GetMethod,
    &GrepMethod,
    &HasFieldMethod,
    &HoistMethod,
    &MapMethod,
    &SortByMethod,
    &TransformFieldsMethod,
    &TransformValuesMethod,
    &UnblessMethod,
    &ValuesMethod,
    &WithFieldMethod,
    &WhereMethod,
    &WhereNotMethod,
    &ALL_METHOD,
    &ALL_BUT_LAST_METHOD,
    &ANY_METHOD,
    &FIND_METHOD,
    &FIRST_METHOD,
    &FLAT_MAP_METHOD,
    &IS_EMPTY_METHOD,
    &LAST_METHOD,
    &LENGTH_METHOD,
    &NON_EMPTY_METHOD,
    &RENAME_METHOD,
    &REVERSE_METHOD,
    &SKIP_METHOD,
    &SORT_METHOD,
    &TRANSPOSE_METHOD,
];

pub(crate) const OBJECT_STATIC_METHODS: &[&dyn BuiltinFunction] = &[&FromPairsFunction, &MergeFunction];

// Collection functions as (shy) Object methods, as in Scala's ObjectClass
static ALL_METHOD: Methodised = Methodised::new(&AllFunction, "sequence", &[], true);
static ALL_BUT_LAST_METHOD: Methodised = Methodised::new(&AllButLastFunction, "sequence", &[], true);
static ANY_METHOD: Methodised = Methodised::new(&AnyFunction, "sequence", &[], true);
static FIND_METHOD: Methodised = Methodised::new(&FindFunction, "sequence", &[], true);
static RENAME_METHOD: Methodised = Methodised::new(&RenameFunction, "target", &[], true);
static FIRST_METHOD: Methodised = Methodised::new(&FirstFunction, "sequence", &[], true);
static FLAT_MAP_METHOD: Methodised = Methodised::new(&FlatMapFunction, "sequence", &[], true);
static IS_EMPTY_METHOD: Methodised = Methodised::new(&IsEmptyFunction, "sequence", &[], true);
static LAST_METHOD: Methodised = Methodised::new(&LastFunction, "sequence", &[], true);
static LENGTH_METHOD: Methodised = Methodised::new(&LengthFunction, "sequence", &["count"], true);
static NON_EMPTY_METHOD: Methodised = Methodised::new(&NonEmptyFunction, "sequence", &[], true);
static REVERSE_METHOD: Methodised = Methodised::new(&ReverseFunction, "sequence", &[], true);
static SKIP_METHOD: Methodised = Methodised::new(&SkipFunction, "sequence", &[], true);
static SORT_METHOD: Methodised = Methodised::new(&SortFunction, "sequence", &[], true);
static TRANSPOSE_METHOD: Methodised = Methodised::new(&TransposeFunction, "sequence", &[], true);

pub static FIELD_AND_VALUE_CLASS: BuiltinClass = BuiltinClass {
    summary: "A field and its value",
    fields: &[
        Field {
            name: "name",
            summary: "Field name",
            ty: || Type::ANY,
        },
        Field {
            name: "value",
            summary: "Field value",
            ty: || Type::ANY,
        },
    ],
    static_methods: &[&FieldAndValueNewFunction],
    ..BuiltinClass::new("core", "FieldAndValue", Some(&OBJECT_CLASS))
};

fn field_and_value(name: Value, value: Value) -> Value {
    let object = Object::with_class(Some(ClassRef::Builtin(&FIELD_AND_VALUE_CLASS)));
    object.set_str("name", name);
    object.set_str("value", value);
    Value::Object(object)
}

/// Construct a new FieldAndValue object
#[builtin(namespace = "core.FieldAndValue", name = "new")]
pub(crate) fn field_and_value_new(
    #[param("Field name")] name: Value,
    #[param("Field value")] value: Value,
) -> R<Value> {
    Ok(field_and_value(name, value))
}

/// A function of one or two arguments (Scala `validateFunction1Or2`): functions and bound methods that can
/// take two positional arguments get two, anything else callable one
fn takes_two(ctx: &Ctx, f: &Callable) -> bool {
    matches!(f.0, Value::Function(_) | Value::Builtin(_) | Value::BoundMethod(_)) && f.accepts_positional(ctx, 2)
}

fn new_object(fields: impl IntoIterator<Item = (Value, Value)>) -> Object {
    let object = Object::new();
    for (k, v) in fields {
        object.set(k, v);
    }
    object
}

/// Give this object the given class
#[builtin_method(shy, infer = super::infer_rules::object_bless)]
fn bless(target: Object, args: &Args, #[param("Class to associate with this object")] class: Value) -> R<Object> {
    let Value::Class(class) = class else {
        return Err(args.invalid("class", &format!("Must be a class, but was a {}", class.type_name())));
    };
    // As Class.bless: an object can't become a String or Number (Scala allowed it, making broken values)
    if !is_subclass_of(&class, &ClassRef::Builtin(&OBJECT_CLASS)) {
        return Err(MashError::new("Only object classes can be used to bless values"));
    }
    {
        let missing: Vec<String> = class_field_names(&class)
            .into_iter()
            .filter(|name| !target.contains_str(name))
            .map(|name| format!("'{name}'"))
            .collect();
        if !missing.is_empty() {
            return Err(MashError::new(format!(
                "Missing fields for class '{}': {}",
                class.full_name(),
                missing.join(", ")
            )));
        }
    }
    Ok(target.copy_with_class(Some(class)))
}

/// Return the fields of this object
#[builtin_method(shy, returns = crate::infer::Type::seq(crate::infer::Type::Instance(&FIELD_AND_VALUE_CLASS)))]
fn fields(target: Object) -> R<Vec<Value>> {
    Ok(target
        .entries()
        .into_iter()
        .map(|(k, v)| field_and_value(k, v))
        .collect())
}

/// Get the value of the given field, else use a default (null by default)
///
/// Only fields are returned, not methods.
///
/// Examples:
/// <mash>
///   { name: 'Alice', age: 30 }.get 'age' # 30
///   {}.get 'age'                         # null
///   {}.get 'age' --default=18            # 18
/// </mash>
#[builtin_method(shy, infer = super::infer_rules::get, completion = super::infer_rules::get_completion)]
fn get(
    target: Object,
    #[param("Field name")] name: Value,
    #[param(
        "Default to use if no field with that name present in object (default null)",
        default = Value::Null
    )]
    default: Value,
) -> R<Value> {
    Ok(target.get(&name).unwrap_or(default))
}

/// Find all the fields in the object which match the given query somewhere in its String representation
#[builtin_method(shy, returns = crate::infer::Type::OBJECT)]
fn grep(
    ctx: &mut Ctx,
    target: Object,
    #[param(
        "Return the first match, if there is one, else null (default false)",
        short = 'f',
        boolean_flag
    )]
    first: bool,
    #[param("Perform a case-insensitive match (default false)", short = 'i', boolean_flag)] ignore_case: bool,
    #[param(
        "Find all items that don't match the given query (default false)",
        short = 'n',
        boolean_flag
    )]
    negate: bool,
    #[param(
        "Interpret query as a regular expression; otherwise, interpret query as the literal string (default false)",
        short = 'r',
        boolean_flag
    )]
    regex: bool,
    #[param("Query to find in the given input")] query: Value,
) -> R<Value> {
    let query = ctx.stringify(&query)?;
    let options = GrepOptions {
        ignore_case,
        regex,
        negate,
        first,
        ignore_fields: false,
    };
    grep_object(ctx, &target, &query, options)
}

/// Return true if this object contains the given field
#[builtin_method(shy, returns = crate::infer::Type::BOOLEAN)]
fn has_field(target: Object, #[param("Field name")] name: Value) -> R<bool> {
    Ok(target.contains(&name))
}

/// Hoist the fields of a subobject up into this object
///
/// Examples:
/// <mash>
///   {
///     foo: 42,
///     bar: {
///       baz1: 100,
///       baz2: 200
///     }
///   }.hoist 'bar'
/// </mash>
/// Becomes:
/// <mash>
///   {
///     foo: 42,
///     baz1: 100,
///     baz2: 200
///   }
/// </mash>
#[builtin_method(shy, infer = super::infer_rules::hoist, completion = super::infer_rules::hoist_completion)]
fn hoist(
    ctx: &mut Ctx,
    args: &Args,
    target: Object,
    #[param("Field name to hoist")] field_name: Value,
    #[param("Add this prefix to hoisted field names", flag, flag_value_name = "prefix")] prefix: Option<Str>,
) -> R<Value> {
    let prefix = prefix.map(|p| p.as_str().to_string()).unwrap_or_default();
    let Some(value) = target.get(&field_name) else {
        let field = ctx.stringify(&field_name)?;
        return Err(args.invalid(
            "fieldName",
            &format!(
                "No '{field}' field in value of type {}",
                Value::Object(target.clone()).type_name()
            ),
        ));
    };
    let not_an_object = |x: &Value| {
        args.invalid(
            "fieldName",
            &format!("Field value is not an object, but instead a {}", x.type_name()),
        )
    };
    match &value {
        Value::Object(sub) => Ok(Value::Object(hoist_one(ctx, &target, &field_name, sub, &prefix)?)),
        Value::List(xs) => {
            let mut results = vec![];
            for item in xs.snapshot() {
                match &item {
                    Value::Object(sub) => {
                        results.push(Value::Object(hoist_one(ctx, &target, &field_name, sub, &prefix)?))
                    }
                    other => return Err(not_an_object(other)),
                }
            }
            Ok(Value::list(results))
        }
        other => Err(not_an_object(other)),
    }
}

/// The object with `field` replaced, in place, by the fields of `sub` (and no class, as in Scala)
fn hoist_one(ctx: &mut Ctx, object: &Object, field: &Value, sub: &Object, prefix: &str) -> R<Object> {
    let result = Object::new();
    for (k, v) in object.entries() {
        if &k == field {
            for (sub_field, sub_value) in sub.entries() {
                let name = format!("{prefix}{}", ctx.stringify(&sub_field)?);
                result.set_str(&name, sub_value);
            }
        } else {
            result.set(k, v);
        }
    }
    Ok(result)
}

/// Return a new object with the fields transformed
///
/// Examples:
/// <mash>
///   { apple: 1, bob: 2, cat: 3 }.map (f v => { (f.toUpper): v * v })  # { APPLE: 1, BOB: 4, CAT: 9 }
/// </mash>
#[builtin_method(shy, returns = crate::infer::Type::OBJECT)]
fn map(
    ctx: &mut Ctx,
    args: &Args,
    target: Object,
    #[param(
        "Function used to transform fields of the object",
        description = "The function may take up to three positional arguments:\n  1) the field name\n  2) the field value\n  3) the index of the field\nIf the function return Objects then they are merged into a single output Object.\nIf it returns other types of values then they will be collected into a List."
    )]
    f: Callable,
) -> R<Value> {
    map_object(ctx, args, "f", &f, &target)
}

/// Return a new object with fields sorted according to a given attribute
#[builtin_method(shy, returns = crate::infer::Type::OBJECT)]
fn sort_by(
    ctx: &mut Ctx,
    args: &Args,
    target: Object,
    #[param(def = descending_param)] descending: bool,
    #[param(def = natural_order_param)] natural_order: bool,
    #[param(
        "One or more functions to apply to elements to extract a value to compare",
        variadic,
        at_least_one
    )]
    attributes: List,
) -> R<Object> {
    let attributes = attribute_functions(args, &attributes)?;
    sort_object_by(ctx, &target, &attributes, descending, natural_order)
}

/// Return a new object with the field names transformed
///
/// Examples:
/// <mash>
///   { foo: 3, bar: 4 }.transformFields (.toUpper) # { FOO: 3, BAR: 4 }
/// </mash>
#[builtin_method(shy, returns = crate::infer::Type::OBJECT)]
fn transform_fields(
    ctx: &mut Ctx,
    target: Object,
    #[param(
        "Function used to transform field names of the object",
        description = "The function may take up to two arguments. If it takes only one argument, the field name is provided.\n  If takes two, the field name and value are provided."
    )]
    f: Callable,
) -> R<Object> {
    let two = takes_two(ctx, &f);
    let mut fields = vec![];
    for (name, value) in target.entries() {
        let new_name = if two {
            f.call(ctx, vec![name, value.clone()])?
        } else {
            f.call1(ctx, name)?
        };
        fields.push((new_name, value));
    }
    Ok(new_object(fields))
}

/// Return a new object with the values transformed
///
/// Examples:
/// <mash>
///   { foo: 3, bar: 4 }.transformValues (n => n * n) # { foo: 9, bar: 16 }
/// </mash>
#[builtin_method(shy, returns = crate::infer::Type::OBJECT)]
fn transform_values(
    ctx: &mut Ctx,
    target: Object,
    #[param(
        "Function used to transform values of the object",
        description = "The function may take up to two arguments. If it takes only one argument, the value is provided.\n  If takes two, the field name and value are provided."
    )]
    f: Callable,
) -> R<Object> {
    let two = takes_two(ctx, &f);
    let mut fields = vec![];
    for (name, value) in target.entries() {
        let new_value = if two {
            f.call(ctx, vec![name.clone(), value])?
        } else {
            f.call1(ctx, value)?
        };
        fields.push((name, new_value));
    }
    Ok(new_object(fields))
}

/// This object without any associated class
#[builtin_method(shy, infer = super::infer_rules::unbless)]
fn unbless(target: Object) -> R<Object> {
    Ok(target.copy_with_class(None))
}

/// Return the values of the fields of this object, as a List
#[builtin_method(shy, returns = crate::infer::Type::seq(crate::infer::Type::ANY))]
fn values(target: Object) -> R<Vec<Value>> {
    Ok(target.values())
}

/// Return a copy of this object with the given field added or updated with the given value.
#[builtin_method(shy, infer = super::infer_rules::with_field)]
fn with_field(target: Object, #[param("Field name")] name: Value, #[param("Field value")] value: Value) -> R<Object> {
    let copy = target.copy_with_class(target.class());
    copy.set(name, value);
    Ok(copy)
}

/// Return a new object retaining only fields satisfying the given predicate
///
/// Examples:
/// <mash>
///   { "foo": 1, "bar": 2, "baz": 3 }.where (.startsWith "b")                   # { "bar": 2, "baz": 3 }
///   { "foo": 1, "bar": 2, "baz": 3 }.where (f v => f.startsWith "f" or v == 3) # { "foo": 1, "baz": 3 }
/// </mash>
#[builtin_method(name = "where", shy, returns = crate::infer::Type::OBJECT)]
fn where_(
    ctx: &mut Ctx,
    args: &Args,
    target: Object,
    #[param(
        "Predicate used to test fields",
        description = "May take up to three positional arguments:\n  1) the field name\n  2) the field value\n  3) the index of the field"
    )]
    predicate: Callable,
) -> R<Value> {
    filter_object(ctx, args, "predicate", &predicate, &target, true)
}

/// Return a new object without any fields satisfying the given predicate
///
/// Examples:
/// <mash>
///   { "foo": 1, "bar": 2, "baz": 3 }.whereNot (.startsWith "b")                   # { "foo": 1 }
///   { "foo": 1, "bar": 2, "baz": 3 }.whereNot (f v => f.startsWith "f" or v == 3) # { "bar": 2 }
/// </mash>
#[builtin_method(shy, returns = crate::infer::Type::OBJECT)]
fn where_not(
    ctx: &mut Ctx,
    args: &Args,
    target: Object,
    #[param(
        "Predicate used to test fields",
        description = "May take up to three positional arguments:\n  1) the field name\n  2) the field value\n  3) the index of the field"
    )]
    predicate: Callable,
) -> R<Value> {
    filter_object(ctx, args, "predicate", &predicate, &target, false)
}

/// Merge objects together
#[builtin(namespace = "core.Object", name = "merge")]
pub(crate) fn merge(args: &Args, #[param("Objects to merge", variadic, at_least_one)] objects: List) -> R<Object> {
    let items = objects.snapshot();
    let items = match items.as_slice() {
        [Value::List(list)] => list.snapshot(),
        _ => items,
    };
    let mut merged = vec![];
    for item in items {
        match item {
            Value::Object(o) => merged.push(o),
            bad => return Err(args.invalid("objects", &format!("Cannot merge value of type {}", bad.type_name()))),
        }
    }
    Ok(merge_objects(&merged))
}

enum Selector {
    Index(usize),
    Function(Callable),
}

impl Selector {
    fn from_arg(args: &Args, name: &str, default: usize) -> R<Selector> {
        if !args.is_provided(name) {
            return Ok(Selector::Index(default));
        }
        match args.value(name) {
            Value::Number(..) => Ok(Selector::Index(args.get::<usize>(name)?)),
            _ => Ok(Selector::Function(args.get::<Callable>(name)?)),
        }
    }

    fn select(&self, ctx: &mut Ctx, item: &Value) -> R<Value> {
        match self {
            Selector::Function(f) => f.call1(ctx, item.clone()),
            Selector::Index(i) => {
                let (elements, collection) = match item {
                    Value::Object(o) => (o.values(), "Object"),
                    Value::List(xs) => (xs.snapshot(), "List"),
                    other => return Err(MashError::new(format!("Invalid pair of type {}", other.type_name()))),
                };
                match elements.len() {
                    0 => Err(MashError::new(format!(
                        "No elements in {collection}, at least two are required"
                    ))),
                    1 => Err(MashError::new(format!(
                        "Only one element in {collection}, at least two are required"
                    ))),
                    n if n <= *i => Err(MashError::new(format!(
                        "Cannot select element {i} from {collection} of length {n}"
                    ))),
                    _ => Ok(elements[*i].clone()),
                }
            }
        }
    }
}

/// Construct an Object from a List of pairs
#[builtin(namespace = "core.Object", name = "fromPairs")]
pub(crate) fn from_pairs(
    ctx: &mut Ctx,
    args: &Args,
    #[param("How to select the fields from the given items")] field_selector: Option<Value>,
    #[param("How to select the values from the given items")] value_selector: Option<Value>,
    #[param("Pairs to merge into an object")] pairs: List,
) -> R<Object> {
    let _ = (field_selector, value_selector);
    let field_selector = Selector::from_arg(args, "fieldSelector", 0)?;
    let value_selector = Selector::from_arg(args, "valueSelector", 1)?;
    let object = Object::new();
    for item in pairs.snapshot() {
        let field = field_selector.select(ctx, &item)?;
        let value = value_selector.select(ctx, &item)?;
        object.set(field, value);
    }
    Ok(object)
}
