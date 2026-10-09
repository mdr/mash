//! Functions on sequences in `collections` (lists, strings as sequences of characters, and in some cases
//! objects), and their `List` method wrappers. Ports of the Scala `collections.*Function`s, with
//! `SequenceLikeAnalyser`, `ToListHelper` and the object cases of `map`, `where` and `grep`.

use std::collections::HashSet;

use mash_macros::builtin;

use super::Methodised;
use super::seq_like::{SequenceLike, characters, reassemble, sequence_like};
use crate::ast::BinOp;
use crate::eval::{Ctx, MashError, R, binary_op, render};
use crate::function::{Args, BuiltinFunction, Callable, Sequence};
use crate::value::{Object, Str, Value};

pub(crate) const SEQUENCE_FUNCTIONS: &[&dyn BuiltinFunction] = &[
    &AllFunction,
    &AllButLastFunction,
    &AnyFunction,
    &ChunkedFunction,
    &ContainsFunction,
    &CountMatchesFunction,
    &EachFunction,
    &FindFunction,
    &FirstFunction,
    &FlatMapFunction,
    &FlattenFunction,
    &GrepFunction,
    &IndexOfFunction,
    &IsEmptyFunction,
    &JoinFunction,
    &LastFunction,
    &MapFunction,
    &NonEmptyFunction,
    &ReduceFunction,
    &ReverseFunction,
    &SkipFunction,
    &SkipUntilFunction,
    &SkipWhileFunction,
    &SlidingFunction,
    &SumFunction,
    &SumByFunction,
    &TakeWhileFunction,
    &UniqueFunction,
    &WhereFunction,
    &WhereNotFunction,
    &ZipFunction,
];

macro_rules! methodise {
    ($($static:ident = $function:ident ($target:literal);)*) => {
        $(pub(crate) static $static: Methodised = Methodised::new(&$function, $target, &[], false);)*
    };
}

methodise! {
    ALL_METHOD = AllFunction("sequence");
    ALL_BUT_LAST_METHOD = AllButLastFunction("sequence");
    ANY_METHOD = AnyFunction("sequence");
    CHUNKED_METHOD = ChunkedFunction("sequence");
    CONTAINS_METHOD = ContainsFunction("sequence");
    COUNT_MATCHES_METHOD = CountMatchesFunction("sequence");
    EACH_METHOD = EachFunction("sequence");
    FLATTEN_METHOD = FlattenFunction("sequence");
    FLAT_MAP_METHOD = FlatMapFunction("sequence");
    FIND_METHOD = FindFunction("sequence");
    FIRST_METHOD = FirstFunction("sequence");
    GREP_METHOD = GrepFunction("input");
    INDEX_OF_METHOD = IndexOfFunction("sequence");
    IS_EMPTY_METHOD = IsEmptyFunction("sequence");
    JOIN_METHOD = JoinFunction("sequence");
    LAST_METHOD = LastFunction("sequence");
    MAP_METHOD = MapFunction("sequence");
    NON_EMPTY_METHOD = NonEmptyFunction("sequence");
    REDUCE_METHOD = ReduceFunction("sequence");
    REVERSE_METHOD = ReverseFunction("sequence");
    SKIP_METHOD = SkipFunction("sequence");
    SKIP_UNTIL_METHOD = SkipUntilFunction("sequence");
    SKIP_WHILE_METHOD = SkipWhileFunction("sequence");
    SLIDING_METHOD = SlidingFunction("sequence");
    SUM_BY_METHOD = SumByFunction("sequence");
    SUM_METHOD = SumFunction("sequence");
    TAKE_WHILE_METHOD = TakeWhileFunction("sequence");
    UNIQUE_METHOD = UniqueFunction("sequence");
    WHERE_METHOD = WhereFunction("sequence");
    WHERE_NOT_METHOD = WhereNotFunction("sequence");
}

// ------------------------------------------------------------------------------------------------ helpers

/// A string's text with each char mapped through `f`
fn modify_chars(s: &Str, f: impl FnOnce(Vec<char>) -> Vec<char>) -> Value {
    let chars: Vec<char> = s.as_str().chars().collect();
    Value::Str(s.with_text(f(chars).into_iter().collect::<String>()))
}

fn object_of(fields: impl IntoIterator<Item = (Value, Value)>) -> Object {
    let object = Object::new();
    for (k, v) in fields {
        object.set(k, v);
    }
    object
}

/// Scala `MashObject.merge`: fields of all, later ones winning; the class of the last that has one
pub(crate) fn merge_objects(objects: &[Object]) -> Object {
    let class = objects.iter().rev().find_map(Object::class);
    let merged = Object::with_class(class);
    for object in objects {
        for (k, v) in object.entries() {
            merged.set(k, v);
        }
    }
    merged
}

fn is_function_or_method(f: &Callable) -> bool {
    matches!(f.0, Value::Function(_) | Value::Builtin(_) | Value::BoundMethod(_))
}

/// Call a function with an item, and also its index if it can take two arguments (Scala
/// `validateFunction1Or2`)
fn call_with_index(ctx: &mut Ctx, f: &Callable, takes_index: bool, item: Value, index: usize) -> R<Value> {
    if takes_index {
        f.call(ctx, vec![item, Value::num(index)])
    } else {
        f.call1(ctx, item)
    }
}

fn takes_index(ctx: &Ctx, f: &Callable) -> bool {
    is_function_or_method(f) && f.accepts_positional(ctx, 2)
}

/// Call `f` on each field of an object with as many of (name, value, index) as it takes (Scala
/// `validateFunction1Or2Or3`)
fn map_fields(ctx: &mut Ctx, args: &Args, name: &str, f: &Callable, obj: &Object) -> R<Vec<(Value, Value, Value)>> {
    if !is_function_or_method(f) {
        return Err(args.invalid(name, &format!("Must be a function, but was a {}", f.0.type_name())));
    }
    let arity = if f.accepts_positional(ctx, 3) {
        3
    } else if f.accepts_positional(ctx, 2) {
        2
    } else {
        1
    };
    let mut results = vec![];
    for (i, (field, value)) in obj.entries().into_iter().enumerate() {
        let mut call_args = vec![field.clone(), value.clone(), Value::num(i)];
        call_args.truncate(arity);
        results.push((field, value, f.call(ctx, call_args)?));
    }
    Ok(results)
}

/// `map` over an object (Scala `ObjectClass.MapMethod.doMap`): results that are all objects are merged,
/// otherwise collected into a list
pub(crate) fn map_object(ctx: &mut Ctx, args: &Args, name: &str, f: &Callable, obj: &Object) -> R<Value> {
    let mapped: Vec<Value> = map_fields(ctx, args, name, f, obj)?
        .into_iter()
        .map(|(_, _, r)| r)
        .collect();
    if mapped.is_empty() {
        return Ok(Value::Object(Object::new()));
    }
    let objects: Vec<Object> = mapped
        .iter()
        .filter_map(|v| match v {
            Value::Object(o) => Some(o.clone()),
            _ => None,
        })
        .collect();
    if objects.len() == mapped.len() {
        Ok(Value::Object(merge_objects(&objects)))
    } else {
        Ok(Value::list(mapped))
    }
}

/// The fields of an object for which `f` is (or, if not `keep`, isn't) truthy (Scala `ObjectClass.WhereMethod`
/// and `WhereNotMethod`)
pub(crate) fn filter_object(
    ctx: &mut Ctx,
    args: &Args,
    name: &str,
    f: &Callable,
    obj: &Object,
    keep: bool,
) -> R<Value> {
    let fields = map_fields(ctx, args, name, f, obj)?;
    Ok(Value::Object(object_of(
        fields
            .into_iter()
            .filter(|(_, _, r)| r.is_truthy() == keep)
            .map(|(k, v, _)| (k, v)),
    )))
}

fn add_all(ctx: &mut Ctx, items: Vec<Value>) -> R<Value> {
    let mut items = items.into_iter();
    let mut total = items.next().expect("non-empty");
    for item in items {
        total = binary_op(ctx, BinOp::Plus, &total, &item)?;
    }
    Ok(total)
}

/// Scala `FlattenFunction.flatten`: concatenate lists, merge objects or concatenate strings. An empty
/// sequence gives back `input`.
fn flatten_values(values: Vec<Value>, input: &Value) -> R<Value> {
    if values.is_empty() {
        return Ok(input.clone());
    }
    if values.iter().all(|v| matches!(v, Value::List(_))) {
        let mut items = vec![];
        for v in &values {
            if let Value::List(xs) = v {
                items.extend(xs.snapshot());
            }
        }
        return Ok(Value::list(items));
    }
    if values.iter().all(|v| matches!(v, Value::Object(_))) {
        let objects: Vec<Object> = values
            .iter()
            .filter_map(|v| match v {
                Value::Object(o) => Some(o.clone()),
                _ => None,
            })
            .collect();
        return Ok(Value::Object(merge_objects(&objects)));
    }
    if values.iter().all(|v| matches!(v, Value::Str(_))) {
        let text: String = values.iter().filter_map(Value::as_str).collect();
        let tag = match input {
            Value::Str(s) => s.tag(),
            _ => None,
        };
        return Ok(Value::Str(Str::new(text).with_tag(tag)));
    }
    let first = &values[0];
    let bad = match first {
        Value::Str(_) => values.iter().find(|v| !matches!(v, Value::Str(_))),
        Value::List(_) => values.iter().find(|v| !matches!(v, Value::List(_))),
        _ => Some(first),
    }
    .unwrap_or(first);
    Err(MashError::new(format!("Invalid item of type {}", bad.type_name())))
}

// ------------------------------------------------------------------------------------------------ predicates

/// Check whether a predicate holds for every element in a sequence
///
/// Returns true if the given predicate returns a truthy result for every element in the given sequence;
/// false otherwise.
///
/// Examples:
/// <mash>
///   all (_ > 0) [1, 2, 3]  # true
///   all (_ > 0) [1, -2, 3] # false
///   all (_ > 0) []         # true
/// </mash>
#[builtin(namespace = "collections", infer = super::inference::infer_all, completion = super::inference::complete_map_function)]
pub fn all(
    ctx: &mut Ctx,
    #[param("Predicate used to test elements of the sequence")] predicate: Callable,
    #[param("Sequence of elements to test")] sequence: Sequence,
) -> R<bool> {
    for item in sequence.items() {
        if !predicate.call1(ctx, item)?.is_truthy() {
            return Ok(false);
        }
    }
    Ok(true)
}

/// Check whether a predicate holds for any element in a sequence
///
/// Returns true if the given predicate returns a truthy result for at least one element in the given
/// sequence; false otherwise.
///
/// <mash>
/// Examples:
///   any (_ > 0) [1, -2, 3]   # true
///   any (_ > 0) [-1, -2, -3] # false
///   any (_ > 0) []           # false
/// </mash>
#[builtin(namespace = "collections", infer = super::inference::infer_all, completion = super::inference::complete_map_function)]
pub fn any(
    ctx: &mut Ctx,
    #[param("Predicate used to test elements of the sequence")] predicate: Callable,
    #[param("Sequence of elements to test")] sequence: Sequence,
) -> R<bool> {
    for item in sequence.items() {
        if predicate.call1(ctx, item)?.is_truthy() {
            return Ok(true);
        }
    }
    Ok(false)
}

/// Count how many times a predicate holds within a sequence
///
/// Examples:
/// <mash>
///   countMatches (_ > 3) [1, 2, 3, 4, 5] # 2
/// </mash>
#[builtin(namespace = "collections", infer = super::inference::infer_count_matches, completion = super::inference::complete_map_function)]
pub fn count_matches(
    ctx: &mut Ctx,
    #[param("Predicate used to test elements of the sequence")] predicate: Callable,
    #[param("Sequence to count matches in")] sequence: Sequence,
) -> R<usize> {
    let mut count = 0;
    for item in sequence.items() {
        if predicate.call1(ctx, item)?.is_truthy() {
            count += 1;
        }
    }
    Ok(count)
}

/// Find an element in the sequence for which a predicate holds
///
/// Returns the first element of the sequence for which the predicate returns a truthy value.
/// null is returned if no matching element is found.
///
/// Examples:
/// <mash>
///   find (_ > 10) [1, 20, 3]  # 20
///   find (_ > 100) [1, 2, 3]  # null
/// </mash>
#[builtin(namespace = "collections", infer = super::inference::infer_find, completion = super::inference::complete_map_function)]
pub fn find(
    ctx: &mut Ctx,
    #[param("Predicate used to test elements of the sequence")] predicate: Callable,
    #[param("Sequence to search")] sequence: Sequence,
) -> R<Value> {
    for item in sequence.items() {
        if predicate.call1(ctx, item.clone())?.is_truthy() {
            return Ok(item);
        }
    }
    Ok(Value::Null)
}

/// Check whether a sequence contains an element
///
/// Examples:
/// <mash>
///   contains 2 [1, 2, 3] # true
///   contains 9 [1, 2, 3] # false
/// </mash>
#[builtin(namespace = "collections", returns = crate::infer::Type::BOOLEAN)]
pub fn contains(
    #[param("Element to test for membership")] element: Value,
    #[param("Sequence to check whether it contains the given element")] sequence: Sequence,
) -> R<bool> {
    Ok(sequence.items().contains(&element))
}

/// Return the index of the first occurrence of the given element, if any, else null
///
/// Examples:
/// <mash>
///   indexOf 2 [1, 2, 3] # 1
///   indexOf 9 [1, 2, 3] # null
/// </mash>
#[builtin(namespace = "collections", returns = crate::infer::Type::NUMBER)]
pub fn index_of(
    #[param("Element to test for membership")] element: Value,
    #[param("Sequence to check whether it contains the given element")] sequence: Sequence,
) -> R<Value> {
    Ok(sequence
        .items()
        .iter()
        .position(|item| *item == element)
        .map_or(Value::Null, Value::num))
}

/// Check whether a given sequence is empty
///
/// Examples:
/// <mash>
///   isEmpty []        # true
///   isEmpty [1, 2, 3] # false
/// </mash>
#[builtin(namespace = "collections", returns = crate::infer::Type::BOOLEAN)]
pub fn is_empty(#[param("Sequence of elements to test")] sequence: Sequence) -> R<bool> {
    Ok(sequence.items().is_empty())
}

/// Check whether a given sequence is non-empty
///
/// Examples:
/// <mash>
///   nonEmpty []        # false
///   nonEmpty [1, 2, 3] # true
/// </mash>
#[builtin(namespace = "collections", returns = crate::infer::Type::BOOLEAN)]
pub fn non_empty(#[param("Sequence of elements to test")] sequence: Sequence) -> R<bool> {
    Ok(!sequence.items().is_empty())
}

// ------------------------------------------------------------------------------------------------ transforming

/// Transform each element of a sequence by a given function
///
/// The given function is applied to each element of the input sequence to produce a sequence of
/// transformed output elements.
///
/// Examples:
/// <mash>
///   map (_ * 2) [1, 2, 3]             # [2, 4, 6]
///   map (_ * 2) []                    # []
///   map (v i => [v i]) ["a", "b", "c"] # [["a", 0], ["b", 1], ["c", 2]]
/// </mash>
#[builtin(namespace = "collections", infer = super::inference::infer_map, completion = super::inference::complete_map_function)]
pub fn map(
    ctx: &mut Ctx,
    args: &Args,
    #[param(
        "Function used to transform elements of the sequence",
        description = "If the function can take two arguments, the index is supplied as the second argument"
    )]
    f: Callable,
    #[param("Sequence to map over")] sequence: Value,
) -> R<Value> {
    let _ = sequence;
    let items = match sequence_like(ctx, args, "sequence")? {
        SequenceLike::Object(obj) => return map_object(ctx, args, "f", &f, &obj),
        SequenceLike::List(items) => items,
        SequenceLike::Str(ref s) => characters(s),
    };
    let with_index = takes_index(ctx, &f);
    let mut mapped = Vec::with_capacity(items.len());
    for (i, item) in items.into_iter().enumerate() {
        mapped.push(call_with_index(ctx, &f, with_index, item, i)?);
    }
    Ok(reassemble(args.value("sequence"), mapped))
}

/// Transform each element of a sequence by a given function, and then flatten
///
/// The given function is applied to each element of the input sequence and is expected to yield a sequence
/// for each element. The result is flattened to produce a sequence of transformed output elements.
///
/// Examples:
/// <mash>
///   flatMap (x => [x * 10, x * 100]) [1, 2, 3] # [20, 200, 40, 400, 60, 600]
///   flatMap (n i => [n, i]) [1, 2, 3]          # [1, 0, 2, 1, 3, 2]
/// </mash>
#[builtin(namespace = "collections", infer = super::inference::infer_flat_map, completion = super::inference::complete_function_argument)]
pub fn flat_map(
    ctx: &mut Ctx,
    args: &Args,
    #[param(
        "Function used to transform elements of the sequence",
        description = "Must return a sequence. If the function can take two arguments, the index is supplied as the second argument."
    )]
    f: Callable,
    #[param("Sequence to map over")] sequence: Value,
) -> R<Value> {
    let items = match sequence_like(ctx, args, "sequence")? {
        SequenceLike::Object(obj) => return map_object(ctx, args, "f", &f, &obj),
        SequenceLike::List(items) => items,
        SequenceLike::Str(ref s) => characters(s),
    };
    let with_index = takes_index(ctx, &f);
    let mut mapped = Vec::with_capacity(items.len());
    for (i, item) in items.into_iter().enumerate() {
        mapped.push(call_with_index(ctx, &f, with_index, item, i)?);
    }
    flatten_values(mapped, &sequence)
}

/// Flatten a sequence of sequences
#[builtin(namespace = "collections", infer = super::inference::infer_flatten)]
pub fn flatten(#[param("Sequence of sequences to flatten")] sequence: Sequence) -> R<Value> {
    let input = match &sequence {
        Sequence::List(xs) => Value::List(xs.clone()),
        Sequence::Str(s) => Value::Str(s.clone()),
    };
    flatten_values(sequence.items(), &input)
}

/// Perform an action for each element in a sequence
#[builtin(namespace = "collections", infer = super::inference::infer_each)]
pub fn each(
    ctx: &mut Ctx,
    #[param("Function used to act on elements of the sequence")] action: Callable,
    #[param("Sequence to run an action over")] sequence: Sequence,
) -> R<()> {
    let with_index = takes_index(ctx, &action);
    for (i, item) in sequence.items().into_iter().enumerate() {
        call_with_index(ctx, &action, with_index, item, i)?;
    }
    Ok(())
}

/// Find all the elements in the sequence for which a predicate holds
///
/// Examples:
/// <mash>
///   where (_ > 1) [1, 2, 3, 2, 1] # [2, 3, 2]
/// </mash>
#[builtin(namespace = "collections", infer = super::inference::infer_where, completion = super::inference::complete_map_function)]
pub fn where_(
    ctx: &mut Ctx,
    args: &Args,
    #[param("Predicate used to test elements of the sequence")] predicate: Callable,
    #[param("Sequence to find values in")] sequence: Value,
) -> R<Value> {
    filter(ctx, args, &predicate, &sequence, true)
}

/// Find all the elements in the sequence for which a predicate does not hold
///
/// Examples:
/// <mash>
///   whereNot (_ > 1) [1, 2, 3, 2, 1] # [1, 2, 2, 1]
/// </mash>
#[builtin(namespace = "collections", infer = super::inference::infer_where, completion = super::inference::complete_map_function)]
pub fn where_not(
    ctx: &mut Ctx,
    args: &Args,
    #[param("Predicate used to test elements of the sequence")] predicate: Callable,
    #[param("Sequence to find values in")] sequence: Value,
) -> R<Value> {
    filter(ctx, args, &predicate, &sequence, false)
}

fn filter(ctx: &mut Ctx, args: &Args, predicate: &Callable, sequence: &Value, keep: bool) -> R<Value> {
    let items = match sequence_like(ctx, args, "sequence")? {
        SequenceLike::Object(obj) => return filter_object(ctx, args, "predicate", predicate, &obj, keep),
        SequenceLike::List(items) => items,
        SequenceLike::Str(ref s) => characters(s),
    };
    let mut kept = vec![];
    for item in items {
        if predicate.call1(ctx, item.clone())?.is_truthy() == keep {
            kept.push(item);
        }
    }
    Ok(reassemble(sequence, kept))
}

/// Combine the elements of a sequence into a string using a separator
///
/// Each element of the sequence will be converted into a string and joined with the given separator.
///
/// Examples:
/// <mash>
///   join ":" [1, 2, 3] # "1:2:3"
///   join ":" []        # ""
///   join [1, 2, 3]     # "123"
/// </mash>
#[builtin(namespace = "collections", returns = crate::infer::Type::STRING)]
pub fn join(
    ctx: &mut Ctx,
    #[param(
        "String used to separate elements in the result string; defaults to \"\"",
        default = ""
    )]
    separator: Str,
    #[param("Sequence to combine into a string")] sequence: Sequence,
) -> R<String> {
    let mut chunks = vec![];
    for item in sequence.items() {
        chunks.push(ctx.stringify(&item)?);
    }
    Ok(chunks.join(separator.as_str()))
}

/// Reduce all the elements of the sequence to a single value using a combining function
///
/// Examples:
/// <mash>
///   reduce (x y => x + y) [1, 2, 3] # 6
///   reduce (x y => x + y) 1 [2, 3]  # 6
/// </mash>
#[builtin(namespace = "collections", infer = super::inference::infer_reduce)]
pub fn reduce(
    ctx: &mut Ctx,
    args: &Args,
    #[param("Function to accumulate values. Must have two arguments, first the ")] accumulator: Value,
    #[param("Initial value")] initial: Option<Value>,
    #[param("Sequence of items to reduce")] sequence: Sequence,
) -> R<Value> {
    let mut items = sequence.items().into_iter();
    let mut acc = match initial {
        Some(initial) => initial,
        None => items
            .next()
            .ok_or_else(|| args.invalid("sequence", "Empty sequence and no initial value provided"))?,
    };
    if !matches!(accumulator, Value::Function(_) | Value::Builtin(_)) {
        return Err(args.invalid(
            "accumulator",
            &format!("Invalid accumulator function of type {}", accumulator.type_name()),
        ));
    }
    for item in items {
        acc = ctx.call_values(&accumulator, vec![acc, item])?;
    }
    Ok(acc)
}

/// Sum all the elements of a sequence
///
/// Add all the elements in the sequence together, as if they were combined with the '+' operator.
///
/// Examples:
/// <mash>
///   sum [1, 2, 3]      # 6
///   sum ["foo", "bar"] # "foobar"
///   sum [[1, 2], [3]]  # [1, 2, 3]
///   sum []             # 0
///   sum "" []          # ""
/// </mash>
#[builtin(namespace = "collections", infer = super::inference::infer_sum)]
pub fn sum(
    ctx: &mut Ctx,
    #[param("Value used as the sum of an empty list (default 0)", default = 0i64)] empty_value: Value,
    #[param("Sequence of items to sum")] sequence: Sequence,
) -> R<Value> {
    let items = sequence.items();
    if items.is_empty() {
        Ok(empty_value)
    } else {
        add_all(ctx, items)
    }
}

/// Sum the elements of a sequence using an attribute
///
/// Examples:
/// <mash>
///   sumBy length ["foo", "bar", "baz"] # 9
/// </mash>
#[builtin(namespace = "collections", infer = super::inference::infer_sum_by, completion = super::inference::complete_map_function)]
pub fn sum_by(
    ctx: &mut Ctx,
    #[param("Function to extract a value from to sum")] attribute: Callable,
    #[param("Value used as the sum of an empty list (default 0)", default = 0i64)] empty_value: Value,
    #[param("Sequence to find the sum of")] sequence: Sequence,
) -> R<Value> {
    let mut values = vec![];
    for item in sequence.items() {
        values.push(attribute.call1(ctx, item)?);
    }
    if values.is_empty() {
        Ok(empty_value)
    } else {
        add_all(ctx, values)
    }
}

/// Find all the distinct elements in a sequence
///
/// Examples:
/// <mash>
///   unique [1, 2, 3, 2, 1] # [1, 2, 3]
/// </mash>
#[builtin(namespace = "collections", infer = super::inference::infer_same_sequence)]
pub fn unique(#[param("Sequence to remove duplicates from")] sequence: Sequence) -> R<Value> {
    // Nothing can mutate the items while this runs, so their hashes stay valid
    #[allow(clippy::mutable_key_type)]
    let mut seen = HashSet::new();
    let items: Vec<Value> = sequence
        .items()
        .into_iter()
        .filter(|item| seen.insert(item.clone()))
        .collect();
    Ok(sequence.rebuild(items))
}

/// Zip two sequences
///
/// Examples:
/// <mash>
///   zip [1, 2, 3] [4, 5] # [[1, 4], [2, 5]]
/// </mash>
#[builtin(namespace = "collections", infer = super::inference::infer_zip)]
pub fn zip(
    #[param("First sequence")] sequence1: Sequence,
    #[param("Second sequence")] sequence2: Sequence,
) -> R<Vec<Value>> {
    Ok(sequence1
        .items()
        .into_iter()
        .zip(sequence2.items())
        .map(|(l, r)| Value::list(vec![l, r]))
        .collect())
}

/// Find all the elements in the input which match the given query somewhere in its String representation
///
/// If given a String, matching lines will be returned.
/// If given an Object, matching field/values will be retained.
///
/// Examples:
/// <mash>
///   grep "b" ["apple", "book", "car"] # ["book"]
/// </mash>
#[builtin(namespace = "collections", infer = super::inference::infer_grep)]
#[allow(clippy::too_many_arguments)]
pub fn grep(
    ctx: &mut Ctx,
    args: &Args,
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
    #[param("Sequence or string to search. A string will be treated as a sequence of lines.")] input: Value,
) -> R<Value> {
    let _ = input;
    let query = ctx.stringify(&query)?;
    let options = GrepOptions {
        ignore_case,
        regex,
        negate,
        first,
        ignore_fields: true,
    };
    match sequence_like(ctx, args, "input")? {
        SequenceLike::List(items) => run_grep(ctx, items, &query, &options),
        SequenceLike::Str(s) => {
            let lines = split_into_lines(s.as_str())
                .into_iter()
                .map(|l| Value::Str(s.with_text(l)))
                .collect();
            run_grep(ctx, lines, &query, &options)
        }
        SequenceLike::Object(obj) => grep_object(
            ctx,
            &obj,
            &query,
            GrepOptions {
                ignore_fields: false,
                ..options
            },
        ),
    }
}

pub(crate) struct GrepOptions {
    pub(crate) ignore_case: bool,
    pub(crate) regex: bool,
    pub(crate) negate: bool,
    pub(crate) first: bool,
    /// Match an object item on its field values only
    pub(crate) ignore_fields: bool,
}

/// Scala `StringUtils.splitIntoLines`
pub(crate) fn split_into_lines(s: &str) -> Vec<String> {
    if s.is_empty() {
        return vec![];
    }
    let mut lines: Vec<String> = s
        .split('\n')
        .map(|l| l.strip_suffix('\r').unwrap_or(l).to_string())
        .collect();
    if s.ends_with('\n') {
        lines.pop();
    }
    lines
}

pub(crate) fn run_grep(ctx: &mut Ctx, items: Vec<Value>, query: &str, options: &GrepOptions) -> R<Value> {
    let mut matching = vec![];
    for item in items {
        if grep_matches(ctx, &item, query, options)? {
            if options.first {
                return Ok(item);
            }
            matching.push(item);
        }
    }
    Ok(if options.first {
        Value::Null
    } else {
        Value::list(matching)
    })
}

fn grep_matches(ctx: &mut Ctx, value: &Value, query: &str, options: &GrepOptions) -> R<bool> {
    let text = match value {
        Value::Object(obj) if options.ignore_fields => obj.values().iter().map(render).collect::<Vec<_>>().join("\n"),
        _ => ctx.stringify(value)?,
    };
    let matches = if options.regex {
        let regex = crate::builtins::core::compile_regex(query, options.ignore_case)?;
        regex
            .is_match(&text)
            .map_err(crate::builtins::core::regex_runtime_error)?
    } else if options.ignore_case {
        text.to_lowercase().contains(&query.to_lowercase())
    } else {
        text.contains(query)
    };
    Ok(matches != options.negate)
}

/// `grep` over an object: the fields whose `{ name: value }` matches (Scala `ObjectClass.GrepMethod.doGrep`)
pub(crate) fn grep_object(ctx: &mut Ctx, obj: &Object, query: &str, options: GrepOptions) -> R<Value> {
    let items: Vec<Value> = obj
        .entries()
        .into_iter()
        .map(|(k, v)| Value::Object(object_of([(k, v)])))
        .collect();
    let result = run_grep(ctx, items, query, &options)?;
    if options.first {
        return Ok(result);
    }
    let Value::List(matching) = result else { unreachable!() };
    let objects: Vec<Object> = matching
        .snapshot()
        .into_iter()
        .filter_map(|v| match v {
            Value::Object(o) => Some(o),
            _ => None,
        })
        .collect();
    let merged = object_of(objects.iter().flat_map(Object::entries));
    Ok(Value::Object(merged))
}

// ------------------------------------------------------------------------------------------------ slicing

/// Find the first element(s) of a sequence
///
/// If a count n is provided, the first n items of the sequence will be returned.
/// If there are fewer than n in the sequence, the entire sequence is returned.
/// If a count n is omitted, then the first item of the sequence is returned, if nonempty, else null.
///
/// Examples:
/// <mash>
///   first 3 [1, 2, 3, 4 5]  # [1, 2, 3]
///   first 5 [1, 2, 3]       # [1, 2, 3]
///   first [1, 2, 3]         # 1
///   first []                # null
///   first 3 "abcdef"        # "abc
/// </mash>
#[builtin(namespace = "collections", infer = super::inference::infer_first_last)]
pub fn first(
    ctx: &mut Ctx,
    args: &Args,
    #[param("Number of elements")] n: Option<usize>,
    #[param(
        "Sequence to find the first value(s) of",
        description = "Can be a List, String or Object"
    )]
    sequence: Value,
) -> R<Value> {
    let _ = sequence;
    take_end(ctx, args, n, false)
}

/// Find the last element(s) of a sequence
///
/// If a count n is provided, the last n items of the sequence will be returned.
/// If there are fewer than n in the sequence, the entire sequence is returned.
/// If a count n is omitted, then the last item of the sequence is returned, if nonempty, else null.
///
/// Examples:
/// <mash>
///   last 3 [1, 2, 3, 4, 5] # [3, 4, 5]
///   last 5 [1, 2, 3]       # [1, 2, 3]
///   last [1, 2, 3]         # 3
///   last []                # null
///   last 3 "abcdef"        # "def"
/// </mash>
#[builtin(namespace = "collections", infer = super::inference::infer_first_last)]
pub fn last(
    ctx: &mut Ctx,
    args: &Args,
    #[param("Number of elements to take")] n: Option<usize>,
    #[param(
        "Sequence to find the last value(s) of",
        description = "Can be a List, String or Object"
    )]
    sequence: Value,
) -> R<Value> {
    let _ = sequence;
    take_end(ctx, args, n, true)
}

/// The range of `count` items at the start (or end, if `from_end`) of `len` items
fn end_range(len: usize, count: usize, from_end: bool) -> std::ops::Range<usize> {
    let count = count.min(len);
    if from_end { len - count..len } else { 0..count }
}

/// `first`/`last`: with a count, that many items as a sequence of the same kind; without, the single item or
/// null
fn take_end(ctx: &mut Ctx, args: &Args, n: Option<usize>, from_end: bool) -> R<Value> {
    Ok(match (sequence_like(ctx, args, "sequence")?, n) {
        (SequenceLike::List(items), Some(count)) => {
            Value::list(items[end_range(items.len(), count, from_end)].to_vec())
        }
        (SequenceLike::List(items), None) => {
            let item = if from_end { items.last() } else { items.first() };
            item.cloned().unwrap_or(Value::Null)
        }
        (SequenceLike::Str(s), Some(count)) => {
            modify_chars(&s, |chars| chars[end_range(chars.len(), count, from_end)].to_vec())
        }
        (SequenceLike::Str(s), None) => {
            let chars: Vec<char> = s.as_str().chars().collect();
            let c = if from_end { chars.last() } else { chars.first() };
            // The character keeps the string's tag (Scala `MashString.first`/`last`)
            c.map_or(Value::Null, |c| Value::Str(s.with_text(c.to_string())))
        }
        (SequenceLike::Object(obj), count) => {
            let entries = obj.entries();
            if count.is_none() && entries.is_empty() {
                Value::Null
            } else {
                let range = end_range(entries.len(), count.unwrap_or(1), from_end);
                Value::Object(object_of(entries[range].iter().cloned()))
            }
        }
    })
}

/// Take all but the last element(s) of a sequence
///
/// If a count n is provided, the last n items of the sequence will be omitted.
/// If there are fewer than n in the sequence, the empty sequence is returned.
/// If a count n is omitted, the last item of the sequence is omitted.
///
/// Examples:
/// <mash>
///   allButLast [1, 2, 3, 4, 5]   # [1, 2, 3, 4]
///   allButLast 3 [1, 2, 3, 4, 5] # [1, 2]
///   allButLast 5 [1, 2, 3]       # []
/// </mash>
#[builtin(namespace = "collections", infer = super::inference::infer_same_sequence)]
pub fn all_but_last(
    ctx: &mut Ctx,
    args: &Args,
    #[param("Number of elements to omit (default 1)", default = 1i64)] n: usize,
    #[param("Sequence to find the value(s) of", description = "Can be a List, String or Object")] sequence: Value,
) -> R<Value> {
    let _ = sequence;
    drop_end(ctx, args, n, true)
}

/// Skip elements from the start of a sequence
///
/// If there are fewer elements in the sequence than are requested to skip, the empty sequence is returned.
///
/// Examples:
/// <mash>
///   skip 2 [1, 2, 3, 4] # [3, 4]
///   skip 3 [1, 2]       # []
///   skip 3 "abcdef"     # "def"
/// </mash>
#[builtin(namespace = "collections", infer = super::inference::infer_same_sequence)]
pub fn skip(
    ctx: &mut Ctx,
    args: &Args,
    #[param("Number of elements to skip (default 1)", default = 1i64)] n: usize,
    #[param("Sequence to skip values from", description = "Can be a List, String or Object")] sequence: Value,
) -> R<Value> {
    let _ = sequence;
    drop_end(ctx, args, n, false)
}

/// Drop `n` items from the start (or end, if `from_end`) of a list, string or object
fn drop_end(ctx: &mut Ctx, args: &Args, n: usize, from_end: bool) -> R<Value> {
    let keep = |len: usize| {
        let n = n.min(len);
        if from_end { 0..len - n } else { n..len }
    };
    Ok(match sequence_like(ctx, args, "sequence")? {
        SequenceLike::List(items) => Value::list(items[keep(items.len())].to_vec()),
        SequenceLike::Str(s) => modify_chars(&s, |chars| chars[keep(chars.len())].to_vec()),
        SequenceLike::Object(obj) => {
            let entries = obj.entries();
            Value::Object(object_of(entries[keep(entries.len())].iter().cloned()))
        }
    })
}

/// Reverse a List, String or Object
///
/// Examples:
/// <mash>
///   reverse [1, 2, 3] # [3, 2, 1]
///   reverse []        # []
///   reverse "part"    # "trap"
/// </mash>
#[builtin(namespace = "collections", infer = super::inference::infer_same_sequence)]
pub fn reverse(ctx: &mut Ctx, args: &Args, #[param("Sequence to reverse")] sequence: Value) -> R<Value> {
    let _ = sequence;
    Ok(match sequence_like(ctx, args, "sequence")? {
        SequenceLike::List(mut items) => {
            items.reverse();
            Value::list(items)
        }
        SequenceLike::Str(s) => modify_chars(&s, |mut chars| {
            chars.reverse();
            chars
        }),
        SequenceLike::Object(obj) => {
            let reversed = Object::with_class(obj.class());
            for (k, v) in obj.entries().into_iter().rev() {
                reversed.set(k, v);
            }
            Value::Object(reversed)
        }
    })
}

/// Skip elements from the start of a sequence while a predicate holds
///
/// Examples:
/// <mash>
///   skipWhile (_ < 3) [1, 2, 3, 2, 1] # [3, 2, 1]
/// </mash>
#[builtin(namespace = "collections", infer = super::inference::infer_where, completion = super::inference::complete_map_function)]
pub fn skip_while(
    ctx: &mut Ctx,
    #[param("Predicate used to test elements of the sequence")] predicate: Callable,
    #[param("Sequence to skip values from")] sequence: Sequence,
) -> R<Value> {
    skip_or_take_while(ctx, &predicate, &sequence, true, true)
}

/// Skip elements from the start of a sequence until a predicate holds
///
/// Examples:
/// <mash>
///   skipUntil (_ < 3) [1, 2, 3, 2, 1] # [2, 1]
/// </mash>
#[builtin(namespace = "collections", infer = super::inference::infer_where, completion = super::inference::complete_map_function)]
pub fn skip_until(
    ctx: &mut Ctx,
    #[param("Predicate used to test elements of the sequence")] predicate: Callable,
    #[param("Sequence to skip values from")] sequence: Sequence,
) -> R<Value> {
    skip_or_take_while(ctx, &predicate, &sequence, false, true)
}

/// Take elements from the start of a sequence while a predicate holds
///
/// Examples:
/// <mash>
///   takeWhile (_ < 3) [1, 2, 3, 2, 1] # [1, 2]
/// </mash>
#[builtin(namespace = "collections", infer = super::inference::infer_where, completion = super::inference::complete_map_function)]
pub fn take_while(
    ctx: &mut Ctx,
    #[param("Predicate used to test elements of the sequence")] predicate: Callable,
    #[param("Sequence to take values from")] sequence: Sequence,
) -> R<Value> {
    skip_or_take_while(ctx, &predicate, &sequence, true, false)
}

/// The items after (`skip`) or before (not `skip`) the first one for which the predicate's truthiness isn't
/// `while_truthy`
fn skip_or_take_while(
    ctx: &mut Ctx,
    predicate: &Callable,
    sequence: &Sequence,
    while_truthy: bool,
    skip: bool,
) -> R<Value> {
    let items = sequence.items();
    let mut split = items.len();
    for (i, item) in items.iter().enumerate() {
        if predicate.call1(ctx, item.clone())?.is_truthy() != while_truthy {
            split = i;
            break;
        }
    }
    let (before, after) = items.split_at(split);
    Ok(sequence.rebuild(if skip { after.to_vec() } else { before.to_vec() }))
}

/// Split a sequence up into chunks of a given size
///
/// Returns a list of chunks (sequences of elements of the given size).
/// Any elements left over will be placed into a final chunk.
///
/// Examples:
/// <mash>
///   chunked 2 [1, 2, 3, 4, 5] # [[1, 2], [3, 4], [5]]
/// </mash>
#[builtin(namespace = "collections", infer = super::inference::infer_windows)]
pub fn chunked(
    args: &Args,
    #[param("Number of elements in each chunk")] size: i64,
    #[param("Sequence to split into chunks")] sequence: Sequence,
) -> R<Vec<Value>> {
    let size = positive_size(args, size)?;
    Ok(sequence
        .items()
        .chunks(size)
        .map(|chunk| sequence.rebuild(chunk.to_vec()))
        .collect())
}

/// Slide a window across a sequence
///
/// Examples:
/// <mash>
///   sliding 2 [1, 2, 3] # [[1, 2], [2, 3]]
/// </mash>
#[builtin(namespace = "collections", infer = super::inference::infer_windows)]
pub fn sliding(
    args: &Args,
    #[param("Size of window")] size: i64,
    #[param("Sequence to slide windows along")] sequence: Sequence,
) -> R<Vec<Value>> {
    let size = positive_size(args, size)?;
    let items = sequence.items();
    // As Scala's `sliding`: a sequence shorter than the window gives one partial window
    if !items.is_empty() && items.len() < size {
        return Ok(vec![sequence.rebuild(items)]);
    }
    Ok(items
        .windows(size)
        .map(|window| sequence.rebuild(window.to_vec()))
        .collect())
}

fn positive_size(args: &Args, size: i64) -> R<usize> {
    if size <= 0 {
        return Err(args.invalid("size", "size must be positive"));
    }
    Ok(size as usize)
}
