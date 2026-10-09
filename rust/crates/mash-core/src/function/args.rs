//! Bound arguments as builtins see them, with typed extraction.

use super::{Argument, Bound, Params};
use crate::builtins::core::CHARACTER_CLASS;
use crate::eval::{Ctx, Location, MashError, R};
use crate::value::{ClassRef, List, Number, Object, Str, Value};

/// The arguments of a call to a builtin, bound to its parameters.
#[derive(Debug)]
pub struct Args<'a> {
    params: &'a Params,
    bound: Bound,
}

impl<'a> Args<'a> {
    pub(crate) fn new(params: &'a Params, bound: Bound) -> Self {
        Args { params, bound }
    }

    fn index(&self, name: &str) -> usize {
        self.params
            .index_of(name)
            .unwrap_or_else(|| panic!("no parameter named '{name}'"))
    }

    /// The value of a parameter. Null for an omitted optional parameter.
    pub fn value(&self, name: &str) -> &Value {
        self.bound.values[self.index(name)].as_ref().unwrap_or(&Value::Null)
    }

    /// Whether an optional parameter was given
    pub fn is_provided(&self, name: &str) -> bool {
        self.bound.values[self.index(name)].is_some()
    }

    /// The value of a parameter, converted to `T`
    pub fn get<T: FromValue>(&self, name: &str) -> R<T> {
        let value = self.value(name);
        T::from_value(value).map_err(|message| self.invalid(name, &message))
    }

    /// The value of an optional parameter, converted to `T`; None if it wasn't given
    pub fn get_opt<T: FromValue>(&self, name: &str) -> R<Option<T>> {
        match &self.bound.values[self.index(name)] {
            Some(value) => T::from_value(value)
                .map(Some)
                .map_err(|message| self.invalid(name, &message)),
            None => Ok(None),
        }
    }

    /// Where the argument for a parameter came from
    pub fn location(&self, name: &str) -> Option<&Location> {
        self.bound.locations[self.index(name)].as_ref()
    }

    /// An "Invalid argument" error, located at the argument
    pub fn invalid(&self, name: &str, message: &str) -> MashError {
        MashError::argument(
            format!("Invalid argument '{name}'. {message}"),
            self.location(name).cloned(),
        )
    }

    /// The arguments given to an `all_args` parameter
    pub fn all_args(&self) -> &[Argument] {
        &self.bound.all_args
    }
}

/// Conversion from a Mash value to a Rust type, for builtin arguments.
pub trait FromValue: Sized {
    /// Convert, or explain what was expected (e.g. "Must be a String, but was a Number")
    fn from_value(value: &Value) -> Result<Self, String>;
}

fn must_be(what: &str, value: &Value) -> String {
    format!("Must be {what}, but was a {}", value.type_name())
}

impl FromValue for Value {
    fn from_value(value: &Value) -> Result<Self, String> {
        Ok(value.clone())
    }
}

/// Truthiness
impl FromValue for bool {
    fn from_value(value: &Value) -> Result<Self, String> {
        Ok(value.is_truthy())
    }
}

impl FromValue for Number {
    fn from_value(value: &Value) -> Result<Self, String> {
        value.as_number().ok_or_else(|| must_be("a Number", value))
    }
}

impl FromValue for f64 {
    fn from_value(value: &Value) -> Result<Self, String> {
        Number::from_value(value).map(Number::as_f64)
    }
}

impl FromValue for i64 {
    fn from_value(value: &Value) -> Result<Self, String> {
        match value {
            Value::Number(n, _) => n.as_i64().ok_or_else(|| format!("Must be an integer, but was '{n}'")),
            _ => Err(must_be("an integer", value)),
        }
    }
}

/// A non-negative integer
impl FromValue for usize {
    fn from_value(value: &Value) -> Result<Self, String> {
        let n = i64::from_value(value)?;
        usize::try_from(n).map_err(|_| format!("Must be a positive integer, but was {n}"))
    }
}

impl FromValue for Str {
    fn from_value(value: &Value) -> Result<Self, String> {
        match value {
            Value::Str(s) => Ok(s.clone()),
            _ => Err(must_be("a String", value)),
        }
    }
}

impl FromValue for String {
    fn from_value(value: &Value) -> Result<Self, String> {
        Str::from_value(value).map(|s| s.as_str().to_string())
    }
}

impl FromValue for List {
    fn from_value(value: &Value) -> Result<Self, String> {
        match value {
            Value::List(xs) => Ok(xs.clone()),
            _ => Err(must_be("a List", value)),
        }
    }
}

impl FromValue for Object {
    fn from_value(value: &Value) -> Result<Self, String> {
        match value {
            Value::Object(o) => Ok(o.clone()),
            _ => Err(must_be("an Object", value)),
        }
    }
}

impl FromValue for ClassRef {
    fn from_value(value: &Value) -> Result<Self, String> {
        match value {
            Value::Class(c) => Ok(c.clone()),
            _ => Err(must_be("a class", value)),
        }
    }
}

/// Something callable as a function: a function, bound method, or a string (`'name' x` means `x.name`).
#[derive(Debug, Clone)]
pub struct Callable(pub Value);

impl FromValue for Callable {
    fn from_value(value: &Value) -> Result<Self, String> {
        match value {
            Value::Str(_) | Value::Function(_) | Value::Builtin(_) | Value::BoundMethod(_) => {
                Ok(Callable(value.clone()))
            }
            _ => Err(must_be("a function", value)),
        }
    }
}

impl Callable {
    pub fn call(&self, ctx: &mut Ctx, args: Vec<Value>) -> R<Value> {
        ctx.call_values(&self.0, args)
    }

    pub fn call1(&self, ctx: &mut Ctx, arg: Value) -> R<Value> {
        ctx.call_values(&self.0, vec![arg])
    }

    /// Whether the function can take at least `count` positional arguments
    pub fn accepts_positional(&self, ctx: &Ctx, count: usize) -> bool {
        ctx.params_of(&self.0)
            .is_some_and(|params| params.allows_at_least_positional(count))
    }
}

/// A list, or a string as a sequence of characters (Scala `validateSequence`).
#[derive(Debug, Clone)]
pub enum Sequence {
    List(List),
    Str(Str),
}

impl FromValue for Sequence {
    fn from_value(value: &Value) -> Result<Self, String> {
        match value {
            Value::List(xs) => Ok(Sequence::List(xs.clone())),
            Value::Str(s) => Ok(Sequence::Str(s.clone())),
            _ => Err(must_be("a sequence", value)),
        }
    }
}

impl Sequence {
    /// The elements; a string's characters are `Character`-tagged strings
    pub fn items(&self) -> Vec<Value> {
        match self {
            Sequence::List(xs) => xs.snapshot(),
            Sequence::Str(s) => s
                .as_str()
                .chars()
                .map(|c| Value::Str(Str::tagged(c.to_string(), &CHARACTER_CLASS)))
                .collect(),
        }
    }

    /// Turn processed items back into a sequence of the same kind: for a string, a string (keeping its tag) if
    /// every item is a string, otherwise a list.
    pub fn rebuild(&self, items: Vec<Value>) -> Value {
        match self {
            Sequence::Str(s) if items.iter().all(|v| matches!(v, Value::Str(_))) => {
                let text: String = items.iter().filter_map(Value::as_str).collect();
                Value::Str(s.with_text(text))
            }
            _ => Value::list(items),
        }
    }
}

impl Args<'_> {
    /// These arguments plus `value` for the parameter at `index` of `params` (which are this call's
    /// parameters with that one added): how a function exposed as a method is called with its target
    pub(crate) fn with_extra<'p>(&self, params: &'p Params, index: usize, value: Value) -> Args<'p> {
        let mut values = self.bound.values.clone();
        let mut locations = self.bound.locations.clone();
        values.insert(index, Some(value));
        locations.insert(index, None);
        Args::new(
            params,
            Bound {
                values,
                locations,
                all_args: self.bound.all_args.clone(),
            },
        )
    }
}
