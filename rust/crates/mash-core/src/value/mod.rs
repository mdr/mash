//! Runtime values.
//!
//! Heap values (lists, objects, functions, scopes, user classes, bound methods) are reference counted handles,
//! with a cycle collector for the cycles Mash creates routinely (see [`gc`]). The rules every container
//! follows:
//!
//! * Single-threaded: nothing is `Send`.
//! * No `RefCell` borrow ever escapes a method. Containers hand out cloned `Value`s or snapshots, so calling
//!   back into the interpreter while "iterating" is always safe.
//! * Values are never dropped while a container borrow is held (old values are moved out first), so a
//!   collection triggered by a drop never finds a container mutably borrowed.
//! * Object keys are frozen on insert: a list or object used as a key is deep-copied, so mutating the
//!   original can't corrupt the map.

mod class;
mod function;
pub mod gc;
mod list;
mod number;
mod object;
mod ordering;
mod scope;
mod string;
mod trace_map;

use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};

use dumpster::{Trace, TraceWith, Visitor};
use jiff::Timestamp;
use jiff::civil::Date;

pub use class::{BuiltinClass, Class, ClassRef, Field, UserMethod};
pub use function::{BoundMethod, Builtin, Function, FunctionKind, MethodRef};
pub use list::List;
pub use number::Number;
pub use object::Object;
pub use ordering::{compare, compare_lists};
pub use scope::{Binding, Scope, ScopeKind};
pub use string::Str;

/// The class a string or number is tagged with (e.g. `os.Path`, `time.Seconds`). Tags are always builtin
/// classes.
pub type Tag = &'static BuiltinClass;

/// A Mash value. Cheap to clone: heap variants are reference-counted handles.
#[derive(Clone, Trace)]
pub enum Value {
    Null,
    Unit,
    Bool(bool),
    Number(Number, Option<Tag>),
    Str(Str),
    List(List),
    Object(Object),
    /// A user-defined function or lambda, or another function value that carries data
    Function(Function),
    /// A function implemented in Rust
    Builtin(Builtin),
    BoundMethod(BoundMethod),
    Class(ClassRef),
    DateTime(DateTime),
    Date(LocalDate),
}

/// An instant in time (`time.DateTime`), shown in the system time zone.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct DateTime(pub Timestamp);

/// A calendar date with no time zone (`time.Date`).
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct LocalDate(pub Date);

// SAFETY: plain data, no `Gc`s.
#[allow(unsafe_code)]
unsafe impl<V: Visitor> TraceWith<V> for DateTime {
    fn accept(&self, _: &mut V) -> Result<(), ()> {
        Ok(())
    }
}

// SAFETY: plain data, no `Gc`s.
#[allow(unsafe_code)]
unsafe impl<V: Visitor> TraceWith<V> for LocalDate {
    fn accept(&self, _: &mut V) -> Result<(), ()> {
        Ok(())
    }
}

// SAFETY: plain data, no `Gc`s.
#[allow(unsafe_code)]
unsafe impl<V: Visitor> TraceWith<V> for Number {
    fn accept(&self, _: &mut V) -> Result<(), ()> {
        Ok(())
    }
}

// SAFETY: tags are builtin classes ('static data), so there are no `Gc`s.
#[allow(unsafe_code)]
unsafe impl<V: Visitor> TraceWith<V> for Str {
    fn accept(&self, _: &mut V) -> Result<(), ()> {
        Ok(())
    }
}

impl Value {
    pub fn str(s: &str) -> Value {
        Value::Str(Str::new(s))
    }

    pub fn num(n: impl Into<Number>) -> Value {
        Value::Number(n.into(), None)
    }

    pub fn list(items: Vec<Value>) -> Value {
        Value::List(List::new(items))
    }

    /// Whether this value holds a `Gc` (and so must be traced).
    pub(crate) fn holds_gc(&self) -> bool {
        match self {
            Value::List(_) | Value::Object(_) | Value::Function(_) | Value::BoundMethod(_) => true,
            Value::Class(c) => matches!(c, ClassRef::User(_)),
            Value::Null
            | Value::Unit
            | Value::Bool(_)
            | Value::Number(..)
            | Value::Str(_)
            | Value::Builtin(_)
            | Value::DateTime(_)
            | Value::Date(_) => false,
        }
    }

    pub fn is_null(&self) -> bool {
        matches!(self, Value::Null)
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Null => false,
            Value::Bool(b) => *b,
            Value::Number(n, _) => n.as_f64() != 0.0,
            Value::Str(s) => !s.is_empty(),
            Value::List(l) => !l.is_empty(),
            Value::Object(o) => !o.is_empty(),
            _ => true,
        }
    }

    /// The tag class of a tagged string or number
    pub fn tag(&self) -> Option<Tag> {
        match self {
            Value::Number(_, tag) => *tag,
            Value::Str(s) => s.tag(),
            _ => None,
        }
    }

    /// The name of the value's class (Scala `typeName`), e.g. `Number`
    pub fn type_name(&self) -> String {
        match self {
            Value::Null => "Null".into(),
            Value::Unit => "Unit".into(),
            Value::Bool(_) => "Boolean".into(),
            Value::Number(..) => "Number".into(),
            Value::Str(_) => "String".into(),
            Value::List(_) => "List".into(),
            Value::Object(o) => o.class().map_or_else(|| "Object".into(), |c| c.name()),
            Value::Function(_) | Value::Builtin(_) => "Function".into(),
            Value::BoundMethod(_) => "BoundMethod".into(),
            Value::Class(_) => "Class".into(),
            Value::DateTime(_) => "DateTime".into(),
            Value::Date(_) => "Date".into(),
        }
    }

    pub fn as_str(&self) -> Option<&str> {
        match self {
            Value::Str(s) => Some(s.as_str()),
            _ => None,
        }
    }

    pub fn as_number(&self) -> Option<Number> {
        match self {
            Value::Number(n, _) => Some(*n),
            _ => None,
        }
    }

    /// Mash's `==` operator: structural, with IEEE number comparison (so `NaN != NaN`, unlike key
    /// equality). Tags are ignored. Terminates on cyclic values.
    pub fn mash_equals(&self, other: &Value) -> bool {
        structural_eq(self, other, false, &mut Vec::new())
    }

    /// Deep-copy lists and objects (preserving sharing and cycles), so the result can be used as an object
    /// key that nobody else can mutate. Other values are returned as they are.
    pub(crate) fn freeze_as_key(self) -> Value {
        match self {
            Value::List(_) | Value::Object(_) => deep_copy(&self, &mut HashMap::new()),
            other => other,
        }
    }
}

impl From<&str> for Value {
    fn from(s: &str) -> Self {
        Value::str(s)
    }
}

impl From<String> for Value {
    fn from(s: String) -> Self {
        Value::Str(Str::new(s))
    }
}

impl From<Str> for Value {
    fn from(s: Str) -> Self {
        Value::Str(s)
    }
}

impl From<f64> for Value {
    fn from(n: f64) -> Self {
        Value::num(n)
    }
}

impl From<Number> for Value {
    fn from(n: Number) -> Self {
        Value::Number(n, None)
    }
}

impl From<i64> for Value {
    fn from(n: i64) -> Self {
        Value::num(n)
    }
}

impl From<usize> for Value {
    fn from(n: usize) -> Self {
        Value::num(n)
    }
}

impl From<List> for Value {
    fn from(xs: List) -> Self {
        Value::List(xs)
    }
}

impl From<Vec<Value>> for Value {
    fn from(items: Vec<Value>) -> Self {
        Value::list(items)
    }
}

impl From<Object> for Value {
    fn from(o: Object) -> Self {
        Value::Object(o)
    }
}

impl From<ClassRef> for Value {
    fn from(c: ClassRef) -> Self {
        Value::Class(c)
    }
}

impl From<()> for Value {
    fn from(_: ()) -> Self {
        Value::Unit
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Value::Bool(b)
    }
}

fn deep_copy(v: &Value, seen: &mut HashMap<usize, Value>) -> Value {
    match v {
        Value::List(l) => {
            if let Some(copy) = seen.get(&l.addr()) {
                return copy.clone();
            }
            let copy = List::new(Vec::new());
            seen.insert(l.addr(), Value::List(copy.clone()));
            let items = l.snapshot().iter().map(|x| deep_copy(x, seen)).collect();
            copy.replace_all(items);
            Value::List(copy)
        }
        Value::Object(o) => {
            if let Some(copy) = seen.get(&o.addr()) {
                return copy.clone();
            }
            let copy = Object::with_class(o.class());
            seen.insert(o.addr(), Value::Object(copy.clone()));
            for (k, val) in o.entries() {
                // Keys are already frozen
                copy.__set_unfrozen(k, deep_copy(&val, seen));
            }
            Value::Object(copy)
        }
        other => other.clone(),
    }
}

/// Structural equality that terminates on cyclic values: a pair of containers already being compared further
/// up the stack is assumed equal.
fn structural_eq(a: &Value, b: &Value, key_semantics: bool, stack: &mut Vec<(usize, usize)>) -> bool {
    match (a, b) {
        (Value::Null, Value::Null) | (Value::Unit, Value::Unit) => true,
        (Value::Bool(x), Value::Bool(y)) => x == y,
        (Value::Number(x, _), Value::Number(y, _)) => {
            if key_semantics {
                x.key_bits() == y.key_bits()
            } else {
                x.as_f64() == y.as_f64()
            }
        }
        (Value::Str(x), Value::Str(y)) => x.as_str() == y.as_str(),
        (Value::List(x), Value::List(y)) => {
            if x.ptr_eq(y) || stack.contains(&(x.addr(), y.addr())) {
                return true;
            }
            let (xs, ys) = (x.snapshot(), y.snapshot());
            if xs.len() != ys.len() {
                return false;
            }
            stack.push((x.addr(), y.addr()));
            let eq = xs
                .iter()
                .zip(&ys)
                .all(|(p, q)| structural_eq(p, q, key_semantics, stack));
            stack.pop();
            eq
        }
        (Value::Object(x), Value::Object(y)) => {
            if x.ptr_eq(y) || stack.contains(&(x.addr(), y.addr())) {
                return true;
            }
            let same_class = match (x.class(), y.class()) {
                (None, None) => true,
                (Some(c), Some(d)) => c.ptr_eq(&d),
                _ => false,
            };
            let (xs, ys) = (x.entries(), y.entries());
            if !same_class || xs.len() != ys.len() {
                return false;
            }
            stack.push((x.addr(), y.addr()));
            // Field order is not significant
            let eq = xs.iter().all(|(k, v)| match y.get(k) {
                Some(w) => structural_eq(v, &w, key_semantics, stack),
                None => false,
            });
            stack.pop();
            eq
        }
        (Value::Function(x), Value::Function(y)) => x.ptr_eq(y),
        (Value::Builtin(x), Value::Builtin(y)) => x.ptr_eq(*y),
        (Value::BoundMethod(x), Value::BoundMethod(y)) => x.same_as(y),
        (Value::Class(x), Value::Class(y)) => x.ptr_eq(y),
        (Value::DateTime(x), Value::DateTime(y)) => x == y,
        (Value::Date(x), Value::Date(y)) => x == y,
        _ => false,
    }
}

/// Key equality: structural, but NaN equals NaN and -0.0 equals 0.0. Tags are ignored.
impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        structural_eq(self, other, true, &mut Vec::new())
    }
}

impl Eq for Value {}

/// Hashing is consistent with key equality, and *shallow* for containers (the hashes of their direct
/// elements, but not of nested containers), so it can't loop on cyclic values.
impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        hash_value(self, state, true);
    }
}

const STR_DISCRIMINANT: u8 = 4;

/// How a string key hashes; lets objects look up string keys without allocating.
pub(crate) fn hash_str_key<H: Hasher>(s: &str, state: &mut H) {
    STR_DISCRIMINANT.hash(state);
    s.hash(state);
}

fn hash_value<H: Hasher>(v: &Value, state: &mut H, recurse: bool) {
    match v {
        Value::Null => 0u8.hash(state),
        Value::Unit => 1u8.hash(state),
        Value::Bool(b) => {
            2u8.hash(state);
            b.hash(state)
        }
        Value::Number(n, _) => {
            3u8.hash(state);
            n.key_bits().hash(state)
        }
        Value::Str(s) => hash_str_key(s.as_str(), state),
        Value::List(l) => {
            5u8.hash(state);
            if recurse {
                l.with_items(|items| {
                    items.len().hash(state);
                    for item in items {
                        hash_value(item, state, false);
                    }
                });
            }
        }
        Value::Object(o) => {
            6u8.hash(state);
            if recurse {
                o.with_fields(|fields| {
                    fields.len().hash(state);
                    // Order-independent, to match order-insensitive equality
                    let mut acc: u64 = 0;
                    for (k, val) in fields {
                        let mut h = std::hash::DefaultHasher::new();
                        hash_value(k, &mut h, false);
                        hash_value(val, &mut h, false);
                        acc = acc.wrapping_add(h.finish());
                    }
                    acc.hash(state);
                });
            }
        }
        Value::Function(f) => {
            7u8.hash(state);
            f.addr().hash(state)
        }
        Value::Builtin(b) => {
            8u8.hash(state);
            b.addr().hash(state)
        }
        Value::BoundMethod(m) => {
            9u8.hash(state);
            m.name().hash(state);
            hash_value(m.target(), state, false);
        }
        Value::Class(c) => {
            10u8.hash(state);
            c.addr().hash(state)
        }
        Value::DateTime(t) => {
            11u8.hash(state);
            t.hash(state)
        }
        Value::Date(d) => {
            12u8.hash(state);
            d.hash(state)
        }
    }
}

/// A debugging representation (Mash's own `toString` lives in the evaluator). Terminates on cyclic values.
impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_value(self, f, &mut Vec::new())
    }
}

fn write_value(v: &Value, f: &mut fmt::Formatter<'_>, stack: &mut Vec<usize>) -> fmt::Result {
    match v {
        Value::Null => f.write_str("null"),
        Value::Unit => f.write_str("()"),
        Value::Bool(b) => write!(f, "{b}"),
        Value::Number(n, None) => write!(f, "{n}"),
        Value::Number(n, Some(tag)) => write!(f, "{n}<{}>", tag.full_name()),
        Value::Str(s) => match s.tag() {
            None => write!(f, "{s:?}"),
            Some(tag) => write!(f, "{s:?}<{}>", tag.full_name()),
        },
        Value::List(l) => {
            if stack.contains(&l.addr()) {
                return f.write_str("[…]");
            }
            stack.push(l.addr());
            f.write_str("[")?;
            for (i, item) in l.snapshot().iter().enumerate() {
                if i > 0 {
                    f.write_str(", ")?;
                }
                write_value(item, f, stack)?;
            }
            stack.pop();
            f.write_str("]")
        }
        Value::Object(o) => {
            if stack.contains(&o.addr()) {
                return f.write_str("{…}");
            }
            stack.push(o.addr());
            if let Some(class) = o.class() {
                write!(f, "{} ", class.full_name())?;
            }
            f.write_str("{")?;
            for (i, (k, val)) in o.entries().iter().enumerate() {
                if i > 0 {
                    f.write_str(", ")?;
                }
                write_value(k, f, stack)?;
                f.write_str(": ")?;
                write_value(val, f, stack)?;
            }
            stack.pop();
            f.write_str("}")
        }
        Value::Function(func) => write!(f, "{func:?}"),
        Value::Builtin(b) => write!(f, "{b:?}"),
        Value::BoundMethod(m) => write!(f, "{m:?}"),
        Value::Class(c) => write!(f, "{c:?}"),
        Value::DateTime(t) => write!(f, "{}", t.0),
        Value::Date(d) => write!(f, "{}", d.0),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn value_is_small() {
        // Values are copied around a lot, and evaluator stack frames hold many of them
        assert!(std::mem::size_of::<Value>() <= 32, "{}", std::mem::size_of::<Value>());
    }
}
