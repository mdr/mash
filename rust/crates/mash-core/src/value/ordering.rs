//! The total order on values used for sorting, `min`/`max` and comparison operators (Scala
//! `MashValueOrdering`).

use std::cmp::Ordering;

use jiff::tz::TimeZone;

use super::{ClassRef, Value};

/// Compare two values. Values of the same kind compare naturally (numbers numerically with NaN last, strings
/// by code point, lists lexicographically, objects field by field); otherwise by kind. A date compares with
/// a date-time as midnight at the start of that day in `tz`. Terminates on cyclic values.
pub fn compare(a: &Value, b: &Value, tz: &TimeZone) -> Ordering {
    Comparer { tz, stack: vec![] }.compare(a, b)
}

/// Lexicographic comparison of two sequences of values.
pub fn compare_lists(xs: &[Value], ys: &[Value], tz: &TimeZone) -> Ordering {
    Comparer { tz, stack: vec![] }.compare_slices(xs, ys)
}

struct Comparer<'a> {
    tz: &'a TimeZone,
    /// Pairs of containers being compared further up the stack
    stack: Vec<(usize, usize)>,
}

impl Comparer<'_> {
    fn compare(&mut self, a: &Value, b: &Value) -> Ordering {
        match (a, b) {
            (Value::Class(c1), Value::Class(c2)) => compare_classes(&c1.full_name(), &c2.full_name()),
            (Value::Bool(x), Value::Bool(y)) => x.cmp(y),
            (Value::Number(x, _), Value::Number(y, _)) => x.total_cmp(*y),
            (Value::Str(x), Value::Str(y)) => x.as_str().cmp(y.as_str()),
            (Value::Date(x), Value::Date(y)) => x.cmp(y),
            (Value::DateTime(x), Value::DateTime(y)) => x.cmp(y),
            (Value::Date(d), Value::DateTime(t)) => self.start_of_day(d.0).cmp(&t.0),
            (Value::DateTime(t), Value::Date(d)) => t.0.cmp(&self.start_of_day(d.0)),
            (Value::List(x), Value::List(y)) => {
                let key = (x.addr(), y.addr());
                if x.ptr_eq(y) || self.stack.contains(&key) {
                    return Ordering::Equal;
                }
                self.stack.push(key);
                let result = self.compare_slices(&x.snapshot(), &y.snapshot());
                self.stack.pop();
                result
            }
            (Value::Object(x), Value::Object(y)) => {
                let key = (x.addr(), y.addr());
                if x.ptr_eq(y) || self.stack.contains(&key) {
                    return Ordering::Equal;
                }
                self.stack.push(key);
                let (xs, ys) = (x.entries(), y.entries());
                let mut result = Ordering::Equal;
                for ((k1, v1), (k2, v2)) in xs.iter().zip(&ys) {
                    result = self.compare(k1, k2).then_with(|| self.compare(v1, v2));
                    if result != Ordering::Equal {
                        break;
                    }
                }
                self.stack.pop();
                result.then(xs.len().cmp(&ys.len()))
            }
            _ => compare_classes(&primary_class_name(a), &primary_class_name(b)),
        }
    }

    fn compare_slices(&mut self, xs: &[Value], ys: &[Value]) -> Ordering {
        for (x, y) in xs.iter().zip(ys) {
            match self.compare(x, y) {
                Ordering::Equal => continue,
                other => return other,
            }
        }
        xs.len().cmp(&ys.len())
    }

    fn start_of_day(&self, date: jiff::civil::Date) -> jiff::Timestamp {
        date.to_zoned(self.tz.clone())
            .map(|z| z.timestamp())
            .unwrap_or(jiff::Timestamp::MIN)
    }
}

/// The fully-qualified name of the class of a value (Scala `primaryClass`), as far as ordering needs it.
fn primary_class_name(v: &Value) -> String {
    match v {
        Value::Null => "core.Null".into(),
        Value::Unit => "core.Unit".into(),
        Value::Bool(_) => "core.Boolean".into(),
        Value::Number(..) => "core.Number".into(),
        Value::Str(_) => "core.String".into(),
        Value::Date(_) => "time.Date".into(),
        Value::DateTime(_) => "time.DateTime".into(),
        Value::Function(_) | Value::Builtin(_) => "core.Function".into(),
        Value::BoundMethod(_) => "core.BoundMethod".into(),
        Value::List(_) => "collections.List".into(),
        Value::Object(o) => o
            .class()
            .as_ref()
            .map_or_else(|| "core.Object".into(), ClassRef::full_name),
        Value::Class(_) => "core.Class".into(),
    }
}

/// Scala `MashClassOrdering`: the core classes in a fixed order, then other classes by name.
fn compare_classes(c1: &str, c2: &str) -> Ordering {
    const SPECIAL: [&str; 13] = [
        "core.Null",
        "core.Unit",
        "core.Boolean",
        "core.Number",
        "core.String",
        "time.Date",
        "time.DateTime",
        "core.Function",
        "core.BoundMethod",
        "collections.List",
        "core.Object",
        "core.Class",
        "core.Any",
    ];
    let position = |c: &str| SPECIAL.iter().position(|s| *s == c);
    match (position(c1), position(c2)) {
        (Some(p1), Some(p2)) => p1.cmp(&p2),
        (Some(_), None) => Ordering::Less,
        (None, Some(_)) => Ordering::Greater,
        (None, None) => c1.cmp(c2),
    }
}
