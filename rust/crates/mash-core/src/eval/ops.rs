//! Binary operators (Scala `BinaryOperatorEvaluator`).

use std::cmp::Ordering;

use mash_syntax::TextRange;

use crate::ast::BinOp;
use crate::builtins::time::{self, TimeUnit};
use crate::value::{List, Object, Str, Value, compare};

use super::{Ctx, MashError, R};

impl Ctx {
    pub(crate) fn binary_op(&mut self, op: BinOp, left: &Value, right: &Value, range: Option<TextRange>) -> R<Value> {
        binary_op(self, op, left, right).map_err(|e| e.or_at(self.loc(range)))
    }
}

/// Apply a binary operator to two values. `and`/`or` are handled here without short-circuiting (the evaluator
/// short-circuits them itself).
pub fn binary_op(ctx: &mut Ctx, op: BinOp, left: &Value, right: &Value) -> R<Value> {
    let incompatible = |verb: &str| {
        MashError::new(format!(
            "Could not {verb}, incompatible operands {} and {}",
            left.type_name(),
            right.type_name()
        ))
    };
    let compare_with = |ctx: &Ctx, accept: fn(Ordering) -> bool| -> R<Value> {
        Ok(if left.is_null() || right.is_null() {
            Value::Bool(false)
        } else if let Some((a, b, _)) = in_common_unit(left, right)? {
            Value::Bool(accept(a.total_cmp(&b)))
        } else {
            Value::Bool(accept(compare(left, right, &ctx.time_zone())))
        })
    };
    Ok(match op {
        BinOp::LessThan => compare_with(ctx, Ordering::is_lt)?,
        BinOp::LessThanEquals => compare_with(ctx, Ordering::is_le)?,
        BinOp::GreaterThan => compare_with(ctx, Ordering::is_gt)?,
        BinOp::GreaterThanEquals => compare_with(ctx, Ordering::is_ge)?,
        BinOp::And => {
            if left.is_truthy() {
                right.clone()
            } else {
                left.clone()
            }
        }
        BinOp::Or => {
            if left.is_truthy() {
                left.clone()
            } else {
                right.clone()
            }
        }
        BinOp::Equals => Value::Bool(equal(left, right)?),
        BinOp::NotEquals => Value::Bool(!equal(left, right)?),
        BinOp::Plus => match (left, right) {
            // A date-time plus or minus a number of a unit of time, e.g. `now + 3.days`
            (Value::DateTime(t), amount) | (amount, Value::DateTime(t)) if TimeUnit::of(amount).is_some() => {
                let (n, unit) = TimeUnit::of(amount).expect("checked by the guard");
                time::add_to_date_time(ctx, t.0, n, unit)?
            }
            // A date plus or minus a number of days, weeks or months, e.g. `today + 1.days`
            (Value::Date(d), amount) | (amount, Value::Date(d)) if TimeUnit::of(amount).is_some() => {
                let (n, unit) = TimeUnit::of(amount).expect("checked by the guard");
                time::add_to_date(d.0, n, unit)?
            }
            (Value::List(xs), Value::List(ys)) => {
                let mut items = xs.snapshot();
                items.extend(ys.snapshot());
                Value::list(items)
            }
            // A string plus anything is a string, keeping the string's tag
            (Value::Str(s), _) => Value::Str(s.with_text(format!("{}{}", s.as_str(), ctx.stringify(right)?))),
            (_, Value::Str(s)) => Value::Str(s.with_text(format!("{}{}", ctx.stringify(left)?, s.as_str()))),
            (Value::Number(a, tag_a), Value::Number(b, tag_b)) => match in_common_unit(left, right)? {
                Some((a, b, unit)) => Value::Number((a + b).into(), Some(unit.class())),
                None => Value::Number((a.as_f64() + b.as_f64()).into(), tag_a.or(*tag_b)),
            },
            (Value::Object(a), Value::Object(b)) => {
                let merged = Object::with_class(b.class().or_else(|| a.class()));
                for (k, v) in a.entries().into_iter().chain(b.entries()) {
                    merged.set(k, v);
                }
                Value::Object(merged)
            }
            _ => return Err(incompatible("add")),
        },
        BinOp::Minus => match (left, right) {
            (Value::DateTime(t), amount) if TimeUnit::of(amount).is_some() => {
                let (n, unit) = TimeUnit::of(amount).expect("checked by the guard");
                time::add_to_date_time(ctx, t.0, -n, unit)?
            }
            (Value::Date(d), amount) if TimeUnit::of(amount).is_some() => {
                let (n, unit) = TimeUnit::of(amount).expect("checked by the guard");
                time::add_to_date(d.0, -n, unit)?
            }
            // The difference between two dates, in days
            (Value::Date(a), Value::Date(b)) => time::days_between(a.0, b.0),
            // The difference between two date-times, in milliseconds
            (Value::DateTime(a), Value::DateTime(b)) => time::difference(a.0, b.0),
            (Value::Number(a, tag_a), Value::Number(b, tag_b)) => match in_common_unit(left, right)? {
                Some((a, b, unit)) => Value::Number((a - b).into(), Some(unit.class())),
                None => Value::Number((a.as_f64() - b.as_f64()).into(), tag_a.or(*tag_b)),
            },
            (Value::List(xs), Value::List(ys)) => Value::List(list_difference(xs, ys)),
            (Value::Object(o), Value::List(fields)) => Value::Object(without_fields(o, &fields.snapshot())),
            (Value::Object(o), field) => Value::Object(without_fields(o, std::slice::from_ref(field))),
            _ => return Err(incompatible("subtract")),
        },
        BinOp::Multiply => match (left, right) {
            (Value::Str(s), Value::Number(n, _)) | (Value::Number(n, _), Value::Str(s)) if is_int(n.as_f64()) => {
                Value::Str(repeat_str(s, n.as_f64()))
            }
            (Value::List(xs), Value::Number(n, _)) | (Value::Number(n, _), Value::List(xs)) if is_int(n.as_f64()) => {
                let items = xs.snapshot();
                let times = n.as_f64().max(0.0) as usize;
                Value::list(items.iter().cloned().cycle().take(items.len() * times).collect())
            }
            (Value::Number(a, tag_a), Value::Number(b, tag_b)) => {
                Value::Number((a.as_f64() * b.as_f64()).into(), tag_a.or(*tag_b))
            }
            _ => return Err(incompatible("multiply")),
        },
        BinOp::Divide => match (left, right) {
            (Value::Number(a, tag_a), Value::Number(b, tag_b)) => {
                Value::Number((a.as_f64() / b.as_f64()).into(), tag_a.or(*tag_b))
            }
            _ => return Err(incompatible("divide")),
        },
    })
}

/// `==`: as values are equal, but amounts of time in different units by their lengths (`1.days == 24.hours`), as
/// they are ordered. Months are never equal to amounts of another unit.
fn equal(left: &Value, right: &Value) -> R<bool> {
    Ok(match in_common_unit(left, right) {
        Ok(Some((a, b, _))) => a == b,
        Ok(None) => left.mash_equals(right),
        Err(_) => false,
    })
}

/// Two amounts of different units of time, in the smaller unit (so that `1.days + 3.hours` is `27.hours`, and
/// `3.days < 2.hours` compares lengths): None unless both are times, in different units. Months vary in length,
/// so they don't combine with other units.
fn in_common_unit(left: &Value, right: &Value) -> R<Option<(f64, f64, TimeUnit)>> {
    let (Some((a, unit_a)), Some((b, unit_b))) = (TimeUnit::of(left), TimeUnit::of(right)) else {
        return Ok(None);
    };
    if unit_a == unit_b {
        return Ok(None);
    }
    let (Some(millis_a), Some(millis_b)) = (unit_a.fixed_millis(), unit_b.fixed_millis()) else {
        return Err(MashError::new(format!(
            "Cannot combine {} with {}: months vary in length",
            unit_a.plural_name(),
            unit_b.plural_name()
        )));
    };
    Ok(Some(if millis_a < millis_b {
        (a, b * (millis_b / millis_a), unit_a)
    } else {
        (a * (millis_a / millis_b), b, unit_b)
    }))
}

/// A whole number in the 32-bit range (Scala `isValidInt`)
fn is_int(n: f64) -> bool {
    n.fract() == 0.0 && n >= i32::MIN as f64 && n <= i32::MAX as f64
}

fn repeat_str(s: &Str, n: f64) -> Str {
    s.with_text(s.as_str().repeat(n.max(0.0) as usize))
}

/// Multiset difference: each element of `ys` removes one occurrence from `xs`
fn list_difference(xs: &List, ys: &List) -> List {
    let mut remaining = ys.snapshot();
    let mut result = vec![];
    for x in xs.snapshot() {
        match remaining.iter().position(|y| *y == x) {
            Some(i) => {
                remaining.swap_remove(i);
            }
            None => result.push(x),
        }
    }
    List::new(result)
}

/// The object without the given fields (and without a class, as in Scala)
fn without_fields(object: &Object, fields: &[Value]) -> Object {
    let result = Object::new();
    for (k, v) in object.entries() {
        if !fields.contains(&k) {
            result.set(k, v);
        }
    }
    result
}
