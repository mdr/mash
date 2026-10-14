//! Methods of numbers (Scala `NumberClass`), and the `Bytes` tag class.

use mash_macros::builtin_method;

use crate::builtins::time::TimeUnit;
use crate::eval::{Ctx, MashError, R};
use crate::function::{Args, BuiltinMethod};
use crate::value::{BuiltinClass, ClassRef, Number, Tag, Value};

use super::ANY_CLASS;

pub(super) const METHODS: &[&dyn BuiltinMethod] = &[
    &AbsMethod,
    &BytesMethod,
    &CeilMethod,
    &DaysMethod,
    &FloorMethod,
    &GbMethod,
    &HoursMethod,
    &KbMethod,
    &MbMethod,
    &MillisecondsMethod,
    &MinutesMethod,
    &ModMethod,
    &MonthsMethod,
    &NegateMethod,
    &PowerMethod,
    &RoundMethod,
    &SecondsMethod,
    &TagMethod,
    &TimesMethod,
    &ToIntMethod,
    &ToMethod,
    &UntaggedMethod,
    &UntilMethod,
    &WeeksMethod,
];

/// A units type: number of bytes
pub static BYTES_CLASS: BuiltinClass = BuiltinClass {
    summary: "Tag class representing a quantity of bytes",
    ..BuiltinClass::new("core", "Bytes", Some(&ANY_CLASS))
};

fn number(target: &Value) -> R<(Number, Option<Tag>)> {
    match target {
        Value::Number(n, tag) => Ok((*n, *tag)),
        _ => Err(MashError::new(format!(
            "Expected a Number, but was a {}",
            target.type_name()
        ))),
    }
}

/// The target as an integer, for methods that need one
fn integer(target: &Value) -> R<i64> {
    let (n, _) = number(target)?;
    n.as_i64()
        .ok_or_else(|| MashError::new(format!("Can only call this method on an integer, but was {n}")))
}

/// Returns this number raised to the power of the given exponent
#[builtin_method(returns = crate::infer::Type::NUMBER)]
fn power(target: &Value, #[param("Exponent")] exponent: f64) -> R<f64> {
    Ok(number(target)?.0.as_f64().powf(exponent))
}

/// A list of the integers from `start` towards `end` in steps of `step`, including `end` if `inclusive`
fn range(start: i64, end: i64, step: i64, inclusive: bool) -> R<Value> {
    if step == 0 {
        return Err(MashError::new("Invalid argument 'step'. Step cannot be zero"));
    }
    let mut items = vec![];
    let mut i = start;
    loop {
        let more = match (step > 0, inclusive) {
            (true, true) => i <= end,
            (true, false) => i < end,
            (false, true) => i >= end,
            (false, false) => i > end,
        };
        if !more {
            break;
        }
        items.push(Value::num(i as f64));
        match i.checked_add(step) {
            Some(next) => i = next,
            None => break,
        }
    }
    Ok(Value::list(items))
}

/// Return a list of values from this number to the given end value (inclusive)
///
/// Examples:
/// <mash>
///   1.to 5           # [1, 2, 3, 4, 5]
///   1.to 10 --step=2 # [1, 3, 5, 7, 9]
/// </mash>
#[builtin_method(returns = crate::infer::Type::seq(crate::infer::Type::NUMBER))]
fn to(
    target: &Value,
    #[param("Final number in sequence (inclusive)")] end: i64,
    #[param("The number to increase by for each step of the sequence (default 1)", default = 1.0)] step: i64,
) -> R<Value> {
    range(integer(target)?, end, step, true)
}

/// Return a list of values from this number to the given end value (exclusive)
///
/// Examples:
/// <mash>
///   1.until 5           # [1, 2, 3, 4]
///   1.until 10 --step=2 # [1, 3, 5, 7, 9]
/// </mash>
#[builtin_method(returns = crate::infer::Type::seq(crate::infer::Type::NUMBER))]
fn until(
    target: &Value,
    #[param("Final number in sequence (exclusive)")] end: i64,
    #[param("The number to increase by for each step of the sequence (default 1)", default = 1.0)] step: i64,
) -> R<Value> {
    range(integer(target)?, end, step, false)
}

/// Run the given argument this amount of times
///
/// Returns the results of each run, as a list.
#[builtin_method(infer = super::infer_rules::times)]
fn times(ctx: &mut Ctx, target: &Value, #[param("Code to execute", lazy)] block: Value) -> R<Value> {
    let iterations = integer(target)?;
    let mut results = vec![];
    for _ in 0..iterations.max(0) {
        results.push(ctx.call_values(&block, vec![])?);
    }
    Ok(Value::list(results))
}

/// Convert number to an integer (rounding towards zero)
#[builtin_method(infer = super::infer_rules::same_as_target)]
fn to_int(target: &Value) -> R<Value> {
    let (n, tag) = number(target)?;
    Ok(Value::Number(Number::new(n.as_f64().trunc()), tag))
}

/// This number with `f` applied, keeping its tag (so `2.6.seconds.round` is still seconds)
fn map_number(target: &Value, f: impl FnOnce(f64) -> f64) -> R<Value> {
    let (n, tag) = number(target)?;
    Ok(Value::Number(Number::new(f(n.as_f64())), tag))
}

/// Round this number to the nearest integer, or to a number of decimal places
///
/// A half rounds away from zero: 2.5 rounds to 3, and -2.5 to -3.
///
/// Examples:
/// <mash>
///   2.5.round        # 3
///   3.14159.round 2  # 3.14
///   1234.round -2    # 1200
/// </mash>
#[builtin_method(infer = super::infer_rules::same_as_target)]
fn round(
    target: &Value,
    args: &Args,
    #[param(
        "Number of decimal places to keep (default 0)",
        optional,
        description = "Must be an integer. A negative number rounds to tens, hundreds, …"
    )]
    places: Option<f64>,
) -> R<Value> {
    let places = match places {
        Some(p) if p.fract() != 0.0 || !p.is_finite() => {
            return Err(args.invalid("places", &format!("Must be an integer, but was {}", Number::new(p))));
        }
        Some(p) => p.clamp(-308.0, 308.0) as i32,
        None => 0,
    };
    map_number(target, |n| {
        if places == 0 {
            n.round()
        } else {
            let scale = 10f64.powi(places);
            let rounded = (n * scale).round() / scale;
            if rounded.is_finite() { rounded } else { n }
        }
    })
}

/// The largest integer no greater than this number
///
/// Examples:
/// <mash>
///   2.7.floor     # 2
///   (-2.5).floor  # -3
/// </mash>
#[builtin_method(infer = super::infer_rules::same_as_target)]
fn floor(target: &Value) -> R<Value> {
    map_number(target, f64::floor)
}

/// The smallest integer no less than this number
///
/// Examples:
/// <mash>
///   2.1.ceil     # 3
///   (-2.5).ceil  # -2
/// </mash>
#[builtin_method(infer = super::infer_rules::same_as_target)]
fn ceil(target: &Value) -> R<Value> {
    map_number(target, f64::ceil)
}

/// The absolute value of this number: without its sign
#[builtin_method(infer = super::infer_rules::same_as_target)]
fn abs(target: &Value) -> R<Value> {
    map_number(target, f64::abs)
}

/// The remainder of dividing this number by another
///
/// The result has the sign of the divisor (as Python's `%` and Nushell's `mod`), so it wraps around: `(-1).mod 7`
/// is 6. Dividing by zero is an error.
///
/// Examples:
/// <mash>
///   7.mod 3       # 1
///   (-7).mod 3    # 2
///   7.5.mod 2     # 1.5
/// </mash>
#[builtin_method(name = "mod", infer = super::infer_rules::same_as_target)]
fn mod_(target: &Value, args: &Args, #[param("Number to divide by")] divisor: f64) -> R<Value> {
    if divisor == 0.0 {
        return Err(args.invalid("divisor", "Cannot divide by zero"));
    }
    map_number(target, |n| {
        let remainder = n % divisor;
        if remainder != 0.0 && (remainder < 0.0) != (divisor < 0.0) {
            remainder + divisor
        } else {
            remainder
        }
    })
}

/// Negate this number
#[builtin_method(infer = super::infer_rules::same_as_target)]
fn negate(target: &Value) -> R<Value> {
    let (n, tag) = number(target)?;
    Ok(Value::Number(Number::new(-n.as_f64()), tag))
}

/// This number's tagged type if any, else null
#[builtin_method(returns = crate::infer::Type::CLASS)]
fn tag(target: &Value) -> R<Value> {
    Ok(match number(target)?.1 {
        Some(tag) => Value::Class(ClassRef::Builtin(tag)),
        None => Value::Null,
    })
}

/// This number without any tag class
#[builtin_method(returns = crate::infer::Type::NUMBER)]
fn untagged(target: &Value) -> R<Value> {
    Ok(Value::Number(number(target)?.0, None))
}

fn scaled_bytes(target: &Value, factor: f64) -> R<Value> {
    Ok(Value::Number(
        Number::new(number(target)?.0.as_f64() * factor),
        Some(&BYTES_CLASS),
    ))
}

/// This number of bytes
#[builtin_method(returns = crate::infer::Type::tagged_number(&super::BYTES_CLASS))]
fn bytes(target: &Value) -> R<Value> {
    scaled_bytes(target, 1.0)
}

/// This number of kilobytes
#[builtin_method(returns = crate::infer::Type::tagged_number(&super::BYTES_CLASS))]
fn kb(target: &Value) -> R<Value> {
    scaled_bytes(target, 1024.0)
}

/// This number of megabytes
#[builtin_method(returns = crate::infer::Type::tagged_number(&super::BYTES_CLASS))]
fn mb(target: &Value) -> R<Value> {
    scaled_bytes(target, 1024.0 * 1024.0)
}

/// This number of gigabytes
#[builtin_method(returns = crate::infer::Type::tagged_number(&super::BYTES_CLASS))]
fn gb(target: &Value) -> R<Value> {
    scaled_bytes(target, 1024.0 * 1024.0 * 1024.0)
}

fn with_unit(target: &Value, unit: TimeUnit) -> R<Value> {
    Ok(Value::Number(number(target)?.0, Some(unit.class())))
}

/// This number of milliseconds
#[builtin_method(aliases = ["millisecond"], returns = crate::infer::Type::tagged_number(&crate::builtins::time::MILLISECONDS_CLASS))]
fn milliseconds(target: &Value) -> R<Value> {
    with_unit(target, TimeUnit::Milliseconds)
}

/// This number of seconds
#[builtin_method(aliases = ["second"], returns = crate::infer::Type::tagged_number(&crate::builtins::time::SECONDS_CLASS))]
fn seconds(target: &Value) -> R<Value> {
    with_unit(target, TimeUnit::Seconds)
}

/// This number of minutes
#[builtin_method(aliases = ["minute"], returns = crate::infer::Type::tagged_number(&crate::builtins::time::MINUTES_CLASS))]
fn minutes(target: &Value) -> R<Value> {
    with_unit(target, TimeUnit::Minutes)
}

/// This number of hours
#[builtin_method(aliases = ["hour"], returns = crate::infer::Type::tagged_number(&crate::builtins::time::HOURS_CLASS))]
fn hours(target: &Value) -> R<Value> {
    with_unit(target, TimeUnit::Hours)
}

/// This number of days
#[builtin_method(aliases = ["day"], returns = crate::infer::Type::tagged_number(&crate::builtins::time::DAYS_CLASS))]
fn days(target: &Value) -> R<Value> {
    with_unit(target, TimeUnit::Days)
}

/// This number of weeks
#[builtin_method(aliases = ["week"], returns = crate::infer::Type::tagged_number(&crate::builtins::time::WEEKS_CLASS))]
fn weeks(target: &Value) -> R<Value> {
    with_unit(target, TimeUnit::Weeks)
}

/// This number of months
#[builtin_method(aliases = ["month"], returns = crate::infer::Type::tagged_number(&crate::builtins::time::MONTHS_CLASS))]
fn months(target: &Value) -> R<Value> {
    with_unit(target, TimeUnit::Months)
}
