//! Mash's default string representation of values (Scala's `toString` on values, used by the `toString`
//! method).

use jiff::Timestamp;
use jiff::tz::TimeZone;

use crate::value::{FunctionKind, Value};

/// The default string form of a value: strings as they are, numbers without a trailing `.0`, containers as
/// `[1, 2]` and `{ a: 1 }` (with their contents rendered the same way, so strings inside are unquoted).
/// Date-times are in UTC; `render_in` gives them in a time zone. Terminates on cyclic values.
pub fn render(value: &Value) -> String {
    render_in(value, &TimeZone::UTC)
}

/// `render`, with date-times in time zone `tz`, e.g. 2026-10-09T13:00:00.25+01:00
pub fn render_in(value: &Value, tz: &TimeZone) -> String {
    let mut out = String::new();
    write(&mut out, value, tz, &mut vec![]);
    out
}

/// A date-time in ISO 8601 form, in `tz` with its offset, to the millisecond (trailing zeros dropped)
pub(crate) fn date_time_string(t: Timestamp, tz: &TimeZone) -> String {
    let millis = Timestamp::from_millisecond(t.as_millisecond()).unwrap_or(t);
    millis
        .to_zoned(tz.clone())
        .strftime("%Y-%m-%dT%H:%M:%S%.f%:z")
        .to_string()
}

fn write(out: &mut String, value: &Value, tz: &TimeZone, stack: &mut Vec<usize>) {
    match value {
        Value::Null => out.push_str("null"),
        Value::Unit => {}
        Value::Bool(b) => out.push_str(if *b { "true" } else { "false" }),
        Value::Number(n, _) => out.push_str(&n.to_string()),
        Value::Str(s) => out.push_str(s.as_str()),
        Value::List(xs) => {
            let id = xs.addr();
            if stack.contains(&id) {
                return out.push_str("[…]");
            }
            stack.push(id);
            out.push('[');
            for (i, item) in xs.snapshot().iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                write(out, item, tz, stack);
            }
            out.push(']');
            stack.pop();
        }
        Value::Object(o) => {
            let id = o.addr();
            if stack.contains(&id) {
                return out.push_str("{…}");
            }
            let entries = o.entries();
            if entries.is_empty() {
                return out.push_str("{}");
            }
            stack.push(id);
            out.push_str("{ ");
            for (i, (k, v)) in entries.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                write(out, k, tz, stack);
                out.push_str(": ");
                write(out, v, tz, stack);
            }
            out.push_str(" }");
            stack.pop();
        }
        Value::Function(f) => match f.kind() {
            FunctionKind::Closure { def, .. } => {
                let name = def.name.as_deref().unwrap_or("anonymous");
                match &def.namespace {
                    Some(ns) if !ns.is_empty() => out.push_str(&format!("{}.{name}", ns.join("."))),
                    _ => out.push_str(name),
                }
            }
            FunctionKind::Constructor(_) => out.push_str("new"),
            FunctionKind::SystemCommand(_) | FunctionKind::Suspended(_) => out.push_str("anonymous"),
        },
        Value::Builtin(b) => out.push_str(&b.0.full_name()),
        Value::BoundMethod(m) => out.push_str(&m.full_name()),
        Value::Class(c) => out.push_str(&c.full_name()),
        Value::DateTime(t) => out.push_str(&date_time_string(t.0, tz)),
        Value::Date(d) => out.push_str(&d.0.to_string()),
    }
}
