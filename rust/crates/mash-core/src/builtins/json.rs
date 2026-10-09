//! The `json` namespace: converting between Mash values and JSON text. Reading and writing files and the HTTP
//! functions (`json.read`, `json.get`, …) need the OS and live in `mash-os`.

use mash_macros::builtin;

use crate::eval::{Ctx, MashError, R};
use crate::function::BuiltinFunction;
use crate::value::{Number, Object, Str, Value};

pub(crate) const FUNCTIONS: &[&dyn BuiltinFunction] = &[&FromStringFunction, &PrettyPrintFunction];

/// Read the given string and parse it as JSON
#[builtin(namespace = "json")]
pub fn from_string(#[param("String to parse as JSON")] string: Str) -> R<Value> {
    parse_json(string.as_str())
}

/// Convert the given value to a pretty-printed JSON string
#[builtin(namespace = "json")]
pub fn pretty_print(
    ctx: &mut Ctx,
    #[param("Pretty print to a single line (default false)", short = 'c', boolean_flag)] compact: bool,
    #[param("Value to convert to a JSON string")] value: Value,
) -> R<String> {
    let json = to_json(ctx, &value)?;
    let text = if compact {
        serde_json::to_string(&json)
    } else {
        serde_json::to_string_pretty(&json)
    };
    text.map_err(|e| MashError::new(format!("Could not write JSON: {e}")))
}

/// Parse JSON text into Mash values. Numbers become (floating point) Mash numbers; object field order is
/// kept.
pub(crate) fn parse_json(text: &str) -> R<Value> {
    let json: serde_json::Value =
        serde_json::from_str(text).map_err(|e| MashError::new(format!("Invalid JSON: {e}")))?;
    Ok(from_json(&json))
}

pub(crate) fn from_json(json: &serde_json::Value) -> Value {
    match json {
        serde_json::Value::Null => Value::Null,
        serde_json::Value::Bool(b) => Value::Bool(*b),
        serde_json::Value::Number(n) => Value::num(n.as_f64().unwrap_or(f64::NAN)),
        serde_json::Value::String(s) => Value::Str(Str::new(s.as_str())),
        serde_json::Value::Array(items) => Value::list(items.iter().map(from_json).collect()),
        serde_json::Value::Object(fields) => {
            let object = Object::new();
            for (k, v) in fields {
                object.set_str(k, from_json(v));
            }
            Value::Object(object)
        }
    }
}

/// Convert a Mash value to JSON (Scala `PrettyPrintFunction.asJson`): whole numbers are written as integers;
/// object fields and values that aren't JSON types are converted to strings with `toString`. NaN, infinities
/// and cyclic values can't be written.
pub(crate) fn to_json(ctx: &mut Ctx, value: &Value) -> R<serde_json::Value> {
    to_json_inner(ctx, value, &mut vec![])
}

fn to_json_inner(ctx: &mut Ctx, value: &Value, stack: &mut Vec<usize>) -> R<serde_json::Value> {
    let cyclic = || MashError::new("Cannot convert a cyclic value to JSON");
    Ok(match value {
        Value::Null => serde_json::Value::Null,
        Value::Bool(b) => serde_json::Value::Bool(*b),
        Value::Number(n, _) => number_to_json(*n)?,
        Value::Str(s) => serde_json::Value::String(s.as_str().to_string()),
        Value::List(xs) => {
            if stack.contains(&xs.addr()) {
                return Err(cyclic());
            }
            stack.push(xs.addr());
            let mut items = vec![];
            for item in xs.snapshot() {
                items.push(to_json_inner(ctx, &item, stack)?);
            }
            stack.pop();
            serde_json::Value::Array(items)
        }
        Value::Object(o) => {
            if stack.contains(&o.addr()) {
                return Err(cyclic());
            }
            stack.push(o.addr());
            let mut fields = serde_json::Map::new();
            for (k, v) in o.entries() {
                let key = ctx.stringify(&k)?;
                fields.insert(key, to_json_inner(ctx, &v, stack)?);
            }
            stack.pop();
            serde_json::Value::Object(fields)
        }
        other => serde_json::Value::String(ctx.stringify(other)?),
    })
}

fn number_to_json(n: Number) -> R<serde_json::Value> {
    let f = n.as_f64();
    if let Some(i) = n.as_i64() {
        return Ok(serde_json::Value::from(i));
    }
    serde_json::Number::from_f64(f)
        .map(serde_json::Value::Number)
        .ok_or_else(|| MashError::new(format!("Cannot convert {n} to JSON")))
}
