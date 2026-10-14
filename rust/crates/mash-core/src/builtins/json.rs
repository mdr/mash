//! The `json` namespace: converting between Mash values and JSON text, JSON files, and HTTP requests that
//! send and receive JSON (through the [`Platform`](crate::platform::Platform)).

use mash_macros::builtin;

use super::http::{
    Body, Encoding, Target, basic_auth_param, body_param, body_text, cookies_param, file_param, headers_param,
    insecure_param, query_params_param, send, url_param,
};
use super::os::paths::{PathArg, fs, io_error};
use super::os::{complete_files, read_text};
use crate::eval::{Ctx, MashError, R};
use crate::function::{Args, BuiltinFunction};
use crate::infer::Type;
use crate::value::{Number, Object, Str, Value};

pub(crate) const FUNCTIONS: &[&dyn BuiltinFunction] = &[
    &FromStringFunction,
    &PrettyPrintFunction,
    &ReadFunction,
    &WriteFunction,
    &GetFunction,
    &PostFunction,
    &PutFunction,
    &PatchFunction,
    &DeleteFunction,
];

/// Read the given string and parse it as JSON
#[builtin(namespace = "json", returns = crate::infer::Type::ANY)]
pub fn from_string(#[param("String to parse as JSON")] string: Str) -> R<Value> {
    parse_json(string.as_str())
}

/// Convert the given value to a pretty-printed JSON string
#[builtin(namespace = "json", returns = crate::infer::Type::STRING)]
pub fn pretty_print(
    ctx: &mut Ctx,
    #[param("Pretty print to a single line (default false)", short = 'c', boolean_flag)] compact: bool,
    #[param("Value to convert to a JSON string")] value: Value,
) -> R<String> {
    json_string(ctx, &value, compact)
}

/// A value as JSON text (Scala `PrettyPrintFunction.asJsonString`)
pub(crate) fn json_string(ctx: &mut Ctx, value: &Value, compact: bool) -> R<String> {
    let json = to_json(ctx, value)?;
    let text = if compact {
        serde_json::to_string(&json)
    } else {
        serde_json::to_string_pretty(&json)
    };
    text.map_err(|e| MashError::new(format!("Could not write JSON: {e}")))
}

/// Read the given file and parse it as JSON
#[builtin(namespace = "json", returns = Type::ANY, completion = complete_files)]
pub fn read(ctx: &mut Ctx, #[param("File from which to read JSON")] file: PathArg) -> R<Value> {
    parse_json(&read_text(ctx, &file.0)?)
}

/// Write the given value as JSON to the given file
#[builtin(namespace = "json", returns = Type::UNIT, completion = complete_files)]
pub fn write(
    ctx: &mut Ctx,
    #[param("File to write JSON to")] file: PathArg,
    #[param("Value to write")] value: Value,
) -> R<()> {
    let text = json_string(ctx, &value, false)?;
    ctx.platform()
        .write_file(fs(&file.0), text.as_bytes(), false)
        .map_err(|e| io_error("write", &file.0, &e))
}

/// Make a JSON request, and parse the response body as JSON
fn json_request(ctx: &mut Ctx, args: &Args, method: &str, target: Target, body: Option<Body>) -> R<Value> {
    let response = send(ctx, args, method, target, body, Encoding::Json)?;
    parse_json(&body_text(&response))
}

/// Make an HTTP GET request and return the response as JSON
#[builtin(namespace = "json", returns = Type::OBJECT)]
#[allow(clippy::too_many_arguments)]
pub fn get(
    ctx: &mut Ctx,
    args: &Args,
    #[param(def = url_param)] url: Str,
    #[param(def = basic_auth_param)] basic_auth: Option<Value>,
    #[param(def = headers_param)] headers: Option<Value>,
    #[param(def = cookies_param)] cookies: Option<Value>,
    #[param(def = query_params_param)] query_params: Object,
    #[param(def = insecure_param)] insecure: bool,
) -> R<Value> {
    let target = Target {
        url,
        basic_auth,
        headers,
        cookies,
        query_params,
        insecure,
        timeout: None,
    };
    json_request(ctx, args, "GET", target, None)
}

/// POST some JSON to an HTTP endpoint, and parse the result as JSON
#[builtin(namespace = "json", returns = Type::OBJECT)]
#[allow(clippy::too_many_arguments)]
pub fn post(
    ctx: &mut Ctx,
    args: &Args,
    #[param(def = url_param)] url: Str,
    #[param(def = body_param)] body: Value,
    #[param(def = file_param)] file: bool,
    #[param(def = basic_auth_param)] basic_auth: Option<Value>,
    #[param(def = headers_param)] headers: Option<Value>,
    #[param(def = cookies_param)] cookies: Option<Value>,
    #[param(def = query_params_param)] query_params: Object,
    #[param(def = insecure_param)] insecure: bool,
) -> R<Value> {
    let target = Target {
        url,
        basic_auth,
        headers,
        cookies,
        query_params,
        insecure,
        timeout: None,
    };
    let body = Body {
        value: body,
        from_file: file,
    };
    json_request(ctx, args, "POST", target, Some(body))
}

/// PUT some JSON to an HTTP endpoint, and parse the result as JSON
#[builtin(namespace = "json", returns = Type::OBJECT)]
#[allow(clippy::too_many_arguments)]
pub fn put(
    ctx: &mut Ctx,
    args: &Args,
    #[param(def = url_param)] url: Str,
    #[param(def = body_param)] body: Value,
    #[param(def = file_param)] file: bool,
    #[param(def = basic_auth_param)] basic_auth: Option<Value>,
    #[param(def = headers_param)] headers: Option<Value>,
    #[param(def = cookies_param)] cookies: Option<Value>,
    #[param(def = query_params_param)] query_params: Object,
    #[param(def = insecure_param)] insecure: bool,
) -> R<Value> {
    let target = Target {
        url,
        basic_auth,
        headers,
        cookies,
        query_params,
        insecure,
        timeout: None,
    };
    let body = Body {
        value: body,
        from_file: file,
    };
    json_request(ctx, args, "PUT", target, Some(body))
}

/// PATCH some JSON to an HTTP endpoint, and parse the result as JSON
#[builtin(namespace = "json", returns = Type::OBJECT)]
#[allow(clippy::too_many_arguments)]
pub fn patch(
    ctx: &mut Ctx,
    args: &Args,
    #[param(def = url_param)] url: Str,
    #[param(def = body_param)] body: Value,
    #[param(def = file_param)] file: bool,
    #[param(def = basic_auth_param)] basic_auth: Option<Value>,
    #[param(def = headers_param)] headers: Option<Value>,
    #[param(def = cookies_param)] cookies: Option<Value>,
    #[param(def = query_params_param)] query_params: Object,
    #[param(def = insecure_param)] insecure: bool,
) -> R<Value> {
    let target = Target {
        url,
        basic_auth,
        headers,
        cookies,
        query_params,
        insecure,
        timeout: None,
    };
    let body = Body {
        value: body,
        from_file: file,
    };
    json_request(ctx, args, "PATCH", target, Some(body))
}

/// Make an HTTP DELETE request and return the response as JSON
#[builtin(namespace = "json", returns = Type::OBJECT)]
#[allow(clippy::too_many_arguments)]
pub fn delete(
    ctx: &mut Ctx,
    args: &Args,
    #[param(def = url_param)] url: Str,
    #[param(def = basic_auth_param)] basic_auth: Option<Value>,
    #[param(def = headers_param)] headers: Option<Value>,
    #[param(def = cookies_param)] cookies: Option<Value>,
    #[param(def = query_params_param)] query_params: Object,
    #[param(def = insecure_param)] insecure: bool,
) -> R<Value> {
    let target = Target {
        url,
        basic_auth,
        headers,
        cookies,
        query_params,
        insecure,
        timeout: None,
    };
    json_request(ctx, args, "DELETE", target, None)
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
