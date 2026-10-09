//! The `type` and `mash` namespaces: type hints, and Mash about itself (`version`, `eval`, `expressionFor`).
//! `mash.source` and `mash.reload` need files and the REPL, and come later.

use mash_macros::builtin;
use mash_syntax::escapes::escape_chars;
use mash_syntax::lexer::is_legal_identifier;

use crate::eval::{Ctx, R, render};
use crate::function::BuiltinFunction;
use crate::value::{Str, Value};

pub(crate) const FUNCTIONS: &[&dyn BuiltinFunction] =
    &[&HintFunction, &VersionFunction, &ExpressionForFunction, &EvalFunction];

/// Hint that the argument is a list
///
/// Returns the item unchanged; the hint is used by type inference.
#[builtin(namespace = "type")]
pub fn hint(#[param("Hint")] hint: Value, #[param("Item to apply a type hint to")] item: Value) -> R<Value> {
    let _ = hint;
    Ok(item)
}

/// Return the version of Mash
#[builtin(namespace = "mash")]
pub fn version() -> R<String> {
    Ok(env!("CARGO_PKG_VERSION").to_string())
}

/// Convert the given value into a Mash expression that evaluates to the same value
#[builtin(namespace = "mash")]
pub fn expression_for(#[param("Value to turn into a Mash expression")] value: Value) -> R<Value> {
    Ok(value_to_expression(&value).map_or(Value::Null, |s| Value::Str(Str::new(s))))
}

/// Execute the given string as a Mash expression
#[builtin(namespace = "mash")]
pub fn eval(ctx: &mut Ctx, #[param("Mash code")] mash: Str) -> R<Value> {
    ctx.eval_source("eval", mash.as_str())
}

/// Mash source that evaluates to `value`, if there is any (Scala `ValueToExpression`): for null, booleans,
/// numbers, strings, date-times, and lists and objects of those. None for functions, classes and cyclic
/// values.
pub fn value_to_expression(value: &Value) -> Option<String> {
    to_expression(value, &mut vec![])
}

fn to_expression(value: &Value, visiting: &mut Vec<usize>) -> Option<String> {
    match value {
        Value::Null | Value::Bool(_) | Value::Number(..) => Some(render(value)),
        Value::Str(s) => Some(format!("\"{}\"", escape_chars(s.as_str()))),
        Value::DateTime(t) => Some(format!("time.fromMillisSinceEpoch {}", t.0.as_millisecond())),
        Value::List(xs) => {
            if visiting.contains(&xs.addr()) {
                return None;
            }
            visiting.push(xs.addr());
            let items: Option<Vec<String>> = xs.snapshot().iter().map(|x| to_expression(x, visiting)).collect();
            visiting.pop();
            Some(format!("[{}]", items?.join(", ")))
        }
        Value::Object(o) => {
            if visiting.contains(&o.addr()) {
                return None;
            }
            visiting.push(o.addr());
            let fields: Option<Vec<String>> = o
                .entries()
                .iter()
                .map(|(field, value)| {
                    let field_expression = to_expression(field, visiting)?;
                    let value_expression = to_expression(value, visiting)?;
                    let field_expression = match field {
                        Value::Str(s) if is_legal_identifier(s.as_str()) => s.as_str().to_string(),
                        Value::Null | Value::Bool(_) | Value::Number(..) | Value::Str(_) => field_expression,
                        _ => format!("({field_expression})"),
                    };
                    Some(format!("{field_expression}: {value_expression}"))
                })
                .collect();
            visiting.pop();
            Some(format!("{{ {} }}", fields?.join(", ")))
        }
        _ => None,
    }
}
