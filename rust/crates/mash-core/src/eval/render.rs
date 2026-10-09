//! Mash's default string representation of values (Scala's `toString` on values, used by the `toString`
//! method).

use crate::value::{FunctionKind, Value};

/// The default string form of a value: strings as they are, numbers without a trailing `.0`, containers as
/// `[1, 2]` and `{ a: 1 }` (with their contents rendered the same way, so strings inside are unquoted).
/// Terminates on cyclic values.
pub fn render(value: &Value) -> String {
    let mut out = String::new();
    write(&mut out, value, &mut vec![]);
    out
}

fn write(out: &mut String, value: &Value, stack: &mut Vec<usize>) {
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
                write(out, item, stack);
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
                write(out, k, stack);
                out.push_str(": ");
                write(out, v, stack);
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
        Value::DateTime(t) => out.push_str(&t.0.to_string()),
        Value::Date(d) => out.push_str(&d.0.to_string()),
    }
}
