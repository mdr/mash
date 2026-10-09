//! How a single value is shown: in a table cell, or on its own (Scala `FieldRenderer` and `BytesPrinter`).

use mash_core::builtins::core::BYTES_CLASS;
use mash_core::builtins::os::{PERMISSIONS_CLASS, PERMISSIONS_SECTION_CLASS};
use mash_core::builtins::time::{MILLISECONDS_CLASS, SECONDS_CLASS};
use mash_core::eval::{Ctx, MashError, MemberLookup, R, render};
use mash_core::value::{Number, Object, Value};

use super::ViewConfig;

/// `value` as text. In a table cell (`in_cell`), booleans are `✓`/`✗`, lists have no brackets, and tabs and
/// newlines become spaces.
///
/// Numbers tagged as bytes are human-readable (`1.5KB`), milliseconds and seconds get a unit suffix, and
/// date-times are relative ("3 days ago") or local ISO-style times, depending on `config.fuzzy_time`.
/// Permissions are `rwxr-x---` (Scala `PermissionsPrinter`).
pub fn render_field(ctx: &mut Ctx, value: &Value, in_cell: bool, config: &ViewConfig) -> R<String> {
    Ok(match value {
        Value::Bool(true) if in_cell => "✓".to_string(),
        Value::Bool(false) if in_cell => "✗".to_string(),
        Value::Number(n, Some(tag)) if std::ptr::eq(*tag, &BYTES_CLASS) => human_readable_bytes(n.as_f64()),
        Value::Number(n, Some(tag)) if std::ptr::eq(*tag, &MILLISECONDS_CLASS) => format!("{n}ms"),
        Value::Number(n, Some(tag)) if std::ptr::eq(*tag, &SECONDS_CLASS) => format!("{n}s"),
        Value::Number(n, _) => n.to_string(),
        Value::DateTime(_) if config.fuzzy_time => {
            let fuzzy = ctx
                .lookup_member(value, "fuzzy", MemberLookup::DEFAULT)
                .ok_or_else(|| MashError::new("No fuzzy method"))?;
            let fuzzy = ctx.invoke_nullary(fuzzy, None)?;
            ctx.stringify(&fuzzy)?
        }
        Value::Object(o) if has_class(o, &PERMISSIONS_CLASS) => ["owner", "group", "others"]
            .iter()
            .map(|section| match o.get_str(section) {
                Some(Value::Object(section)) => permissions_section(&section),
                _ => "---".to_string(),
            })
            .collect(),
        Value::Object(o) if has_class(o, &PERMISSIONS_SECTION_CLASS) => permissions_section(o),
        Value::DateTime(t) => t.0.to_zoned(ctx.time_zone()).strftime("%Y-%m-%d %H:%M:%S").to_string(),
        Value::List(xs) => {
            let items = xs
                .snapshot()
                .iter()
                .map(|x| render_field(ctx, x, in_cell, config))
                .collect::<R<Vec<_>>>()?;
            if in_cell {
                items.join(", ")
            } else {
                format!("[{}]", items.join(", "))
            }
        }
        _ => {
            // A user-defined `toString` that fails falls back to the default form (Scala `safeStringify`)
            let s = ctx.stringify(value).unwrap_or_else(|_| render(value));
            if in_cell { replace_problematic_chars(&s) } else { s }
        }
    })
}

fn has_class(object: &Object, class: &'static mash_core::value::BuiltinClass) -> bool {
    object.class().is_some_and(|c| c.is_builtin(class))
}

/// `rwx`, with `-` for each permission not granted
fn permissions_section(section: &Object) -> String {
    [("canRead", 'r'), ("canWrite", 'w'), ("canExecute", 'x')]
        .iter()
        .map(|(field, c)| {
            if section.get_str(field).is_some_and(|v| v.is_truthy()) {
                *c
            } else {
                '-'
            }
        })
        .collect()
}

/// Tabs, newlines, carriage returns and backspaces become spaces, so that text stays in its cell
pub(crate) fn replace_problematic_chars(s: &str) -> String {
    s.chars()
        .map(|c| {
            if matches!(c, '\t' | '\n' | '\r' | '\u{8}') {
                ' '
            } else {
                c
            }
        })
        .collect()
}

/// A number of bytes in the largest unit (B, KB, MB, GB) that keeps it at least 1, to three significant
/// figures (as ICU formats them, with grouping): `1.5KB`, `1,020B`
pub fn human_readable_bytes(n: f64) -> String {
    const KB: f64 = 1024.0;
    const MB: f64 = 1024.0 * KB;
    const GB: f64 = 1024.0 * MB;
    let (amount, unit) = if n < KB {
        (n, "B")
    } else if n < MB {
        (n / KB, "KB")
    } else if n < GB {
        (n / MB, "MB")
    } else {
        (n / GB, "GB")
    };
    format!("{}{unit}", three_significant_figures(amount))
}

fn three_significant_figures(n: f64) -> String {
    if !n.is_finite() {
        return Number::new(n).to_string();
    }
    if n == 0.0 {
        return "0".to_string();
    }
    // Round in decimal (Rust rounds exactly-representable ties to even, as ICU does)
    let scientific = format!("{n:.2e}");
    let rounded: f64 = scientific.parse().expect("valid float");
    let exponent: i32 = scientific
        .split_once('e')
        .and_then(|(_, e)| e.parse().ok())
        .expect("valid exponent");
    let decimals = usize::try_from(2 - exponent).unwrap_or(0);
    let mut fixed = format!("{:.*}", decimals, rounded.abs());
    if fixed.contains('.') {
        fixed = fixed.trim_end_matches('0').trim_end_matches('.').to_string();
    }
    let (whole, fraction) = fixed
        .split_once('.')
        .map_or((fixed.as_str(), None), |(w, f)| (w, Some(f)));
    let mut result = String::new();
    if rounded < 0.0 {
        result.push('-');
    }
    for (i, c) in whole.chars().enumerate() {
        if i > 0 && (whole.len() - i) % 3 == 0 {
            result.push(',');
        }
        result.push(c);
    }
    if let Some(fraction) = fraction {
        result.push('.');
        result.push_str(fraction);
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Expected values from Scala Mash
    #[test]
    fn bytes() {
        assert_eq!(human_readable_bytes(0.0), "0B");
        assert_eq!(human_readable_bytes(999.0), "999B");
        assert_eq!(human_readable_bytes(1000.0), "1,000B");
        assert_eq!(human_readable_bytes(1023.0), "1,020B");
        assert_eq!(human_readable_bytes(1536.0), "1.5KB");
        assert_eq!(human_readable_bytes(1024.0 * 1024.0 * 2.5678), "2.57MB");
        assert_eq!(human_readable_bytes(1024.0 * 1024.0 * 1024.0 * 2048.0), "2,050GB");
        assert_eq!(human_readable_bytes(12.5), "12.5B");
        assert_eq!(human_readable_bytes(1023.5), "1,020B");
        assert_eq!(human_readable_bytes(0.125), "0.125B");
    }
}
