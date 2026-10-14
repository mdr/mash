//! The `csv` namespace: reading CSV as objects, and writing objects as CSV files, in the format of Apache
//! Commons CSV 1.4's `CSVFormat.DEFAULT` (which Scala Mash used).

use mash_macros::builtin;

use super::os::paths::{PathArg, fs, io_error};
use super::os::{complete_files, read_text};
use crate::eval::{Ctx, MashError, R};
use crate::function::{Args, BuiltinFunction};
use crate::infer::Type;
use crate::value::{Object, Str, Value};

pub(crate) const FUNCTIONS: &[&dyn BuiltinFunction] = &[&FromStringFunction, &ReadFunction, &WriteFunction];

/// Parse the given string as CSV
///
/// The first line is a header naming the fields; each line after it becomes an object with those fields, in
/// order. Every value is a string, as written (an empty value is the empty string): use `toNumber` to convert.
/// Values may be quoted, to contain commas, quotes (doubled) or line breaks.
#[builtin(namespace = "csv", returns = Type::seq(Type::OBJECT))]
pub fn from_string(#[param("String to parse as CSV")] string: Str) -> R<Value> {
    parse_csv(string.as_str())
}

/// Read the given CSV file
///
/// The first line is a header naming the fields; each line after it becomes an object with those fields, in
/// order. Every value is a string, as written (an empty value is the empty string): use `toNumber` to convert.
/// Values may be quoted, to contain commas, quotes (doubled) or line breaks.
#[builtin(namespace = "csv", returns = Type::seq(Type::OBJECT), completion = complete_files)]
pub fn read(ctx: &mut Ctx, #[param("File from which to read CSV records")] file: PathArg) -> R<Value> {
    parse_csv(&read_text(ctx, &file.0)?)
}

/// Objects from CSV with a header line, every value a string (as Python's `csv.DictReader` and PowerShell's
/// `Import-Csv`); a line with a different number of values from the header is an error
fn parse_csv(text: &str) -> R<Value> {
    let invalid = |e: csv::Error| {
        MashError::new(match e.kind() {
            csv::ErrorKind::UnequalLengths {
                pos, expected_len, len, ..
            } => format!(
                "Invalid CSV: line {} has {len} value{}, but the header has {expected_len}",
                pos.as_ref().map_or(0, csv::Position::line),
                if *len == 1 { "" } else { "s" }
            ),
            _ => format!("Invalid CSV: {e}"),
        })
    };
    let mut reader = csv::ReaderBuilder::new().from_reader(text.as_bytes());
    let headers = reader.headers().map_err(invalid)?.clone();
    let mut rows = vec![];
    for record in reader.records() {
        let record = record.map_err(invalid)?;
        let object = Object::new();
        for (name, value) in headers.iter().zip(record.iter()) {
            object.set_str(name, Value::str(value));
        }
        rows.push(Value::Object(object));
    }
    Ok(Value::list(rows))
}

/// Write the given values as a CSV file
///
/// The first line is a header naming every field of the objects, in order of first appearance; each object is
/// then a line, with an empty value for a field it doesn't have, or that is null.
#[builtin(namespace = "csv", returns = Type::UNIT, completion = complete_files)]
pub fn write(
    ctx: &mut Ctx,
    args: &Args,
    #[param("File to write CSV records to")] file: PathArg,
    #[param("Data to write")] values: Value,
) -> R<()> {
    let Value::List(values) = values else {
        return Err(args.invalid(
            "values",
            &format!("Must be a sequence, but was a {}", values.type_name()),
        ));
    };
    let mut fields: Vec<String> = vec![];
    let mut records = vec![];
    for value in values.snapshot() {
        let Value::Object(object) = value else {
            return Err(args.invalid(
                "values",
                &format!("Values to write must be objects, but found: {}", value.type_name()),
            ));
        };
        let mut record = vec![];
        for (name, value) in object.entries() {
            let name = ctx.stringify(&name)?;
            if !fields.contains(&name) {
                fields.push(name.clone());
            }
            // Null is no value, as a missing field is (as Python's csv module and jq's @csv write it)
            let text = if value.is_null() {
                String::new()
            } else {
                ctx.stringify(&value)?
            };
            record.push((name, text));
        }
        records.push(record);
    }
    let mut text = String::new();
    write_record(&mut text, fields.iter().map(String::as_str));
    for record in &records {
        write_record(
            &mut text,
            fields.iter().map(|field| {
                record
                    .iter()
                    .find(|(name, _)| name == field)
                    .map_or("", |(_, value)| value.as_str())
            }),
        );
    }
    ctx.platform()
        .write_file(fs(&file.0), text.as_bytes(), false)
        .map_err(|e| io_error("write", &file.0, &e))
}

/// A record: values separated by commas, ending with CRLF
fn write_record<'a>(out: &mut String, values: impl Iterator<Item = &'a str>) {
    for (i, value) in values.enumerate() {
        if i > 0 {
            out.push(',');
        }
        if needs_quotes(value, i == 0) {
            out.push('"');
            out.push_str(&value.replace('"', "\"\""));
            out.push('"');
        } else {
            out.push_str(value);
        }
    }
    out.push_str("\r\n");
}

/// Commons CSV 1.4's `QuoteMode.MINIMAL` rules: quote an empty first value (so that the line isn't blank), a
/// first value starting with anything but an ASCII letter or digit, any value starting with a character up to
/// `#`, containing a comma, quote, CR or LF, or ending with a character up to space
fn needs_quotes(value: &str, first: bool) -> bool {
    let Some(c) = value.chars().next() else {
        return first;
    };
    if first && !c.is_ascii_alphanumeric() {
        return true;
    }
    if c <= '#' || value.contains([',', '"', '\r', '\n']) {
        return true;
    }
    value.chars().last().is_some_and(|c| c <= ' ')
}

#[cfg(test)]
mod tests {
    use super::*;

    fn line(values: &[&str]) -> String {
        let mut out = String::new();
        write_record(&mut out, values.iter().copied());
        out
    }

    /// Expected output from Scala Mash (Commons CSV 1.4)
    #[test]
    fn quoting() {
        assert_eq!(line(&["a", "b", "c"]), "a,b,c\r\n");
        assert_eq!(line(&["1", "x,y", ""]), "1,\"x,y\",\r\n");
        assert_eq!(line(&["\"q\"", "", ""]), "\"\"\"q\"\"\",,\r\n");
        assert_eq!(line(&[" lead", "#c", ""]), "\" lead\",\"#c\",\r\n");
        assert_eq!(line(&["", "1"]), "\"\",1\r\n");
        assert_eq!(line(&["-x", "-y"]), "\"-x\",-y\r\n");
        assert_eq!(line(&["é", "x "]), "\"é\",\"x \"\r\n");
        assert_eq!(line(&["a\"b", "line\\nbreak"]), "\"a\"\"b\",line\\nbreak\r\n");
        assert_eq!(line(&[]), "\r\n");
    }
}
