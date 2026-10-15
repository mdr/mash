//! The `toml` namespace: converting between Mash values and TOML text and files (through the `toml` crate).

use jiff::civil;
use jiff::tz::{Offset, TimeZone};
use mash_macros::builtin;
use toml::value::{Date, Datetime, Offset as TomlOffset, Time};

use super::os::paths::{PathArg, fs, io_error};
use super::os::{complete_files, read_text};
use crate::eval::{Ctx, MashError, R};
use crate::function::BuiltinFunction;
use crate::infer::Type;
use crate::value::{DateTime, LocalDate, Object, Str, Value};

pub(crate) const FUNCTIONS: &[&dyn BuiltinFunction] =
    &[&FromStringFunction, &ReadFunction, &ToStringFunction, &WriteFunction];

/// Read the given string and parse it as TOML
///
/// See toml.read for how TOML becomes Mash values.
#[builtin(namespace = "toml", returns = Type::OBJECT)]
pub fn from_string(ctx: &mut Ctx, #[param("String to parse as TOML")] string: Str) -> R<Value> {
    parse_toml(ctx, string.as_str())
}

/// Read the given file and parse it as TOML
///
/// A date-time with an offset becomes a DateTime; one without, a DateTime in the local time zone (as toDateTime
/// reads one); a date, a Date; and a time of day, a string. Integers beyond 2^53 lose precision, as Mash
/// numbers are floating point. Comments are lost, so writing back what was read loses them.
///
/// Examples:
/// <mash>
///   toml.read "Cargo.toml" | .dependencies.fields.name
///   toml.read "pyproject.toml" | .project.version
/// </mash>
#[builtin(namespace = "toml", returns = Type::OBJECT, completion = complete_files)]
pub fn read(ctx: &mut Ctx, #[param("File from which to read TOML")] file: PathArg) -> R<Value> {
    let text = read_text(ctx, &file.0)?;
    parse_toml(ctx, &text)
}

/// Convert the given value to a TOML string
///
/// The value must be an object, as a TOML document is a table. A field whose value is null is left out, as
/// TOML has no null (null in a list is an error); DateTimes and Dates are written as TOML dates, and lists of
/// objects as arrays of tables. Other values that aren't TOML types are written as strings.
#[builtin(namespace = "toml", name = "toString", returns = Type::STRING)]
pub fn to_string(ctx: &mut Ctx, #[param("Object to convert to a TOML string")] value: Value) -> R<String> {
    toml_string(ctx, &value)
}

/// Write the given object as TOML to the given file
///
/// See toml.toString for how values are written.
#[builtin(namespace = "toml", returns = Type::UNIT, completion = complete_files)]
pub fn write(
    ctx: &mut Ctx,
    #[param("File to write TOML to")] file: PathArg,
    #[param("Object to write")] value: Value,
) -> R<()> {
    let text = toml_string(ctx, &value)?;
    ctx.platform()
        .write_file(fs(&file.0), text.as_bytes(), false)
        .map_err(|e| io_error("write", &file.0, &e))
}

fn parse_toml(ctx: &Ctx, text: &str) -> R<Value> {
    let table: toml::Table = text
        .parse()
        .map_err(|e: toml::de::Error| MashError::new(format!("Invalid TOML: {}", e.message())))?;
    let local = ctx.platform().time_zone();
    from_toml(&toml::Value::Table(table), &local)
}

fn from_toml(toml: &toml::Value, local: &TimeZone) -> R<Value> {
    Ok(match toml {
        toml::Value::String(s) => Value::str(s),
        toml::Value::Integer(i) => Value::num(*i as f64),
        toml::Value::Float(f) => Value::num(*f),
        toml::Value::Boolean(b) => Value::Bool(*b),
        toml::Value::Datetime(datetime) => from_datetime(datetime, local)?,
        toml::Value::Array(items) => Value::list(items.iter().map(|v| from_toml(v, local)).collect::<R<_>>()?),
        toml::Value::Table(table) => {
            let object = Object::new();
            for (key, value) in table {
                object.set_str(key, from_toml(value, local)?);
            }
            Value::Object(object)
        }
    })
}

/// A TOML date or time: with a date and time, a DateTime (in the local time zone if there's no offset); with
/// only a date, a Date; with only a time, a string
fn from_datetime(datetime: &Datetime, local: &TimeZone) -> R<Value> {
    let invalid = |e: jiff::Error| MashError::new(format!("Invalid TOML date-time {datetime}: {e}"));
    let date = |d: &Date| civil::Date::new(d.year as i16, d.month as i8, d.day as i8).map_err(invalid);
    Ok(match (&datetime.date, &datetime.time) {
        (Some(d), Some(t)) => {
            let civil = date(d)?.to_datetime(
                civil::Time::new(
                    t.hour as i8,
                    t.minute as i8,
                    t.second.unwrap_or(0) as i8,
                    t.nanosecond.unwrap_or(0) as i32,
                )
                .map_err(invalid)?,
            );
            let zone = match datetime.offset {
                Some(TomlOffset::Z) => TimeZone::UTC,
                Some(TomlOffset::Custom { minutes }) => {
                    TimeZone::fixed(Offset::from_seconds(i32::from(minutes) * 60).map_err(invalid)?)
                }
                None => local.clone(),
            };
            Value::DateTime(DateTime(civil.to_zoned(zone).map_err(invalid)?.timestamp()))
        }
        (Some(d), None) => Value::Date(LocalDate(date(d)?)),
        _ => Value::str(&datetime.to_string()),
    })
}

/// An object as TOML text
fn toml_string(ctx: &mut Ctx, value: &Value) -> R<String> {
    let Value::Object(_) = value else {
        return Err(MashError::new(format!(
            "Cannot write a {} as TOML: a TOML document is a table, so must be an object",
            value.type_name()
        )));
    };
    match to_toml(ctx, value, &mut vec![])? {
        Some(toml::Value::Table(table)) => {
            toml::to_string(&table).map_err(|e| MashError::new(format!("Could not write TOML: {e}")))
        }
        _ => unreachable!("an object becomes a table"),
    }
}

/// A Mash value as TOML, or None for null (which TOML lacks): whole numbers as integers, DateTimes in UTC,
/// Dates as dates, and other values that aren't TOML types as strings
fn to_toml(ctx: &mut Ctx, value: &Value, stack: &mut Vec<usize>) -> R<Option<toml::Value>> {
    let cyclic = || MashError::new("Cannot convert a cyclic value to TOML");
    Ok(Some(match value {
        Value::Null => return Ok(None),
        Value::Bool(b) => toml::Value::Boolean(*b),
        Value::Number(n, _) => match n.as_i64() {
            Some(i) => toml::Value::Integer(i),
            None => toml::Value::Float(n.as_f64()),
        },
        Value::Str(s) => toml::Value::String(s.as_str().to_string()),
        Value::DateTime(DateTime(t)) => {
            let utc = t.to_zoned(TimeZone::UTC);
            toml::Value::Datetime(Datetime {
                date: Some(toml_date(utc.date())),
                time: Some(Time {
                    hour: utc.hour() as u8,
                    minute: utc.minute() as u8,
                    second: Some(utc.second() as u8),
                    // (No fraction for whole seconds)
                    nanosecond: Some(utc.subsec_nanosecond() as u32).filter(|&n| n > 0),
                }),
                offset: Some(TomlOffset::Z),
            })
        }
        Value::Date(LocalDate(d)) => toml::Value::Datetime(Datetime {
            date: Some(toml_date(*d)),
            time: None,
            offset: None,
        }),
        Value::List(xs) => {
            if stack.contains(&xs.addr()) {
                return Err(cyclic());
            }
            stack.push(xs.addr());
            let mut items = vec![];
            for item in xs.snapshot() {
                match to_toml(ctx, &item, stack)? {
                    Some(item) => items.push(item),
                    None => {
                        return Err(MashError::new(
                            "Cannot write null in a list as TOML, as TOML has no null",
                        ));
                    }
                }
            }
            stack.pop();
            toml::Value::Array(items)
        }
        Value::Object(o) => {
            if stack.contains(&o.addr()) {
                return Err(cyclic());
            }
            stack.push(o.addr());
            let mut table = toml::Table::new();
            for (k, v) in o.entries() {
                // A null field is left out
                if let Some(v) = to_toml(ctx, &v, stack)? {
                    table.insert(ctx.stringify(&k)?, v);
                }
            }
            stack.pop();
            toml::Value::Table(table)
        }
        other => toml::Value::String(ctx.stringify(other)?),
    }))
}

fn toml_date(d: civil::Date) -> Date {
    Date {
        year: d.year() as u16,
        month: d.month() as u8,
        day: d.day() as u8,
    }
}
