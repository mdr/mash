//! The `time` namespace: date-times, dates, and units of time (numbers tagged with e.g. `time.Days`).
//!
//! The clock and time zone always come from the platform. Arithmetic with units (`now - 3.days`,
//! `3.days.ago`) is calendar arithmetic in the local zone: adding a day across a daylight-saving change keeps
//! the time of day, and a month is a calendar month. Fractional milliseconds to hours are exact; fractional
//! days, weeks and months are errors.

pub(crate) mod fuzzy;
pub(crate) mod parse;

use std::time::Duration;

use jiff::tz::TimeZone;
use jiff::{SignedDuration, Span, Timestamp, Zoned};
use mash_macros::{builtin, builtin_method};

use crate::eval::{Ctx, MashError, R};
use crate::function::{BuiltinFunction, BuiltinMethod};
use crate::value::{BuiltinClass, DateTime, LocalDate, Number, Str, Tag, Value};

use super::core::ANY_CLASS;

/// Functions in `time` that are also in the global scope
pub(crate) const FUNCTIONS: &[&dyn BuiltinFunction] = &[
    &FromMillisSinceEpochFunction,
    &NowFunction,
    &ParseIso8601Function,
    &SleepFunction,
    &TodayFunction,
];

/// Classes in `time`
pub(crate) const CLASSES: &[&BuiltinClass] = &[
    &DATE_CLASS,
    &DATE_TIME_CLASS,
    &MILLISECONDS_CLASS,
    &SECONDS_CLASS,
    &MINUTES_CLASS,
    &HOURS_CLASS,
    &DAYS_CLASS,
    &WEEKS_CLASS,
    &MONTHS_CLASS,
];

pub static DATE_CLASS: BuiltinClass = BuiltinClass {
    summary: "A date",
    methods: &[&DayMethod, &MonthMethod, &WeekdayMethod, &YearMethod],
    ..BuiltinClass::new("time", "Date", Some(&ANY_CLASS))
};

pub static DATE_TIME_CLASS: BuiltinClass = BuiltinClass {
    summary: "An instant in time",
    methods: &[
        &DateMethod,
        &DayMethod,
        &FormatMethod,
        &FuzzyMethod,
        &HourMethod,
        &MillisSinceEpochMethod,
        &MinuteMethod,
        &MonthMethod,
        &SecondMethod,
        &WeekdayMethod,
        &YearMethod,
    ],
    ..BuiltinClass::new("time", "DateTime", Some(&ANY_CLASS))
};

const UNIT_METHODS: &[&dyn BuiltinMethod] = &[&AgoMethod, &FromNowMethod, &UnitSleepMethod];

const fn unit_class(name: &'static str, summary: &'static str) -> BuiltinClass {
    BuiltinClass {
        summary,
        methods: UNIT_METHODS,
        ..BuiltinClass::new("time", name, Some(&ANY_CLASS))
    }
}

pub static MILLISECONDS_CLASS: BuiltinClass = unit_class("Milliseconds", "A tag class for a number of milliseconds");
pub static SECONDS_CLASS: BuiltinClass = unit_class("Seconds", "A tag class for a number of seconds");
pub static MINUTES_CLASS: BuiltinClass = unit_class("Minutes", "A tag class for a number of minutes");
pub static HOURS_CLASS: BuiltinClass = unit_class("Hours", "A tag class for a number of hours");
pub static DAYS_CLASS: BuiltinClass = unit_class("Days", "A tag class for a number of days");
pub static WEEKS_CLASS: BuiltinClass = unit_class("Weeks", "A tag class for a number of weeks");
pub static MONTHS_CLASS: BuiltinClass = unit_class("Months", "A tag class for a number of months");

// ------------------------------------------------------------------------------------------------ units

/// A unit of time that numbers can be tagged with
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum TimeUnit {
    Milliseconds,
    Seconds,
    Minutes,
    Hours,
    Days,
    Weeks,
    Months,
}

impl TimeUnit {
    const ALL: [TimeUnit; 7] = [
        TimeUnit::Milliseconds,
        TimeUnit::Seconds,
        TimeUnit::Minutes,
        TimeUnit::Hours,
        TimeUnit::Days,
        TimeUnit::Weeks,
        TimeUnit::Months,
    ];

    pub(crate) fn class(self) -> Tag {
        match self {
            TimeUnit::Milliseconds => &MILLISECONDS_CLASS,
            TimeUnit::Seconds => &SECONDS_CLASS,
            TimeUnit::Minutes => &MINUTES_CLASS,
            TimeUnit::Hours => &HOURS_CLASS,
            TimeUnit::Days => &DAYS_CLASS,
            TimeUnit::Weeks => &WEEKS_CLASS,
            TimeUnit::Months => &MONTHS_CLASS,
        }
    }

    /// The unit a number is tagged with, if any
    pub(crate) fn of(value: &Value) -> Option<(f64, TimeUnit)> {
        match value {
            Value::Number(n, Some(tag)) => {
                let unit = TimeUnit::ALL
                    .into_iter()
                    .find(|unit| std::ptr::eq(unit.class(), *tag))?;
                Some((n.as_f64(), unit))
            }
            _ => None,
        }
    }

    /// The length of one of this unit in milliseconds: None for months, which vary (a day is taken as 24 hours)
    pub(crate) fn fixed_millis(self) -> Option<f64> {
        match self {
            TimeUnit::Milliseconds => Some(1.0),
            TimeUnit::Seconds => Some(1_000.0),
            TimeUnit::Minutes => Some(60_000.0),
            TimeUnit::Hours => Some(3_600_000.0),
            TimeUnit::Days => Some(86_400_000.0),
            TimeUnit::Weeks => Some(604_800_000.0),
            TimeUnit::Months => None,
        }
    }

    pub(crate) fn plural_name(self) -> &'static str {
        match self {
            TimeUnit::Milliseconds => "milliseconds",
            TimeUnit::Seconds => "seconds",
            TimeUnit::Minutes => "minutes",
            TimeUnit::Hours => "hours",
            TimeUnit::Days => "days",
            TimeUnit::Weeks => "weeks",
            TimeUnit::Months => "months",
        }
    }

    /// `t` plus `n` of this unit, with calendar arithmetic in `t`'s zone. Fractions are exact for
    /// milliseconds to hours, and an error for days, weeks and months.
    pub(crate) fn add(self, t: &Zoned, n: f64) -> R<Zoned> {
        let out_of_range = || MashError::new(format!("Date-time out of range adding {n} {}", self.plural_name()));
        let seconds_per_unit = match self {
            TimeUnit::Milliseconds => Some(0.001),
            TimeUnit::Seconds => Some(1.0),
            TimeUnit::Minutes => Some(60.0),
            TimeUnit::Hours => Some(3600.0),
            TimeUnit::Days | TimeUnit::Weeks | TimeUnit::Months => None,
        };
        match seconds_per_unit {
            Some(seconds) => {
                let duration = SignedDuration::try_from_secs_f64(n * seconds).map_err(|_| out_of_range())?;
                t.checked_add(duration).map_err(|_| out_of_range())
            }
            None => {
                if n.fract() != 0.0 {
                    return Err(MashError::new(format!(
                        "Cannot use a fractional number of {} ({n}) in date arithmetic",
                        self.plural_name()
                    )));
                }
                let whole = Number::new(n).as_i64().ok_or_else(out_of_range)?;
                let span = match self {
                    TimeUnit::Days => Span::new().try_days(whole),
                    TimeUnit::Weeks => Span::new().try_weeks(whole),
                    _ => Span::new().try_months(whole),
                }
                .map_err(|_| out_of_range())?;
                t.checked_add(span).map_err(|_| out_of_range())
            }
        }
    }
}

/// `t ± n` of a unit, as a DateTime value
pub(crate) fn add_to_date_time(ctx: &Ctx, t: Timestamp, n: f64, unit: TimeUnit) -> R<Value> {
    let zoned = t.to_zoned(ctx.time_zone());
    Ok(Value::DateTime(DateTime(unit.add(&zoned, n)?.timestamp())))
}

/// The difference between two date-times, in milliseconds (a `time.Milliseconds`-tagged number)
pub(crate) fn difference(later: Timestamp, earlier: Timestamp) -> Value {
    let millis = later.duration_since(earlier).as_millis() as f64;
    Value::Number(Number::new(millis), Some(&MILLISECONDS_CLASS))
}

fn now_zoned(ctx: &Ctx) -> Zoned {
    ctx.platform().now().to_zoned(ctx.time_zone())
}

/// Sleep for `duration`, in slices so that an interrupt (Ctrl-C) is noticed promptly
fn sleep_for(ctx: &Ctx, duration: Duration) -> R<()> {
    const SLICE: Duration = Duration::from_millis(100);
    let mut remaining = duration;
    while !remaining.is_zero() {
        if ctx.poll_interrupt() {
            return Err(MashError::interrupted());
        }
        let slice = remaining.min(SLICE);
        ctx.platform().sleep(slice);
        remaining -= slice;
    }
    Ok(())
}

/// How long until now plus `n` of `unit` (zero if that's in the past)
fn duration_from_now(ctx: &Ctx, n: f64, unit: TimeUnit) -> R<Duration> {
    let now = now_zoned(ctx);
    let later = unit.add(&now, n)?;
    Ok(later
        .timestamp()
        .duration_since(now.timestamp())
        .try_into()
        .unwrap_or(Duration::ZERO))
}

fn instant(target: &Value) -> R<Timestamp> {
    match target {
        Value::DateTime(t) => Ok(t.0),
        _ => Err(MashError::new(format!(
            "Expected a DateTime, but was a {}",
            target.type_name()
        ))),
    }
}

fn tagged_amount(target: &Value) -> R<(f64, TimeUnit)> {
    TimeUnit::of(target).ok_or_else(|| {
        MashError::new(format!(
            "Expected a number of a unit of time, but was a {}",
            target.type_name()
        ))
    })
}

// ------------------------------------------------------------------------------------------------ functions

/// The current date and time
#[builtin(namespace = "time", returns = crate::infer::Type::Instance(&crate::builtins::time::DATE_TIME_CLASS))]
pub fn now(ctx: &mut Ctx) -> R<Value> {
    Ok(Value::DateTime(DateTime(ctx.platform().now())))
}

/// The current date
#[builtin(namespace = "time", returns = crate::infer::Type::Instance(&crate::builtins::time::DATE_CLASS))]
pub fn today(ctx: &mut Ctx) -> R<Value> {
    Ok(Value::Date(LocalDate(now_zoned(ctx).date())))
}

/// Parse the given string as a date/time in ISO-8601 format
#[builtin(namespace = "time", returns = crate::infer::Type::Instance(&crate::builtins::time::DATE_TIME_CLASS))]
pub fn parse_iso8601(#[param("Date in ISO-8601 format")] date: Str) -> R<Value> {
    let s = date.as_str();
    let parsed = s
        .parse::<Zoned>()
        .map(|z| z.timestamp())
        .or_else(|_| s.parse::<Timestamp>());
    match parsed {
        Ok(t) => Ok(Value::DateTime(DateTime(t))),
        Err(_) => Err(MashError::new(format!(
            "Could not parse '{s}' as an ISO 8601 date-time"
        ))),
    }
}

/// Get a DateTime representing the number of millis since the epoch
#[builtin(namespace = "time", returns = crate::infer::Type::Instance(&crate::builtins::time::DATE_TIME_CLASS))]
pub fn from_millis_since_epoch(#[param("Milliseconds since the epoch")] millis: Number) -> R<Value> {
    let millis = millis.as_f64().trunc();
    let t = if millis.is_finite() {
        Timestamp::from_millisecond(millis as i64).ok()
    } else {
        None
    };
    t.map(|t| Value::DateTime(DateTime(t)))
        .ok_or_else(|| MashError::new(format!("{millis} milliseconds since the epoch is out of range")))
}

/// Sleep for a given duration
///
/// Examples:
/// <mash>
///   sleep 1000      # Sleep 1 second
///   sleep 3.seconds
/// </mash>
#[builtin(namespace = "time", returns = crate::infer::Type::UNIT)]
pub fn sleep(
    ctx: &mut Ctx,
    #[param(
        "Duration to sleep",
        description = "Accepts tagged durations. If given an untagged Number, sleeps that number of milliseconds"
    )]
    duration: Value,
) -> R<()> {
    let duration = match (&duration, TimeUnit::of(&duration)) {
        (_, Some((n, unit))) => duration_from_now(ctx, n, unit)?,
        (Value::Number(n, _), None) => Duration::try_from_secs_f64(n.as_f64() / 1000.0).unwrap_or(Duration::ZERO),
        _ => {
            return Err(MashError::new(format!(
                "Invalid argument 'duration'. Invalid duration of type {}",
                duration.type_name()
            )));
        }
    };
    sleep_for(ctx, duration)
}

// ------------------------------------------------------------------------------------------------ unit methods

/// Point in time this many units ago
#[builtin_method(returns = crate::infer::Type::Instance(&crate::builtins::time::DATE_TIME_CLASS))]
fn ago(ctx: &mut Ctx, target: &Value) -> R<Value> {
    let (n, unit) = tagged_amount(target)?;
    add_to_date_time(ctx, ctx.platform().now(), -n, unit)
}

/// Point in time this many units from now
#[builtin_method(returns = crate::infer::Type::Instance(&crate::builtins::time::DATE_TIME_CLASS))]
fn from_now(ctx: &mut Ctx, target: &Value) -> R<Value> {
    let (n, unit) = tagged_amount(target)?;
    add_to_date_time(ctx, ctx.platform().now(), n, unit)
}

/// Sleep for this many units
#[builtin_method(name = "sleep", returns = crate::infer::Type::UNIT)]
fn unit_sleep(ctx: &mut Ctx, target: &Value) -> R<()> {
    let (n, unit) = tagged_amount(target)?;
    let duration = duration_from_now(ctx, n, unit)?;
    sleep_for(ctx, duration)
}

// ------------------------------------------------------------------------------------------------ DateTime methods

fn local(ctx: &Ctx, target: &Value) -> R<Zoned> {
    Ok(instant(target)?.to_zoned(ctx.time_zone()))
}

/// Milliseconds since the Unix epoch
#[builtin_method(returns = crate::infer::Type::NUMBER)]
fn millis_since_epoch(target: &Value) -> R<Value> {
    Ok(Value::num(instant(target)?.as_millisecond() as f64))
}

/// Format as a human-friendly absolute time, e.g. Fri 9 Oct 2026 10:00:00 BST
///
/// The format is fixed and locale-independent, and `toDateTime` parses it back (in the same time zone).
#[builtin_method(returns = crate::infer::Type::STRING)]
fn format(ctx: &mut Ctx, target: &Value) -> R<String> {
    Ok(parse::format(instant(target)?, &ctx.time_zone()))
}

/// Render as human-friendly relative time, e.g. 1 day ago, 3 months from now, etc
#[builtin_method(returns = crate::infer::Type::STRING)]
fn fuzzy(ctx: &mut Ctx, target: &Value) -> R<String> {
    Ok(fuzzy::fuzzy(&local(ctx, target)?, &now_zoned(ctx)))
}

/// Date portion of this date and time
#[builtin_method(returns = crate::infer::Type::Instance(&crate::builtins::time::DATE_CLASS))]
fn date(ctx: &mut Ctx, target: &Value) -> R<Value> {
    Ok(Value::Date(LocalDate(local(ctx, target)?.date())))
}

/// Hour of the day, in the current time zone
#[builtin_method(returns = crate::infer::Type::NUMBER)]
fn hour(ctx: &mut Ctx, target: &Value) -> R<i64> {
    Ok(i64::from(local(ctx, target)?.hour()))
}

/// Minute of the hour, in the current time zone
#[builtin_method(returns = crate::infer::Type::NUMBER)]
fn minute(ctx: &mut Ctx, target: &Value) -> R<i64> {
    Ok(i64::from(local(ctx, target)?.minute()))
}

/// Second of the minute, in the current time zone
#[builtin_method(returns = crate::infer::Type::NUMBER)]
fn second(ctx: &mut Ctx, target: &Value) -> R<i64> {
    Ok(i64::from(local(ctx, target)?.second()))
}

/// The calendar date of a Date, or of a DateTime in the current time zone
fn calendar_date(ctx: &Ctx, target: &Value) -> R<jiff::civil::Date> {
    match target {
        Value::Date(LocalDate(d)) => Ok(*d),
        _ => Ok(local(ctx, target)?.date()),
    }
}

/// Year, e.g. 2026 (for a DateTime, in the current time zone)
#[builtin_method(returns = crate::infer::Type::NUMBER)]
fn year(ctx: &mut Ctx, target: &Value) -> R<i64> {
    Ok(i64::from(calendar_date(ctx, target)?.year()))
}

/// Month of the year, from 1 (January) to 12 (for a DateTime, in the current time zone)
#[builtin_method(returns = crate::infer::Type::NUMBER)]
fn month(ctx: &mut Ctx, target: &Value) -> R<i64> {
    Ok(i64::from(calendar_date(ctx, target)?.month()))
}

/// Day of the month, from 1 (for a DateTime, in the current time zone)
#[builtin_method(returns = crate::infer::Type::NUMBER)]
fn day(ctx: &mut Ctx, target: &Value) -> R<i64> {
    Ok(i64::from(calendar_date(ctx, target)?.day()))
}

/// Day of the week, in English, e.g. Wednesday (for a DateTime, in the current time zone)
#[builtin_method(returns = crate::infer::Type::STRING)]
fn weekday(ctx: &mut Ctx, target: &Value) -> R<String> {
    Ok(calendar_date(ctx, target)?.strftime("%A").to_string())
}

// ------------------------------------------------------------------------------------------------ String methods

/// Parse this string as a DateTime
///
/// Accepts ISO 8601 / RFC 3339 (with or without a zone), RFC 2822, the output of `DateTime.format`, and
/// natural language such as `tomorrow at 5pm`, `3 days ago`, `next tuesday` or `29th June 2017, 18:36:44`.
/// Returns null if the string can't be parsed.
#[builtin_method(returns = crate::infer::Type::Instance(&crate::builtins::time::DATE_TIME_CLASS))]
pub fn to_date_time(ctx: &mut Ctx, target: Str) -> R<Value> {
    Ok(parse::to_date_time(target.as_str(), &now_zoned(ctx)).map_or(Value::Null, |t| Value::DateTime(DateTime(t))))
}

/// Parse this string as a Date
///
/// Accepts the same formats as `toDateTime`, and takes the date in the current time zone. Returns null if the
/// string can't be parsed.
#[builtin_method(returns = crate::infer::Type::Instance(&crate::builtins::time::DATE_CLASS))]
pub fn to_date(ctx: &mut Ctx, target: Str) -> R<Value> {
    let tz: TimeZone = ctx.time_zone();
    Ok(parse::to_date_time(target.as_str(), &now_zoned(ctx))
        .map_or(Value::Null, |t| Value::Date(LocalDate(t.to_zoned(tz).date()))))
}
