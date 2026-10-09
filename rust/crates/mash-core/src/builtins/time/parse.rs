//! Parsing strings as date-times (`String.toDateTime`) and Mash's fixed date-time format (`DateTime.format`).
//!
//! `to_date_time` tries strict, unambiguous formats first (RFC 9557 / RFC 3339 / ISO 8601, RFC 2822, Mash's own
//! `format` output, ISO civil date-times), then falls back to natural language via uutils `parse_datetime` (GNU
//! `date -d` grammar), after a small normalisation pass that rewrites common phrasings (from the natty parser
//! Scala Mash used) into forms that grammar accepts. The current time is always passed in.

use jiff::civil::{Date, DateTime, Time};
use jiff::tz::{AmbiguousOffset, Offset, TimeZone};
use jiff::{SignedDuration, Timestamp, Zoned};

/// Mash's fixed, locale-independent rendering of a date-time, e.g. `Sat 28 Mar 2026 12:00:00 GMT`. The zone
/// abbreviation is the one in force in the local zone at that instant.
const FORMAT: &str = "%a %-d %b %Y %H:%M:%S %Z";
const FORMAT_WITHOUT_ZONE: &str = "%a %d %b %Y %H:%M:%S";

pub(crate) fn format(t: Timestamp, tz: &TimeZone) -> String {
    t.to_zoned(tz.clone()).strftime(FORMAT).to_string()
}

/// Parse a string as a point in time, relative to `now` (whose time zone is the local zone). None if it can't
/// be parsed.
pub(crate) fn to_date_time(s: &str, now: &Zoned) -> Option<Timestamp> {
    let s = s.trim();
    if s.is_empty() {
        return None;
    }
    parse_strict(s, now.time_zone()).or_else(|| parse_natural(s, now))
}

/// Unambiguous machine formats, plus `format`'s output
fn parse_strict(s: &str, tz: &TimeZone) -> Option<Timestamp> {
    // RFC 9557, e.g. 2026-10-09T14:30:00+02:00[Europe/Paris]
    if let Ok(z) = s.parse::<Zoned>() {
        return Some(z.timestamp());
    }
    // RFC 3339 / ISO 8601 with an offset or Z
    if let Ok(t) = s.parse::<Timestamp>() {
        return Some(t);
    }
    if let Ok(z) = jiff::fmt::rfc2822::parse(s) {
        return Some(z.timestamp());
    }
    if let Some(t) = parse_mash_format(s, tz) {
        return Some(t);
    }
    // ISO civil date-time or date without an offset: local time (a bare date is local midnight)
    if let Ok(dt) = s.parse::<DateTime>() {
        return tz.to_ambiguous_zoned(dt).compatible().ok().map(|z| z.timestamp());
    }
    if let Ok(d) = s.parse::<Date>() {
        return tz
            .to_ambiguous_zoned(d.to_datetime(Time::midnight()))
            .compatible()
            .ok()
            .map(|z| z.timestamp());
    }
    None
}

/// Parse `format`'s output. Zone abbreviations are ambiguous in general (BST is British Summer Time or
/// Bangladesh Standard Time), so the abbreviation is resolved against the local zone: whichever of its possible
/// offsets for that civil time carries the abbreviation. That also disambiguates the repeated hour when clocks
/// go back (01:30 BST vs 01:30 GMT). Failing that, only the universal abbreviations are accepted.
fn parse_mash_format(s: &str, tz: &TimeZone) -> Option<Timestamp> {
    let (civil, abbreviation) = s.rsplit_once(' ')?;
    let dt = DateTime::strptime(FORMAT_WITHOUT_ZONE, civil).ok()?;
    let candidates: Vec<Offset> = match tz.to_ambiguous_timestamp(dt).offset() {
        AmbiguousOffset::Unambiguous { offset } => vec![offset],
        AmbiguousOffset::Gap { before, after } | AmbiguousOffset::Fold { before, after } => vec![before, after],
    };
    for offset in candidates {
        let t = offset.to_timestamp(dt).ok()?;
        if tz.to_offset_info(t).abbreviation() == abbreviation {
            return Some(t);
        }
    }
    match abbreviation {
        "UTC" | "GMT" | "UT" | "Z" => Offset::UTC.to_timestamp(dt).ok(),
        _ => None,
    }
}

fn parse_natural(s: &str, now: &Zoned) -> Option<Timestamp> {
    let normalised = normalise(s, now);
    // `parse_datetime_at_date`, not `parse_datetime`: the latter reads the system clock
    let parsed = parse_datetime::parse_datetime_at_date(now.clone(), normalised.as_str()).ok()?;
    parsed.into_zoned().map(|z| z.timestamp())
}

/// Rewrite natty-style phrasings into GNU `date -d` grammar. Deliberately small and word-based.
fn normalise(s: &str, now: &Zoned) -> String {
    let lower = s.to_lowercase();
    let mut text = lower.replace(',', " ");
    for (from, to) in [
        ("the day before yesterday", "2 days ago"),
        ("the day after tomorrow", "+2 days"),
        ("from now", ""),
    ] {
        text = text.replace(from, to);
    }
    let mut words: Vec<String> = Vec::new();
    let mut iter = text.split_whitespace().peekable();
    while let Some(word) = iter.next() {
        match word {
            // "tomorrow at 5pm", "28 March 2026 at 12:00"
            "at" => {}
            "noon" => words.push("12:00".into()),
            "midnight" => words.push("00:00".into()),
            // "in 2 weeks" -> "+2 weeks"
            "in" if iter.peek().is_some_and(|w| w.parse::<u32>().is_ok()) => {
                if let Some(n) = iter.next() {
                    words.push(format!("+{n}"));
                }
            }
            _ => words.push(strip_ordinal(word).to_string()),
        }
    }
    // A relative amount left by "N units from now" needs a sign for GNU's grammar
    if lower.contains("from now") && words.first().is_some_and(|w| w.parse::<u32>().is_ok()) {
        words[0] = format!("+{}", words[0]);
    }
    // Resolve a trailing zone abbreviation against the local zone (see parse_mash_format)
    if let Some(last) = s.split_whitespace().last()
        && (2..=5).contains(&last.len())
        && last.chars().all(|c| c.is_ascii_uppercase())
        && let Some(offset) = local_offset_for_abbreviation(last, now)
    {
        words.pop();
        words.push(format_offset(offset));
    }
    fold_meridiem(&words).join(" ")
}

/// GNU-style numeric offset, e.g. `+0100`
fn format_offset(offset: Offset) -> String {
    let seconds = offset.seconds();
    let sign = if seconds < 0 { '-' } else { '+' };
    let minutes = seconds.abs() / 60;
    format!("{sign}{:02}{:02}", minutes / 60, minutes % 60)
}

/// `6:29:09 pm` -> `18:29:09`: parse_datetime rejects a clock time with seconds plus am/pm when a zone
/// follows. (A bare `5pm` parses fine and is left alone.)
fn fold_meridiem(words: &[String]) -> Vec<String> {
    let mut out: Vec<String> = Vec::new();
    let mut i = 0;
    while i < words.len() {
        let word = &words[i];
        let meridiem = words
            .get(i + 1)
            .map(String::as_str)
            .filter(|w| *w == "am" || *w == "pm");
        if let (Some(m), Some((h, rest))) = (meridiem, word.split_once(':'))
            && let Ok(hour) = h.parse::<u32>()
            && (1..=12).contains(&hour)
        {
            let hour = (hour % 12) + if m == "pm" { 12 } else { 0 };
            out.push(format!("{hour:02}:{rest}"));
            i += 2;
            continue;
        }
        out.push(word.clone());
        i += 1;
    }
    out
}

/// `29th` -> `29`, `1st` -> `1`
fn strip_ordinal(word: &str) -> &str {
    for suffix in ["st", "nd", "rd", "th"] {
        if let Some(digits) = word.strip_suffix(suffix)
            && !digits.is_empty()
            && digits.chars().all(|c| c.is_ascii_digit())
        {
            return digits;
        }
    }
    word
}

/// If the local zone uses this abbreviation (now, or in the other half of the year), its offset
fn local_offset_for_abbreviation(abbreviation: &str, now: &Zoned) -> Option<Offset> {
    let tz = now.time_zone();
    let now = now.timestamp();
    let six_months = SignedDuration::from_hours(24 * 183);
    [now, now.checked_add(six_months).ok()?].into_iter().find_map(|t| {
        let info = tz.to_offset_info(t);
        (info.abbreviation() == abbreviation).then(|| info.offset())
    })
}

#[cfg(test)]
mod tests {
    use jiff::civil::date;

    use super::*;

    fn london() -> TimeZone {
        TimeZone::get("Europe/London").unwrap()
    }

    /// Friday 2026-10-09 10:00 in London (as in the dates spike)
    fn reference_now() -> Zoned {
        date(2026, 10, 9).at(10, 0, 0, 0).to_zoned(london()).unwrap()
    }

    fn parse(s: &str) -> Option<String> {
        to_date_time(s, &reference_now()).map(|t| t.to_zoned(london()).strftime("%Y-%m-%dT%H:%M:%S%:z").to_string())
    }

    #[test]
    fn format_examples() {
        let t = date(2026, 3, 28)
            .at(12, 0, 0, 0)
            .to_zoned(london())
            .unwrap()
            .timestamp();
        assert_eq!(format(t, &london()), "Sat 28 Mar 2026 12:00:00 GMT");
        let t = date(2026, 10, 9)
            .at(10, 0, 0, 0)
            .to_zoned(london())
            .unwrap()
            .timestamp();
        assert_eq!(format(t, &london()), "Fri 9 Oct 2026 10:00:00 BST");
        let tokyo = TimeZone::get("Asia/Tokyo").unwrap();
        assert_eq!(format(t, &tokyo), "Fri 9 Oct 2026 18:00:00 JST");
        assert_eq!(format(t, &TimeZone::UTC), "Fri 9 Oct 2026 09:00:00 UTC");
    }

    /// Mash's DateTimeTest property: `t0 = t.format.toDateTime; t0 == t0.format.toDateTime`. `format` drops
    /// sub-second precision, so the first trip truncates and later trips are exact.
    #[test]
    fn format_round_trips() {
        let zones = [
            "Europe/London",
            "America/New_York",
            "Asia/Kolkata",
            "America/Sao_Paulo",
            "UTC",
            "Australia/Lord_Howe",
        ];
        for zone in zones {
            let tz = TimeZone::get(zone).unwrap();
            let now = Zoned::new(reference_now().timestamp(), tz.clone());
            // Every 7th hour across a year, plus a sub-second offset, covers both DST transitions
            for hour in (0..24 * 366).step_by(7) {
                let t = now.timestamp() + SignedDuration::from_hours(hour) + SignedDuration::from_millis(123);
                let t0 = to_date_time(&format(t, &tz), &now).unwrap_or_else(|| panic!("{zone}: {}", format(t, &tz)));
                assert_eq!(t0.as_second(), t.as_second(), "{zone}: {}", format(t, &tz));
                assert_eq!(to_date_time(&format(t0, &tz), &now), Some(t0));
            }
        }
    }

    /// When clocks go back, 01:30 happens twice; the abbreviation says which one
    #[test]
    fn format_round_trips_in_repeated_hour() {
        let first = "2026-10-25T01:30:00+01:00".parse::<Timestamp>().unwrap();
        let second = "2026-10-25T01:30:00+00:00".parse::<Timestamp>().unwrap();
        assert_eq!(format(first, &london()), "Sun 25 Oct 2026 01:30:00 BST");
        assert_eq!(format(second, &london()), "Sun 25 Oct 2026 01:30:00 GMT");
        assert_eq!(to_date_time(&format(first, &london()), &reference_now()), Some(first));
        assert_eq!(to_date_time(&format(second, &london()), &reference_now()), Some(second));
    }

    #[test]
    fn strict_formats() {
        assert_eq!(parse("2026-10-09T14:30:00Z").unwrap(), "2026-10-09T15:30:00+01:00");
        assert_eq!(
            parse("2026-10-09T14:30:00[Europe/Paris]").unwrap(),
            "2026-10-09T13:30:00+01:00"
        );
        assert_eq!(
            parse("Fri, 09 Oct 2026 14:30:00 +0000").unwrap(),
            "2026-10-09T15:30:00+01:00"
        );
        assert_eq!(parse("2026-10-09 14:30").unwrap(), "2026-10-09T14:30:00+01:00");
        assert_eq!(parse("2026-10-09").unwrap(), "2026-10-09T00:00:00+01:00");
    }

    /// The case from Mash's DateTimeTest
    #[test]
    fn mash_test_case() {
        assert_eq!(parse("29th June 2017, 18:36:44").unwrap(), "2017-06-29T18:36:44+01:00");
    }

    #[test]
    fn natty_style_phrases() {
        assert_eq!(
            parse("June 30, 2017 6:29:09 PM BST").unwrap(),
            "2017-06-30T18:29:09+01:00"
        );
        assert_eq!(
            parse("28 March 2026 at 12:00:00 GMT").unwrap(),
            "2026-03-28T12:00:00+00:00"
        );
        assert_eq!(parse("the day before yesterday").unwrap(), "2026-10-07T10:00:00+01:00");
        assert_eq!(parse("the day after tomorrow").unwrap(), "2026-10-11T10:00:00+01:00");
        assert_eq!(parse("noon").unwrap(), "2026-10-09T12:00:00+01:00");
        assert_eq!(parse("midnight").unwrap(), "2026-10-09T00:00:00+01:00");
        assert_eq!(parse("tomorrow at 5pm").unwrap(), "2026-10-10T17:00:00+01:00");
        assert_eq!(parse("in 2 weeks").unwrap(), "2026-10-23T10:00:00+01:00");
        assert_eq!(parse("2 hours from now").unwrap(), "2026-10-09T12:00:00+01:00");
        assert_eq!(parse("last friday at noon").unwrap(), "2026-10-02T12:00:00+01:00");
    }

    #[test]
    fn gnu_phrases() {
        assert_eq!(parse("3 days ago").unwrap(), "2026-10-06T10:00:00+01:00");
        assert_eq!(parse("next tuesday").unwrap(), "2026-10-13T00:00:00+01:00");
        assert_eq!(parse("-3 days").unwrap(), "2026-10-06T10:00:00+01:00");
        assert_eq!(parse("2026-10-09 +3 days").unwrap(), "2026-10-12T00:00:00+01:00");
        assert_eq!(parse("@1700000000").unwrap(), "2023-11-14T22:13:20+00:00");
        assert_eq!(parse("14:00 EST").unwrap(), "2026-10-09T20:00:00+01:00");
    }

    #[test]
    fn garbage_fails() {
        for s in ["", "  ", "banana", "not a date at all", "2026-13-45", "christmas"] {
            assert_eq!(parse(s), None, "{s}");
        }
    }

    /// Whatever the input, parsing returns rather than panicking
    #[test]
    fn never_panics() {
        let pieces = [
            "2026",
            "-",
            "13",
            ":",
            "at",
            "in",
            "from now",
            "the day",
            "pm",
            "am",
            "BST",
            "GMT",
            "+",
            "@",
            "29th",
            "june",
            ",",
            " ",
            "99999999999",
            "T",
            "Z",
            "[Europe/London]",
            "next",
            "ago",
            "noon",
            "é",
            "\u{0}",
            "0",
            "00:00:00",
            "1st",
            "the day before yesterday",
        ];
        let now = reference_now();
        // A deterministic pseudo-random walk over combinations of pieces
        let mut seed: u64 = 0x2545_F491_4F6C_DD1D;
        for _ in 0..20_000 {
            let mut s = String::new();
            seed ^= seed << 13;
            seed ^= seed >> 7;
            seed ^= seed << 17;
            let mut r = seed;
            for _ in 0..(r % 6 + 1) {
                s.push_str(pieces[(r % pieces.len() as u64) as usize]);
                r /= pieces.len() as u64;
                if r.is_multiple_of(2) {
                    s.push(' ');
                }
                r /= 2;
            }
            let _ = to_date_time(&s, &now);
        }
    }
}
