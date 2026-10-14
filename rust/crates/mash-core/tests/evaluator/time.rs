//! Dates, times and units of time, per the port's agreed semantics (not covered by the Scala tests). The test
//! platform's clock is fixed at Friday 2026-10-09T12:00:00Z, in UTC, and `sleep` advances it.
//!
//! Deliberate differences from Scala Mash: unit arithmetic (`now + 1.months`, `3.days.ago`) is calendar
//! arithmetic in the local zone, and works for months (Scala threw); fractional milliseconds to hours are exact
//! and fractional days/weeks/months are errors (Scala truncated); `format` is a fixed, locale-independent
//! format that `toDateTime` parses back; `fuzzy` uses the largest whole unit; `toDateTime` uses a new parser.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    // The clock
    Case::eq(r#"now"#, r#"'2026-10-09T12:00:00Z'.toDateTime"#),
    Case::eq(r#"today"#, r#"'2026-10-09'.toDate"#),
    Case::eq(r#"now.date == today"#, r#"true"#),
    Case::eq(r#"now.millisSinceEpoch"#, r#"1791547200000"#),
    Case::eq(r#"[now.hour, now.minute, now.second]"#, r#"[12, 0, 0]"#),
    Case::eq(
        r#"[now.year, now.month, now.day, now.weekday]"#,
        r#"[2026, 10, 9, 'Friday']"#,
    ),
    Case::eq(
        r#"[today.year, today.month, today.day, today.weekday]"#,
        r#"[2026, 10, 9, 'Friday']"#,
    ),
    Case::eq(r#"'2026-09-01T23:30:00-05:00'.toDateTime.day"#, r#"2"#),
    Case::eq(r#"'2024-02-29'.toDate.weekday"#, r#"'Thursday'"#),
    Case::eq(r#"now.format '%Y-%m-%d %H:%M'"#, r#"'2026-10-09 12:00'"#),
    Case::eq(r#"today.format '%B %Y'"#, r#"'October 2026'"#),
    Case::eq(r#"today.format"#, r#"'Fri 9 Oct 2026'"#),
    Case::eq(r#"today.format '100%%'"#, r#"'100%'"#),
    Case::throws(r#"today.format '%H:%M'"#),
    Case::eq(r#"time.fromMillisSinceEpoch 0"#, r#"'1970-01-01T00:00:00Z'.toDateTime"#),
    Case::eq(r#"time.fromMillisSinceEpoch 1791547200000"#, r#"now"#),
    Case::eq(
        r#"time.parseIso8601 '2026-10-09T12:00:00+01:00'"#,
        r#"'2026-10-09T11:00:00Z'.toDateTime"#,
    ),
    Case::eq(
        r#"time.parseIso8601 '2026-10-09T12:00:00[Europe/Paris]'"#,
        r#"'2026-10-09T10:00:00Z'.toDateTime"#,
    ),
    Case::throws(r#"time.parseIso8601 'banana'"#),
    // Unit arithmetic
    Case::eq(r#"today + 1.days"#, r#"'2026-10-10'.toDate"#),
    Case::eq(r#"1.day + today"#, r#"'2026-10-10'.toDate"#),
    Case::eq(r#"today - 2.weeks"#, r#"'2026-09-25'.toDate"#),
    Case::eq(r#"'2026-01-31'.toDate + 1.month"#, r#"'2026-02-28'.toDate"#),
    Case::eq(r#"'2026-12-25'.toDate - today"#, r#"77.days"#),
    Case::eq(r#"today - '2026-12-25'.toDate"#, r#"-77.days"#),
    Case::eq(r#"'2024-03-01'.toDate - '2024-02-01'.toDate"#, r#"29"#),
    Case::eq(r#"today + ('2026-12-25'.toDate - today)"#, r#"'2026-12-25'.toDate"#),
    Case::throws(r#"today + 3.hours"#),
    Case::throws(r#"today + 1.5.days"#),
    Case::throws(r#"today + 1"#),
    Case::eq(r#"3.days.ago"#, r#"'2026-10-06T12:00:00Z'.toDateTime"#),
    Case::eq(r#"3.days.ago == now - 3.days"#, r#"true"#),
    Case::eq(r#"1.day.fromNow == now + 1.day"#, r#"true"#),
    Case::eq(r#"1.day + now == now + 1.day"#, r#"true"#),
    Case::eq(r#"now + 1.months"#, r#"'2026-11-09T12:00:00Z'.toDateTime"#),
    Case::eq(r#"1.month.ago"#, r#"'2026-09-09T12:00:00Z'.toDateTime"#),
    Case::eq(r#"now + 2.weeks"#, r#"'2026-10-23T12:00:00Z'.toDateTime"#),
    Case::eq(r#"now - 1.5.hours"#, r#"'2026-10-09T10:30:00Z'.toDateTime"#),
    Case::eq(
        r#"now + 90.minutes + 30.seconds + 500.milliseconds"#,
        r#"'2026-10-09T13:30:30.5Z'.toDateTime"#,
    ),
    Case::throws(r#"now + 1.5.days"#),
    Case::throws(r#"2.5.months.ago"#),
    Case::throws(r#"now + 3"#),
    Case::eq(r#"(now + 2.hours) - now"#, r#"7200000"#),
    Case::eq(r#"((now + 2.hours) - now).tag"#, r#"time.Milliseconds"#),
    // New: amounts in different units of time combine in the smaller unit, and compare by length (Scala kept
    // the left operand's unit, so 1.days + 3.hours was 4 days)
    Case::eq(r#"1.days + 3.hours"#, r#"27.hours"#),
    Case::eq(r#"(1.days + 3.hours).tag"#, r#"time.Hours"#),
    Case::eq(r#"30.minutes + 1.hours"#, r#"90.minutes"#),
    Case::eq(r#"1.weeks - 1.days"#, r#"6.days"#),
    Case::eq(r#"(2.seconds + 500.milliseconds).tag"#, r#"time.Milliseconds"#),
    Case::eq(r#"now + (1.days + 3.hours) == now + 27.hours"#, r#"true"#),
    Case::eq(
        r#"[3.days < 2.hours, 2.hours < 3.days, 90.minutes >= 1.hours]"#,
        r#"[false, true, true]"#,
    ),
    Case::eq(
        r#"[1.days == 24.hours, 1.days != 25.hours, 1.months == 30.days]"#,
        r#"[true, true, false]"#,
    ),
    // The same unit, or an untagged number, as before
    Case::eq(r#"(1.months + 2.months).tag"#, r#"time.Months"#),
    Case::eq(r#"(2.hours + 30).tag"#, r#"time.Hours"#),
    Case::eq(r#"2.hours + 30"#, r#"32"#),
    Case::throws(r#"1.months + 1.days"#),
    Case::throws(r#"1.months < 40.days"#),
    Case::eq(r#"now - 1.day < now"#, r#"true"#),
    Case::eq(r#"today < now"#, r#"true"#),
    // format, fuzzy and parsing
    Case::eq(r#"now.format"#, r#"'Fri 9 Oct 2026 12:00:00 UTC'"#),
    Case::eq(r#"now.format.toDateTime == now"#, r#"true"#),
    Case::eq(r#"now.fuzzy"#, r#"'moments ago'"#),
    Case::eq(r#"3.days.ago.fuzzy"#, r#"'3 days ago'"#),
    Case::eq(r#"(now - 47.hours).fuzzy"#, r#"'1 day ago'"#),
    Case::eq(r#"2.hours.fromNow.fuzzy"#, r#"'2 hours from now'"#),
    Case::eq(
        r#"'tomorrow at 5pm'.toDateTime"#,
        r#"'2026-10-10T17:00:00Z'.toDateTime"#,
    ),
    Case::eq(
        r#"'29th June 2017, 18:36:44'.toDateTime"#,
        r#"'2017-06-29T18:36:44Z'.toDateTime"#,
    ),
    Case::eq(r#"'in 2 weeks'.toDateTime"#, r#"now + 2.weeks"#),
    Case::eq(r#"'3 days ago'.toDate"#, r#"'2026-10-06'.toDate"#),
    Case::eq(r#"'next tuesday'.toDate"#, r#"'2026-10-13'.toDate"#),
    Case::eq(r#"'banana'.toDateTime"#, r#"null"#),
    Case::eq(r#"'banana'.toDate"#, r#"null"#),
    Case::eq(r#"''.toDateTime"#, r#"null"#),
    // sleep advances the test clock
    Case::eq(r#"sleep 1500; now"#, r#"'2026-10-09T12:00:01.5Z'.toDateTime"#),
    Case::eq(r#"3.seconds.sleep; now"#, r#"'2026-10-09T12:00:03Z'.toDateTime"#),
    Case::eq(r#"sleep 2.minutes; now"#, r#"'2026-10-09T12:02:00Z'.toDateTime"#),
    Case::eq(r#"sleep (-5); now"#, r#"'2026-10-09T12:00:00Z'.toDateTime"#),
    Case::throws(r#"sleep 'a while'"#),
    Case::eq(r#"(timeTaken (sleep 250)).duration"#, r#"250"#),
    Case::eq(r#"(timeTaken (sleep 250)).duration.tag"#, r#"time.Milliseconds"#),
    // Numbers
    Case::eq(r#"1.5.kb"#, r#"1536"#),
    Case::eq(r#"2.mb.tag"#, r#"core.Bytes"#),
    Case::eq(r#"3.bytes.untagged.tag"#, r#"null"#),
    Case::eq(r#"[3.7.toInt, (-3.7).toInt]"#, r#"[3, -3]"#),
    Case::eq(r#"3.5.days.toInt.tag"#, r#"time.Days"#),
    Case::eq(r#"3.days.negate"#, r#"-3"#),
    Case::eq(r#"3.days.negate.tag"#, r#"time.Days"#),
    Case::eq(r#"1.to 1"#, r#"[1]"#),
    Case::eq(r#"1.to 0"#, r#"[]"#),
    Case::eq(r#"1.until 1"#, r#"[]"#),
    Case::throws(r#"1.to 5 --step=0"#),
    Case::throws(r#"1.5.to 3"#),
    Case::eq(r#"0.times 42"#, r#"[]"#),
    Case::eq(r#"2.power 0.5"#, r#"1.4142135623730951"#),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

const DEFERRED: &[&str] = &[];
