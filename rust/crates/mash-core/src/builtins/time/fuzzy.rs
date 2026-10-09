//! `DateTime.fuzzy`: "moments ago", "3 days ago", "2 hours from now".
//!
//! Takes the calendar-aware difference in the local zone (so DST and month lengths are respected), picks the
//! largest non-zero unit, and truncates. Under a minute is "moments". (Scala Mash used prettytime, whose
//! rounding is inconsistent: 36 hours is "1 day" but 47 hours is "2 days"; 364 days is "12 months".)

use jiff::{Unit, Zoned};

pub(crate) fn fuzzy(t: &Zoned, now: &Zoned) -> String {
    let past = t <= now;
    let (earlier, later) = if past { (t, now) } else { (now, t) };
    let Ok(span) = earlier.until((Unit::Year, later)) else {
        return if past {
            "a long time ago"
        } else {
            "a long time from now"
        }
        .to_string();
    };
    let days = span.get_days();
    let (n, unit) = if span.get_years() > 0 {
        (i64::from(span.get_years()), "year")
    } else if span.get_months() > 0 {
        (i64::from(span.get_months()), "month")
    } else if days >= 7 {
        (i64::from(days) / 7, "week")
    } else if days > 0 {
        (i64::from(days), "day")
    } else if span.get_hours() > 0 {
        (i64::from(span.get_hours()), "hour")
    } else if span.get_minutes() > 0 {
        (span.get_minutes(), "minute")
    } else {
        return if past { "moments ago" } else { "moments from now" }.to_string();
    };
    let plural = if n == 1 { "" } else { "s" };
    let direction = if past { "ago" } else { "from now" };
    format!("{n} {unit}{plural} {direction}")
}

#[cfg(test)]
mod tests {
    use jiff::civil::date;
    use jiff::tz::TimeZone;
    use jiff::{SignedDuration, ToSpan};

    use super::*;

    fn reference_now() -> Zoned {
        date(2026, 10, 9)
            .at(10, 0, 0, 0)
            .to_zoned(TimeZone::get("Europe/London").unwrap())
            .unwrap()
    }

    fn ago(d: SignedDuration) -> String {
        let now = reference_now();
        fuzzy(&now.checked_sub(d).unwrap(), &now)
    }

    fn ahead(d: SignedDuration) -> String {
        let now = reference_now();
        fuzzy(&now.checked_add(d).unwrap(), &now)
    }

    #[test]
    fn past() {
        assert_eq!(ago(SignedDuration::ZERO), "moments ago");
        assert_eq!(ago(SignedDuration::from_secs(59)), "moments ago");
        assert_eq!(ago(SignedDuration::from_secs(61)), "1 minute ago");
        assert_eq!(ago(SignedDuration::from_mins(45)), "45 minutes ago");
        assert_eq!(ago(SignedDuration::from_mins(90)), "1 hour ago");
        assert_eq!(ago(SignedDuration::from_hours(23)), "23 hours ago");
        assert_eq!(ago(SignedDuration::from_hours(47)), "1 day ago");
        assert_eq!(ago(SignedDuration::from_hours(24 * 6)), "6 days ago");
        assert_eq!(ago(SignedDuration::from_hours(24 * 13)), "1 week ago");
        assert_eq!(ago(SignedDuration::from_hours(24 * 20)), "2 weeks ago");
        assert_eq!(ago(SignedDuration::from_hours(24 * 45)), "1 month ago");
        assert_eq!(ago(SignedDuration::from_hours(24 * 364)), "11 months ago");
        assert_eq!(ago(SignedDuration::from_hours(24 * 366)), "1 year ago");
        assert_eq!(ago(SignedDuration::from_hours(24 * 4000)), "10 years ago");
    }

    #[test]
    fn future() {
        assert_eq!(ahead(SignedDuration::from_secs(5)), "moments from now");
        assert_eq!(ahead(SignedDuration::from_hours(2)), "2 hours from now");
        assert_eq!(ahead(SignedDuration::from_hours(24 * 3)), "3 days from now");
    }

    /// One calendar month back from 9 Oct is 9 Sep, whatever the day count; a calendar day across the
    /// clocks-go-back change is 25 hours
    #[test]
    fn calendar_aware() {
        let now = reference_now();
        assert_eq!(fuzzy(&now.checked_sub(1.month()).unwrap(), &now), "1 month ago");
        let after_dst = "2026-10-25T12:00:00+00:00[Europe/London]".parse::<Zoned>().unwrap();
        let before_dst = after_dst.checked_sub(1.day()).unwrap();
        assert_eq!(
            after_dst.timestamp().duration_since(before_dst.timestamp()),
            SignedDuration::from_hours(25)
        );
        assert_eq!(fuzzy(&before_dst, &after_dst), "1 day ago");
    }
}
