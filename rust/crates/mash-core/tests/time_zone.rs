//! Date-times shown and read in a time zone other than UTC.

use std::rc::Rc;

use jiff::tz::{Offset, TimeZone};
use mash_core::eval::{Ctx, render, with_big_stack};
use mash_core::platform::TestPlatform;

fn eval_in_plus_one(source: &str) -> String {
    let platform = TestPlatform {
        time_zone: TimeZone::fixed(Offset::constant(1)),
        ..TestPlatform::default()
    };
    let mut ctx = Ctx::new(Rc::new(platform));
    render(&ctx.run("test", source).expect("runs"))
}

#[test]
fn date_times_show_in_the_local_zone_with_its_offset() {
    with_big_stack(|| {
        let cases = [
            ("now.toString", "2026-10-09T13:00:00+01:00"),
            ("[now].toString", "[2026-10-09T13:00:00+01:00]"),
            (
                "'2026-10-09T09:00:00.5-02:00'.toDateTime.toString",
                "2026-10-09T12:00:00.5+01:00",
            ),
            (
                "'2026-10-09T09:00:00.123456Z'.toDateTime.toString",
                "2026-10-09T10:00:00.123+01:00",
            ),
            ("now.toString.toDateTime == now", "true"),
            ("[now.hour, now.day]", "[13, 9]"),
        ];
        for (source, expected) in cases {
            assert_eq!(eval_in_plus_one(source), expected, "{source}");
        }
    });
}
