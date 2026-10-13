//! Port of `language/ValueToExpressionTest`: turning values back into Mash source (`mash.expressionFor`).

use std::rc::Rc;

use mash_core::builtins::meta::value_to_expression;
use mash_core::eval::{Ctx, with_big_stack};
use mash_core::platform::TestPlatform;

fn expression(source: &'static str) -> Option<String> {
    with_big_stack(move || {
        let mut ctx = Ctx::new(Rc::new(TestPlatform::default()));
        let value = ctx.run("test", source).expect("evaluates");
        value_to_expression(&value)
    })
}

#[test]
fn converting_a_value_to_an_expression() {
    assert_eq!(expression("null").as_deref(), Some("null"));
    assert_eq!(expression("true").as_deref(), Some("true"));
    assert_eq!(expression("1.23").as_deref(), Some("1.23"));
    assert_eq!(expression("'foo'").as_deref(), Some("\"foo\""));
    assert_eq!(expression("[1, 2, 3]").as_deref(), Some("[1, 2, 3]"));
    assert_eq!(
        expression("{ foo: 42, bar: [1, 2, 3] }").as_deref(),
        Some("{ foo: 42, bar: [1, 2, 3] }")
    );
    assert_eq!(
        expression("{ 10: 1, 'foo-bar': 2, ([1, 2, 3]): 3 }").as_deref(),
        Some("{ 10: 1, \"foo-bar\": 2, ([1, 2, 3]): 3 }")
    );
}

#[test]
fn strings_are_escaped_and_cyclic_values_have_no_expression() {
    assert_eq!(expression("'a\"b`$c'").as_deref(), Some("\"a`\"b`$c\""));
    assert_eq!(expression("x = [1]; x[0] = x; x"), None);
    assert_eq!(expression("x => x"), None);
}

/// New: each evaluates back to the same value (Scala's expanded a leading `~`, had no expression for the
/// special numbers, dropped units, and kept only milliseconds of a date-time)
#[test]
fn expressions_evaluate_to_the_same_value() {
    assert_eq!(expression("'~/x'").as_deref(), Some("\"`~/x\""));
    assert_eq!(
        expression("[1/0, 0 - 1/0, 0/0]").as_deref(),
        Some("[(1 / 0), (-1 / 0), (0 / 0)]")
    );
    assert_eq!(
        expression("[3.days, (0 - 2).hours, 1.5.hours, 1.kb]").as_deref(),
        Some("[3.days, (-2).hours, 1.5.hours, 1024.bytes]")
    );
    assert_eq!(
        expression("'2026-10-13T18:20:01.123456789Z'.toDateTime").as_deref(),
        Some("\"2026-10-13T18:20:01.123456789Z\".toDateTime")
    );
    assert_eq!(
        expression("'2026-10-13'.toDate").as_deref(),
        Some("\"2026-10-13\".toDate")
    );
    let round_trips = with_big_stack(|| {
        let mut ctx = Ctx::new(Rc::new(TestPlatform::default()));
        ctx.run(
            "test",
            "[now, today, '~/x', 1/0, 3.days, [{ a: 2.weeks }]] | all (v => (mash.eval (mash.expressionFor v)) == v)",
        )
        .expect("evaluates")
        .is_truthy()
    });
    assert!(round_trips);
}
