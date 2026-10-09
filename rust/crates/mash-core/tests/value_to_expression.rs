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
