//! Port of `evaluator/PrivateMethodsTest.scala`.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    Case::throws(r#"class A { @private def a = 42 }; A.new.a"#),
    Case::throws(r#"class A { @private def a = 42 }; A.new['a']"#),
    Case::throws(r#"class A { @private def a = 42 }; 'a' A.new"#),
    Case::throws(r#"class A { @private def a = 42 }; [A.new].a"#),
    Case::eq(r#"class A { @private def a = 42; def b = a }; A.new.b"#, r#"42"#),
    Case::eq(r#"class A { @private def a = 42; def b = this.a }; A.new.b"#, r#"42"#),
    Case::eq(
        r#"class A { @private def a = 42; def b = this['a'].invoke }; A.new.b"#,
        r#"42"#,
    ),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[];
