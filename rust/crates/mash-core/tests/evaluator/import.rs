//! Port of `evaluator/ImportTest.scala`.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    Case::eq(r#"import hash.sha256; sha256"#, r#"sha256"#),
    Case::eq(r#"import hash._; sha256"#, r#"sha256"#),
    Case::eq(r#"obj = { foo: 42 }; import obj.foo; foo"#, r#"42"#),
    Case::eq(
        r#"class A n { def inc = n += 1 }; a = A 0; import a._; inc; a.n"#,
        r#"1"#,
    ),
    Case::eq(r#"x = {}; import x._; where.getClass"#, r#"Function"#),
    Case::eq(r#"x = {}; import x._; isNull.getClass"#, r#"Function"#),
    Case::eq(r#"x = {}; import x.where; where.getClass"#, r#"BoundMethod"#),
    Case::throws(r#"class A { @private def method = 42 }; a = A.new; import a.method"#),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[];
