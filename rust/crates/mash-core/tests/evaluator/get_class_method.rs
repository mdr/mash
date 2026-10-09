//! Port of `evaluator/GetClassMethodTest.scala`.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    Case::eq(r#"1.getClass"#, r#"core.Number"#),
    Case::eq(r#"true.getClass"#, r#"core.Boolean"#),
    Case::eq(r#"'foo'.getClass"#, r#"core.String"#),
    Case::eq(r#"now.getClass"#, r#"time.DateTime"#),
    Case::eq(r#"now.date.getClass"#, r#"time.Date"#),
    Case::eq(r#"().getClass"#, r#"core.Unit"#),
    Case::eq(r#"null.getClass"#, r#"core.Null"#),
    Case::eq(r#"{}.getClass"#, r#"core.Object"#),
    Case::eq(r#"[].getClass"#, r#"collections.List"#),
    Case::eq(r#"identity.getClass"#, r#"core.Function"#),
    Case::eq(r#"'foo'.startsWith.getClass"#, r#"core.BoundMethod"#),
    Case::eq(r#"1.getClass.getClass"#, r#"core.Class"#),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[
    // No binding for 'now'
    r#"now.getClass"#,
    // No binding for 'now'
    r#"now.date.getClass"#,
    // Cannot find member 'startsWith' in value of type String
    r#"'foo'.startsWith.getClass"#,
];
