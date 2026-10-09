//! Port of `evaluator/ListMethodsTest.scala`.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    Case::eq(r#"[1, 2, 3].intersect [2, 3, 4]"#, r#"[2, 3]"#),
    Case::eq(r#"[1, 2].append 3"#, r#"[1, 2, 3]"#),
    Case::eq(r#"[1, 2].prepend 0"#, r#"[0, 1, 2]"#),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[];
