//! Port of `evaluator/JoinTest.scala`.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    Case::eq(r#" [1, 2, 3].join(', ') "#, r#" '1, 2, 3' "#),
    Case::eq(r#" [1, 2, 3].join "#, r#" '123' "#),
    Case::eq(r#" join ', ' [1, 2, 3] "#, r#" '1, 2, 3' "#),
    Case::eq(r#" join [1, 2, 3] "#, r#" '123' "#),
    Case::eq(r#" join --sequence=[1, 2, 3] "#, r#" '123' "#),
    Case::eq(r#" join ', ' --sequence=[1, 2, 3] "#, r#" '1, 2, 3' "#),
    Case::eq(r#" join --separator=', ' --sequence=[1, 2, 3] "#, r#" '1, 2, 3' "#),
    Case::eq(r#" join --separator=', ' [1, 2, 3] "#, r#" '1, 2, 3' "#),
    Case::eq(r#" join [] "#, r#" '' "#),
    Case::eq(r#" join 'abc' "#, r#" 'abc' "#),
    Case::eq(r#" join ':' 'abc' "#, r#" 'a:b:c' "#),
    Case::eq(r#" 'abc'.join ':'"#, r#" 'a:b:c' "#),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones (or other modules)
const DEFERRED: &[&str] = &[];
