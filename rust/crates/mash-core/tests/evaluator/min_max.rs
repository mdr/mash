//! Port of `evaluator/MinMaxTest.scala`.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    Case::eq(r#"max [1, 200, 3]"#, r#"200"#),
    Case::eq(r#"[1, 200, 3].max"#, r#"200"#),
    Case::eq(r#"max 'a' 'b' 'c'"#, r#"'c'"#),
    Case::eq(r#"'abc'.max"#, r#"'c'"#),
    Case::eq(r#"max 1 2 3"#, r#"3"#),
    Case::eq(r#"max [2, null, 1]"#, r#"2"#),
    Case::eq(r#"max [] --default=0"#, r#"0"#),
    Case::eq(r#"[].max --default=0"#, r#"0"#),
    Case::eq(r#"min --items=['abc']"#, r#"'abc'"#),
    Case::eq(r#"min --items=['abc', 'def']"#, r#"'abc'"#),
    Case::eq(r#"maxBy length [ 'a', 'bbb', 'cc'] "#, r#" 'bbb' "#),
    Case::eq(r#"maxBy (_) 'abcde'"#, r#"'e'"#),
    Case::eq(r#"maxBy 'foo' [{ foo: null }, { foo: 42 }]"#, r#"{ foo: 42 }"#),
    Case::eq(r#"'abcde'.maxBy (_)"#, r#"'e'"#),
    Case::eq(r#"maxBy length [] --default=0"#, r#"0"#),
    Case::eq(r#"[].maxBy length --default=0"#, r#"0"#),
    Case::eq(r#"min [100, 2, 300]"#, r#"2"#),
    Case::eq(r#"[100, 2, 300].min"#, r#"2"#),
    Case::eq(r#"min 'a' 'b' 'c'"#, r#"'a'"#),
    Case::eq(r#"'abc'.min"#, r#"'a'"#),
    Case::eq(r#"min 1 2 3"#, r#"1"#),
    Case::eq(r#"min [2, null, 1]"#, r#"1"#),
    Case::eq(r#"min [] --default=0"#, r#"0"#),
    Case::eq(r#"[].min --default=0"#, r#"0"#),
    Case::throws(r#"[].min [1, 2, 3]"#),
    Case::eq(r#"a = now; b = 3.days.ago; min a b"#, r#"b"#),
    Case::eq(r#"minBy length [ 'a', 'bbb', 'cc'] "#, r#" 'a' "#),
    Case::eq(r#"minBy 'foo' [{ foo: null }, { foo: 42 }]"#, r#"{ foo: 42 }"#),
    Case::eq(r#"minBy length [] --default=0"#, r#"0"#),
    Case::eq(r#"[].minBy length --default=0"#, r#"0"#),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[];
