//! Port of `evaluator/NamedArgsTest.scala`.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    Case::eq(
        r#"def makeObject (@namedArgs namedArgs) = namedArgs; makeObject --foo=1"#,
        r#"{ foo: 1 }"#,
    ),
    Case::eq(
        r#"def makeObject (@namedArgs namedArgs) = namedArgs; makeObject --foo"#,
        r#"{ foo: true }"#,
    ),
    Case::eq(
        r#"def makeObject (@namedArgs namedArgs) = namedArgs; makeObject -a"#,
        r#"{ a: true }"#,
    ),
    Case::eq(
        r#"def makeObject (@namedArgs namedArgs) = namedArgs; makeObject -abc"#,
        r#"{ a: true, b: true, c: true }"#,
    ),
    Case::eq(
        r#"def makeObject (@namedArgs namedArgs) = namedArgs; makeObject"#,
        r#"{}"#,
    ),
    Case::eq(
        r#"def makeObject (@namedArgs namedArgs) (@flag otherArg) = { namedArgs, otherArg }; makeObject --otherArg=10"#,
        r#"{ namedArgs: {}, otherArg: 10 }"#,
    ),
    Case::eq(
        r#"def makeObject otherParam (@namedArgs namedArgs) = [otherParam, namedArgs]; makeObject 1 --foo=2"#,
        r#"[1, { foo: 2 }]"#,
    ),
    Case::eq(
        r#"def makeObject otherParam (@namedArgs namedArgs) = [otherParam, namedArgs]; makeObject --foo=2 --otherParam=1"#,
        r#"[1, { foo: 2 }]"#,
    ),
    Case::eq(
        r#"def makeObject (@namedArgs namedArgs) = namedArgs; makeObject --namedArgs=42"#,
        r#"{ namedArgs: 42 }"#,
    ),
    Case::eq(
        r#"def makeObject varargs... (@namedArgs namedArgs) = { varargs, namedArgs }; makeObject 1 2 3 --foo=42"#,
        r#"{ varargs: [1, 2, 3], namedArgs: { foo: 42} }"#,
    ),
    Case::throws(r#"def makeObject (@namedArgs namedArgs) = namedArgs; makeObject 1"#),
    Case::throws(r#"def makeObject (@namedArgs namedArgs) = namedArgs; makeObject --arg --arg"#),
    Case::throws(r#"def makeObject (@namedArgs namedArgs) = namedArgs; makeObject -aa"#),
    Case::throws(r#"def makeObject arg (@namedArgs namedArgs) = namedArgs; makeObject 42 --arg=100"#),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

/// Cases that need builtins from later milestones
const DEFERRED: &[&str] = &[];
