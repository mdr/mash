//! Core builtins not covered by the Scala tests. Expected results were checked against Scala Mash.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    Case::eq(r#"parseNumber " 42 ""#, r#"42"#),
    Case::throws(r#"parseNumber "abc""#),
    Case::throws(r#"try (error "x") --catch=1 --finally=(error "f")"#),
    Case::throws(r#"class P x y; P.bless { x: 1 }"#),
    Case::throws(r#"class P x y; P.bless 3"#),
    Case::throws(r#"Number.bless {}"#),
    Case::eq(r#"Object.parent"#, r#"Any"#),
    Case::eq(r#"Any.parent"#, r#"null"#),
    Case::eq(r#"Number.parent"#, r#"Any"#),
    Case::eq(r#"class A; A.parent"#, r#"Object"#),
    Case::eq(r#"class A; A.fullName"#, r#"'A'"#),
    Case::eq(r#"Number.fullName"#, r#"'core.Number'"#),
    Case::throws(r#"Number.helpForMethod "nope""#),
    Case::eq(
        r#"class P x y; P.helpForField "x" | [.name, .owningClass.name]"#,
        r#"['x', 'P']"#,
    ),
    Case::eq(r#"class P x y; (P 1 2).x? | .getClass.name"#, r#"'FieldHelp'"#),
    Case::eq(r#"identity.summary"#, r#"'Return the argument unchanged'"#),
    Case::eq(r#"(x => x).summary"#, r#"'anonymous function'"#),
    Case::eq(r#"class P x y; P.new.summary"#, r#"'Construct a new P object'"#),
    Case::eq(r#"noArg.getClass.name"#, r#"'NoArg'"#),
    Case::eq(r#"true.not"#, r#"false"#),
    Case::throws(r#""5".isDigit"#),
    Case::eq(r#"safe identity | .invoke [3]"#, r#"3"#),
    Case::eq(r#"parallelMap (_ * 2) [1, 2]"#, r#"[2, 4]"#),
    Case::eq(r#"inBackground (error "x")"#, r#"()"#),
    Case::eq(r#"timeTaken 42 | .result"#, r#"42"#),
    Case::eq(r#"help 42"#, r#"Number"#),
    Case::eq(r#"help identity"#, r#"safe identity"#),
    Case::eq(r#"[1].length? | [.name, .owningClass]"#, r#"['length', List]"#),
    Case::eq(r#"(def foo = 42).invoke"#, r#"42"#),
    Case::eq(r#"not.invoke --namedArgs={ item: true }"#, r#"false"#),
    Case::throws(r#"identity.invoke 1 2"#),
    Case::eq(r#"(safe [1].length).target"#, r#"[1]"#),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

const DEFERRED: &[&str] = &[];
