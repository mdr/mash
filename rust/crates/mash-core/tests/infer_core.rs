//! Type inference and completion specs for the core builtins (ports of their Scala `typeInferenceStrategy` and
//! `getCompletionSpecs`). Expected types are written as `Type`'s Debug form, as in tests/infer.rs.

use std::rc::Rc;

use mash_core::ast::SourceUnit;
use mash_core::builtins::core::{OBJECT_CLASS, ParallelMapFunction};
use mash_core::complete::CompletionSpec;
use mash_core::eval::{Ctx, with_big_stack};
use mash_core::function::BuiltinFunction;
use mash_core::infer::{self, Type, TypedArgs};
use mash_core::platform::TestPlatform;

const CASES: &[(&str, &str)] = &[
    // core functions
    ("identity 42", "core.Number"),
    ("try 42", "core.Number"),
    ("try 'a' --catch=1", "core.String"),
    ("timeTaken 42", "core.TimedResult[core.Number]"),
    ("timeTaken 42 | .result", "core.Number"),
    ("(timeTaken 42).duration", "core.Number tagged time.Milliseconds"),
    ("(timeTaken 42).finished", "time.DateTime"),
    ("parseNumber '1'", "core.Number"),
    ("not 1", "core.Boolean"),
    ("tap (x => x) 1", "core.Boolean"),
    ("while false 1", "core.Unit"),
    ("print 1 | forever", "core.Unit"),
    ("'a' | timeout 5", "core.String"),
    ("split 'a b'", "[core.String]"),
    ("split \"a b\"", "[core.String tagged os.Path]"),
    ("parallelMap (_ * 2) [1, 2]", "[core.Number]"),
    ("parallelMap (_.toUpper) 'ab'", "core.String"),
    ("parallelMap (_.toNumber) '12'", "[core.Number]"),
    ("core.thread.inBackground 1", "core.Unit"),
    ("help identity", "core.Function"),
    ("help 'x'.startsWith", "core.help.MethodHelp"),
    ("help Object", "core.Class"),
    ("class A; help A", "core.Class"),
    // Any
    ("42.isNull", "core.Boolean"),
    ("42.isTruthy", "core.Boolean"),
    ("42.isA Number", "core.Boolean"),
    ("42.in [1]", "core.Boolean"),
    ("42.getClass", "core.Class"),
    ("42.toString", "core.String"),
    // String
    ("'abc'.toUpper", "core.String"),
    ("\"abc\".toUpper", "core.String tagged os.Path"),
    ("\"abc\".trim", "core.String tagged os.Path"),
    ("'abc'.replace 'a' 'b'", "core.String"),
    ("'abc'.deleteUpTo 'b'", "core.String"),
    ("'a b'.split", "[core.String]"),
    ("\"a b\".split", "[core.String tagged os.Path]"),
    ("'a'.lines", "[core.String]"),
    ("'a'.grep 'a'", "[core.String]"),
    ("'a'.grep --first 'a'", "core.String"),
    ("'abc'.r", "core.String tagged core.Regex"),
    ("'abc'.regex.match 'a'", "core.Match"),
    ("'abc'.toList", "[core.String tagged core.Character]"),
    ("'a'.toCharacter", "core.String tagged core.Character"),
    ("'a'.toPath", "core.String tagged os.Path"),
    ("'a'.toNumber", "core.Number"),
    ("'a'.untagged", "core.String"),
    ("'a'.tag", "core.Class"),
    ("'a'.startsWith 'a'", "core.Boolean"),
    ("'a'.indexOf 'a'", "core.Number"),
    ("'a'[0].isDigit", "core.Boolean"),
    // Number
    ("3.days", "core.Number tagged time.Days"),
    ("3.second", "core.Number tagged time.Seconds"),
    ("5.bytes", "core.Number tagged core.Bytes"),
    ("5.kb.toInt", "core.Number tagged core.Bytes"),
    ("5.bytes.negate", "core.Number tagged core.Bytes"),
    ("5.bytes.untagged", "core.Number"),
    ("1.to 3", "[core.Number]"),
    ("1.until 3", "[core.Number]"),
    ("3.times 'x'", "[core.String]"),
    ("2.power 3", "core.Number"),
    ("3.tag", "core.Class"),
    // Object
    ("{ a: 1 }.get 'a'", "core.Number"),
    ("{ a: 1 }.get 'b' --default='x'", "core.String"),
    ("{ a: 1 }.withField 'b' 'x'", "{a: core.Number, b: core.String}"),
    ("{ a: 1, b: { c: 'x' } }.hoist 'b'", "{a: core.Number, c: core.String}"),
    (
        "{ a: 1, b: [{ c: 'x' }] }.hoist 'b'",
        "[{a: core.Number, c: core.String}]",
    ),
    ("{ a: 1 }.fields", "[core.FieldAndValue]"),
    ("{ a: 1 }.values", "[core.Any]"),
    ("{ a: 1 }.hasField 'a'", "core.Boolean"),
    ("{ a: 1 }.where (_)", "core.Object"),
    ("{ a: 1 }.map (_)", "core.Object"),
    ("class P x; { x: 1 }.bless P", "UserClassInstance(P)"),
    ("{ name: 'n', value: 1 }.bless core.FieldAndValue", "core.FieldAndValue"),
    ("class P x; (P 1).unbless", "{x: core.Any}"),
    (
        "({ name: 'n', value: 1 }.bless core.FieldAndValue).unbless",
        "{name: core.Any, value: core.Any}",
    ),
    // Class, Function, BoundMethod, Boolean
    ("class P x; P.bless { x: 1 }", "UserClassInstance(P)"),
    ("Object.name", "core.String"),
    ("Object.fullName", "core.String"),
    ("Object.parent", "core.Class"),
    ("Object.helpForMethod 'get'", "core.help.MethodHelp"),
    ("Object.helpForField 'x'", "core.help.FieldHelp"),
    ("identity.summary", "core.String"),
    ("'a'.startsWith.summary", "core.String"),
    ("'a'.startsWith.help", "core.help.MethodHelp"),
    ("'a'.startsWith.target", "core.String"),
    ("\"a\".startsWith.target", "core.String tagged os.Path"),
    ("true.not", "core.Boolean"),
];

fn inferred_type(ctx: &mut Ctx, input: &str) -> String {
    let compiled = ctx.compile_forgiving(SourceUnit::new("test", input));
    let inferred = infer::infer(ctx, &compiled.program.body);
    match inferred.types.get(&compiled.program.body.id) {
        Some(ty) => format!("{ty:?}"),
        None => "<no type>".into(),
    }
}

#[test]
fn core_inference() {
    let failures = with_big_stack(|| {
        let mut failures = vec![];
        for (input, expected) in CASES {
            let actual = inferred_type(&mut Ctx::new(Rc::new(TestPlatform::default())), input);
            if actual != *expected {
                failures.push(format!("{input:?}: got {actual}, expected {expected}"));
            }
        }
        failures
    });
    assert!(
        failures.is_empty(),
        "{} failing case(s):\n  {}",
        failures.len(),
        failures.join("\n  ")
    );
}

fn items(specs: &[CompletionSpec]) -> Vec<Vec<String>> {
    specs
        .iter()
        .map(|s| match s {
            CompletionSpec::Items(items) => items.clone(),
            other => panic!("unexpected spec {other:?}"),
        })
        .collect()
}

#[test]
fn object_get_and_hoist_complete_field_names() {
    let target = Type::object([("a".into(), Type::NUMBER), ("b".into(), Type::STRING)]);
    let args = TypedArgs::positional([Type::STRING]);
    for name in ["get", "hoist"] {
        let method = OBJECT_CLASS.method(name).unwrap();
        assert_eq!(
            items(&method.completion_specs(0, Some(&target), &args)),
            [vec!["a", "b"]],
            "{name}"
        );
        assert!(method.completion_specs(0, None, &args).is_empty(), "{name}");
    }
    // Only the field name argument
    let get = OBJECT_CLASS.method("get").unwrap();
    let with_default = TypedArgs::positional([Type::STRING, Type::NUMBER]);
    assert!(get.completion_specs(1, Some(&target), &with_default).is_empty());
}

#[test]
fn parallel_map_completes_members_of_the_elements() {
    let element = Type::object([("foo".into(), Type::NUMBER)]);
    let args = TypedArgs::positional([Type::STRING, Type::seq(element.clone())]);
    match ParallelMapFunction.completion_specs(0, &args).as_slice() {
        [CompletionSpec::Members(t)] => assert_eq!(*t, element),
        other => panic!("{other:?}"),
    }
    assert!(ParallelMapFunction.completion_specs(1, &args).is_empty());
}
