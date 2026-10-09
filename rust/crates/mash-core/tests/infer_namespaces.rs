//! Type inference rules of the time builtins and the pure namespaces (json, xml, html, base64, hash, maths,
//! random, type, mash, net), from their Scala `typeInferenceStrategy`s. Expected types are written as `Type`'s
//! Debug form, as in tests/infer.rs.

use std::rc::Rc;

use mash_core::ast::SourceUnit;
use mash_core::eval::{Ctx, with_big_stack};
use mash_core::infer;
use mash_core::platform::TestPlatform;

/// (setup to evaluate first, expression, expected type)
const CASES: &[(&str, &str, &str)] = &[
    // time
    ("", "now", "time.DateTime"),
    ("", "today", "time.Date"),
    ("", "time.parseIso8601 '2026-10-09T12:00:00Z'", "time.DateTime"),
    ("", "time.fromMillisSinceEpoch 0", "time.DateTime"),
    ("", "sleep 0", "core.Unit"),
    ("", "now.date", "time.Date"),
    ("", "now.hour", "core.Number"),
    ("", "now.minute", "core.Number"),
    ("", "now.second", "core.Number"),
    ("", "now.millisSinceEpoch", "core.Number"),
    ("", "now.format", "core.String"),
    ("", "now.fuzzy", "core.String"),
    ("", "'2026-10-09'.toDate", "time.Date"),
    ("", "'yesterday'.toDateTime", "time.DateTime"),
    // Unit classes' methods, on a value whose type is known from the environment
    ("d = 3.days", "d.ago", "time.DateTime"),
    ("d = 3.days", "d.fromNow", "time.DateTime"),
    ("d = 3.days", "d.sleep", "core.Unit"),
    // json, xml, html, base64, hash
    ("", "json.fromString '1'", "core.Any"),
    ("", "'{ foo: 42 }' | json.fromString | .foo", "core.Any"),
    ("", "json.prettyPrint 1", "core.String"),
    ("", "xml.fromString '<a/>'", "core.Object"),
    ("", "html.escape '<'", "core.String"),
    ("", "base64.encode 'x'", "core.String"),
    ("", "base64.decode 'eA=='", "core.String"),
    ("", "hash.sha256 'x'", "core.String"),
    // maths, random
    ("", "maths.log 10", "core.Number"),
    ("", "maths.squareRoot 4", "core.Number"),
    ("", "maths.sqrt 4", "core.Number"),
    ("", "maths.stats [1, 2]", "maths.Stats"),
    ("", "(maths.stats [1, 2]).mean", "core.Number"),
    ("", "random.int 0 10", "core.Number"),
    ("", "random.number", "core.Number"),
    ("", "random.uuid", "core.String"),
    // mash
    ("", "mash.version", "core.String"),
    ("", "mash.expressionFor 42", "core.String"),
    // net
    ("", "net.url 'http://example.com'", "core.String tagged net.Url"),
    ("", "(net.url 'http://example.com').host", "core.String tagged net.Host"),
    (
        "",
        "(net.url 'http://example.com').withQueryParams { a: 1 }",
        "core.String tagged net.Url",
    ),
    ("", "net.urlEncode 'a b'", "core.String"),
    // type.hint: a class, a list of one, an object of hints, a user class; otherwise the item's type
    ("", "type.hint String 42", "core.String"),
    ("", "type.hint [String] 42", "[core.String]"),
    (
        "",
        "type.hint { name: String, ages: [Number] } 42",
        "{name: core.String, ages: [core.Number]}",
    ),
    ("", "type.hint { name: 42 } 42", "{name: core.Any}"),
    ("", "class Bob { def n = 42 }; 1 | type.hint Bob | .n", "core.Number"),
    (
        "",
        "class Bob { def n = 42 }; 1 | type.hint [Bob] | .n",
        "[core.Number]",
    ),
    ("class Bob", "type.hint Bob 1", "UserClassInstance(Bob)"),
    ("", "type.hint 'not a type' 42", "core.Number"),
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
fn namespace_inference_rules() {
    let failures = with_big_stack(|| {
        let mut failures = vec![];
        for (setup, input, expected) in CASES {
            let mut ctx = Ctx::new(Rc::new(TestPlatform::default()));
            if !setup.is_empty() {
                ctx.run("setup", setup).expect("setup runs");
            }
            let actual = inferred_type(&mut ctx, input);
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
