//! The pure namespaces (json, xml, html, base64, hash, maths, random, mash, net) and Object methods, beyond
//! what the Scala tests cover. Expected results were checked against Scala Mash, except where noted.

use crate::harness::*;

pub(crate) const CASES: &[Case] = &[
    // json
    Case::eq(
        r#"json.fromString '{"a": [1, 2.5, true, null, "x"]}'"#,
        r#"{ a: [1, 2.5, true, null, 'x'] }"#,
    ),
    Case::throws(r#"json.fromString '[1'"#),
    // Deliberate: Scala's JSON parser was lenient and accepted unquoted field names
    Case::throws(r#"json.fromString '{a: 1}'"#),
    Case::eq(r#"json.prettyPrint --compact [1.5, "a", null]"#, r#"'[1.5,"a",null]'"#),
    Case::eq(r#"json.prettyPrint --compact { (1): 2 }"#, r#"'{"1":2}'"#),
    Case::eq(r#"json.prettyPrint {}"#, r#"'{}'"#),
    Case::eq(r#"json.prettyPrint []"#, r#"'[]'"#),
    // Deliberate: Scala wrote `NaN`, which isn't JSON
    Case::throws(r#"json.prettyPrint (0 / 0)"#),
    Case::throws(r#"x = [1]; x[0] = x; json.prettyPrint x"#),
    Case::eq(
        r#"json.fromString (json.prettyPrint { a: [1, { b: null }] })"#,
        r#"{ a: [1, { b: null }] }"#,
    ),
    // xml (deliberate: text and attribute values stay strings, where Scala converted `1` to a number)
    Case::eq(
        r#"xml.fromString '<a x="1"><b>2</b><b>3</b><c/></a>'"#,
        r#"{ a: { x: '1', b: ['2', '3'], c: '' } }"#,
    ),
    Case::eq(
        r#"xml.fromString '<a>text<b>x</b></a>'"#,
        r#"{ a: { content: 'text', b: 'x' } }"#,
    ),
    Case::eq(r#"xml.fromString '<a><![CDATA[<x>]]></a>'"#, r#"{ a: '<x>' }"#),
    Case::eq(r#"xml.fromString '<a>1</a><b/>'"#, r#"{ a: '1', b: '' }"#),
    Case::eq(
        r#"xml.fromString '<?xml version="1.0"?><!-- c --><a>&lt;&amp;</a>'"#,
        r#"{ a: '<&' }"#,
    ),
    Case::throws(r#"xml.fromString '<a>'"#),
    // html, base64, hash
    Case::eq(
        r#"html.escape '<a href="x">&</a>'"#,
        r#"'&lt;a href=&quot;x&quot;&gt;&amp;&lt;/a&gt;'"#,
    ),
    Case::eq(r#"base64.encode 'é'"#, r#"'w6k='"#),
    Case::eq(r#"base64.decode 'w6k='"#, r#"'é'"#),
    Case::throws(r#"base64.decode '!!'"#),
    Case::eq(
        r#"hash.sha256 42"#,
        r#"'73475cb40a568e8da8a045ced110137e159f890ac4da883b6b17dc651b3a8049'"#,
    ),
    // maths
    Case::eq(
        r#"maths.stats [1, 2, 3, 4] | [.min, .max, .mean, .median, .count]"#,
        r#"[1, 4, 2.5, 2.5, 4]"#,
    ),
    Case::eq(r#"(maths.stats [3, 1, 2]).standardDeviation"#, r#"1"#),
    Case::eq(r#"(maths.stats [5]).standardDeviation"#, r#"0"#),
    Case::eq(r#"(maths.stats []).count"#, r#"0"#),
    Case::eq(r#"(maths.stats [1]).getClass"#, r#"maths.Stats"#),
    Case::throws(r#"maths.stats 'abc'"#),
    Case::throws(r#"maths.stats [1, 'a']"#),
    Case::eq(r#"maths.log 8 --base=2"#, r#"3"#),
    Case::eq(r#"maths.squareRoot 9"#, r#"3"#),
    Case::eq(r#"maths.sqrt 9"#, r#"3"#),
    // random (deterministic in tests, so just check ranges and shape)
    Case::eq(
        r#"[1, 2, 3, 4, 5, 6, 7, 8] | map (_ => random.int 5) | all (n => n >= 0 and n < 5)"#,
        r#"true"#,
    ),
    Case::eq(
        r#"[1, 2, 3] | map (_ => random.int 2 3 --inclusive) | all (n => n == 2 or n == 3)"#,
        r#"true"#,
    ),
    Case::eq(r#"x = random.number; x >= 0 and x < 1"#, r#"true"#),
    Case::eq(
        r#"u = random.uuid; [u[8], u[13], u[14], u[18], u[23]]"#,
        r#"['-', '-', '4', '-', '-']"#,
    ),
    // mash (eval isn't checked against Scala: it needs the REPL there)
    Case::eq(r#"mash.eval '1 + 2'"#, r#"3"#),
    Case::eq(r#"mash.eval 'evalVar = 5'; evalVar"#, r#"5"#),
    Case::throws(r#"mash.eval '1 +'"#),
    Case::eq(r#"mash.version.isA String"#, r#"true"#),
    Case::eq(r#"mash.expressionFor {}"#, r#"'{  }'"#),
    Case::eq(
        r#"mash.expressionFor { (null): 1, true: 2 }"#,
        r#"'{ null: 1, true: 2 }'"#,
    ),
    Case::eq(r#"mash.expressionFor (x => x)"#, r#"null"#),
    // net
    Case::throws(r#"net.url 'a b'"#),
    Case::eq(r#"net.url 'foo' | .host"#, r#"null"#),
    Case::eq(r#"net.url 'http://u@[::1]:80/x' | .host"#, r#"'[::1]'"#),
    Case::eq(r#"(net.url 'http://example.com' | .host).isA net.Host"#, r#"true"#),
    Case::eq(
        r#"net.url 'http://example.com?a=1#f' | .withQueryParams { 'b c': 'd&e' }"#,
        r#"'http://example.com?a=1&b+c=d%26e#f'"#,
    ),
    Case::eq(r#"net.urlEncode 'a b~é*'"#, r#"'a+b%7E%C3%A9*'"#),
    // Object methods
    Case::throws(r#"{ a: 1 }.bless 3"#),
    Case::eq(r#"{ a: 1 }.bless Number | .getClass"#, r#"Number"#),
    Case::throws(r#"[[1]] | Object.fromPairs"#),
    Case::throws(r#"[1] | Object.fromPairs"#),
    Case::throws(r#"[[1, 2, 3]] | Object.fromPairs 0 5"#),
    Case::throws(r#"{ foo: 42 }.hoist 'foo'"#),
    Case::throws(r#"{ foo: 42 }.hoist 'bar'"#),
    Case::throws(r#"Object.merge 1"#),
    Case::eq(r#"{ b: 1, a: 2 }.sortBy (f v => v) | .fields.name"#, r#"['b', 'a']"#),
    Case::eq(r#"{ a: 1, b: 2 }.first"#, r#"{ a: 1 }"#),
    Case::eq(r#"{ a: 1, b: 2 }.reverse | .fields.name"#, r#"['b', 'a']"#),
    Case::eq(r#"(core.FieldAndValue.new 'a' 1).value"#, r#"1"#),
];

#[test]
fn cases() {
    run(CASES, DEFERRED);
}

const DEFERRED: &[&str] = &[];
