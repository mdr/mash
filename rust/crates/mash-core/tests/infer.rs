//! Type inference. Port of `inference/TypeInferencerTest.scala`: each case infers the type of an expression
//! (compiled forgivingly, in a fresh interpreter's global environment) and compares it with the expected type,
//! written as `Type`'s Debug form (`core.Number`, `[core.String]`, `{foo: core.Number}`, `core.String tagged
//! os.Path`, `collections.Group[…]`, `UserClassInstance(Point)`).

use std::rc::Rc;

use mash_core::ast::SourceUnit;
use mash_core::eval::{Ctx, with_big_stack};
use mash_core::infer;
use mash_core::platform::TestPlatform;

#[derive(Debug)]
enum Case {
    Infers(&'static str, &'static str),
    /// Cases run in an interpreter where the setup code has been evaluated first
    Setup(&'static str, &'static [Case]),
}

impl Case {
    const fn infers(input: &'static str, expected: &'static str) -> Case {
        Case::Infers(input, expected)
    }

    const fn setup(setup: &'static str, cases: &'static [Case]) -> Case {
        Case::Setup(setup, cases)
    }
}

const CASES: &[Case] = &[
    Case::infers(r#"42"#, r#"core.Number"#),
    Case::infers(r#"{ foo: 42 }.foo"#, r#"core.Number"#),
    Case::infers(r#"{ foo: 42 }"#, r#"{foo: core.Number}"#),
    Case::infers(r#"foo = 42; { foo }"#, r#"{foo: core.Number}"#),
    Case::infers(r#"def foo = 42; { foo }"#, r#"{foo: core.Number}"#),
    Case::infers(
        r#"{ bar: '{ foo : 42 }' | json.fromString | .foo }"#,
        r#"{bar: core.Any}"#,
    ),
    Case::infers(r#"[{ foo: 42 }] | map (_.foo) | first"#, r#"core.Number"#),
    Case::infers(r#"map [1, 2, 3].map [(_ * 2), (_ * _)]"#, r#"[[core.Number]]"#),
    Case::infers(r#"(_.foo) { foo: 42 }"#, r#"core.Number"#),
    Case::infers(
        r#"(_1 + _2) { foo: 42 } { bar: 128 }"#,
        r#"{foo: core.Number, bar: core.Number}"#,
    ),
    Case::infers(
        r#"(boundMethod => [(_ * 2), (_ * _)].map boundMethod) [1, 2, 3].map"#,
        r#"[[core.Number]]"#,
    ),
    Case::infers(r#"[1, 2, 3][0]"#, r#"core.Number"#),
    Case::infers(r#"[1, 2, 3] 0"#, r#"core.Number"#),
    Case::infers(r#" 'foo'[0] "#, r#"core.String tagged core.Character"#),
    Case::infers(r#" 'foo' + 'bar' "#, r#"core.String"#),
    Case::infers(r#" "foo" + "bar" "#, r#"core.String tagged os.Path"#),
    Case::infers(r#" 'foo' + 42 "#, r#"core.String"#),
    Case::infers(r#" 42 + 'bar' "#, r#"core.String"#),
    Case::infers(r#" 'foo'.untagged + 'bar'.untagged"#, r#"core.String"#),
    Case::infers(r#"42 + 24"#, r#"core.Number"#),
    Case::infers(r#"[1] + [2]"#, r#"[core.Number]"#),
    Case::infers(
        r#"{ foo: 42 } + { bar: 100 }"#,
        r#"{foo: core.Number, bar: core.Number}"#,
    ),
    Case::infers(r#"ls.first + { foo: 42 }"#, r#"os.PathSummary"#),
    Case::infers(r#"ls.first + ls.first"#, r#"os.PathSummary"#),
    Case::infers(r#"ls.first + ls.first.permissions"#, r#"os.Permissions"#),
    Case::infers(
        r#"class Point x y; class Rectangle w h; Point 1 2 + Rectangle 3 4"#,
        r#"UserClassInstance(Rectangle)"#,
    ),
    Case::infers(
        r#"class Point x y; ls.first + Point 1 2"#,
        r#"UserClassInstance(Point)"#,
    ),
    Case::infers(r#"class Point x y; Point 1 2 + ls.first"#, r#"os.PathSummary"#),
    Case::infers(
        r#"class Point x y; Point 1 2 + { foo: 42 }"#,
        r#"UserClassInstance(Point)"#,
    ),
    Case::infers(r#"2 - 1"#, r#"core.Number"#),
    Case::infers(r#"{ foo: 42, bar: 100 } - 'foo'"#, r#"{bar: core.Number}"#),
    Case::infers(
        r#"{ foo: 42, bar: 100, baz: 128 } - ['foo', 'baz']"#,
        r#"{bar: core.Number}"#,
    ),
    Case::infers(r#"class Point x y; Point 1 2 - 'x'"#, r#"{y: core.Any}"#),
    Case::infers(r#"[1, 2, 3] - [2]"#, r#"[core.Number]"#),
    Case::infers(r#"2 * 2"#, r#"core.Number"#),
    Case::infers(r#"'foo' * 2"#, r#"core.String"#),
    Case::infers(r#"2 * 'foo'"#, r#"core.String"#),
    Case::infers(r#" "foo" * 2 "#, r#"core.String tagged os.Path"#),
    Case::infers(r#"[1, 2] * 3"#, r#"[core.Number]"#),
    Case::infers(r#"3 * [1, 2]"#, r#"[core.Number]"#),
    Case::infers(r#"5 * 1.seconds"#, r#"core.Number tagged time.Seconds"#),
    Case::infers(r#"1.seconds * 5"#, r#"core.Number tagged time.Seconds"#),
    Case::infers(r#"[true].map 'not' "#, r#"[core.Boolean]"#),
    Case::infers(r#"['f', 'g'].map ('foo'.startsWith)"#, r#"[core.Boolean]"#),
    Case::infers(r#"['f', 'g'].map 'foo'.startsWith"#, r#"[core.Boolean]"#),
    Case::infers(r#"[ (_ * 2), (_ + 1) ] | map (_ 10)"#, r#"[core.Number]"#),
    Case::infers(r#"map --f=(_ * 2) --sequence=[1, 2, 3]"#, r#"[core.Number]"#),
    Case::infers(r#"map --f=(_ * 2) [1, 2, 3]"#, r#"[core.Number]"#),
    Case::infers(r#"map (_.toUpper) 'foo'"#, r#"core.String"#),
    Case::infers(r#"map (x => x x) [1, 2, 3]"#, r#"[core.Any]"#),
    Case::infers(r#"map (_.toNumber) '123'"#, r#"[core.Number]"#),
    Case::infers(r#"'[1, 2, 3]' | json.fromString | map (x => 2)"#, r#"[core.Number]"#),
    Case::infers(r#"[1, 2, 3] | parallelMap (x => x)"#, r#"[core.Number]"#),
    Case::infers(r#"[1].flatMap (n => [n.toString])"#, r#"[core.String]"#),
    Case::infers(r#"unknown | flatMap (a => [42])"#, r#"[core.Number]"#),
    Case::infers(r#"42.toString"#, r#"core.String"#),
    Case::infers(r#"ls.first.children"#, r#"[os.PathSummary]"#),
    Case::infers(r#"[]"#, r#"[core.Any]"#),
    Case::infers(r#"[].join"#, r#"core.String"#),
    Case::infers(r#"user.groups"#, r#"[core.String tagged os.Group]"#),
    Case::infers(r#"user.primaryGroup"#, r#"core.String tagged os.Group"#),
    Case::infers(r#"user.primaryGroup.gid"#, r#"core.Number tagged os.Gid"#),
    Case::infers(r#"[1, 2, 3] | grep 2"#, r#"[core.Number]"#),
    Case::infers(r#"[1, 2, 3].grep 2"#, r#"[core.Number]"#),
    Case::infers(r#"'foo`nbar' | grep 'b'"#, r#"[core.String]"#),
    Case::infers(r#"'foo`nbar'.grep 'b'"#, r#"[core.String]"#),
    Case::infers(r#"{ foo: 42 } | grep 'f'"#, r#"core.Object"#),
    Case::infers(r#"{ foo: 42 }.grep 'f'"#, r#"core.Object"#),
    Case::infers(r#"[1, 2, 3] | grep --first 2"#, r#"core.Number"#),
    Case::infers(r#"[1, 2, 3].grep --first 2"#, r#"core.Number"#),
    Case::infers(r#"'foo`nbar' | grep --first 'b'"#, r#"core.String"#),
    Case::infers(r#"'foo`nbar'.grep --first 'b'"#, r#"core.String"#),
    Case::infers(r#"{ foo: 42 } | grep --first 'f'"#, r#"core.Object"#),
    Case::infers(r#"{ foo: 42 }.grep --first 'f'"#, r#"core.Object"#),
    Case::infers(r#"[1, 2, 3] | first"#, r#"core.Number"#),
    Case::infers(r#"[1, 2, 3].first"#, r#"core.Number"#),
    Case::infers(r#"[1, 2, 3] | first 2"#, r#"[core.Number]"#),
    Case::infers(r#"[1, 2, 3].first 2"#, r#"[core.Number]"#),
    Case::infers(r#"'foo' | first"#, r#"core.String"#),
    Case::infers(r#"'foo' | first 2"#, r#"core.String"#),
    Case::infers(r#"'foo'.first"#, r#"core.String"#),
    Case::infers(r#"'foo'.first 2"#, r#"core.String"#),
    Case::infers(
        r#"{ a: 1, b: 2, c: 3 } | first 3"#,
        r#"{a: core.Number, b: core.Number, c: core.Number}"#,
    ),
    Case::infers(r#"[1, 2, 3] | last"#, r#"core.Number"#),
    Case::infers(r#"[1, 2, 3] | last 2"#, r#"[core.Number]"#),
    Case::infers(r#"[1, 2, 3].last"#, r#"core.Number"#),
    Case::infers(r#"[1, 2, 3].last 2"#, r#"[core.Number]"#),
    Case::infers(r#"last --n=2 --sequence=[1, 2, 3]"#, r#"[core.Number]"#),
    Case::infers(r#"'foo' | last"#, r#"core.String"#),
    Case::infers(r#"'foo' | last 2"#, r#"core.String"#),
    Case::infers(r#"'foo'.last"#, r#"core.String"#),
    Case::infers(r#"'foo'.last 2"#, r#"core.String"#),
    Case::infers(
        r#"{ a: 1, b: 2, c: 3 } | last 3"#,
        r#"{a: core.Number, b: core.Number, c: core.Number}"#,
    ),
    Case::infers(r#"[1, 2, 3].indexOf 2"#, r#"core.Number"#),
    Case::infers(r#"'abc'.indexOf 'b'"#, r#"core.Number"#),
    Case::infers(r#"{ foo: 42 } | select 'foo' "#, r#"{foo: core.Number}"#),
    Case::infers(r#"[{ foo: 42 }] | select 'foo' "#, r#"[{foo: core.Number}]"#),
    Case::infers(
        r#"{ foo: 42 } | select --add --bar=(_.foo * 2) "#,
        r#"{foo: core.Number, bar: core.Number}"#,
    ),
    Case::infers(
        r#"{ foo: 42, bar: 24 } | select --foo 'bar' "#,
        r#"{foo: core.Number, bar: core.Number}"#,
    ),
    Case::infers(r#"{ foo: 42 } | select --bar='foo'"#, r#"{bar: core.Number}"#),
    Case::infers(r#"{ foo: 42 } | select --bar=(_.foo * 2)"#, r#"{bar: core.Number}"#),
    Case::infers(r#"reverse [42]"#, r#"[core.Number]"#),
    Case::infers(r#"reverse 'abc'"#, r#"core.String"#),
    Case::infers(r#"[1, 2, 3] | where (_ > 2)"#, r#"[core.Number]"#),
    Case::infers(r#"'foo' | where (_ > 'm')"#, r#"core.String"#),
    Case::infers(r#"'[1, 2, 3]' | json.fromString | where (_ > 2)"#, r#"[core.Any]"#),
    Case::infers(r#"null"#, r#"core.Null"#),
    Case::infers(r#"[1.bytes] | sum"#, r#"core.Number tagged core.Bytes"#),
    Case::infers(r#"['foo', 'bar'] | sum"#, r#"core.String"#),
    Case::infers(r#"[[1, 2], [3]] | sum"#, r#"[core.Number]"#),
    Case::infers(r#"[1.bytes] | sumBy (_)"#, r#"core.Number tagged core.Bytes"#),
    Case::infers(r#"sumBy (_.toNumber) '123'"#, r#"core.Number"#),
    Case::infers(
        r#"[{ foo: 42 }, {foo: 100 }] | sumBy identity"#,
        r#"{foo: core.Number}"#,
    ),
    Case::infers(r#"[{ foo: 42 }, {foo: 100 }].sumBy (.foo)"#, r#"core.Number"#),
    Case::infers(r#"[ls.first, ls.first] | sumBy identity"#, r#"os.PathSummary"#),
    Case::infers(
        r#"class Bob { def foo = 42 }; [Bob.new, Bob.new] | sumBy identity | .foo"#,
        r#"core.Number"#,
    ),
    Case::infers(r#"['a', 'b'] | sumBy identity"#, r#"core.String"#),
    Case::infers(r#"['a'.r, 'b'.r] | sumBy identity"#, r#"core.String tagged core.Regex"#),
    Case::infers(r#"[1, 2, 3] | max"#, r#"core.Number"#),
    Case::infers(r#"[1, 2, 3].max"#, r#"core.Number"#),
    Case::infers(r#"'abc' | max"#, r#"core.String"#),
    Case::infers(r#"'abc'.max"#, r#"core.String"#),
    Case::infers(r#"max 1 2 3"#, r#"core.Number"#),
    Case::infers(r#"max 'a' 'b' 'c'"#, r#"core.String"#),
    Case::infers(r#"[1, 2, 3] | min"#, r#"core.Number"#),
    Case::infers(r#"[1, 2, 3].min"#, r#"core.Number"#),
    Case::infers(r#"'abc' | min"#, r#"core.String"#),
    Case::infers(r#"'abc'.min"#, r#"core.String"#),
    Case::infers(r#"min 1 2 3"#, r#"core.Number"#),
    Case::infers(r#"min 'a' 'b' 'c'"#, r#"core.String"#),
    Case::infers(r#"['a', 'bb', 'ccc'] | maxBy length"#, r#"core.String"#),
    Case::infers(r#"'abc' | maxBy (_)"#, r#"core.String"#),
    Case::infers(r#"['a', 'bb', 'ccc'] | minBy length"#, r#"core.String"#),
    Case::infers(r#"'abc' | minBy (_)"#, r#"core.String"#),
    Case::infers(r#"['a', 'bb', 'ccc'] | find (_.length == 2)"#, r#"core.String"#),
    Case::infers(r#"'foo' | find (_ != 'f')"#, r#"core.String"#),
    Case::infers(r#"isEmpty []"#, r#"core.Boolean"#),
    Case::infers(r#"'size' pwd"#, r#"core.Number tagged core.Bytes"#),
    Case::infers(r#"'size' ls"#, r#"[core.Number tagged core.Bytes]"#),
    Case::infers(r#"pwd?.parent"#, r#"core.String tagged os.Path"#),
    Case::infers(r#"['foo', 'bar'].startsWith 'f'"#, r#"[core.Boolean]"#),
    Case::infers(r#"['foo bar', 'baz'].split"#, r#"[[core.String]]"#),
    Case::infers(r#"['foo:bar', 'baz'].split ':'"#, r#"[[core.String]]"#),
    Case::infers(r#"'foo:bar:baz'.split ':'"#, r#"[core.String]"#),
    Case::infers(r#"'foo:bar:baz' | split ':'"#, r#"[core.String]"#),
    Case::infers(r#"'string'.lines"#, r#"[core.String]"#),
    Case::infers(r#"[ { field: 'value' } ] | groupBy (.field)"#, r#"core.Object"#),
    Case::infers(
        r#"[ {foo: 42} ] | groupBy 'foo' --groups"#,
        r#"[collections.Group[core.Number, {foo: core.Number}]]"#,
    ),
    Case::infers(
        r#"[ {foo: 42} ] | groupBy 'foo' --groups | (_.first.key)"#,
        r#"core.Number"#,
    ),
    Case::infers(
        r#"[ {foo: 42} ] | groupBy 'foo' --groups | (_.first.values)"#,
        r#"[{foo: core.Number}]"#,
    ),
    Case::infers(
        r#"[ {foo: 42} ] | groupBy 'foo' --groups | (_.first.count)"#,
        r#"core.Number"#,
    ),
    Case::infers(
        r#"'foo' | groupBy (_) --groups"#,
        r#"[collections.Group[core.String tagged core.Character, core.String]]"#,
    ),
    Case::infers(r#"[1, 2, 3] | sliding 2"#, r#"[[core.Number]]"#),
    Case::infers(r#"[1, 2, 3] | chunked 2"#, r#"[[core.Number]]"#),
    Case::infers(r#" 'foo' { foo: 42 } "#, r#"core.Number"#),
    Case::infers(r#" 'toString' 42 "#, r#"core.String"#),
    Case::infers(r#" 'foo' [{ foo: 42 }] "#, r#"[core.Number]"#),
    Case::infers(r#"ls.first.name.absolute"#, r#"core.String tagged os.Path"#),
    Case::infers(r#"{}.toString"#, r#"core.String"#),
    Case::infers(r#"ls?"#, r#"core.Function"#),
    Case::infers(r#"help readLines"#, r#"core.Function"#),
    Case::infers(r#"help [].maxBy"#, r#"core.help.MethodHelp"#),
    Case::infers(r#"help 42.getClass"#, r#"core.Class"#),
    Case::infers(r#"'foo'.regex?"#, r#"core.help.MethodHelp"#),
    Case::infers(r#"def foo = 42; foo?"#, r#"core.Function"#),
    Case::infers(r#"class A { def a = 42 }; A.new.a?"#, r#"core.help.MethodHelp"#),
    Case::infers(r#"class A { def a b = 42 }; help A.new.a"#, r#"core.help.MethodHelp"#),
    Case::infers(r#"class A; help A"#, r#"core.Class"#),
    Case::infers(r#"[1].sumBy.target"#, r#"[core.Number]"#),
    Case::infers(r#"!!{nano}"#, r#"core.Unit"#),
    Case::infers(r#"!{which ls}"#, r#"os.ProcessResult"#),
    Case::infers(r#"git.status"#, r#"git.Status"#),
    Case::infers(r#"git['status']"#, r#"BuiltinFunction(git.status)"#),
    Case::infers(
        r#"[1, 2, 3]['reverse']"#,
        r#"BoundBuiltinMethod([core.Number], reverse)"#,
    ),
    Case::infers(r#"[1, 2, 3].reverse"#, r#"[core.Number]"#),
    Case::infers(r#"{ foo: 42 } | .foo"#, r#"core.Number"#),
    Case::infers(r#"now"#, r#"time.DateTime"#),
    Case::infers(r#"now.date"#, r#"time.Date"#),
    Case::infers(r#"()"#, r#"core.Unit"#),
    Case::infers(r#"[{foo: 42}].map(_.foo)"#, r#"[core.Number]"#),
    Case::infers(r#"(x y => x * y) 42 24"#, r#"core.Number"#),
    Case::infers(r#"(x => x + 1) 10"#, r#"core.Number"#),
    Case::infers(r#"(=> 42) | x => x"#, r#"core.Number"#),
    Case::infers(r#"{ a: 42 } | { a } => a"#, r#"core.Number"#),
    Case::infers(r#"[1, 2, 3] | reduce (x y => x + [y]) []"#, r#"[core.Number]"#),
    Case::infers(
        r#"{ foo: 42 }.withField 'bar' 256"#,
        r#"{foo: core.Number, bar: core.Number}"#,
    ),
    Case::infers(r#"ls.first.withField 'bar' 256"#, r#"os.PathSummary"#),
    Case::infers(
        r#"class Point x y; Point 3 4 | .withField 'bar' 256"#,
        r#"UserClassInstance(Point)"#,
    ),
    Case::infers(r#"{ foo: 42 }.get 'foo'"#, r#"core.Number"#),
    Case::infers(r#"ls.first.get 'size'"#, r#"core.Number tagged core.Bytes"#),
    Case::infers(
        r#"json.get 'http://example.com' | .get 'foo' --default=42'"#,
        r#"core.Number"#,
    ),
    Case::infers(r#"((@lazy block) => block) 42"#, r#"core.Number"#),
    Case::infers(r#"def foo a... b = b; foo 42"#, r#"core.Number"#),
    Case::infers(r#"def foo (@flag a) b = a; foo [] --a=10"#, r#"core.Number"#),
    Case::infers(r#"5.times { print 'Hi' }"#, r#"[core.Unit]"#),
    Case::infers(r#"timeTaken ls"#, r#"core.TimedResult[[os.PathSummary]]"#),
    Case::infers(r#"timeTaken ls | .result"#, r#"[os.PathSummary]"#),
    Case::infers(r#"zip [1] [2]"#, r#"[[core.Number]]"#),
    Case::infers(
        r#"{ foo: 42, bar: { a: 1, b: 2 } }.hoist 'bar'"#,
        r#"{foo: core.Number, a: core.Number, b: core.Number}"#,
    ),
    Case::infers(r#"a = 42; a"#, r#"core.Number"#),
    Case::infers(r#"a = 42; b = 20; c = a + b; c"#, r#"core.Number"#),
    Case::infers(r#"{ a } = { a: 42 }; a"#, r#"core.Number"#),
    Case::infers(r#"def square n = n * n; square 42"#, r#"core.Number"#),
    Case::infers(
        r#"def foo n = if n > 0 then bar (n - 1) else 42; def bar n = foo (n - 1); bar 10"#,
        r#"core.Number"#,
    ),
    Case::infers(r#"{ foo: => 42 }.foo"#, r#"core.Number"#),
    Case::infers(r#"'{ foo: 42 }' | json.fromString | .foo"#, r#"core.Any"#),
    Case::infers(
        r#"json.read 'file.json' | type.hint { name: String, addresses: [{ houseNumber: String, postcode: String }] }"#,
        r#"{name: core.String, addresses: [{houseNumber: core.String, postcode: core.String}]}"#,
    ),
    Case::infers(r#"42.isNull"#, r#"core.Boolean"#),
    Case::infers(r#"42.isTruthy"#, r#"core.Boolean"#),
    Case::infers(r#"42.isA Number"#, r#"core.Boolean"#),
    Case::infers(r#"def rev x = x.reverse; rev.isNull"#, r#"core.Boolean"#),
    Case::infers(r#"(x => x).isNull"#, r#"core.Boolean"#),
    Case::infers(r#"Object.merge"#, r#"BuiltinFunction(core.Object.merge)"#),
    Case::infers(r#"class Foo { def bar = 10 }; Foo.new.bar"#, r#"core.Number"#),
    Case::infers(r#"class Foo n { def bar = 10 }; Foo.new 5 | .bar"#, r#"core.Number"#),
    Case::infers(r#"class Foo { def bar = 10 }; Foo 5 | .bar"#, r#"core.Number"#),
    Case::infers(r#"class Foo { def bar m = 10 }; Foo 5 | .bar 10"#, r#"core.Number"#),
    Case::infers(r#"class Foo { def bar m = m }; Foo 5 | .bar 10"#, r#"core.Number"#),
    Case::infers(
        r#"class Foo { def bar m = m }; [Foo.new, Foo.new].bar 42"#,
        r#"[core.Number]"#,
    ),
    Case::infers(r#"class Foo { def a = 10; def b = a }; Foo.new.b"#, r#"core.Number"#),
    Case::infers(
        r#"class Foo { @(alias 'aaa') def aardvark = 10; def b = aaa }; Foo.new.b"#,
        r#"core.Number"#,
    ),
    Case::setup(
        r#"class Alias { @(alias 'aaa') def aardvark = 10; def b = aaa }"#,
        &[Case::infers(r#"Alias.new.b"#, r#"core.Number"#)],
    ),
    Case::infers(
        r#"class A { def method1 = this; def method2 = 10 }; A.new.method1.method2"#,
        r#"core.Number"#,
    ),
    Case::infers(
        r#"class A { def method1 n = this; def method2 = 10 }; A.new.method1 3 | .method2"#,
        r#"core.Number"#,
    ),
    Case::infers(r#"[2, 3, 1].sortBy | method => method identity"#, r#"[core.Number]"#),
    Case::infers(r#"'foo'['reverse'] | x => x"#, r#"core.String"#),
    Case::infers(r#"class A { def bar = 100 }; A.new['bar'] | x => x"#, r#"core.Number"#),
    Case::infers(
        r#"class A { def bar = this; def baz = 100 }; A.new['bar'] | x => x.baz"#,
        r#"core.Number"#,
    ),
    Case::infers(
        r#"class A { def square n = n * n }; A.new.square.help"#,
        r#"core.help.MethodHelp"#,
    ),
    Case::infers(r#"def foo (n = ls) = 42; foo"#, r#"core.Number"#),
    Case::infers(r#"(class A { def foo = 42 }) | .new.foo"#, r#"core.Number"#),
    Case::infers(r#"(def foo = 42) | x => x"#, r#"core.Number"#),
    Case::infers(r#"class Bob { def n = 42 }; 1 | type.hint Bob | .n"#, r#"core.Number"#),
    Case::infers(
        r#"class Bob { def n = 42 }; 1 | type.hint [Bob] | .n"#,
        r#"[core.Number]"#,
    ),
    Case::infers(r#"[1, 2, 3].intersect [3, 4]"#, r#"[core.Number]"#),
    Case::infers(r#"[1, 2, 3].append 4"#, r#"[core.Number]"#),
    Case::infers(r#"[1, 2, 3].prepend 0"#, r#"[core.Number]"#),
    Case::infers(
        r#"class Point x y { def method = 42 }; {x: 10, y: 20}.bless Point | .method"#,
        r#"core.Number"#,
    ),
    Case::infers(
        r#"class Point x y { def method = 42 }; Point.bless {x: 10, y: 20}| .method"#,
        r#"core.Number"#,
    ),
    Case::infers(
        r#"{ name: 'name', value: 42 }.bless core.FieldAndValue"#,
        r#"core.FieldAndValue"#,
    ),
    Case::infers(
        r#"class Point x y; Point 3 4 | .unbless"#,
        r#"{x: core.Any, y: core.Any}"#,
    ),
    Case::infers(
        r#"{ foo: 42 }.fields.first.unbless"#,
        r#"{name: core.Any, value: core.Any}"#,
    ),
    Case::infers(
        r#"class A { @(alias 'a') def aardvark = 42 }; A.new.a"#,
        r#"core.Number"#,
    ),
    Case::infers(r#"List 1 2 3"#, r#"[core.Number]"#),
    Case::infers(r#"List.new"#, r#"[core.Any]"#),
    Case::setup(
        r#"class Foo { def a = 10; def b = a }"#,
        &[
            Case::infers(r#"Foo.new.a"#, r#"core.Number"#),
            Case::infers(r#"Foo.new.b"#, r#"core.Number"#),
        ],
    ),
    Case::infers(r#"[1, 2, 3] | skip 2"#, r#"[core.Number]"#),
    Case::infers(r#"[1, 2, 3] | skip"#, r#"[core.Number]"#),
    Case::infers(r#"[1, 2, 3] | allButLast 2"#, r#"[core.Number]"#),
    Case::infers(r#"[1, 2, 3] | allButLast"#, r#"[core.Number]"#),
    Case::infers(r#"true 1 2"#, r#"core.Number"#),
    Case::infers(r#"def add x y = x + y; add --x=1 2 3"#, r#"core.Number"#),
    Case::infers(r#"[[1, 2], [3, 4]] | transpose"#, r#"[[core.Number]]"#),
    Case::infers(
        r#"[{ foo: 1, bar: '2' }, { foo: 3, bar: '4' }] | transpose"#,
        r#"{foo: [core.Number], bar: [core.String]}"#,
    ),
    Case::infers(r#"'abc'.deleteUpTo 'b'"#, r#"core.String"#),
    Case::infers(r#"'abc'.deleteAfter 'b'"#, r#"core.String"#),
];

/// Cases that need builtins from later milestones (os, git, http), or that differ deliberately
const DEFERRED: &[&str] = &[];

fn inferred_type(ctx: &mut Ctx, input: &str) -> String {
    let compiled = ctx.compile_forgiving(SourceUnit::new("test", input));
    let inferred = infer::infer(ctx, &compiled.program.body);
    match inferred.types.get(&compiled.program.body.id) {
        Some(ty) => format!("{ty:?}"),
        None => "<no type>".into(),
    }
}

#[test]
fn type_inference() {
    let (failures, passing_deferred) = with_big_stack(|| {
        let mut failures = vec![];
        let mut passing_deferred = vec![];
        let mut check = |ctx: &mut Ctx, input: &'static str, expected: &str| {
            let actual = inferred_type(ctx, input);
            match (actual == expected, DEFERRED.contains(&input)) {
                (false, false) => failures.push(format!("{input:?}: got {actual}, expected {expected}")),
                (true, true) => passing_deferred.push(format!("{input:?}")),
                _ => {}
            }
        };
        for case in CASES {
            match case {
                Case::Infers(input, expected) => {
                    check(&mut Ctx::new(Rc::new(TestPlatform::default())), input, expected)
                }
                Case::Setup(setup, cases) => {
                    let mut ctx = Ctx::new(Rc::new(TestPlatform::default()));
                    ctx.run("setup", setup).expect("setup runs");
                    for case in *cases {
                        if let Case::Infers(input, expected) = case {
                            check(&mut ctx, input, expected);
                        }
                    }
                }
            }
        }
        (failures, passing_deferred)
    });
    let mut message = String::new();
    if !failures.is_empty() {
        message += &format!("{} failing case(s):\n  {}\n", failures.len(), failures.join("\n  "));
    }
    if !passing_deferred.is_empty() {
        message += &format!(
            "{} deferred case(s) now pass:\n  {}\n",
            passing_deferred.len(),
            passing_deferred.join("\n  ")
        );
    }
    assert!(message.is_empty(), "{message}");
}
