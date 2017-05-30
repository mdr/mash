package com.github.mdr.mash.evaluator

class ObjectMethodsTest extends AbstractEvaluatorTest {

  // Object.where
  "{ apple: 1, bob: 2, aardvark: 3 }.where (.startsWith 'a')" ==> "{ apple: 1, aardvark: 3 }"
  "{ apple: 1, bob: 2, aardvark: 3 }.where (f v => f.startsWith 'b' or v == 3)" ==> "{ bob: 2, aardvark: 3 }"
  "{ apple: 1, bob: 2, aardvark: 3 } | where (.startsWith 'a')" ==> "{ apple: 1, aardvark: 3 }"
  "{ apple: 1, bob: 2, aardvark: 3 } | where (f v => f.startsWith 'b' or v == 3)" ==> "{ bob: 2, aardvark: 3 }"

  // Object.whereNot
  "{ apple: 1, bob: 2, aardvark: 3 }.whereNot (.startsWith 'a')" ==> "{ bob: 2 }"
  "{ apple: 1, bob: 2, aardvark: 3 }.whereNot (f v => f.startsWith 'b' or v == 3)" ==> "{ apple: 1 }"
  "{ apple: 1, bob: 2, aardvark: 3 } | whereNot (.startsWith 'a')" ==> "{ bob: 2 }"
  "{ apple: 1, bob: 2, aardvark: 3 } | whereNot (f v => f.startsWith 'b' or v == 3)" ==> "{ apple: 1 }"

  // Object.map
  "{ apple: 1, bob: 2, cat: 3 }.map (f v => { (f.toUpper): v * v })" ==> "{ APPLE: 1, BOB: 4, CAT: 9 }"
  "{ apple: 1 }.map (f v => { (f): v, (f.reverse): v })" ==> "{ apple: 1, elppa: 1 }"
  "{ apple: 1, bob: 2, cat: 3 } | map (f v => { (f.toUpper): v * v })" ==> "{ APPLE: 1, BOB: 4, CAT: 9 }"
  "{ apple: 1 } | map (f v => { (f): v, (f.reverse): v })" ==> "{ apple: 1, elppa: 1 }"

  // Object.transformValues
  "{ foo: 3, bar: 4 }.transformValues (n => n * n)" ==> "{ foo: 9, bar: 16 }"
  """{ foo: 3, bar: 4 }.transformValues (f v => "$f$v")""" ==> "{ foo: 'foo3', bar: 'bar4' }"

  // Object.transformFields
  "{ foo: 3, bar: 4 }.transformFields (.toUpper)" ==> "{ FOO: 3, BAR: 4 }"
  "{ foo: 3, bar: 4 }.transformFields (f v => f.toUpper + v)" ==> "{ FOO3: 3, BAR4: 4 }"

  // Object.grep
  "{ foo: 'wibble', bar: 'wobble', wibble: 'baz' }.grep 'wibble'" ==> "{ foo: 'wibble', wibble: 'baz' }"
  "{ foo: 'wibble', bar: 'wobble', wibble: 'baz' } | grep 'wibble'" ==> "{ foo: 'wibble', wibble: 'baz' }"
  "{ a: 42 } | grep 'name'" ==> "{}"

  // Object.hoist
  "{ foo: 42, bar: { baz1: 100, baz2: 200 } }.hoist 'bar'" ==> "{ foo: 42, baz1: 100, baz2: 200 }"
  "{ foo: 42, bar: [{ baz1: 100, baz2: 200 }, { baz1: 300, baz2: 400 }] }.hoist 'bar'" ==>
    "[{ foo: 42, baz1: 100, baz2: 200 }, { foo: 42, baz1: 300, baz2: 400 }]"
  "{ foo: 42, bar: { baz1: 100, baz2: 200 } }.hoist 'bar' --prefix='bar_'" ==> "{ foo: 42, bar_baz1: 100, bar_baz2: 200 }"

  // Object.merge
  "Object.merge []" ==> "{}"
  "Object.merge { foo: 42 }" ==> "{ foo: 42 }"
  "Object.merge { foo: 42 } { bar: 128 }" ==> "{ foo: 42, bar: 128 }"
  "Object.merge { foo: 42 } { foo: 128 }" ==> "{ foo: 128 }"
  "class A a; class B b; class C c; Object.merge [A 1, B 2, C 3] | .getClass.name" ==> "'C'"

  // Object.fromPairs
  "[['a', 1], ['b', 2]] | Object.fromPairs" ==> "{ a: 1, b: 2 }"
  "[{ name: 'a', value: 1 }, { name: 'b', value: 2 }] | Object.fromPairs" ==> "{ a: 1, b: 2 }"

  // Object.withField
  "{}.withField 'foo' 42" ==> "{ foo: 42 }"
  "{ foo: 42 }.withField 'bar' 256" ==> "{ foo: 42, bar: 256 }"
  "{ foo: 42 }.withField 'foo' 256" ==> "{ foo: 256 }"
  "{ a: 1, b: 2, c: 3, d: 4, e: 5, f: 6 }.withField 'c' 100 | .fields[2].name" ==> "'c'"

  // Object.get
  "{ foo: 42 }.get 'foo'" ==> 42
  "{}.get 'nope'" ==> null
  "{}.get 'nope' --default=42" ==> 42

  // Object literals
  "{ 'foo': 42 }.foo" ==> 42
  "{ ('foo' + 'bar'): 42 }" ==> "{ foobar: 42 }"
  "foo = 42; bar = 128; { foo, bar }" ==> "{ foo: 42, bar: 128 }"
  "def pi = 3.14; { pi }" ==> "{ pi: 3.14 }"

  // Object.hasField
  "{ foo: 42 }.hasField 'foo'" ==> true
  "{ foo: 42 }.hasField 'bar'" ==> false

  // Object.unbless
  "class Thing; Thing.new.unbless.getClass" ==> "Object"

  // Object.fields
  "{ foo: 42 }.fields.name" ==> "['foo']"
  "42.fields".shouldThrowAnException

}
