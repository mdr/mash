package com.github.mdr.mash.evaluator

class ObjectMethodsTest extends AbstractEvaluatorTest {

  // Object.transformValues
  "{ foo: 3, bar: 4 }.transformValues (n => n * n)" ==> "{ foo: 9, bar: 16 }"
  """{ foo: 3, bar: 4 }.transformValues (f v => "$f$v")""" ==> "{ foo: 'foo3', bar: 'bar4' }"

  // Object.transformFields
  "{ foo: 3, bar: 4 }.transformFields (.toUpper)" ==> "{ FOO: 3, BAR: 4 }"
  "{ foo: 3, bar: 4 }.transformFields (f v => f.toUpper + v)" ==> "{ FOO3: 3, BAR4: 4 }"
  "{ a: 10, bb: 20, ccc: 30 }.transformFields (.length)" ==> "{ (1): 10, (2): 20, (3): 30 }"

  // Object.hoist
  "{ foo: 42, bar: { baz1: 100, baz2: 200 } }.hoist 'bar'" ==> "{ foo: 42, baz1: 100, baz2: 200 }"
  "{ foo: 42, (42): { baz1: 100, baz2: 200 } }.hoist 42" ==> "{ foo: 42, baz1: 100, baz2: 200 }"
  "{ foo: 42, bar: [{ baz1: 100, baz2: 200 }, { baz1: 300, baz2: 400 }] }.hoist 'bar'" ==>
    "[{ foo: 42, baz1: 100, baz2: 200 }, { foo: 42, baz1: 300, baz2: 400 }]"
  "{ foo: 42, bar: { baz1: 100, baz2: 200 } }.hoist 'bar' --prefix='bar_'" ==> "{ foo: 42, bar_baz1: 100, bar_baz2: 200 }"

  // Object.merge
  "Object.merge []" ==> "{}"
  "Object.merge { foo: 42 }" ==> "{ foo: 42 }"
  "Object.merge { foo: 42 } { bar: 128 }" ==> "{ foo: 42, bar: 128 }"
  "Object.merge { foo: 42 } { foo: 128 }" ==> "{ foo: 128 }"
  "Object.merge { foo: 42 } { (42): 'foo' }" ==> "{ foo: 42, (42): 'foo' }"
  "class A a; class B b; class C c; Object.merge [A 1, B 2, C 3] | .getClass.name" ==> "'C'"

  // Object.fromPairs
  "[['a', 1], ['b', 2]] | Object.fromPairs" ==> "{ a: 1, b: 2 }"
  "[['a', 1], [2, 'b']] | Object.fromPairs" ==> "{ a: 1, (2): 'b' }"
  "[['a', 1, 100], ['b', 2, 200]] | Object.fromPairs 0 2" ==> "{ a: 100, b: 200 }"
  "[] | Object.fromPairs" ==> "{}"

  "[{ name: 'a', value: 1 }, { name: 'b', value: 2 }] | Object.fromPairs" ==> "{ a: 1, b: 2 }"
  "[{ name: 'a', value: 1, anotherValue: 100 }, { name: 'b', value: 2, anotherValue: 200 }] | Object.fromPairs 'name' 'anotherValue'" ==>
    "{ a: 100, b: 200 }"

  // Object.withField
  "{}.withField 'foo' 42" ==> "{ foo: 42 }"
  "{ foo: 42 }.withField 'bar' 256" ==> "{ foo: 42, bar: 256 }"
  "{ foo: 42 }.withField 'foo' 256" ==> "{ foo: 256 }"
  "{ foo: 42 }.withField 256 'foo'" ==> "{ foo: 42, (256): 'foo' }"
  "{ a: 1, b: 2, c: 3, d: 4, e: 5, f: 6 }.withField 'c' 100 | .fields[2].name" ==> "'c'"

  // Object.get
  "{ foo: 42 }.get 'foo'" ==> 42
  "{ (42): 'foo'}.get 42" ==> "'foo'"
  "{}.get 'nope'" ==> null
  "{}.get 42" ==> null
  "{}.get 'nope' --default=42" ==> 42

  // Object.hasField
  "{ foo: 42 }.hasField 'foo'" ==> true
  "{ (42): 'foo'}.hasField 42" ==> true
  "{ foo: 42 }.hasField 'bar'" ==> false

  // Object.unbless
  "class Thing; Thing.new.unbless.getClass" ==> "Object"

  // Object.fields
  "{ foo: 42 }.fields.name" ==> "['foo']"
  "{ (42): 'foo'}.fields.name" ==> "[42]"
  "42.fields".shouldThrowAnException

}
