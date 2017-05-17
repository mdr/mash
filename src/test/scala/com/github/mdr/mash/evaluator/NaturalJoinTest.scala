package com.github.mdr.mash.evaluator

class NaturalJoinTest extends AbstractEvaluatorTest {

  "[].naturalJoin []" ==> "[]"
  "[{ bar: 100 }].naturalJoin [{ foo: 42 }]" ==> "[]"

  "[{ foo: 12, bar: 100 }, { foo: 12, bar: 200 }].naturalJoin [{ foo: 12, baz: 100 }, { foo: 12, baz: 500 }]" ==>
    "[{ foo: 12, bar: 100, baz: 100 }, { foo: 12, bar: 100, baz: 500 }, { foo: 12, bar: 200, baz: 100 }, { foo: 12, bar: 200, baz: 500 }]"

  "[{ a: 1, b: 2 }, { y: 3 }].naturalJoin [{ a: 1, c: 10 }, { z: 4 }]" ==> "[{ a: 1, b: 2, c: 10 }]"

}
