package com.github.mdr.mash.evaluator

class SafeMemberTest extends AbstractEvaluatorTest {

  "null?.noSuchMember" ==> null
  "42?.noSuchMember" ==> null
  "{}?.noSuchMember" ==> null
  "null?.toString" ==> "'null'"
  "[null, { foo: 42 }] | map (_?.foo)" ==> "[null, 42]"

  // vectorised
  "[{ foo: { bar: 42 } }, { foo: null }].foo?.bar" ==> "[42, null]"
  "[{ foo: 42 }, { bar: 12 }]?.foo" ==> "[42, null]"

}
