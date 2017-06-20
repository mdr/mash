package com.github.mdr.mash.evaluator

class TransposeTest extends AbstractEvaluatorTest {

  "[] | transpose" ==> "[]"

  // list of lists
  "[[1, 2, 3], [4, 5, 6]].transpose" ==> "[[1, 4], [2, 5], [3, 6]]"
  "[[1, 2, 3], [4, 5, 6]] | transpose" ==> "[[1, 4], [2, 5], [3, 6]]"
  "[[1, 2, 3]] | transpose" ==> "[[1], [2], [3]]"

  "[[1, 2, 3], [4, 5], [6, 7, 8]] | transpose" ==> "[[1, 4, 6], [2, 5, 7], [3, null, 8]]"
  "[[1, 2, 3], [4, 5], [6, 7, 8]] | transpose --skipGaps" ==> "[[1, 4, 6], [2, 5, 7], [3, 8]]"

  "[[]] | transpose" ==> "[]"
  "[[1]] | transpose" ==> "[[1]]"

  // list of objects
  "[{ foo: 42, bar: 100 }, { foo: 100, baz: 99 }] | transpose" ==> "{ foo: [42, 100], bar: [100, null], baz: [null, 99] }"
  "[{ foo: 42, bar: 100 }, { foo: 100, baz: 99 }] | transpose --skipGaps" ==> "{ foo: [42, 100], bar: [100], baz: [99] }"
  "[{ foo: 42, bar: 100 }] | transpose" ==> "{ foo: [42], bar: [100] }"
  "[{ foo: 42 }] | transpose" ==> "{ foo: [42] }"
  "[{}] | transpose" ==> "{}"

  "{} | transpose" ==> "{}"

  // object of lists
  "{ foo: [1, 2, 3], bar: [4, 5, 6] } | transpose" ==> "[{ foo: 1, bar: 4 }, { foo: 2, bar: 5 }, { foo: 3, bar: 6 }]"
  "{ foo: [1, 2, 3], bar: [4, 5, 6] }.transpose" ==> "[{ foo: 1, bar: 4 }, { foo: 2, bar: 5 }, { foo: 3, bar: 6 }]"
  "{ foo: [1, 2, 3], bar: [4, 5, 6] } | transpose | transpose" ==> "{ foo: [1, 2, 3], bar: [4, 5, 6] }"
  "{ foo: [1, 2], bar: [4, 5, 6] } | transpose" ==> "[{ foo: 1, bar: 4 }, { foo: 2, bar: 5 }, { bar: 6 }]"
  "{ foo: [1, 2] } | transpose" ==> "[{ foo: 1 }, { foo: 2 }]"
  "{ foo: [1] } | transpose" ==> "[{ foo: 1 }]"
  "{ foo: [] } | transpose" ==> "[]"

  // object of objects
  "{ foo: { a: 1, b: 2 }, bar: { a: 3, b: 4 } } | transpose" ==> "{ a: { foo: 1, bar: 3 }, b: { foo: 2, bar: 4 } }"
  "{ foo: { a: 1, b: 2 }, bar: { a: 3, b: 4 } } | transpose | transpose" ==> "{ foo: { a: 1, b: 2 }, bar: { a: 3, b: 4 } }"
  "{ foo: { a: 1, b: 2 }, bar: { a: 3 } } | transpose" ==> "{ a: { foo: 1, bar: 3 }, b: { foo: 2 } }"
  "{ foo: { a: 1, b: 2 } } | transpose" ==> "{ a: { foo: 1 }, b: { foo: 2 } }"
  "{ foo: { a: 1 } } | transpose" ==> "{ a: { foo: 1 } }"
  "{ foo: {} } | transpose" ==> "{}"
}
