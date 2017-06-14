package com.github.mdr.mash.evaluator

class MapFlattenTest extends AbstractEvaluatorTest {

  // flatMap
  "[1, 2, 3].flatMap (n => [n * 10, n])" ==> "[10, 1, 20, 2, 30, 3]"
  "[1, 2, 3] | flatMap (n => [n * 10, n])" ==> "[10, 1, 20, 2, 30, 3]"
  "[1, 22, 333] | flatMap (.toString)" ==> "'122333'"
  "[1, 2, 3] | flatMap (n i => [n, i])" ==> "[1, 0, 2, 1, 3, 2]"
  "['a', 'b', 'c'] | flatMap (s i => { (s): i })" ==> "{ a: 0, b: 1, c: 2 }"

  "'abc' | flatMap (_ + '!')" ==> "'a!b!c!'"
  "'abc' | flatMap (c => [c])" ==> "['a', 'b', 'c']"
  "'abc'.flatMap (c => [c]) " ==> "['a', 'b', 'c']"
  "'abc' | flatMap (s i => { (s): i })" ==> "{ a: 0, b: 1, c: 2 }"

  "{ apple: 1 }.flatMap (f v => { (f): v, (f.reverse): v })" ==> "{ apple: 1, elppa: 1 }"
  "{ apple: 1 } | flatMap (f v => { (f): v, (f.reverse): v })" ==> "{ apple: 1, elppa: 1 }"

  // flatten
  "[[], [1], [2, 3]] | flatten" ==> "[1, 2, 3]"
  "[[], [1], [2, 3]].flatten" ==> "[1, 2, 3]"
  "flatten []" ==> "[]"
  "flatten ['', 'a', 'bc']" ==> "'abc'"
  "flatten [{ a: 1, b: 2 }, { c: 3 }]" ==> "{ a: 1, b: 2, c: 3 }"
  "flatten [{ a: 1 }, { a: 2 }, { a: 3 }]" ==> "{ a: 3 }"

  // map
  "[1, 2, 3].map (_ * 2)" ==> "[2, 4, 6]"
  "map --f=(_ * 2) --sequence=[1, 2, 3]" ==> "[2, 4, 6]"
  "map (_ * 2) --sequence=[1, 2, 3]" ==> "[2, 4, 6]"
  "[1, 2, 3] | map --f=(_ * 2)" ==> "[2, 4, 6]"
  "[1, 2, 3] | map (n i => n + i)" ==> "[1, 3, 5]"
  "[1, 2, 3] | map (.toString)" ==> "['1', '2', '3']"

  "'123' | map (_.toNumber)" ==> "[1, 2, 3]"
  "'foo' | map (.toUpper)" ==> "'FOO'"
  "'foo'.map (.toUpper)" ==> "'FOO'"
  "'abc'.map (c => c + c)" ==> "'aabbcc'"
  "'a1b2'.map (.isDigit)" ==> "[false, true, false, true]"
  "''.map (.isDigit)" ==> "''"

  "{ apple: 1, bob: 2, cat: 3 }.map (f v => { (f.toUpper): v * v })" ==> "{ APPLE: 1, BOB: 4, CAT: 9 }"
  "{ apple: 1, bob: 2, cat: 3 } | map (f v => { (f.toUpper): v * v })" ==> "{ APPLE: 1, BOB: 4, CAT: 9 }"
  "{ apple: 1 }.map (f v => { (f): v, (f.reverse): v })" ==> "{ apple: 1, elppa: 1 }"
  "{ apple: 1 } | map (f v => { (f): v, (f.reverse): v })" ==> "{ apple: 1, elppa: 1 }"

}
