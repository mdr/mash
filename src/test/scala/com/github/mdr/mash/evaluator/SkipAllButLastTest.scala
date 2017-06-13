package com.github.mdr.mash.evaluator

class SkipAllButLastTest extends AbstractEvaluatorTest {

  // allButLast
  "[1, 2, 3, 4, 5] | allButLast" ==> "[1, 2, 3, 4]"
  "[1, 2, 3, 4, 5].allButLast" ==> "[1, 2, 3, 4]"
  "[1, 2, 3, 4, 5] | allButLast 3" ==> "[1, 2]"
  "[1, 2, 3] | allButLast 5" ==> "[]"
  "[] | allButLast" ==> "[]"
  "[] | allButLast 5" ==> "[]"

  "'abcde' | allButLast" ==> "'abcd'"
  "'abcde'.allButLast" ==> "'abcd'"

  "{ a: 1, b: 2, c: 3 } | allButLast | select --add --order=(.fields.name.join)" ==> "{ a: 1, b: 2, order: 'ab' }"
  "{ a: 1, b: 2, c: 3 }.allButLast" ==> "{ a: 1, b: 2 }"

  // skip
  "[1, 2, 3, 4, 5] | skip 2" ==> "[3, 4, 5]"
  "[1, 2, 3, 4, 5].skip 2" ==> "[3, 4, 5]"
  "[1, 2] | skip 5" ==> "[]"
  "[1, 2] | skip" ==> "[2]"

  "'abcde' | skip 3" ==> "'de'"
  "'abcde' | skip" ==> "'bcde'"
  "'abcde'.skip 3" ==> "'de'"
  "'abcde'.skip" ==> "'bcde'"

  "{ a: 1, b: 2, c: 3 } | skip | select --add --order=(.fields.name.join)" ==> "{ b: 2, c: 3, order: 'bc' }"
  "{ a: 1, b: 2, c: 3 }.skip" ==> "{ b: 2, c: 3 }"

}
