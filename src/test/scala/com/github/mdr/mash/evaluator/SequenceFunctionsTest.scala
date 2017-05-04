package com.github.mdr.mash.evaluator

import scala.language.postfixOps

class SequenceFunctionsTest extends AbstractEvaluatorTest {

  // all
  "[2, 4, 6] | all (_ < 10)" ==> true
  "[2, 4, 6] | all (_ < 5)" ==> false
  "[] | all (_ < 10)" ==> true
  "[1] | all (_)" ==> true
  "[0] | all (_)" ==> false
  "'aaa' | all (_ == 'a')" ==> true
  "'aaa'.all (_ == 'a')" ==> true

  // allButLast
  "allButLast [1, 2, 3, 4, 5]" ==> "[1, 2, 3, 4]"
  "[1, 2, 3, 4, 5].allButLast" ==> "[1, 2, 3, 4]"
  "allButLast 3 [1, 2, 3, 4, 5]" ==> "[1, 2]"
  "allButLast 5 [1, 2, 3]" ==> "[]"
  "allButLast []" ==> "[]"
  "allButLast 5 []" ==> "[]"
  "allButLast 'abcde'" ==> "'abcd'"

  // any
  "[2, 4, 6] | any (_ > 10)" ==> false
  "[2, 4, 6] | any (_ > 5)" ==> true
  "[] | any (_ > 10)" ==> false
  "[1] | any (_)" ==> true
  "[0] | any (_)" ==> false
  "'abc' | any (_ == 'a')" ==> true
  "'abc'.any (_ == 'a')" ==> true

  // chunked
  "chunked 2 [1, 2, 3, 4, 5]" ==> "[[1, 2], [3, 4], [5]]"
  "chunked 1 [1, 2, 3]" ==> "[[1], [2], [3]]"
  "chunked 3 'foobar'" ==> "['foo', 'bar']"

  // contains
  "[1, 2, 3].contains 2" ==> true
  "[1, 2, 3].contains 7" ==> false
  "[] | contains 42" ==> false
  "'abc' | contains 'b'" ==> true
  "'abc'.untagged | contains 'b'" ==> true
  "'abc' | contains 'b'.untagged" ==> true

  // count
  "[1, 2, 3, 4, 5].count" ==> 5
  "'abc'.count" ==> 3

  // countMatches
  "[1, 2, 300, 2, 400].countMatches (_ < 100)" ==> 3
  "'foo' | countMatches (_ == 'o')" ==> 2
  "'foo'.countMatches (_ == 'o')" ==> 2

  // each
  "a = 0; [1, 2, 3] | each (a = a + _); a" ==> 6
  "a = ''; 'foo' | each (a = _ + a); a" ==> "'oof'"
  "a = ''; 'foo'.each (a = _ + a); a" ==> "'oof'"

  // find
  "[1, 2, 300, 4, 5] | find (_ > 100)" ==> 300
  "[1, 2, 3, 4, 5] | find (_ > 100)" ==> null
  "'xxxaxxx' | find (_ != 'x')" ==> "'a'"
  "'xxxaxxx'.find (_ != 'x')" ==> "'a'"

  // first
  "first [0]" ==> 0
  "first [1, 2, 3]" ==> 1
  "first 2 [1, 2, 3]" ==> "[1, 2]"
  " first 'abc' " ==> " 'a' "
  " first 2 'abc' " ==> " 'ab' "
  " 'abc'.first " ==> " 'a' "
  " 'abc'.first 2" ==> " 'ab' "
  "first []" ==> null
  "first ''" ==> null

  // grep
  "['foo', 'bar', 'baz'].grep 'b'" ==> "['bar', 'baz']"
  "['foo', 'bar', 'baz'].grep -n 'b'" ==> "['foo']"
  "['apple', { obj: 'ball' }] | grep 'b'" ==> "[{ obj: 'ball' }]"
  "['foo', 'bar', 'BAZ'] | grep -i 'b'" ==> "['bar', 'BAZ']"
  "['apple', 'ball', 'cup'] | grep -r '(a|b)'" ==> "['apple', 'ball']"
  "['APPLE', 'ball', 'cup'] | grep -r -i '(a|B)'" ==> "['APPLE', 'ball']"
  "'foo`nbar`nbaz' | grep 'b'" ==> "['bar', 'baz']"
  "'foo`nbar`nbaz'.grep 'b'" ==> "['bar', 'baz']"

  // isEmpty
  "isEmpty []" ==> true
  "isEmpty [1, 2, 3]" ==> false
  "''.isEmpty" ==> true
  "'abc'.isEmpty" ==> false

  // last
  "last [1, 2, 3]" ==> 3
  "last 2 [1, 2, 3]" ==> "[2, 3]"
  "last 'xyz'" ==> " 'z' "
  "last 2 'xyz'" ==> " 'yz' "
  "'xyz'.last" ==> " 'z' "
  "'xyz'.last 2" ==> " 'yz' "
  "last []" ==> null
  "last ''" ==> null

  // length
  "length []" ==> 0
  "length [1, 2, 3]" ==> 3
  " length '' " ==> 0
  " length 'abc' " ==> 3
  " ''.length " ==> 0
  " 'abc'.length " ==> 3
  " 'abc' | length " ==> 3

  // flatMap
  "[1, 2, 3].flatMap (n => [n * 10, n])" ==> "[10, 1, 20, 2, 30, 3]"
  "flatMap (n => [n * 10, n]) [1, 2, 3]" ==> "[10, 1, 20, 2, 30, 3]"
  "flatMap (_ + '!') 'abc'" ==> "'a!b!c!'"
  "flatMap (.toString) [1, 22, 333]" ==> "'122333'"
  "'abc' | flatMap (c => [c])" ==> "['a', 'b', 'c']"
  "'abc'.flatMap (c => [c]) " ==> "['a', 'b', 'c']"
  "flatMap (n i => [n, i]) [1, 2, 3]" ==> "[1, 0, 2, 1, 3, 2]"

  // flatten
  "flatten [[], [1], [2, 3]]" ==> "[1, 2, 3]"
  "flatten []" ==> "[]"
  "flatten ['', 'a', 'bc']" ==> "'abc'"

  // map
  "[1, 2, 3].map (_ * 2)" ==> "[2, 4, 6]"
  "map --f=(_ * 2) --sequence=[1, 2, 3]" ==> "[2, 4, 6]"
  "map (_ * 2) --sequence=[1, 2, 3]" ==> "[2, 4, 6]"
  "map --f=(_ * 2) [1, 2, 3]" ==> "[2, 4, 6]"
  "[1, 2, 3] | map --f=(_ * 2)" ==> "[2, 4, 6]"
  "'123' | map (_.toNumber)" ==> "[1, 2, 3]"
  "map (n i => n + i) [1, 2, 3]" ==> "[1, 3, 5]"
  "'foo' | map (.toUpper)" ==> "'FOO'"
  "'foo'.map (.toUpper)" ==> "'FOO'"

  // nonEmpty
  "nonEmpty []" ==> false
  "nonEmpty [1, 2, 3]" ==> true
  "''.nonEmpty" ==> false
  "'abc'.nonEmpty" ==> true

  // reduce
  "[1, 2, 3, 4, 5] | reduce (x y => x + y)" ==> 15
  "[1] | reduce (x y => x + y)" ==> 1
  "[1, 2, 3, 4, 5] | reduce (x y => x + y) 10" ==> 25
  "[] | reduce (x y => x + y) 10" ==> 10
  "[] | reduce (x y => x + y) null" ==> null
  "[] | reduce (x y => x + y)" shouldThrowAnException

  "'foo' | reduce (acc c => acc + [c]) []" ==> "['f', 'o', 'o']"

  // reverse
  "reverse 'trebor'" ==> " 'robert' "
  "'trebor'.reverse" ==> " 'robert' "
  "[1, 2, 3].reverse" ==> "[3, 2, 1]"

  // skip
  "[1, 2, 3, 4, 5] | skip 2" ==> "[3, 4, 5]"
  "[1, 2] | skip 5" ==> "[]"
  "[1, 2] | skip" ==> "[2]"
  "'abcde' | skip 3" ==> "'de'"
  "'abcde' | skip" ==> "'bcde'"
  "'abcde'.skip 3" ==> "'de'"
  "'abcde'.skip" ==> "'bcde'"

  // skipWhile
  "[1, 2, 3, 4, 5, 1] | skipWhile (_ < 3)" ==> "[3, 4, 5, 1]"
  "'abcdef' | skipWhile (_ < 'd')" ==> "'def'"
  "'abcdef'.skipWhile (_ < 'd')" ==> "'def'"

  // skipUntil
  "[1, 2, 3, 4, 5, 1] | skipUntil (_ > 3)" ==> "[4, 5, 1]"
  "'abcdef' | skipUntil (_ > 'd')" ==> "'ef'"
  "'abcdef'.skipUntil (_ > 'd')" ==> "'ef'"

  // sliding
  "[1, 2, 3] | sliding 2" ==> "[[1, 2], [2, 3]]"
  "'abc' | sliding 2" ==> "['ab', 'bc']"
  "'abc'.sliding 2" ==> "['ab', 'bc']"

  // sum
  " [] | sum " ==> 0
  " [1, 2, 3] | sum " ==> 6
  " [1.bytes].sum.tag " ==> " 1.bytes.tag "
  "['foo', 'bar'] | sum" ==> "'foobar'"
  "[[1, 2], [3]] | sum" ==> "[1, 2, 3]"
  "sum '' []" ==> "''"

  // sumBy
  " ['a', 'bb', 'ccc'] | sumBy length " ==> 6
  " [1.bytes].sumBy (_) | _.tag " ==> " 1.bytes.tag "
  "sumBy (_.toNumber) '123'" ==> 6
  "sumBy (_.toNumber) '' []" ==> "''"

  // takeWhile
  "[1, 2, 3, 4, 3].takeWhile (_ <= 3)" ==> "[1, 2, 3]"
  "[1, 2, 3, 4, 3] | takeWhile (_ <= 3)" ==> "[1, 2, 3]"
  "'abcded'.takeWhile (_ <= 'c')" ==> "'abc'"

  // unique
  "unique [1, 2, 3, 2, 1]" ==> "[1, 2, 3]"
  "unique 'abcba'" ==> "'abc'"
  "'abcba'.unique" ==> "'abc'"

  // where
  "[1, 2, 3] | where (_ > 2)" ==> "[3]"
  "'foobar' | where (_ > 'm')" ==> "'oor'"
  "'foobar'.where (_ > 'm')" ==> "'oor'"

  // whereNot
  "[1, 2, 3] | whereNot (_ > 2)" ==> "[1, 2]"
  "'foobar' | whereNot (_ > 'm')" ==> "'fba'"
  "'foobar'.whereNot (_ > 'm')" ==> "'fba'"

  // zip
  "zip [1, 2, 3] [4, 5]" ==> "[[1, 4], [2, 5]]"

}
