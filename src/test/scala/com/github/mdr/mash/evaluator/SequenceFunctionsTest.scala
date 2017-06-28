package com.github.mdr.mash.evaluator

class SequenceFunctionsTest extends AbstractEvaluatorTest {

  // all
  "[2, 4, 6] | all (_ < 10)" ==> true
  "[2, 4, 6] | all (_ < 5)" ==> false
  "[] | all (_ < 10)" ==> true
  "[1] | all (_)" ==> true
  "[0] | all (_)" ==> false
  "'aaa' | all (_ == 'a')" ==> true
  "'aaa'.all (_ == 'a')" ==> true

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
  """a = ''; 'abc'.each (c i => a += "$c$i"); a""" ==> "'a0b1c2'"

  // find
  "[1, 2, 300, 4, 5] | find (_ > 100)" ==> 300
  "[1, 2, 3, 4, 5] | find (_ > 100)" ==> null
  "'xxxaxxx' | find (_ != 'x')" ==> "'a'"
  "'xxxaxxx'.find (_ != 'x')" ==> "'a'"

  // grep
  "['foo', 'bar', 'baz'].grep 'b'" ==> "['bar', 'baz']"
  "['foo', 'bar', 'baz'].grep -n 'b'" ==> "['foo']"
  "['apple', { obj: 'ball' }] | grep 'b'" ==> "[{ obj: 'ball' }]"
  "['foo', 'bar', 'BAZ'] | grep -i 'b'" ==> "['bar', 'BAZ']"
  "['apple', 'ball', 'cup'] | grep -r '(a|b)'" ==> "['apple', 'ball']"
  "['APPLE', 'ball', 'cup'] | grep -r -i '(a|B)'" ==> "['APPLE', 'ball']"

  "'foo`nbar`nbaz' | grep 'b'" ==> "['bar', 'baz']"
  "'foo`nbar`nbaz'.grep 'b'" ==> "['bar', 'baz']"

  "[{ foo: 'bar' }, { foo: 'foo' }] | grep 'foo'" ==> "[{ foo: 'foo' }]"
  "{ foo: 'wibble', bar: 'wobble', wibble: 'baz' }.grep 'wibble'" ==> "{ foo: 'wibble', wibble: 'baz' }"
  "{ foo: 'wibble', bar: 'wobble', wibble: 'baz' } | grep 'wibble'" ==> "{ foo: 'wibble', wibble: 'baz' }"
  "{ a: 42 } | grep 'name'" ==> "{}"

  // indexOf
  "[1, 2, 3, 2, 1].indexOf 2" ==> 1
  "[1, 2, 3, 2, 1].indexOf 5" ==> null
  "'abcba'.indexOf 'b'" ==> 1
  "'abcba'.indexOf 'ba'" ==> 3
  "'abcba'.indexOf 'z'" ==> null

  // isEmpty
  "isEmpty []" ==> true
  "isEmpty [1, 2, 3]" ==> false
  "''.isEmpty" ==> true
  "'abc'.isEmpty" ==> false

  // length
  "length []" ==> 0
  "length [1, 2, 3]" ==> 3
  " length '' " ==> 0
  " length 'abc' " ==> 3
  " ''.length " ==> 0
  " 'abc'.length " ==> 3
  " 'abc' | length " ==> 3

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
  "[] | reduce (x y => x + y)".shouldThrowAnException

  "'foo' | reduce (acc c => acc + [c]) []" ==> "['f', 'o', 'o']"

  // reverse
  "[1, 2, 3].reverse" ==> "[3, 2, 1]"
  "[1, 2, 3] | reverse" ==> "[3, 2, 1]"

  "'trebor' | reverse" ==> "'robert'"
  "'trebor'.reverse" ==> "'robert'"

  "{ a: 1, b: 2, c: 3 }.reverse.fields.name.join" ==> "'cba'"
  "{ a: 1, b: 2, c: 3 } | reverse | .fields.name.join" ==> "'cba'"

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
  "[{ foo: 42 }, { bar: 100 }, { foo: 10 }] | sum" ==> "{ bar: 100, foo: 10 }"
  "[{ foo: 42 }, { (42): 'foo' }] | sum" ==> "{ foo: 42, (42): 'foo'}"

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
  "[1, 2, 3, 2, 1] | unique" ==> "[1, 2, 3]"
  "'abcba' | unique" ==> "'abc'"
  "'abcba'.unique" ==> "'abc'"

  // zip
  "zip [1, 2, 3] [4, 5]" ==> "[[1, 4], [2, 5]]"

}
