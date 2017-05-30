package com.github.mdr.mash.evaluator

class MinMaxTest extends AbstractEvaluatorTest {

  // max
  "max [1, 200, 3]" ==> 200
  "[1, 200, 3].max" ==> 200
  "max 'a' 'b' 'c'" ==> "'c'"
  "'abc'.max" ==> "'c'"
  "max 1 2 3" ==> 3
  "max [2, null, 1]" ==> 2
  "max [] --default=0" ==> "0"
  "[].max --default=0" ==> "0"
  "min --items=['abc']" ==> "'abc'"
  "min --items=['abc', 'def']" ==> "'abc'"

  // maxBy
  "maxBy length [ 'a', 'bbb', 'cc'] " ==> " 'bbb' "
  "maxBy (_) 'abcde'" ==> "'e'"
  "maxBy 'foo' [{ foo: null }, { foo: 42 }]" ==> "{ foo: 42 }"
  "'abcde'.maxBy (_)" ==> "'e'"
  "maxBy length [] --default=0" ==> 0
  "[].maxBy length --default=0" ==> 0

  // min
  "min [100, 2, 300]" ==> 2
  "[100, 2, 300].min" ==> 2
  "min 'a' 'b' 'c'" ==> "'a'"
  "'abc'.min" ==> "'a'"
  "min 1 2 3" ==> 1
  "min [2, null, 1]" ==> 1
  "min [] --default=0" ==> 0
  "[].min --default=0" ==> 0
  "[].min [1, 2, 3]".shouldThrowAnException // used to be a bug where we leaked an argument

  "a = now; b = 3.days.ago; min a b" ==> "b"

  // minBy
  "minBy length [ 'a', 'bbb', 'cc'] " ==> " 'a' "
  "minBy 'foo' [{ foo: null }, { foo: 42 }]" ==> "{ foo: 42 }"
  "minBy length [] --default=0" ==> 0
  "[].minBy length --default=0" ==> 0

}
