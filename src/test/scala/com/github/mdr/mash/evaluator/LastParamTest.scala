package com.github.mdr.mash.evaluator

class LastParamTest extends AbstractEvaluatorTest {

  "def foo n... (@last m) = m; foo 1 2 3" ==> 3
  "def foo n... (@last m) = m; foo 3" ==> 3
  "def foo (n = 10) (@last m) = m; foo 3" ==> 3

  "def fun first (@last second = 10) = { first, second }; fun 1 2" ==> "{ first: 1, second: 2 }"
  "def fun first (@last second = 10) = { first, second }; fun --first=1" ==> "{ first: 1, second: 10 }"
  "def fun first (@last second = 10) = { first, second }; fun 1".shouldThrowAnException

  "def fun args... (@last arg = 10) = { args, arg }; fun" ==> "{ args: [], arg: 10 }"
  "def fun args... (@last arg = 10) = { args, arg }; fun 1" ==> "{ args: [], arg: 1 }"
  "def fun args... (@last arg = 10) = { args, arg }; fun 1 2" ==> "{ args: [1], arg: 2 }"
  "def fun args... (@last arg = 10) = { args, arg }; fun --arg=20" ==> "{ args: [], arg: 20 }"
  "def fun args... (@last arg = 10) = { args, arg }; fun 1 --arg=20" ==> "{ args: [1], arg: 20 }"

}
