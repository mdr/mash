package com.github.mdr.mash.evaluator

class VariadicParameterTest extends AbstractEvaluatorTest {

  "def mkList n... = n; mkList 1 2 3" ==> "[1, 2, 3]"
  "def mkList n... = n; mkList" ==> "[]"
  "def mkList a b c n... = n + [a, b, c]; mkList 1 2 3 4 5" ==> "[4, 5, 1, 2, 3]"
  "def mkList (n...) = n; mkList 1 2 3" ==> "[1, 2, 3]"
  "def mkList (xs...) = xs; mkList --xs=42".shouldThrowAnException

  "(x... => x.sum) 1 2 3" ==> 6

  // Parameters after varargs
  "def foo bar... (baz=42) = { bar, baz }; foo 1 2 3" ==> "{ bar: [1, 2], baz: 3 }"
  "def foo bar... baz = { bar, baz }; foo 1 2 3" ==> "{ bar: [1, 2], baz: 3 }"
  "def foo bar... baz = { bar, baz }; foo 1" ==> "{ bar: [], baz: 1 }"

  "def foo bar... baz = { bar, baz }; foo 1 --bar=[2, 3]" ==> "{ bar: [2, 3], baz: 1 }"

  havingFirstRun("def f a (b=2) c... (d=4) e = { a, b, c, d, e }") { implicit env ⇒
    "f 10 50" ==> "{ a: 10, b: 2, c: [], d: 4, e: 50 }"
    "f 10 20 50" ==> "{ a: 10, b: 20, c: [], d: 4, e: 50 }"
    "f 10 20 40 50" ==> "{ a: 10, b: 20, c: [], d: 40, e: 50 }"
    "f 10 20 30 40 50" ==> "{ a: 10, b: 20, c: [30], d: 40, e: 50 }"
    "f 20 50 --a=10" ==> "{ a: 10, b: 20, c: [], d: 4, e: 50 }"
  }

  // @flatten
  "def f (@flatten args...) = { args }; f 1 2 3" ==> "{ args: [1, 2, 3] }"
  "def f (@flatten args...) = { args }; f [1, 2, 3]" ==> "{ args: [1, 2, 3] }"
  "def f (args...) = { args }; f [1, 2, 3]" ==> "{ args: [[1, 2, 3]] }"

  // @atLeastOne
  havingFirstRun("def f (@atLeastOne args...) = { args }") { implicit env ⇒
    "f" ==> "f"
  }
  havingFirstRun("def f (args...) = { args }") { implicit env ⇒
    "f" ==> "{ args: [] }"
  }

}
