package com.github.mdr.mash.evaluator

class VariadicParameterTest extends AbstractEvaluatorTest {

  "def mkList n... = n; mkList 1 2 3" ==> "[1, 2, 3]"
  "def mkList n... = n; mkList" ==> "[]"
  "def mkList a b c n... = n + [a, b, c]; mkList 1 2 3 4 5" ==> "[4, 5, 1, 2, 3]"
  "def mkList (n...) = n; mkList 1 2 3" ==> "[1, 2, 3]"
  "def mkList (xs...) = xs; mkList --xs=42".shouldThrowAnException

  "(x... => x.sum) 1 2 3" ==> 6

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
