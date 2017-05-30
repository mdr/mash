package com.github.mdr.mash.evaluator

class FlagParamTest extends AbstractEvaluatorTest {

  // @flag
  "def foo (@flag m = 10) n = m + n; foo 3" ==> 13
  "def foo (@flag m) n = m + n; foo 3 --m=20" ==> 23
  "def twice (@lazy @flag body) = (body; body); a = 0; twice --body=(a += 1); a" ==> 2
  "def twice (@lazy @flag body) = (body; body); a = 0; twice (a += 1); a".shouldThrowAnException

  // @shortFlag
  """def doSomething (@flag @(shortFlag 'd') dryRun = false) =
    |  if dryRun then 'Dry run' else 'For reals'
    |doSomething -d
    | """.stripMargin ==> "'Dry run'"

}
