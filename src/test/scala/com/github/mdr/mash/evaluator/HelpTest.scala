package com.github.mdr.mash.evaluator

class HelpTest extends AbstractEvaluatorTest {

  "listFiles? .name" ==> "'listFiles'"
  "help groupBy" ==> "groupBy?"
  "man groupBy" ==> "groupBy?"
  "groupBy.help" ==> "groupBy?"

  "[1, 2, 3].reverse? .name" ==> "'reverse'"
  "[1, 2, 3].sortBy? .name" ==> "'sortBy'"
  "help [1,2, 3].sortBy" ==> "[1, 2, 3].sortBy?"
  "[1, 2, 3].sortBy.help" ==> "[1, 2, 3].sortBy?"

  "pwd.info.permissions? .name" ==> "'permissions'"
  "[pwd].info.permissions? .name" ==> "'permissions'"

  "git.log? .name" ==> "'log'"
  "help (42.getClass) | _.name" ==> "'Number'"

  "help pwd" ==> "pwd?"
  "help [].reverse" ==> "[].reverse?"

}
