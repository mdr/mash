package com.github.mdr.mash.evaluator

class HelpTest extends AbstractEvaluatorTest {

  "listFiles?" ==> "safe listFiles"
  "help groupBy" ==> "groupBy?"
  "man groupBy" ==> "groupBy?"

  "[1, 2, 3].reverse? .name" ==> "'reverse'"
  "[1, 2, 3].sortBy? .name" ==> "'sortBy'"
  "help [1,2, 3].sortBy" ==> "[1, 2, 3].sortBy?"
  "[1, 2, 3].sortBy.help" ==> "[1, 2, 3].sortBy?"

  "pwd.info.permissions? .name" ==> "'permissions'"
  "[pwd].info.permissions? .name" ==> "'permissions'"

  "git.log?" ==> "safe git.log"
  "help (42.getClass) | _.name" ==> "'Number'"

  "help pwd" ==> "pwd?"
  "help [].reverse" ==> "[].reverse?"

}
