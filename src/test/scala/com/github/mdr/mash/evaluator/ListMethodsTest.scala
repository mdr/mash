package com.github.mdr.mash.evaluator

class ListMethodsTest extends AbstractEvaluatorTest {

  "[1, 2, 3].intersect [2, 3, 4]" ==> "[2, 3]"
  "[1, 2].append 3" ==> "[1, 2, 3]"
  "[1, 2].prepend 0" ==> "[0, 1, 2]"

}
