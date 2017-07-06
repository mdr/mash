package com.github.mdr.mash.parser

import com.github.mdr.mash.parser.ExpressionCombiner.combineSafely
import org.scalatest.{ FlatSpec, Matchers }

class ExpressionCombinerTest extends FlatSpec with Matchers {

  "ExpressionCombiner" should "add parens when required" in {
    combineSafely("f", " a") shouldEqual "(f) a"
    combineSafely("f a", " b") shouldEqual "(f a) b"
    combineSafely("1 + 2", ".negate") shouldEqual "(1 + 2).negate"
    combineSafely("a => b", " | f") shouldEqual "(a => b) | f"
    combineSafely("1 + 2", " * 3") shouldEqual "(1 + 2) * 3"
  }

  "ExpressionCombiner" should "not add parens if not required" in {
    combineSafely("1", " + 2") shouldEqual "1 + 2"
    combineSafely("items", ".reverse") shouldEqual "items.reverse"
    combineSafely("items", "[0]") shouldEqual "items[0]"
    combineSafely("a | f", " | g") shouldEqual "a | f | g"
    combineSafely("a | b => f b", " | g") shouldEqual "a | b => f b | g"
    combineSafely("42", " | f") shouldEqual "42 | f"
    combineSafely("1 + 2", " | f") shouldEqual "1 + 2 | f"
    combineSafely("1 * 2", " + 3") shouldEqual "1 * 2 + 3"
    combineSafely("1 - 2", " - 3") shouldEqual "1 - 2 - 3"
  }

}
