package com.github.mdr.mash.parser

import com.github.mdr.mash.parser.SafeParens.safeParens
import org.scalatest.{ FlatSpec, Matchers }

class SafeParensTest extends FlatSpec with Matchers {

  "SafeParens" should "add parens when required" in {
    safeParens("f", " a") shouldEqual "(f) a"
    safeParens("f a", " b") shouldEqual "(f a) b"
    safeParens("1 + 2", ".negate") shouldEqual "(1 + 2).negate"
    safeParens("a => b", " | f") shouldEqual "(a => b) | f"
    safeParens("1 + 2", " * 3") shouldEqual "(1 + 2) * 3"
  }

  "SafeParens" should "not add parens if not required" in {
    safeParens("1", " + 2") shouldEqual "1 + 2"
    safeParens("items", ".reverse") shouldEqual "items.reverse"
    safeParens("items", "[0]") shouldEqual "items[0]"
    safeParens("a | f", " | g") shouldEqual "a | f | g"
    safeParens("a | b => f b", " | g") shouldEqual "a | b => f b | g"
    safeParens("42", " | f") shouldEqual "42 | f"
    safeParens("1 + 2", " | f") shouldEqual "1 + 2 | f"
    safeParens("1 * 2", " + 3") shouldEqual "1 * 2 + 3"
    safeParens("1 - 2", " - 3") shouldEqual "1 - 2 - 3"
  }

}
