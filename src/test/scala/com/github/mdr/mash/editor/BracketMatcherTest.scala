package com.github.mdr.mash.editor

import com.github.mdr.mash.repl.LineBufferTestHelper.parseLineBuffer
import org.scalatest.{ FlatSpec, Matchers }

class BracketMatcherTest extends FlatSpec with Matchers {

  "▶(3 + 5)" shouldMatch "(3 + 5)◀"
  "(3 + 5)◀" shouldMatch "▶(3 + 5)"

  "square▶(3 + 5)" shouldMatch "square(3 + 5)◀"
  "square(3 + 5)◀" shouldMatch "square▶(3 + 5)"

  "def square ▶(n) = n * n" shouldMatch "def square (n)◀ = n * n"
  "def square (n)◀ = n * n" shouldMatch "def square ▶(n) = n * n"

  "@▶(attribute 10) class X" shouldMatch "@(attribute 10)◀ class X"
  "@(attribute 10)◀ class X" shouldMatch "@▶(attribute 10) class X"

  "xs▶[0]" shouldMatch "xs[0]◀"
  "xs[0]◀" shouldMatch "xs▶[0]"

  "▶[1, 2]" shouldMatch "[1, 2]◀"
  "[1, 2]◀" shouldMatch "▶[1, 2]"

  "▶[a, b] = c" shouldMatch "[a, b]◀ = c"
  "[a, b]◀ = c" shouldMatch "▶[a, b] = c"

  "▶{ x; y }" shouldMatch "{ x; y }◀"
  "{ x; y }◀" shouldMatch "▶{ x; y }"

  "▶{ x: y }" shouldMatch "{ x: y }◀"
  "{ x: y }◀" shouldMatch "▶{ x: y }"

  "▶{ x: y } = z" shouldMatch "{ x: y }◀ = z"
  "{ x: y }◀ = z" shouldMatch "▶{ x: y } = z"

  "class X ▶{  }" shouldMatch "class X {  }◀"
  "class X {  }◀" shouldMatch "class X ▶{  }"

  """ "$▶{pwd}" """ shouldMatch """ "${pwd}◀" """
  """ "${pwd}◀" """ shouldMatch """ "$▶{pwd}" """

  "!▶{ ${pwd} }" shouldMatch "!{ ${pwd} }◀"
  "!{ ${pwd} }◀" shouldMatch "!▶{ ${pwd} }"

  "!!▶{ ${pwd} }" shouldMatch "!!{ ${pwd} }◀"
  "!!{ ${pwd} }◀" shouldMatch "!!▶{ ${pwd} }"

  implicit class RichString(input: String) {

    def shouldMatch(expected: String): Unit = {
      "Bracket matcher" should s"find a matching bracket within '$input': '$expected'" in {
        val inputBuffer = parseLineBuffer(input)
        val Some(actualOffset) = BracketMatcher.findMatchingBracket(inputBuffer.text, inputBuffer.cursorOffset, mish = false)
        actualOffset should equal(parseLineBuffer(expected).cursorOffset)
      }
    }
  }

}
