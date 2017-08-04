package com.github.mdr.mash.editor

import com.github.mdr.mash.editor.SyntaxSelection.expandSelection
import org.scalatest.{ FlatSpec, Matchers }
import com.github.mdr.mash.repl.LineBufferTestHelper.lineBuffer

class SyntaxSelectionTest extends FlatSpec with Matchers {

  "iden▶tifier" ==> "▷identifier▶"

  "1 + 2▶ * 3" ==> "1 + ▷2 * 3▶"

  "1 + 2▶ * 3 * 4" ==> "1 + ▷2 * 3▶ * 4"
  "1 + ▷2 * 3▶ * 4" ==> "1 + ▷2 * 3 * 4▶"
  "1 + ▷2 * 3 * 4▶" ==> "▷1 + 2 * 3 * 4▶"

  "def foo = 4▶2" ==> "def foo = ▷42▶"

  implicit class RichString(s: String) {
    def ==>(expectedStr: String) {
      "Expanding selection using AST" should s"expand $s into $expectedStr" in {
        val expected = lineBuffer(expectedStr)
        val input = lineBuffer(s)
        val actual = expandSelection(input)
        actual should equal(expected)
      }
    }
  }

}
