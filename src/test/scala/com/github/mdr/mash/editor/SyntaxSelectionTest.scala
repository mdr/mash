package com.github.mdr.mash.editor

import com.github.mdr.mash.editor.SyntaxSelection.expandSelection
import org.scalatest.{ FlatSpec, Matchers }
import com.github.mdr.mash.repl.LineBufferTestHelper.lineBuffer

class SyntaxSelectionTest extends FlatSpec with Matchers {

  "iden▶tifier" ==> "▷identifier▶"

  "foo + ba▶r" ==> "foo + ▷bar▶"
  "foo + ▶bar" ==> "foo + ▷bar▶"
  "foo + bar▶" ==> "▷foo + bar▶"

  "foo.ba▶r" ==> "foo.▷bar▶"

  "foo --ba▶r=baz" ==> "foo ▷--bar▶=baz"

  "1 + 2▶ * 3" ==> "1 + ▷2 * 3▶"

  "1 + 2▶ * 3 * 4" ==> "1 + ▷2 * 3▶ * 4"
  "1 + ▷2 * 3▶ * 4" ==> "1 + ▷2 * 3 * 4▶"
  "1 + ▷2 * 3 * 4▶" ==> "▷1 + 2 * 3 * 4▶"

  "def foo = 4▶2" ==> "def foo = ▷42▶"

  "def foo = 42 # comm▶ent" ==> "def foo = 42 ▷# comment▶"
  "def foo = 42 ▷# comment▶" ==> "▷def foo = 42 # comment▶"

  implicit class RichString(s: String) {
    def ==>(expectedStr: String) {
      "Expanding selection using AST" should s"expand $s into $expectedStr" in {
        val expected = lineBuffer(expectedStr)
        val input = lineBuffer(s)
        val actual = input.withSelection(expandSelection(input, mish = false).get)
        actual should equal(expected)
      }
    }
  }

}
