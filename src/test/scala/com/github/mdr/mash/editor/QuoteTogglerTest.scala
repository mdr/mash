package com.github.mdr.mash.editor

import com.github.mdr.mash.repl.LineBufferTestHelper.lineBuffer
import org.scalatest.{ FlatSpec, Matchers }

class QuoteTogglerTest extends FlatSpec with Matchers {

  // ▶ or ◀ points to the cursor position (and is removed from the string)
  "foo bar▶ baz" ==> """foo "bar"▶ baz"""
  """foo "bar"▶ baz""" ==> "foo bar▶ baz"

  "foo b▶ar baz" ==> """foo "b▶ar" baz"""
  """foo "b▶ar" baz""" ==> "foo b▶ar baz"

  "foo ▶bar baz" ==> """foo "▶bar" baz"""
  """foo "▶bar" baz""" ==> "foo ▶bar baz"

  """foo ▶"bar" baz""" ==> "foo ▶bar baz"

  "foo▶" ==> """"foo"▶"""
  """"foo▶"""" ==> "foo▶"
  "'foo▶'" ==> "foo▶"

  "▶" ==> "▶"

  """"foo▶""" ==> """"foo"▶"""
  """"fo▶o""" ==> """"fo▶o""""

  """foo"▶""" ==> """"foo"▶"""

  """$▶""" ==> """"`$"▶"""

  "fo▶o" ==> """"fo▶o""""
  """foo▶"""" ==> """"foo▶""""
  """"f▶oo""" ==> """"f▶oo""""
  """▶"foo""" ==> """▶"foo""""

  "foo ▶ bar" ==> "foo ▶ bar"

  """foo "▶""" ==> """foo ""▶"""

  """"`$"▶""" ==> "$▶"

  "baz ▷foo bar▶ baz" ==> """baz "foo bar"▶ baz"""

  implicit class RichString(s: String) {
    def ==>(expectedStr: String) {
      "QuoteToggler" should s"transform $s into $expectedStr" in {
        val expected = lineBuffer(expectedStr)
        val input = lineBuffer(s)
        val actual = QuoteToggler.toggleQuotes(input, mish = false)
        actual should equal(expected)
      }
    }
  }

}