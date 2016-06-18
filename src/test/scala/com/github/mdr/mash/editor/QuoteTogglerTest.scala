package com.github.mdr.mash.editor

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import com.github.mdr.mash.repl.LineBuffer
import com.github.mdr.mash.repl.LineBufferTestHelper

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

  """$▶""" ==> """"\$"▶"""

  "fo▶o" ==> """"fo▶o""""
  """foo▶"""" ==> """"foo▶""""
  """"f▶oo""" ==> """"f▶oo""""
  """▶"foo""" ==> """▶"foo""""

  "foo ▶ bar" ==> "foo ▶ bar"

  """foo "▶""" ==> """foo ""▶"""

  """"\$"▶""" ==> "$▶"

  implicit class RichString(s: String) {
    def ==>(expectedStr: String) {
      "QuoteToggler" should s"transform $s into $expectedStr" in {
        val expected = LineBufferTestHelper.parseLineBuffer(expectedStr)
        val input = LineBufferTestHelper.parseLineBuffer(s)
        val actual = QuoteToggler.toggleQuotes(input, mish = false)
        actual should equal(expected)
      }
    }
  }

}