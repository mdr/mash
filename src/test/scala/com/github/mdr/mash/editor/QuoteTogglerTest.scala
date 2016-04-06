package com.github.mdr.mash.editor

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import com.github.mdr.mash.LineBuffer

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
    
  implicit class RichString(s: String) {
    def ==>(expectedStr: String) {
      "QuoteToggler" should s"transform $s into $expectedStr" in {
        val expected = parse(expectedStr)
        val input = parse(s)
        val actual = QuoteToggler.toggleQuotes(input, mish = false)
        actual should equal(expected)
      }
    }
  }

  private def indexOf(s: String, s2: String): Option[Int] = s.indexOf(s2) match {
    case -1 ⇒ None
    case n  ⇒ Some(n)
  }

  private def parse(s: String): LineBuffer = {
    val pos = indexOf(s, "▶").orElse(indexOf(s, "◀").map(_ - 1)).getOrElse(
      throw new IllegalArgumentException("No cursor position provided in test case"))
    val text = s.filterNot(c ⇒ c == '▶' || c == '◀')
    LineBuffer(text, pos)
  }

}