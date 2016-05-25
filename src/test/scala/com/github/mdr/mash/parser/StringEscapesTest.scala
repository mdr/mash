package com.github.mdr.mash.parser

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class StringEscapesTest extends FlatSpec with Matchers {

  "Unescape" should "work" in {
    StringEscapes.unescape("") should equal("")
    StringEscapes.unescape("a") should equal("a")
    StringEscapes.unescape("""\n""") should equal("\n")
    StringEscapes.unescape("""\t""") should equal("\t")
    StringEscapes.unescape("""\r""") should equal("\r")
    StringEscapes.unescape("""\~""") should equal("~")
    StringEscapes.unescape("""\$""") should equal("$")
    StringEscapes.unescape("""\'""") should equal("'")
    StringEscapes.unescape(""" \" """) should equal(""" " """)
    StringEscapes.unescape("""\""") should equal("""\""")
    StringEscapes.unescape("""\a""") should equal("a")
  }

}