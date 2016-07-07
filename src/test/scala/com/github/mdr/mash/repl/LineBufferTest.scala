package com.github.mdr.mash.repl

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import com.github.mdr.mash.parser.AbstractSyntax._

class LineBufferTest extends FlatSpec with Matchers {

  "LineBuffer" should "let you move the cursor right one character" in {
    lineBuffer("▶abc").cursorRight should equal(lineBuffer("a▶bc"))
    lineBuffer("▶").cursorRight should equal(lineBuffer("▶"))
    lineBuffer("a▶").cursorRight should equal(lineBuffer("a▶"))
    lineBuffer("▶a").cursorRight should equal(lineBuffer("a▶"))
    
    lineBuffer("""|abc▶
                  |def""").cursorRight should equal(
      lineBuffer("""|abc
                    |▶def"""))
    
    lineBuffer("""|abc
                  |def▶""").cursorRight should equal(
      lineBuffer("""|abc
                    |def▶"""))
  }

  it should "let you move the cursor left one character" in {
    lineBuffer("abc▶").cursorLeft should equal(lineBuffer("ab▶c"))
    lineBuffer("▶").cursorLeft should equal(lineBuffer("▶"))
    lineBuffer("▶a").cursorLeft should equal(lineBuffer("▶a"))
    lineBuffer("a▶").cursorLeft should equal(lineBuffer("▶a"))
    
    lineBuffer("""|abc
                  |▶def""").cursorLeft should equal(
      lineBuffer("""|abc▶
                    |def"""))
  }
  
  it should "let you delete forward a word" in {
    lineBuffer("▶foo").deleteForwardWord should equal(lineBuffer("▶"))
    lineBuffer("fo▶o").deleteForwardWord should equal(lineBuffer("fo▶"))
    lineBuffer("foo▶").deleteForwardWord should equal(lineBuffer("foo▶"))
    lineBuffer("foo ▶bar baz").deleteForwardWord should equal(lineBuffer("foo ▶ baz"))
    lineBuffer("▶").deleteForwardWord should equal(lineBuffer("▶"))
  }

  it should "let you delete backwards a word" in {
    lineBuffer("▶foo").deleteBackwardWord should equal(lineBuffer("▶foo"))
    lineBuffer("fo▶o").deleteBackwardWord should equal(lineBuffer("▶o"))
    lineBuffer("foo▶").deleteBackwardWord should equal(lineBuffer("▶"))
    lineBuffer("foo bar▶ baz").deleteBackwardWord should equal(lineBuffer("foo ▶ baz"))
    lineBuffer("▶").deleteBackwardWord should equal(lineBuffer("▶"))
  }

  it should "let you move backwards a word" in {
    lineBuffer("foo bar▶ baz").backwardWord should equal(lineBuffer("foo ▶bar baz"))
    lineBuffer("foo bar ▶baz").backwardWord should equal(lineBuffer("foo ▶bar baz"))
  }

  it should "let you move forwards a word" in {
    lineBuffer("foo bar ▶baz").forwardWord should equal(lineBuffer("foo bar baz▶"))
    lineBuffer("foo bar▶ baz").forwardWord should equal(lineBuffer("foo bar baz▶"))
  }

  it should "let you delete a character at the cursor position" in {
    lineBuffer("▶123").delete should equal(lineBuffer("▶23"))
    lineBuffer("▶").delete should equal(lineBuffer("▶"))
    lineBuffer("123▶").delete should equal(lineBuffer("123▶"))
    lineBuffer("1▶23").delete should equal(lineBuffer("1▶3"))
  }

  it should "let you delete a character before the cursor position (backspace)" in {
    lineBuffer("123▶").backspace should equal(lineBuffer("12▶"))
    lineBuffer("▶").backspace should equal(lineBuffer("▶"))
    lineBuffer("▶123").backspace should equal(lineBuffer("▶123"))
    lineBuffer("12▶3").backspace should equal(lineBuffer("1▶3"))
  }

  private def lineBuffer(s: String) = LineBufferTestHelper.parseLineBuffer(s.stripMargin)

}