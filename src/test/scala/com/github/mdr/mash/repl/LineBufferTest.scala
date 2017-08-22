package com.github.mdr.mash.repl

import com.github.mdr.mash.utils.Region
import org.scalatest.{ FlatSpec, Matchers }

class LineBufferTest extends FlatSpec with Matchers {

  "LineBuffer" should "let you move the cursor right one character" in {
    lineBuffer("▶abc").cursorRight() shouldEqual lineBuffer("a▶bc")
    lineBuffer("▶").cursorRight() shouldEqual lineBuffer("▶")
    lineBuffer("a▶").cursorRight() shouldEqual lineBuffer("a▶")
    lineBuffer("▶a").cursorRight() shouldEqual lineBuffer("a▶")

    lineBuffer(
      """abc▶
        |def""").cursorRight() shouldEqual lineBuffer(
      """abc
        |▶def""")

    lineBuffer(
      """abc
        |def▶""").cursorRight() shouldEqual lineBuffer(
      """abc
        |def▶""")
  }

  it should "let you move the cursor right, extending selection" in {
    lineBuffer("▶abc").cursorRight(extendSelection = true) shouldEqual lineBuffer("▷a▶bc")
    lineBuffer("▷a▶bc").cursorRight(extendSelection = true) shouldEqual lineBuffer("▷ab▶c")
    lineBuffer("▷ab▶c").cursorRight(extendSelection = true) shouldEqual lineBuffer("▷abc▶")
    lineBuffer("▷abc▶").cursorRight(extendSelection = true) shouldEqual lineBuffer("▷abc▶")

    lineBuffer("▶").cursorRight(extendSelection = true) shouldEqual lineBuffer("▶")
    lineBuffer("a▶").cursorRight(extendSelection = true) shouldEqual lineBuffer("a▶")
    lineBuffer("▶a").cursorRight(extendSelection = true) shouldEqual lineBuffer("▷a▶")

    lineBuffer(
      """abc▶
        |def""").cursorRight(extendSelection = true) shouldEqual lineBuffer(
      """abc▷
        |▶def""")
  }

  it should "let you move the cursor left one character" in {
    lineBuffer("abc▶").cursorLeft() shouldEqual lineBuffer("ab▶c")
    lineBuffer("▶").cursorLeft() shouldEqual lineBuffer("▶")
    lineBuffer("▶a").cursorLeft() shouldEqual lineBuffer("▶a")
    lineBuffer("a▶").cursorLeft() shouldEqual lineBuffer("▶a")

    lineBuffer(
      """abc
        |▶def""").cursorLeft() shouldEqual lineBuffer(
      """abc▶
        |def""")
  }

  it should "let you move the cursor left, extending selection" in {
    lineBuffer("abc▶").cursorLeft(extendSelection = true) shouldEqual lineBuffer("ab▶c▷")
    lineBuffer("ab▶c▷").cursorLeft(extendSelection = true) shouldEqual lineBuffer("a▶bc▷")
    lineBuffer("a▶bc▷").cursorLeft(extendSelection = true) shouldEqual lineBuffer("▶abc▷")
    lineBuffer("▶abc▷").cursorLeft(extendSelection = true) shouldEqual lineBuffer("▶abc▷")

    lineBuffer("▶").cursorLeft(extendSelection = true) shouldEqual lineBuffer("▶")
    lineBuffer("▶a").cursorLeft(extendSelection = true) shouldEqual lineBuffer("▶a")
    lineBuffer("a▶").cursorLeft(extendSelection = true) shouldEqual lineBuffer("▶a▷")

    lineBuffer(
      """abc
        |▶def""").cursorLeft(extendSelection = true) shouldEqual lineBuffer(
      """abc▶
        |▷def""")
  }

  it should "let you move the cursor up" in {
    lineBuffer("abc▶").cursorUp() shouldEqual lineBuffer("abc▶")
    lineBuffer("▶").cursorUp() shouldEqual lineBuffer("▶")

    lineBuffer(
      """abc
        |▶def""").cursorUp() shouldEqual lineBuffer(
      """▶abc
        |def""")

    lineBuffer(
      """abcdef
        |ghi▶""").cursorUp() shouldEqual lineBuffer(
      """abc▶def
        |ghi""")

    lineBuffer(
      """abc
        |defghi▶""").cursorUp() shouldEqual lineBuffer(
      """abc▶
        |defghi""")

    lineBuffer(
      """
        |▶def""").cursorUp() shouldEqual lineBuffer(
      """▶
        |def""")

    lineBuffer(
      """abcdef
        |ghi▶""").cursorUp(extendSelection = true) shouldEqual lineBuffer(
      """abc▶def
        |ghi▷""")
  }

  it should "let you move the cursor down" in {
    lineBuffer("abc▶").cursorDown() shouldEqual lineBuffer("abc▶")
    lineBuffer("▶").cursorDown() shouldEqual lineBuffer("▶")

    lineBuffer(
      """▶abc
        |def""").cursorDown() shouldEqual lineBuffer(
      """abc
        |▶def""")

    lineBuffer(
      """abc▶def
        |ghi""").cursorDown() shouldEqual lineBuffer(
      """abcdef
        |ghi▶""")

    lineBuffer(
      """abc▶
        |defghi""").cursorDown() shouldEqual lineBuffer(
      """abc
        |def▶ghi""")

    lineBuffer(
      """▶
        |def""").cursorDown() shouldEqual lineBuffer(
      """
        |▶def""")

    lineBuffer(
      """abc▶def
        |ghi""").cursorDown(extendSelection = true) shouldEqual lineBuffer(
      """abc▷def
        |ghi▶""")
  }

  it should "abandon selection if you move the cursor without extending selection" in {
    lineBuffer("▷a▶bc").cursorRight() shouldEqual lineBuffer("ab▶c")
    lineBuffer("▷abc▶").cursorRight() shouldEqual lineBuffer("abc▶")
    lineBuffer("a▶bc▷").cursorLeft() shouldEqual lineBuffer("▶abc")
    lineBuffer("▶abc▷").cursorLeft() shouldEqual lineBuffer("▶abc")
    lineBuffer("▶abc▷").cursorUp() shouldEqual lineBuffer("▶abc")
    lineBuffer("▶abc▷").cursorDown() shouldEqual lineBuffer("▶abc")
  }

  it should "let you add a character at the cursor" in {
    lineBuffer("▶").insertAtCursor('a') shouldEqual lineBuffer("a▶")
    lineBuffer("ab▶").insertAtCursor('c') shouldEqual lineBuffer("abc▶")
    lineBuffer("a▶c").insertAtCursor('b') shouldEqual lineBuffer("ab▶c")

    lineBuffer(
      """abc
        |▶""").insertAtCursor('d') shouldEqual lineBuffer(
      """abc
        |d▶""")
  }

  it should "replace any selection when adding character at the cursor" in {
    lineBuffer("a▷bcd▶e").insertAtCursor('X') shouldEqual lineBuffer("aX▶e")
    lineBuffer("a▷bcd▶e").insertAtCursor("XYZ") shouldEqual lineBuffer("aXYZ▶e")
    lineBuffer("a▶bcd▷e").insertAtCursor('X') shouldEqual lineBuffer("aX▶e")
    lineBuffer("▷a▶").insertAtCursor('X') shouldEqual lineBuffer("X▶")
    lineBuffer("▶a▷").insertAtCursor('X') shouldEqual lineBuffer("X▶")
  }

  it should "let you add a newline at the cursor" in {
    lineBuffer("▶").insertAtCursor('\n') shouldEqual lineBuffer(
      """
        |▶""")
  }

  it should "let you delete a character at the cursor position" in {
    lineBuffer("▶123").delete shouldEqual lineBuffer("▶23")
    lineBuffer("▶").delete shouldEqual lineBuffer("▶")
    lineBuffer("123▶").delete shouldEqual lineBuffer("123▶")
    lineBuffer("1▶23").delete shouldEqual lineBuffer("1▶3")

    lineBuffer("1▷234▶5").delete shouldEqual lineBuffer("1▶5")
    lineBuffer("1▶234▷5").delete shouldEqual lineBuffer("1▶5")
    lineBuffer("1▷2345▶").delete shouldEqual lineBuffer("1▶")
  }

  it should "let you delete a character before the cursor position (backspace)" in {
    lineBuffer("123▶").backspace shouldEqual lineBuffer("12▶")
    lineBuffer("▶").backspace shouldEqual lineBuffer("▶")
    lineBuffer("▶123").backspace shouldEqual lineBuffer("▶123")
    lineBuffer("12▶3").backspace shouldEqual lineBuffer("1▶3")

    lineBuffer(
      """abc
        |▶""").backspace shouldEqual lineBuffer("abc▶")
    lineBuffer(
      """abc
        |▶def""").backspace shouldEqual lineBuffer("abc▶def")

    lineBuffer("1▷234▶5").backspace shouldEqual lineBuffer("1▶5")
    lineBuffer("1▶234▷5").backspace shouldEqual lineBuffer("1▶5")
    lineBuffer("1▷2345▶").backspace shouldEqual lineBuffer("1▶")
  }

  it should "let you delete forward a word" in {
    lineBuffer("▶foo").deleteForwardWord shouldEqual lineBuffer("▶")
    lineBuffer("fo▶o").deleteForwardWord shouldEqual lineBuffer("fo▶")
    lineBuffer("foo▶").deleteForwardWord shouldEqual lineBuffer("foo▶")
    lineBuffer("foo ▶bar baz").deleteForwardWord shouldEqual lineBuffer("foo ▶ baz")
    lineBuffer("▶").deleteForwardWord shouldEqual lineBuffer("▶")

    lineBuffer("1▷234▶5").deleteForwardWord shouldEqual lineBuffer("1▶5")
  }

  it should "let you delete backwards a word" in {
    lineBuffer("▶foo").deleteBackwardWord shouldEqual lineBuffer("▶foo")
    lineBuffer("fo▶o").deleteBackwardWord shouldEqual lineBuffer("▶o")
    lineBuffer("foo▶").deleteBackwardWord shouldEqual lineBuffer("▶")
    lineBuffer("foo bar▶ baz").deleteBackwardWord shouldEqual lineBuffer("foo ▶ baz")
    lineBuffer("▶").deleteBackwardWord shouldEqual lineBuffer("▶")
    lineBuffer("1▷234▶5").deleteBackwardWord shouldEqual lineBuffer("1▶5")
  }

  it should "let you move backwards a word" in {
    lineBuffer("foo bar▶ baz").backwardWord() shouldEqual lineBuffer("foo ▶bar baz")
    lineBuffer("foo bar ▶baz").backwardWord() shouldEqual lineBuffer("foo ▶bar baz")
    lineBuffer("▶foo bar baz").backwardWord() shouldEqual lineBuffer("▶foo bar baz")

    lineBuffer("foo ▶bar▷ baz").backwardWord() shouldEqual lineBuffer("▶foo bar baz")
    lineBuffer("foo ▶bar▷ baz").backwardWord(extendSelection = true) shouldEqual lineBuffer("▶foo bar▷ baz")
  }

  it should "let you move forwards a word" in {
    lineBuffer("foo bar ▶baz").forwardWord() shouldEqual lineBuffer("foo bar baz▶")
    lineBuffer("foo bar▶ baz").forwardWord() shouldEqual lineBuffer("foo bar baz▶")
    lineBuffer("foo bar baz▶").forwardWord() shouldEqual lineBuffer("foo bar baz▶")
    lineBuffer(
      """abc
        |abcd▶""").forwardWord() shouldEqual lineBuffer(
      """abc
        |abcd▶""")
    lineBuffer(
      """abc
        |abcd▶ """).forwardWord() shouldEqual lineBuffer(
      """abc
        |abcd ▶""")

    lineBuffer("foo ▷bar ▶baz").forwardWord() shouldEqual lineBuffer("foo bar baz▶")
    lineBuffer("foo ▷bar ▶baz").forwardWord(extendSelection = true) shouldEqual lineBuffer("foo ▷bar baz▶")
  }

  it should "let you move to the beginning of the line" in {
    lineBuffer("abc▶").moveCursorToStart() shouldEqual lineBuffer("▶abc")
    lineBuffer("abc▶def").moveCursorToStart() shouldEqual lineBuffer("▶abcdef")
    lineBuffer("▶abc").moveCursorToStart() shouldEqual lineBuffer("▶abc")
    lineBuffer("▶").moveCursorToStart() shouldEqual lineBuffer("▶")

    lineBuffer(
      """abc
        |def▶""".stripMargin).moveCursorToStart() shouldEqual lineBuffer(
      """abc
        |▶def""".stripMargin)

    lineBuffer(
      """abc
        |def▶""".stripMargin).moveCursorToStart(extendSelection = true) shouldEqual lineBuffer(
      """abc
        |▶def▷""".stripMargin)

    lineBuffer("ab▷c▶").moveCursorToStart() shouldEqual lineBuffer("▶abc")
  }

  it should "let you move to the end of the line" in {
    lineBuffer("abc▶").moveCursorToEnd() shouldEqual lineBuffer("abc▶")
    lineBuffer("abc▶def").moveCursorToEnd() shouldEqual lineBuffer("abcdef▶")
    lineBuffer("▶abc").moveCursorToEnd() shouldEqual lineBuffer("abc▶")
    lineBuffer("▶").moveCursorToEnd() shouldEqual lineBuffer("▶")

    lineBuffer(
      """abc
        |▶def""".stripMargin).moveCursorToEnd() shouldEqual lineBuffer(
      """abc
        |def▶""".stripMargin)

    lineBuffer("▶ab▷c").moveCursorToEnd() shouldEqual lineBuffer("abc▶")

    lineBuffer(
      """abc
        |▶def""".stripMargin).moveCursorToEnd(extendSelection = true) shouldEqual lineBuffer(
      """abc
        |▷def▶""".stripMargin)
  }

  it should "let you move to the beginning of the buffer if at the beginning of a line" in {
    lineBuffer(
      """abc
        |▶def""".stripMargin).moveCursorToStart() shouldEqual lineBuffer(
      """▶abc
        |def""".stripMargin)

    lineBuffer(
      """abc
        |▶def""".stripMargin).moveCursorToStart(extendSelection = true) shouldEqual lineBuffer(
      """▶abc
        |▷def""".stripMargin)
  }

  it should "let you move to the end of the buffer if at the end of a line" in {
    lineBuffer(
      """abc▶
        |def""".stripMargin).moveCursorToEnd() shouldEqual lineBuffer(
      """abc
        |def▶""".stripMargin)

    lineBuffer(
      """abc▶
        |def""".stripMargin).moveCursorToEnd(extendSelection = true) shouldEqual lineBuffer(
      """abc▷
        |def▶""".stripMargin)
  }

  it should "let you move to the end of the buffer" in {
    lineBuffer(
      """a▶bc
        |def""".stripMargin).moveCursorToEndOfBuffer() shouldEqual lineBuffer(
      """abc
        |def▶""".stripMargin)
  }

  it should "let you move to the beginning of the buffer" in {
    lineBuffer(
      """abc
        |de▶f""".stripMargin).moveCursorToStartOfBuffer() shouldEqual lineBuffer(
      """▶abc
        |def""".stripMargin)
  }

  it should "let you delete from the cursor to the beginning of the line" in {
    lineBuffer("▶").deleteToBeginningOfLine shouldEqual lineBuffer("▶")
    lineBuffer("abc▶").deleteToBeginningOfLine shouldEqual lineBuffer("▶")
    lineBuffer("▶abc").deleteToBeginningOfLine shouldEqual lineBuffer("▶abc")
    lineBuffer("foo▶bar").deleteToBeginningOfLine shouldEqual lineBuffer("▶bar")
    lineBuffer(
      """abc
        |def▶ghi""".stripMargin).deleteToBeginningOfLine shouldEqual lineBuffer(
      """abc
        |▶ghi""".stripMargin)

    lineBuffer("foo▶bar▷").deleteToBeginningOfLine shouldEqual lineBuffer("▶bar")
  }

  it should "let you delete from the cursor to the end of the line" in {
    lineBuffer("▶").deleteToEndOfLine shouldEqual lineBuffer("▶")
    lineBuffer("abc▶").deleteToEndOfLine shouldEqual lineBuffer("abc▶")
    lineBuffer("▶abc").deleteToEndOfLine shouldEqual lineBuffer("▶")
    lineBuffer("foo▶bar").deleteToEndOfLine shouldEqual lineBuffer("foo▶")
    lineBuffer(
      """abc
        |def▶ghi""".stripMargin).deleteToEndOfLine shouldEqual lineBuffer(
      """abc
        |def▶""".stripMargin)

    lineBuffer("foo▶bar▷").deleteToEndOfLine shouldEqual lineBuffer("foo▶")
  }

  it should "let you replace a region" in {
    lineBuffer("abc▶").replaceRegion(Region(offset = 1, length = 1), "X") shouldEqual lineBuffer("aXc▶")
    lineBuffer("ab▶c").replaceRegion(Region(offset = 1, length = 1), "X") shouldEqual lineBuffer("aX▶c")
    lineBuffer("a▶bc").replaceRegion(Region(offset = 1, length = 1), "X") shouldEqual lineBuffer("aX▶c")
    lineBuffer("abb▶bbc").replaceRegion(Region(offset = 1, length = 4), "X") shouldEqual lineBuffer("aX▶c")

    lineBuffer("abc▶").replaceRegion(Region(offset = 1, length = 0), "X") shouldEqual lineBuffer("aXbc▶")
    lineBuffer("ab▶c").replaceRegion(Region(offset = 1, length = 0), "X") shouldEqual lineBuffer("aXb▶c")
    lineBuffer("a▶bc").replaceRegion(Region(offset = 1, length = 0), "X") shouldEqual lineBuffer("aX▶bc")
    lineBuffer("▶abc").replaceRegion(Region(offset = 1, length = 0), "X") shouldEqual lineBuffer("▶aXbc")

    lineBuffer("abc▶").replaceRegion(Region(offset = 3, length = 0), "X") shouldEqual lineBuffer("abcX▶")

    lineBuffer("abc▶").replaceRegion(Region(offset = 1, length = 1), "") shouldEqual lineBuffer("ac▶")
    lineBuffer("ab▶c").replaceRegion(Region(offset = 1, length = 1), "") shouldEqual lineBuffer("a▶c")
    lineBuffer("a▶bc").replaceRegion(Region(offset = 1, length = 1), "") shouldEqual lineBuffer("a▶c")
    lineBuffer("▶abc").replaceRegion(Region(offset = 1, length = 1), "") shouldEqual lineBuffer("▶ac")
  }

  private def lineBuffer(s: String) = LineBufferTestHelper.lineBuffer(s.stripMargin)

}