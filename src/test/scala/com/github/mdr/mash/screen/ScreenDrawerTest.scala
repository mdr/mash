package com.github.mdr.mash.screen

import com.github.mdr.mash.utils.CharUtils.Esc
import org.scalatest.{ FlatSpec, Matchers }
import com.github.mdr.mash.screen.Style._
import com.github.mdr.mash.terminal.DummyTerminal.SufficientlyLargeTerminalSize
import com.github.mdr.mash.utils.Point

class ScreenDrawerTest extends FlatSpec with Matchers {

  private val Title = "title"

  "ScreenDrawer" should "handle an empty screen" in {
    val screen = Screen(lines = Seq(), cursorPosOpt = None, title = Title)
    val ScreenDraw(drawString, None) = new ScreenDrawer(SufficientlyLargeTerminalSize).draw(screen)
    replaceEscapes(drawString) shouldEqual s"(hide-cursor)(reset)(set-title $Title)"
  }

  it should "handle a single line" in {
    val screen = makeScreen(Seq(Line("abc".style)), cursorPos = Point(0, 3))
    val ScreenDraw(drawString, None) = new ScreenDrawer(SufficientlyLargeTerminalSize).draw(screen)
    replaceEscapes(drawString) shouldEqual
      s"(hide-cursor)(reset)abc(set-title $Title)(show-cursor)"
  }

  it should "handle a single extra character" in {
    val screen1 = makeScreen(Seq(Line("abc".style)), cursorPos = Point(0, 3))

    val screen2 = makeScreen(Seq(Line("abcd".style)), cursorPos = Point(0, 4))

    val ScreenDraw(drawString, None) = new ScreenDrawer(SufficientlyLargeTerminalSize).draw(screen2, previousScreenOpt = Some(screen1))

    replaceEscapes(drawString) shouldEqual s"(hide-cursor)(reset)d(show-cursor)"
  }

  it should "correctly handle differences between wrapped lines" in {
    val screen1 = makeScreen(
      lines = Seq(
        Line("  000".style, endsInNewline = false),
        Line("00".style)),
      cursorPos = Point(1, 2))

    val screen2 = makeScreen(
      lines = Seq(
        Line("   00".style, endsInNewline = false),
        Line("000".style)),
      cursorPos = Point(1, 3))

    val ScreenDraw(drawString, None) = new ScreenDrawer(terminalWithColumns(5)).draw(screen2, previousScreenOpt = Some(screen1))

    replaceEscapes(drawString) shouldEqual 
      s"(hide-cursor)(reset)(cursor-up 1) 00(carriage-return)(cursor-forward 4)00(cursor-forward 1)0(show-cursor)"
  }
  
  it should "handle drawing a line with a cursor in the last column" in {
    val screen1 = makeScreen(
      lines = Seq(Line("123 X".style)),
      cursorPos = Point(0, 3))

    val screen2 = makeScreen(
      lines = Seq(Line("1234X".style)),
      cursorPos = Point(0, 4))

    val ScreenDraw(drawString, None) = new ScreenDrawer(terminalWithColumns(5)).draw(screen2, previousScreenOpt = Some(screen1))

    replaceEscapes(drawString) shouldEqual
      s"(hide-cursor)(reset)4X(carriage-return)(cursor-forward 4)(show-cursor)"
  }

  private def makeScreen(lines: Seq[Line], cursorPos: Point) = Screen(lines, Some(cursorPos), Title)
  
  private def terminalWithColumns(numberOfColumns: Int) =
    SufficientlyLargeTerminalSize.withColumns(numberOfColumns)

  private def replaceEscapes(s: String): String = {
    val replacements = Map(
      "\n" → "(newline)",
      "\r" → "(carriage-return)",
      s"$Esc\\[(\\d+)A" → "(cursor-up $1)",
      s"$Esc\\[(\\d+)C" → "(cursor-forward $1)",
      s"$Esc\\[(\\d+)D" → "(cursor-backward $1)",
      s"$Esc\\[\\?25l" → "(hide-cursor)",
      s"$Esc\\[\\?25h" → "(show-cursor)",
      s"$Esc\\[39;49m" → "(default-colour)",
      s"$Esc\\[0m" → "(reset)",
      s"$Esc\\[0?K" → "(erase-line-from-cursor)",
      s"$Esc]0;(.+?)\u0007" → "(set-title $1)")
    var result = s
    for ((pattern, replacement) ← replacements)
      result = result.replaceAll(pattern, replacement)
    result.replaceAll(Esc.toString, "(esc)")
  }

}
