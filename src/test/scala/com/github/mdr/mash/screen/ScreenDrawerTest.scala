package com.github.mdr.mash.screen

import com.github.mdr.mash.utils.CharUtils.Esc
import org.scalatest.{ FlatSpec, Matchers }
import com.github.mdr.mash.screen.Style._
import com.github.mdr.mash.terminal.DummyTerminal.SufficientlyLargeTerminalSize
import com.github.mdr.mash.terminal.InMemoryTerminal
import com.github.mdr.mash.utils.Point

class ScreenDrawerTest extends FlatSpec with Matchers {

  private val Title = "title"

  "ScreenDrawer" should "handle an empty screen" in {
    val screen = Screen(lines = Seq(), cursorPosOpt = None, title = Title)
    val ScreenDraw(drawString, None) = new ScreenDrawer(SufficientlyLargeTerminalSize).draw(screen)
    replaceEscapes(drawString) shouldEqual s"(hide-cursor)(reset)(set-title $Title)"
    val terminal = new InMemoryTerminal(SufficientlyLargeTerminalSize)
    terminal.print(drawString)
    terminal.consume()
    terminal.screen shouldEqual screen
  }

  it should "handle a single line" in {
    val screen = makeScreen(Seq(Line("abc".style)), cursorPos = Point(0, 3))
    val ScreenDraw(drawString, None) = new ScreenDrawer(SufficientlyLargeTerminalSize).draw(screen)
    replaceEscapes(drawString) shouldEqual
      s"(hide-cursor)(reset)abc(set-title $Title)(show-cursor)"
    val terminal = new InMemoryTerminal(SufficientlyLargeTerminalSize)
    terminal.print(drawString)
    terminal.consume()
    terminal.screen shouldEqual screen
  }

  it should "handle a single extra character" in {
    val screen1 = makeScreen(Seq(Line("abc".style)), cursorPos = Point(0, 3))

    val screen2 = makeScreen(Seq(Line("abcd".style)), cursorPos = Point(0, 4))

    val ScreenDraw(drawString, None) = new ScreenDrawer(SufficientlyLargeTerminalSize).draw(screen2, previousScreenOpt = Some(screen1))

    replaceEscapes(drawString) shouldEqual s"(hide-cursor)(reset)d(show-cursor)"

    val terminal = new InMemoryTerminal(SufficientlyLargeTerminalSize)
    val screenDrawer = new ScreenDrawer(SufficientlyLargeTerminalSize)
    terminal.print(screenDrawer.draw(screen1).drawString)
    terminal.consume()
    terminal.screen shouldEqual screen1
    terminal.print(drawString)
    terminal.consume()
    terminal.screen shouldEqual screen2
  }

  it should "correctly handle differences between wrapped lines" in {
    val screen1 = makeScreen(
      lines = Seq(
        Line("  000".style, endsInNewline = false),
        Line("00".style)),
      cursorPos = Point(1, 2))

    val terminalSize = terminalWithColumns(5)
    val screenDrawer = new ScreenDrawer(terminalSize)
    val drawString1 = screenDrawer.draw(screen1).drawString
    replaceEscapes(drawString1) shouldEqual
      s"(hide-cursor)(reset)  000(carriage-return)(cursor-forward 4)00(cursor-backward 1)00(set-title title)(show-cursor)"

    val screen2 = makeScreen(
      lines = Seq(
        Line("   00".style, endsInNewline = false),
        Line("000".style)),
      cursorPos = Point(1, 3))

    val ScreenDraw(drawString2, None) = screenDrawer.draw(screen2, previousScreenOpt = Some(screen1))
    replaceEscapes(drawString2) shouldEqual
      s"(hide-cursor)(reset)(cursor-up 1) (cursor-forward 1)00(cursor-forward 1)0(show-cursor)"

    val terminal = new InMemoryTerminal(terminalSize)
    terminal.print(drawString1)
    terminal.consume()
    terminal.screen shouldEqual screen1
    terminal.print(drawString2)
    terminal.consume()
    terminal.screen shouldEqual screen2
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
      s"(hide-cursor)(reset)4(show-cursor)"
  }

  it should "not reset if only changing colour" in {
    val screen = makeScreen(Seq(
      Line(
        "Y".style(foregroundColour = BasicColour.Yellow) +
          "G".style(foregroundColour = BasicColour.Green))), cursorPos = Point(0, 2))
    val ScreenDraw(drawString, None) = new ScreenDrawer(SufficientlyLargeTerminalSize).draw(screen)
    replaceEscapes(drawString) shouldEqual
      s"(hide-cursor)(reset)(yellow)Y(green)G(set-title $Title)(show-cursor)"

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
      s"$Esc\\[32m" → "(green)",
      s"$Esc\\[33m" → "(yellow)",
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
