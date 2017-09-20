package com.github.mdr.mash.screen

import com.github.mdr.mash.screen.Style._
import com.github.mdr.mash.terminal.DummyTerminal.SufficientlyLargeTerminalSize
import com.github.mdr.mash.terminal.InMemoryTerminal
import com.github.mdr.mash.utils.CharUtils.Esc
import com.github.mdr.mash.utils.Point
import org.scalatest.{ FlatSpec, Matchers }

class ScreenDrawerTest extends FlatSpec with Matchers {

  private val Title = "title"

  "ScreenDrawer" should "handle an empty screen" in {
    val screen = Screen(lines = Seq(), cursorPosOpt = None, title = Title)

    val ScreenDraw(drawString, None) = new ScreenDrawer(SufficientlyLargeTerminalSize).draw(screen)

    replaceEscapes(drawString) shouldEqual s"(hide-cursor)(reset)(set-title $Title)"
    val terminal = new InMemoryTerminal(SufficientlyLargeTerminalSize)
    terminal.sendInstructions(drawString) shouldEqual screen
  }

  it should "handle a single line" in {
    val screen = makeScreen(Seq(Line("abc".style)), cursorPos = Point(0, 3))

    val drawString = new ScreenDrawer(SufficientlyLargeTerminalSize).draw(screen).drawString

    replaceEscapes(drawString) shouldEqual
      s"(hide-cursor)(reset)abc(set-title $Title)(show-cursor)"
    val terminal = new InMemoryTerminal(SufficientlyLargeTerminalSize)
    terminal.sendInstructions(drawString) shouldEqual screen
  }

  it should "handle a single extra character" in {
    val screen1 = makeScreen(Seq(Line("abc".style)), cursorPos = Point(0, 3))
    val screen2 = makeScreen(Seq(Line("abcd".style)), cursorPos = Point(0, 4))

    val screenDrawer = new ScreenDrawer(SufficientlyLargeTerminalSize)
    val drawString1 = screenDrawer.draw(screen1).drawString
    val drawString2 = screenDrawer.draw(screen2, previousScreenOpt = Some(screen1)).drawString

    replaceEscapes(drawString1) shouldEqual s"(hide-cursor)(reset)abc(set-title $Title)(show-cursor)"
    replaceEscapes(drawString2) shouldEqual s"(hide-cursor)(reset)d(show-cursor)"

    val terminal = new InMemoryTerminal(SufficientlyLargeTerminalSize)
    terminal.sendInstructions(drawString1) shouldEqual screen1
    terminal.sendInstructions(drawString2) shouldEqual screen2
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

    val terminalSize = terminalWithColumns(5)
    val screenDrawer = new ScreenDrawer(terminalSize)

    val drawString1 = screenDrawer.draw(screen1).drawString
    val drawString2 = screenDrawer.draw(screen2, previousScreenOpt = Some(screen1)).drawString
    replaceEscapes(drawString1) shouldEqual
      s"(hide-cursor)(reset)  00000(set-title title)(show-cursor)"
    replaceEscapes(drawString2) shouldEqual
      s"(hide-cursor)(reset)(cursor-up 1) (cursor-forward 1)00(cursor-forward 1)0(show-cursor)"

    val terminal = new InMemoryTerminal(terminalSize)
    terminal.sendInstructions(drawString1) shouldEqual screen1
    terminal.sendInstructions(drawString2) shouldEqual screen2
  }

  it should "handle drawing a line with a cursor in the last column" in {
    val screen1 = makeScreen(lines = Seq(Line("123 X".style)), cursorPos = Point(0, 3))
    val screen2 = makeScreen(lines = Seq(Line("1234X".style)), cursorPos = Point(0, 4))
    val terminalSize = terminalWithColumns(5)
    val screenDrawer = new ScreenDrawer(terminalSize)

    val drawString1 = screenDrawer.draw(screen1).drawString
    val drawString2 = screenDrawer.draw(screen2, previousScreenOpt = Some(screen1)).drawString

    replaceEscapes(drawString1) shouldEqual s"(hide-cursor)(reset)123 X(carriage-return)(cursor-forward 3)(set-title title)(show-cursor)"
    replaceEscapes(drawString2) shouldEqual s"(hide-cursor)(reset)4(show-cursor)"

    val terminal = new InMemoryTerminal(terminalSize)
    terminal.sendInstructions(drawString1) shouldEqual screen1
    terminal.sendInstructions(drawString2) shouldEqual screen2
  }

  it should "not reset if only changing colour" in {
    val screen = makeScreen(Seq(
      Line(
        "Y".style(foregroundColour = BasicColour.Yellow) +
          "G".style(foregroundColour = BasicColour.Green))),
      cursorPos = Point(0, 2))
    val screenDrawer = new ScreenDrawer(SufficientlyLargeTerminalSize)

    val drawString = screenDrawer.draw(screen).drawString

    replaceEscapes(drawString) shouldEqual
      s"(hide-cursor)(reset)(yellow)Y(green)G(set-title $Title)(show-cursor)"
    val terminal = new InMemoryTerminal(SufficientlyLargeTerminalSize)
    terminal.sendInstructions(drawString) shouldEqual screen
  }

  private def makeScreen(lines: Seq[Line], cursorPos: Point) = Screen(lines, Some(cursorPos), Title)

  private def terminalWithColumns(numberOfColumns: Int) = SufficientlyLargeTerminalSize.withColumns(numberOfColumns)

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
