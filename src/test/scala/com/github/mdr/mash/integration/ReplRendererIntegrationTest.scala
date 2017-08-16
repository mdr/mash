package com.github.mdr.mash.integration

import com.github.mdr.mash.screen.Line
import com.github.mdr.mash.terminal.DummyTerminal
import com.github.mdr.mash.utils.Point

class ReplRendererIntegrationTest extends AbstractIntegrationTest {

  "Rendering the initial empty REPL" should "work" in {
    val screen = makeRepl().render

    val expectedLine = "[0] ~ $ "
    screen.lines.map(getText) shouldEqual Seq(expectedLine)
    screen.cursorPosOpt shouldEqual Some(Point(0, expectedLine.length))
    screen.title shouldEqual "~"
  }

  "Rendering the initial empty REPL" should "wrap if insufficient terminal width" in {
    val screen = makeRepl(terminal = DummyTerminal(rows = 3)).render

    screen.lines.map(getText) shouldEqual Seq(
      "[0]",
      " ~ ",
      "$ ")
  }

  "Multiple lines" should "be indented with dots under the prefix" in {
    val screen = makeRepl()
      .input("{").enter()
      .input("  42").enter()
      .input("}").render

    screen.lines.map(getText) shouldEqual Seq(
      "[0] ~ $ {",
      ".......   42",
      "....... }")
  }

  private def getText(line: Line): String = line.string.forgetStyling

}
