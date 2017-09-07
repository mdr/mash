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
    val repl = makeRepl(terminal = DummyTerminal(columns = 3))

    repl.getText shouldEqual
      """[0]
        | ~ 
        |$ """.stripMargin
    repl.render.cursorPosOpt shouldEqual Some(Point(2, 2))
    repl.render.lines.map(_.endsInNewline) shouldEqual Seq(false, false, true)
  }

  "Rendering a REPL as wide as the terminal" should "work" in {
    val repl = makeRepl(terminal = DummyTerminal(columns = 10))
    repl.input("89")
    repl.getText shouldEqual
      """[0] ~ $ 89
        |""".stripMargin
    repl.render.cursorPosOpt shouldEqual Some(Point(1, 0))
    repl.render.lines.map(_.endsInNewline) shouldEqual Seq(false, true)
  }

  "Multiple lines" should "be indented with dots under the prefix" in {
    val repl = makeRepl()
      .input("{").enter()
      .input("  42").enter()
      .input("}")

    repl.getText shouldEqual
      """[0] ~ $ {
        |.......   42
        |....... }""".stripMargin
  }

  "Empty lines" should "be rendered in browser expression editor" in {
    makeRepl()
      .input("view.browse [{ a: 1, b: 2 }, { a: 3, b: 4 }]").enter()
      .affirmInTwoDBrowser
      .beginExpression()
      .newline()
      .newline()
      .newline()
      .input("XXX")
      .repl
      .getText shouldEqual
      """Browse: r0
        |....... 
        |....... 
        |....... XXX
        |║ │0│1│2║
        |║ │1│3│4║
        |╚═╧═╧═╧═╝
        |⌃g quit, ↩ done""".stripMargin
  }

  private def getText(line: Line): String = line.string.forgetStyling

}
