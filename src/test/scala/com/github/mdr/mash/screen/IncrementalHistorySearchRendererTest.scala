package com.github.mdr.mash.screen

import com.github.mdr.mash.repl.history.IncrementalHistorySearchState
import com.github.mdr.mash.screen.IncrementalHistorySearchRenderer.renderHistorySearchState
import com.github.mdr.mash.utils.{ Dimensions, Point }
import org.scalatest.{ FlatSpec, Matchers }

class IncrementalHistorySearchRendererTest extends FlatSpec with Matchers {

  val SufficientlyLargeTerminalSize = Dimensions(1000, 1000)

  "Incremental history search" should "be rendered correctly if there is enough space" in {
    val state = IncrementalHistorySearchState("searchString")

    val LinesAndCursorPos(Seq(line1, line2), Some(cursorPos)) = renderHistorySearchState(state, SufficientlyLargeTerminalSize)

    val expectedLine1 = "Incremental history search: searchString"
    getText(line1) shouldEqual expectedLine1
    cursorPos shouldEqual Point(0, expectedLine1.length)
    getText(line2) shouldEqual "(^R next, ret done)"
  }

  it should "truncate with ellipses if there is insufficient width, and not display a cursor" in {
    val state = IncrementalHistorySearchState("searchString")
    val LinesAndCursorPos(Seq(line1, line2), cursorPosOpt) = renderHistorySearchState(state, SufficientlyLargeTerminalSize.withColumns(5))
    getText(line1) shouldEqual "Incr…"
    getText(line2) shouldEqual "(^R …"
    cursorPosOpt shouldEqual None
  }

  private def getText(line: Line): String = line.string.chars.map(_.c).mkString

}
