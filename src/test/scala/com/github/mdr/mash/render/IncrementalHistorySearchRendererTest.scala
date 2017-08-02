package com.github.mdr.mash.render

import java.time.Clock
import java.time.temporal.ChronoUnit

import com.github.mdr.mash.render.IncrementalHistorySearchRenderer.renderHistorySearchState
import com.github.mdr.mash.repl.IncrementalHistorySearchState
import com.github.mdr.mash.repl.IncrementalHistorySearchState._
import com.github.mdr.mash.terminal.DummyTerminal.SufficientlyLargeTerminalSize
import com.github.mdr.mash.utils.{ Point, Region }

class IncrementalHistorySearchRendererTest extends RendererTest {

  val expectedHintLine = "^R next, ret done, ^D cd"

  "Incremental history search" should "render correctly before first hit" in {
    val state = IncrementalHistorySearchState("", BeforeFirstHit)

    val LinesAndCursorPos(Seq(line1, line2), Some(cursorPos)) = renderHistorySearchState(state, SufficientlyLargeTerminalSize)

    val expectedLine1 = "Incremental history search: "
    getText(line1) shouldEqual expectedLine1
    cursorPos shouldEqual Point(0, expectedLine1.length)
    getText(line2) shouldEqual expectedHintLine
  }

  it should "render correctly if a hit is found" in {
    val oneDayAgo = Clock.systemDefaultZone.instant.minus(24L, ChronoUnit.HOURS)
    val state = IncrementalHistorySearchState("searchString", Hit(0, Region(0, 1), oneDayAgo, "/etc"))

    val LinesAndCursorPos(Seq(line1, line2, line3), Some(cursorPos)) = renderHistorySearchState(state, SufficientlyLargeTerminalSize)

    val expectedLine1 = "Incremental history search: searchString"
    getText(line1) shouldEqual expectedLine1
    cursorPos shouldEqual Point(0, expectedLine1.length)
    getText(line2) shouldEqual "Hit 1: 1 day ago in /etc"
    getText(line3) shouldEqual expectedHintLine
  }

  it should "truncate with ellipses if there is insufficient width, and not display a cursor" in {
    val state = IncrementalHistorySearchState("searchString", BeforeFirstHit)
    val LinesAndCursorPos(Seq(line1, line2), cursorPosOpt) = renderHistorySearchState(state, SufficientlyLargeTerminalSize.withColumns(5))
    getText(line1) shouldEqual "Incr…"
    getText(line2) shouldEqual "^R n…"
    cursorPosOpt shouldEqual None
  }

}
