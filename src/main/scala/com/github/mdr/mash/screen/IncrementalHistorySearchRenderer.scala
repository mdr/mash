package com.github.mdr.mash.screen

import com.github.mdr.mash.repl.history.IncrementalHistorySearchState
import com.github.mdr.mash.utils.{ Dimensions, Point }
import com.github.mdr.mash.screen.Style.StylableString

object IncrementalHistorySearchRenderer {

  def renderHistorySearchState(searchState: IncrementalHistorySearchState, terminalSize: Dimensions): LinesAndCursorPos = {
    val prefix = "Incremental history search: ".style
    val searchString = searchState.searchString.style(Style(foregroundColour = BasicColour.Cyan))
    val chars = (prefix + searchString).take(terminalSize.columns)
    val searchLine = Line(chars)
    val cursorPos = Point(0, chars.size)

    val hints = KeyHint.renderKeyHints(Seq(KeyHint.NextHistoryHit, KeyHint.DoneSearch))
    val hintLine = Line(("(".style + hints + ")".style).take(terminalSize.columns))

    LinesAndCursorPos(Seq(searchLine, hintLine), cursorPos)
  }

}
