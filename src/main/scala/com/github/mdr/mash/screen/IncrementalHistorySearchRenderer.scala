package com.github.mdr.mash.screen

import com.github.mdr.mash.repl.history.IncrementalHistorySearchState
import com.github.mdr.mash.screen.KeyHint.{ DoneSearch, NextHistoryHit }
import com.github.mdr.mash.screen.Style.StylableString
import com.github.mdr.mash.utils.Utils._
import com.github.mdr.mash.utils.{ Dimensions, Point }

object IncrementalHistorySearchRenderer {

  private val SearchStringStyle = Style(foregroundColour = BasicColour.Cyan)

  def renderHistorySearchState(searchState: IncrementalHistorySearchState, terminalSize: Dimensions): LinesAndCursorPos = {
    val prefix = "Incremental history search: ".style
    val searchString = searchState.searchString.style(SearchStringStyle)
    val chars = prefix + searchString
    val searchLine = Line(chars)
    val cursorPosOpt = (chars.size < terminalSize.columns).option(Point(0, chars.size))

    val hints = KeyHint.renderKeyHints(Seq(NextHistoryHit, DoneSearch))
    val hintLine = Line("(".style + hints + ")".style)

    val lines = Seq(searchLine, hintLine).map(truncateIfNecessary(_, terminalSize))
    LinesAndCursorPos(lines, cursorPosOpt)
  }

  private def truncateIfNecessary(line: Line, terminalSize: Dimensions): Line =
    Line(ellipsisise(line.string, terminalSize.columns))

  private def ellipsisise(s: StyledString, maxLength: Int): StyledString =
    StyledString(truncate(s.chars, maxLength, StyledCharacter('…')))

}
