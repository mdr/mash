package com.github.mdr.mash.screen.browser

import com.github.mdr.mash.repl.browser.SearchState
import com.github.mdr.mash.screen.{ KeyHint, Line, Style }
import com.github.mdr.mash.screen.Style.StylableString

object StatusLineRenderers {
    import KeyHint._

  def renderExpressionInputStatusLine(expression: String): Line = {
    val hints = Seq(DoneSearch)
    Line("(".style + renderKeyHints(hints) + ")".style)
  }

  def renderIncrementalSearchStatusLine(currentRow: Int, searchState: SearchState): Line = {
    val hits = searchState.rows
    val currentHit = hits.indexOf(currentRow)
    val hints = Seq(NextHit, PreviousHit, DoneSearch, if (searchState.ignoreCase) CaseSensitive else CaseInsensitive)
    val countChars = s"${currentHit + 1}/${hits.size}".style(Style(inverse = true))
    Line(countChars + s" Find: ${searchState.query}".style + " (".style + renderKeyHints(hints) + ")".style)
  }


}
