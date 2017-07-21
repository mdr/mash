package com.github.mdr.mash.render.browser

import com.github.mdr.mash.render.KeyHint
import com.github.mdr.mash.repl.browser.SearchState
import com.github.mdr.mash.screen.Style.StylableString
import com.github.mdr.mash.screen.{ Line, Style }

object StatusLineRenderers {

  import KeyHint._

  def renderExpressionInputStatusLine: Line = Line(renderKeyHints(Seq(DoneSearch)))

  def renderIncrementalSearchStatusLine(currentRow: Int, searchState: SearchState): Line = {
    val hits = searchState.rows
    val currentHit = hits.indexOf(currentRow)
    val hints = Seq(NextHit, PreviousHit, DoneSearch, if (searchState.ignoreCase) CaseSensitive else CaseInsensitive)
    val countChars = s"${currentHit + 1}/${hits.size}".style(Style(inverse = true))
    Line(countChars + s" Find: ${searchState.query}".style + " (".style + renderKeyHints(hints) + ")".style)
  }

}
