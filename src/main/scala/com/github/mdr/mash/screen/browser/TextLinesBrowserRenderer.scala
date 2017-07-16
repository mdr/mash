package com.github.mdr.mash.screen.browser

import com.github.mdr.mash.repl.browser.TextLinesBrowserState
import com.github.mdr.mash.screen.Style.StylableString
import com.github.mdr.mash.screen._
import com.github.mdr.mash.utils.Dimension
import com.github.mdr.mash.utils.Utils._

class TextLinesBrowserRenderer(state: TextLinesBrowserState, terminalSize: Dimension)
  extends AbstractBrowserRenderer(state, terminalSize) {

  protected def renderDataLines: Seq[Line] =
    for ((l, index) <- state.model.renderedLines.window(state.firstRow, windowSize).zipWithIndex)
      yield Line(l.style(Style(inverse = index == (state.selectedRow - state.firstRow))))

  protected def renderLines: Seq[Line] =
    combineUpperStatusLines(renderUpperStatusLines, renderDataLines ++ Seq(renderStatusLine))

  private def renderRegularStatusLine = {
    import KeyHint._
    val hints = Seq(Exit, Back, InsertWhole)
    val countChars = s"${state.selectedRow + 1}/${state.model.renderedLines.size}".style(Style(inverse = true))
    Line(countChars + " (".style + renderKeyHints(hints) + ")".style)
  }

  private def renderStatusLine: Line =
    state.expressionStateOpt match {
      case Some(expressionState) ⇒ StatusLineRenderers.renderExpressionInputStatusLine
      case None                  ⇒ renderRegularStatusLine
    }

  protected val windowSize = state.windowSize(terminalSize.rows)

}
