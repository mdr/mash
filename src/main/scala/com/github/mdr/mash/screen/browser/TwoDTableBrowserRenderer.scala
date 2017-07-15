package com.github.mdr.mash.screen.browser

import com.github.mdr.mash.repl.browser.TwoDTableBrowserState
import com.github.mdr.mash.screen.Style.StylableString
import com.github.mdr.mash.screen.{ KeyHint, _ }
import com.github.mdr.mash.terminal.TerminalInfo

class TwoDTableBrowserRenderer(state: TwoDTableBrowserState, terminalInfo: TerminalInfo)
  extends AbstractBrowserRenderer(state, terminalInfo) {

  protected def renderLines: Seq[Line] =
    combineUpperStatusLines(renderUpperStatusLines, renderTableLines :+ renderStatusLine)

  private def renderTableLines: Seq[Line] = {
    val commonRenderer = new TwoDTableCommonRenderer(state.model, markedRowsOpt = Some(state.markedRows),
      Some(state.currentRow), state.currentColumnOpt, state.searchStateOpt)
    commonRenderer.renderTableLines(state.firstRow, windowSize)
  }

  private def renderRegularStatusLine: Line = {
    import KeyHint._
    val hints = Seq(Exit, Mark, Focus, Back, Insert, InsertWhole, Tree, Search, Expression, Open, Copy, Dir) ++
      state.currentColumnOpt.toSeq.flatMap(_ ⇒ Seq(Row, HideColumn))
    val countChars = s"${currentRow + 1}/${model.rows.size}".style(Style(inverse = true))
    Line(countChars + " (".style + renderKeyHints(hints) + ")".style)
  }

  private def renderStatusLine: Line =
    state.searchStateOpt match {
      case Some(searchState) ⇒ StatusLineRenderers.renderIncrementalSearchStatusLine(currentRow, searchState)
      case None              ⇒
        state.expressionStateOpt match {
          case Some(expressionState) ⇒ StatusLineRenderers.renderExpressionInputStatusLine
          case None                  ⇒ renderRegularStatusLine
        }
    }

  private def model = state.model

  private def currentRow = state.currentRow

  protected val windowSize = state.windowSize(terminalInfo.rows)

}