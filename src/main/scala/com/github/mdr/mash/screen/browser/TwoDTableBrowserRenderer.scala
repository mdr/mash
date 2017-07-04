package com.github.mdr.mash.screen.browser

import com.github.mdr.mash.repl.browser.TwoDTableBrowserState
import com.github.mdr.mash.screen.Style.StylableString
import com.github.mdr.mash.screen.{ KeyHint, _ }
import com.github.mdr.mash.terminal.TerminalInfo
import com.github.mdr.mash.utils.Utils._

class TwoDTableBrowserRenderer(state: TwoDTableBrowserState, terminalInfo: TerminalInfo)
  extends AbstractBrowserRenderer(state, terminalInfo) {

  protected def renderLines: Seq[Line] = renderUpperStatusLine +: renderTableLines :+ renderStatusLine

  private def renderTableLines: Seq[Line] = {
    val windowRows = model.rows.window(state.firstRow, windowSize)
    val currentRow = state.currentRow - state.firstRow
    val markedRows = state.markedRows.map(_ - state.firstRow)
    val searchStateOpt = state.searchStateOpt.map { searchState ⇒
      val newByPoint =
        for ((point, cellSearchInfo) <- searchState.byPoint)
          yield point.up(state.firstRow) -> cellSearchInfo
      searchState.copy(byPoint = newByPoint)
    }
    val commonRenderer = new TwoDTableCommonRenderer(state.model, markedRowsOpt = Some(markedRows), Some(currentRow),
      state.currentColumnOpt, searchStateOpt)
    commonRenderer.renderTableLines(windowRows)
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
        state.expressionOpt match {
          case Some(expression) ⇒ StatusLineRenderers.renderExpressionInputStatusLine(expression)
          case None             ⇒ renderRegularStatusLine
        }
    }

  private def model = state.model

  private def currentRow = state.currentRow

  protected val windowSize = state.windowSize(terminalInfo.rows)

}