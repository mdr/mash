package com.github.mdr.mash.screen.browser

import com.github.mdr.mash.repl.browser.SingleObjectTableBrowserState
import com.github.mdr.mash.screen.{ KeyHint, _ }
import com.github.mdr.mash.terminal.TerminalInfo

class SingleObjectTableBrowserRenderer(state: SingleObjectTableBrowserState, terminalInfo: TerminalInfo)
  extends AbstractBrowserRenderer(state, terminalInfo) {

  protected def renderLines: Seq[Line] = renderUpperStatusLine +: renderTableLines :+ renderStatusLine

  private def renderTableLines: Seq[Line] = {
    val commonRenderer = new SingleObjectTableCommonRenderer(state.model, Some(state.selectedRow), moreItemsAboveWindow,
      moreDataItemsBelowWindow, state.searchStateOpt)
    commonRenderer.renderTableLines(state.firstRow, windowSize)
  }

  private def moreItemsAboveWindow: Boolean = state.firstRow > 0

  private def moreDataItemsBelowWindow: Boolean = {
    val lastVisibleRow = state.firstRow + windowSize - 1
    val lastRow = state.size - 1
    lastVisibleRow < lastRow
  }

  private def renderRegularStatusLine = {
    import KeyHint._
    Line(renderKeyHints(Seq(Exit, Focus, Back, Insert, InsertWhole, Tree, Open, Copy)))
  }

  private def renderStatusLine: Line =
    state.searchStateOpt match {
      case Some(searchState) ⇒
        StatusLineRenderers.renderIncrementalSearchStatusLine(currentRow, searchState)
      case None              ⇒
        state.expressionOpt match {
          case Some(expression) ⇒ StatusLineRenderers.renderExpressionInputStatusLine(expression)
          case None             ⇒ renderRegularStatusLine
        }
    }

  override protected val windowSize: Int = state.windowSize(terminalInfo.rows)

  private def currentRow = state.selectedRow

}