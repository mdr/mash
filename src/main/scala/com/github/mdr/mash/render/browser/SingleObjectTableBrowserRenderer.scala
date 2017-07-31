package com.github.mdr.mash.render.browser

import com.github.mdr.mash.render.{ KeyHint, LinesAndCursorPos }
import com.github.mdr.mash.repl.browser.SingleObjectTableBrowserState
import com.github.mdr.mash.screen._
import com.github.mdr.mash.utils.Dimensions

class SingleObjectTableBrowserRenderer(state: SingleObjectTableBrowserState, terminalSize: Dimensions)
  extends AbstractBrowserRenderer(state, terminalSize) {

  protected def renderLines: LinesAndCursorPos =
    combineUpperStatusLines(renderUpperStatusLines, renderTableLines :+ renderStatusLine)

  private def renderTableLines: Seq[Line] = {
    val currentRowIndexOpt = Some(state.currentRow).filterNot(_ ⇒ state.expressionStateOpt.isDefined)
    val commonRenderer = new SingleObjectTableCommonRenderer(state.model, markedRowsOpt = Some(state.markedRows),
      currentRowIndexOpt, state.searchStateOpt)
    commonRenderer.renderTableLines(state.firstRow, windowSize)
  }

  private def renderRegularStatusLine = {
    import KeyHint._
    Line(renderKeyHints(Seq(Exit, Mark, Focus, Back, Insert, InsertWhole, Tree, Search, Expression, Open, Copy)))
  }

  private def renderStatusLine: Line =
    state.searchStateOpt match {
      case Some(searchState) ⇒
        StatusLineRenderers.renderIncrementalSearchStatusLine(currentRow, searchState)
      case None              ⇒
        state.expressionStateOpt match {
          case Some(expressionState) ⇒ StatusLineRenderers.renderExpressionInputStatusLine
          case None                  ⇒ renderRegularStatusLine
        }
    }

  override protected val windowSize: Int = state.windowSize(terminalSize.rows)

  private def currentRow = state.currentRow

}