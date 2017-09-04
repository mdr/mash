package com.github.mdr.mash.view.render.browser

import com.github.mdr.mash.view.render.{ KeyHint, LinesAndCursorPos, MashRenderingContext }
import com.github.mdr.mash.repl.browser.SingleObjectTableBrowserState
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.screen._
import com.github.mdr.mash.utils.Dimensions
import com.github.mdr.mash.view.common.SingleObjectTableCommonRenderer

class SingleObjectTableBrowserRenderer(state: SingleObjectTableBrowserState, terminalSize: Dimensions, mashRenderingContext: MashRenderingContext)
  extends AbstractBrowserRenderer(state, terminalSize, mashRenderingContext) {

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
    Line(renderKeyHints(Seq(Exit, Mark, Focus, Back, Insert, InsertWhole, Tree, Search, Expression, Open, Copy, Read, NextParentResult, PreviousParentResult)))
  }

  private def renderStatusLine: Line =
    state.searchStateOpt match {
      case Some(searchState) ⇒
        StatusLineRenderers.renderIncrementalSearchStatusLine(currentRow, searchState)
      case None              ⇒
        state.expressionStateOpt match {
          case Some(_) ⇒ StatusLineRenderers.renderExpressionInputStatusLine
          case None    ⇒ renderRegularStatusLine
        }
    }

  override protected val windowSize: Int = state.windowSize(terminalSize.rows)

  private def currentRow = state.currentRow

}