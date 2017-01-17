package com.github.mdr.mash.screen.browser

import com.github.mdr.mash.repl.browser.SingleObjectTableBrowserState
import com.github.mdr.mash.screen.{ KeyHint, _ }
import com.github.mdr.mash.terminal.TerminalInfo

class SingleObjectTableBrowserRenderer(state: SingleObjectTableBrowserState, terminalInfo: TerminalInfo)
  extends AbstractBrowserRenderer(state, terminalInfo) {

  private val commonRenderer = new SingleObjectTableCommonRenderer(state.model, terminalInfo)

  protected def renderLines: Seq[Line] = renderUpperStatusLine +: renderTableLines :+ renderStatusLine

  private def renderTableLines: Seq[Line] = {
    val objects = state.model.fields.drop(state.firstRow).take(windowSize).toSeq
    val selectedRow = state.selectedRow - state.firstRow
    val tableLines = commonRenderer.renderTableLines(objects, Some(selectedRow), moreItemsAboveWindow, moreDataItemsBelowWindow)
    tableLines
  }

  private def moreItemsAboveWindow: Boolean = state.firstRow > 0

  private def moreDataItemsBelowWindow: Boolean = {
    val lastVisibleRow = state.firstRow + windowSize - 1
    val lastRow = state.size - 1
    lastVisibleRow < lastRow
  }

  private def renderStatusLine = {
    import KeyHint._
    Line(renderKeyHints(Seq(Exit, Focus, Back, Insert, InsertWhole, Tree)))
  }

  override protected val windowSize: Int = state.windowSize(terminalInfo.rows)

}