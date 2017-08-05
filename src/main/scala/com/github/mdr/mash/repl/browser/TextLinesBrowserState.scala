package com.github.mdr.mash.repl.browser

import com.github.mdr.mash.parser.ExpressionCombiner
import com.github.mdr.mash.printer.model.TextLinesModel
import com.github.mdr.mash.runtime.MashValue

case class TextLinesBrowserState(model: TextLinesModel,
                                 path: String,
                                 selectedRow: Int = 0,
                                 firstRow: Int = 0,
                                 expressionStateOpt: Option[ExpressionState] = None) extends BrowserState {

  override def rawValue: MashValue = model.rawValue

  override def withPath(newPath: String): TextLinesBrowserState = copy(path = newPath)

  override def selectionInfoOpt: Option[SelectionInfo] = {
    val selectionPath = ExpressionCombiner.combineSafely(path, s"[$selectedRow]")
    Some(SelectionInfo(selectionPath, model.rawValue.elements(selectedRow)))
  }

  def size = model.renderedLines.size

  def previousItem(terminalRows: Int): TextLinesBrowserState = adjustSelectedRow(-1, terminalRows)

  def nextItem(terminalRows: Int): TextLinesBrowserState = adjustSelectedRow(1, terminalRows)

  def firstItem(terminalRows: Int): TextLinesBrowserState =
    copy(selectedRow = 0).adjustWindowToFit(terminalRows)

  def lastItem(terminalRows: Int): TextLinesBrowserState =
    copy(selectedRow = size - 1).adjustWindowToFit(terminalRows)

  def nextPage(terminalRows: Int): TextLinesBrowserState = {
    val newRow = selectedRow + windowSize(terminalRows) - 1 min size - 1
    copy(selectedRow = newRow).adjustWindowToFit(terminalRows)
  }

  def previousPage(terminalRows: Int): TextLinesBrowserState = {
    val newRow = 0 max selectedRow - windowSize(terminalRows) - 1
    copy(selectedRow = newRow).adjustWindowToFit(terminalRows)
  }

  def adjustSelectedRow(delta: Int, terminalRows: Int): TextLinesBrowserState =
    copy(selectedRow = (selectedRow + delta + size) % size).adjustWindowToFit(terminalRows)

  def adjustFirstRow(delta: Int): TextLinesBrowserState = copy(firstRow = firstRow + delta)

  def adjustWindowToFit(terminalRows: Int): TextLinesBrowserState = {
    var newState = this

    val delta = selectedRow - (firstRow + windowSize(terminalRows) - 1)
    if (delta >= 0)
      newState = newState.adjustFirstRow(delta)

    val delta2 = firstRow - selectedRow
    if (delta2 >= 0)
      newState = newState.adjustFirstRow(-delta2)

    newState
  }

  def withExpressionState(expressionStateOpt: Option[ExpressionState]): BrowserState =
    copy(expressionStateOpt = expressionStateOpt)

  def windowSize(terminalRows: Int) = terminalRows - 2

}
