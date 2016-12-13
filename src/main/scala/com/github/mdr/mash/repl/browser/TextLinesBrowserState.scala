package com.github.mdr.mash.repl.browser

import com.github.mdr.mash.parser.SafeParens
import com.github.mdr.mash.printer.model.TextLinesModel
import com.github.mdr.mash.runtime.MashValue

case class TextLinesBrowserState(model: TextLinesModel,
                                 path: String,
                                 selectedRow: Int = 0,
                                 firstRow: Int = 0) extends BrowserState {

  override def rawValue: MashValue = model.rawValue

  override def withPath(newPath: String): TextLinesBrowserState = copy(path = newPath)

  override def getInsertExpression: String = {
    val safePath = SafeParens.safeParens(path)
    s"$safePath[$selectedRow]"
  }

  def size = model.renderedLines.size

  def adjustSelectedRow(delta: Int, windowSize: Int): TextLinesBrowserState =
    copy(selectedRow = (selectedRow + delta + size) % size).adjustWindowToFit(windowSize)

  def adjustFirstRow(delta: Int): TextLinesBrowserState = copy(firstRow = firstRow + delta)

  def adjustWindowToFit(windowSize: Int): TextLinesBrowserState = {
    var newState = this

    val delta = selectedRow - (firstRow + windowSize - 1)
    if (delta >= 0)
      newState = newState.adjustFirstRow(delta)

    val delta2 = firstRow - selectedRow
    if (delta2 >= 0)
      newState = newState.adjustFirstRow(-delta2)

    newState
  }
}
