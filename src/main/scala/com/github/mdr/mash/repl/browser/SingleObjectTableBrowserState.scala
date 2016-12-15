package com.github.mdr.mash.repl.browser

import com.github.mdr.mash.printer.model.ObjectModel
import com.github.mdr.mash.runtime.{ MashObject, MashValue }
import com.github.mdr.mash.utils.Utils._

case class SingleObjectTableBrowserState(model: ObjectModel,
                                         selectedRow: Int = 0,
                                         firstRow: Int = 0,
                                         path: String,
                                         expressionOpt: Option[String] = None) extends BrowserState {

  override def rawValue: MashObject = model.rawValue

  val size = model.fields.size

  def previousItem(terminalRows: Int): SingleObjectTableBrowserState = adjustSelectedRow(-1, terminalRows)

  def nextItem(terminalRows: Int): SingleObjectTableBrowserState = adjustSelectedRow(1, terminalRows)

  def adjustSelectedRow(delta: Int, terminalRows: Int): SingleObjectTableBrowserState =
    this.when(size > 0, _.copy(selectedRow = (selectedRow + delta + size) % size).adjustWindowToFit(terminalRows))

  def adjustFirstRow(delta: Int): SingleObjectTableBrowserState = copy(firstRow = firstRow + delta)

  def firstItem(terminalRows: Int): SingleObjectTableBrowserState =
    copy(selectedRow = 0).adjustWindowToFit(terminalRows)

  def lastItem(terminalRows: Int): SingleObjectTableBrowserState =
    copy(selectedRow = size - 1).adjustWindowToFit(terminalRows)

  def selectedField: String = model.fields.toSeq(selectedRow)._1

  def selectedRawValue: MashValue = model.rawValues.toSeq(selectedRow)._2

  def withPath(newPath: String): SingleObjectTableBrowserState = copy(path = newPath)

  def getInsertExpression: String = {
    val field = model.rawValues.toSeq(selectedRow)._1
    val command = path
    BrowserState.safeProperty(command, field)
  }

  def adjustWindowToFit(terminalRows: Int): SingleObjectTableBrowserState = {
    var newState = this

    val delta = selectedRow - (firstRow + windowSize(terminalRows) - 1)
    if (delta >= 0)
      newState = newState.adjustFirstRow(delta)

    val delta2 = firstRow - selectedRow
    if (delta2 >= 0)
      newState = newState.adjustFirstRow(-delta2)

    newState
  }

  private def windowSize(terminalRows: Int) = terminalRows - 4 // 1 header row, 1 footer row, 2 status rows

  def setExpression(expression: String): BrowserState = copy(expressionOpt = Some(expression))
  def acceptExpression: BrowserState = copy(expressionOpt = None)

}
