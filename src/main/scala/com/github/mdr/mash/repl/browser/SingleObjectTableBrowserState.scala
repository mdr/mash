package com.github.mdr.mash.repl.browser

import com.github.mdr.mash.printer.model.ObjectModel
import com.github.mdr.mash.runtime.{ MashObject, MashValue }

case class SingleObjectTableBrowserState(model: ObjectModel,
                                         selectedRow: Int = 0,
                                         firstRow: Int = 0,
                                         path: String) extends BrowserState {

  override def rawValue: MashObject = model.rawValue

  private val size = model.fields.size

  def adjustSelectedRow(delta: Int): SingleObjectTableBrowserState =
    copy(selectedRow = (selectedRow + delta + size) % size)

  def adjustFirstRow(delta: Int): SingleObjectTableBrowserState = copy(firstRow = firstRow + delta)

  def selectedField: String = model.fields.toSeq(selectedRow)._1

  def selectedRawValue: MashValue = model.rawValues.toSeq(selectedRow)._2

  def withPath(newPath: String): SingleObjectTableBrowserState = copy(path = newPath)

  def getInsertExpression: String = {
    val field = model.rawValues.toSeq(selectedRow)._1
    val command = path
    BrowserState.safeProperty(command, field)
  }

}
