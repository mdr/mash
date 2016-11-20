package com.github.mdr.mash.repl

import com.github.mdr.mash.printer.ObjectModel
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

}
