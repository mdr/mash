package com.github.mdr.mash.repl

import com.github.mdr.mash.printer.ObjectModel

case class SingleObjectBrowserState(model: ObjectModel,
                                    currentRow: Int = 0,
                                    firstRow: Int = 0,
                                    path: String) extends BrowserState {

  private val size = model.fields.size

  def adjustCurrentRow(delta: Int): SingleObjectBrowserState =
    copy(currentRow = (currentRow + delta + size) % size)

  def adjustFirstRow(delta: Int): SingleObjectBrowserState = copy(firstRow = firstRow + delta)

}
