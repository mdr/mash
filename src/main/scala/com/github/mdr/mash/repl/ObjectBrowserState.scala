package com.github.mdr.mash.repl

import com.github.mdr.mash.printer.ObjectTableModel

case class ObjectBrowserState(model: ObjectTableModel, currentRow: Int = 0, firstRow: Int = 0) {

  def adjustCurrentRow(delta: Int) = copy(currentRow = currentRow + delta)

  def adjustFirstRow(delta: Int) = copy(firstRow = firstRow + delta)

}