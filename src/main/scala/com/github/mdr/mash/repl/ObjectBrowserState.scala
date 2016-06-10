package com.github.mdr.mash.repl

import com.github.mdr.mash.printer.ObjectTableModel

case class ObjectBrowserState(model: ObjectTableModel, currentRow: Int = 0, firstRow: Int = 0, selectedRows: Set[Int] = Set()) {

  private val size = model.objects.size

  def adjustCurrentRow(delta: Int): ObjectBrowserState = copy(currentRow = (currentRow + delta + size) % size)

  def adjustFirstRow(delta: Int): ObjectBrowserState = copy(firstRow = firstRow + delta)

  def toggleSelectionOfCurrentRow: ObjectBrowserState =
    if (selectedRows contains currentRow)
      copy(selectedRows = selectedRows - currentRow)
    else
      copy(selectedRows = selectedRows + currentRow)
}
