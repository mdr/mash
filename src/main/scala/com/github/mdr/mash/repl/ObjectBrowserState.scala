package com.github.mdr.mash.repl

import com.github.mdr.mash.printer.ObjectTableModel

case class ObjectBrowserState(model: ObjectTableModel,
                              currentRow: Int = 0,
                              firstRow: Int = 0,
                              currentColumnOpt: Option[Int] = None,
                              selectedRows: Set[Int] = Set()) {

  private val size = model.objects.size
  private val numberOfColumns = model.numberOfColumns

  def adjustCurrentRow(delta: Int): ObjectBrowserState = copy(currentRow = (currentRow + delta + size) % size)

  def adjustCurrentColumn(delta: Int): ObjectBrowserState = {
    val currentColumn = currentColumnOpt.getOrElse(if (delta == 1) -1 else 0)
    copy(currentColumnOpt = Some((currentColumn + delta + numberOfColumns) % numberOfColumns))
  }

  def unfocusColumn: ObjectBrowserState = copy(currentColumnOpt = None)

  def rightmostColumn: ObjectBrowserState = copy(currentColumnOpt = Some(numberOfColumns - 1))

  def leftmostColumn: ObjectBrowserState = copy(currentColumnOpt = Some(0))

  def adjustFirstRow(delta: Int): ObjectBrowserState = copy(firstRow = firstRow + delta)

  def toggleSelectionOfCurrentRow: ObjectBrowserState =
    if (selectedRows contains currentRow)
      copy(selectedRows = selectedRows - currentRow)
    else
      copy(selectedRows = selectedRows + currentRow)
}
