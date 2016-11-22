package com.github.mdr.mash.repl

import com.github.mdr.mash.printer.ObjectsTableModel
import com.github.mdr.mash.runtime.MashValue

case class ObjectsTableBrowserState(model: ObjectsTableModel,
                                    selectedRow: Int = 0,
                                    firstRow: Int = 0,
                                    currentColumnOpt: Option[Int] = None,
                                    markedRows: Set[Int] = Set(),
                                    path: String) extends BrowserState {

  def rawValue: MashValue = model.rawValue

  private val size = model.objects.size
  private val numberOfColumns = model.numberOfColumns

  def adjustSelectedRow(delta: Int): ObjectsTableBrowserState =
    copy(selectedRow = (selectedRow + delta + size) % size)

  def adjustSelectedColumn(delta: Int): ObjectsTableBrowserState = {
    val currentColumn = currentColumnOpt.getOrElse(if (delta == 1) -1 else 0)
    copy(currentColumnOpt = Some((currentColumn + delta + numberOfColumns) % numberOfColumns))
  }

  def unfocusColumn: ObjectsTableBrowserState = copy(currentColumnOpt = None)

  def lastColumn: ObjectsTableBrowserState = copy(currentColumnOpt = Some(numberOfColumns - 1))

  def firstColumn: ObjectsTableBrowserState = copy(currentColumnOpt = Some(0))

  def adjustFirstRow(delta: Int): ObjectsTableBrowserState = copy(firstRow = firstRow + delta)

  def toggleMark: ObjectsTableBrowserState =
    if (markedRows contains selectedRow)
      copy(markedRows = markedRows - selectedRow)
    else
      copy(markedRows = markedRows + selectedRow)

  def withPath(newPath: String): ObjectsTableBrowserState = copy(path = newPath)

}
