package com.github.mdr.mash.repl

import com.github.mdr.mash.printer.ObjectTableModel

case class ObjectTableBrowserState(model: ObjectTableModel,
                                   currentRow: Int = 0,
                                   firstRow: Int = 0,
                                   currentColumnOpt: Option[Int] = None,
                                   markedRows: Set[Int] = Set(),
                                   val path: String) extends BrowserState {

  private val size = model.objects.size
  private val numberOfColumns = model.numberOfColumns

  def adjustCurrentRow(delta: Int): ObjectTableBrowserState =
    copy(currentRow = (currentRow + delta + size) % size)

  def adjustCurrentColumn(delta: Int): ObjectTableBrowserState = {
    val currentColumn = currentColumnOpt.getOrElse(if (delta == 1) -1 else 0)
    copy(currentColumnOpt = Some((currentColumn + delta + numberOfColumns) % numberOfColumns))
  }

  def unfocusColumn: ObjectTableBrowserState = copy(currentColumnOpt = None)

  def lastColumn: ObjectTableBrowserState = copy(currentColumnOpt = Some(numberOfColumns - 1))

  def firstColumn: ObjectTableBrowserState = copy(currentColumnOpt = Some(0))

  def adjustFirstRow(delta: Int): ObjectTableBrowserState = copy(firstRow = firstRow + delta)

  def toggleMark: ObjectTableBrowserState =
    if (markedRows contains currentRow)
      copy(markedRows = markedRows - currentRow)
    else
      copy(markedRows = markedRows + currentRow)
}
