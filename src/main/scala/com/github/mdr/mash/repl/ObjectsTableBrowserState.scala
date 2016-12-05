package com.github.mdr.mash.repl

import com.github.mdr.mash.printer.ObjectsTableModel
import com.github.mdr.mash.repl.ObjectsTableBrowserState.SearchState
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.screen.Point
import com.github.mdr.mash.utils.Region

case class CellSearchInfo(matches: Seq[Region])

object ObjectsTableBrowserState {
  case class SearchState(query: String, byPoint: Map[Point, CellSearchInfo] = Map())
}

case class ObjectsTableBrowserState(model: ObjectsTableModel,
                                    selectedRow: Int = 0,
                                    firstRow: Int = 0,
                                    currentColumnOpt: Option[Int] = None,
                                    markedRows: Set[Int] = Set(),
                                    path: String,
                                    hiddenColumns: Seq[String] = Seq(),
                                    searchStateOpt: Option[SearchState] = None) extends BrowserState {

  def stopSearching: BrowserState = copy(searchStateOpt = None)

  def setSearch(query: String, windowSize: Int): BrowserState = {
    val tuples: Seq[(Point, CellSearchInfo)] =
      for {
        row <- 0 until size
        column <- 0 until numberOfColumns
        point = Point(row, column)
        cellInfo <- getCellSearchInfo(query, row, column)
      } yield point -> cellInfo
    val newRow =
      tuples.collectFirst { case (point, _) if point.row >= selectedRow => point.row }
        .orElse(tuples.collectFirst { case (point, _) => point.row })
        .getOrElse(selectedRow)
    val searchInfo = SearchState(query, tuples.toMap)
    copy(searchStateOpt = Some(searchInfo), selectedRow = newRow).adjustWindowToFit(windowSize)
  }

  private def getCellSearchInfo(query: String, row: Int, column: Int): Option[CellSearchInfo] = {
    val obj = model.objects(row)
    val s = obj.data(model.columnNames(column))
    val matches = query.r.findAllMatchIn(s).toList
    val regions = matches.map(m => Region(m.start, m.end - m.start))
    if (regions.nonEmpty) Some(CellSearchInfo(regions)) else None
  }

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

  def getInsertExpression: String = {
    val command = path
    if (markedRows.isEmpty) {
      val rowPath = s"$command[$selectedRow]"
      currentColumnOpt match {
        case Some(column) if column > 0 =>
          val property = model.columnNames(column)
          BrowserState.safeProperty(rowPath, property)
        case _                          =>
          s"$rowPath"
      }
    } else {
      val rows = markedRows.toSeq.sorted
      val items = rows.map(i ⇒ s"$command[$i]").mkString(", ")
      s"[$items]"
    }
  }

  def adjustWindowToFit(windowSize: Int): ObjectsTableBrowserState = {
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
