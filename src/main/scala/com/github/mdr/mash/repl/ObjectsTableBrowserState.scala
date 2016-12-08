package com.github.mdr.mash.repl

import java.util.regex.{ Pattern, PatternSyntaxException }

import scala.util.matching.Regex
import com.github.mdr.mash.printer.ObjectsTableModel
import com.github.mdr.mash.repl.ObjectsTableBrowserState.SearchState
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.screen.Point
import com.github.mdr.mash.utils.Region

import scala.collection.mutable.ArrayBuffer

case class CellSearchInfo(matches: Seq[Region])

object ObjectsTableBrowserState {

  case class SearchState(query: String, byPoint: Map[Point, CellSearchInfo] = Map(), ignoreCase: Boolean = true) {
    def rows = byPoint.map(_._1.row).toSeq.distinct.sorted
  }

}

case class ObjectsTableBrowserState(model: ObjectsTableModel,
                                    selectedRow: Int = 0,
                                    firstRow: Int = 0,
                                    currentColumnOpt: Option[Int] = None,
                                    markedRows: Set[Int] = Set(),
                                    path: String,
                                    hiddenColumns: Seq[String] = Seq(),
                                    searchStateOpt: Option[SearchState] = None) extends BrowserState {

  def toggleCase(windowSize: Int): BrowserState = ifSearching { searchState =>
    runSearch(searchState.query, !searchState.ignoreCase, windowSize)
  }

  private def ifSearching(f: SearchState => BrowserState): BrowserState = searchStateOpt.map(f).getOrElse(this)

  def stopSearching: BrowserState = copy(searchStateOpt = None)

  def nextHit(windowSize: Int): BrowserState = ifSearching { searchState =>
    val rows = searchState.rows
    val nextRow = rows.find(_ > selectedRow).orElse(rows.headOption).getOrElse(selectedRow)
    copy(selectedRow = nextRow).adjustWindowToFit(windowSize)
  }

  def previousHit(windowSize: Int): BrowserState = ifSearching { searchState =>
    val rows = searchState.rows.reverse
    val nextRow = rows.find(_ < selectedRow).orElse(rows.headOption).getOrElse(selectedRow)
    copy(selectedRow = nextRow).adjustWindowToFit(windowSize)
  }

  private def runSearch(query: String, ignoreCase: Boolean, windowSize: Int): BrowserState = {
    val flags = if (ignoreCase) Pattern.CASE_INSENSITIVE else 0
    val pattern =
      try
        Pattern.compile(query, flags)
      catch {
        case _: PatternSyntaxException =>
          Pattern.compile(Pattern.quote(query))
      }
    val tuples: Seq[(Point, CellSearchInfo)] =
      for {
        row <- 0 until size
        column <- 0 until numberOfColumns
        point = Point(row, column)
        cellInfo <- getCellSearchInfo(pattern, row, column)
      } yield point -> cellInfo
    val newRow =
      tuples.collectFirst { case (point, _) if point.row >= selectedRow => point.row }
        .orElse(tuples.collectFirst { case (point, _) => point.row })
        .getOrElse(selectedRow)
    val searchInfo = SearchState(query, tuples.toMap, ignoreCase)
    copy(searchStateOpt = Some(searchInfo), selectedRow = newRow).adjustWindowToFit(windowSize)
  }

  def setSearch(query: String, windowSize: Int): BrowserState = {
    val ignoreCase = searchStateOpt.map(_.ignoreCase).getOrElse(true)
    runSearch(query, ignoreCase, windowSize)
  }

  private def getCellSearchInfo(pattern: Pattern, row: Int, column: Int): Option[CellSearchInfo] = {
    val obj = model.objects(row)
    val s = obj.data(model.columnNames(column))
    val matcher = pattern.matcher(s)
    val regions = ArrayBuffer[Region]()
    while (matcher.find)
      regions += Region(matcher.start, matcher.end - matcher.start)
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
