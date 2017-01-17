package com.github.mdr.mash.repl.browser

import java.util.regex.{ Pattern, PatternSyntaxException }

import com.github.mdr.mash.parser.SafeParens
import com.github.mdr.mash.printer.model.ObjectsTableModel
import com.github.mdr.mash.repl.browser.BrowserState.safeProperty
import com.github.mdr.mash.repl.browser.ObjectsTableBrowserState.SearchState
import com.github.mdr.mash.runtime.{ MashList, MashValue }
import com.github.mdr.mash.screen.Point
import com.github.mdr.mash.utils.Region
import com.github.mdr.mash.utils.Utils._

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
                                    searchStateOpt: Option[SearchState] = None,
                                    expressionOpt: Option[String] = None) extends BrowserState {

  def toggleCase(terminalRows: Int): BrowserState = ifSearching { searchState ⇒
    runSearch(searchState.query, !searchState.ignoreCase, terminalRows)
  }

  private def ifSearching(f: SearchState ⇒ BrowserState): BrowserState = searchStateOpt.map(f).getOrElse(this)

  def stopSearching: BrowserState = copy(searchStateOpt = None)

  def nextHit(terminalRows: Int): BrowserState = ifSearching { searchState ⇒
    val rows = searchState.rows
    val nextRow = rows.find(_ > selectedRow).orElse(rows.headOption).getOrElse(selectedRow)
    copy(selectedRow = nextRow).adjustWindowToFit(terminalRows)
  }

  def previousHit(terminalRows: Int): BrowserState = ifSearching { searchState ⇒
    val rows = searchState.rows.reverse
    val nextRow = rows.find(_ < selectedRow).orElse(rows.headOption).getOrElse(selectedRow)
    copy(selectedRow = nextRow).adjustWindowToFit(terminalRows)
  }

  private def runSearch(query: String, ignoreCase: Boolean, terminalRows: Int): BrowserState = {
    val flags = if (ignoreCase) Pattern.CASE_INSENSITIVE else 0
    val pattern =
      try
        Pattern.compile(query, flags)
      catch {
        case _: PatternSyntaxException ⇒
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
      tuples.collectFirst { case (point, _) if point.row >= selectedRow ⇒ point.row }
        .orElse(tuples.collectFirst { case (point, _) ⇒ point.row })
        .getOrElse(selectedRow)
    val searchInfo = SearchState(query, tuples.toMap, ignoreCase)
    copy(searchStateOpt = Some(searchInfo), selectedRow = newRow).adjustWindowToFit(terminalRows)
  }

  def setExpression(expression: String): BrowserState = copy(expressionOpt = Some(expression))
  def acceptExpression: BrowserState = copy(expressionOpt = None)

  def setSearch(query: String, terminalRows: Int): BrowserState = {
    val ignoreCase = searchStateOpt.forall(_.ignoreCase)
    runSearch(query, ignoreCase, terminalRows)
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

  def previousItem(terminalRows: Int): ObjectsTableBrowserState = adjustSelectedRow(-1).adjustWindowToFit(terminalRows)

  def nextItem(terminalRows: Int): ObjectsTableBrowserState = adjustSelectedRow(1).adjustWindowToFit(terminalRows)

  def adjustSelectedRow(delta: Int): ObjectsTableBrowserState =
    this.when(size > 0, _.copy(selectedRow = (selectedRow + delta + size) % size))

  def nextColumn: ObjectsTableBrowserState = adjustSelectedColumn(1)

  def previousColumn: ObjectsTableBrowserState = adjustSelectedColumn(-1)

  def adjustSelectedColumn(delta: Int): ObjectsTableBrowserState =
    if (numberOfColumns == 0)
      this
    else {
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

  def getInsertExpression: String = selectionInfo.path

  def selectionInfo: SelectionInfo = {
    val safePath = SafeParens.safeParens(path)
    if (markedRows.isEmpty) {
      val rowObject = model.rawObjects(selectedRow)
      val rowPath = s"$safePath[$selectedRow]"
      val cellSelectionInfoOpt =
        for {
          column <- currentColumnOpt
          field = model.columnNames(column)
          cellObject <- model.objects(selectedRow).rawObjects.get(field)
          newPath = safeProperty(rowPath, field)
        } yield SelectionInfo(newPath, cellObject)
      cellSelectionInfoOpt.getOrElse(SelectionInfo(rowPath, rowObject))
    } else {
      val rowIndices = markedRows.toSeq.sorted
      val rowsPath = rowIndices.map(i ⇒ s"$safePath[$i]").mkString("[", ", ", "]")
      val rowObjects = MashList(rowIndices.map(model.rawObjects))
      SelectionInfo(rowsPath, rowObjects)
    }
  }

  def adjustWindowToFit(terminalRows: Int): ObjectsTableBrowserState = {
    var newState = this

    val delta = selectedRow - (firstRow + windowSize(terminalRows) - 1)
    if (delta >= 0)
      newState = newState.adjustFirstRow(delta)

    val delta2 = firstRow - selectedRow
    if (delta2 >= 0)
      newState = newState.adjustFirstRow(-delta2)

    newState
  }

  def windowSize(terminalRows: Int) = terminalRows - 6 // three header rows, a footer row, two status lines

  def nextPage(terminalRows: Int): ObjectsTableBrowserState = {
    val newRow = math.min(model.objects.size - 1, selectedRow + windowSize(terminalRows) - 1)
    copy(selectedRow = newRow).adjustWindowToFit(terminalRows)
  }

  def previousPage(terminalRows: Int): ObjectsTableBrowserState = {
    val newRow = math.max(0, selectedRow - windowSize(terminalRows) - 1)
    copy(selectedRow = newRow).adjustWindowToFit(terminalRows)
  }

  def firstItem(terminalRows: Int): ObjectsTableBrowserState =
    copy(selectedRow = 0).adjustWindowToFit(terminalRows)

  def lastItem(terminalRows: Int): ObjectsTableBrowserState =
    copy(selectedRow = size - 1).adjustWindowToFit(terminalRows)

}