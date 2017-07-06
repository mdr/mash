package com.github.mdr.mash.repl.browser

import java.util.regex.{ Pattern, PatternSyntaxException }

import com.github.mdr.mash.printer.ColumnId
import com.github.mdr.mash.printer.model.TwoDTableModel
import com.github.mdr.mash.repl.browser.TwoDTableBrowserState.{ CellSearchInfo, SearchState }
import com.github.mdr.mash.runtime.{ MashList, MashValue }
import com.github.mdr.mash.screen.Point
import com.github.mdr.mash.utils.Region
import com.github.mdr.mash.utils.Utils._

import scala.collection.mutable.ArrayBuffer

object TwoDTableBrowserState {

  case class CellSearchInfo(matches: Seq[Region]) {
    def isMatched(offset: Int): Boolean = matches exists (_ contains offset)
  }

  case class SearchState(query: String, byPoint: Map[Point, CellSearchInfo] = Map(), ignoreCase: Boolean = true) {

    lazy val rows: Seq[Int] = byPoint.map(_._1.row).toSeq.distinct.sorted

    def getCellSearchInfo(point: Point): Option[CellSearchInfo] = byPoint.get(point)

  }

}

case class TwoDTableBrowserState(model: TwoDTableModel,
                                 currentRow: Int = 0,
                                 firstRow: Int = 0,
                                 currentColumnOpt: Option[Int] = None,
                                 markedRows: Set[Int] = Set(),
                                 path: String,
                                 hiddenColumns: Seq[ColumnId] = Seq(),
                                 searchStateOpt: Option[SearchState] = None,
                                 expressionOpt: Option[String] = None) extends BrowserState {

  private def currentColumnIdOpt: Option[ColumnId] = currentColumnOpt.map(model.columnIds)

  def setSearch(query: String, terminalRows: Int): BrowserState = {
    val ignoreCase = searchStateOpt.forall(_.ignoreCase)
    runSearch(query, ignoreCase, terminalRows)
  }

  def nextHit(terminalRows: Int): BrowserState = ifSearching { searchState ⇒
    val rows = searchState.rows
    val nextRow = rows.find(_ > currentRow).orElse(rows.headOption).getOrElse(currentRow)
    copy(currentRow = nextRow).adjustWindowToFit(terminalRows)
  }

  def previousHit(terminalRows: Int): BrowserState = ifSearching { searchState ⇒
    val rows = searchState.rows.reverse
    val nextRow = rows.find(_ < currentRow).orElse(rows.headOption).getOrElse(currentRow)
    copy(currentRow = nextRow).adjustWindowToFit(terminalRows)
  }

  def toggleCase(terminalRows: Int): BrowserState = ifSearching { searchState ⇒
    runSearch(searchState.query, !searchState.ignoreCase, terminalRows)
  }

  def stopSearching: BrowserState = copy(searchStateOpt = None)

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
        row <- 0 until numberOfRows
        column <- 0 until numberOfColumns
        point = Point(row, column)
        cellInfo <- getCellSearchInfo(pattern, row, column)
      } yield point -> cellInfo
    val newRow =
      tuples.collectFirst { case (point, _) if point.row >= currentRow ⇒ point.row }
        .orElse(tuples.collectFirst { case (point, _) ⇒ point.row })
        .getOrElse(currentRow)
    val searchInfo = SearchState(query, tuples.toMap, ignoreCase)
    copy(searchStateOpt = Some(searchInfo), currentRow = newRow).adjustWindowToFit(terminalRows)
  }

  private def getCellSearchInfo(pattern: Pattern, rowIndex: Int, column: Int): Option[CellSearchInfo] = {
    val row = model.rows(rowIndex)
    val columnId = model.columnIds(column)
    val s = row.renderedValue(columnId)
    val matcher = pattern.matcher(s)
    val regions = ArrayBuffer[Region]()
    while (matcher.find)
      regions += Region(matcher.start, matcher.end - matcher.start)
    if (regions.nonEmpty) Some(CellSearchInfo(regions)) else None
  }

  private def ifSearching(f: SearchState ⇒ BrowserState): BrowserState = searchStateOpt.map(f).getOrElse(this)

  def setExpression(expression: String): BrowserState = copy(expressionOpt = Some(expression))

  def acceptExpression: BrowserState = copy(expressionOpt = None)

  def rawValue: MashValue = model.rawValue

  private def numberOfRows = model.numberOfRows

  private def numberOfColumns = model.numberOfColumns

  def previousItem(terminalRows: Int): TwoDTableBrowserState = adjustSelectedRow(-1).adjustWindowToFit(terminalRows)

  def nextItem(terminalRows: Int): TwoDTableBrowserState = adjustSelectedRow(1).adjustWindowToFit(terminalRows)

  def adjustSelectedRow(delta: Int): TwoDTableBrowserState =
    this.when(numberOfRows > 0, _.copy(currentRow = (currentRow + delta + numberOfRows) % numberOfRows))

  def nextColumn: TwoDTableBrowserState = adjustSelectedColumn(1)

  def previousColumn: TwoDTableBrowserState = adjustSelectedColumn(-1)

  def adjustSelectedColumn(delta: Int): TwoDTableBrowserState =
    if (numberOfColumns == 0)
      this
    else {
      val currentColumn = currentColumnOpt.getOrElse(if (delta == 1) -1 else 0)
      copy(currentColumnOpt = Some((currentColumn + delta + numberOfColumns) % numberOfColumns))
    }

  def unfocusColumn: TwoDTableBrowserState = copy(currentColumnOpt = None)

  def lastColumn: TwoDTableBrowserState = copy(currentColumnOpt = Some(numberOfColumns - 1))

  def firstColumn: TwoDTableBrowserState = copy(currentColumnOpt = Some(0))

  def adjustFirstRow(delta: Int): TwoDTableBrowserState = copy(firstRow = firstRow + delta)

  def toggleMark: TwoDTableBrowserState =
    if (markedRows contains currentRow)
      copy(markedRows = markedRows - currentRow)
    else
      copy(markedRows = markedRows + currentRow)

  def withPath(newPath: String): TwoDTableBrowserState = copy(path = newPath)

  def getInsertExpression: String = selectionInfo.path

  def selectionInfo: SelectionInfo =
    markedRows.toSeq.sorted match {
      case Seq()         ⇒ selectionInfo(currentRow, includeCellSelection = true)
      case Seq(rowIndex) ⇒ selectionInfo(rowIndex, includeCellSelection = false)
      case rowIndices    ⇒ selectionInfo(rowIndices)
    }

  private def selectionInfo(rowIndices: Seq[Int]): SelectionInfo = {
    val rowsPath =
      rowIndices
        .map(rowIndex ⇒ model.rowFetch(rowIndex).fetchPath(path))
        .mkString("[", ", ", "]")
    val rowObjects = MashList(rowIndices.map(model.rowValues))
    SelectionInfo(rowsPath, rowObjects)
  }

  private def selectionInfo(rowIndex: Int, includeCellSelection: Boolean): SelectionInfo = {
    val rowValue = model.rowValue(rowIndex)
    val rowPath = model.rowFetch(rowIndex).fetchPath(path)
    val cellSelectionInfoOpt =
      for {
        columnId ← currentColumnIdOpt
        if includeCellSelection
        cellValue ← model.rows(rowIndex).cells(columnId).rawValueOpt
        fetch ← model.columns(columnId).fetchOpt
        newPath = fetch.fetchPath(rowPath)
      } yield SelectionInfo(newPath, cellValue)
    cellSelectionInfoOpt.getOrElse(SelectionInfo(rowPath, rowValue))
  }

  def adjustWindowToFit(terminalRows: Int): TwoDTableBrowserState = {
    var newState = this

    val delta = currentRow - (firstRow + windowSize(terminalRows) - 1)
    if (delta >= 0)
      newState = newState.adjustFirstRow(delta)

    val delta2 = firstRow - currentRow
    if (delta2 >= 0)
      newState = newState.adjustFirstRow(-delta2)

    newState
  }

  def windowSize(terminalRows: Int) = terminalRows - 6 // three header rows, a footer row, two status lines

  def nextPage(terminalRows: Int): TwoDTableBrowserState = {
    val newRow = math.min(model.numberOfRows- 1, currentRow + windowSize(terminalRows) - 1)
    copy(currentRow = newRow).adjustWindowToFit(terminalRows)
  }

  def previousPage(terminalRows: Int): TwoDTableBrowserState = {
    val newRow = math.max(0, currentRow - windowSize(terminalRows) - 1)
    copy(currentRow = newRow).adjustWindowToFit(terminalRows)
  }

  def firstItem(terminalRows: Int): TwoDTableBrowserState =
    copy(currentRow = 0).adjustWindowToFit(terminalRows)

  def lastItem(terminalRows: Int): TwoDTableBrowserState =
    copy(currentRow = numberOfRows - 1).adjustWindowToFit(terminalRows)

}