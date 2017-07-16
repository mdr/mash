package com.github.mdr.mash.repl.browser

import java.util.regex.{ Pattern, PatternSyntaxException }

import com.github.mdr.mash.parser.ExpressionCombiner._
import com.github.mdr.mash.printer.model.SingleObjectTableModel
import com.github.mdr.mash.repl.LineBuffer
import com.github.mdr.mash.runtime.{ MashObject, MashValue }
import com.github.mdr.mash.utils.{ Point, Region }
import com.github.mdr.mash.utils.Utils._

import scala.collection.mutable.ArrayBuffer

case class SingleObjectTableBrowserState(model: SingleObjectTableModel,
                                         currentRow: Int = 0,
                                         firstRow: Int = 0,
                                         markedRows: Set[Int] = Set(),
                                         path: String,
                                         searchStateOpt: Option[SearchState] = None,
                                         expressionStateOpt: Option[ExpressionState] = None) extends BrowserState {

  def beginSearch: BrowserState = copy(searchStateOpt = Some(SearchState("")))

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
    val fields = model.fields.toSeq
    val tuples: Seq[(Point, CellSearchInfo)] =
      for {
        row ← 0 until numberOfRows
        point ← Seq(Point(row, 0), Point(row, 1))
        (field, value) = fields(row)
        s = if (point.column == 0) field else value
        cellInfo ← getCellSearchInfo(pattern, s)
      } yield point -> cellInfo
    val newRow =
      tuples.collectFirst { case (point, _) if point.row >= currentRow ⇒ point.row }
        .orElse(tuples.collectFirst { case (point, _) ⇒ point.row })
        .getOrElse(currentRow)
    val searchInfo = SearchState(query, tuples.toMap, ignoreCase)
    copy(searchStateOpt = Some(searchInfo), currentRow = newRow).adjustWindowToFit(terminalRows)
  }

  private def getCellSearchInfo(pattern: Pattern, s: String): Option[CellSearchInfo] = {
    val matcher = pattern.matcher(s)
    val regions = ArrayBuffer[Region]()
    while (matcher.find)
      regions += Region(matcher.start, matcher.end - matcher.start)
    if (regions.nonEmpty) Some(CellSearchInfo(regions)) else None
  }

  private def ifSearching(f: SearchState ⇒ BrowserState): BrowserState = searchStateOpt.map(f).getOrElse(this)

  override def rawValue: MashObject = model.rawValue

  def numberOfRows = model.numberOfRows

  def previousItem(terminalRows: Int): SingleObjectTableBrowserState = adjustSelectedRow(-1, terminalRows)

  def nextItem(terminalRows: Int): SingleObjectTableBrowserState = adjustSelectedRow(1, terminalRows)

  def adjustSelectedRow(delta: Int, terminalRows: Int): SingleObjectTableBrowserState =
    this.when(numberOfRows > 0, _.copy(currentRow = (currentRow + delta + numberOfRows) % numberOfRows).adjustWindowToFit(terminalRows))

  def adjustFirstRow(delta: Int): SingleObjectTableBrowserState = copy(firstRow = firstRow + delta)

  def toggleMark: SingleObjectTableBrowserState =
    if (markedRows contains currentRow)
      copy(markedRows = markedRows - currentRow)
    else
      copy(markedRows = markedRows + currentRow)

  def firstItem(terminalRows: Int): SingleObjectTableBrowserState =
    copy(currentRow = 0).adjustWindowToFit(terminalRows)

  def lastItem(terminalRows: Int): SingleObjectTableBrowserState =
    copy(currentRow = numberOfRows - 1).adjustWindowToFit(terminalRows)

  def selectedField: String = model.fields.toSeq(currentRow)._1

  def selectedRawValue: MashValue = model.rawValues.toSeq(currentRow)._2

  def withPath(newPath: String): SingleObjectTableBrowserState = copy(path = newPath)

  override def selectionInfo: SelectionInfo =
    markedRows.toSeq.sorted match {
      case Seq()         ⇒ selectionInfo(currentRow)
      case Seq(rowIndex) ⇒ selectionInfo(rowIndex)
      case rowIndices    ⇒ selectionInfo(rowIndices)
    }

  private def selectionInfo(rowIndex: Int): SelectionInfo = {
    val field = model.rawValues.toSeq(rowIndex)._1
    val selectionPath = BrowserState.safeProperty(path, field)
    SelectionInfo(selectionPath, selectedRawValue)
  }

  private def selectionInfo(rowIndices: Seq[Int]): SelectionInfo = {
    val fields = model.rawValue.immutableFields.toSeq
    val selectedFields = rowIndices.map(fields)
    val selectionExpression = selectedFields.map("--" + _._1).mkString(" | select ", " ", "")
    val selectedPath = combineSafely(path, selectionExpression)
    val selectedValue = MashObject.of(selectedFields)
    SelectionInfo(selectedPath, selectedValue)
  }

  def adjustWindowToFit(terminalRows: Int): SingleObjectTableBrowserState = {
    var newState = this

    val delta = currentRow - (firstRow + windowSize(terminalRows) - 1)
    if (delta >= 0)
      newState = newState.adjustFirstRow(delta)

    val delta2 = firstRow - currentRow
    if (delta2 >= 0)
      newState = newState.adjustFirstRow(-delta2)

    newState
  }

  // an upper status line, one (or three with a class) header row(s), a footer row, a status line
  def windowSize(terminalRows: Int) = terminalRows - 3 - (if (model.classNameOpt.isDefined) 3 else 1)

  def nextPage(terminalRows: Int): SingleObjectTableBrowserState = {
    val newRow = math.min(model.numberOfRows - 1, currentRow + windowSize(terminalRows) - 1)
    copy(currentRow = newRow).adjustWindowToFit(terminalRows)
  }

  def previousPage(terminalRows: Int): SingleObjectTableBrowserState = {
    val newRow = math.max(0, currentRow - windowSize(terminalRows) - 1)
    copy(currentRow = newRow).adjustWindowToFit(terminalRows)
  }

  def withExpressionState(expressionStateOpt: Option[ExpressionState]): BrowserState =
    copy(expressionStateOpt = expressionStateOpt)

}
