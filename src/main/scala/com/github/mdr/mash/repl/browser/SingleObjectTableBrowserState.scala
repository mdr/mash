package com.github.mdr.mash.repl.browser

import java.util.regex.{ Pattern, PatternSyntaxException }

import com.github.mdr.mash.printer.model.SingleObjectTableModel
import com.github.mdr.mash.repl.browser.TwoDTableBrowserState.{ CellSearchInfo, SearchState }
import com.github.mdr.mash.runtime.{ MashObject, MashValue }
import com.github.mdr.mash.screen.Point
import com.github.mdr.mash.utils.Region
import com.github.mdr.mash.utils.Utils._

import scala.collection.mutable.ArrayBuffer

case class SingleObjectTableBrowserState(model: SingleObjectTableModel,
                                         selectedRow: Int = 0,
                                         firstRow: Int = 0,
                                         path: String,
                                         searchStateOpt: Option[SearchState] = None,
                                         expressionOpt: Option[String] = None) extends BrowserState {

  def lastRow = size - 1

  def setSearch(query: String, terminalRows: Int): BrowserState = {
    val ignoreCase = searchStateOpt.forall(_.ignoreCase)
    runSearch(query, ignoreCase, terminalRows)
  }

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
        row ← 0 until size
        point ← Seq(Point(row, 0), Point(row, 1))
        (field, value) = fields(row)
        s = if (point.column == 0) field else value
        cellInfo ← getCellSearchInfo(pattern, s)
      } yield point -> cellInfo
    val newRow =
      tuples.collectFirst { case (point, _) if point.row >= selectedRow ⇒ point.row }
        .orElse(tuples.collectFirst { case (point, _) ⇒ point.row })
        .getOrElse(selectedRow)
    val searchInfo = SearchState(query, tuples.toMap, ignoreCase)
    copy(searchStateOpt = Some(searchInfo), selectedRow = newRow).adjustWindowToFit(terminalRows)
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

  val size = model.fields.size

  def previousItem(terminalRows: Int): SingleObjectTableBrowserState = adjustSelectedRow(-1, terminalRows)

  def nextItem(terminalRows: Int): SingleObjectTableBrowserState = adjustSelectedRow(1, terminalRows)

  def adjustSelectedRow(delta: Int, terminalRows: Int): SingleObjectTableBrowserState =
    this.when(size > 0, _.copy(selectedRow = (selectedRow + delta + size) % size).adjustWindowToFit(terminalRows))

  def adjustFirstRow(delta: Int): SingleObjectTableBrowserState = copy(firstRow = firstRow + delta)

  def firstItem(terminalRows: Int): SingleObjectTableBrowserState =
    copy(selectedRow = 0).adjustWindowToFit(terminalRows)

  def lastItem(terminalRows: Int): SingleObjectTableBrowserState =
    copy(selectedRow = size - 1).adjustWindowToFit(terminalRows)

  def selectedField: String = model.fields.toSeq(selectedRow)._1

  def selectedRawValue: MashValue = model.rawValues.toSeq(selectedRow)._2

  def withPath(newPath: String): SingleObjectTableBrowserState = copy(path = newPath)

  def getInsertExpression: String = {
    val field = model.rawValues.toSeq(selectedRow)._1
    val command = path
    BrowserState.safeProperty(command, field)
  }

  override def selectionInfo: SelectionInfo = SelectionInfo(getInsertExpression, selectedRawValue)

  def adjustWindowToFit(terminalRows: Int): SingleObjectTableBrowserState = {
    var newState = this

    val delta = selectedRow - (firstRow + windowSize(terminalRows) - 1)
    if (delta >= 0)
      newState = newState.adjustFirstRow(delta)

    val delta2 = firstRow - selectedRow
    if (delta2 >= 0)
      newState = newState.adjustFirstRow(-delta2)

    newState
  }

  // an upper status line, one (or three with a class) header row(s), a footer row, a status line
  def windowSize(terminalRows: Int) = terminalRows - 3 - (if (model.classNameOpt.isDefined) 3 else 1)

  def setExpression(expression: String): BrowserState = copy(expressionOpt = Some(expression))

  def acceptExpression: BrowserState = copy(expressionOpt = None)

}
