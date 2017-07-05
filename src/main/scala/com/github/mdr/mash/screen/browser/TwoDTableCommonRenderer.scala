package com.github.mdr.mash.screen.browser

import com.github.mdr.mash.printer.model.{ TwoDTableModel, TwoDTableModelCreator }
import com.github.mdr.mash.printer.{ ColumnId, UnicodeBoxCharacterSupplier }
import com.github.mdr.mash.repl.browser.TwoDTableBrowserState.SearchState
import com.github.mdr.mash.screen.Style.StylableString
import com.github.mdr.mash.screen._
import com.github.mdr.mash.screen.browser.ArrowHelper._
import com.github.mdr.mash.utils.StringUtils
import com.github.mdr.mash.utils.Utils._

import scala.collection.mutable.ArrayBuffer

class TwoDTableCommonRenderer(model: TwoDTableModel,
                              markedRowsOpt: Option[Set[Int]] = None,
                              currentRowIndexOpt: Option[Int] = None,
                              currentColumnIndexOpt: Option[Int] = None,
                              searchStateOpt: Option[SearchState] = None) {

  import UnicodeBoxCharacterSupplier._

  def renderTableLines(rowOffset: Int = 0, rowCount: Int = model.numberOfRows): Seq[Line] = {
    val moreDataItemsBelowWindow = rowOffset + rowCount < model.numberOfRows
    val headerLines = renderHeaderLines(moreDataItemsAboveWindow = rowOffset > 0)
    val dataLines = renderDataLines(rowOffset: Int, rowCount: Int)
    val footerLine = renderFooterLine(model, addArrow = moreDataItemsBelowWindow)
    headerLines ++ dataLines ++ Seq(footerLine)
  }

  private def renderHeaderRow(model: TwoDTableModel): Line = {
    def renderColumn(columnId: ColumnId): StyledString = {
      val fit = StringUtils.fitToWidth(model.columnName(columnId), model.columnWidth(columnId))
      fit.style(Style(foregroundColour = BasicColour.Yellow))
    }
    val buffer = ArrayBuffer[StyledCharacter]()
    buffer ++= doubleVertical.style.chars
    if (showMarkedRows)
      buffer ++= (" " + singleVertical).style.chars
    buffer ++= StyledString.mkString(model.columnIds.map(renderColumn), singleVertical.style).chars
    buffer ++= doubleVertical.style.chars
    Line(StyledString(buffer))
  }

  private def renderHeaderLines(moreDataItemsAboveWindow: Boolean): Seq[Line] =
    Seq(
      renderTopRow(model),
      renderHeaderRow(model),
      renderBelowHeaderRow(model, moreDataItemsAboveWindow))

  private def renderDataLines(rowOffset: Int, rowCount: Int): Seq[Line] =
    for ((row, rowIndex) ← model.rows.zipWithIndex.window(rowOffset, rowCount))
      yield renderRow(row, rowIndex = rowIndex)

  private def renderRow(row: TwoDTableModel.Row,
                        rowIndex: Int): Line = {
    val isCursorRow = currentRowIndexOpt contains rowIndex
    val shouldHighlightRow = isCursorRow && currentColumnIndexOpt.isEmpty

    val markCell: StyledString =
      if (showMarkedRows) {
        val isMarked = markedRowsOpt exists (_ contains rowIndex)
        val markChar = if (isMarked) "◈" else " "
        (markChar + singleVertical).style(Style(inverse = shouldHighlightRow))
      } else
        StyledString.empty

    val renderedCells = model.columnIds.zipWithIndex.map { case (columnId, columnIndex) ⇒
      val cellContents = row.renderedValue(columnId)
      renderCell(cellContents, Point(rowIndex, columnIndex), columnId)
    }
    val internalVertical = singleVertical.style(Style(inverse = shouldHighlightRow))
    val innerChars = StyledString.mkString(renderedCells, internalVertical)
    val tableSide = doubleVertical.style
    Line(tableSide + markCell + innerChars + tableSide)
  }

  def renderCell(cellContents: String, cellLocation: Point, columnId: ColumnId): StyledString = {
    val searchInfoOpt = searchStateOpt.flatMap(_.getCellSearchInfo(cellLocation))
    val isCursorRow = currentRowIndexOpt contains cellLocation.row
    val highlightCell = isCursorRow && currentColumnIndexOpt.forall(_ == cellLocation.column)
    val fitCellContents = StringUtils.fitToWidth(cellContents, model.columnWidth(columnId))
    val renderedChars =
      for ((c, offset) <- fitCellContents.zipWithIndex)
        yield {
          val isSearchMatch = searchInfoOpt exists (_ isMatched offset)
          val isLabel = columnId == TwoDTableModelCreator.RowLabelColumnId
          val style = getCellCharacterStyle(highlightCell, isSearchMatch, isLabel)
          StyledCharacter(c, style)
        }
    StyledString(renderedChars)
  }

  private def getCellCharacterStyle(highlightCell: Boolean, isSearchMatch: Boolean, isLabel: Boolean): Style = {
    val foregroundColour =
      if (isLabel) BasicColour.Yellow
      else if (isSearchMatch) BasicColour.Cyan
      else DefaultColour
    Style(inverse = highlightCell, bold = isSearchMatch, foregroundColour = foregroundColour)
  }

  def renderTopRow(model: TwoDTableModel): Line =
    renderBorderRow(model, doubleTopLeft, doubleHorizontal, doubleHorizontalSingleDown, doubleTopRight)

  def renderBelowHeaderRow(model: TwoDTableModel, addArrow: Boolean): Line =
    renderBorderRow(model, doubleVerticalSingleRight, singleHorizontal, singleIntersect, doubleVerticalSingleLeft, upArrow = addArrow)

  def renderFooterLine(model: TwoDTableModel, addArrow: Boolean): Line =
    renderBorderRow(model, doubleBottomLeft, doubleHorizontal, doubleHorizontalSingleUp, doubleBottomRight, downArrow = addArrow)

  private def renderBorderRow(model: TwoDTableModel,
                              first: String,
                              internal: String,
                              internalColumn: String,
                              last: String,
                              downArrow: Boolean = false,
                              upArrow: Boolean = false): Line = {
    val sb = new StringBuilder
    sb.append(first)
    if (showMarkedRows)
      sb.append(internal + internalColumn)
    sb.append(model.columnIds.map(columnId ⇒ internal * model.columnWidth(columnId)).mkString(internalColumn))
    sb.append(last)
    val chars = sb.toString.when(downArrow, addDownArrow).when(upArrow, addUpArrow)
    Line(chars.style)
  }

  private def showMarkedRows = markedRowsOpt.isDefined

}