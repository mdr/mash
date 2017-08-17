package com.github.mdr.mash.render.browser

import com.github.mdr.mash.printer.model.TwoDTableModel
import com.github.mdr.mash.printer.model.TwoDTableModel.RowLabelColumnId
import com.github.mdr.mash.printer.{ ColumnId, UnicodeBoxCharacterSupplier }
import com.github.mdr.mash.repl.browser.SearchState
import com.github.mdr.mash.screen.Style.StylableString
import com.github.mdr.mash.screen._
import com.github.mdr.mash.render.browser.ArrowHelper._
import com.github.mdr.mash.render.browser.TwoDTableCommonRenderer.ExtraRenderingInfo
import com.github.mdr.mash.utils.Utils._
import com.github.mdr.mash.utils.{ Dimensions, Point, StringUtils, StyledStringUtils }

import scala.collection.mutable.ArrayBuffer

object TwoDTableCommonRenderer {

  case class ExtraRenderingInfo(markedRowsOpt: Option[Set[Int]] = None,
                                currentRowIndexOpt: Option[Int] = None,
                                currentColumnIndexOpt: Option[Int] = None,
                                searchStateOpt: Option[SearchState] = None)

}

class TwoDTableCommonRenderer(model: TwoDTableModel,
                              terminalSize: Dimensions,
                              extraInfo: ExtraRenderingInfo = ExtraRenderingInfo()) {

  import extraInfo._

  import UnicodeBoxCharacterSupplier._

  def renderAllTableLines = renderTableLines(0, model.numberOfRows)

  def renderTableLines(rowOffset: Int, rowCount: Int): Seq[Line] = {
    val headerLines = renderHeaderLines(moreDataItemsAboveWindow = rowOffset > 0)

    val dataLines = renderDataLines(rowOffset, rowCount)

    val moreDataItemsBelowWindow = rowOffset + rowCount < model.numberOfRows
    val footerLine = renderFooterLine(addArrow = moreDataItemsBelowWindow)

    headerLines ++ dataLines ++ Seq(footerLine)
  }

  private def renderHeaderLines(moreDataItemsAboveWindow: Boolean): Seq[Line] =
    Seq(
      renderTopRow,
      renderHeaderRow,
      renderBelowHeaderRow(moreDataItemsAboveWindow))

  def renderTopRow: Line =
    renderBorderRow(doubleTopLeft, doubleHorizontal, doubleHorizontalSingleDown, doubleTopRight)

  def renderBelowHeaderRow(addArrow: Boolean): Line =
    renderBorderRow(doubleVerticalSingleRight, singleHorizontal, singleIntersect, doubleVerticalSingleLeft, upArrow = addArrow)

  def renderFooterLine(addArrow: Boolean): Line =
    renderBorderRow(doubleBottomLeft, doubleHorizontal, doubleHorizontalSingleUp, doubleBottomRight, downArrow = addArrow)

  private def renderBorderRow(first: String,
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
    val lineString = sb.toString.when(downArrow, addDownArrow).when(upArrow, addUpArrow).style
    Line(truncateIfRequired(lineString, internal.style.head))
  }

  private def truncateIfRequired(s: StyledString, ellipsis: StyledCharacter = StyledStringUtils.Ellipsis): StyledString = {
    val columns = terminalSize.columns
    if (s.size >= columns)
      if (columns >= 3)
        s.head.asString + StyledStringUtils.ellipsisise(StyledString(s.chars.tail.init), columns - 2, ellipsis) + s.last.asString
      else
        StyledStringUtils.ellipsisise(s, columns, ellipsis)
    else
      s
  }

  private def renderHeaderRow: Line = {
    def renderColumn(columnId: ColumnId): StyledString = {
      val fit = StringUtils.fitToWidth(model.columnName(columnId), model.columnWidth(columnId))
      fit.style(getStyle(isLabel = true))
    }
    val buffer = ArrayBuffer[StyledCharacter]()
    buffer ++= doubleVertical.style.chars
    if (showMarkedRows)
      buffer ++= (" " + singleVertical).style.chars
    buffer ++= StyledString.join(model.columnIds.map(renderColumn), singleVertical.style).chars
    buffer ++= doubleVertical.style.chars
    val lineString = StyledString(buffer)
    Line(truncateIfRequired(lineString))
  }

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
        (markChar + singleVertical).style(getStyle(highlight = shouldHighlightRow))
      } else
        StyledString.Empty

    val renderedCells = model.columnIds.zipWithIndex.map {
      case (columnId, columnIndex) ⇒
        val cellContents = row.renderedValue(columnId)
        renderCell(cellContents, Point(rowIndex, columnIndex), columnId)
    }
    val internalVertical = singleVertical.style(getStyle(highlight = shouldHighlightRow))
    val innerChars = StyledString.join(renderedCells, internalVertical)
    val tableSide = doubleVertical.style
    val lineString = tableSide + markCell + innerChars + tableSide
    Line(truncateIfRequired(lineString))
  }

  def renderCell(cellContents: String, cellLocation: Point, columnId: ColumnId): StyledString = {
    val searchInfoOpt = searchStateOpt.flatMap(_.getCellSearchInfo(cellLocation))
    val isCursorRow = currentRowIndexOpt contains cellLocation.row
    val highlightCell = isCursorRow && currentColumnIndexOpt.forall(_ == cellLocation.column)
    val fitCellContents = StringUtils.fitToWidth(cellContents, model.columnWidth(columnId))
    val renderedChars =
      for ((c, offset) ← fitCellContents.zipWithIndex)
        yield {
          val isSearchMatch = searchInfoOpt exists (_ isMatched offset)
          val isLabel = columnId == RowLabelColumnId
          val style = getStyle(highlight = highlightCell, isSearchMatch = isSearchMatch, isLabel = isLabel)
          StyledCharacter(c, style)
        }
    StyledString(renderedChars)
  }

  private def getStyle(highlight: Boolean = false, isSearchMatch: Boolean = false, isLabel: Boolean = false): Style = {
    val foregroundColour =
      if (isSearchMatch) DefaultColours.Cyan
      else if (isLabel) DefaultColours.Yellow
      else DefaultColour
    Style(inverse = highlight, bold = isSearchMatch, foregroundColour = foregroundColour)
  }

  private def showMarkedRows = markedRowsOpt.isDefined

}