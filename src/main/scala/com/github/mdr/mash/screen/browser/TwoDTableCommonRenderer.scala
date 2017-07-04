package com.github.mdr.mash.screen.browser

import com.github.mdr.mash.printer.model.{ TwoDTableModel, TwoDTableModelCreator }
import com.github.mdr.mash.printer.{ ColumnId, UnicodeBoxCharacterSupplier }
import com.github.mdr.mash.repl.browser.TwoDTableBrowserState.SearchState
import com.github.mdr.mash.screen.Style.StylableString
import com.github.mdr.mash.screen._
import com.github.mdr.mash.utils.StringUtils
import com.github.mdr.mash.utils.Utils._

import scala.collection.mutable.ArrayBuffer

class TwoDTableCommonRenderer(model: TwoDTableModel,
                              markedRowsOpt: Option[Set[Int]] = None,
                              currentRowIndexOpt: Option[Int] = None,
                              currentColumnIndexOpt: Option[Int] = None,
                              searchStateOpt: Option[SearchState] = None) {

  private val boxChars = UnicodeBoxCharacterSupplier

  def renderTableLines(rowOffset: Int = 0, rowCount: Int = model.numberOfRows): Seq[Line] = {
    val headerLines = renderHeaderLines
    val dataLines = renderDataLines(rowOffset: Int, rowCount: Int)
    val footerLine = renderFooterLine(model)
    headerLines ++ dataLines ++ Seq(footerLine)
  }

  private def renderHeaderRow(model: TwoDTableModel): Line = {
    def renderColumn(columnId: ColumnId): StyledString = {
      val fit = StringUtils.fitToWidth(model.columnName(columnId), model.columnWidth(columnId))
      fit.style(Style(foregroundColour = BasicColour.Yellow))
    }
    val buffer = ArrayBuffer[StyledCharacter]()
    buffer ++= boxChars.doubleVertical.style.chars
    if (showMarkedRows)
      buffer ++= (" " + boxChars.singleVertical).style.chars
    buffer ++= StyledString.mkString(model.columnIds.map(renderColumn), boxChars.singleVertical.style).chars
    buffer ++= boxChars.doubleVertical.style.chars
    Line(StyledString(buffer))
  }

  private def renderHeaderLines: Seq[Line] =
    Seq(
      renderTopRow(model),
      renderHeaderRow(model),
      renderBelowHeaderRow(model))

  private def renderDataLines(rowOffset: Int, rowCount: Int): Seq[Line] =
    for ((row, rowIndex) ← model.rows.zipWithIndex.window(rowOffset, rowCount))
      yield renderRow(row, rowIndex = rowIndex)

  private def renderRow(row: TwoDTableModel.Row,
                        rowIndex: Int): Line = {
    val isCursorRow = currentRowIndexOpt contains rowIndex
    val shouldHighlightRow = isCursorRow && currentColumnIndexOpt.isEmpty

    val isMarked = markedRowsOpt exists (_ contains rowIndex)
    val markChar = if (isMarked) "◈" else " "
    val markCell: StyledString =
      if (showMarkedRows)
        (markChar + boxChars.singleVertical).style(Style(inverse = shouldHighlightRow))
      else
        StyledString.empty

    val renderedCells = model.columnIds.zipWithIndex.map { case (columnId, columnIndex) ⇒
      val cellContents = row.renderedValue(columnId)
      renderCell(cellContents, rowIndex, columnIndex, columnId)
    }
    val internalVertical = boxChars.singleVertical.style(Style(inverse = shouldHighlightRow))
    val innerChars = StyledString.mkString(renderedCells, internalVertical)
    val tableSide = boxChars.doubleVertical.style
    Line(tableSide + markCell + innerChars + tableSide)
  }

  def renderCell(cellContents: String, rowIndex: Int, columnIndex: Int, columnId: ColumnId): StyledString = {
    val searchInfoOpt = searchStateOpt.flatMap(_.byPoint.get(Point(rowIndex, columnIndex)))
    val isCursorRow = currentRowIndexOpt contains rowIndex
    val highlightCell = isCursorRow && currentColumnIndexOpt.forall(_ == columnIndex)
    val fitCellContents = StringUtils.fitToWidth(cellContents, model.columnWidth(columnId))
    val renderedChars =
      for ((c, offset) <- fitCellContents.zipWithIndex)
        yield {
          val isSearchMatch = searchInfoOpt exists (_.matches exists (_ contains offset))
          val bold = isSearchMatch
          val foregroundColour =
            if (columnId == TwoDTableModelCreator.RowLabelColumnId) BasicColour.Yellow
            else if (isSearchMatch) BasicColour.Cyan
            else DefaultColour
          val style = Style(inverse = highlightCell, bold = bold, foregroundColour = foregroundColour)
          StyledCharacter(c, style)
        }
    StyledString(renderedChars)
  }

  import UnicodeBoxCharacterSupplier._

  def renderTopRow(model: TwoDTableModel): Line =
    renderBorderRow(model, doubleTopLeft, doubleHorizontal, doubleHorizontalSingleDown, doubleTopRight)

  def renderBelowHeaderRow(model: TwoDTableModel): Line =
    renderBorderRow(model, doubleVerticalSingleRight, singleHorizontal, singleIntersect, doubleVerticalSingleLeft)

  def renderFooterLine(model: TwoDTableModel): Line =
    renderBorderRow(model, doubleBottomLeft, doubleHorizontal, doubleHorizontalSingleUp, doubleBottomRight)

  private def renderBorderRow(model: TwoDTableModel,
                              first: String,
                              internal: String,
                              internalColumn: String,
                              last: String): Line = {
    val sb = new StringBuilder
    sb.append(first)
    if (showMarkedRows)
      sb.append(internal + internalColumn)
    sb.append(model.columnIds.map(columnId ⇒ internal * model.columnWidth(columnId)).mkString(internalColumn))
    sb.append(last)
    Line(sb.toString.style)
  }

  private def showMarkedRows = markedRowsOpt.isDefined

}