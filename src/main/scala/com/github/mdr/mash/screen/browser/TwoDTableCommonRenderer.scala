package com.github.mdr.mash.screen.browser

import com.github.mdr.mash.printer.model.TwoDTableModel.Row
import com.github.mdr.mash.printer.model.{ TwoDTableModel, TwoDTableModelCreator }
import com.github.mdr.mash.printer.{ ColumnId, TwoDTableStringifier, UnicodeBoxCharacterSupplier }
import com.github.mdr.mash.repl.browser.TwoDTableBrowserState.SearchState
import com.github.mdr.mash.screen.Style.StylableString
import com.github.mdr.mash.screen._
import com.github.mdr.mash.utils.StringUtils
import com.github.mdr.mash.utils.Utils.tupled

import scala.collection.mutable.ArrayBuffer

class TwoDTableCommonRenderer(model: TwoDTableModel, showSelections: Boolean) {

  private val boxCharacterSupplier = UnicodeBoxCharacterSupplier
  private val objectTableStringifier = new TwoDTableStringifier(showSelections = showSelections)

  def renderAllTableLines: Seq[Line] = renderTableLines(model.rows)

  def renderTableLines(rows: Seq[Row],
                       selectedIndexOpt: Option[Int] = None,
                       currentColumnIndexOpt: Option[Int] = None,
                       markedRows: Set[Int] = Set(),
                       searchStateOpt: Option[SearchState] = None): Seq[Line] = {
    val headerLines = renderHeaderLines
    val dataLines = renderDataLines(rows, selectedIndexOpt, currentColumnIndexOpt, markedRows, searchStateOpt)
    val footerLine = renderFooterLine
    headerLines ++ dataLines ++ Seq(footerLine)
  }

  private def renderHeaderRow(model: TwoDTableModel): Line = {
    def renderColumn(columnId: ColumnId): StyledString = {
      val fit = StringUtils.fitToWidth(model.columnName(columnId), model.columnWidth(columnId))
      fit.style(Style(foregroundColour = BasicColour.Yellow))
    }
    val buffer = ArrayBuffer[StyledCharacter]()
    buffer ++= boxCharacterSupplier.doubleVertical.style.chars
    if (showSelections)
      buffer ++= (" " + boxCharacterSupplier.singleVertical).style.chars
    buffer ++= StyledString.mkString(model.columnIds.map(renderColumn), boxCharacterSupplier.singleVertical.style).chars
    buffer ++= boxCharacterSupplier.doubleVertical.style.chars
    Line(StyledString(buffer))
  }

  private def renderHeaderLines: Seq[Line] =
    Seq(
      Line(objectTableStringifier.renderTopRow(model).style),
      renderHeaderRow(model),
      Line(objectTableStringifier.renderBelowHeaderRow(model).style))

  private def renderDataLines(rows: Seq[Row],
                              selectedIndexOpt: Option[Int],
                              currentColumnIndexOpt: Option[Int],
                              markedRows: Set[Int],
                              searchStateOpt: Option[SearchState]): Seq[Line] = {
    for {
      (row, rowIndex) ← rows.zipWithIndex
    } yield renderRow(row,
      isCursorRow = selectedIndexOpt contains rowIndex,
      currentColumnIndexOpt,
      isSelected = markedRows contains rowIndex,
      rowIndex = rowIndex, searchStateOpt)
  }

  private def renderRow(row: TwoDTableModel.Row,
                        isCursorRow: Boolean,
                        currentColumnIndexOpt: Option[Int],
                        isSelected: Boolean,
                        rowIndex: Int,
                        searchStateOpt: Option[SearchState]): Line = {
    val side = boxCharacterSupplier.doubleVertical.style
    val selectedChar = if (isSelected) "◈" else " "
    val highlightRow = isCursorRow && currentColumnIndexOpt.isEmpty
    val selected =
      if (showSelections)
        (selectedChar + boxCharacterSupplier.singleVertical).style(Style(inverse = highlightRow))
      else
        StyledString.empty
    val internalVertical = boxCharacterSupplier.singleVertical.style(Style(inverse = highlightRow))

    def renderCell(columnId: ColumnId, columnIndex: Int): StyledString = {
      val searchInfoOpt = searchStateOpt.flatMap(_.byPoint.get(Point(rowIndex, columnIndex)))
      val highlightCell = isCursorRow && currentColumnIndexOpt.forall(_ == columnIndex)
      val cellContents = StringUtils.fitToWidth(row.renderedValue(columnId), model.columnWidth(columnId))
      val buf = ArrayBuffer[StyledCharacter]()
      for ((c, offset) <- cellContents.zipWithIndex) {
        val isSearchMatch = searchInfoOpt exists (_.matches exists (_ contains offset))
        val bold = isSearchMatch
        val foregroundColour =
          if (columnId == TwoDTableModelCreator.RowLabelColumnId) BasicColour.Yellow
          else if (isSearchMatch) BasicColour.Cyan
          else DefaultColour
        val style = Style(inverse = highlightCell, bold = bold, foregroundColour = foregroundColour)
        buf += StyledCharacter(c, style)
      }
      StyledString(buf)
    }

    val renderedCells = model.columnIds.zipWithIndex.map(tupled(renderCell))
    val innerChars = StyledString.mkString(renderedCells, internalVertical)
    Line(side + selected + innerChars + side)
  }

  private def renderFooterLine = Line(objectTableStringifier.renderBottomRow(model).style)

}