package com.github.mdr.mash.printer

import com.github.mdr.mash.printer.model.TwoDTableModel
import com.github.mdr.mash.printer.model.TwoDTableModel.Row
import com.github.mdr.mash.screen.BasicColour
import com.github.mdr.mash.screen.Screen._
import com.github.mdr.mash.utils.StringUtils
import com.github.mdr.mash.screen.Style.StylableString

class TwoDTableStringifier(showSelections: Boolean = false) {

  private val boxCharacterSupplier: BoxCharacterSupplier = UnicodeBoxCharacterSupplier
  import boxCharacterSupplier._

  def renderTopRow(model: TwoDTableModel): String = {
    val sb = new StringBuilder()
    sb.append(doubleTopLeft)
    if (showSelections)
      sb.append(doubleHorizontal + doubleHorizontalSingleDown)
    sb.append(model.columnIds.map(columnId ⇒ doubleHorizontal * model.columnWidth(columnId)).mkString(doubleHorizontalSingleDown))
    sb.append(doubleTopRight)
    sb.toString
  }

  def renderHeaderRow(model: TwoDTableModel): String = {
    def renderColumn(id: ColumnId) = {
      val fit = StringUtils.fitToWidth(model.columnName(id), model.columnWidth(id))
      drawStyledChars(fit.style(foregroundColour = BasicColour.Yellow, bold = true))
    }
    val sb = new StringBuilder()
    sb.append(doubleVertical)
    if (showSelections)
      sb.append(" " + singleVertical)
    sb.append(model.columnIds.map(renderColumn).mkString(singleVertical))
    sb.append(doubleVertical)
    sb.toString
  }

  def renderBelowHeaderRow(model: TwoDTableModel): String = {
    val sb = new StringBuilder()
    sb.append(doubleVerticalSingleRight)
    if (showSelections)
      sb.append(singleHorizontal + singleIntersect)
    sb.append(model.columnIds.map(columnId ⇒ singleHorizontal * model.columnWidth(columnId)).mkString(singleIntersect))
    sb.append(doubleVerticalSingleLeft)
    sb.toString
  }

  def renderObjectRow(model: TwoDTableModel, row: Row): String = {
    def renderCell(columnId: ColumnId) = StringUtils.fitToWidth(row.renderedValue(columnId), model.columnWidth(columnId))
    new StringBuilder()
      .append(doubleVertical)
      .append(model.columnIds.map(renderCell).mkString(singleVertical))
      .append(doubleVertical)
      .toString
  }

  def renderBottomRow(model: TwoDTableModel): String = {
    val sb = new StringBuilder
    sb.append(doubleBottomLeft)
    if (showSelections)
      sb.append(doubleHorizontal + doubleHorizontalSingleUp)
    sb.append(model.columnIds.map(columnId ⇒ doubleHorizontal * model.columnWidth(columnId)).mkString(doubleHorizontalSingleUp))
    sb.append(doubleBottomRight)
    sb.toString
  }

}