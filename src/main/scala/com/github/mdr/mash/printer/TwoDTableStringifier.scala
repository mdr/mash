package com.github.mdr.mash.printer

import com.github.mdr.mash.printer.model.TwoDTableModel

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

  def renderBelowHeaderRow(model: TwoDTableModel): String = {
    val sb = new StringBuilder()
    sb.append(doubleVerticalSingleRight)
    if (showSelections)
      sb.append(singleHorizontal + singleIntersect)
    sb.append(model.columnIds.map(columnId ⇒ singleHorizontal * model.columnWidth(columnId)).mkString(singleIntersect))
    sb.append(doubleVerticalSingleLeft)
    sb.toString
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