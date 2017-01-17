package com.github.mdr.mash.screen.browser

import com.github.mdr.mash.printer.UnicodeBoxCharacterSupplier
import com.github.mdr.mash.printer.model.SingleObjectTableModel
import com.github.mdr.mash.screen.Style.StylableString
import com.github.mdr.mash.screen.{ Colour, _ }
import com.github.mdr.mash.terminal.TerminalInfo
import com.github.mdr.mash.utils.StringUtils
import com.github.mdr.mash.utils.Utils._

import scala.collection.mutable.ArrayBuffer

class SingleObjectTableCommonRenderer(model: SingleObjectTableModel, terminalInfo: TerminalInfo) {
  private val boxCharacterSupplier = UnicodeBoxCharacterSupplier

  import boxCharacterSupplier._

  private def fullRowWidth = model.fieldColumnWidth + model.valueColumnWidth + 1

  private def renderClassNameLine(className: String): Line = {
    val fit = StringUtils.ellipsisise(StringUtils.centre(className, fullRowWidth), fullRowWidth)
    val renderedClassName = fit.style(Style(bold = true, foregroundColour = Colour.Yellow))
    val buffer = ArrayBuffer[StyledCharacter]()
    buffer ++= boxCharacterSupplier.doubleVertical.style
    buffer ++= renderedClassName
    buffer ++= boxCharacterSupplier.doubleVertical.style
    Line(buffer)
  }

  private def renderHeaderLines(moreDataItemsAboveWindow: Boolean): Seq[Line] = {
    model.classNameOpt match {
      case Some(className) ⇒
        val topLine = Line(renderTopRow(model, break = false).style)
        val classNameLine = renderClassNameLine(className)
        val belowHeaderChars = renderBelowHeaderChars
        val belowHeaderLine = Line(belowHeaderChars.when(moreDataItemsAboveWindow, addUpArrow).style)
        Seq(topLine, classNameLine, belowHeaderLine)
      case None            ⇒
        val topLineChars = renderTopRow(model)
        val newTopLineChars = topLineChars.when(moreDataItemsAboveWindow, addUpArrow)
        Seq(Line(newTopLineChars.style))
    }
  }

  private def addUpArrow(s: String): String = setMiddleCharacter(s, '↑')

  private def addDownArrow(s: String): String = setMiddleCharacter(s, '↓')

  private def setMiddleCharacter(s: String, c: Char): String =
    if (s.isEmpty) s else s.updated(s.length / 2, c)

  private def renderFooterLine(moreDataItemsBelowWindow: Boolean) = {
    val bottomRow = renderBottomRow(model)
    val newBottom = bottomRow.when(moreDataItemsBelowWindow, addDownArrow)
    Line(newBottom.style)
  }

  private def renderRow(renderedField: String, renderedValue: String, isCursorRow: Boolean): Line = {
    val side = boxCharacterSupplier.doubleVertical.style
    val internalVertical = boxCharacterSupplier.singleVertical.style(Style(inverse = isCursorRow))
    val fieldStyle = Style(inverse = isCursorRow, foregroundColour = Colour.Yellow)
    val valueStyle = Style(inverse = isCursorRow)
    val fieldChars = StringUtils.fitToWidth(renderedField, model.fieldColumnWidth).style(fieldStyle)
    val valueChars = StringUtils.fitToWidth(renderedValue, model.valueColumnWidth).style(valueStyle)
    Line(side ++ fieldChars ++ internalVertical ++ valueChars ++ side)
  }

  private def renderDataLines(fieldValuePairs: Seq[(String, String)], selectedIndexOpt: Option[Int]): Seq[Line] =
    for {
      ((renderedField, renderedValue), i) ← fieldValuePairs.zipWithIndex
    } yield renderRow(renderedField, renderedValue, isCursorRow = selectedIndexOpt contains i)

  def renderTableLines(fieldValuePairs: Seq[(String, String)],
                       selectedIndexOpt: Option[Int] = None,
                       moreDataItemsAboveWindow: Boolean = false,
                       moreDataItemsBelowWindow: Boolean = false): Seq[Line] =
    renderHeaderLines(moreDataItemsAboveWindow) ++
      renderDataLines(fieldValuePairs, selectedIndexOpt) ++
      Seq(renderFooterLine(moreDataItemsBelowWindow))


  /** ╔══════════════════════════╗
    * or
    * ╔════════════╤═════════════╗
    */
  private def renderTopRow(model: SingleObjectTableModel, break: Boolean = true): String =
    new StringBuilder()
      .append(doubleTopLeft)
      .append(doubleHorizontal * model.fieldColumnWidth)
      .append(if (break) doubleHorizontalSingleDown else doubleHorizontal)
      .append(doubleHorizontal * model.valueColumnWidth)
      .append(doubleTopRight)
      .toString

  /**
    * ║type        │file         ║
    */
  private def renderFieldRow(renderedField: String, renderedValue: String): String =
    new StringBuilder()
      .append(doubleVertical)
      .append(renderedField)
      .append(singleVertical)
      .append(renderedValue)
      .append(doubleVertical)
      .toString

  /**
    * ╚════════════╧═════════════╝
    */
  private def renderBottomRow(model: SingleObjectTableModel): String =
    new StringBuilder()
      .append(doubleBottomLeft)
      .append(doubleHorizontal * model.fieldColumnWidth)
      .append(doubleHorizontalSingleUp)
      .append(doubleHorizontal * model.valueColumnWidth)
      .append(doubleBottomRight)
      .toString

  /**
    * ╟────────────┬─────────────╢
    */
  private def renderBelowHeaderChars: String = {
    val sb = new StringBuilder()
    sb.append(doubleVerticalSingleRight)
    sb.append(singleHorizontal * model.fieldColumnWidth)
    sb.append(singleHorizontalSingleDown)
    sb.append(singleHorizontal * model.valueColumnWidth)
    sb.append(doubleVerticalSingleLeft)
    sb.toString
  }

}
