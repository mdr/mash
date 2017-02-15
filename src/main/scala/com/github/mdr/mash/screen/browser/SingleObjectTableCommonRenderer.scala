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

  def renderTableLines(fieldValuePairs: Seq[(String, String)],
                       selectedIndexOpt: Option[Int] = None,
                       moreDataItemsAboveWindow: Boolean = false,
                       moreDataItemsBelowWindow: Boolean = false): Seq[Line] =
    renderHeaderLines(moreDataItemsAboveWindow) ++
      renderFieldLines(fieldValuePairs, selectedIndexOpt) ++
      Seq(renderFooterLine(break = hasFields, addArrow = moreDataItemsBelowWindow))

  private def renderHeaderLines(moreDataItemsAboveWindow: Boolean): Seq[Line] =
    model.classNameOpt match {
      case Some(className) ⇒ renderClassHeaderLines(className, moreDataItemsAboveWindow)
      case None            ⇒ Seq(renderTopRow(break = hasFields, addArrow = moreDataItemsAboveWindow))
    }

  private def renderClassHeaderLines(className: String, moreDataItemsAboveWindow: Boolean): Seq[Line] = {
    val topLine = renderTopRow(break = false, addArrow = false)
    val classNameLine = renderClassNameLine(className)
    val belowHeaderLineOpt =
      if (model.fields.isEmpty)
        None
      else
        Some(renderBelowHeaderLine(addArrow = moreDataItemsAboveWindow))
    Seq(topLine, classNameLine) ++ belowHeaderLineOpt
  }

  /**
    * ╔══════════════════════════╗
    * or
    * ╔════════════╤═════════════╗
    */
  private def renderTopRow(break: Boolean = true, addArrow: Boolean): Line = {
    import boxCharacterSupplier._
    val chars = new StringBuilder()
      .append(doubleTopLeft)
      .append(doubleHorizontal * model.fieldColumnWidth)
      .append(if (break) doubleHorizontalSingleDown else doubleHorizontal)
      .append(doubleHorizontal * model.valueColumnWidth)
      .append(doubleTopRight)
      .toString
    Line(chars.when(addArrow, addUpArrow).style)
  }

  /**
    * ║           Class          ║
    **/
  private def renderClassNameLine(className: String): Line = {
    val fit = StringUtils.ellipsisise(StringUtils.centre(className, fullRowWidth), fullRowWidth)
    val renderedClassName = fit.style(classNameStyle)
    val buffer = ArrayBuffer[StyledCharacter]()
    buffer ++= boxCharacterSupplier.doubleVertical.style.chars
    buffer ++= renderedClassName.chars
    buffer ++= boxCharacterSupplier.doubleVertical.style.chars
    Line(StyledString(buffer))
  }

  /**
    * ╟────────────┬─────────────╢
    */
  private def renderBelowHeaderLine(addArrow: Boolean): Line = {
    import boxCharacterSupplier._
    val chars = new StringBuilder()
      .append(doubleVerticalSingleRight)
      .append(singleHorizontal * model.fieldColumnWidth)
      .append(singleHorizontalSingleDown)
      .append(singleHorizontal * model.valueColumnWidth)
      .append(doubleVerticalSingleLeft)
      .toString
    Line(chars.when(addArrow, addUpArrow).style)
  }

  private def renderFieldLines(fieldValuePairs: Seq[(String, String)], selectedIndexOpt: Option[Int]): Seq[Line] =
    for {
      ((renderedField, renderedValue), i) ← fieldValuePairs.zipWithIndex
      isCursorRow = selectedIndexOpt contains i
    } yield renderFieldLine(renderedField, renderedValue, isCursorRow)

  /**
    * ║fieldName   │fieldValue   ║
    */
  private def renderFieldLine(renderedField: String, renderedValue: String, isCursorRow: Boolean): Line = {
    val side = boxCharacterSupplier.doubleVertical.style
    val internalVertical = boxCharacterSupplier.singleVertical.style(internalRowStyle(isCursorRow))
    val fieldChars = StringUtils.fitToWidth(renderedField, model.fieldColumnWidth).style(fieldStyle(isCursorRow))
    val valueChars = StringUtils.fitToWidth(renderedValue, model.valueColumnWidth).style(internalRowStyle(isCursorRow))
    Line(side + fieldChars + internalVertical + valueChars + side)
  }

  /**
    * ╚════════════╧═════════════╝
    * or
    * ╚══════════════════════════╝
    */
  private def renderFooterLine(break: Boolean, addArrow: Boolean): Line = {
    import boxCharacterSupplier._
    val chars = new StringBuilder()
      .append(doubleBottomLeft)
      .append(doubleHorizontal * model.fieldColumnWidth)
      .append(if (break) doubleHorizontalSingleUp else doubleHorizontal)
      .append(doubleHorizontal * model.valueColumnWidth)
      .append(doubleBottomRight)
      .toString
    Line(chars.when(addArrow, addDownArrow).style)
  }

  private def addUpArrow(s: String): String = setMiddleCharacter(s, '↑')

  private def addDownArrow(s: String): String = setMiddleCharacter(s, '↓')

  private def setMiddleCharacter(s: String, c: Char): String =
    if (s.isEmpty) s else s.updated(s.length / 2, c)

  private def fullRowWidth = model.fieldColumnWidth + model.valueColumnWidth + 1

  private def hasFields: Boolean = model.fields.nonEmpty

  private val classNameStyle: Style = Style(bold = true, foregroundColour = Colour.Yellow)

  private def internalRowStyle(isCursorRow: Boolean): Style = Style(inverse = isCursorRow)

  private def fieldStyle(isCursorRow: Boolean): Style = Style(inverse = isCursorRow, foregroundColour = Colour.Yellow)

}
