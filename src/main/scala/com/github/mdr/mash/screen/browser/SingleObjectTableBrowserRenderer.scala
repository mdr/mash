package com.github.mdr.mash.screen.browser

import com.github.mdr.mash.printer.{ SingleObjectTableStringifier, UnicodeBoxCharacterSupplier }
import com.github.mdr.mash.repl.browser.SingleObjectTableBrowserState
import com.github.mdr.mash.screen.Style.StylableString
import com.github.mdr.mash.screen.{ Colour, KeyHint, _ }
import com.github.mdr.mash.terminal.TerminalInfo
import com.github.mdr.mash.utils.StringUtils
import com.github.mdr.mash.utils.Utils._

import scala.collection.mutable.ArrayBuffer

class SingleObjectTableBrowserRenderer(state: SingleObjectTableBrowserState, terminalInfo: TerminalInfo)
  extends AbstractBrowserRenderer(state, terminalInfo) {

  private val boxCharacterSupplier = UnicodeBoxCharacterSupplier
  private val objectStringifier = new SingleObjectTableStringifier(terminalInfo)

  private val model = state.model

  protected def renderLines: Seq[Line] = {
    val upperStatusLine = renderUpperStatusLine
    val headerLines = renderHeaderLines
    val dataLines = renderDataLines
    val footerLine = renderFooterLine
    val statusLine = renderStatusLine
    Seq(upperStatusLine) ++ headerLines ++ dataLines ++ Seq(footerLine, statusLine)
  }

  private def renderBelowHeaderRow: String = {
    import boxCharacterSupplier._
    val sb = new StringBuilder()
    sb.append(doubleVerticalSingleRight)
    sb.append(singleHorizontal * model.fieldColumnWidth)
    sb.append(singleHorizontalSingleDown)
    sb.append(singleHorizontal * model.valueColumnWidth)
    sb.append(doubleVerticalSingleLeft)
    sb.toString
  }

  private def renderHeaderLines: Seq[Line] = model.classNameOpt match {
    case Some(className) ⇒
      val topLine = Line(objectStringifier.renderTopRow(model, break = false).style)
      val classNameLine = renderClassNameLine(className)
      val belowHeaderRow = renderBelowHeaderRow
      val moreDataItemsAboveWindow = state.firstRow > 0
      val belowHeaderLine = Line(belowHeaderRow.when(moreDataItemsAboveWindow, addUpArrow).style)
      Seq(topLine, classNameLine, belowHeaderLine)
    case None            ⇒
      val topLineChars = objectStringifier.renderTopRow(model)
      val moreDataItemsAboveWindow = state.firstRow > 0
      val newTopLineChars = topLineChars.when(moreDataItemsAboveWindow, addUpArrow)
      Seq(Line(newTopLineChars.style))
  }

  private def fullRowWidth = model.fieldColumnWidth + model.valueColumnWidth + 1

  def renderClassNameLine(className: String): Line = {
    def renderColumn(name: String): Seq[StyledCharacter] = {
      val fit = StringUtils.ellipsisise(StringUtils.centre(name, fullRowWidth), fullRowWidth)
      fit.style(Style(bold = true, foregroundColour = Colour.Yellow))
    }
    val buffer = ArrayBuffer[StyledCharacter]()
    buffer ++= boxCharacterSupplier.doubleVertical.style
    buffer ++= renderColumn(className)
    buffer ++= boxCharacterSupplier.doubleVertical.style
    val classNameLine = Line(buffer)
    classNameLine
  }

  private def renderDataLines: Seq[Line] = {
    val objects = model.fields.drop(state.firstRow).take(windowSize)
    for {
      ((renderedField, renderedValue), i) ← objects.zipWithIndex.toSeq
      actualIndex = i + state.firstRow
    } yield renderRow(renderedField, renderedValue, actualIndex == state.selectedRow)
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

  private def renderFooterLine = {
    val bottomRow = objectStringifier.renderBottomRow(model)
    val lastVisibleRow = state.firstRow + windowSize - 1
    val lastRow = state.size - 1
    val moreDataItemsBelowWindow = lastVisibleRow < lastRow
    val newBottom = bottomRow.when(moreDataItemsBelowWindow, addDownArrow)
    Line(newBottom.style)
  }

  private def renderStatusLine = {
    import KeyHint._
    Line(renderKeyHints(Seq(Exit, Focus, Back, Insert, InsertWhole, Tree)))
  }

  private def addUpArrow(s: String): String = setMiddleCharacter(s, '↑')
  private def addDownArrow(s: String): String = setMiddleCharacter(s, '↓')

  private def setMiddleCharacter(s: String, c: Char): String =
    if (s.isEmpty) s else s.updated(s.size / 2, c)

  override protected val windowSize: Int = state.windowSize(terminalInfo.rows)
}
