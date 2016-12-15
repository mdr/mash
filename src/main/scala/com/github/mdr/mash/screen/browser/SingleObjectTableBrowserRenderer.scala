package com.github.mdr.mash.screen.browser

import com.github.mdr.mash.printer.{ ObjectStringifier, UnicodeBoxCharacterSupplier }
import com.github.mdr.mash.repl.browser.SingleObjectTableBrowserState
import com.github.mdr.mash.screen.Style.StylableString
import com.github.mdr.mash.screen.{ Colour, KeyHint, _ }
import com.github.mdr.mash.terminal.TerminalInfo
import com.github.mdr.mash.utils.StringUtils
import com.github.mdr.mash.utils.Utils._

class SingleObjectTableBrowserRenderer(state: SingleObjectTableBrowserState, terminalInfo: TerminalInfo)
  extends AbstractBrowserRenderer(state, terminalInfo) {

  private val boxCharacterSupplier = UnicodeBoxCharacterSupplier
  private val objectStringifier = new ObjectStringifier(terminalInfo)

  private val model = state.model

  protected def renderLines: Seq[Line] = {
    val upperStatusLine = renderUpperStatusLine
    val headerLines = renderHeaderLines
    val dataLines = renderDataLines
    val footerLine = renderFooterLine
    val statusLine = renderStatusLine
    Seq(upperStatusLine) ++ headerLines ++ dataLines ++ Seq(footerLine, statusLine)
  }

  private def renderHeaderLines: Seq[Line] = {
    val topRow = objectStringifier.renderTopRow(model)
    val moreDataItemsAboveWindow = state.firstRow > 0
    val newTopRow = topRow.when(moreDataItemsAboveWindow, _.updated(topRow.size / 2, '↑'))
    Seq(newTopRow).map(s ⇒ Line(s.style))
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
    val highlightRow = isCursorRow
    val internalVertical = boxCharacterSupplier.singleVertical.style(Style(inverse = highlightRow))
    val fieldChars = StringUtils.fitToWidth(renderedField, model.fieldColumnWidth).style(Style(inverse = isCursorRow, foregroundColour = Colour.Yellow))
    val valueChars = StringUtils.fitToWidth(renderedValue, model.valueColumnWidth).style(Style(inverse = isCursorRow))
    Line(side ++ fieldChars ++ internalVertical ++ valueChars ++ side)
  }

  private def renderFooterLine = {
    val bottomRow = objectStringifier.renderBottomRow(model)
    val lastVisibleRow = state.firstRow + windowSize - 1
    val lastRow = state.size - 1
    val moreDataItemsBelowWindow = lastVisibleRow < lastRow
    val newBottom = bottomRow.when(moreDataItemsBelowWindow, _.updated(bottomRow.size / 2, '↓'))
    Line(newBottom.style)
  }

  private def renderStatusLine = {
    import KeyHint._
    Line(renderKeyHints(Seq(Exit, Focus, Back, Insert, InsertWhole, Tree)))
  }

  protected val windowSize = terminalInfo.rows - 4 // an upper status line, one header row, a footer row, a status line

}
