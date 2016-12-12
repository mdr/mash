package com.github.mdr.mash.screen

import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.printer.{ ObjectStringifier, UnicodeBoxCharacterSupplier }
import com.github.mdr.mash.repl.browser.SingleObjectTableBrowserState
import com.github.mdr.mash.screen.Style.StylableString
import com.github.mdr.mash.terminal.TerminalInfo
import com.github.mdr.mash.utils.StringUtils

import scala.collection.mutable

class SingleObjectTableBrowserRenderer(state: SingleObjectTableBrowserState, terminalInfo: TerminalInfo) {
  private val fileSystem = LinuxFileSystem
  private val boxCharacterSupplier = UnicodeBoxCharacterSupplier
  private val objectStringifier = new ObjectStringifier(terminalInfo)

  def renderObjectBrowser: Screen = {
    val lines = renderLines.map(_.truncate(terminalInfo.columns))
    val title = "mash " + fileSystem.pwd.toString
    Screen(lines, cursorPos = Point(0, 0), cursorVisible = false, title = title)
  }

  private def renderLines: Seq[Line] = {
    val upperStatusLine = renderUpperStatusLine
    val headerLines = renderHeaderLines
    val dataLines = renderDataLines
    val footerLine = renderFooterLine
    val statusLine = renderStatusLine
    Seq(upperStatusLine) ++ headerLines ++ dataLines ++ Seq(footerLine, statusLine)
  }

  private def renderUpperStatusLine: Line =
    Line(LineBufferRenderer.renderChars(state.path, mishByDefault = false, globalVariables = mutable.Map(), bareWords = false))

  private def renderHeaderLines: Seq[Line] =
    Seq(objectStringifier.renderTopRow(model)).map(s ⇒ Line(s.style))

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

  private def renderFooterLine = Line(objectStringifier.renderBottomRow(model).style)

  private def renderStatusLine = {
    import KeyHint._
    Line(renderKeyHints(Seq(Exit, Focus, Back, Insert, InsertWhole, Tree)))
  }

  private def model = state.model

  private def windowSize = terminalInfo.rows - 4 // an upper status line, one header row, a footer row, a status line

}
