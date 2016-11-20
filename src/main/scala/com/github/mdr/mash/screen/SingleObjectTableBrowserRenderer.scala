package com.github.mdr.mash.screen

import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.printer.{ ObjectStringifier, UnicodeBoxCharacterSupplier }
import com.github.mdr.mash.repl.SingleObjectTableBrowserState
import com.github.mdr.mash.screen.Style.StylableString
import com.github.mdr.mash.terminal.TerminalInfo
import com.github.mdr.mash.utils.StringUtils

class SingleObjectTableBrowserRenderer(state: SingleObjectTableBrowserState, terminalInfo: TerminalInfo) {
  private val fileSystem = LinuxFileSystem
  private val boxCharacterSupplier = UnicodeBoxCharacterSupplier
  private val objectStringifier = new ObjectStringifier(terminalInfo)

  def renderObjectBrowser: Screen = {
    val lines = renderLines
    val title = "mash " + fileSystem.pwd.toString
    Screen(lines, cursorPos = Point(0, 0), cursorVisible = false, title = title)
  }

  private def renderLines: Seq[Line] = {
    val headerLines = renderHeaderLines
    val dataLines = renderDataLines
    val footerLine = renderFooterLine
    val statusLine = renderStatusLine
    headerLines ++ dataLines ++ Seq(footerLine, statusLine)
  }

  private def renderHeaderLines: Seq[Line] =
    Seq(
      objectStringifier.renderTopRow(model)).map(s ⇒ Line(s.style))

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
    val fieldChars = StringUtils.fitToWidth(renderedField, model.fieldColumnWidth).style(Style(inverse = isCursorRow))
    val valueChars = StringUtils.fitToWidth(renderedValue, model.valueColumnWidth).style(Style(inverse = isCursorRow))
    Line(side ++ fieldChars ++ internalVertical ++ valueChars ++ side)
  }

  private def renderFooterLine = Line(objectStringifier.renderBottomRow(model).style)

  private def renderStatusLine = {
    import KeyHint._
    Line(s"${state.path} (".style ++renderKeyHints(Seq(Exit, Focus, Back, Insert)) ++ ")".style)
  }

  private def model = state.model

  private def windowSize = terminalInfo.rows - 3 // one header row, a footer row, a status line

}
