package com.github.mdr.mash.screen

import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.printer.{ ObjectTableRow, ObjectTableStringifier, UnicodeBoxCharacterSupplier }
import com.github.mdr.mash.repl.ObjectBrowserState
import com.github.mdr.mash.screen.Style.StylableString
import com.github.mdr.mash.terminal.TerminalInfo
import com.github.mdr.mash.utils.{ StringUtils, Utils }

class ObjectBrowserRenderer(state: ObjectBrowserState, terminalInfo: TerminalInfo) {
  private val fileSystem = LinuxFileSystem
  private val boxCharacterSupplier = UnicodeBoxCharacterSupplier
  private val objectTableStringifier = new ObjectTableStringifier(terminalInfo, showSelections = true)

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
      objectTableStringifier.renderTopRow(model),
      objectTableStringifier.renderHeaderRow(model),
      objectTableStringifier.renderBelowHeaderRow(model)).map(s ⇒ Line(s.style))

  private def renderDataLines: Seq[Line] = {
    val objects = model.objects.drop(state.firstRow).take(windowSize)
    for {
      (obj, i) ← objects.zipWithIndex
      actualIndex = i + state.firstRow
    } yield renderObject(obj, actualIndex == state.currentRow, state.currentColumnOpt, state.selectedRows contains actualIndex)
  }

  private def renderObject(obj: ObjectTableRow, isCursorRow: Boolean, currentColumnOpt: Option[Int], isSelected: Boolean): Line = {
    val side = boxCharacterSupplier.doubleVertical.style
    val selectedChar = if (isSelected) "◈" else " "
    val highlightRow = isCursorRow && currentColumnOpt.isEmpty
    val selected = (selectedChar + boxCharacterSupplier.singleVertical).style(Style(inverse = highlightRow))
    val internalVertical = boxCharacterSupplier.singleVertical.style(Style(inverse = highlightRow))
    def renderCell(name: String, column: Int) = {
      val highlightCell = isCursorRow && currentColumnOpt.forall(_ == column)
      val cellContents = StringUtils.fitToWidth(obj.data(name), model.columnWidth(name))
      cellContents.style(Style(inverse = highlightCell))
    }
    val renderedCells = model.columnNames.zipWithIndex.map((renderCell _).tupled)
    val innerChars = Utils.intercalate(renderedCells, internalVertical)
    Line(side ++ selected ++ innerChars ++ side)
  }

  private def renderFooterLine = Line(objectTableStringifier.renderBottomRow(model).style)

  private def renderStatusLine: Line = {
    val countChars = s"${currentRow + 1}/${model.objects.size}".style(Style(inverse = true))
    val keyChars =
      " (".style ++
        "q".style(Style(inverse = true)) ++
        " exit, ".style ++
        "s".style(Style(inverse = true)) ++
        " mark, ".style ++
        "i".style(Style(inverse = true)) ++
        " insert".style ++ (if (state.currentColumnOpt.isDefined) ", ".style ++ "r".style(Style(inverse = true)) ++
        " select row".style else Seq()) ++
        ")".style
    Line(countChars ++ keyChars)
  }

  private def model = state.model

  private def currentRow = state.currentRow

  private def windowSize = terminalInfo.rows - 5 // three header rows, a footer row, a status line

}