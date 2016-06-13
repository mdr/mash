package com.github.mdr.mash.screen

import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.printer.BoxCharacterSupplier
import com.github.mdr.mash.printer.ObjectTablePrinter
import com.github.mdr.mash.printer.UnicodeBoxCharacterSupplier
import com.github.mdr.mash.repl.ObjectBrowserState
import com.github.mdr.mash.terminal.TerminalInfo
import com.github.mdr.mash.utils.StringUtils
import com.github.mdr.mash.utils.Utils
import com.github.mdr.mash.printer.ObjectTableRenderer
import com.github.mdr.mash.printer.ObjectTableRow

object ObjectBrowserRenderer {
  private val fileSystem = LinuxFileSystem

  def renderObjectBrowser(state: ObjectBrowserState, terminalInfo: TerminalInfo): Screen = {
    val boxCharacterSupplier: BoxCharacterSupplier = UnicodeBoxCharacterSupplier

    val model = state.model
    val currentRow = state.currentRow
    val windowSize = terminalInfo.rows - 5 // three header rows, a footer row, a status line
    val objectTablePrinter = new ObjectTableRenderer(terminalInfo, showSelections = true)
    def simpleLine(s: String) = Line(s.map(StyledCharacter(_)))
    val headerLines: Seq[Line] = Seq(
      objectTablePrinter.renderTopRow(model),
      objectTablePrinter.renderHeaderRow(model),
      objectTablePrinter.renderBelowHeaderRow(model)).map(simpleLine)

    val footerLine = simpleLine(objectTablePrinter.renderBottomRow(model))
    val countChars = s"${currentRow + 1}/${model.objects.size}".map(StyledCharacter(_, Style(inverse = true)))
    val keyChars =
      " (".map(StyledCharacter(_)) ++
        "q".map(StyledCharacter(_, Style(inverse = true))) ++
        " to exit, ".map(StyledCharacter(_)) ++
        "s".map(StyledCharacter(_, Style(inverse = true))) ++
        " to select, ".map(StyledCharacter(_)) ++
        "i".map(StyledCharacter(_, Style(inverse = true))) ++
        " to insert reference)".map(StyledCharacter(_))
    val statusLine = Line(countChars ++ keyChars)
    val footerLines = Seq(footerLine, statusLine)

    val objects = model.objects.drop(state.firstRow).take(windowSize)
    def renderObject(obj: ObjectTableRow, cursorRow: Boolean, isSelected: Boolean): Line = {
      val side = boxCharacterSupplier.doubleVertical.map(StyledCharacter(_))
      val selected = ((if (isSelected) "▒" else " ") + boxCharacterSupplier.singleVertical).map(StyledCharacter(_))
      val internalVertical = boxCharacterSupplier.singleVertical.map(StyledCharacter(_, Style(inverse = cursorRow)))
      def renderCell(name: String) = StringUtils.fitToWidth(obj.data(name), model.columnWidth(name)).map(StyledCharacter(_, Style(inverse = cursorRow)))
      val innerChars = Utils.intercalate(model.columnNames.map(renderCell), internalVertical)
      Line(side ++ selected ++ innerChars ++ side)
    }
    val dataLines =
      for {
        (obj, i) ← objects.zipWithIndex
        actualIndex = i + state.firstRow
      } yield renderObject(obj, actualIndex == state.currentRow, state.selectedRows contains actualIndex)

    val title = "mash " + fileSystem.pwd.toString

    Screen(headerLines ++ dataLines ++ footerLines, Point(0, 0), cursorVisible = false, title = title)
  }

}