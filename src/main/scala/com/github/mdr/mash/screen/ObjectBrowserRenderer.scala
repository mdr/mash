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

    val currentRowRelative = state.currentRow - state.firstRow
    val objects = model.objects.drop(state.firstRow).take(windowSize)
    def renderObject(obj: ObjectTableRow, isSelected: Boolean, isSelected2: Boolean): Line = {
      val side = boxCharacterSupplier.doubleVertical.map(StyledCharacter(_))
      val selected = ((if (isSelected2) "X" else " ") + boxCharacterSupplier.singleVertical).map(StyledCharacter(_))
      val internalVertical = boxCharacterSupplier.singleVertical.map(StyledCharacter(_, Style(inverse = isSelected)))
      def renderCell(name: String) = StringUtils.fitToWidth(obj.data(name), model.columnWidth(name)).map(StyledCharacter(_, Style(inverse = isSelected)))
      val innerChars = Utils.intercalate(model.columnNames.map(renderCell), internalVertical)
      Line(side ++ selected ++ innerChars ++ side)
    }
    val dataLines =
      for {
        (obj, i) ← objects.zipWithIndex
      } yield renderObject(obj, i == currentRowRelative, state.selectedRows contains i)

    val title = "mash " + fileSystem.pwd.toString

    Screen(headerLines ++ dataLines ++ footerLines, Point(0, 0), cursorVisible = false, title = title)
  }

}