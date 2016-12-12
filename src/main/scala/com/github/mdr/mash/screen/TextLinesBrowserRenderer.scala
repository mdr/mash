package com.github.mdr.mash.screen

import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.repl.browser.TextLinesBrowserState
import com.github.mdr.mash.screen.Style.StylableString
import com.github.mdr.mash.terminal.TerminalInfo

import scala.collection.mutable

class TextLinesBrowserRenderer(state: TextLinesBrowserState, terminalInfo: TerminalInfo) {
  private val fileSystem = LinuxFileSystem

  private def renderUpperStatusLine: Line =
    Line(LineBufferRenderer.renderChars(state.path, mishByDefault = false, globalVariables = mutable.Map(), bareWords = false))

  private def renderDataLines: Seq[Line] =
    for ((l, index) <- state.model.renderedLines.drop(state.firstRow).take(windowSize).zipWithIndex)
      yield Line(l.style(Style(inverse = index == (state.selectedRow - state.firstRow))))

  private def renderLines: Seq[Line] = {
    val upperStatusLine = renderUpperStatusLine
    val dataLines = renderDataLines
    val statusLine = renderStatusLine
    Seq(upperStatusLine) ++ dataLines ++ Seq(statusLine)
  }

  private def renderStatusLine = {
    import KeyHint._
    val hints = Seq(Exit, Back, InsertWhole)
    val countChars = s"${state.selectedRow + 1}/${state.model.renderedLines.size}".style(Style(inverse = true))
    Line(countChars ++ " (".style ++ renderKeyHints(hints) ++ ")".style)
  }

  def renderObjectBrowser: Screen = {
    val lines = renderLines.map(_.truncate(terminalInfo.columns))
    val title = "mash " + fileSystem.pwd.toString
    Screen(lines, cursorPos = Point(0, 0), cursorVisible = false, title = title)
  }

  private val windowSize = terminalInfo.rows - 2 // two status lines

}
