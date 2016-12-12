package com.github.mdr.mash.screen

import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.repl.TextLinesBrowserState
import com.github.mdr.mash.screen.Style.StylableString
import com.github.mdr.mash.terminal.TerminalInfo

import scala.collection.mutable

class TextLinesBrowserRenderer(state: TextLinesBrowserState, terminalInfo: TerminalInfo) {
  private val fileSystem = LinuxFileSystem

  private def renderUpperStatusLine: Line =
    Line(LineBufferRenderer.renderChars(state.path, mishByDefault = false, globalVariables = mutable.Map(), bareWords = false))

  private def renderDataLines: Seq[Line] =
    state.model.renderedLines.map(l => Line(l.style)).take(windowSize)

  private def renderLines: Seq[Line] = {
    val upperStatusLine = renderUpperStatusLine
    val dataLines = renderDataLines
    val statusLine = renderStatusLine
    Seq(upperStatusLine) ++ dataLines ++ Seq(statusLine)
  }

  private def renderStatusLine = {
    import KeyHint._
    Line(renderKeyHints(Seq(Exit, Back, InsertWhole)))
  }

  def renderObjectBrowser: Screen = {
    val lines = renderLines.map(_.truncate(terminalInfo.columns))
    val title = "mash " + fileSystem.pwd.toString
    Screen(lines, cursorPos = Point(0, 0), cursorVisible = false, title = title)
  }

  private val windowSize = terminalInfo.rows - 2 // two status lines

}
