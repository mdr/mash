package com.github.mdr.mash.screen

import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.repl.ValueBrowserState
import com.github.mdr.mash.terminal.TerminalInfo
import com.github.mdr.mash.screen.Style.StylableString

class ValueBrowserRenderer(state: ValueBrowserState, terminalInfo: TerminalInfo) {
  private val fileSystem = LinuxFileSystem

  def renderDataLines: Seq[Line] = {
    val renderedValue = state.model.renderedValue
    val stringLines = renderedValue.split("""\r?\n""", -1)
    stringLines.flatMap(renderLine).take(windowSize)
  }

  private def renderLine(stringLine: String): Seq[Line] = {
    val groups = stringLine.grouped(terminalInfo.columns).toSeq
    for {
      (group, index) ← groups.zipWithIndex
      endsInNewline = index == groups.size - 1
    } yield Line(group.style, endsInNewline)
  }

  private def renderLines: Seq[Line] = {
    val dataLines = renderDataLines
    val statusLine = renderStatusLine
    dataLines ++ Seq(statusLine)
  }

  private def renderStatusLine = {
    import KeyHint._
    Line(s"${state.path} (".style ++ renderKeyHints(Seq(Exit, Back)) ++ ")".style)
  }

  def renderObjectBrowser: Screen = {
    val lines = renderLines.map(_.truncate(terminalInfo.columns))
    val title = "mash " + fileSystem.pwd.toString
    Screen(lines, cursorPos = Point(0, 0), cursorVisible = false, title = title)
  }

  private val windowSize = terminalInfo.rows - 1 // a status line

}
