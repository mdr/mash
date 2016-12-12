package com.github.mdr.mash.screen

import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.repl.browser.ValueBrowserState
import com.github.mdr.mash.terminal.TerminalInfo
import com.github.mdr.mash.screen.Style.StylableString

import scala.collection.mutable

class ValueBrowserRenderer(state: ValueBrowserState, terminalInfo: TerminalInfo) {
  private val fileSystem = LinuxFileSystem

  private def renderUpperStatusLine: Line =
    Line(LineBufferRenderer.renderChars(state.path, mishByDefault = false, globalVariables = mutable.Map(), bareWords = false))

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
