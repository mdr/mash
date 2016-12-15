package com.github.mdr.mash.screen.browser

import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.repl.browser.BrowserState
import com.github.mdr.mash.screen.{ Line, LineBufferRenderer, Point, Screen }
import com.github.mdr.mash.terminal.TerminalInfo

import scala.collection.mutable

abstract class AbstractBrowserRenderer(state: BrowserState, terminalInfo: TerminalInfo) {

  protected val fileSystem = LinuxFileSystem

  protected val windowSize: Int

  protected def renderLines: Seq[Line]

  def renderObjectBrowser: Screen = {
    val lines = renderLines.map(_.truncate(terminalInfo.columns))
    val title = "mash " + fileSystem.pwd.toString
    Screen(lines, cursorPos = Point(0, 0), cursorVisible = false, title = title)
  }

  protected def renderUpperStatusLine: Line =
    Line(LineBufferRenderer.renderChars(state.path, mishByDefault = false, globalVariables = mutable.Map(), bareWords = false))

}
