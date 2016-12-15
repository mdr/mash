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
    val (cursorPos, cursorVisible) = state.expressionOpt match {
      case Some(expression) => Point(0, expression.length + state.path.length) -> true
      case _                => Point(0, 0) -> false
    }
    Screen(lines, cursorPos = cursorPos, cursorVisible = cursorVisible, title = title)
  }

  protected def renderUpperStatusLine: Line = {
    val fullExpression = state.expressionOpt match {
      case Some(expression) => state.path + expression
      case None             => state.path
    }
    Line(LineBufferRenderer.renderChars(fullExpression, mishByDefault = false, globalVariables = mutable.Map(), bareWords = false))
  }

}
