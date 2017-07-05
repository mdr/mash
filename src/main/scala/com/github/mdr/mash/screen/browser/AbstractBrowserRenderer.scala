package com.github.mdr.mash.screen.browser

import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.repl.browser.BrowserState
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.screen._
import com.github.mdr.mash.terminal.TerminalInfo

abstract class AbstractBrowserRenderer(state: BrowserState, terminalInfo: TerminalInfo) {

  protected val fileSystem = LinuxFileSystem

  protected val windowSize: Int

  protected def renderLines: Seq[Line]

  def renderObjectBrowser: Screen = {
    val lines = renderLines.map(_.truncate(terminalInfo.columns))
    val title = "mash " + fileSystem.pwd.toString
    val (cursorPos, cursorVisible) = renderCursor
    Screen(lines, cursorPos = cursorPos, cursorVisible = cursorVisible, title = title)
  }

  private def renderCursor: (Point, Boolean) =
    state.expressionOpt match {
      case Some(expression) ⇒ Point(0, expression.length + state.path.length) -> true
      case _                ⇒ Point(0, 0) -> false
    }

  protected def renderUpperStatusLine: Line = {
    val fullExpression = state.expressionOpt match {
      case Some(expression) ⇒ state.path + expression
      case None             ⇒ state.path
    }
    val cursorOffsetOpt = if (state.expressionOpt.isDefined) Some(renderCursor._1.column) else None
    Line(new MashRenderer().renderChars(fullExpression, cursorOffsetOpt, mishByDefault = false))
  }

}
