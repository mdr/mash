package com.github.mdr.mash.screen.browser

import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.repl.browser.BrowserState
import com.github.mdr.mash.screen._
import com.github.mdr.mash.terminal.TerminalInfo

abstract class AbstractBrowserRenderer(state: BrowserState, terminalInfo: TerminalInfo) {

  protected val fileSystem = LinuxFileSystem

  protected val windowSize: Int

  protected def renderLines: Seq[Line]

  def renderObjectBrowser: Screen = {
    val lines = renderLines.map(_.truncate(terminalInfo.columns))
    val title = "mash " + fileSystem.pwd.toString
    val cursorPosOpt = getCursorPointOpt
    Screen(lines, cursorPos = cursorPosOpt getOrElse Point(0, 0), cursorVisible = cursorPosOpt.isDefined, title = title)
  }

  private def getCursorPointOpt: Option[Point] =
    state.expressionStateOpt map { expressionState ⇒ Point(0, expressionState.lineBuffer.cursorOffset) }

  protected def renderUpperStatusLines: Seq[Line] =
    state.expressionStateOpt match {
      case Some(expressionState) ⇒
        val cursorOffset = expressionState.lineBuffer.cursorOffset
        val line = Line(new MashRenderer().renderChars(expressionState.lineBuffer.text, cursorOffsetOpt = Some(cursorOffset)))
        val completionLines = CompletionRenderer.renderCompletions(expressionState.completionStateOpt, terminalInfo).lines
        Seq(line) ++ completionLines
      case None                  ⇒
        Seq(Line(new MashRenderer().renderChars(state.path)))
    }

  protected def combineUpperStatusLines(upperLines: Seq[Line], otherLines: Seq[Line]): Seq[Line] =
    upperLines ++ otherLines.drop(upperLines.length - 1)

}
