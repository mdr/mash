package com.github.mdr.mash.screen.browser

import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.repl.browser.BrowserState
import com.github.mdr.mash.screen._
import com.github.mdr.mash.utils.Dimensions

abstract class AbstractBrowserRenderer(state: BrowserState, terminalSize: Dimensions) {

  protected val fileSystem = LinuxFileSystem

  protected val windowSize: Int

  protected def renderLines: LinesAndCursorPos

  def renderObjectBrowser: Screen = {
    val LinesAndCursorPos(lines, cursorPosOpt) = renderLines
    val fixedLines = lines.map(_.truncate(terminalSize.columns))
    val title = "mash " + fileSystem.pwd.toString
    Screen(fixedLines, cursorPosOpt, title = title)
  }

  protected def renderUpperStatusLines: LinesAndCursorPos =
    state.expressionStateOpt match {
      case Some(expressionState) ⇒
        val LinesAndCursorPos(lines, cursorPosOpt) = LineBufferRenderer.renderLineBuffer(expressionState.lineBuffer,
          globalVariablesOpt = None, prefix = StyledString.empty, bareWords = false, mish = false, terminalSize)
        val availableSpace = terminalSize.shrink(rows = lines.size)
        val completionLines = CompletionRenderer.renderCompletions(expressionState.completionStateOpt, availableSpace).lines
        LinesAndCursorPos(lines ++ completionLines, cursorPosOpt)
      case None                  ⇒
        LinesAndCursorPos(Seq(Line(new MashRenderer().renderChars(state.path))))
    }

  protected def combineUpperStatusLines(upperLines: LinesAndCursorPos, otherLines: Seq[Line]): LinesAndCursorPos = {
    val newLines = upperLines.lines ++ otherLines.drop(upperLines.lines.length - 1)
    LinesAndCursorPos(newLines, upperLines.cursorPosOpt)
  }

}
