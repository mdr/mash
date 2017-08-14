package com.github.mdr.mash.render.browser

import com.github.mdr.mash.assist.AssistanceState
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.render._
import com.github.mdr.mash.repl.browser.BrowserState
import com.github.mdr.mash.screen._
import com.github.mdr.mash.utils.Dimensions

abstract class AbstractBrowserRenderer(state: BrowserState, terminalSize: Dimensions, mashRenderingContext: MashRenderingContext) {

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
          terminalSize, mashRenderingContext)
        val assistanceLines = renderAssistanceState(expressionState.assistanceStateOpt, terminalSize)
        val availableSpace = terminalSize.shrink(rows = lines.size + assistanceLines.size)
        val completionLines = CompletionRenderer.renderCompletions(expressionState.completionStateOpt, availableSpace).lines
        val expressionLines = if (lines.isEmpty) Seq(Line.Empty) else lines
        LinesAndCursorPos(expressionLines ++ completionLines ++ assistanceLines, cursorPosOpt)
      case None                  ⇒
        LinesAndCursorPos(Seq(Line(new MashRenderer(mashRenderingContext).renderChars(state.path))))
    }

  private def renderAssistanceState(assistanceStateOpt: Option[AssistanceState], terminalSize: Dimensions) =
    assistanceStateOpt.toSeq.flatMap(AssistanceRenderer.render(_, terminalSize))

  protected def combineUpperStatusLines(upperLines: LinesAndCursorPos, otherLines: Seq[Line]): LinesAndCursorPos = {
    val newLines = upperLines.lines ++ otherLines.drop(upperLines.lines.length - 1)
    LinesAndCursorPos(newLines, upperLines.cursorPosOpt)
  }

}
