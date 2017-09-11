package com.github.mdr.mash.view.render.browser

import com.github.mdr.mash.assist.AssistanceState
import com.github.mdr.mash.os.linux.{ LinuxEnvironmentInteractions, LinuxFileSystem }
import com.github.mdr.mash.repl.browser.BrowserState
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.screen.Style._
import com.github.mdr.mash.screen._
import com.github.mdr.mash.utils.Dimensions
import com.github.mdr.mash.view.render._

abstract class AbstractBrowserRenderer(state: BrowserState, terminalSize: Dimensions, mashRenderingContext: MashRenderingContext) {

  protected val fileSystem = LinuxFileSystem

  protected val windowSize: Int

  protected def renderLines: LinesAndCursorPos

  def renderObjectBrowser: Screen = {
    val LinesAndCursorPos(lines, cursorPosOpt) = renderLines
    val title = "mash " + fileSystem.pwd.toString
    Screen(lines, cursorPosOpt, title = title, isAlternateScreen = true)
  }

  protected def renderUpperStatusLines: LinesAndCursorPos = {
    val prefix = s"$typeName: ".style(DefaultColours.Orange)
    state.expressionStateOpt match {
      case Some(expressionState) ⇒
        val LinesAndCursorPos(lines, cursorPosOpt) = lineBufferRenderer.renderLineBuffer(expressionState.lineBuffer,
          terminalSize, mashRenderingContext, prefix)
        val assistanceLines = renderAssistanceState(expressionState.assistanceStateOpt, terminalSize)
        val availableSpace = terminalSize.shrink(rows = lines.size + assistanceLines.size)
        val completionLines = CompletionRenderer.renderCompletions(expressionState.completionStateOpt, availableSpace).lines
        val expressionLines = if (lines.isEmpty) Seq(Line.Empty) else lines
        LinesAndCursorPos(expressionLines ++ completionLines ++ assistanceLines, cursorPosOpt)
      case None                  ⇒
        val expression = state.path.replace('\n', ' ')
        LinesAndCursorPos(Seq(Line(prefix + new MashRenderer(mashRenderingContext).renderChars(expression))))
    }
  }

  private def lineBufferRenderer: LineBufferRenderer = new LineBufferRenderer(LinuxEnvironmentInteractions, LinuxFileSystem)

  private def renderAssistanceState(assistanceStateOpt: Option[AssistanceState], terminalSize: Dimensions) =
    assistanceStateOpt.toSeq.flatMap(AssistanceRenderer.render(_, terminalSize))

  protected def combineUpperStatusLines(upperLines: LinesAndCursorPos, otherLines: Seq[Line]): LinesAndCursorPos = {
    val newLines = upperLines.lines ++ otherLines.drop(upperLines.lines.length - 1)
    LinesAndCursorPos(newLines, upperLines.cursorPosOpt)
  }

  protected def typeName: String = (state.rawValue match {
    case TaggableMashValue.TagClass(tagClass) ⇒ tagClass
    case other                                ⇒ other.primaryClass
  }).name

  protected def renderCount(current: Int, total: Int): StyledString = {
    val paddedCurrent = current.toString.reverse.padTo(total.toString.length, ' ').reverse
    s"$paddedCurrent/$total".style(inverse = true)
  }

}
