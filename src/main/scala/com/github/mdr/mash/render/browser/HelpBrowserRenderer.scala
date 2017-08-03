package com.github.mdr.mash.render.browser

import com.github.mdr.mash.printer.model.Link
import com.github.mdr.mash.render.{ KeyHint, LinesAndCursorPos }
import com.github.mdr.mash.repl.browser.HelpBrowserState
import com.github.mdr.mash.screen.Line
import com.github.mdr.mash.screen.Style._
import com.github.mdr.mash.utils.Utils._
import com.github.mdr.mash.utils.{ Dimensions, Region, StyledStringUtils }

class HelpBrowserRenderer(state: HelpBrowserState, terminalSize: Dimensions)
  extends AbstractBrowserRenderer(state, terminalSize) {

  override protected val windowSize = state.windowSize(terminalSize.rows)

  protected def renderLines: LinesAndCursorPos =
    combineUpperStatusLines(renderUpperStatusLines, renderDataLines ++ Seq(renderStatusLine))

  protected def renderDataLines: Seq[Line] = {
    val currentLinkOpt = state.currentLinkOpt
    val lines = state.model.lines.zipWithIndex.map { case (line, i) ⇒
      Line(
        if (i == state.currentRow && state.expressionStateOpt.isEmpty)
          currentLinkOpt.collect { case Link(`i`, region, _, _) ⇒ region } match {
            case Some(selectedLinkRegion) ⇒
              line.invert(selectedLinkRegion)
            case None                     ⇒
              if (line.isEmpty)
                " ".style(inverse = true)
              else
                line.invert(Region(0, 1))
          }
        else
          line)
    }.map(addBorder)

    val headerLine = Line(style"┌${"─" * (terminalSize.columns - 2)}┐")
    val footerLine = Line(style"└${"─" * (terminalSize.columns - 2)}┘")
    headerLine +: lines.window(state.firstRow, windowSize) :+ footerLine
  }

  private def addBorder(line: Line): Line = {
    val innerWidth = 0 max terminalSize.columns - 2
    val inner = StyledStringUtils.ellipsisise(line.string.padTo(innerWidth, ' '.style), innerWidth)
    line.copy(string = style"│$inner│")
  }

  private def renderStatusLine: Line =
    state.expressionStateOpt match {
      case Some(expressionState) ⇒ StatusLineRenderers.renderExpressionInputStatusLine
      case None                  ⇒ renderRegularStatusLine
    }

  private def renderRegularStatusLine = {
    import KeyHint._
    val hints = Seq(Exit, Back, InsertWhole)
    Line("(".style + renderKeyHints(hints) + ")".style)
  }
}
