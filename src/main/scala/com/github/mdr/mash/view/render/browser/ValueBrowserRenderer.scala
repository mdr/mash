package com.github.mdr.mash.view.render.browser

import com.github.mdr.mash.view.render.{ KeyHint, LinesAndCursorPos, MashRenderingContext }
import com.github.mdr.mash.repl.browser.ValueBrowserState
import com.github.mdr.mash.screen.Style.StylableString
import com.github.mdr.mash.screen._
import com.github.mdr.mash.utils.Dimensions

class ValueBrowserRenderer(state: ValueBrowserState, terminalSize: Dimensions, mashRenderingContext: MashRenderingContext)
  extends AbstractBrowserRenderer(state, terminalSize, mashRenderingContext) {

  def renderDataLines: Seq[Line] = {
    val renderedValue = state.model.renderedValue
    val stringLines = renderedValue.split("""\r?\n""", -1)
    stringLines.flatMap(renderLine).take(windowSize)
  }

  private def renderLine(stringLine: String): Seq[Line] = {
    val groups = stringLine.grouped(terminalSize.columns).toSeq
    for {
      (group, index) ← groups.zipWithIndex
      endsInNewline = index == groups.size - 1
    } yield Line(group.style, endsInNewline)
  }

  protected def renderLines: LinesAndCursorPos =
    combineUpperStatusLines(renderUpperStatusLines, renderDataLines ++ Seq(renderStatusLine))

  private def renderRegularStatusLine = {
    import KeyHint._
    Line(renderKeyHints(Seq(Exit, Back, Insert, Read, NextParentResult, PreviousParentResult)))
  }

  private def renderStatusLine: Line =
    state.expressionStateOpt match {
      case Some(_) ⇒ StatusLineRenderers.renderExpressionInputStatusLine
      case None    ⇒ renderRegularStatusLine
    }

  protected val windowSize = terminalSize.rows - 2 // two status lines

}
