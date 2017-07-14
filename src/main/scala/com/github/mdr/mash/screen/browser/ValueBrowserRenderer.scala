package com.github.mdr.mash.screen.browser

import com.github.mdr.mash.repl.browser.ValueBrowserState
import com.github.mdr.mash.screen.Style.StylableString
import com.github.mdr.mash.screen._
import com.github.mdr.mash.terminal.TerminalInfo

class ValueBrowserRenderer(state: ValueBrowserState, terminalInfo: TerminalInfo)
  extends AbstractBrowserRenderer(state, terminalInfo) {

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

  protected def renderLines: Seq[Line] = {
    val upperStatusLine = renderUpperStatusLine
    val dataLines = renderDataLines
    val statusLine = renderStatusLine
    Seq(upperStatusLine) ++ dataLines ++ Seq(statusLine)
  }

  private def renderRegularStatusLine = {
    import KeyHint._
    Line(renderKeyHints(Seq(Exit, Back, InsertWhole)))
  }

  private def renderStatusLine: Line =
    state.expressionStateOpt match {
      case Some(expressionState) ⇒ StatusLineRenderers.renderExpressionInputStatusLine(expressionState.expression)
      case None                  ⇒ renderRegularStatusLine
    }

  protected val windowSize = terminalInfo.rows - 2 // two status lines


}
