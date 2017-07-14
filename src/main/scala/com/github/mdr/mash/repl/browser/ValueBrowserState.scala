package com.github.mdr.mash.repl.browser

import com.github.mdr.mash.printer.model.ValueModel
import com.github.mdr.mash.repl.LineBuffer
import com.github.mdr.mash.runtime.MashValue

case class ValueBrowserState(model: ValueModel,
                             path: String,
                             expressionStateOpt: Option[ExpressionState] = None) extends BrowserState {

  override def rawValue: MashValue = model.rawValue

  override def withPath(newPath: String): ValueBrowserState = copy(path = newPath)

  override def selectionInfo: SelectionInfo = SelectionInfo(path, model.rawValue)

  def beginExpression: BrowserState = copy(expressionStateOpt = Some(ExpressionState(LineBuffer.Empty)))

  def setExpression(expressionState: ExpressionState): BrowserState = copy(expressionStateOpt = Some(expressionState))

  def acceptExpression: BrowserState = copy(expressionStateOpt = None)

}
