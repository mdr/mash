package com.github.mdr.mash.repl.browser

import com.github.mdr.mash.printer.model.ValueModel
import com.github.mdr.mash.runtime.MashValue

case class ValueBrowserState(model: ValueModel,
                             path: String,
                             expressionStateOpt: Option[ExpressionState] = None) extends BrowserState {

  override def rawValue: MashValue = model.rawValue

  override def withPath(newPath: String): ValueBrowserState = copy(path = newPath)

  override def selectionInfo: SelectionInfo = SelectionInfo(path, model.rawValue)

  def withExpressionState(expressionStateOpt: Option[ExpressionState]): BrowserState = copy(expressionStateOpt = expressionStateOpt)
}
