package com.github.mdr.mash.repl.browser

import com.github.mdr.mash.printer.model.ValueModel
import com.github.mdr.mash.runtime.MashValue

case class ValueBrowserState(model: ValueModel, path: String) extends BrowserState {

  override def rawValue: MashValue = model.rawValue

  override def withPath(newPath: String): ValueBrowserState = copy(path = newPath)

  override def getInsertExpression: String = path
}
