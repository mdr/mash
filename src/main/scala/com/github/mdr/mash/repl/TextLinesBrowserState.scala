package com.github.mdr.mash.repl

import com.github.mdr.mash.printer.model.{ TextLinesModel, ValueModel }
import com.github.mdr.mash.runtime.MashValue

case class TextLinesBrowserState(model: TextLinesModel, path: String) extends BrowserState {

  override def rawValue: MashValue = model.rawValue

  override def withPath(newPath: String): TextLinesBrowserState = copy(path = newPath)

  override def getInsertExpression: String = path
}
