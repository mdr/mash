package com.github.mdr.mash.repl

import com.github.mdr.mash.printer.ObjectTreeModel
import com.github.mdr.mash.runtime.MashValue

sealed trait ObjectTreeChoice
object ObjectTreeChoice {
  case class FieldChoice(fieldName: String) extends ObjectTreeChoice
  case class IndexChoice(i: Int) extends ObjectTreeChoice
}
case class ObjectTreePath(choices: Seq[ObjectTreeChoice])

case class ObjectTreeBrowserState(model: ObjectTreeModel, selectionPath: ObjectTreePath, path: String) extends BrowserState {

  def rawValue: MashValue = model.rawValue

}
