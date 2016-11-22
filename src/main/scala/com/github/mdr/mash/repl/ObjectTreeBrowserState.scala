package com.github.mdr.mash.repl

import com.github.mdr.mash.printer.{ ObjectTreeModel, ObjectTreeNode }
import com.github.mdr.mash.repl.ObjectTreeChoice.{ FieldChoice, IndexChoice }
import com.github.mdr.mash.runtime.MashValue

sealed trait ObjectTreeChoice

object ObjectTreeChoice {

  case class FieldChoice(fieldName: String) extends ObjectTreeChoice

  case class IndexChoice(i: Int) extends ObjectTreeChoice

}

case class ObjectTreePath(choices: Seq[ObjectTreeChoice] = Seq()) {

  def ascend = if (choices.isEmpty) this else ObjectTreePath(choices.init)

  def descend(index: Int) = ObjectTreePath(choices :+ IndexChoice(index))

  def descend(field: String) = ObjectTreePath(choices :+ FieldChoice(field))

}

case class ObjectTreeBrowserState(model: ObjectTreeModel, selectionPath: ObjectTreePath, path: String) extends BrowserState {

  def rawValue: MashValue = model.rawValue

  private def getNode(node: ObjectTreeNode, path: Seq[ObjectTreeChoice]): ObjectTreeNode = (node, path) match {
    case (_, Seq())                                                           => node
    case (ObjectTreeNode.List(values, _), Seq(IndexChoice(i), tail@_*))       => getNode(values(i), tail)
    case (ObjectTreeNode.Object(values, _), Seq(FieldChoice(field), tail@_*)) => getNode(values.collectFirst { case (`field`, node) => node }.get, tail)
  }

  private def getNode(path: ObjectTreePath): ObjectTreeNode =
    getNode(model.root, path.choices)

  def right: ObjectTreeBrowserState = getNode(selectionPath) match {
    case ObjectTreeNode.List(items, _) if items.nonEmpty   => copy(selectionPath = selectionPath.descend(0))
    case ObjectTreeNode.Object(items, _) if items.nonEmpty => copy(selectionPath = selectionPath.descend(items.head._1))
    case _                                                 => this
  }

  def left: ObjectTreeBrowserState = copy(selectionPath = selectionPath.ascend)

  def down: ObjectTreeBrowserState =
    if (selectionPath.choices.isEmpty)
      this
    else {
      val thing = (getNode(selectionPath.ascend), selectionPath.choices.last) match {
        case (ObjectTreeNode.List(values, _), IndexChoice(i))       => IndexChoice((i + 1 + values.size) % values.size)
        case (ObjectTreeNode.Object(values, _), FieldChoice(field)) =>
          FieldChoice(values((values.map(_._1).indexOf(field) + 1 + values.size) % values.size)._1)
        case (_, choice)                                            => choice
      }
      copy(selectionPath = ObjectTreePath(selectionPath.choices.init :+ thing))
    }

  def up: ObjectTreeBrowserState =
    if (selectionPath.choices.isEmpty)
      this
    else {
      val thing = (getNode(selectionPath.ascend), selectionPath.choices.last) match {
        case (ObjectTreeNode.List(values, _), IndexChoice(i))       => IndexChoice((i - 1 + values.size) % values.size)
        case (ObjectTreeNode.Object(values, _), FieldChoice(field)) =>
          FieldChoice(values((values.map(_._1).indexOf(field) - 1 + values.size) % values.size)._1)
        case (_, choice)                                            => choice
      }
      copy(selectionPath = ObjectTreePath(selectionPath.choices.init :+ thing))
    }

  def withPath(newPath: String): ObjectTreeBrowserState = copy(path = newPath)

}
