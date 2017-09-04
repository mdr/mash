package com.github.mdr.mash.view.model

import com.github.mdr.mash.runtime._

sealed trait ObjectTreeNode {
  val rawValue: MashValue
}

object ObjectTreeNode {

  case class Leaf(value: String, rawValue: MashValue) extends ObjectTreeNode
  case class List(values: Seq[ObjectTreeNode], rawValue: MashList) extends ObjectTreeNode
  case class Object(values: Seq[(String, ObjectTreeNode)], rawValue: MashObject) extends ObjectTreeNode

}

case class ObjectTreeModel(root: ObjectTreeNode, rawValue: MashValue) extends DisplayModel {
  
  
  
}
