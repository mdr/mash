package com.github.mdr.mash.printer

import com.github.mdr.mash.runtime._

class ObjectTreeModelCreator {

  def create(value: MashValue): ObjectTreeModel =
    ObjectTreeModel(createNode(value))

  private def createNode(value: MashValue): ObjectTreeNode = value match {
    case xs: MashList    => ObjectTreeNode.List(xs.items.map(createNode), xs)
    case obj: MashObject => ObjectTreeNode.Object(obj.fields.toSeq.map { case (field, value) => field -> createNode(value) }, obj)
    case _               => ObjectTreeNode.Leaf(Printer.renderField(value, inCell = true), value)
  }

}
