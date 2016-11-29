package com.github.mdr.mash.printer

import com.github.mdr.mash.runtime._

class ObjectTreeModelCreator(viewConfig: ViewConfig) {

  private val fieldRenderer = new FieldRenderer(viewConfig)

  def create(value: MashValue): ObjectTreeModel =
    ObjectTreeModel(createNode(value), value)

  private def createNode(value: MashValue): ObjectTreeNode = value match {
    case xs: MashList    => ObjectTreeNode.List(xs.items.map(createNode), xs)
    case obj: MashObject => ObjectTreeNode.Object(obj.fields.toSeq.map { case (field, value) => field -> createNode(value) }, obj)
    case _               => ObjectTreeNode.Leaf(fieldRenderer.renderField(value, inCell = true), value)
  }

}
