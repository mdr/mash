package com.github.mdr.mash.printer.model

import com.github.mdr.mash.printer.{ FieldRenderer, ViewConfig }
import com.github.mdr.mash.runtime._

class ObjectTreeModelCreator(viewConfig: ViewConfig) {

  private val fieldRenderer = new FieldRenderer(viewConfig)

  def create(value: MashValue): ObjectTreeModel =
    ObjectTreeModel(createNode(value), value)

  private def createNode(nodeValue: MashValue): ObjectTreeNode = nodeValue match {
    case xs: MashList    ⇒
      ObjectTreeNode.List(xs.immutableElements.map(createNode), xs)
    case obj: MashObject ⇒
      val values = obj.immutableFields.toSeq.map { case (field, value) ⇒ renderField(field) -> createNode(value) }
      ObjectTreeNode.Object(values, obj)
    case _               ⇒
      ObjectTreeNode.Leaf(renderField(nodeValue), nodeValue)
  }

  private def renderField(nodeValue: MashValue): String = fieldRenderer.renderField(nodeValue, inCell = true)

}
