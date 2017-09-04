package com.github.mdr.mash.view.model

import com.github.mdr.mash.runtime._
import com.github.mdr.mash.view.{ FieldRenderer, ViewConfig }

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
