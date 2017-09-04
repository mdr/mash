package com.github.mdr.mash.view.common

import com.github.mdr.mash.repl.browser.ObjectTreePath
import com.github.mdr.mash.screen.Style.StylableString
import com.github.mdr.mash.screen._
import com.github.mdr.mash.utils.{ Dimensions, StringUtils }
import com.github.mdr.mash.view.model.{ ObjectTreeModel, ObjectTreeNode }

import scala.collection.mutable.ArrayBuffer

class ObjectTreeCommonRenderer(model: ObjectTreeModel,
                               selectionPathOpt: Option[ObjectTreePath],
                               terminalSize: Dimensions) {

  def renderTableLines: Seq[Line] = {
    val printer = new Printer
    print(printer, model.root)
    printer.getLines
  }

  private class Printer() {

    private val lines = ArrayBuffer[Line]()

    private var line = StyledString.Empty

    def println() {
      lines += Line(line)
      line = StyledString.Empty
    }

    def println(s: String) {
      line += s.style
      println()
    }

    def print(s: String, highlighted: Boolean = false, yellow: Boolean = false) {
      val foregroundColour = if (yellow) DefaultColours.Yellow else DefaultColour
      line += s.style(Style(inverse = highlighted, foregroundColour = foregroundColour))
    }

    def getLines: Seq[Line] = lines :+ Line(line)

    def currentColumn: Int = line.size

  }

  private def print(printer: Printer, node: ObjectTreeNode): Unit = {
    val prefix = ""
    val currentPath = ObjectTreePath(Seq())
    node match {
      case ObjectTreeNode.List(Seq(), _)    ⇒ printer.println("[]")
      case ObjectTreeNode.List(items, _)    ⇒ printList(printer, items, prefix, currentPath, connectUp = true)
      case ObjectTreeNode.Object(Seq(), _)  ⇒ printer.println("{}")
      case ObjectTreeNode.Object(values, _) ⇒ printObject(printer, values, prefix, currentPath, connectUp = true)
      case ObjectTreeNode.Leaf(value, _)    ⇒ printer.println(value)
    }
  }

  private def printList(printer: Printer, nodes: Seq[ObjectTreeNode], prefix: String, currentPath: ObjectTreePath, connectUp: Boolean) {
    for ((node, index) ← nodes.zipWithIndex) {
      val itemPath = currentPath.descend(index)
      val isLastNode = index == nodes.length - 1
      val (nodePrefix, connector) = getPrefixAndConnector(index, nodes, connectUp, prefix)
      printer.print(nodePrefix)
      printer.print(connector, highlighted = isSelected(itemPath))
      val nestingPrefix = prefix + (if (isLastNode) "   " else "│  ")
      node match {
        case ObjectTreeNode.Leaf(value, _)      ⇒
          printer.print("─ ")
          val truncatedValue = StringUtils.ellipsisise(value, 0 max terminalSize.columns - printer.currentColumn)
          printer.print(truncatedValue, highlighted = isSelected(itemPath.ontoValue))
          printer.println()
        case ObjectTreeNode.List(Seq(), _)      ⇒
          printer.print("─ ")
          printer.print("[]", highlighted = isSelected(itemPath.ontoValue))
          printer.println()
        case ObjectTreeNode.Object(Seq(), _)    ⇒
          printer.print("─ ")
          printer.print("{}", highlighted = isSelected(itemPath.ontoValue))
          printer.println()
        case ObjectTreeNode.List(childNodes, _) ⇒
          printer.print(s"──")
          printList(printer, childNodes, nestingPrefix, itemPath, connectUp = false)
        case ObjectTreeNode.Object(values, _)   ⇒
          printer.print(s"──")
          printObject(printer, values, nestingPrefix, itemPath, connectUp = false)
      }
    }
  }

  private def isSelected(path: ObjectTreePath) = selectionPathOpt contains path

  private def printObject(printer: Printer, nodes: Seq[(String, ObjectTreeNode)], prefix: String, currentPath: ObjectTreePath, connectUp: Boolean) {
    for (((field, node), index) ← nodes.zipWithIndex) {
      val itemPath = currentPath.descend(field)
      val isLastNode = index == nodes.length - 1
      val (nodePrefix, connector) = getPrefixAndConnector(index, nodes.map(_._2), connectUp, prefix)
      printer.print(nodePrefix)
      printer.print(connector, highlighted = isSelected(itemPath))
      val fieldPath = itemPath.ontoFieldLabel
      printer.print("─ ")
      printer.print(field, yellow = true, highlighted = isSelected(fieldPath))
      printer.print(":")
      val nestingPrefix = prefix + (if (isLastNode) "   " else "│  ")
      node match {
        case ObjectTreeNode.Leaf(value, _)      ⇒
          printer.print(" ")
          val truncatedValue = StringUtils.ellipsisise(value, 0 max terminalSize.columns - printer.currentColumn)
          printer.print(truncatedValue, highlighted = isSelected(fieldPath.ontoValue))
          printer.println()
        case ObjectTreeNode.List(Seq(), _)      ⇒
          printer.print(" ")
          printer.print("[]", highlighted = isSelected(fieldPath.ontoValue))
          printer.println()
        case ObjectTreeNode.Object(Seq(), _)    ⇒
          printer.print(" ")
          printer.print("{}", highlighted = isSelected(fieldPath.ontoValue))
          printer.println()
        case ObjectTreeNode.List(childNodes, _) ⇒
          printer.println()
          printer.print(nestingPrefix)
          printList(printer, childNodes, nestingPrefix, fieldPath, connectUp = true)
        case ObjectTreeNode.Object(values, _)   ⇒
          printer.println()
          printer.print(nestingPrefix)
          printObject(printer, values, nestingPrefix, fieldPath, connectUp = true)
      }
    }
  }

  private def getPrefixAndConnector(index: Int,
                                    nodes: Seq[ObjectTreeNode],
                                    connectUp: Boolean,
                                    prefix: String): (String, String) = {
    val isFirstNode = index == 0
    val isLastNode = index == nodes.length - 1
    if (isFirstNode) {
      val connector =
        if (prefix.isEmpty) {
          if (nodes.size > 1) "┌" else "─"
        } else {
          if (connectUp) {
            if (isLastNode) "└" else "├"
          } else {
            if (isLastNode) "─" else "┬"
          }
        }
      ("", connector)
    } else {
      (prefix, if (isLastNode) "└" else "├")
    }
  }
  
}
