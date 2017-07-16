package com.github.mdr.mash.screen.browser

import com.github.mdr.mash.printer.model.ObjectTreeNode
import com.github.mdr.mash.repl.browser.{ ObjectTreeBrowserState, ObjectTreePath }
import com.github.mdr.mash.screen.Style.StylableString
import com.github.mdr.mash.screen.{ KeyHint, Style, _ }
import com.github.mdr.mash.utils.{ Dimensions, StringUtils }
import com.github.mdr.mash.utils.Utils._

import scala.collection.mutable.ArrayBuffer

case class ObjectTreeBrowserRenderer(state: ObjectTreeBrowserState, terminalSize: Dimensions)
  extends AbstractBrowserRenderer(state, terminalSize) {

  protected def renderLines: Seq[Line] = {
    val printer = new Printer
    print(printer, state.model.root)
    val treeLines = printer.getLines.window(state.firstRow, windowSize)
    combineUpperStatusLines(renderUpperStatusLines, treeLines :+ renderStatusLine)
  }

  private class Printer() {

    private val lines: ArrayBuffer[Line] = ArrayBuffer()

    private var line: StyledString = StyledString.empty

    def println(): Unit = {
      lines += Line(line)
      line = StyledString.empty
    }

    def println(s: String): Unit = {
      line += s.style
      println()
    }

    def print(s: String, highlighted: Boolean = false, yellow: Boolean = false): Unit = {
      val foregroundColour = if (yellow) BasicColour.Yellow else DefaultColour
      line += s.style(Style(inverse = highlighted, foregroundColour = foregroundColour))
    }

    def getLines = lines :+ Line(line)

    def currentColumn = line.size

  }

  private def renderRegularStatusLine: Line = {
    import KeyHint._
    val hints = Seq(Exit, Back, Focus, Insert, InsertWhole, Table, Open, Copy)
    Line(renderKeyHints(hints))
  }

  private def renderStatusLine: Line =
    state.expressionStateOpt match {
      case Some(expressionState) ⇒ StatusLineRenderers.renderExpressionInputStatusLine
      case None                  ⇒ renderRegularStatusLine
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
    for ((node, index) <- nodes.zipWithIndex) {
      val itemPath = currentPath.descend(index)
      val isLastNode = index == nodes.length - 1
      val (nodePrefix, connector) = getPrefixAndConnector(index, nodes, connectUp, prefix)
      printer.print(nodePrefix)
      printer.print(connector, highlighted = itemPath == state.selectionPath)
      val nestingPrefix = prefix + (if (isLastNode) "   " else "│  ")
      node match {
        case ObjectTreeNode.Leaf(value, _)      ⇒
          printer.print("─ ")
          val truncatedValue = StringUtils.ellipsisise(value, math.max(terminalSize.columns - printer.currentColumn, 0))
          printer.print(truncatedValue, highlighted = itemPath.ontoValue == state.selectionPath)
          printer.println()
        case ObjectTreeNode.List(Seq(), _)      ⇒
          printer.print("─ ")
          printer.print("[]", highlighted = itemPath.ontoValue == state.selectionPath)
          printer.println()
        case ObjectTreeNode.Object(Seq(), _)    ⇒
          printer.print("─ ")
          printer.print("{}", highlighted = itemPath.ontoValue == state.selectionPath)
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

  private def printObject(printer: Printer, nodes: Seq[(String, ObjectTreeNode)], prefix: String, currentPath: ObjectTreePath, connectUp: Boolean) {
    for (((field, node), index) <- nodes.zipWithIndex) {
      val itemPath = currentPath.descend(field)
      val isLastNode = index == nodes.length - 1
      val (nodePrefix, connector) = getPrefixAndConnector(index, nodes.map(_._2), connectUp, prefix)
      printer.print(nodePrefix)
      printer.print(connector, highlighted = itemPath == state.selectionPath)
      val fieldPath = itemPath.ontoFieldLabel
      printer.print("─ ")
      printer.print(field, yellow = true, highlighted = fieldPath == state.selectionPath)
      printer.print(":")
      val nestingPrefix = prefix + (if (isLastNode) "   " else "│  ")
      node match {
        case ObjectTreeNode.Leaf(value, _)      ⇒
          printer.print(" ")
          val truncatedValue = StringUtils.ellipsisise(value, math.max(terminalSize.columns - printer.currentColumn, 0))
          printer.print(truncatedValue, highlighted = fieldPath.ontoValue == state.selectionPath)
          printer.println()
        case ObjectTreeNode.List(Seq(), _)      ⇒
          printer.print(" ")
          printer.print("[]", highlighted = fieldPath.ontoValue == state.selectionPath)
          printer.println()
        case ObjectTreeNode.Object(Seq(), _)    ⇒
          printer.print(" ")
          printer.print("{}", highlighted = fieldPath.ontoValue == state.selectionPath)
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

  protected val windowSize = terminalSize.rows - 2 // two status lines

}
