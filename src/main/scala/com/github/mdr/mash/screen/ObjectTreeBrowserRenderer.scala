package com.github.mdr.mash.screen

import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.printer.ObjectTreeNode
import com.github.mdr.mash.repl.{ ObjectTreeBrowserState, ObjectTreePath }
import com.github.mdr.mash.screen.Style.StylableString
import com.github.mdr.mash.terminal.TerminalInfo

import scala.collection.mutable.ArrayBuffer

case class ObjectTreeBrowserRenderer(state: ObjectTreeBrowserState, terminalInfo: TerminalInfo) {

  private val fileSystem = LinuxFileSystem

  def renderObjectBrowser: Screen = {
    val lines = renderLines
    val title = "mash " + fileSystem.pwd.toString
    Screen(lines, cursorPos = Point(0, 0), cursorVisible = false, title = title)
  }

  private def renderLines: Seq[Line] = {
    val printer = new Printer()
    print(printer, state.model.root, prefix = "", currentPath = ObjectTreePath())
    printer.getLines.take(terminalInfo.rows)
  }

  class Printer() {

    private val lines: ArrayBuffer[Line] = ArrayBuffer()

    private var line: Seq[StyledCharacter] = Seq()

    def printYellow(s: String): Unit = {
      line ++= s.style(Style(foregroundColour = Colour.Yellow))
    }

    def println(): Unit = {
      lines += Line(line)
      line = Seq()
    }

    def println(s: String): Unit = {
      line ++= s.style
      println()
    }

    def print(s: String): Unit = {
      line ++= s.style
    }

    def printHighlighted(s: String): Unit = {
      line ++= s.style(Style(inverse = true))
    }

    def getLines = lines :+ Line(line)

  }

  private def print(printer: Printer, node: ObjectTreeNode, prefix: String, currentPath: ObjectTreePath): Unit = node match {
    case ObjectTreeNode.List(Seq(), _)    => printer.println("[]")
    case ObjectTreeNode.List(items, _)    => printList(printer, items, prefix, currentPath, connectUp = true)
    case ObjectTreeNode.Object(Seq(), _)  => printer.println("{}")
    case ObjectTreeNode.Object(values, _) => printObject(printer, values, prefix, currentPath, connectUp = true)
    case ObjectTreeNode.Leaf(value, _)    => printer.println(value)
  }

  private def printList(printer: Printer, nodes: Seq[ObjectTreeNode], prefix: String, currentPath: ObjectTreePath, connectUp: Boolean) {
    for ((node, index) <- nodes.zipWithIndex) {
      val isLastNode = index == nodes.length - 1
      val (nodePrefix, connector) = getStuff(index, nodes, connectUp, prefix)
      printer.print(nodePrefix)
      if (currentPath == state.selectionPath && index == 0)
        printer.printHighlighted(connector)
      else
        printer.print(connector)
      val nestingPrefix = prefix + (if (isLastNode) "   " else "│  ")
      node match {
        case ObjectTreeNode.Leaf(value, _)      =>
          printer.print("─ ")
          if (currentPath.descend(index) == state.selectionPath)
            printer.printHighlighted(value)
          else
            printer.print(value)
          printer.println()
        case ObjectTreeNode.List(Seq(), _)      =>
          printer.println(s"─ []")
        case ObjectTreeNode.Object(Seq(), _)    =>
          printer.println(s"─ {}")
        case ObjectTreeNode.List(childNodes, _) =>
          printer.print(s"──")
          printList(printer, childNodes, nestingPrefix, currentPath.descend(index), connectUp = false)
        case ObjectTreeNode.Object(values, _)   =>
          printer.print(s"──")
          printObject(printer, values, nestingPrefix, currentPath.descend(index), connectUp = false)
      }
    }
  }

  private def printObject(printer: Printer, nodes: Seq[(String, ObjectTreeNode)], prefix: String, currentPath: ObjectTreePath, connectUp: Boolean) {
    for (((field, node), index) <- nodes.zipWithIndex) {
      val isLastNode = index == nodes.length - 1
      val (nodePrefix, connector) = getStuff(index, nodes.map(_._2), connectUp, prefix)
      printer.print(nodePrefix)
      if (currentPath == state.selectionPath && index == 0)
        printer.printHighlighted(connector)
      else
        printer.print(connector)
      printer.print("─ ")
      printer.printYellow(field)
      printer.print(":")
      val nestingPrefix = prefix + (if (isLastNode) "   " else "│  ")
      node match {
        case ObjectTreeNode.Leaf(value, _)      =>
          printer.print(" ")
          if (currentPath.descend(field) == state.selectionPath)
            printer.printHighlighted(value)
          else
            printer.print(value)
          printer.println()
        case ObjectTreeNode.List(Seq(), _)      =>
          printer.println(" []")
        case ObjectTreeNode.Object(Seq(), _)    =>
          printer.println(" {}")
        case ObjectTreeNode.List(childNodes, _) =>
          printer.println()
          printer.print(s"$nestingPrefix")
          printList(printer, childNodes, nestingPrefix, currentPath.descend(field), connectUp = true)
        case ObjectTreeNode.Object(values, _)   =>
          printer.println()
          printer.print(s"$nestingPrefix")
          printObject(printer, values, nestingPrefix, currentPath.descend(field), connectUp = true)
      }
    }
  }

  private def getStuff(index: Int, nodes: Seq[ObjectTreeNode], connectUp: Boolean, prefix: String): (String, String) = {
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
