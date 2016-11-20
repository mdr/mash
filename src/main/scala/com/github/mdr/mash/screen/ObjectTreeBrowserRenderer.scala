package com.github.mdr.mash.screen

import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.printer.ObjectTreeNode
import com.github.mdr.mash.repl.{ ObjectTreeBrowserState, ObjectsTableBrowserState }
import com.github.mdr.mash.terminal.TerminalInfo
import org.fusesource.jansi.Ansi
import com.github.mdr.mash.screen.Style.StylableString

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
    print(printer, state.model.root, prefix = "")
    printer.getLines
  }

  class Printer() {

    private val lines: ArrayBuffer[Line] = ArrayBuffer()

    private var line: Seq[StyledCharacter] = Seq()

    def printYellow(s: String): Unit = {
      line ++= s.style(Style(foregroundColour = Colour.Yellow))
    }

    def println(s: String): Unit = {
      line ++= s.style
      lines += Line(line)
      line = Seq()
    }

    def print(s: String): Unit = {
      line ++= s.style
    }

    def getLines = lines :+ Line(line)

  }

  private def print(printer: Printer, node: ObjectTreeNode, prefix: String): Unit = node match {
    case ObjectTreeNode.List(Seq(), _)    => printer.println("[]")
    case ObjectTreeNode.List(items, _)    => printList(printer, items, prefix, connectUp = true)
    case ObjectTreeNode.Object(Seq(), _)  => printer.println("{}")
    case ObjectTreeNode.Object(values, _) => printObject(printer, values, prefix, connectUp = true)
    case ObjectTreeNode.Leaf(value, _)    => printer.println(value)
  }

  private def printList(printer: Printer, nodes: Seq[ObjectTreeNode], prefix: String, connectUp: Boolean) {
    for ((node, index) <- nodes.zipWithIndex) {
      val isLastNode = index == nodes.length - 1
      val stuff = getStuff(index, nodes, connectUp, prefix)
      val nestingPrefix = prefix + (if (isLastNode) "   " else "│  ")
      node match {
        case ObjectTreeNode.Leaf(value, _)      =>
          printer.println(s"$stuff─ $value")
        case ObjectTreeNode.List(Seq(), _)      =>
          printer.println(s"$stuff─ []")
        case ObjectTreeNode.Object(Seq(), _)    =>
          printer.println(s"$stuff─ {}")
        case ObjectTreeNode.List(childNodes, _) =>
          printer.print(s"$stuff──")
          printList(printer, childNodes, nestingPrefix, connectUp = false)
        case ObjectTreeNode.Object(values, _)   =>
          printer.print(s"$stuff──")
          printObject(printer, values, nestingPrefix, connectUp = false)
      }
    }
  }

  private def printObject(printer: Printer, nodes: Seq[(String, ObjectTreeNode)], prefix: String, connectUp: Boolean) {
    for (((field, node), index) <- nodes.zipWithIndex) {
      val isLastNode = index == nodes.length - 1
      val stuff = getStuff(index, nodes.map(_._2), connectUp, prefix)
      printer.print(s"$stuff─ ")
      printer.printYellow(field)
      printer.print(":")
      val nestingPrefix = prefix + (if (isLastNode) "   " else "│  ")
      node match {
        case ObjectTreeNode.Leaf(value, _)      =>
          printer.println(s" $value")
        case ObjectTreeNode.List(Seq(), _)      =>
          printer.println(" []")
        case ObjectTreeNode.Object(Seq(), _)    =>
          printer.println(" {}")
        case ObjectTreeNode.List(childNodes, _) =>
          printer.print(s"\n$nestingPrefix")
          printList(printer, childNodes, nestingPrefix, connectUp = true)
        case ObjectTreeNode.Object(values, _)   =>
          printer.print(s"\n$nestingPrefix")
          printObject(printer, values, nestingPrefix, connectUp = true)
      }
    }
  }

  private def getStuff(index: Int, nodes: Seq[ObjectTreeNode], connectUp: Boolean, prefix: String): String = {
    val isFirstNode = index == 0
    val isLastNode = index == nodes.length - 1
    if (isFirstNode) {
      if (prefix.isEmpty) {
        if (nodes.size > 1) "┌" else "─"
      } else {
        if (connectUp) {
          if (isLastNode) "└" else "├"
        } else {
          if (isLastNode) "─" else "┬"
        }
      }
    } else {
      if (isLastNode) s"$prefix└" else s"$prefix├"
    }
  }


}
