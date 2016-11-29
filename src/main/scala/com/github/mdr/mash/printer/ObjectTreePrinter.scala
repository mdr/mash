package com.github.mdr.mash.printer

import java.io.PrintStream

import com.github.mdr.mash.runtime._
import com.github.mdr.mash.terminal.TerminalInfo
import org.fusesource.jansi.Ansi

class ObjectTreePrinter(output: PrintStream, terminalInfo: TerminalInfo, viewConfig: ViewConfig) {

  def printObject(obj: MashValue) {
    val model = new ObjectTreeModelCreator(viewConfig).create(obj)
    val root = model.root
    print(root, prefix = "")
  }

  private def print(node: ObjectTreeNode, prefix: String): Unit = node match {
    case ObjectTreeNode.List(Seq(), _)    => output.println("[]")
    case ObjectTreeNode.List(items, _)    => printList(items, prefix, connectUp = true)
    case ObjectTreeNode.Object(Seq(), _)  => output.println("{}")
    case ObjectTreeNode.Object(values, _) => printObject(values, prefix, connectUp = true)
    case ObjectTreeNode.Leaf(value, _)    => output.println(value)
  }

  private def printList2(nodes: Seq[ObjectTreeNode], prefix: String, connectUp: Boolean) {
    for ((node, index) <- nodes.zipWithIndex) {
      val isFirstNode = index == 0
      val isLastNode = index == nodes.length - 1
      val stuff = getStuff(index, nodes, connectUp, prefix)
      output.print(s"$stuff─ $index:")
      val nestingPrefix = prefix + (if (isLastNode) "   " else "│  ")
      node match {
        case ObjectTreeNode.Leaf(value, _)      =>
          output.println(s" $value")
        case ObjectTreeNode.List(Seq(), _)      =>
          output.println(" []")
        case ObjectTreeNode.Object(Seq(), _)    =>
          output.println(" {}")
        case ObjectTreeNode.List(childNodes, _) =>
          output.print(s"\n$nestingPrefix")
          printList(childNodes, nestingPrefix, connectUp = true)
        case ObjectTreeNode.Object(values, _)   =>
          output.print(s"\n$nestingPrefix")
          printObject(values, nestingPrefix, connectUp = true)
      }
    }
  }

  private def printList(nodes: Seq[ObjectTreeNode], prefix: String, connectUp: Boolean) {
    for ((node, index) <- nodes.zipWithIndex) {
      val isLastNode = index == nodes.length - 1
      val stuff = getStuff(index, nodes, connectUp, prefix)
      val nestingPrefix = prefix + (if (isLastNode) "   " else "│  ")
      node match {
        case ObjectTreeNode.Leaf(value, _)      =>
          output.println(s"$stuff─ $value")
        case ObjectTreeNode.List(Seq(), _)      =>
          output.println(s"$stuff─ []")
        case ObjectTreeNode.Object(Seq(), _)    =>
          output.println(s"$stuff─ {}")
        case ObjectTreeNode.List(childNodes, _) =>
          output.print(s"$stuff──")
          printList(childNodes, nestingPrefix, connectUp = false)
        case ObjectTreeNode.Object(values, _)   =>
          output.print(s"$stuff──")
          printObject(values, nestingPrefix, connectUp = false)
      }
    }
  }

  private def printObject(nodes: Seq[(String, ObjectTreeNode)], prefix: String, connectUp: Boolean) {
    for (((field, node), index) <- nodes.zipWithIndex) {
      val isLastNode = index == nodes.length - 1
      val stuff = getStuff(index, nodes.map(_._2), connectUp, prefix)
      output.print(s"$stuff─ ")
      output.print(Ansi.ansi().fg(Ansi.Color.YELLOW).bold.a(field).reset())
      output.print(":")
      val nestingPrefix = prefix + (if (isLastNode) "   " else "│  ")
      node match {
        case ObjectTreeNode.Leaf(value, _)      =>
          output.println(s" $value")
        case ObjectTreeNode.List(Seq(), _)      =>
          output.println(" []")
        case ObjectTreeNode.Object(Seq(), _)    =>
          output.println(" {}")
        case ObjectTreeNode.List(childNodes, _) =>
          output.print(s"\n$nestingPrefix")
          printList(childNodes, nestingPrefix, connectUp = true)
        case ObjectTreeNode.Object(values, _)   =>
          output.print(s"\n$nestingPrefix")
          printObject(values, nestingPrefix, connectUp = true)
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
