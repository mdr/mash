package com.github.mdr.mash.repl

import com.github.mdr.mash.printer.{ ObjectTreeModel, ObjectTreeNode }
import com.github.mdr.mash.repl.ObjectTreeChoice.{ FieldChoice, IndexChoice, OntoFieldLabel, OntoValue }
import com.github.mdr.mash.runtime.MashValue

import scala.annotation.tailrec
import scala.collection.immutable.ListMap

sealed trait ObjectTreeChoice

object ObjectTreeChoice {

  case class FieldChoice(fieldName: String) extends ObjectTreeChoice

  case class IndexChoice(i: Int) extends ObjectTreeChoice

  case object OntoFieldLabel extends ObjectTreeChoice

  case object OntoValue extends ObjectTreeChoice

}

case class ObjectTreePath(choices: Seq[ObjectTreeChoice] = Seq()) {

  def ascend = if (choices.isEmpty) this else ObjectTreePath(choices.init)

  private def addChoice(choice: ObjectTreeChoice) = ObjectTreePath(choices :+ choice)

  def descend(index: Int) = addChoice(IndexChoice(index))

  def descend(field: String) = addChoice(FieldChoice(field))

  def ontoValue = addChoice(OntoValue)

  def ontoFieldLabel = addChoice(OntoFieldLabel)

  def depth = choices.size

}

object ObjectTreeBrowserState {

  def initial(model: ObjectTreeModel, path: String): ObjectTreeBrowserState = {
    val choice = model.root match {
      case ObjectTreeNode.List(items, _) if items.nonEmpty   => IndexChoice(0)
      case ObjectTreeNode.Object(items, _) if items.nonEmpty => FieldChoice(items.head._1)
      case _                                                 => throw new RuntimeException("Cannot initialise object tree browser on: " + model)
    }
    ObjectTreeBrowserState(model, ObjectTreePath(Seq(choice)), path, 0)
  }

}

case class ObjectTreeBrowserState(model: ObjectTreeModel,
                                  selectionPath: ObjectTreePath,
                                  path: String,
                                  firstRow: Int) extends BrowserState {


  def getSelectedValue: MashValue = getNode(selectionPath).rawValue

  def getNewPath: String = {
    val sb = new StringBuilder(path)
    @tailrec
    def rec(choices: Seq[ObjectTreeChoice]): Unit = {
      if (choices.nonEmpty) {
        choices.head match {
          case ObjectTreeChoice.IndexChoice(i)     => sb.append(s"[$i]")
          case ObjectTreeChoice.FieldChoice(field) => sb.append(s".$field")
          case _                                   =>
        }
        rec(choices.tail)
      }
    }
    rec(selectionPath.choices)
    sb.toString
  }

  def adjustFirstRow(delta: Int): ObjectTreeBrowserState = copy(firstRow = firstRow + delta)

  case class ScanData(depths: ListMap[ObjectTreePath, Int], rows: Map[ObjectTreePath, Int])

  private class DepthScan() {
    private var depths: ListMap[ObjectTreePath, Int] = ListMap()
    private var row: Int = 0
    private var rows: Map[ObjectTreePath, Int] = Map()

    def scan: ScanData = {
      scan(model.root, ObjectTreePath(Seq()), depth = 0)
      ScanData(depths, rows)
    }

    private def scan(node: ObjectTreeNode, path: ObjectTreePath, depth: Int): Unit = node match {

      case ObjectTreeNode.List(values, _) if values.nonEmpty =>
        for ((value, index) <- values.zipWithIndex) {
          val itemPath = path.descend(index)
          rows += (itemPath -> row)
          depths += (itemPath -> depth)
          value match {
            case ObjectTreeNode.Leaf(_, _)       =>
              scan(value, itemPath, depth)
            case ObjectTreeNode.Object(Seq(), _) =>
              scan(value, itemPath, depth)
            case ObjectTreeNode.List(Seq(), _)   =>
              scan(value, itemPath, depth)
            case _                               =>
              scan(value, itemPath, depth + 1)
          }
        }

      case ObjectTreeNode.List(values, _) =>
        rows += (path.ontoValue -> row)
        row += 1
        depths += path.ontoValue -> (depth + 1)

      case ObjectTreeNode.Object(values, _) if values.nonEmpty =>
        for ((field, value) <- values) {
          val itemPath = path.descend(field)
          rows += (itemPath -> row)
          depths += (itemPath -> depth)
          val labelPath = itemPath.ontoFieldLabel
          rows += (labelPath -> row)
          value match {
            case ObjectTreeNode.Leaf(_, _)       =>
            case ObjectTreeNode.Object(Seq(), _) =>
            case ObjectTreeNode.List(Seq(), _)   =>
            case _                               =>
              row += 1
          }
          depths += (labelPath -> (depth + 1))
          scan(value, labelPath, depth + 1)
        }

      case ObjectTreeNode.Object(values, _) =>
        rows += (path.ontoValue -> row)
        row += 1
        depths += path.ontoValue -> (depth + 1)

      case ObjectTreeNode.Leaf(_, _) =>
        rows += (path.ontoValue -> row)
        row += 1
        depths += path.ontoValue -> (depth + 1)

    }

  }

  def rawValue: MashValue = model.rawValue

  private def getNode(node: ObjectTreeNode, path: Seq[ObjectTreeChoice]): ObjectTreeNode = (node, path) match {
    case (_, Seq())                                                           => node
    case (_, Seq(OntoFieldLabel, tail@_*))                                    => getNode(node, tail)
    case (_, Seq(OntoValue, tail@_*))                                         => node
    case (ObjectTreeNode.List(values, _), Seq(IndexChoice(i), tail@_*))       => getNode(values(i), tail)
    case (ObjectTreeNode.Object(values, _), Seq(FieldChoice(field), tail@_*)) =>
      getNode(values.collectFirst { case (`field`, node) => node }.get, tail)
  }

  private def getNode(path: ObjectTreePath): ObjectTreeNode =
    getNode(model.root, path.choices)

  def right: ObjectTreeBrowserState = selectionPath.choices.last match {
    case FieldChoice(_) => copy(selectionPath = selectionPath.ontoFieldLabel)
    case IndexChoice(_) =>
      getNode(selectionPath) match {
        case ObjectTreeNode.List(items, _) if items.nonEmpty   => copy(selectionPath = selectionPath.descend(0))
        case ObjectTreeNode.Object(items, _) if items.nonEmpty => copy(selectionPath = selectionPath.descend(items.head._1))
        case _                                                 => copy(selectionPath = selectionPath.ontoValue)
      }
    case OntoFieldLabel =>
      getNode(selectionPath) match {
        case ObjectTreeNode.List(items, _) if items.nonEmpty   => copy(selectionPath = selectionPath.descend(0))
        case ObjectTreeNode.Object(items, _) if items.nonEmpty => copy(selectionPath = selectionPath.descend(items.head._1))
        case _                                                 => copy(selectionPath = selectionPath.ontoValue)
      }
    case OntoValue      => this
  }

  def left: ObjectTreeBrowserState = copy(selectionPath =
    if (selectionPath.choices.size > 1) selectionPath.ascend else selectionPath)

  def selectedRow = new DepthScan().scan.rows(selectionPath)

  def upDown(delta: Int): ObjectTreeBrowserState = {
    val depths = new DepthScan().scan.depths
    //    for ((path, depth) <- depths.toSeq)
    //      println(path, depth)

    val selectDepth = depths(selectionPath)
    val allPaths =
      for ((path, depth) <- depths.toSeq if depth == selectDepth)
        yield path
    val i = allPaths.indexOf(selectionPath)
    copy(selectionPath = allPaths((i + delta + allPaths.size) % allPaths.size))
  }

  def down: ObjectTreeBrowserState = upDown(1)

  def up: ObjectTreeBrowserState = upDown(-1)

  def withPath(newPath: String): ObjectTreeBrowserState = copy(path = newPath)

}
