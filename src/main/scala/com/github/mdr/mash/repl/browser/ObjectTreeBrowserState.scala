package com.github.mdr.mash.repl.browser

import com.github.mdr.mash.parser.ExpressionCombiner.combineSafely
import com.github.mdr.mash.printer.model.{ ObjectTreeModel, ObjectTreeNode }
import com.github.mdr.mash.repl.browser.ObjectTreeChoice.{ FieldChoice, IndexChoice, OntoFieldLabel, OntoValue }
import com.github.mdr.mash.runtime.MashValue

import scala.annotation.tailrec

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
      case ObjectTreeNode.List(items, _) if items.nonEmpty   ⇒ IndexChoice(0)
      case ObjectTreeNode.Object(items, _) if items.nonEmpty ⇒ FieldChoice(items.head._1)
      case _                                                 ⇒ throw new RuntimeException("Cannot initialise object tree browser on: " + model)
    }
    ObjectTreeBrowserState(model, ObjectTreePath(Seq(choice)), path, 0)
  }

}

case class ObjectTreeBrowserState(model: ObjectTreeModel,
                                  selectionPath: ObjectTreePath,
                                  path: String,
                                  firstRow: Int,
                                  expressionStateOpt: Option[ExpressionState] = None) extends BrowserState {

  private lazy val scanData = new DepthScan().scan

  def getSelectedValue: MashValue = getNode(selectionPath).rawValue

  def getNewPath: String = {
    @tailrec
    def rec(pathSoFar: String, choices: Seq[ObjectTreeChoice]): String =
      if (choices.nonEmpty) {
        val newPath = choices.head match {
          case ObjectTreeChoice.IndexChoice(i)     ⇒ combineSafely(pathSoFar, s"[$i]")
          case ObjectTreeChoice.FieldChoice(field) ⇒ combineSafely(pathSoFar, s".$field")
          case _                                   ⇒ pathSoFar
        }
        rec(newPath, choices.tail)
      } else
        pathSoFar
    rec(path, selectionPath.choices)
  }

  def adjustFirstRow(delta: Int): ObjectTreeBrowserState = copy(firstRow = firstRow + delta)

  case class ScanData(pathDepths: Map[ObjectTreePath, Int],
                      pathsByDepth: Map[Int, Seq[ObjectTreePath]],
                      rows: Map[ObjectTreePath, Int])

  private class DepthScan() {
    private var pathDepths: Map[ObjectTreePath, Int] = Map()
    private var pathsByDepth: Map[Int, Seq[ObjectTreePath]] = Map()
    private var row: Int = 0
    private var rows: Map[ObjectTreePath, Int] = Map()

    def scan: ScanData = {
      scan(model.root, ObjectTreePath(Seq()), depth = 0)
      ScanData(pathDepths, pathsByDepth, rows)
    }

    private def recordPathDepth(path: ObjectTreePath, depth: Int): Unit = {
      pathDepths += (path -> depth)
      pathsByDepth += (depth -> (pathsByDepth.getOrElse(depth, Seq()) :+ path))
    }

    private def scan(node: ObjectTreeNode, path: ObjectTreePath, depth: Int): Unit = node match {

      case ObjectTreeNode.List(values, _) if values.nonEmpty ⇒
        for ((value, index) ← values.zipWithIndex) {
          val itemPath = path.descend(index)
          rows += (itemPath -> row)
          recordPathDepth(itemPath, depth)
          value match {
            case ObjectTreeNode.Leaf(_, _)       ⇒
              scan(value, itemPath, depth)
            case ObjectTreeNode.Object(Seq(), _) ⇒
              scan(value, itemPath, depth)
            case ObjectTreeNode.List(Seq(), _)   ⇒
              scan(value, itemPath, depth)
            case _                               ⇒
              scan(value, itemPath, depth + 1)
          }
        }

      case ObjectTreeNode.List(values, _) ⇒
        rows += (path.ontoValue -> row)
        row += 1
        recordPathDepth(path.ontoValue, depth + 1)

      case ObjectTreeNode.Object(values, _) if values.nonEmpty ⇒
        for ((field, value) ← values) {
          val itemPath = path.descend(field)
          rows += (itemPath -> row)
          recordPathDepth(itemPath, depth)
          val labelPath = itemPath.ontoFieldLabel
          rows += (labelPath -> row)
          value match {
            case ObjectTreeNode.Leaf(_, _)       ⇒
            case ObjectTreeNode.Object(Seq(), _) ⇒
            case ObjectTreeNode.List(Seq(), _)   ⇒
            case _                               ⇒
              row += 1
          }
          recordPathDepth(labelPath, depth + 1)
          scan(value, labelPath, depth + 1)
        }

      case ObjectTreeNode.Object(values, _) ⇒
        rows += (path.ontoValue -> row)
        row += 1
        recordPathDepth(path.ontoValue, depth + 1)

      case ObjectTreeNode.Leaf(_, _) ⇒
        rows += (path.ontoValue -> row)
        row += 1
        recordPathDepth(path.ontoValue, depth + 1)

    }

  }

  def rawValue: MashValue = model.rawValue

  private def getNode(node: ObjectTreeNode, path: Seq[ObjectTreeChoice]): ObjectTreeNode = (node, path) match {
    case (_, Seq())                                                           ⇒ node
    case (_, Seq(OntoFieldLabel, tail@_*))                                    ⇒ getNode(node, tail)
    case (_, Seq(OntoValue, tail@_*))                                         ⇒ node
    case (ObjectTreeNode.List(values, _), Seq(IndexChoice(i), tail@_*))       ⇒ getNode(values(i), tail)
    case (ObjectTreeNode.Object(values, _), Seq(FieldChoice(field), tail@_*)) ⇒
      getNode(values.collectFirst { case (`field`, childNode) ⇒ childNode }.get, tail)
  }

  private def getNode(path: ObjectTreePath): ObjectTreeNode =
    getNode(model.root, path.choices)

  private def withSelectionPath(selectionPath: ObjectTreePath): ObjectTreeBrowserState =
    copy(selectionPath = selectionPath)

  def withPath(newPath: String): ObjectTreeBrowserState = copy(path = newPath)

  def right: ObjectTreeBrowserState = selectionPath.choices.last match {
    case FieldChoice(_) ⇒ withSelectionPath(selectionPath.ontoFieldLabel)
    case IndexChoice(_) ⇒
      getNode(selectionPath) match {
        case ObjectTreeNode.List(items, _) if items.nonEmpty   ⇒ withSelectionPath(selectionPath.descend(0))
        case ObjectTreeNode.Object(items, _) if items.nonEmpty ⇒ withSelectionPath(selectionPath.descend(items.head._1))
        case _                                                 ⇒ withSelectionPath(selectionPath.ontoValue)
      }
    case OntoFieldLabel ⇒
      getNode(selectionPath) match {
        case ObjectTreeNode.List(items, _) if items.nonEmpty   ⇒ withSelectionPath(selectionPath.descend(0))
        case ObjectTreeNode.Object(items, _) if items.nonEmpty ⇒ withSelectionPath(selectionPath.descend(items.head._1))
        case _                                                 ⇒ withSelectionPath(selectionPath.ontoValue)
      }
    case OntoValue      ⇒ this
  }

  def left: ObjectTreeBrowserState = withSelectionPath(
    if (selectionPath.choices.size > 1) selectionPath.ascend else selectionPath)

  def selectedRow = scanData.rows(selectionPath)

  private def upDown(delta: Int, terminalRows: Int): ObjectTreeBrowserState = {
    val depths = scanData.pathDepths

    val selectDepth = depths(selectionPath)
    val allPathsAtDepth = scanData.pathsByDepth(selectDepth)
    val i = allPathsAtDepth.indexOf(selectionPath)
    val path = allPathsAtDepth((i + delta + allPathsAtDepth.size) % allPathsAtDepth.size)
    withSelectionPath(path).adjustWindowToFit(terminalRows)
  }

  def nextItem(terminalRows: Int): ObjectTreeBrowserState = upDown(1, terminalRows)

  def previousItem(terminalRows: Int): ObjectTreeBrowserState = upDown(-1, terminalRows)

  override def selectionInfo: SelectionInfo = SelectionInfo(getNewPath, getSelectedValue)

  private def windowSize(terminalRows: Int) = terminalRows - 2 // 2 status rows

  private def adjustWindowToFit(terminalRows: Int): ObjectTreeBrowserState = {
    var newState = this

    val delta = selectedRow - (firstRow + windowSize(terminalRows) - 1)
    if (delta >= 0)
      newState = newState.adjustFirstRow(delta)

    val delta2 = firstRow - selectedRow
    if (delta2 >= 0)
      newState = newState.adjustFirstRow(-delta2)

    newState
  }

  def withExpressionState(expressionStateOpt: Option[ExpressionState]): BrowserState =
    copy(expressionStateOpt = expressionStateOpt)
}
