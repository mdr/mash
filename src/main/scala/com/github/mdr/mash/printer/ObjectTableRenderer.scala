package com.github.mdr.mash.printer

import com.github.mdr.mash.evaluator.Evaluator
import com.github.mdr.mash.evaluator.MemberEvaluator
import com.github.mdr.mash.ns.collections.GroupClass
import com.github.mdr.mash.ns.git.CommitClass
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.terminal.TerminalInfo
import com.github.mdr.mash.utils.StringUtils

object ObjectTableRenderer {

  private val IndexColumnName = "#"

}

class ObjectTableRenderer(terminalInfo: TerminalInfo, showSelections: Boolean = false) {
  import ObjectTableRenderer._

  private val boxCharacterSupplier: BoxCharacterSupplier = UnicodeBoxCharacterSupplier

  def renderObjects(objects: Seq[MashObject]): ObjectTableModel = {
    val columns = getColumnSpecs(objects)

    val renderedObjects: Seq[ObjectTableRow] =
      objects.zipWithIndex.map { case (obj, i) ⇒ renderObject(obj, i, columns) }

    def desiredColumnWidth(member: String): Int = (renderedObjects.map(_.data(member)) :+ member).map(_.size).max
    val requestedColumnWidths: Map[ColumnSpec, Int] = (for (c ← columns) yield (c -> desiredColumnWidth(c.name))).toMap

    val columnNames = IndexColumnName +: columns.map(_.name)
    val indexColumnWidth = objects.size.toString.length
    val selectionStateWidth = if (showSelections) 2 else 0
    val totalAvailableWidth = terminalInfo.columns - indexColumnWidth - 1 - (columns.size + 1) - selectionStateWidth // accounting for the table and column borders
    val columnWidths =
      (for ((c, w) ← ColumnAllocator.allocateColumns(columns, requestedColumnWidths, totalAvailableWidth))
        yield c.name -> w) + (IndexColumnName -> indexColumnWidth)

    ObjectTableModel(columnNames, columnWidths, renderedObjects)
  }

  private def renderObject(obj: MashObject, index: Int, columns: Seq[ColumnSpec]): ObjectTableRow = {
    val pairs =
      for {
        ColumnSpec(name, _, isNullaryMethod) ← columns
        rawValue = MemberEvaluator.lookup(obj, name)
        value = if (isNullaryMethod) Evaluator.immediatelyResolveNullaryFunctions(rawValue) else rawValue
        renderedValue = Printer.renderField(value, inCell = true)
      } yield name -> renderedValue
    val data = (pairs :+ (IndexColumnName -> index.toString)).toMap
    ObjectTableRow(data)
  }

  def renderTopRow(model: ObjectTableModel): String = {
    import boxCharacterSupplier._
    val sb = new StringBuilder()
    sb.append(doubleTopLeft)
    if (showSelections)
      sb.append(doubleHorizontal + doubleHorizontalSingleDown)
    sb.append(model.columnNames.map(name ⇒ doubleHorizontal * model.columnWidth(name)).mkString(doubleHorizontalSingleDown))
    sb.append(doubletopRight)
    sb.toString
  }

  def renderHeaderRow(model: ObjectTableModel): String = {
    import boxCharacterSupplier._
    def renderColumn(name: String) = StringUtils.fitToWidth(name, model.columnWidth(name))
    val sb = new StringBuilder()
    sb.append(doubleVertical)
    if (showSelections)
      sb.append(" " + singleVertical)
    sb.append(model.columnNames.map(renderColumn).mkString(singleVertical))
    sb.append(doubleVertical)
    sb.toString
  }

  def renderBelowHeaderRow(model: ObjectTableModel): String = {
    import boxCharacterSupplier._
    val sb = new StringBuilder()
    sb.append(doubleVerticalSingleRight)
    if (showSelections)
      sb.append(singleHorizontal + singleIntersect)
    sb.append(model.columnNames.map(name ⇒ singleHorizontal * model.columnWidth(name)).mkString(singleIntersect))
    sb.append(doubleVerticalSingleLeft)
    sb.toString
  }

  def renderObjectRow(model: ObjectTableModel, renderedObject: ObjectTableRow): String = {
    import boxCharacterSupplier._
    def renderCell(name: String) = StringUtils.fitToWidth(renderedObject.data(name), model.columnWidth(name))
    new StringBuilder()
      .append(doubleVertical)
      .append(model.columnNames.map(renderCell).mkString(singleVertical))
      .append(doubleVertical)
      .toString
  }

  def renderBottomRow(model: ObjectTableModel): String = {
    import boxCharacterSupplier._
    val sb = new StringBuilder()
    sb.append(doubleBottomLeft)
    if (showSelections)
      sb.append(doubleHorizontal + doubleHorizontalSingleUp)
    sb.append(model.columnNames.map(name ⇒ doubleHorizontal * model.columnWidths(name)).mkString(doubleHorizontalSingleUp))
    sb.append(doubleBottomRight)
    sb.toString
  }

  private def getColumnSpecs(objects: Seq[MashObject]): Seq[ColumnSpec] = {
    val testObjects = objects.take(10)
    if (testObjects.forall(_.classOpt == Some(GroupClass)))
      Seq(
        ColumnSpec(GroupClass.Fields.Key.name, weight = 10),
        ColumnSpec(GroupClass.CountMethod.name, weight = 3, isNullaryMethod = true),
        ColumnSpec(GroupClass.Fields.Values.name, weight = 1))
    else if (testObjects.forall(_.classOpt == Some(CommitClass)))
      Seq(
        ColumnSpec(CommitClass.Fields.Hash.name, weight = 1),
        ColumnSpec(CommitClass.Fields.CommitTime.name, weight = 10),
        ColumnSpec(CommitClass.Fields.Author.name, weight = 10),
        ColumnSpec(CommitClass.Fields.Summary.name, weight = 3))
    else
      testObjects.flatMap(_.fields.keySet).distinct.map(field ⇒ ColumnSpec(field))
  }
}