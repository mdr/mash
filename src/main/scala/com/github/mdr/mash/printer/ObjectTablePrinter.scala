package com.github.mdr.mash.printer

import scala.collection.immutable.ListMap
import com.github.mdr.mash.evaluator.Evaluator
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.evaluator.MemberEvaluator
import com.github.mdr.mash.ns.collections.GroupClass
import com.github.mdr.mash.utils.StringUtils
import com.github.mdr.mash.terminal.TerminalInfo
import java.io.PrintStream
import com.github.mdr.mash.ns.git.CommitClass

object ObjectTablePrinter {

  private val IndexColumnName = "#"

}

class ObjectTablePrinter(output: PrintStream, terminalInfo: TerminalInfo, alwaysUseBrowser: Boolean = false) {
  import ObjectTablePrinter._

  private val boxCharacterSupplier: BoxCharacterSupplier = UnicodeBoxCharacterSupplier
  private def printer = new Printer(output, terminalInfo)

  def printTable(objects: Seq[MashObject]): Option[ObjectTableModel] = {
    val model = renderObjects(objects)
    val nonDataRows = 3 /* header */ + 1 /* footer */
    if (alwaysUseBrowser || objects.size > terminalInfo.rows - nonDataRows)
      Some(model)
    else {
      printTable(model)
      None
    }
  }

  def renderObjects(objects: Seq[MashObject]): ObjectTableModel = {
    val columns = getColumnSpecs(objects)

    val renderedObjects: Seq[Map[String, String]] =
      objects.zipWithIndex.map { case (obj, i) ⇒ renderObject(obj, i, columns) }

    def desiredColumnWidth(member: String): Int = (renderedObjects.map(_(member)) :+ member).map(_.size).max
    val requestedColumnWidths: Map[ColumnSpec, Int] = (for (c ← columns) yield (c -> desiredColumnWidth(c.name))).toMap

    val columnNames = IndexColumnName +: columns.map(_.name)
    val indexColumnWidth = objects.size.toString.length
    val totalAvailableWidth = terminalInfo.columns - indexColumnWidth - 1 - (columns.size + 1) // accounting for the table and column borders
    val columnWidths =
      (for ((c, w) ← ColumnAllocator.allocateColumns(columns, requestedColumnWidths, totalAvailableWidth))
        yield c.name -> w) + (IndexColumnName -> indexColumnWidth)

    ObjectTableModel(columnNames, columnWidths, renderedObjects)
  }

  private def renderObject(obj: MashObject, index: Int, columns: Seq[ColumnSpec]): Map[String, String] = {
    val pairs =
      for {
        ColumnSpec(name, _, isNullaryMethod) ← columns
        rawValue = MemberEvaluator.lookup(obj, name)
        value = if (isNullaryMethod) Evaluator.immediatelyResolveNullaryFunctions(rawValue) else rawValue
        renderedValue = printer.renderField(value, inCell = true)
      } yield name -> renderedValue
    (pairs :+ (IndexColumnName -> index.toString)).toMap
  }

  def renderTopRow(model: ObjectTableModel): String = {
    import boxCharacterSupplier._
    new StringBuilder()
      .append(doubleTopLeft)
      .append(model.columnNames.map(name ⇒ doubleHorizontal * model.columnWidth(name)).mkString(doubleHorizontalSingleDown))
      .append(doubletopRight)
      .toString
  }

  def renderHeaderRow(model: ObjectTableModel): String = {
    import boxCharacterSupplier._
    def renderColumn(name: String) = StringUtils.fitToWidth(name, model.columnWidth(name))
    new StringBuilder()
      .append(doubleVertical)
      .append(model.columnNames.map(renderColumn).mkString(singleVertical))
      .append(doubleVertical)
      .toString
  }

  def renderBelowHeaderRow(model: ObjectTableModel): String = {
    import boxCharacterSupplier._
    new StringBuilder()
      .append(doubleVerticalSingleRight)
      .append(model.columnNames.map(name ⇒ singleHorizontal * model.columnWidth(name)).mkString(singleIntersect))
      .append(doubleVerticalSingleLeft)
      .toString
  }

  private def renderObjectRow(model: ObjectTableModel, renderedObject: Map[String, String]): String = {
    import boxCharacterSupplier._
    def renderCell(name: String) = StringUtils.fitToWidth(renderedObject(name), model.columnWidth(name))
    new StringBuilder()
      .append(doubleVertical)
      .append(model.columnNames.map(renderCell).mkString(singleVertical))
      .append(doubleVertical)
      .toString
  }

  def renderBottomRow(model: ObjectTableModel): String = {
    import boxCharacterSupplier._
    new StringBuilder()
      .append(doubleBottomLeft)
      .append(model.columnNames.map(name ⇒ doubleHorizontal * model.columnWidths(name)).mkString(doubleHorizontalSingleUp))
      .append(doubleBottomRight)
      .toString
  }

  private def printTable(model: ObjectTableModel) {
    val ObjectTableModel(columnNames, columnWidths, objects) = model
    import boxCharacterSupplier._
    output.println(renderTopRow(model))
    output.println(renderHeaderRow(model))
    output.println(renderBelowHeaderRow(model))
    for (obj ← objects)
      output.println(renderObjectRow(model, obj))
    output.println(renderBottomRow(model))
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