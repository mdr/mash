package com.github.mdr.mash.printer

import scala.collection.immutable.ListMap
import com.github.mdr.mash.evaluator.Evaluator
import com.github.mdr.mash.evaluator.MashObject
import com.github.mdr.mash.evaluator.MemberEvaluator
import com.github.mdr.mash.ns.collections.GroupClass
import com.github.mdr.mash.utils.StringUtils
import com.github.mdr.mash.terminal.TerminalInfo
import java.io.PrintStream

class ObjectTablePrinter(output: PrintStream, terminalInfo: TerminalInfo, alwaysUseBrowser: Boolean = false) {

  private val boxCharacterSupplier: BoxCharacterSupplier = UnicodeBoxCharacterSupplier
  private def printer = new Printer(output, terminalInfo)

  /**
   * @return an optional reference to a selected item
   */
  def printTable(objects: Seq[MashObject]): Option[Int] = {
    val model = renderObjects(objects)
    val nonDataRows = 3 /* header */ + 1 /* footer */
    if (objects.size <= terminalInfo.rows - nonDataRows && !alwaysUseBrowser) {
      printTable(model)
      None
    } else
      ObjectBrowser.launch(model, terminalInfo, output)
  }

  def renderObjects(objects: Seq[MashObject]): ObjectTableModel = {
    val columns = getColumnSpecs(objects)

    val renderedObjects: Seq[ListMap[String, String]] = objects.map(renderObject(_, columns))

    def desiredColumnWidth(member: String): Int = (renderedObjects.map(_(member)) :+ member).map(_.size).max
    val requestedColumnWidths: Map[ColumnSpec, Int] = (for (c ← columns) yield (c -> desiredColumnWidth(c.name))).toMap

    val columnNames = columns.map(_.name)

    val totalAvailableWidth = terminalInfo.columns - 1 - columns.size // accounting for the table and column borders
    val columnWidths =
      for ((c, w) ← ColumnAllocator.allocateColumns(columns, requestedColumnWidths, totalAvailableWidth))
        yield c.name -> w

    ObjectTableModel(columnNames, columnWidths, renderedObjects)
  }

  private def renderObject(obj: MashObject, columns: Seq[ColumnSpec]): ListMap[String, String] = {
    val pairs =
      for {
        ColumnSpec(name, _) ← columns
        rawValue = MemberEvaluator.lookup(obj, name)
        value = Evaluator.immediatelyResolveNullaryFunctions(rawValue)
        renderedValue = printer.renderField(value, inCell = true)
      } yield name -> renderedValue
    ListMap(pairs: _*)
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

  private def getDisplayMembers(objects: Seq[MashObject]): Seq[String] =
    if (objects.forall(_.classOpt == Some(GroupClass)))
      Seq(GroupClass.Fields.Key.name, GroupClass.CountMethod.name)
    else
      objects.flatMap(_.fields.keySet).distinct

  private def getColumnSpecs(objects: Seq[MashObject]): Seq[ColumnSpec] =
    if (objects.forall(_.classOpt == Some(GroupClass)))
      Seq(
        ColumnSpec(GroupClass.Fields.Key.name, 10),
        ColumnSpec(GroupClass.CountMethod.name, 3),
        ColumnSpec(GroupClass.Fields.Values.name, 1))
    else
      objects.flatMap(_.fields.keySet).distinct.map(field ⇒ ColumnSpec(field, 1))

}