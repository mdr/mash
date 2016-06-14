package com.github.mdr.mash.printer

import com.github.mdr.mash.evaluator.Evaluator
import com.github.mdr.mash.evaluator.MemberEvaluator
import com.github.mdr.mash.ns.collections.GroupClass
import com.github.mdr.mash.ns.git.CommitClass
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.terminal.TerminalInfo
import com.github.mdr.mash.utils.StringUtils

object ObjectTableModelCreator {

  private val IndexColumnName = "#"

}

class ObjectTableModelCreator(terminalInfo: TerminalInfo, showSelections: Boolean = false) {
  import ObjectTableModelCreator._

  def create(objects: Seq[MashObject]): ObjectTableModel = {
    val columns = getColumnSpecs(objects)

    val renderedObjects: Seq[ObjectTableRow] =
      objects.zipWithIndex.map { case (obj, i) ⇒ createRow(obj, i, columns) }

    def desiredColumnWidth(member: String): Int = (renderedObjects.map(_.data(member)) :+ member).map(_.size).max
    val requestedColumnWidths: Map[ColumnSpec, Int] = (for (c ← columns) yield (c -> desiredColumnWidth(c.name))).toMap

    val columnNames = IndexColumnName +: columns.map(_.name)
    val indexColumnWidth = objects.size.toString.length
    val selectionStateWidth = if (showSelections) 2 else 0
    val totalAvailableWidth = terminalInfo.columns - indexColumnWidth - 1 - (columns.size + 1) - selectionStateWidth // accounting for the table and column borders
    val columnWidths =
      (for ((c, w) ← ColumnAllocator.allocateColumns(columns, requestedColumnWidths, totalAvailableWidth))
        yield c.name -> w) + (IndexColumnName -> indexColumnWidth)

    ObjectTableModel(columnNames, columnWidths, renderedObjects, objects)
  }

  private def createRow(obj: MashObject, index: Int, columns: Seq[ColumnSpec]): ObjectTableRow = {
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