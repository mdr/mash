package com.github.mdr.mash.printer.model

import com.github.mdr.mash.evaluator.{ Evaluator, MemberEvaluator }
import com.github.mdr.mash.ns.collections.GroupClass
import com.github.mdr.mash.ns.git.CommitClass
import com.github.mdr.mash.printer._
import com.github.mdr.mash.runtime.{ MashList, MashObject }
import com.github.mdr.mash.terminal.TerminalInfo

object ObjectsTableModelCreator {

  private val IndexColumnName = "#"
  private val IndexColumnId = ColumnId(-1)

}

class ObjectsTableModelCreator(terminalInfo: TerminalInfo,
                               showSelections: Boolean = false,
                               viewConfig: ViewConfig,
                               hiddenColumns: Seq[ColumnId] = Seq()) {

  import ObjectsTableModelCreator._

  private val fieldRenderer = new FieldRenderer(viewConfig)

  def create(objects: Seq[MashObject], list: MashList): ObjectsTableModel = {
    val (columnIds, columnSpecs) = getColumnSpecs(objects)

    val tableRows: Seq[ObjectTableRow] =
      objects.zipWithIndex.map { case (obj, rowIndex) ⇒ createTableRow(obj, rowIndex, columnIds, columnSpecs) }

    def desiredColumnWidth(columnId: ColumnId, columnName: String): Int =
      (tableRows.map(_.cells(columnId).data) :+ columnName).map(_.size).max
    val requestedColumnWidths: Map[ColumnId, Int] =
      (for (columnId ← columnIds) yield columnId -> desiredColumnWidth(columnId, columnSpecs(columnId).name)).toMap

    val allColumnIds = IndexColumnId +: columnIds
    val indexColumnWidth = objects.size.toString.length
    val selectionStateWidth = if (showSelections) 2 else 0
    val totalAvailableWidth = terminalInfo.columns - indexColumnWidth - 1 - (columnSpecs.size + 1) - selectionStateWidth // accounting for the table and column borders
    val columnWidths =
      (for ((columnId, w) ← ColumnAllocator.allocateColumns(columnIds, columnSpecs, requestedColumnWidths, totalAvailableWidth))
        yield columnId -> w) + (IndexColumnId -> indexColumnWidth)
    val columnNames = (for ((columnId, colSpec) ← columnSpecs) yield columnId -> colSpec.name) + (IndexColumnId -> IndexColumnName)
    ObjectsTableModel(allColumnIds, columnNames, columnWidths, tableRows, list)
  }

  private def createTableRow(obj: MashObject,
                             rowIndex: Int,
                             columnIds: Seq[ColumnId],
                             columnSpecs: Map[ColumnId, ColumnSpec]): ObjectTableRow = {
    val pairs =
      for {
        columnId ← columnIds
        ColumnSpec(name, _, isNullaryMethod) = columnSpecs(columnId)
        rawValueOpt = MemberEvaluator.maybeLookup(obj, name)
        valueOpt = rawValueOpt.map(rawValue ⇒
          if (isNullaryMethod) Evaluator.invokeNullaryFunctions(rawValue, locationOpt = None) else rawValue)
        renderedValue = valueOpt.map(value ⇒ fieldRenderer.renderField(value, inCell = true)).getOrElse("")
        cell = ObjectTableCell(renderedValue, valueOpt)
      } yield columnId -> cell
    val cells = (pairs :+ (IndexColumnId -> ObjectTableCell(rowIndex.toString))).toMap
    ObjectTableRow(obj, cells)
  }

  private def getColumnSpecs(objects: Seq[MashObject]): (Seq[ColumnId], Map[ColumnId, ColumnSpec]) = {
    val testObjects = objects.take(50)
    val columnSpecs =
      if (testObjects.nonEmpty && testObjects.forall(_ isA GroupClass))
        Seq(
          ColumnId(0) -> ColumnSpec(GroupClass.Fields.Key.name, weight = 10),
          ColumnId(1) -> ColumnSpec(GroupClass.CountMethod.name, weight = 3, isNullaryMethod = true),
          ColumnId(2) -> ColumnSpec(GroupClass.Fields.Values.name, weight = 1))
      else if (testObjects.nonEmpty && testObjects.forall(_ isA CommitClass))
        Seq(
          ColumnId(0) -> ColumnSpec(CommitClass.Fields.Hash.name, weight = 1),
          ColumnId(1) -> ColumnSpec(CommitClass.Fields.CommitTime.name, weight = 10),
          ColumnId(2) -> ColumnSpec(CommitClass.Fields.Author.name, weight = 10),
          ColumnId(3) -> ColumnSpec(CommitClass.Fields.Summary.name, weight = 3))
      else
        testObjects
          .flatMap(_.immutableFields.keys)
          .distinct
          .zipWithIndex
          .map { case (field, columnIndex) ⇒ ColumnId(columnIndex) -> ColumnSpec(field) }
    val columnIds = columnSpecs.map(_._1).filterNot(hiddenColumns.contains)
    (columnIds, columnSpecs.toMap)
  }

}