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
      (tableRows.map(_.data(columnId)) :+ columnName).map(_.size).max
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
    ObjectsTableModel(allColumnIds, columnNames, columnWidths, tableRows, list, objects)
  }

  private def createTableRow(obj: MashObject,
                             rowIndex: Int,
                             columnIds: Seq[ColumnId],
                             columnSpecs: Map[ColumnId, ColumnSpec]): ObjectTableRow = {
    val pairs =
      for {
        columnId ← columnIds
        ColumnSpec(_, name, _, isNullaryMethod) = columnSpecs(columnId)
        rawValueOpt = MemberEvaluator.maybeLookup(obj, name)
        valueOpt = rawValueOpt.map(rawValue ⇒
          if (isNullaryMethod) Evaluator.invokeNullaryFunctions(rawValue, locationOpt = None) else rawValue)
        renderedValue = valueOpt.map(value ⇒ fieldRenderer.renderField(value, inCell = true)).getOrElse("")
      } yield columnId ->(valueOpt, renderedValue)
    val data = ((for {(id, (_, v)) <- pairs} yield id -> v) :+ (IndexColumnId -> rowIndex.toString)).toMap
    val rawObjects = (for {(id, (rawOpt, _)) <- pairs; raw <- rawOpt} yield id -> raw).toMap
    ObjectTableRow(data, rawObjects)
  }

  private def getColumnSpecs(objects: Seq[MashObject]): (Seq[ColumnId], Map[ColumnId, ColumnSpec]) = {
    val testObjects = objects.take(50)
    val columnSpecs =
      if (testObjects.nonEmpty && testObjects.forall(_ isA GroupClass))
        Seq(
          ColumnSpec(ColumnId(0), GroupClass.Fields.Key.name, weight = 10),
          ColumnSpec(ColumnId(1), GroupClass.CountMethod.name, weight = 3, isNullaryMethod = true),
          ColumnSpec(ColumnId(2), GroupClass.Fields.Values.name, weight = 1))
      else if (testObjects.nonEmpty && testObjects.forall(_ isA CommitClass))
        Seq(
          ColumnSpec(ColumnId(0), CommitClass.Fields.Hash.name, weight = 1),
          ColumnSpec(ColumnId(1), CommitClass.Fields.CommitTime.name, weight = 10),
          ColumnSpec(ColumnId(2), CommitClass.Fields.Author.name, weight = 10),
          ColumnSpec(ColumnId(3), CommitClass.Fields.Summary.name, weight = 3))
      else
        testObjects
          .flatMap(_.immutableFields.keys)
          .distinct
          .zipWithIndex
          .map { case (field, columnIndex) ⇒ ColumnSpec(ColumnId(columnIndex), field) }
    val columnIds = columnSpecs.map(_.id).filterNot(hiddenColumns.contains)
    val colSpecs =
      (for (colSpec <- columnSpecs)
        yield colSpec.id -> colSpec).toMap
    (columnIds, colSpecs)
  }

}