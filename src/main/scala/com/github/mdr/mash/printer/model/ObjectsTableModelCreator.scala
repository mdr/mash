package com.github.mdr.mash.printer.model

import com.github.mdr.mash.ns.collections.GroupClass
import com.github.mdr.mash.ns.git.CommitClass
import com.github.mdr.mash.printer._
import com.github.mdr.mash.runtime.{ MashList, MashValue }
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

  def create(list: MashList): ObjectsTableModel = {
    val values = list.immutableElements
    val (columnIds, columnSpecs) = getColumnSpecs(values)

    val tableRows: Seq[ObjectsTableModel.Row] =
      values.zipWithIndex.map { case (obj, rowIndex) ⇒ createTableRow(obj, rowIndex, columnIds, columnSpecs) }

    def desiredColumnWidth(columnId: ColumnId, columnName: String): Int =
      (tableRows.map(_.renderedValue(columnId)) :+ columnName).map(_.size).max
    val requestedColumnWidths: Map[ColumnId, Int] =
      (for (columnId ← columnIds) yield columnId -> desiredColumnWidth(columnId, columnSpecs(columnId).name)).toMap

    val allColumnIds = IndexColumnId +: columnIds
    val indexColumnWidth = values.size.toString.length
    val selectionStateWidth = if (showSelections) 2 else 0
    val totalAvailableWidth = terminalInfo.columns - indexColumnWidth - 1 - (columnSpecs.size + 1) - selectionStateWidth // accounting for the table and column borders
    val allocatedColumnWidths = ColumnAllocator.allocateColumns(columnIds, columnSpecs, requestedColumnWidths, totalAvailableWidth)
    val columnNames = (for ((columnId, colSpec) ← columnSpecs) yield columnId -> colSpec.name) + (IndexColumnId -> IndexColumnName)
    val columns =
      for ((columnId, width) ← allocatedColumnWidths)
        yield columnId -> ObjectsTableModel.Column(columnNames(columnId), width)
    val indexColumn = IndexColumnId -> ObjectsTableModel.Column(IndexColumnName, indexColumnWidth)
    val allColumns = columns + indexColumn
    ObjectsTableModel(allColumnIds, allColumns, tableRows, list)
  }

  private def createTableRow(rowValue: MashValue,
                             rowIndex: Int,
                             columnIds: Seq[ColumnId],
                             columnSpecs: Map[ColumnId, ColumnSpec]): ObjectsTableModel.Row = {
    val pairs =
      for {
        columnId ← columnIds
        ColumnSpec(fetch, _) = columnSpecs(columnId)
        cellValueOpt = fetch.lookup(rowValue)
        renderedValue = cellValueOpt.map(value ⇒ fieldRenderer.renderField(value, inCell = true)).getOrElse("")
        cell = ObjectsTableModel.Cell(renderedValue, cellValueOpt)
      } yield columnId -> cell
    val cells = (pairs :+ (IndexColumnId -> ObjectsTableModel.Cell(rowIndex.toString))).toMap
    ObjectsTableModel.Row(cells, rowValue)
  }

  private def getColumnSpecs(values: Seq[MashValue]): (Seq[ColumnId], Map[ColumnId, ColumnSpec]) = {
    val testValues = values.take(50)
    val columnSpecs =
      if (testValues.nonEmpty && testValues.forall(_ isA GroupClass))
        Seq(
          ColumnId(0) -> ColumnSpec(ColumnFetch.ByMember(GroupClass.Fields.Key), weight = 10),
          ColumnId(1) -> ColumnSpec(ColumnFetch.ByMember(GroupClass.CountMethod.name, isNullaryMethod = true), weight = 3),
          ColumnId(2) -> ColumnSpec(ColumnFetch.ByMember(GroupClass.Fields.Values), weight = 1))
      else if (testValues.nonEmpty && testValues.forall(_ isA CommitClass))
        Seq(
          ColumnId(0) -> ColumnSpec(ColumnFetch.ByMember(CommitClass.Fields.Hash), weight = 1),
          ColumnId(1) -> ColumnSpec(ColumnFetch.ByMember(CommitClass.Fields.CommitTime), weight = 10),
          ColumnId(2) -> ColumnSpec(ColumnFetch.ByMember(CommitClass.Fields.Author), weight = 10),
          ColumnId(3) -> ColumnSpec(ColumnFetch.ByMember(CommitClass.Fields.Summary), weight = 3))
      else if (testValues.nonEmpty && testValues.forall(v ⇒ v.isAnObject || v.isAList)) {
        val fieldSpecs =
          testValues
            .flatMap(_.asObject)
            .flatMap(_.immutableFields.keys)
            .distinct
            .zipWithIndex
            .map { case (field, columnIndex) ⇒ ColumnId(columnIndex) -> ColumnSpec(ColumnFetch.ByMember(field)) }
        val sizes = testValues.flatMap(_.asList).map(_.size)
        val maxSize = if (sizes.isEmpty) 0 else sizes.max
        val indexSpecs = 0.until(maxSize).map(i ⇒ ColumnId(fieldSpecs.length + i) -> ColumnSpec(ColumnFetch.ByIndex(i)))
        fieldSpecs ++ indexSpecs
      }
      else
        Seq()
    val columnIds = columnSpecs.map(_._1).filterNot(hiddenColumns.contains)
    (columnIds, columnSpecs.toMap)
  }

}