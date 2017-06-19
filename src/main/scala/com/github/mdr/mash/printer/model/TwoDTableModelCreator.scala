package com.github.mdr.mash.printer.model

import com.github.mdr.mash.ns.collections.GroupClass
import com.github.mdr.mash.ns.git.CommitClass
import com.github.mdr.mash.printer._
import com.github.mdr.mash.printer.model.TwoDTableModel.{ Cell, Column, Row }
import com.github.mdr.mash.runtime.{ MashList, MashObject, MashValue }
import com.github.mdr.mash.terminal.TerminalInfo
import com.github.mdr.mash.utils.Utils

object TwoDTableModelCreator {

  private val IndexColumnName = "#"
  private val RowLabelColumnId = ColumnId(-1)

}

class TwoDTableModelCreator(terminalInfo: TerminalInfo,
                            showSelections: Boolean = false,
                            viewConfig: ViewConfig,
                            hiddenColumns: Seq[ColumnId] = Seq()) {

  import TwoDTableModelCreator._

  private val fieldRenderer = new FieldRenderer(viewConfig)

  def create(value: MashValue): TwoDTableModel = value match {
    case list: MashList  ⇒ create(list)
    case obj: MashObject ⇒ create(obj)
    case _               ⇒ throw new IllegalArgumentException(s"Could not render 2D table of type ${value.typeName}")
  }

  def create(list: MashList): TwoDTableModel =
    create(list, 0.until(list.size).map(_.toString), list.immutableElements)

  def create(obj: MashObject): TwoDTableModel = {
    val (fieldNames, fieldValues) = obj.immutableFields.toSeq.unzip
    create(obj, fieldNames, fieldValues)
  }

  private def create(rawValue: MashValue, rowLabels: Seq[String], values: Seq[MashValue]): TwoDTableModel = {
    val (dataColumnIds, columnSpecs) = getColumnSpecs(values)

    val tableRows: Seq[Row] = values.zip(rowLabels).map { case (row, rowLabel) ⇒
      createTableRow(dataColumnIds, columnSpecs, row, rowLabel)
    }

    def desiredColumnWidth(columnId: ColumnId): Int = {
      val columnName = columnSpecs(columnId).name
      (tableRows.map(_.renderedValue(columnId)) :+ columnName).map(_.length).max
    }
    val requestedColumnWidths: Map[ColumnId, Int] =
      (for (columnId ← dataColumnIds)
        yield columnId -> desiredColumnWidth(columnId)).toMap

    val allColumnIds = RowLabelColumnId +: dataColumnIds
    val rowLabelWidth = Utils.max(rowLabels.map(_.length), default = 0)
    val selectionStateWidth = if (showSelections) 2 else 0
    val totalAvailableWidth = terminalInfo.columns - rowLabelWidth - 1 - (columnSpecs.size + 1) - selectionStateWidth // accounting for the table and column borders
    val allocatedColumnWidths = ColumnAllocator.allocateColumns(dataColumnIds, columnSpecs, requestedColumnWidths, totalAvailableWidth)

    val dataColumnNames = for ((columnId, colSpec) ← columnSpecs) yield columnId -> colSpec.name
    val allColumnNames = dataColumnNames + (RowLabelColumnId -> IndexColumnName)

    val dataColumns =
      for ((columnId, width) ← allocatedColumnWidths)
        yield columnId -> Column(allColumnNames(columnId), width, columnSpecs.get(columnId).map(_.fetch))
    val rowLabelColumn = RowLabelColumnId -> Column(IndexColumnName, rowLabelWidth)
    val allColumns = dataColumns + rowLabelColumn

    TwoDTableModel(allColumnIds, allColumns, tableRows, rawValue)
  }

  private def createTableRow(columnIds: Seq[ColumnId],
                             columnSpecs: Map[ColumnId, ColumnSpec],
                             rowValue: MashValue,
                             rowLabel: String): Row = {
    val dataCells =
      for {
        columnId ← columnIds
        ColumnSpec(fetch, _) = columnSpecs(columnId)
        cellValueOpt = fetch.lookup(rowValue)
        renderedValue = cellValueOpt.map(value ⇒ fieldRenderer.renderField(value, inCell = true)).getOrElse("")
        cell = Cell(renderedValue, cellValueOpt)
      } yield columnId -> cell
    val indexCell = RowLabelColumnId -> Cell(rowLabel)
    val allCells = (dataCells :+ indexCell).toMap
    Row(allCells, rowValue)
  }

  private def getColumnSpecs(values: Seq[MashValue]): (Seq[ColumnId], Map[ColumnId, ColumnSpec]) = {
    val sampleValues = values.take(50)
    val columnSpecs =
      if (sampleValues.nonEmpty && sampleValues.forall(_ isA GroupClass))
        groupColumnSpecs
      else if (sampleValues.nonEmpty && sampleValues.forall(_ isA CommitClass))
        commitColumnSpecs
      else if (sampleValues.nonEmpty && sampleValues.forall(v ⇒ v.isAnObject || v.isAList)) {
        val fieldSpecs =
          sampleValues
            .flatMap(_.asObject)
            .flatMap(_.immutableFields.keys)
            .distinct
            .zipWithIndex
            .map { case (field, columnIndex) ⇒ ColumnId(columnIndex) -> ColumnSpec(ColumnFetch.ByMember(field)) }
        val sizes = sampleValues.flatMap(_.asList).map(_.size)
        val maxSize = if (sizes.isEmpty) 0 else sizes.max
        val indexSpecs = 0.until(maxSize).map(i ⇒ ColumnId(fieldSpecs.length + i) -> ColumnSpec(ColumnFetch.ByIndex(i)))
        fieldSpecs ++ indexSpecs
      }
      else
        Seq()
    val columnIds = columnSpecs.map(_._1).filterNot(hiddenColumns.contains)
    (columnIds, columnSpecs.toMap)
  }

  private def commitColumnSpecs: Seq[(ColumnId, ColumnSpec)] =
    Seq(
      ColumnId(0) -> ColumnSpec(ColumnFetch.ByMember(CommitClass.Fields.Hash), weight = 1),
      ColumnId(1) -> ColumnSpec(ColumnFetch.ByMember(CommitClass.Fields.CommitTime), weight = 10),
      ColumnId(2) -> ColumnSpec(ColumnFetch.ByMember(CommitClass.Fields.Author), weight = 10),
      ColumnId(3) -> ColumnSpec(ColumnFetch.ByMember(CommitClass.Fields.Summary), weight = 3))

  private def groupColumnSpecs: Seq[(ColumnId, ColumnSpec)] =
    Seq(
      ColumnId(0) -> ColumnSpec(ColumnFetch.ByMember(GroupClass.Fields.Key), weight = 10),
      ColumnId(1) -> ColumnSpec(ColumnFetch.ByMember(GroupClass.CountMethod.name, isNullaryMethod = true), weight = 3),
      ColumnId(2) -> ColumnSpec(ColumnFetch.ByMember(GroupClass.Fields.Values), weight = 1))
}