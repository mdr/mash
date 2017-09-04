package com.github.mdr.mash.view.model

import com.github.mdr.mash.ns.collections.GroupClass
import com.github.mdr.mash.ns.git.CommitClass
import com.github.mdr.mash.view.printer._
import com.github.mdr.mash.view.model.TwoDTableModel.{ Cell, Column, Row, RowLabelColumnId }
import com.github.mdr.mash.runtime.{ MashList, MashObject, MashString, MashValue }
import com.github.mdr.mash.utils.{ Dimensions, Utils }
import com.github.mdr.mash.view.{ FieldRenderer, ViewConfig }

import scala.PartialFunction.cond

object TwoDTableModelCreator {

  private val RowLabelColumnName = "#"

  def isSuitableForTwoDTable(value: MashValue) = cond(value) {
    case xs: MashList    ⇒
      xs.nonEmpty && (xs.forall(_.isAnObject) || xs.forall(_.isAList))
    case obj: MashObject ⇒
      val values = obj.immutableFields.values
      obj.nonEmpty && (values.forall(_.isAnObject) && schemasAreSimilar(values) || values.forall(_.isAList))
  }

  private def schemasAreSimilar(values: Iterable[MashValue]) = {
    val objectValues = values.flatMap(_.asObject).take(10)
    val allFields = objectValues.flatMap(_.immutableFields.keys).toSet
    val sharedFields = objectValues.map(_.immutableFields.keys.toSet).reduceOption(_ intersect _).getOrElse(Set())
    val percentageShared = if (allFields.isEmpty) 0.0 else sharedFields.size.toDouble / allFields.size
    percentageShared > 0.75
  }

}

class TwoDTableModelCreator(terminalSize: Dimensions,
                            supportMarking: Boolean = false,
                            viewConfig: ViewConfig,
                            hiddenColumns: Seq[ColumnId] = Seq()) {

  import TwoDTableModelCreator._

  private val fieldRenderer = new FieldRenderer(viewConfig)

  def create(value: MashValue): TwoDTableModel = value match {
    case list: MashList  ⇒ create(list)
    case obj: MashObject ⇒ create(obj)
    case _               ⇒ throw new IllegalArgumentException(s"Could not render 2D table of type ${value.typeName}")
  }

  def create(list: MashList): TwoDTableModel = {
    val rowInfos = list.immutableElements.zipWithIndex.map { case (element, index) ⇒
      RowInfo(index.toString, element, ValueFetch.ByIndex(index))
    }
    create(list, rowInfos)
  }

  def create(obj: MashObject): TwoDTableModel = {
    val rowInfos = obj.immutableFields.toSeq.map { case (field, value) ⇒
      RowInfo(renderValue(field), value, ValueFetch.ByMember(field))
    }
    create(obj, rowInfos)
  }

  case class RowInfo(label: String, rawValue: MashValue, fetch: ValueFetch)

  private def create(rawValue: MashValue, rowInfos: Seq[RowInfo]): TwoDTableModel = {
    val (dataColumnIds, columnSpecs) = getColumnSpecs(rowInfos.map(_.rawValue))

    val tableRows: Seq[Row] = rowInfos.map(rowInfo ⇒
      createTableRow(dataColumnIds, columnSpecs, rowInfo))

    def desiredColumnWidth(columnId: ColumnId): Int = {
      val columnName = columnSpecs(columnId).name
      (tableRows.map(_.renderedValue(columnId)) :+ columnName).map(_.length).max
    }

    val requestedColumnWidths: Map[ColumnId, Int] =
      (for (columnId ← dataColumnIds)
        yield columnId -> desiredColumnWidth(columnId)).toMap

    val allColumnIds = RowLabelColumnId +: dataColumnIds
    val rowLabelWidth = Utils.max(rowInfos.map(_.label.length), default = 0)
    val markingStateWidth = if (supportMarking) 2 else 0
    val totalAvailableWidth = 0 max terminalSize.columns - rowLabelWidth - 1 - (columnSpecs.size + 1) - markingStateWidth // accounting for the table and column borders
    val allocatedColumnWidths = ColumnAllocator.allocateColumns(dataColumnIds, columnSpecs, requestedColumnWidths, totalAvailableWidth)

    val dataColumnNames = for ((columnId, colSpec) ← columnSpecs) yield columnId -> colSpec.name
    val allColumnNames = dataColumnNames + (RowLabelColumnId -> RowLabelColumnName)

    val dataColumns =
      for ((columnId, width) ← allocatedColumnWidths)
        yield columnId -> Column(allColumnNames(columnId), width, columnSpecs.get(columnId).map(_.fetch))
    val rowLabelColumn = RowLabelColumnId -> Column(RowLabelColumnName, rowLabelWidth)
    val allColumns = dataColumns + rowLabelColumn

    TwoDTableModel(allColumnIds, allColumns, tableRows, rawValue)
  }

  private def createTableRow(columnIds: Seq[ColumnId],
                             columnSpecs: Map[ColumnId, ColumnSpec],
                             rowInfo: RowInfo): Row = {
    val dataCells =
      for {
        columnId ← columnIds
        ColumnSpec(fetch, _) = columnSpecs(columnId)
        cellValueOpt = fetch.lookup(rowInfo.rawValue)
        renderedValue = cellValueOpt.map(renderValue) getOrElse ""
        cell = Cell(renderedValue, cellValueOpt)
      } yield columnId -> cell
    val indexCell = RowLabelColumnId -> Cell(rowInfo.label, Some(rowInfo.fetch.value))
    val allCells = (dataCells :+ indexCell).toMap
    Row(allCells, rowInfo.rawValue, rowInfo.fetch)
  }

  private def renderValue(value: MashValue): String = fieldRenderer.renderField(value, inCell = true)

  private def getColumnSpecs(values: Seq[MashValue]): (Seq[ColumnId], Map[ColumnId, ColumnSpec]) = {
    val sampleValues = values.take(50)
    val columnSpecs =
      if (sampleValues.nonEmpty && sampleValues.forall(_ isA GroupClass))
        groupColumnSpecs
      else if (sampleValues.nonEmpty && sampleValues.forall(_ isA CommitClass))
        commitColumnSpecs
      else if (sampleValues.nonEmpty && sampleValues.forall(v ⇒ v.isAnObject || v.isAList))
        getColumnSpecsFromSample(sampleValues)
      else
        Seq()
    val columnIds = columnSpecs.map(_._1).filterNot(hiddenColumns.contains)
    (columnIds, columnSpecs.toMap)
  }

  private def getColumnSpecsFromSample(sampleValues: Seq[MashValue]): Seq[(ColumnId, ColumnSpec)] = {
    val fieldSpecs =
      sampleValues
        .flatMap(_.asObject)
        .flatMap(_.immutableFields.keys)
        .distinct
        .zipWithIndex
        .map { case (field, columnIndex) ⇒ ColumnId(columnIndex) -> ColumnSpec(ValueFetch.ByMember(field)) }

    val sizes = sampleValues.flatMap(_.asList).map(_.size)
    val maxSize = if (sizes.isEmpty) 0 else sizes.max
    val indexSpecs = 0.until(maxSize).map(i ⇒ ColumnId(fieldSpecs.length + i) -> ColumnSpec(ValueFetch.ByIndex(i)))

    fieldSpecs ++ indexSpecs
  }

  private def commitColumnSpecs: Seq[(ColumnId, ColumnSpec)] =
    Seq(
      ColumnId(0) -> ColumnSpec(ValueFetch.ByMember(CommitClass.Fields.Hash), weight = 1),
      ColumnId(1) -> ColumnSpec(ValueFetch.ByMember(CommitClass.Fields.CommitTime), weight = 10),
      ColumnId(2) -> ColumnSpec(ValueFetch.ByMember(CommitClass.Fields.Author), weight = 10),
      ColumnId(3) -> ColumnSpec(ValueFetch.ByMember(CommitClass.Fields.Summary), weight = 3))

  private def groupColumnSpecs: Seq[(ColumnId, ColumnSpec)] =
    Seq(
      ColumnId(0) -> ColumnSpec(ValueFetch.ByMember(GroupClass.Fields.Key), weight = 10),
      ColumnId(1) -> ColumnSpec(ValueFetch.ByMember(MashString(GroupClass.CountMethod.name), isNullaryMethod = true), weight = 3),
      ColumnId(2) -> ColumnSpec(ValueFetch.ByMember(GroupClass.Fields.Values), weight = 1))
}