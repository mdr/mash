package com.github.mdr.mash.printer.model

import com.github.mdr.mash.evaluator.{ Evaluator, MemberEvaluator }
import com.github.mdr.mash.ns.collections.GroupClass
import com.github.mdr.mash.ns.git.CommitClass
import com.github.mdr.mash.printer.{ ColumnAllocator, ColumnSpec, FieldRenderer, ViewConfig }
import com.github.mdr.mash.runtime.{ MashList, MashObject }
import com.github.mdr.mash.terminal.TerminalInfo

object ObjectsTableModelCreator {

  private val IndexColumnName = "#"

}

class ObjectsTableModelCreator(terminalInfo: TerminalInfo,
                               showSelections: Boolean = false,
                               viewConfig: ViewConfig,
                               hiddenColumns: Seq[String] = Seq()) {

  import ObjectsTableModelCreator._

  private val fieldRenderer = new FieldRenderer(viewConfig)

  def create(objects: Seq[MashObject], list: MashList): ObjectsTableModel = {
    val columns = getColumnSpecs(objects)

    val renderedObjects: Seq[ObjectTableRow] =
      objects.zipWithIndex.map { case (obj, i) ⇒ createRow(obj, i, columns) }

    def desiredColumnWidth(member: String): Int = (renderedObjects.map(_.data(member)) :+ member).map(_.size).max
    val requestedColumnWidths: Map[ColumnSpec, Int] = (for (c ← columns) yield c -> desiredColumnWidth(c.name)).toMap

    val columnNames = IndexColumnName +: columns.map(_.name)
    val indexColumnWidth = objects.size.toString.length
    val selectionStateWidth = if (showSelections) 2 else 0
    val totalAvailableWidth = terminalInfo.columns - indexColumnWidth - 1 - (columns.size + 1) - selectionStateWidth // accounting for the table and column borders
    val columnWidths =
      (for ((c, w) ← ColumnAllocator.allocateColumns(columns, requestedColumnWidths, totalAvailableWidth))
        yield c.name -> w) + (IndexColumnName -> indexColumnWidth)

    ObjectsTableModel(columnNames, columnWidths, renderedObjects, list, objects)
  }

  private def createRow(obj: MashObject, index: Int, columns: Seq[ColumnSpec]): ObjectTableRow = {
    val pairs =
      for {
        ColumnSpec(name, _, isNullaryMethod) ← columns
        rawValueOpt = MemberEvaluator.maybeLookup(obj, name)
        valueOpt = rawValueOpt.map(rawValue ⇒
          if (isNullaryMethod) Evaluator.immediatelyResolveNullaryFunctions(rawValue, locationOpt = None) else rawValue)
        renderedValue = valueOpt.map(value => fieldRenderer.renderField(value, inCell = true)).getOrElse("")
      } yield name -> (valueOpt, renderedValue)
    val data = ((for { (k, (_, v)) <- pairs } yield k -> v) :+ (IndexColumnName -> index.toString)).toMap
    val rawObjects = (for { (k, (rawOpt, _)) <- pairs; raw <- rawOpt } yield k -> raw).toMap
    ObjectTableRow(data, rawObjects)
  }

  private def getColumnSpecs(objects: Seq[MashObject]): Seq[ColumnSpec] = {
    val testObjects = objects.take(50)
    if (testObjects.nonEmpty && testObjects.forall(_ isA GroupClass))
      Seq(
        ColumnSpec(GroupClass.Fields.Key.name, weight = 10),
        ColumnSpec(GroupClass.CountMethod.name, weight = 3, isNullaryMethod = true),
        ColumnSpec(GroupClass.Fields.Values.name, weight = 1))
    else if (testObjects.nonEmpty && testObjects.forall(_ isA CommitClass))
      Seq(
        ColumnSpec(CommitClass.Fields.Hash.name, weight = 1),
        ColumnSpec(CommitClass.Fields.CommitTime.name, weight = 10),
        ColumnSpec(CommitClass.Fields.Author.name, weight = 10),
        ColumnSpec(CommitClass.Fields.Summary.name, weight = 3))
    else
      testObjects.flatMap(_.fields.keySet).distinct.map(field ⇒ ColumnSpec(field))
  }.filterNot(hiddenColumns contains _.name)

}