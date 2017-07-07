package com.github.mdr.mash.printer.model

import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.printer._
import com.github.mdr.mash.runtime.{ MashObject, MashValue }
import com.github.mdr.mash.terminal.TerminalInfo

class SingleObjectTableModelCreator(terminalInfo: TerminalInfo,
                                    supportMarking: Boolean = false,
                                    viewConfig: ViewConfig) {

  private val fieldRenderer = new FieldRenderer(viewConfig)

  def create(obj: MashObject): SingleObjectTableModel = {
    val classNameOpt = obj.classOpt.flatMap(_.nameOpt)

    val (fieldColumnWidth, valueColumnWidth) = calculateColumnWidths(obj, classNameOpt)

    val fields = obj.immutableFields
    val renderedFields =
      for ((field, value) ← fields)
        yield renderValue(field) -> renderValue(value)
    SingleObjectTableModel(classNameOpt, renderedFields, fieldColumnWidth, valueColumnWidth, obj, fields)
  }

  private def calculateColumnWidths(obj: MashObject, classNameOpt: Option[String]): (Int, Int) = {
    val requestedFieldWidth = maxFieldWidth(obj)
    val requestedValueWidth = maxValueWidth(obj)

    val fieldColumnId = ColumnId(0)
    val valueColumnId = ColumnId(1)
    val fieldColumn = ColumnSpec(ValueFetch.ByMember("field"), 10)
    val valueColumn = ColumnSpec(ValueFetch.ByMember("value"), 1)
    val columnIds = Seq(fieldColumnId, valueColumnId)
    val columnSpecs = Map(fieldColumnId -> fieldColumn, valueColumnId -> valueColumn)

    val requestedWidths = Map(fieldColumnId -> requestedFieldWidth, valueColumnId -> requestedValueWidth)
    val markingStateWidth = if (supportMarking) 2 else 0
    val totalAvailableWidth = terminalInfo.columns - 3 - markingStateWidth
    val allocatedWidths = ColumnAllocator.allocateColumns(columnIds, columnSpecs, requestedWidths, totalAvailableWidth)

    val fieldColumnWidth = allocatedWidths(fieldColumnId)
    val valueColumnWidth = allocatedWidths(valueColumnId)
    val extra = extraWidthForClassName(classNameOpt, fieldColumnWidth, valueColumnWidth)

    (fieldColumnWidth, valueColumnWidth + extra)
  }

  /**
    * Add some extra width if we need more room for the class name:
    */
  private def extraWidthForClassName(classNameOpt: Option[String], fieldColumnWidth: Int, valueColumnWidth: Int): Int = {
    val classNameWidth = classNameOpt.getOrElse("").size
    val extraNeededForClassName = classNameWidth - (fieldColumnWidth + valueColumnWidth + 1)
    val remainingSpare = terminalInfo.columns - (fieldColumnWidth + valueColumnWidth + 3)
    math.max(0, math.min(extraNeededForClassName, remainingSpare))
  }

  private def maxValueWidth(obj: MashObject): Int =
    if (obj.isEmpty)
      0
    else
      obj.immutableFields.values.map(valueWidth).max

  private def valueWidth(value: MashValue): Int = renderValue(value).size

  private def renderValue(value: MashValue): String = fieldRenderer.renderField(value, inCell = true)

  private def maxFieldWidth(obj: MashObject): Int =
    if (obj.isEmpty) 0 else obj.immutableFields.keySet.map(k ⇒ ToStringifier.stringify(k).size).max

}
