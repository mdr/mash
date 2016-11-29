package com.github.mdr.mash.printer

import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.terminal.TerminalInfo

class ObjectModelCreator(terminalInfo: TerminalInfo, viewConfig: ViewConfig) {

  private val fieldRenderer: FieldRenderer = new FieldRenderer(viewConfig)

  def create(obj: MashObject): ObjectModel = {
    val requestedKeyWidth = if (obj.fields.isEmpty) 0 else obj.fields.keySet.map(_.size).max
    val requestedValueWidth = if (obj.fields.isEmpty) 0 else obj.fields.values.map(fieldRenderer.renderField(_, inCell = true)).map(_.size).max
    val keyColumn = ColumnSpec("keys", 10)
    val valuesColumn = ColumnSpec("values", 1)
    val requestedWidths = Map(keyColumn -> requestedKeyWidth, valuesColumn -> requestedValueWidth)
    val columns = Seq(keyColumn, valuesColumn)
    val allocatedWidths = ColumnAllocator.allocateColumns(columns, requestedWidths, terminalInfo.columns - 3)
    val keyWidth = allocatedWidths(keyColumn)
    val valuesWidth = allocatedWidths(valuesColumn)
    val fields = obj.immutableFields
    val renderedFields =
      for ((k, v) ← fields)
        yield k -> fieldRenderer.renderField(v, inCell = true)
    ObjectModel(renderedFields, keyWidth, valuesWidth, obj, fields)
  }

}
