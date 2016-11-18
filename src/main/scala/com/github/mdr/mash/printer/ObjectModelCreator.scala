package com.github.mdr.mash.printer

import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.terminal.TerminalInfo

class ObjectModelCreator(terminalInfo: TerminalInfo) {

  def create(obj: MashObject): ObjectModel = {
    val requestedKeyWidth = obj.fields.keySet.map(_.size).max
    val requestedValueWidth = obj.fields.values.map(Printer.renderField(_, inCell = true)).map(_.size).max
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
        yield k -> Printer.renderField(v, inCell = true)
    ObjectModel(renderedFields, keyWidth, valuesWidth, fields)
  }

}
