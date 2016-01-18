package com.github.mdr.mash.printer

import com.github.mdr.mash.evaluator.MashObject
import com.github.mdr.mash.utils.StringUtils

class ObjectPrinter(terminalInfo: TerminalInfo) {

  private def printer = new Printer(terminalInfo)

  def printObject(mo: MashObject) = {
    if (mo.fields.isEmpty)
      println("{}")
    else {
      val keys = mo.fields.keySet
      val requestedKeyWidth = mo.fields.keySet.map(_.size).max
      val requestedValueWidth = mo.fields.values.map(printer.renderField(_, inCell = true)).map(_.size).max
      val keyColumn = ColumnSpec("keys", 10)
      val valuesColumn = ColumnSpec("values", 1)
      val requestedWidths = Map(keyColumn -> requestedKeyWidth, valuesColumn -> requestedValueWidth)
      val columns = Seq(keyColumn, valuesColumn)
      val allocatedWidths = ColumnAllocator.allocateColumns(columns, requestedWidths, terminalInfo.columns - 3)
      val keyWidth = allocatedWidths(keyColumn)
      val valuesWidth = allocatedWidths(valuesColumn)

      // Top row
      print("╔")
      print("═" * keyWidth)
      print("╤")
      print("═" * valuesWidth)
      println("╗")

      for ((k, v) ← mo.fields) {
        print("║")
        print(StringUtils.fitToWidth(k + "", keyWidth))
        print("│")
        print(StringUtils.fitToWidth(printer.renderField(v, inCell = true), valuesWidth))
        println("║")
      }

      // Bottom row
      print("╚")
      print("═" * keyWidth)
      print("╧")
      print("═" * valuesWidth)
      println("╝")
    }
  }

}