package com.github.mdr.mash.printer

import java.io.PrintStream

import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.terminal.TerminalInfo
import com.github.mdr.mash.utils.StringUtils

class ObjectPrinter(output: PrintStream, terminalInfo: TerminalInfo) {

  def printObject(obj: MashObject) = {
    if (obj.fields.isEmpty)
      output.println("{}")
    else {
      val keys = obj.fields.keySet
      val requestedKeyWidth = obj.fields.keySet.map(_.size).max
      val requestedValueWidth = obj.fields.values.map(Printer.renderField(_, inCell = true)).map(_.size).max
      val keyColumn = ColumnSpec("keys", 10)
      val valuesColumn = ColumnSpec("values", 1)
      val requestedWidths = Map(keyColumn -> requestedKeyWidth, valuesColumn -> requestedValueWidth)
      val columns = Seq(keyColumn, valuesColumn)
      val allocatedWidths = ColumnAllocator.allocateColumns(columns, requestedWidths, terminalInfo.columns - 3)
      val keyWidth = allocatedWidths(keyColumn)
      val valuesWidth = allocatedWidths(valuesColumn)

      // Top row
      output.print("╔")
      output.print("═" * keyWidth)
      output.print("╤")
      output.print("═" * valuesWidth)
      output.println("╗")

      for ((k, v) ← obj.fields) {
        output.print("║")
        output.print(StringUtils.fitToWidth(k + "", keyWidth))
        output.print("│")
        output.print(StringUtils.fitToWidth(Printer.renderField(v, inCell = true), valuesWidth))
        output.println("║")
      }

      // Bottom row
      output.print("╚")
      output.print("═" * keyWidth)
      output.print("╧")
      output.print("═" * valuesWidth)
      output.println("╝")
    }
  }

}