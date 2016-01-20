package com.github.mdr.mash.printer

import com.github.mdr.mash.evaluator.MashObject
import com.github.mdr.mash.utils.StringUtils
import com.github.mdr.mash.terminal.TerminalInfo
import java.io.PrintStream

class ObjectPrinter(output: PrintStream, terminalInfo: TerminalInfo) {

  private def printer = new Printer(output, terminalInfo)

  def printObject(mo: MashObject) = {
    if (mo.fields.isEmpty)
      output.println("{}")
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
      output.print("╔")
      output.print("═" * keyWidth)
      output.print("╤")
      output.print("═" * valuesWidth)
      output.println("╗")

      for ((k, v) ← mo.fields) {
        output.print("║")
        output.print(StringUtils.fitToWidth(k + "", keyWidth))
        output.print("│")
        output.print(StringUtils.fitToWidth(printer.renderField(v, inCell = true), valuesWidth))
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